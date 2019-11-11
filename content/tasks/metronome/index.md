+++
title = "Metronome"
description = ""
date = 2019-05-20T13:38:22Z
aliases = []
[extra]
id = 10462
task = """
  Implement a Metronome.
  It should be capable of producing high and low audio beats,
  accompanied by a visual beat indicator,
  and the beat pattern and tempo should be configurable.
"""
[taxonomies]
categories = []
tags = ["music", "metronome"]
+++

![An image of a mechanical metronome](metronome.jpeg)

For the purpose of this task,
it is acceptable to play sound files for production of the beat notes,
and an external player may be used.

However, the playing of the sounds should not interfere
with the timing of the metronome.

The visual indicator can simply be a blinking red or green area of the screen
(depending on whether a high or low beat is being produced),
and the metronome can be implemented using a terminal display,
or optionally, a graphical display, depending on the language capabilities.

If the language has no facility to output sound,
then it is permissible for this to implemented using just the visual indicator.


## Ada

This metronome only does 60 bpm with 1 Measure length.

```Ada
with Ada.Text_IO;   use Ada.Text_IO;

 --This package is for the delay.
with Ada.Calendar;  use Ada.Calendar;

 --This package adds sound
with Ada.Characters.Latin_1;

procedure Main is

begin

   Put_Line ("Hello, this is 60 BPM");

   loop

      Ada.Text_IO.Put (Ada.Characters.Latin_1.BEL);
      delay 0.9; --Delay in seconds.  If you change to 0.0 the program will crash.

   end loop;

end Main;
```


## AutoHotkey

Rather basic implementation,
but meets the requirements and is reasonably accurate.

```AHK
bpm      = 120 ; Beats per minute
pattern  = 4/4 ;
duration = 100 ; Milliseconds
beats    = 0   ; internal counter

Gui -Caption

StringSplit, p, pattern, /

Start := A_TickCount

Loop
{
	Gui Color, 0xFF0000
	Gui Show, w200 h200 Na
	SoundBeep 750, duration
	beats++
	Sleep 1000 * 60 / bpm - duration
	Loop % p1 -1
	{
		Gui Color, 0x00FF00
		Gui Show, w200 h200 Na
		SoundBeep, , duration
		beats++
		Sleep 1000 * 60 / bpm - duration
	}
}

Esc::
	MsgBox % "Metronome beeped " beats " beats, over " (A_TickCount-Start)/1000 " seconds. "
	ExitApp
```


## AppleScript

```applescript
set bpm to the text returned of (display dialog "How many beats per minute?" default answer 60)

set pauseBetweenBeeps to (60 / bpm)

repeat

	beep

	delay pauseBetweenBeeps

end repeat
```


## AWK

```AWK
# syntax: GAWK -f METRONOME.AWK
@load "time"
BEGIN {
    metronome(120,6,10)
    metronome(72,4)
    exit(0)
}
function metronome(beats_per_min,beats_per_bar,limit,  beats,delay,errors) {
    print("")
    if (beats_per_min+0 <= 0) { print("error: beats per minute is invalid") ; errors++ }
    if (beats_per_bar+0 <= 0) { print("error: beats per bar is invalid") ; errors++ }
    if (limit+0 <= 0) { limit = 999999 }
    if (errors > 0) { return }
    delay = 60 / beats_per_min
    printf("delay=%f",delay)
    while (beats < limit) {
      printf((beats++ % beats_per_bar == 0) ? "\nTICK" : " tick")
      sleep(delay)
    }
}
```

Output:

```txt

delay=0.500000
TICK tick tick tick tick tick
TICK tick tick tick
delay=0.833333
TICK tick tick tick
TICK tick tick tick
...

```


## BBC BASIC

Works with BBC BASIC for Windows

```bbcbasic
      BeatPattern$ = "HLLL"
      Tempo% = 100

      *font Arial,36
      REPEAT
        FOR beat% = 1 TO LEN(BeatPattern$)
          IF MID$(BeatPattern$, beat%, 1) = "H" THEN
            SOUND 1,-15,148,1
          ELSE
            SOUND 1,-15,100,1
          ENDIF
          VDU 30
          COLOUR 2
          PRINT LEFT$(BeatPattern$,beat%-1);
          COLOUR 9
          PRINT MID$(BeatPattern$,beat%,1);
          COLOUR 2
          PRINT MID$(BeatPattern$,beat%+1);
          WAIT 6000/Tempo%
        NEXT
      UNTIL FALSE
```


## C

Using `usleep` with self correcting delays.
Audio is the bell character,
which will definitely drive one insane
(but I'm ok: my computer doesn't have the bell device).
Invoke with `./a.out [beats_per_minute]`, default to 60.

```c
#include <stdio.h>

#include <stdlib.h>
#include <unistd.h>
#include <stdint.h>
#include <signal.h>
#include <time.h>
#include <sys/time.h>

struct timeval start, last;

inline int64_t tv_to_u(struct timeval s)
{
	return s.tv_sec * 1000000 + s.tv_usec;
}

inline struct timeval u_to_tv(int64_t x)
{
	struct timeval s;
	s.tv_sec = x / 1000000;
	s.tv_usec = x % 1000000;
	return s;
}

void draw(int dir, int64_t period, int64_t cur, int64_t next)
{
	int len = 40 * (next - cur) / period;
	int s, i;

	if (len > 20) len = 40 - len;
	s = 20 + (dir ? len : -len);

	printf("\033[H");
	for (i = 0; i <= 40; i++) putchar(i == 20 ? '|': i == s ? '#' : '-');
}

void beat(int delay)
{
	struct timeval tv = start;
	int dir = 0;
	int64_t d = 0, corr = 0, slp, cur, next = tv_to_u(start) + delay;
	int64_t draw_interval = 20000;
	printf("\033[H\033[J");
	while (1) {
		gettimeofday(&tv, 0);
		slp = next - tv_to_u(tv) - corr;
		usleep(slp);
		gettimeofday(&tv, 0);

		putchar(7); /* bell */
		fflush(stdout);

		printf("\033[5;1Hdrift: %d compensate: %d (usec)   ",
			(int)d, (int)corr);
		dir = !dir;

		cur = tv_to_u(tv);
		d = cur - next;
		corr = (corr + d) / 2;
		next += delay;

		while (cur + d + draw_interval < next) {
			usleep(draw_interval);
			gettimeofday(&tv, 0);
			cur = tv_to_u(tv);
			draw(dir, delay, cur, next);
			fflush(stdout);
		}
	}
}

int main(int c, char**v)
{
	int bpm;

	if (c < 2 || (bpm = atoi(v[1])) <= 0) bpm = 60;
	if (bpm > 600) {
		fprintf(stderr, "frequency %d too high\n", bpm);
		exit(1);
	}

	gettimeofday(&start, 0);
	last = start;
	beat(60 * 1000000 / bpm);

	return 0;
}
```


## Common Lisp

Depends on quicklisp and OpenAL.

```lisp
(ql:quickload '(cl-openal cl-alc))

(defparameter *short-max* (- (expt 2 15) 1))
(defparameter *2-pi* (* 2 pi))

(defun make-sin (period)
  "Create a generator for a sine wave of the given PERIOD."
  (lambda (x)
    (sin (* *2-pi* (/ x period)))))

(defun make-tone (length frequency sampling-frequency)
  "Create a vector containing sound information of the given LENGTH,
FREQUENCY, and SAMPLING-FREQUENCY."
  (let ((data (make-array (truncate (* length sampling-frequency))
                          :element-type '(signed-byte 16)))
        (generator (make-sin (/ sampling-frequency frequency))))
    (dotimes (i (length data))
      (setf (aref data i)
            (truncate (* *short-max* (funcall generator i)))))
    data))

(defun internal-time-ms ()
  "Get the process's real time in ms."
  (* 1000 (/ (get-internal-real-time) internal-time-units-per-second)))

(defun spin-wait (next-real-time)
  "Wait until the process's real time has reached the given time."
  (loop while (< (internal-time-ms) next-real-time)))

(defun upload (buffer data sampling-frequency)
  "Upload the given vector DATA to a BUFFER at the given SAMPLING-FREQUENCY."
  (cffi:with-pointer-to-vector-data (data-ptr data)
    (al:buffer-data buffer :mono16 data-ptr (* 2 (length data))
                    sampling-frequency)))

(defun metronome (beats/minute pattern &optional (sampling-frequency 44100))
  "Play a metronome until interrupted."
  (let ((ms/beat (/ 60000 beats/minute)))
    (alc:with-device (device)
      (alc:with-context (context device)
        (alc:make-context-current context)
        (al:with-buffer (low-buffer)
          (al:with-buffer (high-buffer)
            (al:with-source (source)
              (al:source source :gain 0.5)
              (flet ((play-it (buffer)
                       (al:source source :buffer buffer)
                       (al:source-play source))
                     (upload-it (buffer time frequency)
                       (upload buffer
                               (make-tone time frequency sampling-frequency)
                               sampling-frequency)))
                (upload-it low-buffer 0.1 440)
                (upload-it high-buffer 0.15 880)
                (let ((next-scheduled-tone (internal-time-ms)))
                  (loop
                     (loop for symbol in pattern do
                          (spin-wait next-scheduled-tone)
                          (incf next-scheduled-tone ms/beat)
                          (case symbol
                            (l (play-it low-buffer))
                            (h (play-it high-buffer)))
                          (princ symbol))
                     (terpri)))))))))))
```

Output:

```txt
CL-USER> (metronome 100 '(h l l l))
HLLL
HLL; Evaluation aborted on NIL.
```


## EchoLisp

```scheme
;; available preloaded sounds are : ok, ko, tick, tack, woosh, beep, digit .
(lib 'timer)

(define (metronome) (blink) (play-sound 'tack))
(at-every 1000 'metronome) ;; every 1000 msec
;; CTRL-C to stop
```


## Factor

```factor
USING: accessors calendar circular colors.constants colors.hsv
command-line continuations io kernel math math.parser namespaces
openal.example sequences system timers ui ui.gadgets
ui.pens.solid ;
IN: rosetta-code.metronome

: bpm>duration ( bpm -- duration ) 60 swap / seconds ;

: blink-gadget ( gadget freq -- )
    1.0 1.0 1.0 <hsva>  <solid> >>interior relayout-1 ;

: blank-gadget ( gadget -- )
    COLOR: white <solid> >>interior relayout-1 ;

: play-note ( gadget freq -- )
    [ blink-gadget ] [ 0.3 play-sine blank-gadget ] 2bi ;

: metronome-iteration ( gadget circular -- )
    [ first play-note ] [ rotate-circular ] bi ;

TUPLE: metronome-gadget < gadget bpm notes timer ;

: <metronome-gadget> ( bpm notes -- gadget )
    \ metronome-gadget new swap >>notes swap >>bpm ;

: metronome-quot ( gadget -- quot )
    dup notes>> <circular> [ metronome-iteration ] 2curry ;

: metronome-timer ( gadget -- timer )
    [ metronome-quot ] [ bpm>> bpm>duration ] bi every ;

M: metronome-gadget graft* ( gadget -- )
    [ metronome-timer ] keep timer<< ;

M: metronome-gadget ungraft*
    timer>> stop-timer ;

M: metronome-gadget pref-dim* drop { 200 200 } ;

: metronome-defaults ( -- bpm notes ) 60 { 440 220 330 } ;

: metronome-ui ( bpm notes -- ) <metronome-gadget> "Metronome" open-window ;

: metronome-example ( -- ) metronome-defaults metronome-ui ;

: validate-args ( int-args -- )
    [ length 2 < ] [ [ 0 <= ] any? ] bi or [ "args error" throw ] when ;

: (metronome-cmdline) ( args -- bpm notes )
    [ string>number ] map dup validate-args
    unclip swap ;

: metronome-cmdline ( -- bpm notes )
    command-line get [ metronome-defaults ] [ (metronome-cmdline) ] if-empty ;

: print-defaults ( -- )
  metronome-defaults swap prefix
  [ " " write ] [ number>string write ] interleave nl ;

: metronome-usage ( -- )
    "Usage: metronome [BPM FREQUENCIES...]" print
    "Arguments must be non-zero" print
    "Example: metronome " write print-defaults flush ;

: metronome-main ( -- )
     [ [ metronome-cmdline metronome-ui ] [ drop metronome-usage 1 exit ] recover ] with-ui ;

MAIN: metronome-main
```


## F\#

```fsharp
open System
open System.Threading
// You can use .wav files for your clicks.
// If used, make sure they are in the same file
// as this program's executable file.
let high_pitch =
    new System.Media.SoundPlayer("Ping Hi.wav")
let low_pitch =
    new System.Media.SoundPlayer("Ping Low.wav")
let factor x y = x / y
// Notice that exact bpm would not work by using
// Thread.Sleep() as there are additional function calls
// that would consume a miniscule amount of time.
// This number may need to be adjusted based on the cpu.
let cpu_error = -750.0
let print = function
| 1 -> high_pitch.Play(); printf "\nTICK "
| _ -> low_pitch.Play(); printf "tick "
let wait (time:int) =
    Thread.Sleep(time)
// Composition of functions
let tick = float>>factor (60000.0+cpu_error)>>int>>wait
let rec play beats_per_measure current_beat beats_per_minute =
    match current_beat, beats_per_measure with
    | a, b ->
        current_beat |> print
        beats_per_minute |> tick
        if a <> b then
            beats_per_minute |> play beats_per_measure (current_beat + 1)
[<EntryPointAttribute>]
let main (args : string[]) =
    let tempo, beats = int args.[0], int args.[1]
    Seq.initInfinite (fun i -> i + 1)
    |> Seq.iter (fun _ -> tempo |> play beats 1 |> ignore)
    0
```

Sample run:

```txt
$ metronome 120 6

TICK tick tick tick tick tick
TICK tick tick tick tick tick
TICK tick tick tick tick tick
TICK tick tick tick tick tick
TICK tick tick^C
```


## Go

As with the [[#Perl_6|Perl example]], just simple text output.
It would be reasonably simple (but covered better in other tasks)
to change bpm and bpb into command line arguments,
make it a function/object,
and/or substitute sound production instead of text output.

`time.Ticker`'s documentation says that it
"adjusts the intervals or drops ticks to make up for slow receivers".
So, as long as the output or sound production finishes before the next tick,
the timing will be reliable and will not drift which is the gist of this task.

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	var bpm = 72.0 // Beats Per Minute
	var bpb = 4    // Beats Per Bar

	d := time.Duration(float64(time.Minute) / bpm)
	fmt.Println("Delay:", d)
	t := time.NewTicker(d)
	i := 1
	for _ = range t.C {
		i--
		if i == 0 {
			i = bpb
			fmt.Printf("\nTICK ")
		} else {
			fmt.Printf("tick ")
		}
	}
}
```

Output:

```txt
Delay: 833.333333ms

TICK tick tick tick
TICK tick tick tick
TICK tick ^C
```


## Haskell

Works with GHC 7.4.2

```Haskell
import Control.Concurrent
import Control.Concurrent.MVar
import System.Process (runCommand)

-- This program works only on the GHC compiler because of the use of
-- threadDelay

data Beep = Stop | Hi | Low

type Pattern = [Beep]

type BeatsPerMinute = Int

minute = 60000000 -- 1 minute = 60,000,000 microseconds

-- give one of the following example patterns to the metronome function

pattern4_4 = [Hi, Low, Low, Low]
pattern2_4 = [Hi, Low]
pattern3_4 = [Hi, Low, Low]
pattern6_8 = [Hi, Low, Low, Low, Low, Low]

-- use this version if you can't play audio, use Windows or don't
-- have audio files to play
-- beep :: Beep -> IO ()
-- beep Stop = return ()
-- beep Hi = putChar 'H'
-- beep Low = putChar 'L'

-- use this version if you can and want to play audio on Linux using
-- Alsa. Change the name of the files to those of your choice

beep Stop = return ()
beep Hi = putChar 'H' >> runCommand "aplay hi.wav &> /dev/null" >> return ()
beep Low = putChar 'L' >> runCommand "aplay low.wav &> /dev/null" >> return ()

tick :: MVar Pattern -> BeatsPerMinute -> IO ()
tick b i = do
    t <- readMVar b
    case t of
        [Stop] -> return ()
        x -> do
            mapM_ (\v -> forkIO (beep v) >> threadDelay (minute `div` i)) t
            tick b i

metronome :: Pattern -> BeatsPerMinute -> IO ()
metronome p i = do
    putStrLn "Press any key to stop the metronome."
    b <- newMVar p
    _ <- forkIO $ tick b i
    _ <- getChar
    putMVar b [Stop]
```


## J

The explicit version (nbars,t) MET (barlengths;bpm)
prints a bell character every beat,
accompanied by a sequence of slashes, spaces,
and backspaces to create a little animation.
It includes a beat hand and a measure (or bar) hand.

MET can take several barlengths and bpm values,
in which case it will cycle through them individually at each measure,
creating (perhaps) interesting patterns.
It will stop when nbars measures have been cycled through,
or at the end of the current measure if the time limit is exceeded.
The clock is self correcting.

MET returns the total number of measures, beats, and elapsed time.

If you leave out the left arguments, it will set them to infinity,
so you can go insane without worrying about the metronome ever stopping.

```j
MET=: _ _&$: :(4 : 0)

  'BEL BS LF CR'=. 7 8 10 13 { a.
  '`print stime delay'=. 1!:2&4`(6!:1)`(6!:3)
  ticker=. 2 2$'\  /'
  'small large'=. (BEL,2#BS) ; 5#BS
  clrln=. CR,(79#' '),CR

  x=. 2 ({.,) x
  y=. _1 |.&.> 2 ({.,) y
  'i j'=. 0
  print 'bpb \  bpm \ ' , 2#BS
  delay 1

  x=. ({. , ('ti t'=. stime'') + {:) x
  while. x *./@:> i,t do.

    'bpb bpm'=. {.@> y=. 1 |.&.> y
    dl=. 60 % bpm

    print clrln,(":bpb),' ',(ticker {~ 2 | i=. >: i),' ',(":bpm),' '

    for. i. bpb do.
      print small ,~ ticker {~ 2 | j=. >: j
      delay 0 >. (t=. t + dl) - stime ''
    end.

  end.

  print clrln
  i , j , t - ti
)
```

Basic tacit version; this is probably considered bad coding style. At least I removed the "magic constants". Sort of.
The above version is by far superior.

```
'BEL BS LF'=: 7 8 10 { a.
'`print delay'=: 1!:2&4`(6!:3)
met=: _&$: :((] ({:@] [ LF print@[ (-.@{.@] [ delay@[ print@] (BEL,2#BS) , (2 2$'\  /') {~ {.@])^:({:@])) 1 , <.@%) 60&% [ print@('\ '"_))
```

Output:

```txt
   16 60 MET 4;120
4  / 120  /

   NB. Variable measure lengths, and corresponding bpm:
   21 _ MET 4 3 4 5 ; 120 100 120 150    NB. _ is infinity.
5 \  150  /
   NB. returns: 21 84 39.2    (21 measures, 84 beats, 39.2 seconds)

   MET 4 8;120 240    NB. It can almost make music!
bpb \  bpm \
```


## Java

```java
class Metronome{
	double bpm;
	int measure, counter;
	public Metronome(double bpm, int measure){
		this.bpm = bpm;
		this.measure = measure;
	}
	public void start(){
		while(true){
			try {
				Thread.sleep((long)(1000*(60.0/bpm)));
			}catch(InterruptedException e) {
				e.printStackTrace();
			}
			counter++;
			if (counter%measure==0){
				 System.out.println("TICK");
			}else{
				 System.out.println("TOCK");
			}
		}
	}
}
public class test {
	public static void main(String[] args) {
		Metronome metronome1 = new Metronome(120,4);
		metronome1.start();
	}
}

```


## Julia

Works with Julia 0.6

```julia
function metronome(bpm::Real=72, bpb::Int=4)
    s = 60.0 / bpm
    counter = 0
    while true
        counter += 1
        if counter % bpb != 0
            println("tick")
        else
            println("TICK")
        end
        sleep(s)
    end
end
```


## Kotlin

```scala
// version 1.1.2

fun metronome(bpm: Int, bpb: Int, maxBeats: Int = Int.MAX_VALUE) {
    val delay = 60_000L / bpm
    var beats = 0
    do {
        Thread.sleep(delay)
        if (beats % bpb == 0) print("\nTICK ")
        else print("tick ")
        beats++
    }
    while (beats < maxBeats)
    println()
}

fun main(args: Array<String>) = metronome(120, 4, 20) // limit to 20 beats
```

Output:

```txt
TICK tick tick tick
TICK tick tick tick
TICK tick tick tick
TICK tick tick tick
TICK tick tick tick
```


## Liberty BASIC

Requires two supplied wav files for accentuated & standard sounds.

```lb
    WindowWidth  =230
    WindowHeight =220

    button #w.b1 "Start",   [start],   LR, 110, 90, 55, 20
    button #w.b2 "Tempo",   [tempo],   LR, 180, 90, 55, 20
    button #w.b3 "Pattern", [pattern], LR,  40, 90, 55, 20

    open "Metronome" for graphics_nsb_nf as #w

    #w "trapclose quit"
    #w "down"
    #w "fill darkblue ; backcolor darkblue ; color white"

    tempo    =   60              '   per minute
    interval =1000 /(tempo /60)  '   timer works in ms
    tickCount =   0              '   cycle counter
    running   =   1              '   flag for state
    bar$      = "HLLL"           '   initially strong-weak-weak-weak
    count     = len( bar$)

    wait

sub quit w$
    close #w$
    end
end sub

[start]
    if running =1 then
        running =0
        #w.b1 "Stop"
        #w.b2 "!disable"
        #w.b3 "!disable"
    else
        running =1
        #w.b1 "Start"
        #w.b2 "!enable"
        #w.b3 "!enable"
    end if
    if running =0 then timer interval, [tick] else timer 0
    wait

[tempo]
    prompt "New tempo 30...360"; tempo$
    tempo =val( tempo$)
    tempo =min( tempo, 360)
    tempo =max( tempo, 30)
    interval =int( 1000 /(tempo /60))
 wait

[pattern]
    prompt "New Pattern, eg 'HLLL' "; bar$
    count =len( bar$)
    if count <2 or count >8 then goto [pattern]

 wait

[tick]
    'beep and flash
    #w "place 115 40"

    if mid$( bar$, tickCount +1, 1) ="H" then
        playwave "mHi.wav", async
        #w "backcolor blue ; color white ; circlefilled "; 20 -tickCount *2
    else
        playwave "mLo.wav", async
        #w "backcolor cyan ; circlefilled "; 20 -tickCount *2
    end if

    #w "place 50 140 ; backcolor darkblue ; color white"
    #w "\  "; tempo; " beats /min."
    #w "place 85 160"
    #w "\"; bar$

    #w "place 85 120"
    #w "\Beat # "; tickCount +1

    #w "place 115 40"
    #w "color darkblue"

    tickCount =( tickCount +1) mod count

    #w "flush"

    wait
```


## Perl

The module `Time::HiRes` provides sub-second <tt>sleep</tt>. Text output only.

```perl
use Time::HiRes qw(sleep gettimeofday);

local $| = 1;    # autoflush

my $beats_per_minute = shift || 72;
my $beats_per_bar    = shift || 4;

my $i         = 0;
my $duration  = 60 / $beats_per_minute;
my $base_time = gettimeofday() + $duration;

for (my $next_time = $base_time ; ; $next_time += $duration) {
    if ($i++ % $beats_per_bar == 0) {
        print "\nTICK";
    }
    else {
        print " tick";
    }
    sleep($next_time - gettimeofday());
}
```

Output:

```txt
$ metronome 60 6
TICK tick tick tick tick tick
TICK tick tick tick tick tick
TICK tick tick tick tick tick
TICK tick tick tick tick tick
^C
```


## Perl 6

This code only uses textual output,
but any noise-generating commands may be substituted;
as long as they are executed synchronously,
and do not run longer than the specified duration,
the timing loop will compensate,
since the sequence operator is determining a list of absolute times
for each <tt>sleep</tt> to target.

```perl6
sub MAIN ($beats-per-minute = 72, $beats-per-bar = 4) {
    my $duration = 60 / $beats-per-minute;
    my $base-time = now + $duration;
    my $i;

    for $base-time, $base-time + $duration ... * -> $next-time {
        if $i++ %% $beats-per-bar {
            print "\nTICK";
        }
        else {
            print  " tick";
        }
        sleep $next-time - now;
    }
}
```

Sample run:

```txt
$ metronome 120 6

TICK tick tick tick tick tick
TICK tick tick tick tick tick
TICK tick tick tick tick tick
TICK tick tick tick tick tick
TICK tick tick^C
```


## Phix

```Phix
integer tempo = 120,    -- beats per minute (max 800)
        measure = 4,    -- beats per bar
        runsecs = 5     -- total run time (rounded up to whole bars)

integer low_freq = #200, low_duration = 20,
        high_freq = #400, high_duration = 20

atom k32=0, xBeep
atom t0 = time(), count = 0
atom duration = 60/tempo,
     next = time()+duration

while time()<t0+runsecs do
    for j=1 to measure do
        if platform()=WINDOWS then
            if k32=0 then
                k32 = open_dll("kernel32.dll")
                xBeep = define_c_proc(k32, "Beep", {C_INT,C_INT})
            end if
            if j=1 then
                c_proc(xBeep,{high_freq,high_duration})
                if count=0 then t0 = time() end if
            else
                c_proc(xBeep,{low_freq,low_duration})
            end if
        else
            puts(1,7)
        end if
        count += 1
        puts(1,iff(j=1?"\nH":"L"))
        sleep(next-time())
        next += duration
    end for
end while
printf(1,"\naverage %f bpm\n",{count*60/(time()-t0)})
```

You may also want to have a look at demo\rosetta\virtunome.exw, a gui version.


## PicoLisp

A short beep (440 Hz, 40 msec) is produced in a child process,
while a "pendulum" is swinging left and right. Hitting any key will stop it.

```PicoLisp
(de metronome (Bpm)
   (if (fork)
      (let Pid @
         (for Pendulum '(" /" . ("^H^H\\ " "^H^H /" .))
            (tell Pid 'call "/usr/bin/beep" "-f" 440 "-l" 40)
            (prin Pendulum)
            (T (key (*/ 30000 Bpm)) (tell Pid 'bye)) )
         (prinl) )
      (wait) ) )
```

Test:

```PicoLisp
: (metronome 60)
 /
-> NIL  # A key was hit
```


## PureBasic

Metronome features:

- A periodic graphical-metronomimic image
- The wav file is included within the resulting executable as raw data
- A milliseconds between each click field in order to assess accuracy
- Volumn controls for when you just can't stand it anymore!

![Screenshot of a metronome app written in PureBasic](metronome_screenshot.png)

```PureBasic
Structure METRONOMEs
  msPerBeat.i
  BeatsPerMinute.i
  BeatsPerCycle.i
  volume.i
  canvasGadget.i
  w.i
  h.i
  originX.i
  originY.i
  radius.i
  activityStatus.i
EndStructure

Enumeration       ;gadgets
  #TEXT_MSPB      ;milliseconds per beat
  #STRING_MSPB    ;milliseconds per beat
  #TEXT_BPM       ;beats per minute
  #STRING_BPM     ;beats per minute
  #TEXT_BPC       ;beats per cycle
  #STRING_BPC     ;beats per cycle
  #BUTTON_VOLM    ;volume -
  #BUTTON_VOLP    ;volume +
  #BUTTON_START   ;start
  #SPIN_BPM
  #CANVAS_METRONOME
EndEnumeration

Enumeration       ;sounds
  #SOUND_LOW
  #SOUND_HIGH
EndEnumeration

#WINDOW = 0       ;window

Procedure handleError(Value, text.s)
  If Not Value: MessageRequester("Error", text): End: EndIf
EndProcedure

Procedure drawMetronome(*m.METRONOMEs, Angle.f, cycleCount = 0)
  Protected CircleX, CircleY, circleColor
  If StartDrawing(CanvasOutput(*m\canvasGadget))
      Box(0, 0, *m\w, *m\h, RGB(0, 0, 0))
      CircleX = Int(*m\radius * Cos(Radian(Angle)))
      CircleY = Int(*m\radius * Sin(Radian(Angle)))
      If Angle = 90
        If cycleCount: circleColor = RGB(255, 0, 0): Else: circleColor = RGB(0, 255, 0): EndIf
        LineXY(*m\originX, *m\originY, *m\originX, *m\originY - CircleY, RGB(255, 255, 0))
        Circle(*m\originX + CircleX, *m\originY - CircleY - *m\radius * 0.15, 10, circleColor)
      Else
        LineXY(*m\originX, *m\originY - *m\radius * 1.02, *m\originX, *m\originY - *m\radius, RGB(255, 255, 0))
        LineXY(*m\originX, *m\originY, *m\originX + CircleX, *m\originY - CircleY, RGB(255, 255, 0))
      EndIf

    StopDrawing()

    ProcedureReturn 1
  EndIf
EndProcedure

Procedure.i Metronome(*m.METRONOMEs)
  Protected milliseconds = Int((60 * 1000) / *m\BeatsPerMinute)
  Protected msPerFrame, framesPerBeat
  Protected i, j, cycleCount, startTime, frameEndTime, delayTime, delayError, h.f

  ;calculate metronome angles for each frame of animation
  If *m\BeatsPerMinute < 60
    framesPerBeat = Round(milliseconds / 150, #PB_Round_Nearest)
  Else
    framesPerBeat = Round((*m\BeatsPerMinute - 420) / -60, #PB_Round_Nearest)
  EndIf

  If framesPerBeat < 1
    framesPerBeat = 1
    Dim metronomeFrameAngle.f(1, framesPerBeat)
    metronomeFrameAngle(0, 1) = 90
    metronomeFrameAngle(1, 1) = 90
  Else
    Dim metronomeFrameAngle.f(1, framesPerBeat * 2)
    For j = 1 To framesPerBeat
      h = 45 / framesPerBeat
      metronomeFrameAngle(0, j) = 90 - h * (j - 1)
      metronomeFrameAngle(0, framesPerBeat + j) = 45 + h * (j - 1)
      metronomeFrameAngle(1, j) = 90 + h * (j - 1)
      metronomeFrameAngle(1, framesPerBeat + j) = 135 - h * (j - 1)
    Next
    framesPerBeat * 2
  EndIf
  msPerFrame   = milliseconds / framesPerBeat

  PlaySound(#SOUND_HIGH)
  startTime = ElapsedMilliseconds()
  Repeat
    For i = 0 To 1
      frameEndTime = startTime + msPerFrame
      For j = 1 To framesPerBeat
        drawMetronome(*m, metronomeFrameAngle(i, j), cycleCount)

        ;check for thread exit
        If *m\activityStatus < 0
          *m\activityStatus = 0
          ProcedureReturn
        EndIf

        delayTime = frameEndTime - ElapsedMilliseconds()
        If (delayTime - delayError) >= 0
          Delay(frameEndTime - ElapsedMilliseconds() - delayError) ;wait the remainder of frame
        ElseIf delayTime < 0
          delayError = - delayTime
        EndIf
        frameEndTime + msPerFrame
      Next

      ;check for thread exit
      If *m\activityStatus < 0
        *m\activityStatus = 0
        ProcedureReturn
      EndIf

      While (ElapsedMilliseconds() - startTime) < milliseconds:  Wend

      SetGadgetText(*m\msPerBeat, Str(ElapsedMilliseconds() - startTime))
      cycleCount + 1: cycleCount % *m\BeatsPerCycle
      If cycleCount = 0
        PlaySound(#SOUND_HIGH)
      Else
        PlaySound(#SOUND_LOW)
      EndIf
      startTime + milliseconds
    Next
  ForEver
EndProcedure

Procedure startMetronome(*m.METRONOMEs, MetronomeThread) ;start up the thread with new values
  *m\BeatsPerMinute = Val(GetGadgetText(#STRING_BPM))
  *m\BeatsPerCycle  = Val(GetGadgetText(#STRING_BPC))
  *m\activityStatus = 1

  If *m\BeatsPerMinute
    MetronomeThread = CreateThread(@Metronome(), *m)
  EndIf
  ProcedureReturn MetronomeThread
EndProcedure

Procedure stopMetronome(*m.METRONOMEs, MetronomeThread) ;if the thread is running: stop it
  If IsThread(MetronomeThread)
    *m\activityStatus = -1 ;signal thread to stop
  EndIf
  drawMetronome(*m, 90)
EndProcedure


Define w = 360, h = 360, ourMetronome.METRONOMEs

;initialize the metronome
With ourMetronome
  \msPerBeat     = #STRING_MSPB
  \canvasGadget   = #CANVAS_METRONOME
  \volume        = 10
  \w             = w
  \h             = h
  \originX       = w / 2
  \originY       = h / 2
  \radius        = 100
EndWith

ourMetronome\canvasGadget = #CANVAS_METRONOME

;initialize sounds
handleError(InitSound(), "Sound system is Not available")
handleError(CatchSound(#SOUND_LOW, ?sClick, ?eClick - ?sClick), "Could Not CatchSound")
handleError(CatchSound(#SOUND_HIGH, ?sClick, ?eClick - ?sClick), "Could Not CatchSound")
SetSoundFrequency(#SOUND_HIGH, 50000)
SoundVolume(#SOUND_LOW, ourMetronome\volume)
SoundVolume(#SOUND_HIGH, ourMetronome\volume)

;setup window & GUI
Define Style, i, wp, gh

Style = #PB_Window_SystemMenu | #PB_Window_ScreenCentered | #PB_Window_MinimizeGadget
handleError(OpenWindow(#WINDOW, 0, 0, w + 200 + 12, h + 4, "Metronome", Style), "Not OpenWindow")
SetWindowColor(#WINDOW, $505050)

If LoadFont(0, "tahoma", 9, #PB_Font_HighQuality | #PB_Font_Bold)
  SetGadgetFont(#PB_Default, FontID(0))
EndIf

i = 3: wp = 10: gh = 22
TextGadget(#TEXT_MSPB, w + wp, gh * i, 100, gh, "MilliSecs/Beat ", #PB_Text_Center)
StringGadget(#STRING_MSPB, w + wp + 108, gh * i, 90, gh, "0", #PB_String_ReadOnly): i + 2
TextGadget(#TEXT_BPM, w + wp, gh * i, 100, gh,"Beats/Min ", #PB_Text_Center)
StringGadget(#STRING_BPM, w + wp + 108, gh * i, 90, gh, "120", #PB_String_Numeric): i + 2
GadgetToolTip(#STRING_BPM, "Valid range is 20 -> 240")
TextGadget(#TEXT_BPC, w + wp, gh * i, 100, gh,"Beats/Cycle ", #PB_Text_Center)
StringGadget(#STRING_BPC, w + wp + 108, gh * i, 90, gh, "4", #PB_String_Numeric): i + 2
GadgetToolTip(#STRING_BPC, "Valid range is 1 -> BPM")
ButtonGadget(#BUTTON_START, w + wp, gh * i, 200, gh, "Start", #PB_Button_Toggle): i + 2
ButtonGadget(#BUTTON_VOLM, w + wp, gh * i, 100, gh, "-Volume")
ButtonGadget(#BUTTON_VOLP, w + wp + 100, gh * i, 100, gh, "+Volume")
CanvasGadget(ourMetronome\canvasGadget, 0, 0, ourMetronome\w, ourMetronome\h, #PB_Image_Border)
drawMetronome(ourMetronome, 90)

Define msg, GID, MetronomeThread, Value
Repeat ;the control loop for our application
  msg = WaitWindowEvent(1)
  GID = EventGadget()
  etp = EventType()

  If GetAsyncKeyState_(#VK_ESCAPE): End: EndIf ;remove when app is o.k.

  Select msg

    Case #PB_Event_CloseWindow
      End

    Case #PB_Event_Gadget
      Select GID

        Case #STRING_BPM
          If etp = #PB_EventType_LostFocus
            Value = Val(GetGadgetText(#STRING_BPM))
            If Value > 390
            Value = 390
            ElseIf Value < 20
            Value = 20
            EndIf
            SetGadgetText(#STRING_BPM, Str(Value))
          EndIf

        Case #STRING_BPC
          If etp = #PB_EventType_LostFocus
            Value = Val(GetGadgetText(#STRING_BPC))
            If Value > Val(GetGadgetText(#STRING_BPM))
              Value = Val(GetGadgetText(#STRING_BPM))
            ElseIf Value < 1
              Value = 1
            EndIf
            SetGadgetText(#STRING_BPC, Str(Value))
          EndIf

        Case #BUTTON_VOLP, #BUTTON_VOLM ;change volume
          If GID = #BUTTON_VOLP And ourMetronome\volume < 100
            ourMetronome\volume + 10
          ElseIf GID = #BUTTON_VOLM And ourMetronome\volume > 0
            ourMetronome\volume - 10
          EndIf
          SoundVolume(#SOUND_LOW, ourMetronome\volume)
          SoundVolume(#SOUND_HIGH, ourMetronome\volume)

        Case #BUTTON_START ;the toggle button for start/stop
          Select GetGadgetState(#BUTTON_START)
            Case 1
              stopMetronome(ourMetronome, MetronomeThread)
              MetronomeThread = startMetronome(ourMetronome, MetronomeThread)
              SetGadgetText(#BUTTON_START,"Stop")
            Case 0
              stopMetronome(ourMetronome, MetronomeThread)
              SetGadgetText(#BUTTON_START,"Start")
          EndSelect

      EndSelect
  EndSelect
ForEver
End

DataSection
  ;a small wav file saved as raw data
  sClick:
  Data.q $0000082E46464952,$20746D6645564157,$0001000100000010,$0000AC440000AC44
  Data.q $6174616400080001,$8484848300000602,$8B8A898886868585,$C0B3A9A29C95918E
  Data.q $31479BD3CED0CFCF,$3233323232323331,$BDAEC4C9A1423133,$D0CFCFD0CFD1CDD4
  Data.q $323232333770A9CB,$2E34313332333232,$CFD0CFCACFCFAF53,$9783AAD3CED0CFD0
  Data.q $3233313332448AA1,$4430333233323233,$CFD0CFAE7D7E9483,$B7B9C3B8BFD1CED0
  Data.q $3233313037476898,$3E48493D32333233,$85959187775C4338,$898A8F8D807A7978
  Data.q $737F898381888D8D,$3131435564717A77,$332F36413A343234,$827B7873695C4D37
  Data.q $9C9B949191908F8A,$4D50515A67758694,$5451484341414245,$7B83868782756559
  Data.q $565D5F616365676E,$7871675F57504B4E,$797C8083827E7C7B,$4D525E686C6D7176
  Data.q $4D4B4B474343464A,$8B82796F655D5953,$7B7C83888B909392,$5153595E666F767B
  Data.q $5A5A5B5B59575553,$696C6E6D67615E5B,$7879777573726F6B,$71727376797C7B79
  Data.q $505352505159646C,$6B6153463C3B414A,$A09B908379706D6E,$6F767A7E858C949C
  Data.q $4D4D4845484E5662,$80796F645B544E4C,$8487888885828283,$555A5D606369737D
  Data.q $6A665F58524E4E51,$878E8F8B867D736D,$54606B72797F8081,$5852504F4E4E4C4D
  Data.q $7E7B7A79756F675F,$6B6C6F757C7F8182,$6D6865676C6F6E6C,$6E6B6C7074777773
  Data.q $615E5D60676F7372,$7069636061636463,$81817F7C7A797976,$65676B6E72797E80
  Data.q $65625F5E5D5D5F62,$7D7C7B7875716E6A,$6F74777B7E7F7F7E,$454950565D63676A
  Data.q $605A55504B464343,$9E9F9D978E817469,$6E7D8A93989A9B9C,$444546494C505861
  Data.q $7B756F665C534C46,$7E82858788888580,$5C5D61666B70757A,$6A6B6B6B6965615E
  Data.q $646B717676736F6B,$727272716D676261,$8285878885817A74,$5F5F636A72797D80
  Data.q $645D58585A5E6060,$827D79777877746D,$7878797C80848685,$6A686664666B7075
  Data.q $76726C666364686B,$807E7B7878787878,$656A6F74797B7D7F,$59585A5C5D5F6163
  Data.q $84807C79746D665E,$777C81878B8C8B89,$555352555B636B72,$82776B625C595957
  Data.q $989B9A979493918B,$656A6E7277818A92,$535457575656585E,$6E6E6F6D675E5753
  Data.q $898C9398968D7F74,$69717E8C9697918B,$3B39414F5D656767,$695B56565A595145
  Data.q $8986878A8F90897B,$7A7875757B848A8C,$747168605E636D76,$7B7365585257636F
  Data.q $8C8981777272777C,$70757676767A8188,$6A6D6D68625F6269,$8687847D746C6868
  Data.q $8485888A89868484,$585A616B747B8083,$5B555355595C5C5A,$8C898786847D7265
  Data.q $888C9096999A9892,$6163666C72797F83,$5E554E4B4C52595E,$91887C7169676663
  Data.q $8E8E8A88888C9193,$656666676A737E88,$655F585352555B62,$8B88837C756F6B69
  Data.q $7E858B8E8E8B898B,$62676B6D6D6C6E74,$6C6C6B6A6764605F,$7C7978787775736F
  Data.q $8E8C8C8C8D8D8982,$686D747C858C9091,$625C58585B5E6265,$908A837C75706C68
  Data.q $848A8E9396989896,$545960676E757A7F,$65605E5D5B575352,$A19D9890877C746C
  Data.q $8992979A9C9FA1A2,$525355575C64707E,$736D665D56514F50,$8D8D8B8986827E79
  Data.q $7777797B7F84898C,$78746F6A6A6E7376,$7B79767372747879,$6C6B69686A6F757A
  Data.q $7C78746F6D6C6C6D,$888B8C8B88858280,$6F75797B7D7F8184,$6F6D6B686565676A
  Data.q $8785817D79747270,$868B8D8D8C8A8988,$6F73777A7C7D7F82,$6F6B68656465666A
  Data.q $8A837D7976757472,$76787A7D82888C8D,$6562606064696F73,$8E8A847C756F6B68
  Data.q $8D90939494929190,$606264686F788088,$73685D58585A5D5F,$9B96918C8987837D
  Data.q $71767B838C959B9D,$5B5756585D63686D,$8C86817B756F6962,$888F939597979591
  Data.q $5C606366696F7780,$7A756D6259535458,$9EA2A0988D837E7C,$79858D8F8F8E9097
  Data.q $656B6E6D6865666E,$797A79746C656060,$7F7F7F7F7E7C7978,$8381828384848280
  Data.q $7F82888E92918D88,$59606A757C7F7F7F,$655E5A5A59585656,$A59F97918A82796E
  Data.q $7E848A939CA6ABAA,$48454548515E6B76,$8A7C6E6057514E4B,$A9ACABA8A4A09B94
  Data.q $5A626A737F8C98A3,$60554D49484A4E54,$A9A19A928A81776B,$848C969FA7ADAFAD
  Data.q $4B494C525C67717B,$8A7D7167605A544F,$A2A4A5A6A6A49F96,$626970777F89949C
  Data.q $6B655E56504F545A,$A19F988F8379736E,$848C9296989A9C9F,$61626465676B717A
  Data.q $807E79736D676261,$86898B8C8A888583,$797A7E8284848484,$77777A7C7E7E7C7A
  Data.q $7979797B7D7E7C79,$7D7C7B7B7C7C7B7A,$7D7D7C7B7A7B7C7D,$797978777677797B
  Data.q $787A7A7976757678,$415380817C777576,$2C31000002005255,$30202C36202C3020
  eClick:
EndDataSection
```


## Pure Data

```txt
#N canvas 553 78 360 608 10;
#X obj 20 20 cnv 15 320 140 empty empty empty 20 12 0 14 -228856 -66577 0;
#X obj 20 190 cnv 15 320 36 empty empty empty 20 12 0 14 -233017 -66577 0;
#X obj 67 30 vradio 20 1 0 6 empty beats empty 0 -8 0 10 -86277 -262144 -1 1;
#X text 40 33 1/1;
#X text 40 53 2/2;
#X text 40 73 3/4;
#X text 40 93 4/4;
#X text 40 133 6/8;
#X obj 67 167 + 1;
#X floatatom 67 201 5 0 0 0 beats - -;
#X obj 181 32 vsl 20 115 208 40 0 0 empty bpm empty 25 10 0 10 -86277 -262144 -1 5971 0;
#X text 208 42 Larghetto 60-66;
#X text 208 58 Adagio 66-76;
#X text 208 74 Andante 76-108;
#X text 208 90 Moderato 108-120;
#X text 208 106 Allegro 120-168;
#X text 208 122 Presto 168-200;
#X text 208 138 Prestissimo 200-208;
#X text 208 26 Largo 40-60;
#X obj 181 167 int;
#X floatatom 181 201 5 0 0 1 bpm - -;
#X obj 149 246 expr 1000 / ($f1/60);
#X obj 122 125 tgl 25 0 empty on on/off -4 -7 0 10 -261682 -86277 -86277 0 1;
#X obj 122 270 metro;
#X obj 122 291 int;
#X obj 42 249 + 1;
#X obj 52 275 mod;
#X obj 122 312 moses 1;
#X obj 122 347 bng 32 250 50 0 empty empty empty 17 7 0 10 -228856 -258113 -1;
#X obj 161 347 bng 32 250 50 0 empty empty empty 17 7 0 10 -228856 -260097 -1;
#X msg 81 399 1 2 \, 1 2 1 \, 0 3 2;
#X obj 81 420 vline~;
#X msg 200 399 1 2 \, 1 2 1 \, 0 3 2;
#X obj 200 420 vline~;
#X obj 20 420 osc~ 1400;
#X obj 139 420 osc~ 1230;
#X obj 65 455 *~;
#X obj 184 455 *~;
#X obj 116 559 dac~;
#X obj 117 523 +~;
#X obj 278 490 loadbang;
#X msg 278 511 \; pd dsp 1 \; beats 1 \; bpm 120 \; on 1;
#X connect 2 0 8 0;
#X connect 8 0 9 0;
#X connect 9 0 26 1;
#X connect 10 0 19 0;
#X connect 19 0 20 0;
#X connect 20 0 21 0;
#X connect 21 0 23 1;
#X connect 22 0 23 0;
#X connect 23 0 24 0;
#X connect 24 0 25 0;
#X connect 24 0 27 0;
#X connect 25 0 26 0;
#X connect 26 0 24 1;
#X connect 27 0 28 0;
#X connect 27 1 29 0;
#X connect 28 0 30 0;
#X connect 29 0 32 0;
#X connect 30 0 31 0;
#X connect 31 0 36 1;
#X connect 32 0 33 0;
#X connect 33 0 37 1;
#X connect 34 0 36 0;
#X connect 35 0 37 0;
#X connect 36 0 39 0;
#X connect 37 0 39 1;
#X connect 39 0 38 0;
#X connect 39 0 38 1;
#X connect 40 0 41 0;
```


## Python

```Python
#lang Python
import time

def main(bpm = 72, bpb = 4):
    sleep = 60.0 / bpm
    counter = 0
    while True:
        counter += 1
        if counter % bpb:
            print 'tick'
        else:
            print 'TICK'
        time.sleep(sleep)



main()
```


## Racket

```Racket
#lang racket

(require racket/gui)

(define msec 500)
(define sounds '("hi.wav" "lo.wav"))
(define colors '("red" "green"))

(define f
  (new frame% [label "Metronome"] [width 200] [height 200]))
(define c
  (new (class canvas%
         (define brushes
           (map (λ(c) (new brush% [color c] [style 'solid])) colors))
         (define cur 0)
         (define/override (on-paint)
           (send* (send this get-dc)
                  (clear)
                  (set-brush (list-ref brushes cur))
                  (draw-rectangle 0 0 200 200)))
         (define/public (flip!)
           (set! cur (modulo (add1 cur) (length sounds)))
           (play-sound (list-ref sounds cur) #f)
           (on-paint))
         (super-new))
       [parent f]))

(define (flip)
  (define init (current-inexact-milliseconds))
  (define next (+ msec init))
  (define ticks 1)
  (let loop ()
    (when (> (current-inexact-milliseconds) next)
      (set! ticks (add1 ticks))
      (set! next (+ init (* msec ticks)))
      (queue-callback (λ() (send c flip!))))
    (sleep 0.01)
    (loop)))

(send* f (center) (show #t))
(void (thread flip))
```


## REXX

These REXX program examples are modeled after the Perl 6 example.


### Textual visual, no sound

```rexx
/*REXX program simulates a visual (textual)  metronome  (with no sound).                */
parse arg bpm bpb dur .                          /*obtain optional arguments from the CL*/
if bpm=='' | bpm==","  then bpm=72               /*the number of  beats  per minute.    */
if bpb=='' | bpb==","  then bpb= 4               /* "     "    "    "     "   bar.      */
if dur=='' | dur==","  then dur= 5               /*duration of the  run  in seconds.    */
call time 'Reset'                                /*reset the REXX elapsed timer.        */
bt=1/bpb                                         /*calculate a   tock-time   interval.  */

  do until et>=dur;    et=time('Elasped')        /*process  tick-tocks  for the duration*/
  say; call charout ,'TICK'                      /*show the first tick for the period.  */
  es=et+1                                        /*bump the elapsed time  "limiter".    */
  $t=et+bt
                        do until e>=es;        e=time('Elapsed')
                        if e<$t then iterate                       /*time for tock?     */
                        call charout , ' tock'                     /*show a  "tock".    */
                        $t=$t+bt                                   /*bump the TOCK time.*/
                         end   /*until e≥es*/
  end   /*until et≥dur*/
                                                 /*stick a fork in it,  we're all done. */
```

Output when using the default inputs:

```txt
TICK tock tock tock tock
TICK tock tock tock tock
TICK tock tock tock tock
TICK tock tock tock tock
TICK tock tock tock tock
TICK tock tock tock tock
```

### With sound, REGINA only

This REXX version only executes when using the Regina REXX interpreter.

```rexx
/*REXX program simulates a  metronome  (with sound).    Regina REXX only.               */
parse arg bpm bpb dur tockf tockd tickf tickd .  /*obtain optional arguments from the CL*/
if   bpm=='' |   bpm==","  then   bpm= 72        /*the number of  beats  per minute.    */
if   bpb=='' |   bpb==","  then   bpb=  4        /* "    "     "    "     "   bar.      */
if   dur=='' |   dur==","  then   dur=  5        /*duration  of the  run         in secs*/
if tockf=='' | tockf==","  then tockf=400        /*frequency  "  "   tock  sound  " HZ. */
if tockd=='' | tockd==","  then tockd= 20        /*duration   "  "     "     "    " msec*/
if tickf=='' | tickf==","  then tickf=600        /*frequency  "  "   tick    "    " HZ. */
if tickd=='' | tickd==","  then tickd= 10        /*duration   "  "     "     "    " msec*/
call time 'Reset'                                /*reset the REXX elapsed timer.        */
bt=1/bpb                                         /*calculate a   tock─time   interval.  */

  do until et>=dur;     et=time('Elasped')       /*process  tick-tocks  for the duration*/
  call beep tockf, tockd                         /*sound a beep for the  "TOCK".        */
  es=et+1                                        /*bump the elapsed time  "limiter".    */
  $t=et+bt
                        do until e>=es;        e=time('Elapsed')
                        if e<$t then iterate                       /*time for tock?     */
                        call beep tickf, tickd                     /*sound a  "tick".   */
                        $t=$t+bt                                   /*bump the TOCK time.*/
                         end   /*until e≥es*/
  end   /*until et≥dur*/
                                                 /*stick a fork in it,  we're all done. */
```


### With sound, PC/REXX only

```rexx
/*REXX program simulates a  metronome  (with sound).      PC/REXX or Personal REXX only.*/
parse arg bpm bpb dur tockf tockd tickf tickd .  /*obtain optional arguments from the CL*/
if   bpm=='' |   bpm==","  then   bpm= 72        /*the number of  beats  per minute.    */
if   bpb=='' |   bpb==","  then   bpb=  4        /* "    "     "    "     "   bar.      */
if   dur=='' |   dur==","  then   dur=  5        /*duration  of the  run         in secs*/
if tockf=='' | tockf==","  then tockf=400        /*frequency  "  "   tock  sound  " HZ. */
if tockd=='' | tockd==","  then tockd=   .02     /*duration   "  "     "     "    " sec.*/
if tickf=='' | tickf==","  then tickf=600        /*frequency  "  "   tick    "    " HZ. */
if tickd=='' | tickd==","  then tickd=   .01     /*duration   "  "     "     "    " sec.*/
call time 'Reset'                                /*reset the REXX elapsed timer.        */
bt=1/bpb                                         /*calculate a   tock─time   interval.  */

  do until et>=dur;      et=time('Elasped')      /*process  tick-tocks  for the duration*/
  call sound tockf, tockd                        /*sound a beep for the  "TOCK".        */
  es=et+1                                        /*bump the elapsed time  "limiter".    */
  $t=et+bt
                         do until e>=es;       e=time('Elapsed')
                         if e<$t then iterate                      /*time for tock?     */
                         call sound tickf, tickd                   /*sound a  tick.     */
                         $t=$t+bt                                  /*bump the TOCK time.*/
                         end   /*until e≥es*/
  end   /*until et≥dur*/
                                                 /*stick a fork in it,  we're all done. */
```


## Ruby

This code rings the audible bell on every beat
and write "And n" to stdout where n is the bar number that was just finished

```ruby
bpm = Integer(ARGV[0]) rescue 60 # sets BPM by the first command line argument, set to 60 if none provided
msr = Integer(ARGV[1]) rescue 4 # sets number of beats in a measure by the second command line argument, set to 4 if none provided
i = 0

loop do
  (msr-1).times do
    puts "\a"
    sleep(60.0/bpm)
  end
  puts "\aAND #{i += 1}"
  sleep(60.0/bpm)
end
```


## Scala

```Scala
def metronome(bpm: Int, bpb: Int, maxBeats: Int = Int.MaxValue) {
  val delay = 60000L / bpm
  var beats = 0
  do {
    Thread.sleep(delay)
    if (beats % bpb == 0) print("\nTICK ")
    else print("tick ")
    beats+=1
  }
  while (beats < maxBeats)
  println()
}

metronome(120, 4, 20) // limit to 20
```

Output:
See it be executed in your browser by
[Scastie (JVM)](https://scastie.scala-lang.org/7iejBcWqQISAIGtBCG6BNQ).


## Sidef

```ruby
func metronome (beats_per_minute = 72, beats_per_bar = 4) {

    var counter   = 0
    var duration  = 60/beats_per_minute
    var base_time = Time.micro+duration

    STDOUT.autoflush(true)

    for next_time in (base_time..Inf `by` duration) {
        if (counter++ %% beats_per_bar) {
            print "\nTICK"
        }
        else {
            print " tick"
        }
        Sys.sleep(next_time - Time.micro)
    }
}

say metronome(ARGV.map{ Num(_) }...)
```

Output:

```txt
% sidef metronome.sf 60 6

TICK tick tick tick tick tick
TICK tick tick tick tick tick
TICK tick tick tick tick tick
TICK tick tick tick tick tick
TICK tick tick tick tick tick
TICK tick tick tick tick tick
TICK tick tick tick^C
```


## Tcl

This code only rings the bell on the high beat,
which occurs at the start of the bar.

```tcl
package require Tcl 8.5

lassign $argv bpm bpb
if {$argc < 2} {set bpb 4}
if {$argc < 1} {set bpm 60}

fconfigure stdout -buffering none
set intervalMS [expr {round(60000.0 / $bpm)}]
set ctr 0

proc beat {} {
    global intervalMS ctr bpb
    after $intervalMS beat      ;# Reschedule first, to encourage minimal drift
    if {[incr ctr] == 1} {
	puts -nonewline "\r\a[string repeat { } [expr {$bpb+4}]]\rTICK"
    } else {
	puts -nonewline "\rtick[string repeat . [expr {$ctr-1}]]"
    }
    if {$ctr >= $bpb} {
	set ctr 0
    }
}

# Run the metronome until the user uses Ctrl+C...
beat
vwait forever
```

It might be executed like this:

```bash
tclsh8.5 metronome.tcl 90 4
```
