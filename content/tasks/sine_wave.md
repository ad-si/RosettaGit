+++
title = "Sine wave"
description = ""
date = 2019-09-22T21:56:06Z
aliases = []
[extra]
id = 21840
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "go",
  "javascript",
  "kotlin",
  "ocaml",
  "perl",
  "perl_6",
  "phix",
  "racket",
  "rexx",
  "scala",
]
+++

## Task

Generate a sine wave:

::# you choose the frequency of the wave
::# generate a sine wave for 5 seconds
::# play sound




## Go

Go lacks audio support in its standard library and, whilst there are third party packages that could be used, an easier approach is to invoke the SoX utility's 'play' command as was done in the second Kotlin example. 

```go
package main

import (
    "fmt"
    "os/exec"
)

func main() {
    synthType := "sine"
    duration := "5"
    frequency := "440"
    cmd := exec.Command("play", "-n", "synth", duration, synthType, frequency)
    err := cmd.Run()
    if err != nil {
        fmt.Println(err)
    }
}
```



## JavaScript


```javascript
let ctx = new (window.AudioContext || window.webkitAudioContext)();
let osc = ctx.createOscillator();
osc.frequency.setValueAtTime(440, ctx.currentTime);
osc.connect(ctx.destination);
osc.start();
osc.stop(ctx.currentTime + 5);
```



## Kotlin


### Using Java Sound API


```scala
// Version 1.2.41

import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioSystem
import kotlin.math.sin
import kotlin.math.PI

fun sineWave(frequency: Int, seconds: Int, sampleRate: Int): ByteArray {
    val samples = seconds * sampleRate
    val result = ByteArray(samples)
    val interval = sampleRate.toDouble() / frequency
    for (i in 0 until samples) {
        val angle = 2.0 * PI * i / interval
        result[i] = (sin(angle) * 127).toByte()
    }
    return result
}

fun main(args: Array<String>) {
    val sampleRate = 44000
    val buffer = sineWave(440, 5, sampleRate)
    val format = AudioFormat(sampleRate.toFloat(), 8, 1, true, true)
    val line = AudioSystem.getSourceDataLine(format)
    with (line) {
        open(format)
        start()
        write(buffer, 0, buffer.size)
        drain()
        close()
    }
}
```



### Invoking SoX

An easier approach invoking the SoX utility's 'play' command which has this stuff built-in. The following was tested on Ubuntu 16.04.

```scala
// Version 1.2.41

fun main(args:Array<String>) {
    val synthType = "sine"
    val duration = "5"
    val frequency = "440"
    val pb = ProcessBuilder("play", "-n", "synth", duration, synthType, frequency)
    pb.directory(null)
    val proc = pb.start()
    proc.waitFor()
}
```




## OCaml


```ocaml
module BA = Bigarray
module BA1 = Bigarray.Array1

let () =
  let samples = 44100 in
  let sample_rate = 44100 in
  let amplitude = 30000. in
  
  let raw = BA1.create BA.int16_signed BA.c_layout samples in

  let two_pi = 6.28318 in
  let increment = 440.0 /. 44100.0 in
  let x = ref 0.0 in

  for i = 0 to samples - 1 do
    raw.{i} <- truncate (amplitude *. sin (!x *. two_pi));
    x := !x +. increment;
  done;
  
  let buffer = SFSoundBuffer.loadFromSamples raw 1 sample_rate in

  let snd = SFSound.create () in
  SFSound.setBuffer snd buffer;
  SFSound.setLoop snd true;
  SFSound.play snd;
  while true do
    SFTime.sleep (SFTime.of_milliseconds 100_l);
  done
```


To run this code in script mode you can add this at the beginning of the file:


```ocaml
#directory "+sfml"
#load "bigarray.cma"
#load "sfml_system.cma"
#load "sfml_audio.cma"
```

Then run:

```txt
ocaml sine_wave.ml
```


Or to compile to native code:

```txt

ocamlopt -I +sfml bigarray.cmxa sfml_system.cmxa sfml_audio.cmxa sine_wave.ml -o sine_wave.exe

```



## Perl


```perl
use Audio::NoiseGen qw(play sine);

Audio::NoiseGen::init() || die 'No access to sound hardware?';

alarm 5;
play( gen => sine( freq => 440 ) );
```



## Perl 6

What a horribly underspecified task. Ah well, gives me lots of wiggle room to cheat in various ways.


```perl6
my ($rows,$cols) = qx/stty size/.words;
my $v = floor $rows / 2;
print "\e[H\e[J", 'Generating sine wave of zero amplitude and zero frequency for 5 seconds...',
  "\e[$v;0H", '_' x $cols;
sleep 5;
say "\e[H\e[J", 'No?, ok how about this:';

use SVG;
my $filename = 'sine.svg';
my $out = open($filename, :w) orelse .die;
$out.say: SVG.serialize(
    svg => [
        width => 400, height => 150, style => 'stroke:rgb(0,0,255)',
        :rect[:width<100%>, :height<100%>, :fill<white>],
        :path[ :fill<none>, :d('M0,25 C36.42,25,63.58,125,100,125 M100,125 C136.42,125,163.58,25,200,25 M200,25 C236.42,25,263.58,125,300,125 M300,125 C336.42,125,363.58,25,400,25') ],
    ],
);
close $out;
say "Sine wave generated to {$filename.IO.absolute}, better open it quickly...";
sleep 5;
unlink $filename;
say 'Oops, too late.';
say 'Still no? Ok how about:';
shell 'play -n -c1 synth 5.0 sin %-12';
```



## Phix


```Phix
atom k32=NULL, xBeep
 
procedure beep(integer frequency, duration=5000)
    if platform()=WINDOWS then
        if k32=NULL then
            k32 = open_dll("kernel32.dll")
            xBeep = define_c_proc(k32, "Beep", {C_INT,C_INT})
        end if
        c_proc(xBeep,{frequency,duration})
    elsif platform()=LINUX then
	system(sprintf("play -n synth %f sine %d", {duration/1000, frequency}))
    end if
end procedure
 
beep(prompt_number("Enter Frequency (100..10000 recommended):",{0x25,0x7FFF}))
```



## Racket

See https://docs.racket-lang.org/rsound/index.html

```Racket

#lang racket
(require rsound)
; 440 Hz, 50% volume, 5 seconds
(play (make-tone 440 0.50 (* 5 FRAME-RATE)))

```



## REXX

Note:   This REXX program will <u>only</u> work for PC/REXX or Personal REXX.

```REXX
/*REXX program  produces  a sine wave  (of a specified frequency)   for   N   seconds.  */
parse arg freq time .                            /*obtain optional arguments from the CL*/
if freq=='' | freq==","  then freq= 880          /*Not specified?  Then use the default.*/
if time=='' | time==","  then time=   5          /* "      "         "   "   "     "    */
call sound freq, time                            /*invoke a BIF to generate a sine wave.*/
exit 0                                           /*stick a fork in it,  we're all done. */
```

<br<


## Scala


```Scala
import javax.sound.sampled.{AudioFormat, AudioSystem, SourceDataLine}

import scala.math.{Pi, sin}

object SineWave extends App {
  val sampleRate = 44000
  val buffer = beep(440, 5, sampleRate)
  val line: SourceDataLine = AudioSystem.getSourceDataLine(format)

  def format = new AudioFormat(sampleRate.toFloat, 8, 1, true, false)

  def beep(frequency: Int, seconds: Int, sampleRate: Int) = {
    val samples = seconds * sampleRate
    val interval = sampleRate / frequency
    val angle = 2.0 * Pi  / interval
    (0 until samples).map(i => (sin(angle * i) * 127).toByte).toArray
  }

  line.open()
  line.start()
  line.write(buffer, 0, buffer.length)
  line.drain()
  line.close()

}
```

