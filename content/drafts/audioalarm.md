+++
title = "AudioAlarm"
description = ""
date = 2019-07-07T13:47:53Z
aliases = []
[extra]
id = 10136
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
'''AudioAlarm''' is a program that asks the user to enter a certain number, representing a number of seconds. After the user enters the number, the program will ask the user to enter the name of an MP3 audio file (without the .mp3 file extension). The program will then display a (usually blank) page. After the time (indicated by the number) is up, a sound (indicated by the MP3 file) will go off. Useful for timers and alarm clocks. The app must be installed in its own folder, preferrably with a name like ''AudioAlarm''. To install a sound on the app, just copy the MP3 to the app folder you set up. Then, when the app asks you for the filename, you just type in the name without an extension.

## AutoHotkey

Uses Run for maximum compatibility

```AHK
Inputbox, seconds, Seconds, Enter a number of seconds:
FileSelectFile, File, 3, %A_ScriptDir%, File to be played, MP3 Files (*.mp3)
Sleep Seconds*1000
RunWait % file
```


## AWK


```AWK

# syntax: GAWK -f AUDIOALARM.AWK
BEGIN {
    printf("enter seconds to wait: ")
    getline seconds
    if (seconds !~ /^[0-9]+$/ || seconds > 99999) {
      print("error: invalid")
      exit(1)
    }
    printf("enter filename to play: ")
    getline music_filename
    if (music_filename ~ /^ *$/) {
      print("error: invalid")
      exit(1)
    }
    if (toupper(music_filename) !~ /\.(MID|MOV|MP[34]|WAV|WMA)$/) {
      music_filename = music_filename ".MP3"
    }
    system(sprintf("TIMEOUT /T %d",seconds))
    system(sprintf("START \"C:\\PROGRAM FILES\\WINDOWS MEDIA PLAYER\\WMPLAYER.EXE\" \"%s\"",music_filename))
    exit(0)
}

```



## Batch File


```dos

@echo off

:: Get user input
:: %alarm% can have spaces, but cannot have quotation marks ("")
:: %time% has a working range of -1 to 99999 seconds
:input
set /p "time=Input amount of time in seconds to wait: "
set /p "alarm=Input name of alarm: "

:: Check if %time% is an integer with 'set /a'
set /a intcheck=%time%
if %intcheck%==0 goto input

cls
timeout /t %time% /nobreak >nul
start "" "%alarm%.mp3"

```

{{out}}

```txt

Input amount of time in seconds to wait: 5
Input name of alarm: alarm name with spaces

```



## EchoLisp


```scheme

(lib 'timer)
(lib 'audio)

(define (AudioAlarm)
	(display "Time to wake-up" "color:red")
	(audio-show) ;; show audio control
	(audio-play) ;; play it
	)
	
(define (task)
  (define audio-file 
    (read-string "ring0" "audio-file name? ring0/ring1/ring2/dalida"))
  (define seconds (read-number 3600 "countdown (seconds) ?"))
  (audio-src (string-append "./sounds/" audio-file))
  (at seconds 'seconds AudioAlarm))

;; out
(task)

11/2/2016 23:43:42 : AudioAlarm
Time to wake-up ;; + ... 🎼 🎶🎵🎶🎵


```



## Go

As Go does not have any audio support in its standard library, this invokes the SoX utility's 'play' command to play the required .mp3 file.

```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
    "os/exec"
    "strconv"
    "time"
)

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    number := 0
    for number < 1 {
        fmt.Print("Enter number of seconds delay > 0 : ")
        scanner.Scan()
        input := scanner.Text()
        if err := scanner.Err(); err != nil {
            log.Fatal(err)
        }
        number, _ = strconv.Atoi(input)
    }

    filename := ""
    for filename == "" {
        fmt.Print("Enter name of .mp3 file to play (without extension) : ")
        scanner.Scan()
        filename = scanner.Text()
        if err := scanner.Err(); err != nil {
            log.Fatal(err)
        }
    }

    cls := "\033[2J\033[0;0H" // ANSI escape code to clear screen and home cursor
    fmt.Printf("%sAlarm will sound in %d seconds...", cls, number)
    time.Sleep(time.Duration(number) * time.Second)
    fmt.Printf(cls)
    cmd := exec.Command("play", filename+".mp3")
    if err := cmd.Run(); err != nil {
        log.Fatal(err)
    }
}
```



## Java


```Java
import com.sun.javafx.application.PlatformImpl;
import java.io.File;
import java.util.Scanner;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import javafx.scene.media.Media;
import javafx.scene.media.MediaPlayer;

public class AudioAlarm {

    public static void main(String[] args) throws InterruptedException {
        Scanner input = new Scanner(System.in);

        System.out.print("Enter a number of seconds: ");
        int seconds = Integer.parseInt(input.nextLine());

        System.out.print("Enter a filename (must end with .mp3 or .wav): ");
        String audio = input.nextLine();

        TimeUnit.SECONDS.sleep(seconds);

        Media media = new Media(new File(audio).toURI().toString());
        AtomicBoolean stop = new AtomicBoolean();
        Runnable onEnd = () -> stop.set(true);

        PlatformImpl.startup(() -> {}); // To initialize the MediaPlayer.

        MediaPlayer player = new MediaPlayer(media);
        player.setOnEndOfMedia(onEnd);
        player.setOnError(onEnd);
        player.setOnHalted(onEnd);
        player.play();

        while (!stop.get()) {
            Thread.sleep(100);
        }
        System.exit(0); // To stop the JavaFX thread.
    }
}
```


=={{header|JavaScript}}/{{header|HTML}}==

```JavaScript><title> AudioAlarm </title

<script>
    var a=prompt("Enter a number of seconds", "");
    var b=prompt("Enter the name of an MP3 file you have installed in the directory (without extension)", "");
    document.write("<meta http-equiv='refresh' content='"+a+";url="+b+".mp3'>")
</script>
```



## Kotlin

For this to work on Ubuntu, 'glib' and 'libav-tools' need to be installed on your system.

```scala
// version 1.1.51

import javafx.application.Application
import javafx.scene.media.Media
import javafx.scene.media.MediaPlayer
import javafx.stage.Stage
import java.io.File
import java.util.concurrent.TimeUnit

class AudioAlarm : Application() {

    override fun start(primaryStage: Stage) {
        with (primaryStage) {
            title = "Audio Alarm"
            width = 400.0
            height= 400.0
            show()
        }
        TimeUnit.SECONDS.sleep(seconds)
        soundAlarm()
    }

    private fun soundAlarm() {
        val source = File(fileName).toURI().toString()
        val media = Media(source)
        val mediaPlayer = MediaPlayer(media)
        mediaPlayer.play()
    }

    companion object {
        fun create(seconds: Long, fileName: String) {
            AudioAlarm.seconds = seconds
            AudioAlarm.fileName = fileName
            Application.launch(AudioAlarm::class.java)
        }

        private var seconds = 0L
        private var fileName = ""
    }
}

fun main(args: Array<String>) {
    print("Enter number of seconds to wait for alarm to sound : ")
    val seconds = readLine()!!.toLong()
    print("Enter name of MP3 file (without the extension) to sound alarm : ")
    val fileName = readLine()!! + ".mp3"
    AudioAlarm.create(seconds, fileName)
}
```


Sample input:

```txt

Enter number of seconds to wait for alarm to sound : 3
Enter name of MP3 file (without the extension) to sound alarm : alarm

```



## Liberty BASIC

LB can play wav files natively. Here we call the standard Windows Media Player for an MP3.
If not already running, this will add an extra delay...
It will error if the mp3 file does not exist in the specified path.

```lb
nomainwin

prompt "Delay in seconds";     sec$
prompt "MP3 to play as alarm"; mp3$
f$ ="f:\"; mp3$; ".mp3"

timer val( sec$) *100, [done]
wait

[done]
timer 0
run "C:\Program Files\Windows Media Player\wmplayer.exe " +chr$(34) +f$ +chr$(34)

end
```



## Lua

{{Trans|Ruby}}
luasdl2 and luafilesystem libraries required.


```lua
SDL = require "SDL"
mixer = require "SDL.mixer"
lfs = require "lfs"

print("Enter a number of seconds: ")
sec = tonumber(io.read())
print("Enter the MP3 file to be played")
mp3filepath = lfs.currentdir() .. "/" .. io.read() .. ".mp3"

mixer.openAudio(44100, SDL.audioFormat.S16, 1, 1024)
Music = mixer.loadMUS(mp3filepath)
Music:play(1)

print("Press Enter to quit")
io.read()

```



## Phix


```Phix
-- edit this as necessary!!
constant dflt = `C:\Program Files (x86)\Phix\demo\libxlsxwriter\euAllegro\Projects\RobotsR\Scream01.mp3`
integer seconds = prompt_number("Enter a number of seconds: ")
string mp3file = prompt_string("Enter an mp3 filename: ")
if mp3file="" then mp3file=dflt end if
if not file_exists(mp3file) then crash("not found") end if
sleep(seconds)
include syswait.ew
{} = system_open(mp3file)
```



## Pure Data

(Pure Data really needed file upload: A screenshot of the patch would save space and at the same time be self-explanatory, thus making the script superfluous.)

'''Alarm.pd:'''

```txt

#N canvas 623 244 351 664 10;
#X obj 18 19 cnv 15 310 30 empty empty empty 20 12 0 14 -203904 -66577 0;
#X obj 227 183 cnv 15 60 30 empty empty empty 20 12 0 14 -204786 -66577 0;
#X obj 18 433 cnv 15 310 36 empty empty empty 20 12 0 14 -203904 -66577 0;
#X floatatom 155 27 5 0 24 1 : h -;
#X floatatom 197 27 5 0 59 1 : - -;
#X floatatom 239 27 5 0 59 1 hh:mm:ss - -;
#X obj 155 63 * 3600;
#X obj 197 63 * 60;
#X obj 209 340 moses 1;
#X msg 209 375 Alarm;
#X obj 209 397 print;
#X floatatom 77 27 8 0 90000 0 duration duration -;
#X obj 155 107 +;
#X obj 200 129 +;
#X obj 239 63 change;
#X msg 239 85 bang;
#X obj 239 107 s h;
#X obj 197 85 int;
#X obj 35 293 metro 1000;
#X obj 35 340 int;
#X obj 75 340 - 1;
#X obj 35 196 bng 15 250 50 0 empty empty start -7 -7 0 10 -204786 -1 -13381;
#X msg 98 246 stop;
#X obj 98 196 bng 15 250 50 0 empty pause pause -7 -7 0 10 -261234 -1 -86277;
#X obj 158 196 bng 15 250 50 0 empty empty resume -10 -7 0 10 -262130 -1 -83269;
#X obj 62 246 int;
#X obj 100 375 > 0;
#X obj 158 246 int;
#X obj 200 152 s duration;
#X obj 35 216 t b b;
#X text 133 28 <-;
#X floatatom 231 196 8 0 0 2 countdown countdown_in countdown_out;
#X obj 35 397 s countdown_in;
#X obj 209 318 r countdown_out;
#X obj 36 484 openpanel;
#X obj 129 575 readsf~;
#X obj 162 626 dac~;
#X msg 129 506 1;
#X obj 246 565 loadbang;
#X obj 36 449 bng 15 250 50 0 empty select select -10 -7 0 10 -262130 -1 -1;
#X obj 122 449 bng 15 250 50 0 empty empty test -4 -7 0 10 -204786 -1 -1;
#X obj 181 449 hsl 100 15 0 1 0 0 empty volume volume -2 -7 0 10 -261682 -1 -1 4950 0;
#X obj 163 600 *~;
#X text 72 438 ALARM;
#X text 72 449 SOUND;
#X obj 129 484 t b b;
#X msg 36 506 set open \$1;
#X obj 294 449 bng 15 250 50 0 empty empty off -1 -7 0 10 -261234 -1 -1;
#X msg 294 506 0;
#X msg 246 587 \; pd dsp 1 \; volume 0.5 \; select 1;
#X msg 36 528 open *.wav;
#X connect 3 0 6 0;
#X connect 4 0 7 0;
#X connect 4 0 14 0;
#X connect 5 0 13 1;
#X connect 5 0 14 0;
#X connect 6 0 12 0;
#X connect 7 0 17 0;
#X connect 8 0 9 0;
#X connect 8 0 22 0;
#X connect 8 0 45 0;
#X connect 9 0 10 0;
#X connect 11 0 25 1;
#X connect 12 0 13 0;
#X connect 13 0 28 0;
#X connect 14 0 15 0;
#X connect 15 0 16 0;
#X connect 17 0 12 1;
#X connect 18 0 19 0;
#X connect 19 0 20 0;
#X connect 19 0 26 0;
#X connect 19 0 32 0;
#X connect 20 0 19 1;
#X connect 21 0 29 0;
#X connect 22 0 18 0;
#X connect 23 0 22 0;
#X connect 24 0 27 0;
#X connect 25 0 19 1;
#X connect 26 0 27 1;
#X connect 27 0 18 0;
#X connect 29 0 18 0;
#X connect 29 1 25 0;
#X connect 33 0 8 0;
#X connect 34 0 46 0;
#X connect 35 0 42 0;
#X connect 35 1 45 0;
#X connect 37 0 35 0;
#X connect 38 0 49 0;
#X connect 39 0 34 0;
#X connect 40 0 45 0;
#X connect 41 0 42 1;
#X connect 42 0 36 0;
#X connect 42 0 36 1;
#X connect 45 0 37 0;
#X connect 45 1 50 0;
#X connect 46 0 50 0;
#X connect 47 0 48 0;
#X connect 48 0 35 0;
#X connect 50 0 35 0;

```

'''Features:'''
* Pd has built-in WAV file support - mp3 will require an extension
* Time input by hours, minutes and seconds
* Start, pause and resume button
* Sound file selection via file browser dialog, test button and volume control


## Python

Python natively supports playing .wav files (via the [https://docs.python.org/3.4/library/wave.html wave] library), but not .mp3. Therefore, this calls to the OS to start the file. This behaves as though the mp3 file were double-clicked and plays the file with the default system player.

{{works with|Python|3.4.1}}


```python
import time
import os

seconds = input("Enter a number of seconds: ")
sound = input("Enter an mp3 filename: ")

time.sleep(float(seconds))
os.startfile(sound + ".mp3")
```



## Racket

Racket does not currently have native mp3 support so this example uses system to call an external application.

```racket
#lang racket
(display "Time to wait in seconds: ")
(define time (string->number (read-line)))

(display "File Name: ")
(define file-name (read-line))

(when (file-exists? (string->path (string-append file-name ".mp3")))
  (sleep time)
  (system* "/usr/bin/mpg123" (string-append file-name ".mp3")))
```



## REXX


### using SLEEP


```rexx
/*REXX pgm to prompt user for: # (of secs); a name of a MP3 file to play*/

say '──────── Please enter a number of seconds to wait:'
parse pull waitTime .
                         /*add code to verify number is a valid number. */

say '──────── Please enter a name of an MP3 file to play:'
parse pull MP3FILE
                        /*add code to verify answer is a valid filename.*/

call sleep waitTime
MP3FILE'.MP3'
                                       /*stick a fork in it, we're done.*/
```

'''output''' when using the input of: <tt> xxx </tt>


### using spin


```rexx
/*REXX pgm to prompt user for: # (of secs); a name of a MP3 file to play*/

say '──────── Please enter a number of seconds to wait:'
parse pull waitTime .

say '──────── Please enter a name of an MP3 file to play:'
parse pull MP3FILE

call time 'Reset'                      /*reset the REXX (elapsed) timer.*/

   do  until time('E')  >waitTime      /*wait out the clock (in seconds)*/
   end

MP3FILE'.MP3'
                                       /*stick a fork in it, we're done.*/
```



## Ring


```ring

# Project : AudioAlarm

load "stdlib.ring"
see "Delay in seconds: "
give sec
see "MP3 to play as alarm: "
give mp3
f = mp3 + ".mp3"
sleep(sec)
system("C:\Ring\wmplayer.exe C:\Ring\calmosoft\" + f)

```

Output:

[https://www.dropbox.com/s/9l5fef83wj61zw5/AudioAlarm.avi?dl=0 AudioAlarm]


## Ruby

requires mpg123 to be installed.


```ruby
puts "Enter a number of seconds:"
seconds = gets.chomp.to_i
puts "Enter a MP3 file to be played"
mp3filepath = File.dirname(__FILE__) + "/" + gets.chomp + ".mp3"
sleep(seconds)
pid = fork{ exec 'mpg123','-q', mp3filepath }

```



## Tcl

{{libheader|Snack}}

```tcl
package require sound

fconfigure stdout -buffering none
puts -nonewline "How long to wait for (seconds): "
gets stdin delay
puts -nonewline "What file to play: "
gets stdin soundFile

snack::sound snd
snd read $soundFile
after [expr {$delay * 1000}] {snd play -command {set done 1}}
vwait done

catch {snd stop}
snd destroy
puts "all done"
exit
```



## Yabasic


```Yabasic
input "How long to wait for (seconds): " sec
input "What file to play: " song$
wait sec
song$ = song$ + ".mp3"

system("explorer " + song$)
exit 0
```



## zkl

There is no built in sound support so this example is coded for my Linux box.
A change: rather than seconds to wait, a time is used.

```zkl
hms :=Time.Date.parseTime(ask("Time to play mp3 file: "));
file:=ask("File to play: ") + ".mp3";

time:=Time.Clock.localTime.copy();
time[3]=hms[0]; time[4]=hms[1]; time[5]=hms[2];
time=Time.Clock.mktime(time.xplode()) - Time.Clock.time;

println("Play ",file," in ",time," seconds");
Thread.Timer(System.cmd.fp("mplayer "+file),time).go();  // a thread
Atomic.sleep(time+1); // veg out until sound plays, then exit
//to just play the file, no threads:
//Atomic.sleep(time);
//System.cmd("mplayer "+file);

```

{{out}}

```txt

$ zkl ding
Time to play mp3 file: 8:23pm
File to play: ding
Play ding.mp3 in 45 seconds
<45 seconds pass>
MPlayer2 UNKNOWN (C) 2000-2012 MPlayer Team
...
Exiting... (End of file)
$ 

```

