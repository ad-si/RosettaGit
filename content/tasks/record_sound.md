+++
title = "Record sound"
description = ""
date = 2019-05-12T12:21:28Z
aliases = []
[extra]
id = 9054
[taxonomies]
categories = ["task", "Sound"]
tags = []
languages = [
  "autohotkey",
  "bbc_basic",
  "c",
  "chuck",
  "common_lisp",
  "cpp",
  "go",
  "guiss",
  "julia",
  "kotlin",
  "liberty_basic",
  "livecode",
  "mathematica",
  "nim",
  "ocaml",
  "picolisp",
  "python",
  "racket",
  "scala",
  "tcl",
  "wee_basic",
]
+++

## Task

{{task|Sound}} [[Category:Temporal media]]
Record a monophonic 16-bit PCM sound into either memory space, a file or array.

(This task neglects to specify the sample rate, and whether to use signed samples.
The programs in this page might use signed 16-bit or unsigned 16-bit samples, at 8000 Hz, 44100 Hz, or any other sample rate.
Therefore, these programs might not record sound in the same format.)


## AutoHotkey


```autohotkey
name := "sample"
waitsec := 5
Tooltip Recording %name%.wav
MCI_SendString("close all wait")
MCI_SendString("open new type waveaudio alias " . name)
MCI_SendString("set " . name . " time format ms wait")
;MCI_SendString("set " . name . " bitspersample 16 wait")
;MCI_SendString("set " . name . " channels 1 wait")
;MCI_SendString("set " . name . " samplespersec 16000 wait")
;MCI_SendString("set " . name . " alignment 1 wait")
;MCI_SendString("set " . name . " bytespersec 8000 wait")
MCI_SendString("record " . name)
Sleep waitsec*1000
MCI_SendString("stop " . name . " wait")
MCI_SendString("save " . name . " """ . name . ".wav""")
Tooltip Finished ... Playing
MCI_SendString("delete " . name)
MCI_SendString("close " . name . " wait")
MCI_SendString("open """ . name . ".wav"" type waveaudio alias " . name)
MCI_SendString("play " . name . " wait")
MCI_SendString("close " . name . " wait")
Tooltip
Return

MCI_SendString(p_lpszCommand,ByRef r_lpszReturnString="",p_hwndCallback=0) {
	VarSetCapacity(r_lpszReturnString,512,0)
	Return DllCall("winmm.dll\mciSendString" . (A_IsUnicode ? "W":"A")
		,"Str",p_lpszCommand						;-- lpszCommand
		,"Str",r_lpszReturnString					;-- lpszReturnString
		,"UInt",512									;-- cchReturn
		,A_PtrSize ? "Ptr":"UInt",p_hwndCallback	;-- hwndCallback
		,"Cdecl Int")								;-- Return type
}
; For more intuitive functions, see the MCI library by jballi.
; doc: http://www.autohotkey.net/~jballi/MCI/v1.1/MCI.html
; download: http://www.autohotkey.net/~jballi/MCI/v1.1/MCI.ahk
```



## BBC BASIC

```bbcbasic
      wavfile$ = @dir$ + "capture.wav"
      bitspersample% = 16
      channels% = 2
      samplespersec% = 44100

      alignment% = bitspersample% * channels% / 8
      bytespersec% = alignment% * samplespersec%

      params$ = " bitspersample " + STR$(bitspersample%) + \
      \         " channels " + STR$(channels%) + \
      \         " alignment " + STR$(alignment%) + \
      \         " samplespersec " + STR$(samplespersec%) + \
      \         " bytespersec " + STR$(bytespersec%)

      SYS "mciSendString", "close all", 0, 0, 0
      SYS "mciSendString", "open new type waveaudio alias capture", 0, 0, 0
      SYS "mciSendString", "set capture" + params$, 0, 0, 0 TO res%
      IF res% ERROR 100, "Couldn't set capture parameters: " + STR$(res% AND &FFFF)

      PRINT "Press SPACE to start recording..."
      REPEAT UNTIL INKEY(1) = 32

      SYS "mciSendString", "record capture", 0, 0, 0 TO res%
      IF res% ERROR 100, "Couldn't start audio capture: " + STR$(res% AND &FFFF)

      PRINT "Recording, press SPACE to stop..."
      REPEAT UNTIL INKEY(1) = 32

      SYS "mciSendString", "stop capture", 0, 0, 0
      SYS "mciSendString", "save capture " + wavfile$, 0, 0, 0 TO res%
      IF res% ERROR 100, "Couldn't save to WAV file: " + STR$(res% AND &FFFF)

      SYS "mciSendString", "delete capture", 0, 0, 0
      SYS "mciSendString", "close capture", 0, 0, 0

      PRINT "Captured audio is stored in " wavfile$
```


## C


Read/write raw device <code>/dev/dsp</code>.  On Linux you need access to said device, meaning probably you should be in audio user group.


```cpp
#include <iostream>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>

void * record(size_t bytes)
{
	int fd;
	if (-1 == (fd = open("/dev/dsp", O_RDONLY))) return 0;
	void *a = malloc(bytes);
	read(fd, a, bytes);
	close(fd);
	return a;
}

int play(void *buf, size_t len)
{
	int fd;
	if (-1 == (fd = open("/dev/dsp", O_WRONLY))) return 0;
	write(fd, buf, len);
	close(fd);
	return 1;
}

int main()
{
	void *p = record(65536);
	play(p, 65536);
	return 0;
}
```



## C++

Uses Windows MCI

```cpp

#include <iostream>
#include <string>
#include <windows.h>
#include <mmsystem.h>

#pragma comment ( lib, "winmm.lib" )
using namespace std;

class recorder
{
public:
    void start()
    {
	paused = rec = false; action = "IDLE";
	while( true )
	{
	    cout << endl << "==" << action << "==" << endl << endl;
	    cout << "1) Record" << endl << "2) Play" << endl << "3) Pause" << endl << "4) Stop" << endl << "5) Quit" << endl;
	    char c; cin >> c;
	    if( c > '0' && c < '6' )
	    {
		switch( c )
		{
		    case '1': record(); break;
		    case '2': play();   break;
		    case '3': pause();  break;
		    case '4': stop();   break;
		    case '5': stop();   return;
		}
	    }
	}
    }
private:
    void record()
    {
	if( mciExecute( "open new type waveaudio alias my_sound") )
	{
	    mciExecute( "record my_sound" );
	    action = "RECORDING"; rec = true;
	}
    }
    void play()
    {
	if( paused )
	    mciExecute( "play my_sound" );
	else
	    if( mciExecute( "open tmp.wav alias my_sound" ) )
		mciExecute( "play my_sound" );

	action = "PLAYING";
	paused = false;
    }
    void pause()
    {
	if( rec ) return;
	mciExecute( "pause my_sound" );
	paused = true; action = "PAUSED";
    }
    void stop()
    {
	if( rec )
	{
	    mciExecute( "stop my_sound" );
	    mciExecute( "save my_sound tmp.wav" );
	    mciExecute( "close my_sound" );
	    action = "IDLE"; rec = false;
	}
	else
	{
	    mciExecute( "stop my_sound" );
	    mciExecute( "close my_sound" );
	    action = "IDLE";
	}
    }
    bool mciExecute( string cmd )
    {
	if( mciSendString( cmd.c_str(), NULL, 0, NULL ) )
	{
	    cout << "Can't do this: " << cmd << endl;
	    return false;
	}
	return true;
    }

    bool paused, rec;
    string action;
};

int main( int argc, char* argv[] )
{
    recorder r; r.start();
    return 0;
}

```



## ChucK



```c
// chuck this with other shreds to record to file
// example> chuck foo.ck bar.ck rec

// arguments: rec:<filename>

// get name
me.arg(0) => string filename;
if( filename.length() == 0 ) "foo.wav" => filename;

// pull samples from the dac
dac => Gain g => WvOut w => blackhole;
// this is the output file name
filename => w.wavFilename;
<<<"writing to file:", "'" + w.filename() + "'">>>;
// any gain you want for the output
.5 => g.gain;

// temporary workaround to automatically close file on remove-shred
null @=> w;

// infinite time loop...
// ctrl-c will stop it, or modify to desired duration
while( true ) 1::second => now;
```



## Common Lisp

```lisp

(defun record (n)
  (with-open-file (in "/dev/dsp" :element-type '(unsigned-byte 8))
    (loop repeat n collect (read-byte in))
    )
  )
(defun play (byte-list)
  (with-open-file (out "/dev/dsp" :direction :output :element-type '(unsigned-byte 8) :if-exists :append)
    (mapcar (lambda (b) (write-byte b out)) byte-list)
    )
  )
(play (record 65536))

```



## Go

As Go does not have any audio support in its standard library, this invokes the 'arecord' command-line utility to record sound from the internal microphone (assuming there's no other audio input device present) and saves it to a monophonic, signed 16-bit .wav file. It then plays it back using the 'aplay' utility.

The file name, sampling rate and duration can all be set by the user.

```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
    "os/exec"
    "strconv"
)

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    name := ""
    for name == "" {
        fmt.Print("Enter output file name (without extension) : ")
        scanner.Scan()
        name = scanner.Text()
        check(scanner.Err())
    }
    name += ".wav"

    rate := 0
    for rate < 2000 || rate > 192000 {
        fmt.Print("Enter sampling rate in Hz (2000 to 192000) : ")
        scanner.Scan()
        input := scanner.Text()
        check(scanner.Err())
        rate, _ = strconv.Atoi(input)
    }
    rateS := strconv.Itoa(rate)

    dur := 0.0
    for dur < 5 || dur > 30 {
        fmt.Print("Enter duration in seconds (5 to 30)        : ")
        scanner.Scan()
        input := scanner.Text()
        check(scanner.Err())
        dur, _ = strconv.ParseFloat(input, 64)
    }
    durS := strconv.FormatFloat(dur, 'f', -1, 64)

    fmt.Println("OK, start speaking now...")
    // Default arguments: -c 1, -t wav. Note only signed 16 bit format supported.
    args := []string{"-r", rateS, "-f", "S16_LE", "-d", durS, name}
    cmd := exec.Command("arecord", args...)
    err := cmd.Run()
    check(err)

    fmt.Printf("'%s' created on disk and will now be played back...\n", name)
    cmd = exec.Command("aplay", name)
    err = cmd.Run()
    check(err)
    fmt.Println("Play-back completed.")
}
```



## GUISS


Here we activate the Microsoft Windows '95 Sound Recorder:


```guiss
Start,Programs,Accessories,Sound Recorder,Button:Record
```



## Julia


```Julia

using PortAudio, LibSndFile

stream = PortAudioStream("Microphone (USB Microphone)", 1, 0) # 44100 samples/sec
buf = read(stream, 441000)
save("recorded10sec.wav", buf)

```



## Kotlin

```scala
// version 1.1.3

import java.io.File
import javax.sound.sampled.*

const val RECORD_TIME = 20000L // twenty seconds say

fun main(args: Array<String>) {
    val wavFile = File("RecordAudio.wav")
    val fileType = AudioFileFormat.Type.WAVE
    val format = AudioFormat(16000.0f, 16, 2, true, true)
    val info = DataLine.Info(TargetDataLine::class.java, format)
    val line = AudioSystem.getLine(info) as TargetDataLine

    // Creates a new thread that waits for 'RECORD_TIME' before stopping
    Thread(object: Runnable {
        override fun run() {
            try {
                Thread.sleep(RECORD_TIME)
            }
            catch (ie: InterruptedException) {
                println(ie.message)
            }
            finally {
                line.stop()
                line.close()
            }
            println("Finished")
         }
    }).start()

    // Captures the sound and saves it in a WAV file
    try {
        if (AudioSystem.isLineSupported(info)) {
            line.open(format)
            line.start()
            println("Recording started")
            AudioSystem.write(AudioInputStream(line), fileType, wavFile)
        }
        else println("Line not supported")
    }
    catch (lue: LineUnavailableException) {
        println(lue.message)
    }
}
```



## Liberty BASIC

LB can easily send a MCI string to the OS, or extra routines eg SOX, so a minimal solution could be

```lb

run "sndrec32.exe"

```

A more direct way is..

```lb

    print "Starting 5 sec. recording..."
    r$ =mciSendString$( "open new type waveaudio alias capture")
    r$ =mciSendString$( "set capture time format ms bitspersample 16")
    r$ =mciSendString$( "set capture channels 1 samplespersec 8000")
    r$ =mciSendString$( "set capture alignment 1 bytespersec 8000")
    r$ =mciSendString$( "record capture")
    timer 5000, [on]
    wait
  [on]
    timer 0
    print "     .... now stopping the recording."
    r$ =mciSendString$( "stop capture")
    r$ =mciSendString$( "save capture " +chr$( 34) +"sample.wav" +chr$( 34))
    r$ =mciSendString$( "close capture")
    print "Done recording."
    r$=mciSendString$( "open " +q$ +"sample.wav" +q$ +" type waveaudio alias sfx")
    r$=mciSendString$( "play sfx wait")
    r$=mciSendString$( "close sfx")
    print "Done playing back."
    end

function mciSendString$( s$)
    print s$
    buffer$ =space$( 1024) +chr$( 0)
    calldll #winmm, "mciSendStringA", s$ as ptr, buffer$ as ptr, 1028 as long, 0 as long, r as long
    buffer$ =left$( buffer$, instr( buffer$, chr$( 0)) -1)
    if r >0 then
        mciSendString$ ="error"
        print "returned "; mciSendString$
    else
        mciSendString$ =""'buffer$
        print "OK"
    end if
end function

```




## LiveCode

This example sets some aspects of a sound recording individually and also shows use of the sound input dialog where a user can conveniently set them as well, either may be used.
```LiveCode
command makeRecording
    set the dontUseQT to false -- on windows use true
    set the recordFormat to "wave" -- can be wav,aiff, au
    set the recordRate to 44.1 -- sample at 44100 Hz
    set the recordSampleSize to 16  --default is 8 bit
    ask file "Save recording as"
    if it is not empty then
        answer record --show sound input dialog with presets above
        record sound file it  -- actual record command
        wait 10 seconds
        stop recording
    end if
end makeRecording
```



## Mathematica


```Mathematica
SystemDialogInput["RecordSound"]
```



## Nim

```nim
proc record(bytes): auto =
  var f = open("/dev/dsp")
  result = newSeq[int8](bytes)
  discard f.readBytes(result, 0, bytes)

proc play(buf) =
  var f = open("/dev/dsp", fmWrite)
  f.write(buf)
  f.close

var p = record(65536)
play(p)
```



## OCaml

```ocaml
#load "unix.cma"

let record bytes =
  let buf = String.make bytes '\000' in
  let ic = open_in "/dev/dsp" in
  let chunk = 4096 in
  for i = 0 to pred (bytes / chunk) do
    ignore (input ic buf (i * chunk) chunk)
  done;
  close_in ic;
  (buf)

let play buf len =
  let oc = open_out "/dev/dsp" in
  output_string oc buf;
  close_out oc

let () =
  let bytes = 65536 in
  let p = record bytes in
  play p bytes
```



## PicoLisp



```PicoLisp
(in '(rec -q -c1 -tu16 - trim 0 2)  # Record 2 seconds
   (make
      (while (rd 2)
         (link @) ) ) )
```


Output:

```txt
-> (16767 19071 17279 ... 5503 9343 14719)  # 96000 numbers
```



## Python



```python
import pyaudio

chunk = 1024
FORMAT = pyaudio.paInt16
CHANNELS = 1
RATE = 44100

p = pyaudio.PyAudio()

stream = p.open(format = FORMAT,
                channels = CHANNELS,
                rate = RATE,
                input = True,
                frames_per_buffer = chunk)

data = stream.read(chunk)
print [ord(i) for i in data]
```



## Racket

```racket

#lang racket
(define (record n) (with-input-from-file "/dev/dsp" ( () (read-bytes n))))
(define (play bs)  (display-to-file bs "/dev/dsp" #:exists 'append))
(play (record 65536))

```



## Scala

```Scala
import java.io.{File, IOException}
import javax.sound.sampled.{AudioFileFormat, AudioFormat, AudioInputStream}
import javax.sound.sampled.{AudioSystem, DataLine, LineUnavailableException, TargetDataLine}

object SoundRecorder extends App {
  // record duration, in milliseconds
  final val RECORD_TIME = 60000 // 1 minute

  // path and format of the wav file
  val (wavFile, fileType) = (new File("RecordAudio.wav"), AudioFileFormat.Type.WAVE)
  val format = new AudioFormat(/*sampleRate =*/ 16000f,
    /*sampleSizeInBits =*/ 16,
    /*channels =*/ 2,
    /*signed =*/ true,
    /*bigEndian =*/ true)

  val info = new DataLine.Info(classOf[TargetDataLine], format)
  val line: TargetDataLine = AudioSystem.getLine(info).asInstanceOf[TargetDataLine]

  // Entry to run the program

  // Creates a new thread that waits for a specified of time before stopping
  new Thread(new Runnable() {
    def run() {
      try {
        Thread.sleep(RECORD_TIME)
      } catch {
        case ex: InterruptedException => ex.printStackTrace()
      }
      finally {
        line.stop()
        line.close()
      }
      println("Finished")
    }
  }).start()

  //Captures the sound and record into a WAV file
  try {
    // checks if system supports the data line
    if (AudioSystem.isLineSupported(info)) {
      line.open(format)
      line.start() // start capturing
      println("Recording started")
      AudioSystem.write(new AudioInputStream(line), fileType, wavFile)
    } else println("Line not supported")
  } catch {
    case ex: LineUnavailableException => ex.printStackTrace()
    case ioe: IOException => ioe.printStackTrace()
  }
}
```



## Tcl

```tcl
package require sound

# Helper to do a responsive wait
proc delay t {after $t {set ::doneDelay ok}; vwait ::doneDelay}

# Make an in-memory recording object
set recording [snack::sound -encoding "Lin16" -rate 44100 -channels 1]

# Set it doing the recording, wait for a second, and stop
$recording record -append true
delay 1000
$recording stop

# Convert the internal buffer to viewable numbers, and print them out
binary scan [$recording data -byteorder littleEndian] s* words
puts [join $words ", "]

# Destroy the recording object
$recording destroy
```



## Wee Basic


```Wee Basic
print 1 "Recording..."
micrec
print 1 "Playing..."
micpla
end
```

