+++
title = "Musical scale"
description = ""
date = 2019-06-04T06:01:05Z
aliases = []
[extra]
id = 13149
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "autohotkey",
  "basic256",
  "befunge",
  "c",
  "clojure",
  "cpp",
  "easylang",
  "forth",
  "go",
  "j",
  "kotlin",
  "lilypond",
  "locomotive_basic",
  "m2000_interpreter",
  "mathematica",
  "oorexx",
  "perl",
  "perl_6",
  "phix",
  "powershell",
  "pure_data",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "scala",
  "sparkling",
  "tcl",
  "ursa",
  "vba",
  "xpl0",
  "zx_spectrum_basic",
]
+++

## Task

Output   8   notes of the   C   major natural diatonic music scale to the default musical sound device on the system. 


(These are the notes     C,D,E,F,G,A,B,c     or     Do, Ra, Me, Fa, So, La, Te, do     on the solfa.) 

For the purpose of this task, middle   C   should be used as the starting note, and crotchet notes should be used.

For languages that cannot utilize a sound device, 
it is permissible to output to a musical score sheet (or midi file), 
or the task can be omitted.





## AutoHotkey

```AutoHotkey
for key, val in [261.63, 293.66, 329.63, 349.23, 392.00, 440.00, 493.88, 523.25]
	SoundBeep, % val, 500
```




## BASIC256

sound {261.63, 500, 293.66, 500, 329.63, 500, 349.23, 500, 392, 500, 440, 500, 493.88, 500, 523.25, 500}


## Befunge

Befunge has no sound support, so this example generates a MIDI file that plays the sequence of notes. The file is written to stdout, so requires an interpreter that can cleanly redirect its output to a file (for example the [[bef]] reference implementation requires the ''-q'' option to suppress its version banner).

The tune to be played is specified on the first line of the code. The notes are specified as MIDI note numbers in reverse order in a Befunge string (for example, ''Middle C'' is note number 60, which is the character "<"). This is followed by the count of notes in the string - 8 in this example.


```befunge>
         "HGECA@><"8:        v
v0*73"MThd"0006010101"MTrk"000<
>>#1-#,:#\_$8*74++,0,28*"'"039v
v,:,\,*2,1"Hd":\<,,,,,,*3"U"*9<
>"@1",2*,\,,1-:#^_"/3d",5*,,, @
```



## C

Although C provides low level access to devices, there is no standardized way. 
Here are illustrated two approaches. 

===Borland's Turbo C===
Borland's Turbo C system has the dos.h header file which contains the functions sound() and nosound(). 
sound() takes an argument which is the frequency of the note to be played through the device speaker. 
delay(), also part of dos.h tells the code how long to suspend execution. 
The sound will however still be playing. 
It is therefore essential to call nosound() at the end which ends the speaker output, otherwise the Turbo C (DOS) session will have to be ended.


```c

#include<stdio.h>
#include<conio.h>
#include<math.h>
#include<dos.h>

typedef struct{
	char str[3];
	int key;
	}note;
	
note sequence[] = {{"Do",0},{"Re",2},{"Mi",4},{"Fa",5},{"So",7},{"La",9},{"Ti",11},{"Do",12}};

int main(void)
{
	int i=0;
	
	while(!kbhit())
	{
		printf("\t%s",sequence[i].str);
		sound(261.63*pow(2,sequence[i].key/12.0));
		delay(sequence[i].key%12==0?500:1000);
		i = (i+1)%8;
		i==0?printf("\n"):printf("");
	}
	nosound();
	return 0;
}
```



### Windows C

I named it Windows C for want of a better name. 
This is actually more constrained than the above example, 
since although it will run on any Windows machine, 
Beep() can only play integer frequencies and thus the tones are noticeably lower than the ones played by sound() above.


```c

#include<windows.h>
#include<stdio.h>
#include<math.h>

typedef struct{
	char str[3];
	int key;
	}note;
	
note sequence[] = {{"Do",0},{"Re",2},{"Mi",4},{"Fa",5},{"So",7},{"La",9},{"Ti",11},{"Do",12}};

int main(void)
{
	int i=0;
	
	while(1)
	{
		printf("\t%s",sequence[i].str);
		Beep(261.63*pow(2,sequence[i].key/12.0),sequence[i].key%12==0?500:1000);
		i = (i+1)%8;
		i==0?printf("\n"):printf("");
	}
	return 0;
}
```



## C++

Uses Windows MIDI device

```cpp

#include <iostream>
#include <windows.h>
#include <mmsystem.h>

#pragma comment ( lib, "winmm.lib" )

typedef unsigned char byte;

typedef union 
{ 
    unsigned long word; 
    unsigned char data[4]; 
}
midi_msg;

class midi
{
public:
    midi()
    {
	if( midiOutOpen( &device, 0, 0, 0, CALLBACK_NULL) != MMSYSERR_NOERROR ) 
	{
	    std::cout << "Error opening MIDI Output..." << std::endl;
	    device = 0;
	}
    }
    ~midi()
    {
	midiOutReset( device );
	midiOutClose( device );
    }
    bool isOpen() { return device != 0; }
    void setInstrument( byte i )
    {
	message.data[0] = 0xc0; message.data[1] = i;
	message.data[2] = 0; message.data[3] = 0;
	midiOutShortMsg( device, message.word );
    }
    void playNote( byte n, unsigned i )
    {
	playNote( n ); Sleep( i ); stopNote( n );
    }

private:
    void playNote( byte n )
    {
	message.data[0] = 0x90; message.data[1] = n;
	message.data[2] = 127; message.data[3] = 0;
	midiOutShortMsg( device, message.word );
    }
    void stopNote( byte n )
    {
	message.data[0] = 0x90; message.data[1] = n;
	message.data[2] = 0; message.data[3] = 0;
	midiOutShortMsg( device, message.word );
    }
    HMIDIOUT device;
    midi_msg message;
};

int main( int argc, char* argv[] )
{
    midi m;
    if( m.isOpen() )
    {
	byte notes[] = { 60, 62, 64, 65, 67, 69, 71, 72 };
	m.setInstrument( 42 );
	for( int x = 0; x < 8; x++ )
	    m.playNote( notes[x], rand() % 100 + 158 );
	Sleep( 1000 );
    }
    return 0;
}

```



## Clojure

```clojure
(use 'overtone.live)

; Define your desired instrument
; Using saw-wave from: https://github.com/overtone/overtone/wiki/Chords-and-scales
(definst saw-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4] 
  (* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(defn play [note ms]
  (saw-wave (midi->hz note))
  (Thread/sleep ms))

(doseq [note (scale :c4 :major)] (play note 500))
```



## EasyLang


<lang>n[] = [ 262 294 330 349 392 440 494 523 ]
for i range len n[]
  sound [ n[i] 0.5 ]
  sleep 0.6
.
```



## Forth

As a low level stack language Forth programming methodology prefers short simple definitions that extend the language in the direction that allows you to solve the problem. The creation of application specific language is common in Forth.
This demonstration code uses the PC speaker to generate musical tones. A simple device driver is created for hardware control via PORT I/O. A set of primitive operations are created to control the on:off times of the sounds.  Then a small MUSIC language is created to create notes of different types. Finally 2 scales are created using "crotcheted notes" as they are called. We chose 1/8 notes.  For fun we added the ability to change the expression of the notes using Italian musical terms.
<lang>HEX
\ PC speaker hardware control (requires giveio or DOSBOX for windows operation)
 042 constant fctrl        061 constant sctrl
 0FC constant smask        043 constant tctrl
 0B6 constant spkr

: sing      ( -- ) sctrl pc@               03 or  sctrl pc! ;
: silence   ( -- ) sctrl pc@   smask and   01 or  sctrl pc! ;

: tone     ( divisor -- )
            ?dup                                         \ check for non-zero input
            if   spkr  tctrl pc!                         \ enable PC speaker
                 dup   fctrl pc!                         \ load low byte
                 8 rshift fctrl pc!                      \ load high byte
                 sing
            else silence
            then ;

DECIMAL
1193181. 2constant clock                                 \ internal oscillator freq. MHz x 10

: Hz ( freq -- divisor) clock rot um/mod nip  ;          \ convert Freq to osc. divisor

\ duration control variables and values
variable on_time
variable off_time
variable feel                                            \ controls the on/off time ratio

60 value tempo

4000 tempo um* 2constant timebase                        \ 1 whole note=4000 ms @ 60 Beats/min

: bpm>ms    ( bpm -- ms) timebase rot um/mod nip ;       \ convert beats per minute to milliseconds
: wholenote ( -- ms )  tempo bpm>ms ;                    \ using tempo set the BPM

: play      ( divisor -- )
            tone on_time @ ms   silence  off_time @ ms ;

: expression ( ms n --)                                  \ adjust the on:off ratio using n
           over swap -  tuck -   ( -- on-mS off-mS )
           off_time !  on_time ! ;                       \ store times in variables

: note      ( -- ms ) on_time @ off_time @ + ;           \ returns duration of current note

: duration!    ( ms -- )  feel @ expression ;

: 50%       ( n -- n/2)    2/ ;
: %         ( n n2  -- n%) 100 */ ;                      \ calculate n2% of n
: 50%+      ( n -- n+50%)  dup 50% + ;                   \ dotted notes have 50% more time

VOCABULARY MUSIC

MUSIC DEFINITIONS
: BPM       ( bpm -- )                                  \ set tempo in beats per minute
            to tempo
            wholenote duration! ;

: legato      0 feel ! ;
: staccatto   note 8 %  feel ! ;
: Marcato     note 3 %  feel ! ;

: 1/1      wholenote      duration! ;
: 1/2      wholenote 50%  duration! ;
: 1/2.     1/2  note 50%+ duration! ;
: 1/4      1/2  note 50%  duration! ;
: 1/4.     1/4  note 50%+ duration! ;
: 1/8      1/4  note 50%  duration! ;
: 1/8.     1/8  note 50%+ duration! ;
: 1/16     1/8  note 50%  duration! ;
: 1/32     1/16 note 50%  duration! ;
: rest     note ms ;

\ note object creator
: note:    create  hz ,                    \ compile time: compile divisor into the note
           does>  @ play ;                 \ run time: fetch the value and play the tone

\ freq  Natural    Freq  Accidental    En-harmonic
\ -------------    ----------------   ----------------
  131 note: C3     139 note: C#3       synonym Db3 C#3
  147 note: D3     156 note: D#3       synonym Eb3 D#3
  165 note: E3
  175 note: F3     185 note: F#3       synonym Gb3 F#3
  196 note: G3     208 note: G#3       synonym Ab3 G#3
  220 note: A3     233 note: A#3       synonym Bb3 A#3
  247 note: B3
  262 note: C4     277 note: C#4       synonym Db4 C#4

: Cmajor      1/8  C3 D3 E3  F3 G3 A3  B3  C4 ;
: Chromatic   1/8  C3 C#3 D3 D#3 E3 F3 F#3 G3 G#3 A3 A#3 B3 C4 ;

```

Interactive test at the Console 

```txt
music ok
120 bpm legato cmajor ok
200 bpm marcato chromatic ok
72 bpm legato c3 eb3 g3 c4 ok 

```



## Go

As Go doesn't have any audio support in its standard library, we instead build a .wav file which can then be played using a utility such as SoX.

```go
package main

import (
    "encoding/binary"
    "log"
    "math"
    "os"
    "strings"
)

func main() {
    const (
        sampleRate = 44100
        duration   = 8
        dataLength = sampleRate * duration
        hdrSize    = 44
        fileLen    = dataLength + hdrSize - 8
    )

    // buffers
    buf1 := make([]byte, 1)
    buf2 := make([]byte, 2)
    buf4 := make([]byte, 4)

    // WAV header
    var sb strings.Builder
    sb.WriteString("RIFF")
    binary.LittleEndian.PutUint32(buf4, fileLen)
    sb.Write(buf4) // file size - 8
    sb.WriteString("WAVE")
    sb.WriteString("fmt ")
    binary.LittleEndian.PutUint32(buf4, 16)
    sb.Write(buf4) // length of format data (= 16)
    binary.LittleEndian.PutUint16(buf2, 1)
    sb.Write(buf2) // type of format (= 1 (PCM))
    sb.Write(buf2) // number of channels (= 1)
    binary.LittleEndian.PutUint32(buf4, sampleRate)
    sb.Write(buf4) // sample rate
    sb.Write(buf4) // sample rate * bps(8) * channels(1) / 8 (= sample rate)
    sb.Write(buf2) // bps(8) * channels(1) / 8  (= 1)
    binary.LittleEndian.PutUint16(buf2, 8)
    sb.Write(buf2) // bits per sample (bps) (= 8)
    sb.WriteString("data")
    binary.LittleEndian.PutUint32(buf4, dataLength)
    sb.Write(buf4) // size of data section
    wavhdr := []byte(sb.String())

    // write WAV header
    f, err := os.Create("notes.wav")
    if err != nil {
        log.Fatal(err)
    }
    defer f.Close()
    f.Write(wavhdr)

    // compute and write actual data
    freqs := [8]float64{261.6, 293.6, 329.6, 349.2, 392.0, 440.0, 493.9, 523.3}
    for j := 0; j < duration; j++ {
        freq := freqs[j]
        omega := 2 * math.Pi * freq
        for i := 0; i < dataLength/duration; i++ {
            y := 32 * math.Sin(omega*float64(i)/float64(sampleRate))
            buf1[0] = byte(math.Round(y))
            f.Write(buf1)
        }
    }
}
```



## J



```J
require'media/wav'
0.25 wavnote 0 2 4 5 7 9 11 12
```


This assumes a version such as J6 which supports media/wav.

0=C, 1=C#, 2=D, ... of main octave

0.25 is the duration of each note (in seconds).


## Kotlin

This uses the same frequencies and duration as the Python entry and works fine on Windows 10.

When building win32.klib from windows.h,  one needs to make sure NOT to filter out utilapiset.h because this is where the Beep function now resides, not in winbase.h as stated in the MSDN documentation.

```scala
// Kotlin Native v0.3

import kotlinx.cinterop.*
import win32.*

fun main(args: Array<String>) {
    val freqs = intArrayOf(262, 294, 330, 349, 392, 440, 494, 523)  // CDEFGABc
    val dur = 500
    repeat(5) { for (freq in freqs) Beep(freq, dur) }   
}
```



## Lilypond

The lilypond tool produces musical score sheets and midi files - if asked for - but does not output notes to the sound device directly.

```lilypond
% Start at middle C
\relative c' {
  c d e f
  g a b c
}
```



## Locomotive Basic



```locobasic
10 mode 1
20 print "Note","Freq. (Hz)","Period"
30 ' program loop:
40 if sq(1)<128 then gosub 70  ' play next note if channel is inactive
50 goto 40
60 ' play next note
70 read n
80 if n<0 then end
90 note=note+1
100 ' calculation from chapter 7, page 26 of the CPC manual:
110 f=440*(2^((n-10)/12))
120 p=round(62500/f)
130 print mid$("cdefgabc",note,1),f,p
140 sound 1,p,100
150 return
160 data 1,3,5,6,8,10,12,13,-1
```


## M2000 Interpreter

Score make an internal bank (replace a previous one), on a voice, (1 to 16), where 10 is for drum machine.
Play assign a midi organ to a score and start play, in a "music" thread. We can can use Play 0 to stop all scores, or Play number, 0 to stop as specific score. Beat value 300 is in milliseconds, so play each not in Tune each 300 milliseconds, and the same for Score (scores may use @1 to @6 to play 300/1 to 300/32 for specific note, and can use V1 to V127 for volume control per note). Spaces in strings are pauses, and for scores we can use @1 to @6 to reduce pause value). We can use a thread to send a drum score every some seconds, to play a rhythm. Thread { score 10... : play 10,10 ....} as drums interval 1000  (second value for play 10 maybe 0 or any other 1 to 127 but always assign the drum machine. [https://en.wikipedia.org/wiki/General_MIDI Midi]

TUNE use kernel [https://msdn.microsoft.com/en-us/library/windows/desktop/ms679277(v=vs.85).aspx  Beep] which is synchronous and not leaving M2000 threads to process, until ends.

<lang>
Module checkit {
      \\ using internal speaker
      TUNE 300, "C3DEFGABC4"
      TUNE 300, "C3C#DD#EFF#GG#AA#BC4"
      Thread {
            score 10, 100,  "CAC"
            Play 10, 1
      } as drums interval 1000
      \\ Play in background (16 scores - no 10 for drum machine)
      SCORE 1, 300, "C3DEFGABC4"
      PLAY 1, 19  ' use score 1 with organ 19
      Wait 2400
}
checkit

```



## Mathematica


```mathematica
EmitSound@Sound[SoundNote /@ {0, 2, 4, 5, 7, 9, 11, 12}]
```



## ooRexx


```ooRexx
/* REXX ---------------------------------------------------------------
* 24.02.2013 Walter Pachl derived from original REXX version
* Changes: sound(f,sec) --> beep(trunc(f),millisec)
*          $ -> sc
*          @. -> f.
*          re > ra (in sc)
*--------------------------------------------------------------------*/
  sc='do ra mi fa so la te do'
  dur=1250                          /* milliseconds                   */
  Do j=1 For words(sc)              /* sound each "note" in the string*/
    Call notes word(sc,j),dur       /* invoke a subroutine for sounds.*/
    End                             /* j                              */
  Exit                              /* stick a fork in it, we're done.*/
notes: Procedure
  Arg note,dur
  f.=0                              /* define common names for sounds.*/
  f.la=220
  f.si=246.94
  f.te=f.si
  f.ta=f.te
  f.ti=f.te
  f.do=261.6256
  f.ut=f.do
  f.ra=293.66
  f.re=f.ra     /* re is to be a synonym for ra */
  f.mi=329.63
  f.ma=f.mi
  f.fa=349.23
  f.so=392
  f.sol=f.so
  Say note trunc(f.note) dur
  If f.note\==0 Then
    Call beep trunc(f.note),dur     /* sound the "note".              */
  Return
```



## Perl


```perl
use MIDI::Simple;

# setup, 1 quarter note is 0.5 seconds (500,000 microseconds)
set_tempo 500_000;

# C-major scale
n 60; n 62; n 64; n 65; n 67; n 69; n 71; n 72;

write_score 'scale.mid';
```



## Perl 6


```perl6
for 0,2,4,5,7,9,11,12 {
    shell "play -n -c1 synth 0.2 sin %{$_ - 9}"
}
```



## Phix


###  version 1 

```Phix
atom xBeep = 0

procedure beep(integer fi)
    if platform()=WINDOWS then
        integer frequency = floor(261.63 * power(2, fi/12)),
                duration = iff(fi == 12 ? 1000 : 500)
        if xBeep=0 then
            atom kernel32 = open_dll("kernel32.dll")
            xBeep = define_c_proc(kernel32, "Beep", {C_INT,C_INT})
        end if
        c_proc(xBeep,{frequency,duration})
    elsif platform()=LINUX then
        string play = sprintf("play -n -c1 synth 0.2 sin %%%d",fi-9)
        system(play)
    end if
end procedure

printf(1,"Please don't shoot the piano player, he's doing the best that he can!\n")
constant f = {0, 2, 4, 5, 7, 9, 11, 12}
for i=1 to length(f) do
    beep(f[i])
end for
printf(1,"That's all\n")
```


###  version 2 

```Phix
constant sample_rate = 44100,
         duration = 8,
         dataLength = sample_rate * duration,
         hdrSize = 44,
         fileLen = dataLength + hdrSize - 8,
         freqs = { 261.6, 293.6, 329.6, 349.2, 392.0, 440.0, 493.9, 523.3 }       
         wavhdr = "RIFF"&
                  int_to_bytes(fileLen,4)&
                  "WAVE"&
                  "fmt "&
                  int_to_bytes(16,4)&           -- length of format data (= 16)
                  int_to_bytes(1,2)&            -- type of format (= 1 (PCM))
                  int_to_bytes(1,2)&            -- number of channels (= 1)
                  int_to_bytes(sample_rate,4)&  -- sample rate
                  int_to_bytes(sample_rate,4)&  -- sample rate * bps(8) * channels(1) / 8 (= sample rate)
                  int_to_bytes(1,2)&            -- bps(8) * channels(1) / 8  (= 1)
                  int_to_bytes(8,2)&            -- bits per sample (bps) (= 8)
                  "data"&               
                  int_to_bytes(dataLength,4),   -- size of data section
        if length(wavhdr)!=hdrSize then ?9/0 end if -- sanity check

integer fn = open("notes.wav", "wb")
puts(fn, wavhdr)
for j=1 to duration do
    atom omega = 2 * PI * freqs[j]
    for i=0 to dataLength/duration-1 do
        atom y = 32 * sin(omega * i / sample_rate)
        integer byte = and_bits(y,#FF)
        puts(fn,byte)
    end for
end for
close(fn)

if platform()=WINDOWS then
    system("notes.wav")
elsif platform()=LINUX then
    system("aplay notes.wav")
end if
```



## PowerShell

List of frequencies directly taken from the Python example.

```powershell
$frequencies = 261.63, 293.66, 329.63, 349.23, 392.00, 440.00, 493.88, 523.25
foreach($tone in $frequencies){
    [Console]::beep($tone, 500)
}
```



## Pure Data

'''scale.pd'''

```txt

#N canvas 898 363 360 460 10;
#X obj 63 21 bng 15 250 50 0 empty start start 17 7 0 10 -262144 -1 -1;
#X floatatom 135 21 5 0 0 1 bpm bpm -;
#X obj 135 40 expr 1000 / ($f1/60);
#X obj 135 62 int;
#X obj 117 123 + 1;
#X obj 117 145 mod 9;
#X obj 63 123 int 1;
#X obj 63 176 hradio 15 1 0 9 empty empty empty 0 -8 0 10 -262144 -1 -1 0;
#X obj 63 196 route 0 1 2 3 4 5 6 7;
#X msg 15 248 0;
#X msg 93 248 62;
#X msg 123 248 64;
#X msg 63 248 60;
#X msg 153 248 65;
#X msg 183 248 67;
#X msg 213 248 69;
#X msg 243 248 71;
#X msg 273 248 72;
#X obj 111 313 mtof;
#X obj 84 357 osc~;
#X obj 84 384 dac~;
#X obj 237 323 loadbang;
#X obj 63 101 metro;
#X msg 237 345 \; pd dsp 1 \; bpm 136 \; start 1;
#X connect 0 0 22 0;
#X connect 1 0 2 0;
#X connect 2 0 3 0;
#X connect 3 0 22 1;
#X connect 4 0 5 0;
#X connect 5 0 6 1;
#X connect 6 0 4 0;
#X connect 6 0 7 0;
#X connect 7 0 8 0;
#X connect 8 0 9 0;
#X connect 8 1 12 0;
#X connect 8 2 10 0;
#X connect 8 3 11 0;
#X connect 8 4 13 0;
#X connect 8 5 14 0;
#X connect 8 6 15 0;
#X connect 8 7 16 0;
#X connect 8 8 17 0;
#X connect 9 0 19 0;
#X connect 9 0 22 0;
#X connect 10 0 18 0;
#X connect 11 0 18 0;
#X connect 12 0 18 0;
#X connect 13 0 18 0;
#X connect 14 0 18 0;
#X connect 15 0 18 0;
#X connect 16 0 18 0;
#X connect 17 0 18 0;
#X connect 18 0 19 0;
#X connect 19 0 20 0;
#X connect 19 0 20 1;
#X connect 21 0 23 0;
#X connect 22 0 6 0;

```



## Python

(Windows)

```python>>>
 import winsound
>>> for note in [261.63, 293.66, 329.63, 349.23, 392.00, 440.00, 493.88, 523.25]:
	winsound.Beep(int(note+.5), 500)	
>>> 
```



## R


```R

install.packages("audio")
library(audio)
hz=c(1635,1835,2060,2183,2450,2750,3087,3270)
for (i in 1:8){
  play(audioSample(sin(1:1000), hz[i]))
  Sys.sleep(.7)
}

```



## Racket

With a quick and dirty WinMM interface.

```racket

#lang racket
(require ffi/unsafe ffi/unsafe/define)
(define-ffi-definer defmm (ffi-lib "Winmm"))
(defmm midiOutOpen (_fun [h : (_ptr o _int32)] [_int = -1] [_pointer = #f]
                         [_pointer = #f] [_int32 = 0] -> _void -> h))
(defmm midiOutShortMsg (_fun _int32 _int32 -> _void))
(define M (midiOutOpen))
(define (midi x y z) (midiOutShortMsg M (+ x (* 256 y) (* 65536 z))))

(for ([i '(60 62 64 65 67 69 71 72)]) (midi #x90 i 127) (sleep 0.5))
(sleep 2)

```



## REXX

```rexx
/*REXX program  sounds  eight notes  of the   C major  natural diatonic  music scale.   */
parse arg !                                      /*obtain optional arguments from the CL*/
if !all( arg() )  then exit                      /*determine which REXX is running,  if */
                                                 /*    any form of help requested, exit.*/
if \!regina & \!pcrexx  then do
                             say "***error***  this program can't execute under:"  !ver
                             exit 13
                             end

$ = 'do ra me fa so la te do'                    /*the words for music scale sounding.  */
dur = 1/4                                        /*define duration as a quarter second. */
           do j=1  for words($)                  /*sound each "note" in the string.     */
           call notes word($,j),dur              /*invoke a subroutine for the sounds.  */
           end   /*j*/                           /* [↑]   sound each of the words.      */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
notes: procedure expose !regina !pcrexx; arg note,dur /*obtain the arguments from list. */
       @.=0                                           /*define common names for sounds. */
       @.la=220;        @.si=246.94;    @.te=@.si;      @.ta=@.te;    @.ti=@.te
       @.do=261.6256;   @.ut=@.do;      @.re=293.66;    @.ra=@.re;    @.mi=329.63
       @.ma=@.mi;       @.fa=349.23;    @.so=392;       @.sol=@.so
       if @.note==0  then return                      /*if frequency is zero,  skip it. */
       if !pcrexx then call  sound @.note,dur         /*sound the note using SOUND bif. */
       if !regina then do                             /* [↓]  reformat some numbers.    */
                       ms=format(dur*1000,,0)         /*Regina requires DUR in millisec.*/
                       intN=format(@.note,,0)         /*   "      "     NOTE is integer.*/
                       call  beep  intN,ms            /*sound the note using  BEEP  BIF.*/
                       end
       return
/*─────────────────────────────general 1─line subroutines and functions────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────*/
!all: !!=!;!=space(!);upper !;call !fid;!nt=right(!var('OS'),2)=='NT';!cls=word('CLS VMFCLEAR CLRSCREEN',1+!cms+!tso*2);if arg(1)\==1 then return 0;if wordpos(!,'? ?SAMPLES ?AUTHOR ?FLOW')==0 then return 0;!call=']$H';call '$H' !fn !;!call=;return 1
!cal: if symbol('!CALL')\=="VAR"  then !call=;                          return !call
!env: !env='ENVIRONMENT';  if !sys=='MSDOS' | !brexx | !r4 | !roo  then !env='SYSTEM';  if !os2  then !env='OS2'!env;  !ebcdic=3=='f3'x;  if !crx  then !env='DOS';                                  return
!fid: parse upper source !sys !fun !fid . 1 . . !fn !ft !fm .;  call !sys;  if !dos  then do;  _=lastpos('\',!fn);  !fm=left(!fn,_);  !fn=substr(!fn,_+1);  parse var !fn !fn '.' !ft;  end;         return word(0 !fn !ft !fm,1+('0'arg(1)))
!rex: parse upper version !ver !vernum !verdate .;  !brexx='BY'==!vernum;  !kexx='KEXX'==!ver;  !pcrexx='REXX/PERSONAL'==!ver | 'REXX/PC'==!ver;  !r4='REXX-R4'==!ver;  !regina='REXX-REGINA'==left(!ver,11);  !roo='REXX-ROO'==!ver;  call !env;  return
!sys: !cms=!sys=='CMS';  !os2=!sys=='OS2';  !tso=!sys=='TSO' | !sys=='MVS';  !vse=!sys=='VSE';  !dos=pos('DOS',!sys)\==0 | pos('WIN',!sys)\==0 | !sys=='CMD';  !crx=left(!sys,6)=='DOSCRX';  call !rex;  return
!var: call !fid;  if !kexx  then return space(dosenv(arg(1)));  return space(value(arg(1),,!env))
```


Programming note:   
The general 1-line subroutines at the end of the REXX program are boilerplate code which:
::* check for invocation with a   '''?'''                   single argument to support general documentation (help).
::* check for invocation with a   '''?SAMPLES'''   single argument to support documentation for sample usages.
::* check for invocation with a   '''?FLOW'''          single argument to support documentation for program logic flow.
::* check for invocation with a   '''?AUTHOR'''      single argument to support showing the author of the REXX pgm.
::* defines several   !ααα   variables indicating:
::::* name and version of the REXX interpreter being used.
::::* name of the program used to clear the terminal screen.
::::* name of the (host) operating system being used.
::::* name of the subsystem to issue commands (via '''address''').
::::* name of the pool used to set environmental variables.
::::* name by which the REXX program was invoked.
::::* name by which the REXX program was loaded (executed).
::::* how the REXX program is being invoked: 
::::::::* as a command 
::::::::* as a function 
::::::::* as a subroutine


## Ring


```ring

# Project : Musical scale

loadlib("C:\Ring\extensions\ringbeep\ringbeep.dll")
freqs = [[262,"Do"], [294,"Ra"], [330,"Me"], [349,"Fa"], [392,"So"], [440,"La"], [494,"Te"], [523,"do"]]
for f = 1 to len(freqs)
     see freqs[f][2] + nl
     beep(freqs[f][1],300)
next

```

Output video:

[https://www.dropbox.com/s/jf34s6apalw0k7c/CalmoSoftMusicalScale.avi?dl=0 Musical scale]


## Scala


### Windows

```Scala
import net.java.dev.sna.SNA

object PlayMusicScale extends App with SNA {

  snaLibrary = "Kernel32"
  val Beep = SNA[Int, Int, Unit]

  println("Please don't shoot the piano player, he's doing the best that he can!")
  List(0, 2, 4, 5, 7, 9, 11, 12).
    foreach(f => Beep((261.63 * math.pow(2, f / 12.0)).toInt, if (f == 12) 1000 else 500))
  println("That's all")
}
```



## Sparkling

The following Sparkling program generates a WAVE audio file named "notes.wav" 
that can be played in order to achieve the required effect:

```Sparkling
var sampleRate = 44100.0;
var duration = 8.0;
var dataLength = round(sampleRate * duration);
var dataLength_b0 = dataLength >> 0  & 0xff;
var dataLength_b1 = dataLength >> 8  & 0xff;
var dataLength_b2 = dataLength >> 16 & 0xff;
var dataLength_b3 = dataLength >> 24 & 0xff;

const adjustedHdrSize = 36;

var len = dataLength - adjustedHdrSize;
var len_b0 = len >> 0  & 0xff;
var len_b1 = len >> 8  & 0xff;
var len_b2 = len >> 16 & 0xff;
var len_b3 = len >> 24 & 0xff;

// WAV header
var wavhdr   = "RIFF";
    wavhdr ..= fmtstr("%c%c%c%c", len_b0, len_b1, len_b2, len_b3);
    wavhdr ..= "WAVE";
    wavhdr ..= "fmt ";
    wavhdr ..= "\x10\x00\x00\x00";
    wavhdr ..= "\x01\x00";
    wavhdr ..= "\x01\x00";
    wavhdr ..= "\x44\xac\x00\x00";
    wavhdr ..= "\x44\xac\x00\x00";
    wavhdr ..= "\x01\x00";
    wavhdr ..= "\x08\x00";
    wavhdr ..= "data";
    wavhdr ..= fmtstr("%c%c%c%c", dataLength_b0, dataLength_b1, dataLength_b2, dataLength_b3);

// write wav header
var f = fopen("notes.wav", "w");
fwrite(f, wavhdr);


// compute and write actual data
var frequs = { 261.6, 293.6, 329.6, 349.2, 392.0, 440.0, 493.9, 523.3 };

for var j = 0; j < duration; j++ {
	var frequ = frequs[j];
	var omega = 2 * M_PI * frequ;
	for var i = 0; i < dataLength / 8; i++ {
		var y = 32 * sin(omega * i / sampleRate);
		var byte = fmtstr("%c", round(y));
		fwrite(f, byte);
	}
}

fclose(f);
```



## Tcl

```tcl
package require sound

# Encapsulate the tone generation
set filter [snack::filter generator 1 20000 0.5 sine -1]
set sound  [snack::sound -rate 22050]
proc play {frequency length} {
    global filter sound

    $filter configure $frequency
    $sound play -filter $filter

    # Need to run event loop; Snack uses it internally
    after $length {set donePlay 1}
    vwait donePlay

    $sound stop
}

# Major scale up, then down; extra delay at ends of scale
set tonicFrequency 261.63; # C4
foreach i {0 2 4 5 7 9 11 12 11 9 7 5 4 2 0} {
    play [expr {$tonicFrequency*2**($i/12.0)}] [expr {$i%12?250:500}]
}
```



## Ursa

```ursa>decl double<
 notes
append 261.63 293.66 329.63 349.23 392.00 440.00 493.88 523.25 notes

for (decl int i) (< i (size notes)) (inc i)
        ursa.util.sound.beep notes<i> 0.5
end for
```



## VBA


```vb

Option Explicit

Declare Function Beep Lib "kernel32" (ByVal Freq As Long, ByVal Dur As Long) As Long

Sub Musical_Scale()
Dim Fqs, i As Integer
   Fqs = Array(264, 297, 330, 352, 396, 440, 495, 528)
   For i = LBound(Fqs) To UBound(Fqs)
      Beep Fqs(i), 500
   Next
End Sub
```



## XPL0


```XPL0
\Square waves on the beeper speaker:
code Sound=39;
real Period;  int I;
[Period:= 1190000.0/261.625565;     \middle C
for I:= 2 to 9 do
    [Sound(1, 4, fix(Period));      \times 2^(-1/6) else 2^(-1/12)
    Period:= Period * (if I&3 then 0.890898719 else 0.943874313);
    ];
]

\MIDI grand piano (requires 32-bit Windows or Sound Blaster 16):
code Sound=39;
int  Note, I;
[port($331):= $3F;      \set MPU-401 into UART mode
Note:= 60;              \start at middle C
for I:= 2 to 9+1 do     \(last note is not played)
    [port($330):= $90;  port($330):= Note;  port($330):= $7F;
    Sound(0, 4, 1);     \This "Sound" is off, but convenient 0.22 sec delay
    Note:= Note + (if I&3 then 2 else 1);
    ];
]
```



## ZX Spectrum Basic


```zxbasic
10 REM Musical scale
20 LET n=0: REM Start at middle C
30 LET d=0.2: REM Make each note 0.2 seconds in duration
40 FOR l=1 TO 8
50 BEEP d,n
60 READ i: REM Number of semitones to increment
70 LET n=n+i           
80 NEXT l
90 STOP
9000 DATA 2,2,1,2,2,2,1,2:REM WWHWWWH
```

