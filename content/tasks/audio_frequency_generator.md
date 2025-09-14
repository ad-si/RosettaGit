+++
title = "Audio frequency generator"
description = ""
date = 2019-09-22T20:03:23Z
aliases = []
[extra]
id = 12966
[taxonomies]
categories = ["Electronics", "Sciences", "Sound", "Temporal media", "task"]
tags = []
languages = [
  "axe",
  "go",
  "perl",
  "phix",
  "pure_data",
  "ring",
  "tcl",
  "zx_spectrum_basic",
]
+++

## Task
{{omit from|GUISS}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|Scala}}
{{omit from|TPP}}

An audio [[wp:Signal_generator|frequency generator]] produces a continual audible monotone at a set frequency and level of volume. 
There are controls to adjust the frequency and the volume up and down as desired. 
Some also have a selector to switch the waveform type between sine wave, square wave and triangular sawtooth.

The task is to emulate an audio frequency generator. 
It is permissible to use an inbuilt computer speaker 
if the system does not have the facility to utilize dedicated sound hardware.

The solution should include:

* A demonstration of how to check for availability of sound hardware on the system (on systems where this is possible)
* A demonstration of how to produce a continual audible monotone (on sound hardware this would typicvally be a sine wave)
* A method of adjusting the frequency and volume of the monotone. (one way would be to use left and right arrow keys to increase or decrease frequency, and up and down keys to increase or decrease the volume)
* A method to silence or reset the sound hardware on exit.

Optionally the solution can also include:

* A demonstration of how to fall back to internal speaker, if sound hardware is not available
* A facility to switch between sine wave, square wave and triangular sawtooth

Languages that provide no facilities for utilizing sound hardware of any kind should be omitted.


## Axe

{{untested|Axe}}

```axe
ClrHome
Disp "FREQ:",i
10→F
Repeat getKey(15)
 If getKey(3)
  F++
  F=0?-1→F
 ElseIf getKey(2)
  F--
  F=-1?0→F
 End
 Output(5,0,F▶Dec)
 Freq(F,10000)
End
```



## Go

As Go does not have any audio support in its standard library, this invokes the SoX utility's 'play' command with the appropriate parameters to emulate an audio frequency generator. It appears that SoX automatically uses the internal speaker if there is no sound hardware available.

The duration of the monotone is set in advance (to a small number of seconds) and the application ends when it finishes playing. Consequently, a method to silence it is not required.

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
    freq := 0
    for freq < 40 || freq > 10000 {
        fmt.Print("Enter frequency in Hz (40 to 10000) : ")
        scanner.Scan()
        input := scanner.Text()
        check(scanner.Err())
        freq, _ = strconv.Atoi(input)
    }
    freqS := strconv.Itoa(freq)

    vol := 0
    for vol < 1 || vol > 50 {
        fmt.Print("Enter volume in dB (1 to 50) : ")
        scanner.Scan()
        input := scanner.Text()
        check(scanner.Err())
        vol, _ = strconv.Atoi(input)
    }
    volS := strconv.Itoa(vol)

    dur := 0.0
    for dur < 2 || dur > 10 {
        fmt.Print("Enter duration in seconds (2 to 10) : ")
        scanner.Scan()
        input := scanner.Text()
        check(scanner.Err())
        dur, _ = strconv.ParseFloat(input, 64)
    }
    durS := strconv.FormatFloat(dur, 'f', -1, 64)

    kind := 0
    for kind < 1 || kind > 3 {
        fmt.Print("Enter kind (1 = Sine, 2 = Square, 3 = Sawtooth) : ")
        scanner.Scan()
        input := scanner.Text()
        check(scanner.Err())
        kind, _ = strconv.Atoi(input)
    }
    kindS := "sine"
    if kind == 2 {
        kindS = "square"
    } else if kind == 3 {
        kindS = "sawtooth"
    }

    args := []string{"-n", "synth", durS, kindS, freqS, "vol", volS, "dB"}
    cmd := exec.Command("play", args...)
    err := cmd.Run()
    check(err)
}
```



## Perl


```perl
use strict 'vars';
use feature 'say';
use feature 'state';
use Audio::NoiseGen qw(play sine square triangle);
use Term::ReadKey qw(ReadMode ReadLine);

Audio::NoiseGen::init() || die 'No access to sound hardware?';

print "Play [S]ine, s[Q]uare or [T]riangle wave? "; my $ans_freq   = uc(<>);
print "Pick a volume [0-9]";                        my $ans_volume =    <>;
say 'Volume: '. (my $volume = 0.1 + 1 * $ans_volume/10);

ReadMode(3);

my $waveform = $ans_freq eq 'Q' ? 'square' : $ans_freq eq 'T' ? 'triangle' : 'sine';
play ( gen => amp ( amount => $volume, gen => &$waveform( freq => setfreq(440) ) ) );

sub setfreq {
    state $freq;
    say $freq = shift;
    return sub {
        ReadMode(3);
        state $cnt;
        unless ($cnt++ % 1000) {
            my $key = ReadLine(-1);
            my $previous = $freq;
            if    ($key eq "\e[A") { $freq += 10  }
            elsif ($key eq "\e[B") { $freq -= 10  }
            elsif ($key eq "\e[C") { $freq += 100 }
            elsif ($key eq "\e[D") { $freq -= 100 }
            say $freq if $freq != $previous;
        }
        return $freq;
    }
}
```



## Phix


```Phix
-- demo/rosetta/Audio_frequency_generator.exw
include pGUI.e
Ihandle dlg, frequency, duration

atom k32=0, xBeep

function button_cb(Ihandle /*playbtn*/)
    integer f = IupGetInt(frequency,"VALUE"),
            d = IupGetInt(duration,"VALUE")
    if platform()=WINDOWS then
        if k32=0 then
            k32 = open_dll("kernel32.dll")
            xBeep = define_c_proc(k32, "Beep", {C_INT,C_INT})
        end if
        c_proc(xBeep,{f,d})
    else
        system(sprintf("play -n synth %f sine %d", {d/1000, f}))
    end if
    end if
    return IUP_DEFAULT
end function

function valuechanged_cb(Ihandle val)
    -- maintain the labels as the sliders are moved
    Ihandle parent = IupGetParent(val),
            lbl = IupGetNextChild(parent, NULL) 
    integer v = IupGetInt(val,"VALUE")
    IupSetInt(lbl,"TITLE",v)
    return IUP_DEFAULT
end function

procedure main()
    Ihandle flabel, dlabel, frame1, frame2, playbtn
    
    IupOpen()

    flabel = IupLabel("2000","ALIGNMENT=ARIGHT,NAME=val_label,SIZE=20x8")
    frequency = IupValuator("HORIZONTAL","VALUECHANGED_CB", Icallback("valuechanged_cb"),
                            "EXPAND=HORIZONTAL, CANFOCUS=NO, MIN=50, MAX=10000, VALUE=2000")
    frame1 = IupFrame(IupHbox({flabel,frequency}),"TITLE=\"Frequency (Hz): \"")

    dlabel = IupLabel("500","ALIGNMENT=ARIGHT,NAME=val_label,SIZE=20x8")
    duration = IupValuator("HORIZONTAL","VALUECHANGED_CB", Icallback("valuechanged_cb"),
                           "EXPAND=HORIZONTAL, CANFOCUS=NO, MIN=100, MAX=3000, VALUE=500")
    frame2 = IupFrame(IupHbox({dlabel,duration}),"TITLE=\"Duration (ms): \"")

    playbtn = IupHbox({IupFill(),
                       IupButton("Play",Icallback("button_cb"),"PADDING=30x0"),
                       IupFill()},"MARGIN=0x20")

    dlg = IupDialog(IupVbox({frame1,
                             frame2,
                             playbtn}, "MARGIN=10x5, GAP=5"))
    IupSetAttribute(dlg,"TITLE","Audio Frequency Generator")
    IupSetAttribute(dlg,"RASTERSIZE","500x230")
    IupCloseOnEscape(dlg)

    IupShow(dlg)
    IupMainLoop()
    IupClose()
end procedure
main()
```



## Pure Data

'''audio-frequency-generator.pd'''

```txt

#N canvas 245 70 635 516 10;
#X obj 501 214 cnv 15 84 66 empty empty empty 80 12 0 10 -262130 -13381 0;
#X obj 319 98 cnv 15 133 15 empty empty frequency 74 8 0 10 -204786 -13381 0;
#X obj 319 55 line;
#X floatatom 319 98 8 0 0 1 - - -;
#X obj 109 436 dac~;
#X obj 322 261 hsl 128 15 0 1 0 0 empty empty volume 90 8 0 10 -203904 -1 -4160 0 0;
#X obj 85 142 osc~;
#X floatatom 554 34 5 0 0 0 MIDI_note_number - -;
#X obj 554 52 mtof;
#X obj 32 142 phasor~;
#X obj 119 371 *~;
#X obj 227 345 line~;
#X floatatom 319 285 5 0 0 0 - - -;
#X floatatom 154 444 5 0 0 1 dB - -;
#X obj 154 423 env~ 256;
#X obj 154 462 - 100;
#X obj 320 357 vu 15 120 empty empty -1 -8 0 10 -66577 -1 1 0;
#X msg 227 323 \$1 20;
#X obj 32 292 *~;
#X obj 84 293 *~;
#X obj 120 293 *~;
#X obj 319 142 tgl 15 0 empty empty sawtooth 20 7 0 10 -261234 -1 -258113 0 1;
#X obj 319 165 tgl 15 0 empty empty sine 20 7 0 10 -261234 -1 -258113 0 1;
#X obj 319 188 tgl 15 0 empty empty square 20 7 0 10 -261234 -1 -258113 0 1;
#N canvas 0 0 450 300 (subpatch) 0;
#X array graph 100 float 0;
#X coords 0 1 99 -1 200 140 1 0 0;
#X restore 386 339 graph;
#X obj 63 239 bng 15 250 50 0 empty empty empty 17 7 0 10 -262144 -228856 -1;
#X obj 173 142 triang~;
#X obj 172 293 *~;
#X obj 319 211 tgl 15 0 empty empty triangle 20 7 0 10 -261234 -1 -258113 0 1;
#X obj 120 142 square~;
#X obj 226 142 pulse~;
#X obj 227 293 *~;
#X obj 319 234 tgl 15 0 empty empty pulse 20 7 0 10 -261234 -1 -258113 0 1;
#X obj 32 164 -~ 0.5;
#X obj 45 462 tabwrite~ graph;
#X obj 529 193 loadbang;
#X msg 529 215 \; pd dsp 1;
#X msg 319 34 18 \, 24000 30000;
#X msg 529 250 \; pd dsp 0;
#X text 509 223 on;
#X text 503 258 off;
#X msg 32 34 50;
#X msg 63 34 100;
#X msg 94 34 200;
#X msg 126 34 500;
#X msg 157 34 1000;
#X msg 195 34 2000;
#X msg 232 34 4000;
#X msg 268 34 8000;
#X connect 2 0 3 0;
#X connect 3 0 6 0;
#X connect 3 0 9 0;
#X connect 3 0 25 0;
#X connect 3 0 26 0;
#X connect 3 0 29 0;
#X connect 3 0 30 0;
#X connect 5 0 12 0;
#X connect 5 0 25 0;
#X connect 6 0 19 0;
#X connect 7 0 8 0;
#X connect 8 0 3 0;
#X connect 9 0 33 0;
#X connect 10 0 4 0;
#X connect 10 0 4 1;
#X connect 10 0 14 0;
#X connect 10 0 34 0;
#X connect 11 0 10 1;
#X connect 12 0 17 0;
#X connect 13 0 15 0;
#X connect 14 0 13 0;
#X connect 15 0 16 0;
#X connect 17 0 11 0;
#X connect 18 0 10 0;
#X connect 19 0 10 0;
#X connect 20 0 10 0;
#X connect 21 0 18 1;
#X connect 21 0 25 0;
#X connect 22 0 19 1;
#X connect 22 0 25 0;
#X connect 23 0 20 1;
#X connect 23 0 25 0;
#X connect 25 0 34 0;
#X connect 26 0 27 0;
#X connect 27 0 10 0;
#X connect 28 0 27 1;
#X connect 28 0 25 0;
#X connect 29 0 20 0;
#X connect 30 0 31 0;
#X connect 31 0 10 0;
#X connect 32 0 25 0;
#X connect 32 0 31 1;
#X connect 33 0 18 0;
#X connect 35 0 36 0;
#X connect 37 0 2 0;
#X connect 41 0 3 0;
#X connect 42 0 3 0;
#X connect 43 0 3 0;
#X connect 44 0 3 0;
#X connect 45 0 3 0;
#X connect 46 0 3 0;
#X connect 47 0 3 0;
#X connect 48 0 3 0;

```

'''square~.pd'''

```txt

#N canvas 787 211 450 300 10;
#X obj 46 17 inlet;
#X obj 112 86 * -1;
#X obj 46 109 phasor~;
#X obj 46 186 -~ 1;
#X obj 112 109 phasor~;
#X obj 151 17 loadbang;
#X msg 108 59 0;
#X obj 47 223 outlet~;
#X msg 151 59 0.5;
#X obj 46 165 +~;
#X obj 108 17 inlet;
#X connect 0 0 1 0;
#X connect 0 0 2 0;
#X connect 1 0 4 0;
#X connect 2 0 9 0;
#X connect 3 0 7 0;
#X connect 4 0 9 1;
#X connect 5 0 6 0;
#X connect 5 0 8 0;
#X connect 6 0 2 1;
#X connect 8 0 4 1;
#X connect 9 0 3 0;
#X connect 10 0 6 0;
#X connect 10 0 8 0;

```

'''triang~.pd'''

```txt

#N canvas 770 214 450 300 10;
#X obj 46 17 inlet;
#X obj 112 51 * -1;
#X obj 46 74 phasor~;
#X obj 46 95 *~ 2;
#X obj 46 116 -~ 1;
#X obj 46 137 clip~ 0 1;
#X obj 112 74 phasor~;
#X obj 112 95 *~ 2;
#X obj 112 116 -~ 1;
#X obj 112 137 clip~ 0 1;
#X obj 194 18 loadbang;
#X msg 194 39 0;
#X obj 47 187 +~;
#X obj 47 208 -~ 0.5;
#X obj 47 229 *~ 2;
#X obj 47 250 outlet~;
#X obj 156 18 inlet;
#X connect 0 0 1 0;
#X connect 0 0 2 0;
#X connect 1 0 6 0;
#X connect 2 0 3 0;
#X connect 3 0 4 0;
#X connect 4 0 5 0;
#X connect 5 0 12 0;
#X connect 6 0 7 0;
#X connect 7 0 8 0;
#X connect 8 0 9 0;
#X connect 9 0 12 1;
#X connect 10 0 11 0;
#X connect 11 0 2 1;
#X connect 11 0 6 1;
#X connect 12 0 13 0;
#X connect 13 0 14 0;
#X connect 14 0 15 0;
#X connect 16 0 11 0;

```

'''pulse~.pd'''

```txt

#N canvas 784 384 450 300 10;
#X obj 51 56 phasor~;
#X obj 51 77 -~ 0.99;
#X obj 51 98 clip~ 0 1;
#X obj 51 119 *~ 100;
#X obj 51 140 outlet~;
#X obj 51 34 inlet;
#X obj 90 34 inlet;
#X connect 0 0 1 0;
#X connect 1 0 2 0;
#X connect 2 0 3 0;
#X connect 3 0 4 0;
#X connect 5 0 0 0;
#X connect 6 0 0 1;

```

* Provides on/off switch, frequency and volume control and five different wave shapes to select from.
* Frequency may be typed in or changed by dragging. Predefined pitches and a frequency ramp (18 - 24000 Hz) are accessible by clicking the respective labels.
* Volume level and wave shape are graphically displayed.
* More shapes, even free wave shape modelling could easily be added.


## Ring


```ring

# Project : Audio frequency generator

Load "guilib.ring"
loadlib("C:\Ring\extensions\ringbeep\ringbeep.dll")

freq = 1000
ImageFile  = "stock.jpg"

    UserIcons = CurrentDir() +"\"

    WinLeft   = 80                  
    WinTop    = 80                  
    WinWidth  = 1200                
    WinHeight = 750                
    WinRight  = WinLeft + WinWidth  
    WinBottom = WinTop  + WinHeight 

    BoxLeft   = 80                  
    BoxTop    = 40                  
    BoxWidth  = WinWidth  -160      
    BoxHeight = WinHeight -100      
    imageW = 400 ;  imageH = 400 ; GrowBy = 8
    volume = 100

MyApp = New qapp
{

    win1 = new qMainWindow()
    {
            setwindowtitle("Video and Music Player")
            setgeometry( WinLeft, WinTop, WinWidth, WinHeight)

             if Fexists(ImageFile)

                imageStock = new qlabel(win1)
                {
                    image = new qpixmap(ImageFile)
                    AspectRatio = image.width() / image.height()

                    imageW = 1000
                    imageH = 600 

                    setpixmap(image.scaled(imageW , imageH ,0,0))   
                    PosLeft = (BoxWidth  - imageW ) / 2 + 80
                    PosTop  = (BoxHeight - imageH ) / 2 +40
                    setGeometry(PosLeft,PosTop,imageW,imageH)

                }

            else
                msg = "ImageFile: -- "+ ImageFile +" -- required. Use an Image JPG of your choice"
                SendMsg(msg)
            ok

            videowidget = new qVideoWidget(win1)   
            {
                setgeometry(BoxLeft, BoxTop, BoxWidth, BoxHeight)
                setstylesheet("background-color: green")
            }

            player = new qMediaPlayer()
            {   
               setVideoOutput(videowidget)
            }

            TimerDuration = new qTimer(win1)
            {
                setinterval(1000)
                settimeoutevent("pTimeDuration()")  ### ==>> func
                start()
            }

            oFont = new qFont("",10,0,0)
            setFont( oFont)

            btnBack = new qpushbutton(win1)    {
                    setGeometry(280,20,80,20)
                    settext("Low")
                    seticon(new qicon(new qpixmap(UserIcons +"Backward.png")))
                    setclickevent( "pBackward()")
            }

            btnDur = new qpushbutton(win1)    {
                    setGeometry(360,20,140,20)
            }

            btnFwd = new qpushbutton(win1)    {
                    setGeometry(500,20,80,20)
                    settext("High")
                    seticon(new qicon(new qpixmap(UserIcons +"Forward.png")))
                    setclickevent( "pForward()")
            }

            btnVolume = new qpushbutton(win1)    {
                setGeometry(760,20,100,20)
                settext("Volume: 100")
                seticon(new qicon(new qpixmap(UserIcons +"Volume.png")))
            }

            VolumeDec = new qpushbutton(win1)
            {
                setgeometry(700,20,60,20)
                settext("Low")
                seticon(new qicon(new qpixmap(UserIcons +"VolumeLow.png")))
                setclickevent( "PVolumeDec()")
            }

            VolumeInc = new qpushbutton(win1)
            {
                setgeometry(860,20,60,20)
                settext("High")
                seticon(new qicon(new qpixmap(UserIcons +"VolumeHigh.png")))
                setclickevent( "pVolumeInc()")
            }

        show()  

    }

     exec()
}

Func pTimeDuration()
    Duration()   
return

Func Duration()

    DurPos = "Frequency: " + string(freq) + " Hz"
    btnDur.setText(DurPos)

return

Func pForward
    freq = freq + 100
    for n = 1 to 3        
         beep(freq,300)
    next
return

Func pBackward
    freq = freq - 100
    for n = 1 to 3        
         beep(freq,300)
    next
return

Func pVolumeDec()
    if volume > 0
       volume = volume - 10
       btnVolume.settext("Volume: " + volume)
       player.setVolume(volume)
    ok
return

Func pVolumeInc()
    if volume < 100
       volume = volume + 10
       btnVolume.settext("Volume: " + volume)
       player.setVolume(volume)
    ok
return

```

Output:

[https://www.dropbox.com/s/jf5uq0bbts40wto/CalmoSoftFrequency.avi?dl=0 Audio frequency generator]


## SuperCollider

SuperCollider is a sound programming language, so the task is predictably easy.


```SuperCollider

// the server application detects audio hardware.
Server.default.boot;

// play a sine monotone at 440 Hz and amplitude 0.2
{ SinOsc.ar(440) * 0.2 }.play;

// use the cursor position to adjust frequency and amplitude (ranges are exponential)
{ SinOsc.ar(MouseX.kr(40, 20000, 1)) * MouseY.kr(0.001, 0.5, 1) }.play;

// use the cursor position to switch between sine wave, square wave and triangular sawtooth
// the rounding and lag smoothes the transition between them 
{ SelectX.ar(MouseX.kr(0, 2).round.lag, [ SinOsc.ar, Pulse.ar, LFTri.ar ]) * 0.1 }.play;

// the same expressed as an array of functions of a common phase
(
{
	var phase = LFSaw.ar;
	var functions = [
		{ |x| sin(x * pi) },
		{ |x| x > 0 },
		{ |x| abs(x) },
	];
	var which = MouseX.kr(0, 2);
	functions.sum { |f, i|
		abs(which - i) < 0.5 * f.(phase)
	} * 0.1
}.play
)

// sound stops on exit
Server.default.quit;

```



## Tcl

{{libheader|Snack}}
This does not work on Windows due the use of the external <tt>stty</tt> program.

```tcl
package require sound

set baseFrequency 261.63
set baseAmplitude [expr {64000 / 100.0}]
set halfSemis 0
set volSteps 10

# How to adjust the generator
proc adjustPitchVolume {changePitch changeVolume} {
    global filter baseFrequency baseAmplitude halfSemis volSteps
    incr halfSemis $changePitch
    incr volSteps $changeVolume
    # Clamp the volume range
    set volSteps [expr {$volSteps < 0 ? 0 : $volSteps > 10 ? 10 : $volSteps}]
    puts -nonewline " Pitch: [expr {$halfSemis / 2.0}]  Volume: $volSteps  \r"
    set freq [expr {$baseFrequency * 2**($halfSemis/24.0)}]
    set ampl [expr {$baseAmplitude * $volSteps**2}]

    # This is where we set the actual frequency of the generated sound
    $filter configure $freq $ampl
}

# Callback handler for pressed keys
proc keyPress {} {
    global done
    switch [string tolower [read stdin 1]] {
	"q" { set done 1 }
	"u" { adjustPitchVolume 1 0 }
	"d" { adjustPitchVolume -1 0 }
	"s" { adjustPitchVolume 0 -1 }
	"l" { adjustPitchVolume 0 1 }
	default {
	    if {[eof stdin]} { set done 1 }
	}
    }
}

# Instantiate the sound generation objects from the Snack library
set filter [snack::filter generator 1 32000 0.5 sine -1]
set sound [snack::sound -rate 32050]

# Make things ready for a console application 
exec stty raw -echo <@stdin >@stdout
fconfigure stdout -buffering none
fileevent stdin readable keyPress
puts "'U' to raise pitch, 'D' to lower pitch, 'L' for louder, 'S' for softer"
puts "'Q' to quit"

# Start the playing
$sound play -filter $filter
adjustPitchVolume 0 0

# Wait until the user is finished
vwait done

# Clean up the console from its non-standard state
fileevent stdin readable {}
puts ""
exec stty -raw echo <@stdin >@stdout

# Stop the sound playing
$sound stop
exit
```



## ZX Spectrum Basic


The ZX Spectrum is not very good at making sound. 
Most applications in BASIC would just produce annoying beeps, 
and the following is no exception. 
To emulate the signal generator, we just produce repetative beeps 
using the inbuilt speaker. 
The left and right keys (5 and 8) change the tone. 
There is no volume control on the Spectrum.


```zxbasic
10 REM The crappest signal generator in the world
20 REM We do not check for boundary errors in this simple demo
30 LET n=1
40 LET k$=INKEY$
50 IF k$="5" THEN LET n=n-0.5
60 IF k$="8" THEN LET n=n+0.5
70 PRINT AT 0,0;n,"    "
80 BEEP 0.1,n: REM beep for 0.1 second at n semitones relative to middle C
90 GO TO 40
```
