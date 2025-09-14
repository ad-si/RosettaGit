+++
title = "Play recorded sounds"
description = ""
date = 2019-05-12T15:41:03Z
aliases = []
[extra]
id = 4366
[taxonomies]
categories = ["task", "Temporal media"]
tags = []
languages = [
  "autohotkey",
  "batch_file",
  "bbc_basic",
  "csharp",
  "delphi",
  "go",
  "guiss",
  "liberty_basic",
  "mathematica",
  "phix",
  "picolisp",
  "purebasic",
  "python",
  "r",
  "racket",
  "ring",
  "ruby",
  "swift",
  "tcl",
  "tuscript",
  "unix_shell",
  "vba",
]
+++

## Task

Load at least two prerecorded sounds, and demonstrate as many of these features as you can:
* playing them individually and simultaneously
* stopping before the end of the sound
* looping (preferably glitch-free)
* setting the volume of each sound
* stereo or 3D positional mixing
* performing other actions when marked times in the sound arrive
Describe:
* The supported audio formats, briefly.
* Whether it is suitable for game sound effects (low-latency start, resource efficiency, supports many simultaneous sounds, etc.)
* Whether it is suitable for playing music (long duration <!-- Other attributes? -->).

[Note: If it seems to be a good idea, this task may be revised to specify a particular timeline rather than just 'demonstrate these features'.]

Where applicable, please categorize examples primarily by the audio facility used (library/API/program/platform) rather than the language if the language is incidental (e.g. "Mac OS X CoreAudio" or "mplayer" rather than "C" or "bash").

## AutoHotkey

No builtin functions for:

1. monitoring timepoints in the sound

2. simultaneous play

```AutoHotkey
SoundPlay, %A_WinDir%\Media\tada.wav, wait
SoundPlay, %A_WinDir%\Media\Windows XP Startup.wav, wait

; simulaneous play may require a second script

SoundPlay, %A_WinDir%\Media\tada.wav
SoundPlay, Nonexistent  ; stop before finishing

SoundSet +10  ; increase volume by 10%
Loop, 2
    SoundPlay, %A_WinDir%\Media\tada.wav, wait
```



## Batch File

It only works on Windows XP and it only reads Wave (.wav) audio files:

  @echo off
  :main
  cls
  echo Drag a .wav file there and press enter
  set /p file1=
  sndrec32 /embedding /play /close %file1%
  cls
  echo Drag a second .wav file and both will play together
  set /p file2=
  sndrec32 /embedding /play /close %file1% | sndrec32 /embedding /play /close %file2%
  echo Thanks
  pause>nul
  exit


## BBC BASIC

BBC BASIC for Windows has native support for playing MIDI files, and WAV files can be played using simple API calls:

```bbcbasic
      SND_LOOP = 8
      SND_ASYNC = 1
      SND_FILENAME = &20000

      PRINT "Playing a MIDI file..."
      *PLAY C:\windows\media\canyon.mid

      WAIT 300
      PRINT "Playing the Windows TADA sound quietly..."
      wave$ = "\windows\media\tada.wav"
      volume% = 10000
      SYS "waveOutSetVolume", -1, volume% + (volume% << 16)
      SYS "PlaySound", wave$, 0, SND_FILENAME + SND_ASYNC

      WAIT 300
      PRINT "Playing the Windows TADA sound loudly on the left channel..."
      volume% = 65535
      SYS "waveOutSetVolume", -1, volume%
      SYS "PlaySound", wave$, 0, SND_FILENAME + SND_ASYNC

      WAIT 300
      PRINT "Playing the Windows TADA sound loudly on the right channel..."
      volume% = 65535
      SYS "waveOutSetVolume", -1, volume% << 16
      SYS "PlaySound", wave$, 0, SND_FILENAME + SND_ASYNC

      WAIT 300
      PRINT "Looping the Windows TADA sound on both channels..."
      volume% = 65535
      SYS "waveOutSetVolume", -1, volume% + (volume% << 16)
      SYS "PlaySound", wave$, 0, SND_FILENAME + SND_ASYNC + SND_LOOP

      WAIT 300
      SYS "PlaySound", 0, 0, 0
      PRINT "Stopped looping..."

      WAIT 300
      SOUND OFF
      PRINT "Stopped MIDI."

      END
```


## C#


```c#
using System;
using System.Threading;
using System.Media;

class Program
{
    static void Main(string[] args)
    {
        //load sound file
        SoundPlayer s1 = new SoundPlayer(); //
        s1.SoundLocation = file; // or "s1 = new SoundPlayer(file)"

        //play
        s1.Play();

        //play for 0.1 seconds
        s1.Play();
        Thread.Sleep(100);
        s1.Stop();

        //loops
        s1.PlayLooping();
    }
}
```



## Delphi


```Delphi
program PlayRecordedSounds;

{$APPTYPE CONSOLE}

uses MMSystem;

begin
  sndPlaySound('SoundFile.wav', SND_NODEFAULT OR SND_ASYNC);
end.
```



## Go

Go doesn't have any audio support in its standard library and it's best therefore to invoke a utility such as SoX to complete a task such as this.

Although there is at least one third party library which provides Go bindings to the libsox sound library, for casual use it's far easier to invoke SoX directly with a list of arguments.

See the PicoLisp entry for a description of what these particular arguments do.

```go
package main

import (
    "log"
    "os"
    "os/exec"
)

func main() {
    args := []string{
        "-m", "-v", "0.75", "a.wav", "-v", "0.25", "b.wav",
        "-d",
        "trim", "4", "6",
        "repeat", "5",
    }
    cmd := exec.Command("sox", args...)
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    if err := cmd.Run(); err != nil {
        log.Fatal(err)
    }
}
```



## GUISS


Here we use the media player to play a sound file in the default directory:


```guiss
Start,Programs,Accessories,Media Player,Menu:File,Open,
Doubleclick:Icon:Sound.WAV,Button:OK,Button:Play
```



## Liberty BASIC


```lb
'Supports .mid and .wav natively
'Midi may be played simultaneously
'with .wav but only one .wav voice
'is supported. Multiple voices can
'be achieved via API or Dlls such
'as FMOD or Wavemix

'to play a .mid and return its length
playmidi "my.mid", midiLength

'check where we are in .mid
if midipos()<midiLength then
    print "Midi still playing"
else
    print "Midi ended"
end if

'to stop a playing .mid
stopmidi

'to play a .wav and wait for it to end
playwave "my.wav"

'to play a .wav and continue procesing
playwave "my.wav", async

'to loop a .wav and continue processing
playwave "my.wav",loop

'to silence any .wav
playwave ""
or
playwave "next.wav"

'to adjust .wav vol
'set left and right full on
left =65535
right=65535
dwVol=right*65536+left
calldll #winmm, "waveOutSetVolume", 0 as long, _
    dwVol as long, ret as long

```



## Mathematica

This code demonstrates :
loading two prerecorded sounds,playing them individually then simultaneously,
stopping before the end of the sound,looping (preferably glitch-free)and setting the volume of each sound


```Mathematica
a = Import["sound1.flac","FLAC"]; b = Import["sound2.flac","FLAC"];

ListPlay[a, {t, 0, 10}]; ListPlay[b, {t, 0, 10}];

ListPlay[{a,b}, {t, 0, 10}];

Stopping before the end can be done using the GUI or by reducing the parameter range of the ListPlay function.

While[True,ListPlay[{a,b}, {t, 0, 10}];]

ListPlay[{0.5*a, 0.3*b}, {t, 0, 10}];
```


-Supported audio formats :
AIFF Macintosh sound format (.aif, .aiff),
AU Mu law encoding Unix Audio Format (.au),
FLAC lossless audio codec (.flac),
SND file format, equivalent to AU (.snd),
"WAV" WAV format (.wav),
"Wave64" Sony Wave64 format (.w64),
MIDI format (.mid)

-Suitability for game sound effects :
low-latency start : yes
resource efficiency : low
support for many simultaneous sounds : yes

-Suitable for playing sound of arbitrary long duration.



## Phix


```Phix
integer xPlaySound = 0

procedure play_sound()--string filename)
    if platform()=WINDOWS then
        string filename = join_path({getenv("SYSTEMROOT"),"Media","chord.wav"})
        if xPlaySound=0 then
            atom winmm = open_dll("winmm.dll")
            xPlaySound = define_c_proc(winmm,"sndPlaySoundA",{C_PTR,C_INT})
        end if
        c_proc(xPlaySound,{filename,1})
    elsif platform()=LINUX then
        system("paplay chimes.wav")
        system("paplay chord.wav")
    end if
end procedure
play_sound()
```



## PicoLisp

The obvious way is to call 'sox', the "Swiss Army knife of audio manipulation"
(man sox).

The following plays two files "a.wav" and "b.wav" simultaneously (to play them
individually, omit the "-m" flag). The first one is played with a volume of 75
percent, the second with 25 percent, starting at the 4th second, with a duration
of 6 seconds, looping 5 times.

```PicoLisp
(call 'sox
   "-m"  "-v" "0.75" "a.wav"  "-v" "0.25" "b.wav"
   "-d"
   "trim" 4 6
   "repeat" 5 )
```



## PureBasic


```PureBasic
InitSound()
; We need this to use Sound functions
UseOGGSoundDecoder()
; Now we can not only load wav sound files, but also ogg encoded ones.
;   With movie library more formats can be played (depends on system) but you cannot
;   handle them with Sound functions
If Not LoadSound(1,"Path/to/Sound/1.ogg") Or Not LoadSound(2,"Path/to/Sound/2.wav")
MessageRequester("Error","One of our sounds could not be loaded"+Chr(10)+"Use Debugger to check which one")
EndIf

;- simultaneous playing
PlaySound(1)
PlaySound(2)

;- manipulating sounds
Delay(1000)
; pause for one second, to let user hear something
SoundVolume(1,90)
SoundVolume(2,60)
; reduce volume of the sounds a bit
SoundPan(1,-80)
SoundPan(2,100)
; Sound 1 mostly left speaker, sound 2 only right speaker
SoundFrequency(1,30000)
; play sound one faster
Delay(1000)
; pause for one second, to let user hear effects of previous actions

;- stopping while playing
StopSound(-1)
; value -1 stops all playing sounds
PlaySound(1,#PB_Sound_Loop)
; continous looping without glitch

;suitable for 2D games and music playing.
; TODO: There is a Sound3D library for 3D Games, needs to be decribed here too
```



## Python

Pygame is a library for cross-platform game development and depends on the SDL multimedia library (SDL_mixer) for its audio playback. SDL_mixer supports any number of simultaneously playing channels of 16 bit stereo audio, plus a single channel of music, mixed by the popular MikMod MOD, Timidity MIDI, Ogg Vorbis, and SMPEG MP3 libraries.


```python
import time
from pygame import mixer

mixer.init(frequency=16000) #set frequency for wav file
s1 = mixer.Sound('test.wav')
s2 = mixer.Sound('test2.wav')

#individual
s1.play(-1)         #loops indefinitely
time.sleep(0.5)

#simultaneously
s2.play()          #play once
time.sleep(2)
s2.play(2)         #optional parameter loops three times
time.sleep(10)

#set volume down
s1.set_volume(0.1)
time.sleep(5)

#set volume up
s1.set_volume(1)
time.sleep(5)

s1.stop()
s2.stop()
mixer.quit()
```


To play back .mp3 (or .ogg) files, the music import is used.


```python
import time
from pygame import mixer
from pygame.mixer import music

mixer.init()
music.load('test.mp3')

music.play()
time.sleep(10)

music.stop()
mixer.quit()
```



## R

R's sound package uses system commands to call a built-in media player.  On Windows, by default, this is Media Player. You can see the system call with <code>WavPlayer()</code>.  Only .WAV files are supported, and the external code call means there is some latency before sounds are played.  Samples longer than 10 minutes may correspond to significant chunks of your machine's memory.


```r

#Setup
# Warning: The example files are Windows specific.
library(sound)
media_dir <- file.path(Sys.getenv("SYSTEMROOT"), "Media")
chimes <- loadSample(file.path(media_dir, "chimes.wav"))
chord <- loadSample(file.path(media_dir, "chord.wav"))

# Play sequentially
play(appendSample(chimes, chord))

# Play simultaneously
play(chimes + chord)

# Stopping before the end
play(cutSample(chimes, 0, 0.2))

#Looping
for(i in 1:3) play(chimes)                 #leaves a gap between instances

three_chimes <- lapply(vector(3, mode = "list"), function(x) chimes)
play(do.call(appendSample, three_chimes))  #no gap, see also cutSampleEnds

# Volume control
play(3 * chimes)

#Stereo effect
play(stereo(chimes, chord))

#Other actions (not obviously possible)

```



## Racket

(Works on all platforms.)

```racket

#lang racket/gui
(play-sound "some-sound.wav" #f)

```



## Ring


```ring

Load "guilib.ring"
new qapp {
         q1=NULL  q2=NULL
         win1 = new qwidget() {
                setwindowtitle("play sound!") show()
                setgeometry(100,100,400,400)
         }
         new qpushbutton(win1) {
             setgeometry(50,50,100,30)
             settext("play1")
             setclickevent("playmusic1()")
             show()
         }
         new qpushbutton(win1) {
             setgeometry(200,50,100,30)
             settext("play2")
             setclickevent("playmusic2()")
             show()
         }
         new qpushbutton(win1) {
             setgeometry(50,100,100,30)
             settext("pause1")
             setclickevent("pauseplay1()")
         show()
         }
         new qpushbutton(win1) {
             setgeometry(200,100,100,30)
             settext("pause2")
             setclickevent("pauseplay2()")
             show()
         }
         new qpushbutton(win1) {
             setgeometry(50,150,100,30)
             settext("stop1")
             setclickevent("stopplay1()")
             show()
                }
         new qpushbutton(win1) {
             setgeometry(200,150,100,30)
             settext("stop2")
             setclickevent("stopplay2()")
             show()
         }
         lineedit1 = new qlineedit(win1) {
                     setGeometry(50,200,100,30)
                     settext("50")
                     show()
         }
         lineedit2 = new qlineedit(win1) {
                     setGeometry(200,200,100,30)
                     settext("50")
                     show()
         }
         new qpushbutton(win1) {
             setgeometry(50,250,100,30)
             settext("volume1")
             setclickevent("volume1()")
             show()
         }
         new qpushbutton(win1) {
             setgeometry(200,250,100,30)
             settext("volume2")
             setclickevent("volume2()")
             show()
         }
         new qpushbutton(win1) {
             setgeometry(50,300,100,30)
             settext("mute1")
             setclickevent("mute1()")
             show()
         }
         new qpushbutton(win1) {
             setgeometry(200,300,100,30)
             settext("mute2")
             setclickevent("mute2()")
             show()
         }
         exec()
         }

func playmusic1
     q1 = new qmediaplayer(win1)  {
          setmedia(new qurl("music1.wav"))
          setvolume(50) play()
     }

func playmusic2
     q2 = new qmediaplayer(win1)  {
          setmedia(new qurl("music2.wav"))
          setvolume(50) play()
     }

func pauseplay1
     q1.pause()

func pauseplay2
     q2.pause()

func stopplay1
     q1.stop()

func stopplay2
     q2.stop()

func volume1
     lineedit1 { vol1 = text() }
     q1 {setvolume(number(vol1))}

func volume2
     lineedit2 { vol2 = text() }
     q2 {setvolume(number(vol2))}

func mute1
     q1.setmuted(true)

func mute2
     q2.setmuted(true)

```

Output:

[https://lh3.googleusercontent.com/-1BjBV-ugV6g/V1f47eTkj8I/AAAAAAAAAJc/kbCzqatjC3w-WWyS6teNwWXjEA1_kj0LQCLcB/s1600/CalmoSoftSounds.jpg CalmoSoftSounds]


## Ruby

There aren't many mature sound libraries for Ruby.  The {{libheader|RubyGems}} package [http://rubyforge.org/projects/win32utils/ win32-sound] can play WAV files on the Windows platform only.


```ruby
require 'win32/sound'
include Win32

sound1 = ENV['WINDIR'] + '\Media\Windows XP Startup.wav'
sound2 = ENV['WINDIR'] + '\Media\Windows XP Shutdown.wav'

puts "play the sounds sequentially"
[sound1, sound2].each do |s|
  t1 = Time.now
  Sound.play(s)
  puts "'#{s}' duration: #{(Time.now.to_f - t1.to_f)} seconds"
end

puts "attempt to play the sounds simultaneously"
[sound1, sound2].each {|s| Sound.play(s, Sound::ASYNC)}

puts <<END
the above only plays the second sound2 because the library only appears
to be able to play one sound at a time.
END

puts "loop a sound for a few seconds"
puts Time.now
Sound.play(sound1, Sound::ASYNC + Sound::LOOP)
sleep 10
Sound.stop
puts Time.now

puts "manipulate the volume"
vol_left, vol_right = Sound.wave_volume
Sound.play(sound1, Sound::ASYNC)
sleep 1
puts "right channel quiet"
Sound.set_wave_volume(vol_left, 0)
sleep 1
puts "left channel quiet"
Sound.set_wave_volume(0, vol_right)
sleep 1
puts "restore volume"
Sound.set_wave_volume(vol_left, vol_right)

sleep 1
puts "the asynchronous sound is cancelled when the program exits"
```


## Swift

Uses AVFoundation's AVAudioPlayer.

```Swift
import AVFoundation

// This example uses AVAudioPlayer for playback.
// AVAudioPlayer is the player Apple recommends for playback, since it suitable for songs
// and offers control over numerous playback parameters.
// It can play any type that is natively supported by OSX or iOS

class PlayerControl: NSObject, AVAudioPlayerDelegate {
    let player1:AVAudioPlayer!
    let player2:AVAudioPlayer!
    var playedBoth = false
    var volume:Float {
        get {
            return player1.volume
        }

        set {
            player1.volume = newValue
            player2.volume = newValue
        }
    }

    init(player1:AVAudioPlayer, player2:AVAudioPlayer) {
        super.init()
        self.player1 = player1
        self.player2 = player2
        self.player1.delegate = self
        self.player2.delegate = self
    }

    func loop() {
        player1.numberOfLoops = 1
        player1.play()

        let time = Int64((Double(player1.duration) + 2.0) * Double(NSEC_PER_SEC))

        dispatch_after(dispatch_time(0, time), dispatch_get_main_queue()) {[weak self] in
            println("Stopping track")
            self?.player1.stop()
            exit(0)
        }
    }

    func playBoth() {
        player1.play()
        player2.play()
    }

    func audioPlayerDidFinishPlaying(player:AVAudioPlayer!, successfully flag:Bool) {
        if player === player2 && !playedBoth {
            playBoth()
            playedBoth = true
        } else if player === player2 && playedBoth {
            loop()
        }
    }
}

let url1 = NSURL(string: "file:///file1.mp3")
let url2 = NSURL(string: "file:///file2.mp3")

var err:NSError?
let player1 = AVAudioPlayer(contentsOfURL: url1, error: &err)
let player2 = AVAudioPlayer(contentsOfURL: url2, error: &err)

let player = PlayerControl(player1: player1, player2: player2)

// Setting the volume
player.volume = 0.5

// Play track 2
// When this track finishes it will play both of them
// by calling the audioPlayerDidFinishPlaying delegate method
// Once both tracks finish playing, it will then loop the first track twice
// stopping the track after 2 seconds of the second play
player.player2.play()

CFRunLoopRun()
```



## Tcl

```tcl
package require sound
# Potentially also require driver support for particular formats

# Load some sounds in; this part can be slow
snack::sound s1
s1 read $soundFile1
snack::sound s2
s2 read $soundFile2

# Play a sound for a while (0.1 seconds; this is a low-latency operation)
s1 play
after 100 set done 1; vwait done; # Run the event loop for a while
s1 stop

# Play two sounds at once (for 30 seconds) while mixing together
s1 play
s2 play
after 30000 set done 1; vwait done
s1 stop
s2 stop
```

Note that this library is capable of handling both short and long sounds (sound effects and music).

{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have a speaker. -->


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
audiofile="test.wav"
ERROR/STOP OPEN (audiofile,READ,-std-)
BROWSE $audiofile

```



## UNIX Shell


```bash
#!/usr/bin/sh

# play.sh

# Plays .au files.
# Usage: play.sh <recorded_sound.au>

cat $1 >> /dev/audio # Write file $1 to the speaker's Character Special (/dev/audio).
```



## VBA

Visual Basic for Applications can play sounds in the .WAV format by calling the multimedia library winmm.dll.
See [http://support.microsoft.com/kb/158140/en-us http://support.microsoft.com/kb/158140/en-us]. The "Flags" parameter can be used e.g. to play a sound continuously (that is, until the function is called again to stop playing).

Volume can be set using the function waveOutSetVolume, see [http://support.microsoft.com/kb/118377/en-us http://support.microsoft.com/kb/118377/en-us].


```vb

Declare Function libPlaySound Lib "winmm.dll" Alias "sndPlaySoundA" _
    (ByVal filename As String, ByVal Flags As Long) As Long

Sub PlaySound(sWav As String)
  Call libPlaySound(sWav, 1) '1 to play asynchronously
End Sub

```

Type
```txt
Playsound "d:\explode.wav"
```
 in the Immediate window and that sound will play. Nothing will happen if the file d:\explode.wav does not exist.

