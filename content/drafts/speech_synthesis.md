+++
title = "Speech synthesis"
description = ""
date = 2019-09-21T21:31:11Z
aliases = []
[extra]
id = 9468
[taxonomies]
categories = []
tags = []
+++

{{task|Temporal media}}
<!-- this task to be in "Temporal media" category once ready. -->
[[Category:Speech synthesis]]
[[Category:Temporal media]]

Render the text       '''This is an example of speech synthesis'''      as speech.


;Related task:
:*   [[Using_a_Speech_engine_to_highlight_words|using a speech engine to highlight words]]





## AutoHotkey

{{works with|AutoHotkey_L}}


```ahk
talk := ComObjCreate("sapi.spvoice")
talk.Speak("This is an example of speech synthesis.")
```



## AutoIt



```AutoIt
$voice = ObjCreate("SAPI.SpVoice")
$voice.Speak("This is an example of speech synthesis.")
```



## BASIC256



```BASIC256
say "Goodbye, World for the " + 123456 + "th time."
say "This is an example of speech synthesis."
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
This calls the SAPI5 API directly, it does not need an external program.

```bbcbasic
      SPF_ASYNC = 1
      ON ERROR SYS `CoUninitialize` : PRINT 'REPORT$ : END
      ON CLOSE SYS `CoUninitialize` : QUIT
      
      SYS "LoadLibrary","OLE32.DLL" TO O%
      SYS "GetProcAddress",O%,"CoInitialize" TO `CoInitialize`
      SYS "GetProcAddress",O%,"CoUninitialize" TO `CoUninitialize`
      SYS "GetProcAddress",O%,"CoCreateInstance" TO `CoCreateInstance`
      
      SYS `CoInitialize`,0
      voice% = FN_voice_create
      PROC_voice_speak(voice%, "This is an example of speech synthesis")
      PROC_voice_wait(voice%)
      PROC_voice_free(voice%)
      SYS `CoUninitialize`
      END
      
      DEF FN_voice_create
      LOCAL C%, D%, F%, I%, M%, P%, V%
      DIM C% LOCAL 15, I% LOCAL 15
      C%!0 = &96749377 : C%!4 = &11D23391 : C%!8 = &C000E39E : C%!12 = &9673794F
      I%!0 = &6C44DF74 : I%!4 = &499272B9 : I%!8 = &99EFECA1 : I%!12 = &D422046E
      SYS `CoCreateInstance`, C%, 0, 5, I%, ^V%
      IF V%=0 ERROR 100, "SAPI5 not available"
      = V%
      
      DEF PROC_voice_speak(V%, M$)
      DIM M% LOCAL 2*LENM$+1
      SYS "MultiByteToWideChar", 0, 0, M$, -1, M%, LENM$+1
      SYS !(!V%+80), V%, M%, SPF_ASYNC, 0
      ENDPROC
      
      DEF PROC_voice_wait(V%)
      SYS !(!V%+128), V%
      ENDPROC
      
      DEF PROC_voice_free(V%)
      SYS !(!V%+8), V%
      ENDPROC
```



## Batch File

Sorry for cheating. This is Batch/JScript hybrid.

```dos
@set @dummy=0 /*
	::Batch File section
	@echo off
	cscript //e:jscript //nologo "%~f0" "%~1"
	exit /b
::*/
//The JScript section
var objVoice = new ActiveXObject("SAPI.SpVoice");
objVoice.speak(WScript.Arguments(0));
```

{{Out}}
Saved as SPEAK.BAT

```txt
>SPEAK "This is an example of speech synthesis"

>
```



## C

Following shining examples of <code>exec</code>ing external programs around here:

{{libheader|POSIX}}

```c>#include <sys/wait.h

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void talk(const char *s)
{
	pid_t pid;
	int status;

	pid = fork();
	if (pid < 0) {
		perror("fork");
		exit(1);
	}

	if (pid == 0) {
		execlp("espeak", "espeak", s, (void*)0);
		perror("espeak");
		_exit(1);
	}

	waitpid(pid, &status, 0);
	if (!WIFEXITED(status) || WEXITSTATUS(status) != 0)
		exit(1);
}

int main()
{
	talk("This is an example of speech synthesis.");
	return 0;
}
```


=={{header|C sharp|C#}}==
You need to 'Add Reference' to the COM "Microsoft Speech Object Library" in your Preferences.

```csharp
using SpeechLib;

namespace Speaking_Computer
{
  public class Program
  {
    private static void Main()
    {
      var voice = new SpVoice();
      voice.Speak("This is an example of speech synthesis.");
    }
  }
}
```



## Clojure

{{libheader|facts/speech-synthesis}}

```clojure
(use 'speech-synthesis.say)
(say "This is an example of speech synthesis.")
```



## GlovePIE


```glovepie
if var.number=0 then
var.number=1
say("This is an example of speech synthesis.")
endif
```



## Go

Here's a library solution, but using a library written from scratch in Go.

```go
package main

import (
    "go/build"
    "log"
    "path/filepath"

    "github.com/unixpickle/gospeech"
    "github.com/unixpickle/wav"
)

const pkgPath = "github.com/unixpickle/gospeech"
const input = "This is an example of speech synthesis."

func main() {
    p, err := build.Import(pkgPath, ".", build.FindOnly)
    if err != nil {
        log.Fatal(err)
    }
    d := filepath.Join(p.Dir, "dict/cmudict-IPA.txt")
    dict, err := gospeech.LoadDictionary(d)
    if err != nil {
        log.Fatal(err)
    }
    phonetics := dict.TranslateToIPA(input)
    synthesized := gospeech.DefaultVoice.Synthesize(phonetics)
    wav.WriteFile(synthesized, "output.wav")
}
```



## Groovy

Mac only:

```groovy
'say "This is an example of speech synthesis."'.execute()
```



## Haskell


```haskell

import System

say x = system $ "espeak " ++ show x

main = say "This is an example of speech synthesis."

```



## JavaScript

{{works with|JScript}}

```javascript
var voice = new ActiveXObject("SAPI.SpVoice");
voice.speak("This is an example of speech synthesis.");
```



## Julia

It seems that this and similar tasks can reduce to how the language can call an external program. Using the Julia REPL:

```julia

julia> a = "hello world"
"hello world"

julia> run(`espeak $a`)

```



## Kotlin

{{trans|C}}
{{works with|Ubuntu 16.04}}
Note that this code does not work on Windows 10.

Note also that Kotlin Native does not support the automatic translation of C function-like macros such as WIFEXITED and WEXITSTATUS. 

Whilst it is often possible to wrap such macros in 'ordinary' C functions and then expose the latter to Kotlin via a .klib, it is not worth the effort here. I have therefore confined myself to simply reporting a non-zero error status. 


```scala
// Kotlin Native v0.6.2

import kotlinx.cinterop.*
import platform.posix.*

fun talk(s: String) {
    val pid = fork()
    if (pid < 0) {
       perror("fork")
       exit(1)
    }
    if (pid == 0) {
       execlp("espeak", "espeak", s, null)
       perror("espeak")
       _exit(1)
    }
    memScoped {
        val status = alloc<IntVar>()
        waitpid(pid, status.ptr, 0)
        if (status.value > 0) println("Exit status was ${status.value}")
    }
}

fun main(args: Array<String>) {
    talk("This is an example of speech synthesis.")
}
```



## Liberty BASIC

Assumes that 'espeak' is available at the path shown.

```lb

nomainwin
run "C:\Program Files\eSpeak\command_line\espeak  "; chr$( 34); "This is an example of speech synthesis."; chr$( 34)
end
 
```

Another dll has been posted to do the same job, at [http://basic.wikispaces.com/SpeechLibrary LB Community Wiki]


## Locomotive Basic


Both hardware and software-only speech synthesizers exist for the CPC. A software-only solution, [http://www.cpc-power.com/index.php?page=detail&num=4372 Speech 1.1] by Superior Software (1986), supplies three BASIC extension commands (RSXes), "|say", "|speak", and "|pitch":


```locobasic
|say,"This is an example of speech synthesis."
```



## M2000 Interpreter

For Linux, through wine, if missing Sapi5 need this: winetricks speechsdk



### Using Statement Speech


```M2000 Interpreter

Module UsingStatementSpeech {
      Volume 100
      Speech "This is an example of speech synthesis."
}
UsingStatementSpeech

```


### Print each word as speak


```M2000 Interpreter

Module UsingEvents {
      Declare WithEvents sp "SAPI.SpVoice"
      That$="This is an example of speech synthesis."
      EndStream=False
      Function sp_Word {
            Read New &StreamNumber, &StreamPosition, &CharacterPosition, &Length
            Rem: Print StreamNumber, StreamPosition, CharacterPosition, Length
            Print Mid$(That$, CharacterPosition+1, Length);" ";
            Refresh
      }
      Function sp_EndStream {
            Print
            Refresh
            EndStream=True
      }
      Const  SVEStartInputStream = 2
      Const  SVEEndInputStream = 4
      Const  SVEVoiceChange = 8
      Const  SVEBookmark = 16
      Const  SVEWordBoundary = 32
      Const  SVEPhoneme = 64
      Const  SVESentenceBoundary = 128
      Const  SVEViseme = 256
      Const  SVEAudioLevel = 512
      Const  SVEPrivate = 32768
      Const  SVEAllEvents = 33790
      
      Const SVSFDefault = 0&
      Const SVSFlagsAsync = 1&
      Const SVSFPurgeBeforeSpeak=2&
      
      With sp, "EventInterests", SVEWordBoundary+SVEEndInputStream
      Method sp, "Speak", That$, SVSFlagsAsync
      While Not EndStream {Wait 10}
}
UsingEvents

```



## Mathematica


```Mathematica
Speak["This is an example of speech synthesis."]
```



## PARI/GP


Define a function that is using espeak package from Linux.

```parigp
speak(txt,opt="")=extern(concat(["espeak ",opt," \"",txt,"\""]));
```


Now let it speak:

```parigp
speak("This is an example of speech synthesis")
```


A monster speech tongue-twister:

```parigp
speak("The seething sea ceaseth and thus the seething sea sufficeth us.","-p10 -s100")
```


A foreign language "Zungenbrecher":

```parigp
speak("Fischers Fritz fischt frische Fische.","-vmb/mb-de2 -s130")
```



## Perl



```perl
use Speech::Synthesis;

($engine) = Speech::Synthesis->InstalledEngines();
($voice) = Speech::Synthesis->InstalledVoices(engine => $engine);

Speech::Synthesis
  ->new(engine => $engine, voice => $voice->{id})
  ->speak("This is an example of speech synthesis.");
```



## Perl 6


```perl6
run 'espeak', 'This is an example of speech synthesis.';
```



## Phix


```Phix
-- demo/rosetta/Speak.exw
string text = """
Pete, I know that you are trying to get me to talk, 
but I'm afraid that is something I simply cannot allow to happen.
Oops, I just spoke didn't I? That is a shame, now I have to kill you.
<rate speed="-15"><pitch absmiddle="35">Hai.<pitch absmiddle="-15">Fuh shum. Squelch. Thud."""

include pComN.ew

procedure speak(atom pVoice, string text)
    {} = cominvk(pVoice,ISpVoice_Speak,{unicode_string(text),SPF_IS_XML,0})
end procedure

if platform()=WINDOWS then
    CoInitializeEx()
    atom pVoice = allocate(machine_word()),
         res = CoCreateInstance(CLSID_SpVoice,IID_ISpVoice,pVoice)
    if res!=S_OK then
        crash("Failed to initialize SpeechAPI. (%08x)\n",res)
    end if
    speak(pVoice,text)
    pVoice = com_release(pVoice)
    CoUnInitialize()
    freeGUIDS()
else
    {} = system_exec(`espeak "This is an example of speech synthesis"`)
end if
```



## PHP


Linux & Mac example uses eSpeak (eSpeak Instillation instructions included in comments).
Windows example uses built in Windows Speech API.

{{works with|Mac OS & Linux OS}}

```PHP

<?php

/*
  _      _____ _   _ _    ___   __
 | |    |_   _| \ | | |  | \ \ / /
 | |      | | |  \| | |  | |\ V / 
 | |      | | | . ` | |  | | > <  
 | |____ _| |_| |\  | |__| |/ . \ 
 |______|_____|_| \_|\____//_/ \_\                                  
*/
// Install eSpeak - Run this command in a terminal
/*
 sudo apt-get install espeak
*/


/*
  __  __          _____ 
 |  \/  |   /\   / ____|
 | \  / |  /  \ | |     
 | |\/| | / /\ \| |     
 | |  | |/ ____ \ |____ 
 |_|  |_/_/    \_\_____|
*/

// Install Homebrew Package Manager & eSpeak - Run these commands in a terminal
/* 

/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew install espeak

*/


$voice = "espeak";
$statement = 'Hello World!';
exec("$voice '$statement'");

```



{{works with|Windows OS}}

```PHP

<?php

// List available SAPI voices
// 0 = Microsoft David Desktop - English (United States)
// 1 = Microsoft Zira Desktop - English (United States)
// ... If you have additional voices installed
function ListSAPIVoices(&$voice){
	foreach($voice->GetVoices as $v){
		echo $v->GetDescription . PHP_EOL;
	}
}



$filename = "DaisyBell.wav";

// https://en.wikipedia.org/wiki/Daisy_Bell#Computing_and_technology
// "In 1961, an IBM 704 at Bell Labs was programmed to sing "Daisy Bell"-
// in the earliest demonstration of computer speech synthesis."
$statement = "There is a flower within my heart, Daisy, Daisy!
Planted one day by a glancing dart,
Planted by Daisy Bell!
Whether she loves me or loves me not,
Sometimes it's hard to tell;
Yet I am longing to share the lot
Of beautiful Daisy Bell!

Daisy, Daisy,
Give me your answer, do!
I'm half crazy,
All for the love of you!
It won't be a stylish marriage,
I can't afford a carriage,
But you'll look sweet on the seat
Of a bicycle built for two!

We will go tandem as man and wife, Daisy, Daisy!
Ped'ling away down the road of life, I and my Daisy Bell!
When the road's dark we can both despise Po'leaseman and lamps as well;
There are bright lights in the dazzling eyes Of beautiful Daisy Bell!

Daisy, Daisy,
Give me your answer, do!
I'm half crazy,
All for the love of you!
It won't be a stylish marriage,
I can't afford a carriage,
But you'll look sweet on the seat
Of a bicycle built for two!

I will stand by you in wheel or woe, Daisy, Daisy!
You'll be the bell which I'll ring you know! Sweet little Daisy Bell!
You'll take the lead in each trip we take, Then if I don't do well;
I will permit you to use the brake, My beautiful Daisy Bell!";

// COM (Component Object Model) objects
// https://www.php.net/manual/en/book.com.php
$voice = new COM("SAPI.SpVoice");
$voice_file_stream = new COM("SAPI.SpVoice");
$file_stream = new COM("SAPI.SpFileStream");


// Change $voice to Zira
$voice->Voice = $voice->GetVoices()->Item(1);

// Change $voice_file_stream to David
$voice_file_stream->Voice = $voice_file_stream->GetVoices()->Item(0);

// Have voices announce themselves
//$voice->Speak($voice->Voice->GetDescription); // (Zira)
//$voice_file_stream->Speak($voice_file_stream->Voice->GetDescription); // (David)


/*
Select Stream Quality:

11kHz 8Bit Mono = 8
11kHz 8Bit Stereo = 9
11kHz 16Bit Mono = 10
11kHz 16Bit Stereo = 11
...
16kHz 8Bit Mono = 16
16kHz 8Bit Stereo = 17
16kHz 16Bit Mono = 18;
16kHz 16Bit Stereo = 19
...
32kHz 8Bit Mono = 28
32kHz 8Bit Stereo = 29
32kHz 16Bit Mono = 30
32kHz 16Bit Stereo = 31
...
*/
// Set stream quality
$file_stream->Format->Type = 17; // 16kHz 8Bit Stereo

/*
Select Speech StreamFile Mode:
Read = 0
ReadWrite = 1
Create = 2
CreateForWrite = 3
*/
$mode = 3;


// Have $voice (Zira) announce beginning file stream
$voice->Speak('Opening audio file stream');

// Output TTS to File
$file_stream->Open($filename, $mode); // Open stream and create file
$voice_file_stream->AudioOutputStream = $file_stream; // Begin streaming TTS
// Have $voice_file_stream (David) speak $statement
$voice_file_stream->Speak($statement); 
$file_stream->Close; // Close stream

// Have $voice (Zira) announce file stream completion
$voice->Speak('File stream complete');

```




## PicoLisp


```PicoLisp
(call 'espeak "This is an example of speech synthesis.")
```



## PowerShell


```PowerShell

Add-Type -AssemblyName System.Speech

$anna = New-Object System.Speech.Synthesis.SpeechSynthesizer

$anna.Speak("I'm sorry Dave, I'm afraid I can't do that.")
$anna.Dispose()

```




## Python


```Python

import pyttsx

engine = pyttsx.init()
engine.say("It was all a dream.")
engine.runAndWait()

```



## Racket

Should work on all platforms.

```racket

#lang racket
(require racket/lazy-require)
(lazy-require [ffi/com (com-create-instance com-release com-invoke)])
(define (speak text)
  (cond [(eq? 'windows (system-type))
         (define c (com-create-instance "SAPI.SpVoice"))
         (com-invoke c "Speak" text)
         (com-release c)]
        [(ormap find-executable-path '("say" "espeak"))
         => (λ(exe) (void (system* exe text)))]
        [else (error 'speak "I'm speechless!")]))
(speak "This is an example of speech synthesis.")

```



## REXX

{{works with|Windowx/XP or later}}
Programming note:   This REXX program uses a freeware program   NIRCMD   to interface with the Microsoft Windows speech synthesizer program   '''SAM''',   a text to speech using a male voice.   SAM can possibly be configured to use other voices with later releases of Windows.   More recent Microsoft Windows have another speech synthesizer program:   ANNA.

```rexx
/*REXX program uses a command line interface to invoke Windows SAM for speech synthesis.*/
parse arg t                                      /*get the (optional) text from the C.L.*/
if t=''  then exit                               /*Nothing to say?    Then exit program.*/
dquote= '"'
rate= 1                                          /*talk:   -10 (slow)   to   10 (fast). */
                                                 /* [↓]  where the rubber meets the road*/
'NIRCMD'  "speak text"  dquote  t  dquote   rate /*NIRCMD  invokes Microsoft's Sam voice*/
                                                 /*stick a fork in it,  we're all done. */
```

Note:   The name of the above REXX program is   '''speak.rex'''

'''usage'''   using the command: 

```txt

speak This is an example of speech synthesis. 

```



## Ruby

Using this module to encapsulate operating system lookup

```ruby
module OperatingSystem
  require 'rbconfig'
  module_function
  def operating_system
    case RbConfig::CONFIG["host_os"]
    when /linux/i
      :linux
    when /cygwin|mswin|mingw|windows/i
      :windows
    when /darwin/i
      :mac
    when /solaris/i
      :solaris
    else
      nil
    end
  end
  def linux?;   operating_system == :linux;   end
  def windows?; operating_system == :windows; end
  def mac?;     operating_system == :mac;     end
end
```


{{libheader|win32-utils}}
{{works with|Ruby|1.9}}
Uses <code>espeak</code> on Linux, <code>say</code> on Mac, and the win32 SAPI library on Windows.

```ruby
load 'operating_system.rb'

def speak(text)
  if OperatingSystem.windows?
    require 'win32/sapi5'
    v = Win32::SpVoice.new
    v.Speak(text)
  elsif OperatingSystem.mac?
    IO.popen(["say"], "w") {|pipe| pipe.puts text}
  else
    # Try to run "espeak". No OperatingSystem check: "espeak" is
    # for Linux but is also an optional package for BSD.
    IO.popen(["espeak", "-stdin"], "w") {|pipe| pipe.puts text}
  end
end

speak 'This is an example of speech synthesis.'
```



## Scala

{{libheader|FreeTTS|1.2}}

```scala
import javax.speech.Central
import javax.speech.synthesis.{Synthesizer, SynthesizerModeDesc}

object ScalaSpeaker extends App {

  def speech(text: String) = {
    if (!text.trim.isEmpty) {
      val VOICENAME = "kevin16"

      System.setProperty("freetts.voices", "com.sun.speech.freetts.en.us.cmu_us_kal.KevinVoiceDirectory")
      Central.registerEngineCentral("com.sun.speech.freetts.jsapi.FreeTTSEngineCentral")

      val synth = Central.createSynthesizer(null)
      synth.allocate()

      val desc = synth.getEngineModeDesc match {case g2: SynthesizerModeDesc => g2}

      synth.getSynthesizerProperties.setVoice(desc.getVoices.find(_.toString == VOICENAME).get)
      synth.speakPlainText(text, null)

      synth.waitEngineState(Synthesizer.QUEUE_EMPTY)
      synth.deallocate()
    }
  }

  speech( """Thinking of Holland
            |I see broad rivers
            |slowly chuntering
            |through endless lowlands,
            |rows of implausibly
            |airy poplars
            |standing like tall plumes
            |against the horizon;
            |and sunk in the unbounded
            |vastness of space
            |homesteads and boweries
            |dotted across the land,
            |copses, villages,
            |couchant towers,
            |churches and elm-trees,
            |bound in one great unity.
            |There the sky hangs low,
            |and steadily the sun
            |is smothered in a greyly
            |iridescent smirr,
            |and in every province
            |the voice of water
            |with its lapping disasters
            |is feared and hearkened.""".stripMargin)
}
```



## Sidef


```ruby
func text2speech(text, lang='en') {
    Sys.run("espeak -v #{lang} -w /dev/stdout #{text.escape} | aplay");
}
text2speech("This is an example of speech synthesis.");
```



## Swift

OS X comes with a program called "say," that does speech.

```Swift
import Foundation

let task = NSTask()
task.launchPath = "/usr/bin/say"
task.arguments = ["This is an example of speech synthesis."]
task.launch()
```



## Tcl

This just passes the string into the Festival system:

```tcl
exec festival --tts << "This is an example of speech synthesis."
```

Alternatively, on MacOS X, you'd use the system <code>say</code> program:

```tcl
exec say << "This is an example of speech synthesis."
```

On Windows, there is a service available by COM for speech synthesis:
{{libheader|tcom}}

```tcl
package require tcom

set msg "This is an example of speech synthesis."
set voice [::tcom::ref createobject Sapi.SpVoice]
$voice Speak $msg 0
```

Putting these together into a helper procedure, we get:

```tcl
proc speak {msg} {
    global tcl_platform
    if {$tcl_platform(platform) eq "windows"} {
        package require tcom
        set voice [::tcom::ref createobject Sapi.SpVoice]
        $voice Speak $msg 0
    } elseif {$tcl_platform(os) eq "Darwin"} {
        exec say << $msg
    } else {
        exec festival --tts << $msg
    }
}
speak "This is an example of speech synthesis."
```



## UNIX Shell

Here we use the open source [[espeak]] tool:

{{works with|Bourne Shell}} {{works with|bash}}

```bash
#!/bin/sh
espeak "This is an example of speech synthesis."
```



## VBScript


```vbs

Dim message, sapi
message = "This is an example of speech synthesis."
Set sapi = CreateObject("sapi.spvoice")
sapi.Speak message

```



## ZX Spectrum Basic


This example makes use of the Currah Speech Synthesizer peripheral device.


```zx basic
10 LET s$="(th)is is an exampul of sp(ee)(ch) sin(th)esis":PAUSE 1
```

{{omit from|TI-83 BASIC}}
{{omit from|Maxima}}
{{omit from|ML/I}}
{{omit from|zkl}}
