+++
title = "Using a Speech engine to highlight words"
description = ""
date = 2018-12-26T01:42:00Z
aliases = []
[extra]
id = 9415
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "autohotkey",
  "go",
  "m2000_interpreter",
  "mathematica",
  "related_tasks",
  "rexx",
  "ruby",
  "tcl",
]
+++

{{draft task}}[[Category:Speech synthesis]][[Category:Temporal media]]
Display a piece of text and produce spoken output via a speech engine. 

As each word is being spoken, highlight the word on the display. 

In languages where cursor control and highlighting are not possible, it is permissible to output each word as it is spoken.


## Related tasks

:*   [[Speech_synthesis|speech synthesis]]





## AutoHotkey

We use the simple SAPI.SPVoice COM Object and a parsing loop. 
The highlighting is done with [http://msdn.microsoft.com/en-us/library/bb761661 EM_SETSEL] and Notepad. Rather crude, but it works. Due to the simplistic nature of the parsing loop, the text ends with a space.

```AutoHotkey
SetTitleMatchMode 2
EM_SETSEL := 0x00B1

Run notepad,,,pid
WinWaitActive ahk_pid %pid%
ControlSetText, Edit1, % text := "AutoHotkey was the first to implement this task! ", ahk_pid %pid%

pVoice := ComObjCreate("Sapi.spvoice"), i := 1 ; the spvoice COM Object ships with the OS

; parse the text
While lf := SubStr(text, i, 1)
{
   If lf = %A_Space%
   {
      SendMessage, EM_SetSel, % i-StrLen(word)-1, % i-1, Edit1, ahk_pid %pid%
      pVoice.speak(word), word := "", i++
   }
   Else word .= lf, i++
}
```



## Go

This uses the eSpeak speech synthesizer which is invoked for each word in the text. As the word is spoken it is printed to the terminal in capitalized form (and the previous word is uncapitalized). After a second's delay the final word is uncapitalized.

Very robotic but it works.

```go
package main

import (
    "fmt"
    "log"
    "os/exec"
    "strings"
    "time"
)

func main() {
    s := "Actions speak louder than words."
    prev := ""
    prevLen := 0
    bs := ""
    for _, word := range strings.Fields(s) {
        cmd := exec.Command("espeak", word)
        if err := cmd.Run(); err != nil {
            log.Fatal(err)
        }
        if prevLen > 0 {
            bs = strings.Repeat("\b", prevLen)
        }
        fmt.Printf("%s%s%s ", bs, prev, strings.ToUpper(word))
        prev = word + " "
        prevLen = len(word) + 1
    }
    bs = strings.Repeat("\b", prevLen)
    time.Sleep(time.Second)
    fmt.Printf("%s%s\n", bs, prev)
}
```



## M2000 Interpreter


```M2000 Interpreter

Module UsingEvents {
      Form 60, 32
      Cls 5, 0
      Pen 14
      Declare WithEvents sp "SAPI.SpVoice"
      That$="Rosetta Code is a programming chrestomathy site"
      margin=(width-Len(That$))/2
      EndStream=False
      \\ this function called as sub routine - same scope as Module
      \\ we can call it from event function too
      Function Localtxt {
            \\ move the cursor to middle line
            Cursor 0, height/2
            \\ using OVER the line erased with background color and then print text over
            \\ ordinary Print using transparent printing of text
            \\ $(0) set mode to non proportional text, @() move the cursor to sepecific position
            Print Over $(0),@(margin), That$
      }
      Call Local LocalTxt()
      Function sp_Word {
            Read New &StreamNumber, &StreamPosition, &CharacterPosition, &Length
            Call Local LocalTxt()
            Cursor 0, height/2
            Pen 15 {Print Part $(0), @(CharacterPosition+margin); Mid$(That$, CharacterPosition+1, Length)}
            Refresh
      }
      Function sp_EndStream {
            Refresh
            EndStream=True
      }
      Const  SVEEndInputStream = 4
      Const  SVEWordBoundary = 32
      Const SVSFlagsAsync = 1&
 
      With sp, "EventInterests", SVEWordBoundary+SVEEndInputStream
      Method sp, "Speak", That$, SVSFlagsAsync
      While Not EndStream {Wait 10}
      Call Local LocalTxt()
}
UsingEvents


```



## Mathematica

<lang>DynamicModule[{text = "This is some text.", words, i = 0}, 
 Panel@Column@{Dynamic[
     Row[Riffle[
       If[i != 0, MapAt[Style[#, Red] &, #, i], #] &@(words = 
          StringSplit@text), " "]]], InputField[Dynamic@text, String],
     Button["Speak", 
     While[i < Length@words, i++; FinishDynamic[]; Speak[words[[i]]]; 
      Pause[Max[0.7, 0.12 StringLength[words[[i]]]]]]; i = 0]}]
```



## REXX

Programming note:   This REXX program uses a freeware program   NIRCMD   to interface with the Microsoft Windows speech synthesizer program   '''SAM''',   a text to speech using a male voice.   SAM can possibly be configured to use other voices with later releases of Windows.   More recent Microsoft Windows have another speech synthesizer program:   ANNA.

Each word of the text is highlighted (by showing the word in uppercase).   the terminal screen is cleared before showing the text that is being spoken;   the repeated calls to the (Windows) speech engine makes for a slower speech rate. 

```rexx
/*REXX program uses a command line interface to invoke Windows SAM for speech synthesis.*/
parse arg t                                      /*get the (optional) text from the C.L.*/
#= words(t)
if #==0  then exit                               /*Nothing to say?    Then exit program.*/
dq= '"'                                          /*needed to enclose text in dbl quotes.*/
rate= 1                                          /*talk:   -10 (slow)   to   10 (fast). */
                                                 /* [↓]  where the rubber meets the road*/
   do j=1  for #
   x= word(t, j);          upper x               /*extract 1 word, capitalize it for HL.*/
   if j==1  then LHS=                            /*obtain text before the spoken word.  */
            else LHS= subword(t, 1, j-1)
   if j==#  then RHS=                            /*obtain text  after the spoken word.  */
            else RHS= subword(t, j+1)
   'CLS'                                         /*use this command to clear the screen.*/
   say 'speaking: '   space(LHS  x  RHS)         /*show text,  one word is capitalized. */
   oneWord= dq  x  dq                            /*surround a word in double quotes (").*/
   'NIRCMD'  "speak text"    oneWord     rate    /*NIRCMD  invokes Microsoft's Sam voice*/
   end   /*j*/                                   /*stick a fork in it,  we're all done. */
```

Note:   The name of the above REXX program is   '''SPEAKHI.REX'''


'''usage'''   using the command: 

```txt

speakhi This is an example of speech synthesis. 

```



## Ruby

I'm having difficulty figuring out how to get Shoes to update the GUI (like Tk's <code>update</code> command), so the user must click the button once for each word.

Uses the Ruby code from [[Speech synthesis]]

```ruby
load 'speechsynthesis.rb'

if ARGV.length == 1
  $text = "This is default text for the highlight and speak program"
else
  $text = ARGV[1..-1].join(" ")
end
$words = $text.split

Shoes.app do
  @idx = 0

  stack do
    @sentence = para(strong($words[0] + " "), $words[1..-1].map {|word| span(word + " ")})
    button "Say word" do
      say_and_highlight
    end
  end

  keypress do |key|
    case key
    when :control_q, "\x11" then exit
    end
  end

  def say_and_highlight
    speak $words[@idx]
    @idx = (@idx + 1) % $words.length
    @sentence.replace($words.each_with_index.map {|word, idx| idx == @idx ? strong(word + " ") : span(word + " ")})
  end
end
```



## Tcl

This code uses the external <code>/usr/bin/say</code> program (known available on Mac OS X) as its interface to the speech engine; this produces rather stilted speech because it forces the text to be spoken one word at a time instead of as a whole sentence (in order to keep the highlighting synchronized).
```tcl
package require Tcl 8.5
package require Tk 8.5
proc say {text button} {
    grab $button
    $button configure -state disabled -cursor watch
    update
    set starts [$text search -all -regexp -count lengths {\S+} 1.0]
    foreach start $starts length $lengths {
	lappend strings [$text get $start "$start + $length char"]
	lappend ends [$text index "$start + $length char"]
    }
    $text tag remove sel 1.0 end
    foreach from $starts str $strings to $ends {
	$text tag add sel $from $to
	update idletasks
	exec /usr/bin/say << $str
	$text tag remove sel 1.0 end
    }
    grab release $button
    $button configure -state normal -cursor {}
}

pack [text .t]
pack [button .b -text "Speak, computer!" -command {say .t .b}] -fill x
.t insert 1.0 "This is an example of speech synthesis with Tcl/Tk."
```

