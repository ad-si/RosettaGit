+++
title = "Audio Overlap Loop"
description = ""
date = 2018-12-31T19:10:30Z
aliases = []
[extra]
id = 10153
[taxonomies]
categories = ["Audio", "Multimedia", "task"]
tags = []
+++

## Task

'''Audio Overlap Loop''' is a program that produces an "echo chamber" effect by playing an audio file several times in an overlapping loop. A repetition level determines the number of times that the file is looped. For the purpose of this task, write a program that takes a parameter for the number of repetitions and plays the file ''loop.wav'' in an overlapping loop according to the number of repetitions.

Optionally take parameters for delay between repetitions, and decay (fractional volume reduction between consecutive repetitions).



## Go

As Go does not have any audio support in its standard library, this invokes the SoX utility's 'play' command with the appropriate parameters to achieve the 'echo chamber' effect. 

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
    fileName := "loop.wav"
    scanner := bufio.NewScanner(os.Stdin)
    reps := 0
    for reps < 1 || reps > 6 {
        fmt.Print("Enter number of repetitions (1 to 6) : ")
        scanner.Scan()
        input := scanner.Text()
        check(scanner.Err())
        reps, _ = strconv.Atoi(input)
    }

    delay := 0
    for delay < 50 || delay > 500 {
        fmt.Print("Enter delay between repetitions in microseconds (50 to 500) : ")
        scanner.Scan()
        input := scanner.Text()
        check(scanner.Err())
        delay, _ = strconv.Atoi(input)
    }

    decay := 0.0
    for decay < 0.2 || decay > 0.9 {
        fmt.Print("Enter decay between repetitions (0.2 to 0.9) : ")
        scanner.Scan()
        input := scanner.Text()
        check(scanner.Err())
        decay, _ = strconv.ParseFloat(input, 64)
    }

    args := []string{fileName, "echo", "0.8", "0.7"}
    decay2 := 1.0    
    for i := 1; i <= reps; i++ {
        delayStr := strconv.Itoa(i * delay)
        decay2 *= decay
        decayStr := strconv.FormatFloat(decay2, 'f', -1, 64)        
        args = append(args, delayStr, decayStr)        
    }
    cmd := exec.Command("play", args...)    
    err := cmd.Run()
    check(err)
}
```


=={{header|JavaScript}}/{{header|HTML}}==

```JavaScript><script

var j = prompt("Enter the sound manipulation level you want", "");
for(i=0; i<j; i++) {
    document.write("<bgsound src='loop.wav'>")
}
</script>
```



## Tcl


Using the popular [http://www.speech.kth.se/snack/ snack] ([http://wiki.tcl.tk/2647 wiki]) extension for audio support, the following presents a GUI to control echo production.

This uses a couple of interesting Tcl features:

* GUI widgets bound to global variables
* playback runs in a coroutine to provide asynchrony with yield
* <tt>lock_play</tt> provides a "transaction"-style control structure similar to "with" in [Python] or (with-*) in [Lisp]

As a bonus, two playback methods are provided - <tt>run</tt> and <tt>mix</tt>, which exercise different capabilities of snack (playing multiple sounds at once vs programmable filters).
Notice that <tt>run</tt> disabled buttons only until the last echo has started, while <tt>mix</tt> does so until the entire playback (cropped to sound level 0) is completed.
Either of these may be desirable in different circumstances, so both are left as an example.


```Tcl
package require Tk
package require snack


# variables bound to GUI:
set filename "sample.wav"
set nreps    5
set delay    200
set decay    0.9


# initial snack objects:
snack::sound wav -load sample.wav
snack::sound mixed  ;# used by [run]
snack::sound out    ;# used by [mix]

snack::sound hush -rate [wav cget -rate] -channels [wav cget -channels]
hush length [wav length]


proc make_gui {} {
    grid [label .l0 -text "Filename:"] [button .b0 -textvariable ::filename -command choose_file] -sticky nsew
    grid [label .l1 -text "Repetitions"] [entry .e1 -textvariable ::nreps] -sticky nsew
    grid [label .l2 -text "Pause"] [entry .e2 -textvariable ::delay] -sticky nsew
    grid [label .l3 -text "Decay"] [entry .e3 -textvariable ::decay] -sticky nsew
    grid [frame .b] -   ;# "-" for colspan
    grid [
        button .b.run  -text "Play" -command {coroutine runner run}
    ] [
        button .b.mix  -text "Premix" -command {coroutine runner mix}
    ] [
        button .b.stop -text "Stop" -command stop -state disabled
    ] [
        button .b.exit -text "Exit" -command exit
    ] -sticky nsew
}

# snack wraps tk_getOpenFile with suitable options to load supported audio files
proc choose_file {} {
    global filename
    set fn [snack::getOpenFile -initialfile $filename]
    if {$fn eq ""} return
    wav read [set filename $fn]
}

# disable play and enable stop for the duration of $script
proc lock_play {script} {
    .b.run configure -state disabled
    .b.mix configure -state disabled
    .b.stop configure -state normal
    try {
        uplevel 1 $script
    } finally {
        .b.run configure -state normal
        .b.mix configure -state normal
        .b.stop configure -state disabled
    }
}

# play by starting each echo as a distinct sound
proc run {} {
    global nreps delay decay
    lock_play {
        mixed copy wav
        mixed play
        for {set i 1} {$i < $nreps} {incr i} {
            yieldto after $delay [list catch [info coroutine]]  ;# delay without blocking the event loop
                                                                ;# [catch] in case the coroutine has been killed
            mixed mix hush -prescaling $decay   ;# scale and mix with silence to fade
            mixed play
        }
    }
}

# play using snack::filter to create the echo
proc mix {} {
    global nreps delay decay
    lock_play {
        out copy wav
        set args {} ;# for snack::filter echo
        for {set i 1} {$i < $nreps} {incr i} {
            lappend args [expr {$delay * $i}] [expr {$decay ** $i}]
        }
        set filter [snack::filter echo 1 1 {*}$args]
        out filter $filter
        $filter destroy
        yieldto out play -command [info coroutine]  ;# return to this proc only when playback completed
    }
}

# stop playback
proc stop {} {
    lock_play {
        foreach s {wav mixed out} {
            $s stop     ;# stop all sounds that may be playing
            catch {rename runner {}}    ;# kill the coroutine if it exists
        }
    }
}

make_gui

```


{{omit from|Bc}}
{{omit from|Brlcad}}
{{omit from|GUISS}}
{{omit from|Lotus 123 Macro Scripting}}

[[Category:Temporal media]]
