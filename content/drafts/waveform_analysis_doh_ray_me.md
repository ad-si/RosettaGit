+++
title = "Waveform analysis/Doh ray me"
description = ""
date = 2019-08-11T17:36:40Z
aliases = []
[extra]
id = 10078
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
Analyse a given section of monophonic audio waveform, for average pitch and output one of the sol-fa trigraphs (like [[wp:Tonic_sol-fa|these]], except each has three letters) corresponding to average pitch level: Doh, Ray, Mee, Fah, Soh, Lah, Tee, doh.

Optionally, follow the trigraph with a plus or minus symbol, to indicate whether the note falls above or below the solfa. Extend the scale to cover 21 notes: DOH, RAY, MEE, FAH, SOH, LAH, TEE, Doh, Ray, Mee, Fah, Soh, Lah, Tee, doh, ray, mee, fah, soh, lah, tee.

A calibration parameter can be provided to suit different voices. This can be provided as a variable defined within the code.


## Go

Clearly, this task is only feasible if you know how frequencies are encoded as bytes in the waveform and even then there are mathematical difficulties in reversing the procedure which mean that the eventual result is unlikely to be exact.

As an example, we analyze the .wav file (notes.wav) created by the [[https://rosettacode.org/wiki/Musical_scale#Go Musical Scale]] task. As we know that the same frequency was used to generate each sample (44100 bytes), it is only necessary to examine a small number of bytes for each sample to determine the average frequency for the file as a whole (8 samples). 

However, as each calculation was of necessity rounded to the nearer byte, it seems sensible to use more than one byte per sample (but not so many that the multi-valued arcsine function will be applied to a value outside its principal range) to try and reduce the effect of rounding. 20 bytes per sample is used here though curiously using only 3 bytes per sample would have produced a more accurate result (384.9 Hz).

Some optional aspects of the task have been ignored as they are not relevant to this particular example.

```go
package main

import (
    "encoding/binary"
    "fmt"
    "log"
    "math"
    "os"
)

var (
    freqs = []float64{261.6, 293.6, 329.6, 349.2, 392.0, 440.0, 493.9, 523.3}
    notes = []string{"Doh", "Ray", "Mee", "Fah", "Soh", "Lah", "Tee", "doh"}
)

func getNote(freq float64) string {
    index := len(freqs)
    for i := 0; i < len(freqs); i++ {
        if freq <= freqs[i] {
            index = i
            break
        }
    }
    switch index {
    case 0:
        return "Doh-"
    case len(freqs):
        return "doh+"
    default:
        if freqs[index]-freq <= freq-freqs[index-1] {
            return notes[index] + "-"
        }
        return notes[index-1] + "+"
    }
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func main() {
    file, err := os.Open("notes.wav")
    check(err)
    defer file.Close()    
    hdr := make([]byte, 44)
    _, err = file.Read(hdr)
    check(err)
    // check header parameters
    sampleRate := int(binary.LittleEndian.Uint32(hdr[24:28]))
    fmt.Println("Sample Rate    :", sampleRate)
    dataLength := int(binary.LittleEndian.Uint32(hdr[40:]))
    duration := dataLength / sampleRate
    fmt.Println("Duration       :", duration)

    sum := 0.0
    sampleRateF := float64(sampleRate)
    data := make([]byte, sampleRate)
    nbytes := 20
    fmt.Println("Bytes examined :", nbytes, "per sample") 
    for j := 0; j < duration; j++ {
        _, err := file.Read(data)
        check(err)
        for i := 1; i <= nbytes; i++ {
            bf := float64(data[i]) / 32
            freq := math.Asin(bf) * sampleRateF / (float64(i) * math.Pi * 2)
            sum += freq
        }
    }
    cav := sum / (float64(duration) * float64(nbytes))
    fmt.Printf("\nComputed average frequency = %.1f Hz (%s)\n", cav, getNote(cav))

    sum = 0.0
    for i := 0; i < len(freqs); i++ {
        sum += freqs[i]
    }
    aav := sum / float64(len(freqs))
    fmt.Printf("Actual average frequency   = %.1f Hz (%s)\n", aav, getNote(aav))
}
```


{{out}}

```txt

Sample Rate    : 44100
Duration       : 8
Bytes examined : 20 per sample

Computed average frequency = 387.1 Hz (Soh-)
Actual average frequency   = 385.4 Hz (Soh-)
```


{{omit from|AWK}}
{{omit from|Blast}}
{{omit from|AutoHotkey}}
{{omit from|Befunge}}
{{omit from|GUISS}}
{{omit from|HTML}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|Openscad}}
{{omit from|Scala}}
{{omit from|TPP}}
{{omit from|UNIX Shell}}

[[Category:Voice recognition]]
[[Category:Waveform analysis]]
