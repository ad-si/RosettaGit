+++
title = "Waveform analysis/Top and tail"
description = ""
date = 2019-08-11T17:34:18Z
aliases = []
[extra]
id = 10491
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "go",
]
+++

The task is to crop a given audio waveform file, removing any leading or trailing silence from the wave file, leaving just the audible sound.

The task should utilize a configurable [http://en.wikipedia.org/wiki/Squelch audio squelch level] calibration parameter that enables the user to set the silence level threshold (enabling low level background hiss or hum to be removed from the leading and trailing edges of the audio file.) 

Moments of silence (below the squelch threshold), should be removed from the leading and trailing edges of the input wave file, to produce a new cleanly cropped audio file.

Note that the output file format should be the same as the initial input format. This should not be changed by the implemented procedure.


## Go

As Go does not have any audio support in its standard library, this invokes the SoX utility to trim any leading or trailing silence from an audio file. 

Unfortunately, you must know the length of the silence at the end of the audio file to trim off silence reliably. To work around this, we first trim off leading silence from the file, reverse it and then trim off the leading silence from that file. We then reverse the resulting file to the output file specified by the user.

Setting the squelch level to 2 or 3 per cent of the maximum sample value worked reasonably well in my tests.

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
    const sec = "00:00:01"
    scanner := bufio.NewScanner(os.Stdin)
    name := ""
    for name == "" {
        fmt.Print("Enter name of audio file to be trimmed : ")
        scanner.Scan()
        name = scanner.Text()
        check(scanner.Err())
    }

    name2 := ""
    for name2 == "" {
        fmt.Print("Enter name of output file              : ")
        scanner.Scan()
        name2 = scanner.Text()
        check(scanner.Err())
    }    
    
    squelch := 0.0
    for squelch < 1 || squelch > 10 {
        fmt.Print("Enter squelch level % max (1 to 10)    : ")
        scanner.Scan()
        input := scanner.Text()
        check(scanner.Err())
        squelch, _ = strconv.ParseFloat(input, 64)
    }
    squelchS := strconv.FormatFloat(squelch, 'f', -1, 64) + "%"

    tmp1 := "tmp1_" + name
    tmp2 := "tmp2_" + name

    // Trim audio below squelch level from start and output to tmp1.
    args := []string{name, tmp1, "silence", "1", sec, squelchS}
    cmd := exec.Command("sox", args...)
    err := cmd.Run()
    check(err) 

    // Reverse tmp1 to tmp2.
    args = []string{tmp1, tmp2, "reverse"}     
    cmd = exec.Command("sox", args...)
    err = cmd.Run()
    check(err)

    // Trim audio below squelch level from tmp2 and output to tmp1.  
    args = []string{tmp2, tmp1, "silence", "1", sec, squelchS}
    cmd  = exec.Command("sox", args...)
    err = cmd.Run()
    check(err)

    // Reverse tmp1 to the output file.
    args = []string{tmp1, name2, "reverse"}    
    cmd = exec.Command("sox", args...)
    err = cmd.Run()
    check(err)

    // Remove the temporary files.
    err = os.Remove(tmp1)
    check(err)
    err = os.Remove(tmp2)
    check(err)
}
```


