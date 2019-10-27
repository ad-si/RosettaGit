+++
title = "RCHQ9+/Go"
description = ""
date = 2018-04-18T19:11:15Z
aliases = []
[extra]
id = 9528
[taxonomies]
categories = []
tags = []
+++

{{implementation|HQ9+}}{{collection|RCHQ9+}}
This interpreter accepts HQ9+ source as the first command line argument.

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    if len(os.Args) < 2 {
        return
    }
    for _, c := range os.Args[1] {
        switch c {
        case 'H':
            fmt.Println("hello, world")
        case 'Q':
            fmt.Printf("%s%c%s%c\n", q, 96, q, 96)
        case '9':
            const l1t = " of beer on the wall"
            const l2t = " of beer.\nTake one down, pass it around,\n"
            const l1p = " bottles"+l1t
            const l2p = " bottles"+l2t
            fmt.Print(99, l1p, ",\n", 99, l2p)
            for n := 98; n > 1; n-- {
                fmt.Print(n, l1p, ".\n\n", n, l1p, ",\n", n, l2p)
            }
            fmt.Print("One bottle"+l1t+".\n\n")
            fmt.Print("One bottle"+l1t+",\nOne bottle"+l2t+"No"+l1p+".\n")
        case '+':
            a++
        }
    }
}

var a int
var q = `package main

import (
    "fmt"
    "os"
)

func main() {
    if len(os.Args) < 2 {
        return
    }
    for _, c := range os.Args[1] {
        switch c {
        case 'H':
            fmt.Println("hello, world")
        case 'Q':
            fmt.Printf("%s%c%s%c\n", q, 96, q, 96)
        case '9':
            const l1t = " of beer on the wall"
            const l2t = " of beer.\nTake one down, pass it around,\n"
            const l1p = " bottles"+l1t
            const l2p = " bottles"+l2t
            fmt.Print(99, l1p, ",\n", 99, l2p)
            for n := 98; n > 1; n-- {
                fmt.Print(n, l1p, ".\n\n", n, l1p, ",\n", n, l2p)
            }
            fmt.Print("One bottle"+l1t+".\n\n")
            fmt.Print("One bottle"+l1t+",\nOne bottle"+l2t+"No"+l1p+".\n")
        case '+':
            a++
        }
    }
}

var a int
var q = `
```

{{out|Example session building and running program}}

```txt

$ go build HQ9+.go
$ ./HQ9+ H+H
hello, world
hello, world

```

{{out|Another, running Q output}}

```txt

$ ./HQ9+ Q >q.go
$ go run q.go H
hello, world

```

