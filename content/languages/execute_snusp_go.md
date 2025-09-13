+++
title = "Execute SNUSP/Go"
description = ""
date = 2014-06-11T17:31:01Z
aliases = []
[extra]
id = 9524
[taxonomies]
categories = []
tags = []
+++


Only Core SNUSP today.  Fixed size data store, no bounds checking.

```go
package main

import (
    "fmt"
    "strings"
)

// lovely Core SNUSP implementation of Hello World,
// taken from the Ruby Execute SNUSP page.
// it happens to run with only 5 bytes of data store!
const hw = `
/++++!/
### =====
?\>++.>+.+++++++..+++\
\+++\ | /+>+++++++>/ /++++++++++<<.++>./
$+++/ | \+++++++++>\ \+++++.>.+++.-----\
      \==-<<<<+>+++/ /=.>.+>.--------.-/`

func main() {
    snusp(5, hw)
}

// input is a multi-line string.
func snusp(dLen int, raw string) {
    ds := make([]byte, dLen) // data store
    var dp int               // data pointer

    // toss leading \n of code if it's there
    if raw[0] == '\n' {
        raw = raw[1:]
    }

    // make 2 dimensional instruction store, declare instruction pointers
    is := strings.Split(raw, "\n")
    var ipr, ipc int

    // look for starting instruction
findStart:
    for r, row := range is {
        for c, i := range row {
            if i == '$' {
                ipr, ipc = r, c
                break findStart
            }
        }
    }

    // starting direction is always rt
    const (
        rt = iota
        dn
        lt
        up
    )
    id := rt

    // handy, below
    step := func() {
        if id&1 == 0 {
            ipc += 1 - int(id&2)
        } else {
            ipr += 1 - int(id&2)
        }
    }

    // execute
    for ipr >= 0 && ipr < len(is) && ipc >= 0 && ipc < len(is[ipr]) {
        switch is[ipr][ipc] {
        case '>':
            dp++
        case '<':
            dp--
        case '+':
            ds[dp]++
        case '-':
            ds[dp]--
        case '.':
            fmt.Printf("%c", ds[dp])
        case ',':
            fmt.Scanf("%c", &ds[dp])
        case '/':
            id = ^id
        case '\\':
            id ^= 1
        case '!':
            step()
        case '?':
            if ds[dp] == 0 {
                step()
            }
        }
        step()
    }
}
```

Output:

```txt

Hello World!

```

