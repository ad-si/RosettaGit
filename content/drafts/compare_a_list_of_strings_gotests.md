+++
title = "Compare a list of strings/GoTests"
description = ""
date = 2017-05-07T19:59:33Z
aliases = []
[extra]
id = 21371
[taxonomies]
categories = []
tags = []
+++

The Go test tool can help validate code as an alternative to writing a complete program and providing output.  The task is coded as a Go package.  Code here would be named something like test_cmp.go and exist in the same directory as the task code.


```go
package cmp_test

import (
    "testing"

    "cmp"
)

var eqTests = []struct {
    desc string
    list []string
    want bool
}{
    {
        "just one string",
        []string{"a"},
        true,
    },
    {
        "2 equal",
        []string{"a", "a"},
        true,
    },
    {
        "2 unequal",
        []string{"a", "b"},
        false,
    },
}

var ltTests = []struct {
    desc string
    list []string
    want bool
}{
    {
        "just one string",
        []string{"a"},
        true,
    },
    {
        "2 ordered",
        []string{"a", "b"},
        true,
    }, 
    {
        "2 not strictly ordered",
        []string{"a", "a"},
        false,
    },
}
    
func TestAllEqual(t *testing.T) {
    for _, tc := range eqTests {
        if got := cmp.AllEqual(tc.list); got != tc.want {
            t.Errorf("%s: want %t, got %t", tc.desc, tc.want, got)
        }
    }
}
    
func TestLessThan(t *testing.T) {
    for _, tc := range ltTests {
        if got := cmp.AllLessThan(tc.list); got != tc.want {
            t.Errorf("%s: want %t, got %t", tc.desc, tc.want, got)
        }
    }
}
```

{{out | Example test}}

```txt

$ go test -coverprofile p
PASS
coverage: 100.0% of statements
ok  	cmp	0.004s

```

