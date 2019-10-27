+++
title = "Knapsack problem/Bounded/Go test"
description = ""
date = 2018-07-21T17:33:23Z
aliases = []
[extra]
id = 17907
[taxonomies]
categories = []
tags = []
+++

Simple test and benchmark for [[Knapsack_problem/Bounded#Go]].

```go
package main

import (
	"reflect"
	"testing"
)

func TestSol(t *testing.T) {
	//v, w, s := choose(400, len(items)-1, make(map[key]*Solution))
	v, w, s := Chooser{Items: items}.Choose(400)
	if e := 1010; v != e {
		t.Errorf("got value %d, expected %d", v, e)
	}
	if e := 396; w != e {
		t.Errorf("got weight %d, expected %d", w, e)
	}
	exSol := []int{1, 1, 1, 0, 2, 0, 3, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0}
	if !reflect.DeepEqual(s, exSol) {
		t.Errorf("got s\t%v,\n\texpected\t%v", s, exSol)
	}
}

func BenchmarkSol(b *testing.B) {
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		//choose(400, len(items)-1, make(map[key]*Solution))
		Chooser{Items: items}.Choose(400)
	}
}
```

{{out}}

```txt

% go test -bench=.
PASS
BenchmarkSol	    1000	   2266833 ns/op	 1587854 B/op	    3013 allocs/op
ok  	rosetta/kp_bounded	2.507s

```

If you're unfamiliar with using benchmarks within go tests:
* Put the above into a <tt>*_test.go</tt> file in the same directory as the <tt>*.go</tt> file from [[Knapsack_problem/Bounded#Go]] (and with no other Go source files)
* Get and build [http://godoc.org/golang.org/x/tools/cmd/benchcmp benchcmp] if you don't already have it: "<code>go get -v golang.org/x/tools/cmd/benchcmp</code>"
* Run <code>go test -v -bench=. > bench.out.orig</code> (or <code>go test -bench=. | tee bench.out.orig</code> or some such)
* Make changes as desired
* Re-run the benchmark this time saving the output to a different file, e.g. <tt>bench.out</tt>
* Compare the results with "<code>benchcmp bench.out{.orig,}</code>" (or if your shell doesn't support "<code>{}</code>", with something like "<code>benchcmp bench.out.orig bench.out</code>")
For example, when I changed the code to use a <code>struct</code> as a map key instead of a string, the output was:

```txt

% go test -bench=. | tee bench.out && benchcmp bench.out{.orig,}
PASS
BenchmarkSol	    1000	   2919580 ns/op	 1885978 B/op	   12239 allocs/op
ok  	rosetta/kp_bounded	3.221s
benchmark        old ns/op     new ns/op     delta
BenchmarkSol     6929766       2919580       -57.87%

benchmark        old allocs     new allocs     delta
BenchmarkSol     16963          12239          -27.85%

benchmark        old bytes     new bytes     delta
BenchmarkSol     1963326       1885978       -3.94%

```

