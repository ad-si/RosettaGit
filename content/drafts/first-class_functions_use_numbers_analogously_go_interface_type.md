+++
title = "First-class functions/Use numbers analogously/Go interface type"
description = ""
date = 2014-09-24T23:28:39Z
aliases = []
[extra]
id = 17963
[taxonomies]
categories = []
tags = []
+++

This is almost the same as the code on the [[First-class_functions/Use_numbers_analogously#Go|main page]]
except that the "empty interface" (<code>interface{}</code>) has been dropped in favour of a newly defined interface type and a pair of simple implementations of that interface.

This is perhaps more idiomatic and gives better compile time type checking with no run-time panics but it possibly "hides" the "first-class function" aspect of the task.

```go
package main

import "fmt"

func main() {
	var (
		x  Float   = 2
		xi Float   = .5
		y  Float   = 4
		yi Float   = .25
		z  FloatFn = func() float64 { return float64(x + y) }
		zi FloatFn = func() float64 { return 1 / float64(x+y) }
	)
	// point A

	numbers := []Number{&x, &y, z}
	inverses := []Number{&xi, &yi, zi}
	// point B

	mfs := make([]FloatFnArg, len(numbers))
	for i := range mfs {
		mfs[i] = multiplier(numbers[i], inverses[i])
	}
	// pointC

	for _, mf := range mfs {
		fmt.Println(mf(1))
	}
}

func multiplier(n1, n2 Number) FloatFnArg {
	return func(m float64) float64 {
		// close on interface objects n1, n2, and m
		return n1.Value() * n2.Value() * m
	}
}

type Float float64
type FloatFn func() float64
type FloatFnArg func(float64) float64

func (f Float) Value() float64    { return float64(f) }
func (fn FloatFn) Value() float64 { return fn() }

type Number interface {
	Value() float64
}
```

