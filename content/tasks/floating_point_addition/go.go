package main

import (
	"fmt"
	"math/big"
)

func main() {
	a := 0.1
	b := 0.2
	fmt.Printf("%.17f\n", a+b)        // 0.30000000000000004
	fmt.Println(a+b == 0.3)           // false

	// math/big.Rat for exact rational arithmetic
	x := big.NewRat(1, 10)
	y := big.NewRat(2, 10)
	z := new(big.Rat).Add(x, y)
	fmt.Println(z.RatString())        // 3/10
}
