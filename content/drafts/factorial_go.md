+++
title = "Factorial/Go"
description = ""
date = 2011-12-03T00:59:10Z
aliases = []
[extra]
id = 9125
[taxonomies]
categories = []
tags = []
+++

An implementation of one of Peter Luschny's [http://www.luschny.de/math/factorial/FastFactorialFunctions.htm fast factorial algorithms].  Fast as this algorithm is, I believe there is room to speed it up more with parallelization and attention to cache effects.  The Go library has a nice Karatsuba multiplier but it is yet single threaded.

```go
package main

import (
    "math/big"
    "fmt"
    "time"
)

func main() {
    const max = 1e6
    var s sieve
    s.sieve(max)

    // some trivial cases
    b := big.NewInt(1)
    tf(&s, 0, b)
    tf(&s, 1, b)
    tf(&s, 3, b.SetInt64(6))
    tf(&s, 10, b.SetInt64(3628800))

    // 20 is the first number that exercises the split factorial code
    tf(&s, 20, b.SetInt64(2432902008176640000))

    // 65 is the first number that exercises the split odd swing code
    b.SetString("8247650592082470666723170306785496252186258551345437492922123134388955774976000000000000000", 10)
    tf(&s, 65, b)

    // 402 is the first number that exercises the split primorial code
    b.SetString("10322493151921465164081017511444523549144957788957729070658850054871632028467255601190963314928373192348001901396930189622367360453148777593779130493841936873495349332423413459470518031076600468677681086479354644916620480632630350145970538235260826120203515476630017152557002993632050731959317164706296917171625287200618560036028326143938282329483693985566225033103398611546364400484246579470387915281737632989645795534475998050620039413447425490893877731061666015468384131920640823824733578473025588407103553854530737735183050931478983505845362197959913863770041359352031682005647007823330600995250982455385703739491695583970372977196372367980241040180516191489137558020294105537577853569647066137370488100581103217089054291400441697731894590238418118698720784367447615471616000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000", 10)
    tf(&s, 402, b)

    df(&s, 800)
    df(&s, 1e5)
    df(&s, max)
}

func tf(s *sieve, n uint, answer *big.Int) {
    f := s.factorial(n)
    if f.Cmp(answer) == 0 {
        fmt.Printf("%d! pass.\n", n)
    } else {
        fmt.Printf("%d! fail.\nExpected %s\nFound    %s\n",
            n, answer.String(), f.String())
    }
}

func df(s *sieve, n uint) {
    start := time.Now()
    a := s.factorial(n)
    stop := time.Now()
    fmt.Printf("n = %d  -> factorial %v\n", n, stop.Sub(start))

    dtrunc := int64(float64(a.BitLen())*.30103) - 10
    var first, rest big.Int
    rest.Exp(first.SetInt64(10), rest.SetInt64(dtrunc), nil)
    first.Quo(a, &rest)
    fstr := first.String()
    fmt.Printf("%d! begins %s... and has %d digits.\n",
        n, fstr, int64(len(fstr))+dtrunc)
}

func (s *sieve) factorial(n uint) *big.Int {
    if n < 20 {
        var r big.Int
        return r.MulRange(1, int64(n))
    }

    if int64(n) > s.limit {
        return nil
    }

    r := s.factorialS(n)
    return r.Lsh(r, n-bitCount32(uint32(n)))
}

func (s *sieve) factorialS(n uint) (swing *big.Int) {
    if n < 2 {
        return big.NewInt(1)
    }

    f2 := s.factorialS(n / 2) // recurse
    f2.Mul(f2, f2)            // square

    if n < uint(len(smallOddSwing)) {
        swing = big.NewInt(smallOddSwing[n])
    } else {
        swing = s.oddSwing(n)
    }

    return swing.Mul(swing, f2)
}

func (s *sieve) oddSwing(k uint) *big.Int {
    if k < uint(len(smallOddSwing)) {
        return big.NewInt(smallOddSwing[k])
    }

    factors := make([]int64, k/2)
    rootK := int64(floorSqrt(uint64(k)))
    var i int
    
    s.iterateFunc(3, rootK, func(p int64) bool {
        q := int64(k) / p
        for q > 0 {
            if q&1 == 1 {
                factors[i] = p
                i++
            }
            q /= p
        }
        return false
    })

    s.iterateFunc(rootK+1, int64(k/3), func(p int64) bool {
        if (int64(k) / p & 1) == 1 {
            factors[i] = p
            i++
        }
        return false
    })
    
    r := product(factors[0:i])
    return r.Mul(r, s.primorial(int64(k/2+1), int64(k)))
}   

func (s *sieve) primorial(m, n int64) *big.Int {
    if m > n {
        return big.NewInt(1)
    }
    
    if n-m < 200 {
        var r, r2 big.Int
        r.SetInt64(1)
        s.iterateFunc(m, n, func(p int64) bool {
            r.Mul(&r, r2.SetInt64(p))
            return false
        }) 
        return &r
    }

    h := (m + n) / 2
    r := s.primorial(m, h)
    return r.Mul(r, s.primorial(h+1, n))
}

type sieve struct {
    limit       int64
    isComposite []uint64
}

func (s *sieve) iterateFunc(min, max int64, visitor func(prime int64) (terminate bool)) (ok bool) {

    if max > s.limit {
        return false // Max larger than sieve
    }
    if min < 2 {
        min = 2
    }
    if min > max {
        return true
    }
    if min == 2 && max >= 2 {
        if visitor(2) {
            return true
        }
    }
    if min <= 3 && max >= 3 {
        if visitor(3) {
            return true
        }
    }

    absPos := (min+(min+1)%2)/3 - 1
    index := absPos / bitsPerInt
    bitPos := absPos % bitsPerInt
    prime := 5 + 3*(bitsPerInt*index+bitPos) - bitPos&1
    inc := bitPos&1*2 + 2

    for prime <= max {
        bitField := s.isComposite[index] >> uint64(bitPos)
        index++
        for ; bitPos < bitsPerInt; bitPos++ {
            if bitField&1 == 0 {
                if visitor(prime) {
                    return true
                }
            }
            prime += inc
            if prime > max {
                return true
            }
            inc = 6 - inc
            bitField >>= 1
        }
        bitPos = 0
    }
    return true
}   

// constants dependent on the word size of sieve.isComposite.
const ( 
    bitsPerInt = 64
    mask       = bitsPerInt - 1
    log2Int    = 6
)
    
func (s *sieve) sieve(n int64) {
    if n <= 0 {
        *s = sieve{}
    }

    s.limit = n
    s.isComposite = make([]uint64, n/(3*bitsPerInt)+1)

    var (
        d1, d2, p1, p2, s1, s2 uint64 = 8, 8, 3, 7, 7, 3
        l, c, max              uint64 = 0, 1, uint64(n) / 3
        toggle                 bool
    )

    for s1 < max {
        if (s.isComposite[l>>log2Int] & (1 << (l & mask))) == 0 {
            inc := p1 + p2
            for c = s1; c < max; c += inc {
                s.isComposite[c>>log2Int] |= 1 << (c & mask)
            } 
            for c = s1 + s2; c < max; c += inc {
                s.isComposite[c>>log2Int] |= 1 << (c & mask)
            }
        }

        l++
        if toggle {
            toggle = false
            s1 += d1
            d2 += 8
            p1 += 2
            p2 += 6
            s2 = p1
        } else {
            toggle = true
            s1 += d2
            d1 += 16
            p1 += 2
            p2 += 2
            s2 = p2
        }
    }
    return
}

var smallOddSwing []int64 = []int64{1, 1, 1, 3, 3, 15, 5,
    35, 35, 315, 63, 693, 231, 3003, 429, 6435, 6435,
    109395, 12155, 230945, 46189, 969969, 88179, 2028117, 676039,
    16900975, 1300075, 35102025, 5014575, 145422675, 9694845,
    300540195, 300540195, 9917826435, 583401555, 20419054425,
    2268783825, 83945001525, 4418157975, 172308161025,
    34461632205, 1412926920405, 67282234305, 2893136075115,
    263012370465, 11835556670925, 514589420475, 24185702762325,
    8061900920775, 395033145117975, 15801325804719,
    805867616040669, 61989816618513, 3285460280781189,
    121683714103007, 6692604275665385, 956086325095055,
    54496920530418135, 1879204156221315, 110873045217057585,
    7391536347803839, 450883717216034179, 14544636039226909,
    916312070471295267, 916312070471295267}

func bitCount32(w uint32) uint {
    const (
        ff    = 1<<32 - 1
        mask1 = ff / 3
        mask3 = ff / 5
        maskf = ff / 17
        maskp = ff / 255
    )
    w -= w >> 1 & mask1
    w = w&mask3 + w>>2&mask3
    w = (w + w>>4) & maskf
    return uint(w * maskp >> 24)
}

func floorSqrt(n uint64) (a uint64) {
    for b := n; ; {
        a := b
        b = (n/a + a) / 2
        if b >= a {
            return a
        }
    }
    return 0
}

func product(seq []int64) *big.Int {
    if len(seq) <= 20 {
        var b big.Int
        sprod := big.NewInt(seq[0])
        for _, s := range seq[1:] {
            b.SetInt64(s)
            sprod.Mul(sprod, &b)
        }
        return sprod
    }

    halfLen := len(seq) / 2 
    lprod := product(seq[0:halfLen]) 
    rprod := product(seq[halfLen:])
    return lprod.Mul(lprod, rprod)
}   
```

Output:
I did 800 because a few others had computed it.  Answers come pretty fast up to 10^5 but slow down after that.  Times shown are for computing the factorial only.  Producing the printable base 10 representation takes longer and isn't fun to wait for.

```txt

0! pass.
1! pass.
3! pass.
10! pass.
20! pass.
65! pass.
402! pass.
n = 800  -> factorial 318us
800! begins 7710530113... and has 1977 digits.
n = 100000  -> factorial 508.322ms
100000! begins 28242294079... and has 456574 digits.
n = 1000000  -> factorial 3m33.118625s
1000000! begins 8263931688... and has 5565709 digits.

```

