+++
title = "First perfect square in base N with N unique digits"
description = ""
date = 2019-10-21T15:44:12Z
aliases = []
[extra]
id = 22334
[taxonomies]
categories = []
tags = []
+++

{{task}}
Find the first perfect square in a given base '''N''' that has at least '''N''' digits and
exactly '''N''' ''significant unique'' digits when expressed in base '''N'''.

E.G. In base '''10''', the first perfect square with at least '''10''' unique digits is '''1026753849''' ('''32043²''').

You may use analytical methods to reduce the search space, but the code must do a search. Do not use magic numbers or just feed the code the answer to verify it is correct.

;Task

* Find and display here, on this page, the first perfect square in base '''N''', with '''N''' significant unique digits when expressed in base '''N''', for each of base '''2''' through '''12'''. Display each number in the base '''N''' for which it was calculated.

* (optional) Do the same for bases '''13''' through '''16'''.

* (stretch goal) Continue on for bases '''17''' - '''??''' (Big Integer math)


;See also:
::*   [[oeis:A260182|OEIS A260182: smallest square that is pandigital in base n]].

;Related task
[[Casting out nines]]




## C++

{{trans|C#}}
A stripped down version of the C#, using unsigned longs instead of BigIntegers, and shifted bits instead of a HashSet accumulator.

```cpp
#include <string>
#include <iostream>
#include <cstdlib>
#include <math.h>
#include <chrono>
#include <iomanip>

using namespace std;

const int maxBase = 16;  // maximum base tabulated
int base, bmo, tc; // globals: base, base minus one, test count
const string chars = "0123456789ABCDEF"; // characters to use for the different bases
unsigned long long full; // for allIn() testing

// converts base 10 to string representation of the current base
string toStr(const unsigned long long ull) {
	unsigned long long u = ull; string res = ""; while (u > 0) {
		lldiv_t result1 = lldiv(u, base); res = chars[(int)result1.rem] + res;
		u = (unsigned long long)result1.quot;
	} return res;
}

// converts string to base 10
unsigned long long to10(string s) {
	unsigned long long res = 0; for (char i : s) res = res * base + chars.find(i); return res;
}

// determines whether all characters are present
bool allIn(const unsigned long long ull) {
	unsigned long long u, found; u = ull; found = 0; while (u > 0) {
		lldiv_t result1 = lldiv(u, base); found |= (unsigned long long)1 << result1.rem;
		u = result1.quot;
	} return found == full;
}

// returns the minimum value string, optionally inserting extra digit
string fixup(int n) {
	string res = chars.substr(0, base); if (n > 0) res = res.insert(n, chars.substr(n, 1));
	return "10" + res.substr(2);
}

// perform the calculations for one base
void doOne() {
	bmo = base - 1; tc = 0; unsigned long long sq, rt, dn, d;
	int id = 0, dr = (base & 1) == 1 ? base >> 1 : 0, inc = 1, sdr[maxBase] = { 0 };
	full = ((unsigned long long)1 << base) - 1;
	int rc = 0; for (int i = 0; i < bmo; i++) {
		sdr[i] = (i * i) % bmo; if (sdr[i] == dr) rc++; if (sdr[i] == 0) sdr[i] += bmo;
	}
	if (dr > 0) {
		id = base; for (int i = 1; i <= dr; i++)
			if (sdr[i] >= dr) if (id > sdr[i]) id = sdr[i]; id -= dr;
	}
	sq = to10(fixup(id)); rt = (unsigned long long)sqrt(sq) + 1; sq = rt * rt;
	dn = (rt << 1) + 1; d = 1; if (base > 3 && rc > 0) {
		while (sq % bmo != dr) { rt += 1; sq += dn; dn += 2; } // alligns sq to dr
		inc = bmo / rc; if (inc > 1) { dn += rt * (inc - 2) - 1; d = inc * inc; }
		dn += dn + d;
	} d <<= 1;
	do { if (allIn(sq)) break; sq += dn; dn += d; tc++; } while (true);
	rt += tc * inc;
	cout << setw(4) << base << setw(3) << inc << "  " << setw(2)
		<< (id > 0 ? chars.substr(id, 1) : " ") << setw(10) << toStr(rt) << "  "
		<< setw(20) << left << toStr(sq) << right << setw(12) << tc << endl;
}

int main() {
	cout << "base inc id      root  sqr                   test count" << endl;
	auto st = chrono::system_clock::now();
	for (base = 2; base <= maxBase; base++) doOne();
	chrono::duration<double> et = chrono::system_clock::now() - st;
	cout << "\nComputation time was " << et.count() * 1000 << " milliseconds" << endl;
	return 0;
}
```

{{out}}

```txt
base inc id      root  sqr                   test count
   2  1            10  100                            0
   3  1            22  2101                           4
   4  3            33  3201                           2
   5  1   2       243  132304                        14
   6  5           523  452013                        20
   7  6          1431  2450361                       34
   8  7          3344  13675420                      41
   9  4         11642  136802574                    289
  10  3         32043  1026753849                    17
  11 10        111453  1240A536789                 1498
  12 11        3966B9  124A7B538609                6883
  13  1   3   3828943  10254773CA86B9              8242
  14 13       3A9DB7C  10269B8C57D3A4              1330
  15 14      1012B857  102597BACE836D4             4216
  16 15      404A9D9B  1025648CFEA37BD9           18457

Computation time was 25.9016 milliseconds
```


=={{header|C#|Csharp}}==
{{libheader|System.Numerics}}
{{trans|Visual Basic .NET}}
Based on the Visual Basic .NET version, plus it shortcuts some of the ''allIn()'' checks.  When the numbers checked are below a threshold, not every digit needs to be checked, saving a little time.

```c#
using System;
using System.Collections.Generic;
using System.Numerics;

static class Program
{
    static byte Base, bmo, blim, ic; static DateTime st0; static BigInteger bllim, threshold;
    static HashSet<byte> hs = new HashSet<byte>(), o = new HashSet<byte>();
    static string chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz|";
    static List<BigInteger> limits;
    static string ms;

    // convert BigInteger to string using current base
    static string toStr(BigInteger b) {
        string res = ""; BigInteger re; while (b > 0) {
            b = BigInteger.DivRem(b, Base, out re); res = chars[(byte)re] + res;
        } return res;
    }

    // check for a portion of digits, bailing if uneven
    static bool allInQS(BigInteger b) {
        BigInteger re; int c = ic; hs.Clear(); hs.UnionWith(o); while (b > bllim) {
            b = BigInteger.DivRem(b, Base, out re);
            hs.Add((byte)re); c += 1; if (c > hs.Count) return false;
        } return true;
    }

    // check for a portion of digits, all the way to the end
    static bool allInS(BigInteger b) {
        BigInteger re; hs.Clear(); hs.UnionWith(o); while (b > bllim) {
            b = BigInteger.DivRem(b, Base, out re); hs.Add((byte)re);
        } return hs.Count == Base;
    }

    // check for all digits, bailing if uneven
    static bool allInQ(BigInteger b) {
        BigInteger re; int c = 0; hs.Clear(); while (b > 0) {
            b = BigInteger.DivRem(b, Base, out re);
            hs.Add((byte)re); c += 1; if (c > hs.Count) return false;
        } return true;
    }

    // check for all digits, all the way to the end
    static bool allIn(BigInteger b) {
        BigInteger re; hs.Clear(); while (b > 0) {
            b = BigInteger.DivRem(b, Base, out re); hs.Add((byte)re);
        } return hs.Count == Base;
    }

    // parse a string into a BigInteger, using current base
    static BigInteger to10(string s) {
        BigInteger res = 0; foreach (char i in s) res = res * Base + chars.IndexOf(i);
        return res;
    }

    // returns the minimum value string, optionally inserting extra digit
    static string fixup(int n) {
        string res = chars.Substring(0, Base); if (n > 0) res = res.Insert(n, n.ToString());
        return "10" + res.Substring(2);
    }

    // checks the square against the threshold, advances various limits when needed
    static void check(BigInteger sq) {
        if (sq > threshold) {
            o.Remove((byte)chars.IndexOf(ms[blim])); blim -= 1; ic -= 1;
            threshold = limits[bmo - blim - 1]; bllim = to10(ms.Substring(0, blim + 1));
        }
    }

    // performs all the caclulations for the current base
    static void doOne() {
        limits = new List<BigInteger>();
        bmo = (byte)(Base - 1); byte dr = 0; if ((Base & 1) == 1) dr = (byte)(Base >> 1);
        o.Clear(); blim = 0;
        byte id = 0; int inc = 1; long i = 0; DateTime st = DateTime.Now; if (Base == 2) st0 = st;
        byte[] sdr = new byte[bmo]; byte rc = 0; for (i = 0; i < bmo; i++) {
            sdr[i] = (byte)((i * i) % bmo); rc += sdr[i] == dr ? (byte)1 : (byte)0;
            sdr[i] += sdr[i] == 0 ? bmo : (byte)0;
        } i = 0; if (dr > 0) {
            id = Base;
            for (i = 1; i <= dr; i++) if (sdr[i] >= dr) if (id > sdr[i]) id = sdr[i]; id -= dr;
            i = 0;
        } ms = fixup(id);
        BigInteger sq = to10(ms); BigInteger rt = new BigInteger(Math.Sqrt((double)sq) + 1);
        sq = rt * rt; if (Base > 9) {
            for (int j = 1; j < Base; j++)
                limits.Add(to10(ms.Substring(0, j) + new string(chars[bmo], Base - j + (rc > 0 ? 0 : 1))));
            limits.Reverse(); while (sq < limits[0]) { rt++; sq = rt * rt; }
        }
        BigInteger dn = (rt << 1) + 1; BigInteger d = 1; if (Base > 3 && rc > 0) {
            while (sq % bmo != dr) { rt += 1; sq += dn; dn += 2; } // alligns sq to dr
            inc = bmo / rc;
            if (inc > 1) { dn += rt * (inc - 2) - 1; d = inc * inc; }
            dn += dn + d;
        }
        d <<= 1; if (Base > 9) {
            blim = 0; while (sq < limits[bmo - blim - 1]) blim++; ic = (byte)(blim + 1);
            threshold = limits[bmo - blim - 1];
            if (blim > 0) for (byte j = 0; j <= blim; j++) o.Add((byte)chars.IndexOf(ms[j]));
            if (blim > 0) bllim = to10(ms.Substring(0, blim + 1)); else bllim = 0;
            if (Base > 5 && rc > 0)
                do { if (allInQS(sq)) break; sq += dn; dn += d; i += 1; check(sq); } while (true);
            else
                do { if (allInS(sq)) break; sq += dn; dn += d; i += 1; check(sq); } while (true);
        } else {
            if (Base > 5 && rc > 0)
                do { if (allInQ(sq)) break; sq += dn; dn += d; i += 1; } while (true);
            else
                do { if (allIn(sq)) break; sq += dn; dn += d; i += 1; } while (true);
        } rt += i * inc;
        Console.WriteLine("{0,3}  {1,2}  {2,2} {3,20} -> {4,-40} {5,10} {6,9:0.0000}s  {7,9:0.0000}s",
            Base, inc, (id > 0 ? chars.Substring(id, 1) : " "), toStr(rt), toStr(sq), i,
            (DateTime.Now - st).TotalSeconds, (DateTime.Now - st0).TotalSeconds);
    }

    static void Main(string[] args) {
        Console.WriteLine("base inc id                 root    square" +
            "                                   test count    time        total");
        for (Base = 2; Base <= 28; Base++) doOne();
        Console.WriteLine("Elasped time was {0,8:0.00} minutes", (DateTime.Now - st0).TotalMinutes);
    }
}
```

{{out}}

```txt
base inc id                 root    square                                   test count    time        total
  2   1                       10 -> 100                                               0    0.0050s     0.0050s
  3   1                       22 -> 2101                                              4    0.0000s     0.0050s
  4   3                       33 -> 3201                                              2    0.0010s     0.0060s
  5   1   2                  243 -> 132304                                           14    0.0000s     0.0060s
  6   5                      523 -> 452013                                           20    0.0000s     0.0060s
  7   6                     1431 -> 2450361                                          34    0.0000s     0.0060s
  8   7                     3344 -> 13675420                                         41    0.0000s     0.0060s
  9   4                    11642 -> 136802574                                       289    0.0010s     0.0070s
 10   3                    32043 -> 1026753849                                       17    0.0050s     0.0120s
 11  10                   111453 -> 1240A536789                                    1498    0.0010s     0.0130s
 12  11                   3966B9 -> 124A7B538609                                   6883    0.0040s     0.0170s
 13   1   3              3828943 -> 10254773CA86B9                                 8242    0.0439s     0.0609s
 14  13                  3A9DB7C -> 10269B8C57D3A4                                 1330    0.0010s     0.0619s
 15  14                 1012B857 -> 102597BACE836D4                                4216    0.0020s     0.0638s
 16  15                 404A9D9B -> 1025648CFEA37BD9                              18457    0.0100s     0.0738s
 17   1   1            423F82GA9 -> 101246A89CGFB357ED                           195112    0.2783s     0.3521s
 18  17                44B482CAD -> 10236B5F8EG4AD9CH7                            30440    0.0199s     0.3720s
 19   6               1011B55E9A -> 10234DHBG7CI8F6A9E5                           93021    0.0589s     0.4309s
 20  19               49DGIH5D3G -> 1024E7CDI3HB695FJA8G                       11310604    6.9833s     7.4142s
 21   1   6          4C9HE5FE27F -> 1023457DG9HI8J6B6KCEAF                       601843    1.0871s     8.5013s
 22  21              4F94788GJ0F -> 102369FBGDEJ48CHI7LKA5                     27804949   18.3290s    26.8302s
 23  22             1011D3EL56MC -> 10234ACEDKG9HM8FBJIL756                    17710217   11.4105s    38.2407s
 24  23             4LJ0HDGF0HD3 -> 102345B87HFECKJNIGMDLA69                    4266555    2.4763s    40.7171s
 25  12            1011E145FHGHM -> 102345DOECKJ6GFB8LIAM7NH9                  78092124   52.6831s    93.4012s
 26   5            52K8N53BDM99K -> 1023458LO6IEMKG79FPCHNJDBA                402922568  287.9058s   381.3080s
 27  26           1011F11E37OBJJ -> 1023458ELOMDHBIJFGKP7CQ9N6A               457555293  326.1714s   707.4794s
 28   9           58A3CKP3N4CQD7 -> 1023456CGJBIRQEDHP98KMOAN7FL              749592976  508.4498s  1215.9292s
Elasped time was    20.27 minutes

```


=={{header|F_Sharp|F#}}==

### The Task


```fsharp

// Nigel Galloway: May 21st., 2019
let fN g=let g=int64(sqrt(float(pown g (int(g-1L)))))+1L in (Seq.unfold(fun(n,g)->Some(n,(n+g,g+2L))))(g*g,g*2L+1L)
let fG n g=Array.unfold(fun n->if n=0L then None else let n,g=System.Math.DivRem(n,g) in Some(g,n)) n
let fL g=let n=set[0L..g-1L] in Seq.find(fun x->set(fG x g)=n) (fN g)
let toS n g=let a=Array.concat [[|'0'..'9'|];[|'a'..'f'|]] in System.String(Array.rev(fG n g)|>Array.map(fun n->a.[(int n)]))
[2L..16L]|>List.iter(fun n->let g=fL n in printfn "Base %d: %s² -> %s" n (toS (int64(sqrt(float g))) n) (toS g n))

```

{{out}}

```txt

Base 2: 10² -> 100
Base 3: 22² -> 2101
Base 4: 33² -> 3201
Base 5: 243² -> 132304
Base 6: 523² -> 452013
Base 7: 1431² -> 2450361
Base 8: 3344² -> 13675420
Base 9: 11642² -> 136802574
Base 10: 32043² -> 1026753849
Base 11: 111453² -> 1240a536789
Base 12: 3966b9² -> 124a7b538609
Base 13: 3828943² -> 10254773ca86b9
Base 14: 3a9db7c² -> 10269b8c57d3a4
Base 15: 1012b857² -> 102597bace836d4
Base 16: 404a9d9b² -> 1025648cfea37bd9

```

===Using [[Factorial base numbers indexing permutations of a collection]]===
On the discussion page for [[Factorial base numbers indexing permutations of a collection]] an anonymous contributor queries the value of [[Factorial base numbers indexing permutations of a collection]]. Well let's see him use an inverse Knuth shuffle to partially solve this task. This solution only applies to bases that do not require an extra digit. Still I think it's short and interesting.
Note that the minimal candidate is 1.0....0 as a factorial base number.

```fsharp

// Nigel Galloway: May 30th., 2019
let fN n g=let g=n|>Array.rev|>Array.mapi(fun i n->(int64 n)*(pown g i))|>Array.sum
           let n=int64(sqrt (float g)) in g=(n*n)
let fG g=lN([|yield 1; yield! Array.zeroCreate(g-2)|])|>Seq.map(fun n->lN2p n [|0..(g-1)|]) |> Seq.filter(fun n->fN n (int64 g))
printfn "%A" (fG 12|>Seq.head) // -> [|1; 2; 4; 10; 7; 11; 5; 3; 8; 6; 0; 9|]
printfn "%A" (fG 14|>Seq.head) // -> [|1; 0; 2; 6; 9; 11; 8; 12; 5; 7; 13; 3; 10; 4|]

```



## Go

This takes advantage of major optimizations described by Nigel Galloway and Thundergnat (inspired by initial pattern analysis by Hout) in the Discussion page and a minor optimization contributed by myself.

```go
package main

import (
    "fmt"
    "math/big"
    "strconv"
    "time"
)

const maxBase = 27
const minSq36 = "1023456789abcdefghijklmnopqrstuvwxyz"
const minSq36x = "10123456789abcdefghijklmnopqrstuvwxyz"

var bigZero = new(big.Int)
var bigOne = new(big.Int).SetUint64(1)

func containsAll(sq string, base int) bool {
    var found [maxBase]byte
    le := len(sq)
    reps := 0
    for _, r := range sq {
        d := r - 48
        if d > 38 {
            d -= 39
        }
        found[d]++
        if found[d] > 1 {
            reps++
            if le-reps < base {
                return false
            }
        }
    }
    return true
}

func sumDigits(n, base *big.Int) *big.Int {
    q := new(big.Int).Set(n)
    r := new(big.Int)
    sum := new(big.Int).Set(bigZero)
    for q.Cmp(bigZero) == 1 {
        q.QuoRem(q, base, r)
        sum.Add(sum, r)
    }
    return sum
}

func digitalRoot(n *big.Int, base int) int {
    root := new(big.Int)
    b := big.NewInt(int64(base))
    for i := new(big.Int).Set(n); i.Cmp(b) >= 0; i.Set(root) {
        root.Set(sumDigits(i, b))
    }
    return int(root.Int64())
}

func minStart(base int) (string, uint64, int) {
    nn := new(big.Int)
    ms := minSq36[:base]
    nn.SetString(ms, base)
    bdr := digitalRoot(nn, base)
    var drs []int
    var ixs []uint64
    for n := uint64(1); n < uint64(2*base); n++ {
        nn.SetUint64(n * n)
        dr := digitalRoot(nn, base)
        if dr == 0 {
            dr = int(n * n)
        }
        if dr == bdr {
            ixs = append(ixs, n)
        }
        if n < uint64(base) && dr >= bdr {
            drs = append(drs, dr)
        }
    }
    inc := uint64(1)
    if len(ixs) >= 2 && base != 3 {
        inc = ixs[1] - ixs[0]
    }
    if len(drs) == 0 {
        return ms, inc, bdr
    }
    min := drs[0]
    for _, dr := range drs[1:] {
        if dr < min {
            min = dr
        }
    }
    rd := min - bdr
    if rd == 0 {
        return ms, inc, bdr
    }
    if rd == 1 {
        return minSq36x[:base+1], 1, bdr
    }
    ins := string(minSq36[rd])
    return (minSq36[:rd] + ins + minSq36[rd:])[:base+1], inc, bdr
}

func main() {
    start := time.Now()
    var nb, nn big.Int
    for n, k, base := uint64(2), uint64(1), 2; ; n += k {
        if base > 2 && n%uint64(base) == 0 {
            continue
        }
        nb.SetUint64(n)
        sq := nb.Mul(&nb, &nb).Text(base)
        if !containsAll(sq, base) {
            continue
        }
        ns := strconv.FormatUint(n, base)
        tt := time.Since(start).Seconds()
        fmt.Printf("Base %2d:%15s² = %-27s in %8.3fs\n", base, ns, sq, tt)
        if base == maxBase {
            break
        }
        base++
        ms, inc, bdr := minStart(base)
        k = inc
        nn.SetString(ms, base)
        nb.Sqrt(&nn)
        if nb.Uint64() < n+1 {
            nb.SetUint64(n + 1)
        }
        if k != 1 {
            for {
                nn.Mul(&nb, &nb)
                dr := digitalRoot(&nn, base)
                if dr == bdr {
                    n = nb.Uint64() - k
                    break
                }
                nb.Add(&nb, bigOne)
            }
        } else {
            n = nb.Uint64() - k
        }
    }
}
```


{{out}}
Timings (in seconds) are for my Intel Core i7-8565U laptop using Go 1.12.9 on Ubuntu 18.04.

```txt

Base  2:             10² = 100                         in    0.000s
Base  3:             22² = 2101                        in    0.000s
Base  4:             33² = 3201                        in    0.000s
Base  5:            243² = 132304                      in    0.000s
Base  6:            523² = 452013                      in    0.000s
Base  7:           1431² = 2450361                     in    0.000s
Base  8:           3344² = 13675420                    in    0.000s
Base  9:          11642² = 136802574                   in    0.000s
Base 10:          32043² = 1026753849                  in    0.000s
Base 11:         111453² = 1240a536789                 in    0.001s
Base 12:         3966b9² = 124a7b538609                in    0.002s
Base 13:        3828943² = 10254773ca86b9              in    0.004s
Base 14:        3a9db7c² = 10269b8c57d3a4              in    0.005s
Base 15:       1012b857² = 102597bace836d4             in    0.006s
Base 16:       404a9d9b² = 1025648cfea37bd9            in    0.008s
Base 17:      423f82ga9² = 101246a89cgfb357ed          in    0.074s
Base 18:      44b482cad² = 10236b5f8eg4ad9ch7          in    0.084s
Base 19:     1011b55e9a² = 10234dhbg7ci8f6a9e5         in    0.116s
Base 20:     49dgih5d3g² = 1024e7cdi3hb695fja8g        in    3.953s
Base 21:    4c9he5fe27f² = 1023457dg9hi8j6b6kceaf      in    4.174s
Base 22:    4f94788gj0f² = 102369fbgdej48chi7lka5      in   14.084s
Base 23:   1011d3el56mc² = 10234acedkg9hm8fbjil756     in   20.563s
Base 24:   4lj0hdgf0hd3² = 102345b87hfeckjnigmdla69    in   22.169s
Base 25:  1011e145fhghm² = 102345doeckj6gfb8liam7nh9   in   52.082s
Base 26:  52k8n53bdm99k² = 1023458lo6iemkg79fpchnjdba  in  209.808s
Base 27: 1011f11e37objj² = 1023458elomdhbijfgkp7cq9n6a in  401.503s

```



It's possible to go beyond base 27 by using big.Int (rather than uint64) for N as well as N² though this takes about 15% longer to reach base 27 itself.

For example, to reach base 28 (the largest base shown in the OEIS table) we have:

```go
package main

import (
    "fmt"
    "math/big"
    "time"
)

const maxBase = 28

// etc

func main() {
    start := time.Now()
    var n, k, b, t, nn big.Int
    n.SetUint64(2)
    k.SetUint64(1)
    b.SetUint64(2)
    for base := 2; ; n.Add(&n, &k) {
       if base > 2 && t.Rem(&n, &b).Cmp(bigZero) == 0 {
            continue
        }
        sq := nn.Mul(&n, &n).Text(base)
        if !containsAll(sq, base) {
            continue
        }
        ns := n.Text(base)
        tt := time.Since(start).Seconds()
        fmt.Printf("Base %2d:%15s² = %-28s in %8.3fs\n", base, ns, sq, tt)
        if base == maxBase {
            break
        }
        base++
        b.SetUint64(uint64(base))
        ms, inc, bdr := minStart(base)
        k.SetUint64(inc)
        nn.SetString(ms, base)
        n.Sqrt(&nn)
        t.Add(&n, bigOne)
        if n.Cmp(&t) == -1 {
            n.Set(&t)
        }
        if inc != 1 {
            for {
                nn.Mul(&n, &n)
                dr := digitalRoot(&nn, base)
                if dr == bdr {
                    n.Sub(&n, &k)
                    break
                }
                n.Add(&n, bigOne)
            }
        } else {
            n.Sub(&n, &k)
        }
    }
}
```


{{out}}

```txt

Base  2:             10² = 100                          in    0.000s
Base  3:             22² = 2101                         in    0.000s
Base  4:             33² = 3201                         in    0.000s
Base  5:            243² = 132304                       in    0.000s
Base  6:            523² = 452013                       in    0.000s
Base  7:           1431² = 2450361                      in    0.000s
Base  8:           3344² = 13675420                     in    0.000s
Base  9:          11642² = 136802574                    in    0.000s
Base 10:          32043² = 1026753849                   in    0.000s
Base 11:         111453² = 1240a536789                  in    0.001s
Base 12:         3966b9² = 124a7b538609                 in    0.003s
Base 13:        3828943² = 10254773ca86b9               in    0.005s
Base 14:        3a9db7c² = 10269b8c57d3a4               in    0.006s
Base 15:       1012b857² = 102597bace836d4              in    0.007s
Base 16:       404a9d9b² = 1025648cfea37bd9             in    0.010s
Base 17:      423f82ga9² = 101246a89cgfb357ed           in    0.088s
Base 18:      44b482cad² = 10236b5f8eg4ad9ch7           in    0.100s
Base 19:     1011b55e9a² = 10234dhbg7ci8f6a9e5          in    0.138s
Base 20:     49dgih5d3g² = 1024e7cdi3hb695fja8g         in    4.632s
Base 21:    4c9he5fe27f² = 1023457dg9hi8j6b6kceaf       in    4.894s
Base 22:    4f94788gj0f² = 102369fbgdej48chi7lka5       in   16.282s
Base 23:   1011d3el56mc² = 10234acedkg9hm8fbjil756      in   23.697s
Base 24:   4lj0hdgf0hd3² = 102345b87hfeckjnigmdla69     in   25.525s
Base 25:  1011e145fhghm² = 102345doeckj6gfb8liam7nh9    in   59.592s
Base 26:  52k8n53bdm99k² = 1023458lo6iemkg79fpchnjdba   in  239.850s
Base 27: 1011f11e37objj² = 1023458elomdhbijfgkp7cq9n6a  in  461.305s
Base 28: 58a3ckp3n4cqd7² = 1023456cgjbirqedhp98kmoan7fl in  911.059s

```



## JavaScript

{{Trans|Python}}

```javascript
(() => {
    'use strict';

    // allDigitSquare :: Int -> Int
    const allDigitSquare = base => {
        const bools = replicate(base, false);
        return untilSucc(
            allDigitsUsedAtBase(base, bools),
            ceil(sqrt(parseInt(
                '10' + '0123456789abcdef'.slice(2, base),
                base
            )))
        );
    };

    // allDigitsUsedAtBase :: Int -> [Bool] -> Int -> Bool
    const allDigitsUsedAtBase = (base, bools) => n => {
        // Fusion of representing the square of integer N at a given base
        // with checking whether all digits of that base contribute to N^2.
        // Sets the bool at a digit position to True when used.
        // True if all digit positions have been used.
        const ds = bools.slice(0);
        let x = n * n;
        while (x) {
            ds[x % base] = true;
            x = floor(x / base);
        }
        return ds.every(x => x)
    };

    // showBaseSquare :: Int -> String
    const showBaseSquare = b => {
        const q = allDigitSquare(b);
        return justifyRight(2, ' ', str(b)) + ' -> ' +
            justifyRight(8, ' ', showIntAtBase(b, digit, q, '')) +
            ' -> ' + showIntAtBase(b, digit, q * q, '');
    };

    // TEST -----------------------------------------------
    const main = () => {
        // 1-12 only - by 15 the squares are truncated by
        // JS integer limits.

        // Returning values through console.log –
        // in separate events to avoid asynchronous disorder.
        print('Smallest perfect squares using all digits in bases 2-12:\n')
        print('Base      Root    Square')

        print(showBaseSquare(2));
        print(showBaseSquare(3));
        print(showBaseSquare(4));
        print(showBaseSquare(5));
        print(showBaseSquare(6));
        print(showBaseSquare(7));
        print(showBaseSquare(8));
        print(showBaseSquare(9));
        print(showBaseSquare(10));
        print(showBaseSquare(11));
        print(showBaseSquare(12));
    };

    // GENERIC FUNCTIONS ----------------------------------

    const
        ceil = Math.ceil,
        floor = Math.floor,
        sqrt = Math.sqrt;

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // digit :: Int -> Char
    const digit = n =>
        // Digit character for given integer.
        '0123456789abcdef' [n];

    // enumFromTo :: (Int, Int) -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: 1 + n - m
        }, (_, i) => m + i);

    // justifyRight :: Int -> Char -> String -> String
    const justifyRight = (n, cFiller, s) =>
        n > s.length ? (
            s.padStart(n, cFiller)
        ) : s;

    // print :: a -> IO ()
    const print = x => console.log(x)

    // quotRem :: Int -> Int -> (Int, Int)
    const quotRem = (m, n) =>
        Tuple(Math.floor(m / n), m % n);

    // replicate :: Int -> a -> [a]
    const replicate = (n, x) =>
        Array.from({
            length: n
        }, () => x);

    // showIntAtBase :: Int -> (Int -> Char) -> Int -> String -> String
    const showIntAtBase = (base, toChr, n, rs) => {
        const go = ([n, d], r) => {
            const r_ = toChr(d) + r;
            return 0 !== n ? (
                go(Array.from(quotRem(n, base)), r_)
            ) : r_;
        };
        return 1 >= base ? (
            'error: showIntAtBase applied to unsupported base'
        ) : 0 > n ? (
            'error: showIntAtBase applied to negative number'
        ) : go(Array.from(quotRem(n, base)), rs);
    };

    // Abbreviation for quick testing - any 2nd arg interpreted as indent size

    // sj :: a -> String
    function sj() {
        const args = Array.from(arguments);
        return JSON.stringify.apply(
            null,
            1 < args.length && !isNaN(args[0]) ? [
                args[1], null, args[0]
            ] : [args[0], null, 2]
        );
    }

    // str :: a -> String
    const str = x => x.toString();

    // untilSucc :: (Int -> Bool) -> Int -> Int
    const untilSucc = (p, x) => {
        // The first in a chain of successive integers
        // for which p(x) returns true.
        let v = x;
        while (!p(v)) v = 1 + v;
        return v;
    };

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
Smallest perfect squares using all digits in bases 2-12:

Base      Root    Square
 2 ->       10 -> 100
 3 ->       22 -> 2101
 4 ->       33 -> 3201
 5 ->      243 -> 132304
 6 ->      523 -> 452013
 7 ->     1431 -> 2450361
 8 ->     3344 -> 13675420
 9 ->    11642 -> 136802574
10 ->    32043 -> 1026753849
11 ->   111453 -> 1240a536789
12 ->   3966b9 -> 124a7b538609
```



## Julia

Runs in about 4 seconds with using occursin().

```julia
const num = "0123456789abcdef"
hasallin(n, nums, b) = (s = string(n, base=b); all(x -> occursin(x, s), nums))

function squaresearch(base)
    basenumerals = [c for c in num[1:base]]
    highest = parse(Int, "10" * num[3:base], base=base)
    for n in Int(trunc(sqrt(highest))):highest
        if hasallin(n * n, basenumerals, base)
            return n
        end
    end
end

println("Base     Root   N")
for b in 2:16
    n = squaresearch(b)
    println(lpad(b, 3), lpad(string(n, base=b), 10), "  ", string(n * n, base=b))
end

```
{{out}}

```txt

Base     Root   N
  2        10  100
  3        22  2101
  4        33  3201
  5       243  132304
  6       523  452013
  7      1431  2450361
  8      3344  13675420
  9     11642  136802574
 10     32043  1026753849
 11    111453  1240a536789
 12    3966b9  124a7b538609
 13   3828943  10254773ca86b9
 14   3a9db7c  10269b8c57d3a4
 15  1012b857  102597bace836d4
 16  404a9d9b  1025648cfea37bd9

```



## Pascal

Using an array of digits to base n, to get rid of base conversions.<BR>
Starting value equals squareroot of smallest value containing all digits to base.<BR>
Than brute force.<BR>
[https://tio.run/##zVdtU9tGEP6uX7EfMmMJZCMBIYmNmYGAGzcJThOntM0wGb0c9lH5ZKQTlMnkr5fu3p3ebDfTtM1M/AHkvX27Z59dr9LwmkWyuwzyKEi6V8vo4WGZpbMsWAD@pzN/YO3sjLiIQc4Z5IsgSVguQRSLkGUgQKYQBjmD0IU8RZ0Az7YEcBElRcxyQH10EPMZlzmkV0bZ@vRoPDo9G8HozfPPnx69npyewenZqzcvxvjt7Px0PPpsFTnLLYD8Pi8kT/KBFaUilxZE8yB7xyT0Iciy4P6D1@vtHVySbzqBYcfzd/f2Hx88efrs@OQ5BvnhxfjHl69en0/e/PT23fT9zxe//PpbZwBgyfslwwjyvFjI9IQSG0LGojSLAcUrHyHDj/GsHdbv7qvA4b1kg80mkZDu5hNEAtDbBYZbs2UCZbdBhnJMjhzkN9nuCT3ELJEBCsm2znywFgKvYF0VIpI8FfCaC46l@yhswr9/Hkh@y95zIZ0@/T3Ypyp3wPfWkev1eh2TCseQtenAOmEzLlCesbxIEJmhqi6msrMDwe8BdHyvg8fjka76EewSiYQGd5Rm5HBIQs2hrg9xapCoXeqnLVLY5oNWNJnh/WxERtpa6Gx7vWf0cSo8LAUlWEjniMVFxuB5Km53kb82Xop43G@gKEo4VmEqq3FTpBj6/VhjpiBpIAIVJOpmnk53yQKprqWNh9g1Mb/VWCk5xu8Zen3gl0qjS7pbtYogKcn0V2wvmzv0XAhsDjw20UpXSDuy4E0ZEa6skUVFihKGEBTLJXay7lCrURdOdZnz2dyuASrTdLBSpkfWk8dE1kCfFOTEVi28AnqFbU2vcZNdd1zOyUSTo5Q2EvWfdM2VFYPuBObt1QnCXcYlszvQcQZtw41WLSMzaz7UF7xUTuh@m7g1FhHe64TPFLmCOPbb7DJcGStG7ewsM5axm4LnGCvH71QaHJnoSA3bYMFc1e6tvivRcnM3dCMcRvebcQvpipRCWfxBi5fG0ny7m/OEIY2OSgxqnPOWH43CtrLeRgMc3RBqXCuPkyy286NhaPBWDvIu2F2tcXyOJuZsxa/SbDDedMoq6TX8NOHKzLXnw69K/1tkvTlHnICVCXXmITUXTsK2s7pn/4ZXG0mlsutX9V/nVR820EpfezjU9DpUKk1i/StKffel@O@FOI7jZiFc/LPbnmX/qKvJ9ohsXeUGDjUu2ySqxqFu7q@sRrutq1Hn0TAn773m0CNIwi@Xp7L5tuUyAxVKPjVTXaUWTvz/i1m4qGRskd4y2uL4FeRHoV5RwnsIuezKOx7HCRez728kTHENb25z48aWgrMTzCKjaM/da5f09dq86TdW/dopb49Vqmr6trdGJa@WJ72P4qrvrp/RytqQVxsrDfRiQSLTRuWJWz4ofTPsqkPfaVFjdavCPSbFLQjwfSGu1xiA6sLGRHfDtaoMJd/70s9/w7h8nLwF24d8njTMqdjXl05daKOsMwZVZmOk4BiWzupa4@fk7dnxSzOh6D6C/SExSEEjQ79oqQMDmka@hdg6ZrvO5kXxKkj09ldvKhjB1SpmSVNeVTJGoCIqjQviSSJs3vf3ncYGpGk39fowPQ0km/KFCqLW/r6o@EZXLOfN1FN8SO8GJf/Qb0dZrL0tlQ8aE1s4UFcJK6gXO6quDmjeKfyDsp5VrzjNYDYG7049Z@vpwb7n9X2vv6fOy3fTi/H56eTi3eeMBXEiBtWLKd659/DwZ3SVBLP8oTvZ@ws Try it online!]

```pascal
program project1;
//Find the smallest number n to base b, so that n*n includes all
//digits of base b
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}
uses
  sysutils;
const
 charSet : array[0..36] of char ='0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
type
  tNumtoBase = record
                 ntb_dgt : array[0..31-4] of byte;
                 ntb_cnt,
                 ntb_bas  : Word;
               end;
var
  Num,
  sqr2B,
  deltaNum  : tNumtoBase;

function Minimal_n(base:NativeUint):Uint64;
//' 1023456789ABCDEFGHIJ...'
var
  i : NativeUint;
Begin
  result := base;  // aka '10'
  IF base > 2 then
    For i := 2 to base-1 do
      result := result*base+i;
  result := trunc(sqrt(result)+0.99999);
end;

procedure Conv2num(var num:tNumtoBase;n:Uint64;base:NativeUint);
var
  quot :UInt64;
  i :NativeUint;
Begin
  i := 0;
  repeat
    quot := n div base;
    Num.ntb_dgt[i] := n-quot*base;
    n := quot;
    inc(i);
  until n = 0;
  Num.ntb_cnt := i;
  Num.ntb_bas := base;
  //clear upper digits
  For i := i to high(tNumtoBase.ntb_dgt) do
     Num.ntb_dgt[i] := 0;
end;

procedure OutNum(const num:tNumtoBase);
var
  i : NativeInt;
Begin
  with num do
  Begin
    For i := 17-ntb_cnt-1 downto 0 do
      write(' ');
    For i := ntb_cnt-1 downto 0 do
      write(charSet[ntb_dgt[i]]);
  end;
end;

procedure IncNumBig(var add1:tNumtoBase;n:NativeUInt);
//prerequisites
//bases are the same,delta : NativeUint
var
  i,s,b,carry : NativeInt;
Begin
  b := add1.ntb_bas;
  i := 0;
  carry := 0;
  while n > 0 do
  Begin
    s := add1.ntb_dgt[i]+carry+ n MOD b;
    carry := Ord(s>=b);
    s := s- (-carry AND b);
    add1.ntb_dgt[i] := s;
    n := n div b;
    inc(i);
  end;

  while carry <> 0 do
  Begin
    s := add1.ntb_dgt[i]+carry;
    carry := Ord(s>=b);
    s := s- (-carry AND b);
    add1.ntb_dgt[i] := s;
    inc(i);
  end;

  IF add1.ntb_cnt < i then
    add1.ntb_cnt := i;
end;

procedure IncNum(var add1:tNumtoBase;carry:NativeInt);
//prerequisites: bases are the same, carry==delta < base
var
  i,s,b : NativeInt;
Begin
  b := add1.ntb_bas;
  i := 0;
  while carry <> 0 do
  Begin
    s := add1.ntb_dgt[i]+carry;
    carry := Ord(s>=b);
    s := s- (-carry AND b);
    add1.ntb_dgt[i] := s;
    inc(i);
  end;
  IF add1.ntb_cnt < i then
    add1.ntb_cnt := i;
end;

procedure AddNum(var add1,add2:tNumtoBase);
//prerequisites
//bases are the same,add1>add2, add1 <= add1+add2;
var
  i,carry,s,b : NativeInt;
Begin
  b := add1.ntb_bas;
  carry := 0;
  For i := 0 to add2.ntb_cnt-1 do
  begin
    s := add1.ntb_dgt[i]+add2.ntb_dgt[i]+carry;
    carry := Ord(s>=b);
    s := s- (-carry AND b);
    add1.ntb_dgt[i] := s;
  end;

  i := add2.ntb_cnt;
  while carry = 1 do
  Begin
    s := add1.ntb_dgt[i]+carry;
    carry := Ord(s>=b);
    // remove of if s>b then by bit-twiddling
    s := s- (-carry AND b);
    add1.ntb_dgt[i] := s;
    inc(i);
  end;

  IF add1.ntb_cnt < i then
    add1.ntb_cnt := i;
end;

procedure Test(base:NativeInt);
var
  n : Uint64;
  i,j,TestSet : NativeInt;
Begin
  write(base:5);
  n := Minimal_n(base);
  Conv2num(sqr2B,n*n,base);
  Conv2num(Num,n,base);
  deltaNum := num;
  AddNum(deltaNum,deltaNum);
  IncNum(deltaNum,1);

  i := 0;
  repeat
    //count used digits
    TestSet := 0;
    For j := sqr2B.ntb_cnt-1 downto 0 do
      TestSet := TestSet OR (1 shl sqr2B.ntb_dgt[j]);
    inc(TestSet);
    IF (1 shl base)=TestSet  then
       BREAK;
    //next square number
    AddNum(sqr2B,deltaNum);
    IncNum(deltaNum,2);
    inc(i);
  until false;
  IncNumBig(num,i);
  OutNum(Num);
  OutNum(sqr2B);
  Writeln(i:14);
end;

var
  T0: TDateTime;
  base :nativeInt;
begin
  T0 := now;
  writeln('base                 n        square(n)       Testcnt');
  For base := 2 to 16 do
    Test(base);
  writeln((now-T0)*86400:10:3);
  {$IFDEF WINDOWS}readln;{$ENDIF}
end.
```

{{out}}

```txt
base                 n        square(n)       Testcnt
    2               10              100             0
    3               22             2101             4
    4               33             3201             6
    5              243           132304            46
    6              523           452013           103
    7             1431          2450361           209
    8             3344         13675420           288
    9            11642        136802574          1156
   10            32043       1026753849            51
   11           111453      1240A536789         14983
   12           3966B9     124A7B538609         75713
   13          3828943   10254773CA86B9      12668112
   14          3A9DB7C   10269B8C57D3A4         17291
   15         1012B857  102597BACE836D4         59026
   16         404A9D9B 1025648CFEA37BD9        276865
     0.401

```

===Inserted nearly all optimizations found by [[User:Hout|Hout]] and [[User:Nigel Galloway|Nigel Galloway]]===
I use now gmp to calculate the start values.Check Chai Wah Wu list on oeis.org/A260182<BR>
Now multithreaded at bases < 20 .Inserted StartOffset to run later on.For bases> 28 an intermediate stage of the minimal advanced thread is saved every minute.This is the candidate to start with again.<BR>
GetThreadCount needs improvement for Linux<BR>
Now 2..28 finishes on [https://tio.run/##7Txrd9tGrt/9K@Z0e65Jm7Iedm1XitO1LKvRXb9u7DTdm@P6UBJl0aZIhaT8aE/@@s0FMA/ODCkl3pvu3Q/rNraIwWBmMAAGwIBKhnfBKK/N/WzkR7XJfPT58zxNblN/xuAvtjU7a/V6P4zHLJ8GLJv5URRkOYsXs2GQspjlCRv6WcCGHssSwPGhbSNmYTyKFuMgY4APBMbhbZhnLJkIZObf@2zuxwT3o7U/vh/0e8d91r84@rTG2B/fn573jlnv@OTizYADzud5OAt/9/Mwidn5mXd4cvKpXr8Igvk0iQIvDW4f/NQ7ujz2Di9P4Q/vdQRkDk8GP5/hckYHzV3v6Pzs8up0cHaw3fKgxyyM4RNQipJkftD02CRJ2dvn34MY5nQcSTqHFxdXf784Ztj7/ASA0HjWG/Q/rS2yICMUvoB3Z4Nfscson6aBP848ahO4jN3O5l69jkwDfo8WkZ8DT3M/zdmDHy2I0uVz9i4Po6yztpY/zwOA5G@DW1j17g47YI2trd3tDgIvk@gyyAmYBTmyVuFR@6mf3VOrn6b@8wfVeI2ogzgXaHOF95voAgMD/Gwx6z7nwWD8xEd1dndqDbfW7IhGbOAtb8LbqaPhu6w3@IVdhr8HycQ5gw17CN6Fce5ST5gMYBaz4pRoTgIVpqZmkCddEJcDlgajJB0DUP7E@fBmfJvDp7ak2bGacTENzwJegewC17w4796MF7PZM/TXxzWxR3FuEwDx9cpYw7wERa4gQ2CAIfwpSAfxmK@vd5u/TZL88mMK7Jj7o/tgrC90nGY3J2GWtys3sKCJeLCk8wkIRLsQi6IRpty28RG1BDqKcxsKc2wdjsc2@CwIxtl5HJwmadBDFQaEBNTQjxFHrfAomQ3DOOgnKeBekUIwYzNHk/wm@5i2hp56HAdR7p8FT8gXC9rWZKKjmkbJIs6ByyhjctkI5@MNemqDQ7nBResbsEBRUMao12ECAdq8hP3QbNHqi7kIueHbAgoAGLVmbXvDQZk/nzjFLN2aAGlq4Br7R7xibM7n042S0T2pYgXruFYvojxkFRoBP7ObSzQlySQT7NhudWwMzq3qdprLKImzfA1Z0GTZNGINj21tefzz7jZatsOz3g23FO3Cyjhr3/fNn2PPhvRKkG4JsufZhI77NlKvBOmWIHt9m9Bx30bqlSDdEmSvbxMCOhZSrwTpliB7fZsQ0jGReiVItwTZ69uEiI6B1CtBuiXIXt8mxOnoSL0SpFuC7PVtQoKOhtQrQbolyF7fJiTpFEi9EqRbguz1bUKKjkLqlSDdEmSvbxMq6EikXgnSLUH2@jYhjY5A6pUg3RJkr28T0ulwpF4J0i1B9vo2IYMOIfVKkG4Jste3CZl0EKlXgnRLkL2@TciiA0i9EqRbglh0@mvkboymfgqnoWard7fJ/B7GWXgEjeyADOB6o9na3vlhd2//x8PuEfhxP78Z/OffTk7Pzi/@6@3l1btf3v/69//2h6NxMLmdhnf30SxO5h/TLF88PD49/7613tFcwKOLd7s76OyhbWyCrf5@v2H@16HGp26YHzDuzWkuokYGHNMqMnr37abRHXxLRkdEq4mekTwtnNlNMpmAm9je7czEYeq0vH0P/rnEqepO8jyhbnR0tHcI/f3h4Orm9BKwmvt7P3TgtABeB3B2BQ9B@sye2Cxj4YRNwjjMpuDXbGy3AHe3Aa4qHDBj9HPBnQ/6/gjAgli92Wg06oDTAWoOCxkcNLA@5qKbuQZL@8s3/AFyg3i@kH438zNGJ58f5zi5k@QEoo0cRqYj8k2oHvdxdnAss@1WDXYAt4nN/CcNZbuFzs@oH0bBmT8LALKObudN66a1v5U/5etAYb0@Dh7qZ@9OTugphqGDOOOtdPgyiCvYFPz/lAcSYXzL2QABAm7K@SQD3kPfHEjTzmxc5sH8fTjOp7grzcb2/v5OY3tvf3t3v/OtmYceC8RTwXiRBgz8Q3B4HAimmD8eNw0vjTMV4S0d7nYwznr0U3QUoSOPsShO4zKvuUOZ0F6KcJa4RWZncH@yaZLml3kKbMN2GBjCU3A0ux4zHExW6ViiZ47Nbd1Jx4Yr7wr8oStY41UPQrcrEGEE3w4jPpcj3Aj0WvtC8HU/a4LKBeOSbzxZxCMKZX8Ocq2vHY0MweePyURJs/AeQvHkMfskHbe3QQb6ytoHSOloviAyHdHl@IQHsCZiC8RGRwZpCzMWJzl7TNJ7lLRFPIbg/iSMF0/sp59@ktSkiZIOfiED5wuxChRCJ2yPk8UwCjpXU4wp@JJwRZkUUU8I8YSsM0WjZFe4LDB250FgrmIzFZplXjZps0tzc7uKR1mewtj77ZaXuaIDrjfbXGdIbl3AlJ5AW42muKFgHIUGB3XjT6jtd/SM8QB1qDXZOBGMHUwI/7UhtB/urreK6CSfBrHytAXx5egdtRg1rQ3otKnzrLkLrDAXmU3442Ma5kEUO5IH8nmiuNKPFtnU4d0r9xL1mWtuvJgZ2oFanhlbYOzbvAubNu@qYDG0trCQ58cQNgCoSz4WLZzKAfurCPI/NK5lmBIiXMTcG@XQSqI9TsH2ApUP4TUazWKrIHoNRk6oEAd9IPkK91XfoFDbe0YJIYKEQOcxBgkwCArmC0/jAw2q5qvET4ieiPbg/MQ9UTvEnyYeZyiAV@6MsEUFw2iUdTRyMI7YPbR4hhKso/0r2rkxtESko0TF7VTJiDJatwEZxuVy4lr7/nL5kDKwmG3ZcrBKdAorV95AUl9JkGdtWI019d0cxCOHk/C4/ChRKQiLT7NkzByREkIyrrHF5c078qNRcZo4mAfVrb2hRkfTe81tNTM/VoYHUClvszpJFHrsrpzpukC/xMoZFcxUfIuSR4cP4yIHKd8nnyu2gDeh7um7wJckoB@uLX2Yoxujt9Aey3O4apR6naePcz9iIovMUuAtczKQDD8SUJVzdvmG4cda0xVERHoMBx6qpBJDz/V8PHZ4N8M0iEyZ7ABjPLCWaA2iLKhCbJAg0Pyxi@wKItNRa8FsMJqt0ZRhWpMWQvn2jwsfZAe3HZ5E0v0xhNUNwZGeBZUiLskbdk9kE8UGOCHbYCHnib7yej0L/HQ0RScAiWZJtECNXzNWdRSv0rCq4YGj@gwOFH8M7ur7S734JYJD0uGxwm5LVS3m41oJVrkcGBgdmwmcq3SDEbNS7lJbW6kNF6UtWluzWFG5h7Yicz0T6UY0kUtdEp9EW5DJxXK7ycU75KJkANuUm9k9vDw2mVrF2C8yV0dQmgs2pISEu6AU3mrVNkIcqs3iOQ3mgZ@vGYsLYUw2R6NkL6G8AC07TYezOXT37fHh33SQMRc@bX3F4HWFEYz/2tCGwndaDyFAS3O2rjNK0Vxq8@FsfLhZhDeguKbBx0wzW4Qqa42WHw9wM0j6moPz3Ys8K1PGpvLmhg8oD1pXlzt5@FrmfKmLJk9Ey6SqGxmyPjylkU1TtuduOK0fdmtqwdZNDaJzHjlNl5LQ3Lw0dVxxluvzE2ADBhK2AHfvtenvmaJl@n4VYsq9SkACUrb1JEkKRSMeDXZjtdgtQmaMaS4pNBuKe6UDmkuteV3qyb1VeQtn@shss8mWCfASCW6dVYpvhchyIMQHs/nvN6ZLk8/mAtx5qUBjL4in85ssyB2go9zWl0rzmeZMGvIspPnshc7mP1vWTeG0RFMJJjEZpO/mI5ge4hejX0N9WFsWuSRi19Fsrvo1XGbp1ArxWyK2XyeTlj9IE4nAFcF5LImGKBz@BdN3RxBIQwBV6VKDb0Wt5ECBJ4uHS1F4IF2q75oNTABvbW11yUFk360ptwxCpRy1BjqROuA5ELM4eET5m/n5libl2QPeZ3rw14OPPt6cajJ/77HxMIJTy7xxrBR2J3twOzYEibvCndS14iYLHTk03@Z6HZb0HY8QyoGHpAEWUfrYW1/wZnS554vAHTV6i@NY95A4Irk8q9yie@UWVTmP9uGPK58tIpTSgt/64i1c2IYq3PtqnxE@lh36JVNu4ZTFIv/fJqzNR0zlX4WPSqnXDHbaDknByvKszTl//Yy/ar7m@ae0ChUeeskeVhptnewOO6f7FHAIZTrelaUDIkO7u4PVPltjiNVgjZM0CHiVFawt4NlWTPFze8@SOHoWqeKycvM1wGrUWHScbLfUkPAcTnIWBfBLo4CsagVPc9ld/FEdTSZpGHwmjlqaiS94Q/yBR60RBtTYjf@kpTH8CE8k5KWFsxpFor5s7viJUBpVgYvN0sECZCTPShmt46c89UciiQ2HisM/naVagoq5epWJ5XAb2VzZ@brKa6CCFxR5Vf1SzjFZuWCzosXwAuxiFq3RDEvUUo/wou5CHYEygVeZvqMDKoyjMA5WRSNURNZWxWRfyu3xNK6WhjO8sLjkgxFZatOKT3hTqljGuwlXSqgs5YEhqjtgO1U7Qclgb8ctPCpJjEakZO5m4/qabiJ540rU5tejtr4edbsCVVkuipqNiMZwC6sIVo6s58VFMPyqUe13geOIxX7h7ZIrv1hqyeoAFhTcw6rRkZ@mz8udf9IUHGRLeNudNdsVJgplb151Mtx5EomYWFalm9KvvWabgu4mYFOgpzRODneegilE4Rq6ZsIfjjCnxtHQaSyalY@eKe@YRJcHi@pWQvPMi5OJz5xTffXV0/9TJl09QRBFxXIRB7yCfVKC2dUmaiPqAYPeVkQNTkUlnRU2VIrqJVYsr7iftqVPBA3zNEiDj4swg0M/k1wnJ6OQ6SUi/WJZXiWvupz/K0vAny8AXw4by/t/Cg4QRHjo4OD2x@beF/k3w1KBE20XBBQchkbS2MJxkfm9WG6qhzWTsavv2SJcZnCAZ@9CYnezlJlisrQiprhRbYeAAsiEG6mn1tdcR2HxA8aE1Xe9lMwZx9lHGCbKy9W3cVWwjRGzUWNBDmq8AcRNRgJd4qSs3Nb2io8pWSiXyycCvwW8OIcEPttQfYyNEVkd2poWIuE/WtOSzMIFnKF@KupLMiefisIOdR1flDSIZVMNP16aYJZBlP4zn1jqwb/NovBha4sAimit6RbVB4JWMOdZJ4gRimKF4QKjcd5J61Gvi9T0LhV/7WJtL3g78KG5w5q7ZgYOpwN/uIMNH4xaGPnM2rjRV9pGEzjTquj26HpRLyHmRWOyekyeCB6/8TL9wMIM8BkZUiBnR9YGPyiwIVMY62rPJpJqFXLSZ0VpFOhYs9oi9Y1kYrE9RSzaNWJRZHyY636/anpfCgjC61WBuAoEcHjJgY6FIOrcjUwxM9VY7HCpjF4BvLIALcsp1OvYm4oGRI4sjJkuqOogUCesGL6KtNDhZbKnpyX7pQCFurgHjdWXhSo@V5eW4tZv3UhkGGGgnfGxLnH1eVeqivvlZIcuNEJJDmS1Zce6FwayZ4WxKvK@/xbNf4vmNxHN4rj7P5Zv8hpVxs4aFEvO6I6j9FN7zfYbrNHA3/x/@iw6O3pn99fztyYZ6NyQfRoN9Zl3dpZ0dvlFi@jcaBad6bOc9pKfjXttZHZfdAY4dOYH6RwY1vTwd4u15@abQ7gJR@hAe7F375027FDgtOmF1htnUnNBuHS/96BwBQvdRgQRQohj9/jXwZX2GhEn0qogIlFpEkCjmDe/oOJF4A09PkGG8toVyr1gMZFz2uD5OLJGh9pQHcGZFkYyqqGIZIhros2McnS2KRuiJTHq9TE8xOBPAVYazKJnBvqDrkh8G8yCmO5fRCBNM/ht87SxqUhu0si/iQgFwhbCEdphDOzEJDy8st5lKFPCAvMgvQZkHQewUO5OudDBH3b55i1e7d2LYI6Ggw6xOaSsDysSLbLmQFYIVfIUdrSYpB4gFFJhJHwUG5omG6Q1@MoVv2zN5VXzdRfXizYX@NqLyYhtF2JM3i9iM340FWulgs3X6k6x0pEzbhw7a5WX6oeaqq0oJBXhm3NoRauv8WYUWeJISbYwqO7UqKOgjbewinGQP0sakSfwR3Kn2nxfhrN5FLzQiBd5XTCKOPp/kFEjKcOIlITxFbX8ubbvhRbpn2yQigeUfkOjDDNV2CdpsL5kpwojBWqm26mX2KVV6onqG3coMHyJPttmUk8Or7Zvtk37CvNVuXN/qlVbxYBNZNE3tWp/8AQIvhgFi/JH02CsmzJd66vt2aGdDutUNLzErimPcIN9Ms2KngTBm5S3i/hqOnbmfurPgjxI22yeAJ0gNRJmofny9Ry6dKP7tvFSs1XczVEo7WkEI5oRUIO6eiWO6PlbZS5tVXmM/jZaOdFWqhjEHaMYg@pveZhhhQzC5hbvsJfeXy9FGVoX8zUkBVlaoGNFWrXQrNwpXerJWUFwohVkKBYcrCqCG@AWnyT4jQQwiZSslyO7LptRuLSqjB3HY77LTmNJxg2FzajfsSvii9SbIWtY1@69ywJBPzPfK1py/XhFhVlx8shpVNYRuVobJ8ewk7wR1/JiWkpKZbFkyrL8qpkpGXqIaiJLk2K8rNRUeXaHyoNfsx2XTmrMFDqrK3nc6lIe8d0ukzAtvtSF16GPfHrPg5ehm2XgpIlwXKZ5VhTnF@6SqlHWXxMB2u41ah62Ar1VSQedLRi2K24YkW/F@3x2gfgS5raqMwsGrzmH8b0OSqcbzOXV4JpaFT03qjBx2Ro9w/JUbbCak5FCQU4Y6@B5Ez2nYVwZ8RJJqfCFabzTPmuqg1DzRUYlbZSfesVahsmwuoqpW6lzXTVLEy1eTFsn5qzzKh6PrTN6HYS/SwzQInu@rpLrB9CgUbfepBJFOqy9blfvfNuRSuNoORylq1VJo6H76nVjiU6KoOMOWK6PS5ru6BbcXVYnVa5Iuftils8qItGPRsQWBvyvhVvgsQvuDDh3rmcUqHjWSwI7GzuNH3eXpfPwnLsrp6tKEnan3qNZedCDXzXkxrG1b6pa11q1eqV2cPZJA7/3w7yfpHzkqwC/IgpPBYOdDf66ps4zT7zG7hoj6G/gkrpHQTB3qlHl67VfTP39ozOs6XY0tNwL4JszIBnbbrq2fyBEW3/Hd0O8wO9V6rh@EFtFxoYbpvb0H10T0xZlSUIv0AXr5VqxZEorp0BWO0oyYQFLyAZiKL6Iy1GOlMsqXFPTit9VqhFn7R0zi7@K86BU32a5c3QohBvaXm5KjE69jkpPRwL/sjK62kxSMGcxL6G27mPxpLor5fGvmobbpUy1rHpbNy2rDbay4@/V4eEA4RrLwRpusP3dnUajzX5ss2007hk5lvIbota9uzZr7sDU1H1gu7nvmvS@LTWNnxurKK9rfjF3eUvvW6xJ8ZS01TdMUN/DLAtvYwQBS8wmsHM1blcO55iYEG/1AnjzE4/RB4ng9iurlOyRb5Jb/pYD/oUDlrdQXHQczRfsSHHK6kvksiCPgvg2nxo67rEyLn7xgiY6WDIsb8zk93TQa6n8o1BoFVHQYFc8MZA81q7cDWK@Md1RgnFZDgqGXxPgsas2a8it4V9YwjdI81isLst6kC0gBq5YcsPVvz8GjqPe@fvLT5Rf8sdRbHzDC@zf1tra58//M5pE/m32uXa@/bl2@fC/ Try it online!]

The runtime is on my 6-Core PC AMD  Ryzen 5 1600 ( 3.4 Ghz on all cores Linux64 with SMT= on)<BR>

```pascal
program project1;
//Find the smallest number n to base b, so that n*n includes all
//digits of base b aka pandigital
{$IFDEF FPC}
  {$MODE DELPHI}
  {$Optimization ON,ALL}//Peephole,regvar,CSE,ASMCSE}
  {$CODEALIGN proc=16,CONSTMIN=32,varmin=32}//loop=1, for Ryzen
{$ElSE}
  {$APPTYPE CONSOLE}
{$ENDIF}
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  gmp,// to calculate start values
  SysUtils;

type
  tRegion64 = 0..63;
  tSolSet64 = set of tRegion64;
  tMask64 = array[tRegion64] of Int64;
  tpMask64 = ^tMask64;

  tNumByteIdx = 0..(64-0)-1;
  tNumIdx  = 0..High(tNumByteIdx) DIV Sizeof(NativeUint);
  tarrNum = array[tNumIdx] of NativeUInt;

  tNumtoBase= record
          ntb_dgt   : tarrNum;
          ntb_Mask0,
          ntb_TestSet,ntB_dummy : NativeUInt;
          ntb_cnt,
          ntb_bas,
          ntb_cntbts,
          ntb_HighByte : byte;
        end;

  tDgtRootSqr = packed record
    drs_List: array[tRegion64] of byte;
    drs_SetOfSol: tSolSet64;
    drs_bas: byte;
    drs_Sol: byte;
    drs_SolCnt: byte;
    drs_Dgt2Add: byte;
    drs_NeedsOneMoreDigit: boolean;
  end;

  tCombineForOneThread = record
    cft_sqr2b,
    cft_deltaNextSqr,
    cft_delta: tNumtoBase;
    cft_count : Uint64;
    cft_ThreadID: NativeUint;
    cft_ThreadHandle: NativeUint;
    //extend to 512 byte
    cft_dummy : array[0..512-1-3*(SizeOf(tNumtoBase)-SizeOf(NativeUint))] of byte;
  end;
  pThreadBlock = ^tCombineForOneThread;
  tMulti = record
             m_Startofs : Uint32;
             m_count  : Uint32;
           end;
const
  //1 shl 0, ..,1 shl 63
  cAND_Mask64: tMask64 =(
$FFFFFFFFFFFFFFFE,$FFFFFFFFFFFFFFFD,$FFFFFFFFFFFFFFFB,$FFFFFFFFFFFFFFF7,
$FFFFFFFFFFFFFFEF,$FFFFFFFFFFFFFFDF,$FFFFFFFFFFFFFFBF,$FFFFFFFFFFFFFF7F,
$FFFFFFFFFFFFFEFF,$FFFFFFFFFFFFFDFF,$FFFFFFFFFFFFFBFF,$FFFFFFFFFFFFF7FF,
$FFFFFFFFFFFFEFFF,$FFFFFFFFFFFFDFFF,$FFFFFFFFFFFFBFFF,$FFFFFFFFFFFF7FFF,
$FFFFFFFFFFFEFFFF,$FFFFFFFFFFFDFFFF,$FFFFFFFFFFFBFFFF,$FFFFFFFFFFF7FFFF,
$FFFFFFFFFFEFFFFF,$FFFFFFFFFFDFFFFF,$FFFFFFFFFFBFFFFF,$FFFFFFFFFF7FFFFF,
$FFFFFFFFFEFFFFFF,$FFFFFFFFFDFFFFFF,$FFFFFFFFFBFFFFFF,$FFFFFFFFF7FFFFFF,
$FFFFFFFFEFFFFFFF,$FFFFFFFFDFFFFFFF,$FFFFFFFFBFFFFFFF,$FFFFFFFF7FFFFFFF,
$FFFFFFFEFFFFFFFF,$FFFFFFFDFFFFFFFF,$FFFFFFFBFFFFFFFF,$FFFFFFF7FFFFFFFF,
$FFFFFFEFFFFFFFFF,$FFFFFFDFFFFFFFFF,$FFFFFFBFFFFFFFFF,$FFFFFF7FFFFFFFFF,
$FFFFFEFFFFFFFFFF,$FFFFFDFFFFFFFFFF,$FFFFFBFFFFFFFFFF,$FFFFF7FFFFFFFFFF,
$FFFFEFFFFFFFFFFF,$FFFFDFFFFFFFFFFF,$FFFFBFFFFFFFFFFF,$FFFF7FFFFFFFFFFF,
$FFFEFFFFFFFFFFFF,$FFFDFFFFFFFFFFFF,$FFFBFFFFFFFFFFFF,$FFF7FFFFFFFFFFFF,
$FFEFFFFFFFFFFFFF,$FFDFFFFFFFFFFFFF,$FFBFFFFFFFFFFFFF,$FF7FFFFFFFFFFFFF,
$FEFFFFFFFFFFFFFF,$FDFFFFFFFFFFFFFF,$FBFFFFFFFFFFFFFF,$F7FFFFFFFFFFFFFF,
$EFFFFFFFFFFFFFFF,$DFFFFFFFFFFFFFFF,$BFFFFFFFFFFFFFFF,$7FFFFFFFFFFFFFFF
);
  charSet: array[0..63] of AnsiChar =
    '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.';
  {$IFDEF CPU64}
  Mask1 = $8080808080808080;
  MaxBit= 63;
  {$ENDIF}
  {$IFDEF CPU32}
  Mask1 = $80808080;
  MaxBit= 31;
  {$ENDIF}
//  Multi21 : tMulti = (m_offset:6;m_delta:(2,8,2,8));
  Multi21 : tMulti = (m_Startofs:6;m_count:4);
  WAIT_MS = 1875; // check every x ms if finished *32 = 60 seconds
  timeFac = WAIT_MS/1000/60 ;// ( i AND 31 ) = 0

//############################################################################
//Input values as constants
  LoLimit = 2;
  HiLimit = 28;// For 32-Bit CPU max HiLimit = 32

  cFileName = 'Test_2_28.txt';//'/dev/NULL';//'nonsens.txt';
  // use hyperthreading = 0

  StartOfs= 1;//testcount*StepWidth = 11038840378368;
//############################################################################

  procedure AddNum(var add1: tNumtoBase; const add2: tNumtoBase); forward;

var
  {$ALIGN 32}
  ThreadBlocks: array of tCombineForOneThread;
  {$ALIGN 32}
  s :shortString;
  Num, sqr2B, deltaNextSqr, delta: tNumtoBase;
  DgtRtSqr: tDgtRootSqr;
  T,T0, T1: TDateTime;
  gblThreadCount,
  Finished: Uint32;
  f : text;

  function GetThreadCount: NativeUInt;
  begin
    {$IFDEF Windows}
       Result := GetCpuCount;
    {$ELSE}
      Result := 12;//GetCpuCount;// is not working under Linux ???
    {$ENDIF}
  end;

  procedure OutThreadStep(i:double;ThCnt:NativeInt;stepWidth,StartOffSet: Int64);
  var
    j,min : NativeInt;
    s,sf: ShortString;
  Begin
    str(i:8:2,s);
    s := s+' min ';
    stepWidth := -ThCnt*stepWidth;
    min := 0;
    For j := 0 to ThCnt-1 do
      If min > ThreadBlocks[j].cft_count then
        min :=  ThreadBlocks[j].cft_count;
    str(stepWidth*min+StartOffSet:16,sf);
    s := s+sf;
    writeln(s);
    writeln(f,s);
    Flush(f);
  end;

  procedure OutNum(const num: tNumtoBase;var s: ShortString);
  var
    pB : pByte;
    i: NativeInt;
  begin
    with num do
    begin
      pB := @ntb_dgt[0];
      i := ntb_cnt*SizeOf(NativeUint);
      while pB[i] = 0 do
        dec(i);
      IF i < 0 then
        i := 0;
      for i := i downto 0 do
        s := s+charSet[pB[i]];
      s := s+' ';
    end;
    Write(s);
    Write(f,String(s));
  end;

  procedure OutNumSqr;
  begin
    s := ' Num ';OutNum(Num,s);
    s := ' sqr ';OutNum(sqr2B,s);
    writeln;writeln(f);Flush(f);
  end;

  function getDgtRtNum(const num: tNumtoBase): NativeInt;
  var
    pB : pByte;
    i: NativeInt;
  begin
    pB := @num.ntb_dgt[0];
    with num do
    begin
      Result := 0;
      for i := 0 to num.ntb_cntbts - 1 do
        Inc(Result, pB[i]);
      Result := Result mod (ntb_bas - 1);
    end;
  end;

  procedure CalcDgtRootSqr(base: NativeUInt);
  var
    ChkSet: array[tRegion64] of tSolSet64;
    ChkCnt: array[tRegion64] of byte;
    i, j: NativeUInt;
    PTest: tSolSet64;
  begin
    for i := low(ChkCnt) to High(ChkCnt) do
    begin
      ChkCnt[i] := 0;
      ChkSet[i] := [];
    end;
    ptest := [];
    with DgtRtSqr do
    begin
      //pandigtal digital root (sum all digits of base) mod (base-1)
      drs_bas := base;
      if Odd(base) then
        drs_Sol := base div 2
      else
        drs_Sol := 0;

      base := base - 1;
      //calc which dgt root the square of the number will become
      for i := 0 to base - 1 do
        drs_List[i] := (i * i) mod base;
      //searching for solution
      drs_SolCnt := 0;
      for i := 0 to base - 1 do
        if drs_List[i] = drs_Sol then
        begin
          include(ptest, i);
          Inc(drs_SolCnt);
        end;
      //if not found then NeedsOneMoreDigit
      drs_NeedsOneMoreDigit := drs_SolCnt = 0;
      if drs_NeedsOneMoreDigit then
      begin
        for j := 1 to Base do
          for i := 0 to Base do
            if drs_List[j] = (drs_Sol + i) mod BASE then
            begin
              include(ptest, i);
              include(ChkSet[i], j);
              Inc(ChkCnt[i]);
            end;
        i := 1;
        repeat
          if i in pTest then
          begin
            drs_Dgt2Add := i;
            BREAK;
          end;
          Inc(i);
        until i > base;
        writeln('insert ', i);
      end;
    end;
  end;

  procedure conv_ui_num(base: NativeUint; ui: Uint64; var Num: tNumtoBase);
  var
    pB : pByte;
    i: NativeUInt;
  begin
    with num do
    begin
      for i := 0 to high(tNumtoBase.ntb_dgt) do
        ntb_dgt[i] := 0;
      pB := @ntb_dgt[0];
      ntb_bas := base;
      ntb_Mask0 := (Mask1 shr 7)*(256-Base);
      ntb_TestSet := Uint64(1) shl base -1;
      ntb_cntbts := 0;
      ntb_cnt := 0;
      if ui <> 0 then
      begin
        i := 0;
        repeat
          pB[i] := ui mod base;
          ui := ui div base;
          Inc(i);
        until ui = 0;
        ntb_cntbts := i;
        ntb_HighByte := pB[i-1];
        ntb_cnt := i DIV SizeOf(NativeUint) +1 ;
      end;
    end;
   end;

  procedure conv2Num(base: NativeUint; var Num: tNumtoBase; var s: mpz_t);
  var
    tmp: mpz_t;
    pB : pByte;
    i: NativeUInt;
  begin
    mpz_init_set(tmp,s);
    for i := 0 to high(tNumtoBase.ntb_dgt) do
      Num.ntb_dgt[i] := 0;
    pB := @Num.ntb_dgt[0];
    with num do
    begin
      ntb_bas := base;
      ntb_Mask0 := (Mask1 shr 7)*(256-Base);
      ntb_TestSet := Uint64(1) shl base -1;
      i := 0;
      repeat
        pB[i] := mpz_tdiv_q_ui(tmp, tmp, base);
        Inc(i);
      until mpz_cmp_ui(tmp, 0) = 0;
      ntb_HighByte := pB[i-1];
      ntb_cntbts := i;
      ntb_cnt := i DIV SizeOf(NativeUint) +1;
    end;
    mpz_clear(tmp);
  end;

  procedure StartValueCreate(base: NativeUInt);
  //create the lowest pandigital number "102345...Base-1 "
  //calc sqrt +1 and convert n new format.
  var
    sv_sqr, sv,sv_add: mpz_t;
    k, dblDgt: NativeUint;

  begin
    mpz_init(sv);
    mpz_init(sv_sqr);

    mpz_init_set_si(sv_sqr, base);//"10"
    CalcDgtRootSqr(base);

    if DgtRtSqr.drs_NeedsOneMoreDigit then
    begin
      dblDgt := DgtRtSqr.drs_Dgt2Add;
      if dblDgt = 1 then
      begin
        for k := 1 to base - 1 do
        begin
          mpz_mul_ui(sv_sqr, sv_sqr, base);
          mpz_add_ui(sv_sqr, sv_sqr, k);
        end;
      end
      else
      begin
        for k := 2 to dblDgt do
        begin
          mpz_mul_ui(sv_sqr, sv_sqr, base);
          mpz_add_ui(sv_sqr, sv_sqr, k);
        end;
        for k := dblDgt to base - 1 do
        begin
          mpz_mul_ui(sv_sqr, sv_sqr, base);
          mpz_add_ui(sv_sqr, sv_sqr, k);
        end;
      end;
    end
    else
    begin
      for k := 2 to base - 1 do
      begin
        mpz_mul_ui(sv_sqr, sv_sqr, base);
        mpz_add_ui(sv_sqr, sv_sqr, k);
      end;
    end;

    mpz_sqrt(sv, sv_sqr);
    writeln('Start Offset ',StartOfs);
    //Windows64 gmp.dll or freepascal does not use Uint64 only Uint32
    mpz_init_set_ui(sv_add,StartOfs shr 32);
    // shift left 32
    mpz_mul_2exp(sv_add,sv_add,32);
    mpz_add_ui(sv_add,sv_add,Uint32(StartOfs));
    mpz_add(sv, sv, sv_add);
    mpz_mul(sv_sqr, sv, sv);

    conv2Num(base, Num, sv);
    conv2Num(base, sqr2B, sv_sqr);

    mpz_clear(sv_add);
    mpz_clear(sv_sqr);
    mpz_clear(sv);
    OutNumSqr;
  end;

  function ExtractThreadVal(ThreadNr: NativeInt ): Uint64;
  begin
    with ThreadBlocks[ThreadNr] do
    begin
      sqr2b := cft_sqr2b;
      Result := cft_count;
      cft_ThreadID := 0;
      cft_ThreadHandle := 0;
    end;
  end;

 function CheckPandigital(const n: tNumtoBase): NativeUint;inline;
  var
    pB : pByte;
    pMask: tpMask64;
    i: NativeInt;
  begin
    i := n.ntb_cntbts;
    pB := @n.ntb_dgt[0];
    pMask := @cAND_Mask64;
    result := n.ntb_TestSet;
    while i >= 4 do
    begin
      dec(i,4);
      result := pMask[pB[i+0]] AND result;
      result := pMask[pB[i+1]] AND result;
      result := pMask[pB[i+2]] AND result;
      result := pMask[pB[i+3]] AND result;
    end;
    if i > 0 then
    repeat
      result := pMask[pB[i]] AND result;
      dec(i);
    until i<0;
  end;

  procedure IncNumBig(var add1: tNumtoBase; n: Uint64);
  var
    pB : pByte;
    i, s, b, carry: NativeUInt;
  begin
    b := add1.ntb_bas;
    i := 0;
    carry := 0;
    pB := @add1.ntb_dgt[0];
    while n > 0 do
    begin
      s := pB[i] + carry + n mod b;
      carry := Ord(s >= b);
      s := s - (-carry and b);
      pB[i] := s;
      n := n div b;
      Inc(i);
    end;

    while carry <> 0 do
    begin
      s := pB[i] + carry;
      carry := Ord(s >= b);
      s := s - (-carry and b);
      pB[i] := s;
      Inc(i);
    end;

    if add1.ntb_cntbts < i then
    Begin
      add1.ntb_cntbts := i;
      add1.ntb_cnt := i DIV (SizeOf(NativeUint))+1;
    end;
  end;

  procedure IncSmallNum(var add1: tNumtoBase; carry: NativeUInt);
  //prerequisites carry < base
  var
   pB : pByte;
    i, s, b: NativeUInt;
  begin
    b := add1.ntb_bas;
    pB := @add1.ntb_dgt[0];
    i := 0;
    while carry <> 0 do
    begin
      s := pB[i] + carry;
      carry := Ord(s >= b);
      s := s - (-carry and b);
      pB[i] := s;
      Inc(i);
    end;
    if add1.ntb_cntbts < i then
    Begin
      add1.ntb_cntbts := i;
      add1.ntb_cnt := i DIV SizeOf(NativeUint) +1;
    end;
  end;

  procedure Mul_num_ui(var n: tNumtoBase; ui: Uint64);
  var
    dbl: tNumtoBase;
  begin
    dbl := n;
    conv_ui_num(n.ntb_bas, 0, n);
    while ui > 0 do
    begin
      if Ui and 1 <> 0 then
        AddNum(n, dbl);
      AddNum(dbl, dbl);
      ui := ui div 2;
    end;
  end;

  procedure CalcDeltaSqr(const num: tNumtoBase; var dnsq, dlt: tNumtoBase;
    n: NativeUInt);
  //calc  deltaNextSqr   //n*num
  begin
    dnsq := num;
    Mul_num_ui(dnsq, n);
    AddNum(dnsq, dnsq);
    IncNumBig(dnsq, n * n);
    conv_ui_num(num.ntb_bas, 2 * n * n, dlt);
  end;

  procedure PrepareThreads(thdCount,stepWidth:NativeInt);
  //starting the threads at num,num+stepWidth,..,num+(thdCount-1)*stepWidth
  //stepwith not stepWidth but thdCount*stepWidth
//insert 6
//  6  :  4  6 14 16
  var
    tmpnum,tmpsqr2B,tmpdeltaNextSqr,tmpdelta :tNumToBase;
    deltas: array[0..7] of Uint32;
    Multi : tMulti;

    i,base : NativeInt;
  Begin
    tmpnum := num;
    tmpsqr2B := sqr2B;
    tmpdeltaNextSqr := deltaNextSqr;
    tmpdelta := delta;
    IF StepWidth <> 1 then
    Begin
      For i := 0 to thdCount-1 do
      Begin
        //init ThreadBlock
        With ThreadBlocks[i] do
        begin
          cft_sqr2b := tmpsqr2B;
          cft_count := 0;
          CalcDeltaSqr(tmpnum,cft_deltaNextSqr,cft_delta,thdCount*stepWidth);
        end;
        //Next sqr number in stepWidth
        IncSmallNum(tmpnum,stepWidth);
        AddNum(tmpsqr2B,tmpdeltaNextSqr);
        IF CheckPandigital(sqr2B)=0 then
        begin
          writeln(' solution found ');
          OutNumSqr;
        end
        else
          AddNum(tmpdeltaNextSqr,tmpdelta);
      end;
    end
    else
    Begin
      Multi := Multi21;
      base := tmpNum.ntb_bas-1;
      For i := 0 to thdCount-1 do
      Begin
        //init ThreadBlock
        With ThreadBlocks[i] do
        begin
          cft_sqr2b := tmpsqr2B;
          cft_count := 0;
          CalcDeltaSqr(tmpnum,cft_deltaNextSqr,cft_delta,thdCount*stepWidth);
        end;
        //Next sqr number in stepWidth
        IncSmallNum(tmpnum,stepWidth);
        AddNum(tmpsqr2B,tmpdeltaNextSqr);
        IF CheckPandigital(sqr2B)=0 then
        begin
          writeln(' solution found ');
          OutNumSqr;
        end
        else
          AddNum(tmpdeltaNextSqr,tmpdelta);
      end;
    end;
  end;

procedure AddNum(var add1: tNumtoBase; const add2: tNumtoBase);
  //   N0 AND mask1                  -> 80 00 80 80 80 00 80
  //  (N0 AND mask1)XOR mask1        -> 00 80 00 00 00 80 00
  // ((N0 AND mask1)XOR mask1) shr 7 -> 00 01 00 00 00 01 00
  //                          *k     -> 00  k 00 00 00 k  00
var
  pAdd1,pAdd2 :pNativeUint;
  NextCarry,n,k,M0: NativeUInt;
  M1,i : NativeUInt;
Begin
  IF add1.ntb_cnt= 0 then
  Begin
    add1 := add2;
    EXIT;
  end;
  IF add2.ntb_cnt= 0 then
    EXIT;

  M1 :=NativeUint(Mask1);
  M0:= add1.ntb_mask0;

  k := byte(M0);

  i := Add2.ntb_cnt;
  pAdd2:= @Add2.ntb_dgt[0];
  pAdd1:= @Add1.ntb_dgt[0];

  NextCarry := 0;
  repeat
    //depends extremly on arrangement.
    n := pAdd2^+M0+NextCarry+pAdd1^;
    inc(pAdd2);
    NextCarry := (n shr MaxBit) XOR 1;
    n := n-M0+(((n AND M1)XOR M1) SHR 7)*k;
    pAdd1^ :=n;
    inc(pAdd1);
    dec(i);
  until i = 0;

  i := Add2.ntb_cnt;
  IF NextCarry <> 0 then
  Begin
    repeat
      n := pAdd1^+M0+NextCarry;
      NextCarry := (n shr MaxBit) XOR 1;
      n := n-M0+(((n AND M1)XOR M1) SHR 7)*k;
      pAdd1^ :=n;
      inc(i);
      inc(pAdd1);
    until NextCarry = 0;
  end;
  with Add1  do
  Begin
    IF i > ntb_cnt then
    Begin
       ntb_cnt := i;
       ntb_cntbts := Add1.ntb_cnt*SizeOf(NativeUint);
       while (Add1.ntb_cntbts > 0) AND (ntb_dgt[Add1.ntb_cntbts] = 0) do
         dec(Add1.ntb_cntbts);
       inc(Add1.ntb_cntbts);
     end;
  end;
end;

procedure AddNumSimple(var add1: tNumtoBase; const add2: tNumtoBase);inline;
  // Add1&Add2 <> 0 and  Add2< Add1
var
  pAdd1,pAdd2 :pNativeUint;
  NextCarry,n,k,M0: NativeUInt;
  M1,i : NativeUInt;
Begin
  M1 :=NativeUint(Mask1);
  M0:= add1.ntb_mask0;
  k := byte(M0);

  i := Add2.ntb_cnt;
  pAdd2:= @Add2.ntb_dgt[0];
  pAdd1:= @Add1.ntb_dgt[0];

  NextCarry := NextCarry XOR NextCarry;
  repeat
    // extremly depends on arrangement.
    n := pAdd2^+NextCarry+M0+pAdd1^;
    NextCarry := (n shr MaxBit) XOR 1;
    n := (((n AND M1)XOR M1) SHR 7)*k-M0+n;
//  n := n-M0+(((n AND M1)XOR M1) SHR 7)*k;
    inc(pAdd2);
    dec(i);
    pAdd1^ :=n;
    inc(pAdd1);
  until i = 0;

  IF NextCarry <> 0 then
  Begin
    i := Add2.ntb_cnt;
    repeat
      n := pAdd1^+M0+NextCarry;
      NextCarry := (n shr MaxBit) XOR 1;
      n := (((n AND M1)XOR M1) SHR 7)*k+n-M0;
      pAdd1^ :=n;
      inc(i);
      inc(pAdd1);
    until NextCarry = 0;
{   //never reached
    IF i > Add1.ntb_cnt then
    Begin
      Add1.ntb_cnt := i;
      Add1.ntb_cntbts := Add1.ntb_cnt*SizeOf(NativeUint);
    end;
    * }
  end;
end;

  procedure TestRunThd(parameter: pointer);
  var
    i: Uint64;
    pThdBlk: pThreadBlock;
  begin
    pThdBlk := @ThreadBlocks[NativeUint(parameter)];
    with pThdBlk^ do
    begin
      i := 0;
      repeat
        if finished <> 0 then
          BREAK;
        //next square number
        AddNumSimple(cft_sqr2b, cft_deltaNextSqr);
        AddNumSimple(cft_deltaNextSqr, cft_delta);
        Inc(i);
        cft_count := -i;
      until CheckPandigital(cft_sqr2b)=0;
      if finished = 0 then
      begin
        InterLockedIncrement(finished);
        cft_count := i;
      end;
    end;
    EndThread(0);
  end;

  procedure Test(base: NativeInt);
  var
    stepWidth: Uint64;
    i, j,UsedThreads,StartOffSet: NativeInt;
  begin
    T0 := now;
    StartValueCreate(base);
    StartOffSet  := StartOfs;
    deltaNextSqr := num;
    AddNum(deltaNextSqr, deltaNextSqr);
    IncSmallNum(deltaNextSqr, 1);
    stepWidth := 1;
    if (Base > 4) and not (DgtRtSqr.drs_NeedsOneMoreDigit) then
    begin
      //Find first number which can get the solution
      with dgtrtsqr do
        while drs_List[getDgtRtNum(num)] <> drs_sol do
        begin
          IncSmallNum(num, 1);
          AddNum(sqr2B, deltaNextSqr);
          IncSmallNum(deltaNextSqr, 2);
        end;
      stepWidth := (Base - 1) div DgtRtSqr.drs_SolCnt;
      if stepWidth * DgtRtSqr.drs_SolCnt <> (Base - 1) then
        stepWidth := 1;
    end;
    CalcDeltaSqr(num,deltaNextSqr,delta,stepWidth);

    i := 0;

    finished := 0;
    j := 0;
    UsedThreads := gblThreadCount;
    if base < 20 then
      UsedThreads := 1;
    PrepareThreads(UsedThreads,stepWidth);

    writeln(f,'Base ', base, ' test every ', stepWidth,' threads = ',UsedThreads);
    Write(f,'Start  :');
    writeln('Base ', base, ' test every ', stepWidth,' threads = ',UsedThreads);
    Write('Start  :');
    OutNumSqr;

    if (CheckPandigital(sqr2b)<>0) then
    begin
      while (j < UsedThreads) and (finished = 0) do
      begin
        with ThreadBlocks[j] do
        begin
          cft_ThreadHandle :=
          BeginThread(@TestRunThd, Pointer(j), cft_ThreadID,
              4*4096);
        end;
        Inc(j);
      end;
      UsedThreads := j;

      i := 0;
      repeat
        IF base > 28 then
        Begin
          {$IFDEF WIN}
          WaitForThreadTerminate(ThreadBlocks[0].cft_ThreadHandle,WAIT_MS)
          {$ELSE}
          sleep(WAIT_MS)
          {$ENDIF}
        end
        else
          WaitForThreadTerminate(ThreadBlocks[0].cft_ThreadHandle,-1);
        inc(i);
        IF (I and 31) = 0 then
          OutThreadStep(i*timeFac,UsedThreads,stepWidth,StartOffSet);
      until finished <> 0;

      WaitForThreadTerminate(ThreadBlocks[0].cft_ThreadHandle, -1);
      repeat
        Dec(j);
        with ThreadBlocks[j] do
        begin
          WaitForThreadTerminate(cft_ThreadHandle, -1);
          CloseThread(cft_ThreadHandle);
          if Int64(cft_count) > 0 then
            finished := j;
        end;
      until j = 0;
    end;

    i := ExtractThreadVal(finished);
    j := i*UsedThreads+finished;//TestCount starts at original num
    IncNumBig(num,j*stepWidth);
    T1 := now;
    Write(f,'Result :');
    Write('Result :');
    OutNumSqr;
    Writeln(f,(T1 - t0) * 86400: 9: 3, ' s Testcount : ',j: 14,j*StepWidth:18);
    Writeln((T1 - t0) * 86400: 9: 3, ' s Testcount : ',j: 14,j*StepWidth:18);
    Writeln(UsedThreads*(T1 - t0) * 86400: 9: 3, ' s');
  end;

var
  base: NativeUint;
begin
  Writeln(cFileName);
  AssignFile(f,cFileName);
  {$I-}
    Append(f);
  {$I+}
  IF IoResult <> 0 then
    rewrite(f);
  gblThreadCount:= GetThreadCount;
  writeln(' Cpu Count : ', gblThreadCount);
  setlength(ThreadBlocks,  gblThreadCount);

  T := now;
  for base := LoLimit to HiLimit do
    Test(base);
  T := (now-T)*86400;
  writeln('completed in ', T: 0: 3, ' seconds');
  writeln(f,'completed in ',T: 0: 3, ' seconds');
  Close(f);

  setlength(ThreadBlocks, 0);
  {$IFDEF WINDOWS}
  readln;
  {$ENDIF}
end.
//{{out}}
program project1;
//Find the smallest number n to base b, so that n*n includes all
//digits of base b aka pandigital
{$IFDEF FPC}
  {$MODE DELPHI}
  {$Optimization ON,ALL}//Peephole,regvar,CSE,ASMCSE}
  {$CODEALIGN proc=16,CONSTMIN=32,varmin=32}//loop=1, for Ryzen
{$ElSE}
  {$APPTYPE CONSOLE}
{$ENDIF}
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  gmp,// to calculate start values
  SysUtils;

type
  tRegion64 = 0..63;
  tSolSet64 = set of tRegion64;
  tMask64 = array[tRegion64] of Int64;
  tpMask64 = ^tMask64;

  tNumByteIdx = 0..(64-0)-1;
  tNumIdx  = 0..High(tNumByteIdx) DIV Sizeof(NativeUint);
  tarrNum = array[tNumIdx] of NativeUInt;

  tNumtoBase= record
          ntb_dgt   : tarrNum;
          ntb_Mask0,
          ntb_TestSet,ntB_dummy : NativeUInt;
          ntb_cnt,
          ntb_bas,
          ntb_cntbts,
          ntb_HighByte : byte;
        end;

  tDgtRootSqr = packed record
    drs_List: array[tRegion64] of byte;
    drs_SetOfSol: tSolSet64;
    drs_bas: byte;
    drs_Sol: byte;
    drs_SolCnt: byte;
    drs_Dgt2Add: byte;
    drs_NeedsOneMoreDigit: boolean;
  end;

  tCombineForOneThread = record
    cft_sqr2b,
    cft_deltaNextSqr,
    cft_delta: tNumtoBase;
    cft_count : Uint64;
    cft_ThreadID: NativeUint;
    cft_ThreadHandle: NativeUint;
    //extend to 512 byte
    cft_dummy : array[0..512-1-3*(SizeOf(tNumtoBase)-SizeOf(NativeUint))] of byte;
  end;
  pThreadBlock = ^tCombineForOneThread;
  tMulti = record
             m_Startofs : Uint32;
             m_count  : Uint32;
           end;
const
  //1 shl 0, ..,1 shl 63
  cAND_Mask64: tMask64 =(
$FFFFFFFFFFFFFFFE,$FFFFFFFFFFFFFFFD,$FFFFFFFFFFFFFFFB,$FFFFFFFFFFFFFFF7,
$FFFFFFFFFFFFFFEF,$FFFFFFFFFFFFFFDF,$FFFFFFFFFFFFFFBF,$FFFFFFFFFFFFFF7F,
$FFFFFFFFFFFFFEFF,$FFFFFFFFFFFFFDFF,$FFFFFFFFFFFFFBFF,$FFFFFFFFFFFFF7FF,
$FFFFFFFFFFFFEFFF,$FFFFFFFFFFFFDFFF,$FFFFFFFFFFFFBFFF,$FFFFFFFFFFFF7FFF,
$FFFFFFFFFFFEFFFF,$FFFFFFFFFFFDFFFF,$FFFFFFFFFFFBFFFF,$FFFFFFFFFFF7FFFF,
$FFFFFFFFFFEFFFFF,$FFFFFFFFFFDFFFFF,$FFFFFFFFFFBFFFFF,$FFFFFFFFFF7FFFFF,
$FFFFFFFFFEFFFFFF,$FFFFFFFFFDFFFFFF,$FFFFFFFFFBFFFFFF,$FFFFFFFFF7FFFFFF,
$FFFFFFFFEFFFFFFF,$FFFFFFFFDFFFFFFF,$FFFFFFFFBFFFFFFF,$FFFFFFFF7FFFFFFF,
$FFFFFFFEFFFFFFFF,$FFFFFFFDFFFFFFFF,$FFFFFFFBFFFFFFFF,$FFFFFFF7FFFFFFFF,
$FFFFFFEFFFFFFFFF,$FFFFFFDFFFFFFFFF,$FFFFFFBFFFFFFFFF,$FFFFFF7FFFFFFFFF,
$FFFFFEFFFFFFFFFF,$FFFFFDFFFFFFFFFF,$FFFFFBFFFFFFFFFF,$FFFFF7FFFFFFFFFF,
$FFFFEFFFFFFFFFFF,$FFFFDFFFFFFFFFFF,$FFFFBFFFFFFFFFFF,$FFFF7FFFFFFFFFFF,
$FFFEFFFFFFFFFFFF,$FFFDFFFFFFFFFFFF,$FFFBFFFFFFFFFFFF,$FFF7FFFFFFFFFFFF,
$FFEFFFFFFFFFFFFF,$FFDFFFFFFFFFFFFF,$FFBFFFFFFFFFFFFF,$FF7FFFFFFFFFFFFF,
$FEFFFFFFFFFFFFFF,$FDFFFFFFFFFFFFFF,$FBFFFFFFFFFFFFFF,$F7FFFFFFFFFFFFFF,
$EFFFFFFFFFFFFFFF,$DFFFFFFFFFFFFFFF,$BFFFFFFFFFFFFFFF,$7FFFFFFFFFFFFFFF
);
  charSet: array[0..63] of AnsiChar =
    '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.';
  {$IFDEF CPU64}
  Mask1 = $8080808080808080;
  MaxBit= 63;
  {$ENDIF}
  {$IFDEF CPU32}
  Mask1 = $80808080;
  MaxBit= 31;
  {$ENDIF}
//  Multi21 : tMulti = (m_offset:6;m_delta:(2,8,2,8));
  Multi21 : tMulti = (m_Startofs:6;m_count:4);
  WAIT_MS = 1875; // check every x ms if finished *32 = 60 seconds
  timeFac = WAIT_MS/1000/60 ;// ( i AND 31 ) = 0

//############################################################################
//Input values as constants
  LoLimit = 2;
  HiLimit = 28;// For 32-Bit CPU max HiLimit = 32

  cFileName = 'Test_2_28.txt';//'/dev/NULL';//'nonsens.txt';
  // use hyperthreading = 0

  StartOfs= 1;//testcount*StepWidth = 11038840378368;
//############################################################################

  procedure AddNum(var add1: tNumtoBase; const add2: tNumtoBase); forward;

var
  {$ALIGN 32}
  ThreadBlocks: array of tCombineForOneThread;
  {$ALIGN 32}
  s :shortString;
  Num, sqr2B, deltaNextSqr, delta: tNumtoBase;
  DgtRtSqr: tDgtRootSqr;
  T,T0, T1: TDateTime;
  gblThreadCount,
  Finished: Uint32;
  f : text;

  function GetThreadCount: NativeUInt;
  begin
    {$IFDEF Windows}
       Result := GetCpuCount;
    {$ELSE}
      Result := 12;//GetCpuCount;// is not working under Linux ???
    {$ENDIF}
  end;

  procedure OutThreadStep(i:double;ThCnt:NativeInt;stepWidth,StartOffSet: Int64);
  var
    j,min : NativeInt;
    s,sf: ShortString;
  Begin
    str(i:8:2,s);
    s := s+' min ';
    stepWidth := -ThCnt*stepWidth;
    min := 0;
    For j := 0 to ThCnt-1 do
      If min > ThreadBlocks[j].cft_count then
        min :=  ThreadBlocks[j].cft_count;
    str(stepWidth*min+StartOffSet:16,sf);
    s := s+sf;
    writeln(s);
    writeln(f,s);
    Flush(f);
  end;

  procedure OutNum(const num: tNumtoBase;var s: ShortString);
  var
    pB : pByte;
    i: NativeInt;
  begin
    with num do
    begin
      pB := @ntb_dgt[0];
      i := ntb_cnt*SizeOf(NativeUint);
      while pB[i] = 0 do
        dec(i);
      IF i < 0 then
        i := 0;
      for i := i downto 0 do
        s := s+charSet[pB[i]];
      s := s+' ';
    end;
    Write(s);
    Write(f,String(s));
  end;

  procedure OutNumSqr;
  begin
    s := ' Num ';OutNum(Num,s);
    s := ' sqr ';OutNum(sqr2B,s);
    writeln;writeln(f);Flush(f);
  end;

  function getDgtRtNum(const num: tNumtoBase): NativeInt;
  var
    pB : pByte;
    i: NativeInt;
  begin
    pB := @num.ntb_dgt[0];
    with num do
    begin
      Result := 0;
      for i := 0 to num.ntb_cntbts - 1 do
        Inc(Result, pB[i]);
      Result := Result mod (ntb_bas - 1);
    end;
  end;

  procedure CalcDgtRootSqr(base: NativeUInt);
  var
    ChkSet: array[tRegion64] of tSolSet64;
    ChkCnt: array[tRegion64] of byte;
    i, j: NativeUInt;
    PTest: tSolSet64;
  begin
    for i := low(ChkCnt) to High(ChkCnt) do
    begin
      ChkCnt[i] := 0;
      ChkSet[i] := [];
    end;
    ptest := [];
    with DgtRtSqr do
    begin
      //pandigtal digital root (sum all digits of base) mod (base-1)
      drs_bas := base;
      if Odd(base) then
        drs_Sol := base div 2
      else
        drs_Sol := 0;

      base := base - 1;
      //calc which dgt root the square of the number will become
      for i := 0 to base - 1 do
        drs_List[i] := (i * i) mod base;
      //searching for solution
      drs_SolCnt := 0;
      for i := 0 to base - 1 do
        if drs_List[i] = drs_Sol then
        begin
          include(ptest, i);
          Inc(drs_SolCnt);
        end;
      //if not found then NeedsOneMoreDigit
      drs_NeedsOneMoreDigit := drs_SolCnt = 0;
      if drs_NeedsOneMoreDigit then
      begin
        for j := 1 to Base do
          for i := 0 to Base do
            if drs_List[j] = (drs_Sol + i) mod BASE then
            begin
              include(ptest, i);
              include(ChkSet[i], j);
              Inc(ChkCnt[i]);
            end;
        i := 1;
        repeat
          if i in pTest then
          begin
            drs_Dgt2Add := i;
            BREAK;
          end;
          Inc(i);
        until i > base;
        writeln('insert ', i);
      end;
    end;
  end;

  procedure conv_ui_num(base: NativeUint; ui: Uint64; var Num: tNumtoBase);
  var
    pB : pByte;
    i: NativeUInt;
  begin
    with num do
    begin
      for i := 0 to high(tNumtoBase.ntb_dgt) do
        ntb_dgt[i] := 0;
      pB := @ntb_dgt[0];
      ntb_bas := base;
      ntb_Mask0 := (Mask1 shr 7)*(256-Base);
      ntb_TestSet := Uint64(1) shl base -1;
      ntb_cntbts := 0;
      ntb_cnt := 0;
      if ui <> 0 then
      begin
        i := 0;
        repeat
          pB[i] := ui mod base;
          ui := ui div base;
          Inc(i);
        until ui = 0;
        ntb_cntbts := i;
        ntb_HighByte := pB[i-1];
        ntb_cnt := i DIV SizeOf(NativeUint) +1 ;
      end;
    end;
   end;

  procedure conv2Num(base: NativeUint; var Num: tNumtoBase; var s: mpz_t);
  var
    tmp: mpz_t;
    pB : pByte;
    i: NativeUInt;
  begin
    mpz_init_set(tmp,s);
    for i := 0 to high(tNumtoBase.ntb_dgt) do
      Num.ntb_dgt[i] := 0;
    pB := @Num.ntb_dgt[0];
    with num do
    begin
      ntb_bas := base;
      ntb_Mask0 := (Mask1 shr 7)*(256-Base);
      ntb_TestSet := Uint64(1) shl base -1;
      i := 0;
      repeat
        pB[i] := mpz_tdiv_q_ui(tmp, tmp, base);
        Inc(i);
      until mpz_cmp_ui(tmp, 0) = 0;
      ntb_HighByte := pB[i-1];
      ntb_cntbts := i;
      ntb_cnt := i DIV SizeOf(NativeUint) +1;
    end;
    mpz_clear(tmp);
  end;

  procedure StartValueCreate(base: NativeUInt);
  //create the lowest pandigital number "102345...Base-1 "
  //calc sqrt +1 and convert n new format.
  var
    sv_sqr, sv,sv_add: mpz_t;
    k, dblDgt: NativeUint;

  begin
    mpz_init(sv);
    mpz_init(sv_sqr);

    mpz_init_set_si(sv_sqr, base);//"10"
    CalcDgtRootSqr(base);

    if DgtRtSqr.drs_NeedsOneMoreDigit then
    begin
      dblDgt := DgtRtSqr.drs_Dgt2Add;
      if dblDgt = 1 then
      begin
        for k := 1 to base - 1 do
        begin
          mpz_mul_ui(sv_sqr, sv_sqr, base);
          mpz_add_ui(sv_sqr, sv_sqr, k);
        end;
      end
      else
      begin
        for k := 2 to dblDgt do
        begin
          mpz_mul_ui(sv_sqr, sv_sqr, base);
          mpz_add_ui(sv_sqr, sv_sqr, k);
        end;
        for k := dblDgt to base - 1 do
        begin
          mpz_mul_ui(sv_sqr, sv_sqr, base);
          mpz_add_ui(sv_sqr, sv_sqr, k);
        end;
      end;
    end
    else
    begin
      for k := 2 to base - 1 do
      begin
        mpz_mul_ui(sv_sqr, sv_sqr, base);
        mpz_add_ui(sv_sqr, sv_sqr, k);
      end;
    end;

    mpz_sqrt(sv, sv_sqr);
    writeln('Start Offset ',StartOfs);
    //Windows64 gmp.dll or freepascal does not use Uint64 only Uint32
    mpz_init_set_ui(sv_add,StartOfs shr 32);
    // shift left 32
    mpz_mul_2exp(sv_add,sv_add,32);
    mpz_add_ui(sv_add,sv_add,Uint32(StartOfs));
    mpz_add(sv, sv, sv_add);
    mpz_mul(sv_sqr, sv, sv);

    conv2Num(base, Num, sv);
    conv2Num(base, sqr2B, sv_sqr);

    mpz_clear(sv_add);
    mpz_clear(sv_sqr);
    mpz_clear(sv);
    OutNumSqr;
  end;

  function ExtractThreadVal(ThreadNr: NativeInt ): Uint64;
  begin
    with ThreadBlocks[ThreadNr] do
    begin
      sqr2b := cft_sqr2b;
      Result := cft_count;
      cft_ThreadID := 0;
      cft_ThreadHandle := 0;
    end;
  end;

 function CheckPandigital(const n: tNumtoBase): NativeUint;inline;
  var
    pB : pByte;
    pMask: tpMask64;
    i: NativeInt;
  begin
    i := n.ntb_cntbts;
    pB := @n.ntb_dgt[0];
    pMask := @cAND_Mask64;
    result := n.ntb_TestSet;
    while i >= 4 do
    begin
      dec(i,4);
      result := pMask[pB[i+0]] AND result;
      result := pMask[pB[i+1]] AND result;
      result := pMask[pB[i+2]] AND result;
      result := pMask[pB[i+3]] AND result;
    end;
    if i > 0 then
    repeat
      result := pMask[pB[i]] AND result;
      dec(i);
    until i<0;
  end;

  procedure IncNumBig(var add1: tNumtoBase; n: Uint64);
  var
    pB : pByte;
    i, s, b, carry: NativeUInt;
  begin
    b := add1.ntb_bas;
    i := 0;
    carry := 0;
    pB := @add1.ntb_dgt[0];
    while n > 0 do
    begin
      s := pB[i] + carry + n mod b;
      carry := Ord(s >= b);
      s := s - (-carry and b);
      pB[i] := s;
      n := n div b;
      Inc(i);
    end;

    while carry <> 0 do
    begin
      s := pB[i] + carry;
      carry := Ord(s >= b);
      s := s - (-carry and b);
      pB[i] := s;
      Inc(i);
    end;

    if add1.ntb_cntbts < i then
    Begin
      add1.ntb_cntbts := i;
      add1.ntb_cnt := i DIV (SizeOf(NativeUint))+1;
    end;
  end;

  procedure IncSmallNum(var add1: tNumtoBase; carry: NativeUInt);
  //prerequisites carry < base
  var
   pB : pByte;
    i, s, b: NativeUInt;
  begin
    b := add1.ntb_bas;
    pB := @add1.ntb_dgt[0];
    i := 0;
    while carry <> 0 do
    begin
      s := pB[i] + carry;
      carry := Ord(s >= b);
      s := s - (-carry and b);
      pB[i] := s;
      Inc(i);
    end;
    if add1.ntb_cntbts < i then
    Begin
      add1.ntb_cntbts := i;
      add1.ntb_cnt := i DIV SizeOf(NativeUint) +1;
    end;
  end;

  procedure Mul_num_ui(var n: tNumtoBase; ui: Uint64);
  var
    dbl: tNumtoBase;
  begin
    dbl := n;
    conv_ui_num(n.ntb_bas, 0, n);
    while ui > 0 do
    begin
      if Ui and 1 <> 0 then
        AddNum(n, dbl);
      AddNum(dbl, dbl);
      ui := ui div 2;
    end;
  end;

  procedure CalcDeltaSqr(const num: tNumtoBase; var dnsq, dlt: tNumtoBase;
    n: NativeUInt);
  //calc  deltaNextSqr   //n*num
  begin
    dnsq := num;
    Mul_num_ui(dnsq, n);
    AddNum(dnsq, dnsq);
    IncNumBig(dnsq, n * n);
    conv_ui_num(num.ntb_bas, 2 * n * n, dlt);
  end;

  procedure PrepareThreads(thdCount,stepWidth:NativeInt);
  //starting the threads at num,num+stepWidth,..,num+(thdCount-1)*stepWidth
  //stepwith not stepWidth but thdCount*stepWidth
//insert 6
//  6  :  4  6 14 16
  var
    tmpnum,tmpsqr2B,tmpdeltaNextSqr,tmpdelta :tNumToBase;
    deltas: array[0..7] of Uint32;
    Multi : tMulti;

    i,base : NativeInt;
  Begin
    tmpnum := num;
    tmpsqr2B := sqr2B;
    tmpdeltaNextSqr := deltaNextSqr;
    tmpdelta := delta;
    IF StepWidth <> 1 then
    Begin
      For i := 0 to thdCount-1 do
      Begin
        //init ThreadBlock
        With ThreadBlocks[i] do
        begin
          cft_sqr2b := tmpsqr2B;
          cft_count := 0;
          CalcDeltaSqr(tmpnum,cft_deltaNextSqr,cft_delta,thdCount*stepWidth);
        end;
        //Next sqr number in stepWidth
        IncSmallNum(tmpnum,stepWidth);
        AddNum(tmpsqr2B,tmpdeltaNextSqr);
        IF CheckPandigital(sqr2B)=0 then
        begin
          writeln(' solution found ');
          OutNumSqr;
        end
        else
          AddNum(tmpdeltaNextSqr,tmpdelta);
      end;
    end
    else
    Begin
      Multi := Multi21;
      base := tmpNum.ntb_bas-1;
      For i := 0 to thdCount-1 do
      Begin
        //init ThreadBlock
        With ThreadBlocks[i] do
        begin
          cft_sqr2b := tmpsqr2B;
          cft_count := 0;
          CalcDeltaSqr(tmpnum,cft_deltaNextSqr,cft_delta,thdCount*stepWidth);
        end;
        //Next sqr number in stepWidth
        IncSmallNum(tmpnum,stepWidth);
        AddNum(tmpsqr2B,tmpdeltaNextSqr);
        IF CheckPandigital(sqr2B)=0 then
        begin
          writeln(' solution found ');
          OutNumSqr;
        end
        else
          AddNum(tmpdeltaNextSqr,tmpdelta);
      end;
    end;
  end;

procedure AddNum(var add1: tNumtoBase; const add2: tNumtoBase);
  //   N0 AND mask1                  -> 80 00 80 80 80 00 80
  //  (N0 AND mask1)XOR mask1        -> 00 80 00 00 00 80 00
  // ((N0 AND mask1)XOR mask1) shr 7 -> 00 01 00 00 00 01 00
  //                          *k     -> 00  k 00 00 00 k  00
var
  pAdd1,pAdd2 :pNativeUint;
  NextCarry,n,k,M0: NativeUInt;
  M1,i : NativeUInt;
Begin
  IF add1.ntb_cnt= 0 then
  Begin
    add1 := add2;
    EXIT;
  end;
  IF add2.ntb_cnt= 0 then
    EXIT;

  M1 :=NativeUint(Mask1);
  M0:= add1.ntb_mask0;

  k := byte(M0);

  i := Add2.ntb_cnt;
  pAdd2:= @Add2.ntb_dgt[0];
  pAdd1:= @Add1.ntb_dgt[0];

  NextCarry := 0;
  repeat
    //depends extremly on arrangement.
    n := pAdd2^+M0+NextCarry+pAdd1^;
    inc(pAdd2);
    NextCarry := (n shr MaxBit) XOR 1;
    n := n-M0+(((n AND M1)XOR M1) SHR 7)*k;
    pAdd1^ :=n;
    inc(pAdd1);
    dec(i);
  until i = 0;

  i := Add2.ntb_cnt;
  IF NextCarry <> 0 then
  Begin
    repeat
      n := pAdd1^+M0+NextCarry;
      NextCarry := (n shr MaxBit) XOR 1;
      n := n-M0+(((n AND M1)XOR M1) SHR 7)*k;
      pAdd1^ :=n;
      inc(i);
      inc(pAdd1);
    until NextCarry = 0;
  end;
  with Add1  do
  Begin
    IF i > ntb_cnt then
    Begin
       ntb_cnt := i;
       ntb_cntbts := Add1.ntb_cnt*SizeOf(NativeUint);
       while (Add1.ntb_cntbts > 0) AND (ntb_dgt[Add1.ntb_cntbts] = 0) do
         dec(Add1.ntb_cntbts);
       inc(Add1.ntb_cntbts);
     end;
  end;
end;

procedure AddNumSimple(var add1: tNumtoBase; const add2: tNumtoBase);inline;
  // Add1&Add2 <> 0 and  Add2< Add1
var
  pAdd1,pAdd2 :pNativeUint;
  NextCarry,n,k,M0: NativeUInt;
  M1,i : NativeUInt;
Begin
  M1 :=NativeUint(Mask1);
  M0:= add1.ntb_mask0;
  k := byte(M0);

  i := Add2.ntb_cnt;
  pAdd2:= @Add2.ntb_dgt[0];
  pAdd1:= @Add1.ntb_dgt[0];

  NextCarry := NextCarry XOR NextCarry;
  repeat
    // extremly depends on arrangement.
    n := pAdd2^+NextCarry+M0+pAdd1^;
    NextCarry := (n shr MaxBit) XOR 1;
    n := (((n AND M1)XOR M1) SHR 7)*k-M0+n;
//  n := n-M0+(((n AND M1)XOR M1) SHR 7)*k;
    inc(pAdd2);
    dec(i);
    pAdd1^ :=n;
    inc(pAdd1);
  until i = 0;

  IF NextCarry <> 0 then
  Begin
    i := Add2.ntb_cnt;
    repeat
      n := pAdd1^+M0+NextCarry;
      NextCarry := (n shr MaxBit) XOR 1;
      n := (((n AND M1)XOR M1) SHR 7)*k+n-M0;
      pAdd1^ :=n;
      inc(i);
      inc(pAdd1);
    until NextCarry = 0;
{   //never reached
    IF i > Add1.ntb_cnt then
    Begin
      Add1.ntb_cnt := i;
      Add1.ntb_cntbts := Add1.ntb_cnt*SizeOf(NativeUint);
    end;
    * }
  end;
end;

  procedure TestRunThd(parameter: pointer);
  var
    i: Uint64;
    pThdBlk: pThreadBlock;
  begin
    pThdBlk := @ThreadBlocks[NativeUint(parameter)];
    with pThdBlk^ do
    begin
      i := 0;
      repeat
        if finished <> 0 then
          BREAK;
        //next square number
        AddNumSimple(cft_sqr2b, cft_deltaNextSqr);
        AddNumSimple(cft_deltaNextSqr, cft_delta);
        Inc(i);
        cft_count := -i;
      until CheckPandigital(cft_sqr2b)=0;
      if finished = 0 then
      begin
        InterLockedIncrement(finished);
        cft_count := i;
      end;
    end;
    EndThread(0);
  end;

  procedure Test(base: NativeInt);
  var
    stepWidth: Uint64;
    i, j,UsedThreads,StartOffSet: NativeInt;
  begin
    T0 := now;
    StartValueCreate(base);
    StartOffSet  := StartOfs;
    deltaNextSqr := num;
    AddNum(deltaNextSqr, deltaNextSqr);
    IncSmallNum(deltaNextSqr, 1);
    stepWidth := 1;
    if (Base > 4) and not (DgtRtSqr.drs_NeedsOneMoreDigit) then
    begin
      //Find first number which can get the solution
      with dgtrtsqr do
        while drs_List[getDgtRtNum(num)] <> drs_sol do
        begin
          IncSmallNum(num, 1);
          AddNum(sqr2B, deltaNextSqr);
          IncSmallNum(deltaNextSqr, 2);
        end;
      stepWidth := (Base - 1) div DgtRtSqr.drs_SolCnt;
      if stepWidth * DgtRtSqr.drs_SolCnt <> (Base - 1) then
        stepWidth := 1;
    end;
    CalcDeltaSqr(num,deltaNextSqr,delta,stepWidth);

    i := 0;

    finished := 0;
    j := 0;
    UsedThreads := gblThreadCount;
    if base < 20 then
      UsedThreads := 1;
    PrepareThreads(UsedThreads,stepWidth);

    writeln(f,'Base ', base, ' test every ', stepWidth,' threads = ',UsedThreads);
    Write(f,'Start  :');
    writeln('Base ', base, ' test every ', stepWidth,' threads = ',UsedThreads);
    Write('Start  :');
    OutNumSqr;

    if (CheckPandigital(sqr2b)<>0) then
    begin
      while (j < UsedThreads) and (finished = 0) do
      begin
        with ThreadBlocks[j] do
        begin
          cft_ThreadHandle :=
          BeginThread(@TestRunThd, Pointer(j), cft_ThreadID,
              4*4096);
        end;
        Inc(j);
      end;
      UsedThreads := j;

      i := 0;
      repeat
        IF base > 28 then
        Begin
          {$IFDEF WIN}
          WaitForThreadTerminate(ThreadBlocks[0].cft_ThreadHandle,WAIT_MS)
          {$ELSE}
          sleep(WAIT_MS)
          {$ENDIF}
        end
        else
          WaitForThreadTerminate(ThreadBlocks[0].cft_ThreadHandle,-1);
        inc(i);
        IF (I and 31) = 0 then
          OutThreadStep(i*timeFac,UsedThreads,stepWidth,StartOffSet);
      until finished <> 0;

      WaitForThreadTerminate(ThreadBlocks[0].cft_ThreadHandle, -1);
      repeat
        Dec(j);
        with ThreadBlocks[j] do
        begin
          WaitForThreadTerminate(cft_ThreadHandle, -1);
          CloseThread(cft_ThreadHandle);
          if Int64(cft_count) > 0 then
            finished := j;
        end;
      until j = 0;
    end;

    i := ExtractThreadVal(finished);
    j := i*UsedThreads+finished;//TestCount starts at original num
    IncNumBig(num,j*stepWidth);
    T1 := now;
    Write(f,'Result :');
    Write('Result :');
    OutNumSqr;
    Writeln(f,(T1 - t0) * 86400: 9: 3, ' s Testcount : ',j: 14,j*StepWidth:18);
    Writeln((T1 - t0) * 86400: 9: 3, ' s Testcount : ',j: 14,j*StepWidth:18);
    Writeln(UsedThreads*(T1 - t0) * 86400: 9: 3, ' s');
  end;

var
  base: NativeUint;
begin
  Writeln(cFileName);
  AssignFile(f,cFileName);
  {$I-}
    Append(f);
  {$I+}
  IF IoResult <> 0 then
    rewrite(f);
  gblThreadCount:= GetThreadCount;
  writeln(' Cpu Count : ', gblThreadCount);
  setlength(ThreadBlocks,  gblThreadCount);

  T := now;
  for base := LoLimit to HiLimit do
    Test(base);
  T := (now-T)*86400;
  writeln('completed in ', T: 0: 3, ' seconds');
  writeln(f,'completed in ',T: 0: 3, ' seconds');
  Close(f);

  setlength(ThreadBlocks, 0);
  {$IFDEF WINDOWS}
  readln;
  {$ENDIF}
end.
```

//{{out}}

```txt
 Num 10  sqr 100
 Num 10  sqr 100
Base 2 test every 1 threads = 1
Start  : Num 10  sqr 100
Result : Num 10  sqr 100
    0.000 s Testcount :              0                 0
 Num 11  sqr 121
 Num 11  sqr 121
Base 3 test every 1 threads = 1
Start  : Num 11  sqr 121
Result : Num 11  sqr 121
    0.000 s Testcount :              0                 0
 Num 21  sqr 1101
Base 4 test every 1 threads = 1
Start  : Num 21  sqr 1101
Result : Num 33  sqr 3201
    0.000 s Testcount :              6                 6
 Num 214  sqr 102411
Base 5 test every 1 threads = 1
Start  : Num 214  sqr 102411
Result : Num 243  sqr 132304
    0.000 s Testcount :             14                14
 Num 232  sqr 103104
Base 6 test every 5 threads = 1
Start  : Num 235  sqr 105441
Result : Num 523  sqr 452013
    0.000 s Testcount :             20               100
 Num 1012  sqr 1024144
Base 7 test every 6 threads = 1
Start  : Num 1020  sqr 1040400
Result : Num 1431  sqr 2450361
    0.000 s Testcount :             34               204
 Num 2704  sqr 10237020
Base 8 test every 7 threads = 1
Start  : Num 2705  sqr 10244631
Result : Num 3344  sqr 13675420
    0.000 s Testcount :             41               287
 Num 10117  sqr 102363814
Base 9 test every 4 threads = 1
Start  : Num 10117  sqr 102363814
Result : Num 11642  sqr 136802574
    0.000 s Testcount :            289              1156
 Num 31992  sqr 1023488064
Base 10 test every 3 threads = 1
Start  : Num 31992  sqr 1023488064
Result : Num 32043  sqr 1026753849
    0.000 s Testcount :             17                51
 Num 101172  sqr 10234761064
Base 11 test every 10 threads = 1
Start  : Num 101175  sqr 10235267A63
Result : Num 111453  sqr 1240A536789
    0.001 s Testcount :           1498             14980
 Num 35A924  sqr 102345A32554
Base 12 test every 11 threads = 1
Start  : Num 35A924  sqr 102345A32554
Result : Num 3966B9  sqr 124A7B538609
    0.000 s Testcount :           6883             75713
 Num 3824C73  sqr 10233460766739
Base 13 test every 1 threads = 1
Start  : Num 3824C73  sqr 10233460766739
Result : Num 3828943  sqr 10254773CA86B9
    0.000 s Testcount :           8242              8242
 Num 3A9774B  sqr 102345706AC8C9
Base 14 test every 13 threads = 1
Start  : Num 3A9774C  sqr 1023457801D984
Result : Num 3A9DB7C  sqr 10269B8C57D3A4
    0.000 s Testcount :           1330             17290
 Num 10119106  sqr 10234567A153C26
Base 15 test every 14 threads = 1
Start  : Num 10119108  sqr 1023456BA5BA144
Result : Num 1012B857  sqr 102597BACE836D4
    0.000 s Testcount :           4216             59024
 Num 4046641A  sqr 10234567E55C52A4
Base 16 test every 15 threads = 1
Start  : Num 40466424  sqr 1023456CEADC2510
Result : Num 404A9D9B  sqr 1025648CFEA37BD9
    0.000 s Testcount :          18457            276855
 Num 423F5E486  sqr 101234567967G80FD2
Base 17 test every 1 threads = 1
Start  : Num 423F5E486  sqr 101234567967G80FD2
Result : Num 423F82GA9  sqr 101246A89CGFB357ED
    0.003 s Testcount :         195112            195112
 Num 44B433H7D  sqr 102345678F601E1FB7
Base 18 test every 17 threads = 1
Start  : Num 44B433H7F  sqr 102345679E6908HD69
Result : Num 44B482CAD  sqr 10236B5F8EG4AD9CH7
    0.001 s Testcount :          30440            517480
 Num 1011B10785  sqr 102345678A30GF85556
Base 19 test every 6 threads = 1
Start  : Num 1011B10789  sqr 102345678I39A8G87F5
Result : Num 1011B55E9A  sqr 10234DHBG7CI8F6A9E5
    0.001 s Testcount :          93021            558126
 Num 49DDBE2J9C  sqr 1023456789DHDH9DH834
Base 20 test every 19 threads = 12
Start  : Num 49DDBE2JA0  sqr 102345678D5CCEH05000
Result : Num 49DGIH5D3G  sqr 1024E7CDI3HB695FJA8G
    0.021 s Testcount :       11310604         214901476
 Num 4C9HE5CC2DB  sqr 10234566789GK362F7BGIG
Base 21 test every 1 threads = 12
Start  : Num 4C9HE5CC2DB  sqr 10234566789GK362F7BGIG
Result : Num 4C9HE6GHFJF  sqr 102345H7AC86GE9BKI6JDF
    0.006 s Testcount :        4914193           4914193
 Num 4F942523JK5  sqr 1023456789AF71694A3533
Base 22 test every 21 threads = 12
Start  : Num 4F942523JL0  sqr 1023456789HL35DJ1I4100
Result : Num 4F94788GJ0F  sqr 102369FBGDEJ48CHI7LKA5
    0.054 s Testcount :       27804949         583903929
 Num 1011D108L541  sqr 1023456789ABD6D2675E381
Base 23 test every 22 threads = 12
Start  : Num 1011D108L54M  sqr 1023456789C7F59L30C8ED1
Result : Num 1011D3EL56MC  sqr 10234ACEDKG9HM8FBJIL756
    0.038 s Testcount :       17710217         389624774
 Num 4LJ0HD4763F4  sqr 1023456789ABE8FFN20B4E0G
Base 24 test every 23 threads = 12
Start  : Num 4LJ0HD4763F6  sqr 1023456789AC9NJIL6HG54DC
Result : Num 4LJ0HDGF0HD3  sqr 102345B87HFECKJNIGMDLA69
    0.008 s Testcount :        4266555          98130765
 Num 1011E109GHMMM  sqr 1023456789ABD5AHDHG370GC9
Base 25 test every 12 threads = 12
Start  : Num 1011E109GHMMM  sqr 1023456789ABD5AHDHG370GC9
Result : Num 1011E145FHGHM  sqr 102345DOECKJ6GFB8LIAM7NH9
    0.160 s Testcount :       78092125         937105500
 Num 52K8N4MNP7AMA  sqr 1023456789ABCMPE8HDDJL8P1M
Base 26 test every 5 threads = 12
Start  : Num 52K8N4MNP7AME  sqr 1023456789ABEBLL1L0F3FG7PE
Result : Num 52K8N53BDM99K  sqr 1023458LO6IEMKG79FPCHNJDBA
    0.884 s Testcount :      402922568        2014612840
 Num 1011F10AB5HL6J  sqr 1023456789ABCF787BM8395B5PA
Base 27 test every 26 threads = 12
Start  : Num 1011F10AB5HL71  sqr 1023456789ABD6808CDF1LQ7AE1
Result : Num 1011F11E37OBJJ  sqr 1023458ELOMDHBIJFGKP7CQ9N6A
    1.049 s Testcount :      457555293       11896437618
 Num 58A3CKOHN4IK4D  sqr 1023456789ABCDIK29E6HB1R37Q1
Base 28 test every 9 threads = 12
Start  : Num 58A3CKOHN4IK4L  sqr 1023456789ABCGJDO8M4JG8HMMFL
Result : Num 58A3CKP3N4CQD7  sqr 1023456CGJBIRQEDHP98KMOAN7FL
    1.672 s Testcount :      749593054        6746337486
completed in 3.898 seconds

Base 29 test every 1 threads = 12
Start  : Num 5BAEFC5QHESPCLA  sqr 10223456789ABCDKM4JI4S470KCSHD
    1.00 min      24099604789
    2.00 min      48201071089
    3.00 min      72295381621
Result : Num 5BAEFC62RGS0KJF  sqr 102234586REOSIGJD9PCF7HBLKANQM
  230.632 s Testcount :    92238034003       92238034003
 Num 5EF7R2P77FFPBMR  sqr 1023456789ABCDEPPNIG6S4MJNB8C9
Base 30 test every 29 threads = 12
Start  : Num 5EF7R2P77FFPBN5  sqr 1023456789ABCDHNHROTMC0MS6RGKP
Result : Num 5EF7R2POS9MQRN7  sqr 1023456DMAPECBQOLSITK9FR87GHNJ
   35.626 s Testcount :    13343410738      386958911402
 Num 1011H10BS64GFL6U  sqr 1023456789ABCDEH3122BRSP7T7G6H1
Base 31 test every 30 threads = 12
Start  : Num 1011H10BS64GFL76  sqr 1023456789ABCDF03FNNQ29H0ULION5
Result : Num 1011H10CDMAUP44O  sqr 10234568ABQUJGCNFP7KEM9RHDLTSOI
   41.251 s Testcount :    15152895679      454586870370
 Num 5L6HID7BTGM6RU9L  sqr 1023456789ABCDEFGQNN3264K1GRK97P
Base 32 test every 31 threads = 12
Start  : Num 5L6HID7BTGM6RUAA  sqr 1023456789ABCDEMULAP8DRPBULSA2B4
Result : Num 5L6HID7BVGE2CIEC  sqr 102345678VS9CMJDRAIOPLHNFQETBUKG
    5.626 s Testcount :     2207946558       68446343298
completed in 317.047 seconds
 Num 1011I10CLMTDCMPC1  sqr 1023456789ABCDEFHSSWJ340NGCV8MTO1
Base 33 test every 8 threads = 12
Start  : Num 1011I10CLMTDCMPC6  sqr 1023456789ABCDEFRT6F1D7S9EA03JJD3
 Num 1011I10CLMTDCMPC1  sqr 1023456789ABCDEFHSSWJ340NGCV8MTO1
Base 33 test every 8 threads = 12
Start  : Num 1011I10CLMTDCMPC6  sqr 1023456789ABCDEFRT6F1D7S9EA03JJD3
    1.00 min     184621467265
    2.00 min     369105711649
Result : Num 1011I10CLWWNS6SKS  sqr 102345678THKFAERNWJGDOSQ9BCIUVMLP
  140.634 s Testcount :    53808573863      430468590904
 Num 5SEMXRII09S90UO6V  sqr 1023456789ABCDEFGKNK3JK9NREFLEH5Q9
Base 34 test every 33 threads = 12
Start  : Num 5SEMXRII09S90UO7P  sqr 1023456789ABCDEFQ7HPX8WRC9L0GV31SD
    1.00 min     747770289553
    2.00 min    1495002801997
..  8.00 min    5978195501257
    9.00 min    6725222258833
Result : Num 5SEMXRII42NG8AKSL  sqr 102345679JIESRPA8BLCVKDNMHUFTGOQWX
  549.392 s Testcount :   205094427126     6768116095158
 Num 1011J10DE6M9QOAY42  sqr 1023456789ABCDEFGHSOEHTX34IF9YB1CG4
Base 35 test every 34 threads = 12
Start  : Num 1011J10DE6M9QOAY42  sqr 1023456789ABCDEFGHSOEHTX34IF9YB1CG4
    1.00 min     749708230993
    2.00 min    1496695534873
.. 27.00 min   20178502394905
   28.00 min   20925398930617  //<== > solution, because finsich tested every 1.875 seconds
Result : Num 1011J10DEFW1QTVBXR  sqr 102345678RUEPV9KGQIWFOBAXCNSLDMYJHT
 1681.925 s Testcount :   614575698110    20895573735740
 Num 6069962AODK1L20LTW  sqr 1023456789ABCDEFGHSWJSDUGHWCR30SK5CG
Base 36 test every 35 threads = 12
Start  : Num 6069962AODK1L20LUU  sqr 1023456789ABCDEFGT58D9PASNXYM2SLPEP0
    1.00 min     787213111441
    2.00 min    1586599478101
..192.00 min  152890333403641
  193.00 min  153686150046241
Result : Num 6069962APW1QG36EV8  sqr 102345678RGQKMOCBLZIYHN9WDJEUXFVPATS
11584.118 s Testcount :  4392178427722   153726244970270
completed in 11584.118 seconds

//Base 37 still missing :-(

 Num 66FVHSMH0OXH39bH6LT  sqr 1023456789ABCDEFGHIV4YWaF08URZ5H135NO5
Base 38 test every 37 threads = 12
Start  : Num 66FVHSMH0OXH39bH6MN  sqr 1023456789ABCDEFGHT7ZLWWKUXYO9YZW62QbZ
    1.00 min     785788279813
    2.00 min    1568354438749
.. 58.00 min   45309747953833
   59.00 min   46095937872493
Result : Num 66FVHSMH0P60WK173YQ  sqr 1023456789DRTAINWaFJCHLYMQPGEBZVOKXSbU
 3547.633 s Testcount :  1242398966051    45968761743887
completed in 3547.634 seconds
 Num 1011L10EZ7510RFTU2Ia  sqr 1023456789ABCDEFGHIKb22ISU7MJC5GAPVLY39
Base 39 test every 38 threads = 12
Start  : Num 1011L10EZ7510RFTU2J0  sqr 1023456789ABCDEFGHIQb8BRYWIcN3BKJ3F7A00
    1.00 min     795117225457
    2.00 min    1589942025409
..398.00 min  315778732951561
  399.00 min  316570538706841
Result : Num 1011L10EZ76L0a5UAJOF  sqr 1023456789DCFaKJPGLcEVSIBYZRTOMAbQHWXNU
23998.810 s Testcount :  8310508262457   315799313973366 // old version 68000s

```



## Perl


{{libheader|ntheory}}

```perl
use strict;
use warnings;
use feature 'say';
use ntheory qw/fromdigits todigitstring/;
use utf8;
binmode('STDOUT', 'utf8');

sub first_square  {
    my $n = shift;
    my $sr = substr('1023456789abcdef',0,$n);
    my $r  = int fromdigits($sr, $n) ** .5;
    my @digits = reverse split '', $sr;
    TRY: while (1) {
        my $sq = $r * $r;
        my $cnt = 0;
        my $s = todigitstring($sq, $n);
        my $i = scalar @digits;
        for (@digits) {
            $r++ and redo TRY if (-1 == index($s, $_)) || ($i-- + $cnt < $n);
            last if $cnt++ == $n;
        }
        return sprintf "Base %2d: %10s² == %s", $n, todigitstring($r, $n),
               todigitstring($sq, $n);
    }
}

say "First perfect square with N unique digits in base N: ";
say first_square($_) for 2..16;
```

{{out}}

```txt
First perfect square with N unique digits in base N:
Base  2:         10² == 100
Base  3:         22² == 2101
Base  4:         33² == 3201
Base  5:        243² == 132304
Base  6:        523² == 452013
Base  7:       1431² == 2450361
Base  8:       3344² == 13675420
Base  9:      11642² == 136802574
Base 10:      32043² == 1026753849
Base 11:     111453² == 1240a536789
Base 12:     3966b9² == 124a7b538609
Base 13:    3828943² == 10254773ca86b9
Base 14:    3a9db7c² == 10269b8c57d3a4
Base 15:   1012b857² == 102597bace836d4
Base 16:   404a9d9b² == 1025648cfea37bd9
```


Alternative solution:

{{libheader|ntheory}}

```perl
use strict;
use warnings;
use ntheory qw(:all);
use List::Util qw(uniq);

sub first_square {
    my ($base) = @_;

    my $start = sqrtint(fromdigits([1, 0, 2 .. $base-1], $base));

    for (my $k = $start ; ; ++$k) {
        if (uniq(todigits($k * $k, $base)) == $base) {
            return $k * $k;
        }
    }
}

foreach my $n (2 .. 16) {
    my $s = first_square($n);
    printf("Base %2d: %10s² == %s\n", $n,
        todigitstring(sqrtint($s), $n), todigitstring($s, $n));
}
```



## Perl 6

{{works with|Rakudo|2019.03}}
As long as you have the patience, this will work for bases 2 through 36.

Bases 2 through 19 finish quickly, (about 10 seconds on my system), 20 takes a while, 21 is pretty fast, 22 is glacial. 23 through 26 takes several hours.

Use analytical start value filtering based on observations by [[User:Hout|Hout]]++
and [[User:Nigel Galloway|Nigel Galloway]]++ on the
[[Talk:First_perfect_square_in_base_N_with_N_unique_digits#Space_compression_and_proof_.3F|discussion page]].

[https://tio.run/##dVTNbtpAEL7zFBPXBDuBlQ1SqkDz0x4i5VBeoGqQA@uwlVmb3XVThMi5fZ1ce@NR@iJ0Zm1jO1UtYS8zO9/8fDOTcZVcHA65FAZ0/gifP95PwXONWHEFV3AXJZr7k06HdLFQ2gz0Oo8UB@9eGnClD9sO4LPawK02kTJoFCeRgV7Y60MvwJc3BMYe6O7@N3uMNB/jGSHJTMR4hmsIA9jCO4hklGyMmGurLHFdlaYGEHchnoSJkgH990p37FsqZB/GhOtROBhsw/aW7mq09YaMoZqtosw727/awxh9tjDdGatwWpCwq0EpYhvQn5@/KvjtUVuFnMax5lSLlZBecYs9KY4uzzDbAsCHQXGatMyLvL6E5yXIV0Sp8F7@1dbGu07xrvhwKz4alWJZpDQfVKkxvUbFnItEyKfJkUhbFKqa4t853gfv4S17Rx/r@izTZ6IJP7WMGgllQcNCFec4VVWIjGFZ6iIiKJqw/WuLSpcCQtWRorY2rv@6Md4MsZkWkETaQC4TrjXasnkqTSSkxhxmNoAy1Ta9RfOfnpateWmPM@h2IZwFQUC/N5QXaZ7b3InVYwmqR0cbcKaoGANs3dlu/1okAx9cfQ3YiF5t6TOV5nLhsSAI/Z124KUFRY9DJtZp6y7wJMo0Xzht55aYBi91s9Aj@Q9j025WUNPwu7NaQoXs1IY6U0Ka2APnE7IB3eFiDN1wpCmvK@gORoF2@ui4T1CNoWryhwNQZ1atnJub8gjn8J@SwMkJ9HpI/65YS80RRmP7ERrmabbBKXbJHfVDUK2q4sZV8cWWWD3arbAdjy5wA/g7nIp8VUZJbx@elyLh1f0lThC1xaQGa02VNSmCQ9LBuaOtCRlXMZ/jji2257MwS5hiZ4p1zqGcNyHBBjsdAzJI1uy0tXOpY2m5kmdaqhAO@7g1FV/nQvEFisORFV@QOM2MSHGhkvi9FV@SWBvFzXzZmRwOfwE Try it online!]


```perl6
#`[

Only search square numbers that have at least N digits;
smaller could not possibly match.

Only bother to use analytics for large N. Finesse takes longer than brute force for small N.

]

unit sub MAIN ($timer = False);

sub first-square (Int $n) {
    my @start = flat '1', '0', (2 ..^ $n)».base: $n;

    if $n > 10 { # analytics
        my $root  = digital-root( @start.join, :base($n) );
        my @roots = (2..$n).map(*²).map: { digital-root($_.base($n), :base($n) ) };
        if $root ∉ @roots {
            my $offset = min(@roots.grep: * > $root ) - $root;
            @start[1+$offset] = $offset ~ @start[1+$offset];
        }
    }

    my $start = @start.join.parse-base($n).sqrt.ceiling;
    my @digits = reverse (^$n)».base: $n;
    my $sq;
    my $now  = now;
    my $time = 0;
    my $sr;
    for $start .. * {
        $sq = .²;
        my $s = $sq.base($n);
        my $f;
        $f = 1 and last unless $s.contains: $_ for @digits;
        if $timer && $n > 19 && $_ %% 1_000_000 {
            $time += now - $now;
            say "N $n:  {$_}² = $sq <$s> : {(now - $now).round(.001)}s" ~
                " : {$time.round(.001)} elapsed";
            $now = now;
        }
        next if $f;
        $sr = $_;
        last
    }
    sprintf( "Base %2d: %13s² == %-30s", $n, $sr.base($n), $sq.base($n) ) ~
        ($timer ?? ($time + now - $now).round(.001) !! '');
}

sub digital-root ($root is copy, :$base = 10) {
    $root = $root.comb.map({:36($_)}).sum.base($base) while $root.chars > 1;
    $root.parse-base($base);
}

say  "First perfect square with N unique digits in base N: ";
say .&first-square for flat
   2 .. 12, # required
  13 .. 16, # optional
  17 .. 19, # stretch
  20, # slow
  21, # pretty fast
  22, # very slow
  23, # don't hold your breath
  24, # slow but not too terrible
  25, # very slow
  26, #   "
;
```

{{out}}

```txt
First perfect square with N unique digits in base N:
Base  2:            10² == 100
Base  3:            22² == 2101
Base  4:            33² == 3201
Base  5:           243² == 132304
Base  6:           523² == 452013
Base  7:          1431² == 2450361
Base  8:          3344² == 13675420
Base  9:         11642² == 136802574
Base 10:         32043² == 1026753849
Base 11:        111453² == 1240A536789
Base 12:        3966B9² == 124A7B538609
Base 13:       3828943² == 10254773CA86B9
Base 14:       3A9DB7C² == 10269B8C57D3A4
Base 15:      1012B857² == 102597BACE836D4
Base 16:      404A9D9B² == 1025648CFEA37BD9
Base 17:     423F82GA9² == 101246A89CGFB357ED
Base 18:     44B482CAD² == 10236B5F8EG4AD9CH7
Base 19:    1011B55E9A² == 10234DHBG7CI8F6A9E5
Base 20:    49DGIH5D3G² == 1024E7CDI3HB695FJA8G
Base 21:   4C9HE5FE27F² == 1023457DG9HI8J6B6KCEAF
Base 22:   4F94788GJ0F² == 102369FBGDEJ48CHI7LKA5
Base 23:  1011D3EL56MC² == 10234ACEDKG9HM8FBJIL756
Base 24:  4LJ0HDGF0HD3² == 102345B87HFECKJNIGMDLA69
Base 25: 1011E145FHGHM² == 102345DOECKJ6GFB8LIAM7NH9
Base 26: 52K8N53BDM99K² == 1023458LO6IEMKG79FPCHNJDBA
```



## Phix

{{libheader|mpfr}}
Partial translation of VB with bitmap idea from C++ and adopting the digit-array approach from pascal
instead of base conversion.

```Phix
-- demo\rosetta\PandigitalSquares.exw
include mpfr.e
atom t0 = time()
constant chars = "0123456789abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz|",
         use_hll_code = true

function str_conv(sequence s, integer mode=+1)
-- mode of +1: eg {1,2,3} -> "123", mode of -1 the reverse.
-- note this doesn't really care what base s/res are in.
    {sequence res,integer dcheck} = iff(mode=+1?{"",9}:{{},'9'})
    for i=1 to length(s) do
        integer d = s[i]
        d += mode*iff(d>dcheck?'a'-10:'0')
        res &= d
    end for
    return res
end function

procedure do_one(integer base)
-- tabulates one base
    integer bm1 = base-1,
            dr = iff(and_bits(base,1) ? floor(base/2) : 0),
            id = 0,
            rc = 0,
            sdri
    atom st = time()
    sequence sdr = repeat(0,bm1)
    for i=0 to bm1-1 do
        sdri = mod(i*i,bm1)
        rc += (sdri==dr)
        sdr[i+1] = iff(sdri=0 ? bm1 : sdri)
    end for
    if dr>0 then
        id = base
        for i=1 to dr do
            sdri = sdr[i+1]
            if sdri>=dr
            and id>sdri then
                id = sdri
            end if
        end for
        id -= dr
    end if
    string sq = chars[1..base]
    if id>0 then sq = sq[1..id]&chars[id+1]&sq[id+1..$] end if
    sq[1..2] = "10"
    mpz sqz = mpz_init(),
        rtz = mpz_init(),
        dnz = mpz_init(),
        tmp = mpz_init()
    mpz_set_str(sqz,sq,base)
    mpz_sqrt(rtz,sqz)
    mpz_add_ui(rtz,rtz,1)       -- rtz = sqrt(sqz)+1
    mpz_mul_si(dnz,rtz,2)
    mpz_add_si(dnz,dnz,1)       -- dnz = rtz*2+1
    mpz_mul(sqz,rtz,rtz)        -- sqz = rtz * rtz
    integer d = 1,
            inc = 1
    if base>3 and rc>0 then
        while mpz_fdiv_ui(sqz,bm1)!=dr do
            -- align sqz to dr
            mpz_add_ui(rtz,rtz,1)   -- rtz += 1
            mpz_add(sqz,sqz,dnz)    -- sqz += dnz
            mpz_add_ui(dnz,dnz,2)   -- dnz += 2
        end while
        inc = floor(bm1/rc)
        if inc>1 then
            mpz_mul_si(tmp,rtz,inc-2)
            mpz_sub_ui(tmp,tmp,1)
            mpz_add(dnz,dnz,tmp)    -- dnz += rtz*(inc-2)-1
        end if
        d = inc * inc
        mpz_add(dnz,dnz,dnz)
        mpz_add_ui(dnz,dnz,d)       -- dnz += dnz + d
    end if
    d *= 2
    atom mask, fullmask = power(2,base)-1 -- ie 0b111..
    integer icount = 0
    mpz_set_si(tmp,d)
    sequence sqi = str_conv(mpz_get_str(sqz,base), mode:=-1),
             dni = str_conv(mpz_get_str(dnz,base), mode:=-1),
             dti = str_conv(mpz_get_str(tmp,base), mode:=-1)
    while true do
        if use_hll_code then
            mask = 0
            for i=1 to length(sqi) do
                mask = or_bits(mask,power(2,sqi[i]))
            end for
        else
            ?9/0 -- see below, inline part 1
        end if
        if mask=fullmask then exit end if
        integer carry = 0, sidx, si
        if use_hll_code then
            for sidx=-1 to -length(dni) by -1 do
                si = sqi[sidx]+dni[sidx]+carry
                carry = si>=base
                sqi[sidx] = si-carry*base
            end for
            sidx += length(sqi)+1
            while carry and sidx do
                si = sqi[sidx]+carry
                carry = si>=base
                sqi[sidx] = si-carry*base
                sidx -= 1
            end while
        else
            ?9/0 --see below, inline part 2
        end if
        if carry then
            sqi = carry&sqi
        end if
        carry = 0
        for sidx=-1 to -length(dti) by -1 do
            si = dni[sidx]+dti[sidx]+carry
            carry = floor(si/base)
            dni[sidx] = remainder(si,base)
        end for
        sidx += length(dni)+1
        while carry and sidx do
            si = dni[sidx]+carry
            carry = si>=base
            dni[sidx] = si-carry*base
            sidx -= 1
        end while
        if carry then
            dni = carry&dni
        end if
        icount += 1
    end while
    mpz_set_si(tmp,icount)
    mpz_mul_si(tmp,tmp,inc)
    mpz_add(rtz,rtz,tmp)            -- rtz += icount * inc
    sq = str_conv(sqi, mode:=+1)
    string rt = mpz_get_str(rtz,base),
           idstr = iff(id?sprintf("%d",id):" "),
           ethis = elapsed_short(time()-st),
           etotal = elapsed_short(time()-t0)
    printf(1,"%3d %3d %s %18s -> %-28s %10d %8s     %8s\n",
             {base, inc, idstr, rt, sq, icount, ethis, etotal})
    {sqz,rtz,dnz,tmp} = mpz_free({sqz,rtz,dnz,tmp})
end procedure

puts(1,"base inc id             root -> square" &
        "                       test count    time        total\n")
for base=2 to 19 do
--for base=2 to 25 do
--for base=2 to 28 do
    do_one(base)
end for
printf(1,"completed in %s\n", {elapsed(time()-t0)})
```

{{out}}

```txt

base inc id             root -> square                       test count    time        total
  2   1                   10 -> 100                                   0       0s           0s
  3   1                   22 -> 2101                                  4       0s           0s
  4   3                   33 -> 3201                                  2       0s           0s
  5   1 2                243 -> 132304                               14       0s           0s
  6   5                  523 -> 452013                               20       0s           0s
  7   6                 1431 -> 2450361                              34       0s           0s
  8   7                 3344 -> 13675420                             41       0s           0s
  9   4                11642 -> 136802574                           289       0s           0s
 10   3                32043 -> 1026753849                           17       0s           0s
 11  10               111453 -> 1240a536789                        1498       0s           0s
 12  11               3966b9 -> 124a7b538609                       6883       0s           0s
 13   1 3            3828943 -> 10254773ca86b9                     8242       0s           0s
 14  13              3a9db7c -> 10269b8c57d3a4                     1330       0s           0s
 15  14             1012b857 -> 102597bace836d4                    4216       0s           0s
 16  15             404a9d9b -> 1025648cfea37bd9                  18457       0s           0s
 17   1 1          423f82ga9 -> 101246a89cgfb357ed               195112       0s           0s
 18  17            44b482cad -> 10236b5f8eg4ad9ch7                30440       0s           0s
 19   6           1011b55e9a -> 10234dhbg7ci8f6a9e5               93021       0s           0s
completed in 0.5s

```

Performance drops significantly after that:

```txt

 20  19           49dgih5d3g -> 1024e7cdi3hb695fja8g           11310604       9s          10s
 21   1 6        4c9he5fe27f -> 1023457dg9hi8j6b6kceaf           601843       0s          10s
 22  21          4f94788gj0f -> 102369fbgdej48chi7lka5         27804949      25s          36s
 23  22         1011d3el56mc -> 10234acedkg9hm8fbjil756        17710217      17s          53s
 24  23         4lj0hdgf0hd3 -> 102345b87hfeckjnigmdla69        4266555       4s          58s
 25  12        1011e145fhghm -> 102345doeckj6gfb8liam7nh9      78092125     1:16         2:14
completed in 2 minutes and 15s

```

It takes a little over half an hour to get to 28. We can use "with profile_time" to identify

a couple of hotspots and replace them with inline assembly (setting use_hll_code to false).

[This is probably quite a good target for improving the quality of the generated code.]

Requires version 0.8.1+, not yet shipped, which will include demo\rosetta\PandigitalSquares.exw

64 bit code omitted for clarity, the code in PandigitalSquares.exw is twice as long.

```Phix
--          ?9/0 -- see below, inline part 1
            mask = length(sqi)
            #ilASM{
                    mov esi,[sqi]
                    mov edx,[mask]
                    shl esi,2
                    xor eax,eax
                  @@:
                    mov edi,1
                    mov cl,[esi]
                    shl edi,cl
                    add esi,4
                    or eax,edi
                    sub edx,1
                    jnz @b
                    mov [mask],eax
                  }
--and
--          ?9/0 --see below, inline part 2
            if length(dni)=length(sqi) then
                sqi = 0&sqi
            end if
            #ilASM{
                    mov esi,[sqi]
                    mov edi,[dni]
                    mov ecx,[ebx+esi*4-12]  -- length(sqi)
                    mov edx,[ebx+edi*4-12]  -- length(dni)
                    lea esi,[esi+ecx-1]
                    lea edi,[edi+edx-1]
                    sub ecx,edx
                    xor eax,eax
                    lea esi,[ebx+esi*4]     -- locate sqi[$]
                    lea edi,[ebx+edi*4]     -- locate dni[$]
                    push ecx
                    mov ecx,[base]
                  @@:
                    add eax,[esi]
                    add eax,[edi]
                    div cl
                    mov [esi],ah
                    xor ah,ah
                    sub esi,4
                    sub edi,4
                    sub edx,1
                    jnz @b
                    pop edx
                  @@:
                    test eax,eax
                    jz @f
                    add eax,[esi]
                    div cl
                    mov [esi],ah
                    xor ah,ah
                    sub esi,4
                    sub edx,1
                    jnz @b
                  @@:
                    mov [carry],eax
                  }
```

{{output}}

```txt

 20  19           49dgih5d3g -> 1024e7cdi3hb695fja8g           11310604       2s           3s
 21   1 6        4c9he5fe27f -> 1023457dg9hi8j6b6kceaf           601843       0s           3s
 22  21          4f94788gj0f -> 102369fbgdej48chi7lka5         27804949       7s          10s
 23  22         1011d3el56mc -> 10234acedkg9hm8fbjil756        17710217       4s          14s
 24  23         4lj0hdgf0hd3 -> 102345b87hfeckjnigmdla69        4266555       1s          15s
 25  12        1011e145fhghm -> 102345doeckj6gfb8liam7nh9      78092125      18s          34s
completed in 34.3s

```

It takes 7 minutes and 20s to get to 28, still not quite up to the frankly astonishing <strike>27s</strike> 12.3s of pascal, but getting there.


## Python

{{Works with|Python|3.7}}

```python
'''Perfect squares using every digit in a given base.'''

from itertools import (count, dropwhile, repeat)
from math import (ceil, sqrt)
from time import time


# allDigitSquare :: Int -> Int -> Int
def allDigitSquare(base, above):
    '''The lowest perfect square which
       requires all digits in the given base.
    '''
    bools = list(repeat(True, base))
    return next(dropwhile(missingDigitsAtBase(base, bools), count(
        max(above, ceil(sqrt(int('10' + '0123456789abcdef'[2:base], base))))
    )))


# missingDigitsAtBase :: Int -> [Bool] -> Int -> Bool
def missingDigitsAtBase(base, bools):
    '''Fusion of representing the square of integer N at a given base
       with checking whether all digits of that base contribute to N^2.
       Clears the bool at a digit position to False when used.
       True if any positions remain uncleared (unused).
    '''
    def go(x):
        xs = bools.copy()
        while x:
            xs[x % base] = False
            x //= base
        return any(xs)
    return lambda n: go(n * n)


# digit :: Int -> Char
def digit(n):
    '''Digit character for given integer.'''
    return '0123456789abcdef'[n]


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Smallest perfect squares using all digits in bases 2-16'''

    start = time()

    print(main.__doc__ + ':\n\nBase      Root    Square')
    q = 0
    for b in enumFromTo(2)(16):
        q = allDigitSquare(b, q)
        print(
            str(b).rjust(2, ' ') + ' -> ' +
            showIntAtBase(b)(digit)(q)('').rjust(8, ' ') + ' -> ' +
            showIntAtBase(b)(digit)(q * q)('')
        )

    print(
        '\nc. ' + str(ceil(time() - start)) + ' seconds.'
    )


# GENERIC -------------------------------------------------

# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# showIntAtBase :: Int -> (Int -> String) -> Int -> String -> String
def showIntAtBase(base):
    '''String representation of an integer in a given base,
       using a supplied function for the string representation
       of digits.
    '''
    def wrap(toChr, n, rs):
        def go(nd, r):
            n, d = nd
            r_ = toChr(d) + r
            return go(divmod(n, base), r_) if 0 != n else r_
        return 'unsupported base' if 1 >= base else (
            'negative number' if 0 > n else (
                go(divmod(n, base), rs))
        )
    return lambda toChr: lambda n: lambda rs: (
        wrap(toChr, n, rs)
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Smallest perfect squares using all digits in bases 2-16:

Base      Root    Square
 2 ->       10 -> 100
 3 ->       22 -> 2101
 4 ->       33 -> 3201
 5 ->      243 -> 132304
 6 ->      523 -> 452013
 7 ->     1431 -> 2450361
 8 ->     3344 -> 13675420
 9 ->    11642 -> 136802574
10 ->    32043 -> 1026753849
11 ->   111453 -> 1240a536789
12 ->   3966b9 -> 124a7b538609
13 ->  3828943 -> 10254773ca86b9
14 ->  3a9db7c -> 10269b8c57d3a4
15 -> 1012b857 -> 102597bace836d4
16 -> 404a9d9b -> 1025648cfea37bd9

c. 30 seconds.
```



## REXX

The   '''REXX'''   language doesn't have
a   '''sqrt'''   function,   nor does it have a general purpose
radix (base) convertor,

so RYO versions were included here.

These REXX versions can handle up to base '''36''', but could be extended.

### slightly optimized


```rexx
/*REXX program finds/displays the first perfect square with  N  unique digits in base N.*/
numeric digits 40                                /*ensure enough decimal digits for a #.*/
parse arg n .                                    /*obtain optional argument from the CL.*/
if n=='' | n==","  then n= 16                    /*not specified?  Then use the default.*/
@start= 1023456789abcdefghijklmnopqrstuvwxyz     /*contains the start # (up to base 36).*/
                           w= length(n)          /* [↓]  find the smallest square with  */
    do j=2  to n;          beg= left(@start, j)  /*      N  unique digits in base  N.   */
       do k=iSqrt( base(beg,10,j) )  until #==0  /*start each search from smallest sqrt.*/
       $= base(k*k, j, 10)                       /*calculate square, convert to base J. */
       $u= $;              upper $u              /*get an uppercase version fast count. */
       #= verify(beg, $u)                        /*count differences between 2 numbers. */
       end   /*k*/
    say 'base'  right(j,w)   "   root="   right(base(k,j,10),max(5,n))    '   square='   $
    end      /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
base: procedure;  arg x 1 #,toB,inB              /*obtain:  three arguments.            */
      @l= '0123456789abcdefghijklmnopqrstuvwxyz' /*lowercase (Latin or English) alphabet*/
      @u= @l;     upper @u                       /*uppercase    "    "    "         "   */
      if inb\==10  then                          /*only convert if  not  base 10.       */
         do;  #= 0                               /*result of converted  X  (in base 10).*/
           do j=1  for length(x)                 /*convert  X:   base inB  ──► base 10. */
           #= # * inB + pos(substr(x,j,1), @u)-1 /*build a new number,  digit by digit. */
           end    /*j*/                          /* [↑]  this also verifies digits.     */
         end
      y=                                         /*the value of  X  in base  B (so far).*/
      if tob==10  then return #                  /*if TOB is ten,  then simply return #.*/
         do  while  # >= toB                     /*convert #:    base 10  ──►  base toB.*/
         y= substr(@l, (# // toB) + 1, 1)y       /*construct the output number.         */
         #= # % toB                              /*      ··· and whittle  #  down also. */
         end    /*while*/                        /* [↑]  algorithm may leave a residual.*/
      return substr(@l, # + 1, 1)y               /*prepend the residual, if any.        */
/*──────────────────────────────────────────────────────────────────────────────────────*/
iSqrt: procedure; parse arg x;  r=0;  q=1;             do while q<=x;  q=q*4;  end
        do while q>1; q=q%4; _=x-r-q; r=r%2; if _>=0 then do;x=_;r=r+q; end; end; return r
```

{{out|output|text=  when using the default input:}}

```txt

base  2    root=           10    square= 100
base  3    root=           22    square= 2101
base  4    root=           33    square= 3201
base  5    root=          243    square= 132304
base  6    root=          523    square= 452013
base  7    root=         1431    square= 2450361
base  8    root=         3344    square= 13675420
base  9    root=        11642    square= 136802574
base 10    root=        32043    square= 1026753849
base 11    root=       111453    square= 1240a536789
base 12    root=       3966b9    square= 124a7b538609
base 13    root=      3828943    square= 10254773ca86b9
base 14    root=      3a9db7c    square= 10269b8c57d3a4
base 15    root=     1012b857    square= 102597bace836d4
base 16    root=     404a9d9b    square= 1025648cfea37bd9

```



### more optimized

This REXX version uses a highly optimized   '''base'''   function since it was that particular function that was consuming the majority of the CPU time.

It is about   '''10%'''   faster.

```rexx
/*REXX program finds/displays the first perfect square with  N  unique digits in base N.*/
numeric digits 40                                /*ensure enough decimal digits for a #.*/
parse arg n .                                    /*obtain optional argument from the CL.*/
if n=='' | n==","  then n= 16                    /*not specified?  Then use the default.*/
@start= 1023456789abcdefghijklmnopqrstuvwxyz     /*contains the start # (up to base 36).*/
call base                                        /*initialize 2 arrays for BASE function*/
                                                 /* [↓]  find the smallest square with  */
    do j=2  to n;          beg= left(@start, j)  /*      N  unique digits in base  N.   */
       do k=iSqrt( base(beg,10,j) )  until #==0  /*start each search from smallest sqrt.*/
       $= base(k*k, j, 10)                       /*calculate square, convert to base J. */
       #= verify(beg, $)                         /*count differences between 2 numbers. */
       end   /*k*/
    say 'base'            right(j, length(n) )                    "   root="   ,
                   lower( right( base(k, j, 10), max(5, n) ) )    '   square='    lower($)
    end      /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
base: procedure expose !. !!.; arg x 1 #,toB,inB /*obtain:  three arguments.            */
      @= 0123456789abcdefghijklmnopqrstuvwxyz    /*the characters for the Latin alphabet*/
      if x==''  then do i=1  for length(@);   _= substr(@, i, 1);    m= i - 1;    !._= m
                     !!.m= substr(@, i, 1)
                     if i==length(@) then return /*Done with shortcuts?  Then go back.  */
                     end   /*i*/                 /* [↑]  assign shortcut radix values.  */
      if inb\==10  then                          /*only convert if  not  base 10.       */
         do;  #= 0                               /*result of converted  X  (in base 10).*/
           do j=1  for length(x)                 /*convert  X:   base inB  ──► base 10. */
           _= substr(x, j, 1);  #= # * inB + !._ /*build a new number,  digit by digit. */
           end    /*j*/                          /* [↑]  this also verifies digits.     */
         end
      y=                                         /*the value of  X  in base  B (so far).*/
      if tob==10  then return #                  /*if TOB is ten,  then simply return #.*/
         do  while  # >= toB                     /*convert #:    base 10  ──►  base toB.*/
         _= # // toB;           y= !!._ || y     /*construct the output number.         */
         #= # % toB                              /*      ··· and whittle  #  down also. */
         end    /*while*/                        /* [↑]  algorithm may leave a residual.*/
      return !!.# || y                           /*prepend the residual, if any.        */
/*──────────────────────────────────────────────────────────────────────────────────────*/
iSqrt: procedure; parse arg x;  r=0;  q=1;             do while q<=x;  q=q*4;  end
        do while q>1; q=q%4; _=x-r-q; r=r%2; if _>=0 then do;x=_;r=r+q; end; end; return r
/*──────────────────────────────────────────────────────────────────────────────────────*/
lower: @abc= 'abcdefghijklmnopqrstuvwxyz'; return translate(arg(1), @abc, translate(@abc))
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}




## Ruby

Takes about 15 seconds on my dated PC, most are spent calculating base 13.

```ruby
DIGITS = "1023456789abcdefghijklmnopqrstuvwxyz"

2.upto(16) do |n|
  start = Integer.sqrt( DIGITS[0,n].to_i(n) )
  res = start.step.detect{|i| (i*i).digits(n).uniq.size == n }
  puts "Base %2d:%10s² = %-14s" % [n, res.to_s(n), (res*res).to_s(n)]
end

```

{{out}}

```txt
Base  2:        10² = 100
Base  3:        22² = 2101
Base  4:        33² = 3201
Base  5:       243² = 132304
Base  6:       523² = 452013
Base  7:      1431² = 2450361
Base  8:      3344² = 13675420
Base  9:     11642² = 136802574
Base 10:     32043² = 1026753849
Base 11:    111453² = 1240a536789
Base 12:    3966b9² = 124a7b538609
Base 13:   3828943² = 10254773ca86b9
Base 14:   3a9db7c² = 10269b8c57d3a4
Base 15:  1012b857² = 102597bace836d4
Base 16:  404a9d9b² = 1025648cfea37bd9

```


## Sidef


```ruby
func first_square(b) {

    var start = [1, 0, (2..^b)...].flip.map_kv{|k,v| v * b**k }.sum.isqrt

    start..Inf -> first_by {|k|
        k.sqr.digits(b).freq.len == b
    }.sqr
}

for b in (2..16) {
    var s = first_square(b)
    printf("Base %2d: %10s² == %s\n", b, s.isqrt.base(b), s.base(b))
}
```

{{out}}

```txt

Base  2:         10² == 100
Base  3:         22² == 2101
Base  4:         33² == 3201
Base  5:        243² == 132304
Base  6:        523² == 452013
Base  7:       1431² == 2450361
Base  8:       3344² == 13675420
Base  9:      11642² == 136802574
Base 10:      32043² == 1026753849
Base 11:     111453² == 1240a536789
Base 12:     3966b9² == 124a7b538609
Base 13:    3828943² == 10254773ca86b9
Base 14:    3a9db7c² == 10269b8c57d3a4
Base 15:   1012b857² == 102597bace836d4
Base 16:   404a9d9b² == 1025648cfea37bd9

```



## Visual Basic .NET

{{libheader|System.Numerics}}
This is faster than the Go version, but not as fast as the Pascal version.  The Pascal version uses an array of integers to represent the square, as it's more efficient to increment and check that way.<br/>This Visual Basic .NET version uses BigInteger variables for computation. It's quick enough for up to base19, tho.
```vbnet
Imports System.Numerics

Module Program
    Dim base, bm1 As Byte, hs As New HashSet(Of Byte), st0 As DateTime
    Const chars As String = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz|"

    ' converts base10 to string, using current base
    Function toStr(ByVal b As BigInteger) As String
        toStr = "" : Dim re As BigInteger : While b > 0
            b = BigInteger.DivRem(b, base, re) : toStr = chars(CByte(re)) & toStr : End While
    End Function

    ' checks for all digits present, checks every one (use when extra digit is present)
    Function allIn(ByVal b As BigInteger) As Boolean
        Dim re As BigInteger : hs.Clear() : While b > 0 : b = BigInteger.DivRem(b, base, re)
            hs.Add(CByte(re)) : End While : Return hs.Count = base
    End Function

    ' checks for all digits present, bailing when duplicates occur (can't use when extra digit is present)
    Function allInQ(ByVal b As BigInteger) As Boolean
        Dim re As BigInteger, c As Integer = 0 : hs.Clear() : While b > 0 : b = BigInteger.DivRem(b, base, re)
            hs.Add(CByte(re)) : c += 1 : If c <> hs.Count Then Return False
        End While : Return True
    End Function

    ' converts string to base 10, using current base
    Function to10(s As String) As BigInteger
        to10 = 0 : For Each i As Char In s : to10 = to10 * base + chars.IndexOf(i) : Next
    End Function

    ' returns minimum string representation, optionally inserting a digit
    Function fixup(n As Integer) As String
        fixup = chars.Substring(0, base)
        If n > 0 Then fixup = fixup.Insert(n, n.ToString)
        fixup = "10" & fixup.Substring(2)
    End Function

    ' returns close approx.
    Function IntSqRoot(v As BigInteger) As BigInteger
        IntSqRoot = New BigInteger(Math.Sqrt(CDbl(v))) : Dim term As BigInteger
        Do : term = v / IntSqRoot : If BigInteger.Abs(term - IntSqRoot) < 2 Then Exit Do
            IntSqRoot = (IntSqRoot + term) / 2 : Loop Until False
    End Function

    ' tabulates one base
    Sub doOne()
        bm1 = base - 1 : Dim dr As Byte = 0 : If (base And 1) = 1 Then dr = base >> 1
        Dim id As Integer = 0, inc As Integer = 1, i As Long = 0, st As DateTime = DateTime.Now
        Dim sdr(bm1 - 1) As Byte, rc As Byte = 0 : For i = 0 To bm1 - 1 : sdr(i) = (i * i) Mod bm1
            rc += If(sdr(i) = dr, 1, 0) : sdr(i) += If(sdr(i) = 0, bm1, 0) : Next : i = 0
        If dr > 0 Then
            id = base : For i = 1 To dr : If sdr(i) >= dr Then If id > sdr(i) Then id = sdr(i)
            Next : id -= dr : i = 0 : End If
        Dim sq As BigInteger = to10(fixup(id)), rt As BigInteger = IntSqRoot(sq) + 0,
            dn As BigInteger = (rt << 1) + 1, d As BigInteger = 1
        sq = rt * rt : If base > 3 AndAlso rc > 0 Then
            While sq Mod bm1 <> dr : rt += 1 : sq += dn : dn += 2 : End While ' alligns sq to dr
            inc = bm1 \ rc : If inc > 1 Then dn += rt * (inc - 2) - 1 : d = inc * inc
            dn += dn + d
        End If : d <<= 1 : If base > 5 AndAlso rc > 0 Then : Do : If allInQ(sq) Then Exit Do
                sq += dn : dn += d : i += 1 : Loop Until False : Else : Do : If allIn(sq) Then Exit Do
                sq += dn : dn += d : i += 1 : Loop Until False : End If : rt += i * inc
        Console.WriteLine("{0,3} {1,3} {2,2} {3,20} -> {4,-38} {5,10} {6,8:0.000}s   {7,8:0.000}s",
                          base, inc, If(id = 0, " ", id.ToString), toStr(rt), toStr(sq), i,
                          (DateTime.Now - st).TotalSeconds, (DateTime.Now - st0).TotalSeconds)
    End Sub

    Sub Main(args As String())
        st0 = DateTime.Now
        Console.WriteLine("base inc id                root    square" & _
            "                                 test count    time        total")
        For base = 2 To 28 : doOne() : Next
        Console.WriteLine("Elasped time was {0,8:0.00} minutes", (DateTime.Now - st0).TotalMinutes)
    End Sub
End Module
```

{{out}}This output is on a somewhat modern PC.  For comparison, it takes TIO.run around 30 seconds to reach base20, so TIO.run is around 3 times slower there.

```txt
base inc id                root    square                                 test count    time        total
  2   1                      10 -> 100                                             1    0.007s      0.057s
  3   1                      22 -> 2101                                            5    0.000s      0.057s
  4   3                      33 -> 3201                                            2    0.001s      0.058s
  5   1  2                  243 -> 132304                                         15    0.000s      0.058s
  6   5                     523 -> 452013                                         20    0.000s      0.059s
  7   6                    1431 -> 2450361                                        35    0.000s      0.059s
  8   7                    3344 -> 13675420                                       41    0.000s      0.059s
  9   4                   11642 -> 136802574                                     289    0.000s      0.059s
 10   3                   32043 -> 1026753849                                     17    0.000s      0.059s
 11  10                  111453 -> 1240A536789                                  1498    0.001s      0.060s
 12  11                  3966B9 -> 124A7B538609                                 6883    0.005s      0.065s
 13   1  3              3828943 -> 10254773CA86B9                               8243    0.013s      0.078s
 14  13                 3A9DB7C -> 10269B8C57D3A4                               1330    0.000s      0.078s
 15  14                1012B857 -> 102597BACE836D4                              4216    0.003s      0.081s
 16  15                404A9D9B -> 1025648CFEA37BD9                            18457    0.012s      0.093s
 17   1  1            423F82GA9 -> 101246A89CGFB357ED                         195113    0.341s      0.434s
 18  17               44B482CAD -> 10236B5F8EG4AD9CH7                          30440    0.022s      0.456s
 19   6              1011B55E9A -> 10234DHBG7CI8F6A9E5                         93021    0.068s      0.524s
 20  19              49DGIH5D3G -> 1024E7CDI3HB695FJA8G                     11310604    8.637s      9.162s
 21   1  6          4C9HE5FE27F -> 1023457DG9HI8J6B6KCEAF                     601844    1.181s     10.342s
 22  21             4F94788GJ0F -> 102369FBGDEJ48CHI7LKA5                   27804949   21.677s     32.020s
 23  22            1011D3EL56MC -> 10234ACEDKG9HM8FBJIL756                  17710217   14.292s     46.312s
 24  23            4LJ0HDGF0HD3 -> 102345B87HFECKJNIGMDLA69                  4266555    3.558s     49.871s
 25  12           1011E145FHGHM -> 102345DOECKJ6GFB8LIAM7NH9                78092125   69.914s    119.785s
 26   5           52K8N53BDM99K -> 1023458LO6IEMKG79FPCHNJDBA              402922569  365.929s    485.714s
 27  26          1011F11E37OBJJ -> 1023458ELOMDHBIJFGKP7CQ9N6A             457555293  420.607s    906.321s
 28   9          58A3CKP3N4CQD7 -> 1023456CGJBIRQEDHP98KMOAN7FL            749593055  711.660s   1617.981s
Elasped time was    26.97 minutes
```
Base29 seems to take an order of magnitude longer.  I'm looking into some shortcuts.


## zkl

{{trans|Julia}}

```zkl
fcn squareSearch(B){
   basenumerals:=B.pump(String,T("toString",B)); // 13 --> "0123456789abc"
   highest:=("10"+basenumerals[2,*]).toInt(B);   // 13 --> "10" "23456789abc"
   foreach n in ([highest.toFloat().sqrt().toInt() .. highest]){
     ns:=(n*n).toString(B);
     if(""==(basenumerals - ns) ) return(n.toString(B),ns);
  }
  Void
}
```


```zkl
println("Base     Root   N");
foreach b in ([2..16])
  { println("%2d %10s  %s".fmt(b,squareSearch(b).xplode())) }
```

{{out}}

```txt

Base     Root   N
 2         10  100
 3         22  2101
 4         33  3201
 5        243  132304
 6        523  452013
 7       1431  2450361
 8       3344  13675420
 9      11642  136802574
10      32043  1026753849
11     111453  1240a536789
12     3966b9  124a7b538609
13    3828943  10254773ca86b9
14    3a9db7c  10269b8c57d3a4
15   1012b857  102597bace836d4
16   404a9d9b  1025648cfea37bd9

```

