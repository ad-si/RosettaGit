+++
title = "Mersenne primes"
description = ""
date = 2019-10-20T01:52:42Z
aliases = []
[extra]
id = 21701
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
Mersenne primes:

Challenge: 

Create code that will list (preferably calculate) all of the Mersenne primes until some limitation is reached.  For information on what a Mersenne prime is, go to this link: [[https://en.wikipedia.org/wiki/Mersenne_prime]]


## AppleScript

<lang>
on isPrime(integ)
	set isComposite to ""
	if (integ / 2) = (integ / 2 div 1) then
		log integ & " is composite because 2 is a factor" as string --buttons {"OK", "Cancel"} default button 1 cancel button 2
		
	else
		set x to 2
		set sqrtOfInteg to integ ^ 0.5
		repeat until x = integ ^ 0.5 + 1 as integer
			if (integ / x) = integ / x div 1 then
				log integ & " is composite because " & x & " & " & (integ / x div 1) & " are factors" as string --buttons {"OK", "Cancel"} default button 1 cancel button 2
				set isComposite to 1
				set x to x + 1
			else
				
				set x to x + 1
			end if
			
			
			
		end repeat
		log integ & " is prime" as string --buttons {"OK", "Cancel"} default button 1 cancel button 2
		if isComposite = 1 then
			log integ & "is composite"
		else
			display dialog integ
		end if
	end if
	
end isPrime
set x to 2
repeat
	isPrime(((2 ^ x) - 1) div 1)
	set x to x + 1
end repeat

```



## C#

Needs a better primality checking algorithm to do really large prime numbers.

```csharp
using System;
using System.Numerics;

namespace MersennePrimes {
    class Program {
        static BigInteger Sqrt(BigInteger x) {
            if (x < 0) throw new ArgumentException("Negative argument.");
            if (x < 2) return x;
            BigInteger y = x / 2;
            while (y > x / y) {
                y = ((x / y) + y) / 2;
            }
            return y;
        }

        static bool IsPrime(BigInteger bi) {
            if (bi < 2) return false;
            if (bi % 2 == 0) return bi == 2;
            if (bi % 3 == 0) return bi == 3;
            if (bi % 5 == 0) return bi == 5;
            if (bi % 7 == 0) return bi == 7;
            if (bi % 11 == 0) return bi == 11;
            if (bi % 13 == 0) return bi == 13;
            if (bi % 17 == 0) return bi == 17;
            if (bi % 19 == 0) return bi == 19;

            BigInteger limit = Sqrt(bi);
            BigInteger test = 23;
            while (test < limit) {
                if (bi % test == 0) return false;
                test += 2;
                if (bi % test == 0) return false;
                test += 4;
            }

            return true;
        }

        static void Main(string[] args) {
            const int MAX = 9;

            int pow = 2;
            int count = 0;

            while (true) {
                if (IsPrime(pow)) {
                    BigInteger p = BigInteger.Pow(2, pow) - 1;
                    if (IsPrime(p)) {
                        Console.WriteLine("2 ^ {0} - 1", pow);
                        if (++count >= MAX) {
                            break;
                        }
                    }
                }
                pow++;
            }
        }
    }
}
```

{{out}}

```txt
2 ^ 2 - 1
2 ^ 3 - 1
2 ^ 5 - 1
2 ^ 7 - 1
2 ^ 13 - 1
2 ^ 17 - 1
2 ^ 19 - 1
2 ^ 31 - 1
2 ^ 61 - 1
```



## D

Simplest thing that could possibly work. Using better primality tests will allow for more results to be calculated in a reasonable amount of time.

```D
import std.bigint;
import std.stdio;

bool isPrime(BigInt bi) {
    if (bi < 2) return false;
    if (bi % 2 == 0) return bi == 2;
    if (bi % 3 == 0) return bi == 3;
    
    auto test = BigInt(5);
    while (test * test < bi) {
        if (bi % test == 0) return false;
        test += 2;
        if (bi % test == 0) return false;
        test += 4;
    }

    return true;
}

void main() {
    auto base = BigInt(2);

    for (int pow=1; pow<32; pow++) {
        if (isPrime(base-1)) {
            writeln("2 ^ ", pow, " - 1");
        }
        base *= 2;
    }
}
```

{{out}}

```txt
2 ^ 2 - 1
2 ^ 3 - 1
2 ^ 5 - 1
2 ^ 7 - 1
2 ^ 13 - 1
2 ^ 17 - 1
2 ^ 19 - 1
2 ^ 31 - 1
```



## F#

{{trans|C#}}

```fsharp
open System
open System.Numerics

let Sqrt (n:BigInteger) =
    if n < (BigInteger 0) then raise (ArgumentException "Negative argument.")
    if n < (BigInteger 2) then n
    else
        let rec H v r s =
            if v < s then
                r
            else
                H (v - s) (r + (BigInteger 1)) (s + (BigInteger 2))
        H n (BigInteger 0) (BigInteger 1)

let IsPrime (n:BigInteger) =
    if n < (BigInteger 2) then false
    elif n % (BigInteger 2) = (BigInteger 0) then n = (BigInteger 2)
    elif n % (BigInteger 3) = (BigInteger 0) then n = (BigInteger 3)
    elif n % (BigInteger 5) = (BigInteger 0) then n = (BigInteger 5)
    elif n % (BigInteger 7) = (BigInteger 0) then n = (BigInteger 7)
    elif n % (BigInteger 11) = (BigInteger 0) then n = (BigInteger 11)
    elif n % (BigInteger 13) = (BigInteger 0) then n = (BigInteger 13)
    elif n % (BigInteger 17) = (BigInteger 0) then n = (BigInteger 17)
    elif n % (BigInteger 19) = (BigInteger 0) then n = (BigInteger 19)
    else
        let limit = (Sqrt n)
        let rec H t =
            if t <= limit then
                if n % t = (BigInteger 0) then false
                else
                    let t2 = t + (BigInteger 2)
                    if n % t2 = (BigInteger 0) then false
                    else H (t2 + (BigInteger 4))
            else
                true
        H (BigInteger 23)

[<EntryPoint>]
let main _ =
    let MAX = BigInteger 9

    let rec loop (pow:int) (count:int) =
        if IsPrime (BigInteger pow) then
            let p = BigInteger.Pow((BigInteger 2), pow) - (BigInteger 1)
            if IsPrime p then
                printfn "2 ^ %A - 1" pow
                if (BigInteger (count + 1)) >= MAX then count
                else loop (pow + 1) (count + 1)
            else loop (pow + 1) count
        else loop (pow + 1) count

    loop 2 0 |> ignore

    0 // return an integer exit code
```

{{out}}

```txt
2 ^ 2 - 1
2 ^ 3 - 1
2 ^ 5 - 1
2 ^ 7 - 1
2 ^ 13 - 1
2 ^ 17 - 1
2 ^ 19 - 1
2 ^ 31 - 1
2 ^ 61 - 1
```



## Factor

Factor comes with a Lucas-Lehmer primality test.

```factor
USING: formatting math.primes.lucas-lehmer math.ranges sequences ;

: mersennes-upto ( n -- seq ) [1,b] [ lucas-lehmer ] filter ;

3500 mersennes-upto [ "2 ^ %d - 1\n" printf ] each
```

{{out}}

```txt

2 ^ 2 - 1
2 ^ 3 - 1
2 ^ 5 - 1
2 ^ 7 - 1
2 ^ 13 - 1
2 ^ 17 - 1
2 ^ 19 - 1
2 ^ 31 - 1
2 ^ 61 - 1
2 ^ 89 - 1
2 ^ 107 - 1
2 ^ 127 - 1
2 ^ 521 - 1
2 ^ 607 - 1
2 ^ 1279 - 1
2 ^ 2203 - 1
2 ^ 2281 - 1
2 ^ 3217 - 1

```



## Go

The <code>github.com/ncw/gmp</code> package is a drop-in replacement for Go's <code>math/big</code> package.
It's a CGo wrapper around the C GMP library and under these circumstances is two to four times as fast as the native Go package.
Editing just the import line you can use whichever is more convenient for you
(CGo has drawbacks, including limited portability).
Normally build tags would be used to control this instead of editing imports in the source, but this keeps the example simpler.
<!-- Note: The example code is the one using the builtin math/big so that anyone cut-n-pasting this example, or running it on the Go playground won't have issues. -->

Note that the use of ProbablyPrime(0) requires Go 1.8 or later. When using the <code>math/big</code> package, passing a parameter of zero to this method forces it to apply only the Baillie-PSW test to check for primality. This is 100% accurate for numbers up to 2^64 and at the time of writing (June 2018) no known composite number above that bound passes the test.

```go
package main

import (
	"fmt"
	"time"

	// Use one or the other of these:
	"math/big"
	//big "github.com/ncw/gmp"
)

func main() {
	start := time.Now()
	one := big.NewInt(1)
	mp := big.NewInt(0)
	bp := big.NewInt(0)
	const max = 22
	for count, p := 0, uint(2); count < max; {
		mp.Lsh(one, p)
		mp.Sub(mp, one)
		if mp.ProbablyPrime(0) {
			elapsed := time.Since(start).Seconds()
			if elapsed >= 0.01 {
				fmt.Printf("2 ^ %-4d - 1 took %6.2f secs\n", p, elapsed)
			} else {
				fmt.Printf("2 ^ %-4d - 1\n", p)
			}
			count++
		}
		for {
			if p > 2 {
				p += 2
			} else {
				p = 3
			}
			bp.SetUint64(uint64(p))
			if bp.ProbablyPrime(0) {
				break
			}
		}
	}
}
```


{{out|text=using the GMP package on a 3.4 GHz Xeon E3-1245:}}

```txt

2 ^ 2    - 1
2 ^ 3    - 1
2 ^ 5    - 1
2 ^ 7    - 1
2 ^ 13   - 1
2 ^ 17   - 1
2 ^ 19   - 1
2 ^ 31   - 1
2 ^ 61   - 1
2 ^ 89   - 1
2 ^ 107  - 1
2 ^ 127  - 1
2 ^ 521  - 1
2 ^ 607  - 1
2 ^ 1279 - 1 took   0.05 secs
2 ^ 2203 - 1 took   0.38 secs
2 ^ 2281 - 1 took   0.44 secs
2 ^ 3217 - 1 took   1.53 secs
2 ^ 4253 - 1 took   4.39 secs
2 ^ 4423 - 1 took   5.02 secs
2 ^ 9689 - 1 took  73.78 secs
2 ^ 9941 - 1 took  81.24 secs

```

(A previous run on more modest hardware - Celeron N3050 @ 1.60GHz × 2 - was ~365 seconds for M<sub>9941</sub>.)

This can be sped up quite a bit for modern multi-core CPUs by some simple changes to use goroutines.

```Go
package main

import (
	"fmt"
	"runtime"
	"time"

	// Use one or the other of these:
	"math/big"
	//big "github.com/ncw/gmp"
)

func main() {
	start := time.Now()

	nworkers := runtime.GOMAXPROCS(0)
	fmt.Println("Using", nworkers, "workers.")
	workC := make(chan uint, 1)
	resultC := make(chan uint, nworkers)

	// Generate possible Mersenne exponents and send them to workC.
	go func() {
		workC <- 2
		bp := big.NewInt(0)
		for p := uint(3); ; p += 2 {
			// Possible exponents must be prime.
			bp.SetUint64(uint64(p))
			if bp.ProbablyPrime(0) {
				workC <- p
			}
		}
	}()

	// Start up worker go routines, each takes
	// possible Mersenne exponents from workC as `p`
	// and if 2^p-1 is prime sends `p` to resultC.
	one := big.NewInt(1)
	for i := 0; i < nworkers; i++ {
		go func() {
			mp := big.NewInt(0)
			for p := range workC {
				mp.Lsh(one, p)
				mp.Sub(mp, one)
				if mp.ProbablyPrime(0) {
					resultC <- p
				}
			}
		}()
	}

	// Receive some maximum number of Mersenne prime exponents
	// from resultC and show the Mersenne primes.
	const max = 24
	for count := 0; count < max; count++ {
		// Note: these could come back out of order, although usually
		// only the first few. If that is an issue, correcting it is
		// left as an excercise to the reader :).
		p := <-resultC
		elapsed := time.Since(start).Seconds()
		if elapsed >= 0.01 {
			fmt.Printf("2 ^ %-5d - 1 took %6.2f secs\n", p, elapsed)
		} else {
			fmt.Printf("2 ^ %-5d - 1\n", p)
		}
	}
}
```

<!-- The output and the previous should be done on the same hardware to keep the numbers comparible; if changing/updating one change the other too. -->
{{out|text=using the GMP package on the same 3.4 GHz Xeon E3-1245 (4 core × 2 SMT threads) as above:}}

```txt
Using 8 workers.
2 ^ 2     - 1
2 ^ 5     - 1
2 ^ 3     - 1
2 ^ 7     - 1
2 ^ 13    - 1
2 ^ 19    - 1
2 ^ 61    - 1
2 ^ 31    - 1
2 ^ 107   - 1
2 ^ 17    - 1
2 ^ 127   - 1
2 ^ 89    - 1
2 ^ 521   - 1
2 ^ 607   - 1
2 ^ 1279  - 1 took   0.01 secs
2 ^ 2203  - 1 took   0.09 secs
2 ^ 2281  - 1 took   0.12 secs
2 ^ 3217  - 1 took   0.36 secs
2 ^ 4253  - 1 took   0.94 secs
2 ^ 4423  - 1 took   1.06 secs
2 ^ 9689  - 1 took  16.28 secs
2 ^ 9941  - 1 took  18.02 secs
2 ^ 11213 - 1 took  26.76 secs
2 ^ 19937 - 1 took 194.16 secs

```

Using this approach, the Celeron machine (dual core) takes ~180 seconds to reach M<sub>9941</sub> and ~270 seconds to reach M<sub>11213</sub>.


## Java

{{trans|Kotlin}}

```Java
import java.math.BigInteger;

public class MersennePrimes {
    private static final int MAX = 20;

    private static final BigInteger ONE = BigInteger.ONE;
    private static final BigInteger TWO = BigInteger.valueOf(2);

    private static boolean isPrime(int n) {
        if (n < 2) return false;
        if (n % 2 == 0) return n == 2;
        if (n % 3 == 0) return n == 3;
        int d = 5;
        while (d * d <= n) {
            if (n % d == 0) return false;
            d += 2;
            if (n % d == 0) return false;
            d += 4;
        }
        return true;
    }

    public static void main(String[] args) {
        int count = 0;
        int p = 2;
        while (true) {
            BigInteger m = TWO.shiftLeft(p - 1).subtract(ONE);
            if (m.isProbablePrime(10)) {
                System.out.printf("2 ^ %d - 1\n", p);
                if (++count == MAX) break;
            }
            // obtain next prime, p
            do {
                p = (p > 2) ? p + 2 : 3;
            } while (!isPrime(p));
        }
    }
}
```

{{out}}

```txt
2 ^ 2 - 1
2 ^ 3 - 1
2 ^ 5 - 1
2 ^ 7 - 1
2 ^ 13 - 1
2 ^ 17 - 1
2 ^ 19 - 1
2 ^ 31 - 1
2 ^ 61 - 1
2 ^ 89 - 1
2 ^ 107 - 1
2 ^ 127 - 1
2 ^ 521 - 1
2 ^ 607 - 1
2 ^ 1279 - 1
2 ^ 2203 - 1
2 ^ 2281 - 1
2 ^ 3217 - 1
2 ^ 4253 - 1
2 ^ 4423 - 1
```



## Julia

{{works with|Julia|0.6}}

Julia module <code>Primes</code> uses Miller-Rabin primality test.


```julia
using Primes

mersenne(n::Integer) = convert(typeof(n), 2) ^ n - one(n)
function main(nmax::Integer)
    n = ith = zero(nmax)
    while ith ≤ nmax
        if isprime(mersenne(n))
            println("M$n")
            ith += 1
        end
        n += 1
    end
end

main(big(20))
```


{{out}}

```txt
M2
M3
M5
M7
M13
M17
M19
M31
M61
M89
M107
M127
M521
M607
M1279
M2203
M2281
M3217
M4253
M4423
M9689
```



## Kotlin

This task is similar to the [[Lucas-Lehmer test]] task except that you can use whatever method you like to test the primality of the Mersenne numbers. Here, I've chosen to use the JDK's BigInteger.isProbablePrime(certainty) method.  The exact algorithm is implementation dependent --- GNU classpath uses only Miller-Rabin, while Oracle JDK uses Miller-Rabin and sometimes adds a Lucas test (this is ''not'' the Lucas-Lehmer test).

A 'certainty' parameter of 10 is enough to find the first 20 Mersenne primes but as even this takes about 90 seconds on my modest machine I've not bothered going beyond that.

```scala
// version 1.2.10

import java.math.BigInteger

const val MAX = 20

val bigOne = BigInteger.ONE
val bigTwo = 2.toBigInteger()

/* for checking 'small' primes */
fun isPrime(n: Int): Boolean {
    if (n < 2) return false
    if (n % 2 == 0) return n == 2
    if (n % 3 == 0) return n == 3
    var d : Int = 5
    while (d * d <= n) {
        if (n % d == 0) return false
        d += 2
        if (n % d == 0) return false
        d += 4
    }
    return true
}

fun main(args: Array<String>) {
    var count = 0
    var p = 2
    while (true) {
        val m = (bigTwo shl (p - 1)) - bigOne
        if (m.isProbablePrime(10)) {
            println("2 ^ $p - 1")
            if (++count == MAX) break
        }
        // obtain next prime, p
        while(true) {
            p = if (p > 2) p + 2 else 3
            if (isPrime(p)) break
        }
    }
}
```


{{out}}

```txt

2 ^ 2 - 1
2 ^ 3 - 1
2 ^ 5 - 1
2 ^ 7 - 1
2 ^ 13 - 1
2 ^ 17 - 1
2 ^ 19 - 1
2 ^ 31 - 1
2 ^ 61 - 1
2 ^ 89 - 1
2 ^ 107 - 1
2 ^ 127 - 1
2 ^ 521 - 1
2 ^ 607 - 1
2 ^ 1279 - 1
2 ^ 2203 - 1
2 ^ 2281 - 1
2 ^ 3217 - 1
2 ^ 4253 - 1
2 ^ 4423 - 1

```



## PARI/GP


```parigp
LL(p)={
  my(m=Mod(4,1<<p-1));
  for(i=3,p,m=m^2-2);
  m==0
};
forprime(p=2,, if(LL(p), print("2^"p"-1")))
```



## Perl

Since [https://www.mersenne.org/report_milestones/ GIMPS] went to the trouble of dedicating thousands of CPU years to finding Mersenne primes, we should be kind enough to use the results.  The [https://metacpan.org/pod/ntheory ntheory] module front end does this, so the results up to 43 million is extremely fast (4 seconds), and we can reduce this another 10x by only checking primes.  After the GIMPS double-checked mark, a Lucas-Lehmer test is done using code similar to [https://rosettacode.org/wiki/Lucas-Lehmer_test#GMP Rosetta Code Lucas-Lehmer in C+GMP].

If this is too contrived, we can use <code>Math::Prime::Util::GMP::is_mersenne_prime</code> instead, which will run the Lucas-Lehmer test on each input.  The first 23 Mersenne primes are found in under 15 seconds.

{{libheader|ntheory}}

```perl
use ntheory qw/forprimes is_mersenne_prime/;
forprimes { is_mersenne_prime($_) && say } 1e9;
```

{{out}}

```txt

2
3
5
7
13
17
19
31
61
...

```



## Perl 6

{{works with|Rakudo|2018.01}}
We already have a multitude of tasks that demonstrate '''how''' to find Mersenne primes; [[Prime decomposition]], [[Primality by trial division]], [[Trial factoring of a Mersenne number]], [[Lucas-Lehmer test]], [[Miller–Rabin primality_test]], etc. that all have Perl 6 entries. I'm not sure what I could add here that would be useful.

Hmmm. 

<blockquote>''Create code that will list all of the Mersenne primes until some limitation is reached.''</blockquote>

It doesn't specify to ''calculate'' them, only to ''list'' them; why throw away all of the computer '''millenia''' of processing power that the GIMPS has invested?


```perl6
use HTTP::UserAgent;
use Gumbo;

my $table = parse-html(HTTP::UserAgent.new.get('https://www.mersenne.org/primes/').content, :TAG<table>);

say 'All known Mersenne primes as of ', Date(now);

say 'M', ++$, ": 2$_ - 1"
  for $table[1]».[*][0][*].comb(/'exp_lo='\d+/)».subst(/\D/, '',:g)
  .trans([<0123456789>.comb] => [<⁰¹²³⁴⁵⁶⁷⁸⁹>.comb]).words;

```

{{out}}

```txt
All known Mersenne primes as of 2018-12-21
M1: 2² - 1
M2: 2³ - 1
M3: 2⁵ - 1
M4: 2⁷ - 1
M5: 2¹³ - 1
M6: 2¹⁷ - 1
M7: 2¹⁹ - 1
M8: 2³¹ - 1
M9: 2⁶¹ - 1
M10: 2⁸⁹ - 1
M11: 2¹⁰⁷ - 1
M12: 2¹²⁷ - 1
M13: 2⁵²¹ - 1
M14: 2⁶⁰⁷ - 1
M15: 2¹²⁷⁹ - 1
M16: 2²²⁰³ - 1
M17: 2²²⁸¹ - 1
M18: 2³²¹⁷ - 1
M19: 2⁴²⁵³ - 1
M20: 2⁴⁴²³ - 1
M21: 2⁹⁶⁸⁹ - 1
M22: 2⁹⁹⁴¹ - 1
M23: 2¹¹²¹³ - 1
M24: 2¹⁹⁹³⁷ - 1
M25: 2²¹⁷⁰¹ - 1
M26: 2²³²⁰⁹ - 1
M27: 2⁴⁴⁴⁹⁷ - 1
M28: 2⁸⁶²⁴³ - 1
M29: 2¹¹⁰⁵⁰³ - 1
M30: 2¹³²⁰⁴⁹ - 1
M31: 2²¹⁶⁰⁹¹ - 1
M32: 2⁷⁵⁶⁸³⁹ - 1
M33: 2⁸⁵⁹⁴³³ - 1
M34: 2¹²⁵⁷⁷⁸⁷ - 1
M35: 2¹³⁹⁸²⁶⁹ - 1
M36: 2²⁹⁷⁶²²¹ - 1
M37: 2³⁰²¹³⁷⁷ - 1
M38: 2⁶⁹⁷²⁵⁹³ - 1
M39: 2¹³⁴⁶⁶⁹¹⁷ - 1
M40: 2²⁰⁹⁹⁶⁰¹¹ - 1
M41: 2²⁴⁰³⁶⁵⁸³ - 1
M42: 2²⁵⁹⁶⁴⁹⁵¹ - 1
M43: 2³⁰⁴⁰²⁴⁵⁷ - 1
M44: 2³²⁵⁸²⁶⁵⁷ - 1
M45: 2³⁷¹⁵⁶⁶⁶⁷ - 1
M46: 2⁴²⁶⁴³⁸⁰¹ - 1
M47: 2⁴³¹¹²⁶⁰⁹ - 1
M48: 2⁵⁷⁸⁸⁵¹⁶¹ - 1
M49: 2⁷⁴²⁰⁷²⁸¹ - 1
M50: 2⁷⁷²³²⁹¹⁷ - 1
M51: 2⁸²⁵⁸⁹⁹³³ - 1
```



## Phix

{{libheader|mpfr}}

```Phix
include mpfr.e
atom t0 = time()
mpz mp = mpz_init(),
    bp = mpz_init()
randstate state = gmp_randinit_mt()
integer p = 0, count = 0
while true do
    mpz_ui_pow_ui(mp,2,p)
    mpz_sub_ui(mp,mp,1)
    if mpz_probable_prime_p(mp, state) then
        string s = iff(time()-t0<1?"":", "&elapsed(time()-t0))
        printf(1, "2^%d-1%s\n",{p,s})
        count += 1
        if count>=17 then exit end if
    end if
    while true do
        p = iff(p>2?p+2:3)
        mpz_set_si(bp,p)
        if mpz_probable_prime_p(bp, state) then exit end if
    end while   
end while
{mp,bp} = mpz_free({mp,bp})
state =  gmp_randclear(state)
```

{{out}}

```txt

2^3-1
2^5-1
2^7-1
2^13-1
2^17-1
2^19-1
2^31-1
2^61-1
2^89-1
2^107-1
2^127-1
2^521-1
2^607-1
2^1279-1
2^2203-1, 2.5s
2^2281-1, 2.9s
2^3217-1, 9.5s

```



## Prolog

Lucas-Lehmer test, works with SWI Prolog

```prolog

lucas_lehmer_seq(M, L) :-
    lazy_list(ll_iter, 4-M, L).

ll_iter(S-M, T-M, T) :-
    T is ((S*S) - 2) mod M.

drop(N, Lz1, Lz2) :-
    append(Pfx, Lz2, Lz1), length(Pfx, N), !.

mersenne_prime(2).
mersenne_prime(P) :-
    P > 2,
    M is (1 << P) - 1,
    lucas_lehmer_seq(M, Residues),
    Skip is P - 3, drop(Skip, Residues, [R|_]),
    R =:= 0.

```

{{out}}

```txt

?- findall(X, (between(1, 1000, X), mersenne_prime(X)), L), write(L).
[2,3,5,7,13,17,19,31,61,89,107,127,521,607]
L = [2, 3, 5, 7, 13, 17, 19, 31, 61|...].

```



## Python

{{trans|Java}}

```python
import random

#Take from https://www.codeproject.com/Articles/691200/%2FArticles%2F691200%2FPrimality-test-algorithms-Prime-test-The-fastest-w
def MillerRabinPrimalityTest(number):
    '''
    because the algorithm input is ODD number than if we get
    even and it is the number 2 we return TRUE ( spcial case )
    if we get the number 1 we return false and any other even 
    number we will return false.
    '''
    if number == 2:
        return True
    elif number == 1 or number % 2 == 0:
        return False
    
    ''' first we want to express n as : 2^s * r ( were r is odd ) '''
    
    ''' the odd part of the number '''
    oddPartOfNumber = number - 1
    
    ''' The number of time that the number is divided by two '''
    timesTwoDividNumber = 0
    
    ''' while r is even divid by 2 to find the odd part '''
    while oddPartOfNumber % 2 == 0:
        oddPartOfNumber = oddPartOfNumber / 2
        timesTwoDividNumber = timesTwoDividNumber + 1 
     
    '''
    since there are number that are cases of "strong liar" we 
    need to check more then one number
    '''
    for time in range(3):
        
        ''' choose "Good" random number '''
        while True:
            ''' Draw a RANDOM number in range of number ( Z_number )  '''
            randomNumber = random.randint(2, number)-1
            if randomNumber != 0 and randomNumber != 1:
                break
        
        ''' randomNumberWithPower = randomNumber^oddPartOfNumber mod number '''
        randomNumberWithPower = pow(randomNumber, oddPartOfNumber, number)
        
        ''' if random number is not 1 and not -1 ( in mod n ) '''
        if (randomNumberWithPower != 1) and (randomNumberWithPower != number - 1):
            # number of iteration
            iterationNumber = 1
            
            ''' while we can squre the number and the squered number is not -1 mod number'''
            while (iterationNumber <= timesTwoDividNumber - 1) and (randomNumberWithPower != number - 1):
                ''' squre the number '''
                randomNumberWithPower = pow(randomNumberWithPower, 2, number)
                
                # inc the number of iteration
                iterationNumber = iterationNumber + 1
            '''     
            if x != -1 mod number then it because we did not found strong witnesses
            hence 1 have more then two roots in mod n ==>
            n is composite ==> return false for primality
            '''
            if (randomNumberWithPower != (number - 1)):
                return False
            
    ''' well the number pass the tests ==> it is probably prime ==> return true for primality '''
    return True

# Main
MAX = 20
p = 2
count = 0
while True:
    m = (2 << (p - 1)) - 1
    if MillerRabinPrimalityTest(m):
        print "2 ^ {} - 1".format(p)
        count = count + 1
        if count == MAX:
            break
    # obtain next prime, p
    while True:
        p = p + 2 if (p > 2) else 3
        if MillerRabinPrimalityTest(p):
            break
print "done"
```

{{out}}

```txt
2 ^ 2 - 1
2 ^ 3 - 1
2 ^ 5 - 1
2 ^ 7 - 1
2 ^ 13 - 1
2 ^ 17 - 1
2 ^ 19 - 1
2 ^ 31 - 1
2 ^ 61 - 1
2 ^ 89 - 1
2 ^ 107 - 1
2 ^ 127 - 1
2 ^ 521 - 1
2 ^ 607 - 1
2 ^ 1279 - 1
2 ^ 2203 - 1
2 ^ 2281 - 1
2 ^ 3217 - 1
2 ^ 4253 - 1
2 ^ 4423 - 1
done
```



## REXX

This REXX version   (using a 32-bit Regina REXX interpreter)   will find those Mersenne primes which are less than 

8 million decimal digits   (which would be '''M43''').   

```rexx
/*REXX program uses  exponent─and─mod  operator to test possible Mersenne numbers.      */
      do j=1;                                    /*process a range,  or run out of time.*/
      if \isPrime(j)  then iterate               /*if  J  isn't a prime,  keep plugging.*/
      r= testMer(j)                              /*If J is prime, give J the 3rd degree.*/
      if r==0   then  say right('M'j, 10)     "──────── is a Mersenne prime."
                else  say right('M'j, 50)     "is composite, a factor:"   r
      end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPrime: procedure; parse arg x;             if wordpos(x, '2 3 5 7') \== 0  then return 1
         if x<11  then return 0;             if x//2 == 0 | x//3       == 0  then return 0
              do j=5  by 6;                  if x//j == 0 | x//(j+2)   == 0  then return 0
              if j*j>x   then return 1                 /*◄─┐         ___                */
              end   /*j*/                              /*  └─◄ Is j>√ x ?  Then return 1*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
iSqrt:   procedure; parse arg x;  #= 1;      r= 0;             do while #<=x;  #=#*4;  end
           do while #>1;  #=#%4;  _= x-r-#;  r= r%2;  if _>=0  then do;  x=_;  r=r+#;  end
           end   /*while*/                             /*iSqrt ≡    integer square root.*/
         return r                                      /*─────      ─       ──     ─  ─ */
/*──────────────────────────────────────────────────────────────────────────────────────*/
testMer: procedure;  parse arg x;              p =2**x /* [↓]  do we have enough digits?*/
         $$=x2b( d2x(x) ) + 0
         if pos('E',p)\==0  then do; parse var p "E" _;  numeric digits _+2;  p=2**x;  end
         !.=1;  !.1=0;  !.7=0                          /*array used for a quicker test. */
         R=iSqrt(p)                                    /*obtain integer square root of P*/
                    do k=2  by 2;        q=k*x  +  1   /*(shortcut) compute value of Q. */
                    m=q // 8                           /*obtain the remainder when ÷ 8. */
                    if !.m               then iterate  /*M  must be either one or seven.*/
                    parse var q '' -1 _; if _==5  then iterate      /*last digit a five?*/
                    if q// 3==0  then iterate                       /*    ÷   by three? */
                    if q// 7==0  then iterate                       /*    "    " seven? */
                    if q//11==0  then iterate                       /*    "    " eleven?*/
                                                       /*      ____                     */
                    if q>R               then return 0 /*Is q>√2**x ?   A Mersenne prime*/
                    sq=1;         $=$$                 /*obtain binary version from  $. */
                        do  until $=='';      sq=sq*sq
                        parse var $  _  2  $           /*obtain 1st digit and the rest. */
                        if _  then sq=(sq+sq) // q
                        end   /*until*/
                    if sq==1  then return q            /*Not a prime?   Return a factor.*/
                    end   /*k*/
```






## Ring


```ring

# Project : Mersenne primes

n = 0
while true
        n = n +1
        if isprime(pow(2,n)-1) = 1
           see n + nl
        ok
end

func isprime num
       if (num <= 1) return 0 ok
       if (num % 2 = 0) and num != 2 return 0 ok
       for i = 3 to floor(num / 2) -1 step 2
            if (num % i = 0) return 0 ok
       next
       return 1

```

Output:

```txt

2
3
5
7
13
17
19

```



## Scala


```Scala
object MersennePrimes extends App {
  import Stream._
 
  def primeSieve(s: Stream[Int]): Stream[Int] =
    s.head #:: primeSieve(s.tail filter { _ % s.head != 0 })
  val primes = primeSieve(from(2))
 
  def mersenne(p: Int): BigInt = (BigInt(2) pow p) - 1
 
  def s(mp: BigInt, p: Int): BigInt = { if (p == 1) 4 else ((s(mp, p - 1) pow 2) - 2) % mp }
 
  val upbPrime = 9941
  println(s"Finding Mersenne primes in M[2..$upbPrime]")
  ((primes takeWhile (_ <= upbPrime)).par map { p => (p, mersenne(p)) }
    map { p => if (p._1 == 2) (p, 0) else (p, s(p._2, p._1 - 1)) } filter { _._2 == 0 })
    .foreach { p =>
      println(s"prime M${(p._1)._1}: " +
        { if ((p._1)._1 < 200) (p._1)._2 else s"(${(p._1)._2.toString.size} digits)" })
    }
  println("That's All Folks!")
}
```


## Sidef

Uses the ''is_mersenne_prime()'' function from [https://metacpan.org/pod/Math::Prime::Util::GMP Math::Prime::Util::GMP].

```ruby
for p in (^Inf -> lazy.grep { .is_mersenne_prime }) {
    say "2^#{p} - 1"
}
```

{{out}}

```txt

2^2 - 1
2^3 - 1
2^5 - 1
2^7 - 1
2^13 - 1
2^17 - 1
2^19 - 1
2^31 - 1
2^61 - 1
2^89 - 1
2^107 - 1
2^127 - 1
2^521 - 1
2^607 - 1
2^1279 - 1
2^2203 - 1
2^2281 - 1
2^3217 - 1
2^4253 - 1
2^4423 - 1
2^9689 - 1
2^9941 - 1
^C
sidef mersenne.sf  12.47s user 0.02s system 99% cpu 12.495 total

```



## zkl

{{libheader|GMP}}
Uses libGMP (GNU MP Bignum Library) and its Miller-Rabin probabilistic primality testing algorithm.

```zkl
var [const] BN=Import.lib("zklBigNum");  // libGMP
fcn mprimes{
   n,m := BN(2),0;
   foreach e in ([2..]){
      n,m = n.shiftLeft(1), n-1;
      if(m.probablyPrime()) println("2^%d - 1".fmt(e));
   }
}()
// gets rather slow after M(4423)
```

{{out}}
<pre style="height:40ex">
2^2 - 1
2^3 - 1
2^5 - 1
2^7 - 1
2^13 - 1
2^17 - 1
2^19 - 1
2^31 - 1
2^61 - 1
2^89 - 1
2^107 - 1
2^127 - 1
2^521 - 1
2^607 - 1
2^1279 - 1
2^2203 - 1
2^2281 - 1
2^3217 - 1
2^4253 - 1
2^4423 - 1
2^9689 - 1
2^9941 - 1
2^11213 - 1
...

```

