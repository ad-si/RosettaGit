+++
title = "Sequence: nth number with exactly n divisors"
description = ""
date = 2019-10-19T22:35:19Z
aliases = []
[extra]
id = 22268
[taxonomies]
categories = []
tags = []
+++

{{task}}
Calculate the sequence where each term <strong>a<sub>n</sub></strong> is the <strong>n<sup>th</sup></strong> that has '''n''' divisors.

;Task

Show here, on this page, at least the first '''15''' terms of the sequence.

;See also

:*[[oeis:A073916|OEIS:A073916]]

;Related tasks

:*[[Sequence: smallest number greater than previous term with exactly n divisors]]
:*[[Sequence: smallest number with exactly n divisors]]


## Factor

This makes use of most of the optimizations discussed in the Go example.

```factor
USING: combinators formatting fry kernel lists lists.lazy
lists.lazy.examples literals math math.functions math.primes
math.primes.factors math.ranges sequences ;
IN: rosetta-code.nth-n-div

CONSTANT: primes $[ 100 nprimes ]

: prime ( m -- n ) 1 - [ primes nth ] [ ^ ] bi ;

: (non-prime) ( m quot -- n )
    '[
        [ 1 - ] [ drop @ ] [ ] tri '[ divisors length _ = ]
        lfilter swap [ cdr ] times car
    ] call ; inline

: non-prime ( m quot -- n )
    {
        { [ over 2 = ] [ 2drop 3 ] }
        { [ over 10 = ] [ 2drop 405 ] }
        [ (non-prime) ]
    } cond ; inline

: fn ( m -- n )
    {
        { [ dup even? ] [ [ evens ] non-prime ] }
        { [ dup prime? ] [ prime ] }
        [ [ squares ] non-prime ]
    } cond ;

: main ( -- ) 45 [1,b] [ dup fn "%2d : %d\n" printf ] each ;

MAIN: main
```

{{out}}

```txt

 1 : 1
 2 : 3
 3 : 25
 4 : 14
 5 : 14641
 6 : 44
 7 : 24137569
 8 : 70
 9 : 1089
10 : 405
11 : 819628286980801
12 : 160
13 : 22563490300366186081
14 : 2752
15 : 9801
16 : 462
17 : 21559177407076402401757871041
18 : 1044
19 : 740195513856780056217081017732809
20 : 1520
21 : 141376
22 : 84992
23 : 1658509762573818415340429240403156732495289
24 : 1170
25 : 52200625
26 : 421888
27 : 52900
28 : 9152
29 : 1116713952456127112240969687448211536647543601817400964721
30 : 6768
31 : 1300503809464370725741704158412711229899345159119325157292552449
32 : 3990
33 : 12166144
34 : 9764864
35 : 446265625
36 : 5472
37 : 11282036144040442334289838466416927162302790252609308623697164994458730076798801
38 : 43778048
39 : 90935296
40 : 10416
41 : 1300532588674810624476094551095787816112173600565095470117230812218524514342511947837104801
42 : 46400
43 : 635918448514386699807643535977466343285944704172890141356181792680152445568879925105775366910081
44 : 240640
45 : 327184

```



## Go

This makes use of the relationship: a[p] = prime[p]^(p-1) if p is prime, mentioned in the blurb for A073916 (and also on the talk page) to calculate the larger terms, some of which require big.Int in Go. It also makes use of another hint on the talk page that all odd terms are square numbers.

The remaining terms (up to the 33rd) are not particularly large and so are calculated by brute force. 

```go
package main

import (
    "fmt"
    "math"
    "math/big"
)

var bi = new(big.Int)

func isPrime(n int) bool {
    bi.SetUint64(uint64(n))
    return bi.ProbablyPrime(0)
}

func generateSmallPrimes(n int) []int {
    primes := make([]int, n)
    primes[0] = 2
    for i, count := 3, 1; count < n; i += 2 {
        if isPrime(i) {
            primes[count] = i
            count++
        }
    }
    return primes
}

func countDivisors(n int) int {
    count := 1
    for n%2 == 0 {
        n >>= 1
        count++
    }
    for d := 3; d*d <= n; d += 2 {
        q, r := n/d, n%d
        if r == 0 {
            dc := 0
            for r == 0 {
                dc += count
                n = q
                q, r = n/d, n%d
            }
            count += dc
        }
    }
    if n != 1 {
        count *= 2
    }
    return count
}

func main() {
    const max = 33
    primes := generateSmallPrimes(max)
    z := new(big.Int)
    p := new(big.Int)
    fmt.Println("The first", max, "terms in the sequence are:")
    for i := 1; i <= max; i++ {
        if isPrime(i) {
            z.SetUint64(uint64(primes[i-1]))
            p.SetUint64(uint64(i - 1))
            z.Exp(z, p, nil)
            fmt.Printf("%2d : %d\n", i, z)
        } else {
            count := 0
            for j := 1; ; j++ {
                if i%2 == 1 {
                    sq := int(math.Sqrt(float64(j)))
                    if sq*sq != j {
                        continue
                    }
                }
                if countDivisors(j) == i {
                    count++
                    if count == i {
                        fmt.Printf("%2d : %d\n", i, j)
                        break
                    }
                }
            }
        }
    }
}
```


{{out}}

```txt

The first 33 terms in the sequence are:
 1 : 1
 2 : 3
 3 : 25
 4 : 14
 5 : 14641
 6 : 44
 7 : 24137569
 8 : 70
 9 : 1089
10 : 405
11 : 819628286980801
12 : 160
13 : 22563490300366186081
14 : 2752
15 : 9801
16 : 462
17 : 21559177407076402401757871041
18 : 1044
19 : 740195513856780056217081017732809
20 : 1520
21 : 141376
22 : 84992
23 : 1658509762573818415340429240403156732495289
24 : 1170
25 : 52200625
26 : 421888
27 : 52900
28 : 9152
29 : 1116713952456127112240969687448211536647543601817400964721
30 : 6768
31 : 1300503809464370725741704158412711229899345159119325157292552449
32 : 3990
33 : 12166144

```


The following much faster version (runs in less than 90 seconds on my 1.6GHz Celeron) uses three further optimizations:

1. Apart from the 2nd and 10th terms, all the even terms are themselves even.

2. A sieve is used to generate all prime divisors needed. This doesn't take up much time or memory but speeds up the counting of all divisors considerably.

3. While searching for the nth number with exactly n divisors, where feasible a record is kept of any numbers found to have exactly k divisors (k > n) so that the search for these numbers can start from a higher base.


```go
package main

import (
    "fmt"
    "math"
    "math/big"
)

type record struct{ num, count int }

var (
    bi     = new(big.Int)
    primes = []int{2}
)

func isPrime(n int) bool {
    bi.SetUint64(uint64(n))
    return bi.ProbablyPrime(0)
}

func sieve(limit int) {
    c := make([]bool, limit+1) // composite = true
    // no need to process even numbers
    p := 3
    for {
        p2 := p * p
        if p2 > limit {
            break
        }
        for i := p2; i <= limit; i += 2 * p {
            c[i] = true
        }
        for {
            p += 2
            if !c[p] {
                break
            }
        }
    }
    for i := 3; i <= limit; i += 2 {
        if !c[i] {
            primes = append(primes, i)
        }
    }
}

func countDivisors(n int) int {
    count := 1
    for i, p := 0, primes[0]; p*p <= n; i, p = i+1, primes[i+1] {
        if n%p != 0 {
            continue
        }
        n /= p
        count2 := 1
        for n%p == 0 {
            n /= p
            count2++
        }
        count *= (count2 + 1)
        if n == 1 {
            return count
        }
    }
    if n != 1 {
        count *= 2
    }
    return count
}

func isOdd(x int) bool {
    return x%2 == 1
}

func main() {
    sieve(22000)
    const max = 45
    records := [max + 1]record{}
    z := new(big.Int)
    p := new(big.Int)
    fmt.Println("The first", max, "terms in the sequence are:")
    for i := 1; i <= max; i++ {
        if isPrime(i) {
            z.SetUint64(uint64(primes[i-1]))
            p.SetUint64(uint64(i - 1))
            z.Exp(z, p, nil)
            fmt.Printf("%2d : %d\n", i, z)
        } else {
            count := records[i].count
            if count == i {
                fmt.Printf("%2d : %d\n", i, records[i].num)
                continue
            }
            odd := isOdd(i)
            k := records[i].num
            l := 1
            if !odd && i != 2 && i != 10 {
                l = 2
            }
            for j := k + l; ; j += l {
                if odd {
                    sq := int(math.Sqrt(float64(j)))
                    if sq*sq != j {
                        continue
                    }
                }
                cd := countDivisors(j)
                if cd == i {
                    count++
                    if count == i {
                        fmt.Printf("%2d : %d\n", i, j)
                        break
                    }
                } else if cd > i && cd <= max && records[cd].count < cd &&
                    j > records[cd].num && (l == 1 || (l == 2 && !isOdd(cd))) {
                    records[cd].num = j
                    records[cd].count++
                }
            }
        }
    }
}
```


{{out}}

```txt

The first 45 terms in the sequence are:
 1 : 1
 2 : 3
 3 : 25
 4 : 14
 5 : 14641
 6 : 44
 7 : 24137569
 8 : 70
 9 : 1089
10 : 405
11 : 819628286980801
12 : 160
13 : 22563490300366186081
14 : 2752
15 : 9801
16 : 462
17 : 21559177407076402401757871041
18 : 1044
19 : 740195513856780056217081017732809
20 : 1520
21 : 141376
22 : 84992
23 : 1658509762573818415340429240403156732495289
24 : 1170
25 : 52200625
26 : 421888
27 : 52900
28 : 9152
29 : 1116713952456127112240969687448211536647543601817400964721
30 : 6768
31 : 1300503809464370725741704158412711229899345159119325157292552449
32 : 3990
33 : 12166144
34 : 9764864
35 : 446265625
36 : 5472
37 : 11282036144040442334289838466416927162302790252609308623697164994458730076798801
38 : 43778048
39 : 90935296
40 : 10416
41 : 1300532588674810624476094551095787816112173600565095470117230812218524514342511947837104801
42 : 46400
43 : 635918448514386699807643535977466343285944704172890141356181792680152445568879925105775366910081
44 : 240640
45 : 327184

```



## Java

{{trans|Go}}

```java
import java.util.ArrayList;
import java.math.BigInteger;
import static java.lang.Math.sqrt;

public class OEIS_A073916 {

    static boolean is_prime(int n) {
        return BigInteger.valueOf(n).isProbablePrime(10);
    }

    static ArrayList<Integer> generate_small_primes(int n) {
        ArrayList<Integer> primes = new ArrayList<Integer>();
        primes.add(2);
        for (int i = 3; primes.size() < n; i += 2) {
            if (is_prime(i)) primes.add(i);
        }
        return primes;
    }

    static int count_divisors(int n) {
        int count = 1;
        while (n % 2 == 0) {
            n >>= 1;
            ++count;
        }
        for (int d = 3; d * d <= n; d += 2) {
            int q = n / d;
            int r = n % d;
            if (r == 0) {
                int dc = 0;
                while (r == 0) {
                    dc += count;
                    n = q;
                    q = n / d;
                    r = n % d;
                }
                count += dc;
            }
        }
        if (n != 1) count *= 2;
        return count;
    }

    public static void main(String[] args) {
        final int max = 33;
        ArrayList<Integer> primes = generate_small_primes(max);
        System.out.printf("The first %d terms of the sequence are:\n", max);
        for (int i = 1; i <= max; ++i) {
            if (is_prime(i)) {
                BigInteger z = BigInteger.valueOf(primes.get(i - 1));
                z = z.pow(i - 1);
                System.out.printf("%2d : %d\n", i, z);
            } else {
                for (int j = 1, count = 0; ; ++j) {
                    if (i % 2 == 1) {
                        int sq = (int)sqrt(j);
                        if (sq * sq != j) continue;
                    }
                    if (count_divisors(j) == i) {
                        if (++count == i) {
                            System.out.printf("%2d : %d\n", i, j);
                            break;
                        }
                    }
                }
            }
        }
    }
}
```


{{out}}

```txt

The first 33 terms of the sequence are:
 1 : 1
 2 : 3
 3 : 25
 4 : 14
 5 : 14641
 6 : 44
 7 : 24137569
 8 : 70
 9 : 1089
10 : 405
11 : 819628286980801
12 : 160
13 : 22563490300366186081
14 : 2752
15 : 9801
16 : 462
17 : 21559177407076402401757871041
18 : 1044
19 : 740195513856780056217081017732809
20 : 1520
21 : 141376
22 : 84992
23 : 1658509762573818415340429240403156732495289
24 : 1170
25 : 52200625
26 : 421888
27 : 52900
28 : 9152
29 : 1116713952456127112240969687448211536647543601817400964721
30 : 6768
31 : 1300503809464370725741704158412711229899345159119325157292552449
32 : 3990
33 : 12166144

```




## Julia


```julia
using Primes

function countdivisors(n)
    f = [one(n)]
    for (p, e) in factor(n)
        f = reduce(vcat, [f * p ^ j for j in 1:e], init = f)
    end
    length(f)
end

function nthwithndivisors(N)
    parray = findall(primesmask(100 * N))
    for i = 1:N
        if isprime(i)
            println("$i : ", BigInt(parray[i])^(i-1))
        else
            k = 0
            for j in 1:100000000000
                if (iseven(i) || Int(floor(sqrt(j)))^2 == j) &&
                    i == countdivisors(j) && (k += 1) == i
                    println("$i : $j")
                    break
                end
            end
        end
    end
end

nthwithndivisors(35)

```
{{out}}

```txt

1 : 1
2 : 3
3 : 25
4 : 14
5 : 14641
6 : 44
7 : 24137569
8 : 70
9 : 1089
10 : 405
11 : 819628286980801
12 : 160
13 : 22563490300366186081
14 : 2752
15 : 9801
16 : 462
17 : 21559177407076402401757871041
18 : 1044
19 : 740195513856780056217081017732809
20 : 1520
21 : 141376
22 : 84992
23 : 1658509762573818415340429240403156732495289
24 : 1170
25 : 52200625
26 : 421888
27 : 52900
28 : 9152
29 : 1116713952456127112240969687448211536647543601817400964721
30 : 6768
31 : 1300503809464370725741704158412711229899345159119325157292552449
32 : 3990
33 : 12166144
34 : 9764864
35 : 446265625

```



## Kotlin

{{trans|Go}}

```scala
// Version 1.3.21

import java.math.BigInteger
import kotlin.math.sqrt

const val MAX = 33

fun isPrime(n: Int) = BigInteger.valueOf(n.toLong()).isProbablePrime(10)

fun generateSmallPrimes(n: Int): List<Int> {
    val primes = mutableListOf<Int>()
    primes.add(2)
    var i = 3
    while (primes.size < n) {
        if (isPrime(i)) {
            primes.add(i)
        }
        i += 2
    }
    return primes
}

fun countDivisors(n: Int): Int {
    var nn = n
    var count = 1
    while (nn % 2 == 0) {
        nn = nn shr 1
        count++
    }
    var d = 3
    while (d * d <= nn) {
        var q = nn / d
        var r = nn % d
        if (r == 0) {
            var dc = 0
            while (r == 0) {
                dc += count
                nn = q
                q = nn / d
                r = nn % d
            }
            count += dc
        }
        d += 2
    }
    if (nn != 1) count *= 2
    return count
}

fun main() {
    var primes = generateSmallPrimes(MAX)
    println("The first $MAX terms in the sequence are:")
    for (i in 1..MAX) {
        if (isPrime(i)) {
            var z = BigInteger.valueOf(primes[i - 1].toLong())
            z = z.pow(i - 1)
            System.out.printf("%2d : %d\n", i, z)
        } else {
            var count = 0
            var j = 1
            while (true) {
                if (i % 2 == 1) {
                    val sq = sqrt(j.toDouble()).toInt()
                    if (sq * sq != j) {
                        j++
                        continue
                    }
                }
                if (countDivisors(j) == i) {
                    if (++count == i) {
                        System.out.printf("%2d : %d\n", i, j)
                        break
                    }
                }
                j++
            }
        }
    }
}
```


{{output}}

```txt

The first 33 terms in the sequence are:
 1 : 1
 2 : 3
 3 : 25
 4 : 14
 5 : 14641
 6 : 44
 7 : 24137569
 8 : 70
 9 : 1089
10 : 405
11 : 819628286980801
12 : 160
13 : 22563490300366186081
14 : 2752
15 : 9801
16 : 462
17 : 21559177407076402401757871041
18 : 1044
19 : 740195513856780056217081017732809
20 : 1520
21 : 141376
22 : 84992
23 : 1658509762573818415340429240403156732495289
24 : 1170
25 : 52200625
26 : 421888
27 : 52900
28 : 9152
29 : 1116713952456127112240969687448211536647543601817400964721
30 : 6768
31 : 1300503809464370725741704158412711229899345159119325157292552449
32 : 3990
33 : 12166144

```



## Perl

{{libheader|ntheory}}
{{trans|Perl 6}}

```perl
use strict;
use warnings;
use bigint;
use ntheory <nth_prime is_prime divisors>;

my $limit = 20;

print "First $limit terms of OEIS:A073916\n";

for my $n (1..$limit) {
    if ($n > 4 and is_prime($n)) {
        print nth_prime($n)**($n-1) . ' ';
    } else {
        my $i = my $x = 0;
        while (1) {
            my $nn = $n%2 ? ++$x**2 : ++$x;
            next unless $n == divisors($nn) and ++$i == $n;
            print "$nn " and last;
      }
    }
}
```

{{out}}

```txt
First 20 terms of OEIS:A073916
1 3 25 14 14641 44 24137569 70 1089 405 819628286980801 160 22563490300366186081 2752 9801 462 21559177407076402401757871041 1044 740195513856780056217081017732809 1520
```



## Perl 6

{{works with|Rakudo|2019.03}}

[https://tio.run/##dVLbTsJAEH3nKw4IpgW6ATQY2QAao4kvmsijEFPoVpr05nZr2hh@yk/wx@p0CwUf3JfuzJwzc/ZMYyH9cVEk6RqO92ltojRUMJaZia8G6EihUhliBM9FxrzEiqUXCK5rPde3CTwEY1RLPqRirh9F0mSBHU9gzbB09m3Kk4a@SBJk6IDSCHIsc0wppsFwOCiYUmU@p1uzCSPvwzGx0/xdg74NorR9L/AU0UYDXmVutKKEUu9SxNS4VoldH0PGuryxx7xW7BXHGSqE2grYUto5w@Lu6YU6xqlC68GTiTrMUkIGCSIXz/ePi8nt4OriejhucY00qH8FM9k2j4U0JqO1rTbbowftcO@BQbcZLmGHDiVrlSa9WNdFFpcQC8M@asE6XplkiMY40YmhpR0evXvA/6ZIsK0iSRWidzq0PPK0VNo1tbH6Vunr/nwfyTWTueX7JyejyhOKTB2WSI1pWey8/mf4v9BerxRZavmLab/VYb1jXhS/ Try it online!]


```perl6
sub div-count (\x) {
    return 2 if x.is-prime;
    +flat (1 .. x.sqrt.floor).map: -> \d {
        unless x % d { my \y = x div d; y == d ?? y !! (y, d) }
    }
}

my $limit = 20;

my @primes = grep { .is-prime }, 1..*;
@primes[$limit]; # prime the array. SCNR

put "First $limit terms of OEIS:A073916";
put (1..$limit).hyper(:2batch).map: -> $n {
    ($n > 4 and $n.is-prime) ??
    exp($n - 1, @primes[$n - 1]) !!
    do {
        my $i = 0;
        my $iterator = $n %% 2 ?? (1..*) !! (1..*).map: *²;
        $iterator.first: {
            next unless $n == .&div-count;
            next unless ++$i == $n;
            $_
        }
    }
};
```



```txt
First 20 terms of OEIS:A073916
1 3 25 14 14641 44 24137569 70 1089 405 819628286980801 160 22563490300366186081 2752 9801 462 21559177407076402401757871041 1044 740195513856780056217081017732809 1520
```



## Phix

{{libheader|mpfr}}

### simple

Certainly not the fastest way to do it, hence the relatively small limit of 24, which takes less than 0.4s,

whereas a limit of 25 would need to invoke factors() 52 million times which would no doubt take a fair while.

```Phix
constant LIMIT = 24
include mpfr.e
mpz z = mpz_init()

sequence fn = 1&repeat(0,LIMIT-1),
         primes = {2,3}
integer k = 1
printf(1,"The first %d terms in the sequence are:\n",LIMIT)
for i=1 to LIMIT do
    sequence f = factors(i,1)
    if length(f)=2 then     -- i is prime (f is {1,i})
        while length(primes)<i do
            integer p = primes[$]+2
            while prime_factors(p)!={} do p += 2 end while
            primes = append(primes,p)
        end while
        mpz_ui_pow_ui(z,primes[i],i-1)
        printf(1,"%2d : %s\n",{i,mpz_get_str(z)})
    else
        while fn[i]<i do
            k += 1
            integer l = length(factors(k,1))
            if l<=LIMIT and fn[l]<l then
                fn[l] = iff(fn[l]+1<l?fn[l]+1:k)
            end if
        end while
        printf(1,"%2d : %d\n",{i,fn[i]})
    end if
end for
```

{{out}}

```txt

The first 24 terms in the sequence are:
 1 : 1
 2 : 3
 3 : 25
 4 : 14
 5 : 14641
 6 : 44
 7 : 24137569
 8 : 70
 9 : 1089
10 : 405
11 : 819628286980801
12 : 160
13 : 22563490300366186081
14 : 2752
15 : 9801
16 : 462
17 : 21559177407076402401757871041
18 : 1044
19 : 740195513856780056217081017732809
20 : 1520
21 : 141376
22 : 84992
23 : 1658509762573818415340429240403156732495289
24 : 1170

```


### cheating slightly

No real patterns that I could see here, but you can still identify and single out the troublemakers (of which there are about 30).

```Phix
include mpfr.e
atom t0 = time()
constant LIMIT = 100
include mpfr.e
include primes.e
mpz z = mpz_init(),
    p = mpz_init() 
string mz
sequence fn = 1&repeat(0,LIMIT-1), dx
integer k = 1, idx, p1, p2
printf(1,"The first %d terms in the sequence are:\n",LIMIT)
for i=1 to LIMIT do
    if is_prime(i) or i=1 then
        mpz_ui_pow_ui(z,get_prime(i),i-1)
        mz = mpz_get_str(z)
    else
        sequence f = prime_factors(i,1)
        if length(f)=2 and f[1]=2 and f[2]>7 then
            mz = sprintf("%d",power(2,f[2]-1)*get_prime(i+1))
        elsif length(f)=2 and f[1]>2 then
            if f[1]=f[2] then
                mz = sprintf("%d",power(f[1]*get_prime(f[1]+2),f[1]-1))
            else -- deal with some tardy ones...
                dx = {15,21,33,35,39,51,55,57,65,69,77,85,87,91,93,95}; idx = find(i,dx)
                p1 = { 3, 2, 2, 5, 2, 2, 2, 2, 2, 2, 7, 2, 2, 7, 2, 2}[idx]
                p2 = { 5,15,29, 6,35,49,34,56,45,69, 7,65,88, 7,94,77}[idx]
                mpz_ui_pow_ui(z,p1,f[2]-1)
                mpz_ui_pow_ui(p,get_prime(p2),f[1]-1)
                mpz_mul(z,z,p)
                mz = mpz_get_str(z)
            end if
        elsif (length(f)=3 and i>50) or (length(f)=4 and (f[1]=3 or f[4]>7)) then 
            if i=99 then    -- (oops, messed that one up!)
                mz = sprintf("%d",4*power(3,10)*31*31)
            elsif i=63 then -- (and another!)
                mz = sprintf("%d",power(2,8)*power(5,6))
            else
                dx = {52,66,68,70,75,76,78,92,98,81,88}; idx = find(i,dx)
                p1 = { 7, 3, 1, 5, 3, 5, 5,13, 3,35,35}[idx]
                p2 = { 1, 2, 1, 4, 4, 1, 2, 1, 1, 2, 1}[idx]
                mpz_ui_pow_ui(z,2,f[$]-1)
                mpz_ui_pow_ui(p,p1,p2)
                mpz_mul(z,z,p)
                p1 = {13,37, 4, 9,34,22,19,12, 4,11,13}[idx]
                p2 = { 1, 1, 3, 1, 2, 1, 1, 1, 6, 2, 1}[idx]
                mpz_ui_pow_ui(p,get_prime(p1),p2)
                mpz_mul(z,z,p)
                mz = mpz_get_str(z)
            end if
        else
            while fn[i]<i do
                k += 1
                integer l = length(factors(k,1))
                if l<=LIMIT and fn[l]<l then
                    fn[l] = iff(fn[l]+1<l?fn[l]+1:k)
                end if
            end while
            mz = sprintf("%d",fn[i])
        end if
    end if
    printf(1,"%3d : %s\n",{i,mz})
end for
printf(1,"completed in %s\n",{elapsed(time()-t0)})
```

{{out}}

```txt

The first 100 terms in the sequence are:
  1 : 1
  2 : 3
  3 : 25
  4 : 14
  5 : 14641
  6 : 44
  7 : 24137569
  8 : 70
  9 : 1089
 10 : 405
 11 : 819628286980801
 12 : 160
 13 : 22563490300366186081
 14 : 2752
 15 : 9801
 16 : 462
 17 : 21559177407076402401757871041
 18 : 1044
 19 : 740195513856780056217081017732809
 20 : 1520
 21 : 141376
 22 : 84992
 23 : 1658509762573818415340429240403156732495289
 24 : 1170
 25 : 52200625
 26 : 421888
 27 : 52900
 28 : 9152
 29 : 1116713952456127112240969687448211536647543601817400964721
 30 : 6768
 31 : 1300503809464370725741704158412711229899345159119325157292552449
 32 : 3990
 33 : 12166144
 34 : 9764864
 35 : 446265625
 36 : 5472
 37 : 11282036144040442334289838466416927162302790252609308623697164994458730076798801
 38 : 43778048
 39 : 90935296
 40 : 10416
 41 : 1300532588674810624476094551095787816112173600565095470117230812218524514342511947837104801
 42 : 46400
 43 : 635918448514386699807643535977466343285944704172890141356181792680152445568879925105775366910081
 44 : 240640
 45 : 327184
 46 : 884998144
 47 : 82602452843197830915655434062758747152610200533183747995128511868250464749389571755574391210629602061883161
 48 : 10296
 49 : 17416274304961
 50 : 231984
 51 : 3377004544
 52 : 1175552
 53 : 7326325566540660915295202005885275873916026034616342139474905237555535331121749053330837020397976615915057535109963186790081
 54 : 62208
 55 : 382260265984
 56 : 63168
 57 : 18132238336
 58 : 74356621312
 59 : 4611334279555550707926152839105934955536765902552873727962394200823974159354935875908492026570361080937000929065119751494662472171586496615769
 60 : 37200
 61 : 1279929743416851311019131209907830943453757487243270654630811620734985849511676634764875391422075025095805774223361200187655617244608064273703030801
 62 : 329638739968
 63 : 4000000
 64 : 41160
 65 : 6169143218176
 66 : 1446912
 67 : 20353897784481135224502113429729640062994484338530413467091588021107086251737634020247647652000753728181181145357697865506347474542010115076391004870941216126804332281
 68 : 22478848
 69 : 505031950336
 70 : 920000
 71 : 22091712217028661091647719716134154062183987922906664635563029317259865249987461330814689139636373404600637581380931231750650949001643115899851798743405544731506806491024751606849
 72 : 48300
 73 : 45285235038445046669368642612544904396805516154393281169675637706411327508046898517381759728413013085702957690245765106506995874808813788844198933536768701568785385215106907990288684161
 74 : 26044681682944
 75 : 25040016
 76 : 103546880
 77 : 6818265813529681
 78 : 6860800
 79 : 110984176612396876252402058909207317796166059426692518840795949938301678339569859458072604697803922487329059012193474923358078243829751108364014428972188856355641430510895584045477184112155202949344511201
 80 : 96720
 81 : 4708900
 82 : 473889511571456
 83 : 1064476683917919713953093000677954858036756167846865592483240200233630032347646244510522542053167377047784795269272961130616738371982635464615430562192693194769301221853619917764723198332349478419665523610384617408161
 84 : 225216
 85 : 629009610244096
 86 : 1974722883485696
 87 : 56062476550144
 88 : 1469440
 89 : 2544962774801294304714624882135254894108219227449639770372304502957346499018390075803907657903246999131414158076182409047363202723848127272231619125736007088495905384436604400674375401897829996007586872027878808309385140119563002941281
 90 : 352512
 91 : 334095024862954369
 92 : 2017460224
 93 : 258858752671744
 94 : 35114003344654336
 95 : 6002585119227904
 96 : 112860
 97 : 69969231567692157576407845029145070949540195647704307603423555494283752374775631665902846216473259715737953596002226233187827382886325202177640164868195792546734599315840795700630834939445407388277880586442087150607690134279001258366485550281200590593848327041
 98 : 22588608
 99 : 226984356
100 : 870000
completed in 4.4s

```



## Python

This implementation exploits the fact that terms corresponding to a prime value for n are always the nth prime to the (n-1)th power.

```Python

def divisors(n):
    divs = [1]
    for ii in range(2, int(n ** 0.5) + 3):
        if n % ii == 0:
            divs.append(ii)
            divs.append(int(n / ii))
    divs.append(n)
    return list(set(divs))


def is_prime(n):
    return len(divisors(n)) == 2


def primes():
    ii = 1
    while True:
        ii += 1
        if is_prime(ii):
            yield ii


def prime(n):
    generator = primes()
    for ii in range(n - 1):
        generator.__next__()
    return generator.__next__()


def n_divisors(n):
    ii = 0
    while True:
        ii += 1
        if len(divisors(ii)) == n:
            yield ii


def sequence(max_n=None):
    if max_n is not None:
        for ii in range(1, max_n + 1):
            if is_prime(ii):
                yield prime(ii) ** (ii - 1)
            else:
                generator = n_divisors(ii)
                for jj, out in zip(range(ii - 1), generator):
                    pass
                yield generator.__next__()
    else:
        ii = 1
        while True:
            ii += 1
            if is_prime(ii):
                yield prime(ii) ** (ii - 1)
            else:
                generator = n_divisors(ii)
                for jj, out in zip(range(ii - 1), generator):
                    pass
                yield generator.__next__()


if __name__ == '__main__':
    for item in sequence(15):
        print(item)

```

<b>Output:</b>

```Python

1
3
25
14
14641
44
24137569
70
1089
405
819628286980801
160
22563490300366186081
2752
9801

```



## REXX

Programming note:   this REXX version has minor optimization, and all terms of the sequence are determined (found) in order.

### little optimization


```rexx
/*REXX program  finds and displays  the    Nth  number   with exactly   N   divisors.   */
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N= 15                    /*Not specified?  Then use the default.*/
if N>=50  then numeric digits 10                 /*use more decimal digits for large N. */
w= 50                                            /*W:  width of the 2nd column of output*/
say '─divisors─'  center("the Nth number with exactly N divisors", w, '─')      /*title.*/
@.1= 2;                                   Ps= 1  /*1st prime;  number of primes (so far)*/
        do p=3  until Ps==N                      /* [↓]  gen N primes, store in @ array.*/
        if \isPrime(p)  then iterate;     Ps= Ps + 1;        @.Ps= p
        end   /*gp*/
!.=                                              /*the  !  array is used for memoization*/
        do i=1  for N;      odd= i//2            /*step through a number of divisors.   */
        if odd  then  if isPrime(i)  then do;  _= pPow();            w= max(w, length(_) )
                                               call tell  commas(_);              iterate
                                          end
        #= 0;            even= \odd              /*the number of occurrences for #div.  */
            do j=1;      jj= j                   /*now, search for a number that ≡ #divs*/
            if odd  then jj= j*j                 /*Odd and non-prime?  Calculate square.*/
            if !.jj==.  then iterate             /*has this number already been found?  */
            d= #divs(jj)                         /*get # divisors;  Is not equal?  Skip.*/
            if even  then if d<i  then do;  !.j=.;  iterate;  end   /*Too low?  Flag it.*/
            if d\==i  then iterate               /*Is not equal?  Then skip this number.*/
            #= # + 1                             /*bump number of occurrences for #div. */
            if #\==i  then iterate               /*Not correct occurrence? Keep looking.*/
            call tell  commas(jj)                /*display Nth number with #divs*/
            leave                                /*found a number, so now get the next I*/
            end   /*j*/
        end       /*i*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: parse arg _;  do j=length(_)-3  to 1  by -3; _=insert(',', _, j); end;    return _
pPow:   numeric digits 1000;  return @.i**(i-1)  /*temporarily increase decimal digits. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
#divs: procedure; parse arg x 1 y                /*X and Y:  both set from 1st argument.*/
       if x<7  then do                           /*handle special cases for numbers < 7.*/
                    if x<3   then return x       /*   "      "      "    "  one and two.*/
                    if x<5   then return x - 1   /*   "      "      "    "  three & four*/
                    if x==5  then return 2       /*   "      "      "    "  five.       */
                    if x==6  then return 4       /*   "      "      "    "  six.        */
                    end
       odd= x // 2                               /*check if   X   is  odd  or not.      */
       if odd  then do;  #= 1;             end   /*Odd?   Assume  Pdivisors  count of 1.*/
               else do;  #= 3;    y= x%2;  end   /*Even?     "        "        "    " 3.*/
                                                 /* [↑]   start with known num of Pdivs.*/
                  do k=3  by 1+odd  while k<y    /*when doing odd numbers,  skip evens. */
                  if x//k==0  then do            /*if no remainder, then found a divisor*/
                                   #=#+2;  y=x%k /*bump  #  Pdivs,  calculate limit  Y. */
                                   if k>=y  then do;  #= #-1;  leave;  end      /*limit?*/
                                   end                                          /*  ___ */
                              else if k*k>x  then leave        /*only divide up to √ x  */
                  end   /*k*/                    /* [↑]  this form of DO loop is faster.*/
       return #+1                                /*bump "proper divisors" to "divisors".*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPrime: procedure; parse arg #;         if wordpos(#, '2 3 5 7 11 13')\==0  then return 1
         if #<2  then return 0;    if #//2==0 | #//3==0 | #//5==0 | #//7==0  then return 0
                                         if # // 2==0 | # // 3    ==0  then return 0
           do j=11  by 6  until j*j>#;   if # // j==0 | # // (J+2)==0  then return 0
           end   /*j*/                           /*           ___                       */
         return 1                                /*Exceeded  √ #  ?    Then # is prime. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell:    parse arg _;     say center(i, 10)   right(_, max(w, length(_) ) )
         if i//5==0  then say;     return        /*display a separator for the eyeballs.*/
```

{{out|output|text=  when using the input:     <tt> 45 </tt>}}

(Shown at   <big>'''<sup>3</sup>/<sub>4</sub>'''</big>   size.)
<pre style="font-size:75%">
─divisors─ ───────────────────────────────────────────the Nth number with exactly N divisors──────────────────────────────────────────────
    1                                                                                                                                    1
    2                                                                                                                                    3
    3                                                                                                                                   25
    4                                                                                                                                   14
    5                                                                                                                               14,641
    6                                                                                                                                   44
    7                                                                                                                           24,137,569
    8                                                                                                                                   70
    9                                                                                                                                1,089
    10                                                                                                                                 405
    11                                                                                                                 819,628,286,980,801
    12                                                                                                                                 160
    13                                                                                                          22,563,490,300,366,186,081
    14                                                                                                                               2,752
    15                                                                                                                               9,801
    16                                                                                                                                 462
    17                                                                                              21,559,177,407,076,402,401,757,871,041
    18                                                                                                                               1,044
    19                                                                                         740,195,513,856,780,056,217,081,017,732,809
    20                                                                                                                               1,520
    21                                                                                                                             141,376
    22                                                                                                                              84,992
    23                                                                           1,658,509,762,573,818,415,340,429,240,403,156,732,495,289
    24                                                                                                                               1,170
    25                                                                                                                          52,200,625
    26                                                                                                                             421,888
    27                                                                                                                              52,900
    28                                                                                                                               9,152
    29                                                       1,116,713,952,456,127,112,240,969,687,448,211,536,647,543,601,817,400,964,721
    30                                                                                                                               6,768
    31                                               1,300,503,809,464,370,725,741,704,158,412,711,229,899,345,159,119,325,157,292,552,449
    32                                                                                                                               3,990
    33                                                                                                                          12,166,144
    34                                                                                                                           9,764,864
    35                                                                                                                         446,265,625
    36                                                                                                                               5,472
    37                          11,282,036,144,040,442,334,289,838,466,416,927,162,302,790,252,609,308,623,697,164,994,458,730,076,798,801
    38                                                                                                                          43,778,048
    39                                                                                                                          90,935,296
    40                                                                                                                              10,416
    41           1,300,532,588,674,810,624,476,094,551,095,787,816,112,173,600,565,095,470,117,230,812,218,524,514,342,511,947,837,104,801
    42                                                                                                                              46,400
    43     635,918,448,514,386,699,807,643,535,977,466,343,285,944,704,172,890,141,356,181,792,680,152,445,568,879,925,105,775,366,910,081
    44                                                                                                                             240,640
    45                                                                                                                             327,184

```



### more optimization

Programming note:   this REXX version has major optimization, and the logic flow is:
:::*   build a table of prime numbers (this also helps winnow the numbers being tested).
:::*   the generation of the sequence is broken into three parts:
::::::*   odd prime numbers.
::::::*   odd non-prime numbers.
::::::*   even numbers.

This REXX version (unlike the 1<sup>st</sup> version),   only goes through the numbers once, instead
of looking for numbers that have specific number of divisors. 

```rexx
/*REXX program  finds and displays  the    Nth  number   with exactly   N   divisors.   */
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N= 15                    /*Not specified?  Then use the default.*/
if N>=50  then numeric digits 10                 /*use more decimal digits for large N. */
@.1= 2;               Ps= 1;    !.= 0;    !.1= 2 /*1st prime;  number of primes (so far)*/
        do p=3  until Ps==N**3                   /* [↓]  gen N primes, store in @ array.*/
        if \isPrime(p)  then iterate;     Ps= Ps + 1;    if Ps<=N  then  @.Ps= p;   !.p= 1
        end   /*p*/

zfin.= 0;    zcnt. = 0;  znum.1= 1;  znum.2= 3   /*completed;   index;   count of items.*/
w= 50                                            /*──────────handle odd primes──────────*/
     do j=3  by 2  to N;  if \!.j  then iterate  /*Not prime?  Then skip this odd number*/
     zfin.j= 1;   zcnt.j= j;   znum.j= pPow();   /*compute # divisors for this odd prime*/
     w= max(w, length( commas( znum.j) ) )       /*the last prime will be the biggest #.*/
     end   /*j*/                                 /*process a small number of primes ≤ N.*/
dd.=;                     mx= 200000             /*──────────handle odd non─primes──────*/
     do j=3  by 2  to N;  if !.j  then iterate   /*Is a prime?  Then skip this odd prime*/
        do sq=6;  _= sq*sq                       /*step through squares starting at  36.*/
        if dd._\=='' then d= dd._                /*maybe use a pre─computed # divisors. */
                     else d= #divs(_)            /*Not defined?  Then calculate # divs. */
        if _<=mx  then dd._= d                   /*use memoization for the  evens  loop.*/
        if d\==j  then iterate                   /*if not the right D, then skip this sq*/
        zcnt.d= zcnt.d+1;         if zcnt.d==d  then zfin.d= 1;        znum.d= _
        if zfin.d  then iterate j                /*if all were found,  then do next odd#*/
        end   /*sq*/
     end      /*j*/
                                                 /*──────────handle even numbers.───────*/
     do j=4  by 2; if dd.j\=='' then d= dd.j     /*maybe use a pre─computed # divisors. */
                                else d= #divs(j) /*Not defined?  Then calculate # divs. */
     if d>N       then iterate                   /*Divisors greater than N?  Then skip. */
     if zfin.d    then iterate                   /*Already populated?          "    "   */
                  else do; zcnt.d= zcnt.d+1;  if zcnt.d==d  then zfin.d= 1;  znum.d= j
                           if done()  then leave  /*j*/    /*Are the even #'s all done? */
                       end
     end       /*j*/

say '─divisors─'  center("the Nth number with exactly N divisors", w, '─')      /*title.*/
     do s=1  for N;  call tell  s,commas(znum.s) /*display  Nth  number with number divs*/
     end   /*s*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: parse arg _;  do c=length(_)-3  to 1  by -3; _=insert(',', _, c); end;    return _
done:      do f=N  by -1  for N-3;      if \zfin.f  then return 0;        end;    return 1
pPow:   numeric digits 2000;  return @.j**(j-1)  /*temporarily increase decimal digits. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
#divs: procedure; parse arg x 1 y                /*X and Y:  both set from 1st argument.*/
       if x<7  then do                           /*handle special cases for numbers < 7.*/
                    if x<3   then return x       /*   "      "      "    "  one and two.*/
                    if x<5   then return x - 1   /*   "      "      "    "  three & four*/
                    if x==5  then return 2       /*   "      "      "    "  five.       */
                    if x==6  then return 4       /*   "      "      "    "  six.        */
                    end
       odd= x // 2                               /*check if   X   is  odd  or not.      */
       if odd  then do;  #= 1;             end   /*Odd?   Assume  Pdivisors  count of 1.*/
               else do;  #= 3;    y= x%2;  end   /*Even?     "        "        "    " 3.*/
                                                 /* [↑]   start with known num of Pdivs.*/
                  do k=3  by 1+odd  while k<y    /*when doing odd numbers,  skip evens. */
                  if x//k==0  then do            /*if no remainder, then found a divisor*/
                                   #=#+2;  y=x%k /*bump  #  Pdivs,  calculate limit  Y. */
                                   if k>=y  then do;  #= #-1;  leave;  end      /*limit?*/
                                   end                                          /*  ___ */
                              else if k*k>x  then leave        /*only divide up to √ x  */
                  end   /*k*/                    /* [↑]  this form of DO loop is faster.*/
       return #+1                                /*bump "proper divisors" to "divisors".*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPrime: procedure; parse arg # . '' -1 _
         if #<31  then do;   if wordpos(#, '2 3 5 7 11 13 17 19 23 29')\==0  then return 1
                             if #<2  then return 0
                       end
         if #// 2==0 then return 0; if #// 3==0  then return 0; if     _==5  then return 0
         if #// 7==0 then return 0; if #//11==0  then return 0; if #//11==0  then return 0
         if #//13==0 then return 0; if #//17==0  then return 0; if #//19==0  then return 0
                               do i=23  by 6  until i*i>#;   if #// i   ==0  then return 0
                                                             if #//(i+2)==0  then return 0
                               end   /*i*/       /*           ___                       */
         return 1                                /*Exceeded  √ #  ?    Then # is prime. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell:    parse arg idx,_;            say center(idx, 10)   right(_, w)
         if idx//5==0  then say;     return      /*display a separator for the eyeballs.*/
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}} 




## Sidef


```ruby
func f(n {.is_prime}) {
    n.prime**(n-1)
}

func f(n) {
    n.th { .sigma0 == n }
}

say 20.of { f(_+1) }
```

{{out}}

```txt

[1, 3, 25, 14, 14641, 44, 24137569, 70, 1089, 405, 819628286980801, 160, 22563490300366186081, 2752, 9801, 462, 21559177407076402401757871041, 1044, 740195513856780056217081017732809, 1520]

```



## zkl

{{trans|Go}}
Using GMP (GNU Multiple Precision Arithmetic Library, probabilistic
primes), because it is easy and fast to generate primes.

[[Extensible prime generator#zkl]] could be used instead.

```zkl
var [const] BI=Import("zklBigNum"), pmax=25;  // libGMP
p:=BI(1);
primes:=pmax.pump(List(0), p.nextPrime, "copy");  //-->(0,3,5,7,11,13,17,19,...)
 
fcn countDivisors(n){
   count:=1;
   while(n%2==0){ n/=2; count+=1; }
   foreach d in ([3..*,2]){
      q,r := n/d, n%d;
      if(r==0){
	 dc:=0;
	 while(r==0){
	    dc+=count;
	    n,q,r = q, n/d, n%d;
	 }
	 count+=dc;
      }
      if(d*d > n) break;
   }
   if(n!=1) count*=2;
   count
}

println("The first ", pmax, " terms in the sequence are:");
foreach i in ([1..pmax]){
   if(BI(i).probablyPrime()) println("%2d : %,d".fmt(i,primes[i].pow(i-1)));
   else{
      count:=0;
      foreach j in ([1..*]){
         if(i%2==1 and j != j.toFloat().sqrt().toInt().pow(2)) continue;
	 if(countDivisors(j) == i){
	    count+=1;
	    if(count==i){
	       println("%2d : %,d".fmt(i,j));
	       break;
	    }
	 }
      }
   }
}
```

{{out}}

```txt

The first 25 terms in the sequence are:
 1 : 1
 2 : 3
 3 : 25
 4 : 14
 5 : 14,641
 6 : 44
 7 : 24,137,569
 8 : 70
 9 : 1,089
10 : 405
11 : 819,628,286,980,801
12 : 160
13 : 22,563,490,300,366,186,081
14 : 2,752
15 : 9,801
16 : 462
17 : 21,559,177,407,076,402,401,757,871,041
18 : 1,044
19 : 740,195,513,856,780,056,217,081,017,732,809
20 : 1,520
21 : 141,376
22 : 84,992
23 : 1,658,509,762,573,818,415,340,429,240,403,156,732,495,289
24 : 1,170
25 : 52,200,625

```

