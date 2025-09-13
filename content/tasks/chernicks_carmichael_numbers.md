+++
title = "Chernick's Carmichael numbers"
description = ""
date = 2019-08-27T16:07:47Z
aliases = []
[extra]
id = 22349
[taxonomies]
categories = ["task"]
tags = []
+++

In 1939, Jack Chernick proved that, for '''n ≥ 3''' and '''m ≥ 1''':

    U(n, m) = (6m + 1) * (12m + 1) * Product_{i=1..n-2} (2^i * 9m + 1)

is a [https://en.wikipedia.org/wiki/Carmichael_number Carmichael number] if all the factors are primes and, for '''n > 4''', '''m''' is a multiple of '''2^(n-4)'''.


;Example

    U(3, m) = (6m + 1) * (12m + 1) * (18m + 1)
    U(4, m) = U(3, m) * (2^2 * 9m + 1)
    U(5, m) = U(4, m) * (2^3 * 9m + 1)
    ...
    U(n, m) = U(n-1, m) * (2^(n-2) * 9m + 1)

* The smallest Chernick's Carmichael number with '''3''' prime factors, is: U(3, 1) = 1729.
* The smallest Chernick's Carmichael number with '''4''' prime factors, is: U(4, 1) = 63973.
* The smallest Chernick's Carmichael number with '''5''' prime factors, is: U(5, 380) = 26641259752490421121.


For '''n = 5''', the smallest number '''m''' that satisfy Chernick's conditions, is '''m = 380''', therefore '''U(5, 380)''' is the smallest Chernick's Carmichael number with '''5''' prime factors.

'''U(5, 380)''' is a Chernick's Carmichael number because '''m = 380''' is a multiple of '''2^(n-4)''', where '''n = 5''', and the factors { (6*380 + 1), (12*380 + 1), (18*380 + 1), (36*380 + 1), (72*380 + 1) } are all prime numbers.


## Task

For '''n ≥ 3''', let '''a(n)''' be the smallest Chernick's Carmichael number with '''n''' prime factors.

* Compute '''a(n)''' for '''n = 3..9'''.
* Optional: find '''a(10)'''.


'''Note''': it's perfectly acceptable to show the terms in factorized form:

  a(3) = 7 * 13 * 19
  a(4) = 7 * 13 * 19 * 37
  a(5) = 2281 * 4561 * 6841 * 13681 * 27361
  ...


## See also

* [http://www.ams.org/journals/bull/1939-45-04/S0002-9904-1939-06953-X/S0002-9904-1939-06953-X.pdf Jack Chernick, On Fermat's simple theorem (PDF)]

* [https://oeis.org/A318646 OEIS A318646: The least Chernick's "universal form" Carmichael number with n prime factors]


## Related tasks

* [[Carmichael 3 strong pseudoprimes]]






## C

```c
#include <stdio.h>
#include <stdlib.h>
#include <gmp.h>

typedef unsigned long long int u64;

#define TRUE 1
#define FALSE 0

int primality_pretest(u64 k) {
    if (!(k %  3) || !(k %  5) || !(k %  7) || !(k % 11) || !(k % 13) || !(k % 17) || !(k % 19) || !(k % 23)) return (k <= 23);
    return TRUE;
}

int probprime(u64 k, mpz_t n) {
    mpz_set_ui(n, k);
    return mpz_probab_prime_p(n, 0);
}

int is_chernick(int n, u64 m, mpz_t z) {
    u64 t = 9 * m;
    if (primality_pretest(6 * m + 1) == FALSE) return FALSE;
    if (primality_pretest(12 * m + 1) == FALSE) return FALSE;
    for (int i = 1; i <= n - 2; i++) if (primality_pretest((t << i) + 1) == FALSE) return FALSE;
    if (probprime(6 * m + 1, z) == FALSE) return FALSE;
    if (probprime(12 * m + 1, z) == FALSE) return FALSE;
    for (int i = 1; i <= n - 2; i++) if (probprime((t << i) + 1, z) == FALSE) return FALSE;
    return TRUE;
}

int main(int argc, char const *argv[]) {
    mpz_t z;
    mpz_inits(z, NULL);

    for (int n = 3; n <= 10; n ++) {
        u64 multiplier = (n > 4) ? (1 << (n - 4)) : 1;

        if (n > 5) multiplier *= 5;

        for (u64 k = 1; ; k++) {
            u64 m = k * multiplier;

            if (is_chernick(n, m, z) == TRUE) {
                printf("a(%d) has m = %llu\n", n, m);
                break;
            }
        }
    }

    return 0;
}
```

```txt

a(3) has m = 1
a(4) has m = 1
a(5) has m = 380
a(6) has m = 380
a(7) has m = 780320
a(8) has m = 950560
a(9) has m = 950560
a(10) has m = 3208386195840

```




## C++

```cpp
#include <gmp.h>
#include <iostream>

using namespace std;

typedef unsigned long long int u64;

bool primality_pretest(u64 k) {     // for k > 23

    if (!(k %  3) || !(k %  5) || !(k %  7) || !(k % 11) ||
        !(k % 13) || !(k % 17) || !(k % 19) || !(k % 23)
    ) {
        return (k <= 23);
    }

    return true;
}

bool probprime(u64 k, mpz_t n) {
    mpz_set_ui(n, k);
    return mpz_probab_prime_p(n, 0);
}

bool is_chernick(int n, u64 m, mpz_t z) {

    if (!primality_pretest(6 * m + 1)) {
        return false;
    }

    if (!primality_pretest(12 * m + 1)) {
        return false;
    }

    u64 t = 9 * m;

    for (int i = 1; i <= n - 2; i++) {
        if (!primality_pretest((t << i) + 1)) {
            return false;
        }
    }

    if (!probprime(6 * m + 1, z)) {
        return false;
    }

    if (!probprime(12 * m + 1, z)) {
        return false;
    }

    for (int i = 1; i <= n - 2; i++) {
        if (!probprime((t << i) + 1, z)) {
            return false;
        }
    }

    return true;
}

int main() {

    mpz_t z;
    mpz_inits(z, NULL);

    for (int n = 3; n <= 10; n++) {

        // `m` is a multiple of 2^(n-4), for n > 4
        u64 multiplier = (n > 4) ? (1 << (n - 4)) : 1;

        // For n > 5, m is also a multiple of 5
        if (n > 5) {
            multiplier *= 5;
        }

        for (u64 k = 1; ; k++) {

            u64 m = k * multiplier;

            if (is_chernick(n, m, z)) {
                cout << "a(" << n << ") has m = " << m << endl;
                break;
            }
        }
    }

    return 0;
}
```

```txt

a(3) has m = 1
a(4) has m = 1
a(5) has m = 380
a(6) has m = 380
a(7) has m = 780320
a(8) has m = 950560
a(9) has m = 950560
a(10) has m = 3208386195840

```

(takes ~3.5 minutes)

=={{header|F_Sharp|F#}}==
This task uses [http://www.rosettacode.org/wiki/Extensible_prime_generator#The_function Extensible Prime Generator (F#)]

```fsharp

// Generate Chernick's Carmichael numbers. Nigel Galloway: June 1st., 2019
let fMk m k=isPrime(6*m+1) && isPrime(12*m+1) && [1..k-2]|>List.forall(fun n->isPrime(9*(pown 2 n)*m+1))
let fX k=Seq.initInfinite(fun n->(n+1)*(pown 2 (k-4))) |> Seq.filter(fun n->fMk n k )
let cherCar k=let m=Seq.head(fX k) in printfn "m=%d primes -> %A " m ([6*m+1;12*m+1]@List.init(k-2)(fun n->9*(pown 2 (n+1))*m+1))
[4..9] |> Seq.iter cherCar

```

```txt

cherCar(4): m=1 primes -> [7; 13; 19; 37]
cherCar(5): m=380 primes -> [2281; 4561; 6841; 13681; 27361]
cherCar(6): m=380 primes -> [2281; 4561; 6841; 13681; 27361; 54721]
cherCar(7): m=780320 primes -> [4681921; 9363841; 14045761; 28091521; 56183041; 112366081; 224732161]
cherCar(8): m=950560 primes -> [5703361; 11406721; 17110081; 34220161; 68440321; 136880641; 273761281; 547522561]
cherCar(9): m=950560 primes -> [5703361; 11406721; 17110081; 34220161; 68440321; 136880641; 273761281; 547522561; 1095045121]

```



## Go


### Basic only


```go
package main

import (
    "fmt"
    "math/big"
)

var (
    zero = new(big.Int)
    prod = new(big.Int)
    fact = new(big.Int)
)

func ccFactors(n, m uint64) (*big.Int, bool) {
    prod.SetUint64(6*m + 1)
    if !prod.ProbablyPrime(0) {
        return zero, false
    }
    fact.SetUint64(12*m + 1)
    if !fact.ProbablyPrime(0) { // 100% accurate up to 2 ^ 64
        return zero, false
    }
    prod.Mul(prod, fact)
    for i := uint64(1); i <= n-2; i++ {
        fact.SetUint64((1<<i)*9*m + 1)
        if !fact.ProbablyPrime(0) {
            return zero, false
        }
        prod.Mul(prod, fact)
    }
    return prod, true
}

func ccNumbers(start, end uint64) {
    for n := start; n <= end; n++ {
        m := uint64(1)
        if n > 4 {
            m = 1 << (n - 4)
        }
        for {
            num, ok := ccFactors(n, m)
            if ok {
                fmt.Printf("a(%d) = %d\n", n, num)
                break
            }
            if n <= 4 {
                m++
            } else {
                m += 1 << (n - 4)
            }
        }
    }
}

func main() {
    ccNumbers(3, 9)
}
```


```txt

a(3) = 1729
a(4) = 63973
a(5) = 26641259752490421121
a(6) = 1457836374916028334162241
a(7) = 24541683183872873851606952966798288052977151461406721
a(8) = 53487697914261966820654105730041031613370337776541835775672321
a(9) = 58571442634534443082821160508299574798027946748324125518533225605795841

```


### Basic plus optional

To reach a(10) in a reasonable time, a much more efficient approach is needed.

The following version takes account of the optimizations referred to in the Talk page and previewed in the C++ entry above.

It also uses a wrapper for the C library, GMP, which despite the overhead of cgo is still much faster than Go's native big.Int library.

The resulting executable is several hundred times faster than before and, even on my modest Celeron @1.6GHZ, reaches a(9) in under 10ms and a(10) in about 22 minutes.

```go
package main

import (
    "fmt"
    big "github.com/ncw/gmp"
)

const (
    min = 3
    max = 10
)

var (
    prod       = new(big.Int)
    fact       = new(big.Int)
    factors    = [max]uint64{}
    bigFactors = [max]*big.Int{}
)

func init() {
    for i := 0; i < max; i++ {
        bigFactors[i] = big.NewInt(0)
    }
}

func isPrimePretest(k uint64) bool {
    if k%3 == 0 || k%5 == 0 || k%7 == 0 || k%11 == 0 ||
        k%13 == 0 || k%17 == 0 || k%19 == 0 || k%23 == 0 {
        return k <= 23
    }
    return true
}

func ccFactors(n, m uint64) bool {
    if !isPrimePretest(6*m + 1) {
        return false
    }
    if !isPrimePretest(12*m + 1) {
        return false
    }
    factors[0] = 6*m + 1
    factors[1] = 12*m + 1
    t := 9 * m
    for i := uint64(1); i <= n-2; i++ {
        tt := (t << i) + 1
        if !isPrimePretest(tt) {
            return false
        }
        factors[i+1] = tt
    }

    for i := 0; i < int(n); i++ {
        fact.SetUint64(factors[i])
        if !fact.ProbablyPrime(0) {
            return false
        }
        bigFactors[i].Set(fact)
    }
    return true
}

func prodFactors(n uint64) *big.Int {
    prod.Set(bigFactors[0])
    for i := 1; i < int(n); i++ {
        prod.Mul(prod, bigFactors[i])
    }
    return prod
}

func ccNumbers(start, end uint64) {
    for n := start; n <= end; n++ {
        mult := uint64(1)
        if n > 4 {
            mult = 1 << (n - 4)
        }
        if n > 5 {
            mult *= 5
        }
        m := mult
        for {
            if ccFactors(n, m) {
                num := prodFactors(n)
                fmt.Printf("a(%d) = %d\n", n, num)
                fmt.Printf("m(%d) = %d\n", n, m)
                fmt.Println("Factors:", factors[:n], "\n")
                break
            }
            m += mult
        }
    }
}

func main() {
    ccNumbers(min, max)
}
```


```txt

a(3) = 1729
m(3) = 1
Factors: [7 13 19]

a(4) = 63973
m(4) = 1
Factors: [7 13 19 37]

a(5) = 26641259752490421121
m(5) = 380
Factors: [2281 4561 6841 13681 27361]

a(6) = 1457836374916028334162241
m(6) = 380
Factors: [2281 4561 6841 13681 27361 54721]

a(7) = 24541683183872873851606952966798288052977151461406721
m(7) = 780320
Factors: [4681921 9363841 14045761 28091521 56183041 112366081 224732161]

a(8) = 53487697914261966820654105730041031613370337776541835775672321
m(8) = 950560
Factors: [5703361 11406721 17110081 34220161 68440321 136880641 273761281 547522561]

a(9) = 58571442634534443082821160508299574798027946748324125518533225605795841
m(9) = 950560
Factors: [5703361 11406721 17110081 34220161 68440321 136880641 273761281 547522561 1095045121]

a(10) = 24616075028246330441656912428380582403261346369700917629170235674289719437963233744091978433592331048416482649086961226304033068172880278517841921
m(10) = 3208386195840
Factors: [19250317175041 38500634350081 57750951525121 115501903050241 231003806100481 462007612200961 924015224401921 1848030448803841 3696060897607681 7392121795215361]

```



## Julia


```julia
using Primes

function trial_pretest(k::UInt64)

    if ((k %  3)==0 || (k %  5)==0 || (k %  7)==0 || (k % 11)==0 ||
        (k % 13)==0 || (k % 17)==0 || (k % 19)==0 || (k % 23)==0)
        return (k <= 23)
    end

    return true
end

function gcd_pretest(k::UInt64)

    if (k <= 107)
        return true
    end

    gcd(29*31*37*41*43*47*53*59*61*67, k) == 1 &&
    gcd(71*73*79*83*89*97*101*103*107, k) == 1
end

function is_chernick(n::Int64, m::UInt64)

    t = 9*m

    if (!trial_pretest(6*m + 1))
        return false
    end

    if (!trial_pretest(12*m + 1))
        return false
    end

    for i in 1:n-2
        if (!trial_pretest((t << i) + 1))
            return false
        end
    end

    if (!gcd_pretest(6*m + 1))
        return false
    end

    if (!gcd_pretest(12*m + 1))
        return false
    end

    for i in 1:n-2
        if (!gcd_pretest((t << i) + 1))
            return false
        end
    end

    if (!isprime(6*m + 1))
        return false
    end

    if (!isprime(12*m + 1))
        return false
    end

    for i in 1:n-2
        if (!isprime((t << i) + 1))
            return false
        end
    end

    return true
end

function chernick_carmichael(n::Int64, m::UInt64)
    prod = big(1)

    prod *= 6*m + 1
    prod *= 12*m + 1

    for i in 1:n-2
        prod *= ((big(9)*m)<<i) + 1
    end

    prod
end

function cc_numbers(from, to)

    for n in from:to

        multiplier = 1

        if (n > 4) multiplier = 1 << (n-4) end
        if (n > 5) multiplier *= 5 end

        m = UInt64(multiplier)

        while true

            if (is_chernick(n, m))
                println("a(", n, ") = ", chernick_carmichael(n, m))
                break
            end

            m += multiplier
        end
    end
end

cc_numbers(3, 10)
```


```txt

a(3) = 1729
a(4) = 63973
a(5) = 26641259752490421121
a(6) = 1457836374916028334162241
a(7) = 24541683183872873851606952966798288052977151461406721
a(8) = 53487697914261966820654105730041031613370337776541835775672321
a(9) = 58571442634534443082821160508299574798027946748324125518533225605795841
a(10) = 24616075028246330441656912428380582403261346369700917629170235674289719437963233744091978433592331048416482649086961226304033068172880278517841921

```


(takes ~6.5 minutes)


## PARI/GP


```parigp

cherCar(n)={
  my(C=vector(n));C[1]=6; C[2]=12; for(g=3,n,C[g]=2^(g-2)*9);
  my(i=1); my(N(g)=while(i<=n&ispseudoprime(g*C[i]+1),i=i+1); return(i>n));
     i=1;  my(G(g)=while(i<=n&isprime(g*C[i]+1),i=i+1); return(i>n));
  i=1; if(n>4,i=2^(n-4)); if(n>5,i=i*5); my(m=i); while(!(N(m)&G(m)),m=m+i);
  printf("cherCar(%d): m = %d\n",n,m)}
for(x=3,9,cherCar(x))

```

```txt

cherCar(3): m = 1
cherCar(4): m = 1
cherCar(5): m = 380
cherCar(6): m = 380
cherCar(7): m = 780320
cherCar(8): m = 950560
cherCar(9): m = 950560
cherCar(10): m = 3208386195840

```



## Perl

```perl
use 5.020;
use warnings;
use ntheory qw/:all/;
use experimental qw/signatures/;

sub chernick_carmichael_factors ($n, $m) {
    (6*$m + 1, 12*$m + 1, (map { (1 << $_) * 9*$m + 1 } 1 .. $n-2));
}

sub chernick_carmichael_number ($n, $callback) {

    my $multiplier = ($n > 4) ? (1 << ($n-4)) : 1;

    for (my $m = 1 ; ; ++$m) {
        my @f = chernick_carmichael_factors($n, $m * $multiplier);
        next if not vecall { is_prime($_) } @f;
        $callback->(@f);
        last;
    }
}

foreach my $n (3..9) {
    chernick_carmichael_number($n, sub (@f) { say "a($n) = ", vecprod(@f) });
}
```


```txt

a(3) = 1729
a(4) = 63973
a(5) = 26641259752490421121
a(6) = 1457836374916028334162241
a(7) = 24541683183872873851606952966798288052977151461406721
a(8) = 53487697914261966820654105730041031613370337776541835775672321
a(9) = 58571442634534443082821160508299574798027946748324125518533225605795841

```



## Perl 6

Use the ntheory library from Perl 5 for primality testing since it is much, ''much'' faster than Perl 6s built-in .is-prime method.


```perl6
use Inline::Perl5;
use ntheory:from<Perl5> <:all>;

sub chernick-factors ($n, $m) {
    6*$m + 1, 12*$m + 1, |((1 .. $n-2).map: { (1 +< $_) * 9*$m + 1 } )
}

sub chernick-carmichael-number ($n) {

    my $multiplier = 1 +< (($n-4) max 0);
    my $iterator   = $n < 5 ?? (1 .. *) !! (1 .. *).map: * * 5;

    $multiplier * $iterator.first: -> $m {
        [&&] chernick-factors($n, $m * $multiplier).map: { is_prime($_) }
    }

}

for 3 .. 9 -> $n {
    my $m = chernick-carmichael-number($n);
    my @f = chernick-factors($n, $m);
    say "U($n, $m): {[*] @f} = {@f.join(' ⨉ ')}";
}
```

```txt
U(3, 1): 1729 = 7 ⨉ 13 ⨉ 19
U(4, 1): 63973 = 7 ⨉ 13 ⨉ 19 ⨉ 37
U(5, 380): 26641259752490421121 = 2281 ⨉ 4561 ⨉ 6841 ⨉ 13681 ⨉ 27361
U(6, 380): 1457836374916028334162241 = 2281 ⨉ 4561 ⨉ 6841 ⨉ 13681 ⨉ 27361 ⨉ 54721
U(7, 780320): 24541683183872873851606952966798288052977151461406721 = 4681921 ⨉ 9363841 ⨉ 14045761 ⨉ 28091521 ⨉ 56183041 ⨉ 112366081 ⨉ 224732161
U(8, 950560): 53487697914261966820654105730041031613370337776541835775672321 = 5703361 ⨉ 11406721 ⨉ 17110081 ⨉ 34220161 ⨉ 68440321 ⨉ 136880641 ⨉ 273761281 ⨉ 547522561
U(9, 950560): 58571442634534443082821160508299574798027946748324125518533225605795841 = 5703361 ⨉ 11406721 ⨉ 17110081 ⨉ 34220161 ⨉ 68440321 ⨉ 136880641 ⨉ 273761281 ⨉ 547522561 ⨉ 1095045121
```



## Phix

```Phix
function chernick_carmichael_factors(integer n, m)
    sequence res = {6*m + 1, 12*m + 1}
    for i=1 to n-2 do
        res &= power(2,i) * 9*m + 1
    end for
    return res
end function

include mpfr.e
mpz p = mpz_init()
randstate state = gmp_randinit_mt()

function m_prime(atom a)
    mpz_set_d(p,a)
    return mpz_probable_prime_p(p, state)
end function

function is_chernick_carmichael(integer n, m)
    return iff(n==2 ? m_prime(6*m + 1) and m_prime(12*m + 1)
                    : m_prime(power(2,n-2) * 9*m + 1) and
                      is_chernick_carmichael(n-1, m))
end function

function chernick_carmichael_number(integer n)
    integer multiplier = iff(n>4 ? power(2,n-4) : 1), m = 1
    while not is_chernick_carmichael(n, m * multiplier) do m += 1 end while
    return chernick_carmichael_factors(n, m * multiplier)
end function

for n=3 to 9 do
    sequence f = chernick_carmichael_number(n)
    for i=1 to length(f) do f[i] = sprintf("%d",f[i]) end for
    printf(1,"a(%d) = %s\n",{n,join(f," * ")})
end for
```

```txt

a(3) = 7 * 13 * 19
a(4) = 7 * 13 * 19 * 37
a(5) = 2281 * 4561 * 6841 * 13681 * 27361
a(6) = 2281 * 4561 * 6841 * 13681 * 27361 * 54721
a(7) = 4681921 * 9363841 * 14045761 * 28091521 * 56183041 * 112366081 * 224732161
a(8) = 5703361 * 11406721 * 17110081 * 34220161 * 68440321 * 136880641 * 273761281 * 547522561
a(9) = 5703361 * 11406721 * 17110081 * 34220161 * 68440321 * 136880641 * 273761281 * 547522561 * 1095045121

```

Pleasingly fast, note however that a(10) remains well out of reach / would probably need a complete rewrite.


## Sidef


```ruby
func chernick_carmichael_factors (n, m) {
    [6*m + 1, 12*m + 1, {|i| 2**i * 9*m + 1 }.map(1 .. n-2)...]
}

func is_chernick_carmichael (n, m) {
    (n == 2) ? (is_prime(6*m + 1) && is_prime(12*m + 1))
             : (is_prime(2**(n-2) * 9*m + 1) && __FUNC__(n-1, m))
}

func chernick_carmichael_number(n, callback) {
    var multiplier = (n>4 ? 2**(n-4) : 1)
    var m = (1..Inf -> first {|m| is_chernick_carmichael(n, m * multiplier) })
    var f = chernick_carmichael_factors(n, m * multiplier)
    callback(f...)
}

for n in (3..9) {
    chernick_carmichael_number(n, {|*f| say "a(#{n}) = #{f.join(' * ')}" })
}
```


```txt

a(3) = 7 * 13 * 19
a(4) = 7 * 13 * 19 * 37
a(5) = 2281 * 4561 * 6841 * 13681 * 27361
a(6) = 2281 * 4561 * 6841 * 13681 * 27361 * 54721
a(7) = 4681921 * 9363841 * 14045761 * 28091521 * 56183041 * 112366081 * 224732161
a(8) = 5703361 * 11406721 * 17110081 * 34220161 * 68440321 * 136880641 * 273761281 * 547522561
a(9) = 5703361 * 11406721 * 17110081 * 34220161 * 68440321 * 136880641 * 273761281 * 547522561 * 1095045121

```



## zkl

{{libheader|GMP}} GNU Multiple Precision Arithmetic Library
Using GMP (probabilistic primes),
because it is easy and fast to check primeness.

```zkl
var [const] BI=Import("zklBigNum");  // libGMP

fcn ccFactors(n,m){	// not re-entrant
   prod:=BI(6*m + 1);
   if(not prod.probablyPrime())    return(False);
   fact:=BI(12*m + 1);
   if(not fact.probablyPrime())    return(False);
   prod.mul(fact);
   foreach i in ([1..n-2]){
      fact.set((2).pow(i) *9*m + 1);
      if(not fact.probablyPrime()) return(False);
      prod.mul(fact);
   }
   prod
}

fcn ccNumbers(start,end){
   foreach n in ([start..end]){
      a,m := ( if(n<=4) 1  else (2).pow(n - 4) ), a;
      while(1){
	 if(num := ccFactors(n,m)){
	    println("a(%d) = %,d".fmt(n,num));
	    break;
	 }
	 m+=a;
      }
   }
}
```


```zkl
ccNumbers(3,9);
```

```txt

a(3) = 1,729
a(4) = 63,973
a(5) = 26,641,259,752,490,421,121
a(6) = 1,457,836,374,916,028,334,162,241
a(7) = 24,541,683,183,872,873,851,606,952,966,798,288,052,977,151,461,406,721
a(8) = 53,487,697,914,261,966,820,654,105,730,041,031,613,370,337,776,541,835,775,672,321
a(9) = 58,571,442,634,534,443,082,821,160,508,299,574,798,027,946,748,324,125,518,533,225,605,795,841

```

