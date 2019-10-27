+++
title = "Fermat numbers"
description = ""
date = 2019-10-12T11:26:06Z
aliases = []
[extra]
id = 22475
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

In mathematics, a Fermat number, ''named after Pierre de Fermat who first studied them,'' is a positive integer of the form  <big>'''F<sub>n</sub> = 2<sup>''2''<sup>''n''</sup></sup> + 1'''</big> where '''n''' is a non-negative integer.

Despite the simplicity of generating Fermat numbers, they have some powerful mathematical properties and are extensively used in cryptography & pseudo-random number generation, and are often linked to other number theoric fields.

As of this writing, (mid 2019), there are only five known prime Fermat numbers, the first five ('''F<sub>0</sub>''' through '''F<sub>4</sub>'''). Only the first twelve Fermat numbers have been completely factored, though many have been partially factored.


;Task:

:* Write a routine (function, procedure, whatever) to generate '''Fermat numbers'''.

:* Use the routine to find and display here, on this page, the first '''10 Fermat numbers''' - '''F<sub>0</sub>''' through '''F<sub>9</sub>'''.

:* Find and display here, on this page, the '''prime factors''' of as many Fermat numbers as you have patience for. (Or as many as can be found in five minutes or less of processing time). ''Note: if you make it past '''F<sub>11</sub>''', there may be money, and certainly will be acclaim in it for you.''


;See also:

:* '''[[wp:Fermat_number|Wikipedia - Fermat numbers]]'''
:* '''[[oeis:A000215|OEIS:A000215 - Fermat numbers]]'''
:* '''[[oeis:A019434|OEIS:A019434 - Fermat primes]]'''





## Arturo



```arturo
nPowers #(1 2 4 8 16 32 64 128 256 512)
fermatSet $(map $(range 0 9) { 2^nPowers.[&]+1 })

loop $(range 0 9) {
	print "F(" + & + ") = " + fermatSet.[&]
}

""

loop $(range 0 9) {
	print "prime factors of F(" + & + ") = " + $(primeFactors fermatSet.[&])
}

```


{{out}}


```txt
F(0) = 3
F(1) = 5
F(2) = 17
F(3) = 257
F(4) = 65537
F(5) = 4294967297
F(6) = 18446744073709551617
F(7) = 340282366920938463463374607431768211457
F(8) = 115792089237316195423570985008687907853269984665640564039457584007913129639937
F(9) = 13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084097

prime factors of F(0) = #(3)
prime factors of F(1) = #(5)
prime factors of F(2) = #(17)
prime factors of F(3) = #(257)
prime factors of F(4) = #(65537)
prime factors of F(5) = #(641 6700417)
prime factors of F(6) = #(274177 67280421310721)

[script timeout]
```



## C

Compile with : 
```txt
gcc -o fermat fermat.c -lgmp
```


```c>#include <stdlib.h

#include <stdio.h>
#include <gmp.h>

void mpz_factors(mpz_t n) {
  int factors = 0;
  mpz_t s, m, p;
  mpz_init(s), mpz_init(m), mpz_init(p);

  mpz_set_ui(m, 3);
  mpz_set(p, n);
  mpz_sqrt(s, p);

  while (mpz_cmp(m, s) < 0) {
    if (mpz_divisible_p(p, m)) {
      gmp_printf("%Zd ", m);
      mpz_fdiv_q(p, p, m);
      mpz_sqrt(s, p);
      factors ++;
    }
    mpz_add_ui(m, m, 2);
  }

  if (factors == 0) printf("PRIME\n");
  else gmp_printf("%Zd\n", p);
}

int main(int argc, char const *argv[]) {
  mpz_t fermat;
  mpz_init_set_ui(fermat, 3);
  printf("F(0) = 3 -> PRIME\n");
  for (unsigned i = 1; i < 10; i ++) {
    mpz_sub_ui(fermat, fermat, 1);
    mpz_mul(fermat, fermat, fermat);
    mpz_add_ui(fermat, fermat, 1);
    gmp_printf("F(%d) = %Zd -> ", i, fermat);
    mpz_factors(fermat);
  }

  return 0;
}
```


```txt

F(0) = 3 -> PRIME
F(1) = 5 -> PRIME
F(2) = 17 -> PRIME
F(3) = 257 -> PRIME
F(4) = 65537 -> PRIME
F(5) = 4294967297 -> 641 6700417
F(6) = 18446744073709551617 -> 274177 67280421310721
F(7) = 340282366920938463463374607431768211457 -> 59649589127497217 5704689200685129054721
F(8) = 115792089237316195423570985008687907853269984665640564039457584007913129639937 -> 1238926361552897 93461639715357977769163558199606896584051237541638188580280321
......

```



## Factor


```factor
USING: formatting io kernel lists lists.lazy math math.functions
math.primes.factors sequences ;

: lfermats ( -- list )
    0 lfrom [ [ 1 2 2 ] dip ^ ^ + ] lmap-lazy ;

CHAR: ₀ 10 lfermats ltake list>array [
    "First 10 Fermat numbers:" print 
    [ dupd "F%c = %d\n" printf 1 + ] each drop nl
] [
    "Factors of first few Fermat numbers:" print [
        dupd factors dup length 1 = " (prime)" "" ?
        "Factors of F%c: %[%d, %]%s\n" printf 1 +
    ] each drop
] 2bi
```

{{out}}

```txt

First 10 Fermat numbers:
F₀ = 3
F₁ = 5
F₂ = 17
F₃ = 257
F₄ = 65537
F₅ = 4294967297
F₆ = 18446744073709551617
F₇ = 340282366920938463463374607431768211457
F₈ = 115792089237316195423570985008687907853269984665640564039457584007913129639937
F₉ = 13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084097

Factors of first few Fermat numbers:
Factors of F₀: { 3 } (prime)
Factors of F₁: { 5 } (prime)
Factors of F₂: { 17 } (prime)
Factors of F₃: { 257 } (prime)
Factors of F₄: { 65537 } (prime)
Factors of F₅: { 641, 6700417 }
Factors of F₆: { 274177, 67280421310721 }
^D

```



## Go

The first seven Fermat numbers are factorized almost instantly by the [https://en.wikipedia.org/wiki/Pollard%27s_rho_algorithm Pollard's rho algorithm] but F₇ took a little over 12 minutes!

However, the algorithm factorizes F₈ and finds the first prime factor of F₉ in a much more acceptable 40 seconds. The first prime factor of F₈ is 16 digits long compared to 17 digits for F₇ which seems to make a big difference in execution time.

As the second and third prime factors of F₉ are respectively 49 and 99 digits long there would be no chance of finding these any time soon so I haven't bothered.

I thought that [https://en.wikipedia.org/wiki/Fermat%27s_factorization_method Fermat's own factorization method], which is easy to code, might be able to factorize F₇ faster than Pollard's Rho as the factors are relatively close together but in fact it was much slower. 

The timings are for my Intel Core i7-8565U laptop using Go 1.12.9 on Ubuntu 18.04.

```go
package main

import (
    "fmt"
    "math/big"
)

var one = new(big.Int).SetUint64(1)
var two = new(big.Int).SetUint64(2)

func fermatNumbers(n int) (res []*big.Int) {
    f := new(big.Int).SetUint64(3) // 2^1 + 1
    for i := 0; i < n; i++ {
        t := new(big.Int).Set(f)
        res = append(res, t)
        f.Sub(f, one)
        f.Mul(f, f)
        f.Add(f, one)
    }
    return res
}

// Uses algorithm in Wikipedia article, including speed-up.
func pollardRho(n *big.Int) (*big.Int, error) {
    // g(x) = (x^2 + 1) mod n
    g := func(x, n *big.Int) *big.Int {
        x2 := new(big.Int)
        x2.Mul(x, x)
        x2.Add(x2, one)
        return x2.Mod(x2, n)
    }
    x, y, d := new(big.Int).Set(two), new(big.Int).Set(two), new(big.Int).Set(one)
    t, z := new(big.Int), new(big.Int).Set(one)
    count := 0
    for {
        x = g(x, n)
        y = g(g(y, n), n)
        t.Sub(x, y)
        t.Abs(t)
        t.Mod(t, n)
        z.Mul(z, t)
        count++
        if count == 100 {
            d.GCD(nil, nil, z, n)
            if d.Cmp(one) != 0 {
                break
            }
            z.Set(one)
            count = 0
        }
    }
    if d.Cmp(n) == 0 {
        return nil, fmt.Errorf("Pollard's rho failure")
    }
    return d, nil
}

func primeFactors(n *big.Int) (res []*big.Int, err error) {
    if n.ProbablyPrime(10) {
        return append(res, n), nil
    }
    factor1, err := pollardRho(n)
    if err != nil {
        return nil, err
    }
    if !factor1.ProbablyPrime(10) {
        return nil, fmt.Errorf("First factor is not prime")
    }
    factor2 := new(big.Int)
    factor2.Quo(n, factor1)
    if !factor2.ProbablyPrime(10) {
        return nil, fmt.Errorf("%d (second factor is not prime)", factor1)
    }
    return append(res, factor1, factor2), nil
}

func main() {
    fns := fermatNumbers(10)
    fmt.Println("First 10 Fermat numbers:")
    for i, f := range fns {
        fmt.Printf("F%c = %d\n", 0x2080+i, f)
    }

    fmt.Println("\nFactors of first 10 Fermat numbers:")
    for i, f := range fns {
        fmt.Printf("F%c = ", 0x2080+i)        
        factors, err := primeFactors(f)
        if err != nil {
            fmt.Println(err)
            continue
        }
        for _, factor := range factors {
            fmt.Printf("%d ", factor)
        }
        if len(factors) == 1 {
            fmt.Println("- prime")
        } else {
            fmt.Println()
        }
    }
}
```


{{out}}

```txt

First 10 Fermat numbers:
F₀ = 3
F₁ = 5
F₂ = 17
F₃ = 257
F₄ = 65537
F₅ = 4294967297
F₆ = 18446744073709551617
F₇ = 340282366920938463463374607431768211457
F₈ = 115792089237316195423570985008687907853269984665640564039457584007913129639937
F₉ = 13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084097

Factors of first 10 Fermat numbers:
F₀ = 3 - prime
F₁ = 5 - prime
F₂ = 17 - prime
F₃ = 257 - prime
F₄ = 65537 - prime
F₅ = 641 6700417 
F₆ = 274177 67280421310721 
F₇ = 59649589127497217 5704689200685129054721
F₈ = 1238926361552897 93461639715357977769163558199606896584051237541638188580280321 
F₉ = 2424833 (second factor is not prime)

```



## Julia


```julia
using Primes

fermat(n) = BigInt(2)^(BigInt(2)^n) + 1
prettyprint(fdict) = replace(replace(string(fdict), r".+\(([^)]+)\)" => s"\1"), r"\=\>" => "^")

function factorfermats(max, nofactor=false) 
    for n in 0:max
        fm = fermat(n)
        if nofactor
            println("Fermat number F($n) is $fm.")
            continue
        end
        factors = factor(fm)
        println("Fermat number F($n), $fm, ", 
            length(factors) < 2 ? "is prime." : "factors to $(prettyprint(factors)).")
    end
end

factorfermats(9, true)
factorfermats(10)

```
{{out}}

```txt

Fermat number F(0) is 3.
Fermat number F(1) is 5.
Fermat number F(2) is 17.
Fermat number F(3) is 257.
Fermat number F(4) is 65537.
Fermat number F(5) is 4294967297.
Fermat number F(6) is 18446744073709551617.
Fermat number F(7) is 340282366920938463463374607431768211457.
Fermat number F(8) is 115792089237316195423570985008687907853269984665640564039457584007913129639937.
Fermat number F(9) is 13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084097.
Fermat number F(0), 3, is prime.
Fermat number F(1), 5, is prime.
Fermat number F(2), 17, is prime.
Fermat number F(3), 257, is prime.
Fermat number F(4), 65537, is prime.
Fermat number F(5), 4294967297, factors to 641^1,6700417^1.
Fermat number F(6), 18446744073709551617, factors to 274177^1,67280421310721^1.
...waited >5 minutes

```



## Perl

{{libheader|ntheory}}
{{trans|Perl 6}}

```perl
use strict;
use warnings;
use feature 'say';
use bigint try=>"GMP";
use ntheory qw<factor>;

my @Fermats = map { 2**(2**$_) + 1 } 0..9;

my $sub = 0;
say 'First 10 Fermat numbers:';
printf "F%s = %s\n", $sub++, $_ for @Fermats;

$sub = 0;
say "\nFactors of first few Fermat numbers:";
for my $f (map { [factor($_)] } @Fermats[0..8]) {
   printf "Factors of F%s: %s\n", $sub++, @$f == 1 ? 'prime' : join ' ', @$f
}
```

{{out}}

```txt
First 10 Fermat numbers:
F0 = 3
F1 = 5
F2 = 17
F3 = 257
F4 = 65537
F5 = 4294967297
F6 = 18446744073709551617
F7 = 340282366920938463463374607431768211457
F8 = 115792089237316195423570985008687907853269984665640564039457584007913129639937
F9 = 13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084097

Factors of first few Fermat numbers:
Factors of F0: prime
Factors of F1: prime
Factors of F2: prime
Factors of F3: prime
Factors of F4: prime
Factors of F5: 641 6700417
Factors of F6: 274177 67280421310721
Factors of F7: 59649589127497217 5704689200685129054721
Factors of F8: 1238926361552897 93461639715357977769163558199606896584051237541638188580280321
```



## Perl 6

{{works with|Rakudo|2019.07.1}}
I gave up on factoring F₉ after about 20 minutes.


```perl6>use ntheory:from<Perl5> <factor
;

my @Fermats = (^Inf).map: 2 ** 2 ** * + 1;

my $sub = '₀';
say "First 10 Fermat numbers:";
printf "F%s = %s\n", $sub++, $_ for @Fermats[^10];

$sub = '₀';
say "\nFactors of first few Fermat numbers:";
for @Fermats[^9].map( {"$_".&factor} ) -> $f {
    printf "Factors of F%s: %s %s\n", $sub++, $f.join(' '), $f.elems == 1 ?? '- prime' !! ''
}
```

{{out}}

```txt
First 10 Fermat numbers:
F₀ = 3
F₁ = 5
F₂ = 17
F₃ = 257
F₄ = 65537
F₅ = 4294967297
F₆ = 18446744073709551617
F₇ = 340282366920938463463374607431768211457
F₈ = 115792089237316195423570985008687907853269984665640564039457584007913129639937
F₉ = 13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084097

Factors of first few Fermat numbers:
Factors of F₀: 3 - prime
Factors of F₁: 5 - prime
Factors of F₂: 17 - prime
Factors of F₃: 257 - prime
Factors of F₄: 65537 - prime
Factors of F₅: 641 6700417 
Factors of F₆: 274177 67280421310721 
Factors of F₇: 59649589127497217 5704689200685129054721 
Factors of F₈: 1238926361552897 93461639715357977769163558199606896584051237541638188580280321

```



## Phix

{{libheader|mpfr}}

```Phix
-- demo\rosetta\Fermat.exw
include mpfr.e

procedure fermat(mpz res, integer n)
    integer pn = power(2,n)
    mpz_ui_pow_ui(res,2,pn)
    mpz_add_si(res,res,1)
end procedure

function shorten(string s)
    integer l = length(s)
    if l>40 then s[10..-10]=sprintf("...<%,d digits>...",l) end if
    return s
end function

mpz fn = mpz_init()
for i=0 to 29 do -- (see note)
    fermat(fn,i)
    if i<=20 then
        printf(1,"F%d = %s\n",{i,shorten(mpz_get_str(fn))})
    else -- (since printing it takes too long...)
        printf(1,"F%d has %,d digits\n",{i,mpz_sizeinbase(fn,10)})
    end if
end for

printf(1,"\n")
randstate state = gmp_randinit_mt()
for i=0 to 13 do
    atom t = time()
    fermat(fn,i)
    sequence f = mpz_prime_factors(fn, 200000)
    t = time()-t
    string fs = "",
           ts = elapsed(t)
    if length(f[$])=1 then -- (as per docs)
        mpz_set_str(fn,f[$][1])
        if not mpz_probable_prime_p(fn, state) then
            if length(f)=1 then
                fs = " (not prime)"
            else
                fs = " (last factor is not prime)"
            end if
        end if
        f[$][1] = shorten(f[$][1])
    elsif length(f)=1
      and mpz_probable_prime_p(fn, state) then
        fs = " (prime)"
    end if
    fs = mpz_factorstring(f)&fs
    printf(1,"Factors of F%d: %s [%s]\n",{i,fs,ts})
end for
```

{{out}}
Note that mpz_prime_factors(), a phix-specific extension to gmp, is designed to find small factors quickly and 
give up early, however it works by maintaining a table of primes, so any prime factor over 10 digits or so is 
beyond reach. You could increase the maxprime parameter, here set at 200,000, which guarantees all factors up 
to 2,750,159 (obviously 7 digits), but it will just get exponentially slower without getting close to finding 
anything more, specifically in this case 1,238,926,361,552,897 (16 digits) or 59,649,589,127,497,217 (17 digits).

Calculating F0..F29 is pretty quick, but F30 and above hit integer limits on 32 bit, F32 and above exceed my physical memory on 64 bit.

As noted above, there is not really much point, and it just takes far too long to bother printing out any numbers with more than 500,000 digits.

Attempting to factor F14 and above gets nowhere, with each attempt some 5-10 times slower than the previous, until F18 which eventually crashes.

```txt

F0 = 3
F1 = 5
F2 = 17
F3 = 257
F4 = 65537
F5 = 4294967297
F6 = 18446744073709551617
F7 = 340282366920938463463374607431768211457
F8 = 115792089...<78 digits>...129639937
F9 = 134078079...<155 digits>...006084097
F10 = 179769313...<309 digits>...224137217
F11 = 323170060...<617 digits>...596230657
F12 = 104438888...<1,234 digits>...154190337
F13 = 109074813...<2,467 digits>...715792897
F14 = 118973149...<4,933 digits>...964066817
F15 = 141546103...<9,865 digits>...712377857
F16 = 200352993...<19,729 digits>...719156737
F17 = 401413218...<39,457 digits>...934173697
F18 = 161132571...<78,914 digits>...298300417
F19 = 259637056...<157,827 digits>...185773057
F20 = 674114012...<315,653 digits>...335579137
F21 has 631,306 digits
F22 has 1,262,612 digits
F23 has 2,525,223 digits
F24 has 5,050,446 digits
F25 has 10,100,891 digits
F26 has 20,201,782 digits
F27 has 40,403,563 digits
F28 has 80,807,125 digits
F29 has 161,614,249 digits

Factors of F0: 3 (prime) [0.0s]
Factors of F1: 5 (prime) [0s]
Factors of F2: 17 (prime) [0s]
Factors of F3: 257 (prime) [0s]
Factors of F4: 65537 (prime) [0s]
Factors of F5: 641*6700417 [0s]
Factors of F6: 274177*67280421310721 [0.0s]
Factors of F7: 340282366920938463463374607431768211457 (not prime) [0.2s]
Factors of F8: 115792089...<78 digits>...129639937 (not prime) [0.2s]
Factors of F9: 2424833*552937374...<148 digits>...393118209 (last factor is not prime) [0.2s]
Factors of F10: 179769313...<309 digits>...224137217 (not prime) [0.2s]
Factors of F11: 319489*974849*103761886...<606 digits>...591348737 (last factor is not prime) [0.3s]
Factors of F12: 114689*910626896...<1,228 digits>...946770433 (last factor is not prime) [0.6s]
Factors of F13: 109074813...<2,467 digits>...715792897 (not prime) [1.3s]

```



## Python


```python
def factors(x):
    factors = []
    i = 2
    s = int(x ** 0.5)
    while i < s:
        if x % i == 0:
            factors.append(i)
            x = int(x / i)
            s = int(x ** 0.5)
        i += 1
    factors.append(x)
    return factors

print("First 10 Fermat numbers:")
for i in range(10):
    fermat = 2 ** 2 ** i + 1
    print("F{} = {}".format(chr(i + 0x2080) , fermat))

print("\nFactors of first few Fermat numbers:")
for i in range(10):
    fermat = 2 ** 2 ** i + 1
    fac = factors(fermat)
    if len(fac) == 1:
        print("F{} -> IS PRIME".format(chr(i + 0x2080)))
    else:
        print("F{} -> FACTORS: {}".format(chr(i + 0x2080), fac))
```

{{out}}

```txt

First 10 Fermat numbers:
F₀ = 3
F₁ = 5
F₂ = 17
F₃ = 257
F₄ = 65537
F₅ = 4294967297
F₆ = 18446744073709551617
F₇ = 340282366920938463463374607431768211457
F₈ = 115792089237316195423570985008687907853269984665640564039457584007913129639937
F₉ = 1340780792994259709957402499820584612747936582059239337772356144372176403007354697680187429816690342769003185818648605085375388281194656994643364
9006084097

Factors of first few Fermat numbers:
F₀ IS PRIME
F₁ IS PRIME
F₂ IS PRIME
F₃ IS PRIME
F₄ IS PRIME
F₅ FACTORS: [641, 6700417]
F₆ FACTORS: [274177, 67280421310721]
F₇ FACTORS: [59649589127497217, 5704689200685129054721]
F₈ FACTORS: [1238926361552897, 93461639715357977769163558199606896584051237541638188580280321]

```





## REXX


###  factoring by trial division 


```rexx
/*REXX program to find and display  Fermat  numbers, and show factors of Fermat numbers.*/
parse arg n .                                    /*obtain optional argument from the CL.*/
if n=='' | n==","  then n= 9                     /*Not specified?  Then use the default.*/
numeric digits 200                               /*ensure enough decimal digits, for n=9*/

     do j=0  to n;   f= 2** (2**j)   +  1        /*calculate a series of Fermat numbers.*/
     say right('F'j, length(n) + 1)': '      f   /*display a particular     "      "    */
     end   /*j*/
say
     do k=0  to n;   f= 2** (2**k)   +  1;  say  /*calculate a series of Fermat numbers.*/
     say center(' F'k": " f' ', 79, "═")         /*display a particular     "      "    */
     p= factr(f)                                 /*factor a Fermat number,  given time. */
     if words(p)==1  then say f ' is prime.'
                     else say 'factors: '    p   
     end   /*k*/
exit
/*──────────────────────────────────────────────────────────────────────────────────────*/
factr:  procedure; parse arg x 1 z,,?
             do k=1  to 11  by 2;  j= k;  if j==1  then j= 2;  if j==9  then iterate
             call build                          /*add  J  to the factors list.         */
             end   /*k*/                         /* [↑]  factor  X  with some low primes*/

             do y=0  by 2;          j= j + 2 +   y // 4     /*ensure not  ÷  by three.  */
             parse var j '' -1 _;   if _==5  then iterate   /*last digit a "5"? Skip it.*/
             if j*j>x | j>z  then leave
             call build                          /*add  Y  to the factors list.         */
             end   /*y*/                         /* [↑]  factor  X  with other higher #s*/
        j= z
        if z\==1  then ?= build()
        if ?=''   then do;  @.1= x;  ?= x;  #= 1;  end
        return ?
/*──────────────────────────────────────────────────────────────────────────────────────*/
build:     do  while z//j==0;    z= z % j;         ?= ? j
           end   /*forever*/
        return strip(?)
```

{{out|output|text=  when using the default input:}}

```txt

F0:  3
F1:  5
F2:  17
F3:  257
F4:  65537
F5:  4294967297
F6:  18446744073709551617
F7:  340282366920938463463374607431768211457
F8:  115792089237316195423570985008687907853269984665640564039457584007913129639937
F9:  13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084097


═══════════════════════════════════ F0:  3 ════════════════════════════════════
3  is prime.

═══════════════════════════════════ F1:  5 ════════════════════════════════════
5  is prime.

═══════════════════════════════════ F2:  17 ═══════════════════════════════════
17  is prime.

══════════════════════════════════ F3:  257 ═══════════════════════════════════
257  is prime.

═════════════════════════════════ F4:  65537 ══════════════════════════════════
65537  is prime.

═══════════════════════════════ F5:  4294967297 ═══════════════════════════════
factors:  641 6700417

══════════════════════════ F6:  18446744073709551617 ══════════════════════════
   ■  ■  ■   (the REXX program stopped via Ctrl─Alt─Break)   ■  ■  ■

```


=== factoring via Pollard's rho algorithm ===

```rexx
/*REXX program to find and display  Fermat  numbers, and show factors of Fermat numbers.*/
parse arg n .                                    /*obtain optional argument from the CL.*/
if n=='' | n==","  then n= 9                     /*Not specified?  Then use the default.*/
numeric digits 200                               /*ensure enough decimal digits, for n=9*/

     do j=0  to n;   f= 2** (2**j)   +  1        /*calculate a series of Fermat numbers.*/
     say right('F'j, length(n) + 1)': '      f   /*display a particular     "      "    */
     end   /*j*/
say
     do k=5  to n;   f= 2** (2**k)   +  1;  say  /*calculate a series of Fermat numbers.*/
     say center(' F'k": " f' ', 79, "═")         /*display a particular     "      "    */
     a= rho(f)                                   /*factor a Fermat number,  given time. */
     b= f % a
     if a==b  then say f ' is prime.'
              else say 'factors:  '   commas(a)     " "     commas(b)
     end   /*k*/
exit
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas:  parse arg _;  do ?=length(_)-3  to 1  by -3; _=insert(',', _, ?); end;   return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
rho:  procedure;  parse arg n;    y= 2;  d= 1    /*initialize  X, Y,  and  D  variables.*/
        do x=2                                   /*try rho method with X=2 for 1st time.*/
          do  while d==1
          x= (x*x + 1) // n
          v= (y*y + 1) // n
          y= (v*v + 1) // n
          parse value  x-y   with  xy  1  sig  2 /*obtain sign of the  x-y  difference. */
          if sig=='-'  then parse var  xy  2  xy /*Negative?   Then use absolute value. */
          nn= n
                do  until nn==0
                parse value xy//nn nn with nn xy /*assign two variables:   NN  and  XY  */
                end   /*until*/                  /*this is an  in-line   GCD  function. */
          d= xy                                  /*assign variable   D   with a new  XY */
          end   /*while*/
        if d==n  then iterate                    /*Current X failure; bump X; try again.*/
        return d                                 /*found a factor of  N.      Return it.*/
        end     /*x*/
```

{{out|output|text=  when using the default input:}}

```txt

F0:  3
F1:  5
F2:  17
F3:  257
F4:  65537
F5:  4294967297
F6:  18446744073709551617
F7:  340282366920938463463374607431768211457
F8:  115792089237316195423570985008687907853269984665640564039457584007913129639937
F9:  13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084097


═══════════════════════════════ F5:  4294967297 ═══════════════════════════════
factors:   641   6,700,417

══════════════════════════ F6:  18446744073709551617 ══════════════════════════
factors:   274,177   67,280,421,310,721

════════════════ F7:  340282366920938463463374607431768211457 ═════════════════
   ■  ■  ■   (the REXX program stopped via Ctrl─Alt─Break)   ■  ■  ■

```



## Sidef


```ruby
func fermat_number(n) {
    2**(2**n) + 1
}

func fermat_one_factor(n) {
    fermat_number(n).ecm_factor
}

for n in (0..9) {
    say "F_#{n} = #{fermat_number(n)}"
}

say ''

for n in (0..13) {
    var f = fermat_one_factor(n)
    say ("F_#{n} = ", join(' * ', f.shift,
      f.map { <C P>[.is_prime] + .len }...))
}
```

{{out}}

```txt

F_0 = 3
F_1 = 5
F_2 = 17
F_3 = 257
F_4 = 65537
F_5 = 4294967297
F_6 = 18446744073709551617
F_7 = 340282366920938463463374607431768211457
F_8 = 115792089237316195423570985008687907853269984665640564039457584007913129639937
F_9 = 13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084097

F_0 = 3
F_1 = 5
F_2 = 17
F_3 = 257
F_4 = 65537
F_5 = 641 * P7
F_6 = 274177 * P14
F_7 = 59649589127497217 * P22
F_8 = 1238926361552897 * P62
F_9 = 2424833 * C148
F_10 = 45592577 * C301
F_11 = 319489 * C612
F_12 = 114689 * C1228
F_13 = 2710954639361 * C2454

```



## zkl

{{libheader|GMP}} GNU Multiple Precision Arithmetic Library
for big ints and primes

```zkl
fermatsW:=[0..].tweak(fcn(n){ BI(2).pow(BI(2).pow(n)) + 1 });
println("First 10 Fermat numbers:");
foreach n in (10){ println("F",n,": ",fermatsW.next()) }
```

{{out}}
<pre style="font-size:83%">
First 10 Fermat numbers:
F0: 3
F1: 5
F2: 17
F3: 257
F4: 65537
F5: 4294967297
F6: 18446744073709551617
F7: 340282366920938463463374607431768211457
F8: 115792089237316195423570985008687907853269984665640564039457584007913129639937
F9: 13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084097

```


```zkl
fcn primeFactorsBI(n){  // Return a list of the prime factors of n
   acc:=fcn(n,k,acc,maxD){  // k is primes
      if(n==1 or k>maxD) acc.close();
      else{
	 q,r:=n.div2(k);   // divr-->(quotient,remainder)
	 if(r==0) return(self.fcn(q,k,acc.write(k.copy()),q.root(2)));
	 return(self.fcn(n, k.nextPrime(), acc,maxD)) # both are tail recursion
      }
   }(n,BI(2),Sink(List),n.root(2));
   m:=acc.reduce('*,BI(1));  // mulitply factors
   if(n!=m) acc.append(n/m); // opps, missed last factor
   else acc;
}
```


```zkl
fermatsW:=[0..].tweak(fcn(n){ BI(2).pow(BI(2).pow(n)) + 1 });
println("Factors of first few Fermat numbers:");
foreach n in (7){
   println("Factors of F",n,": ",factorsBI(fermatsW.next()).concat(" "));
}
```

{{out}}

```txt

Factors of first few Fermat numbers:
Factors of F0: 3
Factors of F1: 5
Factors of F2: 17
Factors of F3: 257
Factors of F4: 65537
Factors of F5: 641 6700417
Factors of F6: 274177 67280421310721

```

