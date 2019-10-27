+++
title = "Smarandache prime-digital sequence"
description = ""
date = 2019-09-16T08:15:25Z
aliases = []
[extra]
id = 22344
[taxonomies]
categories = []
tags = []
+++

{{task|Prime Numbers}}

The Smarandache prime-digital sequence (SPDS for brevity) is the sequence of primes whose digits are themselves prime.

For example 257 is an element of this sequence because it is prime itself and its digits: 2, 5 and 7 are also prime.

;Task
* Show the first 25 SPDS primes.
* Show the hundredth SPDS prime.


;See also:

* [[oeis:A019546|OEIS A019546: Primes whose digits are primes.]]
* https://www.scribd.com/document/214851583/On-the-Smarandache-prime-digital-subsequence-sequences





## AWK


```AWK

# syntax: GAWK -f SMARANDACHE_PRIME-DIGITAL_SEQUENCE.AWK
BEGIN {
    limit = 25
    printf("1-%d:",limit)
    while (1) {
      if (is_prime(++n)) {
        if (all_digits_prime(n) == 1) {
          if (++count <= limit) {
            printf(" %d",n)
          }
          if (count == 100) {
            printf("\n%d: %d\n",count,n)
            break
          }
        }
      }
    }
    exit(0)
}
function all_digits_prime(n, i) {
    for (i=1; i<=length(n); i++) {
      if (!is_prime(substr(n,i,1))) {
        return(0)
      }
    }
    return(1)
}
function is_prime(x,  i) {
    if (x <= 1) {
      return(0)
    }
    for (i=2; i<=int(sqrt(x)); i++) {
      if (x % i == 0) {
        return(0)
      }
    }
    return(1)
}

```

{{out}}

```txt

1-25: 2 3 5 7 23 37 53 73 223 227 233 257 277 337 353 373 523 557 577 727 733 757 773 2237 2273
100: 33223

```

=={{header|F_Sharp|F#}}==
This task uses [http://www.rosettacode.org/wiki/Extensible_prime_generator#The_function Extensible Prime Generator (F#)]

```fsharp

// Generate Smarandache prime-digital sequence. Nigel Galloway: May 31st., 2019
let rec spds g=seq{yield! g; yield! (spds (Seq.collect(fun g->[g*10+2;g*10+3;g*10+5;g*10+7]) g))}|>Seq.filter(isPrime)
spds [2;3;5;7] |> Seq.take 25 |> Seq.iter(printfn "%d")
printfn "\n\n100th item of this sequence is %d" (spds [2;3;5;7] |> Seq.item 99)
printfn "1000th item of this sequence is %d" (spds [2;3;5;7] |> Seq.item 999)

```

{{out}}

```txt

2
3
5
7
23
37
53
73
223
227
233
257
277
337
353
373
523
557
577
727
733
757
773
2237
2273


100th item of this sequence is 33223
1000th item of this sequence is 3273527

```



## Factor


### Naive


```factor
USING: combinators.short-circuit io lists lists.lazy math
math.parser math.primes prettyprint sequences ;
IN: rosetta-code.smarandache-naive

: smarandache? ( n -- ? )
    {
        [ number>string string>digits [ prime? ] all? ]
        [ prime? ]
    } 1&& ;

: smarandache ( -- list ) 1 lfrom [ smarandache? ] lfilter ;

: smarandache-demo ( -- )
    "First 25 members of the Smarandache prime-digital sequence:"
    print 25 smarandache ltake list>array .
    "100th member: " write smarandache 99 [ cdr ] times car . ;

MAIN: smarandache-demo
```

{{out}}

```txt

First 25 members of the Smarandache prime-digital sequence:
{
    2
    3
    5
    7
    23
    37
    53
    73
    223
    227
    233
    257
    277
    337
    353
    373
    523
    557
    577
    727
    733
    757
    773
    2237
    2273
}
100th member: 33223

```



### Optimized


```factor
USING: combinators generalizations io kernel math math.functions
math.primes prettyprint sequences ;
IN: rosetta-code.smarandache

! Observations:
! * For 2-digit numbers and higher, only 3 and 7 are viable in
!   the ones place.
! * Only 2, 3, 5, and 7 are viable anywhere else.
! * It is possible to use this information to drastically
!   reduce the amount of numbers to check for primality.
! * For instance, by these rules we can tell that the next
!   potential Smarandache prime digital after 777 is 2223.

: next-one ( n -- n' ) 3 = 7 3 ? ; inline

: next-ten ( n -- n' )
    { { 2 [ 3 ] } { 3 [ 5 ] } { 5 [ 7 ] } [ drop 2 ] } case ;

: inc ( seq quot: ( n -- n' ) -- seq' )
    [ 0 ] 2dip [ change-nth ] curry keep ; inline

: inc1  ( seq -- seq' ) [ next-one ] inc ;
: inc10 ( seq -- seq' ) [ next-ten ] inc ;

: inc-all ( seq -- seq' )
    inc1 [ zero? not [ next-ten ] when ] V{ } map-index-as ;

: carry ( seq -- seq' )
    dup [ 7 = not ] find drop {
        { 0 [ inc1 ] }
        { f [ inc-all 2 suffix! ] }
        [ cut [ inc-all ] [ inc10 ] bi* append! ]
    } case ;

: digits>integer ( seq -- n ) [ 10 swap ^ * ] map-index sum ;

: next-smarandache ( seq -- seq' )
    [ digits>integer prime? ] [ carry dup ] do until ;

: .sm ( seq -- ) <reversed> [ pprint ] each nl ;

: first25 ( -- )
    2 3 5 7 [ . ] 4 napply V{ 7 } clone
    21 [ next-smarandache dup .sm ] times drop ;

: nth-smarandache ( n -- )
    4 - V{ 7 } clone swap [ next-smarandache ] times .sm ;

: smarandache-demo ( -- )
    "First 25 members of the Smarandache prime-digital sequence:"
    print first25 nl { 100 1000 10000 100000 } [
        dup pprint "th member: " write nth-smarandache
    ] each ;

MAIN: smarandache-demo
```

{{out}}

```txt

First 25 members of the Smarandache prime-digital sequence:
2
3
5
7
23
37
53
73
223
227
233
257
277
337
353
373
523
557
577
727
733
757
773
2237
2273

100th member: 33223
1000th member: 3273527
10000th member: 273322727
100000th member: 23325232253

```



## Go


### Basic


```go
package main

import (
    "fmt"
    "math/big"
)

var b = new(big.Int)

func isSPDSPrime(n uint64) bool {
    nn := n
    for nn > 0 {
        r := nn % 10
        if r != 2 && r != 3 && r != 5 && r != 7 {
            return false
        }
        nn /= 10
    }
    b.SetUint64(n)
    if b.ProbablyPrime(0) { // 100% accurate up to 2 ^ 64
        return true
    }
    return false
}

func listSPDSPrimes(startFrom, countFrom, countTo uint64, printOne bool) uint64 {
    count := countFrom
    for n := startFrom; ; n += 2 {
        if isSPDSPrime(n) {
            count++
            if !printOne {
                fmt.Printf("%2d. %d\n", count, n)
            }
            if count == countTo {
                if printOne {
                    fmt.Println(n)
                }
                return n
            }
        }
    }
}

func main() {
    fmt.Println("The first 25 terms of the Smarandache prime-digital sequence are:")
    fmt.Println(" 1. 2")
    n := listSPDSPrimes(3, 1, 25, false)
    fmt.Println("\nHigher terms:")
    indices := []uint64{25, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000}
    for i := 1; i < len(indices); i++ {
        fmt.Printf("%6d. ", indices[i])
        n = listSPDSPrimes(n+2, indices[i-1], indices[i], true)
    }
}
```


{{out}}

```txt

The first 25 terms of the Smarandache prime-digital sequence are:
 1. 2
 2. 3
 3. 5
 4. 7
 5. 23
 6. 37
 7. 53
 8. 73
 9. 223
10. 227
11. 233
12. 257
13. 277
14. 337
15. 353
16. 373
17. 523
18. 557
19. 577
20. 727
21. 733
22. 757
23. 773
24. 2237
25. 2273

Higher terms:
   100. 33223
   200. 223337
   500. 723337
  1000. 3273527
  2000. 22332337
  5000. 55373333
 10000. 273322727
 20000. 727535273
 50000. 3725522753
100000. 23325232253

```


### Optimized

This version is inspired by the optimizations used in the Factor and Phix entries which are expressed here as a kind of base-4 arithmetic using a digits set of {2, 3, 5, 7} where leading '2's are significant.

This is more than 30 times faster than the above version (runs in about 12.5 seconds on my Celeron @1.6GHx) and could be quickened up further (to around 4 seconds) by using a wrapper for GMP rather than Go's native big.Int type.

```go
package main

import (
    "fmt"
    "math/big"
)

type B2357 []byte

var bi = new(big.Int)

func isSPDSPrime(b B2357) bool {
    bi.SetString(string(b), 10)
    return bi.ProbablyPrime(0) // 100% accurate up to 2 ^ 64
}

func listSPDSPrimes(startFrom B2357, countFrom, countTo uint64, printOne bool) B2357 {
    count := countFrom
    n := startFrom
    for {
        if isSPDSPrime(n) {
            count++
            if !printOne {
                fmt.Printf("%2d. %s\n", count, string(n))
            }
            if count == countTo {
                if printOne {
                    fmt.Println(string(n))
                }
                return n
            }
        }
        if printOne {
            n = n.AddTwo()
        } else {
            n = n.AddOne()
        }
    }
}

func incDigit(digit byte) byte {
    switch digit {
    case '2':
        return '3'
    case '3':
        return '5'
    case '5':
        return '7'
    default:
        return '9' // say
    }
}

func (b B2357) AddOne() B2357 {
    le := len(b)
    b[le-1] = incDigit(b[le-1])
    for i := le - 1; i >= 0; i-- {
        if b[i] < '9' {
            break
        } else if i > 0 {
            b[i] = '2'
            b[i-1] = incDigit(b[i-1])
        } else {
            b[0] = '2'
            nb := make(B2357, le+1)
            copy(nb[1:], b)
            nb[0] = '2'
            return nb
        }
    }
    return b
}

func (b B2357) AddTwo() B2357 {
    return b.AddOne().AddOne()
}

func main() {
    fmt.Println("The first 25 terms of the Smarandache prime-digital sequence are:")
    n := listSPDSPrimes(B2357{'2'}, 0, 4, false)
    n = listSPDSPrimes(n.AddOne(), 4, 25, false)
    fmt.Println("\nHigher terms:")
    indices := []uint64{25, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000}
    for i := 1; i < len(indices); i++ {
        fmt.Printf("%6d. ", indices[i])
        n = listSPDSPrimes(n.AddTwo(), indices[i-1], indices[i], true)
    }
}
```


{{out}}

```txt

Same as before.

```



## Julia

The prime single digits are 2, 3, 5, and 7. Except for 2 and 5, any number
ending in 2 or 5 is not prime. So we start with [2, 3, 5, 7] and then
add numbers that end in 3 or 7 and that only contain 2, 3, 5, and 7. This
can be done via permutations of combinations with repetition.

```julia

using Combinatorics, Primes

combodigits(len) = sort!(unique(map(y -> join(y, ""), with_replacement_combinations("2357", len))))

function getprimes(N, maxdigits=9)
    ret = [2, 3, 5, 7]
    perms = Int[]
    for i in 1:maxdigits-1, combo in combodigits(i), perm in permutations(combo)
        n = parse(Int64, String(perm)) * 10
        push!(perms, n + 3, n + 7)
    end
        for perm in sort!(perms)
        if isprime(perm) && !(perm in ret)
            push!(ret, perm)
            if length(ret) >= N
                return ret
            end
        end
    end
end

const v = getprimes(10000)
println("The first 25 Smarandache primes are: ", v[1:25])
println("The 100th Smarandache prime is: ", v[100])
println("The 10000th Smarandache prime is: ", v[10000])

```
{{out}}

```txt

The first 25 Smarandache primes are: [2, 3, 5, 7, 23, 37, 53, 73, 223, 227, 233, 257, 277, 337, 353, 373, 523, 557, 577, 727, 733, 757, 773, 2237, 2273]
The 100th Smarandache prime is: 33223
The 10000th Smarandache prime is: 273322727

```



## Perl

{{libheader|ntheory}}

```perl
use strict;
use warnings;
use feature 'say';
use feature 'state';
use ntheory qw<is_prime>;
use Lingua::EN::Numbers qw(num2en_ordinal);

my @prime_digits = <2 3 5 7>;
my @spds = grep { is_prime($_) && /^[@{[join '',@prime_digits]}]+$/ } 1..100;
my @p    = map { $_+3, $_+7 } map { 10*$_ } @prime_digits;

while ($#spds < 100_000) {
    state $o++;
    my $oom = 10**(1+$o);
    my @q;
    for my $l (@prime_digits) {
        push @q, map { $l*$oom + $_ } @p;
    }
    push @spds, grep { is_prime($_) } @p = @q;
}

say 'Smarandache prime-digitals:';
printf "%22s: %s\n", ucfirst(num2en_ordinal($_)), $spds[$_-1] for 1..25, 100, 1000, 10_000, 100_000;
```

{{out}}

```txt
                 First: 2
                Second: 3
                 Third: 5
                Fourth: 7
                 Fifth: 23
                 Sixth: 37
               Seventh: 53
                Eighth: 73
                 Ninth: 223
                 Tenth: 227
              Eleventh: 233
               Twelfth: 257
            Thirteenth: 277
            Fourteenth: 337
             Fifteenth: 353
             Sixteenth: 373
           Seventeenth: 523
            Eighteenth: 557
            Nineteenth: 577
             Twentieth: 727
          Twenty-first: 733
         Twenty-second: 757
          Twenty-third: 773
         Twenty-fourth: 2237
          Twenty-fifth: 2273
         One hundredth: 33223
        One thousandth: 3273527
        Ten thousandth: 273322727
One hundred thousandth: 23325232253
```



## Perl 6



```perl6
use Lingua::EN::Numbers;
use ntheory:from<Perl5> <:all>;

# Implemented as a lazy, extendable list
my $spds = grep { .&is_prime }, flat [2,3,5,7], [23,27,33,37,53,57,73,77], -> $p
  { state $o++; my $oom = 10**(1+$o); [ flat (2,3,5,7).map: -> $l { (|$p).map: $l*$oom+* } ] } … *;

say 'Smarandache prime-digitals:';
printf "%22s: %s\n", ordinal(1+$_).tclc, comma $spds[$_] for flat ^25, 99, 999, 9999, 99999;
```

{{out}}

```txt
Smarandache prime-digitals:
                 First: 2
                Second: 3
                 Third: 5
                Fourth: 7
                 Fifth: 23
                 Sixth: 37
               Seventh: 53
                Eighth: 73
                 Ninth: 223
                 Tenth: 227
              Eleventh: 233
               Twelfth: 257
            Thirteenth: 277
            Fourteenth: 337
             Fifteenth: 353
             Sixteenth: 373
           Seventeenth: 523
            Eighteenth: 557
            Nineteenth: 577
             Twentieth: 727
          Twenty-first: 733
         Twenty-second: 757
          Twenty-third: 773
         Twenty-fourth: 2,237
          Twenty-fifth: 2,273
         One hundredth: 33,223
        One thousandth: 3,273,527
        Ten thousandth: 273,322,727
One hundred thousandth: 23,325,232,253
```



## Phix

{{libheader|mpfr}}
Optimised. As noted on the Factor entry, candidates>10 must end in 3 or 7 (since they would not be prime 
if they ended in 2 or 5), which we efficiently achieve by alternately adding {4,-4}. Digits to the left 
of that must all be 2/3/5/7, so we add {1,2,2,-5}*10^k to cycle round those digits. 
Otherwise it is exactly like counting by adding 1 to each digit and carrying 1 left when we do a 9->0.

I had planned to effectively merge a list of potential candidates with a list of all prime numbers,
but because of the massive gaps (eg between 777,777,777 and 2,222,222,223) it proved much faster
to test each candidate for primality individually. Timings below show just how much this improves things.

```Phix
atom t0 = time()
sequence spds = {2,3,5,7}
atom nxt_candidate = 23
sequence adj = {{4,-4},sq_mul({1,2,2,-5},10)},
         adjn = {1,1}

include mpfr.e
mpz zprime = mpz_init()
randstate state = gmp_randinit_mt()

procedure populate_spds(integer n)
    while length(spds)<n do
        mpz_set_d(zprime,nxt_candidate)
        if mpz_probable_prime_p(zprime,state) then
            spds &= nxt_candidate
        end if
        for i=1 to length(adjn) do
            sequence adjs = adj[i]
            integer adx = adjn[i]
            nxt_candidate += adjs[adx]
            adx += 1
            if adx<=length(adjs) then
                adjn[i] = adx
                exit
            end if
            adjn[i] = 1
            if i=length(adjn) then
                -- (this is eg 777, by now 223 carry 1, -> 2223)
                adj = append(adj,sq_mul(adj[$],10))
                adjn = append(adjn, 1)
                nxt_candidate += adj[$][2]
                exit
            end if
        end for
    end while
end procedure

populate_spds(25)
printf(1,"spds[1..25]:%v\n",{spds[1..25]})
for n=2 to 5 do
    integer p = power(10,n)
    populate_spds(p)
    printf(1,"spds[%d]:%d\n",{p,spds[p]})
end for
?elapsed(time()-t0)
```

{{out}}

```txt

spds[1..25]:{2,3,5,7,23,37,53,73,223,227,233,257,277,337,353,373,523,557,577,727,733,757,773,2237,2273}
spds[100]:33223
spds[1000]:3273527
spds[10000]:273322727
spds[100000]:23325232253
"3.6s"

```

For comparison, on the same machine:

Factor (as optimised) took 45s to calculate the 100,000th number.

Go took 1 min 50 secs to calculate the 100,000th number.

Julia crashed when the limit was changed to 100,000, however it took 11s just to calculate the 10,000th number anyway.

Perl 6 was by far the slowest of all I tried, taking 1 min 15s just to calculate the 10,000th number.


## Python


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


def digit_check(n):
    if len(str(n))<2:
        return True
    else:
        for digit in str(n):
            if not is_prime(int(digit)):
                return False
        return True


def sequence(max_n=None):
    ii = 0
    n = 0
    while True:
        ii += 1
        if is_prime(ii):
            if max_n is not None:
                if n>max_n:
                    break
            if digit_check(ii):
                n += 1
                yield ii


if __name__ == '__main__':
    generator = sequence(100)
    for index, item in zip(range(1, 16), generator):
        print(index, item)
    for index, item in zip(range(16, 100), generator):
        pass
    print(100, generator.__next__())

```


<b>Output</b>

```Python

1 2
2 3
3 5
4 7
5 23
6 37
7 53
8 73
9 223
10 227
11 233
12 257
13 277
14 337
15 353
100 33223

```



## REXX

The prime number generator has been simplified and very little optimization was included.  

```rexx
/*REXX program lists a  sequence of  SPDS  (Smarandache prime-digital sequence)  primes.*/
parse arg n q                                    /*get optional number of primes to find*/
if n=='' | n==","  then n=  25                   /*Not specified?  Then use the default.*/
if q=''            then q= 100                   /* "      "         "   "   "     "    */
say '═══listing the first'     n     "SPDS primes═══"
call spds n
             do i=1  for words(q)+1;     y=word(q, i);    if y=='' | y==","   then iterate
             say
             say '═══listing the last of '    y     "SPDS primes═══"
             call spds -y
             end   /*i*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
spds: parse arg x 1 ox;  x= abs(x)               /*obtain the limit to be used for list.*/
      c= 0                                       /*C  number of SPDS primes found so far*/
      #= 0                                       /*#  number of      primes found so far*/
            do j=1  by 2  while  c<x;    z= j    /*start: 1st even prime, then use odd. */
            if z==1  then z= 2                   /*handle the even prime (special case) */
                                                 /* [↓]  divide by the primes.   ___    */
                    do k=2  to #  while  k*k<=z  /*divide  Z  with all primes ≤ √ Z     */
                    if z//@.k==0  then iterate j /*÷ by prev. prime?  ¬prime     ___    */
                    end   /*j*/                  /* [↑]   only divide up to     √ Z     */
            #= # + 1;             @.#= z         /*bump the prime count;  assign prime #*/
            if verify(z, 2357)>0  then iterate j /*Digits ¬prime?  Then skip this prime.*/
            c= c + 1                             /*bump the number of SPDS primes found.*/
            if ox<0  then iterate                /*don't display it, display the last #.*/
            say right(z, 21)                     /*maybe display this prime ──► terminal*/
            end   /*j*/                          /* [↑]  only display N number of primes*/
      if ox<0  then say right(z, 21)             /*display one  (the last)  SPDS prime. */
      return
```

{{out|output|text=  when using the default inputs:}}

```txt

═══listing the first 25 SPDS primes═══
                    2
                    3
                    5
                    7
                   23
                   37
                   53
                   73
                  223
                  227
                  233
                  257
                  277
                  337
                  353
                  373
                  523
                  557
                  577
                  727
                  733
                  757
                  773
                 2237
                 2273

═══listing the last of  100 SPDS primes═══
                33223

═══listing the last of  1000 SPDS primes═══
              3273527

```



## Ring


```ring

# Project: Calmo primes
load "stdlib.ring"
limit = 25
max = 300000
num = 0
see "working..." + nl
see "wait for done..." + nl
see "First 25 Calmo primes are:" + nl
for n = 1 to max
    if isprime(n)
       res = calmo(n)
       if res = 1
          num = num + 1
          if num < limit + 1
             see "" + num + ". " + n + nl
          ok
          if num = 100
             see "The hundredth Calmo prime is:" + nl
             see "" + num + ". " + n + nl
             exit
          ok
       ok
    ok
next
see "done..." + nl

func calmo(p)
     sp = string(p)
     for n = 1 to len(sp)
         if not isprime(sp[n])
            return 0
         ok
     next
     return 1

```

{{Out}}

```txt

working...
wait for done...
First 25 Calmo primes are:
1. 2
2. 3
3. 5
4. 7
5. 23
6. 37
7. 53
8. 73
9. 223
10. 227
11. 233
12. 257
13. 277
14. 337
15. 353
16. 373
17. 523
18. 557
19. 577
20. 727
21. 733
22. 757
23. 773
24. 2237
25. 2273
The hundredth Calmo prime is:
100. 33223
done...

```



## Ruby

Attaching 3 and 7 to permutations of 2,3,5 and 7

```ruby
require "prime"
 
smarandache = Enumerator.new do|y|
  prime_digits = [2,3,5,7]
  prime_digits.each{|pr| y << pr} # yield the below-tens
  (1..).each do |n|
    prime_digits.repeated_permutation(n).each do |perm|
      c = perm.join.to_i * 10 
      y << c + 3 if (c+3).prime?
      y << c + 7 if (c+7).prime?
    end
  end
end

seq = smarandache.take(100)
p seq.first(25)
p seq.last

```

{{out}}

```txt
[2, 3, 5, 7, 23, 37, 53, 73, 223, 227, 233, 257, 277, 337, 353, 373, 523, 557, 577, 727, 733, 757, 773, 2237, 2273]
33223

```

Calculating the 10,000th Smarandache number takes about 1.2 seconds.


## Sidef


```ruby
func is_prime_digital(n) {
    n.is_prime && n.digits.all { .is_prime }
}

say is_prime_digital.first(25).join(',')
say is_prime_digital.nth(100)
```

{{out}}

```txt

2,3,5,7,23,37,53,73,223,227,233,257,277,337,353,373,523,557,577,727,733,757,773,2237,2273
33223

```



## zkl

{{libheader|GMP}} GNU Multiple Precision Arithmetic Library
Using GMP ( probabilistic primes), 
because it is easy and fast to generate primes.

[[Extensible prime generator#zkl]] could be used instead.

```zkl
var [const] BI=Import("zklBigNum");  // libGMP

spds:=Walker.zero().tweak(fcn(ps){
   var [const] nps=T(0,0,1,1,0,1,0,1,0,0);  // 2,3,5,7
   p:=ps.nextPrime().toInt();
   if(p.split().filter( fcn(n){ 0==nps[n] }) ) return(Void.Skip);
   p   //  733 --> (7,3,3) --> () --> good,       29 --> (2,9) --> (9) --> bad
}.fp(BI(1)));
```

Or

```zkl
spds:=Walker.zero().tweak(fcn(ps){
   var [const] nps="014689".inCommon;
   p:=ps.nextPrime().toInt();
   if(nps(p.toString())) return(Void.Skip);
   p   //  733 --> "" --> good,       29 --> "9" --> bad
}.fp(BI(1)));
```


```zkl
println("The first 25 terms of the Smarandache prime-digital sequence are:");
spds.walk(25).concat(",").println();

println("The hundredth term of the sequence is: ",spds.drop(100-25).value);
println("1000th item of this sequence is : ",spds.drop(1_000-spds.n).value);
```

{{out}}

```txt

The first 25 terms of the Smarandache prime-digital sequence are:
2,3,5,7,23,37,53,73,223,227,233,257,277,337,353,373,523,557,577,727,733,757,773,2237,2273
The hundredth term of the sequence is: 33223
1000th item of this sequence is : 3273527

```

