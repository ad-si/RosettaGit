+++
title = "Sequence: smallest number with exactly n divisors"
description = ""
date = 2019-09-16T09:43:22Z
aliases = []
[extra]
id = 22267
[taxonomies]
categories = ["task"]
tags = []
+++

Calculate the sequence where each term <strong>a<sub>n</sub></strong> is the '''smallest natural number''' that has exactly '''n''' divisors.

## Task

Show here, on this page, at least the first '''15''' terms of the sequence.

## See also

:*[[oeis:A005179|OEIS:A005179]]

## Related tasks

:*[[Sequence: smallest number greater than previous term with exactly n divisors]]
:*[[Sequence: nth number with exactly n divisors‎‎]]


## ALGOL 68

```algol68
BEGIN

    PROC count divisors = ( INT n )INT:
         BEGIN
            INT count := 0;
            FOR i WHILE i*i <= n DO
                IF n MOD i = 0 THEN
                    count +:= IF i = n OVER i THEN 1 ELSE 2 FI
                FI
            OD;
            count
         END # count divisors # ;

    INT max = 15;
    [ max ]INT seq;FOR i TO max DO seq[ i ] := 0 OD;
    INT found := 0;
    FOR i WHILE found < max DO
        IF INT divisors = count divisors( i );
           divisors <= max
        THEN
            # have an i with an appropriate number of divisors                 #
            IF seq[ divisors ] = 0 THEN
                # this is the first i with that many divisors                  #
                seq[ divisors ] := i;
                found          +:= 1
            FI
        FI
    OD;
    print( ( "The first ", whole( max, 0 ), " terms of the sequence are:", newline ) );
    FOR i TO max DO
        print( ( whole( seq( i ), 0 ), " " ) )
    OD;
    print( ( newline ) )

END
```

```txt

The first 15 terms of the sequence are:
1 2 4 6 16 12 64 24 36 48 1024 60 4096 192 144

```


## AWK


```AWK

# syntax: GAWK -f SEQUENCE_SMALLEST_NUMBER_WITH_EXACTLY_N_DIVISORS.AWK
# converted from Kotlin
BEGIN {
    limit = 15
    printf("first %d terms:",limit)
    i = 1
    n = 0
    while (n < limit) {
      k = count_divisors(i)
      if (k <= limit && seq[k-1]+0 == 0) {
        seq[k-1] = i
        n++
      }
      i++
    }
    for (i=0; i<limit; i++) {
      printf(" %d",seq[i])
    }
    printf("\n")
    exit(0)
}
function count_divisors(n,  count,i) {
    for (i=1; i*i<=n; i++) {
      if (n % i == 0) {
        count += (i == n / i) ? 1 : 2
      }
    }
    return(count)
}

```

```txt

first 15 terms: 1 2 4 6 16 12 64 24 36 48 1024 60 4096 192 144

```



## C

```c
#include <stdio.h>

#define MAX 15

int count_divisors(int n) {
    int i, count = 0;
    for (i = 1; i * i <= n; ++i) {
        if (!(n % i)) {
            if (i == n / i)
                count++;
            else
                count += 2;
        }
    }
    return count;
}

int main() {
    int i, k, n, seq[MAX];
    for (i = 0; i < MAX; ++i) seq[i] = 0;
    printf("The first %d terms of the sequence are:\n", MAX);
    for (i = 1, n = 0; n <  MAX; ++i) {
        k = count_divisors(i);
        if (k <= MAX && seq[k - 1] == 0) {
            seq[k - 1] = i;
            ++n;
        }
    }
    for (i = 0; i < MAX; ++i) printf("%d ", seq[i]);
    printf("\n");
    return 0;
}
```


```txt

The first 15 terms of the sequence are:
1 2 4 6 16 12 64 24 36 48 1024 60 4096 192 144

```



## C++

```cpp
#include <iostream>

#define MAX 15

using namespace std;

int count_divisors(int n) {
    int count = 0;
    for (int i = 1; i * i <= n; ++i) {
        if (!(n % i)) {
            if (i == n / i)
                count++;
            else
                count += 2;
        }
    }
    return count;
}

int main() {
    int i, k, n, seq[MAX];
    for (i = 0; i < MAX; ++i) seq[i] = 0;
    cout << "The first " << MAX << " terms of the sequence are:" << endl;
    for (i = 1, n = 0; n <  MAX; ++i) {
        k = count_divisors(i);
        if (k <= MAX && seq[k - 1] == 0) {
            seq[k - 1] = i;
            ++n;
        }
    }
    for (i = 0; i < MAX; ++i) cout << seq[i] << " ";
    cout << endl;
    return 0;
}
```


```txt

The first 15 terms of the sequence are:
1 2 4 6 16 12 64 24 36 48 1024 60 0 192 144

```

=={{header|F_Sharp|F#}}==
This task uses [http://www.rosettacode.org/wiki/Extensible_prime_generator#The_function Extensible Prime Generator (F#)]

```fsharp

// Find Antı-Primes plus. Nigel Galloway: April 9th., 2019
// Increasing the value 14 will increase the number of anti-primes plus found
let fI=primes|>Seq.take 14|>Seq.map bigint|>List.ofSeq
let N=Seq.reduce(*) fI
let fG g=Seq.unfold(fun ((n,i,e) as z)->Some(z,(n+1,i+1,(e*g)))) (1,2,g)
let fE n i=n|>Seq.collect(fun(n,e,g)->Seq.map(fun(a,c,b)->(a,c*e,g*b)) (i|>Seq.takeWhile(fun(g,_,_)->g<=n))|> Seq.takeWhile(fun(_,_,n)->n<N))
let fL=let mutable g=0 in (fun n->g<-g+1; n=g)
let n=Seq.concat(Seq.scan(fun n g->fE n (fG g)) (seq[(2147483647,1,1I)]) fI)|>List.ofSeq|>List.groupBy(fun(_,n,_)->n)|>List.sortBy(fun(n,_)->n)|>List.takeWhile(fun(n,_)->fL n)
for n,g in n do printfn "%d->%A" n (g|>List.map(fun(_,_,n)->n)|>List.min)

```

```txt

1->1
2->2
3->4
4->6
5->16
6->12
7->64
8->24
9->36
10->48
11->1024
12->60
13->4096
14->192
15->144
16->120
17->65536
18->180
19->262144
20->240
21->576
22->3072
23->4194304
24->360
25->1296
26->12288
27->900
28->960
29->268435456
30->720
31->1073741824
32->840
33->9216
34->196608
35->5184
36->1260
37->68719476736
38->786432
39->36864
40->1680
41->1099511627776
42->2880
43->4398046511104
44->15360
45->3600
46->12582912
47->70368744177664
48->2520
49->46656
50->6480
51->589824
52->61440
53->4503599627370496
54->6300
55->82944
56->6720
57->2359296
58->805306368
Real: 00:00:01.079, CPU: 00:00:01.080, GC gen0: 47, gen1: 0

```



## Factor


```factor
USING: fry kernel lists lists.lazy math math.primes.factors
prettyprint sequences ;

: A005179 ( -- list )
    1 lfrom [
        1 swap '[ dup divisors length _ = ] [ 1 + ] until
    ] lmap-lazy ;

15 A005179 ltake list>array .
```

```txt

{ 1 2 4 6 16 12 64 24 36 48 1024 60 4096 192 144 }

```



## Go


```go
package main

import "fmt"

func countDivisors(n int) int {
    count := 0
    for i := 1; i*i <= n; i++ {
        if n%i == 0 {
            if i == n/i {
                count++
            } else {
                count += 2
            }
        }
    }
    return count
}

func main() {
    const max = 15
    seq := make([]int, max)
    fmt.Println("The first", max, "terms of the sequence are:")
    for i, n := 1, 0; n < max; i++ {
        if k := countDivisors(i); k <= max && seq[k-1] == 0 {
            seq[k-1] = i
            n++
        }
    }
    fmt.Println(seq)
}
```


```txt

The first 15 terms of the sequence are:
[1 2 4 6 16 12 64 24 36 48 1024 60 4096 192 144]

```



## Java

```java
import java.util.Arrays;

public class OEIS_A005179 {

    static int count_divisors(int n) {
        int count = 0;
        for (int i = 1; i * i <= n; ++i) {
            if (n % i == 0) {
                if (i == n / i)
                    count++;
                else
                    count += 2;
            }
        }
        return count;
    }

    public static void main(String[] args) {
        final int max = 15;
        int[] seq = new int[max];
        System.out.printf("The first %d terms of the sequence are:\n", max);
        for (int i = 1, n = 0; n < max; ++i) {
            int k = count_divisors(i);
            if (k <= max && seq[k - 1] == 0) {
                seq[k- 1] = i;
                n++;
            }
        }
        System.out.println(Arrays.toString(seq));
    }
}
```


```txt

The first 15 terms of the sequence are:
[1, 2, 4, 6, 16, 12, 64, 24, 36, 48, 1024, 60, 4096, 192, 144]

```



## Julia

```julia
using Primes

numfactors(n) = reduce(*, e+1 for (_,e) in factor(n); init=1)

A005179(n) = findfirst(k -> numfactors(k) == n, 1:typemax(Int))

println("The first 15 terms of the sequence are:")
println(map(A005179, 1:15))

```
```txt

First 15 terms of OEIS sequence A005179:
1 2 4 6 16 12 64 24 36 48 1024 60 4096 192 144

```



## Kotlin

```scala
// Version 1.3.21

const val MAX = 15

fun countDivisors(n: Int): Int {
    var count = 0
    var i = 1
    while (i * i <= n) {
        if (n % i == 0) {
            count += if (i == n / i) 1 else 2
        }
        i++
    }
    return count
}

fun main() {
    var seq = IntArray(MAX)
    println("The first $MAX terms of the sequence are:")
    var i = 1
    var n = 0
    while (n < MAX) {
        var k = countDivisors(i)
        if (k <= MAX && seq[k - 1] == 0) {
            seq[k - 1] = i
            n++
        }
        i++
    }
    println(seq.asList())
}
```


```txt

The first 15 terms of the sequence are:
[1, 2, 4, 6, 16, 12, 64, 24, 36, 48, 1024, 60, 4096, 192, 144]

```



## Perl

```perl
use strict;
use warnings;
use ntheory 'divisors';

print "First 15 terms of OEIS: A005179\n";
for my $n (1..15) {
    my $l = 0;
    while (++$l) {
        print "$l " and last if $n == divisors($l);
    }
}
```

```txt
First 15 terms of OEIS: A005179
1 2 4 6 16 12 64 24 36 48 1024 60 4096 192 144
```



## Perl 6

```perl6
sub div-count (\x) {
    return 2 if x.is-prime;
    +flat (1 .. x.sqrt.floor).map: -> \d {
        unless x % d { my \y = x div d; y == d ?? y !! (y, d) }
    }
}

my $limit = 15;

put "First $limit terms of OEIS:A005179";
put (1..$limit).map: -> $n { first { $n == .&div-count }, 1..Inf };


```

```txt
First 15 terms of OEIS:A005179
1 2 4 6 16 12 64 24 36 48 1024 60 4096 192 144
```



## Phix


### naive


```Phix
constant limit = 15
sequence res = repeat(0,limit)
integer found = 0, n = 1
while found<limit do
    integer k = length(factors(n,1))
    if k<=limit and res[k]=0 then
        res[k] = n
        found += 1
    end if
    n += 1
end while
printf(1,"The first %d terms are: %v\n",{limit,res})
```

```txt

The first 15 terms are: {1,2,4,6,16,12,64,24,36,48,1024,60,4096,192,144}

```

You would need something quite a bit smarter to venture over a limit of 28.

### advanced

Using the various formula from the OEIS:A005179 link above.

get_primes() and product() have recently been added as new builtins, if necessary see [[Extensible_prime_generator#Phix|Extensible_prime_generator]] and [[Deconvolution/2D%2B#Phix]].

```Phix
constant limit = iff(machine_bits()=32?58:66)
sequence found = repeat(0,limit)
integer n = 1

procedure populate_found(integer i)
    while found[i]=0 do
        integer k = length(factors(n,1))
        if k<=limit and found[k]=0 then
            found[k] = n
        end if
        n += 1
    end while
end procedure

for i=1 to limit do
    sequence f = factors(i,1)
    integer lf = length(f)
    atom ri
    if lf<=2 then                       ri = power(2,i-1)                               -- prime (or 1)
    elsif lf=3 then                     ri = power(6,f[2]-1)                            -- p^2 (eg f={1,5,25})
    elsif f[2]>2 -- (see note)
      and f[$] = power(f[2],lf-1) then  ri = power(product(get_primes(-(lf-1))),f[2]-1) -- p^k (eg f={1,3,9,27})
    elsif lf=4 then                     ri = power(2,f[3]-1)*power(3,f[2]-1)            -- p*q (eg f={1,2,3,6})
    else populate_found(i)              ri = found[i]                                   -- do the rest manually
    end if
    printf(1,"%d->%d\n",{i,ri})
end for
```

Note: the f[2]>2 test should really be something more like >log(get_primes(-(lf-1))[$])/log(2),
apparently, but everything seems ok within the IEEE 754 53/64 bit limits this imposes.
It takes longer, afaict, to print the answers than it did to calculate them, tee hee!
64-bit (as shown) manages 8 more answers than 32-bit, which as per limit halts on 58: on 32 bit the accuracy limit is 2^53, hence the result for 59, which is 2^58, would get printed wrong since the first /10 needed to print it rounds to the nearest 16 or so. It is quite probably perfectly accurate internally up to much higher limits, but proving/showing that is a bit of a problem, which would in turn probably be easiest to solve by simply rewriting this to use gmp/mpir.

```txt

1->1
2->2
3->4
4->6
5->16
6->12
7->64
8->24
9->36
10->48
11->1024
12->60
13->4096
14->192
15->144
16->120
17->65536
18->180
19->262144
20->240
21->576
22->3072
23->4194304
24->360
25->1296
26->12288
27->900
28->960
29->268435456
30->720
31->1073741824
32->840
33->9216
34->196608
35->5184
36->1260
37->68719476736
38->786432
39->36864
40->1680
41->1099511627776
42->2880
43->4398046511104
44->15360
45->3600
46->12582912
47->70368744177664
48->2520
49->46656
50->6480
51->589824
52->61440
53->4503599627370496
54->6300
55->82944
56->6720
57->2359296
58->805306368
59->288230376151711744
60->5040
61->1152921504606846976
62->3221225472
63->14400
64->7560
65->331776
66->46080

```



### insane

A rather silly (but successful) attempt to reverse engineer all the rules up to 2000.

I got it down to just 11 of them, with only 1 being a complete fudge. Obviously, the
fewer cases each covers, the less sound it is, and those mini-tables for np/p2/p3/p5
and adj are not exactly, um, scientific. Completes in about 0.1s
```Phix
include mpfr.e
mpz r = mpz_init(),
    pn = mpz_init()
sequence rule_names = {},
         rule_counts = {}
for i=1 to 2000 do
    sequence pf = prime_factors(i,true), ri, adj
    integer lf = length(pf), np, p2, p3, p5, p, e
    string what
    if lf>10 then ?9/0 end if
    if lf<=1 then                                   what = "prime (proper rule)"
        np = 1
        adj = {i}
    elsif pf[$]=2 then                              what = "2^k (made up rule)"
        np = lf-1
        p2 = {2,4,4,4,4,4,4,8,8}[np]
        p3 = {2,2,2,2,4,4,4,4,4}[np]
        np = {2,2,3,4,4,5,6,6,7}[np]
        adj = {p2,p3}
    elsif pf[$]=3
      and pf[$-1]=2 then                            what = "2^k*3 (made up rule)"
        np = lf-1
        p2 = {3,3,4,4,4,6,6,6,6}[np]
        p3 = {2,2,3,3,3,4,4,4,4}[np]
        np = {2,3,3,4,5,5,6,7,8}[np]
        adj = {p2,p3}
    elsif lf>4
      and pf[$-1]=2 then                            what="2^k*p (made up rule)"
        np = lf-1
        adj = {0,4}
    elsif lf>4
      and pf[$]=3
      and pf[$-1]=3
      and pf[$-2]=2 then                            what="2^k*3^2*p (made up rule)"
        np = lf-4
        p3 = {3,3,3,4,4}[np]
        p5 = {2,2,2,3,3}[np]
        np = {4,5,6,6,7}[np]
        adj = {6,p3,p5}
    elsif lf>4
      and pf[$]=3
      and pf[$-2]=3
      and pf[$-4]=2 then                            what="2^k*3^3*p (made up rule)"
        np = lf-1
        adj = {6}
    elsif lf>5
      and pf[$]>3
      and pf[$-1]=3
      and pf[$-4]=3
      and pf[2]=3
      and (pf[1]=2 or pf[$]>5) then                 what="2^k*3^4*p (made up rule)"
        np = lf
        adj = {}
    elsif lf>4
      and pf[$-1]=3
      and pf[$-4]=3
      and (lf>5 or pf[$]=3) then                    what="[2^k]*3^(>=4)*p (made up rule)"
        np = lf-1
        adj = {9,pf[$]}&reverse(pf[1..$-3]) -- <bsg>
    elsif lf>=7
      and pf[$]>3
      and pf[$-1]=3
      and pf[$-2]=2 then                            what="2^k*3*p (made up rule)"
        np = lf-1
        adj = {0,4,3}
    elsif i=1440
      and pf={2,2,2,2,2,3,3,5} then                 what="1440 (complete fudge)"
        -- nothing quite like this, nothing to build any pattern from...
        np = 7
        adj = {6,5,3,2,2,2,2}
    else                                            what="general (proper rule)"
        -- (note this incorporates the p^2, (p>2)^k, p*q, and p*m*q rules)
        np = lf
        adj = {}
    end if
    ri = get_primes(-np)
    for j=1 to length(adj) do
        integer aj = adj[j]
        if aj!=0 then pf[-j] = aj end if
    end for
    for j=1 to np do
        ri[j] = {ri[j],pf[-j]-1}
    end for

    string short = ""   -- (eg "2^2*3^3" form)
    mpz_set_si(r,1)     -- (above as big int)
    for j=1 to length(ri) do
        {p, e} = ri[j]
        if length(short) then short &= "*" end if
        short &= sprintf("%d",p)
        if e!=1 then
            short &= sprintf("^%d",{e})
        end if
        mpz_ui_pow_ui(pn,p,e)
        mpz_mul(r,r,pn)
    end for
    if i<=15 or remainder(i-1,250)>=248 or i=1440 then
        string rs = mpz_get_str(r)
        if length(rs)>20 then
            rs[6..-6] = sprintf("<-- %d digits -->",length(rs)-10)
        end if
        if short="2^0" then short = "1" end if
        printf(1,"%4d : %25s %30s %s\n",{i,short,rs,what})
    end if
    integer k = find(what,rule_names)
    if k=0 then
        rule_names = append(rule_names,what)
        rule_counts = append(rule_counts,1)
    else
        rule_counts[k] += 1
    end if
end for
integer lr = length(rule_names)
printf(1,"\nrules(%d):\n",lr)
sequence tags = custom_sort(rule_counts, tagset(lr))
for i=1 to lr do
    integer ti = tags[-i]
    printf(1,"  %30s:%d\n",{rule_names[ti],rule_counts[ti]})
end for
{r,pn} = mpz_free({r,pn})
```

```txt

   1 :                         1                              1 prime (proper rule)
   2 :                         2                              2 prime (proper rule)
   3 :                       2^2                              4 prime (proper rule)
   4 :                       2*3                              6 2^k (made up rule)
   5 :                       2^4                             16 prime (proper rule)
   6 :                     2^2*3                             12 2^k*3 (made up rule)
   7 :                       2^6                             64 prime (proper rule)
   8 :                     2^3*3                             24 2^k (made up rule)
   9 :                   2^2*3^2                             36 general (proper rule)
  10 :                     2^4*3                             48 general (proper rule)
  11 :                      2^10                           1024 prime (proper rule)
  12 :                   2^2*3*5                             60 2^k*3 (made up rule)
  13 :                      2^12                           4096 prime (proper rule)
  14 :                     2^6*3                            192 general (proper rule)
  15 :                   2^4*3^2                            144 general (proper rule)
 249 :                  2^82*3^2    43521<-- 16 digits -->22336 general (proper rule)
 250 :             2^4*3^4*5^4*7                        5670000 general (proper rule)
 499 :                     2^498   81834<-- 140 digits -->97344 prime (proper rule)
 500 :          2^4*3^4*5^4*7*11                       62370000 general (proper rule)
 749 :                 2^106*3^6    59143<-- 25 digits -->22656 general (proper rule)
 750 :        2^4*3^4*5^4*7^2*11                      436590000 general (proper rule)
 999 :          2^36*3^2*5^2*7^2                757632231014400 general (proper rule)
1000 :       2^4*3^4*5^4*7*11*13                      810810000 general (proper rule)
1249 :                    2^1248   48465<-- 366 digits -->22656 prime (proper rule)
1250 :        2^4*3^4*5^4*7^4*11                    21392910000 general (proper rule)
1440 :    2^5*3^4*5^2*7*11*13*17                     1102701600 1440 (complete fudge)
1499 :                    2^1498   87686<-- 441 digits -->37344 prime (proper rule)
1500 :     2^4*3^4*5^4*7^2*11*13                     5675670000 general (proper rule)
1749 :             2^52*3^10*5^2    66483<-- 12 digits -->57600 general (proper rule)
1750 :        2^6*3^4*5^4*7^4*11                    85571640000 general (proper rule)
1999 :                    2^1998   28703<-- 592 digits -->57344 prime (proper rule)
2000 :    2^4*3^4*5^4*7*11*13*17                    13783770000 general (proper rule)

rules(11):
           general (proper rule):1583
             prime (proper rule):304
            2^k*p (made up rule):59
          2^k*3*p (made up rule):9
        2^k*3^3*p (made up rule):9
              2^k (made up rule):9
            2^k*3 (made up rule):9
  [2^k]*3^(>=4)*p (made up rule):8
        2^k*3^2*p (made up rule):5
        2^k*3^4*p (made up rule):4
           1440 (complete fudge):1

```



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


def sequence(max_n=None):
    n = 0
    while True:
        n += 1
        ii = 0
        if max_n is not None:
            if n > max_n:
                break
        while True:
            ii += 1
            if len(divisors(ii)) == n:
                yield ii
                break


if __name__ == '__main__':
    for item in sequence(15):
        print(item)

```

<b>Output:</b>

```Python

1
2
4
6
16
12
64
24
36
48
1024
60
4096
192
144

```



## REXX


```rexx
/*REXX program finds and displays  the   smallest number   with  exactly   N   divisors.*/
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N= 15                    /*Not specified?  Then use the default.*/
say '──divisors──  ──smallest number with N divisors──' /*display title for the numbers.*/
@.=                                              /*the  @  array is used for memoization*/
        do i=1  for N                            /*step through a number of divisors.   */
           do j=1+(i\==1)  by 1+(i\==1)          /*now, search for a number that ≡ #divs*/
           if @.j==.  then iterate               /*has this number already been found?  */
           d= #divs(j);  if d\==i  then iterate  /*get # divisors;  Is not equal?  Skip.*/
           say center(i, 12)  right(j, 19)       /*display the #divs and the smallest #.*/
           @.j=.                                 /*mark as having found #divs for this J*/
           leave                                 /*found a number, so now get the next I*/
           end   /*j*/
        end      /*i*/
exit                                             /*stick a fork in it,  we're all done. */
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
                  do k=3  for x%2-3  by 1+odd  while k<y  /*for odd numbers, skip evens.*/
                  if x//k==0  then do            /*if no remainder, then found a divisor*/
                                   #=#+2;  y=x%k /*bump  #  Pdivs,  calculate limit  Y. */
                                   if k>=y  then do;  #= #-1;  leave;  end      /*limit?*/
                                   end                                          /*  ___ */
                              else if k*k>x  then leave        /*only divide up to √ x  */
                  end   /*k*/                    /* [↑]  this form of DO loop is faster.*/
       return #+1                                /*bump "proper divisors" to "divisors".*/
```

```txt

──divisors──  ──smallest number with N divisors──
     1                         1
     2                         2
     3                         4
     4                         6
     5                        16
     6                        12
     7                        64
     8                        24
     9                        36
     10                       48
     11                     1024
     12                       60
     13                     4096
     14                      192
     15                      144

```



## Ruby


```ruby
require 'prime'

def num_divisors(n)
  n.prime_division.inject(1){|prod, (_p,n)| prod *= (n + 1) }
end

def first_with_num_divs(n)
  (1..).detect{|i| num_divisors(i) == n }
end

p (1..15).map{|n| first_with_num_divs(n) }

```

```txt

[1, 2, 4, 6, 16, 12, 64, 24, 36, 48, 1024, 60, 4096, 192, 144]

```



## Sidef


```ruby
func n_divisors(n) {
    1..Inf -> first_by { .sigma0 == n }
}

say 15.of { n_divisors(_+1) }
```


```txt

[1, 2, 4, 6, 16, 12, 64, 24, 36, 48, 1024, 60, 4096, 192, 144]

```



## zkl


```zkl
fcn countDivisors(n)
   { [1.. n.toFloat().sqrt()].reduce('wrap(s,i){ s + (if(0==n%i) 1 + (i!=n/i)) },0) }
A005179w:=(1).walker(*).tweak(fcn(n){
   var N=0,cache=Dictionary();
   if(cache.find(n)) return(cache.pop(n));	// prune
   while(1){
      if(n == (d:=countDivisors(N+=1))) return(N);
      if(n<d and not cache.find(d)) cache[d]=N;
   }
});
```


```zkl
N:=15;
println("First %d terms of OEIS:A005179".fmt(N));
A005179w.walk(N).concat(" ").println();
```

```txt

First 15 terms of OEIS:A005179
1 2 4 6 16 12 64 24 36 48 1024 60 4096 192 144

```

