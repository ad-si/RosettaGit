+++
title = "Sequence: smallest number greater than previous term with exactly n divisors"
description = ""
date = 2019-09-16T09:49:42Z
aliases = []
[extra]
id = 22259
[taxonomies]
categories = []
tags = []
+++

<!--  This Rosetta Code task previously was named   Anti-prime Plus
      before it was renamed and split into three different tasks.     !-->
{{task}}

Calculate the sequence where each term <strong>a<sub>n</sub></strong> is the '''smallest natural number''' greater than the previous term, that has exactly '''n''' divisors.


;Task
Show here, on this page, at least the first '''15''' terms of the sequence.


;See also
:* [[oeis:A069654|OEIS:A069654]]


;Related tasks
:* [[Sequence: smallest number with exactly n divisors]]
:* [[Sequence: nth number with exactly n divisors‎‎]]





## ALGOL 68

{{trans|Go}}

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

    print( ( "The first ", whole( max, 0 ), " terms of the sequence are:", newline ) );
    INT next := 1;
    FOR i WHILE next <= max DO
        IF next = count divisors( i ) THEN
            print( ( whole( i, 0 ), " " ) );
            next +:= 1
        FI
    OD;
    print( ( newline, newline ) )

END
```

{{out}}

```txt

The first 15 terms of the sequence are:
1 2 4 6 16 18 64 66 100 112 1024 1035 4096 4288 4624

```


## AWK


```AWK

# syntax: GAWK -f SEQUENCE_SMALLEST_NUMBER_GREATER_THAN_PREVIOUS_TERM_WITH_EXACTLY_N_DIVISORS.AWK
# converted from Kotlin
BEGIN {
    limit = 15
    printf("first %d terms:",limit)
    n = 1
    while (n <= limit) {
      if (n == count_divisors(++i)) {
        printf(" %d",i)
        n++
      }
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

{{out}}

```txt

first 15 terms: 1 2 4 6 16 18 64 66 100 112 1024 1035 4096 4288 4624

```



## C

{{trans|Go}}

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
    int i, next = 1;
    printf("The first %d terms of the sequence are:\n", MAX);
    for (i = 1; next <= MAX; ++i) {
        if (next == count_divisors(i)) {
            printf("%d ", i);
            next++;
        }
    }
    printf("\n");
    return 0;
}
```


{{out}}

```txt

The first 15 terms of the sequence are:
1 2 4 6 16 18 64 66 100 112 1024 1035 4096 4288 4624

```



## C++

{{trans|C}}

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
    cout << "The first " << MAX << " terms of the sequence are:" << endl;
    for (int i = 1, next = 1; next <= MAX; ++i) {
        if (next == count_divisors(i)) {
            cout << i << " ";
            next++;
        }
    }
    cout << endl;
    return 0;
}
```


{{out}}

```txt

The first 15 terms of the sequence are:
1 2 4 6 16 18 64 66 100 112 1024 1035 4096 4288 4624

```



## Dyalect


{{trans|Go}}


```dyalect
func countDivisors(n) {
    var count = 0
    var i = 1
    while i * i <= n {
        if n % i == 0 {
            if i == n / i {
                count += 1
            } else {
                count += 2
            }
        }
        i += 1
    }
    return count
}

const max = 15
print("The first \(max) terms of the sequence are:")
var (i, next) = (1, 1)
while next <= max {
    if next == countDivisors(i) {
        print("\(i) ", terminator: "")
        next += 1
    }
    i += 1
}

print()
```


{{out}}


```txt
The first 15 terms of the sequence are:
1 2 4 6 16 18 64 66 100 112 1024 1035 4096 4288 4624
```



## Factor


```factor
USING: io kernel math math.primes.factors prettyprint sequences ;

: next ( n num -- n' num' )
    [ 2dup divisors length = ] [ 1 + ] do until [ 1 + ] dip ;

: A069654 ( n -- seq )
    [ 2 1 ] dip [ [ next ] keep ] replicate 2nip ;

"The first 15 terms of the sequence are:" print 15 A069654 .
```

{{out}}

```txt

The first 15 terms of the sequence are:
{ 1 2 4 6 16 18 64 66 100 112 1024 1035 4096 4288 4624 }

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
    fmt.Println("The first", max, "terms of the sequence are:")
    for i, next := 1, 1; next <= max; i++ {
        if next == countDivisors(i) {
            fmt.Printf("%d ", i)
            next++
        }
    }
    fmt.Println()
}
```


{{out}}

```txt

The first 15 terms of the sequence are:
1 2 4 6 16 18 64 66 100 112 1024 1035 4096 4288 4624

```



## Java

{{trans|C}}

```java
public class AntiPrimesPlus {

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
        System.out.printf("The first %d terms of the sequence are:\n", max);
        for (int i = 1, next = 1; next <= max; ++i) {
            if (next == count_divisors(i)) {
                System.out.printf("%d ", i);
                next++;
            }
        }
        System.out.println();
    }
}
```


{{out}}

```txt

The first 15 terms of the sequence are:
1 2 4 6 16 18 64 66 100 112 1024 1035 4096 4288 4624

```









## Julia

{{trans|Perl}}

```julia
using Primes

function numfactors(n)
    f = [one(n)]
    for (p,e) in factor(n)
        f = reduce(vcat, [f*p^j for j in 1:e], init=f)
    end
    length(f)
end

function A06954(N)
    println("First $N terms of OEIS sequence A069654: ")
    k = 0
    for i in 1:N
        j = k
        while (j += 1) > 0
            if i == numfactors(j)
                print("$j ")
                k = j
                break
            end
        end
    end
end

A06954(15)

```
{{out}}

```txt

First 15 terms of OEIS sequence A069654:
1 2 4 6 16 18 64 66 100 112 1024 1035 4096 4288 4624

```




## Kotlin

{{trans|Go}}

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
    println("The first $MAX terms of the sequence are:")
    var i = 1
    var next = 1
    while (next <= MAX) {
        if (next == countDivisors(i)) {
            print("$i ")
            next++
        }
        i++
    }
    println()
}
```


{{output}}

```txt

The first 15 terms of the sequence are:
1 2 4 6 16 18 64 66 100 112 1024 1035 4096 4288 4624

```



## Pascal

Counting divisors by prime factorisation.<BR>
If divCnt= Count of divisors is prime then the only candidate ist n = prime^(divCnt-1).
There will be more rules. If divCnt is odd then the divisors of divCnt are a^(even_factor*i)*..*k^(even_factor*j).
I think of next = 33 aka 11*3 with the solution 1031^2 * 2^10=1,088,472,064 with a big distance to next= 32 => 1073741830.<BR>
[https://tio.run/##fVRdb9owFH3Pr7gPk0hYWCG0e2hKJcbHhtQCapm0vSCZxAGvwaa2U1ZV/PWxazsQ2k7jIeR@n3t8HLH4RRPd2BCVkLyRbZL9fiPFUpI1dLlmU8nWVE3zQsXey4fRsD8YwnDa23kALx9uJ/0B9Gm@WbEdRgc39wMX6E6ns5/TAfQm4/vJzWAHZ2eQ2jyTNu6PhjuvUFRhsnpWhWa5CtdEr2IvEVxpdN92f0CnHcUeeFnBE80EhyXVffbEUipVj2ufX35nXLejoPyPPZzyOgcWz5Aak/ElcMA0AUwr2JitICOJFlKZMvJAMN6BKLpomr95q96eR/WLeRtWRIHf@tgK6n5kn218Ysq564yDvCciEbIxw8dC6BBX1eSOqlBSpeESDvgWdMk4ZqK7yDHQgZZZEJAeUwzGE8VoKbohkmjYMr0ShbatFVKAqdsVyymMhfYnaerzIIBUoPtL2RpwDezC4f7bne2OP8YT340MjIPy9DA1k2INXGwB2RU8fwaRpsCL9cJs5TayoNrxcbKvHqVvAsFVh78bjtzfCoQVVmQYDoISSAaWkQ40Qa@oK6mKAQ7EmZnNuHRKuqFEl4bb5pRgu5YLHXMsB2b628D/8QEUKPncgby6riBUDB7PtixxZB5gmabRCclIMe6cE2znxAYiMzosFYgJoyHaOKpVEVKpw73VUTnlmTmhzZqoqRHXn8/NCBZy@luHuBkK/r3WtpJpmnO/Nluh4plEKLUQb1dYA4LLNiwQxIM3HIiklzULf9bE8V@pnrHkoSeK46yDZsHMPBonJ@RgmMib28pKwnBhl9NxPf6pAwvaZ4ixFpyegak4Oiy3tseBT9vMCdnaPnSuYS2URkcEwdzWN1qBKVAiL7S7UiUsG4VrOA@gO@6D//ZzY2abb0NQYba4LA2gVjkc@zdaMX5UmJmDsCVdU65pCiTTVG6JTNV77Th@nP7GDgieUlyd4Mmr//pkGrMmMrVWhivs@cnb7/8kWU6Wat@YRH8B Try it online!]

```pascal
program AntiPrimesPlus;
{$IFDEF FPC}
  {$MODE Delphi}
{$ELSE}
  {$APPTYPE CONSOLE} // delphi
{$ENDIF}
uses
  sysutils,math;
const
  MAX =32;

function getDividersCnt(n:Uint32):Uint32;
// getDividersCnt by dividing n into its prime factors
// aka n = 2250 = 2^1*3^2*5^3 has (1+1)*(2+1)*(3+1)= 24 dividers
var
  divi,quot,deltaRes,rest : Uint32;
begin
  result := 1;

  //divi  := 2; //separat without division
  while Not(Odd(n)) do
  Begin
    n := n SHR 1;
    inc(result);
  end;

  //from now on only odd numbers
  divi  := 3;
  while (sqr(divi)<=n) do
  Begin
    DivMod(n,divi,quot,rest);
    if rest = 0 then
    Begin
      deltaRes := 0;
      repeat
        inc(deltaRes,result);
        n := quot;
        DivMod(n,divi,quot,rest);
      until rest <> 0;
      inc(result,deltaRes);
    end;
    inc(divi,2);
  end;
  //if last factor of n is prime
  IF n <> 1 then
    result := result*2;
end;

var
  T0 : Int64;
  i,next,DivCnt: Uint32;
begin
  writeln('The first ',MAX,' anti-primes plus are:');
  T0:= GetTickCount64;
  i := 1;
  next := 1;
  repeat
    DivCnt := getDividersCnt(i);
    IF DivCnt= next then
    Begin
      write(i,' ');
      inc(next);
      //if next is prime then only prime( => mostly 2 )^(next-1) is solution
      IF (next > 4) AND (getDividersCnt(next) = 2) then
        i := 1 shl (next-1) -1;// i is incremented afterwards
    end;
    inc(i);
  until Next > MAX;
  writeln;
  writeln(GetTickCount64-T0,' ms');
end.
```

{{out}}

```txt
The first 32 anti-primes plus are:
1 2 4 6 16 18 64 66 100 112 1024 1035 4096 4288 4624 4632 65536 65572 262144 262192 263169 269312 4194304 4194306 4477456 4493312 4498641 4498752 268435456 268437200 1073741824 1073741830
525 ms
```



## Perl

{{libheader|ntheory}}

```perl
use strict;
use warnings;
use ntheory 'divisors';

print "First 15 terms of OEIS: A069654\n";
my $m = 0;
for my $n (1..15) {
    my $l = $m;
    while (++$l) {
        print("$l "), $m = $l, last if $n == divisors($l);
    }
}
```

{{out}}


```txt
First 15 terms of OEIS: A069654
1 2 4 6 16 18 64 66 100 112 1024 1035 4096 4288 4624
```



## Perl 6

{{works with|Rakudo|2019.03}}


```perl6
sub div-count (\x) {
    return 2 if x.is-prime;
    +flat (1 .. x.sqrt.floor).map: -> \d {
        unless x % d { my \y = x div d; y == d ?? y !! (y, d) }
    }
}

my $limit = 15;

my $m = 1;
put "First $limit terms of OEIS:A069654";
put (1..$limit).map: -> $n { my $ = $m = first { $n == .&div-count }, $m..Inf };

```

{{out}}

```txt

First 15 terms of OEIS:A069654
1 2 4 6 16 18 64 66 100 112 1024 1035 4096 4288 4624

```



## Phix

Uses the optimisation trick from pascal, of n:=power(2,next-1) when next is a prime>4.

```Phix
constant limit = 32
sequence res = repeat(0,limit)
integer next = 1
atom n = 1
while next<=limit do
    integer k = length(factors(n,1))
    if k=next then
        res[k] = n
        next += 1
        if next>4 and length(factors(next,1))=2 then
            n := power(2,next-1)-1 -- n is incremented afterwards
        end if
    end if
    n += 1
end while
printf(1,"The first %d terms are:\n",limit)
pp(res,{pp_Pause,0,pp_StrFmt,1})
```

{{out}}

```txt

The first 32 terms are:
{1,2,4,6,16,18,64,66,100,112,1024,1035,4096,4288,4624,4632,65536,65572,
 262144,262192,263169,269312,4194304,4194306,4477456,4493312,4498641,
 4498752,268435456,268437200,1073741824,1073741830}

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
    previous = 0
    n = 0
    while True:
        n += 1
        ii = previous
        if max_n is not None:
            if n > max_n:
                break
        while True:
            ii += 1
            if len(divisors(ii)) == n:
                yield ii
                previous = ii
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
18
64
66
100
112
1024
1035
4096

```



## REXX

Programming note:   this Rosetta Code task (for 15 sequence numbers) doesn't require any optimization,   but the code was optimized for listing higher numbers.

The method used is to find the number of proper divisors   (up to the integer square root of '''X'''),   and add one.

Optimization was included when examining   ''even''   or   ''odd''   index numbers   (determine how much to increment the   '''do'''   loop).

```rexx
/*REXX program finds and displays   N   numbers of the   "anti─primes plus"   sequence. */
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N= 15                    /*Not specified?  Then use the default.*/
idx= 1                                           /*the maximum number of divisors so far*/
say '──index──  ──anti─prime plus──'             /*display a title for the numbers shown*/
#= 0                                             /*the count of anti─primes found  "  " */
        do i=1  until #==N                       /*step through possible numbers by twos*/
        d= #divs(i);  if d\==idx  then iterate   /*get # divisors;  Is too small?  Skip.*/
        #= # + 1;     idx= idx + 1               /*found an anti─prime #;  set new minD.*/
        say center(#, 8)  right(i, 15)           /*display the index and the anti─prime.*/
        end   /*i*/

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

{{out|output|text=  when using the default input:}}

```txt

──index──  ──anti─prime plus──
   1                   1
   2                   2
   3                   4
   4                   6
   5                  16
   6                  18
   7                  64
   8                  66
   9                 100
   10                112
   11               1024
   12               1035
   13               4096
   14               4288
   15               4624

```



## Ring


```ring

# Project : ANti-primes

see "working..." + nl
see "wait for done..." + nl + nl
see "the first 15 Anti-primes Plus are:" + nl + nl
num = 1
n = 0
result = list(15)
while num < 16
      n = n + 1
      div = factors(n)
      if div = num
         result[num] = n
         num = num + 1
      ok
end
see "["
for n = 1 to len(result)
    if n < len(result)
       see string(result[n]) + ","
    else
       see string(result[n]) + "]" + nl + nl
    ok
next
see "done..." + nl

func factors(an)
     ansum = 2
     if an < 2
        return(1)
     ok
     for nr = 2 to an/2
         if an%nr = 0
            ansum = ansum+1
         ok
     next
     return ansum

```

{{out}}

```txt

working...
wait for done...

the first 15 Anti-primes Plus are:

[1,2,4,6,16,18,64,66,100,112,1024,1035,4096,4288,4624]

done...

```



## Ruby


```ruby
require 'prime'

def num_divisors(n)
  n.prime_division.inject(1){|prod, (_p,n)| prod *= (n + 1) }
end

seq = Enumerator.new do |y|
  cur = 0
  (1..).each do |i|
    if num_divisors(i) == cur + 1 then
      y << i
      cur += 1
    end
  end
end

p seq.take(15)

```

{{out}}

```txt
[1, 2, 4, 6, 16, 18, 64, 66, 100, 112, 1024, 1035, 4096, 4288, 4624]

```




## Sidef


```ruby
func n_divisors(n, from=1) {
    from..Inf -> first_by { .sigma0 == n }
}

with (1) { |from|
    say 15.of { from = n_divisors(_+1, from) }
}
```

{{out}}

```txt

[1, 2, 4, 6, 16, 18, 64, 66, 100, 112, 1024, 1035, 4096, 4288, 4624]

```



## zkl


```zkl
fcn countDivisors(n)
   { [1..(n).toFloat().sqrt()] .reduce('wrap(s,i){ s + (if(0==n%i) 1 + (i!=n/i)) },0) }
```


```zkl
n:=15;
println("The first %d anti-primes plus are:".fmt(n));
(1).walker(*).tweak(
   fcn(n,rn){ if(rn.value==countDivisors(n)){ rn.inc(); n } else Void.Skip }.fp1(Ref(1)))
.walk(n).concat(" ").println();
```

{{out}}

```txt

The first 15 anti-primes plus are:
1 2 4 6 16 18 64 66 100 112 1024 1035 4096 4288 4624

```

