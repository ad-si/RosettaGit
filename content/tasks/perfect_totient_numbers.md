+++
title = "Perfect totient numbers"
description = ""
date = 2019-09-20T21:32:38Z
aliases = []
[extra]
id = 22093
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "awk",
  "c",
  "factor",
  "go",
  "haskell",
  "j",
  "javascript",
  "julia",
  "kotlin",
  "maple",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "racket",
  "related_tasks",
  "rexx",
  "ruby",
  "scala",
  "sidef",
  "swift",
  "zkl",
]
+++

Generate and show here, the first twenty [https://en.wikipedia.org/wiki/Perfect_totient_number Perfect totient numbers].


## Related tasks

::*   [[Totient function]]


;Also see:
::*   the OEIS      entry for   [http://oeis.org/A082897 perfect totient numbers].
::*   mrob            [https://mrob.com/pub/seq/a082897.html list of the first 54]
<br/>


## AWK


```AWK

# syntax: GAWK -f PERFECT_TOTIENT_NUMBERS.AWK
BEGIN {
    i = 20
    printf("The first %d perfect totient numbers:\n%s\n",i,perfect_totient(i))
    exit(0)
}
function perfect_totient(n,  count,m,str,sum,tot) {
    for (m=1; count<n; m++) {
      tot = m
      sum = 0
      while (tot != 1) {
        tot = totient(tot)
        sum += tot
      }
      if (sum == m) {
        str = str m " "
        count++
      }
    }
    return(str)
}
function totient(n,  i,tot) {
    tot = n
    for (i=2; i*i<=n; i+=2) {
      if (n % i == 0) {
        while (n % i == 0) {
          n /= i
        }
        tot -= tot / i
      }
      if (i == 2) {
        i = 1
      }
    }
    if (n > 1) {
      tot -= tot / n
    }
    return(tot)
}

```

```txt

The first 20 perfect totient numbers:
3 9 15 27 39 81 111 183 243 255 327 363 471 729 2187 2199 3063 4359 4375 5571

```


## C

Calculates as many perfect Totient numbers as entered on the command line.

```C
#include<stdlib.h>

#include<stdio.h>

long totient(long n){
	long tot = n,i;
	
	for(i=2;i*i<=n;i+=2){
		if(n%i==0){
			while(n%i==0)
				n/=i;
			tot-=tot/i;
		}
		
		if(i==2)
			i=1;
	}
	
	if(n>1)
		tot-=tot/n;
	
	return tot;
}

long* perfectTotients(long n){
	long *ptList = (long*)malloc(n*sizeof(long)), m,count=0,sum,tot;
	
	for(m=1;count<n;m++){
		 tot = m;
		 sum = 0;
        while(tot != 1){
            tot = totient(tot);
            sum += tot;
        }
        if(sum == m)
			ptList[count++] = m;
        }
		
		return ptList;
}

long main(long argC, char* argV[])
{
	long *ptList,i,n;
	
	if(argC!=2)
		printf("Usage : %s <number of perfect Totient numbers required>",argV[0]);
	else{
		n = atoi(argV[1]);
		
		ptList = perfectTotients(n);
		
		printf("The first %d perfect Totient numbers are : \n[",n);
		
		for(i=0;i<n;i++)
			printf(" %d,",ptList[i]);
		printf("\b]");
	}
	
	return 0;
}

```

Output for multiple runs, a is the default executable file name produced by GCC

```txt

C:\rossetaCode>a 10
The first 10 perfect Totient numbers are :
[ 3, 9, 15, 27, 39, 81, 111, 183, 243, 255]
C:\rossetaCode>a 20
The first 20 perfect Totient numbers are :
[ 3, 9, 15, 27, 39, 81, 111, 183, 243, 255, 327, 363, 471, 729, 2187, 2199, 3063, 4359, 4375, 5571]
C:\rossetaCode>a 30
The first 30 perfect Totient numbers are :
[ 3, 9, 15, 27, 39, 81, 111, 183, 243, 255, 327, 363, 471, 729, 2187, 2199, 3063, 4359, 4375, 5571, 6561, 8751, 15723, 19683, 36759, 46791, 59049, 65535, 140103, 177147]
C:\rossetaCode>a 40
The first 40 perfect Totient numbers are :
[ 3, 9, 15, 27, 39, 81, 111, 183, 243, 255, 327, 363, 471, 729, 2187, 2199, 3063, 4359, 4375, 5571, 6561, 8751, 15723, 19683, 36759, 46791, 59049, 65535, 140103, 177147, 208191, 441027, 531441, 1594323, 4190263, 4782969, 9056583, 14348907, 43046721, 57395631]

```



## Factor


```factor
USING: formatting kernel lists lists.lazy math
math.primes.factors ;

: perfect? ( n -- ? )
    [ 0 ] dip dup [ dup 2 < ] [ totient tuck [ + ] 2dip ] until
    drop = ;

20 1 lfrom [ perfect? ] lfilter ltake list>array
"%[%d, %]\n" printf
```

```txt

{ 3, 9, 15, 27, 39, 81, 111, 183, 243, 255, 327, 363, 471, 729, 2187, 2199, 3063, 4359, 4375, 5571 }

```



## Go


```go
package main

import "fmt"

func gcd(n, k int) int {
    if n < k || k < 1 {
        panic("Need n >= k and k >= 1")
    }

    s := 1
    for n&1 == 0 && k&1 == 0 {
        n >>= 1
        k >>= 1
        s <<= 1
    }

    t := n
    if n&1 != 0 {
        t = -k
    }
    for t != 0 {
        for t&1 == 0 {
            t >>= 1
        }
        if t > 0 {
            n = t
        } else {
            k = -t
        }
        t = n - k
    }
    return n * s
}

func totient(n int) int {
    tot := 0
    for k := 1; k <= n; k++ {
        if gcd(n, k) == 1 {
            tot++
        }
    }
    return tot
}

func main() {
    var perfect []int
    for n := 1; len(perfect) < 20; n += 2 {
        tot := n
        sum := 0
        for tot != 1 {
            tot = totient(tot)
            sum += tot
        }
        if sum == n {
            perfect = append(perfect, n)
        }
    }
    fmt.Println("The first 20 perfect totient numbers are:")
    fmt.Println(perfect)
}
```


```txt

The first 20 perfect totient numbers are:
[3 9 15 27 39 81 111 183 243 255 327 363 471 729 2187 2199 3063 4359 4375 5571]

```


The following much quicker version uses Euler's product formula rather than repeated invocation of the gcd function to calculate the totient:

```go
package main

import "fmt"

func totient(n int) int {
    tot := n
    for i := 2; i*i <= n; i += 2 {
        if n%i == 0 {
            for n%i == 0 {
                n /= i
            }
            tot -= tot / i
        }
        if i == 2 {
            i = 1
        }
    }
    if n > 1 {
        tot -= tot / n
    }
    return tot
}

func main() {
    var perfect []int
    for n := 1; len(perfect) < 20; n += 2 {
        tot := n
        sum := 0
        for tot != 1 {
            tot = totient(tot)
            sum += tot
        }
        if sum == n {
            perfect = append(perfect, n)
        }
    }
    fmt.Println("The first 20 perfect totient numbers are:")
    fmt.Println(perfect)
}
```


The output is the same as before.


## Haskell


```haskell
import Data.Bool (bool)

perfectTotients :: [Int]
perfectTotients =
  [2 ..] >>=
  ((bool [] . return) <*>
   ((==) <*> (succ . sum . tail . takeWhile (1 /=) . iterate φ)))

φ :: Int -> Int
φ = memoize (\n -> length (filter ((1 ==) . gcd n) [1 .. n]))

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

main :: IO ()
main = print $ take 20 perfectTotients
```

```txt
[3,9,15,27,39,81,111,183,243,255,327,363,471,729,2187,2199,3063,4359,4375,5571]
```



## J


```J

Until =: conjunction def 'u^:(0 -: v)^:_'
Filter =: (#~`)(`:6)
totient =: 5&p:
totient_chain =: [: }. (, totient@{:)Until(1={:)
ptnQ =: (= ([: +/ totient_chain))&>

```

With these definitions I've found the first 28 perfect totient numbers

```txt

   PTN =: ptnQ Filter >: i.99999
   #PTN
28
   PTN
3 9 15 27 39 81 111 183 243 255 327 363 471 729 2187 2199 3063 4359 4375 5571 6561 8751 15723 19683 36759 46791 59049 65535

```



## JavaScript


```javascript
(() => {
    'use strict';

    // main :: IO ()
    const main = () =>
        showLog(
            take(20, perfectTotients())
        );

    // perfectTotients :: Generator [Int]
    function* perfectTotients() {
        const
            phi = memoized(
                n => length(
                    filter(
                        k => 1 === gcd(n, k),
                        enumFromTo(1, n)
                    )
                )
            ),
            imperfect = n => n !== sum(
                tail(iterateUntil(
                    x => 1 === x,
                    phi,
                    n
                ))
            );
        let ys = dropWhileGen(imperfect, enumFrom(1))
        while (true) {
            yield ys.next().value - 1;
            ys = dropWhileGen(imperfect, ys)
        }
    }

    // GENERIC FUNCTIONS ----------------------------

    // abs :: Num -> Num
    const abs = Math.abs;

    // dropWhileGen :: (a -> Bool) -> Gen [a] -> [a]
    const dropWhileGen = (p, xs) => {
        let
            nxt = xs.next(),
            v = nxt.value;
        while (!nxt.done && p(v)) {
            nxt = xs.next();
            v = nxt.value;
        }
        return xs;
    };

    // enumFrom :: Int -> [Int]
    function* enumFrom(x) {
        let v = x;
        while (true) {
            yield v;
            v = 1 + v;
        }
    }

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        m <= n ? iterateUntil(
            x => n <= x,
            x => 1 + x,
            m
        ) : [];

    // filter :: (a -> Bool) -> [a] -> [a]
    const filter = (f, xs) => xs.filter(f);

    // gcd :: Int -> Int -> Int
    const gcd = (x, y) => {
        const
            _gcd = (a, b) => (0 === b ? a : _gcd(b, a % b)),
            abs = Math.abs;
        return _gcd(abs(x), abs(y));
    };

    // iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
    const iterateUntil = (p, f, x) => {
        const vs = [x];
        let h = x;
        while (!p(h))(h = f(h), vs.push(h));
        return vs;
    };

    // Returns Infinity over objects without finite length.
    // This enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // memoized :: (a -> b) -> (a -> b)
    const memoized = f => {
        const dctMemo = {};
        return x => {
            const v = dctMemo[x];
            return undefined !== v ? v : (dctMemo[x] = f(x));
        };
    };

    // showLog :: a -> IO ()
    const showLog = (...args) =>
        console.log(
            args
            .map(JSON.stringify)
            .join(' -> ')
        );

    // sum :: [Num] -> Num
    const sum = xs => xs.reduce((a, x) => a + x, 0);

    // tail :: [a] -> [a]
    const tail = xs => 0 < xs.length ? xs.slice(1) : [];

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        'GeneratorFunction' !== xs.constructor.constructor.name ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // MAIN ---
    main();
})();
```

```txt
[3,9,15,27,39,81,111,183,243,255,327,363,471,729,2187,2199,3063,4359,4375,5571]
```



## Julia


```julia
using Primes

eulerphi(n) = (r = one(n); for (p,k) in factor(abs(n)) r *= p^(k-1)*(p-1) end; r)

const phicache = Dict{Int, Int}()

cachedphi(n) = (if !haskey(phicache, n) phicache[n] = eulerphi(n) end; phicache[n])

function perfecttotientseries(n)
    perfect = Vector{Int}()
    i = 1
    while length(perfect) < n
        tot = i
        tsum = 0
        while tot != 1
            tot = cachedphi(tot)
            tsum += tot
        end
        if tsum == i
            push!(perfect, i)
        end
        i += 1
    end
    perfect
end

println("The first 20 perfect totient numbers are: $(perfecttotientseries(20))")
println("The first 40 perfect totient numbers are: $(perfecttotientseries(40))")

```
```txt

 The first 20 perfect totient numbers are: [3, 9, 15, 27, 39, 81, 111, 183, 243, 255, 327, 363, 471, 729, 2187, 2199, 3063, 4359, 4375, 5571]
 The first 40 perfect totient numbers are: [3, 9, 15, 27, 39, 81, 111, 183, 243, 255, 327, 363, 471, 729, 2187, 2199, 3063, 4359, 4375, 5571, 6561, 8751, 15723, 19683, 36759, 46791, 59049, 65535, 140103, 177147, 208191, 441027, 531441, 1594323, 4190263, 4782969, 9056583, 14348907, 43046721, 57395631]

```



## Kotlin

```scala
// Version 1.3.21

fun totient(n: Int): Int {
    var tot = n
    var nn = n
    var i = 2
    while (i * i <= nn) {
        if (nn % i == 0) {
            while (nn % i == 0) nn /= i
            tot -= tot / i
        }
        if (i == 2) i = 1
        i += 2
    }
    if (nn > 1) tot -= tot / nn
    return tot
}

fun main() {
    val perfect = mutableListOf<Int>()
    var n = 1
    while (perfect.size < 20) {
        var tot = n
        var sum = 0
        while (tot != 1) {
            tot = totient(tot)
            sum += tot
        }
        if (sum == n) perfect.add(n)
        n += 2
    }
    println("The first 20 perfect totient numbers are:")
    println(perfect)
}
```


```txt

The first 20 perfect totient numbers are:
[3, 9, 15, 27, 39, 81, 111, 183, 243, 255, 327, 363, 471, 729, 2187, 2199, 3063, 4359, 4375, 5571]

```



## Maple


```Maple
iterated_totient := proc(n::posint, total)
 if NumberTheory:-Totient(n) = 1 then
   return total + 1;
 else
   return iterated_totient(NumberTheory:-Totient(n), total + NumberTheory:-Totient(n));
 end if;
end proc:

isPerfect := n -> evalb(iterated_totient(n, 0) = n):

count := 0:
num_list := []:
for i while count < 20 do
 if isPerfectTotient(i) then
  num_list := [op(num_list), i];
  count := count + 1;
 end if;
end do;
num_list;
```

```txt

[3, 9, 15, 27, 39, 81, 111, 183, 243, 255, 327, 363, 471, 729, 2187, 2199, 3063, 4359, 4375, 5571]

```



## Pascal

I am using a really big array to calculate the Totient of every number up to 1.162.261.467, the 46.te perfect totient number.
( I can only test up to 1.5e9 before I get - out of memory ( 6.5 GB ) ). 
I'm doing this, by using only prime numbers to calculate the Totientnumbers.
After that I sum up the totient numbers Tot[i] := Tot[i]+Tot[Tot[i]];
Tot[Tot[i]] is always < Tot[i], so it is already calculated. So I needn't calculations going trough so whole array ending up in Tot[2].<BR>With limit 57395631 it takes "real	0m2,025s "<BR>
The c-program takes "real	3m12,481s"<BR>
A test with using floating point/SSE is by 2 seconds faster for 46.th perfect totient number, with the coming new Version of Freepascal 3.2.0

```pascal
program Perftotient;
{$IFdef FPC}
  {$MODE DELPHI} {$CodeAlign proc=32,loop=1}
{$IFEND}
uses
  sysutils;
const
  cLimit = 57395631;//177147;//4190263;//57395631;//1162261467;//
//global
var
  TotientList : array of LongWord;
  Sieve : Array of byte;
  SolList : array of LongWord;
  T1,T0 : INt64;

procedure SieveInit(svLimit:NativeUint);
var
  pSieve:pByte;
  i,j,pr :NativeUint;
Begin
  svlimit := (svLimit+1) DIV 2;
  setlength(sieve,svlimit+1);
  pSieve := @Sieve[0];
  For i := 1 to svlimit do
  Begin
    IF pSieve[i]= 0 then
    Begin
      pr := 2*i+1;
      j := (sqr(pr)-1) DIV 2;
      IF  j> svlimit then
        BREAK;
      repeat
        pSieve[j]:= 1;
        inc(j,pr);
      until j> svlimit;
    end;
  end;
  pr := 0;
  j := 0;
  For i := 1 to svlimit do
  Begin
    IF pSieve[i]= 0 then
    Begin
      pSieve[j] := i-pr;
      inc(j);
      pr := i;
    end;
  end;
  setlength(sieve,j);
end;

procedure TotientInit(len: NativeUint);
var
  pTotLst : pLongWord;
  pSieve  : pByte;
  test : double;
  i: NativeInt;
  p,j,k,svLimit : NativeUint;
Begin
  SieveInit(len);
  T0:= GetTickCount64;
  setlength(TotientList,len+12);
  pTotLst := @TotientList[0];

//Fill totient with simple start values for odd and even numbers
//and multiples of 3
  j := 1;
  k := 1;// k == j DIV 2
  p := 1;// p == j div 3;
  repeat
    pTotLst[j] := j;//1
    pTotLst[j+1] := k;//2 j DIV 2; //2
    inc(k);
    inc(j,2);
    pTotLst[j] := j-p;//3
    inc(p);
    pTotLst[j+1] := k;//4  j div 2
    inc(k);
    inc(j,2);
    pTotLst[j] := j;//5
    pTotLst[j+1] := p;//6   j DIV 3 <=  (div 2) * 2 DIV/3
    inc(j,2);
    inc(p);
    inc(k);
  until j>len+6;

//correct values of totient by prime factors
  svLimit := High(sieve);
  p := 3;// starting after 3
  pSieve := @Sieve[svLimit+1];
  i := -svlimit;
  repeat
    p := p+2*pSieve[i];
    j := p;
//  Test := (1-1/p);
    while j <= cLimit do
    Begin
//    pTotLst[j] := trunc(pTotLst[j]*Test);
      k:= pTotLst[j];
      pTotLst[j]:= k-(k DIV p);
      inc(j,p);
    end;
    inc(i);
  until i=0;

  T1:= GetTickCount64;
  writeln('totient calculated in ',T1-T0,' ms');
  setlength(sieve,0);
end;

function GetPerfectTotient(len: NativeUint):NativeUint;
var
  pTotLst : pLongWord;
  i,sum: NativeUint;
Begin
  T0:= GetTickCount64;
  pTotLst := @TotientList[0];
  setlength(SolList,100);
  result := 0;
  For i := 3 to Len do
  Begin
    sum := pTotLst[i];
    pTotLst[i] := sum+pTotLst[sum];
  end;
  //Check for solution ( IF ) in seperate loop ,reduces time consuption ~ 12% for this function
  For i := 3 to Len do
    IF pTotLst[i] =i then
    Begin
      SolList[result] := i;
      inc(result);
    end;

  T1:= GetTickCount64;
  setlength(SolList,result);
  writeln('calculated totientsum in ',T1-T0,' ms');
  writeln('found ',result,' perfect totient numbers');
end;

var
  j,k : NativeUint;

Begin
  TotientInit(climit);
  GetPerfectTotient(climit);
  k := 0;
  For j := 0 to High(Sollist) do
  Begin
    inc(k);
    if k > 4 then
    Begin
      writeln(Sollist[j]);
      k := 0;
    end
    else
      write(Sollist[j],',');
  end;
end.
```

;OutPut:

```txt
compiled with fpc 3.0.4 -O3 "Perftotient.pas"
totient calculated in 32484 ms
calculated totientsum in 8244 ms
found 46 perfect totient numbers
3,9,15,27,39
81,111,183,243,255
327,363,471,729,2187
2199,3063,4359,4375,5571
6561,8751,15723,19683,36759
46791,59049,65535,140103,177147
208191,441027,531441,1594323,4190263
4782969,9056583,14348907,43046721,57395631
129140163,172186887,236923383,387420489,918330183
1162261467,
real  0m47,690s
*
found 40 perfect totient numbers
...
real  0m2,025s
```



## Perl

```perl
use ntheory qw(euler_phi);

sub phi_iter {
    my($p) = @_;
    euler_phi($p) + ($p == 2 ? 0 : phi_iter(euler_phi($p)));
}

my @perfect;
for (my $p = 2; @perfect < 20 ; ++$p) {
    push @perfect, $p if $p == phi_iter($p);
}

printf "The first twenty perfect totient numbers:\n%s\n", join ' ', @perfect;
```

```txt
The first twenty Perfect totient numbers:
3 9 15 27 39 81 111 183 243 255 327 363 471 729 2187 2199 3063 4359 4375 5571
```



## Perl 6

```perl6
my \𝜑  = Nil, |(1..*).hyper.map: -> $t { +(^$t).grep: * gcd $t == 1 };
my \𝜑𝜑 = Nil, |(2..*).grep: -> $p { $p == sum 𝜑[$p], { 𝜑[$_] } … 1 };

put "The first twenty Perfect totient numbers:\n",  𝜑𝜑[1..20];
```

```txt
The first twenty Perfect totient numbers:
3 9 15 27 39 81 111 183 243 255 327 363 471 729 2187 2199 3063 4359 4375 5571
```



## Phix

```Phix
function totient(integer n)
    integer tot = n, i = 2
    while i*i<=n do
        if mod(n,i)=0 then
            while true do
                n /= i
                if mod(n,i)!=0 then exit end if
            end while
            tot -= tot/i
        end if
        i += iff(i=2?1:2)
    end while
    if n>1 then
        tot -= tot/n
    end if
    return tot
end function
 
sequence perfect = {}
integer n = 1
while length(perfect)<20 do
    integer tot = n,
            tsum = 0
    while tot!=1 do
        tot = totient(tot)
        tsum += tot
    end while
    if tsum=n then
        perfect &= n
    end if
    n += 2
end while
printf(1,"The first 20 perfect totient numbers are:\n")
?perfect
```

```txt

The first 20 perfect totient numbers are:
{3,9,15,27,39,81,111,183,243,255,327,363,471,729,2187,2199,3063,4359,4375,5571}

```



## PicoLisp


```PicoLisp
(gc 16)
(de gcd (A B)
   (until (=0 B)
      (let M (% A B)
         (setq A B B M) ) )
   (abs A) )
(de totient (N)
   (let C 0
      (for I N
         (and (=1 (gcd N I)) (inc 'C)) )
      C ) )
(de totients (NIL)
   (let (C 0  N 1)
      (while (> 20 C)
         (let (Cur N  S 0)
            (while (> Cur 1)
               (inc 'S (setq Cur (totient Cur))) )
            (when (= S N)
               (inc 'C)
               (prin N " ")
               (flush) )
            (inc 'N 2) ) )
      (prinl) ) )
(totients)
```

```txt

3 9 15 27 39 81 111 183 243 255 327 363 471 729 2187 2199 3063 4359 4375 5571

```



## Python


```python
from math import gcd
from functools import lru_cache
from itertools import islice, count

@lru_cache(maxsize=None)
def  φ(n):
    return sum(1 for k in range(1, n + 1) if gcd(n, k) == 1)

def perfect_totient():
    for n0 in count(1):
        parts, n = 0, n0
        while n != 1:
            n = φ(n)
            parts += n
        if parts == n0:
            yield n0
        

if __name__ == '__main__':
    print(list(islice(perfect_totient(), 20)))
```


```txt
[3, 9, 15, 27, 39, 81, 111, 183, 243, 255, 327, 363, 471, 729, 2187, 2199, 3063, 4359, 4375, 5571]
```



## Racket


```Racket

#lang racket
(require math/number-theory)

(define (tot n)
  (match n
    [1 0]
    [n (define t (totient n))
       (+ t (tot t))]))

(define (perfect? n)
  (= n (tot n)))

(define-values (ns i)
  (for/fold ([ns '()] [i 0])
            ([n (in-naturals 1)]
             #:break (= i 20)
             #:when (perfect? n))
    (values (cons n ns) (+ i 1))))

(reverse ns)

```


## REXX


### unoptimized


```rexx
/*REXX program  calculates and displays  the first   N   perfect totient  numbers.      */
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N= 20                    /*Not specified?  Then use the default.*/
@.=.                                             /*memoization array of totient numbers.*/
p= 0                                             /*the count of perfect    "       "    */
$=                                               /*list of the     "       "       "    */
    do j=3  by 2  until p==N;   s= phi(j)        /*obtain totient number for a number.  */
    a= s                                         /* [↓]  search for a perfect totient #.*/
                                do until a==1;           a= phi(a);            s= s + a
                                end   /*until*/
    if s\==j  then iterate                       /*Is  J  not a perfect totient number? */
    p= p + 1                                     /*bump count of perfect totient numbers*/
    $= $ j                                       /*add to perfect totient numbers list. */
    end   /*j*/

say 'The first '  N  " perfect totient numbers:" /*display the header to the terminal.  */
say strip($)                                     /*   "     "  list.   "  "     "       */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
gcd: parse arg x,y;   do  until y==0;  parse value  x//y  y   with   y  x;  end;  return x
/*──────────────────────────────────────────────────────────────────────────────────────*/
phi: procedure expose @.; parse arg z;   if @.z\==.  then return @.z /*was found before?*/
     #= z==1;         do m=1  for z-1;   if gcd(m, z)==1  then #= # + 1;    end  /*m*/
     @.z= #;   return #                                              /*use memoization. */
```

```txt

The first  20  perfect totient numbers:
3 9 15 27 39 81 111 183 243 255 327 363 471 729 2187 2199 3063 4359 4375 5571

```



### optimized

This REXX version is over   ''twice''   as fast as the unoptimized version.

It takes advantage of the fact that all known perfect totient numbers less than   '''3<sup>22</sup>'''   have one of these factors:   '''3''',   '''5''',   or   '''7'''

('''3<sup>22</sup>'''   '''='''   '''31,381,059,609'''). 

```rexx
/*REXX program  calculates and displays  the first   N   perfect totient  numbers.      */
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N= 20                    /*Not specified?  Then use the default.*/
@.=.                                             /*memoization array of totient numbers.*/
p= 0                                             /*the count of perfect    "       "    */
$=                                               /*list of the     "       "       "    */
     do j=3  by 2  until p==N                    /*obtain the totient number for index J*/
     if j//3\==0   then  if j//5\==0   then  if j//7\==0   then iterate
     s= phi(j);  a= s                            /* [↑]  J  must have 1 of these factors*/
                               do until a==1;  if @.a==.  then a= phi(a);    else a= @.a
                                               s= s + a
                               end   /*until*/
     if s\==j  then iterate                      /*Is  J  not a perfect totient number? */
     p= p + 1                                    /*bump count of perfect totient numbers*/
     $= $ j                                      /*add to perfect totient numbers list. */
     end   /*j*/

say 'The first '  N  " perfect totient numbers:" /*display the header to the terminal.  */
say strip($)                                     /*   "     "  list.   "  "     "       */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
gcd: parse arg x,y;   do  until y==0;  parse value  x//y  y   with   y  x;  end;  return x
/*──────────────────────────────────────────────────────────────────────────────────────*/
phi: procedure expose @.; parse arg z;   if @.z\==.  then return @.z /*was found before?*/
     #= z==1;         do m=1  for z-1;   if gcd(m, z)==1  then #= # + 1;    end  /*m*/
     @.z= #;   return #                                              /*use memoization. */
```

## Ruby


```ruby
require "prime"

class Integer 

  def φ
    prime_division.inject(1) {|res, (pr, exp)| res *= (pr-1) * pr**(exp-1) } 
  end

  def perfect_totient?
    f, sum = self, 0
    until f == 1 do
      f = f.φ
      sum += f
    end
    self == sum
  end

end

puts (1..).lazy.select(&:perfect_totient?).first(20).join(", ")

```

```txt
3, 9, 15, 27, 39, 81, 111, 183, 243, 255, 327, 363, 471, 729, 2187, 2199, 3063, 4359, 4375, 5571

```



## Scala


In this example we define a function which determines whether or not a number is a perfect totient number, then use it to construct a lazily evaluated list which contains all perfect totient numbers. Calculating the first n perfect totient numbers only requires taking the first n elements from the list.

```scala
//List of perfect totients
def isPerfectTotient(num: Int): Boolean = LazyList.iterate(totient(num))(totient).takeWhile(_ != 1).foldLeft(0L)(_+_) + 1 == num
def perfectTotients: LazyList[Int] = LazyList.from(3).filter(isPerfectTotient)

//Totient Function
@tailrec def scrub(f: Long, num: Long): Long = if(num%f == 0) scrub(f, num/f) else num
def totient(num: Long): Long = LazyList.iterate((num, 2: Long, num)){case (ac, i, n) => if(n%i == 0) (ac*(i - 1)/i, i + 1, scrub(i, n)) else (ac, i + 1, n)}.dropWhile(_._3 != 1).head._1
```


```txt
scala> perfectTotients.take(20).mkString(", ")
res1: String = 3, 9, 15, 27, 39, 81, 111, 183, 243, 255, 327, 363, 471, 729, 2187, 2199, 3063, 4359, 4375, 5571
```



## Sidef


```ruby
func perfect_totient({.<=1}, sum=0) { sum }
func perfect_totient(     n, sum=0) { __FUNC__(var(t = n.euler_phi), sum + t) }

say (1..Inf -> lazy.grep {|n| perfect_totient(n) == n }.first(20))
```

```txt

[3, 9, 15, 27, 39, 81, 111, 183, 243, 255, 327, 363, 471, 729, 2187, 2199, 3063, 4359, 4375, 5571]

```



## Swift



```swift
public func totient(n: Int) -> Int {
  var n = n
  var i = 2
  var tot = n

  while i * i <= n {
    if n % i == 0 {
      while n % i == 0 {
        n /= i
      }

      tot -= tot / i
    }

    if i == 2 {
      i = 1
    }

    i += 2
  }

  if n > 1 {
    tot -= tot / n
  }

  return tot
}

public struct PerfectTotients: Sequence, IteratorProtocol {
  private var m = 1

  public init() { }

  public mutating func next() -> Int? {
    while true {
      defer {
        m += 1
      }

      var tot = m
      var sum = 0

      while tot != 1 {
        tot = totient(n: tot)
        sum += tot
      }

      if sum == m {
        return m
      }
    }
  }
}

print("The first 20 perfect totient numbers are:")
print(Array(PerfectTotients().prefix(20)))
```


```txt
The first 20 perfect totient numbers are:
[3, 9, 15, 27, 39, 81, 111, 183, 243, 255, 327, 363, 471, 729, 2187, 2199, 3063, 4359, 4375, 5571]
```



## zkl


```zkl
var totients=List.createLong(10_000,0);	// cache
fcn totient(n){ if(phi:=totients[n]) return(phi);
   totients[n]=[1..n].reduce('wrap(p,k){ p + (n.gcd(k)==1) }) 
}
fcn perfectTotientW{	// -->iterator
   (1).walker(*).tweak(fcn(z){
      parts,n := 0,z;
      while(n!=1){ parts+=( n=totient(n) ) }
      if(parts==z) z else Void.Skip;
   })
}
```


```zkl
perfectTotientW().walk(20).println();
```

```txt

L(3,9,15,27,39,81,111,183,243,255,327,363,471,729,2187,2199,3063,4359,4375,5571)

```

