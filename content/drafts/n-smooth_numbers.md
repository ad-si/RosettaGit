+++
title = "N-smooth numbers"
description = ""
date = 2019-09-16T02:13:14Z
aliases = []
[extra]
id = 22496
[taxonomies]
categories = []
tags = []
+++

{{task|Prime numbers}}

'''n-smooth'''   numbers are positive integers which have no prime factors <big> &gt; </big> '''n'''.

The   '''n'''   (when using it in the expression   '''n-smooth'''   is always prime,

there are   <u>no</u>   '''9-smooth''' numbers.

'''1'''   (unity)   is always included in n-smooth numbers.



2-smooth   numbers are non-negative powers of two.

5-smooth   numbers are also called         '''Hamming       numbers'''.

7-smooth   numbers are also called    '''humble   numbers'''.



A way to express   11-smooth   numbers is:

  <big><big> 11-smooth  =  2<sup>i</sup> &times; 3<sup>j</sup> &times; 5<sup>k</sup> &times; 7<sup>m</sup> &times; 11<sup>p</sup></big></big>

            where   <big>  i, j, k, m, p &ge;</big> 0


;Task:
:*   show the first                                '''25'''   n-smooth numbers   for   '''n=2'''     ───►   '''n=29'''
:*   show   three numbers starting with    '''3,000'''   n-smooth numbers   for   '''n=3'''     ───►   '''n=29'''
:*   show       twenty numbers starting with   '''30,000'''   n-smooth numbers   for   '''n=503'''   ───►   '''n=521'''   (optional)


All ranges   (for   '''n''')   are to be inclusive, and only prime numbers are to be used.

The (optional) n-smooth numbers for the third range are:   '''503''',   '''509''',   and   '''521'''.

Show all n-smooth numbers for any particular   '''n'''   in a horizontal list.

Show all output here on this page.


;Related tasks:
:*   [[Hamming numbers]]
:*   [[humble  numbers]]


;References:
:*   Wikipedia entry:   [[wp:Hamming numbers|Hamming numbers]]     (this link is re-directed to   '''Regular number''').
:*   Wikipedia entry:   [[wp:Smooth number|Smooth number]]
:*   OEIS entry:        [http://oeis.org/A000079 A000079    2-smooth numbers or non-negative powers of two]
:*   OEIS entry:        [http://oeis.org/A003586 A003586    3-smooth numbers]
:*   OEIS entry:        [http://oeis.org/A051037 A051037    5-smooth numbers or Hamming numbers]
:*   OEIS entry:        [http://oeis.org/A002473 A002473    7-smooth numbers or humble  numbers]
:*   OEIS entry:        [http://oeis.org/A051038 A051038        11-smooth numbers]
:*   OEIS entry:        [http://oeis.org/A080197 A080197        13-smooth numbers]
:*   OEIS entry:        [http://oeis.org/A080681 A080681        17-smooth numbers]
:*   OEIS entry:        [http://oeis.org/A080682 A080682        19-smooth numbers]
:*   OEIS entry:        [http://oeis.org/A080683 A080683        23-smooth numbers]






## Factor


```factor
USING: deques dlists formatting fry io kernel locals make math
math.order math.primes math.text.english namespaces prettyprint
sequences tools.memory.private ;
IN: rosetta-code.n-smooth-numbers

SYMBOL: primes
 
: ns ( n -- seq )
    primes-upto [ primes set ] [ length [ 1 1dlist ] replicate ]
    bi ;
 
: enqueue ( n seq -- )
    [ primes get ] 2dip [ '[ _ * ] map ] dip [ push-back ] 2each
    ;
 
: next ( seq -- n )
    dup [ peek-front ] map infimum
    [ '[ dup peek-front _ = [ pop-front* ] [ drop ] if ] each ]
    [ swap enqueue ] [ nip ] 2tri ;
 
: next-n ( seq n -- seq )
    swap '[ _ [ _ next , ] times ] { } make ;

:: n-smooth ( n from to -- seq )
    n ns to next-n to from - 1 + tail* ;
 
:: show-smooth ( plo phi lo hi -- )
    plo phi primes-between [
        :> p lo commas lo ordinal-suffix hi commas hi
        ordinal-suffix p "%s%s through %s%s %d-smooth numbers: "
        printf p lo hi n-smooth [ pprint bl ] each nl        
    ] each ;

: smooth-numbers-demo ( -- )
    2 29 1 25 show-smooth nl
    3 29 3000 3002 show-smooth nl
    503 521 30,000 30,019 show-smooth ;

MAIN: smooth-numbers-demo
```

{{out}}

```txt

1st through 25th 2-smooth numbers: 1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536 131072 262144 524288 1048576 2097152 4194304 8388608 16777216 
1st through 25th 3-smooth numbers: 1 2 3 4 6 8 9 12 16 18 24 27 32 36 48 54 64 72 81 96 108 128 144 162 192 
1st through 25th 5-smooth numbers: 1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 40 45 48 50 54 
1st through 25th 7-smooth numbers: 1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36 
1st through 25th 11-smooth numbers: 1 2 3 4 5 6 7 8 9 10 11 12 14 15 16 18 20 21 22 24 25 27 28 30 32 
1st through 25th 13-smooth numbers: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 20 21 22 24 25 26 27 28 
1st through 25th 17-smooth numbers: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 20 21 22 24 25 26 27 
1st through 25th 19-smooth numbers: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 24 25 26 
1st through 25th 23-smooth numbers: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 
1st through 25th 29-smooth numbers: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 

3,000th through 3,002nd 3-smooth numbers: 91580367978306252441724649472 92829823186414819915547541504 94096325042746502515294076928 
3,000th through 3,002nd 5-smooth numbers: 278942752080 279936000000 281250000000 
3,000th through 3,002nd 7-smooth numbers: 50176000 50331648 50388480 
3,000th through 3,002nd 11-smooth numbers: 2112880 2116800 2117016 
3,000th through 3,002nd 13-smooth numbers: 390000 390390 390625 
3,000th through 3,002nd 17-smooth numbers: 145800 145860 146016 
3,000th through 3,002nd 19-smooth numbers: 74256 74358 74360 
3,000th through 3,002nd 23-smooth numbers: 46552 46575 46585 
3,000th through 3,002nd 29-smooth numbers: 33516 33524 33534 

30,000th through 30,019th 503-smooth numbers: 62913 62914 62916 62918 62920 62923 62926 62928 62930 62933 62935 62937 62944 62946 62951 62952 62953 62957 62959 62964 
30,000th through 30,019th 509-smooth numbers: 62601 62602 62604 62607 62608 62609 62611 62618 62620 62622 62624 62625 62626 62628 62629 62634 62640 62643 62645 62646 
30,000th through 30,019th 521-smooth numbers: 62287 62288 62291 62292 62300 62304 62307 62308 62310 62315 62320 62321 62322 62325 62328 62329 62330 62331 62335 62336 

```



## Go


```go
package main

import (
    "fmt"
    "log"
    "math/big"
)

var (
    primes      []*big.Int
    smallPrimes []int
)

// cache all primes up to 521
func init() {
    two := big.NewInt(2)
    three := big.NewInt(3)
    p521 := big.NewInt(521)
    p29 := big.NewInt(29)
    primes = append(primes, two)
    smallPrimes = append(smallPrimes, 2)
    for i := three; i.Cmp(p521) <= 0; i.Add(i, two) {
        if i.ProbablyPrime(0) {
            primes = append(primes, new(big.Int).Set(i))
            if i.Cmp(p29) <= 0 {
                smallPrimes = append(smallPrimes, int(i.Int64()))
            }
        }
    }
}

func min(bs []*big.Int) *big.Int {
    if len(bs) == 0 {
        log.Fatal("slice must have at least one element")
    }
    res := bs[0]
    for _, i := range bs[1:] {
        if i.Cmp(res) < 0 {
            res = i
        }
    }
    return res
}

func nSmooth(n, size int) []*big.Int {
    if n < 2 || n > 521 {
        log.Fatal("n must be between 2 and 521")
    }
    if size < 1 {
        log.Fatal("size must be at least 1")
    }
    bn := big.NewInt(int64(n))
    ok := false
    for _, prime := range primes {
        if bn.Cmp(prime) == 0 {
            ok = true
            break
        }
    }
    if !ok {
        log.Fatal("n must be a prime number")
    }

    ns := make([]*big.Int, size)
    ns[0] = big.NewInt(1)
    var next []*big.Int
    for i := 0; i < len(primes); i++ {
        if primes[i].Cmp(bn) > 0 {
            break
        }
        next = append(next, new(big.Int).Set(primes[i]))
    }
    indices := make([]int, len(next))
    for m := 1; m < size; m++ {
        ns[m] = new(big.Int).Set(min(next))
        for i := 0; i < len(indices); i++ {
            if ns[m].Cmp(next[i]) == 0 {
                indices[i]++
                next[i].Mul(primes[i], ns[indices[i]])
            }
        }
    }
    return ns
}

func main() {
    for _, i := range smallPrimes {
        fmt.Printf("The first 25 %d-smooth numbers are:\n", i)
        fmt.Println(nSmooth(i, 25), "\n")
    }
    for _, i := range smallPrimes[1:] {
        fmt.Printf("The 3,000th to 3,202nd %d-smooth numbers are:\n", i)
        fmt.Println(nSmooth(i, 3002)[2999:], "\n")
    }
    for _, i := range []int{503, 509, 521} {
        fmt.Printf("The 30,000th to 30,019th %d-smooth numbers are:\n", i)
        fmt.Println(nSmooth(i, 30019)[29999:], "\n")
    }
}
```


{{out}}

```txt

The first 25 2-smooth numbers are:
[1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536 131072 262144 524288 1048576 2097152 4194304 8388608 16777216] 

The first 25 3-smooth numbers are:
[1 2 3 4 6 8 9 12 16 18 24 27 32 36 48 54 64 72 81 96 108 128 144 162 192] 

The first 25 5-smooth numbers are:
[1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 40 45 48 50 54] 

The first 25 7-smooth numbers are:
[1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36] 

The first 25 11-smooth numbers are:
[1 2 3 4 5 6 7 8 9 10 11 12 14 15 16 18 20 21 22 24 25 27 28 30 32] 

The first 25 13-smooth numbers are:
[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 20 21 22 24 25 26 27 28] 

The first 25 17-smooth numbers are:
[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 20 21 22 24 25 26 27] 

The first 25 19-smooth numbers are:
[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 24 25 26] 

The first 25 23-smooth numbers are:
[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25] 

The first 25 29-smooth numbers are:
[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25] 

The 3,000th to 3,202nd 3-smooth numbers are:
[91580367978306252441724649472 92829823186414819915547541504 94096325042746502515294076928] 

The 3,000th to 3,202nd 5-smooth numbers are:
[278942752080 279936000000 281250000000] 

The 3,000th to 3,202nd 7-smooth numbers are:
[50176000 50331648 50388480] 

The 3,000th to 3,202nd 11-smooth numbers are:
[2112880 2116800 2117016] 

The 3,000th to 3,202nd 13-smooth numbers are:
[390000 390390 390625] 

The 3,000th to 3,202nd 17-smooth numbers are:
[145800 145860 146016] 

The 3,000th to 3,202nd 19-smooth numbers are:
[74256 74358 74360] 

The 3,000th to 3,202nd 23-smooth numbers are:
[46552 46575 46585] 

The 3,000th to 3,202nd 29-smooth numbers are:
[33516 33524 33534] 

The 30,000th to 30,019th 503-smooth numbers are:
[62913 62914 62916 62918 62920 62923 62926 62928 62930 62933 62935 62937 62944 62946 62951 62952 62953 62957 62959 62964] 

The 30,000th to 30,019th 509-smooth numbers are:
[62601 62602 62604 62607 62608 62609 62611 62618 62620 62622 62624 62625 62626 62628 62629 62634 62640 62643 62645 62646] 

The 30,000th to 30,019th 521-smooth numbers are:
[62287 62288 62291 62292 62300 62304 62307 62308 62310 62315 62320 62321 62322 62325 62328 62329 62330 62331 62335 62336] 

```



## Julia


```julia
using Primes

function nsmooth(N, needed)
    nexts, smooths = [BigInt(i) for i in 2:N if isprime(i)], [BigInt(1)]
    prim, count = deepcopy(nexts), 1
    indices = ones(Int, length(nexts))
    while count < needed
        x = minimum(nexts)
        push!(smooths, x)
        count += 1
        for j in 1:length(nexts)
            (nexts[j] <= x) && (nexts[j] = prim[j] * smooths[(indices[j] += 1)])
        end
    end
    return (smooths[end] > typemax(Int)) ? smooths : Int.(smooths)
end

function testnsmoothfilters()
    for i in filter(isprime, 1:29)
        println("The first 25 n-smooth numbers for n = $i are: ", nsmooth(i, 25))
    end
    for i in filter(isprime, 3:29)
        println("The 3000th through 3002nd ($i)-smooth numbers are: ", nsmooth(i, 3002)[3000:3002])
    end
    for i in filter(isprime, 503:521)
        println("The 30000th through 30019th ($i)-smooth numbers >= 30000 are: ", nsmooth(i, 30019)[30000:30019])
    end
end

testnsmoothfilters()

```
{{out}}

```txt

The first 25 n-smooth numbers for n = 2 are: [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608, 16777216]
The first 25 n-smooth numbers for n = 3 are: [1, 2, 3, 4, 6, 8, 9, 12, 16, 18, 24, 27, 32, 36, 48, 54, 64, 72, 81, 96, 108, 128, 144, 162, 192]
The first 25 n-smooth numbers for n = 5 are: [1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, 40, 45, 48, 50, 54]
The first 25 n-smooth numbers for n = 7 are: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 15, 16, 18, 20, 21, 24, 25, 27, 28, 30, 32, 35, 36]
The first 25 n-smooth numbers for n = 11 are: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 18, 20, 21, 22, 24, 25, 27, 28, 30, 32]
The first 25 n-smooth numbers for n = 13 are: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 20, 21, 22, 24, 25, 26, 27, 28]
The first 25 n-smooth numbers for n = 17 are: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20, 21, 22, 24, 25, 26, 27]
The first 25 n-smooth numbers for n = 19 are: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 24, 25, 26]
The first 25 n-smooth numbers for n = 23 are: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]
The first 25 n-smooth numbers for n = 29 are: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]
The 3000th through 3002nd (3)-smooth numbers are: BigInt[91580367978306252441724649472, 92829823186414819915547541504, 94096325042746502515294076928]
The 3000th through 3002nd (5)-smooth numbers are: [278942752080, 279936000000, 281250000000]
The 3000th through 3002nd (7)-smooth numbers are: [50176000, 50331648, 50388480]
The 3000th through 3002nd (11)-smooth numbers are: [2112880, 2116800, 2117016]
The 3000th through 3002nd (13)-smooth numbers are: [390000, 390390, 390625]
The 3000th through 3002nd (17)-smooth numbers are: [145800, 145860, 146016]
The 3000th through 3002nd (19)-smooth numbers are: [74256, 74358, 74360]
The 3000th through 3002nd (23)-smooth numbers are: [46552, 46575, 46585]
The 3000th through 3002nd (29)-smooth numbers are: [33516, 33524, 33534]
The 30000th through 30019th (503)-smooth numbers >= 30000 are: [62913, 62914, 62916, 62918, 62920, 62923, 62926, 62928, 62930, 62933, 62935, 62937, 62944, 62946, 62951, 62952, 62953, 62957, 62959, 62964]
The 30000th through 30019th (509)-smooth numbers >= 30000 are: [62601, 62602, 62604, 62607, 62608, 62609, 62611, 62618, 62620, 62622, 62624, 62625, 62626, 62628, 62629, 62634, 62640, 62643, 62645, 62646]
The 30000th through 30019th (521)-smooth numbers >= 30000 are: [62287, 62288, 62291, 62292, 62300, 62304, 62307, 62308, 62310, 62315, 62320, 62321, 62322, 62325, 62328, 62329, 62330, 62331, 62335, 62336]

```



## Pascal


{{incorrect|Pascal| 

 for the 2<sup>nd</sup> part of the task, 
 starting at three thousand, 
 '''n'''-smooth for '''3''' and '''5''' aren't displayed. 

}}

{{works with|Free Pascal}} 64-Bit.
Using trail-division with the first primes.Takes too long for first 3 after 2999  2,3,5-smooth numbers.

```Pascal
program HammNumb;
{$IFDEF FPC}
  {$MODE DELPHI}
  {$OPTIMIZATION ON,ALL}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}
type
  tHamNum = record
               hampot : array[0..167] of Word;
               hampotmax,
               hamNum : NativeUint;
             end;

const
  primes : array[0..167] of word =
           (2, 3, 5, 7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71
           ,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151
           ,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233
           ,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317
           ,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419
           ,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503
           ,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607
           ,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701
           ,709,719,727,733,739,743,751,757,761,769,773,787,797,809,811
           ,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911
           ,919,929,937,941,947,953,967,971,977,983,991,997);

var
  HNum:tHamNum;

procedure OutHamNum(const HNum:tHamNum);
var
  i : NativeInt;
Begin
  with Hnum do
  Begin
    write(hamNum:12,' : ');
    For i := 0 to hampotmax-1 do
      write(primes[i],'^',hampot[i],'*');
    writeln(primes[hampotmax],'^',hampot[hampotmax]);
  end;
end;

procedure NextHammNum(var HNum:tHamNum;maxP:NativeInt);
var
  q,p,nr,n,pnum,momPrime : NativeUInt;
begin
  n := HNum.hamNum;
  repeat
    inc(n);
    nr := n;
    //check divisibility by first (count=maxP) primes
    pnum := 0;
    repeat
      momPrime := primes[pnum];
      q := nr div momPrime;
      p := 0;
      while q*momPrime=nr do
      Begin
        inc(p);
        nr := q;
        q := nr div momPrime;
      end;
      HNum.hampot[pnum] := p;
      inc(pnum);
    until (nr=1) OR (pnum > maxp)
    //finished ?
  until nr = 1;

  With HNum do
  Begin
    hamNum := n;
    hamPotmax := pnum-1;
  end;
end;

procedure OutXafterYSmooth(X,Y,SmoothIdx: NativeUInt);
var
  i: NativeUint;
begin
  IF SmoothIdx> High(primes) then
    EXIT;
  HNum.HamNum := 0;
  dec(Y);
  for i := 1 to Y do
    NextHammNum(HNum,SmoothIdx);
  write('first ',X,' after ',Y,' ',primes[SmoothIdx]:3,'-smooth numbers : ');
  for i := 1 to X do
  begin
    NextHammNum(HNum,SmoothIdx);
    write(HNum.HamNum,' ');
  end;
  writeln;
end;

var
  j: NativeUint;
Begin
  j := 0;
  while primes[j] <= 29 do
  Begin
    OutXafterYSmooth(25,1,j);
    inc(j);
  end;
  writeln;

  j := 3;
  while primes[j] <= 29 do
  Begin
    OutXafterYSmooth(3,3000,j);
    inc(j);
  end;
  writeln;
  while primes[j] < 503 do
    inc(j);
  while primes[j] <= 521 do
  Begin
    OutXafterYSmooth(20,30000,j);
    inc(j);
  end;
  writeln;
End.
```
{{out}}
```txt
first 25 after 0   2-smooth numbers : 1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536 131072 262144 524288 1048576 2097152 4194304 8388608 16777216
first 25 after 0   3-smooth numbers : 1 2 3 4 6 8 9 12 16 18 24 27 32 36 48 54 64 72 81 96 108 128 144 162 192
first 25 after 0   5-smooth numbers : 1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 40 45 48 50 54
first 25 after 0   7-smooth numbers : 1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36
first 25 after 0  11-smooth numbers : 1 2 3 4 5 6 7 8 9 10 11 12 14 15 16 18 20 21 22 24 25 27 28 30 32
first 25 after 0  13-smooth numbers : 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 20 21 22 24 25 26 27 28
first 25 after 0  17-smooth numbers : 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 20 21 22 24 25 26 27
first 25 after 0  19-smooth numbers : 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 24 25 26
first 25 after 0  23-smooth numbers : 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
first 25 after 0  29-smooth numbers : 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25

first 3 after 2999   7-smooth numbers : 50176000 50331648 50388480
first 3 after 2999  11-smooth numbers : 2112880 2116800 2117016
first 3 after 2999  13-smooth numbers : 390000 390390 390625
first 3 after 2999  17-smooth numbers : 145800 145860 146016
first 3 after 2999  19-smooth numbers : 74256 74358 74360
first 3 after 2999  23-smooth numbers : 46552 46575 46585
first 3 after 2999  29-smooth numbers : 33516 33524 33534

first 20 after 29999 503-smooth numbers : 62913 62914 62916 62918 62920 62923 62926 62928 62930 62933 62935 62937 62944 62946 62951 62952 62953 62957 62959 62964
first 20 after 29999 509-smooth numbers : 62601 62602 62604 62607 62608 62609 62611 62618 62620 62622 62624 62625 62626 62628 62629 62634 62640 62643 62645 62646
first 20 after 29999 521-smooth numbers : 62287 62288 62291 62292 62300 62304 62307 62308 62310 62315 62320 62321 62322 62325 62328 62329 62330 62331 62335 62336

real    0m2,665s
user    0m2,655s
sys 0m0,003s

```



## Perl

{{libheader|ntheory}}

```perl
use strict;
use warnings;
use feature 'say';
use ntheory qw<primes>;
use List::Util qw<min>;

#use bigint     # works, but slow
use Math::GMPz; # this module gives roughly 16x speed-up

sub smooth_numbers {
#    my(@m) = @_;                               # use with 'bigint'
    my @m = map { Math::GMPz->new($_) } @_;     # comment out to NOT use Math::GMPz
    my @s;
    push @s, [1] for 0..$#m;

    return sub {
    my $n = $s[0][0];
    $n = min $n, $s[$_][0] for 1..$#m;
    for (0..$#m) {
            shift @{$s[$_]} if $s[$_][0] == $n;
            push @{$s[$_]}, $n * $m[$_]
        }
        return $n
    }
}

sub abbrev {
    my($n) = @_;
    return $n if length($n) <= 50;
    substr($n,0,10) . "...(@{[length($n) - 2*10]} digits omitted)..." . substr($n, -10, 10)
}

my @primes = @{primes(10_000)};

my $start = 3000; my $cnt = 3;
for my $n_smooth (0..9) {
    say "\nFirst 25, and ${start}th through @{[$start+2]}nd $primes[$n_smooth]-smooth numbers:";
    my $s = smooth_numbers(@primes[0..$n_smooth]);
    my @S25;
    push @S25, $s->() for 1..25;
    say join ' ', @S25;

    my @Sm; my $c = 25;
    do {
        my $sn = $s->();
        push @Sm, abbrev($sn) if ++$c >= $start;
    } until @Sm == $cnt;
    say join ' ', @Sm;
}

$start = 30000; $cnt = 20;
for my $n_smooth (95..97) { # (503, 509, 521) {
    say "\n${start}th through @{[$start+$cnt-1]}th $primes[$n_smooth]-smooth numbers:";
    my $s = smooth_numbers(@primes[0..$n_smooth]);
    my(@Sm,$c);
    do {
        my $sn = $s->();
        push @Sm, $sn if ++$c >= $start;
    } until @Sm == $cnt;
    say join ' ', @Sm;
}
```


{{out}}

```txt
First 25, and 3000th through 3002nd 2-smooth numbers:
1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536 131072 262144 524288 1048576 2097152 4194304 8388608 16777216
6151159610...(883 digits omitted)...9114994688 1230231922...(884 digits omitted)...8229989376 2460463844...(884 digits omitted)...6459978752

First 25, and 3000th through 3002nd 3-smooth numbers:
1 2 3 4 6 8 9 12 16 18 24 27 32 36 48 54 64 72 81 96 108 128 144 162 192
91580367978306252441724649472 92829823186414819915547541504 94096325042746502515294076928

First 25, and 3000th through 3002nd 5-smooth numbers:
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 40 45 48 50 54
278942752080 279936000000 281250000000

First 25, and 3000th through 3002nd 7-smooth numbers:
1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36
50176000 50331648 50388480

First 25, and 3000th through 3002nd 11-smooth numbers:
1 2 3 4 5 6 7 8 9 10 11 12 14 15 16 18 20 21 22 24 25 27 28 30 32
2112880 2116800 2117016

First 25, and 3000th through 3002nd 13-smooth numbers:
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 20 21 22 24 25 26 27 28
390000 390390 390625

First 25, and 3000th through 3002nd 17-smooth numbers:
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 20 21 22 24 25 26 27
145800 145860 146016

First 25, and 3000th through 3002nd 19-smooth numbers:
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 24 25 26
74256 74358 74360

First 25, and 3000th through 3002nd 23-smooth numbers:
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
46552 46575 46585

First 25, and 3000th through 3002nd 29-smooth numbers:
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
33516 33524 33534

30000th through 30019th 503-smooth numbers:
62913 62914 62916 62918 62920 62923 62926 62928 62930 62933 62935 62937 62944 62946 62951 62952 62953 62957 62959 62964

30000th through 30019th 509-smooth numbers:
62601 62602 62604 62607 62608 62609 62611 62618 62620 62622 62624 62625 62626 62628 62629 62634 62640 62643 62645 62646

30000th through 30019th 521-smooth numbers:
62287 62288 62291 62292 62300 62304 62307 62308 62310 62315 62320 62321 62322 62325 62328 62329 62330 62331 62335 62336
```



## Perl 6

{{works with|Rakudo|2019.07.1}}


```perl6
sub smooth-numbers (*@list) {
    cache my \Smooth := gather {
        my %i = (flat @list) Z=> (Smooth.iterator for ^@list);
        my %n = (flat @list) Z=> 1 xx *;
 
        loop {
            take my $n := %n{*}.min;
 
            for @list -> \k {
                %n{k} = %i{k}.pull-one * k if %n{k} == $n;
            }
        }
    }
}

sub abbrev ($n) {
   $n.chars > 50 ??
   $n.substr(0,10) ~ "...({$n.chars - 20} digits omitted)..." ~ $n.substr(* - 10) !!
   $n
}
 
my @primes = (2..*).grep: *.is-prime;
 
my $start = 3000;
 
for ^@primes.first( * > 29, :k ) -> $p {
    put join "\n", "\nFirst 25, and {$start}th through {$start+2}nd {@primes[$p]}-smooth numbers:",
    $(smooth-numbers(|@primes[0..$p])[^25]),
    $(smooth-numbers(|@primes[0..$p])[$start - 1 .. $start + 1]».&abbrev);
}
 
$start = 30000;
 
for 503, 509, 521 -> $p {
    my $i = @primes.first( * == $p, :k );
    put "\n{$start}th through {$start+19}th {@primes[$i]}-smooth numbers:\n" ~
    smooth-numbers(|@primes[0..$i])[$start - 1 .. $start + 18];
}
```


{{out}}

```txt
First 25, and 3000th through 3002nd 2-smooth numbers:
1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536 131072 262144 524288 1048576 2097152 4194304 8388608 16777216
6151159610...(883 digits omitted)...9114994688 1230231922...(884 digits omitted)...8229989376 2460463844...(884 digits omitted)...6459978752

First 25, and 3000th through 3002nd 3-smooth numbers:
1 2 3 4 6 8 9 12 16 18 24 27 32 36 48 54 64 72 81 96 108 128 144 162 192
91580367978306252441724649472 92829823186414819915547541504 94096325042746502515294076928

First 25, and 3000th through 3002nd 5-smooth numbers:
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 40 45 48 50 54
278942752080 279936000000 281250000000

First 25, and 3000th through 3002nd 7-smooth numbers:
1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36
50176000 50331648 50388480

First 25, and 3000th through 3002nd 11-smooth numbers:
1 2 3 4 5 6 7 8 9 10 11 12 14 15 16 18 20 21 22 24 25 27 28 30 32
2112880 2116800 2117016

First 25, and 3000th through 3002nd 13-smooth numbers:
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 20 21 22 24 25 26 27 28
390000 390390 390625

First 25, and 3000th through 3002nd 17-smooth numbers:
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 20 21 22 24 25 26 27
145800 145860 146016

First 25, and 3000th through 3002nd 19-smooth numbers:
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 24 25 26
74256 74358 74360

First 25, and 3000th through 3002nd 23-smooth numbers:
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
46552 46575 46585

First 25, and 3000th through 3002nd 29-smooth numbers:
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
33516 33524 33534

30000th through 30019th 503-smooth numbers:
62913 62914 62916 62918 62920 62923 62926 62928 62930 62933 62935 62937 62944 62946 62951 62952 62953 62957 62959 62964

30000th through 30019th 509-smooth numbers:
62601 62602 62604 62607 62608 62609 62611 62618 62620 62622 62624 62625 62626 62628 62629 62634 62640 62643 62645 62646

30000th through 30019th 521-smooth numbers:
62287 62288 62291 62292 62300 62304 62307 62308 62310 62315 62320 62321 62322 62325 62328 62329 62330 62331 62335 62336
```



## Phix

{{libheader|mpfr}}
{{trans|Julia}}

```Phix
include mpfr.e
 
function nsmooth(integer n, integer needed)
-- note that n is a prime index, ie 1,2,3,4... for 2,3,5,7...
    sequence smooth = {mpz_init(1)},
             nexts = get_primes(-n),
             indices = repeat(1,n)
    for i=1 to n do nexts[i] = mpz_init(nexts[i]) end for
    for i=2 to needed do
        mpz x = mpz_init_set(mpz_min(nexts))
        smooth = append(smooth,x)
        for j=1 to n do
            if mpz_cmp(nexts[j],x)<=0 then
                indices[j] += 1
                mpz_mul_si(nexts[j],smooth[indices[j]],get_prime(j))
            end if
        end for
    end for
    return smooth
end function
 
function flat_str(sequence s)
    for i=1 to length(s) do s[i] = shorten(mpz_get_str(s[i]),ml:=10) end for
    return join(s," ")
end function

for n=1 to 10 do
    printf(1,"%d-smooth[1..25]: %s\n",{get_prime(n),flat_str(nsmooth(n, 25))})
end for
for n=1 to 10 do
    printf(1,"%d-smooth[3000..3002]: %s\n",{get_prime(n),flat_str(nsmooth(n, 3002)[3000..3002])})
end for
for n=96 to 98 do   -- primes 503, 509, and 521
    printf(1,"%d-smooth[30000..30019]: %s\n",{get_prime(n),flat_str(nsmooth(n, 30019)[30000..30019])})
end for
```

{{out}}

```txt

2-smooth[1..25]: 1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536 131072 262144 524288 1048576 2097152 4194304 8388608 16777216
3-smooth[1..25]: 1 2 3 4 6 8 9 12 16 18 24 27 32 36 48 54 64 72 81 96 108 128 144 162 192
5-smooth[1..25]: 1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 40 45 48 50 54
7-smooth[1..25]: 1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36
11-smooth[1..25]: 1 2 3 4 5 6 7 8 9 10 11 12 14 15 16 18 20 21 22 24 25 27 28 30 32
13-smooth[1..25]: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 20 21 22 24 25 26 27 28
17-smooth[1..25]: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 20 21 22 24 25 26 27
19-smooth[1..25]: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 24 25 26
23-smooth[1..25]: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
29-smooth[1..25]: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
2-smooth[3000..3002]: 615115961...114994688 (903 digits) 123023192...229989376 (904 digits) 246046384...459978752 (904 digits)
3-smooth[3000..3002]: 91580367978306252441724649472 92829823186414819915547541504 94096325042746502515294076928
5-smooth[3000..3002]: 278942752080 279936000000 281250000000
7-smooth[3000..3002]: 50176000 50331648 50388480
11-smooth[3000..3002]: 2112880 2116800 2117016
13-smooth[3000..3002]: 390000 390390 390625
17-smooth[3000..3002]: 145800 145860 146016
19-smooth[3000..3002]: 74256 74358 74360
23-smooth[3000..3002]: 46552 46575 46585
29-smooth[3000..3002]: 33516 33524 33534
503-smooth[30000..30019]: 62913 62914 62916 62918 62920 62923 62926 62928 62930 62933 62935 62937 62944 62946 62951 62952 62953 62957 62959 62964
509-smooth[30000..30019]: 62601 62602 62604 62607 62608 62609 62611 62618 62620 62622 62624 62625 62626 62628 62629 62634 62640 62643 62645 62646
521-smooth[30000..30019]: 62287 62288 62291 62292 62300 62304 62307 62308 62310 62315 62320 62321 62322 62325 62328 62329 62330 62331 62335 62336

```



## REXX


```rexx
/*REXX pgm computes&displays X n-smooth numbers; both X and N can be specified as ranges*/
numeric digits 200                               /*be able to handle some big numbers.  */
parse arg LOx HIx LOn HIn .                      /*obtain optional arguments from the CL*/
if LOx=='' | LOx==","  then LOx=  1              /*Not specified?  Then use the default.*/
if HIx=='' | HIx==","  then HIx= LOx + 24        /* "      "         "   "   "     "    */
if LOn=='' | LOn==","  then LOn=  2              /* "      "         "   "   "     "    */
if HIn=='' | HIn==","  then HIn= LOn + 27        /* "      "         "   "   "     "    */
call genP HIn                                    /*generate enough primes to satisfy HIn*/
@aList= ' a list of the ';              @thru= ' through '  /*literals used with a  SAY.*/

     do j=LOn  to  HIn;  if !.j==0  then iterate /*if not prime, then skip this number. */
     call smooth HIx,j;                 $=       /*invoke SMOOTH; initialize $  (list). */
                     do k=LOx  to HIx;  $= $ #.k /*append a  smooth number to  "  "   " */
                     end   /*k*/
     say center(@aList  th(LOx)  @thru  th(HIx)     ' numbers for' j"-smooth ",  130, "═")
     say strip($);                      say
     end   /*j*/                                 /* [↑]  the $ list has a leading blank.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
genP: procedure expose @. !. #; parse arg x      /*#≡num of primes; @. ≡array of primes.*/
      @.=;      @.1=2; @.2=3; @.3=5; @.4=7; @.5=11; @.6=13; @.7=17; @.8=19; @.9=23;    #=9
      !.=0;     !.2=1; !.3=2; !.5=3; !.7=4; !.11=5; !.13=6; !.17=7; !.19=8; !.23=9
           do k=@.#+6  by 2  until #>=x ;        if k//3==0    then iterate
           parse var  k  ''  -1  _;              if _==5       then iterate
                        do d=4  until @.d**2>k;  if k//@.d==0  then iterate k
                        end   /*d*/
           #= # + 1;    !.k= #;       @.#= k     /*found a prime, bump counter; assign @*/
           end  /*k*/;                return
/*──────────────────────────────────────────────────────────────────────────────────────*/
smooth: procedure expose @. !. #.; parse arg y,p /*obtain the arguments from the invoker*/
        if p==''  then p= 3                      /*Not specified? Then assume Hamming #s*/
        n= !.p                                   /*the number of primes being used.     */
        nn= n - 1;            #.=  0;    #.1= 1  /*an array of n-smooth numbers (so far)*/
        f.=  1                                   /*the indices of factors of a number.  */
                do j=2  for y-1;              _= f.1
                z= @.1 * #._
                             do k=2  for nn;  _= f.k;  v= @.k * #._;    if v<z  then z= v
                             end   /*k*/
                #.j= z
                             do d=1  for n;   _= f.d;  if @.d * #._==z  then f.d= f.d + 1
                             end   /*d*/
                end   /*j*/;                  return
/*──────────────────────────────────────────────────────────────────────────────────────*/
th: parse arg th; return th || word('th st nd rd', 1+(th//10)*(th//100%10\==1)*(th//10<4))
```

{{out|output|text=  when using the default inputs:}}

```txt

════════════════════════════════════ a list of the  1st  through  25th  numbers for 2-smooth ═════════════════════════════════════
1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536 131072 262144 524288 1048576 2097152 4194304 8388608 16777216

════════════════════════════════════ a list of the  1st  through  25th  numbers for 3-smooth ═════════════════════════════════════
1 2 3 4 6 8 9 12 16 18 24 27 32 36 48 54 64 72 81 96 108 128 144 162 192

════════════════════════════════════ a list of the  1st  through  25th  numbers for 5-smooth ═════════════════════════════════════
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 40 45 48 50 54

════════════════════════════════════ a list of the  1st  through  25th  numbers for 7-smooth ═════════════════════════════════════
1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36

════════════════════════════════════ a list of the  1st  through  25th  numbers for 11-smooth ════════════════════════════════════
1 2 3 4 5 6 7 8 9 10 11 12 14 15 16 18 20 21 22 24 25 27 28 30 32

════════════════════════════════════ a list of the  1st  through  25th  numbers for 13-smooth ════════════════════════════════════
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 20 21 22 24 25 26 27 28

════════════════════════════════════ a list of the  1st  through  25th  numbers for 17-smooth ════════════════════════════════════
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 20 21 22 24 25 26 27

════════════════════════════════════ a list of the  1st  through  25th  numbers for 19-smooth ════════════════════════════════════
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 24 25 26

════════════════════════════════════ a list of the  1st  through  25th  numbers for 23-smooth ════════════════════════════════════
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25

════════════════════════════════════ a list of the  1st  through  25th  numbers for 29-smooth ════════════════════════════════════
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25

```

{{out|output|text=  when using the input of:     <tt> 3000   3002   3   29 </tt>}}

```txt

══════════════════════════════════ a list of the  3000th  through  3002nd  numbers for 3-smooth ══════════════════════════════════
91580367978306252441724649472 92829823186414819915547541504 94096325042746502515294076928

══════════════════════════════════ a list of the  3000th  through  3002nd  numbers for 5-smooth ══════════════════════════════════
278942752080 279936000000 281250000000

══════════════════════════════════ a list of the  3000th  through  3002nd  numbers for 7-smooth ══════════════════════════════════
50176000 50331648 50388480

═════════════════════════════════ a list of the  3000th  through  3002nd  numbers for 11-smooth ══════════════════════════════════
2112880 2116800 2117016

═════════════════════════════════ a list of the  3000th  through  3002nd  numbers for 13-smooth ══════════════════════════════════
390000 390390 390625

═════════════════════════════════ a list of the  3000th  through  3002nd  numbers for 17-smooth ══════════════════════════════════
145800 145860 146016

═════════════════════════════════ a list of the  3000th  through  3002nd  numbers for 19-smooth ══════════════════════════════════
74256 74358 74360

═════════════════════════════════ a list of the  3000th  through  3002nd  numbers for 23-smooth ══════════════════════════════════
46552 46575 46585

═════════════════════════════════ a list of the  3000th  through  3002nd  numbers for 29-smooth ══════════════════════════════════
33516 33524 33534

```

{{out|output|text=  when using the input of:     <tt> 30000   30019   503   521 </tt>}}

```txt

════════════════════════════════ a list of the  30000th  through  30019th  numbers for 503-smooth ════════════════════════════════
62913 62914 62916 62918 62920 62923 62926 62928 62930 62933 62935 62937 62944 62946 62951 62952 62953 62957 62959 62964

════════════════════════════════ a list of the  30000th  through  30019th  numbers for 509-smooth ════════════════════════════════
62601 62602 62604 62607 62608 62609 62611 62618 62620 62622 62624 62625 62626 62628 62629 62634 62640 62643 62645 62646

════════════════════════════════ a list of the  30000th  through  30019th  numbers for 521-smooth ════════════════════════════════
62287 62288 62291 62292 62300 62304 62307 62308 62310 62315 62320 62321 62322 62325 62328 62329 62330 62331 62335 62336

```



## Sidef


```ruby
func smooth_generator(primes) {
    var s = primes.len.of { [1] }
    {
        var n = s.map { .first }.min
        { |i|
            s[i].shift if (s[i][0] == n)
            s[i] << (n * primes[i])
        } * primes.len
        n
    }
}

for p in (primes(2,29)) {
    var g = smooth_generator(p.primes)
    say ("First 25 #{'%2d'%p}-smooth numbers: ", 25.of { g.run }.join(' '))
}

say ''

for p in (primes(3,29)) {
    var g = smooth_generator(p.primes)
    say ("3,000th through 3,002nd #{'%2d'%p}-smooth numbers: ", 3002.of { g.run }.last(3).join(' '))
}
```

{{out}}

```txt

First 25  2-smooth numbers: 1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536 131072 262144 524288 1048576 2097152 4194304 8388608 16777216
First 25  3-smooth numbers: 1 2 3 4 6 8 9 12 16 18 24 27 32 36 48 54 64 72 81 96 108 128 144 162 192
First 25  5-smooth numbers: 1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 40 45 48 50 54
First 25  7-smooth numbers: 1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36
First 25 11-smooth numbers: 1 2 3 4 5 6 7 8 9 10 11 12 14 15 16 18 20 21 22 24 25 27 28 30 32
First 25 13-smooth numbers: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 20 21 22 24 25 26 27 28
First 25 17-smooth numbers: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 20 21 22 24 25 26 27
First 25 19-smooth numbers: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 24 25 26
First 25 23-smooth numbers: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
First 25 29-smooth numbers: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25

3,000th through 3,002nd  3-smooth numbers: 91580367978306252441724649472 92829823186414819915547541504 94096325042746502515294076928
3,000th through 3,002nd  5-smooth numbers: 278942752080 279936000000 281250000000
3,000th through 3,002nd  7-smooth numbers: 50176000 50331648 50388480
3,000th through 3,002nd 11-smooth numbers: 2112880 2116800 2117016
3,000th through 3,002nd 13-smooth numbers: 390000 390390 390625
3,000th through 3,002nd 17-smooth numbers: 145800 145860 146016
3,000th through 3,002nd 19-smooth numbers: 74256 74358 74360
3,000th through 3,002nd 23-smooth numbers: 46552 46575 46585
3,000th through 3,002nd 29-smooth numbers: 33516 33524 33534

```


Optionally, an efficient algorithm for checking if a given arbitrary large number is smooth over a given product of primes:

```ruby
func is_smooth_over_prod(n, k) {

    return true  if (n == 1)
    return false if (n <= 0)

    for (var g = gcd(n,k); g > 1; g = gcd(n,k)) {
        n /= g**valuation(n,g)        # remove any divisibility by g
        return true if (n == 1)       # smooth if n == 1
    }

    return false
}

for p in (503, 509, 521) {
    var k = p.primorial
    var a = {|n| is_smooth_over_prod(n, k) }.first(30_019).last(20)
    say ("30,000th through 30,019th #{p}-smooth numbers: ", a.join(' '))
}
```

{{out}}

```txt

30,000th through 30,019th 503-smooth numbers: 62913 62914 62916 62918 62920 62923 62926 62928 62930 62933 62935 62937 62944 62946 62951 62952 62953 62957 62959 62964
30,000th through 30,019th 509-smooth numbers: 62601 62602 62604 62607 62608 62609 62611 62618 62620 62622 62624 62625 62626 62628 62629 62634 62640 62643 62645 62646
30,000th through 30,019th 521-smooth numbers: 62287 62288 62291 62292 62300 62304 62307 62308 62310 62315 62320 62321 62322 62325 62328 62329 62330 62331 62335 62336

```


## zkl

{{trans|Go}}
{{libheader|GMP}} GNU Multiple Precision Arithmetic Library and primes

```zkl
var [const] BI=Import("zklBigNum");  // libGMP

fcn nSmooth(n,sz){	// --> List of big ints
   if(sz<1) throw(Exception.ValueError("size must be at least 1"));
   bn,primes,ns := BI(n), List(), List.createLong(sz);
   if(not bn.probablyPrime()) throw(Exception.ValueError("n must be prime"));
   p:=BI(1); while(p<n){ primes.append(p.nextPrime().copy()) }  // includes n
   ns.append(BI(1));
   next:=primes.copy();
   if(Void!=( z:=primes.find(bn)) ) next.del(z+1,*);

   indices:=List.createLong(next.len(),0);
   do(sz-1){
      ns.append( nm:=BI( next.reduce(fcn(a,b){ a.min(b) }) ));
      foreach i in (indices.len()){
         if(nm==next[i]){
	    indices[i]+=1;
	    next[i]=primes[i]*ns[indices[i]];
	 }
      }
   }
   ns
}
```


```zkl
smallPrimes:=List();
p:=BI(1); while(p<29) { smallPrimes.append(p.nextPrime().toInt()) }

foreach p in (smallPrimes){
   println("The first 25 %d-smooth numbers are:".fmt(p));
   println(nSmooth(p,25).concat(" "), "\n")
}
foreach p in (smallPrimes[1,*]){
   print("The 3,000th to 3,202nd %d-smooth numbers are: ".fmt(p));
   println(nSmooth(p,3002)[2999,*].concat(" "));
}
foreach p in (T(503,509,521)){
   println("\nThe 30,000th to 30,019th %d-smooth numbers are:".fmt(p));
   println(nSmooth(p,30019)[29999,*].concat(" "));
}
```

{{out}}
<pre style="height:45ex">
The first 25 2-smooth numbers are:
1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536 131072 262144 524288 1048576 2097152 4194304 8388608 16777216

The first 25 3-smooth numbers are:
1 2 3 4 6 8 9 12 16 18 24 27 32 36 48 54 64 72 81 96 108 128 144 162 192

The first 25 5-smooth numbers are:
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 40 45 48 50 54

The first 25 7-smooth numbers are:
1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36

The first 25 11-smooth numbers are:
1 2 3 4 5 6 7 8 9 10 11 12 14 15 16 18 20 21 22 24 25 27 28 30 32

The first 25 13-smooth numbers are:
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 20 21 22 24 25 26 27 28

The first 25 17-smooth numbers are:
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 20 21 22 24 25 26 27

The first 25 19-smooth numbers are:
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 24 25 26

The first 25 23-smooth numbers are:
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25

The first 25 29-smooth numbers are:
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25

The 3,000th to 3,202nd 3-smooth numbers are: 91580367978306252441724649472 92829823186414819915547541504 94096325042746502515294076928
The 3,000th to 3,202nd 5-smooth numbers are: 278942752080 279936000000 281250000000
The 3,000th to 3,202nd 7-smooth numbers are: 50176000 50331648 50388480
The 3,000th to 3,202nd 11-smooth numbers are: 2112880 2116800 2117016
The 3,000th to 3,202nd 13-smooth numbers are: 390000 390390 390625
The 3,000th to 3,202nd 17-smooth numbers are: 145800 145860 146016
The 3,000th to 3,202nd 19-smooth numbers are: 74256 74358 74360
The 3,000th to 3,202nd 23-smooth numbers are: 46552 46575 46585
The 3,000th to 3,202nd 29-smooth numbers are: 33516 33524 33534

The 30,000th to 30,019th 503-smooth numbers are:
62913 62914 62916 62918 62920 62923 62926 62928 62930 62933 62935 62937 62944 62946 62951 62952 62953 62957 62959 62964

The 30,000th to 30,019th 509-smooth numbers are:
62601 62602 62604 62607 62608 62609 62611 62618 62620 62622 62624 62625 62626 62628 62629 62634 62640 62643 62645 62646

The 30,000th to 30,019th 521-smooth numbers are:
62287 62288 62291 62292 62300 62304 62307 62308 62310 62315 62320 62321 62322 62325 62328 62329 62330 62331 62335 62336

```

