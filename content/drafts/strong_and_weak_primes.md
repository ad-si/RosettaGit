+++
title = "Strong and weak primes"
description = ""
date = 2019-09-12T17:50:49Z
aliases = []
[extra]
id = 22088
[taxonomies]
categories = []
tags = []
+++

{{task|Prime Numbers}}


;Definitions   (as per number theory):
::*   The   '''prime(''p'')'''   is the   ''' ''p''<sup>th</sup>   prime.
:::::*   '''prime(1)'''   is   '''2'''
:::::*   '''prime(4)'''   is   '''7'''
::*   A   '''     strong   prime'''   is when     <big>'''prime(''p'')'''</big>   is   <big>'''>'''   '''[prime(''p''-1) + prime(''p''+1)] ÷ 2</big>'''
::*   A   '''  weak   prime'''   is when     <big>'''prime(''p'')'''</big>   is   <big>'''<'''   '''[prime(''p''-1) + prime(''p''+1)] ÷ 2</big>'''



Note that the definition for    '''strong primes'''   is different when used in the context of   cryptography.


;Task:
::*   Find and display (on one line) the first   '''36'''   strong primes.
::*   Find and display the   ''count''   of the strong primes below   1,000,000.
::*   Find and display the   ''count''   of the strong primes below 10,000,000.
::*   Find and display (on one line) the first   '''37'''   weak primes.
::*   Find and display the   ''count''   of the weak primes below   1,000,000.
::*   Find and display the   ''count''   of the weak primes below 10,000,000.
::*   (Optional)   display the   ''counts''   and   "below numbers"   with commas.

Show all output here.


;Related Task:
::*   [[Safe primes and unsafe primes|safe and unsafe primes]].


;Also see:
::*   The OEIS article:   [http://oeis.org/A051634       strong   primes].
::*   The OEIS article:   [http://oeis.org/A051635   weak    primes].





## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}

```algol68
# find and count strong and weak primes                                       #
PR heap=128M PR # set heap memory size for Algol 68G                          #
# returns a string representation of n with commas                            #
PROC commatise = ( INT n )STRING:
     BEGIN
        STRING result      := "";
        STRING unformatted  = whole( n, 0 );
        INT    ch count    := 0;
        FOR c FROM UPB unformatted BY -1 TO LWB unformatted DO
            IF   ch count <= 2 THEN ch count +:= 1
            ELSE                    ch count  := 1; "," +=: result
            FI;
            unformatted[ c ] +=: result
        OD;
        result
     END # commatise # ;
# sieve values                                                                #
CHAR prime     = "P"; #  unclassified/average prime                           #
CHAR strong    = "S"; #                strong prime                           #
CHAR weak      = "W"; #                  weak prime                           #
CHAR composite = "C"; #                   non-prime                           #
# sieve of Eratosthenes: sets s[i] to prime if i is a prime,                  #
#                                     composite otherwise                     #
PROC sieve = ( REF[]CHAR s )VOID:
     BEGIN
        # start with everything flagged as prime                              #
        FOR i TO UPB s DO s[ i ] := prime OD;
        # sieve out the non-primes                                            #
        s[ 1 ] := composite;
        FOR i FROM 2 TO ENTIER sqrt( UPB s ) DO
            IF s[ i ] = prime THEN FOR p FROM i * i BY i TO UPB s DO s[ p ] := composite OD FI
        OD
     END # sieve # ;

INT max number = 10 000 000;
# construct a sieve of primes up to slightly more than the maximum number     #
# required for the task, as we may need an extra prime for the classification #
[ 1 : max number + 1 000 ]CHAR primes;
sieve( primes );
# classify the primes                                                         #
# find the first three primes                                                 #
INT prev prime := 0;
INT curr prime := 0;
INT next prime := 0;
FOR p FROM 2 WHILE prev prime = 0 DO
    IF primes[ p ] = prime THEN
        prev prime := curr prime;
        curr prime := next prime;
        next prime := p
    FI
OD;
# 2 is the only even prime so the first three primes are the only case where  #
# the average of prev prime and next prime is not an integer                  #
IF   REAL avg = ( prev prime + next prime ) / 2;
     curr prime > avg THEN primes[ curr prime ] := strong
ELIF curr prime < avg THEN primes[ curr prime ] := weak  
FI;
# classify the rest of the primes                                             #
FOR p FROM next prime + 1 WHILE curr prime <= max number DO
    IF primes[ p ] = prime THEN
        prev prime := curr prime;
        curr prime := next prime;
        next prime := p;
        IF   INT avg = ( prev prime + next prime ) OVER 2;
             curr prime > avg THEN primes[ curr prime ] := strong
        ELIF curr prime < avg THEN primes[ curr prime ] := weak  
        FI
    FI
OD;
INT strong1 := 0, strong10 := 0;
INT weak1   := 0, weak10   := 0;
FOR p WHILE p < 10 000 000 DO
    IF   primes[ p ] = strong THEN
        strong10 +:= 1;
        IF p < 1 000 000 THEN strong1 +:= 1 FI
    ELIF primes[ p ] = weak   THEN
        weak10   +:= 1;
        IF p < 1 000 000 THEN weak1   +:= 1 FI
    FI
OD;
INT strong count  := 0;
print( ( "first 36 strong primes:", newline ) );
FOR p WHILE strong count < 36 DO IF primes[ p ] = strong THEN print( ( " ", whole( p, 0 ) ) ); strong count +:= 1 FI OD;
print( ( newline ) );
print( ( "strong primes below   1,000,000: ", commatise(  strong1 ), newline ) );
print( ( "strong primes below  10,000,000: ", commatise( strong10 ), newline ) );
print( ( "first 37   weak primes:", newline ) );
INT weak count    := 0;
FOR p WHILE weak count   < 37 DO IF primes[ p ] = weak   THEN print( ( " ", whole( p, 0 ) ) );   weak count +:= 1 FI OD;
print( ( newline ) );
print( ( "  weak primes below   1,000,000: ", commatise(    weak1 ), newline ) );
print( ( "  weak primes below  10,000,000: ", commatise(   weak10 ), newline ) )
```

{{out}}

```txt

first 36 strong primes:
 11 17 29 37 41 59 67 71 79 97 101 107 127 137 149 163 179 191 197 223 227 239 251 269 277 281 307 311 331 347 367 379 397 419 431 439
strong primes below   1,000,000: 37,723
strong primes below  10,000,000: 320,991
first 37   weak primes:
 3 7 13 19 23 31 43 47 61 73 83 89 103 109 113 131 139 151 167 181 193 199 229 233 241 271 283 293 313 317 337 349 353 359 383 389 401
  weak primes below   1,000,000: 37,780
  weak primes below  10,000,000: 321,750

```



## C sharp

{{works with|C sharp|7}}

```csharp
using static System.Console;
using static System.Linq.Enumerable;
using System;

public static class StrongAndWeakPrimes
{
    public static void Main() {
        var primes = PrimeGenerator(10_000_100).ToList();
        var strongPrimes = from i in Range(1, primes.Count - 2) where primes[i] > (primes[i-1] + primes[i+1]) / 2 select primes[i];
        var weakPrimes = from i in Range(1, primes.Count - 2) where primes[i] < (primes[i-1] + primes[i+1]) / 2.0 select primes[i];
        WriteLine($"First 36 strong primes: {string.Join(", ", strongPrimes.Take(36))}");
        WriteLine($"There are {strongPrimes.TakeWhile(p => p < 1_000_000).Count():N0} strong primes below {1_000_000:N0}");
        WriteLine($"There are {strongPrimes.TakeWhile(p => p < 10_000_000).Count():N0} strong primes below {10_000_000:N0}");
        WriteLine($"First 37 weak primes: {string.Join(", ", weakPrimes.Take(37))}");
        WriteLine($"There are {weakPrimes.TakeWhile(p => p < 1_000_000).Count():N0} weak primes below {1_000_000:N0}");
        WriteLine($"There are {weakPrimes.TakeWhile(p => p < 10_000_000).Count():N0} weak primes below {1_000_000:N0}");
    }
   
}
```

{{out}}

```txt

First 36 strong primes: 11, 17, 29, 37, 41, 59, 67, 71, 79, 97, 101, 107, 127, 137, 149, 163, 179, 191, 197, 223, 227, 239, 251, 269, 277, 281, 307, 311, 331, 347, 367, 379, 397, 419, 431, 439
There are 37,723 strong primes below 1,000,000
There are 320,991 strong primes below 10,000,000
First 37 weak primes: 3, 7, 13, 19, 23, 31, 43, 47, 61, 73, 83, 89, 103, 109, 113, 131, 139, 151, 167, 181, 193, 199, 229, 233, 241, 271, 283, 293, 313, 317, 337, 349, 353, 359, 383, 389, 401
There are 37,780 weak primes below 1,000,000
There are 321,750 weak primes below 1,000,000
```



## Factor


```factor
USING: formatting grouping kernel math math.primes sequences
tools.memory.private ;
IN: rosetta-code.strong-primes

: fn ( p-1 p p+1 -- p sum ) rot + 2 / ;
: strong? ( p-1 p p+1 -- ? ) fn > ;
: weak? ( p-1 p p+1 -- ? ) fn < ;

: swprimes ( seq quot -- seq )
    [ 3 <clumps> ] dip [ first3 ] prepose filter [ second ] map
    ; inline

: stats ( seq n -- firstn count1 count2 )
    [ head ] [ drop [ 1e6 < ] filter length ] [ drop length ]
    2tri [ commas ] bi@ ;

10,000,019 primes-upto [ strong? ] over [ weak? ]
[ swprimes ] 2bi@ [ 36 ] [ 37 ] bi* [ stats ] 2bi@

"First 36 strong primes:\n%[%d, %]
%s strong primes below 1,000,000
%s strong primes below 10,000,000\n
First 37 weak primes:\n%[%d, %]
%s weak primes below 1,000,000
%s weak primes below 10,000,000\n" printf
```

{{out}}

```txt

First 36 strong primes:
{ 11, 17, 29, 37, 41, 59, 67, 71, 79, 97, 101, 107, 127, 137, 149, 163, 179, 191, 197, 223, 227, 239, 251, 269, 277, 281, 307, 311, 331, 347, 367, 379, 397, 419, 431, 439 }
37,723 strong primes below 1,000,000
320,991 strong primes below 10,000,000

First 37 weak primes:
{ 3, 7, 13, 19, 23, 31, 43, 47, 61, 73, 83, 89, 103, 109, 113, 131, 139, 151, 167, 181, 193, 199, 229, 233, 241, 271, 283, 293, 313, 317, 337, 349, 353, 359, 383, 389, 401 }
37,780 weak primes below 1,000,000
321,750 weak primes below 10,000,000

```



## Go


```go
package main

import "fmt"

func sieve(limit int) []bool {
    limit++
    // True denotes composite, false denotes prime.
    // Don't bother marking even numbers >= 4 as composite.
    c := make([]bool, limit)
    c[0] = true
    c[1] = true

    p := 3 // start from 3
    for {
        p2 := p * p
        if p2 >= limit {
            break
        }
        for i := p2; i < limit; i += 2 * p {
            c[i] = true
        }
        for {
            p += 2
            if !c[p] {
                break
            }
        }
    }
    return c
}

func commatize(n int) string {
    s := fmt.Sprintf("%d", n)
    le := len(s)
    for i := le - 3; i >= 1; i -= 3 {
        s = s[0:i] + "," + s[i:]
    }
    return s
}

func main() {
    // sieve up to 10,000,019 - the first prime after 10 million
    const limit = 1e7 + 19
    sieved := sieve(limit)
    // extract primes
    var primes = []int{2}
    for i := 3; i <= limit; i += 2 {
        if !sieved[i] {
            primes = append(primes, i)
        }
    }
    // extract strong and weak primes
    var strong []int
    var weak = []int{3}                  // so can use integer division for rest
    for i := 2; i < len(primes)-1; i++ { // start from 5
        if primes[i] > (primes[i-1]+primes[i+1])/2 {
            strong = append(strong, primes[i])
        } else if primes[i] < (primes[i-1]+primes[i+1])/2 {
            weak = append(weak, primes[i])
        }
    }

    fmt.Println("The first 36 strong primes are:")
    fmt.Println(strong[:36])
    count := 0
    for _, p := range strong {
        if p >= 1e6 {
            break
        }
        count++
    }
    fmt.Println("\nThe number of strong primes below 1,000,000 is", commatize(count))
    fmt.Println("\nThe number of strong primes below 10,000,000 is", commatize(len(strong)))

    fmt.Println("\nThe first 37 weak primes are:")
    fmt.Println(weak[:37])
    count = 0
    for _, p := range weak {
        if p >= 1e6 {
            break
        }
        count++
    }
    fmt.Println("\nThe number of weak primes below 1,000,000 is", commatize(count))
    fmt.Println("\nThe number of weak primes below 10,000,000 is", commatize(len(weak)))
}
```


{{out}}

```txt

The first 36 strong primes are:
[11 17 29 37 41 59 67 71 79 97 101 107 127 137 149 163 179 191 197 223 227 239 251 269 277 281 307 311 331 347 367 379 397 419 431 439]

The number of strong primes below 1,000,000 is 37,723

The number of strong primes below 10,000,000 is 320,991

The first 37 weak primes are:
[3 7 13 19 23 31 43 47 61 73 83 89 103 109 113 131 139 151 167 181 193 199 229 233 241 271 283 293 313 317 337 349 353 359 383 389 401]

The number of weak primes below 1,000,000 is 37,780

The number of weak primes below 10,000,000 is 321,750

```



## J


```txt

   Filter =: (#~`)(`:6)
   average =: +/ % #


   NB. vector of primes from 2 to 10000019
   PRIMES=:i.@>:&.(p:inv) 10000000


   strongQ =: 1&{ > [: average {. , {:
   STRONG_PRIMES=: (0, 0,~ 3&(strongQ\))Filter PRIMES
   NB. first 36 strong primes
   36 {. STRONG_PRIMES
11 17 29 37 41 59 67 71 79 97 101 107 127 137 149 163 179 191 197 223 227 239 251 269 277 281 307 311 331 347 367 379 397 419 431 439
   NB. tally of strong primes less than one and ten million
   +/ STRONG_PRIMES </ 1e6 * 1 10
37723 320991
   

   weakQ =: 1&{ < [: average {. , {:
   weaklings =: (0, 0,~ 3&(weakQ\))Filter PRIMES
   NB. first 37 weak primes   
   37 {. weaklings
3 7 13 19 23 31 43 47 61 73 83 89 103 109 113 131 139 151 167 181 193 199 229 233 241 271 283 293 313 317 337 349 353 359 383 389 401
   NB. tally of weak primes less than one and ten million
   +/ weaklings </ 1e6 * 1 10
37780 321750

```



## Julia


```julia
using Primes, Formatting

function parseprimelist()
    primelist = primes(2, 10000019)
    strongs = Vector{Int64}()
    weaks = Vector{Int64}()
    balanceds = Vector{Int64}()
    for (n, p) in enumerate(primelist)
        if n == 1 || n == length(primelist)
            continue
        end
        x = (primelist[n - 1] + primelist[n + 1]) / 2
        if x > p
            push!(weaks, p)
        elseif x < p 
            push!(strongs, p)
        else
            push!(balanceds, p)
        end
    end
    println("The first 36 strong primes are: ", strongs[1:36])
    println("There are ", format(sum(map(x -> x < 1000000, strongs)), commas=true), " stromg primes less than 1 million.")
    println("There are ", format(length(strongs), commas=true), " strong primes less than 10 million.")    
    println("The first 37 weak primes are: ", weaks[1:37])
    println("There are ", format(sum(map(x -> x < 1000000, weaks)), commas=true), " weak primes less than 1 million.")
    println("There are ", format(length(weaks), commas=true), " weak primes less than 10 million.")    
    println("The first 28 balanced primes are: ", balanceds[1:28])
    println("There are ", format(sum(map(x -> x < 1000000, balanceds)), commas=true), " balanced primes less than 1 million.")
    println("There are ", format(length(balanceds), commas=true), " balanced primes less than 10 million.")    
end

parseprimelist()

```
 {{output}} 
```txt

The first 36 strong primes are: [11, 17, 29, 37, 41, 59, 67, 71, 79, 97, 101, 107, 127, 137, 149, 163, 179, 191, 197, 223, 227, 239, 251, 269, 277, 281, 307, 311, 331, 347, 367, 379, 397, 419, 431, 439]
There are 37,723 stromg primes less than 1 million.
There are 320,991 strong primes less than 10 million.
The first 37 weak primes are: [3, 7, 13, 19, 23, 31, 43, 47, 61, 73, 83, 89, 103, 109, 113, 131, 139, 151, 167, 181, 193, 199, 229, 233, 241, 271, 283, 293, 313, 317, 337, 349, 353, 359, 383, 389, 401]
There are 37,780 weak primes less than 1 million.
There are 321,750 weak primes less than 10 million.
The first 28 balanced primes are: [5, 53, 157, 173, 211, 257, 263, 373, 563, 593, 607, 653, 733, 947, 977, 1103, 1123, 1187, 1223, 1367, 1511, 1747, 1753, 1907, 2287, 2417, 2677, 2903]
There are 2,994 balanced primes less than 1 million.
There are 21,837 balanced primes less than 10 million.

```



## Lua

This could be made faster but favours readability. It runs in about 3.3 seconds in LuaJIT on a 2.8 GHz core.

```lua
-- Return a table of the primes up to n, then one more
function primeList (n)
  local function isPrime (x)
    for d = 3, math.sqrt(x), 2 do
      if x % d == 0 then return false end
    end
    return true
  end
  local pTable, j = {2, 3}
  for i = 5, n, 2 do
    if isPrime(i) then
      table.insert(pTable, i)
    end
    j = i
  end
  repeat j = j + 2 until isPrime(j)
  table.insert(pTable, j)
  return pTable
end

-- Return a boolean indicating whether prime p is strong
function isStrong (p)
  if p == 1 or p == #prime then return false end
  return prime[p] > (prime[p-1] + prime[p+1]) / 2 
end

-- Return a boolean indicating whether prime p is weak
function isWeak (p)
  if p == 1 or p == #prime then return false end
  return prime[p] < (prime[p-1] + prime[p+1]) / 2 
end

-- Main procedure
prime = primeList(1e7)
local strong, weak, sCount, wCount = {}, {}, 0, 0
for k, v in pairs(prime) do
  if isStrong(k) then
    table.insert(strong, v)
    if v < 1e6 then sCount = sCount + 1 end
  end
  if isWeak(k) then
    table.insert(weak, v)
    if v < 1e6 then wCount = wCount + 1 end
  end
end
print("The first 36 strong primes are:")
for i = 1, 36 do io.write(strong[i] .. " ") end
print("\n\nThere are " .. sCount .. " strong primes below one million.")
print("\nThere are " .. #strong .. " strong primes below ten million.")
print("\nThe first 37 weak primes are:")
for i = 1, 37 do io.write(weak[i] .. " ") end
print("\n\nThere are " .. wCount .. " weak primes below one million.")
print("\nThere are " .. #weak .. " weak primes below ten million.")
```

{{out}}

```txt
The first 36 strong primes are:
11 17 29 37 41 59 67 71 79 97 101 107 127 137 149 163 179 191 197 223 227 239 251 269 277 281 307 311 331 347 367 379 397 419 431 439

There are 37723 strong primes below one million.

There are 320991 strong primes below ten million.

The first 37 weak primes are:
3 7 13 19 23 31 43 47 61 73 83 89 103 109 113 131 139 151 167 181 193 199 229 233 241 271 283 293 313 317 337 349 353 359 383 389 401

There are 37780 weak primes below one million.

There are 321750 weak primes below ten million.

```


## Maple


```maple
isStrong := proc(n::posint) local holder; 
holder := false; 
if isprime(n) and 1/2*prevprime(n) + 1/2*nextprime(n) < n then 
   holder := true; 
end if; 
return holder; 
end proc:

isWeak := proc(n::posint) local holder; 
holder := false; 
if isprime(n) and n < 1/2*prevprime(n) + 1/2*nextprime(n) then 
   holder := true; 
end if; 
return holder; 
end proc

findStrong := proc(n::posint) local count, list, k; 
count := 0; list := []; 
for k from 3 while count < n do 
  if isStrong(k) then count := count + 1; 
    list := [op(list), k]; 
  end if; 
end do; 
return list; 
end proc:

findWeak := proc(n::posint) local count, list, k; 
count := 0; 
list := []; 
for k from 3 while count < n do 
  if isWeak(k) then 
     count := count + 1; 
     list := [op(list), k]; 
  end if; 
end do; 
return list; 
end proc:

findStrong(36)
findWeak(37)
countStrong(1000000)
countStrong(10000000)
countWeak(1000000)
countWeak(10000000)
```

{{Out}}  

```txt
[11, 17, 29, 37, 41, 59, 67, 71, 79, 97, 101, 107, 127, 137, 149, 163, 179, 191, 197, 223, 227, 239, 251, 269, 277, 281, 307, 311, 331, 347, 367, 379, 397, 419, 431, 439]
[3, 7, 13, 19, 23, 31, 43, 47, 61, 73, 83, 89, 103, 109, 113, 131, 139, 151, 167, 181, 193, 199, 229, 233, 241, 271, 283, 293, 313, 317, 337, 349, 353, 359, 383, 389, 401]
37723
320991
37780
321750
```



## Pascal

Converting the primes into deltaPrime, so that its easy to check the strong- /weakness.
Startprime 2 +1 -> (3)+2-> (5)+2 ->(7) +4-> (11)+2 .... 1,2,2,4,2,4,2,4,6,2,....
By using only odd primes startprime is 3 and delta -> delta/2
 
If deltaAfter < deltaBefore than a strong prime is found.

```pascal
program WeakPrim;
{$IFNDEF FPC}
  {$AppType CONSOLE}
{$ENDIF}
const
  PrimeLimit = 1000*1000*1000;//must be >= 2*3;
type
  tLimit = 0..(PrimeLimit-1) DIV 2;
  tPrimCnt = 0..51*1000*1000;  
  tWeakStrong = record
                   strong,
                   balanced,
                   weak : NativeUint;
                end;   
var
  primes: array [tLimit] of byte; //always initialized with 0 at startup
  delta : array [tPrimCnt] of byte;
  cntWS : tWeakStrong;  
  deltaCnt :NativeUint;
  
procedure sieveprimes;
//Only odd numbers, minimal count of strikes
var
  spIdx,sieveprime,sievePos,fact :NativeUInt;
begin
  spIdx := 1;
  repeat
    if primes[spIdx]=0 then
    begin
      sieveprime := 2*spIdx+1;
      fact := PrimeLimit DIV sieveprime;
      if Not(odd(fact)) then
        dec(fact);
      IF fact < sieveprime then
        BREAK;
      sievePos := ((fact*sieveprime)-1) DIV 2;
      fact := (fact-1) DIV 2;
      repeat
        primes[sievePos] := 1;
        repeat
          dec(fact);
          dec(sievePos,sieveprime);
        until primes[fact]= 0;
      until fact < spIdx;
    end;
    inc(spIdx);
  until false;
end;  
{ Not neccessary for this small primes.
procedure EmergencyStop(i:NativeInt);
Begin
  Writeln( 'STOP at ',i,'.th prime');
  HALT(i);
end;    
}
function GetDeltas:NativeUint;
//Converting prime positions into distance  
var 
  i,j,last : NativeInt;
Begin
  j :=0;
  i := 1;
  last :=1;
  For i := 1 to High(primes) do
    if primes[i] = 0 then
    Begin
      //IF i-last > 255 {aka delta prim > 512} then  EmergencyStop (j);
      delta[j] := i-last;
      last := i;
      inc(j);
   end;
   GetDeltas := j;
end;  
 
procedure OutHeader;
Begin
  writeln('Limit':12,'Strong':10,'balanced':12,'weak':10);
end;     

procedure OutcntWS (const cntWS : tWeakStrong;Lmt:NativeInt);
Begin
  with cntWS do
    writeln(lmt:12,Strong:10,balanced:12,weak:10);
end;     

procedure CntWeakStrong10(var Out:tWeakStrong);
// Output a table of values for strang/balanced/weak for 10^n 
var
  idx,diff,prime,lmt :NativeInt;
begin 
  OutHeader;
  lmt := 10;
  fillchar(Out,SizeOf(Out),#0);
  idx := 0;
  prime:=3;
  repeat
    dec(prime,2*delta[idx]);  
    while idx < deltaCnt do   
    Begin
      inc(prime,2*delta[idx]);
      IF prime > lmt then 
         BREAK;
         
      diff := delta[idx] - delta[idx+1];
      if diff>0 then 
        inc(Out.strong)
      else  
        if diff< 0 then 
          inc(Out.weak)
        else
          inc(Out.balanced);
          
      inc(idx);            
    end; 
    OutcntWS(Out,Lmt);
    lmt := lmt*10;
  until Lmt >  PrimeLimit; 
end;

procedure WeakOut(cnt:NativeInt);
var   
  idx,prime : NativeInt;
begin 
  Writeln('The first ',cnt,' weak primes');
  prime:=3;      
  idx := 0;
  repeat
    inc(prime,2*delta[idx]);  
    if delta[idx] - delta[idx+1]< 0 then
    Begin 
      write(prime,' ');
      dec(cnt);
      IF cnt <=0 then
        BREAK;
    end; 
    inc(idx);   
  until idx >= deltaCnt;
  Writeln;
end;

procedure StrongOut(cnt:NativeInt);
var   
  idx,prime : NativeInt;
begin 
  Writeln('The first ',cnt,' strong primes');
  prime:=3;      
  idx := 0;
  repeat
    inc(prime,2*delta[idx]);  
    if delta[idx] - delta[idx+1]> 0 then
    Begin 
      write(prime,' ');
      dec(cnt);
      IF cnt <=0 then
        BREAK;
    end; 
    inc(idx);   
  until idx >= deltaCnt;
  Writeln;
end;

begin
  sieveprimes;
  deltaCnt := GetDeltas;  
  
  StrongOut(36);
  WeakOut(37);
  CntWeakStrong10(CntWs);
end.
```

{{Out}}  

```txt

The first 36 strong primes
11 17 29 37 41 59 67 71 79 97 101 107 127 137 149 163 179 191 197 223 227 239 251 269 277 281 307 311 331 347 367 379 397 419 431 439 
The first 37 weak primes
3 7 13 19 23 31 43 47 61 73 83 89 103 109 113 131 139 151 167 181 193 199 229 233 241 271 283 293 313 317 337 349 353 359 383 389 401 
       Limit    Strong    balanced      weak
          10         0           1         2
         100        10           2        12
        1000        73          15        79
       10000       574          65       589
      100000      4543         434      4614
     1000000     37723        2994     37780
    10000000    320991       21837    321750
   100000000   2796946      167032   2797476
  1000000000  24758535     1328401  24760597

real    0m3.011s
```



## Perl

{{trans|Perl 6}}
{{libheader|ntheory}}

```perl
use ntheory qw(primes vecfirst);

sub comma {
    (my $s = reverse shift) =~ s/(.{3})/$1,/g;
    $s =~ s/,(-?)$/$1/;
    $s = reverse $s;
}

sub below { my ($m, @a) = @_; vecfirst { $a[$_] > $m } 0..$#a }

my (@strong, @weak, @balanced);
my @primes = @{ primes(10_000_019) };

for my $k (1 .. $#primes - 1) {
    my $x = ($primes[$k - 1] + $primes[$k + 1]) / 2;
    if    ($x > $primes[$k]) { push @weak,     $primes[$k] }
    elsif ($x < $primes[$k]) { push @strong,   $primes[$k] }
    else                     { push @balanced, $primes[$k] }
}

for ([\@strong,   'strong',   36, 1e6, 1e7],
     [\@weak,     'weak',     37, 1e6, 1e7],
     [\@balanced, 'balanced', 28, 1e6, 1e7]) {
    my($pr, $type, $d, $c1, $c2) = @$_;
    print "\nFirst $d $type primes:\n", join ' ', map { comma $_ } @$pr[0..$d-1], "\n";
    print "Count of $type primes <=  @{[comma $c1]}:  " . comma below($c1,@$pr) . "\n";
    print "Count of $type primes <= @{[comma $c2]}: "   . comma scalar @$pr . "\n";
}
```

{{out}}

```txt

First 36 strong primes:
11 17 29 37 41 59 67 71 79 97 101 107 127 137 149 163 179 191 197 223 227 239 251 269 277 281 307 311 331 347 367 379 397 419 431 439 
Count of strong primes <=  1,000,000:  37,723
Count of strong primes <= 10,000,000: 320,991

First 37 weak primes:
3 7 13 19 23 31 43 47 61 73 83 89 103 109 113 131 139 151 167 181 193 199 229 233 241 271 283 293 313 317 337 349 353 359 383 389 401 
Count of weak primes <=  1,000,000:  37,780
Count of weak primes <= 10,000,000: 321,750

First 28 balanced primes:
5 53 157 173 211 257 263 373 563 593 607 653 733 947 977 1,103 1,123 1,187 1,223 1,367 1,511 1,747 1,753 1,907 2,287 2,417 2,677 2,903 
Count of balanced primes <=  1,000,000:  2,994
Count of balanced primes <= 10,000,000: 21,837

```



## Perl 6

{{works with|Rakudo|2018.11}}


```perl6
sub comma { $^i.flip.comb(3).join(',').flip }

use Math::Primesieve;

my $sieve = Math::Primesieve.new;

my @primes = $sieve.primes(10_000_019);

my (@weak, @balanced, @strong);

for 1 ..^ @primes - 1 -> $p {
    given (@primes[$p - 1] + @primes[$p + 1]) / 2 {
        when * > @primes[$p] {     @weak.push: @primes[$p] }
        when * < @primes[$p] {   @strong.push: @primes[$p] }
        default              { @balanced.push: @primes[$p] }
    }
}

for @strong,   'strong',   36,
    @weak,     'weak',     37,
    @balanced, 'balanced', 28
  -> @pr, $type, $d {
    say "\nFirst $d $type primes:\n", @pr[^$d]».&comma;
    say "Count of $type primes <=  {comma 1e6}:  ", comma +@pr[^(@pr.first: * > 1e6,:k)];
    say "Count of $type primes <= {comma 1e7}: ", comma +@pr;
}
```

{{out}}

```txt
First 36 strong primes:
(11 17 29 37 41 59 67 71 79 97 101 107 127 137 149 163 179 191 197 223 227 239 251 269 277 281 307 311 331 347 367 379 397 419 431 439)
Count of strong primes <=  1,000,000:  37,723
Count of strong primes <= 10,000,000: 320,991

First 37 weak primes:
(3 7 13 19 23 31 43 47 61 73 83 89 103 109 113 131 139 151 167 181 193 199 229 233 241 271 283 293 313 317 337 349 353 359 383 389 401)
Count of weak primes <=  1,000,000:  37,780
Count of weak primes <= 10,000,000: 321,750

First 28 balanced primes:
(5 53 157 173 211 257 263 373 563 593 607 653 733 947 977 1,103 1,123 1,187 1,223 1,367 1,511 1,747 1,753 1,907 2,287 2,417 2,677 2,903)
Count of balanced primes <=  1,000,000:  2,994
Count of balanced primes <= 10,000,000: 21,837
```



## Phix

Using [[Extensible_prime_generator#Phix]]

```Phix
while sieved<10_000_000 do add_block() end while
sequence {strong, weak} @= {}
for i=2 to abs(binary_search(10_000_000,primes))-1 do
    integer p = primes[i],
            c = compare(p,(primes[i-1]+primes[i+1])/2)
    if    c=+1 then strong &= p
    elsif c=-1 then weak   &= p
    end if
end for
printf(1,"The first 36 strong primes:") ?strong[1..36]
printf(1,"The first 37 weak primes:")   ?weak[1..37]
printf(1,"%,7d strong primes below 1,000,000\n",abs(binary_search(1_000_000,strong))-1)
printf(1,"%,7d strong primes below 10,000,000\n",length(strong))
printf(1,"%,7d weak primes below 1,000,000\n",abs(binary_search(1_000_000,weak))-1)
printf(1,"%,7d weak primes below 10,000,000\n",length(weak))
```

{{out}}
<pre style="font-size: 11px">
The first 36 strong primes:{11,17,29,37,41,59,67,71,79,97,101,107,127,137,149,163,179,191,197,223,227,239,251,269,277,281,307,311,331,347,367,379,397,419,431,439}
The first 37 weak primes:{3,7,13,19,23,31,43,47,61,73,83,89,103,109,113,131,139,151,167,181,193,199,229,233,241,271,283,293,313,317,337,349,353,359,383,389,401}
 37,723 strong primes below 1,000,000
320,991 strong primes below 10,000,000
 37,780 weak primes below 1,000,000
321,750 weak primes below 10,000,000

```



## Python

Using the popular [http://www.numpy.org numpy] library for fast prime generation.

COmputes and shows the requested output then adds similar output for the "balanced" case  where <code>prime(p) == [prime(p-1) + prime(p+1)] ÷ 2</code>.

```python
import numpy as np

def primesfrom2to(n):
    # https://stackoverflow.com/questions/2068372/fastest-way-to-list-all-primes-below-n-in-python/3035188#3035188
    """ Input n>=6, Returns a array of primes, 2 <= p < n """
    sieve = np.ones(n//3 + (n%6==2), dtype=np.bool)
    sieve[0] = False
    for i in range(int(n**0.5)//3+1):
        if sieve[i]:
            k=3*i+1|1
            sieve[      ((k*k)//3)      ::2*k] = False
            sieve[(k*k+4*k-2*k*(i&1))//3::2*k] = False
    return np.r_[2,3,((3*np.nonzero(sieve)[0]+1)|1)]

p = primes10m   = primesfrom2to(10_000_000)
s = strong10m   = [t for s, t, u in zip(p, p[1:], p[2:]) 
                   if t > (s + u) / 2]
w = weak10m     = [t for s, t, u in zip(p, p[1:], p[2:]) 
                   if t < (s + u) / 2]
b = balanced10m = [t for s, t, u in zip(p, p[1:], p[2:]) 
                   if t == (s + u) / 2]

print('The first   36   strong primes:', s[:36])
print('The   count   of the strong primes below   1,000,000:',
      sum(1 for p in s if p < 1_000_000))
print('The   count   of the strong primes below  10,000,000:', len(s))
print('\nThe first   37   weak primes:', w[:37])
print('The   count   of the weak   primes below   1,000,000:',
      sum(1 for p in w if p < 1_000_000))
print('The   count   of the weak   primes below  10,000,000:', len(w))
print('\n\nThe first   10 balanced primes:', b[:10])
print('The   count   of balanced   primes below   1,000,000:',
      sum(1 for p in b if p < 1_000_000))
print('The   count   of balanced   primes below  10,000,000:', len(b))
print('\nTOTAL primes below   1,000,000:',
      sum(1 for pr in p if pr < 1_000_000))
print('TOTAL primes below  10,000,000:', len(p))
```


{{out}}

```txt
The first   36   strong primes: [11, 17, 29, 37, 41, 59, 67, 71, 79, 97, 101, 107, 127, 137, 149, 163, 179, 191, 197, 223, 227, 239, 251, 269, 277, 281, 307, 311, 331, 347, 367, 379, 397, 419, 431, 439]
The   count   of the strong primes below   1,000,000: 37723
The   count   of the strong primes below  10,000,000: 320991

The first   37   weak primes: [3, 7, 13, 19, 23, 31, 43, 47, 61, 73, 83, 89, 103, 109, 113, 131, 139, 151, 167, 181, 193, 199, 229, 233, 241, 271, 283, 293, 313, 317, 337, 349, 353, 359, 383, 389, 401]
The   count   of the weak   primes below   1,000,000: 37780
The   count   of the weak   primes below  10,000,000: 321749


The first   10 balanced primes: [5, 53, 157, 173, 211, 257, 263, 373, 563, 593]
The   count   of balanced   primes below   1,000,000: 2994
The   count   of balanced   primes below  10,000,000: 21837

TOTAL primes below   1,000,000: 78498
TOTAL primes below  10,000,000: 664579
```



## REXX


```rexx
/*REXX program lists a sequence  (or a count)  of  ──strong──   or   ──weak──   primes. */
parse arg N kind _ . 1 . okind;     upper kind   /*obtain optional arguments from the CL*/
if N=='' | N==","  then N= 36                    /*Not specified?   Then assume default.*/
if kind=='' | kind==","  then kind= 'STRONG'     /* "      "          "     "      "    */
if _\==''                             then call ser 'too many arguments specified.'
if kind\=='WEAK'  &  kind\=='STRONG'  then call ser 'invalid 2nd argument: '   okind
if kind =='WEAK'  then weak= 1;  else weak= 0    /*WEAK  is a binary value for function.*/
w = linesize() - 1                               /*obtain the usable width of the term. */
tell= (N>0);    @.=;    N= abs(N)                /*N is negative?   Then don't display. */
!.=0;   !.1=2;  !.2=3;  !.3=5;  !.4=7;  !.5=11;  !.6=13;  !.7=17;  !.8=19;   !.9=23;  #= 8
@.='';  @.2=1;  @.3=1;  @.5=1;  @.7=1;  @.11=1;  @.13=1;  @.17=1;  @.19=1;   start= # + 1
m= 0;                           lim= 0           /*#  is the number of low primes so far*/
$=;     do i=3  for #-2   while lim<=N           /* [↓]  find primes, and maybe show 'em*/
        call strongWeak i-1;       $= strip($)   /*go see if other part of a KIND prime.*/
        end   /*i*/                              /* [↑]  allows faster loop (below).    */
                                                 /* [↓]  N:  default lists up to 35 #'s.*/
   do j=!.#+2  by 2  while  lim<N                /*continue on with the next odd prime. */
   if j // 3 == 0  then iterate                  /*is this integer a multiple of three? */
   parse var  j    ''  -1  _                     /*obtain the last decimal digit of  J  */
   if _      == 5  then iterate                  /*is this integer a multiple of five?  */
   if j // 7 == 0  then iterate                  /* "   "     "    "     "     " seven? */
   if j //11 == 0  then iterate                  /* "   "     "    "     "     " eleven?*/
   if j //13 == 0  then iterate                  /* "   "     "    "     "     "  13 ?  */
   if j //17 == 0  then iterate                  /* "   "     "    "     "     "  17 ?  */
   if j //19 == 0  then iterate                  /* "   "     "    "     "     "  19 ?  */
                                                 /* [↓]  divide by the primes.   ___    */
            do k=start  to #  while !.k * !.k<=j /*divide  J  by other primes ≤ √ J     */
            if j // !.k ==0   then iterate j     /*÷ by prev. prime?  ¬prime     ___    */
            end   /*k*/                          /* [↑]   only divide up to     √ J     */
   #= # + 1                                      /*bump the count of number of primes.  */
   !.#= j;                     @.j= 1            /*define a prime  and  its index value.*/
   call strongWeak #-1                           /*go see if other part of a KIND prime.*/
   end   /*j*/
                                                 /* [↓]  display number of primes found.*/
if $\==''  then say $                            /*display any residual primes in $ list*/
say
if tell  then say commas(m)' '     kind    "primes found."
         else say commas(m)' '     kind    "primes found below or equal to "    commas(N).
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
add: m= m+1; lim= m; if \tell & y>N  then do; lim= y; m= m-1; end; else call app; return 1
app: if tell  then if length($ y)>w  then do;  say $; $= y;   end; else $= $ y;   return 1
ser: say;  say;  say '***error***' arg(1);  say;  say;  exit 13   /*tell error message. */
commas: parse arg _;  do jc=length(_)-3  to 1  by -3; _=insert(',', _, jc); end;  return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
strongWeak: parse arg x;  Lp= x - 1;     Hp= x + 1;     y=!.x;        s= (!.Lp + !.Hp) / 2
            if weak  then if y<s  then return add()               /*is  a    weak prime.*/
                                  else return 0                   /*not "      "    "   */
                     else if y>s  then return add()               /*is  an strong prime.*/
                                       return 0                   /*not  "   "      "   */
```

This REXX program makes use of   '''LINESIZE'''   REXX program (or
BIF) which is used to determine the screen width (or linesize) of the
terminal (console).    Some REXXes don't have this BIF.

The   '''LINESIZE.REX'''   REXX program is included
here   ───►   [[LINESIZE.REX]].


{{out|output|text=  when using the default input of:     <tt> 36   strong </tt>}}

```txt

11 17 29 37 41 59 67 71 79 97 101 107 127 137 149 163 179 191 197 223 227 239 251 269 277 281 307 311 331 347 367 379 397 419 431 439

36  STRONG primes found.

```

{{out|output|text=  when using the default input of:     <tt> -1000000   strong </tt>}}

```txt

37,723  STRONG primes found below or equal to  1,000,000.

```

{{out|output|text=  when using the default input of:     <tt> -10000000   strong </tt>}}

```txt

320,991  STRONG primes found below or equal to  10,000,000.

```

{{out|output|text=  when using the default input of:     <tt> 37   weak </tt>}}

```txt

3 7 13 19 23 31 43 47 61 73 83 89 103 109 113 131 139 151 167 181 193 199 229 233 241 271 283 293 313 317 337 349 353 359 383 389 401

37  WEAK primes found.

```

{{out|output|text=  when using the default input of:     <tt> -1000000   weak </tt>}}

```txt

37,780  WEAK primes found below or equal to  1,000,000.

```

{{out|output|text=  when using the default input of:     <tt> -1000000   weak </tt>}}

```txt

321,750  WEAK primes found below or equal to  10,000,000.

```



## Ruby


```ruby
require 'prime'

strong_gen = Enumerator.new{|y| Prime.each_cons(3){|a,b,c|y << b if a+c-b<b} }
weak_gen   = Enumerator.new{|y| Prime.each_cons(3){|a,b,c|y << b if a+c-b>b} }

puts "First 36 strong primes:"
puts strong_gen.take(36).join(" "), "\n"
puts "First 37 weak primes:"
puts weak_gen.take(37).join(" "), "\n"

[1_000_000, 10_000_000].each do |limit|
  strongs, weaks = 0, 0
  Prime.each_cons(3) do |a,b,c|
    strongs += 1 if b > a+c-b
    weaks += 1 if b < a+c-b
    break if c > limit
  end
  puts "#{strongs} strong primes and #{weaks} weak primes below #{limit}."
end

```

{{out}}

```txt
First 36 strong primes:
11 17 29 37 41 59 67 71 79 97 101 107 127 137 149 163 179 191 197 223 227 239 251 269 277 281 307 311 331 347 367 379 397 419 431 439

First 37 weak primes:
3 7 13 19 23 31 43 47 61 73 83 89 103 109 113 131 139 151 167 181 193 199 229 233 241 271 283 293 313 317 337 349 353 359 383 389 401

37723 strong primes and 37780 weak primes below 1000000.
320991 strong primes and 321750 weak primes below 10000000.

```



## Scala

This example works entirely with lazily evaluated lists. It starts with a list of primes, and generates a sliding iterator that looks at each triplet of primes. Lists of strong and weak primes are built by applying the given filters then selecting the middle term from each triplet.

```scala
object StrongWeakPrimes {
  def main(args: Array[String]): Unit = {
    val bnd = 1000000
    println(
      f"""|First 36 Strong Primes: ${strongPrimes.take(36).map(n => f"$n%,d").mkString(", ")}
          |Strong Primes < 1,000,000: ${strongPrimes.takeWhile(_ < bnd).size}%,d
          |Strong Primes < 10,000,000: ${strongPrimes.takeWhile(_ < 10*bnd).size}%,d
          |
          |First 37 Weak Primes: ${weakPrimes.take(37).map(n => f"$n%,d").mkString(", ")}
          |Weak Primes < 1,000,000: ${weakPrimes.takeWhile(_ < bnd).size}%,d
          |Weak Primes < 10,000,000: ${weakPrimes.takeWhile(_ < 10*bnd).size}%,d""".stripMargin)
  }
  
  def weakPrimes: LazyList[Int] = primeTrips.filter{case a +: b +: c +: _ => b < (a + c)/2.0}.map(_(1)).to(LazyList)
  def strongPrimes: LazyList[Int] = primeTrips.filter{case a +: b +: c +: _ => b > (a + c)/2}.map(_(1)).to(LazyList)
  def primeTrips: Iterator[LazyList[Int]] = primes.sliding(3)
  def primes: LazyList[Int] = 2 #:: LazyList.from(3, 2).filter(n => !Iterator.range(3, math.sqrt(n).toInt + 1, 2).exists(n%_ == 0))
}
```


{{out}}

```txt
First 36 Strong Primes: 11, 17, 29, 37, 41, 59, 67, 71, 79, 97, 101, 107, 127, 137, 149, 163, 179, 191, 197, 223, 227, 239, 251, 269, 277, 281, 307, 311, 331, 347, 367, 379, 397, 419, 431, 439
Strong Primes < 1,000,000: 37,723
Strong Primes < 10,000,000: 320,991

First 37 Weak Primes: 3, 7, 13, 19, 23, 31, 43, 47, 61, 73, 83, 89, 103, 109, 113, 131, 139, 151, 167, 181, 193, 199, 229, 233, 241, 271, 283, 293, 313, 317, 337, 349, 353, 359, 383, 389, 401
Weak Primes < 1,000,000: 37,780
Weak Primes < 10,000,000: 321,750
```



## Sidef

{{trans|Perl 6}}

```ruby
var primes = 10_000_019.primes

var (*strong, *weak, *balanced)

for k in (1 ..^ primes.end) {
    var p = primes[k]

    given((primes[k-1] + primes[k+1])/2) { |x|
        case (x > p) {     weak << p }
        case (x < p) {   strong << p }
        else         { balanced << p }
    }
}

for pr, type, d, c1, c2 in [
    [  strong, 'strong',   36, 1e6, 1e7],
    [    weak, 'weak',     37, 1e6, 1e7],
    [balanced, 'balanced', 28, 1e6, 1e7],
] {
    say ("\nFirst #{d} #{type} primes:\n", pr.first(d).map{.commify}.join(' '))
    say ("Count of #{type} primes <= #{c1.commify}:  ", pr.first_index { _ > 1e6 }.commify)
    say ("Count of #{type} primes <= #{c2.commify}: " , pr.len.commify)
}
```

{{out}}

```txt

First 36 strong primes:
11 17 29 37 41 59 67 71 79 97 101 107 127 137 149 163 179 191 197 223 227 239 251 269 277 281 307 311 331 347 367 379 397 419 431 439
Count of strong primes <= 1,000,000:  37,723
Count of strong primes <= 10,000,000: 320,991

First 37 weak primes:
3 7 13 19 23 31 43 47 61 73 83 89 103 109 113 131 139 151 167 181 193 199 229 233 241 271 283 293 313 317 337 349 353 359 383 389 401
Count of weak primes <= 1,000,000:  37,780
Count of weak primes <= 10,000,000: 321,750

First 28 balanced primes:
5 53 157 173 211 257 263 373 563 593 607 653 733 947 977 1,103 1,123 1,187 1,223 1,367 1,511 1,747 1,753 1,907 2,287 2,417 2,677 2,903
Count of balanced primes <= 1,000,000:  2,994
Count of balanced primes <= 10,000,000: 21,837

```



## zkl

Using GMP (GNU Multiple Precision Arithmetic Library, probabilistic
primes), because it is easy and fast to generate primes.

[[Extensible prime generator#zkl]] could be used instead.

```zkl
var [const] BI=Import("zklBigNum");  // libGMP
const N=1e7;

pw,strong,weak := BI(1),List(),List();   // 32,0991  32,1751
ps:=(3).pump(List,'wrap{ pw.nextPrime().toInt() }).copy();  // rolling window
do{
   pp,p,pn := ps;
   if((z:=(pp.toFloat() + pn)/2)){  // 2,3,5 --> 3.5
      if(z>p)      weak  .append(p);
      else if(z<p) strong.append(p);
   }
   ps.pop(0); ps.append(pw.nextPrime().toInt());
}while(pn<=N);
```


```zkl
foreach nm,list,psz in (T(T("strong",strong,36), T("weak",weak,37))){
   println("First %d %s primes:\n%s".fmt(psz,nm,list[0,psz].concat(" ")));
   println("Count of %s primes <= %,10d: %,8d"
	    .fmt(nm,1e6,list.reduce('wrap(s,p){ s + (p<=1e6) },0)));
   println("Count of %s primes <= %,10d: %,8d\n".fmt(nm,1e7,list.len()));
}
```

{{out}}

```txt

First 36 strong primes:
11 17 29 37 41 59 67 71 79 97 101 107 127 137 149 163 179 191 197 223 227 239 251 269 277 281 307 311 331 347 367 379 397 419 431 439
Count of strong primes <=  1,000,000:   37,723
Count of strong primes <= 10,000,000:  320,991

First 37 weak primes:
3 7 13 19 23 31 43 47 61 73 83 89 103 109 113 131 139 151 167 181 193 199 229 233 241 271 283 293 313 317 337 349 353 359 383 389 401
Count of weak primes <=  1,000,000:   37,780
Count of weak primes <= 10,000,000:  321,750

```

