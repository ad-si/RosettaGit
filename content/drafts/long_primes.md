+++
title = "Long primes"
description = ""
date = 2019-07-09T05:11:58Z
aliases = []
[extra]
id = 21940
[taxonomies]
categories = []
tags = []
+++

{{task|Prime Numbers}}


A   '''long prime'''   (the definition that will be used
here)   are primes whose reciprocals   (in decimal)   have
a   ''period length''   of one less than
the prime number   (also expressed in decimal).


'''Long primes'''   are also known as:
::*   base ten cyclic numbers
::*   full reptend primes
::*   golden primes
::*   long period primes
::*   maximal period primes
::*   proper primes



;Example:
'''7'''   is the first long prime,   the reciprocal of seven
is   <big><sup>'''1'''</sup>'''/'''<sub>'''7'''</sub></big>,   which
is equal to the repeating decimal fraction   '''0.142857<u>142857</u>···

The <u>length</u> of the   ''repeating''   part of the decimal fraction
is six,   (the underlined part)   which is one less
than the (decimal) prime number   '''7'''.

Thus   '''7'''   is a long prime.


There are other (more) general definitions of a   '''long prime'''   which
include wording/verbiage for bases other than ten.


;Task:
:*   Show all long primes up to   '''500'''   (preferably on one line).
:*   Show the   number   of long primes up to         '''   500'''
:*   Show the   number   of long primes up to                   ''' 1,000'''
:*   Show the   number   of long primes up to                   ''' 2,000'''
:*   Show the   number   of long primes up to                   ''' 4,000'''
:*   Show the   number   of long primes up to                   ''' 8,000'''
:*   Show the   number   of long primes up to                        '''16,000'''
:*   Show the   number   of long primes up to                        '''32,000'''
:*   Show the   number   of long primes up to                        '''64,000'''   (optional)
:*   Show all output here.


;;;Also see:
:*   the Wikipedia webpage:   [http://wikipedia.org/wiki/Full_reptend_prime full reptend prime].
:*   the MathWorld webpage:   [http://mathworld.wolfram.com/FullReptendPrime.html full reptend prime].
:*   the     OEIS     webpage:   [http://oeis.org/A001913 A1913].





## C

{{trans|Go}}

```c>#include <stdio.h

#include <stdlib.h>

#define TRUE 1
#define FALSE 0

typedef int bool;

void sieve(int limit, int primes[], int *count) {
    bool *c = calloc(limit + 1, sizeof(bool)); // composite = TRUE
    // no need to process even numbers
    int i, p = 3, p2, n = 0;
    while (TRUE) {
        p2 = p * p;
        if (p2 > limit) break;
        for (i = p2; i <= limit; i += 2 * p) c[i] = TRUE;
        while (TRUE) {
            p += 2;
            if (!c[p]) break;
        }
    }
    for (i = 3; i <= limit; i += 2) {
        if (!c[i]) primes[n++] = i;
    }
    *count = n;
    free(c);
}

// finds the period of the reciprocal of n
int findPeriod(int n) {
    int i, r = 1, rr, period = 0;
    for (i = 1; i <= n + 1; ++i) {
        r = (10 * r) % n;
    }
    rr = r;
    while (TRUE) {
        r = (10 * r) % n;
        period++;
        if (r == rr) break;
    }
    return period;
}

int main() {
    int i, prime, count = 0, index = 0, primeCount, longCount = 0;
    int *primes, *longPrimes;
    int numbers[] = {500, 1000, 2000, 4000, 8000, 16000, 32000, 64000};
    int totals[8];
    primes = calloc(6500, sizeof(int));
    longPrimes = calloc(2500, sizeof(int));
    sieve(64000, primes, &primeCount);
    for (i = 0; i < primeCount; ++i) {
        prime = primes[i];
        if (findPeriod(prime) == prime - 1) {
            longPrimes[longCount++] = prime;
        }
    }
    for (i = 0; i < longCount; ++i) {
        if (longPrimes[i] > numbers[index]) {
            totals[index++] = count;
        }
        count++;
    }
    totals[7] = count;
    printf("The long primes up to 500 are:\n");
    printf("[");
    for (i = 0; i < totals[0]; ++i) {
        printf("%d ", longPrimes[i]);
    }
    printf("\b]\n");

    printf("\nThe number of long primes up to:\n");
    for (i = 0; i < 8; ++i) {
        printf("  %5d is %d\n", numbers[i], totals[i]);
    }
    free(longPrimes);
    free(primes);
    return 0;
}
```


{{out}}

```txt

The long primes up to 500 are:
[7 17 19 23 29 47 59 61 97 109 113 131 149 167 179 181 193 223 229 233 257 263 269 313 337 367 379 383 389 419 433 461 487 491 499]

The number of long primes up to:
    500 is 35
   1000 is 60
   2000 is 116
   4000 is 218
   8000 is 390
  16000 is 716
  32000 is 1300
  64000 is 2430

```



## C sharp

{{works with|C sharp|7}}

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public static class LongPrimes
{
    public static void Main() {
        var primes = SomePrimeGenerator.Primes(64000).Skip(1).Where(p => Period(p) == p - 1).Append(99999);
        Console.WriteLine(string.Join(" ", primes.TakeWhile(p => p <= 500)));
        int count = 0, limit = 500;
        foreach (int prime in primes) {
            if (prime > limit) {
                Console.WriteLine($"There are {count} long primes below {limit}");
                limit *= 2;
            }
            count++;
        }

        int Period(int n) {
            int r = 1, rr;
            for (int i = 0; i <= n; i++) r = 10 * r % n;
            rr = r;
            for (int period = 1;; period++) {
                r = (10 * r) % n;
                if (r == rr) return period;
            }
        }
    }

}
```

{{out}}

```txt

7 17 19 23 29 47 59 61 97 109 113 131 149 167 179 181 193 223 229 233 257 263 269 313 337 367 379 383 389 419 433 461 487 491 499
There are 35 long primes below 500
There are 60 long primes below 1000
There are 116 long primes below 2000
There are 218 long primes below 4000
There are 390 long primes below 8000
There are 716 long primes below 16000
There are 1300 long primes below 32000
There are 2430 long primes below 64000
```


=={{header|F_Sharp|F#}}==

### The Functions

This task uses [http://www.rosettacode.org/wiki/Extensible_prime_generator#The_function Extensible Prime Generator (F#)]

This task uses [[Factors_of_an_integer#F.23]]

```fsharp

// Return true if prime n is a long prime. Nigel Galloway: September 25th., 2018
let fN n g = let rec fN i g e l = match e with | 0UL                -> i
                                               | _ when e%2UL = 1UL -> fN ((i*g)%l) ((g*g)%l) (e/2UL) l
                                               | _                  -> fN i ((g*g)%l) (e/2UL) l
             fN 1UL 10UL (uint64 g) (uint64 n)
let isLongPrime n=Seq.length (factors (n-1) |> Seq.filter(fun g->(fN n g)=1UL))=1

```


### The Task


```fsharp

primes |> Seq.skip 3 |> Seq.takeWhile(fun n->n<500) |> Seq.filter isLongPrime |> Seq.iter(printf "%d ")

```

{{out}}

```txt

7 17 19 23 29 47 59 61 97 109 113 131 149 167 179 181 193 223 229 233 257 263 269 313 337 367 379 383 389 419 433 461 487 491 499 

```


```fsharp

printfn "There are %d long primes less than 500" (primes |> Seq.skip 3 |> Seq.takeWhile(fun n->n<500) |> Seq.filter isLongPrime |> Seq.length)

```

{{out}}

```txt

There are 35 long primes less than 500

```


```fsharp

printfn "There are %d long primes less than 1000" (primes |> Seq.skip 3 |> Seq.takeWhile(fun n->n<1000) |> Seq.filter isLongPrime |> Seq.length)

```

{{out}}

```txt

There are 60 long primes less than 1000

```


```fsharp

printfn "There are %d long primes less than 2000" (primes |> Seq.skip 3 |> Seq.takeWhile(fun n->n<2000) |> Seq.filter isLongPrime |> Seq.length)

```

{{out}}

```txt

There are 116 long primes less than 2000

```


```fsharp

printfn "There are %d long primes less than 4000" (primes |> Seq.skip 3 |> Seq.takeWhile(fun n->n<4000) |> Seq.filter isLongPrime|> Seq.length)

```

{{out}}

```txt

There are 218 long primes less than 4000

```


```fsharp

printfn "There are %d long primes less than 8000" (primes |> Seq.skip 3 |> Seq.takeWhile(fun n->n<8000) |> Seq.filter isLongPrime |> Seq.length)

```

{{out}}

```txt

There are 390 long primes less than 8000

```


```fsharp

printfn "There are %d long primes less than 16000" (primes |> Seq.skip 3 |> Seq.takeWhile(fun n->n<16000) |> Seq.filter isLongPrime |> Seq.length)

```

{{out}}

```txt

There are 716 long primes less than 16000

```


```fsharp

printfn "There are %d long primes less than 32000" (primes |> Seq.skip 3 |> Seq.takeWhile(fun n->n<32000) |> Seq.filter isLongPrime |> Seq.length)

```

{{out}}

```txt

There are 1300 long primes less than 32000

```


```fsharp

printfn "There are %d long primes less than 64000" (primes |> Seq.skip 3 |> Seq.takeWhile(fun n->n<64000) |> Seq.filter isLongPrime|> Seq.length)

```

{{out}}

```txt

There are 2430 long primes less than 64000

```


```fsharp

printfn "There are %d long primes less than 128000" (primes |> Seq.skip 3 |> Seq.takeWhile(fun n->n<128000) |> Seq.filter isLongPrime|> Seq.length)

```

{{out}}

```txt

There are 4498 long primes less than 128000
Real: 00:00:01.294, CPU: 00:00:01.300, GC gen0: 27, gen1: 0

```


```fsharp

printfn "There are %d long primes less than 256000" (primes |> Seq.skip 3 |> Seq.takeWhile(fun n->n<256000) |> Seq.filter isLongPrime|> Seq.length)

```

{{out}}

```txt

There are 8434 long primes less than 256000
Real: 00:00:03.434, CPU: 00:00:03.440, GC gen0: 58, gen1: 0

```


```fsharp

printfn "There are %d long primes less than 512000" (primes |> Seq.skip 3 |> Seq.takeWhile(fun n->n<512000) |> Seq.filter isLongPrime|> Seq.length)

```

{{out}}

```txt

There are 15920 long primes less than 512000
Real: 00:00:09.248, CPU: 00:00:09.260, GC gen0: 128, gen1: 0

```


```fsharp

printfn "There are %d long primes less than 1024000" (primes |> Seq.skip 3 |> Seq.takeWhile(fun n->n<1024000) |> Seq.filter isLongPrime|> Seq.length)

```

{{out}}

```txt

There are 30171 long primes less than 1024000
Real: 00:00:24.959, CPU: 00:00:25.020, GC gen0: 278, gen1: 1

```



## Factor


```factor
USING: formatting fry io kernel math math.functions math.primes
math.primes.factors memoize prettyprint sequences ;
IN: rosetta-code.long-primes

: period-length ( p -- len )
    [ 1 - divisors ] [ '[ 10 swap _ ^mod 1 = ] ] bi find nip ;

MEMO: long-prime? ( p -- ? ) [ period-length ] [ 1 - ] bi = ;

: .lp<=500 ( -- )
    500 primes-upto [ long-prime? ] filter
    "Long primes <= 500:" print [ pprint bl ] each nl ;

: .#lp<=n ( n -- )
    dup primes-upto [ long-prime? t = ] count swap
    "%-4d long primes <= %d\n" printf ;

: long-primes-demo ( -- )
    .lp<=500 nl
    { 500 1,000 2,000 4,000 8,000 16,000 32,000 64,000 }
    [ .#lp<=n ] each ;

MAIN: long-primes-demo
```

{{out}}

```txt

Long primes <= 500:
7 17 19 23 29 47 59 61 97 109 113 131 149 167 179 181 193 223 229 233 257 263 269 313 337 367 379 383 389 419 433 461 487 491 499 

35   long primes <= 500
60   long primes <= 1000
116  long primes <= 2000
218  long primes <= 4000
390  long primes <= 8000
716  long primes <= 16000
1300 long primes <= 32000
2430 long primes <= 64000

```


## FreeBASIC


```freebasic
' version 01-02-2019
' compile with: fbc -s console

Dim Shared As UByte prime()

Sub find_primes(n As UInteger)

    ReDim prime(n)
    Dim As UInteger i, k

    ' need only to consider odd primes, 2 has no repetion
    For i = 3 To n Step 2
        If prime(i) = 0 Then
            For k = i * i To n Step i + i
                prime(k) = 1
            Next
        End If
    Next

End Sub

Function find_period(p As UInteger) As UInteger
    ' finds period for every positive number
    Dim As UInteger period, r = 1

    Do
        r = (r * 10) Mod p
        period += 1
        If r <= 1 Then Return period
    Loop

End Function

' ------=< MAIN >=------

#Define max 64000
Dim As UInteger p = 3, n1 = 3, n2 = 500, i, n50, count

find_primes(max)
Print "Long primes upto 500 are ";

For i = n1 To n2 Step 2
    If prime(i) = 0 Then
        If i -1 = find_period(i) Then
            If n50 <= 50 Then
                Print Str(i); " ";
            End If
            count += 1
        End If
    End If
Next

Print : Print

Do
    Print "There are "; Str(count); " long primes upto "; Str(n2)

    n1 = n2 +1
    n2 += n2
    If n1 > max Then Exit Do

    For i = n1 To n2 Step 2
        If prime(i) = 0 Then
            If i -1 = find_period(i) Then
                count += 1
            End If
        End If
    Next
Loop

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
Long primes upto 500 are 7 17 19 23 29 47 59 61 97 109 113 131 149 167 179 181 193 223 229 233 257 263 269 313 337 367 379 383 389 419 433 461 487 491 499

There are 35 long primes upto 500
There are 60 long primes upto 1000
There are 116 long primes upto 2000
There are 218 long primes upto 4000
There are 390 long primes upto 8000
There are 716 long primes upto 16000
There are 1300 long primes upto 32000
There are 2430 long primes upto 64000
```



## Go


```go
package main

import "fmt"

func sieve(limit int) []int {
    var primes []int
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
    return primes
}

// finds the period of the reciprocal of n
func findPeriod(n int) int {
    r := 1
    for i := 1; i <= n+1; i++ {
        r = (10 * r) % n
    }
    rr := r
    period := 0
    for {
        r = (10 * r) % n
        period++
        if r == rr {
            break
        }
    }
    return period
}

func main() {
    primes := sieve(64000)
    var longPrimes []int
    for _, prime := range primes {
        if findPeriod(prime) == prime-1 {
            longPrimes = append(longPrimes, prime)
        }
    }
    numbers := []int{500, 1000, 2000, 4000, 8000, 16000, 32000, 64000}
    index := 0
    count := 0
    totals := make([]int, len(numbers))
    for _, longPrime := range longPrimes {
        if longPrime > numbers[index] {
            totals[index] = count
            index++
        }
        count++
    }
    totals[len(numbers)-1] = count
    fmt.Println("The long primes up to 500 are: ")
    fmt.Println(longPrimes[:totals[0]])

    fmt.Println("\nThe number of long primes up to: ")
    for i, total := range totals {
        fmt.Printf("  %5d is %d\n", numbers[i], total)
    }
}
```


{{out}}

```txt

The long primes up to 500 are: 
[7 17 19 23 29 47 59 61 97 109 113 131 149 167 179 181 193 223 229 233 257 263 269 313 337 367 379 383 389 419 433 461 487 491 499]

The number of long primes up to: 
    500 is 35
   1000 is 60
   2000 is 116
   4000 is 218
   8000 is 390
  16000 is 716
  32000 is 1300
  64000 is 2430

```



## J


```txt

   NB. define the verb long
   NB. long is true iff the prime input greater than 2
   NB. is a rosettacode long prime.
   NB. 0 is false, 1 is true.

   long =: ( <:@:[ = #@~.@( [: }. ( | 10&* )^:( <@[ ) ) )&1&>
   

   NB. demonstration of the long verb
   NB. long applied to integers 3 through 9 inclusively

   (,: long) 3 4 5 6 7 8 9
3 4 5 6 7 8 9
0 0 0 0 1 0 0

 
   NB. find the number of primes through 64000

  [ N =: p:^:_1 ] 64000
6413

 
   NB. copy the long primes, excluding 2, the first.

   LONG_PRIMES =: (#~ long) p: >: i. N


   NB. those less than 500

   ( #~ <&500) LONG_PRIMES
7 17 19 23 29 47 59 61 97 109 113 131 149 167 179 181 193 223 229 233 257 263 269 313 337 367 379 383 389 419 433 461 487 491 499


   NB. counts

   [ MEASURE =: 500 * 2 ^ i. 8
500 1000 2000 4000 8000 16000 32000 64000


   LONG_PRIMES ( ] ,: [: +/ </ ) MEASURE
500 1000 2000 4000 8000 16000 32000 64000
35   60  116  218  390   716  1300  2430

```



## Julia

{{trans|Sidef}}

```julia

using Primes

function divisors(n)
    f = [one(n)]
    for (p,e) in factor(n)
        f = reduce(vcat, [f*p^j for j in 1:e], init=f)
    end
    return length(f) == 1 ? [one(n), n] : sort!(f)
end
 
function islongprime(p)
    for i in divisors(p-1)
        if powermod(10, i, p) == 1
            return i + 1 == p
        end
    end
    false
end
 
println("Long primes ≤ 500: ")
for i in 2:500
    if islongprime(i)
        i == 229 ? println(i) : print(i, "  ")
    end
end
print("\n\n")
 
for i in [500, 1000, 2000, 4000, 8000, 16000, 32000, 64000]
    println("Number of long primes ≤ $i: $(sum(map(x->islongprime(x), 1:i)))")
end

```

{{output}}
```txt

Long primes ≤ 500:
7  17  19  23  29  47  59  61  97  109  113  131  149  167  179  181  193  223  229
233  257  263  269  313  337  367  379  383  389  419  433  461  487  491  499

Number of long primes ≤ 500: 35
Number of long primes ≤ 1000: 60
Number of long primes ≤ 2000: 116
Number of long primes ≤ 4000: 218
Number of long primes ≤ 8000: 390
Number of long primes ≤ 16000: 716
Number of long primes ≤ 32000: 1300
Number of long primes ≤ 64000: 2430

```



## Kotlin

{{trans|Go}}

```scala
// Version 1.2.60

fun sieve(limit: Int): List<Int> {
    val primes = mutableListOf<Int>()
    val c = BooleanArray(limit + 1)  // composite = true
    // no need to process even numbers
    var p = 3
    while (true) {
        val p2 = p * p
        if (p2 > limit) break
        for (i in p2..limit step 2 * p) c[i] = true
        while (true) {
            p += 2
            if (!c[p]) break
        }
    }
    for (i in 3..limit step 2) {
        if (!c[i]) primes.add(i)
    }
    return primes
}

// finds the period of the reciprocal of n
fun findPeriod(n: Int): Int {
    var r = 1
    for (i in 1..n + 1) r = (10 * r) % n
    val rr = r
    var period = 0
    while (true) {
        r = (10 * r) % n
        period++
        if (r == rr) break
    }
    return period
}

fun main(args: Array<String>) {
    val primes = sieve(64000)
    val longPrimes = mutableListOf<Int>()
    for (prime in primes) {
        if (findPeriod(prime) == prime - 1) {
            longPrimes.add(prime)
        }
    }
    val numbers = listOf(500, 1000, 2000, 4000, 8000, 16000, 32000, 64000)
    var index = 0
    var count = 0
    val totals = IntArray(numbers.size)
    for (longPrime in longPrimes) {
        if (longPrime > numbers[index]) {
            totals[index++] = count
        }
        count++
    }
    totals[numbers.lastIndex] = count
    println("The long primes up to 500 are:")
    println(longPrimes.take(totals[0]))

    println("\nThe number of long primes up to:")
    for ((i, total) in totals.withIndex()) {
        System.out.printf("  %5d is %d\n", numbers[i], total)
    }
}
```


{{output}}

```txt

The long primes up to 500 are:
[7, 17, 19, 23, 29, 47, 59, 61, 97, 109, 113, 131, 149, 167, 179, 181, 193, 223, 229, 233, 257, 263, 269, 313, 337, 367, 379, 383, 389, 419, 433, 461, 487, 491, 499]

The number of long primes up to:
    500 is 35
   1000 is 60
   2000 is 116
   4000 is 218
   8000 is 390
  16000 is 716
  32000 is 1300
  64000 is 2430

```



## M2000 Interpreter

{{trans|Go}}
Sieve leave to stack of values primes. This happen because we call the function as a module, so we pass the current stack (modules call modules passing own stack of values). We can place value to stack using Push to the top (as LIFO) or using Data to bottom (as FIFO). Variable Number read a number from stack and drop it.


```M2000 Interpreter

Module LongPrimes {
      Sieve=lambda (limit)->{
            Flush
            Buffer clear c as byte*limit+1
            \\ no need to process even numbers
            p=3
            do
                  p2=p^2
                  if p2>limit then exit
                  i=p2
                  while i<=limit
                        Return c, i:=1
                        i+=2*p
                  end While
                  do
                  p+=2
                  Until not eval(c,p)
            always
            for i = 3 to limit step 2
              if  eval(c,i) else data i
            next i
       }
      findPeriod=lambda (n) -> {
            r = 1
            for i = 1 to n+1 {r = (10 * r) mod n}
            rr = r : period = 0
            do
                    r = (10 * r) mod n
                    period++
                    if r == rr then exit
            always
            =period
      }
      Call sieve(64000)  ' leave stack with primes
      stops=(500,1000,2000,4000,8000,16000,32000,64000)
      acc=0
      stp=0
      limit=array(stops, stp)
      p=number  ' pop one
      Print "Long primes up to 500:"
      document lp500$
      for i=1 to 500
            if i=p then
                  if findPeriod(i)=i-1 then acc++ :lp500$=str$(i)
                  p=number
            end if
            if empty then exit for
      next i
      lp500$="]"
      insert 1,1 lp500$="["
      Print lp500$
      Print
      i=500
      Print "The number of long primes up to:"
      print i," is ";acc
      stp++     
      m=each(stops,1,-2)
      while m
            for i=array(m)+1 to array(m,m^+1)
                  if i=p then
                        if findPeriod(i)=i-1 then acc++
                        p=number
                  end if
                  if empty then exit for
            next i
            print array(m,m^+1)," is ";acc
      end While      
}
LongPrimes

```


{{out}}

```txt

The long primes up to 500 are: 
[7 17 19 23 29 47 59 61 97 109 113 131 149 167 179 181 193 223 229 233 257 263 269 313 337 367 379 383 389 419 433 461 487 491 499]

The number of long primes up to: 
    500 is 35
   1000 is 60
   2000 is 116
   4000 is 218
   8000 is 390
  16000 is 716
  32000 is 1300
  64000 is 2430
</pre >


## Pascal

first post.old program modified. Using Euler Phi 
   www . arndt-bruenner.de/mathe/scripts/periodenlaenge.htm


```pascal

PROGRAM Periode;

{$IFDEF FPC}
  {$MODE Delphi}
  
  {$OPTIMIZATION ON}
  {$OPTIMIZATION Regvar}
  {$OPTIMIZATION Peephole}
  {$OPTIMIZATION cse}
  {$OPTIMIZATION asmcse}
{$else}
  {$Apptype Console}
{$ENDIF}  

uses  
  sysutils;
  
const
  cBASIS = 10;
  PRIMFELDOBERGRENZE = 6542; 
  {Das sind alle Primzahlen bis 2^16}
  {Das reicht fuer al8le Primzahlen bis 2^32}
  TESTZAHL = 500;//429496709;//High(Dword) DIV cBasis;
type
   tPrimFeld = array [1..PRIMFELDOBERGRENZE] of Word;
   tFaktorPotenz = record
                     Faktor,
                     Potenz  : DWord;
                   end;
   //2*3*5*7*11*13*17*19*23  *29 > DWord also maximal 9 Faktoren
   tFaktorFeld =  array [1..9] of TFaktorPotenz;//DWord                           
// tFaktorFeld =  array [1..15] of TFaktorPotenz;//QWord
   tFaktorisieren = class(TObject)
                      private
                        fFakZahl   : DWord;
                        fFakBasis  : DWord;
                        fFakAnzahl : Dword;
                        fAnzahlMoeglicherTeiler : Dword;
                        fEulerPhi  : DWORD;
                        fStartPeriode : DWORD;
                        fPeriodenLaenge  : DWORD;
                        fTeiler    : array of DWord;
                        fFaktoren  : tFaktorFeld;
                        fBasFakt   : tFaktorFeld;
                        fPrimfeld  : tPrimFeld;

                        procedure PrimFeldAufbauen;
                        procedure Fakteinfuegen(var Zahl:Dword;inFak:Dword);
                        function  BasisPeriodeExtrahieren(var inZahl:Dword):DWord;
                        procedure NachkommaPeriode(var OutText: String);
                      public 
                        constructor create; overload;       
                        function  Prim(inZahl:Dword):Boolean;        
                        procedure AusgabeFaktorfeld(n : DWord);
                        procedure Faktorisierung (inZahl: DWord);
                        procedure TeilerErmitteln; 
                        procedure PeriodeErmitteln(inZahl:Dword);
                        function  BasExpMod( b, e, m : Dword) : DWord; 
                                               
                     property 
                        EulerPhi : Dword read fEulerPhi;
                     property    
                        PeriodenLaenge: DWord read fPeriodenLaenge ;
                     property    
                        StartPeriode: DWord read fStartPeriode ;
                    end;  
                    
constructor tFaktorisieren.create;
begin  
  inherited;
  PrimFeldAufbauen;
  
  fFakZahl  := 0;  
  fFakBasis := cBASIS;
  Faktorisierung(fFakBasis);
  fBasFakt := fFaktoren;  

  fFakZahl := 0;  
  fEulerPhi := 1;
  fPeriodenLaenge :=0;
  fFakZahl := 0;  
  fFakAnzahl := 0;
  fAnzahlMoeglicherTeiler := 0;
end;

function tFaktorisieren.Prim(inZahl:Dword):Boolean;
{Testet auf PrimZahl}
var
  Wurzel,
  pos       : Dword;
Begin
  if fFakZahl = inZahl then
    begin
    result := (fAnzahlMoeglicherTeiler = 2);
    exit;
    end;
  result := false;  
  if inZahl >1 then
    begin
    result := true;    
    Pos := 1;  
    Wurzel:= trunc(sqrt(inZahl));
    While fPrimFeld[Pos] <= Wurzel do
      begin
      if (inZahl mod fPrimFeld[Pos])=0 then
        begin
        result := false;
        break;
        end;
      inc(Pos);
      IF Pos > High(fPrimFeld) then
        break;
      end;
    end;  
end;

Procedure tFaktorisieren.PrimFeldAufbauen;
{Baut die Liste der Primzahlen bis Obergrenze auf}
const
  MAX = 65536;
var
  TestaufPrim,
  Zaehler,delta : Dword;
  
begin
Zaehler := 1;
fPrimFeld[Zaehler] := 2;
inc(Zaehler);
fPrimFeld[Zaehler] := 3;

delta := 2;
TestAufPrim:=5;
repeat
  if prim(TestAufPrim) then
    begin
    inc(Zaehler);
    fPrimFeld[Zaehler] := TestAufPrim;
    end;
  inc(TestAufPrim,delta);
  delta := 6-delta; // 2,4,2,4,2,4,2,
until (TestAufPrim>=MAX);

end; {PrimfeldAufbauen}


procedure tFaktorisieren.Fakteinfuegen(var Zahl:Dword;inFak:Dword);
var
  i : DWord;
begin
  inc(fFakAnzahl);
  with fFaktoren[fFakAnzahl] do
    begin
      fEulerPhi := fEulerPhi*(inFak-1);
    Faktor :=inFak;
    Potenz := 0;
    while (Zahl mod inFak) = 0 do
      begin
      Zahl := Zahl div inFak;
      inc(Potenz);
      end;
    For i := 2 to Potenz do
      fEulerPhi := fEulerPhi*inFak;
    end;
  fAnzahlMoeglicherTeiler:=fAnzahlMoeglicherTeiler*(1+fFaktoren[fFakAnzahl].Potenz);
end;

procedure tFaktorisieren.Faktorisierung (inZahl: DWord);
var
  j,
  og : longint;
begin
if fFakZahl = inZahl then
  exit;
  
fPeriodenLaenge := 0;  
fFakZahl   := inZahl;
fEulerPhi  := 1;
fFakAnzahl := 0;
fAnzahlMoeglicherTeiler :=1;
setlength(fTeiler,0);

If inZahl < 2 then
  exit;
og := round(sqrt(inZahl)+1.0);
{Suche Teiler von inZahl}
for j := 1 to High(fPrimfeld) do
  begin
  If fPrimfeld[j]> OG then
    Break;
  if (inZahl mod fPrimfeld[j])= 0 then
    Fakteinfuegen(inZahl,fPrimfeld[j]);
  end;
If inZahl>1 then
  Fakteinfuegen(inZahl,inZahl);
TeilerErmitteln;
end; {Faktorisierung}
  
procedure tFaktorisieren.AusgabeFaktorfeld(n : DWord);
var
  i :integer;
begin
  if fFakZahl <> n then
    Faktorisierung(n);
  write(fAnzahlMoeglicherTeiler:5,' Faktoren ');      

  For i := 1 to fFakAnzahl-1 do
    with fFaktoren[i] do
      IF potenz >1 then
        write(Faktor,'^',Potenz,'*')
      else
        write(Faktor,'*');
  with fFaktoren[fFakAnzahl] do
    IF potenz >1 then
      write(Faktor,'^',Potenz)
    else
      write(Faktor);

  writeln('  Euler Phi: ',fEulerPhi:12,PeriodenLaenge:12);      
end;

procedure tFaktorisieren.TeilerErmitteln;
var
  Position : DWord;
  i,j : DWord;
  procedure FaktorAufbauen(Faktor: DWord;n: DWord);
  var
    i,
    Pot : DWord;
  begin
    Pot := 1;  
    i := 0;
    repeat
      IF n > Low(fFaktoren) then
        FaktorAufbauen(Pot*Faktor,n-1)
      else  
        begin
        FTeiler[Position] := Pot*Faktor;
        inc(Position);
        end;
      Pot := Pot*fFaktoren[n].Faktor;   
      inc(i);
    until  i > fFaktoren[n].Potenz;
  end;
  
begin
  Position:= 0;
  setlength(FTeiler,fAnzahlMoeglicherTeiler);
  FaktorAufbauen(1,fFakAnzahl);
  //Sortieren
  For i := Low(fTeiler) to fAnzahlMoeglicherTeiler-2 do
    begin
    j := i;
    while (j>=Low(fTeiler)) AND (fTeiler[j]>fTeiler[j+1]) do
      begin
      Position := fTeiler[j];
      fTeiler[j] := fTeiler[j+1];
      fTeiler[j+1]:= Position;    
      dec(j);
      end;
    end;
end;

function tFaktorisieren.BasisPeriodeExtrahieren(var inZahl:Dword):DWord;
var
 i,cnt,
 Teiler: Dword;
begin
  cnt := 0;
  result := 0;
  For i := Low(fBasFakt) to High(fBasFakt) do
    begin
    with fBasFakt[i] do 
      begin
      IF Faktor = 0 then
        BREAK;
      Teiler := Faktor;
      For cnt := 2 to Potenz do
        Teiler := Teiler*Faktor;
      end;   
    cnt := 0;  
    while (inZahl<> 0) AND (inZahl mod Teiler = 0) do 
      begin
      inZahl := inZahl div Teiler;
      inc(cnt);
      end;
    IF cnt > result then
      result := cnt;
    end;
end;

procedure tFaktorisieren.PeriodeErmitteln(inZahl:Dword);
var
  i,
  TempZahl,
  TempPhi,
  TempPer,
  TempBasPer: DWord;
begin
  Faktorisierung(inZahl);
  TempZahl := inZahl;
  //Die Basis_Nicht_Periode ermitteln
  TempBasPer := BasisPeriodeExtrahieren(TempZahl);
  TempPer := 0;
  IF TempZahl >1 then
    begin
    Faktorisierung(TempZahl);
    TempPhi := fEulerPhi;
    IF (TempPhi > 1) then
      begin
      Faktorisierung(TempPhi);
      i := 0;
      repeat
        TempPer := fTeiler[i];
        IF BasExpMod(fFakBasis,TempPer,TempZahl)= 1 then
          Break;
        inc(i);
      until i >= Length(fTeiler);
      IF i >= Length(fTeiler) then
        TempPer := inZahl-1;
      end;
    end;

  Faktorisierung(inZahl);
  fPeriodenlaenge := TempPer;
  fStartPeriode   := TempBasPer;  
end;

procedure tFaktorisieren.NachkommaPeriode(var OutText: String);
var
  i,
  limit : integer;
  
  Rest,
  Rest1,
  Divi,
  basis: DWord;
  pText : pChar;
  
  procedure Ziffernfolge(Ende: longint);
  var 
    j : longint;
  begin
    j := i-Ende;
    
    while j < 0 do
      begin
      Rest := Rest *Basis;      
      Rest1:= Rest Div Divi;
      Rest := Rest-Rest1*Divi;//== Rest1 Mod Divi     
    
      pText^ := chr(Rest1+Ord('0'));
      inc(pText);

      inc(j);
	  end;
	
	i := Ende;
  end;
  
begin
  limit:= fStartPeriode+fPeriodenlaenge;
    
  setlength(OutText,limit+2+2+5);
  OutText[1]:='0';
  OutText[2]:='.';
  pText := @OutText[3];

  Rest := 1;
  Divi := fFakZahl; 
  Basis := fFakBasis;
  
  i := 0;
  Ziffernfolge(fStartPeriode);
  if fPeriodenlaenge = 0 then 
    begin
    setlength(OutText,fStartPeriode+2);
    EXIT;
    end;

  pText^ := '_'; inc(pText);
  Ziffernfolge(limit);
  pText^ := '_'; inc(pText);

  Ziffernfolge(limit+5);
end;  

type 
   tZahl   = integer;
   tRestFeld = array[0..31] of integer;

VAR
    F : tFaktorisieren;
  
function tFaktorisieren.BasExpMod( b, e, m : Dword) : DWord; 
begin 
  Result := 1; 
  IF m = 0 then
    exit;
  Result := 1; 
  while ( e > 0 ) do 
    begin 
    if (e AND 1) <> 0 then 
      Result :=  (Result * int64(b)) mod m; 
    b := (int64(b) * b ) mod m; 
    e := e shr 1; 
    end; 
end;

procedure start;
VAR
    Limit,    
    Testzahl : DWord;
    longPrimCount  : int64;
    t1,t0: TDateTime;
BEGIN

  Limit := 500;
  Testzahl := 2;
  longPrimCount := 0;
  t0 := time;

  repeat
    write(Limit:8,': ');
    repeat 
      if F.Prim(Testzahl) then
      begin
        F.PeriodeErmitteln(Testzahl);
        if F.PeriodenLaenge = Testzahl-1 then
        Begin
          inc(longPrimCount);
          IF Limit = 500 then
            write(TestZahl,',');
        end    
      end;
      inc(Testzahl);
    until TestZahl = Limit;
    inc(Limit,Limit);
    write('  .. count ',longPrimCount:8,' ');
    t1:= time;
    If (t1-t0)>1/864000 then
       write(FormatDateTime('HH:NN:SS.ZZZ',t1-T0));
    writeln;   
 until Limit > 10*1000*1000;
    
t1 := time;
writeln;
writeln('count of long primes ',longPrimCount);
writeln('Benoetigte Zeit ',FormatDateTime('HH:NN:SS.ZZZ',T1-T0));
 
END;

BEGIN
  F := tFaktorisieren.create;
  writeln('Start');
  start;
  writeln('Fertig.');
  F.free;  
  readln;
end.
```

{{out}}

```txt
sh-4.4# ./Periode
Start
     500: 7,17,19,23,29,47,59,61,97,109,113,131,149,167,179,181,193,223,229,233,257,263,269,313,337,367,379,383,389,419,433,461,487,491,499,  .. count       35 
    1000:   .. count       60 
    2000:   .. count      116 
    4000:   .. count      218 
    8000:   .. count      390 
   16000:   .. count      716 
   32000:   .. count     1300 
   64000:   .. count     2430 
  128000:   .. count     4498 
  256000:   .. count     8434 00:00:00.100
  512000:   .. count    15920 00:00:00.220
 1024000:   .. count    30171 00:00:00.494
 2048000:   .. count    57115 00:00:01.140
 4096000:   .. count   108381 00:00:02.578
 8192000:   .. count   206594 00:00:06.073

count of long primes 206594
Benoetigte Zeit 00:00:06.073
Fertig.

```



## Perl

{{trans|Sidef}}
{{libheader|ntheory}}

```perl
use ntheory qw/divisors powmod is_prime/;

sub is_long_prime {
    my($p) = @_;
    return 0 unless is_prime($p);
    for my $d (divisors($p-1)) {
        return $d+1 == $p if powmod(10, $d, $p)== 1;
    }
    0;
}

print "Long primes ≤ 500:\n";
print join(' ', grep {is_long_prime($_) } 1 .. 500), "\n\n";

for $n (500, 1000, 2000, 4000, 8000, 16000, 32000, 64000) {
    printf "Number of long primes ≤ $n: %d\n",  scalar grep { is_long_prime($_) } 1 .. $n;
}
```

{{out}}

```txt
Long primes ≤ 500:
7 17 19 23 29 47 59 61 97 109 113 131 149 167 179 181 193 223 229 233 257 263 269 313 337 367 379 383 389 419 433 461 487 491 499

Number of long primes ≤ 500: 35
Number of long primes ≤ 1000: 60
Number of long primes ≤ 2000: 116
Number of long primes ≤ 4000: 218
Number of long primes ≤ 8000: 390
Number of long primes ≤ 16000: 716
Number of long primes ≤ 32000: 1300
Number of long primes ≤ 64000: 2430
```



###  Using znorder 


Faster due to going directly over primes and using znorder.  Takes one second to count to 8,192,000.

```perl
use ntheory qw/forprimes znorder/;
my($t,$z)=(0,0);
forprimes {
  $z = znorder(10, $_);
  $t++ if defined $z && $z+1 == $_;
} 8192000;
print "$t\n";
```

{{out}}

```txt
206594
```



## Perl 6

{{works with|Rakudo|2018.06}}
Not very fast as the numbers get larger.

```perl6
use Math::Primesieve;
my $sieve = Math::Primesieve.new;

sub is-long (Int $p) {
    my $r = 1;
    my $rr = $r = (10 * $r) % $p for ^$p;
    my $period;
    loop {
        $r = (10 * $r) % $p;
        ++$period;
        last if $period >= $p or $r == $rr;
    }
    $period == $p - 1 and $p > 2;
}

my @primes = $sieve.primes(500);
my @long-primes = @primes.grep: {.&is-long};

put "Long primes ≤ 500:\n", @long-primes;

@long-primes = ();

for 500, 1000, 2000, 4000, 8000, 16000, 32000, 64000 -> $upto {
    state $from = 0;
    my @extend = $sieve.primes($from, $upto);
    @long-primes.append: @extend.hyper(:8degree).grep: {.&is-long};
    say "\nNumber of long primes ≤ $upto: ", +@long-primes;
    $from = $upto;
}
```

{{out}}

```txt
Long primes ≤ 500:
7 17 19 23 29 47 59 61 97 109 113 131 149 167 179 181 193 223 229 233 257 263 269 313 337 367 379 383 389 419 433 461 487 491 499

Number of long primes ≤ 500: 35

Number of long primes ≤ 1000: 60

Number of long primes ≤ 2000: 116

Number of long primes ≤ 4000: 218

Number of long primes ≤ 8000: 390

Number of long primes ≤ 16000: 716

Number of long primes ≤ 32000: 1300

Number of long primes ≤ 64000: 2430
```



## Phix

Using primes and add_block from [[Extensible_prime_generator#Phix]]

```Phix
function is_long_prime(integer n)
    integer r = 1, rr, period = 0
    for i=1 to n+1 do
        r = mod(10*r,n)
    end for
    rr = r
    while true do
        r = mod(10*r,n)
        period += 1
        if period>=n then return false end if
        if r=rr then exit end if
    end while
    return period=n-1
end function
```

(use the same main() as below but limit maxN to 8 iterations)

Much faster version:

```Phix
function is_long_prime(integer n)
    sequence f = factors(n-1,1)
    integer count = 0
    for i=1 to length(f) do
        integer fi = f[i], e=1, base=10
        while fi!=0 do
            if mod(fi,2)=1 then
                e = mod(e*base,n)
            end if
            base = mod(base*base,n)
            fi = floor(fi/2)
        end while
        if e=1 then
            count += 1
            if count>1 then exit end if
        end if
    end for
    return count=1
end function

procedure main()
atom t0 = time()
integer maxN = 500*power(2,14)--8)
    while primes[$]<maxN do
        add_block()
    end while
    sequence long_primes = {}
    integer count = 0,
            n = 500
    for i=2 to length(primes) do    -- (skip 2)
        integer prime = primes[i]
        if is_long_prime(prime) then
            if prime<500 then
                long_primes &= prime
            end if      
            if prime>n then
                if n=500 then
                    printf(1,"The long primes up to 500 are:\n")
                    ?long_primes
                    printf(1,"\nThe number of long primes up to:\n")
                end if
                printf(1,"  %7d is %d  (%s)\n", {n, count, elapsed(time()-t0)})
                if n=maxN then exit end if
                n *= 2
            end if
            count += 1
        end if
    end for
end procedure
main()
```

{{out}}
slow version:

```txt

The long primes up to 500 are:
{7,17,19,23,29,47,59,61,97,109,113,131,149,167,179,181,193,223,229,233,257,263,269,313,337,367,379,383,389,419,433,461,487,491,499}

The number of long primes up to:
      500 is 35  (0.2s)
     1000 is 60  (0.3s)
     2000 is 116  (0.3s)
     4000 is 218  (0.6s)
     8000 is 390  (1.5s)
    16000 is 716  (4.9s)
    32000 is 1300  (16.6s)
    64000 is 2430  (1 minute and 01s)

```

fast version:

```txt

The long primes up to 500 are:
{7,17,19,23,29,47,59,61,97,109,113,131,149,167,179,181,193,223,229,233,257,263,269,313,337,367,379,383,389,419,433,461,487,491,499}

The number of long primes up to:
      500 is 35  (0.2s)
     1000 is 60  (0.2s)
     2000 is 116  (0.2s)
     4000 is 218  (0.2s)
     8000 is 390  (0.2s)
    16000 is 716  (0.3s)
    32000 is 1300  (0.4s)
    64000 is 2430  (0.7s)
   128000 is 4498  (1.3s)
   256000 is 8434  (2.8s)
   512000 is 15920  (6.0s)
  1024000 is 30171  (13.1s)
  2048000 is 57115  (28.2s)
  4096000 is 108381  (1 minute and 01s)
  8192000 is 206594  (2 minutes and 11s)

```



## Python

{{trans|Kotlin}}

```python
def sieve(limit):
    primes = []
    c = [False] * (limit + 1) # composite = true
    # no need to process even numbers
    p = 3
    while True:
        p2 = p * p
        if p2 > limit: break
        for i in range(p2, limit, 2 * p): c[i] = True
        while True:
            p += 2
            if not c[p]: break

    for i in range(3, limit, 2):
        if not c[i]: primes.append(i)
    return primes

# finds the period of the reciprocal of n
def findPeriod(n):
    r = 1
    for i in range(1, n): r = (10 * r) % n
    rr = r
    period = 0
    while True:
        r = (10 * r) % n
        period += 1
        if r == rr: break
    return period

primes = sieve(64000)
longPrimes = []
for prime in primes:
    if findPeriod(prime) == prime - 1:
        longPrimes.append(prime)
numbers = [500, 1000, 2000, 4000, 8000, 16000, 32000, 64000]
count = 0
index = 0
totals = [0] * len(numbers)
for longPrime in longPrimes:
    if longPrime > numbers[index]:
        totals[index] = count
        index += 1
    count += 1
totals[-1] = count
print('The long primes up to 500 are:')
print(str(longPrimes[:totals[0]]).replace(',', ''))
print('\nThe number of long primes up to:')
for (i, total) in enumerate(totals):
    print('  %5d is %d' % (numbers[i], total))
```

{{out}}

```txt

The long primes up to 500 are:
[7 17 19 23 29 47 59 61 97 109 113 131 149 167 179 181 193 223 229 233 257 263 269 313 337 367 379 383 389 419 433 461 487 491 499]

The number of long primes up to:
    500 is 35
   1000 is 60
   2000 is 116
   4000 is 218
   8000 is 390
  16000 is 716
  32000 is 1300
  64000 is 2430

```



## Racket


{{trans|Go}} (at least '''find-period''')


```racket
#lang racket
(require math/number-theory)

(define (find-period n)
  (let ((rr (for/fold ((r 1))
                      ((i (in-range 1 (+ n 2))))
              (modulo (* 10 r) n))))
    (let period-loop ((r rr) (p 1))
      (let ((r′ (modulo (* 10 r) n)))
        (if (= r′ rr) p (period-loop r′ (add1 p)))))))

(define (long-prime? n)
  (and (prime? n) (= (find-period n) (sub1 n))))

(define memoised-long-prime? (let ((h# (make-hash))) (λ (n) (hash-ref! h# n (λ () (long-prime? n))))))

(module+ main
  ;; strictly, won't test 500 itself... but does it look prime to you?
  (filter memoised-long-prime? (range 7 500 2))
  (for-each
   (λ (n) (displayln (cons n (for/sum ((i (in-range 7 n 2))) (if (memoised-long-prime? i) 1 0)))))
   '(500 1000 2000 4000 8000 16000 32000 64000)))

(module+ test
  (require rackunit)
  (check-equal? (map find-period '(7 11 977)) '(6 2 976)))
```


{{out}}

```txt
'(7 17 19 23 29 47 59 61 97 109 113 131 149 167 179 181 193 223 229 233 257 263 269 313 337 367 379 383 389 419 433 461 487 491 499)
(500 . 35)
(1000 . 60)
(2000 . 116)
(4000 . 218)
(8000 . 390)
(16000 . 716)
(32000 . 1300)
(64000 . 2430)
```



## REXX

<!--   The three versions of REXX programs have been re-run and they produce the correct results.  
       I had re-wrote the   .len   function to use a faster method, and the re-write was correct; 
       the older function had returned incorrect results where the (smaller) precision  (decimal
       digits)  was yielding incorrect results for integers greater than a certain amount,  and I 
       didn't update the corrected output for 32,000.   !-->  
For every   '''doubling'''   of the limit, it takes about roughly   '''5'''   times longer to compute the long primes.

### uses odd numbers


```rexx
/*REXX pgm calculates/displays base ten  long primes  (AKA golden primes, proper primes,*/
/*───────────────────── maximal period primes, long period primes, full reptend primes).*/
parse arg a                                      /*obtain optional argument from the CL.*/
if a='' | a=","  then a= '500 -500 -1000 -2000 -4000 -8000 -16000' ,  /*Not specified?  */
                         '-32000 -64000 -128000 -512000 -1024000'     /*Then use default*/
    do k=1  for words(a);     H=word(a, k)       /*step through the list of high limits.*/
    neg= H<1                                     /*used as an indicator to display count*/
    H= abs(H)                                    /*obtain the absolute value of  H.     */
    $=                                           /*the list of  long primes   (so far). */
       do j=7  to H  by 2                        /*start with 7,  just use odd integers.*/
       if .len(j) + 1 \== j  then iterate        /*Period length wrong?   Then skip it. */
       $=$ j                                     /*add the   long prime   to the $ list.*/
       end   /*j*/
    say
    if neg  then do;  say 'number of long primes ≤ '    H     " is: "     words($);    end
            else do;  say   'list of long primes ≤ '    H":";         say strip($);    end
    end      /*k*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
.len: procedure; parse arg x;  r=1;   do x;                   r= 10*r // x;     end  /*x*/
                              rr=r;   do p=1  until r==rr;    r= 10*r // x;     end  /*p*/
      return p
```

{{out|output|text=  when using the internal default inputs:}}

```txt

list of long primes ≤  500:
7 17 19 23 29 47 59 61 97 109 113 131 149 167 179 181 193 223 229 233 257 263 269 313 337 367 379 383 389 419 433 461 487 491 499

number of long primes ≤  500  is:  35

number of long primes ≤  1000  is:  60

number of long primes ≤  2000  is:  116

number of long primes ≤  4000  is:  218

number of long primes ≤  8000  is:  390

number of long primes ≤  16000  is:  716

number of long primes ≤  32000  is:  1300

number of long primes ≤  64000  is:  2430

number of long primes ≤  128000  is:  4498

number of long primes ≤  512000  is:  15920

number of long primes ≤  1024000  is:  30171

```


===uses odd numbers, some prime tests===
This REXX version is about   '''2'''   times faster than the 1<sup>st</sup> REXX version   (because it does some primality testing).

```rexx
/*REXX pgm calculates/displays base ten  long primes  (AKA golden primes, proper primes,*/
/*───────────────────── maximal period primes, long period primes, full reptend primes).*/
parse arg a                                      /*obtain optional argument from the CL.*/
if a='' | a=","  then a= '500 -500 -1000 -2000 -4000 -8000 -16000' ,  /*Not specified?  */
                         '-32000 -64000 -128000 -512000 -1024000'     /*Then use default*/
    do k=1  for words(a);     H=word(a, k)       /*step through the list of high limits.*/
    neg= H<1                                     /*used as an indicator to display count*/
    H= abs(H)                                    /*obtain the absolute value of  H.     */
    $=                                           /*the list of  long primes   (so far). */
       do j=7  to H  by 2;  parse var j '' -1 _  /*start with 7,  just use odd integers.*/
                       if     _==5  then iterate /*last digit a five?  Then not a prime.*/
                       if j// 3==0  then iterate /*Is divisible by  3?   "   "  "   "   */
       if j\==11  then if j//11==0  then iterate /* "     "      " 11?   "   "  "   "   */
       if j\==13  then if j//13==0  then iterate /* "     "      " 13?   "   "  "   "   */
       if j\==17  then if j//17==0  then iterate /* "     "      " 17?   "   "  "   "   */
       if j\==19  then if j//19==0  then iterate /* "     "      " 19?   "   "  "   "   */
       if .len(j) + 1 \== j  then iterate        /*Period length wrong?   Then skip it. */
       $=$ j                                     /*add the   long prime   to the $ list.*/
       end   /*j*/
    say
    if neg  then do;  say 'number of long primes ≤ '    H     " is: "     words($);    end
            else do;  say   'list of long primes ≤ '    H":";         say strip($);    end
    end      /*k*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
.len: procedure; parse arg x;  r=1;   do x;                   r= 10*r // x;     end  /*x*/
                              rr=r;   do p=1  until r==rr;    r= 10*r // x;     end  /*p*/
      return p
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}


### uses primes

This REXX version is about   '''5'''   times faster than the 1<sup>st</sup> REXX version   (because it only tests primes).

```rexx
/*REXX pgm calculates/displays base ten  long primes  (AKA golden primes, proper primes,*/
/*───────────────────── maximal period primes, long period primes, full reptend primes).*/
parse arg a                                      /*obtain optional argument from the CL.*/
if a='' | a=","  then a= '500 -500 -1000 -2000 -4000 -8000 -16000' ,  /*Not specified?  */
                         '-32000 -64000 -128000 -512000 -1024000'     /*Then use default*/
m=0;            aa=words(a)                      /* [↑]  two list types of low primes.  */
    do j=1  for aa;   m= max(m, abs(word(a, j))) /*find the maximum argument in the list*/
    end   /*j*/
call genP                                        /*go and generate some primes.         */
    do k=1  for aa;           H=word(a, k)       /*step through the list of high limits.*/
    neg= H<1                                     /*used as an indicator to display count*/
    H= abs(H)                                    /*obtain the absolute value of  H.     */
    $=                                           /*the list of  long primes   (so far). */
       do j=7  to H  by 2
       if \@.j               then iterate        /*Is  J  not a prime?    Then skip it. */
       if .len(j) + 1 \== j  then iterate        /*Period length wrong?     "    "   "  */
       $=$ j                                     /*add the   long prime   to the $ list.*/
       end   /*j*/                               /* [↑]  some pretty weak prime testing.*/
    say
    if neg  then do;  say 'number of long primes ≤ '    H     " is: "     words($);    end
            else do;  say   'list of long primes ≤ '    H":";         say strip($);    end
    end      /*k*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
genP: @.=0; @.2=1; @.3=1; @.5=1; @.7=1; @.11=1;   !.=0; !.1=2; !.2=3; !.3=5; !.4=7; !.5=11
      #=5                                        /*the number of primes  (so far).      */
          do g=!.#+2  by 2  until #>=m           /*gen enough primes to satisfy max  A. */
                do d=2  until !.d**2 > g         /*only divide up to square root of  X. */
                if g // !.d == 0  then iterate g /*Divisible?   Then skip this integer. */
                end   /*d*/                      /* [↓]  a spanking new prime was found.*/
          #=#+1;  @.g=1;  !.#=g                  /*bump P counter; assign P, add to P's.*/
          end         /*g*/
      return
/*──────────────────────────────────────────────────────────────────────────────────────*/
.len: procedure; parse arg x;  r=1;   do x;                   r= 10*r // x;     end  /*x*/
                              rr=r;   do p=1  until r==rr;    r= 10*r // x;     end  /*p*/
      return p
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}} 




## Ruby


Finding long prime numbers using finding period location (translation of Python's module def findPeriod(n))

```Ruby

require 'prime'

batas = 64_000 # limit number
start = Time.now # time of starting
lp_array = [] # array of long-prime numbers

def find_period(n)
  r, period = 1, 0
  (1...n).each {r = (10 * r) % n}
  rr = r
  loop do 
    r = (10 * r) % n
    period += 1
    break if r == rr
  end
  return period
end

Prime.each(batas).each do |prime|
  lp_array.push(prime) if find_period(prime) == prime-1 && prime != 2
end

[500, 1000, 2000, 4000, 8000, 16000, 32000, 64000].each do |s|
  if s == 500
    puts "\nAll long primes up to  #{s} are: #{lp_array.count {|x| x < s}}. They are:"
    lp_array.each {|x| print x, " " if x < s}
  else
    print "\nAll long primes up to #{s} are: #{lp_array.count {|x| x < s}}"
  end
end

puts "\n\nTime: #{Time.now - start}"

```


{{out}}

```txt

All long primes up to  500 are: 35. They are:
7 17 19 23 29 47 59 61 97 109 113 131 149 167 179 181 193 223 229 233 257 263 269 313 337 367 379 383 389 419 433 461 487 491 499
All long primes up to 1000 are: 60
All long primes up to 2000 are: 116
All long primes up to 4000 are: 218
All long primes up to 8000 are: 390
All long primes up to 16000 are: 716
All long primes up to 32000 are: 1300
All long primes up to 64000 are: 2430

Time: 27.085963

```


Alternatively, by using primitive way: converting value into string and make assessment for proper repetend position. Indeed produce same information, but with longer time.

```Ruby

require 'prime'
require 'bigdecimal'
require 'strscan'

batas = 64_000 # limit number
start = Time.now # time of starting
lp_array = [] # array of long-prime numbers
a = BigDecimal.new(1) # number being divided, that is 1.

Prime.each(batas).each do |prime|
  cek = a.div(prime, (prime-1)*2).truncate((prime-1)*2).to_s('F')[2..-1] # Deviding 1 with prime and take its value as string.
  if (cek[0, prime-1] == cek[prime-1, prime-1])
    i = prime-2
    until i < 5
      break if cek[0, i] == cek[i, i]
      i-=1
      cek.slice!(-2, 2) # Shortening checked string to reduce checking process load
    end
    
    until i == 0
      break if cek[0, (cek.size/i)*i].scan(/.{#{i}}/).uniq.length == 1
      i-=1
    end

    lp_array.push(prime) if i == 0
  end
end

[500, 1000, 2000, 4000, 8000, 16000, 32000, 64000].each do |s|
  if s == 500
    puts "\nAll long primes up to  #{s} are: #{lp_array.count {|x| x < s}}. They are:"
    lp_array.each {|x| print x, " " if x < s}
  else
    print "\nAll long primes up to #{s} are: #{lp_array.count {|x| x < s}}"
  end
end

puts "\n\nTime: #{Time.now - start}"


```


{{out}}

```txt

(same output with previous version, but longer time elapse)

Time: 1599.787375

```



## Sidef

The smallest divisor d of p-1 such that 10^d = 1 (mod p), is the length of the period of the decimal expansion of 1/p.

```ruby
func is_long_prime(p) {

    for d in (divisors(p-1)) {
        if (powmod(10, d, p) == 1) {
            return (d+1 == p)
        }
    }

    return false
}

say "Long primes ≤ 500:"
say primes(500).grep(is_long_prime).join(' ')

for n in ([500, 1000, 2000, 4000, 8000, 16000, 32000, 64000]) {
    say ("Number of long primes ≤ #{n}: ", primes(n).count_by(is_long_prime))
}
```

{{out}}

```txt

Long primes ≤ 500:
7 17 19 23 29 47 59 61 97 109 113 131 149 167 179 181 193 223 229 233 257 263 269 313 337 367 379 383 389 419 433 461 487 491 499
Number of long primes ≤ 500: 35
Number of long primes ≤ 1000: 60
Number of long primes ≤ 2000: 116
Number of long primes ≤ 4000: 218
Number of long primes ≤ 8000: 390
Number of long primes ≤ 16000: 716
Number of long primes ≤ 32000: 1300
Number of long primes ≤ 64000: 2430

```


Alternatively, we can implement the ''is_long_prime(p)'' function using the `znorder(a, p)` built-in method, which is considerably faster:


```ruby
func is_long_prime(p) {
    znorder(10, p) == p-1
}
```



## zkl

Using GMP (GNU Multiple Precision Arithmetic Library, probabilistic
primes), because it is easy and fast to generate primes.

```zkl
var [const] BN=Import("zklBigNum");  // libGMP
primes,p := List.createLong(7_000), BN(3);  // one big alloc vs lots of allocs
while(p.nextPrime()<=64_000){ primes.append(p.toInt()) } // 6412 of them, skipped 2
primes.append(p.toInt());	// and one more so tail prime is >64_000

longPrimes:=primes.filter(fcn(p){ findPeriod(p)==p-1 }); // yawn
fcn findPeriod(n){
   r,period := 1,0;
   do(n){ r=(10*r)%n }
   rr:=r;
   while(True){   // reduce is more concise but 2.5 times slower
      r=(10*r)%n;
      period+=1;
      if(r==rr) break;
   }
   period
}
```



```zkl
fiveHundred:=longPrimes.filter('<(500));
println("The long primes up to 500 are:\n",longPrimes.filter('<(500)).concat(","));

println("\nThe number of long primes up to:");
foreach n in (T(500, 1000, 2000, 4000, 8000, 16000, 32000, 64000)){
   println("  %5d is %d".fmt( n, longPrimes.filter1n('>(n)) ));
}
```

{{out}}

```txt

The long primes up to 500 are:
7,17,19,23,29,47,59,61,97,109,113,131,149,167,179,181,193,223,229,233,257,263,269,313,337,367,379,383,389,419,433,461,487,491,499

The number of long primes up to:
    500 is 35
   1000 is 60
   2000 is 116
   4000 is 218
   8000 is 390
  16000 is 716
  32000 is 1300
  64000 is 2430

```


== Headline text ==
