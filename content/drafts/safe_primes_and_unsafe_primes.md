+++
title = "Safe primes and unsafe primes"
description = ""
date = 2019-09-12T17:49:06Z
aliases = []
[extra]
id = 22006
[taxonomies]
categories = []
tags = []
+++

{{task|Prime Numbers}}

;Definitions:
::*   A   '''safe prime'''   is a prime   '''p'''   and where   '''(p-1)/2'''   is also prime.
::*   The corresponding prime  '''(p-1)/2'''   is known as a   '''Sophie Germain'''   prime.
::*   An   '''unsafe prime'''   is a prime   '''p'''   and where   '''(p-1)/2'''   ''isn't''   a prime.
::*   An   '''unsafe prime'''   is a prime that   ''isn't''   a   '''safe'''   prime.


;Task:
::*   Find and display (on one line) the first   '''35'''   safe primes.
::*   Find and display the   ''count''   of the safe primes below   1,000,000.
::*   Find and display the   ''count''   of the safe primes below 10,000,000.
::*   Find and display (on one line) the first   '''40'''   unsafe primes.
::*   Find and display the   ''count''   of the unsafe primes below   1,000,000.
::*   Find and display the   ''count''   of the unsafe primes below 10,000,000.
::*   (Optional)   display the   ''counts''   and   "below numbers"   with commas.

Show all output here.


;Related Task:
::*   [[Strong_and_weak_primes|strong and weak primes]].


;Also see:
::*   The OEIS article:   [http://oeis.org/A005385   safe   primes].
::*   The OEIS article:   [http://oeis.org/A059456 unsafe primes].





## C sharp

{{works with|C sharp|7}}

```csharp
using static System.Console;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

public static class SafePrimes
{
    public static void Main() {
        HashSet<int> primes = Primes(10_000_000).ToHashSet();
        WriteLine("First 35 safe primes:");
        WriteLine(string.Join(" ", primes.Where(IsSafe).Take(35)));
        WriteLine($"There are {primes.TakeWhile(p => p < 1_000_000).Count(IsSafe):n0} safe primes below {1_000_000:n0}");
        WriteLine($"There are {primes.TakeWhile(p => p < 10_000_000).Count(IsSafe):n0} safe primes below {10_000_000:n0}");
        WriteLine("First 40 unsafe primes:");
        WriteLine(string.Join(" ", primes.Where(IsUnsafe).Take(40)));
        WriteLine($"There are {primes.TakeWhile(p => p < 1_000_000).Count(IsUnsafe):n0} unsafe primes below {1_000_000:n0}");
        WriteLine($"There are {primes.TakeWhile(p => p < 10_000_000).Count(IsUnsafe):n0} unsafe primes below {10_000_000:n0}");

        bool IsSafe(int prime) => primes.Contains(prime / 2);
        bool IsUnsafe(int prime) => !primes.Contains(prime / 2);
    }

    //Method from maths library
    static IEnumerable<int> Primes(int bound) {
        if (bound < 2) yield break;
        yield return 2;

        BitArray composite = new BitArray((bound - 1) / 2);
        int limit = ((int)(Math.Sqrt(bound)) - 1) / 2;
        for (int i = 0; i < limit; i++) {
            if (composite[i]) continue;
            int prime = 2 * i + 3;
            yield return prime;
            for (int j = (prime * prime - 2) / 2; j < composite.Count; j += prime) composite[j] = true;
        }
        for (int i = limit; i < composite.Count; i++) {
            if (!composite[i]) yield return 2 * i + 3;
        }
    }

}
```

{{out}}

```txt

First 35 safe primes:
5 7 11 23 47 59 83 107 167 179 227 263 347 359 383 467 479 503 563 587 719 839 863 887 983 1019 1187 1283 1307 1319 1367 1439 1487 1523 1619
There are 4,324 safe primes below 1,000,000
There are 30,657 safe primes below 10,000,000
First 40 unsafe primes:
2 3 13 17 19 29 31 37 41 43 53 61 67 71 73 79 89 97 101 103 109 113 127 131 137 139 149 151 157 163 173 181 191 193 197 199 211 223 229 233
There are 74,174 unsafe primes below 1,000,000
There are 633,922 unsafe primes below 10,000,000
```


=={{header|F_Sharp|F#}}==
This task uses [http://www.rosettacode.org/wiki/Extensible_prime_generator#The_function Extensible Prime Generator (F#)]
```fsharp

pCache |> Seq.filter(fun n->isPrime((n-1)/2)) |> Seq.take 35 |> Seq.iter (printf "%d ")

```

{{out}}

```txt

5 7 11 23 47 59 83 107 167 179 227 263 347 359 383 467 479 503 563 587 719 839 863 887 983 1019 1187 1283 1307 1319 1367 1439 1487 1523 1619

```


```fsharp

printfn "There are %d safe primes less than 1000000" (pCache |> Seq.takeWhile(fun n->n<1000000) |> Seq.filter(fun n->isPrime((n-1)/2)) |> Seq.length)

```

{{out}}

```txt

There are 4324 safe primes less than 10000000

```


```fsharp

printfn "There are %d safe primes less than 10000000" (pCache |> Seq.takeWhile(fun n->n<10000000) |> Seq.filter(fun n->isPrime((n-1)/2)) |> Seq.length)

```

{{out}}

```txt

There are 30657 safe primes less than 10000000

```


```fsharp

pCache |> Seq.filter(fun n->not (isPrime((n-1)/2))) |> Seq.take 40 |> Seq.iter (printf "%d ")

```

{{out}}

```txt

2 3 13 17 19 29 31 37 41 43 53 61 67 71 73 79 89 97 101 103 109 113 127 131 137 139 149 151 157 163 173 181 191 193 197 199 211 223 229 233

```


```fsharp

printfn "There are %d unsafe primes less than 1000000" (pCache |> Seq.takeWhile(fun n->n<1000000) |> Seq.filter(fun n->not (isPrime((n-1)/2))) |> Seq.length);;

```

{{out}}

```txt

There are 74174 unsafe primes less than 1000000

```


```fsharp

printfn "There are %d unsafe primes less than 10000000" (pCache |> Seq.takeWhile(fun n->n<10000000) |> Seq.filter(fun n->not (isPrime((n-1)/2))) |> Seq.length);;

```

{{out}}

```txt

There are 633922 unsafe primes less than 10000000

```



## Factor

Much like the Perl 6 example, this program uses an in-built primes generator to efficiently obtain the first ten million primes. If memory is a concern, it wouldn't be unreasonable to perform primality tests on the (odd) numbers below ten million, however.

```factor
USING: fry interpolate kernel literals math math.primes
sequences tools.memory.private ;
IN: rosetta-code.safe-primes

CONSTANT: primes $[ 10,000,000 primes-upto ]

: safe/unsafe ( -- safe unsafe )
    primes [ 1 - 2/ prime? ] partition ;

: count< ( seq n -- str ) '[ _ < ] count commas ;

: seq>commas ( seq -- str ) [ commas ] map " " join ;

: stats ( seq n -- head count1 count2 )
    '[ _ head seq>commas ] [ 1e6 count< ] [ 1e7 count< ] tri ;

safe/unsafe [ 35 ] [ 40 ] bi* [ stats ] 2bi@

[I
First 35 safe primes:
${5}
Safe prime count below  1,000,000: ${4}
Safe prime count below 10,000,000: ${3}

First 40 unsafe primes:
${2}
Unsafe prime count below  1,000,000: ${1}
Unsafe prime count below 10,000,000: ${}
I]
```

{{out}}

```txt

First 35 safe primes:
5 7 11 23 47 59 83 107 167 179 227 263 347 359 383 467 479 503 563 587 719 839 863 887 983 1,019 1,187 1,283 1,307 1,319 1,367 1,439 1,487 1,523 1,619
Safe prime count below  1,000,000: 4,324
Safe prime count below 10,000,000: 30,657

First 40 unsafe primes:
2 3 13 17 19 29 31 37 41 43 53 61 67 71 73 79 89 97 101 103 109 113 127 131 137 139 149 151 157 163 173 181 191 193 197 199 211 223 229 233
Unsafe prime count below  1,000,000: 74,174
Unsafe prime count below 10,000,000: 633,922

```


## FreeBASIC


```freebasic
' version 19-01-2019
' compile with: fbc -s console

Const As UInteger max = 10000000
Dim As UInteger i, j, sc1, usc1, sc2, usc2
Dim As String safeprimes, unsafeprimes
Dim As UByte sieve()

ReDim sieve(max)
' 0 = prime, 1 = no prime
sieve(0) = 1 : sieve(1) = 1

For i = 4 To max Step 2
    sieve(i) = 1
Next
For i = 3 To Sqr(max) +1 Step 2
    If sieve(i) = 0 Then
        For j = i * i To max Step i * 2
            sieve(j) = 1
        Next
    End If
Next

usc1 = 1 : unsafeprimes = "2"
For i = 3 To 3001 Step 2
    If sieve(i) = 0 Then
        If sieve(i \ 2) = 0 Then
            sc1 += 1
            If sc1 <= 35 Then
                safeprimes += " " + Str(i)
            End If
        Else
            usc1 += 1
            If usc1 <= 40 Then
                unsafeprimes +=  " " + Str(i)
            End If
        End If
    End If
Next

For i = 3003 To max \ 10 Step 2
    If sieve(i) = 0 Then
        If sieve(i \ 2) = 0 Then
            sc1 += 1
        Else
            usc1 += 1
        End If
    End If
Next

sc2 = sc1 : usc2 = usc1
For i = max \ 10 +1 To max Step 2
    If sieve(i) = 0 Then
        If sieve(i \ 2) = 0  Then
            sc2 += 1
        Else
            usc2 += 1
        End If
    End If
Next

Print "the first 35 Safeprimes are: "; safeprimes
Print
Print "the first 40 Unsafeprimes are:  "; unsafeprimes
Print
Print "                  Safeprimes     Unsafeprimes"
Print "    Below         ---------------------------"
Print Using "##########,      ";  max \ 10; sc1; usc1
Print Using "##########,      ";  max     ; sc2; usc2

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
the first 35 Safeprimes are:  5 7 11 23 47 59 83 107 167 179 227 263 347 359 383 467 479 503 563 587 719 839 863 887 983 1019 1187 1283 1307 1319 1367 1439 1487 1523 1619

the first 40 Unsafeprimes are:  2 3 13 17 19 29 31 37 41 43 53 61 67 71 73 79 89 97 101 103 109 113 127 131 137 139 149 151 157 163 173 181 191 193 197 199 211 223 229 233

                  Safeprimes     Unsafeprimes
    Below         ---------------------------
  1,000,000            4,324           74,174
 10,000,000           30,657          633,922
```



## Go


```go
package main

import "fmt"

func sieve(limit uint64) []bool {
    limit++
    // True denotes composite, false denotes prime.
    c := make([]bool, limit) // all false by default
    c[0] = true
    c[1] = true
    // apart from 2 all even numbers are of course composite
    for i := uint64(4); i < limit; i += 2 {
        c[i] = true
    }
    p := uint64(3) // Start from 3.
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
    if n < 0 {
        s = s[1:]
    }
    le := len(s)
    for i := le - 3; i >= 1; i -= 3 {
        s = s[0:i] + "," + s[i:]
    }
    if n >= 0 {
        return s
    }
    return "-" + s
}

func main() {
    // sieve up to 10 million
    sieved := sieve(1e7)
    var safe = make([]int, 35)
    count := 0
    for i := 3; count < 35; i += 2 {
        if !sieved[i] && !sieved[(i-1)/2] {
            safe[count] = i
            count++
        }
    }
    fmt.Println("The first 35 safe primes are:\n", safe, "\n")

    count = 0
    for i := 3; i < 1e6; i += 2 {
        if !sieved[i] && !sieved[(i-1)/2] {
            count++
        }
    }
    fmt.Println("The number of safe primes below 1,000,000 is", commatize(count), "\n")

    for i := 1000001; i < 1e7; i += 2 {
        if !sieved[i] && !sieved[(i-1)/2] {
            count++
        }
    }
    fmt.Println("The number of safe primes below 10,000,000 is", commatize(count), "\n")

    unsafe := make([]int, 40)
    unsafe[0] = 2 // since (2 - 1)/2 is not prime
    count = 1
    for i := 3; count < 40; i += 2 {
        if !sieved[i] && sieved[(i-1)/2] {
            unsafe[count] = i
            count++
        }
    }
    fmt.Println("The first 40 unsafe primes are:\n", unsafe, "\n")

    count = 1
    for i := 3; i < 1e6; i += 2 {
        if !sieved[i] && sieved[(i-1)/2] {
            count++
        }
    }
    fmt.Println("The number of unsafe primes below 1,000,000 is", commatize(count), "\n")

    for i := 1000001; i < 1e7; i += 2 {
        if !sieved[i] && sieved[(i-1)/2] {
            count++
        }
    }
    fmt.Println("The number of unsafe primes below 10,000,000 is", commatize(count), "\n")
}
```


{{out}}

```txt

The first 35 safe primes are:
 [5 7 11 23 47 59 83 107 167 179 227 263 347 359 383 467 479 503 563 587 719 839 863 887 983 1019 1187 1283 1307 1319 1367 1439 1487 1523 1619] 

The number of safe primes below 1,000,000 is 4,324 

The number of safe primes below 10,000,000 is 30,657 

The first 40 unsafe primes are:
 [2 3 13 17 19 29 31 37 41 43 53 61 67 71 73 79 89 97 101 103 109 113 127 131 137 139 149 151 157 163 173 181 191 193 197 199 211 223 229 233] 

The number of unsafe primes below 1,000,000 is 74,174 

The number of unsafe primes below 10,000,000 is 633,922 

```



## J


```txt

   NB. play around a bit to get primes less than ten million
   p:inv 10000000
664579

   p:664579
10000019

   PRIMES =: p:i.664579
   10 {. PRIMES
2 3 5 7 11 13 17 19 23 29

   {: PRIMES
9999991


   primeQ =: 1&p:
   safeQ =: primeQ@:-:@:<:
   Filter =: (#~`)(`:6)

   SAFE =: safeQ Filter PRIMES

   NB. first thirty-five safe primes
   (32+3) {. SAFE
5 7 11 23 47 59 83 107 167 179 227 263 347 359 383 467 479 503 563 587 719 839 863 887 983 1019 1187 1283 1307 1319 1367 1439 1487 1523 1619
   

   NB. first forty unsafe primes
   (33+7) {. PRIMES -. SAFE
   2 3 13 17 19 29 31 37 41 43 53 61 67 71 73 79 89 97 101 103 109 113 127 131 137 139 149 151 157 163 173 181 191 193 197 199 211 223 229 233
   

   NB. tally of safe primes less than ten million
   # SAFE
30657
   

   NB. tally of safe primes below a million
   # 1000000&>Filter SAFE
4324
   

   NB. tally of perilous primes below ten million
   UNSAFE =: PRIMES -. SAFE

   # UNSAFE
633922
   

   NB. tally of these below one million
   K =: 1 : 'm * 1000'
   +/ UNSAFE < 1 K K
74174
   

```

Essentially we have

```J

primeQ =: 1&p:
safeQ =: primeQ@:-:@:<: 
Filter =: (#~`)(`:6)
K =: adverb def 'm * 1000'
PRIMES =: i.&.:(p:inv) 10 K K
SAFE =: safeQ Filter PRIMES
UNSAFE =: PRIMES -. SAFE

```

The rest of the display is mere window dressing.


## Java


```java
public class SafePrimes {
    public static void main(String... args) {
        // Use Sieve of Eratosthenes to find primes
        int SIEVE_SIZE = 10_000_000;
        boolean[] isComposite = new boolean[SIEVE_SIZE];
        // It's really a flag indicating non-prime, but composite usually applies
        isComposite[0] = true;
        isComposite[1] = true;
        for (int n = 2; n < SIEVE_SIZE; n++) {
            if (isComposite[n]) {
                continue;
            }
            for (int i = n * 2; i < SIEVE_SIZE; i += n) {
                isComposite[i] = true;
            }
        }
        
        int oldSafePrimeCount = 0;
        int oldUnsafePrimeCount = 0;
        int safePrimeCount = 0;
        int unsafePrimeCount = 0;
        StringBuilder safePrimes = new StringBuilder();
        StringBuilder unsafePrimes = new StringBuilder();
        int safePrimesStrCount = 0;
        int unsafePrimesStrCount = 0;
        for (int n = 2; n < SIEVE_SIZE; n++) {
            if (n == 1_000_000) {
                oldSafePrimeCount = safePrimeCount;
                oldUnsafePrimeCount = unsafePrimeCount;
            }
            if (isComposite[n]) {
                continue;
            }
            boolean isUnsafe = isComposite[(n - 1) >>> 1];
            if (isUnsafe) {
                if (unsafePrimeCount < 40) {
                    if (unsafePrimeCount > 0) {
                        unsafePrimes.append(", ");
                    }
                    unsafePrimes.append(n);
                    unsafePrimesStrCount++;
                }
                unsafePrimeCount++;
            }
            else {
                if (safePrimeCount < 35) {
                    if (safePrimeCount > 0) {
                        safePrimes.append(", ");
                    }
                    safePrimes.append(n);
                    safePrimesStrCount++;
                }
                safePrimeCount++;
            }
        }
        
        System.out.println("First " + safePrimesStrCount + " safe primes: " + safePrimes.toString());
        System.out.println("Number of safe primes below 1,000,000: " + oldSafePrimeCount);
        System.out.println("Number of safe primes below 10,000,000: " + safePrimeCount);
        System.out.println("First " + unsafePrimesStrCount + " unsafe primes: " + unsafePrimes.toString());
        System.out.println("Number of unsafe primes below 1,000,000: " + oldUnsafePrimeCount);
        System.out.println("Number of unsafe primes below 10,000,000: " + unsafePrimeCount);
        
        return;
    }
}
```

{{out}}

```txt
First 35 safe primes: 5, 7, 11, 23, 47, 59, 83, 107, 167, 179, 227, 263, 347, 359, 383, 467, 479, 503, 563, 587, 719, 839, 863, 887, 983, 1019, 1187, 1283, 1307, 1319, 1367, 1439, 1487, 1523, 1619
Number of safe primes below 1,000,000: 4324
Number of safe primes below 10,000,000: 30657
First 40 unsafe primes: 2, 3, 13, 17, 19, 29, 31, 37, 41, 43, 53, 61, 67, 71, 73, 79, 89, 97, 101, 103, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 173, 181, 191, 193, 197, 199, 211, 223, 229, 233
Number of unsafe primes below 1,000,000: 74174
Number of unsafe primes below 10,000,000: 633922
```



## Julia


```julia
using Primes, Formatting

function parseprimelist()
    primelist = primes(2, 10000000)
    safeprimes = Vector{Int64}()
    unsafeprimes = Vector{Int64}()
    for p in primelist
        if isprime(div(p - 1, 2))
            push!(safeprimes, p)
        else
            push!(unsafeprimes, p)
        end
    end
    println("The first 35 unsafe primes are: ", safeprimes[1:35])
    println("There are ", format(sum(map(x -> x < 1000000, safeprimes)), commas=true), " safe primes less than 1 million.")
    println("There are ", format(length(safeprimes), commas=true), " safe primes less than 10 million.")    
    println("The first 40 unsafe primes are: ", unsafeprimes[1:40])
    println("There are ", format(sum(map(x -> x < 1000000, unsafeprimes)), commas=true), " unsafe primes less than 1 million.")
    println("There are ", format(length(unsafeprimes), commas=true), " unsafe primes less than 10 million.")
end

parseprimelist()

```
 {{output}} 
```txt

The first 35 unsafe primes are: [5, 7, 11, 23, 47, 59, 83, 107, 167, 179, 227, 263, 347, 359, 383, 467, 479, 503, 563, 587, 719, 839, 863, 887, 983, 1019, 1187, 1283, 1307, 1319, 1367, 1439, 1487, 1523, 1619]
There are 4,324 safe primes less than 1 million.
There are 30,657 safe primes less than 10 million.
The first 40 unsafe primes are: [2, 3, 13, 17, 19, 29, 31, 37, 41, 43, 53, 61, 67, 71, 73, 79, 89, 97, 101, 103, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 173, 181, 191, 193, 197, 199, 211, 223, 229, 233]
There are 74,174 unsafe primes less than 1 million.
There are 633,922 unsafe primes less than 10 million.

```




## Kotlin

{{trans|Go}}

```scala
// Version 1.2.70

fun sieve(limit: Int): BooleanArray {
    // True denotes composite, false denotes prime.
    val c = BooleanArray(limit + 1) // all false by default
    c[0] = true
    c[1] = true
    // apart from 2 all even numbers are of course composite
    for (i in 4..limit step 2) c[i] = true
    var p = 3 // start from 3
    while (true) {
        val p2 = p * p
        if (p2 > limit) break
        for (i in p2..limit step 2 * p) c[i] = true
        while (true) {
            p += 2
            if (!c[p]) break
        }
    }
    return c
}

fun main(args: Array<String>) {
    // sieve up to 10 million
    val sieved = sieve(10_000_000)
    val safe = IntArray(35)
    var count = 0
    var i = 3
    while (count < 35) {
        if (!sieved[i] && !sieved[(i - 1) / 2]) safe[count++] = i
        i += 2
    }
    println("The first 35 safe primes are:")
    println(safe.joinToString(" ","[", "]\n"))

    count = 0
    for (j in 3 until 1_000_000 step 2) {
        if (!sieved[j] && !sieved[(j - 1) / 2]) count++
    }
    System.out.printf("The number of safe primes below 1,000,000 is %,d\n\n", count)

    for (j in 1_000_001 until 10_000_000 step 2) {
        if (!sieved[j] && !sieved[(j - 1) / 2]) count++
    }
    System.out.printf("The number of safe primes below 10,000,000 is %,d\n\n", count)

    val unsafe = IntArray(40)
    unsafe[0] = 2  // since (2 - 1)/2 is not prime
    count = 1
    i = 3
    while (count < 40) {
        if (!sieved[i] && sieved[(i - 1) / 2]) unsafe[count++] = i
        i += 2
    }
    println("The first 40 unsafe primes are:")
    println(unsafe.joinToString(" ","[", "]\n"))

    count = 1
    for (j in 3 until 1_000_000 step 2) {
        if (!sieved[j] && sieved[(j - 1) / 2]) count++
    }
    System.out.printf("The number of unsafe primes below 1,000,000 is %,d\n\n", count)

    for (j in 1_000_001 until 10_000_000 step 2) {
        if (!sieved[j] && sieved[(j - 1) / 2]) count++
    }
    System.out.printf("The number of unsafe primes below 10,000,000 is %,d\n\n", count)
}
```


{{output}}

```txt

The first 35 safe primes are:
[5 7 11 23 47 59 83 107 167 179 227 263 347 359 383 467 479 503 563 587 719 839 863 887 983 1019 1187 1283 1307 1319 1367 1439 1487 1523 1619]

The number of safe primes below 1,000,000 is 4,324

The number of safe primes below 10,000,000 is 30,657

The first 40 unsafe primes are:
[2 3 13 17 19 29 31 37 41 43 53 61 67 71 73 79 89 97 101 103 109 113 127 131 137 139 149 151 157 163 173 181 191 193 197 199 211 223 229 233]

The number of unsafe primes below 1,000,000 is 74,174

The number of unsafe primes below 10,000,000 is 633,922

```



## Maple
           

```Maple
showSafePrimes := proc(n::posint) 
local prime_list, k; 
prime_list := [5]; 
for k to n - 1 do 
  prime_list := [op(prime_list), NumberTheory:-NextSafePrime(prime_list[-1])]; 
end do; 
return prime_list; 
end proc;

showUnsafePrimes := proc(n::posint)
local prime_num, k;
prime_num := [2];
for k to n-1 do
  prime_num := [op(prime_num), nextprime(prime_num[-1])];
end do;
return remove(x -> member(x, showSafePrimes(n)), prime_num);
end proc:

countSafePrimes := proc(n::posint) 
local counts, prime; 
counts := 0; 
prime := 5; 
while prime < n do prime := NumberTheory:-NextSafePrime(prime); 
  counts := counts + 1; 
end do; 
return counts; 
end proc;

countUnsafePrimes := proc(n::posint)
local safe_counts, total; 
safe_counts := countSafePrimes(n); 
total := NumberTheory:-PrimeCounting(n); 
return total - safe_counts; 
end proc;

showSafePrimes(35);
showUnsafePrimes(40);
countSafePrimes(1000000);                        
countSafePrimes(10000000);
countUnsafePrimes(1000000);
countUnsafePrimes(10000000);
```

{{out}}

```txt
[5, 7, 11, 23, 47, 59, 83, 107, 167, 179, 227, 263, 347, 359, 383, 467, 479, 503, 563, 587, 719, 839, 863, 887, 983, 1019, 1187, 1283, 1307, 1319, 1367, 1439, 1487, 1523, 1619]
[2, 3, 13, 17, 19, 29, 31, 37, 41, 43, 53, 61, 67, 71, 73, 79, 89, 97, 101, 103, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 173]
4324
30657
74174
633922
```



## Pascal

{{works with|Free Pascal}}
Using unit mp_prime of Wolfgang Erhardt ( RIP ) , of which I use two sieve, to simplify things.
Generating small primes and checked by the second, which starts to run 2x ahead.Sieving of consecutive prime number is much faster than primality check.

```pascal
program Sophie;
{ Find and count Sophie Germain primes }
{ uses unit mp_prime out of mparith of Wolfgang Ehrhardt
* http://wolfgang-ehrhardt.de/misc_en.html#mparith
  http://wolfgang-ehrhardt.de/mp_intro.html }
{$APPTYPE CONSOLE}
uses
 mp_prime,sysutils; 
var
  pS0,pS1:TSieve;  
procedure SafeOrNoSavePrimeOut(totCnt:NativeInt;CntSafe:boolean);
var
  cnt,pr,pSG,testPr : NativeUint;
begin
  prime_sieve_reset(pS0,1);
  prime_sieve_reset(pS1,1);
  cnt := 0;
// memorize prime of the sieve, because sometimes prime_sieve_next(pS1) is to far ahead.
  testPr := prime_sieve_next(pS1);
  IF CntSafe then  
  Begin
    writeln('First ',totCnt,' safe primes');  
    repeat
      pr := prime_sieve_next(pS0);
      pSG := 2*pr+1;
      while testPr< pSG do
        testPr := prime_sieve_next(pS1);
      if pSG = testPr then
      begin
        write(pSG,',');
        inc(cnt);
      end; 
    until cnt >= totCnt
  end  
  else
  Begin
    writeln('First ',totCnt,' unsafe primes');  
    repeat
      pr := prime_sieve_next(pS0);
      pSG := (pr-1) DIV 2;
      while testPr< pSG do
        testPr := prime_sieve_next(pS1);
      if pSG <> testPr then
      begin
        write(pr,',');
        inc(cnt);
      end; 
    until cnt >= totCnt; 
  end;  
  writeln(#8,#32);  
end; 

function CountSafePrimes(Limit:NativeInt):NativeUint;
var
  cnt,pr,pSG,testPr : NativeUint;
begin
  prime_sieve_reset(pS0,1);
  prime_sieve_reset(pS1,1);
  cnt := 0;
  testPr := 0;
  repeat
    pr := prime_sieve_next(pS0);
    pSG := 2*pr+1;
    while testPr< pSG do
      testPr := prime_sieve_next(pS1);
    if pSG = testPr then
      inc(cnt);
  until pSG >= Limit; 
  CountSafePrimes := cnt;
end; 

procedure CountSafePrimesOut(Limit:NativeUint);
Begin
  writeln('there are ',CountSafePrimes(limit),' safe primes out of ',
          primepi32(limit),' primes up to ',Limit);
end;

procedure CountUnSafePrimesOut(Limit:NativeUint);
var
  prCnt: NativeUint;
Begin
  prCnt := primepi32(limit);
  writeln('there are ',prCnt-CountSafePrimes(limit),' unsafe primes out of ',
          prCnt,' primes up to ',Limit);
end;

var
  T1,T0 : INt64;
begin
  T0 :=gettickcount64; 
  prime_sieve_init(pS0,1);
  prime_sieve_init(pS1,1);
//Find and display (on one line) the first  35  safe primes.  
  SafeOrNoSavePrimeOut(35,true);
//Find and display the  count  of the safe primes below  1,000,000. 
  CountSafePrimesOut(1000*1000);
//Find and display the  count  of the safe primes below 10,000,000.  
  CountSafePrimesOut(10*1000*1000);  
//Find and display (on one line) the first  40  unsafe primes.  
  SafeOrNoSavePrimeOut(40,false);
//Find and display the  count  of the unsafe primes below  1,000,000.
  CountUnSafePrimesOut(1000*1000);
//Find and display the  count  of the unsafe primes below 10,000,000.  
  CountUnSafePrimesOut(10*1000*1000);
  writeln;
  CountSafePrimesOut(1000*1000*1000);        
  T1 :=gettickcount64; 
  writeln('runtime ',T1-T0,' ms');
end.
```

{{output}}

```txt
First 35 safe primes
5,7,11,23,47,59,83,107,167,179,227,263,347,359,383,467,479,503,563,587,719,839,863,887,983,1019,1187,1283,1307,1319,1367,1439,1487,1523,1619
there are 4324 safe primes out of 78498 primes up to 1000000
there are 30657 safe primes out of 664579 primes up to 10000000
First 40 unsafe primes
2,3,13,17,19,29,31,37,41,43,53,61,67,71,73,79,89,97,101,103,109,113,127,131,137,139,149,151,157,163,173,181,191,193,197,199,211,223,229,233
there are 74174 unsafe primes out of 78498 primes up to 1000000
there are 633922 unsafe primes out of 664579 primes up to 10000000
there are 1775676 safe primes out of 50847534 primes up to 1000000000
runtime 2797 ms

```



## Perl

The module <code>ntheory</code> does fast prime generation and testing.
{{libheader|ntheory}}

```perl
use ntheory qw(forprimes is_prime);

my $upto = 1e7;
my %class = ( safe => [], unsafe => [2] );

forprimes {
    push @{$class{ is_prime(($_-1)>>1) ? 'safe' : 'unsafe' }}, $_;
} 3, $upto;

for (['safe', 35], ['unsafe', 40]) {
    my($type, $quantity) = @$_;
    print  "The first $quantity $type primes are:\n";
    print join(" ", map { comma($class{$type}->[$_-1]) } 1..$quantity), "\n";
    for my $q ($upto/10, $upto) {
        my $n = scalar(grep { $_ <= $q } @{$class{$type}});
        printf "The number of $type primes up to %s: %s\n", comma($q), comma($n);
    }
}

sub comma {
    (my $s = reverse shift) =~ s/(.{3})/$1,/g;
    $s =~ s/,(-?)$/$1/;
    $s = reverse $s;
}
```

{{out}}

```txt
The first 35 safe primes are:
5 7 11 23 47 59 83 107 167 179 227 263 347 359 383 467 479 503 563 587 719 839 863 887 983 1,019 1,187 1,283 1,307 1,319 1,367 1,439 1,487 1,523 1,619
The number of safe primes up to 1,000,000: 4,324
The number of safe primes up to 10,000,000: 30,657
The first 40 unsafe primes are:
2 3 13 17 19 29 31 37 41 43 53 61 67 71 73 79 89 97 101 103 109 113 127 131 137 139 149 151 157 163 173 181 191 193 197 199 211 223 229 233
The number of unsafe primes up to 1,000,000: 74,174
The number of unsafe primes up to 10,000,000: 633,922
```



## Perl 6

{{works with|Rakudo|2018.08}}
Perl 6 has a built-in method .is-prime to test for prime numbers. It's great for testing individual numbers or to find/filter a few thousand numbers, but when you are looking for millions, it becomes a drag. No fear, the Perl 6 ecosystem has a fast prime sieve module available which can produce 10 million primes in a few seconds. Once we have the primes, it is just a small matter of filtering and formatting them appropriately.   


```perl6
sub comma { $^i.flip.comb(3).join(',').flip }

use Math::Primesieve;

my $sieve = Math::Primesieve.new;

my @primes = $sieve.primes(10_000_000);

my %filter = @primes.Set;

my $primes = @primes.classify: { %filter{($_ - 1)/2} ?? 'safe' !! 'unsafe' };

for 'safe', 35, 'unsafe', 40 -> $type, $quantity {
    say "The first $quantity $type primes are:";

    say $primes{$type}[^$quantity]».&comma;

    say "The number of $type primes up to {comma $_}: ",
    comma $primes{$type}.first(* > $_, :k) // +$primes{$type} for 1e6, 1e7;

    say '';
}
```

{{out}}

```txt
The first 35 safe primes are:
(5 7 11 23 47 59 83 107 167 179 227 263 347 359 383 467 479 503 563 587 719 839 863 887 983 1,019 1,187 1,283 1,307 1,319 1,367 1,439 1,487 1,523 1,619)
The number of safe primes up to 1,000,000: 4,324
The number of safe primes up to 10,000,000: 30,657

The first 40 unsafe primes are:
(2 3 13 17 19 29 31 37 41 43 53 61 67 71 73 79 89 97 101 103 109 113 127 131 137 139 149 151 157 163 173 181 191 193 197 199 211 223 229 233)
The number of unsafe primes up to 1,000,000: 74,174
The number of unsafe primes up to 10,000,000: 633,922
```



## Phix

using [[Extensible_prime_generator#Phix]]

```Phix
while sieved<10_000_000 do
    add_block()
end while
sequence safe = {}, unsafe = {}
procedure filter_range(integer lo, hi)
    for i=lo to hi do
        integer p = primes[i]
        if p>2 and is_prime((p-1)/2) then
            safe &= p
        else
            unsafe &= p
        end if
    end for
end procedure
integer k = abs(binary_search(1_000_000,primes)),
        l = abs(binary_search(10_000_000,primes))
filter_range(1,k-1)
integer ls = length(safe), lu = length(unsafe)
filter_range(k,l-1)
printf(1,"The first 35 safe primes: %s\n",{sprint(safe[1..35])})
printf(1,"Count of safe primes below 1,000,000: %,d\n",ls)
printf(1,"Count of safe primes below 10,000,000: %,d\n",length(safe))
printf(1,"The first 40 unsafe primes: %s\n",{sprint(unsafe[1..40])})
printf(1,"Count of unsafe primes below 1,000,000: %,d\n",lu)
printf(1,"Count of unsafe primes below 10,000,000: %,d\n",length(unsafe))
```

{{out}}
<pre style="font-size: 11px">
The first 35 safe primes: {5,7,11,23,47,59,83,107,167,179,227,263,347,359,383,467,479,503,563,587,719,839,863,887,983,1019,1187,1283,1307,1319,1367,1439,1487,1523,1619}
Count of safe primes below 1,000,000: 4,324
Count of safe primes below 10,000,000: 30,657
The first 40 unsafe primes: {2,3,13,17,19,29,31,37,41,43,53,61,67,71,73,79,89,97,101,103,109,113,127,131,137,139,149,151,157,163,173,181,191,193,197,199,211,223,229,233}
Count of unsafe primes below 1,000,000: 74,174
Count of unsafe primes below 10,000,000: 633,922

```



## Python


```Python

primes =[]
sp =[]
usp=[]
n = 10000000
if 2<n:
    primes.append(2)
for i in range(3,n+1,2):
    for j in primes:
        if(j>i/2) or (j==primes[-1]):
            primes.append(i)
            if((i-1)/2) in primes:
                sp.append(i)
                break
            else:
                usp.append(i)
                break
        if (i%j==0):
            break

print('First 35 safe primes are:\n' , sp[:35])
print('There are '+str(len(sp[:1000000]))+' safe primes below 1,000,000')
print('There are '+str(len(sp))+' safe primes below 10,000,000')
print('First 40 unsafe primes:\n',usp[:40])
print('There are '+str(len(usp[:1000000]))+' unsafe primes below 1,000,000')
print('There are '+str(len(usp))+' safe primes below 10,000,000')

```

{{out}}
<pre style="font-size: 11px">
First 35 safe primes: 
[5,7,11,23,47,59,83,107,167,179,227,263,347,359,383,467,479,503,563,587,719,839,863,887,983,1019,1187,1283,1307,1319,1367,1439,1487,1523,1619]
There are 4,234 safe primes below 1,000,000
There are 30,657 safe primes below 10,000,000
First 40 unsafe primes: 
[2,3,13,17,19,29,31,37,41,43,53,61,67,71,73,79,89,97,101,103,109,113,127,131,137,139,149,151,157,163,173,181,191,193,197,199,211,223,229,233]
There are 74,174 unsafe primes below 1,000,000
There are 633,922 unsafe primes below 10,000,000

```



## REXX


```rexx
/*REXX program lists a sequence  (or a count)  of  ──safe──   or   ──unsafe──   primes. */
parse arg N kind _ . 1 . okind;     upper kind   /*obtain optional arguments from the CL*/
if N=='' | N==","  then N= 35                    /*Not specified?   Then assume default.*/
if kind=='' | kind==","  then kind= 'SAFE'       /* "      "          "     "      "    */
if _\==''                             then call ser 'too many arguments specified.'
if kind\=='SAFE'  &  kind\=='UNSAFE'  then call ser 'invalid 2nd argument: '   okind
if kind =='UNSAFE'  then safe= 0;  else safe= 1  /*SAFE  is a binary value for function.*/
w = linesize() - 1                               /*obtain the usable width of the term. */
tell= (N>0);    @.=;    N= abs(N)                /*N is negative?   Then don't display. */
!.=0;   !.1=2;  !.2=3;  !.3=5;  !.4=7;  !.5=11;  !.6=13;  !.7=17;  !.8=19;    #= 8
@.='';  @.2=1;  @.3=1;  @.5=1;  @.7=1;  @.11=1;  @.13=1;  @.17=1;  @.19=1;    start= # + 1
m= 0;                         lim=0              /*#  is the number of low primes so far*/
$=;     do i=1  for #   while lim<=N;  j= !.i    /* [↓]  find primes, and maybe show 'em*/
        call safeUnsafe;      $= strip($)        /*go see if other part of a KIND prime.*/
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
   call safeUnsafe                               /*go see if other part of a KIND prime.*/
   end   /*j*/
                                                 /* [↓]  display number of primes found.*/
if $\==''  then say $                            /*display any residual primes in $ list*/
say
if tell  then say commas(m)' '     kind    "primes found."
         else say commas(m)' '     kind    "primes found below or equal to "    commas(N).
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
add: m= m+1; lim= m; if \tell & j>N  then do; lim= j; m= m-1; end; else call app; return 1
app: if tell  then if length($ j)>w  then do;  say $; $ =j;   end; else $= $ j;   return 1
ser: say;  say;  say '***error***' arg(1);  say;  say;  exit 13   /*tell error message. */
commas: parse arg _;  do jc=length(_)-3  to 1  by -3; _=insert(',', _, jc); end;  return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
safeUnsafe: ?= (j-1) % 2                         /*obtain the other part of KIND prime. */
            if safe  then if @.? == ''  then return 0             /*not a    safe prime.*/
                                        else return add()         /*is  "      "    "   */
                     else if @.? == ''  then return add()         /*is  an unsafe prime.*/
                                        else return 0             /*not  "   "      "   */
```

This REXX program makes use of   '''LINESIZE'''   REXX program (or BIF) which is used to determine 
the screen width (or linesize) of the terminal (console).   Some REXXes don't have this BIF.

The   '''LINESIZE.REX'''   REXX program is included here   ───►   [[LINESIZE.REX]].


{{out|output|text=  when using the default input of:     <tt> 35 </tt>}}
Shown at   <big>'''<sup>5</sup>/<sub>6</sub>'''</big>   size.)

<pre style="font-size:83%">
5 7 11 23 47 59 83 107 167 179 227 263 347 359 383 467 479 503 563 587 719 839 863 887 983 1019 1187 1283 1307 1319 1367 1439 1487 1523 1619

35  SAFE primes found.

```


{{out|output|text=  when using the input:     <tt> -1000000 </tt>}}

```txt

4,324  SAFE primes found below or equal to  1,000,000.

```


{{out|output|text=  when using the input:     <tt> -10000000 </tt>}}

```txt

30,657  SAFE primes found below or equal to  10,000,000.

```


{{out|output|text=  when using the input:     <tt> 40   unsafe </tt>}}

(Shown at   <big>'''<sup>5</sup>/<sub>6</sub>'''</big>   size.)

<pre style="font-size:83%">
2 3 13 17 19 29 31 37 41 43 53 61 67 71 73 79 89 97 101 103 109 113 127 131 137 139 149 151 157 163 173 181 191 193 197 199 211 223 229 233

40  UNSAFE primes found.

```


{{out|output|text=  when using the input:     <tt> -1000000   unsafe </tt>}}

```txt

74,174  UNSAFE primes found below or equal to  1,000,000.

```


{{out|output|text=  when using the input:     <tt> -10000000 </tt>}}

```txt

633,922  UNSAFE primes found below or equal to  10,000,000.

```



## Ruby


```ruby
require "prime"
class Integer
  def safe_prime? #assumes prime
    ((self-1)/2).prime?
  end
end

def format_parts(n)
  partitions = Prime.each(n).partition(&:safe_prime?).map(&:count)
  "There are %d safes and %d unsafes below #{n}."% partitions
end

puts "First 35 safe-primes:"
p Prime.each.lazy.select(&:safe_prime?).take(35).to_a
puts format_parts(1_000_000), "\n" 

puts "First 40 unsafe-primes:"
p Prime.each.lazy.reject(&:safe_prime?).take(40).to_a
puts format_parts(10_000_000)

```

{{out}}

```txt

First 35 safe-primes:
[5, 7, 11, 23, 47, 59, 83, 107, 167, 179, 227, 263, 347, 359, 383, 467, 479, 503, 563, 587, 719, 839, 863, 887, 983, 1019, 1187, 1283, 1307, 1319, 1367, 1439, 1487, 1523, 1619]
There are 4324 safes and 74174 unsafes below 1000000.

First 40 unsafe-primes:
[2, 3, 13, 17, 19, 29, 31, 37, 41, 43, 53, 61, 67, 71, 73, 79, 89, 97, 101, 103, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 173, 181, 191, 193, 197, 199, 211, 223, 229, 233]
There are 30657 safes and 633922 unsafes below 10000000.


```


## Sidef


```ruby
func is_safeprime(p) {
    is_prime(p) && is_prime((p-1)/2)
}

func is_unsafeprime(p) {
    is_prime(p) && !is_prime((p-1)/2)
}

func safeprime_count(from, to) {
    from..to -> count_by(is_safeprime)
}

func unsafeprime_count(from, to) {
    from..to -> count_by(is_unsafeprime)
}

say "First 35 safe-primes:"
say (1..Inf -> lazy.grep(is_safeprime).first(35).join(' '))
say ''
say "First 40 unsafe-primes:"
say (1..Inf -> lazy.grep(is_unsafeprime).first(40).join(' '))
say ''
say "There are #{safeprime_count(1, 1e6)} safe-primes bellow 10^6"
say "There are #{unsafeprime_count(1, 1e6)} unsafe-primes bellow 10^6"
say ''
say "There are #{safeprime_count(1, 1e7)} safe-primes bellow 10^7"
say "There are #{unsafeprime_count(1, 1e7)} unsafe-primes bellow 10^7"
```

{{out}}

```txt

First 35 safe-primes:
5 7 11 23 47 59 83 107 167 179 227 263 347 359 383 467 479 503 563 587 719 839 863 887 983 1019 1187 1283 1307 1319 1367 1439 1487 1523 1619

First 40 unsafe-primes:
2 3 13 17 19 29 31 37 41 43 53 61 67 71 73 79 89 97 101 103 109 113 127 131 137 139 149 151 157 163 173 181 191 193 197 199 211 223 229 233

There are 4324 safe-primes bellow 10^6
There are 74174 unsafe-primes bellow 10^6

There are 30657 safe-primes bellow 10^7
There are 633922 unsafe-primes bellow 10^7

```


## Simula


```simula

BEGIN

    CLASS BOOLARRAY(N); INTEGER N;
    BEGIN
        BOOLEAN ARRAY DATA(0:N-1);
    END BOOLARRAY;

    CLASS INTARRAY(N); INTEGER N;
    BEGIN
        INTEGER ARRAY DATA(0:N-1);
    END INTARRAY;

    REF(BOOLARRAY) PROCEDURE SIEVE(LIMIT);
        INTEGER LIMIT;
    BEGIN
        REF(BOOLARRAY) C;
        INTEGER P, P2;
        LIMIT := LIMIT+1;
        COMMENT TRUE DENOTES COMPOSITE, FALSE DENOTES PRIME. ;
        C :- NEW BOOLARRAY(LIMIT); COMMENT ALL FALSE BY DEFAULT ;
        C.DATA(0) := TRUE;
        C.DATA(1) := TRUE;
        COMMENT APART FROM 2 ALL EVEN NUMBERS ARE OF COURSE COMPOSITE ;
        FOR I := 4 STEP 2 UNTIL LIMIT-1 DO
            C.DATA(I) := TRUE;
        COMMENT START FROM 3. ;
        P := 3;
        WHILE TRUE DO BEGIN
            P2 := P * P;
            IF P2 >= LIMIT THEN BEGIN
                GO TO OUTER_BREAK;
            END;
            I := P2;
            WHILE I < LIMIT DO BEGIN
                C.DATA(I) := TRUE;
                I := I + 2 * P;
            END;
            WHILE TRUE DO BEGIN
                P := P + 2;
                IF NOT C.DATA(P) THEN BEGIN
                    GO TO INNER_BREAK;
                END;
            END;
            INNER_BREAK:
        END;
        OUTER_BREAK:
        SIEVE :- C;
    END SIEVE;

    COMMENT MAIN BLOCK ;

    REF(BOOLARRAY) SIEVED;
    REF(INTARRAY) UNSAFE, SAFE;
    INTEGER I, COUNT;

    COMMENT SIEVE UP TO 10 MILLION ;
    SIEVED :- SIEVE(10000000);

    SAFE :- NEW INTARRAY(35);
    COUNT := 0;
    I := 3;
    WHILE COUNT < 35 DO BEGIN
        IF NOT SIEVED.DATA(I) AND NOT SIEVED.DATA((I-1)//2) THEN BEGIN
            SAFE.DATA(COUNT) := I;
            COUNT := COUNT+1;
        END;
        I := I+2;
    END;
    OUTTEXT("THE FIRST 35 SAFE PRIMES ARE:");
    OUTIMAGE;
    OUTCHAR('[');
    FOR I := 0 STEP 1 UNTIL 35-1 DO BEGIN
        IF I>0 THEN OUTCHAR(' ');
        OUTINT(SAFE.DATA(I), 0);
    END;
    OUTCHAR(']');
    OUTIMAGE;
    OUTIMAGE;

    COUNT := 0;
    FOR I := 3 STEP 2 UNTIL 1000000 DO BEGIN
        IF NOT SIEVED.DATA(I) AND NOT SIEVED.DATA((I-1)//2) THEN BEGIN
            COUNT := COUNT+1;
        END;
    END;
    OUTTEXT("THE NUMBER OF SAFE PRIMES BELOW 1,000,000 IS ");
    OUTINT(COUNT, 0);
    OUTIMAGE;
    OUTIMAGE;

    FOR I := 1000001 STEP 2 UNTIL 10000000 DO BEGIN
        IF NOT SIEVED.DATA(I) AND NOT SIEVED.DATA((I-1)//2) THEN
            COUNT := COUNT+1;
    END;
    OUTTEXT("THE NUMBER OF SAFE PRIMES BELOW 10,000,000 IS ");
    OUTINT(COUNT, 0);
    OUTIMAGE;
    OUTIMAGE;

    UNSAFE :- NEW INTARRAY(40);
    UNSAFE.DATA(0) := 2; COMMENT SINCE (2 - 1)/2 IS NOT PRIME ;
    COUNT := 1;
    I := 3;
    WHILE COUNT < 40 DO BEGIN
        IF NOT SIEVED.DATA(I) AND SIEVED.DATA((I-1)//2) THEN BEGIN
            UNSAFE.DATA(COUNT) := I;
            COUNT := COUNT+1;
        END;
        I := I+2;
    END;
    OUTTEXT("THE FIRST 40 UNSAFE PRIMES ARE:");
    OUTIMAGE;
    OUTCHAR('[');
    FOR I := 0 STEP 1 UNTIL 40-1 DO BEGIN
        IF I>0 THEN OUTCHAR(' ');
        OUTINT(UNSAFE.DATA(I), 0);
    END;
    OUTCHAR(']');
    OUTIMAGE;
    OUTIMAGE;

    COUNT := 1;
    FOR I := 3 STEP 2 UNTIL 1000000 DO BEGIN
        IF NOT SIEVED.DATA(I) AND SIEVED.DATA((I-1)//2) THEN
            COUNT := COUNT+1;
    END;
    OUTTEXT("THE NUMBER OF UNSAFE PRIMES BELOW 1,000,000 IS ");
    OUTINT(COUNT, 0);
    OUTIMAGE;
    OUTIMAGE;

    FOR I := 1000001 STEP 2 UNTIL 10000000 DO BEGIN
        IF NOT SIEVED.DATA(I) AND SIEVED.DATA((I-1)//2) THEN
            COUNT := COUNT+1;
    END;
    OUTTEXT("THE NUMBER OF UNSAFE PRIMES BELOW 10,000,000 IS ");
    OUTINT(COUNT, 0);
    OUTIMAGE;


END

```

{{out}}

```txt

THE FIRST 35 SAFE PRIMES ARE:
[5 7 11 23 47 59 83 107 167 179 227 263 347 359 383 467 479 503 563 587 719 839
863 887 983 1019 1187 1283 1307 1319 1367 1439 1487 1523 1619]

THE NUMBER OF SAFE PRIMES BELOW 1,000,000 IS 4324

THE NUMBER OF SAFE PRIMES BELOW 10,000,000 IS 30657

THE FIRST 40 UNSAFE PRIMES ARE:
[2 3 13 17 19 29 31 37 41 43 53 61 67 71 73 79 89 97 101 103 109 113 127 131 137
 139 149 151 157 163 173 181 191 193 197 199 211 223 229 233]

THE NUMBER OF UNSAFE PRIMES BELOW 1,000,000 IS 74174

THE NUMBER OF UNSAFE PRIMES BELOW 10,000,000 IS 633922

```



## Visual Basic .NET

{{trans|C#}}Dependent on using .NET Core 2.1 or 2.0, or .NET Framework 4.7.2

```vbnet
Imports System.Console

Namespace safety
    Module SafePrimes
        Dim pri_HS As HashSet(Of Integer) = Primes(10_000_000).ToHashSet()

        Sub Main()
            For Each UnSafe In {False, True} : Dim n As Integer = If(UnSafe, 40, 35)
                WriteLine($"The first {n} {If(UnSafe, "un", "")}safe primes are:")
                WriteLine(String.Join(" ", pri_HS.Where(Function(p) UnSafe Xor
                                                            pri_HS.Contains(p \ 2)).Take(n)))
            Next : Dim limit As Integer = 1_000_000 : Do
                Dim part = pri_HS.TakeWhile(Function(l) l < limit),
                 sc As Integer = part.Count(Function(p) pri_HS.Contains(p \ 2))
                WriteLine($"Of the primes below {limit:n0}: {sc:n0} are safe, and {part.Count() -
                          sc:n0} are unsafe.") : If limit = 1_000_000 Then limit *= 10 Else Exit Do
            Loop
        End Sub

        Private Iterator Function Primes(ByVal bound As Integer) As IEnumerable(Of Integer)
            If bound < 2 Then Return
            Yield 2
            Dim composite As BitArray = New BitArray((bound - 1) \ 2)
            Dim limit As Integer = (CInt((Math.Sqrt(bound))) - 1) \ 2
            For i As Integer = 0 To limit - 1 : If composite(i) Then Continue For
                Dim prime As Integer = 2 * i + 3 : Yield prime
                Dim j As Integer = (prime * prime - 2) \ 2
                While j < composite.Count : composite(j) = True : j += prime : End While
            Next
            For i As integer = limit To composite.Count - 1 : If Not composite(i) Then Yield 2 * i + 3
            Next
        End Function
    End Module
End Namespace
```

If not using the latest version of the '''System.Linq''' namespace, you can implement the ''Enumerable.ToHashSet()'' method by adding 
```vbnet>Imports System.Runtime.CompilerServices</lang
 to the top and this module inside the '''safety''' namespace:
```vbnet
    Module Extensions
        <Extension()>
        Function ToHashSet(Of T)(ByVal src As IEnumerable(Of T), ByVal Optional _
                                 IECmp As IEqualityComparer(Of T) = Nothing) As HashSet(Of T)
            Return New HashSet(Of T)(src, IECmp)
        End Function
    End Module
```

{{out}}

```txt
The first 35 safe primes are:
5 7 11 23 47 59 83 107 167 179 227 263 347 359 383 467 479 503 563 587 719 839 863 887 983 1019 1187 1283 1307 1319 1367 1439 1487 1523 1619
The first 40 unsafe primes are:
2 3 13 17 19 29 31 37 41 43 53 61 67 71 73 79 89 97 101 103 109 113 127 131 137 139 149 151 157 163 173 181 191 193 197 199 211 223 229 233
Of the primes below 1,000,000: 4,324 are safe, and 74,174 are unsafe.
Of the primes below 10,000,000: 30,657 are safe, and 633,922 are unsafe.

```



## zkl

Using GMP (GNU Multiple Precision Arithmetic Library, probabilistic
primes), because it is easy and fast to generate primes.

[[Extensible prime generator#zkl]] could be used instead.

```zkl
var [const] BI=Import("zklBigNum");  // libGMP
// saving 664,578 primes (vs generating them on the fly) seems a bit overkill

fcn safePrime(p){ ((p-1)/2).probablyPrime() } // p is a BigInt prime

fcn safetyList(sN,nsN){
   p,safe,notSafe := BI(2),List(),List();
   do{ 
      if(safePrime(p)) safe.append(p.toInt()) else notSafe.append(p.toInt()); 
      p.nextPrime();
   }while(safe.len()<sN or notSafe.len()<nsN);
   println("The first %d   safe primes are: %s".fmt(sN,safe[0,sN].concat(",")));
   println("The first %d unsafe primes are: %s".fmt(nsN,notSafe[0,nsN].concat(",")));
}(35,40);
```

{{out}}

```txt

The first 35   safe primes are: 5,7,11,23,47,59,83,107,167,179,227,263,347,359,383,467,479,503,563,587,719,839,863,887,983,1019,1187,1283,1307,1319,1367,1439,1487,1523,1619
The first 40 unsafe primes are: 2,3,13,17,19,29,31,37,41,43,53,61,67,71,73,79,89,97,101,103,109,113,127,131,137,139,149,151,157,163,173,181,191,193,197,199,211,223,229,233

```

safetyList could also be written as:

```zkl
println("The first %d  safe primes are: %s".fmt(N:=35,
   Walker(BI(1).nextPrime)  // gyrate (vs Walker.filter) because p mutates
     .pump(N,String,safePrime,Void.Filter,String.fp1(","))));
println("The first %d unsafe primes are: %s".fmt(N=40,
   Walker(BI(1).nextPrime)	// or save as List
     .pump(N,List,safePrime,'==(False),Void.Filter,"toInt").concat(",")));
```

Time to count:

```zkl
fcn safetyCount(N,s=0,ns=0,p=BI(2)){
   do{ 
      if(safePrime(p)) s+=1; else ns+=1;
      p.nextPrime()
   }while(p<N);
   println("The number of   safe primes below %10,d is %7,d".fmt(N,s));
   println("The number of unsafe primes below %10,d is %7,d".fmt(N,ns));
   return(s,ns,p);
}

s,ns,p := safetyCount(1_000_000);
println();
safetyCount(10_000_000,s,ns,p);
```

{{out}}

```txt

The number of   safe primes below  1,000,000 is   4,324
The number of unsafe primes below  1,000,000 is  74,174

The number of   safe primes below 10,000,000 is  30,657
The number of unsafe primes below 10,000,000 is 633,922

```

