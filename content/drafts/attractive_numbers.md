+++
title = "Attractive numbers"
description = ""
date = 2019-10-22T01:13:24Z
aliases = []
[extra]
id = 22260
[taxonomies]
categories = []
tags = []
+++

{{task}}

A number is an   ''attractive number''   if the number of its prime factors (whether distinct or not) is also prime.


;Example:
The number   '''20''',   whose prime decomposition is   '''2 &times; 2 &times; 5''',   is an   ''attractive number''   because the number of its prime factors   ('''3''')   is also prime.


;Task:
Show sequence items up to   '''120'''.


;Reference:
:*   The OEIS entry:   [http://oeis.org/A063989 A063989 attractive numbers].





## AWK


```AWK

# syntax: GAWK -f ATTRACTIVE_NUMBERS.AWK
# converted from C
BEGIN {
    limit = 120
    printf("attractive numbers from 1-%d:\n",limit)
    for (i=1; i<=limit; i++) {
      n = count_prime_factors(i)
      if (is_prime(n)) {
        printf("%d ",i)
      }
    }
    printf("\n")
    exit(0)
}
function count_prime_factors(n,  count,f) {
    f = 2
    if (n == 1) { return(0) }
    if (is_prime(n)) { return(1) }
    while (1) {
      if (!(n % f)) {
        count++
        n /= f
        if (n == 1) { return(count) }
        if (is_prime(n)) { f = n }
      }
      else if (f >= 3) { f += 2 }
      else { f = 3 }
    }
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

attractive numbers from 1-120:
4 6 8 9 10 12 14 15 18 20 21 22 25 26 27 28 30 32 33 34 35 38 39 42 44 45 46 48 49 50 51 52 55 57 58 62 63 65 66 68 69 70 72 74 75 76 77 78 80 82 85 86 87 91 92 93 94 95 98 99 102 105 106 108 110 111 112 114 115 116 117 118 119 120

```


## C

{{trans|Go}}

```c
#include <stdio.h>

#define TRUE 1
#define FALSE 0
#define MAX 120

typedef int bool;

bool is_prime(int n) {
    int d = 5;
    if (n < 2) return FALSE;
    if (!(n % 2)) return n == 2;
    if (!(n % 3)) return n == 3;
    while (d *d <= n) {
        if (!(n % d)) return FALSE;
        d += 2;
        if (!(n % d)) return FALSE;
        d += 4;
    }
    return TRUE;
}

int count_prime_factors(int n) {
    int count = 0, f = 2;
    if (n == 1) return 0;
    if (is_prime(n)) return 1;
    while (TRUE) {
        if (!(n % f)) {
            count++;
            n /= f;
            if (n == 1) return count;
            if (is_prime(n)) f = n;
        }
        else if (f >= 3) f += 2;
        else f = 3;
    }
}

int main() {
    int i, n, count = 0;
    printf("The attractive numbers up to and including %d are:\n", MAX);
    for (i = 1; i <= MAX; ++i) {
        n = count_prime_factors(i);
        if (is_prime(n)) {
            printf("%4d", i);
            if (!(++count % 20)) printf("\n");
        }
    }
    printf("\n");
    return 0;
}
```


{{out}}

```txt

The attractive numbers up to and including 120 are:
   4   6   8   9  10  12  14  15  18  20  21  22  25  26  27  28  30  32  33  34
  35  38  39  42  44  45  46  48  49  50  51  52  55  57  58  62  63  65  66  68
  69  70  72  74  75  76  77  78  80  82  85  86  87  91  92  93  94  95  98  99
 102 105 106 108 110 111 112 114 115 116 117 118 119 120

```



## C++

{{trans|C}}

```cpp
#include <iostream>
#include <iomanip>

#define MAX 120

using namespace std;

bool is_prime(int n) {
    if (n < 2) return false;
    if (!(n % 2)) return n == 2;
    if (!(n % 3)) return n == 3;
    int d = 5;
    while (d *d <= n) {
        if (!(n % d)) return false;
        d += 2;
        if (!(n % d)) return false;
        d += 4;
    }
    return true;
}

int count_prime_factors(int n) {
    if (n == 1) return 0;
    if (is_prime(n)) return 1;
    int count = 0, f = 2;
    while (true) {
        if (!(n % f)) {
            count++;
            n /= f;
            if (n == 1) return count;
            if (is_prime(n)) f = n;
        }
        else if (f >= 3) f += 2;
        else f = 3;
    }
}

int main() {
    cout << "The attractive numbers up to and including " << MAX << " are:" << endl;
    for (int i = 1, count = 0; i <= MAX; ++i) {
        int n = count_prime_factors(i);
        if (is_prime(n)) {
            cout << setw(4) << i;
            if (!(++count % 20)) cout << endl;
        }
    }
    cout << endl;
    return 0;
}
```


{{out}}

```txt

The attractive numbers up to and including 120 are:
   4   6   8   9  10  12  14  15  18  20  21  22  25  26  27  28  30  32  33  34
  35  38  39  42  44  45  46  48  49  50  51  52  55  57  58  62  63  65  66  68
  69  70  72  74  75  76  77  78  80  82  85  86  87  91  92  93  94  95  98  99
 102 105 106 108 110 111 112 114 115 116 117 118 119 120

```


## C#
{{trans|D}}

```c#
using System;

namespace AttractiveNumbers {
    class Program {
        const int MAX = 120;

        static bool IsPrime(int n) {
            if (n < 2) return false;
            if (n % 2 == 0) return n == 2;
            if (n % 3 == 0) return n == 3;
            int d = 5;
            while (d * d <= n) {
                if (n % d == 0) return false;
                d += 2;
                if (n % d == 0) return false;
                d += 4;
            }
            return true;
        }

        static int PrimeFactorCount(int n) {
            if (n == 1) return 0;
            if (IsPrime(n)) return 1;
            int count = 0;
            int f = 2;
            while (true) {
                if (n % f == 0) {
                    count++;
                    n /= f;
                    if (n == 1) return count;
                    if (IsPrime(n)) f = n;
                } else if (f >= 3) {
                    f += 2;
                } else {
                    f = 3;
                }
            }
        }

        static void Main(string[] args) {
            Console.WriteLine("The attractive numbers up to and including {0} are:", MAX);
            int i = 1;
            int count = 0;
            while (i <= MAX) {
                int n = PrimeFactorCount(i);
                if (IsPrime(n)) {
                    Console.Write("{0,4}", i);
                    if (++count % 20 == 0) Console.WriteLine();
                }
                ++i;
            }
            Console.WriteLine();
        }
    }
}
```

{{out}}

```txt
The attractive numbers up to and including 120 are:
   4   6   8   9  10  12  14  15  18  20  21  22  25  26  27  28  30  32  33  34
  35  38  39  42  44  45  46  48  49  50  51  52  55  57  58  62  63  65  66  68
  69  70  72  74  75  76  77  78  80  82  85  86  87  91  92  93  94  95  98  99
 102 105 106 108 110 111 112 114 115 116 117 118 119 120
```



## D

{{trans|C++}}

```d
import std.stdio;

enum MAX = 120;

bool isPrime(int n) {
    if (n < 2) return false;
    if (n % 2 == 0) return n == 2;
    if (n % 3 == 0) return n == 3;
    int d = 5;
    while (d * d <= n) {
        if (n % d == 0) return false;
        d += 2;
        if (n % d == 0) return false;
        d += 4;
    }
    return true;
}

int primeFactorCount(int n) {
    if (n == 1) return 0;
    if (isPrime(n)) return 1;
    int count;
    int f = 2;
    while (true) {
        if (n % f == 0) {
            count++;
            n /= f;
            if (n == 1) return count;
            if (isPrime(n)) f = n;
        } else if (f >= 3) {
            f += 2;
        } else {
            f = 3;
        }
    }
}

void main() {
    writeln("The attractive numbers up to and including ", MAX, " are:");
    int i = 1;
    int count;
    while (i <= MAX) {
        int n = primeFactorCount(i);
        if (isPrime(n)) {
            writef("%4d", i);
            if (++count % 20 == 0) writeln;
        }
        ++i;
    }
    writeln;
}
```

{{out}}

```txt
The attractive numbers up to and including 120 are:
   4   6   8   9  10  12  14  15  18  20  21  22  25  26  27  28  30  32  33  34
  35  38  39  42  44  45  46  48  49  50  51  52  55  57  58  62  63  65  66  68
  69  70  72  74  75  76  77  78  80  82  85  86  87  91  92  93  94  95  98  99
 102 105 106 108 110 111 112 114 115 116 117 118 119 120
```



## Factor

{{works with|Factor|0.99}}

```factor
USING: formatting grouping io math.primes math.primes.factors
math.ranges sequences ;

"The attractive numbers up to and including 120 are:" print
120 [1,b] [ factors length prime? ] filter 20 <groups>
[ [ "%4d" printf ] each nl ] each
```

{{out}}

```txt

The attractive numbers up to and including 120 are:
   4   6   8   9  10  12  14  15  18  20  21  22  25  26  27  28  30  32  33  34
  35  38  39  42  44  45  46  48  49  50  51  52  55  57  58  62  63  65  66  68
  69  70  72  74  75  76  77  78  80  82  85  86  87  91  92  93  94  95  98  99
 102 105 106 108 110 111 112 114 115 116 117 118 119 120

```



## FreeBASIC

{{trans|D}}

```freebasic

Const limite = 120

Declare Function esPrimo(n As Integer) As Boolean
Declare Function ContandoFactoresPrimos(n As Integer) As Integer

Function esPrimo(n As Integer) As Boolean
    If n < 2 Then Return false
    If n Mod 2 = 0 Then Return n = 2
    If n Mod 3 = 0 Then Return n = 3
    Dim As Integer d = 5
    While d * d <= n
        If n Mod d = 0 Then Return false
        d += 2
        If n Mod d = 0 Then Return false
        d += 4
    Wend
    Return true
End Function

Function ContandoFactoresPrimos(n As Integer) As Integer
    If n = 1 Then Return false
    If esPrimo(n) Then Return true
    Dim As Integer f = 2, contar = 0
    While true
        If n Mod f = 0 Then
            contar += 1
            n = n / f
            If n = 1 Then Return contar
            If esPrimo(n) Then f = n
        Elseif f >= 3 Then
            f += 2
        Else
            f = 3
        End If
    Wend
End Function

' Mostrar la sucencia de números atractivos hasta 120.
Dim As Integer i = 1, longlinea = 0

Print "Los numeros atractivos hasta e incluyendo"; limite; " son: "
While i <= limite
    Dim As Integer n = ContandoFactoresPrimos(i)
    If esPrimo(n) Then
        Print Using "####"; i;
        longlinea += 1: If longlinea Mod 20 = 0 Then Print ""
    End If
    i += 1
Wend
End

```

{{out}}

```txt

Los numeros atractivos hasta e incluyendo 120 son:
   4   6   8   9  10  12  14  15  18  20  21  22  25  26  27  28  30  32  33  34
  35  38  39  42  44  45  46  48  49  50  51  52  55  57  58  62  63  65  66  68
  69  70  72  74  75  76  77  78  80  82  85  86  87  91  92  93  94  95  98  99
 102 105 106 108 110 111 112 114 115 116 117 118 119 120

```



## Go

Simple functions to test for primality and to count prime factors suffice here.

```go
package main

import "fmt"

func isPrime(n int) bool {
    switch {
    case n < 2:
        return false
    case n%2 == 0:
        return n == 2
    case n%3 == 0:
        return n == 3
    default:
        d := 5
        for d*d <= n {
            if n%d == 0 {
                return false
            }
            d += 2
            if n%d == 0 {
                return false
            }
            d += 4
        }
        return true
    }
}

func countPrimeFactors(n int) int {
    switch {
    case n == 1:
        return 0
    case isPrime(n):
        return 1
    default:
        count, f := 0, 2
        for {
            if n%f == 0 {
                count++
                n /= f
                if n == 1 {
                    return count
                }
                if isPrime(n) {
                    f = n
                }
            } else if f >= 3 {
                f += 2
            } else {
                f = 3
            }
        }
        return count
    }
}

func main() {
    const max = 120
    fmt.Println("The attractive numbers up to and including", max, "are:")
    count := 0
    for i := 1; i <= max; i++ {
        n := countPrimeFactors(i)
        if isPrime(n) {
            fmt.Printf("%4d", i)
            count++
            if count % 20 == 0 {
                fmt.Println()
            }
        }
    }
    fmt.Println()
}
```


{{out}}

```txt

The attractive numbers up to and including 120 are:
   4   6   8   9  10  12  14  15  18  20  21  22  25  26  27  28  30  32  33  34
  35  38  39  42  44  45  46  48  49  50  51  52  55  57  58  62  63  65  66  68
  69  70  72  74  75  76  77  78  80  82  85  86  87  91  92  93  94  95  98  99
 102 105 106 108 110 111 112 114 115 116 117 118 119 120

```



## Groovy

{{trans|Java}}

```groovy
class AttractiveNumbers {
    static boolean isPrime(int n) {
        if (n < 2) return false
        if (n % 2 == 0) return n == 2
        if (n % 3 == 0) return n == 3
        int d = 5
        while (d * d <= n) {
            if (n % d == 0) return false
            d += 2
            if (n % d == 0) return false
            d += 4
        }
        return true
    }

    static int countPrimeFactors(int n) {
        if (n == 1) return 0
        if (isPrime(n)) return 1
        int count = 0, f = 2
        while (true) {
            if (n % f == 0) {
                count++
                n /= f
                if (n == 1) return count
                if (isPrime(n)) f = n
            } else if (f >= 3) f += 2
            else f = 3
        }
    }

    static void main(String[] args) {
        final int max = 120
        printf("The attractive numbers up to and including %d are:\n", max)
        int count = 0
        for (int i = 1; i <= max; ++i) {
            int n = countPrimeFactors(i)
            if (isPrime(n)) {
                printf("%4d", i)
                if (++count % 20 == 0) println()
            }
        }
        println()
    }
}
```

{{out}}

```txt
The attractive numbers up to and including 120 are:
   4   6   8   9  10  12  14  15  18  20  21  22  25  26  27  28  30  32  33  34
  35  38  39  42  44  45  46  48  49  50  51  52  55  57  58  62  63  65  66  68
  69  70  72  74  75  76  77  78  80  82  85  86  87  91  92  93  94  95  98  99
 102 105 106 108 110 111 112 114 115 116 117 118 119 120
```



## Haskell


```haskell
import Data.Numbers.Primes
import Data.Bool (bool)

attractiveNumbers :: [Integer]
attractiveNumbers =
  [1 ..] >>= (bool [] . return) <*> (isPrime . length . primeFactors)

main :: IO ()
main = print $ takeWhile (<= 120) attractiveNumbers
```


Or equivalently, as a list comprehension:

```haskell
import Data.Numbers.Primes

attractiveNumbers :: [Integer]
attractiveNumbers =
  [ x
  | x <- [1 ..]
  , isPrime (length (primeFactors x)) ]

main :: IO ()
main = print $ takeWhile (<= 120) attractiveNumbers
```


Or simply:

```haskell
import Data.Numbers.Primes

attractiveNumbers :: [Integer]
attractiveNumbers =
  filter
    (isPrime . length . primeFactors)
    [1 ..]

main :: IO ()
main = print $ takeWhile (<= 120) attractiveNumbers
```

{{Out}}

```txt
[4,6,8,9,10,12,14,15,18,20,21,22,25,26,27,28,30,32,33,34,35,38,39,42,44,45,46,48,49,50,51,52,55,57,58,62,63,65,66,68,69,70,72,74,75,76,77,78,80,82,85,86,87,91,92,93,94,95,98,99,102,105,106,108,110,111,112,114,115,116,117,118,119,120]
```



## Java

{{trans|C}}

```java
public class Attractive {

    static boolean is_prime(int n) {
        if (n < 2) return false;
        if (n % 2 == 0) return n == 2;
        if (n % 3 == 0) return n == 3;
        int d = 5;
        while (d *d <= n) {
            if (n % d == 0) return false;
            d += 2;
            if (n % d == 0) return false;
            d += 4;
        }
        return true;
    }

    static int count_prime_factors(int n) {
        if (n == 1) return 0;
        if (is_prime(n)) return 1;
        int count = 0, f = 2;
        while (true) {
            if (n % f == 0) {
                count++;
                n /= f;
                if (n == 1) return count;
                if (is_prime(n)) f = n;
            }
            else if (f >= 3) f += 2;
            else f = 3;
        }
    }

    public static void main(String[] args) {
        final int max = 120;
        System.out.printf("The attractive numbers up to and including %d are:\n", max);
        for (int i = 1, count = 0; i <= max; ++i) {
            int n = count_prime_factors(i);
            if (is_prime(n)) {
                System.out.printf("%4d", i);
                if (++count % 20 == 0) System.out.println();
            }
        }
        System.out.println();
    }
}
```


{{out}}

```txt

The attractive numbers up to and including 120 are:
   4   6   8   9  10  12  14  15  18  20  21  22  25  26  27  28  30  32  33  34
  35  38  39  42  44  45  46  48  49  50  51  52  55  57  58  62  63  65  66  68
  69  70  72  74  75  76  77  78  80  82  85  86  87  91  92  93  94  95  98  99
 102 105 106 108 110 111 112 114 115 116 117 118 119 120

```



## Julia


```julia
using Primes

# oneliner is println("The attractive numbers from 1 to 120 are:\n", filter(x -> isprime(sum(values(factor(x)))), 1:120))

isattractive(n) = isprime(sum(values(factor(n))))

printattractive(m, n) = println("The attractive numbers from $m to $n are:\n", filter(isattractive, m:n))

printattractive(1, 120)

```
{{out}}

```txt

The attractive numbers from 1 to 120 are:
[4, 6, 8, 9, 10, 12, 14, 15, 18, 20, 21, 22, 25, 26, 27, 28, 30, 32, 33, 34, 35, 38, 39, 42, 44, 45, 46, 48, 49, 50, 51, 52, 55, 57, 58, 62, 63, 65, 66, 68, 69, 70, 72, 74, 75, 76, 77, 78, 80, 82, 85, 86, 87, 91, 92, 93, 94, 95, 98, 99, 102, 105, 106, 108, 110, 111, 112, 114, 115, 116, 117, 118, 119, 120]

```



## Kotlin

{{trans|Go}}

```scala
// Version 1.3.21

const val MAX = 120

fun isPrime(n: Int) : Boolean {
    if (n < 2) return false
    if (n % 2 == 0) return n == 2
    if (n % 3 == 0) return n == 3
    var d : Int = 5
    while (d * d <= n) {
        if (n % d == 0) return false
        d += 2
        if (n % d == 0) return false
        d += 4
    }
    return true
}

fun countPrimeFactors(n: Int) =
    when {
        n == 1  -> 0
        isPrime(n) -> 1
        else -> {
            var nn = n
            var count = 0
            var f = 2
            while (true) {
                if (nn % f == 0) {
                    count++
                    nn /= f
                    if (nn == 1) break
                    if (isPrime(nn)) f = nn
                } else if (f >= 3) {
                    f += 2
                } else {
                    f = 3
                }
            }
            count
        }
    }

fun main() {
    println("The attractive numbers up to and including $MAX are:")
    var count = 0
    for (i in 1..MAX) {
        val n = countPrimeFactors(i)
        if (isPrime(n)) {
            System.out.printf("%4d", i)
            if (++count % 20 == 0) println()
        }
    }
    println()
}
```


{{output}}

```txt

The attractive numbers up to and including 120 are:
   4   6   8   9  10  12  14  15  18  20  21  22  25  26  27  28  30  32  33  34
  35  38  39  42  44  45  46  48  49  50  51  52  55  57  58  62  63  65  66  68
  69  70  72  74  75  76  77  78  80  82  85  86  87  91  92  93  94  95  98  99
 102 105 106 108 110 111 112 114 115 116 117 118 119 120

```



## LLVM


```llvm
; This is not strictly LLVM, as it uses the C library function "printf".
; LLVM does not provide a way to print values, so the alternative would be
; to just load the string into memory, and that would be boring.

$"ATTRACTIVE_STR" = comdat any
$"FORMAT_NUMBER" = comdat any
$"NEWLINE_STR" = comdat any

@"ATTRACTIVE_STR" = linkonce_odr unnamed_addr constant [52 x i8] c"The attractive numbers up to and including %d are:\0A\00", comdat, align 1
@"FORMAT_NUMBER" = linkonce_odr unnamed_addr constant [4 x i8] c"%4d\00", comdat, align 1
@"NEWLINE_STR" = linkonce_odr unnamed_addr constant [2 x i8] c"\0A\00", comdat, align 1

;--- The declaration for the external C printf function.
declare i32 @printf(i8*, ...)

; Function Attrs: noinline nounwind optnone uwtable
define zeroext i1 @is_prime(i32) #0 {
  %2 = alloca i1, align 1               ;-- allocate return value
  %3 = alloca i32, align 4              ;-- allocate n
  %4 = alloca i32, align 4              ;-- allocate d
  store i32 %0, i32* %3, align 4        ;-- store local copy of n
  store i32 5, i32* %4, align 4         ;-- store 5 in d
  %5 = load i32, i32* %3, align 4       ;-- load n
  %6 = icmp slt i32 %5, 2               ;-- n < 2
  br i1 %6, label %nlt2, label %niseven

nlt2:
  store i1 false, i1* %2, align 1       ;-- store false in return value
  br label %exit

niseven:
  %7 = load i32, i32* %3, align 4       ;-- load n
  %8 = srem i32 %7, 2                   ;-- n % 2
  %9 = icmp ne i32 %8, 0                ;-- (n % 2) != 0
  br i1 %9, label %odd, label %even

even:
  %10 = load i32, i32* %3, align 4      ;-- load n
  %11 = icmp eq i32 %10, 2              ;-- n == 2
  store i1 %11, i1* %2, align 1         ;-- store (n == 2) in return value
  br label %exit

odd:
  %12 = load i32, i32* %3, align 4      ;-- load n
  %13 = srem i32 %12, 3                 ;-- n % 3
  %14 = icmp ne i32 %13, 0              ;-- (n % 3) != 0
  br i1 %14, label %loop, label %div3

div3:
  %15 = load i32, i32* %3, align 4      ;-- load n
  %16 = icmp eq i32 %15, 3              ;-- n == 3
  store i1 %16, i1* %2, align 1         ;-- store (n == 3) in return value
  br label %exit

loop:
  %17 = load i32, i32* %4, align 4      ;-- load d
  %18 = load i32, i32* %4, align 4      ;-- load d
  %19 = mul nsw i32 %17, %18            ;-- d * d
  %20 = load i32, i32* %3, align 4      ;-- load n
  %21 = icmp sle i32 %19, %20           ;-- (d * d) <= n
  br i1 %21, label %first, label %prime

first:
  %22 = load i32, i32* %3, align 4      ;-- load n
  %23 = load i32, i32* %4, align 4      ;-- load d
  %24 = srem i32 %22, %23               ;-- n % d
  %25 = icmp ne i32 %24, 0              ;-- (n % d) != 0
  br i1 %25, label %second, label %notprime

second:
  %26 = load i32, i32* %4, align 4      ;-- load d
  %27 = add nsw i32 %26, 2              ;-- increment d by 2
  store i32 %27, i32* %4, align 4       ;-- store d
  %28 = load i32, i32* %3, align 4      ;-- load n
  %29 = load i32, i32* %4, align 4      ;-- load d
  %30 = srem i32 %28, %29               ;-- n % d
  %31 = icmp ne i32 %30, 0              ;-- (n % d) != 0
  br i1 %31, label %loop_end, label %notprime

loop_end:
  %32 = load i32, i32* %4, align 4      ;-- load d
  %33 = add nsw i32 %32, 4              ;-- increment d by 4
  store i32 %33, i32* %4, align 4       ;-- store d
  br label %loop

notprime:
  store i1 false, i1* %2, align 1       ;-- store false in return value
  br label %exit

prime:
  store i1 true, i1* %2, align 1        ;-- store true in return value
  br label %exit

exit:
  %34 = load i1, i1* %2, align 1        ;-- load return value
  ret i1 %34
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @count_prime_factors(i32) #0 {
  %2 = alloca i32, align 4                      ;-- allocate return value
  %3 = alloca i32, align 4                      ;-- allocate n
  %4 = alloca i32, align 4                      ;-- allocate count
  %5 = alloca i32, align 4                      ;-- allocate f
  store i32 %0, i32* %3, align 4                ;-- store local copy of n
  store i32 0, i32* %4, align 4                 ;-- store zero in count
  store i32 2, i32* %5, align 4                 ;-- store 2 in f
  %6 = load i32, i32* %3, align 4               ;-- load n
  %7 = icmp eq i32 %6, 1                        ;-- n == 1
  br i1 %7, label %eq1, label %ne1

eq1:
  store i32 0, i32* %2, align 4                 ;-- store zero in return value
  br label %exit

ne1:
  %8 = load i32, i32* %3, align 4               ;-- load n
  %9 = call zeroext i1 @is_prime(i32 %8)        ;-- is n prime?
  br i1 %9, label %prime, label %loop

prime:
  store i32 1, i32* %2, align 4                 ;-- store a in return value
  br label %exit

loop:
  %10 = load i32, i32* %3, align 4              ;-- load n
  %11 = load i32, i32* %5, align 4              ;-- load f
  %12 = srem i32 %10, %11                       ;-- n % f
  %13 = icmp ne i32 %12, 0                      ;-- (n % f) != 0
  br i1 %13, label %br2, label %br1

br1:
  %14 = load i32, i32* %4, align 4              ;-- load count
  %15 = add nsw i32 %14, 1                      ;-- increment count
  store i32 %15, i32* %4, align 4               ;-- store count
  %16 = load i32, i32* %5, align 4              ;-- load f
  %17 = load i32, i32* %3, align 4              ;-- load n
  %18 = sdiv i32 %17, %16                       ;-- n / f
  store i32 %18, i32* %3, align 4               ;-- n = n / f
  %19 = load i32, i32* %3, align 4              ;-- load n
  %20 = icmp eq i32 %19, 1                      ;-- n == 1
  br i1 %20, label %br1_1, label %br1_2

br1_1:
  %21 = load i32, i32* %4, align 4              ;-- load count
  store i32 %21, i32* %2, align 4               ;-- store the count in the return value
  br label %exit

br1_2:
  %22 = load i32, i32* %3, align 4              ;-- load n
  %23 = call zeroext i1 @is_prime(i32 %22)      ;-- is n prime?
  br i1 %23, label %br1_3, label %loop

br1_3:
  %24 = load i32, i32* %3, align 4              ;-- load n
  store i32 %24, i32* %5, align 4               ;-- f = n
  br label %loop

br2:
  %25 = load i32, i32* %5, align 4              ;-- load f
  %26 = icmp sge i32 %25, 3                     ;-- f >= 3
  br i1 %26, label %br2_1, label %br3

br2_1:
  %27 = load i32, i32* %5, align 4              ;-- load f
  %28 = add nsw i32 %27, 2                      ;-- increment f by 2
  store i32 %28, i32* %5, align 4               ;-- store f
  br label %loop

br3:
  store i32 3, i32* %5, align 4                 ;-- store 3 in f
  br label %loop

exit:
  %29 = load i32, i32* %2, align 4              ;-- load return value
  ret i32 %29
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4                      ;-- allocate i
  %2 = alloca i32, align 4                      ;-- allocate n
  %3 = alloca i32, align 4                      ;-- count
  store i32 0, i32* %3, align 4                 ;-- store zero in count
  %4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([52 x i8], [52 x i8]* @"ATTRACTIVE_STR", i32 0, i32 0), i32 120)
  store i32 1, i32* %1, align 4                 ;-- store 1 in i
  br label %loop

loop:
  %5 = load i32, i32* %1, align 4               ;-- load i
  %6 = icmp sle i32 %5, 120                     ;-- i <= 120
  br i1 %6, label %loop_body, label %exit

loop_body:
  %7 = load i32, i32* %1, align 4               ;-- load i
  %8 = call i32 @count_prime_factors(i32 %7)    ;-- count factors of i
  store i32 %8, i32* %2, align 4                ;-- store factors in n
  %9 = call zeroext i1 @is_prime(i32 %8)        ;-- is n prime?
  br i1 %9, label %prime_branch, label %loop_inc

prime_branch:
  %10 = load i32, i32* %1, align 4              ;-- load i
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @"FORMAT_NUMBER", i32 0, i32 0), i32 %10)
  %12 = load i32, i32* %3, align 4              ;-- load count
  %13 = add nsw i32 %12, 1                      ;-- increment count
  store i32 %13, i32* %3, align 4               ;-- store count
  %14 = srem i32 %13, 20                        ;-- count % 20
  %15 = icmp ne i32 %14, 0                      ;-- (count % 20) != 0
  br i1 %15, label %loop_inc, label %row_end

row_end:
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @"NEWLINE_STR", i32 0, i32 0))
  br label %loop_inc

loop_inc:
  %17 = load i32, i32* %1, align 4              ;-- load i
  %18 = add nsw i32 %17, 1                      ;-- increment i
  store i32 %18, i32* %1, align 4               ;-- store i
  br label %loop

exit:
  %19 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @"NEWLINE_STR", i32 0, i32 0))
  ret i32 0
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
```

{{out}}

```txt
The attractive numbers up to and including 120 are:
   4   6   8   9  10  12  14  15  18  20  21  22  25  26  27  28  30  32  33  34
  35  38  39  42  44  45  46  48  49  50  51  52  55  57  58  62  63  65  66  68
  69  70  72  74  75  76  77  78  80  82  85  86  87  91  92  93  94  95  98  99
 102 105 106 108 110 111 112 114 115 116 117 118 119 120
```



## Lua


```lua
-- Returns true if x is prime, and false otherwise
function isPrime (x)
  if x < 2 then return false end
  if x < 4 then return true end
  if x % 2 == 0 then return false end
  for d = 3, math.sqrt(x), 2 do
    if x % d == 0 then return false end
  end
  return true
end

-- Compute the prime factors of n
function factors (n)
  local facList, divisor, count = {}, 1
  if n < 2 then return facList end
  while not isPrime(n) do
    while not isPrime(divisor) do divisor = divisor + 1 end
    count = 0
    while n % divisor == 0 do
      n = n / divisor
      table.insert(facList, divisor)
    end
    divisor = divisor + 1
    if n == 1 then return facList end
  end
  table.insert(facList, n)
  return facList
end

-- Main procedure
for i = 1, 120 do
  if isPrime(#factors(i)) then io.write(i .. "\t") end
end
```

{{out}}

```txt
4       6       8       9       10      12      14      15      18      20      21      22      25      26      27
28      30      32      33      34      35      38      39      42      44      45      46      48      49      50
51      52      55      57      58      62      63      65      66      68      69      70      72      74      75
76      77      78      80      82      85      86      87      91      92      93      94      95      98      99
102     105     106     108     110     111     112     114     115     116     117     118     119     120
```



## Maple


```Maple
attractivenumbers := proc(n::posint)
local an, i;
an :=[]:
for i from 1 to n do
    if isprime(NumberTheory:-NumberOfPrimeFactors(i)) then
        an := [op(an), i]:
    end if:
end do:
end proc:
attractivenumbers(120);
```

{{out}}

```txt
[4, 6, 8, 9, 10, 12, 14, 15, 18, 20, 21, 22, 25, 26, 27, 28, 30, 32, 33, 34, 35, 38, 39, 42, 44, 45, 46, 48, 49, 50, 51, 52, 55, 57, 58, 62, 63, 65, 66, 68, 69, 70, 72, 74, 75, 76, 77, 78, 80, 82, 85, 86, 87, 91, 92, 93, 94, 95, 98, 99, 102, 105, 106, 108, 110, 111, 112, 114, 115, 116, 117, 118, 119, 120]
```


## Nim

{{trans|C}}

```Nim
import strformat

const MAX = 120

proc isPrime(n: int): bool =
  var d = 5
  if n < 2:
    return false
  if n mod 2 == 0:
    return n == 2
  if n mod 3 == 0:
    return n == 3
  while d * d <= n:
    if n mod d == 0:
      return false
    inc d, 2
    if n mod d == 0:
      return false
    inc d, 4
  return true

proc countPrimeFactors(n_in: int): int =
  var count = 0
  var f = 2
  var n = n_in
  if n == 1:
    return 0
  if isPrime(n):
    return 1
  while true:
    if n mod f == 0:
      inc count
      n = n div f
      if n == 1:
        return count
      if isPrime(n):
        f = n
    elif (f >= 3):
      inc f, 2
    else:
      f = 3

proc main() =
  var n, count: int = 0
  echo fmt"The attractive numbers up to and including {MAX} are:"
  for i in 1..MAX:
    n = countPrimeFactors(i)
    if isPrime(n):
      write(stdout, fmt"{i:4d}")
      inc count
      if count mod 20 == 0:
        write(stdout, "\n")
  write(stdout, "\n")

main()

```

{{out}}

```txt
The attractive numbers up to and including 120 are:
   4   6   8   9  10  12  14  15  18  20  21  22  25  26  27  28  30  32  33  34
  35  38  39  42  44  45  46  48  49  50  51  52  55  57  58  62  63  65  66  68
  69  70  72  74  75  76  77  78  80  82  85  86  87  91  92  93  94  95  98  99
 102 105 106 108 110 111 112 114 115 116 117 118 119 120
```



## Perl

{{libheader|ntheory}}

```perl
use ntheory <is_prime factor>;

is_prime +factor $_ and print "$_ " for 1..120;
```

{{out}}

```txt
4 6 8 9 10 12 14 15 18 20 21 22 25 26 27 28 30 32 33 34 35 38 39 42 44 45 46 48 49 50 51 52 55 57 58 62 63 65 66 68 69 70 72 74 75 76 77 78 80 82 85 86 87 91 92 93 94 95 98 99 102 105 106 108 110 111 112 114 115 116 117 118 119 120
```



## Perl 6

{{works with|Rakudo|2019.03}}
This algorithm is concise but not really well suited to finding large quantities of consecutive attractive numbers. It works, but isn't especially speedy. More than a hundred thousand or so gets tedious. There are other, much faster (though more verbose) algorithms that ''could'' be used. This algorithm '''is''' well suited to finding '''arbitrary''' attractive numbers though.


```perl6
use Lingua::EN::Numbers;
use ntheory:from<Perl5> <factor is_prime>;

sub display ($n,$m) { ($n..$m).grep: (~*).&factor.elems.&is_prime }

sub count ($n,$m) { +($n..$m).grep: (~*).&factor.elems.&is_prime }

# The Task
put "Attractive numbers from 1 to 120:\n" ~
display(1, 120)».fmt("%3d").rotor(20, :partial).join: "\n";

# Robusto!
for 1, 1000,  1, 10000, 1, 100000, 2**73 + 1, 2**73 + 100 -> $a, $b {
    put "\nCount of attractive numbers from {comma $a} to {comma $b}:\n" ~
    comma count $a, $b
}
```

{{out}}

```txt
Attractive numbers from 1 to 120:
  4   6   8   9  10  12  14  15  18  20  21  22  25  26  27  28  30  32  33  34
 35  38  39  42  44  45  46  48  49  50  51  52  55  57  58  62  63  65  66  68
 69  70  72  74  75  76  77  78  80  82  85  86  87  91  92  93  94  95  98  99
102 105 106 108 110 111 112 114 115 116 117 118 119 120

Count of attractive numbers from 1 to 1,000:
636

Count of attractive numbers from 1 to 10,000:
6,396

Count of attractive numbers from 1 to 100,000:
63,255

Count of attractive numbers from 9,444,732,965,739,290,427,393 to 9,444,732,965,739,290,427,492:
58
```



## Phix


```Phix
function attractive(integer lim)
    sequence s = {}
    for i=1 to lim do
        integer n = length(prime_factors(i,true))
        if is_prime(n) then s &= i end if
    end for
    return s
end function
sequence s = attractive(120)
printf(1,"There are %d attractive numbers up to and including %d:\n",{length(s),120})
pp(s,{pp_IntCh,false})
for i=3 to 6 do
    atom t0 = time()
    integer p = power(10,i),
            l = length(attractive(p))
    string e = elapsed(time()-t0)
    printf(1,"There are %,d attractive numbers up to %,d (%s)\n",{l,p,e})
end for
```

{{out}}

```txt

There are 74 attractive numbers up to and including 120:
{4,6,8,9,10,12,14,15,18,20,21,22,25,26,27,28,30,32,33,34,35,38,39,42,44,45,
 46,48,49,50,51,52,55,57,58,62,63,65,66,68,69,70,72,74,75,76,77,78,80,82,85,
 86,87,91,92,93,94,95,98,99,102,105,106,108,110,111,112,114,115,116,117,118,
 119,120}
There are 636 attractive numbers up to 1,000 (0s)
There are 6,396 attractive numbers up to 10,000 (0.0s)
There are 63,255 attractive numbers up to 100,000 (0.3s)
There are 617,552 attractive numbers up to 1,000,000 (4.1s)

```



## Python


### Procedural

{{Works with|Python|2.7.12}}

```Python
from sympy import sieve # library for primes

def get_pfct(n):
	i = 2; factors = []
	while i * i <= n:
		if n % i:
			i += 1
		else:
			n //= i
			factors.append(i)
	if n > 1:
		factors.append(n)
	return len(factors)

sieve.extend(110) # first 110 primes...
primes=sieve._list

pool=[]

for each in xrange(0,121):
	pool.append(get_pfct(each))

for i,each in enumerate(pool):
	if each in primes:
		print i,
```

{{out}}

```txt
4,6,8,9,10,12,14,15,18,20,21,22,25,26,27,28,30,32,33,34,35,38,39,42,44,45,46, 48,49,50,51,52,55,57,58,62,63,65,66,68,69,70,72,74,75,76,77,78,80,82,85,86,87, 91,92,93,94,95,98,99,102,105,106,108,110,111,112,114,115,116,117,118,119,120
```



### Functional

Without importing a primes library – at this scale a light and visible implementation is more than enough, and provides more material for comparison.
{{Works with|Python|3.7}}

```python
'''Attractive numbers'''

from itertools import chain, count, takewhile
from functools import reduce


# attractiveNumbers :: () -> [Int]
def attractiveNumbers():
    '''A non-finite stream of attractive numbers.
       (OEIS A063989)
    '''
    return filter(
        compose(
            isPrime,
            len,
            primeDecomposition
        ),
        count(1)
    )


# TEST ----------------------------------------------------
def main():
    '''Attractive numbers drawn from the range [1..120]'''
    for row in chunksOf(15)(list(
            takewhile(
                lambda x: 120 >= x,
                attractiveNumbers()
            )
    )):
        print(' '.join(map(
            compose(justifyRight(3)(' '), str),
            row
        )))


# GENERAL FUNCTIONS ---------------------------------------

# chunksOf :: Int -> [a] -> [[a]]
def chunksOf(n):
    '''A series of lists of length n, subdividing the
       contents of xs. Where the length of xs is not evenly
       divible, the final list will be shorter than n.
    '''
    return lambda xs: reduce(
        lambda a, i: a + [xs[i:n + i]],
        range(0, len(xs), n), []
    ) if 0 < n else []


# compose :: ((a -> a), ...) -> (a -> a)
def compose(*fs):
    '''Composition, from right to left,
       of a series of functions.
    '''
    return lambda x: reduce(
        lambda a, f: f(a),
        fs[::-1], x
    )


# We only need light implementations
# of prime functions here:

# primeDecomposition :: Int -> [Int]
def primeDecomposition(n):
    '''List of integers representing the
       prime decomposition of n.
    '''
    def go(n, p):
        return [p] + go(n // p, p) if (
            0 == n % p
        ) else []
    return list(chain.from_iterable(map(
        lambda p: go(n, p) if isPrime(p) else [],
        range(2, 1 + n)
    )))


# isPrime :: Int -> Bool
def isPrime(n):
    '''True if n is prime.'''
    if n in (2, 3):
        return True
    if 2 > n or 0 == n % 2:
        return False
    if 9 > n:
        return True
    if 0 == n % 3:
        return False

    return not any(map(
        lambda x: 0 == n % x or 0 == n % (2 + x),
        range(5, 1 + int(n ** 0.5), 6)
    ))


# justifyRight :: Int -> Char -> String -> String
def justifyRight(n):
    '''A string padded at left to length n,
       using the padding character c.
    '''
    return lambda c: lambda s: s.rjust(n, c)


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
  4   6   8   9  10  12  14  15  18  20  21  22  25  26  27
 28  30  32  33  34  35  38  39  42  44  45  46  48  49  50
 51  52  55  57  58  62  63  65  66  68  69  70  72  74  75
 76  77  78  80  82  85  86  87  91  92  93  94  95  98  99
102 105 106 108 110 111 112 114 115 116 117 118 119 120
```



## Racket


```racket
#lang racket
(require math/number-theory)
(define attractive? (compose1 prime? prime-omega))
(filter attractive? (range 1 121))
```


{{out}}

```txt

(4 6 8 9 10 12 14 15 18 20 21 22 25 26 27 28 30 32 33 34 35 38 39 42 44 45 46 48 49 50 51 52 55 57 58 62 63 65 66 68 69 70 72 74 75 76 77 78 80 82 85 86 87 91 92 93 94 95 98 99 102 105 106 108 110 111 112 114 115 116 117 118 119 120)

```



## REXX

Programming notes:
The use of a table that contains some low primes is one fast method to test for primality of the

various prime factors.

The   '''cFact'''   (count factors)   function   is optimized way beyond what this task requires,   and it can be optimized

further by expanding the     '''do while'''s     clauses   (lines   3──►6   in the   '''cFact'''   function).

```rexx
/*REXX program finds and shows lists (or counts) attractive numbers up to a specified N.*/
parse arg N .                                    /*get optional argument from the C.L.  */
if N=='' | N==","  then N= 120                   /*Not specified?  Then use the default.*/
cnt= N<0                                         /*semaphore used to control the output.*/
N= abs(N)                                        /*ensure that  N  is a positive number.*/
call genP 100                                    /*gen 100 primes (high= 541); overkill.*/
sw= linesize()  -  1                             /*SW:    is the usable screen width.   */
if \cnt  then say 'attractive numbers up to and including '        N        " are:"
#= 0                                             /*number of attractive #'s  (so far).  */
$=                                               /*a list of attractive numbers (so far)*/
    do j=1  for N;   if @.j  then iterate        /*Is it a prime?  Then skip the number.*/
          a= cFact(j)                            /*call cFact to count the factors in J.*/
    if \@.a  then iterate                        /*if # of factors not prime, then skip.*/
    #= # + 1                                     /*add  the index and number of factors.*/
    if cnt   then iterate                        /*if not displaying numbers, skip list.*/
    _= $  j                                                 /*append a number to $ list.*/
    if length(_)>sw  then do;  say strip($);  $= j;  end    /*display a line of numbers.*/
                     else                     $= _          /*append the latest number. */
    end   /*j*/

if $\==''  &  \cnt   then say strip($)           /*display any residual numbers in list.*/
say;     say #     ' attractive numbers found up to and including '        N
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
cFact: procedure;  parse arg z 1 oz;  if z<2  then return z  /*if Z too small, return Z.*/
       #= 0                                      /*#:  is the number of factors (so far)*/
             do  while z//2==0;  #= #+1;  z= z%2;  end  /*maybe add the factor of two.  */
             do  while z//3==0;  #= #+1;  z= z%3;  end  /*  "    "   "     "    " three.*/
             do  while z//5==0;  #= #+1;  z= z%5;  end  /*  "    "   "     "    " five. */
             do  while z//7==0;  #= #+1;  z= z%7;  end  /*  "    "   "     "    " seven.*/
                                                 /* [↑]  reduce  Z  by some low primes. */
          do k=11  by 6  while k<=z              /*insure that  K  isn't divisible by 3.*/
          parse var k  ''  -1  _                 /*obtain the last decimal digit of  K. */
          if _\==5  then do  while z//k==0;  #= #+1;   z= z%k;   end   /*maybe reduce Z.*/
          if _ ==3  then iterate                 /*Next number ÷ by 5?  Skip.   ____    */
          if k*k>oz then leave                   /*are we  greater  than the   √ OZ  ?  */
          y= k + 2                               /*get next divisor,  hopefully a prime.*/
                         do while  z//y==0;  #= #+1;   z= z%y;   end   /*maybe reduce Z.*/
          end   /*k*/
       if z\==1  then return # + 1               /*if residual isn't unity, then add it.*/
                      return #                   /*return the number of factors in  OZ. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
genP: procedure expose @.; parse arg n;           @.=0;         @.2= 1;     @.3= 1;   p= 2
        do j=3  by 2  until p==n;   do k=3  by 2  until k*k>j;  if j//k==0  then iterate j
                                    end  /*k*/;             @.j = 1;        p= p + 1
        end   /*j*/;          return             /* [↑]  generate  N  primes.           */
```

This REXX program makes use of   '''LINESIZE'''   REXX program (or BIF) which is used to determine the
screen width (or linesize) of the terminal (console).

Some REXXes don't have this BIF.   It is used here to automatically/idiomatically limit the width of the output list.

The   '''LINESIZE.REX'''   REXX program is included here   ───►   [[LINESIZE.REX]].


{{out|output|text=  when using the default input:}}

```txt

attractive numbers up to and including  120  are:
4 6 8 9 10 12 14 15 18 20 21 22 25 26 27 28 30 32 33 34 35 38 39 42 44 45 46 48 49 50 51 52 55 57 58 62 63 65 66 68 69 70 72 74
75 76 77 78 80 82 85 86 87 91 92 93 94 95 98 99 102 105 106 108 110 111 112 114 115 116 117 118 119 120

74  attractive numbers found up to and including  120

```

{{out|output|text=  when using the input of:     <tt> -10000 </tt>}}

```txt

6396  attractive numbers found up to and including  10000

```

{{out|output|text=  when using the input of:     <tt> -100000 </tt>}}

```txt

63255  attractive numbers found up to and including  100000

```

{{out|output|text=  when using the input of:     <tt> -1000000 </tt>}}

```txt

623232  attractive numbers found up to and including  1000000

```



## Ring


```ring

# Project: Attractive Numbers

decomp = []
nump = 0
see "Attractive Numbers up to 120:" + nl
while nump < 120
decomp = []
nump = nump + 1
for i = 1 to nump
    if isPrime(i) and nump%i = 0
       add(decomp,i)
       dec = nump/i
       while dec%i = 0
             add(decomp,i)
             dec = dec/i
       end
    ok
next
if isPrime(len(decomp))
   see string(nump) + " = ["
for n = 1 to len(decomp)
    if n < len(decomp)
       see string(decomp[n]) + "*"
    else
       see string(decomp[n]) + "] - " + len(decomp) + " is prime" + nl
    ok
next
ok
end


func isPrime(num)
     if (num <= 1) return 0 ok
     if (num % 2 = 0) and num != 2 return 0 ok
     for i = 3 to floor(num / 2) -1 step 2
         if (num % i = 0) return 0 ok
     next
     return 1

```

{{out}}

```txt

Attractive Numbers up to 120:
4 = [2*2] - 2 is prime
6 = [2*3] - 2 is prime
8 = [2*2*2] - 3 is prime
9 = [3*3] - 2 is prime
10 = [2*5] - 2 is prime
12 = [2*2*3] - 3 is prime
14 = [2*7] - 2 is prime
15 = [3*5] - 2 is prime
18 = [2*3*3] - 3 is prime
20 = [2*2*5] - 3 is prime
...
...
...
102 = [2*3*17] - 3 is prime
105 = [3*5*7] - 3 is prime
106 = [2*53] - 2 is prime
108 = [2*2*3*3*3] - 5 is prime
110 = [2*5*11] - 3 is prime
111 = [3*37] - 2 is prime
112 = [2*2*2*2*7] - 5 is prime
114 = [2*3*19] - 3 is prime
115 = [5*23] - 2 is prime
116 = [2*2*29] - 3 is prime
117 = [3*3*13] - 3 is prime
118 = [2*59] - 2 is prime
119 = [7*17] - 2 is prime
120 = [2*2*2*3*5] - 5 is prime

```



## Ruby


```ruby
require "prime"

p (1..120).select{|n| n.prime_division.sum(&:last).prime? }

```

{{out}}
```txt
[4, 6, 8, 9, 10, 12, 14, 15, 18, 20, 21, 22, 25, 26, 27, 28, 30, 32, 33, 34, 35, 38, 39, 42, 44, 45, 46, 48, 49, 50, 51, 52, 55, 57, 58, 62, 63, 65, 66, 68, 69, 70, 72, 74, 75, 76, 77, 78, 80, 82, 85, 86, 87, 91, 92, 93, 94, 95, 98, 99, 102, 105, 106, 108, 110, 111, 112, 114, 115, 116, 117, 118, 119, 120]

```



## Rust

Uses [https://crates.io/crates/primal primal]

```rust
use primal::Primes;

const MAX: u64 = 120;

/// Returns an Option with a tuple => Ok((smaller prime factor, num divided by that prime factor))
/// If num is a prime number itself, returns None
fn extract_prime_factor(num: u64) -> Option<(u64, u64)> {
    let mut i = 0;
    if primal::is_prime(num) {
        None
    } else {
        loop {
            let prime = Primes::all().nth(i).unwrap() as u64;
            if num % prime == 0 {
                return Some((prime, num / prime));
            } else {
                i += 1;
            }
        }
    }
}

/// Returns a vector containing all the prime factors of num
fn factorize(num: u64) -> Vec<u64> {
    let mut factorized = Vec::new();
    let mut rest = num;
    while let Some((prime, factorizable_rest)) = extract_prime_factor(rest) {
        factorized.push(prime);
        rest = factorizable_rest;
    }
    factorized.push(rest);
    factorized
}

fn main() {
    let mut output: Vec<u64> = Vec::new();
    for num in 4 ..= MAX {
        if primal::is_prime(factorize(num).len() as u64) {
            output.push(num);
        }
    }
    println!("The attractive numbers up to and including 120 are\n{:?}", output);
}
```

{{out}}
```txt
The attractive numbers up to and including 120 are
[4, 6, 8, 9, 10, 12, 14, 15, 18, 20, 21, 22, 25, 26, 27, 28, 30, 32, 33, 34, 35, 38, 39, 42, 44, 45, 46, 48, 49, 50, 51, 52, 55, 57, 58, 62, 63, 65, 66, 68, 69, 70, 72, 74, 75, 76, 77, 78, 80, 82, 85, 86, 87, 91, 92, 93, 94, 95, 98, 99, 102, 105, 106, 108, 110, 111, 112, 114, 115, 116, 117, 118, 119, 120]
```



## Scala

{{Out}}Best seen in running your browser either by [https://scalafiddle.io/sf/23oE3SQ/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/U0QUQu0uTT24vbDEHU1c0Q Scastie (remote JVM)].

```Scala
object AttractiveNumbers extends App {
  private val max = 120
  private var count = 0

  private def nFactors(n: Int): Int = {
    @scala.annotation.tailrec
    def factors(x: Int, f: Int, acc: Int): Int =
      if (f * f > x) acc + 1
      else
        x % f match {
          case 0 => factors(x / f, f, acc + 1)
          case _ => factors(x, f + 1, acc)
        }

    factors(n, 2, 0)
  }

  private def ls: Seq[String] =
    for (i <- 4 to max;
         n = nFactors(i)
         if n >= 2 && nFactors(n) == 1 // isPrime(n)
         ) yield f"$i%4d($n)"

  println(f"The attractive numbers up to and including $max%d are: [number(factors)]\n")
  ls.zipWithIndex
    .groupBy { case (_, index) => index / 20 }
    .foreach { case (_, row) => println(row.map(_._1).mkString) }
}
```


## Sidef


```ruby
func is_attractive(n) {
    n.bigomega.is_prime
}

1..120 -> grep(is_attractive).say
```

{{out}}

```txt
[4, 6, 8, 9, 10, 12, 14, 15, 18, 20, 21, 22, 25, 26, 27, 28, 30, 32, 33, 34, 35, 38, 39, 42, 44, 45, 46, 48, 49, 50, 51, 52, 55, 57, 58, 62, 63, 65, 66, 68, 69, 70, 72, 74, 75, 76, 77, 78, 80, 82, 85, 86, 87, 91, 92, 93, 94, 95, 98, 99, 102, 105, 106, 108, 110, 111, 112, 114, 115, 116, 117, 118, 119, 120]
```



## Visual Basic .NET

{{trans|D}}

```vbnet
Module Module1
    Const MAX = 120

    Function IsPrime(n As Integer) As Boolean
        If n < 2 Then Return False
        If n Mod 2 = 0 Then Return n = 2
        If n Mod 3 = 0 Then Return n = 3
        Dim d = 5
        While d * d <= n
            If n Mod d = 0 Then Return False
            d += 2
            If n Mod d = 0 Then Return False
            d += 4
        End While
        Return True
    End Function

    Function PrimefactorCount(n As Integer) As Integer
        If n = 1 Then Return 0
        If IsPrime(n) Then Return 1
        Dim count = 0
        Dim f = 2
        While True
            If n Mod f = 0 Then
                count += 1
                n /= f
                If n = 1 Then Return count
                If IsPrime(n) Then f = n
            ElseIf f >= 3 Then
                f += 2
            Else
                f = 3
            End If
        End While
        Throw New Exception("Unexpected")
    End Function

    Sub Main()
        Console.WriteLine("The attractive numbers up to and including {0} are:", MAX)
        Dim i = 1
        Dim count = 0
        While i <= MAX
            Dim n = PrimefactorCount(i)
            If IsPrime(n) Then
                Console.Write("{0,4}", i)
                count += 1
                If count Mod 20 = 0 Then
                    Console.WriteLine()
                End If
            End If
                i += 1
        End While
        Console.WriteLine()
    End Sub

End Module
```

{{out}}

```txt
The attractive numbers up to and including 120 are:
   4   6   8   9  10  12  14  15  18  20  21  22  25  26  27  28  30  32  33  34
  35  38  39  42  44  45  46  48  49  50  51  52  55  57  58  62  63  65  66  68
  69  70  72  74  75  76  77  78  80  82  85  86  87  91  92  93  94  95  98  99
 102 105 106 108 110 111 112 114 115 116 117 118 119 120
```



## zkl

Using GMP (GNU Multiple Precision Arithmetic Library, probabilistic
primes) because it is easy and fast to test for primeness.

```zkl
var [const] BI=Import("zklBigNum");  // libGMP
fcn attractiveNumber(n){ BI(primeFactors(n).len()).probablyPrime() }

println("The attractive numbers up to and including 120 are:");
[1..120].filter(attractiveNumber)
   .apply("%4d".fmt).pump(Void,T(Void.Read,19,False),"println");
```

Using [[Prime decomposition#zkl]]

```zkl
fcn primeFactors(n){  // Return a list of factors of n
   acc:=fcn(n,k,acc,maxD){  // k is 2,3,5,7,9,... not optimum
      if(n==1 or k>maxD) acc.close();
      else{
	 q,r:=n.divr(k);   // divr-->(quotient,remainder)
	 if(r==0) return(self.fcn(q,k,acc.write(k),q.toFloat().sqrt()));
	 return(self.fcn(n,k+1+k.isOdd,acc,maxD))
      }
   }(n,2,Sink(List),n.toFloat().sqrt());
   m:=acc.reduce('*,1);      // mulitply factors
   if(n!=m) acc.append(n/m); // opps, missed last factor
   else acc;
}
```

{{out}}

```txt

The attractive numbers up to and including 120 are:
   4   6   8   9  10  12  14  15  18  20  21  22  25  26  27  28  30  32  33  34
  35  38  39  42  44  45  46  48  49  50  51  52  55  57  58  62  63  65  66  68
  69  70  72  74  75  76  77  78  80  82  85  86  87  91  92  93  94  95  98  99
 102 105 106 108 110 111 112 114 115 116 117 118 119 120

```

