+++
title = "Totient function"
description = ""
date = 2019-10-22T18:38:46Z
aliases = []
[extra]
id = 22091
[taxonomies]
categories = ["task", "Prime Numbers"]
tags = []
languages = [
  "awk",
  "c",
  "csharp",
  "dyalect",
  "factor",
  "freebasic",
  "go",
  "haskell",
  "j",
  "julia",
  "kotlin",
  "lua",
  "nim",
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
  "rust",
  "scala",
  "sidef",
  "vba",
  "zkl",
]
+++

The   '''totient'''   function is also known as:
::*   Euler's totient function
::*   Euler's phi totient function
::*   phi totient function
::*   <big> Φ     </big>   function   (uppercase Greek phi)
::*   <big> &phi; </big>   function   (lowercase Greek phi)



;Definitions   (as per number theory):
The totient function:
::*   counts the integers up to a given positive integer   <big>'''n'''</big>   that are relatively prime to   <big>'''n'''</big>
::*   counts the integers   <big>'''k'''</big>   in the range   <big>'''1 ≤ k ≤ n'''</big>   for which the greatest common divisor   <big>'''gcd(n,k)'''</big>   is equal to   <big>'''1'''</big>
::*   counts numbers   <big>'''≤ n'''</big>   and   prime to   <big>'''n'''</big>



If the totient number   (for '''N''')   is one less than   '''N''',   then   '''N'''   is prime.



## Task

Create a   '''totient'''   function and:
::*   Find and display   (1 per line)   for the 1<sup>st</sup>   '''25'''   integers:
::::*   the integer   (the index)
::::*   the totient number for that integer
::::*   indicate if that integer is prime
::*   Find and display the   ''count''   of the primes up to             100
::*   Find and display the   ''count''   of the primes up to                     1,000
::*   Find and display the   ''count''   of the primes up to                         10,000
::*   Find and display the   ''count''   of the primes up to                             100,000     (optional)

Show all output here.


## Related tasks

::*   [[Perfect totient numbers]]


;Also see:
::*   the Wikipedia entry for   [http://wikipedia.org/wiki/Euler's_totient_function Euler's totient function].
::*   the MathWorld entry for   [http://mathworld.wolfram.com/TotientFunction.html totient function].
::*   the OEIS      entry for   [http://oeis.org/A000010 Euler totient function phi(n)].
<br/>


## AWK


```AWK

# syntax: GAWK -f TOTIENT_FUNCTION.AWK
BEGIN {
    print(" N Phi isPrime")
    for (n=1; n<=1000000; n++) {
      tot = totient(n)
      if (n-1 == tot) {
        count++
      }
      if (n <= 25) {
        printf("%2d %3d %s\n",n,tot,(n-1==tot)?"true":"false")
        if (n == 25) {
          printf("\n  Limit PrimeCount\n")
          printf("%7d %10d\n",n,count)
        }
      }
      else if (n ~ /^100+$/) {
        printf("%7d %10d\n",n,count)
      }
    }
    exit(0)
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

 N Phi isPrime
 1   1 false
 2   1 true
 3   2 true
 4   2 false
 5   4 true
 6   2 false
 7   6 true
 8   4 false
 9   6 false
10   4 false
11  10 true
12   4 false
13  12 true
14   6 false
15   8 false
16   8 false
17  16 true
18   6 false
19  18 true
20   8 false
21  12 false
22  10 false
23  22 true
24   8 false
25  20 false

  Limit PrimeCount
     25          9
    100         25
   1000        168
  10000       1229
 100000       9592
1000000      78498

```


## C

Translation of the second Go example

```C

/*Abhishek Ghosh, 7th December 2018*/

#include<stdio.h>

int totient(int n){
	int tot = n,i;

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

int main()
{
	int count = 0,n,tot;

	printf(" n    %c   prime",237);
        printf("\n---------------\n");

	for(n=1;n<=25;n++){
		tot = totient(n);

		if(n-1 == tot)
			count++;

		printf("%2d   %2d   %s\n", n, tot, n-1 == tot?"True":"False");
	}

	printf("\nNumber of primes up to %6d =%4d\n", 25,count);

	for(n = 26; n <= 100000; n++){
        tot = totient(n);
        if(tot == n-1)
			count++;

        if(n == 100 || n == 1000 || n%10000 == 0){
            printf("\nNumber of primes up to %6d = %4d\n", n, count);
        }
    }

	return 0;
}

```


Output :

```txt

 n    φ   prime
---------------
 1    1   False
 2    1   True
 3    2   True
 4    2   False
 5    4   True
 6    2   False
 7    6   True
 8    4   False
 9    6   False
10    4   False
11   10   True
12    4   False
13   12   True
14    6   False
15    8   False
16    8   False
17   16   True
18    6   False
19   18   True
20    8   False
21   12   False
22   10   False
23   22   True
24    8   False
25   20   False

Number of primes up to     25 =   9

Number of primes up to    100 =   25

Number of primes up to   1000 =  168

Number of primes up to  10000 = 1229

Number of primes up to  20000 = 2262

Number of primes up to  30000 = 3245

Number of primes up to  40000 = 4203

Number of primes up to  50000 = 5133

Number of primes up to  60000 = 6057

Number of primes up to  70000 = 6935

Number of primes up to  80000 = 7837

Number of primes up to  90000 = 8713

Number of primes up to 100000 = 9592

```



## C#


```c#
using static System.Console;
using static System.Linq.Enumerable;

public class Program
{
    static void Main()
    {
        for (int i = 1; i <= 25; i++) {
            int t = Totient(i);
            WriteLine(i + "\t" + t + (t == i - 1 ? "\tprime" : ""));
        }
        WriteLine();
        for (int i = 100; i <= 100_000; i *= 10) {
            WriteLine($"{Range(1, i).Count(x => Totient(x) + 1 == x):n0} primes below {i:n0}");
        }
    }

    static int Totient(int n) {
        if (n < 3) return 1;
        if (n == 3) return 2;

        int totient = n;

        if ((n & 1) == 0) {
            totient >>= 1;
            while (((n >>= 1) & 1) == 0) ;
        }

        for (int i = 3; i * i <= n; i += 2) {
            if (n % i == 0) {
                totient -= totient / i;
                while ((n /= i) % i == 0) ;
            }
        }
        if (n > 1) totient -= totient / n;
        return totient;
    }
}
```

<pre style="height:30ex;overflow:scroll">
1	1
2	1	prime
3	2	prime
4	2
5	4	prime
6	2
7	6	prime
8	4
9	6
10	4
11	10	prime
12	4
13	12	prime
14	6
15	8
16	8
17	16	prime
18	6
19	18	prime
20	8
21	12
22	10
23	22	prime
24	8
25	20

25 primes below 100
168 primes below 1,000
1,229 primes below 10,000
9,592 primes below 100,000

```



## Dyalect


```dyalect
func totient(n) {
    var tot = n
    var i = 2
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

print("n\tphi\tprime")
var count = 0
for n in 1..25 {
    var tot = totient(n)
    var isPrime = n - 1 == tot
    if isPrime {
        count += 1
    }
    print("\(n)\t\(tot)\t\(isPrime)")
}
print("\nNumber of primes up to 25 \t= \(count)")
for n in 26..100000 {
    var tot = totient(n)
    if tot == n - 1 {
        count += 1
    }
    if n == 100 || n == 1000 || n % 10000 == 0 {
        print("Number of primes up to \(n) \t= \(count)")
    }
}
```


```txt
n       phi     prime
1       1       false
2       1       true
3       2       true
4       2       false
5       4       true
6       2       false
7       6       true
8       4       false
9       6       false
10      4       false
11      10      true
12      4       false
13      12      true
14      6       false
15      8       false
16      8       false
17      16      true
18      6       false
19      18      true
20      8       false
21      12      false
22      10      false
23      22      true
24      8       false
25      20      false

Number of primes up to 25       = 9
Number of primes up to 100      = 25
Number of primes up to 1000     = 168
Number of primes up to 10000    = 1229
Number of primes up to 20000    = 2262
Number of primes up to 30000    = 3245
Number of primes up to 40000    = 4203
Number of primes up to 50000    = 5133
Number of primes up to 60000    = 6057
Number of primes up to 70000    = 6935
Number of primes up to 80000    = 7837
Number of primes up to 90000    = 8713
Number of primes up to 100000   = 9592
```



## Factor


```factor
USING: combinators formatting io kernel math math.primes.factors
math.ranges sequences ;
IN: rosetta-code.totient-function

: Φ ( n -- m )
    {
        { [ dup 1 < ] [ drop 0 ] }
        { [ dup 1 = ] [ drop 1 ] }
        [
            dup unique-factors
            [ 1 [ 1 - * ] reduce ] [ product ] bi / *
        ]
    } cond ;

: show-info ( n -- )
    [ Φ ] [ swap 2dup - 1 = ] bi ", prime" "" ?
    "Φ(%2d) = %2d%s\n" printf ;

: totient-demo ( -- )
    25 [1,b] [ show-info ] each nl 0 100,000 [1,b] [
        [ dup Φ - 1 = [ 1 + ] when ]
        [ dup { 100 1,000 10,000 100,000 } member? ] bi
        [ dupd "%4d primes <= %d\n" printf ] [ drop ] if
    ] each drop ;

MAIN: totient-demo
```

```txt

Φ( 1) =  1
Φ( 2) =  1, prime
Φ( 3) =  2, prime
Φ( 4) =  2
Φ( 5) =  4, prime
Φ( 6) =  2
Φ( 7) =  6, prime
Φ( 8) =  4
Φ( 9) =  6
Φ(10) =  4
Φ(11) = 10, prime
Φ(12) =  4
Φ(13) = 12, prime
Φ(14) =  6
Φ(15) =  8
Φ(16) =  8
Φ(17) = 16, prime
Φ(18) =  6
Φ(19) = 18, prime
Φ(20) =  8
Φ(21) = 12
Φ(22) = 10
Φ(23) = 22, prime
Φ(24) =  8
Φ(25) = 20

  25 primes <= 100
 168 primes <= 1000
1229 primes <= 10000
9592 primes <= 100000

```



## FreeBASIC

```freebasic

#define esPar(n) (((n) And 1) = 0)
#define esImpar(n)  (esPar(n) = 0)

Function Totient(n As Integer) As Integer
    'delta son números no divisibles por 2,3,5
    Dim delta(7) As Integer = {6,4,2,4,2,4,6,2}
    Dim As Integer i, quot, idx, result
    ' div mod por constante es rápido.
    'i = 2
    result = n
    If (2*2 <= n) Then
        If Not(esImpar(n)) Then
            ' eliminar números con factor 2,4,8,16,...
            While Not(esImpar(n))
                n = n \ 2
            Wend
            'eliminar los múltiplos de 2
            result -= result \ 2
        End If
    End If
    'i = 3
    If (3*3 <= n) And (n Mod 3 = 0) Then
        Do
            quot = n \ 3
            If n <> quot*3 Then
                Exit Do
            Else
                n = quot
            End If
        Loop Until false
        result -= result \ 3
    End If
    'i = 5
    If (5*5 <= n) And (n Mod 5 = 0) Then
        Do
            quot = n \ 5
            If n <> quot*5 Then
                Exit Do
            Else
                n = quot
            End If
        Loop Until false
        result -= result \ 5
    End If
    i = 7
    idx = 1
    'i = 7,11,13,17,19,23,29,...,49 ..
    While i*i <= n
        quot = n \ i
        If n = quot*i Then
            Do
                If n <> quot*i Then
                    Exit Do
                Else
                    n = quot
                End If
                quot = n \ i
            Loop Until false
            result -= result \ i
        End If
        i = i + delta(idx)
        idx = (idx+1) And 7
    Wend
    If n > 1 Then result -= result \ n
    Totient = result
End Function

Sub ContandoPrimos(n As Integer)
    Dim As Integer i, cnt = 0
    For i = 1 To n
        If Totient(i) = (i-1) Then cnt += 1
    Next i
    Print Using " #######      ######"; i-1; cnt
End Sub

Function esPrimo(n As Ulongint) As String
    esPrimo = "False"
    If n = 1 then Return "False"
    If (n=2) Or (n=3) Then Return "True"
    If n Mod 2=0 Then Exit Function
    If n Mod 3=0 Then Exit Function
    Dim As Ulongint limite = Sqr(N)+1
    For i As Ulongint = 6 To limite Step 6
        If N Mod (i-1)=0 Then Exit Function
        If N Mod (i+1)=0 Then Exit Function
    Next i
    Return "True"
End Function

Sub display(n As Integer)
    Dim As Integer idx, phi
    If n = 0 Then Exit Sub
    Print "  n  phi(n)   esPrimo"
    For idx = 1 To n
        phi = Totient(idx)
        Print Using "###   ###      \   \"; idx; phi; esPrimo(idx)
    Next idx
End Sub

Dim l As Integer
display(25)

Print Chr(10) & "   Limite  Son primos"
ContandoPrimos(25)
l = 100
Do
    ContandoPrimos(l)
    l = l*10
Loop Until l > 1000000
End

```

```txt

  n  phi(n)   esPrimo
  1     1      False
  2     1      True
  3     2      True
  4     2      False
  5     4      True
  6     2      False
  7     6      True
  8     4      False
  9     6      False
 10     4      False
 11    10      True
 12     4      False
 13    12      True
 14     6      False
 15     8      False
 16     8      False
 17    16      True
 18     6      False
 19    18      True
 20     8      False
 21    12      False
 22    10      False
 23    22      True
 24     8      False
 25    20      False

   Limite  Son primos
      25           9
     100          25
    1000         168
   10000        1229
  100000        9592
 1000000       78498

```



## Go

Results for the larger values of n are very slow to emerge.

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
    fmt.Println(" n  phi   prime")
    fmt.Println("---------------")
    count := 0
    for n := 1; n <= 25; n++ {
        tot := totient(n)
        isPrime := n-1 == tot
        if isPrime {
            count++
        }
        fmt.Printf("%2d   %2d   %t\n", n, tot, isPrime)
    }
    fmt.Println("\nNumber of primes up to 25     =", count)
    for n := 26; n <= 100000; n++ {
        tot := totient(n)
        if tot == n-1 {
            count++
        }
        if n == 100 || n == 1000 || n%10000 == 0 {
            fmt.Printf("\nNumber of primes up to %-6d = %d\n", n, count)
        }
    }
}
```


```txt

 n  phi   prime
---------------
 1    1   false
 2    1   true
 3    2   true
 4    2   false
 5    4   true
 6    2   false
 7    6   true
 8    4   false
 9    6   false
10    4   false
11   10   true
12    4   false
13   12   true
14    6   false
15    8   false
16    8   false
17   16   true
18    6   false
19   18   true
20    8   false
21   12   false
22   10   false
23   22   true
24    8   false
25   20   false

Number of primes up to 25     = 9

Number of primes up to 100    = 25

Number of primes up to 1000   = 168

Number of primes up to 10000  = 1229

Number of primes up to 20000  = 2262

Number of primes up to 30000  = 3245

Number of primes up to 40000  = 4203

Number of primes up to 50000  = 5133

Number of primes up to 60000  = 6057

Number of primes up to 70000  = 6935

Number of primes up to 80000  = 7837

Number of primes up to 90000  = 8713

Number of primes up to 100000 = 9592

```


The following much quicker version (runs in less than 150 ms on my machine) uses Euler's product formula rather than repeated invocation of the gcd function to calculate the totient:


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
    fmt.Println(" n  phi   prime")
    fmt.Println("---------------")
    count := 0
    for n := 1; n <= 25; n++ {
        tot := totient(n)
        isPrime := n-1 == tot
        if isPrime {
            count++
        }
        fmt.Printf("%2d   %2d   %t\n", n, tot, isPrime)
    }
    fmt.Println("\nNumber of primes up to 25     =", count)
    for n := 26; n <= 100000; n++ {
        tot := totient(n)
        if tot == n-1 {
            count++
        }
        if n == 100 || n == 1000 || n%10000 == 0 {
            fmt.Printf("\nNumber of primes up to %-6d = %d\n", n, count)
        }
    }
}
```


The output is the same as before.


## Haskell

```Haskell

{-# LANGUAGE BangPatterns #-}

import Control.Monad

totient :: (Integral a) => a -> a
totient n
    | n == 0      = 1             -- by definition phi(0) = 1
    | n < 0       = totient (-n)  -- phi(-n) is taken to be equal to phi(n)
    | otherwise   = loop n n 2    --
    where
        loop !m !tot !i
            | i * i > m         = if m > 1 then tot - (tot `div` m) else tot
            | m `mod` i == 0    = loop m' tot' i'
            | otherwise         = loop m tot i'
            where i'   = if i == 2 then 3 else (i + 2)
                  m'   = nextM m
                  tot' = tot - (tot `div` i)
                  nextM !x | x `mod` i == 0 = nextM $ x `div` i
                           | otherwise      = x

main :: IO ()
main = do
    putStrLn "n\tphi\tprime\n---------------------"
    let loop !i !count
            | i >= 10^6 = return ()
            | otherwise = do
                let i'        = i + 1
                    tot       = totient i'
                    isPrime   = tot == i' - 1
                    count'    = if isPrime then count + 1 else count
                when (i' <= 25) $ do
                    putStrLn $ (show i') ++ "\t" ++ (show tot) ++ "\t" ++ (show isPrime)
                when (i' `elem` 25 : [ 10^k | k <- [2..6] ]) $ do
                    putStrLn $ "Number of primes up to " ++ (show i') ++ " = " ++ (show count')
                loop (i + 1) count'
    loop 0 0

```

```txt

n       phi     prime
---------------------
1       1       False
2       1       True
3       2       True
4       2       False
5       4       True
6       2       False
7       6       True
8       4       False
9       6       False
10      4       False
11      10      True
12      4       False
13      12      True
14      6       False
15      8       False
16      8       False
17      16      True
18      6       False
19      18      True
20      8       False
21      12      False
22      10      False
23      22      True
24      8       False
25      20      False
Number of primes up to 25 = 9
Number of primes up to 100 = 25
Number of primes up to 1000 = 168
Number of primes up to 10000 = 1229
Number of primes up to 100000 = 9592
Number of primes up to 1000000 = 78498

```




## J


```J

  nth_prime =: p:   NB. 2 is the zeroth prime
   totient =: 5&p:
   primeQ =:  1&p:

   NB. first row contains the integer
   NB. second row             totient
   NB. third                  1 iff prime
   (, totient ,: primeQ) >: i. 25
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
1 1 2 2 4 2 6 4 6  4 10  4 12  6  8  8 16  6 18  8 12 10 22  8 20
0 1 1 0 1 0 1 0 0  0  1  0  1  0  0  0  1  0  1  0  0  0  1  0  0


   NB. primes first exceeding the limits
   [&.:(p:inv) 10 ^ 2 + i. 4
101 1009 10007 100003

   p:inv 101 1009 10007 100003
25 168 1229 9592

   NB. limit and prime count
   (,. p:inv) 10 ^ 2 + i. 5
   100    25
  1000   168
 10000  1229
100000  9592
   1e6 78498

```



## Julia

```julia
φ(n) = sum(1 for k in 1:n if gcd(n, k) == 1)

is_prime(n) = φ(n) == n - 1

function runphitests()
    for n in 1:25
        println(" φ($n) == $(φ(n))", is_prime(n) ? ", is prime" : "")
    end
    count = 0
    for n in 1:100_000
        count += is_prime(n)
        if n in [100, 1000, 10_000, 100_000]
            println("Primes up to $n: $count")
        end
    end
end

runphitests()

```
```txt

 φ(1) == 1
 φ(2) == 1, is prime
 φ(3) == 2, is prime
 φ(4) == 2
 φ(5) == 4, is prime
 φ(6) == 2
 φ(7) == 6, is prime
 φ(8) == 4
 φ(9) == 6
 φ(10) == 4
 φ(11) == 10, is prime
 φ(12) == 4
 φ(13) == 12, is prime
 φ(14) == 6
 φ(15) == 8
 φ(16) == 8
 φ(17) == 16, is prime
 φ(18) == 6
 φ(19) == 18, is prime
 φ(20) == 8
 φ(21) == 12
 φ(22) == 10
 φ(23) == 22, is prime
 φ(24) == 8
 φ(25) == 20
 Primes up to 100: 25
 Primes up to 1000: 168
 Primes up to 10000: 1229
 Primes up to 100000: 9592

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
    println(" n  phi   prime")
    println("---------------")
    var count = 0
    for (n in 1..25) {
        val tot = totient(n)
        val isPrime  = n - 1 == tot
        if (isPrime) count++
        System.out.printf("%2d   %2d   %b\n", n, tot, isPrime)
    }
    println("\nNumber of primes up to 25     = $count")
    for (n in 26..100_000) {
        val tot = totient(n)
        if (tot == n-1) count++
        if (n == 100 || n == 1000 || n % 10_000 == 0) {
            System.out.printf("\nNumber of primes up to %-6d = %d\n", n, count)
        }
    }
}
```


```txt

Same as Go example.

```



## Lua

Averages about 7 seconds under LuaJIT

```lua
-- Return the greatest common denominator of x and y
function gcd (x, y)
  return y == 0 and math.abs(x) or gcd(y, x % y)
end

-- Return the totient number for n
function totient (n)
  local count = 0
  for k = 1, n do
    if gcd(n, k) == 1 then count = count + 1 end
  end
  return count
end

-- Determine (inefficiently) whether p is prime
function isPrime (p)
  return totient(p) == p - 1
end

-- Output totient and primality for the first 25 integers
print("n", string.char(237), "prime")
print(string.rep("-", 21))
for i = 1, 25 do
  print(i, totient(i), isPrime(i))
end

-- Count the primes up to 100, 1000 and 10000
local pCount, i, limit = 0, 1
for power = 2, 4 do
  limit = 10 ^ power
  repeat
    i = i + 1
    if isPrime(i) then pCount = pCount + 1 end
  until i == limit
  print("\nThere are " .. pCount .. " primes below " .. limit)
end
```

```txt
n       φ       prime
---------------------
1       1       false
2       1       true
3       2       true
4       2       false
5       4       true
6       2       false
7       6       true
8       4       false
9       6       false
10      4       false
11      10      true
12      4       false
13      12      true
14      6       false
15      8       false
16      8       false
17      16      true
18      6       false
19      18      true
20      8       false
21      12      false
22      10      false
23      22      true
24      8       false
25      20      false

There are 25 primes below 100

There are 168 primes below 1000

There are 1229 primes below 10000
```



## Nim


```Nim
import strformat

func totient(n: int): int =
    var tot = n
    var nn = n
    var i = 2
    while i * i <= nn:
        if nn mod i == 0:
            while nn mod i == 0:
                nn = nn div i
            dec tot, tot div i
        if i == 2:
            i = 1
        inc i, 2
    if nn > 1:
        dec tot, tot div nn
    tot

echo " n    φ   prime"
echo "---------------"
var count = 0
for n in 1..25:
    let tot = totient(n)
    let isPrime = n - 1 == tot
    if isPrime:
        inc count
    echo fmt"{n:2}   {tot:2}   {isPrime}"
echo ""
echo fmt"Number of primes up to {25:>6} = {count:>4}"
for n in 26..100_000:
    let tot = totient(n)
    if tot == n - 1:
        inc count
    if n == 100 or n == 1000 or n mod 10_000 == 0:
        echo fmt"Number of primes up to {n:>6} = {count:>4}"
```

```txt

 n    φ   prime
---------------
 1    1   false
 2    1   true
 3    2   true
 4    2   false
 5    4   true
 6    2   false
 7    6   true
 8    4   false
 9    6   false
10    4   false
11   10   true
12    4   false
13   12   true
14    6   false
15    8   false
16    8   false
17   16   true
18    6   false
19   18   true
20    8   false
21   12   false
22   10   false
23   22   true
24    8   false
25   20   false

Number of primes up to     25 =    9
Number of primes up to    100 =   25
Number of primes up to   1000 =  168
Number of primes up to  10000 = 1229
Number of primes up to  20000 = 2262
Number of primes up to  30000 = 3245
Number of primes up to  40000 = 4203
Number of primes up to  50000 = 5133
Number of primes up to  60000 = 6057
Number of primes up to  70000 = 6935
Number of primes up to  80000 = 7837
Number of primes up to  90000 = 8713
Number of primes up to 100000 = 9592

```



## Pascal

Yes, a very slow possibility to check prime

```pascal
{$IFDEF FPC}
  {$MODE DELPHI}
{$IFEND}
function gcd_mod(u, v: NativeUint): NativeUint;inline;
//prerequisites  u > v and u,v > 0
  var
    t: NativeUInt;
  begin
    repeat
      t := u;
      u := v;
      v := t mod v;
    until v = 0;
    gcd_mod := u;
  end;

function Totient(n:NativeUint):NativeUint;
var
  i : NativeUint;
Begin
  result := 1;
  For i := 2 to n do
    inc(result,ORD(GCD_mod(n,i)=1));
end;

function CheckPrimeTotient(n:NativeUint):Boolean;inline;
begin
  result :=  (Totient(n) = (n-1));
end;

procedure OutCountPrimes(n:NativeUInt);
var
  i,cnt :  NativeUint;
begin
  cnt := 0;
  For i := 1 to n do
    inc(cnt,Ord(CheckPrimeTotient(i)));
  writeln(n:10,cnt:8);
end;

procedure display(n:NativeUint);
var
  idx,phi : NativeUint;
Begin
  if n = 0 then
    EXIT;
  writeln('number n':5,'Totient(n)':11,'isprime':8);
  For idx := 1 to n do
  Begin
    phi := Totient(idx);
    writeln(idx:4,phi:10,(phi=(idx-1)):12);
  end
end;
var
  i : NativeUint;
Begin
  display(25);

  writeln('Limit  primecount');
  i := 100;
  repeat
    OutCountPrimes(i);
    i := i*10;
  until i >100000;
end.
```

;Output:

```txt
number n Totient(n) isprime
   1         1       FALSE
   2         1        TRUE
   3         2        TRUE
   4         2       FALSE
   5         4        TRUE
   6         2       FALSE
   7         6        TRUE
   8         4       FALSE
   9         6       FALSE
  10         4       FALSE
  11        10        TRUE
  12         4       FALSE
  13        12        TRUE
  14         6       FALSE
  15         8       FALSE
  16         8       FALSE
  17        16        TRUE
  18         6       FALSE
  19        18        TRUE
  20         8       FALSE
  21        12       FALSE
  22        10       FALSE
  23        22        TRUE
  24         8       FALSE
  25        20       FALSE
Limit  primecount
       100      25
      1000     168
     10000    1229
    100000    9592

real  3m39,745s
```


### alternative

changing Totient-funtion in program atop to Computing Euler's totient function on wikipedia, like GO and C.

Impressive speedup.Checking with only primes would be even faster.

```pascal
function totient(n:NativeUInt):NativeUInt;
const
  //delta of numbers not divisible by 2,3,5 (0_1+6->7+4->11 ..+6->29+2->3_1
  delta : array[0..7] of NativeUint = (6,4,2,4,2,4,6,2);
var
  i, quot,idx: NativeUint;
Begin
  // div mod by constant is fast.
  //i = 2
  result := n;
  if (2*2 <= n) then
  Begin
    IF not(ODD(n)) then
    Begin
      // remove numbers with factor 2,4,8,16, ...
      while not(ODD(n)) do
        n := n DIV 2;
      //remove count of multiples of 2
      dec(result,result DIV 2);
    end;
  end;
  //i = 3
  If (3*3 <= n) AND (n mod 3 = 0) then
  Begin
    repeat
      quot := n DIV 3;
      IF n <> quot*3 then
        BREAK
      else
        n := quot;
    until false;
    dec(result,result DIV 3);
  end;
  //i = 5
  If (5*5 <= n) AND (n mod 5 = 0) then
  Begin
    repeat
      quot := n DIV 5;
      IF n <> quot*5 then
        BREAK
      else
        n := quot;
    until false;
    dec(result,result DIV 5);
  end;
  i := 7;
  idx := 1;
  //i = 7,11,13,17,19,23,29, ...49 ..
  while i*i <= n do
  Begin
    quot := n DIV i;
    if n = quot*i then
    Begin
      repeat
        IF n <> quot*i then
          BREAK
        else
          n := quot;
        quot := n DIV i;
      until false;
      dec(result,result DIV i);
    end;
    i := i + delta[idx];
    idx := (idx+1) AND 7;
  end;
  if n> 1 then
    dec(result,result div n);
end;
```

;Output:

```txt

number n Totient(n) isprime
   1         1       FALSE
   2         1        TRUE
   3         2        TRUE
   4         2       FALSE
   5         4        TRUE
   6         2       FALSE
   7         6        TRUE
   8         4       FALSE
   9         6       FALSE
  10         4       FALSE
  11        10        TRUE
  12         4       FALSE
  13        12        TRUE
  14         6       FALSE
  15         8       FALSE
  16         8       FALSE
  17        16        TRUE
  18         6       FALSE
  19        18        TRUE
  20         8       FALSE
  21        12       FALSE
  22        10       FALSE
  23        22        TRUE
  24         8       FALSE
  25        20       FALSE
Limit  primecount
       100      25
      1000     168
     10000    1229
    100000    9592
   1000000   78498
  10000000  664579

real	0m7,369s
```



## Perl

====Direct calculation of 𝜑====
```perl
use utf8;
binmode STDOUT, ":utf8";

sub gcd {
  my ($u, $v) = @_;
  while ($v) {
    ($u, $v) = ($v, $u % $v);
  }
  return abs($u);
}

push @𝜑, 0;
for $t (1..10000) {
    push @𝜑, scalar grep { 1 == gcd($_,$t) } 1..$t;
}

printf "𝜑(%2d) = %3d%s\n", $_, $𝜑[$_], $_ - $𝜑[$_] - 1 ? '' : ' Prime' for 1 .. 25;
print "\n";

for $limit (100, 1000, 10000) {
    printf "Count of primes <= $limit: %d\n", scalar grep {$_ == $𝜑[$_] + 1} 0..$limit;
}

```

```txt
𝜑( 1) =   1
𝜑( 2) =   1 Prime
𝜑( 3) =   2 Prime
𝜑( 4) =   2
𝜑( 5) =   4 Prime
𝜑( 6) =   2
𝜑( 7) =   6 Prime
𝜑( 8) =   4
𝜑( 9) =   6
𝜑(10) =   4
𝜑(11) =  10 Prime
𝜑(12) =   4
𝜑(13) =  12 Prime
𝜑(14) =   6
𝜑(15) =   8
𝜑(16) =   8
𝜑(17) =  16 Prime
𝜑(18) =   6
𝜑(19) =  18 Prime
𝜑(20) =   8
𝜑(21) =  12
𝜑(22) =  10
𝜑(23) =  22 Prime
𝜑(24) =   8
𝜑(25) =  20

Count of primes <= 100: 25
Count of primes <= 1000: 168
Count of primes <= 10000: 1229
```

====Using 'ntheory' library====
Much faster. Output is the same as above.
```perl
use utf8;
binmode STDOUT, ":utf8";

use ntheory qw(euler_phi);

my @𝜑 = euler_phi(0,10000);  # Returns list of all values in range

printf "𝜑(%2d) = %3d%s\n", $_, $𝜑[$_], $_ - $𝜑[$_] - 1 ? '' : ' Prime' for 1 .. 25;
print "\n";

for $limit (100, 1000, 10000) {
    printf "Count of primes <= $limit: %d\n", scalar grep {$_ == $𝜑[$_] + 1} 0..$limit;
}
```



## Perl 6

This is an ''incredibly'' inefficient way of finding prime numbers.

<!-- The counting of primes  (or finding of primes)  was included in this task as a verification of the  totient  function's ability to detect a prime,  not to provide a method to find a prime --- it's an artifact of the function.   But, slow as it is, it's not as slow as the AKS test for primes.    Perhaps this could be moved to the discussion page if the efficiency is talk-worthy topic.    Gerard Schildberger.   !-->

<!-- Also, the task requirements haven't shifted, they were clarified as at least one person failed to understand the 3rd requirement (please view the original wording).   If you think this one change constitutes an every-shifting change in the requirements, please feel free to revert the change.  I implore you to try to not add snipes (to the history log that can't be deleted).  I value your comments, but not so much when they're detrimental to the editing of a Rosetta Code task preamble, and comments that aren't constructive.   This is, after all, a draft task.   Better to change the wording now then later.  Adding clarification isn't shifting the requirements,  I tried to make the sentence structure clearer to understand.   I felt that an edification for a task's requirement was needed as it apparently didn't effectively convey what was needed to be marked (as a prime).   !-->


```perl6
my \𝜑 = 0, |(1..*).hyper(:8degree).map: -> $t { +(^$t).grep: * gcd $t == 1 };

printf "𝜑(%2d) = %3d %s\n", $_, 𝜑[$_], $_ - 𝜑[$_] - 1 ?? '' !! 'Prime' for 1 .. 25;

(100, 1000, 10000).map: -> $limit {
    say "\nCount of primes <= $limit: " ~ +(^$limit).grep: {$_ == 𝜑[$_] + 1}
}
```

```txt
𝜑( 1) =   1
𝜑( 2) =   1 Prime
𝜑( 3) =   2 Prime
𝜑( 4) =   2
𝜑( 5) =   4 Prime
𝜑( 6) =   2
𝜑( 7) =   6 Prime
𝜑( 8) =   4
𝜑( 9) =   6
𝜑(10) =   4
𝜑(11) =  10 Prime
𝜑(12) =   4
𝜑(13) =  12 Prime
𝜑(14) =   6
𝜑(15) =   8
𝜑(16) =   8
𝜑(17) =  16 Prime
𝜑(18) =   6
𝜑(19) =  18 Prime
𝜑(20) =   8
𝜑(21) =  12
𝜑(22) =  10
𝜑(23) =  22 Prime
𝜑(24) =   8
𝜑(25) =  20

Count of primes <= 100: 25

Count of primes <= 1000: 168

Count of primes <= 10000: 1229
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

printf(1," n  phi   prime\n")
printf(1," --------------\n")
integer count = 0
for n=1 to 25 do
    integer tot = totient(n),
            prime = (n-1=tot)
    count += prime
    string isp = iff(prime?"true":"false")
    printf(1,"%2d   %2d   %s\n",{n,tot,isp})
end for
printf(1,"\nNumber of primes up to 25     = %d\n",count)
for n=26 to 100000 do
    count += (totient(n)=n-1)
    if find(n,{100,1000,10000,100000}) then
        printf(1,"Number of primes up to %-6d = %d\n",{n,count})
    end if
end for
```

```txt

 n  phi   prime
 --------------
 1    1   false
 2    1   true
 3    2   true
 4    2   false
 5    4   true
 6    2   false
 7    6   true
 8    4   false
 9    6   false
10    4   false
11   10   true
12    4   false
13   12   true
14    6   false
15    8   false
16    8   false
17   16   true
18    6   false
19   18   true
20    8   false
21   12   false
22   10   false
23   22   true
24    8   false
25   20   false

Number of primes up to 25     = 9
Number of primes up to 100    = 25
Number of primes up to 1000   = 168
Number of primes up to 10000  = 1229
Number of primes up to 100000 = 9592

```



## PicoLisp


```PicoLisp
(gc 32)
(de gcd (A B)
   (until (=0 B)
      (let M (% A B)
         (setq A B B M) ) )
   (abs A) )
(de totient (N)
   (let C 0
      (for I N
         (and (=1 (gcd N I)) (inc 'C)) )
      (cons C (= C (dec N))) ) )
(de p? (N)
   (let C 0
      (for A N
         (and
            (cdr (totient A))
            (inc 'C) ) )
      C ) )
(let Fmt (3 7 10)
   (tab Fmt "N" "Phi" "Prime?")
   (tab Fmt "-" "---" "------")
   (for N 25
      (tab Fmt
         N
         (car (setq @ (totient N)))
         (cdr @) ) ) )
(println
   (mapcar p? (25 100 1000 10000 100000)) )
```

```txt

  N    Phi    Prime?
  -    ---    ------
  1      1
  2      1         T
  3      2         T
  4      2
  5      4         T
  6      2
  7      6         T
  8      4
  9      6
 10      4
 11     10         T
 12      4
 13     12         T
 14      6
 15      8
 16      8
 17     16         T
 18      6
 19     18         T
 20      8
 21     12
 22     10
 23     22         T
 24      8
 25     20
(9 25 168 1229 9592)
```



## Python


```python
from math import gcd

def  φ(n):
    return sum(1 for k in range(1, n + 1) if gcd(n, k) == 1)

if __name__ == '__main__':
    def is_prime(n):
        return φ(n) == n - 1

    for n in range(1, 26):
        print(f" φ({n}) == {φ(n)}{', is prime' if is_prime(n)  else ''}")
    count = 0
    for n in range(1, 10_000 + 1):
        count += is_prime(n)
        if n in {100, 1000, 10_000}:
            print(f"Primes up to {n}: {count}")
```


```txt
 φ(1) == 1
 φ(2) == 1, is prime
 φ(3) == 2, is prime
 φ(4) == 2
 φ(5) == 4, is prime
 φ(6) == 2
 φ(7) == 6, is prime
 φ(8) == 4
 φ(9) == 6
 φ(10) == 4
 φ(11) == 10, is prime
 φ(12) == 4
 φ(13) == 12, is prime
 φ(14) == 6
 φ(15) == 8
 φ(16) == 8
 φ(17) == 16, is prime
 φ(18) == 6
 φ(19) == 18, is prime
 φ(20) == 8
 φ(21) == 12
 φ(22) == 10
 φ(23) == 22, is prime
 φ(24) == 8
 φ(25) == 20
Primes up to 100: 25
Primes up to 1000: 168
Primes up to 10000: 1229
```



## Racket



```racket
#lang racket

(require math/number-theory)

(define (prime*? n) (= (totient n) (sub1 n)))

(for ([n (in-range 1 26)])
  (printf "φ(~a) = ~a~a~a\n"
          n
          (totient n)
          (if (prime*? n) " is prime" "")
          (if (prime? n) " (confirmed)" "")))

(for/fold ([count 0] #:result (void)) ([n (in-range 1 10001)])
   (define new-count (if (prime*? n) (add1 count) count))
   (when (member n '(100 1000 10000))
     (printf "Primes up to ~a: ~a\n" n new-count))
   new-count)
```


```txt

φ(1) = 1
φ(2) = 1 is prime (confirmed)
φ(3) = 2 is prime (confirmed)
φ(4) = 2
φ(5) = 4 is prime (confirmed)
φ(6) = 2
φ(7) = 6 is prime (confirmed)
φ(8) = 4
φ(9) = 6
φ(10) = 4
φ(11) = 10 is prime (confirmed)
φ(12) = 4
φ(13) = 12 is prime (confirmed)
φ(14) = 6
φ(15) = 8
φ(16) = 8
φ(17) = 16 is prime (confirmed)
φ(18) = 6
φ(19) = 18 is prime (confirmed)
φ(20) = 8
φ(21) = 12
φ(22) = 10
φ(23) = 22 is prime (confirmed)
φ(24) = 8
φ(25) = 20
Primes up to 100: 25
Primes up to 1000: 168
Primes up to 10000: 1229

```



## REXX


```rexx
/*REXX program calculates the totient numbers for a range of numbers, and count primes. */
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N= 25                    /*Not specified?  Then use the default.*/
tell= N>0                                        /*N positive>?  Then display them all. */
N= abs(N)                                        /*use the absolute value of N for loop.*/
w= length(N)                                     /*W:  is used in aligning the output.  */
primes= 0                                        /*the number of primes found  (so far).*/
                                                 /*if N was negative, only count primes.*/
    do j=1  for  N;     T= phi(j)                /*obtain totient number for a number.  */
    prime= word('(prime)', 1 +  (T \== j-1 ) )   /*determine if  J  is a prime number.  */
    if prime\==''  then primes= primes+1         /*if a prime, then bump the prime count*/
    if tell  then say 'totient number for '  right(j, w)  " ──► "  right(T, w)  ' '  prime
    end   /*j*/
say
say right(primes, w)    ' primes detected for numbers up to and including '    N
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
gcd: parse arg x,y;   do until y==0;     parse value   x//y  y    with    y  x
                      end   /*until*/;                                          return x
/*──────────────────────────────────────────────────────────────────────────────────────*/
phi: procedure; parse arg z;   #= z==1
                      do m=1  for z-1;  if gcd(m, z)==1  then #= # + 1
                      end   /*m*/;                                              return #
```

```txt

totient number for   1  ──►   1
totient number for   2  ──►   1   (prime)
totient number for   3  ──►   2   (prime)
totient number for   4  ──►   2
totient number for   5  ──►   4   (prime)
totient number for   6  ──►   2
totient number for   7  ──►   6   (prime)
totient number for   8  ──►   4
totient number for   9  ──►   6
totient number for  10  ──►   4
totient number for  11  ──►  10   (prime)
totient number for  12  ──►   4
totient number for  13  ──►  12   (prime)
totient number for  14  ──►   6
totient number for  15  ──►   8
totient number for  16  ──►   8
totient number for  17  ──►  16   (prime)
totient number for  18  ──►   6
totient number for  19  ──►  18   (prime)
totient number for  20  ──►   8
totient number for  21  ──►  12
totient number for  22  ──►  10
totient number for  23  ──►  22   (prime)
totient number for  24  ──►   8
totient number for  25  ──►  20

 9  primes detected for numbers up to and including  25

```

```txt

 25  primes detected for numbers up to and including  100

```

```txt

 168  primes detected for numbers up to and including  1000

```

```txt

 1229  primes detected for numbers up to and including  10000

```

```txt

 9592 primes detected for numbers up to and including  100000

```



## Ruby


```ruby

require "prime"

def 𝜑(n)
  n.prime_division.inject(1) {|res, (pr, exp)| res *= (pr-1) * pr**(exp-1) }
end

1.upto 25 do |n|
  tot = 𝜑(n)
  puts "#{n}\t #{tot}\t #{"prime" if n-tot==1}"
end

[100, 1_000, 10_000, 100_000].each do |u|
  puts "Number of primes up to #{u}: #{(1..u).count{|n| n-𝜑(n) == 1} }"
end

```

```txt

1	 1
2	 1	 prime
3	 2	 prime
4	 2
5	 4	 prime
6	 2
7	 6	 prime
8	 4
9	 6
10	 4
11	 10	 prime
12	 4
13	 12	 prime
14	 6
15	 8
16	 8
17	 16	 prime
18	 6
19	 18	 prime
20	 8
21	 12
22	 10
23	 22	 prime
24	 8
25	 20
Number of primes up to 100: 25
Number of primes up to 1000: 168
Number of primes up to 10000: 1229
Number of primes up to 100000: 9592

```



## Rust



```rust
use num::integer::gcd;

fn main() {
    // Compute the totient of the first 25 natural integers
    println!("N\t phi(n)\t Prime");
    for n in 1..26 {
        let phi_n = phi(n);
        println!("{}\t {}\t {:?}", n, phi_n, phi_n == n - 1);
    }

    // Compute the number of prime numbers for various steps
    [1, 100, 1000, 10000, 100000]
        .windows(2)
        .scan(0, |acc, tuple| {
            *acc += (tuple[0]..=tuple[1]).filter(is_prime).count();
            Some((tuple[1], *acc))
        })
        .for_each(|x| println!("Until {}: {} prime numbers", x.0, x.1));
}

fn is_prime(n: &usize) -> bool {
    phi(*n) == *n - 1
}

fn phi(n: usize) -> usize {
    (1..=n).filter(|&x| gcd(n, x) == 1).count()
}
```


Output is:

```txt

N	 phi(n)	 Prime
1	 1	 false
2	 1	 true
3	 2	 true
4	 2	 false
5	 4	 true
6	 2	 false
7	 6	 true
8	 4	 false
9	 6	 false
10	 4	 false
11	 10	 true
12	 4	 false
13	 12	 true
14	 6	 false
15	 8	 false
16	 8	 false
17	 16	 true
18	 6	 false
19	 18	 true
20	 8	 false
21	 12	 false
22	 10	 false
23	 22	 true
24	 8	 false
25	 20	 false
Until 100: 25 prime numbers
Until 1000: 168 prime numbers
Until 10000: 1229 prime numbers
Until 100000: 9592 prime numbers

```



## Scala


The most concise way to write the totient function in Scala is using a naive lazy list:

```scala
@tailrec
def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a%b)
def totientLaz(num: Int): Int = LazyList.range(2, num).count(gcd(num, _) == 1) + 1
```


The above approach, while concise, is not very performant. It must check the GCD with every number between 2 and num, giving it an O(n*log(n)) time complexity. A better solution is to use the product definition of the totient, repeatedly extracting prime factors from num:

```scala
def totientPrd(num: Int): Int = {
  @tailrec
  def dTrec(f: Int, n: Int): Int = if(n%f == 0) dTrec(f, n/f) else n

  @tailrec
  def tTrec(ac: Int, i: Int, n: Int): Int = if(n != 1){
    if(n%i == 0) tTrec(ac*(i - 1)/i, i + 1, dTrec(i, n))
    else tTrec(ac, i + 1, n)
  }else{
    ac
  }

  tTrec(num, 2, num)
}
```


This version is significantly faster, but the introduction of multiple recursive methods makes it far less concise. We can, however, embed the recursion into a lazy list to obtain a function as fast as the second example yet almost as concise as the first, at the cost of some readability:

```scala
@tailrec
def scrub(f: Long, num: Long): Long = if(num%f == 0) scrub(f, num/f) else num
def totientLazPrd(num: Long): Long = LazyList.iterate((num, 2: Long, num)){case (ac, i, n) => if(n%i == 0) (ac*(i - 1)/i, i + 1, scrub(i, n)) else (ac, i + 1, n)}.find(_._3 == 1).get._1
```


To generate the output up to 100000, Longs are necessary.
```txt
1 (Not Prime): 1
2   (Prime)  : 1
3   (Prime)  : 2
4 (Not Prime): 2
5   (Prime)  : 4
6 (Not Prime): 2
7   (Prime)  : 6
8 (Not Prime): 4
9 (Not Prime): 6
10 (Not Prime): 4
11   (Prime)  : 10
12 (Not Prime): 4
13   (Prime)  : 12
14 (Not Prime): 6
15 (Not Prime): 8
16 (Not Prime): 8
17   (Prime)  : 16
18 (Not Prime): 6
19   (Prime)  : 18
20 (Not Prime): 8
21 (Not Prime): 12
22 (Not Prime): 10
23   (Prime)  : 22
24 (Not Prime): 8
25 (Not Prime): 20

Prime Count <= N...
100: 25
1000: 168
10000: 1229
100000: 9592
```



## Sidef

The Euler totient function is built-in as '''Number.euler_phi()''', but we can easily re-implement it using its multiplicative property: '''phi(p^k) = (p-1)*p^(k-1)'''.

```ruby
func 𝜑(n) {
    n.factor_exp.prod {|p|
        (p[0]-1) * p[0]**(p[1]-1)
    }
}
```



```ruby
for n in (1..25) {
    var totient = 𝜑(n)
    printf("𝜑(%2s) = %3s%s\n", n, totient, totient==(n-1) ? ' - prime' : '')
}
```

<pre style="height:35ex">
𝜑( 1) =   1
𝜑( 2) =   1 - prime
𝜑( 3) =   2 - prime
𝜑( 4) =   2
𝜑( 5) =   4 - prime
𝜑( 6) =   2
𝜑( 7) =   6 - prime
𝜑( 8) =   4
𝜑( 9) =   6
𝜑(10) =   4
𝜑(11) =  10 - prime
𝜑(12) =   4
𝜑(13) =  12 - prime
𝜑(14) =   6
𝜑(15) =   8
𝜑(16) =   8
𝜑(17) =  16 - prime
𝜑(18) =   6
𝜑(19) =  18 - prime
𝜑(20) =   8
𝜑(21) =  12
𝜑(22) =  10
𝜑(23) =  22 - prime
𝜑(24) =   8
𝜑(25) =  20

```



```ruby
[100, 1_000, 10_000, 100_000].each {|limit|
    var pi = (1..limit -> count_by {|n| 𝜑(n) == (n-1) })
    say "Number of primes <= #{limit}: #{pi}"
}
```

```txt

Number of primes <= 100: 25
Number of primes <= 1000: 168
Number of primes <= 10000: 1229
Number of primes <= 100000: 9592

```



## VBA

```vb
Private Function totient(ByVal n As Long) As Long
    Dim tot As Long: tot = n
    Dim i As Long: i = 2
    Do While i * i <= n
        If n Mod i = 0 Then
            Do While True
                n = n \ i
                If n Mod i <> 0 Then Exit Do
            Loop
            tot = tot - tot \ i
        End If
        i = i + IIf(i = 2, 1, 2)
    Loop
    If n > 1 Then
        tot = tot - tot \ n
    End If
    totient = tot
End Function

Public Sub main()
    Debug.Print " n  phi   prime"
    Debug.Print " --------------"
    Dim count As Long
    Dim tot As Integer, n As Long
    For n = 1 To 25
        tot = totient(n)
        prime = (n - 1 = tot)
        count = count - prime
        Debug.Print Format(n, "@@"); Format(tot, "@@@@@"); Format(prime, "@@@@@@@@")
    Next n
    Debug.Print
    Debug.Print "Number of primes up to 25     = "; Format(count, "@@@@")
    For n = 26 To 100000
        count = count - (totient(n) = n - 1)
        Select Case n
            Case 100, 1000, 10000, 100000
                Debug.Print "Number of primes up to"; n; String$(6 - Len(CStr(n)), " "); "="; Format(count, "@@@@@")
            Case Else
        End Select
    Next n
End Sub
```
```txt
 n  phi   prime
 --------------
 1    1   False
 2    1    True
 3    2    True
 4    2   False
 5    4    True
 6    2   False
 7    6    True
 8    4   False
 9    6   False
10    4   False
11   10    True
12    4   False
13   12    True
14    6   False
15    8   False
16    8   False
17   16    True
18    6   False
19   18    True
20    8   False
21   12   False
22   10   False
23   22    True
24    8   False
25   20   False

Number of primes up to 25     =    9
Number of primes up to 100    =   25
Number of primes up to 1000   =  168
Number of primes up to 10000  = 1229
Number of primes up to 100000 = 9592
```


## zkl


```zkl
fcn totient(n){ [1..n].reduce('wrap(p,k){ p + (n.gcd(k)==1) }) }
fcn isPrime(n){ totient(n)==(n - 1) }
```


```zkl
foreach n in ([1..25]){
   println("\u03c6(%2d) ==%3d %s"
      .fmt(n,totient(n),isPrime(n) and "is prime" or ""));
}
```

<pre style="height:35ex">
φ( 1) ==  1
φ( 2) ==  1 is prime
φ( 3) ==  2 is prime
φ( 4) ==  2
φ( 5) ==  4 is prime
φ( 6) ==  2
φ( 7) ==  6 is prime
φ( 8) ==  4
φ( 9) ==  6
φ(10) ==  4
φ(11) == 10 is prime
φ(12) ==  4
φ(13) == 12 is prime
φ(14) ==  6
φ(15) ==  8
φ(16) ==  8
φ(17) == 16 is prime
φ(18) ==  6
φ(19) == 18 is prime
φ(20) ==  8
φ(21) == 12
φ(22) == 10
φ(23) == 22 is prime
φ(24) ==  8
φ(25) == 20

```


```zkl
count:=0;
foreach n in ([1..10_000]){	// yes, this is sloooow
   count+=isPrime(n);
   if(n==100 or n==1000 or n==10_000)
      println("Primes <= %,6d : %,5d".fmt(n,count));
}
```

```txt

Primes <=    100 :    25
Primes <=  1,000 :   168
Primes <= 10,000 : 1,229

```

