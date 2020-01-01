+++
title = "Brazilian numbers"
description = ""
date = 2019-09-28T16:26:51Z
aliases = []
[extra]
id = 22457
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
Brazilian numbers are so called as they were first formally presented at the 1994 math Olympiad ''Olimpiada Iberoamericana de Matematica'' in Fortaleza, Brazil.

Brazilian numbers are defined as:

The set of positive integer numbers where each number '''N''' has at least one natural number '''B''' where '''1 < B < N-1''' where the representation of '''N''' in '''base B''' has all equal digits.


;E.G.:

:* '''1, 2 & 3''' can not be Brazilian; there is no base '''B''' that satisfies the condition '''1 < B < N-1'''.
:* '''4''' is not Brazilian; '''4''' in '''base 2''' is '''100'''. The digits are not all the same.
:* '''5''' is not Brazilian; '''5''' in '''base 2''' is '''101''', in '''base 3''' is '''12'''. There is no representation where the digits are the same.
:* '''6''' is not Brazilian; '''6''' in '''base 2''' is '''110''', in '''base 3''' is '''20''', in '''base 4''' is '''12'''. There is no representation where the digits are the same.
:* '''7''' ''is'' Brazilian; '''7''' in '''base 2''' is '''111'''. There is at least one representation where the digits are all the same.
:* '''8''' ''is'' Brazilian; '''8''' in '''base 3''' is '''22'''. There is at least one representation where the digits are all the same.
:* ''and so on...''


All even integers '''2P >= 8''' are Brazilian because '''2P = 2(P-1) + 2''', which is '''22''' in '''base P-1''' when '''P-1 > 2'''. That becomes true when '''P >= 4'''.<BR>
More common: all integers, that factor decomposition is '''R*S >= 8''', with '''S+1 > R''', are Brazilian because '''R*S = R(S-1) + R''', which is '''RR''' in '''base S-1'''<BR>
The only problematic numbers are squares of primes, where R = S.Only 11^2 is brazilian to base 3.<BR>
All prime integers, that are brazilian, can only have the digit '''1''' .Otherwise one could factor out the digit, therefore it cannot be a prime number.Mostly in form of '''111''' to base Integer(sqrt(prime number)).Must be an odd count of '''1''' to stay odd like primes >  2<BR>

;Task:

Write a routine (function, whatever) to determine if a number is Brazilian and use the routine to show here, on this page;

:* the first '''20''' Brazilian numbers;
:* the first '''20 odd''' Brazilian numbers;
:* the first '''20 prime''' Brazilian numbers;


;See also:

:* '''[[oeis:A125134|OEIS:A125134 - Brazilian numbers]]'''
:* '''[[oeis:A257521|OEIS:A257521 - Odd Brazilian numbers]]'''
:* '''[[oeis:A085104|OEIS:A085104 - Prime Brazilian numbers]]'''





## AWK


```AWK

# syntax: GAWK -f BRAZILIAN_NUMBERS.AWK
# converted from C
BEGIN {
    split(",odd ,prime ",kinds,",")
    for (i=1; i<=3; ++i) {
      printf("first 20 %sBrazilian numbers:",kinds[i])
      c = 0
      n = 7
      while (1) {
        if (is_brazilian(n)) {
          printf(" %d",n)
          if (++c == 20) {
            printf("\n")
            break
          }
        }
        switch (i) {
          case 1:
            n++
            break
          case 2:
            n += 2
            break
          case 3:
            do {
              n += 2
            } while (!is_prime(n))
            break
        }
      }
    }
    exit(0)
}
function is_brazilian(n,  b) {
    if (n < 7) { return(0) }
    if (!(n % 2) && n >= 8) { return(1) }
    for (b=2; b<n-1; ++b) {
      if (same_digits(n,b)) { return(1) }
    }
    return(0)
}
function is_prime(n,  d) {
    d = 5
    if (n < 2) { return(0) }
    if (!(n % 2)) { return(n == 2) }
    if (!(n % 3)) { return(n == 3) }
    while (d*d <= n) {
      if (!(n % d)) { return(0) }
      d += 2
      if (!(n % d)) { return(0) }
      d += 4
    }
    return(1)
}
function same_digits(n,b,  f) {
    f = n % b
    n = int(n/b)
    while (n > 0) {
      if (n % b != f) { return(0) }
      n = int(n/b)
    }
    return(1)
}

```

{{out}}

```txt

first 20 Brazilian numbers: 7 8 10 12 13 14 15 16 18 20 21 22 24 26 27 28 30 31 32 33
first 20 odd Brazilian numbers: 7 13 15 21 27 31 33 35 39 43 45 51 55 57 63 65 69 73 75 77
first 20 prime Brazilian numbers: 7 13 31 43 73 127 157 211 241 307 421 463 601 757 1093 1123 1483 1723 2551 2801

```


## C

{{trans|Go}}

```c
#include <stdio.h>

typedef char bool;

#define TRUE 1
#define FALSE 0

bool same_digits(int n, int b) {
    int f = n % b;
    n /= b;
    while (n > 0) {
        if (n % b != f) return FALSE;
        n /= b;
    }
    return TRUE;
}

bool is_brazilian(int n) {
    int b;
    if (n < 7) return FALSE;
    if (!(n % 2) && n >= 8) return TRUE;
    for (b = 2; b < n - 1; ++b) {
        if (same_digits(n, b)) return TRUE;
    }
    return FALSE;
}

bool is_prime(int n) {
    int d = 5;
    if (n < 2) return FALSE;
    if (!(n % 2)) return n == 2;
    if (!(n % 3)) return n == 3;
    while (d * d <= n) {
        if (!(n % d)) return FALSE;
        d += 2;
        if (!(n % d)) return FALSE;
        d += 4;
    }
    return TRUE;
}

int main() {
    int i, c, n;
    const char *kinds[3] = {" ", " odd ", " prime "};
    for (i = 0; i < 3; ++i) {
        printf("First 20%sBrazilian numbers:\n", kinds[i]);
        c = 0;
        n = 7;
        while (TRUE) {
            if (is_brazilian(n)) {
                printf("%d ", n);
                if (++c == 20) {
                    printf("\n\n");
                    break;
                }
            }
            switch (i) {
                case 0: n++; break;
                case 1: n += 2; break;
                case 2:
                    do {
                        n += 2;
                    } while (!is_prime(n));
                    break;
            }
        }
    }

    for (n = 7, c = 0; c < 100000; ++n) {
        if (is_brazilian(n)) c++;
    }
    printf("The 100,000th Brazilian number: %d\n", n - 1);
    return 0;
}
```


{{out}}

```txt

First 20 Brazilian numbers:
7 8 10 12 13 14 15 16 18 20 21 22 24 26 27 28 30 31 32 33

First 20 odd Brazilian numbers:
7 13 15 21 27 31 33 35 39 43 45 51 55 57 63 65 69 73 75 77

First 20 prime Brazilian numbers:
7 13 31 43 73 127 157 211 241 307 421 463 601 757 1093 1123 1483 1723 2551 2801

The 100,000th Brazilian number: 110468

```



## C++

{{trans|D}}

```cpp
#include <iostream>

bool sameDigits(int n, int b) {
    int f = n % b;
    while ((n /= b) > 0) {
        if (n % b != f) {
            return false;
        }
    }
    return true;
}

bool isBrazilian(int n) {
    if (n < 7) return false;
    if (n % 2 == 0)return true;
    for (int b = 2; b < n - 1; b++) {
        if (sameDigits(n, b)) {
            return true;
        }
    }
    return false;
}

bool isPrime(int n) {
    if (n < 2)return false;
    if (n % 2 == 0)return n == 2;
    if (n % 3 == 0)return n == 3;
    int d = 5;
    while (d * d <= n) {
        if (n % d == 0)return false;
        d += 2;
        if (n % d == 0)return false;
        d += 4;
    }
    return true;
}

int main() {
    for (auto kind : { "", "odd ", "prime " }) {
        bool quiet = false;
        int BigLim = 99999;
        int limit = 20;
        std::cout << "First " << limit << ' ' << kind << "Brazillian numbers:\n";
        int c = 0;
        int n = 7;
        while (c < BigLim) {
            if (isBrazilian(n)) {
                if (!quiet)std::cout << n << ' ';
                if (++c == limit) {
                    std::cout << "\n\n";
                    quiet = true;
                }
            }
            if (quiet && kind != "") continue;
            if (kind == "") {
                n++;
            }
            else if (kind == "odd ") {
                n += 2;
            }
            else if (kind == "prime ") {
                while (true) {
                    n += 2;
                    if (isPrime(n)) break;
                }
            } else {
                throw new std::runtime_error("Unexpected");
            }
        }
        if (kind == "") {
            std::cout << "The " << BigLim + 1 << "th Brazillian number is: " << n << "\n\n";
        }
    }

    return 0;
}
```

{{out}}

```txt
First 20 Brazillian numbers:
7 8 10 12 13 14 15 16 18 20 21 22 24 26 27 28 30 31 32 33

The 100000th Brazillian number is: 110468

First 20 odd Brazillian numbers:
7 13 15 21 27 31 33 35 39 43 45 51 55 57 63 65 69 73 75 77

First 20 prime Brazillian numbers:
7 13 31 43 73 127 157 211 241 307 421 463 601 757 1093 1123 1483 1723 2551 2801
```


=={{header|C#|CSharp}}==
{{trans|Go}}

```c#
using System;
class Program {

  static bool sameDigits(int n, int b) {
    int f = n % b;
    while ((n /= b) > 0) if (n % b != f) return false;
    return true;
  }

  static bool isBrazilian(int n) {
    if (n < 7) return false;
    if (n % 2 == 0) return true;
    for (int b = 2; b < n - 1; b++) if (sameDigits(n, b)) return true;
    return false;
  }

  static bool isPrime(int n) {
    if (n < 2) return false;
    if (n % 2 == 0) return n == 2;
    if (n % 3 == 0) return n == 3;
    int d = 5;
    while (d * d <= n) {
      if (n % d == 0) return false; d += 2;
      if (n % d == 0) return false; d += 4;
    }
    return true;
  }

  static void Main(string[] args) {
    foreach (string kind in ",odd ,prime ".Split(',')) {
      bool quiet = false; int BigLim = 99999, limit = 20;
      Console.WriteLine("First {0} {1}Brazilian numbers:", limit, kind);
      int c = 0, n = 7;
      while (c < BigLim) {
        if (isBrazilian(n)) {
          if (!quiet) Console.Write("{0:n0} ", n);
          if (++c == limit) { Console.Write("\n\n"); quiet = true; }
        }
        if (quiet && kind != "") continue;
        switch (kind) {
          case "": n++; break;
          case "odd ": n += 2; break;
          case "prime ":
            while (true) {
              n += 2;
              if (isPrime(n)) break;
            } break;
        }
      }
      if (kind == "") Console.WriteLine("The {0:n0}th Brazilian number is: {1:n0}\n", BigLim + 1, n);
    }
  }
}
```

{{out}}

```txt
First 20 Brazilian numbers:
7 8 10 12 13 14 15 16 18 20 21 22 24 26 27 28 30 31 32 33

The 100,000th Brazilian number is: 110,468

First 20 odd Brazilian numbers:
7 13 15 21 27 31 33 35 39 43 45 51 55 57 63 65 69 73 75 77

First 20 prime Brazilian numbers:
7 13 31 43 73 127 157 211 241 307 421 463 601 757 1,093 1,123 1,483 1,723 2,551 2,801

```

Regarding the 100,000th number, there is a wee discrepancy here with the '''F#''' version.  The OEIS reference only goes to 4000, which is 4618. (4000 being the highest result published elsewhere)


### Speedier Version

Based on the Pascal version, with some shortcuts.  Can calculate to one billion in under 4 1/2 seconds (on a core i7).  This is faster than the Pascal version because the sieve is an array of '''SByte''' (8 bits) and not a '''NativeUInt''' (32 bits).  Also this code does not preserve the base of each Brazilain number in the array, so the Pascal version is more flexible if desiring to quickly verify a quantity of Brazilian numbers.

```c#
using System;

class Program
{
    const           // flags:
    int PrMk = 0,   // a number that is prime
        SqMk = 1,   // a number that is the square of a prime number
        UpMk = 2,   // a number that can be factored (aka un-prime)
        BrMk = -2,  // a prime number that is also a Brazilian number
        Excp = 121; // exception square - the only square prime that is a Brazilian

    static int pow = 9, // power of 10 to count to
        max; // maximum sieve array length
             // An upper limit of the required array length can be calculated like this:
             // power of 10  fraction              limit        actual result
             //   1          2 / 1 * 10          = 20           20
             //   2          4 / 3 * 100         = 133          132
             //   3          6 / 5 * 1000        = 1200         1191
             //   4          8 / 7 * 10000       = 11428        11364
             //   5          10/ 9 * 100000      = 111111       110468
             //   6          12/11 * 1000000     = 1090909      1084566
             //   7          14/13 * 10000000    = 10769230     10708453
             //   8          16/15 * 100000000   = 106666666    106091516
             //   9          18/17 * 1000000000  = 1058823529   1053421821
             // powers above 9 are impractical because of the maximum array length in C#,
             // which is around the UInt32.MaxValue, or 4294967295

    static SByte[] PS; // the prime/Brazilian number sieve
    // once the sieve is populated, primes are <= 0, non-primes are > 0,
    // Brazilian numbers are (< 0) or (> 1)
    // 121 is a special case, in the sieve it is marked with the BrMk (-2)

    // typical sieve of Eratosthenes algorithm
    static void PrimeSieve(int top) {
        PS = new SByte[top]; int i, ii, j;
        i = 2; PS[j = 4] = SqMk; while (j < top - 2) PS[j += 2] = UpMk;
        i = 3; PS[j = 9] = SqMk; while (j < top - 6) PS[j += 6] = UpMk;
        i = 5; while ((ii = i * i) < top) { if (PS[i] == PrMk) {
                j = (top - i) / i; if ((j & 1) == 0) j--;
                do if (PS[j] == PrMk) PS[i * j] = UpMk;
                while ((j -= 2) > i); PS[ii] = SqMk;
            } do ; while (PS[i += 2] != PrMk); }
    }

    // consults the sieve and returns whether a number is Brazilian
    static bool IsBr(int number) { return Math.Abs(PS[number]) > SqMk; }

    // shows the first few Brazilian numbers of several kinds
    static void FirstFew(string kind, int amt) {
        Console.WriteLine("\nThe first {0} {1}Brazilian Numbers are:", amt, kind);
        int i = 7; while (amt > 0) { if (IsBr(i)) { amt--; Console.Write("{0} ", i); }
            switch (kind) {
                case "odd ": i += 2; break;
                case "prime ": do i += 2; while (PS[i] != BrMk || i == Excp); break;
                default: i++; break; } } Console.WriteLine();
    }

    // expands a 111_X number into an integer
    static int Expand(int NumberOfOnes, int Base) {
        int res = 1; while (NumberOfOnes-- > 1) res = res * Base + 1;
        if (res > max || res < 0) res = 0; return res;
    }

    // displays an elapsed time stamp
    static string TS(string fmt, ref DateTime st, bool reset = false) {
        DateTime n = DateTime.Now;
        string res = string.Format(fmt, (n - st).TotalMilliseconds);
        if (reset) st = n; return res;
    }

    static void Main(string[] args) {
        int p2 = pow << 1; DateTime st = DateTime.Now, st0 = st;
        int p10 = (int)Math.Pow(10, pow), p = 10, cnt = 0;
        max = (int)(((long)(p10) * p2) / (p2 - 1)); PrimeSieve(max);
        Console.WriteLine(TS("Sieving took {0} ms", ref st, true));
        int[] primes = new int[7]; // make short list of primes before Brazilians are added
        int n = 3; for (int i = 0; i < primes.Length; i++) { primes[i] = n; do ; while (PS[n += 2] != 0); }
        Console.WriteLine("\nChecking first few prime numbers of sequential ones:\nones checked found");
        // now check the '111_X' style numbers. many are factorable, but some are prime,
        // then re-mark the primes found in the sieve as Brazilian.
        // curiously, only the numbers with a prime number of ones will turn out, so
        // restricting the search to those saves time. no need to wast time on even numbers of ones,
        // or 9 ones, 15 ones, etc...
        foreach(int i in primes) { Console.Write("{0,4}", i); cnt = 0; n = 2;
            do { if ((n - 1) % i != 0) { long br = Expand(i, n);
                    if (br > 0) { if (PS[br] < UpMk) { PS[br] = BrMk; cnt++; } }
                    else { Console.WriteLine("{0,8}{1,6}", n, cnt); break; } }
                n++; } while (true); }
        Console.WriteLine(TS("Adding Brazilian primes to the sieve took {0} ms", ref st, true));
        foreach (string s in ",odd ,prime ".Split(',')) FirstFew(s, 20);
        Console.WriteLine(TS("\nRequired output took {0} ms", ref st, true));
        Console.WriteLine("\nDecade count of Brazilian numbers:");
        n = 6; cnt = 0; do { while (cnt < p) if (IsBr(++n)) cnt++;
            Console.WriteLine("{0,15:n0}th is {1,-15:n0}  {2}", cnt, n, TS("time: {0} ms", ref st));
        } while ((p *= 10) <= p10); PS = new sbyte[0];
        Console.WriteLine("\nTotal elapsed was {0} ms", (DateTime.Now - st0).TotalMilliseconds);
        if (System.Diagnostics.Debugger.IsAttached) Console.ReadKey();
    }
}
```

{{out}}

```txt
Sieving took 3009.2927 ms

Checking first few prime numbers of sequential ones:
ones checked found
   3   32540  3923
   5     182    44
   7      32     9
  11       8     1
  13       6     3
  17       4     1
  19       4     1
Adding Brazilian primes to the sieve took 8.3535 ms

The first 20 Brazilian Numbers are:
7 8 10 12 13 14 15 16 18 20 21 22 24 26 27 28 30 31 32 33

The first 20 odd Brazilian Numbers are:
7 13 15 21 27 31 33 35 39 43 45 51 55 57 63 65 69 73 75 77

The first 20 prime Brazilian Numbers are:
7 13 31 43 73 127 157 211 241 307 421 463 601 757 1093 1123 1483 1723 2551 2801

Required output took 2.4881 ms

Decade count of Brazilian numbers:
             10th is 20               time: 0.057 ms
            100th is 132              time: 0.1022 ms
          1,000th is 1,191            time: 0.1351 ms
         10,000th is 11,364           time: 0.1823 ms
        100,000th is 110,468          time: 0.3758 ms
      1,000,000th is 1,084,566        time: 1.8601 ms
     10,000,000th is 10,708,453       time: 17.8373 ms
    100,000,000th is 106,091,516      time: 155.2622 ms
  1,000,000,000th is 1,053,421,821    time: 1448.9392 ms

Total elapsed was 4469.1985 ms
```
 P.S. The best speed on Tio.run is under 5 seconds for the 100 millionth count ('''pow''' = 8).  If you are very persistent, the 1 billionth count ('''pow''' = 9) can be made to work on Tio.run, it usually overruns the 60 second timeout limit, and cannot finish completely - the sieving by itself takes over 32 seconds (best case), which usually doesn't leave enough time for all the counting.


## D

{{trans|C#}}

```d
import std.stdio;

bool sameDigits(int n, int b) {
    int f = n % b;
    while ((n /= b) > 0) {
        if (n % b != f) {
            return false;
        }
    }
    return true;
}

bool isBrazilian(int n) {
    if (n < 7) return false;
    if (n % 2 == 0) return true;
    for (int b = 2; b < n - 1; ++b) {
        if (sameDigits(n, b)) {
            return true;
        }
    }
    return false;
}

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

void main() {
    foreach (kind; ["", "odd ", "prime "]) {
        bool quiet = false;
        int BigLim = 99999;
        int limit = 20;
        writefln("First %s %sBrazillion numbers:", limit, kind);
        int c = 0;
        int n = 7;
        while (c < BigLim) {
            if (isBrazilian(n)) {
                if (!quiet) write(n, ' ');
                if (++c == limit) {
                    writeln("\n");
                    quiet = true;
                }
            }
            if (quiet && kind != "") continue;
            switch (kind) {
                case "": n++; break;
                case "odd ": n += 2; break;
                case "prime ":
                    while (true) {
                        n += 2;
                        if (isPrime(n)) break;
                    }
                    break;
                default: assert(false);
            }
        }
        if (kind == "") writefln("The %sth Brazillian number is: %s\n", BigLim + 1, n);
    }
}
```

{{out}}

```txt
First 20 Brazillion numbers:
7 8 10 12 13 14 15 16 18 20 21 22 24 26 27 28 30 31 32 33

The 100000th Brazillian number is: 110468

First 20 odd Brazillion numbers:
7 13 15 21 27 31 33 35 39 43 45 51 55 57 63 65 69 73 75 77

First 20 prime Brazillion numbers:
7 13 31 43 73 127 157 211 241 307 421 463 601 757 1093 1123 1483 1723 2551 2801
```


=={{header|F_Sharp|F#}}==

### The functions


```fsharp

// Generate Brazilian sequence. Nigel Galloway: August 13th., 2019
let isBraz α=let mutable n,i,g=α,α+1,1 in (fun β->(while (i*g)<β do if g<α-1 then g<-g+1 else (n<-n*α; i<-n+i; g<-1)); β=i*g)

let Brazilian()=let rec fN n g=seq{if List.exists(fun α->α n) g then yield n
                                   yield! fN (n+1) ((isBraz (n-1))::g)}
                fN 4 [isBraz 2]

```


### The Tasks

;the first 20 Brazilian numbers

```fsharp

Brazilian() |> Seq.take 20 |> Seq.iter(printf "%d "); printfn ""

```

{{out}}

```txt

7 8 10 12 13 14 15 16 18 20 21 22 24 26 27 28 30 31 32 33

```

;the first 20 odd Brazilian numbers

```fsharp

Brazilian() |> Seq.filter(fun n->n%2=1) |> Seq.take 20 |> Seq.iter(printf "%d "); printfn ""

```

{{out}}

```txt

7 13 15 21 27 31 33 35 39 43 45 51 55 57 63 65 69 73 75 77

```

;the first 20 prime Brazilian numbers
Using [http://www.rosettacode.org/wiki/Extensible_prime_generator#The_function Extensible Prime Generator (F#)]

```fsharp

Brazilian() |> Seq.filter isPrime |> Seq.take 20 |> Seq.iter(printf "%d "); printfn ""

```

{{out}}

```txt

7 13 31 43 73 127 157 211 241 307 421 463 601 757 1093 1123 1483 1723 2551 2801

```

;finally that which the crowd really want to know: What is the 100,000<sup>th</sup> Brazilian number?

```fsharp

printfn "%d" (Seq.item 99999 Brazilian)

```

{{out}}

```txt

110468

```

So up to 100,000 ~10% of numbers are non-Brazilian. The millionth Brazilian is 1084566 so less than 10% are non-Brazilian. Large non-Brazilians seem to be rare.


## Factor

{{works with|Factor|0.99 development release 2019-07-10}}

```factor
USING: combinators grouping io kernel lists lists.lazy math
math.parser math.primes.lists math.ranges namespaces prettyprint
prettyprint.config sequences ;

: (brazilian?) ( n -- ? )
    2 over 2 - [a,b] [ >base all-equal? ] with find nip >boolean ;

: brazilian? ( n -- ? )
    {
        { [ dup 7 < ] [ drop f ] }
        { [ dup even? ] [ drop t ] }
        [ (brazilian?) ]
    } cond ;

: .20-brazilians ( list -- )
    [ 20 ] dip [ brazilian? ] lfilter ltake list>array . ;

100 margin set
1 lfrom "First 20 Brazilian numbers:"
1 [ 2 + ] lfrom-by "First 20 odd Brazilian numbers:"
lprimes "First 20 prime Brazilian numbers:"
[ print .20-brazilians nl ] 2tri@
```

{{out}}

```txt

First 20 Brazilian numbers:
{ 7 8 10 12 13 14 15 16 18 20 21 22 24 26 27 28 30 31 32 33 }

First 20 odd Brazilian numbers:
{ 7 13 15 21 27 31 33 35 39 43 45 51 55 57 63 65 69 73 75 77 }

First 20 prime Brazilian numbers:
{ 7 13 31 43 73 127 157 211 241 307 421 463 601 757 1093 1123 1483 1723 2551 2801 }

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Brazilian_numbers this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Go


### Version 1


```go
package main

import "fmt"

func sameDigits(n, b int) bool {
    f := n % b
    n /= b
    for n > 0 {
        if n%b != f {
            return false
        }
        n /= b
    }
    return true
}

func isBrazilian(n int) bool {
    if n < 7 {
        return false
    }
    if n%2 == 0 && n >= 8 {
        return true
    }
    for b := 2; b < n-1; b++ {
        if sameDigits(n, b) {
            return true
        }
    }
    return false
}

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

func main() {
    kinds := []string{" ", " odd ", " prime "}
    for _, kind := range kinds {
        fmt.Printf("First 20%sBrazilian numbers:\n", kind)
        c := 0
        n := 7
        for {
            if isBrazilian(n) {
                fmt.Printf("%d ", n)
                c++
                if c == 20 {
                    fmt.Println("\n")
                    break
                }
            }
            switch kind {
            case " ":
                n++
            case " odd ":
                n += 2
            case " prime ":
                for {
                    n += 2
                    if isPrime(n) {
                        break
                    }
                }
            }
        }
    }

    n := 7
    for c := 0; c < 100000; n++ {
        if isBrazilian(n) {
            c++
        }
    }
    fmt.Println("The 100,000th Brazilian number:", n-1)
}
```


{{out}}

```txt

First 20 Brazilian numbers:
7 8 10 12 13 14 15 16 18 20 21 22 24 26 27 28 30 31 32 33

First 20 odd Brazilian numbers:
7 13 15 21 27 31 33 35 39 43 45 51 55 57 63 65 69 73 75 77

First 20 prime Brazilian numbers:
7 13 31 43 73 127 157 211 241 307 421 463 601 757 1093 1123 1483 1723 2551 2801

The 100,000th Brazilian number: 110468

```



### Version 2

{{trans|C# (speedier version)}}


Some of the comments have been omitted in the interests of brevity.

Running a bit quicker than the .NET versions though not due to any further improvements on my part.

```go
package main

import (
    "fmt"
    "math"
    "time"
)

// flags
const (
    prMk int8 = 0   // prime
    sqMk      = 1   // prime square
    upMk      = 2   // non-prime
    brMk      = -2  // Brazilian prime
    excp      = 121 // the only Brazilian square prime
)

var (
    pow = 9
    max = 0
    ps  []int8
)

// typical sieve of Eratosthenes
func primeSieve(top int) {
    ps = make([]int8, top)
    i, j := 2, 4
    ps[j] = sqMk
    for j < top-2 {
        j += 2
        ps[j] = upMk
    }
    i, j = 3, 9
    ps[j] = sqMk
    for j < top-6 {
        j += 6
        ps[j] = upMk
    }
    i = 5
    for i*i < top {
        if ps[i] == prMk {
            j = (top - i) / i
            if (j & 1) == 0 {
                j--
            }
            for {
                if ps[j] == prMk {
                    ps[i*j] = upMk
                }
                j -= 2
                if j <= i {
                    break
                }
            }
            ps[i*i] = sqMk
        }
        for {
            i += 2
            if ps[i] == prMk {
                break
            }
        }
    }
}

// returns whether a number is Brazilian
func isBr(number int) bool {
    temp := ps[number]
    if temp < 0 {
        temp = -temp
    }
    return temp > sqMk
}

// shows the first few Brazilian numbers of several kinds
func firstFew(kind string, amt int) {
    fmt.Printf("\nThe first %d %sBrazilian numbers are:\n", amt, kind)
    i := 7
    for amt > 0 {
        if isBr(i) {
            amt--
            fmt.Printf("%d ", i)
        }
        switch kind {
        case "odd ":
            i += 2
        case "prime ":
            for {
                i += 2
                if ps[i] == brMk && i != excp {
                    break
                }
            }
        default:
            i++
        }
    }
    fmt.Println()
}

// expands a 111_X number into an integer
func expand(numberOfOnes, base int) int {
    res := 1
    for numberOfOnes > 1 {
        numberOfOnes--
        res = res*base + 1
    }
    if res > max || res < 0 {
        res = 0
    }
    return res
}

func toMs(d time.Duration) float64 {
    return float64(d) / 1e6
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
    start := time.Now()
    st0 := start
    p2 := pow << 1
    p10 := int(math.Pow10(pow))
    p, cnt := 10, 0
    max = p10 * p2 / (p2 - 1)
    primeSieve(max)
    fmt.Printf("Sieving took %.4f ms\n", toMs(time.Since(start)))
    start = time.Now()
    primes := make([]int, 7)
    n := 3
    for i := 0; i < len(primes); i++ {
        primes[i] = n
        for {
            n += 2
            if ps[n] == 0 {
                break
            }
        }
    }
    fmt.Println("\nChecking first few prime numbers of sequential ones:")
    fmt.Println("ones checked found")
    for _, i := range primes {
        fmt.Printf("%4d", i)
        cnt, n = 0, 2
        for {
            if (n-1)%i != 0 {
                br := expand(i, n)
                if br > 0 {
                    if ps[br] < upMk {
                        ps[br] = brMk
                        cnt++
                    }
                } else {
                    fmt.Printf("%8d%6d\n", n, cnt)
                    break
                }
            }
            n++
        }
    }
    ms := toMs(time.Since(start))
    fmt.Printf("Adding Brazilian primes to the sieve took %.4f ms\n", ms)
    start = time.Now()
    for _, s := range []string{"", "odd ", "prime "} {
        firstFew(s, 20)
    }
    fmt.Printf("\nRequired output took %.4f ms\n", toMs(time.Since(start)))
    fmt.Println("\nDecade count of Brazilian numbers:")
    n, cnt = 6, 0
    for {
        for cnt < p {
            n++
            if isBr(n) {
                cnt++
            }
        }
        ms = toMs(time.Since(start))
        fmt.Printf("%15sth is %-15s  time: %8.4f ms\n", commatize(cnt), commatize(n), ms)
        p *= 10
        if p > p10 {
            break
        }
    }
    fmt.Printf("\nTotal elapsed was %.4f ms\n", toMs(time.Since(st0)))
}
```


{{out}}
Timings are for an Intel Core i7-8565U machine using Go 1.12.9 on Ubuntu 18.04.

```txt

Sieving took 2489.6647 ms

Checking first few prime numbers of sequential ones:
ones checked found
   3   32540  3923
   5     182    44
   7      32     9
  11       8     1
  13       6     3
  17       4     1
  19       4     1
Adding Brazilian primes to the sieve took 1.2049 ms

The first 20 Brazilian numbers are:
7 8 10 12 13 14 15 16 18 20 21 22 24 26 27 28 30 31 32 33

The first 20 odd Brazilian numbers are:
7 13 15 21 27 31 33 35 39 43 45 51 55 57 63 65 69 73 75 77

The first 20 prime Brazilian numbers are:
7 13 31 43 73 127 157 211 241 307 421 463 601 757 1093 1123 1483 1723 2551 2801

Required output took 0.0912 ms

Decade count of Brazilian numbers:
             10th is 20               time:   0.0951 ms
            100th is 132              time:   0.0982 ms
          1,000th is 1,191            time:   0.1015 ms
         10,000th is 11,364           time:   0.1121 ms
        100,000th is 110,468          time:   0.2201 ms
      1,000,000th is 1,084,566        time:   0.9421 ms
     10,000,000th is 10,708,453       time:   8.0068 ms
    100,000,000th is 106,091,516      time:  78.0114 ms
  1,000,000,000th is 1,053,421,821    time: 758.0320 ms

Total elapsed was 3249.0197 ms

```



## Haskell


```haskell
import Data.Numbers.Primes (primes)

isBrazil :: Int -> Bool
isBrazil n = 7 <= n && (even n || any (monoDigit n) [2 .. n - 2])

monoDigit :: Int -> Int -> Bool
monoDigit n b =
  let (q, d) = quotRem n b
  in d ==
     snd
       (until
          (uncurry (flip ((||) . (d /=)) . (0 ==)))
          ((`quotRem` b) . fst)
          (q, d))

main :: IO ()
main =
  mapM_
    (\(s, xs) ->
        (putStrLn . concat)
          [ "First 20 "
          , s
          , " Brazilians:\n"
          , show . take 20 $ filter isBrazil xs
          , "\n"
          ])
    [([], [1 ..]), ("odd", [1,3 ..]), ("prime", primes)]
```

{{Out}}

```txt
First 20 Brazilians:
[7,8,10,12,13,14,15,16,18,20,21,22,24,26,27,28,30,31,32,33]

First 20 odd Brazilians:
[7,13,15,21,27,31,33,35,39,43,45,51,55,57,63,65,69,73,75,77]

First 20 prime Brazilians:
[7,13,31,43,73,127,157,211,241,307,421,463,601,757,1093,1123,1483,1723,2551,2801]
```



## Julia

{{trans|Go}}

```julia
using Primes, Lazy

function samedigits(n, b)
    n, f = divrem(n, b)
    while n > 0
        n, f2 = divrem(n, b)
        if f2 != f
            return false
        end
    end
    true
end

isbrazilian(n) = n >= 7 && (iseven(n) || any(b -> samedigits(n, b), 2:n-2))
brazilians = filter(isbrazilian, Lazy.range())
oddbrazilians = filter(n -> isodd(n) && isbrazilian(n), Lazy.range())
primebrazilians = filter(n -> isprime(n) && isbrazilian(n), Lazy.range())

println("The first 20 Brazilian numbers are: ", take(20, brazilians))

println("The first 20 odd Brazilian numbers are: ", take(20, oddbrazilians))

println("The first 20 prime Brazilian numbers are: ", take(20, primebrazilians))

```
{{out}}

```txt

The first 20 Brazilian numbers are: (7 8 10 12 13 14 15 16 18 20 21 22 24 26 27 28 30 31 32 33)
The first 20 odd Brazilian numbers are: (7 13 15 21 27 31 33 35 39 43 45 51 55 57 63 65 69 73 75 77)
The first 20 prime Brazilian numbers are: (7 13 31 43 73 127 157 211 241 307 421 463 601 757 1093 1123 1483 1723 2551 2801)

```


There has been some discussion of larger numbers in the sequence. See below:

```julia
function braziliandensities(N, interval)
    count, intervalcount, icount = 0, 0, 0
    intervalcounts = Int[]
    for i in 7:typemax(Int)
        intervalcount += 1
        if intervalcount > interval
            push!(intervalcounts, icount)
            intervalcount = 0
            icount = 0
        end
        if isbrazilian(i)
            icount += 1
            count += 1
            if count == N
                println("The $N th brazilian is $i.")
                return [n/interval for n in intervalcounts]
            end
        end
    end
end

braziliandensities(10000, 100)
braziliandensities(100000, 1000)
plot(1:1000:1000000, braziliandensities(1000000, 1000))

```
{{out}}

```txt

The 10000 th brazilian is 11364.
The 100000 th brazilian is 110468.
The 1000000 th brazilian is 1084566.

```


[http://alahonua.com/temp/newplot.png link plot png]

## Kotlin

{{trans|C#}}

```scala
fun sameDigits(n: Int, b: Int): Boolean {
    var n2 = n
    val f = n % b
    while (true) {
        n2 /= b
        if (n2 > 0) {
            if (n2 % b != f) {
                return false
            }
        } else {
            break
        }
    }
    return true
}

fun isBrazilian(n: Int): Boolean {
    if (n < 7) return false
    if (n % 2 == 0) return true
    for (b in 2 until n - 1) {
        if (sameDigits(n, b)) {
            return true
        }
    }
    return false
}

fun isPrime(n: Int): Boolean {
    if (n < 2) return false
    if (n % 2 == 0) return n == 2
    if (n % 3 == 0) return n == 3
    var d = 5
    while (d * d <= n) {
        if (n % d == 0) return false
        d += 2
        if (n % d == 0) return false
        d += 4
    }
    return true
}

fun main() {
    val bigLim = 99999
    val limit = 20
    for (kind in ",odd ,prime".split(',')) {
        var quiet = false
        println("First $limit ${kind}Brazilian numbers:")
        var c = 0
        var n = 7
        while (c < bigLim) {
            if (isBrazilian(n)) {
                if (!quiet) print("%,d ".format(n))
                if (++c == limit) {
                    print("\n\n")
                    quiet = true
                }
            }
            if (quiet && kind != "") continue
            when (kind) {
                "" -> n++
                "odd " -> n += 2
                "prime" -> {
                    while (true) {
                        n += 2
                        if (isPrime(n)) break
                    }
                }
            }
        }
        if (kind == "") println("The %,dth Brazilian number is: %,d".format(bigLim + 1, n))
    }
}
```

{{out}}

```txt
First 20 Brazilian numbers:
7 8 10 12 13 14 15 16 18 20 21 22 24 26 27 28 30 31 32 33

The 100,000th Brazilian number is: 110,468
First 20 odd Brazilian numbers:
7 13 15 21 27 31 33 35 39 43 45 51 55 57 63 65 69 73 75 77

First 20 primeBrazilian numbers:
7 13 31 43 73 127 157 211 241 307 421 463 601 757 1,093 1,123 1,483 1,723 2,551 2,801
```



## Pascal

{{works with|Free Pascal}}
Using a sieve of Erathostenes to memorize the smallest factor of a composite number.
Checking primes first for '''111''' to base and if not then to '''11111''' ( Base^4+Base^3..+^1 = (Base^5 -1) / (Base-1) )
extreme reduced runtime time for space.<BR>
At the end only primes and square of primes need to be tested, all others are Brazilian.

```pascal
program brazilianNumbers;

{$IFDEF FPC}
  {$MODE DELPHI}{$OPTIMIZATION ON,All}
  {$CODEALIGN proc=32,loop=4}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}
uses
  SysUtils;

const
  //Must not be a prime
  PrimeMarker = 0;
  SquareMarker = PrimeMarker + 1;
  //MAX =    110468;// 1E5 brazilian
  //MAX =   1084566;// 1E6 brazilian
  //MAX =  10708453;// 1E7 brazilian
  //MAX = 106091516;// 1E8 brazilian
  MAX = 1053421821;// 1E9 brazilian

var
  isprime: array of word;

  procedure MarkSmallestFactor;
  //sieve of erathotenes
  //but saving the smallest factor
  var
    i, j, lmt: NativeUint;
  begin
    lmt := High(isPrime);
    fillWord(isPrime[0], lmt + 1, PrimeMarker);
    //mark even numbers
    i := 2;
    j := i * i;
    isPrime[j] := SquareMarker;
    Inc(j, 2);
    while j <= lmt do
    begin
      isPrime[j] := 2;
      Inc(j, 2);
    end;
    //mark 3 but not 2
    i := 3;
    j := i * i;
    isPrime[j] := SquareMarker;
    Inc(j, 6);
    while j <= lmt do
    begin
      isPrime[j] := 3;
      Inc(j, 6);
    end;

    i := 5;
    while i * i <= lmt do
    begin
      if isPrime[i] = 0 then
      begin
        j := lmt div i;
        if not (odd(j)) then
          Dec(j);
        while j > i do
        begin
          if isPrime[j] = 0 then
            isPrime[i * j] := i;
          Dec(j, 2);
        end;
        //mark square prime
        isPrime[i * i] := SquareMarker;
      end;
      Inc(i, 2);
    end;
  end;

  procedure OutFactors(n: NativeUint);
  var
    divisor, Next, rest: NativeUint;
    pot: NativeUint;
  begin
    divisor := 2;
    Next := 3;
    rest := n;
    Write(n: 10, ' = ');
    while (rest <> 1) do
    begin
      if (rest mod divisor = 0) then
      begin
        Write(divisor);
        pot := 0;
        repeat
          rest := rest div divisor;
          Inc(pot)
        until rest mod divisor <> 0;
        if pot > 1 then
          Write('^', pot);
        if rest > 1 then
          Write('*');
      end;
      divisor := Next;
      Next := Next + 2;
      // cut condition: avoid many useless iterations
      if (rest <> 1) and (rest < divisor * divisor) then
      begin
        Write(rest);
        rest := 1;
      end;
    end;
    Write('  ', #9#9#9);
  end;

  procedure OutToBase(number, base: NativeUint);
  var
    BaseDgt: array[0..63] of NativeUint;
    i, rest: NativeINt;
  begin
    OutFactors(number);
    i := 0;
    while number <> 0 do
    begin
      rest := number div base;
      BaseDgt[i] := number - rest * base;
      number := rest;
      Inc(i);
    end;
    while i > 1 do
    begin
      Dec(i);
      Write(BaseDgt[i]);
    end;
    writeln(BaseDgt[0], ' to base ', base);
  end;

  function PrimeBase(number: NativeUint): NativeUint;
  var
    lnN: extended;
    i, exponent, n: NativeUint;
  begin
    // primes are only brazilian if 111...11 to base > 2
    // the count of "1" must be odd , because brazilian primes are odd
    lnN := ln(number);
    exponent := 4;
    //result := exponent.th root of number
    Result := trunc(exp(lnN*0.25));
    while result >2 do
    Begin
      // calc sum(i= 0 to exponent ) base^i;
      n := Result + 1;
      i := 2;
      repeat
        Inc(i);
        n := n*result + 1;
      until i > exponent;
      if n = number then
        EXIT;
      Inc(exponent,2);
      Result := trunc(exp(lnN/exponent));
    end;
    //not brazilian
    Result := 0;
  end;

  function GetPrimeBrazilianBase(number: NativeUint): NativeUint;
    //result is base
  begin
    // prime of 2^n - 1
    if (Number and (number + 1)) = 0 then
      Result := 2
    else
    begin
      Result := trunc(sqrt(number));
      //most of the brazilian primes are of this type base^2+base+1
      IF (sqr(result)+result+1) <> number then
        result := PrimeBase(number);
    end;
  end;

  function GetBrazilianBase(number: NativeUInt): NativeUint; inline;
  begin
    Result := isPrime[number];
    if Result > SquareMarker then
      Result := (number div Result) - 1
    else
    begin
      if Result = SquareMarker then
      begin
        if number = 121 then
          Result := 3
        else
          Result := 0;
      end
      else
        Result := GetPrimeBrazilianBase(number);
    end;
  end;

  procedure First20Brazilian;
  var
    i, n, cnt: NativeUInt;
  begin
    writeln('first 20 brazilian numbers');
    i := 7;
    cnt := 0;
    while cnt < 20 do
    begin
      n := GetBrazilianBase(i);
      if n <> 0 then
      begin
        Inc(cnt);
        OutToBase(i, n);
      end;
      Inc(i);
    end;
    writeln;
  end;

  procedure First33OddBrazilian;
  var
    i, n, cnt: NativeUInt;
  begin
    writeln('first 33 odd brazilian numbers');
    i := 7;
    cnt := 0;
    while cnt < 33 do
    begin
      n := GetBrazilianBase(i);
      if N <> 0 then
      begin
        Inc(cnt);
        OutToBase(i, n);
      end;
      Inc(i, 2);
    end;
    writeln;
  end;

  procedure First20BrazilianPrimes;
  var
    i, n, cnt: NativeUInt;
  begin
    writeln('first 20 brazilian prime numbers');
    i := 7;
    cnt := 0;
    while cnt < 20 do
    begin
      IF isPrime[i] = PrimeMarker then
      Begin
        n := GetBrazilianBase(i);
        if n <> 0 then
        begin
          Inc(cnt);
          OutToBase(i, n);
        end;
      end;
      Inc(i);
    end;
    writeln;
  end;

var
  T1, T0: TDateTime;
  i, n, cnt, lmt: NativeUInt;
begin
  lmt := MAX;
  setlength(isPrime, lmt + 1);
  MarkSmallestFactor;

  First20Brazilian;
  First33OddBrazilian;
  First20BrazilianPrimes;

  Write('count brazilian numbers up to ', lmt, ' = ');
  T0 := now;
  i := 7;
  cnt := 0;
  n := 0;

  while (i <= lmt) do
  begin
    Inc(n, Ord(isPrime[i] = PrimeMarker));
    if GetBrazilianBase(i) <> 0 then
      Inc(cnt);
    Inc(i);
  end;

  T1 := now;

  writeln(cnt);
  writeln('Count of primes ', n: 11+13);
  writeln((T1 - T0) * 86400 * 1000: 10: 0, ' ms');

  setlength(isPrime, 0);
end.
```

{{out}}

```txt
first 20 brazilian numbers
         7 = 7              111 to base 2
         8 = 2^3            22 to base 3
        10 = 2*5            22 to base 4
        12 = 2^2*3              22 to base 5
        13 = 13             111 to base 3
        14 = 2*7            22 to base 6
        15 = 3*5            33 to base 4
        16 = 2^4            22 to base 7
        18 = 2*3^2              22 to base 8
        20 = 2^2*5              22 to base 9
        21 = 3*7            33 to base 6
        22 = 2*11           22 to base 10
        24 = 2^3*3              22 to base 11
        26 = 2*13           22 to base 12
        27 = 3^3            33 to base 8
        28 = 2^2*7              22 to base 13
        30 = 2*3*5              22 to base 14
        31 = 31             11111 to base 2
        32 = 2^5            22 to base 15
        33 = 3*11           33 to base 10

first 33 odd brazilian numbers
         7 = 7              111 to base 2
        13 = 13             111 to base 3
        15 = 3*5            33 to base 4
        21 = 3*7            33 to base 6
        27 = 3^3            33 to base 8
        31 = 31             11111 to base 2
        33 = 3*11           33 to base 10
        35 = 5*7            55 to base 6
        39 = 3*13           33 to base 12
        43 = 43             111 to base 6
        45 = 3^2*5              33 to base 14
        51 = 3*17           33 to base 16
        55 = 5*11           55 to base 10
        57 = 3*19           33 to base 18
        63 = 3^2*7              33 to base 20
        65 = 5*13           55 to base 12
        69 = 3*23           33 to base 22
        73 = 73             111 to base 8
        75 = 3*5^2              33 to base 24
        77 = 7*11           77 to base 10
        81 = 3^4            33 to base 26
        85 = 5*17           55 to base 16
        87 = 3*29           33 to base 28
        91 = 7*13           77 to base 12
        93 = 3*31           33 to base 30
        95 = 5*19           55 to base 18
        99 = 3^2*11             33 to base 32
       105 = 3*5*7              33 to base 34
       111 = 3*37           33 to base 36
       115 = 5*23           55 to base 22
       117 = 3^2*13             33 to base 38
       119 = 7*17           77 to base 16
       121 = 11^2           11111 to base 3

first 20 brazilian prime numbers
         7 = 7              111 to base 2
        13 = 13             111 to base 3
        31 = 31             11111 to base 2
        43 = 43             111 to base 6
        73 = 73             111 to base 8
       127 = 127            1111111 to base 2
       157 = 157            111 to base 12
       211 = 211            111 to base 14
       241 = 241            111 to base 15
       307 = 307            111 to base 17
       421 = 421            111 to base 20
       463 = 463            111 to base 21
       601 = 601            111 to base 24
       757 = 757            111 to base 27
      1093 = 1093           1111111 to base 3
      1123 = 1123           111 to base 33
      1483 = 1483           111 to base 38
      1723 = 1723           111 to base 41
      2551 = 2551           111 to base 50
      2801 = 2801           11111 to base 7

count brazilian numbers up to 1053421821 = 1000000000
Count of primes                 53422305
     21657 ms  ( from 30971 ms )

real	0m26,411s -> marking small factors improved 7.8-> 3.8 seconds
user	0m26,239s
sys	0m0,157s

```



## Perl

{{libheader|ntheory}}
{{trans|Perl 6}}

```perl
use strict;
use warnings;
use feature 'say';

use ntheory qw<is_prime>;

sub is_Brazilian {
    my($n) = @_;
    return 1 if $n > 6 && 0 == $n%2;
    LOOP: for (my $base = 2; $base < $n - 1; ++$base) {
        my $digit;
        my $nn = $n;
        while (1) {
            my $x = $nn % $base;
            $digit //= $x;
            next LOOP if $digit != $x;
            $nn = int $nn / $base;
            if ($nn < $base) {
                return 1 if $digit == $nn;
                next LOOP;
            }
        }
    }
}

my $upto = 20;
my $Inf  = 10e10;

my $n = 1; my $s = "First $upto Brazilian numbers:\n";
$s .= do { $n <= $upto ? is_Brazilian($_) && $n++ && "$_ " : last } for 1..$Inf;
say $s;

$n = 1; $s = "\nFirst $upto odd Brazilian numbers:\n";
$s .= do { $n <= $upto ? 0 != $_%2 && is_Brazilian($_) && $n++ && "$_ " : last } for 1..$Inf;
say $s;

$n = 1; $s = "\nFirst $upto prime Brazilian numbers:\n";
$s .= do { $n <= $upto ? !!is_prime($_) && is_Brazilian($_) && $n++ && "$_ " : last } for 1..$Inf;
say $s;
```

{{out}}

```txt
First 20 Brazilian numbers:
7 8 10 12 13 14 15 16 18 20 21 22 24 26 27 28 30 31 32 33

First 20 odd Brazilian numbers:
7 13 15 21 27 31 33 35 39 43 45 51 55 57 63 65 69 73 75 77

First 20 prime Brazilian numbers:
7 13 31 43 73 127 157 211 241 307 421 463 601 757 1093 1123 1483 1723 2551 2801
```



## Perl 6

{{works with|Rakudo|2019.07.1}}


```perl6
multi is-Brazilian (Int $n where $n %% 2 && $n > 6) { True }

multi is-Brazilian (Int $n) {
    LOOP: loop (my int $base = 2; $base < $n - 1; ++$base) {
        my $digit;
        for $n.polymod( $base xx * ) {
            $digit //= $_;
            next LOOP if $digit != $_;
        }
        return True
    }
    False
}

my $upto = 20;

put "First $upto Brazilian numbers:\n", (^Inf).hyper.grep( *.&is-Brazilian )[^$upto];

put "\nFirst $upto odd Brazilian numbers:\n", (^Inf).hyper.map( * * 2 + 1 ).grep( *.&is-Brazilian )[^$upto];

put "\nFirst $upto prime Brazilian numbers:\n", (^Inf).hyper(:8degree).grep( { .is-prime && .&is-Brazilian } )[^$upto];
```

{{out}}

```txt
First 20 Brazilian numbers:
7 8 10 12 13 14 15 16 18 20 21 22 24 26 27 28 30 31 32 33

First 20 odd Brazilian numbers:
7 13 15 21 27 31 33 35 39 43 45 51 55 57 63 65 69 73 75 77

First 20 prime Brazilian numbers:
7 13 31 43 73 127 157 211 241 307 421 463 601 757 1093 1123 1483 1723 2551 2801
```



## Phix

{{trans|C}}

```Phix
function same_digits(integer n, b)
    integer f = remainder(n,b)
    n = floor(n/b)
    while n>0 do
        if remainder(n,b)!=f then return false end if
        n = floor(n/b)
    end while
    return true
end function

function is_brazilian(integer n)
    if n>=7 then
        if remainder(n,2)=0 then return true end if
        for b=2 to n-2 do
            if same_digits(n,b) then return true end if
        end for
    end if
    return false
end function

constant kinds = {" ", " odd ", " prime "}
for i=1 to length(kinds) do
    printf(1,"First 20%sBrazilian numbers:\n", {kinds[i]})
    integer c = 0, n = 7, p = 4
    while true do
        if is_brazilian(n) then
            printf(1,"%d ",n)
            c += 1
            if c==20 then
                printf(1,"\n\n")
                exit
            end if
        end if
        switch i
            case 1: n += 1
            case 2: n += 2
            case 3: p += 1; n = get_prime(p)
        end switch
    end while
end for

integer n = 7, c = 0
atom t0 = time(), t1 = time()+1
while c<100000 do
    if time()>t1 then
        printf(1,"checking %d [count:%d]...\r",{n,c})
        t1 = time()+1
    end if
    c += is_brazilian(n)
    n += 1
end while
printf(1,"The %,dth Brazilian number: %d\n", {c,n-1})
?elapsed(time()-t0)
```

{{out}}
(not very fast, takes about 4 times as long as Go)

```txt

First 20 Brazilian numbers:
7 8 10 12 13 14 15 16 18 20 21 22 24 26 27 28 30 31 32 33

First 20 odd Brazilian numbers:
7 13 15 21 27 31 33 35 39 43 45 51 55 57 63 65 69 73 75 77

First 20 prime Brazilian numbers:
7 13 31 43 73 127 157 211 241 307 421 463 601 757 1093 1123 1483 1723 2551 2801

The 100,000th Brazilian number: 110468
"52.8s"

```



## Python


```python
'''Brazilian numbers'''

from itertools import count, islice


# main :: IO ()
def main():
    '''First 20 members each of:
        OEIS:A125134
        OEIS:A257521
        OEIS:A085104
    '''
    for kxs in ([
            (' ', count(1)),
            (' odd ', count(1, 2)),
            (' prime ', primes())
    ]):
        print(
            'First 20' + kxs[0] + 'Brazilians:\n' +
            showList(take(20)(filter(isBrazil, kxs[1]))) + '\n'
        )


# isBrazil :: Int -> Bool
def isBrazil(n):
    '''True if n is a Brazilian number,
       in the sense of OEIS:A125134.
    '''
    return 7 <= n and (
        0 == n % 2 or any(
            map(monoDigit(n), range(2, n - 1))
        )
    )


# monoDigit :: Int -> Int -> Bool
def monoDigit(n):
    '''True if all the digits of n,
       in the given base, are the same.
    '''
    def go(b, n):
        (q, d) = divmod(n, b)
        return d == until(
            lambda qr: d != qr[1] or 0 == qr[0]
        )(
            lambda qr: divmod(qr[0], b)
        )((q, d))[1]
    return lambda base: go(base, n)


# GENERIC -------------------------------------------------

# primes :: [Int]
def primes():
    ''' Non finite sequence of prime numbers.
    '''
    n = 2
    dct = {}
    while True:
        if n in dct:
            for p in dct[n]:
                dct.setdefault(n + p, []).append(p)
            del dct[n]
        else:
            yield n
            dct[n * n] = [n]
        n = 1 + n


# showList :: [a] -> String
def showList(xs):
    '''Stringification of a list.'''
    return '[' + ','.join(str(x) for x in xs) + ']'


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.
    '''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, (list, tuple))
        else list(islice(xs, n))
    )


# until :: (a -> Bool) -> (a -> a) -> a -> a
def until(p):
    '''The result of repeatedly applying f until p holds.
       The initial seed value is x.
    '''
    def go(f, x):
        v = x
        while not p(v):
            v = f(v)
        return v
    return lambda f: lambda x: go(f, x)


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
First 20 Brazilians:
[7,8,10,12,13,14,15,16,18,20,21,22,24,26,27,28,30,31,32,33]

First 20 odd Brazilians:
[7,13,15,21,27,31,33,35,39,43,45,51,55,57,63,65,69,73,75,77]

First 20 prime Brazilians:
[7,13,31,43,73,127,157,211,241,307,421,463,601,757,1093,1123,1483,1723,2551,2801]
```



## REXX

{{trans|GO}}
<lang>/*REXX pgm finds:  1st N Brazilian #s;  odd Brazilian #s;  prime Brazilian #s;  ZZZth #.*/
parse arg  t.1  t.2  t.3  t.4  .                 /*obtain optional arguments from the CL*/
if t.4=='' | t.4==","  then t.4= 0               /*special test case of Nth Brazilian #.*/
hdr.1= 'first';   hdr.2= "first odd";   hdr.3= 'first prime';   hdr.4=   /*four headers.*/
                                        #p= 0    /*#P:   the number of primes  (so far).*/
    do c=1  for 4                                /*process each of the four cases.      */
    if t.c=='' | t.c==","  then t.c= 20          /*check if a target is null or a comma.*/
    step= 1 + (c==2)                             /*STEP is set to unity or two (for ODD)*/
    if t.c==0  then iterate                      /*check to see if this case target ≡ 0.*/
    $=;                       #= 0               /*initialize list to null; counter to 0*/
       do j=1  by step  until #>= t.c            /*search integers for Brazilian # type.*/
       prime= 0                                  /*signify if  J  may not be prime.     */
       if c==3  then do                          /*is this a  "case 3"  calculation?    */
                     if \isPrime(j) then iterate /*(case 3)  Not a prime?  Then skip it.*/
                     prime= 1                    /*signify if  J  is definately a prime.*/
                     end                         /* [↓] J≡prime will be used for speedup*/
       if \isBraz(j, prime)  then iterate        /*Not  Brazilian number?   "    "    " */
       #= # + 1                                  /*bump the counter of Brazilian numbers*/
       if c\==4  then $= $  j                    /*for most cases, append J to ($) list.*/
       end   /*j*/                               /* [↑] cases 1──►3, $ has leading blank*/
    say                                          /* [↓]  use a special header for cases.*/
    if c==4  then do;  $= j;   t.c=th(t.c);  end /*for Nth Brazilian number, just use J.*/
    say center(' 'hdr.c" "     t.c     " Brazilian number"left('s',  c\==4)" ",  79,  '═')
    say strip($)                                 /*display a case result to the terminal*/
    end      /*c*/                               /* [↑] cases 1──►3 have a leading blank*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isBraz:  procedure; parse arg x,p;  if x<7      then return 0  /*Is # too small?   Nope.*/
                                    if x//2==0  then return 1  /*Is # even  &  ≥7?  Yup.*/
         if p  then mx= isqrt(x)                               /*X prime? Use integer √.*/
               else mx= x%3 - 1                                /*X  not known if prime. */
                                do b=2  for mx                 /*scan for base 2 ──► max*/
                                if sameDig(x,b)  then return 1 /*it's a Brazilian number*/
                                end   /*b*/;          return 0 /*not  "     "        "  */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPrime: procedure expose @. !. #p; parse arg x '' -1 _ /*get 1st arg & last decimal dig*/
         if #p==0 then do; !.=0; y= '2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67'
                         do i=1  for words(y);  #p= #p+1; z=word(y,i); @.#p= z; !.z=1; end
                       end                              /*#P:  is the number of primes. */
         if !.x      then return 1;   if x<61  then return 0;  if x//2==0  then return 0
         if x//3==0  then return 0;   if _==5  then return 0;  if x//7==0  then return 0
            do j=5  until @.j**2>x;                 if x//@.j     ==0  then return 0
                                                    if x//(@.j+2) ==0  then return 0
            end   /*j*/;   #p= #p + 1;   @.#p= x;   !.x= 1;    return 1  /*it's a prime.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
iSqrt: procedure; parse arg x;  r= 0;  q= 1;              do while q<=x;  q= q * 4;  end
        do while q>1; q=q%4; _=x-r-q; r=r%2; if _>=0 then do;x=_;r=r+q; end; end; return r
/*──────────────────────────────────────────────────────────────────────────────────────*/
sameDig: procedure; parse arg x, b;           f= x // b        /* //  ◄── the remainder.*/
                                              x= x  % b        /*  %  ◄── is integer  ÷ */
                    do while x>0;  if x//b \==f  then return 0
                    x= x % b
                    end   /*while*/;      return 1             /*it has all the same dig*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
th: parse arg th; return th || word('th st nd rd', 1+(th//10)*(th//100%10\==1)*(th//10<4))
```

{{out|output|text=  when using the inputs of:     <tt> ,   ,   ,   100000 </tt>}}

```txt

══════════════════════ The first  20  Brazilian numbers ═══════════════════════
7 8 10 12 13 14 15 16 18 20 21 22 24 26 27 28 30 31 32 33

════════════════════ The first odd  20  Brazilian numbers ═════════════════════
7 13 15 21 27 31 33 35 39 43 45 51 55 57 63 65 69 73 75 77

═══════════════════ The first prime  20  Brazilian numbers ════════════════════
7 13 31 43 73 127 157 211 241 307 421 463 601 757 1093 1123 1483 1723 2551 2801

═════════════════════════ The  100000th  Brazilian number ═════════════════════
110468

```



## Sidef


```ruby
func is_Brazilian_prime(q) {

    static L = Set()
    static M = 0

    return true  if L.has(q)
    return false if (q < M)

    var N = (q<1000 ? 1000 : 2*q)

    for K in (primes(3, ilog2(N+1))) {
        for n in (2 .. iroot(N-1, K-1)) {
            var p = (n**K - 1)/(n-1)
            L << p if (p<N && p.is_prime)
        }
    }

    M = (L.max \\ 0)
    return L.has(q)
}

func is_Brazilian(n) {

    if (!n.is_prime) {
        n.is_square || return (n>6)
        var m = n.isqrt
        return (m>3 && (!m.is_prime || m==11))
    }

    is_Brazilian_prime(n)
}


with (20) {|n|
    say "First #{n} Brazilian numbers:"
    say (^Inf -> lazy.grep(is_Brazilian).first(n))

    say "\nFirst #{n} odd Brazilian numbers:"
    say (^Inf -> lazy.grep(is_Brazilian).grep{.is_odd}.first(n))

    say "\nFirst #{n} prime Brazilian numbers"
    say (^Inf -> lazy.grep(is_Brazilian).grep{.is_prime}.first(n))
}
```

{{out}}

```txt

First 20 Brazilian numbers:
[7, 8, 10, 12, 13, 14, 15, 16, 18, 20, 21, 22, 24, 26, 27, 28, 30, 31, 32, 33]

First 20 odd Brazilian numbers:
[7, 13, 15, 21, 27, 31, 33, 35, 39, 43, 45, 51, 55, 57, 63, 65, 69, 73, 75, 77]

First 20 prime Brazilian numbers
[7, 13, 31, 43, 73, 127, 157, 211, 241, 307, 421, 463, 601, 757, 1093, 1123, 1483, 1723, 2551, 2801]

```


Extra:

```ruby
for n in (1..6) {
    say ("#{10**n->commify}th Brazilian number = ", is_Brazilian.nth(10**n))
}
```

{{out}}

```txt

10th Brazilian number = 20
100th Brazilian number = 132
1,000th Brazilian number = 1191
10,000th Brazilian number = 11364
100,000th Brazilian number = 110468
1,000,000th Brazilian number = 1084566

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Function sameDigits(ByVal n As Integer, ByVal b As Integer) As Boolean
        Dim f As Integer = n Mod b : n \= b : While n > 0
            If n Mod b <> f Then Return False Else n \= b
        End While : Return True
    End Function

    Function isBrazilian(ByVal n As Integer) As Boolean
        If n < 7 Then Return False
        If n Mod 2 = 0 Then Return True
        For b As Integer = 2 To n - 2
            If sameDigits(n, b) Then Return True
        Next : Return False
    End Function

    Function isPrime(ByVal n As Integer) As Boolean
        If n < 2 Then Return False
        If n Mod 2 = 0 Then Return n = 2
        If n Mod 3 = 0 Then Return n = 3
        Dim d As Integer = 5
        While d * d <= n
            If n Mod d = 0 Then Return False Else d += 2
            If n Mod d = 0 Then Return False Else d += 4
        End While : Return True
    End Function

    Sub Main(args As String())
        For Each kind As String In {" ", " odd ", " prime "}
            Console.WriteLine("First 20{0}Brazilian numbers:", kind)
            Dim Limit As Integer = 20, n As Integer = 7
            Do
                If isBrazilian(n) Then Console.Write("{0} ", n) : Limit -= 1
                Select Case kind
                    Case " " : n += 1
                    Case " odd " : n += 2
                    Case " prime " : Do : n += 2 : Loop Until isPrime(n)
                End Select
            Loop While Limit > 0
            Console.Write(vbLf & vbLf)
        Next
    End Sub

End Module
```

{{out}}

```txt
First 20 Brazilian numbers:
7 8 10 12 13 14 15 16 18 20 21 22 24 26 27 28 30 31 32 33

First 20 odd Brazilian numbers:
7 13 15 21 27 31 33 35 39 43 45 51 55 57 63 65 69 73 75 77

First 20 prime Brazilian numbers:
7 13 31 43 73 127 157 211 241 307 421 463 601 757 1093 1123 1483 1723 2551 2801

```


### Speedier Version

Based on the C# speedier version, performance is just as good, one billion Brazilian numbers counted in 4 1/2 seconds (on a core i7).

```vbnet
Imports System

Module Module1
    ' flags:
    Const _
        PrMk As Integer = 0,  ' a number that is prime
        SqMk As Integer = 1,  ' a number that is the square of a prime number
        UpMk As Integer = 2,  ' a number that can be factored (aka un-prime)
        BrMk As Integer = -2, ' a prime number that is also a Brazilian number
        Excp As Integer = 121 ' exception square - the only square prime that is a Brazilian

    Dim pow As Integer = 9,
        max As Integer '  maximum sieve array length
    '     An upper limit of the required array length can be calculated Like this:
    ' power of 10  fraction              limit        actual result
    '   1          2 / 1 * 10          = 20           20
    '   2          4 / 3 * 100         = 133          132
    '   3          6 / 5 * 1000        = 1200         1191
    '   4          8 / 7 * 10000       = 11428        11364
    '   5          10/ 9 * 100000      = 111111       110468
    '   6          12/11 * 1000000     = 1090909      1084566
    '   7          14/13 * 10000000    = 10769230     10708453
    '   8          16/15 * 100000000   = 106666666    106091516
    '   9          18/17 * 1000000000  = 1058823529   1053421821
    ' powers above 9 are impractical because of the maximum array length in VB.NET,
    ' which is around the UInt32.MaxValue, Or 4294967295

    Dim PS As SByte() ' the prime/Brazilian number sieve
    ' once the sieve is populated, primes are <= 0, non-primes are > 0,
    ' Brazilian numbers are (< 0) or (> 1)
    ' 121 is a special case, in the sieve it is marked with the BrMk (-2)

    ' typical sieve of Eratosthenes algorithm
    Sub PrimeSieve(ByVal top As Integer)
        PS = New SByte(top) {} : Dim i, ii, j As Integer
        i = 2 : j = 4 : PS(j) = SqMk : While j < top - 2 : j += 2 : PS(j) = UpMk : End While
        i = 3 : j = 9 : PS(j) = SqMk : While j < top - 6 : j += 6 : PS(j) = UpMk : End While
        i = 5 : ii = 25 : While ii < top
            If PS(i) = PrMk Then
                j = (top - i) / i : If (j And 1) = 0 Then j -= 1
                Do : If PS(j) = PrMk Then PS(i * j) = UpMk
                    j -= 2 : Loop While j > i : PS(ii) = SqMk
            End If
            Do : i += 2 : Loop While PS(i) <> PrMk : ii = i * i
        End While
    End Sub

    ' consults the sieve and returns whether a number is Brazilian
    Function IsBr(ByVal number As Integer) As Boolean
        Return Math.Abs(PS(number)) > SqMk
    End Function

    ' shows the first few Brazilian numbers of several kinds
    Sub FirstFew(ByVal kind As String, ByVal amt As Integer)
        Console.WriteLine(vbLf & "The first {0} {1}Brazilian Numbers are:", amt, kind)
        Dim i As Integer = 7 : While amt > 0
            If IsBr(i) Then amt -= 1 : Console.Write("{0} ", i)
            Select Case kind : Case "odd " : i += 2
                Case "prime " : Do : i += 2 : Loop While PS(i) <> BrMk OrElse i = Excp
                Case Else : i += 1 : End Select : End While : Console.WriteLine()
    End Sub

    ' expands a 111_X number into an integer
    Function Expand(ByVal NumberOfOnes As Integer, ByVal Base As Integer) As Integer
        Dim res As Integer = 1
        While NumberOfOnes > 1 AndAlso res < Integer.MaxValue \ Base
            res = res * Base + 1 : NumberOfOnes -= 1 : End While
        If res > max OrElse res < 0 Then res = 0
        Return res
    End Function

    ' returns an elapsed time string
    Function TS(ByVal fmt As String, ByRef st As DateTime, ByVal Optional reset As Boolean = False) As String
        Dim n As DateTime = DateTime.Now,
            res As String = String.Format(fmt, (n - st).TotalMilliseconds)
        If reset Then st = n
        Return res
    End Function

    Sub Main(args As String())
        Dim p2 As Integer = pow << 1, primes(6) As Integer, n As Integer,
            st As DateTime = DateTime.Now, st0 As DateTime = st,
            p10 As Integer = CInt(Math.Pow(10, pow)), p As Integer = 10, cnt As Integer = 0
        max = CInt(((CLng((p10)) * p2) / (p2 - 1))) : PrimeSieve(max)
        Console.WriteLine(TS("Sieving took {0} ms", st, True))
        ' make short list of primes before Brazilians are added
        n = 3 : For i As Integer = 0 To primes.Length - 1
            primes(i) = n : Do : n += 2 : Loop While PS(n) <> 0 : Next
        Console.WriteLine(vbLf & "Checking first few prime numbers of sequential ones:" &
                          vbLf & "ones checked found")
        ' now check the '111_X' style numbers. many are factorable, but some are prime,
        ' then re-mark the primes found in the sieve as Brazilian.
        ' curiously, only the numbers with a prime number of ones will turn out, so
        ' restricting the search to those saves time. no need to wast time on even numbers of ones,
        ' or 9 ones, 15 ones, etc...
        For Each i As Integer In primes
            Console.Write("{0,4}", i) : cnt = 0 : n = 2 : Do
                If (n - 1) Mod i <> 0 Then
                    Dim br As Long = Expand(i, n)
                    If br > 0 Then
                        If PS(br) < UpMk Then PS(br) = BrMk : cnt += 1
                    Else
                        Console.WriteLine("{0,8}{1,6}", n, cnt) : Exit Do
                    End If
                End If : n += 1 : Loop While True
        Next
        Console.WriteLine(TS("Adding Brazilian primes to the sieve took {0} ms", st, True))
        For Each s As String In ",odd ,prime ".Split(",") : FirstFew(s, 20) : Next
        Console.WriteLine(TS(vbLf & "Required output took {0} ms", st, True))
        Console.WriteLine(vbLf & "Decade count of Brazilian numbers:")
        n = 6 : cnt = 0 : Do : While cnt < p : n += 1 : If IsBr(n) Then cnt += 1
            End While
            Console.WriteLine("{0,15:n0}th is {1,-15:n0}  {2}", cnt, n, TS("time: {0} ms", st))
            If p < p10 Then p *= 10 Else Exit Do
        Loop While (True) : PS = New SByte(-1) {}
        Console.WriteLine(vbLf & "Total elapsed was {0} ms", (DateTime.Now - st0).TotalMilliseconds)
        If System.Diagnostics.Debugger.IsAttached Then Console.ReadKey()
    End Sub

End Module

```

{{out}}

```txt
Sieving took 2967.834 ms

Checking first few prime numbers of sequential ones:
ones checked found
   3   32540  3923
   5     182    44
   7      32     9
  11       8     1
  13       8     3
  17       4     1
  19       4     1
Adding Brazilian primes to the sieve took 8.6242 ms

The first 20 Brazilian Numbers are:
7 8 10 12 13 14 15 16 18 20 21 22 24 26 27 28 30 31 32 33

The first 20 odd Brazilian Numbers are:
7 13 15 21 27 31 33 35 39 43 45 51 55 57 63 65 69 73 75 77

The first 20 prime Brazilian Numbers are:
7 13 31 43 73 127 157 211 241 307 421 463 601 757 1093 1123 1483 1723 2551 2801

Required output took 2.8256 ms

Decade count of Brazilian numbers:
             10th is 20               time: 0.0625 ms
            100th is 132              time: 0.1156 ms
          1,000th is 1,191            time: 0.1499 ms
         10,000th is 11,364           time: 0.1986 ms
        100,000th is 110,468          time: 0.4081 ms
      1,000,000th is 1,084,566        time: 1.9035 ms
     10,000,000th is 10,708,453       time: 15.9129 ms
    100,000,000th is 106,091,516      time: 149.8814 ms
  1,000,000,000th is 1,053,421,821    time: 1412.3526 ms

Total elapsed was 4391.7287 ms

```

The point of utilizing a sieve is that it caches or memoizes the results.  Since we are going through a long sequence of possible Brazilian numbers, it pays off to check the prime factoring in an efficient way, rather than one at a time.


## zkl


```zkl
fcn isBrazilian(n){
   foreach b in ([2..n-2]){
      f,m := n%b, n/b;
      while(m){
	 if((m % b)!=f) continue(2);
	 m/=b;
      }
      return(True);
   }
   False
}
fcn isBrazilianW(n){ isBrazilian(n) and n or Void.Skip }
```


```zkl
println("First 20 Brazilian numbers:");
[1..].tweak(isBrazilianW).walk(20).println();

println("\nFirst 20 odd Brazilian numbers:");
[1..*,2].tweak(isBrazilianW).walk(20).println();
```

{{out}}

```txt

First 20 Brazilian numbers:
L(7,8,10,12,13,14,15,16,18,20,21,22,24,26,27,28,30,31,32,33)

First 20 odd Brazilian numbers:
L(7,13,15,21,27,31,33,35,39,43,45,51,55,57,63,65,69,73,75,77)

```

{{libheader|GMP}} GNU Multiple Precision Arithmetic Library
Using GMP ( probabilistic primes),
because it is easy and fast to generate primes.

[[Extensible prime generator#zkl]] could be used instead.

```zkl
var [const] BI=Import("zklBigNum");  // libGMP

println("\nFirst 20 prime Brazilian numbers:");
p:=BI(1);
Walker.zero().tweak('wrap{ p.nextPrime().toInt() })
.tweak(isBrazilianW).walk(20).println();
```

{{out}}

```txt

First 20 prime Brazilian numbers:
L(7,13,31,43,73,127,157,211,241,307,421,463,601,757,1093,1123,1483,1723,2551,2801)

```


```zkl
println("The 100,00th Brazilian number: ",
   [1..].tweak(isBrazilianW).drop(100_000).value);
```

{{out}}

```txt

The 100,00th Brazilian number: 110468

```

