+++
title = "Negative base numbers"
description = ""
date = 2019-05-10T18:36:05Z
aliases = []
[extra]
id = 21222
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "algol_68",
  "c",
  "cpp",
  "csharp",
  "d",
  "go",
  "haskell",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "oorexx",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rexx",
  "ring",
  "scala",
  "seed7",
  "sidef",
  "vba",
  "zkl",
]
+++

Negative base numbers are an alternate way to encode numbers without the need for a minus sign. Various negative bases may be used including negadecimal (base -10), negabinary (-2) and negaternary (-3).<ref>[http://mathworld.wolfram.com/Negabinary.html Negabinary on Wolfram Mathworld]</ref><ref>[https://en.wikipedia.org/wiki/Negative_base Negative base] on Wikipedia</ref>


## Task

*Encode the decimal number 10 as negabinary (expect 11110)
*Encode the decimal number 146 as negaternary (expect 21102)
*Encode the decimal number 15 as negadecimal (expect 195)
*In each of the above cases, convert the encoded number back to decimal.


;extra credit:

* supply an integer, that when encoded to base   -62   (or something "higher"),   expresses the
name of the language being used   (with correct capitalization).   If the computer language has
non-alphanumeric characters,   try to encode them into the negatory numerals,   or use other
characters instead.





## ALGOL 68

```algol68
# Conversion to/from negative base numbers                                    #
# Note - no checks for valid bases or digits bases -2 .. -63 are handled      #
#        A-Z represent the digits 11 .. 35, a-z represent the digits 36 .. 61 #
#        the digit 62 is represented by a space                               #

#                               1         2         3         4         5         6          #
#                               01234567890123456789012345678901234567890123456789012        #
[]CHAR base digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz "[ AT 0 ];

# returns s decoded to an integer from the negative base b                   #
PRIO FROMNBASE = 9;
OP   FROMNBASE = ( STRING s, INT b )LONG INT:
     BEGIN
        LONG INT    result          := 0;
        LONG INT    base multiplier := 1;
        FOR d pos FROM UPB s BY -1 TO LWB s DO
            INT digit = IF   s[ d pos ] = " "
                        THEN 62
                        ELIF s[ d pos ] >= "a"
                        THEN ( ABS s[ d pos ] + 36 ) - ABS "a"
                        ELIF s[ d pos ] >= "A"
                        THEN ( ABS s[ d pos ] + 10 ) - ABS "A"
                        ELSE ABS s[ d pos ] - ABS "0"
                        FI;
            result          +:= base multiplier * digit;
            base multiplier *:= b
        OD;
        result
     END # FROMNBASE # ;
OP   FROMNBASE = ( CHAR c, INT b )LONG INT: STRING(c) FROMNBASE b;

# returns n encoded as a string in the negative base b #
PRIO TONBASE = 9;
OP   TONBASE = ( LONG INT n, INT b )STRING:
     BEGIN
        STRING   result := "";
        LONG INT v      := n;
        WHILE
            INT d := SHORTEN IF v < 0 THEN - ( ABS v MOD b ) ELSE v MOD b FI;
            v OVERAB b;
            IF d < 0
            THEN
                d -:= b;
                v +:= 1
            FI;
            base digits[ d ] +=: result;
            v /= 0
        DO SKIP OD;
        result
     END # TONBASE # ;

# tests the TONBASE and FROMNBASE operators #
PROC test n base = ( LONG INT number, INT base, STRING expected )VOID:
     BEGIN
        PROC expect = ( BOOL result )STRING: IF result THEN "" ELSE " INCORRECT" FI;
        STRING   encoded = number  TONBASE   base;
        LONG INT decoded = encoded FROMNBASE base;
        print( ( whole( number, 0 ), " encodes to: ", encoded ) );
        print( ( " base ", whole( base, 0 ), expect( encoded = expected ), newline ) );
        print( ( encoded, " decodes to: ", whole( decoded, 0 ) ) );
        print( ( " base ", whole( base, 0 ), expect( decoded = number   ), newline ) )
     END # test n base # ;

test n base(                    10,  -2,    "11110" );
test n base(                   146,  -3,    "21102" );
test n base(                    15, -10,      "195" );
# The defining document for ALGOL 68 spells the name "Algol 68" on the cover, though inside it is "ALGOL 68" #
# at the risk of "judging a language by it's cover", we use "Algol 68" as the name here...                   #
test n base( - LONG 36492107981104, -63, "Algol 68" )
```

```txt

10 encodes to: 11110 base -2
11110 decodes to: 10 base -2
146 encodes to: 21102 base -3
21102 decodes to: 146 base -3
15 encodes to: 195 base -10
195 decodes to: 15 base -10
-36492107981104 encodes to: Algol 68 base -63
Algol 68 decodes to: -36492107981104 base -63

```



## C

```c
#include <stdio.h>

const char DIGITS[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
const int DIGITS_LEN = 64;

void encodeNegativeBase(long n, long base, char *out) {
    char *ptr = out;

    if (base > -1 || base < -62) {
        /* Bounds check*/
        out = "";
        return;
    }
    if (n == 0) {
        /* Trivial case */
        out = "0";
        return;
    }

    /* Convert the number into a string (in reverse) */
    while (n != 0) {
        long rem = n % base;
        n = n / base;
        if (rem < 0) {
            n++;
            rem = rem - base;
        }
        *ptr = DIGITS[rem];
        ptr++;
    }
    *ptr = 0;

    /* Reverse the current string to get the final result */
    ptr--;
    while (out < ptr) {
        char t = *out;
        *out = *ptr;
        *ptr = t;
        out++;
        ptr--;
    }
    return;
}

long decodeNegativeBase(const char* ns, long base) {
    long value, bb;
    int i;
    const char *ptr;

    if (base < -62 || base > -1) {
        /* Bounds check */
        return 0;
    }
    if (ns[0] == 0 || (ns[0] == '0' && ns[1] == 0)) {
        /* Trivial case */
        return 0;
    }

    /* Find the end of the string */
    ptr = ns;
    while (*ptr != 0) {
        ptr++;
    }

    /* Convert */
    value = 0;
    bb = 1;
    ptr--;
    while (ptr >= ns) {
        for (i = 0; i < DIGITS_LEN; i++) {
            if (*ptr == DIGITS[i]) {
                value = value + i * bb;
                bb = bb * base;
                break;
            }
        }
        ptr--;
    }

    return value;
}

void driver(long n, long b) {
    char buf[64];
    long value;

    encodeNegativeBase(n, b, buf);
    printf("%12d encoded in base %3d = %12s\n", n, b, buf);

    value = decodeNegativeBase(buf, b);
    printf("%12s decoded in base %3d = %12d\n", buf, b, value);

    printf("\n");
}

int main() {
    driver(10, -2);
    driver(146, -3);
    driver(15, -10);
    driver(12, -62);

    return 0;
}
```

```txt
          10 encoded in base  -2 =        11110
       11110 decoded in base  -2 =           10

         146 encoded in base  -3 =        21102
       21102 decoded in base  -3 =          146

          15 encoded in base -10 =          195
         195 decoded in base -10 =           15

          12 encoded in base -62 =            C
           C decoded in base -62 =           12
```



## C++

```cpp
#include <iomanip>
#include <iostream>
#include <tuple>
#include <vector>

const std::string DIGITS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

std::string encodeNegativeBase(int64_t n, int b) {
    if (b < -62 || b > -1) {
        throw std::runtime_error("Argument out of range: b");
    }
    if (n == 0) {
        return "0";
    }

    std::string output;
    int64_t nn = n;
    while (nn != 0) {
        int rem = nn % b;
        nn /= b;
        if (rem < 0) {
            nn++;
            rem -= b;
        }
        output += DIGITS[rem];
    }

    std::reverse(output.begin(), output.end());
    return output;
}

int64_t decodeNegativeBase(const std::string& ns, int b) {
    if (b < -62 || b > -1) {
        throw std::runtime_error("Argument out of range: b");
    }
    if (ns == "0") {
        return 0;
    }

    int64_t total = 0;
    int64_t bb = 1;

    for (auto it = ns.crbegin(); it != ns.crend(); it = std::next(it)) {
        auto ptr = std::find(DIGITS.cbegin(), DIGITS.cend(), *it);
        if (ptr != DIGITS.cend()) {
            auto idx = ptr - DIGITS.cbegin();
            total += idx * bb;
        }
        bb *= b;
    }
    return total;
}

int main() {
    using namespace std;

    vector<pair<int64_t, int>> nbl({
        make_pair(10, -2),
        make_pair(146, -3),
        make_pair(15, -10),
        make_pair(142961, -62)
        });

    for (auto& p : nbl) {
        string ns = encodeNegativeBase(p.first, p.second);
        cout << setw(12) << p.first << " encoded in base " << setw(3) << p.second << " = " << ns.c_str() << endl;

        int64_t n = decodeNegativeBase(ns, p.second);
        cout << setw(12) << ns.c_str() << " decoded in base " << setw(3) << p.second << " = " << n << endl;

        cout << endl;
    }

    return 0;
}
```

```txt
          10 encoded in base  -2 = 11110
       11110 decoded in base  -2 = 10

         146 encoded in base  -3 = 21102
       21102 decoded in base  -3 = 146

          15 encoded in base -10 = 195
         195 decoded in base -10 = 15

      142961 encoded in base -62 = cpp
         cpp decoded in base -62 = 142961
```


## C#
```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace NegativeBaseNumbers {
    class Program {
        const string DIGITS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

        static string EncodeNegativeBase(long n, int b) {
            if (b < -62 || b > -1) {
                throw new ArgumentOutOfRangeException("b");
            }
            if (n == 0) {
                return "0";
            }
            StringBuilder output = new StringBuilder();
            long nn = n;
            while (nn != 0) {
                int rem = (int)(nn % b);
                nn /= b;
                if (rem < 0) {
                    nn++;
                    rem -= b;
                }
                output.Append(DIGITS[rem]);
            }
            return new string(output.ToString().Reverse().ToArray());
        }

        static long DecodeNegativeBase(string ns, int b) {
            if (b < -62 || b > -1) {
                throw new ArgumentOutOfRangeException("b");
            }
            if (ns == "0") {
                return 0;
            }
            long total = 0;
            long bb = 1;
            for (int i = ns.Length - 1; i >= 0; i--) {
                char c = ns[i];
                total += DIGITS.IndexOf(c) * bb;
                bb *= b;
            }
            return total;
        }

        static void Main(string[] args) {
            List<Tuple<long, int>> nbl = new List<Tuple<long, int>>() {
                new Tuple<long, int>(10,-2),
                new Tuple<long, int>(146,-3),
                new Tuple<long, int>(15,-10),
                new Tuple<long, int>(-34025238427,-62),
            };
            foreach (var p in nbl) {
                string ns = EncodeNegativeBase(p.Item1, p.Item2);
                Console.WriteLine("{0,12} encoded in base {1,-3} = {2}", p.Item1, p.Item2, ns);
                long n = DecodeNegativeBase(ns, p.Item2);
                Console.WriteLine("{0,12} decoded in base {1,-3} = {2}", ns, p.Item2, n);
                Console.WriteLine();
            }
        }
    }
}
```

```txt
          10 encoded in base -2  = 11110
       11110 decoded in base -2  = 10

         146 encoded in base -3  = 21102
       21102 decoded in base -3  = 146

          15 encoded in base -10 = 195
         195 decoded in base -10 = 15

-34025238427 encoded in base -62 = csharp
      csharp decoded in base -62 = -34025238427
```



## D


```D
import std.stdio;

immutable DIGITS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

void main() {
    driver(10, -2);
    driver(146, -3);
    driver(15, -10);
    driver(13, -62);
}

void driver(long n, int b) {
    string ns = encodeNegBase(n, b);
    writefln("%12d encoded in base %3d = %12s", n, b, ns);

    long p = decodeNegBase(ns, b);
    writefln("%12s decoded in base %3d = %12d", ns, b, p);

    writeln;
}

string encodeNegBase(long n, int b) in {
    import std.exception : enforce;
    enforce(b <= -1 && b >= -62);
} body {
    if (n==0) return "0";
    char[] output;
    long nn = n;
    while (nn != 0) {
        int rem = nn % b;
        nn /= b;
        if (rem < 0) {
            nn++;
            rem -= b;
        }
        output ~= DIGITS[rem];
    }

    import std.algorithm : reverse;

    reverse(output);
    return cast(string) output;
}

long decodeNegBase(string ns, int b) in {
    import std.exception : enforce;
    enforce(b <= -1 && b >= -62);
} body {
    if (ns == "0") return 0;
    long total = 0;
    long bb = 1;
    foreach_reverse (c; ns) {
        foreach(i,d; DIGITS) {
            if (c==d) {
                total += i * bb;
                bb *= b;
                break;
            }
        }
    }
    return total;
}
```


```txt
          10 encoded in base  -2 =        11110
       11110 decoded in base  -2 =           10

         146 encoded in base  -3 =        21102
       21102 decoded in base  -3 =          146

          15 encoded in base -10 =          195
         195 decoded in base -10 =           15

          13 encoded in base -62 =            D
           D decoded in base -62 =           13
```


=={{header|F_Sharp|F#}}==

### The Functions


```fsharp

//I provide 2 fuctions D2N takes a radix and an integer returning a sequence of integers
//                     N2D takse a radix and a sequence of integers returning an integer
//Note that the radix may be either positive or negative.  Nigel Galloway, May 10th., 2019
let D2N n g=if g=0 then seq[0] else Seq.unfold(fun g->let α,β=g/n,g%n in match (compare g 0,β) with
                                                                          (0,_)->None
                                                                         |(1,_) |(_,0)->Some(β,α)
                                                                         |_->Some(g-(α+1)*n,α+1)) g|>Seq.rev
let N2D n g=fst(Seq.foldBack(fun g (Σ,α)->(Σ+α*g,n*α)) g (0,1))

```


### The Task


```fsharp

let t0,t146,t10,t15=D2N -13 0,D2N -3 146,D2N -2 10,D2N -10 15
Seq.iter(fun n->Seq.iter(printf "%d ")n; printfn "")[t0;t146;t10;t15]
Seq.iter(printfn "%d ")[N2D -13 t0;N2D -3 t146;N2D -2 t10;N2D -10 t15]

```

```txt

0
2 1 1 0 2
1 1 1 1 0
1 9 5
0
146
10
15

```


## Go

```go
package main

import (
    "fmt"
    "log"
    "strings"
)

const digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

func reverse(bs []byte) []byte {
    for i, j := 0, len(bs)-1; i < len(bs)/2; i, j = i+1, j-1 {
        bs[i], bs[j] = bs[j], bs[i]
    }
    return bs
}

func encodeNegBase(n, b int64) (string, error) {
    if b < -62 || b > -1 {
        return "", fmt.Errorf("base must be between -62 and -1 inclusive")
    }
    if n == 0 {
        return "0", nil
    }
    var out []byte
    for n != 0 {
        rem := n % b
        n /= b
        if rem < 0 {
            n++
            rem -= b
        }
        out = append(out, digits[int(rem)])
    }
    reverse(out)
    return string(out), nil
}

func decodeNegBase(ns string, b int64) (int64, error) {
    if b < -62 || b > -1 {
        return 0, fmt.Errorf("base must be between -62 and -1 inclusive")
    }
    if ns == "0" {
        return 0, nil
    }
    total := int64(0)
    bb := int64(1)
    bs := []byte(ns)
    reverse(bs)
    for _, c := range bs {
        total += int64(strings.IndexByte(digits, c)) * bb
        bb *= b
    }
    return total, nil
}

func main() {
    numbers := []int64{10, 146, 15, -942, 1488588316238}
    bases := []int64{-2, -3, -10, -62, -62}
    for i := 0; i < len(numbers); i++ {
        n := numbers[i]
        b := bases[i]
        ns, err := encodeNegBase(n, b)
        if err != nil {
            log.Fatal(err)
        }
        fmt.Printf("%13d encoded in base %-3d = %s\n", n, b, ns)
        n, err = decodeNegBase(ns, b)
        if err != nil {
            log.Fatal(err)
        }
        fmt.Printf("%13s decoded in base %-3d = %d\n\n", ns, b, n)
    }
}
```


```txt

           10 encoded in base -2  = 11110
        11110 decoded in base -2  = 10

          146 encoded in base -3  = 21102
        21102 decoded in base -3  = 146

           15 encoded in base -10 = 195
          195 decoded in base -10 = 15

         -942 encoded in base -62 = Go
           Go decoded in base -62 = -942

1488588316238 encoded in base -62 = Rosetta
      Rosetta decoded in base -62 = 1488588316238

```



## Haskell


```haskell
import Data.Char (chr, ord)
import Numeric (showIntAtBase)

-- The remainder and quotient of n/d, where the remainder is guaranteed to be
-- non-negative.  The divisor, d, is assumed to be negative.
quotRemP :: Integral a => a -> a -> (a, a)
quotRemP n d = let (q, r) = quotRem n d
               in if r < 0 then (q+1, r-d) else (q, r)

-- Convert the number n to base b, where b is assumed to be less than zero.
toNegBase :: Integral a => a -> a -> a
toNegBase b n = let (q, r) = quotRemP n b
                in if q == 0 then r else negate b * toNegBase b q + r

-- Convert n to a string, where n is assumed to be a base b number, with b less
-- than zero.
showAtBase :: (Integral a, Show a) => a -> a -> String
showAtBase b n = showIntAtBase (abs b) charOf n ""
  where charOf m | m < 10    = chr $ m + ord '0'
                 | m < 36    = chr $ m + ord 'a' - 10
                 | otherwise = '?'

-- Print a number in base b, where b is assumed to be less than zero.
printAtBase :: (Integral a, Show a) => a -> a -> IO ()
printAtBase b = putStrLn . showAtBase b . toNegBase b

main :: IO ()
main = do
  printAtBase (-2)  10
  printAtBase (-3)  146
  printAtBase (-10) 15
  printAtBase (-16) 107
  printAtBase (-36) 41371458
```


```txt

$ ./negbase
11110
21102
195
1ab
perl6

```



## J

The j languages has builtin base and antibase verbs defined over the full complex realm.  However, antibase produces negative digits.  Additional coding required for antibase.  Negative base works as is once the literal is converted back to a base 10 vector.


Model: python version.
Python and j divmod for negative divisor agree but for transposition.


```txt

>>> # python divmod
>>> [divmod(i, -2) for i in (2, 3, 4, 5)]
[(-1, 0), (-2, -1), (-2, 0), (-3, -1)]
>>>

```



```txt

   NB. j divmod
   divmod =: <.@:%`(|~)`:0
   2 3 4 5 divmod _2
_1 _2 _2 _3
 0 _1  0 _1

```





```txt

NB. Use:  BASE encode_neg_base INTEGER
NB. Output: literal
neg_antibase =: dyad define
 b =. x
 n =. y
 if. 0 = n do. '0' return. end.
 out =. i.0
 while. n do.
  'n rem' =. n divmod b
  if. rem < 0 do.
   n =. >: n
   rem =. rem - b
  end.
  out =. out , rem
 end.
 (|. out) { _10 |. AlphaNum_j_
)

NB. use:  BASE neg_base LITERAL
NB. output: integer
neg_base =: #. (_10 |. AlphaNum_j_)&i.

   NB. neg_antibase test passes
   _2 _3 _10 neg_antibase&> 10 146 15
11110
21102
195

   NB. in j we would write the tautology
   (":&.>11110 21102 195) -: _2 _3 _10 neg_antibase&.> 10 146 15
1


   NB. expressive numeric notation
   NB. we can write the numbers in arbitrary base
   NB. The digit characters are limited to [0-9a-z]

   _2b11110   _3b21102   _10b195
10 146 15


   NB. neg_base test passes

   _2 neg_base '11110'
10

   _3 neg_base '21102'
146

   _10 neg_base '195'
15


   [ EXTRA_CREDIT =: _63 neg_base&> ;: 'J j apl APL Iverson'
19 45 139718 38136 1069471233985

   _63 neg_antibase&> EXTRA_CREDIT
J
j
apl
APL
Iverson

```



## Java

```Java
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class NegativeBaseNumbers {
    private static final String DIGITS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

    private static String encodeNegBase(long n, int b) {
        if (b < -62 || b > -1) throw new IllegalArgumentException("Parameter b is out of bounds");
        if (n == 0) return "0";
        StringBuilder out = new StringBuilder();
        long nn = n;
        while (nn != 0) {
            int rem = (int) (nn % b);
            nn /= b;
            if (rem < 0) {
                nn++;
                rem -= b;
            }
            out.append(DIGITS.charAt(rem));
        }
        out.reverse();
        return out.toString();
    }

    private static long decodeNegBase(String ns, int b) {
        if (b < -62 || b > -1) throw new IllegalArgumentException("Parameter b is out of bounds");
        if (Objects.equals(ns, "0")) return 0;
        long total = 0;
        long bb = 1;
        for (int i = ns.length() - 1; i >= 0; i--) {
            char c = ns.charAt(i);
            total += DIGITS.indexOf(c) * bb;
            bb *= b;
        }
        return total;
    }

    public static void main(String[] args) {
        List<Map.Entry<Long, Integer>> nbl = List.of(
                Map.entry(10L, -2),
                Map.entry(146L, -3),
                Map.entry(15L, -10),
                Map.entry(-4393346L, -62)
        );
        for (Map.Entry<Long, Integer> p : nbl) {
            String ns = encodeNegBase(p.getKey(), p.getValue());
            System.out.printf("%12d encoded in base %-3d = %s\n", p.getKey(), p.getValue(), ns);
            long n = decodeNegBase(ns, p.getValue());
            System.out.printf("%12s decoded in base %-3d = %d\n\n", ns, p.getValue(), n);
        }
    }
}
```

```txt
          10 encoded in base -2  = 11110
       11110 decoded in base -2  = 10

         146 encoded in base -3  = 21102
       21102 decoded in base -3  = 146

          15 encoded in base -10 = 195
         195 decoded in base -10 = 15

    -4393346 encoded in base -62 = Java
        Java decoded in base -62 = -4393346
```



## jq

If your jq does not have `trunc/0` then use this:

```jq
def trunc: if .>
= 0 then floor else -(-(.)|trunc) end;
```


```jq
def negbase($b):
  if ($b >= 0) then error("negbase requires negative base")
  elif . == 0 then "0"
  else {n: ., ans: ""}
  | until(.n == 0;
         .r = ((.n % $b))
  	 | .n = ((.n / $b) | trunc)
         | if .r < 0 then .n += 1 | .r -= $b else . end
         | .ans = (.r|tostring) + .ans )
  | .ans
  end ;

def sigma(stream): reduce stream as $x (null; .+$x);

def invnegbase($b):
  (explode | reverse | map(. - 48)) as $s
  | sigma( range(0; $s|length) | ($s[.] * pow($b; .)));

def testset: {
   "11110": [10, -2],
   "21102": [146, -3],
   "195":   [15, -10]
 };

def test: testset
| to_entries[]
| .value[0] as $n
| .value[1] as $base
| ($n|negbase($base)) as $neg
| ($neg | invnegbase($base)) as $invneg
| [.key == $neg, $n == $invneg]
;

test
```

```jq
[true,true]
[true,true]
[true,true]
```



## Julia

```julia

function negbase(n, b)
    if n == 0 return "0" end
    out = IOBuffer()
    while n != 0
        n, r = divrem(n, b)
        if r < 0
            n += 1
            r -= b
        end
        print(out, r)
    end
    return reverse(String(out))
end

invnegbase(nst, b) = sum((ch - '0') * b ^ (i - 1) for (i, ch) in enumerate(reverse(nst)))

testset = Dict(
    (10, -2) => "11110",
    (143, -3) => "21102",
    (15, -10) => "195")

for ((num, base), rst) in testset
    encoded = negbase(num, base)
    decoded = invnegbase(encoded, base)
    println("\nencode $num in base $base:\n-> expected: $rst\n-> resulted: $encoded\n-> decoded: $decoded")
end
```


```txt
encode 10 in base -2:
-> expected: 11110
-> resulted: 11110
-> decoded: 10

encode 15 in base -10:
-> expected: 195
-> resulted: 195
-> decoded: 15

encode 143 in base -3:
-> expected: 21102
-> resulted: 21112
-> decoded: 143
```



## Kotlin


```scala
// version 1.1.2

const val DIGITS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

fun encodeNegBase(n: Long, b: Int): String {
    require(b in -62 .. -1)
    if (n == 0L) return "0"
    val out = mutableListOf<Char>()
    var nn = n
    while (nn != 0L) {
        var rem = (nn % b).toInt()
        nn /= b
        if (rem < 0) {
            nn++
            rem -= b
        }
        out.add(DIGITS[rem])
    }
    out.reverse()
    return out.joinToString("")
}

fun decodeNegBase(ns: String, b: Int): Long {
    require(b in -62 .. -1)
    if (ns == "0") return 0
    var total = 0L
    var bb = 1L
    for (c in ns.reversed()) {
        total += DIGITS.indexOf(c) * bb
        bb *= b
    }
    return total
}

fun main(args:Array<String>) {
    val nbl = listOf(10L to -2, 146L to -3, 15L to -10, -17596769891 to -62)
    for (p in nbl) {
        val ns = encodeNegBase(p.first, p.second)
        System.out.printf("%12d encoded in base %-3d = %s\n", p.first, p.second, ns)
        val n  = decodeNegBase(ns, p.second)
        System.out.printf("%12s decoded in base %-3d = %d\n\n", ns, p.second, n)
    }
}
```


```txt

          10 encoded in base -2  = 11110
       11110 decoded in base -2  = 10

         146 encoded in base -3  = 21102
       21102 decoded in base -3  = 146

          15 encoded in base -10 = 195
         195 decoded in base -10 = 15

-17596769891 encoded in base -62 = Kotlin
      Kotlin decoded in base -62 = -17596769891

```


=={{Header|Mathematica}}==

```Mathematica
EncodeBase[number_,base_]:=Module[{
  out={},
  rem,n=number,b=base
 },
 While[
  n!=0,
  {n,rem}={Floor[Divide[n,b]],Mod[n,b]};
  If[
   rem<0,
   n+=1;
   rem-=b
  ];
  PrependTo[out,rem]
 ];
 out
];
DecodeBase[list_,base_]:=Total[list*base^Range[Length[list]-1,0,-1]];

Print[EncodeNegBase[10,-2],"=",DecodeBase[EncodeNegBase[10,-2],-2]];
Print[EncodeNegBase[146,-3],"=",DecodeBase[EncodeNegBase[146,-3],-3]];
Print[EncodeNegBase[15,-10],"=",DecodeBase[EncodeNegBase[15,-10],-10]];
```


```txt
{1,1,1,1,0}=10
{2,1,1,0,2}=146
{1,9,5}=15
```

Extra Credit:

```Mathematica
DecodeBase[ToCharacterCode[$Version], -126](*ascii 1-byte encoding*)
DecodeBase[ToCharacterCode[$Version], -(2^16 - 1)](*2-byte encoding*)
```


```txt
805433247971592164648901981307140864173502418954511864100981464890629926293823767730118860531000284192172723837
1402171866107096793294662824351970913227448502019658754262020315290937172496459102043149157175108006798078612200544768242812932737034448604720533372980987964929785521161173764118702296122915325508945150191589743829011670147956776269027485433441427722106
```


=={{header|Modula-2}}==

```modula2
MODULE NegativeBase;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

CONST DIGITS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
TYPE String = ARRAY[0..63] OF CHAR;

PROCEDURE EncodeNegativeBase(n : LONGINT; base : LONGINT) : String;
    PROCEDURE Mod(a,b : LONGINT) : LONGINT;
    BEGIN
        RETURN a - (a / b) * b
    END Mod;

    PROCEDURE Swap(VAR a,b : CHAR);
    VAR t : CHAR;
    BEGIN
        t := a;
        a := b;
        b := t
    END Swap;
VAR
    ptr,idx : CARDINAL;
    rem : LONGINT;
    result : String;
BEGIN
    IF (base > -1) OR (base < -62) THEN
        RETURN result
    ELSIF n = 0 THEN
        result := "0"
    ELSE
        ptr := 0;
        WHILE n # 0 DO
            rem := Mod(n, base);
            n := n / base;
            IF rem < 0 THEN
                INC(n);
                rem := rem - base
            END;
            result[ptr] := DIGITS[rem];
            INC(ptr);
        END
    END;
    result[ptr] := 0C;

    idx := 0;
    DEC(ptr);
    WHILE idx < ptr DO
        Swap(result[idx], result[ptr]);
        INC(idx);
        DEC(ptr)
    END;

    RETURN result
END EncodeNegativeBase;

PROCEDURE DecodeNegativeBase(ns : String; base : LONGINT) : LONGINT;
VAR
    total,bb,i,j : LONGINT;
BEGIN
    IF (base < -62) OR (base > -1) THEN
        RETURN 0
    ELSIF (ns[0] = 0C) OR ((ns[0] = '0') AND (ns[1] = 0C)) THEN
        RETURN 0
    ELSE
        FOR i:=0 TO HIGH(ns) DO
            IF ns[i] = 0C THEN
                BREAK
            END
        END;
        DEC(i);

        total := 0;
        bb := 1;
        WHILE i >= 0 DO
            FOR j:=0 TO HIGH(DIGITS) DO
                IF ns[i] = DIGITS[j] THEN
                    total := total + j * bb;
                    bb := bb * base;
                    BREAK
                END
            END;
            DEC(i)
        END;
    END;
    RETURN total
END DecodeNegativeBase;

PROCEDURE Driver(n,b : LONGINT);
VAR
    ns,buf : String;
    p : LONGINT;
BEGIN
    ns := EncodeNegativeBase(n, b);
    FormatString("%12l encoded in base %3l = %12s\n", buf, n, b, ns);
    WriteString(buf);

    p := DecodeNegativeBase(ns, b);
    FormatString("%12s decoded in base %3l = %12l\n", buf, ns, b, p);
    WriteString(buf);

    WriteLn
END Driver;

BEGIN
    Driver(10, -2);
    Driver(146, -3);
    Driver(15, -10);
    Driver(-19425187910, -62);

    ReadChar
END NegativeBase.
```

```txt
          10 encoded in base  -2 =        11110
       11110 decoded in base  -2 =           10

         146 encoded in base  -3 =        21102
       21102 decoded in base  -3 =          146

          15 encoded in base -10 =          195
         195 decoded in base -10 =           15

-19425187910 encoded in base -62 =       Modula
      Modula decoded in base -62 = -19425187910
```



## ooRexx

```oorexx
/* REXX ---------------------------------------------------------------
* Adapt for ooRexx (use of now invalid variable names)
* and make it work for base -63 (Algol example)
*--------------------------------------------------------------------*/
Numeric Digits 20
digits='0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz '
txt=' converted to base '
n=      10;  b= -2;  nb=nBase(n,b);  Say right(n,20) txt right(b,3) '---->' nb ok()
n=     146;  b= -3;  nb=nBase(n,b);  Say right(n,20) txt right(b,3) '---->' nb ok()
n=      15;  b=-10;  nb=nBase(n,b);  Say right(n,20) txt right(b,3) '---->' nb ok()
n=     -15;  b=-10;  nb=nBase(n,b);  Say right(n,20) txt right(b,3) '---->' nb ok()
n=       0;  b= -5;  nb=nBase(n,b);  Say right(n,20) txt right(b,3) '---->' nb ok()
n=-6284695;  b=-62;  nb=nBase(n,b);  Say right(n,20) txt right(b,3) '---->' nb ok()
n=-36492107981104; b=-63;nb=nBase(n,b); Say right(n,20) txt right(b,3) '---->' nb ok()
Exit

nBase: Procedure Expose digits
/*---------------------------------------------------------------------
* convert x (base 10) to result (base r)
*--------------------------------------------------------------------*/
  Parse arg x,r
  result=''
  Do While x\==0              /*keep Processing while  X  isn't zero.*/
    z=x//r
    x=x%r                     /*calculate remainder; calculate int ÷.*/
    If z<0 Then Do
      z=z-r                   /*subtract a negative  R  from  Z ?--+ */
      x=x+1                   /*bump  X  by one.                   ¦ */
      End
    result=substr(digits,z+1,1)result        /*prepend the new digit */
    End
  If result='' Then result=0
  Return result

pBase: Procedure Expose digits;
/*---------------------------------------------------------------------
* convert x (base r) to result (base 10)
*--------------------------------------------------------------------*/
  Parse arg x,r;
  result=0;
  p=0
  len=length(x)
  Do j=len by -1 For len            /*Process each of the X's digits */
    v=pos(substr(x,j,1),digits)-1   /*use digit's numeric value.     */
    result=result+v*r**p;           /*add it to result               */
    p=p+1                           /*bump power by 1                */
    End
  Return result

ok:
/*---------------------------------------------------------------------
* check back conversion
*--------------------------------------------------------------------*/
  back=pBase(nb,b)
  If back\=n  Then
    r='Error backward conversion results in' back
  Else
    r='ok'
  Return r
```

```txt
                  10  converted to base   -2 ----> 11110 ok
                 146  converted to base   -3 ----> 21102 ok
                  15  converted to base  -10 ----> 195 ok
                 -15  converted to base  -10 ----> 25 ok
                   0  converted to base   -5 ----> 0 ok
            -6284695  converted to base  -62 ----> Rexx ok
     -36492107981104  converted to base  -63 ----> Algol 68 ok
```



## Perl

```perl
use strict;
use feature 'say';
use POSIX qw(floor);
use ntheory qw/fromdigits todigits/;

sub encode {
    my($n, $b) = @_;
    my @out;
    my $r = 0;

    while ($n) {
        $r = $n % $b;
        $n = floor $n/$b;
        $n += 1, $r -= $b if $r < 0;
        push @out, todigits($r, -$b) || 0;
    }
    join '', reverse @out;
}

sub decode {
    my($s, $b) = @_;
    my $total = 0;
    my $i = 0;
    for my $c (reverse split '', $s) {
        $total += (fromdigits($c, -$b) * $b**$i);
        $i++;
    }
    $total
}

say ' 10 in base  -2: ', encode(10, -2);
say ' 15 in base -10: ', encode(15, -10);
say '146 in base  -3: ', encode(146, -3);
say '';
say  '11110 from base  -2: ', decode("11110", -2);
say  '21102 from base  -3: ', decode("21102", -3);
say  '  195 from base -10: ', decode("195",  -10);
```

```txt

 10 in base  -2: 11110
 15 in base -10: 195
146 in base  -3: 21102

11110 from base  -2: 10
21102 from base  -3: 146
  195 from base -10: 15
```



## Perl 6

Perl 6 provides built-in methods / routines base and parse-base to convert to and from bases 2 through 36. We'll just shadow the core routines with versions that accept negative bases.

As a stretch goal, rather than implement something that can "Spell the name of the language with correct capitalization" (which is silly, trivial, and has absolutely '''''nothing''''' to do with negative base numbers,) we'll implement routines that correctly work with any Real number, not just integers.

The Real candidate has a 'precision' parameter, default -15, (15 places after the decimal point,) to limit the length of the fractional portion of the converted value. Change it if you need or want different precision. The Integer only routine is kept here as a multi-dispatch candidate since it is potentially much faster than the Real capable routine. The dispatcher will automatically use the most appropriate (fastest) routine.

Note that the parse-base routine will handle 'illegal' negative negative-base values without blowing up.


```perl6
multi sub base ( Int $value is copy, Int $radix where -37 < * < -1) {
    my $result;
    while $value {
        my $r = $value mod $radix;
        $value div= $radix;
        if $r < 0 {
            $value++;
            $r -= $radix
        }
        $result ~= $r.base(-$radix);
    }
    flip $result || ~0;
}

multi sub base ( Real $num, Int $radix where -37 < * < -1, :$precision = -15 ) {
    return '0' unless $num;
    my $value  = $num;
    my $result = '';
    my $place  = 0;
    my $upper-bound = 1 / (-$radix + 1);
    my $lower-bound = $radix * $upper-bound;

    $value = $num / $radix ** ++$place until $lower-bound <= $value < $upper-bound;

    while ($value or $place > 0) and $place > $precision {
        my $digit = ($radix * $value - $lower-bound).Int;
        $value    =  $radix * $value - $digit;
        $result ~= '.' unless $place or $result.contains: '.';
        $result ~= $digit == -$radix ?? ($digit-1).base(-$radix)~'0' !! $digit.base(-$radix);
        $place--
    }
    $result
}

multi sub parse-base (Str $str, Int $radix where -37 < * < -1) {
    return -1 * $str.substr(1).&parse-base($radix) if $str.substr(0,1) eq '-';
    my ($whole, $frac) = $str.split: '.';
    my $fraction = 0;
    $fraction = [+] $frac.comb.kv.map: { $^v.parse-base(-$radix) * $radix ** -($^k+1) } if $frac;
    $fraction + [+] $whole.flip.comb.kv.map: { $^v.parse-base(-$radix) * $radix ** $^k }
}

# TESTING
for <4 -4 0 -7  10 -2  146 -3  15 -10  -19 -10  107 -16
    227.65625 -16  2.375 -4 -1.3e2 -8 41371457.268272761 -36> -> $v, $r {
    my $nbase = $v.&base($r, :precision(-5));
    printf "%20s.&base\(%3d\) = %-11s : %13s.&parse-base\(%3d\) = %s\n",
      $v, $r, $nbase, "'$nbase'", $r, $nbase.&parse-base($r);
}

# 'Illegal' negative-base value
say q|  '-21'.&parse-base(-10) = |, '-21'.&parse-base(-10);
```

```txt
                   4.&base( -4) = 130         :         '130'.&parse-base( -4) = 4
                   0.&base( -7) = 0           :           '0'.&parse-base( -7) = 0
                  10.&base( -2) = 11110       :       '11110'.&parse-base( -2) = 10
                 146.&base( -3) = 21102       :       '21102'.&parse-base( -3) = 146
                  15.&base(-10) = 195         :         '195'.&parse-base(-10) = 15
                 -19.&base(-10) = 21          :          '21'.&parse-base(-10) = -19
                 107.&base(-16) = 1AB         :         '1AB'.&parse-base(-16) = 107
           227.65625.&base(-16) = 124.68      :      '124.68'.&parse-base(-16) = 227.65625
               2.375.&base( -4) = 3.32        :        '3.32'.&parse-base( -4) = 2.375
              -1.3e2.&base( -8) = 1616        :        '1616'.&parse-base( -8) = -130
  41371457.268272761.&base(-36) = PERL6.ROCKS : 'PERL6.ROCKS'.&parse-base(-36) = 41371457.268272761
  '-21'.&parse-base(-10) = 19
```



## Phix


```Phix
constant digits = "0123456789"&
                  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"&
                  "abcdefghijklmnopqrstuvwxyz"

type base(integer b)
    return b<=-1 and b>=-length(digits)
end type

function encodeNegBase(atom n, base b)
    string res = iff(n?"":"0")
    while n!=0 do
        atom r = remainder(n,b)
        n = trunc(n/b)
        if r<0 then
            n += 1
            r -= b
        end if
        res &= digits[r+1]
    end while
    return reverse(res)
end function

function decodeNegBase(string ns, base b)
    atom total = 0,
         bb = 1
    for i=length(ns) to 1 by -1 do
        integer k = find(ns[i],digits)-1
        if k=-1 or k>-b then return "invalid digit" end if
        total += k*bb
        bb *= b
    end for
    return total
end function

                -- decimal, base, expected
constant tests = {{10,      -2,   "11110"},
                  {146,     -3,   "21102"},
                  {15,      -10,  "195"},
                  {-5795577,-62,  "Phix"}}

procedure main()
atom n,b
string e
    for i=1 to length(tests) do
        {n,b,e} = tests[i]
        string ns = encodeNegBase(n, b)
        printf(1,"%9d in base %-3d is %6s\n", {n, b, ns})
        atom nn = decodeNegBase(ns, b)
        string ok = iff(ns=e and nn=n?""," ????")
        printf(1,"%9d <--------------'%s\n",{nn,ok})
    end for
end procedure
main()
```

```txt

       10 in base -2  is  11110
       10 <--------------'
      146 in base -3  is  21102
      146 <--------------'
       15 in base -10 is    195
       15 <--------------'
 -5795577 in base -62 is   Phix
 -5795577 <--------------'

```



## Python



```python
#!/bin/python
from __future__ import print_function

def EncodeNegBase(n, b): #Converts from decimal
	if n == 0:
		return "0"
	out = []
	while n != 0:
		n, rem = divmod(n, b)
		if rem < 0:
			n += 1
			rem -= b
		out.append(rem)
	return "".join(map(str, out[::-1]))

def DecodeNegBase(nstr, b): #Converts to decimal
	if nstr == "0":
		return 0

	total = 0
	for i, ch in enumerate(nstr[::-1]):
		total += int(ch) * b**i
	return total

if __name__=="__main__":

	print ("Encode 10 as negabinary (expect 11110)")
	result = EncodeNegBase(10, -2)
	print (result)
	if DecodeNegBase(result, -2) == 10: print ("Converted back to decimal")
	else: print ("Error converting back to decimal")

	print ("Encode 146 as negaternary (expect 21102)")
	result = EncodeNegBase(146, -3)
	print (result)
	if DecodeNegBase(result, -3) == 146: print ("Converted back to decimal")
	else: print ("Error converting back to decimal")

	print ("Encode 15 as negadecimal (expect 195)")
	result = EncodeNegBase(15, -10)
	print (result)
	if DecodeNegBase(result, -10) == 15: print ("Converted back to decimal")
	else: print ("Error converting back to decimal")
```


```txt
Encode 10 as negabinary (expect 11110)
11110
Converted back to decimal
Encode 146 as negaternary (expect 21102)
21102
Converted back to decimal
Encode 15 as negadecimal (expect 195)
195
Converted back to decimal
```



## Racket



```racket
#lang racket

(define all-digits (string->list "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-"))

(define d->i-map (for/hash ((i (in-naturals)) (d all-digits)) (values d i)))

(define max-base (length all-digits))

(define (q/r n d)
  (let-values (((q r) (quotient/remainder n d)))
    (if (negative? r)
        (values (+ q 1) (- r d))
        (values q r))))

(define (negabase-convertors base)
  (when (not (integer? base)) (raise "Non-integer base."))
  (when (not (<= 2 (abs base) max-base)) (raise (format "(abs base) must be inside [2 ~a] interval." max-base)))
  (values
   (let ((q/r_base (curryr q/r base)))
     (λ (num)
       (define (checked->base num dig)
         (match num
           [0 (apply string dig)]
           [(app q/r_base num/ rst) (checked->base num/ (cons (list-ref all-digits rst) dig))]))
       (if (integer? num)
           (checked->base num  (if (zero? num) '(#\0) '()))
           (raise "Non-integer number."))))

   (λ (dig)
     (define (loop digs acc)
       (match digs [(list) acc] [(cons a d) (loop d (+ (* acc base) (hash-ref d->i-map a)))]))
     (loop (string->list dig) 0))))

(define-values (->negabinary negabinary->) (negabase-convertors -2))

(define-values (->negaternary negaternary->) (negabase-convertors -3))

(define-values (->negadecimal negadecimal->) (negabase-convertors -10))

(define-values (->nega63ary nega63ary->) (negabase-convertors (- max-base)))

(module+ main
  (->negaternary 146)
  (->negabinary 10)
  (->negadecimal 15)
  (->nega63ary -26238001742))

(module+ test
  (require rackunit)

  ;; tests from wikipedia page
  (check-equal? (call-with-values (λ () (q/r 146 -3)) cons) '(-48 . 2))
  (check-equal? (call-with-values (λ () (q/r -48 -3)) cons) '(16 . 0))
  (check-equal? (call-with-values (λ () (q/r 16 -3)) cons) '(-5 . 1))
  (check-equal? (call-with-values (λ () (q/r -5 -3)) cons) '(2 . 1))
  (check-equal? (call-with-values (λ () (q/r 2 -3)) cons) '(0 . 2))

  (define-values (->hexadecimal hexadecimal->) (negabase-convertors 16))
  (check-equal? (->hexadecimal 31) "1F"))
```


```txt
"21102"
"11110"
"195"
"Racket"
```



## REXX

Both REXX versions use a type of   ''assert''   (a function call of '''OK''')   that converts the numbers in the

negative base back to the original number in base ten   (and issues an error message if not correct).
===version 1 (up to base -10)===

```rexx
/*REXX pgm converts & displays a base ten integer to a negative base number (up to -10).*/
@=' converted to base ';      numeric digits 300 /*be able to handle ginormous numbers. */
n= 10;  b= -2;  nb=nBase(n,b);  say right(n, 20)   @   right(b, 3)   '────►'    nb    ok()
n=146;  b= -3;  nb=nBase(n,b);  say right(n, 20)   @   right(b, 3)   '────►'    nb    ok()
n= 15;  b=-10;  nb=nBase(n,b);  say right(n, 20)   @   right(b, 3)   '────►'    nb    ok()
n=-15;  b=-10;  nb=nBase(n,b);  say right(n, 20)   @   right(b, 3)   '────►'    nb    ok()
n=  0;  b= -5;  nb=nBase(n,b);  say right(n, 20)   @   right(b, 3)   '────►'    nb    ok()
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
nBase: procedure; parse arg x,r;           $=    /*obtain args; $ is the integer result.*/
       if r<-10 | r>-2 then do; say 'base' r "must be in range: -2 ───► -10"; exit 13; end
                         do  while x\==0         /*keep processing while  X  isn't zero.*/
                         z=x//r;   x=x%r         /*calculate remainder; calculate int ÷.*/
                         if z<0  then do;  z=z-r /*subtract a negative  R  from  Z ◄──┐ */
                                           x=x+1 /*bump  X  by one.                   │ */
                                      end        /*                   Funny "add" ►───┘ */
                         $=z || $                /*prepend new digit (numeral) to result*/
                         end   /*while*/
       return $
/*──────────────────────────────────────────────────────────────────────────────────────*/
ok:    ?=;  if pBase(nb, b)\=n  then ?=' ◄──error in negative base calculation';  return ?
/*──────────────────────────────────────────────────────────────────────────────────────*/
pBase: procedure; parse arg x,r;    p=0;   $=0   /*obtain args; $ is the integer result.*/
       if r<-10 | r>-2 then do; say 'base' r "must be in range: -2 ───► -10"; exit 13; end
       L=length(x);    do j=L  by -1  for L      /*process each of the numerals in  X.  */
                       $=$ + substr(x,j,1)*r**p  /*add value of a numeral to $ (result).*/
                       p=p+1                     /*bump the power by 1.                 */
                       end   /*j*/               /* [↓]  process the number "bottom-up".*/
       return $
```

```txt

                  10  converted to base   -2 ────► 11110
                 146  converted to base   -3 ────► 21102
                  15  converted to base  -10 ────► 195
                 -15  converted to base  -10 ────► 25
                   0  converted to base   -5 ────► 0

```


===version 2 (up to base -71)===
This REXX version supports up to negative base   '''─71''',   but it may not be compatible to other programming examples

because the symbols (glyphs or numerals) used herein may not be in the same exact order.   The symbols represented

in this REXX program should be able to represent   ''any''   programming language used on Rosetta Code.

```rexx
/*REXX pgm converts & displays a base ten integer to a negative base number (up to -71).*/
@=' converted to base ';      numeric digits 300 /*be able to handle ginormous numbers. */
n=      10;  b= -2;  nb=nBase(n,b);  say right(n,20)  @  right(b,3)   '────►'    nb   ok()
n=     146;  b= -3;  nb=nBase(n,b);  say right(n,20)  @  right(b,3)   '────►'    nb   ok()
n=      15;  b=-10;  nb=nBase(n,b);  say right(n,20)  @  right(b,3)   '────►'    nb   ok()
n=     -15;  b=-10;  nb=nBase(n,b);  say right(n,20)  @  right(b,3)   '────►'    nb   ok()
n=       0;  b= -5;  nb=nBase(n,b);  say right(n,20)  @  right(b,3)   '────►'    nb   ok()
n=-6284695;  b=-62;  nb=nBase(n,b);  say right(n,20)  @  right(b,3)   '────►'    nb   ok()
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
_Base: !='0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz /*+-!éáµ' /*sym*/
       parse arg $;        m=length(!);            L=length(x);              p=0
       if r<-m | r>-2 then do;  say 'base' r "must be in range: -2 ───► -"m; exit 13;  end
       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
nBase: procedure; parse arg x,r;   call _Base    /*get args; $  will be integer result. */
                    do  while x\==0              /*keep processing while  X  isn't zero.*/
                    z=x//r;           x= x%r     /*calculate remainder; calculate int ÷.*/
                    if z<0  then do;  z= z-r     /*subtract a negative  R  from  Z ◄──┐ */
                                      x= x+1     /*bump  X  by one.                   │ */
                                 end             /*                   Funny "add" ►───┘ */
                    $=substr(!, z+1, 1)$         /*prepend the new numeral to the result*/
                    end   /*while*/
       return $
/*──────────────────────────────────────────────────────────────────────────────────────*/
ok:    if pBase(nb,b)\=n  then return ' ◄──error in negative base calculation';  return ''
/*──────────────────────────────────────────────────────────────────────────────────────*/
pBase: procedure; parse arg x,r;   call _Base 0  /*get args; $  will be integer result. */
                    do j=L  by -1  for L         /*process each of the numerals in  X.  */
                    v=pos( substr(x,j,1), !) - 1 /*use base  R  for the numeral's value.*/
                    $=$ + v * r**p;       p= p+1 /*add it to $ (result); bump power by 1*/
                    end   /*j*/                  /* [↑]  process the number "bottom-up".*/
       return $
```

```txt

                  10  converted to base   -2 ────► 11110
                 146  converted to base   -3 ────► 21102
                  15  converted to base  -10 ────► 195
                 -15  converted to base  -10 ────► 25
                   0  converted to base   -5 ────► 0
            -6284695  converted to base  -62 ────► Rexx

```



## Ring


```ring

# Project : Negative base numbers

negs = [[146,-3],[21102,-3],[10,-2],[11110,-2],[15,-10],[195,-10]]
for n = 1 to len(negs) step 2
     enc = encodeNegBase(negs[n][1],negs[n][2])
     encstr = showarray(enc)
     dec = decodeNegBase(negs[n+1][1],negs[n+1][2])
     see "" + negs[n][1] + " encoded in base " + negs[n][2] + " = " + encstr + nl
     see "" + negs[n+1][1] + " decoded in base " + negs[n+1][2] + " = " + dec + nl
next

func encodeNegBase(n,b)
       out = []
       while n != 0
               rem = (n%b)
               if rem < 0
                  rem = rem - b
               ok
               n = ceil(n/b)
               rem = fabs(rem)
               add(out,rem)
       end
       out = reverse(out)
       return out

func decodeNegBase(n,b)
        out = 0
        n = string(n)
        for nr = len(n) to 1 step -1
             out = out + n[nr]*pow(b,len(n)-nr)
        next
        return out

func showarray(vect)
        svect = ""
        for n = 1 to len(vect)
              svect = svect + vect[n]
        next
        return svect

```

Output:

```txt

146 encoded in base -3 = 21102
21102 decoded in base -3 = 146
10 encoded in base -2 = 11110
11110 decoded in base -2 = 10
15 encoded in base -10 = 195
195 decoded in base -10 = 15

```



## Scala


```scala
object NegativeBase {
  val digits = ('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z')

  def intToStr(n: Int, b: Int): String = {
    def _fromInt(n: Int): List[Int] = {
      if (n == 0) {
        Nil
      } else {
        val r = n % b
        val rp = if (r < 0) r + b else r
        val m = -(n - rp)/b
        rp :: _fromInt(m)
      }
    }
    _fromInt(n).map(digits).reverse.mkString
  }

  def strToInt(s: String, b: Int): Int = {
    s.map(digits.indexOf).foldRight((0, 1)){ case (x, (sum, pow)) =>
      (sum + x * pow, pow * -b)
    }._1
  }
}
```



```scala
def testConversion(b: Int)(n: Int, s: String): Unit = {
  println(s"$n in base -$b = ${NegativeBase.intToStr(n, b)}")
  println(s"$s from base -$b = ${NegativeBase.strToInt(s, b)}")
}

testConversion(2)(10, "11110")
testConversion(3)(146, "21102")
testConversion(10)(15, "195")
testConversion(62)(795099356, "Scala")
```


```txt
10 in base -2 = 11110
11110 from base -2 = 10
146 in base -3 = 21102
21102 from base -3 = 146
15 in base -10 = 195
195 from base -10 = 15
795099356 in base -62 = Scala
Scala from base -62 = 795099356
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const string: DIGITS is "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

const func string: encodeNegativeBase (in var integer: number, in integer: base) is func
  result
    var string: encoded is "";
  local
    var integer: remainder is 0;
  begin
    if base < -62 or base > -1 then
      raise RANGE_ERROR;
    elsif number = 0 then
      encoded := "0";
    else
      while number <> 0 do
        remainder := number rem base;
        number := number div base;
        if remainder < 0 then
          incr(number);
          remainder -:= base;
        end if;
        encoded &:= DIGITS[succ(remainder)];
      end while;
      encoded := reverse(encoded);
    end if;
  end func;

const func integer: decodeNegativeBase (in string: encoded, in integer: base) is func
  result
    var integer: decoded is 0;
  local
    var integer: factor is 1;
    var integer: index is 0;
    var integer: digit is 0;
  begin
    if base < -62 or base > -1 then
      raise RANGE_ERROR;
    elsif encoded = "0" then
      decoded := 0;
    else
      for index range length(encoded) downto 1 do
        digit := pred(pos(DIGITS, encoded[index]));
        if digit = -1 then
          raise RANGE_ERROR;
        else
          decoded +:= digit * factor;
          factor *:= base;
        end if;
      end for;
    end if;
  end func;

const proc: doCheck (in integer: number, in integer: base) is func
  local
    var string: encoded is "";
    var integer: decoded is 0;
  begin
    encoded := encodeNegativeBase(number, base);
    writeln(number lpad 10 <& " encoded in base " <& base lpad 3 <& " = " <& encoded);
    decoded := decodeNegativeBase(encoded, base);
    writeln(encoded lpad 10 <& " decoded in base " <& base lpad 3 <& " = " <& decoded);
  end func;

const proc: main is func
  begin
    doCheck(10, -2);
    doCheck(146, -3);
    doCheck(15, -10);
    doCheck(404355637, -62);
  end func;
```


```txt

        10 encoded in base  -2 = 11110
     11110 decoded in base  -2 = 10
       146 encoded in base  -3 = 21102
     21102 decoded in base  -3 = 146
        15 encoded in base -10 = 195
       195 decoded in base -10 = 15
 404355637 encoded in base -62 = Seed7
     Seed7 decoded in base -62 = 404355637

```



## Sidef

```ruby
func EncodeNegBase(Num n, Num b { .~~ (-36 .. -2) }) {
    var out = []
    var r = 0
    while (n) {
        (n, r) = divmod(n, b)
        if (r < 0) {
            n += 1
            r -= b
        }
        out << r.base(-b)
    }
    return (out.join.flip || "0")
}

func DecodeNegBase(Str s, Num b { .~~ (-36 .. -2) }) {
    var total = 0
    for i,c in (s.flip.chars.kv) {
        total += (Num(c, -b) * b**i)
    }
    return total
}

say (" 10 in base  -2: ", EncodeNegBase(10, -2))
say ("146 in base  -3: ", EncodeNegBase(146, -3))
say (" 15 in base -10: ", EncodeNegBase(15, -10))

say '-'*25

say ("11110 from base  -2: ", DecodeNegBase("11110", -2))
say ("21102 from base  -3: ", DecodeNegBase("21102", -3))
say ("  195 from base -10: ", DecodeNegBase("195",  -10))

say '-'*25

# Extra
say ("25334424 in base -31: ", EncodeNegBase(25334424, -31))
say ("sidef  from base -31: ", DecodeNegBase("sidef", -31))
```

```txt

 10 in base  -2: 11110
146 in base  -3: 21102
 15 in base -10: 195
-------------------------
11110 from base  -2: 10
21102 from base  -3: 146
  195 from base -10: 15
-------------------------
25334424 in base -31: sidef
sidef  from base -31: 25334424

```



## VBA

```vb
Const DIGITS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
Dim Str(63) As String
Private Function mod2(a As Long, b As Integer) As Long
    mod2 = a - (a \ b) * b
End Function
Private Function swap(a As String, b As String)
    Dim t As String
    t = a
    a = b
    b = t
End Function
Private Function EncodeNegativeBase(ByVal n As Long, base As Integer) As String
    Dim ptr, idx As Long
    Dim rem_ As Long
    Dim result As String
    If base > -1 Or base < -62 Then
        EncodeNegativeBase = result
    Else
        If n = 0 Then
            EncodeNegativeBase = "0"
        Else
            ptr = 0
            Do While n <> 0
                rem_ = mod2(n, base)
                n = n \ base
                If rem_ < 0 Then
                    n = n + 1
                    rem_ = rem_ - base
                End If
                result = result & Mid(DIGITS, rem_ + 1, 1)
            Loop
        End If
    End If
    EncodeNegativeBase = StrReverse(result)
End Function
Private Function DecodeNegativeBase(ns As String, base As Integer) As Long
    Dim total As Long, bb As Long
    Dim i As Integer, j As Integer
    If base < -62 Or base > -1 Then DecodeNegativeBase = 0
    If Mid(ns, 1, 1) = 0 Or (Mid(ns, 1, 1) = "0" And Mid(ns, 2, 1) = 0) Then DecodeNegativeBase = 0
    i = Len(ns)
    total = 0
    bb = 1
    Do While i >= 1
        j = InStr(1, DIGITS, Mid(ns, i, 1), vbTextCompare) - 1
        total = total + j * bb
        bb = bb * base
        i = i - 1
    Loop
    DecodeNegativeBase = total
End Function
Private Sub Driver(n As Long, b As Integer)
    Dim ns As String
    Dim p As Long
    ns = EncodeNegativeBase(n, b)
    Debug.Print CStr(n); " encoded in base "; b; " = "; ns
    p = DecodeNegativeBase(ns, b)
    Debug.Print ns; " decoded in base "; b; " = "; p
    Debug.Print
End Sub
Public Sub main()
    Driver 10, -2
    Driver 146, -3
    Driver 15, -10
    Driver 118492, -62
End Sub
```

```txt
10 encoded in base -2  = 11110
11110 decoded in base -2  =  10

146 encoded in base -3  = 21102
21102 decoded in base -3  =  146

15 encoded in base -10  = 195
195 decoded in base -10  =  15

118492 encoded in base -62  = VBA
VBA decoded in base -62  =  118492
```


## zkl


```zkl
fcn toNBase(n,radix){
   var [const] cs=[0..9].chain(["a".."z"]).pump(String);  //"0123456789abcd..z"
   _assert_(-37 < radix < -1,"invalid radix");
   digits:=List();
   while(n){ reg r;
      n,r=n.divr(radix);	// C compiler semantics
      if(r<0){ n+=1; r-=radix; }
      digits.append(r);
   }
   digits.reverse().pump(String,cs.get);
}

fcn toInt(str,radix){  // the toInt(radix) method radix is 2..36
   str.reduce('wrap(s,d,rdx){ s*radix + d.toInt(rdx); },0,radix.abs());
}
```


```zkl
ns:=T( T(10,-2), T(146,-3), T(15,-10), T(107,-16), T(41371458,-36), T(44661,-36) );
results:=ns.pump(List,Void.Xplode,toNBase);
foreach nb,r in (ns.zip(results)){
   _,b:=nb;
   println("%10d.base(%3d) = \"%s\" --> %d".fmt(nb.xplode(),r,toInt(r,b)));
}
```

```txt

        10.base( -2) = "11110" --> 10
       146.base( -3) = "21102" --> 146
        15.base(-10) = "195" --> 15
       107.base(-16) = "1ab" --> 107
  41371458.base(-36) = "perl6" --> 41371458
     44661.base(-36) = "zkl" --> 44661

```


'''References'''

<references/>
