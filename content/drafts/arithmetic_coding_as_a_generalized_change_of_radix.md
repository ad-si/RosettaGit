+++
title = "Arithmetic coding/As a generalized change of radix"
description = ""
date = 2019-08-22T14:35:45Z
aliases = []
[extra]
id = 19924
[taxonomies]
categories = []
tags = []
+++

{{draft task}} [[wp:Arithmetic coding|Arithmetic coding]] is a form of entropy encoding used in lossless data compression. Normally, a string of characters such as the words "hello there" is represented using a fixed number of bits per character, as in the ASCII code. When a string is converted to arithmetic encoding, frequently used characters will be stored with fewer bits and not-so-frequently occurring characters will be stored with more bits, resulting in fewer bits used in total. Arithmetic coding differs from other forms of entropy encoding, such as [[Huffman coding]], in that rather than separating the input into component symbols and replacing each with a code, arithmetic coding encodes the entire message into a single number.

;Task
Create a program which implements the arithmetic coding [[wp:Arithmetic_coding#Arithmetic_coding_as_a_generalized_change_of_radix|as a generalized change of radix]].

Show the results, in base 10, for all the following strings:

* "DABDDB"
* "DABDDBBDDBA"
* "ABRACADABRA"
* "TOBEORNOTTOBEORTOBEORNOT"


Verify the implementation by decoding the results back into strings and checking for equality with the given strings.

=={{header|C#|C sharp}}==
{{trans|Java}}

```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Text;

namespace AruthmeticCoding {
    using Freq = Dictionary<char, long>;
    using Triple = Tuple<BigInteger, int, Dictionary<char, long>>;

    class Program {
        static Freq CumulativeFreq(Freq freq) {
            long total = 0;
            Freq cf = new Freq();
            for (int i = 0; i < 256; i++) {
                char c = (char)i;
                if (freq.ContainsKey(c)) {
                    long v = freq[c];
                    cf[c] = total;
                    total += v;
                }
            }
            return cf;
        }

        static Triple ArithmeticCoding(string str, long radix) {
            // The frequency of characters
            Freq freq = new Freq();
            foreach (char c in str) {
                if (freq.ContainsKey(c)) {
                    freq[c] += 1;
                } else {
                    freq[c] = 1;
                }
            }

            // The cumulative frequency
            Freq cf = CumulativeFreq(freq);

            // Base
            BigInteger @base = str.Length;

            // Lower bound
            BigInteger lower = 0;

            // Product of all frequencies
            BigInteger pf = 1;

            // Each term is multiplied by the product of the
            // frequencies of all previously occuring symbols
            foreach (char c in str) {
                BigInteger x = cf[c];
                lower = lower * @base + x * pf;
                pf = pf * freq[c];
            }

            // Upper bound
            BigInteger upper = lower + pf;

            int powr = 0;
            BigInteger bigRadix = radix;

            while (true) {
                pf = pf / bigRadix;
                if (pf == 0) break;
                powr++;
            }

            BigInteger diff = (upper - 1) / (BigInteger.Pow(bigRadix, powr));
            return new Triple(diff, powr, freq);
        }

        static string ArithmeticDecoding(BigInteger num, long radix, int pwr, Freq freq) {
            BigInteger powr = radix;
            BigInteger enc = num * BigInteger.Pow(powr, pwr);
            long @base = freq.Values.Sum();

            // Create the cumulative frequency table
            Freq cf = CumulativeFreq(freq);

            // Create the dictionary
            Dictionary<long, char> dict = new Dictionary<long, char>();
            foreach (char key in cf.Keys) {
                long value = cf[key];
                dict[value] = key;
            }

            // Fill the gaps in the dictionary
            long lchar = -1;
            for (long i = 0; i < @base; i++) {
                if (dict.ContainsKey(i)) {
                    lchar = dict[i];
                } else if (lchar != -1) {
                    dict[i] = (char)lchar;
                }
            }

            // Decode the input number
            StringBuilder decoded = new StringBuilder((int)@base);
            BigInteger bigBase = @base;
            for (long i = @base - 1; i >= 0; --i) {
                BigInteger pow = BigInteger.Pow(bigBase, (int)i);
                BigInteger div = enc / pow;
                char c = dict[(long)div];
                BigInteger fv = freq[c];
                BigInteger cv = cf[c];
                BigInteger diff = enc - pow * cv;
                enc = diff / fv;
                decoded.Append(c);
            }

            // Return the decoded output
            return decoded.ToString();
        }

        static void Main(string[] args) {
            long radix = 10;
            string[] strings = { "DABDDB", "DABDDBBDDBA", "ABRACADABRA", "TOBEORNOTTOBEORTOBEORNOT" };
            foreach (string str in strings) {
                Triple encoded = ArithmeticCoding(str, radix);
                string dec = ArithmeticDecoding(encoded.Item1, radix, encoded.Item2, encoded.Item3);
                Console.WriteLine("{0,-25}=> {1,19} * {2}^{3}", str, encoded.Item1, radix, encoded.Item2);
                if (str != dec) {
                    throw new Exception("\tHowever that is incorrect!");
                }
            }
        }
    }
}
```

{{out}}

```txt
DABDDB                   =>                 251 * 10^2
DABDDBBDDBA              =>              167351 * 10^6
ABRACADABRA              =>             7954170 * 10^4
TOBEORNOTTOBEORTOBEORNOT => 1150764267498783364 * 10^15
```



## D

{{trans|Go}}

```D
import std.array;
import std.bigint;
import std.stdio;
import std.typecons;

BigInt bigPow(BigInt b, BigInt e) {
    if (e == 0) {
        return BigInt(1);
    }

    BigInt result = 1;
    while (e > 1) {
        if (e % 2 == 0) {
            b *= b;
            e /= 2;
        } else {
            result *= b;
            b *= b;
            e = (e - 1) / 2;
        }
    }

    return b * result;
}

long[byte] cumulative_freq(long[byte] freq) {
    long[byte] cf;
    long total;
    foreach (i; 0..256) {
        byte b = cast(byte) i;
        if (b in freq) {
            cf[b] = total;
            total += freq[b];
        }
    }
    return cf;
}

Tuple!(BigInt, BigInt, long[byte]) arithmethic_coding(string str, long radix) {
    // Convert the string into a slice of bytes
    auto chars = cast(byte[]) str;

    // The frequency characters
    long[byte] freq;
    foreach (c; chars) {
        freq[c]++;
    }

    // The cumulative frequency
    auto cf = cumulative_freq(freq);

    // Base
    BigInt base = chars.length;

    // Lower bound
    BigInt lower = 0;

    // Product of all frequencies
    BigInt pf = 1;

    // Each term is multiplied by the product of the
    // frequencies of all previously occurring symbols
    foreach (c; chars) {
        BigInt x = cf[c];

        lower = lower*base + x*pf;
        pf = pf*freq[c];
    }

    // Upper bound
    auto upper = lower + pf;

    BigInt tmp = pf;
    auto powr = BigInt("0");

    while (true) {
        tmp = tmp / radix;
        if (tmp == 0) {
            break;
        }
        powr++;
    }

    auto diff = (upper-1) / bigPow(BigInt(radix), powr);

    return tuple(diff, powr, freq);
}

string arithmethic_decoding(BigInt num, long radix, BigInt pow, long[byte] freq) {
    BigInt powr = radix;

    BigInt enc = num * bigPow(powr, pow);

    BigInt base = 0;
    foreach (v; freq) {
        base += v;
    }

    // Create the cumulative frequency table
    auto cf = cumulative_freq(freq);

    // Create the dictionary
    byte[long] dict;
    foreach (k,v; cf) {
        dict[v] = k;
    }

    // Fill the gaps in the dictionary
    long lchar = -1;
    for (long i=0; i<base; i++) {
        if (i in dict) {
            lchar = dict[i];
        } else if (lchar != -1) {
            dict[i] = cast(byte) lchar;
        }
    }

    // Decode the input number
    auto decoded = appender!string;
    for (BigInt i=base-1; i>=0; i--) {
        pow = bigPow(base, i);

        auto div = enc / pow;

        auto c = dict[div.toLong];
        auto fv = freq[c];
        auto cv = cf[c];

        auto prod = pow * cv;
        auto diff = enc - prod;
        enc = diff / fv;

        decoded.put(c);
    }

    // Return the decoded output
    return decoded.data;
}

void main() {
    long radix = 10;

    foreach (str; ["DABDDB", "DABDDBBDDBA", "ABRACADABRA", "TOBEORNOTTOBEORTOBEORNOT"]) {
        auto output = arithmethic_coding(str, radix);
        auto dec = arithmethic_decoding(output[0], radix, output[1], output[2]);
        writefln("%-25s=> %19s * %s^%s", str, output[0], radix, output[1]);

        if (str != dec) {
            throw new Exception("\tHowever that is incorrect!");
        }
    }
}
```

{{out}}

```txt
DABDDB                   =>                 251 * 10^2
DABDDBBDDBA              =>              167351 * 10^6
ABRACADABRA              =>             7954170 * 10^4
TOBEORNOTTOBEORTOBEORNOT => 1150764267498783364 * 10^15
```



## Go


```go
package main

import (
    "fmt"
    "math/big"
)

func cumulative_freq(freq map[byte]int64) map[byte]int64 {
    total := int64(0)
    cf := make(map[byte]int64)
    for i := 0; i < 256; i++ {
        b := byte(i)
        if v, ok := freq[b]; ok {
            cf[b] = total
            total += v
        }
    }
    return cf
}

func arithmethic_coding(str string, radix int64) (*big.Int,
                                *big.Int, map[byte]int64) {

    // Convert the string into a slice of bytes
    chars := []byte(str)

    // The frequency characters
    freq := make(map[byte]int64)
    for _, c := range chars {
        freq[c] += 1
    }

    // The cumulative frequency
    cf := cumulative_freq(freq)

    // Base
    base := len(chars)

    // Lower bound
    L := big.NewInt(0)

    // Product of all frequencies
    pf := big.NewInt(1)

    // Each term is multiplied by the product of the
    // frequencies of all previously occurring symbols
    bigBase := big.NewInt(int64(base))

    for _, c := range chars {
        x := big.NewInt(cf[c])

        L.Mul(L, bigBase)
        L.Add(L, x.Mul(x, pf))
        pf.Mul(pf, big.NewInt(freq[c]))
    }

    // Upper bound
    U := big.NewInt(0)
    U.Set(L)
    U.Add(U, pf)

    bigOne := big.NewInt(1)
    bigZero := big.NewInt(0)
    bigRadix := big.NewInt(radix)

    tmp := big.NewInt(0).Set(pf)
    powr := big.NewInt(0)

    for {
        tmp.Div(tmp, bigRadix)
        if tmp.Cmp(bigZero) == 0 {
            break
        }
        powr.Add(powr, bigOne)
    }

    diff := big.NewInt(0)
    diff.Sub(U, bigOne)
    diff.Div(diff, big.NewInt(0).Exp(bigRadix, powr, nil))

    return diff, powr, freq
}

func arithmethic_decoding(num *big.Int, radix int64,
          pow *big.Int, freq map[byte]int64) string {

    powr := big.NewInt(radix)

    enc := big.NewInt(0).Set(num)
    enc.Mul(enc, powr.Exp(powr, pow, nil))

    base := int64(0)
    for _, v := range freq {
        base += v
    }

    // Create the cumulative frequency table
    cf := cumulative_freq(freq)

    // Create the dictionary
    dict := make(map[int64]byte)
    for k, v := range cf {
        dict[v] = k
    }

    // Fill the gaps in the dictionary
    lchar := -1
    for i := int64(0); i < base; i++ {
        if v, ok := dict[i]; ok {
            lchar = int(v)
        } else if lchar != -1 {
            dict[i] = byte(lchar)
        }
    }

    // Decode the input number
    decoded := make([]byte, base)
    bigBase := big.NewInt(base)

    for i := base - 1; i >= 0; i-- {

        pow := big.NewInt(0)
        pow.Exp(bigBase, big.NewInt(i), nil)

        div := big.NewInt(0)
        div.Div(enc, pow)

        c := dict[div.Int64()]
        fv := freq[c]
        cv := cf[c]

        prod := big.NewInt(0).Mul(pow, big.NewInt(cv))
        diff := big.NewInt(0).Sub(enc, prod)
        enc.Div(diff, big.NewInt(fv))

        decoded[base-i-1] = c
    }

    // Return the decoded output
    return string(decoded)
}

func main() {

    var radix = int64(10)

    strSlice := []string{
        `DABDDB`,
        `DABDDBBDDBA`,
        `ABRACADABRA`,
        `TOBEORNOTTOBEORTOBEORNOT`,
    }

    for _, str := range strSlice {
        enc, pow, freq := arithmethic_coding(str, radix)
        dec := arithmethic_decoding(enc, radix, pow, freq)
        fmt.Printf("%-25s=> %19s * %d^%s\n", str, enc, radix, pow)

        if str != dec {
            panic("\tHowever that is incorrect!")
        }
    }
}
```

{{out}}

```txt

DABDDB                   =>                 251 * 10^2
DABDDBBDDBA              =>              167351 * 10^6
ABRACADABRA              =>             7954170 * 10^4
TOBEORNOTTOBEORTOBEORNOT => 1150764267498783364 * 10^15

```



## J


Implementation:


```J
NB. generate a frequency dictionary from a reference string
aekDict=:verb define
  d=. ~.y            NB. dictionary lists unique characters
  o=. /:d            NB. in canonical order
  f=. (#/.~%&x:#)y   NB. and their relative frequencies
  (o{d);o{f
)

NB. encode a string against a reference dict
aekEnc=:verb define
  NB. use string to generate a dict if none provided
  (aekDict y) aekEnc y
:
  'u F'=.x                   NB. unpack dictionary
  b=. x:#y                   NB. numeric base
  f=. b*F                    NB. absolute frequencies
  i=. u i.y                  NB. character indices
  c=. +/\0,}:f               NB. cumulative frequencies
  L=. b #. (i{c)**/\1,}:i{f  NB. lower bound
  p=. */i{f                  NB. product of character frequencies
  e=. x:<.10^.p              NB. number of decimal positions to drop
  e,~<.(L+p)%10^e
)

aekDec=:adverb define
:
  'u F'=. x                  NB. unpack dictionary
  f=. m*F                    NB. frequencies of characters
  c=.+/\0,}:f                NB. cumulative frequencies
  C=.<:}.c,m                 NB. id lookup table
  N=. (* 10&^)/y             NB. remainder being decoded
  r=. ''                     NB. result of decode 
  for_d. m^x:i.-m do.        NB. positional values
   id=. <.N%d                NB. character id
   i=.C I.id                 NB. character index
   N=.<.(N -(i{c)*d)%i{f     NB. corrected remainder 
   r=.r,u{~i                 NB. accumulated result
  end.
)

NB. task demo utility:
aek=:verb define
  dict=. aekDict y
  echo 'Dictionary:'
  echo ' ',.(0{::dict),.' ',.":,.1{::dict
  echo 'Length:'
  echo ' ',":#y
  echo 'Encoded:'
  echo ' ',":dict aekEnc y
  echo 'Decoded:'
  echo ' ',":dict (#y) aekDec aekEnc y
)
```


Example use:


```J
   aek 'DABDDB'
Dictionary:
 A 1r6
 B 1r3
 D 1r2
Length:
 6
Encoded:
 251 2
Decoded:
 DABDDB

   aek 'DABDDBBDDBA'
Dictionary:
 A 2r11
 B 4r11
 D 5r11
Length:
 11
Encoded:
 167351 6
Decoded:
 DABDDBBDDBA

   aek 'ABRACADABRA'
Dictionary:
 A 5r11
 B 2r11
 C 1r11
 D 1r11
 R 2r11
Length:
 11
Encoded:
 7954170 4
Decoded:
 ABRACADABRA

   aek 'TOBEORNOTTOBEORTOBEORNOT'
Dictionary:
 B  1r8
 E  1r8
 N 1r12
 O  1r3
 R  1r8
 T 5r24
Length:
 24
Encoded:
 1150764267498783364 15
Decoded:
 TOBEORNOTTOBEORTOBEORNOT
```


Note that for this task we use our plaintext to generate our dictionary for decoding. Also note that we use rational numbers, rather than floating point, for our dictionary, because floating point tends to be inexact.


## Java

{{trans|Kotlin}}

```Java
import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class ArithmeticCoding {
    private static class Triple<A, B, C> {
        A a;
        B b;
        C c;

        Triple(A a, B b, C c) {
            this.a = a;
            this.b = b;
            this.c = c;
        }
    }

    private static class Freq extends HashMap<Character, Long> {
        //"type alias"
    }

    private static Freq cumulativeFreq(Freq freq) {
        long total = 0;
        Freq cf = new Freq();
        for (int i = 0; i < 256; ++i) {
            char c = (char) i;
            Long v = freq.get(c);
            if (v != null) {
                cf.put(c, total);
                total += v;
            }
        }
        return cf;
    }

    private static Triple<BigInteger, Integer, Freq> arithmeticCoding(String str, Long radix) {
        // Convert the string into a char array
        char[] chars = str.toCharArray();

        // The frequency characters
        Freq freq = new Freq();
        for (char c : chars) {
            if (!freq.containsKey(c))
                freq.put(c, 1L);
            else
                freq.put(c, freq.get(c) + 1);
        }

        // The cumulative frequency
        Freq cf = cumulativeFreq(freq);

        // Base
        BigInteger base = BigInteger.valueOf(chars.length);

        // LowerBound
        BigInteger lower = BigInteger.ZERO;

        // Product of all frequencies
        BigInteger pf = BigInteger.ONE;

        // Each term is multiplied by the product of the
        // frequencies of all previously occurring symbols
        for (char c : chars) {
            BigInteger x = BigInteger.valueOf(cf.get(c));
            lower = lower.multiply(base).add(x.multiply(pf));
            pf = pf.multiply(BigInteger.valueOf(freq.get(c)));
        }

        // Upper bound
        BigInteger upper = lower.add(pf);

        int powr = 0;
        BigInteger bigRadix = BigInteger.valueOf(radix);

        while (true) {
            pf = pf.divide(bigRadix);
            if (pf.equals(BigInteger.ZERO)) break;
            powr++;
        }

        BigInteger diff = upper.subtract(BigInteger.ONE).divide(bigRadix.pow(powr));
        return new Triple<>(diff, powr, freq);
    }

    private static String arithmeticDecoding(BigInteger num, long radix, int pwr, Freq freq) {
        BigInteger powr = BigInteger.valueOf(radix);
        BigInteger enc = num.multiply(powr.pow(pwr));
        long base = 0;
        for (Long v : freq.values()) base += v;

        // Create the cumulative frequency table
        Freq cf = cumulativeFreq(freq);

        // Create the dictionary
        Map<Long, Character> dict = new HashMap<>();
        for (Map.Entry<Character, Long> entry : cf.entrySet()) dict.put(entry.getValue(), entry.getKey());

        // Fill the gaps in the dictionary
        long lchar = -1;
        for (long i = 0; i < base; ++i) {
            Character v = dict.get(i);
            if (v != null) {
                lchar = v;
            } else if (lchar != -1) {
                dict.put(i, (char) lchar);
            }
        }

        // Decode the input number
        StringBuilder decoded = new StringBuilder((int) base);
        BigInteger bigBase = BigInteger.valueOf(base);
        for (long i = base - 1; i >= 0; --i) {
            BigInteger pow = bigBase.pow((int) i);
            BigInteger div = enc.divide(pow);
            Character c = dict.get(div.longValue());
            BigInteger fv = BigInteger.valueOf(freq.get(c));
            BigInteger cv = BigInteger.valueOf(cf.get(c));
            BigInteger diff = enc.subtract(pow.multiply(cv));
            enc = diff.divide(fv);
            decoded.append(c);
        }
        // Return the decoded output
        return decoded.toString();
    }

    public static void main(String[] args) {
        long radix = 10;
        String[] strings = {"DABDDB", "DABDDBBDDBA", "ABRACADABRA", "TOBEORNOTTOBEORTOBEORNOT"};
        String fmt = "%-25s=> %19s * %d^%s\n";
        for (String str : strings) {
            Triple<BigInteger, Integer, Freq> encoded = arithmeticCoding(str, radix);
            String dec = arithmeticDecoding(encoded.a, radix, encoded.b, encoded.c);
            System.out.printf(fmt, str, encoded.a, radix, encoded.b);
            if (!Objects.equals(str, dec)) throw new RuntimeException("\tHowever that is incorrect!");
        }
    }
}
```

{{out}}

```txt
DABDDB                   =>                 251 * 10^2
DABDDBBDDBA              =>              167351 * 10^6
ABRACADABRA              =>             7954170 * 10^4
TOBEORNOTTOBEORTOBEORNOT => 1150764267498783364 * 10^15
```




## Julia

Taken from the wikipedia example.

```julia
function charfreq(s)
    d = Dict()
    for c in s
        if haskey(d, c)
            d[c] += 1
        else
            d[c] = 1
        end
    end
    d
end

function charcumfreq(dfreq)
    lastval = 0
    d = Dict()
    for c in sort!(collect(keys(dfreq)))
        d[c] = lastval
        lastval += dfreq[c]
    end
    d
end

function L(s, dfreq, dcumfreq)
    nbase = BigInt(length(s))
    lsum, cumprod = BigInt(0), 1
    for (i, c) in enumerate(s)
        lsum += nbase^(nbase - i) * dcumfreq[c] * cumprod
        cumprod *= dfreq[c]
    end
    lsum
end

U(l, s, dfreq) = l + prod(c -> dfreq[c], s)

function mostzeros(low, high)
    z = Int(floor(log10(high - low)))
    if z == 0
        return string(low), 0
    end
    if low <= parse(BigInt, string(high)[1:end- z - 1] * "0"^(z + 1)) <= high
        z += 1
    end
    return string(high)[1:end-z], z
end

function msgnum(s)
    dfreq = charfreq(s)
    dcumfreq = charcumfreq(dfreq)
    low = L(s, dfreq, dcumfreq)
    high = U(low, s, dfreq)
    return mostzeros(low, high), dfreq
end

function decode(encoded, fdict)
    cdict, bas = charcumfreq(fdict), sum(values(fdict))
    kys = sort!(collect(keys(cdict)))
    revdict = Dict([(cdict[k], k) for k in kys])
    lastkey = revdict[0]
    for i in 0:bas
        if !haskey(revdict, i)
            revdict[i] = lastkey
        else
            lastkey = revdict[i]
        end
    end
    rem = parse(BigInt, encoded)
    s = ""
    for i in 1:bas
        basepow = BigInt(bas)^(bas -i)
        c = revdict[div(rem, basepow)]
        s *= c
        rem = div(rem - basepow * cdict[c], fdict[c])
    end
    s
end

for s in ["DABDDB", "DABDDBBDDBA", "ABRACADABRA", "TOBEORNOTTOBEORTOBEORNOT"]
    (encoded, z), dfreq = msgnum(s)
    println(lpad(s, 30), "  ", rpad(encoded, 19), " * 10^", rpad(z, 4), "  ",
        decode(encoded * "0"^z, dfreq))
end

```
{{out}}

```txt

                        DABDDB  251                 * 10^2     DABDDB
                   DABDDBBDDBA  16735               * 10^7     DABDDBBDDBA
                   ABRACADABRA  795417              * 10^5     ABRACADABRA
      TOBEORNOTTOBEORTOBEORNOT  1150764267498783364 * 10^15    TOBEORNOTTOBEORTOBEORNOT

```



## Kotlin

{{trans|Go}}

```scala
// version 1.2.10

import java.math.BigInteger

typealias Freq = Map<Char, Long>

val bigZero = BigInteger.ZERO
val bigOne  = BigInteger.ONE

fun cumulativeFreq(freq: Freq): Freq {
    var total = 0L
    val cf = mutableMapOf<Char, Long>()
    for (i in 0..255) {
        val c = i.toChar()
        val v = freq[c]
        if (v != null) {
            cf[c] = total
            total += v
        }
    }
    return cf
}

fun arithmeticCoding(str: String, radix: Long): Triple<BigInteger, Int, Freq> {
    // Convert the string into a char array
    val chars = str.toCharArray()

    // The frequency characters
    val freq = mutableMapOf<Char, Long>()
    for (c in chars) {
        if (c !in freq)
            freq[c] = 1L
        else
            freq[c] = freq[c]!! + 1
    }

    // The cumulative frequency
    val cf = cumulativeFreq(freq)

    // Base
    val base = chars.size.toBigInteger()

    // LowerBound
    var lower = bigZero

    // Product of all frequencies
    var pf = BigInteger.ONE

    // Each term is multiplied by the product of the
    // frequencies of all previously occurring symbols
    for (c in chars) {
        val x = cf[c]!!.toBigInteger()
        lower  = lower * base + x * pf
        pf *= freq[c]!!.toBigInteger()
    }

    // Upper bound
    val upper = lower + pf

    var powr = 0
    val bigRadix = radix.toBigInteger()

    while (true) {
        pf /= bigRadix
        if (pf == bigZero) break
        powr++
    }

    val diff = (upper - bigOne) / bigRadix.pow(powr)
    return Triple(diff, powr, freq)
}

fun arithmeticDecoding(num: BigInteger, radix: Long, pwr: Int, freq: Freq): String {
    val powr = radix.toBigInteger()
    var enc = num * powr.pow(pwr)
    var base = 0L
    for ((_, v) in freq) base += v

    // Create the cumulative frequency table
    val cf = cumulativeFreq(freq)

    // Create the dictionary
    val dict = mutableMapOf<Long, Char>()
    for ((k, v) in cf) dict[v] = k

    // Fill the gaps in the dictionary
    var lchar = -1
    for (i in 0L until base) {
        val v = dict[i]
        if (v != null) {
            lchar = v.toInt()
        }
        else if(lchar != -1) {
            dict[i] = lchar.toChar()
        }
    }

    // Decode the input number
    val decoded = StringBuilder(base.toInt())
    val bigBase = base.toBigInteger()
    for (i in base - 1L downTo 0L) {
        val pow = bigBase.pow(i.toInt())
        val div = enc / pow
        val c = dict[div.toLong()]
        val fv = freq[c]!!.toBigInteger()
        val cv = cf[c]!!.toBigInteger()
        val diff = enc - pow * cv
        enc = diff / fv
        decoded.append(c)
    }
    // Return the decoded output
    return decoded.toString()
}

fun main(args: Array<String>) {
    val radix = 10L
    val strings = listOf(
        "DABDDB", "DABDDBBDDBA", "ABRACADABRA", "TOBEORNOTTOBEORTOBEORNOT"
    )
    val fmt = "%-25s=> %19s * %d^%s"
    for (str in strings) {
        val (enc, pow, freq) = arithmeticCoding(str, radix)
        val dec = arithmeticDecoding(enc, radix, pow, freq)
        println(fmt.format(str, enc, radix, pow))
        if (str != dec) throw Exception("\tHowever that is incorrect!")
    }
}
```


{{out}}

```txt

DABDDB                   =>                 251 * 10^2
DABDDBBDDBA              =>              167351 * 10^6
ABRACADABRA              =>             7954170 * 10^4
TOBEORNOTTOBEORTOBEORNOT => 1150764267498783364 * 10^15

```



## Perl


```perl
use Math::BigInt (try => 'GMP');

sub cumulative_freq {
    my ($freq) = @_;

    my %cf;
    my $total = Math::BigInt->new(0);
    foreach my $c (sort keys %$freq) {
        $cf{$c} = $total;
        $total += $freq->{$c};
    }

    return %cf;
}

sub arithmethic_coding {
    my ($str, $radix) = @_;
    my @chars = split(//, $str);

    # The frequency characters
    my %freq;
    $freq{$_}++ for @chars;

    # The cumulative frequency table
    my %cf = cumulative_freq(\%freq);

    # Base
    my $base = Math::BigInt->new(scalar @chars);

    # Lower bound
    my $L = Math::BigInt->new(0);

    # Product of all frequencies
    my $pf = Math::BigInt->new(1);

    # Each term is multiplied by the product of the
    # frequencies of all previously occurring symbols
    foreach my $c (@chars) {
        $L->bmuladd($base, $cf{$c} * $pf);
        $pf->bmul($freq{$c});
    }

    # Upper bound
    my $U = $L + $pf;

    my $pow = Math::BigInt->new($pf)->blog($radix);
    my $enc = ($U - 1)->bdiv(Math::BigInt->new($radix)->bpow($pow));

    return ($enc, $pow, \%freq);
}

sub arithmethic_decoding {
    my ($enc, $radix, $pow, $freq) = @_;

    # Multiply enc by radix^pow
    $enc *= $radix**$pow;

    # Base
    my $base = Math::BigInt->new(0);
    $base += $_ for values %{$freq};

    # Create the cumulative frequency table
    my %cf = cumulative_freq($freq);

    # Create the dictionary
    my %dict;
    while (my ($k, $v) = each %cf) {
        $dict{$v} = $k;
    }

    # Fill the gaps in the dictionary
    my $lchar;
    foreach my $i (0 .. $base - 1) {
        if (exists $dict{$i}) {
            $lchar = $dict{$i};
        }
        elsif (defined $lchar) {
            $dict{$i} = $lchar;
        }
    }

    # Decode the input number
    my $decoded = '';
    for (my $pow = $base**($base - 1) ; $pow > 0 ; $pow /= $base) {
        my $div = $enc / $pow;

        my $c  = $dict{$div};
        my $fv = $freq->{$c};
        my $cv = $cf{$c};

        $enc = ($enc - $pow * $cv) / $fv;
        $decoded .= $c;
    }

    # Return the decoded output
    return $decoded;
}

my $radix = 10;    # can be any integer greater or equal with 2

foreach my $str (qw(DABDDB DABDDBBDDBA ABRACADABRA TOBEORNOTTOBEORTOBEORNOT)) {
    my ($enc, $pow, $freq) = arithmethic_coding($str, $radix);
    my $dec = arithmethic_decoding($enc, $radix, $pow, $freq);

    printf("%-25s=> %19s * %d^%s\n", $str, $enc, $radix, $pow);

    if ($str ne $dec) {
        die "\tHowever that is incorrect!";
    }
}
```

{{out}}

```txt

DABDDB                   =>                 251 * 10^2
DABDDBBDDBA              =>              167351 * 10^6
ABRACADABRA              =>             7954170 * 10^4
TOBEORNOTTOBEORTOBEORNOT => 1150764267498783364 * 10^15

```



## Perl 6


```perl6
sub cumulative_freq(%freq) {
    my %cf;
    my $total = 0;
    for %freq.keys.sort -> $c {
        %cf{$c} = $total;
        $total += %freq{$c};
    }
    return %cf;
}

sub arithmethic_coding($str, $radix) {
    my @chars = $str.comb;

    # The frequency characters
    my %freq;
    %freq{$_}++ for @chars;

    # The cumulative frequency table
    my %cf = cumulative_freq(%freq);

    # Base
    my $base = @chars.elems;

    # Lower bound
    my $L = 0;

    # Product of all frequencies
    my $pf = 1;

    # Each term is multiplied by the product of the
    # frequencies of all previously occurring symbols
    for @chars -> $c {
        $L = $L*$base + %cf{$c}*$pf;
        $pf *= %freq{$c};
    }

    # Upper bound
    my $U = $L + $pf;

    my $pow = 0;
    loop {
        $pf div= $radix;
        last if $pf == 0;
        ++$pow;
    }

    my $enc = ($U - 1) div ($radix ** $pow);
    ($enc, $pow, %freq);
}

sub arithmethic_decoding($encoding, $radix, $pow, %freq) {

    # Multiply encoding by radix^pow
    my $enc = $encoding * $radix**$pow;

    # Base
    my $base = [+] %freq.values;

    # Create the cumulative frequency table
    my %cf = cumulative_freq(%freq);

    # Create the dictionary
    my %dict;
    for %cf.kv -> $k,$v {
        %dict{$v} = $k;
    }

    # Fill the gaps in the dictionary
    my $lchar;
    for ^$base -> $i {
        if (%dict{$i}:exists) {
            $lchar = %dict{$i};
        }
        elsif (defined $lchar) {
            %dict{$i} = $lchar;
        }
    }

    # Decode the input number
    my $decoded = '';
    for reverse(^$base) -> $i {

        my $pow = $base**$i;
        my $div = $enc div $pow;

        my $c  = %dict{$div};
        my $fv = %freq{$c};
        my $cv = %cf{$c};

        my $rem = ($enc - $pow*$cv) div $fv;

        $enc = $rem;
        $decoded ~= $c;
    }

    # Return the decoded output
    return $decoded;
}

my $radix = 10;    # can be any integer greater or equal with 2

for <DABDDB DABDDBBDDBA ABRACADABRA TOBEORNOTTOBEORTOBEORNOT> -> $str {
    my ($enc, $pow, %freq) = arithmethic_coding($str, $radix);
    my $dec = arithmethic_decoding($enc, $radix, $pow, %freq);

    printf("%-25s=> %19s * %d^%s\n", $str, $enc, $radix, $pow);

    if ($str ne $dec) {
        die "\tHowever that is incorrect!";
    }
}
```

{{out}}

```txt

DABDDB                   =>                 251 * 10^2
DABDDBBDDBA              =>              167351 * 10^6
ABRACADABRA              =>             7954170 * 10^4
TOBEORNOTTOBEORTOBEORNOT => 1150764267498783364 * 10^15

```



## Phix

{{trans|Go}}
{{trans|Kotlin}}
{{libheader|mpfr}}

```Phix
include mpfr.e
 
function cumulative_freq(sequence freq)
    integer total = 0,
            l = length(freq)
    sequence cf = repeat(-1,l)
    for c=1 to l do
        integer v = freq[c]
        if v!=0 then
            cf[c] = total
            total += v
        end if
    end for
    return cf
end function
 
function arithmethic_coding(string str, integer radix)
    sequence freq = repeat(0,256)
    for i=1 to length(str) do
        freq[str[i]+1] += 1
    end for
    sequence cf = cumulative_freq(freq)
    integer base = length(str)
    mpz lo = mpz_init(0),       -- lower bound
        pf = mpz_init(1),       -- product of all frequencies
        t1 = mpz_init(),        -- temp
        t2 = mpz_init(),        -- temp
        hi = mpz_init(),        -- upper bound
        diff = mpz_init()
    -- Each term is multiplied by the product of the
    -- frequencies of all previously occurring symbols
    for i=1 to length(str) do
        integer c = str[i]+1
        integer x = cf[c]
        mpz_mul_si(t1,lo,base)
        mpz_mul_si(t2,pf,x)
        mpz_add(lo,t1,t2)
        mpz_mul_si(pf,pf,freq[c])
    end for
    mpz_add(hi,lo,pf)
    integer powr = 0
    while true do
        {} = mpz_fdiv_q_ui(pf,pf,radix)
        if mpz_cmp_si(pf,0)=0 then exit end if
        powr += 1
    end while
    mpz_sub_ui(hi,hi,1)
    mpz_ui_pow_ui(t1,radix,powr)
    mpz_fdiv_q(diff,hi,t1)
    return {diff, powr, freq}
end function
 
function arithmethic_decoding(mpz num, integer radix, pwr, sequence freq)
    mpz enc = mpz_init(),
        pow = mpz_init(),
        tmp = mpz_init()
    mpz_ui_pow_ui(enc,radix,pwr)
    mpz_mul(enc,num,enc)
    integer base = sum(freq)
    sequence cf = cumulative_freq(freq)
    sequence dict = repeat(0,base+1)
    for i=1 to length(cf) do
        integer v = cf[i]
        if v!=-1 then dict[v+1] = i-1 end if
    end for
    -- Fill the gaps in the dictionary
    integer lchar = -1
    for i=0 to base do
        integer v = dict[i+1]
        if v!=0 then
            lchar = v
        elsif lchar!=-1 then
            dict[i+1] = lchar
        end if
    end for
    -- Decode the input number
    string decoded = ""
    for i=base-1 to 0 by -1 do
        mpz_ui_pow_ui(pow,base,i)
        mpz_fdiv_q(tmp,enc,pow)
        integer div = mpz_get_si(tmp),
                c = dict[div+1],
                fv = freq[c+1],
                cv = cf[c+1]
        mpz_mul_si(tmp,pow,cv)
        mpz_sub(tmp,enc,tmp)
        {} = mpz_fdiv_q_ui(enc,tmp,fv)
        decoded &= c
    end for
    return decoded
end function
 
constant tests = {"DABDDB", "DABDDBBDDBA", "ABRACADABRA", "TOBEORNOTTOBEORTOBEORNOT"},
         radix = 10
 
for i=1 to length(tests) do
    string str = tests[i]
    {mpz enc, integer pow, sequence freq} = arithmethic_coding(str, radix)
    string dec = arithmethic_decoding(enc, radix, pow, freq),
           ok = iff(str=dec?"(ok)":"***ERROR***"),
           encs = mpz_get_str(enc)
    printf(1,"%-25s=> %19s * %d^%d %s\n",{str, encs, radix, pow, ok})
end for
```

{{out}}

```txt

DABDDB                   =>                 251 * 10^2 (ok)
DABDDBBDDBA              =>              167351 * 10^6 (ok)
ABRACADABRA              =>             7954170 * 10^4 (ok)
TOBEORNOTTOBEORTOBEORNOT => 1150764267498783364 * 10^15 (ok)

```



## Python

{{works with|Python|3.1+}}

```python
from collections import Counter

def cumulative_freq(freq):
    cf = {}
    total = 0
    for b in range(256):
        if b in freq:
            cf[b] = total
            total += freq[b]
    return cf

def arithmethic_coding(bytes, radix):

    # The frequency characters
    freq = Counter(bytes)

    # The cumulative frequency table
    cf = cumulative_freq(freq)

    # Base
    base = len(bytes)

    # Lower bound
    lower = 0

    # Product of all frequencies
    pf = 1

    # Each term is multiplied by the product of the
    # frequencies of all previously occurring symbols
    for b in bytes:
        lower = lower*base + cf[b]*pf
        pf *= freq[b]

    # Upper bound
    upper = lower+pf

    pow = 0
    while True:
        pf //= radix
        if pf==0: break
        pow += 1

    enc = (upper-1) // radix**pow
    return enc, pow, freq

def arithmethic_decoding(enc, radix, pow, freq):

    # Multiply enc by radix^pow
    enc *= radix**pow;

    # Base
    base = sum(freq.values())

    # Create the cumulative frequency table
    cf = cumulative_freq(freq)

    # Create the dictionary
    dict = {}
    for k,v in cf.items():
        dict[v] = k

    # Fill the gaps in the dictionary
    lchar = None
    for i in range(base):
        if i in dict:
            lchar = dict[i]
        elif lchar is not None:
            dict[i] = lchar

    # Decode the input number
    decoded = bytearray()
    for i in range(base-1, -1, -1):
        pow = base**i
        div = enc//pow

        c  = dict[div]
        fv = freq[c]
        cv = cf[c]

        rem = (enc - pow*cv) // fv

        enc = rem
        decoded.append(c)

    # Return the decoded output
    return bytes(decoded)

radix = 10      # can be any integer greater or equal with 2

for str in b'DABDDB DABDDBBDDBA ABRACADABRA TOBEORNOTTOBEORTOBEORNOT'.split():
    enc, pow, freq = arithmethic_coding(str, radix)
    dec = arithmethic_decoding(enc, radix, pow, freq)

    print("%-25s=> %19s * %d^%s" % (str, enc, radix, pow))

    if str != dec:
    	raise Exception("\tHowever that is incorrect!")
```

{{out}}

```txt

DABDDB                   =>                 251 * 10^2
DABDDBBDDBA              =>              167351 * 10^6
ABRACADABRA              =>             7954170 * 10^4
TOBEORNOTTOBEORTOBEORNOT => 1150764267498783364 * 10^15

```



## Ruby


```ruby
def cumulative_freq(freq)
  cf = {}
  total = 0
  freq.keys.sort.each do |b|
    cf[b] = total
    total += freq[b]
  end
  return cf
end

def arithmethic_coding(bytes, radix)

  # The frequency characters
  freq = Hash.new(0)
  bytes.each { |b| freq[b] += 1 }

  # The cumulative frequency table
  cf = cumulative_freq(freq)

  # Base
  base = bytes.size

  # Lower bound
  lower = 0

  # Product of all frequencies
  pf = 1

  # Each term is multiplied by the product of the
  # frequencies of all previously occurring symbols
  bytes.each do |b|
    lower = lower*base + cf[b]*pf
    pf *= freq[b]
  end

  # Upper bound
  upper = lower+pf

  pow = 0
  loop do
    pf /= radix
    break if pf==0
    pow += 1
  end

  enc = ((upper-1) / radix**pow)
  [enc, pow, freq]
end

def arithmethic_decoding(enc, radix, pow, freq)

  # Multiply enc by radix^pow
  enc *= radix**pow;

  # Base
  base = freq.values.reduce(:+)

  # Create the cumulative frequency table
  cf = cumulative_freq(freq)

  # Create the dictionary
  dict = {}
  cf.each_pair do |k,v|
    dict[v] = k
  end

  # Fill the gaps in the dictionary
  lchar = nil
  (0...base).each do |i|
    if dict.has_key?(i)
      lchar = dict[i]
    elsif lchar != nil
      dict[i] = lchar
    end
  end

  # Decode the input number
  decoded = []
  (0...base).reverse_each do |i|
    pow = base**i
    div = enc/pow

    c  = dict[div]
    fv = freq[c]
    cv = cf[c]

    rem = ((enc - pow*cv) / fv)

    enc = rem
    decoded << c
  end

  # Return the decoded output
  return decoded
end

radix = 10      # can be any integer greater or equal with 2

%w(DABDDB DABDDBBDDBA ABRACADABRA TOBEORNOTTOBEORTOBEORNOT).each do |str|

  enc, pow, freq = arithmethic_coding(str.bytes, radix)
  dec = arithmethic_decoding(enc, radix, pow, freq).map{|b| b.chr }.join

  printf("%-25s=> %19s * %d^%s\n", str, enc, radix, pow)

  if str != dec
    raise "\tHowever that is incorrect!"
  end
end
```

{{out}}

```txt

DABDDB                   =>                 251 * 10^2
DABDDBBDDBA              =>              167351 * 10^6
ABRACADABRA              =>             7954170 * 10^4
TOBEORNOTTOBEORTOBEORNOT => 1150764267498783364 * 10^15

```



## Scala

{{Out}}Best seen in running your browser either by [https://scalafiddle.io/sf/EUNJ0zp/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/DVl840oDS2mFvFQ560fxJAScastie (remote JVM)].

```Scala
object ArithmeticCoding extends App {
  val (radix, strings) = (10, Seq("DABDDB", "DABDDBBDDBA", "ABRACADABRA", "TOBEORNOTTOBEORTOBEORNOT"))
  val fmt = "%-25s=> %19s * %d^%s"

  def arithmeticDecoding( num: BigInt, radix: Int, pwr: Int, freq: Map[Char, Long]): String = {
    var enc = num * BigInt(radix).pow(pwr)
    val base: Long = freq.map { case (_: Char, v: Long) => v }.sum

    // Create the cumulative frequency table
    val cf = cumulativeFreq(freq)
    // Create the dictionary
    val dict0: Map[Long, Char] = cf.map { el: (Char, Long) => (el._2, el._1) }

    var lchar = -1
    val dict: Map[Long, Char] = (0L until base)
      .map { i =>
        val v: Option[Char] = dict0.get(i)
        if (v.isDefined) {
          lchar = v.get.toInt
          (i, v.get)
        } else if (lchar != -1) (i, lchar.toChar)
        else (-1L, ' ')
      }.filter(_ != (-1, ' ')).toMap

    // Decode the input number
    val (bigBase, decoded) = (BigInt(base), new StringBuilder(base.toInt))
    for (i <- base - 1 to 0L by -1) {
      val pow = bigBase.pow(i.toInt)
      val div = enc / pow
      val c = dict(div.longValue)
      val diff = enc - (pow * cf(c))
      enc = diff / freq(c)
      decoded.append(c)
    }
    decoded.mkString
  }

  def cumulativeFreq(freq: Map[Char, Long]): Map[Char, Long] = {
    var total = 0L

    // freq.toSeq.scanLeft(('_', 0L)){ case ((_, acc), (x, y)) => (x, (acc + y))}.filter(_ !=('_', 0L) ).toMap
    freq.toSeq.sortBy(_._1).map { case (k, v) => val temp = total; total += v; (k, temp) }.toMap
  }

  private def arithmeticCoding( str: String, radix: Int): (BigInt, Int, Map[Char, Long]) = {
    val freq = str.toSeq.groupBy(c => c).map { el: (Char, Seq[Char]) =>
      (el._1, el._2.length.toLong)
    }
    val cf = cumulativeFreq(freq)
    var (lower, pf) = (BigInt(0), BigInt(1))

    // Each term is multiplied by the product of the
    // frequencies of all previously occurring symbols
    for (c <- str) {
      lower = (lower * str.length) + cf(c) * pf
      pf = pf * freq(c)
    }
    // Upper bound
    var powr = 0
    val (bigRadix, upper) = (BigInt(radix.toLong), lower + pf)
    do { pf = pf / bigRadix
         if (pf != 0) powr += 1
    } while (pf != 0)
    ((upper - 1) / bigRadix.pow(powr), powr, freq)
  }

  for (str <- strings) {
    val encoded = arithmeticCoding(str, radix)

    def dec =
      arithmeticDecoding(num = encoded._1, radix = radix, pwr = encoded._2, freq = encoded._3)

    println(fmt.format(str, encoded._1, radix, encoded._2))
    if (str != dec) throw new RuntimeException("\tHowever that is incorrect!")
  }
}
```


## Sidef


```ruby
func cumulative_freq(freq) {
    var cf = Hash()
    var total = 0
    256.range.each { |b|
        if (freq.contains(b)) {
            cf{b} = total
            total += freq{b}
        }
    }
    return cf
}

func arithmethic_coding(bytes, radix=10) {

    # The frequency characters
    var freq = Hash()
    bytes.each { |b| freq{b} := 0 ++ }

    # The cumulative frequency table
    var cf = cumulative_freq(freq)

    # Base
    var base = bytes.len

    # Lower bound
    var L = 0

    # Product of all frequencies
    var pf = 1

    # Each term is multiplied by the product of the
    # frequencies of all previously occurring symbols
    bytes.each { |b|
        L = (L*base + cf{b}*pf)
        pf *= freq{b}
    }

    # Upper bound
    var U = L+pf

    var pow = pf.log(radix).int
    var enc = ((U-1) // radix**pow)

    return (enc, pow, freq)
}

func arithmethic_decoding(enc, radix, pow, freq) {

    # Multiply enc by radix^pow
    enc *= radix**pow;

    # Base
    var base = freq.values.sum

    # Create the cumulative frequency table
    var cf = cumulative_freq(freq);

    # Create the dictionary
    var dict = Hash()
    cf.each_kv { |k,v|
        dict{v} = k
    }

    # Fill the gaps in the dictionary
    var lchar = ''
    base.range.each { |i|
        if (dict.contains(i)) {
            lchar = dict{i}
        }
        elsif (!lchar.is_empty) {
            dict{i} = lchar
        }
    }

    # Decode the input number
    var decoded = []
    base.range.reverse.each { |i|

        var pow = base**i;
        var div = enc//pow

        var c  = dict{div}
        var fv = freq{c}
        var cv = cf{c}

        var rem = ((enc - pow*cv) // fv)

        enc = rem
        decoded << c
    }

    # Return the decoded output
    return decoded
}

var radix = 10;      # can be any integer greater or equal with 2

%w(DABDDB DABDDBBDDBA ABRACADABRA TOBEORNOTTOBEORTOBEORNOT).each { |str|

    var (enc, pow, freq) = arithmethic_coding(str.bytes, radix)
    var dec = arithmethic_decoding(enc, radix, pow, freq).join_bytes('UTF-8')

    printf("%-25s=> %19s * %d^%s\n", str, enc, radix, pow);

    if (str != dec) {
        die "\tHowever that is incorrect!"
    }
}
```

{{out}}

```txt
DABDDB                   =>                 251 * 10^2
DABDDBBDDBA              =>              167351 * 10^6
ABRACADABRA              =>             7954170 * 10^4
TOBEORNOTTOBEORTOBEORNOT => 1150764267498783364 * 10^15
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.Numerics
Imports System.Text
Imports Freq = System.Collections.Generic.Dictionary(Of Char, Long)
Imports Triple = System.Tuple(Of System.Numerics.BigInteger, Integer, System.Collections.Generic.Dictionary(Of Char, Long))

Module Module1

    Function CumulativeFreq(freq As Freq) As Freq
        Dim total As Long = 0
        Dim cf As New Freq
        For i = 0 To 255
            Dim c = Chr(i)
            If freq.ContainsKey(c) Then
                Dim v = freq(c)
                cf(c) = total
                total += v
            End If
        Next
        Return cf
    End Function

    Function ArithmeticCoding(str As String, radix As Long) As Triple
        'The frequency of characters
        Dim freq As New Freq
        For Each c In str
            If freq.ContainsKey(c) Then
                freq(c) += 1
            Else
                freq(c) = 1
            End If
        Next

        'The cumulative frequency
        Dim cf = CumulativeFreq(freq)

        ' Base
        Dim base As BigInteger = str.Length

        ' Lower bound
        Dim lower As BigInteger = 0

        ' Product of all frequencies
        Dim pf As BigInteger = 1

        ' Each term is multiplied by the product of the
        ' frequencies of all previously occuring symbols
        For Each c In str
            Dim x = cf(c)
            lower = lower * base + x * pf
            pf = pf * freq(c)
        Next

        ' Upper bound
        Dim upper = lower + pf

        Dim powr = 0
        Dim bigRadix As BigInteger = radix

        While True
            pf = pf / bigRadix
            If pf = 0 Then
                Exit While
            End If
            powr = powr + 1
        End While

        Dim diff = (upper - 1) / (BigInteger.Pow(bigRadix, powr))
        Return New Triple(diff, powr, freq)
    End Function

    Function ArithmeticDecoding(num As BigInteger, radix As Long, pwr As Integer, freq As Freq) As String
        Dim powr As BigInteger = radix
        Dim enc = num * BigInteger.Pow(powr, pwr)
        Dim base = freq.Values.Sum()

        ' Create the cumulative frequency table
        Dim cf = CumulativeFreq(freq)

        ' Create the dictionary
        Dim dict As New Dictionary(Of Long, Char)
        For Each key In cf.Keys
            Dim value = cf(key)
            dict(value) = key
        Next

        ' Fill the gaps in the dictionary
        Dim lchar As Long = -1
        For i As Long = 0 To base - 1
            If dict.ContainsKey(i) Then
                lchar = AscW(dict(i))
            Else
                dict(i) = ChrW(lchar)
            End If
        Next

        ' Decode the input number
        Dim decoded As New StringBuilder
        Dim bigBase As BigInteger = base
        For i As Long = base - 1 To 0 Step -1
            Dim pow = BigInteger.Pow(bigBase, i)
            Dim div = enc / pow
            Dim c = dict(div)
            Dim fv = freq(c)
            Dim cv = cf(c)
            Dim diff = enc - pow * cv
            enc = diff / fv
            decoded.Append(c)
        Next

        ' Return the decoded ouput
        Return decoded.ToString()
    End Function

    Sub Main()
        Dim radix As Long = 10
        Dim strings = {"DABDDB", "DABDDBBDDBA", "ABRACADABRA", "TOBEORNOTTOBEORTOBEORNOT"}
        For Each St In strings
            Dim encoded = ArithmeticCoding(St, radix)
            Dim dec = ArithmeticDecoding(encoded.Item1, radix, encoded.Item2, encoded.Item3)
            Console.WriteLine("{0,-25}=> {1,19} * {2}^{3}", St, encoded.Item1, radix, encoded.Item2)
            If St <> dec Then
                Throw New Exception(vbTab + "However that is incorrect!")
            End If
        Next
    End Sub

End Module
```

{{out}}

```txt
DABDDB                   =>                 251 * 10^2
DABDDBBDDBA              =>              167351 * 10^6
ABRACADABRA              =>             7954170 * 10^4
TOBEORNOTTOBEORTOBEORNOT => 1150764267498783364 * 10^15
```



## zkl

Uses libGMP (GNU MP Bignum Library)

```zkl
var [const] BN=Import("zklBigNum");  // libGMP

fcn cumulativeFreq(freqHash){
   total,cf := 0,Dictionary();
   foreach b in (256){ if(v:=freqHash.find(b)){ cf[b]=total; total+=v; } }
   cf
}
 
fcn arithmethicCoding(str, radix){
   bytes   :=str.split("").apply("toAsc");   // string to bytes: "0"-->0x31
   freqHash:=Dictionary(); bytes.pump(Void,freqHash.incV); // frequency chars
   cf      :=cumulativeFreq(freqHash);		// The cumulative frequency
 
   base,lower:=bytes.len(), BN(0);	// Lower bound
   pf:=BN(1);				// Product of all frequencies
 
   // Each term is multiplied by the product of the
   // frequencies of all previously occurring symbols
   foreach b in (bytes){
      lower.mul(base).add(pf*cf[b]);  // gets quite large
      pf.mul(freqHash[b]);	      // gets big
   }

   upper,powr := lower + pf, 0;
   while(1){
      pf.div(radix);	// in place BigInt math, no garbage
      if(pf==0) break;
      powr+=1;
   }
   enc:=(upper - 1)/BN(radix).pow(powr);

   return(enc,powr,freqHash);
}
```


```zkl
fcn arithmethicDecoding(enc, radix, powr, freqHash){
   enc*=radix.pow(powr);
   base:=freqHash.values.sum(0);
   cf  :=cumulativeFreq(freqHash);   // Create the cumulative frequency table
   dict:=cf.pump(Dictionary(),   // Invert/transpose cumulative table, keys are strings
		fcn(kv){ kv.reverse().apply("toInt") });
   // Fill the gaps in the dictionary
   lchar:=Void;
   foreach b in (base){
      if(v:=dict.find(b)) lchar=v;
      else if(lchar)      dict[b]=lchar;
   }
 
   // Decode the input number
   decoded:=Data();	// byte bucket
   foreach n in ([base-1..0, -1]){
      pow:=BN(base).pow(n);	// a big number
      div:=(enc/pow).toInt();	// a small number, convert from BigInt
      c,fv,cv := dict[div],freqHash[c],cf[c];
      decoded.append(c.toChar());
      enc.sub(pow*cv).div(fv);	// in place BigInt math, no garbage
   }
   decoded.text    // Return the decoded output
}
```


```zkl
radix:=10;
testStrings:=T(
        "DABDDB",
        "DABDDBBDDBA",
        "ABRACADABRA",
        "TOBEORNOTTOBEORTOBEORNOT",);
 
foreach  str in (testStrings){
    enc,pow,freq := arithmethicCoding(str,radix);
    dec:=arithmethicDecoding(enc, radix, pow, freq);
    print("%-25s=> %19s * %d^%s\n".fmt(str,enc,radix,pow));
 
    if(str!=dec) println("\tHowever that is incorrect!");
}
```

{{out}}

```txt

DABDDB                   =>                 251 * 10^2
DABDDBBDDBA              =>              167351 * 10^6
ABRACADABRA              =>             7954170 * 10^4
TOBEORNOTTOBEORTOBEORNOT => 1150764267498783364 * 10^15

```

