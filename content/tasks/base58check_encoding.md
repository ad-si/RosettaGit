+++
title = "Base58Check encoding"
description = ""
date = 2019-08-27T21:48:51Z
aliases = []
[extra]
id = 21517
[taxonomies]
categories = ["Encodings", "Checksums", "task"]
tags = []
+++

## Task

The popular encoding of small and medium-sized [[:Category:Checksums|checksums]] is [[wp:base16|base16]], that is more compact than usual base10 and is human readable... For checksums resulting in ''hash digests'' bigger than ~100 bits, the base16 is too long: [[wp:base58|base58]] is shorter and (when using good alphabet) preserves secure human readability.  The most popular alphabet of base58 is the variant used in  bitcoin address (see [[Bitcoin/address validation]]), so it is the "default base58 alphabet".

Write a program that takes a checksum (resultant hash digest) ''integer binary'' representation as argument, and converts (encode it) into base58 with the standard Bitcoin alphabet &mdash; which uses an alphabet of the characters 0 .. 9, A ..Z, a .. z, but without the four characters:
:::*   '''O'''         the uppercase letter "oh",
:::*   '''I'''    the uppercase letter "eye",
:::*   '''l'''    the lowercase letter "ell",   and
:::*   '''0'''         the digit zero.


The ''reference algorithm'' is at [https://en.bitcoin.it/wiki/Base58Check_encoding#Base58_symbol_chart the Bitcoin's Base58Check page].




## C#
{{trans|Java}}

```c#
using System;
using System.Collections.Generic;
using System.Numerics;
using System.Text;

namespace Base58CheckEncoding {
    class Program {
        const string ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

        static BigInteger ToBigInteger(string value, int @base) {
            const string HEX = "0123456789ABCDEF";
            if (@base < 1 || @base > HEX.Length) {
                throw new ArgumentException("Base is out of range.");
            }

            BigInteger bi = BigInteger.Zero;
            foreach (char c in value) {
                char c2 = Char.ToUpper(c);
                int idx = HEX.IndexOf(c2);
                if (idx == -1 || idx >= @base) {
                    throw new ArgumentOutOfRangeException("Illegal character encountered.");
                }
                bi = bi * @base + idx;
            }

            return bi;
        }

        static string ConvertToBase58(string hash, int @base = 16) {
            BigInteger x;
            if (@base == 16 && hash.Substring(0, 2) == "0x") {
                x = ToBigInteger(hash.Substring(2), @base);
            } else {
                x = ToBigInteger(hash, @base);
            }

            StringBuilder sb = new StringBuilder();
            while (x > 0) {
                BigInteger r = x % 58;
                sb.Append(ALPHABET[(int)r]);
                x = x / 58;
            }

            char[] ca = sb.ToString().ToCharArray();
            Array.Reverse(ca);
            return new string(ca);
        }

        static void Main(string[] args) {
            string s = "25420294593250030202636073700053352635053786165627414518";
            string b = ConvertToBase58(s, 10);
            Console.WriteLine("{0} -> {1}", s, b);

            List<string> hashes = new List<string>() {
                "0x61",
                "0x626262",
                "0x636363",
                "0x73696d706c792061206c6f6e6720737472696e67",
                "0x516b6fcd0f",
                "0xbf4f89001e670274dd",
                "0x572e4794",
                "0xecac89cad93923c02321",
                "0x10c8511e",
            };
            foreach (string hash in hashes) {
                string b58 = ConvertToBase58(hash);
                Console.WriteLine("{0,-56} -> {1}", hash, b58);
            }
        }
    }
}
```

{{out}}

```txt
25420294593250030202636073700053352635053786165627414518 -> 6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
0x61                                                     -> 2g
0x626262                                                 -> a3gV
0x636363                                                 -> aPEr
0x73696d706c792061206c6f6e6720737472696e67               -> 2cFupjhnEsSn59qHXstmK2ffpLv2
0x516b6fcd0f                                             -> ABnLTmg
0xbf4f89001e670274dd                                     -> 3SEo3LWLoPntC
0x572e4794                                               -> 3EFU7m
0xecac89cad93923c02321                                   -> EJDM8drfXA6uyA
0x10c8511e                                               -> Rt5zm
```



## FreeBASIC

{{libheader|GMP}}

```freebasic
' version 14-08-2017
' compile with: fbc -s console
' uses GMP

#Include Once "gmp.bi"

Data "25420294593250030202636073700053352635053786165627414518" ' 6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
Data "0x61"                                                     ' 2g
Data "0x626262"                                                 ' a3gV
Data "0x636363"                                                 ' aPEr
Data "0x73696d706c792061206c6f6e6720737472696e67"               ' 2cFupjhnEsSn59qHXstmK2ffpLv2
Data "0x516b6fcd0f"                                             ' ABnLTmg
Data "0xbf4f89001e670274dd"                                     ' 3SEo3LWLoPntC
Data "0x572e4794"                                               ' 3EFU7m
Data "0xecac89cad93923c02321"                                   ' EJDM8drfXA6uyA
Data "0x10c8511e"                                               ' Rt5zm
Data ""

Function conv2base58(decimal As String, _base_ As Integer = 0) As String

    Dim As String convert
    Dim As String base58 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    Dim As String norm58 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuv"
    Dim As ZString Ptr gmp_str : gmp_str = Allocate(1000)
    Dim As Mpz_ptr tmp = Allocate(Len(__mpz_struct)) : Mpz_init(tmp)

    Mpz_set_str(tmp, decimal, _base_)
    Mpz_get_str(gmp_str, 58, tmp)

    convert = *gmp_str

    For i As uinteger = 0 To Len(convert) -1
        convert[i] = base58[InStr(norm58, Chr(convert[i])) -1]
    Next

    Mpz_clear(tmp) : DeAllocate(gmp_str)

    Return convert

End Function

' ------=< MAIN >=------
Dim As String str_in

Print "OkobppXBkab(58) --> "; conv2base58("OkobppXBkab", 58)  ' 10687460092462769069(10)
Print

Do
    Read str_in
    If str_in = "" Then Exit Do
    Print str_in;
    If Len(str_in) < 54 Then Print Tab(43);
    Print " --> "; conv2base58(str_in)
Loop

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
OkobppXBkab(58) --> RosettaCode

25420294593250030202636073700053352635053786165627414518 --> 6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
0x61                                       --> 2g
0x626262                                   --> a3gV
0x636363                                   --> aPEr
0x73696d706c792061206c6f6e6720737472696e67 --> 2cFupjhnEsSn59qHXstmK2ffpLv2
0x516b6fcd0f                               --> ABnLTmg
0xbf4f89001e670274dd                       --> 3SEo3LWLoPntC
0x572e4794                                 --> 3EFU7m
0xecac89cad93923c02321                     --> EJDM8drfXA6uyA
0x10c8511e                                 --> Rt5zm
```



## D


```D
import std.bigint;
import std.stdio;

void main() {
    report("25420294593250030202636073700053352635053786165627414518");
    report(0x61);
    report(0x626262);
    report(0x636363);
    report("0x73696d706c792061206c6f6e6720737472696e67");
    report(0x516b6fcd0f);
    report("0xbf4f89001e670274dd");
    report(0x572e4794);
    report("0xecac89cad93923c02321");
    report(0x10c8511e);
}

void report(T)(T v) {
    import std.traits;
    static if (isIntegral!T) {
        enum format = "%#56x -> %s";
    } else {
        enum format = "%56s -> %s";
    }
    writefln(format, v, v.toBase58);
}

string toBase58(T)(T input) {
    import std.traits;
    static if (isSomeString!T) {
        return toBase58(BigInt(input));
    } else {
        import std.algorithm.mutation : reverse;
        import std.array : appender;
        enum ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

        auto sb = appender!(char[]);
        size_t mod;

        do {
            mod = cast(size_t) (input % ALPHABET.length);
            sb.put(ALPHABET[mod]);

            input /= ALPHABET.length;
        } while (input);

        sb.data.reverse;
        return sb.data.idup;
    }
}
```

{{out}}

```txt
25420294593250030202636073700053352635053786165627414518 -> 6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
                                                    0x61 -> 2g
                                                0x626262 -> a3gV
                                                0x636363 -> aPEr
              0x73696d706c792061206c6f6e6720737472696e67 -> 2cFupjhnEsSn59qHXstmK2ffpLv2
                                            0x516b6fcd0f -> ABnLTmg
                                    0xbf4f89001e670274dd -> 3SEo3LWLoPntC
                                              0x572e4794 -> 3EFU7m
                                  0xecac89cad93923c02321 -> EJDM8drfXA6uyA
                                              0x10c8511e -> Rt5zm
```



## Go

{{trans|Kotlin}}

```go
package main

import (
    "fmt"
    "log"
    "math/big"
    "strings"
)

const alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

var big0 = new(big.Int)
var big58 = big.NewInt(58)

func reverse(s string) string {
    r := []rune(s)
    for i, j := 0, len(r)-1; i < len(r)/2; i, j = i+1, j-1 {
        r[i], r[j] = r[j], r[i]
    }
    return string(r)
}

func convertToBase58(hash string, base int) (string, error) {
    var x, ok = new(big.Int).SetString(hash, base)
    if !ok {
        return "", fmt.Errorf("'%v' is not a valid integer in base '%d'", hash, base)
    }
    var sb strings.Builder
    var rem = new(big.Int)
    for x.Cmp(big0) == 1 {
        x.QuoRem(x, big58, rem)
        r := rem.Int64()
        sb.WriteByte(alphabet[r])
    }
    return reverse(sb.String()), nil
}

func main() {
    s := "25420294593250030202636073700053352635053786165627414518"
    b, err := convertToBase58(s, 10)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(s, "->", b)
    hashes := [...]string{
        "0x61",
        "0x626262",
        "0x636363",
        "0x73696d706c792061206c6f6e6720737472696e67",
        "0x516b6fcd0f",
        "0xbf4f89001e670274dd",
        "0x572e4794",
        "0xecac89cad93923c02321",
        "0x10c8511e",
    }
    for _, hash := range hashes {
        b58, err := convertToBase58(hash, 0)
        if err != nil {
            log.Fatal(err)
        }
        fmt.Printf("%-56s -> %s\n", hash, b58)
    }
}
```


{{out}}

```txt

25420294593250030202636073700053352635053786165627414518 -> 6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
0x61                                                     -> 2g
0x626262                                                 -> a3gV
0x636363                                                 -> aPEr
0x73696d706c792061206c6f6e6720737472696e67               -> 2cFupjhnEsSn59qHXstmK2ffpLv2
0x516b6fcd0f                                             -> ABnLTmg
0xbf4f89001e670274dd                                     -> 3SEo3LWLoPntC
0x572e4794                                               -> 3EFU7m
0xecac89cad93923c02321                                   -> EJDM8drfXA6uyA
0x10c8511e                                               -> Rt5zm

```



## Haskell


```haskell
import Numeric (showIntAtBase)

chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

base58Encode :: Integer -> String
base58Encode n = showIntAtBase 58 (chars !!) n ""

main :: IO ()
main = mapM_ (putStrLn . base58Encode)
        [25420294593250030202636073700053352635053786165627414518,
         0x61,
         0x626262,
         0x636363,
         0x73696d706c792061206c6f6e6720737472696e67,
         0x516b6fcd0f,
         0xbf4f89001e670274dd,
         0x572e4794,
         0xecac89cad93923c02321,
         0x10c8511e]
```

{{out}}

```txt

6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
2g
a3gV
aPEr
2cFupjhnEsSn59qHXstmK2ffpLv2
ABnLTmg
3SEo3LWLoPntC
3EFU7m
EJDM8drfXA6uyA
Rt5zm
```



and for bulk encoding, Array access would be one of various slightly faster alternatives to recursive subscripting of linked lists:
{{Trans|Python}}

```Haskell
import Data.Array (Array, (!), listArray)
import Numeric (showHex, showIntAtBase)

base58Encode
  :: (Integral a, Show a)
  => a -> String
base58Encode =
  baseEncode $
  listArray (0, 57) $
  [('1', '9'), ('A', 'H'), ('J', 'N'), ('P', 'Z'), ('a', 'k'), ('m', 'z')] >>=
  uncurry enumFromTo

baseEncode
  :: (Show a, Integral a)
  => Array Int Char -> a -> String
baseEncode cs n = showIntAtBase (fromIntegral $ length cs) (cs !) n []

-- TEST ---------------------------------------------------
main :: IO ()
main =
  putStrLn $
  fTable
    "Base 58 encoding:\n"
    (\x -> '0' : 'x' : showHex x [])
    base58Encode
    id
    [ 25420294593250030202636073700053352635053786165627414518
    , 0x61
    , 0x626262
    , 0x636363
    , 0x73696d706c792061206c6f6e6720737472696e67
    , 0x516b6fcd0f
    , 0xbf4f89001e670274dd
    , 0x572e4794
    , 0xecac89cad93923c02321
    , 0x10c8511e
    ]

-- OUTPUT FORMATTING --------------------------------------
fTable :: String -> (a -> String) -> (b -> String) -> (a -> b) -> [a] -> String
fTable s xShow fxShow f xs =
  let w = maximum $ fmap length (xShow <$> xs)
      rjust n c s = drop (length s) (replicate n c ++ s)
  in unlines $
     s : fmap (((++) . rjust w ' ' . xShow) <*> ((" -> " ++) . fxShow . f)) xs
```

{{Out}}

```txt
Base 58 encoding:

0x10966776006953d5567439e5e39f86a0d273beed61967f6 -> 6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
                                             0x61 -> 2g
                                         0x626262 -> a3gV
                                         0x636363 -> aPEr
       0x73696d706c792061206c6f6e6720737472696e67 -> 2cFupjhnEsSn59qHXstmK2ffpLv2
                                     0x516b6fcd0f -> ABnLTmg
                             0xbf4f89001e670274dd -> 3SEo3LWLoPntC
                                       0x572e4794 -> 3EFU7m
                           0xecac89cad93923c02321 -> EJDM8drfXA6uyA
                                       0x10c8511e -> Rt5zm
```



## Java

{{trans|Kotlin}}
{{works with|Java|9}}

```Java
import java.math.BigInteger;
import java.util.List;

public class Base58CheckEncoding {
    private static final String ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";
    private static final BigInteger BIG0 = BigInteger.ZERO;
    private static final BigInteger BIG58 = BigInteger.valueOf(58);

    private static String convertToBase58(String hash) {
        return convertToBase58(hash, 16);
    }

    private static String convertToBase58(String hash, int base) {
        BigInteger x;
        if (base == 16 && hash.substring(0, 2).equals("0x")) {
            x = new BigInteger(hash.substring(2), 16);
        } else {
            x = new BigInteger(hash, base);
        }

        StringBuilder sb = new StringBuilder();
        while (x.compareTo(BIG0) > 0) {
            int r = x.mod(BIG58).intValue();
            sb.append(ALPHABET.charAt(r));
            x = x.divide(BIG58);
        }

        return sb.reverse().toString();
    }

    public static void main(String[] args) {
        String s = "25420294593250030202636073700053352635053786165627414518";
        String b = convertToBase58(s, 10);
        System.out.printf("%s -> %s\n", s, b);

        List<String> hashes = List.of(
            "0x61",
            "0x626262",
            "0x636363",
            "0x73696d706c792061206c6f6e6720737472696e67",
            "0x516b6fcd0f",
            "0xbf4f89001e670274dd",
            "0x572e4794",
            "0xecac89cad93923c02321",
            "0x10c8511e"
        );
        for (String hash : hashes) {
            String b58 = convertToBase58(hash);
            System.out.printf("%-56s -> %s\n", hash, b58);
        }
    }
}
```

{{out}}

```txt
25420294593250030202636073700053352635053786165627414518 -> 6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
0x61                                                     -> 2g
0x626262                                                 -> a3gV
0x636363                                                 -> aPEr
0x73696d706c792061206c6f6e6720737472696e67               -> 2cFupjhnEsSn59qHXstmK2ffpLv2
0x516b6fcd0f                                             -> ABnLTmg
0xbf4f89001e670274dd                                     -> 3SEo3LWLoPntC
0x572e4794                                               -> 3EFU7m
0xecac89cad93923c02321                                   -> EJDM8drfXA6uyA
0x10c8511e                                               -> Rt5zm
```



## Julia

{{works with|Julia|0.6}}


```julia
const alpha = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

function encodebase58(hsh::AbstractString, base::Integer=16)
    x = if base == 16 && hsh[1:2] == "0x" parse(BigInt, hsh[3:end], 16)
        else parse(BigInt, hsh, base) end
    sb = IOBuffer()
    while x > 0
        x, r = divrem(x, 58)
        print(sb, alpha[r + 1])
    end
    return String(sb) |> reverse
end

s = "25420294593250030202636073700053352635053786165627414518"
println("# $s\n -> ", encodebase58(s, 10))
for s in ["0x61", "0x626262", "0x636363", "0x73696d706c792061206c6f6e6720737472696e67",
          "0x516b6fcd0f", "0xbf4f89001e670274dd", "0x572e4794", "0xecac89cad93923c02321",
          "0x10c8511e"]
    println("# $s\n -> ", encodebase58(s))
end
```


{{out}}

```txt
# 25420294593250030202636073700053352635053786165627414518
 -> 6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
# 0x61
 -> 2g
# 0x626262
 -> a3gV
# 0x636363
 -> aPEr
# 0x73696d706c792061206c6f6e6720737472696e67
 -> 2cFupjhnEsSn59qHXstmK2ffpLv2
# 0x516b6fcd0f
 -> ABnLTmg
# 0xbf4f89001e670274dd
 -> 3SEo3LWLoPntC
# 0x572e4794
 -> 3EFU7m
# 0xecac89cad93923c02321
 -> EJDM8drfXA6uyA
# 0x10c8511e
 -> Rt5zm
```



## Kotlin


```scala
// version 1.1.51

import java.math.BigInteger

const val ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
val big0  = BigInteger.ZERO
val big58 = BigInteger.valueOf(58L)

fun convertToBase58(hash: String, base: Int = 16): String {
    var x = if (base == 16 && hash.take(2) == "0x") BigInteger(hash.drop(2), 16)
            else BigInteger(hash, base)
    val sb = StringBuilder()
    while (x > big0) {
        val r = (x % big58).toInt()
        sb.append(ALPHABET[r])
        x = x / big58
    }
    return sb.toString().reversed()
}

fun main(args: Array<String>) {
    val s = "25420294593250030202636073700053352635053786165627414518"
    val b = convertToBase58(s, 10)
    println("$s -> $b")
    val hashes = listOf(
        "0x61",
        "0x626262",
        "0x636363",
        "0x73696d706c792061206c6f6e6720737472696e67",
        "0x516b6fcd0f",
        "0xbf4f89001e670274dd",
        "0x572e4794",
        "0xecac89cad93923c02321",
        "0x10c8511e"
    )
    for (hash in hashes) {
        val b58 = convertToBase58(hash)
        println("${hash.padEnd(56)} -> $b58")
    }
}
```


{{out}}

```txt

25420294593250030202636073700053352635053786165627414518 -> 6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
0x61                                                     -> 2g
0x626262                                                 -> a3gV
0x636363                                                 -> aPEr
0x73696d706c792061206c6f6e6720737472696e67               -> 2cFupjhnEsSn59qHXstmK2ffpLv2
0x516b6fcd0f                                             -> ABnLTmg
0xbf4f89001e670274dd                                     -> 3SEo3LWLoPntC
0x572e4794                                               -> 3EFU7m
0xecac89cad93923c02321                                   -> EJDM8drfXA6uyA
0x10c8511e                                               -> Rt5zm

```



## Perl


```perl
use Math::BigInt;

sub encode_base58 {
    my ($num) = @_;
    $num = Math::BigInt->new($num);

    my $chars = [qw(
    1 2 3 4 5 6 7 8 9
    A B C D E F G H   J K L M N   P Q R S T U V W X Y Z
    a b c d e f g h i j k   m n o p q r s t u v w x y z
    )];

    my $base58;
    while ($num->is_pos) {
        my ($quotient, $remainder) = $num->bdiv(58);
        $base58 = $chars->[$remainder] . $base58;
    }
    $base58
}

printf "%56s -> %s\n", $_, encode_base58(+$_)
    for qw(
     25420294593250030202636073700053352635053786165627414518
     0x61
     0x626262
     0x636363
     0x73696d706c792061206c6f6e6720737472696e67
     0x516b6fcd0f
     0xbf4f89001e670274dd
     0x572e4794
     0xecac89cad93923c02321
     0x10c8511e
    );
```

{{out}}

```txt
25420294593250030202636073700053352635053786165627414518 -> 6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
                                                    0x61 -> 2g
                                                0x626262 -> a3gV
                                                0x636363 -> aPEr
              0x73696d706c792061206c6f6e6720737472696e67 -> 2cFupjhnEsSn59qHXstmK2ffpLv2
                                            0x516b6fcd0f -> ABnLTmg
                                    0xbf4f89001e670274dd -> 3SEo3LWLoPntC
                                              0x572e4794 -> 3EFU7m
                                  0xecac89cad93923c02321 -> EJDM8drfXA6uyA
                                              0x10c8511e -> Rt5zm
```



## Perl 6


```perl6
sub encode_Base58 ( Int $x ) {
    constant @codes = <
          1 2 3 4 5 6 7 8 9
        A B C D E F G H   J K L M N   P Q R S T U V W X Y Z
        a b c d e f g h i j k   m n o p q r s t u v w x y z
    >;

    return @codes[ $x.polymod( 58 xx * ) ].join.flip;
}

my @tests =
    25420294593250030202636073700053352635053786165627414518 => '6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM',
    0x61                    => '2g',
    0x626262                => 'a3gV',
    0x636363                => 'aPEr',
    0x73696d706c792061206c6f6e6720737472696e67 => '2cFupjhnEsSn59qHXstmK2ffpLv2',
    0x516b6fcd0f            => 'ABnLTmg',
    0xbf4f89001e670274dd    => '3SEo3LWLoPntC',
    0x572e4794              => '3EFU7m',
    0xecac89cad93923c02321  => 'EJDM8drfXA6uyA',
    0x10c8511e              => 'Rt5zm',
;
use Test;
for @tests {
    is encode_Base58(.key), .value, "{.key} encodes to {.value}";
}

```



## Phix

Slight variation from [[Bitcoin/public_point_to_address#Phix]] in that it accepts any length string (which can be binary or text).

Includes leading zeroes, if you don't want that just comment out the three lines defining/using the integer lz.

```Phix
constant b58 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

function base58(string s)
string out = ""
integer lz = length(s)
    while 1 do
        s = trim_head(s,'\0')
        if length(s)=0 then exit end if
        if out="" then lz -= length(s) end if
        integer c = 0
        for i=1 to length(s) do
            c = c*256+s[i]
            s[i] = floor(c/58)
            c = mod(c,58)
        end for
        out &= b58[c+1]
    end while
    out &= repeat('1',lz)
    return reverse(out)
end function

?base58(x"00010966776006953D5567439E5E39F86A0D273BEED61967F6")
?base58(x"61")      -- == base58("a")
?base58(x"626262")  -- == base58("bbb")
?base58(x"636363")  -- == base58("ccc")
?base58(x"73696d706c792061206c6f6e6720737472696e67")
 -- ^ == base58("simply a long string")
?base58(x"516b6fcd0f")
?base58(x"bf4f89001e670274dd")
?base58(x"572e4794")
?base58(x"ecac89cad93923c02321")
?base58(x"10c8511e")
```

{{out}}

```txt

"16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM"
"2g"
"a3gV"
"aPEr"
"2cFupjhnEsSn59qHXstmK2ffpLv2"
"ABnLTmg"
"3SEo3LWLoPntC"
"3EFU7m"
"EJDM8drfXA6uyA"
"Rt5zm"

```



## PicoLisp


```PicoLisp
(setq *B58Alpha
   (chop "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz") )
(de b58 (S)
   (let N 0
      (pack
         (make
            (if (pre? "0x" S)
               (setq N (hex (cddr (chop S))))
               (setq N (format S)) )
            (while (gt0 N)
               (yoke (get *B58Alpha (inc (% N 58))))
               (setq N (/ N 58)) ) ) ) ) )
(println (b58 "25420294593250030202636073700053352635053786165627414518"))
(println (b58 "0x626262"))
(println (b58 "0x636363"))
```

{{out}}

```txt

"6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM"
"a3gV"
"aPEr"

```



## Python


### Procedural

{{trans|C#}}

```python
ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

def convertToBase58(num):
    sb = ''
    while (num > 0):
        r = num % 58
        sb = sb + ALPHABET[r]
        num = num / 58;
    return sb[::-1]

s = 25420294593250030202636073700053352635053786165627414518
b = convertToBase58(s)
print "%-56d -> %s" % (s, b)

hash_arr = [0x61, 0x626262, 0x636363, 0x73696d706c792061206c6f6e6720737472696e67, 0x516b6fcd0f, 0xbf4f89001e670274dd, 0x572e4794, 0xecac89cad93923c02321, 0x10c8511e]
for num in hash_arr:
    b = convertToBase58(num)
    print "0x%-54x -> %s" % (num, b)
```

{{out}}

```txt
25420294593250030202636073700053352635053786165627414518 -> 6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
0x61                                                     -> 2g
0x626262                                                 -> a3gV
0x636363                                                 -> aPEr
0x73696d706c792061206c6f6e6720737472696e67               -> 2cFupjhnEsSn59qHXstmK2ffpLv2
0x516b6fcd0f                                             -> ABnLTmg
0xbf4f89001e670274dd                                     -> 3SEo3LWLoPntC
0x572e4794                                               -> 3EFU7m
0xecac89cad93923c02321                                   -> EJDM8drfXA6uyA
0x10c8511e                                               -> Rt5zm
```



### Composition of pure functions

{{Works with|Python|3.7}}

```python
'''Base 58 check encoding'''

from functools import reduce
import itertools
import enum


# baseEncode :: [Char] -> (Integer -> String)
def baseEncode(cs):
    '''Given the character set for a given base,
       returns a function from a integer to a string
       representing that integer in the base
       specified by the length of the character set.
    '''
    return lambda n: showIntAtBase(len(cs))(
        index(cs)
    )(n)('')


# TESTS ---------------------------------------------------
# main :: IO ()
def main():
    '''Tests of base58 encoding.'''

    # base58Encode :: Integer -> String
    base58Encode = baseEncode(
        reduce(
            lambda a, xy: a + uncurry(enumFromTo)(xy),
            [
                ('1', '9'),
                ('A', 'H'), ('J', 'N'), ('P', 'Z'),
                ('a', 'k'), ('m', 'z')
            ],
            []
        )
    )

    print(
        fTable(__doc__ + ':\n')(hex)(base58Encode)(stet)([
            25420294593250030202636073700053352635053786165627414518,
            0x61,
            0x626262,
            0x636363,
            0x73696d706c792061206c6f6e6720737472696e67,
            0x516b6fcd0f,
            0xbf4f89001e670274dd,
            0x572e4794,
            0xecac89cad93923c02321,
            0x10c8511e
        ])
    )


# GENERIC -------------------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# enumFromTo :: Enum a => a -> a -> [a]
def enumFromTo(m):
    '''Enumeration of values [m..n]'''
    def go(x, y):
        t = type(m)
        i = fromEnum(x)
        d = 0 if t != float else (x - i)
        return list(map(
            lambda x: toEnum(t)(d + x),
            range(i, 1 + fromEnum(y))
        ) if int != t else range(x, 1 + y))
    return lambda n: go(m, n)


# fromEnum :: Enum a => a -> Int
def fromEnum(x):
    '''Index integer for enumerable value.'''
    Enum = enum.Enum
    return ord(x) if str == type(x) else (
        x.value if isinstance(x, Enum) else int(x)
    )


# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function ->
                 fx display function ->
          f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(compose(len)(xShow), xs))
        return s + '\n' + '\n'.join([
            xShow(x).rjust(w, ' ') + (' -> ') + fxShow(f(x))
            for x in xs
        ])
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# index (!!) :: [a] -> Int -> a
def index(xs):
    '''Item at given (zero-based) index.'''
    islice = itertools.islice
    return lambda n: None if 0 > n else (
        xs[n] if (
            hasattr(xs, "__getitem__")
        ) else next(islice(xs, n, None))
    )


# showIntAtBase :: Int -> (Int -> String) -> Int -> String -> String
def showIntAtBase(base):
    '''String representation of an integer in a given base,
       using a supplied function for the string representation
       of digits.'''
    def wrap(toChr, n, rs):
        def go(nd, r):
            n, d = nd
            r_ = toChr(d) + r
            return go(divmod(n, base), r_) if 0 != n else r_
        return 'unsupported base' if 1 >= base else (
            'negative number' if 0 > n else (
                go(divmod(n, base), rs))
        )
    return lambda toChr: lambda n: lambda rs: (
        wrap(toChr, n, rs)
    )


# stet :: a -> a
def stet(x):
    '''The identity function.
       The usual 'id' is reserved in Python.'''
    return x


# uncurry :: (a -> b -> c) -> ((a, b) -> c)
def uncurry(f):
    '''A function over a tuple,
       derived from a default or
       curried function.'''
    return lambda xy: f(xy[0])(xy[1])


# toEnum :: Type -> Int -> a
def toEnum(t):
    '''Enumerable value from index integer'''
    dct = {
        int: int,
        float: float,
        str: chr,
        bool: bool
    }
    return lambda x: dct[t](x) if t in dct else t(x)


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Base 58 check encoding:

0x10966776006953d5567439e5e39f86a0d273beed61967f6 -> 6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
                                             0x61 -> 2g
                                         0x626262 -> a3gV
                                         0x636363 -> aPEr
       0x73696d706c792061206c6f6e6720737472696e67 -> 2cFupjhnEsSn59qHXstmK2ffpLv2
                                     0x516b6fcd0f -> ABnLTmg
                             0xbf4f89001e670274dd -> 3SEo3LWLoPntC
                                       0x572e4794 -> 3EFU7m
                           0xecac89cad93923c02321 -> EJDM8drfXA6uyA
                                       0x10c8511e -> Rt5zm
```



## Racket

(Examples from some other task)

```racket
#lang racket

(define ((base-n-alphabet-encode alphabet) hash-string (in-base 16))
  (define out-base (string-length alphabet))
  (let reduce-hash ((h (string->number (if (and (= in-base 16) (string-prefix? hash-string "0x"))
                                           (substring hash-string 2)
                                           hash-string)
                                       in-base))
                    (acc (list)))
    (if (zero? h)
        (list->string acc)
        (let-values (((q r) (quotient/remainder h out-base)))
          (reduce-hash q (cons (string-ref alphabet r) acc))))))

(define base58-check-encode (base-n-alphabet-encode "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"))

(module+ main
  (base58-check-encode "25420294593250030202636073700053352635053786165627414518" 10)
  (map base58-check-encode (list "0x61"
                                 "0x626262"
                                 "0x636363"
                                 "0x73696d706c792061206c6f6e6720737472696e67"
                                 "0x516b6fcd0f"
                                 "0xbf4f89001e670274dd"
                                 "0x572e4794"
                                 "0xecac89cad93923c02321"
                                 "0x10c8511e")))
```

{{out}}

```txt
"6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM"
'("2g" "a3gV" "aPEr" "2cFupjhnEsSn59qHXstmK2ffpLv2" "ABnLTmg" "3SEo3LWLoPntC" "3EFU7m" "EJDM8drfXA6uyA" "Rt5zm")
```



## REXX


### version 1

Following the description in
https://www.anintegratedworld.com/how-to-manually-calculate-base58check-encoding/
I get the result expected there.
Apart for the leading 1 the program works also for the inputs shown above.

```rexx
/* REXX */
s="123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
Numeric Digits 100
k='00010966776006953D5567439E5E39F86A0D273BEED61967F6'x
n=c2d(k)
o=''
Do Until n=0
  rem=n//58
  n=n%58
  o=o||substr(s,rem+1,1)
  End
o=o||substr(s,1,1)
Say reverse(o)
```

{{out}}

```txt
16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
```


### version 2

does what the others do

```rexx
/* REXX */
s="123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
Numeric Digits 1000
cnt_ok=0
Call test 'N',25420294593250030202636073700053352635053786165627414518,,
             '6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM'
Call test 'X','61'x                                      ,'2g'
Call test 'X','626262'x                                  ,'a3gV'
Call test 'X','636363'x                                  ,'aPEr'
Call test 'X','73696d706c792061206c6f6e6720737472696e67'x,,
              '2cFupjhnEsSn59qHXstmK2ffpLv2'
Call test 'X','516b6fcd0f'x                              ,'ABnLTmg'
Call test 'X','bf4f89001e670274dd'x                      ,'3SEo3LWLoPntC'
Call test 'X','572e4794'x                                ,'3EFU7m'
Call test 'X','ecac89cad93923c02321'x                    ,'EJDM8drfXA6uyA'
Call test 'X','10c8511e'x                                ,'Rt5zm'
Call test 'X','10c8511e'x                                ,'check_error_handlimng'
Say cnt_ok 'tests ok'
Exit
test:
  Parse Arg how,k,res
  If how='X' Then
    k=c2d(k)
  o=''
  Do Until k=0
    rem=k//58
    k=k%58
    o=o||substr(s,rem+1,1)
    End
  o=reverse(o)
  If o=res Then cnt_ok+=1
  Else Do
    Say 'expected:' res
    Say 'found   :' o
    End
  Return
```

{{out}}

```txt
expected: check_error_handlimng
found   : Rt5zm
10 tests ok
```



### version 3

This REXX version handles a null input.

It also handles the case of the hash digest that contain leading 1's (ones)   which are translated to leading 0's (zeros).

The algorithm used doesn't need to   ''reverse''   the residual string   (it uses   ''prepend''   instead of   ''append'').

```rexx
/*REXX pgm encodes a checksum (hash digest) into Base58 (the standard Bitcoin alphabet).*/
call B58   25420294593250030202636073700053352635053786165627414518
call B58  '61'x
call B58  '626262'x
call B58  '636363'x
call B58  '73696d706c792061206c6f6e6720737472696e67'x
call B58  '516b6fcd0f'x
call B58  'bf4f89001e670274dd'x
call B58  '572e4794'x
call B58  'ecac89cad93923c02321'x
call B58  '10c8511e'x
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
B58: parse arg z 1 oz;      L1=0;         hx=0;   numeric digits 500    /*for huge nums.*/
     if z=''  then return                                               /*Is Z missing? */
     if \datatype(z, 'W') | arg()>1  then hx=1;   if hx  then z=c2d(z)  /*is Z in hex ? */
     if left(z, 1)==1  then L1=verify(z ., 1) -1 /*count number of leading 1's  (ones). */
     /*       0─────────────────I─────O────────────────────l────────────────   ◄───omit.*/
     @=space(" 123456789ABCDEFGH JKLMN PQRSTUVWXYZabcdefghi jkmnopqrstuvwxyz",  0)
     $=
                                    do until z=0;   $=substr(@, z//58 +1, 1)$;    z=z % 58
                                    end  /*until*/
     if hx  then oz="'"c2x(oz)"'x"               /*Original arg in hex?  Then transform.*/
     say right(oz, 60) '───►'  left('', L1, 0)$  /*display given argument & the residual*/
     return                                      /* [↑]  also prepend residual with 1's.*/
```

{{out|output}}

```txt

    25420294593250030202636073700053352635053786165627414518 ───► 6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
                                                       '61'x ───► 2g
                                                   '626262'x ───► a3gV
                                                   '636363'x ───► aPEr
                 '73696D706C792061206C6F6E6720737472696E67'x ───► 2cFupjhnEsSn59qHXstmK2ffpLv2
                                               '516B6FCD0F'x ───► ABnLTmg
                                       'BF4F89001E670274DD'x ───► 3SEo3LWLoPntC
                                                 '572E4794'x ───► 03EFU7m
                                     'ECAC89CAD93923C02321'x ───► 000EJDM8drfXA6uyA
                                                 '10C8511E'x ───► Rt5zm

```



## Ruby


```ruby
ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
nums =  [25420294593250030202636073700053352635053786165627414518,
         0x61,
         0x626262,
         0x636363,
         0x73696d706c792061206c6f6e6720737472696e67,
         0x516b6fcd0f,
         0xbf4f89001e670274dd,
         0x572e4794,
         0xecac89cad93923c02321,
         0x10c8511e]

puts nums.map{|n| n.digits(58).reverse.map{|i| ALPHABET[i]}.join}

```

{{out}}

```txt
6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
2g
a3gV
aPEr
2cFupjhnEsSn59qHXstmK2ffpLv2
ABnLTmg
3SEo3LWLoPntC
3EFU7m
EJDM8drfXA6uyA
Rt5zm

```



## Scala

{{Out}}Best seen in running your browser either by [https://scalafiddle.io/sf/GMcrlBB/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org Scastie (remote JVM)].

```Scala
import java.math.BigInteger

object Base58 extends App {
  private val codeString = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  private val (big0, big58) = (BigInt(0), BigInteger.valueOf(58))

  def convertToBase58(input: String): String = convertToBase58(input, 16)

  def convertToBase58(input: String, base: Int) = {
    if (input.isEmpty) "" else {
      val big =
        if (base == 16 && input.startsWith("0x")) BigInt(input.substring(2), 16) else BigInt(input, base)

      @scala.annotation.tailrec
      def encode(current: BigInt, sb: StringBuilder): StringBuilder = current match {
        case `big0` => sb
        case _ =>
          val Array(dividend, remainder: BigInteger) = current.bigInteger.divideAndRemainder(big58)
          encode(dividend, sb.append(codeString(remainder.intValue)))
      }

      encode(big, new StringBuilder).reverse.toString
    }
  }

  private def decode(input: String): Array[Byte] = {
    val (mapping, trimmed)= (codeString.zipWithIndex.toMap, input.dropWhile(_ == '1').toList)

    def zeroes: Array[Byte] = input.takeWhile(_ == '1').map(_ => 0.toByte).toArray
    def decoded: BigInt = trimmed.foldLeft(big0)((a, b) => a * big58 + BigInt(mapping(b)))

    if (trimmed.nonEmpty) zeroes ++ decoded.toByteArray.dropWhile(_ == 0) else zeroes
  }

  private def bytes2Hex(buf: Array[Byte]): String = "0x" + buf.map("%02x" format _).mkString

  /*
   * Running some test examples.
   */

  private val veryLongNumber = "25420294593250030202636073700053352635053786165627414518"
  println(f"$veryLongNumber%-56s -> ${convertToBase58(veryLongNumber, 10)}%s" )

  private val hashes = List("0x61", "0x626262", "0x636363", "0x73696d706c792061206c6f6e6720737472696e67",
    "0x516b6fcd0f", "0xbf4f89001e670274dd", "0x572e4794", "0xecac89cad93923c02321", "0x10c8511e")

  for (hash <- hashes) {
    val b58: String = convertToBase58(hash)

    println(f"$hash%-56s -> $b58%-28s -> ${bytes2Hex(decode(b58))}%-56s" )
  }

}
```


## Seed7

The Seed7 library [http://seed7.sourceforge.net/libraries/encoding.htm encoding.s7i] defines
the function [http://seed7.sourceforge.net/libraries/encoding.htm#toBase(in_bigInteger,in_string) toBase],
which encodes a number with a positional numeric system. No external library is needed.

```seed7
$ include "seed7_05.s7i";
  include "encoding.s7i";

const proc: main is func
  local
    const bigInteger: s is 25420294593250030202636073700053352635053786165627414518_;
    const array bigInteger: hash_arr is [] (16#61_, 16#626262_, 16#636363_, 16#73696d706c792061206c6f6e6720737472696e67_,
        16#516b6fcd0f_, 16#bf4f89001e670274dd_, 16#572e4794_, 16#ecac89cad93923c02321_, 16#10c8511e_);
    var string: b is "";
    var bigInteger: num is 0_;
  begin
    b := toBase(s, defaultBase58Digits);
    writeln(s rpad 56 <& " -> " <& b);
    for num range hash_arr do
      b := toBase(num, defaultBase58Digits);
      writeln("16#" <& num radix 16 rpad 53 <& " -> " <& b);
    end for;
  end func;
```


{{out}}

```txt
25420294593250030202636073700053352635053786165627414518 -> 6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
16#61                                                    -> 2g
16#626262                                                -> a3gV
16#636363                                                -> aPEr
16#73696d706c792061206c6f6e6720737472696e67              -> 2cFupjhnEsSn59qHXstmK2ffpLv2
16#516b6fcd0f                                            -> ABnLTmg
16#bf4f89001e670274dd                                    -> 3SEo3LWLoPntC
16#572e4794                                              -> 3EFU7m
16#ecac89cad93923c02321                                  -> EJDM8drfXA6uyA
16#10c8511e                                              -> Rt5zm
```



## Sidef

{{trans|Perl 6}}

```ruby
func encode_base58(n) {
    static chars = %w(
        1 2 3 4 5 6 7 8 9
        A B C D E F G H   J K L M N   P Q R S T U V W X Y Z
        a b c d e f g h i j k   m n o p q r s t u v w x y z
    )
    [chars[n.polymod(n.ilog(58).of(58)...)]].join.flip
}

var tests = [
    [25420294593250030202636073700053352635053786165627414518, "6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM"],
    [97, "2g"],[6447714, "a3gV"],[6513507, "aPEr"],
    [658885050385564465925592505944209249682185612903, "2cFupjhnEsSn59qHXstmK2ffpLv2"],
    [349694840079, "ABnLTmg"], [3529059230209907258589, "3SEo3LWLoPntC"],
    [1462650772, "3EFU7m"], [1117661258925082241147681, "EJDM8drfXA6uyA"], [281563422, "Rt5zm"]
]

for num, enc in (tests) {
    printf("%56s -> %s\n", num, encode_base58(num))
    assert_eq(encode_base58(num), enc)
}
```

{{out}}

```txt

25420294593250030202636073700053352635053786165627414518 -> 6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
                                                      97 -> 2g
                                                 6447714 -> a3gV
                                                 6513507 -> aPEr
        658885050385564465925592505944209249682185612903 -> 2cFupjhnEsSn59qHXstmK2ffpLv2
                                            349694840079 -> ABnLTmg
                                  3529059230209907258589 -> 3SEo3LWLoPntC
                                              1462650772 -> 3EFU7m
                               1117661258925082241147681 -> EJDM8drfXA6uyA
                                               281563422 -> Rt5zm

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.Numerics
Imports System.Text

Module Module1
    ReadOnly ALPHABET As String = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    ReadOnly HEX As String = "0123456789ABCDEF"

    Function ToBigInteger(value As String, base As Integer) As BigInteger
        If base < 1 OrElse base > HEX.Length Then
            Throw New ArgumentException("Base is out of range.")
        End If

        Dim bi = BigInteger.Zero
        For Each c In value
            Dim c2 = Char.ToUpper(c)
            Dim idx = HEX.IndexOf(c2)
            If idx = -1 OrElse idx >= base Then
                Throw New ArgumentException("Illegal character encountered.")
            End If
            bi = bi * base + idx
        Next

        Return bi
    End Function

    Function ConvertToBase58(hash As String, Optional base As Integer = 16) As String
        Dim x As BigInteger
        If base = 16 AndAlso hash.Substring(0, 2) = "0x" Then
            x = ToBigInteger(hash.Substring(2), base)
        Else
            x = ToBigInteger(hash, base)
        End If

        Dim sb As New StringBuilder
        While x > 0
            Dim r = x Mod 58
            sb.Append(ALPHABET(r))
            x = x / 58
        End While

        Dim ca = sb.ToString().ToCharArray()
        Array.Reverse(ca)
        Return New String(ca)
    End Function

    Sub Main()
        Dim s = "25420294593250030202636073700053352635053786165627414518"
        Dim b = ConvertToBase58(s, 10)
        Console.WriteLine("{0} -> {1}", s, b)

        Dim hashes = {"0x61", "0x626262", "0x636363", "0x73696d706c792061206c6f6e6720737472696e67", "0x516b6fcd0f", "0xbf4f89001e670274dd", "0x572e4794", "0xecac89cad93923c02321", "0x10c8511e"}
        For Each hash In hashes
            Dim b58 = ConvertToBase58(hash)
            Console.WriteLine("{0,-56} -> {1}", hash, b58)
        Next
    End Sub

End Module
```

{{out}}

```txt
25420294593250030202636073700053352635053786165627414518 -> 6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
0x61                                                     -> 2g
0x626262                                                 -> a3gV
0x636363                                                 -> aPEr
0x73696d706c792061206c6f6e6720737472696e67               -> 2cFupjhnEsSn59qHXstmK2ffpLv2
0x516b6fcd0f                                             -> ABnLTmg
0xbf4f89001e670274dd                                     -> 3SEo3LWLoPntC
0x572e4794                                               -> 3EFU7m
0xecac89cad93923c02321                                   -> EJDM8drfXA6uyA
0x10c8511e                                               -> Rt5zm
```



## zkl

Uses libGMP

```zkl
var [const] BN=Import.lib("zklBigNum"), // GNU Multiple Precision Arithmetic Library
   src="0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuv",
   dst="123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

println("OkobppXBkab(58)-->",  # 10687460092462769069(10)
	"OkobppXBkab".translate(src,dst),"\n");

ns:=T(BN("25420294593250030202636073700053352635053786165627414518"),
      0x61, 0x626262, 0x636363,
      "73696d706c792061206c6f6e6720737472696e67",
      0x516b6fcd0f, "bf4f89001e670274dd", 0x572e4794,
      "ecac89cad93923c02321", 0x10c8511e);
ns.pump(Console.println,'wrap(n){ BN(n,16).toString(58).translate(src,dst) });
```

{{out}}

```txt

OkobppXBkab(58)-->RosettaCode

6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
2g
a3gV
aPEr
2cFupjhnEsSn59qHXstmK2ffpLv2
ABnLTmg
3SEo3LWLoPntC
3EFU7m
EJDM8drfXA6uyA
Rt5zm

```

