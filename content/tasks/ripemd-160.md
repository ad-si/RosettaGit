+++
title = "RIPEMD-160"
description = ""
date = 2019-05-08T07:27:41Z
aliases = []
[extra]
id = 11608
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "clojure",
  "common_lisp",
  "csharp",
  "d",
  "factor",
  "freebasic",
  "go",
  "haskell",
  "java",
  "julia",
  "kotlin",
  "lasso",
  "lua",
  "m2000_interpreter",
  "mathematica",
  "nim",
  "objeck",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "python",
  "racket",
  "ruby",
  "scala",
  "seed7",
  "swift",
  "tcl",
  "zkl",
]
+++

## Task

{{task}} [[Category:Checksums]]
'''RIPEMD-160''' is another hash function; it computes a 160-bit message digest.

There is a [http://homes.esat.kuleuven.be/~bosselae/ripemd160.html RIPEMD-160 home page], with test vectors and [http://www.esat.kuleuven.be/~bosselae/ripemd/rmd160.txt pseudocode for RIPEMD-160].
For padding the message, RIPEMD-160 acts like [[MD4]] (RFC 1320).

Find the RIPEMD-160 message digest of a string of [[octet]]s.
Use the ASCII encoded string “<tt>Rosetta Code</tt>”.
You may either call an RIPEMD-160 library, or implement RIPEMD-160 in your language.

## C#

```c#
using System;
using System.Security.Cryptography;
using System.Text;

class Program
{
    static void Main(string[] args)
    {
        string text = "Rosetta Code";
        byte[] bytes = Encoding.ASCII.GetBytes(text);
        RIPEMD160 myRIPEMD160 = RIPEMD160Managed.Create();
        byte[] hashValue = myRIPEMD160.ComputeHash(bytes);
        var hexdigest = BitConverter.ToString(hashValue).Replace("-", "").ToLower();
        Console.WriteLine(hexdigest);
        Console.ReadLine();
    }
}
```

```txt
b3be159860842cebaa7174c8fff0aa9e50a5199f
```



## Clojure

```clojure
(use 'pandect.core)
(ripemd160 "Rosetta Code")
```


```txt
"b3be159860842cebaa7174c8fff0aa9e50a5199f"
```



## Common Lisp

```lisp
(ql:quickload 'ironclad)
(defun string-to-ripemd-160 (str)
  "Return the RIPEMD-160 digest of the given ASCII string."
  (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence :ripemd-160
                              (ironclad:ascii-string-to-byte-array str)))

(string-to-ripemd-160 "Rosetta Code")
```


```txt
"b3be159860842cebaa7174c8fff0aa9e50a5199f"
```



## D


```d
void main() {
    import std.stdio, std.digest.ripemd;

    writefln("%(%02x%)", "Rosetta Code".ripemd160Of);
}
```

```txt
b3be159860842cebaa7174c8fff0aa9e50a5199f
```



## Factor

```factor
USING: checksums checksums.ripemd io math.parser ;

"Rosetta Code" ripemd-160 checksum-bytes bytes>hex-string print
```

```txt

b3be159860842cebaa7174c8fff0aa9e50a5199f

```



## FreeBASIC


```freebasic
' version 22-10-2016
' compile with: fbc -s console

Function RIPEMD_160(message As String) As String

  #Macro ROtate_left(x, n)
    (x Shl n Or x Shr (32 - n))
  #EndMacro

  #Macro    f1(x, y, z)
    (x Xor y Xor z)               ' (0 <= j <= 15)
  #EndMacro

  #Macro    f2(x, y, z)
    ((x And y) Or ((Not x) And z)) ' (16 <= j <= 31)
  #EndMacro

  #Macro    f3(x, y, z)
    ((x Or (Not y)) Xor z)          ''(32 <= j <= 47)
  #EndMacro

  #Macro    f4(x, y, z)
    ((x And z) Or (y And (Not z)))  ''(48 <= j <= 63)
  #EndMacro

  #Macro   f5(x, y, z)
    (x Xor (y Or (Not z)))          ''(64 <= j <= 79)
  #EndMacro

  Dim As UInteger<32> K(1 To 5), K1(1 To 5)

  K(1)  = &H00000000    '  (0 <= j <= 15)
  K(2)  = &H5A827999    ' (16 <= j <= 31)
  K(3)  = &H6ED9EBA1    ' (32 <= j <= 47)
  K(4)  = &H8F1BBCDC    ' (48 <= j <= 63)
  K(5)  = &HA953FD4E    ' (64 <= j <= 79)
  K1(1) = &H50A28BE6    '  (0 <= j <= 15)
  K1(2) = &H5C4DD124    ' (16 <= j <= 31)
  K1(3) = &H6D703EF3    ' (32 <= j <= 47)
  K1(4) = &H7A6D76E9    ' (48 <= j <= 63)
  K1(5) = &H00000000    ' (64 <= j <= 79)

  Dim As UByte r(16 To ...) = _
  { 7, 4, 13, 1, 10, 6, 15, 3, 12, 0, 9, 5, 2, 14, 11, 8, _
    3, 10, 14, 4, 9, 15, 8, 1, 2, 7, 0, 6, 13, 11, 5, 12, _
    1, 9, 11, 10, 0, 8, 12, 4, 13, 3, 7, 15, 14, 5, 6, 2, _
    4, 0, 5, 9, 7, 12, 2, 10, 14, 1, 3, 8, 11, 6, 15, 13 }

  Dim As UByte r1(0 To ...) = _
  { 5, 14, 7, 0, 9, 2, 11, 4, 13, 6, 15, 8, 1, 10, 3, 12, _
    6, 11, 3, 7, 0, 13, 5, 10, 14, 15, 8, 12, 4, 9, 1, 2, _
    15, 5, 1, 3, 7, 14, 6, 9, 11, 8, 12, 2, 10, 0, 4, 13, _
    8, 6, 4, 1, 3, 11, 15, 0, 5, 12, 2, 13, 9, 7, 10, 14, _
    12, 15, 10, 4, 1, 5, 8, 7, 6, 2, 13, 14, 0, 3, 9, 11 }

  Dim As UByte s(0 To ...) = _
  { 11, 14, 15, 12, 5, 8, 7, 9, 11, 13, 14, 15, 6, 7, 9, 8, _
    7, 6, 8, 13, 11, 9, 7, 15, 7, 12, 15, 9, 11, 7, 13, 12, _
    11, 13, 6, 7, 14, 9, 13, 15, 14, 8, 13, 6, 5, 12, 7, 5, _
    11, 12, 14, 15, 14, 15, 9, 8, 9, 14, 5, 6, 8, 6, 5, 12, _
    9, 15, 5, 11, 6, 8, 13, 12, 5, 12, 13, 14, 11, 8, 5, 6 }

  Dim As UByte s1(0 To ...) = _
  { 8, 9, 9, 11, 13, 15, 15, 5, 7, 7, 8, 11, 14, 14, 12, 6, _
    9, 13, 15, 7, 12, 8, 9, 11, 7, 7, 12, 7, 6, 15, 13, 11, _
    9, 7, 15, 11, 8, 6, 6, 14, 12, 13, 5, 14, 13, 13, 7, 5, _
    15, 5, 8, 11, 14, 14, 6, 14, 6, 9, 12, 9, 12, 5, 15, 8, _
    8, 5, 12, 9, 12, 5, 14, 6, 8, 13, 6, 5, 15, 13, 11, 11 }

  Dim As UInteger<32> h0 = &H67452301
  Dim As UInteger<32> h1 = &HEFCDAB89
  Dim As UInteger<32> h2 = &H98BADCFE
  Dim As UInteger<32> h3 = &H10325476
  Dim As UInteger<32> h4 = &HC3D2E1F0

  Dim As Long i, j

  Dim As ULongInt l = Len(message)
  ' set the first bit after the message to 1
  message = message + Chr(1 Shl 7)
  ' add one char to the length
  Dim As ULong padding = 64 - ((l +1) Mod (512 \ 8)) ' 512 \ 8 = 64 char.

  ' check if we have enough room for inserting the length
  If padding < 8 Then padding = padding + 64

  message = message + String(padding, Chr(0))   ' adjust length
  Dim As ULong l1 = Len(message)                ' new length

  l = l * 8    ' orignal length in bits
  ' create ubyte ptr to point to l ( = length in bits)
  Dim As UByte Ptr ub_ptr = Cast(UByte Ptr, @l)

  For i = 0 To 7  'copy length of message to the last 8 bytes
    message[l1 -8 + i] = ub_ptr[i]
  Next

  Dim As UInteger<32> A, B, C, D, E, A1, B1, C1, D1, E1, T, T1

  For i = 0 To (l1 -1) \ 64 ' split into 64 byte block

    ' x point to 16 * 4byte block inside the string message
    Dim As UInteger<32> Ptr X = Cast(UInteger<32> Ptr, @message[i*64])

    A  = h0 : B  = h1 : C  = h2 : D  = h3 : E  = h4
    A1 = h0 : B1 = h1 : C1 = h2 : D1 = h3 : E1 = h4

    For j = 0 To 79
      Select Case As Const j
        Case 0 To 15
          T = A + f1(B, C, D) + X[j] '+ K(1)
          T = ROtate_Left(T, s(j)) + E
          T1 = A1 + f5(B1, C1, D1) + X[r1(j)] + K1(1)
          T1 = ROtate_Left(T1, s1(j)) + E1
        Case 16 To 31
          T = A + f2(B, C, D) + X[r(j)] + K(2)
          T = ROtate_Left(T, s(j)) + E
          T1 = A1 + f4(B1, C1, D1) + X[r1(j)] + K1(2)
          T1 = ROtate_Left(T1, s1(j)) + E1
        Case 32 To 47
          T = A + f3(B, C, D) + X[r(j)] + K(3)
          T = ROtate_Left(T, s(j)) + E
          T1 = A1 + f3(B1, C1, D1) + X[r1(j)] + K1(3)
          T1 = ROtate_Left(T1, s1(j)) + E1
        Case 48 To 63
          T = A + f4(B, C, D) + X[r(j)] + K(4)
          T = ROtate_Left(T, s(j)) + E
          T1 = A1 + f2(B1, C1, D1) + X[r1(j)] + K1(4)
          T1 = ROtate_Left(T1, s1(j)) + E1
        Case 64 To 79
          T = A + f5(B, C, D) + X[r(j)] + K(5)
          T = ROtate_Left(T, s(j)) + E
          T1 = A1 + f1(B1, C1, D1) + X[r1(j)] '+ K1(5)
          T1 = ROtate_Left(T1, s1(j)) + E1
      End Select

      A = E : E = D : D = ROtate_Left(C, 10) : C = B : B = T
      A1 = E1 : E1 = D1 : D1 = ROtate_left(C1, 10) : C1 = B1 : B1 = T1

    Next

    T  = h1 + C + D1
    h1 = h2 + D + E1
    h2 = h3 + E + A1
    h3 = h4 + A + B1
    h4 = h0 + B + C1
    h0 = T

  Next

  Dim As String answer
  ' convert h0, h1, h2, h3 and h4 in hex, then add, low order first
  Dim As String hs1 = Hex(h0, 8)
  For i = 7 To 1 Step -2 : answer += Mid(hs1, i, 2) : Next
  hs1 = Hex(h1, 8)
  For i = 7 To 1 Step -2 : answer += Mid(hs1, i, 2) : Next
  hs1 = Hex(h2, 8)
  For i = 7 To 1 Step -2 : answer += Mid(hs1, i, 2) : Next
  hs1 = Hex(h3, 8)
  For i = 7 To 1 Step -2 : answer += Mid(hs1, i, 2) : Next
  hs1 = Hex(h4, 8)
  For i = 7 To 1 Step -2 : answer += Mid(hs1, i, 2) : Next

Return LCase(answer)

End Function

' ------=< MAIN >=------

Dim As String test = "Rosetta Code"

Print
Print test; " => "; RIPEMD_160(test)

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
Rosetta Code => b3be159860842cebaa7174c8fff0aa9e50a5199f
```



## Go

```go
package main

import (
    "golang.org/x/crypto/ripemd160"
    "fmt"
)

func main() {
    h := ripemd160.New()
    h.Write([]byte("Rosetta Code"))
    fmt.Printf("%x\n", h.Sum(nil))
}
```

```txt

b3be159860842cebaa7174c8fff0aa9e50a5199f

```


## Haskell


```haskell
import Data.Char (ord)
import Crypto.Hash.RIPEMD160 (hash)
import Data.ByteString (unpack, pack)
import Text.Printf (printf)

main = putStrLn $                     -- output to terminal
       concatMap (printf "%02x") $    -- to hex string
       unpack $                       -- to array of Word8
       hash $                         -- RIPEMD-160 hash to ByteString
       pack $                         -- to ByteString
       map (fromIntegral.ord)         -- to array of Word8
       "Rosetta Code"

```

<pre style="font-size:80%">b3be159860842cebaa7174c8fff0aa9e50a5199f

```


## Java

```java
import org.bouncycastle.crypto.digests.RIPEMD160Digest;
import org.bouncycastle.util.encoders.Hex;

public class RosettaRIPEMD160
{
    public static void main (String[] argv) throws Exception
    {
        byte[] r = "Rosetta Code".getBytes("US-ASCII");
        RIPEMD160Digest d = new RIPEMD160Digest();
        d.update (r, 0, r.length);
        byte[] o = new byte[d.getDigestSize()];
        d.doFinal (o, 0);
        Hex.encode (o, System.out);
        System.out.println();
    }
}
```

```txt

b3be159860842cebaa7174c8fff0aa9e50a5199f

```



## Julia

```julia
using Nettle

labels = ["\"\" (empty string)", "\"a\"", "\"abc\"",
        "\"message digest\"", "\"a...z\"",
        "\"abcdbcde...nopq\"", "\"A...Za...z0...9\"",
        "8 times \"1234567890\"", "1 million times \"a\""]
texts = ["", "a", "abc", "message digest", "abcdefghijklmnopqrstuvwxyz",
        "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
        "1234567890" ^ 8, "a" ^ 1_000_000]
expects = ["9c1185a5c5e9fc54612808977ee8f548b2258d31",
        "0bdc9d2d256b3ee9daae347be6f4dc835a467ffe",
        "8eb208f7e05d987a9b044a8e98c6b087f15a0bfc",
        "5d0689ef49d2fae572b881b123a85ffa21595f36",
        "f71c27109c692c1b56bbdceb5b9d2865b3708dbc",
        "12a053384a9c0c88e405a06c27dcf49ada62eb2b",
        "b0e20b6e3116640286ed3a87a5713079b21f5189",
        "9b752e45573d4b39f4dbd3323cab82bf63326bfb",
        "52783243c1697bdbe16d37f97f68f08325dc1528"]

for (lab, text, expect) in zip(labels, texts, expects)
    digest = hexdigest("ripemd160", text)
    println("# $lab\n -> digest: $digest\n -> expect: $expect")
end
```


```txt
# "" (empty string)
 -> digest: 9c1185a5c5e9fc54612808977ee8f548b2258d31
 -> expect: 9c1185a5c5e9fc54612808977ee8f548b2258d31
# "a"
 -> digest: 0bdc9d2d256b3ee9daae347be6f4dc835a467ffe
 -> expect: 0bdc9d2d256b3ee9daae347be6f4dc835a467ffe
# "abc"
 -> digest: 8eb208f7e05d987a9b044a8e98c6b087f15a0bfc
 -> expect: 8eb208f7e05d987a9b044a8e98c6b087f15a0bfc
# "message digest"
 -> digest: 5d0689ef49d2fae572b881b123a85ffa21595f36
 -> expect: 5d0689ef49d2fae572b881b123a85ffa21595f36
# "a...z"
 -> digest: f71c27109c692c1b56bbdceb5b9d2865b3708dbc
 -> expect: f71c27109c692c1b56bbdceb5b9d2865b3708dbc
# "abcdbcde...nopq"
 -> digest: 12a053384a9c0c88e405a06c27dcf49ada62eb2b
 -> expect: 12a053384a9c0c88e405a06c27dcf49ada62eb2b
# "A...Za...z0...9"
 -> digest: b0e20b6e3116640286ed3a87a5713079b21f5189
 -> expect: b0e20b6e3116640286ed3a87a5713079b21f5189
# 8 times "1234567890"
 -> digest: 9b752e45573d4b39f4dbd3323cab82bf63326bfb
 -> expect: 9b752e45573d4b39f4dbd3323cab82bf63326bfb
# 1 million times "a"
 -> digest: 52783243c1697bdbe16d37f97f68f08325dc1528
 -> expect: 52783243c1697bdbe16d37f97f68f08325dc1528
```



## Kotlin

```scala
import org.bouncycastle.crypto.digests.RIPEMD160Digest
import org.bouncycastle.util.encoders.Hex
import kotlin.text.Charsets.US_ASCII

fun RIPEMD160Digest.inOneGo(input : ByteArray) : ByteArray {
    val output = ByteArray(digestSize)

    update(input, 0, input.size)
    doFinal(output, 0)

    return output
}

fun main(args: Array<String>) {
    val input = "Rosetta Code".toByteArray(US_ASCII)
    val output = RIPEMD160Digest().inOneGo(input)

    Hex.encode(output, System.out)
    System.out.flush()
}
```

```txt

b3be159860842cebaa7174c8fff0aa9e50a5199f

```



## Lasso


```lasso

cipher_digest("Rosetta Code", -digest='RIPEMD160', -hex)

```

```txt

b3be159860842cebaa7174c8fff0aa9e50a5199f

```



## Lua


{{libheader|LuaCrypto}} luarocks install LuaCrypto (see mkottman dot github dot io slash luacrypto; I am getting fed up with reCAPTCHA)


```Lua
#!/usr/bin/lua

require "crypto"

print(crypto.digest("ripemd160", "Rosetta Code"))
```


```txt
b3be159860842cebaa7174c8fff0aa9e50a5199f
```



## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
	Function Prepare_RiPeMd_160 {
		Dim Base 0,  K(5), K1(5)
		K(0)=0x00000000, 0x5A827999, 0x6ED9EBA1, 0x8F1BBCDC, 0xA953FD4E
		K1(0)=0x50A28BE6,0x5C4DD124, 0x6D703EF3, 0x7A6D76E9, 0x00000000
		Dim Base 0,r(80), r1(80), s(80), s1(80)
		r(0)=0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
		r(16)=7, 4, 13, 1, 10, 6, 15, 3, 12, 0, 9, 5, 2, 14, 11, 8
		r(32)= 3, 10, 14, 4, 9, 15, 8, 1, 2, 7, 0, 6, 13, 11, 5, 12
		r(48)=1, 9, 11, 10, 0, 8, 12, 4, 13, 3, 7, 15, 14, 5, 6, 2
		r(64)=4, 0, 5, 9, 7, 12, 2, 10, 14, 1, 3, 8, 11, 6, 15, 13
		k=r() : k*=4   ' k is a pointer to array. We have to multiply to make them offsets

		r1(0)=5, 14, 7, 0, 9, 2, 11, 4, 13, 6, 15, 8, 1, 10, 3, 12
		r1(16)=6, 11, 3, 7, 0, 13, 5, 10, 14, 15, 8, 12, 4, 9, 1, 2
		r1(32)=15, 5, 1, 3, 7, 14, 6, 9, 11, 8, 12, 2, 10, 0, 4, 13
		r1(48)=8, 6, 4, 1, 3, 11, 15, 0, 5, 12, 2, 13, 9, 7, 10, 14
		r1(64)=12, 15, 10, 4, 1, 5, 8, 7, 6, 2, 13, 14, 0, 3, 9, 11

		k=r1() : k*=4

		s(0)=11, 14, 15, 12, 5, 8, 7, 9, 11, 13, 14, 15, 6, 7, 9, 8
		s(16)=7, 6, 8, 13, 11, 9, 7, 15, 7, 12, 15, 9, 11, 7, 13, 12
		s(32)=11, 13, 6, 7, 14, 9, 13, 15, 14, 8, 13, 6, 5, 12, 7, 5
		s(48)=11, 12, 14, 15, 14, 15, 9, 8, 9, 14, 5, 6, 8, 6, 5, 12
		s(64)=9, 15, 5, 11, 6, 8, 13, 12, 5, 12, 13, 14, 11, 8, 5, 6

		s1(0)=8, 9, 9, 11, 13, 15, 15, 5, 7, 7, 8, 11, 14, 14, 12, 6
		s1(16)=9, 13, 15, 7, 12, 8, 9, 11, 7, 7, 12, 7, 6, 15, 13, 11
		s1(32)=9, 7, 15, 11, 8, 6, 6, 14, 12, 13, 5, 14, 13, 13, 7, 5
		s1(48)=15, 5, 8, 11, 14, 14, 6, 14, 6, 9, 12, 9, 12, 5, 15, 8
		s1(64)=8, 5, 12, 9, 12, 5, 14, 6, 8, 13, 6, 5, 15, 13, 11, 11

		Dim Base 0, T(5), TT(5)
		T(0)=lambda ->binary.xor(binary.xor(number,number),number)
		T(1)=lambda (B,C,D)->binary.or(binary.and(B,C), binary.and(binary.not(B), D))
		T(2)=lambda ->binary.xor(binary.or(number, binary.not(number)), number)
		T(3)=lambda (B,C,D)->binary.or(binary.and(B,D), binary.and(C,binary.not(D)))
		T(4)=lambda ->binary.xor(number, binary.or(number, binary.not(number)))

		\\ no need for variables we read form stack with number
		TT(0)=lambda ->binary.xor(number, binary.or(number, binary.not(number)))
		TT(1)=lambda (BB,CC,DD)->binary.or(binary.and(BB,DD), binary.and(CC,binary.not(DD)))
		TT(2)=lambda ->binary.xor(binary.or(number, binary.not(number)), number)
		TT(3)=lambda (BB,CC,DD)->binary.or(binary.and(BB,CC), binary.and(binary.not(BB),DD))
		TT(4)=lambda ->binary.xor(binary.xor(number,number),number)

		\\ return of this function is a lambda function
		\\ all arrays are closures to this lambda
		=lambda K(),K1(),TT(), T(),r(),r1(), s(), s1() (&message$, ansi as boolean=true, ansiid=1033)-> {
			set fast!
			def h0 = 0x67452301, h1 = 0xEFCDAB89, h2 = 0x98BADCFE
			def h3 = 0x10325476, h4 = 0xC3D2E1F0
			def i, j, l, padding, l1, blocks, acc, f64 as boolean=true, oldid
			if ansi then oldid=locale : locale ansiid
			\\ we use a buffer of 64 bytes
			buffer clear message as byte*64
			l=len(message$)*if(ansi->1,2 )
			if binary.and(l,63)>55 then  padding=64
			padding+= 64 - (l Mod 64)
			l1=padding+l+1

			f64=binary.and(l,63)<>0

			blocks=l1 div 64
rem
			Print "blocks:";blocks
			\\ now prepare the buffer
			PrepareBuffer()
			def decimal  A, B, C, D, E, AA, BB, CC, DD, EE, T, TT
			do
			A  = h0 : B  = h1 : C  = h2 : D  = h3 : E  = h4
			AA = h0 : BB = h1 : CC = h2 : DD = h3 : EE = h4
			for J=0 to 79 {
				JJ=J DIV 16
				PUSH binary.add(Binary.Rotate(binary.add(A,T(JJ)(B,C,D),eval(message ,r(j) as long),k(jj)), s(j)), e)
				A = E : E = D : D = Binary.Rotate(C, 10) : C = B : READ B
				PUSH binary.add(Binary.Rotate(binary.add(AA,TT(JJ)(BB,CC,DD),eval(message, r1(j) as long),k1(jj)),s1(j)),EE)
				AA = EE : EE = DD : DD = Binary.Rotate(CC, 10) : CC = BB : READ BB
			}
			push binary.add(h1, C, DD)
			h1 = binary.add(h2, D, EE)
			h2 = binary.add(h3, E, AA)
			h3 = binary.add(h4, A, BB)
			h4 = binary.add(h0, B, CC)
			Read h0
			blocks--
rem
			print over $(0,8), blocks : Refresh
			if blocks=0 then exit
			PrepareBuffer()
			always
rem
			print
			buffer ans as byte*20
			\\ we put ulong (long is ulong in buffers)
			Return ans, 0:=h0 as long, 4:=h1 as long,8:=h2 as long, 12:=h3 as long, 16:=h4 as long
			=ans
			if ansi then locale oldid
			set fast
			Sub PrepareBuffer()

				if l-acc>=64 then
					LoadPart(64)
				else.if blocks=1 then
					return message, 0:=string$(chr$(0),32)
					if l-acc=0 and f64 then
						Return message, 56:=l*8 as long, 60 :=binary.shift(l,-29) as long
					else
						Return message, l-acc:=0x80, 56:=l*8 as long, 60 :=binary.shift(l,-29) as long
						if l>acc then LoadPart(l-acc)
					end if
				else
					Return message, l-acc:=0x80
					LoadPart(l-acc)
				end if
			End Sub
			sub LoadPart(many)
				\\ str$() convert to ansi, one byte per character
				\\ using 1033 as Ansi language
				if ansi then
					Return message, 0:=str$(mid$(message$,1+acc, many))
				else
					Return message, 0:=mid$(message$, 1+acc, many)
				end if
				acc+=many
			end sub
		}
	}
	Module TestHash (RIPEMD){
		Flush
		\\ push data to stack of values, as fifo (each entry append to end of stack)
		Data "b3be159860842cebaa7174c8fff0aa9e50a5199f","Rosetta Code"
		Data "9c1185a5c5e9fc54612808977ee8f548b2258d31",""
		Data "0bdc9d2d256b3ee9daae347be6f4dc835a467ffe","a"
		Data "8eb208f7e05d987a9b044a8e98c6b087f15a0bfc","abc"
		Data "5d0689ef49d2fae572b881b123a85ffa21595f36", "message digest"
		Data "f71c27109c692c1b56bbdceb5b9d2865b3708dbc","abcdefghijklmnopqrstuvwxyz"
		Data "b0e20b6e3116640286ed3a87a5713079b21f5189"
		Data "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
		Data "9b752e45573d4b39f4dbd3323cab82bf63326bfb", String$("1234567890",8)
rem		Data "52783243c1697bdbe16d37f97f68f08325dc1528", String$("a",1000000)

		While not empty
			Read check$, text$
			Print "RIPEMD160 for ";quote$(Left$(if$(len(text$)>30->left$(text$,27)+"...",  text$),30))
			\\ pass text$ by reference
			Display(RIPEMD(&text$))
		End While

		sub Display(ans)
			local answer$
			for i=0 to len(ans)-1
				answer$+=hex$(eval(ans,i),1)
			next i
			Print lcase$(answer$)
			Print lcase$(answer$)=check$
		end sub
	}
	TestHash Prepare_RiPeMd_160()
}
Checkit

```



## Mathematica


<lang>Hash["Rosetta code","RIPEMD160","HexString"]
```


```txt
1cda558e41e47c3090aafd73ca5651d176f95ca9
```



## Nim


```nim
import nimcrypto / [ripemd, hash]

echo ripemd160.digest("Rosetta Code")
```


```txt
B3BE159860842CEBAA7174C8FFF0AA9E50A5199F
```



## Objeck


```objeck

class Hash {
  function : Main(args : String[]) ~ Nil {
    in := "Rosetta Code"->ToByteArray();
    hash := Encryption.Hash->RIPEMD160(in);
    hash->ToHexString()->PrintLine();
  }
}

```

```txt
B3BE159860842CEBAA7174C8FFF0AA9E50A5199F
```



## PARI/GP


Build RIPEMD-160 plugin using Linux system library and PARI's function interface.


```C
#include <pari/pari.h>

#include <openssl/ripemd.h>

#define HEX(x)  (((x) < 10)? (x)+'0': (x)-10+'a')

GEN plug_ripemd160(char *text)
{
  char md[RIPEMD160_DIGEST_LENGTH];
  char hash[sizeof(md) * 2 + 1];
  int i;

  RIPEMD160((unsigned char*)text, strlen(text), (unsigned char*)md);

  for (i = 0; i < sizeof(md); i++) {
    hash[i+i]   = HEX((md[i] >> 4) & 0x0f);
    hash[i+i+1] = HEX(md[i] & 0x0f);
  }

  hash[sizeof(md) * 2] = 0;

  return strtoGENstr(hash);
}
```


Compile with: ''gcc -Wall -O2 -fPIC -shared ripemd160.c -o libripemd160.so -lcrypto -lpari''

Load plugin from your home directory into PARI:

```parigp
install("plug_ripemd160", "s", "RIPEMD160", "~/libripemd160.so");

RIPEMD160("Rosetta Code")
```


Output:
```txt
"b3be159860842cebaa7174c8fff0aa9e50a5199f"
```


It can also be done in GP with an install hack (thanks to Bill Allombert for this code):


```parigp
install(RIPEMD160,"vsLs",,"/usr/lib/x86_64-linux-gnu/libcrypto.so")
ripemd160(a)=
{
  my(b=Strchr(vectorsmall(20,i,32)));
  RIPEMD160(a,length(a),b);
  Strprintf("%x",fromdigits(Vec(Vecsmall(b)),256));
}
ripemd160("Rosetta Code")
```



## Perl


```perl
use Crypt::RIPEMD160;
say unpack "H*", Crypt::RIPEMD160->hash("Rosetta Code");
```

```txt
b3be159860842cebaa7174c8fff0aa9e50a5199f
```


The [https://metacpan.org/release/CryptX CryptX] module also implements RIPEMD-160 along with the 128-, 256-, and 320-bit variants, as well many many other hashes.  This gives identical output as above as expected.

```perl
use Crypt::Digest::RIPEMD160 qw/ripemd160_hex/;
say ripemd160_hex("Rosetta Code")
```



## Perl 6


```perl6
=for CREDITS
Crypto-JS v2.0.0
http:#code.google.com/p/crypto-js/
Copyright (c) 2009, Jeff Mott. All rights reserved.

sub rotl($n, $b) { $n +< $b +| $n +> (32 - $b) }
sub prefix:<m^> { +^$^x % 2**32 }
sub infix:<m+> { ($^x + $^y) % 2**32 }

constant r1 = <
    0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
    7 4 13 1 10 6 15 3 12 0 9 5 2 14 11 8
    3 10 14 4 9 15 8 1 2 7 0 6 13 11 5 12
    1 9 11 10 0 8 12 4 13 3 7 15 14 5 6 2
    4 0 5 9 7 12 2 10 14 1 3 8 11 6 15 13
>;
constant r2 = <
    5 14 7 0 9 2 11 4 13 6 15 8 1 10 3 12
    6 11 3 7 0 13 5 10 14 15 8 12 4 9 1 2
    15 5 1 3 7 14 6 9 11 8 12 2 10 0 4 13
    8 6 4 1 3 11 15 0 5 12 2 13 9 7 10 14
    12 15 10 4 1 5 8 7 6 2 13 14 0 3 9 11
>;
constant s1 = <
    11 14 15 12 5 8 7 9 11 13 14 15 6 7 9 8
    7 6 8 13 11 9 7 15 7 12 15 9 11 7 13 12
    11 13 6 7 14 9 13 15 14 8 13 6 5 12 7 5
    11 12 14 15 14 15 9 8 9 14 5 6 8 6 5 12
    9 15 5 11 6 8 13 12 5 12 13 14 11 8 5 6
>;
constant s2 = <
    8 9 9 11 13 15 15 5 7 7 8 11 14 14 12 6
    9 13 15 7 12 8 9 11 7 7 12 7 6 15 13 11
    9 7 15 11 8 6 6 14 12 13 5 14 13 13 7 5
    15 5 8 11 14 14 6 14 6 9 12 9 12 5 15 8
    8 5 12 9 12 5 14 6 8 13 6 5 15 13 11 11
>;
constant F =
    * +^ * +^ *,
    { ($^x +& $^y) +| (m^$^x +& $^z) },
    (* +| m^*) +^ *,
    { ($^x +& $^z) +| ($^y +& m^$^z) },
    * +^ (* +| m^*),
;
constant K1 = flat | <0x00000000 0x5a827999 0x6ed9eba1 0x8f1bbcdc 0xa953fd4e> »xx» 16;
constant K2 = flat | <0x50a28be6 0x5c4dd124 0x6d703ef3 0x7a6d76e9 0x00000000> »xx» 16;

our proto rmd160($) returns Blob {*}
multi rmd160(Str $s) { rmd160 $s.encode: 'ascii' }
multi rmd160(Blob $data) {
    my @b = | $data, 0x80;
    push @b, 0 until (8*@b-448) %% 512;
    my $len = 8 * $data.elems;
    push @b, | gather for ^8 { take $len % 256; $len div= 256 }

    my @word = gather for @b -> $a, $b, $c, $d {
        take reduce * *256 + *, $d, $c, $b, $a;
    }

    my @h = 0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, 0xc3d2e1f0;
    loop (my $i = 0; $i < @word; $i += 16) {
        my @X = my @Y = @h;
        for ^80 -> $j {
            my $T = rotl(
                @X[0] m+ F[$j div 16](|@X[1..3]) m+ (@word[$i+r1[$j]] // 0) m+ K1[$j], s1[$j]
            ) m+ @X[4];
            @X = @X[4], $T, @X[1], rotl(@X[2], 10) % 2**32, @X[3];
            $T = rotl(
                @Y[0] m+ F[(79-$j) div 16](|@Y[1..3]) m+ (@word[$i+r2[$j]] // 0) m+ K2[$j], s2[$j]
            ) m+ @Y[4];
            @Y = @Y[4], $T, @Y[1], rotl(@Y[2], 10) % 2**32, @Y[3];
        }
        @h = (flat @h[1..4,^1]) Z[m+] (flat @X[2..4,^2]) Z[m+] flat @Y[3..4,^3];
    }
    return Blob.new: gather for @h -> $word is rw {
        for ^4 { take $word % 256; $word div= 256 }
    }
}

say rmd160 "Rosetta Code";
```


```txt
Buf:0x<b3 be 15 98 60 84 2c eb aa 71 74 c8 ff f0 aa 9e 50 a5 19 9f>
```



## Phix


```Phix
include builtins\ripemd160.e

constant test = "Rosetta Code"
printf(1,"\n%s => %s\n",{test,ripemd160(test)})
```

```txt

Rosetta Code => b3be159860842cebaa7174c8fff0aa9e50a5199f

```

The standard include file ripemd160.e is also written in Phix, and is reproduced below.

```Phix
--
-- builtins\ripemd160.e
--
### ==============

--
function rol(atom v, integer n)
-- Programming note: this use of #ilASM{} is more for expediency than efficiency
    #ilASM{ mov eax,[v]
            call :%pLoadMint
            mov ecx,[n]
            rol eax,cl
            lea edi,[v]
            call :%pStoreMint }
    return v
end function

constant K  = {#00000000,#5A827999,#6ED9EBA1,#8F1BBCDC,#A953FD4E},
         KK = {#50A28BE6,#5C4DD124,#6D703EF3,#7A6D76E9,#00000000},
         r  = { 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
                7,  4, 13,  1, 10,  6, 15,  3, 12,  0,  9,  5,  2, 14, 11,  8,
                3, 10, 14,  4,  9, 15,  8,  1,  2,  7,  0,  6, 13, 11,  5, 12,
                1,  9, 11, 10,  0,  8, 12,  4, 13,  3,  7, 15, 14,  5,  6,  2,
                4,  0,  5,  9,  7, 12,  2, 10, 14,  1,  3,  8, 11,  6, 15, 13 },
         rr = { 5, 14,  7,  0,  9,  2, 11,  4, 13,  6, 15,  8,  1, 10,  3, 12,
                6, 11,  3,  7,  0, 13,  5, 10, 14, 15,  8, 12,  4,  9,  1,  2,
               15,  5,  1,  3,  7, 14,  6,  9, 11,  8, 12,  2, 10,  0,  4, 13,
                8,  6,  4,  1,  3, 11, 15,  0,  5, 12,  2, 13,  9,  7, 10, 14,
               12, 15, 10,  4,  1,  5,  8,  7,  6,  2, 13, 14,  0,  3,  9, 11 },
         s  = {11, 14, 15, 12,  5,  8,  7,  9, 11, 13, 14, 15,  6,  7,  9,  8,
                7,  6,  8, 13, 11,  9,  7, 15,  7, 12, 15,  9, 11,  7, 13, 12,
               11, 13,  6,  7, 14,  9, 13, 15, 14,  8, 13,  6,  5, 12,  7,  5,
               11, 12, 14, 15, 14, 15,  9,  8,  9, 14,  5,  6,  8,  6,  5, 12,
                9, 15,  5, 11,  6,  8, 13, 12,  5, 12, 13, 14, 11,  8,  5,  6 },
         ss = { 8,  9,  9, 11, 13, 15, 15,  5,  7,  7,  8, 11, 14, 14, 12,  6,
                9, 13, 15,  7, 12,  8,  9, 11,  7,  7, 12,  7,  6, 15, 13, 11,
                9,  7, 15, 11,  8,  6,  6, 14, 12, 13,  5, 14, 13, 13,  7,  5,
               15,  5,  8, 11, 14, 14,  6, 14,  6,  9, 12,  9, 12,  5, 15,  8,
                8,  5, 12,  9, 12,  5, 14,  6,  8, 13,  6,  5, 15, 13, 11, 11 }

global function ripemd160(string message, bool asString=true, atom pMem=NULL)
--
-- Calculate the ripe-md-160 checksum.
--
-- if asString is true (the default), returns a string representation of the
--  checksum (and pMem is ignored)
-- if asString is false, returns pMem (for want of anything better), which
--  must be a non-NULL pointer to at least 20 bytes of memory.
--
    atom h0 = #67452301,
         h1 = #EFCDAB89,
         h2 = #98BADCFE,
         h3 = #10325476,
         h4 = #C3D2E1F0,
         mraw, t, tt

    integer l = length(message),
            padding = 64 - mod(l+1,64)
    if padding<8 then padding += 64 end if

    message &= #80 & repeat('\0',padding-8)
                   & int_to_bytes(l*8,8)

    #ilASM{ mov eax,[message]
            lea edi,[mraw]
            shl eax,2  -- ref -> raw address
            call :%pStoreMint }

    for i=0 to length(message)-64 by 64 do
        atom {a,  b,  c,  d,  e}  = {h0, h1, h2, h3, h4}
        atom {aa, bb, cc, dd, ee} = {h0, h1, h2, h3, h4}
        for j = 1 to 80 do
            integer k = floor((j-1)/16)
            switch k
                case 0:
                    t = xor_bits(xor_bits(b, c), d)
                    tt = xor_bits(bb,or_bits(cc,not_bits(dd)))
                case 1:
                    t = or_bits(and_bits(b,c),and_bits(not_bits(b),d))
                    tt = or_bits(and_bits(bb,dd),and_bits(cc,not_bits(dd)))
                case 2:
                    t = xor_bits(or_bits(b,not_bits(c)),d)
                    tt = xor_bits(or_bits(bb,not_bits(cc)),dd)
                case 3:
                    t = or_bits(and_bits(b,d),and_bits(c,not_bits(d)))
                    tt = or_bits(and_bits(bb,cc),and_bits(not_bits(bb),dd))
                case 4:
                    t = xor_bits(b,or_bits(c,not_bits(d)))
                    tt = xor_bits(xor_bits(bb, cc), dd)
            end switch
            t  = rol( a +  t + peek4u(mraw+i+ r[j]*4) +  K[k+1],  s[j]) +  e
            tt = rol(aa + tt + peek4u(mraw+i+rr[j]*4) + KK[k+1], ss[j]) + ee
            {a, e, d, c, b } = {e, d, rol(c, 10), b, t }
            {aa,ee,dd,cc,bb} = {ee,dd,rol(cc, 10),bb,tt}
        end for
        {h0, h1, h2, h3, h4} = {h1+c+dd, h2+d+ee, h3+e+aa, h4+a+bb, h0+b+cc}
    end for
    if not asString then
        if pMem=NULL then ?9/0 end if
        poke4(pMem,{h0,h1,h2,h3,h4})
        return pMem
    end if
    atom mem = allocate(20,true)
    poke4(mem,{h0,h1,h2,h3,h4})
    string res = ""
    for i=1 to 20 do
        res &= sprintf("%02X",peek(mem+i-1))
    end for
    return res
end function
```



## PicoLisp


```PicoLisp
(de *R160-R1 . (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
               8 5 14 2 11 7 16 4 13 1 10 6 3 15 12 9
               4 11 15 5 10 16 9 2 3 8 1 7 14 12 6 13
               2 10 12 11 1 9 13 5 14 4 8 16 15 6 7 3
               5 1 6 10 8 13 3 11 15 2 4 9 12 7 16 14 .))
(de *R160-R2 . (6 15 8 1 10 3 12 5 14 7 16 9 2 11 4 13
                7 12 4 8 1 14 6 11 15 16 9 13 5 10 2 3
                16 6 2 4 8 15 7 10 12 9 13 3 11 1 5 14
                9 7 5 2 4 12 16 1 6 13 3 14 10 8 11 15
                13 16 11 5 2 6 9 8 7 3 14 15 1 4 10 12 .))
(de *R160-S1 . (11 14 15 12 5 8 7 9 11 13 14 15 6 7 9 8
               7 6 8 13 11 9 7 15 7 12 15 9 11 7 13 12
               11 13 6 7 14 9 13 15 14 8 13 6 5 12 7 5
               11 12 14 15 14 15 9 8 9 14 5 6 8 6 5 12
               9 15 5 11 6 8 13 12 5 12 13 14 11 8 5 6 .))
(de *R160-S2 . (8 9 9 11 13 15 15 5 7 7 8 11 14 14 12 6
                9 13 15 7 12 8 9 11 7 7 12 7 6 15 13 11
                9 7 15 11 8 6 6 14 12 13 5 14 13 13 7 5
                15 5 8 11 14 14 6 14 6 9 12 9 12 5 15 8
                8 5 12 9 12 5 14 6 8 13 6 5 15 13 11 11 .))

(de mod32 (N)
   (& N `(hex "FFFFFFFF")) )

(de not32 (N)
   (x| N `(hex "FFFFFFFF")) )

(de add32 @
   (mod32 (pass +)) )

(de leftRotate (X C)
   (| (mod32 (>> (- C) X)) (>> (- 32 C) X)) )

(de ripemd160 (Str)
   (let Len (length Str)
      (setq Str
         (conc
            (need
               (- 8 (* 64 (/ (+ Len 1 8 63) 64)))
               (conc
                  (mapcar char (chop Str))
                  (cons `(hex "80")) )
               0 )
            (make
               (setq Len (* 8 Len))
               (do 8
                  (link (& Len 255))
                  (setq Len (>> 8 Len )) ) ) ) ) )
   (let
      (H0 `(hex "67452301")
         H1 `(hex "EFCDAB89")
         H2 `(hex "98BADCFE")
         H3 `(hex "10325476")
         H4 `(hex "C3D2E1F0") )
      (while Str
         (let
            (A1 H0  B1 H1  C1 H2  D1 H3  E1 H4
               A2 H0  B2 H1  C2 H2  D2 H3  E2 H4
               W (make
                     (do 16
                        (link
                           (apply |
                              (mapcar >> (0 -8 -16 -24) (cut 4 'Str)) ) ) ) ) )
            (use (Func1 Func2 Hex1 Hex2)
               (for I 80
                  (cond
                     ((>= 16 I)
                        (setq
                           Func1 '(x| B1 C1 D1)
                           Func2 '(x| B2 (| C2 (not32 D2)))
                           Hex1 0
                           Hex2 `(hex "50A28BE6") ) )
                     ((>= 32 I)
                        (setq
                           Func1 '(| (& B1 C1) (& (not32 B1) D1))
                           Func2 '(| (& B2 D2) (& C2 (not32 D2)))
                           Hex1 `(hex "5A827999")
                           Hex2 `(hex "5C4DD124") ) )
                     ((>= 48 I)
                        (setq
                           Func1 '(x| (| B1 (not32 C1)) D1)
                           Func2 '(x| (| B2 (not32 C2)) D2)
                           Hex1 `(hex "6ED9EBA1")
                           Hex2 `(hex "6D703EF3") ) )
                     ((>= 64 I)
                        (setq
                           Func1 '(| (& B1 D1) (& C1 (not32 D1)))
                           Func2 '(| (& B2 C2) (& (not32 B2) D2))
                           Hex1 `(hex "8F1BBCDC")
                           Hex2 `(hex "7A6D76E9") ) )
                     (T
                        (setq
                           Func1 '(x| B1 (| C1 (not32 D1)))
                           Func2 '(x| B2 C2 D2)
                           Hex1 `(hex "A953FD4E")
                           Hex2 0 ) ) )
                  (setq
                     Tmp1
                     (add32
                        (leftRotate
                           (add32
                              A1
                              (eval Func1)
                              (get W (pop '*R160-R1))
                              Hex1 )
                           (pop '*R160-S1) )
                        E1 )
                     Tmp2
                     (add32
                        (leftRotate
                           (add32
                              A2
                              (eval Func2)
                              (get W (pop '*R160-R2))
                              Hex2 )
                           (pop '*R160-S2) )
                        E2 )
                     A1 E1
                     E1 D1
                     D1 (leftRotate C1 10)
                     C1 B1
                     B1 Tmp1

                     A2 E2
                     E2 D2
                     D2 (leftRotate C2 10)
                     C2 B2
                     B2 Tmp2 ) ) )
               (setq
                  Tmp (add32 H1 C1 D2)
                  H1 (add32 H2 D1 E2)
                  H2 (add32 H3 E1 A2)
                  H3 (add32 H4 A1 B2)
                  H4 (add32 H0 B1 C2)
                  H0 Tmp ) ) )
      (make
         (for N (list H0 H1 H2 H3 H4)
            (do 4
               (link (& N 255))
               (setq N (>> 8 N)) ) ) ) ) )

(let Str "Rosetta Code"
   (println
      (pack
         (mapcar
            '((B) (pad 2 (hex B)))
            (ripemd160 Str) ) ) )
   (println
      (pack
         (mapcar
            '((B) (pad 2 (hex B)))
            (native
               "libcrypto.so"
               "RIPEMD160"
               '(B . 20)
               Str
               (length Str)
               '(NIL (20)) ) ) ) ) )

(bye)
```



## PowerShell

Using .Net's <code>[System.Security.Cryptography.HashAlgorithm]</code>, hash either a string or a file using any of the cryptography hash algorithms.

```PowerShell

function Get-Hash
{
    [CmdletBinding(DefaultParameterSetName="String")]
    [OutputType([string])]
    Param
    (
        [Parameter(Mandatory=$true,
                   ParameterSetName="String",
                   Position=0)]
        [string]
        $String,

        [Parameter(Mandatory=$true,
                   ParameterSetName="FileName",
                   Position=0)]
        [string]
        $FileName,

        [Parameter(Mandatory=$false,
                   Position=1)]
        [ValidateSet("MD5", "RIPEMD160", "SHA1", "SHA256", "SHA384", "SHA512")]
        [string]
        $HashType = "MD5"
    )

    $hashAlgorithm = [System.Security.Cryptography.HashAlgorithm]
    $stringBuilder = New-Object -TypeName System.Text.StringBuilder

    switch ($PSCmdlet.ParameterSetName)
    {
        "String"
        {
	    $hashAlgorithm::Create($HashType).ComputeHash([System.Text.Encoding]::UTF8.GetBytes($String)) | ForEach-Object {
	        $stringBuilder.Append($_.ToString("x2")) | Out-Null
	    }
        }
        "FileName"
        {
            $fileStream = New-Object -TypeName System.IO.FileStream -ArgumentList $FileName, ([System.IO.FileMode]::Open)

	    $hashAlgorithm::Create($HashType).ComputeHash($fileStream) | ForEach-Object {
	        $stringBuilder.Append($_.ToString("x2")) | Out-Null
	    }

	    $fileStream.Close()
	    $fileStream.Dispose()
        }
    }

    $stringBuilder.ToString()
}

```



```PowerShell

Get-Hash "Rosetta Code" -HashType RIPEMD160

```

```txt

b3be159860842cebaa7174c8fff0aa9e50a5199f

```



## Python


```python
Python 3.3.0 (v3.3.0:bd8afb90ebf2, Sep 29 2012, 10:57:17) [MSC v.1600 64 bit (AMD64)] on win32
Type "copyright", "credits" or "license()" for more information.
>>> import hashlib
>>> h = hashlib.new('ripemd160')
>>> h.update(b"Rosetta Code")
>>> h.hexdigest()
'b3be159860842cebaa7174c8fff0aa9e50a5199f'
>>>
```




## Racket


```racket

#lang racket
(require (planet soegaard/digest:1:2/digest))
(ripemd160 #"Rosetta Code")

```

```racket

"b3be159860842cebaa7174c8fff0aa9e50a5199f"

```



## Ruby

Use 'digest' from Ruby's standard library.


```ruby
require 'digest'
puts Digest::RMD160.hexdigest('Rosetta Code')
```


Use 'openssl' from Ruby's standard library.


```ruby
require 'openssl'
puts OpenSSL::Digest::RIPEMD160.hexdigest('Rosetta Code')
```


Implement RIPEMD-160 in Ruby.


```ruby
require 'stringio'

module RMD160
  # functions and constants
  MASK = (1 << 32) - 1
  F = [
    proc {|x, y, z| x ^ y ^ z},
    proc {|x, y, z| (x & y) | (x.^(MASK) & z)},
    proc {|x, y, z| (x | y.^(MASK)) ^ z},
    proc {|x, y, z| (x & z) | (y & z.^(MASK))},
    proc {|x, y, z| x ^ (y | z.^(MASK))},
  ].freeze
  K  = [0x00000000, 0x5a827999, 0x6ed9eba1, 0x8f1bbcdc, 0xa953fd4e]
  KK = [0x50a28be6, 0x5c4dd124, 0x6d703ef3, 0x7a6d76e9, 0x00000000]
  R  = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
        7, 4, 13, 1, 10, 6, 15, 3, 12, 0, 9, 5, 2, 14, 11, 8,
        3, 10, 14, 4, 9, 15, 8, 1, 2, 7, 0, 6, 13, 11, 5, 12,
        1, 9, 11, 10, 0, 8, 12, 4, 13, 3, 7, 15, 14, 5, 6, 2,
        4, 0, 5, 9, 7, 12, 2, 10, 14, 1, 3, 8, 11, 6, 15, 13]
  RR = [5, 14, 7, 0, 9, 2, 11, 4, 13, 6, 15, 8, 1, 10, 3, 12,
        6, 11, 3, 7, 0, 13, 5, 10, 14, 15, 8, 12, 4, 9, 1, 2,
        15, 5, 1, 3, 7, 14, 6, 9, 11, 8, 12, 2, 10, 0, 4, 13,
        8, 6, 4, 1, 3, 11, 15, 0, 5, 12, 2, 13, 9, 7, 10, 14,
        12, 15, 10, 4, 1, 5, 8, 7, 6, 2, 13, 14, 0, 3, 9, 11]
  S  = [11, 14, 15, 12, 5, 8, 7, 9, 11, 13, 14, 15, 6, 7, 9, 8,
        7, 6, 8, 13, 11, 9, 7, 15, 7, 12, 15, 9, 11, 7, 13, 12,
        11, 13, 6, 7, 14, 9, 13, 15, 14, 8, 13, 6, 5, 12, 7, 5,
        11, 12, 14, 15, 14, 15, 9, 8, 9, 14, 5, 6, 8, 6, 5, 12,
        9, 15, 5, 11, 6, 8, 13, 12, 5, 12, 13, 14, 11, 8, 5, 6]
  SS = [8, 9, 9, 11, 13, 15, 15, 5, 7, 7, 8, 11, 14, 14, 12, 6,
        9, 13, 15, 7, 12, 8, 9, 11, 7, 7, 12, 7, 6, 15, 13, 11,
        9, 7, 15, 11, 8, 6, 6, 14, 12, 13, 5, 14, 13, 13, 7, 5,
        15, 5, 8, 11, 14, 14, 6, 14, 6, 9, 12, 9, 12, 5, 15, 8,
        8, 5, 12, 9, 12, 5, 14, 6, 8, 13, 6, 5, 15, 13, 11, 11]

  module_function

  def rol(value, shift)
    (value << shift).&(MASK) | (value.&(MASK) >> (32 - shift))
  end

  # Calculates RIPEMD-160 message digest of _string_. Returns binary
  # digest. For hexadecimal digest, use
  # +*RMD160.rmd160(string).unpack('H*')+.
  def rmd160(string)
    # initial hash
    h0 = 0x67452301
    h1 = 0xefcdab89
    h2 = 0x98badcfe
    h3 = 0x10325476
    h4 = 0xc3d2e1f0

    io = StringIO.new(string)
    block = ""
    term = false  # appended "\x80" in second-last block?
    last = false  # last block?
    until last
      # Read next block of 16 words (64 bytes, 512 bits).
      io.read(64, block) or (
        # Work around a bug in Rubinius 1.2.4. At eof,
        # MRI and JRuby already replace block with "".
        block.replace("")
      )

      # Unpack block into 32-bit words "V".
      case len = block.length
      when 64
        # Unpack 16 words.
        x = block.unpack("V16")
      when 56..63
        # Second-last block: append padding, unpack 16 words.
        block.concat("\x80"); term = true
        block.concat("\0" * (63 - len))
        x = block.unpack("V16")
      when 0..55
        # Last block: append padding, unpack 14 words.
        block.concat(term ? "\0" : "\x80")
        block.concat("\0" * (55 - len))
        x = block.unpack("V14")

        # Append bit length, 2 words.
        bit_len = string.length << 3
        x.push(bit_len & MASK, bit_len >> 32)
        last = true
      else
        fail "impossible"
      end

      # Process this block.
      a,  b,  c,  d,  e  = h0, h1, h2, h3, h4
      aa, bb, cc, dd, ee = h0, h1, h2, h3, h4
      j = 0
      5.times {|ro|
        f, ff = F[ro], F[4 - ro]
        k, kk = K[ro], KK[ro]
        16.times {
          a, e, d, c, b = e, d, rol(c, 10), b,
            rol(a + f[b, c, d] + x[R[j]] + k, S[j]) + e
          aa, ee, dd, cc, bb = ee, dd, rol(cc, 10), bb,
            rol(aa + ff[bb, cc, dd] + x[RR[j]] + kk, SS[j]) + ee
          j += 1
        }
      }
      h0, h1, h2, h3, h4 =
        (h1 + c + dd) & MASK, (h2 + d + ee) & MASK,
        (h3 + e + aa) & MASK, (h4 + a + bb) & MASK,
        (h0 + b + cc) & MASK
    end  # until last

    [h0, h1, h2, h3, h4].pack("V5")
  end
end

if __FILE__ == $0
  # Print an example RIPEMD-160 digest.
  str = 'Rosetta Code'
  printf "%s:\n  %s\n", str, *RMD160.rmd160(str).unpack('H*')
end
```



## Scala

```Scala
import org.bouncycastle.crypto.digests.RIPEMD160Digest

object RosettaRIPEMD160 extends App {
  val (raw, messageDigest) = ("Rosetta Code".getBytes("US-ASCII"), new RIPEMD160Digest())
  messageDigest.update(raw, 0, raw.length)
  val out = Array.fill[Byte](messageDigest.getDigestSize())(0)
  messageDigest.doFinal(out, 0)

  assert(out.map("%02x".format(_)).mkString == "b3be159860842cebaa7174c8fff0aa9e50a5199f")
}
```



## Seed7

The Seed7 library [http://seed7.sourceforge.net/libraries/msgdigest.htm msgdigest.s7i] defines
the function [http://seed7.sourceforge.net/libraries/msgdigest.htm#ripemd160(in_var_string) ripemd160],
which computes a RIPEMD-160 message digest. No external library is needed.
The source code of ripemd160 can be found [http://seed7.sourceforge.net/algorith/msgdigest.htm#ripemd160 here].


```seed7
$ include "seed7_05.s7i";
  include "msgdigest.s7i";

const proc: main is func
  begin
    writeln(hex(ripemd160("Rosetta Code")));
  end func;
```


```txt

b3be159860842cebaa7174c8fff0aa9e50a5199f

```



## Swift

Full implementation on [http://github.com/CryptoCoinSwift/RIPEMD-Swift/ Github]. A single block is processed as shown below.

To apply RIPEMD to "Rosetta Code" takes a single block. The message itself is put in the first 3 words. It's followed by 0x80 in the fourth word.The last two UInt32's (words) are used to specify the length of the message in bits.

Everything is in little endian, so "Rose" becomes "esoR" becomes 0x65_73_6f_52

```swift
// Circular left shift: http://en.wikipedia.org/wiki/Circular_shift
// Precendence should be the same as <<
infix operator  ~<< { precedence 160 associativity none }

public func ~<< (lhs: UInt32, rhs: Int) -> UInt32 {
    return (lhs << UInt32(rhs)) | (lhs >> UInt32(32 - rhs));
}

    public struct Block {
        public init() {}

        var message: [UInt32] = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

        // Initial values
        var h₀: UInt32 = 0x67452301
        var h₁: UInt32 = 0xEFCDAB89
        var h₂: UInt32 = 0x98BADCFE
        var h₃: UInt32 = 0x10325476
        var h₄: UInt32 = 0xC3D2E1F0

        public var hash: [UInt32] {
            return [h₀, h₁, h₂, h₃, h₄]
        }

        // FIXME: Make private as soon as tests support that
        public mutating func compress (message: [UInt32]) -> () {
            assert(count(message) == 16, "Wrong message size")

            var Aᴸ = h₀
            var Bᴸ = h₁
            var Cᴸ = h₂
            var Dᴸ = h₃
            var Eᴸ = h₄

            var Aᴿ = h₀
            var Bᴿ = h₁
            var Cᴿ = h₂
            var Dᴿ = h₃
            var Eᴿ = h₄

            for j in 0...79 {
                // Left side
                let wordᴸ = message[r.Left[j]]
                let functionᴸ = f(j)

                let Tᴸ: UInt32 = ((Aᴸ &+ functionᴸ(Bᴸ,Cᴸ,Dᴸ) &+ wordᴸ &+ K.Left[j]) ~<< s.Left[j]) &+ Eᴸ

                Aᴸ = Eᴸ
                Eᴸ = Dᴸ
                Dᴸ = Cᴸ ~<< 10
                Cᴸ = Bᴸ
                Bᴸ = Tᴸ

                // Right side
                let wordᴿ = message[r.Right[j]]
                let functionᴿ = f(79 - j)

                let Tᴿ: UInt32 = ((Aᴿ &+ functionᴿ(Bᴿ,Cᴿ,Dᴿ) &+ wordᴿ &+ K.Right[j]) ~<< s.Right[j]) &+ Eᴿ

                Aᴿ = Eᴿ
                Eᴿ = Dᴿ
                Dᴿ = Cᴿ ~<< 10
                Cᴿ = Bᴿ
                Bᴿ = Tᴿ
            }

            let T = h₁ &+ Cᴸ &+ Dᴿ
            h₁ = h₂ &+ Dᴸ &+ Eᴿ
            h₂ = h₃ &+ Eᴸ &+ Aᴿ
            h₃ = h₄ &+ Aᴸ &+ Bᴿ
            h₄ = h₀ &+ Bᴸ &+ Cᴿ
            h₀ = T
        }

        public func f (j: Int) -> ((UInt32, UInt32, UInt32) -> UInt32) {
            switch j {
            case let index where j < 0:
                assert(false, "Invalid j")
                return {(_, _, _) in 0 }
            case let index where j <= 15:
                return {(x, y, z) in  x ^ y ^ z }
            case let index where j <= 31:
                return {(x, y, z) in  (x & y) | (~x & z) }
            case let index where j <= 47:
                return {(x, y, z) in  (x | ~y) ^ z }
            case let index where j <= 63:
                return {(x, y, z) in  (x & z) | (y & ~z) }
            case let index where j <= 79:
                return {(x, y, z) in  x ^ (y | ~z) }
            default:
                assert(false, "Invalid j")
                return {(_, _, _) in 0 }
            }
        }

        public enum K {
            case Left, Right

            public subscript(j: Int) -> UInt32 {
                switch j {
                case let index where j < 0:
                    assert(false, "Invalid j")
                    return 0
                case let index where j <= 15:
                    return self == .Left ? 0x00000000 : 0x50A28BE6
                case let index where j <= 31:
                    return self == .Left ? 0x5A827999 : 0x5C4DD124
                case let index where j <= 47:
                    return self == .Left ? 0x6ED9EBA1 : 0x6D703EF3
                case let index where j <= 63:
                    return self == .Left ? 0x8F1BBCDC : 0x7A6D76E9
                case let index where j <= 79:
                    return self == .Left ? 0xA953FD4E : 0x00000000
                default:
                    assert(false, "Invalid j")
                    return 0
                    }
            }
        }

        public enum r {
            case Left, Right

            public subscript (j: Int) -> Int {
                switch j {
                case let index where j < 0:
                    assert(false, "Invalid j")
                    return 0
                case let index where j <= 15:
                    if self == .Left {
                        return index
                    } else {
                        return [5,14,7,0,9,2,11,4,13,6,15,8,1,10,3,12][index]
                    }
                case let index where j <= 31:
                    if self == .Left {
                        return [ 7, 4,13, 1,10, 6,15, 3,12, 0, 9, 5, 2,14,11, 8][index - 16]
                    } else {
                        return [ 6,11, 3, 7, 0,13, 5,10,14,15, 8,12, 4, 9, 1, 2][index - 16]
                    }
                case let index where j <= 47:
                    if self == .Left {
                        return [3,10,14,4,9,15,8,1,2,7,0,6,13,11,5,12][index - 32]
                    } else {
                        return [15,5,1,3,7,14,6,9,11,8,12,2,10,0,4,13][index - 32]
                    }
                case let index where j <= 63:
                    if self == .Left {
                        return [1,9,11,10,0,8,12,4,13,3,7,15,14,5,6,2][index - 48]
                    } else {
                        return [8,6,4,1,3,11,15,0,5,12,2,13,9,7,10,14][index - 48]
                    }
                case let index where j <= 79:
                    if self == .Left {
                        return [ 4,0,5,9,7,12,2,10,14,1,3,8,11,6,15,13][index - 64]
                    } else {
                        return [12,15,10,4,1,5,8,7,6,2,13,14,0,3,9,11][index - 64]
                    }

                default:
                    assert(false, "Invalid j")
                    return 0
                }
            }


        }

        public enum s {
            case Left, Right

            public subscript(j: Int) -> Int {
                switch j {
                case let index where j < 0:
                    assert(false, "Invalid j")
                    return 0
                case let index where j <= 15:
                    return (self == .Left ? [11,14,15,12,5,8,7,9,11,13,14,15,6,7,9,8] : [8,9,9,11,13,15,15,5,7,7,8,11,14,14,12,6])[j]
                case let index where j <= 31:
                    return (self == .Left ? [7,6,8,13,11,9,7,15,7,12,15,9,11,7,13,12] : [9,13,15,7,12,8,9,11,7,7,12,7,6,15,13,11])[j - 16]
                case let index where j <= 47:
                    return (self == .Left ? [11,13,6,7,14,9,13,15,14,8,13,6,5,12,7,5] : [9,7,15,11,8,6,6,14,12,13,5,14,13,13,7,5])[j - 32]
                case let index where j <= 63:
                    return (self == .Left ? [11,12,14,15,14,15,9,8,9,14,5,6,8,6,5,12] : [15,5,8,11,14,14,6,14,6,9,12,9,12,5,15,8])[j - 48]
                case let index where j <= 79:
                    return (self == .Left ? [9,15,5,11,6,8,13,12,5,12,13,14,11,8,5,6] : [8,5,12,9,12,5,14,6,8,13,6,5,15,13,11,11])[j - 64]
                default:
                    assert(false, "Invalid j")
                    return 0
                    }
            }

        }



    }
```


Usage:


```swift
var block = Block()
let message:[UInt32] = [ 0x65_73_6f_52, 0x20_61_74_74, 0x65_64_6f_43, 0x00_00_00_80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 96, 0 ]
block.compress(message)
let digest = NSString(format: "%2x%2x%2x%2x%2x", UInt32(bigEndian: block.hash[0]), UInt32(bigEndian: block.hash[1]),UInt32(bigEndian: block.hash[2]), UInt32(bigEndian: block.hash[3]), UInt32(bigEndian: block.hash[4]))
println(digest)
```

```txt
b3be159860842cebaa7174c8fff0aa9e50a5199f
```



## Tcl

```tcl
package require ripemd160

puts [ripemd::ripemd160 -hex "Rosetta Code"]
```

```txt
b3be159860842cebaa7174c8fff0aa9e50a5199f
```



## zkl

Uses shared library zklMsgHash.so

```zkl
var MsgHash=Import("zklMsgHash");
MsgHash.RIPEMD160("Rosetta Code")
```

```txt
b3be159860842cebaa7174c8fff0aa9e50a5199f
```

