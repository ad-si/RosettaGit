+++
title = "Base64 decode data"
description = ""
date = 2019-10-17T05:23:03Z
aliases = []
[extra]
id = 22103
[taxonomies]
categories = []
tags = []
+++

{{task}}

See [[Base64 encode data]].

Now write a program that takes the output of the [[Base64 encode data]] task as input and regenerate the original file.

When working on the VBA implementation I found several 'solutions' on the net, including one from the software maker himself, that showed output with incorrect padding. Obviously with incorrect padding in the output you can not decode correctly to the original file again.


## Arturo



```arturo
use ~crypto

text "VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLSBQYXVsIFIuIEVocmxpY2g="

print $(decodeBase64 text)
```


{{out}}


```txt
To err is human, but to really foul things up you need a computer.
    -- Paul R. Ehrlich
```



## C

{{trans|C++}}

```c
#include <stdio.h>
#include <stdlib.h>

typedef unsigned char ubyte;
const ubyte BASE64[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

int findIndex(const ubyte val) {
    if ('A' <= val && val <= 'Z') {
        return val - 'A';
    }
    if ('a' <= val && val <= 'z') {
        return val - 'a' + 26;
    }
    if ('0' <= val && val <= '9') {
        return val - '0' + 52;
    }
    if (val == '+') {
        return 62;
    }
    if (val == '/') {
        return 63;
    }
    return -1;
}

int decode(const ubyte source[], ubyte sink[]) {
    const size_t length = strlen(source);
    const ubyte *it = source;
    const ubyte *end = source + length;
    int acc;

    if (length % 4 != 0) {
        return 1;
    }

    while (it != end) {
        const ubyte b1 = *it++;
        const ubyte b2 = *it++;
        const ubyte b3 = *it++;         // might be the first padding byte
        const ubyte b4 = *it++;         // might be the first or second padding byte

        const int i1 = findIndex(b1);
        const int i2 = findIndex(b2);

        acc = i1 << 2;                  // six bits came from the first byte
        acc |= i2 >> 4;                 // two bits came from the first byte
        *sink++ = acc;                  // output the first byte

        if (b3 != '=') {
            const int i3 = findIndex(b3);

            acc = (i2 & 0xF) << 4;      // four bits came from the second byte
            acc += i3 >> 2;             // four bits came from the second byte
            *sink++ = acc;              // output the second byte

            if (b4 != '=') {
                const int i4 = findIndex(b4);

                acc = (i3 & 0x3) << 6;  // two bits came from the third byte
                acc |= i4;              // six bits came from the third byte
                *sink++ = acc;          // output the third byte
            }
        }
    }

    *sink = '\0';   // add the sigil for end of string
    return 0;
}

int main() {
    ubyte data[] = "VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLVBhdWwgUi5FaHJsaWNo";
    ubyte decoded[1024];

    printf("%s\n\n", data);
    decode(data, decoded);
    printf("%s\n\n", decoded);

    return 0;
}
```

{{out}}

```txt
VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLVBhdWwgUi5FaHJsaWNo

To err is human, but to really foul things up you need a computer.
    --Paul R.Ehrlich
```



## C++


```cpp
#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

typedef unsigned char ubyte;
const auto BASE64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

std::vector<ubyte> encode(const std::vector<ubyte>& source) {
    auto it = source.cbegin();
    auto end = source.cend();

    std::vector<ubyte> sink;
    while (it != end) {
        auto b1 = *it++;
        int acc;

        sink.push_back(BASE64[b1 >> 2]);            // first output (first six bits from b1)

        acc = (b1 & 0x3) << 4;                      // last two bits from b1
        if (it != end) {
            auto b2 = *it++;
            acc |= (b2 >> 4);                       // first four bits from b2

            sink.push_back(BASE64[acc]);            // second output

            acc = (b2 & 0xF) << 2;                  // last four bits from b2
            if (it != end) {
                auto b3 = *it++;
                acc |= (b3 >> 6);                   // first two bits from b3

                sink.push_back(BASE64[acc]);        // third output
                sink.push_back(BASE64[b3 & 0x3F]);  // fouth output (final six bits from b3)
            } else {
                sink.push_back(BASE64[acc]);        // third output
                sink.push_back('=');                // fourth output (1 byte padding)
            }
        } else {
            sink.push_back(BASE64[acc]);            // second output
            sink.push_back('=');                    // third output (first padding byte)
            sink.push_back('=');                    // fourth output (second padding byte)
        }
    }
    return sink;
}

int findIndex(ubyte val) {
    if ('A' <= val && val <= 'Z') {
        return val - 'A';
    }
    if ('a' <= val && val <= 'z') {
        return val - 'a' + 26;
    }
    if ('0' <= val && val <= '9') {
        return val - '0' + 52;
    }
    if ('+' == val) {
        return 62;
    }
    if ('/' == val) {
        return 63;
    }
    return -1;
}

std::vector<ubyte> decode(const std::vector<ubyte>& source) {
    if (source.size() % 4 != 0) {
        throw new std::runtime_error("Error in size to the decode method");
    }

    auto it = source.cbegin();
    auto end = source.cend();

    std::vector<ubyte> sink;
    while (it != end) {
        auto b1 = *it++;
        auto b2 = *it++;
        auto b3 = *it++; // might be first padding byte
        auto b4 = *it++; // might be first or second padding byte

        auto i1 = findIndex(b1);
        auto i2 = findIndex(b2);
        int acc;

        acc = i1 << 2;          // six bits came from the first byte
        acc |= i2 >> 4;         // two bits came from the first byte

        sink.push_back(acc);    // output the first byte

        if (b3 != '=') {
            auto i3 = findIndex(b3);

            acc = (i2 & 0xF) << 4;  // four bits came from the second byte
            acc |= i3 >> 2;         // four bits came from the second byte

            sink.push_back(acc);    // output the second byte

            if (b4 != '=') {
                auto i4 = findIndex(b4);

                acc = (i3 & 0x3) << 6;  // two bits came from the third byte
                acc |= i4;              // six bits came from the third byte

                sink.push_back(acc);    // output the third byte
            }
        }
    }
    return sink;
}

int main() {
    using namespace std;

    string data = "VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLVBhdWwgUi5FaHJsaWNo";
    vector<ubyte> datav{ begin(data), end(data) };
    cout << data << "\n\n";

    auto decoded = decode(datav);
    std::for_each(cbegin(decoded), cend(decoded), [](char c) { cout << c; });
    cout << '\n';

    return 0;
}
```

{{out}}

```txt
VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLVBhdWwgUi5FaHJsaWNo

To err is human, but to really foul things up you need a computer.
    --Paul R.Ehrlich
```


=={{header|C#|C_sharp}}==
{{trans|Visual Basic .NET}}

```csharp
using System;
using System.Text;

namespace Base64DecodeData {
    class Program {
        static void Main(string[] args) {
            var data = "VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLSBQYXVsIFIuIEVocmxpY2g=";
            Console.WriteLine(data);
            Console.WriteLine();

            var decoded = Encoding.ASCII.GetString(Convert.FromBase64String(data));
            Console.WriteLine(decoded);
        }
    }
}
```

{{out}}

```txt
VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLSBQYXVsIFIuIEVocmxpY2g=

To err is human, but to really foul things up you need a computer.
    -- Paul R. Ehrlich
```



## D

{{trans|Perl 6}}

```d
import std.base64;
import std.stdio;

void main() {
    auto data = "VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLSBQYXVsIFIuIEVocmxpY2g=";
    writeln(data);
    writeln;

    auto decoded = cast(char[])Base64.decode(data);
    writeln(decoded);
}
```

{{out}}

```txt
VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLSBQYXVsIFIuIEVocmxpY2g=

To err is human, but to really foul things up you need a computer.
    -- Paul R. Ehrlich
```



## Factor


```factor
USING: base64 io strings ;

"VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLVBhdWwgUi5FaHJsaWNo"
base64> >string print
```

{{out}}

```txt

To err is human, but to really foul things up you need a computer.
    --Paul R.Ehrlich

```



## F#


### Standard Library


```fsharp

open System
open System.IO

let encoded = "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"

let decoded = Convert.FromBase64String encoded

File.WriteAllBytes("favicon.ico", decoded)

```

{{out}}
[https://rosettacode.org/favicon.ico Rosetta Code Icon]


### Manual Implementation


```fsharp

open System
open System.IO

let encoded = "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"


let decode (s: string) =
    let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".ToCharArray()
    let filtered = String.filter (fun c -> Array.contains c chars || c = '=') s
    let paddingSize = filtered |> String.filter ((=) '=') |> String.length
    let s = filtered.Replace('=', 'A').ToCharArray() |> Array.map int
    let ints = chars |> Array.map int
    let calc c =
        let n = [c..(c + 3)]
                |> List.sumBy (fun k -> Array.IndexOf(ints, s.[k]) <<< (18 - 6 * (k % 4)))
        [16; 8; 0]
        |> List.map (fun k -> (n >>> k) &&& 255 |> char |> string) |> List.reduce (+)
    [0..4..Array.length s - 1]
    |> List.map calc
    |> List.reduce (+)
    |> fun r -> r.Substring(0, String.length r - paddingSize).ToCharArray()
    |> Array.map byte

let decoded = decode encoded

File.WriteAllBytes("favicon.ico", decoded)

```

{{out}}
[https://rosettacode.org/favicon.ico Rosetta Code Icon]


## Go

As images can no longer be uploaded to RC, I've encoded and decoded a string rather than the Rosetta Code icon.

```go
package main

import (
    "encoding/base64"
    "fmt"
)

func main() {
    msg := "Rosetta Code Base64 decode data task"
    fmt.Println("Original :", msg)
    encoded := base64.StdEncoding.EncodeToString([]byte(msg))
    fmt.Println("\nEncoded  :", encoded)
    decoded, err := base64.StdEncoding.DecodeString(encoded)
    if err != nil {
        fmt.Println(err)
        return
    }
    fmt.Println("\nDecoded  :", string(decoded))
}
```


{{out}}

```txt

Original : Rosetta Code Base64 decode data task

Encoded  : Um9zZXR0YSBDb2RlIEJhc2U2NCBkZWNvZGUgZGF0YSB0YXNr

Decoded  : Rosetta Code Base64 decode data task

```



## Haskell

Simple implementation that decodes an ASCII string.

```haskell
--Decodes Base64 to ASCII
import qualified Data.Map.Strict as Map (Map, lookup, fromList)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Numeric (readInt, showIntAtBase)
import Data.Char (chr, digitToInt)
import Data.List.Split (chunksOf)

byteToASCII :: String -> String
byteToASCII = map chr . decoder

--generates list of bytes (represented by Int)
decoder :: String -> [Int]
decoder =
  map readBin .
  takeWhile (\x -> length x == 8) .
  chunksOf 8 . concatMap toBin . mapMaybe (`Map.lookup` table) . filter (/= '=')

--turns decimal into a list of char that represents a binary number
toBin :: Int -> String
toBin n = leftPad $ showIntAtBase 2 ("01" !!) n ""

--this adds all the zeros to the left that showIntAtBase omitted
leftPad :: String -> String
leftPad a = replicate (6 - length a) '0' ++ a

--turns list of '0' and '1' into list of 0 and 1
readBin :: String -> Int
readBin = fromJust . fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

--lookup list for the sextets
table :: Map.Map Char Int
table =
  Map.fromList $
  zip "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" [0 ..]

main :: IO ()
main =
  putStrLn $
  byteToASCII
    "VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCi0tIFBhdWwgUi4gRWhybGljaA=="
```

{{Out}}

```txt
To err is human, but to really foul things up you need a computer.
-- Paul R. Ehrlich
```




or in terms of Data.ByteString.Base64:


```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Base64 as Base64 (decode, encode)
import qualified Data.ByteString.Char8 as B (putStrLn)

main :: IO ()
main = do
  B.putStrLn $
    Base64.encode
      "To err is human, but to really foul things up you need a computer.\n-- Paul R. Ehrlich"
  B.putStrLn "\n-->\n"
  either print B.putStrLn $
    Base64.decode
      "VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCi0tIFBhdWwgUi4gRWhybGljaA=="
```

{{Out}}

```txt
VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCi0tIFBhdWwgUi4gRWhybGljaA==

-->

To err is human, but to really foul things up you need a computer.
-- Paul R. Ehrlich
```



## Jsish

See [[Base64_encode_data#Jsish]] for ''base64.jsi''.


```javascript
/* Base64 decode, in Jsish */
var data = exec('jsish base64.jsi', {retAll:true}).data;  // or use File.read('stdin');
var icon = Util.base64(data, true);
File.write('rosetta-favicon.ico', icon);
```




## Julia

Using an IOBuffer here, though not really needed to decode a string, shows how we could pipe a network stream or file though Julia's builtin Base64 decoder.

```julia
using Base64

io = IOBuffer()

iob64_decode = Base64DecodePipe(io)

write(io, "VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLVBhdWwgUi5FaHJsaWNo")

seekstart(io)

println(String(read(iob64_decode)))

```
{{out}}

```txt

To err is human, but to really foul things up you need a computer.
    --Paul R.Ehrlich

```



## Kotlin

{{trans|D}}

```scala
import java.util.Base64

fun main() {
    val data =
        "VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLSBQYXVsIFIuIEVocmxpY2g="
    val decoder = Base64.getDecoder()
    val decoded = decoder.decode(data)
    val decodedStr = String(decoded, Charsets.UTF_8)
    println(decodedStr)
}
```

{{out}}

```txt
To err is human, but to really foul things up you need a computer.
    -- Paul R. Ehrlich
```



## Mathematica


```Mathematica
ImportString[
   "VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLVBhdWwgUi5FaHJsaWNo",
   "Base64"
]
```

{{out}}

```txt
To err is human, but to really foul things up you need a computer.
    -- Paul R. Ehrlich
```




## OCaml

We continue from the same toplevel session [[Base64_encode_data#OCaml|from the previous page]]:

```txt

# let plain = Base64.decode_exn enc;;
val plain : string =
  "\000\000\001\000\002\000\016\016\000\000\000\000\000\000h\005\000\000&\000"... (* string length 3638; truncated *)

```



## Perl

The MIME::Base64 module is to be preferred, but this works too.

```perl
sub decode_base64 {
    my($d) = @_;
    $d =~ tr!A-Za-z0-9+/!!cd;
    $d =~ s/=+$//;
    $d =~ tr!A-Za-z0-9+/! -_!;
    my $r = '';
    while( $d =~ /(.{1,60})/gs ){
        my $len = chr(32 + length($1)*3/4);
        $r .= unpack("u", $len . $1 );
    }
    $r;
}

$data = <<EOD;
J1R3YXMgYnJpbGxpZywgYW5kIHRoZSBzbGl0aHkgdG92ZXMKRGlkIGd5cmUgYW5kIGdpbWJsZSBp
biB0aGUgd2FiZToKQWxsIG1pbXN5IHdlcmUgdGhlIGJvcm9nb3ZlcywKQW5kIHRoZSBtb21lIHJh
dGhzIG91dGdyYWJlLgo=
EOD

print decode_base64($data) . "\n";
```

{{out}}

```txt
'Twas brillig, and the slithy toves
Did gyre and gimble in the wabe:
All mimsy were the borogoves,
And the mome raths outgrabe.
```



## Perl 6

{{works with|Rakudo|2018.11}}


```perl6
my $e64 = '
VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY2
9tcHV0ZXIuCiAgICAtLSBQYXVsIFIuIEVocmxpY2g=
';

my @base64map = flat 'A' .. 'Z', 'a' .. 'z', ^10, '+', '/';
my %base64 is default(0) = @base64map.pairs.invert;

sub base64-decode-slow ($enc) {
    my $buf = Buf.new;
    for $enc.subst(/\s/, '', :g).comb(4) -> $chunck {
        $buf.append: |(sprintf "%06d%06d%06d%06d", |$chunck.comb.map:
            {%base64{$_}.base(2)}).comb(8).map: {:2($_)};
    }
    $buf
}

say 'Slow:';
say base64-decode-slow($e64).decode('utf8');


# Of course, the above routine is slow and is only for demonstration purposes.
# For real code you should use a module, which is MUCH faster and heavily tested.
say "\nFast:";
use Base64::Native;
say base64-decode($e64).decode('utf8');
```

{{out}}

```txt
Slow:
To err is human, but to really foul things up you need a computer.
    -- Paul R. Ehrlich

Fast:
To err is human, but to really foul things up you need a computer.
    -- Paul R. Ehrlich
```



## Phix


```Phix
include builtins\base64.e
string s = "Rosetta Code Base64 decode data task"
string e = encode_base64(s)
?e
?decode_base64(e)
```

{{out}}

```txt

"Um9zZXR0YSBDb2RlIEJhc2U2NCBkZWNvZGUgZGF0YSB0YXNr"
"Rosetta Code Base64 decode data task"

```



## PicoLisp


```PicoLisp
(setq *Char64
   `'(chop
         "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" ) )
(de decode64 (S)
   (let S (chop S)
      (pack
         (make
            (while S
               (let
                  (A (dec (index (++ S) *Char64))
                     B (dec (index (++ S) *Char64))
                     C (dec (index (++ S) *Char64))
                     D (dec (index (++ S) *Char64)) )
                  (link
                     (char (| (>> -2 A) (>> 4 B))) )
                  (and
                     C
                     (link
                        (char
                           (| (>> -4 (& B 15)) (>> 2 C)) ) )
                     D
                     (link
                        (char (| (>> -6 (& C 3)) D)) ) ) ) ) ) ) ) )
(prinl (decode64 "VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLVBhdWwgUi5FaHJsaWNo"))
```

{{out}}

```txt

To err is human, but to really foul things up you need a computer.
    --Paul R.Ehrlich

```



## Prolog

In SWI-Prolog base64 is a built in predicate. https://www.swi-prolog.org/pldoc/doc_for?object=base64%3Abase64/2

This predicate is reversable and can encode or decode.
{{out}}

```txt

?- Encoded = 'VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLSBQYXVsIFIuIEVocmxpY2g=',
base64(Plain, Encoded).

Plain = 'To err is human, but to really foul things up you need a computer.\n    -- Paul R. Ehrlich'.

```



## Python


```Python

import base64
data = 'VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLSBQYXVsIFIuIEVocmxpY2g='
print(base64.b64decode(data).decode('utf-8'))

```

{{out}}

```txt

To err is human, but to really foul things up you need a computer.
    -- Paul R. Ehrlich

```



## Ruby


```ruby
require 'base64'

perl6_example ='
VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY2
9tcHV0ZXIuCiAgICAtLSBQYXVsIFIuIEVocmxpY2g=
'
puts Base64.decode64 perl6_example
```

{{out}}

```txt

To err is human, but to really foul things up you need a computer.
    -- Paul R. Ehrlich

```



## Rust


```rust
use std::str;

const INPUT: &str = "VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLVBhdWwgUi5FaHJsaWNo";
const UPPERCASE_OFFSET: i8 = -65;
const LOWERCASE_OFFSET: i8 = 26 - 97;
const NUM_OFFSET: i8 = 52 - 48;

fn main() {
    println!("Input: {}", INPUT);

    let result = INPUT.chars()
        .filter(|&ch| ch != '=')                                //Filter '=' chars
        .map(|ch| {                                             //Map char values using Base64 Characters Table
            let ascii = ch as i8;
            let convert = match ch {
                '0' ... '9' => ascii + NUM_OFFSET,
                'a' ... 'z' => ascii + LOWERCASE_OFFSET,
                'A' ... 'Z' => ascii + UPPERCASE_OFFSET,
                '+' => 62,
                '/' => 63,
                _ => panic!("Not a valid base64 encoded string")
            };
            format!("{:#08b}", convert)[2..].to_string()        //convert indices to binary format and remove the two first digits
        })
        .collect::<String>()                                    //concatenate the resulting binary values
        .chars()
        .collect::<Vec<char>>()
        .chunks(8)                                              //split into 8 character chunks
        .map(|chunk| {
            let num_str = chunk.iter().collect::<String>();
            usize::from_str_radix(&num_str, 2).unwrap() as u8   //convert the binary string into its u8 value
        })
        .collect::<Vec<_>>();

    let result = str::from_utf8(&result).unwrap();              //convert into UTF-8 string

    println!("Output: {}", result);
}
```

{{out}}

```txt

Input: VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLVBhdWwgUi5FaHJsaWNo
Output: To err is human, but to really foul things up you need a computer.
    --Paul R.Ehrlich

```



## Scala

{{Out}}Best seen in running your browser either by [https://scalafiddle.io/sf/mjgxJDp/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/P4RfGhRQSkaKWEmdBi1gaw Scastie (remote JVM)].

```Scala
import java.util.Base64

object Base64Decode extends App {

  def text2BinaryDecoding(encoded: String): String = {
    val decoded = Base64.getDecoder.decode(encoded)
    new String(decoded, "UTF-8")
  }

  def data =
    "VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLSBQYXVsIFIuIEVocmxpY2g="

  println(text2BinaryDecoding(data))
}
```



## Seed7

The Seed7 library [http://seed7.sourceforge.net/libraries/encoding.htm encoding.s7i] defines
the functions [http://seed7.sourceforge.net/libraries/encoding.htm#toBase64(in_string) toBase64] and
[http://seed7.sourceforge.net/libraries/encoding.htm#fromBase64(in_string) fromBase64].


```seed7
$ include "seed7_05.s7i";
  include "gethttp.s7i";
  include "encoding.s7i";

const proc: main is func
  local
    var string: original is "";
    var string: encoded is "";
  begin
    original := getHttp("rosettacode.org/favicon.ico");
    encoded := toBase64(original);
    writeln("Is the Rosetta Code icon the same (byte for byte) encoded then decoded: " <&
            fromBase64(encoded) = original);
  end func;
```


{{out}}

```txt
Is the Rosetta Code icon the same (byte for byte) encoded then decoded: TRUE
```



## Sidef


```ruby
var data = <<'EOT'
VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY2
9tcHV0ZXIuCiAgICAtLSBQYXVsIFIuIEVocmxpY2g=
EOT

say data.decode_base64
```

{{out}}

```txt

To err is human, but to really foul things up you need a computer.
    -- Paul R. Ehrlich

```



## Visual Basic .NET

{{trans|D}}

```vbnet
Module Module1

    Sub Main()
        Dim data = "VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLSBQYXVsIFIuIEVocmxpY2g="
        Console.WriteLine(data)
        Console.WriteLine()

        Dim decoded = Text.Encoding.ASCII.GetString(Convert.FromBase64String(data))
        Console.WriteLine(decoded)
    End Sub

End Module
```

{{out}}

```txt
VG8gZXJyIGlzIGh1bWFuLCBidXQgdG8gcmVhbGx5IGZvdWwgdGhpbmdzIHVwIHlvdSBuZWVkIGEgY29tcHV0ZXIuCiAgICAtLSBQYXVsIFIuIEVocmxpY2g=

To err is human, but to really foul things up you need a computer.
    -- Paul R. Ehrlich
```



## zkl

Using shared libraries for cURL and message hashing:

```zkl
var [const] MsgHash=Import("zklMsgHash"), Curl=Import("zklCurl");

icon:=Curl().get("http://rosettacode.org/favicon.ico"); //-->(Data(4,331),693,0)
icon=icon[0][icon[1],*];	// remove header
iconEncoded:=MsgHash.base64encode(icon);
iconDecoded:=MsgHash.base64decode(iconEncoded);
File("rosettaCodeIcon.ico","wb").write(iconDecoded); # eyeball checking says good
println("Is the Rosetta Code icon the same (byte for byte) encoded then decoded: ",
   icon==iconDecoded);
```

{{out}}

```txt

Is the Rosetta Code icon the same (byte for byte) encoded then decoded: True

```

{{out|Text based test}}

```zkl
msg,b64 := "Rosetta Code Base64 decode data task", MsgHash.base64encode(msg);
println("Original: %s\nEncoded:  %s\nBytes:    %s\nDecoded:  %s"
   .fmt(msg, b64.text, b64.bytes().apply("toString",16).concat(","),
        MsgHash.base64decode(b64).text));
```


```txt

Original: Rosetta Code Base64 decode data task
Encoded:  Um9zZXR0YSBDb2RlIEJhc2U2NCBkZWNvZGUgZGF0YSB0YXNr

Bytes:    55,6d,39,7a,5a,58,52,30,59,53,42,44,62,32,52,6c,49,45,4a,68,63,32,55,32,4e,43,42,6b,5a,57,4e,76,5a,47,55,67,5a,47,46,30,59,53,42,30,59,58,4e,72,a
Decoded:  Rosetta Code Base64 decode data task

```

