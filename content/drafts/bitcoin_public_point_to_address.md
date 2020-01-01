+++
title = "Bitcoin/public point to address"
description = ""
date = 2019-05-24T07:49:50Z
aliases = []
[extra]
id = 12633
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[wp:Bitcoin|Bitcoin]] uses a specific encoding format to encode the digest of an [[wp:elliptic curve|elliptic curve]] public point into a short ASCII string.  The purpose of this task is to perform such a conversion.

The encoding steps are:
* take the X and Y coordinates of the given public point, and concatenate them in order to have a 64 byte-longed string ;
* add one byte prefix equal to 4 (it is a convention for this way of encoding a public point) ;
* compute the [[SHA-256]] of this string ;
* compute the [[RIPEMD-160]] of this SHA-256 digest ;
* compute the checksum of the concatenation of the version number digit (a single zero byte) and this RIPEMD-160 digest, as described in [[bitcoin/address validation]] ;
* Base-58 encode (see below) the concatenation of the version number (zero in this case), the ripemd digest and the checksum

The base-58 encoding is based on an alphabet of alphanumeric characters (numbers, upper case and lower case, in that order) but without the four characters 0, O, l and I.

Here is an example public point:

```txt
X = 0x50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352
Y = 0x2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6
```


The corresponding address should be:
16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM

Nb.  The leading '1' is not significant as 1 is zero in base-58.  It is however often added to the bitcoin address for various reasons.  There can actually be several of them.  You can ignore this and output an address without the leading 1.

''Extra credit:'' add a verification procedure about the public point, making sure it belongs to the secp256k1 elliptic curve


## C


```c
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <openssl/sha.h>
#include <openssl/ripemd.h>

#define COIN_VER 0
const char *coin_err;

typedef unsigned char byte;

int is_hex(const char *s) {
	int i;
	for (i = 0; i < 64; i++)
		if (!isxdigit(s[i])) return 0;
	return 1;
}

void str_to_byte(const char *src, byte *dst, int n) {
	while (n--) sscanf(src + n * 2, "%2hhx", dst + n);
}

char* base58(byte *s, char *out) {
	static const char *tmpl = "123456789"
		"ABCDEFGHJKLMNPQRSTUVWXYZ"
		"abcdefghijkmnopqrstuvwxyz";
	static char buf[40];

	int c, i, n;
	if (!out) out = buf;

	out[n = 34] = 0;
	while (n--) {
		for (c = i = 0; i < 25; i++) {
			c = c * 256 + s[i];
			s[i] = c / 58;
			c %= 58;
		}
		out[n] = tmpl[c];
	}

	for (n = 0; out[n] == '1'; n++);
	memmove(out, out + n, 34 - n);

	return out;
}

char *coin_encode(const char *x, const char *y, char *out) {
	byte s[65];
	byte rmd[5 + RIPEMD160_DIGEST_LENGTH];

	if (!is_hex(x) || !(is_hex(y))) {
		coin_err = "bad public point string";
		return 0;
	}

	s[0] = 4;
	str_to_byte(x, s + 1, 32);
	str_to_byte(y, s + 33, 32);

	rmd[0] = COIN_VER;
	RIPEMD160(SHA256(s, 65, 0), SHA256_DIGEST_LENGTH, rmd + 1);

	memcpy(rmd + 21, SHA256(SHA256(rmd, 21, 0), SHA256_DIGEST_LENGTH, 0), 4);

	return base58(rmd, out);
}

int main(void) {
	puts(coin_encode(
		"50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352",
		"2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6",
		0));
	return 0;
}
```



## D

Requires the second D module from the SHA-256 task.
{{trans|C}}
{{trans|Go}}

```d
import std.stdio, std.algorithm, std.digest.ripemd, sha_256_2;

// A Bitcoin public point.
struct PPoint { ubyte[32] x, y; }

private enum bitcoinVersion = 0;
private enum RIPEMD160_digest_len = typeof("".ripemd160Of).length;
private alias sha = SHA256.digest;
alias Address = ubyte[1 + 4 + RIPEMD160_digest_len];


/// Returns a base 58 encoded bitcoin address corresponding to the receiver.
char[] toBase58(ref Address a) pure nothrow @safe {
    static immutable symbols = "123456789" ~
                               "ABCDEFGHJKLMNPQRSTUVWXYZ" ~
                               "abcdefghijkmnopqrstuvwxyz";
    static assert(symbols.length == 58);

    auto result = new typeof(return)(34);
    foreach_reverse (ref ri; result) {
        uint c = 0;
        foreach (ref ai; a) {
            immutable d = (c % symbols.length) * 256 + ai;
            ai = d / symbols.length;
            c = d;
        }
        ri = symbols[c % symbols.length];
    }

    size_t i = 1;
    for (; i < result.length && result[i] == '1'; i++) {}
    return result[i - 1 .. $];
}


char[] bitcoinEncode(in ref PPoint p) pure nothrow {
    ubyte[typeof(PPoint.x).length + typeof(PPoint.y).length + 1] s;
    s[0] = 4;
    s[1 .. 1 + p.x.length] = p.x[];
    s[1 + p.x.length .. $] = p.y[];

    Address rmd;
    rmd[0] = bitcoinVersion;
    rmd[1 .. RIPEMD160_digest_len + 1] = s.sha.ripemd160Of;
    rmd[$-4 .. $] = rmd[0 .. RIPEMD160_digest_len + 1].sha.sha[0 .. 4];
    return rmd.toBase58;
}


void main() {
    PPoint p = {
cast(typeof(PPoint.x))
x"50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352",
cast(typeof(PPoint.y))
x"2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6"};

    p.bitcoinEncode.writeln;
}
```

{{out}}

```txt
16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
```


## Factor


```factor
USING: checksums checksums.ripemd checksums.sha io.binary kernel
math sequences ;
IN: rosetta-code.bitcoin.point-address

CONSTANT: ALPHABET "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

: btc-checksum ( bytes -- checksum-bytes )
    2 [ sha-256 checksum-bytes ] times 4 head ;

: bigint>base58 ( n -- str )
    33 [ 58 /mod ALPHABET nth ] "" replicate-as reverse nip ;

: >base58 ( bytes -- str )
    be> bigint>base58 ;

: point>address ( X Y -- address )
    [ 32 >be ] bi@ append
    0x4 prefix
    sha-256 checksum-bytes
    ripemd-160 checksum-bytes
    dup 0 prefix btc-checksum
    append 0 prefix >base58 ;

```

{{out}}

```txt

0x50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352
0x2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6
point>address . ! "16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM"

```


## Go

{{libheader|Go sub-repositories}}

```go
package main

import (
    "crypto/sha256"
    "encoding/hex"
    "errors"
    "fmt"

    "golang.org/x/crypto/ripemd160"
)

// Point is a type for a bitcoin public point.
type Point struct {
    x, y [32]byte
}

// SetHex takes two hexidecimal strings and decodes them into the receiver.
func (p *Point) SetHex(x, y string) error {
    if len(x) != 64 || len(y) != 64 {
        return errors.New("invalid hex string length")
    }
    if _, err := hex.Decode(p.x[:], []byte(x)); err != nil {
        return err
    }
    _, err := hex.Decode(p.y[:], []byte(y))
    return err
}

// A25 type in common with Bitcoin/address validation task.
type A25 [25]byte

// doubleSHA256 method in common with Bitcoin/address validation task.
func (a *A25) doubleSHA256() []byte {
    h := sha256.New()
    h.Write(a[:21])
    d := h.Sum([]byte{})
    h = sha256.New()
    h.Write(d)
    return h.Sum(d[:0])
}

// UpdateChecksum computes the address checksum on the first 21 bytes and
// stores the result in the last 4 bytes.
func (a *A25) UpdateChecksum() {
    copy(a[21:], a.doubleSHA256())
}

// SetPoint takes a public point and generates the corresponding address
// into the receiver, complete with valid checksum.
func (a *A25) SetPoint(p *Point) {
    c := [65]byte{4}
    copy(c[1:], p.x[:])
    copy(c[33:], p.y[:])
    h := sha256.New()
    h.Write(c[:])
    s := h.Sum([]byte{})
    h = ripemd160.New()
    h.Write(s)
    h.Sum(a[1:1])
    a.UpdateChecksum()
}

// Tmpl in common with Bitcoin/address validation task.
var tmpl = []byte("123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")

// A58 returns a base58 encoded bitcoin address corresponding to the receiver.
// Code adapted from the C solution to this task.
func (a *A25) A58() []byte {
    var out [34]byte
    for n := 33; n >= 0; n-- {
        c := 0
        for i := 0; i < 25; i++ {
            c = c*256 + int(a[i])
            a[i] = byte(c / 58)
            c %= 58
        }
        out[n] = tmpl[c]
    }
    i := 1
    for i < 34 && out[i] == '1' {
        i++
    }
    return out[i-1:]
}

func main() {
    // parse hex into point object
    var p Point
    err := p.SetHex(
        "50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352",
        "2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6")
    if err != nil {
        fmt.Println(err)
        return
    }
    // generate address object from point
    var a A25
    a.SetPoint(&p)
    // show base58 representation
    fmt.Println(string(a.A58()))
}
```

{{out}}

```txt

16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM

```


## Haskell


```haskell
import Numeric (showIntAtBase)
import Data.List (unfoldr)
import Data.Binary (Word8)
import Crypto.Hash.SHA256 as S (hash)
import Crypto.Hash.RIPEMD160 as R (hash)
import Data.ByteString (unpack, pack)

publicPointToAddress :: Integer -> Integer -> String
publicPointToAddress x y =
  let toBytes x = reverse $ unfoldr (\b -> if b == 0 then Nothing else Just (fromIntegral $ b `mod` 256, b `div` 256)) x
      ripe = 0 : unpack (R.hash $ S.hash $ pack $ 4 : toBytes x ++ toBytes y)
      ripe_checksum = take 4 $ unpack $ S.hash $ S.hash $ pack ripe
      addressAsList = ripe ++ ripe_checksum
      address = foldl (\v b -> v * 256 + fromIntegral b) 0 addressAsList
      base58Digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  in showIntAtBase 58 (base58Digits !!) address ""

main = print $ publicPointToAddress
  0x50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352
  0x2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6

```

{{out}}

```txt
"6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM"

```



## Julia

{{works with|Julia|0.6}}
{{trans|C}}

'''Main functions''':

```julia
const digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
function encodebase58(b::Vector{<:Integer})
    out = Vector{Char}(34)
    for j in endof(out):-1:1
        local c::Int = 0
        for i in eachindex(b)
            c = c * 256 + b[i]
            b[i] = c ÷ 58
            c %= 58
        end
        out[j] = digits[c + 1]
    end
    local i = 1
    while i < endof(out) && out[i] == '1'
        i += 1
    end
    return join(out[i:end])
end

const COINVER = 0x00
function pubpoint2address(x::Vector{UInt8}, y::Vector{UInt8})
    c = vcat(0x04, x, y)
    c = vcat(COINVER, digest("ripemd160", sha256(c)))
    d = sha256(sha256(c))
    return encodebase58(vcat(c, d[1:4]))
end
pubpoint2address(x::AbstractString, y::AbstractString) =
    pubpoint2address(hex2bytes(x), hex2bytes(y))
```


'''Main''':

```julia
x = "50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352"
y = "2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6"
println(pubpoint2address(x, y)))
```


{{out}}

```txt
6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
```



## Nim


{{libheader|base58|0.1.1}}
{{libheader|nimcrypto|0.3.8}}
{{works with|Nim Compiler|0.19.4}}

These libraries can be found on <code>nimble</code>.


```nim
import parseutils
import base58 / bitcoin
import nimcrypto / [hash, sha2, ripemd]

func hexToByteSeq(s: string): seq[byte] =
  var pos = 0
  while pos < s.len:
    var tmp = 0
    let parsed = parseHex(s, tmp, pos, 2)
    if parsed > 0:
      inc pos, parsed
      result.add byte tmp
    else:
      raise newException(ValueError, "Invalid hex string")

func addrEncode(x, y: string): string =
  let pubPoint = 4u8 & x.hexToByteSeq & y.hexToByteSeq
  if pubPoint.len != 65:
    raise newException(ValueError, "Invalid pubpoint string")
  var rmd = @(ripemd160.digest(sha256.digest(pubPoint).data).data)
  rmd.insert 0u8
  rmd.add sha256.digest(sha256.digest(rmd).data).data[0..3]
  result = encode cast[string](rmd)

when isMainModule:
  echo addrEncode("50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352",
                  "2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6")
```


{{out}}

```txt
16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
```



## Perl

Here we'll use the standard Digest::SHA module, and the CPAN-available Crypt::RIPEMD160 and Encode::Base58::GMP.

```perl

use Crypt::RIPEMD160;
use Digest::SHA qw(sha256);
use Encode::Base58::GMP;

sub public_point_to_address {
    my $ec   = join '', '04', @_;                    # EC: concat x and y to one string and prepend '04' magic value

    my $octets   = pack 'C*', map { hex } unpack('(a2)65', $ec);      # transform the hex values string to octets
    my $hash     = chr(0) . Crypt::RIPEMD160->hash(sha256 $octets);   # perform RIPEMD160(SHA256(octets)
    my $checksum = substr sha256(sha256 $hash), 0, 4;                 # build the checksum
    my $hex      = join '', '0x',                                     # build hex value of hash and checksum
                   map { sprintf "%02X", $_ }
                   unpack 'C*', $hash.$checksum;
    return '1' . sprintf "%32s", encode_base58($hex, 'bitcoin');      # Do the Base58 encoding, prepend "1"
}

say public_point_to_address
    '50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352',
    '2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6'
    ;

```

{{out}}

```txt
16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
```



## Perl 6



```perl6
use Digest::SHA;
use Digest::RIPEMD;

constant BASE58 = <
      1 2 3 4 5 6 7 8 9
    A B C D E F G H   J K L M N   P Q R S T U V W X Y Z
    a b c d e f g h i j k   m n o p q r s t u v w x y z
>;

sub encode( UInt $n ) {
    [R~] BASE58[ $n.polymod: 58 xx * ]
}

sub public_point_to_address( UInt $x, UInt $y ) {
    my @bytes = (
        |$y.polymod( 256 xx 32 )[^32], # ignore the extraneous 33rd modulus
        |$x.polymod( 256 xx 32 )[^32],
    );
    my $hash = rmd160 sha256 Blob.new: 4, @bytes.reverse;
    my $checksum = sha256(sha256 Blob.new: 0, $hash.list).subbuf: 0, 4;
    encode reduce * * 256 + * , flat 0, ($hash, $checksum)».list
}

say public_point_to_address
0x50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352,
0x2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6;
```

{{out}}

```txt
6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM
```



## Phix


```Phix
include builtins\sha256.e
include builtins\ripemd160.e

constant b58 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

function base58(string s)
string out = ""
    if length(s)!=25 then ?9/0 end if
    for n=1 to 34 do
        integer c = 0
        for i=1 to 25 do
            c = c*256+s[i]
            s[i] = floor(c/58)
            c = mod(c,58)
        end for
        out &= b58[c+1]
    end for
    if out[$]='1' then
        for i=length(out)-1 to 1 by -1 do
            if out[i]!='1' then
                out = out[1..i+1]
                exit
            end if
        end for
    end if
    return reverse(out)
end function

function coin_encode(string x, y)
    if length(x)!=32
    or length(y)!=32 then
        return "bad public point string"
    end if
    string s = "\x04" & x & y
    string rmd = '\0'&ripemd160(sha256(s),false)
    rmd &= sha256(sha256(rmd))[1..4]
    string res = base58(rmd)
    return res
end function

?coin_encode(x"50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352",
             x"2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6")
```

{{out}}

```txt

"16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM"

```



## PicoLisp


```PicoLisp
(load "ripemd160.l")
(load "sha256.l")

(setq *B58Alpha
   (chop "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz") )
(de hex2L (H)
   (make
      (for (L (chop H) L (cddr L))
         (link (hex (pack (car L) (cadr L)))) ) ) )
(de b58enc (Lst)
   (let
      (P 1
         Z 0
         A
         (sum
            '((X)
               (* X (swap 'P (>> -8 P))) )
            (reverse Lst) ) )
   (for L Lst
      (T (n0 L))
      (inc 'Z) )
   (pack
      (need Z "1")
      (make
         (while (gt0 A)
            (yoke
               (prog1
                  (get *B58Alpha (inc (% A 58)))
                  (setq A (/ A 58)) ) ) ) ) ) ) )
(de point2address (X Y)
   (let L (conc (cons 4) (hex2L X) (hex2L Y))
      (b58enc
         (and
            (conc (cons 0) (ripemd160 (sha256 L)))
            (conc @ (head 4 (sha256 (sha256 @)))) ) ) ) )
(println
   (point2address
      "50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352"
      "2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6" ) )
```

{{out}}

```txt
"16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM"
```



## Python


```python
#!/usr/bin/env python3

import binascii
import functools
import hashlib

digits58 = b'123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'

def b58(n):
    return b58(n//58) + digits58[n%58:n%58+1] if n else b''

def public_point_to_address(x, y):
    c = b'\x04' + binascii.unhexlify(x) + binascii.unhexlify(y)
    r = hashlib.new('ripemd160')
    r.update(hashlib.sha256(c).digest())
    c = b'\x00' + r.digest()
    d = hashlib.sha256(hashlib.sha256(c).digest()).digest()
    return b58(functools.reduce(lambda n, b: n<<8|b, c + d[:4]))

if __name__ == '__main__':
    print(public_point_to_address(
        b'50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352',
        b'2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6'))
```

{{out}}

```txt

b'6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM'

```



## Racket


Uses code from [[SHA-256#Racket]] (which is isolated in a submodule).


```racket
#lang racket/base
(module sha256 racket/base
  ;; define a quick SH256 FFI interface, similar to the Racket's default
  ;; SHA1 interface (from [[SHA-256#Racket]])
  (provide sha256)
  (require ffi/unsafe ffi/unsafe/define openssl/libcrypto)
  (define-ffi-definer defcrypto libcrypto)
  (defcrypto SHA256_Init   (_fun _pointer -> _int))
  (defcrypto SHA256_Update (_fun _pointer _pointer _long -> _int))
  (defcrypto SHA256_Final  (_fun _pointer _pointer -> _int))
  (define (sha256 bytes)
    (define ctx (malloc 128))
    (define result (make-bytes 32))
    (SHA256_Init ctx)
    (SHA256_Update ctx bytes (bytes-length bytes))
    (SHA256_Final result ctx)
    ;; (bytes->hex-string result) -- not needed, we want bytes
    result))

(require
  ;; On windows I needed to "raco planet install soegaard digetst.plt 1 2"
  (only-in (planet soegaard/digest:1:2/digest) ripemd160-bytes)
  (submod "." sha256))

;; Quick utility
(define << arithmetic-shift) ; a bit shorter

;; From: https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses
;; Using Bitcoin's stage numbers:

;; 1 - Take the corresponding public key generated with it
;; (65 bytes, 1 byte 0x04, 32 bytes corresponding to X coordinate,
;;  32 bytes corresponding to Y coordinate)
(define (stage-1 X Y)
  (define (integer->bytes! i B)
    (define l (bytes-length B))
    (for ((b (in-range 0 l))) (bytes-set! B (- l b 1) (bitwise-bit-field i (* b 8) (* (+ b 1) 8)))) B)
  (integer->bytes! (+ (<< 4 (* 32 8 2)) (<< X (* 32 8)) Y) (make-bytes 65)))

;; 2 - Perform SHA-256 hashing on the public key
(define stage-2 sha256)

;; 3 - Perform RIPEMD-160 hashing on the result of SHA-256
(define stage-3 ripemd160-bytes)

;; 4 - Add version byte in front of RIPEMD-160 hash (0x00 for Main Network)
(define (stage-4 s3)
  (bytes-append #"\0" s3))

;; 5 - Perform SHA-256 hash on the extended RIPEMD-160 result
;; 6 - Perform SHA-256 hash on the result of the previous SHA-256 hash
(define (stage-5+6 s4)
  (values s4 (sha256 (sha256 s4))))

;; 7 - Take the first 4 bytes of the second SHA-256 hash. This is the address checksum
(define (stage-7 s4 s6)
  (values s4 (subbytes s6 0 4)))

;; 8 - Add the 4 checksum bytes from stage 7 at the end of extended RIPEMD-160 hash from stage 4.
;;     This is the 25-byte binary Bitcoin Address.
(define (stage-8 s4 s7)
  (bytes-append s4 s7))

;; 9 - Convert the result from a byte string into a base58 string using Base58Check encoding.
;;     This is the most commonly used Bitcoin Address format
(define stage-9 (base58-encode 33))

(define ((base58-encode l) B)
  (define b58 #"123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")
  (define rv (make-bytes l (char->integer #\1))) ; already padded out with 1's
  (define b-int (for/fold ((i 0)) ((b (in-bytes B))) (+ (<< i 8) b)))
  (let loop ((b b-int) (l l))
    (if (zero? b) rv
        (let-values (((q r) (quotient/remainder b 58)))
          (define l- (- l 1))
          (bytes-set! rv l- (bytes-ref b58 r))
          (loop q l-)))))

;; Takes two (rather large) ints for X and Y, returns base-58 PAP.
(define public-address-point
  (compose stage-9 stage-8 stage-7 stage-5+6 stage-4 stage-3 stage-2 stage-1))

(public-address-point
 #x50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352
 #x2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  (require tests/eli-tester (only-in openssl/sha1 bytes->hex-string))
  (define bytes->HEX-STRING (compose string-upcase bytes->hex-string))
  (test
   ((base58-encode 33)
    (bytes-append #"\x00\x01\x09\x66\x77\x60\x06\x95\x3D\x55\x67\x43"
                  #"\x9E\x5E\x39\xF8\x6A\x0D\x27\x3B\xEE\xD6\x19\x67\xF6"))
   =>
   #"16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM")

  (define-values (test-X test-Y)
    (values #x50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352
            #x2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6))
  (define s1 (stage-1 test-X test-Y))
  (define s2 (stage-2 s1))
  (define s3 (stage-3 s2))
  (define s4 (stage-4 s3))
  (define-values (s4_1 s6) (stage-5+6 s4))
  (define-values (s4_2 s7) (stage-7 s4 s6))
  (define s8 (stage-8 s4 s7))
  (define s9 (stage-9 s8))

  (test
   (bytes->HEX-STRING s1)
   => (string-append "0450863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B23522CD470243453"
                     "A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6")
   (bytes->HEX-STRING s2) => "600FFE422B4E00731A59557A5CCA46CC183944191006324A447BDB2D98D4B408"
   (bytes->HEX-STRING s3) => "010966776006953D5567439E5E39F86A0D273BEE"
   (bytes->HEX-STRING s4) => "00010966776006953D5567439E5E39F86A0D273BEE"
   (bytes->HEX-STRING s6) => "D61967F63C7DD183914A4AE452C9F6AD5D462CE3D277798075B107615C1A8A30"
   (bytes->HEX-STRING s7) => "D61967F6"
   (bytes->HEX-STRING s8) => "00010966776006953D5567439E5E39F86A0D273BEED61967F6"
   s9 => #"16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM"))
```


{{out}}

```txt
#"16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM"
1 test passed
8 tests passed
```



## Ruby


```ruby

#  Translate public point to Bitcoin address
#
#  Nigel_Galloway
#  October 12th., 2014
require 'digest/sha2'
def convert g
  i,e = '',[]
  (0...g.length/2).each{|n| e[n] = g[n+=n]+g[n+1]; i+='H2'}
  e.pack(i)
end
X = '50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352'
Y = '2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6'
n = '00'+Digest::RMD160.hexdigest(Digest::SHA256.digest(convert('04'+X+Y)))
n+= Digest::SHA256.hexdigest(Digest::SHA256.digest(convert(n)))[0,8]
G = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
n,res = n.hex,''
while n > 0 do
  n,ng = n.divmod(58)
  res << G[ng]
end
puts res.reverse

```

{{out}}

```txt

6UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM

```



## Seed7

The Seed7 library [http://seed7.sourceforge.net/libraries/msgdigest.htm msgdigest.s7i] defines
the functions [http://seed7.sourceforge.net/libraries/msgdigest.htm#ripemd160(in_var_string) ripemd160] and
[http://seed7.sourceforge.net/libraries/msgdigest.htm#sha256(in_var_string) sha256].
The Seed7 library [http://seed7.sourceforge.net/libraries/encoding.htm encoding.s7i] defines
the function [http://seed7.sourceforge.net/libraries/encoding.htm#toBase58(in_string) toBase58],
which encodes a string with the Base58 encoding used by Bitcoin. No external library is needed.


```seed7
$ include "seed7_05.s7i";
  include "bytedata.s7i";
  include "msgdigest.s7i";
  include "encoding.s7i";

const func string: publicPointToAddress (in string: x, in string: y) is func
  result
    var string: address is "";
  begin
    address := "\4;" & hex2Bytes(x) & hex2Bytes(y);
    address := "\0;" & ripemd160(sha256(address));
    address &:= sha256(sha256(address))[.. 4];
    address := toBase58(address);
  end func;

const proc: main is func
  begin
    writeln(publicPointToAddress("50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352",
                                 "2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6"));
  end func;
```


{{out}}

```txt

16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM

```



## Tcl

{{tcllib|ripemd160}}
{{tcllib|sha256}}

```tcl
package require ripemd160
package require sha256

# Exactly the algorithm from https://en.bitcoin.it/wiki/Base58Check_encoding
proc base58encode data {
    set digits "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    for {set zeroes 0} {[string index $data 0] eq "\x00"} {incr zeroes} {
	set data [string range $data 1 end]
    }
    binary scan $data "H*" hex
    scan $hex "%llx" num
    for {set out ""} {$num > 0} {set num [expr {$num / 58}]} {
	append out [string index $digits [expr {$num % 58}]]
    }
    append out [string repeat [string index $digits 0] $zeroes]
    return [string reverse $out]
}

# Encode a Bitcoin address
proc bitcoin_mkaddr {A B} {
    set A [expr {$A & ((1<<256)-1)}]
    set B [expr {$B & ((1<<256)-1)}]
    set bin [binary format "cH*" 4 [format "%064llx%064llx" $A $B]]
    set md [ripemd::ripemd160 [sha2::sha256 -bin $bin]]
    set addr [binary format "ca*" 0 $md]
    set hash [sha2::sha256 -bin [sha2::sha256 -bin $addr]]
    append addr [binary format "a4" [string range $hash 0 3]]
    return [base58encode $addr]
}
```

Demonstrating

```tcl
puts [bitcoin_mkaddr \
	0x50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352 \
	0x2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6]
```

{{out}}

```txt

16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM

```



## zkl

Uses shared library zklMsgHash.

```zkl
var [const] MsgHash=Import.lib("zklMsgHash"); // SHA-256, etc

const symbols = "123456789"  // 58 characters: no cap i,o; ell, zero
		"ABCDEFGHJKLMNPQRSTUVWXYZ"
		"abcdefghijkmnopqrstuvwxyz";

fcn base58Encode(bytes){ //Data-->String
   bytes=bytes.copy(); sink:=Sink(String);
   do(33){
      bytes.len().reduce('wrap(n,idx){
         n=n*256 + bytes[idx];
         bytes[idx]=(n/58);
         n%58;
      },0) : symbols[_] : sink.write(_)
   }
   sink.close().reverse();
}

const COIN_VER=0;

fcn coinEncode(x,y){ // throws if x or y not hex or (x+y) not even length
   bytes:=(x+y).pump(Data,Void.Read,fcn(a,b){ (a+b).toInt(16) }).insert(0,4);
   (MsgHash.SHA256(bytes,1,bytes) : MsgHash.RIPEMD160(_,1,bytes))
	 .insert(0,COIN_VER);  // we are using bytes for in and out
   d,chkSum := Data(), MsgHash.SHA256(bytes,1,d) : MsgHash.SHA256(_,1,d);
   base58Encode(bytes.append(chkSum.del(4,*))); // first 4 bytes of hashed hash
}
```


```zkl
e:=coinEncode(
   "50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352",
   "2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6");
(e=="16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM").println();
```

{{out}}

```txt
True
```



{{omit from|Brlcad}}
{{omit from|GUISS}}
{{omit from|Lilypond}}
{{omit from|Openscad}}
{{omit from|TPP}}
