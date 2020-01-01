+++
title = "MD4"
description = ""
date = 2019-05-19T21:03:46Z
aliases = []
[extra]
id = 11600
[taxonomies]
categories = []
tags = []
+++

{{task}} [[Category:Checksums]]

Find the MD4 message digest of a string of [[octet]]s.
Use the ASCII encoded string “<tt>Rosetta Code</tt>” (without quotes).
You may either call an MD4 library, or implement MD4 in your language.

'''MD4''' is an obsolete hash function that computes a 128-bit message digest that sometimes appears in obsolete protocols.

RFC 1320 specifies the MD4 algorithm. RFC 6150 declares that MD4 is obsolete.


## AutoHotkey

Source: [https://github.com/jNizM/AutoHotkey_Scripts/tree/master/Functions/Checksums MD4 @github] by jNizM

```AutoHotkey
str := "Rosetta Code"
MsgBox, % "String:`n" (str) "`n`nMD4:`n" MD4(str)



; MD4
### =========================================================================

MD4(string, encoding = "utf-8")
{
    return CalcStringHash(string, 0x8002, encoding)
}

; CalcAddrHash
### ================================================================

CalcAddrHash(addr, length, algid, byref hash = 0, byref hashlength = 0)
{
    static h := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "A", "B", "C", "D", "E", "F"]
    static b := h.minIndex()
    o := ""
    if (DllCall("advapi32\CryptAcquireContext", "Ptr*", hProv, "Ptr", 0, "Ptr", 0, "UInt", 24, "UInt", 0xF0000000))
    {
        if (DllCall("advapi32\CryptCreateHash", "Ptr", hProv, "UInt", algid, "UInt", 0, "UInt", 0, "Ptr*", hHash))
        {
            if (DllCall("advapi32\CryptHashData", "Ptr", hHash, "Ptr", addr, "UInt", length, "UInt", 0))
            {
                if (DllCall("advapi32\CryptGetHashParam", "Ptr", hHash, "UInt", 2, "Ptr", 0, "UInt*", hashlength, "UInt", 0))
                {
                    VarSetCapacity(hash, hashlength, 0)
                    if (DllCall("advapi32\CryptGetHashParam", "Ptr", hHash, "UInt", 2, "Ptr", &hash, "UInt*", hashlength, "UInt", 0))
                    {
                        loop, % hashlength
                        {
                            v := NumGet(hash, A_Index - 1, "UChar")
                            o .= h[(v >> 4) + b] h[(v & 0xf) + b]
                        }
                    }
                }
            }
            DllCall("advapi32\CryptDestroyHash", "Ptr", hHash)
        }
        DllCall("advapi32\CryPtreleaseContext", "Ptr", hProv, "UInt", 0)
    }
    return o
}

; CalcStringHash
### ==============================================================

CalcStringHash(string, algid, encoding = "utf-8", byref hash = 0, byref hashlength = 0)
{
    chrlength := (encoding = "cp1200" || encoding = "utf-16") ? 2 : 1
    length := (StrPut(string, encoding) - 1) * chrlength
    VarSetCapacity(data, length, 0)
    StrPut(string, &data, floor(length / chrlength), encoding)
    return CalcAddrHash(&data, length, algid, hash, hashlength)
}
```

{{out}}

```txt
String:    Rosetta Code
MD4:       A52BCFC6A0D0D300CDC5DDBFBEFE478B
```


## C


```C

/*
 *
 *      Author: George Mossessian
 *
 *      The MD4 hash algorithm, as described in https://tools.ietf.org/html/rfc1320
 */


#include <stdlib.h>
#include <string.h>
#include <stdint.h>

char *MD4(char *str, int len); //this is the prototype you want to call. Everything else is internal.

typedef struct string{
        char *c;
        int len;
        char sign;
}string;

static uint32_t *MD4Digest(uint32_t *w, int len);
static void setMD4Registers(uint32_t AA, uint32_t BB, uint32_t CC, uint32_t DD);
static uint32_t changeEndianness(uint32_t x);
static void resetMD4Registers(void);
static string stringCat(string first, string second);
static string uint32ToString(uint32_t l);
static uint32_t stringToUint32(string s);

static const char *BASE16 = "0123456789abcdef=";

#define F(X,Y,Z) (((X)&(Y))|((~(X))&(Z)))
#define G(X,Y,Z) (((X)&(Y))|((X)&(Z))|((Y)&(Z)))
#define H(X,Y,Z) ((X)^(Y)^(Z))

#define LEFTROTATE(A,N) ((A)<<(N))|((A)>>(32-(N)))

#define MD4ROUND1(a,b,c,d,x,s) a += F(b,c,d) + x; a = LEFTROTATE(a, s);
#define MD4ROUND2(a,b,c,d,x,s) a += G(b,c,d) + x + (uint32_t)0x5A827999; a = LEFTROTATE(a, s);
#define MD4ROUND3(a,b,c,d,x,s) a += H(b,c,d) + x + (uint32_t)0x6ED9EBA1; a = LEFTROTATE(a, s);

static uint32_t A = 0x67452301;
static uint32_t B = 0xefcdab89;
static uint32_t C = 0x98badcfe;
static uint32_t D = 0x10325476;

string newString(char * c, int t){
	string r;
	int i;
	if(c!=NULL){
		r.len = (t<=0)?strlen(c):t;
		r.c=(char *)malloc(sizeof(char)*(r.len+1));
		for(i=0; i<r.len; i++) r.c[i]=c[i];
		r.c[r.len]='\0';
		return r;
	}
	r.len=t;
	r.c=(char *)malloc(sizeof(char)*(r.len+1));
	memset(r.c,(char)0,sizeof(char)*(t+1));
	r.sign = 1;
	return r;
}

string stringCat(string first, string second){
	string str=newString(NULL, first.len+second.len);
	int i;

	for(i=0; i<first.len; i++){
		str.c[i]=first.c[i];
	}
	for(i=first.len; i<str.len; i++){
		str.c[i]=second.c[i-first.len];
	}
	return str;
}

string base16Encode(string in){
	string out=newString(NULL, in.len*2);
	int i,j;

	j=0;
	for(i=0; i<in.len; i++){
		out.c[j++]=BASE16[((in.c[i] & 0xF0)>>4)];
		out.c[j++]=BASE16[(in.c[i] & 0x0F)];
	}
	out.c[j]='\0';
	return out;
}


string uint32ToString(uint32_t l){
	string s = newString(NULL,4);
	int i;
	for(i=0; i<4; i++){
		s.c[i] = (l >> (8*(3-i))) & 0xFF;
	}
	return s;
}

uint32_t stringToUint32(string s){
	uint32_t l;
	int i;
	l=0;
	for(i=0; i<4; i++){
		l = l|(((uint32_t)((unsigned char)s.c[i]))<<(8*(3-i)));
	}
	return l;
}

char *MD4(char *str, int len){
	string m=newString(str, len);
	string digest;
	uint32_t *w;
	uint32_t *hash;
	uint64_t mlen=m.len;
	unsigned char oneBit = 0x80;
	int i, wlen;


	m=stringCat(m, newString((char *)&oneBit,1));

	//append 0 ≤ k < 512 bits '0', such that the resulting message length in bits
	//	is congruent to −64 ≡ 448 (mod 512)4
	i=((56-m.len)%64);
	if(i<0) i+=64;
	m=stringCat(m,newString(NULL, i));

	w = malloc(sizeof(uint32_t)*(m.len/4+2));

	//append length, in bits (hence <<3), least significant word first
	for(i=0; i<m.len/4; i++){
		w[i]=stringToUint32(newString(&(m.c[4*i]), 4));
	}
	w[i++] = (mlen<<3) & 0xFFFFFFFF;
	w[i++] = (mlen>>29) & 0xFFFFFFFF;

	wlen=i;


	//change endianness, but not for the appended message length, for some reason?
	for(i=0; i<wlen-2; i++){
		w[i]=changeEndianness(w[i]);
	}

	hash = MD4Digest(w,wlen);

	digest=newString(NULL,0);
	for(i=0; i<4; i++){
		hash[i]=changeEndianness(hash[i]);
		digest=stringCat(digest,uint32ToString(hash[i]));
	}

	return base16Encode(digest).c;
}

uint32_t *MD4Digest(uint32_t *w, int len){
	//assumes message.len is a multiple of 64 bytes.
	int i,j;
	uint32_t X[16];
	uint32_t *digest = malloc(sizeof(uint32_t)*4);
	uint32_t AA, BB, CC, DD;

	for(i=0; i<len/16; i++){
		for(j=0; j<16; j++){
			X[j]=w[i*16+j];
		}

		AA=A;
		BB=B;
		CC=C;
		DD=D;

		MD4ROUND1(A,B,C,D,X[0],3);
		MD4ROUND1(D,A,B,C,X[1],7);
		MD4ROUND1(C,D,A,B,X[2],11);
		MD4ROUND1(B,C,D,A,X[3],19);
		MD4ROUND1(A,B,C,D,X[4],3);
		MD4ROUND1(D,A,B,C,X[5],7);
		MD4ROUND1(C,D,A,B,X[6],11);
		MD4ROUND1(B,C,D,A,X[7],19);
		MD4ROUND1(A,B,C,D,X[8],3);
		MD4ROUND1(D,A,B,C,X[9],7);
		MD4ROUND1(C,D,A,B,X[10],11);
		MD4ROUND1(B,C,D,A,X[11],19);
		MD4ROUND1(A,B,C,D,X[12],3);
		MD4ROUND1(D,A,B,C,X[13],7);
		MD4ROUND1(C,D,A,B,X[14],11);
		MD4ROUND1(B,C,D,A,X[15],19);

		MD4ROUND2(A,B,C,D,X[0],3);
		MD4ROUND2(D,A,B,C,X[4],5);
		MD4ROUND2(C,D,A,B,X[8],9);
		MD4ROUND2(B,C,D,A,X[12],13);
		MD4ROUND2(A,B,C,D,X[1],3);
		MD4ROUND2(D,A,B,C,X[5],5);
		MD4ROUND2(C,D,A,B,X[9],9);
		MD4ROUND2(B,C,D,A,X[13],13);
		MD4ROUND2(A,B,C,D,X[2],3);
		MD4ROUND2(D,A,B,C,X[6],5);
		MD4ROUND2(C,D,A,B,X[10],9);
		MD4ROUND2(B,C,D,A,X[14],13);
		MD4ROUND2(A,B,C,D,X[3],3);
		MD4ROUND2(D,A,B,C,X[7],5);
		MD4ROUND2(C,D,A,B,X[11],9);
		MD4ROUND2(B,C,D,A,X[15],13);

		MD4ROUND3(A,B,C,D,X[0],3);
		MD4ROUND3(D,A,B,C,X[8],9);
		MD4ROUND3(C,D,A,B,X[4],11);
		MD4ROUND3(B,C,D,A,X[12],15);
		MD4ROUND3(A,B,C,D,X[2],3);
		MD4ROUND3(D,A,B,C,X[10],9);
		MD4ROUND3(C,D,A,B,X[6],11);
		MD4ROUND3(B,C,D,A,X[14],15);
		MD4ROUND3(A,B,C,D,X[1],3);
		MD4ROUND3(D,A,B,C,X[9],9);
		MD4ROUND3(C,D,A,B,X[5],11);
		MD4ROUND3(B,C,D,A,X[13],15);
		MD4ROUND3(A,B,C,D,X[3],3);
		MD4ROUND3(D,A,B,C,X[11],9);
		MD4ROUND3(C,D,A,B,X[7],11);
		MD4ROUND3(B,C,D,A,X[15],15);

		A+=AA;
		B+=BB;
		C+=CC;
		D+=DD;
	}

	digest[0]=A;
	digest[1]=B;
	digest[2]=C;
	digest[3]=D;
	resetMD4Registers();
	return digest;
}

uint32_t changeEndianness(uint32_t x){
	return ((x & 0xFF) << 24) | ((x & 0xFF00) << 8) | ((x & 0xFF0000) >> 8) | ((x & 0xFF000000) >> 24);
}

void setMD4Registers(uint32_t AA, uint32_t BB, uint32_t CC, uint32_t DD){
	A=AA;
	B=BB;
	C=CC;
	D=DD;
}

void resetMD4Registers(void){
	setMD4Registers(0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476);
}

```

{{Out}}

```C
printf("%s\n", MD4("Rosetta Code", 12));
```


```txt
a52bcfc6a0d0d300cdc5ddbfbefe478b
```




## C#

```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
static class Md4
{
    public static string Md4Hash(this string input)
    {
        // get padded uints from bytes
        List<byte> bytes = Encoding.ASCII.GetBytes(input).ToList();
        uint bitCount = (uint)(bytes.Count) * 8;
        bytes.Add(128);
        while (bytes.Count % 64 != 56) bytes.Add(0);
        var uints = new List<uint>();
        for (int i = 0; i + 3 < bytes.Count; i += 4)
            uints.Add(bytes[i] | (uint)bytes[i + 1] << 8 | (uint)bytes[i + 2] << 16 | (uint)bytes[i + 3] << 24);
        uints.Add(bitCount);
        uints.Add(0);

        // run rounds
        uint a = 0x67452301, b = 0xefcdab89, c = 0x98badcfe, d = 0x10325476;
        Func<uint, uint, uint> rol = (x, y) => x << (int)y | x >> 32 - (int)y;
        for (int q = 0; q + 15 < uints.Count; q += 16)
        {
            var chunk = uints.GetRange(q, 16);
            uint aa = a, bb = b, cc = c, dd = d;
            Action<Func<uint, uint, uint, uint>, uint[]> round = (f, y) =>
            {
                foreach (uint i in new[] { y[0], y[1], y[2], y[3] })
                {
                    a = rol(a + f(b, c, d) + chunk[(int)(i + y[4])] + y[12], y[8]);
                    d = rol(d + f(a, b, c) + chunk[(int)(i + y[5])] + y[12], y[9]);
                    c = rol(c + f(d, a, b) + chunk[(int)(i + y[6])] + y[12], y[10]);
                    b = rol(b + f(c, d, a) + chunk[(int)(i + y[7])] + y[12], y[11]);
                }
            };
            round((x, y, z) => (x & y) | (~x & z), new uint[] { 0, 4, 8, 12, 0, 1, 2, 3, 3, 7, 11, 19, 0 });
            round((x, y, z) => (x & y) | (x & z) | (y & z), new uint[] { 0, 1, 2, 3, 0, 4, 8, 12, 3, 5, 9, 13, 0x5a827999 });
            round((x, y, z) => x ^ y ^ z, new uint[] { 0, 2, 1, 3, 0, 8, 4, 12, 3, 9, 11, 15, 0x6ed9eba1 });
            a += aa; b += bb; c += cc; d += dd;
        }

        // return hex encoded string
        byte[] outBytes = new[] { a, b, c, d }.SelectMany(BitConverter.GetBytes).ToArray();
        return BitConverter.ToString(outBytes).Replace("-", "").ToLower();
    }
    static void Main() { Console.WriteLine("Rosetta Code".Md4Hash()); }
}
```

{{out}}

```txt
a52bcfc6a0d0d300cdc5ddbfbefe478b
```



## Clojure

{{libheader|pandect}}

```clojure
(use 'pandect.core)
(md4 "Rosetta Code")
```


{{Out}}

```txt
"a52bcfc6a0d0d300cdc5ddbfbefe478b"
```



## Common Lisp

{{libheader|Ironclad}}

```lisp
(ql:quickload 'ironclad)
(defun md4 (str)
  (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence :md4
                              (ironclad:ascii-string-to-byte-array str))))

(md4 "Rosetta Code")
```


{{Out}}

```txt
"a52bcfc6a0d0d300cdc5ddbfbefe478b"
```



## D

A short but not efficient implementation.
{{trans|Ruby}}

```d
import std.stdio, std.string, std.range;

ubyte[16] md4(const(ubyte)[] inData) pure nothrow {
    enum f = (uint x, uint y, uint z) => (x & y) | (~x & z);
    enum g = (uint x, uint y, uint z) => (x & y) | (x & z) | (y & z);
    enum h = (uint x, uint y, uint z) => x ^ y ^ z;
    enum r = (uint v, uint s) => (v << s) | (v >> (32 - s));

    immutable bitLen = ulong(inData.length) << 3;
    inData ~= 0x80;
    while (inData.length % 64 != 56)
        inData ~= 0;
    const data = cast(uint[])inData ~ [uint(bitLen & uint.max), uint(bitLen >> 32)];

    uint a = 0x67452301, b = 0xefcdab89, c = 0x98badcfe, d = 0x10325476;

    foreach (const x; data.chunks(16)) {
        immutable a2 = a, b2 = b, c2 = c, d2 = d;
        foreach (immutable i; [0, 4, 8, 12]) {
            a = r(a + f(b, c, d) + x[i+0],  3);
            d = r(d + f(a, b, c) + x[i+1],  7);
            c = r(c + f(d, a, b) + x[i+2], 11);
            b = r(b + f(c, d, a) + x[i+3], 19);
        }
        foreach (immutable i; [0, 1, 2, 3]) {
            a = r(a + g(b, c, d) + x[i+0] + 0x5a827999,  3);
            d = r(d + g(a, b, c) + x[i+4] + 0x5a827999,  5);
            c = r(c + g(d, a, b) + x[i+8] + 0x5a827999,  9);
            b = r(b + g(c, d, a) + x[i+12] + 0x5a827999, 13);
        }
        foreach (immutable i; [0, 2, 1, 3]) {
            a = r(a + h(b, c, d) + x[i+0] + 0x6ed9eba1,  3);
            d = r(d + h(a, b, c) + x[i+8] + 0x6ed9eba1,  9);
            c = r(c + h(d, a, b) + x[i+4] + 0x6ed9eba1, 11);
            b = r(b + h(c, d, a) + x[i+12] + 0x6ed9eba1, 15);
        }
        a += a2, b += b2, c += c2, d += d2;
    }

    //return cast(ubyte[16])[a, b, c, d];
    immutable uint[4] result = [a, b, c, d];
    return cast(ubyte[16])result;
}

void main() {
    writefln("%(%02x%)", "Rosetta Code".representation.md4);
}
```

{{out}}

```txt
a52bcfc6a0d0d300cdc5ddbfbefe478b
```



## Emacs Lisp


<code>md4.el</code> by Taro Kawagishi, originally from [http://git.chise.org/elisp/flim/ FLIM] and included in recent Emacs, is an Elisp implementation of the MD4 algorithm.  Its <code>md4</code> function returns the checksum as 16 binary bytes.  <code>encode-hex-string</code> from <code>hex-util.el</code> can convert that to a hex string if desired.


```Lisp
(require 'md4)
(let* ((s  "Rosetta Code")
       (m  (md4 s (length s)))) ;; m = 16 binary bytes
  (require 'hex-util)
  (encode-hex-string m))
=>
"a52bcfc6a0d0d300cdc5ddbfbefe478b"
```



## Erlang


```erlang

-module(md4).
-export([md4/0]).

md4() ->
    <<MD4:128>> = crypto:md4("Rosetta Code"),
    io:fwrite("Rosetta Code => ~.16B~n",[MD4]).

```


```txt

Rosetta Code => A52BCFC6A0D0D300CDC5DDBFBEFE478B

```


## FreeBASIC


```freebasic
' version 19-10-2016
' translation of the (pseudo) code in RFC 1320
' compile with: fbc -s console

Function MD4(test_str As String) As String

  Dim As String message = test_str ' string are passed as ByRef

  ' some macro's
  #Macro F(X, Y, Z)
    (((X) And (Y)) Or ((Not(X)) And (Z)))
  #EndMacro

  #Macro G(X, Y, Z)
    (((X) And (Y)) Or (((X) And (Z)) Or ((Y) And (Z))))
  #EndMacro

  #Macro H(X, Y, Z)
    ((X) Xor (Y) Xor (Z))
  #EndMacro

  ' a little piece of inline asm to do a rotate left on a 32bit variable
  #Macro ROtate_Left(x, n) ' rotate left
    Asm
      rol dword Ptr [x], n
    End Asm
  #EndMacro

  ' #Macro ROtate_left(x, n)
    ' x = x Shl n + x Shr (32 - n)
  ' #EndMacro

  Dim As Long i
  Dim As String answer, s1

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

  ' unsigned 32bit integers only
  Dim As UInteger<32> AA, A = &H67452301
  Dim As UInteger<32> BB, B = &Hefcdab89
  Dim As UInteger<32> CC, C = &H98badcfe
  Dim As UInteger<32> DD, D = &H10325476

  For i = 0 To (l1 -1) \ 64 ' split into 64 byte block

    AA = A : BB = B : CC = C : DD = D

    ' x point to 64 byte block inside the string message
    Dim As UInteger<32> Ptr x = Cast(UInteger<32> Ptr, @message[i*64])

    ' round 1
    A = A + F(B, C, D) + x[ 0] : ROtate_Left(A,  3)
    D = D + F(A, B, C) + x[ 1] : ROtate_Left(D,  7)
    C = C + F(D, A, B) + x[ 2] : ROtate_Left(C, 11)
    B = B + F(C, D, A) + x[ 3] : ROtate_Left(B, 19)
    A = A + F(B, C, D) + x[ 4] : ROtate_Left(A,  3)
    D = D + F(A, B, C) + x[ 5] : ROtate_Left(D,  7)
    C = C + F(D, A, B) + x[ 6] : ROtate_Left(C, 11)
    B = B + F(C, D, A) + x[ 7] : ROtate_Left(B, 19)
    A = A + F(B, C, D) + x[ 8] : ROtate_Left(A,  3)
    D = D + F(A, B, C) + x[ 9] : ROtate_Left(D,  7)
    C = C + F(D, A, B) + x[10] : ROtate_Left(C, 11)
    B = B + F(C, D, A) + x[11] : ROtate_Left(B, 19)
    A = A + F(B, C, D) + x[12] : ROtate_Left(A,  3)
    D = D + F(A, B, C) + x[13] : ROtate_Left(D,  7)
    C = C + F(D, A, B) + x[14] : ROtate_Left(C, 11)
    B = B + F(C, D, A) + x[15] : ROtate_Left(B, 19)

    ' round 2
    A = A + G(B, C, D) + x[ 0] + &H5A827999 : ROtate_Left(A,  3)
    D = D + G(A, B, C) + x[ 4] + &H5A827999 : ROtate_Left(D,  5)
    C = C + G(D, A, B) + x[ 8] + &H5A827999 : ROtate_Left(C,  9)
    B = B + G(C, D, A) + x[12] + &H5A827999 : ROtate_Left(B, 13)
    A = A + G(B, C, D) + x[ 1] + &H5A827999 : ROtate_Left(A,  3)
    D = D + G(A, B, C) + x[ 5] + &H5A827999 : ROtate_Left(D,  5)
    C = C + G(D, A, B) + x[ 9] + &H5A827999 : ROtate_Left(C,  9)
    B = B + G(C, D, A) + x[13] + &H5A827999 : ROtate_Left(B, 13)
    A = A + G(B, C, D) + x[ 2] + &H5A827999 : ROtate_Left(A,  3)
    D = D + G(A, B, C) + x[ 6] + &H5A827999 : ROtate_Left(D,  5)
    C = C + G(D, A, B) + x[10] + &H5A827999 : ROtate_Left(C,  9)
    B = B + G(C, D, A) + x[14] + &H5A827999 : ROtate_Left(B, 13)
    A = A + G(B, C, D) + x[ 3] + &H5A827999 : ROtate_Left(A,  3)
    D = D + G(A, B, C) + x[ 7] + &H5A827999 : ROtate_Left(D,  5)
    C = C + G(D, A, B) + x[11] + &H5A827999 : ROtate_Left(C,  9)
    B = B + G(C, D, A) + x[15] + &H5A827999 : ROtate_Left(B, 13)

    ' round 3
    A = A + H(B, C, D) + x[ 0] + &H6ED9EBA1 : ROtate_Left(A,  3)
    D = D + H(A, B, C) + x[ 8] + &H6ED9EBA1 : ROtate_Left(D,  9)
    C = C + H(D, A, B) + x[ 4] + &H6ED9EBA1 : ROtate_Left(C, 11)
    B = B + H(C, D, A) + x[12] + &H6ED9EBA1 : ROtate_Left(B, 15)
    A = A + H(B, C, D) + x[ 2] + &H6ED9EBA1 : ROtate_Left(A,  3)
    D = D + H(A, B, C) + x[10] + &H6ED9EBA1 : ROtate_Left(D,  9)
    C = C + H(D, A, B) + x[ 6] + &H6ED9EBA1 : ROtate_Left(C, 11)
    B = B + H(C, D, A) + x[14] + &H6ED9EBA1 : ROtate_Left(B, 15)
    A = A + H(B, C, D) + x[ 1] + &H6ED9EBA1 : ROtate_Left(A,  3)
    D = D + H(A, B, C) + x[ 9] + &H6ED9EBA1 : ROtate_Left(D,  9)
    C = C + H(D, A, B) + x[ 5] + &H6ED9EBA1 : ROtate_Left(C, 11)
    B = B + H(C, D, A) + x[13] + &H6ED9EBA1 : ROtate_Left(B, 15)
    A = A + H(B, C, D) + x[ 3] + &H6ED9EBA1 : ROtate_Left(A,  3)
    D = D + H(A, B, C) + x[11] + &H6ED9EBA1 : ROtate_Left(D,  9)
    C = C + H(D, A, B) + x[ 7] + &H6ED9EBA1 : ROtate_Left(C, 11)
    B = B + H(C, D, A) + x[15] + &H6ED9EBA1 : ROtate_Left(B, 15)

    A += AA : B += BB : C += CC : D += DD

  Next

  ' convert A, B, C and D in hex, then add low order first
  s1 = Hex(A, 8)
  For i = 7 To 1 Step -2 : answer +=Mid(s1, i, 2) : Next
  s1 = Hex(B, 8)
  For i = 7 To 1 Step -2 : answer +=Mid(s1, i, 2) : Next
  s1 = Hex(C, 8)
  For i = 7 To 1 Step -2 : answer +=Mid(s1, i, 2) : Next
  s1 = Hex(D, 8)
  For i = 7 To 1 Step -2 : answer +=Mid(s1, i, 2) : Next

Return LCase(answer)

End Function

' ------=< MAIN >=------

Dim As String test = "Rosetta Code"
Print
Print  test; " => "; MD4(test)


' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
Rosetta Code => a52bcfc6a0d0d300cdc5ddbfbefe478b
```



## Haskell

{{libheader|Cryptonite}}

```haskell
#!/usr/bin/env runhaskell

import Data.ByteString.Char8 (pack)
import System.Environment (getArgs)
import Crypto.Hash

main :: IO ()
main = print . md4 . pack . unwords =<< getArgs
         where md4 x = hash x :: Digest MD4
```

{{out}}

```txt

$ ./md4.hs Rosetta Code
a52bcfc6a0d0d300cdc5ddbfbefe478b

```



## Go

{{libheader|Go sub-repositories}}

```go
package main

import (
    "golang.org/x/crypto/md4"
    "fmt"
)

func main() {
    h := md4.New()
    h.Write([]byte("Rosetta Code"))
    fmt.Printf("%x\n", h.Sum(nil))
}
```

{{out}}

```txt

a52bcfc6a0d0d300cdc5ddbfbefe478b

```



## J


```j
   require 'ide/qt'
   gethash_jqtide_ 'MD4';'Rosetta Code'
a52bcfc6a0d0d300cdc5ddbfbefe478b
```



## Java

{{libheader|BouncyCastle}}

```java
import org.bouncycastle.crypto.digests.MD4Digest;
import org.bouncycastle.util.encoders.Hex;

public class RosettaMD4
{
    public static void main (String[] argv) throws Exception
    {
        byte[] r = "Rosetta Code".getBytes("US-ASCII");
        MD4Digest d = new MD4Digest();
        d.update (r, 0, r.length);
        byte[] o = new byte[d.getDigestSize()];
        d.doFinal (o, 0);
        Hex.encode (o, System.out);
        System.out.println();
    }
}
```

{{out}}

```txt

a52bcfc6a0d0d300cdc5ddbfbefe478b

```



## JavaScript


```javascript
const md4func = () => {

  const hexcase = 0; /* hex output format. 0 - lowercase; 1 - uppercase    */
  const b64pad = ""; /* base-64 pad character. "=" for strict RFC compliance  */
  const chrsz = 8; /* bits per input character. 8 - ASCII; 16 - Unicode   */

  const tab = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
  const hex_tab = hexcase ? "0123456789ABCDEF" : "0123456789abcdef";

  /**
   * Add integers, wrapping at 2^32. This uses 16-bit operations internally
   * to work around bugs in some JS interpreters.
   */
  const safe_add = (x, y) => {
    const lsw = (x & 0xFFFF) + (y & 0xFFFF);
    const msw = (x >> 16) + (y >> 16) + (lsw >> 16);
    return (msw << 16) | (lsw & 0xFFFF);
  };

  /**
   * Bitwise rotate a 32-bit number to the left.
   */
  const rol = (num, cnt) => (num << cnt) | (num >>> (32 - cnt));

  /**
   * Convert a string to an array of little-endian words
   * If chrsz is ASCII, characters >255 have their hi-byte silently ignored.
   */
  const str2binl = str => {
    const bin = Array();
    const mask = (1 << chrsz) - 1;
    for (let i = 0; i < str.length * chrsz; i += chrsz)
      bin[i >> 5] |= (str.charCodeAt(i / chrsz) & mask) << (i % 32);
    return bin;
  };

  /**
   * Convert an array of little-endian words to a string
   */
  const binl2str = bin => {
    let str = "";
    const mask = (1 << chrsz) - 1;
    for (let i = 0; i < bin.length * 32; i += chrsz)
      str += String.fromCharCode((bin[i >> 5] >>> (i % 32)) & mask);
    return str;
  };

  /**
   * Convert an array of little-endian words to a hex string.
   */
  const binl2hex = binarray => {
    let str = "";
    for (let i = 0; i < binarray.length * 4; i++) {
      str += hex_tab.charAt((binarray[i >> 2] >> ((i % 4) * 8 + 4)) & 0xF) +
          hex_tab.charAt((binarray[i >> 2] >> ((i % 4) * 8)) & 0xF);
    }
    return str;
  };

  /**
   * Convert an array of little-endian words to a base-64 string
   */
  const binl2b64 = binarray => {
    let str = "";
    for (let i = 0; i < binarray.length * 4; i += 3) {
      const triplet = (((binarray[i >> 2] >> 8 * (i % 4)) & 0xFF) << 16)
          | (((binarray[i + 1 >> 2] >> 8 * ((i + 1) % 4)) & 0xFF) << 8)
          | ((binarray[i + 2 >> 2] >> 8 * ((i + 2) % 4)) & 0xFF);
      for (let j = 0; j < 4; j++) {
        if (i * 8 + j * 6 > binarray.length * 32) str += b64pad;
        else str += tab.charAt((triplet >> 6 * (3 - j)) & 0x3F);
      }
    }
    return str;
  };


  /**
   * Calculate the MD4 of an array of little-endian words, and a bit length
   */
  const core_md4 = (x, len) => {

    x[len >> 5] |= 0x80 << (len % 32);
    x[(((len + 64) >>> 9) << 4) + 14] = len;
    let a = 1732584193;
    let b = -271733879;
    let c = -1732584194;
    let d = 271733878;

    for (let i = 0; i < x.length; i += 16) {

      const olda = a;
      const oldb = b;
      const oldc = c;
      const oldd = d;

      a = md4_ff(a, b, c, d, x[i], 3);
      d = md4_ff(d, a, b, c, x[i + 1], 7);
      c = md4_ff(c, d, a, b, x[i + 2], 11);
      b = md4_ff(b, c, d, a, x[i + 3], 19);
      a = md4_ff(a, b, c, d, x[i + 4], 3);
      d = md4_ff(d, a, b, c, x[i + 5], 7);
      c = md4_ff(c, d, a, b, x[i + 6], 11);
      b = md4_ff(b, c, d, a, x[i + 7], 19);
      a = md4_ff(a, b, c, d, x[i + 8], 3);
      d = md4_ff(d, a, b, c, x[i + 9], 7);
      c = md4_ff(c, d, a, b, x[i + 10], 11);
      b = md4_ff(b, c, d, a, x[i + 11], 19);
      a = md4_ff(a, b, c, d, x[i + 12], 3);
      d = md4_ff(d, a, b, c, x[i + 13], 7);
      c = md4_ff(c, d, a, b, x[i + 14], 11);
      b = md4_ff(b, c, d, a, x[i + 15], 19);

      a = md4_gg(a, b, c, d, x[i], 3);
      d = md4_gg(d, a, b, c, x[i + 4], 5);
      c = md4_gg(c, d, a, b, x[i + 8], 9);
      b = md4_gg(b, c, d, a, x[i + 12], 13);
      a = md4_gg(a, b, c, d, x[i + 1], 3);
      d = md4_gg(d, a, b, c, x[i + 5], 5);
      c = md4_gg(c, d, a, b, x[i + 9], 9);
      b = md4_gg(b, c, d, a, x[i + 13], 13);
      a = md4_gg(a, b, c, d, x[i + 2], 3);
      d = md4_gg(d, a, b, c, x[i + 6], 5);
      c = md4_gg(c, d, a, b, x[i + 10], 9);
      b = md4_gg(b, c, d, a, x[i + 14], 13);
      a = md4_gg(a, b, c, d, x[i + 3], 3);
      d = md4_gg(d, a, b, c, x[i + 7], 5);
      c = md4_gg(c, d, a, b, x[i + 11], 9);
      b = md4_gg(b, c, d, a, x[i + 15], 13);

      a = md4_hh(a, b, c, d, x[i], 3);
      d = md4_hh(d, a, b, c, x[i + 8], 9);
      c = md4_hh(c, d, a, b, x[i + 4], 11);
      b = md4_hh(b, c, d, a, x[i + 12], 15);
      a = md4_hh(a, b, c, d, x[i + 2], 3);
      d = md4_hh(d, a, b, c, x[i + 10], 9);
      c = md4_hh(c, d, a, b, x[i + 6], 11);
      b = md4_hh(b, c, d, a, x[i + 14], 15);
      a = md4_hh(a, b, c, d, x[i + 1], 3);
      d = md4_hh(d, a, b, c, x[i + 9], 9);
      c = md4_hh(c, d, a, b, x[i + 5], 11);
      b = md4_hh(b, c, d, a, x[i + 13], 15);
      a = md4_hh(a, b, c, d, x[i + 3], 3);
      d = md4_hh(d, a, b, c, x[i + 11], 9);
      c = md4_hh(c, d, a, b, x[i + 7], 11);
      b = md4_hh(b, c, d, a, x[i + 15], 15);

      a = safe_add(a, olda);
      b = safe_add(b, oldb);
      c = safe_add(c, oldc);
      d = safe_add(d, oldd);
    }

    return Array(a, b, c, d);

  };

  /**
   * These functions implement the basic operation for each round of the
   * algorithm.
   */
  const md4_cmn = (q, a, b, x, s, t) => safe_add(
      rol(safe_add(safe_add(a, q), safe_add(x, t)), s), b);

  const md4_ff = (a, b, c, d, x, s) => md4_cmn(
      (b & c) | ((~b) & d), a, 0, x, s, 0);

  const md4_gg = (a, b, c, d, x, s) => md4_cmn(
      (b & c) | (b & d) | (c & d), a, 0, x, s, 1518500249);

  const md4_hh = (a, b, c, d, x, s) => md4_cmn(
      b ^ c ^ d, a, 0, x, s, 1859775393);

  /**
   * Calculate the HMAC-MD4, of a key and some data
   */
  const core_hmac_md4 = (key, data) => {

    let bkey = str2binl(key);
    if (bkey.length > 16) {
      bkey = core_md4(bkey, key.length * chrsz)
    }

    const ipad = Array(16);
    const opad = Array(16);

    for (let i = 0; i < 16; i++) {
      ipad[i] = bkey[i] ^ 0x36363636;
      opad[i] = bkey[i] ^ 0x5C5C5C5C;
    }
    const hash = core_md4(
        ipad.concat(str2binl(data)), 512 + data.length * chrsz);

    return core_md4(opad.concat(hash), 512 + 128);
  };

  /**
   * These are the functions you'll usually want to call
   */
  return {
    hex_md4: s => binl2hex(core_md4(str2binl(s), s.length * chrsz)),
    b64_md4: s => binl2b64(core_md4(str2binl(s), s.length * chrsz)),
    str_md4: s => binl2str(core_md4(str2binl(s), s.length * chrsz)),
    hex_hmac_md4: (key, data) => binl2hex(core_hmac_md4(key, data)),
    b64_hmac_md4: (key, data) => binl2b64(core_hmac_md4(key, data)),
    str_hmac_md4: (key, data) => binl2str(core_hmac_md4(key, data)),
  };

};

const md4 = md4func();
console.log(md4.hex_md4('Rosetta Code'));
```

{{out}}

```txt
a52bcfc6a0d0d300cdc5ddbfbefe478b
```



## Julia

[https://github.com/staticfloat/Nettle.jl Nettle.jl] provides a variety of cryptographic functions including the <tt>MD4</tt> hash.

```Julia

using Nettle

msg = "Rosetta Code"

h = HashState(MD4)
update!(h, msg)
h = hexdigest!(h)

println("\"", msg, "\" => ", h)

```


{{out}}

```txt

"Rosetta Code" => a52bcfc6a0d0d300cdc5ddbfbefe478b

```



## Kotlin

This is a translation of the Java code [http://www.java2s.com/Code/Java/Security/ImplementstheMD4messagedigestalgorithminJava.htm here]. In the interests of conciseness, I have removed the comments from the Kotlin version:

```scala
// version 1.0.6

import java.security.MessageDigest

class MD4() : MessageDigest("MD4"), Cloneable {
    private val blockLength = 64
    private var context = IntArray(4)
    private var count = 0L
    private var buffer = ByteArray(blockLength)
    private var x = IntArray(16)

    init {
        engineReset()
    }

    private constructor(md: MD4): this() {
        context = md.context.clone()
        buffer = md.buffer.clone()
        count = md.count
    }

    override fun clone(): Any = MD4(this)

    override fun engineReset() {
        context[0] = 0x67452301
        context[1] = 0xefcdab89.toInt()
        context[2] = 0x98badcfe.toInt()
        context[3] = 0x10325476
        count = 0L
        for (i in 0 until blockLength) buffer[i] = 0
    }

    override fun engineUpdate(b: Byte) {
        val i = (count % blockLength).toInt()
        count++
        buffer[i] = b
        if (i == blockLength - 1) transform(buffer, 0)
    }

    override fun engineUpdate(input: ByteArray, offset: Int, len: Int) {
        if (offset < 0 || len < 0 || offset.toLong() + len > input.size.toLong())
            throw ArrayIndexOutOfBoundsException()
        var bufferNdx = (count % blockLength).toInt()
        count += len
        val partLen = blockLength - bufferNdx
        var i = 0
        if (len >= partLen) {
            System.arraycopy(input, offset, buffer, bufferNdx, partLen)
            transform(buffer, 0)
            i = partLen
            while (i + blockLength - 1 < len) {
                transform(input, offset + i)
                i += blockLength
            }
            bufferNdx = 0
        }
        if (i < len) System.arraycopy(input, offset + i, buffer, bufferNdx, len - i)
    }

    override fun engineDigest(): ByteArray {
        val bufferNdx = (count % blockLength).toInt()
        val padLen = if (bufferNdx < 56) 56 - bufferNdx else 120 - bufferNdx
        val tail = ByteArray(padLen + 8)
        tail[0] = 0x80.toByte()
        for (i in 0..7) tail[padLen + i] = ((count * 8) ushr (8 * i)).toByte()
        engineUpdate(tail, 0, tail.size)
        val result = ByteArray(16)
        for (i in 0..3)
            for (j in 0..3)
                result[i * 4 + j] = (context[i] ushr (8 * j)).toByte()
        engineReset()
        return result
    }

    private fun transform (block: ByteArray, offset: Int) {
        var offset2 = offset
        for (i in 0..15)
            x[i] = ((block[offset2++].toInt() and 0xff)       ) or
                   ((block[offset2++].toInt() and 0xff) shl 8 ) or
                   ((block[offset2++].toInt() and 0xff) shl 16) or
                   ((block[offset2++].toInt() and 0xff) shl 24)

        var a = context[0]
        var b = context[1]
        var c = context[2]
        var d = context[3]

        a = ff(a, b, c, d, x[ 0],  3)
        d = ff(d, a, b, c, x[ 1],  7)
        c = ff(c, d, a, b, x[ 2], 11)
        b = ff(b, c, d, a, x[ 3], 19)
        a = ff(a, b, c, d, x[ 4],  3)
        d = ff(d, a, b, c, x[ 5],  7)
        c = ff(c, d, a, b, x[ 6], 11)
        b = ff(b, c, d, a, x[ 7], 19)
        a = ff(a, b, c, d, x[ 8],  3)
        d = ff(d, a, b, c, x[ 9],  7)
        c = ff(c, d, a, b, x[10], 11)
        b = ff(b, c, d, a, x[11], 19)
        a = ff(a, b, c, d, x[12],  3)
        d = ff(d, a, b, c, x[13],  7)
        c = ff(c, d, a, b, x[14], 11)
        b = ff(b, c, d, a, x[15], 19)

        a = gg(a, b, c, d, x[ 0],  3)
        d = gg(d, a, b, c, x[ 4],  5)
        c = gg(c, d, a, b, x[ 8],  9)
        b = gg(b, c, d, a, x[12], 13)
        a = gg(a, b, c, d, x[ 1],  3)
        d = gg(d, a, b, c, x[ 5],  5)
        c = gg(c, d, a, b, x[ 9],  9)
        b = gg(b, c, d, a, x[13], 13)
        a = gg(a, b, c, d, x[ 2],  3)
        d = gg(d, a, b, c, x[ 6],  5)
        c = gg(c, d, a, b, x[10],  9)
        b = gg(b, c, d, a, x[14], 13)
        a = gg(a, b, c, d, x[ 3],  3)
        d = gg(d, a, b, c, x[ 7],  5)
        c = gg(c, d, a, b, x[11],  9)
        b = gg(b, c, d, a, x[15], 13)

        a = hh(a, b, c, d, x[ 0],  3)
        d = hh(d, a, b, c, x[ 8],  9)
        c = hh(c, d, a, b, x[ 4], 11)
        b = hh(b, c, d, a, x[12], 15)
        a = hh(a, b, c, d, x[ 2],  3)
        d = hh(d, a, b, c, x[10],  9)
        c = hh(c, d, a, b, x[ 6], 11)
        b = hh(b, c, d, a, x[14], 15)
        a = hh(a, b, c, d, x[ 1],  3)
        d = hh(d, a, b, c, x[ 9],  9)
        c = hh(c, d, a, b, x[ 5], 11)
        b = hh(b, c, d, a, x[13], 15)
        a = hh(a, b, c, d, x[ 3],  3)
        d = hh(d, a, b, c, x[11],  9)
        c = hh(c, d, a, b, x[ 7], 11)
        b = hh(b, c, d, a, x[15], 15)

        context[0] += a
        context[1] += b
        context[2] += c
        context[3] += d
    }

    private fun ff(a: Int, b: Int, c: Int, d: Int, x: Int, s: Int): Int {
        val t = a + ((b and c) or (b.inv() and d)) + x
        return (t shl s) or (t ushr (32 - s))
    }

    private fun gg(a: Int, b: Int, c: Int, d: Int, x: Int, s: Int): Int {
        val t = a + ((b and (c or d)) or (c and d)) + x + 0x5a827999
        return (t shl s) or (t ushr (32 - s))
    }

    private fun hh(a: Int, b: Int, c: Int, d: Int, x: Int, s: Int): Int {
        val t = a + (b xor c xor d) + x + 0x6ed9eba1
        return (t shl s) or (t ushr (32 - s))
    }
}

fun main(args: Array<String>) {
    val text  = "Rosetta Code"
    val bytes = text.toByteArray(Charsets.US_ASCII)
    val md: MessageDigest = MD4()
    val digest = md.digest(bytes)
    for (byte in digest) print("%02x".format(byte))
    println()
}
```


{{out}}

```txt

a52bcfc6a0d0d300cdc5ddbfbefe478b

```



## Lasso


```Lasso
cipher_digest('Rosetta Code', -digest='MD4')->encodeHex->asString
```

{{out}}

```txt
 A52BCFC6A0D0D300CDC5DDBFBEFE478B
```



## Lua


{{works with|Lua 5.1.4}}
{{libheader|LuaCrypto}} ([http://mkottman.github.io/luacrypto/ luarocks install LuaCrypto])


```Lua
#!/usr/bin/lua

require "crypto"

print(crypto.digest("MD4", "Rosetta Code"))
```


{{out}}

```txt
a52bcfc6a0d0d300cdc5ddbfbefe478b
```



## Mathematica


```Mathemtica
Hash["Rosetta Code", "MD4", "HexString"]
```


```txt
a52bcfc6a0d0d300cdc5ddbfbefe478b
```



## Nim

{{libheader|OpenSSL}}
Compile with <code>nim -d:ssl c md4.nim</code>:

```nim
import strutils

const MD4Len = 16

proc MD4(d: cstring, n: culong, md: cstring = nil): cstring {.cdecl, dynlib: "libssl.so", importc.}

proc MD4(s: string): string =
  result = ""
  var s = MD4(s.cstring, s.len.culong)
  for i in 0 .. < MD4Len:
    result.add s[i].BiggestInt.toHex(2).toLower

echo MD4("Rosetta Code")
```



## PARI/GP


Build a MD4 plugin using Linux system library and PARI's function interface. (Linux solution)


```C>#include <pari/pari.h

#include <openssl/md4.h>

#define HEX(x)  (((x) < 10)? (x)+'0': (x)-10+'a')

/*
 * PARI/GP func: MD4 hash
 *
 * gp code: install("plug_md4", "s", "MD4", "<library path>");
 */
GEN plug_md4(char *text)
{
  char md[MD4_DIGEST_LENGTH];
  char hash[sizeof(md) * 2 + 1];
  int i;

  MD4((unsigned char*)text, strlen(text), (unsigned char*)md);

  for (i = 0; i < sizeof(md); i++) {
    hash[i+i]   = HEX((md[i] >> 4) & 0x0f);
    hash[i+i+1] = HEX(md[i] & 0x0f);
  }

  hash[sizeof(md) * 2] = 0;

  return strtoGENstr(hash);
}
```


Compile with: gcc -Wall -O2 -fPIC -shared md4.c -o libmd4.so -lcrypt -lpari

Load plugin from your home directory into PARI:

```parigp
install("plug_md4", "s", "MD4", "~/libmd4.so");

MD4("Rosetta Code")
```


Output: "a52bcfc6a0d0d300cdc5ddbfbefe478b"



## Perl

In-lining code from module [https://metacpan.org/pod/Digest::Perl::MD4 Digest::Perl::MD4], lightly edited for clarity.

```perl
sub md4(@) {
    my @input = grep { defined && length > 0 } split /(.{64})/s, join '', @_;
    push @input, '' if !@input || length($input[$#input]) >= 56;

    my @A = (0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476); # initial regs
    my @T = (0, 0x5A827999, 0x6ED9EBA1);
    my @L = qw(3 7 11 19 3 5 9 13 3 9 11 15);   # left rotate counts
    my @O = (1, 4, 4,                           # x stride for input index
             4, 1, 1,                           # y stride for input index
             0, 0, 1);                          # bitwise reverse both indexes
    my @I = map {
                    my $z   = int $_/16;
                    my $x   = $_%4;
                    my $y   = int $_%16/4;
                    ($x,$y) = (R($x),R($y)) if $O[6+$z];
                    $O[$z] * $x + $O[3+$z] * $y
                } 0..47;

    my ($a,$b,$c,$d);
    my($l,$p) = (0,0);
    foreach (@input) {
        my $r = length($_);
        $l += $r;
        $r++, $_.="\x80" if $r<64 && !$p++;
        my @W = unpack 'V16', $_ . "\0"x7;
        push @W, (0)x16 if @W < 16;
        $W[14] = $l*8 if $r < 57;              # add bit-length in low 32-bits
        ($a,$b,$c,$d) = @A;
        for (0..47) {
            my $z = int $_/16;
            $a = L($L[4*($_>>4) + $_%4],
                 M(&{(sub{$b&$c|~$b&$d},       # F
                      sub{$b&$c|$b&$d|$c&$d},  # G
                      sub{$b^$c^$d}            # H
                     )[$z]}
                   + $a + $W[$I[$_]] + $T[$z]));
            ($a,$b,$c,$d) = ($d,$a,$b,$c);
        }
        my @v = ($a, $b, $c, $d);
        $A[$_] = M($A[$_] + $v[$_]) for 0..3;
    }
    pack 'V4', @A;
}

sub L { # left-rotate
    my ($n, $x) = @_;
    $x<<$n | 2**$n - 1 & $x>>(32-$n);
}

sub M { # mod 2**32
    no integer;
    my ($x) = @_;
    my $m = 1+0xffffffff;
    $x - $m * int $x/$m;
}

sub R { # reverse two bit number
    my $n = pop;
    ($n&1)*2 + ($n&2)/2;
}

sub md4_hex(@) { # convert to hexadecimal
  unpack 'H*', &md4;
}

print "Rosetta Code => " . md4_hex( "Rosetta Code" ) . "\n";
```

{{out}}

```txt
Rosetta Code => a52bcfc6a0d0d300cdc5ddbfbefe478b
```



## Perl 6


```perl6
sub md4($str) {
    my @buf = $str.ords;
    my $buflen = @buf.elems;

    my \mask = (1 +< 32) - 1;
    my &f = -> $x, $y, $z { ($x +& $y) +| ($x +^ mask) +& $z }
    my &g = -> $x, $y, $z { ($x +& $y) +| ($x +& $z) +| ($y +& $z) }
    my &h = -> $x, $y, $z { $x +^ $y +^ $z }
    my &r = -> $v, $s { (($v +< $s) +& mask) +| (($v +& mask) +> (32 - $s)) }

    sub pack-le (@a) {
        gather for @a -> $a,$b,$c,$d { take $d +< 24 + $c +< 16 + $b +< 8 + $a }
    }

    my ($a, $b, $c, $d) = 0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476;

    my $term = False;
    my $last = False;
    my $off = 0;
    repeat until $last {
        my @block = @buf[$off..$off+63]:v; $off += 64;

        my @x;
        given +@block {
	    when 64 {
	        @x = pack-le @block;
	    }
	    when 56..63 {
	        $term = True;
	        @block.push(0x80);
	        @block.push(slip 0 xx 63 - $_);
	        @x = pack-le @block;
	    }
	    when 0..55 {
	        @block.push($term ?? 0 !! 0x80);
	        @block.push(slip 0 xx 55 - $_);
	        @x = pack-le @block;

	        my $bit_len = $buflen +< 3;
	        @x.push: $bit_len +& mask, $bit_len +> 32;
	        $last = True;
	    }
	    default {
	        die "oops";
	    }
	}

	my ($aa, $bb, $cc, $dd) = $a, $b, $c, $d;
	for 0, 4, 8, 12 -> \i {
	    $a = r($a + f($b, $c, $d) + @x[ i+0 ],  3);
	    $d = r($d + f($a, $b, $c) + @x[ i+1 ],  7);
	    $c = r($c + f($d, $a, $b) + @x[ i+2 ], 11);
	    $b = r($b + f($c, $d, $a) + @x[ i+3 ], 19);
	}
	for 0, 1, 2, 3 -> \i {
	    $a = r($a + g($b, $c, $d) + @x[ i+0 ] + 0x5a827999,  3);
	    $d = r($d + g($a, $b, $c) + @x[ i+4 ] + 0x5a827999,  5);
	    $c = r($c + g($d, $a, $b) + @x[ i+8 ] + 0x5a827999,  9);
	    $b = r($b + g($c, $d, $a) + @x[ i+12] + 0x5a827999, 13);
	}
	for 0, 2, 1, 3 -> \i {
	    $a = r($a + h($b, $c, $d) + @x[ i+0 ] + 0x6ed9eba1,  3);
	    $d = r($d + h($a, $b, $c) + @x[ i+8 ] + 0x6ed9eba1,  9);
	    $c = r($c + h($d, $a, $b) + @x[ i+4 ] + 0x6ed9eba1, 11);
	    $b = r($b + h($c, $d, $a) + @x[ i+12] + 0x6ed9eba1, 15);
	}
	$a = ($a + $aa) +& mask;
	$b = ($b + $bb) +& mask;
	$c = ($c + $cc) +& mask;
	$d = ($d + $dd) +& mask;
    }

    sub b2l($n is copy) {
	my $x = 0;
	for ^4 {
	    $x +<= 8;
	    $x += $n +& 0xff;
	    $n +>= 8;
	}
	$x;
    }

    b2l($a) +< 96 +
    b2l($b) +< 64 +
    b2l($c) +< 32 +
    b2l($d);
}

sub MAIN {
    my $str = 'Rosetta Code';
    say md4($str).base(16).lc;
}
```

{{out}}

```txt
a52bcfc6a0d0d300cdc5ddbfbefe478b
```



## Phix

{{trans|D}}
{{trans|Ruby}}

```Phix
--
-- demo\rosetta\md4.exw
--
### ==============

--
function r32(atom a)
    if a<0 then a+=#100000000 end if
    return remainder(a,#100000000)
end function

function rol(atom word, integer bits)
-- left rotate the bits of a 32-bit number by the specified number of bits
    word = r32(word)    -- trim to a 32-bit uint again
    return r32(word*power(2,bits))+floor(word/power(2,32-bits))
end function

function f(atom x,y,z)
    return or_bits(and_bits(x,y),and_bits(not_bits(x),z))
end function

function g(atom x,y,z)
    return or_all({r32(and_bits(x,y)),and_bits(x,z),and_bits(y,z)})
end function

function h(atom x,y,z)
    return xor_bits(r32(xor_bits(x,y)),z)
end function

function md4(sequence data)
    integer bytes_to_add = 64-remainder(length(data)+9,64)
    if bytes_to_add=64 then bytes_to_add = 0 end if
    data = data&#80&repeat(0,bytes_to_add)&
           int_to_bytes(length(data)*8,8)

    atom a = 0x67452301, b = 0xefcdab89, c = 0x98badcfe, d = 0x10325476

    atom m64 = allocate(64,true)
    integer i
    for x=1 to length(data)-1 by 64 do
        poke(m64,data[x..x+63])
        sequence z = peek4u({m64,16})
        atom a2 = a, b2 = b, c2 = c, d2 = d
        for i=0 to 12 by 4 do
            a = rol(a + f(b, c, d) + z[i+1],  3)
            d = rol(d + f(a, b, c) + z[i+2],  7)
            c = rol(c + f(d, a, b) + z[i+3], 11)
            b = rol(b + f(c, d, a) + z[i+4], 19)
        end for
        for i=1 to 4 do
            a = rol(a + g(b, c, d) + z[i+0]  + 0x5a827999,  3)
            d = rol(d + g(a, b, c) + z[i+4]  + 0x5a827999,  5)
            c = rol(c + g(d, a, b) + z[i+8]  + 0x5a827999,  9)
            b = rol(b + g(c, d, a) + z[i+12] + 0x5a827999, 13)
        end for
        for j=1 to 4 do
            i = {1, 3, 2, 4}[j]
            a = rol(a + h(b, c, d) + z[i+0]  + 0x6ed9eba1,  3)
            d = rol(d + h(a, b, c) + z[i+8]  + 0x6ed9eba1,  9)
            c = rol(c + h(d, a, b) + z[i+4]  + 0x6ed9eba1, 11)
            b = rol(b + h(c, d, a) + z[i+12] + 0x6ed9eba1, 15)
        end for
        a = r32(a+a2)
        b = r32(b+b2)
        c = r32(c+c2)
        d = r32(d+d2)
    end for
    poke4(m64,{a,b,c,d})
    return peek({m64,16})
end function

function hexify(sequence s)
    for i=1 to length(s) do
        s[i] = sprintf("%02X",s[i])
    end for
    return join(s,"")
end function

?hexify(md4("Rosetta Code"))
```

{{out}}

```txt

"a52bcfc6a0d0d300cdc5ddbfbefe478b"

```



## PicoLisp

Library and implementation.

```picolisp
(de *Md4-W .
   (1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
   1  5  9 13  2  6 10 14  3  7 11 15  4  8 12 16
   1  9  5 13  3 11  7 15  2 10  6 14  4 12  8 16 .))
(de *Md4-R1 . (3  7 11 19 .))
(de *Md4-R2 . (3  5  9 13 .))
(de *Md4-R3 . (3  9 11 15 .))

(de mod32 (N)
   (& N `(hex "FFFFFFFF")) )

(de not32 (N)
   (x| N `(hex "FFFFFFFF")) )

(de add32 @
   (mod32 (pass +)) )

(de leftRotate (X C)
   (| (mod32 (>> (- C) X)) (>> (- 32 C) X)) )

(de md4 (Str)
   (let Len (length Str)
      (setq Str
         (conc
            (need
               (- 8 (* 64 (/ (+ Len 1 8 63) 64)))  # Pad to 64-8 bytes
               (conc
                  (mapcar char (chop Str))   # Works only with ASCII characters
                  (cons `(hex "80")) )       # '1' bit
               0 )                           # Pad with '0'
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
         R2 `(hex "5A827999")
         R3 `(hex "6ED9EBA1") )
      (while Str
         (let
            (A H0  B H1  C H2  D H3
               W (make
                    (do 16
                       (link
                          (apply |
                          (mapcar >> (0 -8 -16 -24) (cut 4 'Str)) ) ) ) ) )
            (for I 12
               (cond
                  ((>= 4 I)
                     (setq
                        A (leftRotate
                             (add32
                                A
                                (| (& B C) (& (not32 B) D))
                                (get W (pop '*Md4-W)) )
                             (pop '*Md4-R1) )
                        D (leftRotate
                             (add32
                                D
                                (| (& A B) (& (not32 A) C))
                                (get W (pop '*Md4-W)) )
                             (pop '*Md4-R1) )
                        C (leftRotate
                             (add32
                                C
                                (| (& D A) (& (not32 D) B))
                                (get W (pop '*Md4-W)) )
                             (pop '*Md4-R1) )
                        B (leftRotate
                             (add32
                                B
                                (| (& C D) (& (not32 C) A))
                                (get W (pop '*Md4-W)) )
                             (pop '*Md4-R1) ) ) )
                  ((>= 8 I)
                     (setq
                        A (leftRotate
                             (add32
                                A
                                (|
                                   (& B (| C D))
                                   (& C D) )
                                (get W (pop '*Md4-W))
                                R2 )
                             (pop '*Md4-R2) )
                        D (leftRotate
                             (add32
                                D
                                (|
                                   (& A (| B C))
                                   (& B C) )
                                (get W (pop '*Md4-W))
                                R2 )
                             (pop '*Md4-R2) )
                        C (leftRotate
                             (add32
                                C
                                (|
                                   (& D (| A B))
                                   (& A B) )
                                (get W (pop '*Md4-W))
                                R2 )
                             (pop '*Md4-R2) )
                        B (leftRotate
                             (add32
                                B
                                (|
                                   (& C (| D A))
                                   (& D A) )
                                (get W (pop '*Md4-W))
                                R2 )
                             (pop '*Md4-R2) ) ) )
                  (T
                     (setq
                        A (leftRotate
                             (add32
                                A
                                (x| B C D)
                                (get W (pop '*Md4-W))
                                R3 )
                             (pop '*Md4-R3) )
                        D (leftRotate
                             (add32
                                D
                                (x| A B C)
                                (get W (pop '*Md4-W))
                                R3 )
                             (pop '*Md4-R3) )
                        C (leftRotate
                             (add32
                                C
                                (x| D A B)
                                (get W (pop '*Md4-W))
                                R3 )
                             (pop '*Md4-R3) )
                        B (leftRotate
                             (add32
                                B
                                (x| C D A)
                                (get W (pop '*Md4-W))
                                R3 )
                             (pop '*Md4-R3) ) ) ) ) )
               (setq
                  H0 (add32 H0 A)
                  H1 (add32 H1 B)
                  H2 (add32 H2 C)
                  H3 (add32 H3 D) ) ) )
      (make
         (for N (list H0 H1 H2 H3)
            (do 4
               (link (& N 255))
               (setq N (>> 8 N)) ) ) ) ) )

(let Str "Rosetta Code"
   (println
      (pack
         (mapcar
            '((B) (pad 2 (hex B)))
            (md4 Str) ) ) )
   (println
      (pack
         (mapcar
            '((B) (pad 2 (hex B)))
            (native
               "libcrypto.so"
               "MD4"
               '(B . 16)
               Str
               (length Str)
               '(NIL (16)) ) ) ) ) )

(bye)
```



## PHP


```php

echo hash('md4', "Rosetta Code"), "\n";

```

{{out}}

```txt
a52bcfc6a0d0d300cdc5ddbfbefe478b
```




## Python

Use 'hashlib' from python's standard library.
{{libheader|hashlib}}

```python
import hashlib
print hashlib.new("md4",raw_input().encode('utf-16le')).hexdigest().upper()
```



## Racket


```racket

#lang racket
(require (planet soegaard/digest:1:2/digest))
(md4 #"Rosetta Code")

```

{{out}}

```txt

"a52bcfc6a0d0d300cdc5ddbfbefe478b"

```



## Ruby

Use 'openssl' from Ruby's standard library.
{{libheader|OpenSSL}}

```ruby
require 'openssl'
puts OpenSSL::Digest::MD4.hexdigest('Rosetta Code')
```


Implement MD4 in Ruby.


```ruby
require 'stringio'

# Calculates MD4 message digest of _string_. Returns binary digest.
# For hexadecimal digest, use +*md4(str).unpack('H*')+.
def md4(string)
  # functions
  mask = (1 << 32) - 1
  f = proc {|x, y, z| x & y | x.^(mask) & z}
  g = proc {|x, y, z| x & y | x & z | y & z}
  h = proc {|x, y, z| x ^ y ^ z}
  r = proc {|v, s| (v << s).&(mask) | (v.&(mask) >> (32 - s))}

  # initial hash
  a, b, c, d = 0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476

  bit_len = string.size << 3
  string += "\x80"
  while (string.size % 64) != 56
    string += "\0"
  end
  string = string.force_encoding('ascii-8bit') + [bit_len & mask, bit_len >> 32].pack("V2")

  if string.size % 64 != 0
    fail "failed to pad to correct length"
  end

  io = StringIO.new(string)
  block = ""

  while io.read(64, block)
    x = block.unpack("V16")

    # Process this block.
    aa, bb, cc, dd = a, b, c, d
    [0, 4, 8, 12].each {|i|
      a = r[a + f[b, c, d] + x[i],  3]; i += 1
      d = r[d + f[a, b, c] + x[i],  7]; i += 1
      c = r[c + f[d, a, b] + x[i], 11]; i += 1
      b = r[b + f[c, d, a] + x[i], 19]
    }
    [0, 1, 2, 3].each {|i|
      a = r[a + g[b, c, d] + x[i] + 0x5a827999,  3]; i += 4
      d = r[d + g[a, b, c] + x[i] + 0x5a827999,  5]; i += 4
      c = r[c + g[d, a, b] + x[i] + 0x5a827999,  9]; i += 4
      b = r[b + g[c, d, a] + x[i] + 0x5a827999, 13]
    }
    [0, 2, 1, 3].each {|i|
      a = r[a + h[b, c, d] + x[i] + 0x6ed9eba1,  3]; i += 8
      d = r[d + h[a, b, c] + x[i] + 0x6ed9eba1,  9]; i -= 4
      c = r[c + h[d, a, b] + x[i] + 0x6ed9eba1, 11]; i += 8
      b = r[b + h[c, d, a] + x[i] + 0x6ed9eba1, 15]
    }
    a = (a + aa) & mask
    b = (b + bb) & mask
    c = (c + cc) & mask
    d = (d + dd) & mask
  end

  [a, b, c, d].pack("V4")
end

if __FILE__ == $0
  # Print an example MD4 digest.
  str = 'Rosetta Code'
  printf "%s:\n  %s\n", str, *md4(str).unpack('H*')
end
```

{{out}}

```txt
Rosetta Code:
  a52bcfc6a0d0d300cdc5ddbfbefe478b
```



## Rust


```rust
// MD4, based on RFC 1186 and RFC 1320.
//
// https://www.ietf.org/rfc/rfc1186.txt
// https://tools.ietf.org/html/rfc1320
//

use std::fmt::Write;
use std::mem;

// Let not(X) denote the bit-wise complement of X.
// Let X v Y denote the bit-wise OR of X and Y.
// Let X xor Y denote the bit-wise XOR of X and Y.
// Let XY denote the bit-wise AND of X and Y.

// f(X,Y,Z)  =  XY v not(X)Z
fn f(x: u32, y: u32, z: u32) -> u32 {
    (x & y) | (!x & z)
}

// g(X,Y,Z)  =  XY v XZ v YZ
fn g(x: u32, y: u32, z: u32) -> u32 {
    (x & y) | (x & z) | (y & z)
}

// h(X,Y,Z)  =  X xor Y xor Z
fn h(x: u32, y: u32, z: u32) -> u32 {
    x ^ y ^ z
}

// Round 1 macro
// Let [A B C D i s] denote the operation
//   A = (A + f(B,C,D) + X[i]) <<< s
macro_rules! md4round1 {
    ( $a:expr, $b:expr, $c:expr, $d:expr, $i:expr, $s:expr, $x:expr) => {
        {
            // Rust defaults to non-overflowing arithmetic, so we need to specify wrapping add.
            $a = ($a.wrapping_add( f($b, $c, $d) ).wrapping_add( $x[$i] ) ).rotate_left($s);
        }
    };
}

// Round 2 macro
// Let [A B C D i s] denote the operation
//   A = (A + g(B,C,D) + X[i] + 5A827999) <<< s .
macro_rules! md4round2 {
    ( $a:expr, $b:expr, $c:expr, $d:expr, $i:expr, $s:expr, $x:expr) => {
        {
            $a = ($a.wrapping_add( g($b, $c, $d)).wrapping_add($x[$i]).wrapping_add(0x5a827999_u32)).rotate_left($s);
        }
    };
}

// Round 3 macro
// Let [A B C D i s] denote the operation
//   A = (A + h(B,C,D) + X[i] + 6ED9EBA1) <<< s .
macro_rules! md4round3 {
    ( $a:expr, $b:expr, $c:expr, $d:expr, $i:expr, $s:expr, $x:expr) => {
        {
            $a = ($a.wrapping_add(h($b, $c, $d)).wrapping_add($x[$i]).wrapping_add(0x6ed9eba1_u32)).rotate_left($s);
        }
    };
}

fn convert_byte_vec_to_u32(mut bytes: Vec<u8>) -> Vec<u32> {

    bytes.shrink_to_fit();
    let num_bytes = bytes.len();
    let num_words = num_bytes / 4;
    unsafe {
        let words = Vec::from_raw_parts(bytes.as_mut_ptr() as *mut u32, num_words, num_words);
        mem::forget(bytes);
        words
    }
}

// Returns a 128-bit MD4 hash as an array of four 32-bit words.
// Based on RFC 1186 from https://www.ietf.org/rfc/rfc1186.txt
fn md4<T: Into<Vec<u8>>>(input: T) -> [u32; 4] {

    let mut bytes = input.into().to_vec();
    let initial_bit_len = (bytes.len() << 3) as u64;

    // Step 1. Append padding bits
    // Append one '1' bit, then append 0 ≤ k < 512 bits '0', such that the resulting message
    // length in bis is congruent to 448 (mod 512).
    // Since our message is in bytes, we use one byte with a set high-order bit (0x80) plus
    // a variable number of zero bytes.

    // Append zeros
    // Number of padding bytes needed is 448 bits (56 bytes) modulo 512 bits (64 bytes)
    bytes.push(0x80_u8);
    while (bytes.len() % 64) != 56 {
        bytes.push(0_u8);
    }

    // Everything after this operates on 32-bit words, so reinterpret the buffer.
    let mut w = convert_byte_vec_to_u32(bytes);

    // Step 2. Append length
    // A 64-bit representation of b (the length of the message before the padding bits were added)
    // is appended to the result of the previous step, low-order bytes first.
    w.push(initial_bit_len as u32); // Push low-order bytes first
    w.push((initial_bit_len >> 32) as u32);

    // Step 3. Initialize MD buffer
    let mut a = 0x67452301_u32;
    let mut b = 0xefcdab89_u32;
    let mut c = 0x98badcfe_u32;
    let mut d = 0x10325476_u32;

    // Step 4. Process message in 16-word blocks
    let n = w.len();
    for i in 0..n / 16 {

        // Select the next 512-bit (16-word) block to process.
        let x = &w[i * 16..i * 16 + 16];

        let aa = a;
        let bb = b;
        let cc = c;
        let dd = d;

        // [Round 1]
        md4round1!(a, b, c, d, 0, 3, x);  // [A B C D 0 3]
        md4round1!(d, a, b, c, 1, 7, x);  // [D A B C 1 7]
        md4round1!(c, d, a, b, 2, 11, x); // [C D A B 2 11]
        md4round1!(b, c, d, a, 3, 19, x); // [B C D A 3 19]
        md4round1!(a, b, c, d, 4, 3, x);  // [A B C D 4 3]
        md4round1!(d, a, b, c, 5, 7, x);  // [D A B C 5 7]
        md4round1!(c, d, a, b, 6, 11, x); // [C D A B 6 11]
        md4round1!(b, c, d, a, 7, 19, x); // [B C D A 7 19]
        md4round1!(a, b, c, d, 8, 3, x);  // [A B C D 8 3]
        md4round1!(d, a, b, c, 9, 7, x);  // [D A B C 9 7]
        md4round1!(c, d, a, b, 10, 11, x);// [C D A B 10 11]
        md4round1!(b, c, d, a, 11, 19, x);// [B C D A 11 19]
        md4round1!(a, b, c, d, 12, 3, x); // [A B C D 12 3]
        md4round1!(d, a, b, c, 13, 7, x); // [D A B C 13 7]
        md4round1!(c, d, a, b, 14, 11, x);// [C D A B 14 11]
        md4round1!(b, c, d, a, 15, 19, x);// [B C D A 15 19]

        // [Round 2]
        md4round2!(a, b, c, d, 0, 3, x);  //[A B C D 0  3]
        md4round2!(d, a, b, c, 4, 5, x);  //[D A B C 4  5]
        md4round2!(c, d, a, b, 8, 9, x);  //[C D A B 8  9]
        md4round2!(b, c, d, a, 12, 13, x);//[B C D A 12 13]
        md4round2!(a, b, c, d, 1, 3, x);  //[A B C D 1  3]
        md4round2!(d, a, b, c, 5, 5, x);  //[D A B C 5  5]
        md4round2!(c, d, a, b, 9, 9, x);  //[C D A B 9  9]
        md4round2!(b, c, d, a, 13, 13, x);//[B C D A 13 13]
        md4round2!(a, b, c, d, 2, 3, x);  //[A B C D 2  3]
        md4round2!(d, a, b, c, 6, 5, x);  //[D A B C 6  5]
        md4round2!(c, d, a, b, 10, 9, x); //[C D A B 10 9]
        md4round2!(b, c, d, a, 14, 13, x);//[B C D A 14 13]
        md4round2!(a, b, c, d, 3, 3, x);  //[A B C D 3  3]
        md4round2!(d, a, b, c, 7, 5, x);  //[D A B C 7  5]
        md4round2!(c, d, a, b, 11, 9, x); //[C D A B 11 9]
        md4round2!(b, c, d, a, 15, 13, x);//[B C D A 15 13]

        // [Round 3]
        md4round3!(a, b, c, d, 0, 3, x);  //[A B C D 0  3]
        md4round3!(d, a, b, c, 8, 9, x);  //[D A B C 8  9]
        md4round3!(c, d, a, b, 4, 11, x); //[C D A B 4  11]
        md4round3!(b, c, d, a, 12, 15, x);//[B C D A 12 15]
        md4round3!(a, b, c, d, 2, 3, x);  //[A B C D 2  3]
        md4round3!(d, a, b, c, 10, 9, x); //[D A B C 10 9]
        md4round3!(c, d, a, b, 6, 11, x); //[C D A B 6  11]
        md4round3!(b, c, d, a, 14, 15, x);//[B C D A 14 15]
        md4round3!(a, b, c, d, 1, 3, x);  //[A B C D 1  3]
        md4round3!(d, a, b, c, 9, 9, x);  //[D A B C 9  9]
        md4round3!(c, d, a, b, 5, 11, x); //[C D A B 5  11]
        md4round3!(b, c, d, a, 13, 15, x);//[B C D A 13 15]
        md4round3!(a, b, c, d, 3, 3, x);  //[A B C D 3  3]
        md4round3!(d, a, b, c, 11, 9, x); //[D A B C 11 9]
        md4round3!(c, d, a, b, 7, 11, x); //[C D A B 7  11]
        md4round3!(b, c, d, a, 15, 15, x);//[B C D A 15 15]

        a = a.wrapping_add(aa);
        b = b.wrapping_add(bb);
        c = c.wrapping_add(cc);
        d = d.wrapping_add(dd);
    }

    // Step 5. Output
    // The message digest produced as output is A, B, C, D. That is, we begin with the low-order
    // byte of A, and end with the high-order byte of D.
    [u32::from_be(a), u32::from_be(b), u32::from_be(c), u32::from_be(d)]
}

fn digest_to_str(digest: &[u32]) -> String {
    let mut s = String::new();
    for &word in digest {
        write!(&mut s, "{:08x}", word).unwrap();
    }
    s
}

fn main() {
    let val = "Rosetta Code";
    println!("md4(\"{}\") = {}", val, digest_to_str(&md4(val)));
}
```


{{out}}

```txt

md4("Rosetta Code") = a52bcfc6a0d0d300cdc5ddbfbefe478b

```



## Scala

{{libheader|Scala}}

```Scala
import org.bouncycastle.crypto.digests.MD4Digest

object RosettaRIPEMD160 extends App {
  val (raw, messageDigest) = ("Rosetta Code".getBytes("US-ASCII"), new MD4Digest())
  messageDigest.update(raw, 0, raw.length)
  val out = Array.fill[Byte](messageDigest.getDigestSize())(0)
  messageDigest.doFinal(out, 0)

  assert(out.map("%02x".format(_)).mkString == "a52bcfc6a0d0d300cdc5ddbfbefe478b")
  import scala.compat.Platform.currentTime
  println(s"Successfully completed without errors. [total ${currentTime - executionStart} ms]")
}
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "msgdigest.s7i";

const proc: main is func
  begin
    writeln(hex(md4("Rosetta Code")));
  end func;
```


{{out}}

```txt

a52bcfc6a0d0d300cdc5ddbfbefe478b

```



## Sidef

{{trans|Perl}}

```ruby
var digest = frequire('Digest::MD4');
say digest.md4_hex('Rosetta Code');
```

{{out}}

```txt

a52bcfc6a0d0d300cdc5ddbfbefe478b

```



## Tcl

{{tcllib|md4}}

```tcl
package require md4

# Use -hex option for hexadecimal output instead of binary
puts [md4::md4 -hex "Rosetta Code"]
```

{{out}}

```txt
A52BCFC6A0D0D300CDC5DDBFBEFE478B
```

