+++
title = "MD5"
description = ""
date = 2019-10-03T09:15:09Z
aliases = []
[extra]
id = 2252
[taxonomies]
categories = []
tags = []
+++

{{task|Checksums}}

;Task:
Encode a string using an MD5 algorithm.   The algorithm can be found on   [[wp:Md5#Algorithm|Wikipedia]].


Optionally, validate your implementation by running all of the test values in   [http://tools.ietf.org/html/rfc1321 IETF RFC (1321)   for MD5].

Additionally,   RFC 1321   provides more precise information on the algorithm than the Wikipedia article.

{{alertbox|lightgray|'''Warning:'''   MD5 has [http://tools.ietf.org/html/rfc6151 known weaknesses], including '''collisions''' and [http://www.win.tue.nl/hashclash/rogue-ca/ forged signatures].   Users may consider a stronger alternative when doing production-grade cryptography, such as SHA-256 (from the SHA-2 family), or the upcoming SHA-3.}}

If the solution on this page is a library solution, see   [[MD5/Implementation]]   for an implementation from scratch.





## 8th


```forth

"md5" cr:hash! "Some text" cr:hash cr:hash>s
. cr bye

```

{{out}}
<tt>
9db5682a4d778ca2cb79580bdb67083f
</tt>


## Ada

{{works with|GNAT}}

```ada
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.MD5;

procedure MD5_Digest is
begin
   Put(GNAT.MD5.Digest("Foo bar baz"));
end MD5_Digest;
```



## ALGOL 68


```algol68


# Based on wikipedia article pseudocode #

# s specifies the per-round shift amounts #
[]INT s = (7,12,17,22, 7,12,17,22, 7,12,17,22, 7,12,17,22,
           5, 9,14,20, 5, 9,14,20, 5, 9,14,20, 5, 9,14,20,
           4,11,16,23, 4,11,16,23, 4,11,16,23, 4,11,16,23,
           6,10,15,21, 6,10,15,21, 6,10,15,21, 6,10,15,21);

[]BITS k = (16rd76aa478, 16re8c7b756, 16r242070db, 16rc1bdceee,
	    16rf57c0faf, 16r4787c62a, 16ra8304613, 16rfd469501,
	    16r698098d8, 16r8b44f7af, 16rffff5bb1, 16r895cd7be,
	    16r6b901122, 16rfd987193, 16ra679438e, 16r49b40821,
	    16rf61e2562, 16rc040b340, 16r265e5a51, 16re9b6c7aa,
	    16rd62f105d, 16r02441453, 16rd8a1e681, 16re7d3fbc8,
	    16r21e1cde6, 16rc33707d6, 16rf4d50d87, 16r455a14ed,
	    16ra9e3e905, 16rfcefa3f8, 16r676f02d9, 16r8d2a4c8a,
	    16rfffa3942, 16r8771f681, 16r6d9d6122, 16rfde5380c,
	    16ra4beea44, 16r4bdecfa9, 16rf6bb4b60, 16rbebfbc70,
	    16r289b7ec6, 16reaa127fa, 16rd4ef3085, 16r04881d05,
	    16rd9d4d039, 16re6db99e5, 16r1fa27cf8, 16rc4ac5665,
	    16rf4292244, 16r432aff97, 16rab9423a7, 16rfc93a039,
	    16r655b59c3, 16r8f0ccc92, 16rffeff47d, 16r85845dd1,
	    16r6fa87e4f, 16rfe2ce6e0, 16ra3014314, 16r4e0811a1,
	    16rf7537e82, 16rbd3af235, 16r2ad7d2bb, 16reb86d391);

OP + = (BITS a, b) BITS:
   BEGIN
      BITS c = BIN (ABS (a AND 16rffff) + ABS (b AND 16rffff));
      BITS d = BIN (ABS (a SHR 16) + ABS (b SHR 16) + ABS (c SHR 16));
      (c AND 16rffff) OR (d SHL 16)
   END;

#[0:63]LONG INT k;
FOR i FROM 0 TO 63 DO
   k[i] :=  ENTIER (ABS (sin(i+1)) * LONG INT(2)**32)
OD;#

PROC md5 = (STRING intext) STRING:
   BEGIN
      # Initialize variables: #
      BITS a0 := 16r67452301,
           a1 := 16refcdab89,
           a2 := 16r98badcfe,
           a3 := 16r10325476;

      STRING text := intext;

      # Pre-processing: adding a single 1 bit #
      text +:= REPR 128;

      # Pre-processing: padding with zeros
        append "0" bit until message length in bits ≡ 448 (mod 512) #
      WHILE ELEMS text MOD 64 ≠ 56 DO
         text +:= REPR 0
      OD;

      # append original length in bits mod (2 pow 64) to message #
      text +:= dec2asc (ELEMS intext * 8);

      # MD5 rounds #
      # Process the message in successive 512-bit chunks: #
      WHILE text ≠ "" DO
	 # for each 512-bit (64 byte) chunk of message #
	 []CHAR chunk = text[1:64]; text := text[65:];
	 # break chunk into sixteen 32-bit words M[j], 0 <= j <= 15 #
	 [0:15]BITS m;
         FOR j FROM 0 TO 15 DO
             m[j] := BIN (ABS chunk[j*4+1]) OR
	             BIN (ABS chunk[j*4+2]) SHL 8 OR
	             BIN (ABS chunk[j*4+3]) SHL 16 OR
	             BIN (ABS chunk[j*4+4]) SHL 24
          OD;
          INT g;
	  BITS a, b, c, d, f, dtemp;

	  # Initialize hash value for this chunk #
	  a := a0;
          b := a1;
          c := a2;
          d := a3;
          FOR i FROM 0 TO 63 DO
             IF 0 <= i AND i <= 15 THEN
		f := (b AND c) OR ((NOT b) AND d);
		g := i
	     ELIF 16 <= i AND i <= 31 THEN
		 f := (d AND b) OR ((NOT d) AND c);
		 g := (5×i + 1) MOD 16
	     ELIF 32 <= i AND i <= 47 THEN
		 f := b XOR c XOR d;
		 g := (3×i + 5) MOD 16
	     ELIF 48 <= i AND i <= 63 THEN
		 f := c XOR (b OR (NOT d));
		 g := (7×i) MOD 16
	     FI;
	     dtemp := d;
	     d := c;
	     c := b;
	     b := b + leftrotate ((a + f + k[1+i] + m[g]), s[1+i]);
	     a := dtemp
          OD;
	  # Add this chunk's hash to result so far #
          a0 := a0 + a;
          a1 := a1 + b;
          a2 := a2 + c;
          a3 := a3 + d
       OD;
       revhex (a0) + revhex (a1) + revhex (a2) + revhex (a3)
    END;

PROC leftrotate = (BITS x, INT c) BITS:
    (x SHL c) OR (x SHR (32-c));

# dec2asc: dec to 8 byte asc #
PROC dec2asc = (INT nn)STRING:
   BEGIN
      STRING h := ""; INT n := nn;
      FOR i TO 8 DO
         h +:= REPR (n MOD 256);
         n ÷:= 256
      OD;
      h
   END;

 PROC revhex = (BITS x) STRING :
   BEGIN # Convert to lowercase hexadecimal STRING #
      PROC hexdig = (BITS x) CHAR: (REPR (ABS(x) <= 9 | ABS(x) + ABS("0") | ABS(x) - 10 + ABS("a")));
      hexdig (x SHR 4 AND 16rf) +
      hexdig (x AND 16rf) +
      hexdig (x SHR 12 AND 16rf) +
      hexdig (x SHR 8 AND 16rf) +
      hexdig (x SHR 20 AND 16rf) +
      hexdig (x SHR 16 AND 16rf) +
      hexdig (x SHR 28 AND 16rf) +
      hexdig (x SHR 24 AND 16rf)
   END;

STRING testmsg = "The quick brown fox jumps over the lazy dog";
STRING checksum = "9e107d9d372bb6826bd81d3542a419d6";

print ((testmsg, new line));
print ((checksum, new line));

STRING test = md5 (testmsg);

IF test = checksum THEN
   print (("passed", new line));
   print ((test, new line))
ELSE
   print (("failed"))
FI

```


=={{Header|AutoHotkey}}==
{{AutoHotkey case}}

### Regular version

Source: [http://www.autohotkey.com/forum/post-275910.html#275910 AutoHotkey forum] by SKAN

```autohotkey
data := "abc"
MsgBox % MD5(data,StrLen(data)) ; 900150983cd24fb0d6963f7d28e17f72

MD5( ByRef V, L=0 ) {
 VarSetCapacity( MD5_CTX,104,0 ), DllCall( "advapi32\MD5Init", Str,MD5_CTX )
 DllCall( "advapi32\MD5Update", Str,MD5_CTX, Str,V, UInt,L ? L : VarSetCapacity(V) )
 DllCall( "advapi32\MD5Final", Str,MD5_CTX )
 Loop % StrLen( Hex:="123456789ABCDEF0" )
  N := NumGet( MD5_CTX,87+A_Index,"Char"), MD5 .= SubStr(Hex,N>>4,1) . SubStr(Hex,N&15,1)
Return MD5
}

```



### Native implementation

Source: [http://www.autohotkey.com/forum/topic17853.html AutoHotkey forum] by Laszlo

```autohotkey
; GLOBAL CONSTANTS r[64], k[64]
r =  12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22
, 5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20
, 4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23
, 6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21
StringSplit r, r, `,
r0 := 7
Loop 64
   i := A_Index-1, k%i% := floor(abs(sin(A_Index)) * 2**32)

; TEST CASES
MsgBox % MD5(x:="", 0) ; d41d8cd98f00b204e9800998ecf8427e
MsgBox % MD5(x:="a", StrLen(x)) ; 0cc175b9c0f1b6a831c399e269772661
MsgBox % MD5(x:="abc", StrLen(x)) ; 900150983cd24fb0d6963f7d28e17f72
MsgBox % MD5(x:="message digest", StrLen(x)) ; f96b697d7cb7938d525a2f31aaf161d0
MsgBox % MD5(x:="abcdefghijklmnopqrstuvwxyz", StrLen(x))
; c3fcd3d76192e4007dfb496cca67e13b
MsgBox % MD5(x:="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", StrLen(x))
; d174ab98d277d9f5a5611c2c9f419d9f
MsgBox % MD5(x:="12345678901234567890123456789012345678901234567890123456789012345678901234567890", StrLen(x))
; 57edf4a22be3c955ac49da2e2107b67a
MsgBox % MD5(x:="The quick brown fox jumps over the lazy dog", StrLen(x))
; 9e107d9d372bb6826bd81d3542a419d6
MsgBox % MD5(x:="The quick brown fox jumps over the lazy cog", StrLen(x))
; 1055d3e698d289f2af8663725127bd4b

MD5(ByRef Buf, L) { ; Binary buffer, Length in bytes
   Static P, Q, N, i, a,b,c,d, t, h0,h1,h2,h3, y = 0xFFFFFFFF

   h0 := 0x67452301, h1 := 0xEFCDAB89, h2 := 0x98BADCFE, h3 := 0x10325476

   N := ceil((L+9)/64)*64 ; padded length (100..separator, 8B length)
   VarSetCapacity(Q,N,0)  ; room for padded data
   P := &Q ; pointer
   DllCall("RtlMoveMemory", UInt,P, UInt,&Buf, UInt,L) ; copy data
   DllCall("RtlFillMemory", UInt,P+L, UInt,1, UInt,0x80) ; pad separator
   DllCall("ntdll.dll\RtlFillMemoryUlong",UInt,P+N-8,UInt,4,UInt,8*L) ; at end: length in bits < 512 MB

   Loop % N//64 {
      Loop 16
         i := A_Index-1, w%i% := *P | *(P+1)<<8 | *(P+2)<<16 | *(P+3)<<24, P += 4

      a := h0, b := h1, c := h2, d := h3

      Loop 64 {
         i := A_Index-1
         If i < 16
             f := (b & c) | (~b & d), g := i
         Else If i < 32
             f := (d & b) | (~d & c), g := 5*i+1 & 15
         Else If i < 48
             f := b ^ c ^ d,          g := 3*i+5 & 15
         Else
             f := c ^ (b | ~d),       g :=  7*i  & 15

         t := d, d := c, c := b
         b += rotate(a + f + k%i% + w%g%, r%i%) ; reduced to 32 bits later
         a := t
      }

      h0 := h0+a & y, h1 := h1+b & y, h2 := h2+c & y, h3 := h3+d & y
   }
   Return hex(h0) . hex(h1) . hex(h2) . hex(h3)
}

rotate(a,b) { ; 32-bit rotate a to left by b bits, bit32..63 garbage
   Return a << b | (a & 0xFFFFFFFF) >> (32-b)
}

hex(x) {      ; 32-bit little endian hex digits
   SetFormat Integer, HEX
   x += 0x100000000, x := SubStr(x,-1) . SubStr(x,8,2) . SubStr(x,6,2) . SubStr(x,4,2)
   SetFormat Integer, DECIMAL
   Return x
}
```



## BASIC


## BaCon


```freebasic

PRAGMA INCLUDE <stdio.h>
PRAGMA INCLUDE <stdlib.h>
PRAGMA INCLUDE <string.h>
PRAGMA INCLUDE <openssl/md5.h>
PRAGMA LDFLAGS -lcrypto -lm -w

DECLARE result TYPE unsigned char *
DECLARE string TYPE const char *

string = "Rosetta code"
strlenght = LEN(string)

result = MD5( string, strlenght , 0)

FOR  i = 0 TO MD5_DIGEST_LENGTH-1
    PRINT   result[i] FORMAT "%02x"
NEXT
```


=={{Header|BBC BASIC}}==
{{works with|BBC BASIC for Windows}}
See [[MD5/Implementation]] for a native version.

```bbcbasic
      PRINT FN_MD5("")
      PRINT FN_MD5("a")
      PRINT FN_MD5("abc")
      PRINT FN_MD5("message digest")
      PRINT FN_MD5("abcdefghijklmnopqrstuvwxyz")
      PRINT FN_MD5("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
      PRINT FN_MD5(STRING$(8,"1234567890"))
      END

      DEF FN_MD5(message$)
      LOCAL I%, MD5$, MD5_CTX{}
      DIM MD5_CTX{i%(1), buf%(3), in&(63), digest&(15)}
      SYS "MD5Init", MD5_CTX{}
      SYS "MD5Update", MD5_CTX{}, message$, LEN(message$)
      SYS "MD5Final", MD5_CTX{}
      FOR I% = 0 TO 15
        MD5$ += RIGHT$("0"+STR$~(MD5_CTX.digest&(I%)),2)
      NEXT
      = MD5$
```



## C

{{libheader|OpenSSL}}

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <openssl/md5.h>

const char *string = "The quick brown fox jumped over the lazy dog's back";

int main()
{
  int i;
  unsigned char result[MD5_DIGEST_LENGTH];

  MD5(string, strlen(string), result);

  // output
  for(i = 0; i < MD5_DIGEST_LENGTH; i++)
    printf("%02x", result[i]);
  printf("\n");

  return EXIT_SUCCESS;
}
```

Implementation of md5
(Needs review - differences observed for the last 8 characters when compared with openssl implementation)

```cpp
#include <iostream>
#include <stdio.h>
#include <string.h>
#include <math.h>

typedef union uwb {
    unsigned w;
    unsigned char b[4];
} WBunion;

typedef unsigned Digest[4];

unsigned f0( unsigned abcd[] ){
    return ( abcd[1] & abcd[2]) | (~abcd[1] & abcd[3]);}

unsigned f1( unsigned abcd[] ){
    return ( abcd[3] & abcd[1]) | (~abcd[3] & abcd[2]);}

unsigned f2( unsigned abcd[] ){
    return  abcd[1] ^ abcd[2] ^ abcd[3];}

unsigned f3( unsigned abcd[] ){
    return abcd[2] ^ (abcd[1] |~ abcd[3]);}

typedef unsigned (*DgstFctn)(unsigned a[]);

unsigned *calcKs( unsigned *k)
{
    double s, pwr;
    int i;

    pwr = pow( 2, 32);
    for (i=0; i<64; i++) {
        s = fabs(sin(1+i));
        k[i] = (unsigned)( s * pwr );
    }
    return k;
}

// ROtate v Left by amt bits
unsigned rol( unsigned v, short amt )
{
    unsigned  msk1 = (1<<amt) -1;
    return ((v>>(32-amt)) & msk1) | ((v<<amt) & ~msk1);
}

unsigned *md5( const char *msg, int mlen)
{
    static Digest h0 = { 0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476 };
//    static Digest h0 = { 0x01234567, 0x89ABCDEF, 0xFEDCBA98, 0x76543210 };
    static DgstFctn ff[] = { &f0, &f1, &f2, &f3 };
    static short M[] = { 1, 5, 3, 7 };
    static short O[] = { 0, 1, 5, 0 };
    static short rot0[] = { 7,12,17,22};
    static short rot1[] = { 5, 9,14,20};
    static short rot2[] = { 4,11,16,23};
    static short rot3[] = { 6,10,15,21};
    static short *rots[] = {rot0, rot1, rot2, rot3 };
    static unsigned kspace[64];
    static unsigned *k;

    static Digest h;
    Digest abcd;
    DgstFctn fctn;
    short m, o, g;
    unsigned f;
    short *rotn;
    union {
        unsigned w[16];
        char     b[64];
    }mm;
    int os = 0;
    int grp, grps, q, p;
    unsigned char *msg2;

    if (k==NULL) k= calcKs(kspace);

    for (q=0; q<4; q++) h[q] = h0[q];   // initialize

    {
        grps  = 1 + (mlen+8)/64;
        msg2 = malloc( 64*grps);
        memcpy( msg2, msg, mlen);
        msg2[mlen] = (unsigned char)0x80;
        q = mlen + 1;
        while (q < 64*grps){ msg2[q] = 0; q++ ; }
        {
//            unsigned char t;
            WBunion u;
            u.w = 8*mlen;
//            t = u.b[0]; u.b[0] = u.b[3]; u.b[3] = t;
//            t = u.b[1]; u.b[1] = u.b[2]; u.b[2] = t;
            q -= 8;
            memcpy(msg2+q, &u.w, 4 );
        }
    }

    for (grp=0; grp<grps; grp++)
    {
        memcpy( mm.b, msg2+os, 64);
        for(q=0;q<4;q++) abcd[q] = h[q];
        for (p = 0; p<4; p++) {
            fctn = ff[p];
            rotn = rots[p];
            m = M[p]; o= O[p];
            for (q=0; q<16; q++) {
                g = (m*q + o) % 16;
                f = abcd[1] + rol( abcd[0]+ fctn(abcd) + k[q+16*p] + mm.w[g], rotn[q%4]);

                abcd[0] = abcd[3];
                abcd[3] = abcd[2];
                abcd[2] = abcd[1];
                abcd[1] = f;
            }
        }
        for (p=0; p<4; p++)
            h[p] += abcd[p];
        os += 64;
    }

    if( msg2 )
        free( msg2 );

    return h;
}

int main( int argc, char *argv[] )
{
    int j,k;
    const char *msg = "The quick brown fox jumps over the lazy dog.";
    unsigned *d = md5(msg, strlen(msg));
    WBunion u;

    printf("= 0x");
    for (j=0;j<4; j++){
        u.w = d[j];
        for (k=0;k<4;k++) printf("%02x",u.b[k]);
    }
    printf("\n");

    return 0;
}
```



## C++

{{libheader|Poco Crypto}}

```cpp
#include <string>
#include <iostream>
#include "Poco/MD5Engine.h"
#include "Poco/DigestStream.h"

using Poco::DigestEngine ;
using Poco::MD5Engine ;
using Poco::DigestOutputStream ;

int main( ) {
   std::string myphrase ( "The quick brown fox jumped over the lazy dog's back" ) ;
   MD5Engine md5 ;
   DigestOutputStream outstr( md5 ) ;
   outstr << myphrase ;
   outstr.flush( ) ; //to pass everything to the digest engine
   const DigestEngine::Digest& digest = md5.digest( ) ;
   std::cout << myphrase << " as a MD5 digest :\n" << DigestEngine::digestToHex( digest )
      << " !" << std::endl ;
   return 0 ;
}
```

{{out}}

```txt
The quick brown fox jumped over the lazy dog's back as a MD5 digest :
e38ca1d920c4b8b8d3946b2c72f01680 !

```


=={{header|C sharp|C#}}==

```csharp
using System.Text;
using System.Security.Cryptography;

byte[] data = Encoding.ASCII.GetBytes("The quick brown fox jumped over the lazy dog's back");
byte[] hash = MD5.Create().ComputeHash(data);
Console.WriteLine(BitConverter.ToString(hash).Replace("-", "").ToLower());
```


=={{header|Caché ObjectScript}}==


```txt
USER>set hash=$System.Encryption.MD5Hash("The quick brown fox jumped over the lazy dog's back")
USER>zzdump hash
0000: E3 8C A1 D9 20 C4 B8 B8 D3 94 6B 2C 72 F0 16 80
```



## Clojure


```lisp
(apply str
  (map (partial format "%02x")
    (.digest (doto (java.security.MessageDigest/getInstance "MD5")
                   .reset
                   (.update (.getBytes "The quick brown fox jumps over the lazy dog"))))))
```



## Common Lisp

{{libheader|Ironclad}}

```lisp
(ql:quickload 'ironclad)
(defun md5 (str)
  (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence :md5
                              (ironclad:ascii-string-to-byte-array str))))
(defvar *tests* '(""
                  "a"
                  "abc"
                  "message digest"
                  "abcdefghijklmnopqrstuvwxyz"
                  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
                  "12345678901234567890123456789012345678901234567890123456789012345678901234567890"))
(dolist (msg *tests*)
  (format T "~s: ~a~%" msg (md5 msg)))

```


{{Out}}

```txt
"": d41d8cd98f00b204e9800998ecf8427e
"a": 0cc175b9c0f1b6a831c399e269772661
"abc": 900150983cd24fb0d6963f7d28e17f72
"message digest": f96b697d7cb7938d525a2f31aaf161d0
"abcdefghijklmnopqrstuvwxyz": c3fcd3d76192e4007dfb496cca67e13b
"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789": d174ab98d277d9f5a5611c2c9f419d9f
"12345678901234567890123456789012345678901234567890123456789012345678901234567890": 57edf4a22be3c955ac49da2e2107b67a
```


<h5>Another method using openssl:</h5> {{libheader|CFFI}}

```lisp
(cffi:load-foreign-library "libcrypto.so")

(cffi:defcfun ("MD5" MD5) :void (string :string) (len :int) (ptr :pointer))

(let ((string-to-convert "The quick brown fox jumped over the lazy dog's back")
      (ptr (cffi:foreign-alloc :unsigned-char :count 16)))
  (md5 string-to-convert (length string-to-convert) ptr)
  (loop for i from 0 below 16 do
       (format t "~a" (write-to-string (cffi:mem-ref ptr :unsigned-char i) :base 16)))
  (cffi:foreign-free ptr))
```


{{Out}}
```txt
E38CA1D920C4B8B8D3946B2C72F01680
```



## D


```d
void main() {
    import std.stdio, std.digest.md;

    auto txt = "The quick brown fox jumped over the lazy dog's back";
    writefln("%-(%02x%)", txt.md5Of);
}
```

{{out}}

```txt
e38ca1d920c4b8b8d3946b2c72f01680
```

Alternative version:
{{libheader|Tango}}

```d
import tango.io.digest.Md5, tango.io.Stdout;

void main(char[][] args) {
  auto md5 = new Md5();
  for(int i = 1; i < args.length; i++) {
    md5.update(args[i]);
    Stdout.formatln("[{}]=>\n[{}]", args[i], md5.hexDigest());
  }
}
```

Output:

```txt
>md5test "The quick brown fox jumped over the lazy dog's back"
[The quick brown fox jumped over the lazy dog's back]=>
[e38ca1d920c4b8b8d3946b2c72f01680]
```



## Delphi

If you require a native implementation, look inside the class '''TIdHashMessageDigest5'''. This class is placed in the unit '''IdHashMessageDigest.pas'''.

```Delphi
program MD5Hash;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  IdHashMessageDigest;

function MD5(aValue: string): string;
begin
  with TIdHashMessageDigest5.Create do
  begin
    Result:= HashStringAsHex(aValue);
    Free;
  end;
end;

begin
  Writeln(MD5(''));
  Writeln(MD5('a'));
  Writeln(MD5('abc'));
  Writeln(MD5('message digest'));
  Writeln(MD5('abcdefghijklmnopqrstuvwxyz'));
  Writeln(MD5('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'));
  Writeln(MD5('12345678901234567890123456789012345678901234567890123456789012345678901234567890'));
  Readln;
end.
```

'''Output:'''

```txt

D41D8CD98F00B204E9800998ECF8427E
0CC175B9C0F1B6A831C399E269772661
900150983CD24FB0D6963F7D28E17F72
F96B697D7CB7938D525A2F31AAF161D0
C3FCD3D76192E4007DFB496CCA67E13B
D174AB98D277D9F5A5611C2C9F419D9F
57EDF4A22BE3C955AC49DA2E2107B67A

```



## E

{{works with|E-on-Java}}
{{trans|Java}} (with modifications)

```e>def makeMessageDigest := <import:java.security.makeMessageDigest

def sprintf := <import:java.lang.makeString>.format

def digest := makeMessageDigest.getInstance("MD5") \
  .digest("The quick brown fox jumped over the lazy dog's back".getBytes("iso-8859-1"))

for b in digest {
  print(sprintf("%02x", [b]))
}
println()
```



## Emacs Lisp


Emacs 23 and up includes <code>md5</code> giving the MD5 hash as a hexadecimal string.  (See [http://www.gnu.org/software/emacs/manual/html_node/elisp/Checksum_002fHash.html GNU Elisp manual on Checksum/Hash]).


```Lisp
(md5 "The quick brown fox jumped over the lazy dog's back")
=>
"e38ca1d920c4b8b8d3946b2c72f01680"
```


This began in [http://git.chise.org/elisp/flim/ FLIM] and FLIM has an <code>md5.el</code> which creates an <code>md5</code> func if it doesn't already exist.

<code>hmac-md5.el</code> also from FLIM and also included in recent Emacs has an <code>md5-binary</code> giving the checksum as 16 binary bytes.  <code>encode-hex-string</code> from <code>hex-util.el</code> can convert that to hex the same as the <code>md5</code> func gives.


```Lisp
(require 'hmac-md5)
(require 'hex-util)
(encode-hex-string
  (md5-binary "The quick brown fox jumped over the lazy dog's back"))
=>
"e38ca1d920c4b8b8d3946b2c72f01680"
```



## Erlang

By default, Erlang's crypto functions like md5 return a binary value rather than a hex string. We have two write our own function to translate it:

```Erlang
-module(tests).
-export([md5/1]).

md5(S) ->
 string:to_upper(
  lists:flatten([io_lib:format("~2.16.0b",[N]) || <<N>> <= erlang:md5(S)])
 ).
```

Testing it:

```erlang>1
 c(tests).
{ok,tests}
2> tests:md5("The quick brown fox jumped over the lazy dog's back").
"E38CA1D920C4B8B8D3946B2C72F01680"
```



## F#

Using built-in System.Security.Cryptography.MD5 class (Link to original blog [https://znprojects.blogspot.com/2017/04/md5-in-f-functionally.html]).


```F#
let md5ootb (msg: string) =
  use md5 = System.Security.Cryptography.MD5.Create()
  msg
  |> System.Text.Encoding.ASCII.GetBytes
  |> md5.ComputeHash
  |> Seq.map (fun c -> c.ToString("X2"))
  |> Seq.reduce ( + )

md5ootb @"The quick brown fox jumped over the lazy dog's back"
```



## Factor

Using builtin library:

 USING: kernel strings io checksums checksums.md5 ;
 "The quick brown fox jumps over the lazy dog"
 md5 checksum-bytes hex-string print


## Forth

{{libheader|Forth Foundation Library}}

```forth
include ffl/md5.fs

\ Create a MD5 variable md1 in the dictionary

md5-create md1

\ Update the variable with data

s" The quick brown fox jumps over the lazy dog" md1 md5-update

\ Finish the MD5 calculation resulting in four unsigned 32 bit words
\ on the stack representing the hash value

md1 md5-finish

\ Convert the hash value to a hex string and print it

md5+to-string type cr
```


## Fortran


### Intel Fortran on Windows

Using Windows API. See [https://msdn.microsoft.com/en-us/library/aa379886.aspx CryptAcquireContext], [https://msdn.microsoft.com/en-us/library/aa379908.aspx CryptCreateHash], [https://msdn.microsoft.com/en-us/library/aa380202.aspx CryptHashData] and [https://msdn.microsoft.com/en-us/library/aa379947.aspx CryptGetHashParam] on MSDN.


```fortran
module md5_mod
    use kernel32
    use advapi32
    implicit none
    integer, parameter :: MD5LEN = 16
contains
    subroutine md5hash(name, hash, dwStatus, filesize)
        implicit none
        character(*) :: name
        integer, parameter :: BUFLEN = 32768
        integer(HANDLE) :: hFile, hProv, hHash
        integer(DWORD) :: dwStatus, nRead
        integer(BOOL) :: status
        integer(BYTE) :: buffer(BUFLEN)
        integer(BYTE) :: hash(MD5LEN)
        integer(UINT64) :: filesize

        dwStatus = 0
        filesize = 0
        hFile = CreateFile(trim(name) // char(0), GENERIC_READ, FILE_SHARE_READ, NULL, &
                           OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, NULL)

        if (hFile == INVALID_HANDLE_VALUE) then
            dwStatus = GetLastError()
            print *, "CreateFile failed."
            return
        end if

        if (CryptAcquireContext(hProv, NULL, NULL, PROV_RSA_FULL, &
                                CRYPT_VERIFYCONTEXT) == FALSE) then
            dwStatus = GetLastError()
            print *, "CryptAcquireContext failed."
            goto 3
        end if

        if (CryptCreateHash(hProv, CALG_MD5, 0_ULONG_PTR, 0_DWORD, hHash) == FALSE) then
            dwStatus = GetLastError()
            print *, "CryptCreateHash failed."
            go to 2
        end if

        do
            status = ReadFile(hFile, loc(buffer), BUFLEN, nRead, NULL)
            if (status == FALSE .or. nRead == 0) exit
            filesize = filesize + nRead
            if (CryptHashData(hHash, buffer, nRead, 0) == FALSE) then
                dwStatus = GetLastError()
                print *, "CryptHashData failed."
                go to 1
            end if
        end do

        if (status == FALSE) then
            dwStatus = GetLastError()
            print *, "ReadFile failed."
            go to 1
        end if

        nRead = MD5LEN
        if (CryptGetHashParam(hHash, HP_HASHVAL, hash, nRead, 0) == FALSE) then
            dwStatus = GetLastError()
            print *, "CryptGetHashParam failed.", status, nRead, dwStatus
        end if

      1 status = CryptDestroyHash(hHash)
      2 status = CryptReleaseContext(hProv, 0)
      3 status = CloseHandle(hFile)
    end subroutine
end module

program md5
    use md5_mod
    implicit none
    integer :: n, m, i, j
    character(:), allocatable :: name
    integer(DWORD) :: dwStatus
    integer(BYTE) :: hash(MD5LEN)
    integer(UINT64) :: filesize

    n = command_argument_count()
    do i = 1, n
        call get_command_argument(i, length=m)
        allocate(character(m) :: name)
        call get_command_argument(i, name)
        call md5hash(name, hash, dwStatus, filesize)
        if (dwStatus == 0) then
            do j = 1, MD5LEN
                write(*, "(Z2.2)", advance="NO") hash(j)
            end do
            write(*, "(' ',A,' (',G0,' bytes)')") name, filesize
        end if
        deallocate(name)
    end do
end program
```



## FreeBASIC

See [[MD5/Implementation#FreeBASIC]]


## Frink

The function <CODE>messageDigest[string, hash]</CODE> returns a hex-encoded hash of any input string with a variety of hashing functions.

```frink
println[messageDigest["The quick brown fox", "MD5"]]
```



## Futhark


Real languages roll their own crypto.


```Futhark

type md5 = (u32, u32, u32, u32)

fun rs(): [64]u32 =
  map u32
  ([ 7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
     5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
     4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
     6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21 ])

fun ks(): [64]u32 =
  map u32
  ([ 0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee ,
     0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501 ,
     0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be ,
     0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821 ,
     0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa ,
     0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8 ,
     0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed ,
     0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a ,
     0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c ,
     0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70 ,
     0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05 ,
     0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665 ,
     0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039 ,
     0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1 ,
     0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1 ,
     0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391 ])

fun md5(ms: [n][16]u32): md5 =
  let a0 = u32(0x67452301)
  let b0 = u32(0xefcdab89)
  let c0 = u32(0x98badcfe)
  let d0 = u32(0x10325476)
  loop ((a0,b0,c0,d0)) = for i < n do
    let (a,b,c,d) = md5_chunk (a0,b0,c0,d0) ms[i]
    in (a0+a, b0+b, c0+c, d0+d)
  in (a0,b0,c0,d0)

fun rotate_left(x: u32, c: u32): u32 = (x << c) | (x >> (32u32 - c))

fun bytes(x: u32): [4]u8 = [u8(x),
                            u8(x/0x100u32),
                            u8(x/0x10000u32),
                            u8(x/0x1000000u32)]

fun unbytes(bs: [4]u8): u32 =
  u32(bs[0]) +
  u32(bs[1]) * 0x100u32 +
  u32(bs[2]) * 0x10000u32 +
  u32(bs[3]) * 0x1000000u32

fun unbytes_block(block: [64]u8): [16]u32 =
  map unbytes (reshape (16,4) block)

fun main(ms: [n]u8): [16]u8 =
  let padding = 64 - (n % 64)
  let n_padded = n + padding
  let ms_padded = concat ms (bytes 0x80u32) (replicate (padding-12) 0x0u8) (bytes (u32(n*8))) ([0u8,0u8,0u8,0u8])
  let (a,b,c,d) = md5 (map unbytes_block (reshape (n_padded / 64, 64) ms_padded))
  in reshape 16 (map bytes ([a,b,c,d]))

-- Process 512 bits of the input.
fun md5_chunk ((a0,b0,c0,d0): md5) (m: [16]u32): md5 =
  loop ((a,b,c,d) = (a0,b0,c0,d0)) = for i < 64 do
    let (f,g) =
      if      i < 16 then ((b & c) | ((~b) & d),
                           i)
      else if i < 32 then ((d & b) | ((~d) & c),
                           i32((5u32*u32(i) + 1u32) % 16u32))
      else if i < 48 then (b ^ c ^ d,
                           i32((3u32*u32(i) + 5u32) % 16u32))
      else                (c ^ (b | (~d)),
                           i32((7u32*u32(i))        % 16u32))
    in (d, b + rotate_left(a + f + (ks())[i] + m[g], (rs())[i]), b, c)
  in (a,b,c,d)

```



## Go


```go
package main

import (
    "crypto/md5"
    "fmt"
)

func main() {
    for _, p := range [][2]string{
        // RFC 1321 test cases
        {"d41d8cd98f00b204e9800998ecf8427e", ""},
        {"0cc175b9c0f1b6a831c399e269772661", "a"},
        {"900150983cd24fb0d6963f7d28e17f72", "abc"},
        {"f96b697d7cb7938d525a2f31aaf161d0", "message digest"},
        {"c3fcd3d76192e4007dfb496cca67e13b", "abcdefghijklmnopqrstuvwxyz"},
        {"d174ab98d277d9f5a5611c2c9f419d9f",
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"},
        {"57edf4a22be3c955ac49da2e2107b67a", "12345678901234567890" +
            "123456789012345678901234567890123456789012345678901234567890"},
        // test case popular with other RC solutions
        {"e38ca1d920c4b8b8d3946b2c72f01680",
            "The quick brown fox jumped over the lazy dog's back"},
    } {
        validate(p[0], p[1])
    }
}

var h = md5.New()

func validate(check, s string) {
    h.Reset()
    h.Write([]byte(s))
    sum := fmt.Sprintf("%x", h.Sum(nil))
    if sum != check {
        fmt.Println("MD5 fail")
        fmt.Println("  for string,", s)
        fmt.Println("  expected:  ", check)
        fmt.Println("  got:       ", sum)
    }
}
```



## Groovy


```Groovy
import java.security.MessageDigest

String.metaClass.md5Checksum = {
    MessageDigest.getInstance('md5').digest(delegate.bytes).collect { String.format("%02x", it) }.join('')
}
```

Testing

```Groovy
assert 'The quick brown fox jumps over the lazy dog'.md5Checksum() == '9e107d9d372bb6826bd81d3542a419d6'
```



## Haskell

Use modules nano-MD5 and ByteString from [http://hackage.haskell.org/packages/hackage.html HackageDB]

```Haskell
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.ByteString (pack)
import Data.Char (ord)

main = do
  let message = "The quick brown fox jumped over the lazy dog's back"
      digest  = (md5sum . pack . map (fromIntegral . ord)) message
  putStrLn digest
```


Use in GHCi:

```txt
*Main> main
e38ca1d920c4b8b8d3946b2c72f01680
```


This version uses the [https://hackage.haskell.org/package/cryptonite Cryptonite] package:
{{libheader|Cryptonite}}

```haskell
#!/usr/bin/env runhaskell

import Data.ByteString.Char8 (pack)
import System.Environment (getArgs)
import Crypto.Hash

main :: IO ()
main = print . md5 . pack . unwords =<< getArgs
         where md5 x = hash x :: Digest MD5
```

{{out}}

```txt

$ ./md5.hs The quick brown fox jumped over the lazy dog\'s back
e38ca1d920c4b8b8d3946b2c72f01680

```



## Io


```io>Io
 MD5
==>  MD5_0x97663e0:
  appendSeq        = MD5_appendSeq()
  md5              = MD5_md5()
  md5String        = MD5_md5String()
Io> MD5 clone appendSeq("The quick brown fox jumped over the lazy dog's back") md5String
==> e38ca1d920c4b8b8d3946b2c72f01680
```


=={{header|Icon}} and {{header|Unicon}}==
The following program demonstrates the MD5 using a native Icon/Unicon implementation (see [[MD5/Implementation]]) and checks the results against reference values. Alternate implementations using call outs to md5sum on Linux or fciv on windows are possible but were not coded.

```Icon
procedure main()  # validate against the RFC test strings and more
   testMD5("The quick brown fox jumps over the lazy dog", 16r9e107d9d372bb6826bd81d3542a419d6)
   testMD5("The quick brown fox jumps over the lazy dog.", 16re4d909c290d0fb1ca068ffaddf22cbd0)
   testMD5("", 16rd41d8cd98f00b204e9800998ecf8427e)
end

procedure testMD5(s,rh)  # compute the MD5 hash and compare it to reference value
   write("Message(length=",*s,") = ",image(s))
   write("Digest = ",hexstring(h := MD5(s)),if h = rh then " matches reference hash" else (" does not match reference hash = " || hexstring(rh)),"\n")
end
```


Sample Output:
```txt
Message(length=43) = "The quick brown fox jumps over the lazy dog"
Digest = 9E107D9D372BB6826BD81D3542A419D6 matches reference hash

Message(length=44) = "The quick brown fox jumps over the lazy dog."
Digest = E4D909C290D0FB1CA068FFADDF22CBD0 matches reference hash

Message(length=0) = ""
Digest = D41D8CD98F00B204E9800998ECF8427E matches reference hash
```



## J

Using the <tt>[http://www.jsoftware.com/wsvn/addons/trunk/convert/misc/md5.ijs md5]</tt> script from the <tt>convert/misc</tt> addon package:

```j
   require 'convert/misc/md5'
   md5 'The quick brown fox jumped over the lazy dog''s back'
e38ca1d920c4b8b8d3946b2c72f01680
```


An alternative and faster approach is to use the Qt library function available using the <tt>ide/qt</tt> addon from J8:

```j
   require '~addons/ide/qt/qt.ijs'
   getmd5=: 'md5'&gethash_jqtide_
   getmd5 'The quick brown fox jumped over the lazy dog''s back'
e38ca1d920c4b8b8d3946b2c72f01680
```



## Java


```java
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class Digester {

    public static void main(String[] args) {
        System.out.println(hexDigest("Rosetta code", "MD5"));
    }

    static String hexDigest(String str, String digestName) {
        try {
            MessageDigest md = MessageDigest.getInstance(digestName);
            byte[] digest = md.digest(str.getBytes(StandardCharsets.UTF_8));
            char[] hex = new char[digest.length * 2];
            for (int i = 0; i < digest.length; i++) {
                hex[2 * i] = "0123456789abcdef".charAt((digest[i] & 0xf0) >> 4);
                hex[2 * i + 1] = "0123456789abcdef".charAt(digest[i] & 0x0f);
            }
            return new String(hex);
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        }
    }
}
```


Other options for digest algorithms (to replace "MD5" in the example above) include: MD2, SHA-1, SHA-256, SHA-384, and SHA-512.
Other encoding options (to replace "UTF_8" in the example above) are available from the Charset and StandardCharsets classes.


## Jsish


```javascript
/* MD5 hash in Jsish */
var str = 'Rosetta code';
puts(Util.hash(str, {type:'md5'}));

/* MD5 RFC1321 test suite */
function MD5(str) { return Util.hash(str, {type:'md5'}); }

;MD5('');
;MD5('a');
;MD5('abc');
;MD5('message digest');
;MD5('abcdefghijklmnopqrstuvwxyz');
;MD5('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789');
;MD5('12345678901234567890123456789012345678901234567890123456789012345678901234567890');

/*
=!EXPECTSTART!=
bf0ac9c7e94e9d50c18f4ff592643546
MD5('') ==> d41d8cd98f00b204e9800998ecf8427e
MD5('a') ==> 0cc175b9c0f1b6a831c399e269772661
MD5('abc') ==> 900150983cd24fb0d6963f7d28e17f72
MD5('message digest') ==> f96b697d7cb7938d525a2f31aaf161d0
MD5('abcdefghijklmnopqrstuvwxyz') ==> c3fcd3d76192e4007dfb496cca67e13b
MD5('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789') ==> d174ab98d277d9f5a5611c2c9f419d9f
MD5('12345678901234567890123456789012345678901234567890123456789012345678901234567890') ==> 57edf4a22be3c955ac49da2e2107b67a
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish md5.jsi
bf0ac9c7e94e9d50c18f4ff592643546

prompt$ jsish --U md5.jsi               # display unit test echos
bf0ac9c7e94e9d50c18f4ff592643546
MD5('') ==> d41d8cd98f00b204e9800998ecf8427e
MD5('a') ==> 0cc175b9c0f1b6a831c399e269772661
MD5('abc') ==> 900150983cd24fb0d6963f7d28e17f72
MD5('message digest') ==> f96b697d7cb7938d525a2f31aaf161d0
MD5('abcdefghijklmnopqrstuvwxyz') ==> c3fcd3d76192e4007dfb496cca67e13b
MD5('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789') ==> d174ab98d277d9f5a5611c2c9f419d9f
MD5('12345678901234567890123456789012345678901234567890123456789012345678901234567890') ==> 57edf4a22be3c955ac49da2e2107b67a

prompt$ jsish -u md5.jsi               # run the unit tests
[PASS] md5.jsi
```



## Julia

{{works with|Julia|0.6}}


```julia
using Nettle

function Base.trunc(s::AbstractString, n::Integer)
    n > 0 || throw(DomainError())
    l = length(s)
    l > n || return s
    n > 3 || return s[1:n]
    return s[1:n-3] * "..."
end

tests = [""    => "d41d8cd98f00b204e9800998ecf8427e",
         "a"   => "0cc175b9c0f1b6a831c399e269772661",
         "abc" => "900150983cd24fb0d6963f7d28e17f72",
         "message digest" => "f96b697d7cb7938d525a2f31aaf161d0",
         "abcdefghijklmnopqrstuvwxyz" => "c3fcd3d76192e4007dfb496cca67e13b",
         "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" =>
         "d174ab98d277d9f5a5611c2c9f419d9f",
         "12345678901234567890123456789012345678901234567890123456789012345678901234567890" =>
         "57edf4a22be3c955ac49da2e2107b67a",
         "foobad" => "3858f62230ac3c915f300c664312c63f"]

println("Testing Julia's MD5 hash against RFC 1321.")
for (k, h) in sort(tests, by = length ∘ first)
    md5sum = hexdigest("md5", k)
    @printf("%20s → %s ", trunc(k, 15), md5sum)
    if md5sum == h
        println("MD5 OK")
    else
        println("MD5 Bad")
        println("* The sum should be  ", h)
    end
end
```


{{out}}

```txt
                     → d41d8cd98f00b204e9800998ecf8427e MD5 OK
                   a → 0cc175b9c0f1b6a831c399e269772661 MD5 OK
                 abc → 900150983cd24fb0d6963f7d28e17f72 MD5 OK
              foobad → 6ce0d31e08fc3c4de8e3b2fa0d3d72ff MD5 Bad
* The sum should be  3858f62230ac3c915f300c664312c63f
      message digest → f96b697d7cb7938d525a2f31aaf161d0 MD5 OK
     abcdefghijkl... → c3fcd3d76192e4007dfb496cca67e13b MD5 OK
     ABCDEFGHIJKL... → d174ab98d277d9f5a5611c2c9f419d9f MD5 OK
     123456789012... → 57edf4a22be3c955ac49da2e2107b67a MD5 O
```



## Kotlin


```scala
// version 1.0.6

import java.security.MessageDigest

fun main(args: Array<String>) {
    val text  = "The quick brown fox jumped over the lazy dog's back"
    val bytes = text.toByteArray()
    val md = MessageDigest.getInstance("MD5")
    val digest = md.digest(bytes)
    for (byte in digest) print("%02x".format(byte))
    println()
}
```


{{out}}

```txt

e38ca1d920c4b8b8d3946b2c72f01680

```



## Lasso


```Lasso
Encrypt_MD5('Welcome all Rhinos!')
//80ba88ee2600e9e9b36e739458c39ebd
```



### Test suite


```Lasso
local(test = map(
		'a' = '0cc175b9c0f1b6a831c399e269772661',
		'abc' = '900150983cd24fb0d6963f7d28e17f72',
		'message digest' = 'f96b697d7cb7938d525a2f31aaf161d0',
		'abcdefghijklmnopqrstuvwxyz' = 'c3fcd3d76192e4007dfb496cca67e13b',
		'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789' = 'd174ab98d277d9f5a5611c2c9f419d9f',
		'12345678901234567890123456789012345678901234567890123456789012345678901234567890' = '57edf4a22be3c955ac49da2e2107b67a'
	)
)
with a in #test->keys do =>  {^
	'testing: "'+#a+'": '+(Encrypt_MD5(#a)->asBytes == #test->find(#a)->asBytes)+'\r'
^}
```

{{out}}

```txt
testing: "12345678901234567890123456789012345678901234567890123456789012345678901234567890": true
testing: "a": true
testing: "abc": true
testing: "abcdefghijklmnopqrstuvwxyz": true
testing: "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789": true
testing: "message digest": true
```



## Liberty BASIC


```lb
'[RC]MD5
    'from tsh73's January 2008 code

    text$="The quick brown fox jumps over the lazy dog"
    checkSum$="9e107d9d372bb6826bd81d3542a419d6"

    print text$
    print checkSum$

    test$=md5$(text$)

    if test$=checkSum$ then
        print "passed"
        print test$
    else
        print "failed"
    end if
    end

    function md5$(text$)

        dim r(64)
        dim k(64)
        dim w(16)
        global two32
        two32=2^32

        'prepare the MD5 checksum table
        restore [perRoundShiftAmounts]
        for i=0 to 63
            read x
            r(i)=x
        next

        'prepare constants
        for i=0 to 63
            k(i) =  int(abs(sin(i+1)) * two32)
        next

        'initialise variables
        h0 = HEXDEC("67452301")
        h1 = HEXDEC("EFCDAB89")
        h2 = HEXDEC("98BADCFE")
        h3 = HEXDEC("10325476")

        'find num bits in message
        numbits=len(text$)*8

        'add bits "10000000"
        text$=text$+chr$(128)

        'add bits "00000000"
        while len(text$) mod 64 <> 56
            text$=text$+chr$(0)
        wend

        'add original length in bits
        text$=text$+dec2asc$(numbits)

        'MD5 rounds
        'process in 64 byte chunks 512bits
        for chunk = 1 to len(text$) step 64
            chunk$ = mid$(text$, chunk, 64)
            for word = 0 TO 15
                'invert byte order
                b0 = asc(mid$(chunk$, word*4+1, 1))
                b1 = asc(mid$(chunk$, word*4+2, 1))
                b2 = asc(mid$(chunk$, word*4+3, 1))
                b3 = asc(mid$(chunk$, word*4+4, 1))
                w(word) = ((b3*256+b2)*256+b1)*256+b0
            next word
            a = h0
            b = h1
            c = h2
            d = h3
            for i = 0 to 63
                select case
                    case 0 <= i and i <= 15
                        f = (b and c) or (bitNot(b) and d)
                        g = i
                    case 16 <= i and i <= 31
                        f = (d and b) or (bitNot(d) and c)
                        g = (5 * i + 1) mod 16
                    case 32 <= i and i <= 47
                        f = b xor c xor d
                        g = (3 * i + 5) mod 16
                    case 48 <= i and i <= 63
                        f = c xor (b or bitNot(d))
                        g = (7 * i) mod 16
                end select
                temp = d
                d = c
                c = b
                b=b+leftrotate(a + f + k(i) + w(g),r(i))
                b = b mod two32
                a = temp
            next i
            h0 = (h0 + a) mod two32
            h1 = (h1 + b) mod two32
            h2 = (h2 + c) mod two32
            h3 = (h3 + d) mod two32
        next chunk

        md5$ =  revOrd$(DECHEX$(h0))+_
                revOrd$(DECHEX$(h1))+_
                revOrd$(DECHEX$(h2))+_
                revOrd$(DECHEX$(h3))

        [perRoundShiftAmounts]
        DATA 7,12,17,22, 7,12,17,22, 7,12,17,22, 7,12,17,22
        DATA 5, 9,14,20, 5, 9,14,20, 5, 9,14,20, 5, 9,14,20
        DATA 4,11,16,23, 4,11,16,23, 4,11,16,23, 4,11,16,23
        DATA 6,10,15,21, 6,10,15,21, 6,10,15,21, 6,10,15,21

    end function


    ' dec2asc: dec to 8 byte asc
    function dec2asc$(n)
        h$ = ""
        for i = 1 to 8
            h$ = h$ + chr$(n mod 256)
            n = int(n/256)
        next
        dec2asc$= h$
    end function

    ' bitNot
    function bitNot(num)
        bitNot = two32 -1 -num
    end function

    ' leftrotate: spins bits left n times
    function leftrotate(num,times)
        num=num mod two32
        r = (num*2^times) mod two32
        l = int(num/(2^(32-times)))
        leftrotate = r+l
    end function

    ' reverse the HEXDEC$ order
    function revOrd$(a$)
        a$=left$("00000000", 8-len(a$))+a$
        revOrd$ = lower$(mid$(a$,7,2)+mid$(a$,5,2)+mid$(a$,3,2)+mid$(a$,1,2))
    end function
```



## Lingo


*Using a binary plugin ("Xtra"):
{{libheader|Crypto Xtra}}

```lingo
put cx_md5_string(str)
```


*Pure Lingo implementation

```lingo
----------------------------------------
-- Calculates MD5 hash of string or bytearray
-- @param {bytearray|string} input
-- @return {bytearray} (16 bytes)
----------------------------------------
on md5 (input)
  if stringP(input) then input = bytearray(input)

  -- Convert string to list of little-endian words...
  t_iLen = input.length * 8
  t_iCnt = (t_iLen + 64) / 512 * 16 + 16

  -- Create list, fill with zeros...
  x = []
  x[t_iCnt] = 0

  t_fArr = [1, 256, 65536, 16777216]
  i = 0
  j = 0
  repeat while i < t_iLen
    j = j + 1
    t_iNext = i / 32 + 1
    t_iTemp = bitAnd(input[i/8+1], 255) * t_fArr[j]
    x[t_iNext] = bitOr(x[t_iNext], t_iTemp)
    i = i + 8
    j = j mod 4
  end repeat

  -- Append padding...
  t_iNext = t_iLen / 32 + 1
  x[t_iNext] = bitOr(x[t_iNext], 128 * t_fArr[j + 1])
  x[(t_iLen + 64) / 512 * 16 + 15] = t_iLen

  -- Actual algorithm starts here...
  a =  1732584193
  b = -271733879
  c = -1732584194
  d =  271733878
  i =  1
  t_iWrap = the maxInteger + 1
  t_iCount = x.count + 1

  repeat while i < t_iCount
    olda = a
    oldb = b
    oldc = c
    oldd = d

    -- Round(1) --
    n = bitOr(bitAnd(b, c), bitAnd(bitNot(b), d)) + a + x[i] - 680876936
    if(n < 0) then a = bitOr(n * 128, bitOr((n + t_iWrap) / 33554432, 64)) + b
    else a = bitOr(n * 128, n / 33554432) + b
    n = bitOr(bitAnd(a, b), bitAnd(bitNot(a), c)) + d + x[i + 1] - 389564586
    if(n < 0) then d = bitOr(n * 4096, bitOr((n + t_iWrap) / 1048576, 2048)) + a
    else d = bitOr(n * 4096, n / 1048576) + a
    n = bitOr(bitAnd(d, a), bitAnd(bitNot(d), b)) + c + x[i + 2] + 606105819
    if(n < 0) then c = bitOr(n * 131072, bitOr((n + t_iWrap) / 32768, 65536)) + d
    else c = bitOr(n * 131072, n / 32768) + d
    n = bitOr(bitAnd(c, d), bitAnd(bitNot(c), a)) + b + x[i + 3] - 1044525330
    if(n < 0) then b = bitOr(n * 4194304, bitOr((n + t_iWrap) / 1024, 2097152)) + c
    else b = bitOr(n * 4194304, n / 1024) + c
    n = bitOr(bitAnd(b, c), bitAnd(bitNot(b), d)) + a + x[i + 4] - 176418897
    if(n < 0) then a = bitOr(n * 128, bitOr((n + t_iWrap) / 33554432, 64)) + b
    else a = bitOr(n * 128, n / 33554432) + b
    n = bitOr(bitAnd(a, b), bitAnd(bitNot(a), c)) + d + x[i + 5] + 1200080426
    if(n < 0) then d = bitOr(n * 4096, bitOr((n + t_iWrap) / 1048576, 2048)) + a
    else d = bitOr(n * 4096, n / 1048576) + a
    n = bitOr(bitAnd(d, a), bitAnd(bitNot(d), b)) + c + x[i + 6] - 1473231341
    if(n < 0) then c = bitOr(n * 131072, bitOr((n + t_iWrap) / 32768, 65536)) + d
    else c = bitOr(n * 131072, n / 32768) + d
    n = bitOr(bitAnd(c, d), bitAnd(bitNot(c), a)) + b + x[i + 7] - 45705983
    if(n < 0) then b = bitOr(n * 4194304, bitOr((n + t_iWrap) / 1024, 2097152)) + c
    else b = bitOr(n * 4194304, n / 1024) + c
    n = bitOr(bitAnd(b, c), bitAnd(bitNot(b), d)) + a + x[i + 8] + 1770035416
    if(n < 0) then a = bitOr(n * 128, bitOr((n + t_iWrap) / 33554432, 64)) + b
    else a = bitOr(n * 128, n / 33554432) + b
    n = bitOr(bitAnd(a, b), bitAnd(bitNot(a), c)) + d + x[i + 9] - 1958414417
    if(n < 0) then d = bitOr(n * 4096, bitOr((n + t_iWrap) / 1048576, 2048)) + a
    else d = bitOr(n * 4096, n / 1048576) + a
    n = bitOr(bitAnd(d, a), bitAnd(bitNot(d), b)) + c + x[i + 10] - 42063
    if(n < 0) then c = bitOr(n * 131072, bitOr((n + t_iWrap) / 32768, 65536)) + d
    else c = bitOr(n * 131072, n / 32768) + d
    n = bitOr(bitAnd(c, d), bitAnd(bitNot(c), a)) + b + x[i + 11] - 1990404162
    if(n < 0) then b = bitOr(n * 4194304, bitOr((n + t_iWrap) / 1024, 2097152)) + c
    else b = bitOr(n * 4194304, n / 1024) + c
    n = bitOr(bitAnd(b, c), bitAnd(bitNot(b), d)) + a + x[i + 12] + 1804603682
    if(n < 0) then a = bitOr(n * 128, bitOr((n + t_iWrap) / 33554432, 64)) + b
    else a = bitOr(n * 128, n / 33554432) + b
    n = bitOr(bitAnd(a, b), bitAnd(bitNot(a), c)) + d + x[i + 13] - 40341101
    if(n < 0) then d = bitOr(n * 4096, bitOr((n + t_iWrap) / 1048576, 2048)) + a
    else d = bitOr(n * 4096, n / 1048576) + a
    n = bitOr(bitAnd(d, a), bitAnd(bitNot(d), b)) + c + x[i + 14] - 1502002290
    if(n < 0) then c = bitOr(n * 131072, bitOr((n + t_iWrap) / 32768, 65536)) + d
    else c = bitOr(n * 131072, n / 32768) + d
    n = bitOr(bitAnd(c, d), bitAnd(bitNot(c), a)) + b + x[i + 15] + 1236535329
    if(n < 0) then b = bitOr(n * 4194304, bitOr((n + t_iWrap) / 1024, 2097152)) + c
    else b = bitOr(n * 4194304, n / 1024) + c

    -- Round(2) --
    n = bitOr(bitAnd(b, d), bitAnd(c, bitNot(d))) + a + x[i + 1] - 165796510
    if(n < 0) then a = bitOr(n * 32, bitOr((n + t_iWrap) / 134217728, 16)) + b
    else a = bitOr(n * 32, n / 134217728) + b
    n = bitOr(bitAnd(a, c), bitAnd(b, bitNot(c))) + d + x[i + 6] - 1069501632
    if(n < 0) then d = bitOr(n * 512, bitOr((n + t_iWrap) / 8388608, 256)) + a
    else d = bitOr(n * 512, n / 8388608) + a
    n = bitOr(bitAnd(d, b), bitAnd(a, bitNot(b))) + c + x[i + 11] + 643717713
    if(n < 0) then c = bitOr(n * 16384, bitOr((n + t_iWrap) / 262144, 8192)) + d
    else c = bitOr(n * 16384, n / 262144) + d
    n = bitOr(bitAnd(c, a), bitAnd(d, bitNot(a))) + b + x[i] - 373897302
    if(n < 0) then b = bitOr(n * 1048576, bitOr((n + t_iWrap) / 4096, 524288)) + c
    else b = bitOr(n * 1048576, n / 4096) + c
    n = bitOr(bitAnd(b, d), bitAnd(c, bitNot(d))) + a + x[i + 5] - 701558691
    if(n < 0) then a = bitOr(n * 32, bitOr((n + t_iWrap) / 134217728, 16)) + b
    else a = bitOr(n * 32, n / 134217728) + b
    n = bitOr(bitAnd(a, c), bitAnd(b, bitNot(c))) + d + x[i + 10] + 38016083
    if(n < 0) then d = bitOr(n * 512, bitOr((n + t_iWrap) / 8388608, 256)) + a
    else d = bitOr(n * 512, n / 8388608) + a
    n = bitOr(bitAnd(d, b), bitAnd(a, bitNot(b))) + c + x[i + 15] - 660478335
    if(n < 0) then c = bitOr(n * 16384, bitOr((n + t_iWrap) / 262144, 8192)) + d
    else c = bitOr(n * 16384, n / 262144) + d
    n = bitOr(bitAnd(c, a), bitAnd(d, bitNot(a))) + b + x[i + 4] - 405537848
    if(n < 0) then b = bitOr(n * 1048576, bitOr((n + t_iWrap) / 4096, 524288)) + c
    else b = bitOr(n * 1048576, n / 4096) + c
    n = bitOr(bitAnd(b, d), bitAnd(c, bitNot(d))) + a + x[i + 9] + 568446438
    if(n < 0) then a = bitOr(n * 32, bitOr((n + t_iWrap) / 134217728, 16)) + b
    else a = bitOr(n * 32, n / 134217728) + b
    n = bitOr(bitAnd(a, c), bitAnd(b, bitNot(c))) + d + x[i + 14] - 1019803690
    if(n < 0) then d = bitOr(n * 512, bitOr((n + t_iWrap) / 8388608, 256)) + a
    else d = bitOr(n * 512, n / 8388608) + a
    n = bitOr(bitAnd(d, b), bitAnd(a, bitNot(b))) + c + x[i + 3] - 187363961
    if(n < 0) then c = bitOr(n * 16384, bitOr((n + t_iWrap) / 262144, 8192)) + d
    else c = bitOr(n * 16384, n / 262144) + d
    n = bitOr(bitAnd(c, a), bitAnd(d, bitNot(a))) + b + x[i + 8] + 1163531501
    if(n < 0) then b = bitOr(n * 1048576, bitOr((n + t_iWrap) / 4096, 524288)) + c
    else b = bitOr(n * 1048576, n / 4096) + c
    n = bitOr(bitAnd(b, d), bitAnd(c, bitNot(d))) + a + x[i + 13] - 1444681467
    if(n < 0) then a = bitOr(n * 32, bitOr((n + t_iWrap) / 134217728, 16)) + b
    else a = bitOr(n * 32, n / 134217728) + b
    n = bitOr(bitAnd(a, c), bitAnd(b, bitNot(c))) + d + x[i + 2] - 51403784
    if(n < 0) then d = bitOr(n * 512, bitOr((n + t_iWrap) / 8388608, 256)) + a
    else d = bitOr(n * 512, n / 8388608) + a
    n = bitOr(bitAnd(d, b), bitAnd(a, bitNot(b))) + c + x[i + 7] + 1735328473
    if(n < 0) then c = bitOr(n * 16384, bitOr((n + t_iWrap) / 262144, 8192)) + d
    else c = bitOr(n * 16384, n / 262144) + d
    n = bitOr(bitAnd(c, a), bitAnd(d, bitNot(a))) + b + x[i + 12] - 1926607734
    if(n < 0) then b = bitOr(n * 1048576, bitOr((n + t_iWrap) / 4096, 524288)) + c
    else b = bitOr(n * 1048576, n / 4096) + c

    -- Round(3) --
    n = bitXor(bitXor(b, c), d) + a + x[i + 5] - 378558
    if(n < 0) then a = bitOr(n * 16, bitOr((n + t_iWrap) / 268435456, 8)) + b
    else a = bitOr(n * 16, n / 268435456) + b
    n = bitXor(bitXor(a, b), c) + d + x[i + 8] - 2022574463
    if(n < 0) then d = bitOr(n * 2048, bitOr((n + t_iWrap) / 2097152, 1024)) + a
    else d = bitOr(n * 2048, n / 2097152) + a
    n = bitXor(bitXor(d, a), b) + c + x[i + 11] + 1839030562
    if(n < 0) then c = bitOr(n * 65536, bitOr((n + t_iWrap) / 65536, 32768)) + d
    else c = bitOr(n * 65536, n / 65536) + d
    n = bitXor(bitXor(c, d), a) + b + x[i + 14] - 35309556
    if(n < 0) then b = bitOr(n * 8388608, bitOr((n + t_iWrap) / 512, 4194304)) + c
    else b = bitOr(n * 8388608, n / 512) + c
    n = bitXor(bitXor(b, c), d) + a + x[i + 1] - 1530992060
    if(n < 0) then a = bitOr(n * 16, bitOr((n + t_iWrap) / 268435456, 8)) + b
    else a = bitOr(n * 16, n / 268435456) + b
    n = bitXor(bitXor(a, b), c) + d + x[i + 4] + 1272893353
    if(n < 0) then d = bitOr(n * 2048, bitOr((n + t_iWrap) / 2097152, 1024)) + a
    else d = bitOr(n * 2048, n / 2097152) + a
    n = bitXor(bitXor(d, a), b) + c + x[i + 7] - 155497632
    if(n < 0) then c = bitOr(n * 65536, bitOr((n + t_iWrap) / 65536, 32768)) + d
    else c = bitOr(n * 65536, n / 65536) + d
    n = bitXor(bitXor(c, d), a) + b + x[i + 10] - 1094730640
    if(n < 0) then b = bitOr(n * 8388608, bitOr((n + t_iWrap) / 512, 4194304)) + c
    else b = bitOr(n * 8388608, n / 512) + c
    n = bitXor(bitXor(b, c), d) + a + x[i + 13] + 681279174
    if(n < 0) then a = bitOr(n * 16, bitOr((n + t_iWrap) / 268435456, 8)) + b
    else a = bitOr(n * 16, n / 268435456) + b
    n = bitXor(bitXor(a, b), c) + d + x[i] - 358537222
    if(n < 0) then d = bitOr(n * 2048, bitOr((n + t_iWrap) / 2097152, 1024)) + a
    else d = bitOr(n * 2048, n / 2097152) + a
    n = bitXor(bitXor(d, a), b) + c + x[i + 3] - 722521979
    if(n < 0) then c = bitOr(n * 65536, bitOr((n + t_iWrap) / 65536, 32768)) + d
    else c = bitOr(n * 65536, n / 65536) + d
    n = bitXor(bitXor(c, d), a) + b + x[i + 6] + 76029189
    if(n < 0) then b = bitOr(n * 8388608, bitOr((n + t_iWrap) / 512, 4194304)) + c
    else b = bitOr(n * 8388608, n / 512) + c
    n = bitXor(bitXor(b, c), d) + a + x[i + 9] - 640364487
    if(n < 0) then a = bitOr(n * 16, bitOr((n + t_iWrap) / 268435456, 8)) + b
    else a = bitOr(n * 16, n / 268435456) + b
    n = bitXor(bitXor(a, b), c) + d + x[i + 12] - 421815835
    if(n < 0) then d = bitOr(n * 2048, bitOr((n + t_iWrap) / 2097152, 1024)) + a
    else d = bitOr(n * 2048, n / 2097152) + a
    n = bitXor(bitXor(d, a), b) + c + x[i + 15] + 530742520
    if(n < 0) then c = bitOr(n * 65536, bitOr((n + t_iWrap) / 65536, 32768)) + d
    else c = bitOr(n * 65536, n / 65536) + d
    n = bitXor(bitXor(c, d), a) + b + x[i + 2] - 995338651
    if(n < 0) then b = bitOr(n * 8388608, bitOr((n + t_iWrap) / 512, 4194304)) + c
    else b = bitOr(n * 8388608, n / 512) + c

    -- Round(4) --
    n = bitXor(c, bitOr(b, bitNot(d))) + a + x[i] - 198630844
    if(n < 0) then a = bitOr(n * 64, bitOr((n + t_iWrap) / 67108864, 32)) + b
    else a = bitOr(n * 64, n / 67108864) + b
    n = bitXor(b, bitOr(a, bitNot(c))) + d + x[i + 7] + 1126891415
    if(n < 0) then d = bitOr(n * 1024, bitOr((n + t_iWrap) / 4194304, 512)) + a
    else d = bitOr(n * 1024, n / 4194304) + a
    n = bitXor(a, bitOr(d, bitNot(b))) + c + x[i + 14] - 1416354905
    if(n < 0) then c = bitOr(n * 32768, bitOr((n + t_iWrap) / 131072, 16384)) + d
    else c = bitOr(n * 32768, n / 131072) + d
    n = bitXor(d, bitOr(c, bitNot(a))) + b + x[i + 5] - 57434055
    if(n < 0) then b = bitOr(n * 2097152, bitOr((n + t_iWrap) / 2048, 1048576)) + c
    else b = bitOr(n * 2097152, n / 2048) + c
    n = bitXor(c, bitOr(b, bitNot(d))) + a + x[i + 12] + 1700485571
    if(n < 0) then a = bitOr(n * 64, bitOr((n + t_iWrap) / 67108864, 32)) + b
    else a = bitOr(n * 64, n / 67108864) + b
    n = bitXor(b, bitOr(a, bitNot(c))) + d + x[i + 3] - 1894986606
    if(n < 0) then d = bitOr(n * 1024, bitOr((n + t_iWrap) / 4194304, 512)) + a
    else d = bitOr(n * 1024, n / 4194304) + a
    n = bitXor(a, bitOr(d, bitNot(b))) + c + x[i + 10] - 1051523
    if(n < 0) then c = bitOr(n * 32768, bitOr((n + t_iWrap) / 131072, 16384)) + d
    else c = bitOr(n * 32768, n / 131072) + d
    n = bitXor(d, bitOr(c, bitNot(a))) + b + x[i + 1] - 2054922799
    if(n < 0) then b = bitOr(n * 2097152, bitOr((n + t_iWrap) / 2048, 1048576)) + c
    else b = bitOr(n * 2097152, n / 2048) + c
    n = bitXor(c, bitOr(b, bitNot(d))) + a + x[i + 8] + 1873313359
    if(n < 0) then a = bitOr(n * 64, bitOr((n + t_iWrap) / 67108864, 32)) + b
    else a = bitOr(n * 64, n / 67108864) + b
    n = bitXor(b, bitOr(a, bitNot(c))) + d + x[i + 15] - 30611744
    if(n < 0) then d = bitOr(n * 1024, bitOr((n + t_iWrap) / 4194304, 512)) + a
    else d = bitOr(n * 1024, n / 4194304) + a
    n = bitXor(a, bitOr(d, bitNot(b))) + c + x[i + 6] - 1560198380
    if(n < 0) then c = bitOr(n * 32768, bitOr((n + t_iWrap) / 131072, 16384)) + d
    else c = bitOr(n * 32768, n / 131072) + d
    n = bitXor(d, bitOr(c, bitNot(a))) + b + x[i + 13] + 1309151649
    if(n < 0) then b = bitOr(n * 2097152, bitOr((n + t_iWrap) / 2048, 1048576)) + c
    else b = bitOr(n * 2097152, n / 2048) + c
    n = bitXor(c, bitOr(b, bitNot(d))) + a + x[i + 4] - 145523070
    if(n < 0) then a = bitOr(n * 64, bitOr((n + t_iWrap) / 67108864, 32)) + b
    else a = bitOr(n * 64, n / 67108864) + b
    n = bitXor(b, bitOr(a, bitNot(c))) + d + x[i + 11] - 1120210379
    if(n < 0) then d = bitOr(n * 1024, bitOr((n + t_iWrap) / 4194304, 512)) + a
    else d = bitOr(n * 1024, n / 4194304) + a
    n = bitXor(a, bitOr(d, bitNot(b))) + c + x[i + 2] + 718787259
    if(n < 0) then c = bitOr(n * 32768, bitOr((n + t_iWrap) / 131072, 16384)) + d
    else c = bitOr(n * 32768, n / 131072) + d
    n = bitXor(d, bitOr(c, bitNot(a))) + b + x[i + 9] - 343485551
    if(n < 0) then b = bitOr(n * 2097152, bitOr((n + t_iWrap) / 2048, 1048576)) + c
    else b = bitOr(n * 2097152, n / 2048) + c
    a = a + olda
    b = b + oldb
    c = c + oldc
    d = d + oldd
    i = i + 16
  end repeat

  t_iArr = [a, b, c, d]
  ba = bytearray()
  p = 1
  repeat with i in t_iArr
    if(i > 0) then
      repeat with n = 1 to 4
        ba[p] = (i mod 256)
        i = i / 256
        p = p+1
      end repeat
    else
      i = bitNot(i)
      repeat with n = 1 to 4
        ba[p] = 255-(i mod 256)
        i = i / 256
        p = p+1
      end repeat
    end if
  end repeat
  ba.position = 1
  return ba
end
```



## LiveCode

Livecode has built-in support for md4 and md5

```LiveCode
function md5sum hashtext
    local md5, mdhex
    put md5Digest(hashtext) into md5
    get binaryDecode("H*",md5,mdhex)
    return mdhex
end md5sum
```


Tests
```LiveCode
command md5testsuite
    //    rfc1321 MD5 test suite:
    local md5
    put md5sum("") is "d41d8cd98f00b204e9800998ecf8427e" into md5["empty"]
    put md5sum("a") is "0cc175b9c0f1b6a831c399e269772661" into md5["a"]
    put md5sum("abc") is "900150983cd24fb0d6963f7d28e17f72" into md5["abc"]
    put md5sum("message digest") is "f96b697d7cb7938d525a2f31aaf161d0" into md5["message digest"]
    put md5sum("abcdefghijklmnopqrstuvwxyz") is "c3fcd3d76192e4007dfb496cca67e13b" \
          into md5["abclower"]
    put md5sum("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") \
          is "d174ab98d277d9f5a5611c2c9f419d9f" into md5["abcupper"]
    put md5sum("12345678901234567890123456789012345678901234567890123456789012345678901234567890")\
    is "57edf4a22be3c955ac49da2e2107b67a" into md5["123"]

    repeat for each line n in the keys of md5
        if md5[n] is not true then
            put "err" & tab & n & return after results
            exit repeat
        else
            put "ok" & tab & n & return after results
        end if
    end repeat
    put results
end md5testsuite
```

Output

```txt
ok	abc
ok	abclower
ok	123
ok	abcupper
ok	message digest
ok	a
ok	empty
```



## Lua

Using the [http://www.keplerproject.org/md5/ Kepler MD5 library]:

```Lua
require "md5"

--printing a sum:
print(md5.sumhexa"The quick brown fox jumps over the lazy dog")

--running the test suite:

local function test(msg,sum) assert(md5.sumhexa(msg)==sum) end

test("","d41d8cd98f00b204e9800998ecf8427e")
test("a","0cc175b9c0f1b6a831c399e269772661")
test("abc","900150983cd24fb0d6963f7d28e17f72")
test("message digest","f96b697d7cb7938d525a2f31aaf161d0")
test("abcdefghijklmnopqrstuvwxyz","c3fcd3d76192e4007dfb496cca67e13b")
test("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789","d174ab98d277d9f5a5611c2c9f419d9f")
test("12345678901234567890123456789012345678901234567890123456789012345678901234567890","57edf4a22be3c955ac49da2e2107b67a")
```



## Maple

The <tt>Hash</tt> command in the <tt>StringTools</tt> package computes the MD5 hash value of a string.

```Maple

> with( StringTools ):
> Hash( "" );
         "d41d8cd98f00b204e9800998ecf8427e"

> Hash( "a" );
         "0cc175b9c0f1b6a831c399e269772661"

> Hash( "abc" );
         "900150983cd24fb0d6963f7d28e17f72"

> Hash( "message digest" );
         "f96b697d7cb7938d525a2f31aaf161d0"

> Hash( "abcdefghijklmnopqrstuvwxyz" );
         "c3fcd3d76192e4007dfb496cca67e13b"

> Hash( "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" );
         "d174ab98d277d9f5a5611c2c9f419d9f"

> Hash( "12345678901234567890123456789012345678901234567890123456789012345678901234567890" );
         "57edf4a22be3c955ac49da2e2107b67a"

```



## Mathematica

Mathematica has built-in functions Hash and FileHash.
Example:

```Mathematica
 Hash["The quick brown fox jumped over the lazy dog's back","MD5","HexString"]
```

gives back:

```Mathematica> e38ca1d920c4b8b8d3946b2c72f01680</lang



## MATLAB

This code also works with Octave (but Octave already provides md5sum(), see [[{{FULLPAGENAME}}#Octave|Octave example]]).


```MATLAB
function digest = md5(message)
    % digest = md5(message)
    %  Compute the MD5 digest of the message, as a hexadecimal digest.

    % Follow the MD5 algorithm from RFC 1321 [1] and Wikipedia [2].
    %  [1] http://tools.ietf.org/html/rfc1321
    %  [2] http://en.wikipedia.org/wiki/MD5

    % m is the modulus for 32-bit unsigned arithmetic.
    m = 2 ^ 32;

    % s is the shift table for circshift(). Each shift is negative
    % because it is a left shift.
    s = [-7, -12, -17, -22
         -5,  -9, -14, -20
         -4, -11, -16, -23
         -6, -10, -15, -21];

    % t is the sine table. Each sine is a 32-bit integer, unsigned.
    t = floor(abs(sin(1:64)) .* m);

    % Initialize the hash, as a row vector of 32-bit integers.
    digest = [hex2dec('67452301') ...
              hex2dec('EFCDAB89') ...
              hex2dec('98BADCFE') ...
              hex2dec('10325476')];

    % If message contains characters, convert them to ASCII values.
    message = double(message);
    bytelen = numel(message);

    % Pad the message by appending a 1, then appending enough 0s to make
    % the bit length congruent to 448 mod 512. Because we have bytes, we
    % append 128 '10000000', then append enough 0s '00000000's to make
    % the byte length congruent to 56 mod 64.
    message = [message, 128, zeros(1, mod(55 - bytelen, 64))];

    % Convert the message to 32-bit integers, little endian.
    % For little endian, first byte is least significant byte.
    message = reshape(message, 4, numel(message) / 4);
    message = message(1,:) + ...            % least significant byte
              message(2,:) * 256 + ...
              message(3,:) * 65536 + ...
              message(4,:) * 16777216;      % most significant byte

    % Append the bit length as a 64-bit integer, little endian.
    bitlen = bytelen * 8;
    message = [message, mod(bitlen, m), mod(bitlen / m, m)];

    % Process each 512-bit block. Because we have 32-bit integers, each
    % block has 16 elements, message(k + (0:15)).
    for k = 1:16:numel(message)
        % Copy hash.
        a = digest(1); b = digest(2); c = digest(3); d = digest(4);

        % Do 64 operations.
        for i = (1:64)
            % Convert b, c, d to row vectors of bits (0s and 1s).
            bv = dec2bin(b, 32) - '0';
            cv = dec2bin(c, 32) - '0';
            dv = dec2bin(d, 32) - '0';

            % Find f  = mix of b, c, d.
            %      ki = index in 0:15, to message(k + ki).
            %      sr = row in 1:4, to s(sr, :).
            if i <= 16          % Round 1
                f = (bv & cv) | (~bv & dv);
                ki = i - 1;
                sr = 1;
            elseif i <= 32      % Round 2
                f = (bv & dv) | (cv & ~dv);
                ki = mod(5 * i - 4, 16);
                sr = 2;
            elseif i <= 48      % Round 3
                f = xor(bv, xor(cv, dv));
                ki = mod(3 * i + 2, 16);
                sr = 3;
            else                % Round 4
                f = xor(cv, bv | ~dv);
                ki = mod(7 * i - 7, 16);
                sr = 4;
            end

            % Convert f, from row vector of bits, to 32-bit integer.
            f = bin2dec(char(f + '0'));

            % Do circular shift of sum.
            sc = mod(i - 1, 4) + 1;
            sum = mod(a + f + message(k + ki) + t(i), m);
            sum = dec2bin(sum, 32);
            sum = circshift(sum, [0, s(sr, sc)]);
            sum = bin2dec(sum);

            % Update a, b, c, d.
            temp = d;
            d = c;
            c = b;
            b = mod(b + sum, m);
            a = temp;
        end %for i

        % Add hash of this block to hash of previous blocks.
        digest = mod(digest + [a, b, c, d], m);
    end %for k

    % Convert hash from 32-bit integers, little endian, to bytes.
    digest = [digest                % least significant byte
              digest / 256
              digest / 65536
              digest / 16777216];   % most significant byte
    digest = reshape(mod(floor(digest), 256), 1, numel(digest));

    % Convert hash to hexadecimal.
    digest = dec2hex(digest);
    digest = reshape(transpose(digest), 1, numel(digest));
end %md5
```


Sample Usage:

```MATLAB>octave:14
 md5('Rosetta Code')
ans = CCA1BF66B09554E10F837838C3D3EFB1
```



## MOO


```moo
string = "The quick brown fox jumped over the lazy dog's back";
player:tell(string_hash(string));
```



## Neko


```ActionScript
/**
 MD5 in Neko
 Tectonics:
   nekoc md5.neko
   neko md5
*/

var MD5 = $loader.loadprim("std@make_md5", 1);
var base_encode = $loader.loadprim("std@base_encode", 2);

var result = MD5("The quick brown fox jumps over the lazy dog");

/* Output in lowercase hex */
$print(base_encode(result, "0123456789abcdef"));
```


{{out}}

```txt
prompt$ nekoc md5.neko
prompt$ neko md5.n
9e107d9d372bb6826bd81d3542a419d6
```



## Nemerle

{{trans|C#}}

```Nemerle
using System;
using System.Console;
using System.Text;
using System.Security.Cryptography;
using Nemerle.Collections;
using Nemerle.Collections.NCollectionsExtensions;

module Md5
{
    HashMD5(input : string) : string
    {
        BitConverter.ToString
            (MD5.Create().ComputeHash(Encoding.Default.GetBytes(input))).Replace("-", "").ToLower()
    }

    IsValidMD5(text : string, hash : string) : bool
    {
        HashMD5(text) == hash.ToLower()
    }

    Main() : void
    {
        def examples = ["The quick brown fox jumped over the lazy dog's back", "", "a", "abc", "message digest",
                        "abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                        "12345678901234567890123456789012345678901234567890123456789012345678901234567890"];
        def hashes = ["e38ca1d920c4b8b8d3946b2c72f01680", "d41d8cd98f00b204e9800998ecf8427e",
                      "0cc175b9c0f1b6a831c399e269772661", "900150983cd24fb0d6963f7d28e17f72",
                      "f96b697d7cb7938d525a2f31aaf161d0", "c3fcd3d76192e4007dfb496cca67e13b",
                      "d174ab98d277d9f5a5611c2c9f419d9f", "57edf4a22be3c955ac49da2e2107b67a"];
        def tests = Hashtable(ZipLazy(examples, hashes));
        foreach (test in tests)
            Write($"$(IsValidMD5(test.Key, test.Value))   ");
    }
}
```

Output:

```txt
True   True   True   True   True   True   True   True
```



## NetRexx


```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols binary

import java.security.MessageDigest

MD5('The quick brown fox jumps over the lazy dog', '9e107d9d372bb6826bd81d3542a419d6')
-- RFC 1321 MD5 test suite:
MD5("", 'd41d8cd98f00b204e9800998ecf8427e')
MD5("a", '0cc175b9c0f1b6a831c399e269772661')
MD5("abc", '900150983cd24fb0d6963f7d28e17f72')
MD5("message digest", 'f96b697d7cb7938d525a2f31aaf161d0')
MD5("abcdefghijklmnopqrstuvwxyz", 'c3fcd3d76192e4007dfb496cca67e13b')
MD5("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", 'd174ab98d277d9f5a5611c2c9f419d9f')
MD5("12345678901234567890123456789012345678901234567890123456789012345678901234567890", '57edf4a22be3c955ac49da2e2107b67a')

return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method MD5(messageText, verifyCheck) public static

  algorithm   = 'MD5'
  digestSum = getDigest(messageText, algorithm)

  say '<Message>'messageText'</Message>'
  say Rexx('<'algorithm'>').right(12) || digestSum'</'algorithm'>'
  say Rexx('<Verify>').right(12) || verifyCheck'</Verify>'
  if digestSum == verifyCheck then say algorithm 'Confirmed'
                              else say algorithm 'Failed'

  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method getDigest(messageText = Rexx, algorithm = Rexx 'MD5', encoding = Rexx 'UTF-8', lowercase = boolean 1) public static returns Rexx

  algorithm = algorithm.upper
  encoding  = encoding.upper

  message      = String(messageText)
  messageBytes = byte[]
  digestBytes  = byte[]
  digestSum    = Rexx ''

  do
    messageBytes = message.getBytes(encoding)
    md = MessageDigest.getInstance(algorithm)
    md.update(messageBytes)
    digestBytes = md.digest

    loop b_ = 0 to digestBytes.length - 1
      bb = Rexx(digestBytes[b_]).d2x(2)
      if lowercase then digestSum = digestSum || bb.lower
                   else digestSum = digestSum || bb.upper
      end b_
  catch ex = Exception
    ex.printStackTrace
  end

  return digestSum

```

'''Output:'''

```txt

<Message>The quick brown fox jumps over the lazy dog</Message>
       <MD5>9e107d9d372bb6826bd81d3542a419d6</MD5>
    <Verify>9e107d9d372bb6826bd81d3542a419d6</Verify>
MD5 Confirmed
<Message></Message>
       <MD5>d41d8cd98f00b204e9800998ecf8427e</MD5>
    <Verify>d41d8cd98f00b204e9800998ecf8427e</Verify>
MD5 Confirmed
<Message>a</Message>
       <MD5>0cc175b9c0f1b6a831c399e269772661</MD5>
    <Verify>0cc175b9c0f1b6a831c399e269772661</Verify>
MD5 Confirmed
<Message>abc</Message>
       <MD5>900150983cd24fb0d6963f7d28e17f72</MD5>
    <Verify>900150983cd24fb0d6963f7d28e17f72</Verify>
MD5 Confirmed
<Message>message digest</Message>
       <MD5>f96b697d7cb7938d525a2f31aaf161d0</MD5>
    <Verify>f96b697d7cb7938d525a2f31aaf161d0</Verify>
MD5 Confirmed
<Message>abcdefghijklmnopqrstuvwxyz</Message>
       <MD5>c3fcd3d76192e4007dfb496cca67e13b</MD5>
    <Verify>c3fcd3d76192e4007dfb496cca67e13b</Verify>
MD5 Confirmed
<Message>ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789</Message>
       <MD5>d174ab98d277d9f5a5611c2c9f419d9f</MD5>
    <Verify>d174ab98d277d9f5a5611c2c9f419d9f</Verify>
MD5 Confirmed
<Message>12345678901234567890123456789012345678901234567890123456789012345678901234567890</Message>
       <MD5>57edf4a22be3c955ac49da2e2107b67a</MD5>
    <Verify>57edf4a22be3c955ac49da2e2107b67a</Verify>
MD5 Confirmed

```



## NewLISP


```NewLISP
;; using the crypto module from http://www.newlisp.org/code/modules/crypto.lsp.html
;; (import native functions from the crypto library, provided by OpenSSL)
(module "crypto.lsp")
(crypto:md5 "The quick brown fox jumped over the lazy dog's back")
```



## Nim


```nim
import md5

echo toMD5("The quick brown fox jumped over the lazy dog's back")
```


=={{header|Oberon-2}}==
{{works with|oo2c}}{{libheader|crypto}}

```oberon2

MODULE MD5;
IMPORT
  Crypto:MD5,
  Crypto:Utils,
  Strings,
  Out;
VAR
  h: MD5.Hash;
  str: ARRAY 128 OF CHAR;
BEGIN
  h := MD5.NewHash();
  h.Initialize;
  str := "The quick brown fox jumped over the lazy dog's back";
  h.Update(str,0,Strings.Length(str));
  h.GetHash(str,0);
  Out.String("MD5: ");Utils.PrintHex(str,0,h.size);Out.Ln
END MD5.

```

{{out}}

```txt

MD5:
   E38CA1D9   20C4B8B8   D3946B2C   72F01680

```



## Objeck


```objeck

class MD5 {
   function : Main(args : String[]) ~ Nil {
      in := "The quick brown fox jumped over the lazy dog's back"->ToByteArray();
      hash := Encryption.Hash->MD5(in);
      hash->ToHexString()->PrintLine();
   }
}

```


=={{header|Objective-C}}==
{{works with|GNUstep}} only; not Cocoa

```objc
NSString *myString = @"The quick brown fox jumped over the lazy dog's back";
NSData *digest = [[myString dataUsingEncoding:NSUTF8StringEncoding] md5Digest]; // or another encoding of your choosing
NSLog(@"%@", [digest hexadecimalRepresentation]);
```


{{works with|iPhone}}
{{works with|Mac OS X}}

```objc>#import <CommonCrypto/CommonDigest.h


NSString *myString = @"The quick brown fox jumped over the lazy dog's back";
NSData *data = [myString dataUsingEncoding:NSUTF8StringEncoding]; // or another encoding of your choosing
unsigned char digest[CC_MD5_DIGEST_LENGTH];
if (CC_MD5([data bytes], [data length], digest)) {
    NSMutableString *hex = [NSMutableString string];
    for (int i = 0; i < CC_MD5_DIGEST_LENGTH; i++) {
        [hex appendFormat: @"%02x", (int)(digest[i])];
    }
    NSLog(@"%@", hex);
}
```


{{works with|Mac OS X}} (need to include "libcrypto.dylib" framework)

```objc
#include <openssl/md5.h>


NSString *myString = @"The quick brown fox jumped over the lazy dog's back";
NSData *data = [myString dataUsingEncoding:NSUTF8StringEncoding]; // or another encoding of your choosing
unsigned char digest[MD5_DIGEST_LENGTH];
if (MD5([data bytes], [data length], digest)) {
    NSMutableString *hex = [NSMutableString string];
    for (int i = 0; i < MD5_DIGEST_LENGTH; i++) {
        [hex appendFormat: @"%02x", (int)(digest[i])];
    }
    NSLog(@"%@", hex);
}
```



## OCaml


```ocaml
# Digest.to_hex(Digest.string "The quick brown fox jumped over the lazy dog's back") ;;
- : string = "e38ca1d920c4b8b8d3946b2c72f01680"
```



## Octave


```octave
s = "The quick brown fox jumped over the lazy dog's back";
hash = md5sum(s, true);
disp(hash)
```


For an implementation of MD5, see [[{{FULLPAGENAME}}#MATLAB|MATLAB example]].


## OpenEdge/Progress

The MD5-DIGEST function is readily available, the output is passed thru HEX-ENCODE to convert the raw result to a hexadecimal string, this then needs to be passed thru STRING for display purposes.

```OpenEdge/Progress
MESSAGE
   1 STRING( HEX-ENCODE( MD5-DIGEST( "" ) ) ) SKIP
   2 STRING( HEX-ENCODE( MD5-DIGEST( "a" ) ) ) SKIP
   3 STRING( HEX-ENCODE( MD5-DIGEST( "abc" ) ) ) SKIP
   4 STRING( HEX-ENCODE( MD5-DIGEST( "message digest" ) ) ) SKIP
   5 STRING( HEX-ENCODE( MD5-DIGEST( "abcdefghijklmnopqrstuvwxyz" ) ) ) SKIP
   6 STRING( HEX-ENCODE( MD5-DIGEST( "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" ) ) ) SKIP
   7 STRING( HEX-ENCODE( MD5-DIGEST( "12345678901234567890123456789012345678901234567890123456789012345678901234567890" ) ) )
VIEW-AS ALERT-BOX
```


Output:

```txt

---------------------------
Message
---------------------------
1 d41d8cd98f00b204e9800998ecf8427e
2 0cc175b9c0f1b6a831c399e269772661
3 900150983cd24fb0d6963f7d28e17f72
4 f96b697d7cb7938d525a2f31aaf161d0
5 c3fcd3d76192e4007dfb496cca67e13b
6 d174ab98d277d9f5a5611c2c9f419d9f
7 57edf4a22be3c955ac49da2e2107b67a
---------------------------
OK
---------------------------

```



## PARI/GP


Build a MD5 plugin using Linux system library and PARI's function interface. (Linux solution)

```C>#include <pari/pari.h

#include <openssl/md5.h>

#define HEX(x)  (((x) < 10)? (x)+'0': (x)-10+'a')

/*
 * PARI/GP func: MD5 hash
 *
 * gp code: install("plug_md5", "s", "MD5", "<library path>");
 */
GEN plug_md5(char *text)
{
  char md[MD5_DIGEST_LENGTH];
  char hash[sizeof(md) * 2 + 1];
  int i;

  MD5((unsigned char*)text, strlen(text), (unsigned char*)md);

  for (i = 0; i < sizeof(md); i++) {
    hash[i+i]   = HEX((md[i] >> 4) & 0x0f);
    hash[i+i+1] = HEX(md[i] & 0x0f);
  }

  hash[sizeof(md) * 2] = 0;

  return strtoGENstr(hash);
}
```


Compile with: gcc -Wall -O2 -fPIC -shared md5.c -o libmd5.so -lcrypt -lpari

Load plugin from your home directory into PARI:

```parigp
install("plug_md5", "s", "MD5", "~/libmd5.so");

MD5("The quick brown fox jumped over the lazy dog's back")
```


Output:

```txt
"e38ca1d920c4b8b8d3946b2c72f01680"
```


==Pascal==
Pascal has a built-in unit called md5. It can be used to get a digest both for a string and a file.

```txt

program GetMd5;

uses md5;

var
  strEncrypted : string;

begin
  strEncrypted := md5Print(md5String('The quick brown fox jumped over the lazy dog''s back'));
  writeln(strEncrypted);
  readln;
end.

```


output:

```txt

 e38ca1d920c4b8b8d3946b2c72f01680

```

To digest a file, use md5Print(md5File('myfile.txt')) where myfile.txt is a file.


## Perl

{{libheader|Digest::MD5}}

```perl
use Digest::MD5 qw(md5_hex);

print md5_hex("The quick brown fox jumped over the lazy dog's back"), "\n";
```


The same in OO manner

```perl
use Digest::MD5;

$md5 = Digest::MD5->new;
$md5->add("The quick brown fox jumped over the lazy dog's back");
print $md5->hexdigest, "\n";
```



## Perl 6

Library [http://github.com/cosimo/perl6-digest-md5/ Digest::MD5]

```perl6
use Digest::MD5;
say Digest::MD5.md5_hex: "The quick brown fox jumped over the lazy dog's back";
```



## Phix

See [[MD5/Implementation#Phix]]


## PHP


```php
$string = "The quick brown fox jumped over the lazy dog's back";
echo md5( $string );
```



## PicoLisp

Using the openssl library (the 'native' function is only in the 64-bit
version available):

```PicoLisp
(let Str "The quick brown fox jumped over the lazy dog's back"
   (pack
      (mapcar '((B) (pad 2 (hex B)))
         (native "libcrypto.so" "MD5" '(B . 16) Str (length Str) '(NIL (16))) ) ) )
```

Output:

```txt
-> "E38CA1D920C4B8B8D3946B2C72F01680"
```



## Pike


```pike
import String;
import Crypto.MD5;

int main(){
   write( string2hex( hash( "The quick brown fox jumped over the lazy dog's back" ) ) + "\n" );
}
```



## PowerShell

{{trans|C#}}

```powershell
$string = "The quick brown fox jumped over the lazy dog's back"
$data = [Text.Encoding]::UTF8.GetBytes($string)
$hash = [Security.Cryptography.MD5]::Create().ComputeHash($data)
([BitConverter]::ToString($hash) -replace '-').ToLower()
```



## PureBasic

;Purebasic 5.x versions:

```purebasic
UseMD5Fingerprint() ; register the MD5 fingerprint plugin

test$ = "The quick brown fox jumped over the lazy dog's back"

; Call StringFingerprint() function and display MD5 result in Debug window
Debug StringFingerprint(test$, #PB_Cipher_MD5)

```



## Python

Using builtin libraries:

;Python 3.x, 2.5 and later 2.x versions:

```python>>>
 import hashlib
>>> # RFC 1321    test suite:
>>> tests = (
  (b"", 'd41d8cd98f00b204e9800998ecf8427e'),
  (b"a", '0cc175b9c0f1b6a831c399e269772661'),
  (b"abc", '900150983cd24fb0d6963f7d28e17f72'),
  (b"message digest", 'f96b697d7cb7938d525a2f31aaf161d0'),
  (b"abcdefghijklmnopqrstuvwxyz", 'c3fcd3d76192e4007dfb496cca67e13b'),
  (b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", 'd174ab98d277d9f5a5611c2c9f419d9f'),
  (b"12345678901234567890123456789012345678901234567890123456789012345678901234567890", '57edf4a22be3c955ac49da2e2107b67a') )
>>> for text, golden in tests: assert hashlib.md5(text).hexdigest() == golden

>>>
```


;Python 2.5 and later:

```python>>>
 import hashlib
>>> print hashlib.md5("The quick brown fox jumped over the lazy dog's back").hexdigest()
e38ca1d920c4b8b8d3946b2c72f01680
```


;Pre-2.5; removed in 3.x:

```python>>>
 import md5
>>> print md5.md5("The quick brown fox jumped over the lazy dog's back").hexdigest()
e38ca1d920c4b8b8d3946b2c72f01680
```



## R


```R
library(digest)
hexdigest <- digest("The quick brown fox jumped over the lazy dog's back",
                    algo="md5", serialize=FALSE)
```



## Racket


```racket

#lang racket
(require file/md5)

(md5 "")
(md5 "a")
(md5 "abc")
(md5 "message digest")
(md5 "abcdefghijklmnopqrstuvwxyz")
(md5 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
(md5 "12345678901234567890123456789012345678901234567890123456789012345678901234567890")

```


Output:


```txt

#"d41d8cd98f00b204e9800998ecf8427e"
#"0cc175b9c0f1b6a831c399e269772661"
#"900150983cd24fb0d6963f7d28e17f72"
#"f96b697d7cb7938d525a2f31aaf161d0"
#"c3fcd3d76192e4007dfb496cca67e13b"
#"d174ab98d277d9f5a5611c2c9f419d9f"
#"57edf4a22be3c955ac49da2e2107b67a"

```



## REBOL


```rebol>>
 checksum/method "The quick brown fox jumped over the lazy dog" 'md5
== #{08A008A01D498C404B0C30852B39D3B8}
```



## REXX


```rexx
/*REXX program tests the MD5 procedure (below) as per a test suite the  IETF RFC (1321).*/
msg.1 =                                          /*─────MD5 test suite [from above doc].*/
msg.2 = 'a'
msg.3 = 'abc'
msg.4 = 'message digest'
msg.5 = 'abcdefghijklmnopqrstuvwxyz'
msg.6 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'
msg.7 =  12345678901234567890123456789012345678901234567890123456789012345678901234567890
msg.0 = 7                                        /* [↑]  last value doesn't need quotes.*/
                do m=1  for  msg.0;       say    /*process each of the seven messages.  */
                say ' in ='  msg.m               /*display the      in      message.    */
                say 'out ='  MD5(msg.m)          /*   "     "       out        "        */
                end   /*m*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
MD5: procedure; parse arg !;  numeric digits 20  /*insure there's enough decimal digits.*/
     a='67452301'x;  b="efcdab89"x;  c='98badcfe'x;  d="10325476"x;   x00='0'x;  x80="80"x
     #=length(!)                                 /*length in bytes of the input message.*/
     L=#*8//512;   if L<448  then plus=448 - L   /*is the length  less   than  448 ?    */
                   if L>448  then plus=960 - L   /* "  "     "   greater   "    "       */
                   if L=448  then plus=512       /* "  "     "    equal    to   "       */
                                                 /* [↓]  a little of this, ···          */
     $=! || x80 || copies(x00, plus%8 -1)reverse(right(d2c(8 * #), 4, x00)) || '00000000'x
                                                 /* [↑]       ···  and a little of that.*/
       do j=0  to length($) % 64 - 1             /*process the message  (lots of steps).*/
       a_=a;       b_=b;       c_=c;       d_=d  /*save the  original values  for later.*/
       chunk=j*64                                /*calculate the  size  of the chunks.  */
                       do k=1  for 16            /*process the message in chunks.       */
                       !.k=reverse( substr($, chunk + 1 + 4*(k-1), 4) )   /*magic stuff.*/
                       end   /*k*/                                        /*────step────*/
       a  =  .part1( a,   b,   c,   d,    0,    7,   3614090360)          /*■■■■  1 ■■■■*/
       d  =  .part1( d,   a,   b,   c,    1,   12,   3905402710)          /*■■■■  2 ■■■■*/
       c  =  .part1( c,   d,   a,   b,    2,   17,    606105819)          /*■■■■  3 ■■■■*/
       b  =  .part1( b,   c,   d,   a,    3,   22,   3250441966)          /*■■■■  4 ■■■■*/
       a  =  .part1( a,   b,   c,   d,    4,    7,   4118548399)          /*■■■■  5 ■■■■*/
       d  =  .part1( d,   a,   b,   c,    5,   12,   1200080426)          /*■■■■  6 ■■■■*/
       c  =  .part1( c,   d,   a,   b,    6,   17,   2821735955)          /*■■■■  7 ■■■■*/
       b  =  .part1( b,   c,   d,   a,    7,   22,   4249261313)          /*■■■■  8 ■■■■*/
       a  =  .part1( a,   b,   c,   d,    8,    7,   1770035416)          /*■■■■  9 ■■■■*/
       d  =  .part1( d,   a,   b,   c,    9,   12,   2336552879)          /*■■■■ 10 ■■■■*/
       c  =  .part1( c,   d,   a,   b,   10,   17,   4294925233)          /*■■■■ 11 ■■■■*/
       b  =  .part1( b,   c,   d,   a,   11,   22,   2304563134)          /*■■■■ 12 ■■■■*/
       a  =  .part1( a,   b,   c,   d,   12,    7,   1804603682)          /*■■■■ 13 ■■■■*/
       d  =  .part1( d,   a,   b,   c,   13,   12,   4254626195)          /*■■■■ 14 ■■■■*/
       c  =  .part1( c,   d,   a,   b,   14,   17,   2792965006)          /*■■■■ 15 ■■■■*/
       b  =  .part1( b,   c,   d,   a,   15,   22,   1236535329)          /*■■■■ 16 ■■■■*/
       a  =  .part2( a,   b,   c,   d,    1,    5,   4129170786)          /*■■■■ 17 ■■■■*/
       d  =  .part2( d,   a,   b,   c,    6,    9,   3225465664)          /*■■■■ 18 ■■■■*/
       c  =  .part2( c,   d,   a,   b,   11,   14,    643717713)          /*■■■■ 19 ■■■■*/
       b  =  .part2( b,   c,   d,   a,    0,   20,   3921069994)          /*■■■■ 20 ■■■■*/
       a  =  .part2( a,   b,   c,   d,    5,    5,   3593408605)          /*■■■■ 21 ■■■■*/
       d  =  .part2( d,   a,   b,   c,   10,    9,     38016083)          /*■■■■ 22 ■■■■*/
       c  =  .part2( c,   d,   a,   b,   15,   14,   3634488961)          /*■■■■ 23 ■■■■*/
       b  =  .part2( b,   c,   d,   a,    4,   20,   3889429448)          /*■■■■ 24 ■■■■*/
       a  =  .part2( a,   b,   c,   d,    9,    5,    568446438)          /*■■■■ 25 ■■■■*/
       d  =  .part2( d,   a,   b,   c,   14,    9,   3275163606)          /*■■■■ 26 ■■■■*/
       c  =  .part2( c,   d,   a,   b,    3,   14,   4107603335)          /*■■■■ 27 ■■■■*/
       b  =  .part2( b,   c,   d,   a,    8,   20,   1163531501)          /*■■■■ 28 ■■■■*/
       a  =  .part2( a,   b,   c,   d,   13,    5,   2850285829)          /*■■■■ 29 ■■■■*/
       d  =  .part2( d,   a,   b,   c,    2,    9,   4243563512)          /*■■■■ 30 ■■■■*/
       c  =  .part2( c,   d,   a,   b,    7,   14,   1735328473)          /*■■■■ 31 ■■■■*/
       b  =  .part2( b,   c,   d,   a,   12,   20,   2368359562)          /*■■■■ 32 ■■■■*/
       a  =  .part3( a,   b,   c,   d,    5,    4,   4294588738)          /*■■■■ 33 ■■■■*/
       d  =  .part3( d,   a,   b,   c,    8,   11,   2272392833)          /*■■■■ 34 ■■■■*/
       c  =  .part3( c,   d,   a,   b,   11,   16,   1839030562)          /*■■■■ 35 ■■■■*/
       b  =  .part3( b,   c,   d,   a,   14,   23,   4259657740)          /*■■■■ 36 ■■■■*/
       a  =  .part3( a,   b,   c,   d,    1,    4,   2763975236)          /*■■■■ 37 ■■■■*/
       d  =  .part3( d,   a,   b,   c,    4,   11,   1272893353)          /*■■■■ 38 ■■■■*/
       c  =  .part3( c,   d,   a,   b,    7,   16,   4139469664)          /*■■■■ 39 ■■■■*/
       b  =  .part3( b,   c,   d,   a,   10,   23,   3200236656)          /*■■■■ 40 ■■■■*/
       a  =  .part3( a,   b,   c,   d,   13,    4,    681279174)          /*■■■■ 41 ■■■■*/
       d  =  .part3( d,   a,   b,   c,    0,   11,   3936430074)          /*■■■■ 42 ■■■■*/
       c  =  .part3( c,   d,   a,   b,    3,   16,   3572445317)          /*■■■■ 43 ■■■■*/
       b  =  .part3( b,   c,   d,   a,    6,   23,     76029189)          /*■■■■ 44 ■■■■*/
       a  =  .part3( a,   b,   c,   d,    9,    4,   3654602809)          /*■■■■ 45 ■■■■*/
       d  =  .part3( d,   a,   b,   c,   12,   11,   3873151461)          /*■■■■ 46 ■■■■*/
       c  =  .part3( c,   d,   a,   b,   15,   16,    530742520)          /*■■■■ 47 ■■■■*/
       b  =  .part3( b,   c,   d,   a,    2,   23,   3299628645)          /*■■■■ 48 ■■■■*/
       a  =  .part4( a,   b,   c,   d,    0,    6,   4096336452)          /*■■■■ 49 ■■■■*/
       d  =  .part4( d,   a,   b,   c,    7,   10,   1126891415)          /*■■■■ 50 ■■■■*/
       c  =  .part4( c,   d,   a,   b,   14,   15,   2878612391)          /*■■■■ 51 ■■■■*/
       b  =  .part4( b,   c,   d,   a,    5,   21,   4237533241)          /*■■■■ 52 ■■■■*/
       a  =  .part4( a,   b,   c,   d,   12,    6,   1700485571)          /*■■■■ 53 ■■■■*/
       d  =  .part4( d,   a,   b,   c,    3,   10,   2399980690)          /*■■■■ 54 ■■■■*/
       c  =  .part4( c,   d,   a,   b,   10,   15,   4293915773)          /*■■■■ 55 ■■■■*/
       b  =  .part4( b,   c,   d,   a,    1,   21,   2240044497)          /*■■■■ 56 ■■■■*/
       a  =  .part4( a,   b,   c,   d,    8,    6,   1873313359)          /*■■■■ 57 ■■■■*/
       d  =  .part4( d,   a,   b,   c,   15,   10,   4264355552)          /*■■■■ 58 ■■■■*/
       c  =  .part4( c,   d,   a,   b,    6,   15,   2734768916)          /*■■■■ 59 ■■■■*/
       b  =  .part4( b,   c,   d,   a,   13,   21,   1309151649)          /*■■■■ 60 ■■■■*/
       a  =  .part4( a,   b,   c,   d,    4,    6,   4149444226)          /*■■■■ 61 ■■■■*/
       d  =  .part4( d,   a,   b,   c,   11,   10,   3174756917)          /*■■■■ 62 ■■■■*/
       c  =  .part4( c,   d,   a,   b,    2,   15,    718787259)          /*■■■■ 63 ■■■■*/
       b  =  .part4( b,   c,   d,   a,    9,   21,   3951481745)          /*■■■■ 64 ■■■■*/
       a  =  .a(a_, a);     b=.a(b_, b);      c=.a(c_, c);    d=.a(d_, d)
       end   /*j*/

     return c2x( reverse(a) )c2x( reverse(b) )c2x( reverse(c) )c2x( reverse(d) )
/*──────────────────────────────────────────────────────────────────────────────────────*/
.a:     return  right( d2c( c2d( arg(1) )   +   c2d( arg(2) ) ),  4, '0'x)
.h:     return  bitxor( bitxor( arg(1), arg(2) ), arg(3) )
.i:     return  bitxor( arg(2), bitor(arg(1), bitxor(arg(3),        'ffffffff'x)))
.f:     return  bitor( bitand(arg(1),arg(2)), bitand(bitxor(arg(1), 'ffffffff'x), arg(3)))
.g:     return  bitor( bitand(arg(1),arg(3)), bitand(arg(2), bitxor(arg(3), 'ffffffff'x)))
.Lr:    procedure;  parse arg _,#;           if #==0  then return _       /*left rotate.*/
                  ?=x2b(c2x(_));     return x2c( b2x( right(? || left(?, #), length(?) )))
.part1: procedure expose !.;   parse arg w,x,y,z,n,m,_;        n=n+1
                  return .a(.Lr(right(d2c(_+c2d(w) +c2d(.f(x,y,z))+c2d(!.n)),4,'0'x),m),x)
.part2: procedure expose !.;   parse arg w,x,y,z,n,m,_;        n=n+1
                  return .a(.Lr(right(d2c(_+c2d(w) +c2d(.g(x,y,z))+c2d(!.n)),4,'0'x),m),x)
.part3: procedure expose !.;   parse arg w,x,y,z,n,m,_;        n=n+1
                  return .a(.Lr(right(d2c(_+c2d(w) +c2d(.h(x,y,z))+c2d(!.n)),4,'0'x),m),x)
.part4: procedure expose !.;   parse arg w,x,y,z,n,m,_;        n=n+1
                  return .a(.Lr(right(d2c(c2d(w) +c2d(.i(x,y,z))+c2d(!.n)+_),4,'0'x),m),x)
```

'''output'''   when using the default (internal) inputs:

```txt

 in =
out = D41D8CD98F00B204E9800998ECF8427E

 in = a
out = 0CC175B9C0F1B6A831C399E269772661

 in = abc
out = 900150983CD24FB0D6963F7D28E17F72

 in = message digest
out = F96B697D7CB7938D525A2F31AAF161D0

 in = abcdefghijklmnopqrstuvwxyz
out = C3FCD3D76192E4007DFB496CCA67E13B

 in = ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789
out = D174AB98D277D9F5A5611C2C9F419D9F

 in = 12345678901234567890123456789012345678901234567890123456789012345678901234567890
out = 57EDF4A22BE3C955AC49DA2E2107B67A

```



## RLaB

RLaB has a built-in function ''hash,''
:<math>hs = hash(fn, s /, nl/) </math>
which implements hash functions ''fn'' as "md2", "md5", "sha", "sha1", "dss", "dss1" and "ripemd160",
for given string vector ''ss'' and new-line delimiter ''nl''. The last is here so that a hash of a string vector
calculated in RLaB is the same as the hash of the same string vector written to a file.


```RLaB>>
 x = "The quick brown fox jumped over the lazy dog's back"
The quick brown fox jumped over the lazy dog's back

>> hash("md5", x)
e38ca1d920c4b8b8d3946b2c72f01680
```



## Ring


```ring

See MD5("my string!") + nl
# output : a83a049fbe50cf7334caa86bf16a3520

```



## RPG

Modified from [http://www.mysamplecode.com/2011/05/rpgle-generate-sha-1-hash-use.html]:

```rpg
**FREE
Ctl-opt MAIN(Main);
Ctl-opt DFTACTGRP(*NO) ACTGRP(*NEW);

dcl-pr QDCXLATE EXTPGM('QDCXLATE');
  dataLen         packed(5 : 0) CONST;
  data            char(32767) options(*VARSIZE);
  conversionTable char(10) CONST;
end-pr;

dcl-pr Qc3CalculateHash EXTPROC('Qc3CalculateHash');
  inputData             pointer value;
  inputDataLen          int(10) const;
  inputDataFormat       char(8) const;
  algorithmDscr         char(16) const;
  algorithmFormat       char(8) const;
  cryptoServiceProvider char(1) const;
  cryptoDeviceName      char(1) const options(*OMIT);
  hash                  char(64) options(*VARSIZE : *OMIT);
  errorCode             char(32767) options(*VARSIZE);
end-pr;

dcl-c HEX_CHARS CONST('0123456789ABCDEF');

dcl-proc Main;
  dcl-s inputData char(45);
  dcl-s inputDataLen int(10) INZ(0);
  dcl-s outputHash char(16);
  dcl-s outputHashHex char(32);
  dcl-ds algorithmDscr QUALIFIED;
    hashAlgorithm int(10) INZ(0);
  end-ds;
  dcl-ds ERRC0100_NULL QUALIFIED;
    bytesProvided int(10) INZ(0);  // Leave at zero
    bytesAvailable int(10);
  end-ds;

  dow inputDataLen = 0;
    DSPLY 'Input: ' '' inputData;
    inputData = %trim(inputData);
    inputDataLen = %len(%trim(inputData));
    DSPLY ('Input=' + inputData);
    DSPLY ('InputLen=' + %char(inputDataLen));
    if inputDataLen = 0;
      DSPLY 'Input must not be blank';
    endif;
  enddo;

  // Convert from EBCDIC to ASCII
  QDCXLATE(inputDataLen : inputData : 'QTCPASC');
  algorithmDscr.hashAlgorithm = 1;   // MD5
  // Calculate hash
  Qc3CalculateHash(%addr(inputData) : inputDataLen : 'DATA0100' : algorithmDscr
                   : 'ALGD0500' : '0' : *OMIT : outputHash : ERRC0100_NULL);
  // Convert to hex
  CVTHC(outputHashHex : outputHash : 32);
  DSPLY ('MD5: ' + outputHashHex);
  return;
end-proc;

// This procedure is actually a MI, but I couldn't get it to bind so I wrote my own version
dcl-proc CVTHC;
  dcl-pi *N;
    target char(65534) options(*VARSIZE);
    srcBits char(32767) options(*VARSIZE) CONST;
    targetLen int(10) value;
  end-pi;
  dcl-s i int(10);
  dcl-s lowNibble ind INZ(*OFF);
  dcl-s inputOffset int(10) INZ(1);
  dcl-ds dataStruct QUALIFIED;
    numField int(5) INZ(0);
    // IBM i is big-endian
    charField char(1) OVERLAY(numField : 2);
  end-ds;

  for i = 1 to targetLen;
    if lowNibble;
      dataStruct.charField = %BitAnd(%subst(srcBits : inputOffset : 1) : X'0F');
      inputOffset += 1;
    else;
      dataStruct.charField = %BitAnd(%subst(srcBits : inputOffset : 1) : X'F0');
      dataStruct.numField /= 16;
    endif;
    %subst(target : i : 1) = %subst(HEX_CHARS : dataStruct.numField + 1 : 1);
    lowNibble = NOT lowNibble;
  endfor;
  return;
end-proc;
```

Note that this implementation converts the input from EBCDIC to ASCII before computing the hash.

Sample output:

```txt
 DSPLY  Input:
 abcdefghijklmnopqrstuvwxyz
 DSPLY  Input=abcdefghijklmnopqrstuvwxyz
 DSPLY  InputLen=26
 DSPLY  MD5: C3FCD3D76192E4007DFB496CCA67E13B
```



## Ruby


```ruby
require 'digest'
Digest::MD5.hexdigest("The quick brown fox jumped over the lazy dog's back")
# => "e38ca1d920c4b8b8d3946b2c72f01680"
```



## Rust

Cargo.toml

```toml

[dependencies]
rust-crypto = "0.2"

```


src/main.rs

```rust

extern crate crypto;

use crypto::digest::Digest;
use crypto::md5::Md5;

fn main() {
    let mut sh = Md5::new();
    sh.input_str("The quick brown fox jumped over the lazy dog's back");
    println!("{}", sh.result_str());
}

```



=={{header|S-lang}}==
Support for MD5 and SHA-1 are included in the standard "chksum" library:

<lang S-lang>require("chksum");
print(md5sum("The quick brown fox jumped over the lazy dog's back"));
```


{{out}}

```txt
"e38ca1d920c4b8b8d3946b2c72f01680"
```



## Scala


```scala
object RosettaMD5 extends App {

  def MD5(s: String): String = {
    // Besides "MD5", "SHA-256", and other hashes are available
    val m = java.security.MessageDigest.getInstance("MD5").digest(s.getBytes("UTF-8"))
    m.map("%02x".format(_)).mkString
  }

  assert("d41d8cd98f00b204e9800998ecf8427e" == MD5(""))
  assert("0cc175b9c0f1b6a831c399e269772661" == MD5("a"))
  assert("900150983cd24fb0d6963f7d28e17f72" == MD5("abc"))
  assert("f96b697d7cb7938d525a2f31aaf161d0" == MD5("message digest"))
  assert("c3fcd3d76192e4007dfb496cca67e13b" == MD5("abcdefghijklmnopqrstuvwxyz"))
  assert("e38ca1d920c4b8b8d3946b2c72f01680" == MD5("The quick brown fox jumped over the lazy dog's back"))
  assert("d174ab98d277d9f5a5611c2c9f419d9f" ==
    MD5("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))
  assert("57edf4a22be3c955ac49da2e2107b67a" ==
    MD5("12345678901234567890123456789012345678901234567890123456789012345678901234567890"))
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
    writeln(hex(md5("The quick brown fox jumped over the lazy dog's back")));
  end func;
```


{{out}}

```txt

e38ca1d920c4b8b8d3946b2c72f01680

```



## Sidef

{{libheader|Digest::MD5}}

```ruby
var digest = frequire('Digest::MD5');
say digest.md5_hex("The quick brown fox jumped over the lazy dog's back");
```


The same in OO manner

```ruby
var md5 = require('Digest::MD5').new;
md5.add("The quick brown fox jumped over the lazy dog's back");
say md5.hexdigest;
```



## Slate

You must load the code in 'src/lib/md5.slate'.

```slate
'The quick brown fox jumped over the lazy dog\'s back' md5String. "==> 'e38ca1d920c4b8b8d3946b2c72f01680'"
```



## Smalltalk

Using utility classes:

{{works with|GNU Smalltalk}}

```smalltalk
PackageLoader fileInPackage: 'Digest' !
(MD5 hexDigestOf: 'The quick brown fox jumped over the lazy dog''s back') displayNl.
```


{{works with|Smalltalk/X}}

```smalltalk
(MD5Stream hashValueOf: 'The quick brown fox jumped over the lazy dog''s back') printCR.
```



## SQL

{{works with|MySQL}}

```sql
SELECT MD5('The quick brown fox jumped over the lazy dog\'s back')
```



## Suneido


```Suneido
Md5('The quick brown fox jumped over the lazy dog\'s back')
```



## Tcl

{{tcllib|md5}}

```tcl
package require md5
puts [md5::md5 -hex "The quick brown fox jumped over the lazy dog's back"]
# ==> E38CA1D920C4B8B8D3946B2C72F01680
```



## UNIX Shell

Shells execute system commands (such as ''md5sum'', in this case). We must pass "-n" to ''echo'', so no trailing newline is appended, which would change the MD5-hash.

[[GNU]] coreutils has ''md5sum'':

```bash
echo -n "The quick brown fox jumped over the lazy dog's back" | md5sum
```


Several [[BSD]] systems have ''md5:''

```bash
echo -n "The quick brown fox jumped over the lazy dog's back" | md5
```


{{libheader|OpenSSL}}

```bash
echo -n "The quick brown fox jumped over the lazy dog's back" |
  openssl md5 | sed 's/.*= //'
```



## Visual Basic .NET

{{works with|Visual Basic .NET|9.0+}}

```vbnet
Imports System.Security.Cryptography
Imports System.Text

Module MD5hash
    Sub Main(args As String())
        Console.WriteLine(GetMD5("Visual Basic .Net"))
    End Sub

    Private Function GetMD5(plainText As String) As String
        Dim hash As String = ""

        Using hashObject As MD5 = MD5.Create()
            Dim ptBytes As Byte() = hashObject.ComputeHash(Encoding.UTF8.GetBytes(plainText))
            Dim hashBuilder As New StringBuilder

            For i As Integer = 0 To ptBytes.Length - 1
                hashBuilder.Append(ptBytes(i).ToString("X2"))
            Next
            hash = hashBuilder.ToString
        End Using

        Return hash
    End Function

End Module

```


{{out}}

```txt

AF397EA30996B22759740AC66452D47A

```



## zkl

md5 is a built in utility

```zkl
Utils.MD5.calc("message digest"); //-->"f96b697d7cb7938d525a2f31aaf161d0"
Utils.MD5.calc("abcdefghijklmnopqrstuvwxyz"); //-->"c3fcd3d76192e4007dfb496cca67e13b"
```



{{omit from|GUISS}}

[[Wikipedia::http://en.wikipedia.org/wiki/MD5]]
