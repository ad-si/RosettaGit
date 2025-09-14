+++
title = "SHA-1"
description = ""
date = 2019-07-08T23:08:47Z
aliases = []
[extra]
id = 11225
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "astro",
  "autohotkey",
  "bbc_basic",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dwscript",
  "erlang",
  "factor",
  "fortran",
  "freebasic",
  "genie",
  "go",
  "halon",
  "haskell",
  "j",
  "java",
  "jsish",
  "julia",
  "kotlin",
  "lasso",
  "liberty_basic",
  "lingo",
  "livecode",
  "lua",
  "mathematica",
  "netrexx",
  "newlisp",
  "nim",
  "ocaml",
  "octave",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "ring",
  "ruby",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "smalltalk",
  "tcl",
  "unix_shell",
  "zkl",
]
+++

## Task

{{task}} [[Category:Checksums]]
'''SHA-1''' or '''SHA1''' is a one-way hash function;
it computes a 160-bit message digest.
SHA-1 often appears in security protocols; for example,
many HTTPS websites use RSA with SHA-1 to secure their connections.
BitTorrent uses SHA-1 to verify downloads.
Git and Mercurial use SHA-1 digests to identify commits.

A US government standard, [[SHA-1/FIPS-180-1|FIPS 180-1]], defines SHA-1.

Find the SHA-1 message digest for a string of [[octet]]s. You may either call a SHA-1 library, or implement SHA-1 in your language. Both approaches interest Rosetta Code.

{{alertbox|lightgray|'''Warning:''' SHA-1 has [https://en.wikinews.org/wiki/Chinese_researchers_crack_major_U.S._government_algorithm_used_in_digital_signatures known weaknesses]. Theoretical attacks may find a collision after [http://lwn.net/Articles/337745/ 2<sup>52</sup> operations], or perhaps fewer.
This is much faster than a brute force attack of 2<sup>80</sup> operations. USgovernment [http://csrc.nist.gov/groups/ST/hash/statement.html deprecated SHA-1].
For production-grade cryptography, users may consider a stronger alternative, such as SHA-256 (from the SHA-2 family) or the upcoming SHA-3.}}


## Ada

```Ada
with Ada.Text_IO;
with GNAT.SHA1;

procedure Main is
begin
   Ada.Text_IO.Put_Line ("SHA1 (""Rosetta Code"") = " &
                         GNAT.SHA1.Digest ("Rosetta Code"));
end Main;
```


```txt
SHA1 ("Rosetta Code") = 48c98f7e5a6e736d790ab740dfc3f51a61abe2b5
```



## Astro


```python
import crypto { sha1 }
let hash = sha1.hexdigest('Ars longa, vita brevis')
print hash

```



## AutoHotkey

Source: [https://github.com/jNizM/AutoHotkey_Scripts/tree/master/Functions/Checksums SHA-1 @github] by jNizM

```AutoHotkey
str := "Rosetta Code"
MsgBox, % "String:`n" (str) "`n`nSHA:`n" SHA(str)



; SHA
### =========================================================================

SHA(string, encoding = "utf-8")
{
    return CalcStringHash(string, 0x8004, encoding)
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

```txt
String:    Rosetta Code
SHA-1:     48C98F7E5A6E736D790AB740DFC3F51A61ABE2B5
```



## BBC BASIC


### Library

```bbcbasic
      PRINT FNsha1("Rosetta Code")
      END

      DEF FNsha1(message$)
      LOCAL buflen%, buffer%, hprov%, hhash%, hash$, i%
      CALG_SHA1 = &8004
      CRYPT_VERIFYCONTEXT = &F0000000
      HP_HASHVAL = 2
      PROV_RSA_FULL = 1
      buflen% = 64
      DIM buffer% LOCAL buflen%-1
      SYS "CryptAcquireContext", ^hprov%, 0, 0, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT
      SYS "CryptCreateHash", hprov%, CALG_SHA1, 0, 0, ^hhash%
      SYS "CryptHashData", hhash%, message$, LEN(message$), 0
      SYS "CryptGetHashParam", hhash%, HP_HASHVAL, buffer%, ^buflen%, 0
      SYS "CryptDestroyHash", hhash%
      SYS "CryptReleaseContext", hprov%
      FOR i% = 0 TO buflen%-1
        hash$ += RIGHT$("0" + STR$~buffer%?i%, 2)
      NEXT
      = hash$
```

```txt

48C98F7E5A6E736D790AB740DFC3F51A61ABE2B5

```



### Native

```bbcbasic
      *FLOAT64
      PRINT FNsha1("Rosetta Code")
      END

      DEF FNsha1(message$)
      LOCAL a%, b%, c%, d%, e%, f%, i%, j%, k%, l%, t%
      LOCAL h0%, h1%, h2%, h3%, h4%, w%()

      REM Initialize variables:
      h0% = &67452301
      h1% = &EFCDAB89
      h2% = &98BADCFE
      h3% = &10325476
      h4% = &C3D2E1F0

      l% = LEN(message$)*8

      REM Pre-processing:
      REM append the bit '1' to the message:
      message$ += CHR$&80

      REM append k bits '0', where k is the minimum number >= 0 such that
      REM the resulting message length (in bits) is congruent to 448 (mod 512)
      WHILE (LEN(message$) MOD 64) <> 56
        message$ += CHR$0
      ENDWHILE

      REM append length of message (before pre-processing), in bits, as
      REM 64-bit big-endian integer
      FOR i% = 56 TO 0 STEP -8
        message$ += CHR$(l% >>> i%)
      NEXT

      REM Process the message in successive 512-bit chunks:
      REM break message into 512-bit chunks, for each chunk
      REM break chunk into sixteen 32-bit big-endian words w[i], 0 <= i <= 15

      DIM w%(79)
      FOR j% = 0 TO LEN(message$) DIV 64 - 1

        FOR i% = 0 TO 15
          w%(i%) = !(!^message$ + 64*j% + 4*i%)
          SWAP ?(^w%(i%)+0),?(^w%(i%)+3)
          SWAP ?(^w%(i%)+1),?(^w%(i%)+2)
        NEXT i%

        REM Extend the sixteen 32-bit words into eighty 32-bit words:
        FOR i% = 16 TO 79
          w%(i%) = w%(i%-3) EOR w%(i%-8) EOR w%(i%-14) EOR w%(i%-16)
          w%(i%) = (w%(i%) << 1) OR (w%(i%) >>> 31)
        NEXT i%

        REM Initialize hash value for this chunk:
        a% = h0%
        b% = h1%
        c% = h2%
        d% = h3%
        e% = h4%

        REM Main loop:
        FOR i% = 0 TO 79
          CASE TRUE OF
            WHEN 0 <= i% AND i% <= 19
              f% = (b% AND c%) OR ((NOT b%) AND d%)
              k% = &5A827999
            WHEN 20 <= i% AND i% <= 39
              f% = b% EOR c% EOR d%
              k% = &6ED9EBA1
            WHEN 40 <= i% AND i% <= 59
              f% = (b% AND c%) OR (b% AND d%) OR (c% AND d%)
              k% = &8F1BBCDC
            WHEN 60 <= i% AND i% <= 79
              f% = b% EOR c% EOR d%
              k% = &CA62C1D6
          ENDCASE

          t% = FN32(((a% << 5) OR (a% >>> 27)) + f% + e% + k% + w%(i%))
          e% = d%
          d% = c%
          c% = (b% << 30) OR (b% >>> 2)
          b% = a%
          a% = t%

        NEXT i%

        REM Add this chunk's hash to result so far:
        h0% = FN32(h0% + a%)
        h1% = FN32(h1% + b%)
        h2% = FN32(h2% + c%)
        h3% = FN32(h3% + d%)
        h4% = FN32(h4% + e%)

      NEXT j%

      = FNhex(h0%) + FNhex(h1%) + FNhex(h2%) + FNhex(h3%) + FNhex(h4%)

      DEF FNhex(A%) = RIGHT$("0000000"+STR$~A%,8)

      DEF FN32(n#)
      WHILE n# > &7FFFFFFF : n# -= 2^32 : ENDWHILE
      WHILE n# < &80000000 : n# += 2^32 : ENDWHILE
      = n#
```

```txt

48C98F7E5A6E736D790AB740DFC3F51A61ABE2B5

```



## C

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <openssl/sha.h>

int main()
{
  int i;
  unsigned char result[SHA_DIGEST_LENGTH];
  const char *string = "Rosetta Code";

  SHA1(string, strlen(string), result);

  for(i = 0; i < SHA_DIGEST_LENGTH; i++)
    printf("%02x%c", result[i], i < (SHA_DIGEST_LENGTH-1) ? ' ' : '\n');

  return EXIT_SUCCESS;
}
```



## C++

Compiling with <code>g++ -lPocoCrypto shaexample.cpp -o shaexample</code>:

```cpp
#include <string>
#include <iostream>
#include "Poco/SHA1Engine.h"
#include "Poco/DigestStream.h"

using Poco::DigestEngine ;
using Poco::SHA1Engine ;
using Poco::DigestOutputStream ;

int main( ) {
   std::string myphrase ( "Rosetta Code" ) ;
   SHA1Engine sha1 ;
   DigestOutputStream outstr( sha1 ) ;
   outstr << myphrase ;
   outstr.flush( ) ; //to pass everything to the digest engine
   const DigestEngine::Digest& digest = sha1.digest( ) ;
   std::cout << myphrase << " as a sha1 digest :" << DigestEngine::digestToHex( digest )
      << " !" << std::endl ;
   return 0 ;
}
```

```txt
Rosetta Code as a sha1 digest :48c98f7e5a6e736d790ab740dfc3f51a61abe2b5 !
```



## C#

Tests the built-in SHA1CryptoServiceProvider:

```c#
using System;
using System.Security.Cryptography;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace RosettaCode.SHA1
{
    [TestClass]
    public class SHA1CryptoServiceProviderTest
    {
        [TestMethod]
        public void TestComputeHash()
        {
            var input = new UTF8Encoding().GetBytes("Rosetta Code");
            var output = new SHA1CryptoServiceProvider().ComputeHash(input);
            Assert.AreEqual(
                "48-C9-8F-7E-5A-6E-73-6D-79-0A-B7-40-DF-C3-F5-1A-61-AB-E2-B5",
                BitConverter.ToString(output));
        }
    }
}
```


=={{header|Caché ObjectScript}}==


```txt
USER>set hash=$System.Encryption.SHA1Hash("Rosetta Code")
USER>zzdump hash
0000: 48 C9 8F 7E 5A 6E 73 6D 79 0A B7 40 DF C3 F5 1A
0010: 61 AB E2 B5
```



## Clojure

As Clojure is interoperable with Java the solution to this task would be a small modification to [[MD5#Clojure|MD5]], as with Java. (Replacing "MD5" with "SHA-1" as noted [http://docs.oracle.com/javase/7/docs/technotes/guides/security/StandardNames.html#MessageDigest here].)


## Common Lisp

This example uses the [http://method-combination.net/lisp/ironclad/ Ironclad] cryptography library (available via Quicklisp as well).

```lisp
;;; in addition to sha1, ironclad provides sha224, sha256, sha384, and sha512.
(defun sha1-hash (data)
  (let ((sha1 (ironclad:make-digest 'ironclad:sha1))
        (bin-data (ironclad:ascii-string-to-byte-array data)))
    (ironclad:update-digest sha1 bin-data)
    (ironclad:byte-array-to-hex-string (ironclad:produce-digest sha1))))

```



## D

'''First:''' Use native 'std.digest.sha' library
```d
void main() {
    import std.stdio, std.digest.sha;

    writefln("%-(%02x%)", "Ars longa, vita brevis".sha1Of);
}
```

```txt
e640d285242886eb96ab80cbf858389b3df52f43
```


'''Second:''' Re-implement SHA-1 in D

```d
import std.stdio, std.string, std.conv, std.algorithm, std.format, std.array,
       std.range, std.digest.sha;

int rol(int n, int b) {
  return ((n << b) | (n >>> (32 - b))) & 0xffffffff;
}

int btoi(string bin) {
  int total = 0;
  foreach (b; bin) {
    total *= 2;
    (b == '1') ? total += 1 : total;
  }
  return total;
}

string sha1(char[] intake) {
  int h0 = 0x67452301;
  int h1 = 0xEFCDAB89;
  int h2 = 0x98BADCFE;
  int h3 = 0x10325476;
  int h4 = 0xC3D2E1F0;

  auto bins = intake.map!(x => format("%08b", x.to!int));
  int binsize = bins.join().length.to!int;
  string o = bins.join() ~ "1";
  o ~= replicate("0", 448%512 - o.length.to!int%512) ~ format("%064b", binsize);
  auto binchunks = chunks(o, 512).array;
  foreach (chunk; binchunks) {
    string[] words = chunk.chunks(512/16).array
                       .map!(x => "%032s".format(x)).array;
    foreach (i; iota(16, 80)) {
      int newWord = btoi(words[i-3]) ^ btoi(words[i-8]) ^
                    btoi(words[i-14]) ^ btoi(words[i-16]);
      newWord = rol(newWord, 1);
      words = words.array ~ "%032b".format(newWord);
    }
    int A = h0;
    int B = h1;
    int C = h2;
    int D = h3;
    int E = h4;
    foreach (i; iota(0, 80)) {
      int F = 0;
      int K = 0;
      if (i < 20) {
        F = D ^ (B & (C ^ D));
        K = 0x5A827999;
      }
      else if (i < 40) {
        F = B ^ C ^ D;
        K = 0x6ED9EBA1;
      }
      else if (i < 60) {
        F = (B & C) | (B & D) | (C & D);
        K = 0x8F1BBCDC;
      }
      else if (i < 80) {
        F = B ^ C ^ D;
        K = 0xCA62C1D6;
      }
      int tempA = A;
      A = rol(A, 5) + F + E + K + btoi(words[i]) & 0xffffffff;
      E = D;
      D = C;
      C = rol(B,30);
      B = tempA;
    }

    h0 = btoi("%032b".format(h0 + A).retro.array[0 .. 32].retro.to!string);
    h1 = btoi("%032b".format(h1 + B).retro.array[0 .. 32].retro.to!string);
    h2 = btoi("%032b".format(h2 + C).retro.array[0 .. 32].retro.to!string);
    h3 = btoi("%032b".format(h3 + D).retro.array[0 .. 32].retro.to!string);
    h4 = btoi("%032b".format(h4 + E).retro.array[0 .. 32].retro.to!string);
  }
  return "%08x%08x%08x%08x%08x".format(h0, h1, h2, h3, h4);
}

void main() {
  writeln(sha1("Rosetta Code".dup));
}
```

```txt
48c98f7e5a6e736d790ab740dfc3f51a61abe2b5
```



## DWScript


```delphi
PrintLn( HashSHA1.HashData('Rosetta code') );
```


```txt

b18c883f4da750164b5af362ea9b9f27f90904b4

```



## Erlang

```txt

12> crypto:hash( sha, "A string" ).
<<110,185,174,8,151,66,9,104,174,225,10,43,9,92,82,190,197,150,224,92>>

```


=={{header|F_Sharp|F#}}==

```fsharp

let n = System.Security.Cryptography.SHA1.Create()
Array.iter (printf "%x ") (n.ComputeHash "Rosetta Code"B)

```

```txt

48 c9 8f 7e 5a 6e 73 6d 79 a b7 40 df c3 f5 1a 61 ab e2 b5

```


## Factor

Factor provides ''sha1'' in the ''checksums.sha'' vocabulary. In Factor, ''checksum-bytes'' returns a [[sequence]] of bytes; ''hex-string'' converts this sequence to a hexadecimal string.

 IN: scratchpad '''USING: checksums checksums.sha ;'''
 IN: scratchpad '''"Rosetta Code" sha1 checksum-bytes hex-string .'''
 "48c98f7e5a6e736d790ab740dfc3f51a61abe2b5"

The implementation is at [https://github.com/slavapestov/factor/blob/master/basis/checksums/sha/sha.factor basis/checksums/sha/sha.factor].

Note: In recent factor builds (after June 2017, ie factor 0.98), checksums:hex-string has been moved to math.parser:hex-string>bytes


## Fortran


### Intel Fortran on Windows

Using Windows API. See [https://msdn.microsoft.com/en-us/library/aa379886.aspx CryptAcquireContext], [https://msdn.microsoft.com/en-us/library/aa379908.aspx CryptCreateHash], [https://msdn.microsoft.com/en-us/library/aa380202.aspx CryptHashData] and [https://msdn.microsoft.com/en-us/library/aa379947.aspx CryptGetHashParam] on MSDN.


```fortran
module sha1_mod
    use kernel32
    use advapi32
    implicit none
    integer, parameter :: SHA1LEN = 20
contains
    subroutine sha1hash(name, hash, dwStatus, filesize)
        implicit none
        character(*) :: name
        integer, parameter :: BUFLEN = 32768
        integer(HANDLE) :: hFile, hProv, hHash
        integer(DWORD) :: dwStatus, nRead
        integer(BOOL) :: status
        integer(BYTE) :: buffer(BUFLEN)
        integer(BYTE) :: hash(SHA1LEN)
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

        if (CryptCreateHash(hProv, CALG_SHA1, 0_ULONG_PTR, 0_DWORD, hHash) == FALSE) then

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

        nRead = SHA1LEN
        if (CryptGetHashParam(hHash, HP_HASHVAL, hash, nRead, 0) == FALSE) then
            dwStatus = GetLastError()
            print *, "CryptGetHashParam failed.", status, nRead, dwStatus
        end if

      1 status = CryptDestroyHash(hHash)
      2 status = CryptReleaseContext(hProv, 0)
      3 status = CloseHandle(hFile)
    end subroutine
end module

program sha1
    use sha1_mod
    implicit none
    integer :: n, m, i, j
    character(:), allocatable :: name
    integer(DWORD) :: dwStatus
    integer(BYTE) :: hash(SHA1LEN)
    integer(UINT64) :: filesize

    n = command_argument_count()
    do i = 1, n
        call get_command_argument(i, length=m)
        allocate(character(m) :: name)
        call get_command_argument(i, name)
        call sha1hash(name, hash, dwStatus, filesize)
        if (dwStatus == 0) then
            do j = 1, SHA1LEN
                write(*, "(Z2.2)", advance="NO") hash(j)
            end do
            write(*, "(' ',A,' (',G0,' bytes)')") name, filesize
        end if
        deallocate(name)
    end do
end program
```



## FreeBASIC


```freebasic
' version 18-10-2016
' started with SHA-1/FIPS-180-1
' but used the BBC BASIC native version to finish.
' compile with: fbc -s console

Function SHA_1(test_str As String) As String

  Dim As String message = test_str   ' strings are passed as ByRef's

  Dim As Long i, j
  Dim As UByte Ptr ww1
  Dim As UInteger<32> Ptr ww4

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
    message[l1 -1 - i] = ub_ptr[i]
  Next

  Dim As UInteger<32> A, B, C, D, E, k, temp, W(0 To 79)
  Dim As UInteger<32> H0 = &H67452301
  Dim As UInteger<32> H1 = &HEFCDAB89
  Dim As UInteger<32> H2 = &H98BADCFE
  Dim As UInteger<32> H3 = &H10325476
  Dim As UInteger<32> H4 = &HC3D2E1F0


  For j = 0 To (l1 -1) \ 64 ' split into block of 64 bytes
    ww1 = Cast(Ubyte Ptr, @message[j * 64])
    ww4 = Cast(UInteger<32> Ptr, @message[j * 64])

    For i = 0 To 60 Step 4  'little endian -> big endian
      Swap ww1[i   ], ww1[i +3]
      Swap ww1[i +1], ww1[i +2]
    Next

    For i = 0 To 15    ' copy the 16 32bit block into the array
      W(i) = ww4[i]
    Next

    For i = 16 To 79   ' fill the rest of the array
      temp = W(i -3) Xor W(i -8) Xor W(i -14) Xor W(i -16)
      temp = temp Shl 1 + temp Shr 31
      W(i) = temp
    Next

    A = h0 : B = h1 : C = h2 : D = h3 : E = h4

    For i = 0 To 79
      Select Case As Const i
        Case 0 To 19
          temp = (B And C) or ((Not B) And D)
          k = &H5A827999
        Case 20 To 39
          temp = B Xor C Xor D
          k = &H6ED9EBA1
        Case 40 To 59
          temp = (B And C) Or (B And D) Or (C And D)
          k = &H8F1BBCDC
        Case 60 To 79
          temp = B Xor C Xor D
          k = &hCA62C1D6
      End Select

      temp = A Shl 5 + A Shr 27 + temp + E + k + W(i)
      E = D
      D = C
      C = (B Shl 30) or (B Shr 2)
      B = A
      A = temp

    Next

    h0 += A : h1 += B : h2 += C : h3 += D : h4 += E

  Next

  Return Hex(h0, 8) + Hex(h1, 8) + Hex(h2, 8) + Hex(h3, 8) + Hex(h4, 8)

End Function

' ------=< MAIN >=------

Dim As String test = "Rosetta Code"
Print test; " => "; SHA_1(test)


' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
Rosetta Code => 48C98F7E5A6E736D790AB740DFC3F51A61ABE2B5
```



## Genie

SHA-1, being overtaken, is not recommended but is supported in GLib checksum, ''ChecksumType.SHA1''.


```genie
print Checksum.compute_for_string(ChecksumType.SHA1, "Rosetta code", -1)
```


(The -1 is NUL byte terminated string indicator for length)

See [[SHA-256#Genie]].


## Go


```go
package main

import (
    "crypto/sha1"
    "fmt"
)

func main() {
    h := sha1.New()
    h.Write([]byte("Rosetta Code"))
    fmt.Printf("%x\n", h.Sum(nil))
}
```

```txt

48c98f7e5a6e736d790ab740dfc3f51a61abe2b5

```



## Halon


```halon
$var = "Rosetta Code";
echo sha1($var);
```

```txt

48c98f7e5a6e736d790ab740dfc3f51a61abe2b5

```



## Haskell


```Haskell
module Digestor
   where
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy as B

convertString :: String -> B.ByteString
convertString phrase = B.pack $ map ( fromIntegral . fromEnum ) phrase

convertToSHA1 :: String -> String
convertToSHA1 word = showDigest $ sha1 $ convertString word

main = do
   putStr "Rosetta Code SHA1-codiert: "
   putStrLn $ convertToSHA1 "Rosetta Code"

```

```txt

Rosetta Code SHA1-codiert: 48c98f7e5a6e736d790ab740dfc3f51a61abe2b5

```



## J

From J8 the <tt>ide/qt</tt> addon includes bindings to the Qt library function for a number of hash algorithms incluing SHA-1. Thus:

```j
   require '~addons/ide/qt/qt.ijs'
   getsha1=: 'sha1'&gethash_jqtide_
   getsha1 'Rosetta Code'
48c98f7e5a6e736d790ab740dfc3f51a61abe2b5
```


From J8.06, the sha family of hashes have builtin support.


```j
   sha1=:128!:6
   sha1'Rosetta Code'
48c98f7e5a6e736d790ab740dfc3f51a61abe2b5
```


A implementation of SHA-1 in J follows:


```J
pad=: ,1,(0#~512 | [: - 65 + #),(64#2)#:#

f=:4 :0
  'B C D'=: _32 ]\ y
  if. x < 20 do.
    (B*C)+.D>B
  elseif. x < 40 do.
    B~:C~:D
  elseif. x < 60 do.
    (B*C)+.(B*D)+.C*D
  elseif. x < 80 do.
    B~:C~:D
  end.
)

K=: ((32#2) #: 16b5a827999 16b6ed9eba1 16b8f1bbcdc 16bca62c1d6) {~ <.@%&20

plus=:+&.((32#2)&#.)

H=: #: 16b67452301 16befcdab89 16b98badcfe 16b10325476 16bc3d2e1f0

process=:4 :0
  W=. (, [: , 1 |."#. _3 _8 _14 _16 ~:/@:{ ])^:64 x ]\~ _32
  'A B C D E'=. y=._32[\,y
  for_t. i.80 do.
    TEMP=. (5|.A) plus (t f B,C,D) plus E plus (W{~t) plus K t
    E=. D
    D=. C
    C=. 30 |. B
    B=. A
    A=. TEMP
  end.
  ,y plus A,B,C,D,:E
)

sha1=: [:> [: process&.>/ (<H) (,~ |.) _512<\ pad
```


Example use:


```J
   text2bits=: (8#2) ,@:#: a. i. ]
   bits2hex=: '0123456789abcdef' {~ _4 #.\ ,

   bits2hex sha1 text2bits 'Rosetta Code'
48c98f7e5a6e736d790ab740dfc3f51a61abe2b5
```


Remember that SHA-1 is an obsolete standard (and if you *really* want high speed you'd probably be using [[wp:Application-specific_integrated_circuit|ASICs]] rather than a general purpose computing platform).


## Java

The solution to this task would be a small modification to [[MD5#Java|MD5]] (replacing "MD5" with "SHA-1" as noted [http://docs.oracle.com/javase/7/docs/technotes/guides/security/StandardNames.html#MessageDigest here]).


## Jsish


```javascript
/* SHA-1 hash in Jsish */
var str = 'Rosetta code';
puts(Util.hash(str, {type:'sha1'}));

/*
=!EXPECTSTART!=
b18c883f4da750164b5af362ea9b9f27f90904b4
=!EXPECTEND!=
*/
```


```txt
prompt$ jsish sha-1.jsi
b18c883f4da750164b5af362ea9b9f27f90904b4
prompt$ jsish -u sha-1.jsi
[PASS] sha-1.jsi
```



## Julia

```julia
using Nettle

testdict = Dict("abc" => "a9993e364706816aba3e25717850c26c9cd0d89d",
                "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" =>
                    "84983e441c3bd26ebaae4aa1f95129e5e54670f1",
                "a" ^ 1_000_000 => "34aa973cd4c4daa4f61eeb2bdbad27316534016f",)

for (text, expect) in testdict
    digest = hexdigest("sha1", text)
    if length(text) > 50 text = text[1:50] * "..." end
    println("# $text\n -> digest: $digest\n -> expect: $expect")
end
```


```txt
# abc
 -> digest: a9993e364706816aba3e25717850c26c9cd0d89d
 -> expect: a9993e364706816aba3e25717850c26c9cd0d89d
# abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomn...
 -> digest: 84983e441c3bd26ebaae4aa1f95129e5e54670f1
 -> expect: 84983e441c3bd26ebaae4aa1f95129e5e54670f1
# aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa...
 -> digest: 34aa973cd4c4daa4f61eeb2bdbad27316534016f
 -> expect: 34aa973cd4c4daa4f61eeb2bdbad27316534016f
```



## Kotlin


```scala
// version 1.0.6

import java.security.MessageDigest

fun main(args: Array<String>) {
    val text  = "Rosetta Code"
    val bytes = text.toByteArray()
    val md = MessageDigest.getInstance("SHA-1")
    val digest = md.digest(bytes)
    for (byte in digest) print("%02x".format(byte))
    println()
}
```


```txt

48c98f7e5a6e736d790ab740dfc3f51a61abe2b5

```



## Lasso


```Lasso
cipher_digest('Rosetta Code', -digest='SHA1',-hex=true)
```


```txt
 48c98f7e5a6e736d790ab740dfc3f51a61abe2b5
```



## Liberty BASIC


```lb

'--------------------------------------------------------------------------------
'           FAST SHA1 CALCULATION BASED ON MS ADVAPI32.DLL BY CRYPTOMAN         '
'           BASED ON SHA256 EXAMPLE BY RICHARD T. RUSSEL AUTHOR OF LBB          '
'                           http://lbb.conforums.com/                           '
'           VERIFY CORRECTNESS BY http://www.fileformat.info/tool/hash.htm      '
'--------------------------------------------------------------------------------

print sha1$("Rosetta Code")
end

    X$="1234567890ABCDEF"

    dat$ = pack$(X$)

    print "SPEED TEST"
    for i=1 to 20
    t1=time$("ms")
    print sha1$(dat$)
    t2=time$("ms")
    print "calculated in ";t2-t1;" ms"
    next
    end

function sha1$(message$)

    HP.HASHVAL      = 2
    CRYPT.NEWKEYSET = 48
    PROV.RSA.AES    = 24
    buffer$         = space$(128)

    PROVRSAFULL     = 1
    ALGCLASSHASH    = 32768
    ALGTYPEANY      = 0
    ALGSIDMD2       = 1
    ALGSIDMD4       = 2
    ALGSIDMD5       = 3
    ALGSIDSHA1      = 4

    ALGOSHA1 = ALGCLASSHASH OR ALGTYPEANY OR ALGSIDSHA1

    struct temp, v as long
    open "ADVAPI32.DLL" for dll as #advapi32
    calldll #advapi32, "CryptAcquireContextA", temp as struct, _
                       0 as long, 0 as long, PROV.RSA.AES as long, _
                       0 as long, re as long
    hprov = temp.v.struct
    calldll #advapi32, "CryptCreateHash", hprov as long, _
                       ALGOSHA1 as long, 0 as long, 0 as long, _
                       temp as struct, re as long
    hhash = temp.v.struct
    l = len(message$)
    calldll #advapi32, "CryptHashData", hhash as long, message$ as ptr, _
                       l as long, 0 as long, re as long
    temp.v.struct = len(buffer$)
    calldll #advapi32, "CryptGetHashParam", hhash as long, _
                       HP.HASHVAL as long, buffer$ as ptr, _
                       temp as struct, 0 as long, re as long
    calldll #advapi32, "CryptDestroyHash", hhash as long, re as long
    calldll #advapi32, "CryptReleaseContext", hprov as long, re as long
    close #advapi32
    for i = 1 TO temp.v.struct
      sha1$ = sha1$ + right$("0" + dechex$(asc(mid$(buffer$,i))), 2)
    next
end function

function pack$(x$)
    for i = 1 TO len(x$) step 2
      pack$ = pack$ + chr$(hexdec(mid$(x$,i,2)))
    next
end function

```

```txt
48C98F7E5A6E736D790AB740DFC3F51A61ABE2B5
```



## Lingo

```lingo
crypto = xtra("Crypto").new()
put crypto.cx_sha1_string("Rosetta Code")
```

```txt
-- "48c98f7e5a6e736d790ab740dfc3f51a61abe2b5"
```



## LiveCode


```LiveCode
command shaRosettaCode
    local shex, sha1
    put sha1Digest("Rosetta Code") into sha1
    get binaryDecode("H*",sha1,shex)
    put shex
end shaRosettaCode
```

```txt
48c98f7e5a6e736d790ab740dfc3f51a61abe2b5
```



## Lua


{{libheader|sha1}} ([https://github.com/kikito/sha1.lua luarocks install sha1])


```Lua
#!/usr/bin/lua

local sha1 = require "sha1"

for i, str in ipairs{"Rosetta code", "Rosetta Code"} do
  print(string.format("SHA-1(%q) = %s", str, sha1(str)))
end
```


```txt

SHA-1("Rosetta code") = b18c883f4da750164b5af362ea9b9f27f90904b4
SHA-1("Rosetta Code") = 48c98f7e5a6e736d790ab740dfc3f51a61abe2b5

```



## Mathematica


<lang>Hash["Rosetta code","SHA1","HexString"]
```


```txt
48c98f7e5a6e736d790ab740dfc3f51a61abe2b5
```



## NetRexx

This solution is basically the same as that for [[MD5#NetRExx|MD5]], substituting "SHA-1" for "MD5" as the algorithm to use in the <tt>MessageDigest</tt> instance.

```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

import java.security.MessageDigest

SHA1('Rosetta Code', '48c98f7e5a6e736d790ab740dfc3f51a61abe2b5')

return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method SHA1(messageText, verifyCheck) public static

  algorithm   = 'SHA-1'
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

```txt

<Message>Rosetta Code</Message>
     <SHA-1>48c98f7e5a6e736d790ab740dfc3f51a61abe2b5</SHA-1>
    <Verify>48c98f7e5a6e736d790ab740dfc3f51a61abe2b5</Verify>
SHA-1 Confirmed

```



## NewLISP


```NewLISP
;; using the crypto module from http://www.newlisp.org/code/modules/crypto.lsp.html
;; (import native functions from the crypto library, provided by OpenSSL)
(module "crypto.lsp")
(crypto:sha1 "Rosetta Code")
```



## Nim

Compile with <code>nim -d:ssl c sha1.nim</code>:

```nim
import strutils

const SHA1Len = 20

proc SHA1(d: cstring, n: culong, md: cstring = nil): cstring {.cdecl, dynlib: "libssl.so", importc.}

proc SHA1(s: string): string =
  result = ""
  var s = SHA1(s.cstring, s.len.culong)
  for i in 0 .. < SHA1Len:
    result.add s[i].BiggestInt.toHex(2).toLower

echo SHA1("Rosetta Code")
```


=={{header|Oberon-2}}==
```oberon2

MODULE SHA1;
IMPORT
  Crypto:SHA1,
  Crypto:Utils,
  Strings,
  Out;
VAR
  h: SHA1.Hash;
  str: ARRAY 128 OF CHAR;
BEGIN
  h := SHA1.NewHash();
  h.Initialize;
  str := "Rosetta Code";
  h.Update(str,0,Strings.Length(str));
  h.GetHash(str,0);
  Out.String("SHA1: ");Utils.PrintHex(str,0,h.size);Out.Ln
END SHA1.

```

```txt

SHA1:
   48C98F7E   5A6E736D   790AB740   DFC3F51A   61ABE2B5

```


## OCaml


Using the library <code>ocaml-sha</code> in the interactive loop:


```ocaml
$ ocaml -I +sha sha1.cma
        Objective Caml version 3.12.1

# Sha1.to_hex (Sha1.string "Rosetta Code") ;;
- : string = "48c98f7e5a6e736d790ab740dfc3f51a61abe2b5"
```



## Octave

Normally SHA-1 is available in the [http://octave.sourceforge.net/general/function/SHA1.html general package].


```octave
sprintf("%02x", SHA1(+"Rosetta Code"(:)))
```

```txt
ans = 48c98f7e5a6e736d790ab740dfc3f51a61abe2b5
```



## PARI/GP

It works on Linux systems.


```parigp
sha1(s)=extern("echo \"Str(`echo -n '"Str(s)"'|sha1sum|cut -d' ' -f1`)\"")
```


The code above creates a new function sha1(s) which returns SHA-1 hash of item s.
```txt

sha1("Rosetta Code") = "48c98f7e5a6e736d790ab740dfc3f51a61abe2b5"
sha1(1+2) = "77de68daecd823babbb58edb1c8e14d7106e83bb"  ; sha(3)

```



## Pascal

```pascal
program RosettaSha1;
uses
    sha1;
var
   d: TSHA1Digest;
begin
     d:=SHA1String('Rosetta Code');
     WriteLn(SHA1Print(d));
end.
```


```txt
48c98f7e5a6e736d790ab740dfc3f51a61abe2b5
```



## Perl

```perl
use Digest::SHA qw(sha1_hex);

print sha1_hex('Rosetta Code'), "\n";
```

```txt
48c98f7e5a6e736d790ab740dfc3f51a61abe2b5
```


The same in OO manner

```perl
use Digest::SHA;

my $sha1 = Digest::SHA->new(1);
$sha1->add('Rosetta Code');
print $sha1->hexdigest, "\n";
```



## Perl 6


A pure Perl 6 implementation that closely follows the description of SHA-1 in FIPS 180-1. Slow.


```perl6
sub postfix:<mod2³²> { $^x % 2**32 }
sub infix:<⊕>        { ($^x + $^y)mod2³² }
sub S                { ($^x +< $^n)mod2³² +| ($x +> (32-$n)) }

my \f = -> \B,\C,\D { (B +& C) +| ((+^B)mod2³² +& D)   },
        -> \B,\C,\D { B +^ C +^ D                      },
        -> \B,\C,\D { (B +& C) +| (B +& D) +| (C +& D) },
        -> \B,\C,\D { B +^ C +^ D                      };

my \K = 0x5A827999, 0x6ED9EBA1, 0x8F1BBCDC, 0xCA62C1D6;

sub sha1-pad(Blob $msg)
{
    my \bits = 8 * $msg.elems;
    my @padded = flat $msg.list, 0x80, 0x00 xx (-($msg.elems + 1 + 8) % 64);
    flat @padded.map({ :256[$^a,$^b,$^c,$^d] }), (bits +> 32)mod2³², (bits)mod2³²;
}

sub sha1-block(@H, @M is copy)
{
    @M.push: S(1, [+^] @M[$_ «-« <3 8 14 16>] ) for 16 .. 79;

    my ($A,$B,$C,$D,$E) = @H;
    for 0..79 -> \t {
        ($A, $B, $C, $D, $E) =
        S(5,$A) ⊕ f[t div 20]($B,$C,$D) ⊕ $E ⊕ @M[t] ⊕ K[t div 20],
        $A, S(30,$B), $C, $D;
    }
    @H »⊕=« ($A,$B,$C,$D,$E);
}

sub sha1(Blob $msg) returns Blob
{
    my @M = sha1-pad($msg);
    my @H = 0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0;
    sha1-block(@H,@M[$_..$_+15]) for 0, 16...^ +@M;
    Blob.new: flat map { reverse .polymod(256 xx 3) }, @H;
}

say sha1(.encode('ascii')), "  $_"
   for 'abc',
       'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq',
       'Rosetta Code',
       'Ars longa, vita brevis';
```

```txt
Buf:0x<a9 99 3e 36 47 06 81 6a ba 3e 25 71 78 50 c2 6c 9c d0 d8 9d>  abc
Buf:0x<84 98 3e 44 1c 3b d2 6e ba ae 4a a1 f9 51 29 e5 e5 46 70 f1>  abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq
Buf:0x<48 c9 8f 7e 5a 6e 73 6d 79 0a b7 40 df c3 f5 1a 61 ab e2 b5>  Rosetta Code
Buf:0x<e6 40 d2 85 24 28 86 eb 96 ab 80 cb f8 58 38 9b 3d f5 2f 43>  Ars longa, vita brevis
```



## Phix


```Phix
--
-- demo\rosetta\sha1.exw
--
### ===============

--
--  NB no longer considered secure. Non-optimised.
--
constant m4 = allocate(4)   -- scratch area, for uint32

function uint32(atom v)
    poke4(m4,v)
    return peek4u(m4)
end function

function sq_uint32(sequence s)
    for i=1 to length(s) do
        s[i] = uint32(s[i])
    end for
    return s
end function

function dword(string msg, integer i)
-- get dword as big-endian
    return msg[i]*#1000000+msg[i+1]*#10000+msg[i+2]*#100+msg[i+3]
end function

function xor_all(sequence s)
atom result = 0
    for i=1 to length(s) do
        result = xor_bits(result, s[i])
    end for
    result = uint32(result)
    return result
end function

function rol(atom word, integer bits)
-- left rotate the bits of a 32-bit number by the specified number of bits
    return uint32(word*power(2,bits))+floor(word/power(2,32-bits))
end function

function sha1(string msg)
atom a,b,c,d,e,temp,k
sequence w = repeat(0,80)
atom h0 = 0x67452301,
     h1 = 0xefcdab89,
     h2 = 0x98badcfe,
     h3 = 0x10325476,
     h4 = 0xc3d2e1f0

    integer bits = length(msg)*8
    msg &= #80
    while mod(length(msg),64)!=56 do msg &= '\0' end while
    msg &= reverse(int_to_bytes(bits,8))

    for chunk=1 to length(msg) by 64 do
        for i=1 to 16 do
            w[i] = dword(msg,chunk+(i-1)*4)
        end for
        for i=17 to 80 do
            w[i] = rol(xor_all({w[i-3],w[i-8],w[i-14],w[i-16]}),1)
        end for
        {a,b,c,d,e} = {h0,h1,h2,h3,h4}
        for i=1 to 80 do
            if i<=20 then
                temp = or_bits(and_bits(b,c),and_bits(not_bits(b),d))
                k = #5A827999
            elsif i<=40 then
                temp = xor_bits(xor_bits(b,c),d)
                k = #6ED9EBA1
            elsif i<=60 then
                temp = or_bits(or_bits(and_bits(b,c),and_bits(b,d)),and_bits(c,d))
                k = #8F1BBCDC
            else -- i<=80
                temp = xor_bits(xor_bits(b,c),d)
                k = #CA62C1D6
            end if
            {a,b,c,d,e} = {uint32(rol(a,5)+temp+e+k+w[i]),a,rol(b,30),c,d}
        end for
        {h0,h1,h2,h3,h4} = sq_uint32(sq_add({h0,h1,h2,h3,h4},{a,b,c,d,e}))
    end for
    sequence res = {h0, h1, h2, h3, h4}
    for i=1 to length(res) do
        res[i] = sprintf("%08X",res[i])
    end for
    return join(res)
end function

?sha1("Rosetta Code")
```

```txt

"48c98f7e 5a6e736d 790ab740 dfc3f51a 61abe2b5"

```



## PHP


```php
<?php
$string = 'Rosetta Code';
echo sha1( $string ), "\n";
?>
```

```txt
48c98f7e5a6e736d790ab740dfc3f51a61abe2b5
```



## PicoLisp

Library and implementation.

```PicoLisp
(de leftRotate (X C)
   (| (mod32 (>> (- C) X)) (>> (- 32 C) X)) )

(de mod32 (N)
   (& N `(hex "FFFFFFFF")) )

(de not32 (N)
   (x| N `(hex "FFFFFFFF")) )

(de add32 @
   (mod32 (pass +)) )

(de sha1 (Str)
   (let Len (length Str)
      (setq Str
         (conc
            (need
               (-
                  8
                  (* 64 (/ (+ Len 1 8 63) 64)) )
               (conc
                  (mapcar char (chop Str))
                  (cons `(hex "80")) )
               0 )
            (flip
               (make
                  (setq Len (* 8 Len))
                  (do 8
                     (link (& Len 255))
                     (setq Len (>> 8 Len )) ) ) ) ) ) )
   (let
      (H0 `(hex "67452301")
         H1 `(hex "EFCDAB89")
         H2 `(hex "98BADCFE")
         H3 `(hex "10325476")
         H4 `(hex "C3D2E1F0") )
      (while Str
         (let
            (A H0  B H1  C H2  D H3  E H4
               W (conc
                    (make
                       (do 16
                          (link
                             (apply
                                |
                                (mapcar >> (-24 -16 -8 0) (cut 4 'Str)) ) ) ) )
                  (need 64 0) ) )
               (for (I 17 (>= 80 I) (inc I))
                  (set (nth W I)
                     (leftRotate
                        (x|
                           (get W (- I 3))
                           (get W (- I 8))
                           (get W (- I 14))
                           (get W (- I 16)) )
                        1 ) ) )
               (use (Tmp F K)
                  (for I 80
                     (cond
                        ((>= 20 I)
                           (setq
                              F (| (& B C) (& (not32 B) D))
                              K `(hex "5A827999") ) )
                        ((>= 40 I)
                           (setq
                              F (x| B C D)
                              K `(hex "6ED9EBA1") ) )
                        ((>= 60 I)
                           (setq
                              F (| (& B C) (& B D) (& C D))
                              K `(hex "8F1BBCDC") ) )
                        (T
                           (setq
                              F (x| B C D)
                              K `(hex "CA62C1D6") ) ) )
                     (setq
                        Tmp (add32 (leftRotate A 5) F E K (get W I) )
                        E D
                        D C
                        C (leftRotate B 30)
                        B A
                        A Tmp ) ) )
               (setq
                  H0 (add32 H0 A)
                  H1 (add32 H1 B)
                  H2 (add32 H2 C)
                  H3 (add32 H3 D)
                  H4 (add32 H4 E) ) ) )
      (mapcan
         '((N)
            (flip
               (make
                  (do 4
                     (link (& 255 N))
                     (setq N (>> 8 N)) ) ) ) )
         (list H0 H1 H2 H3 H4) ) ) )

(let Str "Rosetta Code"
   (println
      (pack
         (mapcar
            '((B) (pad 2 (hex B)))
            (sha1 Str) ) ) )
   (println
      (pack
         (mapcar
            '((B) (pad 2 (hex B)))
            (native
               "libcrypto.so"
               "SHA1"
               '(B . 20)
               Str
               (length Str)
               '(NIL (20)) ) ) ) ) )

(bye)
```



## PowerShell


```PowerShell

Function Calculate-SHA1( $String ){
    $Enc = [system.Text.Encoding]::UTF8
    $Data = $enc.GetBytes($String)

    # Create a New SHA1 Crypto Provider
    $Sha = New-Object System.Security.Cryptography.SHA1CryptoServiceProvider

    # Now hash and display results
    $Result = $sha.ComputeHash($Data)
    [System.Convert]::ToBase64String($Result)
}

```

taken from [http://stackoverflow.com/questions/8051713/convert-a-string-to-a-byte-array-in-powershell-version-2 Stackoverflow] with a little modification


## PureBasic

PB Version 5.40

```purebasic
a$="Rosetta Code"

UseSHA1Fingerprint() : b$=StringFingerprint(a$, #PB_Cipher_SHA1)

OpenConsole()
Print("[SHA1] Text: "+a$+" ==> "+b$)
Input()
```

```txt
[SHA1] Text: Rosetta Code ==> 48c98f7e5a6e736d790ab740dfc3f51a61abe2b5
```



## Python


```python
import hashlib
h = hashlib.sha1()
h.update(bytes("Ars longa, vita brevis", encoding="ASCII"))
h.hexdigest()
# "e640d285242886eb96ab80cbf858389b3df52f43"
```



## R


```rsplus

library(digest)

input <- "Rosetta Code"
cat(digest(input, algo = "sha1", serialize = FALSE), "\n")

```

```txt

48c98f7e5a6e736d790ab740dfc3f51a61abe2b5

```



## Racket


With the built-in <tt>file/sha1</tt> library:


```racket

#lang racket
(require file/sha1)
(sha1 (open-input-string "Rosetta Code"))

```


With the faster <tt>openssl/sha1</tt> library (requires OpenSSL to be installed):


```racket

#lang racket
(require openssl/sha1)
(sha1 (open-input-string "Rosetta Code"))

```



## Ring


```ring

# Project : SHA-1

load "stdlib.ring"
str = "Rosetta Code"
see "String: " + str + nl
see "SHA-1: "
see sha1(str) + nl

```

Output:

```txt

String: Rosetta Code
SHA-1: 48c98f7e5a6e736d790ab740dfc3f51a61abe2b5

```



## Ruby

These programs print the SHA-1 of 'Rosetta Code', which is 48c98f7e5a6e736d790ab740dfc3f51a61abe2b5.

'''First:''' Use 'digest' from Ruby's standard library.

```ruby
require 'digest'
puts Digest::SHA1.hexdigest('Rosetta Code')
```


'''Second:''' Use 'openssl' from Ruby's standard library. {{libheader|OpenSSL}}

```ruby
require 'openssl'
puts OpenSSL::Digest::SHA1.hexdigest('Rosetta Code')
```


'''Third:''' Reimplement SHA-1 in Ruby.

```ruby
require 'stringio'

# Calculates SHA-1 message digest of _string_. Returns binary digest.
# For hexadecimal digest, use +*sha1(string).unpack('H*')+.
#--
# This is a simple, pure-Ruby implementation of SHA-1, following
# the algorithm in FIPS 180-1.
#++
def sha1(string)
  # functions and constants
  mask = 0xffffffff
  s = proc{|n, x| ((x << n) & mask) | (x >> (32 - n))}
  f = [
    proc {|b, c, d| (b & c) | (b.^(mask) & d)},
    proc {|b, c, d| b ^ c ^ d},
    proc {|b, c, d| (b & c) | (b & d) | (c & d)},
    proc {|b, c, d| b ^ c ^ d},
  ].freeze
  k = [0x5a827999, 0x6ed9eba1, 0x8f1bbcdc, 0xca62c1d6].freeze

  # initial hash
  h = [0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, 0xc3d2e1f0]

  bit_len = string.size << 3
  string += "\x80"
  while (string.size % 64) != 56
    string += "\0"
  end
  string = string.force_encoding('ascii-8bit') + [bit_len >> 32, bit_len & mask].pack("N2")

  if string.size % 64 != 0
    fail "failed to pad to correct length"
  end

  io = StringIO.new(string)
  block = ""

  while io.read(64, block)
    w = block.unpack("N16")

    # Process block.
    (16..79).each {|t| w[t] = s[1, w[t-3] ^ w[t-8] ^ w[t-14] ^ w[t-16]]}

    a, b, c, d, e = h
    t = 0
    4.times do |i|
      20.times do
        temp = (s[5, a] + f[i][b, c, d] + e + w[t] + k[i]) & mask
        a, b, c, d, e = temp, a, s[30, b], c, d
        t += 1
      end
    end

    [a,b,c,d,e].each_with_index {|x,i| h[i] = (h[i] + x) & mask}
  end

  h.pack("N5")
end

if __FILE__ == $0
  # Print some example SHA-1 digests.
  # FIPS 180-1 has correct digests for 'abc' and 'abc...opq'.
  [ 'abc',
    'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq',
    'Rosetta Code',
  ].each {|s| printf("%s:\n  %s\n", s, *sha1(s).unpack('H*'))}
end
```

```txt

abc:
  a9993e364706816aba3e25717850c26c9cd0d89d
abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq:
  84983e441c3bd26ebaae4aa1f95129e5e54670f1
Rosetta Code:
  48c98f7e5a6e736d790ab740dfc3f51a61abe2b5

```



=={{header|S-lang}}==
Support for MD5 and SHA-1 are included in the standard "chksum" library:

<lang S-lang>require("chksum");
print(sha1sum("Rosetta Code"));
```


```txt
"48c98f7e5a6e736d790ab740dfc3f51a61abe2b5"

```



## Scala

The solution to this task would be a small modification to [[MD5#Scala|MD5]] (replacing "MD5" with "SHA-1" as noted [http://docs.oracle.com/javase/7/docs/technotes/guides/security/StandardNames.html#MessageDigest here]).


```scala

import java.nio._

case class Hash(message: List[Byte]) {
  val defaultHashes = List(0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0)

  val hash = {
    val padded = generatePadding(message)
    val chunks: List[List[Byte]] = messageToChunks(padded)
    toHashForm(hashesFromChunks(chunks))
  }

  def generatePadding(message: List[Byte]): List[Byte] = {
    val finalPadding = BigInt(message.length * 8).toByteArray match {
      case x => List.fill(8 - x.length)(0.toByte) ++ x
    }
    val padding = (message.length + 1) % 64 match {
      case l if l < 56 =>
        message ::: 0x80.toByte :: List.fill(56 - l)(0.toByte)
      case l =>
        message ::: 0x80.toByte :: List.fill((64 - l) + 56 + 1)(0.toByte)
    }
    padding ::: finalPadding
  }

  def toBigEndian(bytes: List[Byte]) =
    ByteBuffer.wrap(bytes.toArray).getInt

  def messageToChunks(message: List[Byte]) =
    message.grouped(64).toList

  def chunkToWords(chunk: List[Byte]) =
    chunk.grouped(4).map(toBigEndian).toList

  def extendWords(words: List[Int]): List[Int] = words.length match {
    case i if i < 80 => extendWords(words :+ Integer.rotateLeft(
      (words(i - 3) ^ words(i - 8) ^ words(i - 14) ^ words(i - 16)), 1))
    case _ => words
  }

  def generateFK(i: Int, b: Int, c: Int, d: Int) = i match {
    case i if i < 20 => (b & c | ~b & d, 0x5A827999)
    case i if i < 40 => (b ^ c ^ d, 0x6ED9EBA1)
    case i if i < 60 => (b & c | b & d | c & d, 0x8F1BBCDC)
    case i if i < 80 => (b ^ c ^ d, 0xCA62C1D6)
  }

  def generateHash(words: List[Int], prevHash: List[Int]): List[Int] = {
    def generateHash(i: Int, currentHashes: List[Int]): List[Int] = i match {
      case i if i < 80 => currentHashes match {
        case a :: b :: c :: d :: e :: Nil => {
          val (f, k) = generateFK(i, b, c, d)
          val x = Integer.rotateLeft(a, 5) + f + e + k + words(i)
          val t = Integer.rotateLeft(b, 30)
          generateHash(i + 1, x :: a :: t :: c :: d :: Nil)
        }
      }
      case _ => currentHashes
    }
    addHashes(prevHash, generateHash(0, prevHash))
  }

  def addHashes(xs: List[Int], ys: List[Int]) = (xs, ys).zipped.map(_ + _)

  def hashesFromChunks(chunks: List[List[Byte]],
                        remainingHash: List[Int] = defaultHashes): List[Int] =
    chunks match {
      case Nil => remainingHash
      case x :: xs => {
        val words = extendWords(chunkToWords(x))
        val newHash = generateHash(words, remainingHash)
        hashesFromChunks(xs, newHash)
      }
    }

  def toHashForm(hashes: List[Int]) =
    hashes.map(b => ByteBuffer.allocate(4)
      .order(ByteOrder.BIG_ENDIAN).putInt(b).array.toList)
      .map(bytesToHex).mkString

  def bytesToHex(bytes: List[Byte]) =
    (for (byte <- bytes) yield (Character.forDigit((byte >> 4) & 0xF, 16) ::
      Character.forDigit((byte & 0xF), 16) :: Nil).mkString).mkString
}

object Hash extends App {
  def hash(message: String) = new Hash(message.getBytes.toList).hash

  println(hash("Rosetta Code"))
}

```


```txt
48c98f7e5a6e736d790ab740dfc3f51a61abe2b5
```




## Scheme

```scheme

; band - binary AND operation
; bor - binary OR operation
; bxor - binary XOR operation
; >>, << - binary shift operations
; runes->string - convert byte list to string /(runes->string '(65 66 67 65)) => "ABCA"/


(define (sha1-padding-size n)
   (let ((x (mod (- 56 (rem n 64)) 64)))
      (if (= x 0) 64 x)))

(define (sha1-pad-message message)
   (let*((message-len (string-length message))
         (message-len-in-bits (* message-len 8))
         (buffer-len (+ message-len 8 (sha1-padding-size message-len)))
         (message (string-append message (runes->string '(#b10000000))))
         (zeroes-len (- buffer-len message-len 1 4)) ; for ending length encoded value
         (message (string-append message (make-string zeroes-len 0)))
         (message (string-append message (runes->string (list
            (band (>> message-len-in-bits 24) #xFF)
            (band (>> message-len-in-bits 16) #xFF)
            (band (>> message-len-in-bits  8) #xFF)
            (band (>> message-len-in-bits  0) #xFF))))))
;      (print "message-len: " message-len)
;      (print "message-len-in-bits: " message-len-in-bits)
;      (print "buffer-len: " buffer-len)
;      (print "zeroes-len: " zeroes-len)
;      (print "message: " message)
;      (print "length(message): " (string-length message))
      message))

(define XOR (lambda args (fold bxor 0 args))) ; bxor more than 2 arguments
(define OR (lambda args (fold bor 0 args))) ; bor more than 2 arguments
(define NOT (lambda (arg) (bxor arg #xFFFFFFFF))) ; binary not operation

; to 32-bit number
(define (->32 i)
   (band i #xFFFFFFFF))

; binary cycle rotate left
(define (rol bits x)
   (->32
      (bor
         (<< x bits)
         (>> x (- 32 bits)))))

(define (word->list x)
   (list
      (band (>> x 24) #xFF)
      (band (>> x 16) #xFF)
      (band (>> x  8) #xFF)
      (band (>> x  0) #xFF)))

(define (message->words message)
   (let cycle ((W
               (let loop ((t (iota 0 1 16)))
                  (if (null? t)
                     null
                  (let*((p (* (car t) 4)))
                     (cons (OR
                              (<< (string-ref message (+ p 0)) 24)
                              (<< (string-ref message (+ p 1)) 16)
                              (<< (string-ref message (+ p 2))  8)
                              (<< (string-ref message (+ p 3))  0))
                           (loop (cdr t)))))))
               (t 16))
      (if (eq? t 80)
         W
         (cycle (append W (list
            (XOR
               (rol 1 (list-ref W (- t 3)))
               (rol 1 (list-ref W (- t 8)))
               (rol 1 (list-ref W (- t 14)))
               (rol 1 (list-ref W (- t 16))))))
            (+ t 1)))))

(define (sha1:digest message)
   (let*((h0 #x67452301)
         (h1 #xEFCDAB89)
         (h2 #x98BADCFE)
         (h3 #x10325476)
         (h4 #xC3D2E1F0)
         (K '(#x5A827999 #x6ED9EBA1 #x8F1BBCDC #xCA62C1D6))
         (padded-message (sha1-pad-message message))
         (n (/ (string-length padded-message) 64)))

      (let main ((i 0)
                 (A h0) (B h1) (C h2) (D h3) (E h4))
         (if (= i n)
            (fold append null
               (list (word->list A) (word->list B) (word->list C) (word->list D) (word->list E)))
            (let*((message (substring padded-message (* i 64) (+ (* i 64) 64)))
                  (W (message->words message)))
               (let*((a b c d e ; round 1
                        (let loop ((a A) (b B) (c C) (d D) (e E) (t 0))
                           (if (< t 20)
                              (loop (->32
                                          (+ (rol 5 a)
                                             (OR (band b c) (band (NOT b) d))
                                             e
                                             (list-ref W t)
                                             (list-ref K 0)))
                                    a
                                    (rol 30 b)
                                    c
                                    d
                                    (+ t 1))
                              (values a b c d e))))
                     (a b c d e ; round 2
                        (let loop ((a a) (b b) (c c) (d d) (e e) (t 20))
                           (if (< t 40)
                              (loop (->32
                                          (+ (rol 5 a)
                                             (XOR b c d)
                                             e
                                             (list-ref W t)
                                             (list-ref K 1)))
                                    a
                                    (rol 30 b)
                                    c
                                    d
                                    (+ t 1))
                              (values a b c d e))))
                     (a b c d e ; round 3
                        (let loop ((a a) (b b) (c c) (d d) (e e) (t 40))
                           (if (< t 60)
                              (loop (->32
                                          (+ (rol 5 a)
                                             (OR (band b c) (band b d) (band c d))
                                             e
                                             (list-ref W t)
                                             (list-ref K 2)))
                                    a
                                    (rol 30 b)
                                    c
                                    d
                                    (+ t 1))
                              (values a b c d e))))
                     (a b c d e ; round 4
                        (let loop ((a a) (b b) (c c) (d d) (e e) (t 60))
                           (if (< t 80)
                              (loop (->32
                                          (+ (rol 5 a)
                                             (XOR b c d)
                                             e
                                             (list-ref W t)
                                             (list-ref K 3)))
                                    a
                                    (rol 30 b)
                                    c
                                    d
                                    (+ t 1))
                              (values a b c d e)))))

                  (main (+ i 1)
                     (->32 (+ A a))
                     (->32 (+ B b))
                     (->32 (+ C c))
                     (->32 (+ D d))
                     (->32 (+ E e)))))))))

```


```scheme

(define (->string value)
   (runes->string
   (let ((L "0123456789abcdef"))
   (let loop ((v value))
      (if (null? v) null
      (cons
         (string-ref L (>> (car v) 4))
         (cons
         (string-ref L (band (car v) #xF))
            (loop (cdr v)))))))))

(print (->string (sha1:digest "Rosetta Code")))
> 48c98f7e5a6e736d790ab740dfc3f51a61abe2b5
(print (->string (sha1:digest "")))
> da39a3ee5e6b4b0d3255bfef95601890afd80709

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "msgdigest.s7i";

const proc: main is func
  begin
    writeln(hex(sha1("Rosetta Code")));
  end func;
```


```txt

48c98f7e5a6e736d790ab740dfc3f51a61abe2b5

```



## Sidef


```ruby
var sha = frequire('Digest::SHA');
say sha.sha1_hex('Rosetta Code');
```

```txt
48c98f7e5a6e736d790ab740dfc3f51a61abe2b5
```



## Smalltalk

```smalltalk
PackageLoader fileInPackage: 'Digest'.
(SHA1 hexDigestOf: 'Rosetta Code') displayNl.
```

```smalltalk
(SHA1Stream hashValueOf:'Rosetta Code')
```



## Tcl

```tcl
package require sha1
puts [sha1::sha1 "Rosetta Code"]
```

```txt
48c98f7e5a6e736d790ab740dfc3f51a61abe2b5
```

It should be noted that the <code>sha1</code> package is actually a façade that uses an efficient implementation in [[C]] if one is available, or a pure Tcl version otherwise.


## UNIX Shell

```bash
$ echo -n 'ASCII string' | sha1
9e9aeefe5563845ec5c42c5630842048c0fc261b
```


```bash
$ echo -n 'ASCII string' | openssl sha1 | sed 's/.*= //'
9e9aeefe5563845ec5c42c5630842048c0fc261b
```



## zkl

Using zklMsgHash so. Can return the hash as a string of hex digits or bytes, can hash the hash N times.

```zkl
$ zkl // run the REPL
zkl: var MsgHash=Import("zklMsgHash")
MsgHash
zkl: MsgHash.SHA1("Rosetta Code")
48c98f7e5a6e736d790ab740dfc3f51a61abe2b5

zkl: var hash=MsgHash.SHA1("Rosetta Code",1,False)  // hash once, return hash as bytes
Data(20)
zkl:  hash.bytes()
L(72,201,143,126,90,110,115,109,121,10,183,64,223,195,245,26,97,171,226,181)
zkl: hash.bytes().apply("toString",16).concat()
48c98f7e5a6e736d79ab740dfc3f51a61abe2b5

zkl: MsgHash.SHA1("a"*1000,1000); // hash 1000 a's 1000 times
34aa973cd4c4daa4f61eeb2bdbad27316534016f
```



