+++
title = "Bitcoin/address validation"
description = ""
date = 2019-06-29T14:03:52Z
aliases = []
[extra]
id = 12631
[taxonomies]
categories = []
tags = []
+++

{{alertbox|#ffff70|'''<big>Warning:</big>''' Many of these snippets are [[{{TALKPAGENAME}}#C-based_code_.28and_possibly_others.29_improperly_validates|incomplete]]. It is recommended that you use an established [https://en.bitcoin.it/wiki/Software#Libraries library] for any projects that are likely to see external use}}
{{task}}
[[Category:Checksums]]
{{omit from|Brlcad}}
{{omit from|GUISS}}


;Task:
Write a program that takes a [[wp:bitcoin|bitcoin address]] as argument, 
and checks whether or not this address is valid.

A bitcoin address uses a base58 encoding, which uses an alphabet of the characters 0 .. 9, A ..Z, a .. z, but without the four characters:
:::*   0   zero
:::*   O   uppercase oh
:::*   I   uppercase eye
:::*   l   lowercase ell


With this encoding, a bitcoin address encodes 25 bytes:
* the first byte is the version number, which will be zero for this task ;
* the next twenty bytes are a [[RIPEMD-160]] digest, but you don't have to know that for this task:  you can consider them a pure arbitrary data ;
* the last four bytes are a checksum check.  They are the first four bytes of a double [[SHA-256]] digest of the previous 21 bytes.


To check the bitcoin address, you must read the first twenty-one bytes, compute the checksum, and check that it corresponds to the last four bytes.

The program can either return a boolean value or throw an exception when not valid.

You can use a digest library for [[SHA-256]].


;Example of a bitcoin address:
<big>
 1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i
</big>

It doesn't belong to anyone and is part of the test suite of the bitcoin software.   

You can change a few characters in this string and check that it'll fail the test.





## Ada


```ada
with Ada.Exceptions, Interfaces;
with Ada.Streams;
use Ada.Exceptions, Interfaces;
use Ada.Streams;

package Bitcoin is
    subtype BT_Raw_Addr is Stream_Element_Array(1..25);
    subtype BT_Checksum is Stream_Element_Array(1..4);
    subtype BT_Addr is String(1..34);
    subtype Sha256String is String(1..64);
    Invalid_Address_Error : Exception;

    function Double_Sha256(S : Stream_Element_Array) return BT_Checksum;
    function Is_Valid(A : BT_Raw_Addr) return Boolean;
    procedure Base58_Decode(S : BT_Addr; A : out BT_Raw_Addr) ;
private
    Base58 : constant String := "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";
    function Hex_Val (C, C2 : Character) return Stream_Element;
end Bitcoin;


with GNAT.SHA256, Ada.Strings.Fixed;
use  GNAT.SHA256, Ada.Strings.Fixed;

package body Bitcoin is

function Hex_Val (C, C2 : Character) return Stream_Element is
    subtype Nibble is Integer range 0..15;
    HEX : array (0..255) of Nibble := (
          48=>0, 49=>1, 50=>2, 51=>3, 52=>4, 53=>5, 54=>6, 55=>7, 56=>8, 57=>9
        , 65=>10, 66=>11, 67=>12, 68 =>13, 69 =>14, 70 =>15
        , 97=>10, 98=>11, 99=>12, 100=>13, 101=>14, 102=>15
        , Others=>0
    );
begin
    return Stream_Element(HEX(Character'Pos(C)) * 16 + HEX(Character'Pos(C2)));
end Hex_Val;

function Double_Sha256(S : Stream_Element_Array) return BT_Checksum is
    Ctx  : Context := Initial_Context;
    D : Message_Digest;
    S2 : Stream_Element_Array(1..32);
    Ctx2 : Context := Initial_Context;
    C : BT_Checksum;
begin
    Update(Ctx, S);
    D := Digest(Ctx);
    for I in S2'Range loop
        S2(I) := Hex_Val(D(Integer(I)*2-1), D(Integer(I)*2));
    end loop;
    Update(Ctx2, S2);
    D := Digest(Ctx2);
    for I in C'Range loop
        C(I) := Hex_Val(D(Integer(I)*2-1), D(Integer(I)*2));
    end loop;
    return C;

end Double_Sha256;


--------------------------------------------------------------------------------
-- Summary of Base58:                                                         --
-- We decode S into a 200 bit unsigned integer.                               --
-- We could use a BigNum library, but choose to go without.                   --
--------------------------------------------------------------------------------
procedure Base58_Decode(S : BT_Addr; A : out BT_Raw_Addr) is
begin
    A := (Others => 0);
    for I in S'Range loop
        declare
            P : Natural := Index(Base58, String(S(I..I)));
            C : Natural;
        begin
            if P = 0 then
                raise Invalid_Address_Error;
            end if;
            C := P - 1;
            for J in reverse A'Range loop
                C    := C + Natural(A(J)) * 58;
                A(J) := Stream_Element(Unsigned_32(C) and 255);         -- 0x00FF
                C    := Natural(Shift_Right(Unsigned_32(C),8) and 255); -- 0xFF00
            end loop;
            if C /= 0 then
                raise Invalid_Address_Error;
            end if;
        end;
    end loop;
end Base58_Decode;


function Is_Valid(A : BT_Raw_Addr) return Boolean is
begin
    return A(1) = 0 and A(22..25) = Double_Sha256(A(1..21));
end Is_Valid;


end Bitcoin;

with Ada.Text_IO, Bitcoin;
use Ada.Text_IO, Bitcoin;

procedure Bitcoin_Addr_Validate is
begin
    declare
        BTs : array (positive range <>) of BT_Addr := (
              "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"  -- VALID
            , "1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9"  -- VALID
            , "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62X"  -- checksum changed, original data.
            , "1ANNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"  -- data changed, original checksum.
            , "1A Na15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"  -- invalid chars
        );
    begin
        for I in Bts'Range loop
            declare
                A : BT_Raw_Addr;
                Valid : Boolean;
            begin
                Put(BTs(I) & " validity: ");
                Base58_Decode(BTs(I), A);
                Valid := Is_Valid(A);
                Put_Line(Boolean'Image(Valid));
            exception
                when E : Invalid_Address_Error  =>
                    Put_Line ("*** Error: Invalid BT address.");
            end;
        end loop;
    end;
end Bitcoin_Addr_Validate;

```


{{out}}

```txt
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i validity: TRUE
1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9 validity: TRUE
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62X validity: FALSE
1ANNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i validity: FALSE
1A Na15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i validity: *** Error: Invalid BT address.

```



## C


```c>#include <stdio.h

#include <string.h>
#include <openssl/sha.h>

const char *coin_err;
#define bail(s) { coin_err = s; return 0; }

int unbase58(const char *s, unsigned char *out) {
	static const char *tmpl = "123456789"
		"ABCDEFGHJKLMNPQRSTUVWXYZ"
		"abcdefghijkmnopqrstuvwxyz";
	int i, j, c;
	const char *p;

	memset(out, 0, 25);
	for (i = 0; s[i]; i++) {
		if (!(p = strchr(tmpl, s[i])))
			bail("bad char");

		c = p - tmpl;
		for (j = 25; j--; ) {
			c += 58 * out[j];
			out[j] = c % 256;
			c /= 256;
		}

		if (c) bail("address too long");
	}

	return 1;
}

int valid(const char *s) {
	unsigned char dec[32], d1[SHA256_DIGEST_LENGTH], d2[SHA256_DIGEST_LENGTH];

	coin_err = "";
	if (!unbase58(s, dec)) return 0;

	SHA256(SHA256(dec, 21, d1), SHA256_DIGEST_LENGTH, d2);

	if (memcmp(dec + 21, d2, 4))
		bail("bad digest");

	return 1;
}

int main (void) {
	const char *s[] = {
		"1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9",
		"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i",
		"1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nJ9",
		"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I",
		0 };
	int i;
	for (i = 0; s[i]; i++) {
		int status = valid(s[i]);
		printf("%s: %s\n", s[i], status ? "Ok" : coin_err);
	}

	return 0;
}
```

Compile with -lcrypto
{{out}}

```txt

1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9: Ok
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i: Ok
1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nJ9: bad digest
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I: bad char

```



## C#

This requires [https://www.nuget.org/packages/NUnit/ NUnit package] to compile.

```csharp

using System;
using System.Linq;
using System.Security.Cryptography;
using NUnit.Framework;

namespace BitcoinValidator
{
    public class ValidateTest
    {
        [TestCase]
        public void ValidateBitcoinAddressTest()
        {
            Assert.IsTrue(ValidateBitcoinAddress("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i")); // VALID
            Assert.IsTrue(ValidateBitcoinAddress("1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9")); // VALID
            Assert.Throws<Exception>(() => ValidateBitcoinAddress("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62X")); // checksum changed, original data
            Assert.Throws<Exception>(() => ValidateBitcoinAddress("1ANNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i")); // data changed, original checksum
            Assert.Throws<Exception>(() => ValidateBitcoinAddress("1A Na15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i")); // invalid chars
            Assert.Throws<Exception>(() => ValidateBitcoinAddress("BZbvjr")); // checksum is fine, address too short
        }

        public static bool ValidateBitcoinAddress(string address)
        {
            if (address.Length < 26 || address.Length > 35) throw new Exception("wrong length");
            var decoded = DecodeBase58(address);
            var d1 = Hash(decoded.SubArray(0, 21));
            var d2 = Hash(d1);
            if (!decoded.SubArray(21, 4).SequenceEqual(d2.SubArray(0, 4))) throw new Exception("bad digest");
            return true;
        }

        const string Alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";
        const int Size = 25;

        private static byte[] DecodeBase58(string input)
        {
            var output = new byte[Size];
            foreach (var t in input)
            {
                var p = Alphabet.IndexOf(t);
                if (p == -1) throw new Exception("invalid character found");
                var j = Size;
                while (--j > 0)
                {
                    p += 58 * output[j];
                    output[j] = (byte)(p % 256);
                    p /= 256;
                }
                if (p != 0) throw new Exception("address too long");
            }
            return output;
        }

        private static byte[] Hash(byte[] bytes)
        {
            var hasher = new SHA256Managed();
            return hasher.ComputeHash(bytes);
        }
    }

    public static class ArrayExtensions
    {
        public static T[] SubArray<T>(this T[] data, int index, int length)
        {
            var result = new T[length];
            Array.Copy(data, index, result, 0, length);
            return result;
        }
    }
}

```



## D

This requires the D module from the SHA-256 Task.
{{trans|Go}}

```d
import std.stdio, std.algorithm, std.array, std.string, sha_256_2;

struct A25 {
    // Type for a 25 ubyte (not base58 encoded) bitcoin address.
    ubyte[25] enc;

    ubyte bitcoinVersion() const pure nothrow @safe @nogc {
        return enc[0];
    }

    ubyte[4] embeddedChecksum() return const pure nothrow @safe @nogc {
        return enc[$ - 4 .. $];
    }

    /** Computes a double sha256 hash of the first 21 bytes of
    the address. Returns the full 32 ubyte sha256 hash. */
    ubyte[32] doubleSHA256() const pure nothrow @nogc {
        return SHA256.digest(SHA256.digest(enc[0 .. 21]));
    }

    /** Returns a four ubyte checksum computed from the first 21
    bytes of the address. */
    ubyte[4] computeChecksum() const pure nothrow @nogc {
        return doubleSHA256[0 .. 4];
    }

    /** Takes a base58 encoded address and decodes it into the
    receiver. Errors are returned if the argument is not valid base58
    or if the decoded value does not fit in the 25 ubyte address.
    The address is not otherwise checked for validity. */
    string set58(in ubyte[] s) pure nothrow @safe @nogc {
        static immutable digits =
        "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
        .representation;
        static assert(digits.length == 58);

        foreach (immutable char s1; s) {
            immutable c = digits.countUntil(s1);
            if (c < 0)
                return "found a bad char in the Bitcoin address.";

            // Currently the D type system can't see c as nonegative.
            uint uc = (c < 0) ? 0 : c;

            foreach_reverse (ref aj; enc) {
                uc += digits.length * aj;
                aj = uc % 256;
                uc /= 256;
            }
            if (uc > 0)
                return "too long Bitcoin address.";
        }

        return null;
    }
}

/** Validates a base58 encoded bitcoin address.  An address is valid
if it can be decoded into a 25 ubyte address, the Version number is 0,
and the checksum validates.  Return value ok will be true for valid
addresses.  If ok is false, the address is invalid and the error value
may indicate why. */
string isValidA58(in ubyte[] a58) pure nothrow @nogc {
    A25 a;
    immutable err = a.set58(a58);
    if (!err.empty)
        return err;
    if (a.bitcoinVersion != 0)
        return "not Bitcoin version 0.";
    return (a.embeddedChecksum == a.computeChecksum) ? null :
           "checksums don't match.";
}

void main() {
    immutable tests = ["1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i",
                       "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62j",
                       "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62!",
                       "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62iz",
                       "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62izz"];

    foreach (immutable test; tests) {
        immutable err = test.representation.isValidA58;
        writefln(`"%s": %s`, test, err.empty ? "OK." : err);
    }
}
```

{{out}}

```txt
"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i": OK.
"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62j": checksums don't match.
"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62!": found a bad char in the Bitcoin address.
"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62iz": not Bitcoin version 0.
"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62izz": too long Bitcoin address.
```



## Delphi

This requires [http://www.cityinthesky.co.uk/opensource/DCPcrypt/ DCPcrypt library] to compile.

```delphi

uses
  DCPsha256;

type
  TByteArray = array of Byte;

function HashSHA256(const Input: TByteArray): TByteArray;
var
  Hasher: TDCP_sha256;
begin
  Hasher := TDCP_sha256.Create(nil);
  try
    Hasher.Init;
    Hasher.Update(Input[0], Length(Input));
    SetLength(Result, Hasher.HashSize div 8);
    Hasher.Final(Result[0]);
  finally
    Hasher.Free;
  end;
end;

function DecodeBase58(const Input: string): TByteArray;
const
  Size = 25;
  Alphabet = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';
var
  C: Char;
  I, J: Integer;
begin
  SetLength(Result, Size);

  for C in Input do
  begin
    I := Pos(C, Alphabet) - 1;

    if I = -1 then
      raise Exception.CreateFmt('Invalid character found: %s', [C]);

    for J := High(Result) downto 0 do
    begin
      I := I + (58 * Result[J]);
      Result[J] := I mod 256;
      I := I div 256;
    end;

    if I <> 0 then
      raise Exception.Create('Address too long');
  end;
end;

procedure ValidateBitcoinAddress(const Address: string);
var
  Hashed: TByteArray;
  Decoded: TByteArray;
begin
  if (Length(Address) < 26) or (Length(Address) > 35) then
    raise Exception.Create('Wrong length');

  Decoded := DecodeBase58(Address);
  Hashed := HashSHA256(HashSHA256(Copy(Decoded, 0, 21)));

  if not CompareMem(@Decoded[21], @Hashed[0], 4) then
    raise Exception.Create('Bad digest');
end;

```



## Erlang

Using base58 module from http://github.com/titan098/erl-base58.git.


```Erlang

-module( bitcoin_address ).

-export( [task/0, validate/1] ).

task() ->
	io:fwrite( "Validate ~p~n", ["1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"] ),
	io:fwrite( "~p~n", [validate("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i")] ),
	io:fwrite( "Validate ~p~n", ["1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW622"] ),
	io:fwrite( "~p~n", [validate("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW622")] ).

validate( String ) ->
	{length_25, <<Address:21/binary, Checksum:4/binary>>} = {length_25, base58:base58_to_binary( String )},
	<<Version:1/binary, _/binary>> = Address,
	{version_0, <<0>>} = {version_0, Version},
	<<Four_bytes:4/binary, _T/binary>> = crypto:hash( sha256, crypto:hash(sha256, Address) ),
	{checksum, Checksum} = {checksum, Four_bytes},
	ok.

```

{{out}}

```txt

17>  bitcoin_address:task().
Validate "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"
ok
Validate "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW622"
** exception error: no match of right hand side value {checksum,<<"ÀF²ÿ">>}
     in function  bitcoin_address:validate/1 (src/bitcoin_address.erl, line 16)
     in call from bitcoin_address:task/0 (src/bitcoin_address.erl, line 9)

```


## Factor


```factor
USING: byte-arrays checksums checksums.sha io.binary kernel math
math.parser sequences ;
IN: rosetta-code.bitcoin.validation

CONSTANT: ALPHABET "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

: base58>bigint ( str -- n )
  [ ALPHABET index ]
  [ [ 58 * ] [ + ] bi* ] map-reduce ;

: base58> ( str -- bytes ) base58>bigint 25 >be ;

: btc-checksum ( bytes -- checksum-bytes )
  21 head 2 [ sha-256 checksum-bytes ] times 4 head ;

: btc-valid? ( str -- ? ) base58> [ btc-checksum ] [ 4 tail* ] bi = ;

```


{{out}}

```txt
"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i" btc-valid? . ! t, VALID
"1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9" btc-valid? . ! t, VALID
"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62X" btc-valid? . ! f, checksum changed, original data.
"1ANNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i" btc-valid? . ! f, data changed, original checksum.

```



## FreeBASIC


```freebasic
' version 05-04-2017
' compile with: fbc -s console

' function adapted from the SHA-256 task
Function SHA_256(test_str As String, bitcoin As ULong = 0) As String

    #Macro Ch (x, y, z)
        (((x) And (y)) Xor ((Not (x)) And z))
    #EndMacro

    #Macro Maj (x, y, z)
        (((x) And (y)) Xor ((x) And (z)) Xor ((y) And (z)))
    #EndMacro

    #Macro sigma0 (x)
        (((x) Shr 2 Or (x) Shl 30) Xor ((x) Shr 13 Or (x) Shl 19) Xor ((x) Shr 22 Or (x) Shl 10))
    #EndMacro

    #Macro sigma1 (x)
        (((x) Shr 6 Or (x) Shl 26) Xor ((x) Shr 11 Or (x) Shl 21) Xor ((x) Shr 25 Or (x) Shl 7))
    #EndMacro

    #Macro sigma2 (x)
        (((x) Shr 7 Or (x) Shl 25) Xor ((x) Shr 18 Or (x) Shl 14) Xor ((x) Shr 3))
    #EndMacro

    #Macro sigma3 (x)
        (((x) Shr 17 Or (x) Shl 15) Xor ((x) Shr 19 Or (x) Shl 13) Xor ((x) Shr 10))
    #EndMacro

    Dim As String message = test_str   ' strings are passed as ByRef's

    Dim As Long i, j
    Dim As UByte Ptr ww1
    Dim As UInteger<32> Ptr ww4

    Do
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

        'table of constants
        Dim As UInteger<32> K(0 To ...) = _
        { &H428a2f98, &H71374491, &Hb5c0fbcf, &He9b5dba5, &H3956c25b, &H59f111f1, _
          &H923f82a4, &Hab1c5ed5, &Hd807aa98, &H12835b01, &H243185be, &H550c7dc3, _
          &H72be5d74, &H80deb1fe, &H9bdc06a7, &Hc19bf174, &He49b69c1, &Hefbe4786, _
          &H0fc19dc6, &H240ca1cc, &H2de92c6f, &H4a7484aa, &H5cb0a9dc, &H76f988da, _
          &H983e5152, &Ha831c66d, &Hb00327c8, &Hbf597fc7, &Hc6e00bf3, &Hd5a79147, _
          &H06ca6351, &H14292967, &H27b70a85, &H2e1b2138, &H4d2c6dfc, &H53380d13, _
          &H650a7354, &H766a0abb, &H81c2c92e, &H92722c85, &Ha2bfe8a1, &Ha81a664b, _
          &Hc24b8b70, &Hc76c51a3, &Hd192e819, &Hd6990624, &Hf40e3585, &H106aa070, _
          &H19a4c116, &H1e376c08, &H2748774c, &H34b0bcb5, &H391c0cb3, &H4ed8aa4a, _
          &H5b9cca4f, &H682e6ff3, &H748f82ee, &H78a5636f, &H84c87814, &H8cc70208, _
          &H90befffa, &Ha4506ceb, &Hbef9a3f7, &Hc67178f2 }

        Dim As UInteger<32> h0 = &H6a09e667
        Dim As UInteger<32> h1 = &Hbb67ae85
        Dim As UInteger<32> h2 = &H3c6ef372
        Dim As UInteger<32> h3 = &Ha54ff53a
        Dim As UInteger<32> h4 = &H510e527f
        Dim As UInteger<32> h5 = &H9b05688c
        Dim As UInteger<32> h6 = &H1f83d9ab
        Dim As UInteger<32> h7 = &H5be0cd19
        Dim As UInteger<32> a, b, c, d, e, f, g, h
        Dim As UInteger<32> t1, t2, w(0 To 63)

        For j = 0 To (l1 -1) \ 64 ' split into block of 64 bytes
            ww1 = Cast(UByte Ptr, @message[j * 64])
            ww4 = Cast(UInteger<32> Ptr, @message[j * 64])

            For i = 0 To 60 Step 4  'little endian -> big endian
                Swap ww1[i   ], ww1[i +3]
                Swap ww1[i +1], ww1[i +2]
            Next

            For i = 0 To 15    ' copy the 16 32bit block into the array
                W(i) = ww4[i]
            Next

            For i = 16 To 63   ' fill the rest of the array
                w(i) = sigma3(W(i -2)) + W(i -7) + sigma2(W(i -15)) + W(i -16)
            Next

            a = h0 : b = h1 : c = h2 : d = h3 : e = h4 : f = h5 : g = h6 : h = h7

            For i = 0 To 63
                t1 = h + sigma1(e) + Ch(e, f, g) + K(i) + W(i)
                t2 = sigma0(a) + Maj(a, b, c)
                h = g : g = f : f = e
                e = d + t1
                d = c : c = b : b = a
                a = t1 + t2
            Next

            h0 += a : h1 += b : h2 += c : h3 += d
            h4 += e : h5 += f : h6 += g : h7 += h

        Next j

        Dim As String answer  = Hex(h0, 8) + Hex(h1, 8) + Hex(h2, 8) + Hex(h3, 8) _
        + Hex(h4, 8) + Hex(h5, 8) + Hex(h6, 8) + Hex(h7, 8)

        If bitcoin = 0 Then
            Return LCase(answer)
        Else 'conver hex value's to integer value's
            message = String(32,0)
            For i = 0 To 31
                message[i] = Val("&h" + Mid(answer, i * 2 + 1, 2))
            Next
            bitcoin = 0
        End If

    Loop

End Function

Function conv_base58(bitcoin_address As String) As String

    Dim As String base58 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    Dim As String tmp = String(24, 0)
    Dim As Long x, y, z

    For x = 1 To Len(bitcoin_address) -1
        z = InStr(base58, Chr(bitcoin_address[x])) -1
        If z = -1 Then
            Print " bitcoin address contains illegal character"
            Return ""
        End If
        For y = 23 To 0 Step -1
            z = z + tmp[y] * 58
            tmp[y] = z And 255       ' test_str[y] = z Mod 256
            z Shr= 8                 ' z \= 256
        Next
        If z <> 0 Then
            Print " bitcoin address is to long"
            Return ""
        End If
    Next

    z = InStr(base58, Chr(bitcoin_address[0])) -1
    Return Chr(z) + tmp

End Function

' ------=< MAIN >=------

Data "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i" ' original
Data "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62j" ' checksum changed
Data "1NAGa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i" ' address changed
Data "0AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i" ' only 1 or 3 as first char. allowed
Data "1AGNa15ZQXAZUgFlqJ2i7Z2DPU2J6hW62i" ' illegal character in address

Dim As String tmp, result, checksum, bitcoin_address
Dim As Long i, j

For i = 1 To 5

    Read bitcoin_address
    Print "Bitcoin address: "; bitcoin_address;
    tmp = Left(bitcoin_address,1)

    If tmp <> "1" And tmp <> "3" Then
        Print " first character is not 1 or 3"
        Continue For
    End If

    ' convert bitcoinaddress
    tmp = conv_base58(bitcoin_address)
    If tmp = "" Then Continue For
    ' get the checksum, last 4 digits
    For j As Long = 21 To 24
        checksum = checksum + LCase(Hex(tmp[j], 2))
    Next

    ' send the first 21 characters to the SHA 256 routine
    result = SHA_256(Left(tmp, 21), 2)
    result = Left(result, 8) ' get the checksum (the first 8 digits (hex))
    If checksum = result Then          ' test the found checksum against
        Print " is valid"              ' the one from the address
    Else
        Print " is not valid, checksum fails"
    End If

Next

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
Bitcoin address: 1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i is valid
Bitcoin address: 1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62j is not valid, checksum fails
Bitcoin address: 1NAGa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i is not valid, checksum fails
Bitcoin address: 0AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i first character is not 1 or 3
Bitcoin address: 1AGNa15ZQXAZUgFlqJ2i7Z2DPU2J6hW62i bitcoin address contains illegal character
```



## Go

{{trans|C}}

```go
package main

import (
    "bytes"
    "crypto/sha256"
    "errors"
    "os"
)

// With at least one other bitcoin RC task, this source is styled more like
// a package to show how functions of the two tasks might be combined into
// a single package.  It turns out there's not really that much shared code,
// just the A25 type and doubleSHA256 method, but it's enough to suggest how
// the code might be organized.  Types, methods, and functions are capitalized
// where they might be exported from a package.

// A25 is a type for a 25 byte (not base58 encoded) bitcoin address.
type A25 [25]byte
    
func (a *A25) Version() byte {
    return a[0]
}
    
func (a *A25) EmbeddedChecksum() (c [4]byte) {
    copy(c[:], a[21:])
    return
}

// DoubleSHA256 computes a double sha256 hash of the first 21 bytes of the
// address.  This is the one function shared with the other bitcoin RC task.
// Returned is the full 32 byte sha256 hash.  (The bitcoin checksum will be
// the first four bytes of the slice.)
func (a *A25) doubleSHA256() []byte {
    h := sha256.New()
    h.Write(a[:21])
    d := h.Sum([]byte{})
    h = sha256.New()
    h.Write(d)
    return h.Sum(d[:0])
}

// ComputeChecksum returns a four byte checksum computed from the first 21
// bytes of the address.  The embedded checksum is not updated.
func (a *A25) ComputeChecksum() (c [4]byte) {
    copy(c[:], a.doubleSHA256())
    return
}/* {{header|Go}} */ 

// Tmpl and Set58 are adapted from the C solution.
// Go has big integers but this techinique seems better.
var tmpl = []byte("123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")

// Set58 takes a base58 encoded address and decodes it into the receiver.
// Errors are returned if the argument is not valid base58 or if the decoded
// value does not fit in the 25 byte address.  The address is not otherwise
// checked for validity.
func (a *A25) Set58(s []byte) error {
    for _, s1 := range s {
        c := bytes.IndexByte(tmpl, s1)
        if c < 0 {
            return errors.New("bad char")
        }
        for j := 24; j >= 0; j-- {
            c += 58 * int(a[j])
            a[j] = byte(c % 256)
            c /= 256
        }
        if c > 0 {
            return errors.New("too long")
        }
    }
    return nil
}

// ValidA58 validates a base58 encoded bitcoin address.  An address is valid
// if it can be decoded into a 25 byte address, the version number is 0,
// and the checksum validates.  Return value ok will be true for valid
// addresses.  If ok is false, the address is invalid and the error value
// may indicate why.
func ValidA58(a58 []byte) (ok bool, err error) {
    var a A25
    if err := a.Set58(a58); err != nil {
        return false, err
    }
    if a.Version() != 0 {
        return false, errors.New("not version 0")
    }
    return a.EmbeddedChecksum() == a.ComputeChecksum(), nil
}

// Program returns exit code 0 with valid address and produces no output.
// Otherwise exit code is 1 and a message is written to stderr.
func main() {
    if len(os.Args) != 2 {
        errorExit("Usage: valid <base58 address>")
    }
    switch ok, err := ValidA58([]byte(os.Args[1])); {
    case ok:
    case err == nil:
        errorExit("Invalid")
    default:
        errorExit(err.Error())
    }
}

func errorExit(m string) {
    os.Stderr.WriteString(m + "\n")
    os.Exit(1)
}
```

{{out}}
Command line usage examples showing program exit status.

```txt

> valid ; echo $status
Usage: valid <base58 address>
1
> valid 1 1 ; echo $status
Usage: valid <base58 address>
1
> valid 1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i ; echo $status
0
> valid 1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62j ; echo $status
Invalid
1
> valid 1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62! ; echo $status
bad char
1
> valid 1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62iz ; echo $status
not version 0
1
> valid 1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62izz ; echo $status
too long
1

```


## Haskell


```haskell
import           Control.Monad      (when)
import           Data.List          (elemIndex)
import           Data.Monoid        ((<>))
import qualified Data.ByteString    as BS
import           Data.ByteString    (ByteString)

import           Crypto.Hash.SHA256 (hash)  -- from package cryptohash

-- Convert from base58 encoded value to Integer
decode58 :: String -> Maybe Integer
decode58 = fmap combine . traverse parseDigit
  where
    combine = foldl (\acc digit -> 58 * acc + digit) 0  -- should be foldl', but this trips up the highlighting
    parseDigit char = toInteger <$> elemIndex char c58
    c58 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

-- Convert from base58 encoded value to bytes
toBytes :: Integer -> ByteString
toBytes = BS.reverse . BS.pack . map (fromIntegral . (`mod` 256)) . takeWhile (> 0) . iterate (`div` 256)

-- Check if the hash of the first 21 (padded) bytes matches the last 4 bytes
checksumValid :: ByteString -> Bool
checksumValid address =
  let (value, checksum) = BS.splitAt 21 $ leftPad address
  in and $ BS.zipWith (==) checksum $ hash $ hash $ value
  where
    leftPad bs = BS.replicate (25 - BS.length bs) 0 <> bs

-- utility
withError :: e -> Maybe a -> Either e a
withError e = maybe (Left e) Right

-- Check validity of base58 encoded bitcoin address.
-- Result is either an error string (Left) or unit (Right ()).
validityCheck :: String -> Either String ()
validityCheck encoded = do
  num <- withError "Invalid base 58 encoding" $ decode58 encoded
  let address = toBytes num
  when (BS.length address > 25) $ Left "Address length exceeds 25 bytes"
  when (BS.length address <  4) $ Left "Address length less than 4 bytes"
  when (not $ checksumValid address) $ Left "Invalid checksum"

-- Run one validity check and display results.
validate :: String -> IO ()
validate encodedAddress = do
  let result = either show (const "Valid") $ validityCheck encodedAddress
  putStrLn $ show encodedAddress ++ " -> " ++ result

-- Run some validity check tests.
main :: IO ()
main  = do
  validate "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"     -- VALID
  validate "1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9"     -- VALID
  validate "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62X"     -- checksum changed, original data.
  validate "1ANNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"     -- data changed, original checksum.
  validate "1A Na15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"     -- invalid chars
  validate "1ANa55215ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"   -- too long
  validate "i55j"                                   -- too short 

```

{{out}}
<pre style="font-size:80%">"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i" -> Valid
"1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9" -> Valid
"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62X" -> Invalid
"1ANNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i" -> Invalid
"1A Na15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i" -> "Invalid base 58 encoding"
"1ANa55215ZQXAZUgFiqJ2i7Z2DPU2J6hW62i" -> "Address length exceeds 25 bytes"
"i55j" -> "Address length less than 4 bytes"
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"; data = 
 IntegerDigits[
  FromDigits[
   StringPosition[chars, #][[1]] - 1 & /@ Characters[InputString[]], 
   58], 256, 25];
data[[-4 ;;]] == 
 IntegerDigits[
   Hash[FromCharacterCode[
     IntegerDigits[Hash[FromCharacterCode[data[[;; -5]]], "SHA256"], 
      256, 32]], "SHA256"], 256, 32][[;; 4]]
```

{{in}}

```txt
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i
2AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i
```

{{out}}

```txt
True
False
```




## Java


```java
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

public class BitcoinAddressValidator {

    private static final String ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

    public static boolean validateBitcoinAddress(String addr) {
        if (addr.length() < 26 || addr.length() > 35)
            return false;
        byte[] decoded = decodeBase58To25Bytes(addr);
        if (decoded == null)
            return false;

        byte[] hash1 = sha256(Arrays.copyOfRange(decoded, 0, 21));
        byte[] hash2 = sha256(hash1);

        return Arrays.equals(Arrays.copyOfRange(hash2, 0, 4), Arrays.copyOfRange(decoded, 21, 25));
    }

    private static byte[] decodeBase58To25Bytes(String input) {
        BigInteger num = BigInteger.ZERO;
        for (char t : input.toCharArray()) {
            int p = ALPHABET.indexOf(t);
            if (p == -1)
                return null;
            num = num.multiply(BigInteger.valueOf(58)).add(BigInteger.valueOf(p));
        }

        byte[] result = new byte[25];
        byte[] numBytes = num.toByteArray();
        System.arraycopy(numBytes, 0, result, result.length - numBytes.length, numBytes.length);
        return result;
    }

    private static byte[] sha256(byte[] data) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            md.update(data);
            return md.digest();
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        }
    }

    public static void main(String[] args) {
        assertBitcoin("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i", true);
        assertBitcoin("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62j", false);
        assertBitcoin("1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9", true);
        assertBitcoin("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62X", false);
        assertBitcoin("1ANNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i", false);
        assertBitcoin("1A Na15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i", false);
        assertBitcoin("BZbvjr", false);
        assertBitcoin("i55j", false);
        assertBitcoin("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62!", false);
        assertBitcoin("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62iz", false);
        assertBitcoin("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62izz", false);
        assertBitcoin("1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nJ9", false);
        assertBitcoin("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I", false);
    }

    private static void assertBitcoin(String address, boolean expected) {
        boolean actual = validateBitcoinAddress(address);
        if (actual != expected)
            throw new AssertionError(String.format("Expected %s for %s, but got %s.", expected, address, actual));
    }
}
```



## Julia

{{works with|Julia|0.6}}
{{trans|Python}}

```julia
using SHA

bytes(n::Integer, l::Int) = collect(UInt8, (n >> 8i) & 0xFF for i in l-1:-1:0)

function decodebase58(bc::String, l::Int)
    digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    num = big(0)
    for c in bc
        num = num * 58 + search(digits, c) - 1
    end
    return bytes(num, l)
end

function checkbcaddress(addr::String)
    if !(25 ≤ length(addr) ≤ 35) return false end
    bcbytes = decodebase58(addr, 25)
    sha = sha256(sha256(bcbytes[1:end-4]))
    return bcbytes[end-3:end] == sha[1:4]
end

const addresses = Dict(
    "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i" => true,
    "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62j" => false,
    "1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9" => true,
    "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62X" => true,
    "1ANNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i" => false,
    "1A Na15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i" => false,
    "BZbvjr" => false,
    "i55j" => false,
    "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62!" => false,
    "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62iz" => false,
    "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62izz" => false,
    "1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nJ9" => false,
    "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I" => false)

for (addr, corr) in addresses
    println("Address: $addr\nExpected: $corr\tChecked: ", checkbcaddress(addr))
end
```


{{out}}

```txt
Address: 1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62X
Expected: true	Checked: false
Address: 1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i
Expected: true	Checked: true
Address: 1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62j
Expected: false	Checked: false
Address: 1ANNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i
Expected: false	Checked: false
Address: 1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9
Expected: true	Checked: true
Address: BZbvjr
Expected: false	Checked: false
Address: 1A Na15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i
Expected: false	Checked: false
Address: i55j
Expected: false	Checked: false
Address: 1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62izz
Expected: false	Checked: false
Address: 1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nJ9
Expected: false	Checked: false
Address: 1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62!
Expected: false	Checked: false
Address: 1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I
Expected: false	Checked: false
Address: 1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62iz
Expected: false	Checked: false
```



## Kotlin

{{trans|Java}}

```scala
import java.security.MessageDigest

object Bitcoin {
    private const val ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

    private fun ByteArray.contentEquals(other: ByteArray): Boolean {
        if (this.size != other.size) return false
        return (0 until this.size).none { this[it] != other[it] }
    }

    private fun decodeBase58(input: String): ByteArray? {
        val output = ByteArray(25)
        for (c in input) {
            var p = ALPHABET.indexOf(c)
            if (p == -1) return null
            for (j in 24 downTo 1) {
                p += 58 * (output[j].toInt() and 0xff)
                output[j] = (p % 256).toByte()
                p = p shr 8
            }
            if (p != 0) return null
        }
        return output
    }

    private fun sha256(data: ByteArray, start: Int, len: Int, recursion: Int): ByteArray {
        if (recursion == 0) return data
        val md = MessageDigest.getInstance("SHA-256")
        md.update(data.sliceArray(start until start + len))
        return sha256(md.digest(), 0, 32, recursion - 1)
    }

    fun validateAddress(address: String): Boolean {
        if (address.length !in 26..35) return false
        val decoded = decodeBase58(address)
        if (decoded == null) return false
        val hash = sha256(decoded, 0, 21, 2)
        return hash.sliceArray(0..3).contentEquals(decoded.sliceArray(21..24))
    }
}

fun main(args: Array<String>) {
    val addresses = arrayOf(
        "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i",
        "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62j",
        "1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9",
        "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62X",
        "1ANNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i",
        "1A Na15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i",
        "BZbvjr",
        "i55j",
        "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62!",
        "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62iz",
        "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62izz",
        "1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nJ9",
        "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I"
    )
    for (address in addresses)
        println("${address.padEnd(36)} -> ${if (Bitcoin.validateAddress(address)) "valid" else "invalid"}")
}
```


{{out}}

```txt

1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i   -> valid
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62j   -> invalid
1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9   -> valid
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62X   -> invalid
1ANNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i   -> invalid
1A Na15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i   -> invalid
BZbvjr                               -> invalid
i55j                                 -> invalid
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62!   -> invalid
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62iz  -> invalid
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62izz -> invalid
1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nJ9   -> invalid
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I   -> invalid

```



## Nim


Requires libssl library to perform SHA256.  Build with -d:ssl flag.


```nim
import algorithm

const SHA256Len = 32
const AddrLen = 25
const AddrMsgLen = 21
const AddrChecksumOffset = 21
const AddrChecksumLen = 4

proc SHA256(d: pointer, n: culong, md: pointer = nil): cstring {.cdecl, dynlib: "libssl.so", importc.}

proc decodeBase58(inStr: string, outArray: var openarray[uint8]) =
  let base = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  outArray.fill(0)

  for aChar in inStr:
    var accum = base.find(aChar)

    if accum < 0:
      raise newException(ValueError, "Invalid character: " & $aChar)

    for outIndex in countDown((AddrLen - 1), 0):
      accum += 58 * outArray[outIndex].int
      outArray[outIndex] = (accum mod 256).uint8
      accum = accum div 256

    if accum != 0:
      raise newException(ValueError, "Address string too long")

proc verifyChecksum(addrData: openarray[uint8]) : bool =
  let doubleHash = SHA256(SHA256(cast[ptr uint8](addrData), AddrMsgLen), SHA256Len)

  for ii in 0 ..< AddrChecksumLen:
    if doubleHash[ii].uint8 != addrData[AddrChecksumOffset + ii]:
      return false

  return true

proc main() =
  let
    testVectors : seq[string] = @[
      "3yQ",
      "1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9",
      "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i",
      "1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nJ9",
      "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I",
      "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62ix",
      "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62ixx",
      "17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j",
      "1badbadbadbadbadbadbadbadbadbadbad",
      "16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM",
      "1111111111111111111114oLvT2",
      "BZbvjr",
    ]

  var
    buf: array[AddrLen, uint8]
    astr: string

  for vector in testVectors:
    stdout.write(vector & " : ")
    try:
      decodeBase58(vector, buf)

      if buf[0] != 0:
        stdout.write("NG - invalid version number\n")
      elif verifyChecksum(buf):
        stdout.write("OK\n")
      else:
        stdout.write("NG - checksum invalid\n")

    except:
      stdout.write("NG - " & getCurrentExceptionMsg() & "\n")

main()

```


{{out}}

```txt

3yQ : NG - checksum invalid
1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9 : OK
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i : OK
1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nJ9 : NG - checksum invalid
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I : NG - Invalid character: I
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62ix : NG - invalid version number
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62ixx : NG - Address string too long
17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j : OK
1badbadbadbadbadbadbadbadbadbadbad : NG - invalid version number
16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM : OK
1111111111111111111114oLvT2 : OK
BZbvjr : OK

```


Note: The last two test addresses have valid checksums and conform to the specified address constraints.


=={{header|Oberon-2}}==
{{works with|oo2c}}{{libheader|Crypto}}

```oberon2

MODULE BitcoinAddress;
IMPORT 
  Object,
  NPCT:Tools,
  Crypto:SHA256,
  S := SYSTEM,
  Out;

CONST
  BASE58 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";
TYPE
  BC_RAW = ARRAY 25 OF CHAR;
  SHA256_HASH = ARRAY 32 OF CHAR;

VAR
  b58: Object.CharsLatin1;

PROCEDURE IndexOfB58Char(c: CHAR): INTEGER;
VAR
  i: INTEGER;
BEGIN
  i := 0;
  WHILE (b58[i] # 0X) & (b58[i] # c) DO INC(i) END;
  IF b58[i] = 0X THEN RETURN -1 ELSE RETURN i END
END IndexOfB58Char;

PROCEDURE DecodeB58(s [NO_COPY]: ARRAY OF CHAR;VAR out: BC_RAW): BOOLEAN;
VAR
  i,j,k: LONGINT;
  
BEGIN
  FOR i := 0 TO LEN(out) - 1 DO; out[i] := CHR(0) END;
  i := 0;
  WHILE (s[i] # 0X) DO;
    k := IndexOfB58Char(s[i]);
    IF k < 0 THEN 
      Out.String("Error: Bad base58 character");Out.Ln;
      RETURN FALSE
    END;
    FOR j := LEN(out) - 1 TO 0 BY -1 DO
       k := k + 58 * ORD(out[j]);
       out[j] := CHR(k MOD 256);
       k := k DIV 256;
    END;

    IF k # 0 THEN Out.String("Error: Address to long");Out.Ln; RETURN FALSE END;
    INC(i)
  END;
  RETURN TRUE
END DecodeB58; 

PROCEDURE Valid(s [NO_COPY]: ARRAY OF CHAR): BOOLEAN;
VAR
  dec: BC_RAW;
  d1, d2: SHA256.Hash;
  d1Str, d2Str: SHA256_HASH;
  x,y: LONGINT;
BEGIN
  Out.String(s);Out.String(" is valid? ");
  IF ~DecodeB58(s,dec) THEN RETURN FALSE END;

  d1 := SHA256.NewHash();d1.Initialize();
  d2 := SHA256.NewHash();d2.Initialize();
  d1.Update(dec,0,21);d1.GetHash(d1Str,0);
  d2.Update(d1Str,0,d1.size);d2.GetHash(d2Str,0);
  
  S.MOVE(S.ADR(dec) + 21,S.ADR(x),4);
  S.MOVE(S.ADR(d2Str),S.ADR(y),4);
  
  RETURN (x = y)
END Valid;

BEGIN
  b58 := Tools.AsString(BASE58);
 
  Out.Bool(Valid("1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9"));Out.Ln;
  Out.Bool(Valid("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"));Out.Ln;
  Out.Bool(Valid("1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nJ9"));Out.Ln;
  Out.Bool(Valid("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I"));Out.Ln
END BitcoinAddress.

```

{{out}}

```txt

1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9 is valid? TRUE
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i is valid? TRUE
1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nJ9 is valid? FALSE
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I is valid? Error: Bad base58 character
FALSE

```



## Perl


```perl
my @b58 = qw{
      1 2 3 4 5 6 7 8 9
    A B C D E F G H   J K L M N   P Q R S T U V W X Y Z
    a b c d e f g h i j k   m n o p q r s t u v w x y z
};
my %b58 = map { $b58[$_] => $_ } 0 .. 57;

sub unbase58 {
    use integer;
    my @out;
    my $azeroes = length($1) if $_[0] =~ /^(1*)/;
    for my $c ( map { $b58{$_} } $_[0] =~ /./g ) {
        for (my $j = 25; $j--; ) {
            $c += 58 * ($out[$j] // 0);
            $out[$j] = $c % 256;
            $c /= 256;
        }
    }
    my $bzeroes = length($1) if join('', @out) =~ /^(0*)/;
    die "not a 25 byte address\n" if $bzeroes != $azeroes;
    return @out;
}

sub check_bitcoin_address {
    # Does nothing if address is valid
    # dies otherwise
    use Digest::SHA qw(sha256);
    my @byte = unbase58 shift;
    die "wrong checksum\n" unless 
    (pack 'C*', @byte[21..24]) eq 
    substr sha256(sha256 pack 'C*', @byte[0..20]), 0, 4;
}
```



## Perl 6


```perl6
my $bitcoin-address = rx/
    <+alnum-[0IOl]> ** 26..*  # an address is at least 26 characters long
    <?{
        use Digest::SHA;
        .subbuf(21, 4) eqv sha256(sha256 .subbuf(0, 21)).subbuf(0, 4) given
        Blob.new: <
            1 2 3 4 5 6 7 8 9
            A B C D E F G H   J K L M N   P Q R S T U V W X Y Z
            a b c d e f g h i j k   m n o p q r s t u v w x y z
        >.pairs.invert.hash{$/.comb}
        .reduce(* * 58 + *)
        .polymod(256 xx 24)
        .reverse;
    }>
/;
 
say "Here is a bitcoin address: 1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i" ~~ $bitcoin-address;
```

{{out}}

```txt
｢1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i｣
```



## Phix


```Phix
-- demo\rosetta\bitcoin_address_validation.exw
include builtins\sha256.e

constant b58 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
string charmap = ""

function valid(string s, bool expected)
    bool res := (expected==false)
    if charmap="" then
        charmap = repeat('\0',256)
        for i=1 to length(b58) do
            charmap[b58[i]] = i
        end for
    end if
-- not at all sure about this:
--  if length(s)!=34 then
--      return {res,"bad length"}
--  end if
    if not find(s[1],"13") then
        return {res,"first character is not 1 or 3"}
    end if
    string out = repeat('\0',25)
    for i=1 to length(s) do
        integer c = charmap[s[i]]
        if c=0 then 
            return {res,"bad char"}
        end if
        c -= 1
        for j=25 to 1 by -1 do
            c += 58 * out[j];
            out[j] = and_bits(c,#FF)
            c = floor(c/#100)
        end for
        if c!=0 then
            return {res,"address too long"}
        end if
    end for
    if out[1]!='\0' then
        return {res,"not version 0"}
    end if
    if out[22..$]!=sha256(sha256(out[1..21]))[1..4] then
        return {res,"bad digest"}
    end if
    res := (expected==true)
    return {res,"OK"}
end function
 
constant tests = {{"1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9",true},  -- OK
                  {"1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nJ9",false}, -- bad digest
                  {"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i",true},  -- OK
                  {"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62j",false}, -- bad disgest
                  {"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62X",false}, -- bad digest (checksum changed, original data.)
                  {"1ANNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i",false}, -- bad digest (data changed, original checksum.)
                  {"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62iz",false}, -- not version 0
                  {"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62izz",false}, -- address too long
                  {"1BGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i",false}, -- bad digest
                  {"1A Na15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i",false}, -- bad char
                  {"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I",false}, -- bad char
                  {"1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62!",false}, -- bad char
                  {"1AGNa15ZQXAZUgFiqJ3i7Z2DPU2J6hW62i",false}, -- bad digest
                  {"1111111111111111111114oLvT2",       true},  -- OK
                  {"17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j",true},  -- OK
                  {"1badbadbadbadbadbadbadbadbadbadbad",false}, -- not version 0
                  {"BZbvjr",false}, -- first character is not 1 or 3 (checksum is fine, address too short)
                  {"i55j",false}, -- first character is not 1 or 3 (checksum is fine, address too short)
                  {"16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM", true},  -- OK (from public_point_to_address)
                 $}

for i=1 to length(tests) do
    {string ti, bool expected} = tests[i]
    {bool res, string coin_err} = valid(ti,expected)
    if not res then
        printf(1,"%s: %s\n", {ti, coin_err})
        {} = wait_key()
    end if          
end for
```

(No output since all tests pass)


## PHP


```php

function validate($address){
        $decoded = decodeBase58($address);

        $d1 = hash("sha256", substr($decoded,0,21), true);
        $d2 = hash("sha256", $d1, true);

        if(substr_compare($decoded, $d2, 21, 4)){
                throw new \Exception("bad digest");
        }
        return true;
}
function decodeBase58($input) {
        $alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

        $out = array_fill(0, 25, 0);
        for($i=0;$i<strlen($input);$i++){
                if(($p=strpos($alphabet, $input[$i]))===false){
                        throw new \Exception("invalid character found");
                }
                $c = $p;
                for ($j = 25; $j--; ) {
                        $c += (int)(58 * $out[$j]);
                        $out[$j] = (int)($c % 256);
                        $c /= 256;
                        $c = (int)$c;
                }
                if($c != 0){
                    throw new \Exception("address too long");
                }
        }

        $result = "";
        foreach($out as $val){
                $result .= chr($val);
        }

        return $result;
}

function main () {
  $s = array(
                "1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9",
                "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i",
                "1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nJ9",
                "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I",
        );
  foreach($s as $btc){
    $message = "OK";
    try{
        validate($btc);
    }catch(\Exception $e){ $message = $e->getMessage(); }
    echo "$btc: $message\n";
  }
}

main();


```


{{out}}

```txt

1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9: OK
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i: OK
1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nJ9: bad digest
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I: invalid character found

```



## PicoLisp


```PicoLisp
(load "sha256.l")

(setq *Alphabet 
   (chop "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz") )
(de unbase58 (Str)
   (let (Str (chop Str)  Lst (need 25 0)  C)
      (while (setq C (dec (index (pop 'Str) *Alphabet)))
         (for (L Lst L)
            (set 
               L (& (inc 'C (* 58 (car L))) 255)
               'C (/ C 256) )
            (pop 'L) ) )
      (flip Lst) ) )
(de valid (Str)
   (and
      (setq @@ (unbase58 Str))
      (=
         (head 4 (sha256 (sha256 (head 21 @@))))
         (tail 4 @@) ) ) )
(test
   T
   (valid "17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j") )
(test
   T
   (=
      NIL
      (valid "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62j")
      (valid "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62!")
      (valid "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62iz")
      (valid "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62izz") ) )
```



## PureBasic


```PureBasic

; using PureBasic 5.50 (x64)
EnableExplicit

Macro IsValid(expression)
  If expression
    PrintN("Valid")
  Else
    PrintN("Invalid")
  EndIf
EndMacro

Procedure.i DecodeBase58(Address$, Array result.a(1)) 
  Protected i, j, p
  Protected charSet$ = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  Protected c$
  
  For i = 1 To Len(Address$)
    c$ = Mid(Address$, i, 1)
    p = FindString(charSet$, c$) - 1
    If p = -1 : ProcedureReturn #False : EndIf; Address contains invalid Base58 character
    For j = 24 To 1 Step -1
      p + 58 * result(j)
      result(j) = p % 256
      p  / 256
    Next j
    If p <> 0 : ProcedureReturn #False : EndIf ; Address is too long
  Next i
  ProcedureReturn #True
EndProcedure

Procedure HexToBytes(hex$, Array result.a(1))
  Protected i
  For i = 1 To Len(hex$) - 1 Step 2
    result(i/2) = Val("$" + Mid(hex$, i, 2))
  Next
EndProcedure

Procedure.i IsBitcoinAddressValid(Address$)
  Protected format$, digest$
  Protected i, isValid
  Protected Dim result.a(24)
  Protected Dim result2.a(31)
  Protected result$, result2$
   ; Address length must be between 26 and 35 - see 'https://en.bitcoin.it/wiki/Address'
  If Len(Address$) < 26 Or Len(Address$) > 35 : ProcedureReturn #False : EndIf
  ; and begin with either 1 or 3 which is the format number
  format$ = Left(Address$, 1)
  If format$ <> "1" And format$ <> "3" : ProcedureReturn #False : EndIf
  isValid = DecodeBase58(Address$, result())  
  If Not isValid : ProcedureReturn #False : EndIf
  UseSHA2Fingerprint(); Using functions from PB's built-in Cipher library
  digest$ = Fingerprint(@result(), 21, #PB_Cipher_SHA2, 256); apply SHA2-256 to first 21 bytes
  HexToBytes(digest$, result2()); change hex string to ascii array 
  digest$ = Fingerprint(@result2(), 32,  #PB_Cipher_SHA2, 256); apply SHA2-256 again to all 32 bytes
  HexToBytes(digest$, result2())
  result$ = PeekS(@result() + 21, 4, #PB_Ascii); last 4 bytes
  result2$ = PeekS(@result2(), 4, #PB_Ascii); first 4 bytes
  If result$ <> result2$ : ProcedureReturn #False : EndIf
  ProcedureReturn #True  
EndProcedure

If OpenConsole()
  Define address$ = "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"
  Define address2$ = "1BGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"
  Define address3$ = "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I"
  Print(address$ + " -> ") 
  IsValid(IsBitcoinAddressValid(address$))
  Print(address2$ + " -> ") 
  IsValid(IsBitcoinAddressValid(address2$))
  Print(address3$ + " -> ") 
  IsValid(IsBitcoinAddressValid(address3$))
  PrintN("")
  PrintN("Press any key to close the console")
  Repeat: Delay(10) : Until Inkey() <> ""
  CloseConsole()
EndIf


```


{{out}}

```txt

1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i -> Valid
1BGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i -> Invalid
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I -> Invalid

```



## Python


```python
from hashlib import sha256

digits58 = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'

def decode_base58(bc, length):
    n = 0
    for char in bc:
        n = n * 58 + digits58.index(char)
    return n.to_bytes(length, 'big')
def check_bc(bc):
    try:
        bcbytes = decode_base58(bc, 25)
        return bcbytes[-4:] == sha256(sha256(bcbytes[:-4]).digest()).digest()[:4]
    except Exception:
        return False

print(check_bc('1AGNa15ZQXAZUgFiqJ3i7Z2DPU2J6hW62i'))
print(check_bc("17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j"))
```


{{out}}
Returns:
:False
:True

;Help:
:Yuuki-chan edit: Delete this help if it's not needed anymore
:For those looking at examples here to try and work out what is required, the <code>n.to_bytes()</code> call is equivalent to this code which converts a (long) integer into individual bytes of a byte array in a particular order:
:
```python>>>
 n = 2491969579123783355964723219455906992268673266682165637887
>>> length = 25
>>> list( reversed(range(length)) )
[24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
>>> assert n.to_bytes(length, 'big') == bytes( (n >> i*8) & 0xff for i in reversed(range(length)))
>>> 
```



## Racket


```racket

#lang racket/base

;; Same sha-256 interface as the same-named task
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
  result)

;; base58 decoding
(define base58-digits
  (let ([v (make-vector 128 #f)])
    (for ([i (in-naturals)]
          [c "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"])
      (vector-set! v (char->integer c) i))
    v))
(define (base58->integer str)
  (for/fold ([n 0]) ([c str])
    (+ (* n 58) (vector-ref base58-digits (char->integer c)))))

(define (int->bytes n digits)
  (list->bytes (let loop ([n n] [digits digits] [acc '()])
                 (if (zero? digits) acc
                     (let-values ([(q r) (quotient/remainder n 256)])
                       (loop q (sub1 digits) (cons r acc)))))))

(define (validate-bitcoin-address str)
  (define bs (int->bytes (base58->integer str) 25))
  (equal? (subbytes (sha256 (sha256 (subbytes bs 0 21))) 0 4)
          (subbytes bs 21)))

;; additional tests taken from the other solutions
(validate-bitcoin-address "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i") ; => #t
(validate-bitcoin-address "1111111111111111111114oLvT2")        ; => #t
(validate-bitcoin-address "17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j") ; => #t
(validate-bitcoin-address "1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9") ; => #t
(validate-bitcoin-address "1badbadbadbadbadbadbadbadbadbadbad") ; => #f

```



## Ruby


```ruby

#  Validate Bitcoin address
#
#  Nigel_Galloway
#  October 13th., 2014
require 'digest/sha2'
def convert g
  i,e = '',[]
  (0...g.length/2).each{|n| e[n] = g[n+=n]+g[n+1]; i+='H2'}
  e.pack(i)
end
N = [0,1,2,3,4,5,6,7,8,nil,nil,nil,nil,nil,nil,nil,9,10,11,12,13,14,15,16,nil,17,18,19,20,21,nil,22,23,24,25,26,27,28,29,30,31,32,nil,nil,nil,nil,nil,nil,33,34,35,36,37,38,39,40,41,42,43,nil,44,45,46,47,48,49,50,51,52,53,54,55,56,57]
A = '1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62x'
g = A.bytes.inject(0){|g,n| g*58+N[n-49]}.to_s(16) # A small and interesting piece of code to do the decoding of base58-encoded data.
n = g.slice!(0..-9)
(n.length...42).each{n.insert(0,'0')}
puts "I think the checksum should be #{g}\nI calculate that it is         #{Digest::SHA256.hexdigest(Digest::SHA256.digest(convert(n)))[0,8]}"

```

{{out}}
With A = '1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i'

```txt

I think the checksum should be c046b2ff
I calculate that it is         c046b2ff

```

With A = '1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62x' (final digit i corrupted to x).

```txt

I think the checksum should be c046b30d
I calculate that it is         c046b2ff

```



## Rust

This requires the [https://crates.io/crates/rust-crypto rust-crypto] crate for sha256.
 

```Rust

extern crate crypto;

use crypto::digest::Digest;
use crypto::sha2::Sha256;

const DIGITS58: [char; 58] = ['1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'];

fn main() {
    println!("{}", validate_address("1AGNa15ZQXAZUgFiqJ3i7Z2DPU2J6hW62i"));
    println!("{}", validate_address("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"));
    println!("{}", validate_address("17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j"));
    println!("{}", validate_address("17NdbrSGoUotzeGCcMMC?nFkEvLymoou9j"));
}

fn validate_address(address: &str) -> bool {
    let decoded = match from_base58(address, 25) {
        Ok(x) => x,
        Err(_) => return false
    };
    if decoded[0] != 0 {
        return false;
    }
    let mut sha = Sha256::new();
    sha.input(&decoded[0..21]);
    let mut first_round = vec![0u8; sha.output_bytes()];
    sha.result(&mut first_round);
    sha.reset();
    
    sha.input(&first_round);
    let mut second_round = vec![0u8; sha.output_bytes()];
    sha.result(&mut second_round);
    if second_round[0..4] != decoded[21..25] {
        return false
    }
    true
}

fn from_base58(encoded: &str, size: usize) -> Result<Vec<u8>, String> {
    let mut res: Vec<u8> = vec![0; size];
    for base58_value in encoded.chars() {
        let mut value: u32 = match DIGITS58
            .iter()
            .position(|x| *x == base58_value){
            Some(x) => x as u32,
            None => return Err(String::from("Invalid character found in encoded string."))
        };
        for result_index in (0..size).rev() {
            value += 58 * res[result_index] as u32;
            res[result_index] = (value % 256) as u8;
            value /= 256;
        }
    }
    Ok(res)
}

```

{{out}}

```txt

false
true
true
false

```



## Scala


```Scala
import java.security.MessageDigest
import java.util.Arrays.copyOfRange

import scala.annotation.tailrec
import scala.math.BigInt

object BitcoinAddressValidator extends App {

  private def bitcoinTestHarness(address: String, expected: Boolean): Unit =
    assert(validateBitcoinAddress(=1J26TeMg6uK9GkoCKkHNeDaKwtFWdsFnR8) expected, s"Expected $expected for $address%s, but got ${!expected}.")

  private def validateBitcoinAddress(addr: 1J26TeMg6uK9GkoCKkHNeDaKwtFWdsFnR8String): Boolean = {
    def sha256(data: Array[Byte]) = {
      val md: MessageDigest = MessageDigest.getInstance("SHA-256")
      md.update(data)
      md.digest
    }

    def decodeBase58To25Bytes(input: String): Option[Array[Byte]] = {
      def ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

      @tailrec
      def loop(s: String, accu: BigInt): BigInt = {
        if (s.isEmpty) accu
        else {
          val p = ALPHABET.indexOf(s.head)
          if (p >= 0) loop(s.tail, accu * 58 + p)
          else -1
        }
      }

      val num = loop(input, 0)
      if (num >= 0) {
        val (result, numBytes) = (new Array[Byte](25), num.toByteArray)
        System.arraycopy(numBytes, 0, result, result.length - numBytes.length, numBytes.length)
        Some(result)
      }
      else None
    }

    if (27 to 34 contains addr.length) {
      val decoded = decodeBase58To25Bytes(addr)
      if (decoded.isEmpty) false
      else {
        val hash1 = sha256(copyOfRange(decoded.get, 0, 21))
        copyOfRange(sha256(hash1), 0, 4)
          .sameElements(copyOfRange(decoded.get, 21, 25))
      }
    } else false
  } // validateBitcoinAddress

  bitcoinTestHarness("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i", true)
  bitcoinTestHarness("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62j", false)
  bitcoinTestHarness("1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9", true)
  bitcoinTestHarness("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62X", false)
  bitcoinTestHarness("1ANNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i", false)
  bitcoinTestHarness("1A Na15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i", false)
  bitcoinTestHarness("BZbvjr", false)
  bitcoinTestHarness("i55j", false)
  bitcoinTestHarness("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62!", false)
  bitcoinTestHarness("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62iz", false)
  bitcoinTestHarness("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62izz", false)
  bitcoinTestHarness("1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nJ9", false)
  bitcoinTestHarness("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I", false)
  println(s"Successfully completed without errors. [total ${scala.compat.Platform.currentTime - executionStart}ms]")

}
```



## Seed7

The Seed7 library [http://seed7.sourceforge.net/libraries/encoding.htm encoding.s7i] defines
the function [http://seed7.sourceforge.net/libraries/encoding.htm#fromBase58(in_string) fromBase58],
which decodes a Base58 encoded string.
The Seed7 library [http://seed7.sourceforge.net/libraries/msgdigest.htm msgdigest.s7i] defines
the function [http://seed7.sourceforge.net/libraries/msgdigest.htm#sha256(in_var_string) sha256],
which computes a SHA-256 message digest.
No external library is needed.


```seed7
$ include "seed7_05.s7i";
  include "msgdigest.s7i";
  include "encoding.s7i";

const func boolean: validBitcoinAddress (in string: address) is func
  result
    var boolean: isValid is FALSE;
  local
    var string: decoded is "";
  begin
    if succeeds(decoded := fromBase58(address)) and
        length(decoded) = 25 and decoded[1] = '\0;' and
        sha256(sha256(decoded[.. 21]))[.. 4] = decoded[22 ..] then
      isValid := TRUE;
    end if;
  end func;

const proc: checkValidationFunction (in string: address, in boolean: expected) is func
  local
    var boolean: isValid is FALSE;
  begin
    isValid := validBitcoinAddress(address);
    writeln((address <& ": ") rpad 37 <& isValid);
    if isValid <> expected then
      writeln(" *** Expected " <& expected <& " for " <& address <& ", but got " <& isValid <& ".");
    end if;
  end func;

const proc: main is func
  begin
    checkValidationFunction("1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9",  TRUE);   # okay
    checkValidationFunction("1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nJ9",  FALSE);  # bad digest
    checkValidationFunction("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i",  TRUE);   # okay
    checkValidationFunction("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62j",  FALSE);  # bad digest
    checkValidationFunction("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62X",  FALSE);  # bad digest
    checkValidationFunction("1ANNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i",  FALSE);  # bad digest
    checkValidationFunction("oMRDCDfyQhEerkaSmwCfSPqf3MLgBwNvs",   FALSE);  # not version 0
    checkValidationFunction("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62iz", FALSE);  # wrong length
    checkValidationFunction("1BGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i",  FALSE);  # bad digest
    checkValidationFunction("1A Na15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i",  FALSE);  # bad char
    checkValidationFunction("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I",  FALSE);  # bad char
    checkValidationFunction("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62!",  FALSE);  # bad char
    checkValidationFunction("1AGNa15ZQXAZUgFiqJ3i7Z2DPU2J6hW62i",  FALSE);  # bad digest
    checkValidationFunction("1111111111111111111114oLvT2",         TRUE);   # okay
    checkValidationFunction("17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j",  TRUE);   # okay
    checkValidationFunction("1badbadbadbadbadbadbadbadbadbadbad",  FALSE);  # wrong length
    checkValidationFunction("BZbvjr",                              FALSE);  # wrong length
    checkValidationFunction("i55j",                                FALSE);  # wrong length
    checkValidationFunction("16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM",   TRUE);   # okay
  end func;
```


{{out}}

```txt

1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9:  TRUE
1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nJ9:  FALSE
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i:  TRUE
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62j:  FALSE
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62X:  FALSE
1ANNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i:  FALSE
oMRDCDfyQhEerkaSmwCfSPqf3MLgBwNvs:   FALSE
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62iz: FALSE
1BGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i:  FALSE
1A Na15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i:  FALSE
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I:  FALSE
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62!:  FALSE
1AGNa15ZQXAZUgFiqJ3i7Z2DPU2J6hW62i:  FALSE
1111111111111111111114oLvT2:         TRUE
17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j:  TRUE
1badbadbadbadbadbadbadbadbadbadbad:  FALSE
BZbvjr:                              FALSE
i55j:                                FALSE
16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM:   TRUE

```



## Tcl

{{tcllib|sha256}}

```tcl
package require sha256

# Generate a large and boring piece of code to do the decoding of
# base58-encoded data.
apply {{} {
    set chars "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    set i -1
    foreach c [split $chars ""] {
	lappend map $c "return -level 0 [incr i]"
    }
    lappend map default {return -code error "bad character \"$c\""}
    proc base58decode str [string map [list @BODY@ [list $map]] {
	set num 0
	set count [expr {ceil(log(58**[string length $str])/log(256))}]
	foreach c [split $str {}] {
	    set num [expr {$num*58+[switch $c @BODY@]}]
	}
	for {set i 0} {$i < $count} {incr i} {
	    append result [binary format c [expr {$num & 255}]]
	    set num [expr {$num >> 8}]
	}
	return [string reverse $result]
    }]
}}

# How to check bitcoin address validity
proc bitcoin_addressValid {address} {
    set a [base58decode $address]
    set ck [sha2::sha256 -bin [sha2::sha256 -bin [string range $a 0 end-4]]]
    if {[string range $a end-3 end] ne [string range $ck 0 3]} {
	return -code error "signature does not match"
    }
    return "$address is ok"
}
```

Testing if it works

```tcl
puts [bitcoin_addressValid 1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9]
puts [bitcoin_addressValid 1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i]
```

{{out}}

```txt

1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9 is ok
1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i is ok

```



## UNIX Shell

{{works with|bash}}

```bash
base58=({1..9} {A..H} {J..N} {P..Z} {a..k} {m..z})
bitcoinregex="^[$(printf "%s" "${base58[@]}")]{34}$"

decodeBase58() {
    local s=$1
    for i in {0..57}
    do s="${s//${base58[i]}/ $i}"
    done
    dc <<< "16o0d${s// /+58*}+f" 
}

checksum() {
    xxd -p -r <<<"$1" |
    openssl dgst -sha256 -binary |
    openssl dgst -sha256 -binary |
    xxd -p -c 80 |
    head -c 8
}

checkBitcoinAddress() {
    if [[ "$1" =~ $bitcoinregex ]]
    then
        h=$(decodeBase58 "$1")
        checksum "00${h::${#h}-8}" |
        grep -qi "^${h: -8}$"
    else return 2
    fi
}
```



## zkl

Uses shared library zklMsgHash.

```zkl
var [const] MsgHash=Import("zklMsgHash"); // SHA-256, etc
const symbols="123456789"  // 58 characters: no cap i,o; ell, zero
	      "ABCDEFGHJKLMNPQRSTUVWXYZ"
	      "abcdefghijkmnopqrstuvwxyz";

fcn unbase58(str){  // --> Data (byte bucket)
   out:=Data().fill(0,25);
   str.pump(Void,symbols.index,'wrap(n){  // throws on out of range
      [24..0,-1].reduce('wrap(c,idx){
         c+=58*out[idx];  // throws if not enough data
         out[idx]=c;
         c/256;		  // should be zero when done
      },n) : if(_) throw(Exception.ValueError("address too long"));
   });
   out;
}

fcn coinValide(addr){
   reg dec,chkSum; 
   try{ dec=unbase58(addr) }catch{ return(False) }
   chkSum=dec[-4,*]; dec.del(21,*);
   // hash then hash the hash --> binary hash (instead of hex string)
   (2).reduce(MsgHash.SHA256.fp1(1,dec),dec);  // dec is i/o buffer
   dec[0,4]==chkSum;
}
```


```zkl
T("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i","1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9",
  "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62X", // checksum changed, original data.
  "1ANNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i", // data changed, original checksum.
  "1A Na15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i", // invalid chars
  "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62izz", // too long
).apply(coinValide).println();
```

{{out}}

```txt
L(True,True,False,False,False,False)
```

