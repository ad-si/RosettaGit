+++
title = "MD5/Implementation"
description = ""
date = 2019-05-01T19:25:01Z
aliases = []
[extra]
id = 8366
[taxonomies]
categories = ["task", "Checksums"]
tags = []
languages = [
  "ada",
  "autohotkey",
  "bbc_basic",
  "c",
  "coffeescript",
  "common_lisp",
  "csharp",
  "d",
  "freebasic",
  "fsharp",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "liberty_basic",
  "lingo",
  "mathematica",
  "nim",
  "oorexx",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "racket",
  "rexx",
  "rpg",
  "rust",
  "scala",
  "seed7",
  "sidef",
  "swift",
  "tcl",
  "x86_assembly",
]
+++

## Task

The purpose of this task to code and validate an implementation of the MD5 Message Digest Algorithm by coding the algorithm directly (not using a call to a built-in or external hashing library).  For details of the algorithm refer to [[wp:Md5#Algorithm|MD5 on Wikipedia]] or the [http://www.ietf.org/rfc/rfc1321.txt MD5 definition in IETF RFC (1321)].

* The implementation needs to implement the key functionality namely producing a correct message digest for an input string.  It is not necessary to mimic all of the calling modes such as adding to a digest one block at a time over subsequent calls.
* In addition to coding and verifying your implementation, note any challenges your language presented implementing the solution, implementation choices made, or limitations of your solution.
* Solutions on this page should implement MD5 directly and NOT use built in (MD5) functions, call outs to operating system calls or library routines written in other languages as is common in the original [[MD5]] task.
* The following are acceptable:
** An original implementation from the specification, reference implementation, or pseudo-code
** A translation of a correct implementation from another language
** A library routine in the same language; however, the source must be included here.

The solutions shown here will provide practical illustrations of bit manipulation, unsigned integers, working with little-endian data.  Additionally, the task requires an attention to details such as boundary conditions since being out by even 1 bit will produce dramatically different results.  Subtle implementation bugs can result in some hashes being correct while others are wrong.  Not only is it critical to get the individual sub functions working correctly, even small errors in padding, endianness, or data layout will result in failure.

The following verification strings and hashes come from RFC 1321:
```txt
                            hash code <== string
   0xd41d8cd98f00b204e9800998ecf8427e <== ""
   0x0cc175b9c0f1b6a831c399e269772661 <== "a"
   0x900150983cd24fb0d6963f7d28e17f72 <== "abc"
   0xf96b697d7cb7938d525a2f31aaf161d0 <== "message digest"
   0xc3fcd3d76192e4007dfb496cca67e13b <== "abcdefghijklmnopqrstuvwxyz"
   0xd174ab98d277d9f5a5611c2c9f419d9f <== "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
   0x57edf4a22be3c955ac49da2e2107b67a <== "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
```


In addition, intermediate outputs to aid in developing an implementation can be found [[MD5/Implementation Debug|here]].

The MD5 Message-Digest Algorithm was developed by [[wp:RSA_SecurityRSA|RSA Data Security, Inc.]] in 1991.

## Ada

note: this could be dependent on the endianness of the machine it runs on - not tested on big endian.

md5.ads:

```Ada
package MD5 is

   type Int32 is mod 2 ** 32;
   type MD5_Hash is array (1 .. 4) of Int32;
   function MD5 (Input : String) return MD5_Hash;

   -- 32 hexadecimal characters + '0x' prefix
   subtype MD5_String is String (1 .. 34);
   function To_String (Item : MD5_Hash) return MD5_String;

end MD5;
```


md5.adb:

```Ada
with Ada.Unchecked_Conversion;

package body MD5 is
   type Int32_Array is array (Positive range <>) of Int32;

   function Rotate_Left (Value : Int32; Count : Int32) return Int32 is
      Bit    : Boolean;
      Result : Int32 := Value;
   begin
      for I in 1 .. Count loop
         Bit    := (2 ** 31 and Result) = 2 ** 31;
         Result := Result * 2;
         if Bit then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Rotate_Left;

   function Pad_String (Item : String) return Int32_Array is
      -- always pad positive amount of Bytes
      Padding_Bytes : Positive := 64 - Item'Length mod 64;
      subtype String4 is String (1 .. 4);
      function String4_To_Int32 is new Ada.Unchecked_Conversion
        (Source => String4,
         Target => Int32);
   begin
      if Padding_Bytes <= 2 then
         Padding_Bytes := Padding_Bytes + 64;
      end if;
      declare
         Result        : Int32_Array (1 .. (Item'Length + Padding_Bytes) / 4);
         Current_Index : Positive := 1;
      begin
         for I in 1 .. Item'Length / 4 loop
            Result (I)    :=
              String4_To_Int32 (Item (4 * (I - 1) + 1 .. 4 * I));
            Current_Index := Current_Index + 1;
         end loop;

         declare
            Last_String : String4          := (others => Character'Val (0));
            Chars_Left  : constant Natural := Item'Length mod 4;
         begin
            Last_String (1 .. Chars_Left) :=
              Item (Item'Last - Chars_Left + 1 .. Item'Last);
            Last_String (Chars_Left + 1)  := Character'Val (2#1000_0000#);
            Result (Current_Index)        := String4_To_Int32 (Last_String);
            Current_Index                 := Current_Index + 1;
         end;

         Result (Current_Index .. Result'Last) := (others => 0);
         -- append length as bit count
         Result (Result'Last - 1) := Item'Length * 2 ** 3; -- mod 2 ** 32;
         Result (Result'Last)     := Item'Length / 2 ** (32 - 3);
         return Result;
      end;
   end Pad_String;

   function Turn_Around (X : Int32) return Int32 is
      Result : Int32 := 0;
   begin
      for Byte in 1 .. 4 loop
         Result := Result * 16#100#;
         Result := Result + (X / (2 ** (8 * (Byte - 1)))) mod 16#100#;
      end loop;
      return Result;
   end Turn_Around;

   function MD5 (Input : String) return MD5_Hash is
      function F (X, Y, Z : Int32) return Int32 is
      begin
         return Z xor (X and (Y xor Z));
      end F;
      function G (X, Y, Z : Int32) return Int32 is
      begin
         return (X and Z) or (Y and (not Z));
      end G;
      function H (X, Y, Z : Int32) return Int32 is
      begin
         return X xor Y xor Z;
      end H;
      function I (X, Y, Z : Int32) return Int32 is
      begin
         return Y xor (X or (not Z));
      end I;
      T  : constant Int32_Array :=
        (16#d76aa478#, 16#e8c7b756#, 16#242070db#, 16#c1bdceee#,
         16#f57c0faf#, 16#4787c62a#, 16#a8304613#, 16#fd469501#,
         16#698098d8#, 16#8b44f7af#, 16#ffff5bb1#, 16#895cd7be#,
         16#6b901122#, 16#fd987193#, 16#a679438e#, 16#49b40821#,
         16#f61e2562#, 16#c040b340#, 16#265e5a51#, 16#e9b6c7aa#,
         16#d62f105d#, 16#02441453#, 16#d8a1e681#, 16#e7d3fbc8#,
         16#21e1cde6#, 16#c33707d6#, 16#f4d50d87#, 16#455a14ed#,
         16#a9e3e905#, 16#fcefa3f8#, 16#676f02d9#, 16#8d2a4c8a#,
         16#fffa3942#, 16#8771f681#, 16#6d9d6122#, 16#fde5380c#,
         16#a4beea44#, 16#4bdecfa9#, 16#f6bb4b60#, 16#bebfbc70#,
         16#289b7ec6#, 16#eaa127fa#, 16#d4ef3085#, 16#04881d05#,
         16#d9d4d039#, 16#e6db99e5#, 16#1fa27cf8#, 16#c4ac5665#,
         16#f4292244#, 16#432aff97#, 16#ab9423a7#, 16#fc93a039#,
         16#655b59c3#, 16#8f0ccc92#, 16#ffeff47d#, 16#85845dd1#,
         16#6fa87e4f#, 16#fe2ce6e0#, 16#a3014314#, 16#4e0811a1#,
         16#f7537e82#, 16#bd3af235#, 16#2ad7d2bb#, 16#eb86d391#);
      A : Int32 := 16#67452301#;
      B : Int32 := 16#EFCDAB89#;
      C : Int32 := 16#98BADCFE#;
      D : Int32 := 16#10325476#;
      Padded_String : constant Int32_Array := Pad_String (Input);
   begin
      for Block512 in 1 .. Padded_String'Length / 16 loop
         declare
            Words    : constant Int32_Array (1 .. 16) :=
              Padded_String (16 * (Block512 - 1) + 1 .. 16 * Block512);
            AA       : constant Int32                 := A;
            BB       : constant Int32                 := B;
            CC       : constant Int32                 := C;
            DD       : constant Int32                 := D;
         begin
            -- round 1
            A := B + Rotate_Left ((A + F (B, C, D) + Words (1) + T (1)),  7);
            D := A + Rotate_Left ((D + F (A, B, C) + Words (2) + T (2)), 12);
            C := D + Rotate_Left ((C + F (D, A, B) + Words (3) + T (3)), 17);
            B := C + Rotate_Left ((B + F (C, D, A) + Words (4) + T (4)), 22);
            A := B + Rotate_Left ((A + F (B, C, D) + Words (5) + T (5)),  7);
            D := A + Rotate_Left ((D + F (A, B, C) + Words (6) + T (6)), 12);
            C := D + Rotate_Left ((C + F (D, A, B) + Words (7) + T (7)), 17);
            B := C + Rotate_Left ((B + F (C, D, A) + Words (8) + T (8)), 22);
            A := B + Rotate_Left ((A + F (B, C, D) + Words (9) + T (9)),  7);
            D := A + Rotate_Left ((D + F (A, B, C) + Words (10) + T (10)), 12);
            C := D + Rotate_Left ((C + F (D, A, B) + Words (11) + T (11)), 17);
            B := C + Rotate_Left ((B + F (C, D, A) + Words (12) + T (12)), 22);
            A := B + Rotate_Left ((A + F (B, C, D) + Words (13) + T (13)),  7);
            D := A + Rotate_Left ((D + F (A, B, C) + Words (14) + T (14)), 12);
            C := D + Rotate_Left ((C + F (D, A, B) + Words (15) + T (15)), 17);
            B := C + Rotate_Left ((B + F (C, D, A) + Words (16) + T (16)), 22);
            -- round 2
            A := B + Rotate_Left ((A + G (B, C, D) + Words (2) + T (17)),  5);
            D := A + Rotate_Left ((D + G (A, B, C) + Words (7) + T (18)),  9);
            C := D + Rotate_Left ((C + G (D, A, B) + Words (12) + T (19)), 14);
            B := C + Rotate_Left ((B + G (C, D, A) + Words (1) + T (20)), 20);
            A := B + Rotate_Left ((A + G (B, C, D) + Words (6) + T (21)),  5);
            D := A + Rotate_Left ((D + G (A, B, C) + Words (11) + T (22)),  9);
            C := D + Rotate_Left ((C + G (D, A, B) + Words (16) + T (23)), 14);
            B := C + Rotate_Left ((B + G (C, D, A) + Words (5) + T (24)), 20);
            A := B + Rotate_Left ((A + G (B, C, D) + Words (10) + T (25)),  5);
            D := A + Rotate_Left ((D + G (A, B, C) + Words (15) + T (26)),  9);
            C := D + Rotate_Left ((C + G (D, A, B) + Words (4) + T (27)), 14);
            B := C + Rotate_Left ((B + G (C, D, A) + Words (9) + T (28)), 20);
            A := B + Rotate_Left ((A + G (B, C, D) + Words (14) + T (29)),  5);
            D := A + Rotate_Left ((D + G (A, B, C) + Words (3) + T (30)),  9);
            C := D + Rotate_Left ((C + G (D, A, B) + Words (8) + T (31)), 14);
            B := C + Rotate_Left ((B + G (C, D, A) + Words (13) + T (32)), 20);
            -- round 3
            A := B + Rotate_Left ((A + H (B, C, D) + Words (6) + T (33)),  4);
            D := A + Rotate_Left ((D + H (A, B, C) + Words (9) + T (34)), 11);
            C := D + Rotate_Left ((C + H (D, A, B) + Words (12) + T (35)), 16);
            B := C + Rotate_Left ((B + H (C, D, A) + Words (15) + T (36)), 23);
            A := B + Rotate_Left ((A + H (B, C, D) + Words (2) + T (37)),  4);
            D := A + Rotate_Left ((D + H (A, B, C) + Words (5) + T (38)), 11);
            C := D + Rotate_Left ((C + H (D, A, B) + Words (8) + T (39)), 16);
            B := C + Rotate_Left ((B + H (C, D, A) + Words (11) + T (40)), 23);
            A := B + Rotate_Left ((A + H (B, C, D) + Words (14) + T (41)),  4);
            D := A + Rotate_Left ((D + H (A, B, C) + Words (1) + T (42)), 11);
            C := D + Rotate_Left ((C + H (D, A, B) + Words (4) + T (43)), 16);
            B := C + Rotate_Left ((B + H (C, D, A) + Words (7) + T (44)), 23);
            A := B + Rotate_Left ((A + H (B, C, D) + Words (10) + T (45)),  4);
            D := A + Rotate_Left ((D + H (A, B, C) + Words (13) + T (46)), 11);
            C := D + Rotate_Left ((C + H (D, A, B) + Words (16) + T (47)), 16);
            B := C + Rotate_Left ((B + H (C, D, A) + Words (3) + T (48)), 23);
            -- round 4
            A := B + Rotate_Left ((A + I (B, C, D) + Words (1) + T (49)),  6);
            D := A + Rotate_Left ((D + I (A, B, C) + Words (8) + T (50)), 10);
            C := D + Rotate_Left ((C + I (D, A, B) + Words (15) + T (51)), 15);
            B := C + Rotate_Left ((B + I (C, D, A) + Words (6) + T (52)), 21);
            A := B + Rotate_Left ((A + I (B, C, D) + Words (13) + T (53)),  6);
            D := A + Rotate_Left ((D + I (A, B, C) + Words (4) + T (54)), 10);
            C := D + Rotate_Left ((C + I (D, A, B) + Words (11) + T (55)), 15);
            B := C + Rotate_Left ((B + I (C, D, A) + Words (2) + T (56)), 21);
            A := B + Rotate_Left ((A + I (B, C, D) + Words (9) + T (57)),  6);
            D := A + Rotate_Left ((D + I (A, B, C) + Words (16) + T (58)), 10);
            C := D + Rotate_Left ((C + I (D, A, B) + Words (7) + T (59)), 15);
            B := C + Rotate_Left ((B + I (C, D, A) + Words (14) + T (60)), 21);
            A := B + Rotate_Left ((A + I (B, C, D) + Words (5) + T (61)),  6);
            D := A + Rotate_Left ((D + I (A, B, C) + Words (12) + T (62)), 10);
            C := D + Rotate_Left ((C + I (D, A, B) + Words (3) + T (63)), 15);
            B := C + Rotate_Left ((B + I (C, D, A) + Words (10) + T (64)), 21);
            -- increment
            A := A + AA;
            B := B + BB;
            C := C + CC;
            D := D + DD;
         end;
      end loop;
      return
        (Turn_Around (A),
         Turn_Around (B),
         Turn_Around (C),
         Turn_Around (D));
   end MD5;

   function To_String (Item : MD5_Hash) return MD5_String is
      Hex_Chars : constant array (0 .. 15) of Character :=
        ('0', '1', '2', '3', '4', '5', '6', '7',
        '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
      Result    : MD5_String := (1      => '0',
                                 2      => 'x',
                                 others => '0');
      Temp      : Int32;
      Position  : Natural := Result'Last;
   begin
      for Part in reverse Item'Range loop
         Temp := Item (Part);
         while Position > Result'Last - (5 - Part) * 8 loop
            Result (Position) := Hex_Chars (Natural (Temp mod 16));
            Position          := Position - 1;
            Temp              := Temp / 16;
         end loop;
      end loop;
      return Result;
   end To_String;

end MD5;
```


tester.adb:

```Ada
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with MD5;

procedure Tester is
   use Ada.Strings.Unbounded;
   type String_Array is array (Positive range <>) of Unbounded_String;
   Sources : constant String_Array :=
     (To_Unbounded_String (""),
      To_Unbounded_String ("a"),
      To_Unbounded_String ("abc"),
      To_Unbounded_String ("message digest"),
      To_Unbounded_String ("abcdefghijklmnopqrstuvwxyz"),
      To_Unbounded_String
         ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
      To_Unbounded_String
         ("12345678901234567890123456789012345678901234567890123456789012345678901234567890")
     );
   Digests : constant String_Array :=
     (To_Unbounded_String ("0xd41d8cd98f00b204e9800998ecf8427e"),
      To_Unbounded_String ("0x0cc175b9c0f1b6a831c399e269772661"),
      To_Unbounded_String ("0x900150983cd24fb0d6963f7d28e17f72"),
      To_Unbounded_String ("0xf96b697d7cb7938d525a2f31aaf161d0"),
      To_Unbounded_String ("0xc3fcd3d76192e4007dfb496cca67e13b"),
      To_Unbounded_String ("0xd174ab98d277d9f5a5611c2c9f419d9f"),
      To_Unbounded_String ("0x57edf4a22be3c955ac49da2e2107b67a"));
begin
   for I in Sources'Range loop
      Ada.Text_IO.Put_Line ("MD5 (""" & To_String (Sources (I)) & """):");
      Ada.Text_IO.Put_Line
        (MD5.To_String (MD5.MD5 (To_String (Sources (I)))));
      Ada.Text_IO.Put_Line (To_String (Digests (I)) & " (correct value)");
   end loop;
end Tester;
```


output:

```txt
MD5 (""):
0xd41d8cd98f00b204e9800998ecf8427e
0xd41d8cd98f00b204e9800998ecf8427e (correct value)
MD5 ("a"):
0x0cc175b9c0f1b6a831c399e269772661
0x0cc175b9c0f1b6a831c399e269772661 (correct value)
MD5 ("abc"):
0x900150983cd24fb0d6963f7d28e17f72
0x900150983cd24fb0d6963f7d28e17f72 (correct value)
MD5 ("message digest"):
0xf96b697d7cb7938d525a2f31aaf161d0
0xf96b697d7cb7938d525a2f31aaf161d0 (correct value)
MD5 ("abcdefghijklmnopqrstuvwxyz"):
0xc3fcd3d76192e4007dfb496cca67e13b
0xc3fcd3d76192e4007dfb496cca67e13b (correct value)
MD5 ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"):
0xd174ab98d277d9f5a5611c2c9f419d9f
0xd174ab98d277d9f5a5611c2c9f419d9f (correct value)
MD5 ("12345678901234567890123456789012345678901234567890123456789012345678901234567890"):
0x57edf4a22be3c955ac49da2e2107b67a
0x57edf4a22be3c955ac49da2e2107b67a (correct value)
```



## AutoHotkey

''See the implementation at [[MD5#AutoHotkey]].''


## BBC BASIC

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
      LOCAL a%, b%, c%, d%, f%, g%, h0%, h1%, h2%, h3%, i%, bits%, chunk%, temp%
      LOCAL r&(), k%(), w%()
      DIM r&(63), k%(63), w%(15)

      REM r specifies the per-round shift amounts:
      r&() = 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, \
      \      5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, \
      \      4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, \
      \      6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21

      REM Use binary integer part of the sines of integers (Radians) as constants:
      FOR i% = 0 TO 63
        k%(i%) = FN32(INT(ABS(SIN(i% + 1.0#)) * 2^32))
      NEXT

      REM Initialize variables:
      h0% = &67452301
      h1% = &EFCDAB89
      h2% = &98BADCFE
      h3% = &10325476

      bits% = LEN(message$)*8

      REM Append '1' bit to message:
      message$ += CHR$&80

      REM Append '0' bits until message length in bits = 448 (mod 512):
      WHILE (LEN(message$) MOD 64) <> 56
        message$ += CHR$0
      ENDWHILE

      REM Append length of message (before pre-processing), in bits, as
      REM 64-bit little-endian integer:
      FOR i% = 0 TO 56 STEP 8
        message$ += CHR$(bits% >>> i%)
      NEXT

      REM Process the message in successive 512-bit chunks:
      FOR chunk% = 0 TO LEN(message$) DIV 64 - 1

        REM Break chunk into sixteen 32-bit little-endian words:
        FOR i% = 0 TO 15
          w%(i%) = !(!^message$ + 64*chunk% + 4*i%)
        NEXT i%

        REM Initialize hash value for this chunk:
        a% = h0%
        b% = h1%
        c% = h2%
        d% = h3%

        REM Main loop:
        FOR i% = 0 TO 63
          CASE TRUE OF
            WHEN i% <= 15:
              f% = d% EOR (b% AND (c% EOR d%))
              g% = i%
            WHEN 16 <= i% AND i% <= 31:
              f% = c% EOR (d% AND (b% EOR c%))
              g% = (5 * i% + 1) MOD 16
            WHEN 32 <= i% AND i% <= 47:
              f% = b% EOR c% EOR d%
              g% = (3 * i% + 5) MOD 16
            OTHERWISE:
              f% = c% EOR (b% OR (NOT d%))
              g% = (7 * i%) MOD 16
          ENDCASE

          temp% = d%
          d% = c%
          c% = b%
          b% = FN32(b% + FNlrot(FN32(a% + f%) + FN32(k%(i%) + w%(g%)), r&(i%)))
          a% = temp%

        NEXT i%

        REM Add this chunk's hash to result so far:
        h0% = FN32(h0% + a%)
        h1% = FN32(h1% + b%)
        h2% = FN32(h2% + c%)
        h3% = FN32(h3% + d%)

      NEXT chunk%

      = FNrevhex(h0%) + FNrevhex(h1%) + FNrevhex(h2%) + FNrevhex(h3%)

      DEF FNrevhex(A%)
      SWAP ?(^A%+0),?(^A%+3)
      SWAP ?(^A%+1),?(^A%+2)
      = RIGHT$("0000000"+STR$~A%,8)

      DEF FNlrot(n#, r%)
      n# = FN32(n#)
      = (n# << r%) OR (n# >>> (32 - r%))

      DEF FN32(n#)
      WHILE n# > &7FFFFFFF : n# -= 2^32 : ENDWHILE
      WHILE n# < &80000000 : n# += 2^32 : ENDWHILE
      = n#
```



## C

''See the implementation at [[MD5#C]]. Also, RFC 1321 already provides C code.''


## C#

Handwritten implementation ([http://farazmahmood.wordpress.com/projects/md5-implementation-in-c/]):

```c#

	/// Represent digest with ABCD
	sealed public class Digest
	{
		public uint A;
		public uint B;
		public uint C;
		public uint D;

		public Digest()
		{
			A=(uint)MD5InitializerConstant.A;
			B=(uint)MD5InitializerConstant.B;
			C=(uint)MD5InitializerConstant.C;
			D=(uint)MD5InitializerConstant.D;
       	        }

		public override string ToString()
		{
			string st ;
			st= MD5Helper.ReverseByte(A).ToString("X8")+
			    MD5Helper.ReverseByte(B).ToString("X8")+
                            MD5Helper.ReverseByte(C).ToString("X8")+
			    MD5Helper.ReverseByte(D).ToString("X8");
			return st;

		}
	}

	public class MD5
	{
		/***********************VARIABLES************************************/


		/***********************Statics**************************************/
		/// <summary>
		/// lookup table 4294967296*sin(i)
		/// </summary>
		protected readonly static uint []  T =new uint[64]
			{	0xd76aa478,0xe8c7b756,0x242070db,0xc1bdceee,
				0xf57c0faf,0x4787c62a,0xa8304613,0xfd469501,
                0x698098d8,0x8b44f7af,0xffff5bb1,0x895cd7be,
                0x6b901122,0xfd987193,0xa679438e,0x49b40821,
				0xf61e2562,0xc040b340,0x265e5a51,0xe9b6c7aa,
                0xd62f105d,0x2441453,0xd8a1e681,0xe7d3fbc8,
                0x21e1cde6,0xc33707d6,0xf4d50d87,0x455a14ed,
				0xa9e3e905,0xfcefa3f8,0x676f02d9,0x8d2a4c8a,
                0xfffa3942,0x8771f681,0x6d9d6122,0xfde5380c,
                0xa4beea44,0x4bdecfa9,0xf6bb4b60,0xbebfbc70,
                0x289b7ec6,0xeaa127fa,0xd4ef3085,0x4881d05,
				0xd9d4d039,0xe6db99e5,0x1fa27cf8,0xc4ac5665,
                0xf4292244,0x432aff97,0xab9423a7,0xfc93a039,
                0x655b59c3,0x8f0ccc92,0xffeff47d,0x85845dd1,
                0x6fa87e4f,0xfe2ce6e0,0xa3014314,0x4e0811a1,
				0xf7537e82,0xbd3af235,0x2ad7d2bb,0xeb86d391};

		/*****instance variables**************/
		/// <summary>
		/// X used to proces data in
		///	512 bits chunks as 16 32 bit word
		/// </summary>
		protected  uint [] X = new uint [16];

		/// <summary>
		/// the finger print obtained.
		/// </summary>
		protected Digest dgFingerPrint;

		/// <summary>
		/// the input bytes
		/// </summary>
		protected	byte [] m_byteInput;



		/**********************EVENTS AND DELEGATES*******************************************/

		public delegate void ValueChanging (object sender,MD5ChangingEventArgs Changing);
		public delegate void ValueChanged (object sender,MD5ChangedEventArgs Changed);


		public event ValueChanging OnValueChanging;
		public event ValueChanged  OnValueChanged;



		/********************************************************************/
		/***********************PROPERTIES ***********************/
		/// <summary>
		///gets or sets as string
		/// </summary>
		public string Value
		{
			get
			{
				string st ;
				char [] tempCharArray= new Char[m_byteInput.Length];

				for(int i =0; i<m_byteInput.Length;i++)
					tempCharArray[i]=(char)m_byteInput[i];

				st= new String(tempCharArray);
				return st;
			}
			set
			{
				/// raise the event to notify the changing
				if (this.OnValueChanging !=null)
					this.OnValueChanging(this,new MD5ChangingEventArgs(value));


				m_byteInput=new byte[value.Length];
				for (int i =0; i<value.Length;i++)
					m_byteInput[i]=(byte)value[i];
				dgFingerPrint=CalculateMD5Value();

				/// raise the event to notify the change
				if (this.OnValueChanged !=null)
					this.OnValueChanged(this,new MD5ChangedEventArgs(value,dgFingerPrint.ToString()));

			}
		}

		/// <summary>
		/// get/sets as  byte array
		/// </summary>
		public byte [] ValueAsByte
		{
			get
			{
				byte [] bt = new byte[m_byteInput.Length];
				for (int i =0; i<m_byteInput.Length;i++)
					bt[i]=m_byteInput[i];
				return bt;
          }
			set
			{
				/// raise the event to notify the changing
				if (this.OnValueChanging !=null)
					this.OnValueChanging(this,new MD5ChangingEventArgs(value));

				m_byteInput=new byte[value.Length];
				for (int i =0; i<value.Length;i++)
					m_byteInput[i]=value[i];
				dgFingerPrint=CalculateMD5Value();


				/// notify the changed  value
				if (this.OnValueChanged !=null)
					this.OnValueChanged(this,new MD5ChangedEventArgs(value,dgFingerPrint.ToString()));
			}
		}

		//gets the signature/figner print as string
		public  string FingerPrint
		{
			get
			{
				return dgFingerPrint.ToString();
			}
		}


		/*************************************************************************/
		/// <summary>
		/// Constructor
		/// </summary>
		public MD5()
		{
			Value="";
		}


		/******************************************************************************/
		/*********************METHODS**************************/

		/// <summary>
		/// calculat md5 signature of the string in Input
		/// </summary>
		/// <returns> Digest: the finger print of msg</returns>
		protected Digest CalculateMD5Value()
		{
			/***********vairable declaration**************/
			byte [] bMsg;	//buffer to hold bits
			uint N;			//N is the size of msg as  word (32 bit)
			Digest dg =new Digest();			//  the value to be returned

			// create a buffer with bits padded and length is alos padded
			bMsg=CreatePaddedBuffer();

			N=(uint)(bMsg.Length*8)/32;		//no of 32 bit blocks

			for (uint  i=0; i<N/16;i++)
			{
				CopyBlock(bMsg,i);
				PerformTransformation(ref dg.A,ref dg.B,ref dg.C,ref dg.D);
			}
			return dg;
		}

		/********************************************************
		 * TRANSFORMATIONS :  FF , GG , HH , II  acc to RFC 1321
		 * where each Each letter represnets the aux function used
		 *********************************************************/



		/// <summary>
		/// perform transformatio using f(((b&c) | (~(b)&d))
		/// </summary>
		protected void TransF(ref uint a, uint b, uint c, uint d,uint k,ushort s, uint i )
		{
			a = b + MD5Helper.RotateLeft((a + ((b&c) | (~(b)&d)) + X[k] + T[i-1]), s);
		}

		/// <summary>
		/// perform transformatio using g((b&d) | (c & ~d) )
		/// </summary>
		protected void TransG(ref uint a, uint b, uint c, uint d,uint k,ushort s, uint i )
		{
			a = b + MD5Helper.RotateLeft((a + ((b&d) | (c & ~d) ) + X[k] + T[i-1]), s);
		}

		/// <summary>
		/// perform transformatio using h(b^c^d)
		/// </summary>
		protected void TransH(ref uint a, uint b, uint c, uint d,uint k,ushort s, uint i )
		{
			a = b + MD5Helper.RotateLeft((a + (b^c^d) + X[k] + T[i-1]), s);
		}

		/// <summary>
		/// perform transformatio using i (c^(b|~d))
		/// </summary>
		protected void TransI(ref uint a, uint b, uint c, uint d,uint k,ushort s, uint i )
		{
			a = b + MD5Helper.RotateLeft((a + (c^(b|~d))+ X[k] + T[i-1]), s);
		}



		/// <summary>
		/// Perform All the transformation on the data
		/// </summary>
		/// <param name="A">A</param>
		/// <param name="B">B </param>
		/// <param name="C">C</param>
		/// <param name="D">D</param>
		protected void PerformTransformation(ref uint A,ref uint B,ref uint C, ref uint D)
		{
			//// saving  ABCD  to be used in end of loop

			uint AA,BB,CC,DD;

			AA=A;
			BB=B;
			CC=C;
			DD=D;

			/* Round 1
				* [ABCD  0  7  1]  [DABC  1 12  2]  [CDAB  2 17  3]  [BCDA  3 22  4]
				* [ABCD  4  7  5]  [DABC  5 12  6]  [CDAB  6 17  7]  [BCDA  7 22  8]
				* [ABCD  8  7  9]  [DABC  9 12 10]  [CDAB 10 17 11]  [BCDA 11 22 12]
				* [ABCD 12  7 13]  [DABC 13 12 14]  [CDAB 14 17 15]  [BCDA 15 22 16]
				*  * */
			TransF(ref A,B,C,D,0,7,1);TransF(ref D,A,B,C,1,12,2);TransF(ref C,D,A,B,2,17,3);TransF(ref B,C,D,A,3,22,4);
			TransF(ref A,B,C,D,4,7,5);TransF(ref D,A,B,C,5,12,6);TransF(ref C,D,A,B,6,17,7);TransF(ref B,C,D,A,7,22,8);
			TransF(ref A,B,C,D,8,7,9);TransF(ref D,A,B,C,9,12,10);TransF(ref C,D,A,B,10,17,11);TransF(ref B,C,D,A,11,22,12);
			TransF(ref A,B,C,D,12,7,13);TransF(ref D,A,B,C,13,12,14);TransF(ref C,D,A,B,14,17,15);TransF(ref B,C,D,A,15,22,16);
			/** rOUND 2
				**[ABCD  1  5 17]  [DABC  6  9 18]  [CDAB 11 14 19]  [BCDA  0 20 20]
				*[ABCD  5  5 21]  [DABC 10  9 22]  [CDAB 15 14 23]  [BCDA  4 20 24]
				*[ABCD  9  5 25]  [DABC 14  9 26]  [CDAB  3 14 27]  [BCDA  8 20 28]
				*[ABCD 13  5 29]  [DABC  2  9 30]  [CDAB  7 14 31]  [BCDA 12 20 32]
			*/
			TransG(ref A,B,C,D,1,5,17);TransG(ref D,A,B,C,6,9,18);TransG(ref C,D,A,B,11,14,19);TransG(ref B,C,D,A,0,20,20);
			TransG(ref A,B,C,D,5,5,21);TransG(ref D,A,B,C,10,9,22);TransG(ref C,D,A,B,15,14,23);TransG(ref B,C,D,A,4,20,24);
			TransG(ref A,B,C,D,9,5,25);TransG(ref D,A,B,C,14,9,26);TransG(ref C,D,A,B,3,14,27);TransG(ref B,C,D,A,8,20,28);
			TransG(ref A,B,C,D,13,5,29);TransG(ref D,A,B,C,2,9,30);TransG(ref C,D,A,B,7,14,31);TransG(ref B,C,D,A,12,20,32);
			/*  rOUND 3
				* [ABCD  5  4 33]  [DABC  8 11 34]  [CDAB 11 16 35]  [BCDA 14 23 36]
				* [ABCD  1  4 37]  [DABC  4 11 38]  [CDAB  7 16 39]  [BCDA 10 23 40]
				* [ABCD 13  4 41]  [DABC  0 11 42]  [CDAB  3 16 43]  [BCDA  6 23 44]
				* [ABCD  9  4 45]  [DABC 12 11 46]  [CDAB 15 16 47]  [BCDA  2 23 48]
			 * */
			TransH(ref A,B,C,D,5,4,33);TransH(ref D,A,B,C,8,11,34);TransH(ref C,D,A,B,11,16,35);TransH(ref B,C,D,A,14,23,36);
			TransH(ref A,B,C,D,1,4,37);TransH(ref D,A,B,C,4,11,38);TransH(ref C,D,A,B,7,16,39);TransH(ref B,C,D,A,10,23,40);
			TransH(ref A,B,C,D,13,4,41);TransH(ref D,A,B,C,0,11,42);TransH(ref C,D,A,B,3,16,43);TransH(ref B,C,D,A,6,23,44);
			TransH(ref A,B,C,D,9,4,45);TransH(ref D,A,B,C,12,11,46);TransH(ref C,D,A,B,15,16,47);TransH(ref B,C,D,A,2,23,48);
			/*ORUNF  4
				*[ABCD  0  6 49]  [DABC  7 10 50]  [CDAB 14 15 51]  [BCDA  5 21 52]
				*[ABCD 12  6 53]  [DABC  3 10 54]  [CDAB 10 15 55]  [BCDA  1 21 56]
				*[ABCD  8  6 57]  [DABC 15 10 58]  [CDAB  6 15 59]  [BCDA 13 21 60]
				*[ABCD  4  6 61]  [DABC 11 10 62]  [CDAB  2 15 63]  [BCDA  9 21 64]
						 * */
			TransI(ref A,B,C,D,0,6,49);TransI(ref D,A,B,C,7,10,50);TransI(ref C,D,A,B,14,15,51);TransI(ref B,C,D,A,5,21,52);
			TransI(ref A,B,C,D,12,6,53);TransI(ref D,A,B,C,3,10,54);TransI(ref C,D,A,B,10,15,55);TransI(ref B,C,D,A,1,21,56);
			TransI(ref A,B,C,D,8,6,57);TransI(ref D,A,B,C,15,10,58);TransI(ref C,D,A,B,6,15,59);TransI(ref B,C,D,A,13,21,60);
			TransI(ref A,B,C,D,4,6,61);TransI(ref D,A,B,C,11,10,62);TransI(ref C,D,A,B,2,15,63);TransI(ref B,C,D,A,9,21,64);


			A=A+AA;
			B=B+BB;
			C=C+CC;
			D=D+DD;


		}


		/// <summary>
		/// Create Padded buffer for processing , buffer is padded with 0 along
		/// with the size in the end
		/// </summary>
		/// <returns>the padded buffer as byte array</returns>
		protected byte[] CreatePaddedBuffer()
		{
			uint pad;		//no of padding bits for 448 mod 512
			byte [] bMsg;	//buffer to hold bits
			ulong sizeMsg;		//64 bit size pad
			uint sizeMsgBuff;	//buffer size in multiple of bytes
			int temp=(448-((m_byteInput.Length*8)%512)); //temporary


			pad = (uint )((temp+512)%512);		//getting no of bits to  be pad
			if (pad==0)				///pad is in bits
				pad=512;			//at least 1 or max 512 can be added

			sizeMsgBuff= (uint) ((m_byteInput.Length)+ (pad/8)+8);
			sizeMsg=(ulong)m_byteInput.Length*8;
			bMsg=new byte[sizeMsgBuff];	///no need to pad with 0 coz new bytes
			// are already initialize to 0 :)




			////copying string to buffer
			for (int i =0; i<m_byteInput.Length;i++)
				bMsg[i]=m_byteInput[i];

			bMsg[m_byteInput.Length]|=0x80;		///making first bit of padding 1,

			//wrting the size value
			for (int i =8; i >0;i--)
				bMsg[sizeMsgBuff-i]=(byte) (sizeMsg>>((8-i)*8) & 0x00000000000000ff);

			return bMsg;
		}


		/// <summary>
		/// Copies a 512 bit block into X as 16 32 bit words
		/// </summary>
		/// <param name="bMsg"> source buffer</param>
		/// <param name="block">no of block to copy starting from 0</param>
		protected void CopyBlock(byte[] bMsg,uint block)
		{

			block=block<<6;
			for (uint j=0; j<61;j+=4)
			{
				X[j>>2]=(((uint) bMsg[block+(j+3)]) <<24 ) |
						(((uint) bMsg[block+(j+2)]) <<16 ) |
						(((uint) bMsg[block+(j+1)]) <<8 ) |
						(((uint) bMsg[block+(j)]) ) ;

			}
		}
	}


```


Standard library-based implementation:

```c#

System.Security.Cryptography.MD5CryptoServiceProvider x = new System.Security.Cryptography.MD5CryptoServiceProvider();
byte[] bs = System.Text.Encoding.UTF8.GetBytes(password);
bs = x.ComputeHash(bs); //this function is not in the above classdefinition
System.Text.StringBuilder s = new System.Text.StringBuilder();
foreach (byte b in bs)
{
   s.Append(b.ToString("x2").ToLower());
}
password = s.ToString();

```


## CoffeeScript


```coffeescript

# Array sum helper function.
sum = (array) ->
  array.reduce (x, y) -> x + y

md5 = do ->
  # Per-round shift amounts.
  s = [738695, 669989, 770404, 703814]
  s = (s[i >> 4] >> i % 4 * 5 & 31 for i in [0..63])

  # Constants cache generated by sine.
  K = (Math.floor 2**32 * Math.abs Math.sin i for i in [1..64])

  # Bitwise left rotate helper function.
  lrot = (x, y) ->
    x << y | x >>> 32 - y;

  (input) ->
    # Initialize values.
    d0 = 0x10325476;
    a0 = 0x67452301;
    b0 = ~d0
    c0 = ~a0;

    # Convert the message to 32-bit words, little-endian.
    M =
      for i in [0...input.length] by 4
        sum (input.charCodeAt(i + j) << j*8 for j in [0..3])

    # Pre-processing: append a 1 bit, then message length % 2^64.
    len = input.length * 8
    M[len >> 5] |= 128 << len % 32
    M[(len + 64 >>> 9 << 4) + 14] = len

    # Process the message in chunks of 16 32-bit words.
    for x in [0...M.length] by 16
      [A, B, C, D] = [a0, b0, c0, d0]

      # Main loop.
      for i in [0..63]
        if i < 16
          F = B & C | ~B & D
          g = i
        else if i < 32
          F = B & D | C & ~D
          g = i * 5 + 1
        else if i < 48
          F = B ^ C ^ D
          g = i * 3 + 5
        else
          F = C ^ (B | ~D)
          g = i * 7

        [A, B, C, D] =
          [D, B + lrot(A + F + K[i] + (M[x + g % 16] ? 0), s[i]), B, C]

      a0 += A
      b0 += B
      c0 += C
      d0 += D

    # Convert the four words back to a string.
    return (
      for x in [a0, b0, c0, d0]
        (String.fromCharCode x >>> 8 * y & 255 for y in [0..3]).join ''
    ).join ''

```


This implementation is more focused towards brevity rather than speed. Use a javascript MD5 implementation if speed is desired. Fork this code [https://gist.github.com/Higgs1/08ec61fbb250c1c92151 on github].

Note: this only works on byte strings. To use arbitrary Javascript strings, you must first encode as UTF-8.

And tests:


```coffeescript

str2hex = do ->
  hex = ['0', '1', '2', '3', '4', '5', '6', '7',
         '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']
  hex = (hex[x >> 4] + hex[x & 15] for x in [0..255])
  (str) ->
    (hex[c.charCodeAt()] for c in str).join ''

console.log str2hex md5 message for message in [
  ""
  "a"
  "abc"
  "message digest"
  "abcdefghijklmnopqrstuvwxyz"
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
]

```


Output:


```txt

d41d8cd98f00b204e9800998ecf8427e
0cc175b9c0f1b6a831c399e269772661
900150983cd24fb0d6963f7d28e17f72
f96b697d7cb7938d525a2f31aaf161d0
c3fcd3d76192e4007dfb496cca67e13b
d174ab98d277d9f5a5611c2c9f419d9f
57edf4a22be3c955ac49da2e2107b67a

```



## Common Lisp

This code requires the [https://github.com/cl-babel/babel BABEL] package for converting a string to an octet buffer.


```lisp
(defpackage #:md5
  (:use #:cl))

(in-package #:md5)

(require :babel)

(deftype word () '(unsigned-byte 32))
(deftype octet () '(unsigned-byte 8))
(deftype octets () '(vector octet))

(defparameter *s*
  (make-array 16 :element-type 'word
                 :initial-contents '(7 12 17 22
                                     5  9 14 20
                                     4 11 16 23
                                     6 10 15 21)))

(defun s (i)
  (declare ((integer 0 63) i))
  (aref *s* (+ (ash (ash i -4) 2)
               (ldb (byte 2 0) i))))

(defparameter *k*
  (loop with result = (make-array 64 :element-type 'word)
        for i from 0 below 64
        do (setf (aref result i) (floor (* (ash 1 32) (abs (sin (1+ (float i 1d0)))))))
        finally (return result)))

(defun wrap (bits integer)
  (declare (fixnum bits) (integer integer))
  (ldb (byte bits 0) integer))

(defun integer->8octets (integer)
  (declare (integer integer))
  (loop for n = (wrap 64 integer) then (ash n -8)
        repeat 8
        collect (wrap 8 n)))

(defun pad-octets (octets)
  (declare (octets octets))
  (let* ((octets-length (length octets))
         (zero-pad-length (- 64 (mod (+ octets-length 9) 64)))
         (zero-pads (loop repeat zero-pad-length collect 0)))
    (concatenate 'octets octets '(#x80) zero-pads (integer->8octets (* 8 octets-length)))))

(defun octets->words (octets)
  (declare (octets octets))
  (loop with result = (make-array (/ (length octets) 4) :element-type 'word)
        for n from 0 below (length octets) by 4
        for i from 0
        do (setf (aref result i)
                 (dpb (aref octets (+ n 3)) (byte 8 24)
                      (dpb (aref octets (+ n 2)) (byte 8 16)
                           (dpb (aref octets (1+ n)) (byte 8 8)
                                (dpb (aref octets n) (byte 8 0) 0)))))
        finally (return result)))

(defun words->octets (&rest words)
  (loop for word of-type word in words
        collect (ldb (byte 8 0)  word)
        collect (ldb (byte 8 8)  word)
        collect (ldb (byte 8 16) word)
        collect (ldb (byte 8 24) word)))

(defun left-rotate (x c)
  (declare (integer x) (fixnum c))
  (let ((x (wrap 32 x)))
    (wrap 32 (logior (ash x c)
                     (ash x (- c 32))))))

(defun md5 (string)
  (declare (string string))
  (loop with m = (octets->words (pad-octets (babel:string-to-octets string)))
        with a0 of-type word = #x67452301
        with b0 of-type word = #xefcdab89
        with c0 of-type word = #x98badcfe
        with d0 of-type word = #x10325476
        for j from 0 below (length m) by 16
        do (loop for a of-type word = a0 then d
                 and b of-type word = b0 then new-b
                 and c of-type word = c0 then b
                 and d of-type word = d0 then c
                 for i from 0 below 64
                 for new-b = (multiple-value-bind (f g)
                                 (ecase (ash i -4)
                                   (0 (values (wrap 32 (logior (logand b c)
                                                               (logand (lognot b) d)))
                                              i))
                                   (1 (values (wrap 32 (logior (logand d b)
                                                               (logand (lognot d) c)))
                                              (wrap 4 (1+ (* 5 i)))))
                                   (2 (values (wrap 32 (logxor b c d))
                                              (wrap 4 (+ (* 3 i) 5))))
                                   (3 (values (wrap 32 (logxor c
                                                               (logior b (lognot d))))
                                              (wrap 4 (* 7 i)))))
                               (declare (word f g))
                               (wrap 32 (+ b (left-rotate (+ a f (aref *k* i) (aref m (+ j g)))
                                                          (s i)))))
                 finally (setf a0 (wrap 32 (+ a0 a))
                               b0 (wrap 32 (+ b0 b))
                               c0 (wrap 32 (+ c0 c))
                               d0 (wrap 32 (+ d0 d))))
        finally (return (with-output-to-string (s)
                          (dolist (o (words->octets a0 b0 c0 d0))
                            (format s "~(~2,'0X~)" o))))))

(defun test-cases ()
  (assert (string= "d41d8cd98f00b204e9800998ecf8427e"
                   (md5 "")))
  (assert (string= "0cc175b9c0f1b6a831c399e269772661"
                   (md5 "a")))
  (assert (string= "900150983cd24fb0d6963f7d28e17f72"
                   (md5 "abc")))
  (assert (string= "f96b697d7cb7938d525a2f31aaf161d0"
                   (md5 "message digest")))
  (assert (string= "c3fcd3d76192e4007dfb496cca67e13b"
                   (md5 "abcdefghijklmnopqrstuvwxyz")))
  (assert (string= "d174ab98d277d9f5a5611c2c9f419d9f"
                   (md5 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")))
  (assert (string= "57edf4a22be3c955ac49da2e2107b67a"
                   (md5 "12345678901234567890123456789012345678901234567890123456789012345678901234567890"))))
```



## D

The standard library Phobos included an MD5 module.

This code generates x86 assembly code by compile time functions, then mix-in the assembly code. It only works on x86 machine.

```d
import std.bitmanip, core.stdc.string, std.conv, std.math, std.array,
       std.string;

version (D_InlineAsm_X86) {} else {
    static assert(false, "For X86 machine only.");
}

// CTFE construction of transform expressions.
uint S(in uint n) pure nothrow @safe @nogc {
    static immutable aux = [7u, 12, 17, 22, 5, 9, 14, 20, 4, 11,
                            16, 23, 6, 10, 15, 21];
    return aux[(n / 16) * 4 + (n % 4)];
}

uint K(in uint n) pure nothrow @safe @nogc {
    uint r = 0;
    if (n <= 15)
        r = n;
    else if (n <= 31)
        r = 5 * n + 1;
    else if (n <= 47)
        r = 3 * n + 5;
    else
        r = 7 * n;
    return r % 16;
}

uint T(in uint n) pure nothrow @nogc {
    return cast(uint)(abs(sin(n + 1.0L)) * (2UL ^^ 32));
}

string[] ABCD(in int n) pure nothrow {
    enum abcd = ["EAX", "EBX", "ECX", "EDX"];
    return abcd[(64 - n) % 4 .. 4] ~ abcd[0 .. (64 - n) % 4];
}

string SUB(in int n, in string s) pure nothrow {
    return s
           .replace("ax", n.ABCD[0])
           .replace("bx", n.ABCD[1])
           .replace("cx", n.ABCD[2])
           .replace("dx", n.ABCD[3]);
}

// FF, GG, HH & II expressions part 1 (F, G, H, I).
string fghi1(in int n) pure nothrow @nogc {
    switch (n / 16) {
        case 0:
            // (bb & cc) | (~bb & dd)
            return q{
                        mov ESI, bx;
                        mov EDI, bx;
                        not ESI;
                        and EDI, cx;
                        and ESI, dx;
                        or EDI, ESI;
                        add ax, EDI;
                    };
        case 1:
            // (dd & bb) | (~dd & cc)
            return q{
                        mov ESI, dx;
                        mov EDI, dx;
                        not ESI;
                        and EDI, bx;
                        and ESI, cx;
                        or EDI, ESI;
                        add ax, EDI;
                    };
        case 2: // (bb ^ cc ^ dd)
            return q{
                        mov EDI, bx;
                        xor EDI, cx;
                        xor EDI, dx;
                        add ax, EDI;
                    };
        case 3: // (cc ^ (bb | ~dd))
            return q{
                       mov EDI, dx;
                       not EDI;
                       or EDI, bx;
                       xor EDI, cx;
                       add ax, EDI;
                    };
        default:
            assert(false);
    }
}

// FF, GG, HH & II expressions part 2.
string fghi2(in int n) pure nothrow {
    return q{
                add ax, [EBP + 4 * KK];
                add ax, TT;
            } ~ n.fghi1;
}

// FF, GG, HH & II expressions prepended with previous parts
// & subsitute ABCD.
string FGHI(in int n) pure nothrow {
    // aa = ((aa << SS)|( aa >>> (32 - SS))) + bb = ROL(aa, SS) + bb
    return SUB(n, n.fghi2 ~ q{
                                rol ax, SS;
                                add ax, bx;
                             });
}

string genExpr(uint n) pure nothrow {
    return FGHI(n)
           .replace("SS", n.S.text)
           .replace("KK", n.K.text)
           .replace("TT", "0x" ~ to!string(n.T, 16));
}

string genTransformCode(int n) pure nothrow {
    return (n < 63) ? n.genExpr ~ genTransformCode(n + 1) : n.genExpr;
}

enum string coreZMD5 = 0.genTransformCode;

struct ZMD5 {
    uint[4] state = [0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476];
    ulong count;
    ubyte[64] buffer;

    ubyte[64] padding = [
      0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0];

    private void transform(ubyte* block) pure nothrow @nogc {
        uint[16] x = void;

        version (BigEndian) {
            foreach (immutable i; 0 .. 16)
                x[i] = littleEndianToNative!uint(*cast(ubyte[4]*)&block[i * 4]);
        } else {
            (cast(ubyte*)x.ptr)[0 .. 64] = block[0 .. 64];
        }

        auto pState = state.ptr;
        auto pBuffer = x.ptr;

        asm pure nothrow @nogc {
            mov  ESI, pState[EBP];
            mov  EDX, [ESI + 3 * 4];
            mov  ECX, [ESI + 2 * 4];
            mov  EBX, [ESI + 1 * 4];
            mov  EAX, [ESI + 0 * 4];
            push EBP;
            push ESI;

            mov  EBP, pBuffer[EBP];
        }

        mixin("asm pure nothrow @nogc { " ~ coreZMD5 ~ "}");

        asm pure nothrow @nogc {
            pop ESI;
            pop EBP;
            add [ESI + 0 * 4], EAX;
            add [ESI + 1 * 4], EBX;
            add [ESI + 2 * 4], ECX;
            add [ESI + 3 * 4], EDX;
        }
        x[] = 0;
    }

    void update(in void[] input) pure nothrow @nogc {
        auto inputLen = input.length;
        uint index = (count >> 3) & 0b11_1111U;
        count += inputLen * 8;
        immutable uint partLen = 64 - index;

        uint i;
        if (inputLen >= partLen) {
            memcpy(&buffer[index], input.ptr, partLen);
            transform(buffer.ptr);
            for (i = partLen; i + 63 < inputLen; i += 64)
                transform((cast(ubyte[])input)[i .. i + 64].ptr);
            index = 0;
        } else
            i = 0;

        if (inputLen - i)
            memcpy(&buffer[index], &input[i], inputLen - i);
    }

    void finish(ref ubyte[16] digest) pure nothrow @nogc {
        ubyte[8] bits = void;
        bits[0 .. 8] = nativeToLittleEndian(count)[];

        immutable uint index = (count >> 3) & 0b11_1111U;
        immutable uint padLen = (index < 56) ?
                                (56 - index) : (120 - index);
        update(padding[0 .. padLen]);
        update(bits);

        digest[0 .. 4]   = nativeToLittleEndian(state[0])[];
        digest[4 .. 8]   = nativeToLittleEndian(state[1])[];
        digest[8 .. 12]  = nativeToLittleEndian(state[2])[];
        digest[12 .. 16] = nativeToLittleEndian(state[3])[];

        // Zeroize sensitive information.
        memset(&this, 0, ZMD5.sizeof);
    }
}

string getDigestString(in void[][] data...) pure {
    ZMD5 ctx;
    foreach (datum; data)
        ctx.update(datum);
    ubyte[16] digest;
    ctx.finish(digest);
    return format("%-(%02X%)", digest);
}


void main() { // Benchmark code --------------
    import std.stdio, std.datetime, std.digest.md;

    writefln(`md5  digest("")  = %-(%02X%)`, "".md5Of);
    writefln(`zmd5 digest("")  = %s`, "".getDigestString);

    enum megaBytes = 512;
    writefln("\nTest performance / message size %dMBytes:", megaBytes);
    auto data = new float[megaBytes * 0x40000 + 13];

    StopWatch sw;
    sw.start;
    immutable d1 = data.md5Of;
    sw.stop;
    immutable time1 = sw.peek.msecs / 1000.0;
    writefln("digest(data) = %-(%02X%)", d1);
    writefln("std.md5: %8.2f M/sec  ( %8.2f secs)",
             megaBytes / time1, time1);

    sw.reset;
    sw.start;
    immutable d2 = data.getDigestString;
    sw.stop;
    immutable time2 = sw.peek.msecs / 1000.0;
    writefln("digest(data) = %s", d2);
    writefln("zmd5   : %8.2f M/sec  ( %8.2f secs)",
             megaBytes / time2, time2);
}
```

```txt
md5  digest("")  = D41D8CD98F00B204E9800998ECF8427E
zmd5 digest("")  = D41D8CD98F00B204E9800998ECF8427E

Test performance / message size 512MBytes:
digest(data) = A36190ECA92203A477EFC4DAB966CE6F
std.md5:    45.85 M/sec  (    11.17 secs)
digest(data) = A36190ECA92203A477EFC4DAB966CE6F
zmd5   :   244.86 M/sec  (     2.09 secs)
```


```txt
md5  digest("")  = D41D8CD98F00B204E9800998ECF8427E
zmd5 digest("")  = D41D8CD98F00B204E9800998ECF8427E

Test performance / message size 512MBytes:
digest(data) = A36190ECA92203A477EFC4DAB966CE6F
std.md5:   310.12 M/sec  (     1.65 secs)
digest(data) = A36190ECA92203A477EFC4DAB966CE6F
zmd5   :   277.06 M/sec  (     1.85 secs)
```


As you see this asm is much faster than the D code compiled by dmd, but the D code compiled by ldc2 is a little faster still.


## F#

Pure functional implementation (slower than library function) (Link to original blog [https://znprojects.blogspot.com/2017/04/md5-in-f-functionally.html]):

```F#
let fxyz x y z : uint32 = (x &&& y) ||| (~~~x &&& z)
let gxyz x y z : uint32 = (z &&& x) ||| (~~~z &&& y)
let hxyz x y z : uint32 = x ^^^ y ^^^ z
let ixyz x y z : uint32 = y ^^^ (x ||| ~~~z)
let fghi = [ fxyz; gxyz; hxyz; ixyz ] |> List.collect (List.replicate 16)
let g1Idx = id
let g2Idx i = (5 * i + 1) % 16
let g3Idx i = (3 * i + 5) % 16
let g4Idx i = (7 * i) % 16

let gIdxs =
  [ g1Idx; g2Idx; g3Idx; g4Idx ]
  |> List.collect (List.replicate 16)
  |> List.map2 (fun idx func -> func idx) [ 0..63 ]

let s =
  [ [ 7; 12; 17; 22 ]
    [ 5; 9; 14; 20 ]
    [ 4; 11; 16; 23 ]
    [ 6; 10; 15; 21 ] ]
  |> List.collect (List.replicate 4)
  |> List.concat

let k =
  [ 1...64. ] |> List.map (sin
                           >> abs
                           >> ((*) (2. ** 32.))
                           >> floor
                           >> uint32)

type MD5 =
  { a : uint32
    b : uint32
    c : uint32
    d : uint32 }

let initialMD5 =
  { a = 0x67452301u
    b = 0xefcdab89u
    c = 0x98badcfeu
    d = 0x10325476u }

let md5round (msg : uint32 []) { MD5.a = a; MD5.b = b; MD5.c = c; MD5.d = d } i =
  let rotateL32 r x = (x <<< r) ||| (x >>> (32 - r))
  let f = fghi.[i] b c d
  let a' = b + (a + f + k.[i] + msg.[gIdxs.[i]]
                |> rotateL32 s.[i])
  { a = d
    b = a'
    c = b
    d = c }

let md5plus m (bs : byte []) =
  let msg =
    bs
    |> Array.chunkBySize 4
    |> Array.take 16
    |> Array.map (fun elt -> System.BitConverter.ToUInt32(elt, 0))

  let m' = List.fold (md5round msg) m [ 0..63 ]
  { a = m.a + m'.a
    b = m.b + m'.b
    c = m.c + m'.c
    d = m.d + m'.d }

let padMessage (msg : byte []) =
  let msgLen = Array.length msg
  let msgLenInBits = (uint64 msgLen) * 8UL

  let lastSegmentSize =
    let m = msgLen % 64
    if m = 0 then 64
    else m

  let padLen =
    64 - lastSegmentSize + (if lastSegmentSize >= 56 then 64
                            else 0)

  [| yield 128uy
     for i in 2..padLen - 8 do
       yield 0uy
     for i in 0..7 do
       yield ((msgLenInBits >>> (8 * i)) |> byte) |]
  |> Array.append msg

let md5sum (msg : string) =
  System.Text.Encoding.ASCII.GetBytes msg
  |> padMessage
  |> Array.chunkBySize 64
  |> Array.fold md5plus initialMD5
  |> (fun { MD5.a = a; MD5.b = b; MD5.c = c; MD5.d = d } ->
    System.BitConverter.GetBytes a
    |> (fun x -> System.BitConverter.GetBytes b |> Array.append x)
    |> (fun x -> System.BitConverter.GetBytes c |> Array.append x)
    |> (fun x -> System.BitConverter.GetBytes d |> Array.append x))
  |> Array.map (sprintf "%02X")
  |> Array.reduce (+)
```



## FreeBASIC


```freebasic
' version 19-10-2016
' MD5 from the Wikipedia page "MD5"
' compile with: fbc -s console

' macro for a rotate left
#Macro ROtate_Left (x, n) ' rotate left
  (x) = (x) Shl (n) + (x) Shr (32 - (n))
#EndMacro

Function MD5(test_str As String) As String

  Dim As String message = test_str   ' strings are passed as ByRef's

  Dim As UByte sx, s(0 To ...) = { 7, 12, 17, 22,  7, 12, 17, 22,  7, 12, _
  17, 22,  7, 12, 17, 22,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20, _
   5,  9, 14, 20,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, _
  16, 23,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21 }

  Dim As UInteger<32> K(0 To ...) = { &Hd76aa478, &He8c7b756, &H242070db, _
  &Hc1bdceee, &Hf57c0faf, &H4787c62a, &Ha8304613, &Hfd469501, &H698098d8, _
  &H8b44f7af, &Hffff5bb1, &H895cd7be, &H6b901122, &Hfd987193, &Ha679438e, _
  &H49b40821, &Hf61e2562, &Hc040b340, &H265e5a51, &He9b6c7aa, &Hd62f105d, _
  &H02441453, &Hd8a1e681, &He7d3fbc8, &H21e1cde6, &Hc33707d6, &Hf4d50d87, _
  &H455a14ed, &Ha9e3e905, &Hfcefa3f8, &H676f02d9, &H8d2a4c8a, &Hfffa3942, _
  &H8771f681, &H6d9d6122, &Hfde5380c, &Ha4beea44, &H4bdecfa9, &Hf6bb4b60, _
  &Hbebfbc70, &H289b7ec6, &Heaa127fa, &Hd4ef3085, &H04881d05, &Hd9d4d039, _
  &He6db99e5, &H1fa27cf8, &Hc4ac5665, &Hf4292244, &H432aff97, &Hab9423a7, _
  &Hfc93a039, &H655b59c3, &H8f0ccc92, &Hffeff47d, &H85845dd1, &H6fa87e4f, _
  &Hfe2ce6e0, &Ha3014314, &H4e0811a1, &Hf7537e82, &Hbd3af235, &H2ad7d2bb, _
                                                              &Heb86d391 }

  ' Initialize variables
  Dim As UInteger<32> A, a0 = &H67452301
  Dim As UInteger<32> B, b0 = &Hefcdab89
  Dim As UInteger<32> C, c0 = &H98badcfe
  Dim As UInteger<32> D, d0 = &H10325476
  Dim As UInteger<32> dtemp, F, g, temp

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

  For j = 0 To (l1 -1) \ 64 ' split into block of 64 bytes

    A = a0 : B = b0 : C = c0 : D = d0

    ' break chunk into 16 32bit uinteger
    Dim As UInteger<32> Ptr M = Cast(UInteger<32> Ptr, @message[j * 64])

    For i = 0 To 63
      Select Case As Const i
        Case 0 To 15
          F = (B And C) Or ((Not B) And D)
          g = i
        Case 16 To 31
          F = (B And D) Or (C And (Not D))
          g = (i * 5 +1) Mod 16
        Case 32 To 47
          F = (B Xor C Xor D)
          g = (i * 3 +5) Mod 16
        Case 48 To 63
          F = C Xor (B Or (Not D))
          g = (i * 7) Mod 16
      End Select
      dtemp = D
      D = C
      C = B
      temp = A + F + K(i)+ M[g] : ROtate_left(temp, s(i))
      B = B + temp
      A = dtemp
    Next

    a0 += A : b0 += B : c0 += C : d0 += D

  Next

  Dim As String answer
  ' convert a0, b0, c0 and d0 in hex, then add, low order first
  Dim As String s1 = Hex(a0, 8)
  For i = 7 To 1 Step -2 : answer +=Mid(s1, i, 2) : Next
  s1 = Hex(b0, 8)
  For i = 7 To 1 Step -2 : answer +=Mid(s1, i, 2) : Next
  s1 = Hex(c0, 8)
  For i = 7 To 1 Step -2 : answer +=Mid(s1, i, 2) : Next
  s1 = Hex(d0, 8)
  For i = 7 To 1 Step -2 : answer +=Mid(s1, i, 2) : Next

Return LCase(answer)

End Function


' ------=< MAIN >=------

Dim As String test, hash, md5_hash
Dim As ULong i

For i = 1 To 7
  Read hash, test
  md5_hash = MD5(test)

  Print
  Print test
  Print hash
  Print md5_hash;

  If hash = md5_hash Then
    Print " PASS"
  Else
    Print " FAIL"
    Beep
  End If

Next

' testdata
Data "d41d8cd98f00b204e9800998ecf8427e", ""
Data "0cc175b9c0f1b6a831c399e269772661", "a"
Data "900150983cd24fb0d6963f7d28e17f72", "abc"
Data "f96b697d7cb7938d525a2f31aaf161d0", "message digest"
Data "c3fcd3d76192e4007dfb496cca67e13b", "abcdefghijklmnopqrstuvwxyz"
Data "d174ab98d277d9f5a5611c2c9f419d9f"
Data "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
Data "57edf4a22be3c955ac49da2e2107b67a"
Data "123456789012345678901234567890123456789012345678901234567890" _
                                           + "12345678901234567890"

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt

d41d8cd98f00b204e9800998ecf8427e
d41d8cd98f00b204e9800998ecf8427e PASS

a
0cc175b9c0f1b6a831c399e269772661
0cc175b9c0f1b6a831c399e269772661 PASS

abc
900150983cd24fb0d6963f7d28e17f72
900150983cd24fb0d6963f7d28e17f72 PASS

message digest
f96b697d7cb7938d525a2f31aaf161d0
f96b697d7cb7938d525a2f31aaf161d0 PASS

abcdefghijklmnopqrstuvwxyz
c3fcd3d76192e4007dfb496cca67e13b
c3fcd3d76192e4007dfb496cca67e13b PASS

ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789
d174ab98d277d9f5a5611c2c9f419d9f
d174ab98d277d9f5a5611c2c9f419d9f PASS

12345678901234567890123456789012345678901234567890123456789012345678901234567890
57edf4a22be3c955ac49da2e2107b67a
57edf4a22be3c955ac49da2e2107b67a PASS
```



## Go

A limitation from RFC 1321 is that the function md5 takes a string which is a number of whole bytes.  Messages of arbitrary bit length are not supported.

```go
package main

import (
    "fmt"
    "math"
    "bytes"
    "encoding/binary"
)

type testCase struct {
    hashCode string
    string
}

var testCases = []testCase{
    {"d41d8cd98f00b204e9800998ecf8427e", ""},
    {"0cc175b9c0f1b6a831c399e269772661", "a"},
    {"900150983cd24fb0d6963f7d28e17f72", "abc"},
    {"f96b697d7cb7938d525a2f31aaf161d0", "message digest"},
    {"c3fcd3d76192e4007dfb496cca67e13b", "abcdefghijklmnopqrstuvwxyz"},
    {"d174ab98d277d9f5a5611c2c9f419d9f",
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"},
    {"57edf4a22be3c955ac49da2e2107b67a", "12345678901234567890" +
        "123456789012345678901234567890123456789012345678901234567890"},
}

func main() {
    for _, tc := range testCases {
        fmt.Printf("%s\n%x\n\n", tc.hashCode, md5(tc.string))
    }
}

var shift = [...]uint{7, 12, 17, 22, 5, 9, 14, 20, 4, 11, 16, 23, 6, 10, 15, 21}
var table [64]uint32

func init() {
    for i := range table {
        table[i] = uint32((1 << 32) * math.Abs(math.Sin(float64(i + 1))))
    }
}

func md5(s string) (r [16]byte) {
    padded := bytes.NewBuffer([]byte(s))
    padded.WriteByte(0x80)
    for padded.Len() % 64 != 56 {
        padded.WriteByte(0)
    }
    messageLenBits := uint64(len(s)) * 8
    binary.Write(padded, binary.LittleEndian, messageLenBits)

    var a, b, c, d uint32 = 0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476
    var buffer [16]uint32
    for binary.Read(padded, binary.LittleEndian, buffer[:]) == nil { // read every 64 bytes
        a1, b1, c1, d1 := a, b, c, d
        for j := 0; j < 64; j++ {
            var f uint32
            bufferIndex := j
            round := j >> 4
            switch round {
            case 0:
                f = (b1 & c1) | (^b1 & d1)
            case 1:
                f = (b1 & d1) | (c1 & ^d1)
                bufferIndex = (bufferIndex*5 + 1) & 0x0F
            case 2:
                f = b1 ^ c1 ^ d1
                bufferIndex = (bufferIndex*3 + 5) & 0x0F
            case 3:
                f = c1 ^ (b1 | ^d1)
                bufferIndex = (bufferIndex * 7) & 0x0F
            }
            sa := shift[(round<<2)|(j&3)]
            a1 += f + buffer[bufferIndex] + table[j]
            a1, d1, c1, b1 = d1, c1, b1, a1<<sa|a1>>(32-sa)+b1
        }
        a, b, c, d = a+a1, b+b1, c+c1, d+d1
    }

    binary.Write(bytes.NewBuffer(r[:0]), binary.LittleEndian, []uint32{a, b, c, d})
    return
}
```

Output:

```txt

d41d8cd98f00b204e9800998ecf8427e
d41d8cd98f00b204e9800998ecf8427e

0cc175b9c0f1b6a831c399e269772661
0cc175b9c0f1b6a831c399e269772661

900150983cd24fb0d6963f7d28e17f72
900150983cd24fb0d6963f7d28e17f72

f96b697d7cb7938d525a2f31aaf161d0
f96b697d7cb7938d525a2f31aaf161d0

c3fcd3d76192e4007dfb496cca67e13b
c3fcd3d76192e4007dfb496cca67e13b

d174ab98d277d9f5a5611c2c9f419d9f
d174ab98d277d9f5a5611c2c9f419d9f

57edf4a22be3c955ac49da2e2107b67a
57edf4a22be3c955ac49da2e2107b67a

```


## Groovy


```groovy

class MD5 {

    private static final int INIT_A = 0x67452301
    private static final int INIT_B = (int)0xEFCDAB89L
    private static final int INIT_C = (int)0x98BADCFEL
    private static final int INIT_D = 0x10325476

    private static final int[] SHIFT_AMTS = [
            7, 12, 17, 22,
            5,  9, 14, 20,
            4, 11, 16, 23,
            6, 10, 15, 21
    ]

    private static final int[] TABLE_T = new int[64]
    static
    {
        for (int i in 0..63)
            TABLE_T[i] = (int)(long)((1L << 32) * Math.abs(Math.sin(i + 1)))
    }

    static byte[] computeMD5(byte[] message)
    {
        int messageLenBytes = message.length
        int numBlocks = ((messageLenBytes + 8) >>> 6) + 1
        int totalLen = numBlocks << 6
        byte[] paddingBytes = new byte[totalLen - messageLenBytes]
        paddingBytes[0] = (byte)0x80

        long messageLenBits = (long)messageLenBytes << 3
        for (int i in 0..7)
        {
            paddingBytes[paddingBytes.length - 8 + i] = (byte)messageLenBits
            messageLenBits >>>= 8
        }

        int a = INIT_A
        int b = INIT_B
        int c = INIT_C
        int d = INIT_D
        int[] buffer = new int[16]
        for (int i in 0..(numBlocks - 1))
        {
            int index = i << 6
            for (int j in 0..63) {
                buffer[j >>> 2] = ((int) ((index < messageLenBytes) ? message[index] : paddingBytes[index - messageLenBytes]) << 24) | (buffer[j >>> 2] >>> 8)
                index++
            }
            int originalA = a
            int originalB = b
            int originalC = c
            int originalD = d
            for (int j in 0..63)
            {
                int div16 = j >>> 4
                int f = 0
                int bufferIndex = j
                switch (div16)
                {
                    case 0:
                        f = (b & c) | (~b & d)
                        break

                    case 1:
                        f = (b & d) | (c & ~d)
                        bufferIndex = (bufferIndex * 5 + 1) & 0x0F
                        break

                    case 2:
                        f = b ^ c ^ d
                        bufferIndex = (bufferIndex * 3 + 5) & 0x0F
                        break

                    case 3:
                        f = c ^ (b | ~d)
                        bufferIndex = (bufferIndex * 7) & 0x0F
                        break
                }
                int temp = b + Integer.rotateLeft(a + f + buffer[bufferIndex] + TABLE_T[j], SHIFT_AMTS[(div16 << 2) | (j & 3)])
                a = d
                d = c
                c = b
                b = temp
            }

            a += originalA
            b += originalB
            c += originalC
            d += originalD
        }

        byte[] md5 = new byte[16]
        int count = 0
        for (int i in 0..3)
        {
            int n = (i == 0) ? a : ((i == 1) ? b : ((i == 2) ? c : d))
            for (int j in 0..3)
            {
                md5[count++] = (byte)n
                n >>>= 8
            }
        }
        return md5
    }

    static String toHexString(byte[] b)
    {
        StringBuilder sb = new StringBuilder()
        for (int i in 0..(b.length - 1))
        {
            sb.append(String.format("%02X", b[i] & 0xFF))
        }
        return sb.toString()
    }

    static void main(String[] args)
    {
        String[] testStrings = ["", "a", "abc", "message digest", "abcdefghijklmnopqrstuvwxyz",
                                "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                                "12345678901234567890123456789012345678901234567890123456789012345678901234567890" ]
        for (String s : testStrings)
            System.out.println("0x" + toHexString(computeMD5(s.getBytes())) + " <== \"" + s + "\"")
    }

}


```


## Haskell


```haskell
import Control.Monad (replicateM)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits

import Data.Array (Array, listArray, (!))
import Data.List (foldl)
import Data.Word (Word32)

import Numeric (showHex)


-- functions
type Fun = Word32 -> Word32 -> Word32 -> Word32

funF, funG, funH, funI :: Fun
funF x y z = (x .&. y) .|. (complement x .&. z)
funG x y z = (x .&. z) .|. (complement z .&. y)
funH x y z = x `xor` y `xor` z
funI x y z = y `xor` (complement z .|. x)

idxF, idxG, idxH, idxI :: Int -> Int
idxF i = i
idxG i = (5 * i + 1) `mod` 16
idxH i = (3 * i + 5) `mod` 16
idxI i = 7 * i `mod` 16


-- arrays
funA :: Array Int Fun
funA = listArray (1,64) $ replicate 16 =<< [funF, funG, funH, funI]

idxA :: Array Int Int
idxA = listArray (1,64) $ zipWith ($) (replicate 16 =<< [idxF, idxG, idxH, idxI]) [0..63]

rotA :: Array Int Int
rotA = listArray (1,64) $ concat . replicate 4 =<<
       [[7, 12, 17, 22], [5, 9, 14, 20], [4, 11, 16, 23], [6, 10, 15, 21]]

sinA :: Array Int Word32
sinA = listArray (1,64) $ map (floor . (*mult) . abs . sin) [1..64]
    where mult = 2 ** 32 :: Double


-- to lazily calculate MD5 sum for standart input:
-- main = putStrLn . md5sum =<< BL.getContents

main :: IO ()
main = mapM_ (putStrLn . md5sum . BLC.pack)
        [ ""
        , "a"
        , "abc"
        , "message digest"
        , "abcdefghijklmnopqrstuvwxyz"
        , "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
        , "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
        ]


md5sum :: BL.ByteString -> String
md5sum input =
    let MD5 a b c d = getMD5 initial `runGet` input
    in  foldr hex [] . BL.unpack . runPut $ mapM_ putWord32le [a,b,c,d]
    where
      initial = MD5 0x67452301 0xEFCDAB89 0x98BADCFE 0x10325476

      hex x s | x < 16    = '0' : showHex x s -- quick hack: like "%02x"
              | otherwise =       showHex x s


data MD5 = MD5
    { a :: {-# UNPACK #-} !Word32
    , b :: {-# UNPACK #-} !Word32
    , c :: {-# UNPACK #-} !Word32
    , d :: {-# UNPACK #-} !Word32
    }


getMD5 :: MD5 -> Get MD5
getMD5 md5 = do
  chunk <- getLazyByteString 64
  let len = BL.length chunk

  if len == 64
  then getMD5 $! md5 <+> chunk  -- apply and process next chunk

  else do                       -- input is totally eaten, finalize
    bytes <- bytesRead
    let fin   = runPut . putWord64le $ fromIntegral (bytes - 64 + len) * 8
        pad n = chunk `BL.append` (0x80 `BL.cons` BL.replicate (n - 1) 0x00)

    return $ if len >= 56
        then md5 <+> pad (64 - len) <+> BL.replicate 56 0x00 `BL.append` fin
        else md5 <+> pad (56 - len) `BL.append` fin


(<+>) :: MD5 -> BL.ByteString -> MD5
infixl 5  <+>
md5@(MD5 a b c d) <+> bs =
    let datA = listArray (0,15) $ replicateM 16 getWord32le `runGet` bs
        MD5 a' b' c' d' = foldl' (md5round datA) md5 [1..64]
    in MD5 (a + a') (b + b') (c + c') (d + d')


md5round :: Array Int Word32 -> MD5 -> Int -> MD5
md5round datA (MD5 a b c d) i =
    let f  =  funA ! i
        w  =  datA ! (idxA ! i)
        a' =  b + (a + f b c d + w + sinA ! i) `rotateL` rotA ! i
    in MD5 d a' b c
```


=={{header|Icon}} and {{header|Unicon}}==
The following program is based on part on the Wikipedia pseudo-code and in part on the reference implementation in RFC 1321.  The implementation uses large integers. The solution works in both Icon and Unicon.  One limitation of this implementation is that will not handle arbitrary (bit) length messages - all are byte aligned.  Another small challenge was that Icon/Unicon bit manipulation functions work on signed integers (and large integers), as a result there are no native rotation and negation functions.

```Icon
procedure main()  # validate against the RFC test strings and more
   testMD5("The quick brown fox jumps over the lazy dog", 16r9e107d9d372bb6826bd81d3542a419d6)
   testMD5("The quick brown fox jumps over the lazy dog.", 16re4d909c290d0fb1ca068ffaddf22cbd0)
   testMD5("", 16rd41d8cd98f00b204e9800998ecf8427e)    #R = MD5 test suite from RFC
   testMD5("a", 16r0cc175b9c0f1b6a831c399e269772661)   #R
   testMD5("abc", 16r900150983cd24fb0d6963f7d28e17f72) #R
   testMD5("message digest", 16rf96b697d7cb7938d525a2f31aaf161d0) #R
   testMD5("abcdefghijklmnopqrstuvwxyz", 16rc3fcd3d76192e4007dfb496cca67e13b) #R
   testMD5("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", 16rd174ab98d277d9f5a5611c2c9f419d9f) #R
   testMD5("12345678901234567890123456789012345678901234567890123456789012345678901234567890", 16r57edf4a22be3c955ac49da2e2107b67a) #R
end

procedure testMD5(s,rh)  # compute the MD5 hash and compare it to reference value
   write("Message(length=",*s,") = ",image(s))
   write("Digest = ",hexstring(h := MD5(s)),if h = rh then " matches reference hash" else (" does not match reference hash = " || hexstring(rh)),"\n")
end

link hexcvt # for testMD5

$define B32                     4   #  32 bits
$define B64                     8   #  64 bits in bytes
$define B512                   64   # 512 bits in bytes
$define M32          16r100000000   # 2^32
$define M64  16r10000000000000000   # 2^64

procedure MD5(s)                                    #: return MD5 hash of message s
local w,a,b,c,d,i,t,m
local mlength,message,hash
static rs,ks,istate,maxpad,g

initial {
   every (rs := []) |||:=
      (t := [ 7, 12, 17, 22] | [ 5,  9, 14, 20] | [ 4, 11, 16, 23] | [ 6, 10, 15, 21]) ||| t ||| t ||| t
   every put(ks := [],integer(M32 * abs(sin(i := 1 to 64))))
   istate := [ 16r67452301, 16rEFCDAB89, 16r98BADCFE, 16r10325476 ]  # "Magic" IV
   maxpad := left(char(16r80),B512+B64,char(16r00)) # maximum possible padding
   g := []
   every i := 0 to 63 do                            # precompute offsets
      case round := i/16 of {
         0 : put(g,i + 1)
         1 : put(g,(5*i+1) % 16 + 1)
	 2 : put(g,(3*i+5) % 16 + 1)
         3 : put(g,(7*i) % 16 + 1)
         }
   if not (*rs = *ks = 64) then runerr(500,"MD5 setup error")
   }
                                                    # 1. Construct prefix
   t := (*s*8)%M64                                  # original message length
   s ||:= maxpad                                    # append maximum padding
   s[0-:*s%B512] := ""                              # trim to final length
   s[0-:B64] := reverse(unsigned2string(t,B64) )    # as little endian length

   message := []                                    # 2. Subdivide message
   s ? while put(message,move(B512))                #  into 512 bit blocks

                                                    # 3. Transform message ...
   state := copy(istate)                            # Initialize hashes
   every m := !message do {                         # For each message block
      w := []
      m ? while put(w,unsigned(reverse(move(B32)))) # break into little-endian words

      a := state[1]                                 # pick up hashes
      b := state[2]
      c := state[3]
      d := state[4]

      every i := 1 to 64 do  {                      # Process 4 rounds of hashes
         case round := (i-1)/16 of {
	    0 : a +:= ixor(d, iand(b,ixor(c,d)))          # 0..15  - alternate F
            1 : a +:= ixor(c,iand(d,ixor(b,c)))           # 16..31 - alternate G
            2 : a +:= ixor(b,ixor(c,d))                   # 32..47 - H
            3 : a +:= ixor(c,ior(b,ixor(d,16rffffffff)))  # 48..64 - alternate I
	    }                                       # Core of FF, GG, HH, II
         a +:= ks[i] + w[g[i]]                      # and the rest
         a %:= M32
         a := ior( ishift(a,rs[i]), ishift(a,-(32-rs[i]))) # 32bit rotate
         a +:= b
         a :=: b :=: c :=: d                        # rotate variables
	 }

      state[1] +:= a                                # Add back new hashes
      state[2] +:= b
      state[3] +:= c
      state[4] +:= d
      every !state %:= M32                          # mod 2^32
   }
   every (hash := "") ||:= reverse(unsigned2string(!state,4)) # little-endian digest
   return unsigned(hash)
end

procedure unsigned2string(i,w)                      # uint to string pad to w bytes
local s
   if i < 0 then runerr(500,i)
   s := ""
   while (0 < i) | (*s < \w) do {
      s ||:= char(i % 256)
      i /:= 256
      }
   return reverse(s)
end

link unsigned                                       # string to unsigned integer
```


The {{libheader|Icon Programming Library}} provides [http://www.cs.arizona.edu/icon/library/src/procs/unsigned.icn unsigned] and [http://www.cs.arizona.edu/icon/library/src/procs/hexcvt.icn hexcvt]

Sample Output (abridged):
```txt
Message(length=43) = "The quick brown fox jumps over the lazy dog"
Digest = 9E107D9D372BB6826BD81D3542A419D6 matches reference hash

Message(length=44) = "The quick brown fox jumps over the lazy dog."
Digest = E4D909C290D0FB1CA068FFADDF22CBD0 matches reference hash

Message(length=0) = ""
Digest = D41D8CD98F00B204E9800998ECF8427E matches reference hash

Message(length=1) = "a"
Digest = CC175B9C0F1B6A831C399E269772661 matches reference hash
...
```



## J


Note: the following code was extracted from http://www.jsoftware.com/wsvn/addons/trunk/convert/misc/md5.ijs


```j
NB. convert/misc/md5
NB. RSA Data Security, Inc. MD5 Message-Digest Algorithm
NB. version: 1.0.2
NB.
NB. See RFC 1321 for license details
NB. J implementation -- (C) 2003 Oleg Kobchenko;
NB.
NB. 09/04/2003 Oleg Kobchenko
NB. 03/31/2007 Oleg Kobchenko j601, JAL
NB. 12/17/2015 G.Pruss 64-bit
NB. ~60+ times slower than using the jqt library

require 'convert'
coclass 'pcrypt'

NB. lt= (*. -.)~   gt= *. -.   ge= +. -.   xor= ~:
'`lt gt ge xor'=: (20 b.)`(18 b.)`(27 b.)`(22 b.)
'`and or sh'=: (17 b.)`(23 b.)`(33 b.)

3 : 0 ''
if. IF64 do.
rot=: (16bffffffff and sh or ] sh~ 32 -~ [) NB. (y << x) | (y >>> (32 - x))
add=: ((16bffffffff&and)@+)"0
else.
rot=: (32 b.)
add=: (+&(_16&sh) (16&sh@(+ _16&sh) or and&65535@]) +&(and&65535))"0
end.
EMPTY
)

hexlist=: tolower@:,@:hfd@:,@:(|."1)@(256 256 256 256&#:)

cmn=: 4 : 0
'x s t'=. x [ 'q a b'=. y
b add s rot (a add q) add (x add t)
)

ff=: cmn (((1&{ and 2&{) or 1&{ lt 3&{) , 2&{.)
gg=: cmn (((1&{ and 3&{) or 2&{ gt 3&{) , 2&{.)
hh=: cmn (((1&{ xor 2&{)xor 3&{       ) , 2&{.)
ii=: cmn (( 2&{ xor 1&{  ge 3&{       ) , 2&{.)
op=: ff`gg`hh`ii

I=: ".;._2(0 : 0)
0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
1 6 11 0 5 10 15 4 9 14 3 8 13 2 7 12
5 8 11 14 1 4 7 10 13 0 3 6 9 12 15 2
0 7 14 5 12 3 10 1 8 15 6 13 4 11 2 9
)
S=: 4 4$7 12 17 22 5 9 14 20 4 11 16 23 6 10 15 21

T=: |:".;._2(0 : 0)
 _680876936  _165796510     _378558  _198630844
 _389564586 _1069501632 _2022574463  1126891415
  606105819   643717713  1839030562 _1416354905
_1044525330  _373897302   _35309556   _57434055
 _176418897  _701558691 _1530992060  1700485571
 1200080426    38016083  1272893353 _1894986606
_1473231341  _660478335  _155497632    _1051523
  _45705983  _405537848 _1094730640 _2054922799
 1770035416   568446438   681279174  1873313359
_1958414417 _1019803690  _358537222   _30611744
     _42063  _187363961  _722521979 _1560198380
_1990404162  1163531501    76029189  1309151649
 1804603682 _1444681467  _640364487  _145523070
  _40341101   _51403784  _421815835 _1120210379
_1502002290  1735328473   530742520   718787259
 1236535329 _1926607734  _995338651  _343485551
)

norm=: 3 : 0
n=. 16 * 1 + _6 sh 8 + #y
b=. n#0  [  y=. a.i.y
for_i. i. #y do.
  b=. ((j { b) or (8*4|i) sh i{y) (j=. _2 sh i) } b
end.
b=. ((j { b) or (8*4|i) sh 128) (j=._2 sh i=.#y) } b
_16]\ (8 * #y) (n-2) } b
)

NB.*md5 v MD5 Message-Digest Algorithm
NB.  diagest=. md5 message
md5=: 3 : 0
X=. norm y
q=. r=. 1732584193 _271733879 _1732584194 271733878
for_x. X do.
  for_j. i.4 do.
    l=. ((j{I){x) ,. (16$j{S) ,. j{T
    for_i. i.16 do.
      r=. _1|.((i{l) (op@.j) r),}.r
    end.
  end.
  q=. r=. r add q
end.
hexlist r
)

md5_z_=: md5_pcrypt_
```



```j
   md5''
d41d8cd98f00b204e9800998ecf8427e
   md5'a'
0cc175b9c0f1b6a831c399e269772661
   md5'abc'
900150983cd24fb0d6963f7d28e17f72
   md5'message digest'
f96b697d7cb7938d525a2f31aaf161d0
   md5'abcdefghijklmnopqrstuvwxyz'
c3fcd3d76192e4007dfb496cca67e13b
   md5'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'
d174ab98d277d9f5a5611c2c9f419d9f
   md5'12345678901234567890123456789012345678901234567890123456789012345678901234567890'
57edf4a22be3c955ac49da2e2107b67a

```



## Java

Based on RFC-1321.

```java
class MD5
{

  private static final int INIT_A = 0x67452301;
  private static final int INIT_B = (int)0xEFCDAB89L;
  private static final int INIT_C = (int)0x98BADCFEL;
  private static final int INIT_D = 0x10325476;

  private static final int[] SHIFT_AMTS = {
    7, 12, 17, 22,
    5,  9, 14, 20,
    4, 11, 16, 23,
    6, 10, 15, 21
  };

  private static final int[] TABLE_T = new int[64];
  static
  {
    for (int i = 0; i < 64; i++)
      TABLE_T[i] = (int)(long)((1L << 32) * Math.abs(Math.sin(i + 1)));
  }

  public static byte[] computeMD5(byte[] message)
  {
    int messageLenBytes = message.length;
    int numBlocks = ((messageLenBytes + 8) >>> 6) + 1;
    int totalLen = numBlocks << 6;
    byte[] paddingBytes = new byte[totalLen - messageLenBytes];
    paddingBytes[0] = (byte)0x80;

    long messageLenBits = (long)messageLenBytes << 3;
    for (int i = 0; i < 8; i++)
    {
      paddingBytes[paddingBytes.length - 8 + i] = (byte)messageLenBits;
      messageLenBits >>>= 8;
    }

    int a = INIT_A;
    int b = INIT_B;
    int c = INIT_C;
    int d = INIT_D;
    int[] buffer = new int[16];
    for (int i = 0; i < numBlocks; i ++)
    {
      int index = i << 6;
      for (int j = 0; j < 64; j++, index++)
        buffer[j >>> 2] = ((int)((index < messageLenBytes) ? message[index] : paddingBytes[index - messageLenBytes]) << 24) | (buffer[j >>> 2] >>> 8);
      int originalA = a;
      int originalB = b;
      int originalC = c;
      int originalD = d;
      for (int j = 0; j < 64; j++)
      {
        int div16 = j >>> 4;
        int f = 0;
        int bufferIndex = j;
        switch (div16)
        {
          case 0:
            f = (b & c) | (~b & d);
            break;

          case 1:
            f = (b & d) | (c & ~d);
            bufferIndex = (bufferIndex * 5 + 1) & 0x0F;
            break;

          case 2:
            f = b ^ c ^ d;
            bufferIndex = (bufferIndex * 3 + 5) & 0x0F;
            break;

          case 3:
            f = c ^ (b | ~d);
            bufferIndex = (bufferIndex * 7) & 0x0F;
            break;
        }
        int temp = b + Integer.rotateLeft(a + f + buffer[bufferIndex] + TABLE_T[j], SHIFT_AMTS[(div16 << 2) | (j & 3)]);
        a = d;
        d = c;
        c = b;
        b = temp;
      }

      a += originalA;
      b += originalB;
      c += originalC;
      d += originalD;
    }

    byte[] md5 = new byte[16];
    int count = 0;
    for (int i = 0; i < 4; i++)
    {
      int n = (i == 0) ? a : ((i == 1) ? b : ((i == 2) ? c : d));
      for (int j = 0; j < 4; j++)
      {
        md5[count++] = (byte)n;
        n >>>= 8;
      }
    }
    return md5;
  }

  public static String toHexString(byte[] b)
  {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < b.length; i++)
    {
      sb.append(String.format("%02X", b[i] & 0xFF));
    }
    return sb.toString();
  }

  public static void main(String[] args)
  {
    String[] testStrings = { "", "a", "abc", "message digest", "abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", "12345678901234567890123456789012345678901234567890123456789012345678901234567890" };
    for (String s : testStrings)
      System.out.println("0x" + toHexString(computeMD5(s.getBytes())) + " <== \"" + s + "\"");
    return;
  }

}
```


<b>Output:</b>

```txt
0xD41D8CD98F00B204E9800998ECF8427E <== ""
0x0CC175B9C0F1B6A831C399E269772661 <== "a"
0x900150983CD24FB0D6963F7D28E17F72 <== "abc"
0xF96B697D7CB7938D525A2F31AAF161D0 <== "message digest"
0xC3FCD3D76192E4007DFB496CCA67E13B <== "abcdefghijklmnopqrstuvwxyz"
0xD174AB98D277D9F5A5611C2C9F419D9F <== "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
0x57EDF4A22BE3C955AC49DA2E2107B67A <== "12345678901234567890123456789012345678901234567890123456789012345678901234567890"

```


Using <code>ByteBuffer</code>s

```java
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

class MD5
{

  private static final int INIT_A = 0x67452301;
  private static final int INIT_B = (int)0xEFCDAB89L;
  private static final int INIT_C = (int)0x98BADCFEL;
  private static final int INIT_D = 0x10325476;

  private static final int[] SHIFT_AMTS = {
    7, 12, 17, 22,
    5,  9, 14, 20,
    4, 11, 16, 23,
    6, 10, 15, 21
  };

  private static final int[] TABLE_T = new int[64];
  static
  {
    for (int i = 0; i < 64; i++)
      TABLE_T[i] = (int)(long)((1L << 32) * Math.abs(Math.sin(i + 1)));
  }

  public static byte[] computeMD5(byte[] message)
  {
    ByteBuffer padded = ByteBuffer.allocate((((message.length + 8) / 64) + 1) * 64).order(ByteOrder.LITTLE_ENDIAN);
    padded.put(message);
    padded.put((byte)0x80);
    long messageLenBits = (long)message.length * 8;
    padded.putLong(padded.capacity() - 8, messageLenBits);

    padded.rewind();

    int a = INIT_A;
    int b = INIT_B;
    int c = INIT_C;
    int d = INIT_D;
    while (padded.hasRemaining()) {
      // obtain a slice of the buffer from the current position,
      // and view it as an array of 32-bit ints
      IntBuffer chunk = padded.slice().order(ByteOrder.LITTLE_ENDIAN).asIntBuffer();
      int originalA = a;
      int originalB = b;
      int originalC = c;
      int originalD = d;
      for (int j = 0; j < 64; j++)
      {
        int div16 = j >>> 4;
        int f = 0;
        int bufferIndex = j;
        switch (div16)
        {
          case 0:
            f = (b & c) | (~b & d);
            break;

          case 1:
            f = (b & d) | (c & ~d);
            bufferIndex = (bufferIndex * 5 + 1) & 0x0F;
            break;

          case 2:
            f = b ^ c ^ d;
            bufferIndex = (bufferIndex * 3 + 5) & 0x0F;
            break;

          case 3:
            f = c ^ (b | ~d);
            bufferIndex = (bufferIndex * 7) & 0x0F;
            break;
        }
        int temp = b + Integer.rotateLeft(a + f + chunk.get(bufferIndex) + TABLE_T[j], SHIFT_AMTS[(div16 << 2) | (j & 3)]);
        a = d;
        d = c;
        c = b;
        b = temp;
      }

      a += originalA;
      b += originalB;
      c += originalC;
      d += originalD;
      padded.position(padded.position() + 64);
    }

    ByteBuffer md5 = ByteBuffer.allocate(16).order(ByteOrder.LITTLE_ENDIAN);
    for (int n : new int[]{a, b, c, d})
    {
      md5.putInt(n);
    }
    return md5.array();
  }

  public static String toHexString(byte[] b)
  {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < b.length; i++)
    {
      sb.append(String.format("%02X", b[i] & 0xFF));
    }
    return sb.toString();
  }

  public static void main(String[] args)
  {
    String[] testStrings = { "", "a", "abc", "message digest", "abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", "12345678901234567890123456789012345678901234567890123456789012345678901234567890" };
    for (String s : testStrings)
      System.out.println("0x" + toHexString(computeMD5(s.getBytes())) + " <== \"" + s + "\"");
    return;
  }

}
```


<b>Output:</b>

```txt
0xD41D8CD98F00B204E9800998ECF8427E <== ""
0x0CC175B9C0F1B6A831C399E269772661 <== "a"
0x900150983CD24FB0D6963F7D28E17F72 <== "abc"
0xF96B697D7CB7938D525A2F31AAF161D0 <== "message digest"
0xC3FCD3D76192E4007DFB496CCA67E13B <== "abcdefghijklmnopqrstuvwxyz"
0xD174AB98D277D9F5A5611C2C9F419D9F <== "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
0x57EDF4A22BE3C955AC49DA2E2107B67A <== "12345678901234567890123456789012345678901234567890123456789012345678901234567890"

```




## Julia


```julia
# a rather literal translation of the pseudocode at https://en.wikipedia.org/wiki/MD5

const s = UInt32[7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
                 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
                 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,  4, 11, 16, 23,
                 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21]

const K = UInt32[0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf,
    0x4787c62a, 0xa8304613, 0xfd469501, 0x698098d8, 0x8b44f7af, 0xffff5bb1,
    0x895cd7be, 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821, 0xf61e2562,
    0xc040b340, 0x265e5a51, 0xe9b6c7aa, 0xd62f105d, 0x02441453, 0xd8a1e681,
    0xe7d3fbc8, 0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed, 0xa9e3e905,
    0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a, 0xfffa3942, 0x8771f681, 0x6d9d6122,
    0xfde5380c, 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70, 0x289b7ec6,
    0xeaa127fa, 0xd4ef3085, 0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8,
    0xc4ac5665, 0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3,
    0x8f0ccc92, 0xffeff47d, 0x85845dd1, 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314,
    0x4e0811a1, 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391]

function md5(msgbytes)
    a0::UInt32 = 0x67452301  # A
    b0::UInt32 = 0xefcdab89  # B
    c0::UInt32 = 0x98badcfe  # C
    d0::UInt32 = 0x10325476  # D

    oldlen = length(msgbytes)
    umsg = push!([UInt8(b) for b in msgbytes], UInt8(0x80))
    while length(umsg) % 64 != 56
        push!(umsg, UInt8(0))
    end
    append!(umsg, reinterpret(UInt8, [htol(UInt64(oldlen) * 8)]))

    for j in 1:64:length(umsg)-1
        arr = view(umsg, j:j+63)
        M = [reinterpret(UInt32, arr[k:k+3])[1] for k in 1:4:62]
        A = a0
        B = b0
        C = c0
        D = d0

        for i in 0:63
            if 0 ≤ i ≤ 15
                F = D ⊻ (B & (C ⊻ D))
                g = i
            elseif 16 ≤ i ≤ 31
                F = C ⊻ (D & (B ⊻ C))
                g = (5 * i + 1) % 16
            elseif 32 ≤ i ≤ 47
                F = B ⊻ C ⊻ D
                g = (3 * i + 5) % 16
            elseif 48 ≤ i ≤ 63
                F = C ⊻ (B | (~D))
                g = (7 * i) % 16
            end
            F += A + K[i+1] + M[g+1]
            A = D
            D = C
            C = B
            B += ((F) << s[i+1]) | (F >> (32 - s[i+1]))
        end

        a0 += A
        b0 += B
        c0 += C
        d0 += D
    end
    digest = join(map(x -> lpad(string(x, base=16), 2, '0'), reinterpret(UInt8, [a0, b0, c0, d0])), "") # Output is in little-endian
end

for pair in [0xd41d8cd98f00b204e9800998ecf8427e => "", 0x0cc175b9c0f1b6a831c399e269772661 => "a",
   0x900150983cd24fb0d6963f7d28e17f72 => "abc", 0xf96b697d7cb7938d525a2f31aaf161d0 => "message digest",
   0xc3fcd3d76192e4007dfb496cca67e13b => "abcdefghijklmnopqrstuvwxyz",
   0xd174ab98d277d9f5a5611c2c9f419d9f => "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
   0x57edf4a22be3c955ac49da2e2107b67a => "12345678901234567890123456789012345678901234567890123456789012345678901234567890"]
   println("MD5 of $(pair[2]) is $(md5(pair[2])), which checks with $(string(pair[1], base=16)).")
end

```
```txt

MD5 of  is d41d8cd98f00b204e9800998ecf8427e, which checks with d41d8cd98f00b204e9800998ecf8427e.
MD5 of a is 0cc175b9c0f1b6a831c399e269772661, which checks with cc175b9c0f1b6a831c399e269772661.
MD5 of abc is 900150983cd24fb0d6963f7d28e17f72, which checks with 900150983cd24fb0d6963f7d28e17f72.
MD5 of message digest is f96b697d7cb7938d525a2f31aaf161d0, which checks with f96b697d7cb7938d525a2f31aaf161d0.
MD5 of abcdefghijklmnopqrstuvwxyz is c3fcd3d76192e4007dfb496cca67e13b, which checks with c3fcd3d76192e4007dfb496cca67e13b.
MD5 of ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 is d174ab98d277d9f5a5611c2c9f419d9f, which checks with d174ab98d277d9f5a5611c2c9f419d9f.
MD5 of 12345678901234567890123456789012345678901234567890123456789012345678901234567890 is 57edf4a22be3c955ac49da2e2107b67a, which checks with 57edf4a22be3c955ac49da2e2107b67a.

```



## Kotlin

```scala
// version 1.1.3

object MD5 {

    private val INIT_A = 0x67452301
    private val INIT_B = 0xEFCDAB89L.toInt()
    private val INIT_C = 0x98BADCFEL.toInt()
    private val INIT_D = 0x10325476

    private val SHIFT_AMTS = intArrayOf(
        7, 12, 17, 22,
        5,  9, 14, 20,
        4, 11, 16, 23,
        6, 10, 15, 21
    )

    private val TABLE_T = IntArray(64) {
        ((1L shl 32) * Math.abs(Math.sin(it + 1.0))).toLong().toInt()
    }

    fun compute(message: ByteArray): ByteArray {
        val messageLenBytes = message.size
        val numBlocks = ((messageLenBytes + 8) ushr 6) + 1
        val totalLen = numBlocks shl 6
        val paddingBytes = ByteArray(totalLen - messageLenBytes)
        paddingBytes[0] = 0x80.toByte()
        var messageLenBits = (messageLenBytes shl 3).toLong()

        for (i in 0..7) {
            paddingBytes[paddingBytes.size - 8 + i] = messageLenBits.toByte()
            messageLenBits = messageLenBits ushr 8
        }

        var a = INIT_A
        var b = INIT_B
        var c = INIT_C
        var d = INIT_D
        val buffer = IntArray(16)

        for (i in 0 until numBlocks) {
            var index = i shl 6

            for (j in 0..63) {
                val temp = if (index < messageLenBytes) message[index] else
                               paddingBytes[index - messageLenBytes]
                buffer[j ushr 2] = (temp.toInt() shl 24) or (buffer[j ushr 2] ushr 8)
                index++
            }

            val originalA = a
            val originalB = b
            val originalC = c
            val originalD = d

            for (j in 0..63) {
                val div16 = j ushr 4
                var f = 0
                var bufferIndex = j
                when (div16) {
                    0 -> {
                        f = (b and c) or (b.inv() and d)
                    }

                    1 -> {
                        f = (b and d) or (c and d.inv())
                        bufferIndex = (bufferIndex * 5 + 1) and 0x0F
                    }

                    2 -> {
                        f = b xor c xor d;
                        bufferIndex = (bufferIndex * 3 + 5) and 0x0F
                    }

                    3 -> {
                        f = c xor (b or d.inv());
                        bufferIndex = (bufferIndex * 7) and 0x0F
                    }
                }

                val temp = b + Integer.rotateLeft(a + f + buffer[bufferIndex] +
                           TABLE_T[j], SHIFT_AMTS[(div16 shl 2) or (j and 3)])
                a = d
                d = c
                c = b
                b = temp
            }

            a += originalA
            b += originalB
            c += originalC
            d += originalD
        }

        val md5 = ByteArray(16)
        var count = 0

        for (i in 0..3) {
            var n = if (i == 0) a else (if (i == 1) b else (if (i == 2) c else d))

            for (j in 0..3) {
                md5[count++] = n.toByte()
                n = n ushr 8
            }
        }
        return md5
    }
}

fun ByteArray.toHexString(): String {
    val sb = StringBuilder()
    for (b in this) sb.append(String.format("%02x", b.toInt() and 0xFF))
    return sb.toString()
}

fun main(args: Array<String>) {
    val testStrings = arrayOf(
        "",
        "a",
        "abc",
        "message digest",
        "abcdefghijklmnopqrstuvwxyz",
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
        "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
    )

    println("${"hash code".padStart(34)} <== string")
    for (s in testStrings) {
        println("0x${MD5.compute(s.toByteArray()).toHexString()} <== \"$s\"")
    }
}
```


```txt

                         hash code <== string
0xd41d8cd98f00b204e9800998ecf8427e <== ""
0x0cc175b9c0f1b6a831c399e269772661 <== "a"
0x900150983cd24fb0d6963f7d28e17f72 <== "abc"
0xf96b697d7cb7938d525a2f31aaf161d0 <== "message digest"
0xc3fcd3d76192e4007dfb496cca67e13b <== "abcdefghijklmnopqrstuvwxyz"
0xd174ab98d277d9f5a5611c2c9f419d9f <== "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
0x57edf4a22be3c955ac49da2e2107b67a <== "12345678901234567890123456789012345678901234567890123456789012345678901234567890"

```



## Liberty BASIC

''See the implementation at [[MD5#Liberty BASIC]].''


## Lingo


```Lingo
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
    a = 1732584193
    b = -271733879
    c = -1732584194
    d = 271733878
    i = 1
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



```Lingo
tests = []
tests.add("")
tests.add("a")
tests.add("abc")
tests.add("message digest")
tests.add("abcdefghijklmnopqrstuvwxyz")
tests.add("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
tests.add("12345678901234567890123456789012345678901234567890123456789012345678901234567890")
repeat with t in tests
    ba = md5(t)
    put ba.toHexString(1, ba.length)
end repeat
```


```txt

-- "d4 1d 8c d9 8f 00 b2 04 e9 80 09 98 ec f8 42 7e"
-- "0c c1 75 b9 c0 f1 b6 a8 31 c3 99 e2 69 77 26 61"
-- "90 01 50 98 3c d2 4f b0 d6 96 3f 7d 28 e1 7f 72"
-- "f9 6b 69 7d 7c b7 93 8d 52 5a 2f 31 aa f1 61 d0"
-- "c3 fc d3 d7 61 92 e4 00 7d fb 49 6c ca 67 e1 3b"
-- "d1 74 ab 98 d2 77 d9 f5 a5 61 1c 2c 9f 41 9d 9f"
-- "57 ed f4 a2 2b e3 c9 55 ac 49 da 2e 21 07 b6 7a"

```



## Mathematica


```Mathematica
md5[string_String] :=
 Module[{r = {7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17,
     22, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 4,
     11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 6, 10,
     15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21},
   k = Table[Floor[2^32*Abs@Sin@i], {i, 1, 64}], h0 = 16^^67452301,
   h1 = 16^^efcdab89, h2 = 16^^98badcfe, h3 = 16^^10325476,
   data = Partition[
     Join[FromDigits[Reverse@#, 256] & /@
         Partition[
          PadRight[Append[#, 128], Mod[56, 64, Length@# + 1]], 4],
        Reverse@IntegerDigits[8 Length@#, 2^32, 2]] &@
      ImportString[string, "Binary"], 16], a, b, c, d, f, g},
  Do[{a, b, c, d} = {h0, h1, h2, h3};
   Do[Which[1 <= i <= 16,
     f = BitOr[BitAnd[b, c], BitAnd[BitNot[b], d]]; g = i - 1,
     17 <= i <= 32, f = BitOr[BitAnd[d, b], BitAnd[BitNot[d], c]];
     g = Mod[5 i - 4, 16], 33 <= i <= 48, f = BitXor[b, c, d];
     g = Mod[3 i + 2, 16], 49 <= i <= 64,
     f = BitXor[c, BitOr[b, BitNot[d] + 2^32]];
     g = Mod[7 i - 7, 16]]; {a, b, c, d} = {d,
      BitOr[BitShiftLeft[#1, #2], BitShiftRight[#1, 32 - #2]] &[
        Mod[a + f + k[[i]] + w[[g + 1]], 2^32], r[[i]]] + b, b,
      c}, {i, 1, 64}]; {h0, h1, h2, h3} =
    Mod[{h0, h1, h2, h3} + {a, b, c, d}, 2^32], {w, data}];
  "0x" ~~ IntegerString[
    FromDigits[
     Flatten[Reverse@IntegerDigits[#, 256, 4] & /@ {h0, h1, h2, h3}],
     256], 16, 32]]

```

Example:

```Mathematica
md5["12345678901234567890123456789012345678901234567890123456789012345678901234567890"]
```

Output:

```Mathematica>0x57edf4a22be3c955ac49da2e2107b67a</lang


=={{header|MATLAB}} / {{header|Octave}}==
''See the implementation at [[MD5#MATLAB]].''

=={{header|Modula-3}}==


```modula3
INTERFACE MD5;

IMPORT Word;

TYPE Digest = ARRAY [0..15] OF CHAR;
TYPE Buffer = ARRAY [0..63] OF CHAR;

TYPE T = RECORD
  state: ARRAY [0..3] OF Word.T;
  count: ARRAY [0..1] OF Word.T;
  buffer: Buffer;
END;

PROCEDURE Init(VAR md5ctx: T);
PROCEDURE Update(VAR md5ctx: T; input: TEXT);
PROCEDURE Final(VAR md5ctx: T): Digest;
PROCEDURE ToText(hash: Digest): TEXT;

END MD5.
```


```modula3
MODULE MD5;

IMPORT Word, Text, Fmt;

CONST S11 = 7; S12 = 12; S13 = 17; S14 = 22;
      S21 = 5; S22 = 9; S23 = 14; S24 = 20;
      S31 = 4; S32 = 11; S33 = 16; S34 = 23;
      S41 = 6; S42 = 10; S43 = 15; S44 = 21;
      pad1 = "\200\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000";
      pad2 = "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000";
      pad3 = "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000";
      pad4 = "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000";
      padding = pad1 & pad2 & pad3 & pad4;

PROCEDURE Init(VAR md5ctx: T) =
  BEGIN
    <*ASSERT Word.Size = 32*>
    md5ctx.count[0] := 0;
    md5ctx.count[1] := 0;

    md5ctx.state[0] := 16_67452301;
    md5ctx.state[1] := 16_efcdab89;
    md5ctx.state[2] := 16_98badcfe;
    md5ctx.state[3] := 16_10325476;
  END Init;

PROCEDURE Transform(VAR state: ARRAY [0..3] OF Word.T;
                    VAR input: Buffer) =
  VAR a, b, c, d: INTEGER;
      x: ARRAY [0..15] OF INTEGER;

  PROCEDURE Decode(VAR x: ARRAY [0..15] OF INTEGER;
                   VAR input: Buffer) =
    BEGIN
      FOR i := 0 TO 15 DO
        x[i] := Word.Insert(x[i], ORD(input[4*i+0]), 0, 8);
        x[i] := Word.Insert(x[i], ORD(input[4*i+1]), 8, 8);
        x[i] := Word.Insert(x[i], ORD(input[4*i+2]), 16, 8);
        x[i] := Word.Insert(x[i], ORD(input[4*i+3]), 24, 8);
      END;
    END Decode;

  PROCEDURE FF(VAR a: INTEGER; b, c, d, x, s, ac: INTEGER) =
    PROCEDURE F(x, y, z: INTEGER): INTEGER =
      BEGIN
        RETURN Word.Or(Word.And(x, y), Word.And(Word.Not(x), z));
      END F;
    BEGIN
      a := b + Word.Rotate(a + F(b, c, d) + x + ac, s);
    END FF;

  PROCEDURE GG(VAR a: INTEGER; b, c, d, x, s, ac: INTEGER) =
    PROCEDURE G(x, y, z: INTEGER): INTEGER =
      BEGIN
        RETURN Word.Or(Word.And(x, z), Word.And(y, Word.Not(z)));
      END G;
    BEGIN
      a := b + Word.Rotate(a + G(b, c, d) + x + ac, s);
    END GG;

  PROCEDURE HH(VAR a: INTEGER; b, c, d, x, s, ac: INTEGER) =
    PROCEDURE H(x, y, z: INTEGER): INTEGER =
      BEGIN
        RETURN Word.Xor(x, Word.Xor(y,z));
      END H;
    BEGIN
      a := b + Word.Rotate(a + H(b, c, d) + x + ac, s);
    END HH;

  PROCEDURE II(VAR a: INTEGER; b, c, d, x, s, ac: INTEGER) =
    PROCEDURE I(x, y, z: INTEGER): INTEGER =
      BEGIN
        RETURN Word.Xor(y, Word.Or(x, Word.Not(z)))
      END I;
    BEGIN
      a := b + Word.Rotate(a + I(b, c, d) + x + ac, s)
    END II;

  BEGIN
    Decode(x, input);

    a := state[0];
    b := state[1];
    c := state[2];
    d := state[3];

    (* Round 1 *)
    FF(a, b, c, d, x[ 0], S11, 16_d76aa478); (* 1 *)
    FF(d, a, b, c, x[ 1], S12, 16_e8c7b756); (* 2 *)
    FF(c, d, a, b, x[ 2], S13, 16_242070db); (* 3 *)
    FF(b, c, d, a, x[ 3], S14, 16_c1bdceee); (* 4 *)
    FF(a, b, c, d, x[ 4], S11, 16_f57c0faf); (* 5 *)
    FF(d, a, b, c, x[ 5], S12, 16_4787c62a); (* 6 *)
    FF(c, d, a, b, x[ 6], S13, 16_a8304613); (* 7 *)
    FF(b, c, d, a, x[ 7], S14, 16_fd469501); (* 8 *)
    FF(a, b, c, d, x[ 8], S11, 16_698098d8); (* 9 *)
    FF(d, a, b, c, x[ 9], S12, 16_8b44f7af); (* 10 *)
    FF(c, d, a, b, x[10], S13, 16_ffff5bb1); (* 11 *)
    FF(b, c, d, a, x[11], S14, 16_895cd7be); (* 12 *)
    FF(a, b, c, d, x[12], S11, 16_6b901122); (* 13 *)
    FF(d, a, b, c, x[13], S12, 16_fd987193); (* 14 *)
    FF(c, d, a, b, x[14], S13, 16_a679438e); (* 15 *)
    FF(b, c, d, a, x[15], S14, 16_49b40821); (* 16 *)

    (* Round 2 *)
    GG(a, b, c, d, x[ 1], S21, 16_f61e2562); (* 17 *)
    GG(d, a, b, c, x[ 6], S22, 16_c040b340); (* 18 *)
    GG(c, d, a, b, x[11], S23, 16_265e5a51); (* 19 *)
    GG(b, c, d, a, x[ 0], S24, 16_e9b6c7aa); (* 20 *)
    GG(a, b, c, d, x[ 5], S21, 16_d62f105d); (* 21 *)
    GG(d, a, b, c, x[10], S22, 16_02441453); (* 22 *)
    GG(c, d, a, b, x[15], S23, 16_d8a1e681); (* 23 *)
    GG(b, c, d, a, x[ 4], S24, 16_e7d3fbc8); (* 24 *)
    GG(a, b, c, d, x[ 9], S21, 16_21e1cde6); (* 25 *)
    GG(d, a, b, c, x[14], S22, 16_c33707d6); (* 26 *)
    GG(c, d, a, b, x[ 3], S23, 16_f4d50d87); (* 27 *)
    GG(b, c, d, a, x[ 8], S24, 16_455a14ed); (* 28 *)
    GG(a, b, c, d, x[13], S21, 16_a9e3e905); (* 29 *)
    GG(d, a, b, c, x[ 2], S22, 16_fcefa3f8); (* 30 *)
    GG(c, d, a, b, x[ 7], S23, 16_676f02d9); (* 31 *)
    GG(b, c, d, a, x[12], S24, 16_8d2a4c8a); (* 32 *)

    (* Round 3 *)
    HH(a, b, c, d, x[ 5], S31, 16_fffa3942); (* 33 *)
    HH(d, a, b, c, x[ 8], S32, 16_8771f681); (* 34 *)
    HH(c, d, a, b, x[11], S33, 16_6d9d6122); (* 35 *)
    HH(b, c, d, a, x[14], S34, 16_fde5380c); (* 36 *)
    HH(a, b, c, d, x[ 1], S31, 16_a4beea44); (* 37 *)
    HH(d, a, b, c, x[ 4], S32, 16_4bdecfa9); (* 38 *)
    HH(c, d, a, b, x[ 7], S33, 16_f6bb4b60); (* 39 *)
    HH(b, c, d, a, x[10], S34, 16_bebfbc70); (* 40 *)
    HH(a, b, c, d, x[13], S31, 16_289b7ec6); (* 41 *)
    HH(d, a, b, c, x[ 0], S32, 16_eaa127fa); (* 42 *)
    HH(c, d, a, b, x[ 3], S33, 16_d4ef3085); (* 43 *)
    HH(b, c, d, a, x[ 6], S34, 16_04881d05); (* 44 *)
    HH(a, b, c, d, x[ 9], S31, 16_d9d4d039); (* 45 *)
    HH(d, a, b, c, x[12], S32, 16_e6db99e5); (* 46 *)
    HH(c, d, a, b, x[15], S33, 16_1fa27cf8); (* 47 *)
    HH(b, c, d, a, x[ 2], S34, 16_c4ac5665); (* 48 *)

    (* Round 4 *)
    II(a, b, c, d, x[ 0], S41, 16_f4292244); (* 49 *)
    II(d, a, b, c, x[ 7], S42, 16_432aff97); (* 50 *)
    II(c, d, a, b, x[14], S43, 16_ab9423a7); (* 51 *)
    II(b, c, d, a, x[ 5], S44, 16_fc93a039); (* 52 *)
    II(a, b, c, d, x[12], S41, 16_655b59c3); (* 53 *)
    II(d, a, b, c, x[ 3], S42, 16_8f0ccc92); (* 54 *)
    II(c, d, a, b, x[10], S43, 16_ffeff47d); (* 55 *)
    II(b, c, d, a, x[ 1], S44, 16_85845dd1); (* 56 *)
    II(a, b, c, d, x[ 8], S41, 16_6fa87e4f); (* 57 *)
    II(d, a, b, c, x[15], S42, 16_fe2ce6e0); (* 58 *)
    II(c, d, a, b, x[ 6], S43, 16_a3014314); (* 59 *)
    II(b, c, d, a, x[13], S44, 16_4e0811a1); (* 60 *)
    II(a, b, c, d, x[ 4], S41, 16_f7537e82); (* 61 *)
    II(d, a, b, c, x[11], S42, 16_bd3af235); (* 62 *)
    II(c, d, a, b, x[ 2], S43, 16_2ad7d2bb); (* 63 *)
    II(b, c, d, a, x[ 9], S44, 16_eb86d391); (* 64 *)

    state[0] := Word.Plus(state[0], a);
    state[1] := Word.Plus(state[1], b);
    state[2] := Word.Plus(state[2], c);
    state[3] := Word.Plus(state[3], d);
  END Transform;

PROCEDURE Update(VAR md5ctx: T; input: TEXT) =
  VAR index, i, j, partLen: Word.T;
      locbuff: Buffer;

  BEGIN
    index := Word.And(Word.Shift(md5ctx.count[0], -3), 16_3F);
    md5ctx.count[0] :=
        Word.Plus(md5ctx.count[0], Word.Shift(Text.Length(input), 3));

    IF md5ctx.count[0] < Text.Length(input) THEN
      INC(md5ctx.count[1]);
    END;
    md5ctx.count[1] := md5ctx.count[1] + Word.Shift(Text.Length(input), -29);
    partLen := 64 - index;
    IF Text.Length(input) >= partLen THEN
      FOR i := index TO 63 DO
        md5ctx.buffer[i] := Text.GetChar(input, i-index);
      END;
      Transform(md5ctx.state, md5ctx.buffer);
      i := partLen;
      WHILE (i + 63) < Text.Length(input) DO
        FOR j := 0 TO 63 DO
          locbuff[j] := Text.GetChar(input, i+j);
        END;
        Transform(md5ctx.state, locbuff);
        INC(i, 64);
      END;
      index := 0;
    ELSE
      i := 0;
    END;

    j := 0;
    WHILE i+j < Text.Length(input) DO
      md5ctx.buffer[j+index] := Text.GetChar(input, i+j);
      INC(j);
    END;
  END Update;

PROCEDURE Final(VAR md5ctx: T): Digest=
  VAR bits: ARRAY [0..7] OF CHAR;
      index, padLen: INTEGER;
      digest: Digest;

  PROCEDURE Encode(VAR output: ARRAY OF CHAR;
                   VAR input: ARRAY OF Word.T;
                   count: INTEGER) =
    BEGIN
      FOR i := 0 TO count DO
        output[i*4+0] := VAL(Word.Extract(input[i],  0, 8), CHAR);
        output[i*4+1] := VAL(Word.Extract(input[i],  8, 8), CHAR);
        output[i*4+2] := VAL(Word.Extract(input[i], 16, 8), CHAR);
        output[i*4+3] := VAL(Word.Extract(input[i], 24, 8), CHAR)
      END;
    END Encode;
  BEGIN
    Encode(bits, md5ctx.count, 1);
    index := Word.And(Word.Shift(md5ctx.count[0], -3), 16_3F);
    IF index < 56 THEN
      padLen := 56 - index;
    ELSE
      padLen := 120 - index;
    END;
    Update(md5ctx, Text.Sub(padding, 0, padLen));
    Update(md5ctx, Text.FromChars(bits));
    Encode(digest, md5ctx.state, 3);
    RETURN digest;
  END Final;

PROCEDURE ToText(hash: Digest): TEXT =
  VAR buf: TEXT := "";
  BEGIN
    FOR i := 0 TO 15 DO
      buf := buf & Fmt.Pad(Fmt.Int(ORD(hash[i]), 16), 2, '0');
    END;
    RETURN buf;
  END ToText;

BEGIN
END MD5.
```

Example usage:

```modula3
MODULE Main;

IMPORT MD5, IO;

VAR md5ctx: MD5.T;

BEGIN
  MD5.Init(md5ctx);
  MD5.Update(md5ctx, "The quick brown fox jumped over the lazy dog's back");
  IO.Put(MD5.ToText(MD5.Final(md5ctx)) & "\n");
END Main.
```

Output:

```txt

e38ca1d920c4b8b8d3946b2c72f01680

```



## Nim


```nim
import sequtils

const
  ChunkSize = 512 div 8
  SumSize = 128 div 8

proc extractChunk(msg : seq[uint8], chunk: var openarray[uint32], offset: int) =
  var
    srcIndex = offset

  for dstIndex in 0 .. < 16:
    chunk[dstIndex] = 0
    for ii in 0 .. < 4:
      chunk[dstIndex] = chunk[dstIndex] shr 8
      chunk[dstIndex] = chunk[dstIndex] or (msg[srcIndex].uint32 shl 24)
      srcIndex.inc

proc leftRotate(val: uint32, shift: int) : uint32 =
  result = (val shl shift) or (val shr (32 - shift))

proc md5Sum(msg : seq[uint8]) : array[SumSize, uint8] =
  const
    s : array[ChunkSize, int] =
          [ 7, 12, 17, 22,  7, 12, 17, 22,
            7, 12, 17, 22,  7, 12, 17, 22,
            5,  9, 14, 20,  5,  9, 14, 20,
            5,  9, 14, 20,  5,  9, 14, 20,
            4, 11, 16, 23,  4, 11, 16, 23,
            4, 11, 16, 23,  4, 11, 16, 23,
            6, 10, 15, 21,  6, 10, 15, 21,
            6, 10, 15, 21,  6, 10, 15, 21 ]

    K : array[ChunkSize, uint32] =
          [ 0xd76aa478'u32, 0xe8c7b756'u32, 0x242070db'u32, 0xc1bdceee'u32,
            0xf57c0faf'u32, 0x4787c62a'u32, 0xa8304613'u32, 0xfd469501'u32,
            0x698098d8'u32, 0x8b44f7af'u32, 0xffff5bb1'u32, 0x895cd7be'u32,
            0x6b901122'u32, 0xfd987193'u32, 0xa679438e'u32, 0x49b40821'u32,
            0xf61e2562'u32, 0xc040b340'u32, 0x265e5a51'u32, 0xe9b6c7aa'u32,
            0xd62f105d'u32, 0x02441453'u32, 0xd8a1e681'u32, 0xe7d3fbc8'u32,
            0x21e1cde6'u32, 0xc33707d6'u32, 0xf4d50d87'u32, 0x455a14ed'u32,
            0xa9e3e905'u32, 0xfcefa3f8'u32, 0x676f02d9'u32, 0x8d2a4c8a'u32,
            0xfffa3942'u32, 0x8771f681'u32, 0x6d9d6122'u32, 0xfde5380c'u32,
            0xa4beea44'u32, 0x4bdecfa9'u32, 0xf6bb4b60'u32, 0xbebfbc70'u32,
            0x289b7ec6'u32, 0xeaa127fa'u32, 0xd4ef3085'u32, 0x04881d05'u32,
            0xd9d4d039'u32, 0xe6db99e5'u32, 0x1fa27cf8'u32, 0xc4ac5665'u32,
            0xf4292244'u32, 0x432aff97'u32, 0xab9423a7'u32, 0xfc93a039'u32,
            0x655b59c3'u32, 0x8f0ccc92'u32, 0xffeff47d'u32, 0x85845dd1'u32,
            0x6fa87e4f'u32, 0xfe2ce6e0'u32, 0xa3014314'u32, 0x4e0811a1'u32,
            0xf7537e82'u32, 0xbd3af235'u32, 0x2ad7d2bb'u32, 0xeb86d391'u32 ]


  # Pad with 1-bit, and fill with 0's up to 448 bits mod 512
  var paddedMsgSize = msg.len + 1
  var remain = (msg.len + 1) mod ChunkSize
  if remain > (448 div 8):
    paddedMsgSize += ChunkSize - remain + (448 div 8)
  else:
    paddedMsgSize += (448 div 8) - remain

  var paddingSize = paddedMsgSize - msg.len
  var padding = newSeq[uint8](paddingSize)
  padding[0] = 0x80

  # Pad with number of *bits* in original message, little-endian
  var sizePadding = newSeq[uint8](8)
  var size = msg.len * 8
  for ii in 0 .. < 4:
    sizePadding[ii] = uint8(size and 0xff)
    size = size shr 8

  var paddedMsg = concat(msg, padding, sizePadding)

  var accum = [ 0x67452301'u32, 0xefcdab89'u32, 0x98badcfe'u32, 0x10325476'u32 ]

  for offset in countup(0, paddedMsg.len - 1, ChunkSize):
    var A = accum[0]
    var B = accum[1]
    var C = accum[2]
    var D = accum[3]
    var F : uint32
    var g : int
    var M : array[16, uint32]
    var dTemp : uint32

    extractChunk(paddedMsg, M, offset)

    # This is pretty much the same as Wikipedia's MD5 entry
    for ii in 0 .. 63:
      if ii <= 15:
        F = (B and C) or ((not B) and D)
        g = ii

      elif ii <= 31:
        F = (D and B) or ((not D) and C)
        g = (5 * ii + 1) mod 16

      elif ii <= 47:
        F = B xor C xor D
        g = (3 * ii + 5) mod 16

      else:
        F = C xor (B or (not D))
        g = (7 * ii) mod 16

      dTemp = D
      D = C
      C = B
      B = B + leftRotate((A + F + K[ii] + M[g]), s[ii])
      A = dTemp

    accum[0] += A
    accum[1] += B
    accum[2] += C
    accum[3] += D

  # Convert four 32-bit accumulators to 16 byte array, little-endian
  var dstIdx : int
  for acc in accum:
    var tmp = acc

    for ii in 0 .. < 4:
      result[dstIdx] = uint8(tmp and 0xff)
      tmp = tmp shr 8
      dstIdx.inc

# Only needed to convert from string to uint8 sequence
iterator items * (str : string) : uint8 =
  for ii in 0 .. < len(str):
    yield str[ii].uint8

proc main =
  var msg = ""
  var sum = md5Sum(toSeq(msg.items()))
  assert(sum == [ 0xD4'u8, 0x1D, 0x8C, 0xD9, 0x8F, 0x00, 0xB2, 0x04,
                  0xE9, 0x80, 0x09, 0x98, 0xEC, 0xF8, 0x42, 0x7E ] )

  msg = "The quick brown fox jumps over the lazy dog"
  sum = md5Sum(toSeq(msg.items()))
  assert(sum == [ 0x9E'u8, 0x10, 0x7D, 0x9D, 0x37, 0x2B, 0xB6, 0x82,
                  0x6B, 0xD8, 0x1D, 0x35, 0x42, 0xA4, 0x19, 0xD6 ] )

  msg = "The quick brown fox jumps over the lazy dog."
  sum = md5Sum(toSeq(msg.items()))
  assert(sum == [ 0xE4'u8, 0xD9, 0x09, 0xC2, 0x90, 0xD0, 0xFB, 0x1C,
                 0xA0, 0x68, 0xFF, 0xAD, 0xDF, 0x22, 0xCB, 0xD0 ])


  # Message size around magic 512 bits
  msg = "01234567890123456789012345678901234567890123456789012345678901234"
  sum = md5Sum(toSeq(msg.items()))
  assert(sum == [ 0xBE'u8, 0xB9, 0xF4, 0x8B, 0xC8, 0x02, 0xCA, 0x5C,
                  0xA0, 0x43, 0xBC, 0xC1, 0x5E, 0x21, 0x9A, 0x5A ])

  msg = "0123456789012345678901234567890123456789012345678901234567890123"
  sum = md5Sum(toSeq(msg.items()))
  assert(sum == [ 0x7F'u8, 0x7B, 0xFD, 0x34, 0x87, 0x09, 0xDE, 0xEA,
                  0xAC, 0xE1, 0x9E, 0x3F, 0x53, 0x5F, 0x8C, 0x54 ])

  msg = "012345678901234567890123456789012345678901234567890123456789012"
  sum = md5Sum(toSeq(msg.items()))
  assert(sum == [ 0xC5'u8, 0xE2, 0x56, 0x43, 0x7E, 0x75, 0x80, 0x92,
                  0xDB, 0xFE, 0x06, 0x28, 0x3E, 0x48, 0x90, 0x19 ])


  # Message size around magic 448 bits
  msg = "01234567890123456789012345678901234567890123456789012345"
  sum = md5Sum(toSeq(msg.items()))
  assert(sum == [ 0x8A'u8, 0xF2, 0x70, 0xB2, 0x84, 0x76, 0x10, 0xE7,
                  0x42, 0xB0, 0x79, 0x1B, 0x53, 0x64, 0x8C, 0x09 ])

  msg = "0123456789012345678901234567890123456789012345678901234"
  sum = md5Sum(toSeq(msg.items()))
  assert(sum == [ 0x6E'u8, 0x7A, 0x4F, 0xC9, 0x2E, 0xB1, 0xC3, 0xF6,
                  0xE6, 0x52, 0x42, 0x5B, 0xCC, 0x8D, 0x44, 0xB5 ])

  msg = "012345678901234567890123456789012345678901234567890123"
  sum = md5Sum(toSeq(msg.items()))
  assert(sum == [ 0x3D'u8, 0xFF, 0x83, 0xC8, 0xFA, 0xDD, 0x26, 0x37,
                  0x0D, 0x5B, 0x09, 0x84, 0x09, 0x64, 0x44, 0x57 ])

main()
```



## ooRexx

```ooRexx

#!/usr/bin/rexx

/* Expected results:
   0xd41d8cd98f00b204e9800998ecf8427e <== ""
   0x0cc175b9c0f1b6a831c399e269772661 <== "a"
   0x900150983cd24fb0d6963f7d28e17f72 <== "abc"
   0xf96b697d7cb7938d525a2f31aaf161d0 <== "message digest"
   0xc3fcd3d76192e4007dfb496cca67e13b <== "abcdefghijklmnopqrstuvwxyz"
   0xd174ab98d277d9f5a5611c2c9f419d9f <== "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
   0x57edf4a22be3c955ac49da2e2107b67a <== "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
*/

md5 = .md5~new; md5~update(""); say md5~digest
md5 = .md5~new; md5~update("a"); say md5~digest
md5 = .md5~new; md5~update("abc"); say md5~digest
md5 = .md5~new; md5~update("message digest"); say md5~digest
md5 = .md5~new("abcdefghijklmnopqrstuvwxyz"); say md5~digest
md5 = .md5~new("ABCDEFGHIJKLMNOPQRSTUVWXYZ"); md5~update("abcdefghijklmnopqrstuvwxyz0123456789"); say md5~digest
md5 = .md5~new; md5~update("12345678901234567890123456789012345678901234567890123456789012345678901234567890"); say md5~digest

-- requires OORexx 4.2.0 or later
-- standard numeric digits of 9 is not enough in this case
::options digits 20

-- Implementation mainly based on pseudocode in https://en.wikipedia.org/wiki/MD5
::class md5 public

::method init
expose a0 b0 c0 d0 count buffer index K. s  -- instance variables
use strict arg chunk=""
-- Initialize message digest
a0 = .int32~new('67452301'x,"C")   -- A
b0 = .int32~new('efcdab89'x,"C")   -- B
c0 = .int32~new('98badcfe'x,"C")   -- C
d0 = .int32~new('10325476'x,"C")   -- D
-- The 512 bit chunk buffer
buffer = .mutablebuffer~new('00'x~copies(64),64)
-- The position in the buffer to insert new input
index = 1
-- message bytecount
count = 0
-- initialize leftrotate amounts
nrs = .array~of(7,12,17,22)
s = nrs~union(nrs)~union(nrs)~union(nrs)
nrs = .array~of(5,9,14,20)
s = s~union(nrs)~union(nrs)~union(nrs)~union(nrs)
nrs = .array~of(4,11,16,23)
s = s~union(nrs)~union(nrs)~union(nrs)~union(nrs)
nrs = .array~of(6,10,15,21)
s = s~union(nrs)~union(nrs)~union(nrs)~union(nrs)
-- initialize sinus derived constants.
-- sin function from RXMath Library shipped with OORexx
-- see ::routine directive at the end of the code
do i=0 to 63
  K.i = .int32~new(((2**32)*(sin(i+1,16,R)~abs))~floor)
end
-- process initial string if any
self~update(chunk)
exit

::method update
  expose a0 b0 c0 d0 count buffer index K. s  -- instance variables
  use strict arg chunk
  count += chunk~length
  if chunk~length<65-index then do
    buffer~overlay(chunk,index)
    index += chunk~length
  end
  else do
    split = 65-index+1
    parse var chunk part =(split) chunk
    buffer~overlay(part,index)
    index = 65
  end
  -- Only proces completely filled buffer
  do while index=65
    A = a0
    B = b0
    C = c0
    D = d0
    do i=0 to 63
      select
        when i<16 then do
          F = D~xor(B~and(C~xor(D)))
          g = i
        end
        when i<32 then do
          F = C~xor(D~and(B~xor(C)))
          g = (5*i+1)//16
        end
        when i<48 then do
          F = B~xor(C)~xor(D)
          g = (3*i+5)//16
        end
        otherwise do
          F = C~xor(B~or(D~xor(.int32~new('ffffffff'x,"C"))))
          g = (7*i)//16
        end
      end
      M = .int32~new(buffer~substr(g*4+1,4)~reverse,"C")  -- 32bit word in little-endian
      dTemp = D
      D = C
      C = B
      B = (B + (A+F+K.i+M)~bitrotate(s[i+1]))
      A = dTemp
    end
    a0 = a0+A
    b0 = b0+B
    c0 = c0+C
    d0 = d0+D
    parse var chunk part 65 chunk
    index = part~length+1
    buffer~overlay(part,1,part~length)
  end
exit

::method digest
  expose a0 b0 c0 d0 count buffer index K s -- instance variables
  padlen = 64
  if index<57 then padlen = 57-index
  if index>57 then padlen = 121-index
  padding = '00'x~copies(padlen)~bitor('80'x)
  bitcount = count*8//2**64
  lowword = bitcount//2**32
  hiword = bitcount%2**32
  lowcount = lowword~d2c(4)~reverse -- make it little-endian
  hicount = hiword~d2c(4)~reverse   -- make it little-endian
  self~update(padding || lowcount || hicount)
return a0~string || b0~string || c0~string || d0~string

-- A convenience class to encapsulate operations on non OORexx-like
-- things as little-endian 32-bit words
::class int32 public

::attribute arch class

::method init class
  self~arch = "little-endian"   -- can be adapted for multiple architectures

-- Method to create an int32 like object
-- Input can be a OORexx whole number (type="I") or
-- a character string of 4 bytes (type="C")
-- input truncated or padded to 32-bit word/string
::method init
  expose char4 int32
  use strict arg input, type="Integer"
  -- type must be one of "I"nteger or "C"haracter
  t = type~subchar(1)~upper
  select
    when t=='I' then do
      char4 = input~d2c(4)
      int32 = char4~c2d
    end
    when t=='C' then do
      char4 = input~right(4,'00'x)
      int32 = char4~c2d
    end
    otherwise do
      raise syntax 93.915 array("IC",type)
    end
  end
exit

::method xor  -- wrapper for OORexx bitxor method
  expose char4
  use strict arg other
return .int32~new(char4~bitxor(other~char),"C")

::method and  -- wrapper for OORexx bitand method
  expose char4
  use strict arg other
return .int32~new(char4~bitand(other~char),"C")

::method or   -- wrapper for OORexx bitor method
  expose char4
  use strict arg other
return .int32~new(char4~bitor(other~char),"C")

::method bitleft -- OORexx shift (<<) implementation
  expose char4
  use strict arg bits
  bstring = char4~c2x~x2b
  bstring = bstring~substr(bits+1)~left(bstring~length,'0')
return .int32~new(bstring~b2x~x2d)

::method bitright -- OORexx shift (>>) implementation
  expose char4
  use strict arg bits, signed=.false
  bstring = char4~c2x~x2b
  fill = '0'
  if signed then fill = bstring~subchar(1)
  bstring = bstring~left(bstring~length-bits)~right(bstring~length,fill)
return .int32~new(bstring~b2x~x2d)

::method bitnot   -- OORexx not implementation
  expose char4
return .int32~new(char4~bitxor('ffffffff'x)~c2d,"C")

::method bitrotate  -- OORexx (left) rotate method
  expose char4
  use strict arg bits, direction='left'
  d = direction~subchar(1)~upper
  if d=='L' then do
    leftpart = self~bitleft(bits)
    rightpart = self~bitright(32-bits)
  end
  else do
    leftpart = self~bitleft(32-bits)
    rightpart = self~bitright(bits)
  end
return rightpart~or(leftpart)

::method int  -- retrieve integer as number
  expose int32
return int32

::method char -- retrieve integer as characters
  expose char4
return char4

::method '+'  -- OORexx method to add 2 .int32 instances
  expose int32
  use strict arg other
return .int32~new(int32+other~int)

::method string -- retrieve integer as hexadecimal string
  expose char4
return char4~reverse~c2x~lower

-- Simplify function names for the necessary 'RxMath' functions
::routine sin EXTERNAL "LIBRARY rxmath RxCalcSin"

```



## Perl

```perl
use strict;
use warnings;
use integer;
use Test::More;

BEGIN { plan tests => 7 }

sub A()   { 0x67_45_23_01 }
sub B()   { 0xef_cd_ab_89 }
sub C()   { 0x98_ba_dc_fe }
sub D()   { 0x10_32_54_76 }
sub MAX() { 0xFFFFFFFF }

sub padding {
    my $l = length (my $msg = shift() . chr(128));
    $msg .= "\0" x (($l%64<=56?56:120)-$l%64);
    $l = ($l-1)*8;
    $msg .= pack 'VV', $l & MAX , ($l >> 16 >> 16);
}

sub rotate_left($$) {
    ($_[0] << $_[1]) | (( $_[0] >> (32 - $_[1])  )  & ((1 << $_[1]) - 1));
}

sub gen_code {
  # Discard upper 32 bits on 64 bit archs.
  my $MSK = ((1 << 16) << 16) ? ' & ' . MAX : '';
  my %f = (
    FF => "X0=rotate_left((X3^(X1&(X2^X3)))+X0+X4+X6$MSK,X5)+X1$MSK;",
    GG => "X0=rotate_left((X2^(X3&(X1^X2)))+X0+X4+X6$MSK,X5)+X1$MSK;",
    HH => "X0=rotate_left((X1^X2^X3)+X0+X4+X6$MSK,X5)+X1$MSK;",
    II => "X0=rotate_left((X2^(X1|(~X3)))+X0+X4+X6$MSK,X5)+X1$MSK;",
  );

  my %s = (  # shift lengths
    S11 => 7, S12 => 12, S13 => 17, S14 => 22, S21 => 5, S22 => 9, S23 => 14,
    S24 => 20, S31 => 4, S32 => 11, S33 => 16, S34 => 23, S41 => 6, S42 => 10,
    S43 => 15, S44 => 21
  );

  my $insert = "\n";
  while(defined( my $data = <DATA> )) {
    chomp $data;
    next unless $data =~ /^[FGHI]/;
    my ($func,@x) = split /,/, $data;
    my $c = $f{$func};
    $c =~ s/X(\d)/$x[$1]/g;
    $c =~ s/(S\d{2})/$s{$1}/;
    $c =~ s/^(.*)=rotate_left\((.*),(.*)\)\+(.*)$//;

    my $su = 32 - $3;
    my $sh = (1 << $3) - 1;

    $c = "$1=(((\$r=$2)<<$3)|((\$r>>$su)&$sh))+$4";

    $insert .= "\t$c\n";
  }
  close DATA;

  my $dump = '
  sub round {
    my ($a,$b,$c,$d) = @_[0 .. 3];
    my $r;' . $insert . '
    $_[0]+$a' . $MSK . ', $_[1]+$b ' . $MSK .
    ', $_[2]+$c' . $MSK . ', $_[3]+$d' . $MSK . ';
  }';
  eval $dump;
}

gen_code();

sub _encode_hex { unpack 'H*', $_[0] }

sub md5 {
    my $message = padding(join'',@_);
    my ($a,$b,$c,$d) = (A,B,C,D);
    my $i;
    for $i (0 .. (length $message)/64-1) {
        my @X = unpack 'V16', substr $message,$i*64,64;
        ($a,$b,$c,$d) = round($a,$b,$c,$d,@X);
    }
    pack 'V4',$a,$b,$c,$d;
}

my $strings = {
    'd41d8cd98f00b204e9800998ecf8427e' => '',
    '0cc175b9c0f1b6a831c399e269772661' => 'a',
    '900150983cd24fb0d6963f7d28e17f72' => 'abc',
    'f96b697d7cb7938d525a2f31aaf161d0' => 'message digest',
    'c3fcd3d76192e4007dfb496cca67e13b' => 'abcdefghijklmnopqrstuvwxyz',
    'd174ab98d277d9f5a5611c2c9f419d9f' => 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789',
    '57edf4a22be3c955ac49da2e2107b67a' => '12345678901234567890123456789012345678901234567890123456789012345678901234567890',
};

for my $k (keys %$strings) {
    my $digest = _encode_hex md5($strings->{$k});
    is($digest, $k, "$digest is MD5 digest $strings->{$k}");
}

__DATA__
FF,$a,$b,$c,$d,$_[4],7,0xd76aa478,/* 1 */
FF,$d,$a,$b,$c,$_[5],12,0xe8c7b756,/* 2 */
FF,$c,$d,$a,$b,$_[6],17,0x242070db,/* 3 */
FF,$b,$c,$d,$a,$_[7],22,0xc1bdceee,/* 4 */
FF,$a,$b,$c,$d,$_[8],7,0xf57c0faf,/* 5 */
FF,$d,$a,$b,$c,$_[9],12,0x4787c62a,/* 6 */
FF,$c,$d,$a,$b,$_[10],17,0xa8304613,/* 7 */
FF,$b,$c,$d,$a,$_[11],22,0xfd469501,/* 8 */
FF,$a,$b,$c,$d,$_[12],7,0x698098d8,/* 9 */
FF,$d,$a,$b,$c,$_[13],12,0x8b44f7af,/* 10 */
FF,$c,$d,$a,$b,$_[14],17,0xffff5bb1,/* 11 */
FF,$b,$c,$d,$a,$_[15],22,0x895cd7be,/* 12 */
FF,$a,$b,$c,$d,$_[16],7,0x6b901122,/* 13 */
FF,$d,$a,$b,$c,$_[17],12,0xfd987193,/* 14 */
FF,$c,$d,$a,$b,$_[18],17,0xa679438e,/* 15 */
FF,$b,$c,$d,$a,$_[19],22,0x49b40821,/* 16 */
GG,$a,$b,$c,$d,$_[5],5,0xf61e2562,/* 17 */
GG,$d,$a,$b,$c,$_[10],9,0xc040b340,/* 18 */
GG,$c,$d,$a,$b,$_[15],14,0x265e5a51,/* 19 */
GG,$b,$c,$d,$a,$_[4],20,0xe9b6c7aa,/* 20 */
GG,$a,$b,$c,$d,$_[9],5,0xd62f105d,/* 21 */
GG,$d,$a,$b,$c,$_[14],9,0x2441453,/* 22 */
GG,$c,$d,$a,$b,$_[19],14,0xd8a1e681,/* 23 */
GG,$b,$c,$d,$a,$_[8],20,0xe7d3fbc8,/* 24 */
GG,$a,$b,$c,$d,$_[13],5,0x21e1cde6,/* 25 */
GG,$d,$a,$b,$c,$_[18],9,0xc33707d6,/* 26 */
GG,$c,$d,$a,$b,$_[7],14,0xf4d50d87,/* 27 */
GG,$b,$c,$d,$a,$_[12],20,0x455a14ed,/* 28 */
GG,$a,$b,$c,$d,$_[17],5,0xa9e3e905,/* 29 */
GG,$d,$a,$b,$c,$_[6],9,0xfcefa3f8,/* 30 */
GG,$c,$d,$a,$b,$_[11],14,0x676f02d9,/* 31 */
GG,$b,$c,$d,$a,$_[16],20,0x8d2a4c8a,/* 32 */
HH,$a,$b,$c,$d,$_[9],4,0xfffa3942,/* 33 */
HH,$d,$a,$b,$c,$_[12],11,0x8771f681,/* 34 */
HH,$c,$d,$a,$b,$_[15],16,0x6d9d6122,/* 35 */
HH,$b,$c,$d,$a,$_[18],23,0xfde5380c,/* 36 */
HH,$a,$b,$c,$d,$_[5],4,0xa4beea44,/* 37 */
HH,$d,$a,$b,$c,$_[8],11,0x4bdecfa9,/* 38 */
HH,$c,$d,$a,$b,$_[11],16,0xf6bb4b60,/* 39 */
HH,$b,$c,$d,$a,$_[14],23,0xbebfbc70,/* 40 */
HH,$a,$b,$c,$d,$_[17],4,0x289b7ec6,/* 41 */
HH,$d,$a,$b,$c,$_[4],11,0xeaa127fa,/* 42 */
HH,$c,$d,$a,$b,$_[7],16,0xd4ef3085,/* 43 */
HH,$b,$c,$d,$a,$_[10],23,0x4881d05,/* 44 */
HH,$a,$b,$c,$d,$_[13],4,0xd9d4d039,/* 45 */
HH,$d,$a,$b,$c,$_[16],11,0xe6db99e5,/* 46 */
HH,$c,$d,$a,$b,$_[19],16,0x1fa27cf8,/* 47 */
HH,$b,$c,$d,$a,$_[6],23,0xc4ac5665,/* 48 */
II,$a,$b,$c,$d,$_[4],6,0xf4292244,/* 49 */
II,$d,$a,$b,$c,$_[11],10,0x432aff97,/* 50 */
II,$c,$d,$a,$b,$_[18],15,0xab9423a7,/* 51 */
II,$b,$c,$d,$a,$_[9],21,0xfc93a039,/* 52 */
II,$a,$b,$c,$d,$_[16],6,0x655b59c3,/* 53 */
II,$d,$a,$b,$c,$_[7],10,0x8f0ccc92,/* 54 */
II,$c,$d,$a,$b,$_[14],15,0xffeff47d,/* 55 */
II,$b,$c,$d,$a,$_[5],21,0x85845dd1,/* 56 */
II,$a,$b,$c,$d,$_[12],6,0x6fa87e4f,/* 57 */
II,$d,$a,$b,$c,$_[19],10,0xfe2ce6e0,/* 58 */
II,$c,$d,$a,$b,$_[10],15,0xa3014314,/* 59 */
II,$b,$c,$d,$a,$_[17],21,0x4e0811a1,/* 60 */
II,$a,$b,$c,$d,$_[8],6,0xf7537e82,/* 61 */
II,$d,$a,$b,$c,$_[15],10,0xbd3af235,/* 62 */
II,$c,$d,$a,$b,$_[6],15,0x2ad7d2bb,/* 63 */
II,$b,$c,$d,$a,$_[13],21,0xeb86d391,/* 64 */
```


```txt

1..7
ok 1 - c3fcd3d76192e4007dfb496cca67e13b is MD5 digest abcdefghijklmnopqrstuvwxyz
ok 2 - f96b697d7cb7938d525a2f31aaf161d0 is MD5 digest message digest
ok 3 - 900150983cd24fb0d6963f7d28e17f72 is MD5 digest abc
ok 4 - d41d8cd98f00b204e9800998ecf8427e is MD5 digest
ok 5 - 57edf4a22be3c955ac49da2e2107b67a is MD5 digest 12345678901234567890123456789012345678901234567890123456789012345678901234567890
ok 6 - 0cc175b9c0f1b6a831c399e269772661 is MD5 digest a
ok 7 - d174ab98d277d9f5a5611c2c9f419d9f is MD5 digest ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789

```



## Perl 6

```perl6
sub infix:<⊞>(uint32 $a, uint32 $b --> uint32) { ($a + $b) +& 0xffffffff }
sub infix:«<<<»(uint32 $a, UInt $n --> uint32) { ($a +< $n) +& 0xffffffff +| ($a +> (32-$n)) }

constant FGHI = { ($^a +& $^b) +| (+^$a +& $^c) },
                { ($^a +& $^c) +| ($^b +& +^$c) },
                { $^a +^ $^b +^ $^c             },
                { $^b +^ ($^a +| +^$^c)         };

constant _S = flat (7, 12, 17, 22) xx 4,
                  (5,  9, 14, 20) xx 4,
                  (4, 11, 16, 23) xx 4,
                  (6, 10, 15, 21) xx 4;

constant T = (floor(abs(sin($_ + 1)) * 2**32) for ^64);

constant k = flat (   $_           for ^16),
                  ((5*$_ + 1) % 16 for ^16),
                  ((3*$_ + 5) % 16 for ^16),
                  ((7*$_    ) % 16 for ^16);

sub little-endian($w, $n, *@v) {
    my \step1 = $w X* ^$n;
    my \step2 = @v X+> step1;
    step2 X% 2**$w;
}

sub md5-pad(Blob $msg)
{
    my $bits = 8 * $msg.elems;
    my @padded = flat $msg.list, 0x80, 0x00 xx -($bits div 8 + 1 + 8) % 64;
    flat @padded.map({ :256[$^d,$^c,$^b,$^a] }), little-endian(32, 2, $bits);
}

sub md5-block(@H, @X)
{
    my uint32 ($A, $B, $C, $D) = @H;
    ($A, $B, $C, $D) = ($D, $B ⊞ (($A ⊞ FGHI[$_ div 16]($B, $C, $D) ⊞ T[$_] ⊞ @X[k[$_]]) <<< _S[$_]), $B, $C) for ^64;
    @H «⊞=» ($A, $B, $C, $D);
}

sub md5(Blob $msg --> Blob)
{
    my uint32 @M = md5-pad($msg);
    my uint32 @H = 0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476;
    md5-block(@H, @M[$_ .. $_+15]) for 0, 16 ...^ +@M;
    Blob.new: little-endian(8, 4, @H);
}

use Test;
plan 7;

for 'd41d8cd98f00b204e9800998ecf8427e', '',
    '0cc175b9c0f1b6a831c399e269772661', 'a',
    '900150983cd24fb0d6963f7d28e17f72', 'abc',
    'f96b697d7cb7938d525a2f31aaf161d0', 'message digest',
    'c3fcd3d76192e4007dfb496cca67e13b', 'abcdefghijklmnopqrstuvwxyz',
    'd174ab98d277d9f5a5611c2c9f419d9f', 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789',
    '57edf4a22be3c955ac49da2e2107b67a', '12345678901234567890123456789012345678901234567890123456789012345678901234567890'
-> $expected, $msg {
    my $digest = md5($msg.encode('ascii')).list».fmt('%02x').join;
    is($digest, $expected, "$digest is MD5 digest of '$msg'");
}
```


```txt
1..7
ok 1 - d41d8cd98f00b204e9800998ecf8427e is MD5 digest of ''
ok 2 - 0cc175b9c0f1b6a831c399e269772661 is MD5 digest of 'a'
ok 3 - 900150983cd24fb0d6963f7d28e17f72 is MD5 digest of 'abc'
ok 4 - f96b697d7cb7938d525a2f31aaf161d0 is MD5 digest of 'message digest'
ok 5 - c3fcd3d76192e4007dfb496cca67e13b is MD5 digest of 'abcdefghijklmnopqrstuvwxyz'
ok 6 - d174ab98d277d9f5a5611c2c9f419d9f is MD5 digest of 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'
ok 7 - 57edf4a22be3c955ac49da2e2107b67a is MD5 digest of '12345678901234567890123456789012345678901234567890123456789012345678901234567890'
```



## Phix

Non-optimised. Originally written by Davi Tassinari de Figueiredo. Included in the distribution as demo\rosetta\md5.exw

```Phix
function uxor(atom data1,atom data2)
atom result = xor_bits(data1,data2)
    if result<0 then result += #100000000 end if
    return result
end function

function uor(atom data1,atom data2)
atom result = or_bits(data1,data2)
    if result<0 then result += #100000000 end if
    return result
end function

function r32(atom a)
    return remainder(a,#100000000)
end function

function rol(atom word,integer bits)
-- left rotate the bits of a 32-bit number by the specified number of bits
    return r32(word*power(2,bits))+floor(word/power(2,32-bits))
end function

constant K =
{#d76aa478, #e8c7b756, #242070db, #c1bdceee, #f57c0faf, #4787c62a, #a8304613, #fd469501,
 #698098d8, #8b44f7af, #ffff5bb1, #895cd7be, #6b901122, #fd987193, #a679438e, #49b40821,
 #f61e2562, #c040b340, #265e5a51, #e9b6c7aa, #d62f105d, #02441453, #d8a1e681, #e7d3fbc8,
 #21e1cde6, #c33707d6, #f4d50d87, #455a14ed, #a9e3e905, #fcefa3f8, #676f02d9, #8d2a4c8a,
 #fffa3942, #8771f681, #6d9d6122, #fde5380c, #a4beea44, #4bdecfa9, #f6bb4b60, #bebfbc70,
 #289b7ec6, #eaa127fa, #d4ef3085, #04881d05, #d9d4d039, #e6db99e5, #1fa27cf8, #c4ac5665,
 #f4292244, #432aff97, #ab9423a7, #fc93a039, #655b59c3, #8f0ccc92, #ffeff47d, #85845dd1,
 #6fa87e4f, #fe2ce6e0, #a3014314, #4e0811a1, #f7537e82, #bd3af235, #2ad7d2bb, #eb86d391}

constant m_block = {1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,
                    2, 7,12, 1, 6,11,16, 5,10,15, 4, 9,14, 3, 8,13,
                    6, 9,12,15, 2, 5, 8,11,14, 1, 4, 7,10,13,16, 3,
                    1, 8,15, 6,13, 4,11, 2, 9,16, 7,14, 5,12, 3,10}

constant c_words = {#67452301,#efcdab89,#98badcfe,#10325476}

sequence words

function divide_in_words(sequence message)
-- Divides the string into words (32-bit numbers)
sequence res
    res = repeat(0,length(message)/4)
    for word=1 to length(message)/4 do
        res[word] = bytes_to_int(message[word*4-3..word*4])
    end for
    return res
end function

procedure process_block(sequence block)
-- Updates the words according to the contents of the block
atom a,b,c,d

    block = divide_in_words(block)

    a = words[1]
    b = words[2]
    c = words[3]
    d = words[4]

    -- Round 1
    for step=1 to 16 by 4 do
        a = r32(b+rol(r32(a+block[m_block[step  ]]+K[step  ]+uor(and_bits(b,c),and_bits(not_bits(b),d))), 7))
        d = r32(a+rol(r32(d+block[m_block[step+1]]+K[step+1]+uor(and_bits(a,b),and_bits(not_bits(a),c))),12))
        c = r32(d+rol(r32(c+block[m_block[step+2]]+K[step+2]+uor(and_bits(d,a),and_bits(not_bits(d),b))),17))
        b = r32(c+rol(r32(b+block[m_block[step+3]]+K[step+3]+uor(and_bits(c,d),and_bits(not_bits(c),a))),22))
    end for

    -- Round 2
    for step=17 to 32 by 4 do
        a = r32(b+rol(r32(a+block[m_block[step  ]]+K[step  ]+uor(and_bits(b,d),and_bits(c,not_bits(d)))), 5))
        d = r32(a+rol(r32(d+block[m_block[step+1]]+K[step+1]+uor(and_bits(a,c),and_bits(b,not_bits(c)))), 9))
        c = r32(d+rol(r32(c+block[m_block[step+2]]+K[step+2]+uor(and_bits(d,b),and_bits(a,not_bits(b)))),14))
        b = r32(c+rol(r32(b+block[m_block[step+3]]+K[step+3]+uor(and_bits(c,a),and_bits(d,not_bits(a)))),20))
    end for

    -- Round 3
    for step=33 to 48 by 4 do
        a = r32(b+rol(r32(a+block[m_block[step  ]]+K[step  ]+uxor(b,xor_bits(c,d))), 4))
        d = r32(a+rol(r32(d+block[m_block[step+1]]+K[step+1]+uxor(a,xor_bits(b,c))),11))
        c = r32(d+rol(r32(c+block[m_block[step+2]]+K[step+2]+uxor(d,xor_bits(a,b))),16))
        b = r32(c+rol(r32(b+block[m_block[step+3]]+K[step+3]+uxor(c,xor_bits(d,a))),23))
    end for

    -- Round 4
    for step=49 to 64 by 4 do
        a = r32(b+rol(r32(a+block[m_block[step  ]]+K[step  ]+uxor(c,or_bits(b,not_bits(d)))), 6))
        d = r32(a+rol(r32(d+block[m_block[step+1]]+K[step+1]+uxor(b,or_bits(a,not_bits(c)))),10))
        c = r32(d+rol(r32(c+block[m_block[step+2]]+K[step+2]+uxor(a,or_bits(d,not_bits(b)))),15))
        b = r32(c+rol(r32(b+block[m_block[step+3]]+K[step+3]+uxor(d,or_bits(c,not_bits(a)))),21))
    end for

    -- Update the words
    words[1] = r32(words[1]+a)
    words[2] = r32(words[2]+b)
    words[3] = r32(words[3]+c)
    words[4] = r32(words[4]+d)
end procedure

function pad_message(sequence message)
-- Add bytes to the end of the message so it can be divided
-- in an exact number of 64-byte blocks.
integer bytes_to_add
    bytes_to_add = 64-remainder(length(message)+9,64)
    if bytes_to_add=64 then bytes_to_add = 0 end if

    message = message&#80&repeat(0,bytes_to_add)&
              int_to_bytes(length(message)*8)&{0,0,0,0}

    return message
end function


function md5(sequence message)
-- Given a string, returns a 16-byte hash of it.

    words = c_words -- Initialize the H words

    message = pad_message(message)  -- Add bytes to the message

    -- Process each 64-byte block
    for block=1 to length(message) by 64 do
        process_block(message[block..block+63])
    end for

    -- Convert hash into bytes
    return int_to_bytes(words[1])&    -- Return the hash
           int_to_bytes(words[2])&
           int_to_bytes(words[3])&
           int_to_bytes(words[4])

end function

constant fmt = "0x%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X\n"

printf(1,fmt,md5(""))
printf(1,fmt,md5("a"))
printf(1,fmt,md5("abc"))
printf(1,fmt,md5("message digest"))
printf(1,fmt,md5("abcdefghijklmnopqrstuvwxyz"))
printf(1,fmt,md5("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))
printf(1,fmt,md5("12345678901234567890123456789012345678901234567890123456789012345678901234567890"))
```

```txt

0xd41d8cd98f00b204e9800998ecf8427e
0x0cc175b9c0f1b6a831c399e269772661
0x900150983cd24fb0d6963f7d28e17f72
0xf96b697d7cb7938d525a2f31aaf161d0
0xc3fcd3d76192e4007dfb496cca67e13b
0xd174ab98d277d9f5a5611c2c9f419d9f
0x57edf4a22be3c955ac49da2e2107b67a

```




## PicoLisp

This is an implementation of the pseudo-code in the Wikipedia article. Special
care had to be taken with modulo 32-bit arithmetics, as PicoLisp supports only
numbers of unspecified size.

```PicoLisp
(scl 12)
(load "@lib/math.l")  # For 'sin'

(de *Md5-R
   7 12 17 22  7 12 17 22  7 12 17 22  7 12 17 22
   5  9 14 20  5  9 14 20  5  9 14 20  5  9 14 20
   4 11 16 23  4 11 16 23  4 11 16 23  4 11 16 23
   6 10 15 21  6 10 15 21  6 10 15 21  6 10 15 21 )

(de *Md5-K
   ~(make
      (for I 64
         (link
            (/ (* (abs (sin (* I 1.0))) `(** 2 32)) 1.0) ) ) ) )

(de mod32 (N)
   (& N `(hex "FFFFFFFF")) )

(de not32 (N)
   (x| N `(hex "FFFFFFFF")) )

(de add32 @
   (mod32 (pass +)) )

(de leftRotate (X C)
   (| (mod32 (>> (- C) X)) (>> (- 32 C) X)) )

(de md5 (Str)
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
         H3 `(hex "10325476") )
      (while Str
         (let
            (A H0  B H1  C H2  D H3
               W (make
                  (do 16
                     (link
                        (apply |
                           (mapcar >> (0 -8 -16 -24) (cut 4 'Str)) ) ) ) ) )
               (use (Tmp F G)
                  (for I 64
                     (cond
                        ((>= 16 I)
                           (setq
                              F (| (& B C) (& (not32 B) D))
                              G I ) )
                        ((>= 32 I)
                           (setq
                              F (| (& D B) (& (not32 D) C))
                              G (inc (& (inc (* 5 (dec I))) 15)) ) )
                        ((>= 48 I)
                           (setq
                              F (x| B C D)
                              G (inc (& (+ 5 (* 3 (dec I))) 15)) ) )
                        (T
                           (setq
                              F (x| C (| B (not32 D)))
                              G (inc (& (* 7 (dec I)) 15)) ) ) )
                     (setq
                        Tmp D
                        D C
                        C B
                        B
                        (add32 B
                           (leftRotate
                              (add32 A F (get *Md5-K I) (get W G))
                              (get *Md5-R I) ) )
                        A Tmp ) ) )
               (setq
                  H0 (add32 H0 A)
                  H1 (add32 H1 B)
                  H2 (add32 H2 C)
                  H3 (add32 H3 D) ) ) )
      (pack
         (make
            (for N (list H0 H1 H2 H3)
               (do 4  # Convert to little endian hex string
                  (link (pad 2 (hex (& N 255))))
                  (setq N (>> 8 N)) ) ) ) ) ) )
```

Output:

```txt
: (md5 "")
-> "D41D8CD98F00B204E9800998ECF8427E"
: (md5 "a")
-> "0CC175B9C0F1B6A831C399E269772661"
: (md5 "abc")
-> "900150983CD24FB0D6963F7D28E17F72"
: (md5 "message digest")
-> "F96B697D7CB7938D525A2F31AAF161D0"
: (md5 "abcdefghijklmnopqrstuvwxyz")
-> "C3FCD3D76192E4007DFB496CCA67E13B"
: (md5 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
-> "D174AB98D277D9F5A5611C2C9F419D9F"
: (md5 "12345678901234567890123456789012345678901234567890123456789012345678901234567890")
-> "57EDF4A22BE3C955AC49DA2E2107B67A"
```



## Python


Note that the following code focuses on brevity and elegance instead of performance, since Python isn't very good at number crunching anyway. If performance is important, the best solution is to use the built-in '''md5''' module, written in C.


```python
import math

rotate_amounts = [7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
                  5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20,
                  4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
                  6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21]

constants = [int(abs(math.sin(i+1)) * 2**32) & 0xFFFFFFFF for i in range(64)]

init_values = [0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476]

functions = 16*[lambda b, c, d: (b & c) | (~b & d)] + \
            16*[lambda b, c, d: (d & b) | (~d & c)] + \
            16*[lambda b, c, d: b ^ c ^ d] + \
            16*[lambda b, c, d: c ^ (b | ~d)]

index_functions = 16*[lambda i: i] + \
                  16*[lambda i: (5*i + 1)%16] + \
                  16*[lambda i: (3*i + 5)%16] + \
                  16*[lambda i: (7*i)%16]

def left_rotate(x, amount):
    x &= 0xFFFFFFFF
    return ((x<<amount) | (x>>(32-amount))) & 0xFFFFFFFF

def md5(message):

    message = bytearray(message) #copy our input into a mutable buffer
    orig_len_in_bits = (8 * len(message)) & 0xffffffffffffffff
    message.append(0x80)
    while len(message)%64 != 56:
        message.append(0)
    message += orig_len_in_bits.to_bytes(8, byteorder='little')

    hash_pieces = init_values[:]

    for chunk_ofst in range(0, len(message), 64):
        a, b, c, d = hash_pieces
        chunk = message[chunk_ofst:chunk_ofst+64]
        for i in range(64):
            f = functions[i](b, c, d)
            g = index_functions[i](i)
            to_rotate = a + f + constants[i] + int.from_bytes(chunk[4*g:4*g+4], byteorder='little')
            new_b = (b + left_rotate(to_rotate, rotate_amounts[i])) & 0xFFFFFFFF
            a, b, c, d = d, new_b, b, c
        for i, val in enumerate([a, b, c, d]):
            hash_pieces[i] += val
            hash_pieces[i] &= 0xFFFFFFFF

    return sum(x<<(32*i) for i, x in enumerate(hash_pieces))

def md5_to_hex(digest):
    raw = digest.to_bytes(16, byteorder='little')
    return '{:032x}'.format(int.from_bytes(raw, byteorder='big'))

if __name__=='__main__':
    demo = [b"", b"a", b"abc", b"message digest", b"abcdefghijklmnopqrstuvwxyz",
            b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
            b"12345678901234567890123456789012345678901234567890123456789012345678901234567890"]
    for message in demo:
        print(md5_to_hex(md5(message)),' <= "',message.decode('ascii'),'"', sep='')

```


Implementation notes:
* The code works with whole bytes, arbitrary message length is not supported.
* Instead of doing an if-else chain in the inner loop, we build a list of functions to use for each iteration. An if-else chain would probably be faster, but this shows off the language features better.
* Python integers don't ever overflow (they are implemented internally as bignums), so the code actually has to emulate 32-bit overflow by masking manually where it matters. On the other hand, this allows us to return the digest as a regular 128-bit number instead of a bytes object.
* The code makes heavy use of int.from_bytes() and int.to_bytes() to convert between bytes and integers. These methods were introduced in Python 3.2. In earlier versions, you needed to use more cumbersome ways to convert between the two types.
* The multiple assignment feature allows us to easily decompose the four items in hash_pieces into individual variables, and to shuffle around the four helper variables at the end of every iteration without introducing a temporary variable.


## Racket

For an implementation of md5 in Racket see: github.com/racket/racket/blob/master/racket/collects/file/md5.rkt

```racket

#lang racket
(require file/md5)
(md5 #"Rosetta Code")

```

Output:

```racket

#"cca1bf66b09554e10f837838c3d3efb1"

```



## REXX

This REXX program uses the test suite that is from the IETF RFC (1321) contained in the
  ''MD5 Message─Digest Algorithm'',   April 1992.

```rexx
/*REXX program tests the MD5 procedure (below) as per a test suite from IETF RFC (1321).*/
@.1 =                                            /*─────MD5 test suite [from above doc].*/
@.2 = 'a'
@.3 = 'abc'
@.4 = 'message digest'
@.5 = 'abcdefghijklmnopqrstuvwxyz'
@.6 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'
@.7 =  12345678901234567890123456789012345678901234567890123456789012345678901234567890
@.0 = 7                                          /* [↑]  last value doesn't need quotes.*/
                do m=1  for  @.0;         say    /*process each of the seven messages.  */
                say ' in ='  @.m                 /*display the      in      message.    */
                say 'out ='  MD5(@.m)            /*   "     "       out        "        */
                end   /*m*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
MD5: procedure; parse arg !;  numeric digits 20  /*insure there's enough decimal digits.*/
     a= '67452301'x;         b= "efcdab89"x;         c= '98badcfe'x;        d= "10325476"x
     #=length(!)                                 /*length in bytes of the input message.*/
     L=# *8 //512;   if L<448  then plus=448 - L /*is the length  less   than  448 ?    */
                     if L>448  then plus=960 - L /* "  "     "   greater   "    "       */
                     if L=448  then plus=512     /* "  "     "    equal   to    "       */
                                                 /* [↓]  a little of this, ···          */
     $=! || "80"x || copies('0'x,plus%8-1)reverse(right(d2c(8*#), 4, '0'x)) || '00000000'x
                                                 /* [↑]       ···  and a little of that.*/
         do j=0  for length($) % 64              /*process the message  (lots of steps).*/
         a_=a;      b_=b;      c_=c;       d_=d  /*save the  original values  for later.*/
         chunk=j * 64                            /*calculate the  size  of the chunks.  */
                       do k=1  for 16            /*process the message in chunks.       */
                       !.k=reverse( substr($, chunk + 1 + 4*(k-1), 4) )   /*magic stuff.*/
                       end   /*k*/                                        /*────step────*/
         a  =  .p1( a,   b,   c,   d,    0,    7,   3614090360)           /*■■■■  1 ■■■■*/
         d  =  .p1( d,   a,   b,   c,    1,   12,   3905402710)           /*■■■■  2 ■■■■*/
         c  =  .p1( c,   d,   a,   b,    2,   17,    606105819)           /*■■■■  3 ■■■■*/
         b  =  .p1( b,   c,   d,   a,    3,   22,   3250441966)           /*■■■■  4 ■■■■*/
         a  =  .p1( a,   b,   c,   d,    4,    7,   4118548399)           /*■■■■  5 ■■■■*/
         d  =  .p1( d,   a,   b,   c,    5,   12,   1200080426)           /*■■■■  6 ■■■■*/
         c  =  .p1( c,   d,   a,   b,    6,   17,   2821735955)           /*■■■■  7 ■■■■*/
         b  =  .p1( b,   c,   d,   a,    7,   22,   4249261313)           /*■■■■  8 ■■■■*/
         a  =  .p1( a,   b,   c,   d,    8,    7,   1770035416)           /*■■■■  9 ■■■■*/
         d  =  .p1( d,   a,   b,   c,    9,   12,   2336552879)           /*■■■■ 10 ■■■■*/
         c  =  .p1( c,   d,   a,   b,   10,   17,   4294925233)           /*■■■■ 11 ■■■■*/
         b  =  .p1( b,   c,   d,   a,   11,   22,   2304563134)           /*■■■■ 12 ■■■■*/
         a  =  .p1( a,   b,   c,   d,   12,    7,   1804603682)           /*■■■■ 13 ■■■■*/
         d  =  .p1( d,   a,   b,   c,   13,   12,   4254626195)           /*■■■■ 14 ■■■■*/
         c  =  .p1( c,   d,   a,   b,   14,   17,   2792965006)           /*■■■■ 15 ■■■■*/
         b  =  .p1( b,   c,   d,   a,   15,   22,   1236535329)           /*■■■■ 16 ■■■■*/
         a  =  .p2( a,   b,   c,   d,    1,    5,   4129170786)           /*■■■■ 17 ■■■■*/
         d  =  .p2( d,   a,   b,   c,    6,    9,   3225465664)           /*■■■■ 18 ■■■■*/
         c  =  .p2( c,   d,   a,   b,   11,   14,    643717713)           /*■■■■ 19 ■■■■*/
         b  =  .p2( b,   c,   d,   a,    0,   20,   3921069994)           /*■■■■ 20 ■■■■*/
         a  =  .p2( a,   b,   c,   d,    5,    5,   3593408605)           /*■■■■ 21 ■■■■*/
         d  =  .p2( d,   a,   b,   c,   10,    9,     38016083)           /*■■■■ 22 ■■■■*/
         c  =  .p2( c,   d,   a,   b,   15,   14,   3634488961)           /*■■■■ 23 ■■■■*/
         b  =  .p2( b,   c,   d,   a,    4,   20,   3889429448)           /*■■■■ 24 ■■■■*/
         a  =  .p2( a,   b,   c,   d,    9,    5,    568446438)           /*■■■■ 25 ■■■■*/
         d  =  .p2( d,   a,   b,   c,   14,    9,   3275163606)           /*■■■■ 26 ■■■■*/
         c  =  .p2( c,   d,   a,   b,    3,   14,   4107603335)           /*■■■■ 27 ■■■■*/
         b  =  .p2( b,   c,   d,   a,    8,   20,   1163531501)           /*■■■■ 28 ■■■■*/
         a  =  .p2( a,   b,   c,   d,   13,    5,   2850285829)           /*■■■■ 29 ■■■■*/
         d  =  .p2( d,   a,   b,   c,    2,    9,   4243563512)           /*■■■■ 30 ■■■■*/
         c  =  .p2( c,   d,   a,   b,    7,   14,   1735328473)           /*■■■■ 31 ■■■■*/
         b  =  .p2( b,   c,   d,   a,   12,   20,   2368359562)           /*■■■■ 32 ■■■■*/
         a  =  .p3( a,   b,   c,   d,    5,    4,   4294588738)           /*■■■■ 33 ■■■■*/
         d  =  .p3( d,   a,   b,   c,    8,   11,   2272392833)           /*■■■■ 34 ■■■■*/
         c  =  .p3( c,   d,   a,   b,   11,   16,   1839030562)           /*■■■■ 35 ■■■■*/
         b  =  .p3( b,   c,   d,   a,   14,   23,   4259657740)           /*■■■■ 36 ■■■■*/
         a  =  .p3( a,   b,   c,   d,    1,    4,   2763975236)           /*■■■■ 37 ■■■■*/
         d  =  .p3( d,   a,   b,   c,    4,   11,   1272893353)           /*■■■■ 38 ■■■■*/
         c  =  .p3( c,   d,   a,   b,    7,   16,   4139469664)           /*■■■■ 39 ■■■■*/
         b  =  .p3( b,   c,   d,   a,   10,   23,   3200236656)           /*■■■■ 40 ■■■■*/
         a  =  .p3( a,   b,   c,   d,   13,    4,    681279174)           /*■■■■ 41 ■■■■*/
         d  =  .p3( d,   a,   b,   c,    0,   11,   3936430074)           /*■■■■ 42 ■■■■*/
         c  =  .p3( c,   d,   a,   b,    3,   16,   3572445317)           /*■■■■ 43 ■■■■*/
         b  =  .p3( b,   c,   d,   a,    6,   23,     76029189)           /*■■■■ 44 ■■■■*/
         a  =  .p3( a,   b,   c,   d,    9,    4,   3654602809)           /*■■■■ 45 ■■■■*/
         d  =  .p3( d,   a,   b,   c,   12,   11,   3873151461)           /*■■■■ 46 ■■■■*/
         c  =  .p3( c,   d,   a,   b,   15,   16,    530742520)           /*■■■■ 47 ■■■■*/
         b  =  .p3( b,   c,   d,   a,    2,   23,   3299628645)           /*■■■■ 48 ■■■■*/
         a  =  .p4( a,   b,   c,   d,    0,    6,   4096336452)           /*■■■■ 49 ■■■■*/
         d  =  .p4( d,   a,   b,   c,    7,   10,   1126891415)           /*■■■■ 50 ■■■■*/
         c  =  .p4( c,   d,   a,   b,   14,   15,   2878612391)           /*■■■■ 51 ■■■■*/
         b  =  .p4( b,   c,   d,   a,    5,   21,   4237533241)           /*■■■■ 52 ■■■■*/
         a  =  .p4( a,   b,   c,   d,   12,    6,   1700485571)           /*■■■■ 53 ■■■■*/
         d  =  .p4( d,   a,   b,   c,    3,   10,   2399980690)           /*■■■■ 54 ■■■■*/
         c  =  .p4( c,   d,   a,   b,   10,   15,   4293915773)           /*■■■■ 55 ■■■■*/
         b  =  .p4( b,   c,   d,   a,    1,   21,   2240044497)           /*■■■■ 56 ■■■■*/
         a  =  .p4( a,   b,   c,   d,    8,    6,   1873313359)           /*■■■■ 57 ■■■■*/
         d  =  .p4( d,   a,   b,   c,   15,   10,   4264355552)           /*■■■■ 58 ■■■■*/
         c  =  .p4( c,   d,   a,   b,    6,   15,   2734768916)           /*■■■■ 59 ■■■■*/
         b  =  .p4( b,   c,   d,   a,   13,   21,   1309151649)           /*■■■■ 60 ■■■■*/
         a  =  .p4( a,   b,   c,   d,    4,    6,   4149444226)           /*■■■■ 61 ■■■■*/
         d  =  .p4( d,   a,   b,   c,   11,   10,   3174756917)           /*■■■■ 62 ■■■■*/
         c  =  .p4( c,   d,   a,   b,    2,   15,    718787259)           /*■■■■ 63 ■■■■*/
         b  =  .p4( b,   c,   d,   a,    9,   21,   3951481745)           /*■■■■ 64 ■■■■*/
         a  =  .a(a_, a);         b=.a(b_, b);          c=.a(c_, c);        d=.a(d_, d)
         end   /*j*/
     return .rx(a).rx(b).rx(c).rx(d)             /*same as:  .rx(a) || .rx(b) ||  ···   */
/*──────────────────────────────────────────────────────────────────────────────────────*/
.a:  return  right( d2c( c2d( arg(1) )   +   c2d( arg(2) ) ),  4, '0'x)
.h:  return  bitxor( bitxor( arg(1), arg(2) ), arg(3) )
.i:  return  bitxor( arg(2), bitor(arg(1),  bitxor(arg(3),        'ffffffff'x) ) )
.f:  return  bitor( bitand(arg(1), arg(2)), bitand(bitxor(arg(1), 'ffffffff'x), arg(3) ) )
.g:  return  bitor( bitand(arg(1), arg(3)), bitand(arg(2), bitxor(arg(3), 'ffffffff'x) ) )
.rx: return  c2x( reverse( arg(1) ) )
.Lr: procedure;  parse arg _,#;    if #==0  then return _             /*left bit rotate.*/
                 ?=x2b(c2x(_));    return x2c( b2x( right(? || left(?, #), length(?) ) ) )
.p1: procedure expose !.;   parse arg w,x,y,z,n,m,_;             n=n + 1
               return .a(.Lr(right(d2c(_+c2d(w) + c2d(.f(x,y,z)) + c2d(!.n)),4,'0'x),m),x)
.p2: procedure expose !.;   parse arg w,x,y,z,n,m,_;             n=n + 1
               return .a(.Lr(right(d2c(_+c2d(w) + c2d(.g(x,y,z)) + c2d(!.n)),4,'0'x),m),x)
.p3: procedure expose !.;   parse arg w,x,y,z,n,m,_;             n=n + 1
               return .a(.Lr(right(d2c(_+c2d(w) + c2d(.h(x,y,z)) + c2d(!.n)),4,'0'x),m),x)
.p4: procedure expose !.;   parse arg w,x,y,z,n,m,_;             n=n + 1
               return .a(.Lr(right(d2c(c2d(w) + c2d(.i(x,y,z)) + c2d(!.n)+_),4,'0'x),m),x)
```

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



## RPG

Based on my Java implementation. Uses free-form RPG and a CTDATA section to hold lookup tables. Converts input from EBCDIC to ASCII before hashing.


```RPG
**FREE
Ctl-opt MAIN(Main);
Ctl-opt DFTACTGRP(*NO) ACTGRP(*NEW);

dcl-pr QDCXLATE EXTPGM('QDCXLATE');
  dataLen         packed(5 : 0) CONST;
  data            char(32767) options(*VARSIZE);
  conversionTable char(10) CONST;
end-pr;

dcl-c MASK32 CONST(4294967295);
dcl-s SHIFT_AMTS int(3) dim(16) CTDATA PERRCD(16);
dcl-s MD5_TABLE_T int(20) dim(64) CTDATA PERRCD(4);

dcl-proc Main;
  dcl-s inputData char(45);
  dcl-s inputDataLen int(10) INZ(0);
  dcl-s outputHash char(16);
  dcl-s outputHashHex char(32);

  DSPLY 'Input: ' '' inputData;
  inputData = %trim(inputData);
  inputDataLen = %len(%trim(inputData));
  DSPLY ('Input=' + inputData);
  DSPLY ('InputLen=' + %char(inputDataLen));

  // Convert from EBCDIC to ASCII
  if inputDataLen > 0;
    QDCXLATE(inputDataLen : inputData : 'QTCPASC');
  endif;
  CalculateMD5(inputData : inputDataLen : outputHash);
  // Convert to hex
  ConvertToHex(outputHash : 16 : outputHashHex);
  DSPLY ('MD5: ' + outputHashHex);
  return;
end-proc;

dcl-proc CalculateMD5;
  dcl-pi *N;
    message    char(65535) options(*VARSIZE) CONST;
    messageLen int(10) value;
    outputHash char(16);
  end-pi;
  dcl-s numBlocks int(10);
  dcl-s padding char(72);
  dcl-s a int(20) INZ(1732584193);
  dcl-s b int(20) INZ(4023233417);
  dcl-s c int(20) INZ(2562383102);
  dcl-s d int(20) INZ(271733878);
  dcl-s buffer int(20) dim(16) INZ(0);
  dcl-s i int(10);
  dcl-s j int(10);
  dcl-s k int(10);
  dcl-s multiplier int(20);
  dcl-s index int(10);
  dcl-s originalA int(20);
  dcl-s originalB int(20);
  dcl-s originalC int(20);
  dcl-s originalD int(20);
  dcl-s div16 int(10);
  dcl-s f int(20);
  dcl-s tempInt int(20);
  dcl-s bufferIndex int(10);
  dcl-ds byteToInt QUALIFIED;
    n int(5) INZ(0);
    c char(1) OVERLAY(n : 2);
  end-ds;

  numBlocks = (messageLen + 8) / 64 + 1;
  MD5_FillPadding(messageLen : numBlocks : padding);
  for i = 0 to numBlocks - 1;
    index = i * 64;

    // Read message as little-endian 32-bit words
    for j = 1 to 16;
      multiplier = 1;
      for k = 1 to 4;
        index += 1;
        if index <= messageLen;
          byteToInt.c = %subst(message : index : 1);
        else;
          byteToInt.c = %subst(padding : index - messageLen : 1);
        endif;
        buffer(j) += multiplier * byteToInt.n;
        multiplier *= 256;
      endfor;
    endfor;

    originalA = a;
    originalB = b;
    originalC = c;
    originalD = d;

    for j = 0 to 63;
      div16 = j / 16;
      select;
        when div16 = 0;
          f = %bitor(%bitand(b : c) : %bitand(%bitnot(b) : d));
          bufferIndex = j;

        when div16 = 1;
          f = %bitor(%bitand(b : d) : %bitand(c : %bitnot(d)));
          bufferIndex = %bitand(j * 5 + 1 : 15);

        when div16 = 2;
          f = %bitxor(b : %bitxor(c : d));
          bufferIndex = %bitand(j * 3 + 5 : 15);

        when div16 = 3;
          f = %bitxor(c : %bitor(b : Mask32Bit(%bitnot(d))));
          bufferIndex = %bitand(j * 7 : 15);
      endsl;
      tempInt = Mask32Bit(b + RotateLeft32Bit(a + f + buffer(bufferIndex + 1) + MD5_TABLE_T(j + 1) :
                                              SHIFT_AMTS(div16 * 4 + %bitand(j : 3) + 1)));
      a = d;
      d = c;
      c = b;
      b = tempInt;
    endfor;
    a = Mask32Bit(a + originalA);
    b = Mask32Bit(b + originalB);
    c = Mask32Bit(c + originalC);
    d = Mask32Bit(d + originalD);
  endfor;

  for i = 0 to 3;
    if i = 0;
      tempInt = a;
    elseif i = 1;
      tempInt = b;
    elseif i = 2;
      tempInt = c;
    else;
      tempInt = d;
    endif;

    for j = 0 to 3;
      byteToInt.n = %bitand(tempInt : 255);
      %subst(outputHash : i * 4 + j + 1 : 1) = byteToInt.c;
      tempInt /= 256;
    endfor;
  endfor;
  return;
end-proc;

dcl-proc MD5_FillPadding;
  dcl-pi *N;
    messageLen int(10);
    numBlocks int(10);
    padding char(72);
  end-pi;
  dcl-s totalLen int(10);
  dcl-s paddingSize int(10);
  dcl-ds *N;
    messageLenBits int(20);
    mlb_bytes char(8) OVERLAY(messageLenBits);
  end-ds;
  dcl-s i int(10);

  %subst(padding : 1 : 1) = X'80';
  totalLen = numBlocks * 64;
  paddingSize = totalLen - messageLen; // 9 to 72
  messageLenBits = messageLen;
  messageLenBits *= 8;
  for i = 1 to 8;
    %subst(padding : paddingSize - i + 1 : 1) = %subst(mlb_bytes : i : 1);
  endfor;
  for i = 2 to paddingSize - 8;
    %subst(padding : i : 1) = X'00';
  endfor;
  return;
end-proc;

dcl-proc RotateLeft32Bit;
  dcl-pi *N int(20);
    n int(20) value;
    amount int(3) value;
  end-pi;
  dcl-s i int(3);

  n = Mask32Bit(n);
  for i = 1 to amount;
    n *= 2;
    if n >= 4294967296;
      n -= MASK32;
    endif;
  endfor;
  return n;
end-proc;

dcl-proc Mask32Bit;
  dcl-pi *N int(20);
    n int(20) value;
  end-pi;
  return %bitand(n : MASK32);
end-proc;

dcl-proc ConvertToHex;
  dcl-pi *N;
    inputData    char(32767) options(*VARSIZE) CONST;
    inputDataLen int(10) value;
    outputData   char(65534) options(*VARSIZE);
  end-pi;
  dcl-c HEX_CHARS CONST('0123456789ABCDEF');
  dcl-s i int(10);
  dcl-s outputOffset int(10) INZ(1);
  dcl-ds dataStruct QUALIFIED;
    numField int(5) INZ(0);
    // IBM i is big-endian
    charField char(1) OVERLAY(numField : 2);
  end-ds;

  for i = 1 to inputDataLen;
    dataStruct.charField = %BitAnd(%subst(inputData : i : 1) : X'F0');
    dataStruct.numField /= 16;
    %subst(outputData : outputOffset : 1) = %subst(HEX_CHARS : dataStruct.numField + 1 : 1);
    outputOffset += 1;
    dataStruct.charField = %BitAnd(%subst(inputData : i : 1) : X'0F');
    %subst(outputData : outputOffset : 1) = %subst(HEX_CHARS : dataStruct.numField + 1 : 1);
    outputOffset += 1;
  endfor;
  return;
end-proc;

**CTDATA SHIFT_AMTS
  7 12 17 22  5  9 14 20  4 11 16 23  6 10 15 21
**CTDATA MD5_TABLE_T
          3614090360          3905402710           606105819          3250441966
          4118548399          1200080426          2821735955          4249261313
          1770035416          2336552879          4294925233          2304563134
          1804603682          4254626195          2792965006          1236535329
          4129170786          3225465664           643717713          3921069994
          3593408605            38016083          3634488961          3889429448
           568446438          3275163606          4107603335          1163531501
          2850285829          4243563512          1735328473          2368359562
          4294588738          2272392833          1839030562          4259657740
          2763975236          1272893353          4139469664          3200236656
           681279174          3936430074          3572445317            76029189
          3654602809          3873151461           530742520          3299628645
          4096336452          1126891415          2878612391          4237533241
          1700485571          2399980690          4293915773          2240044497
          1873313359          4264355552          2734768916          1309151649
          4149444226          3174756917           718787259          3951481745
```


Sample output:

```txt
 DSPLY  Input:
 abcdefghijklmnopqrstuvwxyz
 DSPLY  Input=abcdefghijklmnopqrstuvwxyz
 DSPLY  InputLen=26
 DSPLY  MD5: C3FCD3D76192E4007DFB496CCA67E13B
```




## Rust


A translation of RFC (1321), to highlight the algorithm itself in bare form. Notable Rust features are the strict limits on casting,  explicit description of bit-widths and wrap-around operations, and macros for the Rounds. This made it easy to translate and debug, although a bit 'wordy' in sections, and requiring special code to transliterate between strings and integers.

Reasonable speed was desired; Profiling revealed the "copy block to X" loop as hot. This loops casts arrays of 8 bit integers into 32 bit integers, which Rust does very slowly in 'safe' code, so an unsafe transmute, and, on big-endian machines a byteswap, was used. Runtime is 120-150% slower than standard linux /usr/bin/md5sum.

As with others implementations, this will not allow bit-lengths non-divisible by 8; this ability can be added in about 8 lines of code but no examples appear to be available for verification of correctness, and so it was elided.


```C

#![allow(non_snake_case)] // RFC 1321 uses many capitalized variables
use std::mem;

fn md5(mut msg: Vec<u8>) -> (u32, u32, u32, u32) {
    let bitcount = msg.len().saturating_mul(8) as u64;

    // Step 1: Append Padding Bits
    msg.push(0b10000000);
    while (msg.len() * 8) % 512 != 448 {
        msg.push(0u8);
    }

    // Step 2. Append Length  (64 bit integer)
    msg.extend(&[
        bitcount as u8,
        (bitcount >> 8) as u8,
        (bitcount >> 16) as u8,
        (bitcount >> 24) as u8,
        (bitcount >> 32) as u8,
        (bitcount >> 40) as u8,
        (bitcount >> 48) as u8,
        (bitcount >> 56) as u8,
    ]);

    // Step 3. Initialize MD Buffer
    /*A four-word buffer (A,B,C,D) is used to compute the message digest.
    Here each of A, B, C, D is a 32-bit register.*/
    let mut A = 0x67452301u32;
    let mut B = 0xefcdab89u32;
    let mut C = 0x98badcfeu32;
    let mut D = 0x10325476u32;

    // Step 4. Process Message in 16-Word Blocks
    /* We first define four auxiliary functions */
    let F = |X: u32, Y: u32, Z: u32| -> u32 { X & Y | !X & Z };
    let G = |X: u32, Y: u32, Z: u32| -> u32 { X & Z | Y & !Z };
    let H = |X: u32, Y: u32, Z: u32| -> u32 { X ^ Y ^ Z };
    let I = |X: u32, Y: u32, Z: u32| -> u32 { Y ^ (X | !Z) };

    /* This step uses a 64-element table T[1 ... 64] constructed from the sine function.  */
    let T = [
        0x00000000, // enable use as a 1-indexed table
        0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a, 0xa8304613,
        0xfd469501, 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be, 0x6b901122, 0xfd987193,
        0xa679438e, 0x49b40821, 0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa, 0xd62f105d,
        0x02441453, 0xd8a1e681, 0xe7d3fbc8, 0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
        0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a, 0xfffa3942, 0x8771f681, 0x6d9d6122,
        0xfde5380c, 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70, 0x289b7ec6, 0xeaa127fa,
        0xd4ef3085, 0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665, 0xf4292244,
        0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
        0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1, 0xf7537e82, 0xbd3af235, 0x2ad7d2bb,
        0xeb86d391,
    ];

    /* Process each 16-word block. (since 1 word is 4 bytes, then 16 words is 64 bytes) */
    for mut block in msg.chunks_exact_mut(64) {
        /* Copy block into X. */
        #![allow(unused_mut)]
        let mut X = unsafe { mem::transmute::<&mut [u8], &mut [u32]>(&mut block) };
        #[cfg(target_endian = "big")]
        for j in 0..16 {
            X[j] = X[j].swap_bytes();
        }

        /* Save Registers A,B,C,D */
        let AA = A;
        let BB = B;
        let CC = C;
        let DD = D;

        /* Round 1.  Let [abcd k s i] denote the operation
        a = b + ((a + F(b,c,d) + X[k] + T[i]) <<< s). */
        macro_rules! op1 {
            ($a:ident,$b:ident,$c:ident,$d:ident,$k:expr,$s:expr,$i:expr) => {
                $a = $b.wrapping_add(
                    ($a.wrapping_add(F($b, $c, $d))
                        .wrapping_add(X[$k])
                        .wrapping_add(T[$i]))
                    .rotate_left($s),
                )
            };
        }

        /* Do the following 16 operations. */
        op1!(A, B, C, D, 0, 7, 1);
        op1!(D, A, B, C, 1, 12, 2);
        op1!(C, D, A, B, 2, 17, 3);
        op1!(B, C, D, A, 3, 22, 4);

        op1!(A, B, C, D, 4, 7, 5);
        op1!(D, A, B, C, 5, 12, 6);
        op1!(C, D, A, B, 6, 17, 7);
        op1!(B, C, D, A, 7, 22, 8);

        op1!(A, B, C, D, 8, 7, 9);
        op1!(D, A, B, C, 9, 12, 10);
        op1!(C, D, A, B, 10, 17, 11);
        op1!(B, C, D, A, 11, 22, 12);

        op1!(A, B, C, D, 12, 7, 13);
        op1!(D, A, B, C, 13, 12, 14);
        op1!(C, D, A, B, 14, 17, 15);
        op1!(B, C, D, A, 15, 22, 16);

        /* Round 2. Let [abcd k s i] denote the operation
        a = b + ((a + G(b,c,d) + X[k] + T[i]) <<< s). */
        macro_rules! op2 {
            ($a:ident,$b:ident,$c:ident,$d:ident,$k:expr,$s:expr,$i:expr) => {
                $a = $b.wrapping_add(
                    ($a.wrapping_add(G($b, $c, $d))
                        .wrapping_add(X[$k])
                        .wrapping_add(T[$i]))
                    .rotate_left($s),
                )
            };
        }

        /* Do the following 16 operations. */
        op2!(A, B, C, D, 1, 5, 17);
        op2!(D, A, B, C, 6, 9, 18);
        op2!(C, D, A, B, 11, 14, 19);
        op2!(B, C, D, A, 0, 20, 20);

        op2!(A, B, C, D, 5, 5, 21);
        op2!(D, A, B, C, 10, 9, 22);
        op2!(C, D, A, B, 15, 14, 23);
        op2!(B, C, D, A, 4, 20, 24);

        op2!(A, B, C, D, 9, 5, 25);
        op2!(D, A, B, C, 14, 9, 26);
        op2!(C, D, A, B, 3, 14, 27);
        op2!(B, C, D, A, 8, 20, 28);

        op2!(A, B, C, D, 13, 5, 29);
        op2!(D, A, B, C, 2, 9, 30);
        op2!(C, D, A, B, 7, 14, 31);
        op2!(B, C, D, A, 12, 20, 32);

        /* Round 3. Let [abcd k s t] denote the operation
        a = b + ((a + H(b,c,d) + X[k] + T[i]) <<< s). */
        macro_rules! op3 {
            ($a:ident,$b:ident,$c:ident,$d:ident,$k:expr,$s:expr,$i:expr) => {
                $a = $b.wrapping_add(
                    ($a.wrapping_add(H($b, $c, $d))
                        .wrapping_add(X[$k])
                        .wrapping_add(T[$i]))
                    .rotate_left($s),
                )
            };
        }

        /* Do the following 16 operations. */
        op3!(A, B, C, D, 5, 4, 33);
        op3!(D, A, B, C, 8, 11, 34);
        op3!(C, D, A, B, 11, 16, 35);
        op3!(B, C, D, A, 14, 23, 36);

        op3!(A, B, C, D, 1, 4, 37);
        op3!(D, A, B, C, 4, 11, 38);
        op3!(C, D, A, B, 7, 16, 39);
        op3!(B, C, D, A, 10, 23, 40);

        op3!(A, B, C, D, 13, 4, 41);
        op3!(D, A, B, C, 0, 11, 42);
        op3!(C, D, A, B, 3, 16, 43);
        op3!(B, C, D, A, 6, 23, 44);

        op3!(A, B, C, D, 9, 4, 45);
        op3!(D, A, B, C, 12, 11, 46);
        op3!(C, D, A, B, 15, 16, 47);
        op3!(B, C, D, A, 2, 23, 48);

        /* Round 4. Let [abcd k s t] denote the operation
        a = b + ((a + I(b,c,d) + X[k] + T[i]) <<< s). */
        macro_rules! op4 {
            ($a:ident,$b:ident,$c:ident,$d:ident,$k:expr,$s:expr,$i:expr) => {
                $a = $b.wrapping_add(
                    ($a.wrapping_add(I($b, $c, $d))
                        .wrapping_add(X[$k])
                        .wrapping_add(T[$i]))
                    .rotate_left($s),
                )
            };
        }

        /* Do the following 16 operations. */
        op4!(A, B, C, D, 0, 6, 49);
        op4!(D, A, B, C, 7, 10, 50);
        op4!(C, D, A, B, 14, 15, 51);
        op4!(B, C, D, A, 5, 21, 52);

        op4!(A, B, C, D, 12, 6, 53);
        op4!(D, A, B, C, 3, 10, 54);
        op4!(C, D, A, B, 10, 15, 55);
        op4!(B, C, D, A, 1, 21, 56);

        op4!(A, B, C, D, 8, 6, 57);
        op4!(D, A, B, C, 15, 10, 58);
        op4!(C, D, A, B, 6, 15, 59);
        op4!(B, C, D, A, 13, 21, 60);

        op4!(A, B, C, D, 4, 6, 61);
        op4!(D, A, B, C, 11, 10, 62);
        op4!(C, D, A, B, 2, 15, 63);
        op4!(B, C, D, A, 9, 21, 64);

        /* . . . increment each of the four registers by the value
        it had before this block was started.) */

        A = A.wrapping_add(AA);
        B = B.wrapping_add(BB);
        C = C.wrapping_add(CC);
        D = D.wrapping_add(DD);
    }
    (
        A.swap_bytes(),
        B.swap_bytes(),
        C.swap_bytes(),
        D.swap_bytes(),
    )
}

fn md5_utf8(smsg: &str) -> String {
    let mut msg = vec![0u8; 0];
    msg.extend(smsg.as_bytes());
    let (A, B, C, D) = md5(msg);
    format!("{:08x}{:08x}{:08x}{:08x}", A, B, C, D)
}

fn main() {
    assert!(md5_utf8("") == "d41d8cd98f00b204e9800998ecf8427e");
    assert!(md5_utf8("a") == "0cc175b9c0f1b6a831c399e269772661");
    assert!(md5_utf8("abc") == "900150983cd24fb0d6963f7d28e17f72");
    assert!(md5_utf8("message digest") == "f96b697d7cb7938d525a2f31aaf161d0");
    assert!(md5_utf8("abcdefghijklmnopqrstuvwxyz") == "c3fcd3d76192e4007dfb496cca67e13b");
    assert!(md5_utf8("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") == "d174ab98d277d9f5a5611c2c9f419d9f");
    assert!(md5_utf8("12345678901234567890123456789012345678901234567890123456789012345678901234567890") == "57edf4a22be3c955ac49da2e2107b67a");
}

```



## Scala


```Scala
object MD5 extends App {

  def hash(s: String) = {
    def b = s.getBytes("UTF-8")

    def m = java.security.MessageDigest.getInstance("MD5").digest(b)

    BigInt(1, m).toString(16).reverse.padTo(32, "0").reverse.mkString
  }

  assert("d41d8cd98f00b204e9800998ecf8427e" == hash(""))
  assert("0000045c5e2b3911eb937d9d8c574f09" == hash("iwrupvqb346386"))
  assert("0cc175b9c0f1b6a831c399e269772661" == hash("a"))
  assert("900150983cd24fb0d6963f7d28e17f72" == hash("abc"))
  assert("f96b697d7cb7938d525a2f31aaf161d0" == hash("message digest"))
  assert("c3fcd3d76192e4007dfb496cca67e13b" == hash("abcdefghijklmnopqrstuvwxyz"))
  assert("d174ab98d277d9f5a5611c2c9f419d9f" == hash("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))
  assert("57edf4a22be3c955ac49da2e2107b67a" == hash("12345678901234567890123456789012345678901234567890123456789012345678901234567890"))

}
```



## Seed7

The example below contains the implementation of the function [http://seed7.sourceforge.net/libraries/msgdigest.htm#md5%28in_var_string%29 md5] from the library [http://seed7.sourceforge.net/libraries/msgdigest.htm msgdigest.s7i].


```seed7
$ include "seed7_05.s7i";
  include "bytedata.s7i";
  include "bin32.s7i";
  include "float.s7i";
  include "math.s7i";

# Use binary integer part of the sines of integers (Radians) as constants:
const func array integer: createMd5Table is func
  result
    var array integer: k is 64 times 0;
  local
    var integer: index is 0;
  begin
    for index range 1 to 64 do
      k[index] := trunc(abs(sin(flt(index))) * 2.0 ** 32);
    end for;
  end func;

const func string: md5 (in var string: message) is func
  result
    var string: digest is "";
  local
    # Specify the per-round shift amounts
    const array integer: shiftAmount is [] (
        7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
        5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
        4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
        6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21);
    const array integer: k is createMd5Table;
    var integer: length is 0;
    var integer: chunkIndex is 0;
    var integer: index is 0;
    var array bin32: m is 16 times bin32.value;
    var integer: a0 is 16#67452301;   # a
    var integer: b0 is 16#efcdab89;   # b
    var integer: c0 is 16#98badcfe;   # c
    var integer: d0 is 16#10325476;   # d
    var bin32: a is bin32(0);
    var bin32: b is bin32(0);
    var bin32: c is bin32(0);
    var bin32: d is bin32(0);
    var bin32: f is bin32(0);
    var integer: g is 0;
    var bin32: temp is bin32(0);
  begin
    length := length(message);
    # Append the bit '1' to the message.
    message &:= '\16#80;';
    # Append '0' bits, so that the resulting bit length is congruent to 448 (mod 512).
    message &:= "\0;" mult 63 - (length + 8) mod 64;
    # Append length of message (before pre-processing), in bits, as 64-bit little-endian integer.
    message &:= int64AsEightBytesLe(8 * length);

    # Process the message in successive 512-bit chunks:
    for chunkIndex range 1 to length(message) step 64 do
      # Break chunk into sixteen 32-bit little-endian words.
      for index range 1 to 16 do
        m[index] := bin32(bytes2Int(message[chunkIndex + 4 * pred(index) len 4], UNSIGNED, LE));
      end for;

      a := bin32(a0 mod 16#100000000);
      b := bin32(b0 mod 16#100000000);
      c := bin32(c0 mod 16#100000000);
      d := bin32(d0 mod 16#100000000);

      for index range 1 to 64 do
        if index <= 16 then
          f := d >< (b & (c >< d));
          g := index;
        elsif index <= 32 then
          f := c >< (d & (b >< c));
          g := (5 * index - 4) mod 16 + 1;
        elsif index <= 48 then
          f := b >< c >< d;
          g := (3 * index + 2) mod 16 + 1;
        else
          f := c >< (b | (bin32(16#ffffffff) >< d));
          g := (7 * pred(index)) mod 16 + 1;
        end if;

        temp := d;
        d := c;
        c := b;
        b := bin32((ord(b) +
             ord(rotLeft(bin32((ord(a) + ord(f) + k[index] + ord(m[g])) mod 16#100000000),
                         shiftAmount[index]))) mod 16#100000000);
        a := temp;
      end for;

      # Add this chunk's hash to result so far:
      a0 +:= ord(a);
      b0 +:= ord(b);
      c0 +:= ord(c);
      d0 +:= ord(d);
    end for;

    # Produce the final hash value:
    digest := int32AsFourBytesLe(a0) &
              int32AsFourBytesLe(b0) &
              int32AsFourBytesLe(c0) &
              int32AsFourBytesLe(d0);
  end func;

const func boolean: checkMd5 (in string: message, in string: hexMd5) is
  return hex(md5(message)) = hexMd5;

const proc: main is func
  begin
    if  checkMd5("", "d41d8cd98f00b204e9800998ecf8427e") and
        checkMd5("a", "0cc175b9c0f1b6a831c399e269772661") and
        checkMd5("abc", "900150983cd24fb0d6963f7d28e17f72") and
        checkMd5("message digest", "f96b697d7cb7938d525a2f31aaf161d0") and
        checkMd5("abcdefghijklmnopqrstuvwxyz", "c3fcd3d76192e4007dfb496cca67e13b") and
        checkMd5("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", "d174ab98d277d9f5a5611c2c9f419d9f") and
        checkMd5("12345678901234567890123456789012345678901234567890123456789012345678901234567890", "57edf4a22be3c955ac49da2e2107b67a") then
      writeln("md5 is computed correct");
    else
      writeln("There is an error in the md5 function");
    end if;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/msgdigest.htm#md5]

```txt

md5 is computed correct

```



## Sidef

```ruby
class MD5(String msg) {

    method init {
        msg = msg.bytes
    }

    const FGHI = [
        {|a,b,c| (a & b) | (~a & c) },
        {|a,b,c| (a & c) | (b & ~c) },
        {|a,b,c| (a ^ b ^ c)        },
        {|a,b,c| (b ^ (a | ~c))     },
    ]

    const S = [
        [7, 12, 17, 22] * 4,
        [5,  9, 14, 20] * 4,
        [4, 11, 16, 23] * 4,
        [6, 10, 15, 21] * 4,
    ].flat

    const T = 64.of {|i| floor(abs(sin(i+1)) * 1<<32) }

    const K = [
        ^16 -> map {|n|    n           },
        ^16 -> map {|n| (5*n + 1) % 16 },
        ^16 -> map {|n| (3*n + 5) % 16 },
        ^16 -> map {|n| (7*n    ) % 16 },
    ].flat

    func radix(Number b, Array a) {
        ^a -> sum {|i| b**i * a[i] }
    }

    func little_endian(Number w, Number n, Array v) {
        var step1 = (^n »*» w)
        var step2 = (v ~X>> step1)
        step2 »%» (1 << w)
    }

    func block(Number a, Number b) { (a  + b) & 0xffffffff }
    func srble(Number a, Number n) { (a << n) & 0xffffffff | (a >> (32-n)) }

    func md5_pad(msg) {
        var bits = 8*msg.len
        var padded = [msg..., 128, [0] * (-(floor(bits / 8) + 1 + 8) % 64)].flat

        gather {
            padded.each_slice(4, {|*a|
                take(radix(256, a))
            })
            take(little_endian(32, 2, [bits]))
        }.flat
    }

    func md5_block(Array H, Array X) {
        var (A, B, C, D) = H...

        for i in ^64 {
            (A, B, C, D) = (D,
                block(B, srble(
                    block(
                        block(
                            block(A, FGHI[floor(i / 16)](B, C, D)), T[i]
                        ), X[K[i]]
                    ), S[i])
                ), B, C)
        }

        for k,v in ([A, B, C, D].kv) {
            H[k] = block(H[k], v)
        }

        return H
    }

    method md5_hex {
        self.md5.map {|n| '%02x' % n }.join
    }

    method md5 {
        var M = md5_pad(msg)
        var H = [0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476]

        for i in (range(0, M.end, 16)) {
            md5_block(H, M.ft(i, i+15))
        }

        little_endian(8, 4, H)
    }
}

var tests = [
    ['d41d8cd98f00b204e9800998ecf8427e', ''],
    ['0cc175b9c0f1b6a831c399e269772661', 'a'],
    ['900150983cd24fb0d6963f7d28e17f72', 'abc'],
    ['f96b697d7cb7938d525a2f31aaf161d0', 'message digest'],
    ['c3fcd3d76192e4007dfb496cca67e13b', 'abcdefghijklmnopqrstuvwxyz'],
    ['d174ab98d277d9f5a5611c2c9f419d9f', 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'],
    ['57edf4a22be3c955ac49da2e2107b67a', '12345678901234567890123456789012345678901234567890123456789012345678901234567890'],
]

for md5,msg in tests {
    var hash = MD5(msg).md5_hex
    say "#{hash} : #{msg}"

    if (hash != md5) {
        say "\tHowever, that is incorrect (expected: #{md5})"
    }
}
```

```txt

d41d8cd98f00b204e9800998ecf8427e :
0cc175b9c0f1b6a831c399e269772661 : a
900150983cd24fb0d6963f7d28e17f72 : abc
f96b697d7cb7938d525a2f31aaf161d0 : message digest
c3fcd3d76192e4007dfb496cca67e13b : abcdefghijklmnopqrstuvwxyz
d174ab98d277d9f5a5611c2c9f419d9f : ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789
57edf4a22be3c955ac49da2e2107b67a : 12345678901234567890123456789012345678901234567890123456789012345678901234567890

```



## Swift


Swift implementation of the pseudo-code found in the Wikipedia article.

Original source: [//github.com/krzyzanowskim/CryptoSwift CryptoSwift]


```swift

    import Foundation
    public class MD5 {
        /** specifies the per-round shift amounts */
        private let s: [UInt32] = [7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
                           5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
                           4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
                           6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21]

        /** binary integer part of the sines of integers (Radians) */
        private let K: [UInt32] = (0 ..< 64).map { UInt32(0x100000000 * abs(sin(Double($0 + 1)))) }

        let a0: UInt32 = 0x67452301
        let b0: UInt32 = 0xefcdab89
        let c0: UInt32 = 0x98badcfe
        let d0: UInt32 = 0x10325476

        private var message: NSData

        //MARK: Public

        public init(_ message: NSData) {
            self.message = message
        }

        public func calculate() -> NSData? {
            var tmpMessage: NSMutableData = NSMutableData(data: message)
            let wordSize = sizeof(UInt32)

            var aa = a0
            var bb = b0
            var cc = c0
            var dd = d0

            // Step 1. Append Padding Bits
            tmpMessage.appendBytes([0x80]) // append one bit (Byte with one bit) to message

            // append "0" bit until message length in bits ≡ 448 (mod 512)
            while tmpMessage.length % 64 != 56 {
                tmpMessage.appendBytes([0x00])
            }

            // Step 2. Append Length a 64-bit representation of lengthInBits
            var lengthInBits = (message.length * 8)
            var lengthBytes = lengthInBits.bytes(64 / 8)
            tmpMessage.appendBytes(reverse(lengthBytes));

            // Process the message in successive 512-bit chunks:
            let chunkSizeBytes = 512 / 8
            var leftMessageBytes = tmpMessage.length
            for var i = 0; i < tmpMessage.length; i = i + chunkSizeBytes, leftMessageBytes -= chunkSizeBytes {
                let chunk = tmpMessage.subdataWithRange(NSRange(location: i, length: min(chunkSizeBytes,leftMessageBytes)))

                // break chunk into sixteen 32-bit words M[j], 0 ≤ j ≤ 15
                // println("wordSize \(wordSize)");
                var M:[UInt32] = [UInt32](count: 16, repeatedValue: 0)
                for x in 0..<M.count {
                    var range = NSRange(location:x * wordSize, length: wordSize)
                    chunk.getBytes(&M[x], range:range);
                }

                // Initialize hash value for this chunk:
                var A:UInt32 = a0
                var B:UInt32 = b0
                var C:UInt32 = c0
                var D:UInt32 = d0

                var dTemp:UInt32 = 0

                // Main loop
                for j in 0...63 {
                    var g = 0
                    var F:UInt32 = 0

                    switch (j) {
                    case 0...15:
                        F = (B & C) | ((~B) & D)
                        g = j
                        break
                    case 16...31:
                        F = (D & B) | (~D & C)
                        g = (5 * j + 1) % 16
                        break
                    case 32...47:
                        F = B ^ C ^ D
                        g = (3 * j + 5) % 16
                        break
                    case 48...63:
                        F = C ^ (B | (~D))
                        g = (7 * j) % 16
                        break
                    default:
                        break
                    }
                    dTemp = D
                    D = C
                    C = B
                    B = B &+ rotateLeft((A &+ F &+ K[j] &+ M[g]), s[j])
                    A = dTemp
                }

                aa = aa &+ A
                bb = bb &+ B
                cc = cc &+ C
                dd = dd &+ D
            }

            var buf: NSMutableData = NSMutableData();
            buf.appendBytes(&aa, length: wordSize)
            buf.appendBytes(&bb, length: wordSize)
            buf.appendBytes(&cc, length: wordSize)
            buf.appendBytes(&dd, length: wordSize)

            return buf.copy() as? NSData;
        }

        //MARK: Class
        class func calculate(message: NSData) -> NSData?
        {
            return MD5(message).calculate();
        }

        //MARK: Private
        private func rotateLeft(x:UInt32, _ n:UInt32) -> UInt32 {
            return (x &<< n) | (x &>> (32 - n))
        }
    }

```


From-scratch implementation based on the solutions on this page without needing any external libraries:

```swift
import Foundation

let shift : [UInt32] = [7, 12, 17, 22, 5, 9, 14, 20, 4, 11, 16, 23, 6, 10, 15, 21]
let table: [UInt32] = (0 ..< 64).map { UInt32(0x100000000 * abs(sin(Double($0 + 1)))) }

func md5(var message: [UInt8]) -> [UInt8] {
  var messageLenBits = UInt64(message.count) * 8
  message.append(0x80)
  while message.count % 64 != 56 {
    message.append(0)
  }

  var lengthBytes = [UInt8](count: 8, repeatedValue: 0)
  UnsafeMutablePointer<UInt64>(lengthBytes).memory = messageLenBits.littleEndian
  message += lengthBytes

  var a : UInt32 = 0x67452301
  var b : UInt32 = 0xEFCDAB89
  var c : UInt32 = 0x98BADCFE
  var d : UInt32 = 0x10325476
  for chunkOffset in stride(from: 0, to: message.count, by: 64) {
    let chunk = UnsafePointer<UInt32>(UnsafePointer<UInt8>(message) + chunkOffset)
    let originalA = a
    let originalB = b
    let originalC = c
    let originalD = d
    for j in 0 ..< 64 {
      var f : UInt32 = 0
      var bufferIndex = j
      let round = j >> 4
      switch round {
      case 0:
        f = (b & c) | (~b & d)
      case 1:
        f = (b & d) | (c & ~d)
        bufferIndex = (bufferIndex*5 + 1) & 0x0F
      case 2:
        f = b ^ c ^ d
        bufferIndex = (bufferIndex*3 + 5) & 0x0F
      case 3:
        f = c ^ (b | ~d)
        bufferIndex = (bufferIndex * 7) & 0x0F
      default:
        assert(false)
      }
      let sa = shift[(round<<2)|(j&3)]
      let tmp = a &+ f &+ UInt32(littleEndian: chunk[bufferIndex]) &+ table[j]
      a = d
      d = c
      c = b
      b = b &+ (tmp << sa | tmp >> (32-sa))
    }
    a = a &+ originalA
    b = b &+ originalB
    c = c &+ originalC
    d = d &+ originalD
  }

  var result = [UInt8](count: 16, repeatedValue: 0)
  for (i, n) in enumerate([a, b, c, d]) {
    UnsafeMutablePointer<UInt32>(result)[i] = n.littleEndian
  }
  return result
}

func toHexString(bytes: [UInt8]) -> String {
  return "".join(bytes.map { String(format:"%02x", $0) })
}

for (hashCode, string) in [
  ("d41d8cd98f00b204e9800998ecf8427e", ""),
  ("0cc175b9c0f1b6a831c399e269772661", "a"),
  ("900150983cd24fb0d6963f7d28e17f72", "abc"),
  ("f96b697d7cb7938d525a2f31aaf161d0", "message digest"),
  ("c3fcd3d76192e4007dfb496cca67e13b", "abcdefghijklmnopqrstuvwxyz"),
  ("d174ab98d277d9f5a5611c2c9f419d9f",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
  ("57edf4a22be3c955ac49da2e2107b67a", "12345678901234567890" +
    "123456789012345678901234567890123456789012345678901234567890")] {
      println(hashCode)
      println(toHexString(md5(Array(string.utf8))))
      println()
}
```

```txt

d41d8cd98f00b204e9800998ecf8427e
d41d8cd98f00b204e9800998ecf8427e

0cc175b9c0f1b6a831c399e269772661
0cc175b9c0f1b6a831c399e269772661

900150983cd24fb0d6963f7d28e17f72
900150983cd24fb0d6963f7d28e17f72

f96b697d7cb7938d525a2f31aaf161d0
f96b697d7cb7938d525a2f31aaf161d0

c3fcd3d76192e4007dfb496cca67e13b
c3fcd3d76192e4007dfb496cca67e13b

d174ab98d277d9f5a5611c2c9f419d9f
d174ab98d277d9f5a5611c2c9f419d9f

57edf4a22be3c955ac49da2e2107b67a
57edf4a22be3c955ac49da2e2107b67a

```



## Tcl

<small>This code is extracted from the <code>md5</code> package in {{libheader|tcllib}}, and is originally due to Don Libes's transcription of the code in the MD5 specification. ''It should not be deployed in production normally; the <code>md5</code> package should be used in preference as it is usually built to be faster.</small>

```tcl
# We just define the body of md5::md5 here; later we regsub to inline a few
# function calls for speed
variable ::md5::md5body {
    ### Step 1. Append Padding Bits

    set msgLen [string length $msg]

    set padLen [expr {56 - $msgLen%64}]
    if {$msgLen % 64 > 56} {
	incr padLen 64
    }

    # pad even if no padding required
    if {$padLen == 0} {
	incr padLen 64
    }

    # append single 1b followed by 0b's
    append msg [binary format "a$padLen" \200]

    ### Step 2. Append Length

    # RFC doesn't say whether to use little- or big-endian; code demonstrates
    # little-endian.
    # This step limits our input to size 2^32b or 2^24B
    append msg [binary format "i1i1" [expr {8*$msgLen}] 0]

    ### Step 3. Initialize MD Buffer

    set A [expr 0x67452301]
    set B [expr 0xefcdab89]
    set C [expr 0x98badcfe]
    set D [expr 0x10325476]

    ### Step 4. Process Message in 16-Word Blocks

    # process each 16-word block
    # RFC doesn't say whether to use little- or big-endian; code says
    # little-endian.
    binary scan $msg i* blocks

    # loop over the message taking 16 blocks at a time

    foreach {X0 X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12 X13 X14 X15} $blocks {
	# Save A as AA, B as BB, C as CC, and D as DD.
	set AA $A
	set BB $B
	set CC $C
	set DD $D

	# Round 1.
	# Let [abcd k s i] denote the operation
	#      a = b + ((a + F(b,c,d) + X[k] + T[i]) <<< s).
	# [ABCD  0  7  1]  [DABC  1 12  2]  [CDAB  2 17  3]  [BCDA  3 22  4]
	set A [expr {$B + [<<< [expr {$A + [F $B $C $D] + $X0  + $T01}]  7]}]
	set D [expr {$A + [<<< [expr {$D + [F $A $B $C] + $X1  + $T02}] 12]}]
	set C [expr {$D + [<<< [expr {$C + [F $D $A $B] + $X2  + $T03}] 17]}]
	set B [expr {$C + [<<< [expr {$B + [F $C $D $A] + $X3  + $T04}] 22]}]
	# [ABCD  4  7  5]  [DABC  5 12  6]  [CDAB  6 17  7]  [BCDA  7 22  8]
	set A [expr {$B + [<<< [expr {$A + [F $B $C $D] + $X4  + $T05}]  7]}]
	set D [expr {$A + [<<< [expr {$D + [F $A $B $C] + $X5  + $T06}] 12]}]
	set C [expr {$D + [<<< [expr {$C + [F $D $A $B] + $X6  + $T07}] 17]}]
	set B [expr {$C + [<<< [expr {$B + [F $C $D $A] + $X7  + $T08}] 22]}]
	# [ABCD  8  7  9]  [DABC  9 12 10]  [CDAB 10 17 11]  [BCDA 11 22 12]
	set A [expr {$B + [<<< [expr {$A + [F $B $C $D] + $X8  + $T09}]  7]}]
	set D [expr {$A + [<<< [expr {$D + [F $A $B $C] + $X9  + $T10}] 12]}]
	set C [expr {$D + [<<< [expr {$C + [F $D $A $B] + $X10 + $T11}] 17]}]
	set B [expr {$C + [<<< [expr {$B + [F $C $D $A] + $X11 + $T12}] 22]}]
	# [ABCD 12  7 13]  [DABC 13 12 14]  [CDAB 14 17 15]  [BCDA 15 22 16]
	set A [expr {$B + [<<< [expr {$A + [F $B $C $D] + $X12 + $T13}]  7]}]
	set D [expr {$A + [<<< [expr {$D + [F $A $B $C] + $X13 + $T14}] 12]}]
	set C [expr {$D + [<<< [expr {$C + [F $D $A $B] + $X14 + $T15}] 17]}]
	set B [expr {$C + [<<< [expr {$B + [F $C $D $A] + $X15 + $T16}] 22]}]

	# Round 2.
	# Let [abcd k s i] denote the operation
	#      a = b + ((a + G(b,c,d) + X[k] + T[i]) <<< s).
	# Do the following 16 operations.
	# [ABCD  1  5 17]  [DABC  6  9 18]  [CDAB 11 14 19]  [BCDA  0 20 20]
	set A [expr {$B + [<<< [expr {$A + [G $B $C $D] + $X1  + $T17}]  5]}]
	set D [expr {$A + [<<< [expr {$D + [G $A $B $C] + $X6  + $T18}]  9]}]
	set C [expr {$D + [<<< [expr {$C + [G $D $A $B] + $X11 + $T19}] 14]}]
	set B [expr {$C + [<<< [expr {$B + [G $C $D $A] + $X0  + $T20}] 20]}]
	# [ABCD  5  5 21]  [DABC 10  9 22]  [CDAB 15 14 23]  [BCDA  4 20 24]
	set A [expr {$B + [<<< [expr {$A + [G $B $C $D] + $X5  + $T21}]  5]}]
	set D [expr {$A + [<<< [expr {$D + [G $A $B $C] + $X10 + $T22}]  9]}]
	set C [expr {$D + [<<< [expr {$C + [G $D $A $B] + $X15 + $T23}] 14]}]
	set B [expr {$C + [<<< [expr {$B + [G $C $D $A] + $X4  + $T24}] 20]}]
	# [ABCD  9  5 25]  [DABC 14  9 26]  [CDAB  3 14 27]  [BCDA  8 20 28]
	set A [expr {$B + [<<< [expr {$A + [G $B $C $D] + $X9  + $T25}]  5]}]
	set D [expr {$A + [<<< [expr {$D + [G $A $B $C] + $X14 + $T26}]  9]}]
	set C [expr {$D + [<<< [expr {$C + [G $D $A $B] + $X3  + $T27}] 14]}]
	set B [expr {$C + [<<< [expr {$B + [G $C $D $A] + $X8  + $T28}] 20]}]
	# [ABCD 13  5 29]  [DABC  2  9 30]  [CDAB  7 14 31]  [BCDA 12 20 32]
	set A [expr {$B + [<<< [expr {$A + [G $B $C $D] + $X13 + $T29}]  5]}]
	set D [expr {$A + [<<< [expr {$D + [G $A $B $C] + $X2  + $T30}]  9]}]
	set C [expr {$D + [<<< [expr {$C + [G $D $A $B] + $X7  + $T31}] 14]}]
	set B [expr {$C + [<<< [expr {$B + [G $C $D $A] + $X12 + $T32}] 20]}]

	# Round 3.
	# Let [abcd k s t] [sic] denote the operation
	#     a = b + ((a + H(b,c,d) + X[k] + T[i]) <<< s).
	# Do the following 16 operations.
	# [ABCD  5  4 33]  [DABC  8 11 34]  [CDAB 11 16 35]  [BCDA 14 23 36]
	set A [expr {$B + [<<< [expr {$A + [H $B $C $D] + $X5  + $T33}]  4]}]
	set D [expr {$A + [<<< [expr {$D + [H $A $B $C] + $X8  + $T34}] 11]}]
	set C [expr {$D + [<<< [expr {$C + [H $D $A $B] + $X11 + $T35}] 16]}]
	set B [expr {$C + [<<< [expr {$B + [H $C $D $A] + $X14 + $T36}] 23]}]
	# [ABCD  1  4 37]  [DABC  4 11 38]  [CDAB  7 16 39]  [BCDA 10 23 40]
	set A [expr {$B + [<<< [expr {$A + [H $B $C $D] + $X1  + $T37}]  4]}]
	set D [expr {$A + [<<< [expr {$D + [H $A $B $C] + $X4  + $T38}] 11]}]
	set C [expr {$D + [<<< [expr {$C + [H $D $A $B] + $X7  + $T39}] 16]}]
	set B [expr {$C + [<<< [expr {$B + [H $C $D $A] + $X10 + $T40}] 23]}]
	# [ABCD 13  4 41]  [DABC  0 11 42]  [CDAB  3 16 43]  [BCDA  6 23 44]
	set A [expr {$B + [<<< [expr {$A + [H $B $C $D] + $X13 + $T41}]  4]}]
	set D [expr {$A + [<<< [expr {$D + [H $A $B $C] + $X0  + $T42}] 11]}]
	set C [expr {$D + [<<< [expr {$C + [H $D $A $B] + $X3  + $T43}] 16]}]
	set B [expr {$C + [<<< [expr {$B + [H $C $D $A] + $X6  + $T44}] 23]}]
	# [ABCD  9  4 45]  [DABC 12 11 46]  [CDAB 15 16 47]  [BCDA  2 23 48]
	set A [expr {$B + [<<< [expr {$A + [H $B $C $D] + $X9  + $T45}]  4]}]
	set D [expr {$A + [<<< [expr {$D + [H $A $B $C] + $X12 + $T46}] 11]}]
	set C [expr {$D + [<<< [expr {$C + [H $D $A $B] + $X15 + $T47}] 16]}]
	set B [expr {$C + [<<< [expr {$B + [H $C $D $A] + $X2  + $T48}] 23]}]

	# Round 4.
	# Let [abcd k s t] [sic] denote the operation
	#     a = b + ((a + I(b,c,d) + X[k] + T[i]) <<< s).
	# Do the following 16 operations.
	# [ABCD  0  6 49]  [DABC  7 10 50]  [CDAB 14 15 51]  [BCDA  5 21 52]
	set A [expr {$B + [<<< [expr {$A + [I $B $C $D] + $X0  + $T49}]  6]}]
	set D [expr {$A + [<<< [expr {$D + [I $A $B $C] + $X7  + $T50}] 10]}]
	set C [expr {$D + [<<< [expr {$C + [I $D $A $B] + $X14 + $T51}] 15]}]
	set B [expr {$C + [<<< [expr {$B + [I $C $D $A] + $X5  + $T52}] 21]}]
	# [ABCD 12  6 53]  [DABC  3 10 54]  [CDAB 10 15 55]  [BCDA  1 21 56]
	set A [expr {$B + [<<< [expr {$A + [I $B $C $D] + $X12 + $T53}]  6]}]
	set D [expr {$A + [<<< [expr {$D + [I $A $B $C] + $X3  + $T54}] 10]}]
	set C [expr {$D + [<<< [expr {$C + [I $D $A $B] + $X10 + $T55}] 15]}]
	set B [expr {$C + [<<< [expr {$B + [I $C $D $A] + $X1  + $T56}] 21]}]
	# [ABCD  8  6 57]  [DABC 15 10 58]  [CDAB  6 15 59]  [BCDA 13 21 60]
	set A [expr {$B + [<<< [expr {$A + [I $B $C $D] + $X8  + $T57}]  6]}]
	set D [expr {$A + [<<< [expr {$D + [I $A $B $C] + $X15 + $T58}] 10]}]
	set C [expr {$D + [<<< [expr {$C + [I $D $A $B] + $X6  + $T59}] 15]}]
	set B [expr {$C + [<<< [expr {$B + [I $C $D $A] + $X13 + $T60}] 21]}]
	# [ABCD  4  6 61]  [DABC 11 10 62]  [CDAB  2 15 63]  [BCDA  9 21 64]
	set A [expr {$B + [<<< [expr {$A + [I $B $C $D] + $X4  + $T61}]  6]}]
	set D [expr {$A + [<<< [expr {$D + [I $A $B $C] + $X11 + $T62}] 10]}]
	set C [expr {$D + [<<< [expr {$C + [I $D $A $B] + $X2  + $T63}] 15]}]
	set B [expr {$C + [<<< [expr {$B + [I $C $D $A] + $X9  + $T64}] 21]}]

	# Then perform the following additions. (That is increment each of the
	#   four registers by the value it had before this block was started.)
	incr A $AA
	incr B $BB
	incr C $CC
	incr D $DD
    }

    ### Step 5. Output

    # ... begin with the low-order byte of A, and end with the high-order byte
    # of D.

    return [bytes $A][bytes $B][bytes $C][bytes $D]
}

### Here we inline/regsub the functions F, G, H, I and <<<

namespace eval ::md5 {
    #proc md5pure::F {x y z} {expr {(($x & $y) | ((~$x) & $z))}}
    regsub -all -- {\[ *F +(\$.) +(\$.) +(\$.) *\]} $md5body {((\1 \& \2) | ((~\1) \& \3))} md5body

    #proc md5pure::G {x y z} {expr {(($x & $z) | ($y & (~$z)))}}
    regsub -all -- {\[ *G +(\$.) +(\$.) +(\$.) *\]} $md5body {((\1 \& \3) | (\2 \& (~\3)))} md5body

    #proc md5pure::H {x y z} {expr {$x ^ $y ^ $z}}
    regsub -all -- {\[ *H +(\$.) +(\$.) +(\$.) *\]} $md5body {(\1 ^ \2 ^ \3)} md5body

    #proc md5pure::I {x y z} {expr {$y ^ ($x | (~$z))}}
    regsub -all -- {\[ *I +(\$.) +(\$.) +(\$.) *\]} $md5body {(\2 ^ (\1 | (~\3)))} md5body

    # inline <<< (bitwise left-rotate)
    regsub -all -- {\[ *<<< +\[ *expr +({[^\}]*})\] +([0-9]+) *\]} $md5body {(([set x [expr \1]] << \2) |  (($x >> R\2) \& S\2))} md5body

    # now replace the R and S
    variable map {}
    variable i
    foreach i {
	7 12 17 22
	5  9 14 20
	4 11 16 23
	6 10 15 21
    } {
	lappend map R$i [expr {32 - $i}] S$i [expr {0x7fffffff >> (31-$i)}]
    }

    # inline the values of T
    variable tName
    variable tVal
    foreach tName {
	T01 T02 T03 T04 T05 T06 T07 T08 T09 T10
	T11 T12 T13 T14 T15 T16 T17 T18 T19 T20
	T21 T22 T23 T24 T25 T26 T27 T28 T29 T30
	T31 T32 T33 T34 T35 T36 T37 T38 T39 T40
	T41 T42 T43 T44 T45 T46 T47 T48 T49 T50
	T51 T52 T53 T54 T55 T56 T57 T58 T59 T60
	T61 T62 T63 T64
    } tVal {
	0xd76aa478 0xe8c7b756 0x242070db 0xc1bdceee
	0xf57c0faf 0x4787c62a 0xa8304613 0xfd469501
	0x698098d8 0x8b44f7af 0xffff5bb1 0x895cd7be
	0x6b901122 0xfd987193 0xa679438e 0x49b40821

	0xf61e2562 0xc040b340 0x265e5a51 0xe9b6c7aa
	0xd62f105d 0x2441453  0xd8a1e681 0xe7d3fbc8
	0x21e1cde6 0xc33707d6 0xf4d50d87 0x455a14ed
	0xa9e3e905 0xfcefa3f8 0x676f02d9 0x8d2a4c8a

	0xfffa3942 0x8771f681 0x6d9d6122 0xfde5380c
	0xa4beea44 0x4bdecfa9 0xf6bb4b60 0xbebfbc70
	0x289b7ec6 0xeaa127fa 0xd4ef3085 0x4881d05
	0xd9d4d039 0xe6db99e5 0x1fa27cf8 0xc4ac5665

	0xf4292244 0x432aff97 0xab9423a7 0xfc93a039
	0x655b59c3 0x8f0ccc92 0xffeff47d 0x85845dd1
	0x6fa87e4f 0xfe2ce6e0 0xa3014314 0x4e0811a1
	0xf7537e82 0xbd3af235 0x2ad7d2bb 0xeb86d391
    } {
	lappend map \$$tName $tVal
    }
    set md5body [string map $map $md5body]

    # Finally, define the proc
    proc md5 {msg} $md5body

    # unset auxiliary variables
    unset md5body tName tVal map

    proc byte0 {i} {expr {0xff & $i}}
    proc byte1 {i} {expr {(0xff00 & $i) >> 8}}
    proc byte2 {i} {expr {(0xff0000 & $i) >> 16}}
    proc byte3 {i} {expr {((0xff000000 & $i) >> 24) & 0xff}}
    proc bytes {i} {
        format %0.2x%0.2x%0.2x%0.2x [byte0 $i] [byte1 $i] [byte2 $i] [byte3 $i]
    }
}
```

Demonstration code:

```tcl
foreach {hash <- string} {
   0xd41d8cd98f00b204e9800998ecf8427e ==> ""
   0x0cc175b9c0f1b6a831c399e269772661 ==> "a"
   0x900150983cd24fb0d6963f7d28e17f72 ==> "abc"
   0xf96b697d7cb7938d525a2f31aaf161d0 ==> "message digest"
   0xc3fcd3d76192e4007dfb496cca67e13b ==> "abcdefghijklmnopqrstuvwxyz"
   0xd174ab98d277d9f5a5611c2c9f419d9f ==> "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
   0x57edf4a22be3c955ac49da2e2107b67a ==> "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
} {
    puts "“$string” -> [md5::md5 $string] (officially: $hash)"
}
```



## x86 Assembly

Uses DOS interrupts for display.


```asm
section .text
org 0x100
	mov	di, md5_for_display
	mov	si, test_input_1
	mov	cx, test_input_1_len
	call	compute_md5
	call	display_md5
	mov	si, test_input_2
	mov	cx, test_input_2_len
	call	compute_md5
	call	display_md5
	mov	si, test_input_3
	mov	cx, test_input_3_len
	call	compute_md5
	call	display_md5
	mov	si, test_input_4
	mov	cx, test_input_4_len
	call	compute_md5
	call	display_md5
	mov	si, test_input_5
	mov	cx, test_input_5_len
	call	compute_md5
	call	display_md5
	mov	si, test_input_6
	mov	cx, test_input_6_len
	call	compute_md5
	call	display_md5
	mov	si, test_input_7
	mov	cx, test_input_7_len
	call	compute_md5
	call	display_md5
	mov	ax, 0x4c00
	int	21h

md5_for_display times 16 db 0
HEX_CHARS db '0123456789ABCDEF'

display_md5:
	mov	ah, 9
	mov	dx, display_str_1
	int	0x21
	push	cx
	push	si
	mov	cx, 16
	mov	si, di
	xor	bx, bx
.loop:
	lodsb
	mov	bl, al
	and	bl, 0x0F
	push	bx
	mov	bl, al
	shr	bx, 4
	mov	ah, 2
	mov	dl, [HEX_CHARS + bx]
	int	0x21
	pop	bx
	mov	dl, [HEX_CHARS + bx]
	int	0x21
	dec	cx
	jnz	.loop
	mov	ah, 9
	mov	dx, display_str_2
	int	0x21
	pop	si
	pop	cx
	test	cx, cx
	jz	do_newline
	mov	ah, 2
display_string:
	lodsb
	mov	dl, al
	int	0x21
	dec	cx
	jnz	display_string
do_newline:
	mov	ah, 9
	mov	dx, display_str_3
	int	0x21
	ret;

compute_md5:
	; si --> input bytes, cx = input len, di --> 16-byte output buffer
	; assumes all in the same segment
	cld
	pusha
	push	di
	push	si
	mov	[message_len], cx

	mov	bx, cx
	shr	bx, 6
	mov	[ending_bytes_block_num], bx
	mov	[num_blocks], bx
	inc	word [num_blocks]
	shl	bx, 6
	add	si, bx
	and	cx, 0x3f
	push	cx
	mov	di, ending_bytes
	rep	movsb
	mov	al, 0x80
	stosb
	pop	cx
	sub	cx, 55
	neg	cx
	jge	add_padding
	add	cx, 64
	inc	word [num_blocks]
add_padding:
	mov	al, 0
	rep	stosb
	xor	eax, eax
	mov	ax, [message_len]
	shl	eax, 3
	mov	cx, 8
store_message_len:
	stosb
	shr	eax, 8
	dec	cx
	jnz	store_message_len
	pop	si
	mov	[md5_a], dword INIT_A
	mov	[md5_b], dword INIT_B
	mov	[md5_c], dword INIT_C
	mov	[md5_d], dword INIT_D
block_loop:
	push	cx
	cmp	cx, [ending_bytes_block_num]
	jne	backup_abcd
	; switch buffers if towards the end where padding needed
	mov	si, ending_bytes
backup_abcd:
	push	dword [md5_d]
	push	dword [md5_c]
	push	dword [md5_b]
	push	dword [md5_a]
	xor	cx, cx
	xor	eax, eax
main_loop:
	push	cx
	mov	ax, cx
	shr	ax, 4
	test	al, al
	jz	pass0
	cmp	al, 1
	je	pass1
	cmp	al, 2
	je	pass2
	; pass3
	mov	eax, [md5_c]
	mov	ebx, [md5_d]
	not	ebx
	or	ebx, [md5_b]
	xor	eax, ebx
	jmp	do_rotate

pass0:
	mov	eax, [md5_b]
	mov	ebx, eax
	and	eax, [md5_c]
	not	ebx
	and	ebx, [md5_d]
	or	eax, ebx
	jmp	do_rotate

pass1:
	mov	eax, [md5_d]
	mov	edx, eax
	and	eax, [md5_b]
	not	edx
	and	edx, [md5_c]
	or	eax, edx
	jmp	do_rotate

pass2:
	mov	eax, [md5_b]
	xor	eax, [md5_c]
	xor	eax, [md5_d]
do_rotate:
	add	eax, [md5_a]
	mov	bx, cx
	shl	bx, 1
	mov	bx, [BUFFER_INDEX_TABLE + bx]
	add	eax, [si + bx]
	mov	bx, cx
	shl	bx, 2
	add	eax, dword [TABLE_T + bx]
	mov	bx, cx
	ror	bx, 2
	shr	bl, 2
	rol	bx, 2
	mov	cl, [SHIFT_AMTS + bx]
	rol	eax, cl
	add	eax, [md5_b]
	push	eax
	push	dword [md5_b]
	push	dword [md5_c]
	push	dword [md5_d]
	pop	dword [md5_a]
	pop	dword [md5_d]
	pop	dword [md5_c]
	pop	dword [md5_b]
	pop	cx
	inc	cx
	cmp	cx, 64
	jb	main_loop
	; add to original values
	pop	eax
	add	[md5_a], eax
	pop	eax
	add	[md5_b], eax
	pop	eax
	add	[md5_c], eax
	pop	eax
	add	[md5_d], eax
	; advance pointers
	add	si, 64
	pop	cx
	inc	cx
	cmp	cx, [num_blocks]
	jne	block_loop
	mov	cx, 4
	mov	si, md5_a
	pop	di
	rep	movsd
	popa
	ret

section .data

INIT_A equ 0x67452301
INIT_B equ 0xEFCDAB89
INIT_C equ 0x98BADCFE
INIT_D equ 0x10325476

SHIFT_AMTS db 7, 12, 17, 22, 5,  9, 14, 20, 4, 11, 16, 23, 6, 10, 15, 21

TABLE_T dd 0xD76AA478, 0xE8C7B756, 0x242070DB, 0xC1BDCEEE, 0xF57C0FAF, 0x4787C62A, 0xA8304613, 0xFD469501, 0x698098D8, 0x8B44F7AF, 0xFFFF5BB1, 0x895CD7BE, 0x6B901122, 0xFD987193, 0xA679438E, 0x49B40821, 0xF61E2562, 0xC040B340, 0x265E5A51, 0xE9B6C7AA, 0xD62F105D, 0x02441453, 0xD8A1E681, 0xE7D3FBC8, 0x21E1CDE6, 0xC33707D6, 0xF4D50D87, 0x455A14ED, 0xA9E3E905, 0xFCEFA3F8, 0x676F02D9, 0x8D2A4C8A, 0xFFFA3942, 0x8771F681, 0x6D9D6122, 0xFDE5380C, 0xA4BEEA44, 0x4BDECFA9, 0xF6BB4B60, 0xBEBFBC70, 0x289B7EC6, 0xEAA127FA, 0xD4EF3085, 0x04881D05, 0xD9D4D039, 0xE6DB99E5, 0x1FA27CF8, 0xC4AC5665, 0xF4292244, 0x432AFF97, 0xAB9423A7, 0xFC93A039, 0x655B59C3, 0x8F0CCC92, 0xFFEFF47D, 0x85845DD1, 0x6FA87E4F, 0xFE2CE6E0, 0xA3014314, 0x4E0811A1, 0xF7537E82, 0xBD3AF235, 0x2AD7D2BB, 0xEB86D391
BUFFER_INDEX_TABLE dw 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 4, 24, 44, 0, 20, 40, 60, 16, 36, 56, 12, 32, 52, 8, 28, 48, 20, 32, 44, 56, 4, 16, 28, 40, 52, 0, 12, 24, 36, 48, 60, 8, 0, 28, 56, 20, 48, 12, 40, 4, 32, 60, 24, 52, 16, 44, 8, 36
ending_bytes_block_num dw 0
ending_bytes times 128 db 0
message_len dw 0
num_blocks dw 0
md5_a dd 0
md5_b dd 0
md5_c dd 0
md5_d dd 0

display_str_1 db '0x$'
display_str_2 db ' <== "$'
display_str_3 db '"', 13, 10, '$'

test_input_1:
test_input_1_len equ $ - test_input_1
test_input_2 db 'a'
test_input_2_len equ $ - test_input_2
test_input_3 db 'abc'
test_input_3_len equ $ - test_input_3
test_input_4 db 'message digest'
test_input_4_len equ $ - test_input_4
test_input_5 db 'abcdefghijklmnopqrstuvwxyz'
test_input_5_len equ $ - test_input_5
test_input_6 db 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'
test_input_6_len equ $ - test_input_6
test_input_7 db '12345678901234567890123456789012345678901234567890123456789012345678901234567890'
test_input_7_len equ $ - test_input_7
```


'''Output:'''

```txt
0xD41D8CD98F00B204E9800998ECF8427E <== ""
0x0CC175B9C0F1B6A831C399E269772661 <== "a"
0x900150983CD24FB0D6963F7D28E17F72 <== "abc"
0xF96B697D7CB7938D525A2F31AAF161D0 <== "message digest"
0xC3FCD3D76192E4007DFB496CCA67E13B <== "abcdefghijklmnopqrstuvwxyz"
0xD174AB98D277D9F5A5611C2C9F419D9F <== "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
0x57EDF4A22BE3C955AC49DA2E2107B67A <== "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
```


