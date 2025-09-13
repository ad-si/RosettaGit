+++
title = "UTF-8 encode and decode"
description = ""
date = 2019-07-05T14:30:22Z
aliases = []
[extra]
id = 21332
[taxonomies]
categories = ["task"]
tags = []
+++

As described in [[UTF-8]] and in [[wp:UTF-8|Wikipedia]], UTF-8 is a popular encoding of (multi-byte) [[Unicode]] code-points into eight-bit octets.

The goal of this task is to write a encoder that takes a unicode code-point (an integer representing a unicode character) and returns a sequence of 1-4 bytes representing that character in the UTF-8 encoding. 

Then you have to write the corresponding decoder that takes a sequence of 1-4 UTF-8 encoded bytes and return the corresponding unicode character.

Demonstrate the functionality of your encoder and decoder on the following five characters:


```txt

Character   Name                                  Unicode    UTF-8 encoding (hex)
---------------------------------------------------------------------------------
A           LATIN CAPITAL LETTER A                U+0041     41
ö           LATIN SMALL LETTER O WITH DIAERESIS   U+00F6     C3 B6
Ж           CYRILLIC CAPITAL LETTER ZHE           U+0416     D0 96
€           EURO SIGN                             U+20AC     E2 82 AC
𝄞           MUSICAL SYMBOL G CLEF                 U+1D11E    F0 9D 84 9E

```


Provided below is a reference implementation in Common Lisp.


## Common Lisp


Helper functions


```lisp

(defun ascii-byte-p (octet)
  "Return t if octet is a single-byte 7-bit ASCII char.
  The most significant bit is 0, so the allowed pattern is 0xxx xxxx."
  (assert (typep octet 'integer))
  (assert (<= (integer-length octet) 8))
  (let ((bitmask  #b10000000)
        (template #b00000000))
    ;; bitwise and the with the bitmask #b11000000 to extract the first two bits.
    ;; check if the first two bits are equal to the template #b10000000.
    (= (logand bitmask octet) template)))

(defun multi-byte-p (octet)
  "Return t if octet is a part of a multi-byte UTF-8 sequence.
  The multibyte pattern is 1xxx xxxx. A multi-byte can be either a lead byte or a trail byte."
  (assert (typep octet 'integer))
  (assert (<= (integer-length octet) 8))
  (let ((bitmask  #b10000000)
        (template #b10000000))
    ;; bitwise and the with the bitmask #b11000000 to extract the first two bits.
    ;; check if the first two bits are equal to the template #b10000000.
    (= (logand bitmask octet) template)))

(defun lead-byte-p (octet)
  "Return t if octet is one of the leading bytes of an UTF-8 sequence, nil otherwise.
  Allowed leading byte patterns are 0xxx xxxx, 110x xxxx, 1110 xxxx and 1111 0xxx."
  (assert (typep octet 'integer))
  (assert (<= (integer-length octet) 8))
  (let ((bitmasks  (list #b10000000 #b11100000 #b11110000 #b11111000))
        (templates (list #b00000000 #b11000000 #b11100000 #b11110000)))
    (some #'(lambda (a b) (= (logand a octet) b)) bitmasks templates)))

(defun n-trail-bytes (octet)
  "Take a leading utf-8 byte, return the number of continuation bytes 1-3."
  (assert (typep octet 'integer))
  (assert (<= (integer-length octet) 8))
  (let ((bitmasks  (list #b10000000 #b11100000 #b11110000 #b11111000))
        (templates (list #b00000000 #b11000000 #b11100000 #b11110000)))
    (loop for i from 0 to 3
       when (= (nth i templates) (logand (nth i bitmasks) octet))
       return i)))

```


Encoder


```lisp

(defun unicode-to-utf-8 (int)
  "Take a unicode code point, return a list of one to four UTF-8 encoded bytes (octets)."
  (assert (<= (integer-length int) 21))
  (let ((n-trail-bytes (cond ((<= #x00000 int #x00007F) 0)
                             ((<= #x00080 int #x0007FF) 1)
                             ((<= #x00800 int #x00FFFF) 2)
                             ((<= #x10000 int #x10FFFF) 3)))
        (lead-templates (list #b00000000 #b11000000 #b11100000 #b11110000))
        (trail-template #b10000000)
        ;; number of content bits in the lead byte.
        (n-lead-bits (list 7 5 4 3))
        ;; number of content bits in the trail byte.
        (n-trail-bits 6)
        ;; list to put the UTF-8 encoded bytes in.
        (byte-list nil))
    (if (= n-trail-bytes 0)
        ;; if we need 0 trail bytes, ist just an ascii single byte.
        (push int byte-list)
        (progn
          ;; if we need more than one byte, first fill the trail bytes with 6 bits each.
          (loop for i from 0 to (1- n-trail-bytes)
             do (push (+ trail-template
                         (ldb (byte n-trail-bits (* i n-trail-bits)) int))
                      byte-list))
          ;; then copy the remaining content bytes to the lead byte.
          (push (+ (nth n-trail-bytes lead-templates)
                   (ldb (byte (nth n-trail-bytes n-lead-bits) (* n-trail-bytes n-trail-bits)) int))
                byte-list)))
    ;; return the list of UTF-8 encoded bytes.
    byte-list))

```


Decoder


```lisp

(defun utf-8-to-unicode (byte-list)
  "Take a list of one to four utf-8 encoded bytes (octets), return a code point."
  (let ((b1 (car byte-list)))
    (cond ((ascii-byte-p b1) b1) ; if a single byte, just return it.
          ((multi-byte-p b1)
           (if (lead-byte-p b1)
               (let ((n (n-trail-bytes b1))
                     ;; Content bits we want to extract from each lead byte.
                     (lead-templates (list #b01111111 #b00011111 #b00001111 #b00000111))
                     ;; Content bits we want to extract from each trail byte.
                     (trail-template #b00111111))
                 (if (= n (1- (list-length byte-list)))
                     ;; add lead byte
                     (+ (ash (logand (nth 0 byte-list) (nth n lead-templates)) (* 6 n))
                        ;; and the trail bytes
                        (loop for i from 1 to n sum
                             (ash (logand (nth i byte-list) trail-template) (* 6 (- n i)))))
                     (error "calculated number of bytes doesnt match the length of the byte list")))
               (error "first byte in the list isnt a lead byte"))))))

```


The test


```lisp

(defun test-utf-8 ()
  "Return t if the chosen unicode points are encoded and decoded correctly."
  (let* ((unicodes-orig (list 65 246 1046 8364 119070))
         (unicodes-test (mapcar #'(lambda (x) (utf-8-to-unicode (unicode-to-utf-8 x)))
                                unicodes-orig)))
    (mapcar #'(lambda (x)
                (format t
                        "character ~A, code point: ~6x, utf-8: ~{~x ~}~%"
                        (code-char x)
                        x
                        (unicode-to-utf-8 x)))
            unicodes-orig)
    ;; return t if all are t
    (every #'= unicodes-orig unicodes-test)))

```


Test output


```lisp

CL-USER> (test-utf-8)
character A, code point:     41, utf-8: 41 
character ö, code point:     F6, utf-8: C3 B6 
character Ж, code point:    416, utf-8: D0 96 
character €, code point:   20AC, utf-8: E2 82 AC 
character 𝄞, code point:  1D11E, utf-8: F0 9D 84 9E 
T

```




## Ada

```Ada
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;

procedure UTF8_Encode_And_Decode
is
   package TIO renames Ada.Text_IO;
   package WWTIO renames Ada.Wide_Wide_Text_IO;
   package WWS renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

   function To_Hex
     (i : in Integer;
      width : in Natural := 0;
      fill : in Character := '0') return String
   is
      holder : String(1 .. 20);
   begin
      Ada.Integer_Text_IO.Put(holder, i, 16);
      declare
         hex : constant String := holder(Index(holder, "#")+1 .. holder'Last-1);
         filled : String := Natural'Max(width, hex'Length) * fill;
      begin
         filled(filled'Last - hex'Length + 1 .. filled'Last) := hex;
         return filled;
      end;
   end To_Hex;

   input : constant Wide_Wide_String := "AöЖ€𝄞";
begin
   TIO.Put_Line("Character   Unicode    UTF-8 encoding (hex)");
   TIO.Put_Line(43 * '-');
   for WWC of input loop
      WWTIO.Put(WWC & "           ");
      declare
         filled : String := 11 * ' ';
         unicode : constant String := "U+" & To_Hex(Wide_Wide_Character'Pos(WWC), width => 4);
         utf8_string : constant String := WWS.Encode((1 => WWC));
      begin
         filled(filled'First .. filled'First + unicode'Length - 1) := unicode;
         TIO.Put(filled);
         for C of utf8_string loop
            TIO.Put(To_Hex(Character'Pos(C)) & " ");
         end loop;
         TIO.New_Line;
      end;
   end loop;
end UTF8_Encode_And_Decode;
```

```txt
Character   Unicode    UTF-8 encoding (hex)
-------------------------------------------
A           U+0041     41 
ö           U+00F6     C3 B6 
Ж           U+0416     D0 96 
€           U+20AC     E2 82 AC 
𝄞           U+1D11E    F0 9D 84 9E 

```



## C


```C

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

typedef struct {
	char mask;    /* char data will be bitwise AND with this */
	char lead;    /* start bytes of current char in utf-8 encoded character */
	uint32_t beg; /* beginning of codepoint range */
	uint32_t end; /* end of codepoint range */
	int bits_stored; /* the number of bits from the codepoint that fits in char */
}utf_t;

utf_t * utf[] = {
	/*             mask        lead        beg      end       bits */
	[0] = &(utf_t){0b00111111, 0b10000000, 0,       0,        6    },
	[1] = &(utf_t){0b01111111, 0b00000000, 0000,    0177,     7    },
	[2] = &(utf_t){0b00011111, 0b11000000, 0200,    03777,    5    },
	[3] = &(utf_t){0b00001111, 0b11100000, 04000,   0177777,  4    },
	[4] = &(utf_t){0b00000111, 0b11110000, 0200000, 04177777, 3    },
	      &(utf_t){0},
};

/* All lengths are in bytes */
int codepoint_len(const uint32_t cp); /* len of associated utf-8 char */
int utf8_len(const char ch);          /* len of utf-8 encoded char */

char *to_utf8(const uint32_t cp);
uint32_t to_cp(const char chr[4]);

int codepoint_len(const uint32_t cp)
{
	int len = 0;
	for(utf_t **u = utf; *u; ++u) {
		if((cp >= (*u)->beg) && (cp <= (*u)->end)) {
			break;
		}
		++len;
	}
	if(len > 4) /* Out of bounds */
		exit(1);

	return len;
}

int utf8_len(const char ch)
{
	int len = 0;
	for(utf_t **u = utf; *u; ++u) {
		if((ch & ~(*u)->mask) == (*u)->lead) {
			break;
		}
		++len;
	}
	if(len > 4) { /* Malformed leading byte */
		exit(1);
	}
	return len;
}

char *to_utf8(const uint32_t cp)
{
	static char ret[5];
	const int bytes = codepoint_len(cp);

	int shift = utf[0]->bits_stored * (bytes - 1);
	ret[0] = (cp >> shift & utf[bytes]->mask) | utf[bytes]->lead;
	shift -= utf[0]->bits_stored;
	for(int i = 1; i < bytes; ++i) {
		ret[i] = (cp >> shift & utf[0]->mask) | utf[0]->lead;
		shift -= utf[0]->bits_stored;
	}
	ret[bytes] = '\0';
	return ret;
}

uint32_t to_cp(const char chr[4])
{
	int bytes = utf8_len(*chr);
	int shift = utf[0]->bits_stored * (bytes - 1);
	uint32_t codep = (*chr++ & utf[bytes]->mask) << shift;

	for(int i = 1; i < bytes; ++i, ++chr) {
		shift -= utf[0]->bits_stored;
		codep |= ((char)*chr & utf[0]->mask) << shift;
	}

	return codep;
}

int main(void)
{
	const uint32_t *in, input[] = {0x0041, 0x00f6, 0x0416, 0x20ac, 0x1d11e, 0x0};

	printf("Character  Unicode  UTF-8 encoding (hex)\n");
	printf("----------------------------------------\n");

	char *utf8;
	uint32_t codepoint;
	for(in = input; *in; ++in) {
		utf8 = to_utf8(*in);
		codepoint = to_cp(utf8);
		printf("%s          U+%-7.4x", utf8, codepoint);

		for(int i = 0; utf8[i] && i < 4; ++i) {
			printf("%hhx ", utf8[i]);
		}
		printf("\n");
	}
	return 0;
}

```

Output
<lang>
Character  Unicode  UTF-8 encoding (hex)
----------------------------------------
A          U+0041   41
ö          U+00f6   c3 b6
Ж          U+0416   d0 96
€          U+20ac   e2 82 ac
𝄞          U+1d11e  f0 9d 84 9e


```



## D


```D
import std.conv;
import std.stdio;

immutable CHARS = ["A","ö","Ж","€","𝄞"];

void main() {
    writeln("Character   Code-Point   Code-Units");
    foreach (c; CHARS) {
        auto bytes = cast(ubyte[]) c; //The raw bytes of a character can be accessed by casting
        auto unicode = cast(uint) to!dstring(c)[0]; //Convert from a UTF8 string to a UTF32 string, and cast the first character to a number
        writefln("%s              %7X   [%(%X, %)]", c, unicode, bytes);
    }
}
```


```txt
Character   Code-Point   Code-Units
A                   41   [41]
ö                   F6   [C3, B6]
Ж                  416   [D0, 96]
€                 20AC   [E2, 82, AC]
𝄞                1D11E   [F0, 9D, 84, 9E]
```


## Elena

ELENA 4.x :

```elena
import system'routines;
import extensions;
 
extension op : String
{
    string printAsString()
    {
       console.print(self," ")
    }
 
    string printAsUTF8Array()
    {
        self.toByteArray().forEach:(b){ console.print(b.toString(16)," ") }
    }
 
    string printAsUTF32()
    {
        self.toArray().forEach:(c){ console.print("U+",c.toInt().toString(16)," ") }
    }
}
 
public program()
{
    "A".printAsString().printAsUTF8Array().printAsUTF32();
    console.printLine();
 
    "ö".printAsString().printAsUTF8Array().printAsUTF32();
    console.printLine();
 
    "Ж".printAsString().printAsUTF8Array().printAsUTF32();
    console.printLine();
 
    "€".printAsString().printAsUTF8Array().printAsUTF32();
    console.printLine();
 
    "𝄞".printAsString().printAsUTF8Array().printAsUTF32();
    console.printLine();
}
```

```txt

A 41 U+41 
ö C3 B6 U+F6 
Ж D0 96 U+416 
€ E2 82 AC U+20AC 
𝄞 F0 9D 84 9E U+1D11E
```


=={{header|F_Sharp|F#}}==

```fsharp

// Unicode character point to UTF8. Nigel Galloway: March 19th., 2018
let fN g = match List.findIndex (fun n->n>g) [0x80;0x800;0x10000;0x110000] with
           |0->[g]
           |1->[0xc0+(g&&&0x7c0>>>6);0x80+(g&&&0x3f)]
           |2->[0xe0+(g&&&0xf000>>>12);0x80+(g&&&0xfc0>>>6);0x80+(g&&&0x3f)]
           |_->[0xf0+(g&&&0x1c0000>>>18);0x80+(g&&&0x3f000>>>12);0x80+(g&&&0xfc0>>>6);0x80+(g&&&0x3f)]

```

```txt

for n in fN 0x41    do printf "%x " n -> 41
for n in fN 0xf6    do printf "%x " n -> c3 b6 
for n in fN 0x416   do printf "%x " n -> d0 96 
for n in fN 0x20ac  do printf "%x " n -> e2 82 ac 
for n in fN 0x1d11e do printf "%x " n -> f0 9d 84 9e 

```


## Go


### Implementation

This implementation is missing all checks for invalid data and so is not production-ready, but illustrates the basic UTF-8 encoding scheme.

```go
package main

import (
    "bytes"
    "encoding/hex"
    "fmt"
    "log"
    "strings"
)

var testCases = []struct {
    rune
    string
}{
    {'A', "41"},
    {'ö', "C3 B6"},
    {'Ж', "D0 96"},
    {'€', "E2 82 AC"},
    {'𝄞', "F0 9D 84 9E"},
}

func main() {
    for _, tc := range testCases {
        // derive some things from test data
        u := fmt.Sprintf("U+%04X", tc.rune)
        b, err := hex.DecodeString(strings.Replace(tc.string, " ", "", -1))
        if err != nil {
            log.Fatal("bad test data")
        }
        // exercise encoder and decoder on test data
        e := encodeUTF8(tc.rune)
        d := decodeUTF8(b)
        // show function return values
        fmt.Printf("%c  %-7s  %X\n", d, u, e)
        // validate return values against test data
        if !bytes.Equal(e, b) {
            log.Fatal("encodeUTF8 wrong")
        }
        if d != tc.rune {
            log.Fatal("decodeUTF8 wrong")
        }
    }
}

const (
    // first byte of a 2-byte encoding starts 110 and carries 5 bits of data
    b2Lead = 0xC0 // 1100 0000
    b2Mask = 0x1F // 0001 1111

    // first byte of a 3-byte encoding starts 1110 and carries 4 bits of data
    b3Lead = 0xE0 // 1110 0000
    b3Mask = 0x0F // 0000 1111

    // first byte of a 4-byte encoding starts 11110 and carries 3 bits of data
    b4Lead = 0xF0 // 1111 0000
    b4Mask = 0x07 // 0000 0111

    // non-first bytes start 10 and carry 6 bits of data
    mbLead = 0x80 // 1000 0000
    mbMask = 0x3F // 0011 1111
)

func encodeUTF8(r rune) []byte {
    switch i := uint32(r); {
    case i <= 1<<7-1: // max code point that encodes into a single byte
        return []byte{byte(r)}
    case i <= 1<<11-1: // into two bytes
        return []byte{
            b2Lead | byte(r>>6),
            mbLead | byte(r)&mbMask}
    case i <= 1<<16-1: // three
        return []byte{
            b3Lead | byte(r>>12),
            mbLead | byte(r>>6)&mbMask,
            mbLead | byte(r)&mbMask}
    default:
        return []byte{
            b4Lead | byte(r>>18),
            mbLead | byte(r>>12)&mbMask,
            mbLead | byte(r>>6)&mbMask,
            mbLead | byte(r)&mbMask}
    }
}

func decodeUTF8(b []byte) rune {
    switch b0 := b[0]; {
    case b0 < 0x80:
        return rune(b0)
    case b0 < 0xE0:
        return rune(b0&b2Mask)<<6 |
            rune(b[1]&mbMask)
    case b0 < 0xF0:
        return rune(b0&b3Mask)<<12 |
            rune(b[1]&mbMask)<<6 |
            rune(b[2]&mbMask)
    default:
        return rune(b0&b4Mask)<<18 |
            rune(b[1]&mbMask)<<12 |
            rune(b[2]&mbMask)<<6 |
            rune(b[3]&mbMask)
    }
}
```

```txt

A  U+0041   41
ö  U+00F6   C3B6
Ж  U+0416   D096
€  U+20AC   E282AC
𝄞  U+1D11E  F09D849E

```



### Library/language


```go
package main

import (
    "fmt"
    "unicode/utf8"
)

func utf8encode(codepoint rune) []byte {
    buffer := make([]byte, 4)
    length := utf8.EncodeRune(buffer, codepoint)
    return buffer[:length]
}

func utf8decode(bytes []byte) rune {
    result, _ := utf8.DecodeRune(bytes)
    return result
}

func main() {
        fmt.Printf("%-7s %7s\t%s\t%s\n", "Char", "Unicode", "UTF-8 encoded", "Decoded");
    for _, codepoint := range []rune{'A', 'ö', 'Ж', '€', '𝄞'} {
        encoded := utf8encode(codepoint)
        decoded := utf8decode(encoded)
        fmt.Printf("%-7c U+%04X\t%-12X\t%c\n", codepoint, codepoint, encoded, decoded)
    }
}
```

```txt

Char    Unicode	UTF-8 encoded	Decoded
A       U+0041	41          	A
ö       U+00F6	C3B6        	ö
Ж       U+0416	D096        	Ж
€       U+20AC	E282AC      	€
𝄞       U+1D11E	F09D849E    	𝄞

```


Alternately:

```go
package main

import (
    "fmt"
)

func utf8encode(codepoint rune) []byte {
    return []byte(string([]rune{codepoint}))
}

func utf8decode(bytes []byte) rune {
    return []rune(string(bytes))[0]
}

func main() {
        fmt.Printf("%-7s %7s\t%s\t%s\n", "Char", "Unicode", "UTF-8 encoded", "Decoded");
    for _, codepoint := range []rune{'A', 'ö', 'Ж', '€', '𝄞'} {
        encoded := utf8encode(codepoint)
        decoded := utf8decode(encoded)
        fmt.Printf("%-7c U+%04X\t%-12X\t%c\n", codepoint, codepoint, encoded, decoded)
    }
}
```

```txt

Char    Unicode	UTF-8 encoded	Decoded
A       U+0041	41          	A
ö       U+00F6	C3B6        	ö
Ж       U+0416	D096        	Ж
€       U+20AC	E282AC      	€
𝄞       U+1D11E	F09D849E    	𝄞

```



## Groovy

```groovy
import java.nio.charset.StandardCharsets

class UTF8EncodeDecode {
    static byte[] utf8encode(int codePoint) {
        char[] characters = [codePoint]
        new String(characters, 0, 1).getBytes StandardCharsets.UTF_8
    }

    static int utf8decode(byte[] bytes) {
        new String(bytes, StandardCharsets.UTF_8).codePointAt(0)
    }

    static void main(String[] args) {
        printf "%-7s %-43s %7s\t%s\t%7s%n", "Char", "Name", "Unicode", "UTF-8 encoded", "Decoded"

        ([0x0041, 0x00F6, 0x0416, 0x20AC, 0x1D11E]).each { int codePoint ->
            byte[] encoded = utf8encode codePoint
            Formatter formatter = new Formatter()
            encoded.each { byte b ->
                formatter.format "%02X ", b
            }
            String encodedHex = formatter.toString()
            int decoded = utf8decode encoded
            printf "%-7c %-43s U+%04X\t%-12s\tU+%04X%n", codePoint, Character.getName(codePoint), codePoint, encodedHex, decoded
        }
    }
}
```

```txt
Char    Name                                        Unicode	UTF-8 encoded	Decoded
A       LATIN CAPITAL LETTER A                      U+0041	41          	U+0041
ö       LATIN SMALL LETTER O WITH DIAERESIS         U+00F6	C3 B6       	U+00F6
Ж       CYRILLIC CAPITAL LETTER ZHE                 U+0416	D0 96       	U+0416
€       EURO SIGN                                   U+20AC	E2 82 AC    	U+20AC
𝄞      MUSICAL SYMBOL G CLEF                       U+1D11E	ED 84 9E    	U+D11E
```



## Haskell


Example makes use of [http://hackage.haskell.org/package/bytestring <tt>bytestring</tt>] and [http://hackage.haskell.org/package/text <tt>text</tt>] packages:


```haskell
module Main (main) where
 
import qualified Data.ByteString as ByteString (pack, unpack)
import           Data.Char (chr, ord)
import           Data.Foldable (for_)
import           Data.List (intercalate)
import qualified Data.Text as Text (head, singleton)
import qualified Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)
import           Text.Printf (printf)
 
encodeCodepoint :: Int -> [Int]
encodeCodepoint = map fromIntegral . ByteString.unpack . Text.encodeUtf8 . Text.singleton . chr

decodeToCodepoint :: [Int] -> Int
decodeToCodepoint = ord . Text.head . Text.decodeUtf8 . ByteString.pack . map fromIntegral

main :: IO ()
main = do
    putStrLn "Character  Unicode  UTF-8 encoding (hex)  Decoded"
    putStrLn "-------------------------------------------------"
    for_ [0x0041, 0x00F6, 0x0416, 0x20AC, 0x1D11E] $ \codepoint -> do
        let values = encodeCodepoint codepoint
            codepoint' = decodeToCodepoint values
        putStrLn $ printf "%c          %-7s  %-20s  %c"
            codepoint
            (printf "U+%04X" codepoint :: String)
            (intercalate " " (map (printf "%02X") values))
            codepoint'
```

```txt

Character  Unicode  UTF-8 encoding (hex)  Decoded
-------------------------------------------------
A          U+0041   41                    A
ö          U+00F6   C3 B6                 ö
Ж          U+0416   D0 96                 Ж
€          U+20AC   E2 82 AC              €
𝄞          U+1D11E  F0 9D 84 9E           𝄞

```



## J

'''Solution:'''

```j
utf8=: 8&u:               NB. converts to UTF-8 from unicode or unicode codepoint integer
ucp=: 9&u:                NB. converts to unicode from UTF-8 or unicode codepoint integer
ucp_hex=: hfd@(3 u: ucp)  NB. converts to unicode codepoint hexadecimal from UTF-8, unicode or unicode codepoint integer
```


'''Examples:'''

```j
   utf8 65 246 1046 8364 119070
AöЖ€𝄞
   ucp 65 246 1046 8364 119070
AöЖ€𝄞
   ucp 'AöЖ€𝄞'
AöЖ€𝄞
   utf8 ucp 65 246 1046 8364 119070
AöЖ€𝄞
   ucp_hex utf8 65 246 1046 8364 119070
00041
000f6
00416
020ac
1d11e
   utf8@dfh ucp_hex utf8 65 246 1046 8364 119070
AöЖ€𝄞
```



## Java

```java
import java.nio.charset.StandardCharsets;
import java.util.Formatter;

public class UTF8EncodeDecode {

    public static byte[] utf8encode(int codepoint) {
        return new String(new int[]{codepoint}, 0, 1).getBytes(StandardCharsets.UTF_8);
    }

    public static int utf8decode(byte[] bytes) {
        return new String(bytes, StandardCharsets.UTF_8).codePointAt(0);
    }

    public static void main(String[] args) {
        System.out.printf("%-7s %-43s %7s\t%s\t%7s%n",
                "Char", "Name", "Unicode", "UTF-8 encoded", "Decoded");

        for (int codepoint : new int[]{0x0041, 0x00F6, 0x0416, 0x20AC, 0x1D11E}) {
            byte[] encoded = utf8encode(codepoint);
            Formatter formatter = new Formatter();
            for (byte b : encoded) {
                formatter.format("%02X ", b);
            }
            String encodedHex = formatter.toString();
            int decoded = utf8decode(encoded);
            System.out.printf("%-7c %-43s U+%04X\t%-12s\tU+%04X%n",
                    codepoint, Character.getName(codepoint), codepoint, encodedHex, decoded);
        }
    }
}
```

```txt

Char    Name                                        Unicode	UTF-8 encoded	Decoded
A       LATIN CAPITAL LETTER A                      U+0041	41          	A
ö       LATIN SMALL LETTER O WITH DIAERESIS         U+00F6	C3 B6       	ö
Ж       CYRILLIC CAPITAL LETTER ZHE                 U+0416	D0 96       	Ж
€       EURO SIGN                                   U+20AC	E2 82 AC    	€
𝄞      MUSICAL SYMBOL G CLEF                       U+1D11E	F0 9D 84 9E 	𝄞

```



## JavaScript

An implementation in ECMAScript 2015 (ES6):

```javascript

/***************************************************************************\
|*  Pure UTF-8 handling without detailed error reporting functionality.    *|
|***************************************************************************|
|*  utf8encode                                                             *|
|*    < String character or UInt32 code point                              *|
|*    > Uint8Array encoded_character                                       *|
|*    | ErrorString                                                        *|
|*                                                                         *|
|*  utf8encode takes a string or uint32 representing a single code point   *|
|*    as its argument and returns an array of length 1 up to 4 containing  *|
|*    utf8 code units representing that character.                         *|
|***************************************************************************|
|*  utf8decode                                                             *|
|*    < Unit8Array [highendbyte highmidendbyte lowmidendbyte lowendbyte]   *|
|*    > uint32 character                                                   *|
|*    | ErrorString                                                        *|
|*                                                                         *|
|*  utf8decode takes an array of one to four uint8 representing utf8 code  *|
|*    units and returns a uint32 representing that code point.             *|
\***************************************************************************/

const
  utf8encode=
    n=>
      (m=>
        m<0x80
       ?Uint8Array.from(
          [ m>>0&0x7f|0x00])
       :m<0x800
       ?Uint8Array.from(
          [ m>>6&0x1f|0xc0,m>>0&0x3f|0x80])
       :m<0x10000
       ?Uint8Array.from(
          [ m>>12&0x0f|0xe0,m>>6&0x3f|0x80,m>>0&0x3f|0x80])
       :m<0x110000
       ?Uint8Array.from(
          [ m>>18&0x07|0xf0,m>>12&0x3f|0x80,m>>6&0x3f|0x80,m>>0&0x3f|0x80])
       :(()=>{throw'Invalid Unicode Code Point!'})())
      ( typeof n==='string'
       ?n.codePointAt(0)
       :n&0x1fffff),
  utf8decode=
    ([m,n,o,p])=>
      m<0x80
     ?( m&0x7f)<<0
     :0xc1<m&&m<0xe0&&n===(n&0xbf)
     ?( m&0x1f)<<6|( n&0x3f)<<0
     :( m===0xe0&&0x9f<n&&n<0xc0
      ||0xe0<m&&m<0xed&&0x7f<n&&n<0xc0
      ||m===0xed&&0x7f<n&&n<0xa0
      ||0xed<m&&m<0xf0&&0x7f<n&&n<0xc0)
    &&o===o&0xbf
     ?( m&0x0f)<<12|( n&0x3f)<<6|( o&0x3f)<<0
     :( m===0xf0&&0x8f<n&&n<0xc0
      ||m===0xf4&&0x7f<n&&n<0x90
      ||0xf0<m&&m<0xf4&&0x7f<n&&n<0xc0)
    &&o===o&0xbf&&p===p&0xbf
     ?( m&0x07)<<18|( n&0x3f)<<12|( o&0x3f)<<6|( p&0x3f)<<0
     :(()=>{throw'Invalid UTF-8 encoding!'})()

```

The testing inputs:

```javascript

const
  str=
    'AöЖ€𝄞'
 ,cps=
    Uint32Array.from(str,s=>s.codePointAt(0))
 ,cus=
    [ [ 0x41]
     ,[ 0xc3,0xb6]
     ,[ 0xd0,0x96]
     ,[ 0xe2,0x82,0xac]
     ,[ 0xf0,0x9d,0x84,0x9e]]
   .map(a=>Uint8Array.from(a))
 ,zip3=
    ([a,...as],[b,...bs],[c,...cs])=>
      0<as.length+bs.length+cs.length
     ?[ [ a,b,c],...zip3(as,bs,cs)]
     :[ [ a,b,c]]
 ,inputs=zip3(str,cps,cus);

```

The testing code:

```javascript

console.log(`\
${'Character'.padEnd(16)}\
${'CodePoint'.padEnd(16)}\
${'CodeUnits'.padEnd(16)}\
${'uft8encode(ch)'.padEnd(16)}\
${'uft8encode(cp)'.padEnd(16)}\
utf8decode(cu)`)
for(let [ch,cp,cu] of inputs)
  console.log(`\
${ch.padEnd(16)}\
${cp.toString(0x10).padStart(8,'U+000000').padEnd(16)}\
${`[${[...cu].map(n=>n.toString(0x10))}]`.padEnd(16)}\
${`[${[...utf8encode(ch)].map(n=>n.toString(0x10))}]`.padEnd(16)}\
${`[${[...utf8encode(cp)].map(n=>n.toString(0x10))}]`.padEnd(16)}\
${utf8decode(cu).toString(0x10).padStart(8,'U+000000')}`)

```

and finally, the output from the test:

```txt

Character       CodePoint       CodeUnits       uft8encode(ch)  uft8encode(cp)  utf8decode(cu)
A               U+000041        [41]            [41]            [41]            U+000041
ö               U+0000f6        [c3,b6]         [c3,b6]         [c3,b6]         U+0000f6
Ж               U+000416        [d0,96]         [d0,96]         [d0,96]         U+000416
€               U+0020ac        [e2,82,ac]      [e2,82,ac]      [e2,82,ac]      U+0020ac
𝄞              U+01d11e        [f0,9d,84,9e]   [f0,9d,84,9e]   [f0,9d,84,9e]   U+01d11e

```
Note that the misalign there on the last line is caused by the string length of astral characters being 2 so the padding functions break.


## Julia

Julia supports by default UTF-8 encoding.


```julia
for t in ("A", "ö", "Ж", "€", "𝄞")
    enc = Vector{UInt8}(t)
    dec = String(enc)
    println(dec, " → ", enc)
end
```


```txt
A → UInt8[0x41]
ö → UInt8[0xc3, 0xb6]
Ж → UInt8[0xd0, 0x96]
€ → UInt8[0xe2, 0x82, 0xac]
𝄞 → UInt8[0xf0, 0x9d, 0x84, 0x9e]
```



## Kotlin


```scala
// version 1.1.2

fun utf8Encode(codePoint: Int) = String(intArrayOf(codePoint), 0, 1).toByteArray(Charsets.UTF_8)

fun utf8Decode(bytes: ByteArray) = String(bytes, Charsets.UTF_8).codePointAt(0)

fun main(args: Array<String>) {
    val codePoints = intArrayOf(0x0041, 0x00F6, 0x0416, 0x20AC, 0x1D11E)
    println("Char  Name                                 Unicode  UTF-8         Decoded")
    for (codePoint in codePoints) {
        var n = if(codePoint <= 0xFFFF) 4 else 5 
        System.out.printf("%-${n}c  %-35s  U+%05X  ", codePoint, Character.getName(codePoint), codePoint)  
        val bytes = utf8Encode(codePoint)
        var s = ""
        for (byte in bytes) s += "%02X ".format(byte)
        val decoded = utf8Decode(bytes)
        n = if(decoded.toInt() <= 0xFFFF) 12 else 11 
        System.out.printf("%-${n}s  %c\n", s, decoded)  
    } 
}
```


```txt

Char  Name                                 Unicode  UTF-8         Decoded
A     LATIN CAPITAL LETTER A               U+00041  41            A
ö     LATIN SMALL LETTER O WITH DIAERESIS  U+000F6  C3 B6         ö
Ж     CYRILLIC CAPITAL LETTER ZHE          U+00416  D0 96         Ж
€     EURO SIGN                            U+020AC  E2 82 AC      €
𝄞     MUSICAL SYMBOL G CLEF                U+1D11E  F0 9D 84 9E   𝄞

```



## Lingo

Since UTF-8 is Lingo's native string encoding, and UTF-8 strings can be read into byteArrays (and v.v.), such UTF-8 encoding and decoding is built-in.<br />
Relevant Lingo functions are:<br />
- charToNum (string): converts single-character string to unicode code point (int)<br />
- numToChar (int): converts unicode code point (int) to single-character string<br />
- byteArray (string): creates byte array of UTF-8 bytes for string<br />
- byteArray.toHexString (intStart, intLen): returns hex string representation of byte array (e.g. for printing)<br />
- byteArray.readRawString (intLen, [strCharSet="UTF-8"]): reads a fixed number of bytes as a string

```Lingo
chars = ["A", "ö", "Ж", "€", "𝄞"]
put "Character   Unicode (int)   UTF-8 (hex)   Decoded"
repeat with c in chars
    ba = bytearray(c)
    put col(c, 12) & col(charToNum(c), 16) & col(ba.toHexString(1, ba.length), 14) & ba.readRawString(ba.length)
end repeat
```

Helper function for table formatting

```Lingo
on col (val, len)
    str = string(val)
    repeat with i = str.length+1 to len
        put " " after str
    end repeat
    return str
end
```

```txt

Character   Unicode (int)   UTF-8 (hex)   Decoded
A           65              41            A
ö           246             c3 b6         ö
Ж           1046            d0 96         Ж
€           8364            e2 82 ac      €
𝄞           119070          f0 9d 84 9e   𝄞

```



## Lua

```Lua

-- Accept an integer representing a codepoint.
-- Return the values of the individual octets.
function encode (codepoint)
  local codepoint_str = utf8.char(codepoint)
  local result = {}

  for i = 1, #codepoint_str do
    result[#result + 1] = string.unpack("B", codepoint_str, i)
  end

  return table.unpack(result)
end

-- Accept a variable number of octets.
-- Return the corresponding Unicode character.
function decode (...)
  local len = select("#", ...) -- the number of octets
  local fmt = string.rep("B", len)

  return string.pack(fmt, ...)
end

-- Run the given test cases.
function test_encode_decode ()
  -- "A", "ö", "Ж", "€", "𝄞"
  local tests = {tonumber("41", 16),  tonumber("f6", 16), tonumber("416", 16),
                  tonumber("20ac", 16), tonumber("1d11e", 16)}

  for i, test in ipairs(tests) do
    print("Char: ", test)
    print("Encoding: ", encode(test))
    print("Decoding: ", decode(encode(test)))
  end
end

```

```txt

Char: 	65
Encoding: 	65
Decoding: 	A
Char: 	246
Encoding: 	195	182
Decoding: 	ö
Char: 	1046
Encoding: 	208	150
Decoding: 	Ж
Char: 	8364
Encoding: 	226	130	172
Decoding: 	€
Char: 	119070
Encoding: 	240	157	132	158
Decoding: 	𝄞

```


## M2000 Interpreter


```M2000 Interpreter

Module EncodeDecodeUTF8 {
      a$=string$("Hello" as UTF8enc)
      Print Len(A$)=2.5   ' 2.5 words=5 bytes
      b$=string$(a$ as UTF8dec)
      Print b$
      Print Len(b$)=5 ' 5 words = 10 bytes   
      
      Print Len(string$("A" as UTF8enc))=.5  ' 1 byte
      Print Len(string$("ö" as UTF8enc))=1   ' 2 bytes
      Print Len(string$("Ж" as UTF8enc))=1   ' 2 bytes
      Print Len(string$("€" as UTF8enc))=1.5   ' 3 bytes
      Print Len(string$("𝄞" as UTF8enc))=2      '4 bytes
      a$=string$("𝄞" as UTF8enc)
      Buffer Bytes as Byte*4
      Return Bytes, 0:=a$
      \\ F0 9D 84 9E
      Hex Eval(bytes, 0), Eval(bytes, 1), Eval(bytes, 2), Eval(bytes, 3) 
}
EncodeDecodeUTF8

```

```txt

      True
Hello
      True
      True
      True
      True
      True
      True
    0x00F0    0x009D    0x0084    0x009E
</pre >


## Mathematica


```Mathematica
utf = ToCharacterCode[ToString["AöЖ€", CharacterEncoding -> "UTF8"]]
ToCharacterCode[FromCharacterCode[utf, "UTF8"]]
```

```txt
{65, 195, 182, 208, 150, 226, 130, 172}
{65, 246, 1046, 8364}

```



## Perl


```perl
#!/usr/bin/perl
use strict;
use warnings;
use Unicode::UCD 'charinfo';         # getting the unicode name of the character
use utf8;                            # using non-ascii-characters in source code
binmode STDOUT, ":encoding(UTF-8)";  # printing non-ascii-characters to screen

my @chars = map {ord} qw/A ö Ж € 𝄞/; # @chars contains the unicode points
my $print_format = '%5s  %-35s';
printf "$print_format %8s  %s\n" , 'char', 'name', 'unicode', 'utf-8 encoding';
map{
	my $name = charinfo($_)->{'name'}; # get unicode name
	printf "$print_format  %06x  " , chr, lc $name, $_;
	my $utf8 = chr;                    # single char (using implicit $_)
	utf8::encode($utf8);               # inplace encoding into utf8 parts
	map{                               # for each utf8 char print ord
		printf " %x", ord;
	} split //, $utf8;
	print "\n";
} @chars;
```


```txt

 char  name                                 unicode  utf-8 encoding
    A  latin capital letter a               000041   41
    ö  latin small letter o with diaeresis  0000f6   c3 b6
    Ж  cyrillic capital letter zhe          000416   d0 96
    €  euro sign                            0020ac   e2 82 ac
    𝄞  musical symbol g clef                01d11e   f0 9d 84 9e

```



## Perl 6

Pretty much all built in to the language.

```perl6
say sprintf("%-18s %-36s|%8s| %7s |%14s | %s\n", 'Character|', 'Name', 'Ordinal', 'Unicode', 'UTF-8 encoded', 'decoded'), '-' x 100;

for < A ö Ж € 𝄞 😜 👨‍👩‍👧‍👦> -> $char {
    printf "   %-5s | %-43s | %6s | %-7s | %12s  |%4s\n", $char, $char.uninames.join(','), $char.ords.join(' '),
      ('U+' X~ $char.ords».base(16)).join(' '), $char.encode('UTF8').list».base(16).Str, $char.encode('UTF8').decode;
}
```

```txt
Character|         Name                                | Ordinal| Unicode | UTF-8 encoded | decoded
----------------------------------------------------------------------------------------------------
   A     | LATIN CAPITAL LETTER A                      |     65 | U+41    |           41  |   A
   ö     | LATIN SMALL LETTER O WITH DIAERESIS         |    246 | U+F6    |        C3 B6  |   ö
   Ж    | CYRILLIC CAPITAL LETTER ZHE                 |   1046 | U+416   |        D0 96  |   Ж
   €     | EURO SIGN                                   |   8364 | U+20AC  |     E2 82 AC  |   €
   𝄞     | MUSICAL SYMBOL G CLEF                       | 119070 | U+1D11E |  F0 9D 84 9E  |   𝄞
   😜    | FACE WITH STUCK-OUT TONGUE AND WINKING EYE  | 128540 | U+1F61C |  F0 9F 98 9C  |   😜
   👨‍👩‍👧‍👦    | MAN,ZERO WIDTH JOINER,WOMAN,ZERO WIDTH JOINER,GIRL,ZERO WIDTH JOINER,BOY | 128104 8205 128105 8205 128103 8205 128102 | U+1F468 U+200D U+1F469 U+200D U+1F467 U+200D U+1F466 | F0 9F 91 A8 E2 80 8D F0 9F 91 A9 E2 80 8D F0 9F 91 A7 E2 80 8D F0 9F 91 A6  |   👨‍👩‍👧‍👦

```



## Phix

Standard autoinclude, see the manual and/or builtins/utfconv.e
( http://phix.x10.mx/docs/html/utfconv.htm and/or https://bitbucket.org/petelomax/phix/src )

As requested in the task description:

```Phix
constant tests = {#0041, #00F6, #0416, #20AC, #1D11E}

function hex(sequence s, string fmt)    -- output helper
    for i=1 to length(s) do
        s[i] = sprintf(fmt,s[i])
    end for
    return join(s,',')
end function

for i=1 to length(tests) do
    integer codepoint = tests[i]
    sequence s = utf32_to_utf8({codepoint}),
             r = utf8_to_utf32(s)
    printf(1,"#%04x -> {%s} -> {%s}\n",{codepoint, hex(s,"#%02x"),hex(r,"#%04x")})
end for
```

```txt

#0041 -> {#41} -> {#0041}
#00F6 -> {#C3,#B6} -> {#00F6}
#0416 -> {#D0,#96} -> {#0416}
#20AC -> {#E2,#82,#AC} -> {#20AC}
#1D11E -> {#F0,#9D,#84,#9E} -> {#1D11E}

```


## PureBasic

The encoding and decoding procedure are kept simple and designed to work with an array of 5 elements for input/output of the UTF-8 encoding for a single code point at a time.  It was decided not to use a more elaborate example that would have been able to operate on a buffer to encode/decode more than one code point at a time.


```purebasic
#UTF8_codePointMaxByteCount = 4 ;UTF-8 encoding uses only a maximum of 4 bytes to encode a codepoint

Procedure UTF8_encode(x, Array encoded_codepoint.a(1)) ;x is codepoint to encode, the array will contain output
  ;Array encoded_codepoint() is used for output.
  ;After encode element zero holds the count of significant bytes in elements 1 to 4
  If ArraySize(encoded_codepoint()) < #UTF8_codePointMaxByteCount
    ReDim encoded_codepoint.a(#UTF8_codePointMaxByteCount)
  EndIf
  
  Select x
    Case 0 To $7F
      encoded_codepoint(0) = 1
      encoded_codepoint(1) = x ;all 7 bits
    Case $80 To $7FF
      encoded_codepoint(0) = 2
      encoded_codepoint(2) = (x & %00111111) | %10000000         ;lowest 6 bits
      encoded_codepoint(1) = (x >> 6) | %11000000                ;highest bits 7 -> 11
    Case $800 To $FFFF
      encoded_codepoint(0) = 3
      encoded_codepoint(3) = (x & %00111111) | %10000000         ;lowest 6 bits
      encoded_codepoint(2) = ((x >> 6) & %00111111) | %10000000  ;bits 7 -> 12
      encoded_codepoint(1) = (x >> 12) | %11100000               ;highest bits 13 -> 16
      
    Case $10000 To $10FFFF
      encoded_codepoint(0) = 4
      encoded_codepoint(4) = (x & %00111111) | %10000000         ;lowest 6 bits
      encoded_codepoint(3) = ((x >> 6) & %00111111) | %10000000  ;bits 7 -> 12
      encoded_codepoint(2) = ((x >> 12) & %00111111) | %10000000 ;bits 13 -> 18
      encoded_codepoint(1) = (x >> 18) | %11110000               ;highest bits 19 -> 21
    Default                                      
      encoded_codepoint(0) = 0  ;error, codepoint is not valid and can't be encoded
  EndSelect
EndProcedure

Procedure UTF8_decode(Array encoded_codepoint.a(1))
  ;Array encoded_codepoint() holds the UTF-8 encoding in elements 1 to 4, element zero isn't used for decoding.
  Protected x = -1 ;initialzie with error value for possible improper encoding
  
  If ArraySize(encoded_codepoint()) < #UTF8_codePointMaxByteCount
    ProcedureReturn x ;Input array was not dimensioned properly.
  EndIf
  
  ;Determine the number of bytes in the UTF8 encoding by looking at first byte
  ;and then proceeding accordingly.
  Select encoded_codepoint(1)
    Case %00000000 To %01111111 ;1 byte encoding
      x = encoded_codepoint(1)
    Case %11000000 To %11011111 ;2 byte encoding
      x = (encoded_codepoint(1) & %00011111) << 6 ;last 5 bits only
      x | (encoded_codepoint(2) & %00111111)
    Case %11100000 To %11101111 ;3 byte encoding
      x = (encoded_codepoint(1) & %00001111) << 6 ;last 4 bits only
      x << 6 + (encoded_codepoint(2) & %00111111) 
      x << 6 + (encoded_codepoint(3) & %00111111) 
    Case %11110000 To %11110111 ;4 byte encoding
      x = (encoded_codepoint(1) & %00000111) << 6 ;last 3 bits only
      x << 6 + (encoded_codepoint(2) & %00111111) 
      x << 6 + (encoded_codepoint(3) & %00111111) 
      x << 6 + (encoded_codepoint(4) & %00111111) 
  EndSelect
  
  ProcedureReturn x
EndProcedure

;helper procedure to format output for this example
Procedure.s formatOutput(c$, c, Array encoded_utf.a(1), dcp) ;character, codepooint, UTf8 encoding, decoded codepoint
  Protected o$, i, encoding$
  
  o$ = "   " + LSet(c$, 8) + LSet("U+" + RSet(Hex(c), 5, "0"), 10)
  For i = 1 To encoded_utf(0)
    encoding$ + RSet(Hex(encoded_utf(i)), 2, "0") + " "
  Next
  o$ + "  " + LSet(encoding$, 11, " ") + "   " + RSet(Hex(dcp), 5, "0")
  
  ProcedureReturn o$  
EndProcedure

DataSection
  ;unicode code points in hex
  unicode_codepoints:
  Data.i 5, $41, $F6, $416, $20AC, $1D11E
  ;The names for these codepoints are: latin capital letter a; latin small letter o With diaeresis
  ;cyrillic capital letter zhe; euro sign; musical symbol g clef.
EndDataSection

;read initial unicode codepoint values
Restore unicode_codepoints
Read num_codepoints
num_codepoints - 1

Dim codepoint(num_codepoints)
For i = 0 To num_codepoints
  Read codepoint(i)
Next

;This array is used for input and output from the UTF8 encode and decode procedures.  After encoding its elements
;hold the byte count of the encoding followed by the respective bytes.  For decoding element zero is not used and
;elements 1 To 4 holds the bytes to be decoded.
Dim encoded_codepoint.a(#UTF8_codePointMaxByteCount)
If OpenConsole("", #PB_UTF8)
  PrintN(LSet("", 11) + LSet("Unicode", 12) + LSet("UTF-8",14) + LSet("Decoded",12))
  PrintN(LSet("Character", 11) + LSet("Code Point", 12) + LSet("Encoding",14) + LSet("Code Point",12))
  PrintN(LSet("---------", 11) + LSet("----------", 12) + LSet("-----------",14) + LSet("-----------",12))
  
  For i = 0 To num_codepoints
    UTF8_encode(codepoint(i), encoded_codepoint())
    dcp = UTF8_decode(encoded_codepoint()) ;Decoded UTF-8 encoding should match original codepoint that was encoded.
    PrintN(formatOutput(Chr(codepoint(i)), codepoint(i), encoded_codepoint(), dcp))
  Next
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
           Unicode     UTF-8         Decoded
Character  Code Point  Encoding      Code Point
---------  ----------  -----------   -----------
   A       U+00041     41            00041
   ö       U+000F6     C3 B6         000F6
   ?       U+00416     D0 96         00416
   ?       U+020AC     E2 82 AC      800AC
   ?       U+1D11E     F0 9D 84 9E   1D11E

```



## Python


```python

#!/usr/bin/env python3
from unicodedata import name


def unicode_code(ch):
    return 'U+{:04x}'.format(ord(ch))


def utf8hex(ch):
    return " ".join([hex(c)[2:] for c in ch.encode('utf8')]).upper()


if __name__ == "__main__":
    print('{:<11} {:<36} {:<15} {:<15}'.format('Character', 'Name', 'Unicode', 'UTF-8 encoding (hex)'))
    chars = ['A', 'ö', 'Ж', '€', '𝄞']
    for char in chars:
        print('{:<11} {:<36} {:<15} {:<15}'.format(char, name(char), unicode_code(char), utf8hex(char)))
```

```txt
Character   Name                                 Unicode         UTF-8 encoding (hex)
A           LATIN CAPITAL LETTER A               U+0041          41             
ö           LATIN SMALL LETTER O WITH DIAERESIS  U+00f6          C3 B6          
Ж           CYRILLIC CAPITAL LETTER ZHE          U+0416          D0 96          
€           EURO SIGN                            U+20ac          E2 82 AC       
𝄞           MUSICAL SYMBOL G CLEF                U+1d11e         F0 9D 84 9E
```



## Racket


```racket
#lang racket

(define char-map
  '((LATIN-CAPITAL-LETTER-A              .  #\U0041)
    (LATIN-SMALL-LETTER-O-WITH-DIAERESIS .  #\U00F6)
    (CYRILLIC-CAPITAL-LETTER-ZHE         .  #\U0416)
    (EURO-SIGN                           .  #\U20AC)
    (MUSICAL-SYMBOL-G-CLEF               .  #\U1D11E)))

(for ((name.char (in-list char-map)))
  (define name (car name.char))
  (define chr (cdr name.char))
  (let ((bites (bytes->list (string->bytes/utf-8 (list->string (list chr))))))
    (printf "~s\t~a\t~a\t~a\t~a~%" chr chr
            (map (curryr number->string 16) bites)
            (bytes->string/utf-8 (list->bytes bites))
            name)))
```

```txt
#\A	A	(41)	A	LATIN-CAPITAL-LETTER-A
#\ö	ö	(c3 b6)	ö	LATIN-SMALL-LETTER-O-WITH-DIAERESIS
#\Ж	Ж	(d0 96)	Ж	CYRILLIC-CAPITAL-LETTER-ZHE
#\€	€	(e2 82 ac)	€	EURO-SIGN
#\𝄞	𝄞	(f0 9d 84 9e)	𝄞	MUSICAL-SYMBOL-G-CLEF
```


## Scala


###  Imperative solution


```scala
object UTF8EncodeAndDecode extends App {

  val codePoints = Seq(0x0041, 0x00F6, 0x0416, 0x20AC, 0x1D11E)

  def utf8Encode(codepoint: Int): Array[Byte] =
    new String(Array[Int](codepoint), 0, 1).getBytes(StandardCharsets.UTF_8)

  def utf8Decode(bytes: Array[Byte]): Int =
    new String(bytes, StandardCharsets.UTF_8).codePointAt(0)

  println("Char Name                                 Unicode  UTF-8       Decoded")
  for (codePoint <- codePoints) {
    val w = if (Character.isBmpCodePoint(codePoint)) 4 else 5 // Compute spacing
    val bytes = utf8Encode(codePoint)

    def leftAlignedHex = f"U+${codePoint}%04X"

    val s = new StringBuilder()
    bytes.foreach(byte => s ++= "%02X ".format(byte))

    printf(s"%-${w}c %-36s %-7s  %-${16 - w}s%c%n",
      codePoint, Character.getName(codePoint), leftAlignedHex, s, utf8Decode(bytes))
  }
```


###  Functional solution


```scala
import java.nio.charset.StandardCharsets

object UTF8EncodeAndDecode extends App {

  val codePoints = Seq(0x0041, 0x00F6, 0x0416, 0x20AC, 0x1D11E)

  def utf8Encode(codepoint: Int): Array[Byte] =
    new String(Array[Int](codepoint), 0, 1).getBytes(StandardCharsets.UTF_8)

  def utf8Decode(bytes: Array[Byte]): Int =
    new String(bytes, StandardCharsets.UTF_8).codePointAt(0)

  println("Char Name                                 Unicode  UTF-8       Decoded")
  codePoints.foreach{ codePoint =>
    val w = if (Character.isBmpCodePoint(codePoint)) 4 else 5 // Compute spacing
    val bytes = utf8Encode(codePoint)

    def leftAlignedHex: String = f"U+${codePoint}%04X"

    def utf: String = bytes.foldLeft("")(_ + "%02X ".format(_))

    printf(s"%-${w}c %-36s %-7s  %-${16 - w}s%c%n",
      codePoint, Character.getName(codePoint), leftAlignedHex, utf, utf8Decode(bytes))  }

  println(s"\nSuccessfully completed without errors. [total ${scala.compat.Platform.currentTime - executionStart} ms]")
}
```


###  Composable and testable solution


```scala
package example

object UTF8EncodeAndDecode extends TheMeat with App {
  val codePoints = Seq(0x0041, 0x00F6, 0x0416, 0x20AC, 0x1D11E)

  println("Char Name                                 Unicode  UTF-8       Decoded")
  codePoints.foreach { codepoint => print(composeString(codepoint)) }

  println(s"\nSuccessfully completed without errors. [total ${scala.compat.Platform.currentTime - executionStart} ms]")
}

trait TheMeat {
  import java.nio.charset.StandardCharsets

  def composeString(codePoint: Int): String = {
    val w = if (Character.isBmpCodePoint(codePoint)) 4 else 5 // Compute spacing
    val bytes = utf8Encode(codePoint)

    def leftAlignedHex: String = f"U+${codePoint}%04X"

    def utf: String = bytes.foldLeft("")(_ + "%02X ".format(_))

    s"%-${w}c %-36s %-7s  %-${16 - w}s%c%n"
      .format(codePoint, Character.getName(codePoint), leftAlignedHex, utf, utf8Decode(bytes))
  }

  def utf8Encode(codepoint: Int): Array[Byte] =
    new String(Array[Int](codepoint), 0, 1).getBytes(StandardCharsets.UTF_8)

  def utf8Decode(bytes: Array[Byte]): Int =
    new String(bytes, StandardCharsets.UTF_8).codePointAt(0)

}

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "unicode.s7i";
  include "console.s7i";
  include "bytedata.s7i";

const proc: main is func
  local
    var char: ch is ' ';
    var string: utf8 is "";
  begin
    OUT := STD_CONSOLE;
    writeln("Character  Unicode  UTF-8 encoding (hex)  Decoded");
    writeln("-------------------------------------------------");
    for ch range "AöЖ€𝄞" do
      utf8 := striToUtf8(str(ch));
      writeln(ch rpad 11 <& "U+" <& ord(ch) radix 16 lpad0 4 rpad 7 <&
              hex(utf8) rpad 22 <& utf8ToStri(utf8));
    end for;
  end func;
```


```txt

Character  Unicode  UTF-8 encoding (hex)  Decoded
-------------------------------------------------
A          U+0041   41                    A
ö          U+00f6   c3b6                  ö
Ж          U+0416   d096                  Ж
€          U+20ac   e282ac                €
𝄞          U+1d11e  f09d849e              𝄞

```



## Sidef


```ruby
func utf8_encoder(Number code) {
    code.chr.encode('UTF-8').bytes.map{.chr}
}

func utf8_decoder(Array bytes) {
    bytes.map{.ord}.decode('UTF-8')
}

for n in ([0x0041, 0x00F6, 0x0416, 0x20AC, 0x1D11E]) {
    var encoded = utf8_encoder(n)
    var decoded = utf8_decoder(encoded)
    assert_eq(n, decoded.ord)
    say "#{decoded} -> #{encoded}"
}
```

```txt

A -> ["A"]
ö -> ["\xC3", "\xB6"]
Ж -> ["\xD0", "\x96"]
€ -> ["\xE2", "\x82", "\xAC"]
𝄞 -> ["\xF0", "\x9D", "\x84", "\x9E"]

```


## Swift

In Swift there's a difference between UnicodeScalar, which is a single unicode code point, and Character which may consist out of multiple UnicodeScalars, usually because of combining characters.

```Swift
import Foundation

func encode(_ scalar: UnicodeScalar) -> Data {
  return Data(String(scalar).utf8)
}

func decode(_ data: Data) -> UnicodeScalar? {
  guard let string = String(data: data, encoding: .utf8) else {
    assertionFailure("Failed to convert data to a valid String")
    return nil
  }
  assert(string.unicodeScalars.count == 1, "Data should contain one scalar!")
  return string.unicodeScalars.first
}

for scalar in "AöЖ€𝄞".unicodeScalars {
  let bytes = encode(scalar)
  let formattedBytes = bytes.map({ String($0, radix: 16)}).joined(separator: " ")
  let decoded = decode(bytes)!
  print("character: \(decoded), code point: U+\(String(scalar.value, radix: 16)), \tutf-8: \(formattedBytes)")
}

```

```txt

character: A, code point: U+41, 	utf-8: 41
character: ö, code point: U+f6, 	utf-8: c3 b6
character: Ж, code point: U+416, 	utf-8: d0 96
character: €, code point: U+20ac, 	utf-8: e2 82 ac
character: 𝄞, code point: U+1d11e, 	utf-8: f0 9d 84 9e

```



## Tcl

Note: Tcl can handle Unicodes only up to U+FFFD, i.e. the Basic Multilingual Plane (BMP, 16 bits wide). Therefore, the fifth test fails as expected.

```Tcl
proc encoder int {
   set u [format %c $int]
   set bytes {}
   foreach byte [split [encoding convertto utf-8 $u] ""] {
      lappend bytes [format %02X [scan $byte %c]]
   }
   return $bytes
}
proc decoder bytes {
   set str {}
   foreach byte $bytes {
      append str [format %c [scan $byte %x]]
   }
   return [encoding convertfrom utf-8 $str]
}
foreach test {0x0041 0x00f6 0x0416 0x20ac 0x1d11e} {
   set res $test
   lappend res [encoder $test] -> [decoder [encoder $test]]
   puts $res
}
```


```txt

0x0041 41 -> A
0x00f6 {C3 B6} -> ö
0x0416 {D0 96} -> Ж
0x20ac {E2 82 AC} -> €
0x1d11e {EF BF BD} -> �

```



###  Alternative Implementation 

While perhaps not as readable as the above, this version handles beyond-BMP codepoints by manually composing the utf-8 byte sequences and emitting raw bytes to the console.  <tt>encoding convertto utf-8</tt> command still does the heavy lifting where it can.


```Tcl
proc utf8 {codepoint} {
    scan $codepoint %llx cp
    if {$cp < 0x10000} {
        set str [subst \\u$codepoint]               ;# substitute per Tcl backslash rule
        set bytes [encoding convertto utf-8 $str]   ;# encode
    } else {                                        ;# codepoints beyond the BMP need manual approach
        set bits [format %021b $cp]                 ;# format as binary string
        set unibits    11110[string range $bits 0 2];# insert extra bits for utf-8 4-byte encoding
        append unibits 10[string range $bits 3 8]
        append unibits 10[string range $bits 9 14]
        append unibits 10[string range $bits 15 20]
        set bytes [binary format B* $unibits]       ;# turn into a sequence of bytes
    }
    return $bytes
}

proc hexchars {s} {
    binary scan $s H* hex
    regsub -all .. $hex {\0 }
}

# for the test, we assume the tty is in utf-8 mode and can handle beyond-BMP chars
# so set output mode to binary so we can write raw bytes!
chan configure stdout -encoding binary
foreach codepoint { 41 F6 416 20AC 1D11E } {
    set utf8 [utf8 $codepoint]
    puts "[format U+%04s $codepoint]\t$utf8\t[hexchars $utf8]"
}
```


```txt
U+0041  A       41
U+00F6  ö       c3 b6
U+0416  Ж       d0 96
U+20AC  €       e2 82 ac
U+1D11E 𝄞       f0 9d 84 9e

```



## VBA


```VB
Private Function unicode_2_utf8(x As Long) As Byte()
    Dim y() As Byte
    Dim r As Long
    Select Case x
        Case 0 To &H7F
            ReDim y(0)
            y(0) = x
        Case &H80 To &H7FF
            ReDim y(1)
            y(0) = 192 + x \ 64
            y(1) = 128 + x Mod 64
        Case &H800 To &H7FFF
            ReDim y(2)
            y(2) = 128 + x Mod 64
            r = x \ 64
            y(1) = 128 + r Mod 64
            y(0) = 224 + r \ 64
        Case 32768 To 65535 '&H8000 To &HFFFF equals in VBA as -32768 to -1
            ReDim y(2)
            y(2) = 128 + x Mod 64
            r = x \ 64
            y(1) = 128 + r Mod 64
            y(0) = 224 + r \ 64
        Case &H10000 To &H10FFFF
            ReDim y(3)
            y(3) = 128 + x Mod 64
            r = x \ 64
            y(2) = 128 + r Mod 64
            r = r \ 64
            y(1) = 128 + r Mod 64
            y(0) = 240 + r \ 64
        Case Else
            MsgBox "what else?" & x & " " & Hex(x)
    End Select
    unicode_2_utf8 = y
End Function
Private Function utf8_2_unicode(x() As Byte) As Long
    Dim first As Long, second As Long, third As Long, fourth As Long
    Dim total As Long
    Select Case UBound(x) - LBound(x)
        Case 0 'one byte
            If x(0) < 128 Then
                total = x(0)
            Else
                MsgBox "highest bit set error"
            End If
        Case 1 'two bytes and assume first byte is leading byte
            If x(0) \ 32 = 6 Then
                first = x(0) Mod 32
                If x(1) \ 64 = 2 Then
                    second = x(1) Mod 64
                Else
                    MsgBox "mask error"
                End If
            Else
                MsgBox "leading byte error"
            End If
            total = 64 * first + second
        Case 2 'three bytes and assume first byte is leading byte
            If x(0) \ 16 = 14 Then
                first = x(0) Mod 16
                If x(1) \ 64 = 2 Then
                    second = x(1) Mod 64
                    If x(2) \ 64 = 2 Then
                        third = x(2) Mod 64
                    Else
                        MsgBox "mask error last byte"
                    End If
                Else
                    MsgBox "mask error middle byte"
                End If
            Else
                MsgBox "leading byte error"
            End If
                total = 4096 * first + 64 * second + third
        Case 3 'four bytes and assume first byte is leading byte
            If x(0) \ 8 = 30 Then
                first = x(0) Mod 8
                If x(1) \ 64 = 2 Then
                    second = x(1) Mod 64
                    If x(2) \ 64 = 2 Then
                        third = x(2) Mod 64
                        If x(3) \ 64 = 2 Then
                            fourth = x(3) Mod 64
                        Else
                            MsgBox "mask error last byte"
                        End If
                    Else
                        MsgBox "mask error third byte"
                    End If
                Else
                    MsgBox "mask error second byte"
                End If
            Else
                MsgBox "mask error leading byte"
            End If
            total = CLng(262144 * first + 4096 * second + 64 * third + fourth)
        Case Else
            MsgBox "more bytes than expected"
        End Select
        utf8_2_unicode = total
End Function
Public Sub program()
    Dim cp As Variant
    Dim r() As Byte, s As String
    cp = [{65, 246, 1046, 8364, 119070}] '[{&H0041,&H00F6,&H0416,&H20AC,&H1D11E}]
    Debug.Print "ch  unicode  UTF-8 encoded  decoded"
    For Each cpi In cp
        r = unicode_2_utf8(CLng(cpi))
        On Error Resume Next
        s = CStr(Hex(cpi))
        Debug.Print ChrW(cpi); String$(10 - Len(s), " "); s,
        If Err.Number = 5 Then Debug.Print "?"; String$(10 - Len(s), " "); s,
        s = ""
        For Each yz In r
            s = s & CStr(Hex(yz)) & " "
        Next yz
        Debug.Print String$(13 - Len(s), " "); s;
        s = CStr(Hex(utf8_2_unicode(r)))
        Debug.Print String$(8 - Len(s), " "); s
    Next cpi
End Sub
```
```txt
ch  unicode  UTF-8 encoded  decoded
A        41             41       41
ö        F6          C3 B6       F6
?       416          D0 96      416
€      20AC       E2 82 AC     20AC
?     1D11E    F0 9D 84 9E    1D11E

```


## zkl


```zkl
println("Char  Unicode  UTF-8");
foreach utf,unicode_int in (T( T("\U41;",0x41), T("\Uf6;",0xf6), 
      T("\U416;",0x416), T("\U20AC;",0x20ac), T("\U1D11E;",0x1d11e))){
   utf_int:=utf.reduce(fcn(s,c){ 0x100*s + c.toAsc() },0);
   char :=unicode_int.toString(-8);	// Unicode int to UTF-8 string
   // UTF-8 bytes to UTF-8 string:
   char2:=Data(Void,utf_int.toBigEndian(utf_int.len())).text;

   println("%s %s %9s  %x".fmt(char,char2,"U+%x".fmt(unicode_int),utf_int));
}
```

Int.len() --> number of bytes in int. This could be hard coded because UTF-8 
has a max of 6 bytes and (0x41).toBigEndian(6) --> 0x41,0,0,0,0,0 which is
a zero terminated string ("A");
```txt

Char  Unicode  UTF-8
A A      U+41  41
ö ö      U+f6  c3b6
Ж Ж     U+416  d096
€ €    U+20ac  e282ac
𝄞 𝄞   U+1d11e  f09d849e

```

