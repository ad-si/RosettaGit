+++
title = "Bitwise IO"
description = ""
date = 2019-08-02T18:46:58Z
aliases = []
[extra]
id = 3266
[taxonomies]
categories = ["Bitwise operations", "task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "autohotkey",
  "bbc_basic",
  "c",
  "common_lisp",
  "csharp",
  "d",
  "erlang",
  "forth",
  "go",
  "haskell",
  "j",
  "julia",
  "kotlin",
  "lingo",
  "mips_assembly",
  "nibble_in_nibble_void",
  "nibble_nibble_void",
  "nim",
  "ocaml",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ruby",
  "seed7",
  "tcl",
  "zkl",
]
+++

## Task
The aim of this task is to write functions (or create a class if your
language is Object Oriented and you prefer) for reading and writing sequences of
bits, most significant bit first. While the output of a <tt>asciiprint "STRING"</tt> is the ASCII byte sequence
"S", "T", "R", "I", "N", "G", the output of a "print" of the bits sequence
0101011101010 (13 bits) must be 0101011101010; real I/O is performed always
''quantized'' by byte (avoiding endianness issues and relying on underlying
buffering for performance), therefore you must obtain as output the bytes
0101 0111 0101 0'''000''' (bold bits are padding bits), i.e. in hexadecimal 57 50.

As test, you can implement a '''rough''' (e.g. don't care about error handling or
other issues) compression/decompression program for ASCII sequences
of bytes, i.e. bytes for which the most significant bit is always unused, so that you can write
seven bits instead of eight (each 8 bytes of input, we write 7 bytes of output).

These bit oriented I/O functions can be used to implement compressors and
decompressors; e.g. Dynamic and Static Huffman encodings use variable length
bits sequences, while LZW (see [[LZW compression]]) use fixed or variable ''words''
nine (or more) bits long.

* Limits in the maximum number of bits that can be written/read in a single read/write operation are allowed.
* Errors handling is not mandatory





## Ada


```ada
with Ada.Streams;  use Ada.Streams;
with Ada.Finalization;

package Bit_Streams is
   type Bit is range 0..1;
   type Bit_Array is array (Positive range <>) of Bit;
   type Bit_Stream (Channel : not null access Root_Stream_Type'Class) is limited private;
   procedure Read (Stream : in out Bit_Stream; Data : out Bit_Array);
   procedure Write (Stream : in out Bit_Stream; Data : Bit_Array);
private
   type Bit_Stream (Channel : not null access Root_Stream_Type'Class) is
      new Ada.Finalization.Limited_Controlled with
   record
      Read_Count  : Natural := 0;
      Write_Count : Natural := 0;
      Input       : Stream_Element_Array (1..1);
      Output      : Stream_Element_Array (1..1);
   end record;
   overriding procedure Finalize (Stream : in out Bit_Stream);
end Bit_Streams;
```

The package provides a bit stream interface to a conventional stream. The object of Bit_Stream has a discriminant of any stream type. This stream will be used for physical I/O. Bit_Stream reads and writes arrays of bits. There is no need to have flush procedure, because this is done upon object destruction. The implementation is straightforward, big endian encoding of bits into Stream_Element units is used as required by the task:

```ada
package body Bit_Streams is
   procedure Finalize (Stream : in out Bit_Stream) is
   begin
      if Stream.Write_Count > 0 then
         Stream.Output (1) := Stream.Output (1) * 2**(Stream_Element'Size - Stream.Write_Count);
         Stream.Channel.Write (Stream.Output);
      end if;
   end Finalize;
   procedure Read (Stream : in out Bit_Stream; Data : out Bit_Array) is
      Last : Stream_Element_Offset;
   begin
      for Index in Data'Range loop
         if Stream.Read_Count = 0 then
            Stream.Channel.Read (Stream.Input, Last);
            Stream.Read_Count := Stream_Element'Size;
         end if;
         Data (Index) := Bit (Stream.Input (1) / 2**(Stream_Element'Size - 1));
         Stream.Input (1)  := Stream.Input (1) * 2;
         Stream.Read_Count := Stream.Read_Count - 1;
      end loop;
   end Read;
   procedure Write (Stream : in out Bit_Stream; Data : Bit_Array) is
   begin
      for Index in Data'Range loop
         if Stream.Write_Count = Stream_Element'Size then
            Stream.Channel.Write (Stream.Output);
            Stream.Write_Count := 0;
         end if;
         Stream.Output (1)  := Stream.Output (1) * 2 or Stream_Element (Data (Index));
         Stream.Write_Count := Stream.Write_Count + 1;
      end loop;
   end Write;
end Bit_Streams;
```

Example of use:

```ada
with Ada.Streams.Stream_IO;  use Ada.Streams.Stream_IO;
with Bit_Streams;            use Bit_Streams;

procedure Test_Bit_Streams is
   File   : File_Type;
   ABACUS : Bit_Array :=
               (  1,0,0,0,0,0,1,  -- A, big endian
                  1,0,0,0,0,1,0,  -- B
                  1,0,0,0,0,0,1,  -- A
                  1,0,0,0,0,1,1,  -- C
                  1,0,1,0,1,0,1,  -- U
                  1,0,1,0,0,1,1   -- S
               );
   Data : Bit_Array (ABACUS'Range);
begin
   Create (File, Out_File, "abacus.dat");
   declare
      Bits : Bit_Stream (Stream (File));
   begin
      Write (Bits, ABACUS);
   end;
   Close (File);
   Open (File, In_File, "abacus.dat");
   declare
      Bits : Bit_Stream (Stream (File));
   begin
      Read (Bits, Data);
   end;
   Close (File);
   if Data /= ABACUS then
      raise Data_Error;
   end if;
end Test_Bit_Streams;
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

```algol68
# NIBBLEs are of any width, eg 1-bit OR 4-bits etc. #
MODE NIBBLE = STRUCT(INT width, BITS bits);

PRIO << = 8, >> = 8; # define C style shift opertors #
OP << = (BITS bits, INT shift)BITS: bits SHL shift;
OP >> = (BITS bits, INT shift)BITS: bits SHR shift;

# define nibble opertors for left/right shift and append #
OP << = (NIBBLE nibble, INT shift)NIBBLE:
  (width OF nibble + shift, bits OF nibble << shift);

OP >> = (NIBBLE nibble, INT shift)NIBBLE:
  (width OF nibble - shift, bits OF nibble >> shift);

OP +:= = (REF NIBBLE lhs, NIBBLE rhs)REF NIBBLE: (
  BITS rhs mask := BIN(ABS(2r1 << width OF rhs)-1);
  lhs := ( width OF lhs + width OF rhs, bits OF lhs << width OF rhs OR bits OF rhs AND rhs mask)
);

# define MODEs for generating NIBBLE streams and yielding NIBBLEs #
MODE YIELDNIBBLE = PROC(NIBBLE)VOID;
MODE GENNIBBLE = PROC(YIELDNIBBLE)VOID;

PROC gen resize nibble = (
  INT out width,
  GENNIBBLE gen nibble,
  YIELDNIBBLE yield
)VOID:(
     NIBBLE buf := (0, 2r0), out;
     BITS out mask := BIN(ABS(2r1 << out width)-1);
# FOR NIBBLE nibble IN # gen nibble( # ) DO #
##   (NIBBLE in nibble)VOID:(
    buf +:= in nibble;
    WHILE width OF buf >= out width DO
      out := buf >> ( width OF buf - out width);
      width OF buf -:= out width; # trim 'out' from buf #
      yield((out width, bits OF out AND out mask))
    OD
# OD # ))
);

# Routines for joining strings and generating a stream of nibbles #

PROC gen nibble from 7bit chars = (STRING string, YIELDNIBBLE yield)VOID:
  FOR key FROM LWB string TO UPB string DO yield((7, BIN ABS string[key])) OD;

PROC gen nibble from 8bit chars = (STRING string, YIELDNIBBLE yield)VOID:
  FOR key FROM LWB string TO UPB string DO yield((8,BIN ABS string[key])) OD;

PROC gen join = ([]STRING strings, STRING new line, YIELDNIBBLE yield)VOID:
   FOR key FROM LWB strings TO UPB strings DO
     gen nibble from 8bit chars(strings[key]+new line, yield)
   OD;

# Two tables for uuencoding 6bits in printable ASCII chacters #

[0:63]CHAR encode uue 6bit:= # [0:63] => CHAR64 #
  "`!""#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_"[@0];

[0:255]BITS decode uue 6bit; # CHAR64 => [0:63] #
  FOR key FROM LWB encode uue 6bit TO UPB encode uue 6bit DO
    decode uue 6bit[ABS encode uue 6bit[key]] := BIN key
  OD;
  decode uue 6bit[ABS " "] := 2r0; # extra #

# Some basic examples #

PROC example uudecode nibble stream = VOID:(

  []STRING encoded uue 6bit hello world = (
    ":&5L;&\L('=O<FQD""DAE;&QO+""!W;W)L9""$*1V]O9&)Y92P@8W)U96P@=V]R",
    ";&0*22=M(&QE879I;F<@>6]U('1O9&%Y""D=O;V1B>64L(&=O;V1B>64L(&=O",
    ";V1B>64@""@``"
  );

  PROC gen join hello world = (YIELDNIBBLE yield)VOID:
  # FOR NIBBLE nibble IN # gen join(encoded uue 6bit hello world, "", # ) DO #
  ##   (NIBBLE nibble)VOID:(
    yield((6, decode uue 6bit[ABS bits OF nibble]))
  # OD # ));

  print(("Decode uue 6bit NIBBLEs into 8bit CHARs:", new line));

# FOR NIBBLE nibble IN # gen resize nibble(8, gen join hello world, # ) DO ( #
##   (NIBBLE nibble)VOID:(
    print(REPR ABS bits OF nibble)
# OD # ))
);

PROC example uuencode nibble stream = VOID: (
  []STRING hello world = (
    "hello, world",
    "Hello, world!",
    "Goodbye, cruel world",
    "I'm leaving you today",
    "Goodbye, goodbye, goodbye "
  );

  PROC gen join hello world = (YIELDNIBBLE yield)VOID:
    gen join(hello world, REPR ABS 8r12, yield); # 8r12 = ASCII new line #

  print((new line, "Encode 8bit CHARs into uue 6bit NIBBLEs:", new line));
  INT count := 0;
# FOR NIBBLE nibble IN # gen resize nibble(6, gen join hello world, # ) DO ( #
##   (NIBBLE nibble)VOID:(
    print(encode uue 6bit[ABS bits OF nibble]);
    count+:=1;
    IF count MOD 60 = 0 THEN print(newline) FI
# OD # ));
    print(new line); print(new line)
);

PROC example compress 7bit chars = VOID: (
  STRING example 7bit string = "STRING & ABACUS";

  print(("Convert 7bit ASCII CHARS to a 1bit stream: ",new line,
          example 7bit string + " => "));

  PROC gen example 7bit string = (YIELDNIBBLE yield)VOID:
    gen nibble from 7bit chars(example 7bit string,yield);

# FOR NIBBLE nibble IN # gen resize nibble(1, gen example 7bit string, # ) DO ( #
##   (NIBBLE nibble)VOID: (
    print(whole(ABS bits OF nibble,0))
# OD # ));
  print(new line)
);

example uudecode nibble stream;
example uuencode nibble stream;
example compress 7bit chars
```

{{out}}

```txt

Decode uue 6bit NIBBLEs into 8bit CHARs:
hello, world
Hello, world!
Goodbye, cruel world
I'm leaving you today
Goodbye, goodbye, goodbye

Encode 8bit CHARs into uue 6bit NIBBLEs:
:&5L;&\L('=O<FQD"DAE;&QO+"!W;W)L9"$*1V]O9&)Y92P@8W)U96P@=V]R
;&0*22=M(&QE879I;F<@>6]U('1O9&%Y"D=O;V1B>64L(&=O;V1B>64L(&=O
;V1B>64@"

Convert 7bit ASCII CHARS to a 1bit stream:
STRING & ABACUS => 101001110101001010010100100110011101000111010000001001100100000100000110000101000001100001110101011010011

```



## AutoHotkey


```autohotkey
file = %A_WorkingDir%\z.dat
IfExist, %A_WorkingDir%\z.dat
	FileDelete %file%
IfNotEqual ErrorLevel,0, MsgBox Can't delete file "%file%"`nErrorLevel = "%ErrorLevel%"

res := BinWrite(file,"000102030405060708090a0b0c0d0e0f00")
MsgBox ErrorLevel = %ErrorLevel%`nBytes Written = %res%
res := BinRead(file,data)
MsgBox ErrorLevel = %ErrorLevel%`nBytes Read = %res%`nData = "%data%"

res := BinWrite(file,"aa00bb",0,2)
MsgBox ErrorLevel = %ErrorLevel%`nBytes Written = %res%
res := BinRead(file,data)
MsgBox ErrorLevel = %ErrorLevel%`nBytes Read = %res%`nData = "%data%"

res := BinRead(file,data,3,-2)
MsgBox ErrorLevel = %ErrorLevel%`nBytes Read = %res%`nData = "%data%"
ExitApp

/* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BinWrite ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|  - Open binary file
|  - (Over)Write n bytes (n = 0: all)
|  - From offset (offset < 0: counted from end)
|  - Close file
|  data -> file[offset + 0..n-1], rest of file unchanged
|  Return #bytes actually written
*/ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BinWrite(file, data, n=0, offset=0)
{
   ; Open file for WRITE (0x40..), OPEN_ALWAYS (4): creates only if it does not exists
   h := DllCall("CreateFile","str",file,"Uint",0x40000000,"Uint",0,"UInt",0,"UInt",4,"Uint",0,"UInt",0)
   IfEqual h,-1, SetEnv, ErrorLevel, -1
   IfNotEqual ErrorLevel,0,Return,0 ; couldn't create the file

   m = 0                            ; seek to offset
   IfLess offset,0, SetEnv,m,2
   r := DllCall("SetFilePointerEx","Uint",h,"Int64",offset,"UInt *",p,"Int",m)
   IfEqual r,0, SetEnv, ErrorLevel, -3
   IfNotEqual ErrorLevel,0, {
      t = %ErrorLevel%              ; save ErrorLevel to be returned
      DllCall("CloseHandle", "Uint", h)
      ErrorLevel = %t%              ; return seek error
      Return 0
   }

   TotalWritten = 0
   m := Ceil(StrLen(data)/2)
   If (n <= 0 or n > m)
       n := m
   Loop %n%
   {
      StringLeft c, data, 2         ; extract next byte
      StringTrimLeft data, data, 2  ; remove  used byte
      c = 0x%c%                     ; make it number
      result := DllCall("WriteFile","UInt",h,"UChar *",c,"UInt",1,"UInt *",Written,"UInt",0)
      TotalWritten += Written       ; count written
      if (!result or Written < 1 or ErrorLevel)
         break
   }

   IfNotEqual ErrorLevel,0, SetEnv,t,%ErrorLevel%

   h := DllCall("CloseHandle", "Uint", h)
   IfEqual h,-1, SetEnv, ErrorLevel, -2
   IfNotEqual t,,SetEnv, ErrorLevel, %t%

   Return TotalWritten
}

/* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BinRead ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|  - Open binary file
|  - Read n bytes (n = 0: all)
|  - From offset (offset < 0: counted from end)
|  - Close file
|  data (replaced) <- file[offset + 0..n-1]
|  Return #bytes actually read
*/ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BinRead(file, ByRef data, n=0, offset=0)
{
   h := DllCall("CreateFile","Str",file,"Uint",0x80000000,"Uint",3,"UInt",0,"UInt",3,"Uint",0,"UInt",0)
   IfEqual h,-1, SetEnv, ErrorLevel, -1
   IfNotEqual ErrorLevel,0,Return,0 ; couldn't open the file

   m = 0                            ; seek to offset
   IfLess offset,0, SetEnv,m,2
   r := DllCall("SetFilePointerEx","Uint",h,"Int64",offset,"UInt *",p,"Int",m)
   IfEqual r,0, SetEnv, ErrorLevel, -3
   IfNotEqual ErrorLevel,0, {
      t = %ErrorLevel%              ; save ErrorLevel to be returned
      DllCall("CloseHandle", "Uint", h)
      ErrorLevel = %t%              ; return seek error
      Return 0
   }

   TotalRead = 0
   data =
   IfEqual n,0, SetEnv n,0xffffffff ; almost infinite

   format = %A_FormatInteger%       ; save original integer format
   SetFormat Integer, Hex           ; for converting bytes to hex

   Loop %n%
   {
      result := DllCall("ReadFile","UInt",h,"UChar *",c,"UInt",1,"UInt *",Read,"UInt",0)
      if (!result or Read < 1 or ErrorLevel)
         break
      TotalRead += Read             ; count read
      c += 0                        ; convert to hex
      StringTrimLeft c, c, 2        ; remove 0x
      c = 0%c%                      ; pad left with 0
      StringRight c, c, 2           ; always 2 digits
      data = %data%%c%              ; append 2 hex digits
   }

   IfNotEqual ErrorLevel,0, SetEnv,t,%ErrorLevel%

   h := DllCall("CloseHandle", "Uint", h)
   IfEqual h,-1, SetEnv, ErrorLevel, -2
   IfNotEqual t,,SetEnv, ErrorLevel, %t%

   SetFormat Integer, %format%      ; restore original format
   Totalread += 0                   ; convert to original format
   Return TotalRead
}
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      file$ = @tmp$ + "bitwise.tmp"
      test$ = "Hello, world!"

      REM Write to file, 7 bits per character:
      file% = OPENOUT(file$)
      FOR i% = 1 TO LEN(test$)
        PROCwritebits(file%, ASCMID$(test$,i%), 7)
      NEXT
      PROCwritebits(file%, 0, 0)
      CLOSE #file%

      REM Read from file, 7 bits per character:
      file% = OPENIN(file$)
      REPEAT
        ch% = FNreadbits(file%, 7)
        VDU ch%
      UNTIL ch% = 0
      PRINT
      CLOSE #file%
      END

      REM Write n% bits from b% to file f% (n% = 0 to flush):
      DEF PROCwritebits(f%, b%, n%)
      PRIVATE a%, c%
      IF n% = 0 BPUT #f%,a% : a% = 0 : c% = 0
      WHILE n%
        IF c% = 8 BPUT #f%,a% : a% = 0 : c% = 0
        n% -= 1
        c% += 1
        IF b% AND 1 << n% THEN a% OR= 1 << (8 - c%)
      ENDWHILE
      ENDPROC

      REM Read n% bits from file f%:
      DEF FNreadbits(f%, n%)
      PRIVATE a%, c% : LOCAL v%
      WHILE n%
        IF c% = 0 a% = BGET#f% : c% = 8
        n% -= 1
        c% -= 1
        v% = v% << 1 OR (a% >> c%) AND 1
      ENDWHILE
      = v%
```

{{out}}

```txt

Hello, world!

```



## C

MSB in a byte is considered the "first" bit.  Read and write methods somewhat mimic fread and fwrite, though there's no fflush-like function because flushing bits into a file is ill-defined (this whole task is pretty ill-defined).  Only way to make sure all bits are written to the file is by detaching the bit filter, just like how closing a file flushes out buffer. There's no limit on read/write size, but caller should ensure the buffer is large enough.

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef uint8_t byte;
typedef struct {
	FILE *fp;
	uint32_t accu;
	int bits;
} bit_io_t, *bit_filter;

bit_filter b_attach(FILE *f)
{
	bit_filter b = malloc(sizeof(bit_io_t));
	b->bits = b->accu = 0;
	b->fp = f;
	return b;
}

void b_write(byte *buf, size_t n_bits, size_t shift, bit_filter bf)
{
	uint32_t accu = bf->accu;
	int bits = bf->bits;

	buf += shift / 8;
	shift %= 8;

	while (n_bits || bits >= 8) {
		while (bits >= 8) {
			bits -= 8;
			fputc(accu >> bits, bf->fp);
			accu &= (1 << bits) - 1;
		}
		while (bits < 8 && n_bits) {
			accu = (accu << 1) | (((128 >> shift) & *buf) >> (7 - shift));
			--n_bits;
			bits++;
			if (++shift == 8) {
				shift = 0;
				buf++;
			}
		}
	}
	bf->accu = accu;
	bf->bits = bits;
}

size_t b_read(byte *buf, size_t n_bits, size_t shift, bit_filter bf)
{
	uint32_t accu = bf->accu;
	int bits = bf->bits;
	int mask, i = 0;

	buf += shift / 8;
	shift %= 8;

	while (n_bits) {
		while (bits && n_bits) {
			mask = 128 >> shift;
			if (accu & (1 << (bits - 1))) *buf |= mask;
			else *buf &= ~mask;

			n_bits--;
			bits--;

			if (++shift >= 8) {
				shift = 0;
				buf++;
			}
		}
		if (!n_bits) break;
		accu = (accu << 8) | fgetc(bf->fp);
		bits += 8;
	}
	bf->accu = accu;
	bf->bits = bits;

	return i;
}

void b_detach(bit_filter bf)
{
	if (bf->bits) {
		bf->accu <<= 8 - bf->bits;
		fputc(bf->accu, bf->fp);
	}
	free(bf);
}

int main()
{
	unsigned char s[] = "abcdefghijk";
	unsigned char s2[11] = {0};
	int i;

	FILE *f = fopen("test.bin", "wb");
	bit_filter b = b_attach(f);
	/* for each byte in s, write 7 bits skipping 1 */
	for (i = 0; i < 10; i++) b_write(s + i, 7, 1, b);
	b_detach(b);
	fclose(f);

	/* read 7 bits and expand to each byte of s2 skipping 1 bit */
	f = fopen("test.bin", "rb");
	b = b_attach(f);
	for (i = 0; i < 10; i++) b_read(s2 + i, 7, 1, b);
	b_detach(b);
	fclose(f);

	printf("%10s\n", s2); /* should be the same first 10 bytes as in s */

	return 0;
}
```



## Common Lisp


Common Lisp already has tonnes of bitwise-I/O functionality (remember, folks have written operating systems in Lisp …); in particular, READ-SEQUENCE and WRITE-SEQUENCE neatly handle dumping (VECTOR (UNSIGNED-BYTE 8)) objects directly to/from I/O streams with :ELEMENT-TYPE (UNSIGNED-BYTE 8). This is a fairly robust but not very optimized toolkit that shows off changing between vectors of bytes, vectors of characters, and bit-vectors in a few ways.


```lisp

(defpackage :rosetta.bitwise-i/o
  (:use :common-lisp)
  (:export :bitwise-i/o-demo))
(in-package :rosetta.bitwise-i/o)

(defun byte->bit-vector (byte byte-bits)
  "Convert one BYTE into a bit-vector of BYTE-BITS length."
  (let ((vector (make-array byte-bits :element-type 'bit))
        (bit-value 1))
    (declare (optimize (speed 3)))
    (dotimes (bit-index byte-bits vector)
      (setf (aref vector bit-index)
            (if (plusp (logand byte (the (unsigned-byte 8) bit-value)))
                1 0))
      (setq bit-value (ash bit-value 1)))))

(defun bytes->bit-vector (byte-vector byte-bits)
  "Convert a BYTE-VECTOR into a bit-vector, with each byte taking BYTE-BITS.

For  optimization's sake,  I  limit the  size of  the  vector to  (FLOOR
MOST-POSITIVE-FIXNUM  BYTE-BITS), which  is somewhat  ridiculously long,
but allows the compiler to trust that indices will fit in a FIXNUM."
  (reduce (lambda (a b) (concatenate 'bit-vector a b))
          (map 'list (lambda (byte) (byte->bit-vector byte byte-bits)) byte-vector)))

(defun ascii-char-p (char)
  "True if CHAR is an ASCII character"
  (< (char-code char) #x80))

(defun assert-ascii-string (string)
  "`ASSERT' that STRING is an ASCII string."
  (assert (every #'ascii-char-p string)
          (string)
          "STRING must contain only ASCII (7-bit) characters;~%“~a”
…contains non-ASCII character~p~:*: ~{~% • ~c ~:*— ~@c ~}"
          string (coerce (remove-duplicates (remove-if #'ascii-char-p string)
                                            :test #'char=)
                         'list)))

(defun ascii-string->bit-vector (string)
  "Convert a STRING consisting only  of characters in the ASCII \(7-bit)
range into a bit-vector of 7 bits per character.

This assumes \(as is now, in  2017, I believe universally the case) that
the local character code system \(as for `CHAR-CODE' and `CODE-CHAR') is
Unicode, or at least, a superset of ASCII \(eg: ISO-8859-*)
"
  (check-type string simple-string)
  (assert-ascii-string string)
  (bytes->bit-vector (map 'vector #'char-code string) 7))

(defun pad-bit-vector-to-8 (vector)
  "Ensure that VECTOR is a multiple of 8 bits in length."
  (adjust-array vector (* 8 (ceiling (length vector) 8))))

(defun bit-vector->byte (vector)
  "Convert VECTOR into a single byte."
  (declare (optimize (speed 3)))
  (check-type vector bit-vector)
  (assert (<= (length vector) 8))
  (reduce (lambda (x y)
            (logior (the (unsigned-byte 8)
                         (ash (the (unsigned-byte 8) x) 1))
                    (the bit y)))
          (reverse vector) :initial-value 0))

(defun bit-vector->bytes (vector byte-size &key (truncatep nil))
  "Convert a bit vector VECTOR into a vector of bytes of BYTE-SIZE bits each.

If TRUNCATEP, then discard any trailing bits."
  (let* ((out-length (funcall (if truncatep 'floor 'ceiling)
                              (length vector)
                              byte-size))
         (output (make-array out-length
                             :element-type (list 'unsigned-byte byte-size))))
    (loop for byte from 0 below out-length
          for start-bit = 0 then end-bit
          for end-bit = byte-size then (min (+ byte-size end-bit)
                                            (length vector))
          do (setf (aref output byte)
                   (bit-vector->byte (subseq vector start-bit end-bit))))
    output))

(defun ascii-pack-to-8-bit (string)
  "Pack an ASCII STRING into 8-bit bytes (7→8 bit packing)"
  (bit-vector->bytes (ascii-string->bit-vector string)
                     8))

(defun unpack-ascii-from-8-bits (byte-vector)
  "Convert an 8-bit BYTE-VECTOR into an array of (unpacked) 7-bit bytes."
  (map 'string #'code-char
       (bit-vector->bytes
        (pad-bit-vector-to-8 (bytes->bit-vector byte-vector 8))
        7
        :truncatep t)))

(defun write-7->8-bit-string-to-file (string pathname)
  "Given a string of 7-bit character STRING, create a new file at PATHNAME
with the contents of that string packed into 8-bit bytes."
  (format *trace-output* "~&Writing string to ~a in packed 7→8 bits…~%“~a”"
          pathname string)
  (assert-ascii-string string)
  (with-open-file (output pathname
                          :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-sequence (ascii-pack-to-8-bit string) output)
    (finish-output output)
    (let ((expected-length (ceiling (* (length string) 7) 8)))
      (assert (= (file-length output) expected-length) ()
              "The file written was ~:d byte~:p in length, ~
but the string supplied should have written ~:d byte~:p."
              (file-length output) expected-length))))

(defun read-file-into-byte-array (pathname)
  "Read a binary file into a byte array"
  (with-open-file (input pathname
                         :direction :input
                         :if-does-not-exist :error
                         :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length input)
                              :element-type '(unsigned-byte 8))))
      (read-sequence buffer input)
      buffer)))

(defun read-8->7-bit-string-from-file (pathname)
  "Read   8-bit   packed  data   from   PATHNAME   and  return   it   as
a 7-bit string."
  (unpack-ascii-from-8-bits (read-file-into-byte-array pathname)))

(defun bitwise-i/o-demo (&key (string "Hello, World.")
                              (pathname #p"/tmp/demo.bin"))
  "Writes STRING  to PATHNAME after  7→8 bit  packing, then reads  it back
to validate."
  (write-7->8-bit-string-to-file string pathname)
  (let ((read-back (read-8->7-bit-string-from-file pathname)))
    (assert (equal string read-back) ()
            "Reading back string got:~%“~a”~%…expected:~%“~a”" read-back string)
    (format *trace-output* "~&String read back matches:~%“~a”" read-back))
  (finish-output *trace-output*))


```

{{out}}

```lisp

BITWISE-I/O> (bitwise-i/o-demo)

Writing string to /tmp/demo.bin in packed 7→8 bits…
“Hello, World.”
String read back matches:
“Hello, World.”
NIL
BITWISE-I/O> (bitwise-i/o-demo :string "It doesn't, however, do UTF-7. So, no ☠ or 🙋")
Writing string to /tmp/demo.bin in packed 7→8 bits…
“It doesn't, however, do UTF-7. So, no ☠ or 🙋”

STRING must contain only ASCII (7-bit) characters;
“It doesn't, however, do UTF-7. So, no ☠ or 🙋”
…contains non-ASCII characters:
 • ☠ — #\SKULL_AND_CROSSBONES
 • 🙋 — #\HAPPY_PERSON_RAISING_ONE_HAND
   [Condition of type SIMPLE-ERROR]


Restarts:
 0: [CONTINUE] Retry assertion with new value for STRING.
 1: [RETRY] Retry SLIME REPL evaluation request.
 2: [*ABORT] Return to SLIME's top level.
 3: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {10152E8033}>)

⇒ ABORT

```


## C#

```c#
using System;
using System.IO;

public class BitReader
{
    uint readData = 0;
    int startPosition = 0;
    int endPosition = 0;

    public int InBuffer
    {
        get { return endPosition - startPosition; }
    }

    private Stream stream;

    public Stream BaseStream
    {
        get { return stream; }
    }

    public BitReader(Stream stream)
    {
        this.stream = stream;
    }

    void EnsureData(int bitCount)
    {
        int readBits = bitCount - InBuffer;
        while (readBits > 0)
        {
            int b = BaseStream.ReadByte();

            if (b < 0) throw new InvalidOperationException("Unexpected end of stream");

            readData |= checked((uint)b << endPosition);
            endPosition += 8;
            readBits -= 8;
        }
    }

    public bool ReadBit()
    {
        return Read(1) > 0;
    }

    public int Read(int bitCount)
    {
        EnsureData(bitCount);

        int result = (int)(readData >> startPosition) & ((1 << bitCount) - 1);
        startPosition += bitCount;
        if (endPosition == startPosition)
        {
            endPosition = startPosition = 0;
            readData = 0;
        }
        else if (startPosition >= 8)
        {
            readData >>= startPosition;
            endPosition -= startPosition;
            startPosition = 0;
        }

        return result;
    }

    public void Align()
    {
        endPosition = startPosition = 0;
        readData = 0;
    }
}

public class BitWriter
{
    uint data = 0;
    int dataLength = 0;
    Stream stream;

    public Stream BaseStream
    {
        get { return stream; }
    }

    public int BitsToAligment
    {
        get { return (32 - dataLength) % 8; }
    }

    public BitWriter(Stream stream)
    {
        this.stream = stream;
    }

    public void WriteBit(bool value)
    {
        Write(value ? 1 : 0, 1);
    }

    public void Write(int value, int length)
    {
        uint currentData = data | checked((uint)value << dataLength);
        int currentLength = dataLength + length;
        while (currentLength >= 8)
        {
            BaseStream.WriteByte((byte)currentData);
            currentData >>= 8;
            currentLength -= 8;
        }
        data = currentData;
        dataLength = currentLength;
    }

    public void Align()
    {
        if (dataLength > 0)
        {
            BaseStream.WriteByte((byte)data);

            data = 0;
            dataLength = 0;
        }
    }
}

class Program
{
    static void Main(string[] args)
    {
        MemoryStream ms = new MemoryStream();
        BitWriter writer = new BitWriter(ms);
        writer.WriteBit(true);
        writer.Write(5, 3);
        writer.Write(0x0155, 11);
        writer.Align();

        ms.Position = 0;
        BitReader reader = new BitReader(ms);
        Console.WriteLine(reader.ReadBit());
        Console.WriteLine(reader.Read(3));
        Console.WriteLine(reader.Read(11).ToString("x4"));
        reader.Align();
    }
}
```



## D

{{trans|C}}

```d
import std.stdio: File;
import core.stdc.stdio: FILE, fputc, fgetc;
import std.string: representation;

/***********
Bitwise I/O, the file must be in binary mode, and its FILE*
must be kept open during the usage of BitwiseFile.
*/
struct BitwiseFile {
    FILE* fp;
    uint accu;
    int bits;

    this(File f)
    in {
        assert(f.isOpen);
        //assert(f.isBinary);
    } body {
        this.fp = f.getFP();
    }

    void write(const(ubyte)[] buf, size_t nBits, size_t shift)
    nothrow {
        auto accu = this.accu;
        auto bits = this.bits;
        auto bufPtr = buf.ptr;

        bufPtr += shift / 8;
        shift %= 8;

        while (nBits || bits >= 8) {
            while (bits >= 8) {
                bits -= 8;
                fputc(accu >> bits, this.fp);
                accu &= (1 << bits) - 1;
            }
            while (bits < 8 && nBits) {
                accu = (accu << 1) |
                       (((128 >> shift) & *bufPtr) >> (7 - shift));
                nBits--;
                bits++;
                shift++;
                if (shift == 8) {
                    shift = 0;
                    bufPtr++;
                }
            }
        }

        this.accu = accu;
        this.bits = bits;
    }

    size_t read(ubyte[] buf, size_t nBits, size_t shift) nothrow {
        auto accu = this.accu;
        auto bits = this.bits;
        auto bufPtr = buf.ptr;
        int i = 0;

        bufPtr += shift / 8;
        shift %= 8;

        while (nBits) {
            while (bits && nBits) {
                immutable mask = 128u >> shift;
                if (accu & (1 << (bits - 1)))
                    *bufPtr |= mask;
                else
                    *bufPtr &= ~mask;

                nBits--;
                bits--;
                shift++;

                if (shift >= 8) {
                    shift = 0;
                    bufPtr++;
                }
            }
            if (!nBits)
                break;
            accu = (accu << 8) | fgetc(this.fp);
            bits += 8;
        }

        this.accu = accu;
        this.bits = bits;
        return i;
    }

    void detach() nothrow {
        if (this.bits) {
            this.accu <<= 8 - this.bits;
            fputc(this.accu, this.fp);
        }
        this.fp = null;
        this.accu = 0;
        this.bits = 0;
    }

    nothrow ~this() {
        detach;
    }
}

void main() { // Demo code.
    import core.stdc.stdio: fopen, fclose;
    import std.string: assumeUTF;
    import std.stdio: writeln;

    immutable data = "abcdefghijk".representation;
    enum n = data.length;

    // For each ubyte in data, write 7 bits skipping 1.
    auto fout = File("bitwise_io_test.bin", "wb");
    auto bf1 = BitwiseFile(fout);
    foreach (immutable i; 0 .. n)
        bf1.write(data[i .. $], 7, 1);
    bf1.detach();
    fout.close();

    // Read 7 bits and expand to each ubyte of result skipping 1 bit.
    ubyte[n + 1] result = '\0';
    auto fin = File("bitwise_io_test.bin", "rb");
    auto bf2 = BitwiseFile(fin);
    foreach (immutable i; 0 .. n)
        bf2.read(result[i .. $], 7, 1);
    bf2.detach();
    fin.close();

    // Should be the same chars as 'data'.
    result.assumeUTF.writeln;
}
```

{{out}}
  abcdefghijk


## Erlang


```erlang
-module(bitwise_io).
-compile([export_all]).

%% Erlang allows for easy manipulation of bitstrings. Here I'll
%% present a function that can take a message of 8-bit ASCII
%% characters and remove the MSB, leaving the same message in 7-bits.

compress(Message) ->
    << <<X:7>> || <<X:8>> <= Message >>.

%% Here we decompress the message.

decompress(Message) ->
    << <<X:8>> || <<X:7>> <= Message >>.

%% Now a test to demonstrate that this conversion works:

test_bitstring_conversion() ->
    Message = <<"Hello, Rosetta Code!">>,
    io:format("~p: ~B~n",[Message, bit_size(Message)]),
    Compressed = compress(Message),
    io:format("~p: ~B~n",[Compressed, bit_size(Compressed)]),
    Decompressed = decompress(Compressed),
    io:format("~p: ~B~n",[Decompressed, bit_size(Decompressed)]),
    io:format("~p = ~p ? ~p~n",[Message, Decompressed,
                                Message =:= Decompressed]).

%% Now to show this on file output, we'll write the compressed version
%% to a file. Now, erlang's file:write_file expects objects that are
%% multiples of 8bits. We'll add padding to allow the writing to
%% complete, and then discard the padding when reading the file back
%% in.

test_file_io() ->
    Message = <<"Hello, Rosetta Code!">>,
    FileName = "bitwise_io.dat",
    Compressed = compress(Message),
    PaddingSize = (8 - (bit_size(Compressed) rem 8)) rem 8,
    PaddedCompressed = <<Compressed:(bit_size(Compressed))/bitstring,
                         0:PaddingSize>>,
    file:write_file(FileName,PaddedCompressed),
    {ok, ReadIn} = file:read_file(FileName),
    UnpaddedSize = bit_size(ReadIn) - 7,
    Unpadded =
        case UnpaddedSize rem 7 =:= 0 of
            true ->
                <<T:(UnpaddedSize)/bitstring,_:7>> = ReadIn,
                T;
            false ->
                << <<X:7>> || <<X:7>> <= ReadIn >>
        end,
    Decompressed = decompress(Unpadded),
    io:format("~p~n",[Decompressed]).
```

{{out}}

```erlang>184
 bitwise_io:test_bitstring_conversion().
<<"Hello, Rosetta Code!">>: 160
<<145,151,102,205,235,16,82,223,207,47,78,152,80,67,223,147,42,1:4>>: 140
<<"Hello, Rosetta Code!">>: 160
<<"Hello, Rosetta Code!">> = <<"Hello, Rosetta Code!">> ? true
ok
185> bitwise_io:test_file_io().
<<"Hello, Rosetta Code!">>
```



## Forth

The stream status is kept on the stack ( b m ), where b is the character accumulator
and m is a mask for the current bit.  The accumulator is filled with bits starting
with the MSB. (The writing code was originally used for Mandelbrot generation.)


```forth
\ writing

: init-write ( -- b m ) 0 128 ;

: flush-bits ( b m -- 0 128 ) drop emit init-write ;

: ?flush-bits ( b m -- b' m' ) dup 128 < if flush-bits then ;

: write-bit ( b m f -- b' m' )
  if tuck or swap then
  2/ dup 0= if flush-bits then ;

\ reading

: init-read ( -- b m ) key 128 ;

: eof? ( b m -- b m f ) dup if false else key? 0= then ;

: read-bit ( b m -- b' m' f )
  dup 0= if 2drop init-read then
  2dup and swap 2/ swap ;
```



## Go

Based on the
<code>[https://golang.org/pkg/compress/lzw compress/lzw]</code>
encoder and decoder internal types
(with LZW specific stuff trimmed).

```Go
// Package bit provides bit-wise IO to an io.Writer and from an io.Reader.
package bit

import (
	"bufio"
	"errors"
	"io"
)

// Order specifies the bit ordering within a byte stream.
type Order int

const (
	// LSB is for Least Significant Bits first
	LSB Order = iota
	// MSB is for Most  Significant Bits first
	MSB
)

//
### = Writing / Encoding =


type writer interface {
	io.ByteWriter
	Flush() error
}

// Writer implements bit-wise writing to an io.Writer.
type Writer struct {
	w     writer
	order Order
	write func(uint32, uint) error // writeLSB or writeMSB
	bits  uint32
	nBits uint
	err   error
}

// writeLSB writes `width` bits of `c` in LSB order.
func (w *Writer) writeLSB(c uint32, width uint) error {
	w.bits |= c << w.nBits
	w.nBits += width
	for w.nBits >= 8 {
		if err := w.w.WriteByte(uint8(w.bits)); err != nil {
			return err
		}
		w.bits >>= 8
		w.nBits -= 8
	}
	return nil
}

// writeMSB writes `width` bits of `c` in MSB order.
func (w *Writer) writeMSB(c uint32, width uint) error {
	w.bits |= c << (32 - width - w.nBits)
	w.nBits += width
	for w.nBits >= 8 {
		if err := w.w.WriteByte(uint8(w.bits >> 24)); err != nil {
			return err
		}
		w.bits <<= 8
		w.nBits -= 8
	}
	return nil
}

// WriteBits writes up to 16 bits of `c` to the underlying writer.
// Even for MSB ordering the bits are taken from the lower bits of `c`.
// (e.g. WriteBits(0x0f,4) writes four 1 bits).
func (w *Writer) WriteBits(c uint16, width uint) error {
	if w.err == nil {
		w.err = w.write(uint32(c), width)
	}
	return w.err
}

var errClosed = errors.New("bit reader/writer is closed")

// Close closes the writer, flushing any pending output.
// It does not close the underlying writer.
func (w *Writer) Close() error {
	if w.err != nil {
		if w.err == errClosed {
			return nil
		}
		return w.err
	}
	// Write the final bits (zero padded).
	if w.nBits > 0 {
		if w.order == MSB {
			w.bits >>= 24
		}
		if w.err = w.w.WriteByte(uint8(w.bits)); w.err != nil {
			return w.err
		}
	}
	w.err = w.w.Flush()
	if w.err != nil {
		return w.err
	}

	// Make any future calls to Write return errClosed.
	w.err = errClosed
	return nil
}

// NewWriter returns a new bit Writer that writes completed bytes to `w`.
func NewWriter(w io.Writer, order Order) *Writer {
	bw := &Writer{order: order}
	switch order {
	case LSB:
		bw.write = bw.writeLSB
	case MSB:
		bw.write = bw.writeMSB
	default:
		bw.err = errors.New("bit writer: unknown order")
		return bw
	}
	if byteWriter, ok := w.(writer); ok {
		bw.w = byteWriter
	} else {
		bw.w = bufio.NewWriter(w)
	}
	return bw
}

//
### = Reading / Decoding =


// Reader implements bit-wise reading from an io.Reader.
type Reader struct {
	r     io.ByteReader
	bits  uint32
	nBits uint
	read  func(width uint) (uint16, error) // readLSB or readMSB
	err   error
}

func (r *Reader) readLSB(width uint) (uint16, error) {
	for r.nBits < width {
		x, err := r.r.ReadByte()
		if err != nil {
			return 0, err
		}
		r.bits |= uint32(x) << r.nBits
		r.nBits += 8
	}
	bits := uint16(r.bits & (1<<width - 1))
	r.bits >>= width
	r.nBits -= width
	return bits, nil
}

func (r *Reader) readMSB(width uint) (uint16, error) {
	for r.nBits < width {
		x, err := r.r.ReadByte()
		if err != nil {
			return 0, err
		}
		r.bits |= uint32(x) << (24 - r.nBits)
		r.nBits += 8
	}
	bits := uint16(r.bits >> (32 - width))
	r.bits <<= width
	r.nBits -= width
	return bits, nil
}

// ReadBits reads up to 16 bits from the underlying reader.
func (r *Reader) ReadBits(width uint) (uint16, error) {
	var bits uint16
	if r.err == nil {
		bits, r.err = r.read(width)
	}
	return bits, r.err
}

// Close closes the reader.
// It does not close the underlying reader.
func (r *Reader) Close() error {
	if r.err != nil && r.err != errClosed {
		return r.err
	}
	r.err = errClosed
	return nil
}

// NewReader returns a new bit Reader that reads bytes from `r`.
func NewReader(r io.Reader, order Order) *Reader {
	br := new(Reader)
	switch order {
	case LSB:
		br.read = br.readLSB
	case MSB:
		br.read = br.readMSB
	default:
		br.err = errors.New("bit writer: unknown order")
		return br
	}
	if byteReader, ok := r.(io.ByteReader); ok {
		br.r = byteReader
	} else {
		br.r = bufio.NewReader(r)
	}
	return br
}
```

And a test file (such as <code>bit_test.go</code>):

```Go
package bit

import (
	"bytes"
	"fmt"
	"io"
	"log"
)

func ExampleWriter_WriteBits() {
	var buf bytes.Buffer
	bw := NewWriter(&buf, MSB)
	bw.WriteBits(0x0f, 4) // Writes 1111
	bw.WriteBits(0x00, 1) //        0
	bw.WriteBits(0x13, 5) //        1001 1
	// Close will flush with zero bits, in this case
	//                              0000 00
	if err := bw.Close(); err != nil {
		log.Fatal(err)
	}
	fmt.Printf("%08b", buf.Bytes())
	// Output:
	// [11110100 11000000]
}

func Example() {
	const message = "This is a test."
	fmt.Printf("%q as bytes: % 02[1]X\n", message, []byte(message))
	fmt.Printf("    original bits: %08b\n", []byte(message))

	// Re-write in 7 bit chunks to buf:
	var buf bytes.Buffer
	bw := NewWriter(&buf, MSB)
	for _, r := range message {
		bw.WriteBits(uint16(r), 7) // nolint: errcheck
	}
	if err := bw.Close(); err != nil {
		log.Fatal(err)
	}
	fmt.Printf("Written bitstream: %08b\n", buf.Bytes())
	fmt.Printf("Written bytes: % 02X\n", buf.Bytes())

	// Read back in 7 bit chunks:
	br := NewReader(&buf, MSB)
	var result []byte
	for {
		v, err := br.ReadBits(7)
		if err != nil {
			if err != io.EOF {
				log.Fatal(err)
			}
			break
		}
		if v != 0 {
			result = append(result, byte(v))
		}
	}
	fmt.Printf("Read back as \"%s\"\n", result)
	// Output:
	// "This is a test." as bytes: 54 68 69 73 20 69 73 20 61 20 74 65 73 74 2E
	//     original bits: [01010100 01101000 01101001 01110011 00100000 01101001 01110011 00100000 01100001 00100000 01110100 01100101 01110011 01110100 00101110]
	// Written bitstream: [10101001 10100011 01001111 00110100 00011010 01111001 10100000 11000010 10000011 10100110 01011110 01111101 00010111 00000000]
	// Written bytes: A9 A3 4F 34 1A 79 A0 C2 83 A6 5E 7D 17 00
	// Read back as "This is a test."
}
```

With this test file, running <code>go test -v</code> will compile the package and run the example verifying the output is as listed above in the <code>// Output:</code> comments.
Additionally, <code>godoc -ex</code> will extract the code comments for documentation and include the examples at the appropriate place
(here the first goes with the <code>WriteBits</code> method and the later with the package documentation).


## Haskell


```haskell
import Data.List
import Data.Char
import Control.Monad
import Control.Arrow
import System.Environment

int2bin :: Int -> [Int]
int2bin = unfoldr(\x -> if x==0 then Nothing
                          else Just (uncurry(flip(,)) (divMod x 2)))

bin2int :: [Int] -> Int
bin2int = foldr ((.(2 *)).(+)) 0

bitReader = map (chr.bin2int). takeWhile(not.null). unfoldr(Just. splitAt 7)
  . (take =<< (7 *) . (`div` 7) . length)

bitWriter xs =  tt ++ z00 where
  tt = concatMap (take 7.(++repeat 0).int2bin.ord) xs
  z00 = replicate (length xs `mod` 8) 0

main = do
  (xs:_) <- getArgs
  let bits = bitWriter xs

  putStrLn "Text to compress:"
  putStrLn $ '\t' : xs
  putStrLn $ "Uncompressed text length is " ++ show (length xs)
  putStrLn $ "Compressed text has " ++ show (length bits `div` 8) ++ " bytes."
  putStrLn "Read and decompress:"
  putStrLn $ '\t' : bitReader bits
```

*  7-bits code has lsb leading.

```txt
*Main> :main ["This text is used to illustrate the Rosetta Code task 'bit oriented IO'."]
Text to compress:
        This text is used to illustrate the Rosetta Code task 'bit oriented IO'.
Uncompressed text length is 72
Compressed text has 63 bytes.
Read and decompress:
        This text is used to illustrate the Rosetta Code task 'bit oriented IO'.
```



## J

;Solution

```j
bitReader =: a. {~ _7 #.\ ({.~ <.&.(%&7)@#)
bitWriter =: , @ ((7$2) & #: @ (a.&i.)), 0 $~ 8 | #
```

;Usage
Do and undo bit oriented IO:

```j
text=: 'This text is used to illustrate the Rosetta Code task about ''bit oriented IO''.'

   bitReader bitWriter text
This text is used to illustrate the Rosetta Code task about 'bit oriented IO'.
```

Original text length:

```j
   # text
78
```

Note: '#' here counts the number of items in its right argument. (In some other languages, # marks the beginning of an in-line comment. J is not one of those languages. Also note that J's command prompt is three spaces. This makes copy&paste easier to use with previously issued commands.)

Compressed length:

```j
   %&8 # bitWriter text
69
```

Note: this implementation writes the bytes to the session (which is to say, it just gets displayed like any other result. Also, the compressed result is represented as bits - like 1 0 1 0 1...  You'll of course need other code when you want to do other things.)



## Julia

ASCII 7-bit character string compression and decompression, to demonstrate bit twiddling. Implemented as generic IO, so that file handles are usable with the same functions.

```Julia

function compress7(inio, outio)
    nextwritebyte = read(inio, UInt8) & 0x7f
    filled = 7
    while !eof(inio)
        inbyte = read(inio, UInt8)
        write(outio, UInt8(nextwritebyte | inbyte << filled))
        nextwritebyte = inbyte >> (8 - filled)
        filled = (filled + 7) % 8
        if filled == 0
            if eof(inio)
                break
            end
            nextwritebyte = read(inio, UInt8) & 0x7f
            filled = 7
        end
    end
    if filled != 0
        write(outio, UInt8(nextwritebyte))
    end
end

function expand7(inio, outio)
    newbyte = read(inio, UInt8)
    write(outio, UInt8(newbyte & 0x7f))
    residualbyte::UInt8 = newbyte >> 7
    filled = 1
    while !eof(inio)
        inbyte = read(inio, UInt8)
        write(outio, UInt8((residualbyte | inbyte << filled) & 0x7f))
        residualbyte = inbyte >> (7 - filled)
        filled = (filled + 1) % 7
        if filled == 0
            write(outio, UInt8(residualbyte & 0x7f))
            residualbyte = 0
        end
    end
end

str = b"These bit oriented I/O functions can be used to implement compressors and decompressors."
ins = IOBuffer(str)
outs = IOBuffer()
newouts = IOBuffer()
compress7(ins, outs)
seek(outs,0)
expand7(outs, newouts)
println("Initial string of length $(length(str)): ", String(ins.data))
println("Compressed to length $(length(outs.data)) on line below:\n", String(outs.data))
println("Decompressed string: ", String(newouts.data))

```
 {{out}}
```txt

Initial string of length 88: These bit oriented I/O functions can be used to implement compressors and decompressors.
Compressed to length 77 on line below:
��A�y����A�6�]n��t��݆����[>��d��<~����|��]
Decompressed string: These bit oriented I/O functions can be used to implement compressors and decompressors.

```



## Kotlin

{{trans|C}}

```scala
// version 1.2.31

import java.io.File

class BitFilter(val f: File, var accu: Int = 0, var bits: Int = 0) {

    private val bw = f.bufferedWriter()
    private val br = f.bufferedReader()

    fun write(buf: ByteArray, start: Int, _nBits: Int, _shift: Int) {
        var nBits = _nBits
        var index = start + _shift / 8
        var shift = _shift % 8

        while (nBits != 0 || bits >= 8) {
            while (bits >= 8) {
                bits -= 8
                bw.write(accu ushr bits)
                accu = accu and ((1 shl bits) - 1)
            }
            while (bits < 8 && nBits != 0) {
                val b = buf[index].toInt()
                accu = (accu shl 1) or (((128 ushr shift) and b) ushr (7 - shift))
                nBits--
                bits++
                if (++shift == 8) { shift = 0; index++ }
            }
        }
    }

    fun read(buf: ByteArray, start: Int, _nBits: Int, _shift: Int) {
        var nBits = _nBits
        var index = start + _shift / 8
        var shift = _shift % 8

        while (nBits != 0) {
            while (bits != 0 && nBits != 0) {
                val mask = 128 ushr shift
                if ((accu and (1 shl (bits - 1))) != 0)
                    buf[index] = (buf[index].toInt() or mask).toByte()
                else
                    buf[index] = (buf[index].toInt() and mask.inv()).toByte()
                nBits--
                bits--
                if (++shift >= 8) { shift = 0; index++ }
            }
            if (nBits == 0) break
            accu = (accu shl 8) or br.read()
            bits += 8
        }
    }

    fun closeWriter() {
        if (bits != 0) {
            accu = (accu shl (8 - bits))
            bw.write(accu)
        }
        bw.close()
        accu = 0
        bits = 0
    }

    fun closeReader() {
        br.close()
        accu = 0
        bits = 0
    }
}

fun main(args: Array<String>) {
    val s = "abcdefghijk".toByteArray(Charsets.UTF_8)
    val f = File("test.bin")
    val bf = BitFilter(f)

    /* for each byte in s, write 7 bits skipping 1 */
    for (i in 0 until s.size) bf.write(s, i, 7, 1)
    bf.closeWriter()

    /* read 7 bits and expand to each byte of s2 skipping 1 bit */
    val s2 = ByteArray(s.size)
    for (i in 0 until s2.size) bf.read(s2, i, 7, 1)
    bf.closeReader()
    println(String(s2, Charsets.UTF_8))
}
```


{{out}}

```txt

abcdefghijk

```



## Lingo

A "BitArray" class can be implemented by sub-classing ByteArray and extending it with methods that allow to get/set individual bits:

```lingo
-- parent script "BitArray"

property ancestor
property bitSize
property _pow2

----------------------------------------
-- @constructor
-- @param {integer} [bSize=0]
----------------------------------------
on new (me, bSize)
  if voidP(bitSize) then bitSize=0
  me.bitSize = bSize
  byteSize = bitSize/8 + (bitSize mod 8>0)
  me._pow2 = [128,64,32,16,8,4,2,1] -- pow2 lookup list
  me.ancestor = ByteArray(byteSize)
  return me
end

----------------------------------------
-- Sets bit at position <bitPos> to <bitValue>.
-- @param {integer} bitPos - starts at 1, as ByteArray's native byte access functions
-- @param {boolean} bitValue
----------------------------------------
on setBit (me, bitPos, bitValue)
  bytePos = (bitPos-1)/8 + 1
  bitPos = (bitPos-1) mod 8 + 1
  if bitValue then
    me[bytePos] = bitOr(me[bytePos], me._pow2[bitPos])
  else
    me[bytePos] = bitAnd(me[bytePos], bitNot(me._pow2[bitPos]))
  end if
end

----------------------------------------
-- Gets bit value at position <bitPos>.
-- @param {integer} bitPos - starts at 1, as ByteArray's native byte access functions
-- @return {boolean} bitValue
----------------------------------------
on getBit (me, bitPos)
  bytePos = (bitPos-1)/8 + 1
  bitPos = (bitPos-1) mod 8 + 1
  return bitAnd(me[bytePos], me._pow2[bitPos])<>0
end

----------------------------------------
-- Returns all bits as string. To be in accordance with ByteArray's native toHexString(),
-- returned string is separated with SPACE (e.g. "0 1 1 0...")
-- @param {integer} [bitSizeOnly=FALSE] - if TRUE, only <bitSize> bits without byte-padding
-- @return {string}
----------------------------------------
on toBinString (me, bitSizeOnly)
  res = ""
  repeat with i = 1 to me.length
    byte = me[i]
    repeat with j = 1 to 8
      put (bitAnd(byte, me._pow2[j])<>0)&" " after res
      if bitSizeOnly and (i-1)*8+j=me.bitSize then exit repeat
    end repeat
  end repeat
  delete the last char of res
  return res
end
```


Simple compression/decompression functions for 7-bit ASCII strings:

```lingo
----------------------------------------
-- @param {string} str - ASCII string
-- @return {instance} BitArray
----------------------------------------
on crunchASCII (str)
  ba = script("BitArray").new(str.length * 7)
  pow2 = [64,32,16,8,4,2,1]
  pos = 1
  repeat with i = 1 to str.length
    n = chartonum(str.char[i])
    repeat with j = 1 to 7
      ba.setBit(pos, bitAnd(n, pow2[j])<>0)
      pos = pos+1
    end repeat
  end repeat
  return ba
end

----------------------------------------
-- @param {instance} bitArray
-- @return {string} ASCII string
----------------------------------------
on decrunchASCII (bitArray)
  str = ""
  pow2 = [64,32,16,8,4,2,1]
  pos = 1
  cnt = bitArray.bitSize/7
  repeat with i = 1 to cnt
    n = 0
    repeat with j = 1 to 7
      n = n + bitArray.getBit(pos)*pow2[j]
    pos = pos+1
    end repeat
    put numtochar(n) after str
  end repeat
  return str
end
```



```lingo
str = "ABC"
ba = crunchASCII(str)

put ba.toBinString()
-- "1 0 0 0 0 0 1 1 0 0 0 0 1 0 1 0 0 0 0 1 1 0 0 0"

put ba.toBinString(TRUE)
 -- "1 0 0 0 0 0 1 1 0 0 0 0 1 0 1 0 0 0 0 1 1"

put decrunchASCII(ba)
-- "ABC"
```



## MIPS Assembly

See [[Bitwise IO/MIPS Assembly]]


## Nim



```nim
type
  BitWriter * = tuple
    file: File
    bits: uint8
    nRemain: int

  BitReader * = tuple
    file: File
    bits: uint8
    nRemain: int
    nRead: int

proc newBitWriter * (file: File) : ref BitWriter =
  result = new BitWriter
  result.file = file
  result.bits = 0
  result.nRemain = 8

proc flushBits (stream : ref BitWriter) =
  discard stream.file.writeBuffer(stream.bits.addr, 1)
  stream.nRemain = 8
  stream.bits = 0

proc write * (stream: ref BitWriter, bits: uint8, nBits: int) =
  assert(nBits <= 8)

  for ii in countdown((nBits - 1), 0) :
    stream.bits = (stream.bits shl 1) or ((bits shr ii) and 1)
    stream.nRemain.dec(1)
    if stream.nRemain == 0:
      stream.flushBits

proc flush * (stream: ref BitWriter) =
  if stream.nRemain < 8:
    stream.bits = stream.bits shl stream.nRemain
    stream.flushBits

proc newBitReader * (file: File) : ref BitReader =
  result = new BitReader
  result.file = file
  result.bits = 0
  result.nRemain = 0
  result.nRead = 0

proc read * (stream: ref BitReader, nBits: int) : uint8 =
  assert(nBits <= 8)

  result = 0
  for ii in 0 .. < nBits :
    if stream.nRemain == 0:
      stream.nRead = stream.file.readBuffer(stream.bits.addr, 1)
      if stream.nRead == 0:
        break
      stream.nRemain = 8

    result = (result shl 1) or ((stream.bits shr 7) and 1)

    stream.bits = stream.bits shl 1
    stream.nRemain.dec(1)


when isMainModule:
  var
    file: File
    writer: ref BitWriter
    reader: ref BitReader

  file = open("testfile.dat", fmWrite)
  writer = newBitWriter(file)

  for ii in 0 .. 255:
    writer.write(ii.uint8, 7)

  writer.flush
  file.close

  var dataCtr = 0

  file = open("testfile.dat", fmRead)
  reader = newBitReader(file)

  while true:
    let aByte = reader.read(7)

    if reader.nRead == 0:
      break

    assert((dataCtr and 0x7f).uint8 == aByte)

  assert(dataCtr == 256)

  file.close

  echo "OK"
```



## OCaml

The [http://code.google.com/p/ocaml-extlib/ extLib] provides [http://ocaml-extlib.googlecode.com/svn/doc/apiref/IO.html#6_BitsAPI bit oriented IO functions].

```ocaml
let write_7bit_string ~filename ~str =
  let oc = open_out filename in
  let ob = IO.output_bits(IO.output_channel oc) in
  String.iter (fun c -> IO.write_bits ob 7 (int_of_char c)) str;
  IO.flush_bits ob;
  close_out oc;
;;
```



```ocaml
let read_7bit_string ~filename =
  let ic = open_in filename in
  let ib = IO.input_bits(IO.input_channel ic) in
  let buf = Buffer.create 2048 in
  try while true do
    let c = IO.read_bits ib 7 in
    Buffer.add_char buf (char_of_int c);
  done; ""
  with IO.No_more_input ->
    (Buffer.contents buf)
```



## Perl


```perl
#! /usr/bin/perl

use strict;

# $buffer = write_bits(*STDOUT, $buffer, $number, $bits)
sub write_bits( $$$$ )
{
    my ($out, $l, $num, $q) = @_;
    $l .= substr(unpack("B*", pack("N", $num)),
		 -$q);
    if ( (length($l) > 8) ) {
	my $left = substr($l, 8);
	print $out pack("B8", $l);
	$l = $left;
    }
    return $l;
}

# flush_bits(*STDOUT, $buffer)
sub flush_bits( $$ )
{
    my ($out, $b) = @_;
    print $out pack("B*", $b);
}

# ($val, $buf) = read_bits(*STDIN, $buf, $n)
sub read_bits( $$$ )
{
    my ( $in, $b, $n ) = @_;
    # we put a limit in the number of bits we can read
    # with one shot; this should mirror the limit of the max
    # integer value perl can hold
    if ( $n > 32 ) { return 0; }
    while ( length($b) < $n ) {
	my $v;
	my $red = read($in, $v, 1);
	if ( $red < 1 ) { return ( 0, -1 ); }
	$b .= substr(unpack("B*", $v), -8);
    }
    my $bits = "0" x ( 32-$n ) . substr($b, 0, $n);
    my $val = unpack("N", pack("B32", $bits));
    $b = substr($b, $n);
    return ($val, $b);
}
```

''Crunching'' bytes discarding most significant bit (lossless compression for ASCII and few more!)

```perl
my $buf = "";
my $c;
while( read(*STDIN, $c, 1) > 0 ) {
    $buf = write_bits(*STDOUT, $buf, unpack("C1", $c), 7);
}
flush_bits(*STDOUT, $buf);
```

Expanding each seven bits to fit a byte (padding the ''eight'' most significant bit with 0):

```perl
my $buf = "";
my $v;
while(1) {
    ( $v, $buf ) = read_bits(*STDIN, $buf, 7);
    last if ($buf < 0);
    print pack("C1", $v);
}
```



## Perl 6


```perl6
sub encode-ascii(Str $s) {
    my @b = flat $s.ords».fmt("%07b")».comb;
    @b.push(0) until @b %% 8;   # padding
    Buf.new: gather while @b { take reduce * *2+*, (@b.pop for ^8) }
}

sub decode-ascii(Buf $b) {
    my @b = flat $b.list».fmt("%08b")».comb;
    @b.shift until @b %% 7;   # remove padding
    @b = gather while @b { take reduce * *2+*, (@b.pop for ^7) }
    return [~] @b».chr;
}
say my $encode = encode-ascii 'STRING';
say decode-ascii $encode;
```

{{out}}

```txt
Buf:0x<03 8b 99 29 4a e5>
STRING
```



## Phix


```Phix
enum FN, V, BITS -- fields of a bitwiseioreader/writer

function new_bitwiseio(string filename, mode)
    integer fn = open(filename,mode)
    return {fn,0,0}     -- ie {FN,V=0,BITS=0}
end function

function new_bitwiseiowriter(string filename)
    return new_bitwiseio(filename,"wb")
end function

function new_bitwiseioreader(string filename)
    return new_bitwiseio(filename,"rb")
end function

function write_bits(sequence writer, integer v, bits)
    integer {fn,wv,wb} = writer,
            p2 = power(2,bits), ch
    if v!=and_bits(v,p2*2-1) then ?9/0 end if
    wv = wv*p2+v
    wb += bits
    while wb>=8 do
        wb -= 8
        p2 = power(2,wb)
        ch = floor(wv/p2)
        puts(fn,ch)
        wv -= ch*p2
    end while
    if wv>=#100 then ?9/0 end if
    if wb>=8 then ?9/0 end if
    writer[V]= wv
    writer[BITS]= wb
    return writer
end function

function close_bitwiseiowriter(sequence writer)
    integer {fn,wv,wb} = writer
    if wb then
        if wb>=8 then ?9/0 end if -- sanity check
        writer = write_bits(writer,0,8-wb)
    end if
    if writer[V]!=0 then ?9/0 end if
    if writer[BITS]!=0 then ?9/0 end if
    close(fn)
    writer[FN]=-1
    return writer
end function

function read_bits(sequence reader, integer bits)
    integer {fn,rv,rb} = reader, ch, p2
    while bits>rb do
        ch = getc(fn)
        if ch=-1 then return {-1,reader} end if
        rv = rv*#100+ch
        rb += 8
    end while
    rb -= bits
    p2 = power(2,rb)
    ch = floor(rv/p2)
    rv -= ch*p2
    reader[V]= rv
    reader[BITS]= rb
    return {ch,reader}
end function

function as_hexb(string s, fmt="%02x ")
-- helper funtion, returns hex string, or binary if fmt="%08b "
    string res = ""
    for i=1 to length(s) do
        res &= sprintf(fmt,s[i])
    end for
    return trim(res)
end function

constant test = "This is a test."
--constant test = "This is a test"
--constant test = "abcdefghijk"
--constant test = "STRING"
--constant test = "This is an ascii string that will be crunched, written, read and expanded."
printf(1,"\"%s\" as bytes: %s (length %d)\n",{test,as_hexb(test),length(test)})
printf(1,"    original bits: %s\n",{as_hexb(test,"%08b ")})

sequence writer = new_bitwiseiowriter("test.bin")
for i=1 to length(test) do
    writer = write_bits(writer,test[i],7)
end for
writer = close_bitwiseiowriter(writer)

integer fn = open("test.bin","rb")
string bytes = get_text(fn,GT_WHOLE_FILE)
printf(1,"Written bitstream: %s\n",{as_hexb(bytes,"%08b ")})
printf(1,"Written bytes: %s (length %d)\n",{as_hexb(bytes),length(bytes)})
close(fn)

sequence reader = new_bitwiseioreader("test.bin")
bytes = ""
integer ch
while true do
    {ch,reader} = read_bits(reader,7)
    if ch=-1 then exit end if
    bytes &= ch
end while
printf(1,"\"%s\" as bytes: %s (length %d)\n",{bytes,as_hexb(bytes),length(bytes)})
```

{{out}}

```txt

"This is a test." as bytes: 54 68 69 73 20 69 73 20 61 20 74 65 73 74 2E (length 15)
    original bits: 01010100 01101000 01101001 01110011 00100000 01101001 01110011 00100000 01100001 00100000 01110100 01100101 01110011 01110100 00101110
Written bitstream: 10101001 10100011 01001111 00110100 00011010 01111001 10100000 11000010 10000011 10100110 01011110 01111101 00010111 00000000
Written bytes: A9 A3 4F 34 1A 79 A0 C2 83 A6 5E 7D 17 00 (length 14)
"This is a test. " as bytes: 54 68 69 73 20 69 73 20 61 20 74 65 73 74 2E 00 (length 16)

```

Note that the 105(=15*7) bits written out are rounded up/padded to 112(=14*8), reading them back in makes for 16 whole 7-bit values,
which, since 16*7==112, cannot be distinguished from deliberately writing a trailing 7 zero bits. You could, as per the Go example,
simply ignore retrieved zero bytes, but that could fairly obviously create problems for some forms of binary data. A better solution
might be for the data to embed or prefix it's own length. The other four (commented-out) test values do not exhibit this problem.


## PL/I


```PL/I
declare onebit bit(1) aligned, bs bit (1000) varying aligned;
on endfile (sysin) go to ending;
bs = ''b;
do forever;
   get edit (onebit) (F(1));
   bs = bs || onebit;
end;
ending:
bs = bs || copy('0'b, mod(length(bs), 8) );
                                 /* pad length to a multiple of 8 */
put edit (bs) (b);
```

Example:

```txt

/* Test, read text, write out 7 bits for each 8-bit character. */
/* Pad, so that bits written are a multiple of 8. */

test: procedure options (main);            /* 6/9/2012 */

   declare 1 z union,
             2 ch character (1),
             2 bits,
               3 dummy bit (1), 3 bitvalue bit (7);
   declare length fixed initial (0);
   declare in file;
   open file (in) input title ('/BIT-DATA.DAT,type(fixed),recsize(1)');

   on endfile (in) go to finish_up;
   do forever;
      get file (in) edit (ch) (a(1));
      put edit (bitvalue) (b(7)); /* The least-significant 7 bits. */
      length = mod(length + 7, 8);
   end;

finish_up:
   put edit (substr('0000000', 1, length)) (b);
end test;

```

data for test: STRING.
The output is:

```txt

1010011101010010100101001001100111010001111010011000110100010100000000

```



## PicoLisp


```PicoLisp
(de write7bitwise (Lst)
   (let (Bits 0  Byte)
      (for N Lst
         (if (=0 Bits)
            (setq Bits 7  Byte (* 2 N))
            (wr (| Byte (>> (dec 'Bits) N)))
            (setq Byte (>> (- Bits 8) N)) ) )
      (unless (=0 Bits)
         (wr Byte) ) ) )

(de read7bitwise ()
   (make
      (let (Bits 0  Byte)
         (while (rd 1)
            (let N @
               (link
                  (if (=0 Bits)
                     (>> (one Bits) N)
                     (| Byte (>> (inc 'Bits) N)) ) )
               (setq Byte (& 127 (>> (- Bits 7) N))) ) )
         (when (= 7 Bits)
            (link Byte) ) ) ) )
```


```PicoLisp
(out 'a (write7bitwise (127 0 127 0 127 0 127 0 127)))
(hd 'a)
(in 'a (println (read7bitwise)))

(out 'a (write7bitwise (0 127 0 127 0 127 0 127 0)))
(hd 'a)
(in 'a (println (read7bitwise)))

(out 'a (write7bitwise (mapcar char (chop "STRING"))))
(hd 'a)
(println (mapcar char (in 'a (read7bitwise))))
```

{{out}}

```txt
00000000  FE 03 F8 0F E0 3F 80 FE                          .....?..
(127 0 127 0 127 0 127 0)
00000000  01 FC 07 F0 1F C0 7F 00                          .......
(0 127 0 127 0 127 0 127)
00000000  A7 52 94 99 D1 C0                                .R....
("S" "T" "R" "I" "N" "G")
```



## PureBasic

The bits are read/written with the HSB being read/written first, then each full byte (8 bits) is read/written in succession.  Depending on the native integer size the compiler is using, upto 32-bits or 64-bits can be read/written at once.  The procedure flushBits() should be called when the reading/writing of bits is completed.  If a partial byte is written the bits containing data will begin with the HSB and the padding bits will end with the LSB.

As a slight speed modification, the readBits() and writeBits() procedures will attempt to write groups of bits whenever possible.

```PureBasic
Structure fileDataBits
  bitsToWrite.i
  bitsToRead.i
  outputByte.i
  inputByte.i
EndStructure
#BitsPerByte = SizeOf(Byte) * 8
#BitsPerInteger = SizeOf(Integer) * 8

Global Dim fileBitMask(8)
Define i, x
For i = 0 To 8
  fileBitMask(i) = x
  x = (x << 1) + 1
Next

Global NewMap fileDataBits.fileDataBits()
Procedure flushBits(fileID)
  If FindMapElement(fileDataBits(), Str(fileID))
    If fileDataBits()\bitsToWrite > 0
      WriteByte(fileID, fileDataBits()\outputByte << (#BitsPerByte - fileDataBits()\bitsToWrite))
      fileDataBits()\bitsToWrite = 0
      fileDataBits()\outputByte = 0
    EndIf
    DeleteMapElement(fileDataBits())
  EndIf
EndProcedure

Procedure writeBits(fileID, Value.i, bitCount)
  Protected *fileData.fileDataBits = FindMapElement(fileDataBits(), Str(fileID))
  If Not *fileData
    *fileData = AddMapElement(fileDataBits(), Str(fileID))
    If Not *fileData: End: EndIf ;simple error check for lack of memory
  EndIf

  Value << (#BitsPerInteger - bitCount) ;shift value so it's first bit (HSB) is in the highest position
  While bitCount > 0
    If bitCount > #BitsPerByte - *fileData\bitsToWrite
      bitGroupSize = #BitsPerByte - *fileData\bitsToWrite
    Else
      bitGroupSize = bitCount
    EndIf

    *fileData\outputByte << bitGroupSize
    *fileData\outputByte + (Value >> (#BitsPerInteger - bitGroupSize)) & fileBitMask(bitGroupSize)
    Value << bitGroupSize
    *fileData\bitsToWrite + bitGroupSize
    If *fileData\bitsToWrite = #BitsPerByte
      WriteByte(fileID, *fileData\outputByte)
      *fileData\bitsToWrite = 0
      *fileData\outputByte = 0
    EndIf
    bitCount - bitGroupSize
  Wend
EndProcedure

Procedure.i readBits(fileID, bitCount)
  Protected *fileData.fileDataBits = FindMapElement(fileDataBits(), Str(fileID))
  If Not *fileData
    *fileData = AddMapElement(fileDataBits(), Str(fileID))
    If Not *fileData: End: EndIf ;simple error check for lack of memory
  EndIf

  Protected Value.i, bitGroupSize
  While bitCount > 0
    If *fileData\bitsToRead = 0
      If Not Eof(fileID)
        *fileData\inputByte = ReadByte(fileID)
        *fileData\bitsToRead = #BitsPerByte
      Else
        flushBits(fileID)
        Break ;simple error check aborts if nothing left to read
      EndIf
    EndIf

    If bitCount > *fileData\bitsToRead
      bitGroupSize = *fileData\bitsToRead
    Else
      bitGroupSize = bitCount
    EndIf
    Value << bitGroupSize
    Value + (*fileData\inputByte >> (#BitsPerByte - bitGroupSize)) & fileBitMask(bitGroupSize) ;shift last bit being read in byte to the lowest position
    *fileData\inputByte << bitGroupSize
    *fileData\bitsToRead - bitGroupSize
    bitCount - bitGroupSize
  Wend
  ProcedureReturn Value
EndProcedure

;test
Define testWriteString.s, testReadString.s, fileNum, result.s
testWriteString = "This is an ascii string that will be crunched, written, read and expanded."
fileNum = CreateFile(#PB_Any, "BitIO_Test.dat")
If fileNum
  For i = 1 To Len(testWriteString)
    writeBits(fileNum, Asc(Mid(testWriteString, i, 1)), 7)
  Next
  flushBits(fileNum)
  CloseFile(fileNum)
EndIf

fileNum = ReadFile(#PB_Any, "BitIO_Test.dat")
If fileNum
  For i = 1 To Len(testWriteString)
    testReadString + Chr(readBits(fileNum, 7))
  Next
  flushBits(fileNum)
  CloseFile(fileNum)
EndIf

result = "Original ascii string is " + Str(Len(testWriteString)) + " bytes." + #LF$
result + "Filesize written is " + Str(FileSize("test.xxx")) + " bytes." + #LF$
If testReadString = testWriteString
  result + "The expanded string is the same as the original." + #LF$
Else
  result + "The expanded string is not the same as the original." + #LF$
EndIf
result + "Expanded string = '" + testReadString + "'"

MessageRequester("Results", result)
```

{{out}}

```txt
Original ascii string is 74 bytes.
Filesize written is 65 bytes.
The expanded string is the same as the original.
Expanded string = 'This is an ascii string that will be crunched, written, read and expanded.'
```



## Python

This following code works in both Python 2 & 3. Suggested module file name <tt>bitio.py</tt>.

```python
class BitWriter(object):
    def __init__(self, f):
        self.accumulator = 0
        self.bcount = 0
        self.out = f

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.flush()

    def __del__(self):
        try:
            self.flush()
        except ValueError:   # I/O operation on closed file.
            pass

    def _writebit(self, bit):
        if self.bcount == 8:
            self.flush()
        if bit > 0:
            self.accumulator |= 1 << 7-self.bcount
        self.bcount += 1

    def writebits(self, bits, n):
        while n > 0:
            self._writebit(bits & 1 << n-1)
            n -= 1

    def flush(self):
        self.out.write(bytearray([self.accumulator]))
        self.accumulator = 0
        self.bcount = 0


class BitReader(object):
    def __init__(self, f):
        self.input = f
        self.accumulator = 0
        self.bcount = 0
        self.read = 0

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        pass

    def _readbit(self):
        if not self.bcount:
            a = self.input.read(1)
            if a:
                self.accumulator = ord(a)
            self.bcount = 8
            self.read = len(a)
        rv = (self.accumulator & (1 << self.bcount-1)) >> self.bcount-1
        self.bcount -= 1
        return rv

    def readbits(self, n):
        v = 0
        while n > 0:
            v = (v << 1) | self._readbit()
            n -= 1
        return v

if __name__ == '__main__':
    import os
    import sys
    # Determine this module's name from it's file name and import it.
    module_name = os.path.splitext(os.path.basename(__file__))[0]
    bitio = __import__(module_name)

    with open('bitio_test.dat', 'wb') as outfile:
        with bitio.BitWriter(outfile) as writer:
            chars = '12345abcde'
            for ch in chars:
                writer.writebits(ord(ch), 7)

    with open('bitio_test.dat', 'rb') as infile:
        with bitio.BitReader(infile) as reader:
            chars = []
            while True:
                x = reader.readbits(7)
                if not reader.read:  # End-of-file?
                    break
                chars.append(chr(x))
            print(''.join(chars))

```

Another usage example showing how to "crunch" an 8-bit byte ASCII stream discarding the most significant "unused" bit...and read it back.

```python
import sys
import bitio

o = bitio.BitWriter(sys.stdout)
c = sys.stdin.read(1)
while len(c) > 0:
    o.writebits(ord(c), 7)
    c = sys.stdin.read(1)
o.flush()

```

...and to "decrunch" the same stream:

```python
import sys
import bitio

r = bitio.BitReader(sys.stdin)
while True:
    x = r.readbits(7)
    if not r.read:  # nothing read
        break
    sys.stdout.write(chr(x))
```



## Racket



```racket

#lang racket

(require racket/fixnum)

(define (make-bit-writer file)
  (define o (open-output-file file #:exists 'truncate))
  (define b+len (cons 0 0))
  (define (write-some-bits! n len)
    (if (<= 8 len)
      (begin (write-byte (fxand n #xFF) o)
             (write-some-bits! (fxrshift n 8) (- len 8)))
      (set! b+len (cons n len))))
  (define write-bits
    (case-lambda
      [(n) (if (eof-object? n)
             (begin (when (positive? (cdr b+len)) (write-byte (car b+len) o))
                    (close-output-port o))
             (write-bits n (integer-length n)))]
      [(n nbits)
       (when (< nbits (integer-length n))
         (error 'write-bits "integer bigger than number of bits"))
       (write-some-bits! (fxior (car b+len) (fxlshift n (cdr b+len)))
                         (+ (cdr b+len) nbits))]))
  write-bits)

(define (make-bit-reader file)
  (define i (open-input-file file))
  (define b+len (cons 0 0))
  (define (read-some-bits wanted n len)
    (if (<= wanted len)
      (begin0 (fxand n (sub1 (expt 2 wanted)))
        (set! b+len (cons (fxrshift n wanted) (- len wanted))))
      (read-some-bits wanted (+ n (fxlshift (read-byte i) len)) (+ len 8))))
  (define (read-bits n)
    (if (eof-object? n)
      (close-input-port i)
      (read-some-bits n (car b+len) (cdr b+len))))
  read-bits)

(define (crunch str file)
  (define out (make-bit-writer file))
  (for ([b (in-bytes (string->bytes/utf-8 str))]) (out b 7))
  (out eof))

(define (decrunch file)
  (define in (make-bit-reader file))
  (define bs (for/list ([i (in-range (quotient (* 8 (file-size file)) 7))])
               (in 7)))
  (in eof)
  (bytes->string/utf-8 (list->bytes bs)))

(define orig
  (string-append "This is an ascii string that will be"
                 " crunched, written, read and expanded."))

(crunch orig "crunched.out")

(printf "Decrunched string ~aequal to original.\n"
        (if (equal? orig (decrunch "crunched.out")) "" "NOT "))

```


{{out}}

```txt

Decrunched string equal to original.

```



## REXX


### version 1


```rexx
/* REXX ****************************************************************
* 01.11.2012 Walter Pachl
***********************************************************************/
s='STRING'                             /* Test input                  */
Say 's='s
ol=''                                  /* initialize target           */
Do While s<>''                         /* loop through input          */
  Parse Var s c +1 s                   /* pick a character            */
  cx=c2x(c)                            /* convert to hex              */
  cb=x2b(cx)                           /* convert to bits             */
  ol=ol||substr(cb,2)                  /* append to target            */
  End
l=length(ol)                           /* current length              */
lm=l//8
ol=ol||copies('0',8-lm)                /* pad to multiple of 8        */
pd=copies(' ',l)||copies('0',8-lm)
Say 'b='ol                             /* show target                 */
Say '  'pd 'padding'
r=''                                   /* initialize result           */
Do While length(ol)>6                  /* loop through target         */
  Parse Var ol b +7 ol                 /* pick 7 bits                 */
  b='0'||b                             /* add a leading '0'           */
  x=b2x(b)                             /* convert to hex              */
  r=r||x2c(x)                          /* convert to character        */
  End                                  /* and append to result        */
Say 'r='r                              /* show result                 */
```

{{out}}

```txt

s=STRING
b=101001110101001010010100100110011101000111000000
                                            000000 padding
r=STRING

```



### version 2


```rexx
/*REXX pgm encodes/decodes/displays ASCII character strings as  (7─bits)  binary string.*/
parse arg $;        if $==''  then $='STRING'    /*get optional argument; Use default ? */
say '   input string=' $                         /*display the input string to terminal.*/
out=comp($);        say ' encoded string='  out  /*encode─► 7─bit binary string; display*/
ori=dcomp(out);     say ' decoded string='  ori  /*decode─► 8─bit char      "  ;    "   */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
comp:  parse arg x; z=;    do j=1  for length(x) /*convert─►right-justified 7-bit binary*/
                           z=z || right( x2b( c2x( substr(x, j, 1) )), 7)
                           end   /*j*/;     return left(z,length(z)+(8-length(z)//8)//8,0)
/*──────────────────────────────────────────────────────────────────────────────────────*/
dcomp: parse arg x; z=;    do k=1  by 7  to length(x);       _=substr(x, k, 7)
                           if right(_, 1)==' '  then leave
                           z=z || x2c( b2x(0 || _) )
                           end   /*k*/;     return z
```

{{out|output|text=  when using the default input:}}

```txt

   input string= STRING
 encoded string= 101001110101001010010100100110011101000111000000
 decoded string= STRING

```



## Ruby

{{trans|Tcl}}
{{works with|Ruby|1.8.7}}

```ruby
def crunch(ascii)
  bitstring = ascii.bytes.collect {|b| "%07b" % b}.join
  [bitstring].pack("B*")
end

def expand(binary)
  bitstring = binary.unpack("B*")[0]
  bitstring.scan(/[01]{7}/).collect {|b| b.to_i(2).chr}.join
end

original = "This is an ascii string that will be crunched, written, read and expanded."
puts "my ascii string is #{original.length} bytes"

filename = "crunched.out"

# write the compressed data
File.open(filename, "w") do |fh|
  fh.binmode
  fh.print crunch(original)
end

filesize = File.size(filename)
puts "the file containing the crunched text is #{filesize} bytes"

# read and expand
expanded = File.open(filename, "r") do |fh|
  fh.binmode
  expand(fh.read)
end

if original == expanded
  puts "success"
else
  puts "fail!"
end
```



## Seed7

The Seed7 library [http://seed7.sourceforge.net/libraries/bitdata.htm bitdata.s7i] defines
several functions to do bitwise I/O. Bitwise data can be read from (or written to) a string or a file.
The direction of bits can be from LSB (least significant bit) to MSB (most significant bit) or vice versa.
In the program below the functions
[http://seed7.sourceforge.net/libraries/bitdata.htm#putBitsMsb(inout_file,inout_integer,in_var_integer,in_var_integer) putBitsMsb] and [http://seed7.sourceforge.net/libraries/bitdata.htm#getBitsMsb(inout_file,inout_integer,in_var_integer) getBitsMsb] are used.


```seed7
$ include "seed7_05.s7i";
  include "bitdata.s7i";
  include "strifile.s7i";

const proc: initWriteAscii (inout file: outFile, inout integer: bitPos) is func
  begin
    outFile.bufferChar := '\0;';
    bitPos := 0;
  end func;

const proc: writeAscii (inout file: outFile, inout integer: bitPos, in string: ascii) is func
  local
    var char: ch is ' ';
  begin
    for ch range ascii do
      if ch > '\127;' then
        raise RANGE_ERROR;
      else
        putBitsMsb(outFile, bitPos, ord(ch), 7);
      end if;
    end for;
  end func;

const proc: finishWriteAscii (inout file: outFile, inout integer: bitPos) is func
  begin
    write(outFile, chr(ord(outFile.bufferChar)));
  end func;

const proc: initReadAscii (inout file: outFile, inout integer: bitPos) is func
  begin
    bitPos := 8;
  end func;

const func string: readAscii (inout file: inFile, inout integer: bitPos, in integer: length) is func
  result
    var string: stri is "";
  local
    var char: ch is ' ';
  begin
    while not eof(inFile) and length(stri) < length do
      ch := chr(getBitsMsb(inFile, bitPos, 7));
      if inFile.bufferChar <> EOF then
        stri &:= ch;
      end if;
    end while;
  end func;

const proc: main is func
  local
    var file: aFile is STD_NULL;
    var integer: bitPos is 0;
  begin
    aFile := openStrifile;
    initWriteAscii(aFile, bitPos);
    writeAscii(aFile, bitPos, "Hello, Rosetta Code!");
    finishWriteAscii(aFile, bitPos);
    seek(aFile, 1);
    initReadAscii(aFile, bitPos);
    writeln(literal(readAscii(aFile, bitPos, 100)));
  end func;
```


{{out}}

```txt

"Hello, Rosetta Code!"

```



## Tcl


```tcl
package require Tcl 8.5

proc crunch {ascii} {
    binary scan $ascii B* bitstring
    # crunch: remove the extraneous leading 0 bit
    regsub -all {0(.{7})} $bitstring {\1} 7bitstring
    set padded "$7bitstring[string repeat 0 [expr {8 - [string length $7bitstring]%8}]]"
    return [binary format B* $padded]
}

proc expand {binary} {
    binary scan $binary B* padded
    # expand the 7 bit segments with their leading 0 bit
    set bitstring "0[join [regexp -inline -all {.{7}} $padded] 0]"
    return [binary format B* $bitstring]
}

proc crunch_and_write {ascii filename} {
    set fh [open $filename w]
    fconfigure $fh -translation binary
    puts -nonewline $fh [crunch $ascii]
    close $fh
}

proc read_and_expand {filename} {
    set fh [open $filename r]
    fconfigure $fh -translation binary
    set input [read $fh [file size $filename]]
    close $fh
    return [expand $input]
}

set original "This is an ascii string that will be crunched, written, read and expanded."
puts "my ascii string is [string length $original] bytes"

set filename crunched.out
crunch_and_write $original $filename

set filesize [file size $filename]
puts "the file containing the crunched text is $filesize bytes"

set expanded [read_and_expand $filename]

if {$original eq $expanded} {
    puts "the expanded string is the same as the original"
} else {
    error "not the same"
}
```

{{out}}

```txt
my ascii string is 74 bytes
the file containing the crunched text is 65 bytes
the expanded string is the same as the original
```



## zkl

This code implements two state machines: one that transforms a bit stream (ie a stream of ints with a maximum number of bits) to a byte stream and one does the reverse transform. All streams are considered infinite (eg a file or socket).

Bits to bytes:

```zkl
   // stream of numBits sized ints to bytes, numBits<8
fcn toBytes(n,[(numBits,acc,bitsSoFar)]state){
   acc=acc.shiftLeft(numBits) + n; bitsSoFar+=numBits;
   reg r;
   if(bitsSoFar>=8){
      bitsSoFar-=8;
      r=acc.shiftRight(bitsSoFar);
      acc=acc.bitAnd((-1).shiftLeft(bitsSoFar).bitNot());
   }
   else r=Void.Skip;  // need more bits to make a byte
   state.clear(numBits,acc,bitsSoFar);
   r
}
```

Encode a stream of 6 bit characters:

```zkl
ns:="THIS IS A TEST".pump(List,"toAsc",'-(0x20));
ns.println(ns.len());

state:=L(6,0,0,L());  // input is six bits wide
cns:=ns.pump(List,toBytes.fp1(state)); // List could be a file or socket or ...
if(state[2]) cns+=toBytes(0,state); // flush
cns.println(cns.len());
```

{{out}}

```txt

L(52,40,41,51,0,41,51,0,33,0,52,37,51,52)14
L(210,138,115,2,156,192,132,13,37,207,64)11

```

Byte stream to bit stream:

```zkl
   // stream of bytes to numBits sized ints, 1<numBits<32
fcn fromBytes(n,[(numBits,acc,bitsSoFar,buf)]state){
   acc=acc.shiftLeft(8) + n; bitsSoFar+=8;
   buf.clear();
   while(bitsSoFar>=numBits){
      bitsSoFar-=numBits;
      buf.append(acc.shiftRight(bitsSoFar));
      acc=acc.bitAnd((-1).shiftLeft(bitsSoFar).bitNot());
   }
   state.clear(numBits,acc,bitsSoFar,buf);
   return(Void.Write,Void.Write,buf); // append contents of buf to result
}
```

Decode the above stream:

```zkl
state:=L(6,0,0,L());  // output is six bits wide
r:=cns.pump(List,fromBytes.fp1(state)); // cns could be a file or ...
r.println(r.len());
r.pump(String,'+(0x20),"toChar").println();
```

{{out}}

```txt

L(52,40,41,51,0,41,51,0,33,0,52,37,51,52)14
THIS IS A TEST

```


{{omit from|AWK|Traditional AWK has no bitwise operators}}
{{omit from|gnuplot}}
{{omit from|LaTeX}}
{{omit from|Lilypond}}
{{omit from|Make}}
{{omit from|PlainTeX}}
{{omit from|TorqueScript}}
