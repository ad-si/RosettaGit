+++
title = "CRC-32"
description = ""
date = 2019-03-27T23:07:03Z
aliases = []
[extra]
id = 10966
[taxonomies]
categories = []
tags = []
+++

{{task|Checksums}}
[[Category:Checksums]]
{{omit from|GUISS}}
{{omit from|Lilypond}}
{{omit from|TPP}}


;Task:
Demonstrate a method of deriving the [[wp:Computation of cyclic redundancy checks|Cyclic Redundancy Check]] from within the language. 


The result should be in accordance with ISO 3309, [http://www.itu.int/rec/T-REC-V.42-200203-I/en ITU-T V.42], [http://tools.ietf.org/html/rfc1952 Gzip] and [http://www.w3.org/TR/2003/REC-PNG-20031110/ PNG]. 

Algorithms are described on [[wp:Cyclic redundancy check|Computation of CRC]] in Wikipedia. 
This variant of CRC-32 uses LSB-first order, sets the initial CRC to FFFFFFFF<sub>16</sub>, and complements the final CRC.

For the purpose of this task, generate a CRC-32 checksum for the ASCII encoded string:
:: <big><big><code>The quick brown fox jumps over the lazy dog</code></big></big>





## 11l

{{trans|C}}

```11l
V crc_table = [0] * 256
L(i) 256
   UInt32 rem = i
   L 8
      I rem [&] 1
         rem >>= 1
         rem (+)= EDB8'8320
      E
         rem >>= 1
   crc_table[i] = rem

F crc32(buf, =crc = UInt32(0))
   crc = (-)crc
   L(k) buf
      crc = (crc >> 8) (+) :crc_table[(crc [&] 00'FF) (+) k.code]
   R (-)crc

print(hex(crc32(‘The quick brown fox jumps over the lazy dog’)))
```


{{out}}

```txt

414FA339

```



## Ada

{{works with|GNAT}} 

```Ada
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.CRC32; use GNAT.CRC32;
with Interfaces; use Interfaces;
procedure TestCRC is
   package IIO is new Ada.Text_IO.Modular_IO (Unsigned_32);
   crc : CRC32;
   num : Unsigned_32;
   str : String := "The quick brown fox jumps over the lazy dog";
begin
   Initialize (crc);
   Update (crc, str);
   num := Get_Value (crc);
   IIO.Put (num, Base => 16); New_Line;
end TestCRC;
```

{{out}}

```txt
16#414FA339#
```



## ALGOL 68


```algol68

[0:255]BITS crc_table;
BOOL crc_table_computed := FALSE;

PROC make_crc_table = VOID:
   BEGIN 
      INT n, k;
      FOR n FROM 0 TO 255 DO 
         BITS c := BIN n;
         FOR k TO 8 DO 
            c := IF 32 ELEM c THEN 
                    16redb88320 XOR (c SHR 1)
                 ELSE
                    c SHR 1
		 FI
	 OD;
	 crc_table[n] := c
      OD;
      crc_table_computed := TRUE
   END;

PROC update_crc = (BITS crc, STRING buf) BITS:
   BEGIN 
      BITS c := crc XOR 16rffffffff;
      INT n;

      IF NOT crc_table_computed THEN make_crc_table FI;
      FOR n TO UPB buf DO 
         c := crc_table[ABS ((c XOR BIN ABS buf[n]) AND 16rff)] XOR (c SHR 8)
      OD ;
      c XOR 16rffffffff
   END;

 PROC hex = (BITS x) STRING :
   BEGIN
      PROC hexdig = (BITS x) CHAR: REPR (IF ABS x ≤ 9 THEN ABS x + ABS "0"
                                         ELSE ABS x - 10 + ABS "a"
					 FI);
      STRING h := "";
      IF x = 16r0 THEN
	 h := "0"
      ELSE
	 BITS n := x;
	 WHILE h := hexdig (n AND 16rf) + h; n ≠ 16r0 DO
	    n := n SHR 4
	 OD
      FI;
      h
   END;

PROC crc = (STRING buf) BITS:
   update_crc(16r0, buf);

STRING s = "The quick brown fox jumps over the lazy dog";
print(("CRC32 OF ", s, " is: ", hex (crc (s)), newline))

```


{{out}}

```txt
CRC32 OF The quick brown fox jumps over the lazy dog is: 0414fa339
```



## AutoHotkey


### DllCall / WinAPI


```AutoHotkey
CRC32(str, enc = "UTF-8")
{
    l := (enc = "CP1200" || enc = "UTF-16") ? 2 : 1, s := (StrPut(str, enc) - 1) * l
    VarSetCapacity(b, s, 0) && StrPut(str, &b, floor(s / l), enc)
    CRC32 := DllCall("ntdll.dll\RtlComputeCrc32", "UInt", 0, "Ptr", &b, "UInt", s)
    return Format("{:#x}", CRC32)
}

MsgBox % CRC32("The quick brown fox jumps over the lazy dog")
```

{{out}}

```txt
0x414fa339
```



### Implementation


```AutoHotkey
CRC32(str)
{
    static table := []
    loop 256 {
        crc := A_Index - 1
        loop 8
            crc := (crc & 1) ? (crc >> 1) ^ 0xEDB88320 : (crc >> 1)
        table[A_Index - 1] := crc
    }
    crc := ~0
    loop, parse, str
        crc := table[(crc & 0xFF) ^ Asc(A_LoopField)] ^ (crc >> 8)
    return Format("{:#x}", ~crc)
}

MsgBox % CRC32("The quick brown fox jumps over the lazy dog")
```

{{out}}

```txt
0x414fa339
```



## C


### Library

Using [http://www.stillhq.com/gpg/source-modified-1.0.3/zlib/crc32.html zlib's crc32]:

```c>#include <stdio.h

#include <string.h>
#include <zlib.h>
 
int main()
{
	const char *s = "The quick brown fox jumps over the lazy dog";
	printf("%lX\n", crc32(0, (const void*)s, strlen(s)));

	return 0;
}
```



### Implementation

This code is a translation from [[{{FULLPAGENAME}}#Ruby|Ruby]], with an adjustment to use 32-bit integers. This code happens to resemble the examples from [http://tools.ietf.org/html/rfc1952#section-8 RFC 1952 section 8] and from [http://www.w3.org/TR/2003/REC-PNG-20031110/#D-CRCAppendix PNG annex D], because those examples use an identical table.


```c>#include <inttypes.h

#include <stdio.h>
#include <string.h>

uint32_t
rc_crc32(uint32_t crc, const char *buf, size_t len)
{
	static uint32_t table[256];
	static int have_table = 0;
	uint32_t rem;
	uint8_t octet;
	int i, j;
	const char *p, *q;

	/* This check is not thread safe; there is no mutex. */
	if (have_table == 0) {
		/* Calculate CRC table. */
		for (i = 0; i < 256; i++) {
			rem = i;  /* remainder from polynomial division */
			for (j = 0; j < 8; j++) {
				if (rem & 1) {
					rem >>= 1;
					rem ^= 0xedb88320;
				} else
					rem >>= 1;
			}
			table[i] = rem;
		}
		have_table = 1;
	}

	crc = ~crc;
	q = buf + len;
	for (p = buf; p < q; p++) {
		octet = *p;  /* Cast to unsigned octet. */
		crc = (crc >> 8) ^ table[(crc & 0xff) ^ octet];
	}
	return ~crc;
}

int
main()
{
	const char *s = "The quick brown fox jumps over the lazy dog";
	printf("%" PRIX32 "\n", rc_crc32(0, s, strlen(s)));
 
	return 0;
}
```



## C++



```cpp>#include <algorithm

#include <array>
#include <cstdint>
#include <numeric>

// These headers are only needed for main(), to demonstrate.
#include <iomanip>
#include <iostream>
#include <string>

// Generates a lookup table for the checksums of all 8-bit values.
std::array<std::uint_fast32_t, 256> generate_crc_lookup_table() noexcept
{
  auto const reversed_polynomial = std::uint_fast32_t{0xEDB88320uL};
  
  // This is a function object that calculates the checksum for a value,
  // then increments the value, starting from zero.
  struct byte_checksum
  {
    std::uint_fast32_t operator()() noexcept
    {
      auto checksum = static_cast<std::uint_fast32_t>(n++);
      
      for (auto i = 0; i < 8; ++i)
        checksum = (checksum >> 1) ^ ((checksum & 0x1u) ? reversed_polynomial : 0);
      
      return checksum;
    }
    
    unsigned n = 0;
  };
  
  auto table = std::array<std::uint_fast32_t, 256>{};
  std::generate(table.begin(), table.end(), byte_checksum{});
  
  return table;
}

// Calculates the CRC for any sequence of values. (You could use type traits and a
// static assert to ensure the values can be converted to 8 bits.)
template <typename InputIterator>
std::uint_fast32_t crc(InputIterator first, InputIterator last)
{
  // Generate lookup table only on first use then cache it - this is thread-safe.
  static auto const table = generate_crc_lookup_table();
  
  // Calculate the checksum - make sure to clip to 32 bits, for systems that don't
  // have a true (fast) 32-bit type.
  return std::uint_fast32_t{0xFFFFFFFFuL} &
    ~std::accumulate(first, last,
      ~std::uint_fast32_t{0} & std::uint_fast32_t{0xFFFFFFFFuL},
        [](std::uint_fast32_t checksum, std::uint_fast8_t value) 
          { return table[(checksum ^ value) & 0xFFu] ^ (checksum >> 8); });
}

int main()
{
  auto const s = std::string{"The quick brown fox jumps over the lazy dog"};
  
  std::cout << std::hex << std::setw(8) << std::setfill('0') << crc(s.begin(), s.end()) << '\n';
}

```

{{out}}

```txt

414fa339

```



```txt

"The quick brown fox jumps over the lazy dog"
(to hex ...)
54686520717569636B2062726F776E20666F78206A756D7073206F76657220746865206C617A7920646F67 414FA339
[other useful test vectors]
0000000000000000000000000000000000000000000000000000000000000000 190A55AD
FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF FF6CAB0B
000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F 91267E8A

```

{{libheader|boost}}

```cpp>#include <boost\crc.hpp

#include <string>
#include <iostream>

int main()
{
    std::string str( "The quick brown fox jumps over the lazy dog" );
    boost::crc_32_type  crc;
    crc.process_bytes( str.data(), str.size() );

    std::cout << "Checksum: " << std::hex << crc.checksum() << std::endl;
    return 0;
}
```

{{out}}

```txt

Checksum: 414fa339

```


=={{header|C sharp|C#}}==

```Csharp

    /// <summary>
    /// Performs 32-bit reversed cyclic redundancy checks.
    /// </summary>
    public class Crc32
    {
        #region Constants
        /// <summary>
        /// Generator polynomial (modulo 2) for the reversed CRC32 algorithm. 
        /// </summary>
        private const UInt32 s_generator = 0xEDB88320;
        #endregion

        #region Constructors
        /// <summary>
        /// Creates a new instance of the Crc32 class.
        /// </summary>
        public Crc32()
        {
            // Constructs the checksum lookup table. Used to optimize the checksum.
            m_checksumTable = Enumerable.Range(0, 256).Select(i =>
            {
                var tableEntry = (uint)i;
                for (var j = 0; j < 8; ++j)
                {
                    tableEntry = ((tableEntry & 1) != 0)
                        ? (s_generator ^ (tableEntry >> 1)) 
                        : (tableEntry >> 1);
                }
                return tableEntry;
            }).ToArray();
        }
        #endregion

        #region Methods
        /// <summary>
        /// Calculates the checksum of the byte stream.
        /// </summary>
        /// <param name="byteStream">The byte stream to calculate the checksum for.</param>
        /// <returns>A 32-bit reversed checksum.</returns>
        public UInt32 Get<T>(IEnumerable<T> byteStream)
        {
            try
            {
                // Initialize checksumRegister to 0xFFFFFFFF and calculate the checksum.
                return ~byteStream.Aggregate(0xFFFFFFFF, (checksumRegister, currentByte) => 
                          (m_checksumTable[(checksumRegister & 0xFF) ^ Convert.ToByte(currentByte)] ^ (checksumRegister >> 8)));
            }
            catch (FormatException e)
            {
                throw new CrcException("Could not read the stream out as bytes.", e);
            }
            catch (InvalidCastException e)
            {
                throw new CrcException("Could not read the stream out as bytes.", e);
            }
            catch (OverflowException e)
            {
                throw new CrcException("Could not read the stream out as bytes.", e);
            }
        }
        #endregion

        #region Fields
        /// <summary>
        /// Contains a cache of calculated checksum chunks.
        /// </summary>
        private readonly UInt32[] m_checksumTable;

        #endregion
    }

```


Test:

```Csharp

	var arrayOfBytes = Encoding.ASCII.GetBytes("The quick brown fox jumps over the lazy dog");

	var crc32 = new Crc32();
	Console.WriteLine(crc32.Get(arrayOfBytes).ToString("X"));

```


{{out}}
414fa339


## Clojure

{{trans|Java}}

```clojure
(let [crc (new java.util.zip.CRC32)
      str "The quick brown fox jumps over the lazy dog"]
  (. crc update (. str getBytes))
  (printf "CRC-32('%s') = %s\n" str (Long/toHexString (. crc getValue))))
```


{{out}}

```txt
CRC-32('The quick brown fox jumps over the lazy dog') = 414fa339

```



## COBOL

{{works with|GnuCOBOL}} {{libheader|zlib}}

```COBOL
      *> tectonics: cobc -xj crc32-zlib.cob -lz
       identification division.
       program-id. rosetta-crc32.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       data division.
       working-storage section.
       01 crc32-initial        usage binary-c-long.
       01 crc32-result         usage binary-c-long unsigned.
       01 crc32-input.
          05 value "The quick brown fox jumps over the lazy dog".
       01 crc32-hex            usage pointer.

       procedure division.
       crc32-main.

      *> libz crc32
       call "crc32" using
           by value crc32-initial
           by reference crc32-input
           by value length(crc32-input)
           returning crc32-result
           on exception
               display "error: no crc32 zlib linkage" upon syserr
       end-call
       call "printf" using "checksum: %lx" & x"0a" by value crc32-result

      *> GnuCOBOL pointers are displayed in hex by default
       set crc32-hex up by crc32-result
       display 'crc32 of "' crc32-input '" is ' crc32-hex

       goback.
       end program rosetta-crc32.
```

{{out}}

```txt
prompt$ cobc -xj crc32-zlib.cob -lz
checksum: 414fa339
crc32 of "The quick brown fox jumps over the lazy dog" is 0x00000000414fa339

```



## CoffeeScript

Allows the specification of the initial CRC value, which defaults to 0xFFFFFFFF. Optimized for speed and terseness (then readability/indentation).

```coffeescript

crc32 = do ->
  table =
    for n in [0..255]
      for [0..7]
        if n & 1
          n = 0xEDB88320 ^ n >>> 1
        else
          n >>>= 1
      n
  (str, crc = -1) ->
    for c in str
      crc = crc >>> 8 ^ table[(crc ^ c.charCodeAt 0) & 255]
    (crc ^ -1) >>> 0

```

Test:

```coffeescript
console.log (crc32 'The quick brown fox jumps over the lazy dog').toString 16
```

Output:
<lang>414fa339
```



## Common Lisp

{{libheader|Ironclad}}

```lisp
(ql:quickload :ironclad)
(defun string-to-digest (str digest)
  "Return the specified digest for the ASCII string as a hex string."
  (ironclad:byte-array-to-hex-string 
    (ironclad:digest-sequence digest 
                              (ironclad:ascii-string-to-byte-array str))))

(string-to-digest "The quick brown fox jumps over the lazy dog" :crc32)

```

{{out}}

```txt
"414fa339"
```



## Component Pascal

BlackBox Component Builder<br/>
Require ZLib Subsystem

```oberon2

MODULE BbtComputeCRC32;
IMPORT ZlibCrc32,StdLog;

PROCEDURE Do*;
VAR
	s: ARRAY 128 OF SHORTCHAR;
BEGIN	
	s := "The quick brown fox jumps over the lazy dog";
	StdLog.IntForm(ZlibCrc32.CRC32(0,s,0,LEN(s$)),16,12,'0',TRUE);
	StdLog.Ln;
END Do;
END BbtComputeCRC32.

```

Execute: ^Q BbtComputeCRC32.Do<br/>
{{out}}

```txt

0414FA339%16

```



## D


```d
void main() {
    import std.stdio, std.digest.crc;

    "The quick brown fox jumps over the lazy dog"
    .crc32Of.crcHexString.writeln;
}
```

{{out}}

```txt
414FA339
```



## Elixir


```elixir
defmodule Test do
  def crc32(str) do
    IO.puts :erlang.crc32(str) |> Integer.to_string(16)
  end
end

Test.crc32("The quick brown fox jumps over the lazy dog")
```


{{out}}

```txt

414FA339

```



## Erlang

Using the built-in crc32 implementation.


```erlang

-module(crc32).
-export([test/0]).
test() ->
  io:fwrite("~.16#~n",[erlang:crc32(<<"The quick brown fox jumps over the lazy dog">>)]).

```


{{out}}

```erlang

16#414FA339

```



## Factor

Like [[SHA-1#Factor]], but with crc32.

 IN: scratchpad '''USING: checksums checksums.crc32 ;'''
 IN: scratchpad '''"The quick brown fox jumps over the lazy dog" crc32'''
                '''checksum-bytes hex-string .'''
 "414fa339"

The implementation is at [https://github.com/slavapestov/factor/blob/master/core/checksums/crc32/crc32.factor core/checksums/crc32/crc32.factor].


## FBSL


```qbasic
#APPTYPE CONSOLE

PRINT HEX(CHECKSUM("The quick brown fox jumps over the lazy dog"))

PAUSE
```

{{out}}

```txt
414FA339

Press any key to continue...
```



## Forth

This code can implement other types of CRC by using other polynomial constants: use $8408 for CCITT CRC-16, or $a001 for IBM CRC-16.


```forth

: crc/ ( n -- n ) 8 0 do dup 1 rshift swap 1 and if $edb88320 xor then loop ;

: crcfill   256 0 do i crc/ , loop ;

create crctbl crcfill

: crc+ ( crc n -- crc' ) over xor $ff and  cells crctbl + @  swap 8 rshift xor ;

: crcbuf ( crc str len -- crc ) bounds ?do i c@ crc+ loop ;

$ffffffff s" The quick brown fox jumps over the lazy dog" crcbuf $ffffffff xor hex.  bye   \ $414FA339

```



## Fortran


```fortran
module crc32_m
    use iso_fortran_env
    implicit none
    integer(int32) :: crc_table(0:255)
contains
    subroutine update_crc(a, crc)
        integer :: n, i
        character(*) :: a
        integer(int32) :: crc
        
        crc = not(crc)
        n = len(a)
        do i = 1, n
            crc = ieor(shiftr(crc, 8), crc_table(iand(ieor(crc, iachar(a(i:i))), 255)))
        end do
        crc = not(crc)
    end subroutine
    
    subroutine init_table
        integer :: i, j
        integer(int32) :: k
        
        do i = 0, 255
            k = i
            do j = 1, 8
                if (btest(k, 0)) then
                    k = ieor(shiftr(k, 1), -306674912)
                else
                    k = shiftr(k, 1)
                end if
            end do
            crc_table(i) = k
        end do
    end subroutine
end module

program crc32
    use crc32_m
    implicit none
    integer(int32) :: crc = 0
    character(*), parameter :: s = "The quick brown fox jumps over the lazy dog"
    call init_table
    call update_crc(s, crc)
    print "(Z8)", crc
end program
```


## FreeBASIC

{{trans|C}}

```freebasic
' version 18-03-2017
' compile with: fbc -s console

Function crc32(buf As String) As UInteger<32>

    Static As UInteger<32> table(256)
    Static As UInteger<32> have_table
    Dim As UInteger<32> crc, k
    Dim As ULong i, j

    If have_table = 0 Then
        For i = 0 To 255
            k = i
            For j = 0 To 7
                If (k And 1) Then
                    k Shr= 1
                    k Xor= &Hedb88320
                Else
                    k Shr= 1
                End If
                table(i) = k
            Next
        Next
        have_table = 1
    End If

    crc = Not crc ' crc = &Hffffffff
    
    For i = 0 To Len(buf) -1
        crc = (crc Shr 8) Xor table((crc And &hff) Xor buf[i])
    Next

    Return Not crc

End Function

' ------=< MAIN >=------

Dim As String l = "The quick brown fox jumps over the lazy dog"
Dim As UInteger<32> crc

Print "input = "; l
print
Print "The CRC-32 checksum = "; Hex(crc32(l), 8)

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
input = The quick brown fox jumps over the lazy dog

The CRC-32 checksum = 414FA339
```



## Go


### Library


```go
package main

import (
    "fmt"
    "hash/crc32"
)

func main() {
    s := []byte("The quick brown fox jumps over the lazy dog")
    result := crc32.ChecksumIEEE(s)
    fmt.Printf("%X\n", result)
}
```

{{out}}

```txt
414FA339
```



### Implementation


```go
package main

import "fmt"

var table [256]uint32

func init() {
    for i := range table {
        word := uint32(i)
        for j := 0; j < 8; j++ {
            if word&1 == 1 {
                word = (word >> 1) ^ 0xedb88320
            } else {
                word >>= 1
            }
        }
        table[i] = word
    }
}

func crc32(s string) uint32 {
    crc := ^uint32(0)
    for i := 0; i < len(s); i++ {
        crc = table[byte(crc)^s[i]] ^ (crc >> 8)
    }
    return ^crc
}

func main() {
    fmt.Printf("%0x\n", crc32("The quick brown fox jumps over the lazy dog"))
}
```

{{out}}

```txt

414fa339

```



## Groovy


```Groovy
def crc32(byte[] bytes) {
    new java.util.zip.CRC32().with { update bytes; value }
}
```

Testing:

```Groovy
assert '414FA339' == sprintf('%04X', crc32('The quick brown fox jumps over the lazy dog'.bytes))
```




## Haskell


Pure Haskell:


```haskell
import Data.Bits ((.&.), complement, shiftR, xor)
import Data.Word (Word32)
import Numeric (showHex)

crcTable :: Word32 -> Word32
crcTable = (table !!) . fromIntegral
  where
    table = ((!! 8) . iterate xf) <$> [0 .. 255]
    shifted x = shiftR x 1
    xf r
      | r .&. 1 == 1 = xor (shifted r) 0xedb88320
      | otherwise = shifted r

charToWord :: Char -> Word32
charToWord = fromIntegral . fromEnum

calcCrc :: String -> Word32
calcCrc = complement . foldl cf (complement 0)
  where
    cf crc x = xor (shiftR crc 8) (crcTable $ xor (crc .&. 0xff) (charToWord x))

crc32 :: String -> String
crc32 = flip showHex [] . calcCrc

main :: IO ()
main = putStrLn $ crc32 "The quick brown fox jumps over the lazy dog"
```

{{Out}}

```txt
414fa339
```



Using the zlib C library ( compile with "ghc -lz file.hs"):


```haskell
import Data.List (genericLength)
import Numeric (showHex)
import Foreign.C

foreign import ccall "zlib.h crc32" zlib_crc32 ::
               CULong -> CString -> CUInt -> CULong

main :: IO ()
main = do
  let s = "The quick brown fox jumps over the lazy dog"
  ptr <- newCString s
  let r = zlib_crc32 0 ptr (genericLength s)
  putStrLn $ showHex r ""
```

{{Out}}

```txt
414fa339
```


=={{header|Icon}} and {{header|Unicon}}==
There is no library function for this so we implement one.  Icon/Unicon binary operations apply to large integers so we need to mask to the desired unsigned word size. This also only applies to full bytes.

```Icon
link hexcvt,printf

procedure main()
   s := "The quick brown fox jumps over the lazy dog"
   a := "414FA339"
   printf("crc(%i)=%s - implementation is %s\n",
          s,r := crc32(s),if r == a then "correct" else "in error")
end

procedure crc32(s)      #: return crc-32 (ISO 3309, ITU-T V.42, Gzip, PNG) of s
static crcL,mask
initial {
   crcL := list(256)                            # crc table
   p := [0,1,2,4,5,7,8,10,11,12,16,22,23,26]    # polynomial terms 
   mask := 2^32-1                               # word size mask   
   every (poly := 0) := ior(poly,ishift(1,31-p[1 to *p]))
   every c := n := 0 to *crcL-1 do {            # table 
      every 1 to 8 do 
         c := iand(mask, 
                   if iand(c,1) = 1 then
                      ixor(poly,ishift(c,-1)) 
                   else 
                      ishift(c,-1)
                  )
      crcL[n+1] := c
      }
   }
   
   crc := ixor(0,mask)                          # invert bits
   every crc := iand(mask,
                     ixor(crcL[iand(255,ixor(crc,ord(!s)))+1],ishift(crc,-8)))               
   return hexstring(ixor(crc,mask))             # return hexstring
end
```


{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/hexcvt.icn hexcvt.icn] (provides hex and hexstring)
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn] (provides formatting) 

{{out}}

```txt
crc("The quick brown fox jumps over the lazy dog")=414FA339 - implementation is correct
```



## J


```j
   ((i.32) e. 32 26 23 22 16 12 11 10 8 7 5 4 2 1 0) 128!:3 'The quick brown fox jumps over the lazy dog'
_3199229127
```


Other possible representations of this result:


```j
   (2^32x)|((i.32) e. 32 26 23 22 16 12 11 10 8 7 5 4 2 1 0) 128!:3 'The quick brown fox jumps over the lazy dog'
1095738169
   require'convert'
   hfd (2^32x)|((i.32) e. 32 26 23 22 16 12 11 10 8 7 5 4 2 1 0) 128!:3 'The quick brown fox jumps over the lazy dog'
414FA339
```



## Java


```Java
import java.util.zip.* ;

public class CRCMaker {
   public static void main( String[ ] args ) {
      String toBeEncoded = new String( "The quick brown fox jumps over the lazy dog" ) ;
      CRC32 myCRC = new CRC32( ) ;
      myCRC.update( toBeEncoded.getBytes( ) ) ;
      System.out.println( "The CRC-32 value is : " + Long.toHexString( myCRC.getValue( ) ) + " !" ) ;
   }
}
```

{{out}}

```txt
The CRC-32 value is : 414fa339 !
```



## JavaScript


```JavaScript
(() => {
    'use strict';

    const main = () =>
        showHex(
            crc32('The quick brown fox jumps over the lazy dog')
        );

    // crc32 :: String -> Int
    const crc32 = str => {

        // table :: [Int]
        const table = map(
            n => take(9,
                iterate(
                    x => (
                        x & 1 ? z => 0xEDB88320 ^ z : id
                    )(x >>> 1),
                    n
                )
            )[8],
            enumFromTo(0, 255)
        );
        return (
            foldl(
                (a, c) => (a >>> 8) ^ table[
                    (a ^ c.charCodeAt(0)) & 255
                ],
                -1,
                chars(str)
            ) ^ -1
        );
    };

    // GENERIC ABSTRACTIONS -------------------------------

    // chars :: String -> [Char]
    const chars = s => s.split('');

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: 1 + n - m
        }, (_, i) => m + i);

    // foldl :: (a -> b -> a) -> a -> [b] -> a
    const foldl = (f, a, xs) => xs.reduce(f, a);

    // id :: a -> a
    const id = x => x;

    // iterate :: (a -> a) -> a -> Gen [a]
    function* iterate(f, x) {
        let v = x;
        while (true) {
            yield(v);
            v = f(v);
        }
    }

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // showHex :: Int -> String
    const showHex = n =>
        n.toString(16);

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        xs.constructor.constructor.name !== 'GeneratorFunction' ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));


    // MAIN -------------
    const result = main();
    return (
        console.log(result),
        result
    );
})();
```

{{Out}}

```txt
414fa339
```



## Jsish

From the shell

```javascript
# Util.crc32('The quick brown fox jumps over the lazy dog').toString(16);
```

{{out}}

```txt
"414fa339"
```



## Julia


### Using the zlib Library


```julia
using Libz
println(string(Libz.crc32(UInt8.(b"The quick brown fox jumps over the lazy dog")), base=16))

```
{{out}}

```txt

414fa339

```



### Source Implementation

{{works with|Julia|0.6}}

```Julia
function crc32(crc::Int, str::String)
    table = zeros(UInt32, 256)

    for i in 0:255
        tmp = i
        for j in 0:7
            if tmp & 1 == 1
                tmp >>= 1
                tmp ⊻= 0xedb88320
            else
                tmp >>= 1
            end
        end

        table[i + 1] = tmp
    end

    crc ⊻= 0xffffffff

    for i in UInt32.(collect(str))
        crc = (crc >> 8) ⊻ table[(crc & 0xff) ⊻ i + 1]
    end

    crc ⊻ 0xffffffff
end

str = "The quick brown fox jumps over the lazy dog"
crc = crc32(0, str)
assert(crc == 0x414fa339)
println("Message: ", str)
println("Checksum: ", hex(crc))
```


{{out}}

```txt
Message: The quick brown fox jumps over the lazy dog
Checksum: 414fa339
```




## Kotlin


```scala
// version 1.0.6

import java.util.zip.CRC32

fun main(args: Array<String>) {
    val text = "The quick brown fox jumps over the lazy dog"
    val crc = CRC32()
    with (crc) {
        update(text.toByteArray())
        println("The CRC-32 checksum of '$text' = ${"%x".format(value)}")
    }
}
```


{{out}}

```txt

The CRC-32 checksum of 'The quick brown fox jumps over the lazy dog' = 414fa339

```



## Lingo



### Pure Lingo



```lingo
crcObj = script("CRC").new()

crc32 = crcObj.crc32("The quick brown fox jumps over the lazy dog")

put crc32
-- <ByteArrayObject length = 4 ByteArray = 0x41, 0x4f, 0xa3, 0x39 >

put crc32.toHexString(1, crc32.length)
-- "41 4f a3 39"
```


Implementation:


```lingo
--****************************************************************************
-- @desc      CRC-32 Class
-- @file      parent script "CRC"
-- @version   0.1
--****************************************************************************

property _CRC32Table

----------------------------------------
-- @constructor
----------------------------------------
on new me

  -- used for fast CRC32 calculation
  me._CRC32Table = [\
  0, 1996959894, -301047508, -1727442502, 124634137, 1886057615, -379345611, -1637575261, 249268274, 2044508324,\
  -522852066, -1747789432, 162941995, 2125561021, -407360249, -1866523247, 498536548, 1789927666, -205950648,\
  -2067906082, 450548861, 1843258603, -187386543, -2083289657, 325883990, 1684777152, -43845254, -1973040660,\
  335633487, 1661365465, -99664541, -1928851979, 997073096, 1281953886, -715111964, -1570279054, 1006888145,\
  1258607687, -770865667, -1526024853, 901097722, 1119000684, -608450090, -1396901568, 853044451, 1172266101,\
  -589951537, -1412350631, 651767980, 1373503546, -925412992, -1076862698, 565507253, 1454621731, -809855591,\
  -1195530993, 671266974, 1594198024, -972236366, -1324619484, 795835527, 1483230225, -1050600021, -1234817731,\
  1994146192, 31158534, -1731059524, -271249366, 1907459465, 112637215, -1614814043, -390540237, 2013776290,\
  251722036, -1777751922, -519137256, 2137656763, 141376813, -1855689577, -429695999, 1802195444, 476864866,\
  -2056965928, -228458418, 1812370925, 453092731, -2113342271, -183516073, 1706088902, 314042704, -1950435094,\
  -54949764, 1658658271, 366619977, -1932296973, -69972891, 1303535960, 984961486, -1547960204, -725929758,\
  1256170817, 1037604311, -1529756563, -740887301, 1131014506, 879679996, -1385723834, -631195440, 1141124467,\
  855842277, -1442165665, -586318647, 1342533948, 654459306, -1106571248, -921952122, 1466479909, 544179635,\
  -1184443383, -832445281, 1591671054, 702138776, -1328506846, -942167884, 1504918807, 783551873, -1212326853,\
  -1061524307, -306674912, -1698712650, 62317068, 1957810842, -355121351, -1647151185, 81470997, 1943803523,\
  -480048366, -1805370492, 225274430, 2053790376, -468791541, -1828061283, 167816743, 2097651377, -267414716,\
  -2029476910, 503444072, 1762050814, -144550051, -2140837941, 426522225, 1852507879, -19653770, -1982649376,\
  282753626, 1742555852, -105259153, -1900089351, 397917763, 1622183637, -690576408, -1580100738, 953729732,\
  1340076626, -776247311, -1497606297, 1068828381, 1219638859, -670225446, -1358292148, 906185462, 1090812512,\
  -547295293, -1469587627, 829329135, 1181335161, -882789492, -1134132454, 628085408, 1382605366, -871598187,\
  -1156888829, 570562233, 1426400815, -977650754, -1296233688, 733239954, 1555261956, -1026031705, -1244606671,\
  752459403, 1541320221, -1687895376, -328994266, 1969922972, 40735498, -1677130071, -351390145, 1913087877,\
  83908371, -1782625662, -491226604, 2075208622, 213261112, -1831694693, -438977011, 2094854071, 198958881,\
  -2032938284, -237706686, 1759359992, 534414190, -2118248755, -155638181, 1873836001, 414664567, -2012718362,\
  -15766928, 1711684554, 285281116, -1889165569, -127750551, 1634467795, 376229701, -1609899400, -686959890,\
  1308918612, 956543938, -1486412191, -799009033, 1231636301, 1047427035, -1362007478, -640263460, 1088359270,\
  936918000, -1447252397, -558129467, 1202900863, 817233897, -1111625188, -893730166, 1404277552, 615818150,\
  -1160759803, -841546093, 1423857449, 601450431, -1285129682, -1000256840, 1567103746, 711928724, -1274298825,\
  -1022587231, 1510334235, 755167117]
  return me
end

----------------------------------------
-- Calculates CRC-32 checksum of string or bytearray
-- @param {bytearray|string} input
-- @return {bytearray} (4 bytes)
----------------------------------------
on crc32 (me, input)
  if stringP(input) then input = bytearray(input)
  crc = -1
  len = input.length
  repeat with i = 1 to len
    if (crc>0) then bitShift8 = crc/256
    else bitShift8 = bitAnd(crc,2147483647)/256+8388608
    crc = bitXor(bitShift8,me._CRC32Table[bitAnd(bitXor(crc,input[i]),255)+1])
  end repeat
  ba = bytearray()
  ba.endian = #bigEndian
  ba.writeInt32(bitXOr(crc,-1))
  ba.position = 1
  return ba
end
```


===Using an "Xtra" (=binary plugin)===


```lingo
cx = Xtra("Crypto").new()
put cx.cx_crc32_string("The quick brown fox jumps over the lazy dog")
-- "414fa339"
```




## Lua


### Using Library

[https://github.com/brimworks/lua-zlib <code>zlib.crc32</code>]


```lua
local compute=require"zlib".crc32()
local sum=compute("The quick brown fox jumps over the lazy dog")
print(string.format("0x%x", sum))

```


{{out}}
0x414fa339


## M2000 Interpreter


### Using Code


```M2000 Interpreter

Module CheckIt {
      Function PrepareTable {
            Dim Base 0, table(256)
            For i = 0 To 255 {
                    k = i
                    For j = 0 To 7 {
                              If binary.and(k,1)=1 Then {
                                  k =binary.Xor(binary.shift(k, -1) ,  0xEDB88320)
                              }  Else k=binary.shift(k, -1)
                    }
                    table(i) = k
             }
             =table()      
      }       
      crctable=PrepareTable()
      crc32= lambda crctable (buf$) -> {
                crc =0xFFFFFFFF
                For i = 0 To Len(buf$) -1
                     crc = binary.xor(binary.shift(crc, -8), array(crctable, binary.xor(binary.and(crc, 0xff), asc(mid$(buf$, i+1, 1)))))
                Next i
                =0xFFFFFFFF-crc       
      }
      Print crc32("The quick brown fox jumps over the lazy dog")=0x414fa339
}
CheckIt

```



### Using Api


```M2000 Interpreter

Module CheckApi {
      Declare CRC32 LIB "ntdll.RtlComputeCrc32" {Long Zero, a$, long s}
      a$=Str$("The quick brown fox jumps over the lazy dog")
      l=len(a$)*2
      Hex Uint(CRC32(0,a$,l))
}
CheckApi

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
type="CRC32"; (*pick one out of 13 predefined hash types*)
StringForm[
"The "<>type<>" hash code of \"``\" is ``.",
s="The quick brown fox jumps over the lazy dog",
Hash[s,type,"HexString"]
]

```

{{out}}

```txt

The CRC32 hash code of "The quick brown fox jumps over the lazy dog" is 414fa339.

```



## Neko

The NekoVM is a 31 bit machine; 30 signed.  Loadable primitives handle 32bit integers.  The zlib library API exposes a CRC-32 function, that expects and returns Int32.


```ActionScript
/**
 <doc>CRC32 in Neko</doc>
**/

var int32_new = $loader.loadprim("std@int32_new", 1)
var update_crc32 = $loader.loadprim("zlib@update_crc32", 4)

var crc = int32_new(0)
var txt = "The quick brown fox jumps over the lazy dog"

crc = update_crc32(crc, txt, 0, $ssize(txt))
$print(crc, "\n")
```


{{out}}

```txt
prompt$ nekoc crc32.neko
prompt$ neko crc32.n
1095738169
prompt$ dc -e "$(neko crc32.n) 16op"
414FA339
```



## NetRexx

{{trans|Java}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

import java.util.zip.CRC32

toBeEncoded = String("The quick brown fox jumps over the lazy dog")
myCRC = CRC32()
myCRC.update(toBeEncoded.getBytes())
say "The CRC-32 value is :" Long.toHexString(myCRC.getValue()) "!"

return

```


{{out}}

```txt

The CRC-32 value is : 414fa339 !

```



## Nim


```nim
import unsigned, strutils

type TCrc32* = uint32
const InitCrc32* = TCrc32(-1)

proc createCrcTable(): array[0..255, TCrc32] =
  for i in 0..255:
    var rem = TCrc32(i)
    for j in 0..7:
      if (rem and 1) > 0: rem = (rem shr 1) xor TCrc32(0xedb88320)
      else: rem = rem shr 1
    result[i] = rem

# Table created at compile time
const crc32table = createCrcTable()

proc crc32(s: string): TCrc32 =
  result = InitCrc32
  for c in s:
    result = (result shr 8) xor crc32table[(result and 0xff) xor ord(c)]
  result = not result

echo crc32("The quick brown fox jumps over the lazy dog").int64.toHex(8)
```

{{out}}

```txt
414FA339
```



## Objeck


```objeck
class CRC32 {
  function : Main(args : String[]) ~ Nil {
    "The quick brown fox jumps over the lazy dog"->ToByteArray()->CRC32()->PrintLine();
  }
}

```


{{out}}

```txt

1095738169

```


=={{header|Oberon-2}}==
{{Works with|oo2c Version 2}}

```oberon2

MODULE CRC32;
IMPORT
  NPCT:Zlib,
  Strings,
  Out;
VAR
  s: ARRAY 128 OF CHAR;
BEGIN
  COPY("The quick brown fox jumps over the lazy dog",s);
  Out.Hex(Zlib.CRC32(0,s,0,Strings.Length(s)),0);Out.Ln
END CRC32.

```

{{out}}

```txt

414FA339

```



## OCaml


{{libheader|camlzip}}


```ocaml
let () =
  let s = "The quick brown fox jumps over the lazy dog" in
  let crc = Zlib.update_crc 0l s 0 (String.length s) in
  Printf.printf "crc: %lX\n" crc
```


Running this code in interpreted mode:<nowiki>[[Media:Insert non-formatted text here]][[File:[Example.jpg][http://www.example.com link title]]]</nowiki>


```txt

$ ocaml unix.cma -I +zip zip.cma crc.ml
crc: 414FA339

```



## ooRexx

This Program shows how easy it is to use JAVA functionality from ooRexx.
bsf4oorexx from Sourceforge https://sourceforge.net/projects/bsf4oorexx/ makes that possible.

```oorexx
/* ooRexx */
clzCRC32=bsf.importClass("java.util.zip.CRC32")
myCRC32 =clzCRC32~new
toBeEncoded="The quick brown fox jumps over the lazy dog"
myCRC32~update(BsfRawBytes(toBeEncoded))
numeric digits 20
say 'The CRC-32 value of "'toBeEncoded'" is:' myCRC32~getValue~d2x

::requires "BSF.CLS"    -- get Java bridge      
```

{{out}}

```txt
The CRC-32 value of "The quick brown fox jumps over the lazy dog" is: 414FA339
```



## PARI/GP


Using Linux system library (Linux only solution)
{{libheader|libz.so}}


```parigp

install("crc32", "lLsL", "crc32", "libz.so");
s = "The quick brown fox jumps over the lazy dog";
printf("%0x\n", crc32(0, s, #s))

```


Output:

```txt
414fa339
```



## Perl


```Perl
#!/usr/bin/perl
use 5.010 ;
use strict ;
use warnings ;
use Digest::CRC qw( crc32 ) ;

my $crc = Digest::CRC->new( type => "crc32" ) ;
$crc->add ( "The quick brown fox jumps over the lazy dog" )  ;
say "The checksum is " . $crc->hexdigest( ) ;

```

{{out}}

```txt
The checksum is 414fa339
```



## Perl 6



###  Call to native function crc32 in zlib 



```perl6
use NativeCall;
 
sub crc32(int32 $crc, Buf $buf, int32 $len --> int32) is native('z') { * }
 
my $buf = 'The quick brown fox jumps over the lazy dog'.encode;
say crc32(0, $buf, $buf.bytes).fmt('%08x');
```


The libary name "z" resolves to <tt>/usr/lib/libz.so</tt> on a typical Linux system and <tt>/usr/lib/libz.dylib</tt> on Mac OS X, but may need to be changed for other platforms. Types may be platform-dependent as well. As written, the solution has been tested on Mac OS X 10.5.8 and Arch Linux 2016.08.01 x86_64.

{{out}}

```txt
414fa339
```



###  Pure Perl 6 


A fairly generic implementation with no regard to execution speed:


```perl6
sub crc(
    Blob $buf,
             # polynomial including leading term, default: ISO 3309/PNG/gzip
    :@poly = (1,0,0,0,0,0,1,0,0,1,1,0,0,0,0,0,1,0,0,0,1,1,1,0,1,1,0,1,1,0,1,1,1),
    :$n = @poly.end,      # degree of polynomial
    :@init = 1 xx $n,     # initial XOR bits
    :@fini = 1 xx $n,     # final XOR bits
    :@bitorder = 0..7,    # default: eat bytes LSB-first
    :@crcorder = 0..$n-1, # default: MSB of checksum is coefficient of x⁰
) {
    my @bit = flat ($buf.list X+& (1 X+< @bitorder))».so».Int, 0 xx $n;

    @bit[0   .. $n-1] «+^=» @init;
    @bit[$_  ..$_+$n] «+^=» @poly if @bit[$_] for 0..@bit.end-$n;
    @bit[*-$n..  *-1] «+^=» @fini;

    :2[@bit[@bit.end X- @crcorder]];
}

say crc('The quick brown fox jumps over the lazy dog'.encode('ascii')).base(16);
```


{{out}}

```txt
414FA339
```



## Phix

Included as demo\rosetta\crc32.exw, which also includes a thread-safe version

```Phix
sequence table
integer have_table = 0

procedure make_crc()
atom rem
    if have_table=0 then
        have_table = 1
        table = repeat(0,256)
        for i=0 to 255 do
            rem = i
            for j=1 to 8 do
                if and_bits(rem,1) then
                    rem = xor_bits(floor(rem/2),#EDB88320)
                else
                    rem = floor(rem/2)
                end if
                if rem<0 then
                    rem += #100000000
                end if
            end for
            table[i+1] = rem
        end for
    end if
end procedure

function crc32(string s)
atom crc = #FFFFFFFF
    if have_table=0 then make_crc() end if
    for i=1 to length(s) do
        crc = xor_bits(floor(crc/#100),table[xor_bits(and_bits(crc,0xff),s[i])+1])
        if crc<0 then
            crc += #100000000
        end if
    end for
--  return not_bits(crc)
    return and_bits(not_bits(crc),#FFFFFFFF)
end function
```

Test code:

```Phix
string s = "The quick brown fox jumps over the lazy dog"
printf(1,"The CRC of %s is %08x\n",{s,crc32(s)})
```

{{out}}

```txt

The CRC of The quick brown fox jumps over the lazy dog is 414FA339

```



## PHP

PHP has a built-in function [http://us2.php.net/manual/en/function.crc32.php crc32].


```php
printf("%x\n", crc32("The quick brown fox jumps over the lazy dog"));
```



```txt
414fa339
```



## PL/I


```pli
*process source attributes xref or(!) nest;
 crct: Proc Options(main);
 /*********************************************************************
 * 19.08.2013 Walter Pachl  derived from REXX
 *********************************************************************/
 Dcl (LEFT,LENGTH,RIGHT,SUBSTR,UNSPEC) Builtin;
 Dcl SYSPRINT Print;
 dcl tab(0:255) Bit(32);
 Call mk_tab;
 Call crc_32('The quick brown fox jumps over the lazy dog');
 Call crc_32('Generate CRC32 Checksum For Byte Array Example');

 crc_32: Proc(s);
 /*********************************************************************
 * compute checksum for s
 *********************************************************************/
 Dcl s Char(*);
 Dcl d   Bit(32);
 Dcl d1  Bit( 8);
 Dcl d2  Bit(24);
 Dcl cc  Char(1);
 Dcl ccb Bit(8);
 Dcl tib Bit(8);
 Dcl ti  Bin Fixed(16) Unsigned;
 Dcl k   Bin Fixed(16) Unsigned;
 d=(32)'1'b;
 Do k=1 To length(s);
    d1=right(d,8);
    d2=left(d,24);
    cc=substr(s,k,1);
    ccb=unspec(cc);
    tib=d1^ccb;
    Unspec(ti)=tib;
    d='00000000'b!!d2^tab(ti);
    End;
  d=d^(32)'1'b;
  Put Edit(s,'CRC_32=',b2x(d))(Skip,a(50),a,a);
  Put Edit('decimal ',b2d(d))(skip,x(49),a,f(10));
 End;

 b2x: proc(b) Returns(char(8));
 dcl b bit(32);
 dcl b4 bit(4);
 dcl i Bin Fixed(31);
 dcl r Char(8) Var init('');
 Do i=1 To 29 By 4;
   b4=substr(b,i,4);
   Select(b4);
     When('0000'b) r=r!!'0';
     When('0001'b) r=r!!'1';
     When('0010'b) r=r!!'2';
     When('0011'b) r=r!!'3';
     When('0100'b) r=r!!'4';
     When('0101'b) r=r!!'5';
     When('0110'b) r=r!!'6';
     When('0111'b) r=r!!'7';
     When('1000'b) r=r!!'8';
     When('1001'b) r=r!!'9';
     When('1010'b) r=r!!'A';
     When('1011'b) r=r!!'B';
     When('1100'b) r=r!!'C';
     When('1101'b) r=r!!'D';
     When('1110'b) r=r!!'E';
     When('1111'b) r=r!!'F';
     End;
   End;
 Return(r);
 End;

 b2d: Proc(b) Returns(Dec Fixed(15));
 Dcl b Bit(32);
 Dcl r Dec Fixed(15) Init(0);
 Dcl i Bin Fixed(16);
 Do i=1 To 32;
   r=r*2
   If substr(b,i,1) Then
     r=r+1;
   End;
 Return(r);
 End;

 mk_tab: Proc;
 dcl b32 bit(32);
 dcl lb  bit( 1);
 dcl ccc bit(32) Init('edb88320'bx);
 dcl (i,j) Bin Fixed(15);
 Do i=0 To 255;
   b32=(24)'0'b!!unspec(i);
   Do j=0 To 7;
     lb=right(b32,1);
     b32='0'b!!left(b32,31);
     If lb='1'b Then
       b32=b32^ccc;
     End;
   tab(i)=b32;
   End;
 End;
 End;
```

{{out}}

```txt

The quick brown fox jumps over the lazy dog       CRC_32=414FA339
                                                 decimal 1095738169
Generate CRC32 Checksum For Byte Array Example    CRC_32=D1370232
                                                 decimal 3510043186 

```
           


## PicoLisp

Library and implementation.


```PicoLisp
(setq *Table
   (mapcar
      '((N)
         (do 8
            (setq N
               (if (bit? 1 N)
                  (x| (>> 1 N) `(hex "EDB88320"))
                  (>> 1 N) ) ) ) )
      (range 0 255) ) )
 
(de crc32 (Lst)
   (let Crc `(hex "FFFFFFFF")
      (for I (chop Lst)
         (setq Crc
            (x|
               (get
                  *Table
                  (inc (x| (& Crc 255) (char I))) )
               (>> 8 Crc) ) ) )
      (x| `(hex "FFFFFFFF") Crc) ) )
 
(let Str "The quick brown fox jumps over the lazy dog"
   (println (hex (crc32 Str)))
   (println
      (hex (native "libz.so" "crc32" 'N 0 Str (length Str))) ) )
 
(bye)
```



## PowerBASIC


```powerbasic
#COMPILE EXE
#DIM ALL
#COMPILER PBCC 6

' ***********

FUNCTION CRC32(BYVAL p AS BYTE PTR, BYVAL NumBytes AS DWORD) AS DWORD
  STATIC LUT() AS DWORD
  LOCAL i, j, k, crc AS DWORD

  IF ARRAYATTR(LUT(), 0) = 0 THEN
    REDIM LUT(0 TO 255)
    FOR i = 0 TO 255
      k = i
      FOR j = 0 TO 7
        IF (k AND 1) THEN
          SHIFT RIGHT k, 1
          k XOR= &HEDB88320
        ELSE
          SHIFT RIGHT k, 1
        END IF
      NEXT j
      LUT(i) = k
    NEXT i
  END IF

  crc = &HFFFFFFFF

  FOR i = 0 TO NumBytes - 1
    k = (crc AND &HFF& XOR @p[i])
    SHIFT RIGHT crc, 8
    crc XOR= LUT(k)
  NEXT i

  FUNCTION = NOT crc
END FUNCTION

' ***********

FUNCTION PBMAIN () AS LONG
  LOCAL s AS STRING
  LOCAL crc AS DWORD

  s = "The quick brown fox jumps over the lazy dog"
  CON.PRINT "Text:  " & s
  crc = CRC32(STRPTR(s), LEN(s))
  CON.PRINT "CRC32: " & HEX$(crc)
END FUNCTION
```

{{out}}

```txt
Text:  The quick brown fox jumps over the lazy dog
CRC32: 414FA339
```



## PureBasic

{{works with|PB Version 5.40}}

```PureBasic

a$="The quick brown fox jumps over the lazy dog"

UseCRC32Fingerprint() : b$=StringFingerprint(a$, #PB_Cipher_CRC32)

OpenConsole()
PrintN("CRC32 Cecksum [hex] = "+UCase(b$))
PrintN("CRC32 Cecksum [dec] = "+Val("$"+b$))
Input()

End
```

{{out}}
```txt
CRC32 Cecksum [hex] = 414FA339
CRC32 Cecksum [dec] = 1095738169
```



## Python


### Library

[http://docs.python.org/library/zlib.html#zlib.crc32 <code>zlib.crc32</code>] and [http://docs.python.org/library/binascii.html#binascii.crc32 <code>binascii.crc32</code>] give identical results.


```python>>>
 s = 'The quick brown fox jumps over the lazy dog'
>>> import zlib
>>> hex(zlib.crc32(s))
'0x414fa339'

>>> import binascii
>>> hex(binascii.crc32(s))
'0x414fa339'
```


If you have Python 2.x, these functions might return a negative integer; you would need to use <code>& 0xffffffff</code> to get the same answer as Python 3.x. With Python 3.x, convert first the string to bytes, for instance with <code>s.encode('UTF-8')</code>, as these functions do not accept strings.

### Implementation


### =Procedural=


```python
def create_table():
    a = []
    for i in range(256):
        k = i
        for j in range(8):
            if k & 1:
                k ^= 0x1db710640
            k >>= 1
        a.append(k)
    return a

def crc_update(buf, crc):
    crc ^= 0xffffffff
    for k in buf:
        crc = (crc >> 8) ^ crc_table[(crc & 0xff) ^ k]
    return crc ^ 0xffffffff
    
crc_table = create_table()
print(hex(crc_update(b"The quick brown fox jumps over the lazy dog", 0)))
```



### =Composition of pure functions=


```Python
'''CRC-32 checksums for ascii strings'''

from functools import (reduce)
from itertools import (islice)


# crc32 :: String -> Int
def crc32(s):
    '''CRC-32 checksum for an ASCII encoded string'''
    def go(x):
        x2 = x >> 1
        return 0xedb88320 ^ x2 if x & 1 else x2
    table = [
        index(iterate(go)(n))(8)
        for n in range(0, 256)
    ]
    return reduce(
        lambda a, c: (a >> 8) ^ table[
            (a ^ ord(c)) & 0xff
        ],
        list(s),
        (0xffffffff)
    ) ^ 0xffffffff


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Test'''
    print(
        format(
            crc32('The quick brown fox jumps over the lazy dog'),
            '02x'
        )
    )


# GENERIC ABSTRACTION -------------------------------------

# index (!!) :: [a] -> Int -> a
def index(xs):
    '''Item at given (zero-based) index.'''
    return lambda n: None if 0 > n else (
        xs[n] if (
            hasattr(xs, "__getitem__")
        ) else next(islice(xs, n, None))
    )


# iterate :: (a -> a) -> a -> Gen [a]
def iterate(f):
    '''An infinite list of repeated applications of f to x.'''
    def go(x):
        v = x
        while True:
            yield v
            v = f(v)
    return lambda x: go(x)


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
414fa339
```



## R


```R

digest("The quick brown fox jumps over the lazy dog","crc32", serialize=F)

```

{{out}}

```txt
[1] "414fa339"
```



## Racket


```scheme
#lang racket
(define (bytes-crc32 data)
  (bitwise-xor
   (for/fold ([accum #xFFFFFFFF])
     ([byte  (in-bytes data)])
     (for/fold ([accum (bitwise-xor accum byte)])
       ([num (in-range 0 8)])
       (bitwise-xor (quotient accum 2)
                    (* #xEDB88320 (bitwise-and accum 1)))))
   #xFFFFFFFF))

(define (crc32 s)
  (bytes-crc32 (string->bytes/utf-8 s)))

(format "~x" (crc32 "The quick brown fox jumps over the lazy dog"))
```

{{out}}

```txt
"414fa339"
```



## REXX


```rexx
/*REXX program computes the  CRC─32  (32 bit Cyclic Redundancy Check)  checksum  for a  */
/*─────────────────────────────────given string  [as described in ISO 3309, ITU─T V.42].*/
call show  'The quick brown fox jumps over the lazy dog'               /*the 1st string.*/
call show  'Generate CRC32 Checksum For Byte Array Example'            /* "  2nd    "   */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
CRC_32: procedure; parse arg !,$;  c='edb88320'x /*2nd arg used for repeated invocations*/
                                   f='ffFFffFF'x /* [↓]  build an  8─bit  indexed table,*/
            do i=0  for 256;       z=d2c(i)      /*                  one byte at a time.*/
            r=right(z, 4, '0'x)                  /*insure the  "R"   is thirty-two bits.*/
                                                 /* [↓]  handle each rightmost byte bit.*/
              do j=0  for 8;      rb=x2b(c2x(r)) /*handle each bit of rightmost 8 bits. */
              r=x2c(b2x(0 || left(rb, 31)))      /*shift it right (an unsigned)  1  bit.*/
              if right(rb,1)  then r=bitxor(r,c) /*this is a bin bit for XOR grunt─work.*/
              end    /*j*/
            !.z=r                                /*assign to an eight─bit index table.  */
            end      /*i*/

        $=bitxor(word($ '0000000'x, 1), f)       /*utilize the user's CRC or a default. */
            do k=1  for length(!)                /*start number crunching the input data*/
            ?=bitxor(right($,1), substr(!,k,1))
            $=bitxor('0'x || left($, 3),  !.?)
            end   /*k*/
        return $                                 /*return with cyclic redundancy check. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:   procedure;   parse arg Xstring;   numeric digits 12;      say;     say
        checksum=CRC_32(Xstring)                       /*invoke  CRC_32 to create a CRC.*/
        checksum=bitxor(checksum, 'ffFFffFF'x)         /*final convolution for checksum.*/
        say center(' input string [length of' length(Xstring) "bytes] ", 79, '═')
        say Xstring                                    /*show the string on its own line*/
        say                                            /*↓↓↓↓↓↓↓↓↓↓↓↓  is fifteen blanks*/
        say  "hex CRC-32 checksum ="   c2x(checksum)     left('', 15),
             "dec CRC-32 checksum ="   c2d(checksum)   /*show the CRC-32 in hex and dec.*/
        return
```

'''output'''

```txt

══════════════════════ input string [length of 43 bytes] ══════════════════════
The quick brown fox jumps over the lazy dog

hex CRC-32 checksum = 414FA339                 dec CRC-32 checksum = 1095738169


══════════════════════ input string [length of 46 bytes] ══════════════════════
Generate CRC32 Checksum For Byte Array Example

hex CRC-32 checksum = D1370232                 dec CRC-32 checksum = 3510043186

```



## Ruby

Use 'zlib' from standard library.


```ruby
require 'zlib'
printf "0x%08x\n", Zlib.crc32('The quick brown fox jumps over the lazy dog')
# => 0x414fa339
```


Reimplement CRC-32 in Ruby, with comments to show the polynomials.


```ruby
module CRC
  # Divisor is a polynomial of degree 32 with coefficients modulo 2.
  # We store Divisor in a 33-bit Integer; the polynomial is
  #   Divisor[32] + Divisor[31] * x + ... + Divisor[0] * x**32
  Divisor = [0, 1, 2, 4, 5, 7, 8, 10, 11, 12, 16, 22, 23, 26, 32] \
    .inject(0) {|sum, exponent| sum + (1 << (32 - exponent))}

  # This table gives the crc (without conditioning) of every possible
  # _octet_ from 0 to 255. Each _octet_ is a polynomial of degree 7,
  #   octet[7] + octet[6] * x + ... + octet[0] * x**7
  # Then remainder = Table[octet] is the remainder from
  # _octet_ times x**32 divided by Divisor,
  #   remainder[31] + remainder[30] + ... + remainder[0] * x**31
  Table = Array.new(256) do |octet|
    # Find remainder from polynomial long division.
    #    octet[ 7] * x**32 + ... +   octet[0] * x**39
    #  Divisor[32] * x**0  + ... + Divisor[0] * x**32
    remainder = octet
    (0..7).each do |i|
      # Find next term of quotient. To simplify the code,
      # we assume that Divisor[0] is 1, and we only check
      # remainder[i]. We save remainder, forget quotient.
      if remainder[i].zero?
        # Next term of quotient is 0 * x**(7 - i).
        # No change to remainder.
      else
        # Next term of quotient is 1 * x**(7 - i). Multiply
        # this term by Divisor, then subtract from remainder.
        #  * Multiplication uses left shift :<< to align
        #    the x**(39 - i) terms.
        #  * Subtraction uses bitwise exclusive-or :^.
        remainder ^= (Divisor << i)
      end
    end
    remainder >> 8      # Remove x**32 to x**39 terms.
  end

  module_function

  def crc32(string, crc = 0)
    # Pre-conditioning: Flip all 32 bits. Without this step, a string
    # preprended with extra "\0" would have same crc32 value.
    crc ^= 0xffff_ffff

    # Iterate octets to perform polynomial long division.
    string.each_byte do |octet|

      # Update _crc_ by continuing its polynomial long division.
      # Our current remainder is old _crc_ times x**8, plus
      # new _octet_ times x**32, which is
      #   crc[32] * x**8 + crc[31] * x**9 + ... + crc[8] * x**31 \
      #     + (crc[7] + octet[7]) * x**32 + ... \
      #     + (crc[0] + octet[0]) * x**39
      #
      # Our new _crc_ is the remainder from this polynomial divided by
      # Divisor. We split the terms into part 1 for x**8 to x**31, and
      # part 2 for x**32 to x**39, and divide each part separately.
      # Then remainder 1 is trivial, and remainder 2 is in our Table.

      remainder_1 = crc >> 8
      remainder_2 = Table[(crc & 0xff) ^ octet]

      # Our new _crc_ is sum of both remainders. (This sum never
      # overflows to x**32, so is not too big for Divisor.)
      crc = remainder_1 ^ remainder_2
    end

    # Post-conditioning: Flip all 32 bits. If we later update _crc_,
    # this step cancels the next pre-conditioning.
    crc ^ 0xffff_ffff
  end
end

printf "0x%08x\n", CRC.crc32("The quick brown fox jumps over the lazy dog")
# => 0x414fa339
```



## Rust

This does not perform any caching of the lookup table for simplicity.

```rust

fn crc32_compute_table() -> [u32; 256] {
    let mut crc32_table = [0; 256];

    for n in 0..256 {
        crc32_table[n as usize] = (0..8).fold(n as u32, |acc, _| {
            match acc & 1 {
                1 => 0xedb88320 ^ (acc >> 1),
                _ => acc >> 1,
            }
        });
    }

    crc32_table
}

fn crc32(buf: &str) -> u32 {
    let crc_table = crc32_compute_table();

    !buf.bytes().fold(!0, |acc, octet| {
        (acc >> 8) ^ crc_table[((acc & 0xff) ^ octet as u32) as usize]
    })
}

fn main() {
    println!("{:x}", crc32("The quick brown fox jumps over the lazy dog"));
}

```

{{Output}}

```txt

414fa339

```



## Scala

{{trans|Java}}

```scala
import java.util.zip.CRC32
val crc=new CRC32
crc.update("The quick brown fox jumps over the lazy dog".getBytes)
println(crc.getValue.toHexString)  //> 414fa339
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "crc32.s7i";

const proc: main is func
  begin
    writeln(ord(crc32("The quick brown fox jumps over the lazy dog")) radix 16 lpad0 8);
  end func;
```


{{out}}

```txt

414fa339

```



## Shell


### Bash


```Bash
#!/usr/bin/env bash
declare -i -a CRC32_LOOKUP_TABLE

__generate_crc_lookup_table() {
  local -i -r LSB_CRC32_POLY=0xEDB88320 # The CRC32 polynomal LSB order
  local -i index byte lsb
  for index in {0..255}; do
    ((byte = 255 - index))
    for _ in {0..7}; do # 8-bit lsb shift
      ((lsb = byte & 0x01, byte = ((byte >> 1) & 0x7FFFFFFF) ^ (lsb == 0 ? LSB_CRC32_POLY : 0)))
    done
    ((CRC32_LOOKUP_TABLE[index] = byte))
  done
}
__generate_crc_lookup_table
typeset -r CRC32_LOOKUP_TABLE

crc32_string() {
  [[ ${#} -eq 1 ]] || return
  local -i i byte crc=0xFFFFFFFF index
  for ((i = 0; i < ${#1}; i++)); do
    byte=$(printf '%d' "'${1:i:1}") # Get byte value of character at i
    ((index = (crc ^ byte) & 0xFF, crc = (CRC32_LOOKUP_TABLE[index] ^ (crc >> 8)) & 0xFFFFFFFF))
  done
  echo $((crc ^ 0xFFFFFFFF))
}

printf 'The CRC32 of: %s\nis: 0x%08x\n' "${1}" "$(crc32_string "${1}")"

# crc32_string "The quick brown fox jumps over the lazy dog"
# yields 414fa339

```


### POSIX

The POSIX Shell has no array type and no string indexation.
It costs less to recompute polynomal shift for each character than indexing
with external tools like <code>awk</code> or <code>tr</code>.

```Bash
#!/bin/sh
# POSIX Shell CRC32 of string
# @Name: crc32.sh
# @Version: 1.0.0
# @Author: Léa Gris <lea.gris@noiraude.net>
# @Date: Wed, 27 Mar 2019
# @License: WTFPL http://www.wtfpl.net/

# POSIX Shell has no array or string index
# Implementing a pre-computed CRC32 byte indexed look-up table
# would cost more CPU cycles calling external tools like
# awk or tr to index records from a string.

# Computes the CRC32 of the input data stream
# <&1: The input data stream
# >&1: The CRC32 integer of the input data stream
crc32_stream() {
  crc=0xFFFFFFFF # The Initial CRC32 value
  p=0xedb88320   # The CRC32 polynomal
  r=0            # The polynomal reminder
  c=''           # The current character
  byte=0         # Tge byte value of the current character
  # Iterates each character of the input stream
  while c="$(dd bs=1 count=1 2>/dev/null)" && [ -n "$c" ]; do
    byte=$(printf '%d' "'${c}") # Converts the character into its byte value
    r=$(((crc & 0xff) ^ byte))  # XOR LSB of CRC with current byte
    b=0                         # bit index
    # 8-bit lsb shift with XOR polynomal reminder when odd
    while [ $((b <= 7)) -ne 0 ] && b=$((b + 1)); do
      r=$(((r & 0x1) ? (r >> 1) ^ p : r >> 1))
    done
    crc=$(((crc >> 8) ^ r)) # XOR MSB of CRC with Reminder
  done

  # Output CRC32 integer XOR mask 32 bits
  echo $((crc ^ 0xFFFFFFFF))
}

# Computes the CRC32 of argument string
# 1: The argument string
# >&1: The CRC32 integer of the argument string
crc32_string() {
  [ $# -eq 1 ] || return # argument required
  # Streams with printf to prevent postfixing the string
  # with a newline, since echo -n is not available in POSIX Shell
  printf '%s' "$1" | crc32_stream
}

printf 'The CRC32 of: %s\nis: %08x\n' "$1" "$(crc32_string "$1")"

# crc32_string "The quick brown fox jumps over the lazy dog"
# yields 414fa339

```

{{out}}

```txt
bash ./crc32.sh "The quick brown fox jumps over the lazy dog"
The CRC32 of: The quick brown fox jumps over the lazy dog
is: 0x414fa339
```



## Smalltalk

{{works with|Smalltalk/X}}
the CRC32Stream utility class can do it for me:

```smalltalk
CRC32Stream hashValueOf:'The quick brown fox jumps over the lazy dog'
```

{{out}}
 1095738169 "which is 16r414FA339"


## Swift

Using the zlib crc32 function available to Swift from libz.dylib.

```Swift
import Foundation

let strData = "The quick brown fox jumps over the lazy dog".dataUsingEncoding(NSUTF8StringEncoding,
    allowLossyConversion: false)
let crc = crc32(uLong(0), UnsafePointer<Bytef>(strData!.bytes), uInt(strData!.length))

println(NSString(format:"%2X", crc))
```

{{out}}

```txt
414FA339

```



## Tcl


```tcl
package require Tcl 8.6

set data "The quick brown fox jumps over the lazy dog"
puts [format "%x" [zlib crc32 $data]]
```

{{out}}

```txt
414fa339
```


Alternatively, with older versions of Tcl:
{{tcllib|crc32}}

```tcl
package require crc32
puts [format "%x" [crc::crc32 $data]]
```

With the same input data, it produces identical output.

## VAX Assembly


```VAX Assembly
                           EDB88320  0000     1 poly:   .long   ^xedb88320                      ;crc32
                           00000044  0004     2 table:  .blkl   16
                                     0044     3 
         4C 58 21 0000004C'010E0000' 0044     4 fmt:    .ascid  "!XL"                           ;result format
36 35 34 33 32 31 00000057'010E0000' 004F     5 result: .ascid  "12345678"                      ; and buffer
                              38 37  005D       
                               0000  005F     6 .entry  crc,0
                         A0 AF   7F  0061     7         pushaq  table                           ;fill table
                         99 AF   DF  0064     8         pushal  poly                            ; for
              00000000'GF   02   FB  0067     9         calls   #2, g^lib$crc_table             ;  crc opcode
      2B'  FFFFFFFF 8F   93 AF   0B  006E    10         crc     table, #-1, s^#len, b^msg       ;table,init,len,string
                         98'AF       0077       
                       50   50   D2  0079    11         mcoml   r0, r0                          ;invert result
                                     007C    12         $fao_s	ctrstr = fmt, outbuf = result, p1 = r0 ; format
                         BF AF   7F  008D    13 	pushaq	result				;and show
              00000000'GF   01   FB  0090    14         calls   #1, g^lib$put_output            ;  result 414fa339
                                 04  0097    15         ret
                                     0098    16 
72 62 20 6B 63 69 75 71 20 65 68 54  0098    17 msg:    .ascii  "The quick brown fox jumps over the lazy dog"
70 6D 75 6A 20 78 6F 66 20 6E 77 6F  00A4       
6C 20 65 68 74 20 72 65 76 6F 20 73  00B0       
               67 6F 64 20 79 7A 61  00BC       
                           0000002B  00C3    18 len = .-msg
                                     00C3    19 .end	crc
```



## Visual Basic

{{works with|Visual Basic|5}}
{{works with|Visual Basic|6}}
{{works with|VBA|Access 97}}
{{works with|VBA|6.5}}
{{works with|VBA|7.1}}
{{libheader|Win32}}
Not an ideal task for Visual Basic because the language lacks bit shifting operators (which can of course be emulated, but that's slow). Then again, since the only platform supported by VB is Microsoft Windows (32 Bit Subsystem), we can let the Windows API do the work for us. RtlComputeCrc32() was available since Windows XP and is still present in Windows 10.

```vb
Option Explicit
Declare Function RtlComputeCrc32 Lib "ntdll.dll" _
  (ByVal dwInitial As Long, pData As Any, ByVal iLen As Long) As Long
'--------------------------------------------------------------------
Sub Main()
Dim s As String
Dim b() As Byte
Dim l As Long
  
  s = "The quick brown fox jumps over the lazy dog"
  b() = StrConv(s, vbFromUnicode) 'convert Unicode to ASCII
  l = RtlComputeCrc32(0&, b(0), Len(s))
  Debug.Assert l = &H414FA339

End Sub
```



## Visual Basic .NET

Allows the resumption of calculations, useful for processing a large file with a series of buffer reads.

```vbnet
Public Class Crc32

    ' Table for pre-calculated values.
    Shared table(255) As UInteger

    ' Initialize table
    Shared Sub New()
        For i As UInteger = 0 To table.Length - 1
            Dim te As UInteger = i ' table entry
            For j As Integer = 0 To 7
                If (te And 1) = 1 Then te = (te >> 1) Xor &HEDB88320UI Else te >>= 1
            Next
            table(i) = te
        Next
    End Sub

    ' Return checksum calculation for Byte Array,
    '  optionally resuming (used when breaking a large file into read-buffer-sized blocks).
    ' Call with Init = False to continue calculation.
    Public Shared Function cs(BA As Byte(), Optional Init As Boolean = True) As UInteger
        Static crc As UInteger
        If Init Then crc = UInteger.MaxValue
        For Each b In BA
            crc = (crc >> 8) Xor table((crc And &HFF) Xor b)
        Next
        Return Not crc
    End Function

End Class
```

Test:

```vbnet
    ' Returns a Byte Array from a string of ASCII characters.
    Function Str2BA(Str As String) As Byte()
        Return System.Text.Encoding.ASCII.GetBytes(Str)
    End Function

    ' Returns a Hex string from an UInteger, formatted to a number of digits,
    '  adding leading zeros If necessary.
    Function HexF(Value As UInteger, Digits As Integer) As String
        HexF = Hex(Value)
        If Len(HexF) < Digits Then HexF = StrDup(Digits - Len(HexF), "0") & HexF
    End Function

    ' Tests Crc32 class
    Sub Test()
        Dim Str As String = "The quick brown fox jumps over the lazy dog"
        Debug.Print("Input = """ & Str & """")
        ' Convert string to Byte Array, compute crc32, and display formatted result
        Debug.Print("Crc32 = " & HexF(Crc32.cs(Str2BA(Str)), 8))
        ' This next code demonstrates continuing a crc32 calculation when breaking the input
        ' into pieces, such as processing a large file by a series of buffer reads.
        Crc32.cs(Str2BA(Mid(Str, 1, 20)))
        Debug.Print("Crc32 = " & HexF(Crc32.cs(Str2BA(Mid(Str, 21)), False), 8))
    End Sub
```

Output:
<lang>Input = "The quick brown fox jumps over the lazy dog"
Crc32 = 414FA339
Crc32 = 414FA339
```



## XPL0


```XPL0
code HexOut=27;         \intrinsic routine
string 0;               \use zero-terminated strings

func CRC32(Str, Len);   \Return CRC-32 for given string
char Str;  int Len;     \byte array, number of bytes
int  I, J, R, C;
[R:= -1;                \initialize with all 1's
for J:= 0 to Len-1 do
    [C:= Str(J);
    for I:= 0 to 8-1 do \for each bit in byte...
        [if (R xor C) and 1 then R:= R>>1 xor $EDB88320
        else R:= R>>1;
        C:= C>>1;
        ];
    ];
return not R;
];

HexOut(0, CRC32("The quick brown fox jumps over the lazy dog", 43))
```


{{out}}

```txt

414FA339

```



## zkl

Using zlib:

```zkl
var [const] ZLib=Import("zeelib");
ZLib.calcCRC32(Data(Void,"The quick brown fox jumps over the lazy dog"));
//-->0x414fa339
```

