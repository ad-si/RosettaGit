+++
title = "Host introspection"
description = ""
date = 2019-03-16T20:16:03Z
aliases = []
[extra]
id = 3077
[taxonomies]
categories = ["task", "Programming environment operations"]
tags = []
languages = [
  "ada",
  "algol_68",
  "applesoft_basic",
  "babel",
  "bbc_basic",
  "c",
  "clojure",
  "common_lisp",
  "csharp",
  "d",
  "delphi",
  "erlang",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "frink",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "m2000_interpreter",
  "neko",
  "netrexx",
  "nim",
  "ocaml",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "retro",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "slate",
  "tcl",
  "txr",
  "xpl0",
]
+++

## Task

Print the [[wp:Word_size#Word_size_choice|word size]] and [[wp:Endianness|endianness]] of the host machine.

See also: [[Variable size/Get]]


## Ada


```ada
with Ada.Text_IO;  use Ada.Text_IO;
with System;       use System;

procedure Host_Introspection is
begin
   Put_Line ("Word size" & Integer'Image (Word_Size));
   Put_Line ("Endianness " & Bit_Order'Image (Default_Bit_Order));
end Host_Introspection;
```


```txt

Word size 32
Endianness LOW_ORDER_FIRST

```



## ALGOL 68

```algol68
INT max abs bit = ABS(BIN 1 SHL 1)-1;
INT bits per char = ENTIER (ln(max abs char+1)/ln(max abs bit+1));
INT bits per int = ENTIER (1+ln(max int+1.0)/ln(max abs bit+1));

printf(($"states per bit: "dl$,max abs bit+1));
printf(($"bits per char: "z-dl$,bits per char));
printf(($"bits per int:  "z-dl$,bits per int));
printf(($"chars per int: "z-dl$,bits per int OVER bits per char));

printf(($"bits width: "z-dl$, bits width));

STRING abcds = "ABCD";
FILE abcdf;
INT abcdi;

INT errno := open(abcdf, "abcd.dat",stand back channel);
put(abcdf,abcds); # output alphabetically #
reset(abcdf);
get bin(abcdf,abcdi); # input in word byte order #
STRING int byte order := "";
FOR shift FROM 0 BY bits per char TO bits per int - bits per char DO
  int byte order +:= REPR(abcdi OVER (max abs bit+1) ** shift MOD (max abs char+1))
OD;
printf(($"int byte order: "g,", Hex:",16r8dl$,int byte order, BIN abcdi))
```

{{out}} (Intel i686):

```txt

states per bit:  2
bits per char:   8
bits per int:   32
chars per int:   4
bits width:  32
int byte order: ABCD, Hex:44434241

```

On older CPUs the results would vary:
{|border="1" align="center"
|style="text-align: center;"| ALGOL 68R
|colspan="2" style="text-align: center;"| ALGOL 68RS
|-
|| ~
```txt
bits per char:   6
bits per int:   24
chars per int:   4
```

|| [[wp:ICL 2900|ICL 2900]]
```txt
bits per char:   8
bits per int:   32
chars per int:   4
```

|| [[wp:Multics|Multics]]
```txt
bits per char:   6
bits per int:   36
chars per int:   6
```

|}


## Applesoft BASIC


```ApplesoftBasic
1  DATA248,169,153,24,105,1,48
2  DATA6,24,251,144,2,251,56
3  DATA216,105,0,133,251,96
4  FOR I = 768 TO 787
5  READ B: POKE I,B: NEXT
6  CALL 768:M =  PEEK (251)
7  PRINT " WORD SIZE: ";
8  IF  NOT M THEN  PRINT 8
9 M$ = "HYBRID 8/16"
10  IF M THEN  PRINT M$
11  PRINT "ENDIANNESS: ";
12  PRINT "LITTLE-ENDIAN"
```



## Babel


```babel
main :
    { "Word size: " << msize 3 shl %d << " bits" cr <<
     "Endianness: " << { endian } { "little" } { "big" } ifte cr << }
```



## BBC BASIC


```bbcbasic
      DIM P% 8
      !P% = -1
      I% = 0 : REPEAT I% += 1 : UNTIL P%?I%=0
      PRINT "Word size = " ; I% " bytes"
      !P% = 1
      IF P%?0 = 1 THEN PRINT "Little-endian"
      IF P%?(I%-1) = 1 THEN PRINT "Big-endian"
```

The 'word size' is reported as the number of bytes accessed by the ! indirection operator, which is 4 in all current versions of BBC BASIC.


## C


```c
#include <stdio.h>
#include <stddef.h> /* for size_t */
#include <limits.h> /* for CHAR_BIT */

int main() {
    int one = 1;

    /*
     * Best bet: size_t typically is exactly one word.
     */
    printf("word size = %d bits\n", (int)(CHAR_BIT * sizeof(size_t)));

    /*
     * Check if the least significant bit is located
     * in the lowest-address byte.
     */
    if (*(char *)&one)
        printf("little endian\n");
    else
        printf("big endian\n");
    return 0;
}
```


On POSIX-compatible systems, the following also tests the endianness (this makes use of the fact that network order is big endian):

```c
#include <stdio.h>
#include <arpa/inet.h>

int main()
{
  if (htonl(1) == 1)
    printf("big endian\n");
  else
    printf("little endian\n");
}
```



## C#


```c#
static void Main()
{
  Console.WriteLine("Word size = {0} bytes,",sizeof(int));

  if (BitConverter.IsLittleEndian)
    Console.WriteLine("Little-endian.");
  else
    Console.WriteLine("Big-endian.");
}
```


=={{header|Caché ObjectScript}}==


```txt
USER>Write "Word Size: "_$Case($System.Version.Is64Bits(), 1: 64, : 32)
Word Size: 32

USER>Write "Endianness: "_$Case($System.Version.IsBigEndian(), 1: "Big", : "Little")
Endianness: Little
```



## Clojure


```clojure
(println "word size: " (System/getProperty "sun.arch.data.model"))
(println "endianness: " (System/getProperty "sun.cpu.endian"))
```



## Common Lisp


Common Lisp doesn't provide a native way to reliably determine this (though some unlike other languages, you rarely, if ever, need this information).

The [http://www.lispworks.com/documentation/HyperSpec/Body/c_enviro.htm Environment] has some implementation-specific functions that might provide a good hint, e.g.,

```lisp
(machine-type) ;; => "X86-64" on SBCL here
```


The [http://www.cliki.net/features *features*] list also provides useful information, e.g., some compilers declare :LITTLE-ENDIAN there.

The [http://www.cliki.net/trivial-features cl-trivial-features] library standardizes this, so you will always get either :LITTLE-ENDIAN or :BIG-ENDIAN.  It also adds the CPU (:X86, :X86-64, :PPC, :PPC64, etc.), from which you can probably derive the word size, but it's not (yet) available as a separate flag.


## D


```d
void main() {
  import std.stdio, std.system;

  writeln("Word size = ", size_t.sizeof * 8, " bits.");
  writeln(endian == Endian.littleEndian ? "Little" : "Big", " endian.");
}
```

```txt
Word size = 64 bits.
Little endian.
```



## Delphi


```Delphi
program HostIntrospection ;

{$APPTYPE CONSOLE}

uses SysUtils;

begin
  Writeln('word size: ', SizeOf(Integer));
  Writeln('endianness: little endian'); // Windows is always little endian
end.
```



## Erlang

To find the word size:

```erlang>1
 erlang:system_info(wordsize).
4
```


In the case of endianness, Erlang's bit syntax by default has a 'native' option which lets you use what is supported natively.
As such, there is no function to find endianness.
However, one could write one by using bit syntax, setting endianness and then comparing to the native format:


```erlang>1
 <<1:4/native-unit:8>>.
<<1,0,0,0>>
2> <<1:4/big-unit:8>>
<<0,0,0,1>>
3> <<1:4/little-unit:8>>.
<<1,0,0,0>>
```


And so the following function would output endianness:


```erlang
endianness() when <<1:4/native-unit:8>> =:= <<1:4/big-unit:8>> -> big;
endianness() -> little.
```



## Factor


```factor
USING: alien.c-types alien.data io layouts ;
"Word size: " write cell 8 * .
"Endianness: " write little-endian? "little" "big" ? print
```



## Forth


```forth
: endian
  cr 1 cells . ." address units per cell"
  s" ADDRESS-UNIT-BITS" environment? if cr . ." bits per address unit" then
  cr 1 here ! here c@ if ." little" else ." big" then ."  endian" ;
```

This relies on '''c@''' being a byte fetch (4 chars = 1 cells).  Although it is on most architectures, ANS Forth only guarantees that 1 chars <= 1 cells. Some Forths like OpenFirmware have explicitly sized fetches, like b@.


## Fortran

```fortran
   integer :: i
   character(len=1) :: c(20)
   equivalence (c, i)

   WRITE(*,*) bit_size(1)  ! number of bits in the default integer type
                           ! which may (or may not!) equal the word size
   i = 1

   IF (ichar(c(1)) == 0) THEN
      WRITE(*,*) "Big Endian"
   ELSE
     WRITE(*,*) "Little Endian"
   END IF
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64 (so little endian, 8 byte word size, expected)

' uses intrinsic defines, set by the compiler

#Ifdef __FB_64BIT__
  Print "Host has an 8 byte word size"
#Else
  Print "Host has a 4 byte word size"
#EndIf

#Ifdef __FB_BIGENDIAN__
  Print "Host is big endian"
#Else
  Print "Host is little endian"
#EndIf

Sleep
```


```txt

Host has an 8 byte word size
Host is little endian

```



## Frink


```frink

println["Word size:  " + callJava["java.lang.System", "getProperty", "sun.arch.data.model"]]
println["Endianness: " + callJava["java.lang.System", "getProperty", "sun.cpu.endian"]]

```


=={{header|F_Sharp|F#}}==
A lot of research before I finally came up with an answer to this that isn't dependent on the machine it was compiled on.  Works on Win32 machines only (obviously, due to the interop).  I think that strictly speaking, I should be double checking the OS version before making the call to wow64Process, but I'm not worrying about it.

```fsharp
open System
open System.Runtime.InteropServices
open System.Diagnostics

[<DllImport("kernel32.dll", SetLastError = true, CallingConvention = CallingConvention.Winapi)>]
extern bool IsWow64Process(nativeint hProcess, bool &wow64Process);

let answerHostInfo =
    let Is64Bit() =
        let mutable f64Bit = false;
        IsWow64Process(Process.GetCurrentProcess().Handle, &f64Bit) |> ignore
        f64Bit
    let IsLittleEndian() = BitConverter.IsLittleEndian
    (IsLittleEndian(), Is64Bit())
```


## Go


```go
package main

import (
	"fmt"
	"io/ioutil"
	"runtime"
	"strconv"
	"strings"
	"unsafe"
)

func main() {
	fmt.Println(runtime.Version(), runtime.GOOS, runtime.GOARCH)

	// Inspect a uint32 variable to determine endianness.
	x := uint32(0x01020304)
	switch *(*byte)(unsafe.Pointer(&x)) {
	case 0x01:
		fmt.Println("big endian")
	case 0x04:
		fmt.Println("little endian")
	default:
		fmt.Println("mixed endian?")
	}

	// Usually one cares about the size the executible was compiled for
	// rather than the actual underlying host's size.

	// There are several ways of determining the size of an int/uint.
	fmt.Println("         strconv.IntSize =", strconv.IntSize)
	// That uses the following definition we can also be done by hand
	intSize := 32 << uint(^uint(0)>>63)
	fmt.Println("32 << uint(^uint(0)>>63) =", intSize)

	// With Go 1.0, 64-bit architectures had 32-bit int and 64-bit
	// uintptr. This was changed in Go 1.1. In general it would
	// still be possible that int and uintptr (the type large enough
	// to hold the bit pattern of any pointer) are of different sizes.
	const bitsPerByte = 8
	fmt.Println("  sizeof(int)     in bits:", unsafe.Sizeof(int(0))*bitsPerByte)
	fmt.Println("  sizeof(uintptr) in bits:", unsafe.Sizeof(uintptr(0))*bitsPerByte)
	// If we really want to know the architecture size the executable was
	// compiled for and not the size of int it safest to take the max of those.
	archSize := unsafe.Sizeof(int(0))
	if psize := unsafe.Sizeof(uintptr(0)); psize > archSize {
		archSize = psize
	}
	fmt.Println("  compiled with word size:", archSize*bitsPerByte)

	// There are some *very* unportable ways to attempt to get the actual
	// underlying hosts' word size.
	// Inspect cpuinfo to determine word size (some unix-like OS' only).
	c, err := ioutil.ReadFile("/proc/cpuinfo")
	if err != nil {
		fmt.Println(err)
		return
	}
	ls := strings.Split(string(c), "\n")
	for _, l := range ls {
		if strings.HasPrefix(l, "flags") {
			for _, f := range strings.Fields(l) {
				if f == "lm" { // "long mode"
					fmt.Println("64 bit word size")
					return
				}
			}
			fmt.Println("32 bit word size")
			return
		}
	}
}
```

```txt

go1.3.1 freebsd amd64
little endian
         strconv.IntSize = 64
32 << uint(^uint(0)>>63) = 64
  sizeof(int)     in bits: 64
  sizeof(uintptr) in bits: 64
  compiled with word size: 64
open /proc/cpuinfo: no such file or directory

```


```txt

go1.3.1 freebsd 386
little endian
         strconv.IntSize = 32
32 << uint(^uint(0)>>63) = 32
  sizeof(int)     in bits: 32
  sizeof(uintptr) in bits: 32
  compiled with word size: 32
open /proc/cpuinfo: no such file or directory

```


```txt

go1.3.1 nacl amd64p32
little endian
         strconv.IntSize = 32
32 << uint(^uint(0)>>63) = 32
  sizeof(int)     in bits: 32
  sizeof(uintptr) in bits: 32
  compiled with word size: 32
open /proc/cpuinfo: No such file or directory

```

Alternative technique:

```go
package main

import (
    "debug/elf"
    "fmt"
    "os"
)

func main() {
    f, err := elf.Open(os.Args[0])
    if err != nil {
        fmt.Println("  ", err)
        return
    }
    fmt.Println(f.FileHeader.ByteOrder)
    f.Close()
}
```

```txt

LittleEndian

```



## Groovy

Solution follows [[Java]]:

```groovy
println "word size:  ${System.getProperty('sun.arch.data.model')}"
println "endianness: ${System.getProperty('sun.cpu.endian')}"
```


```txt
word size:  64
endianness: little
```



## Haskell


```haskell
import Data.Bits
import ADNS.Endian -- http://hackage.haskell.org/package/hsdns

main = do
  putStrLn $ "Word size: " ++ bitsize
  putStrLn $ "Endianness: " ++ show endian
      where
        bitsize = show $ bitSize (undefined :: Int)
```


=={{header|Icon}} and {{header|Unicon}}==


```unicon
procedure main()
    write(if 0 = ishift(1,-1) then "little" else "big"," endian")
    if match("flags",line := !open("/proc/cpuinfo")) then    # Unix-like only
        write(if find(" lm ",line) then 64 else 32," bits per word")
    else write("Cannot determine word size.")
end
```


Sample run:


```txt

->hi
little endian
64 bits per word
->

```



## J



```j
   IF64 {32 64
64
```


This returns <code>32</code> in 32 bit J.

Note that this mechanism is testing the interpreter, and not the OS or Hardware.  (Though, of course, you cannot run a 64 bit interpreter on a machine that does not support it.)

That said, this does not deal with endianness.  For the most part, J programs do not need to know their own endianness.  When converting to and from binary format you can specify "native", "little endian" and "big endian", and it's rare that you have an interface which would need anything else.  That said, you can inspect the binary representation of a simple constant:


```j
   ":&> (|: 32 64  ;"0 big`little) {"_1~ 2 2 #: 16b_e0 + a. i. 0 { 3!:1  ''
64
little
```



## Java

Java conceals the byte order of its integers, but reports the native byte order through java.nio.ByteOrder.nativeOrder().

```java
import java.nio.ByteOrder;

public class ShowByteOrder {
    public static void main(String[] args) {
        // Print "BIG_ENDIAN" or "LITTLE_ENDIAN".
        System.out.println(ByteOrder.nativeOrder());
    }
}
```


Some JVMs also have system properties for the word size and byte order.


```java
System.out.println("word size: "+System.getProperty("sun.arch.data.model"));
System.out.println("endianness: "+System.getProperty("sun.cpu.endian"));
```



## Julia

<code>Julia</code> creates <code>ENDIAN_BOM</code> a 32 bit unsigned integer out of an array of 4 8 bit unsigned integers to serve as an endianness marker.

```Julia

print("This host's word size is ", WORD_SIZE, ".")
if ENDIAN_BOM == 0x04030201
    println("And it is a little-endian machine.")
elseif ENDIAN_BOM == 0x01020304
    println("And it is a big-endian machine.")
else
    println("ENDIAN_BOM = ", ENDIAN_BOM, ", which is confusing")
end

```


```txt

This host's word size is 64.And it is a little-endian machine.

```



## Kotlin

The following is not guaranteed to work on all JVMs but is working fine on my x64 Windows 10 machine:

```scala
// version 1.0.6

fun main(args: Array<String>) {
    println("Word size : ${System.getProperty("sun.arch.data.model")} bits")
    println("Endianness: ${System.getProperty("sun.cpu.endian")}-endian")
}
```


```txt

Word size : 64 bits
Endianness: little-endian

```


## M2000 Interpreter


```M2000 Interpreter

Module CheckIt {
      \\ Always run in Little-endian, 32 bits (in Wow64 in 64 bit os)
      Module EndiannessAndSize {
            Buffer Check as Long
            Return Check, 0:=1
            if eval(Check, 0 as byte)=1 then {
                  Print "Little-endian"
            }
            \\ 4 bytes
            Print "Word size:"; Len(Check)*8;" bits"
      }
      EndiannessAndSize
      \\ Access to internal com object clsOsInfo
      Declare OsInfo Information
      Print Type$(OsInfo) ="clsOSInfo"
      \\ Build is a read only property
      With OsInfo, "Build" as Build, "OSName" as OSName$, "IsElevated" as IsElevated
      Print OsName$
      Print "Build=";Build
      \\ IsWow64 is a function
      Method OsInfo, "IsWow64" as IsWow64
      If  IsWow64 Then {
            Print "64 bit Os"
      } Else {
            Print  "32 bit OS"
      }
      Print "IsElevated:";IsElevated
}
Checkit

```

=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
If[$ByteOrdering > 0, Print["Big endian"], Print["Little endian" ]]
$SystemWordLength "bits"
```


{{out}} x86

```txt

Little endian
32 bits

```


=={{header|MATLAB}} / {{header|Octave}}==

The concept of "word size" is not meaningful in Matlab and Octave, uint64 is also available on 32bit-platforms, and there are no pointers. Endianity can be tested with the function below:


```MATLAB
  function [endian]=endian()
    fid=tmpfile();
    fwrite(fid,1:8,'uint8');

    fseek(fid,0,'bof');
    t=fread(fid,8,'int8');
    i8=sprintf('%02X',t);

    fseek(fid,0,'bof');
    t=fread(fid,4,'int16');
    i16=sprintf('%04X',t);

    fclose(fid);

    if strcmp(i8,i16) endian='big';
    else endian='little';
    end;

```


```txt
  octave:128> computer
  x86_64-unknown-linux-gnu
  octave:129> endian
  endian = little
```



=={{header|Modula-3}}==

```modula3
MODULE Host EXPORTS Main;

IMPORT IO, Fmt, Word, Swap;

BEGIN
  IO.Put("Word Size: " & Fmt.Int(Word.Size) & "\n");
  IF Swap.endian = Swap.Endian.Big THEN
    IO.Put("Endianness: Big\n");
  ELSE
    IO.Put("Endianness: Little\n");
  END;
END Host.
```


{{out}} (on an x86):

```txt

Word Size: 32
Endianness: Little

```



## Neko

NekoVM can include shared library functions that adhere to an API of passing Neko values and library file naming. A small C helper is included here to get at the Host wordsize. NekoVM link library search path (.ndll files), includes looking in current directory. The endianess test is a BUILTIN (accessible with leading $ identifier).

C support file, host-introspection.c


```C
/* Return wordsize to Neko */
/* From Rosetta Code, C entry, with Neko marshalling */

#include <stdio.h>
#include <stddef.h> /* for size_t */
#include <limits.h> /* for CHAR_BIT */
#include <neko.h>

value wordsize(void) {
    /*
     * Best bet: size_t typically is exactly one word.
     */
    return alloc_int((int)(CHAR_BIT * sizeof(size_t)));
}
/* Expose symbol to Neko loader */
DEFINE_PRIM(wordsize, 0);
```


Neko caller, host-introspection.neko


```ActionScript
/**
 Host introspection, in Neko
*/

/* higher order byte first?  Intel being little ended. */
$print("isbigendian: ", $isbigendian(), "\n")

/*
  Getting at word size is a little more difficult in Neko source.
  Neko is a fixed bit-width VM, Int is 31 bits, 30 signed, etc.
  There is no builtin native sizeof, but a few lines of
  C data marshalling wrapper, a small change to tectonics, and...
*/

var wordsize = $loader.loadprim("native@wordsize", 0)
$print("wordsize: ", wordsize(), " bits\n")
```


```txt

prompt$ gcc -shared -fPIC host-introspection.c -o native.ndll
prompt$ nekoc host-introspection.neko
prompt$ neko host-introspection.n
isbigendian: false
wordsize: 64 bits
```



## NetRexx

NetRexx can access this information from the [[Java]] virtual machine in the same way as the [[#Java|Java]] sample above.

```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

wordSize = System.getProperty('sun.arch.data.model')
endian   = System.getProperty('sun.cpu.endian')

say ' word size:' wordSize
say 'endianness:' endian

```



## Nim


```nim
import math
echo cpuEndian
echo round(log2(float(int.high))) + 1
```


=={{header|Objective-C}}==
Endianness:

```objc
switch (NSHostByteOrder()) {
  case NS_BigEndian:
    NSLog(@"%@", @"Big Endian");
    break;
  case NS_LittleEndian:
    NSLog(@"%@", @"Little Endian");
    break;
  case NS_UnknownByteOrder:
    NSLog(@"%@", @"endianness unknown");
    break;
}
```


Architecture:
(works on Mac OS X 10.6+)

```objc
switch ([NSRunningApplication currentApplication].executableArchitecture) {
  case NSBundleExecutableArchitectureI386:
    NSLog(@"%@", @"i386 32-bit");
    break;

  case NSBundleExecutableArchitectureX86_64:
    NSLog(@"%@", @"x86_64 64-bit");
    break;

  case NSBundleExecutableArchitecturePPC:
    NSLog(@"%@", @"PPC 32-bit");
    break;

  case NSBundleExecutableArchitecturePPC64:
    NSLog(@"%@", @"PPC64 64-bit");
    break;

  default:
    NSLog(@"%@", @"Unknown");
    break;
}
```



## OCaml



```ocaml
Printf.printf "%d\n" Sys.word_size; (* Print word size *)
Printf.printf "%s\n" Sys.os_type;   (* Print operating system *)
```


```ocaml
(* Print endianness *)
Printf.printf "%s\n" (if Sys.big_endian then "big endian" else "little endian");
```


On OCaml 3 and below, there are tricks to get endianness. For example in Linux or Unix variants,
one may use the [http://unixhelp.ed.ac.uk/CGI/man-cgi?uname uname] shell command :


```ocaml
let uname arg =
  let arg = if arg = "" then "-" else arg in
  let ic = Unix.open_process_in ("uname -" ^ arg) in
  (input_line ic)
;;

# uname "sm";;
- : string = "Linux i686"
```


In most cases, endianness can be infered from informations given by uname.

One may also read files in the /proc directory in order to get informations about the host, only under linux :


```ocaml
(* Reading all the lines from a file.
If the loop is implemented by a recursive auxiliary function, the try...with breaks
tail recursion if not written carefully *)
let lines name =
  let f = open_in name
  and r = ref []
  in
  (try
     while true do
       r := (input_line f)::!r
     done
   with End_of_file -> close_in f);
  (List.rev !r)
;;

# lines "/proc/meminfo";;
- : string list =
["MemTotal:      2075240 kB"; "MemFree:        469964 kB";
 "Buffers:         34512 kB"; "Cached:        1296380 kB";
 "SwapCached:         96 kB"; "Active:         317484 kB";
 "Inactive:      1233500 kB"; "HighTotal:     1178432 kB";
 "HighFree:        45508 kB"; "LowTotal:       896808 kB";
 "LowFree:        424456 kB"; "SwapTotal:     2650684 kB";
 "SwapFree:      2650588 kB"; "Dirty:             228 kB";
 "Writeback:           0 kB"; "AnonPages:      220036 kB";
 "Mapped:          67160 kB"; "Slab:            41540 kB";
 "SReclaimable:    34872 kB"; "SUnreclaim:       6668 kB";
 "PageTables:       1880 kB"; "NFS_Unstable:        0 kB";
 "Bounce:              0 kB"; "WritebackTmp:        0 kB";
 "CommitLimit:   3688304 kB"; "Committed_AS:   549912 kB";
 "VmallocTotal:   114680 kB"; "VmallocUsed:      5172 kB";
 "VmallocChunk:   109320 kB"; "HugePages_Total:     0";
 "HugePages_Free:      0"; "HugePages_Rsvd:      0";
 "HugePages_Surp:      0"; "Hugepagesize:     4096 kB"]
```


Same methods can be used to get the results of commands lshw, dmidecode...


## Pascal


```pascal
program HostIntrospection(output);
begin
  writeln('Pointer size: ', SizeOf(Pointer), ' byte, i.e. ', SizeOf(Pointer)*8, ' bit.');
{ NtoBE converts from native endianess to big endianess }
  if 23453 = NtoBE(23453) then
    writeln('This host is big endian.')
  else
    writeln('This host is little endian.');
end.
```

```txt

>: ./HostIntrospection
Pointer size: 4 byte, i.e. 32 bit.
This host is little endian.

```



## Perl

Most basic example:

```perl
use Config;
print "UV size: $Config{uvsize}, byte order: $Config{byteorder}\n";
```

```txt

UV size: 4, byte order: 1234

```


More verbose example:

```perl
use 5.010;
use Config;
my ($size, $order, $end) = @Config{qw(uvsize byteorder)};
given ($order) {
    when (join '', sort split '') { $end = 'little' }
    when (join '', reverse sort split '') { $end = 'big' }
    default { $end = 'mixed' }
}
say "UV size: $size, byte order: $order ($end-endian)";
```

```txt

UV size: 4, byte order: 1234 (little-endian)

```


```txt

UV size: 4, byte order: 3412 (mixed-endian)

```


```txt

UV size: 8, byte order: 87654321 (big-endian)

```


## Perl 6

Endian detection translated from C. {{works with|Rakudo|2018.03}}

```perl6
use NativeCall;
say $*VM.config<ptr_size>;
my $bytes = nativecast(CArray[uint8], CArray[uint16].new(1));
say $bytes[0] ?? "little-endian" !! "big-endian";
```

```txt
8
little-endian
```

Note: Rakudo 2018.12 is introducing the endian-sensitive<code>read-int16</code> method,
which makes endian detection a little easier:

```perl6
say blob8.new(1,0).read-int16(0) == 1 ?? "little-endian" !! "big-endian"
```



## Phix

Note that machine_word() and machine_bits() test the interpreter or compiled executable, rather than the OS or hardware.

Also, all known implementations of Phix are currently little-endian. See also platform(), which yields WINDOWS or LINUX.

```Phix
function endianness()
atom m4 = allocate(4)
    poke4(m4,#01020304)
    integer b1 = peek1s(m4)
    free(m4)
    if b1=#01 then
        return "big-endian"
    elsif b1=#04 then
        return "little-endian"
    else
        return "???"
    end if
end function

printf(1,"Endianness: %s\n",{endianness()})
printf(1,"Word size: %d bytes/%d bits\n",{machine_word(),machine_bits()})
```

```txt

Endianness: little-endian
Word size: 4 bytes/32 bits

```

or

```txt

Endianness: little-endian
Word size: 8 bytes/64 bits

```



## PicoLisp

We inspect the ELF header of the executable file (the 'cmd' function returns the
path to the command that invoked the interpreter). Note that this (like most
other contributions to this task) only tells how the binary was
compiled/assembled/linked, not necessarily the nature of the underlying system.

```PicoLisp
(in (cmd)                              # Inspect ELF header
   (rd 4)                              # Skip "7F" and 'E', 'L' and 'F'
   (prinl
      (case (rd 1)                     # Get EI_CLASS byte
         (1 "32 bits")
         (2 "64 bits")
         (T "Bad EI_CLASS") ) )
   (prinl
      (case (rd 1)                     # Get EI_DATA byte
         (1 "Little endian")
         (2 "Big endian")
         (T "Bad EI_DATA") ) ) )
```

```txt
64 bits
Little endian
```



## PL/I


```PL/I

details: procedure options (main); /* 6 July 2012 */
	declare x float, i fixed binary initial (1);

	put skip list ('word size=', length(unspec(x)));

	if unspec(i) = '0000000000000001'b then
		put skip list ('Big endian');
	else
		put skip list ('Little endian');

end details;

```

```txt

word size=                          32
Little endian

```



## PowerShell


```powershell
Write-Host Word Size: ((Get-WMIObject Win32_Processor).DataWidth)
Write-Host -NoNewLine "Endianness: "
if ([BitConverter]::IsLittleEndian) {
    Write-Host Little-Endian
} else {
    Write-Host Big-Endian
}
```

Note that endianness is essentially a moot point with PowerShell,
as there is only a Windows implementation currently
and current Windows versions don't run on big-endian systems.
But in theory this check should work.


## PureBasic


```PureBasic
Enumeration
  #LittleEndian
  #BigEndian
EndEnumeration

ProcedureDLL EndianTest()
  Protected Endian = #LittleEndian
  Protected dummy.l= 'ABCD'
  If "A"=Chr(PeekA(@dummy))
    Endian=#BigEndian
  EndIf
  ProcedureReturn Endian
EndProcedure

;- *** Start of test code
If OpenConsole()
  PrintN("Your word size is "+Str(SizeOf(Integer)) +" bytes,")
  Select EndianTest()
    Case #LittleEndian
      PrintN("and you use Little Endian.")
    Default
      PrintN("and you use Big Endian.")
  EndSelect
EndIf
```



## Python


```python>>>
 import platform, sys, socket
>>> platform.architecture()
('64bit', 'ELF')
>>> platform.machine()
'x86_64'
>>> platform.node()
'yourhostname'
>>> platform.system()
'Linux'
>>> sys.byteorder
little
>>> socket.gethostname()
'yourhostname'
>>>
```



## R

Word size

```R
8 * .Machine$sizeof.long # e.g. 32
```

Endianness

```R
.Platform$endian         # e.g. "little"
```



## Racket


```Racket

#lang racket/base

(printf "Word size: ~a\n" (system-type 'word))
(printf "Endianness: ~a\n" (if (system-big-endian?) 'big 'little))

```



## Retro

These introspections are possible through the standard '''variations''' library.

Word Size


```Retro
needs variations'
^variations'size
```


Returns the number of bits per cell. This is normally 32, though may be smaller or larger on embedded systems and under special cases.

Endianness


```Retro
needs variations'
^variations'endian
```


Returns 0 for little endian, and 1 for big endian.


## REXX

Since all variables in the REXX language are stored as characters, the wordsize is immaterial (REXX supports variable precision for numbers).

This also applies to the "endianness" of words or how they are stored.

The REXX language was designed for scripting and interfacing with the operating system.

However, there is a STORAGE built-in function that allows a program to look at (local) storage, and if there is an

indicator stored anywhere in the virtual address space, it can be examined.

```rexx
/*REXX program to examine which operating system that REXX is running under. */

parse source opSys howInvoked pathName

/*where  opSys  will indicate which operating system REXX is running under, and */
/*from that, one could make assumptions what the wordsize is, etc.              */
```



## Ruby


```ruby
# We assume that a Fixnum occupies one machine word.
# Fixnum#size returns bytes (1 byte = 8 bits).
word_size = 42.size * 8
puts "Word size: #{word_size} bits"

# Array#pack knows the native byte order. We pack 1 as a 16-bit integer,
# then unpack bytes: [0, 1] is big endian, [1, 0] is little endian.
bytes = [1].pack('S').unpack('C*')
byte_order = (bytes[0] == 0 ? 'big' : 'little') + ' endian'
puts "Byte order: #{byte_order}"
```


With [[MRI]], <code>ri Fixnum</code> states, "A Fixnum holds Integer values that can be represented in a native machine word (minus 1 bit)." This bases our claim that a Fixnum occupies one machine word.

Some other implementations of Ruby are different. With [[JRuby]], a Fixnum is always 64 bits, because it is a Java <code>long</code> [http://www.jruby.org/git?p=jruby.git;a=blob;f=src/org/jruby/RubyFixnum.java;h=ba8d076d58d28c30ecd8e378e6e2482486dba22d;hb=HEAD#l91 (1)]. JRuby uses the correct native byte order by calling java.nio.ByteOrder.nativeOrder() [http://www.jruby.org/git?p=jruby.git;a=blob;f=src/org/jruby/platform/Platform.java;h=d84b0b55b1aca381b2101297185d3b7f872c8cfd;hb=HEAD#l110 (2)].


## Rust


```Rust
#[derive(Copy, Clone, Debug)]
enum Endianness {
    Big, Little,
}

impl Endianness {
    fn target() -> Self {
        #[cfg(target_endian = "big")]
        {
            Endianness::Big
        }
        #[cfg(not(target_endian = "big"))]
        {
            Endianness::Little
        }
    }
}

fn main() {
    println!("Word size: {} bytes", std::mem::size_of::<usize>());
    println!("Endianness: {:?}", Endianness::target());
}
```


```txt
Word size: 8 bytes
Endianness: Little
```



## Scala

```Scala
import java.nio.ByteOrder

object ShowByteOrder extends App {
  println(ByteOrder.nativeOrder())
  println(s"Word size: ${System.getProperty("sun.arch.data.model")}")
  println(s"Endianness: ${System.getProperty("sun.cpu.endian")}")
}
```



## Scheme

```scheme
(define host-info
  (begin
    (display "Endianness: ")
    (display (machine-byte-order))
    (newline)
    (display "Word Size: ")
    (display (if (fixnum? (expt 2 33)) 64 32))
    (newline)))
```

 Endianness: little-endian
 Word Size: 32


## Seed7

The library [http://seed7.sourceforge.net/libraries/cc_conf.htm cc_conf.s7i] provides values that describe C compiler and runtime library.
The example below assumes that the word size is the size of a pointer.


```seed7
$ include "seed7_05.s7i";
  include "cc_conf.s7i";

const proc: main is func
  begin
    writeln("Word size: " <& ccConf.POINTER_SIZE);
    write("Endianness: ");
    if ccConf.LITTLE_ENDIAN_INTTYPE then
      writeln("Little endian");
    else
      writeln("Big endian");
    end if;
  end func;
```


```txt

Word size: 64
Endianness: Little endian

```



## Slate


```slate
inform: 'Endianness: ' ; Platform current endianness.
inform: 'Word Size: ' ; (Platform current bytesPerWord * 8) printString.
```

```txt

Endianness: LittleEndian
Word Size: 32

```



## Tcl

This is very straightforward in Tcl.  The global array <code>tcl_platform</code> contains these values.  In an interactive <code>tclsh</code>:

```tcl
% parray tcl_platform
tcl_platform(byteOrder)   = littleEndian
tcl_platform(machine)     = intel
tcl_platform(os)          = Windows NT
tcl_platform(osVersion)   = 5.1
tcl_platform(platform)    = windows
tcl_platform(pointerSize) = 4
tcl_platform(threaded)    = 1
tcl_platform(user)        = glennj
tcl_platform(wordSize)    = 4
```


=={{header|TI-89 BASIC}}==


```ti89b
Disp "32-bit big-endian"
```



## TXR


Interactive session:

Which word? Pointer size or size of <code>int</code>? Let's get both:


```txt
This is the TXR Lisp interactive listener of TXR 177.
Use the :quit command or type Ctrl-D on empty line to exit.
1> (sizeof (ptr char))
8
2> (sizeof int)
4
```


Endianness: what we can do is put the integer 1 into a buffer as a <code>uint32</code>, the 32 bit unsigned integer type in the local representation. We then retrieve it as a <code>le-uint32</code>: little-endian <code>uint32</code>:


```txt
3> (ffi-put 1 (ffi uint32))
#b'01000000'
4> (ffi-get *3 (ffi le-uint32))
1
```


The extracted value 1 matches, so the machine must be little endian. Here is a transcript from a big-endian PPC64 machine:


```txt
1> (ffi-put 1 (ffi uint32))
#b'00000001'
2> (ffi-get *1 (ffi le-uint32))
16777216
```


No match, so big endian.


## XPL0

This is the result when running the 32-bit version of the language on
Intel 386 (and later) processors. Other versions give 2 bytes per word,
and the Motorola 68000 version would give 4 bytes per word and Big
endian.


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
int  A, B;
char C;
[IntOut(0, @B-@A);  CrLf(0);    \word size = integer size
A:= $1234;
C:= @A;
Text(0, if C(0)=$34 then "Little" else "Big");
Text(0, " endian
");
]
```


```txt

4
Little endian

```

