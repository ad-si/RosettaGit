+++
title = "Random number generator (device)"
description = ""
date = 2019-10-04T05:12:34Z
aliases = []
[extra]
id = 9143
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "aarch64_assembly",
  "ada",
  "arm_assembly",
  "batch_file",
  "bbc_basic",
  "c",
  "c_sharp",
  "common_lisp",
  "d",
  "echo_lisp",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "groovy",
  "j",
  "java",
  "jq",
  "julia",
  "haskell",
  "kotlin",
  "lasso",
  "mathematica",
  "nim",
  "ocaml",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "pico_lisp",
  "power_shell",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "tcl",
  "unix_shell",
  "wee_basic",
  "x86_assembly",
  "xpl0",
  "zkl"
]
+++

## Task

If your system has a means to generate random numbers involving not only a software algorithm   (like the [[wp:/dev/random|/dev/urandom]] devices in Unix),   then:

show how to obtain a random 32-bit number from that mechanism.





## AArch64 Assembly


Linux provides <code>getrandom</code> syscall for most architectures, which draws random bytes from <code>/dev/urandom</code> by default.

The syscall number on AArch64 is 278.

<lang ARM_Assembly>.equ STDOUT, 1
.equ SVC_WRITE, 64
.equ SVC_GETRANDOM, 278
.equ SVC_EXIT, 93

.text
.global _start

_start:
	stp x29, x30, [sp, -32]! // allocate buffer space at [sp]
	mov x29, sp
	mov x0, sp
	mov x1, #4
	bl _getrandom // getrandom(&tmp, 4);
	ldr w0, [sp]
	bl print_uint64 // print_uint64(tmp);
	ldp x29, x30, [sp], 32
	mov x0, #0
	b _exit // exit(0);

// void print_uint64(uint64_t x) - print an unsigned integer in base 10.
print_uint64:
	// x0 = remaining number to convert
	// x1 = pointer to most significant digit
	// x2 = 10
	// x3 = x0 / 10
	// x4 = x0 % 10
	// compute x0 divmod 10, store a digit, repeat if x0 > 0
	ldr x1, =strbuf_end
	mov x2, #10
1:	udiv x3, x0, x2
	msub x4, x3, x2, x0
	add x4, x4, #48
	mov x0, x3
	strb w4, [x1, #-1]!
	cbnz x0, 1b
	// compute the number of digits to print, then call write()
	ldr x3, =strbuf_end_newline
	sub x2, x3, x1
	mov x0, #STDOUT
	b _write

.data
strbuf:
	.space 31
strbuf_end:
	.ascii "\n"
strbuf_end_newline:
.align 4

.text
//////////////// system call wrappers
// ssize_t _write(int fd, void *buf, size_t count)
_write:
	mov x8, #SVC_WRITE
	svc #0
	ret

// ssize_t getrandom(void *buf, size_t buflen, unsigned int flags=0)
_getrandom:
	mov x2, #0
	mov x8, #SVC_GETRANDOM
	svc #0
	ret

// void _exit(int retval)
_exit:
	mov x8, #SVC_EXIT
	svc #0
```



## Ada

random.adb:

```Ada
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
procedure Random is
   Number : Integer;
   Random_File : Ada.Streams.Stream_IO.File_Type;
begin
   Ada.Streams.Stream_IO.Open (File => Random_File,
                               Mode => Ada.Streams.Stream_IO.In_File,
                               Name => "/dev/random");
   Integer'Read (Ada.Streams.Stream_IO.Stream (Random_File), Number);
   Ada.Streams.Stream_IO.Close (Random_File);
   Ada.Text_IO.Put_Line ("Number:" & Integer'Image (Number));
end Random;
```


## ARM Assembly

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program urandom.s   */

/* Constantes    */
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ READ,   3
.equ WRITE,  4
.equ OPEN,   5
.equ CLOSE,  6

.equ O_RDONLY, 0                         @ open for reading only

.equ BUFFERSIZE,          4              @ random number 32 bits

/* Initialized data */
.data
szFileName:              .asciz "/dev/urandom"      @ see linux doc
szCarriageReturn:        .asciz "\n"
/* datas error display */
szMessErreur:        .asciz "Error detected.\n"
szMessErr:           .ascii "Error code hexa : "
sHexa:               .space 9,' '
                     .ascii "  decimal :  "
sDeci:               .space 15,' '
                     .asciz "\n"
/* datas message display */
szMessResult:        .ascii "Random number :"
sValue:              .space 12,' '
                     .asciz "\n"
/* UnInitialized data */
.bss
sBuffer:             .skip BUFFERSIZE             @ buffer result

/*  code section */
.text
.global main
main:
    ldr r0,iAdrszFileName               @ File name
    mov r1,#O_RDONLY                    @  flags
    mov r2,#0                           @ mode
    mov r7,#OPEN                        @ open file
    svc #0
    cmp r0,#0                           @ error ?
    ble error
    mov r8,r0                           @ save FD
    mov r4,#0                           @ loop counter
1:
    mov r0,r8                           @ File Descriptor
    ldr r1,iAdrsBuffer                  @ buffer address
    mov r2,#BUFFERSIZE                  @ buffer size
    mov r7,#READ                        @ call system read file
    svc 0
    cmp r0,#0                           @ read error ?
    ble error
    ldr r1,iAdrsBuffer                  @ buffer address
    ldr r0,[r1]                         @ display buffer value
    ldr r1,iAdrsValue
    bl conversion10
    ldr r0,iAdrszMessResult
    bl affichageMess
    add r4,#1                           @ increment counter
    cmp r4,#10                          @ maxi ?
    blt 1b                              @ no -> loop


end:
    mov r0,r8
    mov r7, #CLOSE                      @ call system close file
    svc #0
    cmp r0,#0
    blt error
    mov r0,#0                           @ return code
    b 100f
error:
    ldr r1,iAdrszMessErreur             @ error message
    bl   displayError
    mov r0,#1                           @ return error code
100:                                    @ standard end of the program
    mov r7, #EXIT                       @ request to exit program
    svc 0                               @ perform system call
iAdrsBuffer:               .int sBuffer
iAdrsValue:                .int sValue
iAdrszMessResult:          .int szMessResult
iAdrszFileName:            .int szFileName
iAdrszMessErreur:          .int szMessErreur
iAdrszCarriageReturn:      .int szCarriageReturn

/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                       @ save  registers
    mov r2,#0                                   @ counter length */
1:                                              @ loop length calculation
    ldrb r1,[r0,r2]                             @ read octet start position + index
    cmp r1,#0                                   @ if 0 its over
    addne r2,r2,#1                              @ else add 1 in the length
    bne 1b                                      @ and loop
                                                @ so here r2 contains the length of the message
    mov r1,r0                                   @ address message in r1
    mov r0,#STDOUT                              @ code to write to the standard output Linux
    mov r7, #WRITE                              @ code call system "write"
    svc #0                                      @ call system
    pop {r0,r1,r2,r7,lr}                        @ restaur registers
    bx lr                                       @ return
/***************************************************/
/*   display error message                         */
/***************************************************/
/* r0 contains error code  r1 : message address */
displayError:
    push {r0-r2,lr}                         @ save registers
    mov r2,r0                               @ save error code
    mov r0,r1
    bl affichageMess
    mov r0,r2                               @ error code
    ldr r1,iAdrsHexa
    bl conversion16                         @ conversion hexa
    mov r0,r2                               @ error code
    ldr r1,iAdrsDeci                        @ result address
    bl conversion10S                        @ conversion decimale
    ldr r0,iAdrszMessErr                    @ display error message
    bl affichageMess
100:
    pop {r0-r2,lr}                          @ restaur registers
    bx lr                                   @ return
iAdrszMessErr:                 .int szMessErr
iAdrsHexa:                     .int sHexa
iAdrsDeci:                     .int sDeci
/******************************************************************/
/*     Converting a register to hexadecimal                      */
/******************************************************************/
/* r0 contains value and r1 address area   */
conversion16:
    push {r1-r4,lr}                          @ save registers
    mov r2,#28                               @ start bit position
    mov r4,#0xF0000000                       @ mask
    mov r3,r0                                @ save entry value
1:                                           @ start loop
    and r0,r3,r4                             @ value register and mask
    lsr r0,r2                                @ move right
    cmp r0,#10                               @ compare value
    addlt r0,#48                             @ <10  ->digit
    addge r0,#55                             @ >10  ->letter A-F
    strb r0,[r1],#1                          @ store digit on area and + 1 in area address
    lsr r4,#4                                @ shift mask 4 positions
    subs r2,#4                               @ counter bits - 4 <= zero  ?
    bge 1b                                   @ no -> loop

100:
    pop {r1-r4,lr}                                     @ restaur registers
    bx lr
/***************************************************/
/*  Converting a register to a signed decimal      */
/***************************************************/
/* r0 contains value and r1 area address    */
conversion10S:
    push {r0-r4,lr}       @ save registers
    mov r2,r1             @ debut zone stockage
    mov r3,#'+'           @ par defaut le signe est +
    cmp r0,#0             @ negative number ?
    movlt r3,#'-'         @ yes
    mvnlt r0,r0           @ number inversion
    addlt r0,#1
    mov r4,#10            @ length area
1:                        @ start loop
    bl divisionpar10U
    add r1,#48            @ digit
    strb r1,[r2,r4]       @ store digit on area
    sub r4,r4,#1          @ previous position
    cmp r0,#0             @ stop if quotient = 0
    bne 1b

    strb r3,[r2,r4]       @ store signe
    subs r4,r4,#1         @ previous position
    blt  100f             @ if r4 < 0 -> end

    mov r1,#' '           @ space
2:
    strb r1,[r2,r4]       @store byte space
    subs r4,r4,#1         @ previous position
    bge 2b                @ loop if r4 > 0
100:
    pop {r0-r4,lr}        @ restaur registers
    bx lr
/***************************************************/
/*   division par 10   unsigned                    */
/***************************************************/
/* r0 dividende   */
/* r0 quotient    */
/* r1 remainder   */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0                                          @ save value
    //mov r3,#0xCCCD                                   @ r3 <- magic_number lower  raspberry 3
    //movt r3,#0xCCCC                                  @ r3 <- magic_number higter raspberry 3
    ldr r3,iMagicNumber                                @ r3 <- magic_number    raspberry 1 2
    umull r1, r2, r3, r0                               @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0)
    mov r0, r2, LSR #3                                 @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2                               @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1                               @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                                              @ leave function
iMagicNumber:  	.int 0xCCCCCCCD


```


## Batch File

The dynamic environmental variable <code>%random%</code> contains a number between 0 and 32767.

```dos

@echo %random%

```



## BBC BASIC

Requires Windows XP or later.

```bbcbasic
      SYS "SystemFunction036", ^random%, 4
      PRINT ~random%
```



## C

It works on systems having /dev/urandom, like [[GNU]]/[[Linux]].


```c
#include <stdio.h>
#include <stdlib.h>

#define RANDOM_PATH "/dev/urandom"

int main(void)
{
        unsigned char buf[4];
        unsigned long v;
        FILE *fin;

        if ((fin = fopen(RANDOM_PATH, "r")) == NULL) {
                fprintf(stderr, "%s: unable to open file\n", RANDOM_PATH);
                return EXIT_FAILURE;
        }
        if (fread(buf, 1, sizeof buf, fin) != sizeof buf) {
                fprintf(stderr, "%s: not enough bytes (expected %u)\n",
                        RANDOM_PATH, (unsigned) sizeof buf);
                return EXIT_FAILURE;
        }
        fclose(fin);
        v = buf[0] | buf[1] << 8UL | buf[2] << 16UL | buf[3] << 24UL;
        printf("%lu\n", v);
        return 0;
}
```


=== {{libheader|BSD libc}} ===
[http://www.openbsd.org/cgi-bin/man.cgi?query=arc4random&apropos=0&sektion=3&manpath=OpenBSD+Current&arch=i386&format=html arc4random()] appeared in [[OpenBSD]] 2.1 and has spread to many [[BSD]] systems. This function runs an ARC4 random number generator that takes entropy from a kernel device. (This kernel device is sysctl kern.arandom in OpenBSD, or /dev/urandom in some other systems.)


```c
#include <inttypes.h> /* PRIu32 */
#include <stdlib.h> /* arc4random */
#include <stdio.h>  /* printf */

int
main()
{
  printf("%" PRIu32 "\n", arc4random());
  return 0;
}
```


=== {{libheader|OpenSSL}} ===
OpenSSL can generate random numbers. The default generator uses SHA1. For [[Unix]] systems, OpenSSL will gather entropy by reading a kernel device like /dev/urandom, or by using [http://egd.sourceforge.net/ EGD, the Entropy Gathering Daemon]. For other systems, OpenSSL might use a different source of entropy.


```c
#include <inttypes.h>
#include <stdio.h>

#include <openssl/err.h>
#include <openssl/rand.h>

int
main()
{
  uint32_t v;

  if (RAND_bytes((unsigned char *)&v, sizeof v) == 0) {
    ERR_print_errors_fp(stderr);
    return 1;
  }
  printf("%" PRIu32 "\n", v);
  return 0;
}
```



###  Windows

```c
#include <stdio.h>  /* printf */
#include <windows.h>
#include <wincrypt.h> /* CryptAcquireContext, CryptGenRandom */

int
main()
{
  HCRYPTPROV p;
  ULONG i;

  if (CryptAcquireContext(&p, NULL, NULL,
      PROV_RSA_FULL, CRYPT_VERIFYCONTEXT) == FALSE) {
    fputs("CryptAcquireContext failed.\n", stderr);
    return 1;
  }
  if (CryptGenRandom(p, sizeof i, (BYTE *)&i) == FALSE) {
    fputs("CryptGenRandom failed.\n", stderr);
    return 1;
  }
  printf("%lu\n", i);
  CryptReleaseContext(p, 0);
  return 0;
}
```


== {{header|C++}} ==
<code>std::random_device</code> is a uniformly-distributed integer random number generator that produces non-deterministic random numbers.

Note that <code>std::random_device</code> may be implemented in terms of a pseudo-random number engine if a non-deterministic source (e.g. a hardware device) is not available to the implementation.

See the C++ section on [[Random_number_generator_(included)#C.2B.2B|Random number generator (included)]] for the list of pseudo-random number engines available.
```cpp
#include <iostream>
#include <random>

int main()
{
    std::random_device rd;
    std::uniform_int_distribution<long> dist; // long is guaranteed to be 32 bits

    std::cout << "Random Number: " << dist(rd) << std::endl;
}
```



## C#


```c#
using System;
using System.Security.Cryptography;

private static int GetRandomInt()
{
  int result = 0;
  var rng = new RNGCryptoServiceProvider();
  var buffer = new byte[4];

  rng.GetBytes(buffer);
  result = BitConverter.ToInt32(buffer, 0);

  return result;
}
```


Park-Miller random number generator

```c#

const long m = 2147483647L;
const long a = 48271L;
const long q = 44488L;
const long r = 3399L;
static long r_seed = 12345678L;

public static byte gen()
{
   long hi = r_seed / q;
   long lo = r_seed - q * hi;
   long t = a * lo - r * hi;
       if (t > 0)
           r_seed = t;
       else
           r_seed = t + m;
       return (byte)r_seed;
}

public static void ParkMiller(byte[] arr)
{
   byte[] arr = new byte[10900000];
    for (int i = 0; i < arr.Length; i++)
                {
                       arr[i] = gen();
                }
}
```


== {{header|ChucK}} ==

```c
 Math.random2(-(Math.random()),Math.random();
```



## Common Lisp


```lisp
(defun random-int32 ()
  (with-open-file (s "/dev/random" :element-type '(unsigned-byte 32))
    (read-byte s)))
```




## D

Example of MersenneTwisterEngine for generating uniformly-distributed 32-bit numbers with a period of 2 to the power of 19937.

```d

import std.stdio;
import std.random;

void main()
{
  Mt19937 gen;
  gen.seed(unpredictableSeed);
  auto n = gen.front;
  writeln(n);
}

```


```txt

run 1: 3500391376
run 2: 9537841895
run 3: 1588499117
run 4: ...

```



## EchoLisp

No random device provided by the host (browser). But we can use the system timer to get a physical input.

```lisp

(random-seed "simon")
(random (expt 2 32)) → 2275215386
(random-seed "simon")
(random (expt 2 32)) → 2275215386 ;; the same


(random-seed (current-time-milliseconds ))
(random (expt 2 32)) → 4061857345
(random-seed (current-time-milliseconds ))
(random (expt 2 32)) → 1322611152

```



## Factor

Factor has good support for switching between different random number generators. <code>with-system-random</code> is a combinator that encapsulates the task of using a system RNG (/dev/random in the case of GNU/Linux).

```factor
USE: random
[ random-32 ] with-system-random .
```



## Forth


```forth
variable rnd

: randoms ( n -- )
  s" /dev/random" r/o open-file throw
  swap 0 do
    dup rnd 1 cells rot read-file throw drop
    rnd @ .
  loop
  close-file throw ;
```



## Fortran

Using system /dev/urandom in [[GNU]]/[[Linux]].


```fortran

!-----------------------------------------------------------------------
! Test Linux urandom in Fortran
!-----------------------------------------------------------------------
program    urandom_test
  use iso_c_binding, only : c_long
  implicit none

  character(len=*), parameter :: RANDOM_PATH = "/dev/urandom"
  integer :: funit, ios
  integer(c_long) :: buf

  open(newunit=funit, file=RANDOM_PATH, access="stream", form="UNFORMATTED", &
       iostat=ios, status="old", action="read")
  if ( ios /= 0 ) stop "Error opening file: "//RANDOM_PATH

  read(funit) buf

  close(funit)

  write(*,'(A,I64)') "Integer:     ", buf
  write(*,'(A,B64)') "Binary:      ", buf
  write(*,'(A,Z64)') "Hexadecimal: ", buf

end program urandom_test

```


Here's an example of the use of the latter:

## FreeBASIC

FreeBASIC can in theory use any C library to produce pseudo-random numbers including those which are partly device based.

However, in practice, there is little need for this as specifying a second parameter of 5 to FB's Randomize statement produces cryptographically strong pseudo-random numbers using either the Win32 Crypto API or the /dev/urandom device under Linux.

```freebasic
' FB 1.05.0 Win64

Randomize , 5

'generate 10 cryptographic random integers in the range 1 To 100
For i As Integer = 1 To 10
  Print Int(Rnd * 100) + 1
Next

Sleep
```



## GlovePIE


```glovepie
var.rand=random(10)
```



## Go

In the Go library is crypto/rand, a source specified to use dev/urandom on Unix-like systems and the CryptGenRandom API on Windows.  Also implemented here is a source using dev/random, if you really want it.  On my system it would print a few numbers then hang until I moved the mouse or pressed some keys on the keyboard.

```go
package main

import (
    "crypto/rand"
    "encoding/binary"
    "fmt"
    "io"
    "os"
)

func main() {
    testRandom("crypto/rand", rand.Reader)
    testRandom("dev/random", newDevRandom())
}

func newDevRandom() (f *os.File) {
    var err error
    if f, err = os.Open("/dev/random"); err != nil {
        panic(err)
    }
    return
}

func testRandom(label string, src io.Reader) {
    fmt.Printf("%s:\n", label)
    var r int32
    for i := 0; i < 10; i++ {
        if err := binary.Read(src, binary.LittleEndian, &r); err != nil {
            panic(err)
        }
        fmt.Print(r, " ")
    }
    fmt.Println()
}
```



## Groovy

Based, necessarily, on Java solution:

```groovy
def rng = new java.security.SecureRandom()
```


Test:

```groovy
(0..4).each { println rng.nextInt() }
```


```txt
380425053
-1003791794
-1972330603
1152610574
714616658
```


=={{header|Icon}} and {{header|Unicon}}==

The following is Unicon-specific but trivially converted into Icon.


```unicon
procedure main(A)
    n := integer(A[1])|5
    every !n do write(rand(4))
end

procedure rand(n)
    f := open("/dev/urandom") | stop("Cannot get to urandom!")
    x := 0
    every !n do x := x*256 + ord(reads(f,1))
    close(f)
    return x
end
```


Sample runs:

```txt

->urand
910795827
1135996175
3545606085
944909079
2464790129
->

```



## J


Untested:

```j
256#.a.i.1!:11'/dev/urandom';0 4
```


Fallback:

```j
256#.a.i.4{.host'dd if=/dev/urandom bs=4 count=1'
```


Note: this assumes that J is running on linux.


## Java


```java
import java.security.SecureRandom;

public class RandomExample {
  public static void main(String[] args) {
    SecureRandom rng = new SecureRandom();

    /* Prints a random signed 32-bit integer. */
    System.out.println(rng.nextInt());
  }
}
```



## jq

jq does not provide direct access to /dev/urandom, so in the following we assume the availability of `od`, `tr`, and `fold`, and illustrate how to produce an indefinitely long stream of pseudo-random numbers that are approximately uniformly distributed in the range [0,1].

Assuming the jq program shown below is in a file named uniform.jq, the command-line invocation would be:

```txt
od -t x -An /dev/urandom | tr -d " " | fold -w 8 | jq -R -f uniform.jq
```



```jq
# allow both upper and lower-case characters
def hex2integer:
  explode
  | reverse
  | map(if . > 96  then . - 87 elif . > 64 then . - 55 else . - 48 end)
  | reduce .[] as $c
      # state: [power, ans]
      ([1,0]; (.[0] * 16) as $b | [$b, .[1] + (.[0] * $c)])
  | .[1];

select(length>0) | hex2integer / pow(16;length)
```


Notice that the program automatically adjusts the precision based on the length of the hexadecimal numbers presented.  Since jq uses IEEE 754 64-bit arithmetic, specifying a larger value to `fold`, such as 10, will produce more precise results.

## Julia

```Julia

const rdev = "/dev/random"
rstream = try
    open(rdev, "r")
catch
    false
end

if isa(rstream, IOStream)
    b = readbytes(rstream, 4)
    close(rstream)
    i = reinterpret(Int32, b)[1]
    println("A hardware random number is:  ", i)
else
    println("The hardware random number stream, ", rdev, ", was unavailable.")
end

```


```txt

A hardware random number is:  986109744

```



## Haskell

```haskell
#!/usr/bin/env runhaskell

import System.Entropy
import Data.Binary.Get
import qualified Data.ByteString.Lazy as B

main = do
  bytes <- getEntropy 4
  print (runGet getWord32be $ B.fromChunks [bytes])
```



## Kotlin


```scala
// version 1.1.2

import java.security.SecureRandom

fun main(args: Array<String>) {
    val rng = SecureRandom()
    val rn1 = rng.nextInt()
    val rn2 = rng.nextInt()
    val newSeed = rn1.toLong() * rn2
    rng.setSeed(newSeed)    // reseed using the previous 2 random numbers
    println(rng.nextInt())  // get random 32-bit number and print it
}
```



## Lasso


```lasso
file(`/dev/urandom`)->readSomeBytes(4)->export32bits
```

```txt
723217350
```



## M2000 Interpreter


```M2000 Interpreter

Module checkit {
      Declare random1 lib "advapi32.SystemFunction036" {long lpbuffer, long length}
      Buffer Clear Alfa as long*2
      Print Eval(Alfa,0)
      Print Eval(Alfa,1)
      call void random1(alfa(0), 8)
      Print Eval(Alfa,0)
      Print Eval(Alfa,1)
}
checkit

```




```M2000 Interpreter

Function Random2  {
      Declare CryptAcquireContext Lib "advapi32.CryptAcquireContextW" {long ByRefhProv,  pszContainer$,pszProvider$, long dwProvType, long dwFlags}
      Declare CryptReleaseContext Lib "advapi32.CryptReleaseContext" {Long hProv, Long dwFlags}
      Declare CryptGenRandom Lib "advapi32.CryptGenRandom" {Long hProv, Long dwLen, Long ByRef}
      Const PROV_RSA_FULL As Long = 1
      Const VERIFY_CONTEXT As Long = 0xF0000000&
      Buffer Clear RandomNum as Long
      Buffer Clear hProv as long
      Call Void CryptAcquireContext( hProv(0), "", "", PROV_RSA_FULL, VERIFY_CONTEXT)
      Call Void CryptGenRandom( Eval(hProv,0), 4, RandomNum(0))
      Call Void CryptReleaseContext(Eval(hProv,0), 0&)
      =Eval(RandomNum,0)
}
Print Random2()

```



## Mathematica


```Mathematica
rand32[] := RandomInteger[{-2^31, 2^31 - 1}]
```


Example: create array of 10 rand32 numbers

```Mathematica
Table[rand32[], {i, 1, 10}]
```


```txt
{355587317, -869860319, -91421859, 1605907693, 101463390, 891823090,
-531713717, -1038608428, 1717313407, 674189312}
```



## NetRexx

{{Works with|Mac OS X}} and probably other UNIX systems that provide <tt>/dev/random</tt> or <tt>/dev/urandom</tt> random data source devices.<br />

```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

import java.math.BigInteger

randomDevNameFile = File
randomDevNameList = ['/dev/random', '/dev/urandom'] -- list of random data source devices
randomDevIStream = InputStream
do
  loop dn = 0 to randomDevNameList.length - 1
    randomDevNameFile = File(randomDevNameList[dn])
    if randomDevNameFile.exists() then leave dn -- We're done! Use this device
    randomDevNameFile = null -- ensure we don't use a non-existant device
    end dn
  if randomDevNameFile == null then signal FileNotFoundException('Cannot locate a random data source device on this system')

  -- read 8 bytes from the random data source device, convert it into a BigInteger then display the result
  randomBytes = byte[8]
  randomDevIStream = BufferedInputStream(FileInputStream(randomDevNameFile))
  randomDevIStream.read(randomBytes, 0, randomBytes.length)
  randomDevIStream.close()
  randomNum = BigInteger(randomBytes)
  say Rexx(randomNum.longValue()).right(24) '0x'Rexx(Long.toHexString(randomNum.longValue())).right(16, 0)
catch ex = IOException
  ex.printStackTrace()
end
return

/*
To run the program in a loop 10 times from a bash shell prompt use:
for ((i=0; i<10; ++i)); do java <program_name>; done # Shell loop to run the command 10 times
*/

```

```txt

$ for ((i=0; i<10; ++i)); do java RRandomGen; done # Shell loop to run the command 10 times
    -3724652236619320966 0xcc4f60865c70f17a
    -8287324416757903696 0x8cfd8259e0b94eb0
    -2951181559250748016 0xd70b4c02052cfd90
     8171526404483923658 0x716717f863fd3eca
    -4285529734202916706 0xc486bd699676009e
     4783094698411310978 0x4260f74949dc3f82
     6972277496665184225 0x60c28171482d97e1
    -2382194670272317046 0xdef0be919c96f98a
     7952058769071853043 0x6e5b6351938ecdf3
    -1857830580859698636 0xe637a8ee0f000234
$

```



## Nim


```nim
var f = open("/dev/urandom")
var r: int32
discard f.readBuffer(addr r, 4)
close(f)
echo r
```



## OCaml


OCaml's default integers are 31 bits on 32 bits architectures:


```ocaml
let input_rand_int ic =
  let i1 = int_of_char (input_char ic)
  and i2 = int_of_char (input_char ic)
  and i3 = int_of_char (input_char ic)
  and i4 = int_of_char (input_char ic) in
  i1 lor (i2 lsl 8) lor (i3 lsl 16) lor (i4 lsl 24)

let () =
  let ic = open_in "/dev/urandom" in
  let ri31 = input_rand_int ic in
  close_in ic;
  Printf.printf "%d\n" ri31;
;;
```


but if we really want 32 bits integers there is a module for this:


```ocaml
let input_rand_int32 ic =
  let i1 = Int32.of_int (int_of_char (input_char ic))
  and i2 = Int32.of_int (int_of_char (input_char ic))
  and i3 = Int32.of_int (int_of_char (input_char ic))
  and i4 = Int32.of_int (int_of_char (input_char ic)) in
  let i2 = Int32.shift_left i2 8
  and i3 = Int32.shift_left i3 16
  and i4 = Int32.shift_left i4 24 in
  Int32.logor i1 (Int32.logor i2 (Int32.logor i3 i4))

let () =
  let ic = open_in "/dev/urandom" in
  let ri32 = input_rand_int32 ic in
  close_in ic;
  Printf.printf "%ld\n" ri32;
;;
```



## PARI/GP

It works on systems having /dev/urandom and Linux.


```parigp
rnd(n=10)=extern("cat /dev/urandom|tr -dc '[:digit:]'|fold -w"n"|head -1")
```


The code above creates a new function rnd() which returns cryptographically strong integers with max. 10 random digits from /dev/urandom. rnd(n) returns integer with max. n random digits. No leading zeros.
```txt

rnd() = 3055652197
rnd(20) = 75735303746547944580
...

```



## Pascal

This works with FreePascal on "unixoids":

```pascal
program RandomNumberDevice;
var
  byteFile: file of byte;
  randomByte: byte;
begin
  assign(byteFile, '/dev/urandom');
  reset (byteFile);
  read  (byteFile, randomByte);
  close (byteFile);
  writeln('The random byte is: ', randomByte);
end.

```

```txt

>: ./RandomNumberDevice
The random byte is: 9
>: ./RandomNumberDevice
The random byte is: 237

```



## Perl

Typically one would use a module as they will work on UNIX, Win32, and other O/S's.  Crypt::Random::Seed, for instance, will use Win32 sources, EGD/PRNGD, /dev/u?random, or if none of those exist for some reason, a userspace entropy method.

```Perl
use Crypt::Random::Seed;
my $source = Crypt::Random::Seed->new( NonBlocking => 1 ); # Allow non-blocking sources like /dev/urandom
print "$_\n" for $source->random_values(10);               # A method returning an array of 32-bit values
```

or (similar but many more dependencies):

```Perl
use Crypt::Random::Source qw/get_weak/;    # Alternately get_strong
print unpack('L*',get_weak(4)), "\n" for 1..10;
```


Or we can read values from /dev/urandom ourselves:

```Perl
sub read_random {
        my $device = '/dev/urandom';
        open my $in, "<:raw", $device   # :raw because it's not unicode string
                or die "Can't open $device: $!";

        sysread $in, my $rand, 4 * shift;
        unpack('L*', $rand);
}

print "$_\n" for read_random(10);
```

Whether /dev/urandom is good enough for cryptographic work is debated, though on most UNIX systems it is at least as good as the Win32 Crypto API.


## Perl 6

A lazy list of random numbers:


```perl6
use experimental :pack;
my $UR = open("/dev/urandom", :bin) orelse .die;
my @random-spigot = $UR.read(1024).unpack("L*") ... *;

.say for @random-spigot[^10];
```

```txt
1431009271
1702240522
670020272
588612037
1864913839
2155430433
1690056587
385405103
2366495746
692037942
```



## Phix

My machine does not support the rdrand instruction.

Tested as best I can by commenting out the jnc instructions and replacing rdrand with rdtsc.

I have uploaded replacement pttree.e and pilasm.e (use at your own risk) for
anyone wanting to test prior to 0.8.0 being shipped.

If your chip does not support rdrand, you get {1,0}, else {0,-2147483648..2147483647}.

For completeness, I have shown how to convert the signed result to an unsigned one.


```Phix
integer res  -- 1=failure, 0=success
atom rint = 0   -- random 32-bit int

#ilASM{
        mov eax,1
        cpuid
        bt ecx,30
        mov edi,1 -- exit code: failure
        jnc :exit

        -- rdrand sets CF=0 if no random number
        -- was available. Intel documentation
        -- recommends 10 retries in a tight loop
        mov ecx,11
    ::loop1
        sub ecx, 1
        jz :exit -- exit code is set already
        rdrand eax
        -- (the above generates exception #C000001D if not supported)
--      rdtsc
        jnc :loop1

        lea edi,[rint]
        call :%pStoreMint
        xor edi,edi

    ::exit
        mov [res],edi
        xor ebx,ebx     -- important!
      }

?{res,rint}

if res=0 then   -- (success)

    --
    -- To convert a signed 32-bit int to an unsigned one:
    --
    --  method 1
--  atom urint1 = rint
--  if urint1<0 then urint1+=#100000000 end if
    atom urint1 = rint+iff(rint<0?#100000000:0)

    --  method 2
    atom pMem = allocate(4)
    poke4(pMem,rint)
    atom urint2 = peek4u(pMem)
    free(pMem)

    --  method 3
    atom urint3 = bytes_to_int(int_to_bytes(rint,4),signed:=false)

    ?{urint1,urint2,urint3}

end if
```

A linux-only solution:

```Phix
integer fn = open("/dev/urandom","rb")
if fn=-1 then
    puts(1,"cannot open /dev/urandom\n")
else
    sequence s = {}
    for i=1 to 4 do
        s &= getc(fn)
    end for
    close(fn)
    ?bytes_to_int(s,signed:=false)
end if
```



## PicoLisp


```PicoLisp
: (in "/dev/urandom" (rd 4))
-> 2917110327
```



## PowerShell


```PowerShell

function Get-RandomInteger
{
    Param
    (
        [Parameter(Mandatory=$false,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        [ValidateScript({$_ -ge 4})]
        [int[]]
        $InputObject = 64
    )

    Begin
    {
        $rng = New-Object -TypeName System.Security.Cryptography.RNGCryptoServiceProvider
    }
    Process
    {
        foreach($count in $InputObject)
        {
            $bytes = New-Object -TypeName Byte[] -Argument $count
            $rng.GetBytes($bytes)
            [System.BitConverter]::ToInt32($bytes,0)
        }
    }
    End
    {
        Remove-Variable -Name rng -Scope Local
    }
}

```


```PowerShell

4,8,16,32,64,128 | Get-RandomInteger | Format-Wide {$_} -Column 6 -Force

```

```txt

1402572656             432337086              413089699             1404567509            -82797202             -261009960

```

As hexadecimal:

```PowerShell

4,8,16,32,64,128 | Get-RandomInteger | Format-Wide {"0x{0:X}" -f $_} -Column 6 -Force

```

```txt

0x24305255             0x916002DD             0x9587046             0x5F236274            0xC0BAF6F0            0xC0B93118

```



## ProDOS

Uses math module:

```ProDOS
printline -random-
```


## PureBasic

PureBasic has the source for the random data is the "/dev/urandom" device on Linux or Mac OSX and the "Microsoft Cryptography API" on Windows.

```PureBasic
If OpenCryptRandom()
  MyRandom = CryptRandom(#MAXLONG)
  CloseCryptRandom()
EndIf
```



## Python


```Python
import random
rand = random.SystemRandom()
rand.randint(1,10)
```



## Racket


```Racket

#lang racket
;; Assuming a device to provide random bits:
(call-with-input-file* "/dev/random"
  (λ(i) (integer-bytes->integer (read-bytes 4 i) #f)))

```



## REXX


### version 1

The   32-bit   random number is unsigned and constructed from two smaller 16-bit   numbers,   and it's expressed in decimal.

Note:   the REXX   '''random'''   BIF has a maximum range
of   100,000.

```rexx
/*REXX program  generates and displays a random  32-bit  number  using the  RANDOM  BIF.*/
numeric digits 10                                /*ensure REXX has enough decimal digits*/
_=2**16                                          /*a handy─dandy constant to have around*/
r#= random(0, _-1) * _    +    random(0, _-1)    /*generate an unsigned 32-bit random #.*/
say r#                                           /*stick a fork in it,  we're all done. */
```

```txt

4294967296

```



### version 2

This program generates a random 4 byte character string in the range '00000000'x to 'ffffffff'x

```rexx
left=0
rite=0
lo=hex(left)hex(rite)
Say 'low   ' c2x(lo)
left=random(0,2**16-1)
rite=random(0,2**16-1)
rand=hex(left)hex(rite)
Say 'random' c2x(rand)
left=2**16-1
rite=2**16-1
hi=hex(left)hex(rite)
Say 'high  ' c2x(hi)
Exit
hex: Return d2c(arg(1),2)
```

```txt
low    00000000
random 3E4C3CDE
high   FFFFFFFF
```



## Ring


```ring

nr = 10
for i = 1 to nr
    see random(i) + nl
next

```



## Ruby

Ruby 1.8.7 introduces the 'securerandom' library. For [[MRI]] users, this library tries to get random numbers by loading OpenSSL, or opening /dev/urandom, or calling CryptGenRandom.

```Ruby
require 'securerandom'
SecureRandom.random_number(1 << 32)
```



## Rust

<code>rand</code> used to be part of Rust standard library but it was extracted as a 'crate' (https://crates.io/crates/rand).  <code>OsRng</code> uses the appropriate device for many platforms including Unix, Windows, BSD, and iOS (listed [https://docs.rs/rand/0.4/rand/os/struct.OsRng.html here]).  Other methods like <code>RDRAND</code> can be found in other crates (https://crates.io/crates/rdrand).


```rust
extern crate rand;

use rand::{OsRng, Rng};

fn main() {
    // because `OsRng` opens files, it may fail
    let mut rng = match OsRng::new() {
        Ok(v) => v,
        Err(e) => panic!("Failed to obtain OS RNG: {}", e)
    };

    let rand_num: u32 = rng.gen();
    println!("{}", rand_num);
}
```



## Scala


```Scala
import java.security.SecureRandom

object RandomExample extends App {
  new SecureRandom {
    val newSeed: Long = this.nextInt().toLong * this.nextInt()
    this.setSeed(newSeed) // reseed using the previous 2 random numbers
    println(this.nextInt()) // get random 32-bit number and print it
  }
}
```


## Sidef


```ruby
func urandom() {
    const device = %f'/dev/urandom';

    device.open('<:raw', \var fh, \var err) ->
        || die "Can't open `#{device}': #{err}";

    fh.sysread(\var noise, 4);
    'L'.unpack(noise);
}

say urandom();    # sample: 3517432564
```



## Tcl


```tcl
package require Tcl 8.5

# Allow override of device name
proc systemRandomInteger {{device "/dev/random"}} {
    set f [open $device "rb"]
    binary scan [read $f 4] "I" x
    close $f
    return $x
}
```

Usage:

```tcl
% puts [systemRandomInteger]
636131349
```



## UNIX Shell


```UNIX Shell
od -An -N 4 -t u4 /dev/urandom
```



## Wee Basic

Due to how the code works, any key has to be entered to generate the random number.

```Wee Basic
let keycode=0
let number=1
print 1 "Press any key to generate a random number from 1 to 10.
while keycode=0
let number=number+1
let keycode=key()
rem The maximum number is the number in the "if number=" line with 1 taken away. For example, if this number was 11, the maximum number would be 10. *
if number=11
let number=1
endif
wend
print 1 number
end
```



## X86 Assembly

Processors supporting the new RDRAND feature can generate a random 32-bit integer in two instructions:

```x86
L: rdrand eax
jnc L
```

RDRAND reads the CPU's cryptographically-secure hardware random number generator. The loop is needed because RDRAND can occasionally fail to retrieve a value — it sets the carry flag to indicate whether it succeeded.


## XPL0

The random number generator is seeded with the 32-bit system timer each
time a program starts. From then on, a linear congruential algorithm is
used (that passes the Diehard test suite). Since the Ran intrinsic routine
returns a signed positive integer (modulo the argument), the value is
limited to 31 bits.


```XPL0
code Ran=1;
int R;
R:= Ran($7FFF_FFFF)
```



## zkl

Linux:

```zkl
const RANDOM_PATH="/dev/urandom";

fin,buf:=File(RANDOM_PATH,"r"), fin.read(4);
fin.close();  // GC would also close the file
println(buf.toBigEndian(0,4));  // 4 bytes @ offset 0
```

```txt

2152746403

```

