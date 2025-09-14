+++
title = "Machine code"
description = ""
date = 2019-07-13T23:04:39Z
aliases = []
[extra]
id = 17023
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "autohotkey",
  "bbc_basic",
  "c",
  "cobol",
  "common_lisp",
  "d",
  "go",
  "julia",
  "kotlin",
  "m2000_interpreter",
  "nim",
  "pari_gp",
  "pascal",
  "phix",
  "picolisp",
  "purebasic",
  "python",
  "racket",
  "rust",
  "scala",
  "swift",
  "tcl",
]
+++

The task requires poking machine code directly into memory and executing it.

This is strictly for x86 (32 bit) architectures.

The machine code is the opcodes of the following simple program:


```asm
mov EAX, [ESP+4]
add EAX, [ESP+8]
ret
```


which translates into the following opcodes:
(139 68 36 4 3 68 36 8 195)
and in Hex this would correspond to the following:
("8B" "44" "24" "4" "3" "44" "24" "8" "C3")


## Task

Implement the following in your favorite programming language (take the common lisp code as an example if you wish):
<ol>
<li> Poke the above opcodes into a memory pointer</li>
<li>Execute it with the following arguments: [ESP+4] => unsigned-byte argument of value 7; [ESP+8] => unsigned-byte argument of value 12; The result would be 19.</li>
<li>Free the Pointer</li>
</ol>





## AutoHotkey

'''MCode Tutorial''' ([http://ahkscript.org/boards/viewtopic.php?f=7&t=32 Forum Thread])

'''MCode4GCC''' ([http://ahkscript.org/boards/viewtopic.php?f=6&t=4642 Forum Thread] | [https://github.com/joedf/MCode4GCC GitHub]) - An MCode generator using the GCC Compiler.

```AutoHotkey
MCode(Var, "8B44240403442408C3")
MsgBox, % DllCall(&Var, "Char",7, "Char",12)
Var := ""
return

; http://www.autohotkey.com/board/topic/19483-machine-code-functions-bit-wizardry/
MCode(ByRef code, hex) { ; allocate memory and write Machine Code there
   VarSetCapacity(code, StrLen(hex) // 2)
   Loop % StrLen(hex) // 2
      NumPut("0x" . SubStr(hex, 2 * A_Index - 1, 2), code, A_Index - 1, "Char")
}
```



## BBC BASIC

'''Note''' that ''BBC BASIC for Windows'' includes an 80386/80486 assembler as standard!


```bbcbasic
      REM Claim 9 bytes of memory
      SYS "GlobalAlloc",0,9 TO code%

      REM Poke machine code into it
      P%=code%
      [OPT 0
      mov EAX, [ESP+4]
      add EAX, [ESP+8]
      ret
      ]

      REM Run code
      SYS code%,7,12 TO result%
      PRINT result%

      REM Free memory
      SYS "GlobalFree",code%
      END
```



## C


```c
#include <stdio.h>
#include <sys/mman.h>
#include <string.h>

int test (int a, int b)
{
  /*
       mov EAX, [ESP+4]
       add EAX, [ESP+8]
       ret
  */
  char code[] = {0x8B, 0x44, 0x24, 0x4, 0x3, 0x44, 0x24, 0x8, 0xC3};
  void *buf;
  int c;
  /* copy code to executable buffer */
  buf = mmap (0,sizeof(code),PROT_READ|PROT_WRITE|PROT_EXEC,
             MAP_PRIVATE|MAP_ANON,-1,0);

  memcpy (buf, code, sizeof(code));
  /* run code */
  c = ((int (*) (int, int))buf)(a, b);
  /* free buffer */
  munmap (buf, sizeof(code));
  return c;
}

int main ()
{
  printf("%d\n", test(7,12));
  return 0;
}
```



## COBOL

This solution is a 64-bit adaptation of the task, using the macOS ABI and 64-bit instructions. The assembly code in question is:


```Assembler
pushq	%rbp
movq	%rsp, %rbp
movl	%edi, -0x4(%rbp)
movl	%esi, -0x8(%rbp)
movl	-0x4(%rbp), %esi
addl	-0x8(%rbp), %esi
movl	%esi, -0xc(%rbp)
movl	-0xc(%rbp), %eax
popq	%rbp
retq

```

The 64-bit "wrapper code" used by the PicoLisp and Go implementations have the parameters <code>7</code> and <code>12</code> baked into it, so I opted for a pure 64-bit implementation rather than manipulating the 64-bit stack to support the 32-bit instructions.

```COBOL>       >
SOURCE FORMAT IS FIXED
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 INSTRUCTIONS.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'55'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'48'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'89'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'E5'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'89'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'7D'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'FC'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'89'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'75'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'F8'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'8B'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'75'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'FC'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'03'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'75'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'F8'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'89'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'75'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'F4'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'8B'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'45'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'F4'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'5D'.
              03 USAGE BINARY-CHAR UNSIGNED VALUE H'C3'.
           01 MMAP.
              03 MMAP-ADDR   USAGE POINTER VALUE NULL.
              03 MMAP-LEN    USAGE BINARY-LONG UNSIGNED VALUE 24.
              03 MMAP-PROT   USAGE BINARY-INT VALUE H'0007'.
              03 MMAP-FLAGS  USAGE BINARY-INT VALUE H'1002'.
              03 MMAP-FD     USAGE BINARY-INT VALUE -1.
              03 MMAP-OFFSET USAGE BINARY-LONG VALUE 0.
           03 CODE-PTR USAGE PROCEDURE-POINTER.
           01 ARG-A USAGE BINARY-INT VALUE 7.
           01 ARG-B USAGE BINARY-INT VALUE 12.
           01 RESULT USAGE BINARY-INT.
       LINKAGE SECTION.
           01 MACHINE-CODE PIC X(24).
       PROCEDURE DIVISION.
       MAIN SECTION.
           PERFORM SET-UP.
           CALL CODE-PTR USING
              BY VALUE ARG-A
              BY VALUE ARG-B
              RETURNING RESULT.
           DISPLAY RESULT.
           PERFORM TEAR-DOWN.
           STOP RUN.

       SET-UP SECTION.
           CALL 'mmap' USING
              BY VALUE MMAP-ADDR
              BY VALUE MMAP-LEN
              BY VALUE MMAP-PROT
              BY VALUE MMAP-FLAGS
              BY VALUE MMAP-FD
              BY VALUE MMAP-OFFSET
              RETURNING CODE-PTR.
           SET ADDRESS OF MACHINE-CODE TO CODE-PTR.
           MOVE INSTRUCTIONS TO MACHINE-CODE.

       TEAR-DOWN SECTION.
           SET ADDRESS OF MACHINE-CODE TO NULL.
           CALL 'munmap' USING
              BY VALUE CODE-PTR
              BY VALUE MMAP-LEN.



```

+0000000019


## Common Lisp


```lisp
;;Note that by using the 'CFFI' library, one can apply this procedure portably in any lisp implementation;
;; in this code however I chose to demonstrate only the implementation-dependent programs.

;;CCL
;; Allocate a memory pointer and poke the opcode into it
(defparameter ptr (ccl::malloc 9))

(loop for i in '(139 68 36 4 3 68 36 8 195)
   for j from 0 do
   (setf (ccl::%get-unsigned-byte ptr j) i))

;; Execute with the required arguments and return the result as an unsigned-byte
(ccl::ff-call ptr :UNSIGNED-BYTE 7 :UNSIGNED-BYTE 12 :UNSIGNED-BYTE)

;; Output = 19

;; Free the pointer
(ccl::free ptr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;SBCL
(defparameter mmap (list 139 68 36 4 3 68 36 8 195))

(defparameter pointer (sb-alien:make-alien sb-alien:unsigned-char (length mmap)))

(defparameter callp (loop for byte in mmap
                          for i from 0
		       do
		       (setf (sb-alien:deref pointer i) byte)
		       finally
		       (return (sb-alien:cast pointer (function integer integer integer)))))

(sb-alien:alien-funcall callp 7 12)

(loop for i from 0 below 18 collect (sb-alien:deref ptr i))

(sb-alien:free-alien pointer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;CLISP
(defparameter mmap (list 139 68 36 4 3 68 36 8 195))

(defparameter POINTER (FFI:FOREIGN-ADDRESS  (FFI:FOREIGN-ALLOCATE 'FFI:UINT8 :COUNT 9)))

(loop for i in mmap
   for j from 0 do
   (FUNCALL #'(SETF FFI:MEMORY-AS) i POINTER 'FFI:INT j))

(FUNCALL
 (FFI:FOREIGN-FUNCTION POINTER
		       (LOAD-TIME-VALUE
			(FFI:PARSE-C-TYPE
			 '(FFI:C-FUNCTION (:ARGUMENTS 'FFI:INT 'FFI:INT) (:RETURN-TYPE FFI:INT) (:LANGUAGE :STDC)))))
 7 12)

(FFI:FOREIGN-FREE POINTER)

```



## D

In D you usually use a nicer <code>asm {}</code> statement for similar purposes.

Generally new operating systems forbid execution of any address unless it's known to contain executable code. This is a basic version that unlike the C entry executes from array memory. This may crash on some operating systems.

```d
int test(in int a, in int b) pure nothrow @nogc {
    /*
    mov EAX, [ESP+4]
    add EAX, [ESP+8]
    ret
    */
    immutable ubyte[9] code = [0x8B, 0x44, 0x24, 0x4, 0x3, 0x44, 0x24, 0x8, 0xC3];
    alias F = extern(C) int function(int, int) pure nothrow @nogc;
    immutable f = cast(F)code.ptr;
    return f(a, b); // Run code.
}

void main() {
    import std.stdio;

    test(7, 12).writeln;
}
```

  19


## Go

This task requires the use of 'cgo' which enables Go to interface with C code by importing a pseudo-package called "C".

Although Go supports both 32-bit and 64-bit architectures, I'm writing this on a 64-bit Ubuntu 16.04 system. I've therefore utilized the PicoLisp entry's 'glue code' to enable the 32-bit code to run on it.

There doesn't appear to be a way to cast a pointer to a native buffer to a Go function pointer so that the machine code can be run directly. I've therefore written a C function to perform this step and embedded it in the program which 'cgo' allows us to do.

```go
package main

import "fmt"

/*
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <string.h>

typedef unsigned char byte;
typedef byte (*mcfunc) (byte, byte);

void runMachineCode(void *buf, byte a, byte b) {
    mcfunc fp = (mcfunc)buf;
    printf("%d\n", fp(a, b));
}
*/
import "C"

func main() {
    code := []byte{
        144, // Align
        144,
        106, 12, // Prepare stack
        184, 7, 0, 0, 0,
        72, 193, 224, 32,
        80,
        139, 68, 36, 4, 3, 68, 36, 8, // Rosetta task code
        76, 137, 227, // Get result
        137, 195,
        72, 193, 227, 4,
        128, 203, 2,
        72, 131, 196, 16, // Clean up stack
        195, // Return
    }
    le := len(code)
    buf := C.mmap(nil, C.size_t(le), C.PROT_READ|C.PROT_WRITE|C.PROT_EXEC,
        C.MAP_PRIVATE|C.MAP_ANON, -1, 0)
    codePtr := C.CBytes(code)
    C.memcpy(buf, codePtr, C.size_t(le))
    var a, b byte = 7, 12
    fmt.Printf("%d + %d = ", a, b)
    C.runMachineCode(buf, C.byte(a), C.byte(b))
    C.munmap(buf, C.size_t(le))
    C.free(codePtr)
}
```


```txt

7 + 12 = 19

```



## Julia

Julia cannot execute machine code directly, but can embed C and C++ with the Cxx library.

```julia
using Cxx

cxx"""
#include <stdio.h>
#include <sys/mman.h>
#include <string.h>

int test (int a, int b)
{
  /*
       mov EAX, [ESP+4]
       add EAX, [ESP+8]
       ret
  */
  char code[] = {0x8B, 0x44, 0x24, 0x4, 0x3, 0x44, 0x24, 0x8, 0xC3};
  void *buf;
  int c;
  /* copy code to executable buffer */
  buf = mmap (0,sizeof(code),PROT_READ|PROT_WRITE|PROT_EXEC,
             MAP_PRIVATE|MAP_ANON,-1,0);

  memcpy (buf, code, sizeof(code));
  /* run code */
  c = ((int (*) (int, int))buf)(a, b);
  /* free buffer */
  munmap (buf, sizeof(code));
  return c;
}

int main ()
{
  printf("%d\n", test(7,12));
  return 0;
}
"""

julia_function = @cxx main()
julia_function()

```



## Kotlin

This task presents a number of issues for Kotlin Native which at the time of writing (August 2017) is still in the earlier stages of development:-

1. The language doesn't (yet) have an unsigned Byte type, though this is easily solved by subtracting 256 from unsigned values between 128 and 255 inclusive and then using the signed Byte type.

2. As far as x86 is concerned, the language is currently only targetting 64-bit platforms including Ubuntu 14.04 on which I'm writing this. Rather than rewrite the task using x64 opcodes, I've used the PicoLisp entry's 'glue code' to enable the 32-bit machine code to run on a 64-bit system.

3. There doesn't appear to be a way to cast a pointer to a native buffer to a Kotlin function pointer so that the machine code can be run. I've therefore written a 'one line' C helper function (in mcode.def) to perform this step and compiled it to a library (mcode.klib) so that it can be called from Kotlin code.

```C
// mcode.def
---

static inline unsigned char runMachineCode(void *code, unsigned char a, unsigned char b) {
    return ((unsigned char (*) (unsigned char, unsigned char))code)(a, b);
}
```



```scala
// Kotlin Native version 0.3

import kotlinx.cinterop.*
import string.*
import mman.*
import mcode.*

fun main(args: Array<String>) {
    memScoped {
        val bytes = byteArrayOf(
            144 - 256,                            // Align
            144 - 256,
            106, 12,                              // Prepare stack
            184 - 256, 7, 0, 0, 0,
            72, 193 - 256, 224 - 256, 32,
            80,
            139 - 256, 68, 36, 4, 3, 68, 36, 8,   // Rosetta task code
            76, 137 - 256, 227 - 256,             // Get result
            137 - 256, 195 - 256,
            72, 193 - 256, 227 - 256, 4,
            128 - 256, 203 - 256, 2,
            72, 131 - 256, 196 - 256, 16,         // Clean up stack
            195 - 256                             // Return
        )
        val len = bytes.size
        val code = allocArray<ByteVar>(len)
        for (i in 0 until len) code[i] = bytes[i]
        val buf = mmap(null, len.toLong(), PROT_READ or PROT_WRITE or PROT_EXEC,
                       MAP_PRIVATE or MAP_ANON, -1, 0)
        memcpy(buf, code, len.toLong())
        val a: Byte = 7
        val b: Byte = 12
        val c = runMachineCode(buf, a, b)
        munmap(buf, len.toLong())
        println("$a + $b = ${if(c >= 0) c.toInt() else c + 256}")
    }
}
```


```txt

7 + 12 = 19

```



## M2000 Interpreter

We can execute machine code, in a buffer for code. We can't push to stack and then call, we can use a buffer for data. If eax is non zero then error raised, with error number the eax number. When execute code the code buffer can't be used to write over. so we have to use a buffer for data for read/write data.
This example perform these: At Datamem(1) put 500, eax=5100, eax add Datamem(1), eax add 5, store eax to Datamem(0). We have an option to clear eax, or use it to return value as error code.
We have to leave all other registers, and stack as we found it.
Both running in Wine (Linux 64bit) too



```M2000 Interpreter

Module Checkit {
      Buffer DataMem as Long*10
      Return DataMem, 1:=500    ' second Long
      Print Eval(DataMem, 1)+5100+5=5605
      \\ Now we do math executing machine code
      Buffer Code ExecMem as byte*1024
      Address=0
      EmbLong(0xb8, 5100) ' mov eax,5100
      EmbByteByte(0x83, 0xC0, 5) ' add  eax,0x5
      EmbByteLong(0x3,0x5, DataMem(1)) ' add eax, [DataMem(1)]
      EmbLong(0xa3, DataMem(0)) ' mov [DataMem(0)], eax
      \\ split rem to execute xor eax eax (eax=0)
      Rem : EmbByte(0x31, 0xC0) ' xor eax, eax
      Ret() ' Return
      \\
      Try ok {
            Execute Code ExecMem, 0
      }
      \\If  Eax <>0 then we get error, so we read error as Uint()
      \\ Error read once then change to zero
      m=Uint(Error)
      \\ Hex is Print Hexadecimal for unsigned numbers
      Hex m
      Print m=5605
      Print Error=0, ok=False

      Print Eval(DataMem, 0)=5605,  Eval(DataMem, 0)
      \\ sub used as Exit here
      Sub Ret()
            Return ExecMem, Address:=0xC3
            Address++
      End Sub
      Sub EmbByteByte()
            Return ExecMem, Address:=Number, Address+1:=Number, Address+2:=Number
            Address+=3
      End Sub
      Sub EmbByte()
            Return ExecMem, Address:=Number, Address+1:=Number
            Address+=2
      End Sub
      Sub EmbLong()
            Return ExecMem, Address:=Number, Address+1:=Number as Long
            Address+=5
      End Sub
      Sub EmbByteLong()
            Return ExecMem, Address:=Number, Address+1:=Number, Address+2:=Number as Long
            Address+=6
      End Sub

}
CheckIt

```


Using a lambda function with closures two buffers (buffers are objects in M2000 to handle memory blocks). This also add 12 +7 as the task want (but with no pushing to stack, but poke to data buffer)


```M2000 Interpreter

Function MyAdd {
      Buffer DataMem as Long*2
      Buffer Code ExecMem as byte*32
      Address=0
      EmbByte(0x31, 0xC0)
      EmbByteLong(0x3,0x5, DataMem(0)) ' add eax, [DataMem(0)]
      EmbByteLong(0x3,0x5, DataMem(1)) ' add eax, [DataMem(1)]
      EmbLong(0xa3, DataMem(0)) ' mov [DataMem(0)], eax
      Rem :
      EmbByte(0x31, 0xC0) ' xor eax, eax
      Ret() ' Return
      =lambda ExecMem, DataMem (a as double, b as double)-> {
            Return DataMem, 0:=a, 1:=b
            Try ok  {
                  Execute Code ExecMem, 0
            }
            If not ok then {
                  =Uint(Error)
            }  Else {
                  =Eval(DataMem, 0)
            }
      }
      Sub Ret()
            Return ExecMem, Address:=0xC3
            Address++
      End Sub
      Sub EmbByte()
            Return ExecMem, Address:=Number, Address+1:=Number
            Address+=2
      End Sub
      Sub EmbLong()
            Return ExecMem, Address:=Number, Address+1:=Number as Long
            Address+=5
      End Sub
      Sub EmbByteLong()
            Return ExecMem, Address:=Number, Address+1:=Number, Address+2:=Number as Long
            Address+=6
      End Sub
}
\\ Produce a lambda function with machine code inside
UnsingedAdd=MyAdd()
Print UnsingedAdd(12, 7), UnsingedAdd(500, 100)

```



## Nim

```nim
import posix

when defined(macosx) or defined(bsd):
  const MAP_ANONYMOUS = 0x1000
elif defined(solaris):
  const MAP_ANONYMOUS = 0x100
else:
  var
    MAP_ANONYMOUS {.importc: "MAP_ANONYMOUS", header: "<sys/mman.h>".}: cint

proc test(a, b: cint): cint =
  # mov EAX, [ESP+4]
  # add EAX, [ESP+8]
  var code = [0x8B'u8, 0x44, 0x24, 0x4, 0x3, 0x44, 0x24, 0x8, 0xC3]

  # create executable buffer
  var buf = mmap(nil, sizeof(code), PROT_READ or PROT_WRITE or PROT_EXEC,
    MAP_PRIVATE or MAP_ANONYMOUS, -1, 0)

  # copy code to buffer
  copyMem(addr buf, addr code[0], sizeof(code))

  # run code
  {.emit: "`result` = ((int (*) (int, int))&`buf`)(`a`,`b`);".}

  # free buffer
  discard munmap(buf, sizeof(code))

echo test(7, 12)
```



## PARI/GP

GP can't peek and poke into memory, but PARI can add in those capabilities via [[#C|C]].
```c
#include <stdio.h>
#include <sys/mman.h>
#include <string.h>
#include <pari/pari.h>

int
test(int a, int b)
{
  char code[] = {0x8B, 0x44, 0x24, 0x4, 0x3, 0x44, 0x24, 0x8, 0xC3};
  void *buf;
  int c;
  /* copy code to executable buffer */
  buf = mmap (0,sizeof(code),PROT_READ|PROT_WRITE|PROT_EXEC,
             MAP_PRIVATE|MAP_ANON,-1,0);

  memcpy (buf, code, sizeof(code));
  /* run code */
  c = ((int (*) (int, int))buf)(a, b);
  /* free buffer */
  munmap (buf, sizeof(code));
  return c;
}

void
init_auto(void)
{
  pari_printf("%d\n", test(7,12));
  return 0;
}
```



## Pascal

Tested under Linux with Freepascal 2.6.4-32BIt ( like the Code used )
cdecl doesn't work in Freepascal under Linux 64-bit

``` pascal
Program Example66;
{Inspired... program to demonstrate the MMap function. Freepascal docs }
Uses
  BaseUnix,Unix;

const
  code : array[0..9] of byte = ($8B, $44, $24, $4, $3, $44, $24, $8, $C3, $00);
  a :longInt= 12;
  b :longInt=  7;
type
  tDummyFunc = function(a,b:LongInt):LongInt;cdecl;
Var
    Len,k  : cint;
    P    : Pointer;

begin
  len := sizeof(code);
  P:= fpmmap(nil,
             len+1 ,
             PROT_READ OR PROT_WRITE OR PROT_EXEC,
             MAP_ANONYMOUS OR MAP_PRIVATE,
             -1, // for MAP_ANONYMOUS
             0);
  If P =  Pointer(-1) then
    Halt(4);

  for k := 0 to len-1 do
    pChar(p)[k] := char(code[k]);

  k := tDummyFunc(P)(a,b);

  Writeln(a,'+',b,' = ',k);
  if fpMUnMap(P,Len)<>0 Then
    Halt(fpgeterrno);
end.
```

;output:

```txt
12+7 = 19
```



## Phix


```Phix
atom mem = allocate(9)
poke(mem,{#8B,#44,#24,#04,#03,#44,#24,#08,#C3})
constant mfunc = define_c_func({},mem,{C_INT,C_INT},C_INT)
?c_func(mfunc,{12,7})
free(mem)
```

In Phix the #ilASM statement (which has guards to allow 32/64/WIN/LNX variants) is usually used for inline assembly, for example (but sticking to the task):

```Phix
atom mem = allocate(9)
poke(mem,{#8B,#44,#24,#04,#03,#44,#24,#08,#C3})
integer res
#ilASM{ mov eax,[mem]
        call :%pLoadMint -- eax:=(int32)eax, in case mem>#3FFFFFFF
        push 12
        push 7
        call eax
        add esp,8
        mov [res],eax }
?res
free(mem)
```

Better yet, albeit deviating somewhat from the task (and this runs on both 32 and 64 bit):

```Phix
integer res
#ilASM{ jmp @f
      ::add
    [32]
        mov eax,[esp+4]
        add eax,[esp+8]
    [64]
        mov rax,[rsp+8]
        add rax,[rsp+16]
    []
        ret
      @@:
        push 12
        push 7
        call :add
    [32]
        add esp,8
        mov [res],eax
    [64]
        add rsp,16
        mov [res],rax
    []
      }
?res
```

All three cases output 19


## PicoLisp

The following runs on 64-bit PicoLisp. Therefore we need some glue code to
interface to the task's 32-bit code.

```PicoLisp
(setq P
   (struct (native "@" "malloc" 'N 39) 'N
      # Align
      144                  # nop
      144                  # nop

      # Prepare stack
      106 12               # pushq $12
      184 7 0 0 0          # mov $7, %eax
      72 193 224 32        # shl $32, %rax
      80                   # pushq %rax

      # Rosetta task code
      139 68 36 4 3 68 36 8

      # Get result
      76 137 227           # mov %r12, %rbx
      137 195              # mov %eax, %ebx
      72 193 227 4         # shl $4, %rbx
      128 203 2            # orb $2, %bl

      # Clean up stack
      72 131 196 16        # add $16, %rsp

      # Return
      195 )                # ret
   foo (>> 4 P) )

# Execute
(println (foo))

# Free memory
(native "@" "free" NIL P)
```

Output:

```txt
19
```



## Python


The ctypes module is meant for calling existing native code from Python, but you can get it to execute your own bytes with some tricks. The bulk of the code is spent establishing an executable memory area - once that's done, the actual execution takes just a few lines.


```Python
import ctypes
import os
from ctypes import c_ubyte, c_int

code = bytes([0x8b, 0x44, 0x24, 0x04, 0x03, 0x44, 0x24, 0x08, 0xc3])

code_size = len(code)
# copy code into an executable buffer
if (os.name == 'posix'):
    import mmap
    executable_map = mmap.mmap(-1, code_size, mmap.MAP_PRIVATE | mmap.MAP_ANON, mmap.PROT_READ | mmap.PROT_WRITE | mmap.PROT_EXEC)
    # we must keep a reference to executable_map until the call, to avoid freeing the mapped memory
    executable_map.write(code)
    # the mmap object won't tell us the actual address of the mapping, but we can fish it out by allocating
    # some ctypes object over its buffer, then asking the address of that
    func_address = ctypes.addressof(c_ubyte.from_buffer(executable_map))
elif (os.name == 'nt'):
    # the mmap module doesn't support protection flags on Windows, so execute VirtualAlloc instead
    code_buffer = ctypes.create_string_buffer(code)
    PAGE_EXECUTE_READWRITE = 0x40  # Windows constants that would usually come from header files
    MEM_COMMIT = 0x1000
    executable_buffer_address = ctypes.windll.kernel32.VirtualAlloc(0, code_size, MEM_COMMIT, PAGE_EXECUTE_READWRITE)
    if (executable_buffer_address == 0):
        print('Warning: Failed to enable code execution, call will likely cause a protection fault.')
        func_address = ctypes.addressof(code_buffer)
    else:
        ctypes.memmove(executable_buffer_address, code_buffer, code_size)
        func_address = executable_buffer_address
else:
    # for other platforms, we just hope DEP isn't enabled
    code_buffer = ctypes.create_string_buffer(code)
    func_address = ctypes.addressof(code_buffer)

prototype = ctypes.CFUNCTYPE(c_int, c_ubyte, c_ubyte) # build a function prototype from return type and argument types
func = prototype(func_address)                        # build an actual function from the prototype by specifying the address
res = func(7,12)
print(res)

```



## PureBasic

<font face="Courier New">Using the Windows API:
</font>


```PureBasic
CompilerIf #PB_Compiler_Processor <> #PB_Processor_x86
  CompilerError "Code requires a 32-bit processor."
CompilerEndIf


; Machine code using the Windows API

Procedure MachineCodeVirtualAlloc(a,b)
*vm = VirtualAlloc_(#Null,?ecode-?scode,#MEM_COMMIT,#PAGE_EXECUTE_READWRITE)
    If(*vm)
        CopyMemory(?scode, *vm, ?ecode-?scode)
        eax_result=CallFunctionFast(*vm,a,b)
        VirtualFree_(*vm,0,#MEM_RELEASE)
        ProcedureReturn eax_result
    EndIf
EndProcedure

rv=MachineCodeVirtualAlloc( 7, 12)
MessageRequester("MachineCodeVirtualAlloc",Str(rv)+Space(50),#PB_MessageRequester_Ok)

#HEAP_CREATE_ENABLE_EXECUTE=$00040000

Procedure MachineCodeHeapCreate(a,b)
hHeap=HeapCreate_(#HEAP_CREATE_ENABLE_EXECUTE,?ecode-?scode,?ecode-?scode)
    If(hHeap)
        CopyMemory(?scode, hHeap, ?ecode-?scode)
        eax_result=CallFunctionFast(hHeap,a,b)
        HeapDestroy_(hHeap)
        ProcedureReturn eax_result
    EndIf
EndProcedure

rv=MachineCodeHeapCreate(7,12)
MessageRequester("MachineCodeHeapCreate",Str(rv)+Space(50),#PB_MessageRequester_Ok)
End

; 8B442404               mov     eax,[esp+4]
; 03442408               add     eax,[esp+8]
; C20800                 ret     8

DataSection
scode:
Data.a $8B,$44,$24,$04,$03,$44,$24,$08,$C2,$08,$00
ecode:
EndDataSection
```



## Racket


```racket
#lang racket/base

(require ffi/unsafe)

; set up access to racket internals
(define scheme-malloc-code
  (get-ffi-obj 'scheme_malloc_code #f (_fun (len : _intptr) -> _pointer)))
(define scheme-free-code
  (get-ffi-obj 'scheme_free_code #f (_fun _pointer -> _void)))

(define opcodes '(139 68 36 4 3 68 36 8 195))

(define code (scheme-malloc-code 64))

(for ([byte opcodes]
      [i (in-naturals)])
  (ptr-set! code _ubyte i byte))

(define function (cast code _pointer (_fun _ubyte _ubyte -> _ubyte)))

(function 7 12)

(scheme-free-code code)
```



## Rust

This is heavily inspired by https://www.jonathanturner.org/2015/12/building-a-simple-jit-in-rust.html<br />
Hence, only working on Linux (the only other way to disable memory execution protection on other OSes was to use other crates, which kind of defeats the purpose.)

```Rust
extern crate libc;

#[cfg(all(
    target_os = "linux",
    any(target_pointer_width = "32", target_pointer_width = "64")
))]
fn main() {
    use std::mem;
    use std::ptr;

    let page_size: usize = 4096;
    let (bytes, size): (Vec<u8>, usize) = if cfg!(target_pointer_width = "32") {
        (
            vec![0x8b, 0x44, 0x24, 0x04, 0x03, 0x44, 0x24, 0x08, 0xc3],
            9,
        )
    } else {
        (vec![0x48, 0x89, 0xf8, 0x48, 0x01, 0xf0, 0xc3], 7)
    };
    let f: fn(u8, u8) -> u8 = unsafe {
        let mut page: *mut libc::c_void = ptr::null_mut();
        libc::posix_memalign(&mut page, page_size, size);
        libc::mprotect(
            page,
            size,
            libc::PROT_EXEC | libc::PROT_READ | libc::PROT_WRITE,
        );
        let contents: *mut u8 = page as *mut u8;
        ptr::copy(bytes.as_ptr(), contents, 9);
        mem::transmute(contents)
    };

    let return_value = f(7, 12);
    println!("Returned value: {}", return_value);
    assert_eq!(return_value, 19);
}

#[cfg(any(
    not(target_os = "linux"),
    not(any(target_pointer_width = "32", target_pointer_width = "64"))
))]
fn main() {
    println!("Not supported on this platform.");
}

```



## Scala

PEEK, POKE and inserting machine opcode makes your system vulnerable which is not quite professional.

Considered to be more harmful than useful.


## Swift


Using 64-bit glue code since Swift has limited 32-bit support on x86.


```swift
import Foundation

typealias TwoIntsOneInt = @convention(c) (Int, Int) -> Int

let code = [
  144, // Align
  144,
  106, 12, // Prepare stack
  184, 7, 0, 0, 0,
  72, 193, 224, 32,
  80,
  139, 68, 36, 4, 3, 68, 36, 8, // Rosetta task code
  76, 137, 227, // Get result
  137, 195,
  72, 193, 227, 4,
  128, 203, 2,
  72, 131, 196, 16, // Clean up stack
  195, // Return
] as [UInt8]

func fudge(x: Int, y: Int) -> Int {
  let buf = mmap(nil, code.count, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_PRIVATE|MAP_ANON, -1, 0)

  memcpy(buf, code, code.count)

  let fun = unsafeBitCast(buf, to: TwoIntsOneInt.self)
  let ret = fun(x, y)

  munmap(buf, code.count)

  return ret
}

print(fudge(x: 7, y: 12))

```



## Tcl

```tcl
package require critcl

critcl::ccode {
    #include <sys/mman.h>
}

# Define a command using C. The C is embedded in Tcl, and will be
# built into a shared library at runtime. Note that Tcl does not
# provide a native way of doing this sort of thing; this thunk is
# mandatory.
critcl::cproc runMachineCode {Tcl_Obj* codeObj int a int b} int {
    int size, result;
    unsigned char *code = Tcl_GetByteArrayFromObj(codeObj, &size);
    void *buf;

    /* copy code to executable buffer */
    buf = mmap(0, (size_t) size, PROT_READ|PROT_WRITE|PROT_EXEC,
            MAP_PRIVATE|MAP_ANON, -1, 0);
    memcpy(buf, code, (size_t) size);
    /* run code */
    result = ((int (*) (int, int)) buf)(a, b);
    /* dispose buffer */
    munmap(buf, (size_t) size);

    return result;
}

# But now we have our thunk, we can execute arbitrary binary blobs
set code [binary format c* {0x8B 0x44 0x24 0x4 0x3 0x44 0x24 0x8 0xC3}]
puts [runMachineCode $code 7 12]
```

Note that it would be more common to put that thunk in its own package (e.g., <code>machineCodeThunk</code>) and then just do something like this:

```tcl
package require machineCodeThunk 1.0

set code [binary format c* {0x8B 0x44 0x24 0x4 0x3 0x44 0x24 0x8 0xC3}]
puts [runMachineCode $code 7 12]
```


