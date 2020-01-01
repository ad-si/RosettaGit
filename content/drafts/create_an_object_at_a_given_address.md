+++
title = "Create an object at a given address"
description = ""
date = 2019-07-27T21:36:49Z
aliases = []
[extra]
id = 4347
[taxonomies]
categories = []
tags = []
+++

{{task|Basic Data Operations}}
{{basic data operation}}
{{omit from|Modula-2}}

In systems programing it is sometimes required to place language objects at specific memory locations, like I/O registers, hardware interrupt vectors etc.


;Task:
Show how language objects can be allocated at a specific machine addresses.

Since most [[OS]]es prohibit access to the physical memory if it is not mapped by the application, as an example, rather than a physical address, take the address of some existing object (using suitable [[Address Operations|address operations]] if necessary).


For example:
::*   create an integer object
::*   print the machine address of the object
::*   take the address of the object and create another integer object at this address
::*   print the value of this object to verify that it is same as one of the origin
::*   change the value of the origin and verify it again




=={{Header|6502 Assembly}}==
In [[6502 Assembly]] memory is represented by either an 8-bit or a 16-bit address (i.e. $0000 - $FFFF). 8-bit address are reserved for the memory from $00 to $FF - known as zero page; access to this memory takes one less byte in the opcode and one less cycle to execute.

Data can be stored, one byte at a time, through the store instructions, for example to store data at $1900:

```6502asm
        sta $1900
        stx $1901
        sty $1902
```


Storage can be indexed through the use of the X or Y registers:

```6502asm
        ldx #54
.loop   sta $1900,X
        dex
        bne loop
```


It can also be stored via indirect indexed addressing (i.e. memory points to an address), using the Y register:

```6502asm
        lda #0
        sta $70
        lda #$20
        sta $71
        ldy #0
        sta ($70),Y
```


Finally, it can be stored via indexed indirect addressing (i.e. read the address of memory from the table stored at the parameter), using the X register:

```6502asm
        lda #0
        sta $70
        lda #$20
        sta $71
        ldx #0
        sta ($70,X)
```


It should be noted that on the 6502 processor hardware is normally memory mapped, so this is often used for manipulating hardware.

=={{Header|Ada}}==
In [[Ada]] object address can be specified using the address representation clause [http://www.adaic.org/standards/05rm/html/RM-13-3.html RM 13.3]:

```ada

type IO_Port is mod 2**8; -- One byte
Device_Port : type IO_Port;
for Device_Port'Address use 16#FFFF_F000#;

```

In the example above the address is specified constant. It is also possible to specify address dynamically as the following solution of the task does:

```ada

with Ada.Text_IO;              use Ada.Text_IO;
with System.Storage_Elements;  use System.Storage_Elements;

procedure Test_Address is
   X : Integer := 123;
   Y : Integer;
   for Y'Address use X'Address;
begin
   Put_Line ("At address:" & Integer_Address'Image (To_Integer (Y'Address)));
   Put_Line (Integer'Image (Y));
   X := 456;
   Put_Line (Integer'Image (Y));
end Test_Address;

```

Sample output:

```txt

At address: 38207236
 123
 456

```

=={{Header|Aikido}}==
Aikido doesn't support getting the address of a variable.  However, in the spirit of this task, it does support raw memory access using <code>peek</code> and <code>poke</code>.  These can be used on both an integer representing an address (64 bit) or a value obtained from calling <code>malloc</code>.

```aikido


var portaddr = 0x80
var v = peek (portaddr, 1)   // 1 byte
v |= 0x40
poke (portaddr, v, 1) // 1 byte back again

var addr = malloc (16)
poke (addr, 1234, 4)
poke (addr+4, 0, 2)
poke (addr+6, 12, 2)


```



## AutoHotkey

In AutoHotkey indeed no language objects can be created at a specified address. But it's very well possible to read and write memory addresses directly. All standard number types are allowed.


```AutoHotkey
; Create a variable with 4 bytes size and show it's machine address.
VarSetCapacity(var, 4, 0)
pAddress := &var
MsgBox Machine address: %pAddress%

; pAddress contains the memory address.
; Write a number and read it back.
NumPut(123456, pAddress+0, 0, "UInt")
MsgBox % "Contents of *pAddress: " . NumGet(pAddress+0, 0, "UInt")
```



## BBC BASIC


```bbcbasic
      REM Create an integer object:
      anInteger% = 12345678
      PRINT "Original value =", anInteger%

      REM Print the machine address of the object:
      address% = ^anInteger%
      PRINT "Hexadecimal address =   ";~address%

      REM Take the address of the object and create
      REM another integer object at this address:
      !address% = 87654321

      REM Print the value of this object to verify
      REM that it is same as one of the origin:
      PRINT "New value =", anInteger%

      REM Change the value and verify it again:
      anInteger% = 55555555
      PRINT "Final value =", !address%

```

Output:

```txt
Original value =      12345678
Hexadecimal address =   B51955
New value =           87654321
Final value =         55555555
```



## C


```c
#include <stdio.h>

int main()
{
  int intspace;
  int *address;

  address = &intspace; // address = 0x100;
  *address = 65535;
  printf("%p: %08x (=%08x)\n", address, *address, intspace);
  // likely we must be worried about endianness, e.g.
  *((char*)address) = 0x00;
  *((char*)address+1) = 0x00;
  *((char*)address+2) = 0xff;
  *((char*)address+3) = 0xff; // if sizeof(int) == 4!
  // which maybe is not the best way of writing 32 bit values...
  printf("%p: %08x (=%08x)\n", address, *address, intspace);
  return 0;
}
```



```txt
0xbfc5675c: 0000ffff (=0000ffff)
0xbfc5675c: ffff0000 (=ffff0000)
```


A more typical embedded way of doing this is below.  Note that the OS will probably not allow this due to memory protections.  Embedded systems often do not have memory managers.

```c
#include <stdint.h>
#include <stddef.h>

// This is a port variable located at address 0x100
#define PORT_A (*(volatile uint32_t*)0x100)

int main()
{
  uint32_t dat;
  size_t addr;

  PORT_A ^= 0x01;   // Toggle bit 0 of PORT_A
  dat = PORT_A;     // Read PORT_A
  addr = &PORT_A;   // addr = 0x100

  return 0;
}
```



## C++

C++ supports this natively through placement new. This allows construction of complex object types in arbitrary memory locations.

```cpp
#include <string>
#include <iostream>

int main()
{
    // Allocate enough memory to hold an instance of std::string
    char* data = new char[sizeof(std::string)];

    // use placement new to construct a std::string in the memory we allocated previously
    std::string* stringPtr = new (data) std::string("ABCD");

    std::cout << *stringPtr << " 0x" << stringPtr << std::endl;

    // use placement new to construct a new string object in the same memory location
    // remember to manually call destructor
    stringPtr->~basic_string();
    stringPtr = new (data) std::string("123456");

    std::cout << *stringPtr << " 0x" << stringPtr << std::endl;

    // clean up
    stringPtr->~basic_string();
    delete[] data;
}
```


Sample output:

```txt
ABCD 0x00204040
123456 0x00204040
```


{{omit from|Clojure}}


## COBOL

{{trans|PicoLisp}}
{{works with|COBOL|2002}}
{{works with|OpenCOBOL|1.1}}

```COBOL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. object-address-test.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01 int-space.
          05 val PICTURE 9(5) VALUE 12345.
       01 addr BASED.
          05 val PICTURE 9(5) VALUE ZERO.
       01 point USAGE POINTER.
       PROCEDURE DIVISION.
         DISPLAY val OF int-space END-DISPLAY
         SET point TO ADDRESS OF int-space
         DISPLAY point END-DISPLAY
         SET ADDRESS OF addr TO point
         DISPLAY val OF addr END-DISPLAY
         MOVE 65535 TO val OF addr
         DISPLAY val OF addr END-DISPLAY
         DISPLAY val OF int-space END-DISPLAY
         STOP RUN.
       END PROGRAM object-address-test.

```


Output:

```txt

12345
3215227472
12345
65535
65535

```



## D


A better presentation.

```d
import std.stdio ;

void main() {
    int[] arr ;
    foreach(i; [0,1,2,3])
        arr ~= i*(1 << 24) + 0x417e7e7e ;

    struct X {
        char[16] msg ;
    }

    X* xPtr ;
    int* iPtr ;
    float* fPtr ;

    int adrSpace = cast(int) arr.ptr ;
    // get address of an existing object arr

    xPtr = cast(X*) adrSpace ;
    // xPtr now point to arr, as a struct X
    writefln("arr(as X)'s msg = '%s' (len %d) @ 0x%08x",
        xPtr.msg, xPtr.msg.length, xPtr) ;

    iPtr = cast(int*) (adrSpace + 1 * 4 /*bytes*/) ;
    fPtr = cast(float*) iPtr ;
    // pointers now point to arr[1]
    writefln("arr[1] = 0x%8x (%9.4f) @ 0x%08X", *iPtr, *fPtr, iPtr) ;
    iPtr = cast(int*) (adrSpace + 3 * 4 /*bytes*/) ;
    fPtr = cast(float*) iPtr ;
    // pointers now point to arr[3]
    writefln("arr[3] = 0x%8x (%9.4f) @ 0x%08X", *iPtr, *fPtr, iPtr) ;
    *fPtr = 0.5f ; // change value
    writefln("arr[3] = 0x%8x (%9.4f) @ 0x%08X", *iPtr, *fPtr, iPtr) ;
}
```

output:

```txt
arr(as X)'s msg = '~~~A~~~B~~~C~~~D' (len 16) @ 0x401C2F80
arr[1] = 0x427e7e7e (  63.6235) @ 0x401C2F84
arr[3] = 0x447e7e7e (1017.9764) @ 0x401C2F8C
arr[3] = 0x3f000000 (   0.5000) @ 0x401C2F8C

```



## Forth

As an untyped language, specific machine addresses are very easy to represent in Forth.  This is usually most useful for embedded targets.

```forth

$3f8 constant LPT1:

LPT1: c@ .
$3f LPT1: c!

```

Some architectures may require special fetch and store operators to access ports. For example, [[Open Firmware]] defines l@ and l! for safe 32-bit port writes.


## FreeBASIC


```freebasic
' FB 1.05.0

Type Person
  As String name
  As Integer age
  Declare Constructor(name As String, age As Integer)
End Type

Constructor Person(name As String, age As Integer)
  This.name = name
  This.age = age
End Constructor

Dim ap As Any Ptr = CAllocate(SizeOf(Person)) ' allocate memory to store a Person object

'create a Person object at the address of the memory we've just allocated

Dim p As Person Ptr = New(ap) Person("Teresa", 60)

'check addresses are same
Print ap, p

'check data is not corrupt
Print p -> name, p -> age

'call implicit destructor
p -> Destructor

'free memory
Deallocate(ap)

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

4790800       4790800
Teresa         60

```



## Go

Go has several ways to access arbitrary memory locations using the built-in unsafe package. If the desired memory contains an array, since Go doesn't have pointer arithmetic, then a slice should be used instead of a pointer. The following solution demonstrates both a pointer and a slice.

```go
package main

import(
	"fmt"
	"unsafe"
	"reflect"
)

func pointer() {
	fmt.Printf("Pointer:\n")

	// Create a *int and store the address of 'i' in it. To create a pointer to
	// an arbitrary memory location, use something like the following:
	//    p := (*int)(unsafe.Pointer(uintptr(0x100)))
	// And replace '0x100' with the desired address.
	var i int
	p := &i

	fmt.Printf("Before:\n\t%v: %v, %v\n", p, *p, i)

	*p = 3

	fmt.Printf("After:\n\t%v: %v, %v\n", p, *p, i)
}

func slice() {
	fmt.Printf("Slice:\n")

	var a [10]byte

	// reflect.SliceHeader is a runtime representation of the internal workings
	// of a slice. To make it point to a specific address, use something like
	// the following:
	//    h.Data = uintptr(0x100)
	// And replace '0x100' with the desired address.
	var h reflect.SliceHeader
	h.Data = uintptr(unsafe.Pointer(&a)) // The address of the first element of the underlying array.
	h.Len = len(a)
	h.Cap = len(a)

	// Create an actual slice from the SliceHeader.
	s := *(*[]byte)(unsafe.Pointer(&h))

	fmt.Printf("Before:\n\ts: %v\n\ta: %v\n", s, a)

	// Copy a string into the slice. This fills the underlying array, which in
	// this case has been manually set to 'a'.
	copy(s, "A string.")

	fmt.Printf("After:\n\ts: %v\n\ta: %v\n", s, a)
}

func main() {
	pointer()
	fmt.Println()

	slice()
}
```

Output:

```txt

Pointer:
Before:
        0xf840026018: 0, 0
After:
        0xf840026018: 3, 3

Slice:
Before:
        s: [0 0 0 0 0 0 0 0 0 0]
        a: [0 0 0 0 0 0 0 0 0 0]
After:
        s: [65 32 115 116 114 105 110 103 46 0]
        a: [65 32 115 116 114 105 110 103 46 0]

```



## Julia

Julia has pointer access functions for interface with C code. Because the
address of a Julia integer variable within the VM may change when it is
re-assigned a new value, an array of a single integer is used below.

```julia

function unsafepointers()
    intspace = [42]
    address = pointer_from_objref(intspace)
    println("The address of intspace is $address")
    anotherint = unsafe_pointer_to_objref(address)
    println("intspace is $(intspace[1]), memory at $address, reference value $(anotherint[1])")
    intspace[1] = 123456
    println("Now, intspace is $(intspace[1]), memory at $address, reference value $(anotherint[1])")
    anotherint[1] = 7890
    println("Now, intspace is $(intspace[1]), memory at $(pointer_from_objref(anotherint)), reference value $(anotherint[1])")
end

unsafepointers()

```

{{output}}
```txt

The address of intspace is Ptr{Void} @0x0000000007271030
intspace is 42, memory at Ptr{Void} @0x0000000007271030, reference value 42
Now, intspace is 123456, memory at Ptr{Void} @0x0000000007271030, reference value 123456
Now, intspace is 7890, memory at Ptr{Void} @0x0000000007271030, reference value 7890

```



## Kotlin

{{Works with|Ubuntu|14.04}}

```scala
// Kotlin/Native Technology Preview

import kotlinx.cinterop.*

fun main(args: Array<String>) {
    val intVar = nativeHeap.alloc<IntVar>().apply { value = 42 }
    with(intVar) { println("Value is $value, address is $rawPtr") }
    intVar.value = 52  // create new value at this address
    with(intVar) { println("Value is $value, address is $rawPtr") }
    nativeHeap.free(intVar)
}
```


{{out}}
Sample output:

```txt

Value is 42, address is 26431776
Value is 52, address is 26431776

```


## M2000 Interpreter

In M2000 we can create two kind of buffers, one for data, and one for code. Buffer for code is immutable at execution time. We can execute code by using an offset. Buffer for data is always mutable, but can't execute code.

There is no assembler x86 for M2000 yet, so we have to write code using a reference book and some subs for help

Memory addresses are nit the physical address, it's from virtual space.

```M2000 Interpreter

Module CheckIt {
      structure  alfa {
            val as long
      }
      Buffer Clear Beta as alfa*2
      Print Beta(0)  ' return address
      Return Beta, 0!val:=500 ' unsigned integer 32 bit
      Print Eval(Beta, 0!val)=500
      Return Beta, 0!val:=0xFFFFFFFF
      Print Eval(Beta, 0!val)=4294967295
      Buffer Code ExecMem as byte*1024
      Offset=0
      EmbLong(0xb8, 5000) ' mov eax,5100
      EmbByteLong(0x3,0x5, Beta(0)) ' add eax, [Beta(0)]
      EmbLong(0xa3, Beta(1)) ' mov [Beta(1)], eax
      EmbByte(0x31, 0xC0) ' xor eax, eax
      Ret() ' Return
      Execute Code ExecMem, 0
      Print eval(Beta, 1!val)=4999
      Sub Ret()
            Return ExecMem, Offset:=0xC3
            Offset++
      End Sub
      Sub EmbByte()
            Return ExecMem, Offset:=Number, Offset+1:=Number
            Offset+=2
      End Sub
      Sub EmbLong()
            Return ExecMem, Offset:=Number, Offset+1:=Number as Long
            Offset+=5
      End Sub
      Sub EmbByteLong()
            Return ExecMem, Offset:=Number, Offset+1:=Number, Offset+2:=Number as Long
            Offset+=6
      End Sub
}
Checkit

```



## Nim



```nim
type
  MyObject = object
    x: int
    y: float

var
  mem = alloc(sizeof(MyObject))
  objPtr = cast[ptr MyObject](mem)
echo "object at ", cast[int](mem), ": ", objPtr[]

objPtr[] = MyObject(x: 42, y: 3.1415)
echo "object at ", cast[int](mem), ": ", objPtr[]

```


Output:

```txt

object at 139966605271112: (x: 0, y: 0.0)
object at 139966605271112: (x: 42, y: 3.1415)

```



## Pascal

Like in Ada you can assigne different variables at the same adress of an already declared variable.
Nice to get the bytes out of an Int64.

```pascal
program test;
type
  t8Byte =  array[0..7] of byte;
var
  I : integer;
  A : integer absolute I;
  K : t8Byte;
  L : Int64 absolute K;
begin
  I := 0;
  A := 255; writeln(I);
  I := 4711;writeln(A);

  For i in t8Byte do
  Begin
    K[i]:=i;
    write(i:3,' ');
  end;
  writeln(#8#32);
  writeln(L);
end.
```
{OUT}
```txt
255
4711
  0  1  2  3  4  5  6  7
506097522914230528
```



## Perl 6

Perl 6 has fairly comprehensive facilities for accessing allocating and accessing memory and also declaring C-style structs, via the NativeCall interface, as this example demonstrates.

```perl6
use v6;
use NativeCall;
use NativeCall::Types;

# bind to basic libc memory management
sub malloc(size_t) returns Pointer[uint8] is native {*};
sub memset(Pointer, uint32, size_t) is native {*};
sub free(Pointer[uint8]) is native {*};

my Pointer[uint8] $base-p = malloc(100);
memset($base-p, 0, 100);

# define object as a C struct that contains a short and an int
class SampleObject is repr('CStruct') {
    has uint16 $.foo is rw;
    has uint8  $.bar is rw;
}

# for arguments sake our object is at byte offset 64 in the
# allocated memory

my $offset-p =  $base-p.clone.add(64);
my $object-p := nativecast(Pointer[SampleObject], $offset-p);
note "creating object at address {+$object-p}";

my $struct := $object-p.deref;

$struct.foo = 41;
$struct.bar = 99;

# check we can update
$struct.foo++; # 42

# Check that we're actually updating the memory
use Test;

# look at the bytes directly to verify we've written to memory. Don't be too exact, as
# the positions may vary on different platforms depending on endianess and field alignment.

my $rec-size = nativesizeof(SampleObject);
my uint8 @bytes-written = (0 ..^ $rec-size).map(-> $i {$base-p[64 + $i]}).grep: * > 0;

# first field 'foo' (amount is small enough to fit in one byte)
is @bytes-written[0], 42, 'object first field';

# second field 'bar'
is @bytes-written[1], 99, 'object second field';

# verify that changing the origin changes the object values
memset($base-p, 1, 100); # set every byte to 1

is $struct.foo, 256 + 1, 'short updated at origin';
is $struct.bar, 1, 'byte updated at origin';

# tidy up
free($base-p);
done-testing;

```

{{out}}

```txt
creating object at address 94299589110352
ok 1 - object first field
ok 2 - object second field
ok 3 - short updated at origin
ok 4 - byte updated at origin
1..4

```



## Phix

Phix does not support creation of a "language object" at a specific address, but you can peek and poke bytes, words, dwords and qwords
to any address, as long as doing so does not trigger a hardware exception. You could also use inline assembly, if that helps any.

```Phix
poke(0x80,or_bits(peek(0x80),0x40))
#ilASM{ mov al,[0x80]
        or al,0x40
        mov [0x80],al}
```



## PicoLisp


```PicoLisp
: (setq IntSpace 12345)          # Integer
-> 12345

: (setq Address (adr 'IntSpace)) # Encoded machine address
-> -2969166782547

: (set (adr Address) 65535)      # Set this address to a new value
-> 65535

: IntSpace                       # Show the new value
-> 65535
```



## PureBasic


```PureBasic
; Allocate a 1Mb memory area work within to avoid conflicts,
; this address could be any number but it may then fail on some systems.
*a=AllocateMemory(1024*1024)

; Write a int wit value "31415" at address +312,
; using pointer '*a' with a displacement.
PokeI(*a+312, 31415)

; Write a float with value Pi at address +316,
; by creating a new pointer '*b' for this address
*b=*a+316
PokeF(*b, #PI)

;Now test it
For i=0 To 1024000 Step 4
  n=PeekI(*a+i)
  If n
    Debug "Int at +"+Str(i)+"  = "+Str(n)
    Debug "Float at +"+Str(i)+"= "+StrF(PeekF(*a+i))
  EndIf
Next

```



## Racket


```racket

#lang racket
(require ffi/unsafe)

(define x #"Foo")
;; Get the address of the `x' object
(printf "The address of `x' is: ~s\n" (cast x _scheme _long))
(define address (cast x _bytes _long))
(printf "The address of the bytestring it holds: ~s\n" address)
(define y (cast address _long _bytes))
(printf "Converting this back to a bytestring: ~s\n" y)
(bytes-set! y 0 71)
(printf "Changed the converted bytestring: ~s\n" y)
(printf "The original one is now: ~s\n" x)
;; But (bytes-set! x 0 71) will throw an error since `x' is immutable,
;; showing that we've really modifed the memory directly in a way that
;; the runtime doesn't like.

;; Also, the above can fail at any moment if a GC happens, since
;; Racket's GC moves objects.  So a proper way to do this is not to
;; start from an existing object, but allocate one outside of the GC's
;; reach, using raw malloc():
(define buf (malloc 4 'raw))
(make-sized-byte-string buf 4)
;; or start with a given address of something like a memory-mapped IO
;; object

```



## Rust

In a real program, most if not all of the contents of main would all be in one `unsafe` block, however in this one each unsafe operation gets its own block to emphasize exactly which actions Rust considers unsafe.

```rust
use std::{mem,ptr};

fn main() {
    let mut data: i32;

    // Rust does not allow us to use uninitialized memory but the STL provides an `unsafe`
    // function to override this protection.
    unsafe {data = mem::uninitialized()}

    // Construct a raw pointer (perfectly safe)
    let address = &mut data as *mut _;

    unsafe {ptr::write(address, 5)}
    println!("{0:p}: {0}", &data);

    unsafe {ptr::write(address, 6)}
    println!("{0:p}: {0}", &data);

}
```



## Scala

As a high-level, type safe Functional Programming language this sort of (low-level) assembler tasks are in the danger zone and therefore not allowed. One of the reasons; a variable at a physical memory address has also a type. A memory location could contain e.g. an integer but for the same ease it could also a character object. This must by guarded by the programming language in order to shield the programmer from errors.

Direct physical memory address is considered harmful. It divides the languages which supports and the other that prohibits this bad practice. Its adorns languages which are you preventing from this evil.

It is rather unprofessional to deliver this kind of error-prone software. And we are wondering why this tasks is made.

## Tcl

As noted in the [[Address Operations]] task, it is highly unusual to work with low-level addresses in Tcl. However it is possible to use Tcl's [[C]] API (specifically <code>[http://www.tcl.tk/man/tcl8.6/TclLib/LinkVar.htm Tcl_LinkVar]</code>) to couple Tcl variables to a particular address:

{{libheader|critcl}}

```tcl
package require critcl

# A command to 'make an integer object' and couple it to a Tcl variable
critcl::cproc linkvar {Tcl_Interp* interp char* var1} int {
    int *intPtr = (int *) ckalloc(sizeof(int));

    *intPtr = 0;
    Tcl_LinkVar(interp, var1, (void *) intPtr, TCL_LINK_INT);
    return (int) intPtr;
}

# A command to couple another Tcl variable to an 'integer object'; UNSAFE!
critcl::cproc linkagain(Tcl_Interp* interp int addr char* var2} void {
    int *intPtr = (int *) addr;

    Tcl_LinkVar(interp, var2, (void *) intPtr, TCL_LINK_INT);
}

# Conventionally, programs that use critcl structure in packages
# This is used to prevent recompilation, especially on systems like Windows
package provide machAddrDemo 1
```

Demonstrating:

```tcl
package require machAddrDemo
set addr [linkvar foo]
puts "var 'foo' at $addr with value $foo"
linkagain $addr bar
puts "var 'bar' at $addr with value $bar"
incr foo
puts "incremented 'foo' so 'bar' is $bar"
```

Example output (your mileage may vary when it comes to addresses):

```txt
var 'foo' at 19363848 with value 0
var 'bar' at 19363848 with value 0
incremented 'foo' so 'bar' is 1
```


{{omit from|AWK}}
{{omit from|Batch File|No objects or memory management}}
{{omit from|bc|No direct access to memory}}
{{omit from|dc|No direct access to memory}}
{{omit from|E}}
{{omit from|Erlang}}
{{omit from|Groovy}}
{{omit from|Haskell}}
{{omit from|Icon}}{{omit from|Unicon}}
{{omit from|Java}}
{{omit from|JavaScript}}
{{omit from|Joy}}
{{Omit From|LabVIEW}}
{{omit from|Logtalk}}
{{omit from|M4}}
{{Omit From|Mathematica}}
{{Omit From|Maxima}}
{{Omit From|MATLAB}}
{{Omit From|NetRexx}}
{{omit from|Oz}}
{{Omit From|NSIS}}
{{omit from|PARI/GP}}
{{omit from|PowerShell}}
{{omit from|Python}}
{{omit from|Ruby}}
{{omit from|Standard ML}}
{{omit from|Swift}}
{{omit from|TI-89 BASIC}}
{{omit from|zkl}}
