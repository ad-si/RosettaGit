+++
title = "Variable size/Set"
description = ""
date = 2019-09-15T15:27:10Z
aliases = []
[extra]
id = 1986
[taxonomies]
categories = ["task", "Type System"]
tags = []
+++

## Task

Demonstrate how to specify the minimum size of a variable or a data type.





## 360 Assembly

The 360 architecture data specifications are:

```360 Assembly

* Binary interger (H,F)
I2       DS     H          half word        2 bytes
I4       DS     F          full word        4 bytes
* Real (floating point) (E,D,L)
X4       DS     E          short            4 bytes
X8       DS     D          double           8 bytes
X16      DS     L          extended        16 bytes
* Packed decimal (P)
P3       DS     PL3                         2 bytes
P7       DS     PL7                         4 bytes
P15      DS     PL15                        8 bytes
* Zoned decimal (Z)
Z8       DS     ZL8                         8 bytes
Z16      DS     ZL16                       16 bytes
* Character (C)
C1       DS     C                           1 byte
C16      DS     CL16                       16 bytes
C256     DS     CL256                     256 bytes
* Bit value (B)
B1       DC     B'10101010'                 1 byte
* Hexadecimal value (X)
X1       DC     X'AA'                       1 byte
* Address value (A)
A4       DC     A(176)                      4 bytes   but only 3 bytes used
*                                                     (24 bits => 16 MB of storage)

```



## Ada


```ada
type Response is (Yes, No); -- Definition of an enumeration type with two values
for Response'Size use 1; -- Setting the size of Response to 1 bit, rather than the default single byte size
```



## AutoHotkey

The documentation explains how the built-in function [http://www.autohotkey.com/docs/commands/VarSetCapacity.htm VarSetCapacity()] may be used to do so.


## BASIC


Numerical values and arrays generally are a fixed size. Strings are dynamically resized according to the data that they hold.

Variable sizes are in chunks relating to the type of data that they contain. There may also be additional bytes of storage in the variable table that do not show in the dimensions. Typically, strings are allocated in single characters (bytes), so C$(12) in the following example is stored as 12 bytes + additional bytes used for the header in the variable table. In some implementations of basic (such as those that support the storage of variable length strings in arrays), additional terminator characters (such as a trailing Ascii NUL) may also be included. In traditional basic, integers are typically 2 bytes each, so A%(10) contains 10 lots of 2 bytes (20 bytes in total) + additional bytes used for header data in the variable table. Floating point values are typically 8 bytes each, so B(10) holds 10 lots of 8 bytes (80 bytes in total) + additional bytes for header in the variable table:


```basic
10 DIM A%(10): REM the array size is 10 integers
20 DIM B(10): REM the array will hold 10 floating point values
30 DIM C$(12): REM a character array of 12 bytes
```



## BBC BASIC

The only way to 'set' the size of a scalar numeric variable is to declare it with the appropriate type suffix:

```bbcbasic
      var& = 1 : REM Variable occupies 8 bits
      var% = 1 : REM Variable occupies 32 bits
      var  = 1 : REM Variable occupies 40 bits
      var# = 1 : REM Variable occupies 64 bits
```

If the task is talking about setting the size of a variable ''at run time'' that is only possible with strings, arrays and structures.


## C

```c
#include <stdint.h>

int_least32_t foo;
```


Here <var>foo</var> is a signed integer with at least 32 bits. [[wp:stdint.h#Minimum-width integer types|stdint.h]] also defines minimum-width types for at least 8, 16, 32, and 64 bits, as well as unsigned integer types.


```c
union u {
  int i;
  long l;
  double d;
  /* ... */
};
```


Here the use of <code>union</code> results in a datatype which is at least as large as the largest type. Unions are sometimes exploited to just meet a minimum size:


```c
union must_be_at_least_512_bytes {
  int interesting_datum;
  char padding[512];
};
```


Here, the application will never access <code>padding</code> nor store anything; the padding is there to make the type large enough to meet some requirement. For instance, so that some third party API function which fills in the object, doesn't write past the end of the memory, when the program is only interested in <code>interesting_datum</code>.


## C++

```Cpp>#include <boost/cstdint.hpp


boost::int_least32_t foo;
```



## D

In D, any variables of static array of zero length has a size of zero. But such data is useless, as no base type element can be accessed.

```d
typedef long[0] zeroLength ;
writefln(zeroLength.sizeof) ; // print 0
```

NOTE: a dynamic array variable's size is always 8 bytes, 4(32-bit) for length and 4 for a reference pointer of the actual storage somewhere in runtime memory.

The proper candidates of minimum size variable are empty structure, 1-byte size data type variable (include <tt>byte, ubyte, char and bool</tt>), and void, they all occupy 1 byte.

```d
byte b ;
ubyte ub ;
char c ;
bool t ;
```

<tt>bool</tt> is logically 1-bit size, but it actually occupy 1 byte.

<tt>void</tt> can't be declared alone, but <tt>void.sizeof</tt> gives 1.

An empty structure is logically zero size, but still occupy 1 byte.

```d
struct Empty { }
writefln(Empty.sizeof) ; // print 1
```



## Erlang

Variables and data type sizes are outside of the programmers control, with one exception: binary data. Here you can say exactly how many bits you want. Default is 8 bits so below the 0 is 8 bits and the 1 is 3 bits.

```txt

15> <<1:11>>.
<<0,1:3>>

```



## ERRE


Numerical values and arrays generally are a fixed size. Strings are dynamically resized according to the data that they hold.

Variable sizes are in chunks relating to the type of data that they contain. There may also be additional bytes of storage in the variable table that do not show in the dimensions. Typically, in ERRE strings are allocated in single characters (bytes), so C$[12] in the following example is stored as 12 bytes + additional bytes used for the header in the variable table. Integers are typically 2 bytes each, so A%[10] contains 10 numbers of 2 bytes (20 bytes in total) + additional bytes used for header data in the variable table. Floating point values are typically 4 bytes each, so B[10] holds 10 numbers of 4 bytes (40 bytes in total) + additional bytes for header in the variable table:

DIM A%[10] ! the array size is 10 integers

DIM B[10]  ! the array will hold 10 floating point values

DIM C$[12] ! a character array of 12 bytes
```


There is also "double" floating point values (8 bytes). Variables of this type use the suffix #.


## Fortran


Since Fortran 90 each intrinsic data type (INTEGER, REAL, COMPLEX, LOGICAL and CHARACTER) has a KIND parameter associated with it that can be used to set the required level of precision. The actual values which these KIND parameters can take are not specified in the standard and are implementation-dependent. In order to select an appropriate KIND value that is portable over different platforms we can use the intrinsic functions SELECTED_REAL_KIND and SELECTED_INT_KIND.

The syntax of these functions are as follows:-

'''selected_real_kind(P, R)''', where P is the required number of significant decimal digits and R is the required decimal exponent range. At least one argument must be present. The return value is the kind type parameter for real values with the given precision and/or range. A value of -1 is returned if P is out of range, a value of -2 is returned if R is out of range and a value of -3 is returned if both P and R are out of range.

'''selected_int_kind(R)''', where R is the required decimal exponent range. The return value is the kind type parameter for integer values n such that -10^R < n < 10^R. A value of -1 is returned if R is out of range.


```fortran
program setsize
implicit none

  integer, parameter :: p1 = 6
  integer, parameter :: p2 = 12
  integer, parameter :: r1 = 30
  integer, parameter :: r2 = 1000
  integer, parameter :: r3 = 2
  integer, parameter :: r4 = 4
  integer, parameter :: r5 = 8
  integer, parameter :: r6 = 16
  integer, parameter :: rprec1 = selected_real_kind(p1, r1)
  integer, parameter :: rprec2 = selected_real_kind(p2, r1)
  integer, parameter :: rprec3 = selected_real_kind(p2, r2)
  integer, parameter :: iprec1 = selected_int_kind(r3)
  integer, parameter :: iprec2 = selected_int_kind(r4)
  integer, parameter :: iprec3 = selected_int_kind(r5)
  integer, parameter :: iprec4 = selected_int_kind(r6)

  real(rprec1)    :: n1
  real(rprec2)    :: n2
  real(rprec3)    :: n3
  integer(iprec1) :: n4
  integer(iprec2) :: n5
  integer(iprec3) :: n6
  integer(iprec4) :: n7
  character(30) :: form

  form = "(a7, i11, i10, i6, i9, i8)"
  write(*, "(a)") "KIND NAME   KIND NUMBER   PRECISION        RANGE "
  write(*, "(a)") "                          min   set     min     set"
  write(*, "(a)") "______________________________________________________"
  write(*, form) "rprec1", kind(n1), p1, precision(n1), r1, range(n1)
  write(*, form) "rprec2", kind(n2), p2, precision(n2), r1, range(n2)
  write(*, form) "rprec3", kind(n3), p2, precision(n3), r2, range(n3)
  write(*,*)
  form = "(a7, i11, i25, i8)"
  write(*, form) "iprec1", kind(n4), r3, range(n4)
  write(*, form) "iprec2", kind(n5), r4, range(n5)
  write(*, form) "iprec3", kind(n6), r5, range(n6)
  write(*, form) "iprec4", kind(n7), r6, range(n7)

end program
```

Output

```txt
KIND NAME   KIND NUMBER   PRECISION        RANGE
                          min   set     min     set
______________________________________________________
 rprec1          1         6     6       30      37
 rprec2          2        12    15       30     307
 rprec3          3        12    18     1000    4931

 iprec1          1                        2       2
 iprec2          2                        4       4
 iprec3          3                        8       9
 iprec4          4                       16      18
```



## FreeBASIC

FreeBASIC variables have a fixed size (depending on their type) with four exceptions:

1. The size of the Integer and UInteger types depends on the underlying platform - 4 bytes for 32-bit and 8 bytes for 64-bit platforms.

2. The size of individual characters of the WString type depends on the operating system - 2 bytes for Windows and 4 bytes for Linux.

3. The length of variable length strings is determined at run time and can be changed.

4. The bounds of dynamic arrays are determined at run time and can be changed (using ReDim). However, the number of dimensions (which can be up to 8) must be specified at compile time and cannot be changed.

Variables of types 3. and 4. don't hold their data directly but instead contain a fixed length descriptor - 24 bytes for a string and between 64 and 232 bytes for an array depending on the number of dimensions. The descriptor contains, amongst other things, a pointer to where the actual data is stored.


## Free Pascal

''See also: [[#Pascal|Pascal]]''

Only enumeration type definitions can have a minimum size:
```pascal
type
	{$packEnum 4}
	enum = (x, y, z);
```

Only a <tt>{$packEnum}</tt> of <tt>1</tt>, <tt>2</tt>, or <tt>4</tt> Bytes can be specified.


## Go

For task interpretation this follows the spirit of the Ada example included by the task author.  In it, an enumeration type is defined from enumeration values, then a storage size--smaller than the default--is specified for the type.  A similar situation exists within Go.  Defining types from values is called duck-typing, and the situation where a type smaller than the default can be specified exists when a variable is duck-typed from a numeric literal.

```go
package main

import (
    "fmt"
    "unsafe"
)

func main() {
    i := 5   // default type is int
    r := '5' // default type is rune (which is int32)
    f := 5.  // default type is float64
    c := 5i  // default type is complex128
    fmt.Println("i:", unsafe.Sizeof(i), "bytes")
    fmt.Println("r:", unsafe.Sizeof(r), "bytes")
    fmt.Println("f:", unsafe.Sizeof(f), "bytes")
    fmt.Println("c:", unsafe.Sizeof(c), "bytes")
    iMin := int8(5)
    rMin := byte('5')
    fMin := float32(5.)
    cMin := complex64(5i)
    fmt.Println("iMin:", unsafe.Sizeof(iMin), "bytes")
    fmt.Println("rMin:", unsafe.Sizeof(rMin), "bytes")
    fmt.Println("fMin:", unsafe.Sizeof(fMin), "bytes")
    fmt.Println("cMin:", unsafe.Sizeof(cMin), "bytes")
}
```

Output:

```txt

i: 4 bytes
r: 4 bytes
f: 8 bytes
c: 16 bytes
iMin: 1 bytes
rMin: 1 bytes
fMin: 4 bytes
cMin: 8 bytes

```



## Haskell


```Haskell

import Data.Int
import Foreign.Storable

task name value = putStrLn $ name ++ ": " ++ show (sizeOf value) ++ " byte(s)"

main = do
  let i8  = 0::Int8
  let i16 = 0::Int16
  let i32 = 0::Int32
  let i64 = 0::Int64
  let int = 0::Int
  task "Int8" i8
  task "Int16" i16
  task "Int32" i32
  task "Int64" i64
  task "Int" int

```

```txt

Int8: 1 byte(s)
Int16: 2 byte(s)
Int32: 4 byte(s)
Int64: 8 byte(s)
Int: 8 byte(s)

```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon values are self-descriptive types subject to automatic garbage collection.  As a result the opportunities for setting the sizes of the variables are limited.
* strings are always variable in length with some fixed overhead
* csets are a fixed size
* tables and sets are variable in size and start empty
* integers and reals are fixed sizes
* records are a fized size
* co-expressions vary in size based on the environment when they are created
* file, window, and procedure references are all fixed in size
* lists can be specified with a minimum size (see below):


```Icon
   L := list(10) # 10 element list
```



## J



```J
v=: ''
```


Here, v is specified to have a minimum size.  In this case, the minimum size of the content is zero, though the size of the representation is somewhat larger.


## Julia


```julia
types = [Bool, Char, Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64]

for t in types
    println("For type ", lpad(t,6), " size is $(sizeof(t)) 8-bit bytes, or ",
        lpad(string(8*sizeof(t)), 2), " bits.")
end

primitive type MyInt24 24 end

println("\nFor the 24-bit user defined type MyInt24, size is ", sizeof(MyInt24), " bytes.")

```
 {{output}}
```txt

For type   Bool size is 1 8-bit bytes, or  8 bits.
For type   Char size is 4 8-bit bytes, or 32 bits.
For type   Int8 size is 1 8-bit bytes, or  8 bits.
For type  UInt8 size is 1 8-bit bytes, or  8 bits.
For type  Int16 size is 2 8-bit bytes, or 16 bits.
For type UInt16 size is 2 8-bit bytes, or 16 bits.
For type  Int32 size is 4 8-bit bytes, or 32 bits.
For type UInt32 size is 4 8-bit bytes, or 32 bits.
For type  Int64 size is 8 8-bit bytes, or 64 bits.
For type UInt64 size is 8 8-bit bytes, or 64 bits.

For the 24-bit user defined type MyInt24, size is 3 bytes.

```



## Kotlin

In Kotlin (or any other language targetting the JVM) the size of variables is outside the programmer's control. The primitive types are either fixed in size or (in the case of Boolean) implementation dependent and the size of objects will depend not only on the aggregate size of their fields but also on any overhead or alignment padding needed.

If one wants a numeric type to be able to accomodate a certain size of number, then one can of course declare a variable of the appropriate type (up to 8 bytes) or use the BigInteger or BigDecimal types where more than 8 byte precision is required.

The following program shows the range of numbers which the primitive numeric types can accomodate to enable one to choose the appropriate type:

```scala
// version 1.0.6

fun main(args: Array<String>) {
   /* ranges for variables of the primitive numeric types */
   println("A  Byte   variable has a range of :  ${Byte.MIN_VALUE} to ${Byte.MAX_VALUE}")
   println("A  Short  variable has a range of :  ${Short.MIN_VALUE} to ${Short.MAX_VALUE}")
   println("An Int    variable has a range of :  ${Int.MIN_VALUE} to ${Int.MAX_VALUE}")
   println("A  Long   variable has a range of :  ${Long.MIN_VALUE} to ${Long.MAX_VALUE}")
   println("A  Float  variable has a range of :  ${Float.MIN_VALUE} to ${Float.MAX_VALUE}")
   println("A  Double variable has a range of :  ${Double.MIN_VALUE} to ${Double.MAX_VALUE}")
}
```


```txt

A  Byte   variable has a range of :  -128 to 127
A  Short  variable has a range of :  -32768 to 32767
An Int    variable has a range of :  -2147483648 to 2147483647
A  Long   variable has a range of :  -9223372036854775808 to 9223372036854775807
A  Float  variable has a range of :  1.4E-45 to 3.4028235E38
A  Double variable has a range of :  4.9E-324 to 1.7976931348623157E308

```



## Mathematica

Mathematica stores variables in symbols : e.g. variable 'A' containing integer 0 requires 24 bytes under Windows.

=={{header|Modula-3}}==

```modula3
TYPE UByte = BITS 8 FOR [0..255];
```

Note that this only works for records, arrays, and objects. Also note that the size in bits must be large enough to hold the entire range (in this case, 8 bits is the correct amount for the range 0 to 255) or the compiler will error.


## Nim


```nim
var a: int8 = 0
var b: int16 = 1
var c: int32 = 10
var d: int64 = 100
```



## ooRexx

ooRexx variables are all references to object instances, so the variables themselves have no settable or gettable size.


## PARI/GP


```parigp
default(precision, 1000)
```

Alternately, in the gp interpreter,

```parigp>\p 1000</lang



## Pascal

Pascal discourages the programmer to think about specific internal memory structures.
Therefore, there is no way to specify the size of any data type.

The GPC (GNU Pascal compiler), however, allows for ''integer'' types the specification of a minimum precision:

```pascal
type
	correctInteger = integer attribute (size = 42);
```

''See also: [[#Free Pascal|Free Pascal]]''


## Perl

I suppose you could use vec() or similar to twiddle a single bit. The thing is, as soon as you store this in a variable, the SV (the underlying C implementation of the most simple data type) already takes a couple dozen of bytes.

In Perl, memory is readily and happily traded for expressiveness and ease of use.


## Perl 6

In Perl 6, normal user-facing types (Int, Rat, Str, Array, Hash) are all auto-sizing, so there is no need to specify a minimum size for them.  (Floating point, known as "Num", defaults to a machine double.)  For storage declarations, native storage types (starting with a lowercase letter) may also be specified, in which case the required bit size is part of the type name: int16, uint8 (aka "byte"), num32 (a "float"), complex64 (made of two num64's), etc.  More generally, such types are created through an API supporting representational polymorphism, in this case, the NativeHOW representation, when provides methods to set the size of a type; the actual allocation calculation happens when such generic types are composed into a class instance representing the semantics of the effective type to the compiler and run-time system.  But mostly this is not something users will concern themselves with directly.

By spec, arrays may be declared with dimensions of fixed size, but as of this writing, such arrays not yet implemented.  An array of fixed size that returns elements of a native type will be stored compactly, and uses exactly the memory you'd think it should, (modulo alignment constraints between elements and any slop at the end due to your memory allocator).


## Phix

Phix native numeric types are fixed size:

on 32 bit integers are 4 bytes and floats 8 bytes,

on 64 bit integers are 8 bytes and floats 10 bytes.

Note that native integers are always signed and one bit shy of a full machine word, ie

-1,073,741,824 to +1,073,741,823 (-#40000000 to #3FFFFFFF) on 32 bit, and

-4,611,686,018,427,387,904 to +4,611,686,018,427,387,903 (-#4000000000000000 to #3FFFFFFFFFFFFFFF) on 64 bit.

Sequences are 4 or 8 bytes per element, and can grow or shrink at will.

Strings are always one byte per character, ie ansi or utf-8, utf-16 and utf-32 are held as sequences.

When using mprf.e (aka gmp), variables can have any precision required, up to available memory.

mpz (integer) variables automatically grow as needed but can optionally be initialised with a minimum bitcount to avoid later reallocations.

mpfr (floating point) variables require the precision to be explicitly specified in binary bits, for example if you want PI to 1000 decimal places:
```Phix
include mpfr.e                              -- requires 0.8.1+
mpfr pi = mpfr_init(0,-1001) -- 1000 dp, +1 for the "3."
mpfr_const_pi(pi)
mpfr_printf(1,"PI with 1000 decimals: %.1000RDf\n\n",pi)
```



## PL/I


```pli

declare i fixed binary (7),      /* occupies  1 byte  */
        j fixed binary (15),     /* occupies  2 bytes */
        k fixed binary (31),     /* occupies  4 bytes */
        l fixed binary (63);     /* occupies  8 bytes */

declare d fixed decimal (1),     /* occupies  1 byte  */
        e fixed decimal (3),     /* occupies  2 bytes */
                                 /* an so on ...      */
        f fixed decimal (15);    /* occupies  8 bytes */

declare b(16) bit (1) unaligned; /* occupies  2 bytes */
declare c(16) bit (1) aligned;   /* occupies 16 bytes */

declare x float decimal (6),     /* occupies  4 bytes */
        y float decimal (16),    /* occupies  8 bytes */
        z float decimal (33);    /* occupies 16 bytes */

```



## PicoLisp

In PicoLisp, all variables have the same size (a single cell). But it is
possible to create a data structure of a given minimal size with the
'[http://software-lab.de/doc/refN.html#need need]' function.


## PureBasic


```PureBasic

EnableExplicit

Structure AllTypes
  b.b
  a.a
  w.w
  u.u
  c.c    ; character type : 1 byte on x86, 2 bytes on x64
  l.l
  i.i    ; integer type : 4 bytes on x86, 8 bytes on x64
  q.q
  f.f
  d.d
  s.s    ; pointer to string on heap : pointer size same as integer
  z.s{2} ; fixed length string of 2 characters, stored inline
EndStructure

If OpenConsole()
  Define at.AllTypes
  PrintN("Size of types in bytes (x64)")
  PrintN("")
  PrintN("byte      = " + SizeOf(at\b))
  PrintN("ascii     = " + SizeOf(at\a))
  PrintN("word      = " + SizeOf(at\w))
  PrintN("unicode   = " + SizeOf(at\u))
  PrintN("character = " + SizeOf(at\c))
  PrintN("long      = " + SizeOf(at\l))
  PrintN("integer   = " + SizeOf(at\i))
  PrintN("quod      = " + SizeOf(at\q))
  PrintN("float     = " + SizeOf(at\f))
  PrintN("double    = " + SizeOf(at\d))
  PrintN("string    = " + SizeOf(at\s))
  PrintN("string{2} = " + SizeOf(at\z))
  PrintN("---------------")
  PrintN("AllTypes  = " + SizeOf(at))
  PrintN("")
  PrintN("Press any key to close the console")
  Repeat: Delay(10) : Until Inkey() <> ""
  CloseConsole()
EndIf


```


```txt

Size of types in bytes (x64)

byte      = 1
ascii     = 1
word      = 2
unicode   = 2
character = 2
long      = 4
integer   = 8
quod      = 8
float     = 4
double    = 8
string    = 8
string{2} = 4
---------------
AllTypes  = 52

```



## Python

For compatibility with the calling conventions of external C functions, the [http://docs.python.org/library/ctypes.html?highlight=ctypes#module-ctypes ctypes module] has functions that map data types and sizes between Python and C:
<table class="docutils" border="1">
<tr>
  <th class="head">ctypes type</th>
  <th class="head">C type</th>
  <th class="head">Python type</th>
</tr>
  <tr>
    <td>c_char</td>
    <td>char</td>
    <td>1-character string</td>
  </tr>
  <tr>
    <td>c_wchar</td>
    <td>wchar_t</td>
    <td>1-character unicode string</td>
  </tr>
  <tr>
    <td>c_byte</td>
    <td>char</td>
    <td>int/long</td>
  </tr>
  <tr>
    <td>c_ubyte</td>
    <td>unsigned char</td>
    <td>int/long</td>
  </tr>
  <tr>
    <td>c_short</td>
    <td>short</td>
    <td>int/long</td>
  </tr>
  <tr>
    <td>c_ushort</td>
    <td>unsigned short</td>
    <td>int/long</td>
  </tr>
  <tr>
    <td>c_int</td>
    <td>int</td>
    <td>int/long</td>
  </tr>
  <tr>
    <td>c_uint</td>
    <td>unsigned int</td>
    <td>int/long</td>
  </tr>
  <tr>
    <td>c_long</td>
    <td>long</td>
    <td>int/long</td>
  </tr>
  <tr>
    <td>c_ulong</td>
    <td>unsigned long</td>
    <td>int/long</td>
  </tr>
  <tr>
    <td>c_longlong</td>
    <td>__int64 or long long</td>
    <td>int/long</td>
  </tr>
  <tr>
    <td>c_ulonglong</td>
    <td>unsigned __int64 or unsigned long long</td>
  <td>int/long</td>
</tr>
<tr>
  <td>c_float</td>
  <td>float</td>
  <td>float</td>
</tr>
<tr>
  <td>c_double</td>
  <td>double</td>
  <td>float</td>
</tr>
<tr>
  <td>c_longdouble</td>
  <td>long double</td>
  <td>float</td>
</tr>
<tr>
  <td>c_char_p</td>
  <td>char * (NUL terminated)</td>
  <td>string or None</td>
</tr>
<tr>
  <td>c_wchar_p</td>
  <td>wchar_t * (NUL terminated)</td>
  <td>unicode or None</td>
</tr>
<tr>
  <td>c_void_p</td>
  <td>void *</td>
  <td>int/long or None</td>
</tr>
</table>


## Racket

Like many other highlevel languages, Racket doesn't have direct control on object sizes.  More than that, objects are almost always references, so holding a vector or a list still starts from some object with pointers to the rest.  It is possible, however, to create random ffi structs with some given length, by using something like <tt>(_array _byte N)</tt> and it's possible to add that to some other ffi type by wrapping it with such an array in a struct.  But to create and manage chunks of memory, it's much better to just use <tt>malloc</tt> (which is also available via the ffi).


## REXX

In REXX, there are no minimums for variables holding character literals, so you just simply assign (set)

character strings (or numbers) to REXX variables.

Note that REXX stores all the values of variables as characters, and that includes numbers (all kinds),

booleans (logical), and labels (including subroutine/function names).

However, to insure that REXX can store numbers with a minimum size (amount of decimal digits),

the     '''NUMERIC DIGITS nnn'''     REXX instruction can be used.   This will ensure that the decimal

number can be stored without resorting to exponential notation   (although exponential notation

can be forced via the   '''format'''   BIF  ('''B'''uilt '''I'''n '''F'''unction).


The default for   ''numeric digits''   is   '''9'''   (decimal) digits.


There's effectively no limit for the precision [or length] for REXX numbers (except for memory),

but eight million is probably the practical limit.

```rexx
/*REXX program demonstrates on setting a variable (using a "minimum var size".*/
numeric digits 100                     /*default: 9 (decimal digs) for numbers*/

/*──         1         2         3         4         5         6         7──*/
/*──1234567890123456789012345678901234567890123456789012345678901234567890──*/

z = 12345678901111111112222222222333333333344444444445555555555.66
n =-12345678901111111112222222222333333333344444444445555555555.66

                                       /* [↑]  these #'s are stored as coded. */
                                       /*stick a fork in it,  we're all done. */
```



## Scala


```Scala
/* Ranges for variables of the primitive numeric types */
println(s"A  Byte   variable has a range of :  ${Byte.MinValue} to ${Byte.MaxValue}")
println(s"A  Short  variable has a range of :  ${Short.MinValue} to ${Short.MaxValue}")
println(s"An Int    variable has a range of :  ${Int.MinValue} to ${Int.MaxValue}")
println(s"A  Long   variable has a range of :  ${Long.MinValue} to ${Long.MaxValue}")
println(s"A  Float  variable has a range of :  ${Float.MinValue} to ${Float.MaxValue}")
println(s"A  Double variable has a range of :  ${Double.MinValue} to ${Double.MaxValue}")
```


See it running in your browser by [https://scastie.scala-lang.org/dX0sTLz5Q1ShT8mLL0cv1g Scastie (JVM)].


## Tcl

In Tcl, most values are (Unicode) strings. Their size is measured in characters, and the minimum size of a string is of course 0.
However, one can arrange, via write traces, that the value of a variable is reformatted to bigger size. Examples, from an interactive [[tclsh]] session:

```Tcl
% proc format_trace {fmt _var el op} {upvar 1 $_var v; set v [format $fmt $v]}

% trace var foo w {format_trace %10s}
% puts "/[set foo bar]/"
/       bar/

% trace var grill w {format_trace %-10s}
% puts "/[set grill bar]/"
/bar       /
```
..or limit its size to a certain length:

```Tcl
% proc range_trace {n _var el op} {upvar 1 $_var v; set v [string range $v 0 [incr n -1]]}

% trace var baz w {range_trace 2}
% set baz Frankfurt
Fr
```



## TXR


This task has many possible interpretations in many contexts.

For instance, there is a buffer type. When we create a buffer, we specify its length. Optionally, we can also specify how much storage is actually allocated. This will prevent re-allocations if the length is increased within that limit.

Here, the buffer holds eight zero bytes, but 4096 bytes is allocated to it:


```txrlisp
(make-buf 8 0 4096)
```


Another situation, in the context of FFI, is that some structure needs to achieve some size, but we don't care about all of its members. We can add anonymous padding to ensure that it meets the minimum size. For instance, suppose we want to call <code>uname</code>, and we only care about retrieving the <code>sysname</code>:


```txt
1> (with-dyn-lib nil
     (deffi uname "uname" int ((ptr-out (struct utsname
                                          (sysname (zarray 65 char))
                                          (nil (array 512 uint)))))))
** warning: (expr-1:2) defun: redefining uname, which is a built-in defun
#:lib-0172
2> (defvar u (new utsname))
u
3> (uname u)
0
4> u
#S(utsname sysname "Linux" nodename nil release nil version nil machine nil
           domainname nil)
```


We have specified a FFI definition for <code>utsname</code> which lays down the <code>sysname</code> member to the correct system-specific array size, and then a generous amount of padding: 512 unsigned integers.

Anonymous padding can be specified anywhere in a FFI structure by using the slot name <code>nil</code>. The corresponding space will be reserved in the structure using the type of that slot, but the slot will not participate in any data conversions. FFI will not fill in that area of the structure when preparing data, and will not extract anything from that area in the reverse direction.

The padding prevents the <code>uname</code> function from accessing beyond the end of the memory that is passed to it.

We can, of course, determine the exact size of <code>struct utsname</code> we can specify the padding such that we know for certain that it meets or exceeds the requirement.


## Ursala

There is no way to set the minimum size of natural, integer, or
rational numbers, but no need because they all have unlimited
precision.

For ([http://www.mpfr.org mpfr] format) arbitrary precision floating point numbers, there are several
mechanisms for setting the minimum precision, although not the exact amount of real memory used.
* If it's initialized from a literal constant, the compiler infers the intended precision from the number of digits in the constant (or 160 bits, whichever is greater).
* The library function <code>mpfr..grow(x,n)</code> returns a copy of <code>x</code> with its precision increased by <code>n</code> bits (padded with zeros).
* The library function <code>mpfr..shrink(x,n)</code> returns a copy of <code>x</code> with its precision reduced by <code>n</code> bits, or to <code>MPFR_PREC_MIN</code>, whichever is greater.
* Library functions such as <code>mpfr..pi</code> and <code>mpfr..const_catalan</code> take a natural number specifying the precision as an argument and return a constant with at least that precision.
* If two numbers of unequal precision are combined using any binary operation from the mpfr library, the result is computed and allocated using the greater precision of the two.
The last feature eliminates the need for explicitly setting the precision of numbers having exact
representations, albeit contrary to the convention in physical sciences.

```Ursala
p = mpfr..pi 200               # 200 bits of precision

x = mpfr..grow(1.0E+0,1000)    # 160 default precision, grown to 1160

y = mpfr..shrink(1.0+0,40)     # 160 default shrunk to 120

z = mpfr..add(p,y)             # inherits 200 bits of precision

a = # 180 bits (not the default 160) because of more digits in the constant

1.00000000000000000000000000000000000000000000000000000E0
```



## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations
string 0;               \use zero-terminated strings
char S,
     A(1),              \sets up an array containing one byte
     B(0);              \sets up an array containing no bytes
int  I;
[S:= "";                \a zero-length (null) string
A:= Reserve(1);         \sets up a 1-byte array at runtime
B:= Reserve(0);         \sets up a 0-byte array at runtime
I:= I ! 1<<3;           \stores a single 1 bit into an integer
I:= I & ~(1<<29);       \stores a 0 bit into bit 29 of the integer
IntOut(0, I>>3 & 1);    \displays value of bit 3
]
```


Other than arrays and strings, variables are a fixed size. Integers are
four bytes and reals are eight bytes.


## zkl

It is up to the object to decide on size. For example, Ints and Floats are 8 bytes, Strings are immutable and are sized when created. Mutable lists and dictionaries grow and shrink as needed. Some mutable types (such as Lists and Dictionaries) can take [programmer supplied] hints as to how big they might become.


## ZX Spectrum Basic



```basic
10 DIM a$(10): REM This array will be 10 characters long
20 DIM b(10): REM this will hold a set of numbers. The fixed number of bytes per number is implementation specific
30 LET c=5: REM this is a single numerical value of fixed size
```


{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have user-defined data structures. -->
