+++
title = "Variable size/Get"
description = ""
date = 2019-09-07T23:42:56Z
aliases = []
[extra]
id = 1987
[taxonomies]
categories = ["task", "Type System"]
tags = []
languages = [
  "actionscript",
  "ada",
  "algol_68",
  "autohotkey",
  "babel",
  "basic",
  "bbc_basic",
  "c",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "elixir",
  "erlang",
  "forth",
  "fortran",
  "free_pascal",
  "freebasic",
  "gambas",
  "go",
  "haskell",
  "idl",
  "j",
  "julia",
  "kotlin",
  "lasso",
  "mathematica",
  "nim",
  "ocaml",
  "oorexx",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "pop11",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "swift",
  "tcl",
  "toka",
  "tuscript",
  "txr",
  "unix_shell",
  "ursala",
  "vala",
  "xpl0",
  "zkl",
]
+++

## Task

Demonstrate how to get the size of a variable.

See also: [[Host introspection]]


## ActionScript

Requires the debug Flash Player 9.0.115.0 or higher.

```ActionScript

package  {

    import flash.display.Sprite;
    import flash.events.Event;
    import flash.sampler.getSize;

    public class VariableSizeGet extends Sprite {

        public function VariableSizeGet() {
            if ( stage ) _init();
            else addEventListener(Event.ADDED_TO_STAGE, _init);
        }

        private function _init(e:Event = null):void {

            var i:int = 1;
            var n:Number = 0.5;
            var s:String = "abc";
            var b:Boolean = true;
            var date:Date = new Date();

            trace("An int contains " + getSize(i) + " bytes.");  // 4
            trace("A Number contains " + getSize(n) + " bytes.");  // 8
            trace("The string 'abc' contains " + getSize(s) + " bytes.");  // 24
            trace("A Boolean contains " + getSize(b) + " bytes.");  // 4
            trace("A Date object contains " + getSize(date) + " bytes.");  // 48

        }

    }

}

```



## Ada

Ada represents the size of a variable in bits, not bytes like many other languages.

```ada
Int_Bits : constant Integer := Integer'size;
Whole_Bytes : constant Integer := Int_Bits / Storage_Unit; -- Storage_Unit is the number of bits per storage element
```



## ALGOL 68

This kind of information is not directly available to the coder.  On some implementations this can be manually inferred by using the constant ''bytes width'' from the ''prelude''.  Alternatively the ''binary transput'' can be used to shunt data to a '''file''', and from there the size of resulting '''file''' might be calculated.  Note that Algol68 has ''tagged'' '''union'''s, and control structures for arrays, thus the size of these can only be estimated by the coder.  Similarly the size of any '''struct''' may internally be subject to byte and word alignment.

Not withstanding the above, with [[ALGOL 68G]] Revision 1.18 the size of '''int''' is not equal to the size of '''bytes'''.

Also note that the size of a '''byte''' can vary from one [[wp:CPU|CPU]] to another, c.f. [[Host_introspection#ALGOL_68]] for additional details.

```algol68
INT i; BYTES b; # typically INT and BYTES are the same size #
STRING s:="DCLXVI", [666]CHAR c;
print((
  "sizeof INT i =",bytes width, new line,
  "UPB STRING s =",UPB s, new line,
  "UPB []CHAR c =",UPB c, new line
))
```

```txt

sizeof INT i =        +32
UPB STRING s =         +6
UPB []CHAR c =       +666

```



## AutoHotkey


```AutoHotkey
VarSetCapacity(Var, 10240000)  ; allocate 10 megabytes
MsgBox % size := VarSetCapacity(Var)  ; 10240000
```



## Babel

In Babel, you can get the raw size with the mu operator and you can also
break this down into its components:

```babel
main:
    { (1 2 (3 4) 5 6)
    dup mu  disp
    dup nva disp
    dup npt disp
    dup nlf disp
    dup nin disp
    dup nhref disp
    dup nhword disp }

disp! : { %d cr << }

```

```txt

38
6
14
6
7
1
4

```


Note that 38 = 6 + 14 + 6 + 7 + 1 + 4

* mu - Memory Usage
* nva - Number of values (each numeric constant is a value)
* npt - Number of pointers (each cons has two pointers)
* nlf - Number of leaf-arrays (same as the number of values)
* nin - Number of interior arrays (the number of conses)
* nhref - Number of hash-references (nil is a hash-reference)
* nhword - Number of words devoted to storing hash-references (each hash-reference takes up 4 words)

Sizes are relative to the machine-word size.

You can also take the size of an object by first serializing it:


```babel
main : { (1 2 (3 4) 5 6) unload size %d << }

```

 38


## BASIC


```gwbasic
10 REM this only works with strings
20 PRINT LEN(variable$)
```


Many BASICs, especially those compatible with [[QBasic]],
can use <code>LEN</code> to find the size of any variable:

```qbasic
DIM a AS INTEGER, b AS LONG, c AS SINGLE, d AS DOUBLE, e AS STRING
PRINT LEN(a), LEN(b), LEN(c), LEN(d), LEN(e)
```


 2             4             4             8             0

Note that when used with a string, <code>LEN</code> reports the length of the string, not its size in memory.
BASIC typically stores information about the string separately from the string itself, usually immediately before the string itself in memory (but some implementations may store such information elsewhere).


## BBC BASIC

A variable's size is implied by its type suffix.  The easiest way to determine the size is to use a structure:

```bbcbasic
      DIM bstruct{b&}
      DIM istruct{i%}
      DIM fstruct{f}
      DIM dstruct{d#}
      DIM sstruct{s$}

      PRINT "Size of b& is ";DIM(bstruct{})
      PRINT "Size of i% is ";DIM(istruct{})
      PRINT "Size of f  is ";DIM(fstruct{})
      PRINT "Size of d# is ";DIM(dstruct{})
      PRINT "Size of s$ is ";DIM(sstruct{})
```

Here the size given for the string s$ is for its descriptor, not its contents.

```txt

Size of b& is 1
Size of i% is 4
Size of f  is 5
Size of d# is 8
Size of s$ is 6

```



## C


```c
printf("An int contains %u bytes.\n", sizeof(int));
```


## C#

```c#

class Program
{
    static void Main(string[] args)
    {
        int i = sizeof(int);
        Console.WriteLine(i);
        Console.ReadLine();
    }
}

```



## C++

Store the size of an int in bytes:


```cpp
#include <cstdlib>
std::size_t intsize = sizeof(int);
```


Note: sizeof can be used without the header <cstdlib>; the latter is only needed for the type std::size_t, which is an alias for whatever type is used to store sizes for the given compiler.

Output the number of bits of an int:

```cpp
#include <climits>
#include <cstdlib>

std::size_t intbits = CHAR_BITS*sizeof(int);
```


Note: the type char is always 1 byte (which, however, need not be 8 bits).

Get the size of a variable in bytes:


```cpp
#include <cstdlib>
int a = 1;
std::size_t a_size = sizeof a;
```


Note: Parentheses are needed around types, but not around variables.

Get the size of an expression's type:


```cpp
#include <cstdlib>
std::size_t size = sizeof (3*6 + 7.5);
```



## COBOL

COBOL is by and large a fixed length system, most data types have a size
determined by standard.

Some native architecture sizing can be controlled by configuration or will be
bound by implementation constraint.

Group items are mostly determined by the data definition, with some variance
due to binary sizing and programmer controlled OCCURS DEPENDING ON run time
values.

There are run-time functions for LENGTH, BYTE-LENGTH, and a common extension
for a LENGTH OF phrase that works at both compile time if possible and at run
time when compile time sizing is not determinable.

In COBOL a BINARY-LONG is 32 bits, by definition, and does not change from platform to platform.
Same for BINARY-CHAR (1), BINARY-SHORT (2), BINARY-DOUBLE (8), by spec.

POINTER data is one of the few platform dependent fields, and most compilers
provide some way of determining this at compile time, as with the Compiler
Directing Facility <code>>>IF P64 IS SET</code>, shown here.
```COBOL

       identification division.
       program-id. variable-size-get.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       data division.
       working-storage section.
       01 bc-len           constant as length of binary-char.
       01 fd-34-len        constant as length of float-decimal-34.

       77 fixed-character  pic x(13).
       77 fixed-national   pic n(13).
       77 fixed-nine       pic s9(5).
       77 fixed-separate   pic s9(5) sign trailing separate.
       77 computable-field pic s9(5) usage computational-5.
       77 formatted-field  pic +z(4),9.

       77 binary-field     usage binary-double.
       01 pointer-item     usage pointer.

       01 group-item.
          05 first-inner   pic x occurs 0 to 3 times depending on odo.
          05 second-inner  pic x occurs 0 to 5 times depending on odo-2.
       01 odo              usage index value 2.
       01 odo-2            usage index value 4.

       procedure division.
       sample-main.
       display "Size of:"
       display "BINARY-CHAR             : " bc-len
       display "  bc-len constant       : " byte-length(bc-len)
       display "FLOAT-DECIMAL-34        : " fd-34-len
       display "  fd-34-len constant    : " byte-length(fd-34-len)

       display "PIC X(13) field         : " length of fixed-character
       display "PIC N(13) field         : " length of fixed-national

       display "PIC S9(5) field         : " length of fixed-nine
       display "PIC S9(5) sign separate : " length of fixed-separate
       display "PIC S9(5) COMP-5        : " length of computable-field

       display "ALPHANUMERIC-EDITED     : " length(formatted-field)

       display "BINARY-DOUBLE field     : " byte-length(binary-field)
       display "POINTER field           : " length(pointer-item)
       >>IF P64 IS SET
       display "  sizeof(char *) > 4"
       >>ELSE
       display "  sizeof(char *) = 4"
       >>END-IF

       display "Complex ODO at 2 and 4  : " length of group-item
       set odo down by 1.
       set odo-2 up by 1.
       display "Complex ODO at 1 and 5  : " length(group-item)

       goback.
       end program variable-size-get.

```


```txt

prompt$ cobc -xj variable-size-get.cob -cb_conf=complex-odo:yes -cb_conf=binary-size:1--8
Size of:
BINARY-CHAR             : 1
  bc-len constant       : 1
FLOAT-DECIMAL-34        : 16
  fd-34-len constant    : 2
PIC X(13) field         : 13
PIC N(13) field         : 26
PIC S9(5) field         : 5
PIC S9(5) sign separate : 6
PIC S9(5) COMP-5        : 3
ALPHANUMERIC-EDITED     : 000000007
BINARY-DOUBLE field     : 000000008
POINTER field           : 000000008
  sizeof(char *) > 4
Complex ODO at 2 and 4  : +000000007
Complex ODO at 1 and 5  : 000000008

```


Note the Complex ODO at 2 and 4.  It uses the '''length of''' phrase, which in
this case cannot be detemined at compile time.  Internally, this creates a
field that happens to cause DISPLAY to treat it as a signed quantity; an
implementation detail.

Also note that complex ODO is not an overly common COBOL structure, and many
compilers will not allow it.  Where does <code>second-inner</code> start?
At byte 4 in this example, even when the initial <code>first-inner</code> is
set at 2. Total size is 7 at first, 3 bytes (2 usable at <code>odo</code> set
to 2) and 4 for the <code>second-inner</code>. It can get more complicated
when complex ODO is grouped within groups, so it is not a common practice
in COBOL.

With a change of '''binary-size''' compile time configuration from 1--8 to 1-2-4-8:


```txt

prompt$ cobc -xj variable-size-get.cob -cb_conf=complex-odo:yes -cb_conf=binary-size:1-2-4-8
...
PIC S9(5) COMP-5        : 4
...

```

and the computional field is now allocated (and handled as) 4 bytes, even
though it will fit in 3.  (Be wary allowing 3 byte binary fields, as they can
be a performance hit on some platforms, possibly even leading to SS$_ACCVIO on
OpenVMS/Vax systems, for instance).  All other fields remain the same size, in
this example.

Sizing of data items is both an art and science for COBOL programmers, and the
standard goes to great lengths to make sure data field sizes are as
deterministic as possible.


## Common Lisp


As with some of the other dynamic languages, we're not concerned with variable size, but rather the size of a value that a variable references. There's not a standard interface for this, but implementations may provide this functionality.

```lisp
(let ((a (cons 1 2))
      (b (make-array 10))
      (c "a string"))
  (list (hcl:find-object-size a)
        (hcl:find-object-size b)
        (hcl:find-object-size c)))
```

returns


```lisp
(12 48 24)
```


However, note that interesting objects are generally composed of several levels of references, often including references to preexisting objects, so what the size should be considered as is often hard to define after the fact. A robust though non-automatic way to determine the “true” memory utilization of some data is to do something like this:


```lisp
(let (items)
  (gc) ; name varies by implementation
  (room)
  (dotimes (x 512)
    (push (allocate-something-of-interest) items))
  (gc)
  (room))
```


[http://www.lispworks.com/documentation/HyperSpec/Body/f_room.htm room] prints information about current memory usage, but in an implementation-defined format. Take the difference of the relevant numbers, divide by 512, and you have the amount of memory consumed by allocating one additional instance of whatever it is.


## D

Every type and variable in D has a property <tt>sizeof</tt>, which give the size of the type in bytes. eg.

```d
int i ;
writefln(i.sizeof) ;        // print 4
int[13] ints1 ;             // static integer array of length 13
writefln(ints1.sizeof) ;    // print 52
int[] ints2 = new int[13] ; // dynamic integer array, variable length, currently 13
writefln(ints2.sizeof) ;    // print 8, all dynamic array has this size
writefln(ints2.length) ;    // print 13, length is the number of allocated element in aggregated type
```



## Delphi


```delphi
i := sizeof({any variable or data type identifier});
```



## Elixir

When “counting” the number of elements in a data structure, Elixir also abides by a simple rule: the function is named '''size''' if the operation is in constant time or '''length''' if the operation is linear.

```elixir
list = [1,2,3]
IO.puts length(list)                    #=> 3

tuple = {1,2,3,4}
IO.puts tuple_size(tuple)               #=> 4

string = "Elixir"
IO.puts String.length(string)           #=> 6
IO.puts byte_size(string)               #=> 6
IO.puts bit_size(string)                #=> 48

utf8 = "○×△"
IO.puts String.length(utf8)             #=> 3
IO.puts byte_size(utf8)                 #=> 8
IO.puts bit_size(utf8)                  #=> 64

bitstring = <<3 :: 2>>
IO.puts byte_size(bitstring)            #=> 1
IO.puts bit_size(bitstring)             #=> 2

map = Map.new([{:b, 1}, {:a, 2}])
IO.puts map_size(map)                   #=> 2
```



## Erlang


```txt

24> erlang:tuple_size( {1,2,3} ).
3
25> erlang:length( [1,2,3] ).
3
29> erlang:bit_size( <<1:11>> ).
11
30> erlang:byte_size( <<1:11>> ).
2

```



## Forth

Forth is very close to the metal. Forth 94 standardized the size of an integer to be the native integer size of the CPU. Forth refers to this as a CELL. The defining word VARIABLE creates a variable that is one cell wide. The word CELLS takes an integer parameter and returns the number of bytes in one CELL.

```Forth
: .CELLSIZE  ( -- ) CR 1 CELLS . ." Bytes" ;
VARIABLE X  ( creates a variable 1 cell wide)
```

Test at the GNU Forth (32bit) console

```forth
.CELLSIZE
4 Bytes ok
-1 X !  ok
HEX X @ U. FFFFFFFF  ok</Lang>


## Fortran

The intrinsic functions bit_size and digits can be used to find the size of an integer. Bit_size returns the number of bits in an integer while digits returns the number of significant digits in the integer. Because of the use of signed integers this will be one less than the bit size. Digits can be used on real variables where it returns the number of significant figures in the mantissa.

```fortran
INTEGER, PARAMETER ::  i8 = SELECTED_INT_KIND(2)
INTEGER, PARAMETER :: i16 = SELECTED_INT_KIND(4)
INTEGER, PARAMETER :: i32 = SELECTED_INT_KIND(8)
INTEGER, PARAMETER :: i64 = SELECTED_INT_KIND(16)
INTEGER(i8)  :: onebyte = 0
INTEGER(i16) :: twobytes = 0
INTEGER(i32) :: fourbytes = 0
INTEGER(i64) :: eightbytes = 0

WRITE (*,*) BIT_SIZE(onebyte), DIGITS(onebyte)             ! prints 8 and 7
WRITE (*,*) BIT_SIZE(twobytes), DIGITS(twobytes)           ! prints 16 and 15
WRITE (*,*) BIT_SIZE(fourbytes),  DIGITS(fourbytes)        ! prints 32 and 31
WRITE (*,*) BIT_SIZE(eightbytes),  DIGITS(eightbytes)      ! prints 64 and 63
WRITE (*,*) DIGITS(0.0), DIGITS(0d0)                       ! prints 24 and 53
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim i As Integer
Dim l As Long
Dim s As Short
Dim b As Byte
Print "An integer occupies "; SizeOf(i); " bytes"
Print "A  long occupies    "; SizeOf(l); " bytes"
Print "A  short occupies   "; SizeOf(s); " bytes"
Print "A  byte occupies    "; SizeOf(b); " byte"

' or use type directly rather than a variable

Print "A  boolean occupies "; SizeOf(Boolean); " byte"
Print "A  single occupies  "; SizeOf(Single);  " bytes"
Print "A  double occupies  "; SizeOf(Double);  " bytes"

Print
Print "Press any key to quit"
Sleep

```


```txt

An integer occupies  8 bytes
A  long occupies     4 bytes
A  short occupies    2 bytes
A  byte occupies     1 byte
A  boolean occupies  1 byte
A  single occupies   4 bytes
A  double occupies   8 bytes

```



## Free Pascal

The FPC (Free Pascal compiler) supports the UCSD Pascal extension <tt>sizeOf</tt>.
See [[#Delphi|Delphi]] for an example.
It can be used in compile-time expressions, too.
Furthermore, the GNU Pascal extension <tt>bitSizeOf</tt> is available, too, which is particularly interesting for <tt>packed</tt> structures’ members.


## Gambas

'''[https://gambas-playground.proko.eu/?gist=87f102023bd9ef6dbd52b0158be2c3ee Click this link to run this code]'''

```gambas
Public Sub Main()

Print "Boolean =\t " & SizeOf(gb.Boolean)
Print "Byte =\t\t " & SizeOf(gb.Byte)
Print "Short =\t\t " & SizeOf(gb.Short)
Print "Integer =\t " & SizeOf(gb.Integer)
Print "Single =\t " & SizeOf(gb.Single)
Print "Long =\t\t " & SizeOf(gb.Long)
Print "Float =\t\t " & SizeOf(gb.Float)
Print "Date =\t\t " & SizeOf(gb.Date)
Print "String =\t " & SizeOf(gb.String)
Print "Object =\t " & SizeOf(gb.Object)
Print "Pointer =\t " & SizeOf(gb.Pointer)
Print "Variant =\t " & SizeOf(gb.Variant)

End
```

Output:

```txt

Boolean =        1
Byte =           1
Short =          2
Integer =        4
Single =         4
Long =           8
Float =          8
Date =           8
String =         8
Object =         8
Pointer =        8
Variant =        16

```



## Go


```go
import "unsafe"

unsafe.Sizeof(x)
```

More detail:

```go
package main

import (
    "fmt"
    "reflect"
    "runtime"
    "unsafe"
)

func main() {
    // unsafe.Sizeof returns the size in bytes.
    var i int
    fmt.Println(unsafe.Sizeof(i))
    // The size returned is that of the top level object and does not
    // include any referenced data.  A type like string always returns
    // the same number, the size of the string header.
    fmt.Println(unsafe.Sizeof("Rosetta"))
    fmt.Println(unsafe.Sizeof("Code"))
    // For some untrusted environments, package unsafe is not available
    // but reflect is.  The Size method of a type will return the same value
    // as unsafe.Sizeof.
    fmt.Println(reflect.TypeOf("Cod").Size())
    // Some sizes are implementation dependent.
    fmt.Println(runtime.Version(), runtime.GOARCH)
}
```

```txt

8
16
16
16
go1.2 amd64

```



## Haskell

only works with types that instance Storable:

```haskell
import Foreign

sizeOf (undefined :: Int) -- size of Int in bytes (4 on mine)
sizeOf (undefined :: Double) -- size of Double in bytes (8 on mine)
sizeOf (undefined :: Bool) -- size of Bool in bytes (4 on mine)
sizeOf (undefined :: Ptr a) -- size of Ptr in bytes (4 on mine)
```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon fall into the category of high level languages that don't get close to the machine.  As much as possible is done to provide high level and portable interfaces.  Two methods are available, one returns the high level size of a value and the other gives information about actual size.

* The unary operator ''*'' returns the size of most data types and structures
** integer and real numbers will be coerced to strings
** not defined for files and I/O objects
** for coexpressions it returns the number of results produced
* The keyword &allocated provides information about the actual storage allocated for the data and overhead of each type
** This will show 0 for the assignment of program constants such as strings.  Manipulation will be required to produce an allocation.


```Icon
record rec0()
record rec4(a,b,c,d)

procedure main() # get size

every i := seq(1) do {
   a0 := &allocated
      x := case i of {
            1 : "ABCDEFGH"
            2 : reverse(x)
            10 : &digits
            11 : x--x
            20 : []
            21 : [1,2]
            22 : [1,2,3]
            30 : set()
            31 : set("X")
            32 : set("A","B")
            40 : table(1)
            50 : rec0()
            51 : rec4()
            60 : create seq(1)
            99 : break
            default : next
            }
   a1 := &allocated
   write("type=",type(x)," *x=",*x," bytes allocated=",a1-a0)
   }
end
```


```txt
type=string *x=8 bytes allocated=0
type=string *x=8 bytes allocated=8
type=cset *x=10 bytes allocated=0
type=cset *x=0 bytes allocated=40
type=list *x=0 bytes allocated=116
type=list *x=2 bytes allocated=68
type=list *x=3 bytes allocated=76
type=set *x=0 bytes allocated=104
type=set *x=1 bytes allocated=124
type=set *x=2 bytes allocated=144
type=table *x=0 bytes allocated=112
type=rec0 *x=0 bytes allocated=16
type=rec4 *x=4 bytes allocated=48
type=co-expression *x=0 bytes allocated=96
```

The results above illustrate both measurements.
I believe that an empty list is allocated with growth room for 8 elements which explains why it would require more storage than a list of 1 or 2 elements.


## IDL

IDL is array based, so its <tt>size()</tt> function is geared towards that:


```idl
arr = intarr(3,4)
print,size(arr)
;=> prints this:
       2           3           4           2          12
```


The result means: 2 dimensions in the array, the first dimension has extent 3, the second has extent 4, the elements of the array are 2-byte integers (IDL's default for an "int"), there's a total of 12 elements in the array.


## J


In J, the function <code>7!:5</code> is analogous to <code>sizeof</code> in C.

For example:

```j
some_variable =: 42
7!:5<'some_variable'
```

An advantage of <code>7!:5</code> is that it can be used on any name, including functions, operators, etc (i.e. it's not just restricted to variables):

```j
some_function =: +/ % #
7!:5<'some_function'
```



## Julia


```julia>julia
 sizeof(Int8)
1

julia> t = 1
1

julia> sizeof(t)
8
```

(The last line returns 4 on a 32-bit machine or when Julia is compiled in 32-bit mode.)


## Kotlin


```scala
// version 1.1.2

fun main(args: Array<String>) {
   /* sizes for variables of the primitive types (except Boolean which is JVM dependent) */
   println("A  Byte   variable occupies:  ${java.lang.Byte.SIZE / 8} byte")
   println("A  Short  variable occupies:  ${java.lang.Short.SIZE / 8} bytes")
   println("An Int    variable occupies:  ${java.lang.Integer.SIZE / 8} bytes")
   println("A  Long   variable occupies:  ${java.lang.Long.SIZE / 8} bytes")
   println("A  Float  variable occupies:  ${java.lang.Float.SIZE / 8} bytes")
   println("A  Double variable occupies:  ${java.lang.Double.SIZE / 8} bytes")
   println("A  Char   variable occupies:  ${java.lang.Character.SIZE / 8} bytes")
}
```


```txt

A  Byte   variable occupies:  1 byte
A  Short  variable occupies:  2 bytes
An Int    variable occupies:  4 bytes
A  Long   variable occupies:  8 bytes
A  Float  variable occupies:  4 bytes
A  Double variable occupies:  8 bytes
A  Char   variable occupies:  2 bytes

```



## Lasso


```Lasso
local(
	mystring	= 'Hello World',
	myarray		= array('one', 'two', 3),
	myinteger	= 1234
)

// size of a string will be a character count
#mystring -> size
'<br />'

// size of an array or map will be a count of elements
#myarray -> size
'<br />'

// elements within an array can report size
#myarray -> get(2) -> size
'<br />'

// integers or decimals does not have sizes
//#myinteger -> size // will fail
// an integer can however be converted to a string first
string(#myinteger) -> size
```

->11

3

3

4


## Mathematica


```Mathematica
ByteCount["somerandomstring"]
```


```txt
64
```


=={{header|Modula-3}}==
BITSIZE and BYTESIZE are built in functions.


```modula3
MODULE Size EXPORTS Main;

FROM IO IMPORT Put;
FROM Fmt IMPORT Int;

BEGIN
  Put("Integer in bits: " & Int(BITSIZE(INTEGER)) & "\n");
  Put("Integer in bytes: " & Int(BYTESIZE(INTEGER)) & "\n");
END Size.
```


```txt

Integer in bits: 32
Integer in bytes: 4

```



## Nim


```nim
echo "An int contains ", sizeof(int), " bytes."
```


=={{header|NS-HUBASIC}}==
Note: This only works with strings.
<lang NS-HUBASIC>10 PRINT LEN(VARIABLE$)
```



## OCaml


```ocaml
(** The result is the size given in word.
  The word size in octet can be found with (Sys.word_size / 8).
  (The size of all the datas in OCaml is at least one word, even chars and bools.)
*)
let sizeof v =
  let rec rec_size d r =
    if List.memq r d then (1, d) else
    if not(Obj.is_block r) then (1, r::d) else
    if (Obj.tag r) = (Obj.double_tag) then (2, r::d) else
    if (Obj.tag r) = (Obj.string_tag) then (Obj.size r, r::d) else
    if (Obj.tag r) = (Obj.object_tag) ||
       (Obj.tag r) = (Obj.closure_tag)
    then invalid_arg "please only provide datas"
    else
      let len = Obj.size r in
      let rec aux d sum i =
        if i >= len then (sum, r::d) else
        let this = Obj.field r i in
        let this_size, d = rec_size d this in
        aux d (sum + this_size) (i+1)
      in
      aux d (1) 0
  in
  fst(rec_size [] (Obj.repr v))
;;
```


testing in the toplevel:

```ocaml
# sizeof 234 ;;
- : int = 1

# sizeof 23.4 ;;
- : int = 2

# sizeof (1,2);;
- : int = 3

# sizeof (2, 3.4) ;;
- : int = 4

# sizeof (1,2,3,4,5) ;;
- : int = 6

# sizeof [| 1;2;3;4;5 |] ;;
- : int = 6

# sizeof [1;2;3;4;5] ;;
- : int = 11

(* because a list is equivalent to *)

# sizeof (1,(2,(3,(4,(5,0))))) ;;
- : int = 11

# type foo = A | B of int | C of int * int ;;
type foo = A | B of int | C of int * int

# sizeof A ;;
- : int = 1

# sizeof (B 3) ;;
- : int = 2

# sizeof (C(1,2)) ;;
- : int = 3

# sizeof true ;;
- : int = 1

# sizeof 'A' ;;
- : int = 1

# sizeof `some_pvar ;;
- : int = 1

# sizeof "" ;;
- : int = 1

# sizeof "Hello!" ;;
- : int = 2
(* remember the size is given in words
   (so 4 octets on 32 bits machines) *)

# for i=0 to 16 do
    Printf.printf "%d -> %d\n" i (sizeof(String.create i))
  done;;
0 -> 1
1 -> 1
2 -> 1
3 -> 1
4 -> 2
5 -> 2
6 -> 2
7 -> 2
8 -> 3
9 -> 3
10 -> 3
11 -> 3
12 -> 4
13 -> 4
14 -> 4
15 -> 4
16 -> 5
- : unit = ()

# sizeof(Array.create 10 0) ;;
- : int = 11

# sizeof(Array.create 10 (String.create 20)) ;;
- : int = 16

# sizeof(Array.init 10 (fun _ -> String.create 20)) ;;
- : int = 61
```



## ooRexx

In ooRexx, all variables are just untyped references to objects, so there is no inherent or visible variable size.

For character strings see Rexx (except for the d= example).
Other objects have a "required string value" used, e.g., in Say object.
The "size" of objects is indeed hidden and unvisible. --[[User:Walterpachl|Walterpachl]] 05:51, 1 October 2012 (UTC)


## PARI/GP

In GP, this gets the size of the variable x in bytes:

```parigp
sizebyte(x)
```


In PARI,

```C
lg(x)
```

returns the length of the variable x in words. <code>gsizebyte</code> and <code>gsizeword</code> are also available.


## Pascal

Pascal discourages the programmer to care about specific data sizes.
Nevertheless [[#Delphi|Delphi]], [[#Free Pascal|FPC]] and GPC (GNU Pascal compiler) among other compilers support the UCSD Pascal extension <tt>sizeOf</tt>.


## Perl

```perl
use Devel::Size qw(size total_size);

my $var = 9384752;
my @arr = (1, 2, 3, 4, 5, 6);
print size($var);         # 24
print total_size(\@arr);  # 256
```



## Perl 6

Perl 6 tries to avoid generic terms such as "size" and "length", instead providing methods that are expressed in terms of the units appropriate to the abstraction level.

```perl6
# Textual strings are measured in characters (graphemes)
my $string = "abc";

# Arrays are measured in elements.
say $string.chars;     # 3
my @array = 1..5;
say @array.elems;      # 5

# Buffers may be viewed either as a byte-string or as an array of elements.
my $buffer = '#56997; means "four dragons".'.encode('utf8');
say $buffer.bytes;     # 26
say $buffer.elems;     # 26
```

Perl's Int type is arbitrary sized, and therefore the abstract size of the integer depends on how many bits are needed to represent it.  While the internal representation is likely to be "chunky" by 32 or 64 bits, this is considered an implementation detail and is not revealed to the programmer.

Native types such as int64 or num32 are of fixed size; Perl 6 supports representational polymorphism of such types (and compositions of such types) through a pluggable meta-object protocol providing "knowhow" objects for the various representations.  Currently the NativeHOW knowhow may be interrogated for the bit sizes of native types, but this is officially outside the purview of the language itself (so far).


## Phix


```Phix
printf(1,"An integer contains %d bytes.\n", machine_word())
```

See builtins\VM\pHeap.e for the complete low-level details. Because of shared references,
it is often not practical to obtain a meaningful size, and you can often store a "sparse"
structure in far less memory than expected. Plus (as per Icon) sequences and strings have
some room (up to 50%) for expansion. The builtin length() function gives no indication of
the latter or the size of individual elements, unless used in a recursive routine, and as
said even then may grossly overstate actual memory use due to shared references.
You may also wish to examine builtins\VM\pMemChk.e for some ideas about how to perform a
detailed heap analysis - in that specific case for post-run memory leak checking, done from
the safety of a secondary heap, so that the one it is examining does not suddenly change.


## PicoLisp

In PicoLisp, all variables have the same size (a single cell). Therefore it
makes more sense to inspect the size of data structures. This can be done with
the '[http://software-lab.de/doc/refS.html#size size]' and
'[http://software-lab.de/doc/refL.html#length length]' functions.


## PL/I


```PL/I

put skip list (SIZE(x)); /* gives the number of bytes occupied by X */
                         /* whatever data type or structure it is.  */

put skip list (CURRENTSIZE(x));
                         /* gives the current number of bytes of X  */
                         /* actually used by such things as a       */
                         /* varying-length string, including its    */
                         /* length field.                           */

```



## Pop11

From abstract point of view Pop11 variables are bindings between identifiers and values.  In concrete terms Pop11 variables store references to values in the heap.  Each reference takes one machine word (4 bytes on 32-bit machines and 8 bytes on 64-bit machines).  Pop11 identifiers take 3 machine words, but are only needed for "permanent" variables (lexical variables do not need identifiers after compilation). Additionally variable names (words) need space (4 machine for word + space for string
corresponding to the word).  The bottom line is: variable needs one machine word plus some overhead due to introspection.

Form user point of view more important is space taken by values (size of values referenced by a single variable typically varies during program execution).  The datasize function gives amount (in machine words) of space directly used by given value:


```pop11
;;; Prints 0 because small integers need no heap storage
datasize(12) =>
;;; Prints 3: 3 character fits into single machine word, 1 word
;;; for tag, 1 for length
datasize('str') =>
;;; 3 element vector takes 5 words: 3 for values, 1 for tag, 1 for
;;; length
datasize({1 2 3}) =>
;;; Prints 3 because only first node counts
datasize([1 2 3]) =>
```


Note that large amount of data my be referenced from given value, but this data is potentially shared, so there is no canonical way to assign it to a single value or variable.


## PureBasic


```PureBasic
Define a
Debug  SizeOf(a)
; This also works for structured variables
```



## Python

This information is only easily available for the array type:

```python>>>
 from array import array
>>> argslist = [('l', []), ('c', 'hello world'), ('u', u'hello \u2641'),
	('l', [1, 2, 3, 4, 5]), ('d', [1.0, 2.0, 3.14])]
>>> for typecode, initializer in argslist:
	a = array(typecode, initializer)
	print a, '\tSize =', a.buffer_info()[1] * a.itemsize
	del a


array('l') 	Size = 0
array('c', 'hello world') 	Size = 11
array('u', u'hello \u2641') 	Size = 14
array('l', [1, 2, 3, 4, 5]) 	Size = 20
array('d', [1.0, 2.0, 3.1400000000000001]) 	Size = 24
>>>
```

Also:
```python
import sys
sys.getsizeof(obj)
```



## R

object.size gives '''an estimate''' of the amount of memory used to store the variable, in (kilo/Mega/Giga) bytes.  See also dim, length and nchar for determining the extent of mulitdimensional variables (such as matrices, lists, etc).

```R
# Results are system dependent
num <- c(1, 3, 6, 10)
object.size(num)  # e.g. 56 bytes

#Allocating vectors using ':' results in less memory being (reportedly) used
num2 <- 1:4
object.size(num2) # e.g. 40 bytes

#Memory shared by objects isn't always counted
l <- list(a=c(1, 3, 6, 10), b=1:4)
object.size(l)    # e.g. 280 bytes

l2 <- list(num, num2)
object.size(l2)   # e.g. 128 bytes
```



## Racket


```racket

#lang racket
(require ffi/unsafe)
(define-syntax-rule (sizes t ...)
  (begin (printf "sizeof(~a) = ~a\n" 't (ctype-sizeof t)) ...))
(sizes _byte _short _int _long _llong _float _double)

```



## REXX

In REXX, you simply set a variable to a value (it could be an expression);

the value's length is the size of the variable's value.

```rexx
/*REXX program demonstrates  (see the penultimate statement)  how to    */
/*     to find the  size  (length)  of the value of a REXX variable.    */

/*REXX doesn't reserve any storage for any variables, as all variables  */
/*are stored as character strings, including boolean.   Storage is      */
/*obtained as necessary when REXX variables are assigned (or reassigned)*/

a = 456                                /*length of  A   is    3         */
b = "heptathlon"                       /*length of  B   is   10         */
c = "heptathlon (7 events)"            /*length of  C   is   21         */
d = ''                                 /*length of  D   is    0         */
d = ""                                 /*same as above.                 */
d = left('space junk' ,0)              /*same as above.                 */
d =                                    /*same as above.                 */
e = 99-9                               /*length of  E   is    2  (e=90) */
f = copies(a,100)                      /*length of  F   is  300  (a=456)*/
g.1 = -1                               /*length of  G.1 is    2         */
g.2 = -1.0000                          /*length of  G.2 is    7         */
                                       /*length of  HHH is    3         */

                                       /*Note that when a REXX variable */
                                       /*isn't SET, then the value of it*/
                                       /*is the uppercased name itself, */
                                       /*so in this case (upper):   HHH */

something = copies(a, random(100))     /*length is something, all right,*/
                                       /*could be 3 to 300 bytes, by gum*/
thingLen  = length(something)          /*use LENGTH bif to find its len.*/
say 'length of SOMETHING =' thingLen   /*display the length of SOMETHING*/

/*┌────────────────────────────────────────────────────────────────────┐
  │ Note that the variable's data (value) isn't the true cost of the   │
  │ size of the variable's value.  REXX also keeps the   name   of     │
  │ the (fully qualified) variable as well.                            │
  │                                                                    │
  │ Most REXX interpreters keep (at a miminum):                        │
  │                                                                    │
  │   ∙  a four-byte field which contains the length of the value      │
  │   ∙  a four-byte field which contains the length of the var name   │
  │   ∙  an   N-byte field which contains the name of the variable     │
  │   ∙  an   X-byte field which contains the variable's value         │
  │   ∙  a  one-byte field which contains the status of the variable   │
  │                                                                    │
  │ [Note that PC/REXX uses a two-byte field for the first two fields] │
  │                                                                    │
  │                                                                    │
  │ Consider the following two DO loops assigning a million variables: │
  │                                                                    │
  │                            do j=1 to 1000000                       │
  │                            integer_numbers.j=j                     │
  │                            end                                     │
  │                        ════════ and ════════                       │
  │                            do k=1 to 1000000                       │
  │                            #.k=k                                   │
  │                            end                                     │
  │                                                                    │
  │ The  "j" loop uses  35,777,792  bytes for the compound variables,  │
  │ The  "k" loop uses  21,777,792  bytes for the compound variables,  │
  │ (excluding the DO loop indices  [j and k]  themselves).            │
  └────────────────────────────────────────────────────────────────────┘*/
```



## Ring


```ring

list1 = list(2)
list2 = list(4)
list3 = list(6)
list4 = list(7)
list5 = list(5)

see "Size of list1 is : " + len(list1) + nl
see "Size of list2 is : " + len(list2) + nl
see "Size of list3 is : " + len(list3) + nl
see "Size of list4 is : " + len(list4) + nl
see "Size of list5 is : " + len(list5) + nl

```

Output:

```txt

Size of list1 is : 2
Size of list2 is : 4
Size of list3 is : 6
Size of list4 is : 7
Size of list5 is : 5

```



## Ruby

Almost all objects in Ruby (MRI) take 40 bytes (on a x64 machine) initially. Sometimes this is enough to store the entire object, sometimes additional memory has to be allocated. For strings that is the case if they are longer than 23 chars. The additional memory can be retrieved using 'ObjectSpace':


```ruby

require 'objspace'

p ObjectSpace.memsize_of("a"*23)    #=> 0
p ObjectSpace.memsize_of("a"*24)    #=> 25
p ObjectSpace.memsize_of("a"*1000) #=> 1001

```



## Rust


```Rust
use std::mem;

fn main() {
    // Specify type
    assert_eq!(4, mem::size_of::<i32>());

    // Provide a value
    let arr: [u16; 3] = [1, 2, 3];
    assert_eq!(6, mem::size_of_val(&arr));
}

```



## Scala

```Scala
  def nBytes(x: Double) = ((Math.log(x) / Math.log(2) + 1e-10).round + 1) / 8

  val primitives: List[(Any, Long)] =
    List((Byte, Byte.MaxValue),
      (Short, Short.MaxValue),
      (Int, Int.MaxValue),
      (Long, Long.MaxValue))

  primitives.foreach(t => println(f"A Scala ${t._1.toString.drop(13)}%-5s has ${nBytes(t._2)} bytes"))
```

```txt
A Scala Byte  has 1 bytes
A Scala Short has 2 bytes
A Scala Int   has 4 bytes
A Scala Long  has 8 bytes
```



## Swift


```swift
sizeofValue(x)
```

More detail:

```swift
// sizeof and sizeofValue return the size in bytes.
println(sizeof(Int))
var i: Int = 42
println(sizeofValue(i))
// The size returned is that of the top level value and does not
// include any referenced data.  A type like String always returns
// the same number, the size of the String struct.
println(sizeofValue("Rosetta"))
println(sizeofValue("Code"))}
```

```txt

8
8
24
24

```



## Tcl

Since all variables are ultimately strings in Tcl, this is easy:

```tcl
string bytelength $var
```

There is additional overhead per value and per variable, which depends on the architecture that Tcl was built for and the version of Tcl. In 8.5 on a ILP-32 architecture, local variables have an overhead of 8 bytes (without traces) and values have a minimum overhead of 24 bytes (this minimum is achieved for integers that fit in a signed 64-bit integer type or a double-precision float).

However, it is usual to not think in terms of the low-level size of a value; instead, concepts such as the length of the string (<code>string length</code>), list (<code>llength</code>) or dictionary (<code>dict size</code>) are used. <!-- Also, we plan to get rid of bytelength; it misleads people into doing really wrong things… -->


## Toka

There are two primary data types in Toka, cells and characters. The size of these can be obtained easily:


```toka
char-size .
cell-size .
```


If you load the floating point support, you can also obtain the size of a floating point number:


```toka
needs floats
float-size .
```


All sizes are returned in bytes.


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT,{}
string="somerandomstring"
size=SIZE(string)
length=LENGTH(string)

PRINT "string:     ",string
PRINT "has size:   ",size
PRINT "and length: ",length

```

```txt

string:     somerandomstring
has size:   1
and length: 16

```



## TXR


### Lisp Object Size

All Lisp values are pointer-sized cells, so they have a basic size that is four or eight bytes, depending on whether the processor architecture is 32 or 64 bits. Heap values take up a four-cell record. And some objects have additional dynamically allocated memory.  The <code>prof</code> operator can be wrapped around code which constructs and returns an object to calculate the size of the heap part plus dynamically allocated memory:


```txt
1> (prof 1)
(1 0 0 0)
```


The first element is the value itself; the remaining values are dynamic memory from <code>malloc</code>, Lisp heap memory and execution time. Here, no memory is attributed to the 1. It takes up a four byte pointer on this system, but that isn't counted.


```txt
2> (list 1 2 3)
((1 2 3) 0 48 0)
```


The list object requires three cons cells at 16 (4x4) bytes each.


```txt
3> (prof (copy "foobar"))
("foobar" 28 16 0)
```


The <code>"foobar"</code> string requires 28 bytes of <code>malloc</code> memory (7 wide characters including a terminating null). The heap entry takes 16 bytes.

**Note:** the <code>pprof</code> macro ("pretty prof") will gather and print these values in a nice way on the <code>*stdout*</code> stream:


```txt
2> (pprof (copy "foobar"))
malloc bytes:            28
gc heap bytes:           16
total:                   44
milliseconds:             0
"foobar"
```



### FFI


In the FFI type system, the <code>sizeof</code> macro operator reports size of types.


```txt
1> (sizeof uchar)
1
2> (sizeof (array 3 char))
3
3> (sizeof (struct foo (x (bit 17 uint32))
                       (y (bit 3 uint8))
                       (z (array 16 char))))
20
4> (sizeof double)
8
20
```


The <code>struct</code> size corresponds to the size of the C struct


```c
struct foo {
  uint32_t x : 17;
  uint8_t y : 3;
  char z[16];
};
```


as calculated by the GNU C compiler on the same platform.  The `uint32_t` leading bitfield creates a minimum alignment of four bytes. The `y` bitfield is packed into the third byte of the structure, and the `z` array starts on the fourth, ending on the nineteenth. The alignment requirement pads the structure to 20.

We can influence the alignment with the <code>align</code> type constructor:


```txt
6> (sizeof (struct foo (x (align 1 (bit 17 uint32)))
                       (y (bit 3 uint8))
                       (z (array 16 char))))
19
```

The leading bitfield is now deemed to be byte aligned, so the structure is no longer padded for the sake of its alignment.


### Variable Size


Since the task is worded as being about '''variables''' rather than objects, what we can do is explore the memory costs of a lexical environment.

An empty environment takes up a 16 byte heap record:


```txt
1> (prof (let ()))
(nil 0 16 0)
```


Adding a variable to the environment brings in an additional 32 bytes:


```txt
2> (prof (let (a)))
(nil 0 48 0)
```



## UNIX Shell



```sh
# In the shell all variables are stored as strings, so we use the same technique
# as for finding the length of a string:
greeting='Hello, world!'
greetinglength=`printf '%s' "$greeting" | wc -c`
echo "The greeting is $greetinglength characters in length"
```



## Ursala

The virtual machine represents all code and data as binary trees of
cells.
The number of cells required for any object can be computed by the built in <code>weight</code> function (or by an equivalent user-defined function), which takes an argument of any type and returns a natural number.
Host memory usage for any given object is worst case linear in the
weight, but may be considerably less due to sharing (i.e., copying
something by copying only a reference to it, which is done
automatically and invisibly to the programmer with correct semantics).

An additional facility exists for arbitrary precision floating point numbers,
which are based on the [http://www.mpfr.org mpfr] library.
The library function <code>mpfr..prec</code> applies to a number in mpfr format
and returns the number of bits of precision in the mantissa.
Host memory usage is linear plus a small constant.

```Ursala
#import std

#cast %nL

examples = <weight 'hello',mpfr..prec 1.0E+0>
```

```txt

<40,160>

```



## Vala


```vala

void main(){
    ulong x = sizeof(int); // returns # of bytes used to store an int
    // is returned as a ulong, but could be typecasted to int with: int x = (int) sizeof(int)
    stdout.printf("%lu\n", x);
}

```



## XPL0

This ugly piece of code may be required because there is currently
no 'sizeof' operator (at least not in the DOS version,
but it is in the Windows version).
This is not a big problem since there are only two data types, integer and real; thus there are only two sizes to keep track of, 4 and 8 bytes.
However, there are 16-bit versions of the compilers that have 2-byte integers,
and a program that runs in both the 16- and 32-bit versions
usually defines a constant (typically called "IntSize" = 2 or 4)
at the beginning of the program to deal with the different sizes.
A common situation where this arises is when allocating memory
for dynamic integer arrays using the Reserve intrinsic.


```XPL0
include c:\cxpl\codes;
int  Size;
int  A, B;
real X, Y;
[Size:= addr B - addr A;
IntOut(0, Size);  CrLf(0);
Size:= addr Y - addr X;
IntOut(0, Size);  CrLf(0);
]
```


```txt

4
8

```



## zkl


```zkl
(123).len() //-->1 (byte)
(0).MAX.len()   //-->8 (bytes), ie the max number of bytes in an int
(1.0).MAX.len() //-->8 (bytes), ie the max number of bytes in an float
"this is a test".len() //-->14
L(1,2,3,4).len()   //-->4
Dictionary("1",1, "2",2).len() //-->2 (keys)
Data(0,Int,1,2,3,4).len() //-->4 (bytes)
Data(0,String,"1","2","3","4").len() //-->8 bytes (ASCIIZ)
```


Put the data into a variable, same results: a:=123; a.len() --> 1

There is also a size property which returns the size of the underlying [implementation] data structure.
