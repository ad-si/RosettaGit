+++
title = "Non-decimal radices/Input"
description = ""
date = 2018-05-04T15:31:54Z
aliases = []
[extra]
id = 4352
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
languages = [
  "ada",
  "aime",
  "algol_68",
  "autohotkey",
  "bbc_basic",
  "c",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "e",
  "elixir",
  "erlang",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "hicest",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lua",
  "mathematica",
  "nim",
  "ocaml",
  "oz",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powershell",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "standard_ml",
  "tcl",
  "xpl0",
  "zkl",
]
+++

## Task

It is common to have a string containing a number written in some format, with the most common ones being decimal, hexadecimal, octal and binary. Such strings are found in many places (user interfaces, configuration files, XML data, network protocols, etc.)

This task requires parsing of such a string (which may be assumed to contain nothing else) using the language's built-in facilities if possible. Parsing of decimal strings is required, parsing of other formats is optional but should be shown (i.e., if the language can parse in base-19 then that should be illustrated).

The solutions may assume that the base of the number in the string is known. In particular, if your language has a facility to guess the base of a number by looking at a prefix (e.g. "0x" for hexadecimal) or other distinguishing syntax as it parses it, please show that.

The reverse operation is in task [[Non-decimal radices/Output]]

For general number base conversion, see [[Non-decimal radices/Convert]].


## Ada


Ada supports the input format <BASE>#<VALUE>#, for example 16#AF42# or 2#1010110# or 8#777#. This can be used for input through Ada.Text_IO.Integer_IO or for conversion through Integer'Value. More details on this format can be found here: [http://www.adaic.com/standards/05rm/html/RM-2-4-2.html Ada 2005 Reference Manual - 2.4.2 Based Literals].

Limited to Bases 2 to 16.

Works with Float values, too.

numbers.adb:

```Ada
with Ada.Text_IO;
procedure Numbers is
   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
   package Float_IO is new Ada.Text_IO.Float_IO (Float);
begin
   Int_IO.Put (Integer'Value ("16#ABCF123#"));
   Ada.Text_IO.New_Line;
   Int_IO.Put (Integer'Value ("8#7651#"));
   Ada.Text_IO.New_Line;
   Int_IO.Put (Integer'Value ("2#1010011010#"));
   Ada.Text_IO.New_Line;
   Float_IO.Put (Float'Value ("16#F.FF#E+2"));
   Ada.Text_IO.New_Line;
end Numbers;
```


Output:

```txt
  180154659
       4009
        666
 4.09500E+03
```



## Aime


```aime
o_integer(alpha("f4240", 16));
o_byte('\n');
o_integer(alpha("224000000", 5));
o_byte('\n');
o_integer(alpha("11110100001001000000", 2));
o_byte('\n');

o_integer(alpha("03641100", 0));
o_byte('\n');
o_integer(alpha("0xf4240", 0));
o_byte('\n');
```



## ALGOL 68

```algol68
main:
(
  FILE fbuf; STRING sbuf;

  OP FBUF = (STRING in sbuf)REF FILE: (
    sbuf := in sbuf;
    associate(fbuf, sbuf);
    fbuf
  );

  BITS num;

  getf(FBUF("0123459"), ($10r7d$, num));
  printf(($gl$, ABS num)); # prints 123459 #

  getf(FBUF("abcf123"), ($16r7d$, num));
  printf(($gl$, ABS num)); # prints 180154659 #

  getf(FBUF("7651"), ($8r4d$, num));
  printf(($gl$, ABS num)); # prints 4009 #

  getf(FBUF("1010011010"), ($2r10d$, num));
  printf(($gl$, ABS num)) # prints 666 #

)
```

Output:

```txt

    +123459
 +180154659
      +4009
       +666

```



## AutoHotkey

There is no built in support for generic base parsing.

Please see [[Number base conversion#AutoHotkey|Number base conversion]]



## BBC BASIC


```bbcbasic
      REM VAL parses decimal strings:
      PRINT VAL("0")
      PRINT VAL("123456789")
      PRINT VAL("-987654321")

      REM EVAL can be used to parse binary and hexadecimal strings:
      PRINT EVAL("%10101010")
      PRINT EVAL("%1111111111")
      PRINT EVAL("&ABCD")
      PRINT EVAL("&FFFFFFFF")
```

'''Output:'''

```txt

         0
 123456789
-987654321
       170
      1023
     43981
        -1

```



## C


In addition to <tt>strtol()</tt> described in the Number base conversion task, you could also use the <code>scanf</code> family of functions to parse un-prefixed hexadecimal, decimal, and octal numbers:

```c
#include <stdio.h>

int main()
{
  int num;

  sscanf("0123459", "%d", &num);
  printf("%d\n", num); /* prints 123459 */

  sscanf("abcf123", "%x", &num);
  printf("%d\n", num); /* prints 180154659 */

  sscanf("7651", "%o", &num);
  printf("%d\n", num); /* prints 4009 */

  /* binary not supported */

  return 0;
}
```


The <code>strtol()</code> function can also parse prefixed hexadecimal, octal, and decimal strings based on the prefix, when passed a base of 0:

```c
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

int main()
{
  int num;
  char *endptr;

  num = strtol("123459", &endptr, 0);
  assert(*endptr == '\0');
  printf("%d\n", num); /* prints 123459 */

  num = strtol("0xabcf123", &endptr, 0);
  assert(*endptr == '\0');
  printf("%d\n", num); /* prints 180154659 */

  num = strtol("07651", &endptr, 0);
  assert(*endptr == '\0');
  printf("%d\n", num); /* prints 4009 */

  /* binary not supported */

  return 0;
}
```


## C#

```c#
using System;

class Program
{
    static void Main()
    {
        var value = "100";
        var fromBases = new[] { 2, 8, 10, 16 };
        var toBase = 10;
        foreach (var fromBase in fromBases)
        {
            Console.WriteLine("{0} in base {1} is {2} in base {3}",
                value, fromBase, Convert.ToInt32(value, fromBase), toBase);
        }
    }
}
```

Output:
<lang>100 in base 2 is 4 in base 10
100 in base 8 is 64 in base 10
100 in base 10 is 100 in base 10
100 in base 16 is 256 in base 10
```


## C++


```cpp
#include <iostream>
#include <sstream>

int main()
{
  int num;

  std::istringstream("0123459") >> num;
  std::cout << num << std::endl; // prints 123459

  std::istringstream("0123459") >> std::dec >> num;
  std::cout << num << std::endl; // prints 123459

  std::istringstream("abcf123") >> std::hex >> num;
  std::cout << num << std::endl; // prints 180154659

  std::istringstream("7651") >> std::oct >> num;
  std::cout << num << std::endl; // prints 4009

  // binary not supported

  return 0;
}
```



## Common Lisp


```lisp
(parse-integer "abc" :radix 20 :junk-allowed t) ; => 4232
```


If <code>:radix</code> is omitted, it defaults to 10. If <code>:junk-allowed</code> is omitted, it defaults to <code>nil</code>, causing <code>#'parse-integer</code> to signal an error of type <code>parse-error</code> rather than just returning <code>nil</code> whenever the input string isn't a numeral possibly surrounded by whitespace.

## D

```d
import std.stdio, std.conv;

void main() {
    immutable text = "100";
    foreach (base; 2 .. 21)
        writefln("String '%s' in base %d is  %d in base 10" ,
                 text, base, to!int(text, base));
}
```

```txt
String '100' in base 2 is  4 in base 10
String '100' in base 3 is  9 in base 10
String '100' in base 4 is  16 in base 10
String '100' in base 5 is  25 in base 10
String '100' in base 6 is  36 in base 10
String '100' in base 7 is  49 in base 10
String '100' in base 8 is  64 in base 10
String '100' in base 9 is  81 in base 10
String '100' in base 10 is  100 in base 10
String '100' in base 11 is  121 in base 10
String '100' in base 12 is  144 in base 10
String '100' in base 13 is  169 in base 10
String '100' in base 14 is  196 in base 10
String '100' in base 15 is  225 in base 10
String '100' in base 16 is  256 in base 10
String '100' in base 17 is  289 in base 10
String '100' in base 18 is  324 in base 10
String '100' in base 19 is  361 in base 10
String '100' in base 20 is  400 in base 10
```



## E


[[Category:E examples needing attention]] <!-- Is it appropriate to explicitly use __makeInt in this situation, or should it be preferred to import? Does the answer to this change in the presence of a module system? -->


```e
? __makeInt("200", 16)
# value: 512

? __makeInt("200", 10)
# value: 200
```



## Elixir

base: 2 .. 36

```elixir
iex(1)> String.to_integer("1000")
1000
iex(2)> String.to_integer("1000",2)
8
iex(3)> String.to_integer("1000",8)
512
iex(4)> String.to_integer("1000",16)
4096
iex(5)> String.to_integer("ffff",16)
65535
```



## Erlang

My interpretation of the task description is that I can state that the base (here: 17) can be 2..36, without having to show one example of each.
```txt

<12> erlang:list_to_integer("ffff", 17).
78300

```


=={{header|F_Sharp|F#}}==

```fsharp
let value = "100"
let fromBases = [ 2; 8; 10; 16 ]
let values = Seq.initInfinite (fun i -> value)
Seq.zip fromBases (Seq.zip values fromBases |> Seq.map (System.Convert.ToInt32))
|> Seq.iter (
    fun (fromBase, valueFromBaseX) ->
        printfn "%s in base %i is %i in base 10" value fromBase valueFromBaseX)
```

```txt
100 in base 2 is 4 in base 10
100 in base 8 is 64 in base 10
100 in base 10 is 100 in base 10
100 in base 16 is 256 in base 10
```



## Factor

Bases from 2 to 16 are supported through the generic base> word (see online docs [http://docs.factorcode.org/content/word-base__gt__,math.parser.html])
but 4 functions are defined for the most used cases:
    ( scratchpad ) "ff" hex> . ! base 16
    255
    ( scratchpad ) "777" oct> . ! base 8
    511
    ( scratchpad ) "1111" bin> . ! base 2
    15
    ( scratchpad ) "99" string>number . ! base 10
    99
Note that these words are very simple : for example, here's oct> :

```factor
IN: math.parser
: oct> ( str -- n/f ) 8 base> ; inline
```

Also, fractions are handled transparently :
    ( scratchpad ) "1+F/2" hex> .
    8+1/2
Hex floats are supported, anything else is taken as base 10 :
    ( scratchpad ) "ff.f" hex> .
    255.9375
    ( scratchpad ) "11.1101" bin> .
    11.1101

## Forth

Arbitrary base 2-36 parsing is supported by the same mechanism as [[User Input#Forth|decimal parsing]]: set the user variable BASE to the desired base, then scan the number. There are two convenience words for setting the base to DECIMAL or HEX.

```forth
: parse# ( str len -- u true | false )
   0. 2SWAP DUP >R >NUMBER NIP NIP
   R> <> DUP 0= IF NIP THEN ;

: base# ( str len base -- u true | false )
  BASE @ >R  BASE !  parse#  R> BASE ! ;
```



## Fortran

```fortran
program Example
  implicit none

  integer :: num
  character(32) :: str

  str = "0123459"
  read(str, "(i10)") num   ! Decimal
  write(*,*) num           ! Prints 123459

  str = "abcf123"
  read(str, "(z8)") num    ! Hexadecimal
  write(*,*) num           ! Prints 180154659

  str = "7651"
  read(str, "(o11)") num   ! Octal
  write(*,*) num           ! Prints 4009

  str = "1010011010"
  read(str, "(b32)") num   ! Binary
  write(*,*) num           ! Prints 666

end program
```



## FreeBASIC

FreeBASIC has built-in string to integer conversion functions which automatically recognize numbers in hexadecimal,
decimal, octal or binary format provided that they are prefixed by &H, (nothing), &O and &B respectively. Here's
an example:

```freebasic
' FB 1.05.0 Win64

Dim s(1 To 4) As String = {"&H1a", "26", "&O32", "&B11010"} '' 26 in various bases
For i As Integer = 1 To 4
  Print s(i); Tab(9); "="; CInt(s(i))
Next

Sleep
```


```txt

&H1a    = 26
26      = 26
&O32    = 26
&B11010 = 26

```



## Go


```go
package main

import (
    "fmt"
    "math/big"
    "strconv"
)

func main() {
    // package strconv:  the most common string to int conversion,
    // base 10 only.
    x, _ := strconv.Atoi("13")
    fmt.Println(x)

    // ParseInt handles arbitrary bases from 2 to 36, and returns
    // a result of the requested size (64 bits shown here.)
    // If the base argument is zero the base is determined by prefix
    // as with math/big below.
    x64, _ := strconv.ParseInt("3c2", 19, 64)
    fmt.Println(x64)

    // package fmt:  allows direct conversion from strings, standard
    // input, or from an io.Reader (file, buffer, etc) to integer types
    // for bases 2, 8, 10, and 16 or to any type that implements the
    // fmt.Scanner interface (e.g. a big.Int).
    // (Fscanf and Scanf are more common for reading from
    // an io.Reader or stdin than Sscanf for reading from strings.)
    fmt.Sscanf("1101", "%b", &x)
    fmt.Println(x)

    fmt.Sscanf("15", "%o", &x)
    fmt.Println(x)

    fmt.Sscanf("13", "%d", &x)
    fmt.Println(x)

    fmt.Sscanf("d", "%x", &x)
    fmt.Println(x)

    // package math/big:  allows conversion from string to big integer.
    // any base from 2 to 36 can be specified as second parameter.
    var z big.Int
    z.SetString("111", 3)
    fmt.Println(&z)

    // if second parameter is 0, base is determined by prefix, if any
    z.SetString("0b1101", 0) // 0b -> base 2
    fmt.Println(&z)

    z.SetString("015", 0) // 0 -> base 8
    fmt.Println(&z)

    z.SetString("13", 0) // no prefix -> base 10
    fmt.Println(&z)

    z.SetString("0xd", 0) // 0x -> base 16
    fmt.Println(&z)

    // As mentioned, a big.Int (or any type implementing fmt.Scanner)
    // can also be use with any of the fmt scanning functions.
    fmt.Sscanf("15", "%o", &z)
    fmt.Println(&z)
}
```

Output is all 13s.


## Haskell

Haskell's <tt>read</tt> can parse strings with the same prefix used for literals in Haskell (0x or 0X for hex, 0o or 0O for octal):

```haskell>Prelude
 read "123459" :: Integer
123459
Prelude> read "0xabcf123" :: Integer
180154659
Prelude> read "0o7651" :: Integer
4009
```



## HicEst


```HicEst
READ(Text="123459    ", Format="i10") dec    ! 123459
READ(Text=" abcf123  ", Format="Z10") hex    ! 180154659
READ(Text="   7651   ", Format="o10") oct    ! 4009
READ(Text=" 101011001", Format="B10.10") bin ! 345
```


=={{header|Icon}} and {{header|Unicon}}==

Icon allows numbers to be defined as 'root' + "R" + 'number', where 'root' is a base from 2 to 36, and 'number' is a string of digits or letters, using 'A' to 'Z' as appropriate for the base; case is ignored.  Strings are automatically parsed into numbers when needed, using the procedure 'integer'.


```Icon

procedure convert (str)
  write (left(str, 10) || " = " || integer(str))
end

procedure main ()
  convert (" 2r1001")
  convert (" 8r7135")
  convert ("16rABC1234")
  convert ("36r1Z")

  write ("2r1001" + "36r1Z") # shows type conversion, string->integer
end

```


Output:

```txt

 2r1001    = 9
 8r7135    = 3677
16rABC1234 = 180097588
36r1Z      = 71
80

```



## J


'''Solution 1''':
```j
   baseN=: (, 'b'&,)&.":
```

'''Solution 2''' (input sanitizing):
```j
   baseN=: 0&".@,&": 'b' , ] NB.  Use if the source of the non-decimal "numbers" is not trustworthy
```

'''Example''':
```j
   16 baseN 'abcf123'
180154659
   8 baseN '7651'
4009
   10 baseN '123459'
123459
```

'''Note''':
J also provides builtin support for numeric literals of an arbitrary base.  The format is ''radix'''''b'''''digits'' (where ''radix'' is specified in base 10). The one restriction is that you cannot use digits larger than 36 ('z'):
```j
   16babcf123 8b7651 10b123459
180154659 4009 123459
```


However you can use digits larger than the radix:


```j
   2bhelloworld
17955
```


And you can use bases where not all digits are representable:


```j
   1000bogus
24016030028
```


Letters used for digits have base 10 values ranging from 10 (a) to 35 (z).


## Java

You must know the base that the String is in before you scan it. Create a <tt>Scanner</tt> in the usual way, but then set its radix to that base (obviously, the default is 10):

```java5
Scanner sc = new Scanner(System.in); //or any other InputStream or String
sc.useRadix(base); //any number from Character.MIN_RADIX (2) to CHARACTER.MAX_RADIX (36)
sc.nextInt(); //read in a value
```

Later you can call <tt>sc.reset()</tt> or <tt>sc.useRadix(10)</tt> to undo this change.

Another option using the <tt>Integer</tt> class:

```java
int number = Integer.parseInt(stringNum, base);
```

The base here has the same restrictions as the <tt>Scanner</tt> example. A similar method is available in the <tt>Long</tt> class. Use no second argument for base 10.

If you have a prefixed string ("0x", "0X", or "#" for hex; "0" for octal; otherwise decimal), you can use the <tt>.decode()</tt> utility method to parse the number based on the base indicated by the prefix (note: this returns an Integer object, not a primitive int):

```java
Integer.decode("0xabcf123"); // hex
Integer.decode("07651");     // octal
Integer.decode("123459");    // decimal
```

<tt>Long</tt>, <tt>Short</tt>, and <tt>Byte</tt> also have a <tt>.decode()</tt> method, to decode to the appropriate number object type.


## JavaScript

For base 10 and 16 ("0x"-prefixed), (but not 8), it is fastest to parse strings using the unary plus (+) operator:

```javascript
+"0123459"; // 123459
+"0xabcf123"; // 180154659

// also supports negative numbers, but not for hex:
+"-0123459"; // -123459
+"-0xabcf123"; // NaN
```

See http://www.jibbering.com/faq/notes/type-conversion/#tcNumber for more information.

The <code>parseInt(''string'',''radix'')</code> core function is the reverse of the <code>''number''.toString(''radix'')</code> method.  The following is taken from [http://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Global_Functions/parseInt#Example.3a_Using_parseInt Mozilla's JavaScript 1.5 reference].

<div style='height:40ex; overflow:scroll'>The following examples all return 15:


```javascript
parseInt(" 0xF", 16);
parseInt(" F", 16);
parseInt("17", 8);
parseInt(021, 8);
parseInt("015", 10);
parseInt(15.99, 10);
parseInt("FXX123", 16);
parseInt("1111", 2);
parseInt("15*3", 10);
parseInt("15e2", 10);
parseInt("15px", 10);
parseInt("12", 13);
```


The following examples all return NaN:


```javascript
parseInt("Hello", 8); // Not a number at all
parseInt("546", 2);   // Digits are not valid for binary representations
```


The following examples all return -15:


```javascript
parseInt("-F", 16);
parseInt("-0F", 16);
parseInt("-0XF", 16);
parseInt(-10, 16);
parseInt(-15.1, 10)
parseInt(" -17", 8);
parseInt(" -15", 10);
parseInt("-1111", 2);
parseInt("-15e1", 10);
parseInt("-12", 13);
```


The following example returns 224:


```javascript
parseInt("0e0", 16);
```


Although it is optional, most implementations interpret a numeric string beginning with a leading '0' as octal. The following may have an octal result.


```javascript
parseInt("0e0"); // 0
parseInt("08"); // 0, '8' is not an octal digit.
```
</div>


## Julia


```julia
# Version 5.2
txt = "100"
for base = 2:21
    base10 = parse(Int, txt, base)
    println("String $txt in base $base is $base10 in base 10")
end

```

If not specify the base it will figure out the base from the prefix:

```julia

@show parse(Int, "123459")
@show parse(Int, "0xabcf123")
@show parse(Int, "0o7651")
@show parse(Int, "0b101011001")

```


```txt

String 100 in base 2 is 4 in base 10
String 100 in base 3 is 9 in base 10
String 100 in base 4 is 16 in base 10
String 100 in base 5 is 25 in base 10
String 100 in base 6 is 36 in base 10
String 100 in base 7 is 49 in base 10
String 100 in base 8 is 64 in base 10
String 100 in base 9 is 81 in base 10
String 100 in base 10 is 100 in base 10
String 100 in base 11 is 121 in base 10
String 100 in base 12 is 144 in base 10
String 100 in base 13 is 169 in base 10
String 100 in base 14 is 196 in base 10
String 100 in base 15 is 225 in base 10
String 100 in base 16 is 256 in base 10
String 100 in base 17 is 289 in base 10
String 100 in base 18 is 324 in base 10
String 100 in base 19 is 361 in base 10
String 100 in base 20 is 400 in base 10
String 100 in base 21 is 441 in base 10
parse(Int,"123459") = 123459
parse(Int,"0xabcf123") = 180154659
parse(Int,"0o7651") = 4009
parse(Int,"0b101011001") = 345

```



## Kotlin


```scala
// version 1.1.2

fun main(args: Array<String>) {
    val s = "100"
    val bases = intArrayOf(2, 8, 10, 16, 19, 36)
    for (base in bases)
        println("$s in base ${"%2d".format(base)} is ${s.toInt(base)}")
}
```


```txt

100 in base  2 is 4
100 in base  8 is 64
100 in base 10 is 100
100 in base 16 is 256
100 in base 19 is 361
100 in base 36 is 1296

```



## Lua

Lua supports bases between 2 and 36.

```lua
print( tonumber("123") )
print( tonumber("a5b0", 16) )
print( tonumber("011101", 2) )
print( tonumber("za3r", 36) )
```



## Mathematica


```Mathematica
19^^91g5dcg2h6da7260a9f3c4a
->123456789012345678901234567890

2^^11110001001000000
->123456
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
val = sscanf('11 11 11','%d   %o  %x')
```

Output:

```txt
val =
   11
    9
   17
```



## Nim


```nim
import strutils

echo parseInt "10"       # 10

echo parseHexInt "0x10"  # 16
echo parseHexInt "10"    # 16

echo parseOctInt "0o120" # 80
echo parseOctInt "120"   # 80
```

Output:

```txt
10
16
16
80
80
```



## OCaml

The <code>int_of_string</code> function can parse hexadecimal, octal, and binary numbers that have the same prefix that is used to specify OCaml constants ("0x", "0o", and "0b", respectively):

```ocaml
# int_of_string "123459";;
- : int = 123459
# int_of_string "0xabcf123";;
- : int = 180154659
# int_of_string "0o7651";;
- : int = 4009
# int_of_string "0b101011001";;
- : int = 345
```

The <code>Int32.of_string</code>, <code>Int64.of_string</code>, and <code>Nativeint.of_string</code> functions also can understand the above prefixes when parsing into their appropriate types.

Starting in OCaml 4.02, the <code>Big_int.big_int_of_string</code> and <code>Num.num_of_string</code> functions also understand these prefixes.

You could also use the <code>Scanf</code> module to parse un-prefixed hexadecimal, decimal, and octal numbers (binary not supported):

```ocaml
# Scanf.sscanf "123459" "%d" (fun x -> x);;
- : int = 123459
# Scanf.sscanf "abcf123" "%x" (fun x -> x);;
- : int = 180154659
# Scanf.sscanf "7651" "%o" (fun x -> x);;
- : int = 4009
```



## Oz

<code>String.toInt</code> understands the usual prefixes. If a string cannot be parsed, an exception will be thrown.

```oz
{String.toInt "42"}         %% decimal
= {String.toInt "0x2a"}     %% hexadecimal
= {String.toInt "052"}      %% octal
= {String.toInt "0b101010"} %% binary
```



## PARI/GP

Binary conversion is built in to PARI/GP, this script can convert from bases2-36 to bases 2-36. I've had help with this script at http:\\mersenneforums.org . The main flaw in this script I see is that it doesn't allow 36^x-1 type strings, I'll have to add that on later.

```parigp
convert(numb1,b1,b2)={
  my(B=["0","1","2","3","4","5","6","7","8","9","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"],a=0,c="");
  numb1=Vec(Str(numb1));
  forstep(y=#numb1,1,-1,
    for(x=1,b1,
      if(numb1[y]==B[x],
        a=a+(x-1)*b1^(#numb1-y)
      )
    )
  );
  until(a/b2==0,
    c=concat(B[a%b2+1],c);
    a=a\b2
  );
  c
};
```


Note that version 2.8.0+ supports hexadecimal (0x1ff) and binary (0b10101) inputs. Further, it can accept generic input as a vector:
```parigp
fromdigits([1,15,15],16)
```



## Perl

The <tt>hex()</tt> function parses hexadecimal strings. The <tt>oct()</tt> function parses octal strings, as well as hexadecimal, octal, or binary strings with the appropriate prefix ("0x", "0", and "0b", respectively). There is no need to parse decimal strings because in Perl decimal strings and numbers are interchangeable.

```perl
my $dec = "0123459";
my $hex_noprefix = "abcf123";
my $hex_withprefix = "0xabcf123";
my $oct_noprefix = "7651";
my $oct_withprefix = "07651";
my $bin_withprefix = "0b101011001";

print 0 + $dec, "\n";   # => 123459
print hex($hex_noprefix), "\n";    # => 180154659
print hex($hex_withprefix), "\n";    # => 180154659
print oct($hex_withprefix), "\n";    # => 180154659
print oct($oct_noprefix), "\n";    # => 4009
print oct($oct_withprefix), "\n";    # => 4009
print oct($bin_withprefix), "\n";    # => 345
# nothing for binary without prefix
```



## Perl 6

By default, all strings of digits are parsed as base 10 numbers, including those with a leading zero. Numbers with a prefix 0b, 0o, 0d or 0x are parsed as binary, octal, decimal or hexadecimal respectively.

```perl6
say 0b11011;  # -> 27
say 0o11011;  # -> 4617
say 0d11011;  # -> 11011
say 0x11011;  # -> 69649
```


Additionally, there are built-in adverbial prefix operators to parse strings of "digits" of radix 2 through radix 36 into decimal. They will fail with a runtime error if they are fed a digit that is not valid in that radix.

```perl6
my $n = '11011';

say  :2($n); # -> 27
say  :3($n); # -> 112
say  :4($n); # -> 325
say  :5($n); # -> 756
say  :6($n); # -> 1519
say  :7($n); # -> 2752
say  :8($n); # -> 4617
say  :9($n); # -> 7300
say :10($n); # -> 11011
say :11($n); # -> 15984
say :12($n); # -> 22477
say :13($n); # -> 30772
say :14($n); # -> 41175
say :15($n); # -> 54016
say :16($n); # -> 69649
say :17($n); # -> 88452
say :18($n); # -> 110827
say :19($n); # -> 137200
say :20($n); # -> 168021
say :21($n); # -> 203764
say :22($n); # -> 244927
say :23($n); # -> 292032
say :24($n); # -> 345625
say :25($n); # -> 406276
say :26($n); # -> 474579
say :27($n); # -> 551152
say :28($n); # -> 636637
say :29($n); # -> 731700
say :30($n); # -> 837031
say :31($n); # -> 953344
say :32($n); # -> 1081377
say :33($n); # -> 1221892
say :34($n); # -> 1375675
say :35($n); # -> 1543536
say :36($n); # -> 1726309
```



## Phix


```Phix
?scanf("1234","%d")
?scanf("0b10101010","%d")
?scanf("#ABCD","%d")
?scanf("#FFFFFFFF","%f")
?scanf("0xFFFFFFFF","%f")
?scanf("0o377","%o")
```

```txt

```

Note the need for %f (if you want to get an atom rather than an integer back), and double braces (it's a list of potentially several different possible result sets/interpretations), and that scanf() works best with a few literals (esp spaces but most certainly <b><i>not</i></b> radix prefixes) in the format string.
Finally note that while you can use "#DEADBEEF" (without the quotes, ie a fairly big hex number) in a source code file, the compiler will choke on "DEADBEEF", and likewise so too will scanf(), and the only way round that is to insert the right prefix at the right place.


## PHP

The <tt>hexdec(), octdec(), bindec()</tt> function parses hexadecimal, octal, and binary strings, respectively. They skip any invalid characters, so a prefix will be ignored. There is no need to parse decimal strings because in PHP decimal strings and numbers are interchangeable.

```php
<?php
echo +"0123459", "\n"; // prints 123459
echo intval("0123459"), "\n"; // prints 123459
echo hexdec("abcf123"), "\n"; // prints 180154659
echo octdec("7651"), "\n";  // prints 4009
echo bindec("101011001"), "\n"; // prints 345
?>
```


An undocumented feature of <tt>intval()</tt> is that it can parse prefixed strings when given the base 0:

```php
<?php
echo intval("123459", 0), "\n"; // prints 123459
echo intval("0xabcf123", 0), "\n"; // prints 180154659
echo intval("07651", 0), "\n";  // prints 4009
?>
```


In addition, for hexadecimals, if you have a "0x"-prefixed string, you can just use it in a numeric operation, and it gets converted to the number automatically:

```php
<?php
echo +"0xabcf123", "\n"; // prints 180154659
# This does not work for octals, however:
echo +"07651", "\n"; // prints 7651
?>
```



## PL/I


```PL/I
declare N fixed binary;
get edit (N) (A(7)); /* decimal input of 7 columns */
put skip list (N);

declare BS bit (32);
get edit (BS) (B(32)); /* Binary input of 32 binary digits. */
put skip edit (BS) (B);
```


```txt

       23
11010101010111111110000000011101

```



## PicoLisp


```PicoLisp
(de parseNumber (S Base)
   (let N 0
      (for C (chop S)
         (when (> (setq C (- (char C) `(char "0"))) 9)
            (dec 'C 39) )
         (setq N (+ C (* N Base))) )
      N ) )

(println (parseNumber "91g5dcg2h6da7260a9f3c4a" 19))
```

Output:

```txt
123456789012345678901234567890
```



## PowerShell

'''PowerShell parses an integer prefixed with "0x" as hexadecimal.  Binary and Octal conversions must use the .NET <code>[Convert]</code>. Here follows a (verbose) example:'''

```PowerShell

function Select-NumberFromString
{
    [CmdletBinding(DefaultParameterSetName="Decimal")]
    [OutputType([PSCustomObject])]
    Param
    (
        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        [string]
        $InputObject,

        [Parameter(ParameterSetName="Decimal")]
        [Alias("d","Dec")]
        [switch]
        $Decimal,

        [Parameter(ParameterSetName="Hexadecimal")]
        [Alias("h","Hex")]
        [switch]
        $Hexadecimal,

        [Parameter(ParameterSetName="Octal")]
        [Alias("o","Oct")]
        [switch]
        $Octal,

        [Parameter(ParameterSetName="Binary")]
        [Alias("b","Bin")]
        [switch]
        $Binary
    )

    Begin
    {
        switch ($PSCmdlet.ParameterSetName)
        {
            "Decimal"     {$base = 10; $pattern = '[+-]?\b[0-9]+\b'; break}
            "Hexadecimal" {$base = 16; $pattern = '\b[0-9A-F]+\b'  ; break}
            "Octal"       {$base =  8; $pattern = '\b[0-7]+\b'     ; break}
            "Binary"      {$base =  2; $pattern = '\b[01]+\b'      ; break}
            "Default"     {$base = 10; $pattern = '[+-]?\b[0-9]+\b'; break}
        }
    }
    Process
    {
        foreach ($object in $InputObject)
        {
            if ($object -match $pattern)
            {
                $string = $Matches[0]
            }
            else
            {
                $string = $null
            }


            try
            {
                $value = [Convert]::ToInt32($string, $base)
            }
            catch
            {
                $value = $null
            }

            [PSCustomObject]@{
                Number      = $value
                String      = $string
                Base        = $base
                IsNumber    = $value -is [int]
                InputString = $object
            }

        }
    }
}

```

'''Using a pretend file:'''

```PowerShell

$file = @'
John Doe abc1 K2hdystkrs
Jane Doe xyz2 Ew3jtdkufy
Joe Blow def3 Ouy1ttluyl
'@ -split [Environment]::NewLine

$file | Select-NumberFromString -Hexadecimal | Format-Table

```

```txt

Number String Base IsNumber InputString
------ ------ ---- -------- -----------
 43969 abc1     16     True John Doe abc1 K2hdystkrs
                16    False Jane Doe xyz2 Ew3jtdkufy
 57075 def3     16     True Joe Blow def3 Ouy1ttluyl

```


=={{Header|PureBasic}}==

```PureBasic
  ;Val() parses integer strings
  ; decimal numbers have no prefix, hexadecimal needs a prefix of '$', binary needs a prefix of '%'
  Val("1024102410241024")      ; => 1024102410241024
  Val("$10FFFFFFFF")           ; => 73014444031
  Val("%1000")                 ; => 8
```



## Python

The [http://docs.python.org/library/functions.html#int int] function will interpret strings as numbers expressed to some base:

```python>>>
 text = '100'
>>> for base in range(2,21):
    print ("String '%s' in base %i is  %i in base 10"
           % (text, base, int(text, base)))


String '100' in base 2 is  4 in base 10
String '100' in base 3 is  9 in base 10
String '100' in base 4 is  16 in base 10
String '100' in base 5 is  25 in base 10
String '100' in base 6 is  36 in base 10
String '100' in base 7 is  49 in base 10
String '100' in base 8 is  64 in base 10
String '100' in base 9 is  81 in base 10
String '100' in base 10 is  100 in base 10
String '100' in base 11 is  121 in base 10
String '100' in base 12 is  144 in base 10
String '100' in base 13 is  169 in base 10
String '100' in base 14 is  196 in base 10
String '100' in base 15 is  225 in base 10
String '100' in base 16 is  256 in base 10
String '100' in base 17 is  289 in base 10
String '100' in base 18 is  324 in base 10
String '100' in base 19 is  361 in base 10
String '100' in base 20 is  400 in base 10
```


In addition, if you give a base of 0, it will try to figure out the base from the prefix, with the same syntax as a numeric literal in Python:

Python 3.x and 2.6:

```txt

>>> int("123459", 0)
123459
>>> int("0xabcf123", 0)
180154659
>>> int("0o7651", 0)
4009
>>> int("0b101011001", 0)
345

```

Python 2.x:

```txt

>>> int("123459", 0)
123459
>>> int("0xabcf123", 0)
180154659
>>> int("07651", 0)
4009

```

Python 2.6 supports both the above formats, because it supports both types of literals.


## R


```R
# parse a string to decimal
as.numeric("20")    # 20
# parse a hex-string to decimal
as.numeric("0x20")  # 32
# parse a string to hexadecimal
as.hexmode(as.numeric("32")) # "20"
# parse a string to octal
as.octmode(as.numeric("20")) # "24"
```



## Racket



```Racket

#lang racket

;; Number literals can use #x, #o, and #b for different radices
(list 123 #x7B #o173 #b1111011)
;; -> '(123 123 123 123)

;; Explicit conversion of strings can use any radix up to 16
(list (string->number     "123")
      (string->number     "123" 10)
      (string->number      "7B" 16)
      (string->number      "83" 15)
      (string->number      "96" 13)
      (string->number     "173"  8)
      (string->number   "11120"  3)
      (string->number "1111011"  2))
;; -> '(123 123 123 123 123 123 123 123)

```



## REXX


```txt

  ╔══════════════════════════════════════════════════════════════════════════════════╗
  ║ In REXX, there are no  numeric-type  variables  (integer, float, real, unsigned, ║
  ║ logical, binary, complex, double, etc),  only  character.   Everything is stored ║
  ║ as a character string.   Arithmetic is done almost exactly the way a schoolchild ║
  ║ would perform it.  Putting it simply,  to add,  align the two numbers up  (right ║
  ║ justified, with the decimal being the pivot)  and add the columns up, adding the ║
  ║ carries and honoring the signs.                                                  ║
  ║                                                                                  ║
  ║ Multiplications and divisions are similarly performed.                           ║
  ╚══════════════════════════════════════════════════════════════════════════════════╝

```


```rexx
/*REXX program demonstrates REXX's ability to handle non-decimal radices*/
a=123                        /*all of these assignments are identical:  */
b='123'
c='1' || "2" || '3'
d= 1  ||  2  ||  3
e= 12        ||  3
f=120 + 3
g=substr(9912388,3,3)
h=left(123456,3)
i=right(777.123,3)
j=120 + '     3   '
k=0000000123.0000/1          /*division "normalizes the number (──► 123)*/

                             /*parsing of a  decimal number  is no      */
                             /*different then parsing a character string*/
                             /*because decimal numbers  ARE  character  */
                             /*strings.    As such, numbers may have    */
                             /*leading and/or trailing blanks, and in   */
                             /*some cases, imbedded blanks (after any   */
                             /*leading sign).                           */

aa=' 123 '                   /*AA's  exact value is different the  A,   */
                             /*but it's   numerically equal    to  A.   */
bb=123.                      /*the same can be said for the rest of 'em.*/
cc=+123
dd=' +  123'
ee=0000123
ff=1.23e+2
gg=001.23E0002
hh=1230e-1
ii=122.999999999999999999999999999999999    /*assuming NUMERIC DIGITS 9 */
jj= +++123
kk= - -123

bingoA='10101'b               /*stores a binary value. */
bingoB='10101'B               /*  B  can be uppercase. */
bingoC='1 0101'b              /*apostrophes may be used*/
bingoD="1 0101"b              /*quotes may be used.    */

hyoidA='deadbeaf'x            /*stores a hexadecimal value.*/
hyoidB="deadbeaf"X
hyoidC='dead beaf'X
hyoidD='de ad be af'X
hyoidE='dead be af'X
hyoidF='7abc'x
                              /*REXX has several built-in functions     */
                              /*(BIFs) to handle conversions of the     */
                              /*above-mentioned "number" formats.       */

cyanA=d2x(a)                  /*converts a decimal string to hexadecimal*/
cyanB=d2x(5612)               /*converts a decimal string to hexadecimal*/

cyanD=b2x(101101)             /*converts a binary  string to hexadecimal*/

cyanE=b2c(101101)             /*some REXXes support this, others don't. */
cyanF=c2b('copernicium')      /*some REXXes support this, others don't. */

cyanG=c2d('nilla')            /*converts a character string to decimal. */
cyanH=d2c(251)                /*converts a decimal number to character. */

cyanI=x2d(fab)                /*converts a hexadecimal string to decimal*/
cyanJ=x2c(fab)                /*converts a hexadecimal string to chars. */
cyanK=x2b(fab)                /*converts a hexadecimal string to binary.*/

befog=d2b(144)                /*there's no dec──►binary,  but see below.*/
unfog=b2d(101)                /*there's no bin──►decimal, but see below.*/

  do j=0  to 27               /*show some simple low-value conversions. */
  say right(j,2) 'in decimal is' d2b(j) "in binary and" d2x(j) 'in hex.'
  end   /*j*/
exit                                   /*stick a fork in it, we're done.*/
/*────────────────────────────add these subroutines to end─of─program.  */
d2b: return word(strip(x2b(d2x(arg(1))),'L',0) 0,1)  /*convert dec──►bin*/
b2d: return x2d(b2x(arg(1)))                         /*convert bin──►dec*/
b2c: return x2c(b2x(arg(1)))                         /*convert bin──►chr*/
c2b: return word(strip(x2b(c2x(arg(1))),'L',0) 0,1)  /*convert chr──►bin*/
```



## Ring


```ring

see number("0") + nl
see number("123456789") + nl
see number("-987654321") + nl

```

Output:

```txt

0
123456789
-987654321

```



## Ruby

The String class has methods to coerce a string into another form:

```ruby
dec1 = "0123459"
hex2 = "abcf123"
oct3 = "7651"
bin4 = "101011001"

p dec1.to_i   # => 123459
p hex2.hex    # => 180154659
p oct3.oct    # => 4009
# nothing for binary
```


The String class has '''to_i(base)''' method ( base : 2 .. 36 ).
Invalid characters past the end of a valid number are ignored.
(This method never raises an exception when base is valid.)

```ruby
p dec1.to_i(10)         # => 123459
p hex2.to_i(16)         # => 180154659
p oct3.to_i(8)          # => 4009
p bin4.to_i(2)          # => 345
p "xyz9".to_i(10)       # => 0  If there is not a valid letter, 0 is returned.
```


The '''Integer()''' method can parse a string, provided the string has the right prefix:

```ruby
p ((Integer(dec1) rescue nil)) # => ArgumentError: invalid value for Integer: "0123459"
p Integer(dec1.sub(/^0+/,""))  # => 123459
p Integer("0d" + dec1)         # => 123459
p Integer("0x" + hex2)         # => 180154659
p Integer("0"  + oct3)         # => 4009
p Integer("0o" + oct3)         # => 4009
p Integer("0b" + bin4)         # => 345
```


So can the <code>.to_i(0)</code> method, which never raises an exception:

```ruby
p dec1.to_i(0)      # => 5349 (which is 12345 in octal, the 9 is discarded)
p ("0d" + dec1).to_i(0)        # => 123459
p ("0x" + hex2).to_i(0)        # => 180154659
p ("0"  + oct3).to_i(0)        # => 4009
p ("0o" + oct3).to_i(0)        # => 4009
p ("0b" + bin4).to_i(0)        # => 345
```


And then there's the poorly documented Scanf module in the Ruby stdlib, that seems to wrap the matched value in an array:

```ruby
require 'scanf'
p dec1.scanf("%d")  # => [123459]
p hex2.scanf("%x")  # => [180154659]
p oct3.scanf("%o")  # => [4009]
# no scanf specifier for binary numbers.
```



## Scala


```scala
object Main extends App {

  val (s, bases) = ("100", Seq(2, 8, 10, 16, 19, 36))
  bases.foreach(base => println(f"String $s in base $base%2d is $BigInt(s, base)%5d"))
}
```


## Scheme


```scheme>
 (string->number "abcf123" 16) ; hex
180154659
> (string->number "123459" 10) ; decimal, the "10" is optional
123459
> (string->number "7651" 8) ; octal
4009
> (string->number "101011001" 2) ; binary
345
```



## Seed7

The function [http://seed7.sourceforge.net/libraries/integer.htm#integer%28in_string,in_integer%29 integer(str, radix)]
converts a numeric string, with a specified radix, to an [http://seed7.sourceforge.net/manual/types.htm#integer integer].


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  begin
    writeln(integer("0123459", 10));
    writeln(integer("abcf123", 16));
    writeln(integer("7651", 8));
    writeln(integer("1010011010", 2));
    writeln(integer("tplig0", 32));
    writeln(integer("gc0uy9", 36));
  end func;
```


```txt

123459
180154659
4009
666
1000000000
987654321

```



## Sidef


```ruby
var dec            = '0123459';
var hex_noprefix   = 'abcf123';
var hex_withprefix = '0xabcf123';
var oct_noprefix   = '7651';
var oct_withprefix = '07651';
var bin_noprefix   = '101011001';
var bin_withprefix = '0b101011001';

say dec.num;                    # => 123459
say hex_noprefix.hex;           # => 180154659
say hex_withprefix.hex;         # => 180154659
say oct_noprefix.oct;           # => 4009
say oct_withprefix.oct;         # => 4009
say bin_noprefix.bin;           # => 345
say bin_withprefix.bin;         # => 345
```



## Standard ML


```sml
- Int.fromString "0123459";
val it = SOME 123459 : int option
- StringCvt.scanString (Int.scan StringCvt.HEX) "0xabcf123";
val it = SOME 180154659 : int option
- StringCvt.scanString (Int.scan StringCvt.HEX) "abcf123";
val it = SOME 180154659 : int option
- StringCvt.scanString (Int.scan StringCvt.OCT) "7651";
val it = SOME 4009 : int option
- StringCvt.scanString (Int.scan StringCvt.BIN) "101011001";
val it = SOME 345 : int option
```



## Tcl


```tcl
package require Tcl 8.6;  # For easy scanning of binary

# The strings to parse
set dec1 "0123459"
set hex2 "abcf123"
set oct3 "7651"
set bin4 "101011001"

# Parse the numbers
scan $dec1 "%d" v1
scan $hex2 "%x" v2
scan $oct3 "%o" v3
scan $bin4 "%b" v4; # Only 8.6-specific operation; others work in all versions

# Print out what happened
puts "$dec1->$v1 $hex2->$v2 $oct3->$v3 $bin4->$v4"
```

This produces this output:

```txt
0123459->123459 abcf123->180154659 7651->4009 101011001->345
```


For a general parser up to base 36, a little function can be written:

```Tcl
proc scanbase {str base} {
   set res 0
   set digits {0 1 2 3 4 5 6 7 8 9 a b c d e f g h i j k l m n o p q r s t u v w x y z}
   foreach char [split [string tolower $str] ""] {
      set value [lsearch [lrange $digits 0 [expr {$base - 1}]] $char]
      if {$value < 0} {error "bad digit $char"}
      set res [expr {$res*$base + $value}]
   }
   return $res
}
```

Example:

```txt
% scanbase 255 19
822
% scanbase $dec1 8
bad digit 9
```



## XPL0


```XPL0
include c:\cxpl\codes;
int  N;  real R;
[Text(8, "123_456");
N:= IntIn(8);
IntOut(0, N);  CrLf(0);
Text(8, "ABcd12");
N:= HexIn(8);
HexOut(0, N);  CrLf(0);
Text(8, "-3.14159e3");
R:= RlIn(8);
RlOut(0, R);  CrLf(0);
]
```

```txt

123456
00ABCD12
-3141.59000

```



## zkl

The compiler knows numbers like 0x123, 0b1|101, 0d1_000 but the number conversion functions don't.

```zkl
fcn b2b(base){
   ns:=[20..30].pump(List,T("toString",base));
   ns.println();
   ns.pump(List,T("toInt",base)).println("\n")
}
b2b(2); b2b(10); b2b(16); b2b(19);
```

Print 20 .. 30 in binary, decimal, hex & base 19 (or any base 2 .. 32) and parse them to decimal:
```txt

L("10100","10101","10110","10111","11000","11001","11010","11011","11100","11101","11110")
L(20,21,22,23,24,25,26,27,28,29,30)

L("20","21","22","23","24","25","26","27","28","29","30")
L(20,21,22,23,24,25,26,27,28,29,30)

L("14","15","16","17","18","19","1a","1b","1c","1d","1e")
L(20,21,22,23,24,25,26,27,28,29,30)

L("11","12","13","14","15","16","17","18","19","1a","1b")
L(20,21,22,23,24,25,26,27,28,29,30)

```


