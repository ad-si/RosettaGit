+++
title = "Integer overflow"
description = ""
date = 2019-06-05T20:55:07Z
aliases = []
[extra]
id = 17806
[taxonomies]
categories = ["task"]
tags = []
+++

Some languages support one or more integer types of the underlying processor.

This integer types have fixed size;   usually   '''8'''-bit,   '''16'''-bit,   '''32'''-bit,   or   '''64'''-bit.

The integers supported by such a type can be   ''signed''   or   ''unsigned''.

Arithmetic for machine level integers can often be done by single CPU instructions.

This allows high performance and is the main reason to support machine level integers.


;Definition:
An integer overflow happens when the result of a computation does not fit into the fixed size integer.
The result can be too small or too big to be representable in the fixed size integer.


## Task

When a language has fixed size integer types, create a program that
does arithmetic computations for the fixed size integers of the language.

These computations must be done such that the result would overflow.

The program should demonstrate what the following expressions do.


For 32-bit signed integers:
{|class="wikitable"
!Expression
!Result that does not fit into a 32-bit signed integer
|-
| -(-2147483647-1)
| 2147483648
|-
| 2000000000 + 2000000000
| 4000000000
|-
| -2147483647 - 2147483647
| -4294967294
|-
| 46341 * 46341
| 2147488281
|-
| (-2147483647-1) / -1
| 2147483648
|}

For 64-bit signed integers:
{|class="wikitable"
!Expression
!Result that does not fit into a 64-bit signed integer
|-
| -(-9223372036854775807-1)
| 9223372036854775808
|-
| 5000000000000000000+5000000000000000000
| 10000000000000000000
|-
| -9223372036854775807 - 9223372036854775807
| -18446744073709551614
|-
| 3037000500 * 3037000500
| 9223372037000250000
|-
| (-9223372036854775807-1) / -1
| 9223372036854775808
|}

For 32-bit unsigned integers:
{|class="wikitable"
!Expression
!Result that does not fit into a 32-bit unsigned integer
|-
| -4294967295
| -4294967295
|-
| 3000000000 + 3000000000
| 6000000000
|-
| 2147483647 - 4294967295
| -2147483648
|-
| 65537 * 65537
| 4295098369
|}

For 64-bit unsigned integers:
{|class="wikitable"
!Expression
!Result that does not fit into a 64-bit unsigned integer
|-
| -18446744073709551615
| -18446744073709551615
|-
| 10000000000000000000 + 10000000000000000000
| 20000000000000000000
|-
| 9223372036854775807 - 18446744073709551615
| -9223372036854775808
|-
| 4294967296 * 4294967296
| 18446744073709551616
|}

When the integer overflow does trigger an exception show how the exception is caught.
When the integer overflow produces some value print it.
It should be explicitly noted when an integer overflow is not recognized and the program continues with wrong results.
This should be done for signed and unsigned integers of various sizes supported by the language.
When a language has no fixed size integer type or when no integer overflow can occur
for other reasons this should be noted.
It is okay to mention, when a language supports unlimited precision integers, but
this task is NOT the place to demonstrate the capabilities of unlimited precision integers.





## 360 Assembly

You can choose to manage or not the binary integer overflow with the program mask bits of the PSW (Program Status Word). Bit 20 enables fixed-point overflow. Two non-privileged instructions (IPM,SPM) are available for retrieving and setting the program mask of the current PSW.


If you mask, you can test it in your program:

```360asm
         L     2,=F'2147483647'   2**31-1
         L     3,=F'1'            1
         AR    2,3                add register3 to register2
         BO    OVERFLOW           branch on overflow
         ....
OVERFLOW EQU   *
```

On the other hand,
you will have the S0C8 system abend code : '''fixed point overflow exception'''
with the same program, if you unmask bit 20 by:

```360asm
         IPM   1                  Insert Program Mask
         O     1,BITFPO           unmask Fixed Overflow
         SPM   1                  Set Program Mask
         ...
         DS    0F                 alignment
BITFPO   DC    BL1'00001000'      bit20=1    [start at 16]
```



## Ada

In Ada, both predefined and user-defined integer types are in a given range, between Type'First and Type'Last, inclusive. The range of predefined types is implementation specific. When the result of a computation is out of the type's range, the program <b>does not continue with a wrong result, but</b> instead <b>raises an exception</b>.


```Ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Overflow is

   generic
      type T is Range <>;
      Name_Of_T: String;
   procedure Print_Bounds; -- first instantiate this with T, Name
                           -- then call the instantiation
   procedure Print_Bounds is
   begin
      Put_Line("   " & Name_Of_T & " " & T'Image(T'First)
		 & " .." & T'Image(T'Last));
   end Print_Bounds;

   procedure P_Int  is new Print_Bounds(Integer,      "Integer ");
   procedure P_Nat  is new Print_Bounds(Natural,      "Natural ");
   procedure P_Pos  is new Print_Bounds(Positive,     "Positive");
   procedure P_Long is new Print_Bounds(Long_Integer, "Long    ");

   type Unsigned_Byte is range 0 .. 255;
   type Signed_Byte   is range -128 .. 127;
   type Unsigned_Word is range 0 .. 2**32-1;
   type Thousand is range 0 .. 999;
   type Signed_Double is range - 2**63 .. 2**63-1;
   type Crazy is range -11 .. -3;

   procedure P_UB is new Print_Bounds(Unsigned_Byte, "U 8  ");
   procedure P_SB is new Print_Bounds(Signed_Byte, "S 8  ");
   procedure P_UW is new Print_Bounds(Unsigned_Word, "U 32 ");
   procedure P_Th is new Print_Bounds(Thousand, "Thous");
   procedure P_SD is new Print_Bounds(Signed_Double, "S 64 ");
   procedure P_Cr is new Print_Bounds(Crazy, "Crazy");

   A: Crazy := Crazy'First;

begin
   Put_Line("Predefined Types:");
   P_Int; P_Nat; P_Pos; P_Long;
   New_Line;

   Put_Line("Types defined by the user:");
   P_UB; P_SB; P_UW; P_Th; P_SD; P_Cr;
   New_Line;

   Put_Line("Forcing a variable of type Crazy to overflow:");
   loop -- endless loop
      Put("  " & Crazy'Image(A) &  "+1");
      A := A + 1; -- line 49 -- this will later raise a CONSTRAINT_ERROR
   end loop;
end Overflow;
```


```txt
Predefined Types:
   Integer  -2147483648 .. 2147483647
   Natural   0 .. 2147483647
   Positive  1 .. 2147483647
   Long     -9223372036854775808 .. 9223372036854775807

Types defined by the user:
   U 8    0 .. 255
   S 8   -128 .. 127
   U 32   0 .. 4294967295
   Thous  0 .. 999
   S 64  -9223372036854775808 .. 9223372036854775807
   Crazy -11 ..-3

Forcing a variable of type Crazy to overflow:
  -11+1  -10+1  -9+1  -8+1  -7+1  -6+1  -5+1  -4+1  -3+1

raised CONSTRAINT_ERROR : overflow.adb:49 range check failed
```



## ALGOL 68

In this instance, one must distinguish
between the language and a particular implementation of the language.
The Algol 68 Genie manual describes its behaviour thusly:

:''As mentioned, the maximum integer which a68g can represent is ''max int'' and the maximum real is ''max real''.  Addition could give a sum which exceeds those two values, which is called overflow. Algol 68 leaves such case ''[sic] ''undefined, meaning that an implementation can choose what to do.  a68g will give a runtime error in case of arithmetic overflow.''

Other implementations are at liberty to take any action they wish, including to continue silently with a "wrong" result or to throw a catchable exception (though the latter would require at least one addition to the standard prelude so as to provide the handler routine(s).

```algol68
BEGIN
   print (max int);
   print (1+max int)
END
```

```txt
+2147483647
3        print (1+max int)
                 1
a68g: runtime error: 1: INT math error (numerical result out of range) (detected in VOID closed-clause starting at "BEGIN" in line 1).
```


Note that, unlike many other languages, there is no presupposition that Algol 68 is running on a binary computer.  The second example code below shows that for variables of mode '''long int''' arithmetic is fundamentally decimal in Algol 68 Genie.


```algol68
BEGIN
   print (long max int);
   print (1+ long max int)
END

```


```txt

+99999999999999999999999999999999999
3        print (1+ long max int)
                 1
a68g: runtime error: 1: LONG INT value out of bounds (numerical result out of range) (detected in VOID closed-clause starting at "BEGIN" in line 1).
```



## AutoHotkey

Since AutoHotkey treats all integers as signed 64-bit, there is no point in demonstrating overflow with other integer types.
A AutoHotkey program does <b>not</b> recognize a signed integer overflow and the program <b>continues with wrong results</b>.

```AutoHotkey
Msgbox, % "Testing signed 64-bit integer overflow with AutoHotkey:`n" -(-9223372036854775807-1) "`n" 5000000000000000000+5000000000000000000 "`n" -9223372036854775807-9223372036854775807 "`n" 3037000500*3037000500 "`n" (-9223372036854775807-1)//-1
```

```txt
Testing signed 64-bit integer overflow with AutoHotkey:
-9223372036854775808
-8446744073709551616
2
-9223372036709301616
-9223372036854775808
```

This shows AutoHotkey does not handle integer overflow, and produces wrong results.


## Axe

Axe supports 16-bit unsigned integers. It also supports 16-bit unsigned integers, but only for comparison. The task has been modified accordingly to accommodate this.

Overflow does <b>not</b> trigger an exception (because Axe does not support exceptions). After an overflow the program <b>continues with wrong results</b> (specifically, the value modulo 65536).


```axe
Disp -65535▶Dec,i
Disp 40000+40000▶Dec,i
Disp 32767-65535▶Dec,i
Disp 257*257▶Dec,i
```


```txt
    1
14464
32768
  513
```



## Befunge

The Befunge-93 specification defines the range for stack cells as being the equivalent of a C signed long int on the same platform. However, in practice it will often depend on the underlying language of the interpreter, with Python-base implementations typically having an unlimited range, and JavaScript implementations using floating point.

For those with a finite integer range, though, the most common stack cell size is a 32 bit signed integer, which will usually just wrap when overflowing (as shown in the sample output below). That said, it's not uncommon for the last expression to produce some kind of runtime error or OS exception, frequently even crashing the interpreter itself.


```befunge
"a9jc>"*:*+*+:0\- "(-",,:.048*"="99")1 -" >:#,_$v
v,,,9"="*84 .: ,,"+"*84 .: **:*" }}" ,+55 .-\0-1<
>:+. 55+, ::0\- :. 48*"-",, \:. 48*"="9,,, -. 55v
v.*: ,,,,,999"="*84 .: ,,"*"*84 .: *+8*7"s9"  ,+<
>55+, 0\- "(",:.048*"="99"1-/)1 -">:#,_$ 1-01-/.@
```


```txt
-(-2147483647 - 1)              = -2147483648
2000000000 + 2000000000         = -294967296
-2147483647 - 2147483647        = 2
46341 * 46341                   = -2147479015
(-2147483647 - 1)/-1            = -2147483648
```



## Bracmat

Bracmat does arithmetic with arbitrary precision integer and rational numbers. No fixed size number types are supported.


## C

C supports integer types of various sizes with and without signedness.
Unsigned integer arithmetic is defined to be modulus a power of two.
An overflow for signed integer arithmetic is undefined behavior.
A C program does <b>not</b> recognize a signed integer overflow and the program <b>continues with wrong results</b>.

```c
#include <stdio.h>

int main (int argc, char *argv[])
{
  printf("Signed 32-bit:\n");
  printf("%d\n", -(-2147483647-1));
  printf("%d\n", 2000000000 + 2000000000);
  printf("%d\n", -2147483647 - 2147483647);
  printf("%d\n", 46341 * 46341);
  printf("%d\n", (-2147483647-1) / -1);
  printf("Signed 64-bit:\n");
  printf("%ld\n", -(-9223372036854775807-1));
  printf("%ld\n", 5000000000000000000+5000000000000000000);
  printf("%ld\n", -9223372036854775807 - 9223372036854775807);
  printf("%ld\n", 3037000500 * 3037000500);
  printf("%ld\n", (-9223372036854775807-1) / -1);
  printf("Unsigned 32-bit:\n");
  printf("%u\n", -4294967295U);
  printf("%u\n", 3000000000U + 3000000000U);
  printf("%u\n", 2147483647U - 4294967295U);
  printf("%u\n", 65537U * 65537U);
  printf("Unsigned 64-bit:\n");
  printf("%lu\n", -18446744073709551615LU);
  printf("%lu\n", 10000000000000000000LU + 10000000000000000000LU);
  printf("%lu\n", 9223372036854775807LU - 18446744073709551615LU);
  printf("%lu\n", 4294967296LU * 4294967296LU);
  return 0;
}
```


```txt

Signed 32-bit:
-2147483648
-294967296
2
-2147479015
-2147483648
Signed 64-bit:
-9223372036854775808
-8446744073709551616
2
-9223372036709301616
-9223372036854775808
Unsigned 32-bit:
1
1705032704
2147483648
131073
Unsigned 64-bit:
1
1553255926290448384
9223372036854775808
0

```



## C#

C# has 2 modes for doing arithmetic: checked and unchecked.

Constant arithmetic (i.e. compile-time) is checked by default. Since all the examples use constant expressions, all these statements would result in compile-time exceptions. To change this behaviour, the statements can be wrapped inside a block marked with the 'unchecked' keyword.

Runtime arithmetic is unchecked by default. Values that overflow will simply 'wrap around' and the program will continue with wrong results. To make C# recognize overflow and throw an OverflowException, the statements can be wrapped inside a block marked with the 'checked' keyword.

The default behavior can be changed with a compiler flag.


```c#
using System;

public class IntegerOverflow
{
    public static void Main() {
        unchecked {
            Console.WriteLine("For 32-bit signed integers:");
            Console.WriteLine(-(-2147483647 - 1));
            Console.WriteLine(2000000000 + 2000000000);
            Console.WriteLine(-2147483647 - 2147483647);
            Console.WriteLine(46341 * 46341);
            Console.WriteLine((-2147483647 - 1) / -1);
            Console.WriteLine();

            Console.WriteLine("For 64-bit signed integers:");
            Console.WriteLine(-(-9223372036854775807L - 1));
            Console.WriteLine(5000000000000000000L + 5000000000000000000L);
            Console.WriteLine(-9223372036854775807L - 9223372036854775807L);
            Console.WriteLine(3037000500L * 3037000500L);
            Console.WriteLine((-9223372036854775807L - 1) / -1);
            Console.WriteLine();

            Console.WriteLine("For 32-bit unsigned integers:");
            //Negating a 32-bit unsigned integer will convert it to a signed 64-bit integer.
            Console.WriteLine(-4294967295U);
            Console.WriteLine(3000000000U + 3000000000U);
            Console.WriteLine(2147483647U - 4294967295U);
            Console.WriteLine(65537U * 65537U);
            Console.WriteLine();

            Console.WriteLine("For 64-bit unsigned integers:");
            // The - operator cannot be applied to 64-bit unsigned integers; it will always give a compile-time error.
            //Console.WriteLine(-18446744073709551615UL);
            Console.WriteLine(10000000000000000000UL + 10000000000000000000UL);
            Console.WriteLine(9223372036854775807UL - 18446744073709551615UL);
            Console.WriteLine(4294967296UL * 4294967296UL);
            Console.WriteLine();
        }

        int i = 2147483647;
        Console.WriteLine(i + 1);
        try {
            checked { Console.WriteLine(i + 1); }
        } catch (OverflowException) {
            Console.WriteLine("Overflow!");
        }
    }

}
```

```txt

For 32-bit signed integers:
-2147483648
-294967296
2
-2147479015
-2147483648

For 64-bit signed integers:
-9223372036854775808
-8446744073709551616
2
-9223372036709301616
-9223372036854775808

For 32-bit unsigned integers:
-4294967295
1705032704
2147483648
131073

For 64-bit unsigned integers:
1553255926290448384
9223372036854775808
0

-2147483648
Overflow!
```



## C++

Same as C, except that if <code>std::numeric_limits<IntegerType>::is_modulo</code> is <code>true</code>, then the type <code>IntegerType</code> uses modulo arithmetic (the behavior is defined), even if it is a signed type.
A C++ program does <b>not</b> recognize a signed integer overflow and the program <b>continues with wrong results</b>.

```cpp
#include <iostream>
#include <cstdint>
#include <limits>

int main (int argc, char *argv[])
{
  std::cout << std::boolalpha
  << std::numeric_limits<std::int32_t>::is_modulo << '\n'
  << std::numeric_limits<std::uint32_t>::is_modulo << '\n' // always true
  << std::numeric_limits<std::int64_t>::is_modulo << '\n'
  << std::numeric_limits<std::uint64_t>::is_modulo << '\n' // always true
  << "Signed 32-bit:\n"
    << -(-2147483647-1) << '\n'
    << 2000000000 + 2000000000 << '\n'
    << -2147483647 - 2147483647 << '\n'
    << 46341 * 46341 << '\n'
    << (-2147483647-1) / -1 << '\n'
  << "Signed 64-bit:\n"
    << -(-9223372036854775807-1) << '\n'
    << 5000000000000000000+5000000000000000000 << '\n'
    << -9223372036854775807 - 9223372036854775807 << '\n'
    << 3037000500 * 3037000500 << '\n'
    << (-9223372036854775807-1) / -1 << '\n'
  << "Unsigned 32-bit:\n"
    << -4294967295U << '\n'
    << 3000000000U + 3000000000U << '\n'
    << 2147483647U - 4294967295U << '\n'
    << 65537U * 65537U << '\n'
  << "Unsigned 64-bit:\n"
    << -18446744073709551615LU << '\n'
    << 10000000000000000000LU + 10000000000000000000LU << '\n'
    << 9223372036854775807LU - 18446744073709551615LU << '\n'
    << 4294967296LU * 4294967296LU << '\n';
  return 0;
}
```


```txt

true
true
true
true
Signed 32-bit:
-2147483648
-294967296
2
-2147479015
-2147483648
Signed 64-bit:
-9223372036854775808
-8446744073709551616
2
-9223372036709301616
-9223372036854775808
Unsigned 32-bit:
1
1705032704
2147483648
131073
Unsigned 64-bit:
1
1553255926290448384
9223372036854775808
0

```



## COBOL

COBOL uses decimal arithmetic, so the examples given in the specification are not directly relevant. This program declares a variable that can store three decimal digits, and attempts to assign a four-digit number to it. The result is that the number is truncated to fit, with only the three least significant digits actually being stored; and the program then proceeds. This behaviour may sometimes be what we want.

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. PROCRUSTES-PROGRAM.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-EXAMPLE.
    05 X            PIC  999.
PROCEDURE DIVISION.
    MOVE     1002   TO   X.
    DISPLAY  X      UPON CONSOLE.
    STOP RUN.
```

```txt
002
```


<br/>'''Update:'''<br/>
COBOL is by specification designed to be safe for use in financial situations.  All standard native types are fixed size.

* <code>BINARY-CHAR [SIGNED/UNSIGNED]</code>, is 8 bits, always
* <code>BINARY-SHORT [SIGNED/UNSIGNED]</code>, is fixed at 16 bits, always
* <code>BINARY-LONG [SIGNED/UNSIGNED]</code>, is fixed at 32 bits, by spec
* <code>BINARY-DOUBLE [SIGNED/UNSIGNED]</code> is 64 bits, always
* <code>PICTURE</code> data is sized according to picture, a <code>9</code> means decimal display storage, holding one digit within the grouping.

All data types are fully specified.

All COBOL basic arithmetic operations support <code>ON SIZE ERROR</code> and <code>NOT ON SIZE ERROR</code> clauses, which trap any attempt to store invalid data, for both native and <code>PICTURE</code> types.  Programmers are free to ignore these features, but that willful ignorance is unlikely in production systems. Especially in programs destined for use in banking, government, or other industries where correctness of result is paramount.

A small example:


```cobol
       identification division.
       program-id. overflowing.

       data division.
       working-storage section.
       01 bit8-sized       usage binary-char.          *> standard
       01 bit16-sized      usage binary-short.         *> standard
       01 bit32-sized      usage binary-long.          *> standard
       01 bit64-sized      usage binary-double.        *> standard
       01 bit8-unsigned    usage binary-char unsigned. *> standard

       01 nebulous-size    usage binary-c-long.        *> extension

       01 picture-size     picture s999.               *> standard

      *> ***************************************************************
       procedure division.

      *> 32 bit signed integer
       subtract 2147483647 from zero giving bit32-sized
       display bit32-sized

       subtract 1 from bit32-sized giving bit32-sized
           ON SIZE ERROR display "32bit signed SIZE ERROR"
       end-subtract
      *> value was unchanged due to size error trap and trigger
       display bit32-sized
       display space

      *> 8 bit unsigned, size tested, invalid results discarded
       add -257 to zero giving bit8-unsigned
           ON SIZE ERROR display "bit8-unsigned SIZE ERROR"
       end-add
       display bit8-unsigned

      *> programmers can ignore the safety features
       compute bit8-unsigned = -257
       display "you asked for it: " bit8-unsigned
       display space

      *> fixed size
       move 999 to picture-size
       add 1 to picture-size
           ON SIZE ERROR display "picture-sized SIZE ERROR"
       end-add
       display picture-size

      *> programmers doing the following, inadvertently,
      *>   do not stay employed at banks for long
       move 999 to picture-size
       add 1 to picture-size
      *> intermediate goes to 1000, left end truncated on storage
       display "you asked for it: " picture-size

       add 1 to picture-size
       display "really? you want to keep doing this?: " picture-size
       display space

      *> C values are undefined by spec, only minimums givens
       display "How many bytes in a C long? "
               length of nebulous-size
               ", varies by platform"
       display "Regardless, ON SIZE ERROR will catch any invalid result"

      *> on a 64bit machine, C long of 8 bytes
       add 1 to h'ffffffffffffffff' giving nebulous-size
           ON SIZE ERROR display "binary-c-long SIZE ERROR"
       end-add
       display nebulous-size
      *> value will still be in initial state, GnuCOBOL initializes to 0
      *> value now goes to 1, no size error, that ship has sailed
       add 1 to nebulous-size
           ON SIZE ERROR display "binary-c-long size error"
       end-add
       display "error state is not persistent: ", nebulous-size

       goback.
       end program overflowing.
```


```txt
prompt$ cobc -xj overflowing.cob
-2147483647
32bit signed SIZE ERROR
-2147483647

bit8-unsigned SIZE ERROR
000
you asked for it: 001

picture-sized SIZE ERROR
+999
you asked for it: +000
really? you want to keep doing this?: +001

How many bytes in a C long? 8, varies by platform
Regardless, ON SIZE ERROR will catch any invalid result
binary-c-long SIZE ERROR
+00000000000000000000
error state is not persistent: +00000000000000000001
```



## Computer/zero Assembly

Arithmetic is performed modulo 256; overflow is not detected. This fragment:

```czasm
        LDA  ff
        ADD  one

...

ff:          255
one:         1
```

causes the accumulator to adopt the value 0. With a little care, the programmer can exploit this behaviour by treating each eight-bit word as either an unsigned byte or a signed byte using two's complement (although the instruction set does not provide explicit support for negative numbers). On the two's complement interpretation, the code given above would express the computation "–1 + 1 = 0".


## D

In D both signed and unsigned integer arithmetic is defined to be modulus a power of two. Such overflow is <b>not</b> detected at run-time and the program <b>continues with wrong results</b>.

Additionally, standard functions are available to perform arithmetic on int, long, uint, ulong values that modify a boolean value to signal when an overflow has occurred.


```d
void main() @safe {
    import std.stdio;

    writeln("Signed 32-bit:");
    writeln(-(-2_147_483_647 - 1));
    writeln(2_000_000_000 + 2_000_000_000);
    writeln(-2147483647 - 2147483647);
    writeln(46_341 * 46_341);
    writeln((-2_147_483_647 - 1) / -1);

    writeln("\nSigned 64-bit:");
    writeln(-(-9_223_372_036_854_775_807 - 1));
    writeln(5_000_000_000_000_000_000 + 5_000_000_000_000_000_000);
    writeln(-9_223_372_036_854_775_807 - 9_223_372_036_854_775_807);
    writeln(3_037_000_500 * 3_037_000_500);
    writeln((-9_223_372_036_854_775_807 - 1) / -1);

    writeln("\nUnsigned 32-bit:");
    writeln(-4_294_967_295U);
    writeln(3_000_000_000U + 3_000_000_000U);
    writeln(2_147_483_647U - 4_294_967_295U);
    writeln(65_537U * 65_537U);

    writeln("\nUnsigned 64-bit:");
    writeln(-18_446_744_073_709_551_615UL);
    writeln(10_000_000_000_000_000_000UL + 10_000_000_000_000_000_000UL);
    writeln(9_223_372_036_854_775_807UL - 18_446_744_073_709_551_615UL);
    writeln(4_294_967_296UL * 4_294_967_296UL);

    import core.checkedint;
    bool overflow = false;
    // Checked signed multiplication.
    // Eventually such functions will be recognized by D compilers
    // and they will be implemented with efficient intrinsics.
    immutable r = muls(46_341, 46_341, overflow);
    writeln("\n", r, " ", overflow);
}
```

```txt
Signed 32-bit:
-2147483648
-294967296
2
-2147479015
-2147483648

Signed 64-bit:
-9223372036854775808
-8446744073709551616
2
-9223372036709301616
-9223372036854775808

Unsigned 32-bit:
1
1705032704
2147483648
131073

Unsigned 64-bit:
1
1553255926290448384
9223372036854775808
0

-2147479015 true
```



## Clojure

Clojure supports Java's primitive integers, int (32-bit signed) and long (64-bit signed).  However, Clojure automatically promotes the smaller int to a 64-bit long internally, so no 32-bit integer overflow issues can occur.   For more information, see the [http://dev.clojure.org/display/design/Documentation+for+1.3+Numerics documentation].

By default, Clojure throws Exceptions on overflow conditions:

```clojure
(* -1 (dec -9223372036854775807))
(+ 5000000000000000000 5000000000000000000)
(- -9223372036854775807 9223372036854775807)
(* 3037000500 3037000500)
```

{{out}} for all of the above statements:

```txt
ArithmeticException integer overflow  clojure.lang.Numbers.throwIntOverflow
```


If you want to silently overflow, you can set the special *unchecked-math* variable to true or use the special operations, unchecked-add, unchecked-multiply, etc..

```clojure
(set! *unchecked-math* true)
(* -1 (dec -9223372036854775807)) ;=> -9223372036854775808
(+ 5000000000000000000 5000000000000000000) ;=> -8446744073709551616
(- -9223372036854775807 9223372036854775807) ;=> 2
(* 3037000500 3037000500) ;=> -9223372036709301616
; Note: The following division will currently silently overflow regardless of *unchecked-math*
; See: http://dev.clojure.org/jira/browse/CLJ-1253
(/ (dec -9223372036854775807) -1) ;=> -9223372036854775808
```


Clojure supports an arbitrary precision integer, BigInt and alternative math operators suffixed with an apostrophe: +', -', *', inc', and dec'. These operators auto-promote to BigInt upon overflow.



## Fortran

The Fortran standard does not specify the behaviour of program during integer overflow, so it depends on compiler implementation. Intel Fortran compiler does not have integer overflow detection. GNU gfortran runs some limited checks during compilations.  The standard's model of integers is symmetric around zero, and using intrinsic function huge(my_integer) one can only discover the maximal number for kind of integer my_integer but cannot go beyond that.


## FreeBASIC

For the 64-bit integer type a FreeBASIC program does <b>not</b> recognize a signed integer overflow and the program <b>continues with wrong results</b>.

```c
#include <stdio.h>


```freebasic
' FB 1.05.0 Win64

' The suffixes L, LL, UL and ULL are added to the numbers to make it
' clear to the compiler that they are to be treated as:
' signed 4 byte, signed 8 byte, unsigned 4 byte and unsigned 8 byte
' integers, respectively.

' Integer types in FB are freely convertible to each other.
' In general if the result of a computation would otherwise overflow
' it is converted to a higher integer type.

' Consequently, although the calculations are the same as the C example,
' the results for the 32-bit integers are arithmetically correct (and different
' therefore from the C results) because they are converted to 8 byte integers.

' However, as 8 byte integers are the largest integral type, no higher conversions are
' possible and so the results 'wrap round'. The 64-bit results are therefore the
' same as the C examples except the one where the compiler warns that there is an overflow
' which, frankly, I don't understand.

Print "Signed 32-bit:"
Print -(-2147483647L-1L)
Print 2000000000L + 2000000000L
Print -2147483647L - 2147483647L
Print 46341L * 46341L
Print (-2147483647L-1L) \ -1L
Print
Print "Signed 64-bit:"
Print -(-9223372036854775807LL-1LL)
Print 5000000000000000000LL + 5000000000000000000LL
Print -9223372036854775807LL - 9223372036854775807LL
Print 3037000500LL * 3037000500LL
Print (-9223372036854775807LL - 1LL) \ -1LL  ' compiler warning : Overflow in constant conversion
Print
Print "Unsigned 32-bit:"
Print -4294967295UL
Print 3000000000UL + 3000000000UL
Print 2147483647UL - 4294967295UL
Print 65537UL * 65537UL
Print
Print "Unsigned 64-bit:"
Print -18446744073709551615ULL  ' compiler warning : Implicit conversion
Print 10000000000000000000ULL + 10000000000000000000ULL
Print 9223372036854775807ULL - 18446744073709551615ULL
Print 4294967296ULL * 4294967296ULL
Print
Print "Press any key to quit"
Sleep
```


```txt

Signed 32-bit:
 2147483648
 4000000000
-4294967294
 2147488281
 2147483648

Signed 64-bit:
-9223372036854775808
-8446744073709551616
 2
-9223372036709301616
 0

Unsigned 32-bit:
-4294967295
 6000000000
-2147483648
 4295098369

Unsigned 64-bit:
 1
1553255926290448384
9223372036854775808
0

```



## Frink

Frink's numerical type is designed to "do the right thing" with all mathematics.  It will not overflow, and integers can be of any size.

Frink's numerical type automatically promotes and demotes between arbitrary-size integers, arbitrary-size rational numbers, arbitrary-precision floating-point numbers, complex numbers, and arbitrary-sized intervals of real values.


## Go

[http://play.golang.org/p/jsPWC8KGzD Run this in the Go playground].
A Go program does <b>not</b> recognize an integer overflow and the program <b>continues with wrong results</b>.

```go
package main

import "fmt"

func main() {
	// Go's builtin integer types are:
	//    int,  int8,  int16,  int32,  int64
	//   uint, uint8, uint16, uint32, uint64
	//   byte, rune, uintptr
	//
	// int is either 32 or 64 bit, depending on the system
	// uintptr is large enough to hold the bit pattern of any pointer
	// byte is 8 bits like int8
	// rune is 32 bits like int32
	//
	// Overflow and underflow is silent. The math package defines a number
	// of constants that can be helpfull, e.g.:
	//    math.MaxInt64  = 1<<63 - 1
	//    math.MinInt64  = -1 << 63
	//    math.MaxUint64 = 1<<64 - 1
	//
	// The math/big package implements multi-precision
	// arithmetic (big numbers).
	//
	// In all cases assignment from one type to another requires
	// an explicit cast, even if the types are otherwise identical
	// (e.g. rune and int32 or int and either int32 or int64).
	// Casts silently truncate if required.
	//
	// Invalid:
	//    var i int  = int32(0)
	//    var r rune = int32(0)
	//    var b byte = int8(0)
	//
	// Valid:
	var i64 int64 = 42
	var i32 int32 = int32(i64)
	var i16 int16 = int16(i64)
	var i8 int8 = int8(i16)
	var i int = int(i8)
	var r rune = rune(i)
	var b byte = byte(r)
	var u64 uint64 = uint64(b)
	var u32 uint32

	//const c int = -(-2147483647 - 1) // Compiler error on 32 bit systems, ok on 64 bit
	const c = -(-2147483647 - 1) // Allowed even on 32 bit systems, c is untyped
	i64 = c
	//i32 = c                          // Compiler error
	//i32 = -(-2147483647 - 1)         // Compiler error
	i32 = -2147483647
	i32 = -(-i32 - 1)
	fmt.Println("32 bit signed integers")
	fmt.Printf("  -(-2147483647 - 1) = %d, got %d\n", i64, i32)

	i64 = 2000000000 + 2000000000
	//i32 = 2000000000 + 2000000000    // Compiler error
	i32 = 2000000000
	i32 = i32 + i32
	fmt.Printf("  2000000000 + 2000000000 = %d, got %d\n", i64, i32)
	i64 = -2147483647 - 2147483647
	i32 = 2147483647
	i32 = -i32 - i32
	fmt.Printf("  -2147483647 - 2147483647 = %d, got %d\n", i64, i32)
	i64 = 46341 * 46341
	i32 = 46341
	i32 = i32 * i32
	fmt.Printf("  46341 * 46341 = %d, got %d\n", i64, i32)
	i64 = (-2147483647 - 1) / -1
	i32 = -2147483647
	i32 = (i32 - 1) / -1
	fmt.Printf("  (-2147483647-1) / -1 = %d, got %d\n", i64, i32)

	fmt.Println("\n64 bit signed integers")
	i64 = -9223372036854775807
	fmt.Printf("  -(%d - 1): %d\n", i64, -(i64 - 1))
	i64 = 5000000000000000000
	fmt.Printf("  %d + %d: %d\n", i64, i64, i64+i64)
	i64 = 9223372036854775807
	fmt.Printf("  -%d - %d: %d\n", i64, i64, -i64-i64)
	i64 = 3037000500
	fmt.Printf("  %d * %d: %d\n", i64, i64, i64*i64)
	i64 = -9223372036854775807
	fmt.Printf("  (%d - 1) / -1: %d\n", i64, (i64-1)/-1)

	fmt.Println("\n32 bit unsigned integers:")
	//u32 = -4294967295 // Compiler error
	u32 = 4294967295
	fmt.Printf("  -%d: %d\n", u32, -u32)
	u32 = 3000000000
	fmt.Printf("  %d + %d: %d\n", u32, u32, u32+u32)
	a := uint32(2147483647)
	u32 = 4294967295
	fmt.Printf("  %d - %d: %d\n", a, u32, a-u32)
	u32 = 65537
	fmt.Printf("  %d * %d: %d\n", u32, u32, u32*u32)

	fmt.Println("\n64 bit unsigned integers:")
	u64 = 18446744073709551615
	fmt.Printf("  -%d: %d\n", u64, -u64)
	u64 = 10000000000000000000
	fmt.Printf("  %d + %d: %d\n", u64, u64, u64+u64)
	aa := uint64(9223372036854775807)
	u64 = 18446744073709551615
	fmt.Printf("  %d - %d: %d\n", aa, u64, aa-u64)
	u64 = 4294967296
	fmt.Printf("  %d * %d: %d\n", u64, u64, u64*u64)
}
```

```txt
32 bit signed integers
  -(-2147483647 - 1) = 2147483648, got -2147483646
  2000000000 + 2000000000 = 4000000000, got -294967296
  -2147483647 - 2147483647 = -4294967294, got 2
  46341 * 46341 = 2147488281, got -2147479015
  (-2147483647-1) / -1 = 2147483648, got -2147483648

64 bit signed integers
  -(-9223372036854775807 - 1): -9223372036854775808
  5000000000000000000 + 5000000000000000000: -8446744073709551616
  -9223372036854775807 - 9223372036854775807: 2
  3037000500 * 3037000500: -9223372036709301616
  (-9223372036854775807 - 1) / -1: -9223372036854775808

32 bit unsigned integers:
  -4294967295: 1
  3000000000 + 3000000000: 1705032704
  2147483647 - 4294967295: 2147483648
  65537 * 65537: 131073

64 bit unsigned integers:
  -18446744073709551615: 1
  10000000000000000000 + 10000000000000000000: 1553255926290448384
  9223372036854775807 - 18446744073709551615: 9223372036854775808
  4294967296 * 4294967296: 0
```



## Groovy

{{Trans|Java}} + assertions + ''BigInteger'' + Groovy differences


Type ''int'' is a signed 32-bit integer. Type ''long'' is a signed 64-bit integer. Type ''BigInteger'' (also in Java) is a signed unbounded integer.

Other integral types (also in Java): ''byte'' (8-bit signed), ''short'' (16-bit signed), ''char'' (16-bit signed)

Groovy does not recognize integer overflow in any bounded integral type and the program '''continues with wrong results'''. All bounded integral types use ''2's-complement'' arithmetic.


```groovy
println "\nSigned 32-bit (failed):"
assert -(-2147483647-1) != 2147483648g
println(-(-2147483647-1))
assert 2000000000 + 2000000000 != 4000000000g
println(2000000000 + 2000000000)
assert -2147483647 - 2147483647 != -4294967294g
println(-2147483647 - 2147483647)
assert 46341 * 46341 != 2147488281g
println(46341 * 46341)
//Groovy converts divisor and dividend of "/" to floating point. Use "intdiv" to remain integral
//assert (-2147483647-1) / -1 != 2147483648g
assert (-2147483647-1).intdiv(-1) != 2147483648g
println((-2147483647-1).intdiv(-1))

println "\nSigned 64-bit (passed):"
assert -(-2147483647L-1) == 2147483648g
println(-(-2147483647L-1))
assert 2000000000L + 2000000000L == 4000000000g
println(2000000000L + 2000000000L)
assert -2147483647L - 2147483647L == -4294967294g
println(-2147483647L - 2147483647L)
assert 46341L * 46341L == 2147488281g
println(46341L * 46341L)
assert (-2147483647L-1).intdiv(-1) == 2147483648g
println((-2147483647L-1).intdiv(-1))

println "\nSigned 64-bit (failed):"
assert -(-9223372036854775807L-1) != 9223372036854775808g
println(-(-9223372036854775807L-1))
assert 5000000000000000000L+5000000000000000000L != 10000000000000000000g
println(5000000000000000000L+5000000000000000000L)
assert -9223372036854775807L - 9223372036854775807L != -18446744073709551614g
println(-9223372036854775807L - 9223372036854775807L)
assert 3037000500L * 3037000500L != 9223372037000250000g
println(3037000500L * 3037000500L)
//Groovy converts divisor and dividend of "/" to floating point. Use "intdiv" to remain integral
//assert (-9223372036854775807L-1) / -1 != 9223372036854775808g
assert (-9223372036854775807L-1).intdiv(-1) != 9223372036854775808g
println((-9223372036854775807L-1).intdiv(-1))

println "\nSigned unbounded (passed):"
assert -(-2147483647g-1g) == 2147483648g
println(-(-2147483647g-1g))
assert 2000000000g + 2000000000g == 4000000000g
println(2000000000g + 2000000000g)
assert -2147483647g - 2147483647g == -4294967294g
println(-2147483647g - 2147483647g)
assert 46341g * 46341g == 2147488281g
println(46341g * 46341g)
assert (-2147483647g-1g).intdiv(-1) == 2147483648g
println((-2147483647g-1g).intdiv(-1))
assert -(-9223372036854775807g-1) == 9223372036854775808g
println(-(-9223372036854775807g-1))
assert 5000000000000000000g+5000000000000000000g == 10000000000000000000g
println(5000000000000000000g+5000000000000000000g)
assert -9223372036854775807g - 9223372036854775807g == -18446744073709551614g
println(-9223372036854775807g - 9223372036854775807g)
assert 3037000500g * 3037000500g == 9223372037000250000g
println(3037000500g * 3037000500g)
assert (-9223372036854775807g-1g).intdiv(-1) == 9223372036854775808g
println((-9223372036854775807g-1g).intdiv(-1))
```


Output:

```txt

Signed 32-bit (failed):
-2147483648
-294967296
2
-2147479015
-2147483648

Signed 64-bit (passed):
2147483648
4000000000
-4294967294
2147488281
2147483648

Signed 64-bit (failed):
-9223372036854775808
-8446744073709551616
2
-9223372036709301616
-9223372036854775808

Signed unbounded (passed):
2147483648
4000000000
-4294967294
2147488281
2147483648
9223372036854775808
10000000000000000000
-18446744073709551614
9223372037000250000
9223372036854775808
```



## Haskell

Haskell supports both fixed sized signed integers (Int) and unbounded integers (Integer). Various sizes of signed and unsigned integers are available in Data.Int and Data.Word, respectively. The Haskell 2010 Language Report explains the following: "The results of exceptional conditions (such as overflow or underflow) on the fixed-precision numeric types are undefined; an implementation may choose error (⊥, semantically), a truncated value, or a special value such as infinity, indefinite, etc" (http://www.haskell.org/definition/haskell2010.pdf Section 6.4 Paragraph 4).

```Haskell
import Data.Int
import Data.Word
import Control.Exception

f x = do
  catch (print x) (\e -> print (e :: ArithException))

main = do
  f ((- (-2147483647 - 1)) :: Int32)
  f ((2000000000 + 2000000000) :: Int32)
  f (((-2147483647) - 2147483647) :: Int32)
  f ((46341 * 46341) :: Int32)
  f ((((-2147483647) - 1) `div` (-1)) :: Int32)
  f ((- ((-9223372036854775807) - 1)) :: Int64)
  f ((5000000000000000000 + 5000000000000000000) :: Int64)
  f (((-9223372036854775807) - 9223372036854775807) :: Int64)
  f ((3037000500 * 3037000500) :: Int64)
  f ((((-9223372036854775807) - 1) `div` (-1)) :: Int64)
  f ((-4294967295) :: Word32)
  f ((3000000000 + 3000000000) :: Word32)
  f ((2147483647 - 4294967295) :: Word32)
  f ((65537 * 65537) :: Word32)
  f ((-18446744073709551615) :: Word64)
  f ((10000000000000000000 + 10000000000000000000) :: Word64)
  f ((9223372036854775807 - 18446744073709551615) :: Word64)
  f ((4294967296 * 4294967296) :: Word64)
```

```txt
-2147483648
-294967296
2
-2147479015
arithmetic overflow
-9223372036854775808
-8446744073709551616
2
-9223372036709301616
arithmetic overflow
1
1705032704
2147483648
131073
1
1553255926290448384
9223372036854775808
0
```



## J


J has both 32 bit implementations and 64 bit implementations. Integers are signed and overflow is handled by yielding a floating point result (ieee 754's 64 bit format in both implementations).

Also, negative numbers do not use - for the negative sign in J (a preceding - means to negate the argument on the right - in some cases this is the same kind of result, but in other cases it's different). Instead, use _ to denote negative numbers. Also, J does not use / for division, instead J uses % for division. With those changes, here's what the results look like in a 32 bit version of J:


```J
  -(_2147483647-1)
2.14748e9
   2000000000 + 2000000000
4e9
   _2147483647 - 2147483647
_4.29497e9
   46341 * 46341
2.14749e9
   (_2147483647-1) % -1
2.14748e9

   -(_9223372036854775807-1)
9.22337e18
   5000000000000000000+5000000000000000000
1e19
   _9223372036854775807 - 9223372036854775807
_1.84467e19
   3037000500 * 3037000500
9.22337e18
   (_9223372036854775807-1) % -1
9.22337e18

   _4294967295
_4.29497e9
   3000000000 + 3000000000
6e9
   2147483647 - 4294967295
_2.14748e9
   65537 * 65537
4.2951e9

   _18446744073709551615
_1.84467e19
   10000000000000000000 + 10000000000000000000
2e19
   9223372036854775807 - 18446744073709551615
_9.22337e18
   4294967296 * 4294967296
1.84467e19
```


And, here's what it looks like in a 64 bit version of J:


```J
   -(_2147483647-1)
2147483648
   2000000000 + 2000000000
4000000000
   _2147483647 - 2147483647
_4294967294
   46341 * 46341
2147488281
   (_2147483647-1) % -1
2.14748e9

   -(_9223372036854775807-1)
9.22337e18
   5000000000000000000+5000000000000000000
1e19
   _9223372036854775807 - 9223372036854775807
_1.84467e19
   3037000500 * 3037000500
9.22337e18
   (_9223372036854775807-1) % -1
9.22337e18

   _4294967295
_4294967295
   3000000000 + 3000000000
6000000000
   2147483647 - 4294967295
_2147483648
   65537 * 65537
4295098369

   _18446744073709551615
_1.84467e19
   10000000000000000000 + 10000000000000000000
2e19
   9223372036854775807 - 18446744073709551615
_9.22337e18
   4294967296 * 4294967296
1.84467e19
```


That said, note that the above was with default 6 digits of "printing precision". Here's how things look with that limit relaxed:

32 bit J:


```J
   -(_2147483647-1)
2147483648
   2000000000 + 2000000000
4000000000
   _2147483647 - 2147483647
_4294967294
   46341 * 46341
2147488281
   (_2147483647-1) % -1
2147483648

   -(_9223372036854775807-1)
9223372036854775800
   5000000000000000000+5000000000000000000
10000000000000000000
   _9223372036854775807 - 9223372036854775807
_18446744073709552000
   3037000500 * 3037000500
9223372037000249300
   (_9223372036854775807-1) % -1
9223372036854775800

   _4294967295
_4294967295
   3000000000 + 3000000000
6000000000
   2147483647 - 4294967295
_2147483648
   65537 * 65537
4295098369

   _18446744073709551615
_18446744073709552000
   10000000000000000000 + 10000000000000000000
20000000000000000000
   9223372036854775807 - 18446744073709551615
_9223372036854775800
   4294967296 * 4294967296
18446744073709552000
```


64 bit J:


```J
   -(_2147483647-1)
2147483648
   2000000000 + 2000000000
4000000000
   _2147483647 - 2147483647
_4294967294
   46341 * 46341
2147488281
   (_2147483647-1) % -1
2147483648

   -(_9223372036854775807-1)
9223372036854775800
   5000000000000000000+5000000000000000000
10000000000000000000
   _9223372036854775807 - 9223372036854775807
_18446744073709552000
   3037000500 * 3037000500
9223372037000249300
   (_9223372036854775807-1) % -1
9223372036854775800

   _4294967295
_4294967295
   3000000000 + 3000000000
6000000000
   2147483647 - 4294967295
_2147483648
   65537 * 65537
4295098369

   _18446744073709551615
_18446744073709552000
   10000000000000000000 + 10000000000000000000
20000000000000000000
   9223372036854775807 - 18446744073709551615
_9223372036854775800
   4294967296 * 4294967296
18446744073709552000
```


Finally, note that both versions of J support arbitrary precision integers. These are not the default, for performance reasons, but are available for cases where their performance penalty is acceptable.


## Java

The type int is a signed 32-bit integer and the type long is a 64-bit integer.
A Java program does <b>not</b> recognize an integer overflow and the program <b>continues with wrong results</b>.

```java
public class integerOverflow {

    public static void main(String[] args) {
        System.out.println("Signed 32-bit:");
        System.out.println(-(-2147483647-1));
        System.out.println(2000000000 + 2000000000);
        System.out.println(-2147483647 - 2147483647);
        System.out.println(46341 * 46341);
        System.out.println((-2147483647-1) / -1);
        System.out.println("Signed 64-bit:");
        System.out.println(-(-9223372036854775807L-1));
        System.out.println(5000000000000000000L+5000000000000000000L);
        System.out.println(-9223372036854775807L - 9223372036854775807L);
        System.out.println(3037000500L * 3037000500L);
        System.out.println((-9223372036854775807L-1) / -1);
    }

}
```


```txt

Signed 32-bit:
-2147483648
-294967296
2
-2147479015
-2147483648
Signed 64-bit:
-9223372036854775808
-8446744073709551616
2
-9223372036709301616
-9223372036854775808

```



## Julia

'''Plain Integer Types and Their Limits'''

```julia
S = subtypes(Signed)
U = subtypes(Unsigned)

println("Integer limits:")
for (s, u) in zip(S, U)
    @printf("%8s: [%s, %s]\n", s, typemin(s), typemax(s))
    @printf("%8s: [%s, %s]\n", u, typemin(u), typemax(u))
end
```

```txt
Integer limits:
  Int128: [-170141183460469231731687303715884105728, 170141183460469231731687303715884105727]
 UInt128: [0, 340282366920938463463374607431768211455]
   Int16: [-32768, 32767]
  UInt16: [0, 65535]
   Int32: [-2147483648, 2147483647]
  UInt32: [0, 4294967295]
   Int64: [-9223372036854775808, 9223372036854775807]
  UInt64: [0, 18446744073709551615]
    Int8: [-128, 127]
   UInt8: [0, 255]
```


'''Add to 1 Signed typemax'''

Julia does not throw an explicit error on integer overflow.


```julia
println("Add one to typemax:")
for t in S
    over = typemax(t) + one(t)
    @printf("%8s →  %-25s (%s)\n", t, over, typeof(over))
end
```


```txt
Add one to typemax:
  Int128 →  -170141183460469231731687303715884105728 (Int128)
   Int16 →  -32768                    (Int16)
   Int32 →  -2147483648               (Int32)
   Int64 →  -9223372036854775808      (Int64)
    Int8 →  -128                      (Int8)
```



## Kotlin

A Kotlin program does <b>not</b> recognize a signed integer overflow and the program <b>continues with wrong results</b>.


```scala
// version 1.0.5-2

/*  Kotlin (like Java) does not have unsigned integer types but we can simulate
    what would happen if we did have an unsigned 32 bit integer type using this extension function */
fun Long.toUInt(): Long = this and 0xffffffffL

@Suppress("INTEGER_OVERFLOW")
fun main(args: Array<String>) {
    // The following 'signed' computations all produce compiler warnings that they will lead to an overflow
    // which have been ignored
    println("*** Signed 32 bit integers ***\n")
    println(-(-2147483647 - 1))
    println(2000000000 + 2000000000)
    println(-2147483647 - 2147483647)
    println(46341 * 46341)
    println((-2147483647 - 1) / -1)
    println("\n*** Signed 64 bit integers ***\n")
    println(-(-9223372036854775807 - 1))
    println(5000000000000000000 + 5000000000000000000)
    println(-9223372036854775807 - 9223372036854775807)
    println(3037000500 * 3037000500)
    println((-9223372036854775807 - 1) / -1)
    // Simulated unsigned computations, no overflow warnings as we're using the Long type
    println("\n*** Unsigned 32 bit integers ***\n")
    println((-4294967295L).toUInt())
    println((3000000000L.toUInt() + 3000000000L.toUInt()).toUInt())
    println((2147483647L - 4294967295L.toUInt()).toUInt())
    println((65537L * 65537L).toUInt())
}
```


```txt

*** Signed 32 bit integers ***

-2147483648
-294967296
2
-2147479015
-2147483648

*** Signed 64 bit integers ***

-9223372036854775808
-8446744073709551616
2
-9223372036709301616
-9223372036854775808

*** Unsigned 32 bit integers ***

1
1705032704
2147483648
131073

```



## Lingo

Lingo uses 32-bit signed integers.
A Lingo program does <b>not</b> recognize a signed integer overflow and the program <b>continues with wrong results</b>.

```c
#include <stdio.h>


```lingo
put -(-2147483647-1)
-- -2147483648

put 2000000000 + 2000000000
-- -294967296

put -2147483647 - 2147483647
-- 2

put 46341 * 46341
-- -2147479015

put (-2147483647-1) / -1
--> crashes Director (jeez!)
```



## M2000 Interpreter


```M2000 Interpreter

Long A
Try ok {
      A=12121221212121
}
If not ok then Print Error$ 'Overflow Long
Def Integer B
Try ok {
      B=1212121212
}
If not ok then Print Error$  ' Overflow Integer
Def Currency C
Try ok {
      C=121212121232934392898274327927948
}
If not ok then Print Error$  ' return  Overflow Long, but is overflow Currency
Def Decimal D
Try ok {
      D=121212121232934392898274327927948
}
If not ok then Print Error$  ' return  Overflow Long, but is overflow Decimal

\\ No overflow for unsigned numbers in structs
Structure Struct {
      \\ union a1, a2| b
     {
             a1 as integer
             a2 as integer
      }
      b as long
}
\\ structures are type for Memory Block, or other sttructure
\\ we use Clear to erase internal Memory Block
Buffer Clear DataMem as Struct*20
\\ from a1 we get only the low word
Return DataMem, 0!a2:=0xBBBB, 0!a1:=0xFFFFAAAA
Print Hex$(Eval(DataMem, 0!b))="BBBBAAAA"
Print Eval(DataMem, 0!b)=Eval(DataMem, 0!a2)*0x10000+Eval(DataMem, 0!a1)

```




## Mathematica

Mathematica uses arbitrary number types. There is a $MaxNumber which is approximately 1.60521676193366172702774105306375828321e1355718576299609, but extensive research has shown it to allow numbers up to
```Mathematica
$MaxNumber +
 10^-15.954589770191003298111788092733772206160314 $MaxNumber
```
I haven't bothered testing it to any more precision. If you try to use any number above that, it returns an Overflow[].


## Nim

Note that some of the UInt64 tests not tested in the code because they raise compile-time errors (alerting the programmer to range errors).  The exception text has been included for completeness.
In the INT64 case the Nim program does <b>not</b> recognize a signed integer overflow and the program <b>continues with wrong results</b>.


```Nim
import macros, strutils

macro toIntVal(s: static[string]): untyped =
  result = parseExpr(s)

proc `/`(x,y:int64): float = return (x.float / y.float)

const
  strInt32 = ["-(-2147483647-1)",
              "2_000_000_000 + 2_000_000_000",
              "-2147483647 - 2147483647",
              "46341 * 46341",
              "(-2147483647-1) / -1"]
  shouldBInt32 = ["2147483648",
                  "4000000000",
                  "-4294967294",
                  "2147488281",
                  "2147483648"]
  strInt64 = ["-(-9_223372_036854_775807-1) ",
              "5_000000_000000_000000+5_000000_000000_000000",
              "-9_223372_036854_775807 - 9_223372_036854_775807",
              "3037_000500 * 3037_000500",
              "(-9_223372_036854_775807-1) / -1"]
  shouldBInt64 = ["9223372036854775808",
                  "10000000000000000000",
                  "-18446744073709551614",
                  "9223372037000250000",
                  "9223372036854775808"]
  strUInt32 = ["-4294967295",
              "3000_000000 +% 3000_000000",
              "2147483647 -% 4294967295",
              "65537 *% 65537"]
  shouldBUInt32 = ["-4294967295",
                  "6000000000",
                  "-2147483648",
                  "4295098369"]
  strUInt64 = ["-18446744073709551615",
               "10_000000_000000_000000 +% 10_000000_000000_000000",
               "9_223372_036854_775807 -% 18_446744_073709_551615",
               "4294967296 * 4294967296",
               "4294967296 *% 4294967296",]  # testing * and *%
  shouldBUInt64 = ["-18446744073709551615",
                   "20000000000000000000",
                   "-9223372036854775808",
                   "18446744073709551616",
                   "18446744073709551616"]
#
# use compile time macro to convert string expr to numeric value
#
var
  resInt32: seq[string] = @[$toIntVal(strInt32[0]),
                         $toIntVal(strInt32[1]),
                         $toIntVal(strInt32[2]),
                         $toIntVal(strInt32[3]),
                         $toIntVal(strInt32[4])]

  resInt64: seq[string] = @[$toIntVal(strInt64[0]),
                         $toIntVal(strInt64[1]),
                         $toIntVal(strInt64[2]),
                         $toIntVal(strInt64[3]),
                         $toIntVal(strInt64[4])]

  resUInt32: seq[string] = @[$toIntVal(strUInt32[0]),
                         $toIntVal(strUInt32[1]),
                         $toIntVal(strUInt32[2]),
                         $toIntVal(strUInt32[3])]

  resUInt64: seq[string] = @["18446744073709551615 out of valid range",
                         "10_000000_000000_000000 out of valid range",
                         "18_446744_073709_551615 out of valid range",
                         $toIntVal(strUInt64[3]),
                         $toIntVal(strUInt64[4])]

proc main() =
  # output format:
  #
  #   stringExpr -> calculatedValueAsAString (expectedValueAsAString)
  echo "-- INT32 --"
  for i in 0..<resInt32.len:
    echo align(strInt32[i], 35), " -> ", align($resInt32[i], 15), " (", $shouldBInt32[i],")"

  echo "-- INT64 --"
  for i in 0..<resInt64.len:
    echo align(strInt64[i], 55), " -> ", align($resInt64[i], 25), " (", $shouldBInt64[i],")"

  echo "-- UINT32 --"
  for i in 0..<resUInt32.len:
    echo align(strUInt32[i], 35), " -> ", align($resUInt32[i], 20), " (", $shouldBUInt32[i],")"

  echo "-- UINT64 --"
  for i in 0..<resUInt64.len:
    echo align(strUInt64[i], 55), " -> ", align($resUInt64[i], 42), " (", $shouldBUInt64[i],")"

main()

```

```txt
-- INT32 --
                   -(-2147483647-1) ->      2147483648 (2147483648)
      2_000_000_000 + 2_000_000_000 ->      4000000000 (4000000000)
           -2147483647 - 2147483647 ->     -4294967294 (-4294967294)
                      46341 * 46341 ->      2147488281 (2147488281)
               (-2147483647-1) / -1 ->    2147483648.0 (2147483648)
-- INT64 --
                          -(-9_223372_036854_775807-1)  ->      -9223372036854775808 (9223372036854775808)
          5_000000_000000_000000+5_000000_000000_000000 ->       9223372036854775807 (10000000000000000000)
       -9_223372_036854_775807 - 9_223372_036854_775807 ->      -9223372036854775808 (-18446744073709551614)
                              3037_000500 * 3037_000500 ->       9223372036854775807 (9223372037000250000)
                       (-9_223372_036854_775807-1) / -1 ->    9.223372036854776e+018 (9223372036854775808)
-- UINT32 --
                        -4294967295 ->          -4294967295 (-4294967295)
         3000_000000 +% 3000_000000 ->           6000000000 (6000000000)
           2147483647 -% 4294967295 ->          -2147483648 (-2147483648)
                     65537 *% 65537 ->           4295098369 (4295098369)
-- UINT64 --
                                  -18446744073709551615 ->    18446744073709551615 out of valid range (-18446744073709551615)
     10_000000_000000_000000 +% 10_000000_000000_000000 -> 10_000000_000000_000000 out of valid range (20000000000000000000)
      9_223372_036854_775807 -% 18_446744_073709_551615 -> 18_446744_073709_551615 out of valid range (-9223372036854775808)
                                4294967296 * 4294967296 ->                        9223372036854775807 (18446744073709551616)
                               4294967296 *% 4294967296 ->                                          0 (18446744073709551616)

```



## Oforth


Oforth handles arbitrary precision integers. There is no integer overflow nor undefined behavior (unless no more memory) :

```txt

5000000000000000000 5000000000000000000 + println
10000000000000000000
ok

```



## PARI/GP

It appears that GP offers only <code>t_INT</code> (unlimited precision) integers, but in fact machine-sized integers can be used inside a <code>Vecsmall</code>:

```parigp
Vecsmall([1])
Vecsmall([2^64])
```

```txt
%1 = Vecsmall([1])
  ***   at top-level: Vecsmall([2^64])
  ***                 ^----------------
  *** Vecsmall: overflow in t_INT-->long assignment.
  ***   Break loop: type 'break' to go back to GP prompt
```


Of course PARI can use the same techniques as [[#C|C]].


## Perl

Using Perl 5.18 on 64-bit Linux with use integer:
The Perl 5 program below does <b>not</b> recognize a signed integer overflow and the program <b>continues with wrong results</b>.

```c
#include <stdio.h>


```perl

use strict;
use warnings;
use integer;
use feature 'say';

say("Testing 64-bit signed overflow:");
say(-(-9223372036854775807-1));
say(5000000000000000000+5000000000000000000);
say(-9223372036854775807 - 9223372036854775807);
say(3037000500 * 3037000500);
say((-9223372036854775807-1) / -1);

```

```txt

Testing 64-bit signed overflow:
-9223372036854775808
-8446744073709551616
2
-9223372036709301616
-9223372036854775808

```



## Perl 6

The Perl 6 program below does <b>not</b> recognize a signed integer overflow and the program <b>continues with wrong results</b>.


```perl6
my int64 ($a, $b, $c) = 9223372036854775807, 5000000000000000000, 3037000500;
.say for -(-$a - 1), $b + $b, -$a - $a, $c * $c, (-$a - 1)/-1;
```

```txt
-9223372036854775808
-8446744073709551616
2
-9223372036709301616
9223372036854775808
```



## Phix

Phix has both 32 and 64 bit implementations. Integers are signed and limited to 31 (or 63) bits, ie
-1,073,741,824 to +1,073,741,823 (-#40000000 to #3FFFFFFF) on 32 bit, whereas on
64-bit it is -4,611,686,018,427,387,904 to +4,611,686,018,427,387,903 (-#4000000000000000 to #3FFFFFFFFFFFFFFF).
Integer overflow is handled by automatic promotion to atom (an IEEE float, 64/80 bit for the 32/64 bit implementations respectively),
which triggers a run-time type check if stored in a variable declared as integer, eg:

```Phix>integer i = 1000000000 + 1000000000</lang

```txt

C:\Program Files (x86)\Phix\test.exw:1
type check failure, i is 2000000000.0

```

The <b>overflow is automatically caught</b> and the program <b>does not continue</b> with the wrong results. You are always given the exact source file and line number that the error occurs on, and several editors, including Edita which is bundled with the compiler, will automatically jump to the source code line at fault. Alternatively you may declare a variable as atom and get the same performance for small integers, with seamless conversion to floats (with 53 or 64 bits of precision) as needed. Phix has no concept of unsigned numbers, except as user defined types that trigger errors when negative values are detected, but otherwise have the same ranges as above.


## PicoLisp

PicoLisp supports only integers of unlimited size. An overflow does not occur,
except when a number grows larger than the available memory.


## PureBasic

CPU=x64, OS=Windows7

```purebasic
#MAX_BYTE =127

#MAX_ASCII=255                  ;=MAX_CHAR Ascii-Mode

CompilerIf #PB_Compiler_Unicode=1
#MAX_CHAR =65535                ;Unicode-Mode
CompilerElse
#MAX_CHAR =255
CompilerEndIf

#MAX_WORD =32767

#MAX_UNIC =65535

#MAX_LONG =2147483647

CompilerIf #PB_Compiler_Processor=#PB_Processor_x86
#MAX_INT  =2147483647           ;32-bit CPU
CompilerElseIf #PB_Compiler_Processor=#PB_Processor_x64
#MAX_INT  =9223372036854775807  ;64-bit CPU
CompilerEndIf

#MAX_QUAD =9223372036854775807

Macro say(Type,maxv,minv,sz)
  PrintN(Type+#TAB$+RSet(Str(minv),30,Chr(32))+#TAB$+RSet(Str(maxv),30,Chr(32))+#TAB$+RSet(Str(sz),6,Chr(32))+" Byte")
EndMacro

OpenConsole()
PrintN("TYPE"+#TAB$+RSet("MIN",30,Chr(32))+#TAB$+RSet("MAX",30,Chr(32))+#TAB$+RSet("SIZE",6,Chr(32)))

Define.b b1=#MAX_BYTE, b2=b1+1
say("Byte",b1,b2,SizeOf(b1))

Define.a a1=#MAX_ASCII, a2=a1+1
say("Ascii",a1,a2,SizeOf(a1))

Define.c c1=#MAX_CHAR, c2=c1+1
say("Char",c1,c2,SizeOf(c1))

Define.w w1=#MAX_WORD, w2=w1+1
say("Word",w1,w2,SizeOf(w1))

Define.u u1=#MAX_UNIC, u2=u1+1
say("Unicode",u1,u2,SizeOf(u1))

Define.l l1=#MAX_LONG, l2=l1+1
say("Long   ",l1,l2,SizeOf(l1))

Define.i i1=#MAX_INT, i2=i1+1
say("Int",i1,i2,SizeOf(i1))

Define.q q1=#MAX_QUAD, q2=q1+1
say("Quad",q1,q2,SizeOf(q1))

Input()
```

```txt

TYPE                               MIN                             MAX    SIZE
Byte                              -128                             127       1 Byte
Ascii                                0                             255       1 Byte
Char                                 0                           65535       2 Byte
Word                            -32768                           32767       2 Byte
Unicode                              0                           65535       2 Byte
Long                       -2147483648                      2147483647       4 Byte
Int               -9223372036854775808             9223372036854775807       8 Byte
Quad              -9223372036854775808             9223372036854775807       8 Byte

```



## Python


### Python 2.X

Python 2.X has a 32 bit signed integer type called 'int' that automatically converts to type 'long' on overflow. Type long is of arbitrary precision adjusting its precision up to computer limits, as needed.


```python
Python 2.7.5 (default, May 15 2013, 22:43:36) [MSC v.1500 32 bit (Intel)] on win32
Type "copyright", "credits" or "license()" for more information.
>>> for calc in '''   -(-2147483647-1)
   2000000000 + 2000000000
   -2147483647 - 2147483647
   46341 * 46341
   (-2147483647-1) / -1'''.split('\n'):
	ans = eval(calc)
	print('Expression: %r evaluates to %s of type %s'
	      % (calc.strip(), ans, type(ans)))


Expression: '-(-2147483647-1)' evaluates to 2147483648 of type <type 'long'>
Expression: '2000000000 + 2000000000' evaluates to 4000000000 of type <type 'long'>
Expression: '-2147483647 - 2147483647' evaluates to -4294967294 of type <type 'long'>
Expression: '46341 * 46341' evaluates to 2147488281 of type <type 'long'>
Expression: '(-2147483647-1) / -1' evaluates to 2147483648 of type <type 'long'>
>>>
```



### Python 3.x

Python 3.X has the one 'int' type that is of arbitrary precision. Implementations ''may'' use 32 bit integers for speed and silently shift to arbitrary precision to avoid overflow.

```python
Python 3.4.1 (v3.4.1:c0e311e010fc, May 18 2014, 10:38:22) [MSC v.1600 32 bit (Intel)] on win32
Type "copyright", "credits" or "license()" for more information.
>>> for calc in '''   -(-2147483647-1)
   2000000000 + 2000000000
   -2147483647 - 2147483647
   46341 * 46341
   (-2147483647-1) / -1'''.split('\n'):
	ans = eval(calc)
	print('Expression: %r evaluates to %s of type %s'
	      % (calc.strip(), ans, type(ans)))


Expression: '-(-2147483647-1)' evaluates to 2147483648 of type <class 'int'>
Expression: '2000000000 + 2000000000' evaluates to 4000000000 of type <class 'int'>
Expression: '-2147483647 - 2147483647' evaluates to -4294967294 of type <class 'int'>
Expression: '46341 * 46341' evaluates to 2147488281 of type <class 'int'>
Expression: '(-2147483647-1) / -1' evaluates to 2147483648.0 of type <class 'float'>
>>>
```


Note: In Python 3.X the division operator used between two ints returns a floating point result, (as this was seen as most often required and expected in the Python community). Use <code>//</code> to get integer division.


## Racket

The 32-bit version of Racket stores internally the fixnum <code>n</code> as the signed integer <code>2n+1</code>, to distinguish it from the pointers to objects that are stored as even integers. This is invisible from inside Racket because in the usual operations when the result is not a fixnum, it's promoted to a bignum.

The effect of this representation is that the fixnums have only 31 bits available, and one of them is used for the sign. So all the examples have to be reduced to the half in order to fit into 31-bit signed values.

The unsafe operations expects fixnums in the arguments, and that the result is also a fixnum. They don't autopromote the result. They are faster but they should be used only in special cases, where the values known to be bounded. We can use them to see the behavior after an overflow. In case of a overflow they have undefined behaviour, so they may give different results or change without warning in future versions. (I don't expect that they will change soon, but there is no official guaranty.)


```Racket
#lang racket
(require racket/unsafe/ops)

(fixnum? -1073741824) ;==> #t
(fixnum? (- -1073741824)) ;==> #f

(- -1073741824) ;==> 1073741824
(unsafe-fx- 0 -1073741824) ;==> -1073741824

(+ 1000000000 1000000000) ;==> 2000000000
(unsafe-fx+ 1000000000 1000000000) ;==> -147483648

(- -1073741823 1073741823) ;==> -2147483646
(unsafe-fx- -1073741823 1073741823) ;==> 2

(* 46341 46341) ;==> 2147488281
(unsafe-fx* 46341 46341) ;==> 4633

(/ -1073741824 -1) ;==> 1073741824
(unsafe-fxquotient -1073741824 -1) ;==> -1073741824
```


The 64-bit version is similar. The fixnum are effectively 63-bits signed integers.


## REXX

The REXX language normally uses a fixed amount (but re-definable) amount of   ''decimal''   digits, the default is   '''9'''.

When the value exceeds   '''9'''   decimal digits   (or whatever was specified via the     '''numeric digits NNN'''     REXX

statement), REXX will quietly automatically change to   ''exponential format''   and
round the given number, if necessary.

For newer versions of REXX, the   '''signal on lostDigits'''   statement can be used to accomplish the same results

(for detecting a loss of significance [digits]).

```rexx
/*REXX program  displays values  when  integers  have an   overflow  or  underflow.     */
numeric digits 9                                 /*the REXX default is 9 decimal digits.*/
call  showResult(  999999997 + 1 )
call  showResult(  999999998 + 1 )
call  showResult(  999999999 + 1 )
call  showResult( -999999998 - 2 )
call  showResult(  40000 * 25000 )
call  showResult( -50000 * 20000 )
call  showResult(  50000 *-30000 )
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
showResult: procedure;  parse arg x,,_;  x=x/1                      /*normalize   X.    */
            if pos(., x)\==0  then  if x>0  then _=' [overflow]'    /*did it  overflow? */
                                            else _=' [underflow]'   /*did it underflow? */
            say right(x, 20) _                                      /*show the result.  */
            return x                                                /*return the value. */
```

'''output'''   using the default input(s):


Output note:   (as it happens, all of the results below are numerically correct)

```txt

           999999998
           999999999
       1.00000000E+9  [overflow]
      -1.00000000E+9  [underflow]
       1.00000000E+9  [overflow]
      -1.00000000E+9  [underflow]
      -1.50000000E+9  [underflow]

```



## Ruby

Ruby has unlimited precision integers.

The Integer class  is the basis for two concrete classes that hold whole numbers, Bignum and Fixnum. Bignum objects hold integers outside the range of Fixnum.
Bignum objects are created automatically when integer calculations would otherwise overflow a Fixnum.
When a calculation involving Bignum objects returns a result that will fit in a Fixnum, the result is automatically converted.

```ruby>2.1.1 :001
 a = 2**62 -1
 => 4611686018427387903
2.1.1 :002 > a.class
 => Fixnum
2.1.1 :003 > (b = a + 1).class
 => Bignum
2.1.1 :004 > (b-1).class
 => Fixnum

```

Since Ruby 2.4 these different classes have disappeared: all numbers in above code are of class Integer.


## Rust

Rust does not allow signed integer overflow, due to it being undefined behaviour. If an integer overflow can be determined at compile-time, a warning is emitted. e.g.

```txt

integer_overflow.rs:6:23: 6:48 warning: attempted to divide with overflow, #[warn(const_err)] on by default
integer_overflow.rs:6     let i32_5 : i32 = (-2_147_483_647 - 1) / -1;

```


If signed overflow occurs during program execution, a panic! is raised.

```txt

$ ./integer_overflow
thread '<main>' panicked at 'attempted to negate with overflow', integer_overflow.rs:2
note: Run with `RUST_BACKTRACE=1` for a backtrace.

```


The following code will always panic when run in any mode


```Rust

    // The following will panic!
    let i32_1 : i32 = -(-2_147_483_647 - 1);
    let i32_2 : i32 = 2_000_000_000 + 2_000_000_000;
    let i32_3 : i32 = -2_147_483_647 - 2_147_483_647;
    let i32_4 : i32 = 46341 * 46341;
    let i32_5 : i32 = (-2_147_483_647 - 1) / -1;

    // These will panic! also
    let i64_1 : i64 = -(-9_223_372_036_854_775_807 - 1);
    let i64_2 : i64 = 5_000_000_000_000_000_000 + 5_000_000_000_000_000_000;
    let i64_3 : i64 = -9_223_372_036_854_775_807 - 9_223_372_036_854_775_807;
    let i64_4 : i64 = 3_037_000_500 * 3_037_000_500;
    let i64_5 : i64 = (-9_223_372_036_854_775_807 - 1) / -1;

```


Unsigned overflow works slightly different that most languages. Compiling in debug mode will make unsigned overflow act in the same way as signed overflow. When compiling in release mode, unsigned overflow is defined as it is in C, with the usual wrapping procedures.

Note: Unary negation is not allowed on unsigned types in rust, so we simulate this via a manual two's complement calculation. (! is unary negation)


```Rust

    // The following will panic!, but only in release mode
    let u32_1 : u32 = !4_294_967_295 + 1;
    let u32_2 : u32 = 3_000_000_000 + 3_000_000_000;
    let u32_3 : u32 = 2_147_483_647 - 4_294_967_295;
    let u32_4 : u32 = 65_537 * 65_537;

    // The following panics! in release mode
    let u64_1 : u64 = !18_446_744_073_709_551_615 + 1;
    let u64_2 : u64 = 10_000_000_000_000_000_000 + 10_000_000_000_000_000_000;
    let u64_3 : u64 = 9_223_372_036_854_775_807 - 18_446_744_073_709_551_615;
    let u64_4 : u64 = 4_294_967_296 * 4_294_967_296;

    println!("{}", u32_1); // 1
    println!("{}", u32_2); // 1705032704
    println!("{}", u32_3); // 2147483648
    println!("{}", u32_4); // 131703

    println!("{}", u64_1); // 1
    println!("{}", u64_2); // 1553255926290448384
    println!("{}", u64_3); // 9223372036854775808
    println!("{}", u64_4); // 0

```


Alternatively, Rust provides a number functions on primitive integer types that do take into account overflow by default. Unlike the above unsigned values, these also work in release mode.

There are three types of functions:
<ul>
 <li>checked: Return the result or 'None' on overflow</li>
 <li>saturating: Return the result or 'MAX' for the specified type.</li>
 <li>wrapping: Return the result or the wrapped value.</li>
</ul>




```Rust

    // The following will never panic!
    println!("{:?}", 65_537u32.checked_mul(65_537));    // None
    println!("{:?}", 65_537u32.saturating_mul(65_537)); // 4294967295
    println!("{:?}", 65_537u32.wrapping_mul(65_537));   // 131073

    // These will never panic! either
    println!("{:?}", 65_537i32.checked_mul(65_537));     // None
    println!("{:?}", 65_537i32.saturating_mul(65_537));  // 2147483647
    println!("{:?}", 65_537i32.wrapping_mul(-65_537));   // -131073

```



## Scala

Math.addExact works for both 32-bit unsigned and 64-bit unsigned integers, but Java does not support signed integers.

```Scala
import Math.{addExact => ++, multiplyExact => **, negateExact => ~~, subtractExact => --}

def requireOverflow(f: => Unit) =
  try {f; println("Undetected overflow")} catch{case e: Exception => /* caught */}

println("Testing overflow detection for 32-bit unsigned integers")
requireOverflow(~~(--(~~(2147483647), 1))) // -(-2147483647-1)
requireOverflow(++(2000000000, 2000000000)) // 2000000000 + 2000000000
requireOverflow(--(~~(2147483647), 2147483647)) // -2147483647 + 2147483647
requireOverflow(**(46341, 46341)) // 46341 * 46341
requireOverflow(**(--(~~(2147483647),1), -1)) // same as (-2147483647-1) / -1

println("Test - Expect Undetected overflow:")
requireOverflow(++(1,1)) // Undetected overflow

```



## Seed7

Seed7 supports unlimited precision integers with the type [http://seed7.sourceforge.net/manual/types.htm#bigInteger bigInteger].
The type [http://seed7.sourceforge.net/manual/types.htm#integer integer] is a 64-bit signed integer type.
All computations with the type integer are checked for overflow.

```seed7
$ include "seed7_05.s7i";

const proc: writeResult (ref func integer: expression) is func
  begin
    block
      writeln(expression);
    exception
      catch OVERFLOW_ERROR: writeln("OVERFLOW_ERROR");
    end block;
  end func;

const proc: main is func
  begin
    writeResult(-(-9223372036854775807-1));
    writeResult(5000000000000000000+5000000000000000000);
    writeResult(-9223372036854775807 - 9223372036854775807);
    writeResult(3037000500 * 3037000500);
    writeResult((-9223372036854775807-1) div -1);
  end func;
```


```txt

OVERFLOW_ERROR
OVERFLOW_ERROR
OVERFLOW_ERROR
OVERFLOW_ERROR
OVERFLOW_ERROR

```



## Sidef

Sidef has unlimited precision integers.

```ruby
var (a, b, c) = (9223372036854775807, 5000000000000000000, 3037000500);
[-(-a - 1), b + b, -a - a, c * c, (-a - 1)/-1].each { say _ };
```

```txt

9223372036854775808
10000000000000000000
-18446744073709551614
9223372037000250000
9223372036854775808

```



## Swift


```Swift
// By default, all overflows in Swift result in a runtime exception, which is always fatal
// However, you can opt-in to overflow behavior with the overflow operators and continue with wrong results

var int32:Int32
var int64:Int64
var uInt32:UInt32
var uInt64:UInt64

println("signed 32-bit int:")
int32 = -1 &* (-2147483647 - 1)
println(int32)
int32 = 2000000000 &+ 2000000000
println(int32)
int32 = -2147483647 &- 2147483647
println(int32)
int32 = 46341 &* 46341
println(int32)
int32 = (-2147483647-1) &/ -1
println(int32)
println()

println("signed 64-bit int:")
int64 = -1 &* (-9223372036854775807 - 1)
println(int64)
int64 = 5000000000000000000&+5000000000000000000
println(int64)
int64 = -9223372036854775807 &- 9223372036854775807
println(int64)
int64 = 3037000500 &* 3037000500
println(int64)
int64 = (-9223372036854775807-1) &/ -1
println(int64)
println()

println("unsigned 32-bit int:")
println("-4294967295 is caught as a compile time error")
uInt32 = 3000000000 &+ 3000000000
println(uInt32)
uInt32 = 2147483647 &- 4294967295
println(uInt32)
uInt32 = 65537 &* 65537
println(uInt32)
println()

println("unsigned 64-bit int:")
println("-18446744073709551615 is caught as a compile time error")
uInt64 = 10000000000000000000 &+ 10000000000000000000
println(uInt64)
uInt64 = 9223372036854775807 &- 18446744073709551615
println(uInt64)
uInt64 = 4294967296 &* 4294967296
println(uInt64)
```

```txt

signed 32-bit int:
-2147483648
-294967296
2
-2147479015
0

signed 64-bit int:
-9223372036854775808
-8446744073709551616
2
-9223372036709301616
0

unsigned 32-bit int:
-4294967295 is caught as a compile time error
1705032704
2147483648
131073

unsigned 64-bit int:
-18446744073709551615 is caught as a compile time error
1553255926290448384
9223372036854775808
0

```



## Tcl

Tcl (since 8.5) uses logical signed integers throughout that are “large enough to hold the number you are using” (being internally anything from a single machine word up to a bignum). The only way to get 32-bit and 64-bit values in arithmetic is to apply a clamping function at appropriate points:

```tcl
proc tcl::mathfunc::clamp32 {x} {
    expr {$x<0 ? -((-$x) & 0x7fffffff) : $x & 0x7fffffff}
}
puts [expr { clamp32(2000000000 + 2000000000) }]; # ==> 1852516352
```

Tcl 8.4 used a mix of 32-bit and 64-bit numbers on 32-bit platforms and 64-bit numbers only on 64-bit platforms. Users are recommended to upgrade to avoid this complexity.


## VBScript

In VBScript, if we declare a variable, there is no type. "As Integer" or "As Long" cannot be specified. Integer is a flexible type, internally it can be Integer (Fixed 16-bits), Long (Fixed 32-bits) or Double (Floating point).
So, in VBScript is there an integer overflow? Answer: No and Yes.

- No, because 2147483647+1 is equal to 2147483648.

- Yes, because typename(2147483647)="Long" and typename(2147483648)="Double", so we have switched from fixed binary integer to double floating point. But thanks to mantissa precision there is no harm. The integer overflow is when you reach 10^15, because you are now out of the integer set : (1E+15)+1=1E+15 !?.

A good way to test integer overflow is to use the vartype() or typename() builtin functions.

```vb
'Binary Integer overflow - vbs
i=(-2147483647-1)/-1
wscript.echo i
i0=32767 	    '=32767      Integer (Fixed)  type=2
i1=2147483647	    '=2147483647 Long    (Fixed)  type=3
i2=-(-2147483647-1) '=2147483648 Double  (Float)  type=5
wscript.echo Cstr(i0) & " : " & typename(i0) & " , " & vartype(i0) & vbcrlf _
           & Cstr(i1) & " : " & typename(i1) & " , " & vartype(i1) & vbcrlf _
           & Cstr(i2) & " : " & typename(i2) & " , " & vartype(i2)
ii=2147483648-2147483647
if vartype(ii)<>3 or vartype(ii)<>2 then wscript.echo "Integer overflow type=" & typename(ii)
i1=1000000000000000-1 '1E+15-1
i2=i1+1               '1E+15
wscript.echo Cstr(i1) & " , " & Cstr(i2)
```

```txt

2147483648
32767 : Integer , 2
2147483647 : Long , 3
2147483648 : Double , 5
Integer overflow type=Double
999999999999999 , 1E+15

```



## Visual Basic

Overflow is well handled, except for a strange bug in the computation of f the constant -(-2147483648).

```vb
    'Binary Integer overflow - vb6 - 28/02/2017
    Dim i As Long '32-bit signed integer
    i = -(-2147483647 - 1)           '=-2147483648   ?! bug
    i = -Int(-2147483647 - 1)        '=-2147483648   ?! bug
    i = 0 - (-2147483647 - 1)        'Run-time error '6' : Overflow
    i = -2147483647: i = -(i - 1)    'Run-time error '6' : Overflow
    i = -(-2147483647 - 2)           'Run-time error '6' : Overflow
    i = 2147483647 + 1               'Run-time error '6' : Overflow
    i = 2000000000 + 2000000000      'Run-time error '6' : Overflow
    i = -2147483647 - 2147483647     'Run-time error '6' : Overflow
    i = 46341 * 46341                'Run-time error '6' : Overflow
    i = (-2147483647 - 1) / -1       'Run-time error '6' : Overflow

```

'''Error handling - method 1'''

```vb
    i=0
    On Error Resume Next
    i = 2147483647 + 1
    Debug.Print i                    'i=0

```

'''Error handling - method 2'''

```vb
    i=0
    On Error GoTo overflow
    i = 2147483647 + 1
    ...
overflow:
    Debug.Print "Error: " & Err.Description      '-> Error: Overflow

```

'''Error handling - method 3'''

```vb
    On Error GoTo 0
    i = 2147483647 + 1               'Run-time error '6' : Overflow
    Debug.Print i

```



## Visual Basic .NET

All the examples for the task are in error before any compilation or execution!
The visual studio editor spots the overflow errors with the message:

```txt

	Constant expression not representable in type 'Integer/Long/UInteger'

```

To have an execution time overflow we must have something else than constant expressions.

'''32-bit signed integer'''

```vbnet
        Dim i As Integer '32-bit signed integer
```

  Pre-compilation error:
    'Error: Constant expression not representable in type 'Integer'
  for:

```vbnet
        i = -(-2147483647 - 1)
        i = 0 - (-2147483647 - 1)
        i = -(-2147483647L - 1)
        i = -(-2147483647 - 2)
        i = 2147483647 + 1
        i = 2000000000 + 2000000000
        i = -2147483647 - 2147483647
        i = 46341 * 46341
        i = (-2147483647 - 1) / -1
```

  Execution error:
    'An unhandled exception of type 'System.OverflowException' occurred
    'Additional information: Arithmetic operation resulted in an overflow.
  for:

```vbnet
        i = -Int(-2147483647 - 1)
        i = -2147483647: i = -(i - 1)
```

'''32-bit unsigned integer'''

In Visual Basic .Net there is no specific UInteger constants as in C.

```vbnet
        Dim i As UInteger '32-bit unsigned integer
```

  Pre-compilation error:
    'Error: Constant expression not representable in type 'UInteger'
  for:

```vbnet
        i = -4294967295
        i = 3000000000 + 3000000000
        i = 2147483647 - 4294967295
        i = 65537 * 65537
```

  Execution error:
    'An unhandled exception of type 'System.OverflowException' occurred
    'Additional information: Arithmetic operation resulted in an overflow.
  for:

```vbnet>        i = 3000000000 : i = i + i </lang

'''64-bit signed integer'''

```vbnet
        Dim i As Long '64-bit signed integer
```

  Pre-compilation error:
    'Error: Constant expression not representable in type 'Long'
  for:

```vbnet
        i = -(-9223372036854775807 - 1)
        i = 5000000000000000000 + 5000000000000000000
        i = -9223372036854775807 - 9223372036854775807
        i = 3037000500 * 3037000500
        i = (-9223372036854775807 - 1) / -1
```

  Execution error:
    'An unhandled exception of type 'System.OverflowException' occurred
    'Additional information: Arithmetic operation resulted in an overflow.
  for:

```vbnet
        i = -9223372036854775807 : i = -(i - 1)
```


'''64-bit unsigned integer'''

In Visual Basic .Net there is no specific ULong constants as in C.
And 'Long' constants are not good enough.

```vbnet
        Dim i As ULong '64-bit unsigned integer
```

  Pre-compilation error:
    'Error: Overflow
  for:

```vbnet
        i = -18446744073709551615
        i = 10000000000000000000 + 10000000000000000000
        i = 9223372036854775807 - 18446744073709551615
```

  Pre-compilation error:
    'Error: Constant expression not representable in type 'Long'
  for:

```vbnet
        i = 4294967296 * 4294967296
```

  Execution error:
    'An unhandled exception of type 'System.OverflowException' occurred
    'Additional information: Arithmetic operation resulted in an overflow.
  for:

```vbnet
        i = 4294967296 : i = i * i
```


'''how the exception is catched'''

```vbnet
        Dim i As Integer '32-bit signed integer
        Try
            i = -2147483647 : i = -(i - 1)
            Debug.Print(i)
        Catch ex As Exception
            Debug.Print("Exception raised : " & ex.Message)
        End Try
```

```txt

Arithmetic operation resulted in an overflow.

```



## zkl

zkl uses C's 64 bit integer math and the results are OS dependent. Integers are signed. GMP can be used for big ints.
A zkl program does <b>not</b> recognize an integer overflow and the program <b>continues with wrong results</b>.

```zkl
print("Signed 64-bit:\n");
println(-(-9223372036854775807-1));
println(5000000000000000000+5000000000000000000);
println(-9223372036854775807 - 9223372036854775807);
println(3037000500 * 3037000500);
println((-9223372036854775807-1) / -1);
```

Linux/BSD/clang

```txt

Signed 64-bit:
-9223372036854775808
-8446744073709551616
2
-9223372036709301616
uncatchable floating point exception thrown by OS

```

Windows XP

```txt

Signed 64-bit:
-9223372036854775808
-8446744073709551616
2
-9223372036709301616
-9223372036854775808

```

