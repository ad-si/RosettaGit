+++
title = "Bitwise operations"
description = ""
date = 2019-10-12T08:31:23Z
aliases = []
[extra]
id = 2251
[taxonomies]
categories = []
tags = []
+++

{{task|Discrete math}}
{{basic data operation}}

;Task:
Write a routine to perform a bitwise AND, OR, and XOR on two integers, a bitwise NOT on the first integer, a left shift, right shift, right arithmetic shift, left rotate, and right rotate.

All shifts and rotates should be done on the first integer with a shift/rotate amount of the second integer.

If any operation is not available in your language, note it.





## 11l

{{trans|Kotlin}}

```11l
V x = 10
V y = 2
print(‘x       = ’x)
print(‘y       = ’y)
print(‘NOT x   = ’(-)x)
print(‘x AND y = ’(x [&] y))
print(‘x OR  y = ’(x [|] y))
print(‘x XOR y = ’(x (+) y))
print(‘x SHL y = ’(x << y))
print(‘x SHR y = ’(x >> y))
print(‘x ROL y = ’rotl(x, y))
print(‘x ROR y = ’rotr(x, y))
```

{{out}}

```txt

x       = 10
y       = 2
NOT x   = -11
x AND y = 2
x OR  y = 10
x XOR y = 8
x SHL y = 40
x SHR y = 2
x ROL y = 40
x ROR y = -2147483646

```



## 360 Assembly


```360asm
*        Bitwise operations        15/02/2017
BITWISE  CSECT
         USING  BITWISE,R13
         B      72(R15)
         DC     17F'0'
         STM    R14,R12,12(R13)
         ST     R13,4(R15)
         ST     R15,8(R13)
         LR     R13,R15
         L      R1,A
         XDECO  R1,PG
         MVC    OP,=CL7'A='
         XPRNT  OP,L'OP+L'PG
         L      R1,B
         XDECO  R1,PG
         MVC    OP,=CL7'B='
         XPRNT  OP,L'OP+L'PG
*                                  And
         L      R1,A
         N      R1,B
         XDECO  R1,PG
         MVC    OP,=C'A AND B'
         XPRNT  OP,L'OP+L'PG
*                                  Or
         L      R1,A
         O      R1,B
         XDECO  R1,PG
         MVC    OP,=C'A OR  B'
         XPRNT  OP,L'OP+L'PG
*                                  Xor
         L      R1,A
         X      R1,B
         XDECO  R1,PG
         MVC    OP,=C'A XOR B'
         XPRNT  OP,L'OP+L'PG
*                                  Not
         L      R1,A
         X      R1,=X'FFFFFFFF'    not (by xor -1)
         XDECO  R1,PG
         MVC    OP,=CL7'NOT A'
         XPRNT  OP,L'OP+L'PG
*
         MVC    A,=X'80000008'     a=-2147483640 (-2^31+8)
         L      R1,A
         XDECO  R1,PG
         MVC    OP,=CL7'A='
         XPRNT  OP,L'OP+L'PG
*                                  shift right arithmetic (on 31 bits)
         L      R1,A
         SRA    R1,3
         XDECO  R1,PG
         MVC    OP,=C'A SRA 3'
         XPRNT  OP,L'OP+L'PG
*                                  shift left arithmetic (on 31 bits)
         L      R1,A
         SLA    R1,3
         XDECO  R1,PG
         MVC    OP,=C'A SLA 3'
         XPRNT  OP,L'OP+L'PG
*                                  shift right logical (on 32 bits)
         L      R1,A
         SRL    R1,3
         XDECO  R1,PG
         MVC    OP,=C'A SRL 3'
         XPRNT  OP,L'OP+L'PG
*                                  shift left logical (on 32 bits)
         L      R1,A
         SLL    R1,3
         XDECO  R1,PG
         MVC    OP,=C'A SLL 3'
         XPRNT  OP,L'OP+L'PG
*
RETURN   L      R13,4(0,R13)
         LM     R14,R12,12(R13)
         XR     R15,R15
         BR     R14
A        DC     F'21'
B        DC     F'3'
OP       DS     CL7
PG       DS     CL12
         YREGS
         END    BITWISE
```

{{out}}

```txt

A=               21
B=                3
A AND B           1
A OR  B          23
A XOR B          22
NOT A           -22
A=      -2147483640
A SRA 3  -268435455
A SLA 3 -2147483584
A SRL 3   268435457
A SLL 3          64

```



## 8051 Assembly

Integer one is assumed to be a, integer two assumed to be b.
Each operation affects one or both operands and would not be used sequentially.
The end result of each operation resides in a.
The shift and rotate operations should likely push psw and pop psw because they affect the carry flag.

```asm
; bitwise AND
anl a, b

; bitwise OR
orl a, b

; bitwise XOR
xrl a, b

; bitwise NOT
cpl a

; left shift
inc b
rrc a
loop:
rlc a
clr c
djnz b, loop

; right shift
inc b
rlc a
loop:
rrc a
clr c
djnz b, loop

; arithmetic right shift
push 20
inc b
rlc a
mov 20.0, c
loop:
rrc a
mov c, 20.0
djnz b, loop
pop 20

; left rotate
inc b
rr a
loop:
rl a
djnz b, loop

; right rotate
inc b
rl a
loop:
rr a
djnz b, loop
```



## ABAP

This works in ABAP 7.40 and above. The missing arithmetic shift operations have been implemented with arithmetic, whereas the logical shift and the rotate operations have been implemented using the built in string functions shift_left and shift_right.


```ABAP

report z_bitwise_operations.

class hex_converter definition.
  public section.
    class-methods:
      to_binary
        importing
          hex_value           type x
        returning
          value(binary_value) type string,

      to_decimal
        importing
          hex_value            type x
        returning
          value(decimal_value) type int4.
endclass.


class hex_converter implementation.
  method to_binary.
    data(number_of_bits) = xstrlen( hex_value ) * 8.

    do number_of_bits times.
      get bit sy-index of hex_value into data(bit).

      binary_value = |{ binary_value }{ bit }|.
    enddo.
  endmethod.


  method to_decimal.
    decimal_value = hex_value.
  endmethod.
endclass.


class missing_bitwise_operations definition.
  public section.
    class-methods:
      arithmetic_shift_left
        importing
          old_value   type x
          change_with type x
        exporting
          new_value   type x,

      arithmetic_shift_right
        importing
          old_value   type x
          change_with type x
        exporting
          new_value   type x,

      logical_shift_left
        importing
          old_value   type x
          change_with type x
        exporting
          new_value   type x,

      logical_shift_right
        importing
          old_value   type x
          change_with type x
        exporting
          new_value   type x,

      rotate_left
        importing
          old_value   type x
          change_with type x
        exporting
          new_value   type x,

      rotate_right
        importing
          old_value   type x
          change_with type x
        exporting
          new_value   type x.
endclass.


class missing_bitwise_operations implementation.
  method arithmetic_shift_left.
    clear new_value.

    new_value = old_value * 2 ** change_with.
  endmethod.


  method arithmetic_shift_right.
    clear new_value.

    new_value = old_value div 2 ** change_with.
  endmethod.


  method logical_shift_left.
    clear new_value.

    data(bits) = hex_converter=>to_binary( old_value ).

    data(length_of_bit_sequence) = strlen( bits ).

    bits = shift_left(
      val = bits
      places = change_with ).

    while strlen( bits ) < length_of_bit_sequence.
      bits = |{ bits }0|.
    endwhile.

    do strlen( bits ) times.
      data(index) = sy-index - 1.

      data(current_bit) = bits+index(1).

      if current_bit eq `1`.
        set bit sy-index of new_value.
      endif.
    enddo.
  endmethod.


  method logical_shift_right.
    clear new_value.

    data(bits) = hex_converter=>to_binary( old_value ).

    data(length_of_bit_sequence) = strlen( bits ).

    bits = shift_right(
      val = bits
      places = change_with ).

    while strlen( bits ) < length_of_bit_sequence.
      bits = |0{ bits }|.
    endwhile.

    do strlen( bits ) times.
      data(index) = sy-index - 1.

      data(current_bit) = bits+index(1).

      if current_bit eq `1`.
        set bit sy-index of new_value.
      endif.
    enddo.
  endmethod.


  method rotate_left.
    clear new_value.

    data(bits) = hex_converter=>to_binary( old_value ).

    bits = shift_left(
      val = bits
      circular = change_with ).

    do strlen( bits ) times.
      data(index) = sy-index - 1.

      data(current_bit) = bits+index(1).

      if current_bit eq `1`.
        set bit sy-index of new_value.
      endif.
    enddo.
  endmethod.


  method rotate_right.
    clear new_value.

    data(bits) = hex_converter=>to_binary( old_value ).

    bits = shift_right(
      val = bits
      circular = change_with ).

    do strlen( bits ) times.
      data(index) = sy-index - 1.

      data(current_bit) = bits+index(1).

      if current_bit eq `1`.
        set bit sy-index of new_value.
      endif.
    enddo.
  endmethod.
endclass.


start-of-selection.
  data:
    a      type x length 4 value 255,
    b      type x length 4 value 2,
    result type x length 4.

  write: |a         -> { a }, { hex_converter=>to_binary( a ) }, { hex_converter=>to_decimal( a ) }|, /.

  write: |b         -> { b }, { hex_converter=>to_binary( b ) }, { hex_converter=>to_decimal( b ) }|, /.

  result = a bit-and b.
  write: |a & b     -> { result }, { hex_converter=>to_binary( result ) }, { hex_converter=>to_decimal( result ) }|, /.

  result = a bit-or b.
  write: |a \| b     -> { result }, { hex_converter=>to_binary( result ) }, { hex_converter=>to_decimal( result ) }|, /.

  result = a bit-xor b.
  write: |a ^ b     -> { result }, { hex_converter=>to_binary( result ) }, { hex_converter=>to_decimal( result ) }|, /.

  result = bit-not a.
  write: |~a        -> { result }, { hex_converter=>to_binary( result ) }, { hex_converter=>to_decimal( result ) }|, /.

  missing_bitwise_operations=>arithmetic_shift_left(
    exporting
      old_value = bit-not a
      change_with = b
    importing
      new_value = result ).
  write: |~a << b   -> { result }, { hex_converter=>to_binary( result ) }, { hex_converter=>to_decimal( result ) }|, /.

  missing_bitwise_operations=>arithmetic_shift_right(
    exporting
      old_value = bit-not a
      change_with = b
    importing
      new_value = result ).
  write: |~a >> b   -> { result }, { hex_converter=>to_binary( result ) }, { hex_converter=>to_decimal( result ) }|, /.

  missing_bitwise_operations=>logical_shift_left(
    exporting
      old_value = a
      change_with = b
    importing
      new_value = result ).
  write: |a <<< b   -> { result }, { hex_converter=>to_binary( result ) }, { hex_converter=>to_decimal( result ) }|, /.

  missing_bitwise_operations=>logical_shift_right(
    exporting
      old_value = bit-not a
      change_with = b
    importing
      new_value = result ).
  write: |~a >>> b  -> { result }, { hex_converter=>to_binary( result ) }, { hex_converter=>to_decimal( result ) }|, /.

  missing_bitwise_operations=>rotate_left(
    exporting
      old_value = bit-not a
      change_with = b
    importing
      new_value = result ).
  write: |~a rotl b -> { result }, { hex_converter=>to_binary( result ) }, { hex_converter=>to_decimal( result ) }|, /.

  missing_bitwise_operations=>rotate_right(
    exporting
      old_value = a
      change_with = b
    importing
      new_value = result ).
  write: |a rotr b  -> { result }, { hex_converter=>to_binary( result ) }, { hex_converter=>to_decimal( result ) }|, /.

```


{{output}}

```txt

a         -> 000000FF, 00000000000000000000000011111111, 255

b         -> 00000002, 00000000000000000000000000000010, 2

a & b     -> 00000002, 00000000000000000000000000000010, 2

a | b     -> 000000FF, 00000000000000000000000011111111, 255

a ^ b     -> 000000FD, 00000000000000000000000011111101, 253

~a        -> FFFFFF00, 11111111111111111111111100000000, -256

~a << b   -> FFFFFC00, 11111111111111111111110000000000, -1024

~a >> b   -> FFFFFFC0, 11111111111111111111111111000000, -64

a <<< b   -> 000003FC, 00000000000000000000001111111100, 1020

~a >>> b  -> 3FFFFFC0, 00111111111111111111111111000000, 1073741760

~a rotl b -> FFFFFC03, 11111111111111111111110000000011, -1021

a rotr b  -> C000003F, 11000000000000000000000000111111, -1073741761

```



## ACL2

Unlisted operations are not available

```Lisp
(defun bitwise (a b)
   (list (logand a b)
         (logior a b)
         (logxor a b)
         (lognot a)
         (ash a b)
         (ash a (- b))))
```



## ActionScript

ActionScript does not support bitwise rotations.

```ActionScript
function bitwise(a:int, b:int):void
{
	trace("And: ", a & b);
	trace("Or: ", a | b);
	trace("Xor: ", a ^ b);
	trace("Not: ", ~a);
	trace("Left Shift: ", a << b);
	trace("Right Shift(Arithmetic): ", a >> b);
	trace("Right Shift(Logical): ", a >>> b);
}
```



## Ada

The following program performs all required operations and prints the resulting values in base 2 for easy checking of the bit values.


```ada
with Ada.Text_IO, Interfaces;
use  Ada.Text_IO, Interfaces;

procedure Bitwise is
   subtype Byte is Unsigned_8;
   package Byte_IO is new Ada.Text_Io.Modular_IO (Byte);

   A : constant Byte    := 2#00011110#;
   B : constant Byte    := 2#11110100#;
   X : constant Byte    := 128;
   N : constant Natural := 1;
begin
   Put ("A and B = ");  Byte_IO.Put (Item => A and B, Base => 2);  New_Line;
   Put ("A or B  = ");  Byte_IO.Put (Item => A or B,  Base => 2);  New_Line;
   Put ("A xor B = ");  Byte_IO.Put (Item => A xor B, Base => 2);  New_Line;
   Put ("not A   = ");  Byte_IO.Put (Item => not A,   Base => 2);  New_Line;
   New_Line (2);
   Put_Line (Unsigned_8'Image (Shift_Left  (X, N)));
   Put_Line (Unsigned_8'Image (Shift_Right (X, N)));
   Put_Line (Unsigned_8'Image (Shift_Right_Arithmetic (X, N)));
   Put_Line (Unsigned_8'Image (Rotate_Left  (X, N)));
   Put_Line (Unsigned_8'Image (Rotate_Right (X, N)));
end Bitwise;
```



## Aikido

{{trans|Javascript}}

There is no rotate support built in to Aikido.

```aikido
function bitwise(a, b){
   println("a AND b: " + (a & b))
   println("a OR b: "+ (a | b))
   println("a XOR b: "+ (a ^ b))
   println("NOT a: " + ~a)
   println("a << b: " + (a << b)) // left shift
   println("a >> b: " + (a >> b)) // arithmetic right shift
   println("a >>> b: " + (a >>> b)) // logical right shift
}
```



## ALGOL 68

{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
<!-- {{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386}} not tested -->
Aside from decimal, [[ALGOL 68]] has 5 different alternative was of representing the number 170:
* 2r00000000000000000000000010101010, 4r0000000000002222, 8r00000000252, 16r000000aa
* and as an array of BOOL: FFFFFFFFFFFFFFFFFFFFFFFFTFTFTFTF

```algol68
main:(

  PRIO SLC = 8, SRC = 8; # SLC and SRC are not built in, define and overload them here #
  OP SLC = (BITS b, INT rotate) BITS: b SHL rotate OR b SHR ( bits width - rotate );
  OP SRC = (BITS b, INT rotate) BITS: b SHR rotate OR b SHL ( bits width - rotate );
  # SRC and SRL are non-standard, but versions are built in to ALGOL 68R's standard prelude #

  PRIO XOR = 2;
  OP XOR = (BITS p, q) BITS: p AND NOT q OR NOT p AND q;
  # XOR is non-standard, but a version is built in to ALGOL 68G's standard prelude #

  # ALGOL 68 has 5 different ways of representing a BINary BITS - Bases: 2, 4, 8, 16 and flip/flop #
  FORMAT b5 = $"2r"2r32d," 4r"4r16d," 8r"8r11d," 16r"16r8d," "gl$;
  OP BBBBB = (BITS b)[]BITS: (b,b,b,b,b);

  PROC bitwise = (BITS a, BITS b, INT shift)VOID:
  (
    printf((
      $"         bits shorths: "gxgl$, bits shorths, "1 plus the number of extra SHORT BITS types",
      $"         bits lengths: "gxgl$, bits lengths, "1 plus the number of extra LONG BITS types",
      $"             max bits: "gl$, max bits,
      $"        long max bits: "gl$, long max bits,
      $"   long long max bits: "gl$, long long max bits,
      $"           bits width: "gxgl$, bits width, "The number of CHAR required to display BITS",
      $"      long bits width: "gxgl$, long bits width, "The number of CHAR required to display LONG BITS",
      $" long long bits width: "gxgl$, long long bits width, "The number of CHAR required to display LONG LONG BITS",
      $"         bytes shorths: "gxgl$, bytes shorths, "1 plus the number of extra SHORT BYTES types",
      $"         bytes lengths: "gxgl$, bits lengths, "1 plus the number of extra LONG BYTES types",
      $"          bytes width: "gxgl$, bytes width, "The number of CHAR required to display BYTES",
      $"     long bytes width: "gxgl$, long bytes width, "The number of CHAR required to display LONG BYTES"
    ));

    printf(($" a:       "f(b5)$, BBBBB a));
    printf(($" b:       "f(b5)$, BBBBB b));
    printf(($" a AND b: "f(b5)$, BBBBB(a AND b)));
    printf(($" a OR b:  "f(b5)$, BBBBB(a OR b)));
    printf(($" a XOR b: "f(b5)$, BBBBB(a XOR b)));
    printf(($" NOT b:   "f(b5)$, BBBBB NOT a));
    printf(($" a SHL "d": "f(b5)$, shift, BBBBB(a SHL shift)));
    printf(($" a SHR "d": "f(b5)$, shift, BBBBB(a SHR shift)));

    printf(($" a SLC "d": "f(b5)$, shift, BBBBB(a SLC shift)));
    printf(($" a SRC "d": "f(b5)$, shift, BBBBB(a SRC shift)))
COMMENT with original ALGOL 68 character set;
    printf(($" a AND b: "f(b5)$, BBBBB(a ∧ b)));
    printf(($" a OR b:  "f(b5)$, BBBBB(a ∨ b)));
    printf(($" NOT b:   "f(b5)$, BBBBB ¬ a));
    printf(($" a SHL "d": "f(b5)$, shift, BBBBB(a ↑ shift)));
    printf(($" a SHR "d": "f(b5)$, shift, BBBBB(a ↓ shift)));
Also:
    printf(($" a AND b: "f(b5)$, BBBBB(a /\ b)));
    printf(($" a OR b: "f(b5)$, BBBBB(a \/ b)));
COMMENT
  );

  bitwise(BIN 255, BIN 170, 5)
# or using alternate representations for 255 and 170 in BITS #
CO
  bitwise(2r11111111,2r10101010,5);
  bitwise(4r3333,4r2222,5);
  bitwise(8r377,8r252,5);
  bitwise(16rff,16raa,5)
END CO
)
```

Output:

```txt
         bits shorths:          +1 1 plus the number of extra SHORT BITS types
         bits lengths:          +3 1 plus the number of extra LONG BITS types
             max bits: TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
        long max bits: TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
   long long max bits: TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
           bits width:         +32 The number of CHAR required to display BITS
      long bits width:        +116 The number of CHAR required to display LONG BITS
 long long bits width:        +232 The number of CHAR required to display LONG LONG BITS
         bytes shorths:          +1 1 plus the number of extra SHORT BYTES types
         bytes lengths:          +3 1 plus the number of extra LONG BYTES types
          bytes width:         +32 The number of CHAR required to display BYTES
     long bytes width:         +64 The number of CHAR required to display LONG BYTES
 a:       2r00000000000000000000000011111111 4r0000000000003333 8r00000000377 16r000000ff FFFFFFFFFFFFFFFFFFFFFFFFTTTTTTTT
 b:       2r00000000000000000000000010101010 4r0000000000002222 8r00000000252 16r000000aa FFFFFFFFFFFFFFFFFFFFFFFFTFTFTFTF
 a AND b: 2r00000000000000000000000010101010 4r0000000000002222 8r00000000252 16r000000aa FFFFFFFFFFFFFFFFFFFFFFFFTFTFTFTF
 a OR b:  2r00000000000000000000000011111111 4r0000000000003333 8r00000000377 16r000000ff FFFFFFFFFFFFFFFFFFFFFFFFTTTTTTTT
 a XOR b: 2r00000000000000000000000001010101 4r0000000000001111 8r00000000125 16r00000055 FFFFFFFFFFFFFFFFFFFFFFFFFTFTFTFT
 NOT b:   2r11111111111111111111111100000000 4r3333333333330000 8r37777777400 16rffffff00 TTTTTTTTTTTTTTTTTTTTTTTTFFFFFFFF
 a SHL 5: 2r00000000000000000001111111100000 4r0000000001333200 8r00000017740 16r00001fe0 FFFFFFFFFFFFFFFFFFFTTTTTTTTFFFFF
 a SHR 5: 2r00000000000000000000000000000111 4r0000000000000013 8r00000000007 16r00000007 FFFFFFFFFFFFFFFFFFFFFFFFFFFFFTTT
 a SLC 5: 2r00000000000000000001111111100000 4r0000000001333200 8r00000017740 16r00001fe0 FFFFFFFFFFFFFFFFFFFTTTTTTTTFFFFF
 a SRC 5: 2r11111000000000000000000000000111 4r3320000000000013 8r37000000007 16rf8000007 TTTTTFFFFFFFFFFFFFFFFFFFFFFFFTTT

```

Note that an INT can be widened into BITS, and BITS can be widened into an array of BOOL. eg:

```algol68
# unpack (widen) some data back into an a BOOL array #
INT i := 170;
BITS j := BIN i;
[bits width]BOOL k := j;

printf(($g", 8r"8r4d", "8(g)l$, i, j, k[bits width-8+1:]));

# now pack some data back into an INT #
k[bits width-8+1:] := (FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE);
j := bits pack(k);
i := ABS j;

printf(($g", 8r"8r4d", "8(g)l$, i, j, k[bits width-8+1:]))
```

Output:

```txt

       +170, 8r0252, TFTFTFTF
        +85, 8r0125, FTFTFTFT

```



## ALGOL W


```algolw
% performs bitwise and, or, not, left-shift and right shift on the integers n1 and n2 %
% Algol W does not have xor, arithmetic right shift, left rotate or right rotate      %
procedure bitOperations ( integer value n1, n2 ) ;
begin
    bits b1, b2;
    % the Algol W bitwse operations operate on bits values, so we first convert the   %
    % integers to bits values using the builtin bitstring procedure                   %
    % the results are converted back to integers using the builtin number procedure   %
    % all Algol W bits and integers are 32 bits quantities                            %
    b1 := bitstring( n1 );
    b2 := bitstring( n2 );
    % perform the operaations and display the results as integers                     %
    write( n1, " and ", n2, " = ", number( b1 and b2 ) );
    write( n1, " or  ", n2, " = ", number( b1 or  b2 ) );
    write( "                "
         ,     " not ", n1, " = ", number(    not b1 ) );
    write( n1, " shl ", n2, " = ", number( b1 shl n2 ), " (  left-shift )"  );
    write( n1, " shr ", n2, " = ", number( b1 shr n2 ), " ( right-shift )"  )
end bitOPerations ;
```



## AppleScript

Applescript has no bitwise operators. It's probably not the right tool to reach for if you need to work with bits.

If we really do need to use Applescript for bitwise operations, two immediate possibilities come to mind:

* We can use JavaScript operators through an ObjC bridge to JavaScript for Automation, or
* we can write our own functions – converting between 32-bit signed integers and corresponding lists of booleans, and performing the bitwise operations on the boolean lists before converting back to integers.


'''First option''' – 'dialling out to JavaScript for Automation':

This is feasible, (see below) subject to the limitations that:

* Javascript lacks bit rotation operators, and
* in the case of the JS left shift operator '''(<<)''' the right operand needs to be masked with '''0x1F''' (31), which is its maximum effective value.



```applescript
use AppleScript version "2.4"
use framework "Foundation"
use scripting additions

-- BIT OPERATIONS FOR APPLESCRIPT (VIA JAVASCRIPT FOR AUTOMATION)

-- bitAND :: Int -> Int -> Int
on bitAND(x, y)
    jsOp2("&", x, y)
end bitAND

-- bitOR :: Int -> Int -> Int
on bitOR(x, y)
    jsOp2("|", x, y)
end bitOR

-- bitXOr :: Int -> Int -> Int
on bitXOR(x, y)
    jsOp2("^", x, y)
end bitXOR

-- bitNOT :: Int -> Int
on bitNOT(x)
    jsOp1("~", x)
end bitNOT

-- (<<) :: Int -> Int -> Int
on |<<|(x, y)
    if 31 < y then
        0
    else
        jsOp2("<<", x, y)
    end if
end |<<|

-- Logical right shift
-- (>>>) :: Int -> Int -> Int
on |>>>|(x, y)
    jsOp2(">>>", x, y)
end |>>>|

-- Arithmetic right shift
-- (>>) :: Int -> Int -> Int
on |>>|(x, y)
    jsOp2(">>", x, y)
end |>>|


-- TEST ----------------------------------------------------------
on run
    -- Using an ObjC interface to Javascript for Automation

    set strClip to bitWise(255, 170)
    set the clipboard to strClip
    strClip
end run

-- bitWise :: Int -> Int -> String
on bitWise(a, b)
    set labels to {"a AND b", "a OR b", "a XOR b", "NOT a", ¬
        "a << b", "a >>> b", "a >> b"}
    set xs to {bitAND(a, b), bitOR(a, b), bitXOR(a, b), bitNOT(a), ¬
        |<<|(a, b), |>>>|(a, b), |>>|(a, b)}

    script asBin
        property arrow : " -> "
        on |λ|(x, y)
            justifyRight(8, space, x) & arrow & ¬
                justifyRight(14, space, y as text) & arrow & showBinary(y)
        end |λ|
    end script

    unlines({"32 bit signed integers   (in two's complement binary encoding)", "", ¬
        unlines(zipWith(asBin, ¬
            {"a = " & a as text, "b = " & b as text}, {a, b})), "", ¬
        unlines(zipWith(asBin, labels, xs))})
end bitWise

-- CONVERSIONS AND DISPLAY

-- bitsFromInt :: Int -> Either String [Bool]
on bitsFromIntLR(x)
    script go
        on |λ|(n, d, bools)
            set xs to {0 ≠ d} & bools
            if n > 0 then
                |λ|(n div 2, n mod 2, xs)
            else
                xs
            end if
        end |λ|
    end script

    set a to abs(x)
    if (2.147483647E+9) < a then
        |Left|("Integer overflow – maximum is (2 ^ 31) - 1")
    else
        set bs to go's |λ|(a div 2, a mod 2, {})
        if 0 > x then
            |Right|(replicate(32 - (length of bs), true) & ¬
                binSucc(map(my |not|, bs)))
        else
            set bs to go's |λ|(a div 2, a mod 2, {})
            |Right|(replicate(32 - (length of bs), false) & bs)
        end if
    end if
end bitsFromIntLR

-- Successor function (+1) for unsigned binary integer

-- binSucc :: [Bool] -> [Bool]
on binSucc(bs)
    script succ
        on |λ|(a, x)
            if a then
                if x then
                    Tuple(a, false)
                else
                    Tuple(x, true)
                end if
            else
                Tuple(a, x)
            end if
        end |λ|
    end script

    set tpl to mapAccumR(succ, true, bs)
    if |1| of tpl then
        {true} & |2| of tpl
    else
        |2| of tpl
    end if
end binSucc

-- showBinary :: Int -> String
on showBinary(x)
    script showBin
        on |λ|(xs)
            script bChar
                on |λ|(b)
                    if b then
                        "1"
                    else
                        "0"
                    end if
                end |λ|
            end script

            map(bChar, xs)
        end |λ|
    end script
    bindLR(my bitsFromIntLR(x), showBin)
end showBinary


-- JXA ------------------------------------------------------------------

--jsOp2 :: String -> a -> b -> c
on jsOp2(strOp, a, b)
    bindLR(evalJSLR(unwords({a as text, strOp, b as text})), my |id|) as integer
end jsOp2

--jsOp2 :: String -> a -> b
on jsOp1(strOp, a)
    bindLR(evalJSLR(unwords({strOp, a as text})), my |id|) as integer
end jsOp1

-- evalJSLR :: String -> Either String a
on evalJSLR(strJS)
    try -- NB if gJSC is global it must be released
        -- (e.g. set to null) at end of script
        gJSC's evaluateScript
    on error
        set gJSC to current application's JSContext's new()
        log ("new JSC")
    end try
    set v to unwrap((gJSC's evaluateScript:(strJS))'s toObject())
    if v is missing value then
        |Left|("JS evaluation error")
    else
        |Right|(v)
    end if
end evalJSLR

-- GENERIC FUNCTIONS --------------------------------------------------

-- Left :: a -> Either a b
on |Left|(x)
    {type:"Either", |Left|:x, |Right|:missing value}
end |Left|

-- Right :: b -> Either a b
on |Right|(x)
    {type:"Either", |Left|:missing value, |Right|:x}
end |Right|

-- Tuple (,) :: a -> b -> (a, b)
on Tuple(a, b)
    {type:"Tuple", |1|:a, |2|:b, length:2}
end Tuple

-- Absolute value.
-- abs :: Num -> Num
on abs(x)
    if 0 > x then
        -x
    else
        x
    end if
end abs

-- bindLR (>>=) :: Either a -> (a -> Either b) -> Either b
on bindLR(m, mf)
    if missing value is not |Right| of m then
        mReturn(mf)'s |λ|(|Right| of m)
    else
        m
    end if
end bindLR

-- foldr :: (a -> b -> b) -> b -> [a] -> b
on foldr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from lng to 1 by -1
            set v to |λ|(item i of xs, v, i, xs)
        end repeat
        return v
    end tell
end foldr

-- id :: a -> a
on |id|(x)
    x
end |id|

-- justifyRight :: Int -> Char -> String -> String
on justifyRight(n, cFiller, strText)
    if n > length of strText then
        text -n thru -1 of ((replicate(n, cFiller) as text) & strText)
    else
        strText
    end if
end justifyRight

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- 'The mapAccumR function behaves like a combination of map and foldr;
--  it applies a function to each element of a list, passing an accumulating
--  parameter from |Right| to |Left|, and returning a final value of this
--  accumulator together with the new list.' (see Hoogle)
-- mapAccumR :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
on mapAccumR(f, acc, xs)
    script
        on |λ|(x, a, i)
            tell mReturn(f) to set pair to |λ|(|1| of a, x, i)
            Tuple(|1| of pair, (|2| of pair) & |2| of a)
        end |λ|
    end script
    foldr(result, Tuple(acc, []), xs)
end mapAccumR

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- not :: Bool -> Bool
on |not|(p)
    not p
end |not|

-- Egyptian multiplication - progressively doubling a list, appending
-- stages of doubling to an accumulator where needed for binary
-- assembly of a target length
-- replicate :: Int -> a -> [a]
on replicate(n, a)
    set out to {}
    if n < 1 then return out
    set dbl to {a}

    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate

-- unlines :: [String] -> String
on unlines(xs)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, linefeed}
    set str to xs as text
    set my text item delimiters to dlm
    str
end unlines

-- unwords :: [String] -> String
on unwords(xs)
    set {dlm, my text item delimiters} to {my text item delimiters, space}
    set s to xs as text
    set my text item delimiters to dlm
    return s
end unwords

-- unwrap :: NSObject -> a
on unwrap(objCValue)
    if objCValue is missing value then
        missing value
    else
        set ca to current application
        item 1 of ((ca's NSArray's arrayWithObject:objCValue) as list)
    end if
end unwrap

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
on zipWith(f, xs, ys)
    set lng to min(length of xs, length of ys)
    if 1 > lng then return {}
    set lst to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, item i of ys)
        end repeat
        return lst
    end tell
end zipWith
```

{{Out}}

```txt
32 bit signed integers   (in two's complement binary encoding)

 a = 255 ->            255 -> 00000000000000000000000011111111
 b = 170 ->            170 -> 00000000000000000000000010101010

 a AND b ->            170 -> 00000000000000000000000010101010
  a OR b ->            255 -> 00000000000000000000000011111111
 a XOR b ->             85 -> 00000000000000000000000001010101
   NOT a ->           -256 -> 11111111111111111111111100000000
  a << b ->              0 -> 00000000000000000000000000000000
 a >>> b ->              0 -> 00000000000000000000000000000000
  a >> b ->              0 -> 00000000000000000000000000000000
```



'''Second option''' – writing our own bitwise functions for Applescript:

```applescript
use AppleScript version "2.4"
use framework "Foundation"
use scripting additions

-- BITWISE OPERATIONS FOR APPLESCRIPT ---------------------------------------

-- bitAND :: Int -> Int -> Int
on bitAND(x, y)
    bitOp2(my |and|, x, y)
end bitAND

-- bitOR :: Int -> Int -> Int
on bitOR(x, y)
    bitOp2(my |or|, x, y)
end bitOR

-- bitXOr :: Int -> Int -> Int
on bitXOR(x, y)
    bitOp2(my xor, x, y)
end bitXOR

-- bitNOT :: Int -> Int
on bitNOT(x)
    script notBits
        on |λ|(xs)
            bindLR(intFromBitsLR(map(my |not|, xs)), my |id|)
        end |λ|
    end script
    bindLR(bitsFromIntLR(x), notBits)
end bitNOT

-- (<<) :: Int -> Int -> Int
on |<<|(a, b)
    script logicLshift
        on |λ|(bs)
            bindLR(intFromBitsLR(take(32, drop(b, bs) & replicate(b, false))), my |id|)
        end |λ|
    end script
    bindLR(bitsFromIntLR(a), logicLshift)
end |<<|

-- Logical right shift
-- (>>>) :: Int -> Int -> Int
on |>>>|(a, b)
    script logicRShift
        on |λ|(bs)
            bindLR(intFromBitsLR(take(32, replicate(b, false) & drop(b, bs))), my |id|)
        end |λ|
    end script
    bindLR(bitsFromIntLR(a), logicRShift)
end |>>>|

-- Arithmetic right shift
-- (>>) :: Int -> Int -> Int
on |>>|(a, b)
    script arithRShift
        on |λ|(bs)
            if 0 < length of bs then
                set sign to item 1 of bs
            else
                set sign to false
            end if
            bindLR(intFromBitsLR(take(32, replicate(b, sign) & drop(b, bs))), my |id|)
        end |λ|
    end script
    bindLR(bitsFromIntLR(a), arithRShift)

end |>>|

-- bitRotL :: Int -> Int -> Int
on bitRotL(a, b)
    script lRot
        on |λ|(bs)
            bindLR(intFromBitsLR(rotate(-b, bs)), my |id|)
        end |λ|
    end script
    bindLR(bitsFromIntLR(a), lRot)
end bitRotL

-- bitRotR :: Int -> Int -> Int
on bitRotR(a, b)
    script rRot
        on |λ|(bs)
            bindLR(intFromBitsLR(rotate(b, bs)), my |id|)
        end |λ|
    end script
    bindLR(bitsFromIntLR(a), rRot)
end bitRotR

-- TEST ---------------------------------------------------------------

-- bitWise :: Int -> Int -> String
on bitWise(a, b)
    set labels to {"a AND b", "a OR b", "a XOR b", "NOT a", ¬
        "a << b", "a >>> b", "a >> b", "ROTL a b", "ROTR a b"}
    set xs to {bitAND(a, b), bitOR(a, b), bitXOR(a, b), bitNOT(a), ¬
        |<<|(a, b), |>>>|(a, b), |>>|(a, b), bitRotL(a, b), bitRotR(a, b)}

    script asBin
        property arrow : " -> "
        on |λ|(x, y)
            justifyRight(8, space, x) & arrow & ¬
                justifyRight(14, space, y as text) & arrow & showBinary(y)
        end |λ|
    end script

    unlines({"32 bit signed integers   (in two's complement binary encoding)", "", ¬
        unlines(zipWith(asBin, ¬
            {"a = " & a as text, "b = " & b as text}, {a, b})), "", ¬
        unlines(zipWith(asBin, labels, xs))})
end bitWise

on run
    -- Assuming 32 bit signed integers (in two's complement binary encoding)

    set strClip to bitWise(255, 170)
    set the clipboard to strClip
    strClip
end run

-- BINARY INTEGER CONVERSIONS AND DISPLAY  ------------------------------------------------------------------

-- bitsFromInt :: Int -> Either String [Bool]
on bitsFromIntLR(x)
    script go
        on |λ|(n, d, bools)
            set xs to {0 ≠ d} & bools
            if n > 0 then
                |λ|(n div 2, n mod 2, xs)
            else
                xs
            end if
        end |λ|
    end script

    set a to abs(x)
    if (2.147483647E+9) < a then
        |Left|("Integer overflow – maximum is (2 ^ 31) - 1")
    else
        set bs to go's |λ|(a div 2, a mod 2, {})
        if 0 > x then
            |Right|(replicate(32 - (length of bs), true) & ¬
                binSucc(map(my |not|, bs)))
        else
            set bs to go's |λ|(a div 2, a mod 2, {})
            |Right|(replicate(32 - (length of bs), false) & bs)
        end if
    end if
end bitsFromIntLR

-- intFromBitsLR :: [Bool] -> Either String Int
on intFromBitsLR(xs)
    script bitSum
        on |λ|(x, a, i)
            if x then
                a + (2 ^ (31 - i))
            else
                a
            end if
        end |λ|
    end script

    set lngBits to length of xs
    if 32 < lngBits then
        |Left|("Applescript limited to signed 32 bit integers")
    else if 1 > lngBits then
        |Right|(0 as integer)
    else
        set bits to (rest of xs)
        if item 1 of xs then
            |Right|(0 - foldr(bitSum, 1, map(my |not|, bits)) as integer)
        else
            |Right|(foldr(bitSum, 0, bits) as integer)
        end if
    end if
end intFromBitsLR

-- showBinary :: Int -> String
on showBinary(x)
    script showBin
        on |λ|(xs)
            script bChar
                on |λ|(b)
                    if b then
                        "1"
                    else
                        "0"
                    end if
                end |λ|
            end script

            map(bChar, xs)
        end |λ|
    end script
    bindLR(my bitsFromIntLR(x), showBin)
end showBinary

-- bitOp2 :: ((Bool -> Bool -> Bool) -> Int -> Int -> Int
on bitOp2(f, x, y)
    script yBits
        on |λ|(bitX)
            script zipOp
                on |λ|(bitY)
                    bitZipWithLR(f, bitX, bitY)
                end |λ|
            end script
            bindLR(bindLR(bindLR(bitsFromIntLR(y), ¬
                zipOp), my intFromBitsLR), my |id|)
        end |λ|
    end script
    bindLR(bitsFromIntLR(x), yBits)
end bitOp2

-- bitZipWithLR ::  ((a, b) -> c ) -> [Bool] -> [Bool] -> Either String  [(Bool, Bool)]
on bitZipWithLR(f, xs, ys)
    set intX to length of xs
    set intY to length of ys
    set intMax to max(intX, intY)
    if 33 > intMax then
        if intX > intY then
            set {bxs, bys} to {xs, ys & replicate(intX - intY, false)}
        else
            set {bxs, bys} to {xs & replicate(intY - intX, false), ys}
        end if
        tell mReturn(f)
            set lst to {}
            repeat with i from 1 to intMax
                set end of lst to |λ|(item i of bxs, item i of bys)
            end repeat
            return |Right|(lst)
        end tell
    else
        |Left|("Above maximum of 32 bits")
    end if
end bitZipWithLR

-- Successor function (+1) for unsigned binary integer

-- binSucc :: [Bool] -> [Bool]
on binSucc(bs)
    script succ
        on |λ|(a, x)
            if a then
                if x then
                    Tuple(a, false)
                else
                    Tuple(x, true)
                end if
            else
                Tuple(a, x)
            end if
        end |λ|
    end script

    set tpl to mapAccumR(succ, true, bs)
    if |1| of tpl then
        {true} & |2| of tpl
    else
        |2| of tpl
    end if
end binSucc

-- BOOLEANS  ----------------------------------------------------

-- |or| :: Bool -> Bool -> Bool
on |or|(x, y)
    x or y
end |or|

-- |and| :: Bool -> Bool -> Bool
on |and|(x, y)
    x and y
end |and|

-- xor :: Bool -> Bool -> Bool
on xor(x, y)
    (x or y) and not (x and y)
end xor

-- not :: Bool -> Bool
on |not|(p)
    not p
end |not|

-- GENERAL ----------------------------------------------------

-- Right :: b -> Either a b
on |Right|(x)
    {type:"Either", |Left|:missing value, |Right|:x}
end |Right|

-- Left :: a -> Either a b
on |Left|(x)
    {type:"Either", |Left|:x, |Right|:missing value}
end |Left|

-- Tuple (,) :: a -> b -> (a, b)
on Tuple(a, b)
    {type:"Tuple", |1|:a, |2|:b, length:2}
end Tuple

-- Absolute value.
-- abs :: Num -> Num
on abs(x)
    if 0 > x then
        -x
    else
        x
    end if
end abs

-- bindLR (>>=) :: Either a -> (a -> Either b) -> Either b
on bindLR(m, mf)
    if missing value is not |Right| of m then
        mReturn(mf)'s |λ|(|Right| of m)
    else
        m
    end if
end bindLR

-- drop :: Int -> [a] -> [a]
-- drop :: Int -> String -> String
on drop(n, xs)
    if class of xs is not string then
        if n < length of xs then
            items (1 + n) thru -1 of xs
        else
            {}
        end if
    else
        if n < length of xs then
            text (1 + n) thru -1 of xs
        else
            ""
        end if
    end if
end drop

-- foldr :: (a -> b -> b) -> b -> [a] -> b
on foldr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from lng to 1 by -1
            set v to |λ|(item i of xs, v, i, xs)
        end repeat
        return v
    end tell
end foldr

-- id :: a -> a
on |id|(x)
    x
end |id|

-- justifyRight :: Int -> Char -> String -> String
on justifyRight(n, cFiller, strText)
    if n > length of strText then
        text -n thru -1 of ((replicate(n, cFiller) as text) & strText)
    else
        strText
    end if
end justifyRight

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- 'The mapAccumR function behaves like a combination of map and foldr;
--  it applies a function to each element of a list, passing an accumulating
--  parameter from |Right| to |Left|, and returning a final value of this
--  accumulator together with the new list.' (see Hoogle)
-- mapAccumR :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
on mapAccumR(f, acc, xs)
    script
        on |λ|(x, a, i)
            tell mReturn(f) to set pair to |λ|(|1| of a, x, i)
            Tuple(|1| of pair, (|2| of pair) & |2| of a)
        end |λ|
    end script
    foldr(result, Tuple(acc, []), xs)
end mapAccumR

-- max :: Ord a => a -> a -> a
on max(x, y)
    if x > y then
        x
    else
        y
    end if
end max

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- Egyptian multiplication - progressively doubling a list, appending
-- stages of doubling to an accumulator where needed for binary
-- assembly of a target length
-- replicate :: Int -> a -> [a]
on replicate(n, a)
    set out to {}
    if n < 1 then return out
    set dbl to {a}

    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate

-- rotate :: Int -> [a] -> [a]
on rotate(n, xs)
    set lng to length of xs
    if 0 > n then
        set d to (-n) mod lng
    else
        set d to lng - (n mod lng)
    end if
    drop(d, xs) & take(d, xs)
end rotate

-- take :: Int -> [a] -> [a]
-- take :: Int -> String -> String
on take(n, xs)
    if class of xs is string then
        if 0 < n then
            text 1 thru min(n, length of xs) of xs
        else
            ""
        end if
    else
        if 0 < n then
            items 1 thru min(n, length of xs) of xs
        else
            {}
        end if
    end if
end take

-- unlines :: [String] -> String
on unlines(xs)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, linefeed}
    set str to xs as text
    set my text item delimiters to dlm
    str
end unlines

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
on zipWith(f, xs, ys)
    set lng to min(length of xs, length of ys)
    if 1 > lng then return {}
    set lst to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, item i of ys)
        end repeat
        return lst
    end tell
end zipWith
```

{{Out}}

```txt
32 bit signed integers   (in two's complement binary encoding)

 a = 255 ->            255 -> 00000000000000000000000011111111
 b = 170 ->            170 -> 00000000000000000000000010101010

 a AND b ->            170 -> 00000000000000000000000010101010
  a OR b ->            255 -> 00000000000000000000000011111111
 a XOR b ->             85 -> 00000000000000000000000001010101
   NOT a ->           -256 -> 11111111111111111111111100000000
  a << b ->              0 -> 00000000000000000000000000000000
 a >>> b ->              0 -> 00000000000000000000000000000000
  a >> b ->              0 -> 00000000000000000000000000000000
ROTL a b ->         261120 -> 00000000000000111111110000000000
ROTR a b ->  1.06954752E+9 -> 00111111110000000000000000000000
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program binarydigit.s   */
/* Constantes    */
.equ STDOUT, 1
.equ WRITE,  4
.equ EXIT,   1
/* Initialized data */
.data
szMessResultAnd: .asciz "Result of And : \n"
szMessResultOr: .asciz "Result of Or : \n"
szMessResultEor: .asciz "Result of Exclusif Or : \n"
szMessResultNot: .asciz "Result of Not : \n"
szMessResultLsl: .asciz "Result of left shift : \n"
szMessResultLsr: .asciz "Result of right shift : \n"
szMessResultAsr: .asciz "Result of Arithmetic right shift : \n"
szMessResultRor: .asciz "Result of rotate right : \n"
szMessResultRrx: .asciz "Result of rotate right with extend : \n"
szMessResultClear: .asciz "Result of Bit Clear : \n"

sMessAffBin: .ascii "Register value : "
sZoneBin: .space 36,' '
              .asciz "\n"

/*  code section */
.text
.global main
main:                /* entry of program  */
    push {fp,lr}    /* save des  2 registres */
    ldr r0,iAdrszMessResultAnd
    bl affichageMess
    mov r0,#5
    and r0,#15
    bl affichage2
    ldr r0,iAdrszMessResultOr
    bl affichageMess
    mov r0,#5
    orr r0,#15
    bl affichage2
    ldr r0,iAdrszMessResultEor
    bl affichageMess
    mov r0,#5
    eor r0,#15
    bl affichage2
    ldr r0,iAdrszMessResultNot
    bl affichageMess
    mov r0,#5
    mvn r0,r0
    bl affichage2
    ldr r0,iAdrszMessResultLsl
    bl affichageMess
    mov r0,#5
    lsl r0,#1
    bl affichage2
    ldr r0,iAdrszMessResultLsr
    bl affichageMess
    mov r0,#5
    lsr r0,#1
    bl affichage2
    ldr r0,iAdrszMessResultAsr
    bl affichageMess
    mov r0,#-5
    bl affichage2
    mov r0,#-5
    asr r0,#1
    bl affichage2
    ldr r0,iAdrszMessResultRor
    bl affichageMess
    mov r0,#5
    ror r0,#1
    bl affichage2
    ldr r0,iAdrszMessResultRrx
    bl affichageMess
    mov r0,#5
    mov r1,#15
    rrx r0,r1
    bl affichage2
	ldr r0,iAdrszMessResultClear
    bl affichageMess
	mov r0,#5
    bic r0,#0b100     @  clear 3ieme bit
    bl affichage2
	bic r0,#4          @  clear 3ieme bit  ( 4 = 100 binary)
    bl affichage2

100:   /* standard end of the program */
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call
iAdrszMessResultAnd:  .int szMessResultAnd
iAdrszMessResultOr:  .int szMessResultOr
iAdrszMessResultEor:  .int szMessResultEor
iAdrszMessResultNot:  .int szMessResultNot
iAdrszMessResultLsl:  .int szMessResultLsl
iAdrszMessResultLsr:  .int szMessResultLsr
iAdrszMessResultAsr:  .int szMessResultAsr
iAdrszMessResultRor:  .int szMessResultRor
iAdrszMessResultRrx:  .int szMessResultRrx
iAdrszMessResultClear:  .int szMessResultClear
/******************************************************************/
/*     register display in binary                              */
/******************************************************************/
/* r0 contains the register */
affichage2:
    push {r0,lr}     /* save  registers */
    push {r1-r5} /* save others registers */
    mrs r5,cpsr  /* saves state register in r5 */
    ldr r1,iAdrsZoneBin
    mov r2,#0    @ read bit position counter
    mov r3,#0    @ position counter of the written character
1:               @ loop
    lsls r0,#1    @ left shift  with flags
    movcc r4,#48  @ flag carry off   character '0'
    movcs r4,#49  @ flag carry on    character '1'
    strb r4,[r1,r3]   @ character ->   display zone
    add r2,r2,#1      @ + 1 read bit position counter
    add r3,r3,#1      @ + 1 position counter of the written character
    cmp r2,#8         @ 8 bits read
    addeq r3,r3,#1   @ + 1 position counter of the written character
    cmp r2,#16         @ etc
    addeq r3,r3,#1
    cmp r2,#24
    addeq r3,r3,#1
    cmp r2,#31        @ 32 bits shifted ?
    ble 1b           @  no -> loop

    ldr r0,iAdrsZoneMessBin   @ address of message result
    bl affichageMess           @ display result

100:
    msr cpsr,r5    /*restaur state register */
    pop {r1-r5}  /* restaur others registers */
    pop {r0,lr}
    bx lr
iAdrsZoneBin: .int sZoneBin
iAdrsZoneMessBin: .int sMessAffBin

/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {fp,lr}    			/* save  registres */
    push {r0,r1,r2,r7}    		/* save others registres */
    mov r2,#0   				/* counter length */
1:      	/* loop length calculation */
    ldrb r1,[r0,r2]  			/* read octet start position + index */
    cmp r1,#0       			/* if 0 its over */
    addne r2,r2,#1   			/* else add 1 in the length */
    bne 1b          			/* and loop */
                                /* so here r2 contains the length of the message */
    mov r1,r0        			/* address message in r1 */
    mov r0,#STDOUT      		/* code to write to the standard output Linux */
    mov r7, #WRITE             /* code call system write */
    swi #0                      /* call systeme */
    pop {r0,r1,r2,r7}     		/* restaur others registres */
    pop {fp,lr}    				/* restaur des  2 registres */
    bx lr	        			/* return  */



```



## Arturo



```arturo
a 255
b 2

print "`a` AND `b` = " + $(and a b)
print "`a` OR `b` = " + $(or a b)
print "`a` XOR `b` = " + $(xor a b)
print "NOT `a` = " + $(not a)
print "`a` SHL `b` = " + $(shl a b)
print "`a` SHR `b` = " + $(shr a b)
```


{{out}}


```txt
255 AND 2 = 2
255 OR 2 = 255
255 XOR 2 = 253
NOT 255 = -256
255 SHL 2 = 1020
255 SHR 2 = 63
```



## AutoHotkey


```AutoHotkey
bitwise(3, 4)
bitwise(a, b)
{
  MsgBox % "a and b: " . a & b
  MsgBox % "a or b: " . a | b
  MsgBox % "a xor b: " . a ^ b
  MsgBox % "not a: " . ~a       ; treated as unsigned integer
  MsgBox % "a << b: " . a << b  ; left shift
  MsgBox % "a >> b: " . a >> b  ; arithmetic right shift
}
```



## AWK

Standard awk does not have bitwise operators. Gawk has built-in functions for many bitwise operations. No rotation of bits.

{{works with|gawk}}


```awk
BEGIN {
  n = 11
  p = 1
  print n " or  " p " = " or(n,p)
  print n " and " p " = " and(n,p)
  print n " xor " p " = " xor(n,p)
  print n " <<  " p " = " lshift(n, p)   # left shift
  print n " >>  " p " = " rshift(n, p)   # right shift
  printf "not %d = 0x%x\n", n, compl(n)  # bitwise complement
}
```


[[OpenBSD]] <code>/usr/bin/awk</code> (a variant of [[nawk]]) has these same functions, with a few differences. Gawk uses 53-bit unsigned integers, but OpenBSD awk uses 32-bit signed integers. Therefore Gawk prints <code>not 11 = 0x1ffffffffffff4</code>, but OpenBSD awk prints <code>not 11 = 0xfffffff4</code>.


## Axe


```axe
Lbl BITS
r₁→A
r₂→B
Disp "AND:",A·B▶Dec,i
Disp "OR:",AᕀB▶Dec,i
Disp "XOR:",A▫B▶Dec,i
Disp "NOT:",not(A)ʳ▶Dec,i
.No language support for shifts or rotations
Return
```


Note that the symbols for AND, OR, and XOR are the stat plot marks near the bottom of the Catalog.


## Babel


In Babel, we prefix the logic operators with a 'c' to denote that they are C-style operations, that is, they are word-width operations, not arbitrary size operations. The following program combines the numbers 5 and 9 using the various bitwise operators and then displays the results.


```babel
({5 9}) ({cand} {cor} {cnor} {cxor} {cxnor} {shl} {shr} {ashr} {rol}) cart ! {give <- cp -> compose !} over ! {eval} over ! {;} each
```


{{Out}}

```txt
[val 0x1 ]
[val 0xd ]
[val 0xfffffff7 ]
[val 0xc ]
[val 0xfffffff3 ]
[val 0xa00 ]
[val 0x0 ]
[val 0x0 ]
[val 0xa00 ]
```


The cnot operator works on just one operand:


```babel>9 cnot ;</lang


{{Out}}

```txt
[val 0xfffffff6 ]
```



## BASIC

{{works with|QuickBasic|4.5}}
QuickBasic does not have shift or rotate operations defined. Here are the logical operations:

```qbasic
SUB bitwise (a, b)
  PRINT a AND b
  PRINT a OR b
  PRINT a XOR b
  PRINT NOT a
END SUB
```


{{works with|FreeBASIC}}
FreeBASIC does not have rotate operators. Shift Right operator performs arithmetic shift if the left value is signed number and logical shift if the left value is unsigned number.

```freebasic
SUB bitwise (a AS Integer, b AS Integer)
  DIM u AS UInteger

  PRINT "a AND b = "; a AND b
  PRINT "a OR b  = "; a OR b
  PRINT "a XOR b = "; a XOR b
  PRINT "NOT a   = "; NOT a
  PRINT "a SHL b = "; a SHL b
  PRINT "a SHR b (arithmetic) = "; a SHR b
  u = a
  PRINT "a SHR b (logical) = "; u SHR b
END SUB
```


=
## Commodore BASIC
=
Commodore BASIC V2.0 does not have '''XOR''', '''left shift''', '''right shift''', '''right arithmetic shift''', '''left rotate''', and '''right rotate''' operators. In this implementation the '''XOR''' operation is done with an equivalent formula.


```basic
10 INPUT "A="; A
20 INPUT "B="; B
30 PRINT "A AND B =" A AND B    :rem AND
40 PRINT "A OR B =" A OR B      :rem OR
50 PRINT "A XOR B =" (A AND(NOT B))OR((NOT A)AND B)    :rem XOR
60 PRINT "NOT A =" NOT A        :rem NOT
```

{{in}}

```txt
A=? 2
B=? 6
```

{{out}}

```txt
A AND B = 2
A OR B = 6
A XOR B = 4
NOT A =-3

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 LET A=10:LET B=12
110 PRINT A;"and";B;"=";A AND B
120 PRINT A;"band";B;"=";A BAND B
130 PRINT A;"or ";B;"=";A OR B
140 PRINT A;"bor";B;"=";A BOR B
150 PRINT A;"xor";B;"=";XOR(A,B)
160 PRINT " not";A;"=";NOT A
170 DEF XOR(A,B)=(A BOR B)-(A BAND B)
```


=
## Sinclair ZX81 BASIC
=
ZX81 BASIC has no integer type (a major lacuna) and consequently no bitwise operations; but the CPU has them, so we can use a tiny machine code routine to do the actual work and then return to BASIC to print the answers.

This program is a proof of concept, really, and will only work with 8-bit values. In addition, with 1k of RAM there is only space for the first of the shifts/rotates; the others could be implemented along exactly the same lines.

The disassembly of the Z80 code would be:

```z80asm
           org   4084
3a 83 40   ld    a, (4083)
47         ld    b, a
3a 82 40   ld    a, (4082)
a0         and   b
00         nop            ; negate and shift instructions take 2 bytes
06 00      ld    b, 0
4f         ld    c, a     ; value in BC reg pair is returned to BASIC
c9         ret
```

We then use <code>POKE</code> statements to replace the <code>and</code> instruction with each successive operation we want to perform.

Note that the left shift instruction shifts by one bit at a time, so we need a loop. The present program has the loop written in BASIC, because it seemed sensible to use BASIC for anything we could use it for and only drop into machine code when there was no alternative; it would of course be faster to do the whole thing in machine code.

Finally, observe that the first line reserves 15 bytes for our machine code routine by hiding them in a comment.


```basic
 10 REM ABCDEFGHIJKLMNO
 20 INPUT A
 30 INPUT B
 40 POKE 16514,A
 50 POKE 16515,B
 60 LET ADDR=16516
 70 LET R$="3A8340473A8240A00006004FC9"
 80 POKE ADDR,CODE R$*16+CODE R$(2)-476
 90 LET R$=R$(3 TO )
100 LET ADDR=ADDR+1
110 IF R$<>"" THEN GOTO 80
120 PRINT A;" AND ";B;" = ";USR 16516
130 POKE 16523,176
140 PRINT A;" OR ";B;" = ";USR 16516
150 POKE 16523,168
160 PRINT A;" XOR ";B;" = ";USR 16516
170 POKE 16523,237
180 POKE 16524,68
190 PRINT "NOT ";A;" = ";USR 16516
200 POKE 16523,203
210 POKE 16524,39
220 FOR I=1 TO B
230 POKE 16514,USR 16516
240 NEXT I
250 PRINT A;" << ";B;" = ";PEEK 16514
```

{{in}}

```txt
21
3
```

{{out}}

```txt
21 AND 3 = 1
21 OR 3 = 23
21 XOR 3 = 22
NOT 21 = 235
21 << 3 = 168
```



## BASIC256


```BASIC256
# bitwise operators - floating point numbers will be cast to integer
a = 0b00010001
b = 0b11110000
print a
print int(a * 2)  # shift left (multiply by 2)
print a \ 2  # shift right (integer divide by 2)
print a | b  # bitwise or on two integer values
print a & b  # bitwise or on two integer values
```



## Batch File

The SET command with the /A option supports arithmetic and bit operations on signed 8 byte integers.

The SET /? documentation claims it supports logical shift operations, but in reality it performs an arithmetic right shift.

The following script (bitops.bat) not only demonstrates the basic bit operations, it also uses bit operations to convert each integral value into a string of 32 binary digits.

```dos

@echo off
setlocal
set /a "a=%~1, b=%~2"
call :num2bin %a% aStr
call :num2bin %b% bStr

::AND
set /a "val=a&b"
call :display "%a% AND %b%" %val% %aStr% %bStr%

::OR
set /a "val=a|b"
call :display "%a% OR %b%" %val% %aStr% %bStr%

::XOR
set /a "val=a^b"
call :display "%a% XOR %b%" %val% %aStr% %bStr%

::NOT
set /a "val=~a"
call :display "NOT %a%" %val% %aStr%

::LEFT SHIFT
set /a "val=a<<b"
call :display "%a% Left Shift %b%" %val% %aStr%

::ARITHMETIC RIGHT SHIFT
set /a "val=a>>b"
call :display "%a% Arithmetic Right Shift %b%" %val% %aStr%

::The remaining operations do not have native support
::The implementations use additional operators
::  %% = mod
::  ! = logical negation where !(zero)=1 and !(non-zero)=0
::  * = multiplication
::  - = subtraction

::LOGICAL RIGHT SHIFT (No native support)
set /a "val=(a>>b)&~((0x80000000>>b-1)*!!b)"
call :display "%a% Logical Right Shift %b%" %val% %aStr%

::ROTATE LEFT (No native support)
set /a "val=(a<<b%%32) | (a>>32-b%%32)&~((0x80000000>>31-b%%32)*!!(32-b%%32))"
call :display "%a% Rotate Left %b%" %val% %aStr%

::ROTATE RIGHT (No native support)
set /a "val=(a<<32-b%%32) | (a>>b%%32)&~((0x80000000>>b%%32-1)*!!(b%%32)) "
call :display "%a% Rotate Right %b%" %val% %aStr%

exit /b


:display op result aStr [bStr]
echo(
echo %~1 = %2
echo %3
if "%4" neq "" echo %4
call :num2bin %2
exit /b


:num2bin    IntVal [RtnVar]
  setlocal enableDelayedExpansion
  set n=%~1
  set rtn=
  for /l %%b in (0,1,31) do (
    set /a "d=n&1, n>>=1"
    set rtn=!d!!rtn!
  )
  (endlocal & rem -- return values
    if "%~2" neq "" (set %~2=%rtn%) else echo %rtn%
  )
exit /b

```


Sample output

```txt

>bitops 0x800000FE 7

-2147483394 AND 7 = 6
10000000000000000000000011111110
00000000000000000000000000000111
00000000000000000000000000000110

-2147483394 OR 7 = -2147483393
10000000000000000000000011111110
00000000000000000000000000000111
10000000000000000000000011111111

-2147483394 XOR 7 = -2147483399
10000000000000000000000011111110
00000000000000000000000000000111
10000000000000000000000011111001

NOT -2147483394 = 2147483393
10000000000000000000000011111110
01111111111111111111111100000001

-2147483394 Left Shift 7 = 32512
10000000000000000000000011111110
00000000000000000111111100000000

-2147483394 Arithmetic Right Shift 7 = -16777215
10000000000000000000000011111110
11111111000000000000000000000001

-2147483394 Logical Right Shift 7 = 16777217
10000000000000000000000011111110
00000001000000000000000000000001

-2147483394 Rotate Left 7 = 32576
10000000000000000000000011111110
00000000000000000111111101000000

-2147483394 Rotate Right 7 = -50331647
10000000000000000000000011111110
11111101000000000000000000000001

```



## BBC BASIC


```bbcbasic
      number1% = &89ABCDEF
      number2% = 8

      PRINT ~ number1% AND number2% : REM bitwise AND
      PRINT ~ number1% OR number2%  : REM bitwise OR
      PRINT ~ number1% EOR number2% : REM bitwise exclusive-OR
      PRINT ~ NOT number1%          : REM bitwise NOT
      PRINT ~ number1% << number2%  : REM left shift
      PRINT ~ number1% >>> number2% : REM right shift (logical)
      PRINT ~ number1% >> number2%  : REM right shift (arithmetic)
      PRINT ~ (number1% << number2%) OR (number1% >>> (32-number2%)) : REM left rotate
      PRINT ~ (number1% >>> number2%) OR (number1% << (32-number2%)) : REM right rotate
```



## beeswax


```beeswax
#eX~T~T_#
###>N{` AND `~{~` = `&{Nz1~3J
UXe#
##>{` OR  `~{~` = `|{Nz1~5J
UXe#
##>{` XOR `~{~` = `${Nz1~7J
UXe#
##>`NOT `{` = `!{Nz1~9J
UXe#
##>{` <<  `~{~` = `({Nz1~9PPJ
UXe#
##>{` >>> `~{~` = `){` (logical shift right)`N7F+M~1~J
UXe#
##>{` ROL `~{~` = `[{N7F+P~1~J
UXe#
##>{` ROR `~{~` = `]{NN8F+P~1~J
UXe#
##>`Arithmetic shift right is not originally implemented in beeswax.`N     q
 qN`,noitagen yb dezilaer eb nac srebmun evitagen rof RSA ,yllacinhcet tuB`N<
##>`logical shift right, and negating the result again:`NN7F++~1~J
UXe#      #>e#
 #>~1~[&'pUX{` >> `~{~` = `){` , interpreted as (positive) signed Int64 number (MSB=0), equivalent to >>>`NN;
          ###
         >UX`-`!P{M!` >> `~{~` = `!)!`-`M!{` , interpreted as (negative) signed Int64 number (MSB=1)`NN;
           #>e#
```


Example:

```Julia

julia> beeswax("Bitops.bswx",0,0.0,Int(20000))
i9223653511831486512
i48

9223653511831486512 AND 48 = 48
9223653511831486512 OR  48 = 9223653511831486512
9223653511831486512 XOR 48 = 9223653511831486464
NOT 9223653511831486512 = 9223090561878065103
9223653511831486512 <<  48 = 13510798882111488
9223653511831486512 >>> 48 = 32769 (logical shift right)
9223653511831486512 ROL 48 = 13651540665434112
9223653511831486512 ROR 48 = 3178497

Arithmetic shift right is not originally implemented in beeswax.

But technically, ASR for negative numbers can be realized by negation,
logical shift right, and negating the result again:

-9223090561878065104 >> 48 = -32767 , interpreted as (negative) signed Int64 number (MSB=1)
```


The natural number range for beeswax is unsigned Int64, but it is easy to implement signed Int64 by realizing negative numbers by their 2’s complements or interpreting numbers as negative if their MSB is 1, as shown in the example above.

Arithmetic shift right is not originally implemented in beeswax because it does not make sense for unsigned integers, but for negative numbers, it can be realized easily with

```beeswax>A>
B = NOT(NOT(A)>>>B)
```

as demonstrated above.

In beeswax, rotate left (ROL) and rotate right (ROT) operators are implemented using modulo 64, so rotations by more than 63 bits wrap around:


```beeswax
A ROL B = A<<(B%64)+A>>>(64-B%64)
A ROR B = A>>>(B%64)+A<<(64-B%64)
```



## Befunge


```befunge>> v   MCR
v
    1    2       3   4       5            6>61g-:|        8       9
  >&&\481p >88*61p371p >:61g\`!:68*+71g81gp|    7 >61g2/61p71g1+71pv
   >v>v>v>v  <                             >      ^
>#A       1 $^         ^                                           <
  B       6^                   <
   ^>^>^>^1  C                 |!`5p18:+1g18$    <
   ^  9   p#p17*93p189p150     <    >61g71g81gg+71g81gpv D
          >071g81gp          v      ^               <
              AND            >+2\`!#^_>                v
              XOR             +2%  #^_>                v
              OR              +1\`!#^_>                v
              NOT             !    #^_>                v
              LSHFT           0    #^_>48*71g3+81gp    v
              RSHFT           $      48*71g3+81gp  #^_>v E
              END             v    #^_>                >61g2*61pv
                              @    F
                           v_^#                              `2:<
                           >71g81gg.48*71g2+81gp79*1-71g2+81g1+pv
   ^                                    <_v#!`2p15:+1g15p18+1g18<
^                                         < G


```

The labelled points (1 to G) are:
1. Read in A and B,
2. Set the current operating row (R) to 4,
3. Set the current bit value (M) to 64,
4. Set Current operating column (C) to 3,
5. Check if M > A (i.e. bit is 0 or 1),
6. Write the bit value into location (R,C),
7. A = A - M,
8. M = M/2,
9. C++,
A&B. Storage of corresponding bits,
C. Initialise R & C to operation storage (OP) and M to 1,
D. Increment OP by M if true,
E. M = M*2,
F (2 rows below). Print value of OP, increment operation to perform by moving ">" down,
G. If doing the NOT, LSHFT or RSHFT (current operation to perform > 3) only read A.

The code requires input be separated by spaces and only works for numbers less than 128, due to form of bit storage and ASCII locations not able to store beyond 127. Overflow will happen if 127 is shifted left due to aforementioned ASCII limit in most Befunge-93 interpreters.

'''Inputs''':

```txt

21 3

```


{{out}}

```txt

1 22 23 106 42 10

```



## C


```c
void bitwise(int a, int b)
{
  printf("a and b: %d\n", a & b);
  printf("a or b: %d\n", a | b);
  printf("a xor b: %d\n", a ^ b);
  printf("not a: %d\n", ~a);
  printf("a << n: %d\n", a << b); /* left shift */
  printf("a >> n: %d\n", a >> b); /* on most platforms: arithmetic right shift */
  /* convert the signed integer into unsigned, so it will perform logical shift */
  unsigned int c = a;
  printf("c >> b: %d\n", c >> b); /* logical right shift */
  /* there are no rotation operators in C */
  return 0;
}
```


To rotate an integer, you can combine a left shift and a right shift:

```C
/* rotate x to the right by s bits */
unsigned int rotr(unsigned int x, unsigned int s)
{
	return (x >> s) | (x << 32 - s);
}
```
With a smart enough compiler, the above actually compiles into a single machine bit rotate instruction when possible. E.g. <code>gcc -S</code> on IA32 produced following assembly code:
```Assembly
rotr:
        movl    4(%esp), %eax        ; arg1: x
        movl    8(%esp), %ecx        ; arg2: s
        rorl    %cl, %eax            ; right rotate x by s
        ret
```



## C++

{{trans|C}}

```cpp
#include <iostream>

void bitwise(int a, int b)
{
  std::cout << "a and b: " << (a & b)  << '\n'; // Note: parentheses are needed because & has lower precedence than <<
  std::cout << "a or b:  " << (a | b)  << '\n';
  std::cout << "a xor b: " << (a ^ b)  << '\n';
  std::cout << "not a:   " << ~a       << '\n';
  std::cout << "a shl b: " << (a << b) << '\n'; // Note: "<<" is used both for output and for left shift
  std::cout << "a shr b: " << (a >> b) << '\n'; // typically arithmetic right shift, but not guaranteed
  unsigned int c = a;
  std::cout << "c sra b: " << (c >> b) << '\n'; // logical right shift (guaranteed)
  // there are no rotation operators in C++
}
```

=={{header|C sharp|C#}}==

```csharp
static void bitwise(int a, int b)
        {
            Console.WriteLine("a and b is {0}", a & b);
            Console.WriteLine("a or b is {0}", a | b);
            Console.WriteLine("a xor b is {0}", a ^ b);
            Console.WriteLine("not a is {0}", ~a);
            Console.WriteLine("a lshift b is {0}", a << b);
            Console.WriteLine("a arshift b is {0}", a >> b); // When the left operand of the >> operator is of a signed integral type,
                                                             // the operator performs an arithmetic shift right
            uint c = (uint)a;
            Console.WriteLine("c rshift b is {0}", c >> b); // When the left operand of the >> operator is of an unsigned integral type,
                                                            // the operator performs a logical shift right
            // there are no rotation operators in C#
        }
```



## Clojure


```lisp
(bit-and x y)
(bit-or x y)
(bit-xor x y)
(bit-not x)
(bit-shift-left x n)
(bit-shift-right x n)
;;There is no built-in for rotation.
```



## COBOL

Results are displayed in decimal.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. bitwise-ops.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  a                       PIC 1(32) USAGE BIT.
       01  b                       PIC 1(32) USAGE BIT.

       01  result                  PIC 1(32) USAGE BIT.
       01  result-disp             REDEFINES result PIC S9(9) COMP.

       LINKAGE SECTION.
       01  a-int                   USAGE BINARY-LONG.
       01  b-int                   USAGE BINARY-LONG.

       PROCEDURE DIVISION USING a-int, b-int.
           MOVE FUNCTION BOOLEAN-OF-INTEGER(a-int, 32) TO a
           MOVE FUNCTION BOOLEAN-OF-INTEGER(b-int, 32) TO b

           COMPUTE result = a B-AND b
           DISPLAY "a and b is " result-disp

           COMPUTE result = a B-OR b
           DISPLAY "a or b is " result-disp

           COMPUTE result = B-NOT a
           DISPLAY "Not a is " result-disp

           COMPUTE result = a B-XOR b
           DISPLAY "a exclusive-or b is " result-disp

           *> COBOL does not have shift or rotation operators.

           GOBACK
           .
```


{{works with|Visual COBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. mf-bitwise-ops.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  result                  USAGE BINARY-LONG.

       78  arg-len                 VALUE LENGTH OF result.

       LINKAGE SECTION.
       01  a                       USAGE BINARY-LONG.
       01  b                       USAGE BINARY-LONG.

       PROCEDURE DIVISION USING a, b.
       main-line.
           MOVE b TO result
           CALL "CBL_AND" USING a, result, VALUE arg-len
           DISPLAY "a and b is " result

           MOVE b TO result
           CALL "CBL_OR" USING a, result, VALUE arg-len
           DISPLAY "a or b is " result

           MOVE a TO result
           CALL "CBL_NOT" USING result, VALUE arg-len
           DISPLAY "Not a is " result

           MOVE b TO result
           CALL "CBL_XOR" USING a, result, VALUE arg-len
           DISPLAY "a exclusive-or b is " result

           MOVE b TO result
           CALL "CBL_EQ" USING a, result, VALUE arg-len
           DISPLAY "Logical equivalence of a and b is " result

           MOVE b TO result
           CALL "CBL_IMP" USING a, result, VALUE arg-len
           DISPLAY "Logical implication of a and b is " result

           GOBACK
           .
```



## CoffeeScript

CoffeeScript provides sugar for some JavaScript operators, but the bitwise operators are taken directly from JS.  See more here: http://coffeescript.org/#operators


```coffeescript

f = (a, b) ->
  p "and", a & b
  p "or", a | b
  p "xor", a ^ b
  p "not", ~a
  p "<<", a << b
  p ">>", a >> b
  # no rotation shifts that I know of

p = (label, n) -> console.log label, n

f(10,2)

```


output
<lang>
> coffee foo.coffee
and 2
or 10
xor 8
not -11
<< 40
>> 2

```



## Common Lisp


```lisp
(defun bitwise (a b)
  (print (logand a b))  ; AND
  (print (logior a b))  ; OR ("ior" = inclusive or)
  (print (logxor a b))  ; XOR
  (print (lognot a))    ; NOT
  (print (ash a b))     ; arithmetic left shift (positive 2nd arg)
  (print (ash a (- b))) ; arithmetic right shift (negative 2nd arg)
                        ; no logical shift
)
```


Left and right logical shift may be implemented by the following functions:


```lisp

(defun shl (x width bits)
  "Compute bitwise left shift of x by 'bits' bits, represented on 'width' bits"
  (logand (ash x bits)
          (1- (ash 1 width))))

(defun shr (x width bits)
  "Compute bitwise right shift of x by 'bits' bits, represented on 'width' bits"
  (logand (ash x (- bits))
          (1- (ash 1 width))))

```


Left and right rotation may be implemented by the following functions:


```lisp

(defun rotl (x width bits)
  "Compute bitwise left rotation of x by 'bits' bits, represented on 'width' bits"
  (logior (logand (ash x (mod bits width))
                  (1- (ash 1 width)))
          (logand (ash x (- (- width (mod bits width))))
                  (1- (ash 1 width)))))

(defun rotr (x width bits)
  "Compute bitwise right rotation of x by 'bits' bits, represented on 'width' bits"
  (logior (logand (ash x (- (mod bits width)))
                  (1- (ash 1 width)))
          (logand (ash x (- width (mod bits width)))
                  (1- (ash 1 width)))))

```



## D


```d
T rot(T)(in T x, in int shift) pure nothrow @nogc {
    return (x >>> shift) | (x << (T.sizeof * 8 - shift));
}

void testBit(in int a, in int b) {
  import std.stdio;
  writefln("Input: a = %d, b = %d", a, b);
  writefln("AND  : %8b  & %08b = %032b (%4d)", a, b, a & b, a & b);
  writefln(" OR  : %8b  | %08b = %032b (%4d)", a, b, a | b, a | b);
  writefln("XOR  : %8b  ^ %08b = %032b (%4d)", a, b, a ^ b, a ^ b);
  writefln("LSH  : %8b << %08b = %032b (%4d)", a, b, a << b, a << b);
  writefln("RSH  : %8b >> %08b = %032b (%4d)", a, b, a >> b, a >> b);
  writefln("NOT  : %8s  ~ %08b = %032b (%4d)", "", a, ~a, ~a);
  writefln("ROT  : rot(%8b, %d)     = %032b (%4d)",
           a, b, rot(a, b), rot(a, b));
}

void main() {
  immutable int a = 0b_1111_1111; // bit literal 255
  immutable int b = 0b_0000_0010; // bit literal 2

  testBit(a, b);
}
```

{{out}}

```txt
Input: a = 255, b = 2
AND  : 11111111  & 00000010 = 00000000000000000000000000000010 (   2)
 OR  : 11111111  | 00000010 = 00000000000000000000000011111111 ( 255)
XOR  : 11111111  ^ 00000010 = 00000000000000000000000011111101 ( 253)
LSH  : 11111111 << 00000010 = 00000000000000000000001111111100 (1020)
RSH  : 11111111 >> 00000010 = 00000000000000000000000000111111 (  63)
NOT  :           ~ 11111111 = 11111111111111111111111100000000 (-256)
ROT  : rot(11111111, 2)     = 11000000000000000000000000111111 (-1073741761)
```


Compilers are usually able to optimize the code pattern of the rot function to one CPU instruction plus loads. The DMD compiler too performs such optimization.


## Delphi


```Delphi
program Bitwise;

{$APPTYPE CONSOLE}

begin
  Writeln('2 and 3 = ', 2 and 3);
  Writeln('2 or 3 = ', 2 or 3);
  Writeln('2 xor 3 = ', 2 xor 3);
  Writeln('not 2 = ', not 2);
  Writeln('2 shl 3 = ', 2 shl 3);
  Writeln('2 shr 3 = ', 2 shr 3);
// there are no built-in rotation operators in Delphi
  Readln;
end.
```



## DWScript


```Delphi
PrintLn('2 and 3 = '+IntToStr(2 and 3));
PrintLn('2 or 3 = '+IntToStr(2 or 3));
PrintLn('2 xor 3 = '+IntToStr(2 xor 3));
PrintLn('not 2 = '+IntToStr(not 2));
PrintLn('2 shl 3 = '+IntToStr(2 shl 3));
PrintLn('2 shr 3 = '+IntToStr(2 shr 3));
```



## E

E provides arbitrary-size integers, so there is no distinct arithmetic and logical shift right. E does not provide bit rotate operations.


```e
def bitwise(a :int, b :int) {
   println(`Bitwise operations:
   a AND b: ${a & b}
   a OR b: ${a | b}
   a XOR b: ${a ^ b}
   NOT a: " + ${~a}
   a left shift b: ${a << b}
   a right shift b: ${a >> b}
`)
}
```



## ECL


```ECL

BitwiseOperations(INTEGER A, INTEGER B) := FUNCTION
  BitAND := A & B;
  BitOR  := A | B;
  BitXOR := A ^ B;
  BitNOT := BNOT A;
  BitSL  := A << B;
  BitSR  := A >> B;
  DS     := DATASET([{A,B,'Bitwise AND:',BitAND},
	             {A,B,'Bitwise OR:',BitOR},
		     {A,B,'Bitwise XOR',BitXOR},
		     {A,B,'Bitwise NOT A:',BitNOT},
		     {A,B,'ShiftLeft A:',BitSL},
		     {A,B,'ShiftRight A:',BitSR}],
		     {INTEGER AVal,INTEGER BVal,STRING15 valuetype,INTEGER val});
  RETURN DS;
END;

BitwiseOperations(255,5);
//right arithmetic shift, left and right rotate not implemented
/*
   OUTPUT:
   255	5	Bitwise AND:   	5
   255	5	Bitwise OR:    	255
   255	5	Bitwise XOR    	250
   255	5	Bitwise NOT A: 	-256
   255	5	ShiftLeft A:   	8160
   255	5	ShiftRight A:  	7

*/

```



## Elena

ELENA 4.x :

```elena
import extensions;

extension testOp
{
    bitwiseTest(y)
    {
        console.printLine(self," and ",y," = ",self.and(y));
        console.printLine(self," or ",y," = ",self.or(y));
        console.printLine(self," xor ",y," = ",self.xor(y));
        console.printLine("not ",self," = ",self.Inverted);
        console.printLine(self," shr ",y," = ",self.shiftRight(y));
        console.printLine(self," shl ",y," = ",self.shiftLeft(y));
    }
}

public program()
{
    console.loadLineTo(new Integer()).bitwiseTest(console.loadLineTo(new Integer()))
}
```

{{out}}

```txt

255 and 2 = 2
255 or 2 = 255
255 xor 2 = 253
not 255 = -256
255 shr 2 = 63
255 shl 2 = 1020

```



## Elixir


```elixir
defmodule Bitwise_operation do
  use Bitwise

  def test(a \\ 255, b \\ 170, c \\ 2) do
    IO.puts "Bitwise function:"
    IO.puts "band(#{a}, #{b}) = #{band(a, b)}"
    IO.puts "bor(#{a}, #{b}) = #{bor(a, b)}"
    IO.puts "bxor(#{a}, #{b}) = #{bxor(a, b)}"
    IO.puts "bnot(#{a}) = #{bnot(a)}"
    IO.puts "bsl(#{a}, #{c}) = #{bsl(a, c)}"
    IO.puts "bsr(#{a}, #{c}) = #{bsr(a, c)}"
    IO.puts "\nBitwise as operator:"
    IO.puts "#{a} &&& #{b} = #{a &&& b}"
    IO.puts "#{a} ||| #{b} = #{a ||| b}"
    IO.puts "#{a} ^^^ #{b} = #{a ^^^ b}"
    IO.puts "~~~#{a} = #{~~~a}"
    IO.puts "#{a} <<< #{c} = #{a <<< c}"
    IO.puts "#{a} >>> #{c} = #{a >>> c}"
  end
end

Bitwise_operation.test
```


{{out}}

```txt

Bitwise function:
band(255, 170) = 170
bor(255, 170) = 255
bxor(255, 170) = 85
bnot(255) = -256
bsl(255, 2) = 1020
bsr(255, 2) = 63

Bitwise as operator:
255 &&& 170 = 170
255 ||| 170 = 255
255 ^^^ 170 = 85
~~~255 = -256
255 <<< 2 = 1020
255 >>> 2 = 63

```



## Erlang

All these operations are built-in functions except right arithmetic shift, left rotate, and right rotate.


```erlang

-module(bitwise_operations).

-export([test/0]).

test() ->
   A = 255,
   B = 170,
   io:format("~p band ~p = ~p\n",[A,B,A band B]),
   io:format("~p bor ~p = ~p\n",[A,B,A bor B]),
   io:format("~p bxor ~p = ~p\n",[A,B,A bxor B]),
   io:format("not ~p = ~p\n",[A,bnot A]),
   io:format("~p bsl ~p = ~p\n",[A,B,A bsl B]),
   io:format("~p bsr ~p = ~p\n",[A,B,A bsr B]).

```


outputs:

```erlang

255 band 170 = 170
255 bor 170 = 255
255 bxor 170 = 85
not 255 = -256
255 bsl 170 = 381627307539845370001346183518875822092557105621893120
255 bsr 170 = 0

```



=={{header|F_Sharp|F#}}==

```fsharp
let bitwise a b =
    printfn "a and b: %d" (a &&& b)
    printfn "a or  b: %d" (a ||| b)
    printfn "a xor b: %d" (a ^^^ b)
    printfn "not a: %d"   (~~~a)
    printfn "a shl b: %d" (a <<< b)
    printfn "a shr b: %d" (a >>> b)          // arithmetic shift
    printfn "a shr b: %d" ((uint32 a) >>> b) // logical shift
    // No rotation operators.
```



## Factor


```factor
"a=" "b=" [ write readln string>number ] bi@
{
    [ bitand "a AND b: " write . ]
    [ bitor "a OR b: " write . ]
    [ bitxor "a XOR b: " write . ]
    [ drop bitnot "NOT a: " write . ]
    [ abs shift "a asl b: " write . ]
    [ neg shift "a asr b: " write . ]
} 2cleave
```


outputs:

```factor
a=255
b=5
a AND b: 5
a OR b: 255
a XOR b: 250
NOT a: -256
a asl b: 8160
a asr b: 7
```

Currently rotation and logical shifts are not implemented.


## FALSE

Only AND, OR, and NOT are available.

```false
10 3
\$@$@$@$@\  { 3 copies }
"a & b = "&."
a | b  = "|."
~a = "%~."
"
```



## Forth


```forth
: arshift 0 ?do 2/ loop ;            \ 2/ is an arithmetic shift right by one bit (2* shifts left one bit)
: bitwise ( a b -- )
  cr ." a = " over . ." b = " dup .
  cr ." a and b = " 2dup and .
  cr ." a  or b = " 2dup  or .
  cr ." a xor b = " 2dup xor .
  cr ." not a = " over invert .
  cr ." a shl b = " 2dup lshift .
  cr ." a shr b = " 2dup rshift .
  cr ." a ashr b = " 2dup arshift .
  2drop ;
```

Rotation is not standard, but may be provided in particular Forth implementations, or as an assembly instruction in CODE words.


## Fortran

In ISO Fortran 90 and later the following BIT INTRINSIC functions are defined:

```fortran
integer :: i, j = -1, k = 42
logical :: a

i = bit_size(j)       ! returns the number of bits in the given INTEGER variable

! bitwise boolean operations on integers
i = iand(k, j)        ! returns bitwise AND of K and J
i = ior(k, j)         ! returns bitwise OR of K and J
i = ieor(k, j)        ! returns bitwise EXCLUSIVE OR of K and J
i = not(j)            ! returns bitwise NOT of J

! single-bit integer/logical operations (bit positions are zero-based)
a = btest(i, 4)       ! returns logical .TRUE. if bit position 4 of I is 1, .FALSE. if 0
i = ibclr(k, 8)       ! returns value of K with 8th bit position "cleared" (set to 0)
i = ibset(k, 13)      ! returns value of K with 13th bit position "set" (set to 1)

! multi-bit integer operations
i = ishft(k, j)       ! returns value of K shifted by J bit positions, with ZERO fill
                      !    (right shift if J < 0 and left shift if J > 0).
i = ishftc(k, j)      ! returns value of K shifted CIRCULARLY by J bit positions
                      !    (right circular shift if J < 0 and left if J > 0)
i = ishftc(k, j, 20)  ! returns value as before except that ONLY the 20 lowest order
                      !    (rightmost) bits are circularly shifted
i = ibits(k, 7, 8)    ! extracts 8 contiguous bits from K starting at position 7 and
                      !    returns them as the rightmost bits of an otherwise
                      !    zero-filled integer. For non-negative K this is
                      !    arithmetically equivalent to:   MOD((K / 2**7), 2**8)
```

The following INTRINSIC ELEMENTAL SUBROUTINE is also defined:

```fortran
 call mvbits(k, 2, 4, j, 0)  ! copy a sequence of 4 bits from k starting at bit 2 into j starting at bit 0
```



```fortran

program    bits_rosetta
implicit none

 call bitwise(14,3)

 contains

subroutine bitwise(a,b)
implicit none
 integer, intent(in):: a,b
 character(len=*), parameter :: fmt1 = '(2(a,i10))'
 character(len=*),parameter :: fmt2 = '(3(a,b32.32),i20)'

write(*,fmt1) 'input a=',a,' b=',b
write(*,fmt2) 'and : ', a,' &  ',b,' = ',iand(a, b),iand(a, b)
write(*,fmt2) 'or  : ', a,' |  ',b,' = ',ior(a, b),ior(a, b)
write(*,fmt2) 'xor : ', a,' ^  ',b,' = ',ieor(a, b),ieor(a, b)
write(*,fmt2) 'lsh : ', a,' << ',b,' = ',shiftl(a,b),shiftl(a,b) !since F2008, otherwise use ishft(a, abs(b))
write(*,fmt2) 'rsh : ', a,' >> ',b,' = ',shiftr(a,b),shiftr(a,b) !since F2008, otherwise use ishft(a, -abs(b))
write(*,fmt2) 'not : ', a,' ~  ',b,' = ',not(a),not(a)
write(*,fmt2) 'rot : ', a,' r  ',b,' = ',ishftc(a,-abs(b)),ishftc(a,-abs(b))

end subroutine bitwise

end program bits_rosetta

```

Output
<lang>
Input a=        14 b=         3
AND : 00000000000000000000000000001110 &  00000000000000000000000000000011 = 00000000000000000000000000000010                   2
OR  : 00000000000000000000000000001110 |  00000000000000000000000000000011 = 00000000000000000000000000001111                  15
XOR : 00000000000000000000000000001110 ^  00000000000000000000000000000011 = 00000000000000000000000000001101                  13
LSH : 00000000000000000000000000001110 << 00000000000000000000000000000011 = 00000000000000000000000001110000                 112
RSH : 00000000000000000000000000001110 >> 00000000000000000000000000000011 = 00000000000000000000000000000001                   1
NOT : 00000000000000000000000000001110 ~  00000000000000000000000000000011 = 11111111111111111111111111110001                 -15
ROT : 00000000000000000000000000001110 ~  00000000000000000000000000000011 = 11000000000000000000000000000001         -1073741823

```



## FreeBASIC


```freebasic

' FB 1.05.0 Win64 (Note the (U)Integer type is 64 bits)

' FB doesn't have built-in logical shift right or rotation operators
' but, as they're not difficult to implement, I've done so below.

Function lsr(x As Const Integer, y As Const Integer) As Integer
  Dim As UInteger z = x
  Return z Shr y
End Function

Function rol(x As Const Integer, y As Const UInteger) As Integer
  Dim z As Integer = x
  Dim high As Integer
  For i As Integer = 1 To y
    high = Bit(z, 63)
    For j As Integer = 62 To 0 Step -1
      If Bit(z, j) Then
        z = BitSet(z, j + 1)
      Else
        z = BitReset (z, j + 1)
      End If
    Next j
    If high Then
      z = BitSet(z, 0)
    Else
      z = BitReset(z, 0)
    End If
  Next i
  Return z
End Function

Function ror(x As Const Integer, y As Const UInteger) As Integer
  Dim z As Integer = x
  Dim low As Integer
  For i As Integer = 1 To y
    low = Bit(z, 0)
    For j As Integer = 1 To 63
      If Bit(z, j) Then
        z = BitSet(z, j - 1)
      Else
        z = BitReset (z, j - 1)
      End If
    Next j
    If low Then
      z = BitSet(z, 63)
    Else
      z = BitReset(z, 63)
    End If
  Next i
  Return z
End Function

Sub bitwise(x As Integer, y As Integer)
  Print "x       = "; x
  Print "y       = "; y
  Print "x AND y = "; x And y
  Print "x OR y  = "; x Or y
  Print "x XOR y = "; x XOr y
  Print "NOT x   = "; Not x
  Print "x SHL y = "; x Shl y
  Print "x SHR y = "; x Shr y
  Print "x LSR y = "; lsr(x, y)
  Print "x ROL y = "; rol(x, y)
  Print "x ROR y = "; ror(x, y)
End Sub

bitwise -15, 3
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

x       = -15
y       =  3
x AND y =  1
x OR y  = -13
x XOR y = -14
NOT x   =  14
x SHL y = -120
x SHR y = -2
x LSR y =  2305843009213693950
x ROL y = -113
x ROR y =  4611686018427387902

```



## Free Pascal


```pascal
program Bitwise;
{$mode objfpc}
var
  // Pascal uses a native int type as a default literal type
  // Make sure the operants work on an exact type.
  x:shortint = 2;
  y:ShortInt = 3;
begin
  Writeln('2 and 3 = ', x and y);
  Writeln('2 or 3 = ', x or y);
  Writeln('2 xor 3 = ', x xor y);
  Writeln('not 2 = ', not x);
  Writeln('2 shl 3 = ', x >> y);
  Writeln('2 shr 3 = ', x << y);
  writeln('2 rol 3 = ', rolbyte(x,y));
  writeln('2 ror 3 = ', rorbyte(x,y));
  writeln('2 sar 3 = ', sarshortint(x,y));
  Readln;
end.
```



## FutureBasic

FB does not have a bitwise symbol for not, but rather uses the "not" expression. It does not support predefined bitwise symbols for rotate left and rotate right, but functions in this demo provide that capability.

```futurebasic

include "ConsoleWindow"

// Set tab width for printing
def tab 1

local fn rotl( b as long, n as long ) as long
end fn = ( ( 2^n * b) mod 256) or (b > 127)

local fn rotr( b as long, n as long ) as long
end fn = (b >> n mod 32) or ( b << (32-n) mod 32)

local fn bitwise( a as long, b as long )
print "Input: a = "; a; "  b = "; b
print
print "AND  :", "a && b = ", bin$(a && b), ": "; a && b
print "NAND :", "a ^& b = ", bin$(a ^& b), ": "; a ^& b
print "OR   :", "a || b = ", bin$(a || b), ": "; a || b
print "NOR  :", "a ^| b = ", bin$(a ^| b), ": "; a ^| b
print "XOR  :", "a ^^ b = ", bin$(a ^^ b), ": "; a ^^ b
print "NOT  :", " not a = ", bin$( not a), ": ";  not a
print
print "Left shift   :", "a << b =", bin$(a << b), ": "; a << b
print "Right shift  :", "a >> b =", bin$(a >> b), ": "; a >> b
print
print "Rotate left  :", "fn rotl( a, b ) = ", bin$(fn rotl( a, b)), ": "; fn rotl( a, b )
print "Rotate right :", "fn rotr( a, b ) = ", bin$(fn rotr( a, b )),": "; fn rotr( a, b )
end fn

fn bitwise( 255, 2 )

```


Output:

```txt

Input: a =  255  b =  2

AND  : a && b =  00000000000000000000000000000010 :  2
NAND : a ^& b =  00000000000000000000000011111101 :  253
OR   : a || b =  00000000000000000000000011111111 :  255
NOR  : a ^| b =  11111111111111111111111111111111 : -1
XOR  : a ^^ b =  00000000000000000000000011111101 :  253
NOT  :  not a =  11111111111111111111111100000000 : -256

Left shift   : a << b = 00000000000000000000001111111100 :  1020
Right shift  : a >> b = 00000000000000000000000000111111 :  63

Rotate left  : fn rotl( a, b ) =  11111111111111111111111111111111 : -1
Rotate right : fn rotr( a, b ) =  11000000000000000000000000111111 : -1073741761

```



## Go


```go
package main

import "fmt"

func bitwise(a, b int16) {
	fmt.Printf("a:   %016b\n", uint16(a))
	fmt.Printf("b:   %016b\n", uint16(b))

	// Bitwise logical operations
	fmt.Printf("and: %016b\n", uint16(a&b))
	fmt.Printf("or:  %016b\n", uint16(a|b))
	fmt.Printf("xor: %016b\n", uint16(a^b))
	fmt.Printf("not: %016b\n", uint16(^a))

	if b < 0 {
		fmt.Println("Right operand is negative, but all shifts require an unsigned right operand (shift distance).")
		return
	}
	ua := uint16(a)
	ub := uint32(b)

	// Logical shifts (unsigned left operand)
	fmt.Printf("shl: %016b\n", uint16(ua<<ub))
	fmt.Printf("shr: %016b\n", uint16(ua>>ub))

	// Arithmetic shifts (signed left operand)
	fmt.Printf("las: %016b\n", uint16(a<<ub))
	fmt.Printf("ras: %016b\n", uint16(a>>ub))

	// Rotations
	fmt.Printf("rol: %016b\n", uint16(a<<ub|int16(uint16(a)>>(16-ub))))
	fmt.Printf("ror: %016b\n", uint16(int16(uint16(a)>>ub)|a<<(16-ub)))
}

func main() {
	var a, b int16 = -460, 6
	bitwise(a, b)
}
```

Output:

```txt
a:   1111111000110100
b:   0000000000000110
and: 0000000000000100
or:  1111111000110110
xor: 1111111000110010
not: 0000000111001011
shl: 1000110100000000
shr: 0000001111111000
las: 1000110100000000
ras: 1111111111111000
rol: 1000110100111111
ror: 1101001111111000
```



## Groovy


```groovy
def bitwise = { a, b ->
    println """
a & b   = ${a} & ${b}   = ${a & b}
a | b   = ${a} | ${b}   = ${a | b}
a ^ b   = ${a} ^ ${b}   = ${a ^ b}
~ a     = ~ ${a}     = ${~ a}
a << b  = ${a} << ${b}  = ${a << b}
a >> b  = ${a} >> ${b}  = ${a >> b}         arithmetic (sign-preserving) shift
a >>> b = ${a} >>> ${b} = ${a >>> b}  logical (zero-filling) shift
"""
}
```


Program:

```groovy
bitwise(-15,3)
```


Output:

```txt
a & b   = -15 & 3   = 1
a | b   = -15 | 3   = -13
a ^ b   = -15 ^ 3   = -14
~ a     = ~ -15     = 14
a << b  = -15 << 3  = -120
a >> b  = -15 >> 3  = -2         arithmetic (sign-preserving) shift
a >>> b = -15 >>> 3 = 536870910  logical (zero-filling) shift
```



## Haskell


The operations in ''Data.Bits'' work on ''Int'', ''Integer'', and any of the sized integer and word types.

```haskell
import Data.Bits

bitwise :: Int -> Int -> IO ()
bitwise a b =
  mapM_
    print
    [ a .&. b
    , a .|. b
    , a `xor` b
    , complement a
    , shiftL a b -- left shift
    , shiftR a b -- arithmetic right shift
    , shift a b -- You can also use the "unified" shift function;
      -- positive is for left shift, negative is for right shift
    , shift a (-b)
    , rotateL a b -- rotate left
    , rotateR a b -- rotate right
    , rotate a b -- You can also use the "unified" rotate function;
      -- positive is for left rotate, negative is for right rotate
    , rotate a (-b)
    ]

main :: IO ()
main = bitwise 255 170
```

{{Out}}

```txt
170
255
85
-256
0
0
0
0
1121501860331520
1069547520
1121501860331520
1069547520
```


If you were shifting Words (unsigned integers) instead of Ints, then the shift would be automatically logical shifts:
 import Data.Word
 print $ shiftL (-1 :: Word) 1
 print $ shiftR (-1 :: Word) 1


## HicEst

There is no rotate and no shift support built in to HicEst

```hicest
i = IAND(k, j)
i = IOR( k, j)
i = IEOR(k, j)
i = NOT( k   )
```



## HPPPL


```hpppl
EXPORT BITOPS(a, b)
BEGIN
	PRINT(BITAND(a, b));
	PRINT(BITOR(a, b));
	PRINT(BITXOR(a, b));
	PRINT(BITNOT(a));
	PRINT(BITSL(a, b));
	PRINT(BITSR(a, b));
	// HPPPL has no builtin rotates or arithmetic right shift.
END;
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
bitdemo(255,2)
bitdemo(-15,3)
end

procedure bitdemo(i,i2)
   write()
   demowrite("i",i)
   demowrite("i2",i2)
   demowrite("complement i",icom(i))
   demowrite("i or i2",ior(i,i2))
   demowrite("i and i2",iand(i,i2))
   demowrite("i xor i2",ixor(i,i2))
   demowrite("i shift " || i2,ishift(i,i2))
   demowrite("i shift -" || i2,ishift(i,-i2))
   return
end

procedure demowrite(vs,v)
return write(vs, ": ", v, " = ", int2bit(v),"b")
end
```


Icon/Unicon implements bitwise operations on integers.  Because integers can be transparently large integers operations that require fixed sizes don't make sense and aren't defined.  These include rotation and logical shifting (shift is arithmetic) .  Please note also that 'not' is a reserved word and the negation function is 'icom'

Sample output:

```txt

i: 255 = 11111111b
i2: 2 = 10b
complement i: -256 = -100000000b
i or i2: 255 = 11111111b
i and i2: 2 = 10b
i xor i2: 253 = 11111101b
i shift 2: 1020 = 1111111100b
i shift -2: 63 = 111111b


i: -15 = -1111b
i2: 3 = 11b
complement i: 14 = 1110b
i or i2: -13 = -1101b
i and i2: 1 = 1b
i xor i2: -14 = -1110b
i shift 3: -120 = -1111000b
i shift -3: -2 = -10b
```



## Inform 6

Inform 6 has no xor or rotate operators.  It also has no shift operators, although the Z-machine, its usual target architecture, does.  These can be accessed with inline assembly, which is done here.


```Inform 6
[ bitwise a b temp;
  print "a and b: ", a & b, "^";
  print "a or b: ", a | b, "^";
  print "not a: ", ~a, "^";
  @art_shift a b -> temp;
  print "a << b (arithmetic): ", temp, "^";
  temp = -b;
  @art_shift a temp -> temp;
  print "a >> b (arithmetic): ", temp, "^";
  @log_shift a b -> temp;
  print "a << b (logical): ", temp, "^";
  temp = -b;
  @log_shift a temp -> temp;
  print "a >> b (logical): ", temp, "^";
];

```


## J


Here are the "[http://www.jsoftware.com/help/dictionary/dbdotn.htm bitwise operators]":


```j
bAND=:  17 b.  NB. 16+#.0 0 0 1
bOR=:   23 b.  NB. 16+#.0 1 1 1
bXOR=:  22 b.  NB. 16+#.0 1 1 0
b1NOT=: 28 b.  NB. 16+#.1 1 0 0
bLshift=:  33 b.~ NB. see http://www.jsoftware.com/help/release/bdot.htm
bRshift=:  33 b.~ -
bRAshift=: 34 b.~ -
bLrot=:    32 b.~
bRrot=:    32 b.~ -
```


And here is a routine which takes a list of bitwise operators and two numbers and displays a table of results from combining those two numbers with each of the operators:


```j
bitwise=: 1 :0
:
  smoutput (((":x),"1' ',.(>u),.' '),"1":y),"1' => ',"1'.X'{~#:x u`:0 y
)
```


And here they are in action:


```j
   254 bAND`bOR`bXOR`b1NOT`bLshift`bRshift`bRAshift`bLrot`bRrot bitwise 3
254 bAND     3 => ............................X.
254 bOR      3 => ......................XXXXXXXX
254 bXOR     3 => ......................XXXXXX.X
254 b1NOT    3 => XXXXXXXXXXXXXXXXXXXXXX.......X
254 bLshift  3 => ...................XXXXXXX....
254 bRshift  3 => .........................XXXXX
254 bRAshift 3 => .........................XXXXX
254 bLrot    3 => ...................XXXXXXX....
254 bRrot    3 => .........................XXXXX
```


Further test

```j

bXOR/ 3333 5555 7777 9999
8664

```



## Java


```java
public static void bitwise(int a, int b){
  System.out.println("a AND b: " + (a & b));
  System.out.println("a OR b: "+ (a | b));
  System.out.println("a XOR b: "+ (a ^ b));
  System.out.println("NOT a: " + ~a);
  System.out.println("a << b: " + (a << b)); // left shift
  System.out.println("a >> b: " + (a >> b)); // arithmetic right shift
  System.out.println("a >>> b: " + (a >>> b)); // logical right shift
  System.out.println("a rol b: " + Integer.rotateLeft(a, b)); //rotate left, Java 1.5+
  System.out.println("a ror b: " + Integer.rotateRight(a, b)); //rotate right, Java 1.5+
}
```

All of the operators may be combined with the <tt>=</tt> operator to save space. For example, the following lines each do the same thing:

```java
a <<= 3;
a = a << 3;
a *= 8; //2 * 2 * 2 = 8
a = a * 8;
```



## JavaScript

There are no integers in Javascript, but there are still bitwise operators. They will convert their number operands into integers before performing they task. In other languages, these operators are very close to the hardware and very fast. In JavaScript, they are very far from the hardware and very slow and rarely used.


```javascript
function bitwise(a, b){
   alert("a AND b: " + (a & b));
   alert("a OR b: "+ (a | b));
   alert("a XOR b: "+ (a ^ b));
   alert("NOT a: " + ~a);
   alert("a << b: " + (a << b)); // left shift
   alert("a >> b: " + (a >> b)); // arithmetic right shift
   alert("a >>> b: " + (a >>> b)); // logical right shift
}
```



## Julia


```julia
# Version 5.2
@show 1 & 2   # AND
@show 1 | 2   # OR
@show 1 ^ 2   # XOR -- for Julia 6.0 the operator is `⊻`
@show ~1      # NOT
@show 1 >>> 2 # SHIFT RIGHT (LOGICAL)
@show 1 >> 2  # SHIFT RIGHT (ARITMETIC)
@show 1 << 2  # SHIFT LEFT (ARITMETIC/LOGICAL)

A = BitArray([true, true, false, false, true])
@show A ror(A,1) ror(A,2) ror(A,5) # ROTATION RIGHT
@show rol(A,1) rol(A,2) rol(A,5) # ROTATION LEFT

```


{{out}}

```txt

1 & 2 = 0
1 | 2 = 3
1 ^ 2 = 1
~1 = -2
1 >>> 2 = 0
1 >> 2 = 0
1 << 2 = 4
A = Bool[true,true,false,false,true]
ror(A,1) = Bool[true,true,true,false,false]
ror(A,2) = Bool[false,true,true,true,false]
ror(A,5) = Bool[true,true,false,false,true]
rol(A,1) = Bool[true,false,false,true,true]
rol(A,2) = Bool[false,false,true,true,true]
rol(A,5) = Bool[true,true,false,false,true]

```



## Kotlin


```scala
/*  for symmetry with Kotlin's other binary bitwise operators
    we wrap Java's 'rotate' methods as infix functions */
infix fun Int.rol(distance: Int): Int = Integer.rotateLeft(this, distance)
infix fun Int.ror(distance: Int): Int = Integer.rotateRight(this, distance)

fun main(args: Array<String>) {
    // inferred type of x and y is Int i.e. 32 bit signed integers
    val x = 10
    val y = 2
    println("x       = $x")
    println("y       = $y")
    println("NOT x   = ${x.inv()}")
    println("x AND y = ${x and y}")
    println("x OR  y = ${x or y}")
    println("x XOR y = ${x xor y}")
    println("x SHL y = ${x shl y}")
    println("x ASR y = ${x shr y}")   // arithmetic shift right (sign bit filled)
    println("x LSR y = ${x ushr y}")  // logical shift right    (zero filled)
    println("x ROL y = ${x rol y}")
    println("x ROR y = ${x ror y}")
}
```


{{out}}

```txt

x       = 10
y       = 2
NOT x   = -11
x AND y = 2
x OR  y = 10
x XOR y = 8
x SHL y = 40
x ASR y = 2
x LSR y = 2
x ROL y = 40
x ROR y = -2147483646

```



## LFE


All these operations are built-in functions except right arithmetic shift, left rotate, and right rotate.

```lisp
(defun bitwise (a b)
    (io:format '"~p~n" (list (band a b)))
    (io:format '"~p~n" (list (bor a b)))
    (io:format '"~p~n" (list (bxor a b)))
    (io:format '"~p~n" (list (bnot a)))
    (io:format '"~p~n" (list (bsl a b)))
    (io:format '"~p~n" (list (bsr a b))))

(defun d2b
  (x) (integer_to_list x 2))

(defun bitwise
  ((a b 'binary)
    (io:format '"(~s ~s ~s): ~s~n"
               (list "band" (d2b a) (d2b b) (d2b (band a b))))
    (io:format '"(~s ~s ~s): ~s~n"
               (list "bor" (d2b a) (d2b b) (d2b (bor a b))))
    (io:format '"(~s ~s ~s): ~s~n"
               (list "bxor" (d2b a) (d2b b) (d2b (bxor a b))))
    (io:format '"(~s ~s): ~s~n"
               (list "bnot" (d2b a) (d2b (bnot a))))
    (io:format '"(~s ~s ~s): ~s~n"
               (list "bsl" (d2b a) (d2b b) (d2b (bsl a b))))
    (io:format '"(~s ~s ~s): ~s~n"
               (list "bsr" (d2b a) (d2b b) (d2b (bsr a b))))))

```


Example usage:

```lisp

> (bitwise 255 170)
170
255
85
-256
381627307539845370001346183518875822092557105621893120
0
ok
> (bitwise 255 170 'binary)
(band 11111111 10101010): 10101010
(bor 11111111 10101010): 11111111
(bxor 11111111 10101010): 1010101
(bnot 11111111): -100000000
(bsl 11111111 10101010): 1111111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
(bsr 11111111 10101010): 0
ok
>

```



## Liberty BASIC

Written as functions.

```lb

'   bitwise operations on byte-sized variables

v =int( 256 *rnd( 1))

s = 1

print "Shift            ="; s; " place."
print
print "Number as dec. = "; v; " & as 8-bits byte = ", dec2Bin$( v)
print "NOT  as dec.          =  "; bitInvert(   v),    dec2Bin$( bitInvert(   v))
print "Shifted left  as dec. =  "; shiftLeft(   v, s), dec2Bin$( shiftLeft(   v, s))
print "Shifted right as dec. =  "; shiftRight(  v, s), dec2Bin$( shiftRight(  v, s))
print "Rotated left  as dec. =  "; rotateLeft(  v, s), dec2Bin$( rotateLeft(  v, s))
print "Rotated right as dec. =  "; rotateRight( v, s), dec2Bin$( rotateRight( v, s))

end

function shiftLeft( b, n)
    shiftLeft =( b *2^n) and 255
end function

function shiftRight( b, n)
    shiftRight =int(b /2^n)
end function

function rotateLeft( b, n)
    rotateLeft = (( 2^n *b) mod 256) or ( b >127)
end function

function rotateRight( b, n)
    rotateRight = (128*( b and 1)) or int( b /2)
end function

function bitInvert( b)
    bitInvert =b xor 255
end function

function dec2Bin$( num) '   Given an integer decimal 0 <--> 255, returns binary equivalent as a string
    n =num
    dec2Bin$ =""
    while ( num >0)
        dec2Bin$    =str$( num mod 2) +dec2Bin$
        num         =int(  num /2)
    wend
    dec2Bin$ =right$( "00000000" +dec2Bin$, 8)
end function

```



## Lingo

Lingo has built-in functions for bitwise AND, OR, XOR and NOT:

```lingo
put bitAND(2,7)
put bitOR(2,7)
put bitXOR(2,7)
put bitNOT(7)
```

Bit shifting and rotating has to be implemented by custom functions.


## LiveCode


```LiveCode
put "and:" && (255 bitand 2) & comma into bitops
put " or:" && (255 bitor 2) & comma after bitops
put " xor:" && (255 bitxor 2) & comma after bitops
put " not:" && (bitnot 255) after bitops
put bitops

-- Ouput
and: 2, or: 255, xor: 253, not: 4294967040
```

LiveCode does not provide built-in bit-shift operations.


## LLVM


```llvm
; ModuleID = 'test.o'
;e means little endian
;p: { pointer size : pointer abi : preferred alignment for pointers }
;i same for integers
;v is for vectors
;f for floats
;a for aggregate types
;s for stack objects
;n: {size:size:size...}, best integer sizes
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32"
;this was compiled with mingw32; thus it must be linked to an ABI compatible c library
target triple = "i386-mingw32"

@.str = private constant [13 x i8] c"a and b: %d\0A\00", align 1 ; <[13 x i8]*> [#uses=1]
@.str1 = private constant [12 x i8] c"a or b: %d\0A\00", align 1 ; <[12 x i8]*> [#uses=1]
@.str2 = private constant [13 x i8] c"a xor b: %d\0A\00", align 1 ; <[13 x i8]*> [#uses=1]
@.str3 = private constant [11 x i8] c"not a: %d\0A\00", align 1 ; <[11 x i8]*> [#uses=1]
@.str4 = private constant [12 x i8] c"a << n: %d\0A\00", align 1 ; <[12 x i8]*> [#uses=1]
@.str5 = private constant [12 x i8] c"a >> n: %d\0A\00", align 1 ; <[12 x i8]*> [#uses=1]
@.str6 = private constant [12 x i8] c"c >> b: %d\0A\00", align 1 ; <[12 x i8]*> [#uses=1]

;A function that will do many bitwise opreations to two integer arguments, %a and %b
define void @bitwise(i32 %a, i32 %b) nounwind {
;entry block
entry:
  ;Register to store (a & b)
  %0 = and i32 %b, %a                             ; <i32> [#uses=1]
  ;print the results
  %1 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([13 x i8]* @.str, i32 0, i32 0), i32 %0) nounwind ; <i32> [#uses=0]
  ;Register to store (a | b)
  %2 = or i32 %b, %a                              ; <i32> [#uses=1]
  ;print the results
  %3 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @.str1, i32 0, i32 0), i32 %2) nounwind ; <i32> [#uses=0]
  ;Register to store (a ^ b)
  %4 = xor i32 %b, %a                             ; <i32> [#uses=1]
  ;print the results
  %5 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([13 x i8]* @.str2, i32 0, i32 0), i32 %4) nounwind ; <i32> [#uses=0]
  ;Register to store (~a)
  %not = xor i32 %a, -1                           ; <i32> [#uses=1]
  ;print the results
  %6 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([11 x i8]* @.str3, i32 0, i32 0), i32 %not) nounwind ; <i32> [#uses=0]
  ;Register to store (a << b)
  %7 = shl i32 %a, %b                             ; <i32> [#uses=1]
  ;print the results
  %8 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @.str4, i32 0, i32 0), i32 %7) nounwind ; <i32> [#uses=0]
  ;Register to store (a >> b) (a is signed)
  %9 = ashr i32 %a, %b                            ; <i32> [#uses=1]
  ;print the results
  %10 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @.str5, i32 0, i32 0), i32 %9) nounwind ; <i32> [#uses=0]
  ;Register to store (c >> b), where c is unsiged (eg. logical right shift)
  %11 = lshr i32 %a, %b                           ; <i32> [#uses=1]
  ;print the results
  %12 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @.str6, i32 0, i32 0), i32 %11) nounwind ; <i32> [#uses=0]

  ;terminator instruction
  ret void
}

;Declare external fuctions
declare i32 @printf(i8* nocapture, ...) nounwind
```



## Logo

{{works with|UCB Logo}}

```logo
to bitwise :a :b
  (print [a and b:] BitAnd :a :b)
  (print [a or b:] BitOr :a :b)
  (print [a xor b:] BitXor :a :b)
  (print [not a:] BitNot :a)
  ; shifts are to the left if positive, to the right if negative
  (print [a lshift b:] LShift :a :b)
  (print [a lshift -b:] LShift :a minus :b)
  (print [-a ashift -b:] AShift minus :a minus :b)
end
bitwise 255 5
```

The output of this program is:

```logo
a and b: 5
a or b: 255
a xor b: 250
not a: -256
a lshift b: 8160
a lshift -b: 7
-a ashift -b: -8
```



## Lua


LuaBitOp implements bitwise functionality for Lua:


```lua
local bit = require"bit"

local vb = {
  0, 1, -1, 2, -2, 0x12345678, 0x87654321,
  0x33333333, 0x77777777, 0x55aa55aa, 0xaa55aa55,
  0x7fffffff, 0x80000000, 0xffffffff
}

local function cksum(name, s, r)
  local z = 0
  for i=1,#s do z = (z + string.byte(s, i)*i) % 2147483629 end
  if z ~= r then
    error("bit."..name.." test failed (got "..z..", expected "..r..")", 0)
  end
end

local function check_unop(name, r)
  local f = bit[name]
  local s = ""
  if pcall(f) or pcall(f, "z") or pcall(f, true) then
    error("bit."..name.." fails to detect argument errors", 0)
  end
  for _,x in ipairs(vb) do s = s..","..tostring(f(x)) end
  cksum(name, s, r)
end

local function check_binop(name, r)
  local f = bit[name]
  local s = ""
  if pcall(f) or pcall(f, "z") or pcall(f, true) then
    error("bit."..name.." fails to detect argument errors", 0)
  end
  for _,x in ipairs(vb) do
    for _,y in ipairs(vb) do s = s..","..tostring(f(x, y)) end
  end
  cksum(name, s, r)
end

local function check_binop_range(name, r, yb, ye)
  local f = bit[name]
  local s = ""
  if pcall(f) or pcall(f, "z") or pcall(f, true) or pcall(f, 1, true) then
    error("bit."..name.." fails to detect argument errors", 0)
  end
  for _,x in ipairs(vb) do
    for y=yb,ye do s = s..","..tostring(f(x, y)) end
  end
  cksum(name, s, r)
end

local function check_shift(name, r)
  check_binop_range(name, r, 0, 31)
end

-- Minimal sanity checks.
assert(0x7fffffff == 2147483647, "broken hex literals")
assert(0xffffffff == -1 or 0xffffffff == 2^32-1, "broken hex literals")
assert(tostring(-1) == "-1", "broken tostring()")
assert(tostring(0xffffffff) == "-1" or tostring(0xffffffff) == "4294967295", "broken tostring()")

-- Basic argument processing.
assert(bit.tobit(1) == 1)
assert(bit.band(1) == 1)
assert(bit.bxor(1,2) == 3)
assert(bit.bor(1,2,4,8,16,32,64,128) == 255)

```


The ''RiscLua'' dialect, for [http://lua.riscos.org.uk/ '''RISC OS'''], has
32-bit integers as the default number type. It provides binary operations
& (and), | (or), ^^ (xor), << (logical shift left), >> (logical shift right)
and a unary operation ~ (negate).


## LSE64

{{incorrect|LSE64|No reason given.}}

```lse64
over : 2 pick
2dup : over over

bitwise : \
  " A=" ,t over ,h sp " B=" ,t dup ,h nl \
  " A and B=" ,t 2dup & ,h nl \
  " A  or B=" ,t 2dup | ,h nl \
  " A xor B=" ,t over ^ ,h nl \
  " not A="  ,t      ~ ,h nl

\ a \ 7 bitwise   # hex literals
```



## Maple


```Maple

with(Bits):
bit:=proc(A,B)
local a,b,c,d,e,f,g,h,i,x,bitpow;
bitpow := 2^B:
a:=And(A,B);
b:=Not(A);
c:=Or(A,B);
d:=Xor(A,B);
#Left Shift
e:= irem(2*A,bitpow);
#Right Shift
f := iquo(A,2);
#Left Rotate
g:= irem(2*A,bitpow,'x')+x;
#Rightarithshift
i:= iquo(A,2)+bitpow/2*irem(A,bitpow/2);
return a,b,c,d,e,f,g,i;
end proc;

```


=={{header|Mathematica}}/ {{header|Wolfram Language}}==
Most functions are built-in or can be made really easily:

```Mathematica
(*and xor and or*)
BitAnd[integer1, integer2]
BitXor[integer1, integer2]
BitOr[integer1, integer2]

(*logical not*)
BitNot[integer1]

(*left and right shift*)
BitShiftLeft[integer1]
BitShiftRight[integer1]

(*rotate digits left and right*)
FromDigits[RotateLeft[IntegerDigits[integer1, 2]], 2]
FromDigits[RotateRight[IntegerDigits[integer1, 2]], 2]

(*right arithmetic shift*)
FromDigits[Prepend[Most[#], #[[1]]], 2] &[IntegerDigits[integer1, 2]]
```

The function BitShiftLeft, BitShiftRight, RotateRight, RotateLeft all take a second argument, which is the displacement, by default it is set to 1. BitAnd, BitXor and BitOr can handle more than 2 arguments:

```Mathematica
BitXor[3333, 5555, 7777, 9999]
```

gives back:

```Mathematica>8664</lang


=={{header|MATLAB}} / {{header|Octave}}==
Newer versions of MATLAB have even more bitwise operations than those demonstrated here. A complete list of bitwise operations for the newest version of MATLAB can be found at [http://www.mathworks.com/help/toolbox/fixedpoint/ref/f20333.html#bp7caxc-42 MathWorks]


```MATLAB
function bitwiseOps(a,b)

    disp(sprintf('%d and %d = %d', [a b bitand(a,b)]));
    disp(sprintf('%d or %d = %d', [a b bitor(a,b)]));
    disp(sprintf('%d xor %d = %d', [a b bitxor(a,b)]));
    disp(sprintf('%d << %d = %d', [a b bitshift(a,b)]));
    disp(sprintf('%d >> %d = %d', [a b bitshift(a,-b)]));

end
```


Output:

```MATLAB>>
 bitwiseOps(255,2)
255 and 2 = 2
255 or 2 = 255
255 xor 2 = 253
255 << 2 = 1020
255 >> 2 = 63
```



## Maxima


```maxima
load(functs)$

a: 3661$
b: 2541$

logor(a, b);
/* 4077 */

logand(a, b);
/* 2125 */

logxor(a, b);
/* 1952 */

/* NOT(x) is simply -x - 1
-a - 1;
/* -3662 */

logor(a, -a - 1);
/* -1 */

logand(a, -a - 1);
/* 0 */
```



## MAXScript


```maxscript
fn bitwise a b =
(
    format "a and b: %\n" (bit.and a b)
    format "a or b: %\n" (bit.or a b)
    format "a xor b: %\n" (bit.xor a b)
    format "not a: %\n" (bit.not a)
    format "Left shift a: %\n" (bit.shift a b)
    format "Right shift a: %\n" (bit.shift a -b)
)

bitwise 255 170
```


MAXScript doesn't have arithmetic shift or rotate operations.


## ML/I

ML/I only supports bitwise AND and OR operations. These are available from version CKD onwards.


```ML/I
MCSKIP "WITH" NL
"" Bitwise operations
"" assumes macros on input stream 1, terminal on stream 2
MCSKIP MT,<>
MCINS %.
MCDEF SL SPACES NL AS <MCSET T1=%A1.
MCSET T2=%A2.
a and b = %%T1.&%T2..
a or b  = %%T1.|%T2..
The other operators are not supported.
MCSET S10=0
>
MCSKIP SL WITH *
MCSET S1=1
*MCSET S10=2

```


=={{header|Modula-3}}==


```modula3
MODULE Bitwise EXPORTS Main;

IMPORT IO, Fmt, Word;

VAR c: Word.T;

PROCEDURE Bitwise(a, b: INTEGER) =
  BEGIN
    IO.Put("a AND b: " & Fmt.Int(Word.And(a, b)) & "\n");
    IO.Put("a OR b: " & Fmt.Int(Word.Or(a, b)) & "\n");
    IO.Put("a XOR b: " & Fmt.Int(Word.Xor(a, b)) & "\n");
    IO.Put("NOT a: " & Fmt.Int(Word.Not(a)) & "\n");
    c := a;
    IO.Put("c LeftShift b: " & Fmt.Unsigned(Word.LeftShift(c, b)) & "\n");
    IO.Put("c RightShift b: " & Fmt.Unsigned(Word.RightShift(c, b)) & "\n");
    IO.Put("c LeftRotate b: " & Fmt.Unsigned(Word.LeftRotate(c, b)) & "\n");
    IO.Put("c RightRotate b: " & Fmt.Unsigned(Word.RightRotate(c, b)) & "\n");
  END Bitwise;

BEGIN
  Bitwise(255, 5);
END Bitwise.
```


Output:

```txt

a AND b: 5
a OR b: 255
a XOR b: 250
NOT a: -256
c LeftShift b: 1fe0
c RightShift b: 7
c LeftRotate b: 1fe0
c RightRotate b: f8000007

```



## Neko


```ActionScript
/**
 <doc>
   <h2>bitwise operations</h2>
   <p>Tectonics:

  nekoc bitwise.neko

  neko bitwise</p>
 </doc>
*/

// Neko is a signed 31 bit integer VM, full 32 bit requires builtins
var int32_new = $loader.loadprim("std@int32_new", 1);
var int32_and = $loader.loadprim("std@int32_and", 2);
var int32_or = $loader.loadprim("std@int32_or", 2);
var int32_xor = $loader.loadprim("std@int32_xor", 2);
var int32_shl = $loader.loadprim("std@int32_shl", 2);
var int32_shr = $loader.loadprim("std@int32_shr", 2);
var int32_ushr = $loader.loadprim("std@int32_ushr", 2);
var int32_complement = $loader.loadprim("std@int32_complement", 1);

// Function to show bitwise operations on a,b
var bitwise = function(a, b) {
  var ia = int32_new(a);
  var ib = int32_new(b);

  $print("Neko 32 bit integer library\n");
  $print("a AND b: ", a, " ", b, " ", int32_and(ia, ib), "\n");
  $print("a OR b:  ", a, " ", b, " ", int32_or(ia, ib), "\n");
  $print("a XOR b: ", a, " ", b, " ", int32_xor(ia, ib), "\n");
  $print("ones complement a:   ", a, " ", int32_complement(ia), "\n");
  $print("a SHL b: ", a, " ", b, " ", int32_shl(ia, ib), "\n");
  $print("a SHR b: ", a, " ", b, " ", int32_shr(ia, ib), "\n");
  $print("a USHR b: ", a, " ", b, " ", int32_ushr(ia, ib), "\n");
  $print("a ROL b: is not directly supported in Neko Int32\n");
  $print("a ROR b: is not directly supported in Neko Int32\n");

  $print("\nNormal Neko 31 bit signed integers\n");
  a = $int(a);
  b = $int(b);
  $print("a AND b: ", a, " ", b, " ", a & b, "\n");
  $print("a OR  b: ", a, " ", b, " ", a | b, "\n");
  $print("a XOR b: ", a, " ", b, " ", a ^ b, "\n");
  $print("NOT a: is not directly supported in Neko syntax\n");
  $print("a SHL b: ", a, " ", b, " ", a << b, "\n");
  $print("a SHR b: ", a, " ", b, " ", a >> b, "\n");
  $print("a USHR b: ", a, " ", b, " ", a >>> b, "\n");
  $print("a ROL b: is not directly supported in Neko syntax\n");
  $print("a ROR b: is not directly supported in Neko syntax\n");
}

// Pass command line arguments to the demo function
// initially as float, to ensure no internal bit truncation
var a = $float($loader.args[0]);
var b = $float($loader.args[1]);
if a == null a = 0;
if b == null b = 0;

bitwise(a,b);
```


{{out}}

```txt
prompt$ nekoc bitwise.neko
prompt$ neko bitwise 0x7fffffff 2
Neko 32 bit integer library
a AND b: 2147483647 2 2
a OR b:  2147483647 2 2147483647
a XOR b: 2147483647 2 2147483645
ones complement a:   2147483647 -2147483648
a SHL b: 2147483647 2 -4
a SHR b: 2147483647 2 536870911
a USHR b: 2147483647 2 536870911
a ROL b: is not directly supported in Neko Int32
a ROR b: is not directly supported in Neko Int32

Normal Neko 31 bit signed integers
a AND b: -1 2 2
a OR  b: -1 2 -1
a XOR b: -1 2 -3
NOT a: is not directly supported in Neko syntax
a SHL b: -1 2 -4
a SHR b: -1 2 -1
a USHR b: -1 2 1073741823
a ROL b: is not directly supported in Neko syntax
a ROR b: is not directly supported in Neko syntax
```



## Nemerle


```Nemerle
def i = 255;
def j = 2;

WriteLine($"$i and $j is $(i & j)");
WriteLine($"$i or $j is $(i | j)");
WriteLine($"$i xor $j is $(i ^ j)");
WriteLine($"not $i is $(~i)");
WriteLine($"$i lshift $j is $(i << j)");
WriteLine($"$i arshift $j is $(i >> j)");          // When the left operand of the >> operator is of a signed integral type,
                                                   // the operator performs an arithmetic shift right
WriteLine($"$(i :> uint) rshift $j is $(c >> j)"); // When the left operand of the >> operator is of an unsigned integral type,
                                                   // the operator performs a logical shift right
// there are no rotation operators in Nemerle, but you could define your own w/ a macro if you really wanted it
```



## Nim


```nim
proc bitwise(a, b) =
  echo "a and b: " , a and b
  echo "a or b: ", a or b
  echo "a xor b: ", a xor b
  echo "not a: ", not a
  echo "a << b: ", a shl b
  echo "a >> b: ", a shr b
```



## NSIS

All bitwise operations in NSIS are handled by the [http://nsis.sourceforge.net/Docs/Chapter4.html#4.9.10.2 IntOp] instruction.

```nsis
Function Bitwise
	Push $0
	Push $1
	Push $2
	StrCpy $0 7
	StrCpy $1 2

	IntOp $2 $0 & $1
	DetailPrint "Bitwise AND: $0 & $1 = $2"
	IntOp $2 $0 | $1
	DetailPrint "Bitwise OR: $0 | $1 = $2"
	IntOp $2 $0 ^ $1
	DetailPrint "Bitwise XOR: $0 ^ $1 = $2"
	IntOp $2 $0 ~
	DetailPrint "Bitwise NOT (negate in NSIS docs): ~$0 = $2"
	DetailPrint "There are no Arithmetic shifts in NSIS"
	IntOp $2 $0 >> $1
	DetailPrint "Right Shift: $0 >> 1 = $2"
	IntOp $2 $0 << $1
	DetailPrint "Left Shift: $0 << $1 = $2"
	DetailPrint "There are no Rotates in NSIS"


	Pop $2
	Pop $1
	Pop $0
FunctionEnd
```


=={{header|Oberon-2}}==
{{Works with|oo2c version 2}}

```oberon2

MODULE Bitwise;
IMPORT
  SYSTEM,
  Out;

PROCEDURE Do(a,b: LONGINT);
VAR
  x,y: SET;
BEGIN
  x := SYSTEM.VAL(SET,a);y := SYSTEM.VAL(SET,b);
  Out.String("a and b :> ");Out.Int(SYSTEM.VAL(LONGINT,x * y),0);Out.Ln;
  Out.String("a or b  :> ");Out.Int(SYSTEM.VAL(LONGINT,x + y),0);Out.Ln;
  Out.String("a xor b :> ");Out.Int(SYSTEM.VAL(LONGINT,x / y),0);Out.Ln;
  Out.String("a and ~b:> ");Out.Int(SYSTEM.VAL(LONGINT,x - y),0);Out.Ln;
  Out.String("~a      :> ");Out.Int(SYSTEM.VAL(LONGINT,-x),0);Out.Ln;
  Out.String("a left shift b :> ");Out.Int(SYSTEM.VAL(LONGINT,SYSTEM.LSH(x,b)),0);Out.Ln;
  Out.String("a right shift b :> ");Out.Int(SYSTEM.VAL(LONGINT,SYSTEM.LSH(x,-b)),0);Out.Ln;
  Out.String("a left rotate b :> ");Out.Int(SYSTEM.VAL(LONGINT,SYSTEM.ROT(x,b)),0);Out.Ln;
  Out.String("a right rotate b :> ");Out.Int(SYSTEM.VAL(LONGINT,SYSTEM.ROT(x,-b)),0);Out.Ln;
  Out.String("a arithmetic left shift b :> ");Out.Int(SYSTEM.VAL(LONGINT,ASH(a,b)),0);Out.Ln;
  Out.String("a arithmetic right shift b :> ");Out.Int(SYSTEM.VAL(LONGINT,ASH(a,-b)),0);Out.Ln
END Do;

BEGIN
  Do(10,2);
END Bitwise.

```

{{out}}

```txt

a and b :> 2
a or b  :> 10
a xor b :> 8
a and ~b:> 8
~a      :> -11
a left shift b :> 40
a right shift b :> 2
a left rotate b :> 40
a right rotate b :> -2147483646
a arithmetic left shift b :> 40
a arithmetic right shift b :> 2

```



## Objeck


```objeck
use IO;

bundle Default {
  class Test {
    function : Main(args : String[]) ~ Nil {
      BitWise(3, 4);
    }

    function : BitWise(a : Int, b : Int) ~ Nil {
        Console->GetInstance()->Print("a and b: ")->PrintLine(a and b);
      Console->GetInstance()->Print("a or b: ")->PrintLine(a or b);
      Console->GetInstance()->Print("a xor b: ")->PrintLine(a xor b);
      # shift left & right are supported by the compiler and VM but not
      # exposed to end-users; those instructions are used for optimizations
    }
  }
}
```



## OCaml


```ocaml
let bitwise a b =
  Printf.printf "a and b: %d\n" (a land b);
  Printf.printf "a or b: %d\n" (a lor b);
  Printf.printf "a xor b: %d\n" (a lxor b);
  Printf.printf "not a: %d\n" (lnot a);
  Printf.printf "a lsl b: %d\n" (a lsl b);  (* left shift *)
  Printf.printf "a asr b: %d\n" (a asr b);  (* arithmetic right shift *)
  Printf.printf "a lsr b: %d\n" (a lsr b);  (* logical right shift *)
;;
```



## Octave


There's no arithmetic shift nor rotation (and the not can be done through a xor)


```octave
function bitops(a, b)
  s = sprintf("%s %%s %s = %%s\n", dec2bin(a), dec2bin(b));
  printf(s, "or", dec2bin(bitor(a, b)));
  printf(s, "and", dec2bin(bitand(a, b)));
  printf(s, "xor", dec2bin(bitxor(a, b)));
  printf(s, "left shift", dec2bin(bitshift(a, abs(b))));
  printf(s, "right shift", dec2bin(bitshift(a, -abs(b))));
  printf("simul not %s = %s", dec2bin(a), dec2bin(bitxor(a, 0xffffffff)));
endfunction

bitops(0x1e, 0x3);
```



## Oforth


There is no built-in for not and rotation


```Oforth
: bitwise(a, b)
   a b bitAnd println
   a b bitOr println
   a b bitXor println
   a bitLeft(b) println
   a bitRight(b) println ;
```



## ooRexx


```ooRexx
/* ooRexx *************************************************************
/ Bit Operations work as in Rexx (of course)
* Bit operations are performed up to the length of the shorter string.
* The rest of the longer string is copied to the result.
* ooRexx introduces the possibility to specify a padding character
* to be used for expanding the shorter string.
* 10.11.2012 Walter Pachl taken over from REXX and extended for ooRexx
**********************************************************************/
a=21
b=347
Say '          a :'c2b(a) '        'c2x(a)
Say '          b :'c2b(b)           c2x(b)
Say 'bitand(a,b) :'c2b(bitand(a,b)) c2x(bitand(a,b))
Say 'bitor(a,b)  :'c2b(bitor(a,b))  c2x(bitor(a,b))
Say 'bitxor(a,b) :'c2b(bitxor(a,b)) c2x(bitxor(a,b))
p='11111111'B
Say 'ooRexx only:'
Say 'a~bitor(b,p):'c2b(a~bitor(b,p)) c2x(a~bitor(b,p))
Exit
c2b: return x2b(c2x(arg(1)))
```

Output:

```txt

          a :0011001000110001         3231
          b :001100110011010000110111 333437
bitand(a,b) :001100100011000000110111 323037
bitor(a,b)  :001100110011010100110111 333537
bitxor(a,b) :000000010000010100110111 010537
ooRexx only:
a~bitor(b,p):001100110011010111111111 3335FF

```



## OpenEdge/Progress


The only bit operators available in OpenEdge are the GET-BITS() and PUT-BITS() functions. These functions can be used to implement all bitwise operators.


## PARI/GP

Pari does not support bitwise rotations, which have no obvious meaning with arbitrary-precision integers.  See also <code>bitnegimply</code> for another bitwise operator. For shifts, see also <code>shiftmul</code>.

```parigp
bo(a,b)={
  print("And: "bitand(a,b));
  print("Or: "bitor(a,b));
  print("Not: "bitneg(a));
  print("Xor: "bitxor(a,b));
  print("Left shift: ",a<<b);
  print("Right shift: ",a>>b);
}
```



## Pascal

While Standard Pascal does not have bitwise operations, most Pascal implementations (including Turbo Pascal and Delphi) extend the standard logical operators to also provide bitwise operations:

```pascal
var
 a, b: integer;
begin
 a := 10; { binary 1010 }
 b := 12; { binary 1100 }
 writeln('a and b = ', a and b); {  8 = 1000 }
 writeln('a or b  = ', a or b);  { 14 = 1110 }
 writeln('a xor b = ', a xor b)  {  6 = 0110 }
end.
```



## Perl


```perl
use integer;

sub bitwise($$) {
   ($a, $b) = @_;
   print 'a and b: '. ($a & $b) ."\n";
   print 'a or b: '.  ($a | $b) ."\n";
   print 'a xor b: '. ($a ^ $b) ."\n";
   print 'not a: '.   (~$a)     ."\n";
   print 'a >> b: ', $a >> $b, "\n"; # logical right shift

   use integer; # "use integer" enables bitwise operations to return signed ints
   print "after use integer:\n";
   print 'a << b: ', $a << $b, "\n"; # left shift
   print 'a >> b: ', $a >> $b, "\n"; # arithmetic right shift
}
```



## Perl 6

{{works with|Rakudo|2017.05}}

```perl6
constant MAXINT = uint.Range.max;
constant BITS = MAXINT.base(2).chars;

# define rotate ops for the fun of it
multi sub infix:<⥁>(Int:D \a, Int:D \b) { :2[(a +& MAXINT).polymod(2 xx BITS-1).list.rotate(b).reverse] }
multi sub infix:<⥀>(Int:D \a, Int:D \b) { :2[(a +& MAXINT).polymod(2 xx BITS-1).reverse.list.rotate(b)] }

sub int-bits (Int $a, Int $b) {
    say '';
    say_bit "$a", $a;
    say '';
    say_bit "2's complement $a", +^$a;
    say_bit "$a and $b", $a +& $b;
    say_bit "$a or $b",  $a +| $b;
    say_bit "$a xor $b", $a +^ $b;
    say_bit "$a unsigned shift right $b", ($a +& MAXINT) +> $b;
    say_bit "$a signed shift right $b", $a +> $b;
    say_bit "$a rotate right $b", $a ⥁ $b;
    say_bit "$a shift left $b", $a +< $b;
    say_bit "$a rotate left $b", $a ⥀ $b;
}

int-bits(7,2);
int-bits(-65432,31);

sub say_bit ($message, $value) {
    printf("%30s: %{'0' ~ BITS}b\n", $message, $value +& MAXINT);
}
```

{{out}}

```txt
                             7: 0000000000000000000000000000000000000000000000000000000000000111

              2's complement 7: 1111111111111111111111111111111111111111111111111111111111111000
                       7 and 2: 0000000000000000000000000000000000000000000000000000000000000010
                        7 or 2: 0000000000000000000000000000000000000000000000000000000000000111
                       7 xor 2: 0000000000000000000000000000000000000000000000000000000000000101
      7 unsigned shift right 2: 0000000000000000000000000000000000000000000000000000000000000001
        7 signed shift right 2: 0000000000000000000000000000000000000000000000000000000000000001
              7 rotate right 2: 1100000000000000000000000000000000000000000000000000000000000001
                7 shift left 2: 0000000000000000000000000000000000000000000000000000000000011100
               7 rotate left 2: 0000000000000000000000000000000000000000000000000000000000011100

                        -65432: 1111111111111111111111111111111111111111111111110000000001101000

         2's complement -65432: 0000000000000000000000000000000000000000000000001111111110010111
                 -65432 and 31: 0000000000000000000000000000000000000000000000000000000000001000
                  -65432 or 31: 1111111111111111111111111111111111111111111111110000000001111111
                 -65432 xor 31: 1111111111111111111111111111111111111111111111110000000001110111
-65432 unsigned shift right 31: 0000000000000000000000000000000111111111111111111111111111111111
  -65432 signed shift right 31: 1111111111111111111111111111111111111111111111111111111111111111
        -65432 rotate right 31: 1111111111111110000000001101000111111111111111111111111111111111
          -65432 shift left 31: 1111111111111111100000000011010000000000000000000000000000000000
         -65432 rotate left 31: 1111111111111111100000000011010001111111111111111111111111111111
```



## Phix

Phix has four builtin bitwise operations (and/or/xor/not), all of which have sequence variants. There are no builtin shift or rotate operations,
but it would be easy to devise one using / or * powers of 2 [which the compiler often optimises to single machine instructions] and the builtins,
see [[Bitwise_operations#C|C]] for an example, or use inline assembly as shown below.

```Phix
enum SHL, SAR, SHR, ROL, ROR
function bitop(atom a, integer b, integer op)
atom res
    #ilASM{
        [32]
            mov eax,[a]
            call :%pLoadMint
            mov ecx,[b]
            mov edx,[op]
            cmp dl,SHL
            jne @f
                shl eax,cl
                jmp :storeres
          @@:
            cmp dl,SAR
            jne @f
                sar eax,cl
                jmp :storeres
          @@:
            cmp dl,SHR
            jne @f
                shr eax,cl
                jmp :storeres
          @@:
            cmp dl,ROL
            jne @f
                rol eax,cl
                jmp :storeres
          @@:
            cmp dl,ROR
            jne @f
                ror eax,cl
                jmp :storeres
          @@:
            int3
          ::storeres
            lea edi,[res]
            call :%pStoreMint
        [64]
            mov rax,[a]
            mov rcx,[b]
            mov edx,[op]
            cmp dl,SHL
            jne @f
                shl rax,cl
                jmp :storeres
          @@:
            cmp dl,SAR
            jne @f
                sar rax,cl
                jmp :storeres
          @@:
            cmp dl,SHR
            jne @f
                shr rax,cl
                jmp :storeres
          @@:
            cmp dl,ROL
            jne @f
                rol rax,cl
                jmp :storeres
          @@:
            cmp dl,ROR
            jne @f
                ror eax,cl
                jmp :storeres
          @@:
            int3
          ::storeres
            lea rdi,[res]
            call :%pStoreMint
          }
    return res
end function

procedure bitwise(atom a, atom b)
    printf(1,"and_bits(%b,%b) = %032b\n",{a,b,and_bits(a,b)})
    printf(1," or_bits(%b,%b) = %032b\n",{a,b, or_bits(a,b)})
    printf(1,"xor_bits(%b,%b) = %032b\n",{a,b,xor_bits(a,b)})
    printf(1,"not_bits(%b)     = %032b\n",{a,not_bits(a)})
    printf(1,"     shl(%b,%b) = %032b\n",{a,b,bitop(a,b,SHL)})
    printf(1,"     sar(%b,%b) = %032b\n",{a,b,bitop(a,b,SAR)})
    printf(1,"     shr(%b,%b) = %032b\n",{a,b,bitop(a,b,SHR)})
    printf(1,"     rol(%b,%b) = %032b\n",{a,b,bitop(a,b,ROL)})
    printf(1,"     ror(%b,%b) = %032b\n",{a,b,bitop(a,b,ROR)})
end procedure

bitwise(0x800000FE,7)
```

{{out}}

```txt

and_bits(10000000000000000000000011111110,111) = 00000000000000000000000000000110
 or_bits(10000000000000000000000011111110,111) = 10000000000000000000000011111111
xor_bits(10000000000000000000000011111110,111) = 10000000000000000000000011111001
not_bits(10000000000000000000000011111110)     = 01111111111111111111111100000001
     shl(10000000000000000000000011111110,111) = 00000000000000000111111100000000
     sar(10000000000000000000000011111110,111) = 11111111000000000000000000000001
     shr(10000000000000000000000011111110,111) = 00000001000000000000000000000001
     rol(10000000000000000000000011111110,111) = 00000000000000000111111101000000
     ror(10000000000000000000000011111110,111) = 11111101000000000000000000000001

```



## PHP


```php
function bitwise($a, $b)
{
    function zerofill($a,$b) {
        if($a>=0) return $a>>$b;
        if($b==0) return (($a>>1)&0x7fffffff)*2+(($a>>$b)&1); // this line shifts a 0 into the sign bit for compatibility, replace with "if($b==0) return $a;" if you need $b=0 to mean that nothing happens
        return ((~$a)>>$b)^(0x7fffffff>>($b-1));

    echo '$a AND $b: ' . $a & $b . '\n';
    echo '$a OR $b: ' . $a | $b . '\n';
    echo '$a XOR $b: ' . $a ^ $b . '\n';
    echo 'NOT $a: ' . ~$a . '\n';
    echo '$a << $b: ' . $a << $b . '\n'; // left shift
    echo '$a >> $b: ' . $a >> $b . '\n'; // arithmetic right shift
    echo 'zerofill($a, $b): ' . zerofill($a, $b) . '\n'; // logical right shift
}
```



## PicoLisp

PicoLisp has no specific word size. Numbers grow to arbitrary length. Therefore,
bitwise NOT, logical (non-arithmetic) SHIFTs, and rotate operations do not make
sense.

Bitwise AND:

```PicoLisp
: (& 6 3)
-> 2

: (& 7 3 1)
-> 1
```

Bitwise AND-Test (tests if all bits in the first argument are set in the
following arguments):

```PicoLisp
: (bit? 1 2)
-> NIL

: (bit? 6 3)
-> NIL

: (bit? 6 15 255)
-> 6
```

Bitwise OR:

```PicoLisp
: (| 1 2)
-> 3

: (| 1 2 4 8)
-> 15
```

Bitwise XOR:

```PicoLisp
: (x| 2 7)
-> 5

: (x| 2 7 1)
-> 4
```

Shift (right with a positive count, left with a negative count):

```PicoLisp
: (>> 1 8)
-> 4

: (>> 3 16)
-> 2

: (>> -3 16)
-> 128

: (>> -1 -16)
-> -32
```



## PL/I


```pli
/* PL/I can perform bit operations on binary integers. */
k = iand(i,j);
k = ior(i,j);
k = inot(i,j);
k = ieor(i,j);
k = isll(i,n); /* unsigned shifts i left  by n places. */
k = isrl(i,n); /* unsigned shifts i right by n places. */
k = lower2(i, n); /* arithmetic right shift i by n places. */
k = raise2(i, n); /* arithmetic left  shift i by n places. */

/* PL/I can also perform boolean operations on bit strings */
/* of any length: */

declare (s, t, u) bit (*);

u = s & t; /* logical and  */
u = s | t; /* logical or   */
u = ^ s;   /* logical not  */
u = s ^ t; /* exclusive or */

Built-in rotate functions are not available.
They can be readily implemented by the user, though:

u = substr(s, length(s), 1) || substr(s, 1, length(s)-1); /* implements rotate right. */
u = substr(s, 2) || substr(s, 1, 1);                      /* implements rotate left.  */

```



## Pop11



```pop11
define bitwise(a, b);
    printf(a && b, 'a and b = %p\n');
    printf(a || b, 'a or b = %p\n');
    printf(a ||/& b, 'a xor b = %p\n');
    printf(~~ a, 'not a = %p\n');
    printf(a << b, 'left shift of a by b = %p\n');
    printf(a >> b, 'arithmetic right shift of a by b = %p\n');
enddefine;
```


Conceptually in Pop11 integers have infinite precision, in particular negative numbers conceptually have infinitely many leading 1's in two's complement notation. Hence, logical right shift is not defined. If needed, logical right shift can be simulated by masking high order bits.

Similarly, on infinitely precise numbers rotation is undefined.


## PureBasic


```PureBasic
Procedure Bitwise(a, b)
  Debug  a & b      ; And
  Debug a | b       ;Or
  Debug a ! b       ; XOr
  Debug ~a          ;Not
  Debug a << b      ; shift left
  Debug a >> b      ; arithmetic shift right
  ; Logical shift right and rotates are not available
  ; You can of use inline ASM to achieve this:
  Define Temp
  ; logical shift right
  !mov edx, dword [p.v_a]
  !mov ecx, dword [p.v_b]
  !shr edx, cl
  !mov dword [p.v_Temp], edx
  Debug Temp
  ; rotate left
  !mov edx, dword [p.v_a]
  !mov ecx, dword [p.v_b]
  !rol edx, cl
  !mov dword [p.v_Temp], edx
  Debug Temp
  ; rotate right
  !mov edx, dword [p.v_a]
  !mov ecx, dword [p.v_b]
  !ror edx, cl
  !mov dword [p.v_Temp], edx
  Debug Temp
EndProcedure
```



## PowerShell

Logical right shift and rotations are not supported in PowerShell.
{{works with|PowerShell|2.0}}

```PowerShell
$X -band $Y
$X -bor  $Y
$X -bxor $Y
-bnot $X
```


{{works with|PowerShell|3.0}}

```PowerShell
$X -shl $Y
# Arithmetic right shift
$X -shr $Y

# Requires quite a stretch of the imagination to call this "native" support of right rotate, but it works
[System.Security.Cryptography.SHA256Managed].GetMethod('RotateRight', 'NonPublic, Static', $null, @([UInt32], [Int32]), $null).Invoke($null, @([uint32]$X, $Y))
```



## Python


```python
def bitwise(a, b):
        print 'a and b:', a & b
        print 'a or b:', a | b
        print 'a xor b:', a ^ b
        print 'not a:', ~a
        print 'a << b:', a << b # left shift
        print 'a >> b:', a >> b # arithmetic right shift
```


Python does not have built in rotate or logical right shift operations.

Note: Newer Python versions (circa 2.4?) will automatically promote integers into "long integers" (arbitrary length, bounded by available memory). This can be noticed especially when using left shift operations.  When using bitwise operations one usually wants to keep these bounded to specific sizes such as 8, 16, 32 or 64 bit widths.  To do these we use the AND operator with specific values (bitmasks).  For example:


```python
# 8-bit bounded shift:
x = x << n & 0xff
# ditto for 16 bit:
x = x << n & 0xffff
# ... and 32-bit:
x = x << n & 0xffffffff
# ... and 64-bit:
x = x << n & 0xffffffffffffffff
```


We can easily implement our own rotation functions.  For left rotations this is down by ORing the left shifted and masked lower bits against the right shifted upper bits.  For right rotations we perform the converse operations, ORing a set of right shifted lower bits against the appropriate number of left shifted upper bits.


```python
def bitstr(n, width=None):
   """return the binary representation of n as a string and
      optionally zero-fill (pad) it to a given length
   """
   result = list()
   while n:
      result.append(str(n%2))
      n = int(n/2)
   if (width is not None) and len(result) < width:
      result.extend(['0'] * (width - len(result)))
   result.reverse()
   return ''.join(result)

def mask(n):
   """Return a bitmask of length n (suitable for masking against an
      int to coerce the size to a given length)
   """
   if n >= 0:
       return 2**n - 1
   else:
       return 0

def rol(n, rotations=1, width=8):
    """Return a given number of bitwise left rotations of an integer n,
       for a given bit field width.
    """
    rotations %= width
    if rotations < 1:
        return n
    n &= mask(width) ## Should it be an error to truncate here?
    return ((n << rotations) & mask(width)) | (n >> (width - rotations))

def ror(n, rotations=1, width=8):
    """Return a given number of bitwise right rotations of an integer n,
       for a given bit field width.
    """
    rotations %= width
    if rotations < 1:
        return n
    n &= mask(width)
    return (n >> rotations) | ((n << (width - rotations)) & mask(width))
```


In this example we show a relatively straightforward function for converting integers into strings of bits, and another simple ''mask()'' function to create arbitrary lengths of bits against which we perform our masking operations.  Also note that the implementation of these functions defaults to single bit rotations of 8-bit bytes.  Additional arguments can be used to over-ride these defaults. Any case where the number of rotations modulo the width is zero represents a rotation of all bits back to their starting positions.  This implementation should handle any integer number of rotations over bitfields of any valid (positive integer) length.


## R



###  Native functions in R 3.x


```r
# Since R 3.0.0, the base package provides bitwise operators, see ?bitwAnd

a <- 35
b <- 42
bitwAnd(a, b)
bitwOr(a, b)
bitwXor(a, b)
bitwNot(a)
bitwShiftL(a, 2)
bitwShiftR(a, 2)

# See also http://cran.r-project.org/src/base/NEWS.html
```


===Using ''as.hexmode'' or ''as.octmode''===

```r
a <- as.hexmode(35)
b <- as.hexmode(42)
as.integer(a & b)      # 34
as.integer(a | b)      # 43
as.integer(xor(a, b))  # 9
```


===Using ''intToBits''===
The logical operators in R, namely &, | and !, are designed to work on logical vectors rather than bits.  It is possible to convert from integer to logical vector and back to make these work as required, e.g.

```R
intToLogicalBits <- function(intx) as.logical(intToBits(intx))
logicalBitsToInt <- function(lb) as.integer(sum((2^(0:31))[lb]))
"%AND%" <- function(x, y)
{
   logicalBitsToInt(intToLogicalBits(x) & intToLogicalBits(y))
}
"%OR%" <- function(x, y)
{
   logicalBitsToInt(intToLogicalBits(x) | intToLogicalBits(y))
}

35 %AND% 42    # 34
35 %OR% 42     # 42
```


===Using ''bitops'' package===

```R
library(bitops)
bitAnd(35, 42)          # 34
bitOr(35, 42)           # 43
bitXor(35, 42)          # 9
bitFlip(35, bitWidth=8) # 220
bitShiftL(35, 1)        # 70
bitShiftR(35, 1)        # 17
# Note that no bit rotation is provided in this package
```


===Using hidden native functions from ''base'' package===

```r
# As one can see from
getDLLRegisteredRoutines(getLoadedDLLs()$base)
# R knows functions bitwiseAnd, bitwiseOr, bitwiseXor and bitwiseNot.
# Here is how to call them (see ?.Call for the calling mechanism):

.Call("bitwiseOr",  as.integer(12), as.integer(10))
.Call("bitwiseXor", as.integer(12), as.integer(10))
.Call("bitwiseAnd", as.integer(12), as.integer(10))
.Call("bitwiseNot", as.integer(12))

# It would be easy to embed these calls in R functions, for better readability
# Also, it's possible to call these functions on integer vectors:

.Call("bitwiseOr", c(5L, 2L), c(3L, 8L))
```



## Racket


```racket

#lang racket
(define a 255)
(define b 5)
(list (bitwise-and a b)
      (bitwise-ior a b)
      (bitwise-xor a b)
      (bitwise-not a)
      (arithmetic-shift a b)      ; left shift
      (arithmetic-shift a (- b))) ; right shift

```

Output:

```txt

'(5 255 250 -256 8160 7)

```



## Retro

There is no predefined arithmetic shifts in Retro.


```Retro

: bitwise ( ab- )
  cr
  over     "a = %d\n" puts
  dup      "b = %d\n" puts
  2over and "a and b = %d\n" puts
  2over or  "a or b = %d\n" puts
  2over xor "a xor b = %d\n" puts
  over not "not a = %d\n" puts
  2over <<  "a << b = %d\n" puts
  2over >>  "a >> b = %d\n" puts
  2drop ;
```



## REXX


```txt

 ╔═══════════════════════════════════════════════════════════════════════════════════════╗
 ║ Since REXX stores numbers  (indeed, all values)  as characters, it makes no sense to  ║
 ║ "rotate"  a value,  since there aren't any boundaries for the value.    I.E.:  there  ║
 ║ isn't any 32─bit word  "container"  or  "cell"  (for instance)  to store an integer.  ║
 ║                                                                                       ║
 ║ Furthermore, since REXX numbers can be arbitrary precision,  the concept of rotating  ║
 ║ a number has no meaning.                                                              ║
 ╚═══════════════════════════════════════════════════════════════════════════════════════╝

```


```rexx
/*REXX program performs  bit─wise operations  on integers:   &   |   &&   ¬   «L   »R   */
numeric digits 1000                              /*be able to handle ginormous integers.*/
           say  center('decimal', 9)      center("value", 9)        center('bits', 50)
           say  copies('─'      , 9)      copies("─"    , 9)        copies('─',    50)
a = 21 ;   call show           a          ,      'A'                   /* display   A   */
b =  3 ;   call show              b       ,      'B'                   /* display   B   */
           call show      bAnd(a, b)      ,      'A & B'               /*  and          */
           call show       bOr(a, b)      ,      'A | B'               /*   or          */
           call show      bXor(a, b)      ,      'A && B'              /*  xor          */
           call show      bNot(a)         ,      '¬ A'                 /*  not          */
           call show   bShiftL(a, b)      ,      'A [«B]'              /* shift  left   */
           call show   bShiftR(a, b)      ,      'A [»B]'              /* shirt right   */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:    say  right( arg(1), 9)  center( arg(2), 9)  right( d2b( arg(1) ), 50);     return
d2b:     return x2b( d2x( arg(1) ) ) + 0         /*some REXXes have the   D2B   BIF.    */
b2d:     return x2d( b2x( arg(1) ) )             /*  "     "     "   "    B2D    "      */
bNot:    return b2d( translate( d2b( arg(1) ), 10, 01) )     +0   /*+0 ≡ normalizes a #.*/
bShiftL: return b2d( d2b( arg(1) ) || copies(0, arg(2) ) )   +0   /* " "      "     " " */
bAnd:    return c2d( bitand( d2c( arg(1) ), d2c( arg(2) ) ) )
bOr:     return c2d(  bitor( d2c( arg(1) ), d2c( arg(2) ) ) )
bXor:    return c2d( bitxor( d2c( arg(1) ), d2c( arg(2) ) ) )
bShiftR: $=substr(reverse(d2b(arg(1))),arg(2)+1); if $='' then $=0; return b2d(reverse($))
```

{{out|output}}

```txt

 decimal    value                          bits
───────── ───────── ──────────────────────────────────────────────────
       21     A                                                  10101
        3     B                                                     11
        1   A & B                                                    1
       23   A | B                                                10111
       22  A && B                                                10110
       10    ¬ A                                                  1010
      168  A [«B]                                             10101000
        2  A [»B]                                                   10

```



## Ring


```ring

x = 8
y = 2

see "x & y - Binary AND : " + (x & y) + nl
see "x | y - Binary OR : " + (x | y) + nl
see "x ^ y - Binary XOR : " + (x ^ y) +nl
see "~x - Binary Ones Complement : " + (~x) + nl
see "x << y - Binary Left Shift : " + (x << y) + nl
see "x >> y - Binary Right Shift : " + (x >> y) + nl

```



## RLaB


In RLaB the bitwise operations are available for ''integers'' type of numbers. For the operations below if both arguments
are integers then the result of the operation is an integer as well.


```RLaB>>
 x = int(3);
>> y = int(1);
>> z = x && y; printf("0x%08x\n",z); // logical 'and'
0x00000001
>> z = x || y; printf("0x%08x\n",z); // logical 'or'
0x00000003
>> z = !x; printf("0x%08x\n",z);     // logical 'not'
0xfffffffc
>> i2 = int(2);
>> z = x * i2; printf("0x%08x\n",z);  // left-shift is multiplication by 2 where both arguments are integers
0x00000006
>> z = x / i2; printf("0x%08x\n",z);  // right-shift is division by 2 where both arguments are integers
0x00000001
```



## Robotic


```robotic

input string "First value"
set "local1" to "input"
input string "Second value"
set "local2" to "input"

. ">>> is an arithmetic shift; >> is a logical shift"
[ "a AND b = ('local1' a 'local2')"
[ "a OR b = ('local1' o 'local2')"
[ "a XOR b = ('local1' x 'local2')"
[ "NOT a = (~'local1')"
[ "a << b = ('local1' << 'local2')"
[ "a >> b = ('local1' >> 'local2')"
[ "a >>> b = ('local1' >>> 'local2')"
end
. "Bitwise rotation is not natively supported"

```



## Ruby


```ruby
def bitwise(a, b)
  form = "%1$7s:%2$6d  %2$016b"
  puts form % ["a", a]
  puts form % ["b", b]
  puts form % ["a and b", a & b]
  puts form % ["a or b ", a | b]
  puts form % ["a xor b", a ^ b]
  puts form % ["not a  ", ~a]
  puts form % ["a << b ", a << b]  # left shift
  puts form % ["a >> b ", a >> b]  # arithmetic right shift
end

bitwise(14,3)
```


{{out}}

```txt

      a:    14  0000000000001110
      b:     3  0000000000000011
a and b:     2  0000000000000010
a or b :    15  0000000000001111
a xor b:    13  0000000000001101
not a  :   -15  ..11111111110001
a << b :   112  0000000001110000
a >> b :     1  0000000000000001

```



## Rust


```rust
fn main() {
    let a: u8 = 105;
    let b: u8 = 91;
    println!("a      = {:0>8b}", a);
    println!("b      = {:0>8b}", b);
    println!("a | b  = {:0>8b}", a | b);
    println!("a & b  = {:0>8b}", a & b);
    println!("a ^ b  = {:0>8b}", a ^ b);
    println!("!a     = {:0>8b}", !a);
    println!("a << 3 = {:0>8b}", a << 3);
    println!("a >> 3 = {:0>8b}", a >> 3);
}
```


Output:


```txt

a      = 01101001
b      = 01011011
a | b  = 01111011
a & b  = 01001001
a ^ b  = 00110010
!a     = 10010110
a << 3 = 01001000
a >> 3 = 00001101

```



## SAS


```sas
/* rotations are not available, but are easy to implement with the other bitwise operators */
data _null_;
   a=105;
   b=91;
   c=bxor(a,b);
   d=band(a,b);
   e=bor(a,b);
   f=bnot(a); /* on 32 bits */
   g=blshift(a,1);
   h=brshift(a,1);
   put _all_;
run;
```



## Scala



```scala
def bitwise(a: Int, b: Int) {
  println("a and b: " + (a & b))
  println("a or b: " + (a | b))
  println("a xor b: " + (a ^ b))
  println("not a: " + (~a))
  println("a << b: " + (a << b)) // left shift
  println("a >> b: " + (a >> b)) // arithmetic right shift
  println("a >>> b: " + (a >>> b)) // unsigned right shift
  println("a rot b: " + Integer.rotateLeft(a, b)) // Rotate Left
  println("a rol b: " + Integer.rotateRight(a, b)) // Rotate Right
}
```



## Scheme

{{Works with|Scheme|R<math>^6</math>RS}}

```scheme
(import (rnrs arithmetic bitwise (6)))

(define (bitwise a b)
  (display (bitwise-and a b))
  (newline)
  (display (bitwise-ior a b))
  (newline)
  (display (bitwise-xor a b))
  (newline)
  (display (bitwise-not a))
  (newline)
  (display (bitwise-arithmetic-shift-right a b))
  (newline))

(bitwise 255 5)
```

Output:
<lang>5
255
250
-256
7
```


''Note: bitwise operations were also described in [http://srfi.schemers.org/srfi-60/ SRFI-60], with additional aliases (and previously discussed in [http://srfi.schemers.org/srfi-33/ SRFI-33] which remained draft).''


## Seed7

The type [http://seed7.sourceforge.net/manual/types.htm#integer integer] is intended for arithmetic operations.
Besides arithmetic shifts, which are seen as multiplication and division by powers of two, no bitwise operations are supported.
The type [http://seed7.sourceforge.net/libraries/bin32.htm bin32] is intended for bit-pattern operations.
Bin32 has the same internal representation as integer.
That way conversions between them don't cause an overhead.
Right shifting of bin32 values is done with logical shifts.


```seed7
$ include "seed7_05.s7i";
  include "bin32.s7i";

const proc: bitwise (in integer: a, in integer: b) is func
  begin
    writeln("a:        " <& a  radix 2 lpad0 32);
    writeln("b:        " <& b  radix 2 lpad0 32);
    writeln("integer operations:");
    writeln("a << b:   " <& a << b  radix 2 lpad0 32); # left shift
    writeln("a >> b:   " <& a >> b  radix 2 lpad0 32); # arithmetic right shift
  end func;

const proc: bitwise (in bin32: a, in bin32: b) is func
  begin
    writeln("bin32 operations:");
    writeln("a and b:  " <& a & b  radix 2 lpad0 32);
    writeln("a or b:   " <& a | b  radix 2 lpad0 32);
    writeln("a xor b:  " <& a >< b  radix 2 lpad0 32);
    writeln("not a:    " <& ~a  radix 2 lpad0 32);
    writeln("a << b:   " <& a << ord(b)  radix 2 lpad0 32);  # left shift
    writeln("a >> b:   " <& a >> ord(b)  radix 2 lpad0 32);  # logical right shift
    writeln("a rotL b: " <& rotLeft(a, ord(b))  radix 2 lpad0 32);  # Rotate Left
    writeln("a rolR b: " <& rotRight(a, ord(b))  radix 2 lpad0 32); # Rotate Right
  end func;

const proc: main is func
  begin
    bitwise(65076, 6);
    bitwise(bin32(65076), bin32(6));
  end func;
```


{{out}}

```txt

a:        00000000000000001111111000110100
b:        00000000000000000000000000000110
integer operations:
a << b:   00000000001111111000110100000000
a >> b:   00000000000000000000001111111000
bin32 operations:
a and b:  00000000000000000000000000000100
a or b:   00000000000000001111111000110110
a xor b:  00000000000000001111111000110010
not a:    11111111111111110000000111001011
a << b:   00000000001111111000110100000000
a >> b:   00000000000000000000001111111000
a rotL b: 00000000001111111000110100000000
a rolR b: 11010000000000000000001111111000

```



## Sidef


```ruby
func bitwise(a, b) {
   say ('a and b : ',  a & b)
   say ('a or b  : ',  a | b)
   say ('a xor b : ',  a ^ b)
   say ('not a   : ',     ~a)
   say ('a << b  : ', a << b)  # left shift
   say ('a >> b  : ', a >> b)  # arithmetic right shift
}

bitwise(14,3)
```

{{out}}

```txt

a and b : 2
a or b  : 15
a xor b : 13
not a   : -15
a << b  : 112
a >> b  : 1

```


## Simula


```simula
BEGIN
   COMMENT TO MY KNOWLEDGE SIMULA DOES NOT SUPPORT BITWISE OPERATIONS SO WE MUST WRITE PROCEDURES FOR THE JOB ;
   INTEGER WORDSIZE;
   WORDSIZE := 32;
   BEGIN

      PROCEDURE TOBITS(N,B); INTEGER N; BOOLEAN ARRAY B;
      BEGIN
         INTEGER I,BITN;
         FOR I := WORDSIZE-1 STEP -1 UNTIL 0 DO BEGIN
            BITN := MOD(N,2); B(I) := BITN<>0; N := N // 2;
         END;
      END TOBITS;

      INTEGER PROCEDURE FROMBITS(B); BOOLEAN ARRAY B;
      BEGIN
         INTEGER I, RESULT;
         FOR I := 0 STEP 1 UNTIL WORDSIZE-1 DO
             RESULT := 2 * RESULT + (IF B(I) THEN 1 ELSE 0);
         FROMBITS := RESULT;
      END FROMBITS;

      INTEGER PROCEDURE BITOP(A,B,F);
         INTEGER A,B;
         PROCEDURE F IS BOOLEAN PROCEDURE F(A,B); BOOLEAN A,B;;
      BEGIN
         INTEGER I;
         BOOLEAN ARRAY BA(0:WORDSIZE-1);
         BOOLEAN ARRAY BB(0:WORDSIZE-1);
         TOBITS(A,BA);
         TOBITS(B,BB);
         FOR I := 0 STEP 1 UNTIL WORDSIZE-1 DO BA(I) := F(BA(I),BB(I));
         BITOP := FROMBITS(BA);
      END BITOP;

      INTEGER PROCEDURE BITUOP(A,F);
         INTEGER A;
         PROCEDURE F IS BOOLEAN PROCEDURE F(A); BOOLEAN A;;
      BEGIN
         INTEGER I;
         BOOLEAN ARRAY BA(0:WORDSIZE-1);
         TOBITS(A,BA);
         FOR I := 0 STEP 1 UNTIL WORDSIZE-1 DO BA(I) := F(BA(I));
         BITUOP := FROMBITS(BA);
      END BITUOP;

      BOOLEAN PROCEDURE OPAND(A,B); BOOLEAN A,B; OPAND := A AND B;
      INTEGER PROCEDURE BITAND(A,B); INTEGER A,B; BITAND := BITOP(A,B,OPAND);

      BOOLEAN PROCEDURE OPOR(A,B); BOOLEAN A,B; OPOR := A OR B;
      INTEGER PROCEDURE BITOR(A,B); INTEGER A,B; BITOR := BITOP(A,B,OPOR);

      BOOLEAN PROCEDURE OPXOR(A,B); BOOLEAN A,B; OPXOR := (A AND NOT B) OR (NOT A AND B);
      INTEGER PROCEDURE BITXOR(A,B); INTEGER A,B; BITXOR := BITOP(A,B,OPXOR);

      BOOLEAN PROCEDURE OPNOT(A); BOOLEAN A; OPNOT := NOT A;
      INTEGER PROCEDURE BITNOT(A); INTEGER A; BITNOT := BITUOP(A,OPNOT);

      INTEGER PROCEDURE BITSHL(A,B); INTEGER A,B;
      BEGIN
          IF B < 0 THEN A := BITSHR(A,-B)
          ELSE WHILE B > 0 DO BEGIN A := 2 * A; B := B-1; END;
          BITSHL := A;
      END BITSHL;

      INTEGER PROCEDURE BITSHR(A,B); INTEGER A,B;
      BEGIN
          IF B < 0 THEN A := BITSHL(A,-B)
          ELSE WHILE B > 0 DO BEGIN A := A // 2; B := B-1; END;
          BITSHR := A;
      END BITSHR;

      INTEGER PROCEDURE BITROTR(A,B); INTEGER A,B;
      BEGIN
         INTEGER I,J;
         BOOLEAN ARRAY BA(0:WORDSIZE-1);
         BOOLEAN ARRAY BB(0:WORDSIZE-1);
         TOBITS(A,BA);
         FOR I := 0 STEP 1 UNTIL WORDSIZE-1 DO BEGIN
            J := MOD(I + B, WORDSIZE); BB(J) := BA(I);
         END;
         BITROTR := FROMBITS(BB);
      END BITROTR;

      INTEGER PROCEDURE BITROTL(A,B); INTEGER A,B;
         BITROTL := BITROTR(A,-B);

      PROCEDURE BITWISE(A,B); INTEGER A,B;
      BEGIN
        OUTTEXT("A AND B   : "); OUTINT(BITAND(A,B),0); OUTIMAGE;
        OUTTEXT("A OR B    : "); OUTINT(BITOR (A,B),0); OUTIMAGE;
        OUTTEXT("A XOR B   : "); OUTINT(BITXOR(A,B),0); OUTIMAGE;
        OUTTEXT("NOT A     : "); OUTINT(BITNOT(A),  0); OUTIMAGE;
        OUTTEXT("A << B    : "); OUTINT(BITSHL(A,B),0); OUTIMAGE;  ! LEFT SHIFT ;
        OUTTEXT("A >> B    : "); OUTINT(BITSHR(A,B),0); OUTIMAGE;  ! ARITHMETIC RIGHT SHIFT ;
        OUTTEXT("A ROTL B  : "); OUTINT(BITROTL(A,B),0); OUTIMAGE;  ! ROTATE LEFT ;
        OUTTEXT("A ROTR B  : "); OUTINT(BITROTR(A,B),0); OUTIMAGE;  ! ROTATE RIGHT ;
      END BITWISE;

      BITWISE(14,3);
   END;
END

```

{{out}}

```txt
A AND B   : 2
A OR B    : 15
A XOR B   : 13
NOT A     : -15
A << B    : 112
A >> B    : 1
A ROTL B  : 112
A ROTR B  : -1073741823

```



## Slate


```slate
[ |:a :b |

 inform: (a bitAnd: b) printString.
 inform: (a bitOr: b) printString.
 inform: (a bitXor: b) printString.
 inform: (a bitNot) printString.
 inform: (a << b) printString.
 inform: (a >> b) printString.

] applyTo: {8. 12}.
```

'''Bold text'''


## Smalltalk

{{works with|GNU Smalltalk}}
{{works with|Smalltalk/X}}
{{works with|VisualWorks Smalltalk}}
Since [[GNU Smalltalk]] by default runs without a graphical user interface, I wrote the program in that dialect.  The actual methods for bitwise operations (''bitAnd:'', etc.) are the same in all implementations.

```smalltalk
| testBitFunc |
testBitFunc := [ :a :b |
     ('%1 and %2 is %3' % { a. b. (a bitAnd: b) }) displayNl.
     ('%1 or %2 is %3' % { a. b. (a bitOr: b) }) displayNl.
     ('%1 xor %2 is %3' % { a. b. (a bitXor: b) }) displayNl.
     ('not %1 is %2' % { a. (a bitInvert) }) displayNl.
     ('%1 left shift %2 is %3' % { a. b. (a bitShift: b) }) displayNl.
     ('%1 right shift %2 is %3' % { a. b. (a bitShift: (b negated)) }) displayNl.
 ].
testBitFunc value: 16r7F value: 4 .
```


in addition to the above,
{{works with|Smalltalk/X}}

```smalltalk
(a bitClear: b) "mask out bits"
(a bitAt: index) "retrieve a bit (bit-index, one-based)"
(a setBit: index) "set a bit (bit-index)"
(a clearBit: index) "clear a bit (bit-index)"
(a invertBit: index) "invert a bit (bit index)"
lowBit "find the index of the lowest one-bit; zero if none"
highBit "find the index of the highest one-bit; zero if none"
bitCount "count the one-bits"
```


Notice that all of those work on arbitrarily large integers (i.e. 1000 factorial lowBit -> 995).


## Standard ML

For integers, IntInfs provide bitwise operations:

```sml
fun bitwise_ints (a, b) = (
  print ("a and b: " ^ IntInf.toString (IntInf.andb (IntInf.fromInt a, IntInf.fromInt b)) ^ "\n");
  print ("a or b: "  ^ IntInf.toString (IntInf.orb  (IntInf.fromInt a, IntInf.fromInt b)) ^ "\n");
  print ("a xor b: " ^ IntInf.toString (IntInf.xorb (IntInf.fromInt a, IntInf.fromInt b)) ^ "\n");
  print ("not a: "   ^ IntInf.toString (IntInf.notb (IntInf.fromInt a                  )) ^ "\n");
  print ("a lsl b: " ^ IntInf.toString (IntInf.<<   (IntInf.fromInt a, Word.fromInt b  )) ^ "\n");  (* left shift *)
  print ("a asr b: " ^ IntInf.toString (IntInf.~>>  (IntInf.fromInt a, Word.fromInt b  )) ^ "\n")   (* arithmetic right shift *)
)
```

More shifts are available for words (unsigned ints):

```sml
fun bitwise_words (a, b) = (
  print ("a and b: " ^ Word.fmt StringCvt.DEC (Word.andb (a, b)) ^ "\n");
  print ("a or b: "  ^ Word.fmt StringCvt.DEC (Word.orb  (a, b)) ^ "\n");
  print ("a xor b: " ^ Word.fmt StringCvt.DEC (Word.xorb (a, b)) ^ "\n");
  print ("not a: "   ^ Word.fmt StringCvt.DEC (Word.notb a     ) ^ "\n");
  print ("a lsl b: " ^ Word.fmt StringCvt.DEC (Word.<< (a, b)  ) ^ "\n");  (* left shift *)
  print ("a asr b: " ^ Word.fmt StringCvt.DEC (Word.~>> (a, b) ) ^ "\n");  (* arithmetic right shift *)
  print ("a asr b: " ^ Word.fmt StringCvt.DEC (Word.>> (a, b)  ) ^ "\n")   (* logical right shift *)
)
```



## Stata

Stata does not have bitwise operators as of version 15.1. It's possible to use Mata functions '''[https://www.stata.com/help.cgi?mf_inbase inbase]''' and '''frombase''' to convert integers to binary strings, and operate on these, but it will be much slower than native operators. William Matsuoka has written functions for this [http://www.wmatsuoka.com/stata/building-an-api-library here].


## Swift


```swift
func bitwise(a: Int, b: Int) {
  // All bitwise operations (including shifts)
  // require both operands to be the same type
  println("a AND b: \(a & b)")
  println("a OR b: \(a | b)")
  println("a XOR b: \(a ^ b)")
  println("NOT a: \(~a)")
  println("a << b: \(a << b)") // left shift
  // for right shifts, if the operands are unsigned, Swift performs
  // a logical shift; if signed, an arithmetic shift.
  println("a >> b: \(a >> b)") // arithmetic right shift
  println("a lsr b: \(Int(bitPattern: UInt(bitPattern: a) >> UInt(bitPattern: b)))") // logical right shift
}

bitwise(-15,3)
```

{{out}}

```txt

a AND b: 1
a OR b: -13
a XOR b: -14
NOT a: 14
a << b: -120
a >> b: -2
a lsr b: 2305843009213693950

```



## SystemVerilog

Verilog, being a hardware description language, had pretty comprehensive support for bit twiddling; though rotation is still a slightly manual operation. Just to be different, I decided to use a couple of 53-bit integers:

```SystemVerilog
program main;

  initial begin
    bit [52:0] a,b,c;
    a = 53'h123476547890fe;
    b = 53'h06453bdef23ca6;

    c = a & b; $display("%h & %h = %h", a,b,c);
    c = a | b; $display("%h | %h = %h", a,b,c);
    c = a ^ b; $display("%h ^ %h = %h", a,b,c);
    c = ~ a;   $display("~%h = %h", a, c);

    c = a << 5; $display("%h << 5 = %h", a, c);
    c = a >> 5; $display("%h >> 5 = %h", a, c);

    c = { a[53-23:0], a[52-:23] }; $display("%h rotate-left 23 = %h", a, c);
    c = { a[1:0], a[52:2] }; $display("%h rotate-right 2 = %h", a, c);
  end

endprogram
```


If we want to do a variable bit rotation, then we need to think in hardware terms, and build a mux structure (this could be a function, but using a module allows it to be parameterized:


```SystemVerilog
module rotate(in, out, shift);

  parameter BITS = 32;
  parameter SHIFT_BITS = 5;

  input  [BITS-1:0] in;
  output [BITS-1:0] out;
  input  [SHIFT_BITS-1:0] shift;

  always_comb foreach (out[i]) out[i] = in[ (i+shift) % BITS ];

endmodule
```


of course, one could always write the foreach loop inline.


## Tcl


```tcl
proc bitwise {a b} {
    puts [format "a and b: %#08x" [expr {$a & $b}]]
    puts [format "a or b: %#08x"  [expr {$a | $b}]]
    puts [format "a xor b: %#08x" [expr {$a ^ $b}]]
    puts [format "not a: %#08x"   [expr {~$a}]]
    puts [format "a << b: %#08x"  [expr {$a << $b}]]
    puts [format "a >> b: %#08x"  [expr {$a >> $b}]]
}
```

There are no built-in operations for arithmetic right shift or for bit rotation. Indeed, rotation precludes the use of arbitrary-width integers and can only be defined with respect to a particular width. However, we can simulate these operations for 32-bit values (requires Tcl 8.5):

```tcl
proc bitwiseUnsupported {a b} {
    set bits 0xFFFFFFFF
    # Force interpretation as a 32-bit unsigned value
    puts [format "a ArithRightShift b: %#08x" [expr {($a & $bits) >> $b}]]
    puts [format "a RotateRight b: %#08x" [expr {
        (($a >> $b) & ($bits >> $b)) |
        (($a << (32-$b)) & ($bits ^ ($bits >> $b)))
    }]]
    puts [format "a RotateLeft b: %#08x" [expr {
        (($a << $b) & $bits & ($bits << $b)) |
        (($a >> (32-$b)) & ($bits ^ ($bits << $b)))
    }]]
}
```


=={{header|TI-89 BASIC}}==

While the TI-89 supports arbitrary-size integers, all bitwise arithmetic is performed on the rightmost 32 bits of the integers' two's complement representation.

The right shift operation fills the new leftmost bit with a copy of the old leftmost bit.


```ti89b
bitwise(a,b)
Prgm
  Local show, oldbase
  Define show(label, x)=Prgm
    Local r
    setMode("Base","DEC")
    string(x) → r
    setMode("Base","HEX")
    Disp label & r & " " & string(x)
  EndPrgm
  getMode("Base") → oldbase
  show("", {a, b})
  show("And ", a and b)
  show("Or  ", a or b)
  show("Xor ", a xor b)
  show("Not ", not a)
  Pause "[Press ENTER]"
  show("LSh ", shift(a,b))
  show("RSh ", shift(a,–b))
  show("LRo ", rotate(a,b))
  show("RRo ", rotate(a,–b))
  setMode("Base",oldbase)
EndPrgm
```



## VBA

In VBA, the logical operators And, Or, Xor, Not are actually binary operators. There are also Eqv and Imp (for bitwise "equivalence" and "logical implication").


```vb
Debug.Print Hex(&HF0F0 And &HFF00)  'F000
Debug.Print Hex(&HF0F0 Or &HFF00)   'FFF0
Debug.Print Hex(&HF0F0 Xor &HFF00)  'FF0
Debug.Print Hex(Not &HF0F0)         'F0F
Debug.Print Hex(&HF0F0 Eqv &HFF00)  'F00F
Debug.Print Hex(&HF0F0 Imp &HFF00)  'FF0F

```


The other operations in the task are not builtin, but are easy to implement. Integers are signed, and overflow throws and exception, one must take care of this.


```vb
Function MaskL(k As Integer) As Long
    If k < 1 Then
        MaskL = 0
    ElseIf k > 31 Then
        MaskL = -1
    Else
        MaskL = (-1) Xor (2 ^ (32 - k) - 1)
    End If
End Function
Function MaskR(k As Integer) As Long
    If k < 1 Then
        MaskR = 0
    ElseIf k > 31 Then
        MaskR = -1
    Else
        MaskR = 2 ^ k - 1
    End If
End Function
Function Bit(k As Integer) As Long
    If k < 0 Or k > 31 Then
        Bit = 0
    ElseIf k = 31 Then
        Bit = MaskL(1)
    Else
        Bit = 2 ^ k
    End If
End Function
Function ShiftL(n As Long, k As Integer) As Long
    If k = 0 Then
        ShiftL = n
    ElseIf k > 31 Then
        ShiftL = 0
    ElseIf k < 0 Then
        ShiftL = ShiftR(n, -k)
    Else
        ShiftL = (n And MaskR(31 - k)) * 2 ^ k
        If (n And Bit(31 - k)) <> 0 Then ShiftL = ShiftL Or MaskL(1)
    End If
End Function
Function ShiftR(n As Long, k As Integer) As Long
    If k = 0 Then
        ShiftR = n
    ElseIf k > 31 Then
        ShiftR = 0
    ElseIf k < 0 Then
        ShiftR = ShiftL(n, -k)
    Else
        ShiftR = (n And MaskR(31)) \ 2 ^ k
        If (n And MaskL(1)) <> 0 Then ShiftR = ShiftR Or Bit(31 - k)
    End If
End Function
Function RotateL(n As Long, k As Integer) As Long
    k = (32768 + k) Mod 32
    If k = 0 Then
        RotateL = n
    Else
        RotateL = ShiftL(n, k) Or ShiftR(n, 32 - k)
    End If
End Function
Function RotateR(n As Long, k As Integer) As Long
    k = (32768 + k) Mod 32
    If k = 0 Then
        RotateR = n
    Else
        RotateR = ShiftR(n, k) Or ShiftL(n, 32 - k)
    End If
End Function
Function ClearBit(n As Long, k As Integer) As Long
    ClearBit = n And Not Bit(k)
End Function
Function SetBit(n As Long, k As Integer) As Long
    SetBit = n Or Bit(k)
End Function
Function SwitchBit(n As Long, k As Integer) As Long
    SwitchBit = n Xor Bit(k)
End Function
Function TestBit(n As Long, k As Integer) As Boolean
    TestBit = (n And Bit(k)) <> 0
End Function
```


Examples


```vb
Debug.Print Hex(MaskL(8))               'FF000000
Debug.Print Hex(MaskR(8))               'FF
Debug.Print Hex(Bit(7))                 '80
Debug.Print Hex(ShiftL(-1, 8))          'FFFFFF00
Debug.Print Hex(ShiftL(-1, -8))         'FFFFFF
Debug.Print Hex(ShiftR(-1, 8))          'FFFFFF
Debug.Print Hex(ShiftR(-1, -8))         'FFFFFF00
Debug.Print Hex(RotateL(65535, 8))      'FFFF00
Debug.Print Hex(RotateL(65535, -8))     'FF0000FF
Debug.Print Hex(RotateR(65535, 8))      'FF0000FF
Debug.Print Hex(RotateR(65535, -8))     'FFFF00

```



## Visual Basic

{{works with|Visual Basic|VB6 Standard}}
identical syntax as in [[#VBA]].


## Visual Basic .NET


```vbnet
Sub Test(a as Integer, b as Integer)
   WriteLine("And " & a And b)
   WriteLine("Or " & a Or b)
   WriteLine("Xor " & a Xor b)
   WriteLine("Not " & Not a)
   WriteLine("Left Shift " & a << 2)
   WriteLine("Right Shift " & a >> 2)
End Sub
```


Visual Basic doesn't have built-in support for bitwise rotation.


## x86 Assembly

{{works with|nasm}}
It must be linked with the libc and "start" code; lazyly a <tt>gcc bitops.o</tt> works, being bitops.o produced by <tt>nasm -f elf bitops.asm</tt> (I've chosen ELF since I am on a GNU/Linux box)

```asm
	extern printf
	global main

	section .text
main
	mov	eax, dword [_a]
	mov	ecx, dword [_b]
	push	ecx
	push	eax

	and 	eax, ecx
	mov	ebx, _opand
	call	out_ops

	call	get_nums
	or	eax, ecx
	mov	ebx, _opor
	call	out_ops

	call	get_nums
	xor     eax, ecx
	mov	ebx, _opxor
	call	out_ops

	call	get_nums
	shr	eax, cl
	mov	ebx, _opshr
	call	out_ops

	call	get_nums
	shl	eax, cl
	mov	ebx, _opshl
	call	out_ops

	call	get_nums
	rol	eax, cl
	mov	ebx, _oprol
	call	out_ops

	call	get_nums
	ror	eax, cl
	mov	ebx, _opror
	call	out_ops

	call	get_nums
	sal	eax, cl
	mov	ebx, _opsal
	call	out_ops

	call	get_nums
	sar	eax, cl
	mov	ebx, _opsar
	call	out_ops

	mov	eax, dword [esp+0]
	not	eax
	push 	eax
	not	eax
	push	eax
	push	_opnot
	push	_null
	push	_testn
	call	printf
	add	esp, 20

	add	esp, 8
	ret

out_ops
	push	eax
	push	ecx
	push	ebx
	push	dword [_a]
	push	_test
	call	printf
	add	esp, 20
	ret

get_nums
	mov	eax, dword [esp+4]
	mov	ecx, dword [esp+8]
	ret

	section .data

_a	dd	11
_b	dd	3

	section .rodata
_test	db	'%08x %s %08x = %08x', 10, 0
_testn	db	'%08s %s %08x = %08x', 10, 0
_opand	db	'and', 0
_opor	db	'or ', 0
_opxor	db	'xor', 0
_opshl	db	'shl', 0
_opshr	db	'shr', 0
_opror	db	'ror', 0
_oprol	db	'rol', 0
_opnot	db	'not', 0
_opsal	db	'sal', 0
_opsar	db	'sar', 0
_null	db 	0

	end
```



## XLISP


```lisp
(defun bitwise-operations (a b)
	; rotate operations are not supported
	(print `(,a and ,b = ,(logand a b)))
	(print `(,a or ,b = ,(logior a b)))
	(print `(,a xor ,b = ,(logxor a b)))
	(print `(,a left shift by ,b = ,(lsh a b)))
	(print `(,a right shift by ,b = ,(lsh a (- b)))) ; negative second operand shifts right
	(print `(,a arithmetic right shift by ,b = ,(ash a (- b)))) )
```



## XPL0


```XPL0
Text(0, "A and B = ");  HexOut(0, A and B);  CrLf(0);   \alternate symbol: &
Text(0, "A or B = ");   HexOut(0, A or B);   CrLf(0);   \alternate symbol: !
Text(0, "A xor B = ");  HexOut(0, A xor B);  CrLf(0);   \alternate symbol: |
Text(0, "not A = ");    HexOut(0, not A);    CrLf(0);   \alternate symbol: ~
Text(0, "A << B = ");   HexOut(0, A << B);   CrLf(0);
Text(0, "A >> B logical = ");  HexOut(0, A >> B);  CrLf(0);
Text(0, "A >> B arithmetic = ");  HexOut(0, A ->> B);  CrLf(0);

\Rotate operations must be done by calling a function such as:
func ROR(A, B); int A, B; return A>>B ! A<<(32-B);

Text(0, "A ror B = ");  HexOut(0, ROR(A,B));  CrLf(0);
```


The reason the "!" and "|" symbols may seem reversed is that the OR
operator was introduced at a time when only uppercase characters were
available (such as on the Apple II). The XOR operator was added later.


## Yabasic


```Yabasic
sub formBin$(n)
    return right$("00000000" + bin$(n), 8)
end sub

a = 6 : b = 3
print a, " = \t", formBin$(a)
print b, " = \t", formBin$(b)
print "\t--------"
print "AND = \t", formBin$(and(a, b))
print "OR = \t", formBin$(or(a, b))
print "XOR = \t", formBin$(xor(a, b))
print "NOT ", a, " =\t", formBin$(xor(255, a))
```


```txt
6 =	00000110
3 =	00000011
	--------
AND =	00000010
OR =	00000111
XOR =	00000101
NOT 6 =	11111001
```



## zkl

No bitwise rotates. Shifts are unsigned.

```zkl
(7).bitAnd(1) //-->1
(8).bitOr(1)  //-->9
(7).bitXor(1) //-->6
(1).bitNot() : "%,x".fmt(_) //-->ff|ff|ff|ff|ff|ff|ff|fe
(7).shiftRight(1) //-->3
(7).shiftLeft(1)  //-->0xe
(-1).toString(16) //-->ffffffffffffffff
(-1).shiftRight(1).toString(16) //-->7fffffffffffffff
```



{{omit from|bc|No built-in bitwise operations}}
{{omit from|dc|No built-in bitwise operations}}
{{omit from|GUISS}}
{{omit from|TPP}}
{{omit from|UNIX Shell}}
