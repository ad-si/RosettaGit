+++
title = "Long multiplication"
description = ""
date = 2019-05-24T11:25:56Z
aliases = []
[extra]
id = 3851
[taxonomies]
categories = []
tags = []
+++

{{task|Arbitrary precision}}
[[Category:Arbitrary precision]]
[[Category:Arithmetic operations]]

;Task:
Explicitly implement   [[wp:long multiplication|long multiplication]].

This is one possible approach to arbitrary-precision integer algebra.


For output, display the result of   <big><big> 2<sup>64</sup> * 2<sup>64</sup>.</big></big>


The decimal representation of   <big><big> 2<sup>64</sup> </big></big>   is:
 18,446,744,073,709,551,616

The output of   <big><big> 2<sup>64</sup> * 2<sup>64</sup> </big></big>   is   <big><big> 2<sup>128</sup>, </big></big>   and is:
 340,282,366,920,938,463,463,374,607,431,768,211,456





## 360 Assembly

For maximum compatibility, we use only the basic 370 instruction set (use of MVCL). Pseudo-macro instruction XPRNT can be replaced by a WTO.

```360asm
LONGINT  CSECT
         USING  LONGINT,R13
SAVEAREA B      PROLOG-SAVEAREA(R15)
         DC     17F'0'
         DC     CL8'LONGINT'
PROLOG   STM    R14,R12,12(R13)
         ST     R13,4(R15)
         ST     R15,8(R13)
         LR     R13,R15
         MVC    XX(1),=C'1'
         MVC    LENXX,=H'1'        xx=1
         LA     R2,64
LOOPII   ST     R2,RLOOPII         do for 64
         MVC    X-2(LL+2),XX-2     x=xx
         MVC    Y(1),=C'2'
         MVC    LENY,=H'1'         y=2
         BAL    R14,LONGMULT
         MVC    XX-2(LL+2),Z-2     xx=longmult(xx,2)   xx=xx*2
         L      R2,RLOOPII
ELOOPII  BCT    R2,LOOPII          loop
         MVC    X-2(LL+2),XX-2
         MVC    Y-2(LL+2),XX-2
         BAL    R14,LONGMULT
         MVC    YY-2(LL+2),Z-2     yy=longmult(xx,xx)  yy=xx*xx
         XPRNT  XX,LL              output xx
         XPRNT  YY,LL              output yy
RETURN   L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)
         XR     R15,R15            set return code
         BR     R14                return to caller
RLOOPII  DS     F
*
LONGMULT EQU    *                  function longmult z=(x,y)
         MVC    LENSHIFT,=H'0'     shift=''
         MVC    LENZ,=H'0'         z=''
         LH     R6,LENX
         LA     R6,1(R6)           from lenx
         XR     R8,R8
         BCTR   R8,0               by -1
         LA     R9,0               to 1
LOOPI    BXLE   R6,R8,ELOOPI       do i=lenx to 1 by -1
         LA     R2,X
         AR     R2,R6              +i
         BCTR   R2,0
         MVC    CI,0(R2)           ci=substr(x,i,1)
         IC     R0,CI              ni=integer(ci)
         N      R0,=X'0000000F'
         STH    R0,NI
         MVC    LENT,=H'0'         t=''
         SR     R0,R0
         STH    R0,CARRY           carry=0
         LH     R7,LENY
         LA     R7,1(R7)           from lenx
         XR     R10,R10
         BCTR   R10,0              by -1
         LA     R11,0              to 1
LOOPJ1   BXLE   R7,R10,ELOOPJ1     do j=leny to 1 by -1
         LA     R2,Y
         AR     R2,R7              +j
         BCTR   R2,0
         MVC    CJ,0(R2)           cj=substr(y,j,1)
         IC     R0,CJ
         N      R0,=X'0000000F'
         STH    R0,NJ              nj=integer(cj)
         LH     R2,NI
         MH     R2,NJ
         AH     R2,CARRY
         STH    R2,NKR             nkr=ni*nj+carry
         LH     R2,NKR
         LA     R1,10
         SRDA   R2,32
         DR     R2,R1
         STH    R2,NK              nk=nkr//10
         STH    R3,CARRY           carry=nkr/10
         LH     R2,NK
         O      R2,=X'000000F0'
         STC    R2,CK              ck=string(nk)
         MVC    TEMP,T
         MVC    T(1),CK
         MVC    T+1(LL-1),TEMP
         LH     R2,LENT
         LA     R2,1(R2)
         STH    R2,LENT            t=ck!!t
         B      LOOPJ1             next j
ELOOPJ1  EQU    *
         LH     R2,CARRY
         O      R2,=X'000000F0'
         STC    R2,CK              ck=string(carry)
         MVC    TEMP,T
         MVC    T(1),CK
         MVC    T+1(LL-1),TEMP
         LH     R2,LENT
         LA     R2,1(R2)
         STH    R2,LENT            t=ck!!t
         LA     R2,T
         AH     R2,LENT
         LH     R3,LENSHIFT
         LA     R4,SHIFT
         LH     R5,LENSHIFT
         MVCL   R2,R4
         LH     R2,LENT
         AH     R2,LENSHIFT
         STH    R2,LENT            t=t!!shift
IF1      LH     R4,LENZ
         CH     R4,LENT            if lenz>lent
         BNH    ELSE1
         LH     R2,LENZ            then
         LA     R2,1(R2)
         STH    R2,L               l=lenz+1
         B      EIF1
ELSE1    LH     R2,LENT            else
         LA     R2,1(R2)
         STH    R2,L               l=lent+1
EIF1     EQU    *
         MVI    TEMP,C'0'          to
         MVC    TEMP+1(LL-1),TEMP
         LA     R2,TEMP
         AH     R2,L
         SH     R2,LENZ
         LH     R3,LENZ
         LA     R4,Z
         LH     R5,LENZ
         MVCL   R2,R4
         MVC    LENZ,L
         MVC    Z,TEMP             z=right(z,l,'0')
         MVI    TEMP,C'0'          to
         MVC    TEMP+1(LL-1),TEMP
         LA     R2,TEMP
         AH     R2,L
         SH     R2,LENT
         LH     R3,LENT
         LA     R4,T
         LH     R5,LENT
         MVCL   R2,R4
         MVC    LENT,L
         MVC    T,TEMP             t=right(t,l,'0')
         MVC    LENW,=H'0'         w=''
         SR     R0,R0
         STH    R0,CARRY           carry=0
         LH     R7,L
         LA     R7,1(R7)           from l
         XR     R10,R10
         BCTR   R10,0              by -1
         LA     R11,0              to 1
LOOPJ2   BXLE   R7,R10,ELOOPJ2     do j=l to 1 by -1
         LA     R2,Z
         AR     R2,R7              +j
         BCTR   R2,0
         MVC    CZ,0(R2)           cz=substr(z,j,1)
         IC     R0,CZ
         N      R0,=X'0000000F'
         STH    R0,NZ              nz=integer(cz)
         LA     R2,T
         AR     R2,R7              -j
         BCTR   R2,0
         MVC    CT,0(R2)           ct=substr(t,j,1)
         IC     R0,CT
         N      R0,=X'0000000F'
         STH    R0,NT              nt=integer(ct)
         LH     R2,NZ
         AH     R2,NT
         AH     R2,CARRY
         STH    R2,NKR             nkr=nz+nt+carry
         LH     R2,NKR
         LA     R1,10
         SRDA   R2,32
         DR     R2,R1
         STH    R2,NK
         STH    R3,CARRY           nk=nkr//10; carry=nkr/10
         LH     R2,NK
         O      R2,=X'000000F0'
         STC    R2,CK              ck=string(nk)
         MVC    TEMP,W
         MVC    W(1),CK
         MVC    W+1(LL-1),TEMP
         LH     R2,LENW
         LA     R2,1(R2)
         STH    R2,LENW            w=ck!!w
         B      LOOPJ2             next j
ELOOPJ2  EQU    *
         LH     R2,CARRY
         O      R2,=X'000000F0'
         STC    R2,CK             ck=string(carry)
         MVC    Z(1),CK
         MVC    Z+1(LL-1),W
         LH     R2,LENW
         LA     R2,1(R2)
         STH    R2,LENZ            z=ck!!w
         LA     R7,0               from 1
         LA     R10,1              by 1
         LH     R11,LENZ           to lenz
LOOPJ3   BXH    R7,R10,ELOOPJ3     do j=1 to lenz
         LA     R2,Z
         AR     R2,R7              j
         BCTR   R2,0
         MVC    ZJ(1),0(R2)        zj=substr(z,j,1)
         CLI    ZJ,C'0'            if zj^='0'
         BNE    ELOOPJ3            then leave j
         B      LOOPJ3             next j
ELOOPJ3  EQU    *
IF2      CH     R7,LENZ            if j>lenz
         BNH    EIF2
         LH     R7,LENZ            then j=lenz
EIF2     EQU    *
         LA     R2,TEMP            to
         LH     R3,LENZ
         SR     R3,R7              -j
         LA     R3,1(R3)
         STH    R3,LENTEMP
         LA     R4,Z               from
         AR     R4,R7              +j
         BCTR   R4,0
         LR     R5,R3
         MVCL   R2,R4
         MVC    Z-2(LL+2),TEMP-2   z=substr(z,j)
         LA     R2,SHIFT
         AH     R2,LENSHIFT
         MVI    0(R2),C'0'
         LH     R3,LENSHIFT
         LA     R3,1(R3)
         STH    R3,LENSHIFT        shift=shift!!'0'
         MVC    TEMP,Z
         LA     R2,TEMP
         AH     R2,LENZ
         MVC    0(2,R2),=C'  '
         B      LOOPI              next i
ELOOPI   EQU    *
         MVI    TEMP,C' '
         LA     R2,Z
         AH     R2,LENZ
         LH     R3,=AL2(LL)
         SH     R3,LENZ
         LA     R4,TEMP
         LH     R5,=H'1'
         ICM    R5,8,=C' '
         MVCL   R2,R4              z=clean(z)
         BR     R14                end function longmult
*
L        DS     H
NI       DS     H
NJ       DS     H
NK       DS     H
NZ       DS     H
NT       DS     H
CARRY    DS     H
NKR      DS     H
CI       DS     CL1
CJ       DS     CL1
CZ       DS     CL1
CT       DS     CL1
CK       DS     CL1
ZJ       DS     CL1
LENXX    DS     H
XX       DS     CL94
LENYY    DS     H
YY       DS     CL94
LENX     DS     H
X        DS     CL94
LENY     DS     H
Y        DS     CL94
LENZ     DS     H
Z        DS     CL94
LENT     DS     H
T        DS     CL94
LENW     DS     H
W        DS     CL94
LENSHIFT DS     H
SHIFT    DS     CL94
LENTEMP  DS     H
TEMP     DS     CL94
LL       EQU    94
         YREGS
         END    LONGINT
```

{{out}}

```txt

18446744073709551616
340282366920938463463374607431768211456

```



## Ada


===Using properly range-checked integers===

(The source text for these examples can also be found on [https://bitbucket.org/ada_on_rosetta_code/solutions Bitbucket].)

First we specify the required operations and declare our number type as an array of digits (in base 2^16):

```ada
package Long_Multiplication is
   type Number (<>) is private;

   Zero : constant Number;
   One  : constant Number;

   function Value (Item : in String) return Number;
   function Image (Item : in Number) return String;

   overriding
   function "=" (Left, Right : in Number) return Boolean;

   function "+" (Left, Right : in Number) return Number;
   function "*" (Left, Right : in Number) return Number;

   function Trim (Item : in Number) return Number;
private
   Bits : constant := 16;
   Base : constant := 2 ** Bits;

   type Accumulated_Value is range 0 .. (Base - 1) * Base;
   subtype Digit is Accumulated_Value range 0 .. Base - 1;

   type Number is array (Natural range <>) of Digit;
   for Number'Component_Size use Bits; -- or pragma Pack (Number);

   Zero : constant Number := (1 .. 0 => 0);
   One  : constant Number := (0 => 1);

   procedure Divide (Dividend  : in     Number;
                     Divisor   : in     Digit;
                     Result    :    out Number;
                     Remainder :    out Digit);
end Long_Multiplication;
```

Some of the operations declared above are useful helper operations for the conversion of numbers to and from base 10 digit strings.

Then we implement the operations:

```ada
package body Long_Multiplication is
   function Value (Item : in String) return Number is
      subtype Base_Ten_Digit is Digit range 0 .. 9;
      Ten : constant Number := (0 => 10);
   begin
      case Item'Length is
         when 0 =>
            raise Constraint_Error;
         when 1 =>
            return (0 => Base_Ten_Digit'Value (Item));
         when others =>
            return (0 => Base_Ten_Digit'Value (Item (Item'Last .. Item'Last)))
              + Ten * Value (Item (Item'First .. Item'Last - 1));
      end case;
   end Value;

   function Image (Item : in Number) return String is
      Base_Ten  : constant array (Digit range 0 .. 9) of String (1 .. 1) :=
                    ("0", "1", "2", "3", "4", "5", "6", "7", "8", "9");
      Result    : Number (0 .. Item'Last);
      Remainder : Digit;
   begin
      if Item = Zero then
         return "0";
      else
         Divide (Dividend  => Item,
                 Divisor   => 10,
                 Result    => Result,
                 Remainder => Remainder);

         if Result = Zero then
            return Base_Ten (Remainder);
         else
            return Image (Trim (Result)) & Base_Ten (Remainder);
         end if;
      end if;
   end Image;

   overriding
   function "=" (Left, Right : in Number) return Boolean is
   begin
      for Position in Integer'Min (Left'First, Right'First) ..
                      Integer'Max (Left'Last,  Right'Last) loop
         if Position in Left'Range and Position in Right'Range then
            if Left (Position) /= Right (Position) then
               return False;
            end if;
         elsif Position in Left'Range then
            if Left (Position) /= 0 then
               return False;
            end if;
         elsif Position in Right'Range then
            if Right (Position) /= 0 then
               return False;
            end if;
         else
            raise Program_Error;
         end if;
      end loop;

      return True;
   end "=";

   function "+" (Left, Right : in Number) return Number is
      Result      : Number (Integer'Min (Left'First, Right'First) ..
                            Integer'Max (Left'Last , Right'Last) + 1);
      Accumulator : Accumulated_Value := 0;
      Used        : Integer := Integer'First;
   begin
      for Position in Result'Range loop
         if Position in Left'Range then
            Accumulator := Accumulator + Left (Position);
         end if;

         if Position in Right'Range then
            Accumulator := Accumulator + Right (Position);
         end if;

         Result (Position) := Accumulator mod Base;
         Accumulator := Accumulator / Base;

         if Result (Position) /= 0 then
            Used := Position;
         end if;
      end loop;

      if Accumulator = 0 then
         return Result (Result'First .. Used);
      else
         raise Constraint_Error;
      end if;
   end "+";

   function "*" (Left, Right : in Number) return Number is
      Accumulator : Accumulated_Value;
      Result      : Number (Left'First + Right'First ..
                            Left'Last  + Right'Last + 1) := (others => 0);
      Used        : Integer := Integer'First;
   begin
      for L in Left'Range loop
         for R in Right'Range loop
            Accumulator := Left (L) * Right (R);

            for Position in L + R .. Result'Last loop
               exit when Accumulator = 0;

               Accumulator := Accumulator + Result (Position);
               Result (Position) := Accumulator mod Base;
               Accumulator := Accumulator / Base;
               Used := Position;
            end loop;
         end loop;
      end loop;

      return Result (Result'First .. Used);
   end "*";

   procedure Divide (Dividend  : in     Number;
                     Divisor   : in     Digit;
                     Result    :    out Number;
                     Remainder :    out Digit) is
      Accumulator : Accumulated_Value := 0;
   begin
      Result := (others => 0);

      for Position in reverse Dividend'Range loop
         Accumulator := Accumulator * Base + Dividend (Position);
         Result (Position) := Accumulator / Divisor;
         Accumulator := Accumulator mod Divisor;
      end loop;

      Remainder := Accumulator;
   end Divide;

   function Trim (Item : in Number) return Number is
   begin
      for Position in reverse Item'Range loop
         if Item (Position) /= 0 then
            return Item (Item'First .. Position);
         end if;
      end loop;

      return Zero;
   end Trim;
end Long_Multiplication;
```


And finally we have the requested test application:

```ada
with Ada.Text_IO;
with Long_Multiplication;

procedure Test_Long_Multiplication is
   use Ada.Text_IO, Long_Multiplication;

   N : Number := Value ("18446744073709551616");
   M : Number := N * N;
begin
   Put_Line (Image (N) & " * " & Image (N) & " = " & Image (M));
end Test_Long_Multiplication;
```


{{out}}

```txt
18446744073709551616 * 18446744073709551616 = 340282366920938463463374607431768211456
```



### Using modular types


The following implementation uses representation of a long number by an array of 32-bit elements:

```ada
type Long_Number is array (Natural range <>) of Unsigned_32;

function "*" (Left, Right : Long_Number) return Long_Number is
   Result : Long_Number (0..Left'Length + Right'Length - 1) := (others => 0);
   Accum  : Unsigned_64;
begin
   for I in Left'Range loop
      for J in Right'Range loop
         Accum := Unsigned_64 (Left (I)) * Unsigned_64 (Right (J));
         for K in I + J..Result'Last loop
            exit when Accum = 0;
            Accum := Accum + Unsigned_64 (Result (K));
            Result (K) := Unsigned_32 (Accum and 16#FFFF_FFFF#);
            Accum := Accum / 2**32;
         end loop;
      end loop;
   end loop;
   for Index in reverse Result'Range loop -- Normalization
      if Result (Index) /= 0 then
         return Result (0..Index);
      end if;
   end loop;
   return (0 => 0);
end "*";
```


The task requires conversion into decimal base. For this we also need division to short number with a remainder. Here it is:

```ada
procedure Div
          (  Dividend  : in out Long_Number;
             Last      : in out Natural;
             Remainder : out Unsigned_32;
             Divisor   : Unsigned_32
          )  is
   Div   : constant Unsigned_64 := Unsigned_64 (Divisor);
   Accum : Unsigned_64 := 0;
   Size  : Natural     := 0;
begin
   for Index in reverse Dividend'First..Last loop
      Accum := Accum * 2**32 + Unsigned_64 (Dividend (Index));
      Dividend (Index) := Unsigned_32 (Accum / Div);
      if Size = 0 and then Dividend (Index) /= 0 then
         Size := Index;
      end if;
      Accum := Accum mod Div;
   end loop;
   Remainder := Unsigned_32 (Accum);
   Last := Size;
end Div;
```


With the above the test program:

```ada
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Text_IO;            use Ada.Text_IO;
with Interfaces;             use Interfaces;

procedure Long_Multiplication is
   -- Insert definitions above here
   procedure Put (Value : Long_Number) is
      X      : Long_Number := Value;
      Last   : Natural     := X'Last;
      Digit  : Unsigned_32;
      Result : Unbounded_String;
   begin
      loop
         Div (X, Last, Digit, 10);
         Append (Result, Character'Val (Digit + Character'Pos ('0')));
         exit when Last = 0 and then X (0) = 0;
      end loop;
      for Index in reverse 1..Length (Result) loop
         Put (Element (Result, Index));
      end loop;
   end Put;

   X : Long_Number := (0 => 0, 1 => 0, 2 => 1) * (0 => 0, 1 => 0, 2 => 1);
begin
   Put (X);
end Long_Multiplication;
```


Sample output:

```txt

340282366920938463463374607431768211456

```



## Aime


```aime
data b, c, v;
integer d, e, i, j, s;

b = 1.argv;
b.dump(',');
v = 2.argv;
v.dump(',');

c.run(~b + ~v + 1, 0);

for (i, d in b) {
    b[i] = d - '0';
}

for (j, d of v) {
    d = v[j] - '0';

    s = 0;
    for (i, e of b) {
        s += e * d + c[i + j];
        c[i + j] = s % 10;
        s /= 10;
    }
    while (s) {
        s += c[i + j];
        c[i + j] = s % 10;
        s /= 10;
        i -= 1;
    }
}

c.delete(-1);
c.bf_drop0("");

for (i, d in c) {
    c[i] = d + '0';
}

o_form("~\n", c);
```



## ALGOL 68

The long multiplication for the golden ratio has been included as half the digits
cancel and end up as being zero.  This is useful for testing.


### Built in or standard distribution routines

{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
[[ALGOL 68G]] allows any precision for '''long long int''' to be defined
when the program is run, e.g. 200 digits.

```algol68
PRAGMAT precision=200 PRAGMAT
MODE INTEGER = LONG LONG INT;

LONG INT default integer width := 69;
INT width = 69+2;

INT fix w = 1, fix h = 1; # round up #

LONG LONG INT golden ratio w := ENTIER ((long long sqrt(5)-1) / 2 * LENG LENG 10 ** default integer width + fix w),
              golden ratio h := ENTIER ((long long sqrt(5)+1) / 2 * LENG LENG 10 ** default integer width + fix h);

test: (
  print((
    "The approximate golden ratios, width: ",  whole(golden ratio w,width), new line,
    "                              length: ", whole(golden ratio h,width), new line,
    "                product is exactly: ", whole(golden ratio w*golden ratio h,width*2), new line));

  INTEGER two to the power of 64 = LONG 2 ** 64;
  INTEGER neg two to the power of 64 = -(LONG 2 ** 64);
  print(("2 ** 64 * -(2 ** 64) = ", whole(two to the power of 64*neg two to the power of 64,width), new line))
)
```

Output:

```txt

The approximate golden ratios, width:  +618033988749894848204586834365638117720309179805762862135448622705261
                              length: +1618033988749894848204586834365638117720309179805762862135448622705261
                product is exactly:   +1000000000000000000000000000000000000000000000000000000000000000000001201173450350400438606015942314498798603569682901026716145698077078121
2 ** 64 * -(2 ** 64) =                                -340282366920938463463374607431768211456

```



### Implementation example

{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
<!-- {{does not works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386 - translator throws and assert.  I'm not sure why.}} -->

```algol68
MODE DIGIT = INT;
MODE INTEGER = FLEX[0]DIGIT; # an arbitary number of digits #

# "digits" are stored in digit base ten, but 10000 & 2**n (inc hex) can be used #
INT digit base = 1000;

# if possible, then print the digit with one character #
STRING hex digit repr = "0123456789abcdefghijklmnopqrstuvwxyz"[AT 0];
INT digit base digit width = ( digit base <=  UPB hex digit repr + 1 | 1 | 1 + ENTIER log(digit base-1) );

INT next digit = -1; # reverse order so digits appear in "normal" order when printed #

PROC raise value error = ([]STRING args)VOID:
  ( print(("Value Error: ", args, new line)); stop );

PROC raise not implemented error = ([]STRING args)VOID:
  ( print(("Not implemented Error: ", args, new line)); stop );

PROC raise integer not implemented error = (STRING message)INTEGER:
  ( raise not implemented error(("INTEGER ", message)); SKIP );

INT half max int = max int OVER 2;
IF digit base > half max int THEN raise value error("INTEGER addition may fail") FI;

INT sqrt max int = ENTIER sqrt(max int);
IF digit base > sqrt max int THEN raise value error("INTEGER multiplication may fail") FI;

# initialise/cast a INTEGER from a LONG LONG INT #
OP INTEGERINIT = (LONG LONG INT number)INTEGER:(
  [1 + ENTIER (SHORTEN SHORTEN long long log(ABS number) / log(digit base))]DIGIT out;
  LONG LONG INT carry := number;
  FOR digit out FROM UPB out BY next digit TO LWB out DO
    LONG LONG INT prev carry := carry;
    carry %:= digit base; # avoid MOD as it doesn't under handle -ve numbers #
    out[digit out] := SHORTEN SHORTEN (prev carry - carry * digit base)
  OD;
  out
);

# initialise/cast a INTEGER from an LONG INT #
OP INTEGERINIT = (LONG INT number)INTEGER: INTEGERINIT LENG number;

# initialise/cast a INTEGER from an INT #
OP INTEGERINIT = (INT number)INTEGER: INTEGERINIT LENG LENG number;

# remove leading zero "digits" #
OP NORMALISE = ([]DIGIT number)INTEGER: (
  INT leading zeros := LWB number - 1;
  FOR digit number FROM LWB number TO UPB number
    WHILE number[digit number] = 0 DO leading zeros := digit number OD;
  IF leading zeros = UPB number THEN 0 ELSE number[leading zeros+1:] FI
);

#####################################################################
  Define a standard representation for the INTEGER mode.  Note: this is
  rather crude because for a large "digit base" the number is represented as
  blocks of decimals. It works nicely for powers of ten (10,100,1000,...),
  but for most larger bases (greater then 35) the repr will be a surprise.
#####################################################################
OP REPR = (DIGIT d)STRING:
    IF digit base > UPB hex digit repr THEN
      STRING out := whole(ABS d, -digit base digit width);
# Replace spaces with zeros #
      FOR digit out FROM LWB out TO UPB out DO
        IF out[digit out] = " " THEN out[digit out] := "0" FI
      OD;
      out
    ELSE # small enough to represent as ASCII (hex) characters #
      hex digit repr[ABS d]
    FI;

OP REPR = (INTEGER number)STRING:(
  STRING sep = ( digit base digit width > 1 | "," | "" );
  INT width := digit base digit width + UPB sep;
  [width * UPB number - UPB sep]CHAR out;
  INT leading zeros := LWB out - 1;
  FOR digit TO UPB number DO
    INT start := digit * width - width + 1;
    out[start:start+digit base digit width-1] := REPR number[digit];
    IF digit base digit width /= 1 & digit /= UPB number THEN
      out[start+digit base digit width] := ","
    FI
  OD;

# eliminate leading zeros #
  FOR digit out FROM LWB out TO UPB out
    WHILE out[digit out] = "0" OR out[digit out] = sep
  DO leading zeros := digit out OD;

  CHAR sign = ( number[1]<0 | "-" | "+" );
# finally return the semi-normalised result #
  IF leading zeros = UPB out THEN "0" ELSE sign + out[leading zeros+1:] FI
);
```



```algol68
################################################################
# Finally Define the required INTEGER multiplication OPerator. #
################################################################
OP * = (INTEGER a, b)INTEGER:(

# initialise out to all zeros #
  [UPB a + UPB b]INT ab; FOR place ab TO UPB ab DO ab[place ab]:=0 OD;

  FOR place a FROM UPB a BY next digit TO LWB a DO
    DIGIT carry := 0;

# calculate each digit (whilst removing the carry) #
    FOR place b FROM UPB b BY next digit TO LWB b DO
      # n.b. result may be 2 digits #
      INT result := ab[place a + place b] + a[place a]*b[place b] + carry;
      carry := result % digit base; # avoid MOD as it doesn't under handle -ve numbers #
      ab[place a + place b] := result  - carry * digit base
    OD;
    ab[place a + LWB b + next digit] +:= carry

  OD;
  NORMALISE ab
);
```



```algol68
# The following standard operators could (potentially) also be defined #
OP -   = (INTEGER a)INTEGER: raise integer not implemented error("monadic minus"),
  ABS  = (INTEGER a)INTEGER: raise integer not implemented error("ABS"),
  ODD  = (INTEGER a)INTEGER: raise integer not implemented error("ODD"),
  BIN  = (INTEGER a)INTEGER: raise integer not implemented error("BIN");

OP +  = (INTEGER a, b)INTEGER: raise integer not implemented error("addition"),
   -  = (INTEGER a, b)INTEGER: raise integer not implemented error("subtraction"),
   /  = (INTEGER a, b)REAL: ( VOID(raise integer not implemented error("floating point division")); SKIP),
   %  = (INTEGER a, b)INTEGER: raise integer not implemented error("fixed point division"),
   %* = (INTEGER a, b)INTEGER: raise integer not implemented error("modulo division"),
   ** = (INTEGER a, b)INTEGER: raise integer not implemented error("to the power of");

LONG INT default integer width := long long int width - 2;

INT fix w = -1177584, fix h = -3915074; # floating point error, probably GMP/hardware specific #

INTEGER golden ratio w := INTEGERINIT ENTIER ((long long sqrt(5)-1) / 2 * LENG LENG 10 ** default integer width + fix w),
        golden ratio h := INTEGERINIT ENTIER ((long long sqrt(5)+1) / 2 * LENG LENG 10 ** default integer width + fix h);

test: (
  print((
    "The approximate golden ratios, width: ",  REPR golden ratio w, new line,
    "                            length: ", REPR golden ratio h, new line,
    "                product is exactly: ", REPR (golden ratio w * golden ratio h), new line));

  INTEGER two to the power of 64 = INTEGERINIT(LONG 2 ** 64);
  INTEGER neg two to the power of 64 = INTEGERINIT(-(LONG 2 ** 64));
  print(("2 ** 64 * -(2 ** 64) = ", REPR (two to the power of 64 * neg two to the power of 64), new line))
)
```


Output:

```txt

The approximate golden ratios, width: +618,033,988,749,894,848,204,586,834,365,638,117,720,309,179,805,762,862,135,448,622,705,261
                            length: +1,618,033,988,749,894,848,204,586,834,365,638,117,720,309,179,805,762,862,135,448,622,705,261
                product is exactly: +1,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,001,201,173,450,350,400,438,606,015,942,314,498,798,603,569,682,901,026,716,145,698,077,078,121
2 ** 64 * -(2 ** 64) = -340,282,366,920,938,463,463,374,607,431,768,211,456

```



### Other libraries or implementation specific extensions

As of February 2009 no open source libraries to do this task have been located.


## ALGOL W


```algolw
begin
    % long multiplication of large integers                                 %
    % large integers are represented by arrays of integers whose absolute   %
    % values are in 0 .. ELEMENT_MAX - 1                                    %
    % negative large integers should have negative values in all non-zero   %
    % elements                                                              %
    % the least significant digits of the large integer are in element 1    %
    integer ELEMENT_DIGITS; % number of digits in an element of a large     %
                            % integer                                       %
    integer ELEMENT_MAX;    % max absolute value of an element of a large   %
                            % integer - must be 10^( ELEMENT_DIGITS + 1 )   %
    integer ELEMENT_COUNT;  % number of elements in each large integer      %
    % implements long multiplication, c is set to a * b                     %
    %     c can be the same array as a or b                                 %
    %     n is the number of elements in the large integers a, b and c      %
    procedure longMultiply( integer array a, b, c ( * )
                          ; integer value n
                          ) ;
    begin
        % multiplies the large integer in b by the integer a, the result    %
        %     is added to c, starting from offset                           %
        %     overflow is ignored                                           %
        procedure multiplyElement( integer value a
                                 ; integer array b, c ( * )
                                 ; integer value offset, n
                                 ) ;
        begin
            integer carry, cPos;
            carry := 0;
            cPos  := offset;
            for bPos := 1 until highestNonZeroElementPosition( b, ( n + 1 ) - offset ) do begin
                integer cElement;
                cElement := c( cPos ) + ( a * b( bPos ) ) + carry;
                if abs cElement < ELEMENT_MAX then carry := 0
                else begin
                    % have digits to carry                                  %
                    carry    := cElement div ELEMENT_MAX;
                    cElement := ( abs cElement ) rem ELEMENT_MAX;
                    if carry < 0 then cElement := - cElement
                end if_no_carry_ ;
                c( cPos ) := cElement;
                cPos := cPos + 1
            end for_aPos ;
            if cPos <= n then c( cPos ) := carry
        end multiplyElement ;
        integer array mResult ( 1 :: n );
        % the result will be computed in mResult, allowing a or b to be c   %
        for rPos := 1 until n do mResult( rPos ) := 0;
        % multiply and add each element to the result                       %
        for aPos := 1 until highestNonZeroElementPosition( a, n ) do begin
            if a( aPos ) not = 0 then multiplyElement( a( aPos ), b, mResult, aPos, n )
        end for_aPos ;
        % return the result in c                                            %
        for rPos := 1 until n do c( rPos ) := mResult( rPos )
    end longMultiply ;
    % writes the decimal value of a large integer a with n elements         %
    procedure writeonLargeInteger( integer array a ( * )
                                 ; integer value n
                                 ) ;
    begin
        integer aMax;
        aMax := highestNonZeroElementPosition( a, n );
        if aMax < 1 then writeon( "0" )
        else begin
            % the large integer is non-zero                                 %
            writeon( i_w := 1, s_w := 0, a( aMax ) ); % highest element     %
            % handle the remaining elements - show leading zeros            %
            for aPos := aMax - 1 step -1 until 1 do begin
                integer v;
                integer array digits ( 1 :: ELEMENT_DIGITS );
                v := abs a( aPos );
                for dPos := ELEMENT_DIGITS step -1 until 1 do begin
                    digits( dPos ) := v rem 10;
                    v              := v div 10
                end for_dPos;
                for dPos := 1 until ELEMENT_DIGITS do writeon( i_w := 1, s_w := 0, digits( dPos ) )
            end for_aPos
        end if_aMax_lt_1_
    end writeonLargeInteger ;
    % returns the position of the highest non-zero element of the large     %
    %     integer a with n elements                                         %
    integer procedure highestNonZeroElementPosition( integer array a ( * )
                                                   ; integer value n
                                                   ) ;
    begin
        integer aMax;
        aMax := n;
        while aMax > 0 and a( aMax ) = 0 do aMax := aMax - 1;
        aMax
    end highestNonZeroElementPosition ;
    % allow each element to contain 4 decimal digits, so element by element %
    % multiplication won't overflow 32-bits                                 %
    ELEMENT_DIGITS :=     4;
    ELEMENT_MAX    := 10000;
    ELEMENT_COUNT  :=    12; % allows up to 48 digits - enough for the task %
    begin
        integer array twoTo64, twoTo128 ( 1 :: ELEMENT_COUNT );
        integer pwr;
        % construct 2^64 in twoTo64                                         %
        for tPos := 2 until ELEMENT_COUNT do twoTo64( tPos ) := 0;
        twoTo64( 1 ) := 2;
        pwr          := 1;
        while pwr < 64 do begin
            longMultiply( twoTo64, twoTo64, twoTo64, ELEMENT_COUNT );
            pwr := pwr * 2
        end while_pwr_lt_64 ;
        % construct 2^128                                                   %
        longMultiply( twoTo64, twoTo64, twoTo128, ELEMENT_COUNT );
        write( "2^128: " );
        writeonLargeInteger( twoTo128, ELEMENT_COUNT )
    end
end.
```

{{out}}

```txt

2^128: 340282366920938463463374607431768211456

```



## AutoHotkey

ahk [http://www.autohotkey.com/forum/viewtopic.php?t=44657&postdays=0&postorder=asc&start=149 discussion]

```autohotkey
MsgBox % x := mul(256,256)
MsgBox % x := mul(x,x)
MsgBox % x := mul(x,x) ; 18446744073709551616
MsgBox % x := mul(x,x) ; 340282366920938463463374607431768211456

mul(b,c) { ; <- b*c
   VarSetCapacity(a, n:=StrLen(b)+StrLen(c), 48), NumPut(0,a,n,"char")
   Loop % StrLen(c) {
      i := StrLen(c)+1-A_Index, cy := 0
      Loop % StrLen(b) {
         j := StrLen(b)+1-A_Index,
         t := SubStr(a,i+j,1) + SubStr(b,j,1) * SubStr(c,i,1) + cy
         cy := t // 10
         NumPut(mod(t,10)+48,a,i+j-1,"char")
      }
      NumPut(cy+48,a,i+j-2,"char")
   }
   Return cy ? a : SubStr(a,2)
}
```


=={{Header|AWK}}==
{{works with|gawk|3.1.7}}
{{works with|nawk|20100523}}
{{trans|Tcl}}

```awk
BEGIN {
    DEBUG = 0
    n = 2^64
    nn = sprintf("%.0f", n)
    printf "2^64 * 2^64 = %.0f\n", multiply(nn, nn)
    printf "2^64 * 2^64 = %.0f\n", n*n
    exit
}

function multiply(x, y,     len_x,len_y,ax,ay,j,m,c,i,k,d,v,res,mul,result) {
    len_x = split_reverse(x, ax)
    len_y = split_reverse(y, ay)
    print_array(ax)
    print_array(ay)
    for (j=1; j<=len_y; j++) {
        m = ay[j]
        c = 0
        i = j - 1
        for (k=1; k<=len_x; k++) {
            d = ax[k]
            i++
            v = res[i]
            if (v == "") {
                append_array(res, 0)
                v = 0
            }
            mul = v + c + d*m
            c = int(mul / 10)
            v = mul % 10
            res[i] = v
        }
        append_array(res, c)
    }
    print_array(res)
    result = reverse_join(res)
    sub(/^0+/, "", result)
    return result
}

function split_reverse(x, a,        a_x) {
    split(x, a_x, "")
    return reverse_array(a_x, a)
}

function reverse_array(a,b,         len,i) {
    len = length_array(a)
    for (i in a) {
        b[1+len-i] = a[i]
    }
    return len
}

function length_array(a,        len,i) {
    len = 0
    for (i in a) len++
    return len
}

function append_array(a, value,     len) {
    len = length_array(a)
    a[++len] = value
}

function reverse_join(a,        len,str,i) {
    len = length_array(a)
    str = ""
    for (i=len; i>=1; i--) {
        str = str a[i]
    }
    return str
}

function print_array(a,         len,i) {
    if (DEBUG) {
        len = length_array(a)
        print "length=" len
        for (i=1; i<=len; i++) {
            printf("%s ", i%10)
        }
        print ""
        for (i=1; i<=len; i++) {
            #print i " " a[i]
            printf("%s ", a[i])
        }
        print ""
        print "===="
    }
}
```

outputs:

```txt
2^64 * 2^64 = 340282366920938463463374607431768211456
2^64 * 2^64 = 340282366920938463463374607431768211456
```


=={{Header|BASIC}}==
{{works with|QBasic|}}


### Version 1


```qbasic
'PROGRAM : BIG MULTIPLICATION VER #1
'LRCVS 01.01.2010
'THIS PROGRAM SIMPLY MAKES A MULTIPLICATION
'WITH ALL THE PARTIAL PRODUCTS.
'............................................................

DECLARE SUB A.INICIO (A$, B$)
DECLARE SUB B.STORE (CAD$, N$)
DECLARE SUB C.PIZARRA ()
DECLARE SUB D.ENCABEZADOS (A$, B$)
DECLARE SUB E.MULTIPLICACION (A$, B$)
DECLARE SUB G.SUMA ()
DECLARE FUNCTION F.INVCAD$ (CAD$)

RANDOMIZE TIMER
CALL A.INICIO(A$, B$)
CALL B.STORE(A$, "A")
CALL B.STORE(B$, "B")
CALL C.PIZARRA
CALL D.ENCABEZADOS(A$, B$)
CALL E.MULTIPLICACION(A$, B$)
CALL G.SUMA

SUB A.INICIO (A$, B$)
    CLS
'Note: Number of digits > 1000
    INPUT "NUMBER OF DIGITS  "; S
    CLS
    A$ = ""
    B$ = ""
    FOR N = 1 TO S
        A$ = A$ + LTRIM$(STR$(INT(RND * 9)))
    NEXT N
    FOR N = 1 TO S
        B$ = B$ + LTRIM$(STR$(INT(RND * 9)))
    NEXT N
END SUB

SUB B.STORE (CAD$, N$)
    OPEN "O", #1, N$
    FOR M = LEN(CAD$) TO 1 STEP -1
        WRITE #1, MID$(CAD$, M, 1)
    NEXT M
    CLOSE (1)
END SUB

SUB C.PIZARRA
    OPEN "A", #3, "R"
    WRITE #3, ""
    CLOSE (3)
    KILL "R"
END SUB

SUB D.ENCABEZADOS (A$, B$)
    LT = LEN(A$) + LEN(B$) + 1
    L$ = STRING$(LT, " ")
    OPEN "A", #3, "R"
    MID$(L$, LT - LEN(A$) + 1) = A$
    WRITE #3, L$
    CLOSE (3)
    L$ = STRING$(LT, " ")
    OPEN "A", #3, "R"
    MID$(L$, LT - LEN(B$) - 1) = "X " + B$
    WRITE #3, L$
    CLOSE (3)
END SUB

SUB E.MULTIPLICACION (A$, B$)
    LT = LEN(A$) + LEN(B$) + 1
    L$ = STRING$(LT, " ")
    C$ = ""
    D$ = ""
    E$ = ""
    CT1 = 1
    ACUM = 0
    OPEN "I", #2, "B"
    WHILE EOF(2) <> -1
        INPUT #2, B$
        OPEN "I", #1, "A"
        WHILE EOF(1) <> -1
            INPUT #1, A$
            RP = (VAL(A$) * VAL(B$)) + ACUM
            C$ = LTRIM$(STR$(RP))
            IF EOF(1) <> -1 THEN D$ = D$ + RIGHT$(C$, 1)
            IF EOF(1) = -1 THEN D$ = D$ + F.INVCAD$(C$)
            E$ = LEFT$(C$, LEN(C$) - 1)
            ACUM = VAL(E$)
        WEND
        CLOSE (1)
        MID$(L$, LT - CT1 - LEN(D$) + 2) = F.INVCAD$(D$)
        OPEN "A", #3, "R"
        WRITE #3, L$
        CLOSE (3)
        L$ = STRING$(LT, " ")
        ACUM = 0
        C$ = ""
        D$ = ""
        E$ = ""
        CT1 = CT1 + 1
    WEND
    CLOSE (2)
END SUB

FUNCTION F.INVCAD$ (CAD$)
    LCAD = LEN(CAD$)
    CADTEM$ = ""
    FOR CAD = LCAD TO 1 STEP -1
        CADTEM$ = CADTEM$ + MID$(CAD$, CAD, 1)
    NEXT CAD
    F.INVCAD$ = CADTEM$
END FUNCTION

SUB G.SUMA
    CF = 0
    OPEN "I", #3, "R"
    WHILE EOF(3) <> -1
        INPUT #3, R$
        CF = CF + 1
        AN = LEN(R$)
    WEND
    CF = CF - 2
    CLOSE (3)
    W$ = ""
    ST = 0
    ACUS = 0
    FOR P = 1 TO AN
        K = 0
        OPEN "I", #3, "R"
        WHILE EOF(3) <> -1
            INPUT #3, R$
            K = K + 1
            IF K > 2 THEN ST = ST + VAL(MID$(R$, AN - P + 1, 1))
            IF K > 2 THEN M$ = LTRIM$(STR$(ST + ACUS))
        WEND
        'COLOR 10: LOCATE CF + 3, AN - P + 1: PRINT RIGHT$(M$, 1); : COLOR 7
        W$ = W$ + RIGHT$(M$, 1)
        ACUS = VAL(LEFT$(M$, LEN(M$) - 1))
        CLOSE (3)
        ST = 0
    NEXT P

    OPEN "A", #3, "R"
    WRITE #3, " " + RIGHT$(F.INVCAD(W$), AN - 1)
    CLOSE (3)
    CLS
    PRINT "THE SOLUTION IN THE FILE: R"
END SUB
```



### Version 2

<!-- I'm not sure what the difference is; don't feel like reading through them, I was just making sure they work and bughunting. -->

```qbasic
'PROGRAM: BIG MULTIPLICATION VER # 2
'LRCVS 01/01/2010
'THIS PROGRAM SIMPLY MAKES A BIG MULTIPLICATION
'WITHOUT THE PARTIAL PRODUCTS.
'HERE SEE ONLY THE SOLUTION.
'...............................................................
CLS
PRINT "WAIT"

NA = 2000 'NUMBER OF ELEMENTS OF THE MULTIPLY.
NB = 2000  'NUMBER OF ELEMENTS OF THE MULTIPLIER.
'Solution = 4000 Exacts digits

'......................................................
OPEN "X" + ".MLT" FOR BINARY AS #1
CLOSE (1)
KILL "*.MLT"
'.....................................................
'CREATING THE MULTIPLY  >>> A
'CREATING THE MULTIPLIER >>> B
FOR N = 1 TO 2
IF N = 1 THEN F$ = "A" + ".MLT": NN = NA
IF N = 2 THEN F$ = "B" + ".MLT": NN = NB
    OPEN F$ FOR BINARY AS #1
        FOR N2 = 1 TO NN
            RANDOMIZE TIMER
            X$ = LTRIM$(STR$(INT(RND * 10)))
            SEEK #1, N2: PUT #1, N2, X$
        NEXT N2
    SEEK #1, N2
    CLOSE (1)
NEXT N
'.....................................................
OPEN "A" + ".MLT" FOR BINARY AS #1
FOR K = 0 TO 9
NUM$ = "": Z$ = "": ACU = 0: GG = NA
C$ = LTRIM$(STR$(K))
    OPEN C$ + ".MLT" FOR BINARY AS #2
        'OPEN "A" + ".MLT" FOR BINARY AS #1
            FOR N = 1 TO NA
                SEEK #1, GG: GET #1, GG, X$
                NUM$ = X$
                Z$ = LTRIM$(STR$(ACU + (VAL(X$) * VAL(C$))))
                L = LEN(Z$)
                ACU = 0
                IF L = 1 THEN NUM$ = Z$: PUT #2, N, NUM$
                IF L > 1 THEN ACU = VAL(LEFT$(Z$, LEN(Z$) - 1)): NUM$ = RIGHT$(Z$, 1): PUT #2, N, NUM$
                SEEK #2, N: PUT #2, N, NUM$
                GG = GG - 1
            NEXT N
        IF L > 1 THEN ACU = VAL(LEFT$(Z$, LEN(Z$) - 1)): NUM$ = LTRIM$(STR$(ACU)): XX$ = XX$ + NUM$: PUT #2, N, NUM$
        'CLOSE (1)
    CLOSE (2)
NEXT K
CLOSE (1)
'......................................................
ACU = 0
LT5 = 1
LT6 = LT5
OPEN "B" + ".MLT" FOR BINARY AS #1
    OPEN "D" + ".MLT" FOR BINARY AS #3
        FOR JB = NB TO 1 STEP -1
        SEEK #1, JB
        GET #1, JB, X$

            OPEN X$ + ".MLT" FOR BINARY AS #2: LF = LOF(2): CLOSE (2)

            OPEN X$ + ".MLT" FOR BINARY AS #2
                FOR KB = 1 TO LF
                    SEEK #2, KB
                    GET #2, , NUM$
                    SEEK #3, LT5
                    GET #3, LT5, PR$
                    T$ = ""
                    T$ = LTRIM$(STR$(ACU + VAL(NUM$) + VAL(PR$)))
                    PR$ = RIGHT$(T$, 1)
                    ACU = 0
                    IF LEN(T$) > 1 THEN ACU = VAL(LEFT$(T$, LEN(T$) - 1))
                    SEEK #3, LT5: PUT #3, LT5, PR$
                    LT5 = LT5 + 1
                NEXT KB
                IF ACU <> 0 THEN PR$ = LTRIM$(STR$(ACU)): PUT #3, LT5, PR$
            CLOSE (2)
        LT6 = LT6 + 1
        LT5 = LT6
        ACU = 0
        NEXT JB
    CLOSE (3)
CLOSE (1)
OPEN "D" + ".MLT" FOR BINARY AS #3: LD = LOF(3): CLOSE (3)
ER = 1
OPEN "D" + ".MLT" FOR BINARY AS #3
    OPEN "R" + ".MLT" FOR BINARY AS #4
        FOR N = LD TO 1 STEP -1
            SEEK #3, N: GET #3, N, PR$
            SEEK #4, ER: PUT #4, ER, PR$
            ER = ER + 1
        NEXT N
    CLOSE (4)
CLOSE (3)
KILL "D.MLT"
FOR N = 0 TO 9
    C$ = LTRIM$(STR$(N))
    KILL C$ + ".MLT"
NEXT N
PRINT "END"
PRINT "THE SOLUTION IN THE FILE: R.MLT"
```


==={{Header|Applesoft BASIC}}===

```ApplesoftBasic
 100 A$ = "18446744073709551616"
 110 B$ = A$
 120 GOSUB 400
 130 PRINT E$
 140 END

 400  REM MULTIPLY A$ * B$
 410 C$ = "":D$ = "0"
 420  FOR I =  LEN (B$) TO 1 STEP  - 1
 430 C = 0:B =  VAL ( MID$ (B$,I,1))
 440  FOR J =  LEN (A$) TO 1 STEP  - 1
 450 V = B *  VAL ( MID$ (A$,J,1)) + C
 460 C =  INT (V / 10):V = V - C * 10
 470 C$ =  STR$ (V) + C$
 480  NEXT J
 490  IF C THEN C$ =  STR$ (C) + C$
 510  GOSUB 600"ADD C$ + D$
 520 D$ = E$:C$ = "0":J =  LEN (B$) - I
 530  IF J THEN J = J - 1:C$ = C$ + "0": GOTO 530
 550  NEXT I
 560  RETURN

 600  REM ADD C$ + D$
 610 E =  LEN (D$):E$ = "":C = 0
 620  FOR J =  LEN (C$) TO 1 STEP  - 1
 630  IF E THEN D =  VAL ( MID$ (D$,E,1))
 640 V =  VAL ( MID$ (C$,J,1)) + D + C
 650 C = V > 9:V = V - 10 * C
 660 E$ =  STR$ (V) + E$
 670  IF E THEN E = E - 1:D = 0
 680  NEXT J
 700  IF E THEN V =  VAL ( MID$ (D$,E,1)) + C:C = V > 9:V = V - 10 * C:E$ =  STR$ (V) + E$:E = E - 1: GOTO 700
 720  RETURN
```



## Batch File

Based on the JavaScript iterative code.

```dos
::Long Multiplication Task from Rosetta Code
::Batch File Implementation

@echo off
call :longmul 18446744073709551616 18446744073709551616 answer
echo(%answer%
exit /b 0

rem The Hellish Procedure
rem Syntax: call :longmul <n1> <n2> <variable to store product>
:longmul
    setlocal enabledelayedexpansion

    rem Define variables
    set "num1=%1"
    set "num2=%2"
    set "limit1=-1"
    set "limit2=-1"
    set "length=0"
    set "prod="

    rem Reverse the digits of each factor
    for %%A in (1,2) do (
        for /l %%B in (0,1,9) do set "num%%A=!num%%A:%%B=%%B !"
        for %%C in (!num%%A!) do ( set /a limit%%A+=1 & set "rev%%A=%%C!rev%%A!" )
    )

    rem Do the multiplication
    for /l %%A in (0,1,%limit1%) do (
        for /l %%B in (0,1,%limit2%) do (
            set /a iter=%%A+%%B
            set /a iternext=iter+1
            set /a iternext2=iter+2

            set /a prev=digit!iter!
            set /a digit!iter!=!rev1:~%%A,1!*!rev2:~%%B,1!

            rem The next line updates the length of "digits"
            if !iternext! gtr !length! set length=!iternext!
            if !iter! lss !length! set /a digit!iter!+=prev

            set /a currdigit=digit!iter!
            if !currDigit! gtr 9 (
                set /a prev=digit!iternext!
                set /a digit!iternext!=currdigit/10
                set /a digit!iter!=currdigit%%10

                rem The next line updates the length of "digits"
                if !iternext2! gtr !length! set length=!iternext2!
                if !iternext! lss !length! set /a digit!iternext!+=prev
            )
        )
    )

    rem Finalize product reversing the digits
    for /l %%F in (0,1,%length%) do set "prod=!digit%%F!!prod!"
    endlocal & set "%3=%prod%"
goto :eof
```

{{Out}}

```txt
340282366920938463463374607431768211456
```


=={{Header|BBC BASIC}}==
{{works with|BBC BASIC for Windows}}
Library method:

```bbcbasic
      INSTALL @lib$+"BB4WMAPMLIB"
      MAPM_DllPath$ = @lib$+"BB4WMAPM.DLL"
      PROCMAPM_Init

      twoto64$ = "18446744073709551616"
      PRINT "2^64 * 2^64 = " ; FNMAPM_Multiply(twoto64$, twoto64$)
```

Explicit method:

```bbcbasic
      twoto64$ = "18446744073709551616"
      PRINT "2^64 * 2^64 = " ; FNlongmult(twoto64$, twoto64$)
      END

      DEF FNlongmult(num1$, num2$)
      LOCAL C%, I%, J%, S%, num1&(), num2&(), num3&()
      S% = LEN(num1$)+LEN(num2$)
      DIM num1&(S%), num2&(S%), num3&(S%)
      IF LEN(num1$) > LEN(num2$) SWAP num1$,num2$
      $$^num1&(1) = num1$
      num1&() AND= 15
      FOR I% = LEN(num1$) TO 1 STEP -1
        $$^num2&(I%) = num2$
        num2&() AND= 15
        num3&() += num2&() * num1&(I%)
        IF I% MOD 3 = 1 THEN
          C% = 0
          FOR J% = S%-1 TO I%-1 STEP -1
            C% += num3&(J%)
            num3&(J%) = C% MOD 10
            C% DIV= 10
          NEXT
        ENDIF
      NEXT I%
      num3&() += &30
      num3&(S%) = 0
      IF num3&(0) = &30 THEN = $$^num3&(1)
      = $$^num3&(0)
```


=={{Header|C}}==
Doing it as if by hand.

```c
#include <stdio.h>
#include <string.h>

/* c = a * b.  Caller is responsible for memory.
   c must not be the same as either a or b. */
void longmulti(const char *a, const char *b, char *c)
{
	int i = 0, j = 0, k = 0, n, carry;
	int la, lb;

	/* either is zero, return "0" */
	if (!strcmp(a, "0") || !strcmp(b, "0")) {
		c[0] = '0', c[1] = '\0';
		return;
	}

	/* see if either a or b is negative */
	if (a[0] == '-') { i = 1; k = !k; }
	if (b[0] == '-') { j = 1; k = !k; }

	/* if yes, prepend minus sign if needed and skip the sign */
	if (i || j) {
		if (k) c[0] = '-';
		longmulti(a + i, b + j, c + k);
		return;
	}

	la = strlen(a);
	lb = strlen(b);
	memset(c, '0', la + lb);
	c[la + lb] = '\0';

#	define I(a) (a - '0')
	for (i = la - 1; i >= 0; i--) {
		for (j = lb - 1, k = i + j + 1, carry = 0; j >= 0; j--, k--) {
			n = I(a[i]) * I(b[j]) + I(c[k]) + carry;
			carry = n / 10;
			c[k] = (n % 10) + '0';
		}
		c[k] += carry;
	}
#	undef I
	if (c[0] == '0') memmove(c, c + 1, la + lb);

	return;
}

int main()
{
	char c[1024];
	longmulti("-18446744073709551616", "-18446744073709551616", c);
	printf("%s\n", c);

	return 0;
}
```
output<lang>340282366920938463463374607431768211456
```



## C++


### Version 1


```cpp

#include <iostream>
#include <sstream>
//--------------------------------------------------------------------------------------------------
typedef long long bigInt;
//--------------------------------------------------------------------------------------------------
using namespace std;
//--------------------------------------------------------------------------------------------------
class number
{
public:
    number()                                { s = "0"; neg = false; }
    number( bigInt a )                      { set( a ); }
    number( string a )                      { set( a ); }
    void set( bigInt a )                    { neg = false; if( a < 0 ) { a = -a; neg = true; } ostringstream o; o << a; s = o.str(); clearStr(); }
    void set( string a )                    { neg = false; s = a; if( s.length() > 1 && s[0] == '-' ) { neg = true; } clearStr(); }
    number operator *  ( const number& b )  { return this->mul( b ); }
    number& operator *= ( const number& b ) { *this = *this * b; return *this; }
    number& operator = ( const number& b )  { s = b.s; return *this; }
    friend ostream& operator << ( ostream& out, const number& a ) { if( a.neg ) out << "-"; out << a.s; return out; }
    friend istream& operator >> ( istream& in, number& a ){ string b; in >> b; a.set( b ); return in; }

private:
    number mul( const number& b )
    {
	number a; bool neg = false;
	string r, bs = b.s; r.resize( 2 * max( b.s.length(), s.length() ), '0' );
	int xx, ss, rr, t, c, stp = 0;
	string::reverse_iterator xi = bs.rbegin(), si, ri;
	for( ; xi != bs.rend(); xi++ )
	{
	    c = 0; ri = r.rbegin() + stp;
	    for( si = s.rbegin(); si != s.rend(); si++ )
	    {
		xx = ( *xi ) - 48; ss = ( *si ) - 48; rr = ( *ri ) - 48;
		ss = ss * xx + rr + c; t = ss % 10; c = ( ss - t ) / 10;
		( *ri++ ) = t + 48;
	    }
	    if( c > 0 ) ( *ri ) = c + 48;
	    stp++;
	}
	trimLeft( r ); t = b.neg ? 1 : 0; t += neg ? 1 : 0;
	if( t & 1 ) a.s = "-" + r;
	else a.s = r;
	return a;
    }

    void trimLeft( string& r )
    {
	if( r.length() < 2 ) return;
	for( string::iterator x = r.begin(); x != ( r.end() - 1 ); )
	{
	    if( ( *x ) != '0' ) return;
	    x = r.erase( x );
	}
    }

    void clearStr()
    {
	for( string::iterator x = s.begin(); x != s.end(); )
	{
	    if( ( *x ) < '0' || ( *x ) > '9' ) x = s.erase( x );
	    else x++;
	}
    }
    string s;
    bool neg;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    number a, b;
    a.set( "18446744073709551616" ); b.set( "18446744073709551616" );
    cout << a * b << endl << endl;

    cout << "Factor 1 = "; cin >> a;
    cout << "Factor 2 = "; cin >> b;
    cout << "Product: = " << a * b << endl << endl;
    return system( "pause" );
}
//--------------------------------------------------------------------------------------------------

```

{{out}}

```txt
340282366920938463463374607431768211456

Factor 1 = 9876548974569852365985574874787454878778975948
Factor 2 = 8954564845421878741168741154541897945138974567
Product: = 88440198241770705041777453160463400993104404280916080859287340887463980926235972531076714516

```




### Version 2


```cpp

#include <iostream>
#include <vector>
using namespace std;

typedef unsigned long native_t;

struct ZPlus_	// unsigned int, represented as digits base 10
{
	vector<native_t> digits_;	// least significant first; value is sum(digits_[i] * 10^i)

	ZPlus_(native_t n) : digits_(1, n)
	{
		while(Sweep());
	}

	bool Sweep()	// clean up digits so they are in [0,9]
	{
		bool changed = false;
		int carry = 0;
		for (auto pd = digits_.begin(); pd != digits_.end(); ++pd)
		{
			*pd += carry;
			carry = *pd / 10;
			*pd -= 10 * carry;
			changed = changed || carry > 0;
		}
		if (carry)
			digits_.push_back(carry);
		return changed || carry > 9;
	}
};

ZPlus_ operator*(const ZPlus_& lhs, const ZPlus_& rhs)
{
	ZPlus_ retval(0);
	// hold enough space
	retval.digits_.resize(lhs.digits_.size() + rhs.digits_.size(), 0ul);
	// accumulate one-digit multiples
	for (size_t ir = 0; ir < rhs.digits_.size(); ++ir)
		for (size_t il = 0; il < lhs.digits_.size(); ++il)
			retval.digits_[ir + il] += rhs.digits_[ir] * lhs.digits_[il];
	// sweep clean and drop zeroes
	while(retval.Sweep());
	while (!retval.digits_.empty() && !retval.digits_.back())
		retval.digits_.pop_back();
	return retval;
}

ostream& operator<<(ostream& dst, const ZPlus_& n)
{
	for (auto pd = n.digits_.rbegin(); pd != n.digits_.rend(); ++pd)
		dst << *pd;
	return dst;
}

int main(int argc, char* argv[])
{
	int p2 = 1;
	ZPlus_ n(2ul);
	for (int ii = 0; ii < 7; ++ii)
	{
		p2 *= 2;
		n = n * n;
		cout << "2^" << p2 << " = " << n << "\n";
	}
	return 0;
}
```



```txt

2^2 = 4
2^4 = 16
2^8 = 256
2^16 = 65536
2^32 = 4294967296
2^64 = 18446744073709551616
2^128 = 340282366920938463463374607431768211456

```


=={{header|C sharp|C#}}==
{{incorrect|F#|The problem is to implement long multiplication, not to demonstrate bignum support.}}

<code>System.Numerics.BigInteger</code> was added with C# 4.
{{works with|C sharp|C#|4+}}

```csharp
using System;
using System.Numerics;

class Program {
    static void Main() {
        BigInteger pow2_64 = BigInteger.Pow(2, 64);
        BigInteger result = BigInteger.Multiply(pow2_64, pow2_64);
        Console.WriteLine(result);
    }
}
```


Output:

```txt

340282366920938463463374607431768211456

```



=={{Header|Ceylon}}==

```Ceylon
"run() is the main function of this module."

shared void run() {

    function multiply(String|Integer|Integer[] top, String|Integer|Integer[] bottom, Integer base = 10) {

        function fromString(String s) =>
                s
                .filter(not(','.equals))
                .map((char) => Integer.parse(char.string))
                .narrow<Integer>()
                .sequence()
                .reversed;

        function toString(Integer[] ints) =>
                ""
                .join(ints.interpose(',', 3))
                .reversed
                .trimLeading((char) => char in "0,");

        function fromInteger(Integer int) => fromString(int.string);

        function convertArg(String|Integer|Integer[] arg) =>
                switch(arg)
                case (is String) fromString(arg)
                case (is Integer) fromInteger(arg)
                case (is Integer[]) arg;

        value a = convertArg(top);
        value b = convertArg(bottom);

        value p = a.size;
        value q = b.size;
        value product = Array.ofSize(p + q, 0);

        for (bIndex->bDigit in b.indexed) {
            variable value carry = 0;
            for (aIndex->aDigit in a.indexed) {
                assert (exists prodDigit = product[aIndex + bIndex]);
                value temp =  prodDigit + carry + aDigit * bDigit;
                carry = temp / base;
                product[aIndex + bIndex] = temp % base;
            }
            assert (exists lastDigit = product[bIndex + p]);
            product[bIndex + p] = lastDigit + carry;
        }

        return toString(product.sequence());
    }

    value twoToThe64th = "18,446,744,073,709,551,616";
    value expectedResult = "340,282,366,920,938,463,463,374,607,431,768,211,456";
    value result = multiply(twoToThe64th, twoToThe64th);

    print("The expected result is ``expectedResult``");
    print("The actual result is   ``result``");
    print("Do they match? ``expectedResult == result then "Yes!" else "No!"``");
}
```


=={{Header|COBOL}}==

```COBOL

       identification division.
       program-id. long-mul.
       data division.
       replace ==ij-lim== by ==7== ==ir-lim== by ==14==.
       working-storage section.
       1 input-string pic x(26) value "18,446,744,073,709,551,616".
       1 a-table.
        2 a pic 999 occurs ij-lim.
       1 b-table.
        2 b pic 999 occurs ij-lim.
       1 ir-table value all "0".
        2 occurs ij-lim.
         3 ir pic 999 occurs ir-lim.
       1 s-table value all "0".
        2 s pic 999 occurs ir-lim.
       1 display.
        2 temp-result pic 9(6) value 0.
        2 carry pic 999 value 0.
        2 remain pic 999 value 0.
       1 binary.
        2 i pic 9(4) value 0.
        2 j pic 9(4) value 0.
        2 k pic 9(4) value 0.
       procedure division.
       begin.
           move 1 to j
           perform varying i from 1 by 1 until i > ij-lim
               unstring input-string delimited ","
                   into a (i) with pointer j
           end-perform
           move a-table to b-table
           perform intermediate-calc
           perform sum-ir
           perform display-result
       stop run
       .

       intermediate-calc.
           perform varying i from ij-lim by -1 until i < 1
               move 0 to carry
               perform varying j from ij-lim by -1 until j < 1
                   compute temp-result = a (i) * b (j) + carry
                   divide temp-result by 1000 giving carry
                       remainder remain
                   compute k = i + j
                   move remain to ir (i k)
               end-perform
               subtract 1 from k
               move carry to ir (i k)
           end-perform
           .

       sum-ir.
           move 0 to carry
           perform varying k from ir-lim by -1 until k < 1
               move carry to temp-result
               perform varying i from ij-lim by -1 until i < 1
                   compute temp-result = temp-result + ir (i k)
               end-perform
               divide temp-result by 1000 giving carry
                   remainder remain
               move remain to s (k)
           end-perform
           .

       display-result.
           display "   " input-string
           display " * " input-string
           display " = " with no advancing
           perform varying k from 1 by 1
           until k > ir-lim or s (k) not = 0
           end-perform
           if s (k) < 100
               move 1 to i
               inspect s (k) tallying i for leading "0"
               display s (k) (i:) "," with no advancing
               add 1 to k
           end-if
           perform varying k from k by 1 until k > ir-lim
               display s (k) with no advancing
               if k < ir-lim
                   display "," with no advancing
               end-if
           end-perform
           display space
           .

       end program long-mul.

```



```txt

   18,446,744,073,709,551,616
 * 18,446,744,073,709,551,616
 = 340,282,366,920,938,463,463,374,607,431,768,211,456

```



## CoffeeScript


```coffeescript

# This very limited BCD-based collection of functions
# allows for long multiplication.  It works for positive
# numbers only.  The assumed data structure is as follows:
# BcdInteger.from_integer(4321) == [1, 2, 3, 4]

BcdInteger =
  from_string: (s) ->
    arr = []
    for c in s
      arr.unshift parseInt(c)
    arr

  from_integer: (n) ->
    result = []
    while n > 0
      result.push n % 10
      n = Math.floor n / 10
    result

  to_string: (arr) ->
    s = ''
    for elem in arr
      s = elem.toString() + s
    s

  sum: (arr1, arr2) ->
    if arr1.length < arr2.length
      return BcdInteger.sum(arr2, arr1)
    carry = 0
    result= []
    for d1, pos in arr1
      d = d1 + (arr2[pos] || 0) + carry
      result.push d % 10
      carry = Math.floor d / 10
    if carry
      result.push 1
    result

  multiply_by_power_of_ten: (arr, power_of_ten) ->
    result = (0 for i in [0...power_of_ten])
    result.concat arr

  product_by_integer: (arr, n) ->
    result = []
    for digit, i in arr
      prod = BcdInteger.from_integer n * digit
      prod = BcdInteger.multiply_by_power_of_ten prod, i
      result = BcdInteger.sum result, prod
    result

  product: (arr1, arr2) ->
    result = []
    for digit, i in arr1
      prod = BcdInteger.product_by_integer arr2, digit
      prod = BcdInteger.multiply_by_power_of_ten prod, i
      result = BcdInteger.sum result, prod
    result

x = BcdInteger.from_integer 1
for i in [1..64]
  x = BcdInteger.product_by_integer x, 2
console.log BcdInteger.to_string x #   18446744073709551616
square = BcdInteger.product x, x
console.log BcdInteger.to_string square # 340282366920938463463374607431768211456

```




## Common Lisp


```lisp
(defun number->digits (number)
  (do ((digits '())) ((zerop number) digits)
    (multiple-value-bind (quotient remainder) (floor number 10)
      (setf number quotient)
      (push remainder digits))))

(defun digits->number (digits)
  (reduce #'(lambda (n d) (+ (* 10 n) d)) digits :initial-value 0))

(defun long-multiply (a b)
  (labels ((first-digit (list)
             "0 if list is empty, else first element of list."
             (if (endp list) 0
               (first list)))
           (long-add (digitses &optional (carry 0) (sum '()))
             "Do long addition on the list of lists of digits.  Each
              list of digits in digitses should begin with the least
              significant digit.  This is the opposite of the digit
              list returned by number->digits which places the most
              significant digit first.  The digits returned by
              long-add do have the most significant bit first."
             (if (every 'endp digitses)
               (nconc (number->digits carry) sum)
               (let ((column-sum (reduce '+ (mapcar #'first-digit digitses)
                                         :initial-value carry)))
                 (multiple-value-bind (carry column-digit)
                     (floor column-sum 10)
                   (long-add (mapcar 'rest digitses)
                             carry (list* column-digit sum)))))))
    ;; get the digits of a and b (least significant bit first), and
    ;; compute the zero padded rows. Then, add these rows (using
    ;; long-add) and convert the digits back to a number.
    (do ((a (nreverse (number->digits a)))
         (b (nreverse (number->digits b)))
         (prefix '() (list* 0 prefix))
         (rows '()))
        ((endp b) (digits->number (long-add rows)))
      (let* ((bi (pop b))
             (row (mapcar #'(lambda (ai) (* ai bi)) a)))
        (push (append prefix row) rows)))))
```


 > (long-multiply (expt 2 64) (expt 2 64))
 340282366920938463463374607431768211456


## D

Using the standard library:

```d
void main() {
    import std.stdio, std.bigint;

    writeln(2.BigInt ^^ 64 * 2.BigInt ^^ 64);
}
```

{{out}}

```txt
340282366920938463463374607431768211456
```

Long multiplication, same output:
{{trans|JavaScript}}

```d
import std.stdio, std.algorithm, std.range, std.ascii, std.string;

auto longMult(in string x1, in string x2) pure nothrow @safe {
    auto digits1 = x1.representation.retro.map!q{a - '0'};
    immutable digits2 = x2.representation.retro.map!q{a - '0'}.array;
    uint[] res;

    foreach (immutable i, immutable d1; digits1.enumerate) {
        foreach (immutable j, immutable d2; digits2) {
            immutable k = i + j;
            if (res.length <= k)
                res.length++;
            res[k] += d1 * d2;

            if (res[k] > 9) {
                if (res.length <= k + 1)
                    res.length++;
                res[k + 1] = res[k] / 10 + res[k + 1];
                res[k] -= res[k] / 10 * 10;
            }
        }
    }

    //return res.retro.map!digits;
    return res.retro.map!(d => digits[d]);
}

void main() {
    immutable two64 = "18446744073709551616";
    longMult(two64, two64).writeln;
}
```



## Dc

{{incorrect|Dc|Code does not explicitly implement long multiplication}}
Since Dc has arbitrary precision built-in, the task is no different than a normal multiplication:

```Dc
2 64^ 2 64^ *p
```

{{incorrect|Dc|A Dc solution might be: Represent bignums as numerical strings and implement arithmetic functions on them.}}


## EchoLisp

We implement long multiplication by multiplying polynomials, knowing that the number 1234 is the polynomial x^3 +2x^2 +3x +4 at x=10. As we assume no bigint library is present, long-mul operates on strings.

```lisp

(lib 'math) ;; for poly multiplication

;; convert string of decimal digits to polynomial
;; "1234" → x^3 +2x^2 +3x +4
;; least-significant digit first
(define (string->long N)
	(reverse (map string->number (string->list N))))

;; convert polynomial to string
(define (long->string N)
(if (pair? N)
   (string-append (number->string (first N)) (long->string (rest N)))  ""))

;; convert poly coefficients to base 10
(define (poly->10 P (carry 0))
(append
	(for/list ((coeff P))
		(set! coeff (+ carry coeff ))
		(set! carry (quotient coeff 10)) ;; new carry
		(modulo coeff 10))
	(if(zero? carry) null (list carry)))) ;; remove leading 0 if any

;; long multiplication
;; convert input - strings of decimal digits - to polynomials
;; perform poly multiplication in base 10
;; convert result to string of decimal digits

(define (long-mul A B )
 (long->string (reverse  (poly->10 (poly-mul (string->long A) (string->long B))))))

(define two-64 "18446744073709551616")
(long-mul two-64 two-64)
    → "340282366920938463463374607431768211456"

;; check it
(lib 'bigint)
Lib: bigint.lib loaded.
(expt 2 128)
   → 340282366920938463463374607431768211456



```



## Euphoria


```euphoria
constant base = 1000000000

function atom_to_long(atom a)
    sequence s
    s = {}
    while a>0 do
        s = append(s,remainder(a,base))
        a = floor(a/base)
    end while
    return s
end function

function long_mult(object a, object b)
    sequence c
    if atom(a) then
        a = atom_to_long(a)
    end if
    if atom(b) then
        b = atom_to_long(b)
    end if
    c = repeat(0,length(a)+length(b))
    for i = 1 to length(a) do
        c[i .. i+length(b)-1] += a[i]*b
    end for

    for i = 1 to length(c) do
        if c[i] > base then
            c[i+1] += floor(c[i]/base) -- carry
            c[i] = remainder(c[i],base)
        end if
    end for

    if c[$] = 0 then
        c = c[1..$-1]
    end if
    return c
end function


function long_to_str(sequence a)
    sequence s
    s = sprintf("%d",a[$])
    for i = length(a)-1 to 1 by -1 do
        s &= sprintf("%09d",a[i])
    end for
    return s
end function

sequence a, b, c

a = atom_to_long(power(2,32))
printf(1,"a is %s\n",{long_to_str(a)})

b = long_mult(a,a)
printf(1,"a*a is %s\n",{long_to_str(b)})

c = long_mult(b,b)
printf(1,"a*a*a*a is %s\n",{long_to_str(c)})
```


Output:

```txt
a is 4294967296
a*a is 18446744073709551616
a*a*a*a is 340282366920938463488374607424768211456
```


=={{header|F Sharp|F#}}==

{{incorrect|F#|The problem is to implement long multiplication, not to demonstrate bignum support.}}


```F#>
 let X = 2I ** 64 * 2I ** 64 ;;

val X : System.Numerics.BigInteger = 340282366920938463463374607431768211456

```



## Factor


```factor
USING: kernel math sequences ;

: longmult-seq ( xs ys -- zs )
[ * ] cartesian-map
dup length iota [ 0 <repetition> ] map
[ prepend ] 2map
[ ] [ [ 0 suffix ] dip [ + ] 2map ] map-reduce ;

: integer->digits ( x -- xs )  { } swap  [ dup 0 > ] [ 10 /mod swap [ prefix ] dip ] while  drop ;
: digits->integer ( xs -- x )  0 [ swap 10 * + ] reduce ;

: longmult ( x y -- z )  [ integer->digits ] bi@ longmult-seq digits->integer ;
```


```factor
( scratchpad ) 2 64 ^ dup longmult .
340282366920938463463374607431768211456
( scratchpad ) 2 64 ^ dup * .
340282366920938463463374607431768211456
```



## Fortran

{{works with|Fortran|95 and later}}

```fortran
module LongMoltiplication
  implicit none

  type longnum
     integer, dimension(:), pointer :: num
  end type longnum

  interface operator (*)
     module procedure longmolt_ll
  end interface

contains

  subroutine longmolt_s2l(istring, num)
    character(len=*), intent(in) :: istring
    type(longnum), intent(out) :: num

    integer :: i, l

    l = len(istring)

    allocate(num%num(l))

    forall(i=1:l) num%num(l-i+1) = iachar(istring(i:i)) - 48

  end subroutine longmolt_s2l

  ! this one performs the moltiplication
  function longmolt_ll(a, b) result(c)
    type(longnum) :: c
    type(longnum), intent(in) :: a, b

    integer, dimension(:,:), allocatable :: t
    integer :: ntlen, i, j

    ntlen = size(a%num) + size(b%num) + 1
    allocate(c%num(ntlen))
    c%num = 0

    allocate(t(size(b%num), ntlen))

    t = 0
    forall(i=1:size(b%num), j=1:size(a%num)) t(i, j+i-1) = b%num(i) * a%num(j)

    do j=2, ntlen
       forall(i=1:size(b%num)) t(i, j) = t(i, j) + t(i, j-1)/10
    end do

    forall(j=1:ntlen) c%num(j) = sum(mod(t(:,j), 10))

    do j=2, ntlen
       c%num(j) = c%num(j) + c%num(j-1)/10
    end do

    c%num = mod(c%num, 10)

    deallocate(t)
  end function longmolt_ll


  subroutine longmolt_print(num)
    type(longnum), intent(in) :: num

    integer :: i, j

    do j=size(num%num), 2, -1
       if ( num%num(j) /= 0 ) exit
    end do

    do i=j, 1, -1
       write(*,"(I1)", advance="no") num%num(i)
    end do
  end subroutine longmolt_print

end module LongMoltiplication
```



```fortran
program Test
  use LongMoltiplication

  type(longnum) :: a, b, r

  call longmolt_s2l("18446744073709551616", a)
  call longmolt_s2l("18446744073709551616", b)

  r = a * b
  call longmolt_print(r)
  write(*,*)

end program Test
```


## FreeBASIC


```freebasic
' version 08-01-2017
' compile with: fbc -s console

Const As UInteger base_ = 1000000000 ' base 1,000,000,000

Function multiply(a1 As String, b1 As String) As String

    Dim As String a = a1, b = b1

    Trim(a) : Trim(b) ' remove spaces
    If Len(a) = 0 Or Len(b) = 0 Then Return "0"

    If Len(a) + Len(b) > 10000 Then
        Print "number(s) are to big"
        Sleep 5000,1
        Return ""
    End If

    If Len(a) < Len(b) Then
        Swap a, b
    End If

    Dim As ULongInt product
    Dim As UInteger carry, i, m, shift
    Dim As UInteger la = Len(a), lb = Len(b)
    Dim As UInteger la9 = la \ 9 + IIf((la Mod 9) = 0, 0, 1)
    Dim As UInteger lb9 = lb \ 9 + IIf((lb Mod 9) = 0, 0, 1)
    Dim As UInteger arr_a(la9), answer((la9 + lb9) + 2)
    Dim As Integer last = la9

    ' make length a, b a multipy of 9
    a = Right((String(9, "0") + a), la9 * 9)
    b = Right((String(9, "0") + b), lb9 * 9)

    For i = 1 To la9
        arr_a(la9 - i +1) = Val(Mid(a, i * 9 -8, 9))
    Next

    Do
        carry = 0
        m = Val(Mid(b, lb9 * 9 -8, 9))
        For i = 1 To la9
            product = CULngInt(arr_a(i)) * m + answer(i + shift) + carry
            carry = product \ base_
            answer(i + shift) = product - carry * base_
        Next
        If carry <> 0 Then
            last = la9 + shift +1
            answer(last) = carry
        End If
        lb9 = lb9 -1
        shift = shift +1
    Loop Until lb9 = 0

    Dim As String tmp = Str(answer(last))
    last = last -1
    While last > 0
        tmp = tmp + Right(String(9,"0") + Str(answer(last)), 9)
        last = last -1
    Wend

    Return tmp

End Function

' ------=< MAIN >=------

Dim As String a = "2", b = "2", answer
Dim As UInteger i = 1, j

For j = 1 To 7
    answer = multiply(a, b)
    a = answer
    b = answer
    i = i + i
    Print using "2 ^ ### = "; i;
    Print answer
Next

Print
Print "-------------------------------------------------"
Print

a = "2" : b = "1" : answer = ""
For j = 1 To 128
    answer = multiply(a, b)
    b = answer
Next
Print "2 ^ 128 = "; answer

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
2 ^   2 = 4
2 ^   4 = 16
2 ^   8 = 256
2 ^  16 = 65536
2 ^  32 = 4294967296
2 ^  64 = 18446744073709551616
2 ^ 128 = 340282366920938463463374607431768211456

-------------------------------------------------

2 ^ 128 = 340282366920938463463374607431768211456
```



## Go


```go
// Long multiplication per WP article referenced by task description.
// That is, multiplicand is multiplied by single digits of multiplier
// to form intermediate results.  Intermediate results are accumulated
// for the product.  Used here is the abacus method mentioned by the
// article, of summing intermediate results as they are produced,
// rather than all at once at the end.
//
// Limitations:  Negative numbers not supported, superfluous leading zeros
// not generally removed.
package main

import "fmt"

// argument validation
func d(b byte) byte {
    if b < '0' || b > '9' {
        panic("digit 0-9 expected")
    }
    return b - '0'
}

// add two numbers as strings
func add(x, y string) string {
    if len(y) > len(x) {
        x, y = y, x
    }
    b := make([]byte, len(x)+1)
    var c byte
    for i := 1; i <= len(x); i++ {
        if i <= len(y) {
            c += d(y[len(y)-i])
        }
        s := d(x[len(x)-i]) + c
        c = s / 10
        b[len(b)-i] = (s % 10) + '0'
    }
    if c == 0 {
        return string(b[1:])
    }
    b[0] = c + '0'
    return string(b)
}

// multipy a number by a single digit
func mulDigit(x string, y byte) string {
    if y == '0' {
        return "0"
    }
    y = d(y)
    b := make([]byte, len(x)+1)
    var c byte
    for i := 1; i <= len(x); i++ {
        s := d(x[len(x)-i])*y + c
        c = s / 10
        b[len(b)-i] = (s % 10) + '0'
    }
    if c == 0 {
        return string(b[1:])
    }
    b[0] = c + '0'
    return string(b)
}

// multiply two numbers as strings
func mul(x, y string) string {
    result := mulDigit(x, y[len(y)-1])
    for i, zeros := 2, ""; i <= len(y); i++ {
        zeros += "0"
        result = add(result, mulDigit(x, y[len(y)-i])+zeros)
    }
    return result
}

// requested output
const n = "18446744073709551616"

func main() {
    fmt.Println(mul(n, n))
}
```

Output:

```txt

340282366920938463463374607431768211456

```



## Haskell


```haskell
import Data.List (transpose, inits)
import Data.Char (digitToInt)

longmult :: Integer -> Integer -> Integer
longmult x y = foldl1 ((+) . (10 *)) (polymul (digits x) (digits y))

polymul :: [Integer] -> [Integer] -> [Integer]
polymul xs ys =
  sum <$>
  transpose
    (zipWith
       (++)
       (inits $ repeat 0)
       ((\f x -> fmap $ flip fmap x . f) (*) xs ys))

digits :: Integer -> [Integer]
digits = fmap (fromIntegral . digitToInt) . show

main :: IO ()
main = print $ (2 ^ 64) `longmult` (2 ^ 64)
```

{{out}}

```txt
340282366920938463463374607431768211456
```


== {{header|Icon}} and {{header|Unicon}} ==
Large integers are native to Icon and Unicon. Neither libraries nor special programming is required.

```Icon
procedure main()
   write(2^64*2^64)
end
```

{{output}}
```txt
340282366920938463463374607431768211456
```



## J

'''Solution:'''

```j
   digits      =: ,.&.":
   polymult    =: +//.@(*/)
   buildDecimal=: 10x&#.

   longmult=: buildDecimal@polymult&digits
```

'''Example:'''

```j
   longmult~ 2x^64
340282366920938463463374607431768211456
```


'''Alternatives:'''

<code>longmult</code> could have been defined concisely:

```j
longmult=: 10x&#.@(+//.@(*/)&(,.&.":))
```

Or, of course, the task may be accomplished without the verb definitions:

```j
   10x&#.@(+//.@(*/)&(,.&.":))~2x^64
340282366920938463463374607431768211456
```

Or using the code <code>(+ 10x&*)/@|.</code> instead of <code>#.</code>:

```j
   (+ 10x&*)/@|.@(+//.@(*/)&(,.&.":))~2x^64
340282366920938463463374607431768211456
```

Or you could use the built-in language support for arbitrary precision multiplication:

```j
   (2x^64)*(2x^64)
340282366920938463463374607431768211456
```


'''Explaining the component verbs:'''
* <code>digits</code> translates a number to a corresponding list of digits;

```j
   ,.&.": 123
1 2 3
```

* <code>polymult</code> (multiplies polynomials): '''ref.''' [http://www.jsoftware.com/help/dictionary/samp23.htm]

```j
   1 2 3 (+//.@(*/)) 1 2 3
1 4 10 12 9
```

* <code>buildDecimal</code> (translates a list of decimal digits - possibly including "carry" - to the corresponding extended precision number):

```j
   (+ 10x&*)/|. 1 4 10 12 9
15129
```



## Java



### Decimal version


This version of the code keeps the data in base ten. By doing this, we can avoid converting the whole number to binary and we can keep things simple, but the runtime will be suboptimal.


```java
public class LongMult {

	private static byte[] stringToDigits(String num) {
		byte[] result = new byte[num.length()];
		for (int i = 0; i < num.length(); i++) {
			char c = num.charAt(i);
			if (c < '0' || c > '9') {
				throw new IllegalArgumentException("Invalid digit " + c
						+ " found at position " + i);
			}
			result[num.length() - 1 - i] = (byte) (c - '0');
		}
		return result;
	}

	public static String longMult(String num1, String num2) {
		byte[] left = stringToDigits(num1);
		byte[] right = stringToDigits(num2);
		byte[] result = new byte[left.length + right.length];
		for (int rightPos = 0; rightPos < right.length; rightPos++) {
			byte rightDigit = right[rightPos];
			byte temp = 0;
			for (int leftPos = 0; leftPos < left.length; leftPos++) {
				temp += result[leftPos + rightPos];
				temp += rightDigit * left[leftPos];
				result[leftPos + rightPos] = (byte) (temp % 10);
				temp /= 10;
			}
			int destPos = rightPos + left.length;
			while (temp != 0) {
				temp += result[destPos] & 0xFFFFFFFFL;
				result[destPos] = (byte) (temp % 10);
				temp /= 10;
				destPos++;
			}
		}
		StringBuilder stringResultBuilder = new StringBuilder(result.length);
		for (int i = result.length - 1; i >= 0; i--) {
			byte digit = result[i];
			if (digit != 0 || stringResultBuilder.length() > 0) {
				stringResultBuilder.append((char) (digit + '0'));
			}
		}
		return stringResultBuilder.toString();
	}

	public static void main(String[] args) {
		System.out.println(longMult("18446744073709551616",
				"18446744073709551616"));
	}
}

```



### Binary version


This version tries to be as efficient as possible, so it converts numbers into binary before doing any calculations. The complexity is higher because of the need to convert to and from base ten, which requires the implementation of some additional arithmetic operations beyond long multiplication itself.


```java
import java.util.Arrays;

public class LongMultBinary {

	/**
	 * A very basic arbitrary-precision integer class. It only handles
	 * non-negative numbers and doesn't implement any arithmetic not necessary
	 * for the task at hand.
	 */
	public static class MyLongNum implements Cloneable {

		/*
		 * The actual bits of the integer, with the least significant place
		 * first. The biggest native integer type of Java is the 64-bit long,
		 * but since we need to be able to store the result of two digits
		 * multiplied, we have to use the second biggest native type, the 32-bit
		 * int. All numeric types are signed in Java, but we don't want to waste
		 * the sign bit, so we need to take extra care while doing arithmetic to
		 * ensure unsigned semantics.
		 */
		private int[] digits;

		/*
		 * The number of digits actually used in the digits array. Since arrays
		 * cannot be resized in Java, we are better off remembering the logical
		 * size ourselves, instead of reallocating and copying every time we need to shrink.
		 */
		private int digitsUsed;

		@Override
		public MyLongNum clone() {
			try {
				MyLongNum clone = (MyLongNum) super.clone();
				clone.digits = clone.digits.clone();
				return clone;
			} catch (CloneNotSupportedException e) {
				throw new Error("Object.clone() threw exception", e);
			}
		}

		private void resize(int newLength) {
			if (digits.length < newLength) {
				digits = Arrays.copyOf(digits, newLength);
			}
		}

		private void adjustDigitsUsed() {
			while (digitsUsed > 0 && digits[digitsUsed - 1] == 0) {
				digitsUsed--;
			}
		}

		/**
		 * "Short" multiplication by one digit. Used to convert strings to long numbers.
		 */
		public void multiply(int multiplier) {
			if (multiplier < 0) {
				throw new IllegalArgumentException(
						"Signed arithmetic isn't supported");
			}
			resize(digitsUsed + 1);
			long temp = 0;
			for (int i = 0; i < digitsUsed; i++) {
				temp += (digits[i] & 0xFFFFFFFFL) * multiplier;
				digits[i] = (int) temp; // store the low 32 bits
				temp >>>= 32;
			}
			digits[digitsUsed] = (int) temp;
			digitsUsed++;
			adjustDigitsUsed();
		}

		/**
		 * "Short" addition (adding a one-digit number). Used to convert strings to long numbers.
		 */
		public void add(int addend) {
			if (addend < 0) {
				throw new IllegalArgumentException(
						"Signed arithmetic isn't supported");
			}
			long temp = addend;
			for (int i = 0; i < digitsUsed && temp != 0; i++) {
				temp += (digits[i] & 0xFFFFFFFFL);
				digits[i] = (int) temp; // store the low 32 bits
				temp >>>= 32;
			}
			if (temp != 0) {
				resize(digitsUsed + 1);
				digits[digitsUsed] = (int) temp;
				digitsUsed++;
			}
		}

		/**
		 * "Short" division (dividing by a one-digit number). Used to convert numbers to strings.
		 * @param divisor The digit to divide by.
		 * @return The remainder of the division.
		 */
		public int divide(int divisor) {
			if (divisor < 0) {
				throw new IllegalArgumentException(
						"Signed arithmetic isn't supported");
			}
			int remainder = 0;
			for (int i = digitsUsed - 1; i >= 0; i--) {
				long twoDigits = (((long) remainder << 32) | (digits[i] & 0xFFFFFFFFL));
				remainder = (int) (twoDigits % divisor);
				digits[i] = (int) (twoDigits / divisor);
			}
			adjustDigitsUsed();
			return remainder;
		}

		public MyLongNum(String value) {
			// each of our 32-bit digits can store at least 9 decimal digit's worth
			this.digits = new int[value.length() / 9 + 1];
			this.digitsUsed = 0;
			// To lower the number of bignum operations, handle nine digits at a time.
			for (int i = 0; i < value.length(); i+=9) {
				String chunk = value.substring(i, Math.min(i+9, value.length()));
				int multiplier = 1;
				int addend = 0;
				for (int j=0; j<chunk.length(); j++) {
					char c = chunk.charAt(j);
					if (c < '0' || c > '9') {
						throw new IllegalArgumentException("Invalid digit " + c
								+ " found in input");
					}
					multiplier *= 10;
					addend *= 10;
					addend += c - '0';
				}
				multiply(multiplier);
				add(addend);
			}
		}

		@Override
		public String toString() {
			if (digitsUsed == 0) {
				return "0";
			}
			MyLongNum dummy = this.clone();
			StringBuilder resultBuilder = new StringBuilder(digitsUsed * 9);
			while (dummy.digitsUsed > 0) {
				// To limit the number of bignum divisions, handle nine digits at a time.
				int decimalDigits = dummy.divide(1000000000);
				for (int i=0; i<9; i++) {
					resultBuilder.append((char) (decimalDigits % 10 + '0'));
					decimalDigits /= 10;
				}
			}
			// Trim any leading zeros we may have created.
			while (resultBuilder.charAt(resultBuilder.length()-1) == '0') {
				resultBuilder.deleteCharAt(resultBuilder.length()-1);
			}
			return resultBuilder.reverse().toString();
		}

		/**
		 * Long multiplication.
		 */
		public void multiply(MyLongNum multiplier) {
			MyLongNum left, right;
			// Make sure the shorter number is on the right-hand side to make things a bit more efficient.
			if (this.digitsUsed > multiplier.digitsUsed) {
				left = this;
				right = multiplier;
			} else {
				left = multiplier;
				right = this;
			}
			int[] newDigits = new int[left.digitsUsed + right.digitsUsed];
			for (int rightPos = 0; rightPos < right.digitsUsed; rightPos++) {
				long rightDigit = right.digits[rightPos] & 0xFFFFFFFFL;
				long temp = 0;
				for (int leftPos = 0; leftPos < left.digitsUsed; leftPos++) {
					temp += (newDigits[leftPos + rightPos] & 0xFFFFFFFFL);
					temp += rightDigit * (left.digits[leftPos] & 0xFFFFFFFFL);
					newDigits[leftPos + rightPos] = (int) temp;
					temp >>>= 32;
				}
				// Roll forward any carry we may have.
				int destPos = rightPos + digitsUsed;
				while (temp != 0) {
					temp += (newDigits[destPos] & 0xFFFFFFFFL);
					newDigits[destPos] = (int) temp;
					temp >>>= 32;
					destPos++;
				}
			}
			this.digits = newDigits;
			this.digitsUsed = newDigits.length;
			adjustDigitsUsed();
		}
	}

	public static void main(String[] args) {
		MyLongNum one = new MyLongNum("18446744073709551616");
		MyLongNum two = one.clone();
		one.multiply(two);
		System.out.println(one);
	}

}

```



## JavaScript



### Iterative


With integer expression inputs at this scale, JavaScript still gives a slightly lossy result, despite the subsequent digit by digit string concatenation approach.

The problem is that the JavaScript Math.pow expressions become lossy at around 2^54, and Math.pow(2, 64) evaluates to a rounded:

18446744073709552000  rather than the full 18446744073709551616

This means that to handle larger inputs, the multiplication function needs to have string parameters:


```javascript
function mult(strNum1,strNum2){

    var a1 = strNum1.split("").reverse();
    var a2 = strNum2.toString().split("").reverse();
    var aResult = new Array;

    for ( var iterNum1 = 0; iterNum1 < a1.length; iterNum1++ ) {
        for ( var iterNum2 = 0; iterNum2 < a2.length; iterNum2++ ) {
            var idxIter = iterNum1 + iterNum2;    // Get the current array position.
            aResult[idxIter] = a1[iterNum1] * a2[iterNum2] + ( idxIter >= aResult.length ? 0 : aResult[idxIter] );

            if ( aResult[idxIter] > 9 ) {    // Carrying
                aResult[idxIter + 1] = Math.floor( aResult[idxIter] / 10 ) + ( idxIter + 1 >= aResult.length ? 0 : aResult[idxIter + 1] );
                aResult[idxIter] %= 10;
            }
        }
    }
    return aResult.reverse().join("");
}


mult('18446744073709551616', '18446744073709551616')
```


{{Out}}

```txt
340282366920938463463374607431768211456
```


===Functional (ES 5)===

The function below accepts integer string or native integer arguments, but as JavaScript (unlike Haskell and Python, for example), lacks an arbitrary precision integer type, larger inputs to this function (beyond  the scale of c. 2^54) need to take the form of integer strings, to avoid rounding.

For the same reason, the output always takes the form of an arbitrary precision integer string, rather than a native integer data type. (See the '''largeIntegerString()''' helper function below)


```JavaScript
(function () {
    'use strict';

    // Javascript lacks an unbounded integer type
    // so this multiplication function takes and returns
    // long integer strings rather than any kind of native integer

    // longMult :: (String | Integer) -> (String | Integer) -> String
    function longMult(num1, num2) {
        return largeIntegerString(
            digitProducts(digits(num1), digits(num2))
        );
    }

    // digitProducts :: [Int] -> [Int] -> [Int]
    function digitProducts(xs, ys) {
        return multTable(xs, ys)
            .map(function (zs, i) {
                return Array.apply(null, Array(i))
                    .map(function () {
                        return 0;
                    })
                    .concat(zs);
            })
            .reduce(function (a, x) {
                if (a) {
                    var lng = a.length;

                    return x.map(function (y, i) {
                        return y + (i < lng ? a[i] : 0);
                    })

                } else return x;
            })
    }

    // largeIntegerString :: [Int] -> String
    function largeIntegerString(lstColumnValues) {
        var dctProduct = lstColumnValues
            .reduceRight(function (a, x) {
                var intSum = x + a.carried,
                    intDigit = intSum % 10;

                return {
                    digits: intDigit
                        .toString() + a.digits,
                    carried: (intSum - intDigit) / 10
                };
            }, {
                digits: '',
                carried: 0
            });

        return (dctProduct.carried > 0 ? (
            dctProduct.carried.toString()
        ) : '') + dctProduct.digits;
    }

    // multTables :: [Int] -> [Int] -> [[Int]]
    function multTable(xs, ys) {
        return ys.map(function (y) {
            return xs.map(function (x) {
                return x * y;
            })
        });
    }

    // digits :: (Integer | String) -> [Integer]
    function digits(n) {
        return (typeof n === 'string' ? n : n.toString())
            .split('')
            .map(function (x) {
                return parseInt(x, 10);
            });
    }

    // TEST showing that larged bounded integer inputs give only rounded results
    // whereas integer string inputs allow for full precision on this scale (2^128)

    return {
        fromIntegerStrings: longMult(
            '18446744073709551616',
            '18446744073709551616'
        ),
        fromBoundedIntegers: longMult(
            18446744073709551616,
            18446744073709551616
        )
    };
})();
```

{{Out}}

```txt
{"fromIntegerStrings":"340282366920938463463374607431768211456",
"fromBoundedIntegers":"340282366920938477630474056040704000000"}
```



## jq

{{Works with|jq|1.4}}
Since the task description mentions 2^64, the following includes "long_power(i)" for computing n^i.

```jq
# multiply two decimal strings, which may be signed (+ or -)
def long_multiply(num1; num2):

   def stripsign:
     .[0:1] as $a
     | if $a == "-" then [ -1, .[1:]]
     elif $a == "+" then [  1, .[1:]]
     else [1, .]
     end;

  def adjustsign(sign):
     if sign == 1 then . else "-" + . end;

  # mult/2 assumes neither argument has a sign
  def mult(num1;num2):
      (num1 | explode | map(.-48) | reverse) as $a1
    | (num2 | explode | map(.-48) | reverse) as $a2
    | reduce range(0; num1|length) as $i1
        ([];  # result
         reduce range(0; num2|length) as $i2 (.;
  	  ($i1 + $i2) as $ix
  	  | ( $a1[$i1] * $a2[$i2] +
                (if $ix >= length then 0
                 else .[$ix]
                 end) ) as $r
            | if $r > 9 # carrying
              then
                .[$ix + 1] = ($r / 10 | floor) +
                   (if $ix + 1 >= length then 0
                    else .[$ix + 1]
                    end)
                | .[$ix] = $r - ( $r / 10 | floor ) * 10
              else
                .[$ix] = $r
              end
         )
       )
    | reverse | map(.+48) | implode;

     (num1|stripsign) as $a1
   | (num2|stripsign) as $a2
   | if $a1[1] == "0" or  $a2[1] == "0" then "0"
     elif $a1[1] == "1" then $a2[1]|adjustsign( $a1[0] * $a2[0] )
     elif $a2[1] == "1" then $a1[1]|adjustsign( $a1[0] * $a2[0] )
     else mult($a1[1]; $a2[1]) | adjustsign( $a1[0] * $a2[0] )
     end;
```


```jq
# Emit (input)^i where input and i are non-negative decimal integers,
# represented as numbers and/or strings.
def long_power(i):
  def power(i):
    tostring as $self
    | (i|tostring) as $i
    | if   $i == "0" then "1"
      elif $i == "1" then $self
      elif $self == "0" then "0"
      else reduce range(1;i) as $_ ( $self; long_multiply(.; $self) )
      end;

  (i|tonumber) as $i
  | if $i < 4 then power($i)
    else ($i|sqrt|floor) as $j
    | ($i - $j*$j) as $k
    | long_multiply( power($j) | power($j) ; power($k) )
  end ;
```

'''Example''':

```jq
 2 | long_power(64) | long_multiply(.;.)
```

{{Out}}
 $ jq -n -f Long_multiplication.jq
 "340282366920938463463374607431768211456"


## Julia

{{works with|Julia|0.6}}
{{trans|Python}}

'''Module''':

```julia
module LongMultiplication

using Compat

function addwithcarry!(r, addend, addendpos)
    while true
        pad = max(0, addendpos - lastindex(r))
        append!(r, fill(0, pad))
        addendrst = addend + r[addendpos]
        addend, r[addendpos] = divrem(addendrst, 10)
        iszero(addend) && break
        addendpos += 1
    end
    return r
end

function longmult(mult1::AbstractVector{T}, mult2::AbstractVector{T}) where T <: Integer
    r = T[]
    for (offset1, digit1) in enumerate(mult1), (offset2, digit2) in zip(eachindex(mult2) + offset1 - 1, mult2)
        single_multrst = digits(digit1 * digit2)
        for (addoffset, rstdigit) in zip(eachindex(single_multrst) + offset2 - 1, single_multrst)
            addwithcarry!(r, rstdigit, addoffset)
        end
    end
    return r
end

function longmult(a::T, b::T)::T where T <: Integer
    mult1 = digits(a)
    mult2 = digits(b)
    r = longmult(mult1, mult2)
    return sum(d * T(10) ^ (e - 1) for (e, d) in enumerate(r))
end

function longmult(a::AbstractString, b::AbstractString)
    if !ismatch(r"^\d+", a) || !ismatch(r"^\d+", b)
        throw(ArgumentError("string must contain only digits"))
    end
    mult1 = reverse(collect(Char, a) .- '0')
    mult2 = reverse(collect(Char, b) .- '0')
    r = longmult(mult1, mult2)
    return reverse(join(r))
end

end  # module LongMultiplication
```


'''Main''':

```julia
@show LongMultiplication.longmult(big(2) ^ 64, big(2) ^ 64)
@show LongMultiplication.longmult("18446744073709551616", "18446744073709551616")
```


{{out}}

```txt
LongMultiplication.longmult(big(2) ^ 64, big(2) ^ 64) = 340282366920938463463374607431768211456
LongMultiplication.longmult("18446744073709551616", "18446744073709551616") = "340282366920938463463374607431768211456"
```



## Kotlin

{{trans|Java}}

```scala
fun String.toDigits() = mapIndexed { i, c ->
    if (!c.isDigit())
        throw IllegalArgumentException("Invalid digit $c found at position $i")
    c - '0'
}.reversed()

operator fun String.times(n: String): String {
    val left = toDigits()
    val right = n.toDigits()
    val result = IntArray(left.size + right.size)

    right.mapIndexed { rightPos, rightDigit ->
        var tmp = 0
        left.indices.forEach { leftPos ->
            tmp += result[leftPos + rightPos] + rightDigit * left[leftPos]
            result[leftPos + rightPos] = tmp % 10
            tmp /= 10
        }
        var destPos = rightPos + left.size
        while (tmp != 0) {
            tmp += (result[destPos].toLong() and 0xFFFFFFFFL).toInt()
            result[destPos] = tmp % 10
            tmp /= 10
            destPos++
        }
    }

    return result.foldRight(StringBuilder(result.size), { digit, sb ->
        if (digit != 0 || sb.length > 0) sb.append('0' + digit)
        sb
    }).toString()
}

fun main(args: Array<out String>) {
    println("18446744073709551616" * "18446744073709551616")
}
```



## Lambdatalk



```scheme

Natural positive numbers are defined as strings, for instance 123 -> "123".
{lambda talk} has a small set of primitives working on strings, [equal?, empty?, chars, charAt, substring]

1) helper functions

{def lastchar
 {lambda {:w}
  {charAt {- {chars :w} 1} :w}
}}
{def butlast
 {lambda {:w}
  {substring 0 {- {chars :w} 1} :w}
}}
{def zeros
 {lambda {:n}
  {if {< :n 1}
   then
   else 0{zeros {- :n 1}}
}}}

2) add function

{def add
 {def add.r
 {lambda {:a :b :c :d}
  {if {equal? :a #}
   then {if {equal? :d 1} then 1 else}{butlast :c}
   else {let { {:a :a} {:b :b} {:c :c}
               {:d {+ :d {lastchar :a} {lastchar :b} }} }
    {add.r {butlast :a} {butlast :b} {lastchar :d}:c
         {if {equal? {chars :d} 1} then 0 else 1}}
 }}}}
 {lambda {:a :b}
  {{lambda {:a :b :n}
    {add.r #{zeros {- :n {chars :a}}}:a
           #{zeros {- :n {chars :b}}}:b  # 0}
  } :a :b {max {chars :a} {chars :b}}}
}}

3) mul function

{def mul
 {def muln
  {lambda {:a :b :n}
   {if {< :n 1}
    then :b
    else {muln :a {add :a :b} {- :n 1}}
 }}}
 {def mul.r
  {lambda {:a :b :c :n}
   {if {equal? :b #}
    then :c
    else {mul.r :a {butlast :b}
         {add {muln :a 0 {lastchar :b}}{zeros :n} :c} {+ :n 1}}
 }}}
 {lambda {:a :b}
  {mul.r :a #:b 0 0}
}}

4) applying to the task

Due to JS numbers limits, we compute first 2^32 using the JS pow function, then 2^64 and 2^128 using the mul function.

2^32 = '{def p32 {pow 2 32}}          -> '{p32}  = 4294967296
2^64 = '{def p64 {mul {p32} {p32}}}   -> '{p64}  = 18446744073709551616
2^128 = '{def p128 {mul {p64} {p64}}} -> '{p128} = 340282366920938463463374607431768211456

This can be tested in http://lambdaway.free.fr/lambdaspeech/?view=numbers8

```



## Liberty BASIC



```lb

'[RC] long multiplication

'now, count 2^64
print "2^64"
a$="1"
for i = 1 to 64
    a$ = multByD$(a$, 2)
next
print a$
print "(check with native LB)"
print 2^64
print "(looks OK)"

'now let's do b$*a$ stuff
print
print "2^64*2^64"
print longMult$(a$, a$)
print "(check with native LB)"
print 2^64*2^64
print "(looks OK)"

end
'---------------------------------------
function longMult$(a$, b$)
    signA = 1
    if left$(a$,1) = "-" then a$ = mid$(a$,2): signA = -1
    signB = 1
    if left$(b$,1) = "-" then b$ = mid$(b$,2): signB = -1

    c$ = ""
    t$ = ""
    shift$ = ""
    for i = len(a$) to 1 step -1
        d = val(mid$(a$,i,1))
        t$ = multByD$(b$, d)
        c$ = addLong$(c$, t$+shift$)
        shift$ = shift$ +"0"
    'print d, t$, c$
    next
    if signA*signB<0 then c$ = "-" + c$
    'print c$
    longMult$ = c$
end function

function multByD$(a$, d)
'multiply a$ by digit d
c$ = ""
carry = 0
for i = len(a$) to 1 step -1
        a = val(mid$(a$,i,1))
        c = a*d+carry
        carry = int(c/10)
        c = c mod 10
        'print a, c
        c$ = str$(c)+c$
next
    if carry>0 then c$ = str$(carry)+c$
    'print c$
    multByD$ = c$
end function

function addLong$(a$, b$)
'add a$ + b$, for now only positive
    l = max(len(a$), len(b$))
    a$=pad$(a$,l)
    b$=pad$(b$,l)
    c$ = "" 'result
    carry = 0
    for i = l to 1 step -1
        a = val(mid$(a$,i,1))
        b = val(mid$(b$,i,1))
        c = a+b+carry
        carry = int(c/10)
        c = c mod 10
        'print a, b, c
        c$ = str$(c)+c$
    next
    if carry>0 then c$ = str$(carry)+c$
    'print c$
    addLong$ = c$
end function

function pad$(a$,n)  'pad from right with 0 to length n
     pad$ = a$
     while len(pad$)<n
        pad$ = "0"+pad$
     wend
end function



```



## Maple


```Maple

longmult := proc(a::integer,b::integer)
    local A,B,m,n,i,j;
    # Note, return a*b; works in Maple for any sized integer
    A := convert(a,base,10);
    B := convert(b,base,10);
    m := numelems(A);
    n := numelems(B);
    add( add( A[i]*B[j]*10^(j-1), j=1..n )*10^(i-1), i=1..m );
end;

> longmult( 2^64, 2^64 );
                    340282366920938463463374607431768211456

```



## Mathematica

We define the long multiplication function:

```Mathematica
 LongMultiplication[a_,b_]:=Module[{d1,d2},
  d1=IntegerDigits[a]//Reverse;
  d2=IntegerDigits[b]//Reverse;
  Sum[d1[[i]]d2[[j]]*10^(i+j-2),{i,1,Length[d1]},{j,1,Length[d2]}]
 ]
```


Example:

```Mathematica
 n1 = 2^64;
 n2 = 2^64;
 LongMultiplication[n1, n2]
```


gives back:

```Mathematica> 340282366920938463463374607431768211456</lang


To check the speed difference between built-in multiplication (which is already arbitrary precision) we multiply two big numbers (2^8000 has '''2409''' digits!) and divide their timings:

```Mathematica
 n1=2^8000;
 n2=2^8000;
 Timing[LongMultiplication[n1,n2]][[1]]
 Timing[n1 n2][[1]]
 Floor[%%/%]
```


gives back:

```Mathematica
 72.9686
 7.*10^-6
 10424088
```


So our custom function takes about 73 second, the built-in function a couple of millionths of a second, so the long multiplication is about 10.5 million times slower! Mathematica uses Karatsuba multiplication for large integers, which is several magnitudes faster for really big numbers. Making it able to multiply <math>3^{(10^7)}\times3^{(10^7)}</math> in about a second; the final result has 9542426 digits; result omitted for obvious reasons.


## NetRexx

{{trans|REXX}}
A reworking of the example at Rexx Version 2.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

numeric digits 100

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method multiply(multiplier, multiplicand) public static
  result = ''
  mpa = s2a(multiplier)
  mpb = s2a(multiplicand)
  r_ = 0
  rim = 1
  loop bi = 1 to mpb[0]
    loop ai = 1 to mpa[0]
      ri = ai + bi -1
      p_ = mpa[ai] * mpb[bi]
      loop i_ = ri by 1 until p_ = 0
        s_ = r_[i_] + p_
        r_[i_] = s_ // 10
        p_ = s_ % 10
        end i_
      rim = rim.max(i_)
      end ai
    end bi
  r_[0] = rim
  result = a2s(r_)
  result = result.strip('l', 0)
  if result = '' then result = 0
  return result

-- .............................................................................
-- copy characters of a numeric string into a corresponding array
-- digits are numbered 1 to n from right to left
method s2a(numbr) private static
  result = 0
  lstr = numbr.length()
  loop z_ = 1 to lstr
    result[z_] = numbr.substr(lstr - z_ + 1, 1)
    end z_
  result[0] = lstr
  return result

-- .............................................................................
-- turn the array of digits into a numeric string
method a2s(numbr) private static
  result = ''
  loop z_ = numbr[0] to 1 by -1
    result = result || numbr[z_]
    end z_
  return result

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  mms = [ -
                      123',  '123, -
                      012',  '12, -
             123456789012' , '44444444444, -
                  2 ** 64' , '2**64, -
                        0'                      ,0 ' -
  ]
  ok = 0
  errors = 0

  loop mm over mms
    parse mm multiplier . ',' multiplicand .
    builtIn = multiplier * multiplicand
    calculated = multiply(multiplier, multiplicand)
    say 'Calculate' multiplier + 0 'x' multiplicand + 0
    say 'Built in:' builtIn
    say 'Derived: ' calculated
    say
    if builtIn = calculated then ok = ok + 1
    else                         errors = errors + 1
    end mm
  say ok 'ok'
  say errors 'not ok'

  return

```

{{out}}

```txt

Calculate 123 x 123
Built in: 15129
Derived:  15129

Calculate 12 x 12
Built in: 144
Derived:  144

Calculate 123456789012 x 44444444444
Built in: 5486968400478463649328
Derived:  5486968400478463649328

Calculate 18446744073709551616 x 18446744073709551616
Built in: 340282366920938463463374607431768211456
Derived:  340282366920938463463374607431768211456

Calculate 0 x 0
Built in: 0
Derived:  0

5 ok
0 not ok

```



## Nim


{{trans|C}}

```nim
import strutils

proc ti(a): int = ord(a) - ord('0')

proc longmulti(a, b: string): string =
  var
    i, j, n, carry, la, lb = 0
    k = false

  # either is zero, return "0"
  if a == "0" or b == "0":
    return "0"

  # see if either a or b is negative
  if a[0] == '-':
    i = 1; k = not k
  if b[0] == '-':
    j = 1; k = not k

  # if yes, prepend minus sign if needed and skip the sign
  if i > 0 or j > 0:
    result = if k: "-" else: ""
    result.add longmulti(a[i..a.high], b[j..b.high])
    return

  result = repeatChar(a.len + b.len, '0')

  for i in countdown(a.high, 0):
    var carry = 0
    var k = i + b.len
    for j in countdown(b.high, 0):
      let n = ti(a[i]) * ti(b[j]) + ti(result[k]) + carry
      carry = n div 10
      result[k] = chr(n mod 10 + ord('0'))
      dec k
    result[k] = chr(ord(result[k]) + carry)

  if result[0] == '0':
    result[0..result.high-1] = result[1..result.high]

echo longmulti("-18446744073709551616", "-18446744073709551616")
```

Output:

```txt
3402823669209384634633746074317682114566
```



## Oforth


Oforth handles arbitrary precision integers, so there is no need to
implement long multiplication :

{{out}}

```txt

2 64 pow dup * println
340282366920938463463374607431768211456

```


But, if long multiplication was to be implemented :

A natural is implemented as a list of integers with base 1000000000
(in order to print them easier)

Just multiplication is implemented here.


```Oforth
Number Class new: Natural(v)

Natural method: initialize  := v ;
Natural method: _v  @v ;

Natural classMethod: newValues super new ;
Natural classMethod: newFrom   asList self newValues ;

Natural method: *(n)
| v i j l x k |
   n _v ->v
   ListBuffer initValue(@v size v size + 1+, 0) ->l

   v size loop: i [
      i v at dup ->x 0 ifEq: [ continue ]
      0 @v size loop: j [
         i j + 1- ->k
         j @v at x * + l at(k) + 1000000000 /mod k rot l put
         ]
      k 1+ swap l put
      ]
   while(l last 0 == l size 0 <> and) [ l removeLast drop ]
   l dup freeze Natural newValues ;

Natural method: <<
| i |
   @v last <<
   @v size 1 - loop: i [ @v at(@v size i -) <<wjp(0, JUSTIFY_RIGHT, 8) ] ;
```


{{out}}

```txt

>Natural newFrom(2 16 pow) .s
[1] (Natural) 65536
ok
>dup * .s
[1] (Natural) 4294967296
ok
>dup * .s
[1] (Natural) 18446744073709551616
ok
>dup * .s
[1] (Natural) 340282366920938463463374607431768211456
ok
>_v .s
[1] (List) [768211456, 374607431, 938463463, 282366920, 340]
ok

```



## PARI/GP


```parigp
long(a,b)={
  a=eval(Vec(a));
  b=eval(Vec(b));
  my(c=vector(#a+#b),carry=0);
  for(i=1,#a,
    for(j=1,#b,
      c[i+j]+=a[i]*b[j]
    )
  );
  forstep(i=#c,1,-1,
    c[i] += carry;
    carry = c[i] \ 10;
    c[i] = c[i] % 10
  );
  for(i=1,#c,
    if(c[i], return(concat(apply(s->Str(s),vector(#c+1-i,j,c[i+j-1])))))
  );
  "0"
};
long("18446744073709551616","18446744073709551616")
```

Output:

```txt
%1 = "340282366920938463463374607431768211456"
```



## Pascal

Extracted from a programme to calculate and factor the number (two versions) in Frederick Pohl's book ''The Gold at the Starbow's End'', and compute Godel encodings of text. Compiles with the Free Pascal Compiler. The original would compile with Turbo Pascal (and used pointers to allow access to the "heap" storage scheme) except that does not allow functions to return a "big number" data aggregate, and it is so much nicer to be able to write X:=BigMult(A,B); The original has a special "square" calculation but this task is to exhibit long multiplication. However, raising to a power by iteration is painful, so a special routine for that.

```Pascal

Program TwoUp; Uses DOS, crt;
{Concocted by R.N.McLean (whom God preserve), Victoria university, NZ.}
 Procedure Croak(gasp: string);
  Begin
   Writeln;
   Write(Gasp);
   HALT;
  End;

 const BigBase = 10;		{The base of big arithmetic.}
 const BigEnuff = 333;		{The most storage possible is 65532 bytes with Turbo Pascal.}
 type  BigNumberIndexer = word;	{To access 0:BigEnuff BigNumberDigit data.}
 type  BigNumberDigit = byte;	{The data.}
 type  BigNumberDigit2 = word;	{Capable of digit*digit + carry. Like, 255*255 = 65025}

 type BigNumber =		{All sorts of arrangements are possible.}
  Record				{Could include a sign indication.}
   TopDigit: BigNumberDigit;		{Finger the high-order digit.}
   digit: array[0..BigEnuff] of byte;	{The digits: note the "downto" in BigShow.}
  end;					{Could add fractional digits too. Endless, endless.}

 Procedure BigShow(var a: BigNumber);	{Print the number.}
  var i: integer;	{A stepper.}
  Begin
   for i:=a.TopDigit downto 0 do	{Thus high-order to low, as is the custom.}
    if BigBase = 10 then write(a.digit[i])	{Constant following by the Turbo Pascal compiler}
     else if BigBase = 100 then Write(a.digit[i] div 10,a.digit[i] mod 10)	{Means that there will be no tests.}
      else write(a.digit[i],',');		{And dead code will be omitted.}
  End;

 Procedure BigZero(var A: BigNumber); {A:=0;}
  Begin;
   A.TopDigit:=0;
   A.Digit[0]:=0;
  End;
 Procedure BigOne(var A: BigNumber);  {A:=1;}
  Begin;
   A.TopDigit:=0;
   A.Digit[0]:=1;
  End;
 Function BigInt(n: longint): BigNumber; {A:=N;}
  var l: BigNumberIndexer;
  Begin
   l:=0;
   if n < 0 then croak('Negative integers are not yet considered.');
   repeat		{At least one digit is to be placed.}
    if l > BigEnuff then Croak('BigInt overflowed!');	{Oh dear.}
    BigInt.Digit[l]:=N mod BigBase;	{The low-order digit.}
    n:=n div BigBase;			{Shift down a digit.}
    l:=l + 1;				{Count in anticipation.}
   until N = 0;			{Still some number left?}
   BigInt.TopDigit:=l - 1;	{Went one too far.}
  End;

 Function BigMult(a,b: BigNumber): BigNumber;	{x:=BigMult(a,b);}
{Suppose the digits of A are a5,a4,a3,a2,a1,a0...
 To multiply A and B.
                               a5   a4   a3   a2   a1   a0: six digits, d1
                                x   b4   b3   b2   b1   b0: five digits, d2
                               ---------------------------
                             a5b0 a4b0 a3b0 a2b0 a1b0 a0b0
                        a5b1 a4b1 a3b1 a2b1 a1b1 a0b1
                   a5b2 a4b2 a3b2 a2b2 a1b2 a0b2
              a5b3 a4b3 a3b3 a2b3 a1b3 a0b3
         a5b4 a4b4 a3b4 a2b4 a1b4 a0b4
   -------------------------------------------------------
   carry    9    8    7    6    5    4    3    2    1    0: at least nine digits,
   -------------------------------------------------------  = d1 + d2 - 1
   But the indices are also the powers, so the highest power is 9 = 5 + 4,
and a possible tenth for any carry.}
  var X: BigNumber;		{Scratchpad, so b:=BigMult(a,b); doesn't overwrite b as it goes...}
  var d: BigNumberDigit;	{A digit.}
  var c: BigNumberDigit;	{A carry.}
  var dd: BigNumberDigit2;	{A digit product.}
  var i,j,l: BigNumberIndexer;	{Steppers.}
  Begin
   if ((A.TopDigit = 0) and (A.Digit[0] = 0))
    or((B.TopDigit = 0) and (B.Digit[0] = 0)) then begin BigZero(BigMult); exit; end;
   l:=A.TopDigit + B.TopDigit;       {Minimal digit requirement. (Counting is from zero)}
   if l > BigEnuff then Croak('BigMult will overflow.');
   for i:=l downto 0 do X.Digit[i]:=0;	{Clear for action.}
   for i:=0 to A.TopDigit do		{Arbitrarily, choose A on the one hand.}
    begin				{Though there could be a better choice.}
     d:=A.Digit[i];			{Select the digit.}
     if d <> 0 then			{What the hell. One in BigBase chance.}
      begin				{But not this time.}
       l:=i;				{Locate the power of BigBase.}
       c:=0;				{Start this digit's multiply pass.}
       for j:=0 to B.TopDigit do	{Stepping along B's digits.}
        begin				{One by one.}
         dd:=BigNumberDigit2(B.Digit[j])*d + X.Digit[l] + c;	{The deed.}
         X.Digit[l]:=dd mod BigBase;	{Place the new digit.}
         c:=dd div BigBase;		{And extract the carry.}
         l:=l + 1;			{Ready for the next power up.}
        end;				{Advance to it.}
       if c > 0 then			{The multiply done, place the carry.}
        begin				{Ah. We *will* use the next power up.}
         if l > BigEnuff then Croak('BigMultX has overflowed.');	{Oh dear.}
         X.Digit[l]:=c;		{Thus as if BigMult..Digit[l] was zeroed.}
         l:=l + 1;			{Preserve the one-too-far for the last case}
        end;				{So much for a carry at the end of a pass.}
      end;				{So much for a non-zero digit.}
    end;			{On to another digit to multiply with.}
   X.TopDigit:=l - 1;	{Remember the one-too-far.}
   BigMult:=X;		{Deliver, possibly scragging A or B, or, both!}
 End; {of BigMult.}

 Procedure BigPower(var X: BigNumber; P: longint); {Replaces X by X**P}
  var A,W: BigNumber;	{Scratchpads}
  label up;
  Begin		{Each squaring doubles the power, melding nicely with binary reduction.}
   if P <= 0 then Croak('Negative powers are not accommodated!');
   BigOne(A);		{x**0 = 1}
   W:=X;		{Holds X**1, 2, 4, 8, etc.}
up:if P mod 2 = 1 then A:=BigMult(A,W);	{Bit on, so include this order.}
   P:=P div 2;		{Halve the power contrariwise to W's doubling.}
   if P > 0 then 	{Still some power to come?}
    begin		{Yes.}
     W:=BigMult(W,W);	{Step up to the next bit's power.}
     goto up;		{And see if it is "on".}
    end;		{Odd layout avoids multiply testing P > 0.}
   X:=A;		{The result.}
  End;

 var X: BigNumber;
 var p: longint;
 BEGIN
  ClrScr;
  WriteLn('To calculate  x = 2**64, then x*x via multi-digit long multiplication.');
  p:=64;		{As per the specification.}
  X:=BigInt(2);		{Start with 2.}
  BigPower(X,p);	{First stage: 2**64}
  Write ('x = 2**',p,' = '); BigShow(X);
  WriteLn;
  X:=BigMult(X,X);	{Second stage.}
  Write ('x*x = ');BigShow(X);	{Can't have Write('x*x = ',BigShow(BigMult(X,X))), after all. Oh well.}
 END.

```


Output:
 To calculate  x = 2**64, then x*x via multi-digit long multiplication.
 x = 2**64 = 18446744073709551616
 x*x = 340282366920938463463374607431768211456


## Perl


```perl
#!/usr/bin/perl -w
use strict;

# This should probably be done in a loop rather than be recursive.
sub add_with_carry
{
  my $resultref = shift;
  my $addend = shift;
  my $addendpos = shift;

  push @$resultref, (0) while (scalar @$resultref < $addendpos + 1);
  my $addend_result = $addend + $resultref->[$addendpos];
  my @addend_digits = reverse split //, $addend_result;
  $resultref->[$addendpos] = shift @addend_digits;

  my $carry_digit = shift @addend_digits;
  &add_with_carry($resultref, $carry_digit, $addendpos + 1)
    if( defined $carry_digit )
}

sub longhand_multiplication
{
  my @multiplicand = reverse split //, shift;
  my @multiplier = reverse split //, shift;
  my @result = ();
  my $multiplicand_offset = 0;
  foreach my $multiplicand_digit (@multiplicand)
  {
    my $multiplier_offset = $multiplicand_offset;
    foreach my $multiplier_digit (@multiplier)
    {
      my $multiplication_result = $multiplicand_digit * $multiplier_digit;
      my @result_digit_addend_list = reverse split //, $multiplication_result;

      my $addend_offset = $multiplier_offset;
      foreach my $result_digit_addend (@result_digit_addend_list)
      {
        &add_with_carry(\@result, $result_digit_addend, $addend_offset++)
      }

      ++$multiplier_offset;
    }

    ++$multiplicand_offset;
  }

  @result = reverse @result;

  return join '', @result;
}

my $sixtyfour = "18446744073709551616";

my $onetwentyeight = &longhand_multiplication($sixtyfour, $sixtyfour);
print "$onetwentyeight\n";
```



## Perl 6

{{works with|rakudo|2015-09-17}}
For efficiency (and novelty), this program explicitly implements long multiplication, but in base 10000. That base was chosen because multiplying two 5-digit numbers can overflow a 32-bit integer, but two 4-digit numbers cannot.

```perl6
sub num_to_groups ( $num ) { $num.flip.comb(/.**1..4/)».flip     };
sub groups_to_num ( @g   ) { [~] flat @g.pop, @g.reverse».fmt('%04d') };

sub long_multiply ( Str $x, Str $y ) {
    my @group_sums;
    for flat num_to_groups($x).pairs X num_to_groups($y).pairs -> $xp, $yp {
        @group_sums[ $xp.key + $yp.key ] += $xp.value * $yp.value;
    }

    for @group_sums.keys -> $k {
        next if @group_sums[$k] < 10000;
        @group_sums[$k+1] += @group_sums[$k].Int div 10000;
        @group_sums[$k] %= 10000;
    }

    return groups_to_num @group_sums;
}

my $str = '18446744073709551616';
long_multiply( $str, $str ).say;

# cross-check with native implementation
say +$str * +$str;
```


{{out}}

```txt

340282366920938463463374607431768211456
340282366920938463463374607431768211456

```



## Phix

{{Trans|Euphoria}}
Simple longhand multiplication. To keep things as simple as possible, this does not handle negative numbers.

If bcd1 is a number split into digits 0..9, bcd9 is a number split into "digits" 000,000,000..999,999,999, which fit in an integer.

They are held lsb-style mainly so that trimming a trailing 0 does not alter their value.

```Phix
constant base = 1_000_000_000

function bcd9_mult(sequence a, sequence b)
sequence c
integer j
atom ci
    c = repeat(0,length(a)+length(b))
    for i=1 to length(a) do
        j = i+length(b)-1
        c[i..j] = sq_add(c[i..j],sq_mul(a[i],b))
    end for

    for i=1 to length(c) do
        ci = c[i]
        if ci>base then
            c[i+1] += floor(ci/base) -- carry
            c[i] = remainder(ci,base)
        end if
    end for

    if c[$]=0 then
        c = c[1..$-1]
    end if
    return c
end function

function atom_to_bcd9(atom a)
sequence s = {}
    while a>0 do
        s = append(s,remainder(a,base))
        a = floor(a/base)
    end while
    return s
end function

function bcd9_to_str(sequence a)
string s = sprintf("%d",a[$])
    for i=length(a)-1 to 1 by -1 do
        s &= sprintf("%09d",a[i])
    end for
    -- (might want to trim leading 0s here)
    return s
end function

sequence a, b, c

a = atom_to_bcd9(power(2,32))
printf(1,"a is %s\n",{bcd9_to_str(a)})

b = bcd9_mult(a,a)
printf(1,"a*a is %s\n",{bcd9_to_str(b)})

c = bcd9_mult(b,b)
printf(1,"a*a*a*a is %s\n",{bcd9_to_str(c)})
```

{{out}}

```txt

a is 4294967296
a*a is 18446744073709551616
a*a*a*a is 340282366920938463488374607488768211456

```



## PHP



```PHP
<?php
function longMult($a, $b)
{
  $as = (string) $a;
  $bs = (string) $b;
  for($pi = 0, $ai = strlen($as) - 1; $ai >= 0; $pi++, $ai--)
    {
      for($p = 0; $p < $pi; $p++)
        {
          $regi[$ai][] = 0;
        }
      for($bi = strlen($bs) - 1; $bi >= 0; $bi--)
        {
          $regi[$ai][] = $as[$ai] * $bs[$bi];
        }
    }
  return $regi;
}

function longAdd($arr)
{
  $outer = count($arr);
  $inner = count($arr[$outer-1]) + $outer;
  for($i = 0; $i <= $inner; $i++)
    {
      for($o = 0; $o < $outer; $o++)
        {
          $val  = isset($arr[$o][$i]) ? $arr[$o][$i] : 0;
          @$sum[$i] += $val;
        }
    }
  return $sum;
}

function carry($arr)
{
  for($i = 0; $i < count($arr); $i++)
    {
      $s = (string) $arr[$i];
      switch(strlen($s))
        {
          case 2:
            $arr[$i] = $s{1};
            @$arr[$i+1] += $s{0};
            break;
          case 3:
            $arr[$i] = $s{2};
            @$arr[$i+1] += $s{0}.$s{1};
            break;
        }
    }
  return ltrim(implode('',array_reverse($arr)),'0');
}

function lm($a,$b)
{
  return carry(longAdd(longMult($a,$b)));
}

if(lm('18446744073709551616','18446744073709551616') == '340282366920938463463374607431768211456')
  {
    echo 'pass!';
  }; // 2^64 * 2^64

</Lang>


## PicoLisp


```PicoLisp

(de multi (A B)
   (setq A (format A) B (reverse (chop B)))
   (let Result 0
      (for (I . X) B
         (setq Result (+ Result (* (format X) A (** 10 (dec I)))))) ) )

```



## PL/I


```PL/I
/* Multiply a by b, giving c. */
multiply: procedure (a, b, c);
   declare (a, b, c) (*) fixed decimal (1);
   declare (d, e, f) (hbound(a,1)) fixed decimal (1);
   declare pr (-hbound(a,1) : hbound(a,1)) fixed decimal (1);
   declare p fixed decimal (2), (carry, s) fixed decimal (1);
   declare neg bit (1) aligned;
   declare (i, j, n, offset) fixed binary (31);

   n = hbound(a,1);
   d = a;
   e = b;
   s = a(1) + b(1);
   neg = (s = 9);
   if a(1) = 9 then call complement (d);
   if b(1) = 9 then call complement (e);
   pr = 0;
   offset = 0; carry = 0;
   do i = n to 1 by -1;
      do j = n to 1 by -1;
         p = d(i) * e(j) + pr(j-offset) + carry;
         if p > 9 then do; carry = p/10; p = mod(p, 10); end; else carry = 0;
         pr(j-offset) = p;
      end;
      offset = offset + 1;
   end;
   do i = hbound(a,1) to 1 by -1;
      c(i) = pr(i);
   end;
   do i = -hbound(a,1) to 1;
      if pr(i) ^= 0 then signal fixedoverflow;
   end;
   if neg then call complement (c);
end multiply;

complement: procedure (a);
   declare a(*) fixed decimal (1);
   declare i fixed binary (31), carry fixed decimal (1);
   declare s fixed decimal (2);

   carry = 1;
   do i = hbound(a,1) to 1 by -1;
      s = 9 - a(i) + carry;
      if s > 9 then do; s = s - 10; carry = 1; end; else carry = 0;
      a(i) = s;
   end;
end complement;
```

Calling sequence:

```PL/I
   a = 0; b = 0; c = 0;
   a(60) = 1;
   do i = 1 to 64; /* Generate 2**64 */
      call add (a, a, b);
      put skip;
      call output (b);
      a = b;
   end;
   call multiply (a, b, c);
   put skip;
   call output (c);
```

Final output:

```txt

18446744073709551616
 340282366920938463463374607431768211456

```



## PowerShell


### Implementation


```PowerShell

# LongAddition only supports Unsigned Integers represented as Strings/Character Arrays
Function LongAddition ( [Char[]] $lhs, [Char[]] $rhs )
{
	$lhsl = $lhs.length
	$rhsl = $rhs.length
	if(($lhsl -gt 0) -and ($rhsl -gt 0))
	{
		$maxplace = [Math]::Max($rhsl,$lhsl)+1
		1..$maxplace | ForEach-Object {
			$carry = 0
			$result = ""
		} {
			$add1 = 0
			$add2 = 0
			if( $_ -le $lhsl ) { $add1 = [int]$lhs[ -$_ ] - 48 }
			if( $_ -le $rhsl ) { $add2 = [int]$rhs[ -$_ ] - 48 }
			$iresult = $add1 + $add2 + $carry
			if( ( $_ -lt $maxplace ) -or ( $iresult -gt 0 ) )
			{
				$result = "{0}{1}" -f ( $iresult % 10 ),$result
			}
			$carry = [Math]::Floor( $iresult / 10 )
		} {
			$result
		}
	} elseif($lhsl -gt 0) {
		[String]::Join( '', $lhs )
	} elseif($rhsl -gt 0) {
		[String]::Join( '', $rhs )
	} else {
		"0"
	}
}

# LongMultiplication only supports Unsigned Integers represented as Strings/Character Arrays
Function LongMultiplication ( [Char[]] $lhs, [Char[]] $rhs )
{
	$lhsl = $lhs.length
	$rhsl = $rhs.length
	if(($lhsl -gt 0) -and ($rhsl -gt 0))
	{
		1..$lhsl | ForEach-Object {
			$carry0 = ""
			$result0 = ""
		} {
			$i = -$_
			$add1 = ( 1..$rhsl | ForEach-Object {
				$carry1 = 0
				$result1 = ""
			} {
				$j = -$_
				$mult1 = [int]$lhs[ $i ] - 48
				$mult2 = [int]$rhs[ $j ] - 48
				$iresult1 = $mult1 * $mult2 + $carry1
				$result1 = "{0}{1}" -f ( $iresult1 % 10 ), $result1
				$carry1 = [Math]::Floor( $iresult1 / 10 )
			} {
				if( $carry1 -gt 0 )
				{
					$result1 = "{0}{1}" -f $carry1, $result1
				}
				$result1
			} )
			$iresult0 = ( LongAddition $add1 $carry0 )
			$iresultl = $iresult0.length
			$result0 = "{0}{1}" -f $iresult0[-1],$result0
			if( $iresultl -gt 1 ) {
				$carry0 = [String]::Join( '', $iresult0[ -$iresultl..-2 ] )
			} else { $carry0 = "" }
		} {
			if( $carry0 -ne "" )
			{
				$result0 = "{0}{1}" -f $carry0, $result0
			}
			$result0
		}
	} else { "0" }
}

LongMultiplication "18446744073709551616" "18446744073709551616"
```


### Library Method

{{works with|PowerShell|4.0}}

```PowerShell

[BigInt]$n = [Math]::Pow(2,64)
[BigInt]::Multiply($n,$n)

```

<b>Output:</b>

```txt

340282366920938463463374607431768211456

```



## Prolog

Arbitrary precision arithmetic is native in most Prolog implementations.

```Prolog
 ?- X is 2**64 * 2**64.
X = 340282366920938463463374607431768211456.
```



## PureBasic


### Explicit Implementation


```purebasic
Structure decDigitFmt ;decimal digit format
  Array Digit.b(0) ;contains each digit of number, right-most digit is index 0
  digitCount.i ;zero based
  sign.i ; {x < 0} = -1, {x = 0} = 0,  {x > 0} = 1
EndStructure

Global zero_decDigitFmt.decDigitFmt ;represents zero in the decimal digit format

;converts string representation of integer into the digit format, number can include signus but no imbedded spaces
Procedure stringToDecDigitFmt(numString.s, *x.decDigitFmt)
  Protected *c.Character, digitIdx, digitCount
  If numString And *x
    *c.Character = @numString
    Repeat
      Select *c\c
        Case '0' To '9', '-', '+'
          *c + SizeOf(Character)
        Default
          numString = Left(numString, *c - @numString)
          Break
      EndSelect
    ForEver
    *c = @numString
    Select  *c\c
      Case '-'
        *x\sign = -1
        *c + SizeOf(Character)
      Case '+'
        *x\sign = 1
        *c + SizeOf(Character)
      Case '0' To '9'
        *x\sign = 1
    EndSelect

    numString = LTrim(PeekS(*c), "0") ;remove leading zeroes
    If numString = "" ;is true if equal to zero or if only a signus is present
      CopyStructure(@zero_decDigitFmt, *x, decDigitFmt)
      ProcedureReturn
    EndIf
    *c = @numString

    digitCount = Len(PeekS(*c)) - 1
    Dim *x\Digit(digitCount)
    *x\digitCount = digitCount

    digitIdx = 0
    While *c\c
      If *c\c >= '0' And *c\c <= '9'
        *x\Digit(digitCount - digitIdx) = *c\c - '0'
        digitIdx + 1
        *c + SizeOf(Character)
      Else
        Break
      EndIf
    Wend

  EndIf
EndProcedure

;converts digit format representation of integer into string representation
Procedure.s decDigitFmtToString(*x.decDigitFmt)
  Protected i, number.s
  If *x
    If *x\sign = 0
      number = "0"
    Else

      For i = *x\digitCount To 0 Step -1
        number + Str(*x\Digit(i))
      Next
      number = LTrim(number, "0")
      If *x\sign = -1
        number = "-" + number
      EndIf
    EndIf
  EndIf

  ProcedureReturn number
EndProcedure

;handles only positive numbers and zero, negative numbers left as an exercise for the reader ;)
Procedure add_decDigitFmt(*a.decDigitFmt, *b.decDigitFmt, *sum.decDigitFmt, digitPos = 0) ;*sum contains the result of (*a ) * 10^digitPos + (*b)
  Protected carry, i, newDigitCount, workingSum, a_dup.decDigitFmt

  If *a And *b And *sum

    If *a = *sum: CopyStructure(*a, @a_dup, decDigitFmt): *a = @a_dup: EndIf ;handle special case of  *sum + *b = *sum
    If *b <> *sum: CopyStructure(*b, *sum, decDigitFmt): EndIf ;handle general case of *a + *b = *sum and special case of *a + *sum = *sum

    ;calculate number of digits needed for sum and resize array of digits if necessary
    newDigitCount = *a\digitCount + digitPos
    If newDigitCount >= *sum\digitCount
      If *sum\digitCount = newDigitCount And *sum\Digit(*sum\digitCount) <> 0
        newDigitCount + 1
      EndIf

      If *sum\digitCount <> newDigitCount
        *sum\digitCount = newDigitCount
        Redim *sum\Digit(*sum\digitCount)
      EndIf
    EndIf

    i = 0
    Repeat
      If i <= *a\digitCount
        workingSum = *a\Digit(i) + *sum\Digit(digitPos) + carry
      Else
        workingSum = *sum\Digit(digitPos) + carry
      EndIf

      If workingSum > 9
        carry = 1
        workingSum - 10
      Else
        carry = 0
      EndIf
      *sum\Digit(digitPos)  = workingSum
      digitPos + 1
      i + 1
    Until i > *a\digitCount And carry = 0

    If *a\sign <> 0 Or *sum\sign <> 0
      *sum\sign = 1 ;only handle positive numbers and zero for now
    EndIf
  EndIf
EndProcedure

Procedure multiply_decDigitFmt(*a.decDigitFmt, *b.decDigitFmt, *product.decDigitFmt) ;*product contains the result of (*a) * (*b)
  Protected i, digitPos, productSignus
  Protected Dim multTable.decDigitFmt(9)
  Protected NewList digitProduct.decDigitFmt()

  If *a And *b And *product
    If *a\sign = 0 Or *b\sign = 0
      CopyStructure(zero_decDigitFmt, *product, decDigitFmt)
      ProcedureReturn
    EndIf

    If *b\digitCount > *a\digitCount: Swap *a, *b: EndIf

    ;build multiplication table
    CopyStructure(*a, @multTable(1), decDigitFmt): multTable(1)\sign = 1 ;always positive
    For i = 2 To 9
      add_decDigitFmt(*a, multTable(i - 1), multTable(i))
    Next

    ;collect individual digit products for later summation; these could also be added as we go along
    For i = 0 To *b\digitCount
      AddElement(digitProduct())
      digitProduct() = multTable(*b\Digit(i))
    Next

    ;determine sign of product
    If *a\sign <> *b\sign
      productSignus = -1
    Else
      productSignus = 1
    EndIf

    digitPos = 0
    CopyStructure(zero_decDigitFmt, *product, decDigitFmt)
    ForEach digitProduct()
      add_decDigitFmt(digitProduct(), *product, *product, digitPos)
      digitPos + 1
    Next
    *product\sign = productSignus ;set sign of product
  EndIf
EndProcedure

;handles only positive integer exponents or an exponent of zero, does not raise an error for 0^0
Procedure exponent_decDigitFmt(*a.decDigitFmt, exponent, *product.decDigitFmt)
  Protected i, a_dup.decDigitFmt
  If *a And *product And exponent >= 0
    If *a = *product: CopyStructure(*a, @a_dup, decDigitFmt): *a = @a_dup: EndIf
    stringToDecDigitFmt("1", *product)
    For i = 1 To exponent: multiply_decDigitFmt(*product, *a, *product): Next
  EndIf
EndProcedure

If OpenConsole()
  Define a.decDigitFmt, product.decDigitFmt

  stringToDecDigitFmt("2", a)
  exponent_decDigitFmt(a, 64, a) ;2^64
  multiply_decDigitFmt(a, a, product)
  PrintN("The result of 2^64 * 2^64 is " + decDigitFmtToString(product))
  Print(#crlf$ + #crlf$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Output:

```txt
The result of 2^64 * 2^64 is 340282366920938463463374607431768211456
```



###  Library Method

{{works with|PureBasic|4.41}}

Using [http://www.purebasic.fr/english/viewtopic.php?p=309763#p309763 Decimal.pbi] by Stargåte allows for calculation with long numbers, this is useful since version 4.41 of PureBasic mostly only supporter data types native to x86/x64/PPC etc processors.

```PureBasic
XIncludeFile "decimal.pbi"

Define.Decimal *a, *b
*a=PowerDecimal(IntegerToDecimal(2),IntegerToDecimal(64))
*b=TimesDecimal(*a,*a,#NoDecimal)

Print("2^64*2^64 = "+DecimalToString(*b))
```


'''Outputs
 2^64*2^64 = 340282366920938463463374607431768211456


## Python

(Note that Python comes with arbitrary length integers).


```python
#!/usr/bin/env python
print 2**64*2**64
```


{{works with|Python|3.0}}
{{trans|Perl}}

```python
#!/usr/bin/env python

def add_with_carry(result, addend, addendpos):
    while True:
        while len(result) < addendpos + 1:
            result.append(0)
        addend_result = str(int(addend) + int(result[addendpos]))
        addend_digits = list(addend_result)
        result[addendpos] = addend_digits.pop()

        if not addend_digits:
            break
        addend = addend_digits.pop()
        addendpos += 1

def longhand_multiplication(multiplicand, multiplier):
    result = []
    for multiplicand_offset, multiplicand_digit in enumerate(reversed(multiplicand)):
        for multiplier_offset, multiplier_digit in enumerate(reversed(multiplier), start=multiplicand_offset):
            multiplication_result = str(int(multiplicand_digit) * int(multiplier_digit))

            for addend_offset, result_digit_addend in enumerate(reversed(multiplication_result), start=multiplier_offset):
                add_with_carry(result, result_digit_addend, addend_offset)

    result.reverse()

    return ''.join(result)

if __name__ == "__main__":
    sixtyfour = "18446744073709551616"

    onetwentyeight = longhand_multiplication(sixtyfour, sixtyfour)
    print(onetwentyeight)
```


Shorter version:
{{trans|Haskell}}
{{Works with|Python|3.7}}

```python
'''Long multiplication'''

from functools import reduce


def longmult(x, y):
    '''Long multiplication.'''
    return reduce(
        digitSum,
        polymul(digits(x), digits(y)), 0
    )


def digitSum(a, x):
    '''Left to right decimal digit summing.'''
    return a * 10 + x


def polymul(xs, ys):
    '''List of specific products.'''
    return map(
        lambda *vs: sum(filter(None, vs)),
        *[
            [0] * i + zs for i, zs in
            enumerate(mult_table(xs, ys))
        ]
    )


def mult_table(xs, ys):
    '''Rows of all products.'''
    return [[x * y for x in xs] for y in ys]


def digits(x):
    '''Digits of x as a list of integers.'''
    return [int(c) for c in str(x)]


if __name__ == '__main__':
    print(
        longmult(2 ** 64, 2 ** 64)
    )
```



## Ol

Ol already supports long numbers "out-of-the-box".


```scheme

(define x (* 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)) ; 2^64

(print (* x x))

```


```txt

340282366920938463463374607431768211456

```



## R


### Using GMP

{{libheader|gmp}}

```R
library(gmp)
a <- as.bigz("18446744073709551616")
mul.bigz(a,a)
```

 "340282366920938463463374607431768211456"

### A native implementation

This code is more verbose than necessary, for ease of understanding.

```R
longmult <- function(xstr, ystr)
{
   #get the number described in each string
   getnumeric <- function(xstr) as.numeric(unlist(strsplit(xstr, "")))

   x <- getnumeric(xstr)
   y <- getnumeric(ystr)

   #multiply each pair of digits together
   mat <- apply(x %o% y, 1, as.character)

   #loop over columns, then rows, adding zeroes to end of each number in the matrix to get the correct positioning
   ncols <- ncol(mat)
   cols <- seq_len(ncols)
   for(j in cols)
   {
      zeroes <- paste(rep("0", ncols-j), collapse="")
      mat[,j] <- paste(mat[,j], zeroes, sep="")
   }

   nrows <- nrow(mat)
   rows <- seq_len(nrows)
   for(i in rows)
   {
      zeroes <- paste(rep("0", nrows-i), collapse="")
      mat[i,] <- paste(mat[i,], zeroes, sep="")
   }

   #add zeroes to the start of the each number, so they are all the same length
   len <- max(nchar(mat))
   strcolumns <- formatC(cbind(as.vector(mat)), width=len)
   strcolumns <- gsub(" ", "0", strcolumns)

   #line up all the numbers below each other
   strmat <- matrix(unlist(strsplit(strcolumns, "")), byrow=TRUE, ncol=len)

   #convert to numeric and add them
   mat2 <- apply(strmat, 2, as.numeric)
   sum1 <- colSums(mat2)

   #repeat the process on each of the totals, until each total is a single digit
   repeat
   {
      ntotals <- length(sum1)
      totals <- seq_len(ntotals)
      for(i in totals)
      {
         zeroes <- paste(rep("0", ntotals-i), collapse="")
         sum1[i] <- paste(sum1[i], zeroes, sep="")
      }
      len2 <- max(nchar(sum1))
      strcolumns2 <- formatC(cbind(as.vector(sum1)), width=len2)
      strcolumns2 <- gsub(" ", "0", strcolumns2)
      strmat2 <- matrix(unlist(strsplit(strcolumns2, "")), byrow=TRUE, ncol=len2)
      mat3 <- apply(strmat2, 2, as.numeric)
      sum1 <- colSums(mat3)
      if(all(sum1 < 10)) break
   }

   #Concatenate the digits together
   ans <- paste(sum1, collapse="")
   ans
}

a <- "18446744073709551616"
longmult(a, a)
```


```txt

"340282366920938463463374607431768211456"

```



## Racket


```Racket

#lang racket

(define (mult A B)
  (define nums
    (let loop ([B B] [zeros '()])
      (if (null? B)
        '()
        (cons (append zeros (let loop ([c 0] [A A])
                              (cond [(pair? A)
                                     (define-values [q r]
                                       (quotient/remainder
                                        (+ c (* (car A) (car B)))
                                        10))
                                     (cons r (loop q (cdr A)))]
                                    [(zero? c) '()]
                                    [else (list c)])))
              (loop (cdr B) (cons 0 zeros))))))
  (let loop ([c 0] [nums nums])
    (if (null? nums)
      '()
      (let-values ([(q r) (quotient/remainder (apply + c (map car nums)) 10)])
        (cons r (loop q (filter pair? (map cdr nums))))))))

(define (number->list n)
  (if (zero? n) '()
      (let-values ([(q r) (quotient/remainder n 10)])
        (cons r (number->list q)))))

(define 2^64 (number->list (expt 2 64)))
(for-each display (reverse (mult 2^64 2^64))) (newline)
;; for comparison
(* (expt 2 64) (expt 2 64))

;; Output:
;; 340282366920938463463374607431768211456
;; 340282366920938463463374607431768211456

```



## REXX


### version 1

This REXX version supports:
::*   leading signs
::*   decimal points
::*   automatically adjusting the number of decimal digits needed

Programming note:   <big>&&</big>   is REXX's   '''exclusive or'''   operand.

```rexx
/*REXX program  performs  long multiplication  on  two numbers  (without the "E").      */
numeric digits 300                               /*be able to handle gihugeic input #s. */
parse arg x y .                                  /*obtain optional arguments from the CL*/
if x=='' | x==","  then x=2**64                  /*Not specified?  Then use the default.*/
if y=='' | y==","  then y=x                      /* "      "         "   "   "     "    */
if x<0  &&  y<0    then sign= '-'                /*there only a single negative number? */
                   else sign=                    /*no, then result sign must be positive*/
xx=x;    x=strip(x, 'T', .);      x1=left(x, 1)  /*remove any trailing decimal points.  */
yy=y;    y=strip(y, 'T', .);      y1=left(y, 1)  /*   "    "     "        "       "     */
if x1=='-' | x1=="+"  then x=substr(x, 2)        /*remove a leading  ±  sign.           */
if y1=='-' | y1=="+"  then y=substr(y, 2)        /*   "   "    "     "    "             */
parse var x '.' xf;   parse var y "." yf         /*obtain the fractional part of X and Y*/
#=length(xf || yf)                               /*#: digits past the decimal points (.)*/
x=space( translate( x, , .),  0)                 /*remove decimal point if there is any.*/
y=space( translate( y, , .),  0)                 /*   "       "     "    "    "   "  "  */
Lx=length(x);  Ly=length(y)                      /*get the lengths of the new  X and Y. */
numeric digits max(digits(), Lx + Ly)            /*use a new  decimal digits  precision.*/
$=0                                              /*$:  is the product  (so far).        */
                  do j=Ly  by -1  for Ly         /*almost like REXX does it, ··· but no.*/
                  $=$ + ((x*substr(y, j, 1))copies(0, Ly-j) )
                  end   /*j*/
f=length($) - #                                  /*does product has enough decimal digs?*/
if f<0  then $=copies(0, abs(f) + 1)$            /*Negative?  Add leading 0s for INSERT.*/
say 'long mult:'  xx  "*"  yy  '──►'   sign || strip( insert(., $, length($) - #), 'T', .)
say ' built─in:'  xx  "*"  yy  '──►'   xx*yy     /*stick a fork in it,  we're all done. */
```

'''output'''   when using the default inputs:

```txt

long mult: 18446744073709551616 * 18446744073709551616 ──► 340282366920938463463374607431768211456
 built─in: 18446744073709551616 * 18446744073709551616 ──► 340282366920938463463374607431768211456

```

'''output'''   when using the inputs of:   <tt> 123   -456789000 </tt>

```txt

long mult: 123 * -456789000 ──► -56185047000
 built─in: 123 * -456789000 ──► -56185047000

```

'''output'''   when using the inputs of:   <tt> -123.678   +456789000 </tt>

```txt

long mult: -123.678 * +456789000 ──► -56494749942.000
 built─in: -123.678 * +456789000 ──► -56494749942.000

```



### version 2


```rexx
/* REXX **************************************************************
* While REXX can multiply arbitrary large integers
* here is the algorithm asked for by the task description
* 13.05.2013 Walter Pachl
*********************************************************************/
cnt.=0
Numeric Digits 100
Call test 123 123
Call test 12 12
Call test 123456789012 44444444444
Call test 2**64 2**64
Call test 0 0
say cnt.0ok 'ok'
say cnt.0nok 'not ok'
Exit
test:
  Parse Arg a b
  soll=a*b
  haben=multiply(a b)
  Say 'soll =' soll
  Say 'haben=' haben
  If haben<>soll Then
    cnt.0nok=cnt.0nok+1
  Else
    cnt.0ok=cnt.0ok+1
  Return

multiply: Procedure
/* REXX **************************************************************
* Multiply(a b) -> a*b
*********************************************************************/
  Parse Arg a b
  Call s2a 'a'
  Call s2a 'b'
  r.=0
  rim=1
  r0=0
  Do bi=1 To b.0
    Do ai=1 To a.0
      ri=ai+bi-1
      p=a.ai*b.bi
      Do i=ri by 1 Until p=0
        s=r.i+p
        r.i=s//10
        p=s%10
        End
      rim=max(rim,i)
      End
    End
  res=strip(a2s('r'),'L','0')
  If res='' Then
    res='0'
  Return res

s2a:
/**********************************************************************
* copy characters of a string into a corresponding array
* digits are numbered 1 to n fron right to left
**********************************************************************/
  Parse arg name
  string=value(name)
  lstring=length(string)
  do z=1 to lstring
    Call value name'.'z,substr(string,lstring-z+1,1)
    End
  Call value name'.0',lstring
  Return

a2s:
/**********************************************************************
* turn the array of digits into a string
**********************************************************************/
  call trace 'o'
  Parse Arg name
  ol=''
  Do z=rim To 1 By -1
    ol=ol||value(name'.z')
    End
  Return ol
```

Output:

```txt
soll = 15129
haben= 15129
soll = 144
haben= 144
soll = 5486968400478463649328
haben= 5486968400478463649328
soll = 340282366920938463463374607431768211456
haben= 340282366920938463463374607431768211456
soll = 0
haben= 0
5 ok
0 not ok
```



## Ruby

{{trans|Tcl}}

```ruby
def longmult(x,y)
  result = [0]
  j = 0
  y.digits.each do |m|
    c = 0
    i = j
    x.digits.each do |d|
      v = result[i]
      result << 0 if v.zero?
      c, v = (v + c + d*m).divmod(10)
      result[i] = v
      i += 1
    end
    result[i] += c
    j += 1
  end
  # calculate the answer from the result array of digits
  result.reverse.inject(0) {|sum, n| 10*sum + n}
end

n=2**64
printf "         %d * %d = %d\n", n, n, n*n
printf "longmult(%d, %d) = %d\n", n, n, longmult(n,n)
```


```txt
         18446744073709551616 * 18446744073709551616 = 340282366920938463463374607431768211456
longmult(18446744073709551616, 18446744073709551616) = 340282366920938463463374607431768211456
```



## Scala

This implementation does not rely on an arbitrary precision numeric type. Instead, only single digits
are ever multiplied or added, and all partial results are kept as string.


```scala
def addNums(x: String, y: String) = {
  val padSize = x.length max y.length
  val paddedX = "0" * (padSize - x.length) + x
  val paddedY = "0" * (padSize - y.length) + y
  val (sum, carry) = (paddedX zip paddedY).foldRight(("", 0)) {
    case ((dx, dy), (acc, carry)) =>
      val sum = dx.asDigit + dy.asDigit + carry
      ((sum % 10).toString + acc, sum / 10)
  }
  if (carry != 0) carry.toString + sum else sum
}

def multByDigit(num: String, digit: Int) = {
  val (mult, carry) = num.foldRight(("", 0)) {
    case (d, (acc, carry)) =>
      val mult = d.asDigit * digit + carry
      ((mult % 10).toString + acc, mult / 10)
  }
  if (carry != 0) carry.toString + mult else mult
}

def mult(x: String, y: String) =
  y.foldLeft("")((acc, digit) => addNums(acc + "0", multByDigit(x, digit.asDigit)))
```


Sample:


```txt

scala> mult("18446744073709551616", "18446744073709551616")
res25: java.lang.String = 340282366920938463463374607431768211456

```


{{works with|Scala|2.8}}
Scala 2.8 introduces `scanLeft` and `scanRight` which can be used to simplify this further:


```scala
def adjustResult(result: IndexedSeq[Int]) = (
  result
  .map(_ % 10)        // remove carry from each digit
  .tail               // drop the seed carry
  .reverse            // put most significant digits on the left
  .dropWhile(_ == 0)  // remove leading zeroes
  .mkString
)

def addNums(x: String, y: String) = {
  val padSize = (x.length max y.length) + 1 // We want to keep a zero to the left, to catch the carry
  val paddedX = "0" * (padSize - x.length) + x
  val paddedY = "0" * (padSize - y.length) + y
  adjustResult((paddedX zip paddedY).scanRight(0) {
    case ((dx, dy), last) => dx.asDigit + dy.asDigit + last / 10
  })
}

def multByDigit(num: String, digit: Int) = adjustResult(("0"+num).scanRight(0)(_.asDigit * digit + _ / 10))

def mult(x: String, y: String) =
  y.foldLeft("")((acc, digit) => addNums(acc + "0", multByDigit(x, digit.asDigit)))

```



## Scheme

Since Scheme already supports arbitrary precision arithmetic, build it out of church numerals. Don't try converting these to native integers. You will die waiting for the answer.


```scheme
(define one (lambda (f) (lambda (x) (f x))))
(define (add a b) (lambda (f) (lambda (x) ((a f) ((b f) x)))))
(define (mult a b) (lambda (f) (lambda (x) ((a (b f)) x))))
(define (expo a b) (lambda (f) (lambda (x) (((b a) f) x))))
(define two (add one one))
(define six (add two (add two two)))
(define sixty-four (expo two six))
(display (mult (expo two sixty-four) (expo two sixty-four)))
```



## Seed7

Seed7 supports arbitrary-precision arithmetic.
The library [http://seed7.sourceforge.net/libraries/bigint.htm bigint.s7i] defines
the type [http://seed7.sourceforge.net/manual/types.htm#bigInteger bigInteger].
A bigInteger is a signed integer number of unlimited size.
With library support the task can be solved by using the multiplication operator
[http://seed7.sourceforge.net/libraries/bigint.htm#%28in_bigInteger%29*%28in_bigInteger%29 *]:


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";

const proc: main is func
  begin
    writeln(2_**64 * 2_**64);
  end func;
```


Output:

```txt

340282366920938463463374607431768211456

```


This task seems to prefer an inferior implementation of a long multiplication,
where long numbers are stored in decimal strings. Besides type safety there
are seveal other drawbacks triggered by such a representation.
E.g.: In almost all cases a representation with decimal strings leads to
significant lower computing speed.
The multiplication example below uses the requested inferior implementation:


```seed7
$ include "seed7_05.s7i";

const func string: (in string: a) * (in string: b) is func
  result
    var string: product is "";
  local
    var integer: i is 1;
    var integer: j is 1;
    var integer: k is 0;
    var integer: carry is 0;
  begin
    if startsWith(a, "-") then
      if startsWith(b, "-") then
        product := a[2 ..] * b[2 ..];
      else
        product := "-" & a[2 ..] * b;
      end if;
    elsif startsWith(b, "-") then
      product := "-" & a * b[2 ..];
    else
      product := "0" mult length(a) + length(b);
      for i range length(a) downto 1 do
        k := i + length(b);
        carry := 0;
        for j range length(b) downto 1 do
          carry +:= (ord(a[i]) - ord('0')) * (ord(b[j]) - ord('0')) + (ord(product[k]) - ord('0'));
          product @:= [k] chr(carry rem 10 + ord('0'));
          carry := carry div 10;
          decr(k);
        end for;
        product @:= [k] chr(ord(product[k]) + carry);
      end for;
      while startsWith(product, "0") and length(product) >= 2 do
        product := product[2 ..];
      end while;
    end if;
  end func;

const proc: main is func
  begin
    writeln("-18446744073709551616" * "-18446744073709551616");
  end func;
```


The output is the same as with the superior solution.


## Sidef

(Note that arbitrary precision arithmetic is native in Sidef).

```ruby
say (2**64 * 2**64);
```

{{trans|Python}}

```ruby
func add_with_carry(result, addend, addendpos) {
    loop {
        while (result.len < addendpos+1) {
            result.append(0)
        }
        var addend_digits = (addend.to_i + result[addendpos] -> to_s.chars)
        result[addendpos] = addend_digits.pop
        addend_digits.len > 0 || break
        addend = addend_digits.pop
        addendpos++
    }
}
 
func longhand_multiplication(multiplicand, multiplier) {
 
    var result = []
    var multiplicand_offset = 0
 
    multiplicand.reverse.each { |multiplicand_digit|
        var multiplier_offset = multiplicand_offset
        multiplier.reverse.each { |multiplier_digit|
            var multiplication_result = (multiplicand_digit.to_i * multiplier_digit.to_i -> to_s)
 
            var addend_offset = multiplier_offset
            multiplication_result.reverse.each { |result_digit_addend|
                add_with_carry(result, result_digit_addend, addend_offset)
                addend_offset++
            }
            multiplier_offset++
        }
        multiplicand_offset++
    }
 
    return result.join.reverse
}
 
say longhand_multiplication('18446744073709551616', '18446744073709551616')
```


{{out}}

```txt

340282366920938463463374607431768211456

```



## Slate

{{incorrect|Slate|Code does not explicitly implement long multiplication}}

```slate
(2 raisedTo: 64) * (2 raisedTo: 64).
```



## Smalltalk

(Note that arbitrary precision arithmetic is native in Smalltalk).

```smalltalk
(2 raisedTo: 64) * (2 raisedTo: 64).
```



## Tcl

{{works with|Tcl|8.5}}
Tcl 8.5 supports arbitrary-precision integers, which improves math operations on large integers. It is easy to define our own by following rules for long multiplication; we can then check this against the built-in's result:

```tcl
package require Tcl 8.5

proc longmult {x y} {
    set digits [lreverse [split $x ""]]
    set result {0}
    set j -2
    foreach m [lreverse [split $y ""]] {
	set c 0
	set i [incr j]
	foreach d $digits {
	    set v [lindex $result [incr i]]
	    if {$v eq ""} {
		lappend result 0
		set v 0
	    }
	    regexp (.)(.)$ 0[expr {$v + $c + $d*$m}] -> c v
	    lset result $i $v
	}
	lappend result $c
    }
    # Reconvert digit list into a decimal number
    set result [string trimleft [join [lreverse $result] ""] 0]
    if {$result == ""} then {return 0} else {return $result}
}

puts [set n [expr {2**64}]]
puts [longmult $n $n]
puts [expr {$n * $n}]
```

outputs

```txt
18446744073709551616
340282366920938463463374607431768211456
340282366920938463463374607431768211456
```



## UNIX Shell


In real shell scripts, I would use either `bc` or `dc` for this:


```sh
multiply() { echo "$1 $2 * p" | dc; }
```


But you can also do it with bash's built-in arithmetic:


```bash
add() { # arbitrary-precision addition
  local a="$1" b="$2" sum= carry=0
  if (( ${#a} < ${#b} )); then
    local t="$a"
    a="$b" b="$t"
  fi

  while (( ${#a} )); do
    local -i d1="${a##${a%?}}" d2="10#0${b##${b%?}}" s=carry+d1+d2
    sum="${s##${s%?}}$sum"
    carry="10#0${s%?}"
    a="${a%?}" b="${b%?}"
  done
  echo "$sum"
}

multiply() { # arbitrary-precision multiplication
  local a="$1" b="$2" product=0
  if (( ${#a} < ${#b} )); then
    local t="$a"
    a="$b" b="$t"
  fi

  local zeroes=
  while (( ${#b} )); do
    local m1="$a"
    local m2="${b##${b%?}}"
    local partial=$zeroes
    local -i carry=0
    while (( ${#m1} )); do
      local -i d="${m1##${m1%?}}"
      m1="${m1%?}"
      local -i p=d*m2+carry
      partial="${p##${p%?}}$partial"
      carry="10#0${p%?}"
    done
    partial="${carry#0}$partial"
    product="$(add "$product" "$partial")"
    zeroes=0$zeroes
    b="${b%?}"
  done
  echo "$product"
}
```


Output is the same either way:
```txt
$ multiply 18446744073709551616 18446744073709551616
340282366920928463463374607431768211456

```



## Ursala


Natural numbers of unlimited size are a built in type,
and arithmetic operations on them are available as library functions.
However, since the task calls for explicitly implementing long
multiplication, here is an implementation using nothing but language
primitives. The numbers are represented as lists of booleans,
LSB first. The compiler already knows how to parse and display
them in decimal.


```Ursala
successor = ~&a^?\1! ~&ah?/~&NfatPRC ~&NNXatPC

sum = ~&B^?a\~&Y@a ~&B?abh/successor@alh2fabt2RC ~&Yabh2Ofabt2RC

product = ~&alrB^& sum@NfalrtPXPRCarh2alPNQX

x = 18446744073709551616

#show+

y = %nP product@iiX x
```

output:

```txt
340282366920938463463374607431768211456
```



## Vedit macro language

This example multiplies the value on current line with the value on next line and stores result on the 3rd line.

```vedit
BOL
#11 = EOL_Pos-Cur_Pos
#12 = EOL_Pos-1
Line(1)
#21 = EOL_Pos-Cur_Pos
#22 = EOL_Pos-1
EOL Ins_Newline
Ins_Char('0', COUNT, #11+#21)
#32 = Cur_Pos-1

for (#2 = 0; #2 < #21; #2++) {
    Goto_Pos(#22-#2) #5 = Cur_Char - '0'
    for (#1 = 0; #1 < #11; #1++) {
        Goto_Pos(#12-#1) #6 = Cur_Char - '0'
	#7 = #5 * #6
	#3 = #1 + #2
	while (#7 > 0) {
	    Goto_Pos(#32-#3)
	    #7 += Cur_Char - '0'
	    Ins_Char(#7%10 + '0', OVERWRITE)
	    #3++
	    #7 = #7/10
        }
    }
}
```

Sample input and output:

```txt

18446744073709551616
18446744073709551616
0340282366920938463463374607431768211456

```




## XPL0


```XPL0
include c:\cxpl\stdlib;
char Two64, Product(40);
[Two64:= "18446744073709551616";
StrNMul(Two64, Two64, Product, 20);
Product(39):= Product(39)!$80;          \terminate string
Text(0, Product+1);                     \skip leading zero
]
```


Output:

```txt

340282366920938463463374607431768211456

```



## zkl

[gnu] BigNums are supported via an extension library

```zkl
var BN=Import("zklBigNum");
BN(2).pow(64) * BN(2).pow(64)
340282366920938463463374607431768211456

BN(2).pow(128) : "%,d".fmt(_)
340,282,366,920,938,463,463,374,607,431,768,211,456

   //42!, also BN(42).factorial()
[2..42].reduce(fcn(p,n){p*n},BN(1)) : "%,d".fmt(_)
1,405,006,117,752,879,898,543,142,606,244,511,569,936,384,000,000,000
```


{{omit from|Erlang|Erlang has this built in}}
