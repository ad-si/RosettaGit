+++
title = "Arithmetic/Rational/Ada"
description = ""
date = 2012-02-20T15:08:03Z
aliases = []
[extra]
id = 5287
[taxonomies]
categories = []
tags = []
+++

<noinclude>{{collection|Rational Arithmetic}}</noinclude>
The generic package specification:

```ada
generic
   type Number is range <>;
package Generic_Rational is
   type Rational is private;
   
   function "abs"   (A : Rational) return Rational;
   function "+"     (A : Rational) return Rational;
   function "-"     (A : Rational) return Rational;
   function Inverse (A : Rational) return Rational;
   
   function "+" (A : Rational; B : Rational) return Rational;
   function "+" (A : Rational; B : Number  ) return Rational;
   function "+" (A : Number;   B : Rational) return Rational;

   function "-" (A : Rational; B : Rational) return Rational;
   function "-" (A : Rational; B : Number  ) return Rational;
   function "-" (A : Number;   B : Rational) return Rational;

   function "*" (A : Rational; B : Rational) return Rational;
   function "*" (A : Rational; B : Number  ) return Rational;
   function "*" (A : Number;   B : Rational) return Rational;

   function "/" (A : Rational; B : Rational) return Rational;
   function "/" (A : Rational; B : Number  ) return Rational;
   function "/" (A : Number;   B : Rational) return Rational;
   function "/" (A : Number;   B : Number)   return Rational;
   
   function ">"  (A : Rational; B : Rational) return Boolean;
   function ">"  (A : Number;   B : Rational) return Boolean;
   function ">"  (A : Rational; B : Number)   return Boolean;

   function "<"  (A : Rational; B : Rational) return Boolean;
   function "<"  (A : Number;   B : Rational) return Boolean;
   function "<"  (A : Rational; B : Number)   return Boolean;

   function ">=" (A : Rational; B : Rational) return Boolean;
   function ">=" (A : Number;   B : Rational) return Boolean;
   function ">=" (A : Rational; B : Number)   return Boolean;

   function "<=" (A : Rational; B : Rational) return Boolean;
   function "<=" (A : Number;   B : Rational) return Boolean;
   function "<=" (A : Rational; B : Number)   return Boolean;

   function "="  (A : Number;   B : Rational) return Boolean;
   function "="  (A : Rational; B : Number)   return Boolean;

   function Numerator   (A : Rational) return Number;
   function Denominator (A : Rational) return Number;
             
   Zero : constant Rational;
   One  : constant Rational;
private
   type Rational is record
      Numerator   : Number;
      Denominator : Number;
   end record;

   Zero : constant Rational := (0, 1);
   One  : constant Rational := (1, 1);
end Generic_Rational;
```

The package can be instantiated with any integer type. It provides rational numbers represented by a numerator and denominator cleaned from the common divisors. Mixed arithmetic of the base integer type and the rational type is supported. Division to zero raises Constraint_Error. The implementation of the specification above is as follows:

```ada
package body Generic_Rational is

   function GCD (A, B : Number) return Number is
   begin
      if A = 0 then
         return B;
      end if;
      if B = 0 then
         return A;
      end if;
      if A > B then
         return GCD (B, A mod B);
      else
         return GCD (A, B mod A);
      end if;
   end GCD;

   function Inverse (A : Rational) return Rational is
   begin
      if A.Numerator > 0 then
         return (A.Denominator, A.Numerator);
      elsif A.Numerator < 0 then
         return (-A.Denominator, -A.Numerator);
      else
         raise Constraint_Error;
      end if;
   end Inverse;

   function "abs" (A : Rational) return Rational is
   begin
      return (abs A.Numerator, A.Denominator);
   end "abs";

   function "+" (A : Rational) return Rational is
   begin
      return A;
   end "+";

   function "-" (A : Rational) return Rational is
   begin
      return (-A.Numerator, A.Denominator);
   end "-";
   
   function "+" (A : Rational; B : Rational) return Rational is
      Common        : constant Number := GCD (A.Denominator, B.Denominator);
      A_Denominator : constant Number := A.Denominator / Common; 
      B_Denominator : constant Number := B.Denominator / Common; 
   begin
      return (A.Numerator * B_Denominator + B.Numerator * A_Denominator) /
             (A_Denominator * B.Denominator);
   end "+";

   function "+" (A : Rational; B : Number) return Rational is
   begin
      return (A.Numerator + B * A.Denominator) / A.Denominator;
   end "+";

   function "+" (A : Number; B : Rational) return Rational is
   begin
      return B + A;
   end "+";

   function "-" (A : Rational; B : Rational) return Rational is
   begin
      return A + (-B);
   end "-";

   function "-" (A : Rational; B : Number) return Rational is
   begin
      return A + (-B);
   end "-";

   function "-" (A : Number; B : Rational) return Rational is
   begin
      return A + (-B);
   end "-";

   function "*" (A : Rational; B : Rational) return Rational is
   begin
      return (A.Numerator * B.Numerator) / (A.Denominator * B.Denominator);
   end "*";

   function "*" (A : Rational; B : Number) return Rational is
      Common : constant Number := GCD (A.Denominator, abs B);
   begin
      return (A.Numerator * B / Common, A.Denominator / Common);
   end "*";

   function "*" (A : Number; B : Rational) return Rational is
   begin
      return B * A;
   end "*";

   function "/" (A : Rational; B : Rational) return Rational is
   begin
      return A * Inverse (B);
   end "/";

   function "/" (A : Rational; B : Number) return Rational is
      Common : constant Number := GCD (abs A.Numerator, abs B);
   begin
      if B > 0 then
         return (A.Numerator / Common, A.Denominator * (B / Common));
      else
         return ((-A.Numerator) / Common, A.Denominator * ((-B) / Common));
      end if;
   end "/";

   function "/" (A : Number; B : Rational) return Rational is
   begin
      return Inverse (B) * A;
   end "/";

   function "/" (A : Number; B : Number) return Rational is
      Common : constant Number := GCD (abs A, abs B);
   begin
      if B = 0 then
         raise Constraint_Error;
      elsif A = 0 then
         return (0, 1);
      elsif A > 0 xor B > 0 then
         return (-(abs A / Common), abs B / Common);
      else
         return (abs A / Common, abs B / Common);
      end if;
   end "/";
   
   function ">" (A, B : Rational) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator > 0;
   end ">";

   function ">" (A : Number; B : Rational) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator > 0;
   end ">";

   function ">" (A : Rational; B : Number) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator > 0;
   end ">";

   function "<" (A, B : Rational) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator < 0;
   end "<";

   function "<" (A : Number; B : Rational) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator < 0;
   end "<";
   
   function "<" (A : Rational; B : Number) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator < 0;
   end "<";

   function ">=" (A, B : Rational) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator >= 0;
   end ">=";

   function ">=" (A : Number; B : Rational) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator >= 0;
   end ">=";

   function ">=" (A : Rational; B : Number) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator >= 0;
   end ">=";

   function "<=" (A, B : Rational) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator <= 0;
   end "<=";

   function "<=" (A : Number; B : Rational) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator <= 0;
   end "<=";

   function "<=" (A : Rational; B : Number) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator <= 0;
   end "<=";

   function "=" (A : Number; B : Rational) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator = 0;
   end "=";

   function "=" (A : Rational; B : Number) return Boolean is
      Diff : constant Rational := A - B;
   begin
      return Diff.Numerator = 0;
   end "=";

   function Numerator (A : Rational) return Number is
   begin
      return A.Numerator;
   end Numerator;

   function Denominator (A : Rational) return Number is
   begin
      return A.Denominator;
   end Denominator;

end Generic_Rational;
```

The implementation uses solution of the [[greatest common divisor]] task. Here is the implementation of the test:

```ada
with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;                        use Ada.Text_IO;
with Generic_Rational;

procedure Test_Rational is
   package Integer_Rational is new Generic_Rational (Integer);
   use Integer_Rational;
begin
   for Candidate in 2..2**15 loop
      declare
         Sum  : Rational := 1 / Candidate;
      begin
         for Divisor in 2..Integer (Sqrt (Float (Candidate))) loop
            if Candidate mod Divisor = 0 then -- Factor is a divisor of Candidate
               Sum := Sum + One / Divisor + Rational'(Divisor / Candidate);
            end if;
         end loop;
         if Sum = 1 then
            Put_Line (Integer'Image (Candidate) & " is perfect");
         end if;
      end;
   end loop;
end Test_Rational;
```

The perfect numbers are searched by summing of the reciprocal of each of the divisors of a candidate except 1. This sum must be 1 for a perfect number.
{{out}}

```txt

 6 is perfect
 28 is perfect
 496 is perfect
 8128 is perfect

```

