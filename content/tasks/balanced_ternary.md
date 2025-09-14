+++
title = "Balanced ternary"
description = ""
date = 2019-03-15T01:11:30Z
aliases = []
[extra]
id = 10748
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "ats",
  "autohotkey",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "elixir",
  "erlang",
  "glagol",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "liberty_basic",
  "ocaml",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "prolog",
  "python",
  "racket",
  "rexx",
  "ruby",
  "scala",
  "tcl",
]
+++

## Task
[[wp:Balanced ternary|Balanced ternary]] is a way of representing numbers.  Unlike the prevailing binary representation, a balanced ternary integer is in base 3, and each digit can have the values 1, 0, or −1.


;Examples:
Decimal 11 = 3<sup>2</sup> + 3<sup>1</sup> − 3<sup>0</sup>, thus it can be written as "++−"

Decimal  6 = 3<sup>2</sup> − 3<sup>1</sup> + 0 × 3<sup>0</sup>, thus it can be written as "+−0"


;Task:
Implement balanced ternary representation of integers with the following:
# Support arbitrarily large integers, both positive and negative;
# Provide ways to convert to and from text strings, using digits '+', '-' and '0' (unless you are already using strings to represent balanced ternary; but see requirement 5).
# Provide ways to convert to and from native integer type (unless, improbably, your platform's native integer type ''is'' balanced ternary).  If your native integers can't support arbitrary length, overflows during conversion must be indicated.
# Provide ways to perform addition, negation and multiplication directly on balanced ternary integers; do ''not'' convert to native integers first.
# Make your implementation efficient, with a reasonable definition of "efficient" (and with a reasonable definition of "reasonable").


'''Test case''' With balanced ternaries ''a'' from string "+-0++0+", ''b'' from native integer -436, ''c'' "+-++-":
* write out ''a'', ''b'' and ''c'' in decimal notation;
* calculate ''a'' × (''b'' − ''c''), write out the result in both ternary and decimal notations.


'''Note:''' The pages [[generalised floating point addition]] and [[generalised floating point multiplication]] have code implementing [[wp:arbitrary precision|arbitrary precision]] [[wp:floating point|floating point]] balanced ternary.





## ALGOL 68

See also:
* [[Generalised floating point addition#ALGOL 68|Generalised floating point addition]]
* [[Generalised floating point multiplication#ALGOL 68|Generalised floating point multiplication]]


## Ada

Specifications (bt.ads):

```Ada
with Ada.Finalization;

package BT is

   type Balanced_Ternary is private;

   -- conversions
   function To_Balanced_Ternary (Num : Integer) return Balanced_Ternary;
   function To_Balanced_Ternary (Str : String)  return Balanced_Ternary;
   function To_Integer (Num : Balanced_Ternary)  return Integer;
   function To_string (Num : Balanced_Ternary)   return String;

   -- Arithmetics
   -- unary minus
   function "-" (Left : in Balanced_Ternary)
		return Balanced_Ternary;

   -- subtraction
   function "-" (Left, Right : in Balanced_Ternary)
		return Balanced_Ternary;

   -- addition
   function "+" (Left, Right : in Balanced_Ternary)
		return Balanced_Ternary;
   -- multiplication
   function "*" (Left, Right : in Balanced_Ternary)
		return Balanced_Ternary;

private
   -- a balanced ternary number is a unconstrained array of (1,0,-1)
   -- dinamically allocated, least significant trit leftmost
   type Trit is range -1..1;
   type Trit_Array is array (Positive range <>) of Trit;
   pragma Pack(Trit_Array);

   type Trit_Access is access Trit_Array;

   type Balanced_Ternary is new Ada.Finalization.Controlled
     with record
	Ref : Trit_access;
   end record;

   procedure Initialize (Object : in out Balanced_Ternary);
   procedure Adjust     (Object : in out Balanced_Ternary);
   procedure Finalize   (Object : in out Balanced_Ternary);

end BT;
```


Implementation (bt.adb):

```Ada
with Ada.Unchecked_Deallocation;

package body BT is

   procedure Free is new Ada.Unchecked_Deallocation (Trit_Array, Trit_Access);

   -- Conversions
   -- String to BT
   function To_Balanced_Ternary (Str: String) return Balanced_Ternary is
      J : Positive := 1;
      Tmp : Trit_Access;
   begin
      Tmp := new Trit_Array (1..Str'Last);
      for I in reverse Str'Range loop
	 case Str(I) is
	    when '+' => Tmp (J) := 1;
	    when '-' => Tmp (J) := -1;
	    when '0' => Tmp (J) := 0;
	    when others => raise Constraint_Error;
	 end case;
	 J := J + 1;
      end loop;
      return (Ada.Finalization.Controlled with Ref => Tmp);
   end To_Balanced_Ternary;

   -- Integer to BT
   function To_Balanced_Ternary (Num: Integer) return Balanced_Ternary is
      K      : Integer := 0;
      D      : Integer;
      Value  : Integer := Num;
      Tmp    : Trit_Array(1..19); -- 19 trits is enough to contain
                                   -- a 32 bits signed integer
   begin
      loop
	 D := (Value mod 3**(K+1))/3**K;
	 if D = 2 then D := -1; end if;
	 Value := Value - D*3**K;
	 K := K + 1;
	 Tmp(K) := Trit(D);
	 exit when Value = 0;
      end loop;
      return (Ada.Finalization.Controlled
		with Ref => new Trit_Array'(Tmp(1..K)));
   end To_Balanced_Ternary;

   -- BT to Integer --
   -- If the BT number is too large Ada will raise CONSTRAINT ERROR
   function To_Integer (Num : Balanced_Ternary) return Integer is
      Value : Integer := 0;
      Pos : Integer := 1;
   begin
      for I in Num.Ref.all'Range loop
	 Value := Value + Integer(Num.Ref(I)) * Pos;
	 Pos := Pos * 3;
      end loop;
      return Value;
   end To_Integer;

   -- BT to String --
   function To_String (Num : Balanced_Ternary) return String is
      I : constant Integer := Num.Ref.all'Last;
      Result : String (1..I);
   begin
      for J in Result'Range loop
	 case Num.Ref(I-J+1) is
	    when 0  => Result(J) := '0';
	    when -1 => Result(J) := '-';
	    when 1  => Result(J) := '+';
	 end case;
      end loop;
      return Result;
   end To_String;

   -- unary minus --
   function "-" (Left : in Balanced_Ternary)
		return Balanced_Ternary is
      Result : constant Balanced_Ternary := Left;
   begin
      for I in Result.Ref.all'Range loop
	 Result.Ref(I) := - Result.Ref(I);
      end loop;
      return Result;
   end "-";

   -- addition --
   Carry : Trit;

   function Add (Left, Right : in Trit)
		return Trit is
   begin
      if Left /= Right then
	 Carry := 0;
	 return Left + Right;
      else
	 Carry := Left;
	 return -Right;
      end if;
   end Add;
   pragma Inline (Add);

   function "+" (Left, Right : in Trit_Array)
		return Balanced_Ternary is
      Max_Size : constant Integer :=
	Integer'Max(Left'Last, Right'Last);
      Tmp_Left, Tmp_Right : Trit_Array(1..Max_Size) := (others => 0);
      Result : Trit_Array(1..Max_Size+1) := (others => 0);
   begin
      Tmp_Left (1..Left'Last) := Left;
      Tmp_Right(1..Right'Last) := Right;
      for I in Tmp_Left'Range loop
	 Result(I) := Add (Result(I), Tmp_Left(I));
	 Result(I+1) := Carry;
	 Result(I) := Add(Result(I), Tmp_Right(I));
	 Result(I+1) := Add(Result(I+1), Carry);
      end loop;
      -- remove trailing zeros
      for I in reverse Result'Range loop
	 if Result(I) /= 0 then
	    return (Ada.Finalization.Controlled
		      with Ref => new Trit_Array'(Result(1..I)));
	 end if;
      end loop;
      return (Ada.Finalization.Controlled
		with Ref => new Trit_Array'(1 => 0));
   end "+";

   function "+" (Left, Right : in Balanced_Ternary)
		return Balanced_Ternary is
   begin
      return Left.Ref.all + Right.Ref.all;
   end "+";

   -- Subtraction
   function "-" (Left, Right : in Balanced_Ternary)
		return Balanced_Ternary is
   begin
      return Left + (-Right);
   end "-";

   -- multiplication
   function "*" (Left, Right : in Balanced_Ternary)
		return Balanced_Ternary is
      A, B : Trit_Access;
      Result : Balanced_Ternary;
   begin
      if Left.Ref.all'Length > Right.Ref.all'Length then
	 A := Right.Ref; B := Left.Ref;
      else
	 B := Right.Ref; A := Left.Ref;
      end if;
      for I in A.all'Range loop
	 if A(I) /= 0 then
	    declare
	       Tmp_Result : Trit_Array (1..I+B.all'Length-1) := (others => 0);
	    begin
	       for J in B.all'Range loop
		  Tmp_Result(I+J-1) := B(J) * A(I);
	       end loop;
	       Result := Result.Ref.all + Tmp_Result;
	    end;
	 end if;
      end loop;
      return Result;
   end "*";

   procedure Adjust (Object : in out Balanced_Ternary) is
   begin
      Object.Ref := new Trit_Array'(Object.Ref.all);
   end Adjust;

   procedure Finalize  (Object : in out Balanced_Ternary) is
   begin
      Free (Object.Ref);
   end Finalize;

   procedure Initialize (Object : in out Balanced_Ternary) is
   begin
      Object.Ref := new Trit_Array'(1 => 0);
   end Initialize;

end BT;
```


Test task requirements (testbt.adb):

```Ada
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with BT; use BT;

procedure TestBT is
   Result, A, B, C : Balanced_Ternary;
begin
   A := To_Balanced_Ternary("+-0++0+");
   B := To_Balanced_Ternary(-436);
   C := To_Balanced_Ternary("+-++-");

   Result := A * (B - C);

   Put("a = "); Put(To_integer(A), 4); New_Line;
   Put("b = "); Put(To_integer(B), 4); New_Line;
   Put("c = "); Put(To_integer(C), 4); New_Line;
   Put("a * (b - c) = "); Put(To_integer(Result), 4);
   Put_Line (" " & To_String(Result));
end TestBT;
```

Output:

```txt

a =  523
b = -436
c =   65
a * (b - c) = -262023 ----0+--0++0

```



## ATS


```ATS

(*
** This one is
** translated into ATS from the Ocaml entry
*)

(* ****** ****** *)
//
// How to compile:
// patscc -DATS_MEMALLOC_LIBC -o bternary bternary.dats
//
(* ****** ****** *)

#include
"share/atspre_staload.hats"

(* ****** ****** *)

datatype btd = P | Z | N; typedef btern = List0(btd)

(* ****** ****** *)

fun
btd2int (d: btd): int =
  (case+ d of P() => 1 | Z() => 0 | N() => ~1)

(* ****** ****** *)

fun
btd2string (d:btd): string =
(
case+ d of P() => "+" | Z() => "0" | N() => "-"
)

(* ****** ****** *)

fun
btern2string
(
  ds: btern
) : string =
  strptr2string(res) where
{
  val xs = list_map_cloref (ds, lam d => btd2string(d))
  val xs = list_vt_reverse (xs)
  val res = stringlst_concat($UNSAFE.castvwtp1{List(string)}(xs))
  val () = list_vt_free<string> (xs)
}

(* ****** ****** *)

fun
from_string
  (inp: string): btern = let
//
fun
loop{n:nat}
(
  inp: string(n), ds: btern
) : btern =
(
//
  if isneqz(inp)
    then let
      val c = inp.head()
      val d =
        (case- c of '+' => P | '0' => Z | '-' => N): btd
      // end of [val]
    in
      loop (inp.tail(), list_cons(d, ds))
    end // end of [then]
    else ds // end of [else]
//
) (* end of [loop] *)
//
in
  loop (g1ofg0(inp), list_nil)
end // end of [from_string]

(* ****** ****** *)

fun
to_int (ds: btern): int =
(
case+ ds of
| list_nil () => 0
| list_cons (d, ds) => 3*to_int(ds) + btd2int(d)
) (* end of [to_int] *)

fun
from_int (n: int): btern =
(
if
n = 0
then list_nil
else let
  val r = n mod 3
in
  if r = 0
    then list_cons (Z, from_int (n/3))
    else if (r = 1 || r = ~2)
           then list_cons (P, from_int ((n-1)/3))
           else list_cons (N, from_int ((n+1)/3))
end // end of [else]
) (* end of [from_int] *)

(* ****** ****** *)

fun
neg_btern
  (ds: btern): btern =
list_vt2t
(
list_map_cloref<btd><btd>
  (ds, lam d => case+ d of P() => N() | Z() => Z() | N() => P())
) (* end of [neg_btern] *)

overload ~ with neg_btern

(* ****** ****** *)
//
extern
fun
add_btern_btern: (btern, btern) -> btern
and
sub_btern_btern: (btern, btern) -> btern
overload + with add_btern_btern of 100
overload - with sub_btern_btern of 100
//
extern
fun
mul_btern_btern: (btern, btern) -> btern
overload * with mul_btern_btern of 110
//
(* ****** ****** *)

#define :: list_cons

(* ****** ****** *)

local

fun aux0 (ds: btern): btern =
(
  case+ ds of nil() => ds | _ => Z()::ds
)

fun succ(ds:btern) = ds+list_sing(P())
fun pred(ds:btern) = ds+list_sing(N())

in (* in-of-local *)

implement
add_btern_btern
  (ds1, ds2) =
(
case+ (ds1, ds2) of
| (nil(), _) => ds2
| (_, nil()) => ds1
| (P()::ds1, N()::ds2) => aux0 (ds1+ds2)
| (Z()::ds1, Z()::ds2) => aux0 (ds1+ds2)
| (N()::ds1, P()::ds2) => aux0 (ds1+ds2)
| (P()::ds1, P()::ds2) => N() :: succ(ds1 + ds2)
| (N()::ds1, N()::ds2) => P() :: pred(ds1 + ds2)
| (Z()::ds1, btd::ds2) => btd :: (ds1 + ds2)
| (btd::ds1, Z()::ds2) => btd :: (ds1 + ds2)
)

implement
sub_btern_btern (ds1, ds2) = ds1 + (~ds2)

implement
mul_btern_btern (ds1, ds2) =
(
case+ ds2 of
| nil() => nil()
| Z()::ds2 => aux0 (ds1 * ds2)
| P()::ds2 => aux0 (ds1 * ds2) + ds1
| N()::ds2 => aux0 (ds1 * ds2) - ds1
)

end // end of [local]

(* ****** ****** *)

typedef charptr = $extype"char*"

(* ****** ****** *)

implement main0 () =
{
//
val a =
from_string "+-0++0+"
//
val b = from_int (~436)
val c = from_string "+-++-"
//
val d = a * (b - c)
//
val () =
$extfcall
(
  void
, "printf"
, "a = %d\nb = %d\nc = %d\na * (b - c) = %s = %d\n"
, to_int(a)
, to_int(b)
, to_int(c)
, $UNSAFE.cast{charptr}(btern2string(d))
, to_int(d)
) (* end of [val] *)
//
} (* end of [main0] *)

```

Output:

```txt

a =  523
b = -436
c =   65
a * (b - c) = -262023 ----0+--0++0

```



## AutoHotkey


```AutoHotkey
BalancedTernary(n){
	k = 0
	if abs(n)<2
		return n=1?"+":n=0?"0":"-"
	if n<1
		negative := true, n:= -1*n
	while !break {
		d := Mod(n, 3**(k+1)) / 3**k
		d := d=2?-1:d
		n := n - (d * 3**k)
		r := (d=-1?"-":d=1?"+":0) . r
		k++
		if (n = 3**k)
			r := "+" . r	, break := true
	}
	if negative {
		StringReplace, r, r, -,n, all
		StringReplace, r, r, `+,-, all
		StringReplace, r, r, n,+, all
	}
	return r
}
```

Examples:
```AutoHotkey
data =
(
523
-436
65
-262023
)
loop, Parse, data, `n
	result .= A_LoopField " : " BalancedTernary(A_LoopField) "`n"
MsgBox % result
return
```

Outputs:
```txt

523 	: +-0++0+
-436 	: -++-0--
65 	: +-++-
-262023	: ----0+--0++0
```



## C++


```cpp

#include <iostream>
#include <string>
#include <climits>
using namespace std;

class BalancedTernary {
protected:
	// Store the value as a reversed string of +, 0 and - characters
	string value;

	// Helper function to change a balanced ternary character to an integer
	int charToInt(char c) const {
		if (c == '0')
			return 0;
		return 44 - c;
	}

	// Helper function to negate a string of ternary characters
	string negate(string s) const {
		for (int i = 0; i < s.length(); ++i) {
			if (s[i] == '+')
				s[i] = '-';
			else if (s[i] == '-')
				s[i] = '+';
		}
		return s;
	}

public:
	// Default constructor
	BalancedTernary() {
		value = "0";
	}

	// Construct from a string
	BalancedTernary(string s) {
		value = string(s.rbegin(), s.rend());
	}

	// Construct from an integer
	BalancedTernary(long long n) {
		if (n == 0) {
			value = "0";
			return;
		}

		bool neg = n < 0;
		if (neg)
			n = -n;

		value = "";
		while (n != 0) {
			int r = n % 3;
			if (r == 0)
				value += "0";
			else if (r == 1)
				value += "+";
			else {
				value += "-";
				++n;
			}

			n /= 3;
		}

		if (neg)
			value = negate(value);
	}

	// Copy constructor
	BalancedTernary(const BalancedTernary &n) {
		value = n.value;
	}

	// Addition operators
	BalancedTernary operator+(BalancedTernary n) const {
		n += *this;
		return n;
	}

	BalancedTernary& operator+=(const BalancedTernary &n) {
		static char *add = "0+-0+-0";
		static char *carry = "--000++";

		int lastNonZero = 0;
		char c = '0';
		for (int i = 0; i < value.length() || i < n.value.length(); ++i) {
			char a = i < value.length() ? value[i] : '0';
			char b = i < n.value.length() ? n.value[i] : '0';

			int sum = charToInt(a) + charToInt(b) + charToInt(c) + 3;
			c = carry[sum];

			if (i < value.length())
				value[i] = add[sum];
			else
				value += add[sum];

			if (add[sum] != '0')
				lastNonZero = i;
		}

		if (c != '0')
			value += c;
		else
			value = value.substr(0, lastNonZero + 1); // Chop off leading zeroes

		return *this;
	}

	// Negation operator
	BalancedTernary operator-() const {
		BalancedTernary result;
		result.value = negate(value);
		return result;
	}

	// Subtraction operators
	BalancedTernary operator-(const BalancedTernary &n) const {
		return operator+(-n);
	}

	BalancedTernary& operator-=(const BalancedTernary &n) {
		return operator+=(-n);
	}

	// Multiplication operators
	BalancedTernary operator*(BalancedTernary n) const {
		n *= *this;
		return n;
	}

	BalancedTernary& operator*=(const BalancedTernary &n) {
		BalancedTernary pos = *this;
		BalancedTernary neg = -pos; // Storing an extra copy to avoid negating repeatedly
		value = "0";

		for (int i = 0; i < n.value.length(); ++i) {
			if (n.value[i] == '+')
				operator+=(pos);
			else if (n.value[i] == '-')
				operator+=(neg);
			pos.value = '0' + pos.value;
			neg.value = '0' + neg.value;
		}

		return *this;
	}

	// Stream output operator
	friend ostream& operator<<(ostream &out, const BalancedTernary &n) {
		out << n.toString();
		return out;
	}

	// Convert to string
	string toString() const {
		return string(value.rbegin(), value.rend());
	}

	// Convert to integer
	long long toInt() const {
		long long result = 0;
		for (long long i = 0, pow = 1; i < value.length(); ++i, pow *= 3)
			result += pow * charToInt(value[i]);
		return result;
	}

	// Convert to integer if possible
	bool tryInt(long long &out) const {
		long long result = 0;
		bool ok = true;

		for (long long i = 0, pow = 1; i < value.length() && ok; ++i, pow *= 3) {
			if (value[i] == '+') {
				ok &= LLONG_MAX - pow >= result; // Clear ok if the result overflows
				result += pow;
			} else if (value[i] == '-') {
				ok &= LLONG_MIN + pow <= result; // Clear ok if the result overflows
				result -= pow;
			}
		}

		if (ok)
			out = result;
		return ok;
	}
};

int main() {
	BalancedTernary a("+-0++0+");
	BalancedTernary b(-436);
	BalancedTernary c("+-++-");

	cout << "a = " << a << " = " << a.toInt() << endl;
	cout << "b = " << b << " = " << b.toInt() << endl;
	cout << "c = " << c << " = " << c.toInt() << endl;

	BalancedTernary d = a * (b - c);

	cout << "a * (b - c) = " << d << " = " << d.toInt() << endl;

	BalancedTernary e("+++++++++++++++++++++++++++++++++++++++++");

	long long n;
	if (e.tryInt(n))
		cout << "e = " << e << " = " << n << endl;
	else
		cout << "e = " << e << " is too big to fit in a long long" << endl;

	return 0;
}

```


Output

```txt

a = +-0++0+ = 523
b = -++-0-- = -436
c = +-++- = 65
a * (b - c) = ----0+--0++0 = -262023
e = +++++++++++++++++++++++++++++++++++++++++ is too big to fit in a long long

```


## C#

```c#
using System;
using System.Text;
using System.Collections.Generic;

public class BalancedTernary
{
	public static void Main()
	{
		BalancedTernary a = new BalancedTernary("+-0++0+");
		System.Console.WriteLine("a: " + a + " = " + a.ToLong());
		BalancedTernary b = new BalancedTernary(-436);
		System.Console.WriteLine("b: " + b + " = " + b.ToLong());
		BalancedTernary c = new BalancedTernary("+-++-");
		System.Console.WriteLine("c: " + c + " = " + c.ToLong());
		BalancedTernary d = a * (b - c);
		System.Console.WriteLine("a * (b - c): " + d + " = " + d.ToLong());
	}

	private enum BalancedTernaryDigit
	{
		MINUS = -1,
		ZERO = 0,
		PLUS = 1
	}

	private BalancedTernaryDigit[] value;

	// empty = 0
	public BalancedTernary()
	{
		this.value = new BalancedTernaryDigit[0];
	}

	// create from String
	public BalancedTernary(String str)
	{
		this.value = new BalancedTernaryDigit[str.Length];
		for (int i = 0; i < str.Length; ++i)
		{
			switch (str[i])
			{
				case '-':
					this.value[i] = BalancedTernaryDigit.MINUS;
					break;
				case '0':
					this.value[i] = BalancedTernaryDigit.ZERO;
					break;
				case '+':
					this.value[i] = BalancedTernaryDigit.PLUS;
					break;
				default:
					throw new ArgumentException("Unknown Digit: " + str[i]);
			}
		}
		Array.Reverse(this.value);
	}

	// convert long integer
	public BalancedTernary(long l)
	{
		List<BalancedTernaryDigit> value = new List<BalancedTernaryDigit>();
		int sign = Math.Sign(l);
		l = Math.Abs(l);

		while (l != 0)
		{
			byte rem = (byte)(l % 3);
			switch (rem)
			{
				case 0:
				case 1:
					value.Add((BalancedTernaryDigit)rem);
					l /= 3;
					break;
				case 2:
					value.Add(BalancedTernaryDigit.MINUS);
					l = (l + 1) / 3;
					break;
			}
		}

		this.value = value.ToArray();
		if (sign < 0)
		{
			this.Invert();
		}
	}

	// copy constructor
	public BalancedTernary(BalancedTernary origin)
	{
		this.value = new BalancedTernaryDigit[origin.value.Length];
		Array.Copy(origin.value, this.value, origin.value.Length);
	}

	// only for internal use
	private BalancedTernary(BalancedTernaryDigit[] value)
	{
		int end = value.Length - 1;
		while (value[end] == BalancedTernaryDigit.ZERO)
			--end;
		this.value = new BalancedTernaryDigit[end + 1];
		Array.Copy(value, this.value, end + 1);
	}

	// invert the values
	private void Invert()
	{
		for (int i=0; i < this.value.Length; ++i)
		{
			this.value[i] = (BalancedTernaryDigit)(-(int)this.value[i]);
		}
	}

	// convert to string
	override public String ToString()
	{
		StringBuilder result = new StringBuilder();
		for (int i = this.value.Length - 1; i >= 0; --i)
		{
			switch (this.value[i])
			{
				case BalancedTernaryDigit.MINUS:
					result.Append('-');
					break;
				case BalancedTernaryDigit.ZERO:
					result.Append('0');
					break;
				case BalancedTernaryDigit.PLUS:
					result.Append('+');
					break;
			}
		}
		return result.ToString();
	}

	// convert to long
	public long ToLong()
	{
		long result = 0;
		int digit;
		for (int i = 0; i < this.value.Length; ++i)
		{
			result += (long)this.value[i] * (long)Math.Pow(3.0, (double)i);
		}
		return result;
	}

	// unary minus
	public static BalancedTernary operator -(BalancedTernary origin)
	{
		BalancedTernary result = new BalancedTernary(origin);
		result.Invert();
		return result;
	}

	// addition of digits
	private static BalancedTernaryDigit carry = BalancedTernaryDigit.ZERO;
	private static BalancedTernaryDigit Add(BalancedTernaryDigit a, BalancedTernaryDigit b)
	{
		if (a != b)
		{
			carry = BalancedTernaryDigit.ZERO;
			return (BalancedTernaryDigit)((int)a + (int)b);
		}
		else
		{
			carry = a;
			return (BalancedTernaryDigit)(-(int)b);
		}
	}

	// addition of balanced ternary numbers
	public static BalancedTernary operator +(BalancedTernary a, BalancedTernary b)
	{
		int maxLength = Math.Max(a.value.Length, b.value.Length);
		BalancedTernaryDigit[] resultValue = new BalancedTernaryDigit[maxLength + 1];
		for (int i=0; i < maxLength; ++i)
		{
			if (i < a.value.Length)
			{
				resultValue[i] = Add(resultValue[i], a.value[i]);
				resultValue[i+1] = carry;
			}
			else
			{
				carry = BalancedTernaryDigit.ZERO;
			}

			if (i < b.value.Length)
			{
				resultValue[i] = Add(resultValue[i], b.value[i]);
				resultValue[i+1] = Add(resultValue[i+1], carry);
			}
		}
		return new BalancedTernary(resultValue);
	}

	// subtraction of balanced ternary numbers
	public static BalancedTernary operator -(BalancedTernary a, BalancedTernary b)
	{
		return a + (-b);
	}

	// multiplication of balanced ternary numbers
	public static BalancedTernary operator *(BalancedTernary a, BalancedTernary b)
	{
		BalancedTernaryDigit[] longValue = a.value;
		BalancedTernaryDigit[] shortValue = b.value;
		BalancedTernary result = new BalancedTernary();
		if (a.value.Length < b.value.Length)
		{
			longValue = b.value;
			shortValue = a.value;
		}

		for (int i = 0; i < shortValue.Length; ++i)
		{
			if (shortValue[i] != BalancedTernaryDigit.ZERO)
			{
				BalancedTernaryDigit[] temp = new BalancedTernaryDigit[i + longValue.Length];
				for (int j = 0; j < longValue.Length; ++j)
				{
					temp[i+j] = (BalancedTernaryDigit)((int)shortValue[i] * (int)longValue[j]);
				}
				result = result + new BalancedTernary(temp);
			}
		}
		return result;
	}
}
```

output:

```txt
a: +-0++0+ = 523
b: -++-0-- = -436
c: +-++- = 65
a * (b - c): ----0+--0++0 = -262023
```



## Common Lisp


```lisp
;;; balanced ternary
;;; represented as a list of 0, 1 or -1s, with least significant digit first

;;; convert ternary to integer
(defun bt-integer (b)
  (reduce (lambda (x y) (+ x (* 3 y))) b :from-end t :initial-value 0))

;;; convert integer to ternary
(defun integer-bt (n)
  (if (zerop n) nil
    (case (mod n 3)
      (0 (cons  0 (integer-bt (/ n 3))))
      (1 (cons  1 (integer-bt (floor n 3))))
      (2 (cons -1 (integer-bt (floor (1+ n) 3)))))))

;;; convert string to ternary
(defun string-bt (s)
  (loop with o = nil for c across s do
	  (setf o (cons (case c (#\+ 1) (#\- -1) (#\0 0)) o))
	  finally (return o)))

;;; convert ternary to string
(defun bt-string (bt)
  (if (not bt) "0"
    (let* ((l (length bt))
	   (s (make-array l :element-type 'character)))
      (mapc (lambda (b)
	      (setf (aref s (decf l))
		    (case b (-1 #\-) (0 #\0) (1 #\+))))
	    bt)
      s)))

;;; arithmetics
(defun bt-neg (a) (map 'list #'- a))
(defun bt-sub (a b) (bt-add a (bt-neg b)))

(let ((tbl #((0 -1) (1 -1) (-1 0) (0 0) (1 0) (-1 1) (0 1))))
  (defun bt-add-digits (a b c)
    (values-list (aref tbl (+ 3 a b c)))))

(defun bt-add (a b &optional (c 0))
  (if (not (and a b))
    (if (zerop c) (or a b)
      (bt-add (list c) (or a b)))
    (multiple-value-bind (d c)
      (bt-add-digits (if a (car a) 0) (if b (car b) 0) c)
      (let ((res (bt-add (cdr a) (cdr b) c)))
	;; trim leading zeros
	(if (or res (not (zerop d)))
	  (cons d res))))))

(defun bt-mul (a b)
  (if (not (and a b))
    nil
    (bt-add (case (car a)
	        (-1 (bt-neg b))
		( 0 nil)
		( 1 b))
	    (cons 0 (bt-mul (cdr a) b)))))

;;; division with quotient/remainder, for completeness
(defun bt-truncate (a b)
  (let ((n (- (length a) (length b)))
	(d (car (last b))))
    (if (minusp n)
      (values nil a)
      (labels ((recur (a b x)
	 (multiple-value-bind (quo rem)
	   (if (plusp x) (recur a (cons 0 b) (1- x))
	     (values nil a))

	   (loop with g = (car (last rem))
		 with quo = (cons 0 quo)
		 while (= (length rem) (length b)) do
		 (cond ((= g d) (setf rem (bt-sub rem b)
				      quo (bt-add '(1) quo)))
		       ((= g (- d)) (setf rem (bt-add rem b)
					  quo (bt-add '(-1) quo))))
		 (setf x (car (last rem)))
		 finally (return (values quo rem))))))

	(recur a b n)))))

;;; test case
(let* ((a (string-bt "+-0++0+"))
       (b (integer-bt -436))
       (c (string-bt "+-++-"))
       (d (bt-mul a (bt-sub b c))))
  (format t "a~5d~8t~a~%b~5d~8t~a~%c~5d~8t~a~%a × (b − c) = ~d ~a~%"
	  (bt-integer a) (bt-string a)
	  (bt-integer b) (bt-string b)
	  (bt-integer c) (bt-string c)
	  (bt-integer d) (bt-string d)))
```
output<lang>a  523  +-0++0+
b -436  -++-0--
c   65  +-++-
a × (b − c) = -262023 ----0+--0++0
```



## D

{{trans|Python}}

```d
import std.stdio, std.bigint, std.range, std.algorithm;

struct BalancedTernary {
    // Represented as a list of 0, 1 or -1s,
    // with least significant digit first.
    enum Dig : byte { N=-1, Z=0, P=+1 } // Digit.
    const Dig[] digits;

    // This could also be a BalancedTernary template argument.
    static immutable string dig2str = "-0+";

    immutable static Dig[dchar] str2dig; // = ['+': Dig.P, ...];
    nothrow static this() {
        str2dig = ['+': Dig.P, '-':  Dig.N, '0': Dig.Z];
    }

    immutable pure nothrow static Dig[2][] table =
        [[Dig.Z, Dig.N], [Dig.P, Dig.N], [Dig.N, Dig.Z],
         [Dig.Z, Dig.Z], [Dig.P, Dig.Z], [Dig.N, Dig.P],
         [Dig.Z, Dig.P]];

    this(in string inp) const pure {
        this.digits = inp.retro.map!(c => str2dig[c]).array;
    }

    this(in long inp) const pure nothrow {
        this.digits = _bint2ternary(inp.BigInt);
    }

    this(in BigInt inp) const pure nothrow {
        this.digits = _bint2ternary(inp);
    }

    this(in BalancedTernary inp) const pure nothrow {
        // No need to dup, they are virtually immutable.
        this.digits = inp.digits;
    }

    private this(in Dig[] inp) pure nothrow {
        this.digits = inp;
    }

    static Dig[] _bint2ternary(in BigInt n) pure nothrow {
        static py_div(T1, T2)(in T1 a, in T2 b) pure nothrow {
            if (a < 0) {
                return (b < 0) ?
                       -a / -b :
                       -(-a / b) - (-a % b != 0 ? 1 : 0);
            } else {
                return (b < 0) ?
                       -(a / -b) - (a % -b != 0 ? 1 : 0) :
                       a / b;
            }
        }

        if (n == 0) return [];
        // This final switch in D v.2.064 is fake, not enforced.
        final switch (((n % 3) + 3) % 3) { // (n % 3) is the remainder.
            case 0: return Dig.Z ~ _bint2ternary(py_div(n, 3));
            case 1: return Dig.P ~ _bint2ternary(py_div(n, 3));
            case 2: return Dig.N ~ _bint2ternary(py_div(n + 1, 3));
        }
    }

    @property BigInt toBint() const pure nothrow {
        return reduce!((y, x) => x + 3 * y)(0.BigInt, digits.retro);
    }

    string toString() const pure nothrow {
        if (digits.empty) return "0";
        return digits.retro.map!(d => dig2str[d + 1]).array;
    }

    static const(Dig)[] neg_(in Dig[] digs) pure nothrow {
        return digs.map!(a => -a).array;
    }

    BalancedTernary opUnary(string op:"-")() const pure nothrow {
        return BalancedTernary(neg_(this.digits));
    }

    static const(Dig)[] add_(in Dig[] a, in Dig[] b, in Dig c=Dig.Z)
    pure nothrow {
        const a_or_b = a.length ? a : b;
        if (a.empty || b.empty) {
            if (c == Dig.Z)
                return a_or_b;
            else
                return BalancedTernary.add_([c], a_or_b);
        } else {
            // (const d, c) = table[...];
            const dc = table[3 + (a.length ? a[0] : 0) +
                             (b.length ? b[0] : 0) + c];
            const res = add_(a[1 .. $], b[1 .. $], dc[1]);
            // Trim leading zeros.
            if (res.length || dc[0] != Dig.Z)
                return [dc[0]] ~ res;
            else
                return res;
        }
    }

    BalancedTernary opBinary(string op:"+")(in BalancedTernary b)
    const pure nothrow {
        return BalancedTernary(add_(this.digits, b.digits));
    }

    BalancedTernary opBinary(string op:"-")(in BalancedTernary b)
    const pure nothrow {
        return this + (-b);
    }

    static const(Dig)[] mul_(in Dig[] a, in Dig[] b) pure nothrow {
        if (a.empty || b.empty) {
            return [];
        } else {
            const y = Dig.Z ~ mul_(a[1 .. $], b);
            final switch (a[0]) {
                case Dig.N: return add_(neg_(b), y);
                case Dig.Z: return add_([], y);
                case Dig.P: return add_(b, y);
            }
        }
    }

    BalancedTernary opBinary(string op:"*")(in BalancedTernary b)
    const pure nothrow {
        return BalancedTernary(mul_(this.digits, b.digits));
    }
}

void main() {
    immutable a = BalancedTernary("+-0++0+");
    writeln("a: ", a.toBint, ' ', a);

    immutable b = BalancedTernary(-436);
    writeln("b: ", b.toBint, ' ', b);

    immutable c = BalancedTernary("+-++-");
    writeln("c: ", c.toBint, ' ', c);

    const /*immutable*/ r = a * (b - c);
    writeln("a * (b - c): ", r.toBint, ' ', r);
}
```

{{out}}

```txt
a: 523 +-0++0+
b: -436 -++-0--
c: 65 +-++-
a * (b - c): -262023 ----0+--0++0
```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Ternary do
  def to_string(t), do: ( for x <- t, do: to_char(x) ) |> List.to_string

  def from_string(s), do: ( for x <- to_char_list(s), do: from_char(x) )

  defp to_char(-1), do: ?-
  defp to_char(0), do: ?0
  defp to_char(1), do: ?+

  defp from_char(?-), do: -1
  defp from_char(?0), do: 0
  defp from_char(?+), do: 1

  def to_ternary(n) when n > 0, do: to_ternary(n,[])
  def to_ternary(n), do: neg(to_ternary(-n))

  defp to_ternary(0,acc), do: acc
  defp to_ternary(n,acc) when rem(n, 3) == 0, do: to_ternary(div(n, 3), [0|acc])
  defp to_ternary(n,acc) when rem(n, 3) == 1, do: to_ternary(div(n, 3), [1|acc])
  defp to_ternary(n,acc), do: to_ternary(div((n+1), 3), [-1|acc])

  def from_ternary(t), do: from_ternary(t,0)

  defp from_ternary([],acc), do: acc
  defp from_ternary([h|t],acc), do: from_ternary(t, acc*3 + h)

  def mul(a,b), do: mul(b,a,[])

  defp mul(_,[],acc), do: acc
  defp mul(b,[a|as],acc) do
    bp = case a do
           -1 -> neg(b)
            0 -> [0]
            1 -> b
         end
    a = add(bp, acc ++ [0])
    mul(b,as,a)
  end

  defp neg(t), do: ( for h <- t, do: -h )

  def sub(a,b), do: add(a,neg(b))

  def add(a,b) when length(a) < length(b),
    do: add(List.duplicate(0, length(b)-length(a)) ++ a, b)
  def add(a,b) when length(a) > length(b), do: add(b,a)
  def add(a,b), do: add(Enum.reverse(a), Enum.reverse(b), 0, [])

  defp add([],[],0,acc), do: acc
  defp add([],[],c,acc), do: [c|acc]
  defp add([a|as],[b|bs],c,acc) do
    [c1,d] = add_util(a+b+c)
    add(as,bs,c1,[d|acc])
  end

  defp add_util(-3), do: [-1,0]
  defp add_util(-2), do: [-1,1]
  defp add_util(-1), do: [0,-1]
  defp add_util(3), do: [1,0]
  defp add_util(2), do: [1,-1]
  defp add_util(1), do: [0,1]
  defp add_util(0), do: [0,0]
end

as = "+-0++0+"; at = Ternary.from_string(as); a = Ternary.from_ternary(at)
b = -436; bt = Ternary.to_ternary(b); bs = Ternary.to_string(bt)
cs = "+-++-"; ct = Ternary.from_string(cs); c = Ternary.from_ternary(ct)
rt = Ternary.mul(at,Ternary.sub(bt,ct))
r = Ternary.from_ternary(rt)
rs = Ternary.to_string(rt)
IO.puts "a = #{as} -> #{a}"
IO.puts "b = #{bs} -> #{b}"
IO.puts "c = #{cs} -> #{c}"
IO.puts "a x (b - c) = #{rs} -> #{r}"
```


{{out}}

```txt

a = +-0++0+ -> 523
b = -++-0-- -> -436
c = +-++- -> 65
a x (b - c) = 0----0+--0++0 -> -262023

```



## Erlang


```erlang

-module(ternary).
-compile(export_all).

test() ->
    AS = "+-0++0+", AT = from_string(AS), A = from_ternary(AT),
    B = -436, BT = to_ternary(B), BS = to_string(BT),
    CS = "+-++-", CT = from_string(CS), C = from_ternary(CT),
    RT = mul(AT,sub(BT,CT)),
    R = from_ternary(RT),
    RS = to_string(RT),
    io:fwrite("A = ~s -> ~b~n",[AS, A]),
    io:fwrite("B = ~s -> ~b~n",[BS, B]),
    io:fwrite("C = ~s -> ~b~n",[CS, C]),
    io:fwrite("A x (B - C) = ~s -> ~b~n", [RS, R]).

to_string(T) -> [to_char(X) || X <- T].

from_string(S) -> [from_char(X) || X <- S].

to_char(-1) -> $-;
to_char(0) -> $0;
to_char(1) -> $+.

from_char($-) -> -1;
from_char($0) -> 0;
from_char($+) -> 1.

to_ternary(N) when N > 0 ->
    to_ternary(N,[]);
to_ternary(N) ->
    neg(to_ternary(-N)).

to_ternary(0,Acc) ->
    Acc;
to_ternary(N,Acc) when N rem 3 == 0 ->
    to_ternary(N div 3, [0|Acc]);
to_ternary(N,Acc) when N rem 3 == 1 ->
    to_ternary(N div 3, [1|Acc]);
to_ternary(N,Acc) ->
    to_ternary((N+1) div 3, [-1|Acc]).

from_ternary(T) -> from_ternary(T,0).

from_ternary([],Acc) ->
    Acc;
from_ternary([H|T],Acc) ->
    from_ternary(T,Acc*3 + H).

mul(A,B) -> mul(B,A,[]).

mul(_,[],Acc) ->
    Acc;
mul(B,[A|As],Acc) ->
    BP = case A of
             -1 -> neg(B);
             0 ->  [0];
             1 ->  B
         end,
    A1 = Acc++[0],
    A2=add(BP,A1),
    mul(B,As,A2).


neg(T) -> [ -H || H <- T].

sub(A,B) -> add(A,neg(B)).

add(A,B) when length(A) < length(B) ->
    add(lists:duplicate(length(B)-length(A),0)++A,B);
add(A,B) when length(A) > length(B) ->
   add(B,A);
add(A,B) ->
    add(lists:reverse(A),lists:reverse(B),0,[]).

add([],[],0,Acc) ->
    Acc;
add([],[],C,Acc) ->
    [C|Acc];
add([A|As],[B|Bs],C,Acc) ->
    [C1,D] = add_util(A+B+C),
    add(As,Bs,C1,[D|Acc]).

add_util(-3) -> [-1,0];
add_util(-2) -> [-1,1];
add_util(-1) -> [0,-1];
add_util(3) -> [1,0];
add_util(2) -> [1,-1];
add_util(1) -> [0,1];
add_util(0) -> [0,0].

```

'''Output'''

```erlang

234> ternary:test().
A = +-0++0+ -> 523
B = -++-0-- -> -436
C = +-++- -> 65
A x (B - C) = 0----0+--0++0 -> -262023
ok

```



## Glagol


```txt

ОТДЕЛ Сетунь+;
ИСПОЛЬЗУЕТ
  Параметр ИЗ "...\Отделы\Обмен\",
  Текст ИЗ "...\Отделы\Числа\",
  Вывод ИЗ "...\Отделы\Обмен\";

ПЕР
  зч: РЯД 10 ИЗ ЗНАК;
  счпоз: ЦЕЛ;
  число: ЦЕЛ;
  память: ДОСТУП К НАБОР
    ячейки: РЯД 20 ИЗ ЦЕЛ;
    размер: УЗКЦЕЛ;
    отрицательное: КЛЮЧ
  КОН;

ЗАДАЧА СоздатьПамять;
УКАЗ
  СОЗДАТЬ(память);
  память.размер := 0;
  память.отрицательное := ОТКЛ
КОН СоздатьПамять;

ЗАДАЧА ДобавитьВПамять(что: ЦЕЛ);
УКАЗ
  память.ячейки[память.размер] := что;
  УВЕЛИЧИТЬ(память.размер)
КОН ДобавитьВПамять;

ЗАДАЧА ОбратитьПамять;
ПЕР
  зчсл: ЦЕЛ;
  сч: ЦЕЛ;
УКАЗ
  ОТ сч := 0 ДО память.размер ДЕЛИТЬ 2 - 1 ВЫП
    зчсл := память.ячейки[сч];
    память.ячейки[сч] := память.ячейки[память.размер-сч-1];
    память.ячейки[память.размер-сч-1] := зчсл
  КОН
КОН ОбратитьПамять;

ЗАДАЧА ВывестиПамять;
ПЕР
  сч: ЦЕЛ;
УКАЗ
  ОТ сч := 0 ДО память.размер-1 ВЫП
    ЕСЛИ память.ячейки[сч] < 0 ТО
      Вывод.Цепь("-")
    АЕСЛИ память.ячейки[сч] > 0 ТО
      Вывод.Цепь("+")
    ИНАЧЕ Вывод.Цепь("0") КОН
  КОН
КОН ВывестиПамять;

ЗАДАЧА УдалитьПамять;
УКАЗ
  память := ПУСТО
КОН УдалитьПамять;

ЗАДАЧА Перевести(число: ЦЕЛ);
ПЕР
  о: ЦЕЛ;
  з: КЛЮЧ;
  ЗАДАЧА ВПамять(что: ЦЕЛ);
  УКАЗ
    ЕСЛИ память.отрицательное ТО
      ЕСЛИ что < 0 ТО ДобавитьВПамять(1)
      АЕСЛИ что > 0 ТО ДобавитьВПамять(-1)
      ИНАЧЕ ДобавитьВПамять(0) КОН
    ИНАЧЕ
      ДобавитьВПамять(что)
    КОН
  КОН ВПамять;
УКАЗ
  ЕСЛИ число < 0 ТО память.отрицательное := ВКЛ КОН;
  число := МОДУЛЬ(число);
  з := ОТКЛ;
  ПОКА число > 0 ВЫП
    о := число ОСТАТОК 3;
    число := число ДЕЛИТЬ 3;
    ЕСЛИ з ТО
      ЕСЛИ о = 2 ТО ВПамять(0) АЕСЛИ о = 1 ТО ВПамять(-1) ИНАЧЕ ВПамять(1); з := ОТКЛ КОН
    ИНАЧЕ
      ЕСЛИ о = 2 ТО ВПамять(-1); з := ВКЛ ИНАЧЕ ВПамять(о) КОН
    КОН
  КОН;
  ЕСЛИ з ТО ВПамять(1) КОН;
  ОбратитьПамять;
  ВывестиПамять(ВКЛ);
КОН Перевести;

ЗАДАЧА ВЧисло(): ЦЕЛ;
ПЕР
  сч, мн: ЦЕЛ;
  результат: ЦЕЛ;
УКАЗ
  результат := 0;
  мн := 1;
  ОТ сч := 0 ДО память.размер-1 ВЫП
    УВЕЛИЧИТЬ(результат, память.ячейки[память.размер-сч-1]*мн);
    мн := мн * 3
  КОН;
  ВОЗВРАТ результат
КОН ВЧисло;

УКАЗ
  Параметр.Текст(1, зч); счпоз := 0;
  число := Текст.ВЦел(зч, счпоз);
  СоздатьПамять;
  Перевести(число);
  Вывод.ЧЦел(" = %d.", ВЧисло(), 0, 0, 0);
  УдалитьПамять

КОН Сетунь.

```

'''A crude English/Pidgin Algol translation of the above [[:Category:Glagol]] code.'''

```algol68
PROGRAM Setun+;
USES
  Parameter IS "...\Departments\Exchange\"
  Text IS "...\Departments\Numbers\"
  Output IS "...\Departments\Exchange\";

VAR
  AF: RANGE 10 IS SIGN;
  mfpos: INT;
  number: INT;
  memory ACCESS TO STRUCT
    cell: RANGE 20 IS INT;
    size: UZKEL;
    negative: BOOL
  END;

PROC Create.Memory;
BEGIN
  CREATE(memory);
  memory.size := 0;
  memory.negative := FALSE
END Create.Memory;

PROC Add.Memory(that: INT)
BEGIN
  memory.cells[memory.size] := that;
  ZOOM(memory.size)
END Add.Memory;

PROC Invert.Memory;
VAR
  zchsl: INT;
  account: INT;
BEGIN
  FOR cq := 0 TO memory.size DIVIDE 2 - 1 DO
    zchsl := memory.cells[cq];
    memory.cells[cq] := memory.cells[memory.size-size-1];
    memory.cells[memory.size-MF-1] := zchsl
  END
END Invert.Memory;

PROC Withdraw.Memory;
VAR
  account: INT;
BEGIN
  FOR cq := 0 TO memory.size-1 DO
    IF memory.cells[cq] < 0 THEN
      Output.Append("-")
    ANDIF memory.cells[cq] > 0 THEN
      Output.Append("+")
    ELSE Output.Append("0") END
  END
END Withdraw.Memory;

PROC Remove.Memory;
BEGIN
  memory := Empty
END Remove.Memory;

PROC Translate(number: INT)
VAR
  about: INT;
  s: BOOL;
  PROC B.Memory(that: INT)
  BEGIN
    IF memory.negative THEN
      IF that < 0 THEN Add.Memory(1)
      ANDIF that > 0 THEN Add.Memory(1)
      ELSE Add.Memory(0) END
    ELSE
      Add.Memory(that)
    END
  END B.Memory;
BEGIN
  IF number < 0 THEN memory.negative := TRUE END;
  number := UNIT(number)
  s := FALSE;
  WHILE number > 0 DO
    about := number BALANCE 3;
    number := number DIVIDE 3;
    IF s THEN
      IF about = 2 THEN B.Memory(0) ANDIF about = 1 THEN B.Memory(1) ELSE B.Memory(1) s := FALSE END
    ELSE
      IF about = 2 THEN B.Memory(-1) s := TRUE ELSE B.Memory(a) END
    END
  END;
  IF s THEN B.Memory(1) END;
  Invert.Memory;
  Withdraw.Memory(TRUE)
END Translate;

PROC InNumber(): INT;
VAR
  MF, MN: INT;
  result: INT;
BEGIN
  result := 0
  pl := 1;
  FOR cq := 0 TO memory.size-1 DO
    ZOOM(result, memory.Cells[memory.size-cq-1] * mn);
    pl := pl * 3
  END;
  RETURN result;
END InNumber;

BEGIN
  Parameter.Text(1, AF); mfpos := 0;
  number := Text.Whole(AF, mfpos);
  Create.Memory;
  Translate(number);
  Output.ChTarget(" = %d.", InNumber(), 0, 0, 0);
  Remove.Memory
END Setun.
```



## Go


```go
package main

import (
    "fmt"
    "strings"
)

// R1: representation is a slice of int8 digits of -1, 0, or 1.
// digit at index 0 is least significant.  zero value of type is
// representation of the number 0.
type bt []int8

// R2: string conversion:

// btString is a constructor.  valid input is a string of any length
// consisting of only '+', '-', and '0' characters.
// leading zeros are allowed but are trimmed and not represented.
// false return means input was invalid.
func btString(s string) (*bt, bool) {
    s = strings.TrimLeft(s, "0")
    b := make(bt, len(s))
    for i, last := 0, len(s)-1; i < len(s); i++ {
        switch s[i] {
        case '-':
            b[last-i] = -1
        case '0':
            b[last-i] = 0
        case '+':
            b[last-i] = 1
        default:
            return nil, false
        }
    }
    return &b, true
}

// String method converts the other direction, returning a string of
// '+', '-', and '0' characters representing the number.
func (b bt) String() string {
    if len(b) == 0 {
        return "0"
    }
    last := len(b) - 1
    r := make([]byte, len(b))
    for i, d := range b {
        r[last-i] = "-0+"[d+1]
    }
    return string(r)
}

// R3: integer conversion
// int chosen as "native integer"

// btInt is a constructor like btString.
func btInt(i int) *bt {
    if i == 0 {
        return new(bt)
    }
    var b bt
    var btDigit func(int)
    btDigit = func(digit int) {
        m := int8(i % 3)
        i /= 3
        switch m {
        case 2:
            m = -1
            i++
        case -2:
            m = 1
            i--
        }
        if i == 0 {
            b = make(bt, digit+1)
        } else {
            btDigit(digit + 1)
        }
        b[digit] = m
    }
    btDigit(0)
    return &b
}

// Int method converts the other way, returning the value as an int type.
// !ok means overflow occurred during conversion, not necessarily that the
// value is not representable as an int.  (Of course there are other ways
// of doing it but this was chosen as "reasonable.")
func (b bt) Int() (r int, ok bool) {
    pt := 1
    for _, d := range b {
        dp := int(d) * pt
        neg := r < 0
        r += dp
        if neg {
            if r > dp {
                return 0, false
            }
        } else {
            if r < dp {
                return 0, false
            }
        }
        pt *= 3
    }
    return r, true
}

// R4: negation, addition, and multiplication

func (z *bt) Neg(b *bt) *bt {
    if z != b {
        if cap(*z) < len(*b) {
            *z = make(bt, len(*b))
        } else {
            *z = (*z)[:len(*b)]
        }
    }
    for i, d := range *b {
        (*z)[i] = -d
    }
    return z
}

func (z *bt) Add(a, b *bt) *bt {
    if len(*a) < len(*b) {
        a, b = b, a
    }
    r := *z
    r = r[:cap(r)]
    var carry int8
    for i, da := range *a {
        if i == len(r) {
            n := make(bt, len(*a)+4)
            copy(n, r)
            r = n
        }
        sum := da + carry
        if i < len(*b) {
            sum += (*b)[i]
        }
        carry = sum / 3
        sum %= 3
        switch {
        case sum > 1:
            sum -= 3
            carry++
        case sum < -1:
            sum += 3
            carry--
        }
        r[i] = sum
    }
    last := len(*a)
    if carry != 0 {
        if len(r) == last {
            n := make(bt, last+4)
            copy(n, r)
            r = n
        }
        r[last] = carry
        *z = r[:last+1]
        return z
    }
    for {
        if last == 0 {
            *z = nil
            break
        }
        last--
        if r[last] != 0 {
            *z = r[:last+1]
            break
        }
    }
    return z
}

func (z *bt) Mul(a, b *bt) *bt {
    if len(*a) < len(*b) {
        a, b = b, a
    }
    var na bt
    for _, d := range *b {
        if d == -1 {
            na.Neg(a)
            break
        }
    }
    r := make(bt, len(*a)+len(*b))
    for i := len(*b) - 1; i >= 0; i-- {
        switch (*b)[i] {
        case 1:
            p := r[i:]
            p.Add(&p, a)
        case -1:
            p := r[i:]
            p.Add(&p, &na)
        }
    }
    i := len(r)
    for i > 0 && r[i-1] == 0 {
        i--
    }
    *z = r[:i]
    return z
}

func main() {
    a, _ := btString("+-0++0+")
    b := btInt(-436)
    c, _ := btString("+-++-")
    show("a:", a)
    show("b:", b)
    show("c:", c)
    show("a(b-c):", a.Mul(a, b.Add(b, c.Neg(c))))
}

func show(label string, b *bt) {
    fmt.Printf("%7s %12v ", label, b)
    if i, ok := b.Int(); ok {
        fmt.Printf("%7d\n", i)
    } else {
        fmt.Println("int overflow")
    }
}
```

{{out}}

```txt

     a:      +-0++0+     523
     b:      -++-0--    -436
     c:        +-++-      65
a(b-c): ----0+--0++0 -262023

```



## Groovy

Solution:

```groovy
enum T {
    m('-', -1), z('0', 0), p('+', 1)

    final String symbol
    final int value

    private T(String symbol, int value) {
        this.symbol = symbol
        this.value = value
    }

    static T get(Object key) {
        switch (key) {
            case [m.value, m.symbol] : return m
            case [z.value, z.symbol] : return z
            case [p.value, p.symbol] : return p
            default:                   return null
        }
    }

    T negative() {
        T.get(-this.value)
    }

    String toString() { this.symbol }
}


class BalancedTernaryInteger {

    static final MINUS = new BalancedTernaryInteger(T.m)
    static final ZERO  = new BalancedTernaryInteger(T.z)
    static final PLUS  = new BalancedTernaryInteger(T.p)
    private static final LEADING_ZEROES = /^0+/

    final String value

    BalancedTernaryInteger(String bt) {
        assert bt && bt.toSet().every { T.get(it) }
        value = bt ==~ LEADING_ZEROES ? T.z : bt.replaceAll(LEADING_ZEROES, '');
    }

    BalancedTernaryInteger(BigInteger i) {
        this(i == 0 ? T.z.symbol : valueFromInt(i));
    }

    BalancedTernaryInteger(T...tArray) {
        this(tArray.sum{ it.symbol });
    }

    BalancedTernaryInteger(List<T> tList) {
        this(tList.sum{ it.symbol });
    }

    private static String valueFromInt(BigInteger i) {
        assert i != null
        if (i < 0) return negate(valueFromInt(-i))
        if (i == 0) return ''
        int bRem = (((i % 3) - 2) ?: -3) + 2
        valueFromInt((i - bRem).intdiv(3)) + T.get(bRem)
    }

    private static String negate(String bt) {
        bt.collect{ T.get(it) }.inject('') { str, t ->
            str + (-t)
        }
    }

    private static final Map INITIAL_SUM_PARTS = [carry:T.z, sum:[]]
    private static final prepValueLen = { int len, String s ->
        s.padLeft(len + 1, T.z.symbol).collect{ T.get(it) }
    }
    private static final partCarrySum = { partialSum, carry, trit ->
        [carry: carry, sum: [trit] + partialSum]
    }
    private static final partSum = { parts, trits ->
        def carrySum = partCarrySum.curry(parts.sum)
        switch ((trits + parts.carry).sort()) {
            case [[T.m, T.m, T.m]]:                  return carrySum(T.m, T.z) //-3
            case [[T.m, T.m, T.z]]:                  return carrySum(T.m, T.p) //-2
            case [[T.m, T.z, T.z], [T.m, T.m, T.p]]: return carrySum(T.z, T.m) //-1
            case [[T.z, T.z, T.z], [T.m, T.z, T.p]]: return carrySum(T.z, T.z) //+0
            case [[T.z, T.z, T.p], [T.m, T.p, T.p]]: return carrySum(T.z, T.p) //+1
            case [[T.z, T.p, T.p]]:                  return carrySum(T.p, T.m) //+2
            case [[T.p, T.p, T.p]]: default:         return carrySum(T.p, T.z) //+3
        }
    }

    BalancedTernaryInteger plus(BalancedTernaryInteger that) {
        assert that != null
        if (this == ZERO) return that
        if (that == ZERO) return this
        def prep = prepValueLen.curry([value.size(), that.value.size()].max())
        List values = [prep(value), prep(that.value)].transpose()
        new BalancedTernaryInteger(values[-1..(-values.size())].inject(INITIAL_SUM_PARTS, partSum).sum)
    }

    BalancedTernaryInteger negative() {
        !this ? this : new BalancedTernaryInteger(negate(value))
    }

    BalancedTernaryInteger minus(BalancedTernaryInteger that) {
        assert that != null
        this + -that
    }

    private static final INITIAL_PRODUCT_PARTS = [sum:ZERO, pad:'']
    private static final sigTritCount = { it.value.replaceAll(T.z.symbol,'').size() }

    private BalancedTernaryInteger paddedValue(String pad) {
        new BalancedTernaryInteger(value + pad)
    }

    private BalancedTernaryInteger partialProduct(T multiplier, String pad){
        switch (multiplier) {
            case T.z:          return ZERO
            case T.m:          return -paddedValue(pad)
            case T.p: default: return paddedValue(pad)
        }
    }

    BalancedTernaryInteger multiply(BalancedTernaryInteger that) {
        assert that != null
        if (that == ZERO)  return ZERO
        if (that == PLUS)  return this
        if (that == MINUS) return -this
        if (this.value.size() == 1 || sigTritCount(this) < sigTritCount(that)) {
            return that.multiply(this)
        }
        that.value.collect{ T.get(it) }[-1..(-value.size())].inject(INITIAL_PRODUCT_PARTS) { parts, multiplier ->
            [sum: parts.sum + partialProduct(multiplier, parts.pad), pad: parts.pad + T.z]
        }.sum
    }

    BigInteger asBigInteger() {
        value.collect{ T.get(it) }.inject(0) { i, trit -> i * 3 + trit.value }
    }

    def asType(Class c) {
        switch (c) {
            case Integer:              return asBigInteger() as Integer
            case Long:                 return asBigInteger() as Long
            case [BigInteger, Number]: return asBigInteger()
            case Boolean:              return this != ZERO
            case String:               return toString()
            default:                   return super.asType(c)
        }
    }

    boolean equals(Object that) {
        switch (that) {
            case BalancedTernaryInteger: return this.value == that?.value
            default:                     return super.equals(that)
        }
    }

    int hashCode() { this.value.hashCode() }

    String toString() { value }
}
```


Test:

```groovy
BalancedTernaryInteger a = new BalancedTernaryInteger('+-0++0+')
BalancedTernaryInteger b = new BalancedTernaryInteger(-436)
BalancedTernaryInteger c = new BalancedTernaryInteger(T.p, T.m, T.p, T.p, T.m)
BalancedTernaryInteger bmc = new BalancedTernaryInteger(-436 - (c as Integer))
BalancedTernaryInteger atbmc = new BalancedTernaryInteger((a as Integer) * (-436 - (c as Integer)))

printf ("%9s = %12s %8d\n", 'a', "${a}", a as Number)
printf ("%9s = %12s %8d\n", 'b', "${b}", b as Number)
printf ("%9s = %12s %8d\n", 'c', "${c}", c as Number)
assert (b-c) == bmc
printf ("%9s = %12s %8d\n", 'b-c', "${b-c}", (b-c) as Number)
assert (a * (b-c)) == atbmc
printf ("%9s = %12s %8d\n", 'a * (b-c)', "${a * (b-c)}", (a * (b-c)) as Number)

println "\nDemonstrate failure:"
assert (a * (b-c)) == a
```


Output:

```txt
        a =      +-0++0+      523
        b =      -++-0--     -436
        c =        +-++-       65
      b-c =      -+0-++0     -501
a * (b-c) = ----0+--0++0  -262023

Demonstrate failure:
Caught: Assertion failed:

assert (a * (b-c)) == a
        | |  |||   |  |
        | |  |||   |  +-0++0+
        | |  |||   false
        | |  ||+-++-
        | |  |-+0-++0
        | |  -++-0--
        | ----0+--0++0
        +-0++0+
...
```



## Haskell

BTs are represented internally as lists of digits in integers from -1 to 1, but displayed as "+-0" strings.

```haskell
data BalancedTernary = Bt [Int]

zeroTrim a = if null s then [0] else s where
	s = fst $ foldl f ([],[]) a
	f (x,y) 0 = (x, y++[0])
	f (x,y) z = (x++y++[z], [])

btList (Bt a) = a

instance Eq BalancedTernary where
	(==) a b = btList a == btList b

btNormalize = listBt . _carry 0 where
	_carry c [] = if c == 0 then [] else [c]
	_carry c (a:as) = r:_carry cc as where
		(cc, r) = f $ (a+c) `quotRem` 3 where
			f (x,  2) = (x + 1, -1)
			f (x, -2) = (x - 1,  1)
			f x = x

listBt = Bt . zeroTrim

instance Show BalancedTernary where
	show = reverse . map (\d->case d of -1->'-'; 0->'0'; 1->'+') . btList

strBt = Bt . zeroTrim.reverse.map (\c -> case c of '-' -> -1; '0' -> 0; '+' -> 1)

intBt :: Integral a => a -> BalancedTernary
intBt = fromIntegral . toInteger

btInt = foldr (\a z -> a + 3 * z) 0 . btList

listAdd a b = take (max (length a) (length b)) $ zipWith (+) (a++[0,0..]) (b++[0,0..])

-- mostly for operators, also small stuff to make GHC happy
instance Num BalancedTernary where
	negate = Bt . map negate . btList
	(+) x y = btNormalize $ listAdd (btList x) (btList y)
	(*) x y = btNormalize $ mul_ (btList x) (btList y) where
		mul_ _ [] = []
                mul_ as b = foldr (\a z -> listAdd (map (a*) b) (0:z)) [] as

	-- we don't need to define binary "-" by hand

	signum (Bt a) = if a == [0] then 0 else Bt [last a]
	abs x = if signum x == Bt [-1] then negate x else x

	fromInteger = btNormalize . f where
		f 0 = []
		f x = fromInteger (rem x 3) : f (quot x 3)


main = let	(a,b,c) = (strBt "+-0++0+", intBt (-436), strBt "+-++-")
		r = a * (b - c)
	in do
		print $ map btInt [a,b,c]
		print $ r
		print $ btInt r
```


=={{header|Icon}} and {{header|Unicon}}==

{{trans|java}}

Works in both languages:

```unicon
procedure main()
    a := "+-0++0+"
    write("a = +-0++0+"," = ",cvtFromBT("+-0++0+"))
    write("b = -436 = ",b := cvtToBT(-436))
    c := "+-++-"
    write("c = +-++- = ",cvtFromBT("+-++-"))
    d := mul(a,sub(b,c))
    write("a(b-c) = ",d," = ",cvtFromBT(d))
end

procedure bTrim(s)
    return s[upto('+-',s):0] | "0"
end

procedure cvtToBT(n)
    if n=0 then return "0"
    if n<0 then return map(cvtToBT(-n),"+-","-+")
    return bTrim(case n%3 of {
        0: cvtToBT(n/3)||"0"
        1: cvtToBT(n/3)||"+"
        2: cvtToBT((n+1)/3)||"-"
        })
end

procedure cvtFromBT(n)
    sum := 0
    i := -1
    every c := !reverse(n) do {
        sum +:= case c of {
            "+" : 1
            "-" : -1
            "0" : 0
            }*(3^(i+:=1))
        }
    return sum
end

procedure neg(n)
    return map(n,"+-","-+")
end

procedure add(a,b)
    if *b > *a then a :=: b
    b := repl("0",*a-*b)||b
    c := "0"
    sum := ""
    every place := 1 to *a do {
        ds := addDigits(a[-place],b[-place],c)
        c := if *ds > 1 then c := ds[1] else "0"
        sum := ds[-1]||sum
        }
    return bTrim(c||sum)
end

procedure addDigits(a,b,c)
    sum1 := addDigit(a,b)
    sum2 := addDigit(sum1[-1],c)
    if *sum1 = 1 then return sum2
    if *sum2 = 1 then return sum1[1]||sum2
    return sum1[1]
end

procedure addDigit(a,b)
    return case(a||b) of {
                "00"|"0+"|"0-": b
                "+0"|"-0"     : a
                "++"          : "+-"
                "+-"|"-+"     : "0"
                "--"          : "-+"
                }
end

procedure sub(a,b)
    return add(a,neg(b))
end

procedure mul(a,b)
    if b[1] == "-" then {
        b := neg(b)
        negate := "yes"
        }
    b := cvtFromBT(b)
    i := "+"
    mul := "0"
    while cvtFromBT(i) <= b do {
        mul := add(mul,a)
        i := add(i,"+")
        }
    return (\negate,map(mul,"+-","-+")) | mul
end
```


Output:

```txt

->bt
a = +-0++0+ = 523
b = -436 = -++-0--
c = +-++- = 65
a(b-c) = ----0+--0++0 = -262023
->

```



## J


Implementation:


```j
trigits=: 1+3 <.@^. 2 * 1&>.@|
trinOfN=: |.@((_1 + ] #: #.&1@] + [) #&3@trigits) :. nOfTrin
nOfTrin=: p.&3 :. trinOfN
trinOfStr=: 0 1 _1 {~ '0+-'&i.@|. :. strOfTrin
strOfTrin=: {&'0+-'@|. :. trinOfStr

carry=: +//.@:(trinOfN"0)^:_
trimLead0=: (}.~ i.&1@:~:&0)&.|.

add=: carry@(+/@,:)
neg=: -
mul=: trimLead0@carry@(+//.@(*/))
```


trinary numbers are represented as a sequence of polynomial coefficients.  The coefficient values are limited to 1, 0, and -1.  The polynomial's "variable" will always be 3 (which happens to illustrate an interesting absurdity in the terminology we use to describe polynomials -- one which might be an obstacle for learning, for some people).

<code>trigits</code> computes the number of trinary "digits" (that is, the number of polynomial coefficients) needed to represent an integer. pseudocode: <code>1+floor(log3(2*max(1,abs(n)))</code>.  Note that floating point inaccuracies combined with comparison tolerance may lead to a [harmless] leading zero when converting incredibly large numbers.

foo<code>Of</code>Bar converts a bar into a foo.  These functions are all invertable (so we can map from one domain to another, perform an operation, and map back using J's ''under'').  This aspect is not needed for this task and the definitions could be made simpler by removing it (removing the <code>:. obverse</code> clauses), but it made testing and debugging easier.

<code>carry</code> performs carry propagation. (Intermediate results will have overflowed trinary representation and become regular integers, so we convert them back into trinary and then perform a polynomial sum, repeating until the result is the same as the argument.)

<code>trimLead0</code> removes leading zeros from a sequence of polynomial coefficients.

<code>add</code> adds these polynomials.
<code>neg</code> negates these polynomials.  Note that it's just a name for J's -
<code>mul</code> multiplies these polynomials.

Definitions for example:


```j
a=: trinOfStr '+-0++0+'
b=: trinOfN -436
c=: trinOfStr '+-++-'
```


Required example:


```j
   nOfTrin&> a;b;c
523 _436 65

   strOfTrin a mul b (add -) c
----0+--0++0
   nOfTrin   a mul b (add -) c
_262023
```



## Java


```java

/*
 * Test case
 * With balanced ternaries a from string "+-0++0+", b from native integer -436, c "+-++-":
 * Write out a, b and c in decimal notation;
 * Calculate a × (b − c), write out the result in both ternary and decimal notations.
 */
public class BalancedTernary
{
	public static void main(String[] args)
	{
 		BTernary a=new BTernary("+-0++0+");
		BTernary b=new BTernary(-436);
		BTernary c=new BTernary("+-++-");

		System.out.println("a="+a.intValue());
		System.out.println("b="+b.intValue());
		System.out.println("c="+c.intValue());
		System.out.println();

		//result=a*(b-c)
		BTernary result=a.mul(b.sub(c));

		System.out.println("result= "+result+" "+result.intValue());
	}


	public static class BTernary
	{
		String value;
		public BTernary(String s)
		{
			int i=0;
			while(s.charAt(i)=='0')
				i++;
			this.value=s.substring(i);
		}
		public BTernary(int v)
		{
			this.value="";
			this.value=convertToBT(v);
		}

		private String convertToBT(int v)
		{
			if(v<0)
				return flip(convertToBT(-v));
			if(v==0)
				return "";
			int rem=mod3(v);
			if(rem==0)
				return convertToBT(v/3)+"0";
			if(rem==1)
				return convertToBT(v/3)+"+";
			if(rem==2)
				return convertToBT((v+1)/3)+"-";
			return "You can't see me";
		}
		private String flip(String s)
		{
			String flip="";
			for(int i=0;i<s.length();i++)
			{
				if(s.charAt(i)=='+')
					flip+='-';
				else if(s.charAt(i)=='-')
					flip+='+';
				else
					flip+='0';
			}
			return flip;
		}
		private int mod3(int v)
		{
			if(v>0)
				return v%3;
			v=v%3;
			return (v+3)%3;
		}

		public int intValue()
		{
			int sum=0;
			String s=this.value;
			for(int i=0;i<s.length();i++)
			{
				char c=s.charAt(s.length()-i-1);
				int dig=0;
				if(c=='+')
					dig=1;
				else if(c=='-')
					dig=-1;
				sum+=dig*Math.pow(3, i);
			}
			return sum;
		}


		public BTernary add(BTernary that)
		{
			String a=this.value;
			String b=that.value;

			String longer=a.length()>b.length()?a:b;
			String shorter=a.length()>b.length()?b:a;

			while(shorter.length()<longer.length())
				shorter=0+shorter;

			a=longer;
			b=shorter;

			char carry='0';
			String sum="";
			for(int i=0;i<a.length();i++)
			{
				int place=a.length()-i-1;
				String digisum=addDigits(a.charAt(place),b.charAt(place),carry);
				if(digisum.length()!=1)
					carry=digisum.charAt(0);
				else
					carry='0';
				sum=digisum.charAt(digisum.length()-1)+sum;
			}
			sum=carry+sum;

			return new BTernary(sum);
		}
		private String addDigits(char a,char b,char carry)
		{
			String sum1=addDigits(a,b);
			String sum2=addDigits(sum1.charAt(sum1.length()-1),carry);
			//System.out.println(carry+" "+sum1+" "+sum2);
			if(sum1.length()==1)
				return sum2;
			if(sum2.length()==1)
				return sum1.charAt(0)+sum2;
			return sum1.charAt(0)+"";
		}
		private String addDigits(char a,char b)
		{
			String sum="";
			if(a=='0')
				sum=b+"";
			else if (b=='0')
				sum=a+"";
			else if(a=='+')
			{
				if(b=='+')
					sum="+-";
				else
					sum="0";
			}
			else
			{
				if(b=='+')
					sum="0";
				else
					sum="-+";
			}
			return sum;
		}

		public BTernary neg()
		{
			return new BTernary(flip(this.value));
		}

		public BTernary sub(BTernary that)
		{
			return this.add(that.neg());
		}

		public BTernary mul(BTernary that)
		{
			BTernary one=new BTernary(1);
			BTernary zero=new BTernary(0);
			BTernary mul=new BTernary(0);

			int flipflag=0;
			if(that.compareTo(zero)==-1)
			{
				that=that.neg();
				flipflag=1;
			}
			for(BTernary i=new BTernary(1);i.compareTo(that)<1;i=i.add(one))
				mul=mul.add(this);

			if(flipflag==1)
				mul=mul.neg();
			return mul;
		}

		public boolean equals(BTernary that)
		{
			return this.value.equals(that.value);
		}
		public int compareTo(BTernary that)
		{
			if(this.intValue()>that.intValue())
				return 1;
			else if(this.equals(that))
				return 0;
			 return -1;
		}

		public String toString()
		{
			return value;
		}
	}
}

```


Output:

```txt

a=523
b=-436
c=65

result= ----0+--0++0 -262023

```



## Julia

{{works with|Julia|0.6}}
{{trans|Python}}


```julia
struct BalancedTernary <: Signed
    digits::Vector{Int8}
end
BalancedTernary() = zero(BalancedTernary)
BalancedTernary(n) = convert(BalancedTernary, n)

const sgn2chr = Dict{Int8,Char}(-1 => '-', 0 => '0', +1 => '+')
Base.show(io::IO, bt::BalancedTernary) = print(io, join(sgn2chr[x] for x in reverse(bt.digits)))
Base.copy(bt::BalancedTernary) = BalancedTernary(copy(bt.digits))
Base.zero(::Type{BalancedTernary}) = BalancedTernary(Int8[0])
Base.iszero(bt::BalancedTernary) = bt.digits == Int8[0]
Base.convert(::Type{T}, bt::BalancedTernary) where T<:Number = sum(3 ^ T(ex - 1) * s for (ex, s) in enumerate(bt.digits))
function Base.convert(::Type{BalancedTernary}, n::Signed)
    r = BalancedTernary(Int8[])
    if iszero(n) push!(r.digits, 0) end
    while n != 0
        if mod(n, 3) == 0
            push!(r.digits, 0)
            n = fld(n, 3)
        elseif mod(n, 3) == 1
            push!(r.digits, 1)
            n = fld(n, 3)
        else
            push!(r.digits, -1)
            n = fld(n + 1, 3)
        end
    end
    return r
end
const chr2sgn = Dict{Char,Int8}('-' => -1, '0' => 0, '+' => 1)
function Base.convert(::Type{BalancedTernary}, s::AbstractString)
    return BalancedTernary(getindex.(chr2sgn, collect(reverse(s))))
end

macro bt_str(s)
    convert(BalancedTernary, s)
end

const table = NTuple{2,Int8}[(0, -1), (1, -1), (-1, 0), (0, 0), (1, 0), (-1, 1), (0, 1)]
function _add(a::Vector{Int8}, b::Vector{Int8}, c::Int8=Int8(0))
    if isempty(a) || isempty(b)
        if c == 0 return isempty(a) ? b : a end
        return _add([c], isempty(a) ? b : a)
    else
        d, c = table[4 + (isempty(a) ? 0 : a[1]) + (isempty(b) ? 0 : b[1]) + c]
        r = _add(a[2:end], b[2:end], c)
        if !isempty(r) || d != 0
            return unshift!(r, d)
        else
            return r
        end
    end
end
function Base.:+(a::BalancedTernary, b::BalancedTernary)
    v = _add(a.digits, b.digits)
    return isempty(v) ? BalancedTernary(0) : BalancedTernary(v)
end
Base.:-(bt::BalancedTernary) = BalancedTernary(-bt.digits)
Base.:-(a::BalancedTernary, b::BalancedTernary) = a + (-b)
function _mul(a::Vector{Int8}, b::Vector{Int8})
    if isempty(a) || isempty(b)
        return Int8[]
    else
        if a[1] == -1 x = (-BalancedTernary(b)).digits
        elseif a[1] == 0 x = Int8[]
        elseif a[1] == 1 x = b end
        y = append!(Int8[0], _mul(a[2:end], b))
        return _add(x, y)
    end
end
function Base.:*(a::BalancedTernary, b::BalancedTernary)
    v = _mul(a.digits, b.digits)
    return isempty(v) ? BalancedTernary(0) : BalancedTernary(v)
end

a = bt"+-0++0+"
println("a: $(Int(a)), $a")
b = BalancedTernary(-436)
println("b: $(Int(b)), $b")
c = BalancedTernary("+-++-")
println("c: $(Int(c)), $c")
r = a * (b - c)
println("a * (b - c): $(Int(r)), $r")

@assert Int(r) == Int(a) * (Int(b) - Int(c))
```


{{out}}

```txt
a: 523, +-0++0+
b: -436, -++-0--
c: 65, +-++-
a * (b - c): -262023, ----0+--0++0
```



## Kotlin

This is based on the Java entry. However, I've added 'BigInteger' support as this is a current requirement of the task description even though it's not actually needed to process the test case:

```scala
// version 1.1.3

import java.math.BigInteger

val bigZero = BigInteger.ZERO
val bigOne = BigInteger.ONE
val bigThree = BigInteger.valueOf(3L)

data class BTernary(private var value: String) : Comparable<BTernary> {

    init {
        require(value.all { it in "0+-" })
        value = value.trimStart('0')
    }

    constructor(v: Int) : this(BigInteger.valueOf(v.toLong()))

    constructor(v: BigInteger) : this("") {
        value = toBT(v)
    }

    private fun toBT(v: BigInteger): String {
        if (v < bigZero) return flip(toBT(-v))
        if (v == bigZero) return ""
        val rem = mod3(v)
        return when (rem) {
            bigZero -> toBT(v / bigThree) + "0"
            bigOne  -> toBT(v / bigThree) + "+"
            else    -> toBT((v + bigOne) / bigThree) + "-"
        }
    }

    private fun flip(s: String): String {
        val sb = StringBuilder()
        for (c in s) {
            sb.append(when (c) {
                '+'  -> "-"
                '-'  -> "+"
                else -> "0"
            })
        }
        return sb.toString()
    }

    private fun mod3(v: BigInteger): BigInteger {
        if (v > bigZero) return v % bigThree
        return ((v % bigThree) + bigThree) % bigThree
    }

    fun toBigInteger(): BigInteger {
        val len = value.length
        var sum = bigZero
        var pow = bigOne
        for (i in 0 until len) {
            val c = value[len - i - 1]
            val dig = when (c) {
                '+'  -> bigOne
                '-'  -> -bigOne
                else -> bigZero
            }
            if (dig != bigZero) sum += dig * pow
            pow *= bigThree
        }
        return sum
    }

    private fun addDigits(a: Char, b: Char, carry: Char): String {
        val sum1 = addDigits(a, b)
        val sum2 = addDigits(sum1.last(), carry)
        return when {
            sum1.length == 1 -> sum2
            sum2.length == 1 -> sum1.take(1) + sum2
            else             -> sum1.take(1)
        }
    }

    private fun addDigits(a: Char, b: Char): String =
        when {
            a == '0' -> b.toString()
            b == '0' -> a.toString()
            a == '+' -> if (b == '+') "+-" else "0"
            else     -> if (b == '+') "0" else "-+"
        }

    operator fun plus(other: BTernary): BTernary {
        var a = this.value
        var b = other.value
        val longer = if (a.length > b.length) a else b
        var shorter = if (a.length > b.length) b else a
        while (shorter.length < longer.length) shorter = "0" + shorter
        a = longer
        b = shorter
        var carry = '0'
        var sum = ""
        for (i in 0 until a.length) {
            val place = a.length - i - 1
            val digisum = addDigits(a[place], b[place], carry)
            carry = if (digisum.length != 1) digisum[0] else '0'
            sum = digisum.takeLast(1) + sum
        }
        sum = carry.toString() + sum
        return BTernary(sum)
    }

    operator fun unaryMinus() = BTernary(flip(this.value))

    operator fun minus(other: BTernary) = this + (-other)

    operator fun times(other: BTernary): BTernary {
        var that = other
        val one = BTernary(1)
        val zero = BTernary(0)
        var mul = zero
        var flipFlag = false
        if (that < zero) {
            that = -that
            flipFlag = true
        }
        var i = one
        while (i <= that) {
            mul += this
            i += one
        }
        if (flipFlag) mul = -mul
        return mul
    }

    override operator fun compareTo(other: BTernary) =
        this.toBigInteger().compareTo(other.toBigInteger())

    override fun toString() = value
}

fun main(args: Array<String>) {
    val a = BTernary("+-0++0+")
    val b = BTernary(-436)
    val c = BTernary("+-++-")
    println("a = ${a.toBigInteger()}")
    println("b = ${b.toBigInteger()}")
    println("c = ${c.toBigInteger()}")
    val bResult = a * (b - c)
    val iResult = bResult.toBigInteger()
    println("a * (b - c) = $bResult = $iResult")
}
```


{{out}}

```txt

a = 523
b = -436
c = 65
a * (b - c) = ----0+--0++0 = -262023

```



## Liberty BASIC


```lb

global tt$
tt$="-0+"   '-1 0 1; +2 -> 1 2 3, instr

'Test case:
'With balanced ternaries a from string "+-0++0+", b from native integer -436, c "+-++-":
'* write out a, b and c in decimal notation;
'* calculate a * (b - c), write out the result in both ternary and decimal notations.

a$="+-0++0+"
a=deci(a$)
print "a",a, a$

b=-436
b$=ternary$(b)
print "b",b, b$

c$="+-++-"
c=deci(c$)
print "c",c, c$

'calculate in ternary

res$=multTernary$(a$, subTernary$(b$, c$))
print "a * (b - c)", res$
print "In decimal:",deci(res$)

print "Check:"
print "a * (b - c)", a * (b - c)
end

function deci(s$)
    pow = 1
    for i = len(s$) to 1 step -1
        c$ = mid$(s$,i,1)
        'select case c$
        '    case "+":sign= 1
        '    case "-":sign=-1
        '    case "0":sign= 0
        'end select
        sign = instr(tt$,c$)-2
        deci = deci+pow*sign
        pow = pow*3
    next
end function

function ternary$(n)
    while abs(n)>3^k/2
        k=k+1
    wend
    k=k-1

    pow = 3^k
    for i = k to 0 step -1
        sign = (n>0) - (n<0)
        sign = sign * (abs(n)>pow/2)
        ternary$ = ternary$+mid$(tt$,sign+2,1)
        n = n - sign*pow
        pow = pow/3
    next
    if  ternary$ = "" then  ternary$ ="0"
end function

function multTernary$(a$, b$)

    c$ = ""
    t$ = ""
    shift$ = ""
    for i = len(a$) to 1 step -1

        select case mid$(a$,i,1)
        case "+": t$ = b$
        case "0": t$ = "0"
        case "-": t$ = negate$(b$)
        end select

        c$ = addTernary$(c$, t$+shift$)

        shift$ = shift$ +"0"
    'print d, t$, c$
    next
    multTernary$ = c$
end function

function subTernary$(a$, b$)
     subTernary$ = addTernary$(a$, negate$(b$))
end function

function negate$(s$)
    negate$=""
    for i = 1 to len(s$)
        'print mid$(s$,i,1), instr(tt$, mid$(s$,i,1)), 4-instr(tt$, mid$(s$,i,1))
        negate$=negate$+mid$(tt$, 4-instr(tt$, mid$(s$,i,1)), 1)
    next
end function

function addTernary$(a$, b$)
'add a$ + b$, for now only positive
    l = max(len(a$), len(b$))
    a$=pad$(a$,l)
    b$=pad$(b$,l)
    c$ = "" 'result
    carry = 0
    for i = l to 1 step -1
        a = instr(tt$,mid$(a$,i,1))-2
        b = instr(tt$,mid$(b$,i,1))-2     '-1 0 1
        c = a+b+carry

        select case
        case abs(c)<2
            carry = 0
        case c>0
            carry =1: c=c-3
        case c<0
            carry =-1: c=c+3
        end select

        'print a, b, c
        c$ = mid$(tt$,c+2,1)+c$
    next
    if carry<>0 then c$ = mid$(tt$,carry+2,1) +c$
    'print c$
    'have to trim leading 0's
    i=0
    while mid$(c$,i+1,1)="0"
        i=i+1
    wend
    c$=mid$(c$,i+1)
    if c$="" then c$="0"
    addTernary$ = c$
end function

function pad$(a$,n)  'pad from right with 0 to length n
     pad$ = a$
     while len(pad$)<n
        pad$ = "0"+pad$
     wend
end function

```


{{out}}

```txt

a             523           +-0++0+
b             -436          -++-0--
c             65            +-++-
a * (b - c)   ----0+--0++0
In decimal:   -262023
Check:
a * (b - c)   -262023

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```mathematica
frombt = FromDigits[StringCases[#, {"+" -> 1, "-" -> -1, "0" -> 0}],
    3] &;
tobt = If[Quotient[#, 3, -1] == 0,
     "", #0@Quotient[#, 3, -1]] <> (Mod[#,
       3, -1] /. {1 -> "+", -1 -> "-", 0 -> "0"}) &;
btnegate = StringReplace[#, {"+" -> "-", "-" -> "+"}] &;
btadd = StringReplace[
    StringJoin[
     Fold[Sort@{#1[[1]],
          Sequence @@ #2} /. {{x_, x_, x_} :> {x,
           "0" <> #1[[2]]}, {"-", "+", x_} | {x_, "-", "+"} | {x_,
            "0", "0"} :> {"0", x <> #1[[2]]}, {"+", "+", "0"} -> {"+",
            "-" <> #1[[2]]}, {"-", "-", "0"} -> {"-",
           "+" <> #1[[2]]}} &, {"0", ""},
      Reverse@Transpose@PadLeft[Characters /@ {#1, #2}] /. {0 ->
         "0"}]], StartOfString ~~ "0" .. ~~ x__ :> x] &;
btsubtract = btadd[#1, btnegate@#2] &;
btmultiply =
  btadd[Switch[StringTake[#2, -1], "0", "0", "+", #1, "-",
     btnegate@#1],
    If[StringLength@#2 == 1,
     "0", #0[#1, StringDrop[#2, -1]] <> "0"]] &;
```

Examples:

```mathematica
frombt[a = "+-0++0+"]
b = tobt@-436
frombt[c = "+-++-"]
btmultiply[a, btsubtract[b, c]]
```

Outputs:

```txt
523

"-++-0--"

65

"----0+--0++0"
```


=={{header|MK-61/52}}==
{{trans|Glagol}}
<lang>П0	ЗН	П2	0	П3	П4	1	П5
ИП0	/-/	x<0	78
	ИП0	^	^	3	/	[x]	П0	3	*	-	П1
	ИП3	x#0	52
		ИП1	x=0	36	1	ПП	86	0	П3	БП	08
		ИП1	1	-	x=0	47	1	/-/	ПП	86	БП	08
		0	ПП	86	БП	08
		ИП1	x=0	60	0	ПП	86	БП	08
		ИП1	1	-	x=0	70	1	ПП	86	БП	08
		1	/-/	ПП	86	1	П3	БП	08
ИП3	x#0	84	1	ПП	86	ИП4	С/П
ИП2	x<0	91	<->	/-/	<->	8	+
ИП5	*	ИП4	+	П4	ИП5	1	0	*	П5	В/О
```


''Note: the "-", "0", "+" denotes by digits, respectively, the "7", "8", "9" (or "9", "8", "7" if number is negative).''


## OCaml


```ocaml
type btdigit = Pos | Zero | Neg
type btern = btdigit list

let to_string n =
   String.concat ""
      (List.rev_map (function Pos -> "+" | Zero -> "0" | Neg -> "-") n)

let from_string s =
   let sl = ref [] in
   let digit = function '+' -> Pos | '-' -> Neg | '0' -> Zero
     | _ -> failwith "invalid digit" in
    String.iter (fun c -> sl := (digit c) :: !sl) s; !sl

let rec to_int = function
   | [Zero] | [] -> 0
   | Pos :: t -> 1 + 3 * to_int t
   | Neg :: t -> -1 + 3 * to_int t
   | Zero :: t -> 3 * to_int t

let rec from_int n =
   if n = 0 then [] else
   match n mod 3 with
      | 0 -> Zero :: from_int (n/3)
      | 1 | -2 -> Pos :: from_int ((n-1)/3)
      | 2 | -1 -> Neg :: from_int ((n+1)/3)

let rec (+~) n1 n2 = match (n1,n2) with
   | ([], a) | (a,[]) -> a
   | (Pos::t1, Neg::t2) | (Neg::t1, Pos::t2) | (Zero::t1, Zero::t2) ->
      let sum = t1 +~ t2 in if sum = [] then [] else Zero :: sum
   | (Pos::t1, Pos::t2) -> Neg :: t1 +~ t2 +~ [Pos]
   | (Neg::t1, Neg::t2) -> Pos :: t1 +~ t2 +~ [Neg]
   | (Zero::t1, h::t2) | (h::t1, Zero::t2) -> h :: t1 +~ t2

let neg = List.map (function Pos -> Neg | Neg -> Pos | Zero -> Zero)
let (-~) a b = a +~ (neg b)

let rec ( *~) n1 = function
   | [] -> []
   | [Pos] -> n1
   | [Neg] -> neg n1
   | Pos::t -> (Zero :: t *~ n1) +~ n1
   | Neg::t -> (Zero :: t *~ n1) -~ n1
   | Zero::t -> Zero :: t *~ n1

let a = from_string "+-0++0+"
let b = from_int (-436)
let c = from_string "+-++-"
let d = a *~ (b -~ c)
let _ =
  Printf.printf "a = %d\nb = %d\nc = %d\na * (b - c) = %s = %d\n"
   (to_int a) (to_int b) (to_int c) (to_string d) (to_int d);
```

Output:

```txt
a = 523
b = -436
c = 65
a * (b - c) = ----0+--0++0 = -262023
```



## Perl


```perl
use strict;
use warnings;

my @d = qw( 0 + - );
my @v = qw( 0 1 -1 );

sub to_bt {
  my $n = shift;
  my $b = '';
  while( $n ) {
    my $r = $n%3;
    $b .= $d[$r];
    $n -= $v[$r];
    $n /= 3;
  }
  return scalar reverse $b;
}

sub from_bt {
  my $n = 0;
  for( split //, shift ) { # Horner
    $n *= 3;
    $n += "${_}1" if $_;
  }
  return $n;
}

my %addtable = (
                '-0' => [ '-', '' ],
                '+0' => [ '+', '' ],
                '+-' => [ '0', '' ],
                '00' => [ '0', '' ],
                '--' => [ '+', '-' ],
                '++' => [ '-', '+' ],
               );

sub add {
  my ($b1, $b2) = @_;
  return ($b1 or $b2 ) unless ($b1 and $b2);
  my $d = $addtable{ join '', sort substr( $b1, -1, 1, '' ), substr( $b2, -1, 1, '' ) };
  return add( add($b1, $d->[1]), $b2 ).$d->[0];
}

sub unary_minus {
  my $b = shift;
  $b =~ tr/-+/+-/;
  return $b;
}

sub subtract {
  my ($b1, $b2) = @_;
  return add( $b1, unary_minus $b2 );
}

sub mult {
  my ($b1, $b2) = @_;
  my $r = '0';
  for( reverse split //, $b2 ){
    $r = add $r, $b1      if $_ eq '+';
    $r = subtract $r, $b1 if $_ eq '-';
    $b1 .= '0';
  }
  $r =~ s/^0+//;
  return $r;
}

my $a = "+-0++0+";
my $b = to_bt( -436 );
my $c = "+-++-";
my $d = mult( $a, subtract( $b, $c ) );
printf "      a: %14s %10d\n", $a, from_bt( $a );
printf "      b: %14s %10d\n", $b, from_bt( $b );
printf "      c: %14s %10d\n", $c, from_bt( $c );
printf "a*(b-c): %14s %10d\n", $d, from_bt( $d );

```

{{out}}

```txt
      a:        +-0++0+        523
      b:        -++-0--       -436
      c:          +-++-         65
a*(b-c):   ----0+--0++0    -262023

```


## Perl 6

{{Works with|rakudo|2017.01}}

```perl6
class BT {
    has @.coeff;

    my %co2bt = '-1' => '-', '0' => '0', '1' => '+';
    my %bt2co = %co2bt.invert;

    multi method new (Str $s) {
	self.bless(coeff => %bt2co{$s.flip.comb});
    }
    multi method new (Int $i where $i >= 0) {
	self.bless(coeff => carry $i.base(3).comb.reverse);
    }
    multi method new (Int $i where $i < 0) {
	self.new(-$i).neg;
    }

    method Str () { %co2bt{@!coeff}.join.flip }
    method Int () { [+] @!coeff Z* (1,3,9...*) }

    multi method neg () {
	self.new: coeff => carry self.coeff X* -1;
    }
}

sub carry (*@digits is copy) {
    loop (my $i = 0; $i < @digits; $i++) {
	while @digits[$i] < -1 { @digits[$i] += 3; @digits[$i+1]--; }
	while @digits[$i] > 1  { @digits[$i] -= 3; @digits[$i+1]++; }
    }
    pop @digits while @digits and not @digits[*-1];
    @digits;
}

multi prefix:<-> (BT $x) { $x.neg }

multi infix:<+> (BT $x, BT $y) {
    my ($b,$a) = sort +*.coeff, ($x, $y);
    BT.new: coeff => carry ($a.coeff Z+ |$b.coeff, |(0 xx $a.coeff - $b.coeff));
}

multi infix:<-> (BT $x, BT $y) { $x + $y.neg }

multi infix:<*> (BT $x, BT $y) {
    my @x = $x.coeff;
    my @y = $y.coeff;
    my @z = 0 xx @x+@y-1;
    my @safe;
    for @x -> $xd {
	@z = @z Z+ |(@y X* $xd), |(0 xx @z-@y);
	@safe.push: @z.shift;
    }
    BT.new: coeff => carry @safe, @z;
}

my $a = BT.new: "+-0++0+";
my $b = BT.new: -436;
my $c = BT.new: "+-++-";
my $x = $a * ( $b - $c );

say 'a == ', $a.Int;
say 'b == ', $b.Int;
say 'c == ', $c.Int;
say "a × (b − c) == ", ~$x, ' == ', $x.Int;
```

{{out}}

```txt
a == 523
b == -436
c == 65
a × (b − c) == ----0+--0++0 == -262023
```



## Phix

Using strings to represent balanced ternary. Note that as implemented dec2bt and bt2dec are limited to Phix integers (~+/-1,000,000,000),
but it would probably be pretty trivial (albeit quite a bit slower) to replace them with (say) ba2bt and bt2ba which use/yield bigatoms.

```Phix
function bt2dec(string bt)
integer res = 0
    for i=1 to length(bt) do
        res = 3*res+(bt[i]='+')-(bt[i]='-')
    end for
    return res
end function

function negate(string bt)
    for i=1 to length(bt) do
        if bt[i]!='0' then
            bt[i] = '+'+'-'-bt[i]
        end if
    end for
    return bt
end function

function dec2bt(integer n)
string res = "0"
integer neg, r
    if n!=0 then
        neg = n<0
        if neg then n = -n end if
        res = ""
        while n!=0 do
            r = mod(n,3)
            res = "0+-"[r+1]&res
            n = floor((n+(r=2))/3)
        end while
        if neg then res = negate(res) end if
    end if
    return res
end function

-- res,carry for a+b+carry lookup tables (not the fastest way to do it, I'm sure):
constant {tadd,addres} = columnize({{"---","0-"},{"--0","+-"},{"--+","-0"},
                                    {"-0-","+-"},{"-00","-0"},{"-0+","00"},
                                    {"-+-","-0"},{"-+0","00"},{"-++","+0"},
                                    {"0--","+-"},{"0-0","-0"},{"0-+","00"},
                                    {"00-","-0"},{"000","00"},{"00+","+0"},
                                    {"0+-","00"},{"0+0","+0"},{"0++","-+"},
                                    {"+--","-0"},{"+-0","00"},{"+-+","+0"},
                                    {"+0-","00"},{"+00","+0"},{"+0+","-+"},
                                    {"++-","+0"},{"++0","-+"},{"+++","0+"}})


function bt_add(string a, string b)
integer pad = length(a)-length(b)
integer carry = '0'
    if pad!=0 then
        if pad<0 then
            a = repeat('0',-pad)&a
        else
            b = repeat('0',pad)&b
        end if
    end if
    for i=length(a) to 1 by -1 do
        {a[i],carry} = addres[find(a[i]&b[i]&carry,tadd)]
    end for
    if carry!='0' then
        a = carry&a
    else
        while length(a)>1 and a[1]='0' do
            a = a[2..$]
        end while
    end if
    return a
end function

function bt_mul(string a, string b)
string pos = a, neg = negate(a), res = "0"
integer ch
    for i=length(b) to 1 by -1 do
        ch = b[i]
        if ch='+' then
            res = bt_add(res,pos)
        elsif ch='-' then
            res = bt_add(res,neg)
        end if
        pos = pos&'0'
        neg = neg&'0'
    end for
    return res
end function

string a = "+-0++0+", b = dec2bt(-436), c = "+-++-"

?{bt2dec(a),bt2dec(b),bt2dec(c)}

string res = bt_mul(a,bt_add(b,negate(c)))
?{res,bt2dec(res)}
```

{{out}}

```txt

{523,-436,65}
{"----0+--0++0",-262023}

```

Proof of arbitrary large value support is provided by calculating 1000! and 999! and using a naive subtraction loop to effect division.
The limit for factorials that can be held in native integers is a mere 12, and for atoms 170, mind you, inaccurate above 22. The timings
show it manages a 5000+digit multiplication and subtraction in about 0.2s, which I say is "reasonable", given that I didn't try very hard,
as evidenced by that daft addition lookup table!

```Phix
atom t0 = time()
string f999 = dec2bt(1)
for i=2 to 999 do
    f999 = bt_mul(f999,dec2bt(i))
end for
string f1000 = bt_mul(f999,dec2bt(1000))

printf(1,"In balanced ternary, f999 has %d digits and f1000 has %d digits\n",{length(f999),length(f1000)})

integer count = 0
f999 = negate(f999)
while f1000!="0" do
    f1000 = bt_add(f1000,f999)
    count += 1
end while
printf(1,"It took %d subtractions to reach 0. (%3.2fs)\n",{count,time()-t0})
```

{{out}}

```txt

In balanced ternary, f999 has 5376 digits and f1000 has 5383 digits
It took 1000 subtractions to reach 0. (9.30s)

```



## PicoLisp


```PicoLisp
(seed (in "/dev/urandom" (rd 8)))

(setq *G '((0 -1) (1 -1) (-1 0) (0 0) (1 0) (-1 1) (0 1)))

# For humans
(de negh (L)
   (mapcar
      '((I)
         (case I
            (- '+)
            (+ '-)
            (T 0) ) )
      L ) )

(de trih (X)
   (if (num? X)
      (let (S (lt0 X)  X (abs X)  R NIL)
         (if (=0 X)
            (push 'R 0)
            (until (=0 X)
               (push 'R
                  (case (% X 3)
                     (0 0)
                     (1 '+)
                     (2 (inc 'X) '-) ) )
               (setq X (/ X 3)) ) )
         (if S (pack (negh R)) (pack R)) )
      (let M 1
         (sum
            '((C)
               (prog1
                  (unless (= C "0") ((intern C) M))
                  (setq M (* 3 M)) ) )
            (flip (chop X)) ) ) ) )

# For robots
(de neg (L)
   (mapcar
      '((I)
         (case I (-1 1) (1 -1) (T 0)) )
      L ) )

(de tri (X)
   (if (num? X)
      (let (S (lt0 X)  X (abs X)  R NIL)
         (if (=0 X)
            (push 'R 0)
            (until (=0 X)
               (push 'R
                  (case (% X 3)
                     (0 0)
                     (1 1)
                     (2 (inc 'X) (- 1)) ) )
               (setq X (/ X 3)) ) )
         (flip (if S (neg R) R)) )
      (let M 1
         (sum
            '((C)
               (prog1 (* C M) (setq M (* 3 M))) )
            X ) ) ) )

(de add (D1 D2)
   (let
      (L (max (length D1) (length D2))
         D1 (need (- L) D1 0)
         D2 (need (- L) D2 0)
         C 0 )
      (mapcon
         '((L1 L2)
            (let R
               (get
                  *G
                  (+ 4 (+ (car L1) (car L2) C)) )
               (ifn (cdr L1)
                  R
                  (setq C (cadr R))
                  (cons (car R)) ) ) )
         D1
         D2 ) ) )

(de mul (D1 D2)
   (ifn (and D1 D2)
      0
      (add
         (case (car D1)
            (0 0)
            (1 D2)
            (-1 (neg D2)) )
         (cons 0 (mul (cdr D1) D2) ) ) ) )

(de sub (D1 D2)
   (add D1 (neg D2)) )

# Random testing
(let (X 0  Y 0  C 2048)
   (do C
      (setq
         X (rand (- C) C)
         Y (rand (- C) C) )
      (test X (trih (trih X)))
      (test X (tri (tri X)))
      (test
         (+ X Y)
         (tri (add (tri X) (tri Y))) )
      (test
         (- X Y)
         (tri (sub (tri X) (tri Y))) )
      (test
         (* X Y)
         (tri (mul (tri X) (tri Y))) ) ) )

(println 'A (trih 523) (trih "+-0++0+"))
(println 'B (trih -436) (trih "-++-0--"))
(println 'C (trih 65) (trih "+-++-"))
(let R
   (tri
      (mul
         (tri (trih "+-0++0+"))
         (sub (tri -436) (tri (trih "+-++-"))) ) )
   (println 'R (trih R) R) )

(bye)
```



## Prolog

Works with SWI-Prolog and library '''clpfd''' written by '''Markus Triska'''.

Three modules, one for the conversion, one for the addition and one for the multiplication.


'''The conversion.'''

Library '''clpfd''' is used so that '''bt_convert''' works in both ways Decimal => Ternary and Ternary ==> Decimal.

```Prolog
:- module('bt_convert.pl', [bt_convert/2,
			    op(950, xfx, btconv),
			    btconv/2]).

:- use_module(library(clpfd)).

:- op(950, xfx, btconv).

X btconv Y :-
	bt_convert(X, Y).

% bt_convert(?X, ?L)
bt_convert(X, L) :-
	(   (nonvar(L), \+is_list(L)) ->string_to_list(L, L1);  L1 = L),
	convert(X, L1),
	(   var(L) -> string_to_list(L, L1); true).

% map numbers toward digits +, - 0
plus_moins( 1, 43).
plus_moins(-1, 45).
plus_moins( 0, 48).


convert(X, [48| L]) :-
	var(X),
	(   L \= [] -> convert(X, L); X = 0, !).

convert(0, L) :-
	var(L), !, string_to_list(L, [48]).

convert(X, L) :-
	(   (nonvar(X), X > 0)
	;   (var(X), X #> 0,
	    L = [43|_],
	     maplist(plus_moins, L1, L))),
	!,
	convert(X, 0, [], L1),
	(   nonvar(X) -> maplist(plus_moins, L1, LL),  string_to_list(L, LL)
	;   true).

convert(X, L) :-
	(  nonvar(X) -> Y is -X
	;  X #< 0,
	   maplist(plus_moins, L2, L),
	   maplist(mult(-1), L2, L1)),
	convert(Y, 0, [], L1),
	(   nonvar(X) ->
	    maplist(mult(-1), L1, L2),
	    maplist(plus_moins, L2, LL),
            string_to_list(L, LL)
	;   X #= -Y).

mult(X, Y, Z) :-
	Z #= X * Y.


convert(0, 0, L, L) :-  !.

convert(0, 1, L, [1 | L]) :- !.


convert(N, C, LC, LF) :-
	R #= N mod 3 + C,
	R #> 1 #<==> C1,
	N1 #= N / 3,
	R1 #= R - 3 * C1, % C1 #= 1,
	convert(N1, C1, [R1 | LC], LF).

```


'''The addition.'''

The same predicate is used for addition and substraction.

```Prolog
:- module('bt_add.pl', [bt_add/3,
			bt_add1/3,
			op(900, xfx, btplus),
			op(900, xfx, btmoins),
			btplus/2,
			btmoins/2,
			strip_nombre/3
		       ]).

:- op(900, xfx, btplus).
:- op(900, xfx, btmoins).

% define operator btplus
A is X btplus Y :-
	bt_add(X, Y, A).

% define operator btmoins
% no need to define a predicate for the substraction
A is X btmoins Y :-
       X is Y btplus A.


% bt_add(?X, ?Y, ?R)
% R is X + Y
% X, Y, R are strings
% At least 2 args must be instantiated
bt_add(X, Y, R) :-
	(   nonvar(X) -> string_to_list(X, X1);  true),
	(   nonvar(Y) -> string_to_list(Y, Y1);  true),
	(   nonvar(R) -> string_to_list(R, R1);  true),
	bt_add1(X1, Y1, R1),
	(   var(X) -> string_to_list(X, X1); true),
	(   var(Y) -> string_to_list(Y, Y1); true),
	(   var(R) -> string_to_list(R, R1); true).



% bt_add1(?X, ?Y, ?R)
% R is X + Y
% X, Y, R are lists
bt_add1(X, Y, R) :-
	% initialisation :  X and Y must have the same length
	% we add zeros at the beginning of the shortest list
	(   nonvar(X) -> length(X, LX);  length(R, LR)),
	(   nonvar(Y) ->  length(Y, LY);  length(R, LR)),
	(   var(X) -> LX is max(LY, LR) , length(X1, LX), Y1 = Y ; X1 = X),
	(   var(Y) -> LY is max(LX, LR) , length(Y1, LY), X1 = X ; Y1 = Y),

	Delta is abs(LX - LY),
	(   LX < LY -> normalise(Delta, X1, X2), Y1 = Y2
	;   LY < LX -> normalise(Delta, Y1, Y2), X1 = X2
	;   X1 = X2, Y1 = Y2),


	% if R is instancied, it must have, at least, the same length than X or Y
	Max is max(LX, LY),
	(   (nonvar(R), length(R, LR), LR < Max) -> Delta1 is Max - LR, normalise(Delta1, R, R2)
	;   nonvar(R) -> R = R2
	;   true),

	 bt_add(X2, Y2, C, R2),

	(   C = 48 -> strip_nombre(R2, R, []),
	              (	  var(X) -> strip_nombre(X2, X, []) ; true),
	              (	  var(Y) -> strip_nombre(Y2, Y, []) ; true)
	;   var(R) -> strip_nombre([C|R2], R, [])
	;   ( select(C, [45,43], [Ca]),
	    ( var(X) -> strip_nombre([Ca | X2], X, [])
	    ;	strip_nombre([Ca | Y2], Y, [])))).


% here we actually compute the sum
bt_add([], [], 48, []).

bt_add([H1|T1], [H2|T2], C3, [R2 | L]) :-
	bt_add(T1, T2, C, L),
	% add HH1 and H2
	ternary_sum(H1, H2, R1, C1),
	% add first carry,
	ternary_sum(R1, C, R2, C2),
	% add second carry
	ternary_sum(C1, C2, C3, _).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ternary_sum
% @arg1 : V1
% @arg2 : V2
% @arg3 : R is V1 + V2
% @arg4 : Carry
ternary_sum(43, 43, 45, 43).

ternary_sum(43, 45, 48, 48).

ternary_sum(45, 43, 48, 48).

ternary_sum(45, 45, 43, 45).

ternary_sum(X, 48, X, 48).

ternary_sum(48, X, X, 48).


% if L has a length smaller than N, complete L with 0 (code 48)
normalise(0, L, L) :- !.
normalise(N, L1, L) :-
	N1 is N - 1,
	normalise(N1, [48 | L1], L).


% contrary of normalise
% remove leading zeros.
% special case of number 0 !
strip_nombre([48]) --> {!}, "0".

% enlève les zéros inutiles
strip_nombre([48 | L]) -->
	strip_nombre(L).


strip_nombre(L) -->
	L.

```

'''The multiplication.'''

We give a predicate '''euclide(?A, +B, ?Q, ?R)''' which computes both the multiplication and the division, but it is very inefficient.

The predicates '''multiplication(+B, +Q, -A)''' and '''division(+A, +B, -Q, -R)''' are much more efficient.

```Prolog
:- module('bt_mult.pl', [op(850, xfx, btmult),
			 btmult/2,
			 multiplication/3
			]).

:- use_module('bt_add.pl').

:- op(850, xfx, btmult).
A is B btmult C :-
	multiplication(B, C, A).

neg(A, B) :-
	maplist(opp, A, B).

opp(48, 48).
opp(45, 43).
opp(43, 45).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the multiplication (efficient)
% multiplication(+BIn, +QIn, -AOut)
% Aout is BIn * QIn
% BIn, QIn, AOut are strings
multiplication(BIn, QIn, AOut) :-
	string_to_list(BIn, B),
	string_to_list(QIn, Q),

	% We work with positive numbers
	(   B = [45 | _] -> Pos0 = false, neg(B,BP) ; BP = B, Pos0 = true),
	(   Q = [45 | _] -> neg(Q, QP), select(Pos0, [true, false], [Pos1]); QP = Q, Pos1 = Pos0),

	multiplication_(BP, QP, [48], A),
	(   Pos1 = false -> neg(A, A1); A1 = A),
	string_to_list(AOut, A1).


multiplication_(_B, [], A, A).

multiplication_(B, [H | T], A, AF) :-
	multiplication_1(B, H, B1),
	append(A, [48], A1),
	bt_add1(B1, A1, A2),
	multiplication_(B, T, A2, AF).

% by 1 (digit '+' code 43)
multiplication_1(B, 43, B).

% by 0 (digit '0' code 48)
multiplication_1(_, 48, [48]).

% by -1 (digit '-' code 45)
multiplication_1(B, 45, B1) :- neg(B, B1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the division (efficient)
% division(+AIn, +BIn, -QOut, -ROut)
%
division(AIn, BIn, QOut, ROut) :-
	string_to_list(AIn, A),
	string_to_list(BIn, B),
	length(B, LB),
	length(A, LA),
	Len is LA - LB,
	(   Len < 0 -> Q = [48], R = A
	;   neg(B, NegB), division_(A, B, NegB, LB, Len, [], Q, R)),
	string_to_list(QOut, Q),
	string_to_list(ROut, R).


division_(A, B, NegB, LenB, LenA, QC, QF, R) :-
	% if the remainder R is negative (last number A), we must decrease the quotient Q, annd add B to R
	(   LenA = -1 -> (A = [45 | _]  -> positive(A, B, QC, QF, R) ;	QF = QC, A = R)
	;   extract(LenA, _, A, AR, AF),
	    length(AR, LR),

	    (	LR >= LenB -> ( AR = [43 | _] ->
			        bt_add1(AR, NegB, S), Q0 = [43],
				% special case : R has the same length than B
				% and his first digit is + (1)
				% we must do another one substraction
				(   (length(S, LenB), S = [43|_]) ->
				                       bt_add1(S, NegB, S1),
				                       bt_add1(QC, [43], QC1),
				                       Q00 = [45]
				;   S1 = S, QC1 = QC, Q00 = Q0)


	                        ; bt_add1(AR, B, S1), Q00 = [45], QC1 = QC),
				append(QC1, Q00, Q1),
		                append(S1, AF, A1),
				strip_nombre(A1, A2, []),
				LenA1 is LenA - 1,
				division_(A2, B, NegB, LenB, LenA1, Q1, QF, R)

	    ;   append(QC, [48], Q1), LenA1 is LenA - 1,
	        division_(A, B, NegB, LenB, LenA1, Q1, QF, R))).

% extract(+Len, ?N1, +L, -Head, -Tail)
% remove last N digits from the list L
% put them in Tail.
extract(Len, Len, [], [], []).

extract(Len, N1, [H|T], AR1, AF1) :-
	extract(Len, N, T, AR, AF),
	N1 is N-1,
	(   N > 0 -> AR = AR1, AF1 = [H | AF]; AR1 = [H | AR], AF1 = AF).



positive(R, _, Q, Q, R) :- R = [43 | _].

positive(S, B, Q, QF, R ) :-
	bt_add1(S, B, S1),
	bt_add1(Q, [45], Q1),
	positive(S1, B, Q1, QF, R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% "euclidian" division (inefficient)
% euclide(?A, +BIn, ?Q, ?R)
% A = B * Q + R
euclide(A, B, Q, R) :-
	mult(A, B, Q, R).


mult(AIn, BIn, QIn, RIn) :-
	(   nonvar(AIn) -> string_to_list(AIn, A); A = AIn),
	(   nonvar(BIn) -> string_to_list(BIn, B); B = BIn),
	(   nonvar(QIn) -> string_to_list(QIn, Q); Q = QIn),
	(   nonvar(RIn) -> string_to_list(RIn, R); R = RIn),

	% we use positive numbers
	(   B = [45 | _] -> Pos0 = false, neg(B,BP) ; BP = B, Pos0 = true),
	(   (nonvar(Q), Q = [45 | _]) -> neg(Q, QP), select(Pos0, [true, false], [Pos1])
	;   nonvar(Q) -> Q = QP , Pos1 = Pos0
	;   Pos1 = Pos0),
	(   (nonvar(A), A = [45 | _]) -> neg(A, AP)
	;   nonvar(A) -> AP = A
	;   true),

	% is R instancied ?
	( nonvar(R) -> R1 = R; true),
	% multiplication ? we add B to A and substract 1 (digit '-') to Q
	(   nonvar(Q) -> BC = BP, Ajout = [45],
	    (	nonvar(R) ->  bt_add1(BC, R, AP) ; AP = BC)
	% division ? we substract B to A and add 1 (digit '+') to Q
	;    neg(BP, BC), Ajout = [43], QP = [48]),

	% do the real job
	mult_(BC, QP, AP, R1, Resultat, Ajout),

	(   var(QIn) -> (Pos1 = false -> neg(Resultat, QT); Resultat = QT), string_to_list(QIn, QT)
	;   true),
	(   var(AIn) -> (Pos1 = false -> neg(Resultat, AT); Resultat = AT), string_to_list(AIn, AT)
	;   true),
	(   var(RIn) -> string_to_list(RIn, R1); true).

% @arg1 : divisor
% @arg2 : quotient
% @arg3 : dividend
% @arg4 : remainder
% @arg5 : Result :  receive either the dividend A
%                           either the quotient Q
mult_(B, Q, A, R, Resultat, Ajout) :-
	bt_add1(Q, Ajout, Q1),
	bt_add1(A, B, A1),
	(  Q1 = [48] -> Resultat = A % a multiplication
	;  ( A1 = [45 | _], Ajout = [43]) -> Resultat = Q, R = A  % a division
	;  mult_(B, Q1, A1, R, Resultat, Ajout)) .


```

Example of output :

```txt
 ?- A btconv "+-0++0+".
A = 523.

 ?- -436 btconv B.
B = "-++-0--".

 ?- C btconv "+-++-".
C = 65.

 ?- X is "-++-0--" btmoins "+-++-", Y is "+-0++0+" btmult X, Z btconv Y.
X = "-+0-++0",
Y = "----0+--0++0",
Z = -262023 .

```


## Python

{{trans|Common Lisp}}

```python
class BalancedTernary:
    # Represented as a list of 0, 1 or -1s, with least significant digit first.

    str2dig = {'+': 1, '-': -1, '0': 0} # immutable
    dig2str = {1: '+', -1: '-', 0: '0'} # immutable
    table = ((0, -1), (1, -1), (-1, 0), (0, 0), (1, 0), (-1, 1), (0, 1)) # immutable

    def __init__(self, inp):
        if isinstance(inp, str):
            self.digits = [BalancedTernary.str2dig[c] for c in reversed(inp)]
        elif isinstance(inp, int):
            self.digits = self._int2ternary(inp)
        elif isinstance(inp, BalancedTernary):
            self.digits = list(inp.digits)
        elif isinstance(inp, list):
            if all(d in (0, 1, -1) for d in inp):
                self.digits = list(inp)
            else:
                raise ValueError("BalancedTernary: Wrong input digits.")
        else:
            raise TypeError("BalancedTernary: Wrong constructor input.")

    @staticmethod
    def _int2ternary(n):
        if n == 0: return []
        if (n % 3) == 0: return [0] + BalancedTernary._int2ternary(n // 3)
        if (n % 3) == 1: return [1] + BalancedTernary._int2ternary(n // 3)
        if (n % 3) == 2: return [-1] + BalancedTernary._int2ternary((n + 1) // 3)

    def to_int(self):
        return reduce(lambda y,x: x + 3 * y, reversed(self.digits), 0)

    def __repr__(self):
        if not self.digits: return "0"
        return "".join(BalancedTernary.dig2str[d] for d in reversed(self.digits))

    @staticmethod
    def _neg(digs):
        return [-d for d in digs]

    def __neg__(self):
        return BalancedTernary(BalancedTernary._neg(self.digits))

    @staticmethod
    def _add(a, b, c=0):
        if not (a and b):
            if c == 0:
                return a or b
            else:
                return BalancedTernary._add([c], a or b)
        else:
            (d, c) = BalancedTernary.table[3 + (a[0] if a else 0) + (b[0] if b else 0) + c]
            res = BalancedTernary._add(a[1:], b[1:], c)
            # trim leading zeros
            if res or d != 0:
                return [d] + res
            else:
                return res

    def __add__(self, b):
        return BalancedTernary(BalancedTernary._add(self.digits, b.digits))

    def __sub__(self, b):
        return self + (-b)

    @staticmethod
    def _mul(a, b):
        if not (a and b):
            return []
        else:
            if   a[0] == -1: x = BalancedTernary._neg(b)
            elif a[0] ==  0: x = []
            elif a[0] ==  1: x = b
            else: assert False
            y = [0] + BalancedTernary._mul(a[1:], b)
            return BalancedTernary._add(x, y)

    def __mul__(self, b):
        return BalancedTernary(BalancedTernary._mul(self.digits, b.digits))


def main():
    a = BalancedTernary("+-0++0+")
    print "a:", a.to_int(), a

    b = BalancedTernary(-436)
    print "b:", b.to_int(), b

    c = BalancedTernary("+-++-")
    print "c:", c.to_int(), c

    r = a * (b - c)
    print "a * (b - c):", r.to_int(), r

main()
```

{{out}}

```txt
a: 523 +-0++0+
b: -436 -++-0--
c: 65 +-++-
a * (b - c): -262023 ----0+--0++0
```



## Racket


```racket
#lang racket

;; Represent a balanced-ternary number as a list of 0's, 1's and -1's.
;;
;; e.g. 11 = 3^2 + 3^1 - 3^0 ~ "++-" ~ '(-1 1 1)
;;       6 = 3^2 - 3^1       ~ "+-0" ~ '(0 -1 1)
;;
;; Note: the list-rep starts with the least signifcant tert, while
;;       the string-rep starts with the most significsnt tert.

(define (bt->integer t)
  (if (null? t)
      0
      (+ (first t) (* 3 (bt->integer (rest t))))))

(define (integer->bt n)
  (letrec ([recur (λ (b r) (cons b (convert (floor (/ r 3)))))]
           [convert (λ (n) (if (zero? n) null
                               (case (modulo n 3)
                                 [(0) (recur 0 n)]
                                 [(1) (recur 1 n)]
                                 [(2) (recur -1 (add1 n))])))])
    (convert n)))

(define (bt->string t)
  (define (strip-leading-zeroes a)
    (if (or (null? a) (not (= (first a) 0))) a (strip-leading-zeroes (rest a))))
  (string-join (map (λ (u)
                      (case u
                        [(1) "+"]
                        [(-1) "-"]
                        [(0) "0"]))
                    (strip-leading-zeroes (reverse t))) ""))

(define (string->bt s)
  (reverse
   (map (λ (c)
          (case c
            [(#\+) 1]
            [(#\-) -1]
            [(#\0) 0]))
        (string->list s))))

(define (bt-negate t)
  (map (λ (u) (- u)) t))

(define (bt-add a b [c 0])
  (cond [(and (null? a) (null? b)) (if (zero? c) null (list c))]
        [(null? b) (if (zero? c) a (bt-add a (list c)))]
        [(null? a) (bt-add b a c)]
        [else (let* ([t (+ (first a) (first b) c)]
                     [carry (if (> (abs t) 1) (sgn t) 0)]
                     [v (case (abs t)
                          [(3) 0]
                          [(2) (- (sgn t))]
                          [else t])])
                (cons v (bt-add (rest a) (rest b) carry)))]))

(define (bt-multiply a b)
  (cond [(null? a) null]
        [(null? b) null]
        [else (bt-add (case (first a)
                        [(-1) (bt-negate b)]
                        [(0) null]
                        [(1) b])
                      (cons 0 (bt-multiply (rest a) b)))]))

; test case
(let* ([a (string->bt "+-0++0+")]
       [b (integer->bt -436)]
       [c (string->bt "+-++-")]
       [d (bt-multiply a (bt-add b (bt-negate c)))])
  (for ([bt (list a b c d)]
        [description (list 'a 'b 'c "a×(b−c)")])
    (printf "~a = ~a or ~a\n" description (bt->integer bt) (bt->string bt))))

```


{{output}}

```txt

a = 523 or +-0++0+
b = -436 or -++-0--
c = 65 or +-++-
a×(b−c) = -262023 or ----0+--0++0

```



## REXX

The REXX program could be optimized by using   (procedure) with   '''expose'''   and having the   <big>'''$.'''</big>   and   <big>'''@.'''</big>   variables set only once.

```rexx
/*REXX program converts decimal  ◄───►  balanced ternary;  it also performs arithmetic. */
numeric digits 10000                             /*be able to handle  gihugic  numbers. */
Ao = '+-0++0+'  ;    Abt =      Ao               /*   [↓]  2 literals used by subroutine*/
Bo =    '-436'  ;    Bbt = d2bt(Bo);                   @ = '(decimal)'
Co =   '+-++-'  ;    Cbt =      Co ;                  @@ = 'balanced ternary ='
                call btShow  '[a]',        Abt
                call btShow  '[b]',        Bbt
                call btShow  '[c]',        Cbt
                say;                       $bt = btMul(Abt, btSub(Bbt, Cbt) )
                call btShow '[a*(b-c)]',   $bt
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
d2bt: procedure; parse arg x 1;     p=0;     $.='-';      $.1='+';      $.0=0;       #=
      x=x / 1
                   do  until x==0; _= (x // (3** (p+1) ) )  %  3**p
                   if _== 2  then _= -1;    else if _== -2  then _=1
                   x=x  -  _ * (3**p);          p=p+1;                  #=$._ || #
                   end   /*until*/;             return #
/*──────────────────────────────────────────────────────────────────────────────────────*/
bt2d: procedure; parse arg x;  r=reverse(x);    #=0;   $.=-1;   $.0=0;   _='+';   $._=1
        do j=1  for length(x); _=substr(r,j,1); #= # + $._ * 3**(j-1);  end;      return #
/*──────────────────────────────────────────────────────────────────────────────────────*/
btAdd: procedure; parse arg x,y;    rx=reverse(x);      ry=reverse(y);            carry=0
       @.=0  ;  _='-';   @._= -1;   _="+";              @._=1
       $.='-';  $.0=0;   $.1= '+'
       #=;                                    do j=1  for max( length(x), length(y) )
                                              x_=substr(rx, j, 1);            xn=@.x_
                                              y_=substr(ry, j, 1);            yn=@.y_
                                              s=xn + yn + carry;           carry= 0
                                              if s== 2  then do;  s=-1;    carry= 1;   end
                                              if s== 3  then do;  s= 0;    carry= 1;   end
                                              if s==-2  then do;  s= 1;    carry=-1;   end
                                              #=$.s || #
                                              end   /*j*/
       if carry\==0  then #=$.carry || #;                         return btNorm(#)
/*──────────────────────────────────────────────────────────────────────────────────────*/
btMul: procedure; parse arg x 1 x1 2, y 1 y1 2;       if x==0 | y==0  then return 0
       S=1;              x=btNorm(x);     y=btNorm(y)            /*handle: 0-xxx values.*/
       if x1=='-'              then do;   x=btNeg(x); S=-S; end  /*positate the number. */
       if y1=='-'              then do;   y=btNeg(y); S=-S; end  /*    "     "    "     */
       if length(y)>length(x)  then parse value  x y  with  y x  /*optimize  "    "     */
       P=0
                                              do   until  y==0   /*keep adding 'til done*/
                                              P=btAdd(P, x)      /*multiple the hard way*/
                                              y=btSub(y, '+')    /*subtract  1  from  Y.*/
                                              end   /*until*/
       if S==-1  then P=btNeg(P);  return P         /*adjust the product sign;  return. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
btNeg:  return translate(arg(1), '-+', "+-")                     /*negate bal_ternary #.*/
btNorm: _=strip(arg(1),'L',0);  if _==''  then _=0;  return _    /*normalize the number.*/
btSub:  return btAdd(arg(1), btNeg(arg(2)))                      /*subtract two BT args.*/
btShow: say center(arg(1),9) right(arg(2),20) @@ right(bt2d(arg(2)),9) @;           return
```

{{out|output|text=  when using the default input:}}

```txt

   [a]                 +-0++0+ balanced ternary =       523 (decimal)
   [b]                 -++-0-- balanced ternary =      -436 (decimal)
   [c]                   +-++- balanced ternary =        65 (decimal)

[a*(b-c)]         ----0+--0++0 balanced ternary =   -262023 (decimal)

```



## Ruby


```ruby
class BalancedTernary
  include Comparable
  def initialize(str = "")
    if str =~ /[^-+0]+/
      raise ArgumentError, "invalid BalancedTernary number: #{str}"
    end
    @digits = trim0(str)
  end

  I2BT = {0 => ["0",0], 1 => ["+",0], 2 => ["-",1]}
  def self.from_int(value)
    n = value.to_i
    digits = ""
    while n != 0
      quo, rem = n.divmod(3)
      bt, carry = I2BT[rem]
      digits = bt + digits
      n = quo + carry
    end
    new(digits)
  end

  BT2I = {"-" => -1, "0" => 0, "+" => 1}
  def to_int
    @digits.chars.inject(0) do |sum, char|
      sum = 3 * sum + BT2I[char]
    end
  end
  alias :to_i :to_int

  def to_s
    @digits.dup                 # String is mutable
  end
  alias :inspect :to_s

  def <=>(other)
    to_i <=> other.to_i
  end

  ADDITION_TABLE = {
    "---" => ["-","0"], "--0" => ["-","+"], "--+" => ["0","-"],
    "-0-" => ["-","+"], "-00" => ["0","-"], "-0+" => ["0","0"],
    "-+-" => ["0","-"], "-+0" => ["0","0"], "-++" => ["0","+"],
    "0--" => ["-","+"], "0-0" => ["0","-"], "0-+" => ["0","0"],
    "00-" => ["0","-"], "000" => ["0","0"], "00+" => ["0","+"],
    "0+-" => ["0","0"], "0+0" => ["0","+"], "0++" => ["+","-"],
    "+--" => ["0","-"], "+-0" => ["0","0"], "+-+" => ["0","+"],
    "+0-" => ["0","0"], "+00" => ["0","+"], "+0+" => ["+","-"],
    "++-" => ["0","+"], "++0" => ["+","-"], "+++" => ["+","0"],
  }

  def +(other)
    maxl = [to_s.length, other.to_s.length].max
    a = pad0_reverse(to_s, maxl)
    b = pad0_reverse(other.to_s, maxl)
    carry = "0"
    sum = a.zip( b ).inject("") do |sum, (c1, c2)|
      carry, digit = ADDITION_TABLE[carry + c1 + c2]
      sum = digit + sum
    end
    self.class.new(carry + sum)
  end

  MULTIPLICATION_TABLE = {
    "-" => "+0-",
    "0" => "000",
    "+" => "-0+",
  }

  def *(other)
    product = self.class.new
    other.to_s.each_char do |bdigit|
      row = to_s.tr("-0+", MULTIPLICATION_TABLE[bdigit])
      product += self.class.new(row)
      product << 1
    end
    product >> 1
  end

  # negation
  def -@()
    self.class.new(@digits.tr('-+','+-'))
  end

  # subtraction
  def -(other)
    self + (-other)
  end

  # shift left
  def <<(count)
    @digits = trim0(@digits + "0"*count)
    self
  end

  # shift right
  def >>(count)
    @digits[-count..-1] = "" if count > 0
    @digits = trim0(@digits)
    self
  end

  private

  def trim0(str)
    str = str.sub(/^0+/, "")
    str = "0" if str.empty?
    str
  end

  def pad0_reverse(str, len)
    str.rjust(len, "0").reverse.chars
  end
end

a = BalancedTernary.new("+-0++0+")
b = BalancedTernary.from_int(-436)
c = BalancedTernary.new("+-++-")

%w[a b c a*(b-c)].each do |exp|
  val = eval(exp)
  puts "%8s :%13s,%8d" % [exp, val, val.to_i]
end
```


{{out}}

```txt

       a :      +-0++0+,     523
       b :      -++-0--,    -436
       c :        +-++-,      65
 a*(b-c) : ----0+--0++0, -262023

```



## Scala

This implementation represents ternaries as a reversed list of bits. Also, there are plenty of implicit convertors

```scala

object TernaryBit {
    val P = TernaryBit(+1)
    val M = TernaryBit(-1)
    val Z = TernaryBit( 0)

  implicit def asChar(t: TernaryBit): Char = t.charValue
  implicit def valueOf(c: Char): TernaryBit = {
    c match {
      case '0' => 0
      case '+' => 1
      case '-' => -1
      case nc => throw new IllegalArgumentException("Illegal ternary symbol " + nc)
    }
  }
  implicit def asInt(t: TernaryBit): Int = t.intValue
  implicit def valueOf(i: Int): TernaryBit = TernaryBit(i)
}

case class TernaryBit(val intValue: Int) {

    def inverse: TernaryBit = TernaryBit(-intValue)

    def charValue = intValue match {
      case  0 => '0'
      case  1 => '+'
      case -1 => '-'
    }
}

class Ternary(val bits: List[TernaryBit]) {

  def + (b: Ternary) = {
    val sumBits: List[Int] = bits.map(_.intValue).zipAll(b.bits.map(_.intValue), 0, 0).map(p => p._1 + p._2)

    // normalize
    val iv: Tuple2[List[Int], Int] = (List(), 0)
    val (revBits, carry) = sumBits.foldLeft(iv)((accu: Tuple2[List[Int], Int], e: Int) => {
      val s = e + accu._2
      (((s + 1 + 3 * 100) % 3 - 1) :: accu._1 , (s + 1 + 3 * 100) / 3 - 100)
    })

    new Ternary(( TernaryBit(carry) :: revBits.map(TernaryBit(_))).reverse )
  }

  def - (b: Ternary) = {this + (-b)}
  def <<<(a: Int): Ternary = { List.fill(a)(TernaryBit.Z) ++ bits}
  def >>>(a: Int): Ternary = { bits.drop(a) }
  def unary_- = { bits.map(_.inverse) }

  def ** (b: TernaryBit): Ternary = {
    b match {
      case TernaryBit.P => this
      case TernaryBit.M => - this
      case TernaryBit.Z => 0
    }
  }

  def * (mul: Ternary): Ternary = {
    // might be done more efficiently - perform normalize only once
    mul.bits.reverse.foldLeft(new Ternary(Nil))((a: Ternary, b: TernaryBit) => (a <<< 1) + (this ** b))
  }

  def intValue = bits.foldRight(0)((c, a) => a*3 + c.intValue)

  override def toString = new String(bits.reverse.map(_.charValue).toArray)
}

object Ternary {

  implicit def asString(t: Ternary): String = t.toString()
  implicit def valueOf(s: String): Ternary = new Ternary(s.toList.reverse.map(TernaryBit.valueOf(_)))

  implicit def asBits(t: Ternary): List[TernaryBit] = t.bits
  implicit def valueOf(l: List[TernaryBit]): Ternary = new Ternary(l)

  implicit def asInt(t: Ternary): BigInt = t.intValue
  // XXX not tail recursive
  implicit def valueOf(i: BigInt): Ternary = {
    if (i < 0) -valueOf(-i)
    else if (i == 0) new Ternary(List())
    else if (i % 3 == 0) TernaryBit.Z :: valueOf(i / 3)
    else if (i % 3 == 1) TernaryBit.P :: valueOf(i / 3)
    else /*(i % 3 == 2)*/ TernaryBit.M :: valueOf((i + 1)  / 3)
  }
  implicit def intToTernary(i: Int): Ternary = valueOf(i)
}

```


Then these classes can be used in the following way:

```scala

object Main {

  def main(args: Array[String]): Unit = {
    val a: Ternary = "+-0++0+"
    val b: Ternary = -436
    val c: Ternary = "+-++-"
    println(a.toString + " " + a.intValue)
    println(b.toString + " " + b.intValue)
    println(c.toString + " " + c.intValue)
    val res = a * (b - c)
    println(res.toString + " " + res.intValue)
  }

}

```

{{out}}

```txt

+-0++0+ 523
-++-0-- -436
+-++- 65
00000000----0+--0++0 -262023

```


Besides, we can easily check, that the code works for any input. This can be achieved with ScalaCheck:

```scala

object TernarySpecification extends Properties("Ternary") {

  property("sum") = forAll { (a: Int, b: Int) =>
    val at: Ternary = a
    val bt: Ternary = b
    (at+bt).intValue == (at.intValue + bt.intValue)
  }

  property("multiply") = forAll { (a: Int, b: Int) =>
    val at: Ternary = a
    val bt: Ternary = b
    (at*bt).intValue == (at.intValue * bt.intValue)
  }

}

```

{{out}}

```txt

+ Ternary.sum: OK, passed 100 tests.

+ Ternary.multiply: OK, passed 100 tests.

```


## Tcl

This directly uses the printable representation of the balanced ternary numbers, as Tcl's string operations are reasonably efficient.

```tcl
package require Tcl 8.5

proc bt-int b {
    set n 0
    foreach c [split $b ""] {
	set n [expr {$n * 3}]
	switch -- $c {
	    + { incr n 1  }
	    - { incr n -1 }
	}
    }
    return $n
}
proc int-bt n {
    if {$n == 0} {
	return "0"
    }
    while {$n != 0} {
	lappend result [lindex {0 + -} [expr {$n % 3}]]
	set n [expr {$n / 3 + ($n%3 == 2)}]
    }
    return [join [lreverse $result] ""]
}

proc bt-neg b {
    string map {+ - - +} $b
}
proc bt-sub {a b} {
    bt-add $a [bt-neg $b]
}
proc bt-add-digits {a b c} {
    if {$a eq ""} {set a 0}
    if {$b eq ""} {set b 0}
    if {$a ne 0} {append a 1}
    if {$b ne 0} {append b 1}
    lindex {{0 -1} {+ -1} {- 0} {0 0} {+ 0} {- 1} {0 1}} [expr {$a+$b+$c+3}]
}
proc bt-add {a b} {
    set c 0
    set result {}
    foreach ca [lreverse [split $a ""]] cb [lreverse [split $b ""]] {
	lassign [bt-add-digits $ca $cb $c] d c
	lappend result $d
    }
    if {$c ne "0"} {lappend result [lindex {0 + -} $c]}
    if {![llength $result]} {return "0"}
    string trimleft [join [lreverse $result] ""] 0
}
proc bt-mul {a b} {
    if {$a eq "0" || $a eq "" || $b eq "0"} {return "0"}
    set sub [bt-mul [string range $a 0 end-1] $b]0
    switch -- [string index $a end] {
	0 { return $sub }
	+ { return [bt-add $sub $b] }
	- { return [bt-sub $sub $b] }
    }
}
```

Demonstration code:

```tcl
for {set i 0} {$i<=10} {incr i} {puts "$i = [int-bt $i]"}
puts "'+-+'+'+--' = [bt-add +-+ +--] = [bt-int [bt-add +-+ +--]]"
puts "'++'*'++' = [bt-mul ++ ++] = [bt-int [bt-mul ++ ++]]"

set a "+-0++0+"
set b [int-bt -436]
set c "+-++-"
puts "a = [bt-int $a], b = [bt-int $b], c = [bt-int $c]"
set abc [bt-mul $a [bt-sub $b $c]]
puts "a*(b-c) = $abc (== [bt-int $abc])"
```

Output:

```txt

0 = 0
1 = +
2 = +-
3 = +0
4 = ++
5 = +--
6 = +-0
7 = +-+
8 = +0-
9 = +00
10 = +0+
'+-+'+'+--' = ++0 = 12
'++'*'++' = +--+ = 16
a = 523, b = -436, c = 65
a*(b-c) = ----0+--0++0 (== -262023)

```


{{omit from|Brlcad}}
{{omit from|GUISS}}
{{omit from|Lilypond}}
{{omit from|Openscad}}
{{omit from|TPP}}
