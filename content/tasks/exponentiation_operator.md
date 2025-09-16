+++
title = "Exponentiation operator"
description = ""
date = 2019-07-29T15:28:38Z
aliases = []
[extra]
id = 2602
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
languages = [
  "ada",
  "algol_68",
  "autohotkey",
  "awk",
  "basic",
  "bbc_basic",
  "befunge",
  "brat",
  "c",
  "chef",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "e",
  "echolisp",
  "ela",
  "elixir",
  "erlang",
  "erre",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "gap",
  "go",
  "haskell",
  "hicest",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "liberty_basic",
  "lingo",
  "logo",
  "lua",
  "lucid",
  "m2000_interpreter",
  "m4",
  "maxima",
  "nemerle",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "retro",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "slate",
  "smalltalk",
  "standard_ml",
  "stata",
  "swift",
  "tcl",
  "ursa",
  "vba",
  "vbscript",
  "xpl0",
  "zkl",
  "zx_spectrum_basic",
]
+++

Most programming languages have a built-in implementation of exponentiation.


## Task

Re-implement integer exponentiation for both   <big>int<sup>int</sup></big>   and   <big>float<sup>int</sup></big>   as both a procedure,   and an operator (if your language supports operator definition).

If the language supports operator (or procedure) overloading, then an overloaded form should be provided for both   <big>int<sup>int</sup></big>   and   <big>float<sup>int</sup></big>   variants.





## Ada


First we declare the specifications of the two procedures and the two corresponding operators (written as functions with quoted operators as their names):

```ada
package Integer_Exponentiation is
   --  int^int
   procedure Exponentiate (Argument : in     Integer;
                           Exponent : in     Natural;
                           Result   :    out Integer);
   function "**" (Left  : Integer;
                  Right : Natural) return Integer;

   --  real^int
   procedure Exponentiate (Argument : in     Float;
                           Exponent : in     Integer;
                           Result   :    out Float);
   function "**" (Left  : Float;
                  Right : Integer) return Float;
end Integer_Exponentiation;
```


Now we can create a test program:


```ada
with Ada.Float_Text_IO, Ada.Integer_Text_IO, Ada.Text_IO;
with Integer_Exponentiation;

procedure Test_Integer_Exponentiation is
   use Ada.Float_Text_IO, Ada.Integer_Text_IO, Ada.Text_IO;
   use Integer_Exponentiation;
   R : Float;
   I : Integer;
begin
   Exponentiate (Argument => 2.5, Exponent => 3, Result => R);
   Put ("2.5 ^ 3 = ");
   Put (R, Fore => 2, Aft => 4, Exp => 0);
   New_Line;

   Exponentiate (Argument => -12, Exponent => 3, Result => I);
   Put ("-12 ^ 3 = ");
   Put (I, Width => 7);
   New_Line;
end Test_Integer_Exponentiation;
```


Finally we can implement the procedures and operations:


```ada
package body Integer_Exponentiation is
   --  int^int
   procedure Exponentiate (Argument : in     Integer;
                           Exponent : in     Natural;
                           Result   :    out Integer) is
   begin
      Result := 1;
      for Counter in 1 .. Exponent loop
         Result := Result * Argument;
      end loop;
   end Exponentiate;

   function "**" (Left  : Integer;
                  Right : Natural) return Integer is
      Result : Integer;
   begin
      Exponentiate (Argument => Left,
                    Exponent => Right,
                    Result   => Result);
      return Result;
   end "**";

   --  real^int
   procedure Exponentiate (Argument : in     Float;
                           Exponent : in     Integer;
                           Result   :    out Float) is
   begin
      Result := 1.0;
      if Exponent < 0 then
         for Counter in Exponent .. -1 loop
            Result := Result / Argument;
         end loop;
      else
         for Counter in 1 .. Exponent loop
            Result := Result * Argument;
         end loop;
      end if;
   end Exponentiate;

   function "**" (Left  : Float;
                  Right : Integer) return Float is
      Result : Float;
   begin
       Exponentiate (Argument => Left,
                    Exponent => Right,
                    Result   => Result);
      return Result;
   end "**";
end Integer_Exponentiation;
```



## ALGOL 68

```algol68
main:(
  INT two=2, thirty=30; # test constants #
  PROC VOID undefined;

# First implement exponentiation using a rather slow but sure FOR loop #
  PROC int pow = (INT base, exponent)INT: ( # PROC cannot be over loaded #
    IF exponent<0 THEN undefined FI;
    INT out:=( exponent=0 | 1 | base );
    FROM 2 TO exponent DO out*:=base OD;
    out
  );

  printf(($" One Gibi-unit is: int pow("g(0)","g(0)")="g(0)" - (cost: "g(0)
           " INT multiplications)"l$,two, thirty, int pow(two,thirty),thirty-1));

# implement exponentiation using a faster binary technique and WHILE LOOP #
  OP ** = (INT base, exponent)INT: (
    BITS binary exponent:=BIN exponent ; # do exponent arithmetic in binary #
    INT out := IF bits width ELEM binary exponent THEN base ELSE 1 FI;
    INT sq := IF exponent < 0 THEN undefined; ~ ELSE base FI;

    WHILE
      binary exponent := binary exponent SHR 1;
      binary exponent /= BIN 0
    DO
      sq *:= sq;
      IF bits width ELEM binary exponent THEN out *:= sq FI
    OD;
    out
  );

  printf(($" One Gibi-unit is: "g(0)"**"g(0)"="g(0)" - (cost: "g(0)
           " INT multiplications)"l$,two, thirty, two ** thirty,8));

  OP ** = (REAL in base, INT in exponent)REAL: ( # ** INT Operator can be overloaded #
    REAL base := ( in exponent<0 | 1/in base | in base);
    INT exponent := ABS in exponent;
    BITS binary exponent:=BIN exponent ; # do exponent arithmetic in binary #
    REAL out := IF bits width ELEM binary exponent THEN base ELSE 1 FI;
    REAL sq := base;

    WHILE
      binary exponent := binary exponent SHR 1;
      binary exponent /= BIN 0
    DO
      sq *:= sq;
      IF bits width ELEM binary exponent THEN out *:= sq FI
    OD;
    out
  );

  printf(($" One Gibi-unit is: "g(0,1)"**"g(0)"="g(0,1)" - (cost: "g(0)
           " REAL multiplications)"l$, 2.0, thirty, 2.0 ** thirty,8));

  OP ** = (REAL base, REAL exponent)REAL: ( # ** REAL Operator can be overloaded #
    exp(ln(base)*exponent)
  );

  printf(($" One Gibi-unit is: "g(0,1)"**"g(0,1)"="g(0,1)" - (cost: "
           "depends on precision)"l$, 2.0, 30.0, 2.0 ** 30.0))
)
```

```txt
One Gibi-unit is: int pow(2,30)=1073741824 - (cost: 29 INT multiplications)
One Gibi-unit is: 2**30=1073741824 - (cost: 8 INT multiplications)
One Gibi-unit is: 2.0**30=1073741824.0 - (cost: 8 REAL multiplications)
One Gibi-unit is: 2.0**30.0=1073741824.0 - (cost: depends on precision)
```



###  Recursive operator calls

```algol68
main:(
  INT two=2, thirty=30; # test constants #
  PROC VOID undefined;

# First implement exponentiation using a rather slow but sure FOR loop #
  PROC int pow = (INT base, exponent)INT: ( # PROC cannot be over loaded #
    IF exponent<0 THEN undefined FI;
    INT out:=( exponent=0 | 1 | base );
    FROM 2 TO exponent DO out*:=base OD;
    out
  );

  printf(($" One Gibi-unit is: int pow("g(0)","g(0)")="g(0)" - (cost: "g(0)
           " INT multiplications)"l$,two, thirty, int pow(two,thirty),thirty-1));

# implement exponentiation using a faster binary technique and WHILE LOOP #
  OP ** = (INT base, exponent)INT:
    IF   base = 0 THEN 0 ELIF base = 1 THEN 1
    ELIF exponent = 0 THEN 1 ELIF exponent = 1 THEN base
    ELIF ODD exponent THEN
      (base*base) ** (exponent OVER 2) * base
    ELSE
      (base*base) ** (exponent OVER 2)
    FI;

  printf(($" One Gibi-unit is: "g(0)"**"g(0)"="g(0)" - (cost: "g(0)
           " INT multiplications)"l$,two, thirty, two ** thirty,8));

  OP ** = (REAL in base, INT in exponent)REAL: ( # ** INT Operator can be overloaded #
    REAL base := ( in exponent<0 | 1/in base | in base);
    INT exponent := ABS in exponent;
    IF   base = 0 THEN 0 ELIF base = 1 THEN 1
    ELIF exponent = 0 THEN 1 ELIF exponent = 1 THEN base
    ELIF ODD exponent THEN
      (base*base) ** (exponent OVER 2) * base
    ELSE
      (base*base) ** (exponent OVER 2)
    FI
  );

  printf(($" One Gibi-unit is: "g(0,1)"**"g(0)"="g(0,1)" - (cost: "g(0)
           " REAL multiplications)"l$, 2.0, thirty, 2.0 ** thirty,8));

  OP ** = (REAL base, REAL exponent)REAL: ( # ** REAL Operator can be overloaded #
    exp(ln(base)*exponent)
  );

  printf(($" One Gibi-unit is: "g(0,1)"**"g(0,1)"="g(0,1)" - (cost: "
           "depends on precision)"l$, 2.0, 30.0, 2.0 ** 30.0))
)
```

```txt

 One Gibi-unit is: int pow(2,30)=1073741824 - (cost: 29 INT multiplications)
 One Gibi-unit is: 2**30=1073741824 - (cost: 8 INT multiplications)
 One Gibi-unit is: 2.0**30=1073741824.0 - (cost: 8 REAL multiplications)
 One Gibi-unit is: 2.0**30.0=1073741824.0 - (cost: depends on precision)

```



## AutoHotkey


```AutoHotkey
MsgBox % Pow(5,3)
MsgBox % Pow(2.5,4)

Pow(x, n){
	r:=1
	loop %n%
		r *= x
	return r
}
```



## AWK


Traditional awk implementations do not provide an exponent operator, so we define a function to calculate the exponent.
This one-liner reads base and exponent from stdin, one pair per line, and writes the result to stdout:

```awk
$ awk 'function pow(x,n){r=1;for(i=0;i<n;i++)r=r*x;return r}{print pow($1,$2)}'

```


```txt

2.5 2
6.25
10 6
1000000
3 0
1
But this last exponentation is wrong :
10 140
100000000000000048235962126657397336628942202864391877882749784196997612045996878213993935631944257381261260379071613194067266765009513873408
This is because traditionnal awk treat number internaly by finite precision.

```



```awk
If you want to use arbitrary precision number with (more recent) awk, you have to use -M option :
$ gawk -M '{ printf("%f\n",$1^$2) }'

```


```txt

10 140
100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000,000000

```



```awk
And if you want to use locales for decimal separator, you have tu use -N option :
$ gawk -N '{ printf("%f\n",$1^$2) }'

```


```txt

2,5 2
6,250000

```



## BASIC

The vast majority of BASIC implementations don't support defining custom operators, or overloading of any kind.


```qbasic
DECLARE FUNCTION powL& (x AS INTEGER, y AS INTEGER)
DECLARE FUNCTION powS# (x AS SINGLE, y AS INTEGER)

DIM x AS INTEGER, y AS INTEGER
DIM a AS SINGLE

RANDOMIZE TIMER
a = RND * 10
x = INT(RND * 10)
y = INT(RND * 10)
PRINT x, y, powL&(x, y)
PRINT a, y, powS#(a, y)

FUNCTION powL& (x AS INTEGER, y AS INTEGER)
    DIM n AS INTEGER, m AS LONG
    IF x <> 0 THEN
        m = 1
        IF SGN(y) > 0 THEN
            FOR n = 1 TO y
                m = m * x
            NEXT
        END IF
    END IF
    powL& = m
END FUNCTION

FUNCTION powS# (x AS SINGLE, y AS INTEGER)
    DIM n AS INTEGER, m AS DOUBLE
    IF x <> 0 THEN
        m = 1
        IF y <> 0 THEN
            FOR n = 1 TO y
                m = m * x
            NEXT
            IF y < 0 THEN m = 1# / m
        END IF
    END IF
    powS# = m
END FUNCTION
```


```txt

 0             8             0
 7.768213      8             13260781.61887441
 1             9             1
 2.707636      9             7821.90151734948
 8             2             64
 9.712946      2             94.34131879665438

```


=
## BBC BASIC
=

```bbcbasic
      PRINT "11^5 = " ; FNipow(11, 5)
      PRINT "PI^3 = " ; FNfpow(PI, 3)
      END

      DEF FNipow(A%, B%)
      LOCAL I%, P%
      P% = 1
      FOR I% = 1 TO 32
        P% *= P%
        IF B% < 0 THEN P% *= A%
        B% = B% << 1
      NEXT
      = P%

      DEF FNfpow(A, B%)
      LOCAL I%, P
      P = 1
      FOR I% = 1 TO 32
        P *= P
        IF B% < 0 THEN P *= A
        B% = B% << 1
      NEXT
      = P
```

```txt
11^5 = 161051
PI^3 = 31.0062767
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 DEF POW(X,Y)
110   IF X=0 THEN LET POW=0:EXIT DEF
120   LET POW=EXP(Y*LOG(X))
130 END DEF
140 PRINT POW(PI,3)
150 PRINT PI^3
```



## Befunge

Note: Only works for integer bases and powers.

```befunge
v         v       \<
>&:32p&1-\>32g*\1-:|
                   $
                   .
                   @
```



## Brat


```brat
#Procedure
exp = { base, exp |
  1.to(exp).reduce 1, { m, n | m = m * base }
}

#Numbers are weird
1.parent.^ = { rhs |
  num = my
  1.to(rhs).reduce 1 { m, n | m = m * num }
}

p exp 2 5 #Prints 32
p 2 ^ 5   #Prints 32
```



## C

Two versions are given - one for integer  bases, the other for floating point. The
integer version returns 0 when the abs(base) is != 1 and the exponent is negative.

```c
#include <stdio.h>
#include <assert.h>

int ipow(int base, int exp)
{
   int pow = base;
   int v = 1;
   if (exp < 0) {
      assert (base != 0);  /* divide by zero */
      return (base*base != 1)? 0: (exp&1)? base : 1;
   }

   while(exp > 0 )
   {
      if (exp & 1) v *= pow;
      pow *= pow;
      exp >>= 1;
    }
   return v;
}

double dpow(double base, int exp)
{
   double v=1.0;
   double pow = (exp <0)? 1.0/base : base;
   if (exp < 0) exp = - exp;

   while(exp > 0 )
   {
      if (exp & 1) v *= pow;
      pow *= pow;
      exp >>= 1;
   }
   return v;
}

int main()
{
    printf("2^6 = %d\n", ipow(2,6));
    printf("2^-6 = %d\n", ipow(2,-6));
    printf("2.71^6 = %lf\n", dpow(2.71,6));
    printf("2.71^-6 = %lf\n", dpow(2.71,-6));
}
```


The C11 standard features type-generic expressions via the _Generic keyword. We can add to the above example to use this feature.
```c

#define generic_pow(base, exp)\
    _Generic((base),\
            double: dpow,\
            int: ipow)\
    (base, exp)

int main()
{
    printf("2^6 = %d\n", generic_pow(2,6));
    printf("2^-6 = %d\n", generic_pow(2,-6));
    printf("2.71^6 = %lf\n", generic_pow(2.71,6));
    printf("2.71^-6 = %lf\n", generic_pow(2.71,-6));
}

```


## C#
In C# it is possible to [http://msdn.microsoft.com/en-us/library/s53ehcz3%28VS.71%29.aspx overload operators] (+, -, *, etc..), but to do so requires the overload to implement at least one argument as the calling type.

What this means, is that if we have the class, A, to do an overload of + - we must set one of the arguments as the type "A".
This is because in C#, overloads are defined on a class basis - so when doing an operator, .Net looks at the class to find the operators. In this manner, one of the arguments must be of the class, else .Net would be looking there in vain.

This again means, that a direct overloading of the ^-character between two integers / double and integer is not possible.

However - coming to think of it, one could overload the "int" class, and enter the operator there.
--[[User:LordMike|LordMike]] 17:45, 5 May 2010 (UTC)


```c#

static void Main(string[] args)
{
	Console.WriteLine("5^5 = " + Expon(5, 5));
	Console.WriteLine("5.5^5 = " + Expon(5.5, 5));
	Console.ReadLine();
}

static double Expon(int Val, int Pow)
{
	return Math.Pow(Val, Pow);
}
static double Expon(double Val, int Pow)
{
	return Math.Pow(Val, Pow);
}

```


```txt

5^5 = 3125
5.5^5 = 5032,84375

```



## C++

While C++ does allow operator overloading, it does not have an exponentiation operator, therefore only a function definition is given. For non-negative exponents the integer and floating point versions are exactly the same, for obvious reasons. For negative exponents, the integer exponentiation would not give integer results; therefore there are several possibilities:
# Use floating point results even for integer exponents.
# Use integer results for integer exponents and give an error for negative exponents.
# Use integer results for integer exponents and return just the integer part (i.e. return 0 if the base is larger than one and the exponent is negative).
The third option somewhat resembles the integer division rules, and has the nice property that it can use the exact same algorithm as the floating point version. Therefore this option is chosen here. Actually the template can be used with any type which supports multiplication, division and explicit initialization from int. Note that there are several aspects about <tt>int</tt> which are not portably defined; most notably it is not guaranteed
* that the negative of a valid int is again a valid int; indeed for most implementations, the minimal value doesn't have a positive counterpart,
* whether the result of <tt>a%b</tt> is positive or negative if <tt>a</tt> is negative, and in which direction the corresponding division is rounded (however, it ''is'' guaranteed that <tt>(a/b)*b + a%b == a</tt>)
The code below tries to avoid those platform dependencies. Note that bitwise operations wouldn't help here either, because the representation of negative numbers can vary as well.


```cpp
template<typename Number>

Number power(Number base, int exponent)
{
  int zerodir;
  Number factor;
  if (exponent < 0)
  {
    zerodir = 1;
    factor = Number(1)/base;
  }
  else
  {
    zerodir = -1;
    factor = base;
  }

  Number result(1);
  while (exponent != 0)
  {
    if (exponent % 2 != 0)
    {
      result *= factor;
      exponent += zerodir;
    }
    else
    {
      factor *= factor;
      exponent /= 2;
    }
  }
  return result;
}
```



## Chef

See [[Basic integer arithmetic#Chef]].


## Clojure

Operators in Clojure are functions, so this satisfies both requirements. Also, this is polymorphic- it will work with integers, floats, etc, even ratios. (Since operators are implemented as functions they are used in prefix notation)

```lisp
(defn ** [x n] (reduce * (repeat n x)))
```

Usage:

```txt
(** 2 3)        ; 8
(** 7.2 2.1)    ; 373.24800000000005
(** 7/2 3)      ; 343/8
```



## Common Lisp

Common Lisp has a few forms of iteration. One of the more general is the do loop. Using the do loop, one definition is given below:

```lisp
(defun my-expt-do (a b)
  (do ((x 1 (* x a))
       (y 0 (+ y 1)))
      ((= y b) x)))
```

<tt>do</tt> takes three forms. The first is a list of variable initializers and incrementers. In this case, <tt>x</tt>, the eventual return value, is initialized to 1, and every iteration of the <tt>do</tt> loop replaces the value of <tt>x</tt> with ''x * a''. Similarly, <tt>y</tt> is initialized to 0 and is replaced with ''y + 1''. The second is a list of conditions and return values. In this case, when y = b, the loop stops, and the current value of <tt>x</tt> is returned. Common Lisp has no explicit return keyword, so <tt>x</tt> ends up being the return value for the function. The last form is the body of the loop, and usually consists of some action to perform (that has some side-effect). In this case, all the work is being done by the first and second forms, so there are no extra actions.

Of course, Lisp programmers often prefer recursive solutions.

```lisp
(defun my-expt-rec (a b)
  (cond
    ((= b 0) 1)
    (t (* a (my-expt-rec a (- b 1))))))
```

This solution uses the fact that a^0 = 1 and that a^b = a * a^{b-1}. <tt>cond</tt> is essentially a generalized if-statement. It takes a list of forms of the form (''cond'' ''result''). For instance, in this case, if b = 0, then function returns 1. ''t'' is the truth constant in Common Lisp and is often used as a default condition (similar to the <tt>default</tt> keyword in C/C++/Java or the <tt>else</tt> block in many languages).

Common Lisp has much more lenient rules for identifiers. In particular, <tt>^</tt> is a valid CL identifier. Since it is not already defined in the standard library, we can simply use it as a function name, just like any other function.

```lisp
(defun ^ (a b)
  (do ((x 1 (* x a))
       (y 0 (+ y 1)))
      ((= y b) x)))
```



## D

D has a built-in exponentiation operator: ^^

```d
import std.stdio, std.conv;

struct Number(T) {
    T x; // base
    alias x this;
    string toString() const { return text(x); }

    Number opBinary(string op)(in int exponent)
    const pure nothrow @nogc if (op == "^^") in {
        if (exponent < 0)
            assert (x != 0, "Division by zero");
    } body {
        debug puts("opBinary ^^");

        int zerodir;
        T factor;
        if (exponent < 0) {
            zerodir = +1;
            factor = T(1) / x;
        } else {
            zerodir = -1;
            factor = x;
        }

        T result = 1;
        int e = exponent;
        while (e != 0)
            if (e % 2 != 0) {
                result *= factor;
                e += zerodir;
            } else {
                factor *= factor;
                e /= 2;
            }

        return Number(result);
    }
}

void main() {
    alias Double = Number!double;
    writeln(Double(2.5) ^^ 5);

    alias Int = Number!int;
    writeln(Int(3) ^^ 3);
    writeln(Int(0) ^^ -2); // Division by zero.
}
```

(Compiled in debug mode, stack trace removed)

```txt
core.exception.AssertError@exponentiation_operator.d(11): Division by zero
opBinary ^^
97.6563
opBinary ^^
27
opBinary
```



## E

Simple, unoptimized implementation which will accept any kind of number for the base. If the base is an <code>int</code>, then the result will be of type <code>float64</code> if the exponent is negative, and <code>int</code> otherwise.


```e
def power(base, exponent :int) {
    var r := base
    if (exponent < 0) {
        for _ in exponent..0 { r /= base }
    } else if (exponent <=> 0) {
        return 1
    } else {
        for _ in 2..exponent { r *= base }
    }
    return r
}
```



## EchoLisp


```lisp

;; this exponentiation function handles integer, rational or float x.
;; n is a positive or negative integer.

(define (** x n) (cond
    ((zero? n) 1)
    ((< n 0) (/ (** x (- n)))) ;; x**-n = 1 / x**n
    ((= n 1) x)
    ((= n 0) 1)
    ((odd? n) (* x (** x (1- n)))) ;; x**(2p+1) = x * x**2p
    (else (let ((m (** x (/ n 2)))) (* m m))))) ;; x**2p = (x**p) * (x**p)

(** 3 0) → 1
(** 3 4) → 81
(** 3 5) → 243
(** 10 10) → 10000000000
(** 1.3 10) → 13.785849184900007

(** -3 5) → -243
(** 3 -4) → 1/81
(** 3.7 -4) → 0.005335720890574502
(** 2/3 7) → 128/2187

(lib 'bigint)
(** 666 42) →
38540524895511613165266748863173814985473295063157418576769816295283207864908351682948692085553606681763707358759878656


```



## Ela


Ela standard prelude already defines an exponentiation operator (**) but we will implement it by ourselves anyway:


```ela
open number

_ ^ 0           =  1
x ^ n | n > 0   =  f x (n - 1) x
      |else = fail "Negative exponent"
  where f _ 0 y = y
        f a d y = g a d
          where g b i | even i  = g (b * b) (i `quot` 2)
                      | else = f b (i - 1) (b * y)

(12 ^ 4, 12 ** 4)
```


```txt
(20736,20736)
```


Ela supports generic arithmetic functions and generic numeric literals. This is how we can change an implementation of a (^) function
and make it generic:


```ela
open number

//Function quot from number module is defined only for
//integral numbers. We can use this as an universal quot.
uquot x y | x is Integral = x `quot` y
          | else = x / y

//Changing implementation by using generic numeric literals
//(e.g. 2u) and elimitating all comparisons with 0.
!x ^ n  | n ~= 0u = 1u
        | n > 0u  =  f x (n - 1u) x
        | else = fail "Negative exponent"
  where f a d y
          | d ~= 0u = y
          | else = g a d
          where g b i | even i  = g (b * b) (i `uquot` 2u)
                      | else = f b (i - 1u) (b * y)


(12 ^ 4, 12.34 ^ 4.04)
```


```txt
(20736,286138.2f)
```


We have a case of true polymorphism here and no overloading is required. However Ela supports overloading
using classes (somewhat similar to Haskell type classes) so we can show how the same implementation could
work with overloading (less preferable in this case because of more redundant code but still possible):


```ela
open number

//A class that defines our overloadable function
class Exponent a where
  (^) a->a->_

//Implementation for integers
instance Exponent Int where
  _ ^ 0           =  1
  x ^ n | n > 0   =  f x (n - 1) x
        |else = fail "Negative exponent"
    where f _ 0 y = y
          f a d y = g a d
            where g b i | even i  = g (b * b) (i `quot` 2)
                        | else = f b (i - 1) (b * y)

//Implementation for floats
instance Exponent Single where
  x ^ n | n < 0.001 = 1
        | n > 0 =  f x (n - 1) x
        | else = fail "Negative exponent"
    where f a d y
            | d < 0.001 = y
            | else = g a d
            where g b i | even i  = g (b * b) (i / 2)
                        | else = f b (i - 1) (b * y)

(12 ^ 4, 12.34 ^ 4.04)
```


```txt
(20736,286138.2f)
```



## Elixir


```elixir
defmodule My do
  def exp(x,y) when is_integer(x) and is_integer(y) and y>=0 do
    IO.write("int>   ")         # debug test
    exp_int(x,y)
  end
  def exp(x,y) when is_integer(y) do
    IO.write("float> ")         # debug test
    exp_float(x,y)
  end
  def exp(x,y), do: (IO.write("       "); :math.pow(x,y))

  defp exp_int(_,0), do: 1
  defp exp_int(x,y), do: Enum.reduce(1..y, 1, fn _,acc -> x * acc end)

  defp exp_float(_,y) when y==0, do: 1.0
  defp exp_float(x,y) when y<0, do: 1/exp_float(x,-y)
  defp exp_float(x,y), do: Enum.reduce(1..y, 1, fn _,acc -> x * acc end)
end

list = [{2,0}, {2,3}, {2,-2},
        {2.0,0}, {2.0,3}, {2.0,-2},
        {0.5,0}, {0.5,3}, {0.5,-2},
        {-2,2}, {-2,3}, {-2.0,2}, {-2.0,3},
        ]
IO.puts "                    ___My.exp___  __:math.pow_"
Enum.each(list, fn {x,y} ->
  sxy = "#{x} ** #{y}"
  sexp = inspect My.exp(x,y)
  spow = inspect :math.pow(x,y)         # For the comparison
  :io.fwrite("~10s = ~12s, ~12s~n", [sxy, sexp, spow])
end)
```


```txt

                    ___My.exp___  __:math.pow_
int>       2 ** 0 =            1,          1.0
int>       2 ** 3 =            8,          8.0
float>    2 ** -2 =         0.25,         0.25
float>   2.0 ** 0 =          1.0,          1.0
float>   2.0 ** 3 =          8.0,          8.0
float>  2.0 ** -2 =         0.25,         0.25
float>   0.5 ** 0 =          1.0,          1.0
float>   0.5 ** 3 =        0.125,        0.125
float>  0.5 ** -2 =          4.0,          4.0
int>      -2 ** 2 =            4,          4.0
int>      -2 ** 3 =           -8,         -8.0
float>  -2.0 ** 2 =          4.0,          4.0
float>  -2.0 ** 3 =         -8.0,         -8.0

```



## Erlang

pow(number, integer) -> number

```erlang

pow(X, Y) when Y < 0 ->
    1/pow(X, -Y);
pow(X, Y) when is_integer(Y) ->
    pow(X, Y, 1).

pow(_, 0, B) ->
    B;
pow(X, Y, B) ->
    B2 = if Y rem 2 =:= 0 -> B; true -> X * B end,
    pow(X * X, Y div 2, B2).

```


Tail call optimised version which works for both integers and float bases.


## ERRE

ERRE does not permit operator overloading, so we can use a procedure only. The procedure below
handles *integer powers*: for floating point exponent you must use EXP and LOG predefined
functions.

```ERRE
PROGRAM POWER

PROCEDURE POWER(A,B->POW)   ! this routine handles only *INTEGER* powers
  LOCAL FLAG%
  IF B<0 THEN B=-B FLAG%=TRUE
  POW=1
  FOR X=1 TO B DO
    POW=POW*A
  END FOR
  IF FLAG% THEN POW=1/POW
END PROCEDURE

BEGIN
   POWER(11,-2->POW) PRINT(POW)
   POWER(π,3->POW) PRINT(POW)
END PROGRAM
```

```txt
 8.264463E-03
 31.00628

```


=={{header|F_Sharp|F#}}==

```fsharp

//Integer Exponentiation, more interesting anyway than repeated multiplication. Nigel Galloway, October 12th., 2018
let rec myExp n g=match g with
                  |0            ->1
                  |g when g%2=1 ->n*(myExp n (g-1))
                  |_            ->let p=myExp n (g/2) in p*p

printfn "%d" (myExp 3 15)

```

```txt

14348907

```


## Factor

Simple, unoptimized implementation which accepts a positive or negative exponent:

```factor
: pow ( f n -- f' )
    dup 0 < [ abs pow recip ]
    [ [ 1 ] 2dip swap [ * ] curry times ] if ;
```


Here is a recursive implementation which splits the exponent in two:

```factor
: pow ( f n -- f' )
    {
        { [ dup 0 < ] [ abs pow recip ] }
        { [ dup 0 = ] [ 2drop 1 ] }
        [ [ 2 mod 1 = swap 1 ? ] [ [ sq ] [ 2 /i ] bi* pow ] 2bi * ]
    } cond ;
```


This implementation recurses only when an odd factor is found:

```factor
USING: combinators kernel math ;
IN: test

: (pow) ( f n -- f' )
    [ dup even? ] [ [ sq ] [ 2 /i ] bi* ] while
    dup 1 = [ drop ] [ dupd 1 - (pow) * ] if ;

: pow ( f n -- f' )
    {
        { [ dup 0 < ] [ abs (pow) recip ] }
        { [ dup 0 = ] [ 2drop 1 ] }
        [ (pow) ]
    } cond ;
```


A non-recursive version of (pow) can be written as:

```factor
: (pow) ( f n -- f' )
    [ 1 ] 2dip
    [ dup 1 = ] [
        dup even? [ [ sq ] [ 2 /i ] bi* ] [ [ [ * ] keep ] dip 1 - ] if
    ] until
    drop * ;
```



## Forth


```forth
: ** ( n m -- n^m )
  1 swap  0 ?do over * loop  nip ;
```



```forth
: f**n ( f n -- f^n )
  dup 0= if
    drop fdrop 1e
  else dup 1 and if
    1- fdup recurse f*
  else
    2/ fdup f* recurse
  then then ;
```



## Fortran

```fortran
MODULE Exp_Mod
IMPLICIT NONE

INTERFACE OPERATOR (.pow.)    ! Using ** instead would overload the standard exponentiation operator
  MODULE PROCEDURE Intexp, Realexp
END INTERFACE

CONTAINS

  FUNCTION Intexp (base, exponent)
    INTEGER :: Intexp
    INTEGER, INTENT(IN) :: base, exponent
    INTEGER :: i

    IF (exponent < 0) THEN
       IF (base == 1) THEN
          Intexp = 1
       ELSE
          Intexp = 0
       END IF
       RETURN
    END IF
    Intexp = 1
    DO i = 1, exponent
      Intexp = Intexp * base
    END DO
  END FUNCTION IntExp

  FUNCTION Realexp (base, exponent)
    REAL :: Realexp
    REAL, INTENT(IN) :: base
    INTEGER, INTENT(IN) :: exponent
    INTEGER :: i

    Realexp = 1.0
    IF (exponent < 0) THEN
       DO i = exponent, -1
          Realexp = Realexp / base
       END DO
    ELSE
       DO i = 1, exponent
          Realexp = Realexp * base
       END DO
    END IF
  END FUNCTION RealExp
END MODULE Exp_Mod

PROGRAM EXAMPLE
USE Exp_Mod
  WRITE(*,*) 2.pow.30, 2.0.pow.30
END PROGRAM EXAMPLE
```

```txt

  1073741824    1.073742E+09

```



## FreeBASIC


```freebasic
' FB 1.05.0

' Note that 'base' is a keyword in FB, so we use 'base_' instead as a parameter

Function Pow Overload (base_ As Double, exponent As Integer) As Double
  If exponent = 0.0 Then Return 1.0
  If exponent = 1.0 Then Return base_
  If exponent < 0.0 Then Return 1.0 / Pow(base_, -exponent)
  Dim power As Double = base_
  For i As Integer = 2 To exponent
     power *= base_
  Next
  Return power
End Function

Function Pow Overload(base_ As Integer, exponent As Integer) As Double
   Return Pow(CDbl(base_), exponent)
End Function

' check results of these functions using FB's built in '^' operator
Print "Pow(2, 2)       = "; Pow(2, 2)
Print "Pow(2.5, 2)     = "; Pow(2.5, 2)
Print "Pow(2, -3)      = "; Pow(2, -3)
Print "Pow(1.78, 3)    = "; Pow(1.78, 3)
Print
Print "2 ^ 2           = "; 2 ^ 2
Print "2.5 ^ 2         = "; 2.5 ^ 2
Print "2 ^ -3          = "; 2 ^ -3
Print "1.78 ^ 3        = "; 1.78 ^ 3
Print
Print "Press any key to quit"
Sleep
```


```txt

Pow(2, 2)       =  4
Pow(2.5, 2)     =  6.25
Pow(2, -3)      =  0.125
Pow(1.78, 3)    =  5.639752000000001

2 ^ 2           =  4
2.5 ^ 2         =  6.25
2 ^ -3          =  0.125
1.78 ^ 3        =  5.639752000000001

```



## GAP


```gap
expon := function(a, n, one, mul)
	local p;
	p := one;
	while n > 0 do
		if IsOddInt(n) then
			p := mul(a, p);
		fi;
		a := mul(a, a);
		n := QuoInt(n, 2);
	od;
	return p;
end;

expon(2, 10, 1, \*);
# 1024

# a more creative use of exponentiation
List([0 .. 31], n -> (1 - expon(0, n, 1, \-))/2);
# [ 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
#   1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1 ]
```



## Go

Go doesn't support operator defintion.  Other notes:  While I left the integer algorithm simple, I used the shift and square trick for the float algorithm, just to show an alternative.

```go
package main

import (
    "errors"
    "fmt"
)

func expI(b, p int) (int, error) {
    if p < 0 {
        return 0, errors.New("negative power not allowed")
    }
    r := 1
    for i := 1; i <= p; i++ {
        r *= b
    }
    return r, nil
}

func expF(b float32, p int) float32 {
    var neg bool
    if p < 0 {
        neg = true
        p = -p
    }
    r := float32(1)
    for pow := b; p > 0; pow *= pow {
        if p&1 == 1 {
            r *= pow
        }
        p >>= 1
    }
    if neg {
        r = 1 / r
    }
    return r
}

func main() {
    ti := func(b, p int) {
        fmt.Printf("%d^%d: ", b, p)
        e, err := expI(b, p)
        if err != nil {
            fmt.Println(err)
        } else {
            fmt.Println(e)
        }
    }

    fmt.Println("expI tests")
    ti(2, 10)
    ti(2, -10)
    ti(-2, 10)
    ti(-2, 11)
    ti(11, 0)

    fmt.Println("overflow undetected")
    ti(10, 10)

    tf := func(b float32, p int) {
        fmt.Printf("%g^%d: %g\n", b, p, expF(b, p))
    }

    fmt.Println("\nexpF tests:")
    tf(2, 10)
    tf(2, -10)
    tf(-2, 10)
    tf(-2, 11)
    tf(11, 0)

    fmt.Println("disallowed in expI, allowed here")
    tf(0, -1)

    fmt.Println("other interesting cases for 32 bit float type")
    tf(10, 39)
    tf(10, -39)
    tf(-10, 39)
}
```

```txt

expI tests
2^10: 1024
2^-10: negative power not allowed
-2^10: 1024
-2^11: -2048
11^0: 1
overflow undetected
10^10: 1410065408

expF tests:
2^10: 1024
2^-10: 0.0009765625
-2^10: 1024
-2^11: -2048
11^0: 1
disallowed in expI, allowed here
0^-1: +Inf
other interesting cases for 32 bit float type
10^39: +Inf
10^-39: 0
-10^39: -Inf

```



## Haskell

Here's the exponentiation operator from the Prelude:

```haskell
(^) :: (Num a, Integral b) => a -> b -> a
_ ^ 0           =  1
x ^ n | n > 0   =  f x (n-1) x where
  f _ 0 y = y
  f a d y = g a d  where
    g b i | even i  = g (b*b) (i `quot` 2)
          | otherwise = f b (i-1) (b*y)
_ ^ _           = error "Prelude.^: negative exponent"
```


There's no difference in Haskell between a procedure (or function) and an operator, other than the infix notation. This routine is overloaded for any integral exponent (which includes the arbitrarily large ''Integer'' type) and any numeric type for the bases (including, for example, ''Complex''). It uses the fast "binary" exponentiation algorithm. For a negative exponent, the type of the base must support division (and hence reciprocals):


```haskell
(^^) :: (Fractional a, Integral b) => a -> b -> a
x ^^ n = if n >= 0 then x^n else recip (x^(negate n))
```


This rules out e.g. the integer types as base values in this case. Haskell also has a third exponentiation operator,


```haskell
(**) :: Floating a => a -> a -> a
x ** y = exp (log x * y)
```

which is used for floating point arithmetic.


## HicEst


```hicest
WRITE(Clipboard) pow(5,   3)  ! 125
WRITE(ClipBoard) pow(5.5, 7)  ! 152243.5234

FUNCTION pow(x, n)
   pow = 1
   DO i = 1, n
      pow = pow * x
   ENDDO
END
```


=={{header|Icon}} and {{header|Unicon}}==
The procedure below will take an integer or real base and integer exponent and return base ^ exponent.  If exponent is negative, base is coerced to real so as not to return 0.  Operator overloading is not supported and this is not an efficient implementation.

```Icon
procedure main()
bases := [5,5.]
numbers := [0,2,2.,-1,3]
every  write("expon(",b := !bases,", ",x := !numbers,")=",(expon(b,x) | "failed") \ 1)
end

procedure expon(base,power)
local op,res

base := numeric(base)            | runerror(102,base)
power := power = integer(power)  | runerr(101,power)

if power = 0 then return 1
else op := if power < 1 then
              (base := real(base)) & "/"   # force real base
              else "*"

res := 1
every 1 to abs(power) do
   res := op(res,base)
return res
end
```



## J

J is concretely specified, which makes it easy to define primitives in terms of other primitives (this is especially true of mathematical primitives, given the language's mathematical emphasis).

So we have any number of options.  Here's the simplest, equivalent to the <code>for each number, product = product * number</code> of other languages.  The base may be any number, and the exponent may be any non-negative integer (including zero):


```j
   exp  =:  */@:#~

   10 exp 3
1000

   10 exp 0
1
```


We can make this more general by allowing the exponent to be any integer (including negatives), at the cost of a slight increase in complexity:


```j
   exp  =:  *@:] %: */@:(#~|)

   10 exp _3
0.001
```


Or, we can define exponentiation as repeated multiplication (as opposed to multiplying a given number of copies of the base)


```J
   exp =: dyad def 'x *^:y 1'

   10 exp 3
1000
   10 exp _3
0.001
```


Here, when we specify a negative number of repetitions, multiplication's inverse is used that many times.

J's calculus of functions permits us to define exponentiation in its full generality, as the '''inverse of log''' (i.e.  ''exp = log<sup>-1</sup>''):


```j
 exp  =:  ^.^:_1

 81 exp 0.5
9
```


Note that the definition above does '''not''' use the primitive exponentiation function <code>^</code>  .  The carets in it represent different (but related) things .  The function is composed of three parts:  <code>^.  ^:  _1</code>  .  The first part, <code>^.</code>, is the primitive logarithm operator (e.g. <code>3 = 10^.1000</code>)  .

The second part,  <code>^:</code>  , is interesting:  it is a "meta operator". It takes two arguments:  a function <code>f</code> on its left, and a number <code>N</code> on its right.  It produces a new function, which, when given an argument, applies <code>f</code> to that argument <code>N</code> times.  For example, if we had a function <code>increment</code>, then <code>increment^:3 X</code> would increment <code>X</code> three times, so the result would be <code>X+3</code>.

In the case of  <code>^.  ^:  _1 </code>, <code>f</code> is <code>^.</code> (i.e. logarithm) and <code>N</code> is -1.  Therefore we apply log negative one times or '''the inverse of log once''' (precisely as in ''log<sup>-1</sup>'').

Similarly, we can define exponentiation as the reverse of the inverse of root.  That is, ''x pow y = y root<sup>-1</sup> x'':


```j
 exp  =:  %:^:_1~

 81 exp 0.5
9
```


Compare this with the previous definition:  it is the same, except that  <code>%:</code>  , ''root'', has been substituted for  <code>^.</code>  , ''logarithm'', and the arguments have been reversed (or ''reflected'') with <code>~</code>.

That is, J is telling us that '''power is the same as the reflex of the inverse of root''', exactly as we'd expect.

One last note:  we said these definitions are the same as <code>^</code> in its full generality.  What is meant by that?  Well, in the context of this puzzle, it means both the base and exponent may be any real number.  But J goes further than that: it also permits '''complex numbers'''.

Let's use Euler's famous formula, ''e<sup>pi*i</sup> = -1'' as an example:


```j
   pi =: 3.14159265358979323846
   e  =: 2.71828182845904523536
   i  =: 2 %: _1                  NB.  Square root of -1

   e^(pi*i)
_1
```


And, as stated, our redefinition is equivalent:


```j
   exp =: %:^:_1~

   e exp (pi*i)
_1
```



## Java

Java does not support operator definition. This example is unoptimized, but will handle negative exponents as well. It is unnecessary to show int<sup>int</sup> since an int in Java will be cast as a double.

```java
public class Exp{
   public static void main(String[] args){
      System.out.println(pow(2,30));
      System.out.println(pow(2.0,30)); //tests
      System.out.println(pow(2.0,-2));
   }

   public static double pow(double base, int exp){
      if(exp < 0) return 1 / pow(base, -exp);
      double ans = 1.0;
      for(;exp > 0;--exp) ans *= base;
      return ans;
   }
}
```

```txt

 1.073741824E9
 1.073741824E9
 0.25

```



## JavaScript


```javascript
function pow(base, exp) {
    if (exp != Math.floor(exp))
        throw "exponent must be an integer";
    if (exp < 0)
        return 1 / pow(base, -exp);
    var ans = 1;
    while (exp > 0) {
        ans *= base;
        exp--;
    }
    return ans;
}
```



## jq


```jq
# 0^0 => 1
# NOTE: jq converts very large integers to floats.
# This implementation uses reduce to avoid deep recursion
def power_int(n):
  if n == 0 then 1
  elif . == 0 then 0
  elif n < 0 then 1/power_int(-n)
  elif ((n | floor) == n) then
       ( (n % 2) | if . == 0 then 1 else -1 end ) as $sign
       | if (. == -1) then $sign
         elif . < 0 then (( -(.) | power_int(n) ) * $sign)
         else . as $in | reduce range(1;n) as $i ($in; . * $in)
         end
  else error("This is a toy implementation that requires n be integral")
  end ;
```

Demonstration:
```jq
def demo(x;y):
  x | [ power_int(y), (log*y|exp) ] ;

demo(2; 3),
demo(2; 64),
demo(1.1; 1024),
demo(1.1; -1024)

# Output:
[8,                      7.999999999999998]
[18446744073709552000,   18446744073709525000]
[2.4328178969536854e+42, 2.4328178969536693e+42]
[4.1104597317052596e-43, 4.1104597317052874e-43]

```



## Julia


```julia

function pow(base::Number, exp::Integer)
  r = one(base)
  for i = 1:exp
    r *= base
  end
  return r
end

```


```txt

julia> println("5 ^ 3 ^ 2 = ", 5 ^ 3 ^ 2)
5 ^ 3 ^ 2 = 1953125

julia> println("(5 ^ 3) ^ 2 = ", (5 ^ 3) ^ 2)
(5 ^ 3) ^ 2 = 15625

julia> println("5 ^ (3 ^ 2) = ", 5 ^ (3 ^ 2))
5 ^ (3 ^ 2) = 1953125


```



## Kotlin

Kotlin does not have a dedicated exponentiation operator (we would normally use Java's Math.pow method instead) but it's possible to implement integer and floating power exponentiation (with integer exponents) using infix extension functions which look like non-symbolic operators for these actions:

```scala
// version 1.0.6

infix fun Int.ipow(exp: Int): Int =
    when {
        this ==  1 -> 1
        this == -1 -> if (exp % 2 == 0) 1 else -1
        exp <  0   -> throw IllegalArgumentException("invalid exponent")
        exp == 0   -> 1
        else       -> {
            var ans = 1
            var base = this
            var e = exp
            while (e > 0) {
                if (e and 1 == 1) ans *= base
                e = e shr 1
                base *= base
            }
            ans
        }
    }

infix fun Double.dpow(exp: Int): Double {
    var ans = 1.0
    var e   = exp
    var base = if (e < 0) 1.0 / this else this
    if (e < 0) e = -e
    while (e > 0) {
        if (e and 1 == 1) ans *= base
        e = e shr 1
        base *= base
    }
    return ans
}

fun main(args: Array<String>) {
    println("2  ^ 3   = ${2 ipow 3}")
    println("1  ^ -10 = ${1 ipow -10}")
    println("-1 ^ -3  = ${-1 ipow -3}")
    println()
    println("2.0 ^ -3 = ${2.0 dpow -3}")
    println("1.5 ^ 0  = ${1.5 dpow 0}")
    println("4.5 ^ 2  = ${4.5 dpow 2}")
}
```


```txt

2  ^ 3   = 8
1  ^ -10 = 1
-1 ^ -3  = -1

2.0 ^ -3 = 0.125
1.5 ^ 0  = 1.0
4.5 ^ 2  = 20.25

```



## Liberty BASIC


```lb

  print " 11^5     = ", floatPow(  11,       5  )
  print " (-11)^5  = ", floatPow( -11,       5  )
  print " 11^( -5) = ", floatPow(  11,      -5  )
  print " 3.1416^3 = ", floatPow(   3.1416,  3  )
  print " 0^2      = ", floatPow(   0,       2  )
  print "  2^0     = ", floatPow(   2,       0  )
  print " -2^0     = ", floatPow(  -2,       0  )

  end

  function floatPow( a, b)
      if a <>0 then
          m =1
          if b =abs( b) then
              for n =1 to b
                  m =m *a
              next n
          else
              m =1 /floatPow( a, 0 - b)  ' LB has no unitary minus operator.
          end if
      else
          m =0
      end if
      floatPow =m
  end function

```



## Lingo

Lingo doesn't support user-defined operators.

```lingo
-- As for built-in power() function:
-- base can be either integer or float; returns float.
on pow (base, exp)
  if exp=0 then return 1.0
  else if exp<0 then
    exp = -exp
    base = 1.0/base
  end if
  res = float(base)
  repeat with i = 2 to exp
    res = res*base
  end repeat
  return res
end
```



## Logo


```logo
to int_power :n :m
  if equal? 0 :m [output 1]
  if equal? 0 modulo :m 2 [output int_power :n*:n :m/2]
  output :n * int_power :n :m-1
end
```



## Lua

All numbers in Lua are floating point numbers (thus, there are no real integers). Operator overloading is supported for tables only.

```lua
number = {}

function number.pow( a, b )
    local ret = 1
    if b >= 0 then
        for i = 1, b do
            ret = ret * a.val
        end
    else
        for i = b, -1 do
            ret = ret / a.val
        end
    end
    return ret
end

function number.New( v )
    local num = { val = v }
    local mt = { __pow = number.pow }
    setmetatable( num, mt )
    return num
end

x = number.New( 5 )
print( x^2 )                   --> 25
print( number.pow( x, -4 ) )   --> 0.016
```



## Lucid

[http://portal.acm.org/citation.cfm?id=947727.947728&coll=GUIDE&dl=GUIDE Some misconceptions about Lucid]


```lucid
pow(n,x)
   k = n fby k div 2;
   p = x fby p*p;
   y =1 fby if even(k) then y else y*p;
   result y asa k eq 0;
end
```


## M2000 Interpreter


```M2000 Interpreter

Module Exponentiation {
	\\ a variable can be any type except  a string (no $ in name)
	\\ variable b is long type.
	\\ by default we pass by value arguments to a function
	\\ to pass by reference we have to use & before name,
	\\ in the signature and in the call
	function pow(a, b as long) {
		p=a-a  ' make p same type as a
		p++
		if b>0 then for i=1& to b {p*=a}
		=p
	}
	const fst$="{0::-32} {1}"
	Document exp$
	k= pow(11&, 5)
	exp$=format$(fst$, k, type$(k)="Long")+{
	}
	l=pow(11, 5)
	exp$=format$(fst$, l, type$(l)="Double")+{
	}
	m=pow(pi, 3)
	exp$=format$(fst$, m, type$(m)="Decimal")+{
	}
	\\ send to clipboard
	clipboard exp$
	\\ send  monospaced type text to console using cr char to change lines
	Print #-2, exp$
	Rem Report exp$  ' send to console using proportional spacing and justification
}
Exponentiation



```


```txt

                          161051 True
                          161051 True
  31.006276680299820175476315064 True

</pre >


## M4

M4 lacks floating point computation and operator definition.

```M4
define(`power',`ifelse($2,0,1,`eval($1*$0($1,decr($2)))')')
power(2,10)
```

```txt

1024

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Define a function and an infix operator \[CirclePlus] with the same definition:

```Mathematica
exponentiation[x_,y_Integer]:=Which[y>0,Times@@ConstantArray[x,y],y==0,1,y<0,1/exponentiation[x,-y]]
CirclePlus[x_,y_Integer]:=exponentiation[x,y]
```

Examples:

```Mathematica
exponentiation[1.23,3]
exponentiation[4,0]
exponentiation[2.5,-2]
1.23\[CirclePlus]3
4\[CirclePlus]0
2.5\[CirclePlus]-2
```

gives back:

```Mathematica
1.86087
1
0.16
1.86087
1
0.16
```

Note that \[CirclePlus] shows up as a special character in Mathematica namely a circle divided in 4 pieces. Note also that this function supports negative and positive exponents.


## Maxima


```maxima
"^^^"(a, n) := block(
   [p: 1],
   while n > 0 do (
      if oddp(n) then p: p * a,
      a: a * a,
      n: quotient(n, 2)
   ),
   p
)$

infix("^^^")$

2 ^^^ 10;
1024

2.5 ^^^ 10;
9536.7431640625
```


=={{header|MK-61/52}}==
<lang>С/П	x^y	С/П
```


=={{header|Modula-2}}==
Whilst some implementations or dialects of Modula-2 may permit definition or overloading of operators, neither is permitted in N.Wirth's classic language definition and the ISO Modula-2 standard. The operations are therefore given as library functions.

```modula2

(* Library Interface *)
DEFINITION MODULE Exponentiation;

PROCEDURE IntExp(base, exp : INTEGER) : INTEGER;
 (* Raises base to the power of exp and returns the result
    both base and exp must be of type INTEGER *)

PROCEDURE RealExp(base : REAL; exp : INTEGER) : REAL;
 (* Raises base to the power of exp and returns the result
    base must be of type REAL, exp of type INTEGER *)

END Exponentiation.

(* Library Implementation *)
IMPLEMENTATION MODULE Exponentiation;

PROCEDURE IntExp(base, exp : INTEGER) : INTEGER;
  VAR
    i, res : INTEGER;
  BEGIN
    res := 1;
    FOR i := 1 TO exp DO
      res := res * base;
    END;
    RETURN res;
  END IntExp;

PROCEDURE RealExp(base: REAL; exp: INTEGER) : REAL;
  VAR
    i : INTEGER;
    res : REAL;
  BEGIN
    res := 1.0;
    IF exp < 0 THEN
      FOR i := exp TO -1 DO
        res := res / base;
      END;
    ELSE (* exp >= 0 *)
      FOR i := 1 TO exp DO
        res := res * base;
      END;
    END;
    RETURN res;
  END RealExp;

END Exponentiation.

```


=={{header|Modula-3}}==

```modula3
MODULE Expt EXPORTS Main;

IMPORT IO, Fmt;

PROCEDURE IntExpt(arg, exp: INTEGER): INTEGER =
  VAR result := 1;
  BEGIN
    FOR i := 1 TO exp DO
      result := result * arg;
    END;
    RETURN result;
  END IntExpt;

PROCEDURE RealExpt(arg: REAL; exp: INTEGER): REAL =
  VAR result := 1.0;
  BEGIN
    IF exp < 0 THEN
      FOR i := exp TO -1 DO
        result := result / arg;
      END;
    ELSE
      FOR i := 1 TO exp DO
        result := result * arg;
      END;
    END;
    RETURN result;
  END RealExpt;

BEGIN
  IO.Put("2 ^ 4 = " & Fmt.Int(IntExpt(2, 4)) & "\n");
  IO.Put("2.5 ^ 4 = " & Fmt.Real(RealExpt(2.5, 4)) & "\n");
END Expt.
```

```txt

2 ^ 4 = 16
2.5 ^ 4 = 39.0625

```



## Nemerle

Macros can be used to define a new operator:

```Nemerle
using System;

macro @^ (val, pow : int)
{
    <[ Math.Pow($val, $pow) ]>
}
```

The file with the macro needs to be compiled as a library, and the resulting assembly must be referenced when compiling source files which use the operator.

```Nemerle
using System;
using System.Console;
using Nemerle.Assertions;

module Expon
{
    Expon(val : int, pow : int) : int            // demonstrates simple/naive method
      requires pow > 0 otherwise throw ArgumentOutOfRangeException("Negative powers not allowed, will not return int.")
    {
        mutable result = 1;
        repeat(pow) {
            result *= val
        }
        result
    }

    Expon(val : double, pow : int) : double     // demonstrates shift and square method
    {
        mutable neg = false;
        mutable p = pow;
        when (pow < 0) {neg = true; p = -pow};
        mutable v = val;
        mutable result = 1d;

        while (p > 0) {
            when (p & 1 == 1) result *= v;
            v *= v;
            p >>= 1;
        }
        if (neg) 1d/result else result
    }

    Main() : void
    {
        def eight = 2^3;
        // def oops = 2^1.5; // compilation error as operator is defined for integer exponentiation
        def four = Expon(2, 2);
        def four_d = Expon(2.0, 2);

        WriteLine($"$eight, $four, $four_d");
    }
}
```



## Nim


```nim
proc `^`[T: float|int](base: T; exp: int): T =
  var (base, exp) = (base, exp)
  result = 1

  if exp < 0:
    when T is int:
      if base * base != 1: return 0
      elif (exp and 1) == 0: return 1
      else: return base
    else:
      base = 1.0 / base
      exp = -exp

  while exp != 0:
    if (exp and 1) != 0:
      result *= base
    exp = exp shr 1
    base *= base

echo "2^6 = ", 2^6
echo "2^-6 = ", 2 ^ -6
echo "2.71^6 = ", 2.71^6
echo "2.71^-6 = ", 2.71 ^ -6
```



## Objeck


```objeck
class Exp {
  function : Main(args : String[]) ~ Nil {
    Pow(2,30)->PrintLine();
    Pow(2.0,30)->PrintLine();
    Pow(2.0,-2)->PrintLine();
   }

  function : native : Pow(base : Float, exp : Int) ~ Float {
    if(exp < 0) {
      return 1 / base->Power(exp * -1.0);
    };

    ans := 1.0;
    while(exp > 0) {
      ans *= base;
      exp -= 1;
    };

    return ans;
  }
}
```



```txt
1.07374182e+009
1.07374182e+009
0.25
```



## OCaml

It is possible to create a generic exponential. For this, one must know the
multiplication function, and the unit value. Here, the usual fast algorithm is
used:


```ocaml
let pow one mul a n =
  let rec g p x = function
  | 0 -> x
  | i ->
      g (mul p p) (if i mod 2 = 1 then mul p x else x) (i/2)
  in
  g a one n
;;

pow 1 ( * ) 2 16;;  (* 65536 *)
pow 1.0 ( *. ) 2.0 16;; (* 65536. *)

(* pow is not limited to exponentiation *)
pow 0 ( + ) 2 16;;  (* 32 *)
pow "" ( ^ ) "abc " 10;;  (* "abc abc abc abc abc abc abc abc abc abc " *)
pow [ ] ( @ ) [ 1; 2 ] 10;;  (* [1; 2; 1; 2; 1; 2; 1; 2; 1; 2; 1; 2; 1; 2; 1; 2; 1; 2; 1; 2] *)

(* Thue-Morse sequence *)
Array.init 32 (fun n -> (1 - pow 1 ( - ) 0 n) lsr 1);;

(* [|0; 1; 1; 0; 1; 0; 0; 1; 1; 0; 0; 1; 0; 1; 1; 0;
     1; 0; 0; 1; 0; 1; 1; 0; 0; 1; 1; 0; 1; 0; 0; 1|]

See http://en.wikipedia.org/wiki/Thue-Morse_sequence
*)
```


See also [[Matrix-exponentiation operator#OCaml]] for a matrix usage.


## Oforth


This function works either for int or floats :


```Oforth
: powint(r, n)
| i |
   1 n abs loop: i [ r * ]
   n isNegative ifTrue: [ inv ] ;

2 3 powint println
2 powint(3) println
1.2 4 powint println
1.2 powint(4) println
```


```txt

8
8
2.0736
2.0736

```



## PARI/GP

This version works for integer and floating-point bases (as well as intmod bases, ...).

```parigp
ex(a, b)={
  my(c = 1);
  while(b > 1,
    if(b % 2, c *= a);
    a = a^2;
    b >>= 1
  );
  a * c
};
```


PARI/GP also has a built-in operator that works for any type of numerical exponent:

```parigp
ex2(a, b) = a ^ b;
```



## Pascal


```pascal
Program ExponentiationOperator(output);

function intexp (base, exponent: integer): longint;
  var
    i: integer;

  begin
    if (exponent < 0) then
      if (base = 1) then
        intexp := 1
      else
        intexp := 0
    else
    begin
      intexp := 1;
      for i := 1 to exponent do
        intexp := intexp * base;
    end;
  end;

function realexp (base: real; exponent: integer): real;
  var
    i: integer;

  begin
    realexp := 1.0;
    if (exponent < 0) then
      for i := exponent to -1 do
        realexp := realexp / base
    else
      for i := 1 to exponent do
        realexp := realexp * base;
  end;

begin
  writeln('2^30: ', intexp(2, 30));
  writeln('2.0^30: ', realexp(2.0, 30));
end.
```

```txt
% ./ExponentiationOperator
2^30: 1073741824
2.0^30:  1.07374182400000E+009
```



### With overload

Pascal functions can be overloaded. This means that the two functions can have the same name and the particular function executed will depend on the data types of the arguments.

```txt

Program ExponentiationOperator(output);

function newpower (base, exponent: integer): longint;
  var
    i: integer;

  begin
    if (exponent < 0) then
      if (base = 1) then
        newpower := 1
      else
        newpower := 0
    else
    begin
      newpower := 1;
      for i := 1 to exponent do
        newpower := newpower * base;
    end;
  end;

function newpower (base: real; exponent: integer): real;
  var
    i: integer;

  begin
    newpower := 1.0;
    if (exponent < 0) then
      for i := exponent to -1 do
        newpower := newpower / base
    else
      for i := 1 to exponent do
        newpower := newpower * base;
  end;

begin
  writeln('2^30: ', newpower(2, 30));
  writeln('2.0^30: ', newpower(2.0, 30));
  readln;
end.

```

Output is as before.


## Perl


```Perl
#!/usr/bin/perl -w
use strict ;

sub expon {
   my ( $base , $expo ) = @_ ;
   if ( $expo == 0 ) {
      return 1 ;
   }
   elsif ( $expo == 1 ) {
      return $base ;
   }
   elsif ( $expo > 1 ) {
      my $prod = 1 ;
      foreach my $n ( 0..($expo - 1) ) {
	 $prod *= $base ;
      }
      return $prod ;
   }
   elsif ( $expo < 0 ) {
      return 1 / ( expon ( $base , -$expo ) ) ;
   }
}
print "3 to the power of 10 as a function is " . expon( 3 , 10 ) . " !\n" ;
print "3 to the power of 10 as a builtin is " . 3**10 . " !\n" ;
print "5.5 to the power of -3 as a function is " . expon( 5.5 , -3 ) . " !\n" ;
print "5.5 to the power of -3 as a builtin is " . 5.5**-3 . " !\n" ;

```

```txt

3 to the power of 10 as a function is 59049 !
3 to the power of 10 as a builtin is 59049 !
5.5 to the power of -3 as a function is 0.00601051840721262 !
5.5 to the power of -3 as a builtin is 0.00601051840721262 !

```


The following version is simpler and much faster for large exponents, since it uses exponentiation by squaring.

```Perl
sub ex {
  my($base,$exp) = @_;
  die "Exponent '$exp' must be an integer!" if $exp != int($exp);
  return 1 if $exp == 0;
  ($base, $exp) = (1/$base, -$exp)  if $exp < 0;
  my $c = 1;
  while ($exp > 1) {
    $c *= $base if $exp % 2;
    $base *= $base;
    $exp >>= 1;
  }
  $base * $c;
}
```



## Perl 6

```perl6
subset Natural of Int where { $^n >= 0 }

multi pow (0,     0)            { fail '0**0 is undefined' }
multi pow ($base, Natural $exp) { [*] $base xx $exp }
multi pow ($base, Int $exp)     { 1 / pow $base, -$exp }

sub infix:<***> ($a, $b) { pow $a, $b }

# Testing

say pow .75, -5;
say .75 *** -5;
```



## Phix

The builtin power function handles atoms and integers for both arguments, whereas this deliberately restricts the exponent to an integer.

There is no operator overloading in Phix, or for that matter any builtin overriding.

```Phix
function powi(atom b, integer i)
atom v=1
    b = iff(i<0 ? 1/b : b)
    i = abs(i)
    while i>0 do
        if and_bits(i,1) then v *= b end if
        b *= b
        i = floor(i/2)
    end while
    return v
end function
?powi(-3,-5)
?power(-3,-5)
```

```txt

-0.004115226337
-0.004115226337

```



## PicoLisp

This uses Knuth's algorithm (The Art of Computer Programming, Vol. 2, page 442)

```PicoLisp
(de ** (X N)  # N th power of X
   (if (ge0 N)
      (let Y 1
         (loop
            (when (bit? 1 N)
               (setq Y (* Y X)) )
            (T (=0 (setq N (>> 1 N)))
               Y )
            (setq X (* X X)) ) )
      0 ) )
```



## PL/I


```pli
declare exp generic
  (iexp when (fixed, fixed),
   fexp when (float, fixed) );
iexp: procedure (m, n) returns (fixed binary (31));
   declare (m, n) fixed binary (31) nonassignable;
   declare exp fixed binary (31) initial (m), i fixed binary;
   if m = 0 & n = 0 then signal error;
   if n = 0 then return (1);
   do i = 2 to n;
      exp = exp * m;
   end;
   return (exp);
end iexp;
fexp: procedure (a, n) returns (float (15));
   declare (a float, n fixed binary (31)) nonassignable;
   declare exp float initial (a), i fixed binary;
   if a = 0 & n = 0 then signal error;
   if n = 0 then return (1);
   do i = 2 to n;
      exp = exp * a;
   end;
   return (exp);
end fexp;
```



## PowerShell


```powershell
function pow($a, [int]$b) {
    if ($b -eq -1) { return 1/$a }
    if ($b -eq 0)  { return 1 }
    if ($b -eq 1)  { return $a }
    if ($b -lt 0) {
        $rec = $true # reciprocal needed
        $b = -$b
    }

    $result = $a
    2..$b | ForEach-Object {
        $result *= $a
    }

    if ($rec) {
        return 1/$result
    } else {
        return $result
    }
}
```

The function works for both integers and floating-point values as first argument.

PowerShell does not support operator overloading directly
(and there wouldn't be an exponentiation operator to overload).

```txt
PS> pow 2 15
32768
PS> pow 2.71 -4
0,018540559532257
PS> pow (-1.35) 3
−2,460375
```

The negative first argument needs to be put in parentheses because it would otherwise be passed as string.
This can be circumvented by declaring the first argument to the function as <code>double</code>, but then the return type would be always double while currently <code>pow 2 3</code> returns an <code>int</code>.


## PureBasic

PureBasic does not allow an operator to be redefined or operator overloading.

```PureBasic
Procedure powI(base, exponent)
  Protected i, result.d
  If exponent < 0
    If base = 1
      result = 1
    EndIf
    ProcedureReturn result
  EndIf
  result = 1
  For i = 1 To exponent
    result * base
  Next
  ProcedureReturn result
EndProcedure

Procedure.f powF(base.f, exponent)
  Protected i, magExponent = Abs(exponent), result.d
  If base <> 0
    result = 1.0
    If exponent <> 0
      For i = 1 To magExponent
        result * base
      Next
      If exponent < 0
        result = 1.0 / result
      EndIf
    EndIf
  EndIf
  ProcedureReturn result
EndProcedure

If OpenConsole()
  Define x, a.f, exp

  x = Random(10) - 5
  a = Random(10000) / 10000 * 10
  For exp = -3 To 3
    PrintN(Str(x) + " ^ " + Str(exp) + " = " + Str(powI(x, exp)))
    PrintN(StrF(a) + " ^ " + Str(exp) + " = " + StrF(powF(a, exp)))
    PrintN("--------------")
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

```txt
-3 ^ -3 = 0
6.997000 ^ -3 = 0.002919
--------------
-3 ^ -2 = 0
6.997000 ^ -2 = 0.020426
--------------
-3 ^ -1 = 0
6.997000 ^ -1 = 0.142918
--------------
-3 ^ 0 = 1
6.997000 ^ 0 = 1.000000
--------------
-3 ^ 1 = -3
6.997000 ^ 1 = 6.997000
--------------
-3 ^ 2 = 9
6.997000 ^ 2 = 48.958012
--------------
-3 ^ 3 = -27
6.997000 ^ 3 = 342.559235
-------------
```




## Prolog

###  Declaring an Operator as an Arithmetic Function


In Prolog, we define ''predicates'' rather than ''functions''. Still, functions and predicates are related: going one way, we can think of an n-place predicate as a function from its arguments to a member of the set <code>{true, false}</code>; going the other way, we can think of functions as predicates with a hidden ultimate argument, called a "return value". Following the latter approach, Prolog sometimes uses macro expansion to provide functional syntax by

1. catching terms fitting  a certain pattern (viz. <code>Base ^^ Exp</code>, which is the same as <code>'^^'(N, 3)</code>),

2. calling the term with an extra argument (viz. <code>call('^^'(Base, Exp), Power)</code>),

3. replacing the occurrence of the term with the value instantiated in the extra argument (viz. Power).

The predicate <code>is/2</code> supports functional syntax in its second argument: e.g., <code>X is sqrt(2) + 1</code>. New arithmetic functions can be added with the `arithmetic_function/1` directive, wherein the arity attributed to the function is one less than the arity of the predicate which will be called during term expansion and evaluation. The following directives establish <code>^^/2</code> as, first, an arithmetic function, and then as a right-associative binary operator (so that <code>X is 2^^2^^2</code> == ''X = 2^(2^2)</code>):


```prolog
:- arithmetic_function((^^)/2).
:- op(200, xfy, user:(^^)).
```


When <code>^^/2</code> occurs in an expression in the second argument of <code>is/2</code>, Prolog calls the subsequently defined predicate <code>^^/3</code>, and obtains the operators replacement value from the predicate's third argument.

=== Higher-order Predicate: ===

This solution employs the higher-order predicate <code>foldl/4</code> from the standard SWI-Prolog <code>library(apply)</code>, in conjunction with an auxiliary "folding predicate" (note, the definition uses the <code>^^</code> operator as an arithmetic function):


```prolog
%% ^^/3
%
%   True if Power is Base ^ Exp.

^^(Base, Exp, Power) :-
    ( Exp < 0   ->  Power is 1 / (Base ^^ (Exp * -1))            % If exponent is negative, then ...

    ; Exp > 0   ->  length(Powers, Exp),                         % If exponent is positive, then
                    foldl( exp_folder(Base), Powers, 1, Power )  %    Powers is a list of free variables with length Exp
                                                                 %    and Power is Powers folded with exp_folder/4

    ; Power = 1                                                  % otherwise Exp must be 0, so
    ).

%% exp_folder/4
%
%       True when Power is the product of Base and Powers.
%
%       This predicate is designed to work with foldl and a list of free variables.
%       It passes the result of each evaluation to the next application through its
%       fourth argument, instantiating the elements of Powers to each successive Power of the Base.

exp_folder(Base, Power, Powers, Power) :-
    Power is Base * Powers.
```


'''Example usage:'''


```prolog
?- X is 2 ^^ 3.
X = 8.

?- X is 2 ^^ -3.
X = 0.125.

?- X is 2.5 ^^ -3.
X = 0.064.

?- X is 2.5 ^^ 3.
X = 15.625.
```



###  Recursive Predicate


An implementation of exponentiation using recursion and no control predicates.


```prolog
exp_recursive(Base, NegExp, NegPower) :-
    NegExp < 0,
    Exp is NegExp * -1,
    exp_recursive_(Base, Exp, Base, Power),
    NegPower is 1 / Power.
exp_recursive(Base, Exp, Power) :-
    Exp > 0,
    exp_recursive_(Base, Exp, Base, Power).
exp_recursive(_, 0, 1).

exp_recursive_(_,    1,   Power, Power).
exp_recursive_(Base, Exp, Acc,   Power)   :-
    Exp > 1,
    NewAcc is Base * Acc,
    NewExp is Exp  - 1,
    exp_recursive_(Base, NewExp, NewAcc, Power).
```



## Python


```python
MULTIPLY = lambda x, y: x*y

class num(float):
    # the following method has complexity O(b)
    # rather than O(log b) via the rapid exponentiation
    def __pow__(self, b):
        return reduce(MULTIPLY, [self]*b, 1)

# works with ints as function or operator
print num(2).__pow__(3)
print num(2) ** 3

# works with floats as function or operator
print num(2.3).__pow__(8)
print num(2.3) ** 8
```



## R


```r
# Method
pow <- function(x, y)
{
   x <- as.numeric(x)
   y <- as.integer(y)
   prod(rep(x, y))
}
#Operator
"%pow%" <- function(x,y) pow(x,y)

pow(3, 4)    # 81
2.5 %pow% 2  # 6.25
```



## Racket


```Racket
#lang racket
(define (^ base expt)
  (for/fold ((acum 1))
    ((i (in-range expt)))
    (* acum base)))

(^ 5 2) ; 25
(^ 5.0 2) ; 25.0
```



## Retro

Retro has no floating point support in the standard VM.

From the '''math'''' vocabulary:


```Retro
: pow  ( bp-n ) 1 swap [ over * ] times nip ;
```


And in use:


```Retro
2 5 ^math'pow
```


The fast exponentiation algorithm can be coded as follows:


```Retro
: pow ( n m -- n^m )
1 2rot
[ dup 1 and 0 <>
  [ [ tuck * swap ] dip ] ifTrue
  [ dup * ] dip 1 >> dup 0 <>
] while
drop drop ;
```



## REXX

The   '''iPow'''   function doesn't care what kind of number is to be raised to a power,

it can be an integer or floating point number.

Extra error checking was added to verify that the invocation is syntactically correct.

```rexx
/*REXX program  computes and displays  various   (integer)   exponentiations.           */
                                                 say center('digits='digits(), 79, "─")
say '17**65   is:'
say  17**65
say

numeric digits 100;                              say center('digits='digits(), 79, "─")
say '17**65   is:'
say  17**65
say

numeric digits 10;                               say center('digits='digits(), 79, "─")
say '2 ** -10   is:'
say  2 ** -10
say

numeric digits 30;                               say center('digits='digits(), 79, "─")
say '-3.1415926535897932384626433 ** 3  is:'
say  -3.1415926535897932384626433 ** 3
say

numeric digits 1000;                             say center('digits='digits(), 79, "─")
say '2 ** 1000   is:'
say  2 ** 1000
say

numeric digits 60;                               say center('digits='digits(), 79, "─")
say 'iPow(5, 70)  is:'
say  iPow(5, 70)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
errMsg: say;     say '***error***';     say;     say arg(1);     say;     say;     exit 13
/*──────────────────────────────────────────────────────────────────────────────────────*/
iPow:   procedure;  parse arg x 1 _,p
        if arg()<2           then call errMsg  "not enough arguments specified"
        if arg()>2           then call errMsg  "too many arguments specified"
        if \datatype(x,'N')  then call errMsg  "1st arg isn't numeric:"         x
        if \datatype(p,'W')  then call errMsg  "2nd arg isn't an integer:"      p
        if p=0               then return 1
                do abs(p) - 1;    _=_*x;    end  /*abs(p)-1*/
        if p<0               then _=1/_
        return _
```

;;;output'''

```txt

───────────────────────────────────digits=9────────────────────────────────────
17**65   is:
9.53190909E+79

──────────────────────────────────digits=100───────────────────────────────────
17**65   is:
95319090450218007303742536355848761234066170796000792973413605849481890760893457

───────────────────────────────────digits=10───────────────────────────────────
2 ** -10   is:
0.0009765625

───────────────────────────────────digits=30───────────────────────────────────
-3.1415926535897932384626433 ** 3  is:
-31.0062766802998201754763126013

──────────────────────────────────digits=1000──────────────────────────────────
2 ** 1000   is:
10715086071862673209484250490600018105614048117055336074437503883703510511249361224931983788156958581275946729175531468251871452856923140435984577574698574803934567774824230985421074605062371141877954182153046474983581941267398767559165543946077062914571196477686542167660429831652624386837205668069376

───────────────────────────────────digits=60───────────────────────────────────
ipow(5,70)  is:
8470329472543003390683225006796419620513916015625

```



## Ring


```ring

see "11^5 = " + ipow(11, 5) + nl
see "pi^3 = " + fpow(3.14, 3) + nl

func ipow a, b
     p2 = 1
     for i = 1 to 32
         p2 *= p2
         if b < 0  p2 *= a ok
         b = b << 1
     next
     return p2

func fpow a, b
     p = 1
     for i = 1 to 32
         p *= p
         if b < 0  p *= a ok
         b = b << 1
     next
     return p

```

Output :

```txt

11^5 = 161051
pi^3 = 30.96

```



## Ruby

We add a <tt>pow</tt> method to <tt>Numeric</tt> objects. To calculate <tt>5.pow 3</tt>, this method fills an array <tt>[5, 5, 5]</tt> and then multiplies together the elements.


```ruby
class Numeric
  def pow(m)
    raise TypeError, "exponent must be an integer: #{m}" unless m.is_a? Integer
    puts "pow!!"
    Array.new(m, self).reduce(1, :*)
  end
end

p 5.pow(3)
p 5.5.pow(3)
p 5.pow(3.1)
```


```txt
pow!!
125
pow!!
166.375
pow.rb:3:in `pow': exponent must be an integer: 3.1 (TypeError)
        from pow.rb:16:in `<main>'
```


To overload the ** exponentiation operator, this might work, but doesn't:

```ruby
class Numeric
  def **(m)
    pow(m)
  end
end
```

It doesn't work because the ** method is defined independently for Numeric subclasses Fixnum, Bignum and Float.  One must:

```ruby
class Fixnum
  def **(m)
    print "Fixnum "
    pow(m)
  end
end
class Bignum
  def **(m)
    print "Bignum "
    pow(m)
  end
end
class Float
  def **(m)
    print "Float "
    pow(m)
  end
end

p i=2**64
p i ** 2
p 2.2 ** 3
```

```txt
Fixnum pow!!
18446744073709551616
Bignum pow!!
340282366920938463463374607431768211456
Float pow!!
10.648
```



## Run BASIC


```runbasic
print " 11^5     = ";11^5
print " (-11)^5  = ";-11^5
print " 11^( -5) = ";11^-5
print " 3.1416^3 = ";3.1416^3
print " 0^2      = ";0^2
print "  2^0     = ";2^0
print " -2^0     = ";-2^0
```

```txt
 11^5     = 161051
 (-11)^5  = -161051
 11^( -5) = 6.20921325e-6
 3.1416^3 = 31.0064942
 0^2      = 0
  2^0     = 1
 -2^0     = 1
```


## Rust

The <code>num</code> crate is the de-facto Rust library for numerical generics and it provides the <code>One</code> trait which allows for an exponentiation function that is generic over both integral and floating point types. The library provides this generic exponentiation function, the implementation of which is the <code>pow</code> function below.

```rust
extern crate num;
use num::traits::One;
use std::ops::Mul;

fn pow<T>(mut base: T, mut exp: usize) -> T
    where T: Clone + One + Mul<T, Output=T>
{
    if exp == 0 { return T::one() }
    while exp & 1 == 0 {
        base = base.clone() * base;
        exp >>= 1;
    }
    if exp == 1 { return base }
    let mut acc = base.clone();

    while exp > 1 {
        exp >>= 1;
        base = base.clone() * base;
        if exp & 1 == 1 {
            acc = acc * base.clone();
        }
    }
    acc
}
```



## Scala

{{improve|Scala}}<!-- Why? This template added in place of direct categorization that failed to give any reason. -->
There's no distinction between an operator and a method in Scala. Alas, there is no way of adding methods to a class, but one can make it look like a method has been added, through a method commonly known as [http://www.artima.com/weblogs/viewpost.jsp?thread=179766 Pimp My Library]. Therefore, we show below how that can beaccomplished. We define the operator ↑ (unicode's uparrow), which is written as \u2191 below, to make cut & paste easier.

To use it, one has to import the implicit from the appropriate object. ExponentI will work for any integral type (Int, BigInt, etc), ExponentF will work for any fractional type (Double, BigDecimal, etc). Importing both at the same time won't work. In this case, it might be better to define implicits for the actual types being used, such as was done in Exponents.

```scala
object Exponentiation {
  import scala.annotation.tailrec

  @tailrec def powI[N](n: N, exponent: Int)(implicit num: Integral[N]): N = {
    import num._
    exponent match {
      case 0 => one
      case _ if exponent % 2 == 0 => powI((n * n), (exponent / 2))
      case _ => powI(n, (exponent - 1)) * n
    }
  }

  @tailrec def powF[N](n: N, exponent: Int)(implicit num: Fractional[N]): N = {
    import num._
    exponent match {
      case 0 => one
      case _ if exponent < 0 => one / powF(n, exponent.abs)
      case _ if exponent % 2 == 0 => powF((n * n), (exponent / 2))
      case _ => powF(n, (exponent - 1)) * n
    }
  }

  class ExponentI[N : Integral](n: N) {
    def \u2191(exponent: Int): N = powI(n, exponent)
  }

  class ExponentF[N : Fractional](n: N) {
    def \u2191(exponent: Int): N = powF(n, exponent)
  }

  object ExponentI {
    implicit def toExponentI[N : Integral](n: N): ExponentI[N] = new ExponentI(n)
  }

  object ExponentF {
    implicit def toExponentF[N : Fractional](n: N): ExponentF[N] = new ExponentF(n)
  }

  object Exponents {
    implicit def toExponent(n: Int): ExponentI[Int] = new ExponentI(n)
    implicit def toExponent(n: Double): ExponentF[Double] = new ExponentF(n)
  }
}
```
Functions powI and powF above are not tail recursive, since the result of the recursive call is multiplied by n. A tail recursive version of powI would be:

```scala
  @tailrec def powI[N](n: N, exponent: Int, acc:Int=1)(implicit num: Integral[N]): N = {
    exponent match {
      case 0 => acc
      case _ if exponent % 2 == 0 => powI(n * n, exponent / 2, acc)
      case _ => powI(n, (exponent - 1), acc*n)
    }
  }
```



## Scheme

This definition of the exponentiation procedure <code>^</code> operates on bases of all numerical types that the multiplication procedure <code>*</code> operates on, i. e. integer, rational, real, and complex. The notion of an operator does not exist in Scheme. Application of a procedure to its arguments is '''always''' expressed with a prefix notation.

```scheme
(define (^ base exponent)
  (define (*^ exponent acc)
    (if (= exponent 0)
        acc
        (*^ (- exponent 1) (* acc base))))
  (*^ exponent 1))

(display (^ 2 3))
(newline)
(display (^ (/ 1 2) 3))
(newline)
(display (^ 0.5 3))
(newline)
(display (^ 2+i 3))
(newline)
```

```txt

 8
 1/8
 0.125
 2+11i

```



## Seed7

In Seed7 the ** operator is overloaded for both
[http://seed7.sourceforge.net/libraries/integer.htm#%28in_integer%29**%28in_integer%29 integer<sup>integer</sup>]
and [http://seed7.sourceforge.net/libraries/float.htm#%28ref_float%29**%28ref_integer%29 float<sup>integer</sup>]
(additionally there is a ** operator
for [http://seed7.sourceforge.net/libraries/float.htm#%28ref_float%29**%28ref_float%29 float<sup>float</sup>]).
The following re-implementation of both functions does not use another exponentiation
function to do the computation.
Instead the exponentiation-by-squaring algorithm is used.


```seed7
const func integer: intPow (in var integer: base, in var integer: exponent) is func
  result
    var integer: result is 0;
  begin
    if exponent < 0 then
      raise(NUMERIC_ERROR);
    else
      if odd(exponent) then
        result := base;
      else
        result := 1;
      end if;
      exponent := exponent div 2;
      while exponent <> 0 do
        base *:= base;
        if odd(exponent) then
          result *:= base;
        end if;
        exponent := exponent div 2;
      end while;
    end if;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/math.htm#intPow]


```seed7
const func float: fltIPow (in var float: base, in var integer: exponent) is func
  result
    var float: power is 1.0;
  local
    var integer: stop is 0;
  begin
    if base = 0.0 then
      if exponent < 0 then
        power := Infinity;
      elsif exponent > 0 then
        power := 0.0;
      end if;
    else
      if exponent < 0 then
        stop := -1;
      end if;
      if odd(exponent) then
        power := base;
      end if;
      exponent >>:= 1;
      while exponent <> stop do
        base *:= base;
        if odd(exponent) then
          power *:= base;
        end if;
        exponent >>:= 1;
      end while;
      if stop = -1 then
        power := 1.0 / power;
      end if;
    end if;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/math.htm#fltIPow]

Since Seed7 supports operator and function overloading a new exponentiation
operator like ^* can be defined for
[http://seed7.sourceforge.net/libraries/integer.htm integer]
and [http://seed7.sourceforge.net/libraries/float.htm float] bases:


```seed7
$ syntax expr: .(). ^* .() is <- 4;

const func integer: (in var integer: base) ^* (in var integer: exponent) is
  return intPow(base, exponent);

const func float: (in var float: base) ^* (in var integer: exponent) is
  return fltIPow(base, exponent);
```



## Sidef

Function definition:

```ruby
func expon(_, {.is_zero}) { 1 }

func expon(base, exp {.is_neg}) {
    expon(1/base, -exp)
}

func expon(base, exp {.is_int}) {

  var c = 1
  while (exp > 1) {
    c *= base if exp.is_odd
    base *= base
    exp >>= 1
  }

  return (base * c)
}

say expon(3, 10)
say expon(5.5, -3)
```


Operator definition:

```ruby
class Number {
    method ⊙(exp) {
        expon(self, exp)
    }
}

say (3 ⊙ 10)
say (5.5 ⊙ -3)
```

```txt

59049
0.006010518407212622088655146506386175807661607813673929376408715251690458302028550142749812171299774605559729526671675432

```



## Slate

This code is from the current slate implementation:

```slate
x@(Number traits) raisedTo: y@(Integer traits)
[
  y isZero ifTrue: [^ x unit].
  x isZero \/ [y = 1] ifTrue: [^ x].
  y isPositive
    ifTrue:
      "(x * x raisedTo: y // 2) * (x raisedTo: y \\ 2)"
      [| count result |
       count: 1.
       [(count: (count bitShift: 1)) < y] whileTrue.
       result: x unit.
       [count isPositive]
	 whileTrue:
	   [result: result squared.
	    (y bitAnd: count) isZero ifFalse: [result: result * x].
	    count: (count bitShift: -1)].
       result]
    ifFalse: [(x raisedTo: y negated) reciprocal]
].
```


For floating numbers:


```slate
x@(Float traits) raisedTo: y@(Float traits)
"Implements floating-point exponentiation in terms of the natural logarithm
and exponential primitives - this is generally faster than the naive method."
[
  y isZero ifTrue: [^ x unit].
  x isZero \/ [y isUnit] ifTrue: [^ x].
  (x ln * y) exp
].
```


## Smalltalk

''Extending'' the class Number, we provide the operator for integers, floating points, ''rationals'' numbers (and any other derived class)

```smalltalk
Number extend [
  ** anInt [
       | r |
       ( anInt isInteger )
            ifFalse:
              [ '** works fine only for integer powers'
	        displayOn: stderr . Character nl displayOn: stderr ].
       r := 1.
       1 to: anInt do: [ :i | r := ( r * self ) ].
       ^r
  ]
].

( 2.5 ** 3 ) displayNl.
( 2 ** 10 ) displayNl.
( 3/7 ** 3 ) displayNl.
```


```txt
15.625
1024
27/343
```



## Standard ML

The following operators only take nonnegative integer exponents.

```sml
fun expt_int (a, b) = let
  fun aux (x, i) =
    if i = b then x
    else aux (x * a, i + 1)
in
  aux (1, 0)
end

fun expt_real (a, b) = let
  fun aux (x, i) =
    if i = b then x
    else aux (x * a, i + 1)
in
  aux (1.0, 0)
end

val op ** = expt_int
infix 6 **
val op *** = expt_real
infix 6 ***
```



```sml
- 2 ** 3;
val it = 8 : int
- 2.4 *** 3;
val it = 13.824 : real
```



## Stata


```stata
mata
function pow(a, n) {
	x = a
	for(p=1; n>0; n=floor(n/2)) {
		if(mod(n,2)==1) p = p*x
		x = x*x
	}
	return(p)
}
end
```



## Swift

Defines generic function raise(_:to:) and operator ** that will work with all bases conforming to protocol Numeric, including Float and Int.

```swift
func raise<T: Numeric
(_ base: T, to exponent: Int) -> T {
    precondition(exponent >= 0, "Exponent has to be nonnegative")
    return Array(repeating: base, count: exponent).reduce(1, *)
}

infix operator **: MultiplicationPrecedence

func **<T: Numeric>(lhs: T, rhs: Int) -> T {
    return raise(lhs, to: rhs)
}

let someFloat: Float = 2
let someInt: Int = 10

assert(raise(someFloat, to: someInt) == 1024)
assert(someFloat ** someInt == 1024)
assert(raise(someInt, to: someInt) == 10000000000)
assert(someInt ** someInt == 10000000000)
```



## Tcl

Tcl already has both an exponentiation function (<code>set x [expr {pow(2.4, 3.5)}]</code>) and operator (<code>set x [expr {2.4 ** 3.5}]</code>).  The operator cannot be overridden.  The function may be overridden by a procedure in the <code>tcl::mathfunc</code> namespace, relative to
the calling namespace.

This solution does not consider negative exponents.

```tcl
package require Tcl 8.5
proc tcl::mathfunc::mypow {a b} {
    if { ! [string is int -strict $b]} {error "exponent must be an integer"}
    set res 1
    for {set i 1} {$i <= $b} {incr i} {set res [expr {$res * $a}]}
    return $res
}
expr {mypow(3, 3)} ;# ==> 27
expr {mypow(3.5, 3)} ;# ==> 42.875
expr {mypow(3.5, 3.2)} ;# ==> exponent must be an integer
```



## Ursa


```ursa
# these implementations ignore negative exponents
def intpow (int m, int n)
	if (< n 1)
		return 1
	end if
	decl int ret
	set ret 1
	for () (> n 0) (dec n)
		set ret (* ret m)
	end for
	return ret
end intpow

def floatpow (double m, int n)
	if (or (< n 1) (and (= m 0) (= n 0)))
		return 1
	end if
	decl int ret
	set ret 1
	for () (> n 0) (dec n)
		set ret (* ret m)
	end for
	return ret
end floatpow
```



## VBA


```vb
Public Function exp(ByVal base As Variant, ByVal exponent As Long) As Variant
    Dim result As Variant
    If TypeName(base) = "Integer" Or TypeName(base) = "Long" Then
        'integer exponentiation
        result = 1
        If exponent < 0 Then
            result = IIf(Abs(base) <> 1, CVErr(2019), IIf(exponent Mod 2 = -1, base, 1))
        End If
        Do While exponent > 0
            If exponent Mod 2 = 1 Then result = result * base
            base = base * base
            exponent = exponent \ 2
        Loop
    Else
        Debug.Assert IsNumeric(base)
        'float exponentiation
        If base = 0 Then
            If exponent < 0 Then result = CVErr(11)
        Else
            If exponent < 0 Then
                base = 1# / base
                exponent = -exponent
            End If
            result = 1
            Do While exponent > 0
                If exponent Mod 2 = 1 Then result = result * base
                base = base * base
                exponent = exponent \ 2
            Loop
        End If
    End If
    exp = result
End Function
Public Sub main()
    Debug.Print "Integer exponentiation"
    Debug.Print "10^7=", exp(10, 7)
    Debug.Print "10^4=", exp(10, 4)
    Debug.Print "(-3)^3=", exp(-3, 3)
    Debug.Print "(-1)^(-5)=", exp(-1, -5)
    Debug.Print "10^(-1)=", exp(10, -1)
    Debug.Print "0^2=", exp(0, 2)
    Debug.Print "Float exponentiation"
    Debug.Print "10.0^(-3)=", exp(10#, -3)
    Debug.Print "10.0^(-4)=", exp(10#, -4)
    Debug.Print "(-3.0)^(-5)=", exp(-3#, -5)
    Debug.Print "(-3.0)^(-4)=", exp(-3#, -4)
    Debug.Print "0.0^(-4)=", exp(0#, -4)
End Sub
```
```txt
Integer exponentiation
10^7=          10000000
10^4=          10000
(-3)^3=       -27
(-1)^(-5)=    -1
10^(-1)=      Fout 2019
0^2=           0
Float exponentiation
10.0^(-3)=     0,001
10.0^(-4)=     0,0001
(-3.0)^(-5)=  -4,11522633744856E-03
(-3.0)^(-4)=   1,23456790123457E-02
0.0^(-4)=     Fout 11
```


## VBScript


```vb

Function pow(x,y)
	pow = 1
	If y < 0 Then
		For i = 1 To Abs(y)
			pow = pow * (1/x)
		Next
	Else
		For i = 1 To y
			pow = pow * x
		Next
	End If
End Function

WScript.StdOut.Write "2 ^ 0 = " & pow(2,0)
WScript.StdOut.WriteLine
WScript.StdOut.Write "7 ^ 6 = " & pow(7,6)
WScript.StdOut.WriteLine
WScript.StdOut.Write "3.14159265359 ^ 9 = " & pow(3.14159265359,9)
WScript.StdOut.WriteLine
WScript.StdOut.Write "4 ^ -6 = " & pow(4,-6)
WScript.StdOut.WriteLine
WScript.StdOut.Write "-3 ^ 5 = " & pow(-3,5)
WScript.StdOut.WriteLine

```


```txt

2 ^ 0 = 1
7 ^ 6 = 117649
3.14159265359 ^ 9 = 29809.0993334639
4 ^ -6 = 0.000244140625
-3 ^ 5 = -243

```



## XPL0

To create an exponent operator you need to modify the compiler code,
which is open source.

```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations

func real Power(X, Y);  \X raised to the Y power; (X > 0.0)
real X;  int Y;
return Exp(float(Y) * Ln(X));

func IPower(X, Y);      \X raised to the Y power
int  X, Y;
int  P;
[P:= 1;
while Y do
    [if Y&1 then P:= P*X;
    X:= X*X;
    Y:= Y>>1;
    ];
return P;
];

int X, Y;
[Format(9, 0);
for X:= 1 to 10 do
    [for Y:= 0 to 7 do
        RlOut(0, Power(float(X), Y));
    CrLf(0);
    ];
CrLf(0);
for X:= 1 to 10 do
    [for Y:= 0 to 7 do
        [ChOut(0, 9);  IntOut(0, IPower(X, Y))];
    CrLf(0);
    ];
]
```


```txt

        1        1        1        1        1        1        1        1
        1        2        4        8       16       32       64      128
        1        3        9       27       81      243      729     2187
        1        4       16       64      256     1024     4096    16384
        1        5       25      125      625     3125    15625    78125
        1        6       36      216     1296     7776    46656   279936
        1        7       49      343     2401    16807   117649   823543
        1        8       64      512     4096    32768   262144  2097152
        1        9       81      729     6561    59049   531441  4782969
        1       10      100     1000    10000   100000  1000000 10000000

        1       1       1       1       1       1       1       1
        1       2       4       8       16      32      64      128
        1       3       9       27      81      243     729     2187
        1       4       16      64      256     1024    4096    16384
        1       5       25      125     625     3125    15625   78125
        1       6       36      216     1296    7776    46656   279936
        1       7       49      343     2401    16807   117649  823543
        1       8       64      512     4096    32768   262144  2097152
        1       9       81      729     6561    59049   531441  4782969
        1       10      100     1000    10000   100000  1000000 10000000

```



## zkl

Int and Float have pow methods and zkl doesn't allow you to add operators, classes can implement existing ones.
```zkl
fcn pow(n,exp){
   reg v;
   if(n.isType(1)){ // Int
      if (exp<0) return(if(n*n!=1) 0 else (if(exp.isOdd) n else 1));
      v=1;
   }else{
      if(exp<0){ n=1.0/n; exp=-exp; }
      v=1.0;
   }
   while(exp>0){
      if(exp.isOdd) v*=n;
      n*=n;
      exp/=2;
   }
   v
}
```


```zkl
println("2^6 = %d".fmt(pow(2,6)));
println("2^-6 = %d".fmt(pow(2,-6)));
println("2.71^6 = %f".fmt(pow(2.71,6)));
println("2.71^-6 = %f".fmt(pow(2.71,-6)));
```

```txt

2^6 = 64
2^-6 = 0
2.71^6 = 396.109944
2.71^-6 = 0.002525

```



## ZX Spectrum Basic


ZX Spectrum Basic does not support custom operators or integer datatypes, but here we implement exponentation using a function.
The function itself makes use of the inbuilt exponentiation operator, which is kind of cheating, but hey this provides a working implementation.


```zxbasic
10 PRINT e(3,2): REM 3 ^ 2
20 PRINT e(1.5,2.7): REM 1.5 ^ 2.7
30 STOP
9950 DEF FN e(a,b)=a^b
```

