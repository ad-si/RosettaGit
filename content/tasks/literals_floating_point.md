+++
title = "Literals/Floating point"
description = ""
date = 2019-10-08T18:02:08Z
aliases = []
[extra]
id = 8651
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
+++

Programming languages have different ways of expressing floating-point literals.


## Task

Show how floating-point literals can be expressed in your language: decimal or other bases, exponential notation, and any other special features.

You may want to include a regular expression or BNF/ABNF/EBNF defining allowable formats for your language.


## Related tasks

*   [[Literals/Integer]]
*   [[Extreme floating point values]]





## 360 Assembly

[[wp:IBM_hexadecimal_floating_point|IBM hexadecimal floating point]]

```360asm
XS4      DC  E'1.23456E-4'       short floating-point

XDPI     DC  D'3.141592653589793' long floating-point
XD1      DC  D'0'                 long floating-point
XD2      DC  D'1'                 long floating-point
XD3      DC  D'-1'                long floating-point
XD4      DC  D'1.2345E-4'         long floating-point

XQPI     DC  L'3.14159265358979323846264338327950' extended

* short    floating-point -  32 bits -  4 bytes :  6 decimal digits
* long     floating-point -  64 bits -  8 bytes : 16 decimal digits
* extended floating-point - 128 bits - 16 bytes : 33 decimal digits

* absolute approximate range: 5e-79 to 7e75
```



## Ada

Real literals contain decimal point. The exponent part is optional. Underline may be used to separate groups of digits. A literal does not have sign, + or - are unary operations. Examples of real literals:

```Ada

3.141_592_6
1.0E-12
0.13

```



## Aime


```aime
3.14
5.0
8r      # without the "r"(eal) suffix, "8" would be an integer
.125
```



## ALGOL 68


```algol68
# floating point literals are called REAL denotations in Algol 68      #
# They have the following forms:                                       #
#     1: a digit sequence followed by "." followed by a digit sequence #
#     2: a "." followed by a digit sequence                            #
#     3: forms 1 or 2 followed by "e" followed by an optional sign     #
#                     followed by a digit sequence                     #
#     4: a digit sequence follows by "e" followed by an optional sign  #
#                     followed by a digit sequence                     #
#                                                                      #
# The "e" indicates the following optionally-signed digit sequence is  #
# the exponent of the literal.                                         #
# If the implementation allows, a "times ten to the power symbol"      #
# can be used to replace "e" - e.g. a subscript "10" character         #
#                                                                      #
# spaces can appear anywhere in the denotation                         #
# Examples:                                                            #
REAL r;
r := 1.234;
r :=  .987;
r := 4.2e-9;
r := .4e+23;
r := 1e10;
r := 3.142e-23;
r := 1 234 567 .      9           e -                  4;

```



## ALGOL W


```algolw
begin
    real r; long real lr;
    % floating point literals have the following forms:                      %
    %    1 - a digit sequence followed by "." followed by a digit sequence   %
    %    2 - a digit sequence followed by "."                                %
    %    3 - "." followed by a digit sequence                                %
    %    4 - one of the above, followed by "'" followed by an optional sign  %
    %        folloed by a digit sequence                                     %
    % the literal can be followed by "L", indicating it is long real         %
    % the literal can be followed by "I", indicating it is imaginary         %
    % the literal can be followed by "LI" or "IL" indicating it is a long    %
    % imaginary number                                                       %
    % an integer literal ( digit sequence ) can also be used where a         %
    % floating point literal is required                                     %
    % non-imaginary examples:                                                %
    r  := 1.23;
    r  := 1.;
    r  := .9;
    r  := 1.23'5;
    r  := 1.'+4;
    r  := .9'-12;
    r  := 7;
    lr := 5.4321L;
end.
```



## Applesoft BASIC


All numeric literals are treated as floating point. (In the Apple II world, Applesoft was sometimes called "floating-point BASIC" to contrast it with [[Integer BASIC]].)


```txt

0
19
-3
29.59
-239.4
1E10
1.9E+09
-6.66E-32

```



## Arturo


```arturo>pi 3.14159265</lang



## AWK

With the One True Awk ([[nawk]]), all numbers are floating-point. A numeric literal consists of one or more digits '0-9', with an optional decimal point '.', followed by an optional exponent. The exponent is a letter 'E' or 'e', then an optional '+' or '-' sign, then one or more digits '0-9'.


```awk
2
2.
.3
45e6
45e+6
78e-9
1.2E34
```


Other implementations of Awk can differ. They might not use floating-point numbers for integers.

This Awk program will detect whether each line of input contains a valid integer.


```awk
/^([0-9]+(\.[0-9]*)?|\.[0-9]+)([Ee][-+]?[0-9]+)?$/ {
	print $0 " is a literal number."
	next
}

{
	print $0 " is not valid."
}
```


A leading plus or minus sign (as in <tt>+23</tt> or <tt>-14</tt>) is not part of the literal; it is a unary operator. This is easy to check if you know that exponentiation has a higher precedence than unary minus; <tt>-14 ** 2</tt> acts like <tt>-(14 ** 2)</tt>, not like <tt>(-14) ** 2</tt>.


## Axe

Axe does not support floating point literals. However, it does support converting floats to integers and vice versa.

```axe
123→float{L₁}
float{L₁}→I
```


Axe does, however, support fixed-point literals.

```axe
12.25→A
```


There are some mathematical operators in Axe that operate specifically on fixed-point numbers.


## BBC BASIC


```bbcbasic
      REM Floating-point literal syntax:
      REM  [-]{digit}[.]{digit}[E[-]{digit}]

      REM Examples:
      PRINT -123.456E-1
      PRINT 1000.0
      PRINT 1E-5

      REM Valid but non-standard examples:
      PRINT 67.
      PRINT 8.9E
      PRINT .33E-
      PRINT -.
```

'''Output:'''

```txt

  -12.3456
      1000
      1E-5
        67
       8.9
      0.33
         0

```



## bc

A literal floating point number can be written as <code>.NUMBER</code>, <code>NUMBER.</code> or <code>NUMBER.NUMBER</code> where <code>NUMBER</code> consists of the hexadecimal digits <code>0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F</code>. If digits in the number are greater than or equal to the current value of <code>ibase</code> (i.e. the input number radix) the behaviour is undefined.
Examples:
```txt
12.34   .34   99.   ABC.DEF
```



## C

Floating-point numbers can be given in decimal or hexadecimal.  Decimal floating-point numbers must have at least one of a decimal point and an exponent part, which is marked by an E:

```txt
((\d*\.\d+|\d+\.)([eE][+-]?[0-9]+)?[flFL]?)|([0-9]+[eE][+-]?[0-9]+[flFL]?)
```

Hexadecimal is similar, but allowing A-F as well as 0-9. They have a binary exponent part marked with a P instead of a decimal exponent:

```txt
(0[xX]([0-9a-fA-F]*\.[0-9a-fA-F]+|[0-9a-fA-F]+\.)([pP][+-]?\d+[flFL]?)|(0[xX][0-9a-fA-F]+[pP][+-]?\d+[flFL]?)
```

<!--
Why does 6.4.4.2 seem to disallow negative floats?  Clearly this isn't the intent... maybe -1.2 is considered to be the unary negation of 1.2?
-->


## C#

Floating point suffixes are not case-sensitive.

```c#
double d = 1;
d = 1d;
d = 1D;
d = 1.2; //double is the default if there's no suffix
d = 1.2d; //The suffix is redundant here
d = .2;
d = 12e-12;
d = 12E-12;
d = 1_234e-1_2; //digit separators are allowed since C# 7
float f = 1;
f = 1f;
f = 1F;
f = 1.2f;
f = .2f;
f = 12e-12f;
f = 12E-12f;
f = 1_234e-1_2f;
decimal m = 1;
m = 1m;
m = 1m;
m = 1.2m;
m = .2m;
m = 12e-12m;
m = 12E-12m;
m = 1_234e-1_2m;
```



## Clojure


Clojure supports both standard and scientific notation.


```txt
user=> 1.
1.0
user=> 1.0
1.0
user=> 3.1415
3.1415
user=> 1.234E-10
1.234E-10
user=> 1e100
1.0E100
user=> (Float/valueOf "1.0f")
1.0
```


Clojure also supports returning ratios (fractions) if you divide integers.  These are not subject to roundoff error.  If you do specify a floating point in the division, it will return a floating point value.

```txt
user=> (/ 1 3)
1/3
user=> (/ 1.0 3)
0.3333333333333333
```



## Common Lisp

The grammar for floating point literals in EBNF (ISO/IEC 14977):

```txt

float = [ sign ], { decimal-digit }, decimal-point, decimal-digit, { decimal-digit }, [exponent]
      | [ sign ], decimal-digit, { decimal-digit }, [ decimal-point, { decimal-digit } ], exponent ;
exponent = exponent-marker, [ sign ], decimal-digit, { decimal-digit } ;
sign = "+" | "-" ;
decimal-point = "." ;
decimal-digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
exponent-marker = "e" | "E" | "s" | "S" | "d" | "D" | "f" | "F" | "l" | "L" ;
```


Common Lisp implementations can provide up to 4 different float subtypes: short-float, single-float, double-float and long-float. The exponent marker specifies the type of the literal. "e"/"E" denotes the default floating point subtype (it is initially single-float but you can set it with the global variable <code>*READ-DEFAULT-FLOAT-FORMAT*</code> to any of the other subtypes).
The standard only recommends a minimum precision and exponent size for each subtype and an implementation doesn't have to provide all of them:

```txt
Format       | Minimum Precision | Minimum Exponent Size
--------------------------------------------------
Short (s/S)  |    13 bits        |      5 bits
Single (f/F) |    24 bits        |      8 bits
Double (d/D) |    50 bits        |      8 bits
Long (l/L)   |    50 bits        |      8 bits
```


Some examples:

```txt
> 1.0
1.0
> -.1
-0.1
> -1e-4
-1.0E-4
> 1d2
100.0d0
> .1f3
100.0
> .001l-300
1.0L-303

```


Note that <code>123.</code> is not a floating point number but an integer:

```txt
> (floatp 123.)
NIL
> (integerp 123.)
T
```



## D

D built-in floating point types include ''float'' (32-bit), ''double'' (64-bit) and ''real'' (machine hardware maximum precision floating point type, 80-bit on x86 machine) and respective complex number types. Here's information for [http://www.d-programming-language.org/lex.html#floatliteral Floating Literals].


## Dyalect


Dyalect built-in types include only one floating point number of type ''Float'' (64-bit). Both regular and scientific notations are supported:


```Dyalect
var x = 42.02
var y = 0.174e-17
```


EBNF grammar for the floating point number is as follows:


```txt
float  = "." digit { digit } [ ("e"|"E") [ "+" | "-" ] digit { digit } ]
    | digit { digit }
    (
        "." digit { digit } [ ( "e" | "E" ) ["+" | "-" ] digit { digit } ]
        | ( "e" | "E") ["+" | "-" ] digit { digit }
    ).
```



## Eiffel

Floating point literals are of the form D.DeSD, where D represents a sequence of decimal digits, and S represents an optional sign. A leading "+" or "-" indicates a unary plus or minus feature and is not considered part of the literal.

'''Examples:'''
```Eiffel

1.
1.23
1e-5
.5
1.23E4

```


## Elena


```elena
real r := 1;
r := 23.2r;
r := 1.2e+11r;
```



## Elixir


```elixir
iex(180)> 0.123
0.123
iex(181)> -123.4
-123.4
iex(182)> 1.23e4
1.23e4
iex(183)> 1.2e-3
0.0012
iex(184)> 1.23E4
1.23e4
iex(185)> 10_000.0
1.0e4
iex(186)> .5
** (SyntaxError) iex:186: syntax error before: '.'

iex(186)> 2. + 3
** (CompileError) iex:186: invalid call 2.+(3)

iex(187)> 1e4
** (SyntaxError) iex:187: syntax error before: e4
```



## Erlang

Floating point literal examples: 1.0 , -1.0 , 1.2e3 , 1.2e-3 and 1.2E3 , 1.2E-3 .


## Euphoria


```euphoria

printf(1,"Exponential:\t%e, %e, %e, %e\n",{-10.1246,10.2356,16.123456789,64.12})
printf(1,"Floating Point\t%03.3f, %04.3f, %+3.3f, %3.3f\n",{-10.1246,10.2356,16.123456789,64.12})
printf(1,"Floating Point or Exponential:  %g, %g, %g, %g\n",{10,16.123456789,64,123456789.123})

```

```txt

Exponential:    -1.012460e+001, 1.023560e+001, 1.612346e+001, 6.412000e+001
Floating Point  -10.125, 10.236, +16.123, 64.120
Floating Point or Exponential:  10, 16.1235, 64, 1.23457e+008

```



## Factor


```factor
3.14           ! basic float
+3.14          ! Optional signs
-3.14
10e5           ! exponents signified by e or E
10E+5          ! with optional signs
+10e-5
1.             ! equivalent to 1.0
.5             ! equivalent to 0.5
1/2.           ! floating point approximation of a ratio (0.5)
1/3.           ! 0.3333333333333333
1/0.           ! positive infinity
-1/0.          ! negative infinity
0/0.           ! not-a-number
               ! hexadecimal, octal, and binary float literals are supported.
               ! they require a base 2 exponent expressed as a decimal
               ! preceded by p or P.
0x1.0p3        ! 8.0
-0x1.0P-3      ! -0.125
0b1.010001p3   ! 10.125
0o1.21p3       ! 10.125
               ! comma separators are allowed
1,234.123,456  ! 1234.123456


! normalized hex form ±0x1.MMMMMMMMMMMMMp±EEEE allows any floating-point
! number to be specified precisely according to IEEE 754 representation
+0x1.1234567891234p+0002   ! 4.28444444440952
```



## Forth

Unlike most other languages, floating point literals in Forth are distinguished by their exponent ('E' or 'e') rather than their decimal point ('.' or ',').  Numeric literals with just a decimal point are regarded as two-cell double precision integers.  From the ANS Forth standards document:


```txt
Convertible string := <significand><exponent>

<significand> := [<sign>]<digits>[.<digits0>]
<exponent>    := E[<sign>]<digits0>
<sign>        := { + | - }
<digits>      := <digit><digits0>
<digits0>     := <digit>*
<digit>       := { 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 }
```

These are examples of valid representations of floating-point numbers in program source:


```txt
	1E   1.E   1.E0   +1.23E-1   -1.23E+1
```



## Fortran

Floating-point literals involve a decimal point, otherwise they're integers. The rule is <sign><digits><.><digits><exponent> with each optional - except that there must be ''some'' digits! Spaces are irrelevant in source files, so 3 .141 159 would be acceptable, however when data are read, internal spaces are not allowed so "- 3.14" would be rejected - at least for free-format (or "list") style input. With formatted input, spaces are considered to be zeroes and a data field lacking a decimal point can have one assumed so that "  31" read by F4.1 would yield 3.1. There is no requirement that there be digits before the decimal point, nor digits after the decimal point (if there are digits before), so .5 and 5. are both acceptable.

The status of the sign is delicate, being a matter of context. In a DATA statement or in an assignment such as x = -5.5, the sign is a part of the number, but not in an arithmetic expression such as y = x*-5.5 which has two operators in a row and is rejected. x*(-5.5) is accepted and the sign is a part of the number.

The exponent part signifies a power of ten and if present, has the form <E or D><sign><integer>, the sign optional, where E signifies a single-precision number and D a double-precision number, irrespective of the number of digits offered in the number. Thus, a constant 3.14159265 will be double-precision only if there follows a D, presumably with a zero exponent. As a result, 1.0D0 is not the same as 1.0E0, even though they are equal, and a calculation such as 4*atan(1.0) will be in single precision unless it is 4*atan(1.0D0) or similar. Some compilers offer an option to regard all constants as being in double precision irrespective of E or D, but in the absence of that, 1.15 or 1.15E0 will ''not'' equal 1.15D0 because most decimal fractions are recurring sequences in binary, and if such constants were assigned to suitable variables and printed with one decimal digit, then on common computers, the double-precision value will come out as 10.2 because with 53-bit precision its value is (exactly) 10·1500000000000003552713678800500929355621337890625, which rounds up, while in single precision it is (exactly) 10·1499996185302734375, which rounds down.

There are also options for specifying constants as hexadecimal sequences, and if assigned to a floating-point variable, then Z"FFFFFFFFFFFFFFFF" will generate a (double precision) NaN value while Z"FFF0000000000000" will generate negative infinity, on cpus supporting such features. Similarly with octal and binary sequences. In these cases, the bit patterns are as they will be in the floating-point format, not as a number expressed in hexadecimal, etc. Thus, pi = Z"40490FDB" or 1000000010010010000111111011011, which is not 11.00100100001111110... at all.

Complex number constants are typically specified as (x,y) where x and y are floating-point literals; there is no provision for complex integers.


## FreeBASIC

FreeBASIC has two floating point types : Single (4 bytes) and Double (8 bytes)

Numeric literals of these types can be specified by using the following suffixes:

Single !, f or F   :   Double #

However, this is not usually necessary as the compiler will automatically infer the type from the context and the two types are implicitly convertible to each other or explicitly convertible using the CSng or CDbl functions. However, conversions from Double to Single may lose precision.

All numeric literals which include a decimal point or exponent (i.e. scientific notation) are considered to be of floating point rather than integral type and are generally of the form:

number[.[fraction]][((D|E) [+|-] exponent)|(D|E)|][suffix]

or

.fraction[((D|E) [+|-] exponent)|(D|E)|][suffix]

Where scientific notation is used, D denotes  Double precision and E denotes default precision , though these can be over-ridden by the suffix, if there is one. They can also be used on their own, without a following exponent.

The default precision is Double unless the 'QB' dialect of the language is used (for compatibility with QuickBasic code) where numbers of no more than 7 digits are considered to be Single precision.

Some examples, taken from the language documentation follow:


```freebasic
' FB 1.05.0 Win64 (default dialect)

Dim a As Double = 123.456
Dim b As Double = -123.0
Dim c As Double = -123.0d
Dim d As Double = -123e
Dim e As Double = 743.1e+13
Dim f As Double = 743.1D-13
Dim g As Double = 743.1E13
Dim h As Single = 743D!    Rem ! overrides D
Dim i As Single = 3.1!
Dim j As Single = -123.456e-7f
Dim k As Double = 0#
Dim l As Double = 3.141592653589e3#
```



## GAP



```gap
-3.14
22.03e4
4.54e-5
```



## gecho


```gecho

0.0
-1
-1.2
-1.4324
3 4 /

```



## Go

See [http://golang.org/doc/go_spec.html#Floating-point_literals relevant section] of language reference.  Basically they are base 10, need either a decimal point or an exponent, and specify no precision or representation.  The exponent can be signed, but the mantissa is not.  One of the integer part or the fractional part may be elided; one of the decimal point or the exponent may be elided.  A leading minus sign would be an operator and not part of the floating point literal.  Examples,

```txt
0.
0.0
.0
1e3
1e-300
6.02E+23
```



## Groovy

Solution:

```groovy
println 1.00f    // float (IEEE-32)
println 1.00d    // double (IEEE-64)
println 1.00     // BigDecimal (scaled BigInteger)
println 1.00g    // BigDecimal
println 1.00e0   // BigDecimal

assert 1.00f  instanceof Float
assert 1.00d  instanceof Double
assert 1.00   instanceof BigDecimal
assert 1.00g  instanceof BigDecimal
assert 1.00e0 instanceof BigDecimal
```


```txt
1.0
1.0
1.00
1.00
1.00
```



## Haskell

Haskell supports decimal representation of float literals, with or without an exponent. For more information, see the [http://www.haskell.org/onlinereport/lexemes.html#sect2.5 relevant portion] of the Haskell 98 Report.


```haskell
main = print [0.1,23.3,35e-1,56E+2,14.67e1]

```


Output:

```txt
[0.1,23.3,3.5,5600.0,146.7]
```


=={{header|Icon}} and {{header|Unicon}}==
Real literals can be represented in two forms by (a) decimal literals, or (b) exponent literals.  There is no sign as + and - are unary operators.

The program below shows a full range of valid real literals.

```Icon
procedure main()
every write( ![  1., .1, 0.1,  2e10, 2E10, 3e-1, .4e2, 1.41e2, 8.e+3, 3.141e43 ])
end
```


The function write will cause the real values to be coerced as string constants.  Icon/Unicon will format these as it sees fit resorting to exponent forms only where needed.
[http://www.cs.arizona.edu/icon/library/src/procs/printf The IPL library routine printf provides a broader range of formatting choices.]


## J


This paragraph highlights current implementation specific details of internal types:  J has a syntax for specifying numbers, but numeric constants are stored in their most compact implementation; for example, <code>2.1</code> is a floating point number, while <code>2.0</code> is an integer and <code>1.0</code> is a boolean.  If the exact type of a value is important, an expression may be used; for example, <code>1.1-0.1</code> produces a floating point result.

J's numeric constant mini-language allows the specification of numbers which are not floating point, but as indicated above, numeric type in J is a semantic triviality and not a syntactic feature.  (And this pervades the language.  For example, 1+1 is 2, despite the result having a different type from both of the arguments.  Or, for example, if maxint is the largest value represented using an integer type, maxint+1 will produce a floating point result instead of an error or a wraparound.)

Here is an informal bnf for J's numeric constant language.  Note, however, that the implementation may disallow some unusual cases -- cases which are not treated as exceptional here (for example, the [http://jsoftware.com/help/dictionary/dcons.htm language specification] allows 1.2e3.4 but the current implementation does not support fractional powers of 10 in numeric constants):


```bnf
numeric-constant ::= number-constant | number-constant whitespace numeric-constant
whitespace ::= whitespacecharacter | whitespacecharacter whitespace
whitespacecharacter ::= ' ' | TAB
TAB is ascii 9
number-constant ::= arbitrary-constant | arbitrary-constant base-token base-constant
base-token ::= 'b' | 'b-'
base-constant ::= base-digits | base-digits '.' base-digits
base-digits ::= base-digit | base-digit base-digits
base-digit ::= digit | alpha1 | alpha2
alpha1 ::= 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'
alpha2 ::= 'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z'
arbitrary-constant ::= complex-constant | pi-constant | euler-constant | extended-constant
pi-constant ::= complex-constant 'p' complex-constant
euler-constant ::= complex-constant 'x' complex-constant
extended-constant ::= signed-digits 'x' | signed-digits 'r' signed-digits
complex-constant ::= exponential-constant | exponential-constant complex-token exponential-constant
complex-token ::= 'ad' | 'ar' | 'j'
exponential-constant ::= signed-constant | signed-constant 'e' signed-constant
signed-constant ::= decimal-constant | '_' decimal-constant
decimal-constant ::= digits | digits '.' digits
signed-digits ::= digits | '_' digits
digits ::= digit | digit digits
digit ::= '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'
```


e indicates exponential or scientific notation (number on left multiplied by 10 raised to power indicated by number on right)

ad, ar and j are used to describe complex numbers (angle in degrees, in radians, and rectangular form)

p and infix x are analogous to e except the base is pi or the base of natural logarithms

r and x are also used for arbitrary precision numbers, r indication a ration and a trailing x indicating an extended precision integer.

b is used for arbitrary bases, and letters a-z indicate digit values 10 through 35 when they follow a b

Floating point examples:


```j
   0 1 _2 3.4 3e4 3p4 3x4
0 1 _2 3.4 30000 292.227 163.794
   16bcafe.babe _16b_cafe.babe _10b11
51966.7 46818.7 _9
```


Note that all the values in an array are the same type, thus the 0, 1 and 2 in the above example are floating point because they do not appear by themselves.  Note also that by default J displays no more than six significant digits of floating point values.


## jq

jq floating point literals are identical to JSON floating point literals.  However, when jq parses a floating point or integer literal, conversion to IEEE 754 numbers takes place, which may result in a loss of accuracy and/or an apparent change of type, as illustrated by the following sequence of input => output pairs:

```sh>1.0 =
 1
1.2 => 1.2
1e10 => 10000000000
1e100 => 1e+100
1e1234 => 1.7976931348623157e+308
.1 => 0.1
.1e1 => 1
```



## Java


```java
1. //double equal to 1.0
1.0 //double
2432311.7567374 //double
1.234E-10 //double
1.234e-10 //double
758832d //double
728832f //float
1.0f //float
758832D //double
728832F //float
1.0F //float
1 / 2. //double
1 / 2 //int equal to 0
```

Values that are outside the bounds of a type will give compiler errors when trying to force them to that type.


## Julia

```julia
0.1
.1
1.
1e-1    # scientific notation
1e+10
1e-10
0x01p-1 # hex float
```



## Kotlin


```scala
val d: Double = 1.0
val d2: Double = 1.234e-10
val f: Float = 728832f
val f2: Float = 728832F
```



## Lasso


```Lasso
0.0
0.1
-0.1
1.2e3
1.3e+3
1.2e-3
```



## Lingo


```lingo
put 0.23
-- 0.2300

-- activate higher printing precision
the floatPrecision = 8

put -.23
-- -0.23000000

put 9.00719925474099e15
-- 9.00719925474099e15

-- result is NOT a float
put 2/3
-- 0

-- casting integer to float
put float(2)/3
-- 0.66666667

-- casting string to float
put float("0.23")
-- 0.23000000
```



## Lua


```lua
3.14159
314.159E-2
```



## M2000 Interpreter

We can use Decimal using @ and Currency using # (no exponent part, both types)


```M2000 Interpreter

Def ExpType$(x)=Type$(x)
Print ExpType$(-12)="Double", -12
Print ExpType$(12.)="Double", 12.
Print ExpType$(12.e-5)="Double", 12.e-5
Print ExpType$(.1)="Double", .1
Print ExpType$(-12~)="Single", -12~
Print ExpType$(12.~)="Single", 12.~
Print ExpType$(12.e-5~)="Single", 12.e-5~
Print ExpType$(.1~)="Single", .1~

```



## Maple

Maple distinguishes "software floats" (of arbitrary precision) and "hardware floats" (of machine precision).  To get the latter, use the "HFloat" constructor.

```Maple

> 123.456; # decimal notation
                                123.456

> 1.23456e2; # scientific notation
                                123.456

> Float( 23, -2 ); # float constructor notation, by mantissa and exponent
                                  0.23

> Float( .123456, 3 ); # again
                                123.456

> Float( 1.23456, 2 ); # again
                                123.456

> Float( 12.3456, 1 ); # again
                                123.456

> HFloat( 1.23456, 2 ); # hardware float constructor
                            123.456000000000

> HFloat( 123.456 ); # again
                            123.456000000000

> 2.3^30; # large floats are printed using scientific notation
                                          11
                           0.7109434879 10

> 2/3; # NOT a float!
                                  2/3

> evalf( 2/3 ); # but you can get one
                              0.6666666667

> 0.0; # zero
                                   0.

> -0.0; # negative zero
                                  -0.

> Float(infinity); # positive infinity
                            Float(infinity)

> Float(-infinity); # minus infinity
                            Float(-infinity)

> Float(undefined); # "NaN", not-a-number
                            Float(undefined)

```

Whether a given float is a software or hardware float can be determined by using "type".

```Maple

> type( 2.3, 'hfloat' );
                                 false

> type( HFloat( 2.3 ), 'hfloat' );
                                  true

```

(There is also a type "sfloat" for software floats, and the type "float", which covers both.)


## Mathematica


```Mathematica
These numbers are given in the default output format. Large numbers are given in scientific notation.
{6.7^-4,6.7^6,6.7^8}
{0.00049625,90458.4,4.06068*10^6}

This gives all numbers in scientific notation.
ScientificForm[%]
{4.9625*10^(-4),9.04584*10^(4),4.06068*10^(6)}

This gives the numbers in engineering notation, with exponents arranged to be multiples of three.
EngineeringForm[%]
{496.25*10^(-6),90.4584*10^(3),4.06068*10^(6)}

In accounting form, negative numbers are given in parentheses, and scientific notation is never used.
AccountingForm[{5.6,-6.7,10.^7}]
{5.6,(6.7),10000000.}
```



## Maxima


```maxima
/* Maxima has machine floating point (usually double precision IEEE 754), and
arbitrary length "big floats" */

/* Here are ordinary floats */
3.14159
2.718e0
1.2345d10
1.2345e10
1.2345f10

/* And big floats (always with a "b" for the exponent) */
3.14159b0
2.718b0
1.2345b10

/* Before computing with big float, one must set precision to some value (default is 16 decimal digits) */
fpprec: 40$

bfloat(%pi);
3.141592653589793238462643383279502884197b0
```



## Nemerle


```txt
3.14f                                   // float literal
3.14d, 3.14                             // double literal
3.14m                                   // decimal literal
```


Formally (from the [http://nemerle.org/wiki/index.php?title=Lexical_structure_%28ref%29 Reference Manual]):

```txt
<floating_point_literal> ::=
	[ <digits_> ] '.' <digits_> [ <exponent> ] [ <suffix> ]
|       <digits_> <exponent> [ <suffix> ]
|       <digits_> <suffix>
<exponent> ::=
	<exponential_marker> [ <sign> ] <digits>
<digits> ::=
	{ <digit> }
<digits_> ::=
	<digits> [ { '_' <digits> } ]
<exponential_marker> ::=
	'e'
|       'E'
<sign> ::=
	'+'
|       '-'
<digit> ::=
	<decimal_digit>
<suffix> ::=
	<floating_point_suffix>
<floating_point_suffix> ::=
	'f'
|       'd'
|       'm'
```



## NetRexx

NetRexx supports ''decimal'' and ''exponential'' notation for floating point constants.
A number in ''exponential notation'' is a simple number followed immediately by the
sequence &quot;<tt>E</tt>&quot; (or &quot;<tt>e</tt>&quot;), followed immediately
by a sign (&quot;<tt>+</tt>&quot; or &quot;<tt>-</tt>&quot;), followed immediately
by one or more digits.

NetRexx supports floating point number notation in the primitive ''float'' and ''double'' types,
it's built in ''Rexx'' object and any other Java object that supports floating point numbers.


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

numeric digits 40 -- make lots of space for big numbers
numeric form scientific -- set output form for exponential notation

say 'Sample using objects of type "Rexx" (default):'
fv =      1.5; say      '1.5'.right(20) '==' normalize(fv).right(20) --            1.5
fv =     -1.5; say     '-1.5'.right(20) '==' normalize(fv).right(20) --           -1.5
fv =    15e-1; say    '15e-1'.right(20) '==' normalize(fv).right(20) --            1.5
fv =    3e-12; say    '3e-12'.right(20) '==' normalize(fv).right(20) --          3E-12
fv =    3e+12; say    '3e+12'.right(20) '==' normalize(fv).right(20) --  3000000000000
fv = 17.3E-12; say '17.3E-12'.right(20) '==' normalize(fv).right(20) --       1.73E-11
fv = 17.3E+12; say '17.3E+12'.right(20) '==' normalize(fv).right(20) -- 17300000000000
fv = 17.3E+40; say '17.3E+40'.right(20) '==' normalize(fv).right(20) --       1.73E+41
fv = 0.033e+9; say '0.033e+9'.right(20) '==' normalize(fv).right(20) --       33000000
fv = 0.033e-9; say '0.033e-9'.right(20) '==' normalize(fv).right(20) --        3.3E-11
say

say 'Sample using primitive type "float":'
ff = float
ff = float    15e-1; say    '15e-1'.right(20) '==' normalize(ff).right(20) --            1.5
ff = float 17.3E-12; say '17.3E-12'.right(20) '==' normalize(ff).right(20) --       1.73E-11
ff = float 17.3E+12; say '17.3E+12'.right(20) '==' normalize(ff).right(20) -- 17300000000000
ff = float 0.033E+9; say '0.033E+9'.right(20) '==' normalize(ff).right(20) --       33000000
ff = float 0.033E-9; say '0.033E-9'.right(20) '==' normalize(ff).right(20) --        3.3E-11
say

say 'Sample using primitive type "double":'
fd = double
fd =    15e-1; say    '15e-1'.right(20) '==' normalize(fd).right(20) --            1.5
fd = 17.3E-12; say '17.3E-12'.right(20) '==' normalize(fd).right(20) --       1.73E-11
fd = 17.3E+12; say '17.3E+12'.right(20) '==' normalize(fd).right(20) -- 17300000000000
fd = 17.3E+40; say '17.3E+40'.right(20) '==' normalize(fd).right(20) --       1.73E+41
fd = 0.033E+9; say '0.033E+9'.right(20) '==' normalize(fd).right(20) --       33000000
fd = 0.033E-9; say '0.033E-9'.right(20) '==' normalize(fd).right(20) --        3.3E-11
say

return

/**
 * Convert input to a Rexx object and add zero to the value which forces NetRexx to change its internal representation
 *
 * @param fv a Rexx object containing the floating point value
 * @return a Rexx object which allows NetRexx string manipulation methods to act on it
 */
method normalize(fv) private constant
  return fv + 0

```

'''Output:'''

```txt

Sample using objects of type "Rexx" (default):
                 1.5 ==                  1.5
                -1.5 ==                 -1.5
               15e-1 ==                  1.5
               3e-12 ==                3E-12
               3e+12 ==        3000000000000
            17.3E-12 ==             1.73E-11
            17.3E+12 ==       17300000000000
            17.3E+40 ==             1.73E+41
            0.033e+9 ==             33000000
            0.033e-9 ==              3.3E-11

Sample using primitive type "float":
               15e-1 ==                  1.5
            17.3E-12 ==             1.73E-11
            17.3E+12 ==       17300000000000
            0.033E+9 ==             33000000
            0.033E-9 ==              3.3E-11

Sample using primitive type "double":
               15e-1 ==                  1.5
            17.3E-12 ==             1.73E-11
            17.3E+12 ==       17300000000000
            17.3E+40 ==             1.73E+41
            0.033E+9 ==             33000000
            0.033E-9 ==              3.3E-11

```



## Nim


```nim
var x: float
x = 2.3
x = 2.0
x = 0.3
x = 123_456_789.000_000_1
x = 2e10
x = 2.5e10
x = 2.523_123E10
x = 5.2e-10

var y = 2'f32 # Automatically a float32
var z = 2'f64 # Automatically a float64

```



## Objeck


```objeck

3 + .14159
3.14159
314.159E-2

```



## OCaml


In the OCaml manual, the chapter '''lexical conventions''' describes [http://caml.inria.fr/pub/docs/manual-ocaml/lex.html#float-literal floating-point literals], which are:

 float-literal  ::=   [-] (0…9) { 0…9∣ _ } [. { 0…9∣ _ }] [(e∣ E) [+∣ -] (0…9) { 0…9∣ _ }]

Here are some examples:


```ocaml
0.5
1.0
1.    (* it is not possible to write only "1" because OCaml is strongly typed,
         and this would be interpreted as an integer *)
1e-10
3.14159_26535_89793
```



## Oforth


A literal floating point number is written with a . and with or without an exponential notation :


```Oforth
3.14
1.0e-12
0.13
1000.0
.22
```



## PARI/GP

Similar to C, but allowing only decimal. Also, GP allows a trailing decimal point:

```txt
[+-]?((\d*\.\d+\b)|(\d+(\.\d*)?[Ee][+-]?\d+\b)|-?(\.\d+[Ee][+-]?\d+\b)|(\d+\.))
```


PARI t_REAL numbers have a maximum value of

{|class="wikitable"
! 32-bit
| <math>2^{2^{29}}(1-\epsilon)</math>
| 161,614,249 decimal digits
|-
! 64-bit
| <math>2^{2^{61}}(1-\epsilon)</math>
| 694,127,911,065,419,642 decimal digits
|}
where <math>\epsilon</math> is the machine epsilon at the selected precision. The minimum value is the opposite of the maximum value (reverse the sign bit).

```txt
0.0
0.  \\ == 0.0
.0  \\ == 0.0
.   \\ == 0.0
2e2
6.02e23
-2e48
1e-9
1e0
```



## Pascal


```txt

1.345
-0.5345
5.34e-34

```



## Perl


```perl
# Standard notations:
.5;
0.5;
1.23345e10;
1.23445e-10;
# The numbers can be grouped:
100_000_000;	# equals to 100000000

```



## Perl 6

Floating point numbers (the Num type) are written in the standard 'e' scientific notation:

```perl6
2e2      # same as 200e0, 2e2, 200.0e0  and 2.0e2
6.02e23
-2e48
1e-9
1e0
```


A number like <tt>3.1416</tt> is specifically not floating point, but rational (the Rat type), equivalent to <tt>3927/1250</tt>.  On the other hand, <tt>Num(3.1416)</tt> would be considered a floating literal though by virtue of mandatory constant folding.


## Phix

Phix does not require any distinction between integers and floats: 5 and 5.0 are exactly the same.
A variable declared as atom can hold an integer or a floating point value.

Division and other operators do what a sensible language should, eg 1/3 is 0.333333, not 0. [for the latter use floor(1/3)]

Floats cannot be expressed in any base other than decimal. They may optinally include a sign for mantissa and/or exponent.

It is not necessary for a digit to precede a decimal point, but one must follow it. Upper or lower e/g may be used.

In the 32-bit version, integers outside -1,073,741,824 to +1,073,741,823 must be stored as atoms.
In the 64-bit version the limits of integers are -4,611,686,018,427,387,904 to +4,611,686,018,427,387,903.

On a 32-bit architecture floats can range from approximately -1e308 to +1e308 with 15 decimal digits,
and on a 64-bit architecture they can range from approximately -1e4932 to +1e4932 with 19 decimal digits.

The included bigatom library allows working with extremely large integers and floats with arbitrary precision. In the following, '?x' is the Phix shorthand for 'print(1,x)', plus \n

```Phix
?1e+12  -- (same as 1e12)
?1e-12
?5      -- (same as 5.0)
--?1.   -- (illegal, use 1 or 1.0)
?.1     -- (same as 0.1)
?1/3    -- 0.333333
printf(1,"%g %G\n",1e-30)
```

```txt

1e+12
1e-12
5
0.1
0.3333333333
1e-30 1E-30

```



## PHP

More [http://php.net/manual/en/language.types.float.php information] about floating point numbers in PHP.

```PHP
.12
0.1234
1.2e3
7E-10

```

Formal representation:

```txt

LNUM          [0-9]+
DNUM          ([0-9]*[\.]{LNUM}) | ({LNUM}[\.][0-9]*)
EXPONENT_DNUM [+-]?(({LNUM} | {DNUM}) [eE][+-]? {LNUM})

```



## PicoLisp


PicoLisp does not support floating point literals in the base language, only
fixed point (scaled) decimal integers of unlimited size and precision. See
[http://software-lab.de/doc/ref.html#num-io Numbers] in the reference.


## PL/I


```PL/I

1.2345e-4      decimal floating-point
7e5            decimal floating-point
1.234_567_89e0 decimal floating-point.
1.0s0          decimal floating-point (single precision)
1.0d0          decimal floating-point (double precision)
1.34q0         decimal floating-point (quadruple/extended precision)

111.0101e7b    binary floating-point equals 111.0101 * 2**7
                                     or 7.3125 * 2**7
1e5b           binary floating-point equals 1 * 2**5

```



## PureBasic

Floating point literals do not need a decimal point if an exponent is used.  They may also include a sign for the number or exponent.

```txt
-1.0   1.0  1.0E2  1.0E+2  1.0E-2  -1E2
```



## Python

This is an excerpt of an ANTLR grammar for python obtained from [http://www.antlr.org/grammar/1200715779785/Python.g here].


```ebnf
FLOAT
    :   '.' DIGITS (Exponent)?
    |   DIGITS '.' Exponent
    |   DIGITS ('.' (DIGITS (Exponent)?)? | Exponent)
    ;

DIGITS : ( '0' .. '9' )+ ;

Exponent
    :    ('e' | 'E') ( '+' | '-' )? DIGITS
    ;
```


Examples

```python

2.3    # 2.2999999999999998
.3     # 0.29999999999999999
.3e4   # 3000.0
.3e+34 # 2.9999999999999998e+33
.3e-34 # 2.9999999999999999e-35
2.e34  # 1.9999999999999999e+34

```



## Racket



```racket

#lang racket
.2
2.
2.+0i  ; zero imaginary part
2e0
#x10.8 ; hex float
#o1e2  ; oct float
2.0f0  ; single float
1.0t0  ; extended 80-bit float (when available on platform)

```


Output:

```txt

0.2
2.0
2.0
2.0
16.5
64.0
2.0f0
1.0t0

```



## REXX

All values in REXX are character strings,   so a value could hold such things as these (decimal) numbers:

```rexx
something = 127
something = '127'     /*exactly the same as the above. */
something = 1.27e2
something = 1.27E2
something = 1.27E+2
something = '    +    0001.27e+00000000000000002     '
```

To forcibly express a value in exponential notation,   REXX has a built-in function   '''format'''   that can be used.

Note that a value of   '''0'''   (zero)   in any form is always converted to

```txt

  0

```

by the   '''format'''   BIF.

```rexx
something = -.00478
say something
say format(something,,,,0)
```

'''output'''

```txt

-0.00478
-4.78E-3

```

The last invocation of   '''format'''   (above,   with the 5th parameter equal to zero)   forces exponential notation,   unless the exponent is   '''0'''   (zero),   then exponential notation won't be used.

There are other options for the   '''format'''   BIF to force any number of digits before and/or after the decimal point,   and/or specifying the number of digits in the exponent.




## Ruby

A Float literal is an optional sign followed by one or more digits and a dot, one or more digits and an optional exponent (e or E followed by an optional sign and one or more digits). Unlike many languages .1 is not a valid float.

Underscores can be used for clarity:
1_000_000_000.01


## Rust

The fractional part may be elided (so 1. is valid) but the integer part may not (so .0 is not valid).

```rust
2.3         // Normal floating point literal
3.          // Equivalent to 3.0 (3 would be interpreted as an integer)
2f64        // The type (in this case f64, a 64-bit floating point number) may be appended to the value
1_000.2_f32 // Underscores may appear anywhere in the number for clarity.
```



## Scala

As all values in Scala, values are boxed with wrapper classes. The compiler will unbox them to primitive types for run-time execution.

```Scala
1. //Double equal to 1.0
1.0 //Double, a 64-bit IEEE-754 floating point number (equivalent to Java's double primitive type)
2432311.7567374 //Double
1.234E-10 //Double
1.234e-10 //Double
758832d //Double
728832f //32-bit IEEE-754 floating point number (equivalent to Java's float primitive type)
1.0f //Float
758832D //Double
728832F //Float
1.0F //Float
1 / 2. //Double
1 / 2 //Int equal to 0

// Constants
Float.MinPositiveValue
Float.NaN
Float.PositiveInfinity
Float.NegativeInfinity

Double.MinPositiveValue
Double.NaN
Double.PositiveInfinity
Double.NegativeInfinity

```

Values that are outside the bounds of a type will give compiler-time errors when trying to force them to that type.


## Scheme


```scheme

.2      ; 0.2
2.      ; 2.0
2e3     ; 2000
2.+3.i  ; complex floating-point number

; in Scheme, floating-point numbers are inexact numbers
(inexact? 2.)
; #t
(inexact? 2)
; #f
```



## Seed7

The type [http://seed7.sourceforge.net/libraries/float.htm float] consists of single precision floating point numbers. Float literals are base 10 and contain a decimal point. There must be at least one digit before and after the decimal point. An exponent part, which is introduced with E or e, is optional. The exponent can be signed, but the mantissa is not. A literal does not have a sign, + or - are unary operations. Examples of float literals are:

```seed7

3.14159265358979
1.0E-12
0.1234

```

The functions [http://seed7.sourceforge.net/libraries/float.htm#str%28ref_float%29 str] and the operators [http://seed7.sourceforge.net/libraries/float.htm#%28ref_float%29digits%28ref_integer%29 digits] and [http://seed7.sourceforge.net/libraries/float.htm#%28attr_float%29parse%28in_string%29 parse] create and accept float literals with sign.

Original source: [http://seed7.sourceforge.net/manual/types.htm#float]


## Sidef


```ruby
say 1.234;
say .1234;
say 1234e-5;
say 12.34e5;
```

```txt
1.234
0.1234
0.01234
1234000
```



## Smalltalk


```smalltalk
2.0
45e6
45e+6
78e-9
1.2E34
```


base 2 mantissa:

```smalltalk
2r1010.0  -> 10.0
2r0.01      -> 0.25
2r1010e5 -> 320.0
```


base 2 mantissa and base 2 exponent:

```smalltalk
2r1010e2r0101 -> 320.0
```


## Stata

Only decimal floating-point are supported, computations are done in double precision (but storage can be made in integer or single floating-point as well).

Examples:

```stata
.3
1.5
-1.5e10
3.15e-100
```



## Swift


```Swift
let double = 1.0 as Double  // Double precision
let float = 1.0 as Float // Single precision
let scientific = 1.0E-12

// Swift does not feature type coercion for explicit type declaration
let sum = double + float // Error

let div = 1.1 / 2 // Double
let div1 = 1 / 2 // 0
```



## Tcl

Floating point literals in Tcl always contain either “<tt>.</tt>” or “<tt>e</tt>” (of any case), if not both, or are references to either one of the IEEE infinities or NaN. Formally, they are values that (case-insensitively) match one these regular expressions:
;Normal values
:<code>[-+]?[0-9]*\.[0-9]+(e[-+]?[0-9]+)?</code>
:<code>[-+]?[0-9]+\.?e[-+]?[0-9]+</code>
;Infinite values
:<code>[-+]?inf(inity)?</code>
;NaN values
:<code>[-+]?nan(\([0-9a-f]+\))?</code>
Note also that NaN values usually result in checked exceptions; they are supported mainly so that they can be handled when parsing and generating external binary data. All other floating-point literals are fully legal values. (Also note that this excludes the definition of integer literals; for details see [http://www.tcl.tk/cgi-bin/tct/tip/249.html this TIP document], which explains the whole state machine.)


## Ursa

Cygnus/X Ursa (the standard Ursa interpreter) is written in Java and supports Java style floating-point literals.

```ursa
1.
1.0
2432311.7567374
1.234E-10
1.234e-10
758832d
728832f
1.0f
758832D
728832F
1.0F
```



## Verbexx


```verbexx
//    Floating-point Literals:
//
//    If present,the exponent must be of the form:
//
//         eNNN...N
//         ENNN...N
//         e-NNN...N
//         E-NNN...N
//         e+NNN...N
//         E+NNN...N
//
//    If present, length suffix must be:
//
//         f     F               (FLOAT64_T)
//         f32   F32             (FLOAT32_T)
//         f64   F64             (FLOAT64_T)
//         fd    Fd   fD   FD    (FLOATD_T) -- boost::multiprecision::
//                                             cpp_dec_float<100, int64_t>
//
//    The presence of "." "E" "e" "F" or "f" indicates a floating point literal.
//
//    A literal can start with "-"  "."  or a decimal digit, but not "+" or "_".
//    There must be at least one digit, so forms like ".F" ".e+11_f32" or just "."
//    are not recognized as floating point literals.
//
//    Floating-point literal examples:

@SAY   0.       .0       0.0      1.        .1       123.123                         ;// FLOAT64_T
@SAY  -0.      -.0      -0.0     -1.       -.1      -123.123                         ;// FLOAT64_T

@SAY  -0.E1    .0e0     0.0E6    -1.e6     -.1E8     12.12e44     -0E0     1e20      ;// FLOAT64_T
@SAY  -0.e+1   .0E+0    0.0e+6   -1.E+6    -.1e+8    12.12E+44    -0e+0    1E+20     ;// FLOAT64_T
@SAY  -0.E-1   .0e-0    0.0E-6   -1.e-6    -.1E-8    12.12e-44    -0E-0    1e-20     ;// FLOAT64_T
@SAY  -0E9999999999999999  .0e+9999999999  0.E-999999999999999999                    ;// FLOAT64_T
@SAY  -8e0000000000000299  .6E+0000000299  5.e-000000000000000299                    ;// FLOAT64_T

@SAY   0f   -0f   0F   -0f   .0F   -1234f     12.F     12.34f                        ;// FLOAT64_T
@SAY   0F32 -0f32 0f32 -0f32 .0f32 -1234F32   12.f32   12.34F32                      ;// FLOAT32_T
@SAY   0f64 -0f64 0F64 -0f64 .0F64 -1234f64   12.F64   12.34f64                      ;// FLOAT64_T
@SAY   0fD  -0fd  0FD  -0fd  .0Fd  -1234fD    12.FD    12.34fD                       ;// FLOATD_T

@SAY  -0.E1f    .0e0F    0.0E6f64  -1.e6F64  -.1E8f    12.12e44f64  -0E0F    1e20f64 ;// FLOAT64_T
@SAY  -0.e+1f32 .0E+0F32 0.0e+6f32 -1.E+6F32 -.1e+8f32 12.12E+34F32 -0e+0f32 1E+20F32;// FLOAT32_T
@SAY  -0.E-1fd  .0e-0fD  0.0E-6Fd  -1.e-6FD  -.1E-8fd  12.12e-44fD  -0E-0Fd  1e-20FD ;// FLOATD_T
@SAY  -0E9999999999999999f32 .0e+9999999999F32    0.E-999999999999999999f32          ;// FLOAT32_T
@SAY  -8e0000000000000299f   .6E+0000000299f64    5.e-00000000000000000000299F64     ;// FLOAT64_T
@SAY  -8e9999999999999999fD  .6E+99999999999fD    5.e-12345678987654321FD            ;// FLOATD_T

//  note: _ (underscores) can appear in the main numeric part of the literal
//        after the first digit and before any length suffix:

@SAY   -10_000__f    1__0._55__    -1__.__   .0___44    1_._2__E-23F32 debug:;

//        Underscores can also appear in the exponent, after the first digit:

@SAY -1_E-0__2_f32     1.e+0___5_5____   -1.0_E123_456_789_987_654_321__fD debug:;
```



## Vim Script

There are two ways to write floating point literals:
* <code>[-+]?[0-9]+\.[0-9]+</code>
* <code>[-+]?[0-9]+\.[0-9]+[eE][-+]?[0-9]+</code>

Examples: <code>12.34  +0.34  -1.0  12.34e5  0.99e-2  -1.0E+4</code>

Note that there must always be at least one digit before and after the period (and for the exponent).


## Visual Basic

```vb
Sub Main()
Dim d As Double ' 8 Bytes, type specifier = #
Dim s As Single ' 4 Bytes, type specifier = !
  d = -12.3456
  d = 1000#
  d = 0.00001
  d = 67#
  d = 8.9
  d = 0.33
  d = 0#
  d = 2# * 10 ^ 3
  d = 2E+50
  d = 2E-50
  s = -12.3456!
  s = 1000!
  s = 0.00001!
  s = 67!
  s = 8.9!
  s = 0.33!
  s = 0!
  s = 2! * 10 ^ 3
End Sub
```

There is no built-in support for not-a-number, but here's a way to handle that anyway:

```vb
Option Explicit
Public Declare Function RtlCompareMemory Lib "ntdll.dll" _
  (ByRef Source1 As Any, ByRef Source2 As Any, ByVal Length As Long) As Long

Public Function IsNAN(ByRef d As Double) As Boolean
Dim d1 As Double
    d1 = NaN()
    IsNAN = (RtlCompareMemory(d, d1, 8) = 8)
End Function

Public Function NaN() As Double
    On Error Resume Next ' ignore the error
    NaN = 0 / 0
End Function

Sub Main()
Dim d1 As Double
Dim d2 As Double
    d1 = NaN()
    d2 = d1
    Debug.Assert IsNAN(d2)
    Debug.Print CStr(d2)
End Sub
```

```txt
-1,#IND
```



## XPL0


```XPL0
0.
.1
1e3
123.456E-300
-123_456_789e+123
```



## zkl

zkl requires something on both sides of the dot for a thing to be a float

```zkl
1.0, 0.1, 3.1415, 1.e-100, 1.2e100, -1e10, -1e+10, 123.456E-300
```



