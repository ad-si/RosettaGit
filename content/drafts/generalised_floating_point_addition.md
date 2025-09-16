+++
title = "Generalised floating point addition"
description = ""
date = 2019-10-04T23:14:33Z
aliases = []
[extra]
id = 10732
[taxonomies]
categories = []
tags = []
+++

{{Template:Draft task}}
It is possible to implement floating point arithmetic in many different ways; though the standard IEEE arithmetic implementation on modern hardware usually uses a fixed-width binary implementation (using a total of 32 bits for single-precision floats — <code>float</code> in a number of languages — and 64 bits for double-precision floats — <code>double</code>) many other schemes can be used. For example, the base need not be binary (alternatives include decimal, [[wp:Binary-coded decimal|binary-coded decimal]], and even [[balanced ternary]]) and there is no requirement that a fixed number of digits be used.

Define addition for floating point numbers where the digits are stored in an arbitrary base; e.g. the digits can be stored as binary, decimal, [[wp:Binary-coded decimal|binary-coded decimal]], or even [[balanced ternary]].
You should implement the code in a generalised form (such as a [[wp:Template (programming)|Template]], [[wp:Modular programming|Module]] or [[wp:Mixin|Mixin]] etc) that permits reusing of the code for different [[wp:Base_(exponentiation)#In_numeral_systems|Bases]].

If it is not possible to implement code by providing an implementation that uses the ''syntax'' of the specific language then:
* Note the reason why this is.
* Demonstrate that the language still supports the semantics of generalised floating point by implementing the ''test case'' using built-in code or a library.

'''Test case:'''

Define [[wp:Arbitrary-precision arithmetic|arbitrary precision addition]] on floating point numbers (e.g., encoding those numbers using Binary Coded Decimal, or using an arbitrary precision integer arithmetic library). Calculate the terms for -7 to 21 in this sequence of calculations:
{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Calculate the terms for -7 to 21 in this sequence of calculations
|-
! Number !! Term calculation || Result
|-
| -7 || 12345679e63 &times; 81 + 1e63 || 1e72
|-
| -6 || 12345679012345679e54 &times; 81 + 1e54 || 1e72
|-
| -5 || 12345679012345679012345679e45 &times; 81 + 1e45 || 1e72
|-
| -4 || 12345679012345679012345679012345679e36 &times; 81 + 1e36 || 1e72
|-
|etc.|| The final calculation will be over 256 digits wide || 1e72
|}
You will either need to implement multiplication, or perform the multiplication by 81 by using repeated additions. The results will always be 1e72.

;Bonus
Make the template (module, class, mixing, etc.) able to successfully handle using other bases to perform the above test case. Demonstrate using [[balanced ternary]].


## ALGOL 68

{{works with|ALGOL 68|Revision 1 - one minor extension to language used - PRAGMA READ, similar to C's #include directive.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.3.3 algol68g-2.3.3].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
''Note:'' This code stores the digits as array of digits with the "most significant digit" on the "left" as per normal "human" form.  The net effect is that whole numbers (such as 100) are stored in the negative array positions, eg -2, -1 & 0, or [-2:0],  And the fractional part of the floating point numbers are stored from index 1, eg. 1, 2, 3 etc. or [1:].

'''See also:''' [[Generalised floating point multiplication#ALGOL 68|Generalised floating point multiplication]]

'''File: Template.Big_float.Addition.a68''' - task code
```algol68
########################################
#  Define the basic addition operators #
#  for the generalised base            #
########################################

# derived DIGIT operators #
OP + = (DIGIT arg)DIGIT: arg;
OP + = (DIGIT a,b)DIGIT: (DIGIT out := a; MOID(out +:= b); out);

# derived hybrid of DIGIT & DIGITS operators #
OP + = (DIGITS a, DIGIT b)DIGITS: a + INITDIGITS b;
OP + = (DIGIT a, DIGITS b)DIGITS: INITDIGITS a + a;
OP +:= = (REF DIGITS lhs, DIGIT arg)DIGITS: lhs := lhs + arg;

# derived DIGITS operators #
OP + = (DIGITS arg)DIGITS: arg;
OP +:= = (REF DIGITS lhs, DIGITS arg)DIGITS: lhs := lhs + arg;

####################################
#            TASK CODE             #
# Actual generic addition operator #
####################################
OP + = (DIGITS a, b)DIGITS: (
  IF SIGN a = 0 THEN b ELIF SIGN b = 0 THEN a
  ELSE
    MODE SIGNED = DIGIT;

    INT extreme highest = MSD a MIN MSD b,
        overlap highest = MSD a MAX MSD b,
        overlap lowest  = LSD a MIN LSD b,
        extreme lowest  = LSD a MAX LSD b;

    SIGNED zero = ZERO LOC SIGNED;
    INT order = digit order OF arithmetic;
    DIGITS out;

    IF overlap highest > overlap lowest THEN # Either: NO overlapping digits #

      [extreme highest:extreme lowest]SIGNED a plus b;

# First: simply insert the known digits with their correct sign #
      a plus b[MSD a:LSD a] := a[@1];
      a plus b[MSD b:LSD b] := b[@1];

# Next: Zero any totally non overlapping digit #
      FOR place FROM overlap highest+order BY order TO overlap lowest-order
      DO a plus b[place] := zero OD;

# Finally: normalise by removing leading & trailing "zero" digit #
      out := INITDIGITS a plus b

    ELSE # Or: Add ALL overlapping digits #

      [extreme highest+(carry OF arithmetic|order|0):extreme lowest]SIGNED a plus b;

#   First: Deal with the non overlapping Least Significant Digits #
      a plus b[overlap lowest-order:] := (LSD a > LSD b|a|b) [overlap lowest-order:];

#   Or: Add any overlapping digits #
      SIGNED carry := zero;
      FOR place FROM overlap lowest BY order TO overlap highest DO
        SIGNED digit a = a[place], digit b = b[place];
        REF SIGNED result = a plus b[place];
        IF carry OF arithmetic THEN # used in big float #
          result := carry;
          carry := ( result +:= digit a );
          MOID( carry +:= ( result +:= (digit b) ) )
        ELSE
          result  := digit a;
          MOID( result +:= digit b )
        FI
      OD;

#   Next: Deal with the non overlapping Most Significant digits #
      FOR place FROM overlap highest+order BY order TO extreme highest DO
        []DIGIT etc = (MSD a < MSD b|a|b);
        REF SIGNED result = a plus b[place];
        IF carry OF arithmetic THEN
          result := carry;
          carry := ( result +:= etc[place] )
        ELSE
          result := etc[place]
        FI
      OD;

#   Next: Deal with the carry #
      IF carry OF arithmetic THEN
        a plus b[extreme highest+order] := carry
      FI;

#   Finally: normalise by removing leading & trailing "zero" digits #
      out := INITDIGITS a plus b

    FI;
    out # EXIT #
  FI
);
```
'''File: Template.Big_float.Base.a68''' - task utility code
```algol68
# -*- coding: utf-8 -*- #
################################################
#  Define the basic operators and routines for #
#  manipulating DIGITS in a generalised base   #
################################################

STRUCT (
  BOOL balanced,
       carry, # aka "carry" between digits #
  INT base,
      digit width,
      digit places,
      digit order,
  USTRING repr
) arithmetic := (
  FALSE, TRUE,
  10, 1, 81, -1, # Default is BCD/Hex #
  USTRING( # Note that the "circled" digits are negative - used in balance arithmetic #
           "ⓩ","ⓨ","ⓧ","ⓦ","ⓥ","ⓤ","ⓣ","ⓢ","ⓡ","ⓠ","ⓟ","ⓞ","ⓝ","ⓜ","ⓛ","ⓚ","ⓙ","ⓘ","ⓗ","ⓖ","ⓕ","ⓔ","ⓓ","ⓒ","ⓑ","ⓐ",
           "Ⓩ","Ⓨ","Ⓧ","Ⓦ","Ⓥ","Ⓤ","Ⓣ","Ⓢ","Ⓡ","Ⓠ","Ⓟ","Ⓞ","Ⓝ","Ⓜ","Ⓛ","Ⓚ","Ⓙ","Ⓘ","Ⓗ","Ⓖ","Ⓕ","Ⓔ","Ⓓ","Ⓒ","Ⓑ","Ⓐ",
                   "⑨","⑧","⑦","⑥","⑤","④","③","②","①",    "0",    "1","2","3","4","5","6","7","8","9",
           "A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z",
           "a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"
  )[@-(26*2+9)] # can print up to base 61, or balanced base 123 #
);

MODE DIGITS = FLEX[0]DIGIT;

# DIGIT OPerators #
OP INITDIGIT = (#LONG# INT i)DIGIT: (DIGIT out; digit OF out := #SHORTEN# i; out);
#OP INITDIGIT = (INT i)DIGIT: INITDIGIT LENG i;#

OP /= = (DIGIT a,b)BOOL: digit OF a /= digit OF b;

# Define additive and multiplicative identities #
OP ZERO = (DIGITS skip)DIGITS: INITDIGITS []DIGIT(ZERO LOC DIGIT),
   IDENTITY = (DIGITS skip)DIGITS: INITDIGITS []DIGIT(IDENTITY LOC DIGIT);

OP SIGN = (DIGIT digit)INT: SIGN digit OF digit;

# Define OPerators for Least and Most Significant DIGIT #
OP MSD = (DIGITS t)INT: LWB t,
   EXP = (DIGITS t)INT: digit order OF arithmetic * LWB t, # exponent #
   LSD = (DIGITS t)INT: UPB t;

OP INITDIGITS = (DIGIT in)DIGITS: INITDIGITS []DIGIT(in)[@0];

OP INITDIGITS = ([]DIGIT digits)DIGITS: (
### normalise digits:
  A) removed leading & trailing zeros
  B) IF not balanced arithmetic
     THEN make all digits positive and set sign bit FI
###
    MODE SIGNED = DIGIT;
    DIGIT zero = ZERO LOC DIGIT;
    DIGIT one = IDENTITY LOC DIGIT;
    DIGIT base digit = INITDIGIT base OF arithmetic;
    INT base = base OF arithmetic;
    INT half base = base % 2;
    INT order = digit order OF arithmetic;

    INT msd := LWB digits + int width*order, lsd := UPB digits; # XXX #

# create an array with some "extra" significant digits incase there is a "big" input value #
    [msd:lsd]SIGNED signed; signed[LWB digits:UPB digits] := digits[@1];
    FOR place FROM LWB signed TO LWB digits+order DO
      signed[place] := zero
    OD;
    IF msd + order /= lsd THEN

#   Trim leading zeros #
      FOR place FROM msd BY -order TO lsd DO
        IF SIGN signed[place] /= 0 THEN msd := place; done msd FI
      OD;
      msd := lsd-order;
    done msd:
#   Trim trailing zeros #
      FOR place FROM lsd BY order TO msd DO
        IF SIGN signed[place] /= 0 THEN lsd := place; done lsd FI
      OD;
      lsd := msd+order;
    done lsd:
      IF msd + order /= lsd THEN  # not zero #
        IF carry OF arithmetic THEN # Normalise to the "base OF arithmetic": #
          INT sign msd := SIGN digits[msd]; # first non zero digit #
          INT lwb digit := (balanced OF arithmetic|-half base|:sign msd < 0 |1-base|0);
          INT upb digit := (balanced OF arithmetic| half base|:sign msd > 0 |base-1|0);
          SIGNED carry := zero;
          FOR place FROM lsd BY order WHILE SIGN carry /=0 OR place >= LWB digits DO
             SIGNED digit := signed[place];
             carry := digit +:= carry;
             WHILE digit OF digit < lwb digit DO
               MOID(digit +:= base digit);
               MOID(carry -:= one)
             OD;
             WHILE digit OF digit > upb digit DO
               MOID(digit -:= base digit);
               MOID(carry +:= one)
             OD;
             signed[place] := digit; # normalised #
             IF SIGN digit /= 0 THEN msd := place FI
          OD
        FI
      FI;
      signed[msd:lsd][@msd]
    FI
  );

# re anchor the array with a shift #
OP SHL = (DIGITS in, INT shl)DIGITS: in[@MSD in-shl];
OP SHR = (DIGITS in, INT shr)DIGITS: in[@MSD in+shr];
```
'''File: Template.Big_float_BCD.Base.a68 - test case code'''
```algol68
################################################
#  Define the basic operators and routines for #
#  manipulating DIGITS specific to a BCD base  #
################################################

####################################################
# BCD noramlly means Binary Coded Decimal, but...  #
# this code handles "Balanced Coded Data", meaning #
# Data can be in any numerical bases, and the data #
# can optionally be stored as Balanced about zero  #
####################################################

# define the basic axioms of the number system you are extending #
MODE DIGIT = STRUCT(#LONG# INT digit);
# Note: If the +:= and *:= operators for INT are being "overloaded",
        then it is sometimes necessary to wrap an INT in a STRUCT to
        protect the builtin definitions of the INT OPerators
#

# mixin the Big_float base definitions #
PR READ "Template.Big_float.Base.a68" PR

MODE BIGREAL  = DIGITS;

# the Yoneda ambiguity forces the peculiar coercion n the body of OP #
OP ZERO = (DIGIT skip)DIGIT: (DIGIT out; digit OF out := 0; out);
OP IDENTITY = (DIGIT skip)DIGIT: (DIGIT out; digit OF out := 1; out);

# define the basis operators #
OP ABS = (DIGIT a)INT: ABS digit OF a;
OP - = (DIGIT a)DIGIT: INITDIGIT -digit OF a;

####################################################################
# Important: Operator +:= is required by Template_Big_float_addition. #
####################################################################
# Note: +:= returns carry DIGIT #
OP +:= = (REF DIGIT lhs, DIGIT arg)DIGIT: (
# Todo: Implement balanced arithmetic #
  INT sum = digit OF lhs + digit OF arg;   # arg may be -ve #
  INT carry := sum % base OF arithmetic;
  INT digit := sum - carry * base OF arithmetic;
  IF balanced OF arithmetic THEN
     INT half base = base OF arithmetic OVER 2;
     IF   digit > half base THEN
       digit -:= base OF arithmetic;
       carry +:= 1
     ELIF digit < -half base THEN
       digit +:= base OF arithmetic;
       carry -:= 1
     FI
  FI;
  lhs := INITDIGIT digit; INITDIGIT carry
);

INT half = base OF arithmetic OVER 2;
# ASSERT NOT balanced OF arithmetic OR ODD base OF arithmetic #
##########################################################################
# Important: Operator *:= is required by Template_Big_float_multiplication. #
##########################################################################
# Note: *:= returns carry DIGIT #

OP *:= = (REF DIGIT lhs, DIGIT arg)DIGIT: (
# Todo: Implement balanced arithmetic #
  INT product = digit OF lhs * digit OF arg;   # arg may be -ve #
  INT carry = product % base OF arithmetic;
  lhs := INITDIGIT(product - carry * base OF arithmetic);
  INITDIGIT carry
);

##########################################################
# Define the basic coersion/casting rules between types. #
##########################################################
OP INITLONGREAL = (BIGREAL a)LONG REAL:
  IF SIGN a = 0 THEN 0
  ELSE
    INT lsd a = LSD a; # Todo: Optimise/reduce to match "long real width" #
    LONG REAL out := digit OF a[MSD a];
    FOR place FROM MSD a - digit order OF arithmetic BY -digit order OF arithmetic TO lsd a DO
      out := out * base OF arithmetic + digit OF a[place]
    OD;
    out * LONG REAL(base OF arithmetic) ** -LSD a
  FI;

OP INITREAL = (BIGREAL r)REAL:
  SHORTEN INITLONGREAL r;

OP MSD = (LONG INT i)INT: (
  LONG INT remainder := i; INT count := 0;
  WHILE remainder /= 0 DO
    remainder %:= base OF arithmetic;
    MOID(count +:= 1)
  OD;
  count
);

OP INITBIGREAL = (LONG INT in int)BIGREAL: (

  INT max = MSD in int;
  [1-max:0]DIGIT out;

  LONG INT int := ABS in int;
  INT sign = SIGN in int;

  FOR place FROM UPB out BY digit order OF arithmetic TO LWB out WHILE int /= 0 DO
    INT digit := SHORTEN (int MOD base OF arithmetic);
    int := (int-digit) OVER base OF arithmetic;
    (digit OF out)[place] := sign * digit
  OD;
done:
  INITDIGITS out # normalise #
);

OP INITBIGREAL = (INT in int)BIGREAL:
  INITBIGREAL LENG in int;

OP INITBIGREAL = (LONG REAL in real)BIGREAL: (

  INT sign = SIGN in real;
  LONG REAL real := ABS in real;
  LONG REAL frac := real - ENTIER real;
  BIGREAL whole = INITBIGREAL ENTIER real; # normalised #

  INT base = base OF arithmetic,
      order = digit order OF arithmetic,
      lsd = digit places OF arithmetic; # Todo: can be optimised/reduced #

  [MSD whole:lsd]DIGIT out; out[MSD whole:LSD whole] := whole[@1];
  FOR place FROM LSD whole - order TO 0 DO out[place] := INITDIGIT 0 OD; # pad #

  FOR place FROM -order BY -order TO lsd DO
    frac *:= base;
    #LONG# INT digit := SHORTEN ENTIER frac;
    frac -:= digit;
    (digit OF out)[place] := digit;
    IF frac = 0 THEN done FI
  OD;
done:
  IF sign > 1 THEN INITDIGITS out ELSE - INITDIGITS out FI
);

OP INITBIGREAL = (REAL in real)BIGREAL:
  INITBIGREAL LENG in real;

#FORMAT digit fmt = $n(digit width OF arithmetic+ABS balanced OF arithmetic)(d)$;#
FORMAT big real fmt = $g((digit width OF arithmetic+ABS balanced OF arithmetic))","$;

OP REPR = (DIGIT digit)STRING:
  IF LWB repr OF arithmetic <= digit OF digit AND digit OF digit <= UPB repr OF arithmetic THEN
    (repr OF arithmetic)[digit OF digit]
  ELIF balanced OF arithmetic THEN
    whole(digit OF digit,digit width OF arithmetic+1)
  ELSE
    whole(digit OF digit,-digit width OF arithmetic)
  FI;

OP REPR = (BIGREAL real)STRING:(
  CHAR repr 0 = "0";
  STRING out;
  FOR place FROM MSD real BY -digit order OF arithmetic TO LSD real DO
    IF place = 1 AND place =  MSD real THEN out +:= "." FI;
    out +:= REPR(real[place]);
    IF place = 0 AND place /=  LSD real THEN out +:= "." FI
  OD;
  IF out = "" THEN out := repr 0 FI;
  IF SIGN real < 0 AND NOT balanced OF arithmetic THEN "-" +=: out FI;
  IF MSD real > 1 AND LSD real > 1 OR
     MSD real < 0 AND LSD real < 0 THEN
  # No decimal point yet, so maybe we need to add an exponent #
    out+IF digit order OF arithmetic*LSD real = 1 THEN repr 0
        ELSE "e"+REPR INITBIGREAL(digit order OF arithmetic*LSD real) FI
        # ELSE "E"+whole(digit order OF arithmetic*LSD real,0) FI #
  ELSE
    out
  FI
);
```
'''File: Template.Big_float.Subtraction.a68''' - bonus subtraction definitions
```algol68
OP - = (DIGITS arg)DIGITS: (
  DIGITS out := arg;
  FOR digit FROM LSD arg BY digit order OF arithmetic TO MSD arg DO
    out[digit]:= -out[digit]
  OD;
  out
);

OP SIGN = (DIGITS arg)INT:
  IF LSD arg - MSD arg  = digit order OF arithmetic THEN 0 # empty array #
  ELSE # balanced artihmetic # SIGN arg[MSD arg] FI;

OP ABS = (DIGITS arg)DIGITS:
  IF SIGN arg < 0 THEN -arg ELSE arg FI;

# derived DIGIT operators #
OP - = (DIGIT a, b)DIGIT: a + -b;
OP -:= = (REF DIGIT a, DIGIT b)DIGIT: a := a + -b;

# derived DIGITS operators #
OP - = (DIGITS a, b)DIGITS: a + -b;
OP -:= = (REF DIGITS a, DIGITS b)DIGITS: a := a + -b;

# derived hybrid DIGIT and DIGITS operators #
OP - = (DIGITS a, DIGIT b)DIGITS: a - INITDIGITS b;
OP - = (DIGIT a, DIGITS b)DIGITS: INITDIGITS a - a;
OP -:= = (REF DIGITS lhs, DIGIT arg)DIGITS: lhs := lhs - arg;
```
'''File: test.Big_float_BCD.Addition.a68''' - test case code main program
```algol68
#!/usr/local/bin/a68g --script #

##################################################################
#                        TEST CASE                               #
# A program to test abritary length BCD floating point addition. #
##################################################################

PR READ "prelude/general.a68" PR # [[rc:Template:ALGOL 68/prelude]] #

##################################################################
# READ Template for doing the actual arbitary precsion addition. #
##################################################################
PR READ "Template.Big_float.Addition.a68" PR

# include the basic axioms of the digits being used #
PR READ "Template.Big_float_BCD.Base.a68" PR
PR READ "Template.Big_float.Subtraction.a68" PR

test: (
  BIGREAL pattern = INITBIGREAL 012345679,
  INT pattern width = 9;

  BIGREAL
    sum := INITBIGREAL 0,
    shifted pattern := pattern,
    shifted tiny := INITBIGREAL 1; # typically 0.000.....00001 etc #

  FOR term FROM -8 TO 20 DO

  # First make shifted pattern smaller by shifting right by the pattern width #
    shifted pattern := (shifted pattern)[@term*pattern width+2];
    shifted tiny := (shifted tiny)[@(term+1)*pattern width];

    MOID(sum +:= shifted pattern);

  # Manually multiply by 81 by repeated addition #
    BIGREAL prod := sum + sum + sum;
    MOID(prod +:= prod + prod);
    MOID(prod +:= prod + prod);
    MOID(prod +:= prod + prod);

    BIGREAL total = prod + shifted tiny;

    IF term < -4 THEN
      print(( REPR sum," x 81 gives: ", REPR prod, ", Plus ",REPR shifted tiny," gives: "))
    ELSE
      print((LSD prod - MSD prod + 1," digit test result: "))
    FI;
    printf(($g$, REPR total, $" => "b("Passed","Failed")"!"$, LSD total = MSD total, $l$))
  OD
)
```
'''Output:'''

```txt
12345679e63 x 81 gives: 999999999e63, Plus 1e63 gives: 1e72 => Passed!
12345679012345679e54 x 81 gives: 999999999999999999e54, Plus 1e54 gives: 1e72 => Passed!
12345679012345679012345679e45 x 81 gives: 999999999999999999999999999e45, Plus 1e45 gives: 1e72 => Passed!
12345679012345679012345679012345679e36 x 81 gives: 999999999999999999999999999999999999e36, Plus 1e36 gives: 1e72 => Passed!
        +45 digit test result: 1e72 => Passed!
        +54 digit test result: 1e72 => Passed!
        +63 digit test result: 1e72 => Passed!
        +72 digit test result: 1e72 => Passed!
        +81 digit test result: 1e72 => Passed!
        +90 digit test result: 1e72 => Passed!
        +99 digit test result: 1e72 => Passed!
       +108 digit test result: 1e72 => Passed!
       +117 digit test result: 1e72 => Passed!
       +126 digit test result: 1e72 => Passed!
       +135 digit test result: 1e72 => Passed!
       +144 digit test result: 1e72 => Passed!
       +153 digit test result: 1e72 => Passed!
       +162 digit test result: 1e72 => Passed!
       +171 digit test result: 1e72 => Passed!
       +180 digit test result: 1e72 => Passed!
       +189 digit test result: 1e72 => Passed!
       +198 digit test result: 1e72 => Passed!
       +207 digit test result: 1e72 => Passed!
       +216 digit test result: 1e72 => Passed!
       +225 digit test result: 1e72 => Passed!
       +234 digit test result: 1e72 => Passed!
       +243 digit test result: 1e72 => Passed!
       +252 digit test result: 1e72 => Passed!
       +261 digit test result: 1e72 => Passed!

```

[[Category:Arbitrary precision]]


## Go

Although the big.Float type already has a 'Mul' method, we re-implement it by repeated application of the 'Add' method.

```go
package main

import (
    "fmt"
    "math/big"
)

func repeatedAdd(bf *big.Float, times int) *big.Float {
    if times < 2 {
        return bf
    }
    var sum big.Float
    for i := 0; i < times; i++ {
        sum.Add(&sum, bf)
    }
    return &sum
}

func main() {
    s := "12345679"
    t := "123456790"
    e := 63
    var bf, extra big.Float
    for n := -7; n <= 21; n++ {
        bf.SetString(fmt.Sprintf("%se%d", s, e))
        extra.SetString(fmt.Sprintf("1e%d", e))
        bf = *repeatedAdd(&bf, 81)
        bf.Add(&bf, &extra)
        fmt.Printf("%2d : %s\n", n, bf.String())
        s = t + s
        e -= 9
    }
}
```


{{out}}

```txt

-7 : 1e+72
-6 : 1e+72
-5 : 1e+72
-4 : 1e+72
-3 : 1e+72
-2 : 1e+72
-1 : 1e+72
 0 : 1e+72
 1 : 1e+72
 2 : 1e+72
 3 : 1e+72
 4 : 1e+72
 5 : 1e+72
 6 : 1e+72
 7 : 1e+72
 8 : 1e+72
 9 : 1e+72
10 : 1e+72
11 : 1e+72
12 : 1e+72
13 : 1e+72
14 : 1e+72
15 : 1e+72
16 : 1e+72
17 : 1e+72
18 : 1e+72
19 : 1e+72
20 : 1e+72
21 : 1e+72

```



## J


I am not currently able to implement the task exactly because I do not quite understand what is being asked for (nor why it would be useful).

That said, the task does specify some calculations to be performed.

Given


```j
e=: 2 : 0
  u * 10x ^ v
)
```


In other words, given a parse time word (<code>e</code>) which combines its two arguments as numbers, multiplying the number on its left by the exact exponent of 10 given on the right, I can do:

<lang>   1 e 63 + 12345679 e 63 * 81
1000000000000000000000000000000000000000000000000000000000000000000000000
   1 e 54 + 12345679012345679 e 54 * 81
1000000000000000000000000000000000000000000000000000000000000000000000000
   1 e 45 + 12345679012345679012345679x e 45 * 81
1000000000000000000000000000000000000000000000000000000000000000000000000
   1 e 36 + 12345679012345679012345679012345679x e 36 * 81
1000000000000000000000000000000000000000000000000000000000000000000000000
```


So, ok, let's turn this into a sequence:


```j
factor=: [: +/ [: 12345679 e ] _9 * 1 + i.&.(+&8)
adjust=: 1 e (_9&*)
```


Here we show some examples of what these words mean:


```j
   factor _4  NB. this is the number we multiply by 81
12345679012345679012345679012345679000000000000000000000000000000000000
   factor _3
12345679012345679012345679012345679012345679000000000000000000000000000
   factor 2 NB. here we see that we are using rational numbers
12345679012345679012345679012345679012345679012345679012345679012345679012345679012345679r1000000000000000000
   90j18 ":  factor 2  NB. formatted as decimal in 90 characters with 18 characters after the decimal point
12345679012345679012345679012345679012345679012345679012345679012345679.012345679012345679
   adjust _4  NB. this is the number we add to the result of multiplying our factor by 81
1000000000000000000000000000000000000
   adjust _3
1000000000000000000000000000
```


Given these words:


```j

   _7+i.29 NB. these are the sequence elements we are going to generate
_7 _6 _5 _4 _3 _2 _1 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21
   #(adju + 81 * factor)&> _7+i.29 NB. we generate a sequence of 29 numbers
29
   ~.(adju + 81 * factor)&> _7+i.29  NB. here we see that they are all the same number
1000000000000000000000000000000000000000000000000000000000000000000000000
```


Note that <code>~. list</code> returns the unique elements from that list.



## Julia

Julia implements arbitary precision intergers and floating point numbers in its base function.

```julia

@assert(big"12345679e63" * BigFloat(81) + big"1e63" == big"1.0e+72")
@assert(big"12345679012345679e54" * BigFloat(81) + big"1e54" == big"1.0e+72")
@assert(big"12345679012345679012345679e45" * BigFloat(81) + big"1e45" == big"1.0e+72")
@assert(big"12345679012345679012345679012345679e36" * BigFloat(81) + big"1e36" == big"1.0e+72")

```

All assertions pass.



## Kotlin

The JDK has an arbitrary precision BigDecimal class which uses a scaled decimal representation internally. This enables decimal numbers of any size to be represented exactly.

Although the BigDecimal type supports multiplication, I've used repeated addition here to be more in tune with the spirit of the task.

```scala
// version 1.2.10

import java.math.BigDecimal

fun BigDecimal.repeatedAdd(times: Int): BigDecimal {
    var sum = BigDecimal.ZERO
    for (i in 0 until times) sum += this
    return sum
}

fun main(args: Array<String>) {
    var s = "12345679"
    val t = "123456790"
    var e = 63
    for (n in -7..21) {
        val bd = BigDecimal("${s}e$e")
        val oneE = BigDecimal("1e$e")
        val temp = bd.repeatedAdd(9)
        val result = temp.repeatedAdd(9) + oneE
        println("%2d : %e".format(n, result))
        s = t + s
        e -= 9
    }
}
```


{{out}}

```txt

-7 : 1.000000e+72
-6 : 1.000000e+72
-5 : 1.000000e+72
-4 : 1.000000e+72
-3 : 1.000000e+72
-2 : 1.000000e+72
-1 : 1.000000e+72
 0 : 1.000000e+72
 1 : 1.000000e+72
 2 : 1.000000e+72
 3 : 1.000000e+72
 4 : 1.000000e+72
 5 : 1.000000e+72
 6 : 1.000000e+72
 7 : 1.000000e+72
 8 : 1.000000e+72
 9 : 1.000000e+72
10 : 1.000000e+72
11 : 1.000000e+72
12 : 1.000000e+72
13 : 1.000000e+72
14 : 1.000000e+72
15 : 1.000000e+72
16 : 1.000000e+72
17 : 1.000000e+72
18 : 1.000000e+72
19 : 1.000000e+72
20 : 1.000000e+72
21 : 1.000000e+72

```



## Perl

Calculations done in decimal, prints 'Y' for each successful test.

```perl
use strict;
use warnings;
use Math::Decimal qw(dec_add dec_mul_pow10);

my $e = 63;
for my $n (-7..21) {
    my $num = '12345679' . '012345679' x ($n+7);
    my $sum = dec_mul_pow10(1, $e);
    $sum = dec_add($sum, dec_mul_pow10($num,$e)) for 1..81;
    printf "$n:%s ", 10**72 == $sum ? 'Y' : 'N';
    $e -= 9;
}
```

{{out}}

```txt
-7:Y -6:Y -5:Y -4:Y -3:Y -2:Y -1:Y 0:Y 1:Y 2:Y 3:Y 4:Y 5:Y 6:Y 7:Y 8:Y 9:Y 10:Y 11:Y 12:Y 13:Y 14:Y 15:Y 16:Y 17:Y 18:Y 19:Y 20:Y 21:Y

```



## Perl 6

As long as all values are kept as rationals (type <code>Rat</code>) calculations are done full precision.

```perl6
my $e = 63;
for -7..21 -> $n {
    my $num = '12345679' ~ '012345679' x ($n+7);
    my $sum = $_ + ($num * $_) * 81 given $e > -20 ?? 10**$e !! Rat.new(1,10**abs $e);
    printf "$n:%s ", 10**72 == $sum ?? 'Y' !! 'N';
    $e -= 9;
}
```

{{out}}

```txt
-7:Y -6:Y -5:Y -4:Y -3:Y -2:Y -1:Y 0:Y 1:Y 2:Y 3:Y 4:Y 5:Y 6:Y 7:Y 8:Y 9:Y 10:Y 11:Y 12:Y 13:Y 14:Y 15:Y 16:Y 17:Y 18:Y 19:Y 20:Y 21:Y

```



## Phix


### bigatom

Note this is decimal-only, and that mpfr.e does not really cope because it cannot hold decimal fractions exactly (though it could probably be fudged with a smidgen of rounding)

```Phix
include bigatom.e
{} = ba_scale(200)

string s = "12345679",
       t = "123456790"
integer e = 63
for n = -7 to 21 do
    bigatom bd = ba_new(sprintf("%se%d",{s,e})),
            e1 = ba_new(sprintf("1e%d",e)),
            res = ba_add(ba_mul(bd,81),e1)
--          res = ba_mul(bd,81)
    printf(1,"%2d : %s\n",{n,ba_sprintf("%.eB",res)})
--  printf(1,"%2d : %s\n",{n,ba_sprintf("%B",res)})
    s = t & s
    e -= 9
end for
```

{{out|Output (trimmed)}}
(Use the %B format to better see the difference between adding and not adding e1.)

```txt

-7 : 1e72
...
21 : 1e72

```



###  any base

Uses b_add() and b_mul() from [[Generalised_floating_point_multiplication#Phix]]

```Phix
-- demo\rosetta\Generic_addition.exw
function normalise_decimal(string a)
--
-- eg "12.34e-3" ==> "0.01234"
--     and "1e2" ==> "100"
--
    integer d = find('.',a),
            e = find('e',a)
    if e then
        integer {{z}} = scanf(a[e+1..$],"%d")
        a = a[1..e-1]
        if d then
            -- eg 12.34e-3 == 1234e-5
            a[d..d] = ""
            z -= length(a)-d+1
        end if
        if z>=0 then
            -- eg 1e2 ==> 100
            a &= repeat('0',z)
        elsif z<-length(a) then
            -- eg 1234e-5 ==> 0.01234
            a = "0."&repeat('0',-z-length(a))&a
        else
            -- eg 1234e-3 ==> 1.234
            a[z..z-1] = "."
        end if
    end if
    return a
end function

function e_notation(string a)
-- eg "1000000" ==> "1e6"
    if find('.',a) then ?9/0 end if -- (to do)
    integer l = length(a)
    a = trim_tail(a,".0")
    l -= length(a)
    if l!=0 then
        a &=sprintf("e%d",l)
    end if
    return a
end function

string s = "12345679",
       t = "123456790"
integer e = 63
for n = -7 to 21 do
    string bd = normalise_decimal(sprintf("%se%d",{s,e})),
           e1 = normalise_decimal(sprintf("1e%d",e)),
           res = b_add(b_mul(bd,"81",decimal),e1,decimal)
    printf(1,"%2d res : %s\n",{n,e_notation(res)})
    s = t & s
    e -= 9
end for
```

same ouput


## Racket


Racket has native arbitrary precision numbers (if in doubt, prefixed with <code>#e...</code>)

The printed result is the exact difference of 1e72 and the result. So those <code>0</code>s you see are exactly nothing.


```racket
#lang racket
(define (f n (printf printf))
  (define exponent (* (- n) 9))
  (define mantissa
    (for/fold ((m 0))
              ((i (in-range -7 (add1 n))))
      (+ (* m 1000000000) 12345679)))

  (let ((rv (+ (expt 10 exponent) (* mantissa (expt 10 exponent) 81))))
    (printf "~ae~a * 81 + 1e~a - 1e72 = ~a~%"
            (~.a mantissa
                 #:max-width 20
                 #:limit-marker (format "..[~a].." (add1 (order-of-magnitude mantissa))))
            exponent
            exponent
            (- #e1e72 rv))
    rv))

(module+ test
  (require rackunit)
  (check-equal? (f -7 void) (expt 10 72))
  (check-equal? (f -6 void) (expt 10 72)))

(module+ main
  (displayln "number in brackets is TOTAL number of digits")
  (for ((i (in-range -7 (add1 21)))) (f i)))
```


{{out}}


```txt
number in brackets is TOTAL number of digits
12345679e63 * 81 + 1e63 - 1e72 = 0
12345679012345679e54 * 81 + 1e54 - 1e72 = 0
123456790123..[26]..e45 * 81 + 1e45 - 1e72 = 0
123456790123..[35]..e36 * 81 + 1e36 - 1e72 = 0
123456790123..[44]..e27 * 81 + 1e27 - 1e72 = 0
123456790123..[53]..e18 * 81 + 1e18 - 1e72 = 0
123456790123..[62]..e9 * 81 + 1e9 - 1e72 = 0
123456790123..[71]..e0 * 81 + 1e0 - 1e72 = 0
123456790123..[80]..e-9 * 81 + 1e-9 - 1e72 = 0
123456790123..[89]..e-18 * 81 + 1e-18 - 1e72 = 0
123456790123..[98]..e-27 * 81 + 1e-27 - 1e72 = 0
12345679012..[107]..e-36 * 81 + 1e-36 - 1e72 = 0
12345679012..[116]..e-45 * 81 + 1e-45 - 1e72 = 0
12345679012..[125]..e-54 * 81 + 1e-54 - 1e72 = 0
12345679012..[134]..e-63 * 81 + 1e-63 - 1e72 = 0
12345679012..[143]..e-72 * 81 + 1e-72 - 1e72 = 0
12345679012..[152]..e-81 * 81 + 1e-81 - 1e72 = 0
12345679012..[161]..e-90 * 81 + 1e-90 - 1e72 = 0
12345679012..[170]..e-99 * 81 + 1e-99 - 1e72 = 0
12345679012..[179]..e-108 * 81 + 1e-108 - 1e72 = 0
12345679012..[188]..e-117 * 81 + 1e-117 - 1e72 = 0
12345679012..[197]..e-126 * 81 + 1e-126 - 1e72 = 0
12345679012..[206]..e-135 * 81 + 1e-135 - 1e72 = 0
12345679012..[215]..e-144 * 81 + 1e-144 - 1e72 = 0
12345679012..[224]..e-153 * 81 + 1e-153 - 1e72 = 0
12345679012..[233]..e-162 * 81 + 1e-162 - 1e72 = 0
12345679012..[242]..e-171 * 81 + 1e-171 - 1e72 = 0
12345679012..[251]..e-180 * 81 + 1e-180 - 1e72 = 0
12345679012..[260]..e-189 * 81 + 1e-189 - 1e72 = 0

```



## REXX


### base ten only


```rexx
/*REXX pgm to perform generalized floating point addition using BCD nums*/
/*┌────────────────────────────────────────────────────────────────────┐
┌─┘  This REXX program uses an  uncompressed  (or zoned)  BCD  which   └─┐
│ consumes one byte for each represented digit.  A leading sign (+ or -) │
│ is optional.  An exponent is also allowed which is preceded by a  ^.   │
│ The value of the exponent may have a leading sign  (+ or -).           │
│ Each numeral (digit)  is stored as its own character (glyph), as well  │
│ as the signs and exponent indicator.  There is essentially no limit on │
│ the number of digits in the mantissa or the exponent, but the value of │
│ the exponent is limited to around 16 million.  The mantissa may also   │
│ have a decimal point (.).                                              │
│                                                                        │
│ Method:  a table of twenty-eight BCD numbers is built, and a test case │
│ of adding that BCD number 81 times  (essentially multiplying by 81),   │
│ and then a number is added to that sum,  and the resultant sum should  │
│ result in the final sum of  1e72   (for all cases).                    │
│                                                                        │
└─┐  The number of digits for the precision is automatically adjusted. ┌─┘
  └────────────────────────────────────────────────────────────────────┘*/
maxW=linesize()-1                      /*max width allowed for displays.*/
                                       /*Not all REXXes have  LINESIZE. */
_123=012345679;   reps=0;   mult=63    /*used to construct test cases.  */
say ' # addend               uncompressed (zoned) BCD number'  /*header.*/
say left('── ────── ─',maxW,'─')                               /*hdr sep*/

   do j=-7  to 21                      /*traipse through the test cases.*/
   reps=reps+1                         /*increase number of repetitions.*/
   BCD.j=strip(copies(_123,reps)'^'mult,'L',0)    /*construct zoned BCD.*/
   if j//3==0 then BCD.J='+'BCD.j      /*add a leading + sign every 3rd#*/
   parse var BCD.j '^' pow             /*get the exponent part of the #.*/
   addend.j='1e'pow                    /*build the addend the hard way. */
   _=right(j,2) right(addend.j,6)      /*construct the prefix for a line*/
   aLine=_ BCD.j                       /*construct a line of output.    */
   if length(aLine)<maxW then say aLine     /*Fit on a line?  Display it*/
                         else say _ ' ['length(BCD.j) 'digits]'  /*other*/
   mult=mult-9                         /*decrease multiplier's exponent.*/
   maxDigs=length(BCD.j)+abs(pow)+5    /*compute max precision needed.  */
   if maxDigs>digits() then numeric digits maxDigs  /*inflate if needed.*/
   end    /*j*/

say copies('═',maxW)                   /*display a fence for separation.*/
times=81                               /*the number of times to add it. */

   do k=-7  to 21                      /*traipse through the test cases.*/
   parse var BCD.k mantissa '^' exponent  /*decompose the zoned BCD num.*/
   x=mantissa'e'exponent               /*reconstitute the original num. */
   sum=0                               /*prepare for the 81 additions.  */
               do times
               sum=sum+x               /*multiplying the hard way, yup! */
               end

   sum=(sum+addend.k)/1                /*a way to elide trailing zeroes.*/
   _=format(sum,,,,0)                  /*force sum ──►exponentional fmt.*/
   say right(k,3) 'sum='translate(_,"e",'E')   /*lets lowercase the  E. */
   end   /*k*/

exit                                   /*stick a fork in it, we're done.*/
```

This REXX program makes use of   '''LINESIZE'''   REXX program (or BIF) which is used to determine the screen width (or linesize) of the terminal (console).

The   '''LINESIZE.REX'''   REXX program is included here ──► [[LINESIZE.REX]].


'''output'''

```txt

 # addend               uncompressed (zoned) BCD number
── ────── ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
-7   1e63 12345679^63
-6   1e54 +12345679012345679^54
-5   1e45 12345679012345679012345679^45
-4   1e36 12345679012345679012345679012345679^36
-3   1e27 +12345679012345679012345679012345679012345679^27
-2   1e18 12345679012345679012345679012345679012345679012345679^18
-1    1e9 12345679012345679012345679012345679012345679012345679012345679^9
 0    1e0 +12345679012345679012345679012345679012345679012345679012345679012345679^0
 1   1e-9 12345679012345679012345679012345679012345679012345679012345679012345679012345679^-9
 2  1e-18 12345679012345679012345679012345679012345679012345679012345679012345679012345679012345679^-18
 3  1e-27 +12345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679^-27
 4  1e-36 12345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679^-36
 5  1e-45 12345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679^-45
 6  1e-54 +12345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679^-54
 7  1e-63 12345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679^-63
 8  1e-72 12345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679^-72
 9  1e-81 +12345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679^-81
10  1e-90 12345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679^-90
11  1e-99 12345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679^-99
12 1e-108 +12345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679012345679^-108
13 1e-117  [193 digits]
14 1e-126  [202 digits]
15 1e-135  [212 digits]
16 1e-144  [220 digits]
17 1e-153  [229 digits]
18 1e-162  [239 digits]
19 1e-171  [247 digits]
20 1e-180  [256 digits]
21 1e-189  [266 digits]
═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
 -7 sum=1e+72
 -6 sum=1e+72
 -5 sum=1e+72
 -4 sum=1e+72
 -3 sum=1e+72
 -2 sum=1e+72
 -1 sum=1e+72
  0 sum=1e+72
  1 sum=1e+72
  2 sum=1e+72
  3 sum=1e+72
  4 sum=1e+72
  5 sum=1e+72
  6 sum=1e+72
  7 sum=1e+72
  8 sum=1e+72
  9 sum=1e+72
 10 sum=1e+72
 11 sum=1e+72
 12 sum=1e+72
 13 sum=1e+72
 14 sum=1e+72
 15 sum=1e+72
 16 sum=1e+72
 17 sum=1e+72
 18 sum=1e+72
 19 sum=1e+72
 20 sum=1e+72
 21 sum=1e+72

```



### any base


```rexx
/*REXX pgm to perform generalized floating point addition using BCD nums*/

/*┌────────────────────────────────────────────────────────────────────┐
┌─┘  This REXX program uses an  uncompressed  (or zoned)  BCD  which   └─┐
│ consumes one byte for each represented digit.  A leading sign (+ or -) │
│ is optional.  An exponent is also allowed which is preceded by a  ^.   │
│ The value of the exponent may have a leading sign  (+ or -).           │
│ Each numeral (digit)  is stored as its own character (glyph), as well  │
│ as the signs and exponent indicator.  There is essentially no limit on │
│ the number of digits in the mantissa or the exponent, but the value of │
│ the exponent is limited to around 16 million.  The mantissa may also   │
│ have a decimal point (.).                                              │
│                                                                        │
│ Method:  a table of twenty-eight BCD numbers is built, and a test case │
│ of adding that BCD number 81 times  (essentially multiplying by 81),   │
│ and then a number is added to that sum,  and the resultant sum should  │
│ result in the final sum of  1e72   (for all cases).                    │
│                                                                        │
│ The (input) numbers may be in any base;  the REXX variable   BASE      │
│ (below) is the value of the base (radix) that the numbers are expressed│
│ in.                                                                    │
└─┐  The number of digits for the precision is automatically adjusted. ┌─┘
  └────────────────────────────────────────────────────────────────────┘*/

maxW=linesize()-1                      /*max width allowed for displays.*/
                                       /*Not all REXXes have  LINESIZE. */
base=10                                /*radix that the numbers are in. */
_123=012345679;   reps=0;   mult=63    /*used to construct test cases.  */
say ' # addend               uncompressed (zoned) BCD number'  /*header.*/
say left('── ────── ─',maxW,'─')                               /*hdr sep*/

   do j=-7 to 21                       /*traipse through the test cases.*/
   reps=reps+1                         /*increase number of repetitions.*/
   BCD.j=strip(copies(_123,reps)'^'mult,'L',0)    /*construct zoned BCD.*/
   if j//3==0 then BCD.J='+'BCD.j      /*add a leading + sign every 3rd#*/
   parse var BCD.j '^' pow             /*get the exponent part of the #.*/
   addend.j='1e'pow                    /*build the addend the hard way. */
   _=right(j,2) right(addend.j,6)      /*construct the prefix for a line*/
   aLine=_ BCD.j                       /*construct a line of output.    */
   if length(aLine)<maxW then say aLine     /*Fit on a line?  Display it*/
                         else say _ ' ['length(BCD.j) 'digits]'  /*other*/
   mult=mult-9                         /*decrease multiplier's exponent.*/
   maxDigs=length(BCD.j)+abs(pow)+5    /*compute max precision needed.  */
   if maxDigs>digits() then numeric digits maxDigs  /*inflate if needed.*/
   end    /*j*/

say copies('═',maxW)                   /*display a fence for separation.*/
times=81                               /*the number of times to add it. */

   do k=-7 to 21                       /*traipse through the test cases.*/
   parse var BCD.k mantissa '^' pow    /*decompose the zoned BCD num.*/
   exp10=base(pow,,base)               /*convert the power to base 10.  */
   x=base(mantissa,,base) * 10**base(pow,,base)   /*express without a ^ */
   sum=0                               /*prepare for the 81 additions.  */
               do times
               sum=sum+x               /*multiplying the hard way, yup! */
               end

   sum=(sum+addend.k)/1                /*a way to elide trailing zeroes.*/
   _=format(sum,,,,0)                  /*force sum ──►exponentional fmt.*/
   _baseX=base(_/1,base)               /*this expresses  _ in base BASE.*/
   say right(k,3) 'sum='translate(_,"e",'E')   /*lets lowercase the  E. */
   end   /*k*/                         /*output is in base ten.         */

exit                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────BASE subroutine─────────────────────*/
base: procedure;  parse arg x 1 s 2 1 ox,tt,ii,left_,right_;   f='BASE'
@#=0123456789; @abc='abcdefghijklmnopqrstuvwxyz';  @abcu=@abc; upper @abcu
$=$basex()                                  /*char string of max base.  */
m=length($)-1                               /*"M" is the maximum base.  */
c=left_\=='' | right_\==''
if tt=='' then tt=10                        /*assume base 10 if omitted.*/
if ii=='' then ii=10                        /*assume base 10 if omitted.*/
i=abs(ii)
t=abs(tt)
if t==999 | t=="*" then t=m
if t>m & \c then call er81 t,2 m f'-to'
if i>m      then call er81 i,2 m f'-from'

if \c then do                               /*build char str for base ? */
           !=substr($,1+10*(tt<0),t)        /*character string for base.*/
           if tt<0 then !=0||!              /*prefix a zero if neg base.*/
           end

if x=='' then if c then return left_||t||right_
                   else return left(!,t)

@=substr($,1+10*(ii<0),i)                   /*@ =legal chars for base X.*/
oS=                                         /*original sign placeholder.*/
if s='-' | s="+" then do                    /*process the sign (if any).*/
                      x=substr(x,2)         /*strip the sign character. */
                      oS=s                  /*save the original sign.   */
                      end

if (ii>10 & ii<37) | (ii<0 & ii>-27) then upper x      /*uppercase it ? */

                                            /*if base 10, must be a num.*/

if pos('-',x)\==0 |,                        /*too many minus signs ?    */
   pos('+',x)\==0 |,                        /*too many  plus signs ?    */
   x=='.'         |,                        /*is single decimal point ? */
   x==''             then call er53 ox      /*or a single + or - sign ? */

parse var x w '.' g                         /*sep whole from fraction.  */
if pos('.',g)\==0 then call er53 ox         /*too many decimals points? */
items.1=0                                   /*# of whole part "digits". */
items.2=0                                   /*# of fractional "digits". */

if c then do                                /*any "digit" specifiers ?  */
              do forever while w\==''       /*process "whole" part.     */
              parse var w w.1 (left_) w.2 (right_) w
                do j=1 to 2;  if w.j\=='' then call putit w.j,1
                end
              end

              do forever while g\==''       /*process fractional part.  */
              parse var g g.1 (left_) g.2 (right_) g
                do j=1 to 2;  if g.j\=='' then call putit g.j,2
                end
              end

          _=0;  p=0                         /*convert the whole # part. */

            do j=items.1 to 1 by -1;  _=_+item.1.j*(i**p)
            p=p+1                           /*increase power of the base*/
            end

          w=_;  _=0;  p=0                   /*convert fractional part.  */

            do j=1 to items.2;        _=_+item.2.j/i**p
            p=p+1                           /*increase power of the base*/
            end

          g=strip(strip(_,'L',0),,".")      /*strip leading dec point.  */
          if g=0 then g=                    /*no signifcant fract. part.*/
          end

__=w||g                                     /*verify re-composed number.*/
_=verify(__,@'.')                           /*# have any unusual digits?*/
if _\==0 then call er48,ox,substr(__,_,1) '[for' f i"]"      /*oops-sey.*/

if i\==10 then do                           /*convert # base I──►base 10*/
                                            /*...but only if not base 10*/
               _=0;  p=0                    /*convert the whole # part. */

                                do j=length(w) to 1 by -1 while w\==''
                                _=_+((pos(substr(w,j,1),@)-1)*i**p)
                                p=p+1       /*increase power of the base*/
                                end
               w=_;  _=0;  p=1              /*convert fractional part.  */

                  do j=1 for length(g);_=_+((pos(substr(g,j,1),@)-1)/i**p)
                  p=p+1                     /*increase power of the base*/
                  end
               g=_
               end
          else if g\=='' then g="."g        /*reinsert period if needed.*/

if t\==10 then do                           /*convert base10 # to base T*/
               if w\=='' then do            /*convert whole number part.*/
                                    do j=1;   _=t**j;   if _>w then leave
                                    end
                              n=
                                  do k=j-1 to 1 by -1;   _=t**k;   d=w%_
                                  if c then n=n left_||d||right_
                                       else n=n||substr(!,1+d,1)
                                  w=w//_
                                  end
                              if c then w=n left_||w||right_
                                   else w=n||substr(!,1+w,1)
                              end

               if g\=='' then do;  n=       /*convert fractional part.  */
                                       do digits()+1;   if g==0 then leave
                                       p=g*t;    g=p//1;  d=trunc(p)
                                       if c then n=n left_||d||right_
                                            else n=n||substr(!,d+1,1)
                                       end
                              if n==0 then n=
                              if n\=='' then n='.'n   /*only a fraction?*/
                              g=n
                              end
               end

return oS||p(strip(space(w),'L',0)strip(strip(g,,0),"T",'.') 0)

/*═════════════════════════════general 1-line subs══════════════════════*/
$basex: return @#||@abcu||@abc||space(translate(,
        xrange('1'x,"fe"x),,@#'.+-'@abc||@abcu"0708090a0b0c0d"x),0)
er:     say '***error!***'; say; say arg(1); say; exit 13
er48:   call er arg(1) 'contains invalid characters:' arg(2)
er53:   call er arg(1) 'not numeric'
er81:   call er arg(1) 'must be in the range:' arg(2)
isint:  return datatype(arg(1),'W')
isnum:  return datatype(arg(1),'N')
num:    procedure;  parse arg x .,f,q;  if x=='' then return x
        if isnum(x) then return x/1; x=space(translate(x,,','),0)
        if isnum(x) then return x/1; return numnot()
numnot: if q==1 then return x; call er53 x
numx:   return num(arg(1),arg(2),1)
p:      return subword(arg(1),1,max(1,words(arg(1))-1))
putit:  parse arg px,which;if \isint(px) then px=numx(px)
        items.which=items.which+1;  _=items.which; item.which._=px; return
```

'''output''' is identical to the previous version.





## Ruby

No code, it's built in (uses '*' for multiplication):

```ruby
p 12345679e63 * 81 + 1e63
p 12345679012345679e54 * 81 + 1e54
p 12345679012345679012345679e45 * 81 + 1e45
p 12345679012345679012345679012345679e36 * 81 + 1e36
```

All result in 1.0e+72.


## Tcl

Tcl does not allow overriding the default mathematical operators (it does allow you to define your own expression engine — this is even relatively simple to do — but this is out of the scope of this task) but it also allows expressions to be written using a Lisp-like prefix form:

```tcl
namespace path ::tcl::mathop
for {set n -7; set e 63} {$n <= 21} {incr n;incr e -9} {
    append m 012345679
    puts $n:[+ [* [format "%se%s" $m $e] 81] 1e${e}]
}
```

The above won't work as intended though; the default implementation of the mathematical operators does not handle arbitrary precision floats (though in fact it will produce the right output with these specific values; this is a feature of the rounding used, and the fact that the exponent remains expressible in an IEEE double-precision float). So here is a version that does everything properly:

```tcl
namespace eval longfloat {
    proc + {num args} {
	set num [impl::Tidy $num]
	foreach x $args {
	    set num [impl::Add $num [impl::Tidy $x]]
	}
	return [impl::Normalize $num]
    }
    proc * {num args} {
	set num [impl::Tidy $num]
	foreach x $args {
	    set num [impl::Mul $num [impl::Tidy $x]]
	}
	return [impl::Normalize $num]
    }

    namespace export + *

    # This namespace contains the implementations of the operations and
    # isn't intended to be called directly by the rest of the program.
    namespace eval impl {
	variable FloatRE \
		{(?i)^([-+]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+))(?:e([-+]?[0-9]+))?$}

	proc Tidy {n} {
	    variable FloatRE
	    # Parse the input
	    if {[llength $n] == 2} {
		return $n
	    } elseif {[llength $n] != 1} {
		return -level 2 -code error "non-numeric argument"
	    } elseif {![regexp $FloatRE $n -> mantissa exponent]} {
		return -level 2 -code error "non-numeric argument"
	    }

	    # Default exponent is zero
	    if {$exponent eq ""} {
		set exponent 0
	    }
	    # Eliminate the decimal point
	    set bits [split $mantissa .]
	    if {[llength $bits] == 2} {
		incr exponent [expr {-[string length [lindex $bits 1]]}]
		set mantissa [join $bits ""]
	    }
	    # Trim useless leading zeroes
	    return [list [regsub {^([-+]?)0*([0-9])} $mantissa {\1\2}] $exponent]
	}

	proc Normalize {n} {
	    lassign $n mantissa exponent
	    # Trim useless trailing zeroes
	    while {[regexp {^([-+]?.+?)(0+)$} $mantissa -> head tail]} {
		set mantissa $head
		incr exponent [string length $tail]
	    }
	    # Human-readable form, please
	    return ${mantissa}e${exponent}
	}

	# Addition and multiplication on pairs of arbitrary-precision floats,
	# in decomposed form
	proc Add {a b} {
	    lassign $a am ae
	    lassign $b bm be
	    set de [expr {$ae - $be}]
	    if {$de < 0} {
		append bm [string repeat 0 [expr {-$de}]]
	    } elseif {$de > 0} {
		incr ae [expr {-$de}]
		append am [string repeat 0 $de]
	    }
	    list [expr {$am+$bm}] $ae
	}
	proc Mul {a b} {
	    lassign $a am ae
	    lassign $b bm be
	    list [expr {$am * $bm}] [expr {$ae + $be}]
	}
    }
}
```

Now we can demonstrate (compare with the original code to see how little has changed syntactically):

```tcl
namespace path longfloat
for {set n -7; set e 63} {$n <= 21} {incr n;incr e -9} {
    append m 012345679
    puts $n:[+ [* [format "%se%s" $m $e] 81] 1e${e}]
}
```

{{out|Output (trimmed)}}

```txt

-7:1e72
-6:1e72
-5:1e72
-4:1e72
………
18:1e72
19:1e72
20:1e72
21:1e72

```

If we extend the series to 40<!-- not shown here; very boring output! -->, we still get correct computations, and that's something which is not computable with normal IEEE arithmetic (due to problems with limits on the size of exponent<!-- happens at entry 29 -->) even at reduced precision.

Obviously, changing the code inside the <code>impl</code> namespace would alter how the calculation was carried out.
