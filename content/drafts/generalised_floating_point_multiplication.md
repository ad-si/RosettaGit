+++
title = "Generalised floating point multiplication"
description = ""
date = 2019-09-02T01:37:33Z
aliases = []
[extra]
id = 10738
[taxonomies]
categories = []
tags = []
+++

{{Template:Draft task}}
Use the [[Generalised floating point addition]] template to implement generalised floating point multiplication for a
[[wp:Balanced ternary|Balanced ternary]] test case.

'''Test case details''':
Balanced ternary is a way of representing numbers.  Unlike the prevailing binary representation, a balanced ternary "real" is in base 3, and each digit can have the values 1, 0, or −1.  For example, decimal 11 = 3<sup>2</sup> + 3<sup>1</sup> − 3<sup>0</sup>, thus can be written as "++−", while 6 = 3<sup>2</sup> − 3<sup>1</sup> + 0 × 3<sup>0</sup>, i.e., "+−0" and for an actual real number 6⅓ the ''exact'' representation is 3<sup>2</sup> − 3<sup>1</sup> + 0 × 3<sup>0</sup> + 1 × 3<sup>-1</sup> i.e., "+−0.+"

For this task, implement balanced ternary representation of real numbers with the following:

'''Requirements'''
# Support arbitrary precision real numbers, both positive and negative;
# Provide ways to convert to and from text strings, using digits '+', '-' and '0' (unless you are already using strings to represent balanced ternary; but see requirement 5).
# Provide ways to convert to and from native integer and real type (unless, improbably, your platform's native integer type ''is'' balanced ternary).  If your native integers can't support arbitrary length, overflows during conversion must be indicated.
# Provide ways to perform addition, negation and multiplication directly on balanced ternary integers; do ''not'' convert to native integers first.
# Make your implementation efficient, with a reasonable definition of "efficient" (and with a reasonable definition of "reasonable").
# The Template should successfully handle these multiplications in other bases.  In particular  [[wp:Septemvigesimal|Septemvigesimal]] and "Balanced base-27".

Optionally:
* For faster ''long'' multiplication use [[wp:Karatsuba_algorithm|Karatsuba algorithm]].
* Using the Karatsuba algorithm, spread the computation across multiple CPUs.

'''Test case 1''' - With balanced ternaries ''a'' from string "+-0++0+.+-0++0+", ''b'' from native real -436.436, ''c'' "+-++-.+-++-":
* write out ''a'', ''b'' and ''c'' in decimal notation.
* calculate ''a'' × (''b'' − ''c''), write out the result in both ternary and decimal notations.
* In the above limit the precision to 81 ternary digits after the point.

'''Test case 2''' - Generate a multiplication table of balanced ternaries where the rows of the table are for a 1st factor of 1 to 27, and the column of the table are for the second factor of 1 to 12.

Implement the code in a generalised form (such as a [[wp:Template (programming)|Template]], [[wp:Modular programming|Module]] or [[wp:Mixin|Mixin]] etc) that permits reusing of the code for different [[wp:Base_(exponentiation)#In_numeral_systems|Bases]].

If it is not possible to implement code in syntax of the specific language then:
* note the reason.
* perform the ''test case'' using a built-in or external library.
<!--
'''Test case:'''

Use the Template to define [[wp:Arbitrary-precision arithmetic|Arbitrary precision multiplication]] on numbers stored in Balanced Ternary.

{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Calculate the terms for -7 to 21 in this sequence of calculations
|-
! Number !! Term calculation !! Result
|-
| -7 || 111111111e63**2 x 81 + 2e135 - 1e126 || 1e144
|-
| -6 || 111111111111111111e54**2 x 81 + 2e126 - 1e108 || 1e144
|-
| -5 || 111111111111111111111111111e45**2 x 81 + 2e117 - 1e90 || 1e144
|-
| -4 || 111111111111111111111111111111111111e36**2 x 81 + 2e108 - 1e72 || 1e144
|-
| etc. || The last calculation will be with floating point numbers of more then 500 digits  || 1e144
|}
Note: The results will always be 1e144.
-->

## ALGOL 68

{{works with|ALGOL 68|Revision 1 - one minor extension to language used - PRAGMA READ, similar to C's #include directive.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.3.3 algol68g-2.3.3].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
'''File: Template.Big_float.Multiplication.a68'''
```algol68
##########################################
#               TASK CODE                #
# Actual generic mulitplication operator #
##########################################
# Alternatively use http://en.wikipedia.org/wiki/Karatsuba_algorithm #

OP * = (DIGITS a, b)DIGITS: (
  DIGITS minus one = -IDENTITY LOC DIGITS,
         zero = ZERO LOC DIGITS,
         one = IDENTITY LOC DIGITS;
  INT order = digit order OF arithmetic;
  IF SIGN a = 0 OR SIGN b = 0 THEN zero
CO # Note: The following require the inequality operators #
  ELIF a = one THEN b
  ELIF b = one THEN a
  ELIF a = minus one THEN -b
  ELIF b = minus one THEN -a
END CO
  ELSE
    DIGIT zero = ZERO LOC DIGIT;
    DIGIT one =  IDENTITY LOC DIGIT;
    [order + MSD a+MSD b: LSD a+LSD b]DIGIT a x b;

    FOR place FROM LSD a+LSD b BY order TO LSD a+MSD b DO
      a x b[place] := zero # pad the MSDs of the result with Zero #
    OD;
    FOR place a FROM LSD a BY order TO MSD a DO
      DIGIT digit a = a[place a];
      DIGIT carry := zero;
      FOR place b FROM LSD b BY order TO MSD b DO
        DIGIT digit b = b[place b];
        REF DIGIT digit ab = a x b[place a + place b];
        IF carry OF arithmetic THEN # used for big number arithmetic #
          MOID(carry := ( digit ab +:= carry ));
          DIGIT prod := digit a;
          MOID(carry +:= ( prod *:= digit b ));
          MOID(carry +:= ( digit ab +:= prod ))
        ELSE # carry = 0 so we can just ignore the carry #
          DIGIT prod := digit a;
          MOID(prod *:= digit b);
          MOID(digit ab +:= prod)
        FI
      OD;
      a x b[place a + MSD b + order] := carry
    OD;
    INITDIGITS a x b # normalise #
  FI
);

######################################
#  Define the hybrid multiplication  #
# operators for the generalised base #
######################################

OP * = (DIGIT a, DIGITS b)DIGITS: INITDIGITS a * b;
OP * = (DIGITS a, DIGIT b)DIGITS: a * INITDIGITS b;

OP *:= = (REF DIGITS lhs, DIGIT arg)DIGITS: lhs := lhs * INITDIGITS arg;

```
'''File: Template.Balanced_ternary_float.Base.a68'''
```algol68
PR READ "Template.Big_float_BCD.Base.a68" PR # [[rc:Generalised floating point addition]] #

################################################################
# First: define the attributes of the arithmetic we are using. #
################################################################
arithmetic := (
  # balanced = # TRUE,
  # carry = # TRUE,
  # base = # 3, # width = # 1, # places = # 81, # order = # -1,
  # repr = # USTRING("-","0","+")[@-1]
);

OP INITDIGIT = (CHAR c)DIGIT: (
  DIGIT out;
  digit OF out :=
    IF   c = "+" THEN +1
    ELIF c = "0" THEN  0
    ELIF c = "-" THEN -1
    ELSE raise value error("Unknown digit :"""+c+""""); SKIP
    FI;
  out
);

OP INITBIGREAL = (STRING s)BIGREAL: (
  BIGREAL out;
  BIGREAL base of arithmetic = INITBIGREAL base OF arithmetic; # Todo: Opt #
  INT point := UPB s; # put the point on the extreme right #
  FOR place FROM LWB s TO UPB s DO
    IF s[place]="." THEN
      point := place
    ELSE
      out := out SHR digit order OF arithmetic + INITDIGIT s[place]
    FI
  OD;
  out SHR (UPB s-point)
);
```
'''File: test.Balanced_ternary_float.Multiplication.a68'''
```algol68
#!/usr/local/bin/a68g --script #
####################################################################
# A program to test arbitrary length floating point multiplication #
####################################################################

PR READ "prelude/general.a68" PR  # [[rc:Template:ALGOL 68/prelude]] #

PR READ "Template.Big_float.Multiplication.a68" PR

# include the basic axioms of the digits being used #
PR READ "Template.Balanced_ternary_float.Base.a68" PR

PR READ "Template.Big_float.Addition.a68" PR # [[rc:Generalised floating point addition]] #
PR READ "Template.Big_float.Subtraction.a68" PR # [[rc:Generalised floating point addition]] #

test1:( # Basic arithmetic #
  INT rw = long real width;
  BIGREAL a = INITBIGREAL "+-0++0+.+-0++0+", # 523.239... #
          b = INITBIGREAL - LONG 436.436,
          c = INITBIGREAL "+-++-.+-++-"; # 65.267... #
  printf(($g 9k g(rw,rw-5)39kgl$,
    "a =",INITLONGREAL a, REPR a,
    "b =",INITLONGREAL b, REPR b,
    "c =",INITLONGREAL c, REPR c,
    "a*(b-c)",INITLONGREAL(a*(b-c)), REPR(a*(b-c)),
  $l$))
);

test2:( # A floating point Ternary multiplication table #
  FORMAT s = $"|"$; # field seperator #

  INT lwb = 1, tab = 8, upb = 12;

  printf($"# "f(s)" *   "f(s)$);
  FOR j FROM lwb TO upb DO
    FORMAT col = $n(tab)k f(s)$;
    printf(($g" #"g(0)f(col)$, REPR INITBIGREAL j,j))
  OD;
  printf($l$);
  FOR i FROM lwb TO 27 DO
    printf(($g(0) 3k f(s) g 9k f(s)$,i,REPR INITBIGREAL i));
    FOR j FROM lwb TO i MIN upb DO
      FORMAT col = $n(tab)k f(s)$;
      BIGREAL product = INITBIGREAL i * INITBIGREAL j;
      printf(($gf(col)$, REPR product))
    OD;
    IF upb > i THEN printf($n(upb-i)(n(tab-1)x f(s))$) FI;
    printf($l$)
  OD
)
```
'''Output:'''

```txt

a =     +523.23914037494284407864655  +-0++0+.+-0++0+
b =     -436.43600000000000000000000  -++-0--.--0+-00+++-0-+---0-+0++++0--0000+00-+-+--+0-0-00--++0-+00---+0+-+++0+-0----0++
c =      +65.26748971193415637860082  +-++-.+-++-
a*(b-c) -262510.90267998140903693919  ----000-0+0+.0+0-0-00---00--0-0+--+--00-0++-000++0-000-+0+-----+++-+-0+-+0+0++0+0-++-++0+---00++++

# | *   |+ #1   |+- #2  |+0 #3  |++ #4  |+-- #5 |+-0 #6 |+-+ #7 |+0- #8 |+e+- #9|+0+ #10|++- #11|++0 #12|
1 |+    |+      |       |       |       |       |       |       |       |       |       |       |       |
2 |+-   |+-     |++     |       |       |       |       |       |       |       |       |       |       |
3 |+0   |+0     |+-0    |+e+-   |       |       |       |       |       |       |       |       |       |
4 |++   |++     |+0-    |++0    |+--+   |       |       |       |       |       |       |       |       |
5 |+--  |+--    |+0+    |+--0   |+-+-   |+0-+   |       |       |       |       |       |       |       |
6 |+-0  |+-0    |++0    |+-e+-  |+0-0   |+0+0   |++e+-  |       |       |       |       |       |       |
7 |+-+  |+-+    |+---   |+-+0   |+00+   |++0-   |+---0  |+--++  |       |       |       |       |       |
8 |+0-  |+0-    |+--+   |+0-0   |++--   |++++   |+--+0  |+-0+-  |+-+0+  |       |       |       |       |
9 |+e+- |+e+-   |+-e+-  |+e+0   |++e+-  |+--e+- |+-e+0  |+-+e+- |+0-e+- |+e++   |       |       |       |
10|+0+  |+0+    |+-+-   |+0+0   |++++   |+-0--  |+-+-0  |+0--+  |+000-  |+0+e+- |++-0+  |       |       |
11|++-  |++-    |+-++   |++-0   |+--0-  |+-00+  |+-++0  |+00--  |+0+-+  |++-e+- |++0+-  |+++++  |       |
12|++0  |++0    |+0-0   |++e+-  |+--+0  |+-+-0  |+0-e+- |+00+0  |++--0  |++e+0  |++++0  |+--0-0 |+--+e+-|
13|+++  |+++    |+00-   |+++0   |+-0-+  |+-++-  |+00-0  |+0+0+  |++0--  |+++e+- |+---++ |+--+0- |+-0-+0 |
14|+--- |+---   |+00+   |+---0  |+-0+-  |+0--+  |+00+0  |++-0-  |++0++  |+---e+-|+--+-- |+-0-0+ |+-0+-0 |
15|+--0 |+--0   |+0+0   |+--e+- |+-+-0  |+0-+0  |+0+e+- |++0-0  |++++0  |+--e+0 |+-0--0 |+-00+0 |+-+-e+-|
16|+--+ |+--+   |++--   |+--+0  |+-+0+  |+000-  |++--0  |++0++  |+---+- |+--+e+-|+-00-+ |+-+--- |+-+0+0 |
17|+-0- |+-0-   |++-+   |+-0-0  |+0---  |+00++  |++-+0  |++++-  |+--00+ |+-0-e+-|+-0+0- |+-+0-+ |+0---0 |
18|+-e+-|+-e+-  |++e+-  |+-e+0  |+0-e+- |+0+e+- |++e+0  |+---e+-|+--+e+-|+-e++  |+-+-e+-|+-++e+-|+0-e+0 |
19|+-0+ |+-0+   |+++-   |+-0+0  |+0-++  |++---  |+++-0  |+--0-+ |+-0-0- |+-0+e+-|+-+00+ |+0--+- |+0-++0 |
20|+-+- |+-+-   |++++   |+-+-0  |+000-  |++-0+  |++++0  |+--+-- |+-00-+ |+-+-e+-|+-+++- |+0-0++ |+000-0 |
21|+-+0 |+-+0   |+---0  |+-+e+- |+00+0  |++0-0  |+---e+-|+--++0 |+-0+-0 |+-+e+0 |+0--+0 |+00--0 |+00+e+-|
22|+-++ |+-++   |+--0-  |+-++0  |+0+-+  |++0+-  |+--0-0 |+-0-0+ |+-+--- |+-++e+-|+0-0++ |+0000- |+0+-+0 |
23|+0-- |+0--   |+--0+  |+0--0  |+0++-  |+++-+  |+--0+0 |+-000- |+-+-++ |+0--e+-|+00--- |+00+0+ |+0++-0 |
24|+0-0 |+0-0   |+--+0  |+0-e+- |++--0  |++++0  |+--+e+-|+-0+-0 |+-+0+0 |+0-e+0 |+000-0 |+0+-+0 |++--e+-|
25|+0-+ |+0-+   |+-0--  |+0-+0  |++-0+  |+---0- |+-0--0 |+-0+++ |+-+++- |+0-+e+-|+00+-+ |+0++-- |++-0+0 |
26|+00- |+00-   |+-0-+  |+00-0  |++0--  |+---++ |+-0-+0 |+-+-+- |+0--0+ |+00-e+-|+0+-0- |++---+ |++0--0 |
27|+e+0 |+e+0   |+-e+0  |+e++   |++e+0  |+--e+0 |+-e++  |+-+e+0 |+0-e+0 |+e+--  |+0+e+0 |++-e+0 |++e++  |


```



## Go

{{trans|Phix}}
In the interests of brevity many of the comments and all of the commented-out code has been omitted.

```go
package main

import (
    "fmt"
    "log"
    "math"
    "strings"
)

const (
    maxdp           = 81
    binary          = "01"
    ternary         = "012"
    balancedTernary = "-0+"
    decimal         = "0123456789"
    hexadecimal     = "0123456789ABCDEF"
    septemVigesimal = "0123456789ABCDEFGHIJKLMNOPQ"
    balancedBase27  = "ZYXWVUTSRQPON0ABCDEFGHIJKLM"
    base37          = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
)

/* helper functions */

func changeByte(s string, idx int, c byte) string {
    bytes := []byte(s)
    bytes[idx] = c
    return string(bytes)
}

func removeByte(s string, idx int) string {
    le := len(s)
    bytes := []byte(s)
    copy(bytes[idx:], bytes[idx+1:])
    return string(bytes[0 : le-1])
}

func insertByte(s string, idx int, c byte) string {
    le := len(s)
    t := make([]byte, le+1)
    copy(t, s)
    copy(t[idx+1:], t[idx:])
    t[idx] = c
    return string(t)
}

func prependByte(s string, c byte) string {
    le := len(s)
    bytes := make([]byte, le+1)
    copy(bytes[1:], s)
    bytes[0] = c
    return string(bytes)
}

func abs(i int) int {
    if i < 0 {
        return -i
    }
    return i
}

// converts Phix indices to Go
func gIndex(pIndex, le int) int {
    if pIndex < 0 {
        return pIndex + le
    }
    return pIndex - 1
}

func getCarry(digit, base int) int {
    if digit > base {
        return 1
    } else if digit < 1 {
        return -1
    }
    return 0
}

// convert string 'b' to a decimal floating point number
func b2dec(b, alphabet string) float64 {
    res := 0.0
    base := len(alphabet)
    zdx := strings.IndexByte(alphabet, '0') + 1
    signed := zdx == 1 && b[0] == '-'
    if signed {
        b = b[1:]
    }
    le := len(b)
    ndp := strings.IndexByte(b, '.') + 1
    if ndp != 0 {
        b = removeByte(b, ndp-1) // remove decimal point
        ndp = le - ndp
    }
    for i := 1; i <= len(b); i++ {
        idx := strings.IndexByte(alphabet, b[i-1]) + 1
        res = float64(base)*res + float64(idx) - float64(zdx)
    }
    if ndp != 0 {
        res /= math.Pow(float64(base), float64(ndp))
    }
    if signed {
        res = -res
    }
    return res
}

// string 'b' can be balanced or unbalanced
func negate(b, alphabet string) string {
    if alphabet[0] == '0' {
        if b != "0" {
            if b[0] == '-' {
                b = b[1:]
            } else {
                b = prependByte(b, '-')
            }
        }
    } else {
        for i := 1; i <= len(b); i++ {
            if b[i-1] != '.' {
                idx := strings.IndexByte(alphabet, b[i-1]) + 1
                gi := gIndex(-idx, len(alphabet))
                b = changeByte(b, i-1, alphabet[gi])
            }
        }
    }
    return b
}

func bTrim(b string) string {
    // trim  trailing ".000"
    idx := strings.IndexByte(b, '.') + 1
    if idx != 0 {
        b = strings.TrimRight(strings.TrimRight(b, "0"), ".")
    }
    // trim leading zeros but not "0.nnn"
    for len(b) > 1 && b[0] == '0' && b[1] != '.' {
        b = b[1:]
    }
    return b
}

// for balanced number systems only
func bCarry(digit, base, idx int, n, alphabet string) (int, string) {
    carry := getCarry(digit, base)
    if carry != 0 {
        for i := idx; i >= 1; i-- {
            if n[i-1] != '.' {
                k := strings.IndexByte(alphabet, n[i-1]) + 1
                if k < base {
                    n = changeByte(n, i-1, alphabet[k])
                    break
                }
                n = changeByte(n, i-1, alphabet[0])
            }
        }
        digit -= base * carry
    }
    return digit, n
}

// convert a string from alphabet to alphabet2
func b2b(n, alphabet, alphabet2 string) string {
    res, m := "0", ""
    if n != "0" {
        base := len(alphabet)
        base2 := len(alphabet2)
        zdx := strings.IndexByte(alphabet, '0') + 1
        zdx2 := strings.IndexByte(alphabet2, '0') + 1
        var carry, q, r, digit int
        idx := strings.IndexByte(alphabet, n[0]) + 1
        negative := (zdx == 1 && n[0] == '-') || (zdx != 1 && idx < zdx)
        if negative {
            n = negate(n, alphabet)
        }
        ndp := strings.IndexByte(n, '.') + 1
        if ndp != 0 {
            n, m = n[0:ndp-1], n[ndp:]
        }
        res = ""
        for len(n) > 0 {
            q = 0
            for i := 1; i <= len(n); i++ {
                digit = strings.IndexByte(alphabet, n[i-1]) + 1 - zdx
                q = q*base + digit
                r = abs(q) % base2
                digit = abs(q)/base2 + zdx
                if q < 0 {
                    digit--
                }
                if zdx != 1 {
                    digit, n = bCarry(digit, base, i-1, n, alphabet)
                }
                n = changeByte(n, i-1, alphabet[digit-1])
                q = r
            }
            r += zdx2
            if zdx2 != 1 {
                r += carry
                carry = getCarry(r, base2)
                r -= base2 * carry
            }
            res = prependByte(res, alphabet2[r-1])
            n = strings.TrimLeft(n, "0")
        }
        if carry != 0 {
            res = prependByte(res, alphabet2[carry+zdx2-1])
        }
        if len(m) > 0 {
            res += "."
            ndp = 0
            if zdx != 1 {
                lm := len(m)
                alphaNew := base37[0:len(alphabet)]
                m = b2b(m, alphabet, alphaNew)
                m = strings.Repeat("0", lm-len(m)) + m
                alphabet = alphaNew
                zdx = 1
            }
            for len(m) > 0 && ndp < maxdp {
                q = 0
                for i := len(m); i >= 1; i-- {
                    digit = strings.IndexByte(alphabet, m[i-1]) + 1 - zdx
                    q += digit * base2
                    r = abs(q)%base + zdx
                    q /= base
                    if q < 0 {
                        q--
                    }
                    m = changeByte(m, i-1, alphabet[r-1])
                }
                digit = q + zdx2
                if zdx2 != 1 {
                    digit, res = bCarry(digit, base2, len(res), res, alphabet2)
                }
                res += string(alphabet2[digit-1])
                m = strings.TrimRight(m, "0")
                ndp++
            }
        }
        res = bTrim(res)
        if negative {
            res = negate(res, alphabet2)
        }
    }
    return res
}

// convert 'd' to a string in the specified base
func float2b(d float64, alphabet string) string {
    base := len(alphabet)
    zdx := strings.Index(alphabet, "0") + 1
    carry := 0
    neg := d < 0
    if neg {
        d = -d
    }
    res := ""
    whole := int(d)
    d -= float64(whole)
    for {
        ch := whole%base + zdx
        if zdx != 1 {
            ch += carry
            carry = getCarry(ch, base)
            ch -= base * carry
        }
        res = prependByte(res, alphabet[ch-1])
        whole /= base
        if whole == 0 {
            break
        }
    }
    if carry != 0 {
        res = prependByte(res, alphabet[carry+zdx-1])
        carry = 0
    }
    if d != 0 {
        res += "."
        ndp := 0
        for d != 0 && ndp < maxdp {
            d *= float64(base)
            digit := int(d) + zdx
            d -= float64(digit)
            if zdx != 1 {
                digit, res = bCarry(digit, base, len(res), res, alphabet)
            }
            res += string(alphabet[digit-1])
            ndp++
        }
    }
    if neg {
        res = negate(res, alphabet)
    }
    return res
}

func bAdd(a, b, alphabet string) string {
    base := len(alphabet)
    zdx := strings.IndexByte(alphabet, '0') + 1
    var carry, da, db, digit int
    if zdx == 1 {
        if a[0] == '-' {
            return bSub(b, negate(a, alphabet), alphabet)
        }
        if b[0] == '-' {
            return bSub(a, negate(b, alphabet), alphabet)
        }
    }
    adt := strings.IndexByte(a, '.') + 1
    bdt := strings.IndexByte(b, '.') + 1
    if adt != 0 || bdt != 0 {
        if adt != 0 {
            adt = len(a) - adt + 1
            gi := gIndex(-adt, len(a))
            a = removeByte(a, gi)
        }
        if bdt != 0 {
            bdt = len(b) - bdt + 1
            gi := gIndex(-bdt, len(b))
            b = removeByte(b, gi)
        }
        if bdt > adt {
            a += strings.Repeat("0", bdt-adt)
            adt = bdt
        } else if adt > bdt {
            b += strings.Repeat("0", adt-bdt)
        }
    }
    if len(a) < len(b) {
        a, b = b, a
    }
    for i := -1; i >= -len(a); i-- {
        if i < -len(a) {
            da = 0
        } else {
            da = strings.IndexByte(alphabet, a[len(a)+i]) + 1 - zdx
        }
        if i < -len(b) {
            db = 0
        } else {
            db = strings.IndexByte(alphabet, b[len(b)+i]) + 1 - zdx
        }
        digit = da + db + carry + zdx
        carry = getCarry(digit, base)
        a = changeByte(a, i+len(a), alphabet[digit-carry*base-1])
        if i < -len(b) && carry == 0 {
            break
        }
    }
    if carry != 0 {
        a = prependByte(a, alphabet[carry+zdx-1])
    }
    if adt != 0 {
        gi := gIndex(-adt+1, len(a))
        a = insertByte(a, gi, '.')
    }
    a = bTrim(a)
    return a
}

func aSmaller(a, b, alphabet string) bool {
    if len(a) != len(b) {
        log.Fatal("strings should be equal in length")
    }
    for i := 1; i <= len(a); i++ {
        da := strings.IndexByte(alphabet, a[i-1]) + 1
        db := strings.IndexByte(alphabet, b[i-1]) + 1
        if da != db {
            return da < db
        }
    }
    return false
}

func bSub(a, b, alphabet string) string {
    base := len(alphabet)
    zdx := strings.IndexByte(alphabet, '0') + 1
    var carry, da, db, digit int
    if zdx == 1 {
        if a[0] == '-' {
            return negate(bAdd(negate(a, alphabet), b, alphabet), alphabet)
        }
        if b[0] == '-' {
            return bAdd(a, negate(b, alphabet), alphabet)
        }
    }
    adt := strings.Index(a, ".") + 1
    bdt := strings.Index(b, ".") + 1
    if adt != 0 || bdt != 0 {
        if adt != 0 {
            adt = len(a) - adt + 1
            gi := gIndex(-adt, len(a))
            a = removeByte(a, gi)
        }
        if bdt != 0 {
            bdt = len(b) - bdt + 1
            gi := gIndex(-bdt, len(b))
            b = removeByte(b, gi)
        }
        if bdt > adt {
            a += strings.Repeat("0", bdt-adt)
            adt = bdt
        } else if adt > bdt {
            b += strings.Repeat("0", adt-bdt)
        }
    }
    bNegate := false
    if len(a) < len(b) || (len(a) == len(b) && aSmaller(a, b, alphabet)) {
        bNegate = true
        a, b = b, a
    }
    for i := -1; i >= -len(a); i-- {
        if i < -len(a) {
            da = 0
        } else {
            da = strings.IndexByte(alphabet, a[len(a)+i]) + 1 - zdx
        }
        if i < -len(b) {
            db = 0
        } else {
            db = strings.IndexByte(alphabet, b[len(b)+i]) + 1 - zdx
        }
        digit = da - db - carry + zdx
        carry = 0
        if digit <= 0 {
            carry = 1
        }
        a = changeByte(a, i+len(a), alphabet[digit+carry*base-1])
        if i < -len(b) && carry == 0 {
            break
        }
    }
    if carry != 0 {
        log.Fatal("carry should be zero")
    }
    if adt != 0 {
        gi := gIndex(-adt+1, len(a))
        a = insertByte(a, gi, '.')
    }
    a = bTrim(a)
    if bNegate {
        a = negate(a, alphabet)
    }
    return a
}

func bMul(a, b, alphabet string) string {
    zdx := strings.IndexByte(alphabet, '0') + 1
    dpa := strings.IndexByte(a, '.') + 1
    dpb := strings.IndexByte(b, '.') + 1
    ndp := 0
    if dpa != 0 {
        ndp += len(a) - dpa
        a = removeByte(a, dpa-1)
    }
    if dpb != 0 {
        ndp += len(b) - dpb
        b = removeByte(b, dpb-1)
    }
    pos, res := a, "0"
    if zdx != 1 {
        // balanced number systems
        neg := negate(pos, alphabet)
        for i := len(b); i >= 1; i-- {
            m := strings.IndexByte(alphabet, b[i-1]) + 1 - zdx
            for m != 0 {
                temp, temp2 := pos, -1
                if m < 0 {
                    temp = neg
                    temp2 = 1
                }
                res = bAdd(res, temp, alphabet)
                m += temp2
            }
            pos += "0"
            neg += "0"
        }
    } else {
        // non-balanced number systems
        negative := false
        if a[0] == '-' {
            a = a[1:]
            negative = true
        }
        if b[0] == '-' {
            b = b[1:]
            negative = !negative
        }
        for i := len(b); i >= 1; i-- {
            m := strings.IndexByte(alphabet, b[i-1]) + 1 - zdx
            for m > 0 {
                res = bAdd(res, pos, alphabet)
                m--
            }
            pos += "0"
        }
        if negative {
            res = negate(res, alphabet)
        }
    }
    if ndp != 0 {
        gi := gIndex(-ndp, len(res))
        res = insertByte(res, gi, '.')
    }
    res = bTrim(res)
    return res
}

func multTable() {
    fmt.Println("multiplication table")
    fmt.Println("
### ==============
")
    fmt.Printf("* |")
    for j := 1; j <= 12; j++ {
        fj := float64(j)
        fmt.Printf(" #%s %3s |", float2b(fj, hexadecimal), float2b(fj, balancedTernary))
    }
    for i := 1; i <= 27; i++ {
        fi := float64(i)
        a := float2b(fi, balancedTernary)
        fmt.Printf("\n%-2s|", float2b(fi, septemVigesimal))
        for j := 1; j <= 12; j++ {
            if j > i {
                fmt.Printf("        |")
            } else {
                fj := float64(j)
                b := float2b(fj, balancedTernary)
                m := bMul(a, b, balancedTernary)
                fmt.Printf(" %6s |", m)
            }
        }
    }
    fmt.Println()
}

func test(name, alphabet string) {
    a := b2b("+-0++0+.+-0++0+", balancedTernary, alphabet)
    b := b2b("-436.436", decimal, alphabet)
    c := b2b("+-++-.+-++-", balancedTernary, alphabet)
    d := bSub(b, c, alphabet)
    r := bMul(a, d, alphabet)
    fmt.Printf("%s\n%s\n", name, strings.Repeat("=", len(name)))
    fmt.Printf("      a = %.16g  %s\n", b2dec(a, alphabet), a)
    fmt.Printf("      b = %.16g  %s\n", b2dec(b, alphabet), b)
    fmt.Printf("      c = %.16g  %s\n", b2dec(c, alphabet), c)
    fmt.Printf("a*(b-c) = %.16g  %s\n\n", b2dec(r, alphabet), r)
}

func main() {
    test("balanced ternary", balancedTernary)
    test("balanced base 27", balancedBase27)
    test("decimal", decimal)
    test("binary", binary)
    test("ternary", ternary)
    test("hexadecimal", hexadecimal)
    test("septemvigesimal", septemVigesimal)
    multTable()
}
```


{{out}}

```txt

balanced ternary

### ==========

      a = 523.2391403749428  +-0++0+.+-0++0+
      b = -436.4359999999999  -++-0--.--0+-00+++-0-+---0-+0++++0--0000+00-+-+--+0-0-00--++0-+00---+0+-+++0+-0----0++
      c = 65.26748971193416  +-++-.+-++-
a*(b-c) = -262510.9026799813  ----000-0+0+.0+0-0-00---00--0-0+--+--00-0++-000++0-000-+0+-----+++-+-0+-+0+0++0+0-++-++0+---00++++

balanced base 27

### ==========

      a = 523.2391403749428  AUJ.FLI
      b = -436.4359999999999  NKQ.YFDFTYSMHVANGXPVXHIZJRJWZD0PBGFJAEBAKOZODLY0ITEHPQLSQSGLFZUINATKCIKUVMWEWJMQ0COTS
      c = 65.26748971193416  BK.GF
a*(b-c) = -262510.9026799812  ZVPJ.CWNYQPEENDVDPNJZXKFGCLHKLCX0YIBOMETHFWWBTVUFAH0SEZMTBJDCRRAQIQCAWMKXSTPYUXYPK0LODUO

decimal

### =

      a = 523.2391403749428  523.239140374942844078646547782350251486053955189757658893461362597165066300868770004
      b = -436.436  -436.436
      c = 65.26748971193413  65.267489711934156378600823045267489711934156378600823045267489711934156378600823045
a*(b-c) = -262510.9026799813  -262510.90267998140903693918986303277315826215892262734715612833785876513103053772667101895163734826631742752252837097627017862754285047634638652268078676654605120794218

binary
======
      a = 523.2391403749427  1000001011.001111010011100001001101101110011000100001011110100101001010100100000111001000111
      b = -436.436  -110110100.011011111001110110110010001011010000111001010110000001000001100010010011011101001
      c = 65.26748971193416  1000001.01000100011110100011010010101100110001100000111010111111101111001001001101111101
a*(b-c) = -262510.9026799814  -1000000000101101110.111001110001011000001001000001101110011111011100000100000100001000101011100011110010110001010100110111001011101001010000001110110100111110001101000000001111110101

ternary

### =

      a = 523.2391403749428  201101.0201101
      b = -436.4360000000002  -121011.102202211210021110012111201022222000202102010100101200200110122011122101110212
      c = 65.26748971193416  2102.02102
a*(b-c) = -262510.9026799813  -111100002121.2201010011100110022102110002120222120100001221111011202022012121122001201122110221112

hexadecimal

### =====

      a = 523.2391403749427  20B.3D384DB9885E94A90723EF9CBCB174B443E45FFC41152FE0293416F15E3AC303A0F3799ED81589C62
      b = -436.436  -1B4.6F9DB22D0E5604189374BC6A7EF9DB22D0E5604189374BC6A7EF9DB22D0E5604189374BC6A7EF9DB2
      c = 65.26748971193416  41.447A34ACC60EBFBC937D5DC2E5A99CF8A021B641511E8D2B3183AFEF24DF5770B96A673E28086D905
a*(b-c) = -262510.9026799814  -4016E.E7160906E7DC10422DA508321819F4A637E5AEE668ED5163B12FCB17A732442F589975B7F24112B2E8F6E95EAD45803915EE26D20DF323D67CAEEC75D7BED68AA34E02F2B492257D66F028545FB398F60E

septemvigesimal

### =========

      a = 523.2391403749428  JA.6C9
      b = -436.4359999999999  -G4.BKML7C5DJ8Q0KB39AIICH4HACN02OJKGPLOPG2D1MFBQI6LJ33F645JELD7I0Q6FNHG88E9M9GE3QO276
      c = 65.26748971193416  2B.76
a*(b-c) = -262510.9026799812  -D92G.OA1C42LM0N8N30HDAFKJNEIFEOB0BHP1DM6ILA9P797KPJ05MCE6OGMO54Q3I3NQ9DGB673C8BC2FQF1N82

multiplication table

### ==============

* | #1   + | #2  +- | #3  +0 | #4  ++ | #5 +-- | #6 +-0 | #7 +-+ | #8 +0- | #9 +00 | #A +0+ | #B ++- | #C ++0 |
1 |      + |        |        |        |        |        |        |        |        |        |        |        |
2 |     +- |     ++ |        |        |        |        |        |        |        |        |        |        |
3 |     +0 |    +-0 |    +00 |        |        |        |        |        |        |        |        |        |
4 |     ++ |    +0- |    ++0 |   +--+ |        |        |        |        |        |        |        |        |
5 |    +-- |    +0+ |   +--0 |   +-+- |   +0-+ |        |        |        |        |        |        |        |
6 |    +-0 |    ++0 |   +-00 |   +0-0 |   +0+0 |   ++00 |        |        |        |        |        |        |
7 |    +-+ |   +--- |   +-+0 |   +00+ |   ++0- |  +---0 |  +--++ |        |        |        |        |        |
8 |    +0- |   +--+ |   +0-0 |   ++-- |   ++++ |  +--+0 |  +-0+- |  +-+0+ |        |        |        |        |
9 |    +00 |   +-00 |   +000 |   ++00 |  +--00 |  +-000 |  +-+00 |  +0-00 |  +0000 |        |        |        |
A |    +0+ |   +-+- |   +0+0 |   ++++ |  +-0-- |  +-+-0 |  +0--+ |  +000- |  +0+00 |  ++-0+ |        |        |
B |    ++- |   +-++ |   ++-0 |  +--0- |  +-00+ |  +-++0 |  +00-- |  +0+-+ |  ++-00 |  ++0+- |  +++++ |        |
C |    ++0 |   +0-0 |   ++00 |  +--+0 |  +-+-0 |  +0-00 |  +00+0 |  ++--0 |  ++000 |  ++++0 | +--0-0 | +--+00 |
D |    +++ |   +00- |   +++0 |  +-0-+ |  +-++- |  +00-0 |  +0+0+ |  ++0-- |  +++00 | +---++ | +--+0- | +-0-+0 |
E |   +--- |   +00+ |  +---0 |  +-0+- |  +0--+ |  +00+0 |  ++-0- |  ++0++ | +---00 | +--+-- | +-0-0+ | +-0+-0 |
F |   +--0 |   +0+0 |  +--00 |  +-+-0 |  +0-+0 |  +0+00 |  ++0-0 |  ++++0 | +--000 | +-0--0 | +-00+0 | +-+-00 |
G |   +--+ |   ++-- |  +--+0 |  +-+0+ |  +000- |  ++--0 |  ++0++ | +---+- | +--+00 | +-00-+ | +-+--- | +-+0+0 |
H |   +-0- |   ++-+ |  +-0-0 |  +0--- |  +00++ |  ++-+0 |  ++++- | +--00+ | +-0-00 | +-0+0- | +-+0-+ | +0---0 |
I |   +-00 |   ++00 |  +-000 |  +0-00 |  +0+00 |  ++000 | +---00 | +--+00 | +-0000 | +-+-00 | +-++00 | +0-000 |
J |   +-0+ |   +++- |  +-0+0 |  +0-++ |  ++--- |  +++-0 | +--0-+ | +-0-0- | +-0+00 | +-+00+ | +0--+- | +0-++0 |
K |   +-+- |   ++++ |  +-+-0 |  +000- |  ++-0+ |  ++++0 | +--+-- | +-00-+ | +-+-00 | +-+++- | +0-0++ | +000-0 |
L |   +-+0 |  +---0 |  +-+00 |  +00+0 |  ++0-0 | +---00 | +--++0 | +-0+-0 | +-+000 | +0--+0 | +00--0 | +00+00 |
M |   +-++ |  +--0- |  +-++0 |  +0+-+ |  ++0+- | +--0-0 | +-0-0+ | +-+--- | +-++00 | +0-0++ | +0000- | +0+-+0 |
N |   +0-- |  +--0+ |  +0--0 |  +0++- |  +++-+ | +--0+0 | +-000- | +-+-++ | +0--00 | +00--- | +00+0+ | +0++-0 |
O |   +0-0 |  +--+0 |  +0-00 |  ++--0 |  ++++0 | +--+00 | +-0+-0 | +-+0+0 | +0-000 | +000-0 | +0+-+0 | ++--00 |
P |   +0-+ |  +-0-- |  +0-+0 |  ++-0+ | +---0- | +-0--0 | +-0+++ | +-+++- | +0-+00 | +00+-+ | +0++-- | ++-0+0 |
Q |   +00- |  +-0-+ |  +00-0 |  ++0-- | +---++ | +-0-+0 | +-+-+- | +0--0+ | +00-00 | +0+-0- | ++---+ | ++0--0 |
10|   +000 |  +-000 |  +0000 |  ++000 | +--000 | +-0000 | +-+000 | +0-000 | +00000 | +0+000 | ++-000 | ++0000 |

```



## Phix

Note regarding requirement #5: While this meets my definition of "reasonably efficient",
it should not shock anyone that this kind of "string maths" which works digit-by-digit
and uses repeated addition (eg *999 performs 27 additions) could easily be 10,000 times
slower than raw hardware or a carefully optimised library such as gmp. However this does
offer perfect accuracy in any given base, whereas gmp, for all it's brilliance, can hold
0.1 accurate to several million decimal places, but just never quite exact.


```Phix
-- demo\rosetta\Generic_multiplication.exw
constant MAX_DP = 81

constant binary = "01",
         ternary = "012",
         balancedternary = "-0+",
         decimal = "0123456789",
         hexadecimal = "0123456789ABCDEF",
         septemvigesimal = "0123456789ABCDEFGHIJKLMNOPQ",
--       heptavintimal   = "0123456789ABCDEFGHKMNPRTVXZ", -- ??
--       wonky_donkey_26 = "0ABCDEFGHIJKLMNOPQRSTUVWXY",
--       wonky_donkey_27 = "0ABCDEFGHIJKLMNOPQRSTUVWXYZ",
         balanced_base27 = "ZYXWVUTSRQPON0ABCDEFGHIJKLM",
         base37 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
--
--Note: I have seen some schemes where balanced-base-27 uses
--====  the same character set as septemvigesimal, with 'D'
--      representing 0, and wonky_donkey_27 with 'M'==0(!).
--      These routines do not support that directly, except
--      (perhaps) via a simple mapping on all inputs/outputs.
--      It may be possible to add a defaulted parameter such
--      as zero='0' - left as an exercise for the reader.
--      Admittedly that balanced_base27 is entirely my own
--      invention, just for this specific task.
--

function b2dec(string b, alphabet)
--
-- convert string b back into a normal (decimal) atom,
--  eg b2dec("+0-",balancedternary) yields 8
--
    atom res = 0
    integer base = length(alphabet),
            zdx = find('0',alphabet)
    bool signed = (zdx=1 and b[1]='-')
    if signed then b = b[2..$] end if
    integer len = length(b),
            ndp = find('.',b)
    if ndp!=0 then
        b[ndp..ndp] = "" -- remove '.'
        ndp = len-ndp
    end if
    for i=1 to length(b) do
        res = base*res+find(b[i],alphabet)-zdx
    end for
    if ndp!=0 then res /= power(base,ndp) end if
    if signed then res = -res end if
    return res
end function

function negate(string b, alphabet)
--
-- negate b (can be balanced or unbalanced)
--
    if alphabet[1]='0' then
        -- traditional: add/remove a leading '-'
        -- eg "-123" <==> "123"
        if b!="0" then
            if b[1]='-' then
                b = b[2..$]
            else
                b = "-"&b
            end if
        end if
    else
        -- balanced: mirror [non-0] digits
        -- eg "-0+" (ie -8) <==> "+0-" (ie +8)
        for i=1 to length(b) do
            if b[i]!='.' then
                b[i] = alphabet[-find(b[i],alphabet)]
            end if
        end for
    end if
    return b
end function

function b_trim(string b)
-- (common code)
    -- trim trailing ".000"
    if find('.',b) then
        b = trim_tail(trim_tail(b,'0'),'.')
    end if
    -- trim leading zeroes, but not "0.nnn" -> ".nnn"
    -- [hence we cannot use the standard trim_head()]
    while length(b)>1 and b[1]='0' and b[2]!='.' do
        b = b[2..$]
    end while
    return b
end function

function b_carry(integer digit, base, idx, string n, alphabet)
-- (common code, for balanced number systems only)
    integer carry = iff(digit>base?+1:iff(digit<1?-1:0))
    if carry then
        for i=idx to 0 by -1 do
            if n[i]!='.' then
                integer k = find(n[i],alphabet)
                if k<base then
                    n[i] = alphabet[k+1]
                    exit
                end if
                n[i]=alphabet[1]
            end if
        end for
        digit -= base*carry
    end if
    return {digit,n}
end function

function b2b(string n, alphabet, alphabet2)
--
-- convert a string from alphabet to alphabet2,
--  eg b2b("8",decimal,balancedternary) yields "+0-",
--   & b2b("+0-",balancedternary,decimal) yields "8",
--
    string res = "0", m = ""
    if n!="0" then
        integer base = length(alphabet),
                base2 = length(alphabet2),
                zdx = find('0',alphabet),
                zdx2 = find('0',alphabet2),
                carry = 0, q, r, digit
        bool negative = ((zdx=1 and n[1]='-') or
                         (zdx!=1 and find(n[1],alphabet)<zdx))
        if negative then n = negate(n,alphabet) end if
        integer ndp = find('.',n)
        if ndp!=0 then
            {n,m} = {n[1..ndp-1],n[ndp+1..$]}
        end if
        res = ""
        while length(n) do
            q = 0
            for i=1 to length(n) do
                --
                -- this is a digit-by-digit divide (/mod) loop
                -- eg for hex->decimal we would want:
                --  this loop/modrem("FFFF",10) --> "1999" rem 5,
                --  this loop/modrem("1999",10) --> "28F" rem 3,
                --  this loop/modrem("28F",10) --> "41" rem 5,
                --  this loop/modrem("41",10) --> "6" rem 5,
                --  this loop/modrem("6",10) --> "0" rem 6,
                -- ==> res:="65535" (in 5 full iterations over n).
                --
                digit = find(n[i],alphabet)-zdx
                q = q*base+digit
                r = mod(q,base2)
                digit = floor(q/base2)+zdx
                if zdx!=1 then
                    {digit,n} = b_carry(digit,base,i-1,n,alphabet)
                end if
                n[i] = alphabet[digit]
                q = r
            end for
            r += zdx2
            if zdx2!=1 then
                r += carry
                carry = iff(r>base2?+1:iff(r<1?-1:0))
                r -= base2*carry
            end if
            res = alphabet2[r]&res
            n = trim_head(n,'0')
        end while
        if carry then
            res = alphabet2[carry+zdx2]&res
        end if
        if length(m) then
            res &= '.'
            ndp = 0
            if zdx!=1 then
                -- convert fraction to unbalanced, to simplify the (other-base) multiply.
                integer lm = length(m)
                string alphanew = base37[1..length(alphabet)]
                m = b2b(m,alphabet,alphanew) -- (nb: no fractional part!)
                m = repeat('0',lm-length(m))&m -- zero-pad if required
                alphabet = alphanew
                zdx = 1
            end if
            while length(m) and ndp<MAX_DP do
                q = 0
                for i=length(m) to 1 by -1 do
                    --
                    -- this is a digit-by-digit multiply loop
                    -- eg for [.]"1415" decimal->decimal we
                    -- would repeatedly multiply by 10, giving
                    -- 1 and "4150", then 4 and "1500", then
                    -- 1 and "5000", then 5 and "0000". We
                    -- strip zeroes between each output digit
                    -- & obviously normally alphabet in!=out.
                    --
                    digit = find(m[i],alphabet)-zdx
                    q += digit*base2
                    r = mod(q,base)+zdx
                    q = floor(q/base)
                    m[i] = alphabet[r]
                end for
                digit = q + zdx2
                if zdx2!=1 then
                    {digit,res} = b_carry(digit,base2,length(res),res,alphabet2)
                end if
                res &= alphabet2[digit]
                m = trim_tail(m,'0')
                ndp += 1
            end while
        end if
        res = b_trim(res)
        if negative then res = negate(res,alphabet2) end if
    end if
    return res
end function

function atm2b(atom d, string alphabet)
--
-- convert d to a string in the specified base,
--   eg atm2b(65535,hexadecimal) => "FFFF"
--
-- As a standard feature of phix, you can actually specify
-- d in any number base between 2 and 36, eg 0(13)168 is
-- equivalent to 255 (see test\t37misc.exw for more), but
-- not (yet) in balanced number bases, or with fractions,
-- except (of course) for normal decimal fractions.
--
-- Note that eg b2b("-436.436",decimal,balancedternary) is
-- more acccurate that atm2b(-436.436,balancedternary) due
-- to standard IEEE 754 floating point limitations.
-- For integers, discrepancies only creep in for values
-- outside the range +/-9,007,199,254,740,992 (on 32-bit).
-- However, this is much simpler and faster than b2b().
--
    integer base = length(alphabet),
            zdx = find('0',alphabet),
            carry = 0
    bool neg = d<0
    if neg then d = -d end if
    string res = ""
    integer whole = floor(d)
    d -= whole
    while true do
        integer ch = mod(whole,base) + zdx
        if zdx!=1 then
            ch += carry
            carry = iff(ch>base?+1:iff(ch<1?-1:0))
            ch -= base*carry
        end if
        res = alphabet[ch]&res
        whole = floor(whole/base)
        if whole=0 then exit end if
    end while
    if carry then
        res = alphabet[carry+zdx]&res
        carry = 0
    end if
    if d!=0 then
        res &= '.'
        integer ndp = 0
        while d!=0 and ndp<MAX_DP do
            d *= base
            integer digit = floor(d) + zdx
            d -= digit
            if zdx!=1 then
                {digit,res} = b_carry(digit,base,length(res),res,alphabet)
            end if
            res &= alphabet[digit]
            ndp += 1
        end while
    end if
    if neg then res = negate(res,alphabet) end if
    return res
end function

-- negative numbers in addition and subtraction
-- (esp. non-balanced) are treated as follows:
-- for -ve a:   (-a)+b == b-a;      (-a)-b == -(a+b)
-- for -ve b:   a+(-b) == a-b;      a-(-b) == a+b
-- for a>b:     a-b == -(b-a)  [avoid running off end]

forward function b_sub(string a, b, alphabet)

function b_add(string a, b, alphabet)
    integer base = length(alphabet),
            zdx = find('0',alphabet),
            carry = 0, da, db, digit
    if zdx=1 then
        if a[1]='-' then    -- (-a)+b == b-a
            return b_sub(b,negate(a,alphabet),alphabet)
        end if
        if b[1]='-' then    -- a+(-b) == a-b
            return b_sub(a,negate(b,alphabet),alphabet)
        end if
    end if
    integer adt = find('.',a),
            bdt = find('.',b)
    if adt or bdt then
        -- remove the '.'s and zero-pad the shorter as needed
        --   (thereafter treat them as two whole integers)
        -- eg "1.23"+"4.5" -> "123"+"450" (leaving adt==2)
        if adt then adt = length(a)-adt+1;  a[-adt..-adt] = "" end if
        if bdt then bdt = length(b)-bdt+1;  b[-bdt..-bdt] = "" end if
        if bdt>adt then
            a &= repeat('0',bdt-adt)
            adt = bdt
        elsif adt>bdt then
            b &= repeat('0',adt-bdt)
        end if
    end if
    if length(a)<length(b) then
        {a,b} = {b,a}   -- ensure b is the shorter
    end if
    for i=-1 to -length(a) by -1 do
        da = iff(i<-length(a)?0:find(a[i],alphabet)-zdx)
        db = iff(i<-length(b)?0:find(b[i],alphabet)-zdx)
        digit = da + db + carry + zdx
        carry = iff(digit>base?+1:iff(digit<1?-1:0))
        a[i] = alphabet[digit-carry*base]
        if i<-length(b) and carry=0 then exit end if
    end for
    if carry then
        a = alphabet[carry+zdx]&a
    end if
    if adt then
        a[-adt+1..-adt] = "."
    end if
    a = b_trim(a)
    return a
end function

function a_smaller(string a, b, alphabet)
-- return true if a is smaller than b
-- if not balanced then both are +ve
    if length(a)!=length(b) then ?9/0 end if -- sanity check
    for i=1 to length(a) do
        integer da = find(a[i],alphabet),
                db = find(b[i],alphabet),
                c = compare(a,b)
        if c!=0 then return c<0 end if
    end for
    return false -- (=, which is not <)
end function

function b_sub(string a, b, alphabet)
    integer base = length(alphabet),
            zdx = find('0',alphabet),
            carry = 0, da, db, digit
    if zdx=1 then
        if a[1]='-' then    -- (-a)-b == -(a+b)
            return negate(b_add(negate(a,alphabet),b,alphabet),alphabet)
        end if
        if b[1]='-' then    -- a-(-b) == a+b
            return b_add(a,negate(b,alphabet),alphabet)
        end if
    end if
    integer adt = find('.',a),
            bdt = find('.',b)
    if adt or bdt then
        -- remove the '.'s and zero-pad the shorter as needed
        --   (thereafter treat them as two whole integers)
        -- eg "1.23"+"4.5" -> "123"+"450" (leaving adt==2)
        if adt then adt = length(a)-adt+1;  a[-adt..-adt] = "" end if
        if bdt then bdt = length(b)-bdt+1;  b[-bdt..-bdt] = "" end if
        if bdt>adt then
            a &= repeat('0',bdt-adt)
            adt = bdt
        elsif adt>bdt then
            b &= repeat('0',adt-bdt)
        end if
    end if
    bool bNegate = false
    if length(a)<length(b)
    or (length(a)=length(b) and a_smaller(a,b,alphabet)) then
        bNegate = true
        {a,b} = {b,a}   -- ensure b is the shorter/smaller
    end if
    for i=-1 to -length(a) by -1 do
        da = iff(i<-length(a)?0:find(a[i],alphabet)-zdx)
        db = iff(i<-length(b)?0:find(b[i],alphabet)-zdx)
        digit = da - (db + carry) + zdx
        carry = digit<=0
        a[i] = alphabet[digit+carry*base]
        if i<-length(b) and carry=0 then exit end if
    end for
    if carry then
        ?9/0    -- should have set bNegate above...
    end if
    if adt then
        a[-adt+1..-adt] = "."
    end if
    a = b_trim(a)
    if bNegate then
        a = negate(a,alphabet)
    end if
    return a
end function

function b_mul(string a, b, alphabet)
    integer base = length(alphabet),
            zdx = find('0',alphabet),
            dpa = find('.',a),
            dpb = find('.',b),
            ndp = 0
    if dpa then ndp += length(a)-dpa; a[dpa..dpa] = "" end if
    if dpb then ndp += length(b)-dpb; b[dpb..dpb] = "" end if
    string pos = a, res = "0"
    if zdx!=1 then
        -- balanced number systems
        string neg = negate(pos,alphabet)
        for i=length(b) to 1 by -1 do
            integer m = find(b[i],alphabet)-zdx
            while m do
                res = b_add(res,iff(m<0?neg:pos),alphabet)
                m += iff(m<0?+1:-1)
            end while
            pos &= '0'
            neg &= '0'
        end for
    else
        -- non-balanced (normal) number systems
        bool negative = false
        if a[1]='-' then a = a[2..$]; negative = true end if
        if b[1]='-' then b = b[2..$]; negative = not negative end if
        for i=length(b) to 1 by -1 do
            integer m = find(b[i],alphabet)-zdx
            while m>0 do
                res = b_add(res,pos,alphabet)
                m -= 1
            end while
            pos &= '0'
        end for
        if negative then res = negate(res,alphabet) end if
    end if
    if ndp then
        res[-ndp..-ndp-1] = "."
    end if
    res = b_trim(res)
    return res
end function

-- [note 1] not surprisingly, the decimal output is somewhat cleaner/shorter when
--          the decimal string inputs for a and c are used, whereas tests 1/2/5/7
--          (the 3-based ones) look much better with all ternary string inputs.

procedure test(string name, alphabet)
--string a = b2b("523.2391403749428",decimal,alphabet),         -- [see note 1]
string a = b2b("+-0++0+.+-0++0+",balancedternary,alphabet),
       b = b2b("-436.436",decimal,alphabet),
--     b = b2b("-++-0--.--0+-00+++-",balancedternary,alphabet),
--     c = b2b("65.26748971193416",decimal,alphabet),           -- [see note 1]
       c = b2b("+-++-.+-++-",balancedternary,alphabet),
       d = b_add(b,c,alphabet),
       r = b_mul(a,d,alphabet)
    printf(1,"%s\n%s\n",{name,repeat('=',length(name))})
    printf(1,"      a = %.16g  %s\n",{b2dec(a,alphabet),a})
    printf(1,"      b = %.16g  %s\n",{b2dec(b,alphabet),b})
    printf(1,"      c = %.16g  %s\n",{b2dec(c,alphabet),c})
--  printf(1,"      d = %.16g  %s\n",{b2dec(d,alphabet),d})
    printf(1,"a*(b-c) = %.16g  %s\n\n",{b2dec(r,alphabet),r})
end procedure
test("balanced ternary", balancedternary)
test("balanced base 27", balanced_base27)
test("decimal", decimal)
test("binary", binary)
test("ternary", ternary)
test("hexadecimal", hexadecimal)
test("septemvigesimal", septemvigesimal)
```

The printed decimal output is inherently limited to IEEE 754 precision, hence I
deliberately limited output (%.16g) because it is silly to try and go any higher,
whereas the output from b_mul() is actually perfectly accurate, see [note 1] above.
{{out}}

```txt

balanced ternary

### ==========

      a = 523.2391403749428  +-0++0+.+-0++0+
      b = -436.4359999999999  -++-0--.--0+-00+++-0-+---0-+0++++0--0000+00-+-+--+0-0-00--++0-+00---+0+-+++0+-0----0++
      c = 65.26748971193416  +-++-.+-++-
a*(b-c) = -262510.9026799813  ----000-0+0+.0+0-0-00---00--0-0+--+--00-0++-000++0-000-+0+-----+++-+-0+-+0+0++0+0-++-++0+---00++++

balanced base 27

### ==========

      a = 523.2391403749428  AUJ.FLI
      b = -436.436  NKQ.YFDFTYSMHVANGXPVXHIZJRJWZD0PBGFJAEBAKOZODLY0ITEHPQLSQSGLFZUINATKCIKUVMWEWJMQ0COTS
      c = 65.26748971193416  BK.GF
a*(b-c) = -262510.9026799813  ZVPJ.CWNYQPEENDVDPNJZXKFGCLHKLCX0YIBOMETHFWWBTVUFAH0SEZMTBJDCRRAQIQCAWMKXSTPYUXYPK0LODUO

decimal

### =

      a = 523.239140374943  523.239140374942844078646547782350251486053955189757658893461362597165066300868770004
      b = -436.436  -436.436
      c = 65.26748971193415  65.267489711934156378600823045267489711934156378600823045267489711934156378600823045
a*(b-c) = -262510.9026799814  -262510.90267998140903693918986303277315826215892262734715612833785876513103053772667101895163734826631742752252837097627017862754285047634638652268078676654605120794218

binary
======
      a = 523.2391403749427  1000001011.001111010011100001001101101110011000100001011110100101001010100100000111001000111
      b = -436.436  -110110100.011011111001110110110010001011010000111001010110000001000001100010010011011101001
      c = 65.26748971193416  1000001.01000100011110100011010010101100110001100000111010111111101111001001001101111101
a*(b-c) = -262510.9026799814  -1000000000101101110.111001110001011000001001000001101110011111011100000100000100001000101011100011110010110001010100110111001011101001010000001110110100111110001101000000001111110101

ternary

### =

      a = 523.2391403749428  201101.0201101
      b = -436.4360000000001  -121011.102202211210021110012111201022222000202102010100101200200110122011122101110212
      c = 65.26748971193416  2102.02102
a*(b-c) = -262510.9026799813  -111100002121.2201010011100110022102110002120222120100001221111011202022012121122001201122110221112

hexadecimal

### =====

      a = 523.2391403749427  20B.3D384DB9885E94A90723EF9CBCB174B443E45FFC41152FE0293416F15E3AC303A0F3799ED81589C62
      b = -436.436  -1B4.6F9DB22D0E5604189374BC6A7EF9DB22D0E5604189374BC6A7EF9DB22D0E5604189374BC6A7EF9DB2
      c = 65.26748971193416  41.447A34ACC60EBFBC937D5DC2E5A99CF8A021B641511E8D2B3183AFEF24DF5770B96A673E28086D905
a*(b-c) = -262510.9026799814  -4016E.E7160906E7DC10422DA508321819F4A637E5AEE668ED5163B12FCB17A732442F589975B7F24112B2E8F6E95EAD45803915EE26D20DF323D67CAEEC75D7BED68AA34E02F2B492257D66F028545FB398F60E

septemvigesimal

### =========

      a = 523.2391403749428  JA.6C9
      b = -436.436  -G4.BKML7C5DJ8Q0KB39AIICH4HACN02OJKGPLOPG2D1MFBQI6LJ33F645JELD7I0Q6FNHG88E9M9GE3QO276
      c = 65.26748971193416  2B.76
a*(b-c) = -262510.9026799813  -D92G.OA1C42LM0N8N30HDAFKJNEIFEOB0BHP1DM6ILA9P797KPJ05MCE6OGMO54Q3I3NQ9DGB673C8BC2FQF1N82

```



###  multiplication table

Without e notation, with hexadecimal across, septemvigesimal down, and balanced ternary contents!

```Phix
printf(1,"* |")
for j=1 to 12 do
    printf(1," #%s %3s |",{atm2b(j,hexadecimal),atm2b(j,balancedternary)})
end for
for i=1 to 27 do
    string a = atm2b(i,balancedternary)
    printf(1,"\n%-2s|",{atm2b(i,septemvigesimal)})
    for j=1 to 12 do
        if j>i then
            printf(1,"        |")
        else
            string b = atm2b(j,balancedternary)
            string m = b_mul(a,b,balancedternary)
            printf(1," %6s |",{m})
        end if
    end for
end for
printf(1,"\n")
```

{{out}}

```txt

* | #1   + | #2  +- | #3  +0 | #4  ++ | #5 +-- | #6 +-0 | #7 +-+ | #8 +0- | #9 +00 | #A +0+ | #B ++- | #C ++0 |
1 |      + |        |        |        |        |        |        |        |        |        |        |        |
2 |     +- |     ++ |        |        |        |        |        |        |        |        |        |        |
3 |     +0 |    +-0 |    +00 |        |        |        |        |        |        |        |        |        |
4 |     ++ |    +0- |    ++0 |   +--+ |        |        |        |        |        |        |        |        |
5 |    +-- |    +0+ |   +--0 |   +-+- |   +0-+ |        |        |        |        |        |        |        |
6 |    +-0 |    ++0 |   +-00 |   +0-0 |   +0+0 |   ++00 |        |        |        |        |        |        |
7 |    +-+ |   +--- |   +-+0 |   +00+ |   ++0- |  +---0 |  +--++ |        |        |        |        |        |
8 |    +0- |   +--+ |   +0-0 |   ++-- |   ++++ |  +--+0 |  +-0+- |  +-+0+ |        |        |        |        |
9 |    +00 |   +-00 |   +000 |   ++00 |  +--00 |  +-000 |  +-+00 |  +0-00 |  +0000 |        |        |        |
A |    +0+ |   +-+- |   +0+0 |   ++++ |  +-0-- |  +-+-0 |  +0--+ |  +000- |  +0+00 |  ++-0+ |        |        |
B |    ++- |   +-++ |   ++-0 |  +--0- |  +-00+ |  +-++0 |  +00-- |  +0+-+ |  ++-00 |  ++0+- |  +++++ |        |
C |    ++0 |   +0-0 |   ++00 |  +--+0 |  +-+-0 |  +0-00 |  +00+0 |  ++--0 |  ++000 |  ++++0 | +--0-0 | +--+00 |
D |    +++ |   +00- |   +++0 |  +-0-+ |  +-++- |  +00-0 |  +0+0+ |  ++0-- |  +++00 | +---++ | +--+0- | +-0-+0 |
E |   +--- |   +00+ |  +---0 |  +-0+- |  +0--+ |  +00+0 |  ++-0- |  ++0++ | +---00 | +--+-- | +-0-0+ | +-0+-0 |
F |   +--0 |   +0+0 |  +--00 |  +-+-0 |  +0-+0 |  +0+00 |  ++0-0 |  ++++0 | +--000 | +-0--0 | +-00+0 | +-+-00 |
G |   +--+ |   ++-- |  +--+0 |  +-+0+ |  +000- |  ++--0 |  ++0++ | +---+- | +--+00 | +-00-+ | +-+--- | +-+0+0 |
H |   +-0- |   ++-+ |  +-0-0 |  +0--- |  +00++ |  ++-+0 |  ++++- | +--00+ | +-0-00 | +-0+0- | +-+0-+ | +0---0 |
I |   +-00 |   ++00 |  +-000 |  +0-00 |  +0+00 |  ++000 | +---00 | +--+00 | +-0000 | +-+-00 | +-++00 | +0-000 |
J |   +-0+ |   +++- |  +-0+0 |  +0-++ |  ++--- |  +++-0 | +--0-+ | +-0-0- | +-0+00 | +-+00+ | +0--+- | +0-++0 |
K |   +-+- |   ++++ |  +-+-0 |  +000- |  ++-0+ |  ++++0 | +--+-- | +-00-+ | +-+-00 | +-+++- | +0-0++ | +000-0 |
L |   +-+0 |  +---0 |  +-+00 |  +00+0 |  ++0-0 | +---00 | +--++0 | +-0+-0 | +-+000 | +0--+0 | +00--0 | +00+00 |
M |   +-++ |  +--0- |  +-++0 |  +0+-+ |  ++0+- | +--0-0 | +-0-0+ | +-+--- | +-++00 | +0-0++ | +0000- | +0+-+0 |
N |   +0-- |  +--0+ |  +0--0 |  +0++- |  +++-+ | +--0+0 | +-000- | +-+-++ | +0--00 | +00--- | +00+0+ | +0++-0 |
O |   +0-0 |  +--+0 |  +0-00 |  ++--0 |  ++++0 | +--+00 | +-0+-0 | +-+0+0 | +0-000 | +000-0 | +0+-+0 | ++--00 |
P |   +0-+ |  +-0-- |  +0-+0 |  ++-0+ | +---0- | +-0--0 | +-0+++ | +-+++- | +0-+00 | +00+-+ | +0++-- | ++-0+0 |
Q |   +00- |  +-0-+ |  +00-0 |  ++0-- | +---++ | +-0-+0 | +-+-+- | +0--0+ | +00-00 | +0+-0- | ++---+ | ++0--0 |
10|   +000 |  +-000 |  +0000 |  ++000 | +--000 | +-0000 | +-+000 | +0-000 | +00000 | +0+000 | ++-000 | ++0000 |

```


[[Category:Arbitrary precision]]
