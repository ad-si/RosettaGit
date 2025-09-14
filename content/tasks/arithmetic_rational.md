+++
title = "Arithmetic/Rational"
description = ""
date = 2019-09-20T08:36:30Z
aliases = []
[extra]
id = 3370
[taxonomies]
categories = ["Arithmetic operations", "Arithmetic", "task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "bbc_basic",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "elisa",
  "elixir",
  "erre",
  "factor",
  "forth",
  "fortran",
  "frink",
  "gap",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "liberty_basic",
  "lingo",
  "lua",
  "m2000_interpreter",
  "maple",
  "maxima",
  "nim",
  "ocaml",
  "oorexx",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "python",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "slate",
  "smalltalk",
  "tcl",
  "zkl",
]
+++

## Task

;Task:
Create a reasonably complete implementation of rational arithmetic in the particular language using the idioms of the language.


;Example:
Define a new type called '''frac''' with binary operator "//" of two integers that returns a '''structure''' made up of the numerator and the denominator (as per a rational number).

Further define the appropriate rational unary '''operators''' '''abs''' and '-', with the binary '''operators''' for addition '+', subtraction '-', multiplication '&times;', division '/', integer division '&divide;', modulo division, the comparison operators (e.g. '<', '&le;', '>', & '&ge;') and equality operators (e.g. '=' & '&ne;').

Define standard coercion '''operators''' for casting '''int''' to '''frac''' etc.

If space allows, define standard increment and decrement '''operators''' (e.g. '+:=' & '-:=' etc.).

Finally test the operators:
Use the new type '''frac''' to find all [[Perfect Numbers|perfect numbers]] less than 2<sup>19</sup> by summing the reciprocal of the factors.


;Related task:
*   [[Perfect Numbers]]





## Ada

<div style="text-align:right;font-size:7pt">''<nowiki>[</nowiki>This section is included from [[Arithmetic/Rational/Ada|a subpage]] and should be edited there, not here.<nowiki>]</nowiki>''</div>
{{:Arithmetic/Rational/Ada}}


## ALGOL 68

{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}

```algol68
 MODE FRAC = STRUCT( INT num #erator#,  den #ominator#);
 FORMAT frac repr = $g(-0)"//"g(-0)$;

 PROC gcd = (INT a, b) INT: # greatest common divisor #
   (a = 0 | b |: b = 0 | a |: ABS a > ABS b  | gcd(b, a MOD b) | gcd(a, b MOD a));

 PROC lcm = (INT a, b)INT: # least common multiple #
   a OVER gcd(a, b) * b;

 PROC raise not implemented error = ([]STRING args)VOID: (
   put(stand error, ("Not implemented error: ",args, newline));
   stop
 );

 PRIO // = 9; # higher then the ** operator #
 OP // = (INT num, den)FRAC: ( # initialise and normalise #
   INT common = gcd(num, den);
   IF den < 0 THEN
     ( -num OVER common, -den OVER common)
   ELSE
     ( num OVER common, den OVER common)
   FI
 );

 OP + = (FRAC a, b)FRAC: (
   INT common = lcm(den OF a, den OF b);
   FRAC result := ( common OVER den OF a * num OF a + common OVER den OF b * num OF b, common );
   num OF result//den OF result
 );

 OP - = (FRAC a, b)FRAC: a + -b,
    * = (FRAC a, b)FRAC: (
   INT num = num OF a * num OF b,
       den = den OF a * den OF b;
   INT common = gcd(num, den);
   (num OVER common) // (den OVER common)
 );

 OP /  = (FRAC a, b)FRAC: a * FRAC(den OF b, num OF b),# real division #
    %  = (FRAC a, b)INT: ENTIER (a / b),               # integer divison #
    %* = (FRAC a, b)FRAC: a/b - FRACINIT ENTIER (a/b), # modulo division #
    ** = (FRAC a, INT exponent)FRAC:
     IF exponent >= 0 THEN
       (num OF a ** exponent, den OF a ** exponent )
     ELSE
       (den OF a ** exponent, num OF a ** exponent )
     FI;

 OP REALINIT = (FRAC frac)REAL: num OF frac / den OF frac,
    FRACINIT = (INT num)FRAC: num // 1,
    FRACINIT = (REAL num)FRAC: (
      # express real number as a fraction # # a future execise! #
      raise not implemented error(("Convert a REAL to a FRAC","!"));
      SKIP
    );

 OP <  = (FRAC a, b)BOOL: num OF (a - b) <  0,
    >  = (FRAC a, b)BOOL: num OF (a - b) >  0,
    <= = (FRAC a, b)BOOL: NOT ( a > b ),
    >= = (FRAC a, b)BOOL: NOT ( a < b ),
    =  = (FRAC a, b)BOOL: (num OF a, den OF a) = (num OF b, den OF b),
    /= = (FRAC a, b)BOOL: (num OF a, den OF a) /= (num OF b, den OF b);

 # Unary operators #
 OP - = (FRAC frac)FRAC: (-num OF frac, den OF frac),
    ABS = (FRAC frac)FRAC: (ABS num OF frac, ABS den OF frac),
    ENTIER = (FRAC frac)INT: (num OF frac OVER den OF frac) * den OF frac;

 COMMENT Operators for extended characters set, and increment/decrement:
 OP +:= = (REF FRAC a, FRAC b)REF FRAC: ( a := a + b ),
    +=: = (FRAC a, REF FRAC b)REF FRAC: ( b := a + b ),
    -:= = (REF FRAC a, FRAC b)REF FRAC: ( a := a - b ),
    *:= = (REF FRAC a, FRAC b)REF FRAC: ( a := a * b ),
    /:= = (REF FRAC a, FRAC b)REF FRAC: ( a := a / b ),
    %:= = (REF FRAC a, FRAC b)REF FRAC: ( a := FRACINIT (a % b) ),
    %*:= = (REF FRAC a, FRAC b)REF FRAC: ( a := a %* b );

 # OP aliases for extended character sets (eg: Unicode, APL, ALCOR and GOST 10859) #
 OP ×  = (FRAC a, b)FRAC: a * b,
    ÷  = (FRAC a, b)INT: a OVER b,
    ÷× = (FRAC a, b)FRAC: a MOD b,
    ÷* = (FRAC a, b)FRAC: a MOD b,
    %× = (FRAC a, b)FRAC: a MOD b,
    ≤  = (FRAC a, b)FRAC: a <= b,
    ≥  = (FRAC a, b)FRAC: a >= b,
    ≠  = (FRAC a, b)BOOL: a /= b,
    ↑  = (FRAC frac, INT exponent)FRAC: frac ** exponent,

    ÷×:= = (REF FRAC a, FRAC b)REF FRAC: ( a := a MOD b ),
    %×:= = (REF FRAC a, FRAC b)REF FRAC: ( a := a MOD b ),
    ÷*:= = (REF FRAC a, FRAC b)REF FRAC: ( a := a MOD b );

 # BOLD aliases for CPU that only support uppercase for 6-bit bytes  - wrist watches #
 OP OVER = (FRAC a, b)INT: a % b,
    MOD = (FRAC a, b)FRAC: a %*b,
    LT = (FRAC a, b)BOOL: a <  b,
    GT = (FRAC a, b)BOOL: a >  b,
    LE = (FRAC a, b)BOOL: a <= b,
    GE = (FRAC a, b)BOOL: a >= b,
    EQ = (FRAC a, b)BOOL: a =  b,
    NE = (FRAC a, b)BOOL: a /= b,
    UP = (FRAC frac, INT exponent)FRAC: frac**exponent;

 # the required standard assignment operators #
 OP PLUSAB  = (REF FRAC a, FRAC b)REF FRAC: ( a +:= b ), # PLUS #
    PLUSTO  = (FRAC a, REF FRAC b)REF FRAC: ( a +=: b ), # PRUS #
    MINUSAB = (REF FRAC a, FRAC b)REF FRAC: ( a *:= b ),
    DIVAB   = (REF FRAC a, FRAC b)REF FRAC: ( a /:= b ),
    OVERAB  = (REF FRAC a, FRAC b)REF FRAC: ( a %:= b ),
    MODAB   = (REF FRAC a, FRAC b)REF FRAC: ( a %*:= b );

END COMMENT
Example: searching for Perfect Numbers.
 FRAC sum:= FRACINIT 0;
 FORMAT perfect = $b(" perfect!","")$;

 FOR i FROM 2 TO 2**19 DO
   INT candidate := i;
   FRAC sum := 1 // candidate;
   REAL real sum := 1 / candidate;
   FOR factor FROM 2 TO ENTIER sqrt(candidate) DO
     IF candidate MOD factor = 0 THEN
       sum :=  sum + 1 // factor + 1 // ( candidate OVER factor);
       real sum +:= 1 / factor + 1 / ( candidate OVER factor)
     FI
   OD;
   IF den OF sum  = 1 THEN
     printf(($"Sum of reciprocal factors of "g(-0)" = "g(-0)" exactly, about "g(0,real width) f(perfect)l$,
             candidate, ENTIER sum, real sum, ENTIER sum = 1))
   FI
 OD
```

{{out}}

```txt

Sum of reciprocal factors of 6 = 1 exactly, about 1.0000000000000000000000000001 perfect!
Sum of reciprocal factors of 28 = 1 exactly, about 1.0000000000000000000000000001 perfect!
Sum of reciprocal factors of 120 = 2 exactly, about 2.0000000000000000000000000002
Sum of reciprocal factors of 496 = 1 exactly, about 1.0000000000000000000000000001 perfect!
Sum of reciprocal factors of 672 = 2 exactly, about 2.0000000000000000000000000001
Sum of reciprocal factors of 8128 = 1 exactly, about 1.0000000000000000000000000001 perfect!
Sum of reciprocal factors of 30240 = 3 exactly, about 3.0000000000000000000000000002
Sum of reciprocal factors of 32760 = 3 exactly, about 3.0000000000000000000000000003
Sum of reciprocal factors of 523776 = 2 exactly, about 2.0000000000000000000000000005

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      *FLOAT64
      DIM frac{num, den}
      DIM Sum{} = frac{}, Kf{} = frac{}, One{} = frac{}
      One.num = 1 : One.den = 1

      FOR n% = 2 TO 2^19-1
        Sum.num = 1 : Sum.den = n%
        FOR k% = 2 TO SQR(n%)
          IF (n% MOD k%) = 0 THEN
            Kf.num = 1 : Kf.den = k%
            PROCadd(Sum{}, Kf{})
            PROCnormalise(Sum{})
            Kf.den = n% DIV k%
            PROCadd(Sum{}, Kf{})
            PROCnormalise(Sum{})
          ENDIF
        NEXT
        IF FNeq(Sum{}, One{}) PRINT n% " is perfect"
      NEXT n%
      END

      DEF PROCabs(a{}) : a.num = ABS(a.num) : ENDPROC
      DEF PROCneg(a{}) : a.num = -a.num : ENDPROC

      DEF PROCadd(a{}, b{})
      LOCAL t : t = a.den * b.den
      a.num = a.num * b.den + b.num * a.den
      a.den = t
      ENDPROC

      DEF PROCsub(a{}, b{})
      LOCAL t : t = a.den * b.den
      a.num = a.num * b.den - b.num * a.den
      a.den = t
      ENDPROC

      DEF PROCmul(a{}, b{})
      a.num *= b.num : a.den *= b.den
      ENDPROC

      DEF PROCdiv(a{}, b{})
      a.num *= b.den : a.den *= b.num
      ENDPROC

      DEF FNeq(a{}, b{}) = a.num * b.den = b.num * a.den
      DEF FNlt(a{}, b{}) = a.num * b.den < b.num * a.den
      DEF FNgt(a{}, b{}) = a.num * b.den > b.num * a.den
      DEF FNne(a{}, b{}) = a.num * b.den <> b.num * a.den
      DEF FNle(a{}, b{}) = a.num * b.den <= b.num * a.den
      DEF FNge(a{}, b{}) = a.num * b.den >= b.num * a.den

      DEF PROCnormalise(a{})
      LOCAL a, b, t
      a = a.num : b = a.den
      WHILE b <> 0
        t = a
        a = b
        b = t - b * INT(t / b)
      ENDWHILE
      a.num /= a : a.den /= a
      IF a.den < 0 a.num *= -1 : a.den *= -1
      ENDPROC
```

Output:

```txt

         6 is perfect
        28 is perfect
       496 is perfect
      8128 is perfect

```



## C

C does not have overloadable operators. The following implementation <u>''does not define all operations''</u> so as to keep the example short. Note that the code passes around struct values instead of pointers to keep it simple, a practice normally avoided for efficiency reasons.

```c
#include <stdio.h>
#include <stdlib.h>
#define FMT "%lld"
typedef long long int fr_int_t;
typedef struct { fr_int_t num, den; } frac;

fr_int_t gcd(fr_int_t m, fr_int_t n)
{
	fr_int_t t;
	while (n) { t = n; n = m % n; m = t; }
	return m;
}

frac frac_new(fr_int_t num, fr_int_t den)
{
	frac a;
	if (!den) {
		printf("divide by zero: "FMT"/"FMT"\n", num, den);
		abort();
	}

	int g = gcd(num, den);

	if (g)	{ num /= g; den /= g; }
	else	{ num = 0; den = 1;   }

	if (den < 0) {
		den = -den;
		num = -num;
	}
	a.num = num; a.den = den;
	return a;
}

#define BINOP(op, n, d) frac frac_##op(frac a, frac b) { return frac_new(n,d); }
BINOP(add, a.num * b.den + b.num * a.den, a.den * b.den);
BINOP(sub, a.num * b.den - b.num + a.den, a.den * b.den);
BINOP(mul, a.num * b.num, a.den * b.den);
BINOP(div, a.num * b.den, a.den * b.num);

int frac_cmp(frac a, frac b) {
	int l = a.num * b.den, r = a.den * b.num;
	return l < r ? -1 : l > r;
}
#define frac_cmp_int(a, b) frac_cmp(a, frac_new(b, 1))
int frtoi(frac a) { return a.den / a.num; }
double frtod(frac a) { return (double)a.den / a.num; }

int main()
{
	int n, k;
	frac sum, kf;

	for (n = 2; n < 1<<19; n++) {
		sum = frac_new(1, n);

		for (k = 2; k * k < n; k++) {
			if (n % k) continue;
			kf = frac_new(1, k);
			sum = frac_add(sum, kf);

			kf = frac_new(1, n / k);
			sum = frac_add(sum, kf);
		}
		if (frac_cmp_int(sum, 1) == 0) printf("%d\n", n);
	}

	return 0;
}
```

See [[Rational Arithmetic/C]]


## C#

<div style="text-align:right;font-size:7pt">''<nowiki>[</nowiki>This section is included from [[Arithmetic/Rational/C sharp|a subpage]] and should be edited there, not here.<nowiki>]</nowiki>''</div>
{{:Arithmetic/Rational/C sharp}}


## C++

{{libheader|Boost}}
Boost provides a rational number template.

```cpp
#include <iostream>
#include "math.h"
#include "boost/rational.hpp"

typedef  boost::rational<int> frac;

bool is_perfect(int c)
{
    frac sum(1, c);
    for (int f = 2;f < sqrt(static_cast<float>(c)); ++f){

        if (c % f == 0) sum += frac(1,f) + frac(1, c/f);
    }
    if (sum.denominator() == 1){
 	return (sum == 1);
    }
    return false;
}

int main()
{
    for (int candidate = 2; candidate < 0x80000; ++candidate){
        if (is_perfect(candidate))
	        std::cout << candidate << " is perfect" << std::endl;
    }
    return 0;
}
```



## Clojure

Ratios are built in to Clojure and support math operations already.  They automatically reduce and become Integers if possible.

```Clojure>user
 22/7
22/7
user> 34/2
17
user> (+ 37/5 42/9)
181/15
```



## Common Lisp

Common Lisp has rational numbers built-in and integrated with all other number types. Common Lisp's number system is not extensible so reimplementing rational arithmetic would require all-new operator names.

```lisp
(loop for candidate from 2 below (expt 2 19)
      for sum = (+ (/ candidate)
                   (loop for factor from 2 to (isqrt candidate)
                         when (zerop (mod candidate factor))
                           sum (+ (/ factor) (/ (floor candidate factor)))))
      when (= sum 1)
        collect candidate)
```



## D


```d
import std.bigint, std.traits, std.conv;

// std.numeric.gcd doesn't work with BigInt.
T gcd(T)(in T a, in T b) pure nothrow {
    return (b != 0) ? gcd(b, a % b) : (a < 0) ? -a : a;
}

T lcm(T)(in T a, in T b) pure nothrow {
    return a / gcd(a, b) * b;
}

struct RationalT(T) if (!isUnsigned!T) {
    private T num, den; // Numerator & denominator.

    private enum Type { NegINF = -2,
                        NegDEN = -1,
                        NaRAT  =  0,
                        NORMAL =  1,
                        PosINF =  2 };

    this(U : RationalT)(U n) pure nothrow {
        num = n.num;
        den = n.den;
    }

    this(U)(in U n) pure nothrow if (isIntegral!U) {
        num = toT(n);
        den = 1UL;
    }

    this(U, V)(in U n, in V d) pure nothrow {
        num = toT(n);
        den = toT(d);
        const common = gcd(num, den);
        if (common != 0) {
            num /= common;
            den /= common;
        } else { // infinite or NOT a Number
            num = (num == 0) ? 0 : (num < 0) ? -1 : 1;
            den = 0;
        }
        if (den < 0) { // Assure den is non-negative.
            num = -num;
            den = -den;
        }
    }

    static T toT(U)(in ref U n) pure nothrow if (is(U == T)) {
        return n;
    }

    static T toT(U)(in ref U n) pure nothrow if (!is(U == T)) {
        T result = n;
        return result;
    }

    T numerator() const pure nothrow @property {
        return num;
    }

    T denominator() const pure nothrow @property {
        return den;
    }

    string toString() const /*pure nothrow*/ {
        if (den != 0)
            return num.text ~ (den == 1 ? "" : "/" ~ den.text);
        if (num == 0)
            return "NaRat";
        else
            return ((num < 0) ? "-" : "+") ~ "infRat";
    }

    real toReal() pure const nothrow {
        static if (is(T == BigInt))
            return num.toLong / real(den.toLong);
        else
            return num / real(den);
    }

    RationalT opBinary(string op)(in RationalT r)
    const pure nothrow if (op == "+" || op == "-") {
        T common = lcm(den, r.den);
        T n = mixin("common / den * num" ~ op ~
                    "common / r.den * r.num" );
        return RationalT(n, common);
    }

    RationalT opBinary(string op)(in RationalT r)
    const pure nothrow if (op == "*") {
        return RationalT(num * r.num, den * r.den);
    }

    RationalT opBinary(string op)(in RationalT r)
    const pure nothrow if (op == "/") {
        return RationalT(num * r.den, den * r.num);
    }

    RationalT opBinary(string op, U)(in U r)
    const pure nothrow if (isIntegral!U && (op == "+" ||
                           op == "-" || op == "*" || op == "/")) {
        return opBinary!op(RationalT(r));
    }

    RationalT opBinary(string op)(in size_t p)
    const pure nothrow if (op == "^^") {
        return RationalT(num ^^ p, den ^^ p);
    }

    RationalT opBinaryRight(string op, U)(in U l)
    const pure nothrow if (isIntegral!U) {
        return RationalT(l).opBinary!op(RationalT(num, den));
    }

    RationalT opOpAssign(string op, U)(in U l) pure /*nothrow*/ {
        mixin("this = this " ~ op ~ "l;");
        return this;
    }

    RationalT opUnary(string op)()
    const pure nothrow if (op == "+" || op == "-") {
        return RationalT(mixin(op ~ "num"), den);
    }

    bool opCast(U)() const if (is(U == bool)) {
        return num != 0;
    }

    bool opEquals(U)(in U r) const pure nothrow {
        RationalT rhs = RationalT(r);
        if (type() == Type.NaRAT || rhs.type() == Type.NaRAT)
            return false;
        return num == rhs.num && den == rhs.den;
    }

    int opCmp(U)(in U r) const pure nothrow {
        auto rhs = RationalT(r);
        if (type() == Type.NaRAT || rhs.type() == Type.NaRAT)
            throw new Error("Compare involve a NaRAT.");
        if (type() != Type.NORMAL ||
            rhs.type() != Type.NORMAL) // for infinite
            return (type() == rhs.type()) ? 0 :
                ((type() < rhs.type()) ? -1 : 1);
        auto diff = num * rhs.den - den * rhs.num;
        return (diff == 0) ? 0 : ((diff < 0) ? -1 : 1);
    }

    Type type() const pure nothrow {
        if (den > 0) return Type.NORMAL;
        if (den < 0) return Type.NegDEN;
        if (num > 0) return Type.PosINF;
        if (num < 0) return Type.NegINF;
        return Type.NaRAT;
    }
}

RationalT!U rational(U)(in U n) pure nothrow {
    return typeof(return)(n);
}

RationalT!(CommonType!(U1, U2))
rational(U1, U2)(in U1 n, in U2 d) pure nothrow {
    return typeof(return)(n, d);
}

alias Rational = RationalT!BigInt;

version (arithmetic_rational_main) { // Test.
    void main() {
        import std.stdio, std.math;
        alias RatL = RationalT!long;

        foreach (immutable p; 2 .. 2 ^^ 19) {
            auto sum = RatL(1, p);
            immutable limit = 1 + cast(uint)real(p).sqrt;
            foreach (immutable factor; 2 .. limit)
                if (p % factor == 0)
                    sum += RatL(1, factor) + RatL(factor, p);
            if (sum.denominator == 1)
                writefln("Sum of recipr. factors of %6s = %s exactly%s",
                         p, sum, (sum == 1) ? ", perfect." : ".");
        }
    }
}
```

Use the <code>-version=rational_arithmetic_main</code> compiler switch to run the test code.
{{out}}

```txt
Sum of recipr. factors of      6 = 1 exactly, perfect.
Sum of recipr. factors of     28 = 1 exactly, perfect.
Sum of recipr. factors of    120 = 2 exactly.
Sum of recipr. factors of    496 = 1 exactly, perfect.
Sum of recipr. factors of    672 = 2 exactly.
Sum of recipr. factors of   8128 = 1 exactly, perfect.
Sum of recipr. factors of  30240 = 3 exactly.
Sum of recipr. factors of  32760 = 3 exactly.
Sum of recipr. factors of 523776 = 2 exactly.
```

Currently RationalT!BigInt is not fast.


## EchoLisp

EchoLisp supports rational numbers as native type. "Big" rational i.e bigint/bigint are not supported.

```lisp

;; Finding perfect numbers
(define (sum/inv n) ;; look for div's in [2..sqrt(n)] and add 1/n
	(for/fold (acc (/ n)) [(i (in-range 2 (sqrt n)))]
		#:break (> acc 1) ; no hope
		(when (zero? (modulo n i ))
			(set! acc (+ acc (/ i) (/ i n))))))

```

{{out}}

```lisp

;; rational operations
(+ 1/42 1/666) → 59/2331
42/666 → 7/111
(expt 3/4 7) → 2187/16384 ; 3/4 ^7
(/ 6 8) → 3/4   ;; / operator → rational
(// 6 8) → 0.75 ;; // operator → float
(* 6/7 14/12) → 1

;; even perfect numbers (up to 100000)
(for [(i (in-range 4 100000 2))] ;; 8 seconds
	(when (= (sum/inv i) 1)
		(printf "🍏 🍒 🍓 %d is perfect." i)))

🍏 🍒 🍓 6 is perfect.
🍏 🍒 🍓 28 is perfect.
🍏 🍒 🍓 496 is perfect.
🍏 🍒 🍓 8128 is perfect.

```



## Elisa


```Elisa
component RationalNumbers;
  type Rational;
       Rational(Numerator = integer, Denominater = integer) -> Rational;

       Rational + Rational -> Rational;
       Rational - Rational -> Rational;
       Rational * Rational -> Rational;
       Rational / Rational -> Rational;

       Rational == Rational -> boolean;
       Rational <> Rational -> boolean;
       Rational >= Rational -> boolean;
       Rational <= Rational -> boolean;
       Rational >  Rational -> boolean;
       Rational <  Rational -> boolean;

       + Rational -> Rational;
       - Rational -> Rational;
       abs(Rational) -> Rational;

       Rational(integer) -> Rational;
       Numerator(Rational) -> integer;
       Denominator(Rational) -> integer;
  begin
       Rational(A,B) = Rational:[A;B];

       R1 + R2 = Normalize( R1.A * R2.B + R1.B * R2.A, R1.B * R2.B);
       R1 - R2 = Normalize( R1.A * R2.B - R1.B * R2.A, R1.B * R2.B);
       R1 * R2 = Normalize( R1.A * R2.A, R1.B * R2.B);
       R1 / R2 = Normalize( R1.A * R2.B, R1.B * R2.A);

       R1 == R2 = [ R = (R1 - R2); R.A == 0];
       R1 <> R2 = [ R = (R1 - R2); R.A <> 0];
       R1 >= R2 = [ R = (R1 - R2); R.A >= 0];
       R1 <= R2 = [ R = (R1 - R2); R.A <= 0];
       R1 > R2  = [ R = (R1 - R2); R.A > 0];
       R1 < R2  = [ R = (R1 - R2); R.A < 0];

       + R = R;
       - R = Rational(-R.A, R.B);

       abs(R) = Rational(abs(R.A), abs(R.B));
       Rational(I) = Rational (I, 1);
       Numerator(R) = R.A;
       Denominator(R) = R.B;

				<< internal definitions >>

       Normalize (A = integer, B = integer) -> Rational;
       Normalize (A, B) = [ exception( B == 0, "Illegal Rational Number");
	                    Common = GCD(abs(A), abs(B));
			    if B < 0 then Rational(-A / Common, -B / Common)
			             else Rational( A / Common,  B / Common) ];

       GCD (A = integer, B = integer) -> integer;
       GCD (A, B) = [ if A == 0 then return(B);
	              if B == 0 then return(A);
		      if A > B  then GCD (B, mod(A,B))
 		                else GCD (A, mod(B,A)) ];

end component RationalNumbers;
```

Tests

```Elisa
use RationalNumbers;

PerfectNumbers( Limit = integer) -> multi(integer);
PerfectNumbers( Limit) =
  	      [ Candidate = 2 .. Limit;
		Sum:= Rational(1,Candidate);
		[ Divisor = 2 .. integer(sqrt(real(Candidate)));
		  if mod(Candidate, Divisor) == 0 then
			Sum := Sum + Rational(1, Divisor) + Rational(Divisor, Candidate);
		];
		if Sum == Rational(1,1) then Candidate
              ];

PerfectNumbers(10000)?
```

{{out}}

```txt

6
28
496
8128

```



## Elixir


```elixir
defmodule Rational do
  import Kernel, except: [div: 2]

  defstruct numerator: 0, denominator: 1

  def new(numerator), do: %Rational{numerator: numerator, denominator: 1}

  def new(numerator, denominator) do
    sign = if numerator * denominator < 0, do: -1, else: 1
    {numerator, denominator} = {abs(numerator), abs(denominator)}
    gcd = gcd(numerator, denominator)
    %Rational{numerator: sign * Kernel.div(numerator, gcd),
              denominator: Kernel.div(denominator, gcd)}
  end

  def add(a, b) do
    {a, b} = convert(a, b)
    new(a.numerator * b.denominator + b.numerator * a.denominator,
        a.denominator * b.denominator)
  end

  def sub(a, b) do
    {a, b} = convert(a, b)
    new(a.numerator * b.denominator - b.numerator * a.denominator,
        a.denominator * b.denominator)
  end

  def mult(a, b) do
    {a, b} = convert(a, b)
    new(a.numerator * b.numerator, a.denominator * b.denominator)
  end

  def div(a, b) do
    {a, b} = convert(a, b)
    new(a.numerator * b.denominator, a.denominator * b.numerator)
  end

  defp convert(a), do: if is_integer(a), do: new(a), else: a

  defp convert(a, b), do: {convert(a), convert(b)}

  defp gcd(a, 0), do: a
  defp gcd(a, b), do: gcd(b, rem(a, b))
end

defimpl Inspect, for: Rational do
  def inspect(r, _opts) do
    "%Rational<#{r.numerator}/#{r.denominator}>"
  end
end

Enum.each(2..trunc(:math.pow(2,19)), fn candidate ->
  sum = 2 .. round(:math.sqrt(candidate))
        |> Enum.reduce(Rational.new(1, candidate), fn factor,sum ->
             if rem(candidate, factor) == 0 do
               Rational.add(sum, Rational.new(1, factor))
               |> Rational.add(Rational.new(1, div(candidate, factor)))
             else
               sum
             end
           end)
  if sum.denominator == 1 do
    :io.format "Sum of recipr. factors of ~6w = ~w exactly ~s~n",
           [candidate, sum.numerator, (if sum.numerator == 1, do: "perfect!", else: "")]
  end
end)
```


{{out}}

```txt

Sum of recipr. factors of      6 = 1 exactly perfect!
Sum of recipr. factors of     28 = 1 exactly perfect!
Sum of recipr. factors of    120 = 2 exactly
Sum of recipr. factors of    496 = 1 exactly perfect!
Sum of recipr. factors of    672 = 2 exactly
Sum of recipr. factors of   8128 = 1 exactly perfect!
Sum of recipr. factors of  30240 = 3 exactly
Sum of recipr. factors of  32760 = 3 exactly
Sum of recipr. factors of 523776 = 2 exactly

```



## ERRE


```ERRE
PROGRAM RATIONAL_ARITH

!
! for rosettacode.org
!

TYPE RATIONAL=(NUM,DEN)

DIM SUM:RATIONAL,ONE:RATIONAL,KF:RATIONAL

DIM A:RATIONAL,B:RATIONAL
PROCEDURE ABS(A.->A.)
      A.NUM=ABS(A.NUM)
END PROCEDURE

PROCEDURE NEG(A.->A.)
      A.NUM=-A.NUM
END PROCEDURE

PROCEDURE ADD(A.,B.->A.)
      LOCAL T
      T=A.DEN*B.DEN
      A.NUM=A.NUM*B.DEN+B.NUM*A.DEN
      A.DEN=T
END PROCEDURE

PROCEDURE SUB(A.,B.->A.)
      LOCAL T
      T=A.DEN*B.DEN
      A.NUM=A.NUM*B.DEN-B.NUM*A.DEN
      A.DEN=T
END PROCEDURE

PROCEDURE MULT(A.,B.->A.)
      A.NUM*=B.NUM  A.DEN*=B.DEN
END PROCEDURE

PROCEDURE DIVIDE(A.,B.->A.)
      A.NUM*=B.DEN
      A.DEN*=B.NUM
END PROCEDURE

PROCEDURE EQ(A.,B.->RES%)
      RES%=A.NUM*B.DEN=B.NUM*A.DEN
END PROCEDURE

PROCEDURE LT(A.,B.->RES%)
      RES%=A.NUM*B.DEN<B.NUM*A.DEN
END PROCEDURE

PROCEDURE GT(A.,B.->RES%)
      RES%=A.NUM*B.DEN>B.NUM*A.DEN
END PROCEDURE

PROCEDURE NE(A.,B.->RES%)
      RES%=A.NUM*B.DEN<>B.NUM*A.DEN
END PROCEDURE

PROCEDURE LE(A.,B.->RES%)
      RES%=A.NUM*B.DEN<=B.NUM*A.DEN
END PROCEDURE

PROCEDURE GE(A.,B.->RES%)
      RES%=A.NUM*B.DEN>=B.NUM*A.DEN
END PROCEDURE

PROCEDURE NORMALIZE(A.->A.)
      LOCAL A,B,T
      A=A.NUM   B=A.DEN
      WHILE B<>0 DO
        T=A
        A=B
        B=T-B*INT(T/B)
      END WHILE
      A.NUM/=A  A.DEN/=A
      IF A.DEN<0 THEN A.NUM*=-1 A.DEN*=-1 END IF
END PROCEDURE

BEGIN
    ONE.NUM=1 ONE.DEN=1
    FOR N=2 TO 2^19-1 DO
      SUM.NUM=1 SUM.DEN=N
      FOR K=2 TO SQR(N) DO
        IF N=K*INT(N/K) THEN
          KF.NUM=1 KF.DEN=K
          ADD(SUM.,KF.->SUM.)
          NORMALIZE(SUM.->SUM.)
          KF.DEN=INT(N/K)
          ADD(SUM.,KF.->SUM.)
          NORMALIZE(SUM.->SUM.)
        END IF
      END FOR
      EQ(SUM.,ONE.->RES%)
      IF RES% THEN PRINT(N;" is perfect") END IF
   END FOR
END PROGRAM
```

{{out}}

```txt
 6  is perfect
 28  is perfect
 496  is perfect
 8128  is perfect

```


=={{header|F_Sharp|F#}}==
The F# Powerpack library defines the BigRational data type.

```fsharp
type frac = Microsoft.FSharp.Math.BigRational

let perf n = 1N = List.fold (+) 0N (List.map (fun i -> if n % i = 0 then 1N/frac.FromInt(i) else 0N) [2..n])

for i in 1..(1<<<19) do if (perf i) then printfn "%i is perfect" i
```



## Factor

<code>ratio</code> is a built-in numeric type.

```factor
USING: generalizations io kernel math math.functions
math.primes.factors math.ranges prettyprint sequences ;
IN: rosetta-code.arithmetic-rational

2/5              ! literal syntax 2/5
2/4              ! automatically simplifies to 1/2
5/1              ! automatically coerced to 5
26/5             ! mixed fraction 5+1/5
13/178 >fraction ! get the numerator and denominator 13 178
8 recip          ! get the reciprocal 1/8

! ratios can be any size
12417829731289312/61237812937138912735712
8 ndrop ! clear the stack
! arithmetic works the same as any other number.

: perfect? ( n -- ? )
    divisors rest [ recip ] map-sum 1 = ;

"Perfect numbers <= 2^19: " print
2 19 ^ [1,b] [ perfect? ] filter .
```

{{out}}

```txt

Perfect numbers <= 2^19:
V{ 6 28 496 8128 }

```



## Forth


```forth
\ Rationals can use any double cell operations:  2!, 2@, 2dup, 2swap, etc.
\ Uses the stack convention of the built-in "*/" for int * frac -> int

: numerator  drop ;
: denominator nip ;

: s>rat      1 ;		\ integer to rational  (n/1)
: rat>s      / ;		\ integer
: rat>frac   mod ;		\ fractional part
: rat>float  swap s>f s>f f/ ;

: rat.  swap 1 .r [char] / emit . ;

\ normalize: factors out gcd and puts sign into numerator
: gcd ( a b -- gcd ) begin ?dup while tuck mod repeat ;
: rat-normalize ( rat -- rat ) 2dup gcd tuck / >r / r> ;

: rat-abs     swap abs    swap ;
: rat-negate  swap negate swap ;
: 1/rat       over 0< if negate swap negate else swap then ;

: rat+ ( a b c d -- ad+bc bd )
  rot 2dup * >r
   rot * >r * r> +
  r> rat-normalize ;
: rat-  rat-negate rat+ ;

: rat* ( a b c d -- ac bd )
  rot * >r * r> rat-normalize ;
: rat/  swap rat* ;

: rat-equal  d= ;
: rat-less ( a b c d -- ad<bc )
  -rot * >r * r> < ;
: rat-more  2swap rat-less ;

: rat-inc  tuck + swap ;
: rat-dec  tuck - swap ;
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
module module_rational

  implicit none
  private
  public :: rational
  public :: rational_simplify
  public :: assignment (=)
  public :: operator (//)
  public :: operator (+)
  public :: operator (-)
  public :: operator (*)
  public :: operator (/)
  public :: operator (<)
  public :: operator (<=)
  public :: operator (>)
  public :: operator (>=)
  public :: operator (==)
  public :: operator (/=)
  public :: abs
  public :: int
  public :: modulo
  type rational
    integer :: numerator
    integer :: denominator
  end type rational
  interface assignment (=)
    module procedure assign_rational_int, assign_rational_real
  end interface
  interface operator (//)
    module procedure make_rational
  end interface
  interface operator (+)
    module procedure rational_add
  end interface
  interface operator (-)
    module procedure rational_minus, rational_subtract
  end interface
  interface operator (*)
    module procedure rational_multiply
  end interface
  interface operator (/)
    module procedure rational_divide
  end interface
  interface operator (<)
    module procedure rational_lt
  end interface
  interface operator (<=)
    module procedure rational_le
  end interface
  interface operator (>)
    module procedure rational_gt
  end interface
  interface operator (>=)
    module procedure rational_ge
  end interface
  interface operator (==)
    module procedure rational_eq
  end interface
  interface operator (/=)
    module procedure rational_ne
  end interface
  interface abs
    module procedure rational_abs
  end interface
  interface int
    module procedure rational_int
  end interface
  interface modulo
    module procedure rational_modulo
  end interface

contains

  recursive function gcd (i, j) result (res)
    integer, intent (in) :: i
    integer, intent (in) :: j
    integer :: res
    if (j == 0) then
      res = i
    else
      res = gcd (j, modulo (i, j))
    end if
  end function gcd

  function rational_simplify (r) result (res)
    type (rational), intent (in) :: r
    type (rational) :: res
    integer :: g
    g = gcd (r % numerator, r % denominator)
    res = r % numerator / g // r % denominator / g
  end function rational_simplify

  function make_rational (numerator, denominator) result (res)
    integer, intent (in) :: numerator
    integer, intent (in) :: denominator
    type (rational) :: res
    res = rational (numerator, denominator)
  end function make_rational

  subroutine assign_rational_int (res, i)
    type (rational), intent (out), volatile :: res
    integer, intent (in) :: i
    res = i // 1
  end subroutine assign_rational_int

  subroutine assign_rational_real (res, x)
    type (rational), intent(out), volatile :: res
    real, intent (in) :: x
    integer :: x_floor
    real :: x_frac
    x_floor = floor (x)
    x_frac = x - x_floor
    if (x_frac == 0) then
      res = x_floor // 1
    else
      res = (x_floor // 1) + (1 // floor (1 / x_frac))
    end if
  end subroutine assign_rational_real

  function rational_add (r, s) result (res)
    type (rational), intent (in) :: r
    type (rational), intent (in) :: s
    type (rational) :: res
    res = r % numerator * s % denominator + r % denominator * s % numerator // &
        & r % denominator * s % denominator
  end function rational_add

  function rational_minus (r) result (res)
    type (rational), intent (in) :: r
    type (rational) :: res
    res = - r % numerator // r % denominator
  end function rational_minus

  function rational_subtract (r, s) result (res)
    type (rational), intent (in) :: r
    type (rational), intent (in) :: s
    type (rational) :: res
    res = r % numerator * s % denominator - r % denominator * s % numerator // &
        & r % denominator * s % denominator
  end function rational_subtract

  function rational_multiply (r, s) result (res)
    type (rational), intent (in) :: r
    type (rational), intent (in) :: s
    type (rational) :: res
    res = r % numerator * s % numerator // r % denominator * s % denominator
  end function rational_multiply

  function rational_divide (r, s) result (res)
    type (rational), intent (in) :: r
    type (rational), intent (in) :: s
    type (rational) :: res
    res = r % numerator * s % denominator // r % denominator * s % numerator
  end function rational_divide

  function rational_lt (r, s) result (res)
    type (rational), intent (in) :: r
    type (rational), intent (in) :: s
    type (rational) :: r_simple
    type (rational) :: s_simple
    logical :: res
    r_simple = rational_simplify (r)
    s_simple = rational_simplify (s)
    res = r_simple % numerator * s_simple % denominator < &
        & s_simple % numerator * r_simple % denominator
  end function rational_lt

  function rational_le (r, s) result (res)
    type (rational), intent (in) :: r
    type (rational), intent (in) :: s
    type (rational) :: r_simple
    type (rational) :: s_simple
    logical :: res
    r_simple = rational_simplify (r)
    s_simple = rational_simplify (s)
    res = r_simple % numerator * s_simple % denominator <= &
        & s_simple % numerator * r_simple % denominator
  end function rational_le

  function rational_gt (r, s) result (res)
    type (rational), intent (in) :: r
    type (rational), intent (in) :: s
    type (rational) :: r_simple
    type (rational) :: s_simple
    logical :: res
    r_simple = rational_simplify (r)
    s_simple = rational_simplify (s)
    res = r_simple % numerator * s_simple % denominator > &
        & s_simple % numerator * r_simple % denominator
  end function rational_gt

  function rational_ge (r, s) result (res)
    type (rational), intent (in) :: r
    type (rational), intent (in) :: s
    type (rational) :: r_simple
    type (rational) :: s_simple
    logical :: res
    r_simple = rational_simplify (r)
    s_simple = rational_simplify (s)
    res = r_simple % numerator * s_simple % denominator >= &
        & s_simple % numerator * r_simple % denominator
  end function rational_ge

  function rational_eq (r, s) result (res)
    type (rational), intent (in) :: r
    type (rational), intent (in) :: s
    logical :: res
    res = r % numerator * s % denominator == s % numerator * r % denominator
  end function rational_eq

  function rational_ne (r, s) result (res)
    type (rational), intent (in) :: r
    type (rational), intent (in) :: s
    logical :: res
    res = r % numerator * s % denominator /= s % numerator * r % denominator
  end function rational_ne

  function rational_abs (r) result (res)
    type (rational), intent (in) :: r
    type (rational) :: res
    res = sign (r % numerator, r % denominator) // r % denominator
  end function rational_abs

  function rational_int (r) result (res)
    type (rational), intent (in) :: r
    integer :: res
    res = r % numerator / r % denominator
  end function rational_int

  function rational_modulo (r) result (res)
    type (rational), intent (in) :: r
    integer :: res
    res = modulo (r % numerator, r % denominator)
  end function rational_modulo

end module module_rational
```

Example:

```fortran
program perfect_numbers

  use module_rational
  implicit none
  integer, parameter :: n_min = 2
  integer, parameter :: n_max = 2 ** 19 - 1
  integer :: n
  integer :: factor
  type (rational) :: sum

  do n = n_min, n_max
    sum = 1 // n
    factor = 2
    do
      if (factor * factor >= n) then
        exit
      end if
      if (modulo (n, factor) == 0) then
        sum = rational_simplify (sum + (1 // factor) + (factor // n))
      end if
      factor = factor + 1
    end do
    if (sum % numerator == 1 .and. sum % denominator == 1) then
      write (*, '(i0)') n
    end if
  end do

end program perfect_numbers
```

{{out}}

```txt
6
28
496
8128
```



## Frink

Rational numbers are built into Frink and the numerator and denominator can be arbitrarily-sized.  They are automatically simplified and collapsed into integers if necessary.  All functions in the language can work with rational numbers.  Rational numbers are treated as exact.  Rational numbers can exist in complex numbers or intervals.

```frink

1/2 + 2/3
// 7/6 (approx. 1.1666666666666667)

1/2 + 1/2
// 1

5/sextillion + 3/quadrillion
// 600001/200000000000000000000 (exactly 3.000005e-15)

8^(1/3)
// 2    (note the exact integer result.)

```



## GAP

Rational numbers are built-in.

```gap
2/3 in Rationals;
# true
2/3 + 3/4;
# 17/12
```



## Go

Go does not have user defined operators.  Go does however have a rational number type in the <code>math/big</code> package of the standard library.  The big.Rat type supports the operations of the task, although typically with methods rather than operators:

* Rat.Abs
* Rat.Neg
* Rat.Add
* Rat.Sub
* Rat.Mul
* Rat.Quo
* Rat.Cmp
* Rat.SetInt

Code here implements the perfect number test described in the task using the standard library.

```go
package main

import (
    "fmt"
    "math"
    "math/big"
)

func main() {
    var recip big.Rat
    max := int64(1 << 19)
    for candidate := int64(2); candidate < max; candidate++ {
        sum := big.NewRat(1, candidate)
        max2 := int64(math.Sqrt(float64(candidate)))
        for factor := int64(2); factor <= max2; factor++ {
            if candidate%factor == 0 {
                sum.Add(sum, recip.SetFrac64(1, factor))
                if f2 := candidate / factor; f2 != factor {
                    sum.Add(sum, recip.SetFrac64(1, f2))
                }
            }
        }
        if sum.Denom().Int64() == 1 {
            perfectstring := ""
            if sum.Num().Int64() == 1 {
                perfectstring = "perfect!"
            }
            fmt.Printf("Sum of recipr. factors of %d = %d exactly %s\n",
                candidate, sum.Num().Int64(), perfectstring)
        }
    }
}
```

{{out}}

```txt

Sum of recipr. factors of 6 = 1 exactly perfect!
Sum of recipr. factors of 28 = 1 exactly perfect!
Sum of recipr. factors of 120 = 2 exactly
Sum of recipr. factors of 496 = 1 exactly perfect!
Sum of recipr. factors of 672 = 2 exactly
Sum of recipr. factors of 8128 = 1 exactly perfect!
Sum of recipr. factors of 30240 = 3 exactly
Sum of recipr. factors of 32760 = 3 exactly
Sum of recipr. factors of 523776 = 2 exactly

```



## Groovy

Groovy does not provide any built-in facility for rational arithmetic. However, it does support arithmetic operator overloading. Thus it is not too hard to build a fairly robust, complete, and intuitive rational number class, such as the following:

```groovy
class Rational extends Number implements Comparable {
    final BigInteger num, denom

    static final Rational ONE = new Rational(1)
    static final Rational ZERO = new Rational(0)

    Rational(BigDecimal decimal) {
        this(
        decimal.scale() < 0 ? decimal.unscaledValue() * 10 ** -decimal.scale() : decimal.unscaledValue(),
        decimal.scale() < 0 ? 1                                                : 10 ** decimal.scale()
        )
    }

    Rational(BigInteger n, BigInteger d = 1) {
        if (!d || n == null) { n/d }
        (num, denom) = reduce(n, d)
    }

    private List reduce(BigInteger n, BigInteger d) {
        BigInteger sign = ((n < 0) ^ (d < 0)) ? -1 : 1
        (n, d) = [n.abs(), d.abs()]
        BigInteger commonFactor = gcd(n, d)

        [n.intdiv(commonFactor) * sign, d.intdiv(commonFactor)]
    }

    Rational toLeastTerms() { reduce(num, denom) as Rational }

    private BigInteger gcd(BigInteger n, BigInteger m) {
        n == 0 ? m : { while(m%n != 0) { (n, m) = [m%n, n] }; n }()
    }

    Rational plus(Rational r) { [num*r.denom + r.num*denom, denom*r.denom] }
    Rational plus(BigInteger n) { [num + n*denom, denom] }
    Rational plus(Number n) { this + ([n] as Rational) }

    Rational next() { [num + denom, denom] }

    Rational minus(Rational r) { [num*r.denom - r.num*denom, denom*r.denom] }
    Rational minus(BigInteger n) { [num - n*denom, denom] }
    Rational minus(Number n) { this - ([n] as Rational) }

    Rational previous() { [num - denom, denom] }

    Rational multiply(Rational r) { [num*r.num, denom*r.denom] }
    Rational multiply(BigInteger n) { [num*n, denom] }
    Rational multiply(Number n) { this * ([n] as Rational) }


    Rational div(Rational r) { new Rational(num*r.denom, denom*r.num) }
    Rational div(BigInteger n) { new Rational(num, denom*n) }
    Rational div(Number n) { this / ([n] as Rational) }

    BigInteger intdiv(BigInteger n) { num.intdiv(denom*n) }

    Rational negative() { [-num, denom] }

    Rational abs() { [num.abs(), denom] }

    Rational reciprocal() { new Rational(denom, num) }

    Rational power(BigInteger n) {
        def (nu, de) = (n < 0 ? [denom, num] : [num, denom])*.power(n.abs())
        new Rational (nu, de)
    }

    boolean asBoolean() { num != 0 }

    BigDecimal toBigDecimal() { (num as BigDecimal)/(denom as BigDecimal) }

    BigInteger toBigInteger() { num.intdiv(denom) }

    Double toDouble() { toBigDecimal().toDouble() }
    double doubleValue() { toDouble() as double }

    Float toFloat() { toBigDecimal().toFloat() }
    float floatValue() { toFloat() as float }

    Integer toInteger() { toBigInteger().toInteger() }
    int intValue() { toInteger() as int }

    Long toLong() { toBigInteger().toLong() }
    long longValue() { toLong() as long }

    Object asType(Class type) {
        switch (type) {
            case this.class:              return this
            case [Boolean, Boolean.TYPE]: return asBoolean()
            case BigDecimal:              return toBigDecimal()
            case BigInteger:              return toBigInteger()
            case [Double, Double.TYPE]:   return toDouble()
            case [Float, Float.TYPE]:     return toFloat()
            case [Integer, Integer.TYPE]: return toInteger()
            case [Long, Long.TYPE]:       return toLong()
            case String:                  return toString()
            default: throw new ClassCastException("Cannot convert from type Rational to type " + type)
        }
    }

    boolean equals(o) { compareTo(o) == 0 }

    int compareTo(o) {
        o instanceof Rational
            ? compareTo(o as Rational)
            : o instanceof Number
                ? compareTo(o as Number)
                : (Double.NaN as int)
    }
    int compareTo(Rational r) { num*r.denom <=> denom*r.num }
    int compareTo(Number n) { num <=> denom*(n as BigInteger) }

    int hashCode() { [num, denom].hashCode() }

    String toString() {
        "${num}//${denom}"
    }
}
```


The following ''RationalCategory'' class allows for modification of regular ''Number'' behavior when interacting with ''Rational''.

```groovy
import org.codehaus.groovy.runtime.DefaultGroovyMethods

class RationalCategory {
    static Rational plus (Number a, Rational b) { ([a] as Rational) + b }
    static Rational minus (Number a, Rational b) { ([a] as Rational) - b }
    static Rational multiply (Number a, Rational b) { ([a] as Rational) * b }
    static Rational div (Number a, Rational b) { ([a] as Rational) / b  }

    static <T> T asType (Number a, Class<T> type) {
        type == Rational \
            ? [a] as Rational
                : DefaultGroovyMethods.asType(a, type)
    }
}
```


Test Program (mixes the ''RationalCategory'' methods into the ''Number'' class):

```groovy
Number.metaClass.mixin RationalCategory

def x = [5, 20] as Rational
def y = [9, 12] as Rational
def z = [0, 10000] as Rational

println x
println y
println z
println (x <=> y)
println (x.compareTo(y))
assert x < y
assert x*3 == y
assert x*5.5 == 5.5*x
assert (z + 1) <= y*4
assert x + 1.3 == 1.3 + x
assert 24 - y == -(y - 24)
assert 3 / y == (y / 3).reciprocal()
assert x != y

println "x + y == ${x} + ${y} == ${x + y}"
println "x + z == ${x} + ${z} == ${x + z}"
println "x - y == ${x} - ${y} == ${x - y}"
println "x - z == ${x} - ${z} == ${x - z}"
println "x * y == ${x} * ${y} == ${x * y}"
println "y ** 3 == ${y} ** 3 == ${y ** 3}"
println "y ** -3 == ${y} ** -3 == ${y ** -3}"
println "x * z == ${x} * ${z} == ${x * z}"
println "x / y == ${x} / ${y} == ${x / y}"
try { print "x / z == ${x} / ${z} == "; println "${x / z}" }
catch (Throwable t) { println t.message }

println "-x == -${x} == ${-x}"
println "-y == -${y} == ${-y}"
println "-z == -${z} == ${-z}"

print "x as int == ${x} as int == "; println x.intValue()
print "x as double == ${x} as double == "; println x.doubleValue()
print "1 / x as int == 1 / ${x} as int == "; println x.reciprocal().intValue()
print "1.0 / x == 1.0 / ${x} == "; println x.reciprocal().doubleValue()
print "y as int == ${y} as int == "; println y.intValue()
print "y as double == ${y} as double == "; println y.doubleValue()
print "1 / y as int == 1 / ${y} as int == "; println y.reciprocal().intValue()
print "1.0 / y == 1.0 / ${y} == "; println y.reciprocal().doubleValue()
print "z as int == ${z} as int == "; println z.intValue()
print "z as double == ${z} as double == "; println z.doubleValue()
try { print "1 / z as int == 1 / ${z} as int == "; println z.reciprocal().intValue() }
catch (Throwable t) { println t.message }
try { print "1.0 / z == 1.0 / ${z} == "; println z.reciprocal().doubleValue() }
catch (Throwable t) { println t.message }

println "++x == ++ ${x} == ${++x}"
println "++y == ++ ${y} == ${++y}"
println "++z == ++ ${z} == ${++z}"
println "-- --x == -- -- ${x} == ${-- (--x)}"
println "-- --y == -- -- ${y} == ${-- (--y)}"
println "-- --z == -- -- ${z} == ${-- (--z)}"
println x
println y
println z

println (x <=> y)
assert x*3 == y
assert (z + 1) <= y*4
assert (x < y)

println 25 as Rational
println 25.0 as Rational
println 0.25 as Rational

def ε = 0.000000001  // tolerance (epsilon): acceptable "wrongness" to account for rounding error

def π = Math.PI
def α = π as Rational
assert (π - (α as BigDecimal)).abs() < ε
println π
println α
println (α.toBigDecimal())
println (α as BigDecimal)
println (α as Double)
println (α as double)
println (α as boolean)
println (z as boolean)
try { println (α as Date) }
catch (Throwable t) { println t.message }
try { println (α as char) }
catch (Throwable t) { println t.message }
```

{{out}}
<pre style="height:30ex;overflow:scroll;">1//4
3//4
0//1
-1
-1
x + y == 1//4 + 3//4 == 1//1
x + z == 1//4 + 0//1 == 1//4
x - y == 1//4 - 3//4 == -1//2
x - z == 1//4 - 0//1 == 1//4
x * y == 1//4 * 3//4 == 3//16
y ** 3 == 3//4 ** 3 == 27//64
y ** -3 == 3//4 ** -3 == 64//27
x * z == 1//4 * 0//1 == 0//1
x / y == 1//4 / 3//4 == 1//3
x / z == 1//4 / 0//1 == Division by zero
-x == -1//4 == -1//4
-y == -3//4 == -3//4
-z == -0//1 == 0//1
x as int == 1//4 as int == 0
x as double == 1//4 as double == 0.25
1 / x as int == 1 / 1//4 as int == 4
1.0 / x == 1.0 / 1//4 == 4.0
y as int == 3//4 as int == 0
y as double == 3//4 as double == 0.75
1 / y as int == 1 / 3//4 as int == 1
1.0 / y == 1.0 / 3//4 == 1.3333333333
z as int == 0//1 as int == 0
z as double == 0//1 as double == 0.0
1 / z as int == 1 / 0//1 as int == Division by zero
1.0 / z == 1.0 / 0//1 == Division by zero
++x == ++ 1//4 == 5//4
++y == ++ 3//4 == 7//4
++z == ++ 0//1 == 1//1
-- --x == -- -- 5//4 == -3//4
-- --y == -- -- 7//4 == -1//4
-- --z == -- -- 1//1 == -1//1
1//4
3//4
0//1
-1
25//1
25//1
1//4
3.141592653589793
884279719003555//281474976710656
3.141592653589793115997963468544185161590576171875
3.141592653589793115997963468544185161590576171875
3.141592653589793
3.141592653589793
true
false
Cannot convert from type Rational to type class java.util.Date
Cannot convert from type Rational to type char

```

The following uses the ''Rational'' class, with ''RationalCategory'' mixed into ''Number'', to find all perfect numbers less than 2<sup>19</sup>:

```groovy
Number.metaClass.mixin RationalCategory

def factorize = { target ->
    assert target > 0
    if (target == 1L) { return [1L] }
    if ([2L, 3L].contains(target)) { return [1L, target] }
    def targetSqrt = Math.sqrt(target)
    def lowFactors = (2L..targetSqrt).findAll { (target % it) == 0 }

    if (!lowFactors) { return [1L, target] }
    def highFactors = lowFactors[-1..0].findResults { target.intdiv(it) } - lowFactors[-1]

    return [1L] + lowFactors + highFactors + [target]
}

def perfect = {
    def factors = factorize(it)
    2 as Rational == factors.sum{ factor -> new Rational(1, factor) } \
        ? [perfect: it, factors: factors]
        : null
}

def trackProgress = { if ((it % (100*1000)) == 0) { println it } else if ((it % 1000) == 0) { print "." } }

(1..(2**19)).findResults { trackProgress(it); perfect(it) }.each { println(); print it }
```

{{out}}

```txt
...................................................................................................100000
...................................................................................................200000
...................................................................................................300000
...................................................................................................400000
...................................................................................................500000
........................
[perfect:6, factors:[1, 2, 3, 6]]
[perfect:28, factors:[1, 2, 4, 7, 14, 28]]
[perfect:496, factors:[1, 2, 4, 8, 16, 31, 62, 124, 248, 496]]
[perfect:8128, factors:[1, 2, 4, 8, 16, 32, 64, 127, 254, 508, 1016, 2032, 4064, 8128]]
```



## Haskell

Haskell provides a <code>Rational</code> type, which is really an alias for <code>Ratio Integer</code> (<code>Ratio</code> being a polymorphic type implementing rational numbers for any <code>Integral</code> type of numerators and denominators). The fraction is constructed using the <code>%</code> operator.

```haskell
import Data.Ratio ((%))

-- Prints the first N perfect numbers.
main = do
  let n = 4
  mapM_ print $
    take
      n
      [ candidate
      | candidate <- [2 .. 2 ^ 19]
      , getSum candidate == 1 ]
  where
    getSum candidate =
      1 % candidate +
      sum
        [ 1 % factor + 1 % (candidate `div` factor)
        | factor <- [2 .. floor (sqrt (fromIntegral candidate))]
        , candidate `mod` factor == 0 ]

```


For a sample implementation of <code>Ratio</code>, see [http://www.haskell.org/onlinereport/ratio.html the Haskell 98 Report].

=={{header|Icon}} and {{header|Unicon}}==
The IPL provides support for rational arithmetic
*  The data type is called 'rational' not 'frac'.
*  Use the record constructor 'rational' to create a rational.  Sign must be 1 or -1.
*  Neither Icon nor Unicon supports operator overloading.  Augmented assignments make little sense w/o this.
*  Procedures include 'negrat' (unary -), 'addrat' (+), 'subrat' (-), 'mpyrat' (*), 'divrat' (modulo /).
Additional procedures are implemented here to complete the task:
*  'makerat' (make), 'absrat' (abs), 'eqrat' (=), 'nerat' (~=), 'ltrat' (<), 'lerat' (<=), 'gerat' (>=), 'gtrat' (>)

```Icon
procedure main()
   limit := 2^19

   write("Perfect numbers up to ",limit," (using rational arithmetic):")
   every write(is_perfect(c := 2 to limit))
   write("End of perfect numbers")

   # verify the rest of the implementation

   zero := makerat(0)          # from integer
   half := makerat(0.5)        # from real
   qtr  := makerat("1/4")      # from strings ...
   one  := makerat("1")
   mone := makerat("-1")

   verifyrat("eqrat",zero,zero)
   verifyrat("ltrat",zero,half)
   verifyrat("ltrat",half,zero)
   verifyrat("gtrat",zero,half)
   verifyrat("gtrat",half,zero)
   verifyrat("nerat",zero,half)
   verifyrat("nerat",zero,zero)
   verifyrat("absrat",mone,)

end

procedure is_perfect(c)       #: test for perfect numbers using rational arithmetic
   rsum := rational(1, c, 1)
   every f := 2 to sqrt(c) do
      if 0 = c % f then
         rsum := addrat(rsum,addrat(rational(1,f,1),rational(1,integer(c/f),1)))
   if rsum.numer = rsum.denom = 1 then
      return c
end
```

{{out}}

```txt
Perfect numbers up to 524288 (using rational arithmetic):
6
28
496
8128
End of perfect numbers
Testing eqrat( (0/1), (0/1) ) ==> returned (0/1)
Testing ltrat( (0/1), (1/2) ) ==> returned (1/2)
Testing ltrat( (1/2), (0/1) ) ==> failed
Testing gtrat( (0/1), (1/2) ) ==> failed
Testing gtrat( (1/2), (0/1) ) ==> returned (0/1)
Testing nerat( (0/1), (1/2) ) ==> returned (1/2)
Testing nerat( (0/1), (0/1) ) ==> failed
Testing absrat( (-1/1),  ) ==> returned (1/1)
```

The following task functions are missing from the IPL:

```Icon
procedure verifyrat(p,r1,r2)  #: verification tests for rational procedures
return write("Testing ",p,"( ",rat2str(r1),", ",rat2str(\r2) | &null," ) ==> ","returned " || rat2str(p(r1,r2)) | "failed")
end

procedure makerat(x)          #: make rational (from integer, real, or strings)
local n,d
static c
initial c := &digits++'+-'

   return case type(x) of {
             "real"    : real2rat(x)
             "integer" : ratred(rational(x,1,1))
             "string"  : if x ? ( n := integer(tab(many(c))), ="/", d := integer(tab(many(c))), pos(0)) then
                            ratred(rational(n,d,1))
                         else
                            makerat(numeric(x))
          }
end

procedure absrat(r1)          #: abs(rational)
   r1 := ratred(r1)
   r1.sign := 1
   return r1
end

invocable all                 #  for string invocation

procedure xoprat(op,r1,r2)    #: support procedure for binary operations that cross denominators
   local numer, denom, div

   r1 := ratred(r1)
   r2 := ratred(r2)

   return if op(r1.numer * r2.denom,r2.numer * r1.denom) then r2   # return right argument on success
end

procedure eqrat(r1,r2)        #: rational r1 = r2
return xoprat("=",r1,r2)
end

procedure nerat(r1,r2)        #: rational r1 ~= r2
return xoprat("~=",r1,r2)
end

procedure ltrat(r1,r2)        #: rational r1 < r2
return xoprat("<",r1,r2)
end

procedure lerat(r1,r2)        #: rational r1 <= r2
return xoprat("<=",r1,r2)
end

procedure gerat(r1,r2)        #: rational r1 >= r2
return xoprat(">=",r1,r2)
end

procedure gtrat(r1,r2)        #: rational r1 > r2
return xoprat(">",r1,r2)
end

link rational
```

The {{libheader|Icon Programming Library}} provides [http://www.cs.arizona.edu/icon/library/src/procs/rational.icn rational] and [http://www.cs.arizona.edu/icon/library/src/procs/numbers.icn gcd in numbers].  Record definition and usage is shown below:

```Icon
   record rational(numer, denom, sign)        # rational type

   addrat(r1,r2) # Add rational numbers r1 and r2.
   divrat(r1,r2) # Divide rational numbers r1 and r2.
   medrat(r1,r2) # Form mediant of r1 and r2.
   mpyrat(r1,r2) # Multiply rational numbers r1 and r2.
   negrat(r)     # Produce negative of rational number r.
   rat2real(r)   # Produce floating-point approximation of r
   rat2str(r)    # Convert the rational number r to its string representation.
   real2rat(v,p) # Convert real to rational with precision p (default 1e-10). Warning: excessive p gives ugly fractions
   reciprat(r)   # Produce the reciprocal of rational number r.
   str2rat(s)    # Convert the string representation (such as "3/2") to a rational number
   subrat(r1,r2) # Subtract rational numbers r1 and r2.

   gcd(i, j)     # returns greatest common divisor of i and j
```



## J

Rational numbers in J may be formed from fixed precision integers by first upgrading them to arbitrary precision integers and then dividing them:

```J
  (x: 3) % (x: -4)
_3r4
   3 %&x: -4
_3r4
```

Note that the syntax is analogous to the syntax for floating point numbers, but uses <code>r</code> to separate the numerator and denominator instead of <code>e</code> to separate the mantissa and exponent.
Thus:

```J

   | _3r4             NB. absolute value
3r4
   -2r5               NB. negation
_2r5
   3r4+2r5            NB. addition
23r20
   3r4-2r5            NB. subtraction
7r20
   3r4*2r5            NB. multiplication
3r10
   3r4%2r5            NB. division
15r8
   3r4 <.@% 2r5       NB. integer division
1
   3r4 (-~ <.)@% 2r5  NB. remainder
_7r8
   3r4 < 2r5          NB. less than
0
   3r4 <: 2r5         NB. less than or equal
0
   3r4 > 2r5          NB. greater than
1
   3r4 >: 2r5         NB. greater than or equal
1
   3r4 = 2r5          NB. equal
0
   3r4 ~: 2r5         NB. not equal
1
```


You can also coerce numbers directly to rational using x: (or to integer or floating point as appropriate using its inverse)


```J
   x: 3%4
3r4
   x:inv 3%4
0.75
```


Increment and decrement are also included in the language, but you could just as easily add or subtract 1:


```J>
: 3r4
7r4
   <: 3r4
_1r4
```


J does not encourage the use of specialized mutators, but those could also be defined:


```j
mutadd=:adverb define
   (m)=: (".m)+y
)

mutsub=:adverb define
   (m)=: (".m)-y
)
```


Note that the name whose association is being modified in this fashion needs to be quoted (or you can use an expression to provide the name):


```J
   n=: 3r4
   'n' mutadd 1
7r4
   'n' mutsub 1
3r4
   'n' mutsub 1
_1r4
```


(Bare words to the immediate left of the assignment operator are implicitly quoted - but this is just syntactic sugar because that is such an overwhelmingly common case.)

That said, note that J's floating point numbers work just fine for the stated problem:

```j
   is_perfect_rational=: 2 = (1 + i.) +/@:%@([ #~ 0 = |) ]
```

Faster version (but the problem, as stated, is still tremendously inefficient):

```j
factors=: */&>@{@((^ i.@>:)&.>/)@q:~&__
is_perfect_rational=: 2= +/@:%@,@factors
```

Exhaustive testing would take forever:

```j
   I.is_perfect_rational@"0 i.2^19
6 28 496 8128
   I.is_perfect_rational@x:@"0 i.2^19x
6 28 496 8128
```

More limited testing takes reasonable amounts of time:

```j
   (#~ is_perfect_rational"0) (* <:@+:) 2^i.10x
6 28 496 8128
```



## Java

Uses BigRational class: [[Arithmetic/Rational/Java]]

```java
public class BigRationalFindPerfectNumbers {
    public static void main(String[] args) {
        int MAX_NUM = 1 << 19;
        System.out.println("Searching for perfect numbers in the range [1, " + (MAX_NUM - 1) + "]");

        BigRational TWO = BigRational.valueOf(2);
        for (int i = 1; i < MAX_NUM; i++) {
            BigRational reciprocalSum = BigRational.ONE;
            if (i > 1)
                reciprocalSum = reciprocalSum.add(BigRational.valueOf(i).reciprocal());
            int maxDivisor = (int) Math.sqrt(i);
            if (maxDivisor >= i)
                maxDivisor--;

            for (int divisor = 2; divisor <= maxDivisor; divisor++) {
                if (i % divisor == 0) {
                    reciprocalSum = reciprocalSum.add(BigRational.valueOf(divisor).reciprocal());
                    int dividend = i / divisor;
                    if (divisor != dividend)
                        reciprocalSum = reciprocalSum.add(BigRational.valueOf(dividend).reciprocal());
                }
            }
            if (reciprocalSum.equals(TWO))
                System.out.println(String.valueOf(i) + " is a perfect number");
        }
    }
}
```

{{out}}

```txt
Searching for perfect numbers in the range [1, 524287]
6 is a perfect number
28 is a perfect number
496 is a perfect number
8128 is a perfect number
```



## JavaScript

<div style="text-align:right;font-size:7pt">''<nowiki>[</nowiki>This section is included from [[Arithmetic/Rational/JavaScript|a subpage]] and should be edited there, not here.<nowiki>]</nowiki>''</div>
{{:Arithmetic/Rational/JavaScript}}


## Julia

Julia has native support for rational numbers.  Rationals are expressed as <tt>m//n</tt>, where <tt>m</tt> and <tt>n</tt> are integers.  In addition to supporting most of the usual mathematical functions in a natural way on rationals, the methods <tt>num</tt> and <tt>den</tt> provide the fully reduced numerator and denominator of a rational value.
{{works with|Julia|1.2}}

```Julia
using Primes
divisors(n) = foldl((a, (p, e)) -> vcat((a * [p^i for i in 0:e]')...), factor(n), init=[1])

isperfect(n) = sum(1 // d for d in divisors(n)) == 2

lo, hi = 2, 2^19
println("Perfect numbers between ", lo, " and ", hi, ": ", collect(filter(isperfect, lo:hi)))

```


{{out}}

```txt

Perfect numbers between 2 and 524288: [6, 28, 496, 8128]

```



## Kotlin

As it's not possible to define arbitrary symbols such as // to be operators in Kotlin, we instead use infix functions idiv (for Ints) and ldiv (for Longs) as a shortcut to generate Frac instances.

```scala
// version 1.1.2

fun gcd(a: Long, b: Long): Long = if (b == 0L) a else gcd(b, a % b)

infix fun Long.ldiv(denom: Long) = Frac(this, denom)

infix fun Int.idiv(denom: Int) = Frac(this.toLong(), denom.toLong())

fun Long.toFrac() = Frac(this, 1)

fun Int.toFrac() = Frac(this.toLong(), 1)

class Frac : Comparable<Frac> {
    val num: Long
    val denom: Long

    companion object {
        val ZERO = Frac(0, 1)
        val ONE  = Frac(1, 1)
    }

    constructor(n: Long, d: Long) {
        require(d != 0L)
        var nn = n
        var dd = d
        if (nn == 0L) {
            dd = 1
        }
        else if (dd < 0) {
            nn = -nn
            dd = -dd
        }
        val g = Math.abs(gcd(nn, dd))
        if (g > 1) {
            nn /= g
            dd /= g
        }
        num = nn
        denom = dd
    }

    constructor(n: Int, d: Int) : this(n.toLong(), d.toLong())

    operator fun plus(other: Frac) =
        Frac(num * other.denom + denom * other.num, other.denom * denom)

    operator fun unaryPlus() = this

    operator fun unaryMinus() = Frac(-num, denom)

    operator fun minus(other: Frac) = this + (-other)

    operator fun times(other: Frac) = Frac(this.num * other.num, this.denom * other.denom)

    operator fun rem(other: Frac) = this - Frac((this / other).toLong(), 1) * other

    operator fun inc() = this + ONE
    operator fun dec() = this - ONE

    fun inverse(): Frac {
        require(num != 0L)
        return Frac(denom, num)
    }

    operator fun div(other: Frac) = this * other.inverse()

    fun abs() = if (num >= 0) this else -this

    override fun compareTo(other: Frac): Int {
        val diff = this.toDouble() - other.toDouble()
        return when {
            diff < 0.0  -> -1
            diff > 0.0  -> +1
            else        ->  0
        }
    }

    override fun equals(other: Any?): Boolean {
       if (other == null || other !is Frac) return false
       return this.compareTo(other) == 0
    }

    override fun hashCode() = num.hashCode() xor denom.hashCode()

    override fun toString() = if (denom == 1L) "$num" else "$num/$denom"

    fun toDouble() = num.toDouble() / denom

    fun toLong() = num / denom
}

fun isPerfect(n: Long): Boolean {
    var sum = Frac(1, n)
    val limit = Math.sqrt(n.toDouble()).toLong()
    for (i in 2L..limit) {
        if (n % i == 0L) sum += Frac(1, i) + Frac(1, n / i)
    }
    return sum == Frac.ONE
}

fun main(args: Array<String>) {
    var frac1 = Frac(12, 3)
    println ("frac1 = $frac1")
    var frac2 = 15 idiv 2
    println("frac2 = $frac2")
    println("frac1 <= frac2 is ${frac1 <= frac2}")
    println("frac1 >= frac2 is ${frac1 >= frac2}")
    println("frac1 == frac2 is ${frac1 == frac2}")
    println("frac1 != frac2 is ${frac1 != frac2}")
    println("frac1 + frac2 = ${frac1 + frac2}")
    println("frac1 - frac2 = ${frac1 - frac2}")
    println("frac1 * frac2 = ${frac1 * frac2}")
    println("frac1 / frac2 = ${frac1 / frac2}")
    println("frac1 % frac2 = ${frac1 % frac2}")
    println("inv(frac1)    = ${frac1.inverse()}")
    println("abs(-frac1)   = ${-frac1.abs()}")
    println("inc(frac2)    = ${++frac2}")
    println("dec(frac2)    = ${--frac2}")
    println("dbl(frac2)    = ${frac2.toDouble()}")
    println("lng(frac2)    = ${frac2.toLong()}")
    println("\nThe Perfect numbers less than 2^19 are:")
    // We can skip odd numbers as no known perfect numbers are odd
    for (i in 2 until (1 shl 19) step 2) {
        if (isPerfect(i.toLong())) print("  $i")
    }
    println()
}
```


{{out}}

```txt

frac1 = 4
frac2 = 15/2
frac1 <= frac2 is true
frac1 >= frac2 is false
frac1 == frac2 is false
frac1 != frac2 is true
frac1 + frac2 = 23/2
frac1 - frac2 = -7/2
frac1 * frac2 = 30
frac1 / frac2 = 8/15
frac1 % frac2 = 4
inv(frac1)    = 1/4
abs(-frac1)   = -4
inc(frac2)    = 17/2
dec(frac2)    = 15/2
dbl(frac2)    = 7.5
lng(frac2)    = 7

The Perfect numbers less than 2^19 are:
  6  28  496  8128

```



## Lingo

A new 'frac' data type can be implemented like this:

```lingo
-- parent script "Frac"
property num
property denom

----------------------------------------
-- @constructor
-- @param {integer} numerator
-- @param {integer} [denominator=1]
----------------------------------------
on new (me, numerator, denominator)
  if voidP(denominator) then denominator = 1
  if denominator=0 then return VOID -- rule out division by zero
  g = me._gcd(numerator, denominator)
  if g<>0 then
    numerator = numerator/g
    denominator = denominator/g
  else
    numerator = 0
    denominator = 1
  end if
  if denominator<0 then
    numerator = -numerator
    denominator = -denominator
  end if
  me.num = numerator
  me.denom = denominator
  return me
end

----------------------------------------
-- Returns string representation "<num>/<denom>"
-- @return {string}
----------------------------------------
on toString (me)
  return me.num&"/"&me.denom
end

----------------------------------------
--
----------------------------------------
on _gcd (me, a, b)
  if a = 0 then return b
  if b = 0 then return a
  if a > b then return me._gcd(b, a mod b)
  return me._gcd(a, b mod a)
end
```


Lingo does not support overwriting built-in operators, so 'frac'-operators must be implemented as functions:

```lingo
-- Frac library (movie script)

----------------------------------------
-- Shortcut for creating 'frac' values
-- @param {integer} numerator
-- @param {integer} denominator
-- @return {instance}
----------------------------------------
on frac (numerator, denominator)
  return script("Frac").new(numerator, denominator)
end

----------------------------------------
-- All functions below this comment only support 'fracs', i.e. instances
-- of the Frac Class, as arguments. An integer n is casted to frac via frac(n).
----------------------------------------

-- Optionally supports more than 2 arguments
on fAdd (a, b) -- ...
  res = a
  repeat with i = 2 to the paramCount
    p = param(i)
    num = res.num * p.denom + res.denom * p.num
    denom = res.denom * p.denom
    res = frac(num, denom)
  end repeat
  return res
end

on fSub (a, b)
  return frac(a.num * b.den - a.den * b.num, a.den * b.den)
end

-- Optionally supports more than 2 arguments
on fMul (a, b) -- ...
  res = a
  repeat with i = 2 to the paramCount
    p = param(i)
    res = frac(res.num * p.num, res.denom * p.denom)
  end repeat
  return res
end

on fDiv (a, b)
  return frac(a.num * b.denom, a.denom * b.num)
end

on fAbs (f)
  return frac(abs(f.num), f.denom)
end

on fNeg (f)
  return frac(-f.num, f.denom)
end

on fEQ (a, b)
  diff = fSub(a, b)
  return diff.num=0
end

on fNE (a, b)
  return not fEQ (a, b)
end

on fGT (a, b)
  diff = fSub(a, b)
  return diff.num>0
end

on fLT (a, b)
  diff = fSub(a, b)
  return diff.num<0
end

on fGE (a, b)
  diff = fSub(a, b)
  return diff.num>=0
end

on fLE (a, b)
  diff = fSub(a, b)
  return diff.num<=0
end
```

Usage:

```lingo
f = frac(2,3)
put f.toString()
-- "2/3"

-- fractions are normalized on the fly
f = frac(4,6)
put f.toString()
-- "2/3"

-- casting integer to frac
f = frac(23)
put f.toString()
-- "23/1"
```


Finding perfect numbers:

```lingo
-- in some movie script
----------------------------------------
-- Prints all perfect numbers up to n
-- @param {integer|float} n
----------------------------------------
on findPerfects (n)
  repeat with i = 2 to n
    sum = frac(1, i)
    cnt = sqrt(i)
    repeat with fac = 2 to cnt
      if i mod fac = 0 then sum = fAdd(sum, frac(1, fac), frac(fac, i))
    end repeat
    if sum.denom = sum.num then put i
  end repeat
end
```


```lingo
findPerfects(power(2, 19))
-- 6
-- 28
-- 496
-- 8128
```



## Lua


```lua
function gcd(a,b) return a == 0 and b or gcd(b % a, a) end

do
  local function coerce(a, b)
    if type(a) == "number" then return rational(a, 1), b end
    if type(b) == "number" then return a, rational(b, 1) end
    return a, b
  end
  rational = setmetatable({
  __add = function(a, b)
      local a, b = coerce(a, b)
      return rational(a.num * b.den + a.den * b.num, a.den * b.den)
    end,
  __sub = function(a, b)
      local a, b = coerce(a, b)
      return rational(a.num * b.den - a.den * b.num, a.den * b.den)
    end,
  __mul = function(a, b)
      local a, b = coerce(a, b)
      return rational(a.num * b.num, a.den * b.den)
    end,
  __div = function(a, b)
      local a, b = coerce(a, b)
      return rational(a.num * b.den, a.den * b.num)
    end,
  __pow = function(a, b)
      if type(a) == "number" then return a ^ (b.num / b.den) end
      return rational(a.num ^ b, a.den ^ b) --runs into a problem if these aren't integers
    end,
  __concat = function(a, b)
      if getmetatable(a) == rational then return a.num .. "/" .. a.den .. b end
      return a .. b.num .. "/" .. b.den
    end,
  __unm = function(a) return rational(-a.num, -a.den) end}, {
  __call = function(z, a, b) return setmetatable({num = a / gcd(a, b),den = b / gcd(a, b)}, z) end} )
end

print(rational(2, 3) + rational(3, 5) - rational(1, 10) .. "") --> 7/6
print((rational(4, 5) * rational(5, 9)) ^ rational(1, 2) .. "") --> 2/3
print(rational(45, 60) / rational(5, 2) .. "") --> 3/10
print(5 + rational(1, 3) .. "") --> 16/3

function findperfs(n)
  local ret = {}
  for i = 1, n do
    sum = rational(1, i)
    for fac = 2, i^.5 do
      if i % fac == 0 then
        sum = sum + rational(1, fac) + rational(fac, i)
      end
    end
    if sum.den == sum.num then
      ret[#ret + 1] = i
    end
  end
  return table.concat(ret, '\n')
end
print(findperfs(2^19))
```



## Liberty BASIC

Testing all numbers up to 2 ^ 19 takes an excessively long time.

```lb

n=2^19
for testNumber=1 to n
    sum$=castToFraction$(0)
    for factorTest=1 to sqr(testNumber)
        if GCD(factorTest,testNumber)=factorTest then sum$=add$(sum$,add$(reciprocal$(castToFraction$(factorTest)),reciprocal$(castToFraction$(testNumber/factorTest))))
    next factorTest
    if equal(sum$,castToFraction$(2))=1 then print testNumber
next testNumber
end

function abs$(a$)
    aNumerator=val(word$(a$,1,"/"))
    aDenominator=val(word$(a$,2,"/"))
    bNumerator=abs(aNumerator)
    bDenominator=abs(aDenominator)
    b$=str$(bNumerator)+"/"+str$(bDenominator)
    abs$=simplify$(b$)
end function

function negate$(a$)
    aNumerator=val(word$(a$,1,"/"))
    aDenominator=val(word$(a$,2,"/"))
    bNumerator=-1*aNumerator
    bDenominator=aDenominator
    b$=str$(bNumerator)+"/"+str$(bDenominator)
    negate$=simplify$(b$)
end function

function add$(a$,b$)
    aNumerator=val(word$(a$,1,"/"))
    aDenominator=val(word$(a$,2,"/"))
    bNumerator=val(word$(b$,1,"/"))
    bDenominator=val(word$(b$,2,"/"))
    cNumerator=(aNumerator*bDenominator+bNumerator*aDenominator)
    cDenominator=aDenominator*bDenominator
    c$=str$(cNumerator)+"/"+str$(cDenominator)
    add$=simplify$(c$)
end function

function subtract$(a$,b$)
    aNumerator=val(word$(a$,1,"/"))
    aDenominator=val(word$(a$,2,"/"))
    bNumerator=val(word$(b$,1,"/"))
    bDenominator=val(word$(b$,2,"/"))
    cNumerator=(aNumerator*bDenominator-bNumerator*aDenominator)
    cDenominator=aDenominator*bDenominator
    c$=str$(cNumerator)+"/"+str$(cDenominator)
    subtract$=simplify$(c$)
end function

function multiply$(a$,b$)
    aNumerator=val(word$(a$,1,"/"))
    aDenominator=val(word$(a$,2,"/"))
    bNumerator=val(word$(b$,1,"/"))
    bDenominator=val(word$(b$,2,"/"))
    cNumerator=aNumerator*bNumerator
    cDenominator=aDenominator*bDenominator
    c$=str$(cNumerator)+"/"+str$(cDenominator)
    multiply$=simplify$(c$)
end function

function divide$(a$,b$)
    divide$=multiply$(a$,reciprocal$(b$))
end function

function simplify$(a$)
    aNumerator=val(word$(a$,1,"/"))
    aDenominator=val(word$(a$,2,"/"))
    gcd=GCD(aNumerator,aDenominator)
    if aNumerator<0 and aDenominator<0 then gcd=-1*gcd
    bNumerator=aNumerator/gcd
    bDenominator=aDenominator/gcd
    b$=str$(bNumerator)+"/"+str$(bDenominator)
    simplify$=b$
end function

function reciprocal$(a$)
    aNumerator=val(word$(a$,1,"/"))
    aDenominator=val(word$(a$,2,"/"))
    reciprocal$=str$(aDenominator)+"/"+str$(aNumerator)
end function

function equal(a$,b$)
    if simplify$(a$)=simplify$(b$) then equal=1:else equal=0
end function

function castToFraction$(a)
    do
        exp=exp+1
        a=a*10
    loop until a=int(a)
    castToFraction$=simplify$(str$(a)+"/"+str$(10^exp))
end function

function castToReal(a$)
    aNumerator=val(word$(a$,1,"/"))
    aDenominator=val(word$(a$,2,"/"))
    castToReal=aNumerator/aDenominator
end function

function castToInt(a$)
    castToInt=int(castToReal(a$))
end function

function GCD(a,b)
    if a=0 then
        GCD=1
    else
        if a>=b then
            while b
                c = a
                a = b
                b = c mod b
                GCD = abs(a)
            wend
        else
            GCD=GCD(b,a)
        end if
    end if
end function

```


## M2000 Interpreter

http://www.rosettacode.org/wiki/M2000_Interpreter_rational_numbers


```M2000 Interpreter

Class Rational {
      \\ this is a compact version for this task
      numerator as decimal, denominator as decimal
      gcd=lambda->0
      lcm=lambda->0
      operator "+" {
           Read l
           denom=.lcm(l.denominator, .denominator)
           .numerator<=denom/l.denominator*l.numerator+denom/.denominator*.numerator
           if .numerator==0 then denom=1
           .denominator<=denom
      }
      Group Real {
            value {
                  link parent numerator, denominator to n, d
                  =n/d
            }
      }
      Group ToString$ {
           value {
                  link parent numerator, denominator to n, d
                  =Str$(n)+"/"+Str$(d,"")
            }
      }
      class:
      Module Rational (.numerator, .denominator) {
            if .denominator<=0 then Error "Positive only denominator"
            gcd1=lambda (a as decimal, b as decimal) -> {
                  if a<b then swap a,b
                  g=a mod b
                  while g {
                        a=b:b=g: g=a mod b
                  }
                        =abs(b)
            }
            .gcd<=gcd1
            .lcm<=lambda gcd=gcd1 (a as decimal, b as decimal) -> {
                  =a/gcd(a,b)*b
            }
      }
}
sum=rational(1, 1)
onediv=rational(1,1)
divcand=rational(1,1)
Profiler
For sum.denominator= 2 to 2**15 {
      divcand.denominator=sum.denominator
      For onediv.denominator=2 to sqrt(sum.denominator) {
            if sum.denominator mod onediv.denominator = 0 then {
                  divcand.numerator=onediv.denominator
                  sum=sum+onediv+divcand
            }
      }
      if sum.real=1 then Print sum.denominator;" is perfect"
      sum.numerator=1
}
Print timecount

```



## Maple

Maple has full built-in support for arithmetic with fractions (rational numbers).  Fractions are treated like any other number in Maple.

```Maple

> a := 3 / 5;
                                a := 3/5

> numer( a );
                                   3

> denom( a );
                                   5

```

However, while you can enter a fraction such as "4/6", it will automatically be reduced so that the numerator and denominator have no common factor:

```Maple

> b := 4 / 6;
                                b := 2/3

```

All the standard arithmetic operators work with rational numbers.  It is not necessary to call any special routines.

```Maple

> a + b;
                                   19
                                   --
                                   15

> a * b;
                                  2/5

> a / b;
                                  9/10

> a - b;
                                   -1
                                   --
                                   15

> a + 1;
                                  8/5

> a - 1;
                                  -2/5

```

Notice that fractions are treated as exact quantities; they are not converted to floats.  However, you can get a floating point approximation to any desired accuracy by applying the function evalf to a fraction.

```Maple

> evalf( 22 / 7 ); # default is 10 digits
                              3.142857143

> evalf[100]( 22 / 7 ); # 100 digits
3.142857142857142857142857142857142857142857142857142857142857142857\
    142857142857142857142857142857143

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Mathematica has full support for fractions built-in. If one divides two exact numbers it will be left as a fraction if it can't be simplified. Comparison, addition, division, product et cetera are built-in:

```Mathematica
4/16
3/8
8/4
4Pi/2
16!/10!
Sqrt[9/16]
Sqrt[3/4]
(23/12)^5
2 + 1/(1 + 1/(3 + 1/4))

1/2+1/3+1/5
8/Pi+Pi/8 //Together
13/17 + 7/31
Sum[1/n,{n,1,100}]      (*summation of 1/1 + 1/2 + 1/3 + 1/4+ .........+ 1/99 + 1/100*)

1/2-1/3
a=1/3;a+=1/7

1/4==2/8
1/4>3/8
Pi/E >23/20
1/3!=123/370
Sin[3]/Sin[2]>3/20

Numerator[6/9]
Denominator[6/9]
```

gives back:

```txt
1/4
3/8
2
2 Pi
5765760
3/4
Sqrt[3]/2
6436343 / 248832
47/17

31/30
(64+Pi^2) / (8 Pi)
522 / 527
14466636279520351160221518043104131447711 / 2788815009188499086581352357412492142272

1/6
10/21

True
False
True
True
True

2
3
```

As you can see, Mathematica automatically handles fraction as exact things, it doesn't evaluate the fractions to a float. It only does this when either the numerator or the denominator is not exact. I only showed integers above, but Mathematica can handle symbolic fraction in the same and complete way:

```Mathematica
c/(2 c)
(b^2 - c^2)/(b - c)  // Cancel
1/2 + b/c // Together
```

gives back:

```Mathematica
1/2
b+c
(2 b+c) / (2 c)
```

Moreover it does simplification like Sin[x]/Cos[x] => Tan[x]. Division, addition, subtraction, powering and multiplication of a list (of any dimension) is automatically threaded over the elements:

```Mathematica
1+2*{1,2,3}^3
```

gives back:

```Mathematica
{3, 17, 55}
```

To check for perfect numbers in the range 1 to 2^25 we can use:

```Mathematica
found={};
CheckPerfect[num_Integer]:=If[Total[1/Divisors[num]]==2,AppendTo[found,num]];
Do[CheckPerfect[i],{i,1,2^25}];
found
```

gives back:

```Mathematica
{6, 28, 496, 8128, 33550336}
```

Final note; approximations of fractions to any precision can be found using the function N.


## Maxima


```maxima
/* Rational numbers are builtin */
a: 3 / 11;
3/11

b: 117 / 17;
117/17

a + b;
1338/187

a - b;
-1236/187

a * b;
351/187

a / b;
17/429

a^5;
243/161051

num(a);
3

denom(a);
11

ratnump(a);
true
```


=={{header|Modula-2}}==
<div style="text-align:right;font-size:7pt">''<nowiki>[</nowiki>This section is included from [[Arithmetic/Rational/Modula-2|a subpage]] and should be edited there, not here.<nowiki>]</nowiki>''</div>
{{:Arithmetic/Rational/Modula-2}}

=={{header|Modula-3}}==
<div style="text-align:right;font-size:7pt">''<nowiki>[</nowiki>This section is included from [[Arithmetic/Rational/Modula-3|a subpage]] and should be edited there, not here.<nowiki>]</nowiki>''</div>
{{:Arithmetic/Rational/Modula-3}}


## Nim


```nim
import math

proc `^`[T](base, exp: T): T =
  var (base, exp) = (base, exp)
  result = 1

  while exp != 0:
    if (exp and 1) != 0:
      result *= base
    exp = exp shr 1
    base *= base

proc gcd[T](u, v: T): T =
  if v != 0:
    gcd(v, u mod v)
  else:
    u.abs

proc lcm[T](a, b: T): T =
  a div gcd(a, b) * b

type Rational* = tuple[num, den: int64]

proc fromInt*(x: SomeInteger): Rational =
  result.num = x
  result.den = 1

proc frac*(x: var Rational) =
  let common = gcd(x.num, x.den)
  x.num = x.num div common
  x.den = x.den div common

proc `+` *(x, y: Rational): Rational =
  let common = lcm(x.den, y.den)
  result.num = common div x.den * x.num + common div y.den * y.num
  result.den = common
  result.frac

proc `+=` *(x: var Rational, y: Rational) =
  let common = lcm(x.den, y.den)
  x.num = common div x.den * x.num + common div y.den * y.num
  x.den = common
  x.frac

proc `-` *(x: Rational): Rational =
  result.num = -x.num
  result.den = x.den

proc `-` *(x, y: Rational): Rational =
  x + -y

proc `-=` *(x: var Rational, y: Rational) =
  x += -y

proc `*` *(x, y: Rational): Rational =
  result.num = x.num * y.num
  result.den = x.den * y.den
  result.frac

proc `*=` *(x: var Rational, y: Rational) =
  x.num *= y.num
  x.den *= y.den
  x.frac

proc reciprocal*(x: Rational): Rational =
  result.num = x.den
  result.den = x.num

proc `div`*(x, y: Rational): Rational =
  x * y.reciprocal

proc toFloat*(x: Rational): float =
  x.num.float / x.den.float

proc toInt*(x: Rational): int64 =
  x.num div x.den

proc cmp*(x, y: Rational): int =
  cmp x.toFloat, y.toFloat

proc `<` *(x, y: Rational): bool =
  x.toFloat < y.toFloat

proc `<=` *(x, y: Rational): bool =
  x.toFloat <= y.toFloat

proc abs*(x: Rational): Rational =
  result.num = abs x.num
  result.den = abs x.den

for candidate in 2'i64 .. <((2'i64)^19):
  var sum: Rational = (1'i64, candidate)
  for factor in 2'i64 .. pow(candidate.float, 0.5).int64:
    if candidate mod factor == 0:
      sum += (1'i64, factor) + (1'i64, candidate div factor)
  if sum.den == 1:
    echo "Sum of recipr. factors of ",candidate," = ",sum.num," exactly ",
      if sum.num == 1: "perfect!" else: ""
```

Output:

```txt
Sum of recipr. factors of 6 = 1 exactly perfect!
Sum of recipr. factors of 28 = 1 exactly perfect!
Sum of recipr. factors of 120 = 2 exactly
Sum of recipr. factors of 496 = 1 exactly perfect!
Sum of recipr. factors of 672 = 2 exactly
Sum of recipr. factors of 8128 = 1 exactly perfect!
Sum of recipr. factors of 30240 = 3 exactly
Sum of recipr. factors of 32760 = 3 exactly
Sum of recipr. factors of 523776 = 2 exactly
```


=={{header|Objective-C}}==
<div style="text-align:right;font-size:7pt">''<nowiki>[</nowiki>This section is included from [[Arithmetic/Rational/Objective-C|a subpage]] and should be edited there, not here.<nowiki>]</nowiki>''</div>
{{:Arithmetic/Rational/Objective-C}}


## OCaml

OCaml's Num library implements arbitrary-precision rational numbers:

```ocaml
#load "nums.cma";;
open Num;;

for candidate = 2 to 1 lsl 19 do
  let sum = ref (num_of_int 1 // num_of_int candidate) in
  for factor = 2 to truncate (sqrt (float candidate)) do
    if candidate mod factor = 0 then
      sum := !sum +/ num_of_int 1 // num_of_int factor
                  +/ num_of_int 1 // num_of_int (candidate / factor)
  done;
  if is_integer_num !sum then
    Printf.printf "Sum of recipr. factors of %d = %d exactly %s\n%!"
        candidate (int_of_num !sum) (if int_of_num !sum = 1 then "perfect!" else "")
done;;
```

[http://forge.ocamlcore.org/projects/pa-do/ Delimited overloading] can be used to make the arithmetic expressions more readable:

```ocaml
let () =
  for candidate = 2 to 1 lsl 19 do
    let sum = ref Num.(1 / of_int candidate) in
    for factor = 2 to truncate (sqrt (float candidate)) do
      if candidate mod factor = 0 then
        sum := Num.(!sum + 1 / of_int factor + of_int factor / of_int candidate)
    done;
    if Num.is_integer_num !sum then
      Printf.printf "Sum of recipr. factors of %d = %d exactly %s\n%!"
        candidate Num.(to_int !sum) (if Num.(!sum = 1) then "perfect!" else "")
  done
```


A type for rational numbers might be implemented like this:

First define the interface, hiding implementation details:

```ocaml
(* interface *)
module type RATIO =
   sig
     type t
     (* construct *)
     val frac : int -> int -> t
     val from_int : int -> t

     (* integer test *)
     val is_int : t -> bool

     (* output *)
     val to_string : t -> string

     (* arithmetic *)
     val cmp : t -> t -> int
     val ( +/ ) : t -> t -> t
     val ( -/ ) : t -> t -> t
     val ( */ ) : t -> t -> t
     val ( // ) : t -> t -> t
   end
```


then implement the module:

```ocaml
(* implementation conforming to signature *)
module Frac : RATIO =
   struct
      open Big_int

      type t = { num : big_int; den : big_int }

      (* short aliases for big_int values and functions *)
      let zero, one = zero_big_int, unit_big_int
      let big, to_int, eq = big_int_of_int, int_of_big_int, eq_big_int
      let (+~), (-~), ( *~) = add_big_int, sub_big_int, mult_big_int

      (* helper function *)
      let rec norm ({num=n;den=d} as k) =
         if lt_big_int d zero then
           norm {num=minus_big_int n;den=minus_big_int d}
         else
         let rec hcf a b =
           let q,r = quomod_big_int a b in
           if eq r zero then b else hcf b r in
         let f = hcf n d in
         if eq f one then k else
            let div = div_big_int in
            { num=div n f; den = div d f } (* inefficient *)

      (* public functions *)
      let frac a b = norm { num=big a; den=big b }

      let from_int a = norm { num=big a; den=one }

      let is_int {num=n; den=d} =
         eq d one ||
         eq (mod_big_int n d) zero

      let to_string ({num=n; den=d} as r) =
         let r1 = norm r in
         let str = string_of_big_int in
         if is_int r1 then
            str (r1.num)
         else
            str (r1.num) ^ "/" ^ str (r1.den)

      let cmp a b =
         let a1 = norm a and b1 = norm b in
         compare_big_int (a1.num*~b1.den) (b1.num*~a1.den)

      let ( */ ) {num=n1; den=d1} {num=n2; den=d2} =
         norm { num = n1*~n2; den = d1*~d2 }

      let ( // ) {num=n1; den=d1} {num=n2; den=d2} =
         norm { num = n1*~d2; den = d1*~n2 }

      let ( +/ ) {num=n1; den=d1} {num=n2; den=d2} =
         norm { num = n1*~d2 +~ n2*~d1; den = d1*~d2 }

      let ( -/ ) {num=n1; den=d1} {num=n2; den=d2} =
         norm { num = n1*~d2 -~ n2*~d1; den = d1*~d2 }
   end
```


Finally the use type defined by the module to perform the perfect number calculation:

```ocaml
(* use the module to calculate perfect numbers *)
let () =
   for i = 2 to 1 lsl 19 do
      let sum = ref (Frac.frac 1 i) in
      for factor = 2 to truncate (sqrt (float i)) do
         if i mod factor = 0 then
            Frac.(
            sum := !sum +/ frac 1 factor +/ frac 1 (i / factor)
            )
      done;
      if Frac.is_int !sum then
         Printf.printf "Sum of reciprocal factors of %d = %s exactly %s\n%!"
           i (Frac.to_string !sum) (if Frac.to_string !sum = "1" then "perfect!" else "")
   done
```

which produces this output:

 Sum of reciprocal factors of 6 = 1 exactly perfect!
 Sum of reciprocal factors of 28 = 1 exactly perfect!
 Sum of reciprocal factors of 120 = 2 exactly
 Sum of reciprocal factors of 496 = 1 exactly perfect!
 Sum of reciprocal factors of 672 = 2 exactly
 Sum of reciprocal factors of 8128 = 1 exactly perfect!
 Sum of reciprocal factors of 30240 = 3 exactly
 Sum of reciprocal factors of 32760 = 3 exactly
 Sum of reciprocal factors of 523776 = 2 exactly


## ooRexx


```ooRexx

loop candidate = 6 to 2**19
    sum = .fraction~new(1, candidate)
    max2 = rxcalcsqrt(candidate)~trunc

    loop factor = 2 to max2
        if candidate // factor == 0 then do
           sum += .fraction~new(1, factor)
           sum += .fraction~new(1, candidate / factor)
        end
    end
    if sum == 1 then say candidate "is a perfect number"
end

::class fraction inherit orderable
::method init
  expose numerator denominator
  use strict arg numerator, denominator = 1

  if denominator == 0 then raise syntax 98.900 array("Fraction denominator cannot be zero")

  -- if the denominator is negative, make the numerator carry the sign
  if denominator < 0 then do
      numerator = -numerator
      denominator = - denominator
  end


  -- find the greatest common denominator and reduce to
  -- the simplest form
  gcd = self~gcd(numerator~abs, denominator~abs)

  numerator /= gcd
  denominator /= gcd

-- fraction instances are immutable, so these are
-- read only attributes
::attribute numerator GET
::attribute denominator GET

-- calculate the greatest common denominator of a numerator/denominator pair
::method gcd private
  use arg x, y

  loop while y \= 0
      -- check if they divide evenly
      temp = x // y
      x = y
      y = temp
  end
  return x

-- calculate the least common multiple of a numerator/denominator pair
::method lcm private
  use arg x, y
  return x / self~gcd(x, y) * y

::method abs
  expose numerator denominator
  -- the denominator is always forced to be positive
  return self~class~new(numerator~abs, denominator)

::method reciprocal
  expose numerator denominator
  return self~class~new(denominator, numerator)

-- convert a fraction to regular Rexx number
::method toNumber
  expose numerator denominator

  if numerator == 0 then return 0
  return numerator/denominator

::method negative
  expose numerator denominator
  return self~class~new(-numerator, denominator)

::method add
  expose numerator denominator
  use strict arg other
  -- convert to a fraction if a regular number
  if \other~isa(.fraction) then other = self~class~new(other, 1)

  multiple = self~lcm(denominator, other~denominator)
  newa = numerator * multiple / denominator
  newb = other~numerator * multiple / other~denominator
  return self~class~new(newa + newb, multiple)

::method subtract
  use strict arg other
  return self + (-other)

::method times
  expose numerator denominator
  use strict arg other
  -- convert to a fraction if a regular number
  if \other~isa(.fraction) then other = self~class~new(other, 1)
  return self~class~new(numerator * other~numerator, denominator * other~denominator)

::method divide
  use strict arg other
  -- convert to a fraction if a regular number
  if \other~isa(.fraction) then other = self~class~new(other, 1)
  -- and multiply by the reciprocal
  return self * other~reciprocal

-- compareTo method used by the orderable interface to implement
-- the operator methods
::method compareTo
  expose numerator denominator
  -- convert to a fraction if a regular number
  if \other~isa(.fraction) then other = self~class~new(other, 1)

  return (numerator * other~denominator - denominator * other~numerator)~sign

-- we still override "==" and "\==" because we want to bypass the
-- checks for not being an instance of the class
::method "=="
  expose numerator denominator
  use strict arg other

  -- convert to a fraction if a regular number
  if \other~isa(.fraction) then other = self~class~new(other, 1)
  -- Note:  these are numeric comparisons, so we're using the "="
  -- method so those are handled correctly
  return numerator = other~numerator & denominator = other~denominator

::method "\=="
  use strict arg other
  return \self~"\=="(other)

-- some operator overrides -- these only work if the left-hand-side of the
-- subexpression is a quaternion
::method "*"
  forward message("TIMES")

::method "/"
  forward message("DIVIDE")

::method "-"
  -- need to check if this is a prefix minus or a subtract
  if arg() == 0 then
      forward message("NEGATIVE")
  else
      forward message("SUBTRACT")

::method "+"
  -- need to check if this is a prefix plus or an addition
  if arg() == 0 then
      return self  -- we can return this copy since it is imutable
  else
      forward message("ADD")

::method string
  expose numerator denominator
  if denominator == 1 then return numerator
  return numerator"/"denominator

-- override hashcode for collection class hash uses
::method hashCode
  expose numerator denominator
  return numerator~hashcode~bitxor(numerator~hashcode)

::requires rxmath library

```

Output:

```txt

6 is a perfect number
28 is a perfect number
496 is a perfect number
8128 is a perfect number

```



## PARI/GP

Pari handles rational arithmetic natively.

```parigp
for(n=2,1<<19,
  s=0;
  fordiv(n,d,s+=1/d);
  if(s==2,print(n))
)
```



## Perl

Perl's <code>Math::BigRat</code> core module implements arbitrary-precision rational numbers. The <code>bigrat</code> pragma can be used to turn on transparent BigRat support:

```perl
use bigrat;

foreach my $candidate (2 .. 2**19) {
    my $sum = 1 / $candidate;
    foreach my $factor (2 .. sqrt($candidate)+1) {
        if ($candidate % $factor == 0) {
            $sum += 1 / $factor + 1 / ($candidate / $factor);
        }
    }
    if ($sum->denominator() == 1) {
        print "Sum of recipr. factors of $candidate = $sum exactly ", ($sum == 1 ? "perfect!" : ""), "\n";
    }
}
```



## Perl 6

{{Works with|rakudo|2016.08}}
Perl 6 supports rational arithmetic natively.

```perl6
(2..2**19).hyper.map: -> $candidate {
    my $sum = 1 / $candidate;
    for 2 .. ceiling(sqrt($candidate)) -> $factor {
        if $candidate %% $factor {
            $sum += 1 / $factor + 1 / ($candidate / $factor);
        }
    }
    if $sum.nude[1] == 1 {
        say "Sum of reciprocal factors of $candidate = $sum exactly", ($sum == 1 ?? ", perfect!" !! ".");
    }
}
```

Note also that ordinary decimal literals are stored as Rats, so the following loop always stops exactly on 10 despite 0.1 not being exactly representable in floating point:

```perl6
for 1.0, 1.1, 1.2 ... 10 { .say }
```

The arithmetic is all done in rationals, which are converted to floating-point just before display so that people don't have to puzzle out what 53/10 means.


## Phix

{{Trans|Tcl}}
Phix does not support operator overloading (I am strongly opposed to such nonsense), nor does it have a fraction library, but it might look a bit like this.

```Phix
without warning  -- (several unused routines in this code)

constant NUM = 1, DEN = 2

type frac(object r)
    return sequence(r) and integer(r[NUM]) and integer(r[DEN]) and length(r)=2
end type

function normalise(object n, atom d=0)
atom g
    if sequence(n) then
        {n,d} = n
    end if
    if d<0 then
        n = -n
        d = -d
    end if
    g = gcd(n,d)
    return {n/g,d/g}
end function

function frac_new(integer n,d=1)
    return normalise(n,d)
end function

function frac_abs(frac r)
    return {abs(r[NUM]),r[DEN]}
end function

function frac_inv(frac r)
    return reverse(r)
end function

function frac_add(frac a, frac b)
integer {an,ad} = a,
        {bn,bd} = b
    return normalise(an*bd+bn*ad,ad*bd)
end function

function frac_sub(frac a, frac b)
integer {an,ad} = a,
        {bn,bd} = b
    return normalise(an*bd-bn*ad,ad*bd)
end function

function frac_mul(frac a, frac b)
integer {an,ad} = a,
        {bn,bd} = b
    return normalise(an*bn,ad*bd)
end function

function frac_div(frac a, frac b)
integer {an,ad} = a,
        {bn,bd} = b
    return normalise(an*bd,ad*bn)
end function

function frac_eq(frac a, frac b)
    return a==b
end function

function frac_ne(frac a, frac b)
    return a!=b
end function

function frac_lt(frac a, frac b)
    return frac_sub(a,b)[NUM]<0
end function

function frac_gt(frac a, frac b)
    return frac_sub(a,b)[NUM]>0
end function

function frac_le(frac a, frac b)
    return frac_sub(a,b)[NUM]<=0
end function

function frac_ge(frac a, frac b)
    return frac_sub(a,b)[NUM]>=0
end function

function is_perfect(integer num)
frac sum = frac_new(0)
sequence f = factors(num,1)
    for i=1 to length(f) do
        sum = frac_add(sum,frac_new(1,f[i]))
    end for
    return frac_eq(sum,frac_new(2))
end function

procedure get_perfect_numbers()
atom t0 = time()
    for i=2 to power(2,19) do
        if is_perfect(i) then
            printf(1,"perfect: %d\n",i)
        end if
    end for
    printf(1,"elapsed: %3.2f seconds\n",time()-t0)

    integer pn5 = power(2,12)*(power(2,13)-1) -- 5th perfect number
    if is_perfect(pn5) then
        printf(1,"perfect: %d\n",pn5)
    end if
end procedure

get_perfect_numbers()
```

{{out}}

```txt

perfect: 6
perfect: 28
perfect: 496
perfect: 8128
elapsed: 13.56 seconds
perfect: 33550336

```


###  mpq

{{libheader|mpfr}}
Turned out to be slightly slower than native, but worth it for large number support.

See also [[Bernoulli_numbers#Phix|Bernoulli_numbers]] for another example of mpqs in action.

```Phix
include builtins/mpfr.e
function is_perfect(integer num)
mpq tot = mpq_init(),
    fth = mpq_init()
sequence f = factors(num,1)
    for i=1 to length(f) do
        mpq_set_si(fth,1,f[i])
        mpq_add(tot,tot,fth)
    end for
    return mpq_cmp_si(tot,2,1)=0
end function

procedure get_perfect_numbers()
atom t0 = time()
    for i=2 to power(2,19) do
        if is_perfect(i) then
            printf(1,"perfect: %d\n",i)
        end if
    end for
    printf(1,"elapsed: %3.2f seconds\n",time()-t0)

    integer pn5 = power(2,12)*(power(2,13)-1) -- 5th perfect number
    if is_perfect(pn5) then
        printf(1,"perfect: %d\n",pn5)
    end if
end procedure

get_perfect_numbers()
```

{{out}}

```txt

perfect: 6
perfect: 28
perfect: 496
perfect: 8128
elapsed: 17.31 seconds
perfect: 33550336

```



## PicoLisp


```PicoLisp
(load "@lib/frac.l")

(for (N 2  (> (** 2 19) N)  (inc N))
   (let (Sum (frac 1 N)  Lim (sqrt N))
      (for (F 2  (>= Lim F) (inc F))
         (when (=0 (% N F))
            (setq Sum
               (f+ Sum
                  (f+ (frac 1 F) (frac 1 (/ N F))) ) ) ) )
      (when (= 1 (cdr Sum))
         (prinl
            "Perfect " N
            ", sum is " (car Sum)
            (and (= 1 (car Sum)) ": perfect") ) ) ) )
```

{{out}}

```txt
Perfect 6, sum is 1: perfect
Perfect 28, sum is 1: perfect
Perfect 120, sum is 2
Perfect 496, sum is 1: perfect
Perfect 672, sum is 2
Perfect 8128, sum is 1: perfect
Perfect 30240, sum is 3
Perfect 32760, sum is 3
Perfect 523776, sum is 2
```



## PL/I


```pli
*process source attributes xref or(!);
 arat: Proc Options(main);
 /*--------------------------------------------------------------------
 * Rational Arithmetic
 * (Mis)use the Complex data type to represent fractions
 * real(x) is used as numerator
 * imag(x) is used as denominator
 * Output:
 * a=-3/7 b=9/2
 * a*b=-27/14
 * a+b=57/14
 * a-b=-69/14
 * a/b=-2/21
 * -3/7<9/2
 * 9/2>-3/7
 * -3/7=-3/7
 * 26.01.2015 handle 0/0
 *-------------------------------------------------------------------*/
 Dcl (abs,imag,mod,real,sign,trim) Builtin;
 Dcl sysprint Print;
 Dcl (candidate,max2,factor) Dec Fixed(15);
 Dcl sum complex Dec Fixed(15);
 Dcl one complex Dec Fixed(15);

 one=mk_fr(1,1);
 Put Edit('First solve the task at hand')(Skip,a);
 Do candidate = 2 to 10000;
   sum = mk_fr(1, candidate);
   max2 = sqrt(candidate);
   Do factor = 2 to max2;
     If mod(candidate,factor)=0 Then Do;
       sum=fr_add(sum,mk_fr(1,factor));
       sum=fr_add(sum,mk_fr(1,candidate/factor));
       End;
     End;
   If fr_cmp(sum,one)='=' Then Do;
     Put Edit(candidate,' is a perfect number')(Skip,f(7),a);
     Do factor = 2 to candidate-1;
       If mod(candidate,factor)=0 Then
         Put Edit(factor)(f(5));
       End;
     End;
   End;

 Put Edit('','Then try a few things')(Skip,a);
 Dcl a Complex Dec Fixed(15);
 Dcl b Complex Dec Fixed(15);
 Dcl p Complex Dec Fixed(15);
 Dcl s Complex Dec Fixed(15);
 Dcl d Complex Dec Fixed(15);
 Dcl q Complex Dec Fixed(15);
 Dcl zero Complex Dec Fixed(15);
 zero=mk_fr(0,1); Put Edit('zero=',fr_rep(zero))(Skip,2(a));
 a=mk_fr(0,0);    Put Edit('a=',fr_rep(a))(Skip,2(a));
 /*--------------------------------------------------------------------
 a=mk_fr(-3333,0); Put Edit('a=',fr_rep(a))(Skip,2(a));
 =>  Request mk_fr(-3333,0)
     Denominator must not be 0
     IBM0280I  ONCODE=0009  The ERROR condition was raised
               by a SIGNAL statement.
        At offset +00000276 in procedure with entry FT
 *-------------------------------------------------------------------*/
 a=mk_fr(0,3333); Put Edit('a=',fr_rep(a))(Skip,2(a));
 Put Edit('-3,7')(Skip,a);
 a=mk_fr(-3,7);
 b=mk_fr(9,2);
 p=fr_mult(a,b);
 s=fr_add(a,b);
 d=fr_sub(a,b);
 q=fr_div(a,b);
 r=fr_div(b,a);
 Put Edit('a=',fr_rep(a))(Skip,2(a));
 Put Edit('b=',fr_rep(b))(Skip,2(a));
 Put Edit('a*b=',fr_rep(p))(Skip,2(a));
 Put Edit('a+b=',fr_rep(s))(Skip,2(a));
 Put Edit('a-b=',fr_rep(d))(Skip,2(a));
 Put Edit('a/b=',fr_rep(q))(Skip,2(a));
 Put Edit('b/a=',fr_rep(r))(Skip,2(a));
 Put Edit(fr_rep(a),fr_cmp(a,b),fr_rep(b))(Skip,3(a));
 Put Edit(fr_rep(b),fr_cmp(b,a),fr_rep(a))(Skip,3(a));
 Put Edit(fr_rep(a),fr_cmp(a,a),fr_rep(a))(Skip,3(a));

 mk_fr: Proc(n,d) Recursive Returns(Dec Fixed(15) Complex);
 /*--------------------------------------------------------------------
 * make a Complex number
 * normalize and cancel
 *-------------------------------------------------------------------*/
 Dcl (n,d) Dec Fixed(15);
 Dcl (na,da) Dec Fixed(15);
 Dcl res Dec Fixed(15) Complex;
 Dcl x   Dec Fixed(15);
 na=abs(n);
 da=abs(d);
 Select;
   When(n=0) Do;
     real(res)=0;
     imag(res)=1;
     End;
   When(d=0) Do;
     Put Edit('Request mk_fr('!!n_rep(n)!!','!!n_rep(d)!!')')
             (Skip,a);
     Put Edit('Denominator must not be 0')(Skip,a);
     Signal error;
     End;
   Otherwise Do;
     x=gcd(na,da);
     real(res)=sign(n)*sign(d)*na/x;
     imag(res)=da/x;
     End;
   End;
 Return(res);
 End;

 fr_add: Proc(a,b) Returns(Dec Fixed(15) Complex);
 /*--------------------------------------------------------------------
 * add 'fractions' a and b
 *-------------------------------------------------------------------*/
 Dcl (a,b,res)     Dec Fixed(15) Complex;
 Dcl (an,ad,bn,bd) Dec Fixed(15);
 Dcl (rd,rn)       Dec Fixed(15);
 Dcl x             Dec Fixed(15);
 an=real(a);
 ad=imag(a);
 bn=real(b);
 bd=imag(b);
 rd=ad*bd;
 rn=an*bd+bn*ad;
 x=gcd(rd,rn);
 real(res)=rn/x;
 imag(res)=rd/x;
 Return(res);
 End;

 fr_sub: Proc(a,b) Returns(Dec Fixed(15) Complex);
 /*--------------------------------------------------------------------
 * subtract 'fraction' b from a
 *-------------------------------------------------------------------*/
 Dcl (a,b) Dec Fixed(15) Complex;
 Dcl b2    Dec Fixed(15) Complex;
 real(b2)=-real(b);
 imag(b2)=imag(b);
 Return(fr_add(a,b2));
 End;

 fr_mult: Proc(a,b) Returns(Dec Fixed(15) Complex);
 /*--------------------------------------------------------------------
 * multiply 'fractions' a and b
 *-------------------------------------------------------------------*/
 Dcl (a,b,res) Dec Fixed(15) Complex;
 real(res)=real(a)*real(b);
 imag(res)=imag(a)*imag(b);
 Return(res);
 End;

 fr_div: Proc(a,b) Returns(Dec Fixed(15) Complex);
 /*--------------------------------------------------------------------
 * divide 'fraction' a by b
 *-------------------------------------------------------------------*/
 Dcl (a,b) Dec Fixed(15) Complex;
 Dcl b2    Dec Fixed(15) Complex;
 real(b2)=imag(b);
 imag(b2)=real(b);
 If real(a)=0 & real(b)=0 Then
   Return(mk_fr(1,1));
 Return(fr_mult(a,b2));
 End;

 fr_cmp: Proc(a,b) Returns(char(1));
 /*--------------------------------------------------------------------
 * compare 'fractions' a and b
 *-------------------------------------------------------------------*/
 Dcl (a,b)         Dec Fixed(15) Complex;
 Dcl (an,ad,bn,bd) Dec Fixed(15);
 Dcl (a2,b2)       Dec Fixed(15);
 Dcl (rd)          Dec Fixed(15);
 Dcl res           Char(1);
 an=real(a);
 ad=imag(a);
 If ad=0 Then Do;
   Put Edit('ad=',ad,'candidate=',candidate)(Skip,a,f(10));
   Signal Error;
   End;
 bn=real(b);
 bd=imag(b);
 rd=ad*bd;
 a2=abs(an*bd)*sign(an)*sign(ad);
 b2=abs(bn*ad)*sign(bn)*sign(bd);
 Select;
   When(a2<b2) res='<';
   When(a2>b2) res='>';
   Otherwise Do;
     res='=';
     End;
   End;
 Return(res);
 End;

 fr_rep: Proc(f) Returns(char(15) Var);
 /*--------------------------------------------------------------------
 * Return the representation of 'fraction' f
 *-------------------------------------------------------------------*/
 Dcl f     Dec Fixed(15) Complex;
 Dcl res   Char(15) Var;
 Dcl (n,d) Pic'(14)Z9';
 Dcl x     Dec Fixed(15);
 Dcl s     Dec Fixed(15);
 n=abs(real(f));
 d=abs(imag(f));
 x=gcd(n,d);
 s=sign(real(f))*sign(imag(f));
 res=trim(n/x)!!'/'!!trim(d/x);
 If s<0 Then
   res='-'!!res;
 Return(res);
 End;

 n_rep: Proc(x) Returns(char(15) Var);
 /*--------------------------------------------------------------------
 * Return the representation of x
 *-------------------------------------------------------------------*/
 Dcl x     Dec Fixed(15);
 Dcl res   Char(15) Var;
 Put String(res) List(x);
 res=trim(res);
 Return(res);
 End;

 gcd: Proc(a,b) Returns(Dec Fixed(15)) Recursive;
 /*--------------------------------------------------------------------
 * Compute the greatest common divisor
 *-------------------------------------------------------------------*/
 Dcl (a,b) Dec Fixed(15) Nonassignable;
 If b=0 then Return (abs(a));
 Return(gcd(abs(b),mod(abs(a),abs(b))));
 End gcd;

 lcm: Proc(a,b) Returns(Dec Fixed(15));
 /*--------------------------------------------------------------------
 * Compute the least common multiple
 *-------------------------------------------------------------------*/
 Dcl (a,b) Dec Fixed(15) Nonassignable;
 if a=0 ! b=0 then Return (0);
 Return(abs(a*b)/gcd(a,b));
 End lcm;

 End;
```

{{out}}

```txt
First solve the task at hand
      6 is a perfect number    2    3
     28 is a perfect number    2    4    7   14
    496 is a perfect number    2    4    8   16   31   62  124  248
   8128 is a perfect number    2    4    8   16   32   64  127  254  508 1016 2032 4064

Then try a few things
zero=0/1
a=0/1
a=0/1
-3,7
a=-3/7
b=9/2
a*b=-27/14
a+b=57/14
a-b=-69/14
a/b=-2/21
b/a=1/0
-3/7<9/2
9/2>-3/7
-3/7=-3/7
```



## Python

{{works with|Python|3.0}}
Python 3's standard library already implements a Fraction class:

```python
from fractions import Fraction

for candidate in range(2, 2**19):
  sum = Fraction(1, candidate)
  for factor in range(2, int(candidate**0.5)+1):
    if candidate % factor == 0:
      sum += Fraction(1, factor) + Fraction(1, candidate // factor)
  if sum.denominator == 1:
    print("Sum of recipr. factors of %d = %d exactly %s" %
           (candidate, int(sum), "perfect!" if sum == 1 else ""))
```

It might be implemented like this:

```python
def lcm(a, b):
    return a // gcd(a,b) * b

def gcd(u, v):
    return gcd(v, u%v) if v else abs(u)

class Fraction:
    def __init__(self, numerator, denominator):
        common = gcd(numerator, denominator)
        self.numerator = numerator//common
        self.denominator = denominator//common
    def __add__(self, frac):
        common = lcm(self.denominator, frac.denominator)
        n = common // self.denominator * self.numerator + common // frac.denominator * frac.numerator
        return Fraction(n, common)
    def __sub__(self, frac):
        return self.__add__(-frac)
    def __neg__(self):
        return Fraction(-self.numerator, self.denominator)
    def __abs__(self):
        return Fraction(abs(self.numerator), abs(self.denominator))
    def __mul__(self, frac):
        return Fraction(self.numerator * frac.numerator, self.denominator * frac.denominator)
    def __div__(self, frac):
        return self.__mul__(frac.reciprocal())
    def reciprocal(self):
        return Fraction(self.denominator, self.numerator)
    def __cmp__(self, n):
        return int(float(self) - float(n))
    def __float__(self):
        return float(self.numerator / self.denominator)
    def __int__(self):
        return (self.numerator // self.denominator)
```



## Racket


Racket always had support for exact rational numbers as a native numeric type.

Example:

```racket

-> (* 1/7 14)
2

```



## REXX


```rexx
/*REXX program implements a reasonably complete  rational arithmetic  (using fractions).*/
L=length(2**19 - 1)                              /*saves time by checking even numbers. */
     do j=2  by 2  to 2**19 - 1;       s=0       /*ignore unity (which can't be perfect)*/
     mostDivs=eDivs(j);                @=        /*obtain divisors>1; zero sum; null @. */
       do k=1  for  words(mostDivs)              /*unity isn't return from  eDivs  here.*/
       r='1/'word(mostDivs, k);        @=@ r;         s=$fun(r, , s)
       end   /*k*/
     if s\==1  then iterate                      /*Is sum not equal to unity?   Skip it.*/
     say 'perfect number:'       right(j, L)       "   fractions:"            @
     end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
$div: procedure;  parse arg x;   x=space(x,0);   f= 'fractional division'
      parse var x n '/' d;       d=p(d 1)
      if d=0               then call err  'division by zero:'            x
      if \datatype(n,'N')  then call err  'a non─numeric numerator:'     x
      if \datatype(d,'N')  then call err  'a non─numeric denominator:'   x
      return n/d
/*──────────────────────────────────────────────────────────────────────────────────────*/
$fun: procedure;  parse arg z.1,,z.2 1 zz.2;    arg ,op;  op=p(op '+')
F= 'fractionalFunction';        do j=1  for 2;  z.j=translate(z.j, '/', "_");   end  /*j*/
if abbrev('ADD'      , op)                               then op= "+"
if abbrev('DIVIDE'   , op)                               then op= "/"
if abbrev('INTDIVIDE', op, 4)                            then op= "÷"
if abbrev('MODULUS'  , op, 3) | abbrev('MODULO', op, 3)  then op= "//"
if abbrev('MULTIPLY' , op)                               then op= "*"
if abbrev('POWER'    , op)                               then op= "^"
if abbrev('SUBTRACT' , op)                               then op= "-"
if z.1==''                                               then z.1= (op\=="+" & op\=='-')
if z.2==''                                               then z.2= (op\=="+" & op\=='-')
z_=z.2
                                                 /* [↑]  verification of both fractions.*/
  do j=1  for 2
  if pos('/', z.j)==0    then z.j=z.j"/1";         parse var  z.j  n.j  '/'  d.j
  if \datatype(n.j,'N')  then call err  'a non─numeric numerator:'     n.j
  if \datatype(d.j,'N')  then call err  'a non─numeric denominator:'   d.j
  if d.j=0               then call err  'a denominator of zero:'       d.j
                                               n.j=n.j/1;          d.j=d.j/1
             do  while \datatype(n.j,'W');     n.j=(n.j*10)/1;     d.j=(d.j*10)/1
             end  /*while*/                      /* [↑]   {xxx/1}  normalizes a number. */
  g=gcd(n.j, d.j);    if g=0  then iterate;  n.j=n.j/g;          d.j=d.j/g
  end    /*j*/

 select
 when op=='+' | op=='-' then do;  l=lcm(d.1,d.2);    do j=1  for 2;  n.j=l*n.j/d.j;  d.j=l
                                                     end   /*j*/
                                  if op=='-'  then n.2= -n.2;        t=n.1 + n.2;    u=l
                             end
 when op=='**' | op=='↑'  |,
      op=='^'  then do;  if \datatype(z_,'W')  then call err 'a non─integer power:'  z_
                    t=1;  u=1;     do j=1  for abs(z_);  t=t*n.1;  u=u*d.1
                                   end   /*j*/
                    if z_<0  then parse value   t  u   with   u  t      /*swap  U and T */
                    end
 when op=='/'  then do;      if n.2=0   then call err  'a zero divisor:'   zz.2
                             t=n.1*d.2;    u=n.2*d.1
                    end
 when op=='÷'  then do;      if n.2=0   then call err  'a zero divisor:'   zz.2
                             t=trunc($div(n.1 '/' d.1));    u=1
                    end                           /* [↑]  this is integer division.     */
 when op=='//' then do;      if n.2=0   then call err  'a zero divisor:'   zz.2
                    _=trunc($div(n.1 '/' d.1));     t=_ - trunc(_) * d.1;            u=1
                    end                          /* [↑]  modulus division.              */
 when op=='ABS'  then do;   t=abs(n.1);       u=abs(d.1);        end
 when op=='*'    then do;   t=n.1 * n.2;      u=d.1 * d.2;       end
 when op=='EQ' | op=='='                then return $div(n.1 '/' d.1)  = fDiv(n.2 '/' d.2)
 when op=='NE' | op=='\=' | op=='╪' | ,
                            op=='¬='    then return $div(n.1 '/' d.1) \= fDiv(n.2 '/' d.2)
 when op=='GT' | op=='>'                then return $div(n.1 '/' d.1) >  fDiv(n.2 '/' d.2)
 when op=='LT' | op=='<'                then return $div(n.1 '/' d.1) <  fDiv(n.2 '/' d.2)
 when op=='GE' | op=='≥'  | op=='>='    then return $div(n.1 '/' d.1) >= fDiv(n.2 '/' d.2)
 when op=='LE' | op=='≤'  | op=='<='    then return $div(n.1 '/' d.1) <= fDiv(n.2 '/' d.2)
 otherwise       call err  'an illegal function:'   op
 end   /*select*/

if t==0  then return 0;            g=gcd(t, u);             t=t/g;                   u=u/g
if u==1  then return t
              return t'/'u
/*──────────────────────────────────────────────────────────────────────────────────────*/
eDivs: procedure; parse arg x 1 b,a
         do j=2  while j*j<x;       if x//j\==0  then iterate;   a=a j;   b=x%j b;     end
       if j*j==x  then return a j b;                                            return a b
/*───────────────────────────────────────────────────────────────────────────────────────────────────*/
err:   say;   say '***error*** '    f     " detected"   arg(1);    say;         exit 13
gcd:   procedure; parse arg x,y; if x=0  then return y;  do until _==0; _=x//y; x=y; y=_; end; return x
lcm:   procedure; parse arg x,y; if y=0  then return 0; x=x*y/gcd(x, y);        return x
p:     return word( arg(1), 1)
```

Programming note:   the   '''eDivs, gcd, lcm'''   functions are optimized functions for this program only.

'''output'''

```txt

perfect number:      6    fractions:  1/2 1/3 1/6
perfect number:     28    fractions:  1/2 1/4 1/7 1/14 1/28
perfect number:    496    fractions:  1/2 1/4 1/8 1/16 1/31 1/62 1/124 1/248 1/496
perfect number:   8128    fractions:  1/2 1/4 1/8 1/16 1/32 1/64 1/127 1/254 1/508 1/1016 1/2032 1/4064 1/8128

```



## Ruby

Ruby has a Rational class in it's core since 1.9. Before that it was in standard library:

```ruby
require 'rational' #Only needed in Ruby < 1.9

for candidate in 2 .. 2**19
  sum = Rational(1, candidate)
  for factor in 2 ... candidate**0.5
    if candidate % factor == 0
      sum += Rational(1, factor) + Rational(1, candidate / factor)
    end
  end
  if sum.denominator == 1
    puts "Sum of recipr. factors of %d = %d exactly %s" %
           [candidate, sum.to_i, sum == 1 ? "perfect!" : ""]
  end
end
```

{{out}}

```txt

Sum of recipr. factors of 6 = 1 exactly perfect!
Sum of recipr. factors of 28 = 1 exactly perfect!
Sum of recipr. factors of 120 = 2 exactly
Sum of recipr. factors of 496 = 1 exactly perfect!
Sum of recipr. factors of 672 = 2 exactly
Sum of recipr. factors of 8128 = 1 exactly perfect!
Sum of recipr. factors of 30240 = 3 exactly
Sum of recipr. factors of 32760 = 3 exactly
Sum of recipr. factors of 523776 = 2 exactly

```



## Rust


```rust
use std::cmp::Ordering;
use std::ops::{Add, AddAssign, Sub, SubAssign, Mul, MulAssign, Div, DivAssign, Neg};

fn gcd(a: i64, b: i64) -> i64 {
    match b {
        0 => a,
        _ => gcd(b, a % b),
    }
}

fn lcm(a: i64, b: i64) -> i64 {
    a / gcd(a, b) * b
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Ord)]
pub struct Rational {
    numerator: i64,
    denominator: i64,
}

impl Rational {
    fn new(numerator: i64, denominator: i64) -> Self {
        let divisor = gcd(numerator, denominator);
        Rational {
            numerator: numerator / divisor,
            denominator: denominator / divisor,
        }
    }
}

impl Add for Rational {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        let multiplier = lcm(self.denominator, other.denominator);
        Rational::new(self.numerator * multiplier / self.denominator +
                      other.numerator * multiplier / other.denominator,
                      multiplier)
    }
}

impl AddAssign for Rational {
    fn add_assign(&mut self, other: Self) {
        *self = *self + other;
    }
}

impl Sub for Rational {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        self + -other
    }
}

impl SubAssign for Rational {
    fn sub_assign(&mut self, other: Self) {
        *self = *self - other;
    }
}

impl Mul for Rational {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        Rational::new(self.numerator * other.numerator,
                      self.denominator * other.denominator)
    }
}

impl MulAssign for Rational {
    fn mul_assign(&mut self, other: Self) {
        *self = *self * other;
    }
}

impl Div for Rational {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        self *
        Rational {
            numerator: other.denominator,
            denominator: other.numerator,
        }
    }
}

impl DivAssign for Rational {
    fn div_assign(&mut self, other: Self) {
        *self = *self / other;
    }
}

impl Neg for Rational {
    type Output = Self;

    fn neg(self) -> Self {
        Rational {
            numerator: -self.numerator,
            denominator: self.denominator,
        }
    }
}

impl PartialOrd for Rational {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        (self.numerator * other.denominator).partial_cmp(&(self.denominator * other.numerator))
    }
}

impl<T: Into<i64>> From<T> for Rational {
    fn from(value: T) -> Self {
        Rational::new(value.into(), 1)
    }
}

fn main() {
    let max = 1 << 19;
    for candidate in 2..max {
        let mut sum = Rational::new(1, candidate);
        for factor in 2..(candidate as f64).sqrt().ceil() as i64 {
            if candidate % factor == 0 {
                sum += Rational::new(1, factor);
                sum += Rational::new(1, candidate / factor);
            }
        }

        if sum == 1.into() {
            println!("{} is perfect", candidate);
        }
    }
}

```



## Scheme

Scheme has native rational numbers.
{{works with|Scheme|R5RS}}

```scheme
; simply prints all the perfect numbers
(do ((candidate 2 (+ candidate 1))) ((>= candidate (expt 2 19)))
  (let ((sum (/ 1 candidate)))
    (do ((factor 2 (+ factor 1))) ((>= factor (sqrt candidate)))
      (if (= 0 (modulo candidate factor))
          (set! sum (+ sum (/ 1 factor) (/ factor candidate)))))
    (if (= 1 (denominator sum))
        (begin (display candidate) (newline)))))
```

It might be implemented like this:

[insert implementation here]


## Scala


```scala
class Rational(n: Long, d:Long) extends Ordered[Rational]
{
   require(d!=0)
   private val g:Long = gcd(n, d)
   val numerator:Long = n/g
   val denominator:Long = d/g

   def this(n:Long)=this(n,1)

   def +(that:Rational):Rational=new Rational(
      numerator*that.denominator + that.numerator*denominator,
      denominator*that.denominator)

   def -(that:Rational):Rational=new Rational(
      numerator*that.denominator - that.numerator*denominator,
      denominator*that.denominator)

   def *(that:Rational):Rational=
      new Rational(numerator*that.numerator, denominator*that.denominator)

   def /(that:Rational):Rational=
      new Rational(numerator*that.denominator, that.numerator*denominator)

   def unary_~ :Rational=new Rational(denominator, numerator)

   def unary_- :Rational=new Rational(-numerator, denominator)

   def abs :Rational=new Rational(Math.abs(numerator), Math.abs(denominator))

   override def compare(that:Rational):Int=
      (this.numerator*that.denominator-that.numerator*this.denominator).toInt

   override def toString()=numerator+"/"+denominator

   private def gcd(x:Long, y:Long):Long=
      if(y==0) x else gcd(y, x%y)
}

object Rational
{
   def apply(n: Long, d:Long)=new Rational(n,d)
   def apply(n:Long)=new Rational(n)
   implicit def longToRational(i:Long)=new Rational(i)
}
```



```scala
def find_perfects():Unit=
{
   for (candidate <- 2 until 1<<19)
   {
      var sum= ~Rational(candidate)
      for (factor <- 2 until (Math.sqrt(candidate)+1).toInt)
      {
         if (candidate%factor==0)
            sum+= ~Rational(factor)+ ~Rational(candidate/factor)
      }

      if (sum.denominator==1 && sum.numerator==1)
         printf("Perfect number %d sum is %s\n", candidate, sum)
   }
}
```



## Seed7

The library [http://seed7.sourceforge.net/libraries/rational.htm rational.s7i] defines the type
[http://seed7.sourceforge.net/manual/types.htm#rational rational], which supports the required functionality.
Rational numbers are based on the type [http://seed7.sourceforge.net/manual/types.htm#integer integer].
For rational numbers, which are based on integers with unlimited precision, use
[http://seed7.sourceforge.net/manual/types.htm#bigRational bigRational], which is defined
in the library [http://seed7.sourceforge.net/libraries/bigrat.htm bigrat.s7i].


```seed7
$ include "seed7_05.s7i";
  include "rational.s7i";

const func boolean: isPerfect (in integer: candidate) is func
  result
    var boolean: isPerfect is FALSE;
  local
    var integer: divisor is 0;
    var rational: sum is rational.value;
  begin
    sum := 1 / candidate;
    for divisor range 2 to sqrt(candidate) do
      if candidate mod divisor = 0 then
        sum +:= 1 / divisor + 1 / (candidate div divisor);
      end if;
    end for;
    isPerfect := sum = rat(1);
  end func;

const proc: main is func
  local
    var integer: candidate is 0;
  begin
    for candidate range 2 to 2 ** 19 - 1 do
      if isPerfect(candidate) then
        writeln(candidate <& " is perfect");
      end if;
    end for;
  end func;
```


{{out}}

```txt

6 is perfect
28 is perfect
496 is perfect
8128 is perfect

```



## Sidef

Sidef has built-in support for rational numbers.

```ruby
for n in (1 .. 2**19) {
    var frac = 0

    n.divisors.each {|d|
        frac += 1/d
    }

    if (frac.is_int) {
        say "Sum of reciprocal divisors of #{n} = #{frac} exactly #{
            frac == 2 ? '- perfect!' : ''
        }"
    }
}
```

{{out}}

```txt

Sum of reciprocal divisors of 1 = 1 exactly
Sum of reciprocal divisors of 6 = 2 exactly - perfect!
Sum of reciprocal divisors of 28 = 2 exactly - perfect!
Sum of reciprocal divisors of 120 = 3 exactly
Sum of reciprocal divisors of 496 = 2 exactly - perfect!
Sum of reciprocal divisors of 672 = 3 exactly
Sum of reciprocal divisors of 8128 = 2 exactly - perfect!
Sum of reciprocal divisors of 30240 = 4 exactly
Sum of reciprocal divisors of 32760 = 4 exactly
Sum of reciprocal divisors of 523776 = 3 exactly

```



## Slate

Slate uses infinite-precision fractions transparently.

```slate
54 / 7.
20 reciprocal.
(5 / 6) reciprocal.
(5 / 6) as: Float.
```



## Smalltalk

Smalltalk uses ''naturally'' and transparently fractions (through the class Fraction):

```txt
st> 54/7
54/7
st> 54/7 + 1
61/7
st> 54/7 < 50
true
st> 20 reciprocal
1/20
st> (5/6) reciprocal
6/5
st> (5/6) asFloat
0.8333333333333334

```

{{works with|GNU Smalltalk}}

```smalltalk
| sum |
2 to: (2 raisedTo: 19) do: [ :candidate |
  sum := candidate reciprocal.
  2 to: (candidate sqrt) do: [ :factor |
     ( (candidate \\ factor) = 0 )
        ifTrue: [
           sum := sum + (factor reciprocal) + ((candidate / factor) reciprocal)
        ]
  ].
  ( (sum denominator) = 1 )
      ifTrue: [
           ('Sum of recipr. factors of %1 = %2 exactly %3' %
                     { candidate printString .
                       (sum asInteger) printString .
                       ( sum = 1 ) ifTrue: [ 'perfect!' ]
                                   ifFalse: [ ' ' ] }) displayNl
      ]
].
```



## Tcl

<div style="text-align:right;font-size:7pt">''<nowiki>[</nowiki>This section is included from [[Arithmetic/Rational/Tcl|a subpage]] and should be edited there, not here.<nowiki>]</nowiki>''</div>
{{:Arithmetic/Rational/Tcl}}

=={{header|TI-89 BASIC}}==
{{incomplete|TI-89 BASIC}}
While TI-89 BASIC has built-in rational and symbolic arithmetic, it does not have user-defined data types.


## zkl

Enough of a Rational class for this task (ie implement the testing code "nicely").

```zkl
class Rational{  // Weenie Rational class, can handle BigInts
   fcn init(_a,_b){ var a=_a, b=_b; normalize(); }
   fcn toString{
      if(b==1) a.toString()
      else     "%d//%d".fmt(a,b)
   }
   var [proxy] isZero=fcn{ a==0 };
   fcn normalize{  // divide a and b by gcd
      g:= a.gcd(b);
      a/=g; b/=g;
      if(b<0){ a=-a; b=-b; } // denominator > 0
      self
   }
   fcn abs       { a=a.abs(); self }
   fcn __opNegate{ a=-a;      self }			    // -Rat
   fcn __opAdd(n){
      if(Rational.isChildOf(n)) self(a*n.b + b*n.a, b*n.b); // Rat + Rat
      else self(b*n + a, b);				    // Rat + Int
   }
   fcn __opSub(n){ self(a*n.b - b*n.a, b*n.b) }		    // Rat - Rat
   fcn __opMul(n){
      if(Rational.isChildOf(n)) self(a*n.a, b*n.b);	    // Rat * Rat
      else self(a*n, b);				    // Rat * Int
   }
   fcn __opDiv(n){ self(a*n.b,b*n.a) }			    // Rat / Rat
   fcn __opEQ(r){				       // Rat==Rat, Rat==n
      if(Rational.isChildOf(r)) a==r.a and b=r.b;
      else			b==1   and a==r;
   }
}
```


```zkl
foreach p in ([2 .. (2).pow(19)]){
   sum,limit := Rational(1,p), p.toFloat().sqrt();
   foreach factor in ([2 .. limit]){
      if(p%factor == 0) sum+=Rational(1,factor) + Rational(factor,p);
   }
   if(sum.b==1) println("Sum of recipr. factors of %6s = %s exactly%s"
			.fmt(p, sum, (sum==1) and ", perfect." or "."));
}
```

{{out}}

```txt

Sum of recipr. factors of      6 = 1 exactly, perfect.
Sum of recipr. factors of     28 = 1 exactly, perfect.
Sum of recipr. factors of    120 = 2 exactly.
Sum of recipr. factors of    496 = 1 exactly, perfect.
Sum of recipr. factors of    672 = 2 exactly.
Sum of recipr. factors of   8128 = 1 exactly, perfect.
Sum of recipr. factors of  30240 = 3 exactly.
Sum of recipr. factors of  32760 = 3 exactly.
Sum of recipr. factors of 523776 = 2 exactly.

```

