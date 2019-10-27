+++
title = "Arithmetic/Rational/Modula-3"
description = ""
date = 2018-11-07T07:16:54Z
aliases = []
[extra]
id = 22060
[taxonomies]
categories = []
tags = []
+++

{{Rational Arithmetic}}

This implementation implements the task in a module named <code>Frac</code> that exports an opaque type named <code>T</code>, a standard Modula-3 practice. Although the task does not require it, the module also defines the exception <code>ZeroDenominator</code> for occasions when one might attempt division by zero, which includes at initialization. It provides two functions that return rational numbers for 0 and 1.

Unfortunately IMHO Modula-3 eschewed operator overloading, so the arithmetic looks unpleasant.

;Interface module

```modula3
INTERFACE Frac;

EXCEPTION ZeroDenominator;

TYPE

  T <: Public;
  Public = OBJECT
  METHODS
    (* initialization and conversion *)
    init(a, b: INTEGER): T RAISES { ZeroDenominator };
    fromInt(a: INTEGER): T;
    (* unary operators *)
    abs(): T;
    opposite(): T;
    (* binary operators *)
    plus(other: T): T;
    minus(other: T): T;
    times(other: T): T;
    dividedBy(other: T): T RAISES { ZeroDenominator };
    integerDivision(other: INTEGER): T RAISES { ZeroDenominator };
    (* relations *)
    equals(other: T): BOOLEAN;
    notEqualTo(other: T): BOOLEAN;
    lessThan(other: T): BOOLEAN;
    lessEqual(other: T): BOOLEAN;
    greaterEqual(other: T): BOOLEAN;
    greater(other: T): BOOLEAN;
    (* other easily-checked properties *)
    isInt(): BOOLEAN;
    (* inc/decrement *)
    inc(step: CARDINAL := 1);
    dec(step: CARDINAL := 1);
  END;

  PROCEDURE One():  T;
  PROCEDURE Zero(): T;

  (* I/O *)
  PROCEDURE PutRat(a: T);

END Frac.
```


;Implementation module
The implementation keeps all rational numbers in simplest form.


```modula3
MODULE Frac;

IMPORT IO;

TYPE

  REVEAL T = Public BRANDED OBJECT
    num: INTEGER := 0;
    den: INTEGER := 1;
  OVERRIDES
    init := Init;
    fromInt := FromInt;
    abs := Abs;
    opposite := Opposite;
    plus := Plus;
    minus := Minus;
    times := Times;
    dividedBy := DividedBy;
    integerDivision := IntegerDivision;
    equals := Equals;
    notEqualTo := NotEqualTo;
    lessThan := LessThan;
    lessEqual := LessEqual;
    greaterEqual := GreaterEqual;
    greater := Greater;
    isInt := IsInt;
    inc := Inc;
    dec := Dec;
  END;

PROCEDURE Gcd(a, b: CARDINAL): CARDINAL =
VAR
  m := MAX(a, b);
  n := MIN(a, b);
  r: CARDINAL;
BEGIN
  WHILE n # 0 DO
    r := m MOD n;
    m := n;
    n := r;
  END;
  RETURN m;
END Gcd;

PROCEDURE Simplify(VAR a: T) =
VAR
  d := Gcd(ABS(a.num), ABS(a.den));
BEGIN
  a.num := a.num DIV d;
  a.den := a.den DIV d;
END Simplify;

PROCEDURE Init(self: T; a, b: INTEGER): T RAISES { ZeroDenominator } =
BEGIN
  IF (b > 0) THEN
    self.num := a;
    self.den := b;
  ELSIF (b < 0) THEN
    self.num := -a;
    self.den := -b;
  ELSE
    RAISE ZeroDenominator;
  END;
  Simplify(self);
  RETURN self;
END Init;

PROCEDURE FromInt(self: T; a: INTEGER): T =
BEGIN
  self.num := a;
  self.den := 1;
  RETURN self;
END FromInt;

PROCEDURE Abs(self: T): T =
BEGIN
  RETURN NEW(T, num := ABS(self.num), den := self.den);
END Abs;

PROCEDURE Opposite(self: T): T =
BEGIN
  RETURN NEW(T, num := -self.num, den := self.den);
END Opposite;

PROCEDURE Plus(self, other: T): T =
VAR
  result := NEW( T,
      num := self.num * other.den + self.den * other.num,
      den := self.den * other.den
  );
BEGIN
  Simplify(result);
  RETURN result;
END Plus;

PROCEDURE Minus(self, other: T): T =
VAR
  result := NEW( T,
      num := self.num * other.den - self.den * other.num,
      den := self.den * other.den
  );
BEGIN
  Simplify(result);
  RETURN result;
END Minus;

PROCEDURE Times(self, other: T): T =
VAR
  result := NEW( T,
      num := self.num * other.num,
      den := self.den * other.den
  );
BEGIN
  Simplify(result);
  RETURN result;
END Times;

PROCEDURE DividedBy(self, other: T): T RAISES { ZeroDenominator } =
VAR
  result := NEW( T,
      num := self.num * other.den,
      den := self.den * other.num
  );
BEGIN
  IF result.den = 0 THEN RAISE ZeroDenominator; END;
  IF result.den < 0 THEN
    result.num := -result.num;
    result.den := -result.den;
  END;
  Simplify(result);
  RETURN result;
END DividedBy;

PROCEDURE IntegerDivision(self: T; other: INTEGER): T
RAISES { ZeroDenominator } =
VAR
  result := NEW( T, num := self.num, den := self.den * other );
BEGIN
  IF other = 0 THEN RAISE ZeroDenominator; END;
  Simplify(result);
  RETURN result;
END IntegerDivision;

PROCEDURE Equals(self, other: T): BOOLEAN =
BEGIN
  (* this trick works only because we simplify after each operation *)
  RETURN (self.num = other.num) AND (self.den = other.den);
END Equals;

PROCEDURE NotEqualTo(self, other: T): BOOLEAN =
BEGIN
  (* this trick works only because we simplify after each operation *)
  RETURN (self.num # other.num) OR (self.den # other.den);
END NotEqualTo;

PROCEDURE LessThan(self, other: T): BOOLEAN =
BEGIN
  RETURN self.num * other.den < self.den * other.num;
END LessThan;

PROCEDURE LessEqual(self, other: T): BOOLEAN =
BEGIN
  RETURN self.num * other.den <= self.den * other.num;
END LessEqual;

PROCEDURE GreaterEqual(self, other: T): BOOLEAN =
BEGIN
  RETURN self.num * other.den >= self.den * other.num;
END GreaterEqual;

PROCEDURE Greater(self, other: T): BOOLEAN =
BEGIN
  RETURN self.num * other.den > self.den * other.num;
END Greater;

PROCEDURE Inc(self: T; step: CARDINAL) =
BEGIN
  INC(self.num, step * self.den);
END Inc;

PROCEDURE Dec(self: T; step: CARDINAL) =
BEGIN
  DEC(self.num, step * self.den);
END Dec;

PROCEDURE IsInt(self: T): BOOLEAN =
BEGIN
  RETURN self.den = 1;
END IsInt;

PROCEDURE One(): T =
BEGIN
  TRY
    RETURN NEW(T).init(1, 1);
  EXCEPT ZeroDenominator =>
    IO.Put("Something went very wrong.\n");
    RETURN NIL;
  END;
END One;

PROCEDURE Zero(): T =
BEGIN
  TRY
    RETURN NEW(T).init(0, 1);
  EXCEPT ZeroDenominator =>
    IO.Put("Something went very wrong.\n");
    RETURN NIL;
  END;
END Zero;

PROCEDURE PutRat(a: T) =
BEGIN
  IO.PutInt(a.num);
  IF a.den # 1 THEN
    IO.Put(" / "); IO.PutInt(a.den);
  END;
END PutRat;

BEGIN
END Frac.
```


;Test Program

{{Trans|Nim}}
This module performs a few additional tests. The test for perfect numbers is based on that of Nim above.

```modula3
MODULE TestRational EXPORTS Main;

IMPORT IO, Frac AS R, Word;

FROM Math IMPORT sqrt;

PROCEDURE PutBool(b: BOOLEAN) =
BEGIN
  IF b THEN IO.Put("true");
  ELSE IO.Put("false");
  END;
END PutBool;

VAR

  a, b: R.T;

  n: Word.T := 2;
  limit: Word.T := 1;

BEGIN

  TRY
    a := NEW(R.T).init(2,3);
    b := NEW(R.T).init(-3,4);
  EXCEPT | R.ZeroDenominator =>
    IO.Put("Zero division!\n");
  END;

  R.PutRat(a); IO.Put(" op "); R.PutRat(b); IO.Put(" = \n");

  IO.Put("  + : "); R.PutRat(a.plus(b));  IO.PutChar('\n');
  IO.Put("  - : "); R.PutRat(a.minus(b)); IO.PutChar('\n');
  IO.Put("  * : "); R.PutRat(a.times(b)); IO.PutChar('\n');

  TRY
    IO.Put("  /: "); R.PutRat(a.dividedBy(b)); IO.PutChar('\n');
  EXCEPT | R.ZeroDenominator =>
    IO.Put("Zero division\n");
  END;

  IO.Put("  <  : "); PutBool(a.lessThan(b));     IO.PutChar('\n');
  IO.Put("  <= : "); PutBool(a.lessEqual(b));    IO.PutChar('\n');
  IO.Put("  >= : "); PutBool(a.greaterEqual(b)); IO.PutChar('\n');
  IO.Put("  >  : "); PutBool(a.greater(b));      IO.PutChar('\n');

  IO.PutChar('\n');

  IO.Put("Increasing "); R.PutRat(a); IO.Put(" by\n");
  IO.Put("  1 gives "); a.inc(1); R.PutRat(a); IO.PutChar('\n');
  IO.Put("  3 gives "); a.inc(3); R.PutRat(a); IO.PutChar('\n');

  IO.Put("Decreasing "); R.PutRat(a); IO.Put(" by\n");
  IO.Put("  1 gives "); a.dec(1); R.PutRat(a); IO.PutChar('\n');
  IO.Put("  3 gives "); a.dec(3); R.PutRat(a); IO.PutChar('\n');

  limit := Word.LeftShift(limit, 19);
  TRY
    WHILE n < limit DO
        VAR
          sum := NEW(R.T).init(1, n);
          maxFac := FLOOR(sqrt(FLOAT(n, LONGREAL)));
        BEGIN
          FOR i := 2 TO maxFac DO
            IF n MOD i = 0 THEN
              sum := sum.plus(NEW(R.T).init(1, i))
                        .plus(NEW(R.T).init(1, n DIV i));
            END;
          END;
          IF sum.isInt() THEN
            IO.Put("sum of reciprocal factors of "); IO.PutInt(n);
            IO.Put(" is exactly "); R.PutRat(sum);
            IF sum.equals(R.One()) THEN
              IO.Put(" perfect!");
            END;
            IO.PutChar('\n');
          END;
        END;
      INC(n, 2);
    END;
  EXCEPT R.ZeroDenominator =>
    IO.Put("Something went very wrong\n");
  END;

END TestRational.
```


{{out}}

```txt

2 / 3 op -3 / 4 = 
  + : -1 / 12
  - : 17 / 12
  * : -1 / 2
  /: -8 / 9
  <  : false
  <= : false
  >= : true
  >  : true

Increasing 2 / 3 by
  1 gives 5 / 3
  3 gives 14 / 3
Decreasing 14 / 3 by
  1 gives 11 / 3
  3 gives 2 / 3
sum of reciprocal factors of 6 is exactly 1 perfect!
sum of reciprocal factors of 28 is exactly 1 perfect!
sum of reciprocal factors of 120 is exactly 2
sum of reciprocal factors of 496 is exactly 1 perfect!
sum of reciprocal factors of 672 is exactly 2
sum of reciprocal factors of 8128 is exactly 1 perfect!
sum of reciprocal factors of 30240 is exactly 3
sum of reciprocal factors of 32760 is exactly 3
sum of reciprocal factors of 523776 is exactly 2

```

