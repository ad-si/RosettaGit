+++
title = "Define a primitive data type"
description = ""
date = 2019-09-14T16:04:55Z
aliases = []
[extra]
id = 1992
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
languages = [
  "ada",
  "algol_68",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "dyalect",
  "e",
  "echolisp",
  "elena",
  "euphoria",
  "factor",
  "forth",
  "fortran",
  "free_pascal",
  "freebasic",
  "go",
  "haskell",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lasso",
  "m2000_interpreter",
  "matlab",
  "nim",
  "ocaml",
  "oorexx",
  "oz",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "python",
  "racket",
  "retro",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "tcl",
  "toka",
  "unix_shell",
  "ursala",
  "visual_basic",
  "visual_basic_.net",
  "visual_foxpro",
]
+++

## Task

Demonstrate how to define a type that behaves like an integer but has a lowest valid value of 1 and a highest valid value of 10. Include all bounds checking you need to write, or explain how the compiler or interpreter creates those bounds checks for you.


## Ada


```ada
type My_Type is range 1..10;
```

The compiler identifies the range of valid values from the range specification ''1..10'' and automatically builds in bounds checking where it is needed. The compiler is smart enough to omit bounds checking when it is not needed.

```ada
A : My_Type := 3;
B : My_Type := A;
```

The compiler will omit bounds checking for the assignment of A to B above because both values are of My_Type. A cannot hold a value outside the range of 1..10, therefore the assignment cannot produce an out of bounds result.


## ALGOL 68


### Built in or standard distribution routines

Bounded data types are not part of standard '''ALGOL 68''', but can be implemented.

### Implementation example

```algol68
 # assume max int <= ABS - max negative int #
 INT max bounded = ( LENG max int * max int > long max int | ENTIER sqrt(max int) | max int );

 MODE RANGE = STRUCT(INT lwb, upb);
 MODE BOUNDED = STRUCT(INT value, RANGE range);
 FORMAT bounded repr = $g"["g(-0)":"g(-0)"]"$;

 # Define some useful operators for looping over ranges #
 OP LWB = (RANGE range)INT: lwb OF range,
    UPB = (RANGE range)INT: upb OF range,
    LWB = (BOUNDED bounded)INT: lwb OF range OF bounded,
    UPB = (BOUNDED bounded)INT: upb OF range OF bounded;

 PROC raise exception = ([]STRING args)VOID: (
   put(stand error, ("exception: ",args, newline));
   stop
 );

 PROC raise not implemented error := ([]STRING args)VOID: raise exception(args);
 PROC raise bounds error := ([]STRING args)VOID: raise exception(args);

 PRIO MIN=9, MAX=9;
 OP MIN = ([]INT list)INT: (
   INT out:= list[LWB list];
   FOR index FROM LWB list+1 TO UPB list DO IF list[index]<out THEN out :=list[index] FI OD;
   out
 );
 OP MAX = ([]INT list)INT: (
   INT out:= list[LWB list];
   FOR index FROM LWB list+1 TO UPB list DO IF list[index]>out THEN out :=list[index] FI OD;
   out
 );

 PRIO ASSERTIN = 6;
 OP ASSERTIN = (INT result, []RANGE range)BOUNDED: (
     BOUNDED out = (result, (MAX lwb OF range, MIN upb OF range));
     IF value OF out < lwb OF range OF out THEN
       raise bounds error(("out of bounds", whole(result, int width)," < [",whole(MAX lwb OF range, int width),":]"))
     ELIF value OF out > upb OF range OF out THEN
       raise bounds error(("out of bounds", whole(result, int width)," > [:",whole(MIN upb OF range, int width),"]"))
     FI;
     out
   ),
   ASSERTIN = (LONG INT result, []RANGE range)BOUNDED: (
     STRUCT (LONG INT value, RANGE range) out = (result, (MAX lwb OF range, MIN upb OF range));
     IF value OF out < lwb OF range OF out THEN
       raise bounds error(("out of bounds", whole(result, long int width)," < [",whole(MAX lwb OF range, int width),":]"))
     ELIF value OF out > upb OF range OF out THEN
       raise bounds error(("out of bounds", whole(result, long int width)," > [:",whole(MIN upb OF range, int width),"]"))
     FI;
     (SHORTEN value OF out, range OF out)
   ),
   ASSERTIN = (INT result, []BOUNDED bounds)BOUNDED: result ASSERTIN range OF bounds,
   ASSERTIN = (LONG INT result, []BOUNDED bounds)BOUNDED: result ASSERTIN range OF bounds;

 INT half max int = max int OVER 2;
 INT sqrt max int = ENTIER sqrt (max int);

 OP + = (BOUNDED a, b)BOUNDED:
          IF ABS value OF a < half max int AND ABS value OF b < half max int THEN
            value OF a + value OF b ASSERTIN []BOUNDED(a,b)
          ELSE
            LENG value OF a + value OF b ASSERTIN []BOUNDED(a,b)
          FI,
    - = (BOUNDED a, b)BOUNDED: value OF a + -value OF b ASSERTIN []BOUNDED(a,b),
    * = (BOUNDED a, b)BOUNDED:
          IF ABS value OF a < sqrt max int AND ABS value OF b < sqrt max int THEN
            value OF a * value OF b ASSERTIN []BOUNDED(a,b)
          ELSE
            LENG value OF a * value OF b ASSERTIN []BOUNDED(a,b)
          FI,
    /  = (BOUNDED a, b)REAL: value OF a / value OF b,
    %  = (BOUNDED a, b)BOUNDED: value OF a % value OF b ASSERTIN []BOUNDED(a,b),
    %* = (BOUNDED a, b)BOUNDED: value OF a %* value OF b ASSERTIN []BOUNDED(a,b),
    ** = (BOUNDED a, INT exponent)BOUNDED: value OF a ** exponent ASSERTIN []BOUNDED(a);

 OP OVER = (INT value, RANGE range)BOUNDED:
   IF ABS lwb OF range > max bounded THEN
     raise bounds error(("out of bounds, ABS", whole(lwb OF range, int width)," > [:",whole(max bounded, int width),"]"));
     SKIP
   ELIF ABS upb OF range > max bounded THEN
     raise bounds error(("out of bounds, ABS", whole(upb OF range, int width)," > [:",whole(max bounded, int width),"]"));
     SKIP
   ELSE
     value ASSERTIN []RANGE(range)
   FI;

 OP INTINIT = (BOUNDED range)REAL: value OF range;

 OP <  = (BOUNDED a, b)BOOL: value OF a < value OF b,
    >  = (BOUNDED a, b)BOOL: value OF a > value OF b,
    <= = (BOUNDED a, b)BOOL: NOT ( value OF a > value OF b ),
    >= = (BOUNDED a, b)BOOL: NOT ( value OF a < value OF b ),
    =  = (BOUNDED a, b)BOOL: value OF a = value OF b,
    /= = (BOUNDED a, b)BOOL: NOT (a = b);

 # Monadic operators #
 OP - = (BOUNDED range)BOUNDED: -value OF range ASSERTIN []BOUNDED(range),
    ABS = (BOUNDED range)BOUNDED: ABS value OF range ASSERTIN []BOUNDED(range);

 COMMENT Operators for extended characters set, and increment/decrement:
 OP +:= = (REF BOUNDED a, BOUNDED b)REF BOUNDED: ( a := a + b ),
    +=: = (BOUNDED a, REF BOUNDED b)REF BOUNDED: ( b := a + b ),
    -:= = (REF BOUNDED a, BOUNDED b)REF BOUNDED: ( a := a - b ),
    *:= = (REF BOUNDED a, BOUNDED b)REF BOUNDED: ( a := a * b ),
    %:= = (REF BOUNDED a, BOUNDED b)REF BOUNDED: ( a := a % b ),
    %*:= = (REF BOUNDED a, BOUNDED b)REF BOUNDED: ( a := a %* b );

 # OP aliases for extended character sets (eg: Unicode, APL, ALCOR and GOST 10859) #
 OP ×  = (BOUNDED a, b)BOUNDED: a * b,
    ÷  = (BOUNDED a, b)INT: a OVER b,
    ÷× = (BOUNDED a, b)BOUNDED: a MOD b,
    ÷* = (BOUNDED a, b)BOUNDED: a MOD b,
    %× = (BOUNDED a, b)BOUNDED: a MOD b,
    ≤  = (BOUNDED a, b)BOUNDED: a <= b,
    ≥  = (BOUNDED a, b)BOUNDED: a >= b,
    ≠  = (BOUNDED a, b)BOOL: a /= b,
    ↑  = (BOUNDED range, INT exponent)BOUNDED: value OF range ** exponent,

    ÷×:= = (REF BOUNDED a, BOUNDED b)REF BOUNDED: ( a := a MOD b ),
    %×:= = (REF BOUNDED a, BOUNDED b)REF BOUNDED: ( a := a MOD b ),
    ÷*:= = (REF BOUNDED a, BOUNDED b)REF BOUNDED: ( a := a MOD b );

 # BOLD aliases for CPU that only support uppercase for 6-bit bytes  - wrist watches #
 OP OVER = (BOUNDED a, b)INT: a % b,
    MOD = (BOUNDED a, b)BOUNDED: a %*b,
    LT = (BOUNDED a, b)BOOL: a <  b,
    GT = (BOUNDED a, b)BOOL: a >  b,
    LE = (BOUNDED a, b)BOOL: a <= b,
    GE = (BOUNDED a, b)BOOL: a >= b,
    EQ = (BOUNDED a, b)BOOL: a =  b,
    NE = (BOUNDED a, b)BOOL: a /= b,
    UP = (BOUNDED range, INT exponent)BOUNDED: range**exponent;

 # the required standard assignment operators #
 OP PLUSAB  = (REF BOUNDED a, BOUNDED b)REF BOUNDED: ( a +:= b ), # PLUS #
    PLUSTO  = (BOUNDED a, REF BOUNDED b)REF BOUNDED: ( a +=: b ), # PRUS #
    MINUSAB = (REF BOUNDED a, BOUNDED b)REF BOUNDED: ( a *:= b ),
    DIVAB   = (REF BOUNDED a, BOUNDED b)REF BOUNDED: ( a /:= b ),
    OVERAB  = (REF BOUNDED a, BOUNDED b)REF BOUNDED: ( a %:= b ),
    MODAB   = (REF BOUNDED a, BOUNDED b)REF BOUNDED: ( a %*:= b );

END COMMENT
Test:
 RANGE range = RANGE(0, 10000);

 # override the default exception #
 raise bounds error := ([]STRING args)VOID: (
     putf(stand error, ($g$, args, $"- exiting to except bounds error"l$));
     except bounds error
   );

 BOUNDED a, b := 0 OVER range;
 FOR step FROM 4 BY 4 TO UPB range DO # something for pythagoras #
   b := b + step OVER range;
   a := ENTIER sqrt( 1.5 + 2 * value OF b ) OVER range OF b;
   printf(($"Sum of "$, bounded repr, a * a, b * b,
           $" is "$,    bounded repr, a * a + b * b, $l$))
 OD;
 except bounds error:
   SKIP
```

Output:

```txt

Sum of          +9[0:10000]        +16[0:10000] is         +25[0:10000]
Sum of         +25[0:10000]       +144[0:10000] is        +169[0:10000]
Sum of         +49[0:10000]       +576[0:10000] is        +625[0:10000]
Sum of         +81[0:10000]      +1600[0:10000] is       +1681[0:10000]
Sum of        +121[0:10000]      +3600[0:10000] is       +3721[0:10000]
Sum of        +169[0:10000]      +7056[0:10000] is       +7225[0:10000]
out of bounds    +12544 > [:    +10000]- exiting to except bounds error

```



### Other libraries or implementation specific extensions

As of February 2009 no open source libraries to do this task have been located.


## C#

Strictly speaking, this task is impossible in C# because a type that behaves like an integer (<code>int</code>, or <code>System.Int32</code>; <code>Integer</code> in VB.NET) must be a struct (in order to have value-type semantics), and, since C# does not allow the definition of a parameterless constructor for a struct (instead generating a default parameterless constructor that sets each of its fields to the default value for the type of the field), an instance of the struct with the forbidden value of zero can be created through that default constructor.

A way to overcome this in the type's public interface, however, is to expose the field exclusively as a property that checks whether its backing field is out of range, and, if so, returns a valid default value.

Through operator overloading, it is possible to declare types in C# with the full complement of operators available on the primitive types. The following structure attempts to mimic the behavior of the <code>int</code> type in C# on .NET Core as much as possible, including interfaces and static members, delegating as much implementation as possible to the <code>Integer</code> type itself.


```c#
using System;
using System.Globalization;

struct LimitedInt : IComparable, IComparable<LimitedInt>, IConvertible, IEquatable<LimitedInt>, IFormattable
{
    const int MIN_VALUE = 1;
    const int MAX_VALUE = 10;

    public static readonly LimitedInt MinValue = new LimitedInt(MIN_VALUE);
    public static readonly LimitedInt MaxValue = new LimitedInt(MAX_VALUE);

    static bool IsValidValue(int value) => value >= MIN_VALUE && value <= MAX_VALUE;

    readonly int _value;
    public int Value => this._value == 0 ? MIN_VALUE : this._value; // Treat the default, 0, as being the minimum value.

    public LimitedInt(int value)
    {
        if (!IsValidValue(value))
            throw new ArgumentOutOfRangeException(nameof(value), value, $"Value must be between {MIN_VALUE} and {MAX_VALUE}.");
        this._value = value;
    }

    #region IComparable
    public int CompareTo(object obj)
    {
        if (obj is LimitedInt l) return this.Value.CompareTo(l);
        throw new ArgumentException("Object must be of type " + nameof(LimitedInt), nameof(obj));
    }
    #endregion

    #region IComparable<LimitedInt>
    public int CompareTo(LimitedInt other) => this.Value.CompareTo(other.Value);
    #endregion

    #region IConvertible
    public TypeCode GetTypeCode() => this.Value.GetTypeCode();
    bool IConvertible.ToBoolean(IFormatProvider provider) => ((IConvertible)this.Value).ToBoolean(provider);
    byte IConvertible.ToByte(IFormatProvider provider) => ((IConvertible)this.Value).ToByte(provider);
    char IConvertible.ToChar(IFormatProvider provider) => ((IConvertible)this.Value).ToChar(provider);
    DateTime IConvertible.ToDateTime(IFormatProvider provider) => ((IConvertible)this.Value).ToDateTime(provider);
    decimal IConvertible.ToDecimal(IFormatProvider provider) => ((IConvertible)this.Value).ToDecimal(provider);
    double IConvertible.ToDouble(IFormatProvider provider) => ((IConvertible)this.Value).ToDouble(provider);
    short IConvertible.ToInt16(IFormatProvider provider) => ((IConvertible)this.Value).ToInt16(provider);
    int IConvertible.ToInt32(IFormatProvider provider) => ((IConvertible)this.Value).ToInt32(provider);
    long IConvertible.ToInt64(IFormatProvider provider) => ((IConvertible)this.Value).ToInt64(provider);
    sbyte IConvertible.ToSByte(IFormatProvider provider) => ((IConvertible)this.Value).ToSByte(provider);
    float IConvertible.ToSingle(IFormatProvider provider) => ((IConvertible)this.Value).ToSingle(provider);
    string IConvertible.ToString(IFormatProvider provider) => this.Value.ToString(provider);
    object IConvertible.ToType(Type conversionType, IFormatProvider provider) => ((IConvertible)this.Value).ToType(conversionType, provider);
    ushort IConvertible.ToUInt16(IFormatProvider provider) => ((IConvertible)this.Value).ToUInt16(provider);
    uint IConvertible.ToUInt32(IFormatProvider provider) => ((IConvertible)this.Value).ToUInt32(provider);
    ulong IConvertible.ToUInt64(IFormatProvider provider) => ((IConvertible)this.Value).ToUInt64(provider);
    #endregion

    #region IEquatable<LimitedInt>
    public bool Equals(LimitedInt other) => this == other;
    #endregion

    #region IFormattable
    public string ToString(string format, IFormatProvider formatProvider) => this.Value.ToString(format, formatProvider);
    #endregion

    #region operators
    public static bool operator ==(LimitedInt left, LimitedInt right) => left.Value == right.Value;
    public static bool operator !=(LimitedInt left, LimitedInt right) => left.Value != right.Value;
    public static bool operator <(LimitedInt left, LimitedInt right) => left.Value < right.Value;
    public static bool operator >(LimitedInt left, LimitedInt right) => left.Value > right.Value;
    public static bool operator <=(LimitedInt left, LimitedInt right) => left.Value <= right.Value;
    public static bool operator >=(LimitedInt left, LimitedInt right) => left.Value >= right.Value;

    public static LimitedInt operator ++(LimitedInt left) => (LimitedInt)(left.Value + 1);
    public static LimitedInt operator --(LimitedInt left) => (LimitedInt)(left.Value - 1);

    public static LimitedInt operator +(LimitedInt left, LimitedInt right) => (LimitedInt)(left.Value + right.Value);
    public static LimitedInt operator -(LimitedInt left, LimitedInt right) => (LimitedInt)(left.Value - right.Value);
    public static LimitedInt operator *(LimitedInt left, LimitedInt right) => (LimitedInt)(left.Value * right.Value);
    public static LimitedInt operator /(LimitedInt left, LimitedInt right) => (LimitedInt)(left.Value / right.Value);
    public static LimitedInt operator %(LimitedInt left, LimitedInt right) => (LimitedInt)(left.Value % right.Value);

    public static LimitedInt operator &(LimitedInt left, LimitedInt right) => (LimitedInt)(left.Value & right.Value);
    public static LimitedInt operator |(LimitedInt left, LimitedInt right) => (LimitedInt)(left.Value | right.Value);
    public static LimitedInt operator ^(LimitedInt left, LimitedInt right) => (LimitedInt)(left.Value ^ right.Value);
    public static LimitedInt operator ~(LimitedInt left) => (LimitedInt)~left.Value;

    public static LimitedInt operator >>(LimitedInt left, int right) => (LimitedInt)(left.Value >> right);
    public static LimitedInt operator <<(LimitedInt left, int right) => (LimitedInt)(left.Value << right);

    public static implicit operator int(LimitedInt value) => value.Value;
    public static explicit operator LimitedInt(int value)
    {
        if (!IsValidValue(value)) throw new OverflowException();
        return new LimitedInt(value);
    }
    #endregion

    public bool TryFormat(Span<char> destination, out int charsWritten, ReadOnlySpan<char> format = default, IFormatProvider provider = null)
        => this.Value.TryFormat(destination, out charsWritten, format, provider);

    public override int GetHashCode() => this.Value.GetHashCode();
    public override bool Equals(object obj) => obj is LimitedInt l && this.Equals(l);
    public override string ToString() => this.Value.ToString();

    #region static methods
    public static bool TryParse(ReadOnlySpan<char> s, out int result) => int.TryParse(s, out result);
    public static bool TryParse(ReadOnlySpan<char> s, NumberStyles style, IFormatProvider provider, out int result) => int.TryParse(s, style, provider, out result);
    public static int Parse(string s, IFormatProvider provider) => int.Parse(s, provider);
    public static int Parse(string s, NumberStyles style, IFormatProvider provider) => int.Parse(s, style, provider);
    public static bool TryParse(string s, NumberStyles style, IFormatProvider provider, ref int result) => int.TryParse(s, style, provider, out result);
    public static int Parse(string s) => int.Parse(s);
    public static int Parse(string s, NumberStyles style) => int.Parse(s, style);
    public static int Parse(ReadOnlySpan<char> s, NumberStyles style = NumberStyles.Integer, IFormatProvider provider = null) => int.Parse(s, style, provider);
    public static bool TryParse(string s, ref int result) => int.TryParse(s, out result);
    #endregion
}
```



## C++

This class relies on implicit conversions to do most int operations; however the combined operations with assignment have to be coded explicitly.


```cpp
#include <stdexcept>

class tiny_int
{
public:
  tiny_int(int i):
    value(i)
  {
    if (value < 1)
      throw std::out_of_range("tiny_int: value smaller than 1");
    if (value > 10)
      throw std::out_of_range("tiny_int: value larger than 10");
  }
  operator int() const
  {
    return value;
  }
  tiny_int& operator+=(int i)
  {
    // by assigning to *this instead of directly modifying value, the
    // constructor is called and thus the check is enforced
    *this = value + i;
    return *this;
  }
  tiny_int& operator-=(int i)
  {
    *this = value - i;
    return *this;
  }
  tiny_int& operator*=(int i)
  {
    *this = value * i;
    return *this;
  }
  tiny_int& operator/=(int i)
  {
    *this = value / i;
    return *this;
  }
  tiny_int& operator<<=(int i)
  {
    *this = value << i;
    return *this;
  }
  tiny_int& operator>>=(int i)
  {
    *this = value >> i;
    return *this;
  }
  tiny_int& operator&=(int i)
  {
    *this = value & i;
    return *this;
  }
  tiny_int& operator|=(int i)
  {
    *this = value | i;
    return *this;
  }
private:
  unsigned char value; // we don't need more space
};
```



## Clojure


Use proxy on java.lang.Number so it can be used in Clojure's math operations.


```clojure
(defn tinyint [^long value]
  (if (<= 1 value 10)
    (proxy [Number] []
      (doubleValue [] value)
      (longValue [] value))
    (throw (ArithmeticException. "integer overflow"))))
```


```txt
user=> (+ (tinyint 1) (tinyint 9))
10
user=> (* 4 (tinyint 6))
24
user=> (* 4 (tinyint 6.0))
24
user=> (/ (tinyint 3) (tinyint 5))
3/5
user=> (tinyint 11)
ArithmeticException integer overflow
user=> (.doubleValue (tinyint 9.4))
9.0
```



## Common Lisp

The built-in integer type specifier provides range parameters. <code>deftype</code> may be used to define an alias for it.


```lisp
(deftype one-to-ten ()
  '(integer 1 10))
```


For a bounds check, one may use <code>typep</code> (a predicate) or <code>check-type</code> (signals an error if not of the type).


```lisp
(defun word (i)
  (check-type i one-to-ten)
  (case i
    (1 "one")
    (2 "two")
    (3 "three")
    (4 "four")
    (5 "five")
    (6 "six")
    (7 "seven")
    (8 "eight")
    (9 "nine")
    (10 "ten")))
```


(Note that the above can be written without the separate check-type by using <code>ecase</code> instead of <code>case</code>, which signals an error when no case matches.)

To inform the compiler that a variable will be of a certain type, permitting optimizations, use a declaration:


```lisp
(dolist (i numbers)
  (declare (type one-to-ten i))
  ...)
```


Note, however, that the standard does not specify what happens in the event that a declaration is false (though [[SBCL]], for example, does perform type checks on any declaration except when <code>safety</code> is 0); use <code>check-type</code> for portable bounds checks.


## D



```D
import std.exception, std.string, std.traits;

/++
A bounded integral type template.
Template params:
- min: the minimal value
- max: the maximal value
- I: the type used to store the value internally
+/
struct BoundedInt(alias min, alias max, I = int)
{
    // Static checks
    static assert(isIntegral!(typeof(min)));
    static assert(isIntegral!(typeof(max)));
    static assert(isIntegral!I);
    static assert(min < max);
    static assert(cast(I) max == max, "Type " ~ I.stringof
        ~ " cannot hold values up to " ~ max.stringof);

    /// The actual value stored in this struct
    private I _value;

    /// Getter for the internal value
    @property I internalValue()
    {
        return _value;
    }

    /// 'alias this' to make this struct look like a built-in integer
    alias internalValue this;

    /// Constructor
    this(T)(T value)
    {
        opAssign(value);
    }

    /// Assignment operator
    void opAssign(T)(T value)
        if (isIntegral!T)
    {
        _value = checked(value);
    }

    /// Unary operators
    auto opUnary(string op)() const
    {
        return checked(mixin(op ~ "_value"));
    }

    /// Binary operators
    auto opBinary(string op, T)(T other) const
    {
        return checked(mixin("_value" ~ op ~ "other"));
    }

    /// ditto
    auto opBinaryRight(string op, T)(T other) const
        if (isIntegral!T)
    {
        return checked(mixin("_value" ~ op ~ "other"));
    }

    // Bounds enforcement
    private I checked(T)(T value) const
    {
        enforce(value >= min && value <= max,
            format("Value %s is out of bounds (%s to %s).", value, min, max));
        return cast(I) value;
    }
}

unittest
{
    alias MyInt = BoundedInt!(1, 10);
    // alias BoundInt!(1, 10) MyInt; // D < 2.061

    MyInt i = 4;
    MyInt j = i + i;
    assert(j / 2 == 4);
    assert(2 + j == 10);
    assert(i < 5);
    assert(j > i);
}
```



## Delphi

''See [[#Free Pascal|Free Pascal]]''


## Dyalect


The code below defines a new type <code>TinyInt</code>, provides bounds checking and implementation of all standard arithmetic operators:


```dyalect
type TinyInt

private func getInteger(x) {
    match x {
        Integer => x,
        TinyInt => x.toInteger(),
        _ => throw "Type \"\(x.getType().name)\" is not supported by this operation."
    }
}

private func boundsCheck(x) {
    if x < 1 || x > 10 {
        throw "Overflow."
    }
    x
}

static func TinyInt.TinyInt(i) {
    new(boundsCheck(Integer(i)))
}

func TinyInt.toString() {
    valueof(this).toString()
}

func TinyInt.toInteger() {
    valueof(this)
}

func TinyInt + (other) {
    const z = valueof(this) + getInteger(other)
    TinyInt(z)
}

func TinyInt * (other) {
    const z = valueof(this) * getInteger(other)
    TinyInt(z)
}

func TinyInt - (other) {
    const z = valueof(this) - getInteger(other)
    TinyInt(z)
}

func TinyInt / (other) {
    const z = valueof(this) / getInteger(other)
    TinyInt(z)
}
```


Sample usage (interactive session):


```txt
dy>var x = TinyInt(3)

dy>x += 4
7 :: TinyInt

dy>x * 2
Runtime exception Dy601: Overflow.
Stack trace:
        at boundsCheck(Dyalect.Debug.Par) in <stdio>, line 13, column 9
        at TinyInt(Dyalect.Debug.Par) in <stdio>, line 19, column 29
        at *(Dyalect.Debug.Par) in <stdio>, line 37, column 13
        at <external code>
        at TinyInt(Dyalect.Debug.Par) in <stdio>, line 19, column 29
        at *(Dyalect.Debug.Par) in <stdio>, line 37, column 13
        at <external code>
```



## E


```e
def MyNumber := 1..10

for i :MyNumber in [0, 5, 10, 15, 20, 25] {
    println(i)
}
```

(Note: The region guard, while provided with E, is entirely unprivileged code, and could be argued not to be "primitive".)


## EchoLisp

Types are native objects (Boolean, ..) , or defined by predicates and/or combinations of other types. They are used to check data values or to define functions signatures. The '''types.lib''' library must be loaded.

```scheme

(require 'types)
(require 'math)

;; type defined by a predicate
(define (one-ten? x) (in-interval? x 1 10))
(type One-ten [Integer & one-ten?])

;; OR by an enumeration
(type One-ten [ 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 ])

;; EXPLICIT type checking
;; type-of? returns a Boolean

(type-of? 5 One-ten) → #t
(type-of? 'Albert One-ten) → #f

;; type-check raises an error
(type-check 6 One-ten)  → #t
(type-check 88 One-ten)
⛔ error: One-ten : type-check failure : 88 → 'one-ten?'


;; IMPLICIT type checking
;; declare a function signature
(define (f10 x y) (+ x y))
(signature f10 One-ten -> One-ten -> Integer)

(f10 6 7) → 13
(f10 42 666)
❗ error: One-ten : type-check failure : 42 → 'f10:x'

```



## Elena

ELENA 4.x:

```elena
import extensions;

sealed struct TinyInt : BaseNumber
{
    int value;

    int cast() = value;

    constructor(int n)
    {
        if (n <= 1 || n >= 10)
        {
            InvalidArgumentException.raise()
        };

        value := n
    }

    cast t(string s)
    {
        value := s.toInt();

        if (value <= 1 || value >= 10)
        {
            InvalidArgumentException.raise()
        }
    }

    TinyInt add(TinyInt t)
        = value + (cast int(t));

    TinyInt subtract(TinyInt t)
        = value - (cast int(t));

    TinyInt multiply(TinyInt t)
        = value * (cast int(t));

    TinyInt divide(TinyInt t)
        = value / (cast int(t));

    bool equal(TinyInt t)
       = value == (cast int(t));

    bool less(TinyInt t)
       = value == (cast int(t));
}

public program()
{
    TinyInt i := 4t;
    TinyInt j := i + i;

    try
    {
      i + j
    }
    catch(InvalidArgumentException e)
    {
        console.printLine("A value is out of range")
    }
}
```



## Euphoria

In Euphoria types are special functions that may be used in declaring the allowed values for a variable. A type must have exactly one parameter and should return an atom that is either true (non-zero) or false (zero). Types can also be called just like other functions. The types '''object''', '''sequence''', '''atom''' and '''integer''' are predefined.

```euphoria
type My_Type(integer i)
    return i >= 1 and i <= 10
end type
```



## Factor

We can accomplish this using a predicate class. A predicate class must be a subclass of an existing class and allows the programmer to write a predicate which determines membership.

```factor
PREDICATE: my-int < integer [ 0 > ] [ 11 < ] bi and ;
```

This automatically creates the <code>my-int?</code> predicate word which determines whether an object is a <code>my-int</code>.

```factor
11 my-int? ! f
10 my-int? ! t
"hello" my-int? ! f
```

We can now write methods that specialize on <code>my-int</code>. We will define an increment word, <code>++</code>, which increments <code>integer</code>s by <tt>1</tt> but divides <code>my-int</code>s by <tt>2</tt>.

```factor
GENERIC: ++ ( m -- n )
M: integer ++ 1 + ;
M: my-int ++ 2/ ;

10 ++ ! 5
11 ++ ! 12
```



## Forth

Forth can be thought of as a clever Assembler for the Forth two stack virtual machine. As such Standard Forth does not type data but it does implicitly "type" words that act on data in that each function normally operates on a prescribed data type. True to the low level nature of Forth, the responsibility to select the correct action word for specific data is given to the programmer. That being said there are some simple ways to add the requested mechanism to your Forth system.

It is worth noting that Forth's penchant for simplicity means we do not need to write new math operators since each operator is atomic and takes arguments from the stack and returns the result to the stack. Therefore we get the desired functionality by only changing the assignment operation. Simplicity has its advantages.


###  Method 1: Safe Integer Store operators

Forth is a fetch and store based virtual machine. If we create a safe version of store (!) the programmer simply uses this operator rather than the standard store operator.
<LANG>DECIMAL
: CLIP  ( n lo hi -- n') ROT MIN MAX ;
: BETWEEN  ( n lo hi -- flag) 1+ WITHIN ;

\ programmer chooses CLIPPED or SAFE integer assignment
: CLIP! ( n addr -- ) SWAP 1 10 CLIP SWAP  ! ;
: SAFE!   ( n addr -- )
          OVER 1 10 BETWEEN 0= ABORT" out of range!"
          ! ;
```
Testing
<LANG>VARIABLE X
7 X SAFE!  X ? 7  ok
12 X CLIP! X ? 10 ok

99 X SAFE!
:64: out of range!
99 X >>>SAFE!<<<
Backtrace:
$7FAA2B0C throw
$7FAD4698 c(abort")

```
=== Method 2: redefine standard "store" operator as DEFER word ===
Using the code in Method 1, we can re-define the store (!) operator to be switchable.
<LANG>DECIMAL
: FAST!   ( n addr -- )  ! ;   ( alias the standard version)

DEFER !

\ commands to change the action of '!'
: LIMITS-ON   ( -- ) ['] SAFE! IS ! ;
: LIMITS-OFF  ( -- ) ['] FAST! IS ! ;
: CLIPPING-ON ( -- ) ['] CLIP! IS ! ;
```

Testing
<LANG>VARIABLE Y

LIMITS-OFF  ok
  1  Y !   Y ? 1  ok
  10 Y !   Y ? 10  ok
  11 Y !   Y ? 11  ok
  0  Y !   Y ? 0  ok
  LIMITS-ON  ok
  1  Y !   Y ? 1  ok
  10 Y !   Y ? 10  ok
  11 Y !   Y ?
:25: out of range!
  11 Y >>>!<<<   Y ?
Backtrace:
$7FAA2B0C throw
$7FAD4460 c(abort")
  0  Y !   Y ?
:26: out of range!
  0  Y >>>!<<<   Y ?
Backtrace:
$7FAA2B0C throw
$7FAD4460 c(abort")

```

###  Method 3:  Create a safe VALUE assignment operator

A VALUE defines a numerical data type that returns it's value rather than an address (pointer) like a variable.
We can create a word that assigns a number to a VALUE but tests for out of range errors.
<LANG>: (->) ( n <text> -- )
           OVER 1 10 BETWEEN 0= ABORT" out of range!"
           >BODY ! ;

: ->      ( n -- )
           STATE @
           IF   POSTPONE [']  POSTPONE (->) \ compiling action
           ELSE '  (->)                     \ interpret action
           THEN ; IMMEDIATE
```

Test
<LANG>0 VALUE Z  ok
99 TO Z   ok    ( normal assignment)
Z . 99  ok
99 -> Z         ( safe assignment)
:43: out of range!
99 -> >>>Z<<<
Backtrace:
$7FAA2B0C throw
$7FAD4570 c(abort")
$7FAD45DC (->)
```


## Fortran

The module gives an example of how a ''bounded integer'' could be implemented in Fortran (not all the needed interfaces are implemented, and only the one for the + operator are shown). Bounds are checked at run-time.


```fortran
module Bounded
  implicit none

  type BoundedInteger
     integer, private :: v         ! we cannot allow direct access to this, or we
     integer, private :: from, to  !   can't check the bounds!
     logical, private :: critical
  end type BoundedInteger

  interface assignment(=)
     module procedure bounded_assign_bb, bounded_assign_bi !, &
                    ! bounded_assign_ib
  end interface

  interface operator(+)
     module procedure bounded_add_bbb !, bounded_add_bbi, &
                    ! bounded_add_bib, bounded_add_ibb,   &
                    ! bounded_add_iib, bounded_add_ibi,   &
                    ! bounded_add_bii
  end interface

  private :: bounded_assign_bb, bounded_assign_bi, &
             bounded_add_bbb

contains

  subroutine set_bound(bi, lower, upper, critical, value)
    type(BoundedInteger), intent(out) :: bi
    integer, intent(in) :: lower, upper
    integer, intent(in), optional :: value
    logical, intent(in), optional :: critical

    bi%from = min(lower, upper)
    bi%to = max(lower, upper)
    if ( present(critical) ) then
       bi%critical = critical
    else
       bi%critical = .false.
    end if
    if ( present(value) ) then
       bi = value
    end if
  end subroutine set_bound

  subroutine bounded_assign_bb(a, b)
    type(BoundedInteger), intent(out) :: a
    type(BoundedInteger), intent(in)  :: b

    call bounded_assign_bi(a, b%v)

  end subroutine bounded_assign_bb


  subroutine bounded_assign_bi(a, b)
    type(BoundedInteger), intent(out) :: a
    integer,              intent(in)  :: b

    if ( (a%from <= b) .and. (a%to >= b) ) then
       a%v = b
    else
       write(0,*) "BoundedInteger: out of bound assignment"
       if ( a%critical ) then
          stop
       else
          if ( b < a%from ) then
             a%v = a%from
          else
             a%v = a%to
          end if
          write(0,"(A,' (',I0, ')')") "BoundedInteger: set to nearest bound", a%v
       end if
    end if
  end subroutine bounded_assign_bi


  function bounded_add_bbb(a, b) result(c)
    type(BoundedInteger) :: c
    type(BoundedInteger), intent(in) :: a, b

    integer :: t

    c%from = max(a%from, b%from)
    c%to   = min(a%to,   b%to)
    t = a%v + b%v
    if ( c%from <= t .and. c%to >= t ) then
       c%v = t
    else
       write(0,*) "BoundedInteger: out of bound sum"
       if ( a%critical .or. b%critical ) then
          stop
       else
          if ( t < c%from ) then
             c%v = c%from
          else
             c%v = c%to
          end if
          write(0,"(A,' (',I0,')')") "BoundedInteger: set to nearest bound", c%v
       end if
    end if
  end function bounded_add_bbb

end module Bounded
```



```fortran
program BoundedTest
  use Bounded
  implicit none

  type(BoundedInteger)     ::  a, b, c

  call set_bound(a, 1, 10)
  ! if we want to stop the program if a is out of bounds...
  ! call set_bound(a, 1, 10, critical=.true.)
  call set_bound(b, 1, 10)
  call set_bound(c, 1, 10)
  ! if we want to init c to a specific value...:
  ! call set_bound(c, 1, 10, value=6)

  a = 1         ! ok
  a = 4         ! ok
  a = -1        ! warning (a=1)
  a = 11        ! warning (a=10)
  a = 3         ! ok
  b = a         ! ok
  c = a + b     ! ok (3+3)
  c = c + a     ! ok (6+3=9)
  c = c + b     ! warning (c=10)

end program BoundedTest
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Type MyInteger
  Private:
    Dim i_ As Integer
  Public:
    Declare Constructor(i_ As Integer)
    Declare Property I() As Integer
    Declare Operator Cast() As Integer
    Declare Operator Cast() As String
End Type

Constructor MyInteger(i_ As Integer)
  If i_ < 1 Then
    i_ = 1
  ElseIf i_ > 10 Then
    i_ = 10
  End If
  This.i_ = i_
End Constructor

Property MyInteger.I() As Integer
  Return i_
End Property

Operator MyInteger.Cast() As Integer
  Return i_
End Operator

Operator MyInteger.Cast() As String
  Return Str(i_)
End Operator

Dim i As MyInteger = 11 ' implicit constructor call;  i_ automatically limited to 10
Dim j As MyInteger = 3  ' implicit constructor call;  no adjustment needed
Dim k As Integer = 4
Print "i = "; i; "   j = " ; j; "   k = "; k; "   j + 6 ="; j.I + 6; "   j + k ="; j + k
Print
Print "Press any key to quit"
Sleep
```


```txt

i = 10   j = 3   k =  4   j + 6 = 9   j + k = 7

```



## Free Pascal

''See [[#Pascal|Pascal]]''

```pascal
type
	range = 1..10;
var
	n: range;
begin
	n := 10;
	{$rangeChecks on}
	n := n + 10; // will yield a run-time error
end;
```

The FPC (Free Pascal compiler) by default does ''not generate'' range checks.
Assigning a value out of range is possible.
Only constant expressions, i. e. expressions that can be evaluated entirely during compile-time, are not accepted, if they are out of range.

The compiler directive <tt>{$rangeChecks on}</tt> will enable insertion of range checks.
If an out of range assignment is attempted, a run-time error occurs.

Note, in <tt>{$mode Delphi}</tt> range checks are only switchable at procedural level, not on a per expression-basis.


## Go

Creating a type that behaves like an integer but yet is distinct from it can be done in Go subject to the following constraints:

1. An instance of the new type needs to be created via a factory function (Go doesn't have constructors or user defined literals) to ensure it lies between 1 and 10. If an attempt is made to create an instance outside these limits, the value is adjusted so that it is just within them.

2. Go doesn't support operator overloading and so methods need to be defined for the standard operations on integers. As in the case of the Kotlin entry, only the basic arithmetic operations and the increment and decrement operations have been catered for below.

```go
package main

import "fmt"

type TinyInt int

func NewTinyInt(i int) TinyInt {
    if i < 1 {
        i = 1
    } else if i > 10 {
        i = 10
    }
    return TinyInt(i)
}

func (t1 TinyInt) Add(t2 TinyInt) TinyInt {
    return NewTinyInt(int(t1) + int(t2))
}

func (t1 TinyInt) Sub(t2 TinyInt) TinyInt {
    return NewTinyInt(int(t1) - int(t2))
}

func (t1 TinyInt) Mul(t2 TinyInt) TinyInt {
    return NewTinyInt(int(t1) * int(t2))
}

func (t1 TinyInt) Div(t2 TinyInt) TinyInt {
    return NewTinyInt(int(t1) / int(t2))
}

func (t1 TinyInt) Rem(t2 TinyInt) TinyInt {
    return NewTinyInt(int(t1) % int(t2))
}

func (t TinyInt) Inc() TinyInt {
    return t.Add(TinyInt(1))
}

func (t TinyInt) Dec() TinyInt {
    return t.Sub(TinyInt(1))
}

func main() {
    t1 := NewTinyInt(6)
    t2 := NewTinyInt(3)
    fmt.Println("t1      =", t1)
    fmt.Println("t2      =", t2)
    fmt.Println("t1 + t2 =", t1.Add(t2))
    fmt.Println("t1 - t2 =", t1.Sub(t2))
    fmt.Println("t1 * t2 =", t1.Mul(t2))
    fmt.Println("t1 / t2 =", t1.Div(t2))
    fmt.Println("t1 % t2 =", t1.Rem(t2))
    fmt.Println("t1 + 1  =", t1.Inc())
    fmt.Println("t1 - 1  =", t1.Dec())
}
```


```txt

t1      = 6
t2      = 3
t1 + t2 = 9
t1 - t2 = 3
t1 * t2 = 10
t1 / t2 = 2
t1 % t2 = 1
t1 + 1  = 7
t1 - 1  = 5

```



## Haskell


Haskell doesn't have any built-in subrange types. However, it is possible to declare arbitrary types that "behave like" any of the built-in types on the "usual" numeric etc. operations, because these operations are defined by type-classes. So we generalize the task a bit, and first declare a generic container type that supports an additional ''check'' operation. Then, we lift any operation in the base type to the container type, by executing the check after each operation:


```haskell
{-# OPTIONS -fglasgow-exts #-}

data Check a b = Check { unCheck :: b } deriving (Eq, Ord)

class Checked a b where
  check :: b -> Check a b

lift  f x = f (unCheck x)
liftc f x = check $ f (unCheck x)

lift2  f x y = f (unCheck x) (unCheck y)
lift2c f x y = check $ f (unCheck x) (unCheck y)
lift2p f x y = (check u, check v) where (u,v) = f (unCheck x) (unCheck y)

instance Show b => Show (Check a b) where
  show (Check x)        = show x
  showsPrec p (Check x) = showsPrec p x

instance (Enum b, Checked a b) => Enum (Check a b) where
  succ = liftc succ
  pred = liftc pred
  toEnum   = check . toEnum
  fromEnum = lift fromEnum

instance (Num b, Checked a b) => Num (Check a b) where
  (+) = lift2c (+)
  (-) = lift2c (-)
  (*) = lift2c (*)
 negate = liftc negate
  abs    = liftc abs
   signum = liftc signum
  fromInteger = check . fromInteger

instance (Real b, Checked a b) => Real (Check a b) where
  toRational = lift toRational

instance (Integral b, Checked a b) => Integral (Check a b) where
  quot = lift2c quot
  rem  = lift2c rem
  div  = lift2c div
  mod  = lift2c mod
  quotRem = lift2p quotRem
  divMod  = lift2p divMod
  toInteger = lift toInteger
```

Now we can declare the a subrange 1..10 of integer like this:


```haskell
newtype TinyInt = TinyInt Int

instance Checked TinyInt Int where
  check x | x >= 0 && x <= 10  =  Check x
          | otherwise          =  error "Out of range"
```

In the same way, we could now declare the subtype of the even integers:

```haskell
newtype EvenInt = EvenInt Int

 instance Checked EvenInt Int where
   check x | even x     =  Check x
           | otherwise  =  error "Not even"
```


Similarly, we could declare the subtype of floating point numbers with restricted exponent, and so on.


## Java

The closest you can get to defining a primitive type in Java is making a new wrapper class with methods for math operations.

This example class throws an exception if the value is out of the bounds; it is implemented only in the assignment method "assign" and the addition method "add". The class can be easily extended.


```java
class BoundedIntOutOfBoundsException extends Exception
{
  public BoundedIntOutOfBoundsException(int v, int l, int u) {
    super("value " + v + " is out of bounds [" + l + "," + u + "]");
  }
}

class BoundedInt {
  private int value;
  private int lower;
  private int upper;

  public BoundedInt(int l, int u) {
    lower = Math.min(l, u);
    upper = Math.max(l, u);
  }

  private boolean checkBounds(int v) {
    return (v >= this.lower) && (v <= this.upper);
  }

  public void assign(BoundedInt i) throws BoundedIntOutOfBoundsException {{
    assign(i.value()); //could still throw Exception if the other BoundedInt has different bounds
  }

  public void assign(int v) throws BoundedIntOutOfBoundsException {
    if ( checkBounds(v) ) {
      this.value = v;
    } else {
      throw new BoundedIntOutOfBoundsException(v, this.lower, this.upper);
    }
  }

  public int add(BoundedInt i) throws BoundedIntOutOfBoundsException {
    return add(i.value());
  }

  public int add(int i) throws BoundedIntOutOfBoundsException {
    if ( checkBounds(this.value + i) ) {
      this.value += i;
    }  else {
      throw new BoundedIntOutOfBoundsException(this.value + i, this.lower, this.upper);
    }
    return this.value;
  }

  public int value() {
    return this.value;
  }
}


public class Bounded {
  public static void main(String[] args) throws BoundedIntOutOfBoundsException {
    BoundedInt a = new BoundedInt(1, 10);
    BoundedInt b = new BoundedInt(1, 10);

    a.assign(6);
    try {
      b.assign(12);
    } catch (Exception e) {
      System.out.println(e.getMessage());
    }
    b.assign(9);
    try {
      a.add(b.value());
    } catch (Exception e) {
      System.out.println(e.getMessage());
    }
  }
}
```



## JavaScript



```JavaScript
function Num(n){
    n = Math.floor(n);
    if(isNaN(n))
        throw new TypeError("Not a Number");
    if(n < 1 || n > 10)
        throw new TypeError("Out of range");
    this._value = n;
}
Num.prototype.valueOf = function() { return this._value; }
Num.prototype.toString = function () { return this._value.toString();}

var w = new Num(3), x = new Num(4);

WScript.Echo(w + x); //7
WScript.Echo(x - w); //1
WScript.Echo(w * x); //12
WScript.Echo(w / x); //0.75
WScript.Echo(w < x); //true
WScript.Echo(x < w); //false

var y = new Num(0); //TypeError
var z = new Num(11); //TypeError

```



## jq

In the following, we define a new type named "smallint", with arithmetic operations that either return the "smallint" one would expect from ordinary arithmetic, or else return nothing (an empty stream), as opposed to returning null or raising an error.  The choice of an empty stream is made to highlight this alternative.

By design, jq's types are constrained to be JSON types, so jq's type system cannot be used to create a new type, but we can create a parallel type system based on JSON objects.  We shall do so using the convention that if an object has a key named "type", then the type of the object will be the given value:
```jq
def typeof:
  if type == "object" and has("type") then .type else type end;
```
We also define a generic "pretty-print" filter:

```jq
def pp:
  if type == "object" and has("type") then "\(.type)::\(.value)"
  else .
  end;
```
Our instances of smallint will look like this: {"type": "smallint", "value": 0}

This definition ensures that jq's basic tests for equality and inequality (== and !=) can be used for the instances of smallint.

In fact the builtin comparison functions (< and >) will also have the correct semantics, as will sort and unique.

As of this writing, the official release of jq does not support modules, so we will not use jq's new module system here, but it would allow us to place all the smallint functions in a Smallint module.

To generate instances of smallint, we define a function, smallint(i), that will return an instance corresponding to an integer, i, if it is in range.  As noted above, it will otherwise return nothing at all (as opposed to null or raising an error):
```jq
def smallint(i): i as $i
  | if (i|type) == "number" and i == (i|floor) and i > 0 and i < 11 then {"type": "smallint", "value": i}
     else empty
     end ;

# A convenience function to save typing:
def s(i): smallint(i);

# To convert from the pretty-print representation back to smallint:
def tosmallint:
  if type == "string" and startswith("smallint::") then
     split("::") | smallint( .[1] | tonumber )
  else empty
  end ;
```
That leaves just basic arithmetic operations: add minus times mod div
```jq
def add(a;b): smallint(a.value + b.value);
def minus(a;b): smallint(a.value - b.value);
def times(a;b): smallint(a.value * b.value);
def mod(a;b): smallint(a.value % b.value);
def divide(a;b): smallint( (a.value / b.value) | floor );
```
Examples:
```jq
s(1) < s(3)            # => true
add( s(1); s(2)) | pp  # "smallint::3"
add( s(6); s(6))       # (nothing)
```



## Julia

Julia has true, machine-optimized user defined primitives, but they are defined as contiguous groups of N bits:


```julia
struct LittleInt <: Integer
    val::Int8
    function LittleInt(n::Real)
        1 ≤ n ≤ 10 || throw(ArgumentError("LittleInt number must be in [1, 10]"))
        return new(Int8(n))
    end
end
Base.show(io::IO, x::LittleInt) = print(io, x.val)
Base.convert(::Type{T}, x::LittleInt) where T<:Number = convert(T, x.val)
Base.promote_rule(::Type{LittleInt}, ::Type{T}) where T<:Number = T

for op in (:+, :*, :÷, :-, :&, :|, :$, :<, :>, :(==))
    @eval (Base.$op)(a::LittleInt, b::LittleInt) = LittleInt(($op)(a.val, b.val))
end

# Test
a = LittleInt(3)
b = LittleInt(4.0)
@show a b
@show a + b
@show b - a
@show a * LittleInt(2)
@show b ÷ LittleInt(2)
@show a * b
```


```txt
a = 3
b = 4
a + b = 7
b - a = 1
a * LittleInt(2) = 6
b ÷ LittleInt(2) = 2
ERROR: LoadError: ArgumentError: LittleInt number must be in [1, 10]
```



## Kotlin

There are limits to what can be done to create a 'work-alike' primitive type in Kotlin as the language doesn't support either user-defined literals or implicit conversions between types.

However, subject to that, the following code creates a new primitive type called TinyInt whose values lie between 1 and 10 inclusive. Instead of throwing an exception, attempting to create such a type with an out of bounds value results in a value which is within bounds i.e. 1 if the value is less than 1 or 10 if the value is greater than 10.

Rather than attempt to reproduce all functions or operators which the Int type supports, only the basic arithmetic operators and the increment and decrement operators are catered for below:

```scala
// version 1.1

class TinyInt(i: Int) {
    private val value = makeTiny(i)

    operator fun plus (other: TinyInt): TinyInt = TinyInt(this.value + other.value)
    operator fun minus(other: TinyInt): TinyInt = TinyInt(this.value - other.value)
    operator fun times(other: TinyInt): TinyInt = TinyInt(this.value * other.value)
    operator fun div  (other: TinyInt): TinyInt = TinyInt(this.value / other.value)
    operator fun rem  (other: TinyInt): TinyInt = TinyInt(this.value % other.value)

    operator fun inc()  = TinyInt(this.value + 1)
    operator fun dec()  = TinyInt(this.value - 1)

    private fun makeTiny(i: Int): Int =
        when {
            i < 1  -> 1
            i > 10 -> 10
            else   -> i
        }

    override fun toString(): String = value.toString()
}

fun main(args: Array<String>) {
    var t1 = TinyInt(6)
    var t2 = TinyInt(3)
    println("t1      = $t1")
    println("t2      = $t2")
    println("t1 + t2 = ${t1 + t2}")
    println("t1 - t2 = ${t1 - t2}")
    println("t1 * t2 = ${t1 * t2}")
    println("t1 / t2 = ${t1 / t2}")
    println("t1 % t2 = ${t1 % t2}")
    println("t1 + 1  = ${++t1}")
    println("t2 - 1  = ${--t2}")
}
```


```txt

t1      = 6
t2      = 3
t1 + t2 = 9
t1 - t2 = 3
t1 * t2 = 10
t1 / t2 = 2
t1 % t2 = 1
t1 + 1  = 7
t2 - 1  = 2

```



## Lasso


```lasso
define dint =
 type {
   data private value

   public oncreate(value::integer) => {
	fail_if(#value < 1,#value+' less than 1 ')
	fail_if(#value > 10,#value+' greater than 10')
	.value = #value
   }

   public +(rhs::integer) => dint(.value + #rhs)
   public -(rhs::integer) => dint(.value - #rhs)
   public *(rhs::integer) => dint(.value * #rhs)
   public /(rhs::integer) => dint(.value / #rhs)
   public %(rhs::integer) => dint(.value % #rhs)

   public asstring() => string(.value)

}

dint(1) // 1
dint(10) // 10

dint(0) // Error: 0 less than 1
dint(2) - 5 // Error: -3 less than 1

dint(11) // Error: 11 greater than 10
dint(10) + 1 // Error: 11 greater than 10
dint(10) * 2 // Error: 20 greater than 10

```



## M2000 Interpreter

We can make a Group to get and return value, a numeric value (if no $ used in name). Here we make operators ++ -- += and -=. To use it as a group in right expression we have to use Group(). Also we need Group() to push as group, at the last statement of CheckThis, where M is a pointer to a named group (K), which is not a movable Group (when module end run, this group erased), so cannot returned from module (if we do that we get a broken reference, which refer to a non exist group).

Class typeA, always return a unnamed group (a movable one) as a copy of an internal group (referred as This in constructor). Here constructor used once and because is in Class: part, can't copied to movable copy. Private variable mval can be visible only from "this" object. Statement .mval++ is same as This.mval++. Properties min and max has only value part, are read only. Min and max are groups also, and we can add anything that a group can have, using a second definition as Group min {  }.  Also a Property Min make a private variable as [min] so This.[Min] or .[Min] can be read or write inside modules/functions/operators of typeA and those who we insert after. Look at Set {} we place value to .mval using <=. If we use = then we make a local variable in Set not the private variable of group.

Every Module/Function/Operator/Sub/Thread can use stack of values. Rules are simple. Functions/Operators use new stack of values each time we call them in expressions. Module and Function which call using Call, or modules and subs by name use parent stack. Events (call functions) use new stack for each call.

We use stack to push a group (a copy form a pointer M, which points to K), as return value from module.

We can use Currency, Decimal, Double, Single, Long, Integer for primitive data types. When we see type using Type$() we get "Group" (the user object type) but using TypeExp$() (defined in this example) we get group value's type, as Currency here.


```M2000 Interpreter

Module CheckDataType {
      Module CheckThis {
            Class typeA {
            Private:
                  mval as currency
            Public:
                  Property min {value}=-1
                  Property max {value}=0
                  Operator "++" {
                        if .mval=.[max] then Error "Number bigger than Max Value"
                        .mval++
                  }
                  Operator "--" {
                        if .mval=.[min] then Error "Number lower than Min Value"
                        .mval--
                  }
                  Operator "+=" (x){
                        if .mval>.[max]+x then Error "Number bigger than Max Value"
                        .mval+=x
                  }
                  Operator "-=" (x) {
                        if .mval<.[min]-x then Error "Number lower than Min Value"
                        .mval-=x
                  }
                  Value {
                    =.mval
                  }
                  Set (x) {
                        if x>.[max] then Error "Number bigger than Max Value"
                        if x<.[min] then Error "Number lower than Min Value"
                        .mval<=int(x)
                  }
            Class:
                  Module typeA (.[min], .[max]) {
                      .mval<=.[min]
                  }
            }
            K=typeA(1, 10)
            K=5
            Def TypeExp$(x)=Type$(x)
            Print Type$(K)="Group", TypeExp$(K)="Currency"
            K--
            Print K
            K=K+1
            Print K, K.Max, K.Min
            K+=4
            Print K, K.Max, K.Min
            Print -K*4=-36
            Try Ok {
                  K=400
            }
            If  Error or Not Ok then Print Error$
            \\ make a new K as K with name Z
            Z=Group(K)
            Print Z.Max, Z.Min, Z
            K++
            Z=Group(K)
            Print Z=K
            K-=5
            \\ We make M as pointer to K
            M->K
            Print Eval(M), K
            For M {This++}
            Print Eval(M), K
            For M {This=8}
            Print Eval(M), K
            Push Group(M), Group(M)
      }
      CheckThis
      \\ Read Group from stack of values and place a copy in each item in A()
      Dim A(4)=Group
      \\ We have one more group in stack, so we read it to new variable What
      \\ What isn't a pointer to object, is the object (internal is a pointer to objet but isn't usable for M2000)
      Read What
      What++
      Print What.Max=10, What.Min=1, What=9  ' True True True
      Print A(2).Max=10, A(2).Min=1, A(2)=8 ' True True True
}
CheckDataType

```



## MATLAB

In a weird way MATLAB has no primitive data types. All data types are defined as a MATLAB class somewhere in the MATLAB install directory. Therefore, to define a primitive data type, you define a class. Below is a class called "RingInt" that has the properties of an integer data type, but the values are restricted in the range [1,10]. Include in the definition are functions that overload some of the built-in MATLAB operators, e.g. addition and subtraction.

In a folder named "@RingInt"
RingInt.m:

```MATLAB
classdef RingInt

    properties
        value
    end

    methods

        %RingInt constructor
        function theInt = RingInt(varargin)
            if numel(varargin) == 0
                theInt.value = 1;
            elseif numel(varargin) > 1
                error 'The RingInt constructor can''''t take more than 1 argument.';
            else

                %Makes sure any doubles are coerced to ints
                if not(isinteger(varargin{1}))
                    varargin{1} = int32(varargin{1});
                end

                %Maps out of bound values to the proper range
                if varargin{1} > 10
                    theInt.value = varargin{1} - (10 * (idivide(varargin{1},10,'ceil') - 1));
                elseif varargin{1} < 1
                    theInt.value = varargin{1} + (10 * (idivide(abs(varargin{1}),10,'floor') + 1));
                else
                    theInt.value = varargin{1};
                end
            end
        end %constructor

        %Overload the "+" operator
        function sum = plus(firstNumber,secondNumber)

            if isa(firstNumber,'RingInt') && isa(secondNumber,'RingInt')
                sum = firstNumber.value + secondNumber.value;
            elseif isa(firstNumber,'RingInt') && not(isa(secondNumber,'RingInt'))
                sum = firstNumber.value + secondNumber;
            else
                sum = secondNumber.value + firstNumber;
            end

            sum = RingInt(sum);

        end %+

        %Overload the "-" operator
        function difference = minus(firstNumber,secondNumber)

            if isa(firstNumber,'RingInt') && isa(secondNumber,'RingInt')
                difference = firstNumber.value - secondNumber.value;
            elseif isa(firstNumber,'RingInt') && not(isa(secondNumber,'RingInt'))
                difference = firstNumber.value - secondNumber;
            else
                difference = firstNumber - secondNumber.value;
            end

            difference = RingInt(difference);

        end %-

        %Overload the "==" operator
        function trueFalse = eq(firstNumber,secondNumber)

            if isa(firstNumber,'RingInt') && isa(secondNumber,'RingInt')
                trueFalse = firstNumber.value == secondNumber.value;
            else
                error 'You can only compare a RingInt to another RingInt';
            end

        end %==


        %Overload the display() function
        function display(ringInt)
            disp(ringInt);
        end

        %Overload the disp() function
        function disp(ringInt)
            disp(sprintf('\nans =\n\n\t %d\n',ringInt.value));
        end

    end %methods
end %classdef


```


Sample Usage:

```matlab
>
 RingInt(10) + 1

ans =

	 1

>> RingInt(5) - 20

ans =

	 5

>> a = RingInt(3)

ans =

	 3

>> a == RingInt(3)

ans =

     1
```


=={{header|Modula-3}}==
In Modula-3, subrange types are automatically subtypes of their base type, so if you define a type called <code>MyInt</code> to be the subrange [1..10] then <code>MyInt</code> is a subtype of <code>INTEGER</code>.  If we defined <code>MyChar</code> as the subrange ['A'..'C'] then <code>MyChar</code> would be a subtype of <code>CHAR</code>.

```modula3
TYPE MyInt = [1..10];
```

<code>MyInt</code> can now be used anywhere an <code>INTEGER</code> can, such as the standard arithmetic functions. Trying to assign a value outside of the range of <code>MyInt</code> will result in a compile time warning, and/or a runtime error.


## Nim

Subranges are supported by the language and automatically boundchecked:

```python
type
  MyInt = range[0..10]

var x: MyInt = 5

x = x + 6  # Runtime error: value out of range: 11

x = 12 # Compile-time error: conversion from int literal(12) to MyInt is invalid
```



## OCaml


```ocaml
exception Out_of_bounds

type 'a bounds = { min: 'a; max: 'a }

type 'a bounded = { value: 'a; bounds: 'a bounds }

let mk_bounds ~min ~max = { min=min; max=max } ;;
(** val mk_bounds : min:'a -> max:'a -> 'a bounds *)

let check_bounds ~value ~bounds =
  if value < bounds.min || value > bounds.max then
    raise Out_of_bounds ;;
(** val check_bounds : value:'a -> bounds:'a bounds -> unit *)

let mk_bounded ~value ~bounds =
  check_bounds ~value ~bounds;
  { value=value; bounds=bounds } ;;
(** val mk_bounded : value:'a -> bounds:'a bounds -> 'a bounded *)

let op f a b =
  if a.bounds <> b.bounds then
    invalid_arg "different bounds";
  let res = f a.value b.value in
  check_bounds res a.bounds;
  (mk_bounded res a.bounds)
  ;;
(** val op : ('a -> 'a -> 'a) -> 'a bounded -> 'a bounded -> 'a bounded *)
```


Using in the interactive top level:

```ocaml
# let range = mk_bounds 1 10 ;;
val range : int bounds = {min = 1; max = 10}

# let a = mk_bounded 2 range ;;
val a : int bounded = {value = 2; bounds = {min = 1; max = 10}}

# let b = mk_bounded 5 range ;;
val b : int bounded = {value = 5; bounds = {min = 1; max = 10}}

# let c = mk_bounded 14 range ;;
Exception: Out_of_bounds.

# op ( + ) a b ;;
- : int bounded = {value = 7; bounds = {min = 1; max = 10}}
```


which can be used with floats in the same way:

```ocaml
# let rf = mk_bounds 1.0 10.0 ;;
val rf : float bounds = {min = 1.; max = 10.}

# let a = mk_bounded 2.2 rf
  and b = mk_bounded 5.6 rf in
  op ( +. ) a b ;;
- : float bounded = {value = 7.8; bounds = {min = 1.; max = 10.}}
```



## ooRexx


```oorexx
/* REXX ----------------------------------------------------------------
* 21.06.2014 Walter Pachl
* implements a data type tinyint that can have an integer value 1..10
* 22.06.2014 WP corrected by Rony Flatscher to handle arithmetic
*---------------------------------------------------------------------*/
a=.tinyint~new(1)  ; Say 'a='||a~value
Say '2*a='||(2*a)
Say 'a*2='||((0+a)*2)
say "---> rony was here: :)"
say "The above statement was in effect: '(0+a)*2', NOT 'a*2*, hence it worked!"
say "These statements work now:"
say "(a)*2:" (a)*2
say "a*2:  " a*2
say "<--- rony was here, The end. :)"
b=.tinyint~new(11); Say 'b='||b~value
b=.tinyint~new('B'); Say 'b='||b~value
say 'b='||(b)           -- show string value
Say '2*b='||(2*b)
::class tinyint
::method init
  Expose v
  Use Arg i
  Select
    When datatype(i,'W') Then Do
      If i>=1 & i<=10 Then
        v=i
      Else Do
        Say 'Argument 1 must be between 1 and 10'
        Raise Syntax 88.907 array(1,1,10,i)
        End
      End
    Otherwise Do
      Say 'Argument 1 must be a whole number between 1 and 10'
      Raise Syntax 88.905 array(1,i)
      End
    End
::method string
  Expose v
  Return v
::method value
  Expose v
  Return v

-- rgf, 20140622, intercept unknown messages, forward arithmetic messages to string value
::method unknown
  expose v
  use arg methName, methArgs
  if wordpos(methName, "+ - * / % //")>0 then  -- an arithmetic message in hand?
    forward message (methName) to (v) array (methArgs[1])

```

'''output'''

```txt
a=1
2*a=2
a*2=2
---> rony was here: :)
The above statement was in effect: '(0+a)*2', NOT 'a*2*, hence it worked!
These statements work now:
(a)*2: 2
a*2:   2
<--- rony was here, The end. :)
Argument 1 must be between 1 and 10

    28 *-*               Raise Syntax 88.907 array(1,1,10,i)
       *-* Compiled method NEW with scope "Object"
    14 *-* b=.tinyint~new(11);
Error 88 running D:\tinyint2.rex line 28:  Invalid argument
Error 88.907:  Argument 1 must be in the range 1 to 10; found "11"

```



## Oz

Normally new data types are implemented as classes or as abstract data types in modules. We cannot extend operators, though.

In this case we are lucky. As a feature of constraint programming we can easily constrain the domain of integers.


```oz
declare
  I
in
  I::1#10
  I = {Pow 2 4}
```


Output:

```txt

%***************************** failure **************************
%**
%** Tell: I{1#10} = 16
%**
%** Call Stack:
%** procedure 'IntPow' in file "/Users/raph/devel/mozdss-branch/mozart/share/lib/base/Number.oz", line 32, column 3, PC = 16461488
%**--------------------------------------------------------------

```



## Pascal


```pascal
type
	naturalTen = 1..10;
```

As per ISO 7185, any (attempted) assignment out of range constitutes an error.
However, the processor (usually a compiler) is permitted to leave such an error undetected.
Modern compilers such as [[#Delphi|Delphi]] and [[#Free Pascal|FPC]] give the programmer a choice, which behavior is desired, by providing compiler switches/directives.


## Perl

```perl
package One_To_Ten;
use Carp qw(croak);
use Tie::Scalar qw();
use base qw(Tie::StdScalar);

sub STORE {
    my $self = shift;
    my $val = int shift;
    croak 'out of bounds' if $val < 1 or $val > 10;
    $$self = $val;
};

package main;
tie my $t, 'One_To_Ten';
$t = 3;   # ok
$t = 5.2; # ok, silently coerced to int
$t = -2;  # dies, too small
$t = 11;  # dies, too big
$t = 'xyzzy';
# dies, too small. string is 0 interpreted numerically
```



## Perl 6



```perl6
subset OneToTen of Int where 1..10;

my OneToTen $n = 5;
$n += 6;
```


Here the result 11 fails to smartmatch against the range <code>1..10</code>, so the second assignment throws an exception. You may use any valid smartmatch predicate within a <code>where</code> clause, so the following one-argument closure is also legal:


```perl6
subset Prime of Int where -> $n { $n > 1 and so $n %% none 2 .. $n.sqrt }
```



## Phix

In Phix types are special functions that may be used in declaring the allowed values for a variable. A type must have exactly one parameter and should return true (non-zero) or false (zero). Types can also be called just like other functions. The types '''object''', '''sequence''', '''string''', '''atom''' and '''integer''' are predefined.

```Phix
type iten(integer i)
    return i>=1 and i<=10
end type
```

You can then declare variables of the new type just as you would the builtins

```Phix
integer i
iten i10
```

and typechecking occurs automatically

```Phix
i = 11  -- fine
i10 = 11  -- runtime error
```



## PicoLisp

```PicoLisp
(class +BoundedInt)
# value lower upper

(dm T (Low Up)
   (=: lower (min Low Up))
   (=: upper (max Low Up)) )

(de "checkBounds" (Val)
   (if (>= (: upper) Val (: lower))
      Val
      (throw 'boundedIntOutOfBounds
         (pack
            "value " Val
            " is out of bounds [" (: lower) "," (: upper) "]" ) ) ) )

(dm set> (Val)
   (=: value ("checkBounds" Val)) )

(dm +> (Val)
   (=: value ("checkBounds" (+ Val (: value)))) )

(dm val> ()
   (: value) )

(de main ()
   (let (A (new '(+BoundedInt) 1 10)  B (new '(+BoundedInt) 1 10))
      (set> A 6)
      (when (catch 'boundedIntOutOfBounds (set> B 12) NIL)
         (prinl @) )
      (set> B 9)
      (when (catch 'boundedIntOutOfBounds (+> A (val> B)) NIL)
         (prinl @) ) ) )
```

Output:

```txt
: (main)
value 12 is out of bounds [1,10]
value 15 is out of bounds [1,10]
```



## PowerShell


```PowerShell

[Int][ValidateRange(1,10)]$n = 3 #  $n can only accept integers between 1 and 10

```



## Python

This doesn't really apply as Python names don't have a type, but something can be done:

```python
>>
 class num(int):
    def __init__(self, b):
        if 1 <= b <= 10:
            return int.__init__(self+0)
        else:
            raise ValueError,"Value %s should be >=0 and <= 10" % b


>>> x = num(3)
>>> x = num(11)

Traceback (most recent call last):
  File "<pyshell#394>", line 1, in <module>
    x = num(11)
  File "<pyshell#392>", line 6, in __init__
    raise ValueError,"Value %s should be >=0 and <= 10" % b
ValueError: Value 11 should be >=0 and <= 10
>>> x
3
>>> type(x)
<class '__main__.num'>
>>>
```



## Racket


Most Racket programmers will use contracts to enforce additional guarantees on values. In the following example, the program exports <tt>x</tt> with a contract that ensures it is a number between 1 and 10.


```racket

#lang racket

(provide (contract-out [x 1-to-10/c]))

(define 1-to-10/c (between/c 1 10))

(define x 5)

```


In Typed Racket, it is possible to define a type that only contains the integers from 1 to 10. However, this type is inconvenient to use and is unlikely to be used in practice.


```racket

#lang typed/racket

(define-type 1UpTo10 (U 1 2 3 4 5 6 7 8 9 10))

;; type-checks
(: x 1UpTo10)
(define x 3)

;; does not type-check
(: y 1UpTo10)
(define y 18)

```



## Retro


```Retro
{{
  variable update
---reveal---
  : .limited @update &! &@ if update off ;
  : to       dup 1 10 within [ update on ] [ drop "Out of bounds\n" puts ] if ;
  : limited: create 1 , &.limited reclass ;
}}
```


This creates a data element that returns a value from 1 to 10. Alteration of the value is possible using '''to'''.


```Retro
limited: foo
1 to foo
foo .s
51 to foo
foo .s
bye
```



## Ruby

ref http://codeidol.com/other/rubyckbk/Numbers/Simulating-a-Subclass-of-Fixnum/

 Some object-oriented languages won't let you subclass the "basic" data types
 like integers. Other languages implement those data types as classes, so you
 can subclass them, no questions asked. Ruby implements numbers as classes
 (Integer, with its concrete subclasses Fixnum and Bignum), and you can subclass
 those classes. If you try, though, you'll quickly discover that your subclasses
 are useless: they don't have constructors.

 Ruby jealously guards the creation of new Integer objects. This way it ensures
 that, for instance, there can be only one Fixnum instance for a given number

 The easiest way to delegate all methods is to create a class that's nearly empty
 and define a method_missing method.


```ruby
require 'test/unit'
include Test::Unit::Assertions

class MyInt
  @@min = 1
  @@max = 10

  attr_reader :value
  private :value

  def initialize(val)
    begin
      v = Integer(val)
    rescue ArgumentError
      raise ArgumentError, "invalid value '#{val}', must be an integer"
    end

    unless v.between?(@@min, @@max)
      raise ArgumentError, "invalid value '#{v}', must be between #{@@min} and #{@@max}"
    end

    @value = v
  end

  def method_missing(m, *args)
    super unless @value.respond_to?(m)
    myint_args = args.collect do |arg|
      arg.kind_of?(self.class) ? arg.to_int : arg
    end
    result = @value.send(m, *myint_args)
    return result if m == :coerce
    case result
    when Integer
      MyInt.new(result)
    when Array
      result.collect do |element|
        element.kind_of?(Integer) ? MyInt.new(element) : element
      end
    else
      result
    end
  end

  def respond_to?(method)
    super or @value.respond_to? method
  end

  def to_int
    @value
  end
  def to_f
    Float(@value)
  end
  def to_s
    @value.to_s
  end
  def inspect
    to_s
  end
end


assert_raise(ArgumentError) { MyInt.new("foo") }    # => invalid value 'foo', must be an integer
assert_raise(ArgumentError) { MyInt.new(11) }       # => invalid value '11', must be an integer

a = MyInt.new(7)
b = MyInt.new(5)

c = 5 + a
assert_kind_of(Fixnum, c)
assert_equal(12, c)

c = a + 2
assert_kind_of(MyInt, c)
assert_equal(9, c.to_int)

c = a + 2.8
assert_kind_of(Float, c)
assert_equal(9.8, c)

c = a - b
assert_kind_of(MyInt, c)
assert_equal(2, c.to_int)

assert_raise(ArgumentError) { c = a + b }    # => invalid value '12', must be an integer
assert_raise(ArgumentError) { c = b - a }    # => invalid value '-2', must be an integer
```



## Rust


As of version 1.36.0, Rust does not provide special language support for declaring new types which are range-constrained versions of its built-in primitives.

However, it is considered idiomatic to use single-element "tuple structs" (data structures with unnamed fields) to wrap primitive types in containers which impose runtime restrictions on their use without altering their in-memory representation.

The Rust ecosystem refers to this as the "newtype pattern" and an example would be the <code>String</code> type from the standard library, which wraps a <code>Vec&lt;u8&gt;</code> to uphold a "contents are valid UTF-8" invariant and expose an alternative set of methods.

This idiom is bolstered by Rust's collection of traits (interfaces) which a type can implement to integrate with existing conversions and operators.

For example, it is common for functions which take paths as input but don't modify them to take <code>AsRef&lt;Path&gt;</code>, which allows them to transparently accept any type which has implemented that conversion.

A simple example of implementing the newtype pattern manually would look like this:


```rust
use std::convert::TryFrom;

mod test_mod {
    use std::convert::TryFrom;
    use std::fmt;

    // Because the `i8` is not `pub` this cannot be directly constructed
    // by code outside this module
    #[derive(Copy, Clone, Debug)]
    pub struct TwoDigit(i8);

    impl TryFrom<i8> for TwoDigit {
        type Error = &'static str;

        fn try_from(value: i8) -> Result<Self, Self::Error> {
            if value < -99 || value > 99 {
                Err("Number cannot fit into two decimal digits")
            } else {
                Ok(TwoDigit(value))
            }
        }
    }

    impl Into<i8> for TwoDigit {
        fn into(self) -> i8 { self.0 }
    }

    // This powers `println!`'s `{}` token.
    impl fmt::Display for TwoDigit {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.0)
        }
    }
}

pub fn main() {
    let foo = test_mod::TwoDigit::try_from(50).unwrap();
    let bar: i8 = foo.into();
    println!("{} == {}", foo, bar);
}
```


However, there exist crates such as [https://lib.rs/crates/derive_more derive_more] to automate the parts other than the restricted construction logic based on a simple <code>#[derive(Add, Copy, Clone, Debug, Div, Into, Mul, Sub)]</code> and so on and so forth.


## Scala

```Scala
  class TinyInt(val int: Byte) {
    import TinyInt._
    require(int >= lower && int <= upper, "TinyInt out of bounds.")

    override def toString = int.toString
  }

  object TinyInt {
    val (lower, upper) = (1, 10)

    def apply(i: Byte) = new TinyInt(i)
  }

  val test = (TinyInt.lower to TinyInt.upper).map(n => TinyInt(n.toByte))
```



## Sidef


```ruby
subset Integer    < Number  { .is_int }
subset MyIntLimit < Integer { . ~~ (1 ..^ 10) }

class MyInt(value < MyIntLimit) {

    method to_s      { value.to_s }
    method get_value { value.get_value }

    method ==(Number x) { value == x }
    method ==(MyInt  x) { value == x.value }

    method AUTOLOAD(_, name, *args) {
        var results = [value.(name)(args.map {|n| Number(n) }...)]
        results.map{|r| r.kind_of(Number) ? MyInt(r.int) : r}...
    }
}

#
## Example:
#
var a = MyInt(2)    # creates a new object of type `MyInt`
a += 7              # adds 7 to a
say a               # => 9
say a/2             # => 4

var b = (a - 3)     # b is of type `MyInt`
say b               # => 6

say a.as_hex.dump   # => "9" -- an hexadecimal string

a -= 6              # a=3
var c = (a + b)     # MyInt(3) + MyInt(6)
say c               # => 9
say c.class         # => MyInt

a *= 2              # a=6
say a+b             # error: class `MyInt` does not match MyInt(12)
```



## Tcl

Tcl does not attach types to values or variables, but it does allow the programmer to create traces on variables that can be used to enforce type-like constraints among other things. Traces are procedures that execute when variables are read, written and/or unset. (Traces are also available for commands and for the execution of a script.) Tcl's compiler does not enforce these constraints; they're strictly runtime entities.

```tcl
namespace eval ::myIntType {
    variable value_cache
    array set value_cache {}
    variable type integer
    variable min 1
    variable max 10
    variable errMsg "cannot set %s to %s: must be a $type between $min and $max"
}
proc ::myIntType::declare varname {
    set ns [namespace current]
    uplevel [list trace add variable $varname write ${ns}::write]
    uplevel [list trace add variable $varname unset ${ns}::unset_var]
}
proc ::myIntType::unset_var {varname args} {
    variable value_cache
    unset value_cache($varname)
}
proc ::myIntType::validate {value} {
    variable type
    variable min
    variable max
    expr {[string is $type -strict $value] && $min <= $value && $value <= $max}
}
proc ::myIntType::write {varname args} {
    variable value_cache
    upvar $varname var
    set value $var
    if {[validate $value]} {
        set value_cache($varname) $value
    } else {
        if {[info exists value_cache($varname)]} {
            set var $value_cache($varname)
        }
        variable errMsg
        error [format $errMsg $varname $value]
    }
}
```


So, in an interactive tclsh we can see:

```txt
% myIntType::declare foo
% set foo ;# regular Tcl error:  foo is declared but still unset
can't read "foo": no such variable
% set foo bar
can't set "foo": cannot set foo to bar: must be a integer between 1 and 10
% set foo 3
3
% incr foo 10
can't set "foo": cannot set foo to 13: must be a integer between 1 and 10
% incr foo -10
can't set "foo": cannot set foo to -7: must be a integer between 1 and 10
% set foo 0
can't set "foo": cannot set foo to 0: must be a integer between 1 and 10
% set foo
3
% set foo [expr {$foo * 1.5}]
can't set "foo": cannot set foo to 4.5: must be a integer between 1 and 10
% set foo
3
% unset foo
%
```



## Toka


```toka
needs quotes
{
  variable update
  [ update @ [ ! ] [ @ ] ifTrueFalse update off ] is action
  [ dup >r 0 11 r> within [ update on ] [ drop ." Out of bounds\n " ] ifTrueFalse ]
  [ ` [ invoke cell-size malloc # ` action compile ` ] invoke is ]
} is value:1-10:
  is to

value:1-10: foo
1 to foo
foo .
```



## UNIX Shell

ksh93 has
* compound variables and
* "discipline functions" which get fired at get/set/unset events

```bash
typeset -i boundedint
function boundedint.set {
    nameref var=${.sh.name}
    if (( 1 <= .sh.value && .sh.value <= 10 )); then
        # stash the valid value as a backup, in case we need to restore it
        typeset -i var.previous_value=${.sh.value}
    else
        print -u2 "value out of bounds"
        # restore previous value
        .sh.value=${var.previous_value}
    fi
}

boundedint=-5; echo $boundedint
boundedint=5;  echo $boundedint
boundedint=15; echo $boundedint
```

```txt
value out of bounds
0
5
value out of bounds
5
```



## Ursala

We define a new record type <code>my_number</code>, having two fields, of which
one is the number and the other is the pair of bounds. Fields in
records can have initializing functions, and any function can throw an
exception, so we make the initializing function of the number field
that which throws an exception if it's outside the bounds. The
initializing function of the bounds field sets its default values to 1
and 10. The initializing functions are automatically evaluated any
time a record of type <code>_my_number</code> is constructed by a function of the
form <code>my_number$[...]</code>, so we can proceed to define all the arithmetic
operations we need as functions of this form in terms of the
corresponding operations on natural numbers with no further bounds
checking required.

```Ursala
#import nat

my_number ::

the_number %n  -|~bounds.&BZ,~&B+ nleq~~lrlXPrX@G+ ~/the_number bounds|-?(~the_number,<'out of bounds'>!%)
bounds     %nW ~bounds.&B?(~bounds,(1,10)!)

add = my_number$[the_number: sum+ ~the_number~~]
mul = my_number$[the_number: product+ ~the_number~~]
```

test program:

```Ursala
#cast _my_number

total = add(my_number[the_number: 3],my_number[the_number: 4])
```

output:

```txt

my_number[the_number: 7,bounds: (1,10)]

```

test program demonstrating bounds checking:

```Ursala
#cast _my_number

total = mul(my_number[the_number: 3],my_number[the_number: 4])
```

compile-time diagnostic output:

```txt

fun:prim.fun:3:12: out of bounds

```

Note that restricted types are not idiomatic in Ursala if all we really want is integer arithmetic with run-time bounds checking, which can be done more directly like this.

```Ursala
#import nat

#library+

#import
   ^|A(~&,//+ nrange(1,10)?</~& <'out of bounds'>!%)*
   ~&n-=<'sum','difference','product','quotient','remainder'>*~ %QI nat
```

This code creates a new library of functions of natural numbers by selecting several of them by name from the standard <code>nat</code> library and putting a wrapper around each one that checks the bounds on the result and throws an exception if necessary. These functions can be used as drop-in replacements for the standard ones.


## Visual Basic


VB can't really do primitive data types, but they can be faked with classes.

TinyInt.cls:

```vb
Private mvarValue As Integer

Public Property Let Value(ByVal vData As Integer)
    If (vData > 10) Or (vData < 1) Then
        Error 380   'Invalid property value; could also use 6, Overflow
    Else
        mvarValue = vData
    End If
End Property

Public Property Get Value() As Integer
    Value = mvarValue
End Property

Private Sub Class_Initialize()
    'vb defaults to 0 for numbers; let's change that...
    mvarValue = 1
End Sub
```


Usage (in this case, from a form):

```vb
Public x As TinyInt

Private Sub Form_Click()
    '0-11, to give chance of errors; also not integers, because VB massages data to fit, if possible.
    x = Rnd * 12
    Me.Print x
End Sub

Private Sub Form_Load()
    Randomize Timer
    Set x = New TinyInt '"Set = New" REQUIRED
End Sub
```



## Visual Basic .NET

See [[#C#|C#]] for why this task ''technically'' impossible in VB.NET.

Through operator overloading, it is possible to declare types in VB.NET with the full complement of operators available on the primitive types. The following structure attempts to mimic the behavior of the <code>Integer</code> type in VB.NET on .NET Core as much as possible, including interfaces and static members, delegating as much implementation as possible to the <code>Integer</code> type itself.

Note that some operators return a <code>Double</code> instead of a new <code>LimitedInt</code>. This was intentional in order to match the behavior of the corresponding <code>Integer</code> operators in VB.NET. Also note that some members are commented out. This is because the newest supported release of VB.NET (as of 2019-09-14) does not support the .NET Core <code>Span(Of T)</code> and <code>ReadOnlySpan(Of T)</code> types.


```vbnet
Structure LimitedInt
    Implements IComparable, IComparable(Of LimitedInt), IConvertible, IEquatable(Of LimitedInt), IFormattable

    Private Const MIN_VALUE = 1
    Private Const MAX_VALUE = 10

    Shared ReadOnly MinValue As New LimitedInt(MIN_VALUE)
    Shared ReadOnly MaxValue As New LimitedInt(MAX_VALUE)

    Private Shared Function IsValidValue(value As Integer) As Boolean
        Return value >= MIN_VALUE AndAlso value <= MAX_VALUE
    End Function

    Private ReadOnly _value As Integer
    ReadOnly Property Value As Integer
        Get
            ' Treat the default, 0, as being the minimum value.
            Return If(Me._value = 0, MIN_VALUE, Me._value)
        End Get
    End Property

    Sub New(value As Integer)
        If Not IsValidValue(value) Then Throw New ArgumentOutOfRangeException(NameOf(value), value, $"Value must be between {MIN_VALUE} and {MAX_VALUE}.")
        Me._value = value
    End Sub

#Region "IComparable"
    Function CompareTo(obj As Object) As Integer Implements IComparable.CompareTo
        If TypeOf obj IsNot LimitedInt Then Throw New ArgumentException("Object must be of type " + NameOf(LimitedInt), NameOf(obj))
        Return Me.CompareTo(DirectCast(obj, LimitedInt))
    End Function
#End Region

#Region "IComparable(Of LimitedInt)"
    Function CompareTo(other As LimitedInt) As Integer Implements IComparable(Of LimitedInt).CompareTo
        Return Me.Value.CompareTo(other.Value)
    End Function
#End Region

#Region "IConvertible"
    Function GetTypeCode() As TypeCode Implements IConvertible.GetTypeCode
        Return Me.Value.GetTypeCode()
    End Function

    Private Function ToBoolean(provider As IFormatProvider) As Boolean Implements IConvertible.ToBoolean
        Return DirectCast(Me.Value, IConvertible).ToBoolean(provider)
    End Function

    Private Function ToByte(provider As IFormatProvider) As Byte Implements IConvertible.ToByte
        Return DirectCast(Me.Value, IConvertible).ToByte(provider)
    End Function

    Private Function ToChar(provider As IFormatProvider) As Char Implements IConvertible.ToChar
        Return DirectCast(Me.Value, IConvertible).ToChar(provider)
    End Function

    Private Function ToDateTime(provider As IFormatProvider) As Date Implements IConvertible.ToDateTime
        Return DirectCast(Me.Value, IConvertible).ToDateTime(provider)
    End Function

    Private Function ToDecimal(provider As IFormatProvider) As Decimal Implements IConvertible.ToDecimal
        Return DirectCast(Me.Value, IConvertible).ToDecimal(provider)
    End Function

    Private Function ToDouble(provider As IFormatProvider) As Double Implements IConvertible.ToDouble
        Return DirectCast(Me.Value, IConvertible).ToDouble(provider)
    End Function

    Private Function ToInt16(provider As IFormatProvider) As Short Implements IConvertible.ToInt16
        Return DirectCast(Me.Value, IConvertible).ToInt16(provider)
    End Function

    Private Function ToInt32(provider As IFormatProvider) As Integer Implements IConvertible.ToInt32
        Return DirectCast(Me.Value, IConvertible).ToInt32(provider)
    End Function

    Private Function ToInt64(provider As IFormatProvider) As Long Implements IConvertible.ToInt64
        Return DirectCast(Me.Value, IConvertible).ToInt64(provider)
    End Function

    Private Function ToSByte(provider As IFormatProvider) As SByte Implements IConvertible.ToSByte
        Return DirectCast(Me.Value, IConvertible).ToSByte(provider)
    End Function

    Private Function ToSingle(provider As IFormatProvider) As Single Implements IConvertible.ToSingle
        Return DirectCast(Me.Value, IConvertible).ToSingle(provider)
    End Function

    Private Overloads Function ToString(provider As IFormatProvider) As String Implements IConvertible.ToString
        Return Me.Value.ToString(provider)
    End Function

    Private Function ToType(conversionType As Type, provider As IFormatProvider) As Object Implements IConvertible.ToType
        Return DirectCast(Me.Value, IConvertible).ToType(conversionType, provider)
    End Function

    Private Function ToUInt16(provider As IFormatProvider) As UShort Implements IConvertible.ToUInt16
        Return DirectCast(Me.Value, IConvertible).ToUInt16(provider)
    End Function

    Private Function ToUInt32(provider As IFormatProvider) As UInteger Implements IConvertible.ToUInt32
        Return DirectCast(Me.Value, IConvertible).ToUInt32(provider)
    End Function

    Private Function ToUInt64(provider As IFormatProvider) As ULong Implements IConvertible.ToUInt64
        Return DirectCast(Me.Value, IConvertible).ToUInt64(provider)
    End Function
#End Region

#Region "IEquatable(Of LimitedInt)"
    Overloads Function Equals(other As LimitedInt) As Boolean Implements IEquatable(Of LimitedInt).Equals
        Return Me = other
    End Function
#End Region

#Region "IFormattable"
    Private Overloads Function ToString(format As String, formatProvider As IFormatProvider) As String Implements IFormattable.ToString
        Return Me.Value.ToString(format, formatProvider)
    End Function
#End Region

#Region "Operators"
    Shared Operator =(left As LimitedInt, right As LimitedInt) As Boolean
        Return left.Value = right.Value
    End Operator

    Shared Operator <>(left As LimitedInt, right As LimitedInt) As Boolean
        Return left.Value <> right.Value
    End Operator

    Shared Operator <(left As LimitedInt, right As LimitedInt) As Boolean
        Return left.Value < right.Value
    End Operator

    Shared Operator >(left As LimitedInt, right As LimitedInt) As Boolean
        Return left.Value > right.Value
    End Operator

    Shared Operator <=(left As LimitedInt, right As LimitedInt) As Boolean
        Return left.Value <= right.Value
    End Operator

    Shared Operator >=(left As LimitedInt, right As LimitedInt) As Boolean
        Return left.Value >= right.Value
    End Operator

    Shared Operator +(left As LimitedInt) As LimitedInt
        Return CType(+left.Value, LimitedInt)
    End Operator

    Shared Operator -(left As LimitedInt) As LimitedInt
        Return CType(-left.Value, LimitedInt)
    End Operator

    Shared Operator +(left As LimitedInt, right As LimitedInt) As LimitedInt
        Return CType(left.Value + right.Value, LimitedInt)
    End Operator

    Shared Operator -(left As LimitedInt, right As LimitedInt) As LimitedInt
        Return CType(left.Value - right.Value, LimitedInt)
    End Operator

    Shared Operator *(left As LimitedInt, right As LimitedInt) As LimitedInt
        Return CType(left.Value * right.Value, LimitedInt)
    End Operator

    Shared Operator /(left As LimitedInt, right As LimitedInt) As Double
        Return left.Value / right.Value
    End Operator

    Shared Operator \(left As LimitedInt, right As LimitedInt) As LimitedInt
        Return CType(left.Value \ right.Value, LimitedInt)
    End Operator

    Shared Operator ^(left As LimitedInt, right As LimitedInt) As Double
        Return left.Value ^ right.Value
    End Operator

    Shared Operator Mod(left As LimitedInt, right As LimitedInt) As LimitedInt
        Return CType(left.Value Mod right.Value, LimitedInt)
    End Operator

    Shared Operator And(left As LimitedInt, right As LimitedInt) As LimitedInt
        Return CType(left.Value And right.Value, LimitedInt)
    End Operator

    Shared Operator Or(left As LimitedInt, right As LimitedInt) As LimitedInt
        Return CType(left.Value Or right.Value, LimitedInt)
    End Operator

    Shared Operator Xor(left As LimitedInt, right As LimitedInt) As LimitedInt
        Return CType(left.Value Xor right.Value, LimitedInt)
    End Operator

    Shared Operator Not(left As LimitedInt) As LimitedInt
        Return CType(Not left.Value, LimitedInt)
    End Operator

    Shared Operator >>(left As LimitedInt, right As Integer) As LimitedInt
        Return CType(left.Value >> right, LimitedInt)
    End Operator

    Shared Operator <<(left As LimitedInt, right As Integer) As LimitedInt
        Return CType(left.Value << right, LimitedInt)
    End Operator

    Shared Widening Operator CType(value As LimitedInt) As Integer
        Return value.Value
    End Operator

    Shared Narrowing Operator CType(value As Integer) As LimitedInt
        If Not IsValidValue(value) Then Throw New OverflowException()
        Return New LimitedInt(value)
    End Operator
#End Region

    'Function TryFormat(destination As Span(Of Char), ByRef charsWritten As Integer, Optional format As ReadOnlySpan(Of Char) = Nothing, Optional provider As IFormatProvider = Nothing) As Boolean
    '    Return Me.Value.TryFormat(destination, charsWritten, format, provider)
    'End Function

    Overrides Function GetHashCode() As Integer
        Return Me.Value.GetHashCode
    End Function

    Overrides Function Equals(obj As Object) As Boolean
        Return TypeOf obj Is LimitedInt AndAlso Me.Equals(DirectCast(obj, LimitedInt))
    End Function

    Overrides Function ToString() As String
        Return Me.Value.ToString()
    End Function

#Region "Shared Methods"
    'Shared Function TryParse(s As ReadOnlySpan(Of Char), ByRef result As Integer) As Boolean
    '    Return Integer.TryParse(s, result)
    'End Function

    'Shared Function TryParse(s As ReadOnlySpan(Of Char), style As Globalization.NumberStyles, provider As IFormatProvider, ByRef result As Integer) As Boolean
    '    Return Integer.TryParse(s, style, provider, result)
    'End Function

    Shared Function Parse(s As String, provider As IFormatProvider) As Integer
        Return Integer.Parse(s, provider)
    End Function

    Shared Function Parse(s As String, style As Globalization.NumberStyles, provider As IFormatProvider) As Integer
        Return Integer.Parse(s, style, provider)
    End Function

    Shared Function TryParse(s As String, style As Globalization.NumberStyles, provider As IFormatProvider, ByRef result As Integer) As Boolean
        Return Integer.TryParse(s, style, provider, result)
    End Function

    Shared Function Parse(s As String) As Integer
        Return Integer.Parse(s)
    End Function
    Shared Function Parse(s As String, style As Globalization.NumberStyles) As Integer
        Return Integer.Parse(s, style)
    End Function

    'Shared Function Parse(s As ReadOnlySpan(Of Char), Optional style As Globalization.NumberStyles = Globalization.NumberStyles.Integer, Optional provider As IFormatProvider = Nothing) As Integer
    '    Return Integer.Parse(s, style, provider)
    'End Function

    Shared Function TryParse(s As String, ByRef result As Integer) As Boolean
        Return Integer.TryParse(s, result)
    End Function
#End Region
End Structure
```



## Visual FoxPro

Visual FoxPro can't define primitives but they can be emulated with custom classes.

```vfp
LOCAL o As BoundedInt
o = NEWOBJECT("BoundedInt")
DO WHILE NOT o.lHasError
    o.nValue = o.nValue + 2 && will get as far as 9.
    ? o.nValue
ENDDO

DEFINE CLASS BoundedInt As Custom
nValue = 1	&& default value
lHasError = .F.

PROCEDURE nValue_Assign(tnValue)
*!* This method will check the parameter and if
*!* it is out of bounds, the value will remain unchanged
*!* and an error generated.
tnValue = CAST(tnValue As I)
IF BETWEEN(tnValue, 1, 10)
    THIS.nValue = tnValue
ELSE
    ERROR "Value must be between 1 and 10."
ENDIF
ENDPROC

PROCEDURE Error(nError, cMethod, nLine)
IF nError = 1098
    MESSAGEBOX(MESSAGE(), 0, "Error")
ELSE
    DODEFAULT()
ENDIF
THIS.lHasError = .T.
ENDDEFINE
```

