+++
title = "Implicit type conversion"
description = ""
date = 2019-05-29T09:20:41Z
aliases = []
[extra]
id = 13350
[taxonomies]
categories = []
tags = []
+++

{{draft task|Basic language learning}}
Some programming languages have [[wp:Type conversion#Implicit type conversion|implicit type conversion]].  Type conversion is  also known as ''coercion''.

For example:
```algol68>COMPL z := 1;</lang
Here the assignment "''':='''" implicitly converts the '''integer''' 1, to the '''complex''' number <math>1+0i</math> in the programming language [[ALGOL 68]].

The alternative would be to ''explicitly'' convert a value from one type to another, using a ''function'' or some other mechanism (e.g. an explicit cast).


;Task:
Demonstrate the various type conversions in each language, and give an example of a ''implicit type conversion'' path from the smallest possible variable size to the largest possible variable size.  (Where the size of the underlying variable's data strictly increases).

In strong typed languages ''some'' types are actually mutually incompatible. In this case the language may have disjoint type conversion paths, or even branching type conversion paths.  (Where it occurs in a specific language, it is demonstrated in the code samples below.)

Languages that don't support ''any'' ''implicit type conversion'' are detailed in the [[:Category:Maintenance/OmitCategoriesCreated|/Omit]] categories at the bottom of this page.

Indicate if the language supports ''user defined'' type conversion definitions.

And give an example of such a definition.   (E.g. define an ''implicit type conversion'' from '''real''' to '''complex''' numbers, or from '''char''' to an array of '''char''' of length 1.)





## ALGOL 68

{{works with|ALGOL 68|Revision 1}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.6 algol68g-2.6].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
'''File: implicit_type_conversion.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

main:(
# a representative sample of builtin types #
    BOOL b; INT bool width = 1;
    BITS bits; LONG BITS lbits;
    BYTES bytes; LONG BYTES lbytes; # lbytes := bytes is not illegal #
    [bits width]BOOL array bool;
    [long bits width]BOOL long array bool;
    CHAR c; INT char width = 1;
    STRING array char;

    SHORT SHORT INT ssi; SHORT INT si; INT i; LONG INT li; LONG LONG INT lli;
    SHORT SHORT REAL ssr; SHORT REAL sr; REAL r; LONG REAL lr; LONG LONG REAL llr;
    SHORT SHORT COMPL ssz; SHORT COMPL sz; COMPL z; LONG COMPL lz; LONG LONG COMPL llz;
    INT long long compl width = 2 * long long real width;

    STRUCT (BOOL b, CHAR c, INT i, REAL r, COMPL z)cbcirz;  # NO implicit casting #
    REF []INT rai;

    FORMAT long long real fmt = $g(-0,long long real width-2)$;
    FORMAT long long compl fmt = $f(long long real fmt)"+"f(long long real fmt)"i"$;
# type conversion stating points #
    b := TRUE;
    c := "1";
    ssi := SHORT SHORT 1234; i := 1234;
# a representative sample of implied casts for subtypes of personality INT/REAL/COMPL #
     si:=ssi;
      i:=ssi;  i:=si;
     li:=ssi; li:=si;  li:=i;
    lli:=ssi;lli:=si; lli:=i;  lli:=li;
    ssr:=ssi;
     sr:=ssr; sr:=ssi; sr:=si;
      r:=ssr;  r:=sr;   r:=ssi;  r:=si;   r:=i;
     lr:=ssr; lr:=sr;  lr:=r;   lr:=ssi; lr:=si;  lr:=i;   lr:=li;
    llr:=ssr;llr:=sr; llr:=r;  llr:=lr; llr:=ssi;llr:=si; llr:=i;  llr:=li; llr:=i;
    ssz:=ssr;ssz:=ssi;
     sz:=ssz; sz:=ssr; sz:=sr;  sz:=ssi; sz:=si;
      z:=ssz;  z:=sz;   z:=ssr;  z:=sr;   z:=r;    z:=ssi;  z:=si;   z:=i;
     lz:=ssz; lz:=sz;  lz:=z;   lz:=ssr; lz:=sr;  lz:=r;   lz:=lr;  lz:=ssi; lz:=si;  lz:=i;   lz:=li;
    llz:=ssz;llz:=sz; llz:=z;  llz:=lz; llz:=ssr;llz:=sr; llz:=r;  llz:=lr; llz:=r;  llz:=ssi;llz:=si; llz:=i;  llz:=li; llz:=lli;

# conversion branch SHORT SHORT INT => LONG LONG COMPL #
# a summary result, using the longest sizeof increasing casting path #
    printf((long long compl fmt,llz:=(llr:=(lr:=(i:=(si:=ssi)))),$
            $l"  was increasingly cast"$,
            $" from "g(-0)$, long long compl width, long long real width, long real width,
                            int width, short int width, short short int width, $" digits"$,
            $" from "g(-0)l$,ssi ));

# conversion branch BITS => []BOOL #
    bits := 16rf0f0ff00;
    lbits := bits;
    printf(($g$,"[]BOOL := LONG BITS := BITS - implicit widening: ",array bool := bits, $l$));

# conversion branch BYTES => []CHAR #
    bytes := bytes pack("0123456789ABCDEF0123456789abcdef");
    long array bool := LONG 2r111;
    printf(($g$,"[]CHAR := LONG BYTES := BYTES - implicit widening: ",array char := bytes, $l$));

# deproceduring conversion branch  PROC PROC PROC INT => INT #
    PROC pi = INT: i;
    PROC ppi = PROC INT: pi;
    PROC pppi = PROC PROC INT: ppi;
    printf(($g$,"PROC PROC PROC INT => INT - implicit deprocceduring^3: ",pppi, $l$));

# dereferencing conversion branch REF REF REF INT => INT #
    REF INT ri := i;
    REF REF INT rri := ri;
    REF REF REF INT rrri := rri;
    printf(($g$,"REF REF REF INT => INT - implicit dereferencing^3: ",rrri, $l$));

# rowing conversion branch INT => []INT => [,]INT => [,,]INT #
# a representative sample of implied casts, type pointer #
    rai := ai; # starts at the first element of ai #
    rai := i;  # an array of length 1 #

    FLEX[0]INT ai := i;
    FLEX[0,0]INT aai := ai;
    FLEX[0,0,0]INT aaai := aai;
    printf(($g$,"INT => []INT => [,]INT => [,,]INT - implicit rowing^3: ",aaai, $l$));

# uniting conversion branch UNION(VOID, INT) => UNION(VOID,INT,REAL) => UNION(VOID,INT,REAL,COMPL) #
    UNION(VOID,INT) ui := i;
    UNION(VOID,INT,REAL) uui := ui;
    UNION(VOID,INT,REAL,COMPL) uuui := uui;
    printf(($g$,"INT => UNION(VOID, INT) => UNION(VOID,INT,REAL,COMPL) - implicit uniting^3: ",(uuui|(INT i):i), $l$));
  SKIP
)
```

{{out}}

```txt

1234.0000000000000000000000000000000000000000000000000000000000000+.0000000000000000000000000000000000000000000000000000000000000i
  was increasingly cast from 126 from 63 from 28 from 10 from 10 from 10 digits from 1234
[]BOOL := LONG BITS := BITS - implicit widening: TTTTFFFFTTTTFFFFTTTTTTTTFFFFFFFF
[]CHAR := LONG BYTES := BYTES - implicit widening: 0123456789ABCDEF0123456789abcdef
PROC PROC PROC INT => INT - implicit deprocceduring^3:       +1234
REF REF REF INT => INT - implicit dereferencing^3:       +1234
INT => []INT => [,]INT => [,,]INT - implicit rowing^3:       +1234
INT => UNION(VOID, INT) => UNION(VOID,INT,REAL,COMPL) - implicit uniting^3:       +1234

```


## AWK


```AWK

# syntax: GAWK -f IMPLICIT_TYPE_CONVERSION.AWK
BEGIN {
    n = 1     # number
    s = "1"   # string
    a = n ""  # number coerced to string
    b = s + 0 # string coerced to number
    print(n,s,a,b)
    print(("19" 91) + 4) # string and number concatenation
    c = "10e1"
    print(c,c+0)
    exit(0)
}

```

{{out}}

```txt

1 1 1 1
1995
10e1 100

```



## C


```c
#include <stdio.h>
main(){
/* a representative sample of builtin types */
    unsigned char uc; char c;
    enum{e1, e2, e3}e123;
    short si; int i; long li;
    unsigned short su; unsigned u; unsigned long lu;
    float sf; float f; double lf; long double llf;
    union {char c; unsigned u; int i; float f; }ucuif;  /* manual casting only */
    struct {char c; unsigned u; int i; float f; }scuif; /* manual casting only */
    int ai[99];
    int (*rai)[99];
    int *ri;
    uc = '1';
/* a representitive sample of implied casts for subtypes of personality int/float */
    c=uc;
    si=uc; si=c;
    su=uc; su=c; su=si;
    i=uc;  i=c;  i=si;  i=su;
    e123=i; i=e123;
    u=uc;  u=c;  u=si;  u=su;  u=i;
    li=uc; li=c; li=si; li=su; li=i; li=u;
    lu=uc; lu=c; lu=si; lu=su; lu=i; lu=u; lu=li;
    sf=uc; sf=c; sf=si; sf=su; sf=i; sf=u; sf=li; sf=lu;
    f=uc;  f=c;  f=si;  f=su;  f=i;  f=u;  f=li;  f=lu;  f=sf;
    lf=uc; lf=c; lf=si; lf=su; lf=i; lf=u; lf=li; lf=lu; lf=sf; lf=f;
    llf=uc;llf=c;llf=si;llf=su;llf=i;llf=u;llf=li;llf=lu;llf=sf;llf=f;llf=lf;
/*  ucuif = i; no implied cast; try: iucuif.i = i */
/*  ai = i; no implied cast; try: rai = &i */
/* a representitive sample of implied casts, type pointer */
    rai = ai; /* starts at the first element of ai */
    ri = ai;  /* points to the first element of ai */
/* a summary result, using the longest sizeof increasing casting path */
    printf("%LF was increasingly cast from %d from %d from %d from %d from %d bytes from '%c'\n",
           llf=(lf=(i=(si=c))), sizeof llf, sizeof lf, sizeof i, sizeof si,sizeof c, c);
}
```

{{out}}

```txt

49.000000 was increasingly cast from 12 from 8 from 4 from 2 from 1 bytes from '1'

```



## C sharp

C# has built-in implicit conversions for primitive numerical types. Any value can be implicitly converted to a value of a larger type. Many non-primitive types also have implicit conversion operators defined, for instance the '''BigInteger''' and '''Complex''' types.

```csharp
byte aByte = 2;
short aShort = aByte;
int anInt = aShort;
long aLong = anInt;

float aFloat = 1.2f;
double aDouble = aFloat;

BigInteger b = 5;
Complex c = 2.5; // 2.5 + 0i

```

Users are able to define implicit (and also explicit) conversion operators. To define a conversion from A to B, the operator must be defined inside either type A or type B. Therefore, we cannot define a conversion from '''char''' to an array of '''char'''.

```csharp
public class Person
{
    //Define an implicit conversion from string to Person
    public static implicit operator Person(string name) => new Person { Name = name };

    public string Name { get; set; }
    public override string ToString() => $"Name={Name}";

    public static void Main() {
        Person p = "Mike";
        Console.WriteLine(p);
    }
}
```

{{out}}

```txt

Name=Mike

```



## D

This covers a large sample of built-in types and few library-defined types.

```d
void main() {
    import std.stdio, std.typetuple, std.variant, std.complex;

    enum IntEnum : int { A, B }
    union IntFloatUnion { int x; float y; }
    struct IntStruct { int x; }
    class ClassRef {}
    class DerivedClassRef : ClassRef {}
    alias IntDouble = Algebraic!(int, double);
    alias ComplexDouble = Complex!double;

    // On a 64 bit system size_t and ptrdiff_t are twice larger,
    // so this changes few implicit assignments results.
    writeln("On a ", size_t.sizeof * 8, " bit system:\n");

    // Represented as strings so size_t prints as "size_t"
    // instead of uint/ulong.
    alias types = TypeTuple!(
        `IntEnum`, `IntFloatUnion`, `IntStruct`,
        `bool`,
        `char`, `wchar`, `dchar`,
        `ubyte`, `ushort`, `uint`, `ulong`, /*`ucent`,*/
        `byte`, `short`, `int`, `long`, /*`cent`,*/
        `size_t`, `hash_t`, `ptrdiff_t`,
        `float`, `double`, `real`,
        `int[2]`, `int[]`, `int[int]`,
        `int*`, `void*`, `ClassRef`, `DerivedClassRef`,
        `void function()`, `void delegate()`,
        `IntDouble`, `ComplexDouble`,
    );

    foreach (T1; types) {
        mixin(T1 ~ " x;");
        write("A ", T1, " can be assigned with: ");
        foreach (T2; types) {
            mixin(T2 ~ " y;");
            static if (__traits(compiles, x = y))
                write(T2, " ");
        }
        writeln;
    }
    writeln;

    // Represented as strings so 1.0 prints as "1.0" instead of "1."
    alias values = TypeTuple!(
        `true`, `'x'`, `"hello"`, `"hello"w`, `"hello"d`,
        `0`, `255`, `1L`, `2.0f`, `3.0`, `4.0L`, `10_000_000_000L`,
        `[1, 2]`, `[3: 4]`,
        `void*`, `null`,
    );

    foreach (T; types) {
        mixin(T ~ " x;");
        write("A ", T, " can be assigned with value literal(s): ");
        foreach (y; values) {
            static if (__traits(compiles, x = mixin(y)))
                write(y, " ");
        }
        writeln;
    }

    // Few extras:
    int[] a1;
    const int[] a2 = a1;                 // OK.
    // immutable int[] a3 = a1;          // Not allowed.
    // immutable int[] a4 = a2;          // Not allowed.
    int[int] aa1;
    const int[int] aa2 = aa1;            // OK.
    //immutable int[int] aa3 = aa1;      // Not allowed.
    //immutable int[int] aa4 = aa2;      // Not allowed.

    void foo() {}
    void delegate() f1 = &foo;           // OK.
    void bar() pure nothrow @safe {}
    void delegate() f2 = &bar;           // OK.
    //void delegate() pure f3 = &foo;    // Not allowed.
    //void delegate() nothrow f4 = &foo; // Not allowed.
    //void delegate() @safe f5 = &foo;   // Not allowed.

    static void spam() {}
    void function() f6 = &spam;          // OK.
    //void function() f7 = &foo;         // Not allowed.
}
```

{{out}}

```txt
On a 32 bit system:

A IntEnum can be assigned with: IntEnum
A IntFloatUnion can be assigned with: IntFloatUnion
A IntStruct can be assigned with: IntStruct
A bool can be assigned with: bool
A char can be assigned with: bool char ubyte byte
A wchar can be assigned with: bool char wchar ubyte ushort byte short
A dchar can be assigned with: IntEnum bool char wchar dchar ubyte ushort uint byte short int size_t hash_t ptrdiff_t
A ubyte can be assigned with: bool char ubyte byte
A ushort can be assigned with: bool char wchar ubyte ushort byte short
A uint can be assigned with: IntEnum bool char wchar dchar ubyte ushort uint byte short int size_t hash_t ptrdiff_t
A ulong can be assigned with: IntEnum bool char wchar dchar ubyte ushort uint ulong byte short int long size_t hash_t ptrdiff_t
A byte can be assigned with: bool char ubyte byte
A short can be assigned with: bool char wchar ubyte ushort byte short
A int can be assigned with: IntEnum bool char wchar dchar ubyte ushort uint byte short int size_t hash_t ptrdiff_t
A long can be assigned with: IntEnum bool char wchar dchar ubyte ushort uint ulong byte short int long size_t hash_t ptrdiff_t
A size_t can be assigned with: IntEnum bool char wchar dchar ubyte ushort uint byte short int size_t hash_t ptrdiff_t
A hash_t can be assigned with: IntEnum bool char wchar dchar ubyte ushort uint byte short int size_t hash_t ptrdiff_t
A ptrdiff_t can be assigned with: IntEnum bool char wchar dchar ubyte ushort uint byte short int size_t hash_t ptrdiff_t
A float can be assigned with: IntEnum bool char wchar dchar ubyte ushort uint ulong byte short int long size_t hash_t ptrdiff_t float double real
A double can be assigned with: IntEnum bool char wchar dchar ubyte ushort uint ulong byte short int long size_t hash_t ptrdiff_t float double real
A real can be assigned with: IntEnum bool char wchar dchar ubyte ushort uint ulong byte short int long size_t hash_t ptrdiff_t float double real
A int[2] can be assigned with: IntEnum bool char wchar dchar ubyte ushort uint byte short int size_t hash_t ptrdiff_t int[2] int[]
A int[] can be assigned with: int[2] int[]
A int[int] can be assigned with: int[int]
A int* can be assigned with: int*
A void* can be assigned with: int* void* void function()
A ClassRef can be assigned with: ClassRef DerivedClassRef
A DerivedClassRef can be assigned with: DerivedClassRef
A void function() can be assigned with: void function()
A void delegate() can be assigned with: void delegate()
A IntDouble can be assigned with: int ptrdiff_t double IntDouble
A ComplexDouble can be assigned with: IntEnum bool char wchar dchar ubyte ushort uint ulong byte short int long size_t hash_t ptrdiff_t float double real ComplexDouble

A IntEnum can be assigned with value literal(s):
A IntFloatUnion can be assigned with value literal(s):
A IntStruct can be assigned with value literal(s):
A bool can be assigned with value literal(s): true 0 1L
A char can be assigned with value literal(s): true 'x' 0 255 1L
A wchar can be assigned with value literal(s): true 'x' 0 255 1L
A dchar can be assigned with value literal(s): true 'x' 0 255 1L
A ubyte can be assigned with value literal(s): true 'x' 0 255 1L
A ushort can be assigned with value literal(s): true 'x' 0 255 1L
A uint can be assigned with value literal(s): true 'x' 0 255 1L
A ulong can be assigned with value literal(s): true 'x' 0 255 1L 10_000_000_000L
A byte can be assigned with value literal(s): true 'x' 0 1L
A short can be assigned with value literal(s): true 'x' 0 255 1L
A int can be assigned with value literal(s): true 'x' 0 255 1L
A long can be assigned with value literal(s): true 'x' 0 255 1L 10_000_000_000L
A size_t can be assigned with value literal(s): true 'x' 0 255 1L
A hash_t can be assigned with value literal(s): true 'x' 0 255 1L
A ptrdiff_t can be assigned with value literal(s): true 'x' 0 255 1L
A float can be assigned with value literal(s): true 'x' 0 255 1L 2.0f 3.0 4.0L 10_000_000_000L
A double can be assigned with value literal(s): true 'x' 0 255 1L 2.0f 3.0 4.0L 10_000_000_000L
A real can be assigned with value literal(s): true 'x' 0 255 1L 2.0f 3.0 4.0L 10_000_000_000L
A int[2] can be assigned with value literal(s): true 'x' 0 255 1L [1, 2] null
A int[] can be assigned with value literal(s): [1, 2] null
A int[int] can be assigned with value literal(s): [3: 4] null
A int* can be assigned with value literal(s): null
A void* can be assigned with value literal(s): null
A ClassRef can be assigned with value literal(s): null
A DerivedClassRef can be assigned with value literal(s): null
A void function() can be assigned with value literal(s): null
A void delegate() can be assigned with value literal(s): null
A IntDouble can be assigned with value literal(s): 0 255 3.0
A ComplexDouble can be assigned with value literal(s): true 'x' 0 255 1L 2.0f 3.0 4.0L 10_000_000_000L
```


=={{header|Déjà Vu}}==

The only implicit conversion currently permitted is boolean to number:


```dejavu><1:1
 #interactive session
<2:1> !. + 3 true #boolean true is equal to 1
4
<3:1> !. * 2 false #boolean false is equal to 0
0
```



## Go

Go is a very strongly typed language and, strictly speaking, doesn't permit any implicit conversions at all - even 'widening' numeric conversions or conversions between types with the same underlying type. Nor does it support user-defined conversions.

Consequently, you always need to use type conversions when dealing with expressions, assignments etc. involving mixed types otherwise the code won't compile.

However, in practice, this is considerably less onerous than it sounds because of the use of 'untyped' constants which (whilst having a default type) mould themselves to whatever type is needed (consistent with their representation) and so give the same 'feel' as an implicit conversion.

There are untyped boolean, rune, integer, floating-point, complex and string constants. The most notable examples of untyped constants are literals of the aforementioned types, for example (and in the same order) :

true, 'a', 1, 1.0, 1 + 1i, "a"

In a context in which a typed value is needed and in the absence of any other type information, these would assume their default types which in the same order are:

bool, rune, int, float64, complex128, string

Otherwise, they would adopt whatever type is needed to enable a representable variable declaration, assignment or expression to compile.

Some simple examples may help to make this clearer.

```go
package main

import "fmt"

func main() {
    // 1 and 2 are untyped integer constants with a default type of int.

    // Here a local variable 'i' is created and explicitly given the type int32.
    // 1 implicitly assumes this type which is consistent with its representation.
    var i int32 = 1
    fmt.Printf("i : type %-7T value %d\n", i, i)

    // Here a local variable 'j' is created and implicitly given the type int.
    // In the absence of any type information, 2 adopts its default type of int
    // and the type of 'j' is therefore inferred to be 'int' also.
    j := 2
    fmt.Printf("j : type %-7T value %d\n", j, j)

    // Here 'k' is declared to be an int64 variable and it can only therefore be initialized
    // with an expression of the same type. As 'i' and 'j' have respective types of int32
    // and int, they need to be explicitly converted to int64 so that, first the addition and
    // second the assignment compile.
    var k int64 = int64(i) + int64(j)
    fmt.Printf("k : type %-7T value %d\n", k, k)

    // 4.0 is an untyped floating point constant with a default type of float64. However,
    // (unusually in my experience of other strongly typed languages) it can also represent
    // an integer type because it happens to be a whole number.

    // Here 'l' is declared to be an int8 variable and it can only therefore be initialized
    // with an expression of the same type. As noted above 4.0 can represent this type and
    // so the following compiles fine.
    var l int8 = 4.0
    fmt.Printf("l : type %-7T value %d\n", l, l)

    // Here 'm' is created and implicitly given the type float64 because the expression on
    // the RHS is of that type. Note that 'l' needs to be converted to this type before it
    // can be added to the untyped floating point constants 0.3 and 0.7.
    m := 0.3 + float64(l) + 0.7
    fmt.Printf("m : type %-7T value %g\n", m, m)
}
```


{{out}}

```txt

i : type int32   value 1
j : type int     value 2
k : type int64   value 3
l : type int8    value 4
m : type float64 value 5

```



## Idris


Idris provides the "[http://docs.idris-lang.org/en/latest/tutorial/miscellany.html#implicit-conversions implicit]" keyword which enables the implicit conversion from one type to another.


```idris
implicit
boolToInt : Bool -> Int
boolToInt True  = 1
boolToInt False = 0

one : Int
one = 1

two : Int
two = one + True
```



## J


Overview: Types are viewed as a necessary evil - where possible mathematical identities are given precedence over the arbitrariness of machine representation.

J has 4 "static types" (noun, verb, adverb, conjunction).
There are almost no implicit conversions between these types
(but a noun can be promoted to a constant verb in certain static contexts or a noun representation of a verb can be placed in a context which uses that definition to perform the corresponding operation).

Translating from "traditional english" to "contemporary computer science" nomenclature: Nouns are "data", verbs are "functions", adverbs and conjunctions are "metafunctions".

Nouns break down into four disjoint collections of subtypes: boxed, literal, numeric and symbolic (which is rarely used).
Most of J's implicit conversions happen within the first three subtypes. (And J supports some "extra conversions" between these types in some cases where no values are involved.
For example a list which contains no characters (literals) may be used as a list which contains no numbers (numerics)).

There is one type of box, two types of literals (8 bit wide and 16 bit wide), and a variety of types of numerics.
Sparse arrays are also (partially) supported and treated internally as distinct datatypes, implemented under the covers as a sequence of arrays (one to indicate which indices have values, and another to hold the corresponding values, and also a default value to fill in the rest).

The primary implicit type conversion in J applies to numeric values.
In particular, J tries to present numeric values as "[http://www.jsoftware.com/pipermail/beta/2006-April/000749.html analytic]"; that is, numeric values which are "the same" should presented to the user (J programmer) as "the same" in as many different contexts as is feasible, irrespective of their representation in the the computer's model or layout in memory.  So, for example, on a 32-bit machine, `(2^31)-1` is the largest value a signed integer, which is stored in 4 bytes, can represent; in J, incrementing this value (adding 1) causes the underlying representation to switch to IEEE double-precision floating point number.
In other words `1+(2^31)-1` doesn't overflow: it represents `2^31` exactly (using double the memory: 8 bytes). Similar comments apply to the two varieties of character values (ASCII and Unicode), though the implications are more straightforward and less interesting.

Having said that all that, because of the potential performance penalties involved, J does not stretch this abstraction too far. For example, numbers will never be automatically promoted to the (available, but expensive) arbitrary precision format, nor will values be automatically "demoted" (automatic demotion, paired with automatic promotion, has the potential to cause cycles of expansion and contraction during calculation of intermediate values; this, combined with J's homogeneous array-oriented nature, which requires an entire array to be promoted/demoted along with any one of its values, means including automatic demotion would probably hurt programs' performance more often than it benefited them.)

[And, though it is unrelated to type conversion, note that hiding the details of representation also requires J to treat comparison in a tolerant fashion: that is, floating point values are considered identical if they are equal up to some epsilon (2^-44 by default) times the value with the larger magnitude; and character values are identical if their [[wp:Code_point|code points]] are equal). Intolerant or exact comparison is available, though, should that be needed.]

The rich j datatypes:

```J

   datatype    NB. data type identification verb
3 : 0
n=. 1 2 4 8 16 32 64 128 1024 2048 4096 8192 16384 32768 65536 131072
t=. '/boolean/literal/integer/floating/complex/boxed/extended/rational'
t=. t,'/sparse boolean/sparse literal/sparse integer/sparse floating'
t=. t,'/sparse complex/sparse boxed/symbol/unicode'
(n i. 3!:0 y) pick <;._1 t
)


   NB. examples of the data types
   [A =: 0 1 ; 0 1 2 ; (!24x) ; 1r2 ; 1.2 ; 1j2 ; (<'boxed') ; (s:'`symbol')  ; 'literal' ; (u: 16b263a)
┌───┬─────┬────────────────────────┬───┬───┬───┬───────┬───────┬───────┬───┐
│0 1│0 1 2│620448401733239439360000│1r2│1.2│1j2│┌─────┐│`symbol│literal│☺│
│   │     │                        │   │   │   ││boxed││       │       │   │
│   │     │                        │   │   │   │└─────┘│       │       │   │
└───┴─────┴────────────────────────┴───┴───┴───┴───────┴───────┴───────┴───┘


   datatype&.>A
┌───────┬───────┬────────┬────────┬────────┬───────┬─────┬──────┬───────┬───────┐
│boolean│integer│extended│rational│floating│complex│boxed│symbol│literal│unicode│
└───────┴───────┴────────┴────────┴────────┴───────┴─────┴──────┴───────┴───────┘


   [I =: =i.4  NB. Boolean matrix
1 0 0 0
0 1 0 0
0 0 1 0
0 0 0 1

   datatype I
boolean


   $. I  NB. sparse matrix
0 0 │ 1
1 1 │ 1
2 2 │ 1
3 3 │ 1


   datatype $. I
sparse boolean

   (+ $.)I  NB. hook adds data to sparse version of data resulting in sparse
0 0 │ 2
1 1 │ 2
2 2 │ 2
3 3 │ 2

```


J has verbs causing explicit conversion.  Some appear in the above examples.
J's lexical notation provides for us to directly specify the datatype as demonstrated in the other samples.  The Extended and Rational Arithmetic section of the J dictionary (DOJ) explains the fundamental implicit conversions.
Before quoting this the section here, please note that arrays have the homogeneous data type of the highest atomic data type as shown in the 0 1 2 integer vector---implicit conversion without using the primitive verbs.

''Various primitive verbs produce (exact) rational results if the argument(s) are rational; non-rational verbs produce (inexact) floating point or complex results when applied to rationals, if the verb only has a limited number of rational arguments that produce rational results.
(For example, %:y is rational if the atoms of y are perfect squares; ^0r1 is floating point.)
The quotient of two extended integers is an extended integer (if evenly divisible) or rational (if not). Comparisons involving two rationals are exact.
Dyadic verbs (e.g. + - * % , = <) that require argument type conversions do so according to the following table:''

```txt

     |  B  I  X  Q  D  Z
  ---+------------------
  B  |  B  I  X  Q  D  Z     B - bit
  I  |  I  I  X  Q  D  Z     I - integer
  X  |  X  X  X  Q  D  Z     X - extended integer
  Q  |  Q  Q  Q  Q  D  Z     Q - rational
  D  |  D  D  D  D  D  Z     D - floating point
  Z  |  Z  Z  Z  Z  Z  Z     Z - complex

```



## Java

The Java Language Specification includes several varieties of implicit type conversion. This code illustrates: <ul><li>widening conversions of primitives</li><li>boxing and unboxing conversions</li><li>string conversions (with the + operator)</li></ul>

```java
public class ImplicitTypeConversion{
   public static void main(String...args){
      System.out.println( "Primitive conversions" );
      byte  by = -1;
      short sh = by;
      int   in = sh;
      long  lo = in;
      System.out.println( "byte value    -1         to 3 integral types:  " + lo );

      float  fl = 0.1f;
      double db = fl;
      System.out.println( "float value   0.1        to double:            " + db );

      int    in2 = -1;
      float  fl2 = in2;
      double db2 = fl2;
      System.out.println( "int value     -1         to float and double:  " + db2 );

      int    in3 = Integer.MAX_VALUE;
      float  fl3 = in3;
      double db3 = fl3;
      System.out.println( "int value     " + Integer.MAX_VALUE + " to float and double:  " + db3 );

      char   ch  = 'a';
      int    in4 = ch;
      double db4 = in4;
      System.out.println( "char value    '" + ch + "'        to int and double:    " + db4 );

      System.out.println();
      System.out.println( "Boxing and unboxing" );
      Integer in5 = -1;
      int     in6 = in5;
      System.out.println( "int  value    -1         to Integer and int:   " + in6 );

      Double db5 = 0.1;
      double db6 = db5;
      System.out.println( "double value  0.1        to Double and double: " + db6 );
   }
}

```

{{out}}

```txt

Primitive conversions
byte value    -1         to 3 integral types:  -1
float value   0.1        to double:            0.10000000149011612
int value     -1         to float and double:  -1.0
int value     2147483647 to float and double:  2.147483648E9
char value    'a'        to int and double:    97.0
Boxing and unboxing
int  value    -1         to Integer and int:   -1
double value  0.1        to Double and double: 0.1

```

Notice that in the second and fourth conversions the result is a slightly different value.

Not included are:
<ul>
<li>reference conversions (for example, you can treat a reference as if it were a reference to its superclass)</li>
<li>unchecked conversions (the sort of thing that the compiler warns about if you use non-generic collections)
<li>capture conversions (not sure I understand, but it seems to allow wildcard generics where explicit generics would otherise be required)
</ul>


## jq


jq variables are simply untyped references to JSON values, and so there are
no implicit conversions on assignment.

Some builtin operators are polymorphic, e.g. 1 (of type "number") and
null (of type "null") can be added (for any finite number, x, "x + null"
yields x); in effect, null is in such cases implicitly converted to 0.

Currently jq uses IEEE 754 64-bit numbers, which means that some
conversions between integers and floats take place implicitly, but
jq's only builtin numeric type is "number", so these are
behind-the-scenes type conversions.

However, if one were to define "is_integer" as follows:

```jq
def is_integer: type == "number" and . == floor;
```


then one would find:

```jq
(1/3) | is_integer # yields false

(1/3 + 1/3 + 1/3) | is_integer # yields true
```


For reference, jq's builtin types are "number", "boolean", "null", "object" and "array".



## Julia

In general, Julia will promote a smaller sized datatype to a larger one when needed for a calculation involving mixed data types. Julia also accepts type annotations on variable declarations as shown below, though such type declarations are usually only allowed for variables that are declared within a function.

```julia

    julia> function testme()
       ui8::UInt8 = 1
       ui16::UInt16 = ui8
       ui32::UInt32 = ui8
       ui64::UInt64 = ui8
       flo::Float64 = ui8
       return ui8, sizeof(ui8), ui16, sizeof(ui16), ui32, sizeof(ui32), ui64, sizeof(ui64), flo, sizeof(flo)
       end
    testme (generic function with 1 method)

    julia> testme()
    (0x01, 1, 0x0001, 2, 0x00000001, 4, 0x0000000000000001, 8, 1.0, 8)

```



## Kotlin

In general, the only implicit type conversions allowed in Kotlin are between objects of sub-types and variables of their ancestor types. Even then, unless 'boxing' of primitive types is required, the object is not actually changed - it is simply viewed in a different way.

If follows from this that there are no implicit conversions (even 'widening' conversions) between the numeric types because there is no inheritance relationship between them. However, there is one exception to this - integer literals, which would normally be regarded as of type Int (4 bytes) can be assigned to variables of the other integral types provided they are within the range of that type. This is allowed because it can be checked statically by the compiler.

```scala
// version 1.1.2

open class C(val x: Int)

class D(x: Int) : C(x)

fun main(args: Array<String>) {
    val c: C = D(42)        // OK because D is a sub-class of C
    println(c.x)
    val b: Byte = 100       // OK because 100 is within the range of the Byte type
    println(b)
    val s: Short = 32000    // OK because 32000 is within the range of the Short Type
    println(s)
    val l: Long = 1_000_000 // OK because any Int literal is within the range of the Long type
    println(l)
    val n : Int? = c.x      // OK because Int is a sub-class of its nullable type Int? (c.x is boxed on heap)
    println(n)
}
```


{{out}}

```txt

42
100
32000
1000000
42

```



## Lua

For the most part, Lua is strongly typed, but there are a few cases where it will coerce if the result would be of a predictable type.
Coercions are never performed during comparisons or while indexing an object.

```lua
-- During concatenation, numbers are always converted to strings. arithmetic operations will attempt to coerce strings to numbers, or throw an error if they can't
type(123 .. "123") --> string
type(123 + "123") --> number
type(123 + "foo") --> error thrown

-- Because Lua supports multiple returns, there is a concept of "no" value when a function does not return anything, or does not return enough. If Lua is expecting a value, it will coerce these "nothing" values into nil. The same applies for lists of values in general.
function noop () end
local a = noop()
print(a) --> nil
local x, y, z = noop()
print(x, y, z) --> nil nil nil

-- As in many languages, all types can be automatically coerced into their boolean value if required. Only nil and false will coerce to false
print(not not nil, not not false, not not 1, not not "foo", not not { }) --> false false true true true
```


The only two explicit conversion functions offered by Lua are <code>tonumber</code> and <code>tostring</code>.
Only the latter has a corresponding metamethod, so the former is usually only ever useful for converting strings, although in LuaJIT <code>tonumber</code> is used for converting numerical cdata into Lua numbers.

## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
      Long a=12.5
      \\ 12 is a double literal
      Print a=12    ' compare a long with a double
      Print a=12&   ' compare two long values
      def decimal b
      b=12.e12
      Print b=12000000000000    ' compare a decimal and a double
      Print b=12000000000000@   ' now we compare two decimals
      z=10#+20&
      Print type$(z)="Currency"
      \\ now z can't change type (except using a Swap)
      z=100@   ' 100 decimal
      Print type$(z)="Currency"
      z=12.1234567   ' round to fit to currency
      Print z=12.1235
      \\ swap just swap values
      swap z, b
      Print type$(z)="Decimal", type$(b)="Currency"
      \\ if a variable is a number then can be change to an object
      z=(1,2,3,4,5)
      Print type$(z)="mArray"
      Try {
            z=100
      }
      Print Error$  ' Missing Object

      Function AnyType$(x) {
            =type$(x)
      }
      Print AnyType$(z)="mArray"
      Print AnyType$(100@)="Decimal", AnyType$(100#)="Currency"
      \\ integer 16bit, long 32 bit
      Print AnyType$(100%)="Integer", AnyType$(100&)="Long"
      Print AnyType$(100~)="Single",  AnyType$(100)="Double"
      \\ double used as unsigned 32 bit long
      Print AnyType$(0x100)="Double",  AnyType$(0x100&)="Long", AnyType$(0x100%)="Integer"
      Print 0xFFFFFFFF=4294967295, 0xFFFFFFFF&=-1, 0xFFFF%=-1
      k=100@
      Print AnyType$(val(k->Long))="Long"
      \\ define type for argument
      Function OnlyLong$(x as Long) {
            =type$(x)
      }
      Print OnlyLong$(k)="Long"
      Try {
            s=stack:=1,2,3,4
            Print OnlyLong$(s)
      }
      Print Error$  'Wrong object type
      Print AnyType$(s)="mStiva"  ' name of object for stack
      Try {
            Print OnlyLong$(0xFFFFFFFF)
      }
      Print Error$  ' overflow long
}
Checkit

```



## Oforth


Oforth allows implicit conversions only on : ==, <=, +, -, * /, rem, pow

Classes have a priority. Most classes have 0 as priority.
Into basic classes :
   Integer priority is 2
   Float   priority is 40
   String  priority is 1024
   List    priority is 2048

A new class is created with 0 priority unless explicitly provided.

When, for instance, + is called, it checks priorities and convert the object with the smaller priority.

Conversion uses a convertor : a method with name "asClass" where Class is the name of the object wwith the higher priority.
Conversion is not the right word here, as all theses objects are immutables : new objects are created.

For instance, adding an Integer and a Float will convert the integer into a float using asFloat method.

Let's create a Complex class with 80 as priority (please note asComplex methods that will be used for conversions) :


```Oforth
100 Number Class newPriority: Complex(re, im)

Complex method: re  @re ;
Complex method: im  @im ;

Complex method: initialize  := im := re ;
Complex method: <<   '(' <<c @re << ',' <<c @im << ')' <<c ;

Integer method: asComplex  Complex new(self, 0) ;
Float   method: asComplex  Complex new(self, 0) ;

Complex new(0, 1) Constant new: I

Complex method: ==(c)  c re @re == c im @im == and ;
Complex method: norm   @re sq @im sq + sqrt ;
Complex method: conj   Complex new(@re, @im neg) ;
Complex method: +(c)   Complex new(c re @re +, c im @im +) ;
Complex method: -(c)   Complex new(c re @re -, c im @im -) ;

Complex method: *(c)  Complex new(c re @re * c im @im * -, c re @im * @re c im * + ) ;

Complex method: inv
| n |
   @re sq @im sq + asFloat ->n
   Complex new(@re n /, @im neg n / ) ;

Complex method: /(c)   c self inv * ;
```


Usage :


```Oforth
2 3.2 I * + println
Complex new(2, 3) 1.2 + println
Complex new(2, 3) 1.2 * println
2 Complex new(2, 3) / println
```


{{out}}

```txt

(2,3.2)
(3.2,3)
(2.4,3.6)
(0.307692307692308,-0.461538461538462)

```



## PARI/GP


PARI has access to all the implicit type conversions of [[#C|C]].
In addition, certain objects are automatically simplified when stored in history objects (in addition to explicit conversions of various types).
So a complex number with imaginary part an exact 0 is simplified to a t_REAL, t_INT, etc.

There are no user-defined types and hence no implicit conversion on them.


## Perl

Perl is the original DWIM language, implicit type conversion is the default mode of operation. Perl does not have static types; the concept of distinct integers/strings/floats is not present. Instead, operators defines how the operands will behave. An operator that requires a string coerces its operand into a string, an operator that requires an integer coerces its operand into an integer, and so forth.

```perl
print  1  + '2';  # 3
print '1' + '2';  # 3
print  1  .  1;   # 11

$a = 1;
$b = 2;
say "$a+$b";  # 1+2

# Even if you intentionally jumble the expected roles of numbers and strings, thing just work out
say hex int( (2 . 0 x '2') ** substr 98.5, '2', '2' ) . 'beef'; # 1359599
```


On the other hand, since Perl gives you a lot of rope, you have to be careful what you do with it.  The expression
<code>'x' +  1</code> will return the answer '1', silently glossing over the meaningless use of an alphabetic character in addition.
This is the reason that <code>use warnings</code> should be present in most all your Perl code.  Enabling warnings will alert you that <tt>Argument "x" isn't numeric in addition</tt>.


## Perl 6

Perl 6 was designed with a specific goal of maximizing the principle of DWIM (Do What I Mean) while simultaneously minimizing the principle of DDWIDM (Don't Do What I Don't Mean). Implicit type conversion is a natural and basic feature.

Variable names in Perl 6 are prepended with a sigil.
The most basic variable container type is a scalar, with the sigil dollar sign: $x.
A single scalar variable in list context will be converted to a list of one element regardless of the variables structure.
(A scalar variable may be bound to any object, including a collective object.
A scalar variable is always treated as a singular item, regardless of whether the object is essentially composite or unitary.
There is no implicit conversion from singular to plural; a plural object within a singular container must be explicitly decontainerized somehow.
Use of a subscript is considered sufficiently explicit though.)

The type of object contained in a scalar depends on how you assign it and how you use it.


```perl6
my $x;

$x = 1234;      say $x.WHAT; # (Int) Integer
$x = 12.34;     say $x.WHAT; # (Rat) Rational
$x = 1234e-2;   say $x.WHAT; # (Num) Floating point Number
$x = 1234+i;    say $x.WHAT; # (Complex)
$x = '1234';    say $x.WHAT; # (Str) String
$x = (1,2,3,4); say $x.WHAT; # (List)
$x = [1,2,3,4]; say $x.WHAT; # (Array)
$x = 1 .. 4;    say $x.WHAT; # (Range)
$x = (1 => 2);  say $x.WHAT; # (Pair)
$x = {1 => 2};  say $x.WHAT; # (Hash)
$x = {1, 2};    say $x.WHAT; # (Block)
$x = sub {1};   say $x.WHAT; # (Sub) Code Reference
$x = True;      say $x.WHAT; # (Bool) Boolean
```



Objects may be converted between various types many times during an operation. Consider the following line of code.


```perl6
say :16(([+] 1234.ords).sqrt.floor ~ "beef");
```


In English: Take the floor of the square root of the sum of the ordinals of the digits of the integer 1234, concatenate that number with the string 'beef', interpret the result as a hexadecimal number and print it.

Broken down step by step:


```perl6
my $x = 1234;                                  say $x, ' ', $x.WHAT; # 1234 (Int)
$x = 1234.ords;                                say $x, ' ', $x.WHAT; # 49 50 51 52 (List)
$x = [+] 1234.ords;                            say $x, ' ', $x.WHAT; # 202 (Int)
$x = ([+] 1234.ords).sqrt;                     say $x, ' ', $x.WHAT; # 14.2126704035519 (Num)
$x = ([+] 1234.ords).sqrt.floor;               say $x, ' ', $x.WHAT; # 14 (Int)
$x = ([+] 1234.ords).sqrt.floor ~ "beef";      say $x, ' ', $x.WHAT; # 14beef (Str)
$x = :16(([+] 1234.ords).sqrt.floor ~ "beef"); say $x, ' ', $x.WHAT; # 1359599 (Int)
```



Some types are not implicitly converted.
For instance, you must explicitly request and cast to Complex numbers and FatRat numbers.
(A normal Rat number has a denominator that is limited to 64 bits, with underflow to floating point to prevent performance degradation; a FatRat, in contrast, has an unlimited denominator size, and can chew up all your memory if you're not careful.)


```perl6
my $x;
$x = (-1).sqrt;           say $x, ' ', $x.WHAT; # NaN (Num)
$x = (-1).Complex.sqrt;   say $x, ' ', $x.WHAT; # 6.12323399573677e-17+1i (Complex)

$x = (22/7) * 2;          say $x, ' ', $x.WHAT; # 6.285714 (Rat)
$x /= 10**10;             say $x, ' ', $x.WHAT; # 0.000000000629 (Rat)
$x /= 10**10;             say $x, ' ', $x.WHAT; # 6.28571428571429e-20 (Num)

$x = (22/7).FatRat * 2;   say $x, ' ', $x.WHAT; # 6.285714 (FatRat)
$x /= 10**10;             say $x, ' ', $x.WHAT; # 0.000000000629 (FatRat)
$x /= 10**10;             say $x, ' ', $x.WHAT; # 0.0000000000000000000629 (FatRat)

```


User defined types will support implicit casting if the object has Bridge method that tells it how to do so, or if the operators in question supply multiple dispatch variants that allow for coercions.
Note that Perl 6 does not support implicit assignment coercion to typed variables.
However, different-sized storage types (int16, int32, int64, for example) are not considered different types, and such assignment merely enforces a constraint that will throw an exception if the size is exceeded.  (The calculations on the right side of the assignment are done in an arbitrarily large type such as Int.)

Types may be explicitly cast by using a bridge method (.Int, .Rat, .Str, whatever) or by using a coercion operator:


```txt

    + or -      numify
    ~           stringify
    ? or !      boolify
    i (postfix) complexify
    $()         singularize
    @()         pluralize
    %()         hashify
```



## Phix

If a numerical operation on floating points results in an exact integer, that is how it is stored, eg 1.5+1.5

Likewise a numerical operation on integers can need to be stored in a float, eg 1/3 or #123456*#123456.

If a string character (or slice) is replaced with any value that will not fit in a byte, it is automatically
converted to a dword_sequence, eg

```Phix
sequence s = "this"
s[3] = PI -- s is now {'t','h',3.1415926,'s'}
```

Phix does not, or at least tries very hard not to "drop bits" or "clock round". 1/3 is 0.333333 not 0, 0-1 is
-1 not +#FFFFFFFF, in all cases, with a plain english fatal run-time error should a type check occur.


## Python

Python does do some automatic conversions between different types but is still considered a strongly typed language. Allowed automatic conversions include between numeric types (where it makes sense), and the general rule that empty container types as well as zero are considered False in a boolean context.

```python
from fractions import Fraction
from decimal import Decimal, getcontext
getcontext().prec = 60
from itertools import product

casting_functions = [int, float, complex,   # Numbers
                     Fraction, Decimal,     # Numbers
                     hex, oct, bin,         # Int representations - not strictly types
                     bool,                  # Boolean/integer Number
                     iter,                  # Iterator type
                     list, tuple, range,    # Sequence types
                     str, bytes,            # Strings, byte strings
                     bytearray,             # Mutable bytes
                     set, frozenset,        # Set, hashable set
                     dict,                  # hash mapping dictionary
                    ]

examples_of_types = [0, 42,
                     0.0 -0.0, 12.34, 56.0,
                     (0+0j), (1+2j), (1+0j), (78.9+0j), (0+1.2j),
                     Fraction(0, 1), Fraction(22, 7), Fraction(4, 2),
                     Decimal('0'),
                     Decimal('3.14159265358979323846264338327950288419716939937510'),
                     Decimal('1'), Decimal('1.5'),
                     True, False,
                     iter(()), iter([1, 2, 3]), iter({'A', 'B', 'C'}),
                     iter([[1, 2], [3, 4]]), iter((('a', 1), (2, 'b'))),
                     [], [1, 2], [[1, 2], [3, 4]],
                     (), (1, 'two', (3+0j)), (('a', 1), (2, 'b')),
                     range(0), range(3),
                     "", "A", "ABBA", "Milü",
                     b"", b"A", b"ABBA",
                     bytearray(b""), bytearray(b"A"), bytearray(b"ABBA"),
                     set(), {1, 'two', (3+0j), (4, 5, 6)},
                     frozenset(), frozenset({1, 'two', (3+0j), (4, 5, 6)}),
                     {}, {1: 'one', 'two': (2+3j), ('RC', 3): None}
                    ]
if __name__ == '__main__':
    print('Common Python types/type casting functions:')
    print('  ' + '\n  '.join(f.__name__ for f in casting_functions))
    print('\nExamples of those types:')
    print('  ' + '\n  '.join('%-26s %r' % (type(e), e) for e in examples_of_types))
    print('\nCasts of the examples:')
    for f, e in product(casting_functions, examples_of_types):
        try:
            ans = f(e)
        except BaseException:
            ans = 'EXCEPTION RAISED!'
        print('%-60s -> %r' % ('%s(%r)' % (f.__name__, e), ans))
```


{{out}} (Elided due to size)

```txt
Common Python types/type casting functions:
  int
  float
  complex
  Fraction
  Decimal
  hex
  oct
  bin
  bool
  iter
  list
  tuple
  range
  str
  bytes
  bytearray
  set
  frozenset
  dict

Examples of those types:
  <class 'int'>              0
  <class 'int'>              42
  <class 'float'>            0.0
  <class 'float'>            12.34
  <class 'float'>            56.0
  <class 'complex'>          0j
  <class 'complex'>          (1+2j)
  <class 'complex'>          (1+0j)
  <class 'complex'>          (78.9+0j)
  <class 'complex'>          1.2j
  <class 'fractions.Fraction'> Fraction(0, 1)
  <class 'fractions.Fraction'> Fraction(22, 7)
  <class 'fractions.Fraction'> Fraction(2, 1)
  <class 'decimal.Decimal'>  Decimal('0')
  <class 'decimal.Decimal'>  Decimal('3.14159265358979323846264338327950288419716939937510')
  <class 'decimal.Decimal'>  Decimal('1')
  <class 'decimal.Decimal'>  Decimal('1.5')
  <class 'bool'>             True
  <class 'bool'>             False
  <class 'tuple_iterator'>   <tuple_iterator object at 0x00000085D128E438>
  <class 'list_iterator'>    <list_iterator object at 0x00000085D128E550>
  <class 'set_iterator'>     <set_iterator object at 0x00000085D127EAF8>
  <class 'list_iterator'>    <list_iterator object at 0x00000085D128E668>
  <class 'tuple_iterator'>   <tuple_iterator object at 0x00000085D128E5C0>
  <class 'list'>             []
  <class 'list'>             [1, 2]
  <class 'list'>             [[1, 2], [3, 4]]
  <class 'tuple'>            ()
  <class 'tuple'>            (1, 'two', (3+0j))
  <class 'tuple'>            (('a', 1), (2, 'b'))
  <class 'range'>            range(0, 0)
  <class 'range'>            range(0, 3)
  <class 'str'>              ''
  <class 'str'>              'A'
  <class 'str'>              'ABBA'
  <class 'str'>              'Milü'
  <class 'bytes'>            b''
  <class 'bytes'>            b'A'
  <class 'bytes'>            b'ABBA'
  <class 'bytearray'>        bytearray(b'')
  <class 'bytearray'>        bytearray(b'A')
  <class 'bytearray'>        bytearray(b'ABBA')
  <class 'set'>              set()
  <class 'set'>              {1, 'two', (3+0j), (4, 5, 6)}
  <class 'frozenset'>        frozenset()
  <class 'frozenset'>        frozenset({1, 'two', (3+0j), (4, 5, 6)})
  <class 'dict'>             {}
  <class 'dict'>             {1: 'one', 'two': (2+3j), ('RC', 3): None}

Casts of the examples:
int(0)                                                       -> 0
int(42)                                                      -> 42
int(0.0)                                                     -> 0
int(12.34)                                                   -> 12
int(56.0)                                                    -> 56
int(0j)                                                      -> 'EXCEPTION RAISED!'
int((1+2j))                                                  -> 'EXCEPTION RAISED!'
int((1+0j))                                                  -> 'EXCEPTION RAISED!'
int((78.9+0j))                                               -> 'EXCEPTION RAISED!'
int(1.2j)                                                    -> 'EXCEPTION RAISED!'
int(Fraction(0, 1))                                          -> 0
int(Fraction(22, 7))                                         -> 3
int(Fraction(2, 1))                                          -> 2
int(Decimal('0'))                                            -> 0
int(Decimal('3.14159265358979323846264338327950288419716939937510')) -> 3
int(Decimal('1'))                                            -> 1
int(Decimal('1.5'))                                          -> 1
int(True)                                                    -> 1
int(False)                                                   -> 0
int(<tuple_iterator object at 0x00000085D128E438>)           -> 'EXCEPTION RAISED!'
int(<list_iterator object at 0x00000085D128E550>)            -> 'EXCEPTION RAISED!'
int(<set_iterator object at 0x00000085D127EAF8>)             -> 'EXCEPTION RAISED!'
int(<list_iterator object at 0x00000085D128E668>)            -> 'EXCEPTION RAISED!'
int(<tuple_iterator object at 0x00000085D128E5C0>)           -> 'EXCEPTION RAISED!'
int([])                                                      -> 'EXCEPTION RAISED!'

'''

dict((('a', 1), (2, 'b')))                                   -> {'a': 1, 2: 'b'}
dict(range(0, 0))                                            -> {}
dict(range(0, 3))                                            -> 'EXCEPTION RAISED!'
dict('')                                                     -> {}
dict('A')                                                    -> 'EXCEPTION RAISED!'
dict('ABBA')                                                 -> 'EXCEPTION RAISED!'
dict('Milü')                                                 -> 'EXCEPTION RAISED!'
dict(b'')                                                    -> {}
dict(b'A')                                                   -> 'EXCEPTION RAISED!'
dict(b'ABBA')                                                -> 'EXCEPTION RAISED!'
dict(bytearray(b''))                                         -> {}
dict(bytearray(b'A'))                                        -> 'EXCEPTION RAISED!'
dict(bytearray(b'ABBA'))                                     -> 'EXCEPTION RAISED!'
dict(set())                                                  -> {}
dict({1, 'two', (3+0j), (4, 5, 6)})                          -> 'EXCEPTION RAISED!'
dict(frozenset())                                            -> {}
dict(frozenset({1, 'two', (3+0j), (4, 5, 6)}))               -> 'EXCEPTION RAISED!'
dict({})                                                     -> {}
dict({1: 'one', 'two': (2+3j), ('RC', 3): None})             -> {1: 'one', 'two': (2+3j), ('RC', 3): None}

```



## Racket

The only automatic conversions are in the numeric tower. The common case is in some operations like <code>+</code>, <code>-</code>, <code>*</code>, <code>/</code>, when one of the arguments is of a different type of the other argument. For example, in all the following cases the fixnum <code>1</code> is added to more general kinds of numbers.

```Racket
#lang racket

(+ 1 .1) ; ==> 1.1
(+ 1 0+1i) ; ==> 1+1i
(+ 1 1/2) ; ==> 3/2
(+ 1 (expt 10 30)) ; ==> 1000000000000000000000000000001

```



## REXX


```txt

╔═══════════════════════════════════════════════════════════════════════════════════╗
║ The REXX language has conversion, if  normalization  can be regarded as a type of ║
║ conversion.  Normalization can remove all blanks from (numeric) literals, leading ║
║ plus (+) signs,  the decimal point  (if it's not significant), and leading and/or ║
║ trailing zeroes  (except for zero itself), remove insignificant leading zeroes in ║
║ the exponent,  add a plus sign (+) for any positive exponent, and will capitalize ║
║ the    "e"    in an exponentiated number.                                         ║
║                                                                                   ║
║ Almost all numerical expressions can be normalized after computation, shown below ║
║ are a few examples.   Other expressions with non-numeric values are treated as    ║
║ simple literals.                                                                  ║
║                                                                                   ║
║ Note that REXX can store the number with leading signs,  leading,  trailing,  and ║
║ sometimes imbedded blanks  (which can only occur after a leading sign).           ║
║                                                                                   ║
║ Also noted is how numbers can be assigned using quotes [']  or  apostrophes ["].  ║
╚═══════════════════════════════════════════════════════════════════════════════════╝

```


```rexx
/*REXX program demonstrates various ways REXX can convert and/or normalize some numbers.*/
digs=digits()   ;                                     say digs   /* 9,  the default.*/

a=.1.2...$      ;                                     say a      /* .1.2...$        */
a=+7            ;                                     say a      /* 7               */
a='+66'         ;                                     say a      /* +66             */
a='- 66.'       ;                                     say a      /* - 66.           */
a=- 66          ;                                     say a      /* -66             */
a=- 66.         ;                                     say a      /* -66             */
a=+ 66          ;                                     say a      /* 66              */
a=1             ;    b=2.000   ;    x=a+b      ;      say x      /* 3.000           */
a=1             ;    b=2.000   ;    x=(a+b)/1  ;      say x      /* 3               */
a=+2            ;    b=+3      ;    x=a+b      ;      say x      /* 5               */
a=+5            ;    b=+3e1    ;    x=a+b      ;      say x      /* 35              */
a=1e3           ;                                     say a      /* 1E3             */
a="1e+003"      ;                                     say a      /* 1e+003          */
a=1e+003        ;                                     say a      /* 1E+003          */
a=1e+003        ;    b=0       ;    x=a+b      ;      say x      /* 1000            */
a=12345678912   ;                                     say a      /* 123456789012    */
a=12345678912   ;    b=0       ;    x=a+b      ;      say x      /* 1.23456789E+10  */
```

'''output'''

```txt

9
.1.2...$
7
+66
- 66.
-66
-66
66
3.000
3
5
35
1E3
1e+003
1E+003
1000
12345678912
1.23456789E+10

```



## Scala

See the section [https://docs.scala-lang.org/tour/implicit-conversions.html Tour of Scala about impplicit conversions.]

## Sidef

Since version 3.00, all the number types (int, rat, float and complex) are unified in the ''Number'' class and all the needed conversions are done implicitly. Methods from other classes also make implicit conversions where possible.

```ruby>
 1+"2"            #=> 3
> "1"+2            #=> 12
> sqrt(-4)         #=> 2i
> ("a" + [1,2])    #=> a[1,2]
> ('ha' * '3')     #=> hahaha
> ('ha' * true)    #=> ha
```



## Tcl

Virtually all type conversions in Tcl are implicit.
A value is an integer (or a string, or a list, or …) because that is how you are using it.
The only true explicit type conversion operations are some of the functions in the expression sub-language (<code>int()</code>, <code>double()</code>, etc.).

;Integer conversion:
```tcl
set value "123"
incr someVar $value
# $value will now hold an integer (strictly, one of many integer-related types) with value 123
```


;Float conversion:
```tcl
set value "1.23"
expr {$value + 3.5}
# $value will now hold a double-precision IEEE floating point number that is (approx.) 1.23
```
<!-- IEEE binary floats can't hold the value exactly -->

;String conversion:
```tcl
set value [expr {123 + 456}]
string length $value
# $value will now hold a string (of length 3)
```


;List conversion:
```tcl
set value {a b c d}
llength $value
# $value will now hold a list (of length 4)
```


;Dictionary conversion:
```tcl
set value {a b c d}
dict size $value
# $value will now hold a dictionary (of size 2)
```

There are many other value types (command names, variable names, subcommand index names, etc.) but user code would not normally seek to explicitly convert to those.

Defining a new type requires writing an extension to Tcl in [[C]] (or whatever the host programming language is, so [[Java]] for [[JTcl]]); the interfaces for doing this are not directly exposed to the Tcl script level because they require direct memory access, which Tcl normally does not permit in order to promote overall process stability.


## zkl

Type conversions usually just happen (ie the object knows what it wants and attempts to convert) but sometimes the conversion needs to be explicit (ie the conversion is ambiguous, the object doesn't know about the other type or is too lazy to convert).

```zkl
zkl: 1+"2"
3
zkl: "1"+2
12
zkl: 1/2
0
zkl: (1).toFloat()/2
0.5
zkl: T("one",1,"two",2).toDictionary()
D(two:2,one:1)
zkl: T("one",1,"two",2).toDictionary().toList()
L(L("two",2),L("one",1))
zkl: T("one",1,"two",2).toDictionary().toList().toDictionary()
D(two:2,one:1)
etc
```

