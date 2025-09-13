+++
title = "Modular arithmetic"
description = ""
date = 2019-09-12T19:12:47Z
aliases = []
[extra]
id = 12952
[taxonomies]
categories = ["task"]
tags = []
+++

{{draft task}} [[Category:Mathematics]]
'''[[wp:Modular arithmetic|Modular arithmetic]]''' is a form of arithmetic (a calculation technique involving the concepts of addition and multiplication) which is done on numbers with a defined [[wp:equivalence relation|equivalence relation]] called ''congruence''.

For any positive integer <math>p</math> called the ''congruence modulus'',
two numbers <math>a</math> and <math>b</math> are said to be ''congruent modulo p'' whenever there exists an integer <math>k</math> such that:
:<math>a = b + k\,p</math>

The corresponding set of [[wp:equivalence class|equivalence class]]es forms a [[wp:ring (mathematics)|ring]] denoted <math>\frac{\Z}{p\Z}</math>.

Addition and multiplication on this ring have the same algebraic structure as in usual arithmetics, so that a function such as a polynomial expression could receive a ring element as argument and give a consistent result.

The purpose of this task is to show, if your programming language allows it,
how to redefine operators so that they can be used transparently on modular integers.
You can do it either by using a dedicated library, or by implementing your own class.

You will use the following function for demonstration:
:<math>f(x) = x^{100} + x + 1</math>
You will use <math>13</math> as the congruence modulus and you will compute <math>f(10)</math>.

It is important that the function <math>f</math> is agnostic about whether or not its argument is modular; it should behave the same way with normal and modular integers.
In other words, the function is an algebraic expression that could be used with any ring, not just integers.





## ALGOL 68

```algol68
# allow for large integers in Algol 68G #
PR precision 200 PR

# modular integer type #
MODE MODULARINT = STRUCT( LONG LONG INT v, INT modulus );

# modular integer + and * operators #
# where both operands are modular, they must have the same modulus #
OP +  = ( MODULARINT a,     b )MODULARINT: ( ( v OF a + v OF b ) MOD modulus OF a, modulus OF a );
OP +  = ( MODULARINT a, INT b )MODULARINT: ( ( v OF a + b      ) MOD modulus OF a, modulus OF a );
OP *  = ( MODULARINT a,     b )MODULARINT: ( ( v OF a * v OF b ) MOD modulus OF a, modulus OF a );
OP ** = ( MODULARINT a, INT b )MODULARINT: ( ( v OF a ** b     ) MOD modulus OF a, modulus OF a );

# f(x) function - can be applied to either LONG LONG INT or MODULARINT values #
# the result is always a LONG LONG INT #
PROC f = ( UNION( LONG LONG INT, MODULARINT ) x )LONG LONG INT:
    CASE x
      IN ( LONG LONG INT ix ):      ( ix**100 + ix + 1 )
       , ( MODULARINT    mx ): v OF ( mx**100 + mx + 1 )
    ESAC;

print( ( whole( f( MODULARINT( 10, 13 ) ), 0 ), newline ) )
```

```txt

1

```



## C

```c
#include <stdio.h>

struct ModularArithmetic {
    int value;
    int modulus;
};

struct ModularArithmetic make(const int value, const int modulus) {
    struct ModularArithmetic r = { value % modulus, modulus };
    return r;
}

struct ModularArithmetic add(const struct ModularArithmetic a, const struct ModularArithmetic b) {
    return make(a.value + b.value, a.modulus);
}

struct ModularArithmetic addi(const struct ModularArithmetic a, const int v) {
    return make(a.value + v, a.modulus);
}

struct ModularArithmetic mul(const struct ModularArithmetic a, const struct ModularArithmetic b) {
    return make(a.value * b.value, a.modulus);
}

struct ModularArithmetic pow(const struct ModularArithmetic b, int pow) {
    struct ModularArithmetic r = make(1, b.modulus);
    while (pow-- > 0) {
        r = mul(r, b);
    }
    return r;
}

void print(const struct ModularArithmetic v) {
    printf("ModularArithmetic(%d, %d)", v.value, v.modulus);
}

struct ModularArithmetic f(const struct ModularArithmetic x) {
    return addi(add(pow(x, 100), x), 1);
}

int main() {
    struct ModularArithmetic input = make(10, 13);
    struct ModularArithmetic output = f(input);

    printf("f(");
    print(input);
    printf(") = ");
    print(output);
    printf("\n");

    return 0;
}
```

```txt
f(ModularInteger(10, 13)) = ModularInteger(1, 13)
```



## C++

```cpp
#include <iostream>
#include <ostream>

template<typename T>
T f(const T& x) {
    return (T) pow(x, 100) + x + 1;
}

class ModularInteger {
private:
    int value;
    int modulus;

    void validateOp(const ModularInteger& rhs) const {
        if (modulus != rhs.modulus) {
            throw std::runtime_error("Left-hand modulus does not match right-hand modulus.");
        }
    }

public:
    ModularInteger(int v, int m) {
        modulus = m;
        value = v % m;
    }

    int getValue() const {
        return value;
    }

    int getModulus() const {
        return modulus;
    }

    ModularInteger operator+(const ModularInteger& rhs) const {
        validateOp(rhs);
        return ModularInteger(value + rhs.value, modulus);
    }

    ModularInteger operator+(int rhs) const {
        return ModularInteger(value + rhs, modulus);
    }

    ModularInteger operator*(const ModularInteger& rhs) const {
        validateOp(rhs);
        return ModularInteger(value * rhs.value, modulus);
    }

    friend std::ostream& operator<<(std::ostream&, const ModularInteger&);
};

std::ostream& operator<<(std::ostream& os, const ModularInteger& self) {
    return os << "ModularInteger(" << self.value << ", " << self.modulus << ")";
}

ModularInteger pow(const ModularInteger& lhs, int pow) {
    if (pow < 0) {
        throw std::runtime_error("Power must not be negative.");
    }

    ModularInteger base(1, lhs.getModulus());
    while (pow-- > 0) {
        base = base * lhs;
    }
    return base;
}

int main() {
    using namespace std;

    ModularInteger input(10, 13);
    auto output = f(input);
    cout << "f(" << input << ") = " << output << endl;

    return 0;
}
```

```txt
f(ModularInteger(10, 13)) = ModularInteger(1, 13)
```



## C#

```c#
using System;

namespace ModularArithmetic {
    interface IAddition<T> {
        T Add(T rhs);
    }
    interface IMultiplication<T> {
        T Multiply(T rhs);
    }
    interface IPower<T> {
        T Power(int pow);
    }
    interface IOne<T> {
        T One();
    }

    class ModInt : IAddition<ModInt>, IMultiplication<ModInt>, IPower<ModInt>, IOne<ModInt> {
        private int modulo;

        public ModInt(int value, int modulo) {
            Value = value;
            this.modulo = modulo;
        }

        public int Value { get; }

        public ModInt One() {
            return new ModInt(1, modulo);
        }

        public ModInt Add(ModInt rhs) {
            return this + rhs;
        }

        public ModInt Multiply(ModInt rhs) {
            return this * rhs;
        }

        public ModInt Power(int pow) {
            return Pow(this, pow);
        }

        public override string ToString() {
            return string.Format("ModInt({0}, {1})", Value, modulo);
        }

        public static ModInt operator +(ModInt lhs, ModInt rhs) {
            if (lhs.modulo != rhs.modulo) {
                throw new ArgumentException("Cannot add rings with different modulus");
            }
            return new ModInt((lhs.Value + rhs.Value) % lhs.modulo, lhs.modulo);
        }

        public static ModInt operator *(ModInt lhs, ModInt rhs) {
            if (lhs.modulo != rhs.modulo) {
                throw new ArgumentException("Cannot add rings with different modulus");
            }
            return new ModInt((lhs.Value * rhs.Value) % lhs.modulo, lhs.modulo);
        }

        public static ModInt Pow(ModInt self, int p) {
            if (p < 0) {
                throw new ArgumentException("p must be zero or greater");
            }

            int pp = p;
            ModInt pwr = self.One();
            while (pp-- > 0) {
                pwr *= self;
            }
            return pwr;
        }
    }

    class Program {
        static T F<T>(T x) where T : IAddition<T>, IMultiplication<T>, IPower<T>, IOne<T> {
            return x.Power(100).Add(x).Add(x.One());
        }

        static void Main(string[] args) {
            ModInt x = new ModInt(10, 13);
            ModInt y = F(x);
            Console.WriteLine("x ^ 100 + x + 1 for x = {0} is {1}", x, y);
        }
    }
}
```

```txt
x ^ 100 + x + 1 for x = ModInt(10, 13) is ModInt(1, 13)
```



## D


```D
import std.stdio;

version(unittest) {
    void assertEquals(T)(T actual, T expected) {
        import core.exception;
        import std.conv;
        if (actual != expected) {
            throw new AssertError("Actual [" ~ to!string(actual) ~ "]; Expected [" ~ to!string(expected) ~ "]");
        }
    }
}

void main() {
    auto input = ModularInteger(10,13);
    auto output = f(input);
    writeln("f(", input, ") = ", output);
}

V f(V)(const V x) {
    return x^^100 + x + 1;
}

/// Integer tests on f
unittest {
    assertEquals(f(1), 3);
    assertEquals(f(0), 1);
}

/// Floating tests on f
unittest {
    assertEquals(f(1.0), 3.0);
    assertEquals(f(0.0), 1.0);
}

struct ModularInteger {
    private:
    int value;
    int modulus;

    public:
    this(int value, int modulus) {
        this.modulus = modulus;
        this.value = value % modulus;
    }

    ModularInteger opBinary(string op : "+")(ModularInteger rhs) const in {
        assert(this.modulus == rhs.modulus);
    } body {
        return ModularInteger((this.value + rhs.value) % this.modulus, this.modulus);
    }

    ModularInteger opBinary(string op : "+")(int rhs) const {
        return ModularInteger((this.value + rhs) % this.modulus, this.modulus);
    }

    ModularInteger opBinary(string op : "*")(ModularInteger rhs) const in {
        assert(this.modulus == rhs.modulus);
        assert(this.value < this.modulus);
        assert(rhs.value < this.modulus);
    } body {
        return ModularInteger((this.value * rhs.value) % this.modulus, this.modulus);
    }

    ModularInteger opBinary(string op : "^^")(int pow) const in {
        assert(pow >= 0);
    } body {
        auto base = ModularInteger(1, this.modulus);
        while (pow-- > 0) {
            base = base * this;
        }
        return base;
    }

    string toString() {
        import std.format;
        return format("ModularInteger(%s, %s)", value, modulus);
    }
}

/// Addition with same type of int
unittest {
    auto a = ModularInteger(2,5);
    auto b = ModularInteger(3,5);
    assertEquals(a+b, ModularInteger(0,5));
}

/// Addition with differnt int types
unittest {
    auto a = ModularInteger(2,5);
    assertEquals(a+0, a);
    assertEquals(a+1, ModularInteger(3,5));
}

/// Muliplication
unittest {
    auto a = ModularInteger(2,5);
    auto b = ModularInteger(3,5);
    assertEquals(a*b, ModularInteger(1,5));
}

/// Power
unittest {
    const a = ModularInteger(3,13);
    assertEquals(a^^2, ModularInteger(9,13));
    assertEquals(a^^3, ModularInteger(1,13));

    const b = ModularInteger(10,13);
    assertEquals(b^^1, ModularInteger(10,13));
    assertEquals(b^^2, ModularInteger(9,13));
    assertEquals(b^^3, ModularInteger(12,13));
    assertEquals(b^^4, ModularInteger(3,13));
    assertEquals(b^^5, ModularInteger(4,13));
    assertEquals(b^^6, ModularInteger(1,13));
    assertEquals(b^^7, ModularInteger(10,13));
    assertEquals(b^^8, ModularInteger(9,13));
    assertEquals(b^^10, ModularInteger(3,13));
    assertEquals(b^^20, ModularInteger(9,13));
    assertEquals(b^^30, ModularInteger(1,13));
    assertEquals(b^^50, ModularInteger(9,13));
    assertEquals(b^^75, ModularInteger(12,13));
    assertEquals(b^^90, ModularInteger(1,13));
    assertEquals(b^^95, ModularInteger(4,13));
    assertEquals(b^^97, ModularInteger(10,13));
    assertEquals(b^^98, ModularInteger(9,13));
    assertEquals(b^^99, ModularInteger(12,13));
    assertEquals(b^^100, ModularInteger(3,13));
}
```

```txt

f(ModularInteger(10, 13)) = ModularInteger(1, 13)

```



## Factor

While it's probably not the best idea to define methods in arithmetic words that specialize on custom classes, it <i>can</i> be done. There are a few pitfalls to doing so, which is why custom types typically implement their own arithmetic words. Examples are words like <code>v+</code> from the <code>math.vectors</code> vocabulary and <code>q+</code> from the <code>math.quaternions</code> vocabulary.

The pitfalls are as follows:<br />
First, arithmetic words are declared using <code>MATH:</code>, which means they use the math method combination. These methods will dispatch on both their arguments, and promote lower-priority numeric types to higher-priority types when both types are different. The math method combination also means that methods added to <code>MATH:</code> words <i>cannot</i> specialize on any classes except for <code>fixnum</code>,
<code>bignum</code>, <code>ratio</code>, <code>float</code>, <code>complex</code>, <code>object</code>, or unions of them.

This is a bit of a problem, because we must specialize on <code>object</code> and then do a bunch of manual type checking and stack shuffling to make sure we are performing the correct operations on the correct objects.

Second, if any other vocabularies add methods that specialize on arithmetic words, they will conflict with our modular arithmetic vocabulary due to the aforementioned inability to specialize on specific classes.

For these reasons, I would normally opt to define my own arithmetic words, with the added bonus of being able to use non-<code>MATH:</code> multiple dispatch (from the <code>multi-methods</code> vocabulary) to cleanly implement mixed-type dispatch.

Also note that since <code>^</code> is not a generic word, we employ the strategy of renaming it to <code>**</code> inside our vocabulary and defining a new word named <code>^</code> that can also handle modular integers. This is an acceptable way to handle it because Factor has pretty good word-disambiguation faculties. I just wouldn't want to have to employ them for more frequently-used arithmetic.


```factor
USING: accessors generalizations io kernel math math.functions
parser prettyprint prettyprint.custom sequences ;
IN: rosetta-code.modular-arithmetic
RENAME: ^ math.functions => **

! Define a modular integer class.
TUPLE: mod-int
    { n integer read-only } { mod integer read-only } ;

! Define a constructor for mod-int.
C: <mod-int> mod-int

ERROR: non-equal-modulus m1 m2 ;

! Define a literal syntax for mod-int.
<< SYNTAX: MI{ \ } [ first2 <mod-int> ] parse-literal ; >>

! Implement prettyprinting for mod-int custom syntax.
M: mod-int pprint-delims drop \ MI{ \ } ;
M: mod-int >pprint-sequence [ n>> ] [ mod>> ] bi { } 2sequence ;
M: mod-int pprint* pprint-object ;

<PRIVATE

! Helper words for displaying the results of an arithmetic
! operation.
: show ( quot -- )
    [ unparse 2 tail but-last "= " append write ] [ call . ] bi
    ; inline

: 2show ( quots -- )
    [ 2curry show ] map-compose [ call( -- ) ] each ; inline

! Check whether two mod-ints have the same modulus and throw an
! error if not.
: check-mod ( m1 m2 -- )
    2dup [ mod>> ] bi@ = [ 2drop ] [ non-equal-modulus ] if ;

! Apply quot to the integer parts of two mod-ints and create a
! new mod-int from the result.
: mod-int-op ( m1 m2 quot -- m3 )
    [ [ n>> ] bi@ ] prepose [ 2dup check-mod ] dip over
    mod>> [ call( x x -- x ) ] dip [ mod ] keep <mod-int>
    ; inline

! Promote an integer to a mod-int and call mod-int-op.
: integer-op ( obj1 obj2 quot -- mod-int )
    [
        dup integer?
        [ over mod>> <mod-int> ]
        [ dup [ mod>> <mod-int> ] dip ] if
    ] dip mod-int-op ; inline

! Apply quot, a binary function, to any combination of integers
! and mod-ints.
: binary-op ( obj1 obj2 quot -- mod-int )
    2over [ mod-int? ] both? [ mod-int-op ] [ integer-op ] if
    ; inline

PRIVATE>

! This is where the arithmetic words are 'redefined' by adding a
! method to them that specializes on the object class.
M: object + [ + ] binary-op ;
M: object - [ - ] binary-op ;
M: object * [ * ] binary-op ;
M: object /i [ /i ] binary-op ;

! ^ is a special case because it is not generic.
: ^ ( obj1 obj2 -- obj3 )
    2dup [ mod-int? ] either? [ [ ** ] binary-op ] [ ** ] if ;

: fn ( obj -- obj' ) dup 100 ^ + 1 + ;

: modular-arithmetic-demo ( -- )
    [ MI{ 10 13 } fn ]
    [ 2 fn ] [ show ] bi@
    {
        [ MI{ 10 13 } MI{ 5 13 } [ + ] ]
        [ MI{ 10 13 } 5 [ + ] ]
        [ 5 MI{ 10 13 } [ + ] ]
        [ MI{ 10 13 } 2 [ /i ] ]
        [ 5 10 [ * ] ]
        [ MI{ 3 7 } MI{ 4 7 } [ * ] ]
        [ MI{ 3 7 } 50 [ ^ ] ]
    } 2show ;

MAIN: modular-arithmetic-demo
```

```txt

MI{ 10 13 } fn = MI{ 1 13 }
2 fn = 1267650600228229401496703205379
MI{ 10 13 } MI{ 5 13 } + = MI{ 2 13 }
MI{ 10 13 } 5 + = MI{ 2 13 }
5 MI{ 10 13 } + = MI{ 2 13 }
MI{ 10 13 } 2 /i = MI{ 5 13 }
5 10 * = 50
MI{ 3 7 } MI{ 4 7 } * = MI{ 5 7 }
MI{ 3 7 } 50 ^ = MI{ 2 7 }

```



## Go

Go does not allow redefinition of operators.  That element of the task cannot be done in Go.  The element of defining f so that it can be used with any ring however can be done, just not with the syntactic sugar of operator redefinition.

```go
package main

import "fmt"

// Define enough of a ring to meet the needs of the task.  Addition and
// multiplication are mentioned in the task; multiplicative identity is not
// mentioned but is useful for the power function.

type ring interface {
    add(ringElement, ringElement) ringElement
    mul(ringElement, ringElement) ringElement
    mulIdent() ringElement
}

type ringElement interface{}

// Define a power function that works for any ring.

func ringPow(r ring, a ringElement, p uint) (pow ringElement) {
    for pow = r.mulIdent(); p > 0; p-- {
        pow = r.mul(pow, a)
    }
    return
}

// The task function f has that constant 1 in it.
// Define a special kind of ring that has this element.

type oneRing interface {
    ring
    one() ringElement // return ring element corresponding to '1'
}

// Now define the required function f.
// It works for any ring (that has a "one.")

func f(r oneRing, x ringElement) ringElement {
    return r.add(r.add(ringPow(r, x, 100), x), r.one())
}

// With rings and the function f defined in a general way, now define
// the specific ring of integers modulo n.

type modRing uint // value is congruence modulus n

func (m modRing) add(a, b ringElement) ringElement {
    return (a.(uint) + b.(uint)) % uint(m)
}

func (m modRing) mul(a, b ringElement) ringElement {
    return (a.(uint) * b.(uint)) % uint(m)
}

func (modRing) mulIdent() ringElement { return uint(1) }

func (modRing) one() ringElement { return uint(1) }

// Demonstrate the general function f on the specific ring with the
// specific values.

func main() {
    fmt.Println(f(modRing(13), uint(10)))
}
```

```txt

1

```



## Haskell


```haskell
-- We use a couple of GHC extensions to make the program cooler.  They let us
-- use / as an operator and 13 as a literal at the type level.  (The library
-- also provides the fancy Zahlen (ℤ) symbol as a synonym for Integer.)

{-# Language DataKinds #-}
{-# Language TypeOperators #-}

import Data.Modular

f :: ℤ/13 -> ℤ/13
f x = x^100 + x + 1

main :: IO ()
main = print (f 10)
```


```txt

./modarith
1

```



## Java

```Java
public class ModularArithmetic {
    private interface Ring<T> {
        Ring<T> plus(Ring<T> rhs);

        Ring<T> times(Ring<T> rhs);

        int value();

        Ring<T> one();

        default Ring<T> pow(int p) {
            if (p < 0) {
                throw new IllegalArgumentException("p must be zero or greater");
            }

            int pp = p;
            Ring<T> pwr = this.one();
            while (pp-- > 0) {
                pwr = pwr.times(this);
            }
            return pwr;
        }
    }

    private static class ModInt implements Ring<ModInt> {
        private int value;
        private int modulo;

        private ModInt(int value, int modulo) {
            this.value = value;
            this.modulo = modulo;
        }

        @Override
        public Ring<ModInt> plus(Ring<ModInt> other) {
            if (!(other instanceof ModInt)) {
                throw new IllegalArgumentException("Cannot add an unknown ring.");
            }
            ModInt rhs = (ModInt) other;
            if (modulo != rhs.modulo) {
                throw new IllegalArgumentException("Cannot add rings with different modulus");
            }
            return new ModInt((value + rhs.value) % modulo, modulo);
        }

        @Override
        public Ring<ModInt> times(Ring<ModInt> other) {
            if (!(other instanceof ModInt)) {
                throw new IllegalArgumentException("Cannot multiple an unknown ring.");
            }
            ModInt rhs = (ModInt) other;
            if (modulo != rhs.modulo) {
                throw new IllegalArgumentException("Cannot multiply rings with different modulus");
            }
            return new ModInt((value * rhs.value) % modulo, modulo);
        }

        @Override
        public int value() {
            return value;
        }

        @Override
        public Ring<ModInt> one() {
            return new ModInt(1, modulo);
        }

        @Override
        public String toString() {
            return String.format("ModInt(%d, %d)", value, modulo);
        }
    }

    private static <T> Ring<T> f(Ring<T> x) {
        return x.pow(100).plus(x).plus(x.one());
    }

    public static void main(String[] args) {
        ModInt x = new ModInt(10, 13);
        Ring<ModInt> y = f(x);
        System.out.print("x ^ 100 + x + 1 for x = ModInt(10, 13) is ");
        System.out.println(y);
        System.out.flush();
    }
}
```

```txt
x ^ 100 + x + 1 for x = ModInt(10, 13) is ModInt(1, 13)
```



## Julia

Implements the <tt>Modulo</tt> struct and basic operations.

```julia
struct Modulo{T<:Integer} <: Integer
    val::T
    mod::T
    Modulo(n::T, m::T) where T = new{T}(mod(n, m), m)
end
modulo(n::Integer, m::Integer) = Modulo(promote(n, m)...)

Base.show(io::IO, md::Modulo) = print(io, md.val, " (mod $(md.mod))")
Base.convert(::Type{T}, md::Modulo) where T<:Integer = convert(T, md.val)
Base.copy(md::Modulo{T}) where T = Modulo{T}(md.val, md.mod)

Base.:+(md::Modulo) = copy(md)
Base.:-(md::Modulo) = Modulo(md.mod - md.val, md.mod)
for op in (:+, :-, :*, :÷, :^)
    @eval function Base.$op(a::Modulo, b::Integer)
        val = $op(a.val, b)
        return Modulo(mod(val, a.mod), a.mod)
    end
    @eval Base.$op(a::Integer, b::Modulo) = $op(b, a)
    @eval function Base.$op(a::Modulo, b::Modulo)
        if a.mod != b.mod throw(InexactError()) end
        val = $op(a.val, b.val)
        return Modulo(mod(val, a.mod), a.mod)
    end
end

f(x) = x ^ 100 + x + 1
@show f(modulo(10, 13))
```


```txt
f(modulo(10, 13)) = 11 (mod 13)
```



## Kotlin


```scala
// version 1.1.3

interface Ring<T> {
    operator fun plus(other: Ring<T>): Ring<T>
    operator fun times(other: Ring<T>): Ring<T>
    val value: Int
    val one: Ring<T>
}

fun <T> Ring<T>.pow(p: Int): Ring<T> {
    require(p >= 0)
    var pp = p
    var pwr = this.one
    while (pp-- > 0) pwr *= this
    return pwr
}

class ModInt(override val value: Int, val modulo: Int): Ring<ModInt> {

    override operator fun plus(other: Ring<ModInt>): ModInt {
        require(other is ModInt &&  modulo == other.modulo)
        return ModInt((value + other.value) % modulo, modulo)
    }

    override operator fun times(other: Ring<ModInt>): ModInt {
        require(other is ModInt && modulo == other.modulo)
        return ModInt((value * other.value) % modulo, modulo)
    }

    override val one get() = ModInt(1, modulo)

    override fun toString() = "ModInt($value, $modulo)"
}

fun <T> f(x: Ring<T>): Ring<T> = x.pow(100) + x + x.one

fun main(args: Array<String>) {
    val x = ModInt(10, 13)
    val y = f(x)
    println("x ^ 100 + x + 1 for x == ModInt(10, 13) is $y")
}
```


```txt

x ^ 100 + x + 1 for x == ModInt(10, 13) is ModInt(1, 13)

```



## Lua


```lua
function make(value, modulo)
    local v = value % modulo
    local tbl = {value=v, modulo=modulo}

    local mt = {
        __add = function(lhs, rhs)
            if type(lhs) == "table" then
                if type(rhs) == "table" then
                    if lhs.modulo ~= rhs.modulo then
                        error("Cannot add rings with different modulus")
                    end
                    return make(lhs.value + rhs.value, lhs.modulo)
                else
                    return make(lhs.value + rhs, lhs.modulo)
                end
            else
                error("lhs is not a table in +")
            end
        end,
        __mul = function(lhs, rhs)
            if lhs.modulo ~= rhs.modulo then
                error("Cannot multiply rings with different modulus")
            end
            return make(lhs.value * rhs.value, lhs.modulo)
        end,
        __pow = function(b,p)
            if p<0 then
                error("p must be zero or greater")
            end

            local pp = p
            local pwr = make(1, b.modulo)
            while pp > 0 do
                pp = pp - 1
                pwr = pwr * b
            end
            return pwr
        end,
        __concat = function(lhs, rhs)
            if type(lhs) == "table" and type(rhs) == "string" then
                return "ModInt("..lhs.value..", "..lhs.modulo..")"..rhs
            elseif type(lhs) == "string" and type(rhs) == "table" then
                return lhs.."ModInt("..rhs.value..", "..rhs.modulo..")"
            else
                return "todo"
            end
        end
    }

    setmetatable(tbl, mt)
    return tbl
end

function func(x)
    return x ^ 100 + x + 1
end

-- main
local x = make(10, 13)
local y = func(x)
print("x ^ 100 + x + 1 for "..x.." is "..y)
```

```txt
x ^ 100 + x + 1 for ModInt(10, 13) is ModInt(1, 13)
```



## PARI/GP


This feature exists natively in GP:

```parigp
Mod(3,7)+Mod(4,7)
```



## Perl

There is a CPAN module called Math::ModInt which does the job.

```Perl
use Math::ModInt qw(mod);
sub f { my $x = shift; $x**100 + $x + 1 };
print f mod(10, 13);
```

```txt
mod(1, 13)
```



## Perl 6

There is an ecosystem module called Modular which works basically as Perl 5's Math::ModInt.

```perl6
use Modular;
sub f(\x) { x**100 + x + 1};
say f( 10 Mod 13 )
```

```txt
1 「mod 13」
```



## Phix

Phix does not allow operator overloading, but an f() which is agnostic about whether its parameter is a modular or normal int, we can do.

```Phix
type mi(object m)
    return sequence(m) and length(m)=2 and integer(m[1]) and integer(m[2])
end type

type mii(object m)
    return mi(m) or atom(m)
end type

function mi_one(mii a)
    if atom(a) then a=1 else a[1] = 1 end if
    return a
end function

function mi_add(mii a, mii b)
    if atom(a) then
        if not atom(b) then throw("error") end if
        return a+b
    end if
    if a[2]!=b[2] then throw("error") end if
    a[1] = mod(a[1]+b[1],a[2])
    return a
end function

function mi_mul(mii a, mii b)
    if atom(a) then
        if not atom(b) then throw("error") end if
        return a*b
    end if
    if a[2]!=b[2] then throw("error") end if
    a[1] = mod(a[1]*b[1],a[2])
    return a
end function

function mi_power(mii x, integer p)
    mii res = mi_one(x)
    for i=1 to p do
        res = mi_mul(res,x)
    end for
    return res
end function

function mi_print(mii m)
    return sprintf(iff(atom(m)?"%g":"modint(%d,%d)"),m)
end function

function f(mii x)
    return mi_add(mi_power(x,100),mi_add(x,mi_one(x)))
end function

procedure test(mii x)
    printf(1,"x^100 + x + 1 for x == %s is %s\n",{mi_print(x),mi_print(f(x))})
end procedure
test(10)
test({10,13})
```

```txt

x^100 + x + 1 for x == 10 is 1e+100
x^100 + x + 1 for x == modint(10,13) is modint(1,13)

```



## Prolog

Works with SWI-Prolog versin 6.4.1 and module lambda (found there : http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl ).

```Prolog
:- use_module(library(lambda)).

congruence(Congruence,  In, Fun, Out) :-
	maplist(Congruence +\X^Y^(Y is X mod Congruence), In, In1),
	call(Fun, In1, Out1),
	maplist(Congruence +\X^Y^(Y is X mod Congruence), Out1, Out).

fun_1([X], [Y]) :-
	Y is X^100 + X + 1.

fun_2(L, [R]) :-
	sum_list(L, R).

```

```txt
 ?- congruence(13, [10], fun_1, R).
R = [1].

 ?- congruence(13, [10, 15, 13, 9, 22], fun_2, R).
R = [4].

 ?- congruence(13, [10, 15, 13, 9, 22], maplist(\X^Y^(Y is X * 13)), R).
R = [0,0,0,0,0].

```



## Python


We need to implement a Modulo type first, then give one of its instances to the "f" function.

Thanks to duck typing, the function doesn't need to care about the actual type it's given. We also use the dynamic nature of Python to dynamically build the operator overload methods and avoid repeating very similar code.


```Python
import operator
import functools

@functools.total_ordering
class Mod:
    __slots__ = ['val','mod']

    def __init__(self, val, mod):
        if not isinstance(val, int):
            raise ValueError('Value must be integer')
        if not isinstance(mod, int) or mod<=0:
            raise ValueError('Modulo must be positive integer')
        self.val = val % mod
        self.mod = mod

    def __repr__(self):
        return 'Mod({}, {})'.format(self.val, self.mod)

    def __int__(self):
        return self.val

    def __eq__(self, other):
        if isinstance(other, Mod):
            if self.mod == other.mod:
                return self.val==other.val
            else:
                return NotImplemented
        elif isinstance(other, int):
            return self.val == other
        else:
            return NotImplemented

    def __lt__(self, other):
        if isinstance(other, Mod):
            if self.mod == other.mod:
                return self.val<other.val
            else:
                return NotImplemented
        elif isinstance(other, int):
            return self.val < other
        else:
            return NotImplemented

    def _check_operand(self, other):
        if not isinstance(other, (int, Mod)):
            raise TypeError('Only integer and Mod operands are supported')
        if isinstance(other, Mod) and self.mod != other.mod:
            raise ValueError('Inconsistent modulus: {} vs. {}'.format(self.mod, other.mod))

    def __pow__(self, other):
        self._check_operand(other)
        # We use the built-in modular exponentiation function, this way we can avoid working with huge numbers.
        return Mod(pow(self.val, int(other), self.mod), self.mod)

    def __neg__(self):
        return Mod(self.mod - self.val, self.mod)

    def __pos__(self):
        return self # The unary plus operator does nothing.

    def __abs__(self):
        return self # The value is always kept non-negative, so the abs function should do nothing.

# Helper functions to build common operands based on a template.
# They need to be implemented as functions for the closures to work properly.
def _make_op(opname):
    op_fun = getattr(operator, opname)  # Fetch the operator by name from the operator module
    def op(self, other):
        self._check_operand(other)
        return Mod(op_fun(self.val, int(other)) % self.mod, self.mod)
    return op

def _make_reflected_op(opname):
    op_fun = getattr(operator, opname)
    def op(self, other):
        self._check_operand(other)
        return Mod(op_fun(int(other), self.val) % self.mod, self.mod)
    return op

# Build the actual operator overload methods based on the template.
for opname, reflected_opname in [('__add__', '__radd__'), ('__sub__', '__rsub__'), ('__mul__', '__rmul__')]:
    setattr(Mod, opname, _make_op(opname))
    setattr(Mod, reflected_opname, _make_reflected_op(opname))

def f(x):
    return x**100+x+1

print(f(Mod(10,13)))
# Output: Mod(1, 13)
```



## Racket


```racket
#lang racket
(require racket/require
         ;; grab all "mod*" names, but get them without the "mod", so
         ;; `+' and `expt' is actually `mod+' and `modexpt'
         (filtered-in (λ(n) (and (regexp-match? #rx"^mod" n)
                                 (regexp-replace #rx"^mod" n "")))
                      math)
         (only-in math with-modulus))
(define (f x) (+ (expt x 100) x 1))
(with-modulus 13 (f 10))
;; => 1
```



## Ruby


```ruby
# stripped version of Andrea Fazzi's submission to Ruby Quiz #179

class Modulo
  include Comparable

  def initialize(n = 0, m = 13)
    @n, @m = n % m, m
  end

  def to_i
    @n
  end

  def <=>(other_n)
    @n <=> other_n.to_i
  end

  [:+, :-, :*, :**].each do |meth|
    define_method(meth) { |other_n| Modulo.new(@n.send(meth, other_n.to_i), @m) }
  end

  def coerce(numeric)
    [numeric, @n]
  end

end

# Demo
x, y = Modulo.new(10), Modulo.new(20)

p x > y          # true
p x == y         # false
p [x,y].sort     #[#<Modulo:0x000000012ae0f8 @n=7, @m=13>, #<Modulo:0x000000012ae148 @n=10, @m=13>]
p x + y          ##<Modulo:0x0000000117e110 @n=4, @m=13>
p 2 + y          # 9
p y + 2          ##<Modulo:0x00000000ad1d30 @n=9, @m=13>

p x**100 + x +1  ##<Modulo:0x00000000ad1998 @n=1, @m=13>

```



## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/jOSxPQu/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/ZSeJw1lBRZiHenoQ6coDdA Scastie (remote JVM)].

```Scala
object ModularArithmetic extends App {
  private val x = new ModInt(10, 13)
  private val y = f(x)

  private def f[T](x: Ring[T]) = (x ^ 100) + x + x.one

  private trait Ring[T] {
    def +(rhs: Ring[T]): Ring[T]

    def *(rhs: Ring[T]): Ring[T]

    def one: Ring[T]

    def ^(p: Int): Ring[T] = {
      require(p >= 0, "p must be zero or greater")
      var pp = p
      var pwr = this.one
      while ( {
        pp -= 1;
        pp
      } >= 0) pwr = pwr * this
      pwr
    }
  }

  private class ModInt(var value: Int, var modulo: Int) extends Ring[ModInt] {
    def +(other: Ring[ModInt]): Ring[ModInt] = {
      require(other.isInstanceOf[ModInt], "Cannot add an unknown ring.")
      val rhs = other.asInstanceOf[ModInt]
      require(modulo == rhs.modulo, "Cannot add rings with different modulus")
      new ModInt((value + rhs.value) % modulo, modulo)
    }

    def *(other: Ring[ModInt]): Ring[ModInt] = {
      require(other.isInstanceOf[ModInt], "Cannot multiple an unknown ring.")
      val rhs = other.asInstanceOf[ModInt]
      require(modulo == rhs.modulo,
        "Cannot multiply rings with different modulus")
      new ModInt((value * rhs.value) % modulo, modulo)
    }

    override def one = new ModInt(1, modulo)

    override def toString: String = f"ModInt($value%d, $modulo%d)"
  }

  println("x ^ 100 + x + 1 for x = ModInt(10, 13) is " + y)

}
```


## Sidef

```ruby
class Modulo(n=0, m=13) {

  method init {
     (n, m) = (n % m, m)
  }

  method to_n { n }

  < + - * ** >.each { |meth|
      Modulo.def_method(meth, method(n2) { Modulo(n.(meth)(n2.to_n), m) })
  }

  method to_s { "#{n} 「mod #{m}」" }
}

func f(x) { x**100 + x + 1 }
say f(Modulo(10, 13))
```

```txt
1 「mod 13」
```



## Tcl

Tcl does not permit overriding of operators, but does not force an expression
to be evaluated as a ''standard'' expression.

Creating a parser and custom evaluation engine is relatively straight-forward,
as is shown here.
```tcl
package require Tcl 8.6
package require pt::pgen

###
### A simple expression parser for a subset of Tcl's expression language
###

# Define the grammar of expressions that we want to handle
set grammar {
PEG Calculator (Expression)
    Expression	<- Term (' '* AddOp ' '* Term)*			;
    Term	<- Factor (' '* MulOp ' '* Factor)*		;
    Fragment	<- '(' ' '* Expression ' '*  ')' / Number / Var	;
    Factor	<- Fragment (' '* PowOp ' '* Fragment)*		;
    Number	<- Sign? Digit+					;
    Var		<- '$' ( 'x'/'y'/'z' )				;

    Digit	<- '0'/'1'/'2'/'3'/'4'/'5'/'6'/'7'/'8'/'9'	;
    Sign	<- '-' / '+'					;
    MulOp	<- '*' / '/'					;
    AddOp	<- '+' / '-'					;
    PowOp	<- '**'						;
END;
}

# Instantiate the parser class
catch [pt::pgen peg $grammar snit -class Calculator -name Grammar]

# An engine that compiles an expression into Tcl code
oo::class create CompileAST {
    variable sourcecode opns
    constructor {semantics} {
	set opns $semantics
    }
    method compile {script} {
	# Instantiate the parser
	set c [Calculator]
	set sourcecode $script
	try {
	    return [my {*}[$c parset $script]]
	} finally {
	    $c destroy
	}
    }

    method Expression-Empty args {}
    method Expression-Compound {from to args} {
	foreach {o p} [list Expression-Empty {*}$args] {
	    set o [my {*}$o]; set p [my {*}$p]
	    set v [expr {$o ne "" ? "$o \[$v\] \[$p\]" : $p}]
	}
	return $v
    }
    forward Expression	my Expression-Compound
    forward Term	my Expression-Compound
    forward Factor	my Expression-Compound
    forward Fragment	my Expression-Compound

    method Expression-Operator {from to args} {
	list ${opns} [string range $sourcecode $from $to]
    }
    forward AddOp	my Expression-Operator
    forward MulOp	my Expression-Operator
    forward PowOp	my Expression-Operator

    method Number {from to args} {
	list ${opns} value [string range $sourcecode $from $to]
    }

    method Var {from to args} {
	list ${opns} variable [string range $sourcecode [expr {$from+1}] $to]
    }
}
```

None of the code above knows about modular arithmetic at all, or indeed about actual expression evaluation.
Now we define the semantics that we want to actually use.

```tcl
# The semantic evaluation engine; this is the part that knows mod arithmetic
oo::class create ModEval {
    variable mod
    constructor {modulo} {set mod $modulo}
    method value {literal} {return [expr {$literal}]}
    method variable {name} {return [expr {[set ::$name]}]}
    method + {a b} {return [expr {($a + $b) % $mod}]}
    method - {a b} {return [expr {($a - $b) % $mod}]}
    method * {a b} {return [expr {($a * $b) % $mod}]}
    method / {a b} {return [expr {($a / $b) % $mod}]}
    method ** {a b} {
	# Tcl supports bignums natively, so we use the naive version
	return [expr {($a ** $b) % $mod}]
    }
    export + - * / **
}

# Put all the pieces together
set comp [CompileAST new [ModEval create mod13 13]]
```

Finally, demonstrating…

```tcl
set compiled [$comp compile {$x**100 + $x + 1}]
set x 10
puts "[eval $compiled] = $compiled"
```

```txt

1 = ::mod13 + [::mod13 + [::mod13 ** [::mod13 variable x] [::mod13 value 100]] [::mod13 variable x]] [::mod13 value 1]

```



## VBA

```vb
Option Base 1
Private Function mi_one(ByVal a As Variant) As Variant
    If IsArray(a) Then
        a(1) = 1
    Else
        a = 1
    End If
    mi_one = a
End Function

Private Function mi_add(ByVal a As Variant, b As Variant) As Variant
    If IsArray(a) Then
        If IsArray(b) Then
             If a(2) <> b(2) Then
                mi_add = CVErr(2019)
            Else
                a(1) = (a(1) + b(1)) Mod a(2)
                mi_add = a
            End If
        Else
            mi_add = CVErr(2018)
        End If
    Else
        If IsArray(b) Then
            mi_add = CVErr(2018)
        Else
           a = a + b
           mi_add = a
        End If
    End If
End Function

Private Function mi_mul(ByVal a As Variant, b As Variant) As Variant
    If IsArray(a) Then
        If IsArray(b) Then
            If a(2) <> b(2) Then
                mi_mul = CVErr(2019)
            Else
                a(1) = (a(1) * b(1)) Mod a(2)
                mi_mul = a
            End If
        Else
            mi_mul = CVErr(2018)
        End If
    Else
        If IsArray(b) Then
            mi_mul = CVErr(2018)
        Else
            a = a * b
            mi_mul = a
        End If
    End If
End Function

Private Function mi_power(x As Variant, p As Integer) As Variant
    res = mi_one(x)
    For i = 1 To p
        res = mi_mul(res, x)
    Next i
    mi_power = res
End Function

Private Function mi_print(m As Variant) As Variant
    If IsArray(m) Then
        s = "modint(" & m(1) & "," & m(2) & ")"
    Else
        s = CStr(m)
    End If
    mi_print = s
End Function

Private Function f(x As Variant) As Variant
    f = mi_add(mi_power(x, 100), mi_add(x, mi_one(x)))
End Function

Private Sub test(x As Variant)
    Debug.Print "x^100 + x + 1 for x == " & mi_print(x) & " is " & mi_print(f(x))
End Sub
Public Sub main()
    test 10
    test [{10,13}]
End Sub
```
```txt
x^100 + x + 1 for x == 10 is 1E+100
x^100 + x + 1 for x == modint(10,13) is modint(1,13)
```



## zkl

Doing just enough to perform the task:

```zkl
class MC{
   fcn init(n,mod){ var N=n,M=mod; }
   fcn toString   { String(N.divr(M)[1],"M",M) }
   fcn pow(p)     { self( N.pow(p).divr(M)[1], M ) }
   fcn __opAdd(mc){
      if(mc.isType(Int)) z:=N+mc; else z:=N*M + mc.N*mc.M;
      self(z.divr(M)[1],M)
   }
}
```

Using GNU GMP lib to do the big math (to avoid writing a powmod function):

```zkl
var BN=Import("zklBigNum");
fcn f(n){ n.pow(100) + n + 1 }
f(1).println(" <-- 1^100 + 1 + 1");
n:=MC(BN(10),13);
(n+3).println(" <-- 10M13 + 3");
f(n).println(" <-- 10M13^100 + 10M13 + 1");
```

```txt

3 <-- 1^100 + 1 + 1
0M13 <-- 10M13 + 3
1M13 <-- 10M13^100 + 10M13 + 1

```

