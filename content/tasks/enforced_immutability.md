+++
title = "Enforced immutability"
description = ""
date = 2019-10-17T14:55:25Z
aliases = []
[extra]
id = 9077
[taxonomies]
categories = ["task", "Initialization"]
tags = []
languages = [
  "8th",
  "acl2",
  "ada",
  "algol_68",
  "autohotkey",
  "basic",
  "bbc_basic",
  "bracmat",
  "c",
  "c_shell",
  "clojure",
  "cobol",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "dyalect",
  "e",
  "ela",
  "elixir",
  "erlang",
  "euphoria",
  "factor",
  "fortran",
  "fsharp",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "logtalk",
  "mbs",
  "nemerle",
  "nim",
  "ocaml",
  "oforth",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powerbasic",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "seed7",
  "sidef",
  "supercollider",
  "swift",
  "tcl",
  "unix_shell",
  "xpl0",
  "zkl",
]
+++

## Task

Demonstrate any means your language has to prevent the modification of values, or to create objects that cannot be modified after they have been created.





## 8th

Items in 8th are constants if they are declared inside a word (function).  Otherwise, they are mutable, unless the "const" word is used:

```forth

123 const var, one-two-three

```

That declares that the number 123 is constant and may not be modified (not that the variable named 'one-two-three' is constant)

## ACL2

All variables in ACL2 are constants, with the exception of those accessed using <code>(assign ...)</code> and accessed using <code>(@ ...)</code>

To declare a global constant, use:


```Lisp
(defconst *pi-approx* 22/7)
```


Subsequent attempts to redefine the constant give an error:


```txt
ACL2 Error in ( DEFCONST *PI* ...):  The name *PI* is in use as a constant.
The redefinition feature is currently off.  See :DOC ld-redefinition-
action.
```



## Ada

Ada provides the <code>constant</code> keyword:

```Ada
Foo : constant := 42;
Foo : constant Blahtype := Blahvalue;
```

Types can be declared as limited: Objects of these types cannot be changed and also not compared nor copied:

```Ada
type T is limited private;  -- inner structure is hidden
X, Y: T;
B: Boolean;
-- The following operations do not exist:
X := Y;  -- illegal (cannot be compiled
B := X = Y;  -- illegal

```



## ALGOL 68

When a ''name'' is defined it can be identified as a constant value with an equality, eg pi = 355/113.
For a variable an assignment ":=" would be used instead, eg pi := 355/113;

```ALGOL 68
INT max allowed = 20;
REAL pi = 3.1415 9265;    # pi is constant that the compiler will enforce     #
REF REAL var = LOC REAL;  # var is a constant pointer to a local REAL address #
var := pi # constant pointer var has the REAL value referenced assigned pi    #
```



## AutoHotkey

It should be noted that Enforced immutability goes against the nature of AHK. However, it can be achieved using objects:

```AutoHotkey
MyData := new FinalBox("Immutable data")
MsgBox % "MyData.Data = " MyData.Data
MyData.Data := "This will fail to set"
MsgBox % "MyData.Data = " MyData.Data

Class FinalBox {
   __New(FinalValue) {
      ObjInsert(this, "proxy",{Data:FinalValue})
   }
; override the built-in methods:
   __Get(k) {
      return, this["proxy",k]
   }
   __Set(p*) {
      return
   }
   Insert(p*) {
      return
   }
   Remove(p*) {
      return
   }
}
```

You could still use ''ObjInsert/ObjRemove'' functions, since they are designed to bypass any custom behaviour implemented by the object. Also, technically you could still use the ''SetCapacity method'' to truncate the object, or the ''GetAddress method'' to modify the object using memory addresses.


## BASIC

Many BASICs support the <code>CONST</code> keyword:

```qbasic
CONST x = 1
```


Some flavors of BASIC support other methods of declaring constants. For example, [[FreeBASIC]] supports C-style defines:

```freebasic
#define x 1>
```



## BBC BASIC

BBC BASIC doesn't have named constants.  The closest you can get is to use a function:

```bbcbasic
      DEF FNconst = 2.71828182845905
      PRINT FNconst
      FNconst = 1.234 : REM Reports 'Syntax error'
```



## Bracmat

All values (expressions) in Bracmat are immutable, except those that contain <code>=</code> operators.

```bracmat
myVar=immutable (m=mutable) immutable;
changed:?(myVar.m);
lst$myVar

```

```txt
(myVar=
immutable (m=changed) immutable);
```



## C

You can create simple constants using the C preprocessor:

```c
#define PI      3.14159265358979323
#define MINSIZE 10
#define MAXSIZE 100
```


Alternatively, you can modify parameters and variables with the const keyword to make them immutable:

```c
const char   foo     = 'a';
const double pi      = 3.14159;
const double minsize = 10;
const double maxsize = 10;

// On pointers
const int *       ptrToConst;      // The value is constant, but the pointer may change.
int const *       ptrToConst;      // The value is constant, but the pointer may change. (Identical to the above.)
int       * const constPtr;        // The pointer is constant, but the value may change.
int const * const constPtrToConst; // Both the pointer and value are constant.

// On parameters
int main(const int    argc, // note that here, the "const", applied to the integer argument itself,
                            // is kind of pointless, as arguments are passed by value, so
                            // it does not affect any code outside of the function
         const char** argv)
{
    /* ... */
}
```


It is possible to remove the <tt>const</tt> qualifier of the type a pointer points to through a cast, but doing so will result in undefined behavior.


## C++


In addition to the examples shown in [[#C|C]], you can create a class whose instances contain instance-specific const members, by initializing them in the class's constructor.

```cpp
#include <iostream>

class MyOtherClass
{
public:
  const int m_x;
  MyOtherClass(const int initX = 0) : m_x(initX) { }

};

int main()
{
  MyOtherClass mocA, mocB(7);

  std::cout << mocA.m_x << std::endl; // displays 0, the default value given for MyOtherClass's constructor.
  std::cout << mocB.m_x << std::endl; // displays 7, the value we provided for the constructor for mocB.

  // Uncomment this, and the compile will fail; m_x is a const member.
  // mocB.m_x = 99;

  return 0;
}
```


You can also use the const keyword on methods to indicate that they can be applied to immutable objects:

```cpp
class MyClass
{
private:
    int x;

public:
    int getX() const
    {
        return x;
    }
};
```




## C#

Fields can be made read-only (a runtime constant) with the '''readonly''' keyword.

```csharp
readonly DateTime now = DateTime.Now;
```

When used on reference types, it just means the reference cannot be reassigned. It does not make the object itself immutable.<br/>
Primitive types can be declared as a compile-time constant with the '''const''' keyword.

```csharp
const int Max = 100;
```


Parameters can be made readonly by preceding them with the '''in''' keyword. Again, when used on reference types, it just means the reference cannot be reassigned.

```c#
public void Method(in int x) {
    x = 5; //Compile error
}
```


Local variables of primitive types can be declared as a compile-time constant with the '''const''' keyword.

```c#
public void Method() {
    const double sqrt5 = 2.236;
    ...
}
```


To make a type immutable, the programmer must write it in such a way that mutation is not possible. One important way to this is to use readonly properties. By not providing a setter, the property can only be assigned within the constructor.

```c#
public string Key { get; }
```

On value types (which usually should be immutable from a design perspective), immutability can be enforced by applying the '''readonly''' modifier on the type. It will fail to compile if it contains any members that are not read-only.

```c#

public readonly struct Point
{
    public Point(int x, int y) => (X, Y) = (x, y);

    public int X { get; }
    public int Y { get; }
}
```



## Clojure

Everything in Clojure except for Java interop are immutable.


```clojure
user
 (def d [1 2 3 4 5]) ; immutable vector
#'user/d
user> (assoc d 3 7)
[1 2 3 7 5]
user> d
[1 2 3 4 5]
```



## COBOL

Constants in COBOL are not stored in memory, but are closer to C's macros, by associating a literal with a name.
Prior to COBOL 2002, you could define figurative literals for characters only:

```cobol
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SPECIAL-NAMES.
    SYMBOLIC CHARACTERS NUL IS 0, TAB IS 9.
```


A new syntax was introduced in COBOL 2002 which allowed defining constants for other types.

```cobol
01  Foo CONSTANT AS "Foo".
```


Prior to COBOL 2002, there were non-standard extensions available that also implemented constants. One extension was the the 78 level-number:

```cobol
78  Foo VALUE "Foo".
```

Another was the <code>CONSTANT SECTION</code>:

```cobol
CONSTANT SECTION.
01  Foo VALUE "Foo".
```



## D


```d
import std.random;

// enum allows to define manifest (compile-time) constants:
int sqr(int x) { return x ^^ 2; }
enum int x = 5;
enum y = sqr(5); // Forces Compile-Time Function Evaluation (CTFE).

// enums are compile-time constants:
enum MyEnum { A, B, C }


// immutable defines values that can't change:
immutable double pi = 3.1415;


// A module-level immutable storage class variable that's not
// explicitly initialized can be initialized by its constructor,
// otherwise its value is the default initializer during its life-time.

immutable int z;

static this() {
    z = uniform(0, 100); // Run-time initialization.
}

class Test1 {
    immutable int w;

    this() {
        w = uniform(0, 100); // Run-time initialization.
    }
}


// The items array can't be immutable here.
// "in" is short for "const scope":
void foo(const scope int[] items) {
    // items is constant here.
    // items[0] = 100; // Cannot modify const expression.
}


struct Test2 {
    int x_; // Mutable.
    @property int x() { return this.x_; }
}

// Unlike C++, D const and immutable are transitive.
// And there is also "inout". See D docs.

void main() {
    int[] data = [10, 20, 30];
    foo(data);
    data[0] = 100; // But data is mutable here.

    // Currently manifest constants like arrays and associative arrays
    // are copied in-place every time they are used:
    enum array = [1, 2, 3];
    foo(array);

    auto t = Test2(100);
    auto x2 = t.x; // Reading x is allowed.
    assert(x2 == 100);

    // Not allowed, the setter property is missing:
    // t.x = 10; // Error: not a property t.x
}
```



## Delphi


Typed constants can be assigned to using the {$WRITABLECONST ON} or {J+} compiler directives (off by default).

```Delphi
const
  STR1 = 'abc';         // regular constant
  STR2: string = 'def'; // typed constant
```



## Dyalect


Dyalect supports creation of constants using "const" keyword:


```dyalect
const pi = 3.14
const helloWorld = "Hello, world!"
```


A constant can contain a value of any type:


```dyalect
const sequence = [1,2,3,4,5]
```



## E


Whether an object can be modified is entirely up to whether the object provides methods for mutation — objects cannot be affected except by using their methods. It is conventional in E to provide immutable objects when it is natural to do so (e.g. immutable and mutable collections).

Variables are immutable unless declared with the '<code>var</code>' keyword.


```e
def x := 1

x := 2  # this is an error
```


Below the surface, each variable name is bound to a Slot object, which can be thought of as a one-element collection. If the var keyword is used, then the slot object is mutable; else, immutable. It is never possible to change the slot a name is bound to.

Any object which is immutable and contains no immutable parts has the property DeepFrozen.


```e
var y := 1

def things :DeepFrozen := [&x, 2, 3]  # This is OK

def funnyThings :DeepFrozen := [&y, 2, 3]  # Error: y's slot is not immutable
```


(The unary <code>&</code> operator gets the slot of a variable, and can be thought of almost exactly like C's <code>&</code>.)


## Ela


Normally there is no need to enforce immutability in Ela - everything is immutable by default. Ela doesn't support mutable variables like imperative languages.  All built-in data structures are immutable as well. The only way to create a mutable data structure is to use an unsafe module "cell", that implements reference cells in Ocaml style:


```ela
open unsafe.cell
r = ref 0
```


Function mutate can be used to mutate a reference cell:


```ela
mutate r 1>
```


In order to unwrap a value from a cell one can use a valueof function:


```ela
valueof r>
```



## Elixir

Elixir data is immutable.

Elixir allows variables to be rebound via static single assignment:

```txt

iex(1)> x = 10          # bind
10
iex(2)> 10 = x          # Pattern matching
10
iex(3)> x = 20          # rebound
20
iex(4)> ^x = 10         # pin operator ^
** (MatchError) no match of right hand side value: 10

```



## Erlang


Erlang variables are immutable by nature. The following would be an error:

```erlang
X = 10,
X = 20.
```


However, since = actually performs pattern matching, the following is permissible:

```erlang
X = 10,
X = 10.
```



## Euphoria


```euphoria
constant n = 1
constant s = {1,2,3}
constant str = "immutable string"
```



## Fortran

In type declaration statements a PARAMETER attribute can be specified turning the data object into a named constant.

```fortran
real, parameter :: pi = 3.141593
```

Dummy arguments of procedures can be given an INTENT attribute. An argument with INTENT(IN) cannot be changed by the procedure

```Fortran
subroutine sub1(n)
  real, intent(in) :: n
```



## F#

As a functional language, everything in F# is immutable by default. Interestingly, <code>const</code> is a reserved word but is non-functional.

```fsharp
let hello = "Hello!"
```



## Factor

Tuple slots may be declared <code>read-only</code>. For example, the <code>range</code> tuple declares its slots <code>read-only</code>:

```factor
TUPLE: range
    { from read-only } { length read-only } { step read-only } ;
```


Note that the <code>CONSTANT:</code> word does nothing to enforce immutability on the object it places on the stack, as it is functionally equivalent to a standard word definition with stack effect <code>( -- obj )</code>.


## Go

Strings in Go are immutable.  Attempts to modify them fail to compile:

```go
package main

func main() {
    s := "immutable"
    s[0] = 'a'
}
```


```txt

test.go:5: cannot assign to s[0]

```

Go has const declarations, but they concern compile-time expression evaluation, and not run-time immutability.


## Haskell

Since Haskell is purely functional everything is immutable by default.

```haskell
pi  = 3.14159
msg = "Hello World"
```


=={{header|Icon}} and {{header|Unicon}}==
In Icon and Unicon pretty much everything can be changed. There really isn't an easy way to protect a variable from being changed.  There are compile time constants created by ''$define'' (as shown); although, they can be explicitly undefined.  String values themselves are immutable; however, manipulating them creates new string values.  The effect is that the value assigned to a variable will change even though the value itself won't.  For more see [[Icon%2BUnicon/Intro#Mutable_and_Immutable_Types|Mutable and Immutable Types]].


```Icon
$define "1234"
```



## J


In J's values are immutable. Values external to J may be mutable - this is sometimes significant since J can have references to external values, which we use here with for <code>C</code>. The trick is that when a J variable name refers to an external resource, that association is necessarily tied to the name.

The values associated with a J name can be modified, but that is a modification of the association, and the original value remains.

Note that J has a rich language for defining numeric constants. For example, 2*pi represented as a floating point number would be 2p1.


```j
  B=: A=: 'this is a test'
  A=: '*' 2 3 5 7} A
   A
th** *s*a test
   B
this is a test
```


Names can also be made constant (that is, have their referent fixed), so that name, value, and association between name and value are immutable:
```j
   C=: 'this is a test'
   1 readonly_jmf_ 'C'

   C =: 'some new value'
|read-only data
|   C    =:'some new value'
   C
this is a test
```



## Java


Variables in Java can be made immutable by using the <code>final</code> modifier (works on any type, primitive or reference):

```java
final int immutableInt = 4;
int mutableInt = 4;
mutableInt = 6; //this is fine
immutableInt = 6; //this is an error
```


Using final on a reference type means the reference cannot be reassigned, but does not necessarily mean that the object that it points to can't be changed:

```java
final String immutableString = "test";
immutableString = new String("anotherTest"); //this is an error
final StringBuffer immutableBuffer = new StringBuffer();
immutableBuffer.append("a"); //this is fine and it changes the state of the object
immutableBuffer = new StringBuffer("a"); //this is an error
```


Whether an object can be modified is entirely up to whether the object provides either methods or non-final public/protected fields for mutation. Objects can be made immutable (in a sense that is more appropriate for this task) by making all fields <code>final</code> or <code>private</code>, and making sure that no methods modify the fields:

```java
public class Immute{
    private final int num;
    private final String word;
    private final StringBuffer buff; //still mutable inside this class, but there is no access outside this class

    public Immute(int num){
        this.num = num;
        word = num + "";
        buff = new StringBuffer("test" + word);
    }

    public int getNum(){
        return num;
    }

    public String getWord(){
        return word; //String objects are immutable so passing the object back directly won't harm anything
    }

    public StringBuffer getBuff(){
        return new StringBuffer(buff);
        //using "return buff" here compromises immutability, but copying the object via the constructor makes it ok
    }
    //no "set" methods are given
}
```

In the <code>Immute</code> class above, the object pointed to by "buff" is still technically mutable, since its internal values can still be changed. The <code>private</code> modifier ensures that no other classes can access that variable. Some trickery needed to be done to ensure that no pointers to the actual mutable objects are passed out. Programmers should be aware of which objects that they use are mutable (usually noted in javadocs).

The [http://download.oracle.com/javase/6/docs/api/java/util/Collections.html Collections class] also has methods that will create "unmodifiable" <code>Collection</code>s out of existing <code>Collection</code>s instances.


## JavaScript

You can create constants with the [https://developer.mozilla.org/en/JavaScript/Reference/Statements/const Mozilla-specific extension const]. This is not supported by IE and it only works on simple scalars and not on arrays, objects, or parameters.

'''Update''': const is now a standard part of ES6 JavaScript, and works with all data types, including arrays, objects, and parameters. It is not, however, included in the ES5 standard.

```javascript
const pi = 3.1415;
const msg = "Hello World";
```



## jq

All values in jq are immutable.  Sometimes the syntax may make it appear as though a value is being altered, but that is never the case.  For example, consider the following pipeline:
```jq

["a", "b"] as $a | $a[0] = 1 as $b | $a
```


Here, the result is ["a", "b"].


## Julia

```julia
const x = 1
x = π # ERROR: invalid ridefinition of constant x
```



## Kotlin

Properties and local variables in Kotlin can be made read-only by declaring them with the 'val' keyword rather than the 'var' keyword.

Parameters to functions are always read-only. If you want to change them you need to assign them to a local 'var' variable.

Apart from primitive (Int, Double, Char etc.) or String types, being read-only doesn't necessarily mean that the object to which the variable/property/parameter refers is itself immutable - it only means that the reference to that object cannot be re-assigned. Whether or not the object itself is immutable depends on how it is defined i.e. whether it is possible to mutate or override its properties.

Top level or object properties can also be marked as compile-time constants provided they are declared with the 'const val' modifier, are of primitive or string type and are initialized accordingly. These, of course, are truly immutable.

A distinction is made in Kotlin's standard library between mutable and immutable collections. The size and content of the latter cannot be changed once initialized though, if an immutable collection contains reference types, this doesn't necessarily mean that such objects are immutable for the reasons described earlier.

Here are some examples:

```scala
// version 1.1.0

//  constant top level property
const val N = 5

//  read-only top level property
val letters = listOf('A', 'B', 'C', 'D', 'E') // 'listOf' creates here a List<Char) which is immutable

class MyClass {  // MyClass is effectively immutable because it's only property is read-only
                 // and it is not 'open' so cannot be sub-classed
    // read-only class property
    val myInt = 3

    fun myFunc(p: Int) {  // parameter 'p' is read-only
        var pp = p        // local variable 'pp' is mutable
        while (pp < N) {  // compiler will change 'N' to 5
            print(letters[pp++])
        }
        println()
    }
}

fun main(args: Array<String>) {
    val mc = MyClass()   // 'mc' cannot be re-assigned a different object
    println(mc.myInt)
    mc.myFunc(0)
}
```


```txt

3
ABCDE

```



## Logtalk

Logtalk supports both static and dynamic objects. Static objects are usually defined in source files. Object predicates are static by default. These objects can be defined locked against runtime modifications. For simplicity, the following example uses a prototype:

```logtalk

:- object(immutable).

    % forbid using (complementing) categories for adding to or
    % modifying (aka hot patching) the object
    :- set_logtalk_flag(complements, deny).
    % forbid dynamically adding new predicates at runtime
    :- set_logtalk_flag(dynamic_declarations, deny).

    :- public(foo/1).
    foo(1).       % static predicate by default

    :- private(bar/2)
    bar(2, 3).    % static predicate by default

:- end_object.

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Tau = 2*Pi;Protect[Tau]
{"Tau"}

Tau = 2
->Set::wrsym: Symbol Tau is Protected.
```



## MBS



```mbs
CONSTANT INT foo=640;
```



## Nemerle

Everything is immutable by default.

```Nemerle
def foo = 42;              // immutable by default
mutable bar = "O'Malleys"; // mutable because you asked it to be
```



## Nim


```nim
var x = "mutablefoo" # Mutable variable
let y = "immutablefoo" # Immutable variable, at runtime
const z = "constantfoo" # Immutable constant, at compile time

x[0] = 'M'
y[0] = 'I' # Compile error: 'y[0]' cannot be assigned to
z[0] = 'C' # Compile error: 'z[0]' cannot be assigned to
```



## OCaml


By default integers, floats, characters, booleans are immutable.
Tuples, lists and variants are also immutable as long as they only contain immutable elements.
Records are immutable as long as none of its elements are declared with the keyword <code>"mutable"</code> and also as long as none of its fields contain a mutable element (an array or a string for example).

Objects are immutable as long as none of its variables are declared with the keyword <code>"mutable"</code> or is a mutable type (an array or a string for example).

Arrays and strings are mutable.

In order to use immutable strings or immutable arrays, we would create new modules and aliasing the functions for creating and access, but not those for modifying. Here is below an example of this.

File <code>ImString.mli</code> containing the interface:


```ocaml
type im_string

val create : int -> im_string
val make : int -> char -> im_string
val of_string : string -> im_string
val to_string : im_string -> string
val copy : im_string -> im_string
val sub : im_string -> int -> int -> im_string
val length : im_string -> int
val get : im_string -> int -> char
val iter : (char -> unit) -> im_string -> unit
val escaped : im_string -> im_string
val index : im_string -> char -> int
val contains : im_string -> char -> bool
val print : im_string -> unit
```


File <code>ImString.ml</code> containing the "implementation":


```ocaml
type im_string = string

let create   = String.create
let make     = String.make
let copy     = String.copy
let sub      = String.sub
let length   = String.length
let get      = String.get
let iter     = String.iter
let escaped  = String.escaped
let index    = String.index
let contains = String.contains

let of_string s = s
let to_string s = s
let print = print_string
```


Here we can see that in the implementation the new type for immutable strings is defined with <code>type im_string = string</code>, and the definition of this type is hidden in the interface with <code>type im_string</code>.



## Oforth


Immutability is the default behaviour and Oforth uses immutability to limit side effects.

There is nothing global and mutable like global variables, class attributes, ...

Global objects are :

- Words, which are immutable objects (classes, functions, methods, ...).

- Constants, which values are immutable.

Functions or methods have only access to its parameters, to the data stack and to global immutable objects : they can't update something global used by another function or another task.

Oforth allows mutable objects but they remain local to a task and are not visible by other tasks (there is no need to synchronise tasks). Channels are the only way for tasks to communicate and mutable objects can't be sent into a channel.

For user defined classes, if an attribute is immutable, its value can be set only during initialization and only with an immutable object.

All these rules are checked at runtime and exceptions are raised if a piece of code breaks those immutability rules.


```Oforth
Object Class new: MyClass(a, b)

MyClass method: setA(value)  value := a ;
MyClass method: setB(value)  value := b ;

MyClass method: initialize(v, w)  self setA(v) self setB(w) ;

MyClass new(1, 2)                // OK : An immutable object
MyClass new(1, 2) setA(4)        // KO : An immutable object can't be updated after initialization
MyClass new(ListBuffer new, 12)  // KO : Not an immutable value.
ListBuffer new Constant new: T   // KO : A constant cannot be mutable.
Channel new send(ListBuffer new) // KO : A mutable object can't be sent into a channel.
```



## PARI/GP

GP cannot enforce immutability on its functions or variables.  PARI can do so through the usual [[#C|C]] methods.


## Pascal

See [[Enforced_immutability#Delphi | Delphi]]


## Perl

The constant pragma allows you to create subroutines that always return the same value and that cannot be modified:

```perl
use constant PI =
 3.14159;
use constant MSG => "Hello World";
```


The module Readonly.pm provides a means of enforcing immutablity upon scalars and arrays, however, this imposes a considerable performance penalty:


```perl
use Readonly;

Readonly::Scalar my $pi => 3.14159;
Readonly::Scalar my $msg => "Hello World";

Readonly::Array my @arr => (1, 2, 3, 4, 5);
Readonly::Hash my %hash => (
    "a" => 1,
    "b" => 2,
    "c" => 3
);
```



## Perl 6

You can create constants in Perl 6 with constant:

```perl6
constant $pi = 3.14159;
constant $msg = "Hello World";

constant @arr = (1, 2, 3, 4, 5);
```


Immutability is abstract enough that you can define an infinite constant lazily:

```perl6
constant fibonacci = 0, 1, *+* ... *;
```


Variables are considered mutable by default, but may be marked as readonly after initialization:

```perl6
my $pi := 3 + rand;
```

Unlike variables, formal parameters are considered readonly by default even if bound to a mutable container.

```perl6
sub sum (Num $x, Num $y) {
	$x += $y;  # ERROR
}

# Explicitly ask for pass-by-reference semantics
sub addto (Num $x is rw, Num $y) {
    $x += $y;  # ok, propagated back to caller
}

# Explicitly ask for pass-by-value semantics
sub sum (Num $x is copy, Num $y) {
    $x += $y;  # ok, but NOT propagated back to caller
    $x;
}
```

A number of built-in types are considered immutable value types, including:

```txt
Str         Perl string (finite sequence of Unicode characters)
Int         Perl integer (allows Inf/NaN, arbitrary precision, etc.)
Num         Perl number (approximate Real, generally via floating point)
Rat         Perl rational (exact Real, limited denominator)
FatRat      Perl rational (unlimited precision in both parts)
Complex     Perl complex number
Bool        Perl boolean
Exception   Perl exception
Block       Executable objects that have lexical scopes
Seq         A list of values (can be generated lazily)
Range       A pair of Ordered endpoints
Set         Unordered collection of values that allows no duplicates
Bag         Unordered collection of values that allows duplicates
Enum        An immutable Pair
Map         A mapping of Enums with no duplicate keys
Signature   Function parameters (left-hand side of a binding)
Capture     Function call arguments (right-hand side of a binding)
Blob        An undifferentiated mass of ints, an immutable Buf
Instant     A point on the continuous atomic timeline
Duration    The difference between two Instants
```

These values, though objects, can't mutate; they may only be "changed" by modifying a mutable container holding one of them to hold a different value instead.  (In the abstract, that is.  In the interests of efficiency, a string or list implementation would be allowed to cheat as long as it doesn't get caught cheating.) Some of these types have corresponding "unboxed" native representations, where the container itself must carry the type information since the value can't.  In this case, it's still the container that might be
considered mutable as an lvalue location, not the value stored in that location.

By default, object attributes are not modifiable from outside a class, though this is usually viewed more as encapsulation than as mutability control.


## Phix


```Phix
constant n = 1
constant s = {1,2,3}
constant str = "immutable string"
```



## PHP

You can create constants using the define function. This only works with scalars.

```php
define("PI", 3.14159265358);
define("MSG", "Hello World");
```


Or: {{works with|PHP|5.3+}}

```php
const PI = 3.14159265358;
const MSG = "Hello World";
```


http://us.php.net/manual/en/language.constants.syntax.php


## PicoLisp

In PicoLisp it is a central design issue that the programmer is in control of everything, and thus can modify any value. Even program parts written in C or assembly can be changed on the fly.
The nearest thing would be to define a function, e.g.

```PicoLisp
: (de pi () 4)
-> pi

: (pi)
-> 4
```

but even this could be modified, e.g.:

```PicoLisp
: (set (cdr pi) 3)
-> 3

: (pi)
-> 3
```



## PL/I

PL/I supports Named Constants. This avoids the default data attributes used when writing ''simple'' constants (such as 3).

```pli
*process source attributes xref;
 constants: Proc Options(main);
 Dcl three Bin Fixed(15) Value(3);
 Put Skip List(1/three);
 Put Skip List(1/3);
 End;
```

```txt
    0.33333332
  0.33333333333333
```



## PowerBASIC

Constants are declared by prefacing the variable name with <code>$</code> for strings and <code>%</code> for numeric variables:

```powerbasic
$me = "myname"
%age = 35
```



## PureBasic

PureBasic does not natively use immutable variables, only constants.

```PureBasic
#i_Const1 = 11
#i_Const2 = 3.1415
#i_Const3 = "A'm a string"
```


However using an OO approach, PureBasic allows for creation of new variable classes such as immutable ones.

```PureBasic
;Enforced immutability Variable-Class

Interface PBVariable    ; Interface for any value of this type
  Get()         ; Get the current value
  Set(Value.i)  ; Set (if allowed) a new value in this variable
  ToString.s()  ; Transferee the value to a string.
  Destroy()     ; Destructor
EndInterface

Structure PBV_Structure ; The *VTable structure
  Get.i
  Set.i
  ToString.i
  Destroy.i
EndStructure

Structure PBVar
  *VirtualTable.PBV_Structure
  Value.i
EndStructure

;- Functions for any PBVariable
Procedure immutable_get(*Self.PBVar)
  ProcedureReturn *Self\Value
EndProcedure

Procedure immutable_set(*Self.PBVar, N.i)
  ProcedureReturn #False
EndProcedure

Procedure.s immutable_ToString(*Self.PBVar)
  ProcedureReturn Str(*Self\Value)
EndProcedure

Procedure DestroyImmutabe(*Self.PBVar)
  FreeMemory(*Self)
EndProcedure

;- Init an OO-Table
DataSection
  VTable:
  Data.i @immutable_get()
  Data.i @immutable_set()
  Data.i @immutable_ToString()
  Data.i @DestroyImmutabe()
EndDataSection

;- Create-Class
Procedure CreateImmutabe(Init.i=0)
  Define *p.PBVar
  *p=AllocateMemory(SizeOf(PBVar))
  *p\VirtualTable = ?VTable
  *p\Value = Init
  ProcedureReturn *p
EndProcedure

;- **************
;- Test the Code

;- Initiate two Immutabe variables
*v1.PBVariable = CreateImmutabe()
*v2.PBVariable = CreateImmutabe(24)

;- Present therir content
Debug *v1\ToString() ; = 0
Debug *v2\ToString() ; = 24

;- Try to change the variables
*v1\Set(314)  ; Try to change the value, which is not permitted
*v2\Set(7)

; Present the values again
Debug Str(*v1\Get()) ; = 0
Debug Str(*v2\Get()) ; = 24

;- And clean up
*v1\Destroy()
*v2\Destroy()
```



## Python

Some datatypes such as strings are immutable:

```python
>>
 s = "Hello"
>>> s[0] = "h"

Traceback (most recent call last):
  File "<pyshell#1>", line 1, in <module>
    s[0] = "h"
TypeError: 'str' object does not support item assignment
```


While classes are generally mutable, you can define immutability by overriding __setattr__:

```python
>>
 class Immut(object):
	def __setattr__(self, *args):
		raise TypeError(
			"'Immut' object does not support item assignment")

        __delattr__ = __setattr__

        def __repr__(self):
		return str(self.value)

        def __init__(self, value):
                # assign to the un-assignable the hard way.
		super(Immut, self).__setattr__("value", value)

>>> im = Immut(123)
>>> im
123
>>> im.value = 124

Traceback (most recent call last):
  File "<pyshell#27>", line 1, in <module>
    del a.value
  File "<pyshell#23>", line 4, in __setattr__
    "'Immut' object does not support item assignment")
TypeError: 'Immut' object does not support item assignment
>>>
```


## Racket


Racket supports many kinds immutable values:

* The default <tt>cons</tt> cell pairs are immutable.

* Many primitive mutable types have an immutable variant.  Examples are strings, byte-strings, vectors, hash tables and even boxes.  Note that immutable hash-tables are implemented as balanced trees, making it a good representation for a functional dictionary.

In addition, new type definitions using <tt>struct</tt> are immutable by default:

```Racket
(struct coordinate (x y)) ; immutable struct
```

mutable struct definitions need to explicitly use a <tt>#:mutable</tt>, keyword next to a field to specify it as mutable, or as an option to the whole struct to make all fields mutable.


## REXX

Programming note:   The REXX language doesn't have immutable variables as such, but the method can be emulated with a simple subroutine.   Immutable variables are set via a REXX subroutine which makes a shadow copy of the variable.   Later, the same subroutine can be invoked (mutiple times if wanted) to check if any immutable variables have been altered (compromised).   Three REXX variables are preempted: '''immutable.''', '''_''', and '''__'''   (the last two are just used for temporary variables and any unused variable names can be used).

```rexx
/*REXX program  emulates  immutable variables  (as a post-computational check).         */
call immutable '$=1'                             /* ◄─── assigns an immutable variable. */
call immutable '   pi = 3.14159'                 /* ◄───    "     "     "         "     */
call immutable 'radius= 2*pi/4 '                 /* ◄───    "     "     "         "     */
call immutable '     r=13/2    '                 /* ◄───    "     "     "         "     */
call immutable '     d=0002 * r'                 /* ◄───    "     "     "         "     */
call immutable ' f.1  = 12**2  '                 /* ◄───    "     "     "         "     */

say '       $ ='  $                              /*show the variable, just to be sure.  */
say '      pi ='  pi                             /*  "   "      "       "   "  "   "    */
say '  radius ='  radius                         /*  "   "      "       "   "  "   "    */
say '       r ='  r                              /*  "   "      "       "   "  "   "    */
say '       d ='  d                              /*  "   "      "       "   "  "   "    */

                    do radius=10  to  -10  by -1 /*perform some faux important stuff.   */
                    circum=$*pi*2*radius         /*some kind of impressive calculation. */
                    end   /*k*/                  /* [↑]  that should do it, by gum.     */
call immutable                                   /* ◄═══ see if immutable variables OK. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
immutable: if symbol('immutable.0')=='LIT'  then immutable.0= /*1st time see immutable? */
           if arg()==0 then do                                /* [↓]  chk all immutables*/
                              do __=1  for words(immutable.0); _=word(immutable.0,__)
                              if value(_)==value('IMMUTABLE.!'_)  then iterate   /*same?*/
                              call ser -12, 'immutable variable  ' _ "  compromised."
                              end   /*__*/                  /* [↑]  Error?  ERRmsg, exit*/
                            return 0                        /*return and indicate  A-OK.*/
                            end                             /* [↓] immutable must have =*/
           if pos('=',arg(1))==0  then call ser -4, "no equal sign in assignment:"  arg(1)
           parse arg _ '=' __;         upper _;    _=space(_)    /*purify variable name.*/
           if symbol("_")=='BAD'  then call ser -8,_ "isn't a valid variable symbol."
           immutable.0=immutable.0 _                        /*add immutable var to list.*/
           interpret '__='__;     call value _,__           /*assign value to a variable*/
           call value 'IMMUTABLE.!'_,__                     /*assign value to bkup var. */
           return words(immutable.0)                        /*return number immutables. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
ser:       say;     say '***error***'  arg(2);     say;     exit arg(1)     /*error msg.*/
```

'''output'''

```txt

       $ = 1
      pi = 3.14159
  radius = 1.570795
       r = 6.5
       d = 13.0

***error*** immutable variable   RADIUS   compromised.

```



## Ring


```ring

# Project : Enforced immutability

x = 10
assert( x = 10)
assert( x = 100 )

```

Output:

```txt

Line 8 Assertion Failed!

```



## Ruby

You can make things immutable at run-time with Ruby using the built-in Object#freeze method:

```ruby
msg = "Hello World"
msg << "!"
puts msg                #=> Hello World!

puts msg.frozen?        #=> false
msg.freeze
puts msg.frozen?        #=> true
begin
  msg << "!"
rescue => e
  p e                   #=> #<RuntimeError: can't modify frozen String>
end

puts msg                #=> Hello World!
msg2 = msg

# The object is frozen, not the variable.
msg = "hello world"     # A new object was assigned to the variable.

puts msg.frozen?        #=> false
puts msg2.frozen?       #=> true
```

Since Ruby version 2.1 freezing strings can give a performance boost.
There is no way to unfreeze a frozen object.
The freeze can not be canceled but the object of approximately the same contents not to freeze up can be gotten if using Object#dup.

```ruby
# There are two methods in the copy of the object.
msg = "Hello World!".freeze
msg2 = msg.clone        # Copies the frozen and tainted state of obj.
msg3 = msg.dup          # It doesn't copy the status (frozen, tainted) of obj.
puts msg2               #=> Hello World!
puts msg3               #=> Hello World!
puts msg2.frozen?       #=> true
puts msg3.frozen?       #=> false
```



## Rust


Rust <tt>let</tt> bindings are immutable by default. This will raise a compiler error:


```rust
let x = 3;
x += 2;
```


You must declare a variable mutable explicitly:


```rust
let mut x = 3;
```

Similarly, references are immutable by default e.g.

```rust
let mut x = 4;
let y = &x;
*y += 2 // Raises compiler error. Even though x is mutable, y is an immutable reference.
let y = &mut x;
*y += 2// Works
// Note that though y is now a mutable reference, y itself is still immutable e.g.
let mut z = 5;
let y = &mut z; // Raises compiler error because y is already assigned to '&mut x'
```



## Scala


```scala
val pi = 3.14159
val msg = "Hello World"
```



## Seed7

Seed7 provides <code>const</code> definitons.
Constants can have any type:

```seed7
const integer: foo is 42;
const string: bar is "bar";
const blahtype: blah is blahvalue;
```

Constants can be initialized with expressions:

```seed7
const integer: foobar is 2 * length(bar) * (foo - 35);
```

Any function, even user defined functions can be used to initialize a constant:

```seed7
const func float: deg2rad (in float: degree) is  # User defined function
  return degree * PI / 180.0;

const float: rightAngle is deg2rad(90.0);
```

The initialisation expression is evaluated at compile-time.
It is possible to initialize a constant with data from the file system:

```seed7
const string: fileData is getf("some_file.txt");
```

The compiler can even get initialisation data from the internet:

```seed7
const string: unixDict is getHttp("www.puzzlers.org/pub/wordlists/unixdict.txt");
```

Types are also defined as constants (in other languages this is called a ''typedef''):

```seed7
const type: blahtype is integer;>
```

Function definitions (see above for the definition of ''deg2rad'') have also the form of a <code>const</code> definition.


## Sidef


```ruby
define PI = 3.14159;            # compile-time defined constant
const MSG = "Hello world!";     # run-time defined constant
```



## SuperCollider


```SuperCollider
// you can freeze any object.
b = [1, 2, 3];
b[1] = 100; // returns [1, 100, 3]
b.freeze; // make b immutable
b[1] = 2; // throws an error ("Attempted write to immutable object.")
```



## Swift

Swift has a notion of immutable values built into the language.

```swift
let a = 1
a = 1 // error: a is immutable
var b = 1
b = 1
```


It also extends this to higher level data structures. For example Swift has a notion of value types vs reference types.


```swift
/// Value types are denoted by `struct`s
struct Point {
  var x: Int
  var y: Int
}

let p = Point(x: 1, y: 1)
p.x = 2 // error, because Point is a value type with an immutable variable

/// Reference types are denoted by `class`s
class ClassPoint {
  var x: Int
  var y: Int

  init(x: Int, y: Int) {
    self.x = x
    self.y = y
  }
}

let pClass = ClassPoint(x: 1, y: 1)
pClass.x = 2 // Fine because reference types can be mutated, as long as you are not replacing the reference
```


Value types are always passed by value. This applies to collections in Swift.


```swift

// A common Swift beginner trap
func addToArray(_ arr: [Int]) {
  var arr = arr // Trying to modify arr directly is an error, parameters are immutable
  arr.append(2)
}

let array = [1]
addToArray(array)
print(array) // [1], because value types are pass by copy, array is immutable
```



## Tcl

Although there is no built-in support for constants, it is trivial to construct on top of Tcl's variable tracing facility:

```tcl
proc constant {varName {value ""}} {
    upvar 1 $varName var
    # Allow application of immutability to an existing variable, e.g., a procedure argument
    if {[llength [info frame 0]] == 2} {set value $var} else {set var $value}
    trace add variable var write [list apply {{val v1 v2 op} {
        upvar 1 $v1 var
        set var $val; # Restore to what it should be
        return -code error "immutable"
    }} $value]
}
```

Interactive demonstration:

```tcl
% constant pi 3.14159
% puts "pi=$pi"
pi=3.14159
% set pi 3; # Only in Indiana :-)
can't set "pi": immutable
% puts "pi is still $pi"
pi is still 3.14159
```



## UNIX Shell

The Unix shell does not support constants, but variables can be marked as readonly for the same effect.

```sh
PIE=APPLE
readonly PIE
```


=
## C Shell
=

```csh
set -r PIE = APPLE
```



## XPL0


```XPL0
define Pi=3.14;
Pi:= 3.15;      \causes a compile error: statement starting with a constant

```



## zkl

Mutability is up to each object. Strings, numbers are immutable. Lists can be either. Dictionaries can switch.

```zkl
List(1,2,3).del(0) //--> L(2,3)
ROList(1,2,3).del(0) //-->SyntaxError : Can't find del, which means you can't call it
d:=Dictionary(); d.add("one",1)
D(one:1)
d.makeReadOnly(); d.add("2",2)  //-->AccessError(This Dictionary is read only)
```



{{omit from|MUMPS}} <!-- All variables can be altered. -->
