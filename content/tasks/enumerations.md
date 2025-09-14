+++
title = "Enumerations"
description = ""
date = 2019-09-06T09:51:14Z
aliases = []
[extra]
id = 1967
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
languages = [
  "11l",
  "acl2",
  "ada",
  "algol_68",
  "amigae",
  "autohotkey",
  "awk",
  "bacon",
  "basic",
  "bracmat",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "dwscript",
  "e",
  "egl",
  "elixir",
  "erlang",
  "factor",
  "fantom",
  "forth",
  "fortran",
  "free_pascal",
  "freebasic",
  "futurebasic",
  "go",
  "groovy",
  "haskell",
  "huginn",
  "inform_7",
  "j",
  "java",
  "javascript",
  "jq",
  "jscript_net",
  "json",
  "julia",
  "kotlin",
  "lingo",
  "lua",
  "m2000_interpreter",
  "m4",
  "mathematica",
  "metafont",
  "nemerle",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "oz",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "raven",
  "retro",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "shen",
  "sidef",
  "slate",
  "standard_ml",
  "swift",
  "tcl",
  "toka",
  "vba",
  "visual_basic_dotnet",
  "xpl0",
  "zkl",
  "zonnon",
]
+++

## Task

Create an enumeration of constants with and without explicit values.





## 11l


```11l
T.enum TokenCategory
   NAME
   KEYWORD
   CONSTANT
   TEST_CATEGORY = 10
```



## ACL2


ACL2 doesn't have built-in enumerated types, but these macros add some basic support:


```Lisp
(defun symbol-to-constant (sym)
   (intern (concatenate 'string "*" (symbol-name sym) "*")
           "ACL2"))

(defmacro enum-with-vals (symbol value &rest args)
   (if (endp args)
       `(defconst ,(symbol-to-constant symbol) ,value)
       `(progn (defconst ,(symbol-to-constant symbol) ,value)
               (enum-with-vals ,@args))))

(defun interleave-with-nats-r (xs i)
   (if (endp xs)
       nil
       (cons (first xs)
             (cons i (interleave-with-nats-r (rest xs)
                                             (1+ i))))))

(defun interleave-with-nats (xs)
   (interleave-with-nats-r xs 0))

(defmacro enum (&rest symbols)
   `(enum-with-vals ,@(interleave-with-nats symbols)))
```



## Ada

Ada enumeration types have three distinct attributes, the enumeration literal, the enumeration position, and the representation value. The position value (starting with 0) is implied from the order of specification of the enumeration literals in the type declaration; it provides the ordering for the enumeration values. In the example below, apple (position 0) is less than banana (position 1) which is less than cherry (position 3) due to their positions, not due to their enumeration literal. An enumeration representation, when given, must not violate the order.

```ada
type Fruit is (apple, banana, cherry); -- No specification of the representation value;
for Fruit use (apple => 1, banana => 2, cherry => 4); -- specification of the representation values
```

Ada enumeration types are non-numeric discrete types. They can be used to index arrays, but there are no arithmetic operators for enumeration types; instead, there are predecessor and successor operations. Characters are implemented as an enumeration type in Ada.


## ALGOL 68

Note: In this first example '''ALGOL 68''''s MODE does not create
FRUITS as a distinct ''enumerated'' type.  In particular  FRUITS remain
compatible with INT and so FRUITS inherit/share all INT's operators
and procedures.

```algol68
BEGIN # example 1 #
  MODE FRUIT = INT;
  FRUIT apple = 1, banana = 2, cherry = 4;
  FRUIT x := cherry;
  CASE x IN
    print(("It is an apple #",x, new line)),
    print(("It is a banana #",x, new line)),
    SKIP, # 3 not defined #
    print(("It is a cherry #",x, new line))
  OUT
    SKIP # other values #
  ESAC
END;
```

```txt

It is a cherry #          +4

```


In this second example '''ALGOL 68''''s tagged unions are used to generate
the (private) values of the members of the enumerated type.  However this new type
comes with no operators, not even the "=" equality operator.  Hence at
least REPR (or ABS for INT type) must be defined if anything other then a
'''case''' conditional clause is required.

```algol68
BEGIN # example 2 #
  MODE ENUM = [0]CHAR; # something with minimal size #
  MODE APPLE = STRUCT(ENUM apple), BANANA = STRUCT(ENUM banana), CHERRY = STRUCT(ENUM cherry);
  MODE FRUIT = UNION(APPLE, BANANA, CHERRY);

  OP REPR = (FRUIT f)STRING:
    CASE f IN
      (APPLE):"Apple",
      (BANANA):"Banana",
      (CHERRY):"Cherry"
    OUT
      "?" # uninitalised #
    ESAC;

  FRUIT x := LOC CHERRY;

  CASE x IN
    (APPLE):print(("It is an ",REPR x, new line)),
    (BANANA):print(("It is a ",REPR x, new line)),
    (CHERRY):print(("It is a ",REPR x, new line))
  OUT
    SKIP # uninitialised FRUIT #
  ESAC
END
```

```txt

It is a Cherry

```

Warning: This second example is probably not how the ''conformity case clause'' construct was intended to be used.

See also: [[Standard_Deviation#ALGOL_68|Standard Deviation]] for another example.


## AmigaE


```amigae
ENUM APPLE, BANANA, CHERRY

PROC main()
  DEF x
  ForAll({x}, [APPLE, BANANA, CHERRY],
         `WriteF('\d\n', x))
ENDPROC
```


writes 0, 1, 2 to the console.

## AutoHotkey

AutoHotkey doesn't really enforce types.

However you can simulate types like enumeration with associative arrays:

```AutoHotkey
fruit_%apple% = 0
fruit_%banana% = 1
fruit_%cherry% = 2
```



## AWK

In awk we can use an array, for mapping both ways, or initialize variables:

```awk
fruit["apple"]=1; fruit["banana"]=2; fruit["cherry"]=3
fruit[1]="apple"; fruit[2]="banana"; fruit[3]="cherry"
i=0; apple=++i; banana=++i; cherry=++i;
```



## BASIC

```qbasic
REM Impossible. Can only be faked with arrays of strings.
OPTION BASE 1
DIM SHARED fruitsName$(1 to 3)
DIM SHARED fruitsVal%( 1 to 3)
fruitsName$[1] = "apple"
fruitsName$[2] = "banana"
fruitsName$[3] = "cherry"
fruitsVal%[1] = 1
fruitsVal%[2] = 2
fruitsVal%[3] = 3

REM OR GLOBAL CONSTANTS
DIM SHARED apple%, banana%, cherry%
apple%  = 1
banana% = 2
cherry% = 3
```


=
## BaCon
=
BaCon includes an ENUM statement, with or without fixed values.  If no value is given, enumerations start at zero and increase by integer 1.


```freebasic
' Enumerations
' Start at zero
ENUM
    cat, dog, parrot
END ENUM
PRINT "Dogs are #", dog

' Set value
ENUM
    Sunday=1, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday
END ENUM
PRINT Sunday, " ", Wednesday, " ", Saturday

' Change values, ENUM names must be unique
ENUM
    sunday=7, monday=1, tuesday, wednesday, thursday, friday, saturday
END ENUM
PRINT sunday, " ", wednesday, " ", saturday
```


```txt
prompt$ ./enums
Dogs are #1
1 4 7
7 3 6
```



## Bracmat

Wikipedia says: 'An enumeration is a collection of items that is a complete, ordered listing of all of the items in that collection.'
So the task is taken to be:
'Create a collection of constants that is a complete, ordered listing of all of the constants in that collection, with and without explicit values.'
In Bracmat, each expression is a constant and can be used in situations where one would use an enum in other languages. All expressions have an ordering in sums and products. In the case of non-numeric strings the ordering is alphabetic. It is not possible in Bracmat to have a constant without an explicit value, because the constant is nothing but the value, so only half of the task can be solved.

```bracmat>fruits=apple+banana+cherry;</lang



## C


```c
enum fruits { apple, banana, cherry };

enum fruits { apple = 0, banana = 1, cherry = 2 };
```


However, if defined like the above, in C you must use the type as <code>enum fruits</code>, not just <code>fruits</code>. A common practice in C (same with <code>struct</code>s) is to instead typedef the enum so you can refer to the type as a bare name:


```c
typedef enum { apple, banana, cherry } fruits;

typedef enum { apple = 0, banana = 1, cherry = 2 } fruits;
```



## C++


```cpp
enum fruits { apple, banana, cherry };

enum fruits { apple = 0, banana = 1, cherry = 2 };
```

Note that, unlike in C, you can refer to the type here as <code>fruits</code>.

----
C++11 introduced "strongly typed enumerations", enumerations that cannot be implicitly converted to/from integers:

```cpp
enum class fruits { apple, banana, cherry };

enum class fruits { apple = 0, banana = 1, cherry = 2 };
```


These enumeration constants must be referred to as <code>fruits::apple</code>, not just <code>apple</code>.

You can explicitly specify an underlying type for the enum; the default is <code>int</code>:

```cpp
enum class fruits : unsigned int { apple, banana, cherry };
```


You can also explicitly specify an underlying type for old-style enums:

```cpp
enum fruits : unsigned int { apple, banana, cherry };
```


## C#

```c#
enum fruits { apple, banana, cherry }

enum fruits { apple = 0, banana = 1, cherry = 2 }

enum fruits : int { apple = 0, banana = 1, cherry = 2 }

[FlagsAttribute]
enum Colors { Red = 1, Green = 2, Blue = 4, Yellow = 8 }
```


Placing FlagsAttribute before an enum allows you to perform bitwise operations on the value.
Note: All enums have a value of 0 defined, even if not specified in the set values.


## Clojure

In Clojure you will typically use keywords when you would use enums in other languages. Keywords are symbols that start with a colon and evaluate to themselves. For example:

```clojure
; a set of keywords
(def fruits #{:apple :banana :cherry})

; a predicate to test "fruit" membership
(defn fruit? [x] (contains? fruits x))

; if you need a value associated with each fruit
(def fruit-value (zipmap fruits (iterate inc 1)))

(println (fruit? :apple))
(println (fruit-value :banana))
```



## Common Lisp

Values:


```lisp
;; symbol to number
(defconstant +apple+ 0)
(defconstant +banana+ 1)
(defconstant +cherry+ 2)

;; number to symbol
(defun index-fruit (i)
  (aref #(+apple+ +banana+ +cherry+) i))
```

Of course, the two definitions above can be produced by a single macro, if desired.

Defining a type for documentation or checking purposes:


```lisp
(deftype fruit ()
  '(member +apple+ +banana+ +cherry+))
```



## D


```d
void main() {
    // Named enumeration (commonly used enum in D).
    // The underlying type is a 32 bit int.
    enum Fruits1 { apple, banana, cherry }

    // You can assign an enum to the general type, but not the opposite:
    int f1 = Fruits1.banana; // No error.
    // Fruits1 f2 = 1; // Error: cannot implicitly convert.

    // Anonymous enumeration, as in C, of type 32 bit int.
    enum { APPLE, BANANA, CHERRY }
    static assert(CHERRY == 2);

    // Named enumeration with specified values (int).
    enum Fruits2 { apple = 0, banana = 10, cherry = 20 }

    // Named enumeration, typed and with specified values.
    enum Fruits3 : ubyte { apple = 0, banana = 100, cherry = 200 }

    // Named enumeration, typed and with partially specified values.
    enum Test : ubyte { A = 2, B, C = 3 }
    static assert(Test.B == 3); // Uses the next ubyte, duplicated value.

    // This raises a compile-time error for overflow.
    // enum Fruits5 : ubyte { apple = 254, banana = 255, cherry }

    enum Component {
        none,
        red   = 2 ^^ 0,
        green = 2 ^^ 1,
        blue  = 2 ^^ 2
    }

    // Phobos BitFlags support all the most common operations on flags.
    // Some of the operations are shown below.
    import std.typecons: BitFlags;

    alias ComponentFlags = BitFlags!Component;
    immutable ComponentFlags flagsEmpty;

    // Value can be set with the | operator.
    immutable flagsRed = flagsEmpty | Component.red;

    immutable flagsGreen = ComponentFlags(Component.green);
    immutable flagsRedGreen = ComponentFlags(Component.red, Component.green);
    immutable flagsBlueGreen = ComponentFlags(Component.blue, Component.green);

    // Use the & operator between BitFlags for intersection.
    assert (flagsGreen == (flagsRedGreen & flagsBlueGreen));
}
```



## Delphi

In addition to [[#Pascal|standard Pascal]], one may explicitly specify an index:

```Delphi
type
	fruit = (apple, banana, cherry);
	ape = (gorilla = 0, chimpanzee = 1, orangutan = 5);
```

Note, explicit indices ''have'' to be in ascending order.
You can also just specify explicit indices for ''some'' items.


## DWScript



```Delphi
type TFruit = (Apple, Banana, Cherry);
type TApe = (Gorilla = 0, Chimpanzee = 1, Orangutan = 5);
```



## E

Simple group of object definitions (value methods could be left out if appropriate):


```e
def apple  { to value() { return 0 } }
def banana { to value() { return 1 } }
def cherry { to value() { return 2 } }
```

With a guard for type checks:

```e
interface Fruit guards FruitStamp {}
def apple  implements FruitStamp {}
def banana implements FruitStamp {}
def cherry implements FruitStamp {}

def eat(fruit :Fruit) { ... }
```

With and without values, using a hypothetical enumeration library:

```e
def [Fruit, [=> apple, => banana, => cherry]] := makeEnumeration()

def [Fruit, [=> apple, => banana, => cherry]] :=
  makeEnumeration(0, ["apple", "banana", "cherry"])
```



## EGL

```EGL
// Without explicit values
enumeration FruitsKind
	APPLE,
	BANANA,
	CHERRY
end

program EnumerationTest

	function main()
		whatFruitAmI(FruitsKind.CHERRY);
	end

	function whatFruitAmI(fruit FruitsKind)
		case (fruit)
			when(FruitsKind.APPLE)
				syslib.writestdout("You're an apple.");
			when(FruitsKind.BANANA)
				syslib.writestdout("You're a banana.");
			when(FruitsKind.CHERRY)
				syslib.writestdout("You're a cherry.");
			otherwise
				syslib.writestdout("I'm not sure what you are.");
		end
	end

end
```

-and-
```EGL
// With explicit values
library FruitsKind type BasicLibrary {}
	const APPLE int = 0;
	const BANANA int = 1;
	const CHERRY int = 2;
end

program EnumerationTest

	function main()
		whatFruitAmI(FruitsKind.CHERRY);
	end

	function whatFruitAmI(fruit int in)
		case (fruit)
			when(FruitsKind.APPLE)
				syslib.writestdout("You're an apple.");
			when(FruitsKind.BANANA)
				syslib.writestdout("You're a banana.");
			when(FruitsKind.CHERRY)
				syslib.writestdout("You're a cherry.");
			otherwise
				syslib.writestdout("I'm not sure what you are.");
		end
	end

end

```



## Elixir

It is possible to use a atom if the value is unrelated.

```elixir
fruits = [:apple, :banana, :cherry]
fruits = ~w(apple banana cherry)a                   # Above-mentioned different notation
val = :banana
Enum.member?(fruits, val)                           #=> true
val in fruits                                       #=> true
```


If they have to have a specific value

```elixir
fruits = [{:apple, 1}, {:banana, 2}, {:cherry, 3}]  # Keyword list
fruits = [apple: 1, banana: 2, cherry: 3]           # Above-mentioned different notation
fruits[:apple]                                      #=> 1
Keyword.has_key?(fruits, :banana)                   #=> true

fruits = %{:apple=>1, :banana=>2, :cherry=>3}       # Map
fruits = %{apple: 1, banana: 2, cherry: 3}          # Above-mentioned different notation
fruits[:apple]                                      #=> 1
fruits.apple                                        #=> 1 (Only When the key is Atom)
Map.has_key?(fruits, :banana)                       #=> true
```


To give a number in turn, there is the following method.

```elixir
# Keyword list
fruits = ~w(apple banana cherry)a |> Enum.with_index
#=> [apple: 0, banana: 1, cherry: 2]

# Map
fruits = ~w(apple banana cherry)a |> Enum.with_index |> Map.new
#=> %{apple: 0, banana: 1, cherry: 2}
```



## Erlang

For the unspecific value enum use case, Erlang has atoms. You can use apple, banana, orange directly in the code.
If they have to have a specific value they could be grouped like this: {apple, 1}, {banana, 3}, {orange, 8}


## Factor


Enumerations are essentially association lists with values (keys) assigned sequentially from constants (values) provided by an initial sequence.

```factor
IN: scratchpad { "sun" "mon" "tue" "wed" "thur" "fri" "sat" } <enum>

--- Data stack:
T{ enum f ~array~ }
IN: scratchpad [ 1 swap at ] [ keys ] bi

--- Data stack:
"mon"
{ 0 1 2 3 4 5 6 }
```

Factor also provides C-like enumerations in its C library interface. These enumerations may have explicit values.

```factor
IN: scratchpad USE: alien.syntax
IN: scratchpad ENUM: day sun mon { tue 42 } wed thur fri sat ;
IN: scratchpad 1 <day>

--- Data stack:
mon
IN: scratchpad 42 <day>

--- Data stack:
mon
tue
```



## Fantom


Enumerations with named constants:


```fantom

// create an enumeration with named constants
enum class Fruits { apple, banana, orange }

```


A private constructor can be added to initialise internal fields, which must be constant.


```Fantom

// create an enumeration with explicit values
enum class Fruits_
{
  apple (1), banana (2), orange (3)
  const Int value
  private new make (Int value) { this.value = value }
}

```



## Forth

Forth has no types, and therefore no enumeration type. To define sequential constants, a programmer might write code like this:


```forth
0 CONSTANT apple
1 CONSTANT banana
2 CONSTANT cherry
...
```

However, a common idiom in forth is to define a defining word, such as:

```forth
: ENUM ( n -<name>- n+1 )   DUP CONSTANT 1+ ;
```

This word defines a new constant of the value specified and returns the next value in sequence.
It would be used like this:


```forth>0 ENUM APPLE  ENUM BANANA  ENUM CHERRY  DROP</lang


Or you can use CONSTANT to capture the "end" value instead of dropping it:


```forth>0 ENUM FIRST ENUM SECOND ...  CONSTANT LAST</lang


A variation of this idea is the "stepped enumeration" that increases the value by more than 1, such as:


```forth
: SIZED-ENUM ( n s -<name>- n+s )   OVER CONSTANT + ;
: CELL-ENUM ( n -<name>- n+cell )   CELL SIZED-ENUM ;
```


A programmer could combine these enum definers in any way desired:


```forth
0 ENUM       FIRST   \ value = 0
CELL-ENUM    SECOND  \ value = 1
ENUM         THIRD   \ value = 5
3 SIZED-ENUM FOURTH  \ value = 6
ENUM         FIFTH   \ value = 9
CONSTANT     SIXTH   \ value = 10
```


Note that a similar technique is often used to implement structures in Forth.

For a simple zero-based sequence of constants, one could use a loop in the defining word:

```forth
: CONSTANTS ( n -- ) 0 DO I CONSTANT LOOP ;

\ resistor digit colors
10 CONSTANTS black brown red orange yellow green blue violet gray white
```



## Fortran

```fortran
enum, bind(c)
  enumerator :: one=1, two, three, four, five
  enumerator :: six, seven, nine=9
end enum
```


The syntax


```fortran
enum, bind(c) :: nametype
  enumerator :: one=1, two, three
end enum nametype
```


does not work with gfortran; it is used in some [http://docs.cray.com/books/S-3692-51/html-S-3692-51/z970507905n9123.html Cray docs] about Fortran, but the syntax shown at [http://publib.boulder.ibm.com/infocenter/comphelp/v8v101/index.jsp?topic=/com.ibm.xlf101a.doc/xlflr/enum.htm IBM] is the one gfortran can understand. (Cray's docs refer to Fortran 2003 draft, IBM docs refers to Fortran 2003 standard, but read the brief [http://publib.boulder.ibm.com/infocenter/comphelp/v8v101/topic/com.ibm.xlf101a.doc/xlflr/languagestandards.htm#wq17 Fortran 2003 Standard] section to understand why differences may exist...)


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Enum Animals
  Cat
  Dog
  Zebra
End Enum

Enum Dogs
  Bulldog = 1
  Terrier = 2
  WolfHound = 4
End Enum

Print Cat, Dog, Zebra
Print Bulldog, Terrier, WolfHound
Sleep
```


```txt

 0             1             2
 1             2             4

```



## Free Pascal

See [[#Delphi|Delphi]].
Note, depending on the <tt>{$scopedEnum}</tt> compiler switch (as of definition time), enumeration type members are identified via the type name prepended.

Additionally, enumeration types can be passed to <tt>write</tt>/<tt>writeLn</tt> producing the Pascal (source code) identifier.

=={{header|F_Sharp|F#}}==
Enumerations in F# always have explicit values:

```fsharp
type Fruit =
  | Apple = 0
  | Banana = 1
  | Cherry = 2

let basket = [ Fruit.Apple ; Fruit.Banana ; Fruit.Cherry ]
Seq.iter (printfn "%A") basket
```


If the initialization values are omitted, the resulting type is a discriminated union (algebraic data type) instead.
Simple discriminated unions can be used similarly to enumerations, but they are never convertible from and to integers, and their internal representation is quite different.


```fsharp
type Fruit =
  | Apple
  | Banana
  | Cherry
let basket = [ Apple ; Banana ; Cherry ]
Seq.iter (printfn "%A") basket
```



## FutureBasic

<lang>
include "ConsoleWindow"

begin enum 1
_apple
_banana
_cherry
end enum

begin enum
_appleExplicit  = 10
_bananaExplicit = 15
_cherryExplicit = 30
end enum

print "_apple =";  _apple
print "_banana ="; _banana
print "_cherry ="; _cherry
print
print "_appleExplicit =";  _appleExplicit
print "_bananaExplicit ="; _bananaExplicit
print "_cherryExplicit ="; _cherryExplicit

```


Output

```txt

_apple = 1
_banana = 2
_cherry = 3

_appleExplicit = 10
_bananaExplicit = 15
_cherryExplicit = 30

```



## Go

Go's enumeration-like feature is called iota.  It generates sequential integer constants.

```go
const (
	apple = iota
	banana
	cherry
)
```

The above is equivalent to,

```go
const (
	apple  = 0
	banana = 1
	cherry = 2
)
```

Constants in Go are not typed they way variables are, they are typed when used just like literal constants.
Here is an example of a type safe enumeration:

```go
type fruit int

const (
	apple fruit = iota
	banana
	cherry
)
```

And using explicit values (note each constant must be individual typed here unlike with iota):

```go
type fruit int

const (
	apple  fruit = 0
	banana fruit = 1
	cherry fruit = 2
)
```



## Groovy

Enumerations:

```groovy
enum Fruit { apple, banana, cherry }

enum ValuedFruit {
    apple(1), banana(2), cherry(3);
    def value
    ValuedFruit(val) {value = val}
    String toString() { super.toString() + "(${value})" }
}

println Fruit.values()
println ValuedFruit.values()
```


```txt
[apple, banana, cherry]
[apple(1), banana(2), cherry(3)]
```



## Haskell


```haskell
data Fruit = Apple | Banana | Cherry deriving Enum
```



## Huginn


```huginn
enum FRUIT {
  APPLE,
  BANANA,
  CHERRY
}
```



## Inform 7


```inform7
Fruit is a kind of value. The fruits are apple, banana, and cherry.
```


Inform 7 doesn't have conversions between enumerated values and numbers, but you can assign properties to enumerated values:

```inform7
[sentence form]
Fruit is a kind of value. The fruits are apple, banana, and cherry.
A fruit has a number called numeric value.
The numeric value of apple is 1.
The numeric value of banana is 2.
The numeric value of cherry is 3.
```


```inform7
[table form]
Fruit is a kind of value. The fruits are defined by the Table of Fruits.

Table of Fruits
fruit	numeric value
apple	1
banana	2
cherry	3
```


=={{header|Icon}} and {{header|Unicon}}==
Nether Icon nor Unicon has an explicit enumeration type; however, there are several approaches that can be used for this purpose:


```Icon
  fruits := [ "apple", "banana", "cherry", "apple" ]  # a list keeps ordered data
  fruits := set("apple", "banana", "cherry")          # a set keeps unique data
  fruits := table()                                   # table keeps an unique data with values
  fruits["apple"]  := 1
  fruits["banana"] := 2
  fruits["cherry"] := 3
```



## J


J's typing system is fixed, and so extensions occur at the application level.  For example, one could create an object

```j
   enum =: cocreate''
   ( (;:'apple banana cherry') ,L:0 '__enum' ) =: i. 3
   cherry__enum
2
```


But this is more akin to a "methodless class or object" than an enum in other languages.

That said, note that the "natural way", in J, of dealing with issues treated in other languages through enums is to use an array of names.

```j
   fruit=: ;:'apple banana cherry'
```


Now you can get the name associated with an index:


```j
   2 { fruit
+------+
|cherry|
+------+
```


And you can get the index associated with a name:


```j
  fruit i.<'banana'
1
```


And you can define an arithmetic with the enum for its domain and range.  Here, for example, is 2=1+1:


```j
   (<'banana') +&.(fruit&i.)  <'banana'
+------+
|cherry|
+------+
```


And, you can iterate over the values (though an example of that is probably beyond the scope of this task), along with numerous other variations on these themes.

A person could reasonably argue that enums were introduced in some languages to work around deficiencies in array handling in those languages.


## Java

```java5
enum Fruits{
   APPLE, BANANA, CHERRY
}
```

Or:

```java5
enum Fruits{
  APPLE(0), BANANA(1), CHERRY(2)
  private final int value;
  fruits(int value) { this.value = value; }
  public int value() { return value; }
}
```

Conventionally, enums have the same case rules as classes, while enum values are in all caps (like other constants). All cases are allowed for both names, though, as long as they don't conflict with other classes in the same package.


## JavaScript

In javascript, usually used for this a strings.


```javascript

// enum fruits { apple, banana, cherry }

var f = "apple";

if(f == "apple"){
    f = "banana";
}

```



## jq


Finite, ordered enumerations can be represented in jq as JSON arrays, e.g. ["apple", "banana", "cherry"], or as sequences,
e.g. ("apple", "banana", "cherry").  The latter interpretation corresponds to the idea of '''enumerating''' a collection, and also dovetails with the concept of infinite enumerations.

Countably-infinite ordered enumerations can be represented by generators, e.g. the non-negative natural numbers can be represented by the jq expression:

     1 | while(true; .+1)

Finite, unordered enumerations can be represented as JSON objects, as in the JSON section of this article.
In this context, it is worth noting that jq allows a shorthand notation for specifying objects, so that we can for example write:

    def fruits: {apple, banana, cherry};  # i.e. {"apple" : null, "banana": null, "cherry": null }


## JSON


```json
{"fruits" : { "apple" : null, "banana" : null, "cherry" : null }
{"fruits" : { "apple" : 0, "banana" : 1, "cherry" : 2 }
```



## JScript.NET


```jscript
enum fruits { apple, banana, cherry }
enum fruits { apple = 0, banana = 1, cherry = 2 }
```



## Julia


```julia

@enum Fruits APPLE BANANA CHERRY

```


```txt

julia> Fruits
Enum Fruits:
APPLE = 0
BANANA = 1
CHERRY = 2


```



## Kotlin


```scala
// version 1.0.5-2

enum class Animals {
    CAT, DOG, ZEBRA
}

enum class Dogs(val id: Int) {
    BULLDOG(1), TERRIER(2), WOLFHOUND(4)
}

fun main(args: Array<String>) {
    for (value in Animals.values()) println("${value.name.padEnd(5)} : ${value.ordinal}")
    println()
    for (value in Dogs.values()) println("${value.name.padEnd(9)} : ${value.id}")
}
```


```txt

CAT   : 0
DOG   : 1
ZEBRA : 2

BULLDOG   : 1
TERRIER   : 2
WOLFHOUND : 4

```



## Lingo

Lingo neither knows the concept of enumerations nor of constants. But an enumeration-like hash (property list) that is immutable concerning standard list methods and operators can be created by sub-classing a property list and overwriting list/property list access methods (which also overwrites bracket access operators on the fly):


```lingo
-- parent script "Enumeration"

property ancestor

on new (me)
  data = [:]
  repeat with i = 2 to the paramCount
    data[param(i)] = i-1
  end repeat
  me.ancestor = data
  return me
end

on setAt (me)
  -- do nothing
end

on setProp (me)
  -- do nothing
end

on deleteAt (me)
  -- do nothing
end

on deleteProp (me)
  -- do nothing
end

on addProp (me)
  -- do nothing
end
```



```lingo
enumeration = script("Enumeration").new("APPLE", "BANANA", "CHERRY")

put enumeration["BANANA"]
-- 2

-- try to change a value after construction (fails)
enumeration["BANANA"] = 666
put enumeration["BANANA"]
-- 2

-- try to change a value after construction using setProp (fails)
enumeration.setProp("BANANA", 666)
put enumeration["BANANA"]
-- 2

-- try to delete a value after construction (fails)
enumeration.deleteAt(2)
put enumeration["BANANA"]
-- 2

-- try to delete a value after construction using deleteProp (fails)
enumeration.deleteProp("BANANA")
put enumeration["BANANA"]
-- 2

-- try to add a new value after construction (fails)
enumeration["FOO"] = 666
put enumeration["FOO"]
-- <Void>

-- try to add a new value after construction using addProp (fails)
enumeration.addProp("FOO", 666)
put enumeration["FOO"]
-- <Void>
```



## Lua

An explicit enum can be formed by mapping strings to numbers


```lua

local fruit = {apple = 0, banana = 1, cherry = 2}

```


or simply by local variables.


```lua

local apple, banana, cherry = 0,1,2

```


Although since Lua strings are interned, there is as much benefit to simply using strings.

## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
      \\ need revision 15, version 9.4
      Enum Fruit  {apple, banana, cherry}
      Enum Fruit2  {apple2=10, banana2=20, cherry2=30}
      Print apple, banana, cherry
      Print apple2, banana2, cherry2
      Print Len(apple)=0
      Print Len(banana)=1
      Print Len(cherry)=2
      Print Len(cherry2)=2, Cherry2=30, Type$(Cherry2)="Fruit2"

      k=each(Fruit)
      While k {
            \\ name of variable, value, length from first (0, 1, 2)
            Print Eval$(k), Eval(k), k^
      }
      m=apple
      Print Eval$(m)="apple"
      Print Eval(m)=m
      m++
      Print Eval$(m)="banana"
      Try {
            \\ error, m is an object
            m=100
      }
      Try {
            \\ error not the same type
            m=apple2
      }
      Try {
            \\ read only can't change
            apple2++
      }
      m++
      Print Eval$(m)="cherry", m
      k=Each(Fruit2 end to start)
      While k {
             Print Eval$(k), Eval(k) , k^
             CheckByValue(Eval(k))
      }
      m2=apple2
      Print "-------------------------"
      CheckByValue(m2)
      CheckByReference(&m2)
      Print m2

      Sub CheckByValue(z as Fruit2)
            Print Eval$(z), z
      End Sub

      Sub CheckByReference(&z as Fruit2)
            z++
            Print Eval$(z), z
      End Sub
}
Checkit

```




## M4


```M4
define(`enums',
   `define(`$2',$1)`'ifelse(eval($#>2),1,`enums(incr($1),shift(shift($@)))')')
define(`enum',
   `enums(1,$@)')
enum(a,b,c,d)
`c='c
```


```txt

c=3

```




## Mathematica

Enumerations are not very useful in a symbolic language like Mathematica. If desired, an 'enum' function could be defined :

```Mathematica
MapIndexed[Set, {A, B, F, G}]
->{{1}, {2}, {3}, {4}}

A
->{1}

B
->{2}

G
->{4}
```



=={{header|MATLAB}} / {{header|Octave}}==
Enumeration is done by creating a cell array (a.k.a set) of objects, where the numeral of the object is its index in the 1-based cell array. The cell array structure can contain any type of data structure including other cell arrays, and all members don't have to be the same data type.

Example:

```MATLAB
stuff = {'apple', [1 2 3], 'cherry',1+2i}

stuff =

    'apple'    [1x3 double]    'cherry'    [1.000000000000000 + 2.000000000000000i]
```



## Metafont

Metafont has no an enumeration type. However we can define an useful macro to simulate an enumeration. E.g.

```metafont
vardef enum(expr first)(text t) =
save ?; ? := first;
forsuffixes e := t: e := ?; ?:=?+1; endfor
enddef;
```


Usage example:


```metafont
enum(1, Apple, Banana, Cherry);
enum(5, Orange, Pineapple, Qfruit);
show Apple, Banana, Cherry, Orange, Pineapple, Qfruit;

end
```

=={{header|Modula-3}}==

```modula3
TYPE Fruit = {Apple, Banana, Cherry};
```

The values are accessed by qualifying their names.

```modula3>fruit := Fruit.Apple;</lang

You can get an element's position in the enumeration by using <code>ORD</code> and get the element given the position by using <code>VAL</code>.

```modula3
ORD(Fruit.Apple); (* Returns 0 *)
VAL(0, Fruit); (* Returns Fruit.Apple *)
```



## Nemerle


```Nemerle
enum Fruit {
    |apple
    |banana
    |cherry
}

enum Season {
    |winter = 1
    |spring = 2
    |summer = 3
    |autumn = 4
}
```



## Nim


```nim
type Fruits = enum Apple, Banana, Cherry

type Fruits = enum Apple = 0, Banana = 1, Cherry = 2 # with values

type Fruits {.pure.} = enum Apple, Banana, Cherry # scoped enum
var i: int = Apple # error for scoped enum

type Fruits = enum Apple = "Apple", Banana = "Banana", Cherry = "Cherry" # with string literals
```



## Objeck


```objeck

enum Color := -3 {
  Red,
  White,
  Blue
}

enum Dog {
  Pug,
  Boxer,
  Terrier
}

```


=={{header|Objective-C}}==
With iOS 6+ SDK / Mac OS X 10.8+ SDK:

```objc
typedef NS_ENUM(NSInteger, fruits) { apple, banana, cherry };

typedef NS_ENUM(NSInteger, fruits) { apple = 0, banana = 1, cherry = 2 };
```



## OCaml


```ocaml
type fruit =
  | Apple
  | Banana
  | Cherry
```



## Oforth


In Oforth, you use symbols to define enumerations.Symbols are strings that are identical : if two symbols are equal (==), they are the same object.

You can't define explicit values for these symbols as they a themseelves values.

Symbols begin with $. If the symbol does not exists yet, it is created.


```Oforth
[ $apple, $banana, $cherry ] const: Fruits
```



## Oz

Most of the time you will just use atoms where you would use enums in C. Atoms start with a lower-case letter and are just symbols that evaluate to themselves. For example:

```oz
declare
  fun {IsFruit A}
     {Member A [apple banana cherry]}
  end
in
  {Show {IsFruit banana}}
```


If you need constants with increasing values, you could just enumerate them manually:

```oz
declare
  Apple = 1
  Banana = 2
  Cherry = 3
```


Or you could write a procedure that does the job automatically:

```oz
declare
  proc {Enumeration Xs}
     Xs = {List.number 1 {Length Xs} 1}
  end

  [Apple Banana Cherry] = {Enumeration}
in
  {Show Cherry}
```



## Pascal

Standard Pascal as per ISO 7185 only allows contiguous lists of identifiers as enumerated type definitions.
An explicit index may not be specified, but [[#Delphi|Delphi]] and [[#Free Pascal|Free Pascal]] allow this.
However, it is guaranteed, that the <tt>ord</tt>inal value will correspond to the member’s position in the list (<tt>0</tt>-based).

```pascal
type
	phase = (red, green, blue);
```



## Perl


```perl
# Using an array
my @fruits = qw(apple banana cherry);

# Using a hash
my %fruits = ( apple => 0, banana => 1, cherry => 2 );
```



## Perl 6

```perl6>enum Fruit <Apple Banana Cherry
; # Numbered 0 through 2.

enum ClassicalElement (
    Earth => 5,
    'Air',  # gets the value 6
    'Fire', # gets the value 7
    Water => 10,
);
```



## Phix


```Phix
enum apple, banana, orange
enum apple=5, banana=10, orange=15
```



## PHP


```php
// Using an array/hash
$fruits = array( "apple", "banana", "cherry" );
$fruits = array( "apple" => 0, "banana" => 1, "cherry" => 2 );

// If you are inside a class scope
class Fruit {
  const APPLE = 0;
  const BANANA = 1;
  const CHERRY = 2;
}

// Then you can access them as such
$value = Fruit::APPLE;

// Or, you can do it using define()
define("FRUIT_APPLE", 0);
define("FRUIT_BANANA", 1);
define("FRUIT_CHERRY", 2);
```



## PicoLisp

Enumerations are not very useful in a symbolic language like PicoLisp. If
desired, an 'enum' function could be defined:

```PicoLisp
(de enum "Args"
   (mapc def "Args" (range 1 (length "Args"))) )
```

And used in this way:

```PicoLisp
: (enum A B C D E F)
-> F
```


```txt
: A
-> 1
: B
-> 2
: F
-> 6
```



## PL/I


```PL/I

define ordinal animal (frog, gnu, elephant, snake);

define ordinal color (red value (1), green value (3), blue value (5));

```



## PowerShell

Without explicit values.
```PowerShell

Enum fruits {
    Apple
    Banana
    Cherry
}
[fruits]::Apple
[fruits]::Apple + 1
[fruits]::Banana + 1

```

<b>Output:</b>

```txt

Apple
Banana
Cherry

```

With explicit values.
```PowerShell

Enum fruits {
    Apple = 10
    Banana = 15
    Cherry = 30
}
[fruits]::Apple
[fruits]::Apple + 1
[fruits]::Banana + 1

```


```txt

Apple
11
16

```



## PureBasic

Basic Enumeration is defined as

```PureBasic
Enumeration
   #Apple
   #Banana
   #Cherry
EndEnumeration
```

This can also be adjusted to the form

```PureBasic
Enumeration 10200 Step 12
  #Constant1           ; 10200
  #Constant2           ; 10212
  #Constant3           ; 10224
  #Constant4 = 10117   ; 10117
  #Constant5           ; 10229
EndEnumeration
```

The system constant "#PB_Compiler_EnumerationValue" holds last defined value and can be used to chain to a previously started series.

E.g. in combination with the code above;

```PureBasic
Enumeration #PB_Compiler_EnumerationValue
  #Constant_A           ; 10241
  #Constant_B           ; 10242
EndEnumeration
```


Enumeration groups can also be named to allow continuation where a previous named group left off.

```PureBasic
;This starts the enumeration of a named group 'NamedGroup'.
Enumeration NamedGroup 5
  #Green                ; 5
  #Orange               ; 6
EndEnumeration

;EnumerationBinary will use values that are a double of the previous value (or starting value).
EnumerationBinary
  #North                ; 1
  #West                 ; 2
  #South                ; 4
  #East                 ; 8
EndEnumeration

;This continues the enumeration of the previously named group 'NamedGroup'.
Enumeration NamedGroup
  #Yellow               ; 7
  #Red                  ; 8
EndEnumeration
```



## Python


### Python: Version 3.4+

Note: [http://www.python.org/dev/peps/pep-0435/ enumerations have come to Python version 3.4].


```python>>>
 from enum import Enum
>>> Contact = Enum('Contact', 'FIRST_NAME, LAST_NAME, PHONE')
>>> Contact.__members__
mappingproxy(OrderedDict([('FIRST_NAME', <Contact.FIRST_NAME: 1>), ('LAST_NAME', <Contact.LAST_NAME: 2>), ('PHONE', <Contact.PHONE: 3>)]))
>>>
>>> # Explicit
>>> class Contact2(Enum):
	FIRST_NAME = 1
	LAST_NAME = 2
	PHONE = 3


>>> Contact2.__members__
mappingproxy(OrderedDict([('FIRST_NAME', <Contact2.FIRST_NAME: 1>), ('LAST_NAME', <Contact2.LAST_NAME: 2>), ('PHONE', <Contact2.PHONE: 3>)]))
>>>
```



### Python: Pre version 3.4

There is no special syntax, typically global variables are used with range:

```python
FIRST_NAME, LAST_NAME, PHONE = range(3)
```

Alternately, the above variables can be enumerated from a list with no predetermined length.

```python
vars().update((key,val) for val,key in enumerate(("FIRST_NAME","LAST_NAME","PHONE")))
```



## R

R does not have an enumeration type, though factors provide a similar functionality.

```R
 factor(c("apple", "banana", "cherry"))
# [1] apple  banana cherry
# Levels: apple banana cherry
```

[http://tolstoy.newcastle.edu.au/R/help/04/07/0368.html This thread] in the R mail archive contains code for an enum-like class for traffic light colours.


## Racket



```Racket

#lang racket

;; Like other Lisps, Racketeers prefer using symbols directly instead of
;; numeric definitions, and lists of symbols instead of bitwise
;; combinations
(define fruits '(apple banana cherry))

;; In Typed Racket, a type can be defined for a specific set of symbols
;; (define-type Fruit (U 'apple 'banana 'cherry))

;; The conventional approach is possible too, of course
(define APPLE  1)
(define BANANA 2)
(define CHERRY 4)

;; And finally, when dealing with foreign functions it is useful to
;; translate idiomatic Racket values (= symbols) to/from integers.
;; Racket's ffi has two ways to do this -- either an enumeration (for
;; independent integer constants) or a bitmask (intended to represent
;; sets using bitwise or):
(require ffi/unsafe)
(define _fruit  (_enum '(APPLE = 1
                         BANANA
                         CHERRY = 4)))
(define _fruits (_bitmask '(APPLE = 1
                            BANANA = 2
                            CHERRY = 4)))

;; Normally, Racket code will just use plain values (a symbol for the
;; first, and a list of symbols for the second) and the foreign side
;; sees the integers.  But do demonstrate this, we can use the primitive
;; raw functionality to see how the translation works:
(require (only-in '#%foreign ctype-scheme->c ctype-c->scheme))

((ctype-scheme->c _fruit)  'CHERRY)         ; -> 4
((ctype-scheme->c _fruits) 'CHERRY)         ; -> 4
((ctype-scheme->c _fruits) '(APPLE CHERRY)) ; -> 5

((ctype-c->scheme _fruit)  4) ; -> 'CHERRY
((ctype-c->scheme _fruits) 4) ; -> '(CHERRY)
((ctype-c->scheme _fruits) 5) ; -> '(APPLE CHERRY)

```



## Raven


```raven
{ 'apple' 0 'banana' 1 'cherry' 2 } as fruits
```



## Retro

Retro has a library named '''enum'''' for creation of enumerated values.


```Retro
needs enum'
( Creating a series of values )
0 ^enum'enum| a b c d |

( Create values individually )
0 ^enum'enum a  ^enum'enum b
```


The actual values for each subsequent enumerated value created are determined by the '''^enum'step''' function. This defaults to incrementing by 1, but can be altered as desired:


```Retro
with enum'
[ 10 * ] is step
0 ^enum'enum| a b c d |
```



## REXX

REXX has no types, and therefore has no enumeration type.


However, in the spirit of enumeration, REXX programmers can use stemmed arrays for enumerating constants (shown below).

This REXX entry was kinda modeled after the '''BASIC''', '''Forth''', and
'''VBA''' [which does its own enumeration, as does REXX below (as an inventory count)].

```rexx
/*REXX program illustrates a method of  enumeration  of  constants via  stemmed arrays. */
fruit.=0                              /*the default for all possible "FRUITS."  (zero). */
           fruit.apple      =   65
           fruit.cherry     =    4
           fruit.kiwi       =   12
           fruit.peach      =   48
           fruit.plum       =   50
           fruit.raspberry  =   17
           fruit.tomato     = 8000
           fruit.ugli       =    2
           fruit.watermelon =    0.5  /*◄─────────── could also be specified as:   1/2  */

                                            /*A method of using a list (of some fruits).*/
@fruits= 'apple apricot avocado banana bilberry blackberry blackcurrant blueberry baobab',
         'boysenberry breadfruit cantaloupe cherry chilli chokecherry citron coconut',
         'cranberry cucumber currant date dragonfruit durian eggplant elderberry fig',
         'feijoa gac gooseberry grape grapefruit guava honeydew huckleberry jackfruit',
         'jambul juneberry kiwi kumquat lemon lime lingenberry loquat lychee mandarin',
         'mango mangosteen nectarine orange papaya passionfruit peach pear persimmon',
         'physalis pineapple pitaya pomegranate pomelo plum pumpkin rambutan raspberry',
         'redcurrant satsuma squash strawberry tangerine tomato ugli watermelon zucchini'

/*╔════════════════════════════════════════════════════════════════════════════════════╗
  ║Parental warning: sex is discussed below: PG─13.  Most berries don't have "berry" in║
  ║their name.  A  berry  is a  simple fruit  produced from a single ovary.  Some true ║
  ║berries are: pomegranate, guava, eggplant, tomato, chilli, pumpkin, cucumber, melon,║
  ║and citruses.  Blueberry  is a  false  berry;  blackberry is an  aggregate  fruit;  ║
  ║and strawberry is an  accessory  fruit.  Most nuts are fruits.  The following aren't║
  ║true nuts: almond, cashew, coconut, macadamia, peanut, pecan, pistachio, and walnut.║
  ╚════════════════════════════════════════════════════════════════════════════════════╝*/

                               /*  ┌─◄── due to a Central America blight in 1922; it was*/
                               /*  ↓     called the Panama disease (a soil─borne fungus)*/
if fruit.banana=0  then say "Yes!  We have no bananas today."               /* (sic) */
if fruit.kiwi \=0  then say "We gots "   fruit.kiwi    ' hairy fruit.'      /*   "   */
if fruit.peach\=0  then say "We gots "   fruit.peach   ' fuzzy fruit.'      /*   "   */

maxL=length('  fruit   ')                        /*ensure this header title can be shown*/
maxQ=length(' quantity ')                        /*   "     "    "      "    "   "   "  */
say
     do p    =0  for 2                           /*the first pass finds the maximums.   */
         do j=1  for words(@fruits)              /*process each of the names of fruits. */
         @=word(@fruits, j)                      /*obtain a fruit name from the list.   */
         #=value('FRUIT.'@)                      /*   "   the quantity of a fruit.      */
         if \p  then do                          /*is this the first pass through ?     */
                     maxL=max(maxL, length(@))   /*the longest (widest) name of a fruit.*/
                     maxQ=max(maxQ, length(#))   /*the widest width quantity of fruit.  */
                     iterate  /*j*/              /*now, go get another name of a fruit. */
                     end
         if j==1  then say center('fruit', maxL)    center("quantity", maxQ)
         if j==1  then say copies('─'    , maxL)    copies("─"       , maxQ)
         if #\=0  then say  right( @     , maxL)     right( #        , maxQ)
         end   /*j*/
     end       /*p*/
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''

```txt

Yes!  We have no bananas today.
We gots  12  hairy fruit.
We gots  48  fuzzy fruit.

   fruit      quantity
──────────── ──────────
       apple         65
      cherry          4
        kiwi         12
       peach         48
        plum         50
   raspberry         17
      tomato       8000
        ugli          2
  watermelon        0.5

```



## Ring


```ring

apple = 0
banana = 1
cherry = 2
see "apple : " + apple + nl
see "banana : " + banana + nl
see "cherry : " + cherry + nl

```



## Ruby

There are plenty of ways to represent '''enum''' in Ruby. Here it is just one example:

```ruby
module Fruits
  APPLE  = 0
  BANANA = 1
  CHERRY = 2
end

# It is possible to use a symbol if the value is unrelated.

FRUITS = [:apple, :banana, :cherry]
val = :banana
FRUITS.include?(val)      #=> true
```

To give a number in turn, there is the following method.

```ruby
module Card
  # constants
  SUITS = %i(Clubs Hearts Spades Diamonds)
  SUIT_VALUE = SUITS.each_with_index.to_h               # version 2.1+
# SUIT_VALUE = Hash[ SUITS.each_with_index.to_a ]       # before it
  #=> {:Clubs=>0, :Hearts=>1, :Spades=>2, :Diamonds=>3}

  PIPS = %i(2 3 4 5 6 7 8 9 10 Jack Queen King Ace)
  PIP_VALUE = PIPS.each.with_index(2).to_h              # version 2.1+
# PIP_VALUE = Hash[ PIPS.each.with_index(2).to_a ]      # before it
  #=> {:"2"=>2, :"3"=>3, :"4"=>4, :"5"=>5, :"6"=>6, :"7"=>7, :"8"=>8, :"9"=>9, :"10"=>10, :Jack=>11, :Queen=>12, :King=>13, :Ace=>14}
end
```



## Rust


```rust
enum Fruits {
    Apple,
    Banana,
    Cherry
}

enum FruitsWithNumbers {
    Strawberry = 0,
    Pear = 27,
}

fn main() {
    // Access to numerical value by conversion
    println!("{}", FruitsWithNumbers::Pear as u8);
}
```



## Scala

'''1. Using Algebraic Data Types:'''

```actionscript
sealed abstract class Fruit
case object Apple extends Fruit
case object Banana extends Fruit
case object Cherry extends Fruit

```

'''2. Using scala.Enumeration:'''

```actionscript
object Fruit extends Enumeration {
  val Apple, Banana, Cherry = Value
}

```


## Scheme


```scheme
(define apple 0)
(define banana 1)
(define cherry 2)

(define (fruit? atom)
  (or (equal? 'apple atom)
      (equal? 'banana atom)
      (equal? 'cherry atom)))
```

(This section needs attention from someone familiar with Scheme idioms.)

## Seed7


```seed7
const type: fruits is new enum
    apple, banana, cherry
  end enum;
```


## Shen


```shen
(tc +)

(datatype fruit

  if (element? Fruit [apple banana cherry])
  _____________
  Fruit : fruit;)
```



## Sidef

Implicit:

```ruby
enum {Apple, Banana, Cherry};   # numbered 0 through 2
```

Explicit:

```ruby
enum {
    Apple=3,
    Banana,         # gets the value 4
    Cherry="a",
    Orange,         # gets the value "b"
};
```



## Slate

As just unique objects:

```slate
define: #Fruit &parents: {Cloneable}.
Fruit traits define: #Apple -> Fruit clone.
Fruit traits define: #Banana -> Fruit clone.
Fruit traits define: #Cherry -> Fruit clone.
```


As labels for primitive values:

```slate
define: #Apple -> 1.
define: #Banana -> 2.
define: #Cherry -> 3.
```


As a namespace:

```slate
ensureNamespace: #fruit &slots: {#Apple -> 1. #Banana -> 2. #Cherry -> 3}.
```


Using a dictionary:

```slate
define: #fruit &builder: [{#Apple -> 1. #Banana -> 2. #Cherry -> 3} as: Dictionary].
```


## Standard ML


```sml
datatype fruit =
  Apple
| Banana
| Cherry
```



## Swift


```swift
enum Fruit {
    case Apple
    case Banana
    case Cherry
}
// or
enum Fruit {
    case Apple, Banana, Cherry
}

enum Season : Int {
    case Winter = 1
    case Spring = 2
    case Summer = 3
    case Autumn = 4
}
```



## Tcl

It is normal in Tcl to use strings from a set directly rather than treating them as an enumeration, but enumerations can be simulated easily. The following elegant example comes straight from the [[http://wiki.tcl.tk/1308  Tcl wiki:]]


```tcl
proc enumerate {name values} {
    interp alias {} $name: {} lsearch $values
    interp alias {} $name@ {} lindex $values
}
```


it would be used like this:


```tcl
enumerate fruit {apple blueberry cherry date elderberry}

fruit: date
#   ==> prints "3"
fruit@ 2
#   ==> prints "cherry"
```



## Toka

Toka has no data types, and therefore no actual enumeration type. There is an optional library function which does provide a way to create enumerated values easily though.

This library function takes a starting value and a list of names as shown in the example below.


```toka
needs enum
0 enum| apple banana carrot |
10 enum| foo bar baz |
```



## VBA

Like Visual Basic .NET, actually:

```vb

'this enumerates from 0
Enum fruits
  apple
  banana
  cherry
End Enum

'here we use our own enumeration
Enum fruits2
  pear = 5
  mango = 10
  kiwi = 20
  pineapple = 20
End Enum


Sub test()
Dim f As fruits
  f = apple
  Debug.Print "apple equals "; f
  Debug.Print "kiwi equals "; kiwi
  Debug.Print "cherry plus kiwi plus pineapple equals "; cherry + kiwi + pineapple
End Sub

```

```txt

test
apple equals  0
kiwi equals  20
cherry plus kiwi plus pineapple equals  42

```



## Visual Basic .NET


```vbnet
' Is this valid?!
Enum fruits
apple
banana
cherry
End Enum

' This is correct
Enum fruits
apple = 0
banana = 1
cherry = 2
End Enum
```



## XPL0


```XPL0
def \Fruit\ Apple, Banana, Cherry;      \Apple=0, Banana=1, Cherry=2
def     Apple=1, Banana=2, Cherry=4;

```



## zkl

Enums as thing or type is not supported but they can be simulated by running code at parse time (ie modify the AST before the compiler compiles it).

```zkl
const RGB_COLOR{  // put color names in a name space
   const RED =0xf00;
   const BLUE=0x0f0, GREEN = 0x00f;
   const CYAN=BLUE + GREEN;  // → 0x0ff
}
println(RGB_COLOR.BLUE);
```

```txt
240
```



```zkl
const X0=N;        // --> 0
const A=N,B=N,C=N; // --> 1,2,3
const{ _n=-1; }    // reset Enum, this should be a const space function
const X=N;	   // -->0
```

Since const space runs at a different time [vs compile space], you need to really careful if you mix the two [spaces]:

```zkl
#continuing ...
z:=N;      // -->2 NOT 1 as it is set AFTER Y (compile time vs parse time)
const Y=N; // -->1! because it is set before z
```



## zonnon


```zonnon

module Enumerations;
type
	Fruits = (apple,banana,cherry);

var
	deserts,i: Fruits;

begin
		deserts := Fruits.banana;
		writeln("ord(deserts): ",integer(deserts):2);

		for i := Fruits.apple to Fruits.cherry do
			writeln(integer(i):2)
		end
end Enumerations.

```

```txt

ord(deserts):  1
 0
 1
 2

```


{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have user-defined data structures. -->
{{omit from|ZX Spectrum Basic}} <!-- It is not worthwhile enumerating, because variable names are single letters -->
