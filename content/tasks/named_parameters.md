+++
title = "Named parameters"
description = ""
date = 2019-08-18T15:51:15Z
aliases = []
[extra]
id = 4451
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
+++

## Task

Create a function which takes in a number of arguments which are specified by name rather than (necessarily) position, and show how to call the function. If the language supports reordering the arguments or optionally omitting some of them, note this.

'''Note:'''
: Named parameters relies on being able to use the names given to function parameters when the function is defined, when assigning arguments when the function is called.
: For example, if a function were to be defined as <code>define func1( paramname1, paramname2)</code>; then it could be called normally as <code>func1(argument1, argument2)</code> and in the called function paramname1 would be associated with argument1 and paramname2 with argument2.
: <code>func1</code> '''must also be able to be called in a way that visually binds each parameter to its respective argument, irrespective of argument order''', for example: <code>func1(paramname2=argument2, paramname1=argument1)</code> which ''explicitly'' makes the same parameter/argument bindings as before.
: Named parameters are often a feature of languages used in safety critical areas such as [[wp:Verilog|Verilog]] and [[wp:VHDL|VHDL]].

'''See also:'''
* [[Varargs]]
* [[Optional parameters]]
* [[wp:Named parameter|Wikipedia: Named parameter]]





## Ada

All callable entities (procedures, functions, entries) require named arguments. All of them can be called using either positional or keyed association of the actual arguments. The arguments supplied with default values can be omitted.

```Ada
procedure Foo (Arg_1 : Integer; Arg_2 : Float := 0.0);
```

It can be equivalently called as:

```Ada
Foo (1, 0.0);
Foo (1);
Foo (Arg_2 => 0.0, Arg_1 => 1);
Foo (Arg_1 => 1);
```



## ALGOL 68

Note: this method of implementing Named Parameters is experimental and to date has never been used outside of rosettacode.org.

```algol68
#!/usr/local/bin/a68g --script #

MODE OPTNAME = STRUCT(STRING name),
     OPTSPECIES = STRUCT(STRING species),
     OPTBREED = STRUCT(STRING breed),
     OWNER=STRUCT(STRING first name, middle name, last name);

# due to the Yoneda ambiguity simple arguments must have an unique operator defined #
OP NAME = (STRING name)OPTNAME: (OPTNAME opt; name OF opt := name; opt),
   SPECIES = (STRING species)OPTSPECIES: (OPTSPECIES opt; species OF opt := species; opt),
   BREED = (STRING breed)OPTBREED: (OPTBREED opt; breed OF opt := breed; opt);

PROC print pet = ([]UNION(OPTNAME,OPTSPECIES,OPTBREED,OWNER) option)VOID: (
  STRING name:="Rex", species:="Dinosaur", breed:="Tyrannosaurus"; # Defaults #
  OWNER owner := ("George","W.","Bush");
  FOR i TO UPB option DO
    CASE option[i] IN
      (OPTNAME option): name := name OF option,
      (OPTSPECIES option): species := species OF option,
      (OPTBREED option): breed := breed OF option,
      (OWNER option): owner := option
    ESAC
  OD;
  printf(($gx$,"Details: a",breed,species,"named",name,"owned by",owner,$l$))
);

print pet((NAME "Mike", SPECIES "Dog", BREED "Irish Setter", OWNER("Harry", "S.", "Truman")));
print pet(())
```

Output:

```txt

Details: a Irish Setter Dog named Mike owned by Harry S. Truman
Details: a Tyrannosaurus Dinosaur named Rex owned by George W. Bush

```


## AppleScript

AppleScript does not implement default or optional parameters, but they can be simulated using records.

```AppleScript
on getName(x)
	set {firstName, lastName} to {"?", "?"}
	try
		set firstName to x's firstName
	end try
	try
		set lastName to x's lastName
	end try
end getName
```

Examples:

```AppleScript
getName({firstName:"John", lastName:"Doe"})
--> Returns: "John, Doe"
getName({lastName:"Doe"})
--> Returns: "?, Doe"
```



## Applesoft BASIC

Function definitions in Applesoft BASIC using the DEF FN statement can only have one parameter.  Subroutines and functions with many parameters can be simulated using global variables which are effectively named parameters.  Default or optional parameters can be simulated with a check in the subroutine.


```ApplesoftBasic
 100  IF LAST$ = "" THEN  PRINT "?";
 110  IF LAST$ <  > "" THEN  PRINT LAST$;
 120  IF FIRST$ <  > "" THEN  PRINT ", "FIRST$;
 200 FIRST$ = ""
 210 LAST$ = ""
 220  RETURN
```



```txt
]FIRST$ = "JOHN" : GOSUB 100"PRINT NAME
?, JOHN
]LAST$ = "DOE" : GOSUB 100"PRINT NAME
DOE
]FIRST$ = "JOHN" : LAST$ = "DOE" : GOSUB 100"PRINT NAME
DOE, JOHN
```



## AutoHotkey

AutoHotkey doesn't have named parameters, but they can be simulated as follows.
ahk [http://www.autohotkey.com/forum/viewtopic.php?p=280499 discussion]

```AutoHotkey
MyFunc( "Val=0, w=1024, Text=The Quick Brown Fox, newVar=I'm New" )

MyFunc( _overrides="" ) {
 Static x=5, y=5, w=100, h=100, Count
 Name:="AutoHotkey", Type:="Scripting", Text:="qwerty", Val:=True

 Loop, Parse, _overrides,`,=, %A_Space%  ; Override routine for Local/Static variables
   A_Index & 1  ? (_:=A_LoopField) : (%_%:=A_LoopField)

Listvars
 WinWaitClose, %A_ScriptFullPath%
}
```



## Bracmat

In Bracmat, all functions have exactly one argument, called "arg". To split the argument and assign values to local variables, you always have to use pattern matching.
In this task, the pattern is just made a bit more complex than would have been the case without explicit parameter names in the argument.

```bracmat
( ( testproc
  =   i x y z
    .   out$"Calling testproc"
      & (=~):(=?i:?x:?y:?z)     { initialise variables to 'failure' }
      &   !arg
        : (? (i,?i) ?|?)        { if "i" found, assign value to i. Otherwise just match with no side effect. }
        : (? (x,?x) ?|?)        { if "x" found, assign value to x. Otherwise just match with no side effect. }
        : (? (y,?y) ?|?)        { likewise }
        : (? (z,?z) ?|?)        { likewise }
      & (~!i|put$("   i:=" !i)) { if variable doesn't fail, show its value }
      & (~!x|put$("   x:=" !x))
      & (~!y|put$("   y:=" !y))
      & (~!z|put$("   z:=" !z))
      & put$\n                  { add new line }
  )
& testproc$((x,1) (y,2) (z,3))
& testproc$((x,3) (y,1) (z,2))
& testproc$((z,4) (x,2) (y,3))  { order is not important }
& testproc$((i,1) (y,2) (z,3))
);
```

Output:

```txt
Calling testproc
   x:= 1   y:= 2   z:= 3
Calling testproc
   x:= 3   y:= 1   z:= 2
Calling testproc
   x:= 2   y:= 3   z:= 4
Calling testproc
   i:= 1   y:= 2   z:= 3
```



## C

C has no direct support for named parameters to functions, but it does permit named parameters to struct initializers. These can be used to simulate named function parameters:

```c
#include <stdio.h>

// 1. Named parameters

typedef struct { int x, y, z; } FTest_args;

void FTest (FTest_args args) {
	printf("x: %d, y: %d, z: %d\n", args.x, args.y, args.z);
}

#define FT(...) FTest((FTest_args){ __VA_ARGS__ })


// 2. Default parameters

#define DFT(...) FTest((FTest_args){ .x = 142, .y = 143, .z = 144, __VA_ARGS__ })


// 3. Convenience wrapper to avoid accessing args as "args.name"

void FTest2 (int x, int y, int z) {
	printf("x: %d, y: %d, z: %d\n", x, y, z);
}

static inline void FTest2_default_wrapper (FTest_args args) {
	return FTest2(args.x, args.y, args.z);
}

#define DF2(...) FTest2_default_wrapper((FTest_args){ .x = 142, .y = 143, .z = 144, __VA_ARGS__ })


int main(int argc, char **argv)
{
	// Named parameters
	FTest((FTest_args){ .y = 10 });
	FTest((FTest_args){ .y = 10, .z = 42 });
	FT( .z = 47, .y = 10, .x = 42 );

	// Default parameters
	DFT();
	DFT( .z = 99 );

	// Default parameters with wrapper
	DF2();
	DF2( .z = 99 );

	return 0;
}
```


As a bonus, default values for the parameters left unspecified can be either implicitly set to zero by the struct initializer, or supplied in a wrapper-macro definition. But this is not idiomatic C by any stretch.


## C++

C++ does not support name parameters, but they can be simulated with the "Named Parameter Idiom" (http://www.parashift.com/c++-faq-lite/ctors.html#faq-10.18).

You wrap the parameters in a class, and make the function a friend of the parameter class.


```cpp
class foo_params{
	friend void foo(foo_params p);
public:
    foo_params(int r):
        required_param_(r),
	optional_x_(0),
	optional_y_(1),
	optional_z_(3.1415)
	{}
     foo_params& x(int i){
	optional_x_=i;
	return *this;
     }
     foo_params& y(int i){
	optional_y_=i;
	return *this;
     }
     foo_params& z(float f){
	optional_z_=f;
	return *this;
     }
private:
        int 	required_param_;
	int 	optional_x_;
	int 	optional_y_;
	float 	optional_z_;
};
```


Declare the function to take the parameter class as its only parameter.


```cpp
void foo(foo_params p){ . . .}
```


Call the function using the parameter object constructor with the required parameters and chaining the optional parameters.


```cpp
foo(foo_params(42).x(7).z(23.54));
```



If you want real named parameters you can use The Boost Parameter Library.

```cpp
#include <boost/parameter/name.hpp>
#include <boost/parameter/preprocessor.hpp>
#include <string>

BOOST_PARAMETER_NAME(foo)
BOOST_PARAMETER_NAME(bar)
BOOST_PARAMETER_NAME(baz)
BOOST_PARAMETER_NAME(bonk)

BOOST_PARAMETER_FUNCTION(
    (int),  // the return type of the function, the parentheses are required.
    function_with_named_parameters, // the name of the function.
    tag,  // part of the deep magic. If you use BOOST_PARAMETER_NAME you need to put "tag" here.
    (required // names and types of all required parameters, parentheses are required.
        (foo, (int))
        (bar, (float))
    )
    (optional // names, types, and default values of all optional parameters.
        (baz, (bool) , false)
        (bonk, (std::string), "default value")
    )
)
{
    if (baz && (bar > 1.0)) return foo;
    return bonk.size();
}
```


Once the definition is written, using it is easy, by name or position.


```cpp
function_with_named_parameters(1, 10.0);
function_with_named_parameters(7, _bar = 3.14);
function_with_named_parameters( _bar = 0.0, _foo = 42);
function_with_named_parameters( _bar = 2.5, _bonk= "Hello", _foo = 9);
function_with_named_parameters(9, 2.5, true, "Hello");
```


## C#

Named parameters were added in C# 4.0.  The examples below demonstrate how named parameters and optional parameters are a single concept in the language.


```c#
using System;

namespace NamedParams
{
    class Program
    {
        static void AddWidget(string parent, float x = 0, float y = 0, string text = "Default")
        {
            Console.WriteLine("parent = {0}, x = {1}, y = {2}, text = {3}", parent, x, y, text);
        }

        static void Main(string[] args)
        {
            AddWidget("root", 320, 240, "First");
            AddWidget("root", text: "Origin");
            AddWidget("root", 500);
            AddWidget("root", text: "Footer", y: 400);
        }
    }
}
```


Output:


```txt
parent = root, x = 320, y = 240, text = First
parent = root, x = 0, y = 0, text = Origin
parent = root, x = 500, y = 0, text = Default
parent = root, x = 0, y = 400, text = Footer
```



## Clojure


Clojure doesn't have built-in support for named or keyword arguments, but you can use destructuring to achieve a similar effect.


```clojure
(defn foo [& opts]
  (let [opts (merge {:bar 1 :baz 2} (apply hash-map opts))
        {:keys [bar baz]} opts]
    [bar baz]))
```


Clojure 1.2 supports destructuring of trailing arguments as a map:


```clojure
(defn foo [& {:keys [bar baz] :or {bar 1, baz 2}}]
  [bar baz])
```


You can also use <code>defnk</code> from <code>clojure.contrib.def</code>, which is a macro that works similarly.


```clojure
(use 'clojure.contrib.def)
(defnk foo [:bar 1 :baz 2]
  [bar baz])
```


Sample output for all variants:


```txt
user> (foo :baz 123)
[1 123]

user> (foo :bar 456)
[456 2]
```


## Common Lisp



```lisp
(defun print-name (&key first (last "?"))
  (princ last)
  (when first
    (princ ", ")
    (princ first))
  (values))
```


<code>&key</code> indicates that further parameters are named (keyword parameters); a bare symbol (e.g. <var>first</var>) has a default of NIL, whereas a list (e.g. <code>(<var>last</var> "?")</code>) gives the variable name and the default value (which is evaluated when the function is called, unlike Python).


```lisp
CL-USER> (print-name)
?
CL-USER> (print-name :first "John")
?, John
CL-USER> (print-name :first "John" :last "Doe")
Doe, John
CL-USER> (print-name :last "Doe")
Doe
```


In Common Lisp, the arguments to a function are always a simple list of values; the <code>&key</code> facility merely defines an interpretation of that list by the function: alternating keys and values. (As a result of this, mixing [[varargs]] (<code>&rest</code>) with named parameters is not recommended as it requires some additional means of distinguishing them. On the other hand, functions which pass their arguments on to other functions need not handle named arguments distinctly.)

## Dyalect


Dyalect supports both named and optional parameters.


```dyalect
func fun(x, y = 0, z = 12.2, dsc = "Useless text") {
    print("x=\(x), y=\(y), z=\(z), dsc=\(dsc)")
}

fun(12, z: 24.4, dsc: "Foo", y: 3)
fun(y: 42, x: 12)
```


```txt
x=12, y=3, z=24.4, dsc=Foo
x=12, y=42, z=12.2, dsc=Useless text
```



## Elixir


The easiest and visually most appealing way to have named parameters is using a proplist as last parameter of the function.


```Elixir

def fun(bar: bar, baz: baz), do: IO.puts "#{bar}, #{baz}."

fun(bar: "bar", baz: "baz")

```


For this way to use them, order matters, as well as you can't define default values for arguments.


## Erlang

At a guess "named parameters" are supposed to be used like proplists in Erlang.

```Erlang

Fun = fun( Proplists ) ->
    Argument1 = proplists:get_value( argument1, Proplists, 1 ),
    Kalle = proplists:get_value( kalle, Proplists, "hobbe" ),
    io:fwrite( "~p ~s~n", [Argument1, Kalle] )
end.

```


The function can now be called like these examples.

```txt

25> Fun( [] ).
1 hobbe
26> Fun( [{argument1, 99}] ).
99 hobbe
27> Fun( [{kalle, "gustav"}, {argument1, 9}] ).
9 gustav

```



## E


Since E supports arbitrary pattern matching (in the sense of [[Pattern Matching]] in parameter lists, a map-pattern can be used to provide named parameters, though the syntax is sufficiently noisy that this is not used casually.


```e
def printName([=> first := null, => last := null]) {
    if (last == null) {
        print("?")
    } else {
        print(last)
    }
    if (first != null) {
        print(", ")
        print(first)
    }
}
```


(Note: In map literals and map patterns, “<code>=> <var>x</var></code>” is shorthand for “<code>"<var>x</var>" => <var>x</var></code>”.)


```e
? printName(["first" => "John"])
?, John

? printName(["last" => "Doe"])
Doe

? printName(["first" => "John", "last" => "Doe"])
Doe, John
```



## Factor


Named parameters in Factor are as simple as changing the <code>:</code> word to <code>::</code> and using the variables defined in the stack effect declaration.


```Factor

:: my-named-params ( a b -- c ) a b * ;

```



## Forth

As with many of the other languages here, Forth does not have named parameters, but it does have practices that arise in situations where a Forth programmer might want named parameters.  In Forth's case, these practices also arise when a more natural control language is wanted (e.g., LEFT ARM 36 DEGREES LIFT) or when a word would otherwise take excessively many arguments.

Two regrettably unnatural examples:

```forth
256 buffer: first-name
256 buffer: last-name
: is ( a "name" -- )  parse-name rot place ;

: greet ( -- )
  cr ." Hello, " first-name count type space  last-name count type ." !" ;

first-name is Bob  last-name is Hall  greet


require mini-oof2.fs
require string.fs
object class
  field: given-name
  field: surname
end-class Person

: hiya ( -- )
  cr ." Hiya, " given-name $. space surname $. ." !" ;

Person new >o s" Bob" given-name $!  s" Hall" surname $!  hiya o>
```


```txt
Hello, Bob Hall!
Hiya, Bob Hall!
```



## Fortran

Fortran accepts named parameter and optional parameter. Arguments are always named: if the name is omitted, the order used in the definition of the function / subroutine must be used.


```fortran
subroutine a_sub(arg1, arg2, arg3)
  integer, intent(in) :: arg1, arg2
  integer, intent(out), optional :: arg3
  ! ...
end subroutine a_sub
```



```fortran
! usage
  integer :: r
  call a_sub(1,2, r)
  call a_sub(arg2=2, arg1=1)
```


The presence of an optional argument can be tested with <tt>PRESENT</tt>; if optional arguments come before a non optional argument, the usage of the name of the argument is mandatory.


```fortran
subroutine b_sub(arg1, arg2)
   integer, intent(in), optional :: arg1
   integer, intent(in) :: arg2
   !...
end subroutine b_sub
```



```fortran
call b_sub(1)              ! error: missing non optional arg2
call b_sub(arg2=1)         ! ok
call b_sub(1, 2)           ! ok: arg1 is 1, arg2 is 2
call b_sub(arg2=2, arg1=1) ! the same as the previous line
```



## Go

Although Go doesn't support named parameters as such, they can be simulated using a struct with fields corresponding in name and type to the desired parameters.

A struct literal can then be passed to the function labelling the individual fields with their names. The fields need not appear in the same order as they are declared and, if one or more are omitted, they are given their 'zero' values.

Here's a simple example.

```go
package main

import (
   "fmt"
)

type params struct {x, y, z int}

func myFunc(p params) int {
    return p.x + p.y + p.z
}

func main() {
    r := myFunc(params{x: 1, y: 2, z: 3}) // all fields, same order
    fmt.Println("r =", r)
    s := myFunc(params{z: 3, y: 2, x: 1}) // all fields, different order
    fmt.Println("s =", s)
    t := myFunc(params{y: 2})             // only one field, others set to zero
    fmt.Println("t =", t)
}
```


```txt

r = 6
s = 6
t = 2

```



## Haskell

We can simulate named, but not moveable arguments, with nullary data constructors:


```Haskell
data X = X
data Y = Y
data Point = Point Int Int deriving Show

createPointAt :: X -> Int -> Y -> Int -> Point
createPointAt X x Y y = Point x y

main = print $ createPointAt X 5 Y 3
```


We can also emulate named, moveable, optional arguments with record syntax:


```Haskell
data Point = Point {x, y :: Int} deriving Show
defaultPoint = Point {x = 0, y = 0}

createPointAt :: Point -> Point
createPointAt = id
main = print $ createPointAt (defaultPoint { y = 3, x = 5 })
```


Though this is cumbersome without using template Haskell, as the call site must supply the defaults.

=={{header|Icon}} and {{header|Unicon}}==

Icon and Unicon do not support named parameters.  There are a couple of approaches that could be adapted to provide this kind of functionality.  The basic considerations are:
* writing a string like "parm1=value" would be limiting as the value would have to be parsed and this form would be challenged to represent all data types
* using two parameters like this (...,"parm1:=",x,"parm2:=",y,...) removes this limitation

The test procedure below includes a list of valid parameter names to check against



```Icon
procedure main()
   testproc("x:=",1,"y:=",2,"z:=",3)
   testproc("x:=",3,"y:=",1,"z:=",2)
   testproc("z:=",4,"x:=",2,"y:=",3)
   testproc("i:=",1,"y:=",2,"z:=",3)
end

procedure testproc(A[])   #: demo to test named parameters
   write("Calling testproc")

   while a := get(A) do               # implement named parameters here
      (( a ? (v := =!["x","y","z"], =":=")  |   #  valid parameter name?
         stop("No parameter ",a)) &             #  . . no
            ((variable(a[1:-2]) := get(A))  |   #  assign
               runerr(205,a)))                  #  . . problem

   write("   x:=",x)
   write("   y:=",y)
   write("   z:=",z)
end
```

Output:
```txt
Calling testproc
   x:=1
   y:=2
   z:=3
Calling testproc
   x:=3
   y:=1
   z:=2
Calling testproc
   x:=2
   y:=3
   z:=4
Calling testproc
No parameter i:=
```



## J


J is similar to Perl in that all arguments to functions come in as separate elements in an array.  But it is possible to emulate more complex calling conventions.  For example, using the [http://www.jsoftware.com/svn/DanBron/trunk/environment/calling_convention.ijs calling convention J script], one could write:


```j
NB.  Strand notation
myFunc['c:\file.txt'  906  'blue' fs]

NB.  Commas, like other langs
myFunc['c:\file.txt', 906, 'blue' fs]

NB.  Unspecified args are defaulted ("optional")
myFunc['c:\file.txt' fs]

NB.  Can use named arguments, like eg VB
myFunc[color='blue'  fs]

NB.  Often values needn't be quoted
myFunc[color= blue   fs]

NB.  Combination of comma syntax and name=value
myFunc[max=906, color=blue fs]

NB.  Spelling of names is flexible
myFunc[MAX=906, COLOR=blue fs]

NB.  Order of names is flexible
myFunc[COLOR=blue, MAX=906  fs]

NB.  Even the delimiters are flexible...
myFunc<MAX=906, COLOR=blue fs>
```


For further discussion, see the [http://www.jsoftware.com/pipermail/programming/2009-July/015571.html corresponding thread in the J Forums].


## Java

Like C++, Java also does not support named parameters. Named parameters can however be simulated simply with the "Builder pattern". ([http://drdobbs.com/java/208403883?pgno=2 Joshua Bloch: Builder Pattern])

```java
processNutritionFacts(new NutritionFacts.Builder(240, 8).calories(100).sodium(35).carbohydrate(27).build());
```

Follow the link for extra details about the 'NutritionFacts' class example.


## JavaScript

JavaScript only has positional parameters, but named parameters can be emulated by passing an object with the appropriate properties:

```javascript
function example(options) {
  // assign some defaults where the user's has not provided a value
  opts = {}
  opts.foo = options.foo || 0;
  opts.bar = options.bar || 1;
  opts.grill = options.grill || 'pork chops'

  alert("foo is " + opts.foo + ", bar is " + opts.bar + ", and grill is " + opts.grill);
}

example({grill: "lamb kebab", bar: 3.14});
// => "foo is 0, bar is 3.14, and grill is lamb kebab"
```

===ECMAScript 2015 (ES6) variants===
With this version, ECMAScript adds destrucuring assignments and destructuring in function parameters. Thus you could do something like this (this works in ES6 Fiddle, but is syntax error in Mozilla SpiderMonkey JS Shell, so uses console.log instead of print):
```javascript
let
  example = // The member name in the object can either be the same as the parameter (as in bar, grill),
            // or a different parameter name as in the case of member foo being assigned to parameter a here.
    ({foo: a=0, bar=1, grill='pork chops'}={}) => (
      console.log('foo is ',a,', bar is ',bar,', and grill is '+grill));

example();
//  foo is 0 , bar is 1 , and grill is pork chops
example({grill: "lamb kebab", bar: 3.14});
//  foo is 0 , bar is 3.14 , and grill is lamb kebab
example({foo:null});
//  foo is , bar is 1 , and grill is pork chops
```



## jq

jq does not strictly speaking support named function arguments, but since jq is JSON-oriented, it is possible to achieve the desired effect by using JSON objects.

For example, here is the jq analog of "print-name" defined in the Common Lisp section above. Here we format the full name and return it as a string:

```jq

def formatName(obj):
  ({ "name": "?"} + obj) as $obj  # the default default value is null
  | ($obj|.name) as $name
  | ($obj|.first) as $first
  | if ($first == null) then $name
    else $name + ", " + $first
    end;

```


Here are examples of how the function can be invoked:

```jq

formatName({"first": "George", "name": "Eliot"})

formatName({"name": "Eliot", "first": "George"})

formatName({"name": "Eliot"})

formatName({"first": "George"})

formatName({})

```



## Julia

Julia supports arbitrary named keyword arguments, which are listed (with their default values) after a <code>;</code> in the function definition:

```Julia
function surround(string ; border = :default, padding = 0)

 ve, ho, ul, ur, dl, dr =
   border == :round ? ("\u2502","\u2500","\u256d","\u256e","\u2570","\u256f") :
   border == :bold  ? ("\u2503","\u2501","\u250F","\u2513","\u2517","\u251b") :
   border == :double? ("\u2551","\u2550","\u2554","\u2557","\u255a","\u255d") :
   border == :dotted? ("\u254e","\u254c","\u250c","\u2510","\u2514","\u2518") :
   border == :cross ? ("\u2502","\u2500","\u253c","\u253c","\u253c","\u253c") :
                      ("\u2502","\u2500","\u250c","\u2510","\u2514","\u2518")

 println(ul, ho^(length(string) + 2padding),  ur, "\n",
         ve, " "^padding, string," "^padding, ve, "\n",
         dl, ho^(length(string) + 2padding),  dr)
end
```


```txt
julia> surround("♡", border = :round)
╭─╮
│♡│
╰─╯

julia> surround("Julia", padding = 1, border = :double)
╔═══════╗
║ Julia ║
╚═══════╝
```



## Kotlin


```scala
// version 1.0.6

fun someFunction(first: String, second: Int = 2, third: Double) {
    println("First = ${first.padEnd(10)}, Second = $second, Third = $third")
}

fun main(args: Array<String>) {
    // using positional parameters
    someFunction("positional", 1, 2.0)

    // using named parameters
    someFunction(first = "named", second = 1, third = 2.0)

    // omitting 2nd parameter which is optional because it has a default value
    someFunction(first = "omitted", third = 2.0)

    // using first and third parameters in reverse
    someFunction(third = 2.0, first = "reversed")
}
```


```txt

First = positional, Second = 1, Third = 2.0
First = named     , Second = 1, Third = 2.0
First = omitted   , Second = 2, Third = 2.0
First = reversed  , Second = 2, Third = 2.0

```



## LabVIEW

Function calls in LabVIEW are implemented as instantiated VIs, small icons representing the function to be called. All parameters are positional, by drawing wires to different points along the icon edge. By right-clicking on the VI icon and unchecking "View as Icon", the parameter names can be shown. The parameter names are mainly informational for the user.

This image shows a VI with "View as Icon" unchecked.
<br/>

## Lasso


```Lasso
define mymethod(
	-first::integer, // with no default value the param is required
	-second::integer,
	-delimiter::string = ':' // when given a default value the param becomes optional
) => #first + #delimiter + #second

mymethod(
	-first = 54,
	-second = 45
)
'<br />'
mymethod(
	-second = 45, // named params can be given in any order
	-first = 54,
	-delimiter = '#'
)
```

-> 54:45

54#45


## Lingo

Lingo does not support named function parameters. But this can be simulated by using a single property list (hash) with named properties as function argument. You can also create functions that support both calling methods, like e.g. this function that accepts either 3 integers or a single property list with such named properties:

```lingo
-- accepts either 3 integers or a single property list
on foo (arg1, arg2, arg3)
  if ilk(arg1)=#propList then
    args = arg1
    arg1 = args[#arg1]
    arg2 = args[#arg2]
    arg3 = args[#arg3]
  end if
  put "arg1="&arg1
  put "arg2="&arg2
  put "arg3="&arg3
end

foo(1,2) -- 3rd argument omitted
-- "arg1=1"
-- "arg2=2"
-- "arg3="

foo([#arg3:3]) -- only 3rd argument specified
-- "arg1="
-- "arg2="
-- "arg3=3"
```



## Lua


```Lua

function CreatePet(options)
  local name=options.name
  local species=options.species
  local breed=options.breed
  print('Created a '..breed..' '..species..' named '..name)
end
CreatePet{name='Rex',species='Dog',breed='Irish Setter'}
--position does not matter here.

```



## M2000 Interpreter

We can use named parameters for modules only. Modules are like functions but can't be called from expressions. We can use current stack to return values using Push statement. Here we define type and we set values to make them optionals.
Passing optionals in modules may cause problems if we have values in stack, so we can use Stack New {] to open an empty current stack (the old one is hidden until exit from that block), or using ? as "use standard value".


```M2000 Interpreter

module namedparam (x as decimal=10, y as integer=50) {
      Print type$(x), x
      Print type$(y), y
}
namedparam 10, 20
namedparam  ?, ?
Push 1, 2 : namedparam
Stack New {
      \\ it is empty
      namedparam
      namedparam  %y=500
      namedparam  %x=20
}
namedparam %x=1, %y=1

```



## Maple


```maple
f := proc(a, {b:= 1, c:= 1})
    print (a*(c+b));
end proc:
#a is a mandatory positional parameter, b and c are optional named parameters
f(1);#you must have a value for a for the procedure to work
                                                                                 2
f(1, c = 1, b = 2);
                                                                                 3
f(2, b = 5, c = 3);#b and c can be put in any order
                                                                                16
```



## Mathematica


```Mathematica
Options[fn]={Add->False,Offset-> 0};
fn[x_,y_,OptionsPattern[]]:=If[OptionValue[Add]==True,x+y+OptionValue[Offset],{x,y,OptionValue[Offset]}]

fn[3,4,{Add->True,Offset->2}]
->9
fn[3,4,{Offset->2,Add->True}]
->9
```


=={{header|MATLAB}} / {{header|Octave}}==

Named parameters are not natively supported. However, the following code can be used to implement them.


```Matlab
   function foo(varargin)
      for k= 1:2:length(varargin);
         switch (varargin{k})
         case {'param1'}
            param1 = varargin{k+1};
         case {'param2'}
            param2 = varargin{k+1};
	 end;
      end;
      printf('param1: %s\n',param1);
      printf('param2: %s\n',param2);
   end;

   foo('param1','a1','param2','b2');
   foo('param2','b2','param1','a1');
```


Output:

```txt
>>    foo('param1','a1','param2','b2');
param1: a1
param2: b2
>>    foo('param2','b2','param1','a1');
param1: a1
param2: b2
```



=={{header|Modula-3}}==
Much like [[Ada]], Modula-3 allows either positional or keyed association of actual parameters. Defaults can also be ignored.


```modula3
PROCEDURE Foo(Arg1: INTEGER; Arg2: REAL := 0.0);
```

It can be equivalently called as:

```modula3
Foo(1, 0.0);
Foo(1);
Foo(Arg2 := 0.0, Arg1 := 1);
Foo(Arg1 := 1);
```



## Nemerle


```Nemerle
Foo(number : int, word = "Default", option = true) : void // note type inference with default values

Foo(word = "Bird", number = 3)        // an argument with a default value can be omitted from function call
Foo(3, option = false, word = "Bird") // unnamed arguments must be in same order as function definition and precede named arguments

```



## Nim

In Nim, a regular parameter of a procedure can be used as either a positional or a named parameter.


```nim
proc subtract(x, y): auto = x - y

echo subtract(5, 3)         # used as positional parameters
echo subtract(y = 3, x = 5) # used as named parameters
```


=={{header|Objective-C}}==
Objective-C, like Smalltalk, has a method call syntax that is visually identical to named arguments, but they are not optional and may not be reordered. (Optional arguments may be simulated by defining multiple methods with the same leading name part.)

```objc
@interface Demo : NSObject {
    // Omitted ...
}

- (double) hypotenuseOfX: (double)x andY: (double)y;
- (double) hypotenuseOfX: (double)x andY: (double)y andZ: (double)z;

@end
```


```objc
@implementation Demo

- (double) hypotenuseOfX: (double)x andY: (double)y {
    return hypot(x,y);
}
- (double) hypotenuseOfX: (double)x andY: (double)y andZ: (double)z {
    return hypot(hypot(x, y), z);
}

@end
```


```objc
Demo *example = [[Demo alloc] init];
double h = [example hypotenuseOfX:1.23 andY:3.79];
```

Note in the example above that the name of the method, the ''selector''; is actually “<code>hypotenuseOfX:andY:</code>”.


## OCaml

You can make a named argument (called ''labels'' in OCaml) by putting a tilde (~) before the name:

```ocaml
# let foo ~arg1 ~arg2 = arg1 + arg2;;
val foo : arg1:int -> arg2:int -> int = <fun>
# let foo ~arg1:x ~arg2:y = x + y;; (* you can also use different variable names internally if you want *)
val foo : arg1:int -> arg2:int -> int = <fun>
# foo ~arg2:17 ~arg1:42;;
- : int = 59
```


Named arguments can be re-ordered, but two arguments of the same label cannot be re-ordered relative to each other.

Named arguments can be curried. If you partially apply a function on a named argument (even if the named argument is not first in the function declaration), it will evaluate to a function of the remaining arguments.

Named arguments can be made optional, with the <code>?(arg = value)</code> syntax in the parameter declaration. See the optional parameters task for more details.


## Oz

For <b>methods</b>, Oz does support named parameters with default values. The named parameters can be freely reordered.

```oz
declare
class Foo
   meth init skip end
   meth bar(PP %% positional parameter
	    named1:N1
	    named2:N2
	    namedWithDefault:NWD <= 42)
      {System.showInfo "PP: "#PP#", N1: "#N1#", N2: "#N2#", NWD: "#NWD}
   end
end

O = {New Foo init}
{O bar(1 named1:2 named2:3 namedWithDefault:4)} %% ok
{O bar(1 named2:2 named1:3)} %% ok
{O bar(1 named1:2)} %% not ok, "missing message feature in object application"
```


For <b>procedures</b> only positional parameters are supported. However, you can emulate named parameters by using records:

```oz

declare
proc {Foo PP Other=unit(named1:N1 named2:N2 ...)}
   NWD = {CondSelect Other namedWithDefault 42}
in
      {System.showInfo "PP: "#PP#", N1: "#N1#", N2: "#N2#", NWD: "#NWD}
end

{Foo 1 unit(named1:2 named2:3 namedWithDefault:4)}
{Foo 1 unit(named2:2 named1:3)}
{Foo 1 unit(named1:2)} %% not ok...
```


The procedure Foo is defined with pattern matching in the argument list. The ellipsis means that additional record fields are allowed. To access optional record fields, we have to explicitly try to select a field and provide a default value in case it is missing ("CondSelect").


## Perl

Perl has no non-experimental formal parameters. Instead, Perl subroutines access all of their arguments through the special array <code>@_</code>. You can easily implement named arguments by making your function interpret <code>@_</code> (or part of it) as a hash.


```perl
sub funkshun {
my %h = @_;
  # Print every argument and its value.
    print qq(Argument "$_" has value "$h{$_}".\n)
        foreach sort keys %h;
  # If a 'verbosity' argument was passed in, print its value
  # whatever that value may be.
    print "Verbosity specified as $h{verbosity}.\n" if exists $h{verbosity};
  # Say that safe mode is on if 'safe' is set to a true value.
  # Otherwise, say that it's off.
    print "Safe mode ", ($h{safe} ? 'on' : 'off'), ".\n";
}
```

The semantics of calling such a function follow directly from the semantics of using a hash. For instance, if you provide multiple values for the same named argument, only the last one will be used. An example call:


```perl
funkshun(
    verbosity   => 3,
    password    => 'foobie blech',
    extra_lives => 3,
    '42'        => 'answer',
    password    => 'joshua'
);
```

Its output:


```txt
Argument "42" has value "answer".
Argument "extra_lives" has value "3".
Argument "password" has value "joshua".
Argument "verbosity" has value "3".
Verbosity specified as 3.
Safe mode off.
```


Further flexibility can be obtained by using [[Pass by reference]] semantics:
```Perl
sub event
{
    my ($params_ref, $name) = @_;
    my %params = %$params_ref;
    my @known_params = qw(attendees event food time);

    printf "%s called event() with the following named parameters:\n",
        $name // 'Anonymous';

    say sort map {
        sprintf "%s: %s\n",
            ucfirst $_,
            ref $params{$_} eq ref []
            ? join ', ', @{ $params{$_} }
            : $params{$_};
    } grep exists $params{$_}, @known_params;
    delete $params{$_} foreach @known_params;

    say "But I didn't recognize these ones:";
    while (my ($key, $val) = each %params)
    {
        say "$key: $val";
    }
}

event(
    {   # Curly braces with no label (e.g. 'sub' before it)
        # create a reference to an anonymous hash
        attendees => ['Bob', 'Betty', 'George', 'Bertha'],
        event     => 'birthday',
        foo       => 'bar',
        food      => 'cake',
        frogblast => 'vent core',
        time      => 3,
    },
    "Joe Schmoe"
);
```
Prints:

```txt

Joe Schmoe called event() with the following named parameters:
Attendees: Bob, Betty, George, Bertha
Event: birthday
Food: cake
Time: 3

But I didn't recognize these ones:
frogblast: vent core
foo: bar

```

This is useful when you want your function to take both named (the hash) and positional (Joe Schmoe's $name) parameters.


## Perl 6

Perl 6's support for optional parameters is much like Python's. Consider this declaration:


```perl6
sub funkshun ($a, $b?, $c = 15, :$d, *@e, *%f) {
   ...
}
```


In the above signature:
* <code>$a</code> is a mandatory parameter accepted by position (<code>funkshun 15, ...</code>).
* <code>$b</code> is an optional parameter that can be passed by position or by name. By default, it's undefined.
* <code>$c</code> is an optional parameter that can be passed by position or by name. Its default value is <code>15</code>.
* <code>$d</code> is an optional parameter that can only be passed by name. By default, it's undefined.
* <code>@e</code> is a slurpy array: it receives any leftover positional arguments.
* <code>%f</code> is a slurpy hash: it receives any leftover named arguments.

So, if we defined the function like this:


```perl6
sub funkshun ($a, $b?, :$c = 15, :$d, *@e, *%f) {
   say "$a $b $c $d";
   say join ' ', @e;
   say join ' ', keys %f;
}

# this particularly thorny call:

funkshun
    'Alfa', k1 => 'v1', c => 'Charlie', 'Bravo', 'e1',
    d => 'Delta', 'e2', k2 => 'v2';
```


would print this:


```txt
Alfa Bravo Charlie Delta
e1 e2
k1 k2
```



## Phix

Phix supports named and optional parameters in a very simple, natural, and intuitive way, erring on the side of caution when faced with any potential ambiguity.

Optional parameters are specified simply by providing a default, any non-defaulted parameters must occur before (to the left of) any defaulted parameters.

Named parameters can be given (when invoking a routine) in any order, but must be grouped together after (to the right of) any non-named parameters.

Note that low-level builtins (those defined using AutoAsm() in psym.e/syminit()) do not support named parameters (maybe one day..), but everything else does.

```Phix
global function timedelta(atom weeks=0, atom days=0, atom hours=0, atom minutes=0, atom seconds=0, atom milliseconds=0, atom microseconds=0)
-- can be invoked as:
constant fourdays = timedelta(days:=4)
--       fourdays = timedelta(0,4) -- equivalent
-- **NB** a plain '=' is a very different thing
constant oneday = timedelta(days=1) -- equivalent to timedelta([weeks:=]iff((equal(days,1)?true:false))
                                    -- - with an error if no local variable days exists.
constant shift = timedelta(hours:=hours) -- perfectly valid (param hours:=local hours)
-- timedelta(0,hours:=15,3) -- illegal (it is not clear whether you meant days:=3 or minutes:=3)
```



## PHP

PHP doesn't support named parameters but you can simulate the behavior with PHP arrays.

```PHP
function named($args) {
  $args += ["gbv" => 2,
            "motor" => "away",
            "teenage" => "fbi"];
  echo $args["gbv"] . " men running " . $args['motor'] . " from the " . $args['teenage'];
}

named(["teenage" => "cia", "gbv" => 10]);
```

Output:

```txt
10 men running away from the cia
```



## PicoLisp

PicoLisp uses normally positional parameters, but
'[http://software-lab.de/doc/refB.html#bind bind]' can be used
to establish bindings to passed names.
===Passing symbol-value pairs===

```PicoLisp
(de foo @
   (bind (rest)  # Bind symbols in CARs to values in CDRs
      (println 'Bar 'is Bar)
      (println 'Mumble 'is Mumble) ) )

(foo '(Bar . 123) '(Mumble . "def"))
```


### Passing a name list followed by values


```PicoLisp
(de foo @
   (bind (next)                # Save all symbols in first argument
      (mapc set (arg) (rest))  # then bind them to remaining arguments
      (println 'Bar 'is Bar)
      (println 'Mumble 'is Mumble) ) )

(foo '(Bar Mumble) 123 "def")
```

Output in both cases:

```txt
Bar is 123
Mumble is "def"
```



## PowerShell


### Positional parameters

When writing a function and not stating any parameters explicitly, such as the following function

```powershell
function Test {
    Write-Host Argument 1 is $args[0]
}
```

the only option are positional parameters using the <code>$args</code> array.

### Named parameters

Stating any number of parameters directly in the function definition, such as

```powershell
function Test ($SomeArgument, $AnotherArgument, $ThirdArgument) {
    Write-Host "Some argument:    $SomeArgument"
    Write-Host "Another argument: $AnotherArgument"
    Write-Host "Third argument:   $ThirdArgument"
}
```

will cause them to be named automatically which enables the caller to state the arguments in any order. The syntax follows the convention used with cmdlets as well:

```txt
PS> Test -ThirdArgument foo -AnotherArgument bar -SomeArgument baz
Some argument:    baz
Another argument: bar
Third argument:   foo
```

However, one can still just give the arguments in order without explicitly having to state the names of the parameters.

Arbitrary arguments can be omitted as well:

```txt
PS> Test -ThirdArgument foo -AnotherArgument bar
Some argument:
Another argument: bar
Third argument:   foo
```

This will cause the omitted arguments to have the value <code>$null</code>

### Switch parameters

Functions can have so-called ''switch parameters'' which are always boolean and either present or not. There is no need to give a value for them.

```powershell
function SwitchTest ([switch] $on) {
    Write-Host Switched $(if ($on) { "on" } else { "off" })
}
```

When calling a function with such a parameter the switch is simply given directly (which sets its value to ''true'') or omitted (which causes it to evaluate to ''false''):

```txt
PS> SwitchTest
Switched off
PS> SwitchTest -on
Switched on
```


### Optional parameters and default values

Usually all parameters can be omitted. In the case of switch parameters this will cause them to assume the value ''false'', for normal parameters they will have the value <code>$null</code>. This is not always the desired value, though. Default values can be given too:

```powershell
function Greeting ($Name = "Nobody") {
    Write-Host Hello, $Name!
}
```

If the <code>Name</code> argument is omitted now, its value will be <code>"Nobody"</code> instead of <code>$null</code>:

```txt
PS> Greeting
Hello Nobody!
PS> Greeting John
Hello John!
```



## Python


### Basic explanation

A more detailed explanation of parameters, arguments, and how they are used is in the sections below. This is a simplified explanation:

In Python, a regular parameter of a function can be used as ''either a positional or a named'' parameter. The variable name that you use for the parameter when you declare the function becomes the "name" for the parameter, should you use it as a named parameter. When you call a function, you use the "name = value" syntax to provide the argument to a named parameter. The named arguments must come after all the positional arguments.


```python
def subtract(x, y):
    return x - y

subtract(5, 3)         # used as positional parameters; evaluates to 2
subtract(y = 3, x = 5) # used as named parameters;      evaluates to 2
```


Parameters can be made optional by providing a default argument, as described in the [[optional parameters]] article.


### Detailed Explanation


### =Function Definition Parameters=

Function definitions in Python allow for the following ''parameter'' types:
* Optional ''default parameter'' types which are explicitly specified by name, and may have an optional default value.
* An optional ''positional parameter'' which is an identifier preceded by <code>"*"</code>.
* And an optional ''keyword parameter'' which is an identifier preceded by <code>"**"</code>.
If any of the parameter types are given then they must appear in the order specified above.

The syntax of function parameter declarations is more formally defined as:

 funcdef        ''::=''  "'''def'''" funcname "'''('''" ''[''parameter_list'']'' "''')'''" "''':'''" suite
 dotted_name    ''::=''  identifier ''(''"'''.'''" identifier'')*''
 parameter_list ''::=''  ''(''defparameter "''','''"'')*''
                     ''(''  posparameter ''[,'' keyparameter'']''
                     ''|'' keyparameter
                     ''|'' defparameter ''[''"''','''"''] )''
 defparameter   ''::=''  parameter ''[''"'''='''" expression'']''
 posparameter   ''::=''  "'''*'''" identifier
 keyparameter   ''::=''  "'''**'''" identifier
 sublist        ''::=''  parameter ''(''"''','''" parameter'')* [''"''','''"'']''
 parameter      ''::=''  identifier ''|'' "'''('''" sublist "''')'''"


### =Function Call Arguments=

The call of a function in python can use the following ''argument'' types:
* ''Positional arguments'' that are mapped by their position in the call argument list to the <code>defparameter</code> name in the corresponding position of the function definition.
* ''Sequence arguments'' that are the character <code>"*"</code> followed by an expression evaluating to a sequence (such as a list or tuple). The values from the sequence are unpacked and mapped like individual positional arguments to <code>defparameter</code>s of the function definition. Sequence arguments are “evaluated before any keyword argument, irrespecctive of their relative positions in an argument list”.
* All positional arguments must appear before any keyword argument.
* ''Keyword arguments'' of the form <code>parameter_name "=" value</code> will map the value to the <code>defparameter</code> in the definition of the same name.
* ''Mapping arguments'' that are the characters <code>"**"</code> followed by an expression evaluating to a mapping (such as a dict/hash). The key, value pairs from the mapping are unpacked and mapped like individual keyword arguments to <code>defparameter</code>s of the function definition.
* If the function ''definition'' includes a ''positional parameter'', then if the assignment of ''positional arguments'' and ''sequence arguments'' in the ''call'' gives more values than the <code>defparameters</code> of the definition, then these extra arguments are assembled, in order, into a tuple that is assigned to the <code>posparameter</code> of the definition.
* If the function ''definition'' includes a ''keyword parameter'', then if the parameter name of any ''keyword arguments'' and ''mapping arguments'' in the ''call'' is unknown in the <code>defparameters</code> of the function definition, then these extra keyword/value pairs are assembled into a dict that is assigned to the <code>keyparameter</code> of the definition.
* Any ''default parameter'' of the function ''definition'' that is not assigned a value at this point, but which has a default value, will be aassigned this default value, without re-evaluating the default value.
* Any  ''default parameter'' of the function ''definition'' that is still un-assigned will cause a <code>TypeError</code> exception to be raised.
* In addition, multiple mappings to any parameter will raise a <code>TypeError</code> exception. (This includes multiple mappings into a <code>keyparameter</code> or keyword arguments clashing with positional/sequence arguments).

The more formal definition of a function call's syntax is

 call                 ''::=''  primary "'''('''" ''[''argument_list ''[''"''','''"'']''
                           ''|'' expression genexpr_for'']'' "''')'''"
 argument_list        ''::=''  positional_arguments ''[''"''','''" keyword_arguments'']''
                             ''[''"''','''" sequence_argument''] [''"''','''" keyword_arguments'']''
                             ''[''"''','''" mapping_argument'']''
                           ''|'' keyword_arguments ''[''"''','''" sequence_argument'']''
                             ''[''"''','''" mapping_argument'']''
                           ''|'' sequence_argument ''[''"''','''" sequence_argument''] [''"''','''" mapping_argument'']''
                           ''|'' mapping_argument
 positional_arguments ''::=''  expression ''(''"''','''" expression'')*''
 keyword_arguments    ''::=''  keyword_item ''(''"''','''" keyword_item'')*''
 sequence_argument    ''::=''  "'''*'''" expression
 mapping_argument     ''::=''  "**" expression
 keyword_item         ''::=''  identifier "'''='''" expression


### =Examples=


```python>>>
 from __future__ import print_function
>>>
>>> def show_args(defparam1, defparam2 = 'default value', *posparam, **keyparam):
  "Straight-forward function to show its arguments"
  print ("  Default Parameters:")
  print ("    defparam1 value is:", defparam1)
  print ("    defparam2 value is:", defparam2)

  print ("  Positional Arguments:")
  if posparam:
    n = 0
    for p in posparam:
      print ("    positional argument:", n, "is:", p)
      n += 1
  else:
    print ("    <None>")

  print ("  Keyword Arguments (by sorted key name):")
  if keyparam:
    for k,v in sorted(keyparam.items()):
      print ("    keyword argument:", k, "is:", v)
  else:
    print ("    <None>")


>>> show_args('POSITIONAL', 'ARGUMENTS')
  Default Parameters:
    defparam1 value is: POSITIONAL
    defparam2 value is: ARGUMENTS
  Positional Arguments:
    <None>
  Keyword Arguments (by sorted key name):
    <None>
>>> show_args(defparam2='ARGUMENT', defparam1='KEYWORD')
  Default Parameters:
    defparam1 value is: KEYWORD
    defparam2 value is: ARGUMENT
  Positional Arguments:
    <None>
  Keyword Arguments (by sorted key name):
    <None>
>>> show_args( *('SEQUENCE', 'ARGUMENTS') )
  Default Parameters:
    defparam1 value is: SEQUENCE
    defparam2 value is: ARGUMENTS
  Positional Arguments:
    <None>
  Keyword Arguments (by sorted key name):
    <None>
>>> show_args( **{'defparam2':'ARGUMENTS', 'defparam1':'MAPPING'} )
  Default Parameters:
    defparam1 value is: MAPPING
    defparam2 value is: ARGUMENTS
  Positional Arguments:
    <None>
  Keyword Arguments (by sorted key name):
    <None>
>>> show_args('ONLY DEFINE defparam1 ARGUMENT')
  Default Parameters:
    defparam1 value is: ONLY DEFINE defparam1 ARGUMENT
    defparam2 value is: default value
  Positional Arguments:
    <None>
  Keyword Arguments (by sorted key name):
    <None>
>>> show_args('POSITIONAL', 'ARGUMENTS',
              'EXTRA', 'POSITIONAL', 'ARGUMENTS')
  Default Parameters:
    defparam1 value is: POSITIONAL
    defparam2 value is: ARGUMENTS
  Positional Arguments:
    positional argument: 0 is: EXTRA
    positional argument: 1 is: POSITIONAL
    positional argument: 2 is: ARGUMENTS
  Keyword Arguments (by sorted key name):
    <None>
>>> show_args('POSITIONAL', 'ARGUMENTS',
              kwa1='EXTRA', kwa2='KEYWORD', kwa3='ARGUMENTS')
  Default Parameters:
    defparam1 value is: POSITIONAL
    defparam2 value is: ARGUMENTS
  Positional Arguments:
    <None>
  Keyword Arguments (by sorted key name):
    keyword argument: kwa1 is: EXTRA
    keyword argument: kwa2 is: KEYWORD
    keyword argument: kwa3 is: ARGUMENTS
>>> show_args('POSITIONAL',
              'ARGUMENTS', 'EXTRA', 'POSITIONAL', 'ARGUMENTS',
              kwa1='EXTRA', kwa2='KEYWORD', kwa3='ARGUMENTS')
  Default Parameters:
    defparam1 value is: POSITIONAL
    defparam2 value is: ARGUMENTS
  Positional Arguments:
    positional argument: 0 is: EXTRA
    positional argument: 1 is: POSITIONAL
    positional argument: 2 is: ARGUMENTS
  Keyword Arguments (by sorted key name):
    keyword argument: kwa1 is: EXTRA
    keyword argument: kwa2 is: KEYWORD
    keyword argument: kwa3 is: ARGUMENTS
>>> # But note:
>>> show_args('POSITIONAL', 'ARGUMENTS',
              kwa1='EXTRA', kwa2='KEYWORD', kwa3='ARGUMENTS',
              'EXTRA', 'POSITIONAL', 'ARGUMENTS')
SyntaxError: non-keyword arg after keyword arg
>>>
```


## Prolog

```prolog
:- initialization(main).

main :-
	sum(b=2,output=Output,a=1),
	writeln(Output).

sum(A1,B1,C1) :-
	named_args([A1,B1,C1],[a=A,b=B,output=Output]),
	Output is A + B.

named_args([],_).
named_args([A|B],C) :-
	member(A,C),
	named_args(B,C).

```



## R


R parameters are all named; arguments can be passed either positionally or with explicit naming. The named arguments are matched to their parameters first, then the unnamed arguments fill in remaining slots. A parameter whose name begins with a period will not be matched to unnamed arguments. R allows abbreviated names to be used as long as they match uniquely to an argument.


```rsplus
divide <- function(numerator, denominator) {
  numerator / denominator
}

divide(3, 2)                       # 1.5
divide(numerator=3, denominator=2) # 1.5
divide(n=3, d=2)                   # 1.5
divide(den=3, num=2)               # 0.66
divide(den=3, 2)                   # 0.66
divide(3, num=2)                   # 0.66
```



## Racket


Racket has built-in keyword and optional arguments:


```racket

#lang racket

(define (pizza sauce
               ;; mandatory keyword argument
               #:topping topping
               ;; optional keyword argument with default
               #:type [type "deep dish"])
  (printf "~a pizza with ~a sauce topped with ~a~n"
          type sauce topping))

(pizza "tomato" #:topping "onion")
(pizza #:topping "onion" "garlic" #:type "pan")

```



## REXX


### version 1


```rexx
/*REXX pgm shows named parameters when called as a subroutine/function*/
/*┌────────────────────────────────────────────────────────────────────┐
  │ The syntax of:   xxx = func1(parmName2=arg2, parmName1=arg1)       │
  │                                                                    │
  │ in the REXX language is interpreted specifically as:               │
  │                                                                    │
  │                  xxx = func1(    yyy       ,       zzz     )       │
  │                                                                    │
  │ where  yyy  is the logical result of comparing (the REXX variables)│
  │                                                                    │
  │                    parmName2   with   arg2                   and   │
  │                                                                    │
  │ where  zzz  is the logical result of comparing (the REXX variables)│
  │                                                                    │
  │                    parmName1   with   arg1                         │
  │                                                                    │
  │ (either as two strings,  or arithmetically if both "parmName2" and │
  │ "arg2"  are both valid REXX numbers.   In the REXX language, there │
  │ is no way to declare (define) what a variable is [or its type], as │
  │ each literal that can be a variable is assumed to be one.  If it's │
  │ not defined, then its uppercase name is used for the value.        │
  │                                                                    │
  │ Consider the one-line REXX program:    say Where are you?          │
  │ causes REXX to consider that four-word expression as a  "SAY"      │
  │ statement, followed by three REXX variables,  each of which aren't │
  │ defined (that is, have a value), so REXX uses a value which is the │
  │ uppercased value of the REXX variable name, namely three values in │
  │ this case,  so the following is displayed:   WHERE ARE YOU?        │
  │                                                                    │
  │ [There is a mechanism in REXX to catch this behavior and raise the │
  │ NOVALUE  condition.]                                               │
  │                                                                    │
  │ To allow a solution to be used for this task's requirement,  and   │
  │ and not get tangled up with the legal REXX syntactical expressions │
  │ shown,  this REXX  programming example  uses a  variation  of the  │
  │ task's illustration  to allow a  method in REXX  of using  named   │
  │ parameters:      xxx = func1('parmName2=' arg2, "parmName1=" arg1) │
  │                                                                    │
  │ Also,  REXX allows the omitting of arguments by just specifying a  │
  │ comma  (or nothing at all,  in the case of a single argument):     │
  │                                                                    │
  │                  xxx = func1(,zzz)                                 │
  │                                                                    │
  │ would indicate that the 1st argument has been omitted.             │
  │                                                                    │
  │                  xxx = func1(yyy)                                  │
  │                                                                    │
  │ would indicate that the 2nd argument  (and all other subsequent    │
  │ arguments)  has/have been omitted.                                 │
  └────────────────────────────────────────────────────────────────────┘*/

parse arg count,itemX                  /*assume 2 values have been used,*/
                                       /*or whatever ... just to show...*/
  do j=1  for arg();      _=arg(1)     /*now, lets examine each argument*/
  if arg(j,'Omitted')     then iterate /*skip examining if argJ omitted.*/
                                       /*(above)   This is superfluous, */
                                       /* but it demonstrates a method. */
  if \arg(j,"Exists")     then iterate /*exactly the same as previous.  */
                                       /*Only 1st char (2nd arg) is used*/
  first=strip(word(_,1))               /*extract the 1st word in arg(j).*/
  if right(first,1)\=='=' then iterate /*skip if 1st word isn't:  xxx=  */
  parse var _ varname '= ' value       /*parse the named variable &value*/
  if varname==''          then iterate /*not the correct format, so skip*/
                                       /*(above) fix this for real pgm. */
  call value varname,value             /*use BIF to set REXX variable.  */
  end   /*j*/

/* ∙∙∙ perform some REXX magic here with specified parameters and stuff:*/
/*     do this,  do that,  perform dis & dat, compute, gears whiz, cogs */
/*     turn,  wheels spin,  belts move, things get assigned, stuff gets */
/*     computed,  wheels spin,  belts move,  things get assigned, motors*/
/*     humm, engines roar, coal gets burned, water turns to steam, real */
/*     work (some of it useful)  gets done,  and something is produced. */

return  'the final meaning of life, or 42  --- whichever is appropriate.'
                                       /*stick a fork in it, we're done.*/
```


### version 2


```rexx
/* REXX ---------------------------------------------------------------
* 01.07.2014 Walter Pachl
* Argument values must not start with 'arg'
*--------------------------------------------------------------------*/
x=f(2,3)
Say x
Say ''
y=f('arg2='3,'arg1='2)
Say y
Exit
f: Procedure
Parse Arg p1,p2
Do i=1 to arg()
  If left(arg(i),3)='arg' Then
    Parse Value arg(i) With 'arg' j '=' p.j
  Else p.i=arg(i)
  End
Do i=1 To arg()
  Say 'p.'i'='p.i
  End
Return p.1**p.2
```

```txt
p.1=2
p.2=3
8

p.1=2
p.2=3
8
```



## Ruby

Ruby 2.0 adds keyword arguments to the language. All keyword arguments are optional, because they have default values. Ruby 2.0 rejects unknown keys; <code>example(typo: 4)</code> raises ArgumentError. In Ruby 2.1 the mandatory default values were dropped.

```ruby
def example(foo: 0, bar: 1, grill: "pork chops")
  puts "foo is #{foo}, bar is #{bar}, and grill is #{grill}"
end

# Note that :foo is omitted and :grill precedes :bar
example(grill: "lamb kebab", bar: 3.14)
```


Ruby 1.9 can fake the effect with a Hash. If a caller passes <code>name: value</code> pairs, Ruby makes a Hash, and the called method sees this Hash in its last argument. To complete the effect, the method may declare an optional last argument that defaults to an empty Hash <code>{}</code>. In this version, <code>example(typo: 4)</code> causes no error.

```ruby
def example(opts = {})
  # Hash#merge raises TypeError if _opts_ is not a Hash.
  # Nothing checks if _opts_ contains unknown keys.
  defaults = {foo: 0, bar: 1, grill: "pork chops"}
  opts = defaults.merge(opts)

  printf("foo is %s, bar is %s, and grill is %s\n",
         opts[:foo], opts[:bar], opts[:grill])
end

example(grill: "lamb kebab", bar: 3.14)
```


Ruby 1.8 and older versions can do the same, but must use the old syntax <code>:name => value</code>.


```ruby
def example(opts = {})
  defaults = {:foo => 0, :bar => 1, :grill => "pork chops"}
  opts = defaults.merge(opts)
  printf("foo is %s, bar is %s, and grill is %s\n",
         opts[:foo], opts[:bar], opts[:grill])
end

example(:grill => "lamb kebab", :bar => 3.14)
```


## Scala


Scala 2.8 utilizes named parameters and default values:


```scala

def add(x: Int, y: Int = 1) = x + y

```



```scala

scala> add(5)
6

scala> add(y=10, x=4)
14

```



## Scheme


```scheme

(define (keyarg-parser argdefs args kont)
  (apply kont
	 (map (lambda (argdef)
		(let loop ((args args))
		  (cond ((null? args)
			 (cadr argdef))
			((eq? (car argdef) (car args))
			 (cadr args))
			(else
			 (loop (cdr args))))))
	      argdefs)))

(define (print-name . args)
  (keyarg-parser '((first #f)(last "?"))
		 args
		 (lambda (first last)
		   (display last)
		   (cond (first
			  (display ", ")
			  (display first)))
		   (newline))))

```



```scheme

=> (print-name)
?
=> (print-name 'first "John")
?, John
=>(print-name 'first "John" 'last "Doe")
Doe, John
=>(print-name 'last "Doe")
Doe

```



## Sidef


```ruby
func example(foo: 0, bar: 1, grill: "pork chops") {
    say "foo is #{foo}, bar is #{bar}, and grill is #{grill}";
}

# Note that :foo is omitted and :grill precedes :bar
example(grill: "lamb kebab", bar: 3.14);
```

```txt
foo is 0, bar is 3.14, and grill is lamb kebab
```



## Smalltalk


As for [[Named Arguments#Objective-C|Objective-C]] the ''methods signature'' is made of "<tt>Symbol:<nowiki>[OtherSymbol:]</nowiki>*</tt>" (* stands for 0 or more repetition of the part in the brackets), without the possibility to reorder them (it would be another signature) or to make them optional.

```smalltalk
Object subclass: AnotherClass [
   "..."
   initWithArray: anArray [ "single argument" ]
   initWithArray: anArray andString: aString [
        "two args; these two methods in usage resemble
         a named argument, with optional andString argument"
   ]
   "..."
]
```



## Standard ML

This example uses Standard ML "fields".

```sml
fun dosomething (a, b, c) = print ("a = " ^ a ^ "\nb = " ^ Real.toString b ^ "\nc = " ^ Int.toString c ^ "\n")

fun example {a, b, c} = dosomething (a, b, c)
```


To call the procedure ''example'', use:

```sml
example {a="Hello World!", b=3.14, c=42}
```

However, this does not support optional parameters. To emulate them, we can process a parameter list:

```sml
datatype param = A of string | B of real | C of int

fun args xs =
	let
	  (* Default values *)
	  val a = ref "hello world"
	  val b = ref 3.14
	  val c = ref 42
	in
	  map (fn (A x) => a := x | (B x) => b := x | (C x) => c := x) xs;
	  (!a, !b, !c)
	end
```

To process the argument list and call ''example'', use:

```sml
dosomething (args [A "tam", B 42.0]);
```


## Suneido

Suneido can handle named and unnamed parameters.  When using a combination, unnamed parameters must come before named ones and must be in the correct order.  Named parameters can be in any order.  Named parameters are given a default value so they are not mandatory.

```Suneido

test = function (one, two, three = '', four = '', five = '')
    {
    Print('one: ' $ one $ ', two: ' $ two $ ', three: ' $ three $
        ', four: ' $ four $ ', five: ' $ five)
    }
test('1', '2', five: '5', three: '3')

```

Output:

```Suneido
one: 1, two: 2, three: 3, four: , five: 5
```



## Swift

Each function parameter has both an argument label and a parameter name. The argument label is used when calling the function; each argument is written in the function call with its argument label before it. The parameter name is used in the implementation of the function. By default, parameters use their parameter name as their argument label.


```Swift
func greet(person: String, hometown: String) -> String {
    return "Hello \(person)!  Glad you could visit from \(hometown)."
}
print(greet(person: "Bill", hometown: "Cupertino"))
```

You write an argument label before the parameter name, separated by a space:

```Swift
func greet(person: String, from hometown: String) -> String {
    return "Hello \(person)!  Glad you could visit from \(hometown)."
}
print(greet(person: "Bill", from: "Cupertino"))
```


If you don’t want an argument label for a parameter, write an underscore (_) instead of an explicit argument label for that parameter.
If a parameter has an argument label, the argument must be labeled when you call the function.

```Swift
func greet(_ person: String, _ hometown: String) -> String {
    return "Hello \(person)!  Glad you could visit from \(hometown)."
}
print(greet("Bill", "Cupertino"))
```


You can define a default value for any parameter in a function by assigning a value to the parameter after that parameter’s type. If a default value is defined, you can omit that parameter when calling the function.

```Swift
func greet(person: String, from hometown: String = "Cupertino") -> String {
    return "Hello \(person)!  Glad you could visit from \(hometown)."
}
print(greet(person: "Bill"))
```



## Tcl

The simplest way of passing named parameters is to use the Tcl language's strong support for [[Varargs#Tcl|variadic commands]] together with its arrays. By convention (originally from [[Tk]]) the named parameters names start with a hyphen (“<tt>-</tt>”) and are called options.

```tcl
proc example args {
    # Set the defaults
    array set opts {-foo 0 -bar 1 -grill "hamburger"}
    # Merge in the values from the caller
    array set opts $args
    # Use the arguments
    return "foo is $opts(-foo), bar is $opts(-bar), and grill is $opts(-grill)"
}
# Note that -foo is omitted and -grill precedes -bar
example -grill "lamb kebab" -bar 3.14
# => ‘foo is 0, bar is 3.14, and grill is lamb kebab’
```

More complex option parsing is possible, e.g., with the <tt>opt</tt> package (of which only a small fraction of the functionality is shown here). This package also allows you to specify type constraints, though that is usually not necessary, and will generate a standard help message that can be obtained with the <tt>-help</tt> option:

```tcl
package require opt
tcl::OptProc example {
    {-foo   -int   0           "The number of foos"}
    {-bar   -float 1.0         "How much bar-ness"}
    {-grill -any   "hamburger" "What to cook on the grill"}
} {
    return "foo is $foo, bar is $bar, and grill is $grill"
}
example -grill "lamb kebab" -bar 3.14
# => ‘foo is 0, bar is 3.14, and grill is lamb kebab’
example -help
# Usage information:
#     Var/FlagName Type  Value       Help
#     ------------ ----  -----       ----
#     ( -help                        gives this help )
#     -foo         int   (0)         The number of foos
#     -bar         float (1.0)       How much bar-ness
#     -grill       any   (hamburger) What to cook on the grill
```


According to [http://wiki.tcl.tk/1730 wiki.tcl.tk discussions], '''::tcl::OptProc is deprecated.'''
The recommended replacement is [http://tcllib.sourceforge.net/doc/cmdline.html cmdline] in [http://tcllib.sourceforge.net/doc/index.html tcllib]. "This is probably the most standard and widely-used of these packages."


## VBA

```vb

Public Function timedelta(Optional weeks As Integer = 0, Optional days As Integer = 0, _
    Optional hours As Integer = 0, Optional minutes As Integer = 0, Optional seconds As Integer = 0, _
    Optional milliseconds As Integer = 0, Optional microseconds As Integer = 0) As Variant
End Function
Public Sub main()
    '-- can be invoked as:
    fourdays = timedelta(days:=4)
    '--       fourdays = timedelta(0,4) '-- equivalent
    '-- **NB** a plain '=' is a very different thing
    oneday = timedelta(days = 1) '-- equivalent to timedelta([weeks:=]IIf((days=1,-1:0))
                                        '-- with NO error if no local variable days exists.
                                        'VBA will assume local variable days=0
    Dim hours As Integer
    shift = timedelta(hours:=hours) '-- perfectly valid (param hours:=local hours)
    '-- timedelta(0,hours:=15,3) '-- illegal (it is not clear whether you meant days:=3 or minutes:=3)
                                 'VBA expects a named parameter for 3
End Sub
```


## Visual Basic


```vb
'the function
Sub whatever(foo As Long, bar As Integer, baz As Byte, qux As String)
    '...
End Sub
'calling the function -- note the Pascal-style assignment operator
Sub crap()
    whatever bar:=1, baz:=2, foo:=-1, qux:="Why is ev'rybody always pickin' on me?"
End Sub
```



## XSLT

XSLT only allows specification of template parameters by name, not position.

```xml
<xsl:template name="table-header">
    <xsl:param name="title"/>
    ...
</xsl:template>
```


