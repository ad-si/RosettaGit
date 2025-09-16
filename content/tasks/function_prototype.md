+++
title = "Function prototype"
description = ""
date = 2019-07-29T14:52:59Z
aliases = []
[extra]
id = 10110
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "aime",
  "algol_68",
  "c",
  "clojure",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "freebasic",
  "go",
  "haskell",
  "j",
  "javascript",
  "julia",
  "kotlin",
  "lua",
  "luck",
  "m2000_interpreter",
  "nim",
  "ocaml",
  "oforth",
  "ol",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "pl_i",
  "purebasic",
  "racket",
  "rexx",
  "snobol4",
  "zkl",
]
+++

Some languages provide the facility to declare functions and subroutines through the use of [[wp:Function prototype|function prototyping]].


## Task

Demonstrate the methods available for declaring prototypes within the language. The provided solutions should include:

* An explanation of any placement restrictions for prototype declarations
* A prototype declaration for a function that does not require arguments
* A prototype declaration for a function that requires two arguments
* A prototype declaration for a function that utilizes varargs
* A prototype declaration for a function that utilizes optional arguments
* A prototype declaration for a function that utilizes named parameters
* Example of prototype declarations for subroutines or procedures (if these differ from functions)
* An explanation and example of any special forms of prototyping not covered by the above



Languages that do not provide function prototyping facilities should be omitted from this task.





## Ada

In Ada, prototypes are called specifications.
* Specifications must be an exact copy of everything prior to the "is" statement for a function or procedure.
* All specifications must appear in a declarative section, ie: before a "begin" statement.
* For a main program, specifications are only necessary if a function call appears in the source before the function definition.
* For a package, specifications must appear as part of the specification(.ads) file, and do not appear in the body file(.adb) (The file extensions apply to Gnat Ada and may not apply to all compilers).

```Ada
function noargs return Integer;
function twoargs (a, b : Integer) return Integer;
--  varargs do not exist
function optionalargs (a, b : Integer := 0) return Integer;
--  all parameters are always named, only calling by name differs
procedure dostuff (a : Integer);
```

Other Prototyping: Since pointers are not generic in Ada, a type must be defined before one can have a pointer to that type, thus for making linked-list type semantics another trivial prototyping exists:

```Ada
type Box; --  tell Ada a box exists (undefined yet)
type accBox is access Box; --  define a pointer to a box
type Box is record --  later define what a box is
   next : accBox; --  including that a box holds access to other boxes
end record;
```

Example of a package specification (i.e. prototype):

```Ada
package Stack is
   procedure Push(Object:Integer);
   function Pull return Integer;
end Stack;
```


Example of a package body:

```Ada
package body Stack is

   procedure Push(Object:Integer) is
   begin
      -- implementation goes here
   end;

   function Pull return Integer;
   begin
      -- implementation goes here
   end;

end Stack;
```


To use the package and function:

```Ada
with Stack;
procedure Main is
   N:integer:=5;
begin
   Push(N);
   ...
   N := Pull;
end Main;
```



## ALGOL 68

'''File: Function_prototype.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

# An explanation of any placement restrictions for prototype declarations #
PROC VOID no args; # Declare a function with no argument that returns an INTeger #
PROC (INT #a#,INT #b#)VOID two args; # Declare a function with two arguments that returns an INTeger #
MODE VARARGS = UNION(INT,REAL,COMPL);
PROC ([]VARARGS)VOID var args; # An empty parameter list can be used to declare a function that accepts varargs #
PROC (INT, []VARARGS)VOID at least one args; # One mandatory INTeger argument followed by varargs #

MODE OPTINT = UNION(VOID,INT), OPTSTRING=UNION(VOID,STRING); # a function that utilizes optional arguments #
PROC (OPTINT, OPTSTRING)VOID optional arguments;

# A prototype declaration for a function that utilizes named parameters #
MODE KWNAME = STRUCT(STRING name),
     KWSPECIES = STRUCT(STRING species),
     KWBREED = STRUCT(STRING breed),
     OWNER=STRUCT(STRING first name, middle name, last name);

# due to the "Yoneda ambiguity" simple arguments must have an unique operator defined #
OP NAME = (STRING name)KWNAME: (KWNAME opt; name OF opt := name; opt),
   SPECIES = (STRING species)KWSPECIES: (KWSPECIES opt; species OF opt := species; opt),
   BREED = (STRING breed)KWBREED: (KWBREED opt; breed OF opt := breed; opt);

PROC ([]UNION(KWNAME,KWSPECIES,KWBREED,OWNER) #options#)VOID print pet;

# subroutines, and fuctions are procedures, so have the same prototype declarations #

# An explanation and example of any special forms of prototyping not covered by the above:  #
COMMENT
  If a function has no arguments, eg f,
  then it is not requied to pass it a "vacuum()" to call it, eg "f()" not correct!
  Rather is can be called without the () vacuum. eg "f"
  A GOTO "label" is equivalent to "PROC VOID label".
END COMMENT

SKIP
```



## Aime


```aime
integer f0(void);               # No arguments
void f1(integer, real);         # Two arguments
real f2(...);                   # Varargs
void f3(integer, ...);          # Varargs

void f4(integer &, text &);     # Two arguments (integer and string), pass by reference
integer f5(integer, integer (*)(integer));
                                # Two arguments: integer and function returning integer and taking one integer argument
integer f6(integer a, real b);  # Parameters names are allowed
record f7(void);                # Function returning an associative array
```



## C


Function prototypes are typically included in a header file at the beginning of a source file prior to functional code. However, this is not enforced by a compiler.


```c
int noargs(void); /* Declare a function with no argument that returns an integer */
int twoargs(int a,int b); /* Declare a function with two arguments that returns an integer */
int twoargs(int ,int); /* Parameter names are optional in a prototype definition */
int anyargs(); /* An empty parameter list can be used to declare a function that accepts varargs */
int atleastoneargs(int, ...); /* One mandatory integer argument followed by varargs */
```



## C#

'''Abstract methods'''<br/>
Interfaces and abstract classes can define abstract methods that must be implemented by subclasses.

```c#
using System;
abstract class Printer
{
    public abstract void Print();
}

class PrinterImpl : Printer
{
    public override void Print() {
        Console.WriteLine("Hello world!");
    }
}
```

'''Delegates'''<br/>
A delegate is similar to a function pointer. They are multicast: multiple methods can be attached to them.

```c#
using System;
public delegate int IntFunction(int a, int b);

public class Program
{
    public static int Add(int x, int y) {
        return x + y;
    }

    public static int Multiply(int x, int y) {
        return x * y;
    }

    public static void Main() {
        IntFunction func = Add;
        Console.WriteLine(func(2, 3)); //prints 5
        func = Multiply;
        Console.WriteLine(func(2, 3)); //prints 6
        func += Add;
        Console.WriteLine(func(2, 3)); //prints 5. Both functions are called, but only the last result is kept.
    }
}
```

'''Partial methods'''<br/>
A partial type is a type that is defined in multiple files.<br/>
A partial method has its signature defined in one part of a partial type, and its implementation defined in another part of the type. If it is not implemented, the compiler removes the signature at compile time.<br/>
The following conditions apply to partial methods:<br/>
- Signatures in both parts of the partial type must match.<br/>
- The method must return void.<br/>
- No access modifiers are allowed. Partial methods are implicitly private.

```c#
//file1.cs
public partial class Program
{
    partial void Print();
}

//file2.cs
using System;

public partial class Program
{
    partial void Print() {
        Console.WriteLine("Hello world!");
    }

    static void Main() {
        Program p = new Program();
        p.Print(); //If the implementation above is not written, the compiler will remove this line.
    }
}
```



## C++

Function declaration in C++ differs from that in C in some aspect.


```cpp
int noargs(); // Declare a function with no arguments that returns an integer
int twoargs(int a,int b); // Declare a function with two arguments that returns an integer
int twoargs(int ,int); // Parameter names are optional in a prototype definition
int anyargs(...); // An ellipsis is used to declare a function that accepts varargs
int atleastoneargs(int, ...); // One mandatory integer argument followed by varargs
template<typename T> T declval(T); //A function template
template<typename ...T> tuple<T...> make_tuple(T...); //Function template using parameter pack (since c++11)

```



## Clojure

If you want to make forward declarations, you can use declare.

```clojure
(declare foo)
```



## COBOL


Prototypes were introduced in COBOL 2002. In the following examples, <code>PROGRAM-ID</code> and <code>PROGRAM</code> can be replaced with the equivalents for functions and methods. However, in method prototypes the <code>PROTOTYPE</code> clause is not used.


```cobol
       *> A subprogram taking no arguments and returning nothing.
       PROGRAM-ID. no-args PROTOTYPE.
       END PROGRAM no-args.

       *> A subprogram taking two 8-digit numbers as arguments, and returning
       *> an 8-digit number.
       PROGRAM-ID. two-args PROTOTYPE.
       DATA DIVISION.
       LINKAGE SECTION.
       01  arg-1 PIC 9(8).
       01  arg-2 PIC 9(8).
       01  ret   PIC 9(8).
       PROCEDURE DIVISION USING arg-1, arg-2 RETURNING ret.
       END PROGRAM two-args.

       *> A subprogram taking two optional arguments which are 8-digit
       *> numbers (passed by reference (the default and compulsory for
       *> optional arguments)).
       PROGRAM-ID. optional-args PROTOTYPE.
       DATA DIVISION.
       LINKAGE SECTION.
       01  arg-1 PIC 9(8).
       01  arg-2 PIC 9(8).
       PROCEDURE DIVISION USING OPTIONAL arg-1, OPTIONAL arg-2.
       END PROGRAM optional-args.

       *> Standard COBOL does not support varargs or named parameters.

       *> A function from another language, taking a 32-bit integer by
       *> value and returning a 32-bit integer (in Visual COBOL).
       PROGRAM-ID. foreign-func PROTOTYPE.
       OPTIONS.
           ENTRY-CONVENTION some-langauge.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  arg PIC S9(9) USAGE COMP-5.
       01  ret PIC S9(9) USAGE COMP-5.
       PROCEDURE DIVISION USING arg RETURNING ret.
       END PROGRAM foreign-func.
```



## Common Lisp

In Common Lisp, function prototypes can be used with <code>(declaim (inline func-name))</code> function arguments are taken when the function is defined. In addition, the argument types aren't needed.

Caveat -- This works with specific implementations of CL. This was tested in SBCL.


```lisp

(declaim (inline no-args))
(declaim (inline one-arg))
(declaim (inline two-args))
(declaim (inline optional-args))

(defun no-args ()
  (format nil "no arguments!"))

(defun one-arg (x)
  ; inserts the value of x into a string
  (format nil "~a" x))

(defun two-args (x y)
  ; same as function `one-arg', but with two arguments
  (format nil "~a ~a" x y))

(defun optional-args (x &optional y) ; optional args are denoted with &optional beforehand
  ; same as function `two-args', but if y is not given it just prints NIL
  (format nil "~a ~a~%" x y))


(no-args) ;=> "no arguments!"

(one-arg 1) ;=> "1"

(two-args 1 "example") ;=> "1 example"

(optional-args 1.0) ;=> "1.0 NIL"

(optional-args 1.0 "example") ;=> "1.0 example"

```


[http://clhs.lisp.se/Body/m_declai.htm More about <code>declaim</code> here]


## D

Beside function prototypes similar to the ones available in C (plus templates, D-style varargs), you can define class method prototypes in abstract classes and interfaces. The exact rules for this are best explained by the [//http://dlang.org/interface.html documentation]

```d
/// Declare a function with no arguments that returns an integer.
int noArgs();

/// Declare a function with no arguments that returns an integer.
int twoArgs(int a, int b);

/// Parameter names are optional in a prototype definition.
int twoArgs2(int, int);

/// An ellipsis can be used to declare a function that accepts
/// C-style varargs.
int anyArgs(...);

/// One mandatory integer argument followed by C-style varargs.
int atLeastOneArg(int, ...);

/// Declare a function that accepts any number of integers.
void anyInts(int[] a...);

/// Declare a function that accepts D-style varargs.
void anyArgs2(TArgs...)(TArgs args);

/// Declare a function that accepts two or more D-style varargs.
void anyArgs3(TArgs...)(TArgs args) if (TArgs.length > 2);

/// Currently D doesn't support named arguments.

/// One implementation.
int twoArgs(int a, int b) {
    return a + b;
}

interface SomeInterface {
    void foo();
    void foo(int, int);

    // Varargs
    void foo(...); // C-style.
    void foo(int[] a...);
    void bar(T...)(T args); // D-style.

    // Optional arguments are only supported if a default is provided,
    // the default arg(s) has/have to be at the end of the args list.
    void foo(int a, int b = 10);
}

void main() {}
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' The position regarding prototypes is broadly similar to that of the C language in that functions,
' sub-routines or operators (unless they have already been fully defined) must be declared before they can be used.
' This is usually done near the top of a file or in a separate header file which is then 'included'.

' Parameter names are optional in declarations. When calling functions, using parameter names
' (as opposed to identifying arguments by position) is not supported.

Type MyType ' needed for operator declaration
  i As Integer
End Type

Declare Function noArgs() As Integer  ' function with no argument that returns an integer
Declare Function twoArgs(As Integer, As Integer) As Integer ' function with two arguments that returns an integer
Declare Function atLeastOneArg CDecl(As Integer, ...) As Integer ' one mandatory integer argument followed by varargs
Declare Function optionalArg(As Integer = 0) As Integer ' function with a (single) optional argument with default value
Declare Sub noArgs2() ' sub-routine with no argument
Declare Operator + (As MyType, As MyType) As MyType ' operator declaration (no hidden 'This' parameter for MyType)


' FreeBASIC also supports object-oriented programming and here all constructors, destructors,
' methods (function or sub), properties and operators (having a hidden 'This' parameter) must be
' declared within a user defined type and then defined afterwards.


Type MyType2
  Public:
    Declare Constructor(As Integer)
    Declare Destructor()
    Declare Sub MySub()
    Declare Function MyFunction(As Integer) As Integer
    Declare Property MyProperty As Integer
    Declare Operator Cast() As String
  Private:
    i As Integer
End Type
```


=={{header|F Sharp|F#}}==
In F#, prototypes are called signatures. Signature files are used to bulk annotate the accessibility of the things within them. If something is in an implementation file but not in the signature file, it is assumed to be private to that file. If it is in the signature file without the <code>internal</code> accessibility modifier, then it is assumed to public, otherwise it is internal to the assembly. For more details, see the [http://msdn.microsoft.com/en-us/library/dd233196.aspx documentation]. Below is an example of the signatures produced for the functions specified in the task (without any accessibility modifiers):

```fsharp
// A function taking and returning nothing (unit).
val noArgs : unit -> unit
// A function taking two integers, and returning an integer.
val twoArgs : int -> int -> int
// A function taking a ParamPack array of ints, and returning an int. The ParamPack
// attribute is not included in the signature.
val varArgs : int [] -> int
// A function taking an int and a ParamPack array of ints, and returning an
// object of the same type.
val atLeastOnArg : int -> int [] -> int
// A function taking an int Option, and returning an int.
val optionalArg : Option<int> -> int

// Named arguments and the other form of optional arguments are only available on
// methods.
type methodClass =
  class
    // A method taking an int named x, and returning an int.
    member NamedArg : x:int -> int
    // A method taking two optional ints in a tuple, and returning an int. The
    //optional arguments must be tupled.
    member OptionalArgs : ?x:int * ?y:int -> int
  end
```



## Go

While the language specification does not use the word prototype it states, "A function declaration may omit the body. Such a declaration provides the signature for a function implemented outside Go, such as an assembly routine."  This is the closest analogy to a C (e.g.) prototype.

Function declarations whether with a body or without must be "top level" declarations, that is, after the package clause and outside of any other function.  Examples of function delarations without bodies are,

```go
func a()          // function with no arguments
func b(x, y int)  // function with two arguments
func c(...int)    // varargs are called "variadic parameters" in Go.
```

Go does not directly support optional or named parameters and does not have any concept of procedures or subroutines as distinct from functions.

Otherwise, Go does have the concept of a function signature which includes parameters and return values.  Go is strongly typed and functions are first class objects so function signatures are used in a variety of ways.  These might be considered distinct from the concept of function prototype.


## Haskell

A function can be declared without giving it's prototype in Haskell. The haskell compiler has got type inference
whereby it can infer the return type and type of variable given to function. You can still hardcode the prototype
which specifies the datatype of variables and return type. For ex. Consider a function add which takes two
integers and returns their sum. It can be prototyped and declared as :

```haskell

add :: Int -> Int -> Int
add x y = x+y

```


Actually all functions in haskell are functions with just one arguments. Haskell will treat above function as a
function which takes an int and returns a function with type (:: (Int->Int)) . Then this function which is returned
is such that it takes an int and returns an int.
Similarly for any function add which takes 3 integers and adds them the actual prototype will be as follows:

```haskell

add :: Int->(Int ->(Int->Int))

```

The one that does not require arguements could just be:

```haskell

printThis = putStrLn("This is being printed.")

```

But haskell would rather consider the function to be of return type IO() in this case.

Two arguments:

```haskell

add :: Int -> Int -> Int
add x y = x+y

```


The same thing can be done using the lambda function as :

```haskell

add :: Int -> Int -> Int
add = \x->\y -> x+y

```


Two arguments with unnamed parameters:

```haskell

doThis :: Int-> Int-> String
doThis _ _  = "Function with unnamed parameters"

```


Function with var args requires creation of type class as per the requirement.


## J

J assumes an unknown name is a verb of infinite rank.  Rank determines the frames on which the verb executes.  As the demonstration shows, changing the rank, assigning the rank before the verb is used in other definitions affects the result.  We could, of course, play other games by changing the unknown name from verb to another part of speech.

```J
   NB. j assumes an unknown name f is a verb of infinite rank
   NB. f has infinite ranks
   f b. 0
_ _ _

   NB. The verb g makes a table.
   g=: f/~

   NB. * has rank 0
   f=: *


   NB. indeed, make a multiplication table
   f/~ i.5
0 0 0  0  0
0 1 2  3  4
0 2 4  6  8
0 3 6  9 12
0 4 8 12 16

   NB. g was defined as if f had infinite rank.
   g i.5
0 1 4 9 16

   NB. f is known to have rank 0.
   g=: f/~

   NB. Now we reproduce the table
   g i.5
0 0 0  0  0
0 1 2  3  4
0 2 4  6  8
0 3 6  9 12
0 4 8 12 16



   NB. change f to another rank 0 verb
   f=: +

   NB. and construct an addition table
   g i.5
0 1 2 3 4
1 2 3 4 5
2 3 4 5 6
3 4 5 6 7
4 5 6 7 8


   NB. f is multiplication at infinite rank
   f=: *"_


   NB. g, however, has rank 0
   g i.5
0 0 0  0  0
0 1 2  3  4
0 2 4  6  8
0 3 6  9 12
0 4 8 12 16
```



## JavaScript


###  ES5

JavaScript functions may also be used to define prototype objects

```JavaScript

// A prototype declaration for a function that does not require arguments
function List() {}

List.prototype.push = function() {
  return [].push.apply(this, arguments);
};

List.prototype.pop = function() {
  return [].pop.call(this);
};

var l = new List();
l.push(5);
l.length; // 1
l[0]; 5
l.pop(); // 5
l.length; // 0

// A prototype declaration for a function that utilizes varargs
function List() {
  this.push.apply(this, arguments);
}

List.prototype.push = function() {
  return [].push.apply(this, arguments);
};

List.prototype.pop = function() {
  return [].pop.call(this);
};

var l = new List(5, 10, 15);
l.length; // 3
l[0]; 5
l.pop(); // 15
l.length; // 2


```



###  ES6

Class Declarations are used to define prototype objects

```JavaScript

// A prototype declaration for a function that does not require arguments
class List {
  push() {
    return [].push.apply(this, arguments);
  }
  pop() {
    return [].pop.call(this);
  }
}

var l = new List();
l.push(5);
l.length; // 1
l[0]; 5
l.pop(); // 5
l.length; // 0



// A prototype declaration for a function that utilizes varargs
class List {
  constructor(...args) {
    this.push(...args);
  }
  push() {
    return [].push.apply(this, arguments);
  }
  pop() {
    return [].pop.call(this);
  }
}

var l = new List(5, 10, 15);
l.length; // 3
l[0]; 5
l.pop(); // 15
l.length; // 2

```




## Julia

Julia does not need or use function prototypes in general. Generic functions are further specialized as to argument type and return type during just-in-time compilation if required. However, when interacting with other languages such a C which use function prototypes, Julia can prototype its functions for passing its functions to external languages with the @cfunction macro:


```julia
julia
 function mycompare(a, b)::Cint
      (a < b) ? -1 : ((a > b) ? +1 : 0)
  end
mycompare (generic function with 1 method)

```


Using @cfunction to create a prototype for passing this to C's quicksort:


```julia
julia
 mycompare_c = @cfunction(mycompare, Cint, (Ref{Cdouble}, Ref{Cdouble}))

```



## Kotlin

The order of declarations in Kotlin is unimportant and so forward declaration of 'top level' functions is neither needed nor supported.

The only place where function (or property) prototypes are needed is for abstract members of classes or interfaces whose implementation will be provided by overriding those members in a derived or implementing class or object.

Here's an example of this. Note that since Kotlin allows arguments to be passed either by name or position for all functions, there is no separate prototype for this situation. Moreover, since arguments may be passed by name, it is strongly recommended (but not obligatory) that the parameter names for overriding members should be the same as for the functions they override. The compiler will issue a warning if this recommendation is not followed.

```scala
// version 1.0.6

interface MyInterface {
    fun foo()                     // no arguments, no return type
    fun goo(i: Int, j: Int)       // two arguments, no return type
    fun voo(vararg v: Int)        // variable number of arguments, no return type
    fun ooo(o: Int = 1): Int      // optional argument with default value and return type Int
    fun roo(): Int                // no arguments with return type Int
    val poo: Int                // read only property of type Int
}

abstract class MyAbstractClass {
    abstract fun afoo()           // abstract member function, no arguments or return type
    abstract var apoo: Int        // abstract read/write member property of type Int
}

class Derived : MyAbstractClass(), MyInterface {
    override fun afoo() {}
    override var apoo: Int = 0

    override fun foo() {}
    override fun goo(i: Int, j: Int) {}
    override fun voo(vararg v: Int) {}
    override fun ooo(o: Int): Int = o  // can't specify default argument again here but same as in interface
    override fun roo(): Int = 2
    override val poo: Int = 3
}

fun main(args: Array<String>) {
    val d = Derived()
    println(d.apoo)
    println(d.ooo())  // default argument of 1 inferred
    println(d.roo())
    println(d.poo)
}
```


```txt

0
1
2
3

```



## Lua


```Lua
function Func() -- Does not require arguments
	return 1
end

function Func(a,b) -- Requires arguments
	return a + b
end

function Func(a,b) -- Arguments are optional
	return a or 4 + b or 2
end

function Func(a,...) -- One argument followed by varargs
	return a,{...} -- Returns both arguments, varargs as table
end
```



## Luck


```Luck
function noargs(): int = ? ;;
function twoargs(x:int, y:int): int = ? ;;

/* underscore means ignore and is not bound to lexical scope */
function twoargs(_:bool, _:bool): int = ? ;;

function anyargs(xs: ...): int = ? ;;
function plusargs(x:int, xs: ...): int = ? ;;
```




## M2000 Interpreter

Functions/modules are declared before used. So the flow matter, position in code not matter (perhaps we can put functions in a simple routine, using a label, execute a gosub to label, then make the functions, and then return. Functions amd modules added to a specific list of functions/modules, so every time interpreter check that list (a hash table). They can change definition including signature. Any module/function before executed has no declared local modules/functions. Declarations performed as they executed. For modules, we can prepare a non changed module before begin execute module's code, and when declaration for same name module comes to execute, it just skipped.

Subroutines are parts of functions/modules and first searched from bottom, added to a list of subs positions,therefore they can't changed.
Example of change an inner module using another module with same signature. Module MyBeta {Read x : ... } or Module MyBeta (x) { ... } or Module MyBeta(x) { } is the same.


```M2000 Interpreter

Module Check {
      Module MyBeta (a) {
            Print "MyBeta", a/2
      }
      Module TestMe {
            Module Beta (x) {
                  Print "TestMeBeta", x
            }
            Beta 100
      }
      TestMe ; Beta as MyBeta
}
Check

```


Signatures needed for Event object. An event object get a list of functions, called as modules, and call every function with same signature. We can provide arguments by reference too. We can define simple functions (without visibility except local and global), or group functions (static groups) with visibility local, global and group level, or we can define local scope functions.

```M2000 Interpreter

Module Check {,
      \\ make an event object
      \\ with a prototype signature
      \\ first parameter  is numeric/object by value, and second is by reference
      Event Alfa {
            Read x, &z
      }

      \\ make a function with same signature
      Function ServiceAlfa {
            read a, &b
            b+=a
      }

      \\ add function to event
      Event Alfa new &ServiceAlfa()

      \\ call event in this module
      var=30
      Call Event Alfa,  10, &var
      Print var=40
      \\ make a local module, and pass event by value
      Module checkinside (ev) {
            \\ ev is a copy of Alfa
            m=10
            call event ev, 4, &m
            Print m=14
            \\ clear functions from ev
            Event ev Clear
            \\ we can call it again, but nothing happen
            call event ev, 4, &m
            Print m=14
      }
      checkinside Alfa
      \\ so now we call Alfa
      Call Event Alfa,  10, &var
      Print var=50
      Event Alfa Hold
      \\ calling do nothing, because of Hold state
      Call Event Alfa,  10, &var
      Event Alfa Release
      Call Event Alfa,  10, &var
      Print var=60
}
Check

```


Using a function for local call (module visibility)


```M2000 Interpreter

Module Check {,
      \\ make an event object
      \\ with a prototype signature
      \\ first parameter  is numeric/object by value, and second is by reference
      Event Alfa {
            Read x, &z
      }

      \\ make a function with same signature
      \\ but here prepared to used with current module visibility
      m=0
      Function ServiceAlfa {
            \ this code "see" m variable
            \\ we have to use new, to make new a, b for sure
            read new a, &b
            b+=a
            m++
      }

      \\ add function to event, making reference as local to module
      Event Alfa new Lazy$(&ServiceAlfa())

      \\ call event in this module
      var=30
      Call Event Alfa,  10, &var
      Print var=40
      \\ make a local module, and pass event by value
      Module checkinside (ev) {
            \\ ev is a copy of Alfa
            m=10
            call event ev, 4, &m
            Print m=14
            \\ clear functions from ev
            Event ev Clear
            \\ we can call it again, but nothing happen
            call event ev, 4, &m
            Print m=14
      }
      checkinside Alfa
      \\ so now we call Alfa
      Call Event Alfa,  10, &var
      Print var=50
      Event Alfa Hold
      \\ calling do nothing, because of Hold state
      Call Event Alfa,  10, &var
      Event Alfa Release
      Call Event Alfa,  10, &var
      Print var=60
      Print m=4  ' 4 times called ServiceAlfa
}
Check

```


Using a Function in a Group (Groups are the User objects in M2000)


```M2000 Interpreter

Module Check {,
      \\ make an event object
      \\ with a prototype signature
      \\ first parameter  is numeric/object by value, and second is by reference
      Event Alfa {
            Read x, &z
      }

      \\ make a group function with same signature

     Group IamStatic {
            m=0
            Function ServiceAlfa(a, &b) {
                  b+=a
                  .m++
            }
}

      \\ add function to event, making reference as local to module
      Event Alfa new &IamStatic.ServiceAlfa()

      \\ call event in this module
      var=30
      Call Event Alfa,  10, &var
      Print var=40
      \\ make a local module, and pass event by value
      Module checkinside (ev) {
            \\ ev is a copy of Alfa
            m=10
            call event ev, 4, &m
            Print m=14
            \\ clear functions from ev
            Event ev Clear
            \\ we can call it again, but nothing happen
            call event ev, 4, &m
            Print m=14
      }
      checkinside Alfa
      \\ so now we call Alfa
      Call Event Alfa,  10, &var
      Print var=50
      Event Alfa Hold
      \\ calling do nothing, because of Hold state
      Call Event Alfa,  10, &var
      Event Alfa Release
      Call Event Alfa,  10, &var
      Print var=60
      Print IamStatic.m=4  ' 4 times called IamStatic.ServiceAlfa
}
Check

```




## Nim

Procedure declarations can be used if a proc is to be used before its definition.

```nim
# Procedure declarations. All are named
proc noargs(): int
proc twoargs(a, b: int): int
proc anyargs(x: varargs[int]): int
proc optargs(a, b: int = 10): int

# Usage
discard noargs()
discard twoargs(1,2)
discard anyargs(1,2,3,4,5,6,7,8)
discard optargs(5)

# Procedure definitions
proc noargs(): int = echo "noargs"
proc twoargs(a, b: int): int = echo "twoargs"
proc anyargs(x: varargs[int]): int = echo "anyargs"
proc optargs(a: int, b = 10): int = echo "optargs"
```



## OCaml



```ocaml
(* Usually prototype declarations are put in an interface file,
   a file with .mli filename extension *)

(* A prototype declaration for a function that does not require arguments *)
val no_arg : unit -> unit

(* A prototype declaration for a function that requires two arguments *)
val two_args : int -> int -> unit

(* A prototype declaration for a function that utilizes optional arguments *)
val opt_arg : ?param:int -> unit -> unit
(* in this case we add a unit parameter in order to omit the argument,
   because ocaml supports partial application *)

(* A prototype declaration for a function that utilizes named parameters *)
val named_arg : param1:int -> param2:int -> unit

(* An explanation and example of any special forms of prototyping not covered by the above *)

(* A prototype declaration for a function that requires a function argument *)
val fun_arg : (int -> int) -> unit

(* A prototype declaration for a function with polymorphic argument *)
val poly_arg : 'a -> unit
```



## Oforth


Oforth can only forward declare methods (see Mutual Recursion task). A method can be declared without any class implementation :

```oforth
Method new: myMethod
```


This creates a new method object with name myMethod (or does nothing if this object already exists). It says nothing about method implementations (number of parameters, return value, ...).

A method object is not directly related to classes :

- A method object is created.

- Classes are created.

- Into classes, you can create implementations for particular methods. If, at this point, the method object does not exist yet, it is created.


## Ol

Ol have no function prototypes.

Ol functions is a first-class functions with dynamic arguments translation so no function prototypes is required.


## PARI/GP

GP does not use function prototypes.

PARI uses C prototypes.  Additionally, gp2c parser codes are essentially function prototypes.  They must be placed in the file called by gp2c, not in a file included by it, and they must appear as a <code>GP;</code> comment.  For a function


```c
long
foo(GEN a, GEN b)
```

which takes two (required) <code>t_INT</code> arguments, returns a small integer (a [[C]] long) and appears as <code>bar</code> to the gp interpreter, the following command would be used:

```c
/*
GP;install("foo","LGG","bar","./filename.gp.so");
*/
```


If its arguments were optional it could be coded as

```c
/*
GP;install("foo","LDGDG","bar","./filename.gp.so");
*/
```

although other parser codes are possible; this one sends <code>NULL</code> if the arguments are omitted.

A code like

```c
/*
GP;install("foo","s*","bar","./filename.gp.so");
*/
```

can be used to take a variable (0 or more) number of arguments. Note that the return type in this case is implicitly <code>GEN</code>

Other special forms are described in the User's Guide to the PARI library, section 5.7.3.


## Perl


The perl scripting language allows prototypes to be checked during JIT compilation. Prototypes should be placed before subroutine definitions, declarations, or anonymous subroutines. The sigil [[Special characters#Perl|special symbols]] act as argument type placeholders.


```perl
sub noargs(); # Declare a function with no arguments
sub twoargs($$); # Declare a function with two scalar arguments. The two sigils act as argument type placeholders
sub noargs :prototype(); # Using the :attribute syntax instead
sub twoargs :prototype($$);
```



## Perl 6

There is no restriction on placement of prototype declarations.  (Actually, we call them "stub declarations".)  In fact, stub declarations are rarely needed in Perl 6 because post-declaration of functions is allowed, and normal [http://design.perl6.org/S06.html#Subroutines_and_other_code_objects function declarations] do not bend the syntax the way they sometimes do in Perl 5.

Note that the <tt>...</tt> in all of these stub bodies is literally part of the declaration syntax.

A prototype declaration for a function that does not require arguments (and returns an Int):

```perl6
sub foo ( --> Int) {...}
```


A prototype declaration for a function that requires two arguments.
Note that we can omit the variable name and just use the sigil in the stub, since we don't need to reference the argument until the actual definition of the routine.  Also, unlike in Perl 5, a sigil like <tt>@</tt> defaults to binding a single positional argument.

```perl6
sub foo (@, $ --> Int) {...}
```


A prototype declaration for a function that utilizes varargs after one required argument.
Note the "slurpy" star turns the <tt>@</tt> sigil into a parameter that accepts all the rest of the positional arguments.

```perl6
sub foo ($, *@ --> Int) {...}
```


A prototype declaration for a function that utilizes optional arguments after one required argument.  Optionality is conferred by either a question mark or a default:

```perl6
sub foo ($, $?, $ = 42 --> Int) {...}
```


A prototype declaration for a function that utilizes named parameters:

```perl6
sub foo ($, :$faster, :$cheaper --> Int) {...}
```


Example of prototype declarations for subroutines or procedures, which in Perl 6 is done simply by noting that nothing is returned:

```perl6
sub foo ($, $ --> Nil) {...}
```


A routine may also slurp up all the named arguments that were not bound earlier in the signature:

```perl6
sub foo ($, :$option, *% --> Int) {...}
```


A routine may make a named parameter mandatory using exclamation mark.  (This is more useful in multi subs than in stubs though.)

```perl6
sub foo ($, :$option! --> Int) {...}
```


A routine may unpack an <tt>Array</tt> automaticly. Here the first element is stored in a scalar and the rest in an <tt>Array</tt>. Other buildin types can be [http://design.perl6.org/S06.html#Unpacking_array_parameters unpacked] as well.

```perl6
sub foo ([$, @]) {...}
```


A routine may ask for a closure parameter to implement higher order functions. Typed or untyped signatures can be supplied.

```perl6
sub foo (@, &:(Str --> Int)) {...}
```



## Phix

As explicit forward definitions, and optional - unless (eg) you want to use named parameters in a forward call.

Should be identical to the actual definition, but preceded by "forward" and with no body.

```Phix
forward function noargs() -- Declare a function with no arguments
forward procedure twoargs(integer a, integer b) -- Declare a procedure with two arguments
forward procedure twoargs(integer, integer /*b*/) -- Parameter names are optional in forward (and actual) definitions
forward function anyargs(sequence s) -- varargs are [best/often] handled as a (single) sequence in Phix
forward function atleastonearg(integer a, integer b=1, ...); -- Default makes args optional (== actual defn)
```

No special syntax is needed on actual or forward function definitions for named parameters, and calls are identical whether still forward or now fully defined.

Defaults on optional parameters in forward definitions can also be dummy (but type compatible) values should the actual not yet be defined.


## PL/I


```pli

declare s1 entry;
declare s2 entry (fixed);
declare s3 entry (fixed, float);

declare f1 entry returns (fixed);
declare f2 entry (float) returns (float);
declare f3 entry (character(*), character(*)) returns (character (20));

```


## PureBasic

PureBasic defines both functions and procedures by using the keyword '''Procedure'''.  For the purposes of this task I will describe both 'procedures' and 'functions' using only the term 'procedure'.  PureBasic uses a one-pass compiler.  All procedures need to be defined before use.  The prototypes referred to in the task description are performed with forward declarations in PureBasic.

PureBasic allows two types of prototyping.
The first uses the keyword '''Declare''' and describes the name, return value, and parameters of a procedure.
It is identical in form with the first line of a procedure definition with the exception that the keyword
'''Declare''' is used instead of the keyword 'Procedure'''.
It must be placed before the first use of the procedure and must occur before the procedure's definition.
The procedure declaration's parameteres need to match the order, type, and number of those in the procedure's
definition, though their names may be different.

The keyword '''ProtoType''' may be used for pointers to procedures so that a definition of the parameters and return type for the function being pointed to are defined and that the pointer may be used to execute the function with type checking.  The parameter names do not have to match in the 'ProtoType' definition but the order, type and optional parameters do.  'ProtoTypes' must be defined before their first use.

PureBasic does not allow either variable arguments or named parameters.

```purebasic
;Forward procedure declare defined with no arguments and that returns a string
Declare.s booTwo()
;Forward procedure declare defined with two arguments and that returns a float
Declare.f moo(x.f, y.f)
;Forward procedure declare with two arguments and an optional argument and that returns a float
Declare.f cmoo(x.f, y.f, m.f = 0)

;*** The following three procedures are defined before their first use.
;Procedure defined with no arguments and that returns a string
Procedure.s boo(): ProcedureReturn "boo": EndProcedure
;Procedure defined with two arguments and that returns an float
Procedure.f aoo(x.f, y.f): ProcedureReturn x + y: EndProcedure
;Procedure defined with two arguments and an optional argument and that returns a float
Procedure.f caoo(x.f, y.f, m.f = 1): ProcedureReturn (x + y) * m: EndProcedure

;ProtoType defined for any function with no arguments and that returns a string
Prototype.s showString()
;Prototype defined for any function with two float arguments and that returns a float
Prototype.f doMath(x.f, y.f)
;ProtoType defined for any function with two float arguments and an optional float argument and that returns a float
Prototype.f doMathWithOpt(x.f, y.f, m.f = 0)

Define a.f = 12, b.f = 5, c.f = 9
Define proc_1.showString, proc_2.doMath, proc_3.doMathWithOpt ;using defined ProtoTypes
If OpenConsole("ProtoTypes and Forward Declarations")

  PrintN("Forward Declared procedures:")
  PrintN(boo())
  PrintN(StrF(a, 2) + " * " + StrF(b, 2) + " = " + StrF(moo(a, b), 2))
  PrintN(StrF(a, 2) + " * " + StrF(b, 2) + " + " + StrF(c, 2) + " = " + StrF(cmoo(a, b, c), 2))
  PrintN(StrF(a, 2) + " * " + StrF(b, 2) + " = " + StrF(cmoo(a, b), 2))

  ;set pointers to second set of functions
  proc_1 = @boo()
  proc_2 = @aoo()
  proc_3 = @caoo()

  PrintN("ProtoTyped procedures (set 1):")
  PrintN(proc_1())
  PrintN(StrF(a, 2) + " ? " + StrF(b, 2) + " = " + StrF(proc_2(a, b), 2))
  PrintN(StrF(a, 2) + " ? " + StrF(b, 2) + " ? " + StrF(c, 2) + " = " + StrF(proc_3(a, b, c), 2))
  PrintN(StrF(a, 2) + " ? " + StrF(b, 2) + " = " + StrF(proc_3(a, b), 2))

  ;set pointers to second set of functions
  proc_1 = @booTwo()
  proc_2 = @moo()
  proc_3 = @cmoo()

  PrintN("ProtoTyped procedures (set 2):")
  PrintN(proc_1())
  PrintN(StrF(a, 2) + " ? " + StrF(b, 2) + " = " + StrF(proc_2(a, b), 2))
  PrintN(StrF(a, 2) + " ? " + StrF(b, 2) + " ? " + StrF(c, 2) + " = " + StrF(proc_3(a, b, c), 2))
  PrintN(StrF(a, 2) + " ? " + StrF(b, 2) + " = " + StrF(proc_3(a, b), 2))


  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf


;*** If the forward Declaration above are not used then the following Procedure
;definitions each have to be placed before the call to the respective procedure.

;Procedure defined with no arguments and that returns a string
Procedure.s booTwo()
  ProcedureReturn "booTwo"
EndProcedure

;Procedure defined with two arguments and that returns an float
Procedure.f moo(x.f, y.f)
  ProcedureReturn x * y
EndProcedure

;Procedure defined with two arguments and an optional argument and that returns an float
Procedure.f cmoo(x.f, y.f, m.f = 0)
  ProcedureReturn (x * y) + m
EndProcedure

```

Sample output:

```txt
Forward Declared procedures:
boo
12.00 * 5.00 = 60.00
12.00 * 5.00 + 9.00 = 69.00
12.00 * 5.00 = 60.00
ProtoTyped procedures (set 1):
boo
12.00 ? 5.00 = 17.00
12.00 ? 5.00 ? 9.00 = 153.00
12.00 ? 5.00 = 0.00
ProtoTyped procedures (set 2):
booTwo
12.00 ? 5.00 = 60.00
12.00 ? 5.00 ? 9.00 = 69.00
12.00 ? 5.00 = 60.00
```



## Racket

Most of the points are covered in this program

```racket

#lang racket

(define (no-arg) (void))

(define (two-args a b) (void)) ;arguments are always named

(define (varargs . args) (void)) ;the extra arguments are stored in a list

(define (varargs2 a . args) (void)) ;one obligatory argument and the rest are contained in the list

(define (optional-arg (a 5)) (void)) ;a defaults to 5
```


<tt>(void)</tt> is a function that returns "nothing", so this are prototypes that do nothing.
Although standard Racket doesn't allow type declarations, it allows contracts, so we can add this to the previous declarations

```racket

(provide (contract-out
          [two-args (integer? integer? . -> . any)]))
```

then any module that imports the function can only pass integers to two-args.

Another way is using the <tt>typed/racket</tt> language, like this

```racket

#lang typed/racket

(: two-args (Integer Integer -> Any))
(define (two-args a b) (void))
```



## REXX

In the REXX language, there is no difference between functions and
subroutines,   <big> ''except that'' </big>   functions
''must''   return a value,   even if that value is a
"null"   (empty string).

In REXX, if a function doesn't return a value,
a   ''syntax''   condition is raised.

REXX has no need of pre─allocating a prototype   (such as required
arguments and the like)   for functions or subroutines,   but there
are facilities (in the form of BIFs) to assist the REXX programmer to easily
determine the number of arguments passed (if any),   and perform
(and/or enforce) any necessary argument passing (including
the   ''type''   of values or variables passed),   and
also including checking for omitted arguments.   In effect, the
relaxation of requirements/rules for function or subroutine invocations has
been moved from the compile stage (for REXX, the parsing/interpretive) stage)
to the execution stage.


<small>Note:   REXX is an interpretive language. </small>





## SNOBOL4

In SNOBOL4, functions are actually a hack and are defined in an idiosyncratic way that is simultaneously like a prototype or not like one as the case may be.


### Basics

To begin with, we look at the definition provided [http://rosettacode.org/wiki/Function_definition#SNOBOL4 at the relevant task page]:


```snobol4
          define('multiply(a,b)') :(mul_end)
multiply  multiply = a * b        :(return)
mul_end

* Test
          output = multiply(10.1,12.2)
          output = multiply(10,12)
end
```


The key to this is the <code>define()</code> BIF which declares the actual function and the <code>multiply</code> label which is the entry point to the code that is executed.  The key is that SNOBOL4 is an almost militantly unstructured language.  There is absolutely nothing special about the <code>multiply</code> entry point that distinguishes it from the target of any other branch target.  What happens instead is that the <code>define()</code> BIF associates a certain string pattern--the prototype, in effect--with an entry point.  The <code>:(mul_end)</code> piece at the end, in fact, exists because were it not present the body of the <code>multiply</code> "function" would be executed: it is a branch to the label <code>mul_end</code>.

On execution, the SNOBOL4 runtime will execute line by line of the script.  When it reaches the <code>define</code> BIF call it will do the stuff it needs to do behind the scenes to set up function-like access to the <code>multiply</code> branch target.  It would then proceed to execute the next line were it not for the branch.


### Separation of prototype and body


Of course this implies that you can separate the two pieces.  Which you can, like this:


```snobol4
          define('multiply(a,b)')

*
* Assume lots more code goes here.
*
                                  :(test)

*
* MORE CODE!
*

multiply  multiply = a * b        :(return)

*
* MORE CODE!
*

test
          output = multiply(10.1,12.2)
          output = multiply(10,12)
end
```


With this structure the "function" is declared at the program, the implementation is somewhere down in the middle, and the mainline (<code>test</code> here) is at the end.


### Full prototyping


The <code>define()</code> BIF is used for more than merely providing function-like access to a label with the same name.  It is used to prototype all of these (with some default behaviour):

*  the function name (<code>multiply</code> in the examples);
*  the formal arguments to the function (<code>a, b</code> in the examples);
*  the entry point label for the function's code (defaults to the function name, <code>mult_impl</code> in the following example);
*  any local variables which should be protected in the function (defaults to none, <code>acc1,acc2</code> in the following example).

Thus a highly-contrived example function that illustrates all of these would look like this:


```snobol4
          define('multiply(a,b)acc1,acc2','mult_impl') :(mult_end)
mult_impl acc1 = a
          acc2 = b
          multiply = acc1 * acc2                       :(return)
mult_end

* Test
          output = multiply(10.1,12.2)
          output = multiply(10,12)
end
```



## zkl

In zkl, all functions are var args. Prototypes provide provide some documentation and an overlay on the incoming args. Named parameters are not supported.

```zkl
fcn{"Hello World"}   // no expected args
fcn(){"Hello World"} // ditto

fcn{vm.arglist}(1,2)   // ask the VM for the passed in args -->L(1,2)
fcn f(a,b){a+b}  // fcn(1,2,3) works just fine
fcn f(args){}(1,2,3)  //args = 1
fcn(args){vm.arglist.sum()}(1,2,3) //-->6

fcn(a=1,b=2){vm.arglist}()  //-->L(1,2)
fcn(a=1,b=2){vm.arglist}(5) //-->L(5,2)
fcn(a=1,b){vm.arglist}()    //-->L(1), error if you try to use b
fcn(a,b=2){vm.arglist}(5)   //-->L(5,2)
fcn(a,b=2,c){vm.arglist}(1) //-->L(1,2)

fcn(){vm.nthArg(1)}(5,6)           //-->6
fcn{vm.numArgs}(1,2,3,4,5,6,7,8,9) //-->9
fcn{vm.argsMatch(...)}   // a somewhat feeble attempt arg pattern matching based on type (vs value)

   // you can do list assignment in the prototype:
fcn(a,[(b,c)],d){vm.arglist}(1,L(2,3,4),5) //-->L(1,L(2,3,4),5)
fcn(a,[(b,c)],d){"%s,%s,%s,%s".fmt(a,b,c,d)}(1,L(2,3,4),5) //-->1,2,3,5

   // no type enforcement but you can give a hint to the compiler
fcn([Int]n){n.sin()} //--> syntax error as Ints don't do sin
```


