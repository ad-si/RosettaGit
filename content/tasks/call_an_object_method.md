+++
title = "Call an object method"
description = ""
date = 2019-10-15T08:03:25Z
aliases = []
[extra]
id = 10321
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
languages = [
  "actionscript",
  "ada",
  "apex",
  "arturo",
  "autohotkey",
  "bracmat",
  "chuck",
  "clojure",
  "cobol",
  "coffeescript",
  "common_lisp",
  "cpp",
  "d",
  "dyalect",
  "e",
  "elena",
  "elixir",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "latitude",
  "lfe",
  "lingo",
  "logtalk",
  "lua",
  "m2000_interpreter",
  "maple",
  "miniscript",
  "nemerle",
  "netrexx",
  "nim",
  "oasys_assembler",
  "objeck",
  "object_pascal",
  "ocaml",
  "oforth",
  "oorexx",
  "perl",
  "perl_6",
  "php",
  "picolisp",
  "pike",
  "pl_sql",
  "powershell",
  "processing",
  "python",
  "racket",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "smalltalk",
  "supercollider",
  "swift",
  "tcl",
  "ursa",
  "vba",
  "xlisp",
  "zkl",
]
+++

## Task

In [[object-oriented programming]] a method is a function associated with a particular class or object. In most forms of object oriented implementations methods can be static, associated with the class itself; or instance, associated with an instance of a class.

Show how to call a static or class method, and an instance method of a class.


## ActionScript


```actionscript
// Static
MyClass.method(someParameter);

// Instance
myInstance.method(someParameter);
```



## Ada

Ada is a language based on strict typing. Nevertheless, since Ada 95 (the first major revision of the language), Ada also includes the concepts of a class. Types may be tagged, and for each tagged type T there is an associated type T'Class. If you define a method as "procedure Primitive(Self: T)", the actual parameter Self must be of type T, exactly, and the method Primitive will be called, well, statically. This may be surprising, if you are used to other object-oriented languages.

If you define a method as "prodedure Dynamic(Self: T'Class)", the actual parameter can be either T or any of its descendents. Now, if you call Self.Primitive within the procedure Dynamic, it will be dispatching, i.e., it will call the primitive function matching the type of Self (i.e., either T or any of its subtype). This is what you would expect in many other object-oriented languages.

Finally, a static method can be defined as a subprogram within the same package that holds the object type and the other methods.

Specify the class My_Class, with one primitive subprogram, one dynamic subprogram and a static subprogram:

```Ada
   package My_Class is
      type Object is tagged private;
      procedure Primitive(Self: Object); -- primitive subprogram
      procedure Dynamic(Self: Object'Class);
      procedure Static;
   private
      type Object is tagged null record;
   end My_Class;
```


Implement the package:

```Ada
   package body My_Class is
      procedure Primitive(Self: Object) is
      begin
	 Put_Line("Hello World!");
      end Primitive;

      procedure Dynamic(Self: Object'Class) is
      begin
	 Put("Hi there! ... ");
	 Self.Primitive; -- dispatching call: calls different subprograms,
                         -- depending on the type of Self
      end Dynamic;

      procedure Static is
      begin
	 Put_Line("Greetings");
      end Static;
   end My_Class;
```


Specify and implement a subclass of My_Class:

```Ada
   package Other_Class is
      type Object is new My_Class.Object with null record;
      overriding procedure Primitive(Self: Object);
   end Other_Class;

   package body Other_Class is
      procedure Primitive(Self: Object) is
      begin
	 Put_Line("Hello Universe!");
      end Primitive;
   end Other_Class;
```


The main program, making the dynamic and static calls:


```Ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Call_Method is

   package My_Class is ... -- see above
   package body My_Class is ... -- see above

   package Other_Class is ... -- see above
   package body Other_Class is ... -- see above

   Ob1: My_Class.Object; -- our "root" type
   Ob2: Other_Class.Object; -- a type derived from the "root" type

begin
   My_Class.Static;
   Ob1.Primitive;
   Ob2.Primitive;
   Ob1.Dynamic;
   Ob2.Dynamic;
end Call_Method;
```


```txt
Greetings
Hello World!
Hello Universe!
Hi there! ... Hello World!
Hi there! ... Hello Universe!
```



## Apex


```Java
// Static
MyClass.method(someParameter);

// Instance
myInstance.method(someParameter);
```



## Arturo



```arturo
Person #{
	name ""
	surname ""

	init [n,s]{
		name n
		surname s
	}

	sayHello {
		print "Hello " + name + "!"
	}
}

person $(new ~Person "John" "Doe")
person.sayHello

```


```txt
Hello John!
```



## AutoHotkey

(AutoHotkey Basic does not have classes)

```AHK
class myClass
{
	Method(someParameter){
		MsgBox % SomeParameter
	}
}

myClass.method("hi")
myInstance := new myClass
myInstance.Method("bye")
```



## Bracmat

Bracmat has no classes. Rather, objects can be created as copies of other objects. In the example below, the variable <code>MyObject</code> has a copy of variable <code>MyClass</code> as value. Notice the extra level of reference in <code>(MyObject..Method)$argument</code>, comparable to <code>MyObject->Method(argument)</code> in C. Rather than being the object itself, <code>MyObject</code> points to an object and can be aliased by assigning to another variable.

Inside an object, the object is represented by <code>its</code>, comparable to <code>this</code> in other languages.

```bracmat
( ( myClass
  =   (name=aClass)
      ( Method
      = .out$(str$("Output from " !(its.name) ": " !arg))
      )
      (new=.!arg:?(its.name))
  )
& (myClass.Method)$"Example of calling a 'class' method"
& new$(myClass,object1):?MyObject
& (MyObject..Method)$"Example of calling an instance method"
& !MyObject:?Alias
& (Alias..Method)$"Example of calling an instance method from an alias"
);
```

Output:

```txt
Output from aClass: Example of calling a 'class' method
Output from object1: Example of calling an instance method
Output from object1: Example of calling an instance method from an alias
```



## ChucK

<lang>
MyClass myClassObject;
myClassObject.myFunction(some parameter);

```


## C++


```cpp
// Static
MyClass::method(someParameter);

// Instance
myInstance.method(someParameter);

// Pointer
MyPointer->method(someParameter);

```


=={{header|C_sharp|C#}}==

```c#
// Static
MyClass.Method(someParameter);

// Instance
myInstance.Method(someParameter);
```



## Clojure


```clojure
(Long/toHexString 15) ; use forward slash for static methods
(System/currentTimeMillis)

(.equals 1 2)    ; use dot operator to call instance methods
(. 1 (equals 2)) ; alternative style
```



## COBOL

COBOL has two ways to invoke a method: the <code>INVOKE</code> statement and inline method invocation.

```cobol
*> INVOKE
INVOKE FooClass "someMethod" RETURNING bar        *> Factory object
INVOKE foo-instance "anotherMethod" RETURNING bar *> Instance object

*> Inline method invocation
MOVE FooClass::"someMethod" TO bar        *> Factory object
MOVE foo-instance::"anotherMethod" TO bar *> Instance object
```


To call factory methods of objects of an unknown type (such as when you may have a subclass of the class wanted), it is necessary to get a reference to the class's factory object by calling the <code>"FactoryObject"</code> method.

```cobol
INVOKE foo-instance "FactoryObject" RETURNING foo-factory
*> foo-factory can be treated like a normal object reference.
INVOKE foo-factory "someMethod"
```



## CoffeeScript

While CoffeeScript does provide a useful class abstraction around its prototype-based inheritance, there aren't any actual classes.

```coffeescript
class Foo
    @staticMethod: -> 'Bar'

    instanceMethod: -> 'Baz'

foo = new Foo

foo.instanceMethod() #=> 'Baz'
Foo.staticMethod() #=> 'Bar'
```



## Common Lisp

In Common Lisp, classmethods are methods that apply to classes, rather than classes that contain methods.

```lisp
(defclass my-class ()
  ((x
    :accessor get-x  ;; getter function
    :initarg :x      ;; arg name
    :initform 0)))   ;; initial value

;; declaring a public class method
(defmethod square-x ((class-instance my-class))
  (* (get-x class-instance) (get-x class-instance)))

;; create an instance of my-class
(defvar *instance*
  (make-instance 'my-class :x 10))

(format t "Value of x: ~a~%" (get-x *instance*))

(format t "Value of x^2: ~a~%" (square-x *instance*))

```

Output (CLISP v2.49):

```txt
$ clisp object.cl
Value of x: 10
Value of x^2: 100

```



## D


```d
struct Cat {
    static int staticMethod() {
        return 2;
    }

    string dynamicMethod() { // Never virtual.
        return "Mew!";
    }
}

class Dog {
    static int staticMethod() {
        return 5;
    }

    string dynamicMethod() { // Virtual method.
        return "Woof!";
    }
}

void main() {
    // Static methods calls:
    assert(Cat.staticMethod() == 2);
    assert(Dog.staticMethod() == 5);

    Cat c; // This is a value on the stack.
    Dog d; // This is just a reference, set to null.

    // Other static method calls, discouraged:
    assert(c.staticMethod() == 2);
    assert(d.staticMethod() == 5);

    // Instance method calls:
    assert(c.dynamicMethod() == "Mew!");
    d = new Dog;
    assert(d.dynamicMethod() == "Woof!");
}
```



## Dyalect


Dyalect supports both instance and static methods. Instance methods have a special <code>this</code> reference that returns an instance. Static method are always invoked through the type name.


```dyalect
//Static method on a built-in type Integer
static func Integer.div(x, y) {
    x / y
}

//Instance method
func Integer.div(n) {
    this / n
}

print(Integer.div(12, 3))
print(12.div(3))
```


```txt
4
4
```



## E

A method call in E has the syntax <code><var>recipient</var>.<var>verb</var>(<var>arg1</var>, <var>arg2</var>, <var>...</var>)</code>, where <var>recipient</var> is an expression, <var>verb</var> is an identifier (or a string literal preceded by <code>::</code>), and <var>argN</var> are expressions.


```e
someObject.someMethod(someParameter)
```


In E, there are no distinguished "static methods". Instead, it is idiomatic to place methods on the maker of the object. This is very similar to methods on constructors in JavaScript, or class methods in Objective-C.


## Elena

The message call:

```elena

    console.printLine("Hello"," ","World!");

```



## Elixir

Elixir doesn't do objects.  Instead of calling methods on object you send messages to processes.  Here's an example of a process created with spawn_link which knows how to receive a message "concat" and return a result.

```elixir

defmodule ObjectCall do
  def new() do
    spawn_link(fn -> loop end)
  end

  defp loop do
    receive do
      {:concat, {caller, [str1, str2]}} ->
        result = str1 <> str2
        send caller, {:ok, result}
        loop
    end
  end

  def concat(obj, str1, str2) do
    send obj, {:concat, {self(), [str1, str2]}}

    receive do
      {:ok, result} ->
        result
    end
  end
end

obj = ObjectCall.new()

IO.puts(obj |> ObjectCall.concat("Hello ", "World!"))

```



## Factor

In Factor, there is no distinction between instance and static methods. Methods are contained in generic words and specialize on a class. Generic words define a <i>method combination</i> so methods know which object(s) to dispatch on. (But most methods dispatch on the object at the top of the data stack.) Under this object model, calling a method is no different than calling any other word.


```factor
USING: accessors io kernel literals math sequences ;
IN: rosetta-code.call-a-method

! Define some classes.
SINGLETON: dog
TUPLE: cat sassiness ;

! Define a constructor for cat.
C: <cat> cat

! Define a generic word that dispatches on the object at the top
! of the data stack.
GENERIC: speak ( obj -- )

! Define methods in speak which specialize on various classes.
M: dog speak drop "Woof!" print ;
M: cat speak sassiness>> 0.5 > "Hiss!" "Meow!" ? print ;
M: object speak drop "I don't know how to speak!" print ;

! Place some objects of various classes in a sequence.
${ dog 0.75 <cat> 0.1 <cat> 10 }
! Call speak, a method, just like any other word.
[ speak ] each
```

```txt

Woof!
Hiss!
Meow!
I don't know how to speak!

```



## Forth

There are numerous, mutually incompatible object oriented frameworks for Forth. This one works with the FOOS preprocessor extension of [[4tH]].

```forth
include lib/compare.4th
include 4pp/lib/foos.4pp

[ASSERT]                               \ enable assertions

:: Cat
   class
     method: dynamicCat                \ virtual method
   end-class {

    :static staticCat { 2 } ;          \ static method
    :method { s" Mew!" } ; defines dynamicCat
  }                                    \ for unrelated classes,
;                                      \ method names have to differ

:: Dog
   class
     method: dynamicDog                \ virtual method
   end-class {

    :static staticDog { 5 } ;
    :method { s" Woof!" } ; defines dynamicDog
  }                                    \ for unrelated classes,
;                                      \ method names have to differ

static Cat c                           \ create two static objects
static Dog d

: main
  assert( class -> staticCat 2 = )     \ check for valid method return
  assert( class -> staticDog 5 = )     \ of a static method

  assert( c -> staticCat 2 = )         \ check for valid method return
  assert( d -> staticDog 5 = )         \ of a static method

  assert( c => dynamicCat s" Mew!"  compare 0= )
  assert( d => dynamicDog s" Woof!" compare 0= )
;                                      \ same for dynamic methods

main
```


Works with any ANS Forth

Needs the FMS-SI (single inheritance) library code located here:
http://soton.mpeforth.com/flag/fms/index.html

```forth
include FMS-SI.f

:class animal
  variable cnt 0 cnt ! \ static instance variable
  :m init: 1 cnt +! ;m
  :m cnt: cnt @ . ;m
;class

:class cat <super animal
  :m speak ." meow" ;m
;class

:class dog <super animal
  :m speak ." woof" ;m
;class

cat Frisky   \ instantiate a cat object named Frisky
dog Sparky   \ instantiate a dog object named Sparky

\ The class method cnt: will return the number of animals instantiated
\ regardless of which animal object is used.

\ The instance method speak will respond differently depending
\ on the class of the instance object.

Frisky cnt:  \ => 2 ok
Sparky cnt:  \ => 2 ok
Frisky speak \ => meow ok
Sparky speak \ => woof ok

```



## Fortran

In modern Fortran a "derived type" concept corresponds to "class" in OOP. Such types have "type bound procedures", i.e. static methods. Procedure pointer components depend on the value of the object (so they are object-bound), can be redefined runtime and correspond approx to instances.

```Fortran

! type declaration
type my_type
 contains
procedure, pass :: method1
procedure, pass, pointer :: method2
end type my_type

! declare object of type my_type
type(my_type) :: mytype_object

!static call
 call mytype_object%method1() ! call method1 defined as subroutine
!instance?
 mytype_object%method2() ! call method2 defined as function


```



## FreeBASIC


```freebasic

' FB 1.05.0 Win64

Type MyType
  Public:
    Declare Sub InstanceMethod(s As String)
    Declare Static Sub StaticMethod(s As String)
  Private:
    dummy_ As Integer ' types cannot be empty in FB
End Type

Sub MyType.InstanceMethod(s As String)
  Print s
End Sub

Static Sub MyType.StaticMethod(s As String)
  Print s
End Sub

Dim t As MyType
t.InstanceMethod("Hello world!")
MyType.Staticmethod("Hello static world!")
Print
Print "Press any key to quit the program"
Sleep

```


```txt

Hello world!
Hello static world!

```



## Go

Go distances itself from the word "object" and from many object oriented concepts.  It does however have methods.  Any user-defined type in Go can have methods and these work very much like "instance methods" of object oriented languages.  The examples below illustrate details of Go methods and thus represent the concept of instance methods.

```go
type Foo int // some custom type

// method on the type itself; can be called on that type or its pointer
func (self Foo) ValueMethod(x int) { }

// method on the pointer to the type; can be called on pointers
func (self *Foo) PointerMethod(x int) { }


var myValue Foo
var myPointer *Foo = new(Foo)

// Calling value method on value
myValue.ValueMethod(someParameter)
// Calling pointer method on pointer
myPointer.PointerMethod(someParameter)

// Value methods can always be called on pointers
// equivalent to (*myPointer).ValueMethod(someParameter)
myPointer.ValueMethod(someParameter)

// In a special case, pointer methods can be called on values that are addressable (i.e. lvalues)
// equivalent to (&myValue).PointerMethod(someParameter)
myValue.PointerMethod(someParameter)

// You can get the method out of the type as a function, and then explicitly call it on the object
Foo.ValueMethod(myValue, someParameter)
(*Foo).PointerMethod(myPointer, someParameter)
(*Foo).ValueMethod(myPointer, someParameter)
```

Go has no direct equivalent to class methods as long as you think of a Go type as a class.  A Go ''package'' however can be organized and used much like a class, and in this context, any function exported from the package works much like a class method.

An example package:

```go
package box

import "sync/atomic"

var sn uint32

type box struct {
    Contents string
    secret uint32
}

func New() (b *box) {
    b = &box{secret: atomic.AddUint32(&sn, 1)}
    switch sn {
    case 1:
        b.Contents = "rabbit"
    case 2:
        b.Contents = "rock"
    }
    return
}

func (b *box) TellSecret() uint32 {
    return b.secret
}

func Count() uint32 {
    return atomic.LoadUint32(&sn)
}
```

Example use:

```go
package main

import "box"

func main() {
    // Call constructor.  Technically it's just an exported function,
    // but it's a Go idiom to naming a function New that serves the purpose
    // of a constructor.
    b := box.New()

    // Call instance method.  In Go terms, simply a method.
    b.TellSecret()

    // Call class method.  In Go terms, another exported function.
    box.Count()
}
```


==Icon and {{header|Unicon}}==
Icon does not have objects or methods; they can be implemented on top of other types such as records.

Unicon has no concept of static methods; all methods are normally invoked using an instance of a class. While it is technically possible to call a method without an instance, it requires knowledge of the underlying implementation, such as the name-mangling conventions, and is non-standard.

```unicon
procedure main()

   bar := foo()  # create instance
   bar.m2()      # call method m2 with self=bar, an implicit first parameter

   foo_m1( , "param1", "param2")  # equivalent of static class method, first (self) parameter is null
end

class foo(cp1,cp2)
   method m1(m1p1,m1p2)
      local ml1
      static ms1
      ml1 := m1p1
      # do something
      return
   end
   method m2(m2p1)
      # do something else
      return
   end
initially
   L := [cp1]
end
```



## J


Note: In some languages "everything is an object".  This is not the case in J.  In J, non-trivial objects will contain arrays.  This is a relatively deep issue, beyond the scope of this page, except that: method invocations are relatively rare, in J.

All method invocations must contain two underline characters in J:

Static:


```j
methodName_className_ parameters
```


and, given an object instance reference:


```j
objectReference=:'' conew 'className'
```


an instance invocation could be:


```j
methodName__objectReference parameters
```


Note that J also supports infix notation when using methods.  In this case, there will be a second parameter list, on the left of the method reference.


```j
parameters methodName_className_ parameters
```

or

```j
parameters methodName__objectReference parameters
```


These variations might be useful when building combining words that need to refer to two different kinds of things.  But mostly it's to be consistent with the rest of the language.

Finally, note that static methods can be referred to using the same notation as instance methods -- in this case you must have a reference to the class name:


```j
classReference=: <'className'
methodName__classReference parameters
```


This might be useful when you are working with a variety of classes which share a common structure.

You can also refer to a dynamic method in the same fashion that you use to refer to a instance method -- in this case you must know the object name (which is a number).  For example, to refer to a method on object 123


```j
methodName_123_ parameters
```


This last case can be useful when debugging.


## Java

Static methods in Java are usually called by using the dot operator on a class name:

```java
ClassWithStaticMethod.staticMethodName(argument1, argument2);//for methods with no arguments, use empty parentheses
```

Instance methods are called by using the dot operator on an instance:

```java
ClassWithMethod varName = new ClassWithMethod();
varName.methodName(argument1, argument2);
//or
new ClassWithMethod().methodName(argument1, argument2);
```


Instance methods may not be called on references whose value is <code>null</code> (throws a <code>NullPointerException</code>). <!--Maybe add reflection stuff here too? It might be too complicated or it might not belong here-->

Note: Static methods can also be called using the dot syntax on a reference to an instance, looking identical to an instance method call, but this is highly discouraged, and compilers usually complain about that (with a warning, not a failed compilation). The reason for this is that such a call is actually equivalent to a static method call on the ''static (compile-time) type'' of the reference in the source code. That means the runtime value of that reference is actually irrelevant, and the reference may be <code>null</code> or be a reference to an instance of a subclass, the compiler will still make it a call to the static method based on the class of the value at compile-time (i.e. inheritance does not apply; the method call is not resolved at runtime).

This means that there is no benefit to making a static method call this way, because one can make the call based on the static type that is already known from looking at the code when it is written. Furthermore, such a call may lead to serious pitfalls, leading one to believe that changing the object that it is called on could change the behavior. For example, a common snippet to sleep the thread for 1 second is the following: <code>Thread.currentThread().sleep(1000);</code>. However, since <code>.sleep()</code> is actually a static method, the compiler replaces it with <code>Thread.sleep(1000);</code> (since <code>Thread.currentThread()</code> has a return type of <code>Thread</code>). This makes it clear that the thread that is put to sleep is not under the user's control. However, written as <code>Thread.currentThread().sleep(1000);</code>, it may lead one to falsely believe it to be possible to sleep another thread by doing <code>someOtherThread.sleep(1000);</code> when in fact such a statement would also sleep the current thread.


## JavaScript

If you have a object called <tt>x</tt> and a method called <tt>y</tt> then you can write:

```javascript
x.y()
```



## Julia

    module Animal

    abstract type Animal end
    abstract type Dog <: Animal end
    abstract type Cat <: Animal end

    struct Collie <: Dog
        name::String
        weight::Float64
    end

    struct Persian <: Cat
        name::String
        weight::Float64
    end

    #
    # This function is static: it works on any dog, even on a blank instance, in
    #   the same way, to produce the same output for anything that isa Dog in type.
    #
    function soundalert(a::T) where T <: Dog
        println("Woof!")
    end

    #
    # This function depends on the class instance's data for its value, so this type
    #   of function is one way to do an instance method in Jula.
    #
    masskg(x) = x.weight


## Kotlin

Kotlin does not have static methods as such but they can be easily simulated by 'companion object' methods :

```scala
class MyClass {
    fun instanceMethod(s: String) = println(s)

    companion object {
        fun staticMethod(s: String) = println(s)
    }
}

fun main(args: Array<String>) {
    val mc = MyClass()
    mc.instanceMethod("Hello instance world!")
    MyClass.staticMethod("Hello static world!")
}
```


```txt

Hello instance world!
Hello static world!

```



## Latitude


In Latitude, everything is an object. Being prototype-oriented, Latitude does not distinguish between classes and instances. Calling a method on either a class or an instance is done by simple juxtaposition.

```latitude
myObject someMethod (arg1, arg2, arg3).
MyClass someMethod (arg1, arg2, arg3).
```

The parentheses on the argument list may be omitted if there is at most one argument and the parse is unambiguous.

```latitude
myObject someMethod "string constant argument".
myObject someMethod (argument). ;; Parentheses are necessary here
myObject someMethod. ;; No arguments
```

Finally, a colon may be used instead of parentheses, in which case the rest of the line is considered to be the argument list.

```latitude
myObject someMethod: arg1, arg2, arg3.
```



## LFE


LFE/Erlang doesn't have an object system. However, since LFE is a Lisp, one can do the same thing in LFE as one did in Lisps before CLOS: use closures to create and work with objects.

Not only that, but one may also use Erlang lightweight process to accomplish something very similar!

Examples for both approaches are given below.



### With Closures


```lisp
(defmodule aquarium
 (export all))

(defun fish-class (species)
  "
  This is the constructor that will be used most often, only requiring that
  one pass a 'species' string.

  When the children are not defined, simply use an empty list.
  "
  (fish-class species ()))

(defun fish-class (species children)
  "
  This contructor is mostly useful as a way of abstracting out the id
  generation from the larger constructor. Nothing else uses fish-class/2
  besides fish-class/1, so it's not strictly necessary.

  When the id isn't know, generate one.
  "
  (let* (((binary (id (size 128))) (: crypto rand_bytes 16))
         (formatted-id (car
                         (: io_lib format
                           '"~32.16.0b" (list id)))))
    (fish-class species children formatted-id)))

(defun fish-class (species children id)
  "
  This is the constructor used internally, once the children and fish id are
  known.
  "
  (let ((move-verb '"swam"))
    (lambda (method-name)
      (case method-name
        ('id
          (lambda (self) id))
        ('species
          (lambda (self) species))
        ('children
          (lambda (self) children))
        ('info
          (lambda (self)
            (: io format
              '"id: ~p~nspecies: ~p~nchildren: ~p~n"
              (list (get-id self)
                    (get-species self)
                    (get-children self)))))
        ('move
          (lambda (self distance)
            (: io format
              '"The ~s ~s ~p feet!~n"
              (list species move-verb distance))))
        ('reproduce
          (lambda (self)
            (let* ((child (fish-class species))
                   (child-id (get-id child))
                   (children-ids (: lists append
                                   (list children (list child-id))))
                   (parent-id (get-id self))
                   (parent (fish-class species children-ids parent-id)))
              (list parent child))))
        ('children-count
          (lambda (self)
            (: erlang length children)))))))

(defun get-method (object method-name)
  "
  This is a generic function, used to call into the given object (class
  instance).
  "
  (funcall object method-name))

; define object methods
(defun get-id (object)
  (funcall (get-method object 'id) object))

(defun get-species (object)
  (funcall (get-method object 'species) object))

(defun get-info (object)
  (funcall (get-method object 'info) object))

(defun move (object distance)
  (funcall (get-method object 'move) object distance))

(defun reproduce (object)
  (funcall (get-method object 'reproduce) object))

(defun get-children (object)
  (funcall (get-method object 'children) object))

(defun get-children-count (object)
  (funcall (get-method object 'children-count) object))

```


With this done, one can create objects and interact with them. Here is some usage from the LFE REPL:

```lisp
; Load the file and create a fish-class instance:

> (slurp '"object.lfe")
#(ok object)
> (set mommy-fish (fish-class '"Carp"))
#Fun<lfe_eval.10.91765564>

; Execute some of the basic methods:

> (get-species mommy-fish)
"Carp"
> (move mommy-fish 17)
The Carp swam 17 feet!
ok
> (get-id mommy-fish)
"47eebe91a648f042fc3fb278df663de5"

; Now let's look at "modifying" state data (e.g., children counts):

> (get-children mommy-fish)
()
> (get-children-count mommy-fish)
0
> (set (mommy-fish baby-fish-1) (reproduce mommy-fish))
(#Fun<lfe_eval.10.91765564> #Fun<lfe_eval.10.91765564>)
> (get-id mommy-fish)
"47eebe91a648f042fc3fb278df663de5"
> (get-id baby-fish-1)
"fdcf35983bb496650e558a82e34c9935"
> (get-children-count mommy-fish)
1
> (set (mommy-fish baby-fish-2) (reproduce mommy-fish))
(#Fun<lfe_eval.10.91765564> #Fun<lfe_eval.10.91765564>)
> (get-id mommy-fish)
"47eebe91a648f042fc3fb278df663de5"
> (get-id baby-fish-2)
"3e64e5c20fb742dd88dac1032749c2fd"
> (get-children-count mommy-fish)
2
> (get-info mommy-fish)
id: "47eebe91a648f042fc3fb278df663de5"
species: "Carp"
children: ["fdcf35983bb496650e558a82e34c9935",
           "3e64e5c20fb742dd88dac1032749c2fd"]
ok
```



### With Lightweight Processes


```lisp
(defmodule object
 (export all))

(defun fish-class (species)
  "
  This is the constructor that will be used most often, only requiring that
  one pass a 'species' string.

  When the children are not defined, simply use an empty list.
  "
  (fish-class species ()))

(defun fish-class (species children)
  "
  This constructor is useful for two reasons:
    1) as a way of abstracting out the id generation from the
       larger constructor, and
    2) spawning the 'object loop' code (fish-class/3).
  "
  (let* (((binary (id (size 128))) (: crypto rand_bytes 16))
         (formatted-id (car
                         (: io_lib format
                           '"~32.16.0b" (list id)))))
    (spawn 'object
           'fish-class
           (list species children formatted-id))))

(defun fish-class (species children id)
  "
  This function is intended to be spawned as a separate process which is
  used to track the state of a fish. In particular, fish-class/2 spawns
  this function (which acts as a loop, pattern matching for messages).
  "
  (let ((move-verb '"swam"))
    (receive
      ((tuple caller 'move distance)
        (! caller (list species move-verb distance))
        (fish-class species children id))
      ((tuple caller 'species)
        (! caller species)
        (fish-class species children id))
      ((tuple caller 'children)
        (! caller children)
        (fish-class species children id))
      ((tuple caller 'children-count)
        (! caller (length children))
        (fish-class species children id))
      ((tuple caller 'id)
        (! caller id)
        (fish-class species children id))
      ((tuple caller 'info)
        (! caller (list id species children))
        (fish-class species children id))
      ((tuple caller 'reproduce)
        (let* ((child (fish-class species))
               (child-id (get-id child))
               (children-ids (: lists append
                               (list children (list child-id)))))
        (! caller child)
        (fish-class species children-ids id))))))

(defun call-method (object method-name)
  "
  This is a generic function, used to call into the given object (class
  instance).
  "
  (! object (tuple (self) method-name))
  (receive
    (data data)))

(defun call-method (object method-name arg)
  "
  Same as above, but with an additional argument.
  "
  (! object (tuple (self) method-name arg))
  (receive
    (data data)))

; define object methods
(defun get-id (object)
  (call-method object 'id))

(defun get-species (object)
  (call-method object 'species))

(defun get-info (object)
  (let ((data (call-method object 'info)))
    (: io format '"id: ~s~nspecies: ~s~nchildren: ~p~n" data)))

(defun move (object distance)
  (let ((data (call-method object 'move distance)))
    (: io format '"The ~s ~s ~p feet!~n" data)))

(defun reproduce (object)
  (call-method object 'reproduce))

(defun get-children (object)
  (call-method object 'children))

(defun get-children-count (object)
  (call-method object 'children-count))
```


Here is some usage from the LFE REPL:

```lisp
; Load the file and create a fish-class instance:

> (slurp '"object.lfe")
#(ok object)
> (set mommy-fish (fish-class '"Carp"))
<0.33.0>

; Execute some of the basic methods:

> (get-species mommy-fish)
"Carp"
> (move mommy-fish 17)
The Carp swam 17 feet!
ok
> (get-id mommy-fish)
"47eebe91a648f042fc3fb278df663de5"

; Now let's look at modifying state data:

> (get-children mommy-fish)
()
> (get-children-count mommy-fish)
0
> (set baby-fish-1 (reproduce mommy-fish))
<0.34.0>
> (get-id mommy-fish)
"47eebe91a648f042fc3fb278df663de5"
> (get-id baby-fish-1)
"fdcf35983bb496650e558a82e34c9935"
> (get-children-count mommy-fish)
1
> (set baby-fish-2 (reproduce mommy-fish))
<0.35.0>
> (get-id mommy-fish)
"47eebe91a648f042fc3fb278df663de5"
> (get-id baby-fish-2)
"3e64e5c20fb742dd88dac1032749c2fd"
> (get-children-count mommy-fish)
2
> (get-info mommy-fish)
id: 47eebe91a648f042fc3fb278df663de5
species: Carp
children: ["fdcf35983bb496650e558a82e34c9935",
           "3e64e5c20fb742dd88dac1032749c2fd"]
ok
```



## Lingo


```lingo
-- call static method
script("MyClass").foo()

-- call instance method
obj = script("MyClass").new()
obj.foo()
```



## Logtalk

In Logtalk, class or instance are ''roles'' that an object can play depending on the relations in other objects. Thus, a "class" can be defined by having an object specializing another object and an instance can be defined by having an object instantiating another object. Metaclasses are easily defined and thus a class method is just an instance method defined in the class metaclass.

```logtalk

% avoid infinite metaclass regression by
% making the metaclass an instance of itself
:- object(metaclass,
    instantiates(metaclass)).

    :- public(me/1).
    me(Me) :-
        self(Me).

:- end_object.

```


```logtalk

:- object(class,
    instantiates(metaclass)).

    :- public(my_class/1).
    my_class(Class) :-
        self(Self),
        instantiates_class(Self, Class).

:- end_object.

```


```logtalk

:- object(instance,
    instantiates(class)).

:- end_object.

```

Testing:

```logtalk

| ?- class::me(Me).
Me = class
yes

| ?- instance::my_class(Class).
Class = class
yes

```



## Lua

Lua does not provide a specific OO implementation, but its metatable mechanism and sugar for method-like calls does make it easy to implement and use such as system, as evidenced by its multitude of class libraries in particular.

Instance methods, in their most basic form, are simply function calls where the calling object reference is duplicated and passed as the first argument. Lua offers some syntactical sugar to facilitate this, via the colon operator:

```lua
local object = { name = "foo", func = function (self) print(self.name) end }

object:func() -- with : sugar
object.func(object) -- without : sugar
```


Using metatables, and specifically the __index metamethod, it is possible to call a method stored in an entirely different table, while still passing the object used for the actual call:

```lua
local methods = { }
function methods:func () -- if a function is declared using :, it is given an implicit 'self' parameter
    print(self.name)
end

local object = setmetatable({ name = "foo" }, { __index = methods })

object:func() -- with : sugar
methods.func(object) -- without : sugar
```


Lua does not have a specific way of handling static methods, but similarly to Go, they could simply be implemented as regular functions inside of a module.

Example module, named <code>box.lua</code>:

```lua
local count = 0
local box = { }
local boxmt = { __index = box }
function box:tellSecret ()
    return self.secret
end

local M = { }
function M.new ()
    count = count + 1
    return setmetatable({ secret = count, contents = count % 2 == 0 and "rabbit" or "rock" }, boxmt)
end
function M.count ()
    return count
end
return M
```


Example use:

```lua
local box = require 'box'

local b = box.new()

print(b:tellSecret())
print(box.count())
```



## M2000 Interpreter

In M2000 there are some kinds of objects. Group is the one type object. We can compose members to groups, with functions, operators, modules, events, and inner groups, among other members of type pointer to object. Another type is the COM type. Here we see the Group object

```M2000 Interpreter

Module CheckIt {
      \\ A class definition is a function which return a Group
      \\ We can make groups and we can alter them using Group statement
      \\ Groups may have other groups inside

      Group Alfa {
      Private:
            myvalue=100
      Public:
            Group SetValue {
                  Set (x) {
                        Link parent myvalue to m
                        m<=x
                  }
            }
            Module MyMethod {
                 Read x
                 Print x*.myvalue
            }
      }

      Alfa.MyMethod 5 '500
      Alfa.MyMethod %x=200   ' 20000
      \\ we can copy Alfa to Z
      Z=Alfa
      Z.MyMethod 5
      Z.SetValue=300
      Z.MyMethod 5 ' 1500
      Alfa.MyMethod 5    ' 500
      Dim A(10)
      A(3)=Z
      A(3).MyMethod 5 '1500
      A(3).SetValue=200
      A(3).MyMethod 5 '1000
      \\ get a pointer of group in A(3)
      k->A(3)
      k=>SetValue=100
      A(3).MyMethod 5 '500
      \\ k get pointer to Alfa
      k->Alfa
      k=>SetValue=500
      Alfa.MyMethod 5 '2500
      k->Z
      k=>MyMethod 5 ' 1500
      Z.SetValue=100
      k=>MyMethod 5 ' 500
}
Checkit

```





## Maple

There is no real difference in how you call a static or instance method.

```Maple
# Static
Method( obj, other, arg );
```


```Maple
# Instance
Method( obj, other, arg );
```



## MiniScript

MiniScript uses prototype-based inheritance, so the only difference between a static method and an instance method is in whether it uses the <code>self</code> pseudo-keyword.

```MiniScript
Dog = {}
Dog.name = ""
Dog.help = function()
    print "This class represents dogs."
end function
Dog.speak = function()
    print self.name + " says Woof!"
end function

fido = new Dog
fido.name = "Fido"

Dog.help    // calling a "class method"
fido.speak  // calling an "instance method"
```


```txt
This class represents dogs.
Fido says Woof!
```



## Nemerle


```Nemerle
// Static
MyClass.Method(someParameter);

// Instance
myInstance.Method(someParameter);
```



## NetRexx

Like [[#Java|Java]], static methods in NetRexx are called by using the dot operator on a class name:

```NetRexx
SomeClass.staticMethod()
```

Instance methods are called by using the dot operator on an instance:

```NetRexx
objectInstance = SomeClass() -- create a new instance of the class
objectInstance.instanceMethod() -- call the instance method

SomeClass().instanceMethod() -- same as above; create a new instance of the class and call the instance method immediately
```



## Nim

In Nim there are no object methods, but regular procedures can be called with method call syntax:

```nim
var x = @[1, 2, 3]
add(x, 4)
x.add(5)
```



## OASYS Assembler

The OASYS runtime is strange; all classes share the same methods and properties, there are no static methods and properties, and there are no procedures/functions other than methods on objects. However, the initialization method can be called on a null object (no other method has this possibility).

The following code calls a method called <tt>&amp;GO</tt> on the current object:
<lang oasys_oaa>+&GO
```



## Objeck


```objeck

ClassName->some_function(); # call class function
instance->some_method(); # call instance method
```

Objeck uses the same syntax for instance and class method calls. In Objeck, functions are the equivalent of public static methods.


## Object Pascal


Object Pascal as implemented in Delphi and Free Pascal supports both static (known in Pascal as class method) and instance methods. Free Pascal has two levels of static methods, one using the class keyword and one using both the class and the static keywords. A class method can be called in Pascal, while a static class method could be called even from C, that's the main difference.


```pascal
// Static (known in Pascal as class method)
MyClass.method(someParameter);

// Instance
myInstance.method(someParameter);

```


=={{header|Objective-C}}==
In Objective-C, calling an instance method is sending a message to an instance, and calling a class method is sending a message to a class object. Class methods are inherited through inheritance of classes. All messages (whether sent to a normal object or class object) are resolved dynamically at runtime, hence there are no "static" methods (see also: Smalltalk).

```objc
// Class
[MyClass method:someParameter];
// or equivalently:
id foo = [MyClass class];
[foo method:someParameter];

// Instance
[myInstance method:someParameter];

// Method with multiple arguments
[myInstance methodWithRed:arg1 green:arg2 blue:arg3];

// Method with no arguments
[myInstance method];
```



## OCaml


We can only call the method of an instantiated object:


```ocaml
my_obj#my_meth params
```



## Oforth

When a method is called, the top of the stack is used as the object on which the method will be applyed :

```Oforth>1.2 sqrt</lang


For class methods, the top of the stack must be a class (which is also an object of Class class) :

```Oforth>Date now</lang



## ooRexx


```ooRexx
say "pi:" .circle~pi
c=.circle~new(1)
say "c~area:" c~area
Do r=2 To 10
 c.r=.circle~new(r)
 End
say .circle~instances('') 'circles were created'

::class circle
::method pi class -- a class method
  return 3.14159265358979323

::method instances class -- another class method
  expose in
  use arg a
  If datatype(in)<>'NUM' Then in=0
  If a<>'' Then
    in+=1
  Return in

::method init
  expose radius
  use arg radius
  self~class~instances('x')

::method area     -- an instance method
  expose radius
  Say self~class
  Say self
  return self~class~pi * radius * radius
```

'''Output:'''

```txt
pi: 3.14159265358979323
The CIRCLE class
a CIRCLE
c~area: 3.14159265
10 circles were created
```



## Perl


```perl
# Class method
MyClass->classMethod($someParameter);
# Equivalently using a class name
my $foo = 'MyClass';
$foo->classMethod($someParameter);


# Instance method
$myInstance->method($someParameter);

# Calling a method with no parameters
$myInstance->anotherMethod;

# Class and instance method calls are made behind the scenes by getting the function from
# the package and calling it on the class name or object reference explicitly
MyClass::classMethod('MyClass', $someParameter);
MyClass::method($myInstance, $someParameter);
```



## Perl 6

###  Basic method calls


```perl6
class Thing {
  method regular-example() { say 'I haz a method' }

  multi method multi-example() { say 'No arguments given' }
  multi method multi-example(Str $foo) { say 'String given' }
  multi method multi-example(Int $foo) { say 'Integer given' }
};

# 'new' is actually a method, not a special keyword:
my $thing = Thing.new;

# No arguments: parentheses are optional
$thing.regular-example;
$thing.regular-example();
$thing.multi-example;
$thing.multi-example();

# Arguments: parentheses or colon required
$thing.multi-example("This is a string");
$thing.multi-example: "This is a string";
$thing.multi-example(42);
$thing.multi-example: 42;

# Indirect (reverse order) method call syntax: colon required
my $foo = new Thing: ;
multi-example $thing: 42;

```


=== Meta-operators ===

The <code>.</code> operator can be decorated with meta-operators.


```perl6

my @array = <a z c d y>;
@array .= sort;  # short for @array = @array.sort;

say @array».uc;  # uppercase all the strings: A C D Y Z

```



###  Classless methods


A method that is not in a class can be called by using the <code>&</code> sigil explicitly.


```perl6

my $object = "a string";  # Everything is an object.
my method example-method {
    return "This is { self }.";
}

say $object.&example-method;  # Outputs "This is a string."

```



## PHP


```php
// Static method
MyClass::method($someParameter);
// In PHP 5.3+, static method can be called on a string of the class name
$foo = 'MyClass';
$foo::method($someParameter);


// Instance method
$myInstance->method($someParameter);
```



## PicoLisp

Method invocation is syntactically equivalent to normal function calls. Method names have a trailing '>' by convention.

```PicoLisp
(foo> MyClass)
(foo> MyObject)
```


## Pike

Pike does not have static methods. (the <code>static</code> modifier in Pike is similar to <code>protected</code> in C++).

regular methods can be called in these ways:

```Pike
obj->method();
obj["method"]();
call_function(obj->method);
call_function(obj["method"]);
```

<code>call_function()</code> is rarely used anymore.

because <code>()</code> is actually an operator that is applied to a function reference, the following is also possible:

```Pike
function func = obj->method;
func();
```

as alternative to static function, modules are used. a module is essentially a static class. a function in a module can be called like this:

```Pike
module.func();
module["func"]();
```

it should be noted that <code>module.func</code> is resolved at compile time while <code>module["func"]</code> is resolved at runtime.


## PL/SQL


```PLSQL
create or replace TYPE myClass AS OBJECT (
    -- A class needs at least one member even though we don't use it
    dummy NUMBER,
    STATIC FUNCTION static_method RETURN VARCHAR2,
    MEMBER FUNCTION instance_method RETURN VARCHAR2
);
/
CREATE OR REPLACE TYPE BODY myClass AS
    STATIC FUNCTION static_method RETURN VARCHAR2 IS
    BEGIN
        RETURN 'Called myClass.static_method';
    END static_method;

    MEMBER FUNCTION instance_method RETURN VARCHAR2 IS
    BEGIN
        RETURN 'Called myClass.instance_method';
    END instance_method;
END;
/

DECLARE
    myInstance myClass;
BEGIN
    myInstance := myClass(null);
    DBMS_OUTPUT.put_line( myClass.static_method() );
    DBMS_OUTPUT.put_line( myInstance.instance_method() );
END;/
```



## PowerShell


```PowerShell
$Date = Get-Date
$Date.AddDays( 1 )
[System.Math]::Sqrt( 2 )
```



## Processing


```processing
// define a rudimentary class
class HelloWorld
{
    public static void sayHello()
    {
        println("Hello, world!");
    }
    public void sayGoodbye()
    {
        println("Goodbye, cruel world!");
    }
}

// call the class method
HelloWorld.sayHello();

// create an instance of the class
HelloWorld hello = new HelloWorld();

// and call the instance method
hello.sayGoodbye();
```



## Python


```python
class MyClass(object):
	@classmethod
	def myClassMethod(self, x):
		pass
	@staticmethod
	def myStaticMethod(x):
		pass
	def myMethod(self, x):
		return 42 + x

myInstance = MyClass()

# Instance method
myInstance.myMethod(someParameter)
# A method can also be retrieved as an attribute from the class, and then explicitly called on an instance:
MyClass.myMethod(myInstance, someParameter)


# Class or static methods
MyClass.myClassMethod(someParameter)
MyClass.myStaticMethod(someParameter)
# You can also call class or static methods on an instance, which will simply call it on the instance's class
myInstance.myClassMethod(someParameter)
myInstance.myStaticMethod(someParameter)
```



## Racket


The following snippet shows a call to the <tt>start</tt> method of the <tt>timer%</tt> class.


```racket
#lang racket/gui

(define timer (new timer%))
(send timer start 100)
```



## Ring


```ring

new point { print() }
Class Point
        x = 10  y = 20  z = 30
        func print see x + nl + y + nl + z + nl

```


```ring

o1 = new System.output.console
o1.print("Hello World")

Package System.Output
        Class Console
                Func Print cText
                        see cText + nl

```



## Ruby


```ruby
# Class method
MyClass.some_method(some_parameter)

# Class may be computed at runtime
foo = MyClass
foo.some_method(some_parameter)


# Instance method
my_instance.method(some_parameter)

# The parentheses are optional
my_instance.method some_parameter

# Calling a method with no parameters
my_instance.another_method
```



## Rust


```rust
struct Foo;

impl Foo {
    // implementation of an instance method for struct Foo
    // returning the answer to life
    fn get_the_answer_to_life(&self) -> i32 {
        42
    }

    // implementation of a static method for struct Foo
    // returning a new instance object
    fn new() -> Foo {
        println!("Hello, world!");
        Foo // returning the new Foo object
    }
}

fn main() {
    // create the instance object foo,
    // by calling the static method new of struct Foo
    let foo = Foo::new();

    // get the answer to life
    // by calling the instance method of object foo
    println!("The answer to life is {}.", foo.get_the_answer_to_life());

    // Note that in Rust, methods still work on references to the object.
    // Rust will automatically do the appropriate dereferencing to get the method to work:
    let lots_of_references = &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&foo;
    println!("The answer to life is still {}." lots_of_references.get_the_answer_to_life());
}
```



## Scala


```scala
/* This class implicitly includes a constructor which accepts an Int and
 *  creates "val variable1: Int" with that value.
 */
class MyClass(val memberVal: Int) { // Acts like a getter, getter automatically generated.
  var variable2 = "asdf" // Another instance variable; a public mutable this time
  def this() = this(0) // An auxilliary constructor that instantiates with a default value
}

object HelloObject {
  val s = "Hello" // Not private, so getter auto-generated
}

/** Demonstrate use of our example class.
 */
object Call_an_object_method extends App {
  val s = "Hello"
  val m = new MyClass
  val n = new MyClass(3)

  assert(HelloObject.s == "Hello") // "Hello" by object getterHelloObject
  assert(m.memberVal == 0)
  assert(n.memberVal == 3)
  println("Successfully completed without error.")
}
```



## Sidef


```ruby
class MyClass {
    method foo(arg) { say arg }
}

var arg = 42;

# Call a class method
MyClass.foo(arg);

# Alternatively, using an expression for the method name
MyClass.(:foo)(arg);

# Create an instance
var instance = MyClass();

# Instance method
instance.foo(arg);

# Alternatively, by using an expression for the method name
instance.(:foo)(arg);

# Alternatively, by asking for a method
instance.method(:foo)(arg);
```



## Smalltalk

In Smalltalk, calling an instance method is sending a message to an instance, and calling a class method is sending a message to a class object. Class methods are inherited through inheritance of classes. All messages (whether sent to a normal object or class object) are resolved dynamically at runtime, hence strictly speaking, there are no "static" methods. Often in literature, Smalltalk's class messages are described as synonymous to static function calls. This is wrong, because class methods can be overwritten in subclasses just as any other message.
Classes are first class objects, meaning that references to them can be passed as argument, stored in other objects or returned as the result of a message send. This makes some of the common design patterns which deal with creation of objects trivial or even superfluous.  (see also: ObjectiveC)


```smalltalk
" Class "
MyClass selector: someArgument .
" or equivalently "
foo := MyClass .
foo selector: someArgument.

" Instance "
myInstance selector: someArgument.

" Message with multiple arguments "
myInstance fooWithRed:arg1 green:arg2 blue:arg3 .

" Message with no arguments "
myInstance selector.

" Binary (operator) message"
myInstance + argument .
```



Example for dynamic class determination:

```smalltalk
theCar := (someCondition ifTrue:[ Ford ] ifFalse: [ Jaguar ]) new.
```


Message names (selectors) can be chosen or constructed dynamically at runtime. For example:

```smalltalk
whichMessage := #( #'red' #'green' #'blue') at: computedIndex.
foo perform: whichMessage
```


or:


```smalltalk
theMessage := ('handleFileType' , suffix) asSymbol.
foo perform: theMessage.
```

This is often sometimes used as a dispatch mechanism (also, to implement state machines, for example).

Of course, especially with all that dynamics, a selector for an unimplemented message might be encountered. This raises a MessageNotUnderstood exception (runtime exception).

```smalltalk
[
    foo perform: theMessage
] on: MessageNotUnderstood do:[
   Dialog information: 'sorry'
]
```



## SuperCollider

In SuperCollider, classes are objects. To call a class or object method, a message is passed to the class or the object instance. In the implementation, class method names are prefixed with an asterix, all other methods are instance methods.

```SuperCollider

	SomeClass {

		*someClassMethod {

		}

		someInstanceMethod {

		}

	}

	SomeClass.someClassMethod;

	a = SomeClass.new;
	a.someInstanceMethod;

```



## Swift

In Swift, instance methods can be declared on structs, enums, and classes. "Type methods", which include static methods for structs and enums, and class methods for classes, can be called on the type. Class methods are inherited through inheritance of classes, and are resolved dynamically at runtime like instance methods. (see also: Smalltalk).

```swift
// Class
MyClass.method(someParameter)
// or equivalently:
let foo = MyClass.self
foo.method(someParameter)

// Instance
myInstance.method(someParameter)

// Method with multiple arguments
myInstance.method(red:arg1, green:arg2, blue:arg3)
```



## Tcl


```tcl
package require Tcl 8.6
# "Static" (on class object)
MyClass mthd someParameter

# Instance
$myInstance mthd someParameter
```



## Ursa


```ursa
# create an instance of the built-in file class
decl file f

# call the file.open method
f.open "filename.txt"
```



## VBA

First we have to create a class module named "myObject" :

```vb

Option Explicit

Sub Method_1(Optional myStr As String)
Dim strTemp As String
    If myStr <> "" Then strTemp = myStr
    Debug.Print strTemp
End Sub

Static Sub Method_2(Optional myStr As String)
Dim strTemp As String
    If myStr <> "" Then strTemp = myStr
    Debug.Print strTemp
End Sub
```

In a "standard" Module, the call should be :

```vb
Option Explicit

Sub test()
Dim Obj As New myObject
    Obj.Method_1 "Hello to you"
    Obj.Method_2 "What is your name ?"
    Obj.Method_1
    Obj.Method_2
End Sub
```

```txt
Hello to you
What is your name ?

What is your name ?
```




## XLISP

Class methods and instance methods are defined using <tt>DEFINE-CLASS-METHOD</tt> and <tt>DEFINE-METHOD</tt> respectively. They are called by sending a message to the class or to an instance of it: the message consists of (<i>a</i>) the name of the object that will receive it, which may be a class; (<i>b</i>) the name of the method, as a quoted symbol; and (<i>c</i>) the parameters if any.

```xlisp
(DEFINE-CLASS MY-CLASS)

(DEFINE-CLASS-METHOD (MY-CLASS 'DO-SOMETHING-WITH SOME-PARAMETER)
    (DISPLAY "I am the class -- ")
    (DISPLAY SELF)
    (NEWLINE)
    (DISPLAY "You sent me the parameter ")
    (DISPLAY SOME-PARAMETER)
    (NEWLINE))

(DEFINE-METHOD (MY-CLASS 'DO-SOMETHING-WITH SOME-PARAMETER)
    (DISPLAY "I am an instance of the class --  ")
    (DISPLAY SELF)
    (NEWLINE)
    (DISPLAY "You sent me the parameter ")
    (DISPLAY SOME-PARAMETER)
    (NEWLINE))

(MY-CLASS 'DO-SOMETHING-WITH 'FOO)

(DEFINE MY-INSTANCE (MY-CLASS 'NEW))

(MY-INSTANCE 'DO-SOMETHING-WITH 'BAR)
```

```txt
I am the class -- #<Class:MY-CLASS #x38994c8>
You sent me the parameter FOO
I am an instance of the class --  #<Object:MY-CLASS #x39979d0>
You sent me the parameter BAR
```



## zkl

In zkl, a class can be static (there will only exist one instance of the class). A function is always a member of some class but will only be static if it does not refer to instance data.

```zkl
class C{var v; fcn f{v}}
C.f() // call function f in class C
C.v=5; c2:=C();    // create new instance of C
println(C.f()," ",c2.f()) //-->5 Void
C.f.isStatic //--> False

class [static] D{var v=123; fcn f{v}}
D.f(); D().f(); // both return 123
D.f.isStatic //-->False

class E{var v; fcn f{}} E.f.isStatic //-->True
```


<!-- Same omits as Classes page -->
