+++
title = "Break OO privacy"
description = ""
date = 2019-10-21T10:50:46Z
aliases = []
[extra]
id = 10244
[taxonomies]
categories = []
tags = []
+++

{{task}} [[Category:Object oriented]]
{{omit from|AWK}}
{{omit from|BASIC}}
{{omit from|BBC BASIC}}
{{omit from|C}}
{{omit from|Déjà Vu}}
{{omit from|GUISS}}
{{omit from|Haskell}}
{{omit from|Locomotive Basic}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|Mathematica}}
{{omit from|R}}
{{omit from|Rust}}
{{omit from|SmileBASIC}}
{{omit from|ZX Spectrum Basic}}

Show how to access private or protected members of a class in an object-oriented language from outside an instance of the class, without calling non-private or non-protected members of the class as a proxy.
The intent is to show how a debugger, serializer, or other meta-programming tool might access information that is barred by normal access methods to the object  but can nevertheless be accessed from within the language by some provided escape hatch or reflection mechanism.
The intent is specifically not to demonstrate heroic measures such as peeking and poking raw memory.

Note that cheating on your type system is almost universally regarded
as unidiomatic at best, and poor programming practice at worst.
Nonetheless, if your language intentionally maintains a double-standard for OO privacy, here's where you can show it off.


## ABAP


Similar to C++, ABAP allows the declaration of friends which can be both classes and interfaces. All subclasses of friend classes are automatically friends of the source class. For example if classA (source) has classB as a friend and classC is a subclass of classB then classC is a friend of classA. Similarly all implementing classes of friend interfaces are friends of the source class. Also all interfaces which contain the befriended interface as a component are friends of the source class.


```ABAP
class friendly_class definition deferred.

class my_class definition friends friendly_class .

  public section.
    methods constructor.

  private section.
    data secret type char30.

endclass.

class my_class implementation .

  method constructor.
    secret = 'a password'. " Instantiate secret.
  endmethod.

endclass.

class friendly_class definition create public .

  public section.
    methods return_secret
      returning value(r_secret) type char30.

endclass.

class friendly_class implementation.

  method return_secret.

    data lr_my_class type ref to my_class.

    create object lr_my_class. " Instantiate my_class

    write lr_my_class->secret. " Here's the privacy violation.

  endmethod.

endclass.

```



## Ada


One of the great criticisms of Pascal was "there is no escape". The reason was that sometimes you have to convert the incompatible. For this purpose, Ada has the generic function Unchecked_Conversion, that can be used for such purposes
as breaking OO privacy. We start with the package that holds the "secret":


```Ada
package OO_Privacy is

   type Confidential_Stuff is tagged private;
   subtype Password_Type is String(1 .. 8);

private
   type Confidential_Stuff is tagged record
      Password: Password_Type := "default!"; -- the "secret"
   end record;
end OO_Privacy;
```


Second, the package that provides the "hack" into Confidential_Stuff:


```Ada
with OO_Privacy, Ada.Unchecked_Conversion, Ada.Text_IO;

procedure OO_Break_Privacy is

   type Hacker_Stuff is tagged record
      Password: OO_Privacy.Password_Type := "?unknown";
   end record;

   function Hack is new Ada.Unchecked_Conversion
     (Source => OO_Privacy.Confidential_Stuff,     Target => Hacker_Stuff);

   C: OO_Privacy.Confidential_Stuff; -- which password is hidden inside C?

begin
   Ada.Text_IO.Put_Line("The secret password is """ & Hack(C).Password & """");
end OO_Break_Privacy;
```


The output shows that C holds, surprise, surprise, the default password:


```txt
The secret password is "default!"
```



## C#


```c#
using System;
using System.Reflection;

public class MyClass
{
    private int answer = 42;
}

public class Program
{
    public static void Main()
    {
        var myInstance = new MyClass();
        var fieldInfo = typeof(MyClass).GetField("answer", BindingFlags.NonPublic | BindingFlags.Instance);
        var answer = fieldInfo.GetValue(myInstance);
        Console.WriteLine(answer);
    }
}
```

{{out}}
<lang>42
```



## C++


C++ has the 'friend' keyword to indicate that one class should have access to the private data of another. Here's a simple use case. (Please note that this code is not thread-safe.)


```cpp
#include <iostream>

class CWidget; // Forward-declare that we have a class named CWidget.

class CFactory
{
  friend class CWidget;
private:
  unsigned int m_uiCount;
public:
  CFactory();
  ~CFactory();
  CWidget* GetWidget();
};

class CWidget
{
private:
  CFactory& m_parent;

private:
  CWidget(); // Disallow the default constructor.
  CWidget(const CWidget&); // Disallow the copy constructor
  CWidget& operator=(const CWidget&); // Disallow the assignment operator.
public:
  CWidget(CFactory& parent);
  ~CWidget();
};

// CFactory constructors and destructors. Very simple things.
CFactory::CFactory() : m_uiCount(0) {}
CFactory::~CFactory() {}

// CFactory method which creates CWidgets.
CWidget* CFactory::GetWidget()
{
  // Create a new CWidget, tell it we're its parent.
  return new CWidget(*this);
}

// CWidget constructor
CWidget::CWidget(CFactory& parent) : m_parent(parent)
{
  ++m_parent.m_uiCount;

  std::cout << "Widget spawning. There are now " << m_parent.m_uiCount << " Widgets instanciated." << std::endl;
}

CWidget::~CWidget()
{
  --m_parent.m_uiCount;

  std::cout << "Widget dieing. There are now " << m_parent.m_uiCount << " Widgets instanciated." << std::endl;
}

int main()
{
  CFactory factory;

  CWidget* pWidget1 = factory.GetWidget();
  CWidget* pWidget2 = factory.GetWidget();
  delete pWidget1;

  CWidget* pWidget3 = factory.GetWidget();
  delete pWidget3;
  delete pWidget2;
}
```

{{out}}

```text
Widget spawning. There are now 1 Widgets instanciated.
Widget spawning. There are now 2 Widgets instanciated.
Widget dieing. There are now 1 Widgets instanciated.
Widget spawning. There are now 2 Widgets instanciated.
Widget dieing. There are now 1 Widgets instanciated.
Widget dieing. There are now 0 Widgets instanciated.
```


Without the "friend" mechanism, it's still possible to meaningfully modify any member in another class, as long as you know that member's address in memory, and its type. Here's the same program as above, but using a pointer to m_uicount, rather a reference to the factory:


```cpp
#include <iostream>

class CWidget; // Forward-declare that we have a class named CWidget.

class CFactory
{
private:
  unsigned int m_uiCount;
public:
  CFactory();
  ~CFactory();
  CWidget* GetWidget();
};

class CWidget
{
private:
  unsigned int* m_pCounter;

private:
  CWidget(); // Disallow the default constructor.
  CWidget(const CWidget&); // Disallow the copy constructor
  CWidget& operator=(const CWidget&); // Disallow the assignment operator.
public:
  CWidget(unsigned int* pCounter);
  ~CWidget();
};

// CFactory constructors and destructors. Very simple things.
CFactory::CFactory() : m_uiCount(0) {}
CFactory::~CFactory() {}

// CFactory method which creates CWidgets.
CWidget* CFactory::GetWidget()
{
  // Create a new CWidget, tell it we're its parent.
  return new CWidget(&m_uiCount);
}

// CWidget constructor
CWidget::CWidget(unsigned int* pCounter) : m_pCounter(pCounter)
{
  ++*m_pCounter;

  std::cout << "Widget spawning. There are now " << *m_pCounter<< " Widgets instanciated." << std::endl;
}

CWidget::~CWidget()
{
  --*m_pCounter;

  std::cout << "Widget dieing. There are now " << *m_pCounter<< " Widgets instanciated." << std::endl;
}

int main()
{
  CFactory factory;

  CWidget* pWidget1 = factory.GetWidget();
  CWidget* pWidget2 = factory.GetWidget();
  delete pWidget1;

  CWidget* pWidget3 = factory.GetWidget();
  delete pWidget3;
  delete pWidget2;
}
```



## Clojure

You can use the var-quote macro to get values from private variables.  Here's an example of a variable 'priv' marked private in namespace 'a':

```clojure

(ns a)
(def ^:private priv :secret)

; From REPL, in another namespace 'user':
user=> @a/priv ; fails with: IllegalStateException: var: a/priv is not public
user=> @#'a/priv ; succeeds
:secret

```


Clojure can also access Java private variables with the same strategy that Java uses.  As a convenience, use the [http://clojuredocs.org/clojure_contrib/clojure.contrib.reflect/get-field get-field] function from clojure.contrib.reflect.  Here's an example of grabbing the private field "serialVersionUID" from java.lang.Double:

```clojure

user=> (get-field Double "serialVersionUID" (Double/valueOf 1.0))
-9172774392245257468

```


## Common Lisp


Common Lisp doesn't have concepts like class scopes, name lookup rules, or access. Essentially, the way things are referenced in the source code (the static view of the program) is not involved in the class hierarchy.
For instance, when a class is derived from another one, it is purely an object-oriented inheritance.
There is no parallel symbol-table inheritance going on whereby one namespace becomes a tributary of another (the identifier scope of a derived class inheriting symbols from, or a visibility window into the base class).

The primary concept of naming privacy in Lisp is located in the package system.
An external symbol S in package P can be accessed using P:S. If S is internal then P:S results in error at read time.
This is completely independent of context and orthogonal to the use of symbols to denote class slots (instance variables), methods, functions, class names themselves, global variables, et cetera.
Even if the private symbol appears in a literal piece of data like a quoted list, it is an error to refer to it: <code>(1 2 3 P:S)</code>.
The internal symbol S in package P can be accessed using <code>P::S</code>.
This is easy to do because programmers are assumed to be responsible grownups who can be trusted to know what they are doing.
Also, note that this privacy is a property of the relationship between a symbol and a package.
A symbol can be present in more than one package, such that it can be internal in some of them, and external in others.


```lisp
(defpackage :funky
  ;; only these symbols are public
  (:export :widget :get-wobbliness)
  ;; for convenience, bring common lisp symbols into funky
  (:use :cl))

;; switch reader to funky package: all symbols that are
;; not from the CL package are interned in FUNKY.

(in-package :funky)

(defclass widget ()
  ;; :initarg -> slot "wobbliness" is initialized using :wobbliness keyword
  ;; :initform -> if initarg is missing, slot defaults to 42
  ;; :reader -> a "getter" method called get-wobbliness is generated
  ((wobbliness :initarg :wobbliness :initform 42 :reader get-wobbliness)))

;; simulate being in another source file with its own package:
;; cool package gets external symbols from funky, and cl:
(defpackage :cool
  (:use :funky :cl))

(in-package :cool)

;; we can use the symbol funky:widget without any package prefix:
(defvar *w* (make-instance 'widget :wobbliness 36))

;; ditto with funky:get-wobbliness
(format t "wobbliness: ~a~%" (get-wobbliness *w*))

;; direct access to the slot requires fully qualified private symbol
;; and double colon:
(format t "wobbliness: ~a~%" (slot-value *w* 'funky::wobbliness))

;; if we use unqualified wobbliness, it's a different symbol:
;; it is cool::wobbliness interned in our local package.
;; we do not have funky:wobbliness because it's not exported by funky.
(unless (ignore-errors
          (format t "wobbliness: ~a~%" (slot-value *w* 'wobbliness)))
  (write-line "didn't work"))

;; single colon results in error at read time! The expression is not
;; even read and evaluated. The symbol is internal and so cannot be used.
(format t "wobbliness: ~a~%" (slot-value *w* 'funky:wobbliness))

```


{{Out}} using CLISP:

```txt
wobbliness: 36
wobbliness: 36
didn't work
*** - READ from #<INPUT BUFFERED FILE-STREAM CHARACTER #P"funky.lisp" @44>:
      #<PACKAGE FUNKY> has no external symbol with name "WOBBLINESS"
```



## E


In its goal of supporting programs including mutually suspicious robust components and potentially executing untrusted code, E specifically does not provide any means within the language to break the encapsulation of an object. The official answer to how you access private state is: evaluate the code with a special evaluator which permits such access to the objects the code creates, or, entirely equivalently, transform the code so that each object definition has hooks for private access.

{{improve|E|Show an example of such an evaluator once it is available.}}

=={{header|F_Sharp|F#}}==
{{trans|C#}}

```fsharp
open System
open System.Reflection

type MyClass() =
    let answer = 42
    member this.GetAnswer
        with get() = answer


[<EntryPoint>]
let main argv =
    let myInstance = MyClass()
    let fieldInfo = myInstance.GetType().GetField("answer", BindingFlags.NonPublic ||| BindingFlags.Instance)
    let answer = fieldInfo.GetValue(myInstance)
    printfn "%s = %A" (answer.GetType().ToString()) answer
    0
```

{{out}}

```txt
System.Int32 = 42
```



## Factor

From the [http://docs.factorcode.org/content/article-word-search-private.html documentation for private words]: ''"Privacy is not enforced by the system; private words can be called from other vocabularies, and from the listener. However, this should be avoided where possible."''

This example uses the private word ''sequence/tester'' from the vocabulary ''sets.private''. It tries to count the elements in an intersection of two sets.


```factor
( scratchpad ) USING: sets sets.private ;
( scratchpad ) { 1 2 3 } { 1 2 4 } sequence/tester count .
2
```


There is better way to do the same, without any private words.


```factor
( scratchpad ) USE: sets
( scratchpad ) { 1 2 3 } { 1 2 4 } intersect length .
2
```




## Forth

{{works with|Forth}}
Works with any ANS Forth

Needs the FMS-SI (single inheritance) library code located here:
http://soton.mpeforth.com/flag/fms/index.html

```forth
include FMS-SI.f

99 value x  \ create a global variable named x

:class foo
 ivar x  \ this x is private to the class foo
 :m init: 10 x ! ;m  \ constructor
 :m print x ? ;m
;class

foo f1    \ instantiate a foo object
f1 print  \ 10 access the private x with the print message

x .  \ 99  x is a globally scoped name

50 .. f1.x !  \ use the dot parser to access the private x without a message
f1 print  \ 50

```



## FreeBASIC

FreeBASIC generally does a good job of maintaining OO privacy as it doesn't support reflection and even its OffsetOf keyword cannot obtain the offset within a user defined type of a private or protected field. You therefore have to guess the offset of a non-public field in order to be able to access it using a raw pointer though this is not generally a difficult task.

However, as usual, macros come to the rescue and one can easily access non-public members by the simple expedient (or, if you prefer, 'dirty hack') of redefining the Private and Protected keywords to mean Public:

```freebasic
'FB 1.05.0 Win64

#Undef Private
#Undef Protected
#Define Private Public
#Define Protected Public

Type MyType
  Public :
    x As Integer = 1
  Protected :
    y As Integer = 2
  Private :
    z As Integer = 3
End Type

Dim mt As MyType
Print mt.x, mt.y, mt.z
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

 1             2             3

```



## Go

Go has a <code>reflect</code> and <code>unsafe</code> package that together can do this.
Of course, as mentioned in the task description, this is ''a bad idea''.

A relevant Go Blog article is
[http://blog.golang.org/laws-of-reflection The Laws of Reflection].

```go
package main

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"reflect"
	"unsafe"
)

type foobar struct {
	Exported   int // In Go identifiers that are capitalized are exported,
	unexported int // while lowercase identifiers are not.
}

func main() {
	obj := foobar{12, 42}
	fmt.Println("obj:", obj)

	examineAndModify(&obj)
	fmt.Println("obj:", obj)

	anotherExample()
}

// For simplicity this skips several checks. It assumes the thing in the
// interface is a pointer without checking (v.Kind()==reflect.Ptr),
// it then assumes it is a structure type with two int fields
// (v.Kind()==reflect.Struct, f.Type()==reflect.TypeOf(int(0))).
func examineAndModify(any interface{}) {
	v := reflect.ValueOf(any) // get a reflect.Value
	v = v.Elem()              // dereference the pointer
	fmt.Println(" v:", v, "=", v.Interface())
	t := v.Type()
	// Loop through the struct fields
	fmt.Printf("    %3s %-10s %-4s %s\n", "Idx", "Name", "Type", "CanSet")
	for i := 0; i < v.NumField(); i++ {
		f := v.Field(i) // reflect.Value of the field
		fmt.Printf("    %2d: %-10s %-4s %t\n", i,
			t.Field(i).Name, f.Type(), f.CanSet())
	}

	// "Exported", field 0, has CanSet==true so we can do:
	v.Field(0).SetInt(16)
	// "unexported", field 1, has CanSet==false so the following
	// would fail at run-time with:
	//   panic: reflect: reflect.Value.SetInt using value obtained using unexported field
	//v.Field(1).SetInt(43)

	// However, we can bypass this restriction with the unsafe
	// package once we know what type it is (so we can use the
	// correct pointer type, here *int):
	vp := v.Field(1).Addr()            // Take the fields's address
	up := unsafe.Pointer(vp.Pointer()) // … get an int value of the address and convert it "unsafely"
	p := (*int)(up)                    // … and end up with what we want/need
	fmt.Printf("  vp has type %-14T = %v\n", vp, vp)
	fmt.Printf("  up has type %-14T = %#0x\n", up, up)
	fmt.Printf("   p has type %-14T = %v pointing at %v\n", p, p, *p)
	*p = 43 // effectively obj.unexported = 43
	// or an incr all on one ulgy line:
	*(*int)(unsafe.Pointer(v.Field(1).Addr().Pointer()))++

	// Note that as-per the package "unsafe" documentation,
	// the return value from vp.Pointer *must* be converted to
	// unsafe.Pointer in the same expression; the result is fragile.
	//
	// I.e. it is invalid to do:
	//	thisIsFragile := vp.Pointer()
	//	up := unsafe.Pointer(thisIsFragile)
}

// This time we'll use an external package to demonstrate that it's not
// restricted to things defined locally. We'll mess with bufio.Reader's
// interal workings by happening to know they have a non-exported
// "err error" field. Of course future versions of Go may not have this
// field or use it in the same way :).
func anotherExample() {
	r := bufio.NewReader(os.Stdin)

	// Do the dirty stuff in one ugly and unsafe statement:
	errp := (*error)(unsafe.Pointer(
		reflect.ValueOf(r).Elem().FieldByName("err").Addr().Pointer()))
	*errp = errors.New("unsafely injected error value into bufio inner workings")

	_, err := r.ReadByte()
	fmt.Println("bufio.ReadByte returned error:", err)
}
```

{{out}}

```txt

obj: {12 42}
 v: <main.foobar Value> = {12 42}
    Idx Name       Type CanSet
     0: Exported   int  true
     1: unexported int  false
  vp has type reflect.Value  = <*int Value>
  up has type unsafe.Pointer = 0xc208000208
   p has type *int           = 0xc208000208 pointing at 42
obj: {16 44}
bufio.ReadByte returned error: unsafely injected error value into bufio inner workings

```


==Icon and {{header|Unicon}}==
{{omit from|Icon}}
Unicon implements object environments with records and supporting procedures for creation, initialization, and methods.  The variables in the class environment can be accessed like any other record field.  Additionally, with the ''fieldnames'' procedure you can obtain the names of the class variables.

In addition to debuggers and diagnostic tools, these techniques could be used in 'monkey patching' extensions (see [[Add_a_variable_to_a_class_instance_at_runtime]]).

Note: Unicon can be translated via a command line switch into icon which allows for classes to be shared with Icon code (assuming no other incompatibilities exist).

```unicon
link printf

procedure main()
   (x := foo(1,2,3)).print()                 # create and show a foo
   printf("Fieldnames of foo x : ")          # show fieldnames
   every printf(" %i",fieldnames(x))         # __s (self), __m (methods), vars
   printf("\n")
   printf("var 1 of foo x = %i\n", x.var1)   # read var1 from outside x
   x.var1 := -1                              # change var1 from outside x
   x.print()                                 # show we changed it
end

class foo(var1,var2,var3)                    # class with no set/read methods
   method print()
      printf("foo var1=%i, var2=%i, var3=%i\n",var1,var2,var3)
   end
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting]

Output:
```txt
foo var1=1, var2=2, var3=3
Fieldnames of foo x :  "__s" "__m" "var1" "var2" "var3"
var 1 of foo x = 1
foo var1=-1, var2=2, var3=3
```



## J


Current implementations of J do not enforce OO privacy, instead favoring design techniques (such as functional programming) which eliminate private information in persistent contexts.

It's possible to use OS features (such as other processes, and servers) to hide information.  But that, by definition, is outside the scope of the language.

J does support a "[http://www.jsoftware.com/help/dictionary/dx003.htm Lock Script]" mechanism - to transform a J script so that it's unreadable. However, anyone with access to a machine running the code and ordinary developer tools or who understands the "locking" technique could unlock it.


## Java

Private fields (and in general all members) of a Java class can be accessed via reflection, but must pass a security check in order to do so. There are two such security checks, one for discovering the field at all, and another for granting access to it in order to be able to read and write it. (This in turn means that trusted applications can do this — it is in fact a mechanism used by important frameworks like Spring — but untrusted applets cannot.)

```java
import java.lang.reflect.*;

class Example {
    private String _name;
    public Example(String name) { _name = name; }
    public String toString() { return "Hello, I am " + _name; }
}

public class BreakPrivacy {
    public static final void main(String[] args) throws Exception {
        Example foo = new Example("Eric");

        for (Field f : Example.class.getDeclaredFields()) {
	    if (f.getName().equals("_name")) {
                // make it accessible
                f.setAccessible(true);

                // get private field
                System.out.println(f.get(foo));

                // set private field
                f.set(foo, "Edith");
		System.out.println(foo);

                break;
            }
        }
    }
}
```

{{out}}

```txt

Eric
Hello, I am Edith

```

All classes are vulnerable to this, including <code>String</code> (and therefore, string literals). How long is the word "cat"?

(Note: somewhere between Java 8 and Java 11 this stopped working because the <code>value</code> field of <code>String</code> is <code>final</code>. The reflective access is still possible, but changing a final field isn't.)

```Java
import java.lang.reflect.*;
public class BreakString{
	public static void main(String... args) throws Exception{
		Field f = String.class.getDeclaredField("value");
		f.setAccessible(true);
		f.set("cat",new char[]{'t','i','g','e','r'});
		System.out.println("cat");
		System.out.println("cat".length());
	}
}

```

{{out}}

```txt

tiger
5

```



## Julia


Julia's object model is one of structs which contain data and methods which are just functions using those structs, with multiple dispatch, rather than object methods, used to distinguish similarly named calls for different object types. Julia does not therefore enforce any private fields in its structures, since, except for constructors, it does not distinguish object methods from other functions. If private fields or methods are actually needed they can be kept from view by placing them inside a module which cannot be directly accessed by user code, such as in a module within a module.


## Kotlin

For tasks such as this, reflection is your friend:

```scala
import kotlin.reflect.full.declaredMemberProperties
import kotlin.reflect.jvm.isAccessible

class ToBeBroken {
    @Suppress("unused")
    private val secret: Int = 42
}

fun main(args: Array<String>) {
    val tbb = ToBeBroken()
    val props = ToBeBroken::class.declaredMemberProperties
    for (prop in props) {
        prop.isAccessible = true  // make private properties accessible
        println("${prop.name} -> ${prop.get(tbb)}")
    }
}
```


{{out}}

```txt

secret -> 42

```



## Logtalk

Logtalk provides a ''context switching call'' control construct that allows a call to be executed as from within an object.  It's mainly used for debugging and for writing unit tests. This control construct can be disabled  on a global or per object basis to prevent it of being used to break encapsulation.

In the following example, a prototype is used for simplicity.

```logtalk
:- object(foo).

    % be sure that context switching calls are allowed
    :- set_logtalk_flag(context_switching_calls, allow).

    % declare and define a private method
    :- private(bar/1).
    bar(1).
    bar(2).
    bar(3).

:- end_object.
```

After compiling and loading the above object, we can use the following query to access the private method:

```logtalk
| ?- foo<<bar(X).
X = 1 ;
X = 2 ;
X = 3
true
```



## M2000 Interpreter

We want to read two private variables, and change values without using a public method (a module or a function), and without attach a temporary method (we can do that in M2000). There is a variant in READ statemend to set references from group members (for variables and arrays, and objects) to names with a reference for each. So using these names (here in the exaample K, M) we can read and write private variables.

```M2000 Interpreter

Module CheckIt {
      Group Alfa {
            Private:
            X=10, Y=20
            Public:
            Module SetXY (.X, .Y) {}
            Module Print {
                  Print .X, .Y
            }
      }
      Alfa.Print  ' 10 20
      \\ we have to KnΟw position in group
      \\ so we make references from two first
      Read From Alfa, K, M
      Print K=10, M=20
      K+=10
      M+=1000
      Alfa.Print   ' 20   1020
}
CheckIt

```



## Nim

File oo.nim:

```nim
type Foo* = object
  a: string
  b: string
  c: int

proc createFoo*(a, b, c): Foo =
  Foo(a: a, b: b, c: c)
```

By not adding a <code>*</code> to <code>Foo</code>'s members we don't export them. When we import this module we can't use them directly:

```nim
var x = createFoo("this a", "this b", 12)

echo x.a # compile time error
```

The easiest way to get a debug view of any data:

```nim
echo repr(x)
```

Output:

```txt
[a = 0x7f6bb87a7050"this a",
b = 0x7f6bb87a7078"this b",
c = 12]
```

More fine-grained:

```nim
import typeinfo

for key, val in fields(toAny(x)):
  echo "Key ", key
  case val.kind
  of akString:
    echo "  is a string with value: ", val.getString
  of akInt..akInt64, akUint..akUint64:
    echo "  is an integer with value: ", val.getBiggestInt
  else:
    echo "  is an unknown with value: ", val.repr
```

Output:

```txt
Key a
  is a string with value: this a
Key b
  is a string with value: this b
Key c
  is an integer with value: 12
```


=={{header|Objective-C}}==
In older versions of the compiler, you can simply access a private field from outside of the class. The compiler will give a warning, but you can ignore it and it will still compile. However, in current compiler versions it is now a hard compile error.

===Key-Value Coding===
One solution is to use Key-Value Coding. It treats properties and instance variables as "keys" that you can get and set using key-value coding methods.


```objc>#import <Foundation/Foundation.h


@interface Example : NSObject {
@private
  NSString *_name;
}
- (instancetype)initWithName:(NSString *)name;
@end

@implementation Example
- (NSString *)description {
  return [NSString stringWithFormat:@"Hello, I am %@", _name];
}
- (instancetype)initWithName:(NSString *)name {
  if ((self = [super init])) {
    _name = [name copy];
  }
  return self;
}
@end

int main (int argc, const char * argv[]) {
  @autoreleasepool{

    Example *foo = [[Example alloc] initWithName:@"Eric"];

    // get private field
    NSLog(@"%@", [foo valueForKey:@"name"]);

    // set private field
    [foo setValue:@"Edith" forKey:@"name"];
    NSLog(@"%@", foo);

  }
  return 0;
}
```

{{out}}

```txt

Eric
Hello, I am Edith

```



### Category

Another solution is to use a category to add methods to the class (you can have categories in your code modify any class, even classes compiled by someone else, including system classes). Since the new method is in the class, it can use the class's private instance variables with no problem.


```objc>#import <Foundation/Foundation.h


@interface Example : NSObject {
@private
  NSString *_name;
}
- (instancetype)initWithName:(NSString *)name;
@end

@implementation Example
- (NSString *)description {
  return [NSString stringWithFormat:@"Hello, I am %@", _name];
}
- (instancetype)initWithName:(NSString *)name {
  if ((self = [super init])) {
    _name = [name copy];
  }
  return self;
}
@end

@interface Example (HackName)
- (NSString *)getName;
- (void)setNameTo:(NSString *)newName;
@end

@implementation Example (HackName)
- (NSString *)getName {
  return _name;
}
- (void)setNameTo:(NSString *)newName {
  _name = [newName copy];
}
@end

int main (int argc, const char * argv[]) {
  @autoreleasepool{

    Example *foo = [[Example alloc] initWithName:@"Eric"];

    // get private field
    NSLog(@"%@", [foo getName]);

    // set private field
    [foo setNameTo:@"Edith"];
    NSLog(@"%@", foo);

  }
  return 0;
}
```

{{out}}

```txt

Eric
Hello, I am Edith

```



### Reflection

Finally, you can access the instance variable directly using runtime functions.


```objc>#import <Foundation/Foundation.h

#import <objc/runtime.h>

@interface Example : NSObject {
@private
  NSString *_name;
}
- (instancetype)initWithName:(NSString *)name;
@end

@implementation Example
- (NSString *)description {
  return [NSString stringWithFormat:@"Hello, I am %@", _name];
}
- (instancetype)initWithName:(NSString *)name {
  if ((self = [super init])) {
    _name = [name copy];
  }
  return self;
}
@end

int main (int argc, const char * argv[]) {
  @autoreleasepool{

    Example *foo = [[Example alloc] initWithName:@"Eric"];

    // get private field
    Ivar nameField = class_getInstanceVariable([foo class], "_name");
    NSLog(@"%@", object_getIvar(foo, nameField));

    // set private field
    object_setIvar(foo, nameField, @"Edith");
    NSLog(@"%@", foo);

  }
  return 0;
}
```

{{out}}

```txt

Eric
Hello, I am Edith

```



## OCaml


OCaml includes a function called Obj.magic, of type 'a -> 'b.

That type alone should tell you that this function is crystallized pure evil. The following is not quite as heroic as peeking at random addresses of memory, but to repeat it does require an understanding of the physical layout of OCaml data.

In the following, point's attributes are completely private to the object. They can be revealed with print but can't be directly modified or checked at all. Obj.magic is then used to commit this lie: actually, what was a point object, can be viewed as a four-element tuple of ints. The first two values are meaningless except to OCaml internals and are discarded; the second two values are point's hidden attributes. Then, an even more sinister lie is told that allows us to mutate the point's hidden attributes. Lies of this nature can be used to mutate normally immutable data, which can directly lead to very hard to understand bugs.

The reader is advised to stop reading here.


```ocaml
class point x y =
  object
    val mutable x = x
    val mutable y = y
    method print = Printf.printf "(%d, %d)\n" x y
    method dance =
      x <- x + Random.int 3 - 1;
      y <- y + Random.int 3 - 1
  end

type evil_point {
  blah : int;
  blah2 : int;
  mutable x : int;
  mutable y : int;
}

let evil_reset p =
  let ep = Obj.magic p in
  ep.x <- 0;
  ep.y <- 0

let () =
  let p = new point 0 0 in
  p#print;
  p#dance;
  p#print;
  p#dance;
  p#print;
  let (_, _, x, y) : int * int * int * int = Obj.magic p in
  Printf.printf "Broken coord: (%d, %d)\n" x y;
  evil_reset p
  p#print
```


{{out}}

```txt
(0, 0)
(-1, 0)
(-1, -1)
Broken coord: (-1, -1)
(0, 0)
```



## Oforth


In Oforth, all attributes are private.

There is no other way to access attributes values from outside but to call methods on the object.


## Perl

Perl's object model does not enforce privacy. An object is just a blessed reference, and a blessed reference can be dereferenced just like an ordinary reference.

```perl
package Foo;
sub new {
	my $class = shift;
	my $self = { _bar => 'I am ostensibly private' };
	return bless $self, $class;
}

sub get_bar {
	my $self = shift;
	return $self->{_bar};
}

package main;
my $foo = Foo->new();
print "$_\n" for $foo->get_bar(), $foo->{_bar};
```

{{out}}

```txt
I am ostensibly private
I am ostensibly private
```



## Perl 6

{{works with|Rakudo|2015.12}}
We may call into the MOP (Meta-Object Protocol) via the <tt>.^</tt> operator, and the MOP knows all about the object, including any supposedly private bits.  We ask for its attributes, find the correct one, and get its value.

```perl6
class Foo {
    has $!shyguy = 42;
}
my Foo $foo .= new;

say $foo.^attributes.first('$!shyguy').get_value($foo);
```

{{out}}

```txt
42
```



## PicoLisp

PicoLisp uses [http://software-lab.de/doc/ref.html#transient "transient symbols"] for variables, functions, methods etc. inaccessible from other parts of the program. Lexically, a transient symbol is enclosed by double quotes.
The only way to access a transient symbol outside its namespace is to search for its name in other (public) structures. This is done by the '[http://software-lab.de/doc/refL.html#loc loc]' function.

```PicoLisp
(class +Example)
# "_name"

(dm T (Name)
   (=: "_name" Name) )

(dm string> ()
   (pack "Hello, I am " (: "_name")) )

(====)  # Close transient scope

(setq Foo (new '(+Example) "Eric"))
```

Test:

```PicoLisp
: (string> Foo)                        # Access via method call
-> "Hello, I am Eric"

: (get Foo '"_name")                   # Direct access doesn't work
-> NIL

: (get Foo (loc "_name" +Example))     # Locating the transient symbol works
-> "Eric"

: (put Foo (loc "_name" +Example) "Edith")
-> "Edith"

: (string> Foo)                        # Ditto
-> "Hello, I am Edith"

: (get Foo '"_name")
-> NIL

: (get Foo (loc "_name" +Example))
-> "Edith"
```



## PHP

While normally accessing private variables causes fatal errors, it's possible to catch output of some debugging functions and use it. Known functions which can get private variables include: <code>var_dump()</code>, <code>print_r()</code>, <code>var_export()</code> and <code>serialize()</code>. The easiest to use is <code>var_export()</code> because it's both valid PHP code and doesn't recognize private and public variables.

{{works with|PHP|5.1}}

```php
<?php
class SimpleClass {
    private $answer = "hello\"world\nforever :)";
}

$class = new SimpleClass;
ob_start();

// var_export() expects class to contain __set_state() method which would import
// data from array. But let's ignore this and remove from result the method which
// sets state and just leave data which can be used everywhere...
var_export($class);
$class_content = ob_get_clean();

$class_content = preg_replace('"^SimpleClass::__set_state\("', 'return ', $class_content);
$class_content = preg_replace('"\)$"', ';', $class_content);

$new_class = eval($class_content);
echo $new_class['answer'];
```


Another way commonly used to access private and protected variables in PHP is to cast the object to an array. It's probably unintentional though looking on how casted array contains null bytes (probably "private" mark). This works unless a magic method for the cast operation is implemented:

{{works with|PHP|4.x}}
{{works with|PHP|5.x}}

```php
<?php
class SimpleClass {
    private $answer = 42;
}

$class = new SimpleClass;
$classvars = (array)$class;
echo $classvars["\0SimpleClass\0answer"];
```


{{works with|PHP|5.3}}
Since php 5.3, one can easily read and write any protected and private member in a object via reflection.

```php
<?php
class fragile {
    private $foo = 'bar';
}
$fragile = new fragile;
$ro = new ReflectionObject($fragile);
$rp = $ro->getProperty('foo');
$rp->setAccessible(true);
var_dump($rp->getValue($fragile));
$rp->setValue($fragile, 'haxxorz!');
var_dump($rp->getValue($fragile));
var_dump($fragile);
```

{{out}}

```txt

string(3) "bar"
string(8) "haxxorz!"
object(fragile)#1 (1) {
  ["foo":"fragile":private]=>
  string(8) "haxxorz!"
}

```



## Python

Python isn't heavily into private class names. Although private class names can be defined by using a double underscore at the start of the name, such names are accessible as they are mangled into the original name preceded by the name of its class as shown in this example:

```python>>>
 class MyClassName:
	__private = 123
	non_private = __private * 2


>>> mine = MyClassName()
>>> mine.non_private
246
>>> mine.__private
Traceback (most recent call last):
  File "<pyshell#23>", line 1, in <module>
    mine.__private
AttributeError: 'MyClassName' object has no attribute '__private'
>>> mine._MyClassName__private
123
>>>
```



## Ruby

Ruby lets you redefine great parts of the object model at runtime and provides several methods to do so conveniently. For a list of all available methods look up the documentation of <code>Object</code> and  <code>Module</code> or call informative methods at runtime (<code>puts Object.methods</code>).

```ruby

class Example
  def initialize
     @private_data = "nothing" # instance variables are always private
  end
  private
  def hidden_method
     "secret"
  end
end
example = Example.new
p example.private_methods(false) # => [:hidden_method]
#p example.hidden_method # => NoMethodError: private method `name' called for #<Example:0x101308408>
p example.send(:hidden_method) # => "secret"
p example.instance_variables # => [:@private_data]
p example.instance_variable_get :@private_data # => "nothing"
p example.instance_variable_set :@private_data, 42 # => 42
p example.instance_variable_get :@private_data # => 42

```



## Scala

{{libheader|Scala}}
```scala
class Example(private var name: String) {
  override def toString = s"Hello, I am $name"
}

object BreakPrivacy extends App {
  val field = classOf[Example].getDeclaredField("name")
  field.setAccessible(true)

  val foo = new Example("Erik")
  println(field.get(foo))
  field.set(foo, "Edith")
  println(foo)
}
```



## Sidef

Sidef's object model does not enforce privacy, but it allows storing private attributes inside the container of an object, which is an hash:

```ruby
class Example {
    has public = "foo"
    method init {
        self{:private} = "secret"
    }
}

var obj = Example();

# Access public attributes
say obj.public;                 #=> "foo"
say obj{:public};               #=> "foo"

# Access private attributes
say obj{:private};              #=> "secret"
```



## Swift

Swift reflection provides a Collection of label-value pairs for struct properties

```Swift
struct Example {
    var notSoSecret = "Hello!"
    private var secret = 42
}

let e = Example()
let mirror = Mirror(reflecting: e)

if let secret = mirror.children.filter({ $0.label == "secret" }).first?.value {
    print("Value of the secret is \(secret)")
}

```

{{out}}

```txt
Value of the secret is 42
```



## Tcl

Tcl's object properties are just variables in the a per-instance namespace; all that's required to get hold of them is to discover the name of the namespace concerned:

```tcl
package require Tcl 8.6

oo::class create Example {
    variable name
    constructor n {set name $n}
    method print {} {puts "Hello, I am $name"}
}
set e [Example new "Eric"]
$e print
set [info object namespace $e]::name "Edith"
$e print
```
{{out}}
 Hello, I am Eric
 Hello, I am Edith


## Visual Basic .NET

{{trans|C#}}
{{trans|F#}}

Like the other .NET languages, VB can use Reflection ([https://docs.microsoft.com/en-us/dotnet/framework/reflection-and-codedom/reflection Microsoft docs]).


```vbnet
Imports System.Reflection

' MyClass is a VB keyword.
Public Class MyClazz
    Private answer As Integer = 42
End Class

Public Class Program
    Public Shared Sub Main()
        Dim myInstance = New MyClazz()
        Dim fieldInfo = GetType(MyClazz).GetField("answer", BindingFlags.NonPublic Or BindingFlags.Instance)
        Dim answer = fieldInfo.GetValue(myInstance)
        Console.WriteLine(answer)
    End Sub
End Class
```


{{out}}

```txt
42
```



## zkl

In zkl, privacy is more convention than enforced (unlike const or protected).

```zkl
class C{var [private] v; fcn [private] f{123} class [private] D {}}
C.v; C.f; C.D;  // all generate NotFoundError exceptions
However:
C.fcns      //-->L(Fcn(nullFcn),Fcn(f))
C.fcns[1]() //-->123
C.classes   //-->L(Class(D))
C.vars      //-->L(L("",Void)) (name,value) pairs
```

In the case of private vars, the name isn't saved.

{{omit from|Standard ML|not OO}}
