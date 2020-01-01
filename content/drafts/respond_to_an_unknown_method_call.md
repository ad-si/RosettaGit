+++
title = "Respond to an unknown method call"
description = ""
date = 2019-03-30T04:49:03Z
aliases = []
[extra]
id = 4317
[taxonomies]
categories = []
tags = []
+++

{{task|Object oriented}}

;Task:
Demonstrate how to make the object respond (sensibly/usefully) to an invocation of a method on it that it does not support through its class definitions.

Note that this is not the same as just invoking a defined method whose name is given dynamically; the method named at the point of invocation must not be defined.

This task is intended only for object systems that use a dynamic dispatch mechanism without static checking.


;Related task:
*   [[Send an unknown method call]].





## AutoHotkey

[http://ahkscript.org/boards/viewtopic.php?f=17&t=1363&start=140#p14454 Example by LinearSpoon]

```AutoHotkey
class example
{
  foo()
  {
    Msgbox Called example.foo()
  }

  __Call(method, params*)
  {
    funcRef := Func(funcName := this.__class "." method)
    if !IsObject(funcRef)
    {
      str := "Called undefined method " funcName "() with these parameters:"
      for k,v in params
        str .= "`n" v
      Msgbox %str%
    }
    else
    {
      return funcRef.(this, params*)
    }
  }
}

ex := new example
ex.foo()
ex.bar(1,2)
```



## Brat


```brat
example = object.new

example.no_method = { meth_name, *args |
  p "#{meth_name} was called with these arguments: #{args}"
}

example.this_does_not_exist "at all"  #Prints "this_does_not_exist was called with these arguments: [at all]"
```


## C#
{{works with|C sharp|4.0}}


```c#
using System;
using System.Dynamic;

class Example : DynamicObject
{
    public override bool TryInvokeMember(InvokeMemberBinder binder, object[] args, out object result)
    {
        result = null;

        Console.WriteLine("This is {0}.", binder.Name);
        return true;
    }
}

class Program
{
    static void Main(string[] args)
    {
        dynamic ex = new Example();

        ex.Foo();
        ex.Bar();
    }
}
```



## C++


Program terminates with diagnostic message, typically about a call to an "unimplemented pure virtual function". But in that situation, the function is actually known. The concept of a function not being known at all does not exist in C++, due to static name lookup checking. That is to say, the call a->b() causes the compiler to verify that the symbol b exists in the namespace associated with expression a, and refers to a function.

To avoid the pure virtual call, we can put some default function implementation into the abstract class which handles the situation (e.g. throws a custom exception or whatnot).


```cpp
class animal {
public:
  virtual void bark() // concrete virtual, not pure
  {
    throw "implement me: do not know how to bark";
  }
};

class elephant : public animal // does not implement bark()
{
};

int main()
{
  elephant e;
  e.bark();  // throws exception
}

```


=={{header|Caché ObjectScript}}==

Caché classes can include support for what is called dynamic dispatch. If dynamic dispatch is in use and a program references a property or method that is not part of the class definition, then a method (called a dispatch method) is called that attempts to resolve the undefined method or property. For example, dynamic dispatch can return a value for a property that is not defined or it can invoke a method for a method that is not implemented. The dispatch destination is dynamic in that it does not appear in the class descriptor and is not resolved until runtime.


```cos
Class DynamicDispatch.Example Extends %RegisteredObject
{

Method Foo()
{
	Write "This is foo", !
}

Method Bar()
{
	Write "This is bar", !
}

Method %DispatchMethod(Method As %String, Args...)
{
	Write "Tried to handle unknown method '"_Method_"'"
	For i=1:1:$Get(Args) {
		Write ", " If i=1 Write "with arguments: "
		Write "'"_Args(i)_"'"
	}
	Write !
}

ClassMethod Test()
{
	Set obj=##class(DynamicDispatch.Example).%New()
	Do obj.Foo()
	Do obj.Bar()
	Do obj.Grill()
	Do obj.Ding("Dong", 11)
}

}
```

{{out|Examples}}

```txt

USER>Do ##class(DynamicDispatch.Example).Test()
This is foo
This is bar
Tried to handle unknown method 'Grill'
Tried to handle unknown method 'Ding', with arguments: 'Dong', '11'

```



## Common Lisp


In Common Lisp, if a generic function is invoked on arguments for which there is no applicable specialized method, the method <code>no-applicable-method</code> is called with the generic function and the arguments.


```lisp
(defgeneric do-something (thing)
  (:documentation "Do something to thing."))

(defmethod no-applicable-method ((method (eql #'do-something)) &rest args)
  (format nil "No method for ~w on ~w." method args))

(defmethod do-something ((thing (eql 3)))
  (format nil "Do something to ~w." thing))
```


Evaluating
```lisp
(list (do-something 3) (do-something 4))
```
 produces


```lisp
("Do something to 3."
 "No method for #<STANDARD-GENERIC-FUNCTION DO-SOMETHING 214FC042> on (4).")
```



## D

Similar to the Python entry, but D performs this statically.

```d
import std.stdio;

struct Catcher {
    void foo() { writeln("This is foo"); }

    void bar() { writeln("This is bar"); }

    void opDispatch(string name, ArgsTypes...)(ArgsTypes args) {
        writef("Tried to handle unknown method '%s'", name);
        if (ArgsTypes.length) {
            write(", with arguments: ");
            foreach (arg; args)
                write(arg, " ");
        }
        writeln();
    }
}

void main() {
    Catcher ca;
    ca.foo();
    ca.bar();
    ca.grill();
    ca.ding("dong", 11);
}
```

{{out}}

```txt
This is foo
This is bar
Tried to handle unknown method 'grill'
Tried to handle unknown method 'ding', with arguments: dong 11
```

=={{header|Déjà Vu}}==
The function <code>set-default</code> is useful here:

```dejavu
}
labda:
	print "One!"
:one

labda:
	print "Two!"
:two
local :obj {

labda:
	print "Nope, doesn't exist."
set-default obj

obj!one
obj!two
obj!three

```

{{out}}

```txt
One!
Two!
Nope, doesn't exist.
```



## E


In E, a message consists of a ''verb'' (arbitrary string) and ''arguments'' (sequence of arbitrary objects). It is conceptually entirely up to any given object how it dispatches incoming messages.

Practically, the object definition syntax provides a ''matcher'' clause to handle unrecognized messages. This example has the same behavior as the Python example.


```e
def example {
    to foo() { println("this is foo") }
    to bar() { println("this is bar") }
    match [verb, args] {
        println(`got unrecognized message $verb`)
        if (args.size() > 0) {
            println(`it had arguments: $args`)
        }
    }
}
```


## Elena

Using generic handler (ELENA 4.x):

```elena
import extensions;

class Example
{
    generic()
    {
        // __received is an built-in variable containing the incoming message name
        console.printLine(__received," was invoked")
    }

    generic(x)
    {
        console.printLine(__received,"(",x,") was invoked")
    }

    generic(x,y)
    {
        console.printLine(__received,"(",x,",",y,") was invoked")
    }
}

public program()
{
    var o := new Example();

    o.foo();
    o.bar(1);
    o.someMethod(1,2)
}
```

{{out}}

```txt

foo was invoked
bar(1) was invoked
someMethod(1,2) was invoked

```



## Fancy


```fancy

class CatchThemAll {
  def foo {
    "foo received" println
  }

  def bar {
    "bar received" println
  }

  def unknown_message: msg with_params: params {
    "message: " ++ msg print
    "arguments: " ++ (params join: ", ") println
  }
}

a = CatchThemAll new
a foo
a bar
a we_can_do_it
a they_can_too: "eat" and: "walk"

```



## Fantom


In Fantom, you can call methods statically or dynamically.  Static calls to methods will be checked at compile time.  Dynamic method calls (indicated by an <code>instance->method</code> syntax) are run through a "[http://fantom.org/doc/sys/Obj.html#trap trap]" method at run time.  This method looks up the given method name, and throws an exception if the method/field is not known.  This exception can be caught, and processed specially:


```fantom

class A
{
  public Void doit (Int n)
  {
    echo ("known function called on $n")
  }

  // override the 'trap' method, which catches dynamic invocations of methods
  override Obj? trap(Str name, Obj?[]? args := null)
  {
    try
    {
      return super.trap(name, args)
    }
    catch (UnknownSlotErr err)
    {
      echo ("In trap, you called: " + name + " with args " + args.join(","))
      return null
    }
  }
}

class Main
{
  public static Void main ()
  {
    a := A()
    // note the dynamic dispatch
    a->doit (1)
    a->methodName (1, 2, 3)
  }
}

```


Output:

```txt

$ fan unknown-method.fan
known function called on 1
In trap, you called: methodName with args 1,2,3

```



## Forth

{{works with|Forth}}
Works with any ANS Forth

Needs the FMS-SI (single inheritance) library code located here:
http://soton.mpeforth.com/flag/fms/index.html

```forth
include FMS-SI.f
include FMS-SILib.f

var x  \ instantiate a class var object named x
x add: \ => "aborted: message not understood"

```



## Go

This uses reflection as in [[Send an unknown method call#Go]], but goes one more step to put the reflection code in a method.  This allows an unknown method call to be handled by this method of the receiving object.

```go
package main

import (
    "fmt"
    "reflect"
)

type example struct{}

func (example) Foo() int {
    return 42
}

// a method to call another method by name
func (e example) CallMethod(n string) int {
    if m := reflect.ValueOf(e).MethodByName(n); m.IsValid() {
        // it's known.  call it.
        return int(m.Call(nil)[0].Int())
    }
    // otherwise it's unknown.  handle as needed.
    fmt.Println("Unknown method:", n)
    return 0
}

func main() {
    var e example
    fmt.Println(e.CallMethod("Foo"))
    fmt.Println(e.CallMethod("Bar"))
}
```

{{out}}

```txt

42
Unknown method: Bar
0

```



## Groovy

Groovy allows us to capture all unknown method calls using the methodMissing function

```groovy
class MyObject {
    def foo() {
        println 'Invoked foo'
    }
    def methodMissing(String name, args) {
        println "Invoked missing method $name$args"
    }
}
```


Testing:

```groovy
def o = new MyObject()
o.foo()
o.bar()
o.bar(1, 2, 'Test')
```


{{out}}

```txt

Invoked foo
Invoked missing method bar[]
Invoked missing method bar[1, 2, Test]

```

==Icon and {{header|Unicon}}==
Unicon implements objects via a translator that emits native code. While Icon does not support objects, the native code could potentially be translated and run under Icon provided other Unicon extensions are not used.

Unicon does not natively allow a class to intercept unknown method calls and this task was originally marked as an omit.  However, there are several ways that this functionality could be added to Unicon including:
* Using a try/catch (as per [[:Category:Unicon_Code_Library|The Unicon Code Library]]) to catch the error (invalid method call). This is not very general and adds a lot of syntax for each method call.
* Using execution monitoring to catch and handle the error in a parallel co-expression.  This would be very general and transparent requiring no extra syntax.  It would be an excellent choice for a debugger.
* Using a procedure to invoke the method and catch any error.  While not as general as the execution monitor approach, it doesn't require as much extra syntax as a try/catch and is focused specifically on method calls.

The example below is based upon the last case.  The procedure 'DynMethod' would be used in place of normal method invocation.  A special method 'UndefinedMethod' can be defined to handle unknown methods.  The procedure 'DynMethod' requires knowledge of the internals of the code emitted to support objects.


```Unicon
procedure DynMethod(obj,meth,arglist[])
   local m

   if not (type(obj) ? ( tab(find("__")), ="__state", pos(0))) then
      runerr(205,obj)                       # invalid value - not an object

   if meth == ("initially"|"UndefinedMethod") then fail  # avoid protected

   m := obj.__m                                          # get methods list
   if fieldnames(m) == meth then                         # method exists?
      return m[meth]!push(copy(arglist),obj)             # ... call it
   else
      if fieldnames(m) == "UndefinedMethod" then         # handler exists?
         return obj.UndefinedMethod!arglist              # ... call it
      else runerr(207,obj)                  # error invalid method (i.e. field)
end
```



```Unicon
procedure main()
   x := foo()
   y := foo2()

   x.a()                                      # example of normal method call
   DynMethod(x,"a")                           # using DynMethod
   DynMethod(x,"simplydoesntexist")           # results in error
   DynMethod(y,"simplydoesntexist")           # catches error
   DynMethod(y,"simplydoesntexist",1,2,3,4,5) # with parameters
end

class foo(A) # sample class and methods
   method a(p1)
      l1 := p1
      return
   end
   method b(p2)
      l2 := p2
      return
   end
   initially
      i1 := 0
      return
end

class foo2 : foo (A)
   method UndefinedMethod(x[])  # Undefined Method handler
      write(&errout,"You called an undefinded method of this object.")
      return
   end
end
```



## Io

{{trans|Python}}


```io
Example := Object clone do(
    foo := method(writeln("this is foo"))
    bar := method(writeln("this is bar"))
    forward := method(
        writeln("tried to handle unknown method ",call message name)
        if( call hasArgs,
            writeln("it had arguments: ",call evalArgs)
        )
    )
)

example := Example clone

example foo          // prints "this is foo"
example bar          // prints "this is bar"
example grill        // prints "tried to handle unknown method grill"
example ding("dong") // prints "tried to handle unknown method ding"
                     // prints "it had arguments: list("dong")"
```



## J


In J, you can define a method at invocation time, and this definition can be in <code>z</code> which is the parent from which all objects and classes inherit.

For example, we could define

```J
example=:3 :0
   doSomething_z_=: assert&0 bind 'doSomething was not implemented'
   doSomething__y ''
)

doSomething_adhoc1_=: smoutput bind 'hello world'
dSomethingElse_adhoc2_=: smoutput bind 'hello world'
```


With those definitions in a fresh session (where <code>adhoc2</code> has not been given a definition for <code>doSomething</code>), we get the following behavior:


```J
   example <'adhoc1'
hello world
   example<'adhoc2'
|doSomething was not implemented: assert
```


(Note that need to have a cover verb (or adverb or conjunction) for the method call if you want to abstract it or dynamically intercept previously undefined methods.)


## JavaScript


There is a way (a bit unconfortable if you compare it to php´s way), involves using Proxy interface defined on ES 6, it isn´t supported still on all vendors, but for updated info, view [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Proxy MDN proxy] site
{{works with|Firefox}}
{{works with|IE}}

```javascript

obj  = new Proxy({},
        { get : function(target, prop)
            {
                if(target[prop] === undefined)
                    return function()  {
                        console.log('an otherwise undefined function!!');
                    };
                else
                    return target[prop];
            }
        });
obj.f()        ///'an otherwise undefined function!!'
obj.l = function() {console.log(45);};
obj.l();       ///45

```

Where {} is the object to wrap in the proxy, but it could be a reference to another object, and the second argument to proxy´s constructor are the handlers that supersede the behavior of the target ({}), the 'get' function gets executed '''every time''' a property from target ({}) is needed, in this case, only if the property didn´t existed before, it returns the dynamically generated function.


There are more handlers, that go from modify the way properties are added, to how to retrieve __proto__, just read the docs!.

Before Proxy interface, firefox (only) supported a suitable trap:


```javascript
var example = new Object;
example.foo = function () {
  alert("this is foo");
}
example.bar = function () {
  alert("this is bar");
}
example.__noSuchMethod__ = function (id, args) {
  alert("tried to handle unknown method " + id);
  if (args.length != 0)
    alert("it had arguments: " + args);
}

example.foo();        // alerts "this is foo"
example.bar();        // alerts "this is bar"
example.grill();      // alerts "tried to handle unknown method grill"
example.ding("dong"); // alerts "tried to handle unknown method ding"
                      // alerts "it had arguments: dong
```



## Julia

Julia will throw a MethodError exception if its multiple dispatch cannot find a proper function for a given set of arguments. This exception can be caught and resolved within a generically typed function that is not specific as to the object type of its arguments.

```julia

function add(a, b)
   try
       a + b
   catch
       println("caught exception")
       a * b
   end
end


println(add(2, 6))
println(add(1//2, 1//2))
println(add("Hello ", "world"))

```



## Kotlin

Kotlin JS does not currently target ECMAScript 2015 and so the Proxy object cannot be used for this task. The only way it can currently be accomplished is to use the Mozilla extension __noSuchMethod__ property which works with Firefox 43 but is no longer supported by more up to date versions:

```scala
// Kotlin JS version 1.2.0 (Firefox 43)

class C {
    // this method prevents a TypeError being thrown if an unknown method is called
    fun __noSuchMethod__(id: String, args: Array<Any>) {
        println("Class C does not have a method called $id")
        if (args.size > 0) println("which takes arguments: ${args.asList()}")
    }
}

fun main(args: Array<String>) {
    val c: dynamic = C()  // 'dynamic' turns off compile time checks
    c.foo() // the compiler now allows this call even though foo() is undefined
}
```


{{out}}

```txt

Class C does not have a method called foo

```



## Lasso

Unknown methods are handled by the inclusion of the special "_unknownTag" method.

If this is not included in the type, an error will result that may terminate processing unless otherwise handled.

```Lasso>define exampletype =
 type {
	public foo() => {
		return 'this is foo\r'
	}
	public bar() => {
		return 'this is bar\r'
	}
	public _unknownTag(...) => {
		local(n = method_name->asString)
		return 'tried to handle unknown method called "'+#n+'"'+
			(#rest->size ? ' with args: "'+#rest->join(',')+'"')+'\r'
	}
}

local(e = exampletype)

#e->foo()
// outputs 'this is foo'

#e->bar()
// outputs 'this is bar'

#e->stuff()
// outputs 'tried to handle unknown method called "stuff"'

#e->dothis('here',12,'there','nowhere')
// outputs 'tried to handle unknown method called "dothis" with args: "here,12,there,nowhere"'
```



## Logtalk


There are two ways to handle unknown messages. From the sender side, we can catch the exception that is generated when an object doesn't understand a message:


```logtalk

:- object(foo).

	:- public(try/0).
	try :-
		catch(bar::message, Error, handler(Error)).

	handler(error(existence_error(predicate_declaration,message/0),_)) :-
		% handle the unknown message
		...

:- end_object.

```


From the receiver side, an object can implement the built-in protocol "forwarding", defining the handler for messages that it doesn't understand (e.g. by forwarding or delegating it to another object):


```logtalk

:- object(foo,
	implements(forwarding)).

	forward(Message) :-
		% handle the unknown message
		...

:- end_object.

```



## Lua


This is specifically the purpose of the __index metamethod:


```Lua

local object={print=print}
setmetatable(object,{__index=function(t,k)return function() print("You called the method",k)end end})
object.print("Hi") -->Hi
object.hello() -->You called the method hello

```



## M2000 Interpreter

Normally we can't get an exception from object when we call an unknown method (module in M2000). The exception raised from the caller.


So here we have a custom invoke, three of them, one for modules, two for functions (for numeric and string values).


Each of them before actually invoke the method, check if the method exist, and if exist then make the call.


To check if a method exist we make a group (user object in M2000) with that method with an empty block for code, and we use Valid() to see if the left object has all the members of the right (where we have the one we want to check).




```M2000 Interpreter

module checkit  {

      Class Alfa {
            k=1000
            module a (x, y) {
                  Print x, y
            }
            module NoParam {
                  Print "ok"
            }
            Function Sqr(x) {
                  =Sqrt(x)
            }
            Function NoParam {
                  =.k
            }
            Function Sqr$(x) {
                  =Str$(Sqrt(x),1033)  ' using locale 1033, no leading space for positive
            }
      }
      \\ modules, functions numeric and string, and variables can use same name
      \\ here we have module invoke, function invoke, and function invoke$
      Module invoke (&a, method$) {
            param=(,)
            Read ? param
            Function Check(&a, method$) {
                  group b type "module "+method$+" {}"
                   =valid(@a as b)
            }
            if check(&a, method$) then {
                        for a {
                              \\ we call this.methodname
                              call "."+method$, !param
                        }
            }  else  Flush : Error  "unknown method "+method$   ' flush empty the stack
      }
      Function invoke (&a, method$) {
            \\ functions have own stack of values
            Function Check(&a, method$) {
                  group b type "Function "+filter$(method$, "()")+" {}"
                   =valid(@a as b)
            }
            if check(&a, method$) then {
                        for a {
                              =Function("."+method$, ![])
                        }
            }  else  Error  "unknown Function "+method$
      }
      \\ invoke string functions
      Function invoke$ (&a, method$) {
            \\ functions have own stack of values
            Function Check(&a, method$) {
                  group b type "Function "+filter$(method$, "()")+" {}"
                   =valid(@a as b)
            }
            if check(&a, method$) then {
                        for a {
                              \\ [] is a function which return current stack as a stack object, and pass to current stack a new stack object.
                              =Function$("."+method$, ![])
                        }
            }  else  Error  "unknown Function "+method$
      }

      Module obj.alfa {
            Flush 'empty stack
            Print "this is a fake module, is not part of obj"
      }
      Function obj.alfa {
                  Print "this is a fake function, is not part of obj"
      }
      Obj=Alfa()
      \\ normal invoke, interpreter not know that this is an object method
      \\ this method has a weak reference to obj, so anytime we use This. or just dot, this weak reference make the real name to execute
      Obj.a 10,20
      \\ call the fake method (can't access object methods and properties), has empty weak reference to object
      obj.alfa 10, 20

      \\ check before call using custom invoke
      \\ to check if a method (module) exist, we have to compare this object with other temporary object
      \\ we make one with the method name and empty definition, and then check if obj has anything this temp object has
      \\ arguments passed in a tuple (optional), so we didn't leave stack with unused items, if we have an unknown method.
      invoke &obj, "a", (10, 20)
      invoke &obj, "NoParam"
      \\ now with an unknown method, using alfa
      Try ok {
            invoke &obj, "Alfa", (10, 20)
      }
      If Error Then Print Error$
      \\ we can use invoke for functions
      Print Invoke(&obj, "Sqr()", 4), Invoke(&obj, "NoParam()")
      Print Invoke$(&obj, "Sqr$()",2)
      \ without custom invoke
      Print obj.Sqr(4), obj.Noparam(), obj.Sqr$(2)
      \\ so now we try to call Alfa() and Alfa$()  (unknown functions)
      Try ok {
            Print Invoke(&obj, "Alfa()")
      }
      If Error Then Print Error$
      Try ok {
            Print Invoke$(&obj, "Alfa$()")
      }
      If Error Then Print Error$


      \\ so now lets copy obj to obj2
      \\ fake method didn't passed to new object
      obj2=obj
      Try ok {
            invoke &obj2, "alfa", (10, 20)
      }
      If Error Then Print Error$

      p->obj2
      \\ if p is a pointer to named group we can pass it as is
      invoke &p, "a", (10, 20)
      \\ normal called
      p=>a 10,20

      For p {
            invoke &this, "a", (10, 20)
            Try ok {
                  invoke &this, "alfa", (10, 20)
            }
            If Error Then Print Error$
      }

      p->(obj2)   ' p point to a copy of obj2 (an unnamed group)
      For p {
            invoke &this, "a", (10, 20)
            \\ normal called
            p=>a 10, 20
            Try ok {
                  invoke &this, "alfa", (10, 20)
            }
            If Error Then Print Error$

      }

}
checkit

```



## Mathematica

By default, when evaluating a symbol's <code>DownValues</code>, Mathematica picks the most specific.

```Mathematica
obj[foo] = "This is foo.";
obj[bar] = "This is bar.";
obj[f_Symbol] := "What is " <> SymbolName[f] <> "?";
Print[obj@foo];
Print[obj@bar];
Print[obj@baz];
```

{{out}}

```txt
This is foo.
This is bar.
What is baz?
```



## Object Pascal

{{incorrect|Object Pascal|Fails to meet at least two requirements. 1) Defining an abstract method constitutes predefining the method; bark is not an unknown method, but a known, unimplemented method. 2) The animal object isn't handling the situation; the caller gets an exception.}}

An exception about calling a virtual method will be raised and interrupt the current code flow. But in that situation, the function is actually known. The concept of a function not being known at all does not exist in Object Pascal, due to static name lookup checking. That is to say, the call a.b() causes the compiler to verify that the symbol b exists in the namespace associated with expression a, and refers to a function.

To avoid the possibility of an abstract method call, one common solution is to leave an empty implementation for the virtual method instead of making it abstract. This is what an abstract method call looks like:


```pascal

type
  Tanimal = class
  public
    procedure bark(); virtual; abstract;
  end;

implementation

var
  animal: Tanimal;

initialization

  animal := Tanimal.Create;
  animal.bark(); // abstract method call exception at runtime here
  animal.Free;

end.

```


=={{header|Objective-C}}==
<code>-forwardInvocation:</code> is usually used to "forward" the message on to another object to handle.


```objc
#include <Foundation/Foundation.h>

// The methods need to be declared somewhere
@interface Dummy : NSObject
- (void)grill;
- (void)ding:(NSString *)s;
@end

@interface Example : NSObject
- (void)foo;
- (void)bar;
@end

@implementation Example
- (void)foo {
  NSLog(@"this is foo");
}

- (void)bar {
  NSLog(@"this is bar");
}

- (void)forwardInvocation:(NSInvocation *)inv {
  NSLog(@"tried to handle unknown method %@", NSStringFromSelector([inv selector]));
  NSUInteger n = [[inv methodSignature] numberOfArguments];
  for (NSUInteger i = 0; i < n-2; i++) { // First two arguments are the object and selector.
    id __unsafe_unretained arg;          // We assume that all arguments are objects.
                                         // getArguments: is type-agnostic and does not perform memory management,
                                         //   therefore we must pass it a pointer to an unretained type
    [inv getArgument:&arg atIndex:i+2];
    NSLog(@"argument #%lu: %@", i, arg);
  }
}

// forwardInvocation: does not work without methodSignatureForSelector:
// The runtime uses the signature returned here to construct the invocation.
- (NSMethodSignature *)methodSignatureForSelector:(SEL)aSelector {
  int numArgs = [[NSStringFromSelector(aSelector) componentsSeparatedByString:@":"] count] - 1;
  // we assume that all arguments are objects
  // The type encoding is "v@:@@@...", where "v" is the return type, void
  // "@" is the receiver, object type; ":" is the selector of the current method;
  // and each "@" after corresponds to an object argument
  return [NSMethodSignature signatureWithObjCTypes:
          [[@"v@:" stringByPaddingToLength:numArgs+3 withString:@"@" startingAtIndex:0] UTF8String]];
}
@end

int main()
{
  @autoreleasepool {

    id example = [[Example alloc] init];

    [example foo];          // prints "this is foo"
    [example bar];          // prints "this is bar"
    [example grill];        // prints "tried to handle unknown method grill"
    [example ding:@"dong"]; // prints "tried to handle unknown method ding:"
                            // prints "argument #0: dong"

  }
  return 0;
}
```




## Oforth


In Oforth, a method is an object of Method class. This object is not related to a particular class or hierarchy : each class can provide an implementation for a particular method.

Method call is resolved at runtime : oforth looks at the top of the stack (that will become the method receiver) and searchs for a valid implementation of the method called (into receiver class or its parents). If no implementation is found for this method, doesNotUnderstand method is called instead.


```Oforth
1 first
[1:interpreter] ExRuntime : 1 does not understand method <#first>
```



```Oforth
1 "first" asMethod perform
[1:interpreter] ExRuntime : 1 does not understand method <#first>
```


Oforth has not concept such as "unknow method" : if a method exists, it can be called (even if the object on top of stack does not understand it). If not, no call is possible.


```Oforth
1 "unknow_method" asMethod perform
[1:interpreter] ExRuntime : null does not understand method <#perform>
```


doesNotUnderstand can be redefined for a particular class :


```Oforth
Object Class new: MyClass
MyClass method: doesNotUnderstand(m)
   "Well, sorry, I don't understand " print m println ;

MyClass new first
Well, sorry, I don't understand #first
```



## ooRexx

To respond to unknown method calls, classes can implement an <code>unknown</code> method.  This method is passed the name of the method and an array of the arguments that were used on the call.

```ooRexx
u = .unknown~new
u~foo(1, 2, 3)

::class unknown
::method unknown
  use arg name, args
  say "Unknown method" name "invoked with arguments:" args~tostring('l',', ')
```

Output:

```txt
Unknown method FOO invoked with arguments: 1, 2, 3
```



## Oz

To respond to unknown method calls, classes can implement the <code>otherwise</code> method. As its sole argument, this method gets the received message, i.e. a record with the name of the unknown method as its label and the arguments as the record features.

```oz
declare
  class Example
     meth init skip end

     meth foo {System.showInfo foo} end

     meth bar {System.showInfo bar} end

     meth otherwise(Msg)
        {System.showInfo "Unknown method "#{Label Msg}}
        if {Width Msg} > 0 then
           {System.printInfo "Arguments: "}
           {System.show {Record.toListInd Msg}}
        end
     end
  end

  Object = {New Example init}
 in
  {Object foo}
  {Object bar}
  {Object grill}
  {Object ding(dong)}
```


Output:

```txt

foo
bar
Unknown method grill
Unknown method ding
Arguments: [1#dong]

```



## Perl


```perl
package Example;
sub new {
    bless {}
}
sub foo {
    print "this is foo\n";
}
sub bar {
    print "this is bar\n";
}
sub AUTOLOAD {
    my $name = $Example::AUTOLOAD;
    my ($self, @args) = @_;
    print "tried to handle unknown method $name\n";
    if (@args) {
        print "it had arguments: @args\n";
    }
}
sub DESTROY {}          # dummy method to prevent AUTOLOAD from
                        # being triggered when an Example is
                        # destroyed

package main;
my $example = Example->new;

$example->foo;          # prints "this is foo"
$example->bar;          # prints "this is bar"
$example->grill;        # prints "tried to handle unknown method Example::grill"
$example->ding("dong"); # prints "tried to handle unknown method Example::ding"
                        # and "it had arguments: dong"
```


## Perl 6



```perl6
class Farragut {
    method FALLBACK ($name, *@rest) {
        say "{self.WHAT.perl}: $name.tc() the @rest[], full speed ahead!";
    }
}

class Sparrow is Farragut { }

Farragut.damn: 'torpedoes';
Sparrow.hoist: <Jolly Roger mateys>;
```

{{out}}

```txt
Farragut: Damn the torpedoes, full speed ahead!
Sparrow: Hoist the Jolly Roger mateys, full speed ahead!
```


<tt>[http://design.perl6.org/S12.html#FALLBACK_methods FALLBACK]</tt> will be called for any method that is not defined. Since any class inherits from <tt>Any</tt>, there will be plenty of already defined methods. Those which are not defined can also be used as L-Values by the magic of <tt>[http://design.perl6.org/S12.html#Lvalue_methods is rw]</tt>.


```perl6
class L-Value {
    our $.value = 10;
    method FALLBACK($name, |c) is rw { $.value }
}

my $l = L-Value.new;
say $l.any-odd-name; # 10
$l.some-other-name = 42;
say $l.i-dont-know; # 42
```



## Phix

Phix is not object orientated, but this sort of thing is very easy to emulate.

```Phix
enum METHODS

function invoke(object o, string name, sequence args={})
--(this works on any class, for any function, with any number or type of parameters)
    integer mdict = o[METHODS]
    integer node = getd_index(name,mdict)
    if node!=0 then
        return call_func(getd_by_index(node,mdict),args)
    end if
    return "no such method" -- or throw(), fatal(), etc
end function

--class X: Xmethods emulates a vtable
constant Xmethods = new_dict()

function exists()
    return "exists"
end function

setd("exists",routine_id("exists"),Xmethods)

--class X: create new instances
function newX()
    return {Xmethods}
end function

object x = newX()

?invoke(x,"exists")
?invoke(x,"non_existent_method")
```


{{out}}

```txt

"exists"
"no such method"

```



## PHP


```php
<?php
class Example {
  function foo() {
    echo "this is foo\n";
  }
  function bar() {
    echo "this is bar\n";
  }
  function __call($name, $args) {
    echo "tried to handle unknown method $name\n";
    if ($args)
      echo "it had arguments: ", implode(', ', $args), "\n";
  }
}

$example = new Example();

$example->foo();        // prints "this is foo"
$example->bar();        // prints "this is bar"
$example->grill();      // prints "tried to handle unknown method grill"
$example->ding("dong"); // prints "tried to handle unknown method ding"
                        // prints "it had arguments: dong
?>
```



## PicoLisp

The function '[http://software-lab.de/doc/refT.html#try try]' is used to send a message to an object for which it is not known whether it inherits a method for that message or not. As opposed to the syntacically equivalent '[http://software-lab.de/doc/refS.html#send send]' function, 'try' does not give an error, but returns NIL. We might redefine 'send' to get an effect analog to CLOS.

```PicoLisp
(redef send (Msg Obj . @)
   (or
      (pass try Msg Obj)
      (pass 'no-applicable-method> Obj Msg) ) )

(de no-applicable-method> (This Msg)
   (pack "No method for " Msg " on " This) )

(class +A)

(dm do-something> ()
   (pack "Do something to " This) )
```

Test:

```PicoLisp
: (object 'A '(+A))
-> A
: (object 'B '(+B))
-> B
: (list (send 'do-something> 'A) (send 'do-something> 'B))
-> ("Do something to A" "No method for do-something> on B")
```



## Pike

Pike allows to overload the <code>-></code> operator used to access object members:

```Pike
class CatchAll
{
    mixed `->(string name)
    {
        return lambda(int arg){ write("you are calling %s(%d);\n", name, arg); };
    }
}

> CatchAll()->hello(5);
you are calling hello(5);
> CatchAll()->something(99);
you are calling something(99);
```



## Python

Python objects can implement a <code>__getattr__()</code> method to handle accesses of unknown attributes (methods are just attributes that are callable; so this function handles both methods and non-method fields). Here we assume that if you access an unknown attribute, you want a method, so we return a function that can be called.

```python
class Example(object):
    def foo(self):
        print("this is foo")
    def bar(self):
        print("this is bar")
    def __getattr__(self, name):
        def method(*args):
            print("tried to handle unknown method " + name)
            if args:
                print("it had arguments: " + str(args))
        return method

example = Example()

example.foo()        # prints “this is foo”
example.bar()        # prints “this is bar”
example.grill()      # prints “tried to handle unknown method grill”
example.ding("dong") # prints “tried to handle unknown method ding”
                     # prints “it had arguments: ('dong',)”
```



## Racket

Racket's usual object system can't deal with unknown methods, but we can capture the relevant exception and deal with it:

```racket

#lang racket

(require racket/class)

(define-syntax-rule (send~ obj method x ...)
  ;; note: this is a naive macro, a real one should avoid evaluating `obj' and
  ;; the `xs' more than once
  (with-handlers ([(λ(e) (and (exn:fail:object? e)
                              ;; only do this if there *is* an `unknown-method'
                              (memq 'unknown-method (interface->method-names
                                                     (object-interface o)))))
                   (λ(e) (send obj unknown-method 'method x ...))])
    (send obj method x ...)))

(define foo%
  (class object%
    (define/public (foo x)
      (printf "foo: ~s\n" x))
    (define/public (unknown-method name . xs)
      (printf "Unknown method ~s: ~s\n" name xs))
    (super-new)))

(define o (new foo%))
(send~ o foo 1) ; => foo: 1
(send~ o whatever 1) ; Unknown method whatever: (1)

```


Alternatively, we can use Swindle for a CLOS-like object system, and do something similar to the Common Lisp solution:

```racket

#lang swindle

(defgeneric (foo x))
(defmethod (no-applicable-method [m (singleton foo)] xs)
  (echo "No method in" m "for" :w xs))
(defmethod (foo [x <integer>]) (echo "It's an integer"))

(foo 1)
;; => It's an integer

(foo "one")
;; => No method in #<generic:foo> for "one"

```



## Ruby


```ruby
class Example
    def foo
        puts "this is foo"
    end
    def bar
        puts "this is bar"
    end
    def method_missing(name, *args, &block)
        puts "tried to handle unknown method %s" % name # name is a symbol
        unless args.empty?
            puts "it had arguments: %p" % [args]
        end
    end
end

example = Example.new

example.foo          # prints “this is foo”
example.bar          # prints “this is bar”
example.grill        # prints “tried to handle unknown method grill”
example.ding("dong") # prints “tried to handle unknown method ding”
                     # prints “it had arguments: ["dong"]”
```



## Scala

{{works with|Scala|2.9}}
As of scala 2.9, scalac must receive the -Xexperimental optional for Dynamic to receive this treatment.

```scala
class DynamicTest extends Dynamic
{
  def foo()=println("this is foo")
  def bar()=println("this is bar")
  def applyDynamic(name: String)(args: Any*)={
    println("tried to handle unknown method "+name)
    if(!args.isEmpty)
      println("  it had arguments: "+args.mkString(","))
  }
}

object DynamicTest {
  def main(args: Array[String]): Unit = {
    val d=new DynamicTest()
    d.foo()
    d.bar()
    d.grill()
    d.ding("dong")
  }
}

```

Output:

```txt
this is foo
this is bar
tried to handle unknown method grill
tried to handle unknown method ding
  it had arguments: dong
```



## Sidef

The special '''AUTOLOAD''' method gets called when a method isn't defined in the current class:

```ruby
class Example {
    method foo {
        say "this is foo"
    }
    method bar {
        say "this is bar"
    }
    method AUTOLOAD(_, name, *args) {
        say ("tried to handle unknown method %s" % name);
        if (args.len > 0) {
            say ("it had arguments: %s" % args.join(', '));
        }
    }
}

var example = Example.new;

example.foo;          # prints “this is foo”
example.bar;          # prints “this is bar”
example.grill;        # prints “tried to handle unknown method grill”
example.ding("dong"); # prints “tried to handle unknown method ding”
                      # prints “it had arguments: dong”
```



## Slate


Here is an example of unknown methods being used to call shell commands (this is already defined in the base image):


```slate
define: #shell &builder: [lobby newSubSpace].

_@shell didNotUnderstand: message at: position
"Form a command string and execute it."
[
  position > 0
    ifTrue: [resend]
    ifFalse:
      [([| :command |
	message selector isUnarySelector ifTrue:
	  [command ; message selector.
	   message optionals pairsDo:
	     [| :key :value |
	      command ; ' -' ; (key as: String) allButFirst allButLast ; ' ' ; (value as: String)]].
	message selector isKeywordSelector ifTrue:
	  [| keywords args |
	   keywords: ((message selector as: String) splitWith: $:).
	   command ; keywords first.
	   keywords size = 1 ifTrue: "Read a string or array of arguments."
	     [args: message arguments second.
	      (args is: String) ifTrue: [command ; ' ' ; args]
				ifFalse: [args do: [| :arg | command ; ' ' ; arg]]]]] writingAs: String)
	 ifNil: [resend] ifNotNilDo: [| :cmd | [Platform run: cmd]]]
].
```


Here is an example of it being used:


```slate
slate[1]> shell ls: '*.image'.
kernel.new.little.64.1244260494374694.image  slate2.image
net.image                                    slate.image
True
slate[2]>
```




## Smalltalk

{{works with|GNU Smalltalk}}


```smalltalk
Object subclass: CatchThemAll [
    foo [ 'foo received' displayNl ]

    bar [ 'bar received' displayNl ]

    doesNotUnderstand: aMessage [
      ('message "' , (aMessage selector asString) , '"') displayNl.
      (aMessage arguments) do: [ :a |
        'argument: ' display. a printNl.
      ]
    ]
]

|a| a := CatchThemAll new.
a foo.
a bar.
a weCanDoIt.
a theyCanToo: 'eat' and: 'walk'.
```


There are two ways to catch unimplemented messages:
* on the receiver side, by redefining the "doesNotUnderstand:" method in the receiver class hierarchy, as shown above.
* on the sender side, by catching the MessageNotUnderstood exception, as follows:

```smalltalk
[
   bla := someObject fooBar.
   foo := bla.
] on: MessageNotUnderstood do:[:ex |
   ex return: 'fortyTwo'
]
```

this will leave 'fortyTwo' on bla AND foo (because the handler proceeds).
This sender-side handling is useful if you don't want to or if you are not allowed to change the receiver's class hierarchy (which is not a technical, but solely a political/conventional limitation, because in Smalltalk no class is closed, and extensions can be added to any class simply by loading or dynamically adding methods - even to the base system or third party packages).

Of course, this handler now catches any other unimplemented messages as well, thus if foobar was implemented, but itself sends another bad message, we'd catch that as well.
We can check for this in the handler, by checking for which message got us there:

```smalltalk
[
   bla := someObject fooBar.
   foo := bla.
] on: MessageNotUnderstood do:[:ex |
   ((ex message selector == #fooBar) and: [ ex message receiver == someObject])
   ifTrue:[
       ex return: 'fortyTwo'
   ] ifFalse:[
       ex reject
   ]
]
```

the reject will re-reaise the exception, and lead us to an outer handler, or the debugger, if there is none.
{{works with|Smalltalk/X}}
There is a utility method for exactly the above (it catches only #fooBar to the original receiver):

```smalltalk
anObject perform:#fooBar ifNotUnderstood:[ ...some alternative code and return value... ].
```




## SuperCollider


```SuperCollider
Ingorabilis {

	tell {
		"I told you so".postln;
	}

	find {
		"I found nothing".postln
	}

	doesNotUnderstand { |selector ... args|
		"Method selector '%' not understood by %\n".postf(selector, this.class);
		"Giving you some good arguments in the following".postln;
		args.do { |x| x.postln };
		"And now I delegate the method to my respected superclass".postln;
		super.doesNotUnderstand(selector, args)
	}

}

```

Usage:


```SuperCollider

i = Ingorabilis.new;
i.tell; // prints "I told you so"
i.find; // prints ""I found nothing"
i.think(1, 3, 4, 7);

```


The latter answers:

```SuperCollider
Method selector 'think' not understood by Ingorabilis
Giving you some good arguments in the following
1
3
4
7
And now I delegate the method to my respected superclass
ERROR: Message 'think' not understood.
RECEIVER:
Instance of Ingorabilis {    (0x1817b1398, gc=D4, fmt=00, flg=00, set=00)
  instance variables [0]
}
<...>
```

Catch the method call:

```SuperCollider

i = Ingorabilis.new
try { i.think } { "We are not delegating to super, because I don't want it".postln };

```



## Tcl

{{works with|Tcl|8.6}} or {{libheader|TclOO}}

```tcl
package require TclOO
# First create a simple, conventional class and object
oo::class create Example {
    method foo {} {
        puts "this is foo"
    }
    method bar {} {
        puts "this is bar"
    }
}
Example create example

# Modify the object to have a custom ‘unknown method’ interceptor
oo::objdefine example {
    method unknown {name args} {
        puts "tried to handle unknown method \"$name\""
        if {[llength $args]} {
            puts "it had arguments: $args"
        }
    }
}

# Show off what we can now do...
example foo;       # prints “this is foo”
example bar;       # prints “this is bar”
example grill;     # prints “tried to handle unknown method "grill"”
example ding dong; # prints “tried to handle unknown method "ding"”
                   # prints “it had arguments: dong”
```



## UNIX Shell

{{works with|Bash}}
bash doesn't have objects with methods, but it can handle unknown commands:

```Bash
function handle_error {
  status=$?

  # 127 is: command not found
  if [[ $status -ne 127 ]]; then
    return
  fi

  lastcmd=$(history | tail -1 | sed 's/^ *[0-9]* *//')

  read cmd args <<< "$lastcmd"

  echo "you tried to call $cmd"
}

# Trap errors.
trap 'handle_error' ERR
```

Sample usage:
 $ foo
 bash: foo: command not found
 you tried to call foo


## zkl

If something can not be resolved (in a class) the function __notFound is called. It can redirect to another object.

```zkl
class C{ fcn __notFound(name){println(name," not in ",self); bar}
   fcn bar{vm.arglist.println("***")}
}
```


```txt

C.foo //-->"foo not in Class(C)", returns Fcn(bar)
C.foo(1,2,3) //-->"foo not in Class(C)", "L(1,2,3)***"

```



{{omit from|Ada}}
{{omit from|Assembly}} <!-- Objects do not exist in Assembly! -->
{{omit from|AutoHotkey}}
{{omit from|AWK}}
{{omit from|Axe}}
{{omit from|C}} <!-- static compiled, no runtime function creation -->
{{omit from|C++}}
{{omit from|F_Sharp}}
{{omit from|Java}}
{{omit from|Lily}}
{{omit from|Locomotive Basic}}
{{omit from|M4}}
{{omit from|MATLAB}}
{{omit from|Maxima}}
{{omit from|NetRexx}}
{{omit from|OCaml}}
{{omit from|PARI/GP}}
{{omit from|PowerShell}}
{{omit from|PureBasic}}
{{omit from|Retro}}
{{omit from|Rust}}
{{omit from|Swift}}
{{omit from|TI-89 BASIC}} <!-- No objects -->
{{omit from|ZX Spectrum Basic}}
