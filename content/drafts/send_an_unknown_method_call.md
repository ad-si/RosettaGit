+++
title = "Send an unknown method call"
description = ""
date = 2019-10-18T19:19:52Z
aliases = []
[extra]
id = 10406
[taxonomies]
categories = []
tags = []
+++

{{task|Object oriented}}

;Task:
Invoke an object method where the name of the method to be invoked can be generated at run time. 


;Related tasks:
* [[Respond to an unknown method call]].
* [[Runtime evaluation]]





## AutoHotkey

This object has 3 methods, and asks the user to name one to call. Instead of using Func(), one could use a class definition.

```AHK
obj := {mA: Func("mA"), mB: Func("mB"), mC: Func("mC")}
InputBox, methodToCall, , Which method should I call?
obj[methodToCall].()

mA(){
 MsgBox Method A
}
mB(){
 MsgBox Method B
}
mC(){
 MsgBox Method C
}

```



## Bracmat


```Bracmat
(task=
  ( oracle
  =   (predicate="is made of green cheese")
      (generateTruth=.str$(!arg " " !(its.predicate) "."))
      (generateLie=.str$(!arg " " !(its.predicate) "!"))
  )
& new$oracle:?SourceOfKnowledge
&   put
  $ "You may ask the Source of Eternal Wisdom ONE thing.
Enter \"Truth\" or \"Lie\" on the next line and press the <Enter> key.
"
&   whl
  ' ( get':?trueorlie:~Truth:~Lie
    & put$"Try again\n"
    )
& put$(str$("You want a " !trueorlie ". About what?" \n))
& get'(,STR):?something
& (SourceOfKnowledge..str$(generate !trueorlie))$!something
);

```

{{out|Example}}

```txt
{?} !task
You may ask the Source of Eternal Wisdom ONE thing.
Enter "Truth" or "Lie" on the next line and press the <Enter> key.
"Lie"
You want a Lie. About what?
The sea
{!} The sea is made of green cheese!
```


=={{header|C sharp|C#}}==

```csharp
using System;

class Example
{
    public int foo(int x)
    {
        return 42 + x;
    }
}

class Program
{
    static void Main(string[] args)
    {
        var example = new Example();
        var method = "foo";
        
        var result = (int)example.GetType().GetMethod(method).Invoke(example, new object[]{ 5 });
        Console.WriteLine("{0}(5) = {1}", method, result);
    }
}

```

{{out}}
  foo(5) = 47

=={{header|Caché ObjectScript}}==

$METHOD executes a named instance method for a specified instance of a designated class.


```cos
Class Unknown.Example Extends %RegisteredObject
{

Method Foo()
{
	Write "This is foo", !
}

Method Bar()
{
	Write "This is bar", !
}

}
```

{{out|Examples}}

```txt

USER>Set obj=##class(Unknown.Example).%New()
USER>Do $Method(obj, "Foo")
This is foo
USER>Do $Method(obj, "Bar")
This is bar

```



## Common Lisp

Unknown methods are called just like any other function. Find the method-naming symbol using INTERN then call it with FUNCALL.

```lisp
(funcall (intern "SOME-METHOD") my-object a few arguments)
```


=={{header|Déjà Vu}}==

```dejavu
local :object { :add @+ }
local :method :add

!. object! method 1 2
```

{{out}}

```txt
3
```



## E


This example goes well with the object named <code>example</code> in [[Respond to an unknown method call#E]].


```e
for name in ["foo", "bar"] {
    E.call(example, name, [])
}
```


## Elena

ELENA 4.1 :

```elena
import extensions;
 
class Example
{
    foo(x)
        = x + 42;
}
 
public program()
{
    var example := new Example();
    var methodSignature := "foo";
 
    var invoker := new MessageName(methodSignature);
    var result := invoker(example,5);
 
    console.printLine(methodSignature,"(",5,") = ",result)
}
```

{{out}}

```txt

foo(5) = 47

```



## Factor

Factor's object model is such that objects themselves don't contain methods — generic words do. So there is nothing different about invoking an unknown method than invoking an unknown word in general.

```factor
USING: accessors kernel math prettyprint sequences words ;
IN: rosetta-code.unknown-method-call

TUPLE: foo num ;
C: <foo> foo
GENERIC: add5 ( x -- y )
M: foo add5 num>> 5 + ;

42 <foo>              ! construct a foo
"add" "5" append      ! construct a word name
                      ! must specify vocab to look up a word
"rosetta-code.unknown-method-call"
lookup-word execute . ! 47
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

\ Use a standard Forth string and evaluate it.
\ This is equivalent to sending the !: message to object x
42 x  s" !:"  evaluate  

x p: 42     \ send the print message ( p: ) to x to verify the contents 


```



## Go


```go
package main

import (
    "fmt"
    "reflect"
)

type example struct{}

// the method must be exported to be accessed through reflection.
func (example) Foo() int {
    return 42
}

func main() {
    // create an object with a method
    var e example
    // get the method by name
    m := reflect.ValueOf(e).MethodByName("Foo")
    // call the method with no argments
    r := m.Call(nil)
    // interpret first return value as int
    fmt.Println(r[0].Int()) // => 42
}
```



## Groovy


```grrovy
class Example {
    def foo(value) {
        "Invoked with '$value'"
    }
}

def example = new Example()
def method = "foo"
def arg = "test value"

assert "Invoked with 'test value'" == example."$method"(arg)
```


==Icon and {{header|Unicon}}==

```Unicon
procedure main()
   x := foo()    # create object
   x.m1()        # static call of m1 method
   #  two examples where the method string can be dynamically constructed ...
   "foo_m1"(x)   # ... need to know class name and method name to construct name
   x.__m["m1"]   # ... general method (better)
end

class foo(a,b,c) # define object
method m1(x)
end
end
```


For more information on this see [[Respond_to_an_unknown_method_call#Icon_and_Unicon|Respond to an unknown method call]].

## Io

String literal "foo" may be replaced by any expression resulting in a string.

```Io
Example := Object clone
Example foo := method(x, 42+x)

name := "foo"
Example clone perform(name,5) println  // prints "47"
```



## J


'''Solution''': There are multiple ways to evoke code at runtime. The most common is '''<tt>". y</tt>''' (''eval''uate the code in the string y, producing a noun), but there's also <tt>'name'~ </tt> (which will modify J's stack by replacing the two tokens '''<tt>'name'</tt>''' and <tt>~</tt> with the named object) as well as '''<tt>x 128!:2 y</tt>''' (''apply'' the verb described by <tt>x</tt> to the noun <tt>y</tt>).  

There are other methods as well, e.g., '''<tt>@.</tt>''','''<tt>`:</tt>''', and '''<tt>^:</tt>''', though these are designed to consume gerunds (pre-parsed ASTs) rather than strings (though, of course, a pre-processor can always be provided to convert strings into ASTs before feeding them to these operators).

'''Example''':
```j
   sum =: +/
   prod =: */
   count =: #

   nameToDispatch =: 'sum'    NB. pick a name already defined

   ". nameToDispatch,' 1 2 3'
6
   nameToDispatch~ 1 2 3
6
   nameToDispatch (128!:2) 1 2 3
6

   nameToDispatch =: 'count'  NB. pick another name

   ". nameToDispatch,' 1 2 3'
3
   nameToDispatch~ 1 2 3
3
   nameToDispatch (128!:2) 1 2 3
3
```



## Java

Using reflection

```java
import java.lang.reflect.Method;

class Example {
  public int foo(int x) {
    return 42 + x;
  }
}

public class Main {
  public static void main(String[] args) throws Exception {
    Object example = new Example();
    String name = "foo";
    Class<?> clazz = example.getClass();
    Method meth = clazz.getMethod(name, int.class);
    Object result = meth.invoke(example, 5); // result is int wrapped in an object (Integer)
    System.out.println(result);        // prints "47"
  }
}
```



## JavaScript

String literal "foo" may be replaced by any expression resulting in a string

```javascript
example = new Object;
example.foo = function(x) {
    return 42 + x;
};

name = "foo";
example[name](5)      # => 47
```



## Julia

{{works with|Julia|0.6}}


```julia
const functions = Dict{String,Function}(
    "foo" => x -> 42 + x,
    "bar" => x -> 42 * x)

@show functions["foo"](3)
@show functions["bar"](3)
```


{{out}}

```txt
(functions["foo"])(3) = 45
(functions["bar"])(3) = 126
```



## Kotlin

When you try to compile the following program, it will appear to the compiler that the local variable 'c' is assigned but never used and a warning will be issued accordingly. You can get rid of this warning by compiling using the -nowarn flag.

```scala
// Kotlin JS version 1.1.4-3

class C {
    fun foo() {
        println("foo called")
    }
}

fun main(args: Array<String>) {
    val c = C()
    val f = "c.foo"
    js(f)()  // invokes c.foo dynamically
}
```


{{out}}

```txt

foo called 

```



## Lasso


```Lasso>define mytype =
 type {
	public foo() => {
		return 'foo was called'
	}
	public bar() => {
		return 'this time is was bar'
	}
}
local(obj = mytype, methodname = tag('foo'), methodname2 = tag('bar'))
#obj->\#methodname->invoke
#obj->\#methodname2->invoke
```

{{out}}

```txt
foo was called
this time is was bar
```



## Lingo


```lingo
obj = script("MyClass").new()
-- ...
method = #foo
arg1 = 23
res = call(method, obj, arg1)
```



## Logtalk

For this task, we first define a simple object with a single method:

```logtalk
:- object(foo).

	:- public(bar/1).
	bar(42).

:- end_object.
```
Second, we define another object that asks the user for a message to be sent to the first object:
```logtalk

:- object(query_foo).

	:- public(query/0).
	query :-
		write('Message: '),
		read(Message),
		foo::Message.
		write('Reply: '),
		write(Message), nl.

:- end_object.
```
After compiling and loading both objects, we can try:
 | ?- query_foo::query.
 Message: bar(X).
 Reply: bar(42)

## Lua

Don't forget to pass the object for methods!

```lua
local example = { }
function example:foo (x) return 42 + x end

local name = "foo"
example[name](example, 5) --> 47
```



## Mathematica

Creates a dialog box where one can type a function (Sin, Cos, Tan ...) and then a second dialog box for a value.
<lang>ToExpression[Input["function? E.g. Sin",]][Input["value? E.g. 0.4123"]]
```

{{out}}

```txt

Input: Sin
Input: 3.1415
Output: 0.0000926536

```



=={{header|MATLAB}} / {{header|Octave}}==



```Matlab

  funName = 'foo';   % generate function name
  feval (funNAME, ...)  % evaluation function with optional parameters

  funName = 'a=atan(pi)';   % generate function name
  eval (funName, 'printf(''Error\n'')')

```


=={{header|Objective-C}}==

```objc>#import <Foundation/Foundation.h


@interface Example : NSObject
- (NSNumber *)foo;
@end

@implementation Example
- (NSNumber *)foo {
  return @42;
}
@end

int main (int argc, const char *argv[]) {
  @autoreleasepool {

    id example = [[Example alloc] init];
    SEL selector = @selector(foo); // or = NSSelectorFromString(@"foo");
    NSLog(@"%@", [example performSelector:selector]);
  
  }
  return 0;
}
```

The <code>performSelector: ...</code> methods can only be used with methods with 0 - 2 object arguments, and an object or <code>void</code> return type. For all other calls, one can create an <code>NSInvocation</code> object and invoke it, or directly call one of the <code>objc_msgSend</code> family of runtime functions.


## Oforth


A method object can be retrieved from its name using asMethod.

```Oforth
16 "sqrt" asMethod perform
```

   
Others : 
   asFuntion  : retrieve a function
   asClass    : retrieve a class
   asProperty : retrieve a property

A generic way to search a word into the dictionary in to use find method : 

```Oforth
16 "sqrt" Word find perform
```



## PARI/GP


```parigp
foo()=5;
eval(Str("foo","()"))
```



## Perl


```perl
package Example;
sub new {
    bless {}
}
sub foo {
    my ($self, $x) = @_;
    return 42 + $x;
}

package main;
my $name = "foo";
print Example->new->$name(5), "\n"; # prints "47"
```



## Perl 6

Just for the fun of it, we'll mix in an anonymous role into an integer instead of defining a class.

```perl6
my $object = 42 but role { method add-me($x) { self + $x } }
my $name = 'add-me';
say $object."$name"(5);  # 47
```

The double quotes are required, by the way; without them the variable would be interpreted as a hard ref to a method.


## Phix

Not specifically anything to do with objects, but you can construct routine names at runtime:

```Phix
procedure Hello()
    ?"Hello"
end procedure

string erm = "Hemmm"
for i=3 to 5 do
    erm[i]+=-1+(i=5)*3
end for

call_proc(routine_id(erm),{})
```



## PHP


```php
<?php
class Example {
  function foo($x) {
    return 42 + $x;
  }
}

$example = new Example();

$name = 'foo';
echo $example->$name(5), "\n";        // prints "47"

// alternately:
echo call_user_func(array($example, $name), 5), "\n";
?>
```



## PicoLisp

This can be done with the '[http://software-lab.de/doc/refS.html#send send]' function.

```PicoLisp
(send (expression) Obj arg1 arg2)
```



## Pike

with [] instead of -> a string can be used to name a method:

```Pike
string unknown = "format_nice";
object now = Calendar.now();
now[unknown]();
```



## PowerShell

A random method using a random number:

```PowerShell

$method = ([Math] | Get-Member -MemberType Method -Static | Where-Object {$_.Definition.Split(',').Count -eq 1} | Get-Random).Name
$number = (1..9 | Get-Random) / 10
$result = [Math]::$method($number)
$output = [PSCustomObject]@{
    Method = $method
    Number = $number
    Result = $result
}

$output | Format-List

```

{{Out}}

```txt

Method : Atan
Number : 0.5
Result : 0.463647609000806

```



## Python

String literal "foo" may be replaced by any expression resulting in a string

```python
class Example(object):
     def foo(self, x):
             return 42 + x

name = "foo"
getattr(Example(), name)(5)      # => 47
```



## Qi


```qi

(define foo -> 5)

(define execute-function
  Name -> (eval [(INTERN Name)]))

(execute-function "foo")

```



## Racket


```racket

#lang racket 
(define greeter
  (new (class object% (super-new)
         (define/public (hello name)
           (displayln (~a "Hello " name "."))))))

; normal method call
(send greeter hello "World")

; sending an unknown method
(define unknown 'hello)
(dynamic-send greeter unknown "World")

```



## Ruby

You may replace :foo, :bar or "bar" with any expression that returns a Symbol or String.


```ruby
class Example
  def foo
    42
  end
  def bar(arg1, arg2, &block)
    block.call arg1, arg2
  end
end

symbol = :foo
Example.new.send symbol                         # => 42
Example.new.send( :bar, 1, 2 ) { |x,y| x+y }    # => 3
args = [1, 2]
Example.new.send( "bar", *args ) { |x,y| x+y }  # => 3
```


Object#send can also call protected and private methods, skipping the usual access checks. Ruby 1.9 adds Object#public_send, which only calls public methods.

{{works with|Ruby|1.9}}

```ruby
class Example
  private
  def privacy; "secret"; end
  public
  def publicity; "hi"; end
end

e = Example.new
e.public_send :publicity  # => "hi"
e.public_send :privacy    # raises NoMethodError
e.send :privacy           # => "secret"
```



## Scala

{{libheader|Scala}}
```scala
class Example {
  def foo(x: Int): Int = 42 + x
}

object Main extends App {
  val example = new Example

  val meth = example.getClass.getMethod("foo", classOf[Int])

  assert(meth.invoke(example, 5.asInstanceOf[AnyRef]) == 47.asInstanceOf[AnyRef], "Not confirm expectation.")
  println(s"Successfully completed without errors. [total ${scala.compat.Platform.currentTime - executionStart} ms]")
}
```



## Sidef


```ruby
class Example {
    method foo(x) {
        42 + x
    }
}

var name = 'foo'
var obj = Example()

say obj.(name)(5)          # prints: 47
say obj.method(name)(5)    # =//=
```



## Smalltalk


```smalltalk
Object subclass: #Example.

Example extend [
  foo: x [
    ^ 42 + x ] ].

symbol := 'foo:' asSymbol. " same as symbol := #foo: "

Example new perform: symbol with: 5. " returns 47 "
```

The <code>perform:with:with:</code> family of methods exist for methods with 0 - 2 (3 in [[GNU Smalltalk]]) arguments. For methods with more arguments, use <code>perform:withArguments:</code>, which takes an array of arguments.


## Tcl

Method names are really just strings, i.e., ordinary values that can be produced by any mechanism:

```tcl
package require Tcl 8.6
oo::class create Example {
    method foo {} {return 42}
    method 1 {s} {puts "fee$s"}
    method 2 {s} {puts "fie$s"}
    method 3 {s} {puts "foe$s"}
    method 4 {s} {puts "fum$s"}
}
set eg [Example new]
set mthd [format "%c%c%c" 102 111 111];    # A "foo" by any other means would smell as sweet
puts [$eg $mthd]
for {set i 1} {$i <= 4} {incr i} {
    $eg $i ...
}
```

{{out|The above produces this output}}
 42
 fee...
 fie...
 foe...
 fum...


## zkl


```zkl
name:="len"; "this is a test".resolve(name)() //-->14
```


{{omit from|Ada}}
{{omit from|Axe}}
{{omit from|BASIC}}
{{omit from|C|No such thing as object method}}
{{omit from|GUISS}}
{{omit from|Rust|No runtime reflection}}
{{omit from|ZX Spectrum Basic}}
