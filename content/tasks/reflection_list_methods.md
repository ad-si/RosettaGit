+++
title = "Reflection/List methods"
description = ""
date = 2019-03-30T04:52:34Z
aliases = []
[extra]
id = 21022
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "clojure",
  "csharp",
  "d",
  "elena",
  "factor",
  "go",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lingo",
  "lua",
  "perl",
  "perl_6",
  "phix",
  "php",
  "python",
  "ring",
  "ruby",
  "scala",
  "sidef",
  "tcl",
  "zkl",
]
+++

 [[Category:Programming Tasks]] [[Category:Object oriented]]

## Task

The goal is to get the methods of an object, as names, values or both.

Some languages offer [[Respond to an unknown method call|dynamic methods]], which in general can only be inspected if a class' public API includes a way of listing them.


## C#


```c#
using System;
using System.Reflection;

public class Rosetta
{
    public static void Main()
    {
        //Let's get all methods, not just public ones.
        BindingFlags flags = BindingFlags.Instance | BindingFlags.Static
            | BindingFlags.Public | BindingFlags.NonPublic
            | BindingFlags.DeclaredOnly;

        foreach (var method in typeof(TestForMethodReflection).GetMethods(flags))
            Console.WriteLine(method);
    }

    class TestForMethodReflection
    {
        public void MyPublicMethod() {}
        private void MyPrivateMethod() {}

        public static void MyPublicStaticMethod() {}
        private static void MyPrivateStaticMethod() {}
    }

}
```

```txt

Void MyPublicMethod()
Void MyPrivateMethod()
Void MyPublicStaticMethod()
Void MyPrivateStaticMethod()

//If we do not use BindingFlags.DeclaredOnly, we also get:
System.String ToString()
Boolean Equals(System.Object)
Int32 GetHashCode()
System.Type GetType()
Void Finalize()
System.Object MemberwiseClone()
```



## Clojure



```clojure

; Including listing private methods in the clojure.set namespace:
=> (keys (ns-interns 'clojure.set))
(union map-invert join select intersection superset? index bubble-max-key subset? rename rename-keys project difference)

; Only public:
=> (keys (ns-publics 'clojure.set))
(union map-invert join select intersection superset? index subset? rename rename-keys project difference)
```



## D

D allows you to perform compile-time reflection for code generation, such as printing a list of the functions in a struct or class.

```D
struct S {
    bool b;

    void foo() {}
    private void bar() {}
}

class C {
    bool b;

    void foo() {}
    private void bar() {}
}

void printMethods(T)() if (is(T == class) || is(T == struct)) {
    import std.stdio;
    import std.traits;

    writeln("Methods of ", T.stringof, ":");
    foreach (m; __traits(allMembers, T)) {
        static if (__traits(compiles, (typeof(__traits(getMember, T, m))))) {
            alias typeof(__traits(getMember, T, m)) ti;
            static if (isFunction!ti) {
                writeln("    ", m);
            }
        }
    }
}

void main() {
    printMethods!S;
    printMethods!C;
}
```


```txt
Methods of S:
    foo
    bar
Methods of C:
    foo
    bar
    toString
    toHash
    opCmp
    opEquals
    factory
```



## Elena

ELENA 4.x :

```elena
import system'routines;
import system'dynamic;
import extensions;

class MyClass
{
    myMethod1() {}

    myMethod2(x) {}
}

public program()
{
    var o := new MyClass();

    o.__getClass().__getMessages().forEach:(p)
    {
        console.printLine("o.",p)
    }
}
```

```txt

o.equal[1]
o.notequal[1]
o.myMethod1[0]
o.myMethod2[1]
o.myMethod2[1]
o.#cast[0]

```



## Factor

In Factor, methods are contained in generic words rather than objects, while methods specialize on a class. Therefore, the programmer must decide whether they want the list of methods in a generic word, or the list of methods that specialize on a class. Luckily, the <tt>methods</tt> word can do either depending on what type you give it (a word or a class). The returned sequence contains first-class word values suitable for executing.

```factor
USING: io math prettyprint see ;

"The list of methods contained in the generic word + :" print
\ + methods . nl

"The list of methods specializing on the fixnum class:" print
fixnum methods .
```

```txt

The list of methods contained in the generic word + :
{ M\ bignum + M\ complex + M\ fixnum + M\ float + M\ ratio + }

The list of methods specializing on the fixnum class:
{
    M\ fixnum '
    M\ fixnum (bit-count)
    M\ fixnum (eql?)
    M\ fixnum (log2)
    M\ fixnum (positive>dec)
    M\ fixnum (random-integer)
    M\ fixnum *
    M\ fixnum +
    M\ fixnum -
    M\ fixnum /f
    M\ fixnum /i
    M\ fixnum /mod
    M\ fixnum <
    M\ fixnum <=
    M\ fixnum >
    M\ fixnum >=
    M\ fixnum >bignum
    M\ fixnum >fixnum
    M\ fixnum >float
    M\ fixnum >integer
    M\ fixnum ^n
    M\ fixnum bit?
    M\ fixnum bitand
    M\ fixnum bitnot
    M\ fixnum bitor
    M\ fixnum bitxor
    M\ fixnum eql?
    M\ fixnum equal?
    M\ fixnum hashcode*
    M\ fixnum integer>fixnum
    M\ fixnum integer>fixnum-strict
    M\ fixnum max
    M\ fixnum min
    M\ fixnum mod
    M\ fixnum number=
    M\ fixnum real<=>
    M\ fixnum shift
    M\ fixnum u<
    M\ fixnum u<=
    M\ fixnum u>
    M\ fixnum u>=
}

```



## Go

Shows the name,
[https://golang.org/ref/spec#Method_expressions method expression] and
[https://golang.org/ref/spec#Method_values method value]
of each exported method.

```go
package main

import (
	"fmt"
	"image"
	"reflect"
)

type t int // A type definition

// Some methods on the type
func (r t) Twice() t       { return r * 2 }
func (r t) Half() t        { return r / 2 }
func (r t) Less(r2 t) bool { return r < r2 }
func (r t) privateMethod() {}

func main() {
	report(t(0))
	report(image.Point{})
}

func report(x interface{}) {
	v := reflect.ValueOf(x)
	t := reflect.TypeOf(x) // or v.Type()
	n := t.NumMethod()
	fmt.Printf("Type %v has %d exported methods:\n", t, n)
	const format = "%-6s %-46s %s\n"
	fmt.Printf(format, "Name", "Method expression", "Method value")
	for i := 0; i < n; i++ {
		fmt.Printf(format,
			t.Method(i).Name,
			t.Method(i).Func.Type(),
			v.Method(i).Type(),
		)
	}
	fmt.Println()
}
```

```txt

Type main.t has 3 exported methods:
Name   Method expression                              Method value
Half   func(main.t) main.t                            func() main.t
Less   func(main.t, main.t) bool                      func(main.t) bool
Twice  func(main.t) main.t                            func() main.t

Type image.Point has 8 exported methods:
Name   Method expression                              Method value
Add    func(image.Point, image.Point) image.Point     func(image.Point) image.Point
Div    func(image.Point, int) image.Point             func(int) image.Point
Eq     func(image.Point, image.Point) bool            func(image.Point) bool
In     func(image.Point, image.Rectangle) bool        func(image.Rectangle) bool
Mod    func(image.Point, image.Rectangle) image.Point func(image.Rectangle) image.Point
Mul    func(image.Point, int) image.Point             func(int) image.Point
String func(image.Point) string                       func() string
Sub    func(image.Point, image.Point) image.Point     func(image.Point) image.Point

```



## J


```j

   NB. define a stack class
   coclass 'Stack'
   create =: 3 : 'items =: i. 0'
   push =: 3 : '# items =: items , < y'
   top =: 3 : '> {: items'
   pop =: 3 : ([;._2' a =. top 0; items =: }: items; a;')
   destroy =: codestroy
   cocurrent 'base'

   names_Stack_''      NB. all names
create  destroy pop     push    top

   'p' names_Stack_ 3  NB. verbs that start with p
pop  push


   NB. make an object.  The dyadic definition of cownew invokes the create verb
   S =: conew~ 'Stack'

   names__S''          NB. object specific names
COCREATOR items


   pop__S              NB. introspection: get the verbs definition
3 : 0
 a =. top 0
 items =: }: items
 a
)


   NB. get the search path of object S
   copath S
┌─────┬─┐
│Stack│z│
└─────┴─┘


   names__S 0         NB. get the object specific data
COCREATOR items


```



## Java


```java
import java.lang.reflect.Method;

public class ListMethods {
    public int examplePublicInstanceMethod(char c, double d) {
        return 42;
    }

    private boolean examplePrivateInstanceMethod(String s) {
        return true;
    }

    public static void main(String[] args) {
        Class clazz = ListMethods.class;

        System.out.println("All public methods (including inherited):");
        for (Method m : clazz.getMethods()) {
            System.out.println(m);
        }
        System.out.println();
        System.out.println("All declared methods (excluding inherited):");
        for (Method m : clazz.getDeclaredMethods()) {
            System.out.println(m);
        }
    }
}
```

```txt

public static void ListMethods.main(java.lang.String[])
public int ListMethods.examplePublicInstanceMethod(char,double)
public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException
public final native void java.lang.Object.wait(long) throws java.lang.InterruptedException
public final void java.lang.Object.wait() throws java.lang.InterruptedException
public boolean java.lang.Object.equals(java.lang.Object)
public java.lang.String java.lang.Object.toString()
public native int java.lang.Object.hashCode()
public final native java.lang.Class java.lang.Object.getClass()
public final native void java.lang.Object.notify()
public final native void java.lang.Object.notifyAll()

All declared methods (excluding inherited):
public static void ListMethods.main(java.lang.String[])
public int ListMethods.examplePublicInstanceMethod(char,double)
private boolean ListMethods.examplePrivateInstanceMethod(java.lang.String)

```



## JavaScript

In JavaScript, methods are properties that are functions, so methods are retrieved by [[Reflection/List properties|getting properties]] and filtering. There are multiple ways of getting property names, each of which include different subsets of an object's properties, such as enumerable or inherited properties.


```javascript
// Sample classes for reflection
function Super(name) {
    this.name = name;
    this.superOwn = function() { return 'super owned'; };
}
Super.prototype = {
    constructor: Super
    className: 'super',
    toString: function() { return "Super(" + this.name + ")"; },
    doSup: function() { return 'did super stuff'; }
}

function Sub() {
    Object.getPrototypeOf(this).constructor.apply(this, arguments);
    this.rest = [].slice.call(arguments, 1);
    this.subOwn = function() { return 'sub owned'; };
}
Sub.prototype = Object.assign(
    new Super('prototype'),
    {
        constructor: Sub
        className: 'sub',
        toString: function() { return "Sub(" + this.name + ")"; },
        doSub: function() { return 'did sub stuff'; }
    });

Object.defineProperty(Sub.prototype, 'shush', {
    value: function() { return ' non-enumerable'; },
    enumerable: false // the default
});

var sup = new Super('sup'),
    sub = new Sub('sub', 0, 'I', 'two');

Object.defineProperty(sub, 'quiet', {
    value: function() { return 'sub owned non-enumerable'; },
    enumerable: false
});

// get enumerable methods on an object and its ancestors
function get_method_names(obj) {
    var methods = [];
    for (var p in obj) {
        if (typeof obj[p] == 'function') {
            methods.push(p);
        }
    }
    return methods;
}

get_method_names(sub);
//["subOwn", "superOwn", "toString", "doSub", "doSup"]

// get enumerable properties on an object and its ancestors
function get_property_names(obj) {
    var properties = [];
    for (var p in obj) {
        properties.push(p);
    }
    return properties;
}

// alternate way to get enumerable method names on an object and its ancestors
function get_method_names(obj) {
    return get_property_names(obj)
        .filter(function(p) {return typeof obj[p] == 'function';});
}

get_method_names(sub);
//["subOwn", "superOwn", "toString", "doSub", "doSup"]

// get enumerable & non-enumerable method names set directly on an object
Object.getOwnPropertyNames(sub)
    .filter(function(p) {return typeof sub[p] == 'function';})
//["subOwn", "shhh"]

// get enumerable method names set directly on an object
Object.keys(sub)
    .filter(function(p) {return typeof sub[p] == 'function';})
//["subOwn"]

// get enumerable method names & values set directly on an object
Object.entries(sub)
    .filter(function(p) {return typeof p[1] == 'function';})
//[["subOwn", function () {...}]]
```



## Julia

```julia
methods(methods)
methods(println)
```


```txt
# 3 methods for generic function "methods":
methods(f::Core.Builtin) in Base at reflection.jl:588
methods(f::ANY) in Base at reflection.jl:601
methods(f::ANY, t::ANY) in Base at reflection.jl:580

# 3 methods for generic function "println":
println(io::IO) in Base at coreio.jl:6
println(io::IO, xs...) in Base at strings/io.jl:54
println(xs...) in Base at coreio.jl:5

```



## Kotlin

Note that kotlin-reflect.jar needs to be included in the classpath for this program.

```scala
// Version 1.2.31

import kotlin.reflect.full.functions

open class MySuperClass {
    fun mySuperClassMethod(){}
}

open class MyClass : MySuperClass() {
    fun myPublicMethod(){}

    internal fun myInternalMethod(){}

    protected fun myProtectedMethod(){}

    private fun myPrivateMethod(){}
}

fun main(args: Array<String>) {
    val c = MyClass::class
    println("List of methods declared in ${c.simpleName} and its superclasses:\n")
    val fs = c.functions
    for (f in fs) println("${f.name}, ${f.visibility}")
}
```


```txt

List of methods declared in MyClass and its superclasses:

myInternalMethod, INTERNAL
myPrivateMethod, PRIVATE
myProtectedMethod, PROTECTED
myPublicMethod, PUBLIC
equals, PUBLIC
hashCode, PUBLIC
mySuperClassMethod, PUBLIC
toString, PUBLIC

```



## Lingo


```lingo
-- parent script "MyClass"

on foo (me)
  put "foo"
end

on bar (me)
  put "bar"
end
```



```lingo
obj = script("MyClass").new()
put obj.handlers()
-- [#foo, #bar]

-- The returned list contains the object's methods ("handlers") as "symbols".
-- Those can be used like this to call the corresponding method:
call(#foo, obj)
-- "foo"

call(#bar, obj)
-- "bar"
```



## Lua


```lua
function helloWorld()
    print "Hello World"
end

-- Will list all functions in the given table, but does not recurse into nexted tables
function printFunctions(t)
    local s={}
    local n=0
    for k in pairs(t) do
        n=n+1 s[n]=k
    end
    table.sort(s)
    for k,v in ipairs(s) do
        f = t[v]
        if type(f) == "function" then
            print(v)
        end
    end
end

printFunctions(_G)
```

```txt
assert
collectgarbage
dofile
error
gcinfo
getfenv
getmetatable
helloWorld
ipairs
load
loadfile
loadstring
module
newproxy
next
pairs
pcall
print
printFunctions
rawequal
rawget
rawset
require
select
setfenv
setmetatable
tonumber
tostring
type
unpack
xpcall
```


=={{header|Objective-C}}==

```objc
#import <Foundation/Foundation.h>

#import <objc/runtime.h>

@interface Foo : NSObject
@end
@implementation Foo
- (int)bar:(double)x {
  return 42;
}
@end

int main() {
  unsigned int methodCount;
  Method *methods = class_copyMethodList([Foo class], &methodCount);
  for (unsigned int i = 0; i < methodCount; i++) {
    Method m = methods[i];
    SEL selector = method_getName(m);
    const char *typeEncoding = method_getTypeEncoding(m);
    NSLog(@"%@\t%s", NSStringFromSelector(selector), typeEncoding);
  }
  free(methods);
  return 0;
}
```

```txt

bar:	i24@0:8d16

```



## Perl

Given this simple class, display results of introspection of the symbol table hash.
Note that the overloaded comparison operator also shows up in the list of methods.


```perl
package Nums;

use overload ('<=>' => \&compare);
sub new     { my $self = shift; bless [@_] }
sub flip    { my @a = @_; 1/$a }
sub double  { my @a = @_; 2*$a }
sub compare { my ($a, $b) = @_; abs($a) <=> abs($b) }

my $a = Nums->new(42);
print "$_\n" for %{ref ($a)."::" });
```

```txt
double
(<=>
new
BEGIN
flip
compare
((
```


Another alternative is the module <code>Class::MOP</code>, which implements a meta-object protocol for the Perl. It alters nothing about Perl's object system; it is just a tool for manipulation and introspection. Note that this output includes methods inherited methods (<tt>DOES, VERSION, can, isa</tt>)

```perl
use Class::MOP;
my $meta = Class::MOP::Class->initialize( ref $a );
say join "\n", $meta->get_all_method_names()
```

```txt
compare
new
(<=>
VERSION
double
isa
flip
can
DOES
```



## Perl 6


You can get a list of an object's methods using <tt>.^methods</tt>, which is part of the [https://docs.perl6.org/type/Metamodel$COLON$COLONClassHOW Meta Object Protocol].

Each is represented as a <tt>Method</tt> object that contains a bunch of info:


```perl6
class Foo {
    method foo ($x)      { }
    method bar ($x, $y)  { }
    method baz ($x, $y?) { }
}

my $object = Foo.new;

for $object.^methods {
    say join ", ", .name, .arity, .count, .signature.gist
}
```


```txt

foo, 2, 2, (Foo $: $x, *%_)
bar, 3, 3, (Foo $: $x, $y, *%_)
baz, 2, 3, (Foo $: $x, $y?, *%_)

```



## Phix

Phix is not object orientated, but this sort of thing is fairly easy to emulate.

```Phix
enum METHODS, PROPERTIES

sequence all_methods = {}

function method_visitor(object key, object /*data*/, object /*user_data*/)
    all_methods = append(all_methods,key)
    return 1
end function

function get_all_methods(object o)
    all_methods = {}
    traverse_dict(routine_id("method_visitor"),0,o[METHODS])
    return all_methods
end function

--class X: Xmethods emulates a vtable
constant Xmethods = new_dict()

function exists()
    return "exists"
end function

setd("exists",routine_id("exists"),Xmethods)

--class X: destructor
procedure destructor(object o)
    destroy_dict(o[PROPERTIES])
end procedure
constant r_destroy = routine_id("destructor")

--class X: create new instances
function newX(object x,y)
    integer Xproperties = new_dict()
    setd("x",x,Xproperties)
    setd("y",y,Xproperties)
    object res = delete_routine({Xmethods,Xproperties},r_destroy)
    return res
end function

object x = newX(2,"string")

?get_all_methods(x)
```

```txt

{"exists"}

```



## PHP


```php
<?
class Foo {
    function bar(int $x) {
    }
}

$method_names = get_class_methods('Foo');
foreach ($method_names as $name) {
    echo "$name\n";
    $method_info = new ReflectionMethod('Foo', $name);
    echo $method_info;
}
?>
```

```txt

bar
Method [ <user> public method bar ] {
  @@ /Users/xuanluo/test.php 3 - 4

  - Parameters [1] {
    Parameter #0 [ <required> int $x ]
  }
}

```



## Python

In Python, methods are properties that are functions, so methods are retrieved by [[Reflection/List properties|getting properties]] and filtering, using (e.g.) <code>[https://docs.python.org/3.5/library/functions.html#dir dir()]</code> and a list comprehension. Python's <code>[https://docs.python.org/3.5/library/inspect.html#module-inspect inspect]</code> module offers a simple way to get a list of an object's methods, though it won't include wrapped, C-native methods (type 'method-wrapper', type 'wrapper_descriptor', or class 'wrapper_descriptor', depending on version). Dynamic methods can be listed by overriding <code>[https://docs.python.org/3/reference/datamodel.html#object.__dir__ __dir__]</code> in the class.


```python
import inspect

# Sample classes for inspection
class Super(object):
  def __init__(self, name):
    self.name = name

  def __str__(self):
    return "Super(%s)" % (self.name,)

  def doSup(self):
    return 'did super stuff'

  @classmethod
  def cls(cls):
    return 'cls method (in sup)'

  @classmethod
  def supCls(cls):
    return 'Super method'

  @staticmethod
  def supStatic():
    return 'static method'

class Other(object):
  def otherMethod(self):
    return 'other method'

class Sub(Other, Super):
  def __init__(self, name, *args):
    super(Sub, self).__init__(name);
    self.rest = args;
    self.methods = {}

  def __dir__(self):
    return list(set( \
        sum([dir(base) for base in type(self).__bases__], []) \
        + type(self).__dict__.keys() \
        + self.__dict__.keys() \
        + self.methods.keys() \
      ))

  def __getattr__(self, name):
    if name in self.methods:
      if callable(self.methods[name]) and self.methods[name].__code__.co_argcount > 0:
        if self.methods[name].__code__.co_varnames[0] == 'self':
          return self.methods[name].__get__(self, type(self))
        if self.methods[name].__code__.co_varnames[0] == 'cls':
          return self.methods[name].__get__(type(self), type)
      return self.methods[name]
    raise AttributeError("'%s' object has no attribute '%s'" % (type(self).__name__, name))

  def __str__(self):
    return "Sub(%s)" % self.name

  def doSub():
    return 'did sub stuff'

  @classmethod
  def cls(cls):
    return 'cls method (in Sub)'

  @classmethod
  def subCls(cls):
    return 'Sub method'

  @staticmethod
  def subStatic():
    return 'Sub method'

sup = Super('sup')
sub = Sub('sub', 0, 'I', 'two')
sub.methods['incr'] = lambda x: x+1
sub.methods['strs'] = lambda self, x: str(self) * x

# names
[method for method in dir(sub) if callable(getattr(sub, method))]
# instance methods
[method for method in dir(sub) if callable(getattr(sub, method)) and hasattr(getattr(sub, method), '__self__') and getattr(sub, method).__self__ == sub]
#['__dir__', '__getattr__', '__init__', '__str__', 'doSub', 'doSup', 'otherMethod', 'strs']
# class methods
[method for method in dir(sub) if callable(getattr(sub, method)) and hasattr(getattr(sub, method), '__self__') and getattr(sub, method).__self__ == type(sub)]
#['__subclasshook__', 'cls', 'subCls', 'supCls']
# static & free dynamic methods
[method for method in dir(sub) if callable(getattr(sub, method)) and type(getattr(sub, method)) == type(lambda:nil)]
#['incr', 'subStatic', 'supStatic']

# names & values; doesn't include wrapped, C-native methods
inspect.getmembers(sub, predicate=inspect.ismethod)
# names using inspect
map(lambda t: t[0], inspect.getmembers(sub, predicate=inspect.ismethod))
#['__dir__', '__getattr__', '__init__', '__str__', 'cls', 'doSub', 'doSup', 'otherMethod', 'strs', 'subCls', 'supCls']
```



## Ring


```ring

# Project : Reflection/List methods

o1 = new test
aList = methods(o1)
for x in aList
     cCode = "o1."+x+"()"
     eval(cCode)
next
Class Test
func f1
       see "hello from f1" + nl
func f2
       see "hello from f2" + nl
func f3
       see "hello from f3" + nl
func f4
       see "hello from f4" + nl

```

Output:

```txt

hello from f1
hello from f2
hello from f3
hello from f4

```



## Ruby

Ruby has various properties that will return lists of methods:
* [http://ruby-doc.org/core/Object.html#method-i-methods Object#methods]
* [http://ruby-doc.org/core/Object.html#method-i-public_methods Object#public_methods]
* [http://ruby-doc.org/core/Object.html#method-i-private_methods Object#private_methods]
* [http://ruby-doc.org/core/Object.html#method-i-protected_methods Object#protected_methods]
* [http://ruby-doc.org/core/Object.html#method-i-singleton_methods Object#singleton_methods]

Dynamic methods can be listed by overriding these methods. Ancestor methods can be filtered out by subtracting a list of methods from the ancestor.


```ruby
# Sample classes for reflection
class Super
  CLASSNAME = 'super'

  def initialize(name)
    @name = name
    def self.superOwn
      'super owned'
    end
  end

  def to_s
    "Super(#{@name})"
  end

  def doSup
    'did super stuff'
  end

  def self.superClassStuff
    'did super class stuff'
  end

  protected
  def protSup
    "Super's protected"
  end

  private
  def privSup
    "Super's private"
  end
end

module Other
  def otherStuff
    'did other stuff'
  end
end

class Sub < Super
  CLASSNAME = 'sub'
  attr_reader :dynamic

  include Other

  def initialize(name, *args)
    super(name)
    @rest = args;
    @dynamic = {}
    def self.subOwn
      'sub owned'
    end
  end

  def methods(regular=true)
    super + @dynamic.keys
  end

  def method_missing(name, *args, &block)
    return super unless @dynamic.member?(name)
    method = @dynamic[name]
    if method.arity > 0
      if method.parameters[0][1] == :self
        args.unshift(self)
      end
      if method.lambda?
        # procs (hence methods) set missing arguments to `nil`, lambdas don't, so extend args explicitly
        args += args + [nil] * [method.arity - args.length, 0].max
        # procs (hence methods) discard extra arguments, lambdas don't, so discard arguments explicitly (unless lambda is variadic)
        if method.parameters[-1][0] != :rest
          args = args[0,method.arity]
        end
      end
      method.call(*args)
    else
      method.call
    end
  end

  def public_methods(all=true)
    super + @dynamic.keys
  end

  def respond_to?(symbol, include_all=false)
    @dynamic.member?(symbol) || super
  end

  def to_s
    "Sub(#{@name})"
  end

  def doSub
    'did sub stuff'
  end

  def self.subClassStuff
    'did sub class stuff'
  end

  protected
  def protSub
    "Sub's protected"
  end

  private
  def privSub
    "Sub's private"
  end
end

sup = Super.new('sup')
sub = Sub.new('sub', 0, 'I', 'two')
sub.dynamic[:incr] = proc {|i| i+1}

p sub.public_methods(false)
#=> [:superOwn, :subOwn, :respond_to?, :method_missing, :to_s, :methods, :public_methods, :dynamic, :doSub, :incr]

p sub.methods - Object.methods
#=> [:superOwn, :subOwn, :method_missing, :dynamic, :doSub, :protSub, :otherStuff, :doSup, :protSup, :incr]

p sub.public_methods - Object.public_methods
#=> [:superOwn, :subOwn, :method_missing, :dynamic, :doSub, :otherStuff, :doSup, :incr]

p sub.methods - sup.methods
#=> [:subOwn, :method_missing, :dynamic, :doSub, :protSub, :otherStuff, :incr]

# singleton/eigenclass methods
p sub.methods(false)
#=> [:superOwn, :subOwn, :incr]
p sub.singleton_methods
#=> [:superOwn, :subOwn]
```



## Scala


### Java Interoperability

{{Out}}Best seen running in your browser by [https://scastie.scala-lang.org/5mLHFfBeQCuGpc9Q7PXxgw Scastie (remote JVM)].

```Scala
object ListMethods extends App {

  private val obj = new {
    def examplePublicInstanceMethod(c: Char, d: Double) = 42

    private def examplePrivateInstanceMethod(s: String) = true
  }
  private val clazz = obj.getClass

  println("All public methods (including inherited):")
  clazz.getMethods.foreach(m => println(s"${m}"))

  println("\nAll declared fields (excluding inherited):")
  clazz.getDeclaredMethods.foreach(m => println(s"${m}}"))

}
```



## Sidef

The super-method ''Object.methods()'' returns an Hash with method names as keys and ''LazyMethod'' objects as values. Each ''LazyMethod'' can be called with zero or more arguments, internally invoking the method on the object on which ''.methods'' was called.

```ruby
class Example {
    method foo { }
    method bar(arg) { say "bar(#{arg})" }
}

var obj = Example()
say obj.methods.keys.sort          #=> ["bar", "call", "foo", "new"]

var meth = obj.methods.item(:bar)  # `LazyMethod` representation for `obj.bar()`
meth(123)                          # calls obj.bar()
```



## Tcl

In TclOO, the <tt>info</tt> command can inspect the complete state of an object or a class, including private and methods:


```Tcl
% info object methods ::oo::class -all -private
<cloned> create createWithNamespace destroy eval new unknown variable varname
```


For <i>many</i> more examples, see https://wiki.tcl.tk/40640 and the linked manuals for <tt>info class</tt> and <tt>info object</tt>.  Plugins for <b>tkcon</b> and <b>twDebugInspector</b> (also found on the wiki) use this to create interactive object inspectors similar to [[Smalltalk]]'s.


## zkl

Every object has a "methods" method, which returns a list of method names [for that object]. If you want to get a method from a string, you can use reflection.

```zkl
methods:=List.methods;
methods.println();
List.method(methods[0]).println();  // == .Method(name) == .BaseClass(name)
```

```txt

L("create","createLong","copy","toString","toBool","toData","toDictionary","toList","isType","isInstanceOf","holds","append","write","writeln","read","readln","extend","insert","find","findBop",...)
Method(TSList.create)

```

