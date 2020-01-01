+++
title = "Add a variable to a class instance at runtime"
description = ""
date = 2019-09-10T10:21:34Z
aliases = []
[extra]
id = 2135
[taxonomies]
categories = []
tags = []
+++

{{task|Object oriented}}

Demonstrate how to dynamically add variables to an object (a class instance) at runtime.

This is useful when the methods/variables of an instance are based on a data file that isn't available until runtime.  Hal Fulton gives an example of creating an OO CSV parser at [http://www.devsource.com/article2/0,1759,1928562,00.asp An Exercise in Metaprogramming with Ruby].  This is referred to as "monkeypatching" by Pythonistas and some others.


## ActionScript

In ActionScript this can be done using an Object object

```actionscript
var object:Object = new Object();
object.foo = "bar";
```

Or by creating a dynamic class

```actionscript
package
{
    public dynamic class Foo
    {
        // ...
    }
}
```


```actionscript
var foo:Foo = new Foo();
foo.bar = "zap";
```

{{omit from|Modula-2}}
{{omit from|Scheme}}


## Ada

Ada is not a dynamically typed language. Yet it supports mix-in inheritance, run-time inheritance and interfaces. These three allow us to achieve the desired effect, however questionably useful it could be. The example declares an interface of the class (Class). Then a concrete type is created (Base). The object E is an instance of Base. Later, at the run time, a new type Monkey_Patch is created such that it refers to E and implements the class interface per delegation to E. Monkey_Patch has a new integer member Foo and EE is an instance of Monkey_Path. For the user EE appears as E with Foo.

```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Dynamic is
   package Abstract_Class is
      type Class is limited interface;
      function Boo (X : Class) return String is abstract;
   end Abstract_Class;
   use Abstract_Class;

   package Base_Class is
      type Base is new Class with null record;
      overriding function Boo (X : Base) return String;
   end Base_Class;

   package body Base_Class is
      function Boo (X : Base) return String is
      begin
         return "I am Class";
      end Boo;
   end Base_Class;
   use Base_Class;

   E : aliased Base;  -- An instance of Base

begin
   -- Gone run-time
   declare
      type Monkey_Patch (Root : access Base) is new Class with record
         Foo : Integer := 1;
      end record;
      overriding function Boo (X : Monkey_Patch) return String;
      function Boo (X : Monkey_Patch) return String is
      begin -- Delegation to the base
         return X.Root.Boo;
      end Boo;
      EE : Monkey_Patch (E'Access); -- Extend E
   begin
      Put_Line (EE.Boo & " with" & Integer'Image (EE.Foo));
   end;
end Dynamic;
```

Sample output:

```txt

I am Class with 1

```



## AutoHotkey

{{works with|AutoHotkey_L}}

```AutoHotkey
e := {}
e.foo := 1
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
It's not really intended that you should do this, but if you must you can:

```bbcbasic
      INSTALL @lib$+"CLASSLIB"

      REM Create a base class with no members:
      DIM class{method}
      PROC_class(class{})

      REM Instantiate the class:
      PROC_new(myobject{}, class{})

      REM Add a member at run-time:
      member$ = "mymember#"
      PROCaddmember(myobject{}, member$, 8)

      REM Test that the member can be accessed:
      PROCassign("myobject." + member$, "PI")
      PRINT EVAL("myobject." + member$)
      END

      DEF PROCaddmember(RETURN obj{}, mem$, size%)
      LOCAL D%, F%, P%
      DIM D% DIM(obj{}) + size% - 1, F% LEN(mem$) + 8
      P% = !^obj{} + 4
      WHILE !P% : P% = !P% : ENDWHILE : !P% = F%
      $$(F%+4) = mem$ : F%!(LEN(mem$) + 5) = DIM(obj{})
      !(^obj{} + 4) = D%
      ENDPROC

      DEF PROCassign(v$, n$)
      IF EVAL("FNassign(" + v$ + "," + n$ + ")")
      ENDPROC
      DEF FNassign(RETURN n, v) : n = v : = 0
```



## Bracmat

This solution saves the original members and methods in a variable, using pattern matching. Then, using macro expansion, a new object is created with an additional member variable and also an additional method. Because the new object is assigned to the same variable as the original object, the original object ceases to exist.

```bracmat
( ( struktuur
  = (aMember=) (aMethod=.!(its.aMember))
  )
& new$struktuur:?object
& out$"Object as originally created:"
& lst$object
& A value:?(object..aMember)
& !object:(=?originalMembersAndMethods)
&     new
    $ (
      ' ( (anotherMember=)
          (anotherMethod=.!(its.anotherMember))
          ()$originalMembersAndMethods
        )
      )
  : ?object
&   out
  $ "
Object with additional member and method and with 'aMember' already set to some interesting value:"
& lst$object
& some other value:?(object..anotherMember)
& out$"
Call both methods and output their return values."
& out$("aMember contains:" (object..aMethod)$)
& out$("anotherMember contains:" (object..anotherMethod)$)
&);
```

Output:

```bracmat
Object as originally created:
(object=
=(aMember=) (aMethod=.!(its.aMember)));

Object with additional member and method and with 'aMember' already set to some interesting value:
(object=

=   (anotherMember=)
    (anotherMethod=.!(its.anotherMember))
    (aMember=A value)
    (aMethod=.!(its.aMember)));

Call both methods and output their return values.
aMember contains: A value
anotherMember contains: some other value
```


## C#
{{works with|C sharp|C#|4.0}}

```c#
// ----------------------------------------------------------------------------------------------
//
//  Program.cs - DynamicClassVariable
//
//     Mikko Puonti, 2013
//
// ----------------------------------------------------------------------------------------------

using System;
using System.Dynamic;

namespace DynamicClassVariable
{
    internal static class Program
    {
        #region Static Members

        private static void Main()
        {
            // To enable late binding, we must use dynamic keyword
            // ExpandoObject readily implements IDynamicMetaObjectProvider which allows us to do some dynamic magic
            dynamic sampleObj = new ExpandoObject();
            // Adding a new property
            sampleObj.bar = 1;
            Console.WriteLine( "sampleObj.bar = {0}", sampleObj.bar );

            // We can also add dynamically methods and events to expando object
            // More information: http://msdn.microsoft.com/en-us/library/system.dynamic.expandoobject.aspx
            // This sample only show very small part of dynamic language features - there is lot's more

            Console.WriteLine( "< Press any key >" );
            Console.ReadKey();
        }

        #endregion
    }
}
```


{{out}}

```txt
sampleObj.bar = 1
< Press any key >
```




## CoffeeScript


```coffeescript
# CoffeeScript is dynamic, just like the Javascript it compiles to.
# You can dynamically add attributes to objects.

# First create an object very simply.
e = {}
e.foo = "bar"
e.yo = -> "baz"
console.log e.foo, e.yo()

# CS also has class syntax to instantiate objects, the details of which
# aren't shown here.  The mechanism to add members is the same, though.
class Empty
  # empty class

e = new Empty()
e.foo = "bar"
e.yo = -> "baz"
console.log e.foo, e.yo()

```



## Common Lisp


This version adds a new slot only to one instance, not to the whole class.

{{libheader|Closer to MOP}}


```lisp
(defun augment-instance-with-slots (instance slots)
  (change-class instance
                (make-instance 'standard-class
                  :direct-superclasses (list (class-of instance))
                  :direct-slots slots)))
```


Example:


```lisp
CL-USER> (let* ((instance (make-instance 'foo :bar 42 :baz 69))
                (new-slots '((:name xenu :initargs (:xenu)))))
           (augment-instance-with-slots instance new-slots)
           (reinitialize-instance instance :xenu 666)
           (describe instance))
#<#<STANDARD-CLASS NIL {1003AEE2C1}> {1003AEE271}>
  [standard-object]

Slots with :INSTANCE allocation:
  BAR   = 42
  BAZ   = 69
  XENU  = 666
```


The following REPL transcript (from [[LispWorks]]) shows the definition of a class <code>some-class</code> with no slots, and the creation of an instance of the class.  The first attempt to access the slot named <code>slot1</code> signals an error as there is no such slot.  Then the class is redefined to have such a slot, and with a default value of 23.  Attempting to access the slot in the preëxisting instance now gives the default value, since the slot has been added to the instance.  This behavior is specified in [http://www.lispworks.com/documentation/HyperSpec/Body/04_cf.htm §4.3.6 Redefining Classes] of the [http://www.lispworks.com/documentation/HyperSpec/Front/index.htm HyperSpec].


```txt
CL-USER 57 > (defclass some-class () ())
#<STANDARD-CLASS SOME-CLASS 200BF63B>

CL-USER 58 > (defparameter *an-instance*
               (make-instance 'some-class))
*AN-INSTANCE*

CL-USER 59 > (slot-value *an-instance* 'slot1)

Error: The slot SLOT1 is missing from #<SOME-CLASS 21F59E37> (of class #<STANDARD-CLASS SOME-CLASS 200BF63B>), when reading the value.
  1 (abort) Return to level 0.
  2 Return to top loop level 0.

Type :b for backtrace, :c <option number> to proceed,  or :? for other options

CL-USER 60 : 1 > :a

CL-USER 61 > (defclass some-class ()
               ((slot1 :initform 23)))
#<STANDARD-CLASS SOME-CLASS 200BF63B>

CL-USER 62 > (slot-value *an-instance* 'slot1)
23
```



## D


```d
struct Dynamic(T) {
    private T[string] vars;

    @property T opDispatch(string key)() pure nothrow {
        return vars[key];
    }

    @property void opDispatch(string key, U)(U value) pure nothrow {
        vars[key] = value;
    }
}

void main() {
    import std.variant, std.stdio;

    // If the type of the attributes is known at compile-time:
    auto d1 = Dynamic!double();
    d1.first = 10.5;
    d1.second = 20.2;
    writeln(d1.first, " ", d1.second);


    // If the type of the attributes is mixed:
    auto d2 = Dynamic!Variant();
    d2.a = "Hello";
    d2.b = 11;
    d2.c = ['x':2, 'y':4];
    d2.d = (int x) => x ^^ 3;
    writeln(d2.a, " ", d2.b, " ", d2.c);
    immutable int x = d2.b.get!int;
}
```

{{out}}

```txt
10.5 20.2
Hello 11 ['x':2, 'y':4]
```

If you want Dynamic to be a class the code is similar. If the attribute names aren't known at compile-time, you have to use a more normal syntax:

```d
import std.stdio, std.variant, std.conv;

struct Dyn {
    Variant[string] data;
    alias data this;
}

void main(string[] args) {
    Dyn d;
    const attribute_name = text("attribute_", args.length);
    d[attribute_name] = "something";
    writeln(d[attribute_name]);
}
```

{{out}}

```txt
something
```



## Elena

ELENA does not support adding a field at run-time but it can be simulated with the help of a mix-in.

ELENA 4.x:

```elena
import extensions;

class Extender : BaseExtender
{
    prop object foo;

    constructor(object)
    {
        theObject := object
    }
}

public program()
{
    var object := 234;

    // extending an object with a field
    object := new Extender(object);

    object.foo := "bar";

    console.printLine(object,".foo=",object.foo);

    console.readChar()
}
```

{{out}}

```txt

234.foo=bar

```



## Falcon

Classes and singleton objects have a fixed structure which cannot be changed during runtime.  However falcon does have capability to add variables/functions at runtime with Prototype based objects.  Below are two of the prototype objects that allow adding variables at runtime.  These are arrays and dictionaries (hashes for the perl type out there).

'''Array:'''
In this example we add a function (which prints out the content of the array) and a new value.  While we are not technically adding a "variable", this example is presented to show similar type of functionality.

```falcon
vect = [ 'alpha', 'beta', 'gamma' ]
vect.dump = function ()
  for n in [0: self.len()]
    > @"$(n): ", self[n]
  end
end
vect += 'delta'
vect.dump()
```

Output from the above:

```falcon
0: alpha
1: beta
2: gamma
3: delta
```

'''Dictionary:'''
In this example we will add a variable through the use of an object from a bless'ed dictionary.  We create a new variable called 'newVar' at runtime and assign a string to it.  Additionally we assign an external, to the object, function (sub_func) to the variable 'sub'.

```falcon
function sub_func( value )
  self['prop'] -= value
  return self.prop
end

dict = bless( [
  'prop' => 0,
  'add' => function ( value )
    self.prop += value
    return self.prop
  end ,
  'sub' => sub_func
])

dict[ 'newVar' ] = "I'm Rich In Data"
```



## FBSL

FBSL class instances aren't expandable with additional, directly accessible public methods at runtime once the class template is defined in the user code. But FBSL has an extremely powerful feature -- an ExecLine() function -- which permits the user to execute any additional code on the fly either privately (bypassing the main code flow) or publicly (interacting with the main code). ExecLine() can be used for a variety of applications from the fine-tuning of current tasks to designing application plug-ins or completely standalone code debuggers. The following class instance may be stuffed up at runtime with any code from simple variables to executable private methods and properties.

```qbasic
#APPTYPE CONSOLE

CLASS Growable

	PRIVATE:

	DIM instructions AS STRING = "Sleep(1)"
	:ExecCode
	DIM dummy AS INTEGER = EXECLINE(instructions, 1)

	PUBLIC:

	METHOD Absorb(code AS STRING)
		instructions = code
		GOTO ExecCode
	END METHOD

	METHOD Yield() AS VARIANT
		RETURN result
	END METHOD

END CLASS

DIM Sponge AS NEW Growable()

Sponge.Absorb("DIM b AS VARIANT = 1234567890: DIM result AS VARIANT = b")
PRINT Sponge.Yield()
Sponge.Absorb("b = ""Hello world!"": result = b")
PRINT Sponge.Yield()

PAUSE
```

'''Output:'''
 1234567890
 Hello world!
 Press any key to continue...


## Forth

{{works with|Forth}}
Works with any ANS Forth

Needs the FMS-SI (single inheritance) library code located here:
http://soton.mpeforth.com/flag/fms/index.html

```forth
include FMS-SI.f
include FMS-SILib.f



\ We can add any number of variables at runtime by adding
\ objects of any type to an instance at run time.  The added
\ objects are then accessible via an index number.

:class foo
  object-list inst-objects \ a dynamically growable object container
  :m init: inst-objects init: ;m
  :m add: ( obj -- ) inst-objects add: ;m
  :m at: ( idx -- obj ) inst-objects at: ;m
;class

foo foo1

: main
  heap> string foo1 add:
  heap> fvar   foo1 add:

  s" Now is the time " 0 foo1 at: !:
  3.14159e             1 foo1 at: !:

  0 foo1 at: p: \ send the print message to indexed object 0
  1 foo1 at: p: \ send the print message to indexed object 1
;

main \ => Now is the time 3.14159

```



## Go

{{trans|Kotlin}}


Firstly, Go doesn't have classes or class variables but does have structs (an analogous concept) which consist of fields.

Adding fields to a struct at runtime is not possible as Go is a statically typed, compiled language and therefore the names of all struct fields need to be known at compile time.

However, as in the case of Groovy and Kotlin, we can ''make it appear'' as though fields are being added at runtime by using the built-in map type. For example:

```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
)

type SomeStruct struct {
    runtimeFields map[string]string
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func main() {
    ss := SomeStruct{make(map[string]string)}
    scanner := bufio.NewScanner(os.Stdin)
    fmt.Println("Create two fields at runtime: ")
    for i := 1; i <= 2; i++ {
        fmt.Printf("  Field #%d:\n", i)
        fmt.Print("       Enter name  : ")
        scanner.Scan()
        name := scanner.Text()
        fmt.Print("       Enter value : ")
        scanner.Scan()
        value := scanner.Text()
        check(scanner.Err())
        ss.runtimeFields[name] = value
        fmt.Println()
    }
    for {
        fmt.Print("Which field do you want to inspect ? ")
        scanner.Scan()
        name := scanner.Text()
        check(scanner.Err())
        value, ok := ss.runtimeFields[name]
        if !ok {
            fmt.Println("There is no field of that name, try again")
        } else {
            fmt.Printf("Its value is '%s'\n", value)
            return
        }
    }
}
```


{{out}}
Sample input/output:

```txt

Create two fields at runtime:
  Field #1:
       Enter name  : a
       Enter value : rosetta

  Field #2:
       Enter name  : b
       Enter value : 64

Which field do you want to inspect ? a
Its value is 'rosetta'

```



## Groovy


Any [[Groovy]] class that implements "''Object get(String)''" and "''void set(String, Object)''" will have the '''apparent''' capability to add new properties. However, this capability will only work as expected with an appropriate implementation, backed by a Map object or something very much like a Map.

```groovy
class A {
    final x = { it + 25 }
    private map = new HashMap()
    Object get(String key) { map[key] }
    void set(String key, Object value) { map[key] = value }
}
```


Test:

```groovy
def a = new A()
a.y = 55
a.z = { println (new Date()); Thread.sleep 5000 }

println a.x(25)
println a.y
(0..2).each(a.z)

println a.q
```


Output:

```txt
50
55
Wed Feb 23 21:33:40 CST 2011
Wed Feb 23 21:33:45 CST 2011
Wed Feb 23 21:33:50 CST 2011
null
```



## Io


All "instance variables" (or slots in Io nomenclature) are created at runtime.


```io
Empty := Object clone

e := Empty clone
e foo := 1
```


=={{header|Icon}} and {{header|Unicon}}==
{{omit from|Icon}}
Unicon implements object environments with records and supporting procedures for creation, initialization, and methods. To modify an instance you must create a new record then copy, amend, and replace it.  Strictly speaking we can't guarantee the replace as there is no way to modify the existing object and we are creating a new instance with extensions.  The procedures ''constructor'' and ''fieldnames'' are needed.  This example doesn't do error checking. Here ''extend'' takes three arguments, the class instance, a list of new variable names as strings, and an optional list of new values to be assigned.  The new instance is returned and the object is replaced by assignment.  The caveat here is that if the object was assigned to anything else we will now have two objects floating around with possible side effects.  As written this isn't safe from name collisions - aside from local declarations the use of a fixed constructor name uses the global name space.  There is a final caveat that needs to be observed - if future implementations of objects change then this could easily stop working.

''Note:'' Unicon can be translated via a command line switch into icon which allows for classes to be shared with Icon code (assuming no other incompatibilities exist).

```unicon

link ximage

procedure main()
   c1 := foo(1,2)                            # instance of foo
   write("c1:\n",ximage(c1))
   c1 := extend(c1,["c","d"],[8,9])          # 2 new fields
   write("new c1:\n",ximage(c1))
   c1 := extend(c1,["e"],[7])                # 1 more
   write("newest c1:\n",ximage(c1))
end

class foo(a,b)                               # dummy class
end

procedure extend(instance,newvars,newvals)   #: extend a class instance
   every put(f := [],fieldnames(instance))   # copy existing fieldnames
   c := ["tempconstructor"] ||| f            # new constructor
   every put(c,!newvars)                     # append new vars
   t := constructor!c                        # new constructor
   x := t()                                  # new instance
   every x[v := !f] := instance[v]           # same as old instance
   x.__s := x                                # new self
   if \newvals then
      every i := 1 to min(*newvars,*newvals) do
         x[newvars[i]] := newvals[i]         # add new vars = values
   return x
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/ximage.icn ximage.icn provides ximage to dump variable contents]

Output:
```txt
c1:
R_foo__state_1 := foo__state()
   R_foo__state_1.a := 1
   R_foo__state_1.b := 2
new c1:
R_tempconstructor_1 := tempconstructor()
   R_tempconstructor_1.__s := R_tempconstructor_1
   R_tempconstructor_1.__m := R_foo__methods_1 := foo__methods()
   R_tempconstructor_1.a := 1
   R_tempconstructor_1.b := 2
   R_tempconstructor_1.c := 8
   R_tempconstructor_1.d := 9
newest c1:
R_tempconstructor_1 := tempconstructor()
   R_tempconstructor_1.__s := R_tempconstructor_1
   R_tempconstructor_1.__m := R_foo__methods_1 := foo__methods()
   R_tempconstructor_1.a := 1
   R_tempconstructor_1.b := 2
   R_tempconstructor_1.c := 8
   R_tempconstructor_1.d := 9
   R_tempconstructor_1.e := 7
```



## J

If you assign a value to the name which references a property of a class instance, that name within that instance gets that value.


```j
   C=:<'exampleclass'         NB. this will be our class name
   V__C=: 0                   NB. ensure the class exists
   OBJ1=:conew 'exampleclass' NB. create an instance of our class
   OBJ2=:conew 'exampleclass' NB. create another instance
   V__OBJ1,V__OBJ2            NB. both of our instances exist
0
   W__OBJ1                    NB. instance does not have a W
|value error
   W__OBJ1=: 0                NB. here, we add a W to this instance
   W__OBJ1                    NB. this instance now has a W
0
   W__OBJ2                    NB. our other instance does not
|value error
```



## JavaScript

This kind of thing is fundamental to JavaScript, as it's a prototype-based language rather than a class-based one.

```javascript
e = {}       // generic object
e.foo = 1
e["bar"] = 2    // name specified at runtime
```



## jq

jq's "+" operator can be used to add a key/value pair (or to add multiple key-value pairs) to an existing object at runtime, but
jq is a functional programming language, and objects themselves cannot be altered.  Thus it may be helpful to introduce a variable, since the value of a variable can in effect be updated.  For example:

```jq
{"a":1} as $a | ($a + {"b":2}) as $a | $a

```
Thus the value of $a has undergone the desired transition, that is, its final value is {"a":1, "b":2}.

A Javascript-like syntax can also be used to add (or update) a key, for example:
```jq
$a|.c = 3
# or equivalently:
$a|.["c"] = 3
```



## Julia

Julia does not allow direct modification of the data member variables of a type,
though class methods (just Julia functions) can be added without difficulty.
For special situations, such as when parsing an input file, where new data type names
may be appropriate, this can be accommodated using a Dict as one of the class variables.
For example, consider the below JSON input data for a program processing phone numbers,
where the type of phone numbers for the person is unknown until run-time:

```xml

{"phoneNumbers": [
    {
      "type": "home",
      "number": "212 555-1234"
    },
    {
      "type": "office",
      "number": "646 555-4567"
    },
    {
      "type": "mobile",
      "number": "123 456-7890"
    }]}

```

Add the data into a class member that is declared as a Dict structure:

```Julia

mutable struct Contact
    name::String
    phonenumber::Dict{Any,Any}
end

person = Contact("Jane Doe", Dict())
person.phonenumber["home"] = "212 555-1234"

```



## Kotlin

Adding variables to an object at runtime is not possible in Kotlin (at least in the version targeting the JVM) which is a statically typed language and therefore the names of all class variables need to be known at compile time.

However, as in the case of Groovy, we can ''make it appear'' as though variables are being added at runtime by using a Map or similar structure. For example:


```scala
// version 1.1.2

class SomeClass {
    val runtimeVariables = mutableMapOf<String, Any>()
}

fun main(args: Array<String>) {
    val sc = SomeClass()
    println("Create two variables at runtime: ")
    for (i in 1..2) {
        println("  Variable #$i:")
        print("       Enter name  : ")
        val name = readLine()!!
        print("       Enter value : ")
        val value = readLine()!!
        sc.runtimeVariables.put(name, value)
        println()
    }
    while (true) {
        print("Which variable do you want to inspect ? ")
        val name = readLine()!!
        val value = sc.runtimeVariables[name]
        if (value == null) {
            println("There is no variable of that name, try again")
        } else {
            println("Its value is '${sc.runtimeVariables[name]}'")
            return
        }
    }
}
```


{{out}}
Sample input/output:

```txt

Create two variables at runtime:
  Variable #1:
       Enter name  : a
       Enter value : rosetta

  Variable #2:
       Enter name  : b
       Enter value : 64

Which variable do you want to inspect ? a
Its value is 'rosetta'

```



## Latitude


Latitude is prototype-oriented, so adding slots at runtime is very straightforward and common.

```Latitude
myObject := Object clone.

;; Name known at compile-time.
myObject foo := "bar".

;; Name known at runtime.
myObject slot 'foo = "bar".
```



## Lingo


```Lingo
obj = script("MyClass").new()

put obj.foo
-- "FOO"

-- add new property 'bar'
obj.setProp(#bar, "BAR")
put obj.bar
-- "BAR"
```



## LOLCODE

<tt>BUKKIT</tt>s (the all-purpose container type) can be added to at any point during execution, and the <tt>SRS</tt> operator permits the creation of identifiers from strings. This program and its output demonstrate both by prompting the user for a name and a value, modifying the object accordingly, and then printing the value of the new variable.

```LOLCODE
HAI 1.3

I HAS A object ITZ A BUKKIT
I HAS A name, I HAS A value

IM IN YR interface
    VISIBLE "R U WANTIN 2 (A)DD A VAR OR (P)RINT 1? "!
    I HAS A option, GIMMEH option

    option, WTF?
    OMG "A"
        VISIBLE "NAME: "!, GIMMEH name
        VISIBLE "VALUE: "!, GIMMEH value
        object HAS A SRS name ITZ value, GTFO
    OMG "P"
        VISIBLE "NAME: "!, GIMMEH name
        VISIBLE object'Z SRS name
    OIC
IM OUTTA YR interface

KTHXBYE
```

Example run:

```txt
R U WANTIN 2 (A)DD A VAR OR (P)RINT 1? A
NAME: foo
VALUE: 42
R U WANTIN 2 (A)DD A VAR OR (P)RINT 1? P
NAME: foo
42
```




## Logtalk

Logtalk supports "hot patching" (aka "monkey patching") using a category, which is a first-class entity and a fine grained units of code reuse that can be (virtally) imported by any number of objects but also used for "complementing" an existing object, adding new functionality or patching existing functionality. Complementing cateogries can be enable or disabled globally or on a per object basis.

The following example uses a prototype for simplicity.


```logtalk

% we start by defining an empty object
:- object(foo).

    % ensure that complementing categories are allowed
	:- set_logtalk_flag(complements, allow).

:- end_object.

% define a complementing category, adding a new predicate
:- category(bar,
    complements(foo)).

	:- public(bar/1).
	bar(1).
	bar(2).
	bar(3).

:- end_category.

```


We can test our example by compiling and loading the two entities above and then querying the object:


```logtalk

| ?- foo::bar(X).
X = 1 ;
X = 2 ;
X = 3
true

```



## Lua


```lua
empty = {}
empty.foo = 1
```



## M2000 Interpreter

Adding y member to an object with a x member which made by a class alfa (a global function). We can make m as a copy of this new group (which is in a container, in a(3)). We can make a pointer to A(3) and handle the new member.


```M2000 Interpreter

Module checkit {
      class alfa {
            x=5
      }
      \\ a class is a global function which return a group
      Dim a(5)=alfa()
      Print a(3).x=5
      For a(3) {
            group anyname { y=10}
            \\ merge anyname to this (a(3))
            this=anyname
      }
      Print a(3).y=10
      Print Valid(a(2).y)=false
      \\ make a copy of a(3) to m
      m=a(3)
      m.y*=2
      Print m.y=20, a(3).y=10
      \\ make a pointer to a(3) in n
      n->a(3)
      Print n=>y=10
      n=>y+=20
      Print a(3).y=30
     \\ now n points to a(2)
     n->a(2)
     Print Valid(n=>y)=false  ' y not exist in a(2)
     Print n is a(2)  ' true
     \\ we don't have types for groups
     Print valid(@n as m)=false  ' n haven't all members of m
     Print valid(@m as n)=true  ' m have all members of n
}
checkit

```



## Mathematica

Mathematica doesn't rally have classes, so it doesn't have class variables. However, many rules can be applied to a single tag, so it has some aspects similar to a class. With that definition, adding a class variable is similar to adding a rule:

```Mathematica

f[a]=1;
f[b]=2;
f[a]=3;
? f
```

Output:
 Global`f
 f[a]=3
 f[b]=2

Here, the two 'variables' can be seen under the single heading 'f'. And of course all of this is done at runtime.


## Morfa

To emulate adding a variable to a class instance, Morfa uses user-defined operators <tt>`</tt> and <tt>&lt;-</tt>.

```morfa

import morfa.base;

template <T>
public struct Dynamic
{
    var data: Dict<text, T>;
}

// convenience to create new Dynamic instances
template <T>
public property dynamic(): Dynamic<T>
{
    return Dynamic<T>(new Dict<text,T>());
}

// introduce replacement operator for . - a quoting ` operator
public operator ` { kind = infix, precedence = max, associativity = left, quoting = right }
template <T>
public func `(d: Dynamic<T>, name: text): DynamicElementAccess<T>
{
    return DynamicElementAccess<T>(d, name);
}

// to allow implicit cast from the wrapped instance of T (on access)
template <T>
public func convert(dea: DynamicElementAccess<T>): T
{
    return dea.holder.data[dea.name];
}

// cannot overload assignment - introduce special assignment operator
public operator <- { kind = infix, precedence = assign }
template <T>
public func <-(access: DynamicElementAccess<T>, newEl: T): void
{
    access.holder.data[access.name] = newEl;
}

func main(): void
{
    var test = dynamic<int>;

    test`a <- 10;
    test`b <- 20;
    test`a <- 30;

    println(test`a, test`b);
}

// private helper structure
template <T>
struct DynamicElementAccess
{
    var holder: Dynamic<T>;
    var name: text;

    import morfa.io.format.Formatter;
    public func format(formatt: text, formatter: Formatter): text
    {
        return getFormatFunction(holder.data[name])(formatt, formatter);
    }
}

```

{{out}}

```txt

30 20

```


=={{header|Objective-C}}==
Objective-C doesn't have the ability to add a variable to an instance at runtime. However, since Mac OS X 10.6 and iOS 3.1, it has something that can accomplish a very similar purpose, called "associative references" or "associated objects", which allow you to attach additional objects onto an object without changing its class.

You can put associative references on any object. You can put multiple ones on the same object. They are indexed by a pointer key (typically the address of some dummy variable). You use the functions <code>objc_getAssociatedObject()</code> and <code>objc_setAssociatedObject</code> to get and set them, respectively.


```objc>#import <Foundation/Foundation.h

#import <objc/runtime.h>

static void *fooKey = &fooKey; // one way to define a unique key is a pointer variable that points to itself

int main (int argc, const char *argv[]) {
    @autoreleasepool {

        id e = [[NSObject alloc] init];

        // set
        objc_setAssociatedObject(e, fooKey, @1, OBJC_ASSOCIATION_RETAIN);

        // get
        NSNumber *associatedObject = objc_getAssociatedObject(e, fooKey);
        NSLog(@"associatedObject: %@", associatedObject);

    }
    return 0;
}
```


You can also use a selector as the key, since two selectors with the same content are guaranteed to be equal:

```objc>#import <Foundation/Foundation.h

#import <objc/runtime.h>

int main (int argc, const char *argv[]) {
    @autoreleasepool {

        id e = [[NSObject alloc] init];

        // set
        objc_setAssociatedObject(e, @selector(foo), @1, OBJC_ASSOCIATION_RETAIN);

        // get
        NSNumber *associatedObject = objc_getAssociatedObject(e, @selector(foo));
        NSLog(@"associatedObject: %@", associatedObject);

    }
    return 0;
}
```



## Octave

Octave is dynamically typed, and can have fields added in two methods:


```octave

% Given struct "test"
test.b=1;
test = setfield (test, "c", 3);

```



## ooRexx

ooRexx does not directly expose instance variables to callers.  Encapsulated access to instance variables is done via accesser methods for assignment and retrieval.  In general, it is not possible to just dynamically add support, but it is possible to construct a class that allows for this to happen.

### Unknown Method Access

This example traps unknown method calls, then sets or retrieves the values in an encapsulated directory object.

```ooRexx

d = .dynamicvar~new
d~foo = 123
say d~foo

d2 = .dynamicvar2~new
d~bar = "Fred"
say d~bar

-- a class that allows dynamic variables.  Since this is a mixin, this
-- capability can be added to any class using multiple inheritance
::class dynamicvar MIXINCLASS object
::method init
  expose variables
  variables = .directory~new

-- the UNKNOWN method is invoked for all unknown messages.  We turn this
-- into either an assignment or a retrieval for the desired item
::method unknown
  expose variables
  use strict arg messageName, arguments

  -- assignment messages end with '=', which tells us what to do
  if messageName~right(1) == '=' then do
     variables[messageName~left(messageName~length - 1)] = arguments[1]
  end
  else do
      return variables[messageName]
  end


-- this class is not a direct subclass of dynamicvar, but mixes in the
-- functionality using multiple inheritance
::class dynamicvar2 inherit dynamicvar
::method init
  -- mixin init methods are not automatically invoked, so we must
  -- explicitly invoke this
  self~init:.dynamicvar


```



### Dynamic Method Definitions

An object may be written that can dynamically add methods to itself.  This example is similar to the above example, but the UNKNOWN method attaches a getter/setter pair of methods for the name triggering the UNKNOWN call.  On all subsequent calls, the attribute methods will get called.

```ooRexx

d = .dynamicvar~new
d~foo = 123
say d~foo

-- a class that allows dynamic variables.  Since this is a mixin, this
-- capability can be added to any class using multiple inheritance
::class dynamicvar MIXINCLASS object
::method init
  expose variables
  variables = .directory~new

-- the unknown method will get invoked any time an unknown method is
-- used.  This UNKNOWN method will add attribute methods for the given
-- name that will be available on all subsequent uses.
::method unknown
  expose variables
  use strict arg messageName, arguments

  -- check for an assignment or fetch, and get the proper
  -- method name
  if messageName~right(1) == '=' then do
     variableName = messageName~left(messageName~length - 1)
  end
  else do
     variableName = messageName
  end

  -- define a pair of methods to set and retrieve the instance variable.  These are
  -- created at the object scope
  self~setMethod(variableName, 'expose' variableName'; return' variableName)
  self~setMethod(variableName'=', 'expose' variableName'; use strict arg value;' variableName '= value' )

  -- reinvoke the original message.  This will now go to the dynamically added
  methods
  forward to(self) message(messageName) arguments(arguments)


```



## OxygenBasic

Simple implementation for making runtime members - supports integer, float and string types.

```oxygenbasic

'
### ===========

class fleximembers
'
### ===========


indexbase 0
bstring buf, *varl
sys     dp,en

method addVar(string name,dat)
  sys le=len buf
  if dp+16>le then
    buf+=nuls 0x100 : le+=0x100 :
  end if
  @varl=?buf
  varl[en]=name
  varl[en+1]=dat
  dp+=2*sizeof sys
  en+=2 'next slot
end method

method find(string name) as sys
  sys i
  for i=0 to <en step 2
    if name=varl[i] then return i+1
  next
end method

method vars(string name) as string
  sys f=find(name)
  if f then return varl[f]
end method

method VarF(string name) as double
  return vars(name)
end method

method VarI(string name) as sys
  return vars(name)
end method

method vars(string name,dat)
  bstring varl at buf
  sys f=find(name)
  if f then varl[f]=dat
end method

method delete()
  sys i
  sys v at buf
  for i=0 to <en
     freememory v[i]
  next
  freememory ?buf
  ? buf=0 : en=0 : dp=0
end method

end class

'TEST

fleximembers a

a.addVar "p",5
a.addVar "q",4.5
a.addVar "r","123456"

print a.Vars("q")+a.vars("q") 'result 4.54.5
print a.Varf("q")+a.varf("q") 'result 9

a.delete


```


## Oz

It is not possible to add variables to instances in Oz. Every object has exactly one class and this association cannot be changed after object creation. Classes themselves are immutable.

However, classes are also first-class values and are created at runtime. Many of the tasks that are solved with "monkeypatching" in other languages, can be solved by dynamically creating classes in Oz.


```oz
declare
  %% Creates a new class derived from BaseClass
  %% with an added feature (==public immutable attribute)
  fun {AddFeature BaseClass FeatureName FeatureValue}
     class DerivedClass from BaseClass
        feat
	   %% "FeatureName" is escaped, otherwise a new variable
	   %% refering to a private feature would be created
           !FeatureName:FeatureValue
     end
  in
     DerivedClass
  end

  class Base
     feat
        bar:1

     meth init
        skip
     end
  end

  Derived = {AddFeature Base foo 2}

  Instance = {New Derived init}
in
  {Show Instance.bar} %% inherited feature
  {Show Instance.foo} %% feature of "synthesized" class
```


To add a variable number of features and attributes, you can use [http://www.mozart-oz.org/documentation/base/class.html Class.new].


## Perl

{{works with|Perl|5.x}}

```perl
package Empty;

# Constructor. Object is hash.
sub new { return bless {}, shift; }

package main;

# Object.
my $o = Empty->new;

# Set runtime variable (key => value).
$o->{'foo'} = 1;
```




## Perl 6

{{works with|Rakudo|2015.12}}
You can add variables/methods to a class at runtime by composing in a role. The role only affects that instance, though it is inheritable. An object created from an existing object will inherit any roles composed in with values set to those at the time the role was created. If you want to keep changed values in the new object, clone it instead.

```perl6
class Bar { }             # an empty class

my $object = Bar.new;     # new instance

role a_role {             # role to add a variable: foo,
   has $.foo is rw = 2;   # with an initial value of 2
}

$object does a_role;      # compose in the role

say $object.foo;          # prints: 2
$object.foo = 5;          # change the variable
say $object.foo;          # prints: 5

my $ohno = Bar.new;       # new Bar object
#say $ohno.foo;           # runtime error, base Bar class doesn't have the variable foo

my $this = $object.new;   # instantiate a new Bar derived from $object
say $this.foo;            # prints: 2 - original role value

my $that = $object.clone; # instantiate a new Bar derived from $object copying any variables
say $that.foo;            # 5 - value from the cloned object
```

That's what's going on underneath, but often people just mix in an anonymous role directly using the <tt>but</tt> operator.  Here we'll mix an attribute into a normal integer.

```perl6
my $lue = 42 but role { has $.answer = "Life, the Universe, and Everything" }

say $lue;          # 42
say $lue.answer;   # Life, the Universe, and Everything
```

On the other hand, mixins are frowned upon when it is possible to compose roles directly into classes (as with Smalltalk traits), so that you get method collision detection at compile time.  If you want to change a class at run time, you can also use monkey patching:


```perl6
use MONKEY-TYPING;
augment class Int {
    method answer { "Life, the Universe, and Everything" }
}
say 42.answer;     # Life, the Universe, and Everything
```

This practice, though allowed, is considered to be Evil Action at a Distance.


## PHP


```php
class E {};

$e=new E();

$e->foo=1;

$e->{"foo"} = 1; // using a runtime name
$x = "foo";
$e->$x = 1; // using a runtime name in a variable
```



## PicoLisp

In general, all instance variables in PicoLisp are dynamically created at
runtime.

```PicoLisp
: (setq MyObject (new '(+MyClass)))       # Create some object
-> $385605941
: (put MyObject 'newvar '(some value))    # Set variable
-> (some value)
: (show MyObject)                         # Show the object
$385605941 (+MyClass)
   newvar (some value)
-> $385605941
```



## Pike

Pike does not allow adding variables to existing objects, but we can design a class that allows us to add variables.

```Pike
class CSV
{
    mapping variables = ([]);

    mixed `->(string name)
    {
        return variables[name];
    }

    void `->=(string name, mixed value)
    {
        variables[name] = value;
    }

    array _indices()
    {
        return indices(variables);
    }
}

object csv = CSV();
csv->greeting = "hello world";
csv->count = 1;
csv->lang = "Pike";

indices(csv);
Result: ({ /* 3 elements */
              "lang",
              "count",
              "greeting"
         })

```



## Pop11


In Pop11 instance variables (slots) are specified at class creation
time and there is no way to add new slot to an instance after its
class was created.  However, for most practical purposes one can
obtain desired effect in different way.  Namely, except for a few
low-level routines slots in Pop11 are accessed via getter and
setter methods.  Getters and setters are like ordinary methods,
but are automatically defined and "know" low level details of
slot access.  Pop11 allows dynamic definition of methods, and
one can add new methods which work as "getter" and "setter" but
do not store data directly in instance.  One possibility is
to have one instance variable which contains a hastable (this
is essentially what Perl solution is doing).  Another possibility
(used below) is to create na external hashtable.  Adding new slots
typically make sense if slot name is only known at runtine, so
we create method definition (as a list) at runtime and compile
it using the 'pop11_compile' procedure.


```pop11
lib objectclass;

define :class foo;
enddefine;

define define_named_method(method, class);
    lvars method_str = method >< '';
    lvars class_str = class >< '';
    lvars method_hash_str = 'hash_' >< length(class_str) >< '_'
                              >< class_str >< '_' >< length(method_str)
                              >< '_' >< method_str;
    lvars method_hash = consword(method_hash_str);
    pop11_compile([
        lvars ^method_hash = newassoc([]);
        define :method ^method(self : ^class);
            ^method_hash(self);
        enddefine;
        define :method updaterof ^method(val, self : ^class);
            val -> ^method_hash(self);
        enddefine;
    ]);
enddefine;

define_named_method("met1", "foo");
lvars bar = consfoo();
met1(bar) =>  ;;; default value -- false
"baz" -> met1(bar);
met1(bar) =>  ;;; new value
```



## PowerShell

PowerShell allows extending arbitrary object instances at runtime with the <code>Add-Member</code> cmdlet. The following example adds a property ''Title'' to an integer:

```powershell
$x = 42 `
     | Add-Member -PassThru `
        NoteProperty `
        Title `
        "The answer to the question about life, the universe and everything"
```

Now that property can be accessed:

```txt
PS> $x.Title
The answer to the question about life, the universe and everything
```

or reflected:

```txt
PS> $x | Get-Member

   TypeName: System.Int32

Name        MemberType   Definition
----        ----------   ----------
CompareTo   Method       int CompareTo(System.Object value), ...
Equals      Method       bool Equals(System.Object obj), bool...
GetHashCode Method       int GetHashCode()
GetType     Method       type GetType()
GetTypeCode Method       System.TypeCode GetTypeCode()
ToString    Method       string ToString(), string ToString(s...
Title       NoteProperty System.String Title=The answer to th...
```

While trying to access the same property in another instance will fail:

```txt
PS> $y = 42
PS> $y.Title
```

(which simply causes no output).


## Python



```python
class empty(object):
    pass
e = empty()
```


If the variable (attribute) name is known at "compile" time (hard-coded):


```python>   e.foo = 1</lang


If the variable name is determined at runtime:

```python
   setattr(e, name, value)
```


'''Note:''' Somewhat counter-intuitively one cannot simply use ''e = object(); e.foo = 1'' because the Python base ''object'' (the ultimate ancestor to all new-style classes) will raise attribute exceptions.  However, any normal derivatives of ''object'' can be "monkey patched" at will.

Because functions are first class objects in Python one can not only add variables to instances.  One can add or replace functionality to an instance.  Doing so is tricky if one wishes to refer back to other instance attributes since there's no "magic" binding back to "self."  One trick is to dynamically define the function to be added, nested within the function that applies the patch like so:


```python
class empty(object):
    def __init__(this):
        this.foo = "whatever"

def patch_empty(obj):
    def fn(self=obj):
        print self.foo
    obj.print_output = fn

e = empty()
patch_empty(e)
e.print_output()
# >>> whatever
```

:Note: The name ''self'' is not special; it's merely the pervasive Python convention.  In this example I've deliberately used ''this'' in the class definition to underscore this fact.  The nested definition could use any name for the "self" object.  Because it's nested the value of the object is evaluated at the time that the patch_empty() function is run and thus the function being patched in has a valid reference to the object into which it is being inserted.  Other arguments could be passed as necessary.  Such techniques are not recommended; however they are possible.


## REBOL


```rebol

REBOL [
	Title: "Add Variables to Class at Runtime"
	URL: http://rosettacode.org/wiki/Adding_variables_to_a_class_instance_at_runtime
]

; As I understand it, a REBOL object can only ever have whatever
; properties it was born with. However, this is somewhat offset by the
; fact that every instance can serve as a prototype for a new object
; that also has the new parameter you want to add.

; Here I create an empty instance of the base object (x), then add the
; new instance variable while creating a new object prototyped from
; x. I assign the new object to x, et voila', a dynamically added
; variable.

x: make object! [] ; Empty object.

x: make x [
	newvar: "forty-two" ; New property.
]

print "Empty object modifed with 'newvar' property:"
probe x

; A slightly more interesting example:

starfighter: make object! [
	model: "unknown"
	pilot: none
]
x-wing: make starfighter [
	model: "Incom T-65 X-wing"
]

squadron: reduce [
	make x-wing [pilot: "Luke Skywalker"]
	make x-wing [pilot: "Wedge Antilles"]
	make starfighter [
		model: "Slayn & Korpil B-wing"
		pilot: "General Salm"
	]
]

; Adding new property here.
squadron/1: make squadron/1 [deathstar-removal-expert: yes]

print [crlf "Fighter squadron:"]
foreach pilot squadron [probe pilot]

```



## Red


```Red
person: make object! [
  name: none
  age:  none
]

people: reduce [make person [name: "fred" age: 20] make person [name: "paul" age: 21]]
people/1: make people/1 [skill: "fishing"]

foreach person people [
  print reduce [person/age "year old" person/name "is good at" any [select person 'skill "nothing"]]
]
```



## Ring

We can add an attribute (or a group of attributes) to the object state using addattribute() function

```ring
o1 = new point
addattribute(o1,"x")
addattribute(o1,"y")
addattribute(o1,"z")
see o1 {x=10 y=20 z=30}
class point
```

{{out}}

```txt

x: 10.000000
y: 20.000000
z: 30.000000

```



## Ruby


```ruby
class Empty
end

e = Empty.new
class << e
  attr_accessor :foo
end
e.foo = 1
puts e.foo  # output: "1"

f = Empty.new
f.foo = 1   # raises NoMethodError

```


"class << e" uses the ''singleton class'' of "e", which is an automatic subclass of Empty that has only this single instance. Therefore we added the "foo" accessor only to "e", not to other instances of Empty.
Another way of adding a method to a singleton is:

```ruby
yes_no = "Yes"

def yes_no.not
  replace( self=="Yes" ? "No": "Yes")
end

#Demo:
p yes_no.not # => "No"
p yes_no.not # => "Yes"
p "aaa".not  # => undefined method `not' for "aaa":String (NoMethodError)
```



## Scala

{{works with|Scala|2.10}}
Since version 2.10 Scala supports dynamic types. Dynamic types have to implement trait ''Dynamic'' and implement methods ''selectDynamic'' and ''updateDynamic''.


```scala
import language.dynamics
import scala.collection.mutable.HashMap

class A extends Dynamic {
  private val map = new HashMap[String, Any]
  def selectDynamic(name: String): Any = {
    return map(name)
  }
  def updateDynamic(name:String)(value: Any) = {
    map(name) = value
  }
}
```


Sample output in the REPL:


```scala>scala
 val a = new A
a: A = A@7b20f29d

scala> a.foo = 42
a.foo: Any = 42

scala> a.foo
res10: Any = 42
```



## Sidef


```ruby
class Empty{};
var e = Empty();    # create a new class instance
e{:foo} = 42;       # add variable 'foo'
say e{:foo};        # print the value of 'foo'
```



## Slate

Slate objects are prototypes:

```slate
define: #Empty -> Cloneable clone.
define: #e -> Empty clone.
e addSlotNamed: #foo valued: 1.
```



## Smalltalk

the following addSlot function creates an anonymus class with the additional slot, defines accessor methods and clones a new instance from the given object which becomes the old one.
This preserves object identity. (by the way: if we remember and reuse these temp classes, we get the core of Google's fast JavaScript interpreter implementation ;-)
{{works with|Smalltalk/X}} (should work with all Smalltalks, though)

```smalltalk
|addSlot p|

addSlot :=
  [:obj :slotName |
    |anonCls newObj|
    anonCls := obj class
            subclass:(obj class name,'+') asSymbol
            instanceVariableNames:slotName
            classVariableNames:''
            poolDictionaries:'' category:nil
            inEnvironment:nil.
    anonCls compile:('%1 ^  %1' bindWith:slotName).
    anonCls compile:('%1:v %1 := v' bindWith:slotName).
    newObj := anonCls cloneFrom:obj.
    obj become:newObj.
  ].
```

create a 2D Point object, add a z slot, change and retrieve the z-value, finally inspect it (and see the slots).

```smalltalk
p := Point x:10 y:20.
addSlot value:p value:'z'.
p z:30.
p z.
p z:40.
p inspect
```


The above used a block to perform this operation in privacy. In a real world application, the addSlot code would be added as an extension to the Object class, as in.

```smalltalk
!Object methodsFor:'adding slots'!

addSlot: slotName
    |anonCls newObj|

    anonCls := self class
            subclass:(self class name,'+') asSymbol
            instanceVariableNames:slotName
            classVariableNames:''
            poolDictionaries:'' category:nil
            inEnvironment:nil.
    anonCls compile:('%1 ^  %1' bindWith:slotName).
    anonCls compile:('%1:v %1 := v' bindWith:slotName).
    newObj := anonCls cloneFrom:self.
    self become:newObj.
```


then, again create a 2D Point object, add a z slot, change and retrieve the z-value, finally inspect it (and see the slots).

```smalltalk
p := Point x:10 y:20.
p addSlot:'z'.
p z:30.
p z.
p z:40.
p inspect
```



{{incorrect|Smalltalk|It extends the class (adds a new instance var and new method that will exists even in brand new future instances of that class), not only the particular instance. -- The description of the problem must be reworded then, as it
asks for adding variables to the class, not the instance.

CG: no, you should read the code carefully: the original class remains unchanged, meaning that future instances of the class ("Point" in the above example) are not affected. Only the one affected instance gets a new class (which is anonymous, as it gets installed into the environment "nil", which means: "nowhere"). Actually, there is no way to get more new instances of that new instance's class because the class of it is only referenced by the changed p. (well, it could be done by using reflection, as with "p class new"). So, the above EXACTLY does what was asked for, whereas the example below is wrongly changing the original class.  }}

```smalltalk
Object subclass: #Monkey
  instanceVariableNames: 'aVar'
  classVariableNames: ''
  poolDictionaries: ''
  category: nil !

!Monkey class methodsFor: 'new instance'!
new
  | o |
  o := super new.
  o init.
  ^o
!!

!Monkey methodsFor: 'init instance'!
init
  aVar := 0
!
initWith: value
  aVar := value
!!

!Monkey methodsFor: 'set/get the inst var(s)'!
setVar: var
  aVar := var
!
getVar
  ^aVar
!!


"Create a new instance"
Smalltalk at: #aMonkey put: (Monkey new) !

"set the 'original' instance var to 12"
aMonkey setVar: 12 .

"let's see what's inside"
aMonkey inspect .

"add a new instance var"
Monkey addInstVarName: 'x'.

"let's see what's inside now"
aMonkey inspect .

"let us create a new method for x"
!Monkey methodsFor: 'about x'!
setX: val
   x := val
!
x
  ^x
!!

aMonkey setX: 10 .
aMonkey inspect .
(aMonkey x) printNl .
```


Output is:

```txt
An instance of Monkey
  aVar: 12
An instance of Monkey
  aVar: 12
  x: nil
An instance of Monkey
  aVar: 12
  x: 10
10
```



## Swift

We can use the same associated object mechanism as in Objective-C:


```swift
import Foundation

let fooKey = UnsafeMutablePointer<UInt8>.alloc(1)

class MyClass { }
let e = MyClass()

// set
objc_setAssociatedObject(e, fooKey, 1, .OBJC_ASSOCIATION_RETAIN)

// get
if let associatedObject = objc_getAssociatedObject(e, fooKey) {
  print("associated object: \(associatedObject)")
} else {
  print("no associated object")
}
```



## Tcl

{{works with|Tcl|8.6}} or {{libheader|TclOO}}

The code below uses the fact that each object is implemented as a namespace, to add a ''time'' variable to an instance of ''summation'':

```Tcl
% package require TclOO
% oo::class create summation {
   constructor {} {
       variable v 0
   }
   method add x {
       variable v
       incr v $x
   }
   method value {{var v}} {
       variable $var
       return [set $var]
   }
   destructor {
       variable v
       puts "Ended with value $v"
   }
}
::summation
% set s [summation new]
% # Do the monkey patch!
% set [info object namespace $s]::time now
now
% # Prove it's really part of the object...
% $s value time
now
%
```

An alternative approach is to expose the (normally hidden) <code>varname</code> method on the object so that you can get a handle for an arbitrary variable in the object.

```tcl
% oo::class create summation {
   constructor {} {
       variable v 0
   }
   method add x {
       variable v
       incr v $x
   }
   method value {{var v}} {
       variable $var
       return [set $var]
   }
   destructor {
       variable v
       puts "Ended with value $v"
   }
}
::summation
% set s [summation new]
% set s2 [summation new]
% oo::objdefine $s export varname
% # Do the monkey patch...
% set [$s varname time] "now"
% $s value time
now
% # Show that it is only in one object...
% $s2 value time
can't read "time": no such variable
```



## zkl

Once created, class structure is fixed. However, using reflection, you can blow apart the class structure, add what ever and recompile the class (at run time). The REPL does this to store intermediate user results (defined classes, functions, variables, etc). It is ugly, slow and left as an exercise to the reader who cares.


{{omit from|Applesoft BASIC}}
{{omit from|ALGOL 68}}
{{omit from|AWK}}
{{omit from|Brainfuck}}
{{omit from|C}}
{{omit from|C++}}
{{omit from|Clojure}}
{{omit from|Delphi}}
{{omit from|F Sharp}}
{{omit from|Fortran}}
{{omit from|Free Pascal}}
{{omit from|gnuplot}}
{{omit from|GUISS}}
{{omit from|Haskell}}
{{omit from|Integer BASIC}}
{{omit from|Java}}
{{omit from|LaTeX}}
{{omit from|Lily}}
{{omit from|M4}}
{{omit from|Maxima}}
{{omit from|ML/I}}
{{omit from|Make}}
{{omit from|NetRexx}}
{{omit from|Object Pascal}}
{{omit from|OCaml}}
{{omit from|Octave}}
{{omit from|PARI/GP}}
{{omit from|Pascal}}
{{omit from|PlainTeX}}
{{omit from|PureBasic}}
{{omit from|R}}
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have objects. -->
{{omit from|Retro}}
{{omit from|Rust}}
{{omit from|UNIX Shell}}
{{omit from|ZX Spectrum Basic}} <!-- Does not have objects. -->
