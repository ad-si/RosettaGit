+++
title = "Abstract type"
description = ""
date = 2019-09-03T06:08:33Z
aliases = []
[extra]
id = 3131
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}
[[Category:Object oriented]]
[[Category:Type System]]
'''Abstract type''' is a type without instances or without definition.

For example in [[object-oriented programming]] using some languages, abstract types can be partial implementations of other types, which are to be derived there-from. An abstract type may provide implementation of some operations and/or components. Abstract types without any implementation are called '''interfaces'''. In the languages that do not support multiple [[inheritance]] ([[Ada]], [[Java]]), classes can, nonetheless, inherit from multiple interfaces. The languages with multiple inheritance (like [[C++]]) usually make no distinction between partially implementable abstract types and interfaces. Because the abstract type's implementation is incomplete, [[object-oriented programming | OO]] languages normally prevent instantiation from them  (instantiation must derived from one of their descendant classes).

The term '''abstract datatype''' also may denote a type, with an implementation provided by the programmer rather than directly by the language (a '''built-in''' or an inferred type). Here the word ''abstract'' means that the implementation is abstracted away, irrelevant for the user of the type. Such implementation can and should be hidden if the language supports separation of implementation and specification. This hides complexity while allowing the implementation to change without repercussions on the usage. The corresponding software design practice is said to follow the [[wp:Information_hiding|information hiding principle]].

It is important not to confuse this ''abstractness'' (of implementation) with one of the '''abstract type'''. The latter is abstract in the sense that the set of its values is empty. In the sense of implementation abstracted away, all user-defined types are abstract.

In some languages, like for example in Objective Caml which is strongly statically typed, it is also possible to have '''abstract types''' that are not OO related and are not an abstractness too. These are ''pure abstract types'' without any definition even in the implementation and can be used for example for the type algebra, or for some consistence of the type inference. For example in this area, an abstract type can be used as a phantom type to augment another type as its parameter. <!-- An OCaml Guru would explain this better than me, a poor beginner... -->

'''Task''': show how an abstract type can be declared in the language. If the language makes a distinction between interfaces and partially implemented types illustrate both.



## ABAP


###  Abstract Class 


```ABAP
class abs definition abstract.
  public section.
    methods method1 abstract importing iv_value type f exporting ev_ret type i.
  protected section.
    methods method2 abstract importing iv_name type string exporting ev_ret type i.
    methods add importing iv_a type i iv_b type i exporting ev_ret type i.
endclass.

class abs implementation.
  method add.
    ev_ret = iv_a + iv_b.
  endmethod.
endclass.
```



###  Interfaces 

Interfaces in ABAP are classes with the following restrictions:
1. All methods must be abstract instance methods (Static methods aren't allowed).
2. Variables must be static final. The values may be computed at run time.
3. No static initialiser blockers. No static initialiser helper methods.

```ABAP
interface inter.
  methods: method1 importing iv_value type f exporting ev_ret type i,
           method2 importing iv_name type string exporting ev_ret type i,
           add importing iv_a type i iv_b type i exporting ev_ret type i.
endinterface.
```



## ActionScript

While ActionScript does not support explicit abstract classes, it does have interfaces. Interfaces in ActionScript may not implement any methods and all methods are public and implicitly abstract. Interfaces can extend other interfaces, and interfaces may be multiply inherited.

```actionscript
package
{
    public interface IInterface
    {
        function method1():void;
        function method2(arg1:Array, arg2:Boolean):uint;
    }
}
```


Abstract types can also be simulated using the built-in <code>flash.utils.getQualifiedClassName()</code> function in the constructor to check that the runtime type is an inhertied class, and throwing exceptions from "abstract" methods which can be overridden by inheritors to disable them. If any inheriting class does not implement an abstract method, the error will not be thrown until the non-implemented method is called.

```ActionScript

package {
    import flash.utils.getQualifiedClassName;

    public class AbstractClass {
        
        private static const FULLY_QUALIFIED_NAME:String = "AbstractClass";
        
        // For classes in a package, the fully qualified name should be in the form "package.name::class_name"
        // Note that a double colon and not a dot is used before the class name. This is the format returned
        // by the getQualifiedClassName() function.
        
        public function AbstractClass() {
            if ( getQualifiedClassName(this) == FULLY_QUALIFIED_NAME )
                throw new Error("Class " + FULLY_QUALIFIED_NAME + " is abstract.");
        }
        
        public function abstractMethod(a:int, b:int):void {
            throw new Error("abstractMethod is not implemented.");
        }
        
    }
}

```


Inheriting this class:

```ActionScript

package {

    public class Example extends AbstractClass {

        override public function abstractMethod(a:int, b:int):void {
            trace(a + b);
        }
        
    }
}

```



## Ada


### Interface

Interfaces in [[Ada]] may have no components or implemented operation except for ones implemented as null operations. Interfaces can be multiply inherited.

```ada
type Queue is limited interface;
procedure Enqueue (Lounge : in out Queue; Item : in out Element) is abstract;
procedure Dequeue (Lounge : in out Queue; Item : in out Element) is abstract;
```

Interfaces can be declared synchronized or task when intended implementations are to be provided by protected objects or [[task]]s. For example:

```ada
type Scheduler is task interface;
procedure Plan (Manager : in out Scheduler; Activity : in out Job) is abstract;
```


### Abstract type

Abstract types may provide components and implementation of their operations. Abstract types are singly inherited.

```ada
with Ada.Finalization;
...
type Node is abstract new Ada.Finalization.Limited_Controlled and Queue with record
   Previous : not null access Node'Class := Node'Unchecked_Access;
   Next     : not null access Node'Class := Node'Unchecked_Access;
end record;   
overriding procedure Finalize (X : in out Node); -- Removes the node from its list if any
overriding procedure Dequeue (Lounge : in out Node; Item : in out Element);
overriding procedure Enqueue (Lounge : in out Node; Item : in out Element);
procedure Process (X : in out Node) is abstract; -- To be implemented
```

Here Node is an abstract type that is inherited from Limited_Controlled and implements a node of a [[Doubly-Linked List (element) | doubly linked list]]. It also implements the interface of a queue described above, because any node can be considered a head of the queue of linked elements. For the operation Finalize an implementation is provided to ensure that the element of a list is removed from there upon its finalization. The operation itself is inherited from the parent type Limited_Controlled and then overridden. The operations Dequeue and Enqueue of the Queue interface are also implemented.


## Agda

Using [http://wiki.portal.chalmers.se/agda/agda.php?n=ReferenceManual.Records records] for storing the interface methods and [http://wiki.portal.chalmers.se/agda/pmwiki.php?n=ReferenceManual.InstanceArguments instance arguments] (which are [http://wiki.portal.chalmers.se/agda/pmwiki.php?n=ReferenceManual.ModellingTypeClassesWithInstanceArguments similar] to Haskell type classes) for overloading:


```agda
module AbstractInterfaceExample where

open import Function
open import Data.Bool
open import Data.String

-- * One-parameter interface for the type `a' with only one method.

record VoiceInterface (a : Set) : Set where
  constructor voice-interface
  field say-method-of : a → String

open VoiceInterface

-- * An overloaded method.

say : {a : Set} → ⦃ _ : VoiceInterface a ⦄ → a → String
say ⦃ instance ⦄ = say-method-of instance

-- * Some data types.

data Cat : Set where
  cat : Bool → Cat

crazy! = true
plain-cat = false

-- | This cat is crazy?
crazy? : Cat → Bool
crazy? (cat x) = x

-- | A 'plain' dog.
data Dog : Set where
  dog : Dog

-- * Implementation of the interface (and method).

instance-for-cat : VoiceInterface Cat
instance-for-cat = voice-interface case where
  case : Cat → String
  case x with crazy? x
  ... | true = "meeeoooowwwww!!!"
  ... | false = "meow!"

instance-for-dog : VoiceInterface Dog
instance-for-dog = voice-interface $ const "woof!"

-- * and then:
-- 
-- say dog => "woof!"
-- say (cat crazy!) => "meeeoooowwwww!!!"
-- say (cat plain-cat) => "meow!"
-- 
```


There is <code>dog</code> and <code>cat</code> is objects of different types for which the interface method is implemented.


## Aikido

An abstract class contains functions that have no body defined.  You cannot instantiate a class that contains abstract functions.


```aikido
class Abs {
        public function method1...
        public function method2...

}
```

Interfaces in Aikido define a set of functions, operators, classes, interfaces, monitors or threads (but no variables) that must be implemented by a class implementing the interface.

```aikido
interface Inter {
    function isFatal : integer
    function operate (para : integer = 0) 
    operator -> (stream, isout)
}
```



## AmigaE

In AmigaE, abstract methods are supported but interfaces are not.


```amigae

OBJECT fruit
ENDOBJECT

PROC color OF fruit IS EMPTY

OBJECT apple OF fruit
ENDOBJECT

PROC color OF apple IS WriteF('red ')

OBJECT orange OF fruit
ENDOBJECT

PROC color OF orange IS WriteF('orange ')

PROC main()
  DEF a:PTR TO apple,o:PTR TO orange,x:PTR TO fruit
  FORALL({x},[NEW a, NEW o],`x.color())
ENDPROC

```

prints to the console:

red orange



## Apex


```apex

// Interface
public interface PurchaseOrder {
    // All other functionality excluded
    Double discount();
}

// One implementation of the interface for customers
public class CustomerPurchaseOrder implements PurchaseOrder {
    public Double discount() {
        return .05;  // Flat 5% discount
    }
}


// Abstract Class
public abstract class AbstractExampleClass {
	protected abstract Integer abstractMethod();
}

// Complete the abstract class by implementing its abstract method
public class Class1 extends AbstractExampleClass {
	public override Integer abstractMethod() { return 5; }
}

```



## Argile

{{works with|Argile|1.0.0}}

```Argile
use std

(: abstract class :)

class Abs
   text		name
   AbsIface	iface

class AbsIface
   function(Abs)(int)->int	method

let Abs_Iface = Cdata AbsIface@ {.method = nil}

.: new Abs :. -> Abs {let a = new(Abs); a.iface = Abs_Iface; a}

=: <Abs self>.method <int i> := -> int
   (self.iface.method is nil) ? 0 , (call self.iface.method with self i)

(: implementation :)

class Sub <- Abs { int value }

let Sub_Iface = Cdata AbsIface@ {.method = (code of (nil the Sub).method 0)}

.: new Sub (<int value = -1>) :. -> Sub
   let s = new (Sub)
   s.iface = Sub_Iface
   s.value = value
   s

.: <Sub this>.method <int i> :. -> int {this.value + i}

(: example use :)

.:foobar<Abs a>:. {print a.method 12 ; del a}
foobar (new Sub 34)	(: prints 46 :)
foobar (new Sub)	(: prints 11 :)
foobar (new Abs)	(: prints  0 :)
```



{{omit from|ARM Assembly}}


## AutoHotkey

{{works with | AutoHotkey_L}}

```AutoHotkey
color(r, g, b){ 
   static color
   If !color 
      color := Object("base", Object("R", r, "G", g, "B", b 
                                    ,"GetRGB", "Color_GetRGB"))
   return  Object("base", Color) 
} 
Color_GetRGB(clr) {
    return "not implemented"
}

waterColor(r, g, b){ 
   static waterColor
   If !waterColor 
      waterColor := Object("base", color(r, g, b),"GetRGB", "WaterColor_GetRGB")
   return  Object("base", WaterColor) 
} 

WaterColor_GetRGB(clr){
return clr.R << 16 | clr.G << 8 | clr.B
}

test:
blue := color(0, 0, 255)
msgbox % blue.GetRGB() ; displays "not implemented"
blue := waterColor(0, 0, 255)
msgbox % blue.GetRGB() ; displays 255
return

```


## BBC BASIC

{{works with|BBC BASIC for Windows}}
BBC BASIC is a procedural language with no built-in OO features.  The CLASSLIB library implements simple Object Classes with multiple inheritance; an abstract class may be created without any instantiation, the sole purpose of which is for other classes to inherit from it.  At least one member or method must be declared, but no error will be generated if there is no implementation: 

```bbcbasic
      INSTALL @lib$+"CLASSLIB"
      
      REM Declare a class with no implementation:
      DIM abstract{method}
      PROC_class(abstract{})
      
      REM Inherit from the abstract class:
      DIM derived{member%}
      PROC_inherit(derived{}, abstract{})
      PROC_class(derived{})
      
      REM Provide an implementation for the derived class:
      DEF derived.method : PRINT "Hello world!" : ENDPROC
      
      REM Instantiate the derived class:
      PROC_new(instance{}, derived{})
      
      REM Test by calling the method:
      PROC(instance.method)
```



## C

Doing abstract types in C is not particularly trivial as C doesn't really support classes.  The following series will show an abstract type, followed by a realizable class that provides the abstract interface, and finally followed by an example of usage.

The header file for the abstract class, interfaceAbs.h

```c
#ifndef INTERFACE_ABS
#define INTERFACE_ABS

typedef struct sAbstractCls *AbsCls;

typedef struct sAbstractMethods {
    int         (*method1)(AbsCls c, int a);
    const char *(*method2)(AbsCls c, int b);
    void        (*method3)(AbsCls c, double d);
} *AbstractMethods, sAbsMethods;

struct sAbstractCls {
    AbstractMethods  klass;
    void     *instData;
};

#define ABSTRACT_METHODS( cName, m1, m2, m3 ) \
    static sAbsMethods cName ## _Iface = { &m1, &m2, &m3 }; \
    AbsCls cName ## _Instance( void *clInst) { \
        AbsCls ac = malloc(sizeof(struct sAbstractCls)); \
        if (ac) { \
            ac->klass = &cName ## _Iface; \
            ac->instData = clInst; \
        }\
        return ac; }

#define Abs_Method1( c, a) (c)->klass->method1(c, a)
#define Abs_Method2( c, b) (c)->klass->method2(c, b)
#define Abs_Method3( c, d) (c)->klass->method3(c, d)
#define Abs_Free(c) \
  do { if (c) { free((c)->instData); free(c); } } while(0);
 
#endif
```

That will define the abstract class. The next section declares a public interface for a class providing the interface of the abstract class. This class is Silly and
the code is in file silly.h. Note the actual structure of the class is not provided
here. We don't want it visible.

```c
#ifndef SILLY_H
#define SILLY_H
#include "intefaceAbs.h"
#include <stdlib.h>

typedef struct sillyStruct *Silly;
extern Silly NewSilly( double, const char *);
extern AbsCls Silly_Instance(void *); 

#endif
```

Ok. Now it is necessary to provide the implementation of the realizable class.
This code should be in silly.c.

```c
#include "silly.h"
#include <string.h>
#include <stdio.h>

struct sillyStruct {
    double  v1;
    char   str[32];
};

Silly NewSilly(double vInit, const char *strInit)
{
    Silly sily = malloc(sizeof( struct sillyStruct ));
    sily->v1 = vInit;
    sily->str[0] = '\0';
    strncat(sily->str, strInit, 31);
    return sily;
}

static
int MyMethod1(  AbsCls c, int a)
{
    Silly s = (Silly)(c->instData);
    return a+strlen(s->str);
}

static
const char *MyMethod2(AbsCls c, int b)
{
    Silly s = (Silly)(c->instData);
    sprintf(s->str, "%d", b);
    return s->str;
}

static
void  MyMethod3(AbsCls c, double d)
{
    Silly s = (Silly)(c->instData);
    printf("InMyMethod3, %f\n",s->v1 * d);
}

ABSTRACT_METHODS( Silly, MyMethod1, MyMethod2, MyMethod3)
```

That last macro, ABSTRACT_METHODS may need a little explanation. First note that macros do a string substitution of the parameter values into the arguments of the defined macro, with a little hitch. In the macro definition the ' ## ' expression is special. Here cName ## _Iface gets converted to Silly_Iface, as 'Silly' replaces cName. So the macro call declares an instance of the class record, and defines a constructor named Silly_Instance, which takes a Silly structure as an arguments
and uses the class record it previously set up as well.

The methods MyMethod1, MyMethod2, and MyMethod3 are called through the abstract class interface and do not need to be visible outside this file. Hence, they are declared static. 

Now all's left is some example code that uses all this stuff. 

```c>#include <stdio.h

#include "silly.h"

int main()
{
    AbsCls abster = Silly_Instance(NewSilly( 10.1, "Green Tomato"));

    printf("AbsMethod1: %d\n", Abs_Method1(abster, 5));
    printf("AbsMethod2: %s\n", Abs_Method2(abster, 4));
    Abs_Method3(abster, 21.55);
    Abs_Free(abster);
    return 0;
}

```



## C sharp


```csharp
abstract class Class1
{
   public abstract void method1();

   public int method2()
   {
      return 0;
   }
}
```



## C++

You can declare a virtual function to not have an implementation (called "pure virtual function") by the following "<tt>= 0</tt>" syntax after the method declaration. A class containing at least one pure virtual function (or inheriting one and not overriding it) cannot be instantiated.

```cpp
class Abs {
public:
	virtual int method1(double value) = 0;
	virtual int add(int a, int b){
		return a+b;
	}
};
```

Because C++ allows multiple inheritance of classes, no distinction is made between interfaces and abstract classes.

=={{header|Caché ObjectScript}}==

In Caché, abstract and data type classes cannot be instantiated directly - there must be a 'concrete subclass' that extends them as well as the '%RegisteredObject' class in order to instantiate an object, see example below.


```cos
Class Abstract.Class.Shape [ Abstract ]
{
Parameter SHAPE = 1;
Property Name As %String;
Method Description() {}
}

Class Abstract.Class.Square Extends (%RegisteredObject, Shape)
{
Method Description()
{
	Write "SHAPE=", ..#SHAPE, !
	Write ..%ClassName()_$Case(..%Extends(..%PackageName()_".Shape"), 1: " is a ", : " is not a ")_"shape"
}
}
```


Data type classes differ because they cannot contain properties, see example below.


```cos
Class Abstract.DataType.Shape [ ClassType = datatype ]
{
Parameter SHAPE = 1;
Method Description() {}
}

Class Abstract.DataType.Square Extends (%RegisteredObject, Shape)
{
Method Description()
{
	Write "SHAPE=", ..#SHAPE, !
	Write ..%ClassName()_$Case(..%Extends(..%PackageName()_".Shape"), 1: " is a ", : " is not a ")_"shape"
}
}
```


Both class types can contain implementation code.  Caché allows multiple inheritance of classes, so no distinction is made between abstract classes and interfaces.

{{out|Examples}}

```txt

USER>Do ##class(Abstract.Class.Square).%New().Description()
SHAPE=1
Square is a shape

USER>Do ##class(Abstract.DataType.Square).%New().Description()
SHAPE=1
Square is a shape

```



## Clojure


Using defprotocol, we can define what is essentially an interface.


```lisp
(defprotocol Foo (foo [this]))
```



## COBOL


### Interface

{{trans|F#}}

```cobol
       INTERFACE-ID. Shape.
       
       PROCEDURE DIVISION.
       
       METHOD-ID. perimeter.
       DATA DIVISION.
       LINKAGE SECTION.
       01  ret USAGE FLOAT-LONG.
       PROCEDURE DIVISION RETURNING ret.
       END METHOD perimeter.
       
       METHOD-ID. shape-area.
       DATA DIVISION.
       LINKAGE SECTION.
       01  ret USAGE FLOAT-LONG.
       PROCEDURE DIVISION RETURNING ret.
       END METHOD shape-area.
       
       END INTERFACE Shape.
       

       CLASS-ID. Rectangle. 
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           INTERFACE Shape.
       
       OBJECT IMPLEMENTS Shape.
           DATA DIVISION.
           WORKING-STORAGE SECTION.
           01  width  USAGE FLOAT-LONG PROPERTY.
           01  height USAGE FLOAT-LONG PROPERTY.
           
           PROCEDURE DIVISION.
           
           METHOD-ID. perimeter.
           DATA DIVISION.
           LINKAGE SECTION.
           01  ret USAGE FLOAT-LONG.
           PROCEDURE DIVISION RETURNING ret.
               COMPUTE ret = width * 2.0 + height * 2.0
               GOBACK
               .
           END METHOD perimeter.
       
           METHOD-ID. shape-area.
           DATA DIVISION.
           LINKAGE SECTION.
           01  ret USAGE FLOAT-LONG.
           PROCEDURE DIVISION RETURNING ret.
               COMPUTE ret = width * height
               GOBACK
               .
           END METHOD shape-area.
       END OBJECT.
       
       END CLASS Rectangle.
```



## Common Lisp
 

In Common Lisp, classes do not implement methods, but methods specialized for particular kinds of arguments may be defined for generic functions.  Since we can programmatically determine whether methods are defined for a list of arguments, we can simulate a kind of abstract type.  We define an abstract type <code>kons</code> to which an object belongs if methods for <code>kar</code> and <code>kdr</code> are defined for it.  We define a type predicate <code>konsp</code> and a type <code>kons</code> in terms of the type predicate.


```lisp
(defgeneric kar (kons)
  (:documentation "Return the kar of a kons."))

(defgeneric kdr (kons)
  (:documentation "Return the kdr of a kons."))

(defun konsp (object &aux (args (list object)))
  "True if there are applicable methods for kar and kdr on object."
  (not (or (endp (compute-applicable-methods #'kar args))
           (endp (compute-applicable-methods #'kdr args)))))

(deftype kons ()
  '(satisfies konsp))
```


We can make the built-in types <code>cons</code> and <code>integer</code> <code>kons</code>es.  We start with <code>cons</code>, using the obvious definitions.


```lisp
(defmethod kar ((cons cons))
  (car cons))

(defmethod kdr ((cons cons))
  (cdr cons))

(konsp (cons 1 2))       ; => t
(typep (cons 1 2) 'kons) ; => t
(kar (cons 1 2))         ; => 1
(kdr (cons 1 2))         ; => 2
```


For integers, we'll define the <code>kar</code> of <var>n</var> to be <var>1</var> and the <code>kdr</code> of <var>n</var> to be <var>n - 1</var>.  This means that for an integer <var>n</var>, <var>n</var> = <code>(+ (kar <var>n</var>) (kdr <var>n</var>))</code>.


```lisp
(defmethod kar ((n integer))
  1)

(defmethod kdr ((n integer))
  (if (zerop n) nil
    (1- n)))

(konsp 45)         ; => t
(typep 45 'kons)   ; => t
(kar 45)           ; => 1
(kdr 45)           ; => 44
```



## Component Pascal


```oberon2

(* Abstract type *)
Object = POINTER TO ABSTRACT RECORD END;
	
(* Integer inherits Object *)
Integer = POINTER TO RECORD (Object)
	i: INTEGER
END;
(* Point inherits Object *)
Point = POINTER TO RECORD (Object)
	x,y: REAL
END;

```

...

```oberon2

(* Abstract method of Object *)
PROCEDURE (dn: Object) Show*, NEW, ABSTRACT;
	
(* Implementation of the abstract method Show() in class Integer *)
PROCEDURE (i: Integer) Show*;
BEGIN
	StdLog.String("Integer(");StdLog.Int(i.i);StdLog.String(");");StdLog.Ln
END Show;
	
(* Implementation of the abstract method Show() in class Point *)
PROCEDURE (p: Point) Show*;
BEGIN
	StdLog.String("Point(");StdLog.Real(p.x);StdLog.Char(',');
	StdLog.Real(p.y);StdLog.String(");");StdLog.Ln
END Show;

```


For usage see tasks Stacks.


## Crystal


```ruby
abstract class Animal # only abstract class can have abstract methods
  abstract def move
  abstract def think

  # abstract class can have normal fields and methods
  def initialize(@name : String)
  end

  def process
    think
    move
  end
end

# WalkingAnimal still have to be declared abstract because `think` was not implemented
abstract class WalkingAnimal < Animal
  def move
    puts "#{@name} walks"
  end
end

class Human < WalkingAnimal
  property in_car = false

  def move
    if in_car
      puts "#{@name} drives a car"
    else
      super
    end
  end

  def think
    puts "#{@name} thinks"
  end
end

# Animal.new # => can't instantiate abstract class
he = Human.new("Andrew") # ok
he.process
```

Note that "class" can be replaced with "struct" in the above example, because Crystal also supports abstract structs and their inheritance.


## D


```d
import std.stdio;

class Foo {
    // abstract methods can have an implementation for
    // use in super calls.
    abstract void foo() {
        writeln("Test");
    }
}

interface Bar {
    void bar();

    // Final interface methods are allowed.
    final int spam() { return 1; }
}

class Baz : Foo, Bar { // Super class must come first.
    override void foo() {
        writefln("Meep");
        super.foo();
    }

    void bar() {}
}

void main() {}
```



## Delphi

Abstract Class introduced in Delphi 2006.  An abstract class cannot be instantiated and must be derived from in order to be used.


```delphi
TSomeClass = class abstract (TObject)
  ...
end;
```



Abstract Methods can only be implemented in derived classes.  A concrete class that contains abstract methods can be instantiated.  A warning will be generated at compile time, and an EAbstractError exception will thrown if the method is called at run time.


```delphi
type
  TMyObject = class(TObject)
  public
    procedure AbstractFunction; virtual; abstract; // Your virtual abstract function to overwrite in descendant
    procedure ConcreteFunction; virtual; // Concrete function calling the abstract function
  end;

implementation

procedure TMyObject.ConcreteFunction;
begin
  AbstractFunction; // Calling the abstract function
end;
```



## DWScript


DWScript has both abstract classes and abstract methods.

See [[Abstract_type#Delphi|Delphi]].


## E


In E, the implementation of an object is never used to determine type membership (except when dealing with the host platform's objects if it uses such distinctions, such as the JVM), so all types are abstract.

A simple abstract type without enforcement can be created using the interface expression:


```e
interface Foo {
    to bar(a :int, b :int)
}
```


With enforcement, a separate ''stamp'' is created which must be applied to the instances. This is analogous to a Java interface.


```e
interface Foo guards FooStamp {
    to bar(a :int, b :int)
}

def x implements FooStamp {
    to bar(a :int, b :int) {
        return a - b
    }
}
```



## Eiffel



```Eiffel

deferred class 
    AN_ABSTRACT_CLASS

feature

    a_deferred_feature
        -- a feature whose implementation is left to a descendent
        deferred
        end

    an_effective_feature: STRING
        -- deferred (abstract) classes may still include effective features
        do
            Result := "I am implemented!"
        end

end

```


A more expressive view of an Abstract type in Eiffel:

```eiffel

note
	title: "Prototype Person"
	description: "Abstract notion of a {PERSON}."
	synopsis: "[
		Abstract Data Types as represented by any Eiffel class, fully or partially implemented, are
		not just about the attribute and routine features of the class (deferred or implemented).
		The class and each feature may also have specification rules expressed as preconditions,
		post-conditions, and class invariants. Other assertion contracts may be applied to fully
		implemented features as well.
		
		In the example below, while `age' is deferred (i.e. "abstract"), we have coded a rule which
		states that any caller of `age' must only do so after a `birth_date' has been defined and
		attached to that feature. Failing to do so will cause a contract violation. Moreover, the
		class invariant makes two strong assertions that must always hold for any implemented version
		of {PERSON}: The `birth_date' (if attached--that is--not Void or null) must be in the past
		and never in the future. Also, if "Years" are used to represent the age, the calculation of
		`age' must always agree with "current year - birth year = age".
		
		This form of Abstract Data Type specification has very clear advantages in that not only
		must client code or descendents conform statically, implementing what is deferred, but they
		must also obey the rules of the assertions dynamically in a polymorphic run-time situation.
		]"

deferred class
	PERSON

feature -- Access

	first_name,
	last_name,
	middle_name,
	suffix: STRING

	birth_date: detachable DATE
			-- Date-of-Birth for Current {PERSON}.
		deferred
		end

feature -- Basic Operations

	age: NATURAL_64
			-- Age of Current {PERSON} in some undefined units.
		require
			has_birth_date: attached birth_date
		deferred
		end

	age_units: STRING
			-- Unit-of-Measure (UOM) of `age'.
		attribute
			Result := year_unit_string
		end

	year_unit_string: STRING = "Years"

invariant
	not_future: attached birth_date as al_birth_date implies al_birth_date < (create {DATE}.make_now)
	accurate_age: attached birth_date as al_birth_date and then age > 0 and then age_units.same_string (year_unit_string)
					implies ((create {DATE}.make_now).year - al_birth_date.year) = age

end

```



## Fantom


```fantom

abstract class X
{
  Void method1 ()
  {
    echo ("Method 1 in X")
  }

  abstract Void method2 ()
}

class Y : X
{
  // Y must override the abstract method in X
  override Void method2 ()
  {
    echo ("Method 2 in Y")
  }
}

class Main
{
  public static Void main () 
  {
    y := Y()
    y.method1
    y.method2
  }
}

```


## Forth

{{works with|4tH|3.61.5}}
{{trans|Fantom}}
There are numerous, mutually incompatible object oriented frameworks for Forth. This one works with the FOOS preprocessor extension of [[4tH]].

```forth
include 4pp/lib/foos.4pp

:: X()
   class
     method: method1
     method: method2
   end-class {
     :method { ." Method 1 in X" cr } ; defines method1
   }
;

:: Y()
   extends X()
   end-extends {
     :method { ." Method 2 in Y" cr } ; defines method2
   }
;

: Main
  static Y() y
  y => method1
  y => method2
;

Main
```


Works with any ANS Forth

Needs the FMS-SI (single inheritance) library code located here:
http://soton.mpeforth.com/flag/fms/index.html

```forth
include FMS-SI.f

The FMS object extension uses duck typing and so has no need for abstract types.

```



## Fortran

Simple abstract derived type (i.e. abstract class)  in Fortran 2008 

```fortran
 
   ! abstract derived type
   type, abstract :: TFigure
      real(rdp) :: area
   contains
      ! deferred method i.e. abstract method =  must be overridden in extended type
      procedure(calculate_area), deferred, pass :: calculate_area
   end type TFigure
   ! only declaration of the abstract method/procedure for TFigure type
   abstract interface
      function  calculate_area(this)
         import TFigure !imports TFigure type from host scoping unit and makes it accessible here
         implicit none
         class(TFigure) :: this
         real(rdp) :: calculate_area
      end function calculate_area
   end interface

```



## FreeBASIC

FreeBASIC does not currently support either abstract types or interfaces as such.

However, you can effectively create an abstract type by declaring all its methods to be abstract, so that they do not require a body in the declaring type itself. Such methods can then be overridden and implemented by its derived types. For example :-
 

```freebasic
' FB 1.05.0 Win64

Type Animal Extends Object
  Declare Abstract Sub MakeNoise()
End Type

Type Bear Extends Animal
  name As String
  Declare Constructor(name As String)
  Declare Sub MakeNoise() 
End Type

Constructor Bear(name As String)
  This.name = name
End Constructor

Sub Bear.MakeNoise()
  Print name; " is growling"
End Sub

Type Dog Extends Animal
  name As String
  Declare Constructor(name As String)
  Declare Sub MakeNoise() 
End Type

Constructor Dog(name As String)
  This.name = name
End Constructor

Sub Dog.MakeNoise()
  Print name; " is barking"
End Sub

Dim b As Animal Ptr = New Bear("Bruno")
b -> MakeNoise()
Dim d As Animal Ptr = New Dog("Rover")
d -> MakeNoise()
Delete b
Delete d
Print
Print "Press any key to quit program"
Sleep
```


{{out}}

```txt

Bruno is growling
Rover is barking

```


=={{header|F Sharp|F#}}==
A type with only abstract members and without constructors is an '''interface''' (when not marked with the <code>AbstractClass</code> attribute). Example:

```fsharp
type Shape =
  abstract Perimeter: unit -> float
  abstract Area: unit -> float

type Rectangle(width, height) =
  interface Shape with
    member x.Perimeter() = 2.0 * width + 2.0 * height
    member x.Area() = width * height
```


A type that leaves some or all members unimplemented, is an '''abstract class'''. It has to be marked with the <code>AbstractClass</code> attribute. Example:

``` fsharp>[<AbstractClass
]
type Bird() =
  // an abstract (=virtual) method with default impl.
  abstract Move : unit -> unit
  default x.Move() = printfn "flying"
  // a pure virtual method
  abstract Sing: unit -> string

type Blackbird() =
  inherit Bird()
  override x.Sing() = "tra-la-la"

type Ostrich() =
  inherit Bird()
  override x.Move() = printfn "walking"
  override x.Sing() = "hiss hiss!"
```



## Genyris

In Genyris by default there are no constructors. In effect all classes are Abstract until they are used to tag (describe) an object. This in keeping with the language's roots in Description Logic. To prevent the class ever being associated with an instance it suffices to force the validator to fail. 

```genyris
class AbstractStack()
   def .valid?(object) nil

tag AbstractStack some-object # always fails
```


However this is not much use if we want to use an abstract class to define an '''interface'''. Here is a quasi-abstract class which can be used to tag objects if they conform to the class's membership expectations. In this case it wants two methods, ''.enstack'' and ''.destack'':

```genyris
class StackInterface()
   def .valid?(object)
      object
         and
            bound? .enstack
            is-instance? .enstack Closure
            bound? .destack
            is-instance? .destack Closure
```


So if ever we find an object which conforms to the validator it can be tagged. Here's a 'traditional' class definition using the Object class which ''does'' provide a constructor:

```genyris
class XYZstack(Object)
    def .init()
        var .items ()
    def .enstack(object)
        setq .items (cons object .items)
    def .destack()
        var tmp  (car .items)
        setq .items (cdr .items)
        tmp
```

Now we can tag an object that conforms to the Interface:

```genyris
tag StackInterface (XYZstack(.new))
```



## Go

Go's ''interface type'' is an abstract type. It defines a set of methods that a value must have.

```go
interface {
    Name() string
    SetName(name string)
    Method1(value float64) int
}
```


A variable of an interface type can hold a value of any type that implements the methods that are specified in the interface. You don't need to explicitly "declare" that the type "implements" the interface or anything like that -- the compatibility is purely structural based on the methods.


## Groovy

{{trans|Java}}

As in [[Java]], methods that are declared but not implemented are called "abstract" methods. An interface is a class-level typing construct that can only contain abstract method declarations (well, and constants, but pay no attention to those).

```groovy
public interface Interface {
    int method1(double value)
    int method2(String name)
    int add(int a, int b)
}
```


An abstract class may implement some of its methods and leave others unimplemented. The unimplemented methods and the class itself must be declared "abstract".

```groovy
public abstract class Abstract1 {
    abstract public int methodA(Date value)
    abstract protected int methodB(String name)
    int add(int a, int b) { a + b }
}
```


An abstract class may also be used to partially implement an interface. Here class "Abstract2" implements the "add" method from the inherited "Interface", but leaves the other two methods, "method1" and "method2", unimplemented. Abstract methods that an abstract class inherits from an interface or another abstract class do not have to be redeclared.

```groovy
public abstract class Abstract2 implements Interface {
    int add(int a, int b) { a + b }
}
```


Interfaces and abstract classes cannot be instantiated directly. There must be a "concrete subclass" that contains a complete implementation in order to instantiate an object.

```groovy
public class Concrete1 implements Interface {
    public int method1(double value) { value as int }
    public int method2(String name) { (! name) ? 0 : name.toList().collect { it as char }.sum() }
    public int add(int a, int b) { a + b }
}

public class Concrete2 extends Abstract1 {
    public int methodA(Date value) { value.toCalendar()[Calendar.DAY_OF_YEAR] }
    protected int methodB(String name) { (! name) ? 0 : name.toList().collect { it as char }.sum() }
}

public class Concrete3 extends Abstract2 {
    public int method1(double value) { value as int }
    public int method2(String name) { (! name) ? 0 : name.toList().collect { it as char }.sum() }
}
```


Notice that there are no extra descriptive keywords on the interface method declarations. Interface methods are assumed to be both abstract and public.

Obligatory test:

```groovy
def c1 = new Concrete1()
assert c1 instanceof Interface
println (new Concrete1().method2("Superman"))

def c2 = new Concrete2()
assert c2 instanceof Abstract1
println (new Concrete2().methodB("Spiderman"))

def c3 = new Concrete3()
assert c3 instanceof Interface
assert c3 instanceof Abstract2
println (new Concrete3().method2("Hellboy"))
```


Obligatory test output:

```txt
843
931
719
```


Like [[Java]], [[Groovy]] does not allow subclasses to inherit from multiple superclasses, even abstract superclasses, but it does let subclasses inherit from multiple interfaces.


## Haskell


In Haskell an abstract type is a type class. A type class specifies an interface. One can then define "instances" to provide implementations of the type class for various types.

For example, the built-in type class Eq (the types that can be compared for equality) can be declared as follows:

```haskell
class  Eq a  where
   (==) :: a -> a -> Bool
   (/=) :: a -> a -> Bool
```


Default implementations of the functions can be provided:

```haskell
class  Eq a  where
   (==) :: a -> a -> Bool
   (/=) :: a -> a -> Bool
   x /= y     =  not (x == y)
   x == y     =  not (x /= y)
```

Here default implementations of each of the operators is circularly defined in terms of the other, for convenience of the programmer; so the programmer only needs to implement one of them for it to work.

Consider the following function which uses the operator == of the type class Eq from above. The arguments to == above were of the unknown type "a", which is of class Eq, so the type of the expression below now must include this restriction:

```haskell
func :: (Eq a) => a -> Bool
func x = x == x
```


Suppose I make a new type

```haskell
data Foo = Foo {x :: Integer, str :: String}
```


One could then provide an implementation ("instance") the type class Eq with this type

```haskell
instance Eq Foo where
   (Foo x1 str1) == (Foo x2 str2) =
      (x1 == x2) && (str1 == str2)
```

And now I can, for example, use the function "func" on two arguments of type Foo.

==Icon and {{header|Unicon}}==

Unicon does not distinguish between abstract and concrete classes.
An abstract class is a class with abstract methods.  Icon is not object-oriented.


```unicon
class abstraction()
    abstract method compare(l,r) # generates runerr(700, "method compare()")
end
```



## J

J does not support abstract types, as defined here.  In J, types are typically treated as a necessary evil, which should be minimized, disguised, hidden, neglected or ignored wherever practical.  (2=1+1 regardless of the type of 1 and the type of 2, but 2 and '2' are very different things.)  And allowing user defined types would complicate this approach.

Note also: Types are sometimes thought of as being related to function domains.  But, in the general case, domains of independently defined functions are independent of each other, but nevertheless, the intersections of these domains often enough are not empty.

[In fact, the real motivator for types is the need to allocate finite resources to represent numbers (or whatever else you choose to imagine is being represented). For example: 32 bit integers vs. 64 bit integers vs. 32 bit ieee-854 floating point and 64 bit ieee-854 floating point. Additionally, some operations are sensitive to other details related to these abstractions - the classic examples including overflow vs. carry (add with carry, addition overflow) which depend on the range of numbers involved (2s complement vs. unsigned vs. 1s complement). And then people get carried away trying to "generalize types" rather than "use types" which triggers a need for standardization which mostly means prohibiting some of the most annoying generalizations, which is then followed by other people objecting to those choices... and there is no stopping these trends, which leaves many people fascinated and perhaps horrified at the consequences.]

That said: it's useful to define a type, in the context of J, as "the set of values which may result from a specific parenthesized expression".  And, if compilation to machine code is supported, it may also be useful to define constraint mechanisms to be used in expressions, so that machine code may be more easily generated.

(You can find a variety of languages with rather elaborate implementations of types, but as a general rule those elaborate type systems are either (a) inadequate to represent J arrays, or (b) adequate to represent J arrays but with painfully slow implementations for many typical use cases - especially involving large data sets. That said, you can also find cases where these languages perform well - especially if you tailor the problem to the language or vice versa.)


## Java

Methods that don't have an implementation are called abstract methods in Java. A class that contains an abstract method or inherits one but did not override it must be an abstract class; but an abstract class does not need to contain any abstract methods. An abstract class cannot be instantiated. If a method is abstract, it cannot be private.


```java
public abstract class Abs {
    public abstract int method1(double value);
    protected abstract int method2(String name);
    int add(int a, int b) {
        return a + b;
    }
}
```

Before Java 8, interfaces could not implement any methods and all methods were implicitly public and abstract.

```java
public interface Inter {
    int method1(double value);
    int method2(String name);
    int add(int a, int b);
}
```



## Julia

Abstract types cannot be instantiated, and serve only as nodes in the type graph, thereby describing sets of related concrete types: those concrete types which are their descendants.

Usage:

```julia
abstract type «name» end
abstract type «name» <: «supertype» end
```


Examples:

```julia
abstract type Number end
abstract type Real          <: Number end
abstract type FloatingPoint <: Real end
abstract type Integer       <: Real end
abstract type Signed        <: Integer end
abstract type Unsigned      <: Integer end
```


See more [http://docs.julialang.org/en/latest/manual/types/#abstract-types]


## Kotlin

Kotlin supports abstract classes and interfaces, both of which can contain non-abstract members. The basic difference between them is that interfaces cannot store state. 

Here's a very simple (and silly) example of both:

```scala
// version 1.1

interface Announcer {
    fun announceType()

    // interface can contain non-abstract members but cannot store state
    fun announceName() {
        println("I don't have a name")
    }
}

abstract class Animal: Announcer {
    abstract fun makeNoise()

    // abstract class can contain non-abstract members
    override fun announceType() {
        println("I am an Animal")
    }
}

class Dog(private val name: String) : Animal() {
    override fun makeNoise() {
       println("Woof!")
    }

    override fun announceName() {
       println("I'm called $name")
    }
}

class Cat: Animal() {
    override fun makeNoise() {
       println("Meow!")
    }

    override fun announceType() {
       println("I am a Cat")
    }
}

fun main(args: Array<String>) {
    val d = Dog("Fido")
    with(d) {
        makeNoise()
        announceType()  // inherits Animal's implementation
        announceName()
    }
    println()
    val c = Cat()
    with(c) {
        makeNoise()
        announceType()
        announceName()  // inherits Announcer's implementation
   }
}
```


{{out}}

```txt

Woof!
I am an Aninal
I'm called Fido

Meow!
I am a Cat
I don't have a name

```



## Lasso

Instead of abstract classes or interfaces, Lasso uses a [http://lassoguide.com/language/types.html trait system].

```lasso
define abstract_trait => trait {
    require get(index::integer)
    
    provide first()  => .get(1)
    provide second() => .get(2)
    provide third()  => .get(3)
    provide fourth() => .get(4)
}

define my_type => type {
    parent array
    trait { import abstract_trait }

    public onCreate(...) => ..onCreate(:#rest)
}

local(test) = my_type('a','b','c','d','e')
#test->first  + "\n"
#test->second + "\n"
#test->third  + "\n"
#test->fourth + "\n"
```


{{out}}

```txt
a
b
c
d

```



## Lingo

As weakly typed script language, Lingo does not support any sort of inheritance control at compile time. But you can implement something similar to abstract classes and interfaces as user-defined code, by preventing (and optionally showing error messages) any class instantiations that don't fit to the intended scheme.


###  Abstract Classes 


In some movie script:

```lingo
on extendAbstractClass (instance, abstractClass)
  -- 'raw' instance of abstract class is made parent ("ancestor") of the 
  -- passed instance, i.e. the passed instance extends the abstract class
  instance.setProp(#ancestor, abstractClass.rawNew())
end
```


Parent script "AbstractClass":

```lingo
-- instantiation of abstract class by calling its constructor fails
on new (me)
  -- optional: show error message as alert
  _player.alert("Error:"&&me.script&&" is an abstract class")
  return VOID
end

on ring (me, n)
  repeat with i = 1 to n
    put me.ringtone
  end repeat
end
```


Parent script "MyClass"

```lingo
property ringtone

on new (me)
  extendAbstractClass(me, script("AbstractClass"))
  me.ringtone = "Bell"
  return me
end

on foo (me)
  put "FOO"
end
```


Usage:

```lingo
obj = script("MyClass").new()
obj.ring(3)
-- "Bell"
-- "Bell"
-- "Bell"

-- this fails
test = script("AbstractClass").new()
put test
-- <Void>
```



###  Interfaces 


In some movie script:

```lingo
on implementsInterface (instance, interfaceClass)
  interfaceFuncs = interfaceClass.handlers()
  funcs = instance.handlers()
  repeat with f in interfaceFuncs
    if funcs.getPos(f)=0 then
      -- optional: show error message as alert
      _player.alert("Error:"&&instance.script&&"doesn't implement interface"&&interfaceClass)
      return FALSE
    end if
  end repeat
  return TRUE
end
```


Parent script "InterfaceClass":
(Note: in Lingo closing function definitions with "end" is optional, so this a valid definition of 3 empty functions)

```lingo
on foo
on bar
on foobar
```


Parent script "MyClass":

```lingo
on new (me)
  -- if this class doesn't implement all functions of the 
  -- interface class, instantiation fails
  if not implementsInterface(me, script("InterfaceClass")) then
    return VOID
  end if
  return me
end

on foo (me)
  put "FOO"
end

on bar (me)
  put "BAR"
end

on foobar (me)
  put "FOOBAR"
end
```


Usage:

```lingo
obj = script("MyClass").new()
put obj -- would show <Void> if interface is not fully implemented
-- <offspring "MyClass" 2 171868>
```



## Logtalk

In Logtalk, methods (predicates) must be declared but their definition is not mandatory. Being a logic-based language and making use of the closed-world assumption, invoking a method that is declared but not defined simply fails. If necessary, is trivial to define a method such that it throws an exception. Moreover, Logtalk doesn't define an "abstract" or "virtual" keyword. Instead it uses an operational definition where e.g. a class is considered abstract if it doesn't provide a method for creating new instances.

Logtalk supports the definition of interfaces (protocols), which can contain public, protected, and private declarations of methods (predicates). In addition, an object can qualify an implements relation with an interface (protocol) using the keywords "public", "protected", and "private".

```logtalk

:- protocol(datep).

    :- public(today/3).
    :- public(leap_year/1).
    :- public(name_of_day/3).
    :- public(name_of_month/3).
    :- public(days_in_month/3).

:- end_protocol.

```



## Lua

Lua does not include built-in object oriented paradigms. These features can be added using simple code such as the following:

```lua
BaseClass = {}

function class ( baseClass )
    local new_class = {}
    local class_mt = { __index = new_class }

    function new_class:new()
        local newinst = {}
        setmetatable( newinst, class_mt )
        return newinst
    end

    if not baseClass then baseClass = BaseClass end
        setmetatable( new_class, { __index = baseClass } )

    return new_class
end

function abstractClass ( self )
    local new_class = {}
    local class_mt = { __index = new_class }

    function new_class:new()
        error("Abstract classes cannot be instantiated")
    end

    if not baseClass then baseClass = BaseClass end
        setmetatable( new_class, { __index = baseClass } )

    return new_class
end

BaseClass.class = class
BaseClass.abstractClass = abstractClass
```

The 'class' function produces a new class from an existing parent class (BaseClass is default). From this class other classes or instances can be created. If a class is created through the 'abstractClass' function, however, the resulting class will throw an error if one attempts to instantiate it. Example:

```lua
A = class()         -- New class A inherits BaseClass by default
AA = A:class()      -- New class AA inherits from existing class A
B = abstractClass() -- New abstract class B
BB = B:class()      -- BB is not abstract
A:new()             -- Okay: New class instance
AA:new()            -- Okay: New class instance
B:new()             -- Error: B is abstract
BB:new()            -- Okay: BB is not abstract
```



## M2000 Interpreter

M2000 not use interfaces, but can combine groups (used as objects), and because we can alter definitions, we can make an Abstract group by implement modules and functions with a call to Error "not implement yet"
<lang>Class BaseState {
Private:
      x as double=1212, z1 as currency=1000, k$="ok"
      Module Err {
                  Module "Class.BaseState"
                  Error "not implement yet"
      }      
}
Class AbstractOne {
Public:
      Group z {
            Value {
                  Link parent z1 to z1
                  =z1
            }
      }
      Function M(k as double) {
            .Err
      }
      Module AddCurrency (k as currency) {
            .Err
      }
      Function GetString$ {
            .Err
      } 
Class:
      Module AbstractOne {
                  If Not Match("G") Then Exit
                  Read x
                  \\ combine x with This
                  This=x
      }
}
\\ create new group as K
K=AbstractOne(BaseState())
Try  ok {
      Print K.GetString$()
}
If Not ok Then Print Error$
\\ Now Add final functions/modules
Group k {
      Function Final M(k as double) {
            =.x*k
      }
      Module Final AddCurrency (k as currency) {
            .z1+=k
      }
      Function Final GetString$ {
            =.K$
      }       
}
Print k.M(100), k.GetString$()
K.AddCurrency 50.12
Def ExpType$(x)=Type$(x)
Print k.z=1050.12, ExpType$(k.z), Type$(k.z) ' true, Currency, Group
\\ Now combine AbstractOne without new BaseState
\\ but because all functions are final in k, nothing combined
k=AbstractOne()
Print k.M(100), k.GetString$()
For k {
      \\ we can use For Object {} and a dot before members to get access
      Print .z=1050.12, ExpType$(.z), Type$(.z) ' true, Currency, Group
}

```




## Mathematica


Mathematica is a symbolic language and as such does not support traditional object oriented design patterns. However, it is quite easy to define pseudo-interfaces that depend on an object implementing a set of functions:


```Mathematica

(* Define an interface, Foo, which requires that the functions Foo, Bar, and Baz be defined *)
InterfaceFooQ[obj_] := ValueQ[Foo[obj]] && ValueQ[Bar[obj]] && ValueQ[Baz[obj]];
PrintFoo[obj_] := Print["Object ", obj, " does not implement interface Foo."];
PrintFoo[obj_?InterfaceFooQ] := Print[
   "Foo: ", Foo[obj], "\n",
   "Bar: ", Bar[obj], "\n",
   "Baz: ", Baz[obj], "\n"];

(* Extend all integers with Interface Foo *)
Foo[x_Integer] := Mod[x, 2];
Bar[x_Integer] := Mod[x, 3];
Baz[x_Integer] := Mod[x, 5];

(* Extend a particular string with Interface Foo *)
Foo["Qux"] = "foo";
Bar["Qux"] = "bar";
Baz["Qux"] = "baz";

(* Print a non-interface object *)
PrintFoo[{"Some", "List"}];
(* And for an integer *)
PrintFoo[8];
(* And for the specific string *)
PrintFoo["Qux"];
(* And finally a non-specific string *)
PrintFoo["foobarbaz"]

```

{{out}}

```txt

Object {Some,List} does not implement interface Foo.

Foo: 0
Bar: 2
Baz: 3

Foo: foo
Bar: bar
Baz: baz

Object foobarbaz does not implement interface Foo.

```


Note that, in this implementation, the line between interface and abstract type is blurred. It could be argued that PrintFoo[] is a concrete member of the abstract type InterfaceFoo, or that it's a separate function that accepts anything implementing the interface InterfaceFoo.


## MATLAB

; Abstract Class

```MATLAB
classdef (Abstract) AbsClass 
   ...
end
```

For classes that declare the Abstract class attribute:
* Concrete subclasses must redefine any properties or methods that are declared as abstract.
* The abstract class does not need to define any abstract methods or properties.
When you define any abstract methods or properties, MATLAB® automatically sets the class Abstract attribute to true.
; Abstract Methods

```MATLAB
methods (Abstract)
   abstMethod(obj)
end
```

For methods that declare the Abstract method attribute:
* Do not use a function...end block to define an abstract method, use only the method signature.
* Abstract methods have no implementation in the abstract class.
* Concrete subclasses are not required to support the same number of input and output arguments and do not need to use the same argument names. However, subclasses generally use the same signature when implementing their version of the method.
; Abstract Properties

```MATLAB
properties (Abstract)
   AbsProp
end
```

For properties that declare the Abstract property attribute:
* Concrete subclasses must redefine abstract properties without the Abstract attribute.
* Concrete subclasses must use the same values for the SetAccess and GetAccess attributes as those attributes used in the abstract superclass.
* Abstract properties cannot define access methods and cannot specify initial values. The subclass that defines the concrete property can create access methods and specify initial values.


## Mercury

{{trans|Haskell}}
'Abstract type' in Mercury's parlance is just a type that appears in the
interface of a module but which is only defined in the implementation of the
module, a form of information hiding.

However, in the meaning of this task, Mercury has 'abstract types' in its
typeclasses. For example, the following implements an 'eq' typeclass, a
predicate that works for any instance of this typeclass, and a type that's an
instance of this typeclass.

This module differs from the Haskell in two respects: first, Mercury can't
provide default implementations; and second, this isn't a built-in typeclass.
Mercury lacks Haskell's Eq, Ord, Show, and Read typeclasses, instead having
these features by default for all tpyes.


```Mercury
:- module eq.
:- interface.

:- typeclass eq(T) where [
    pred (T::in) == (T::in) is semidet,
    pred (T::in) \= (T::in) is semidet
].

:- pred f(T::in) is semidet <= eq(T).

:- type foo
    --->    foo(
                x :: int,
                str :: string
            ).

:- instance eq(foo).

:- implementation.

f(X) :- X == X.

:- instance eq(foo) where [
    A == B :- (A^x = B^x, A^str = B^str),
    A \= B :- not A == B
].
```



## Nemerle


```Nemerle
using System.Console;

namespace RosettaCode
{
    abstract class Fruit
    {
        abstract public Eat() : void;
        abstract public Peel() : void;
        
        virtual public Cut() : void      // an abstract class con contain a mixture of abstract and implemented methods
        {                                // the virtual keyword allows the method to be overridden by derivative classes
            WriteLine("Being cut.");
        }
    }
    
    interface IJuiceable
    {
        Juice() : void;           // interfaces contain only the signatures of methods
    }
    
    class Orange : Fruit, IJuiceable
    {
        public override Eat() : void     // implementations of abstract methods need to be marked override
        {
            WriteLine("Being eaten.");
        }
        
        public override Peel() : void
        {
            WriteLine("Being peeled.");
        }
        
        public Juice() : void
        {
            WriteLine("Being juiced.");
        }
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

-- -----------------------------------------------------------------------------
class RCAbstractType public final

method main(args = String[]) public constant

  say '  Testing' RCAbstractType.class.getSimpleName
  say '  Creating an object of type:' Concrete.class.getSimpleName
  conk = Concrete()
  say    'getClassName:'.right(20) conk.getClassName
  say    'getIfaceName:'.right(20) conk.getIfaceName
  say   'mustImplement:'.right(20) conk.mustImplement
  say    'canOverride1:'.right(20) conk.canOverride1
  say    'canOverride2:'.right(20) conk.canOverride2
  say 'callOverridden2:'.right(20) conk.callOverridden2

  return

-- -----------------------------------------------------------------------------
class RCAbstractType.Iface interface

ifaceName = RCAbstractType.Iface.class.getSimpleName

method getIfaceName() public returns String
method canOverride1() public returns String
method canOverride2() public returns String

-- -----------------------------------------------------------------------------
class RCAbstractType.Abstraction abstract implements RCAbstractType.Iface

properties inheritable
  className = String

method Abstraction() public
  setClassName(this.getClass.getSimpleName)
  return

method mustImplement() public abstract returns String

method getClassName() public returns String
  return className

method setClassName(nm = String) public
  className = nm
  return

method getIfaceName() public returns String
  return RCAbstractType.Iface.ifaceName

method canOverride1() public returns String
  return 'In' RCAbstractType.Abstraction.class.getSimpleName'.canOverride1'

method canOverride2() public returns String
  return 'In' RCAbstractType.Abstraction.class.getSimpleName'.canOverride2'

-- -----------------------------------------------------------------------------
class RCAbstractType.Concrete extends RCAbstractType.Abstraction

method Concrete() public
  super()
  return

method mustImplement() public returns String
  return 'In' RCAbstractType.Concrete.class.getSimpleName'.mustImplement'

method canOverride2() public returns String
  return 'In' RCAbstractType.Concrete.class.getSimpleName'.canOverride2'

method callOverridden2() public returns String
  return super.canOverride2

```

;Output

```txt

  Testing RCAbstractType
  Creating an object of type: Concrete
       getClassName: Concrete
       getIfaceName: Iface
      mustImplement: In Concrete.mustImplement
       canOverride1: In Abstraction.canOverride1
       canOverride2: In Concrete.canOverride2
    callOverridden2: In Abstraction.canOverride2

```



## NewLISP



```NewLISP
; file:   abstract.lsp
; url:    http://rosettacode.org/wiki/Abstract_type
; author: oofoe 2012-01-28

; Abstract Shape Class

(new Class 'Shape)     ; Derive new class.

(define (Shape:Shape   ; Shape constructor.
         (pen "X"))    ; Default value.
  (list (context)      ; Assemble data packet.
        (list 'pen pen) 
        (list 'size (args))))

(define (Shape:line x) ; Print out row with 'pen' character.
  (dotimes (i x) 
    (print (lookup 'pen (self)))) 
  (println))

(define (Shape:draw))  ; Placeholder, does nothing.

; Derived Objects

(new Shape 'Box)

(define (Box:draw)     ; Override base draw method.
  (let ((s (lookup 'size (self))))
    (dotimes (i (s 0)) (:line (self) (s 0)))))

(new Shape 'Rectangle)

(define (Rectangle:draw)
  (let ((size (lookup 'size (self))))
    (dotimes (i (size 1)) (:line (self) (size 0)))))

; Demonstration

(:draw (Shape))        ; Nothing happens. 

(println "A box:")
(:draw (Box "O" 5))    ; Create Box object and call draw method.

(println "\nA rectangle:")
(:draw (Rectangle "R" 32 4))

(exit)
```


Sample output:


```txt
A box:
OOOOO
OOOOO
OOOOO
OOOOO
OOOOO

A rectangle:
RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR

```



## Nim

In Nim type classes can be seen as an abstract type. Type classes specify interfaces, which can be instantiated by concrete types.

```nim
type
  Comparable = concept x, y
    (x < y) is bool
  
  Stack[T] = concept s, var v
    s.pop() is T
    v.push(T)
    
    s.len is Ordinal
    
    for value in s:
      value is T
```



## Nit


Source: [https://github.com/nitlang/nit/blob/master/examples/rosettacode/abstract_type.nit the official Nit’s repository]


```nit
# Task: abstract type
#
# Methods without implementation are annotated `abstract`.
#
# Abstract classes and interfaces can contain abstract methods and concrete (i.e. non-abstract) methods.
# Abstract classes can also have attributes.
module abstract_type

interface Inter
	fun method1: Int is abstract
	fun method2: Int do return 1
end

abstract class Abs
	fun method1: Int is abstract
	fun method2: Int do return 1
	var attr: Int
end
```


=={{header|Oberon-2}}==
{{Works with|oo2c Version 2}}

```oberon2

TYPE
  Animal = POINTER TO AnimalDesc;
  AnimalDec = RECORD [ABSTRACT] END;

  (* Cat inherits from Animal *)
  Cat = POINTER TO CatDesc;
  CatDesc = RECORD (AnimalDesc) END;

```



## Objeck


```objeck

class ClassA {
   method : virtual : public : MethodA() ~ Int;
 
   method : public : MethodA() ~ Int {
      return 0;
   }
}

```



## OCaml



### Virtual


The equivalent of what is called abstract type in the other OO examples of this page is just called '''virtual''' in Objective Caml to define '''virtual methods''' and '''virtual classes''':


```ocaml
class virtual foo =
  object
    method virtual bar : int
  end
```



### Abstract Type


In OCaml what we call an abstract type is not OO related, it is only a type defined without definition, for example:

```ocaml>type t</lang

it is used for example to hide an implementation from the interface of a module or for type algebra.

Example of abstracting a type in an interface:

```ocaml
module Foo : sig
  type t
end = struct
  type t = int * int
end
```


Pure abstract types in the implementation:

```ocaml
type u
type v
type 'a t
type ut = u t
type vt = v t
```




## Oforth


Oforth properties implement abstract types.

Classes have only one parent, but can have multiple properties.

Unlike interfaces, properties can include method implementations and attributes (see lang/Comparable.of for instance).


```Oforth
Property new: Spherical(r)
Spherical method: radius  @r ;
Spherical method: setRadius  := r ;
Spherical method: perimeter  @r 2 * PI * ;
Spherical method: surface   @r sq PI * 4 * ;

Object Class new: Ballon(color)
Ballon is: Spherical 
Ballon method: initialize(color, r)  color := color self setRadius(r) ;

Object Class new: Planete(name)
Planete is: Spherical 
Planete method: initialize(n, r)  n := name self setRadius(r) ;
```


Usage : 

```Oforth
: testProperty
| b p |
   Ballon new($red, 0.1) ->b
   System.Out "Ballon radius    is : " << b radius << cr
   System.Out "Ballon perimeter is : " << b perimeter << cr
   System.Out "Ballon surface   is : " << b surface << cr

   Planete new("Earth", 6371000.0) ->p
   System.Out "Earth radius    is : " << p radius << cr
   System.Out "Earth perimeter is : " << p perimeter << cr
   System.Out "Earth surface   is : " << p surface << cr 
;
```


{{out}}

```txt

Ballon radius    is : 0.1
Ballon perimeter is : 0.628318530717959
Ballon surface   is : 0.125663706143592
Earth radius    is : 6371000
Earth perimeter is : 40030173.5920411
Earth surface   is : 510064471909788

```



## ooRexx



### Interface


```ooRexx
  -- Example showing a class that defines an interface in ooRexx
  -- shape is the interface class that defines the methods a shape instance
  -- is expected to implement as abstract methods.  Instances of the shape
  -- class need not directly subclass the interface, but can use multiple
  -- inheritance to mark itself as implementing the interface.

  r=.rectangle~new(5,2)
  say r
  -- check for instance of
  if r~isa(.shape) then say "a" r~name "is a shape"
  say "r~area:" r~area
  say

  c=.circle~new(2)
  say c
  -- check for instance of shape works even if inherited
  if c~isa(.shape) then say "a" c~name "is a shape"
  say "c~area:" c~area
  say

  -- a mixin is still a class and can be instantiated.  The abstract methods
  -- will give an error if invoked
  g=.shape~new
  say g
  say g~name
  say "g~area:" g~area -- invoking abstract method results in a runtime error.

  -- the "MIXINCLASS" tag makes this avaiable for multiple inhertance
  ::class shape MIXINCLASS Object
    ::method area abstract
    ::method name abstract

  -- directly subclassing the the interface
  ::class rectangle subclass shape

    ::method init
      expose length width
      use strict arg length=0, width=0

    ::method area
      expose length width
      return length*width

    ::method name
      return "Rectangle"

  -- inherits the shape methods
  ::class circle subclass object inherit shape

    ::method init
      expose radius
      use strict arg radius=0

    ::method area
      expose radius
      numeric digits 20
      return radius*radius*3.14159265358979323846

    ::method name
      return "Circle"
```

{{out}}

```txt
a RECTANGLE
a Rectangle is a shape
r~area: 10

a CIRCLE
a Circle is a shape
c~area: 12.566370614359172954

a SHAPE
    25 *-* say g~name
Error 93 running E:\ifa.rex line 25:  Incorrect call to method.
Error 93.965:  Method NAME is ABSTRACT and cannot be directly invoked.
```



### Abstract Type


```ooRexx
  -- Example showing an abstract type in ooRexx                         
  -- shape is the abstract class that defines the abstract method area   
  -- which is then implemented by its two subclasses, rectangle and circle
  -- name is the method inherited by the subclasses.                    
  -- author:         Rony G. Flatscher, 2012-05-26
  -- changed/edited: Walter Pachl, 2012-05-28 28 
  -- highlighting:   to come 

  r=.rectangle~new(5,2)
  say r
  say r~name
  say "r~area:" r~area
  say

  c=.circle~new(2)
  say c
  say c~name
  say "c~area:" c~area
  say

  g=.shape~new
  say g
  say g~name
  say "g~area:" g~area -- invoking abstract method results in a runtime error.

  ::class shape
    ::method area abstract
    ::method name
      return "self~class~id:" self~class~id


  ::class rectangle subclass shape

    ::method init
      expose length width
      use strict arg length=0, width=0

    ::method area
      expose length width
      return length*width

  ::class circle subclass shape

    ::method init
      expose radius
      use strict arg radius=0

    ::method area
      expose radius
      numeric digits 20
      return radius*radius*3.14159265358979323846
```

{{out}}

```txt
a RECTANGLE
self~class~id: RECTANGLE
r~area: 10

a CIRCLE
self~class~id: CIRCLE
c~area: 12.566370614359172954

a SHAPE
self~class~id: SHAPE
    24 *-* say "g~area:" g~area -- invoking abstract method results in a runtime error.
Error 93 running E:\aty.rex line 24:  Incorrect call to method.
Error 93.965:  Method AREA is ABSTRACT and cannot be directly invoked.
```



## OxygenBasic


```txt


'ABSTRACT TYPE FOR CONTAINING THINGS

macro ContainerClass(name,body)
  type name##Type body
  class name
  '
  string buffer
  '
  method constructor(sys n=1)
    buffer=nuls n*sizeof name##Type
  end method
  '
  method destructor()
    buffer=""
  end method
  '
  method GetMembers(sys i,n) as name##Type
    sys le=len buffer
    sys en=(n+i)*sizeof name##type
    if le<en
      buffer+=nuls en-le 'auto expand
    end if
    return i+strptr buffer
  end method
  '
end macro


'CREATE CLASS

ContainerClass Vector3dArray, {double x,y,z}
'...
end class


'CREATE OBJECT

new Vector3dArray v(100)


'OBTAIN POINTER AND FILL CHUNK

let pv=v.GetMembers(50,3) 'offset, quantity
pv<=1,2,3, 10,20,30, 100,200,300
'...

'TEST

print pv[3].y

del v


```



## Oz

{{trans|Python}}

There are no abstract types as part of the language, but we can do as in Python and raise exceptions:


```oz
declare
  class BaseQueue
     attr
        contents:nil
      
     meth init
        raise notImplemented(self init) end
     end

     meth enqueue(Item)
        raise notImplemented(self enqueue) end
     end

     meth dequeue(?Item)
        raise notImplemented(self dequeue) end
     end

     meth printContents
        {ForAll @contents Show}
     end
  end

  Queue = {New BaseQueue init} %% throws
```


=={{header|PARI/GP}} ==
GP is not object-oriented and cannot sensibly use abstract types. PARI can use the same solution as [[#C|C]].

=={{header|Pascal}} and {{header|Object Pascal}} ==
In ObjectPascal mode FreePascal has classes and abstract methods.

See [[#Delphi | Delphi]]


## Perl



```perl
package AbstractFoo;

use strict;

sub frob { die "abstract" }
sub baz { die "abstract" }

sub frob_the_baz {
    my $self = shift;
    $self->frob($self->baz());
}


1;
```

Since Perl 5.12, the Yadda Yadda operator (...) dies with an Unimplemented error,


```perl

package AbstractFoo;
 
use strict;
 
sub frob { ... }
sub baz { ... }
 
sub frob_the_baz {
    my $self = shift;
    $self->frob($self->baz());
}

1;

```

Perl 6 inspired roles are provided by the [http://search.cpan.org/perldoc?Moose Moose] library


```perl
package AbstractFoo;

use Moose::Role;

requires qw/frob baz/;

sub frob_the_baz {
    my $self = shift;
    $self->frob($self->baz());
}

1;
```

Roles are also provided in a more lightweight form with [http://search.cpan.org/perldoc?Role::Tiny Role::Tiny] library


```perl
package AbstractFoo;

use Role::Tiny;

requires qw/frob baz/;

sub frob_the_baz {
    my $self = shift;
    $self->frob($self->baz());
}

1;
```



## Perl 6

{{works with|rakudo|2015.12}}

Perl 6 supports roles, which are a bit like interfaces, but unlike interfaces in Java they can also contain some implementation.


```perl6

use v6;

role A {
    # must be filled in by the class it is composed into
    method abstract() { ... };

    # can be overridden in the class, but that's not mandatory
    method concrete() { say '# 42' };
}

class SomeClass does A {
    method abstract() {
        say "# made concrete in class"
    }
}

my $obj = SomeClass.new;
$obj.abstract();
$obj.concrete();

# output:
# made concrete in class
# 42

```



## PHP

The following is for PHP 5.

Methods that don't have an implementation are called abstract methods in PHP. A class that contains an abstract method or inherits one but did not override it must be an abstract class; but an abstract class does not need to contain any abstract methods. An abstract class cannot be instantiated. If a method is abstract, it must be public or protected

```php
abstract class Abs {
	abstract public function method1($value);
	abstract protected function method2($name);
	function add($a, $b){
		return a + b;
	}
}
```

Interfaces in PHP may not implement any methods and all methods are public and implicitly abstract.

```php
interface Inter {
	public function method1($value);
	public function method2($name);
	public function add($a, $b);
}
```



## PicoLisp


```PicoLisp
# In PicoLisp there is no formal difference between abstract and concrete classes.
# There is just a naming convention where abstract classes start with a
# lower-case character after the '+' (the naming convention for classes).
# This tells the programmer that this class has not enough methods
# defined to survive on its own.
   
(class +abstractClass)
   
(dm someMethod> () 
   (foo)
   (bar) )
```




## PowerShell


```PowerShell

#Requires -Version 5.0 

Class Player
{
  <#
    Properties: Name, Team, Position and Number
  #>
    [string]$Name

    [ValidateSet("Baltimore Ravens","Cincinnati Bengals","Cleveland Browns","Pittsburgh Steelers",
                 "Chicago Bears","Detroit Lions","Green Bay Packers","Minnesota Vikings",
                 "Houston Texans","Indianapolis Colts","Jacksonville Jaguars","Tennessee Titans",
                 "Atlanta Falcons","Carolina Panthers","New Orleans Saints","Tampa Bay Buccaneers",
                 "Buffalo Bills","Miami Dolphins","New England Patriots","New York Jets",
                 "Dallas Cowboys","New York Giants","Philadelphia Eagles","Washington Redskins",
                 "Denver Broncos","Kansas City Chiefs","Oakland Raiders","San Diego Chargers",
                 "Arizona Cardinals","Los Angeles Rams","San Francisco 49ers","Seattle Seahawks")]
    [string]$Team

    [ValidateSet("C","G","T","QB","RB","WR","TE","DT","DE","ILB","OLB","CB","S","K","H","LS","P","KOS","R")]
    [string]$Position

    [ValidateRange(0,99)]
    [int]$Number

  <#
    Constructor: Creates a new Player object, with the specified Name, Team, Position and Number.
  #>
    Player([string]$Name, [string]$Team, [string]$Position, [int]$Number)
    {
        $this.Name     = (Get-Culture).TextInfo.ToTitleCase("$Name")
        $this.Team     = (Get-Culture).TextInfo.ToTitleCase("$Team")
        $this.Position = $Position.ToUpper()
        $this.Number   = $Number
    }

  <#
    Methods: Trade the player to a different team (optional parameters for methods in PowerShell 5 classes are not available.  Boo!!)
             An overloaded method is a method with the same name as another method but in a different context,
             in this case with different parameters.
  #>
    Trade([string]$NewTeam)
    {
        [string[]]$league = "Baltimore Ravens","Cincinnati Bengals","Cleveland Browns","Pittsburgh Steelers",
                            "Chicago Bears","Detroit Lions","Green Bay Packers","Minnesota Vikings",
                            "Houston Texans","Indianapolis Colts","Jacksonville Jaguars","Tennessee Titans",
                            "Atlanta Falcons","Carolina Panthers","New Orleans Saints","Tampa Bay Buccaneers",
                            "Buffalo Bills","Miami Dolphins","New England Patriots","New York Jets",
                            "Dallas Cowboys","New York Giants","Philadelphia Eagles","Washington Redskins",
                            "Denver Broncos","Kansas City Chiefs","Oakland Raiders","San Diego Chargers",
                            "Arizona Cardinals","Los Angeles Rams","San Francisco 49ers","Seattle Seahawks"

        if ($NewTeam -in $league | Where-Object {$_ -notmatch $this.Team})
        {
            $this.Team = (Get-Culture).TextInfo.ToTitleCase("$NewTeam")
        }
        else
        {
            throw "Invalid Team"
        }
    }

    Trade([string]$NewTeam, [int]$NewNumber)
    {
        [string[]]$league = "Baltimore Ravens","Cincinnati Bengals","Cleveland Browns","Pittsburgh Steelers",
                            "Chicago Bears","Detroit Lions","Green Bay Packers","Minnesota Vikings",
                            "Houston Texans","Indianapolis Colts","Jacksonville Jaguars","Tennessee Titans",
                            "Atlanta Falcons","Carolina Panthers","New Orleans Saints","Tampa Bay Buccaneers",
                            "Buffalo Bills","Miami Dolphins","New England Patriots","New York Jets",
                            "Dallas Cowboys","New York Giants","Philadelphia Eagles","Washington Redskins",
                            "Denver Broncos","Kansas City Chiefs","Oakland Raiders","San Diego Chargers",
                            "Arizona Cardinals","Los Angeles Rams","San Francisco 49ers","Seattle Seahawks"

        if ($NewTeam -in $league | Where-Object {$_ -notmatch $this.Team})
        {
            $this.Team = (Get-Culture).TextInfo.ToTitleCase("$NewTeam")
        }
        else
        {
            throw "Invalid Team"
        }

        if ($NewNumber -in 0..99)
        {
            $this.Number = $NewNumber
        }
        else
        {
            throw "Invalid Number"
        }
    }
}

```

Create a new player:

```PowerShell

$player1 = [Player]::new("sam bradford", "philadelphia eagles", "qb", 7)
$player1

```

{{Out}}

```txt

Name         Team                Position Number
----         ----                -------- ------
Sam Bradford Philadelphia Eagles QB            7

```

Trade the player:

```PowerShell

$player1.Trade("minnesota vikings", 8)
$player1

```

{{Out}}

```txt

Name         Team                Position Number
----         ----                -------- ------
Sam Bradford Minnesota Vikings   QB            8

```

Create a new player:

```PowerShell

$player2 = [Player]::new("demarco murray", "philadelphia eagles", "rb", 29)
$player2

```

{{Out}}

```txt

Name           Team                Position Number
----           ----                -------- ------
Demarco Murray Philadelphia Eagles RB           29

```

Trade the player:

```PowerShell

$player2.Trade("tennessee titans")
$player2

```

{{Out}}

```txt

Name           Team             Position Number
----           ----             -------- ------
Demarco Murray Tennessee Titans RB           29

```



## Python



```python
class BaseQueue(object):
    """Abstract/Virtual Class 
    """
    def __init__(self):
        self.contents = list()
        raise NotImplementedError
    def Enqueue(self, item):
        raise NotImplementedError
    def Dequeue(self):
        raise NotImplementedError
    def Print_Contents(self):
        for i in self.contents:
            print i,
```


Python allows multiple inheritance and it's more common to implement "mix-in" classes rather than abstract interfaces.  (Mix-in classes can implement functionality as well define interfaces).

In this example we're simply following the Python convention of raising the built-in "NotImplementedError" for each function which must be implemented by our subclasses.  This is a "purely virtual" class because all of its methods raise the exception.  (It is sufficient for __init__ to do so for any partial virtual abstractions since that still ensures that the exception will be raised if anyone attempts to instantiate the base/abstract class directly rather than one of its concrete (fully implemented) descendents).

The method signatures and the instantiation of a "contents" list shown here can be viewed as documentary hints to anyone inheriting from this class.  They won't actually do anything in the derived classes (since these methods must be over-ridden therein).

In this case we've implemented one method (''Print_Contents'').  This would be inherited by any derived classes.  It could be over-ridden, of course.  If it's not over-ridden it establishes a requirement that all derived classes provide some "contents" attribute which must allow for iteration and printing as shown.  Without this method the class would be "purely virtual" or "purely abstract."  With its inclusion the class becomes "partially implemented."

:'''Note:''' This "BaseQueue" example should not be confused with Python's standard library Queue class.  That is used as the principle "producer/consumer" communications mechanism among threads (and newer ''multiprocessing'' processes).

Starting from Python 2.6, abstract classes can be created using the standard abc module:

```python
from abc import ABCMeta, abstractmethod

class BaseQueue():
    """Abstract Class 
    """
    __metaclass__ = ABCMeta

    def __init__(self):
        self.contents = list()

    @abstractmethod
    def Enqueue(self, item):
        pass

    @abstractmethod
    def Dequeue(self):
        pass

    def Print_Contents(self):
        for i in self.contents:
            print i,
```



## Racket



```racket

#lang racket

(define animal-interface (interface () say))

(define cat% (class* object% (animal-interface) (super-new))) ;; error

(define cat% (class* object% (animal-interface)
               (super-new)
               (define/public (say)
                 (display "meeeeew!"))))

(define tom (new cat%))
(send tom say)

```



## REBOL


```REBOL
REBOL [
	Title: "Abstract Type"
	URL: http://rosettacode.org/wiki/Abstract_type
]

; The "shape" class is an abstract class -- it defines the "pen"
; property and "line" method, but "size" and "draw" are undefined and
; unimplemented.

shape: make object! [
	pen:  "X"
	size: none

	line: func [count][loop count [prin self/pen]  prin crlf]
	draw: does [none]
]

; The "box" class inherits from "shape" and provides the missing
; information for drawing boxes.

box: make shape [
	size: 10
	draw: does [loop self/size [line self/size]]
]

; "rectangle" also inherits from "shape", but handles the
; implementation very differently.

rectangle: make shape [
	size: 20x10
	draw: does [loop self/size/y [line self/size/x]]
]

; Unlike some languages discussed, REBOL has absolutely no qualms
; about instantiating an "abstract" class -- that's how I created the
; derived classes of "rectangle" and "box", after all.

s: make shape []  s/draw ; Nothing happens.

print "A box:"
b: make box [pen: "O" size: 5]  b/draw

print [crlf "A rectangle:"]
r: make rectangle [size: 32x5]  r/draw
```



## Red


```Red
Red [
  Title: "Abstract Type"
  Original-Author: oofoe
]

; The "shape" class is an abstract class -- it defines the "pen"
; property and "line" method, but "size" and "draw" are undefined and
; unimplemented.

shape: make object! [
  pen:  "X"
  size: none

  line: func [count][loop count [prin self/pen] prin newline]
  draw: does [none]
]

; The "box" class inherits from "shape" and provides the missing
; information for drawing boxes.

box: make shape [
  size: 10
  draw: does [loop self/size [line self/size]]
]

; "rectangle" also inherits from "shape", but handles the
; implementation very differently.

rectangle: make shape [
  size: 20x10
  draw: does [loop self/size/y [line self/size/x]]
]

; Unlike some languages discussed, REBOL has absolutely no qualms
; about instantiating an "abstract" class -- that's how I created the
; derived classes of "rectangle" and "box", after all.

print "An abstract shape (nothing):"
s: make shape []                s/draw ; Nothing happens.

print [newline "A box:"]
b: make box [pen: "O" size: 5]  b/draw

print [newline "A rectangle:"]
r: make rectangle [size: 32x5]  r/draw
```



## REXX

(This entry modeled after the '''J''' entry.)

(Classic) REXX does not support abstract types   (as defined here on this task page). 

REXX supports a character ''type'', and as such, nothing needs to be declared. 




## Ruby

The Python and Tcl provisos apply to Ruby too.  Nevertheless, a {{libheader|RubyGems}} package called [http://github.com/Peeja/abstraction/tree/master abstraction] exists where:


```ruby
require 'abstraction'

class AbstractQueue
  abstract
  def enqueue(object)
    raise NotImplementedError
  end
  def dequeue
    raise NotImplementedError
  end
end

class ConcreteQueue < AbstractQueue
  def enqueue(object)
    puts "enqueue #{object.inspect}"
  end
end
```

So:

```txt
irb(main):032:0> a = AbstractQueue.new
AbstractClassError: AbstractQueue is an abstract class and cannot be instantiated
        from /usr/lib/ruby/gems/1.8/gems/abstraction-0.0.3/lib/abstraction.rb:10:in `new'
        from (irb):32
        from :0
irb(main):033:0> c = ConcreteQueue.new
=> #<ConcreteQueue:0x7fdea114>
irb(main):034:0> c.enqueue('foo')
enqueue "foo"
=> nil
irb(main):040:0> c.dequeue
NotImplementedError: NotImplementedError
        from (irb):37:in `dequeue'
        from (irb):40
        from :0

```



## Rust

Rust doesn't have traditional object oriented concepts such as classes, instead it uses a concept called traits. Traits are similar to abstract classes in the sense that they define an interface a struct must conform to. A trait can be defined as such:


```rust
trait Shape {
    fn area(self) -> i32;
}
```


The trait can then be implemented on a struct.


```rust
struct Square {
    side_length: i32
}

impl Shape for Square {
    fn area(self) -> i32 {
        self.side_length * self.side_length
    }
}
```


Note, traits can also have a default implementation:


```rust
trait Shape {
    fn area(self) -> i32;

    fn is_shape(self) -> bool {
        true
    }
}
```



## Scala

Scala has ''abstract classes'', which are classes that cannot be instantiated. They
can contain implementation as well as just interface. Non-abstract classes, on the
other hand, cannot contain interfaces without implementation.

Scala also has ''traits'', which may contain implementations or not as needed, without
any abstract requirement. On the other hand, traits must be ''mixed in'' a class,
instead of being directly instantiated. That doesn't matter all that much, as they can
be mixed with <tt>AnyRef</tt>, which is the base parent class of all user-defined
classes.

Any element of a trait or class can be made abstract, including types, with a very
different meaning that described in this page. Here are some examples:


```scala
abstract class X {
  type A
  var B: A
  val C: A
  def D(a: A): A
}

trait Y {
  val x: X
}
```


When integrating with Java, traits without implementation appear as interfaces.


## Seed7

The [http://seed7.sourceforge.net/manual/objects.htm object orientation of Seed7] is based on inteface types.
An abstract type consists of an interface type and interface functions, that use the interface.
Interface (DYNAMIC) functions describe what can be done with objects of an interface type.
Seed7 functions are freestanding and don't have an implicit ''this'' (or ''self'') parameter.
This concept allows [http://seed7.sourceforge.net/faq.htm#multiple_dispatch multiple dispatch].
Instead of an implicit ''this'' parameter an interface function has a parameter of an interface type.
A function is automaticall attached to the interface, when it has an parameter of the interface type.


```seed7

const type: myInterf is sub object interface;

const func integer: method1 (in myInterf: interf, in float: aFloat) is DYNAMIC;
const func integer: method2 (in myInterf: interf, in string: name) is DYNAMIC;
const func integer: add (in myInterf: interf, in integer: a, in integer: b) is DYNAMIC;

```



## Sidef

{{trans|Perl 6}}

```ruby
class A {
    # must be filled in by the class which will inherit it
    method abstract() { die 'Unimplemented' };

    # can be overridden in the class, but that's not mandatory
    method concrete() { say '# 42' };
}

class SomeClass << A {
    method abstract() {
        say "# made concrete in class"
    }
}

var obj = SomeClass.new;
obj.abstract();   # made concrete in class
obj.concrete();   # 42
```


## Simula

Abtract Datatypes are declared using the VIRTUAL keyword.
For example, we need the following two procedures hash and equalto for a hash map implementation.

```simula

    ! ABSTRACT HASH KEY TYPE ;
    LISTVAL CLASS HASHKEY;
    VIRTUAL:
        PROCEDURE HASH IS
            INTEGER PROCEDURE HASH;;
        PROCEDURE EQUALTO IS
            BOOLEAN PROCEDURE EQUALTO(K); REF(HASHKEY) K;;
    BEGIN
    END HASHKEY;

```

A concrete implementation can be derived as follows:

```simula

    ! COMMON HASH KEY TYPE IS TEXT ;
    HASHKEY CLASS TEXTHASHKEY(T); VALUE T; TEXT T;
    BEGIN
        INTEGER PROCEDURE HASH;
        BEGIN
            INTEGER I;
            T.SETPOS(1);
            WHILE T.MORE DO
                I := 31*I+RANK(T.GETCHAR);
            IF DEBUG THEN BEGIN
                OUTIMAGE;
                OUTTEXT("HASHMAPS.TEXTHASHKEY.HASH=");
                OUTINT(I,0);
                OUTIMAGE;
            END;
            HASH := I;
        END HASH;
        BOOLEAN PROCEDURE EQUALTO(K); REF(HASHKEY) K;
            EQUALTO := T = K QUA TEXTHASHKEY.T;
    END TEXTHASHKEY;

```



## Standard ML

Standard ML does not have any built-in support for object-oriented programming.
Instead it supports abstraction through a module system. Every module has a signature
describing the types and values that can be accessed from outside a module.

The act of giving a signature to a module is called ascription. There are two type of ascription:
Transparent (written <tt>:</tt) and opaque (written <tt>:></tt>). If a structure is ascribed transparently,
none of the types are abstract. If it is ascribed opaquely, all types are abstract by default, but can be specified
explicitly in the signature, in which case they are not abstract.

Here is an example signature for a queue data structure:

```sml
signature QUEUE = sig
  type 'a queue
  val empty : 'a queue
  val enqueue : 'a -> 'a queue -> 'a queue
  val dequeue : 'a queue -> ('a * 'a queue) option
end
```


Because we did not specify an implementation for <tt>'a queue</tt>, the type
will be abstract if we use opaque ascription. Instead we could create a version of the signature which specifies the type,
in which case it will never be abstract:

```sml
signature LIST_QUEUE = sig
  type 'a queue = 'a list
  val empty : 'a queue
  val enqueue : 'a -> 'a queue -> 'a queue
  val dequeue : 'a queue -> ('a * 'a queue) option
end
```


Then say we have a structure ListQueue which implements queues as lists. If we write <tt>ListQueue :> QUEUE</tt>
then the queue type will be abstract, but if we write <tt>ListQueue : QUEUE</tt> or <tt>ListQueue : LIST_QUEUE</tt> it won't.


## Tcl

{{works with|Tcl|8.6}} or {{libheader|TclOO}}

While in general Tcl does not use abstract classes at all (and has no need at all for interfaces due to supporting multiple inheritance and mixins), an equivalent effect can be had by concealing the construction methods on the class instance; instances are only created by subclassing the class first (or by mixing it in). In this example, the methods are also error-returning stubs...

```Tcl
oo::class create AbstractQueue {
    method enqueue item {
        error "not implemented"
    }
    method dequeue {} {
        error "not implemented"
    }
    self unexport create new
}
```


{{omit from|TorqueScript}}


## VBA

In VBA a class can implement properties declared by an other class, the interface class.
The implementing class states "Implements <name of interface class". The names of the implemented properties are prepended by the name of the interface class and an underscore "_". Names of (interface) classes and properties can therefore not contain an underscore.

## Visual Basic


###  Abstract Classes 

Visual Basic doesn't support abstract classes or implementation inheritance.


###  Interfaces 

In Visual Basic, every class is also an interface that other classes can implement. It has this feature because it is based on COM.


## Visual Basic .NET


###  Abstract Classes 

* Overridable means subclasses may change the method's implementation. By default, methods in VB cannot be overridden.
* MustOverride means the subclasses must provide an implementation
* By convention all abstract classes have one or more Protected constructors.


```vbnet
MustInherit Class Base

   Protected Sub New()

   End Sub

   Public Sub StandardMethod()
       'code
   End Sub

   Public Overridable Sub Method_Can_Be_Replaced()
       'code
   End Sub

   Public MustOverride Sub Method_Must_Be_Replaced()

End Class
```



###  Interfaces 


Interfaces may contain Functions, Subroutines, Properties, and Events.


```vbnet
Interface IBase
   Sub Method_Must_Be_Implemented()
End Interface
```



## zkl

In zkl, nothing is ever abstract, objects are always runnable. However, it is easy to define "meta" objects, objects that define an interface/api for a "class" of objects. For example, it is desirable for "stream" objects (such as File, List, etc) to share semantics so that code doesn't need to know what the source object really is.

```zkl
class Stream{	// Mostly virtural base class
   var [proxy protected]
      isBroken = fcn { _broken.isSet() },
      isClosed = fcn { return(_closed.isSet() or _broken.isSet()); };
   fcn init{
      var [protected]
         _closed	= Atomic.Bool(True),
	 _broken	= Atomic.Bool(False),
	 whyBroken	= Void;
   }
   fcn clear	  { _closed.clear(); _broken.clear(); return(self.topdog); }
   fcn open       { return(topdog.init(vm.pasteArgs())); }
   fcn toStream   { return(self); }
   fcn close      { _closed.set(); return(self.topdog); }
   fcn flush      { return(self.topdog); }
   fcn read       { throw(Exception.TheEnd); }	// destructive or advance
   fcn readln     { throw(Exception.TheEnd); }
   fcn write(x)	  { return(self.topdog); }
   fcn writeln(x) { return(self.topdog); }
   fcn walker	  { return((0).walker(*,wap((self.topdog.read.fpM(""))))); }
}
```

*The topdog property is the "youngest" child in the inheritance tree (root if you view the tree upside down), it allows a "parent" (or super) to access or pass control to, the actual instance.
*If you wish to "force" method implementation, you can have a meta method throw an NotImplementedError. This is run time thing, not compile time.

And now for a "real" object:

```zkl
class DevNull(Stream){
   var [const] fileName = "DevNull";	// compatibility with File
   fcn init     { Stream.init() }
   fcn write(x) { return(0); }
}
```



{{omit from|Applesoft BASIC}}
{{omit from|ALGOL 68}}
{{omit from|AWK}}
{{omit from|BASIC}}
{{omit from|Commodore BASIC}}
{{omit from|Erlang}}
{{omit from|Factor|Factor is a dynamic language. You could of course do the solution of python/ruby (throw exceptions for unimplemented methods)}}
{{omit from|Falcon|Does not support the concept of an abstract type.}}
{{omit from|gnuplot}}
{{omit from|Integer BASIC}}
{{omit from|J}}
{{omit from|JavaScript}}
{{omit from|Icon}}
{{omit from|LaTeX}}
{{omit from|Make}}
{{omit from|Mathematica}}
{{omit from|Maxima}}
{{omit from|M4}}
{{omit from|Metafont}}
{{omit from|ML/I}}
{{omit from|Modula-2}}
{{omit from|MOO}}
{{omit from|NSIS}}
{{omit from|Octave}}
{{omit from|PlainTeX}}
{{omit from|Scratch}}
{{omit from|TI-89 BASIC|Does not have static or user-defined types.}}
{{omit from|UNIX Shell}}
{{omit from|ZX Spectrum Basic}}
