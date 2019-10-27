+++
title = "Inheritance/Single"
description = ""
date = 2019-10-15T12:00:20Z
aliases = []
[extra]
id = 2838
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}
[[Category:Object oriented]]
[[Category:Type System]]
[[Category:Encyclopedia]]
:''This task is about derived types; for implementation inheritance, see [[Polymorphism]].

Inheritance is an operation of [[type algebra]] that creates a new type from one or several parent types. The obtained type is called '''derived type'''. It inherits some of the properties of its parent types. Usually inherited properties are:
* methods
* components
* parts of the representation
The [[classes | class]] of the new type is a '''subclass''' of the classes rooted in the parent types. When all (in certain sense) properties of the parents are preserved by the derived type, it is said to be a [[wp:Liskov_substitution_principle|Liskov subtype]]. When properties are preserved then the derived type is ''substitutable'' for its parents in all contexts. Usually full substitutability is achievable only in some contexts.

Inheritance is
* '''single''', when only one parent is allowed
* '''[[multiple inheritance | multiple]]''', otherwise

Some single inheritance languages usually allow multiple inheritance for certain [[abstract type]]s, interfaces in particular.

Inheritance can be considered as a relation parent-child. Parent types are sometimes called '''supertype''', the derived ones are '''subtype'''. This relation is [[wp:Transitive_relation|transitive]] and [[wp:Reflexive_relation|reflexive]]. Types bound by the relation form a [[wp:Directed_acyclic_graph directed acyclic graph]] (ignoring reflexivity). With single inheritance it becomes a [[wp:Tree_(graph_theory)|tree]].

'''Task''': Show a tree of types which inherit from each other. The top of the tree should be a class called Animal. The second level should have Dog and Cat. Under Dog should be Lab and Collie. None of the classes need to have any functions, the only thing they need to do is inherit from the specified superclasses (overriding functions should be shown in [[Polymorphism]]). The tree should look like this:

```txt
    Animal
      /\
     /  \
    /    \
   Dog   Cat
   /\
  /  \
 /    \
Lab   Collie
```



## ActionScript


```actionscript
public class Animal {
    // ...
}
```


```actionscript
public class Cat extends Animal {
    // ...
}
```


```actionscript
public class Dog extends Animal {
    // ...
}
```


```actionscript
public class Lab extends Dog {
    // ...
}
```


```actionscript
public class Collie extends Dog {
    // ...
}
```



## Ada


```ada
package Inheritance is
   type Animal is tagged private;
   type Dog is new Animal with private;
   type Cat is new Animal with private;
   type Lab is new Dog with private;
   type Collie is new Dog with private;
private
   type Animal is tagged null record;
   type Dog is new Animal with null record;
   type Cat is new Animal with null record;
   type Lab is new Dog with null record;
   type Collie is new Dog with null record;
end Inheritance;
```



## Aikido


```aikido 
class Animal{
   //functions go here...
}
```


```aikido 
class Dog extends Animal {
   //functions go here...
}
```


```aikido 
class Cat extends Animal {
   //functions go here...
}
```


```aikido 
class Lab extends Dog {
   //functions go here...
}
```


```aikido 
class Collie extends Dog {
   //functions go here...
}
```



## AmigaE


```amigae

OBJECT animal
ENDOBJECT

OBJECT dog OF animal
ENDOBJECT

OBJECT cat OF animal
ENDOBJECT

OBJECT lab OF dog
ENDOBJECT

OBJECT collie OF dog
ENDOBJECT

```



## Arturo



```arturo
Animal #{
	numberOfFeet 0
}

Dog $(inherit ~Animal #{
	numberOfFeet 4
	breed ""
	name ""

	init {
		// some initialization for Dog's
	}
	
})

Lab $(inherit ~Dog #{
	breed "labrador"

	init [n]{
		name n
	}
})

myLab $(new ~Lab "Max")

log myLab
```


{{out}}


```txt
#{
	breed           "labrador"
	init            <function: 0x110647F20>
	name            "Max"
	numberOfFeet    4
}
```



## AutoHotkey

{{works with|AutoHotkey_L}}

AutoHotkey_L is prototype-based. However, for convenience, class-syntax may be used to create a base object.

```AutoHotkey
dog := new Collie
MsgBox, % "A " dog.__Class " is a " dog.base.base.__Class " and is part of the " dog.kingdom " kingdom."

class Animal {
   static kingdom := "Animalia" ; Class variable
}
class Dog extends Animal {
}
class Cat extends Animal {
}
class Lab extends Dog {
}
class Collie extends Dog {
}
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"CLASSLIB"
      
      DIM Animal{method}
      PROC_class(Animal{})
      
      DIM Cat{method}
      PROC_inherit(Cat{}, Animal{})
      PROC_class(Cat{})
      
      DIM Dog{method}
      PROC_inherit(Dog{}, Animal{})
      PROC_class(Dog{})
      
      DIM Labrador{method}
      PROC_inherit(Labrador{}, Dog{})
      PROC_class(Labrador{})
      
      DIM Collie{method}
      PROC_inherit(Collie{}, Dog{})
      PROC_class(Collie{})
```



## C

* See [[Inheritance/C]]


## ChucK


```ChucK
public class Drums{
   //functions go here...
}
```


```ChucK
public class LatinKit extends Drums{
   //functions go here...
}
```


```ChucK
public class ElectronicKit extends Drums{
   //functions go here...
}
```


```ChucK
public class Congas extends LatinKit{
   //functions go here...
}
```


```ChucK
public class TechnoDrums extends ElectronicKit{
   //functions go here...
}
```



## C++


```cpp
class Animal
{
  // ... 
};

class Dog: public Animal
{
  // ... 
};

class Lab: public Dog
{
  // ...
};

class Collie: public Dog
{
  // ...
};

class Cat: public Animal
{
  // ...
};
```


=={{header|C sharp|C#}}==

```csharp
class Animal
{ 
  /* ... */ 
  // ...
}

class Dog : Animal
{ 
  /* ... */ 
  // ...
}

class Lab : Dog
{ 
  /* ... */ 
  // ...
}

class Collie : Dog
{ 
  /* ... */
  // ... 
}

class Cat : Animal
{ 
  /* ... */
  // ... 
}
```



## Clojure


This is not very useful in clojure


```Clojure
(gen-class :name Animal)
(gen-class :name Dog :extends Animal)
(gen-class :name Cat :extends Animal)
(gen-class :name Lab :extends Dog)
(gen-class :name Collie :extends Dog)
```


More useful:


```Clojure
(derive ::dog ::animal)
(derive ::cat ::animal)
(derive ::lab ::dog)
(derive ::collie ::dog)
```


use:


```Clojure>user
 (isa? ::dog ::animal)
true
user> (isa? ::dog ::cat)
false
user> (isa? ::collie ::animal)
true
```



## COBOL


```cobol
       CLASS-ID. Animal.
           *> ...
       END CLASS Animal.
       
       CLASS-ID. Dog INHERITS Animal.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           CLASS Animal.

           *> ...
       END CLASS Dog.
       
       CLASS-ID. Cat INHERITS Animal. 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           CLASS Animal. 

           *> ...
       END CLASS Cat.
           
       CLASS-ID. Lab INHERITS Dog.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           CLASS Dog.

           *> ...
       END CLASS Lab.
       
       CLASS-ID. Collie INHERITS Dog.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           CLASS Dog.

           *> ...
       END CLASS Collie.
```



## Coco



```coco
class Animal
class Cat extends Animal
class Dog extends Animal
class Lab extends Dog
class Collie extends Dog
```


On the subject of inheritance, it is worth noting that Coco's <code>super</code> works differently from CoffeeScript's. In particular, the constructor of a subclass should generally say <code>super ...</code>, not just <code>super</code>. Here is a translation of the example from the CoffeeScript documentation:


```coco
class Animal

  (@name) ->

  move: (meters) ->
    alert @name + " moved #{meters}m."

class Snake extends Animal

  -> super ...

  move: ->
    alert 'Slithering...'
    super 5

class Horse extends Animal

  -> super ...

  move: ->
    alert 'Galloping...'
    super 45

sam = new Snake 'Sammy the Python'
tom = new Horse 'Tommy the Palomino'

sam.move!
tom.move!
```



## Comal

{{works with|UniComal}}
{{works with|AmiComal}}

```Comal
       STRUC Animal
           DIM Species$ OF 20
       ENDSTRUC Animal
       
       STRUC Dog 
              INHERIT Animal
              DIM Race$ OF 20
              FUNC New CONSTRUCTOR
                     Species$="Dog"
              ENDFUNC New
       ENDSTRUC Dog
       
       STRUC Cat
              INHERIT Animal
              DIM Race$ OF 20
              FUNC New CONSTRUCTOR
                     Species$="Cat"
              ENDFUNC New
       ENDSTRUC Cat
           
       STRUC Lab
              INHERIT Dog
              FUNC New CONSTRUCTOR
                     Race$:="Lab"
              ENDFUNC New
       ENDSTRUC Lab
       
       STRUC Collie
              INHERIT Dog
              FUNC New CONSTRUCTOR
                     Race$:="Collie"
              ENDFUNC New
       ENDSTRUC Collie
```


== {{header|Common Lisp}} ==

Using CLOS classes, we have the following:


```lisp
(defclass animal ()       ())
(defclass dog    (animal) ())
(defclass lab    (dog)    ())
(defclass collie (dog)    ())
(defclass cat    (animal) ())
```


Alternatively, since there is no multiple inheritance in the task requirement, structures could also be used:


```lisp
(defstruct animal)
(defstruct (dog    (:include animal)))
(defstruct (lab    (:include dog)))
(defstruct (collie (:include dog)))
(defstruct (cat    (:include animal)))
```


(Structures are less flexible than CLOS objects but often somewhat more efficiently implemented, due to those restrictions.)

Inheritance is not required for object-oriented programming in Lisp. It is used for code reuse, because it allows common utilities and protocol conventions to be factored out into base class methods.  However, a class doesn't have to inherit from a base class just so that some existing methods can work with instances of that class.

Furthermore, all of the "basic types" also have a class, so methods can be readily specialized to lists, integers, strings, symbols, et cetera.   This is done without having to modify any class definitions.


```lisp

;;; ASN.1 serialization logic specialized for animal class
(defmethod serialize-to-asn-1 ((a animal))
  #| ... |#
  )

 ;;; casually introduce the method over strings too; no relation to animal
(defmethod serialize-to-asn-1 ((s string))
  #| ... #|
  )
```


These classes do not have to inherit from some interface or base class which provides a prototype for the serialize-to-asn-1 method. Such a requirement has more to do with static typing than object oriented programming. Usually in languages which require such inheritance, there are also statically typed references. A class must conform to some "ASNEncodable" class so that its instances can be passed to functions which expect references to an ASN1Encodable type, which is verified at compile time.

== {{header|Component Pascal}} ==


```oberon2

	TYPE
		Animal = ABSTRACT RECORD (*  *) END;
		Cat = RECORD (Animal)  (*  *) END; (* final record (cannot be extended) - by default *)
		Dog = EXTENSIBLE RECORD (Animal)  (*  *) END; (* extensible record *)
		Lab = RECORD (Dog)  (*  *) END;
		Collie = RECORD (Dog)  (*  *) END;

```



## D


```d
class Animal {
    // ...
}

class Dog: Animal {
    // ...
}

class Lab: Dog {
    // ...
}

class Collie: Dog {
    // ...
}

class Cat: Animal {
    // ...
}

void main() {}
```



## Delphi



```Delphi
type
  Animal = class(TObject)
  private
    // private functions/variables
  public
    // public functions/variables
  end;

  Dog = class(Animal);
  Cat = class(Animal);
  Collie = class(Dog);
  Lab = class(Dog);
```



## DWScript



```Delphi
type
  Animal = class(TObject)
  private
    // private functions/variables
  public
    // public functions/variables
  end;

type Dog = class(Animal) end;
type Cat = class(Animal) end;
type Collie = class(Dog) end;
type Lab = class(Dog) end;
```



## E


Outside of interactions with the host platform's objects, E does not generally deal in complex type hierarchies; the focus is more on "what guarantees does this object provide", and composition rather than inheritance. However, it is possible to set up a type hierarchy scheme with just a bit of code.

In E, a ''guard'' accepts, or coerces, certain objects and rejects others; its [[wp:Range (mathematics)|range]] constitutes a type. An ''auditor'' examines the implementation of an object and marks it approved; a ''stamp'' is an auditor which does no actual checking. Here, we create a guard/stamp pair; the guard accepts every stamped object. The stamp also asks for each supertype's stamp on the objects it audits.


```e
def makeType(label, superstamps) {
    def stamp {
        to audit(audition) {
            for s in superstamps { audition.ask(s) }
            return true
        }
    }
    def guard {
        to coerce(specimen, ejector) {
            if (__auditedBy(stamp, specimen)) {
                return specimen
            } else {
                throw.eject(ejector, `$specimen is not a $label`)
            }
        }
    }
    return [guard, stamp]
}
```


Setting up the task's specified tree:


```e
def [Animal, AnimalStamp] := makeType("Animal", [])

def [Cat, CatStamp] := makeType("Cat", [AnimalStamp])
def [Dog, DogStamp] := makeType("Dog", [AnimalStamp])

def [Lab, LabStamp] := makeType("Lab", [DogStamp])
def [Collie, CollieStamp] := makeType("Collie", [DogStamp])
```


Some example objects:


```e
def fido implements LabStamp {}
def tom implements CatStamp {}
def brick {} # not an animal
```


Testing against the types:


```e
? fido :Animal
# value: <fido>

? fido :Cat
# problem: <fido> is not a Cat

? fido :Lab
# value: <fido>

? tom :Animal
# value: <tom>

? tom :Cat
# value: <tom>

? brick :Animal
# problem: <brick> is not a Animal
```



## Eiffel


```eiffel 
class
    ANIMAL
end
```


```eiffel 
class
    DOG
inherit
    ANIMAL
end
```


```eiffel 
class
    CAT
inherit
    ANIMAL
end
```


```eiffel 
class
    LAB
inherit
    DOG
end
```


```eiffel 
class
    COLLIE
inherit
    DOG
end
```



## Elena

ELENA 4.x :

```elena
class Animal
{ 
  // ...
}
 
class Dog : Animal
{ 
  // ...
}
 
class Lab : Dog
{ 
  // ...
}
 
class Collie : Dog
{ 
  // ... 
}
 
class Cat : Animal
{ 
  // ... 
}
```



## Factor


```factor
TUPLE: animal ;
TUPLE: dog < animal ;
TUPLE: cat < animal ;
TUPLE: lab < dog ;
TUPLE: collie < dog ;
```



## Fancy


```fancy
class Animal {
  # ...
}

class Dog : Animal {
  # ...
}

class Cat : Animal {
  # ...
}

class Lab : Dog {
  # ...
}

class Collie : Dog {
  # ...
}
```



## Fantom


```fantom
class Animal 
{
}

class Dog : Animal 
{
}

class Cat : Animal 
{
}

class Lab : Dog 
{
}

class Collie : Dog 
{
}
```



## Forth

{{works with|4tH|3.61.5}}
There are numerous, mutually incompatible object oriented frameworks for Forth. This one works with the FOOS preprocessor extension of [[4tH]].

```forth
include 4pp/lib/foos.4pp

:: Animal class end-class {} ;
:: Dog extends    Animal end-extends {} ;
:: Cat extends    Animal end-extends {} ;
:: Lab extends    Dog    end-extends {} ;
:: Collie extends Dog    end-extends {} ;
```



Works with any ANS Forth

Needs the FMS-SI (single inheritance) library code located here:
http://soton.mpeforth.com/flag/fms/index.html

```forth
include FMS-SI.f

:class Animal  ;class
:class Dog    <super Animal ;class
:class Cat    <super Animal ;class
:class Lab    <super Dog    ;class
:class Collie <super Dog    ;class
```



## Fortran

OO has been part of the Fortran standard since 2003 but the compilers are still playing catchup. This example builds with the Intel 11.1.069 compiler (free for personal use on linux).


```fortran
module anim

  type animal
  end type animal

  type, extends(animal) :: dog
  end type dog

  type, extends(animal) :: cat
  end type cat

  type, extends(dog) :: lab
  end type lab

  type, extends(dog) :: collie
  end type collie

end module anim
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Type Animal Extends Object ' to enable virtual methods etc. if needed
  ' ...
End Type

Type Dog Extends Animal
  ' ...
End Type

Type Cat Extends Animal
  ' ...
End Type

Type Lab Extends Dog
  ' ...
End Type

Type Collie Extends Dog
  ' ...
End Type
```



## F#

The <code>()</code> behind the class names indicates a public default constructor; you need some type of public constructor to derive from a class.

```fsharp
type Animal() =
  class  // explicit syntax needed for empty class
  end

type Dog() =
  inherit Animal()

type Lab() =
  inherit Dog()

type Collie() =
  inherit Dog()

type Cat() =
  inherit Animal()
```



## Go

Go eschews most trappings of inheritance, yet it's anonymous field feature allows building one struct type upon another and accessing fields of "embedded" types without extra synax.

```go
package main

type animal struct {
    alive bool
}

type dog struct {
    animal
    obedienceTrained bool
}

type cat struct {
    animal
    litterBoxTrained bool
}

type lab struct {
    dog
    color string
}

type collie struct {
    dog
    catchesFrisbee bool
}

func main() {
    var pet lab
    pet.alive = true
    pet.obedienceTrained = false
    pet.color = "yellow"
}

```



## Groovy


```groovy
class Animal{
   //contents go here...
}
```


```groovy
class Dog extends Animal{
   //contents go here...
}
```


```groovy
class Cat extends Animal{
   //contents go here...
}
```


```groovy
class Lab extends Dog{
   //contents go here...
}
```


```groovy
class Collie extends Dog{
   //contents go here...
}
```



## Haskell

A type can't inherit properties from other types, but it can belong to any number of type classes, which may themselves be subclasses of other type classes.


```haskell
class Animal a
class Animal a => Cat a
class Animal a => Dog a
class Dog a => Lab a
class Dog a => Collie a
```



## Haxe


```haxe
class Animal {
    // ...
}
```


```haxe
class Cat extends Animal {
    // ...
}
```


```haxe
class Dog extends Animal {
    // ...
}
```


```haxe
class Lab extends Dog {
    // ...
}
```


```haxe
class Collie extends Dog {
    // ...
}
```


== Icon and {{header|Unicon}} ==

This example only works in Unicon.


```Unicon

class Animal ()
end
 
class Dog : Animal  ()
end
 
class Cat : Animal  ()
end
 
class Lab : Dog  ()
end
 
class Collie : Dog ()
end

```



## Inform 7


```inform7
An animal is a kind of thing.
A cat is a kind of animal.
A dog is a kind of animal.
A collie is a kind of dog.
A lab is a kind of dog.
```


"Animal" is actually a predefined kind in Inform 7, so its definition here is redundant (but legal).


## Io



```io
Animal := Object clone
Cat := Animal clone
Dog := Animal clone
Collie := Dog clone
Lab := Dog clone
```



## J


Here is how this would normally be done:


```j
coclass 'Animal'
```


```j
coclass 'Dog'
coinsert 'Animal'
```


```j
coclass 'Cat'
coinsert 'Animal'
```


```j
coclass 'Lab'
coinsert 'Dog'
```


```j
coclass 'Collie'
coinsert 'Dog'
```


<code>coclass</code> specifies that following definitions will be within the named class, and <code>coinsert</code> specifies that the current class will inherit from the named classes (or object -- in J the only difference between a class and an object is its name and how you can create them -- this motivates the "co" prefix on operations which manipulate '''c'''lasses and '''o'''bjects).

See http://www.jsoftware.com/help/jforc/modular_code.htm

That said, some operations in J -- including <code>coinsert</code> -- will create classes if they did not already exist.  So the above may be simplified to:


```j
coinsert_Dog_ 'Animal'
coinsert_Cat_ 'Animal'
coinsert_Lab_ 'Dog'
coinsert_Collie_ 'Dog'
```


That said, note that classes and objects are not "types" in J.  Instead, they are components of names.  In general, when we deal with objects and classes we deal with references to the underlying representation, and in J the references are names, so a collection of classes and objects, in J, would be a collection of names which refer to classes and objects.  In other words, the "type" (to the degree that there is a type) would be best thought of as "name" (or, more mechanically: boxed list of characters).


## Java


```java
public class Animal{
   //functions go here...
}
```


```java
public class Dog extends Animal{
   //functions go here...
}
```


```java
public class Cat extends Animal{
   //functions go here...
}
```


```java
public class Lab extends Dog{
   //functions go here...
}
```


```java
public class Collie extends Dog{
   //functions go here...
}
```



## JavaScript

JavaScript is a class-free, object-oriented language, and as such, it uses prototypal inheritance instead of classical inheritance.

```javascript
function Animal() {
    // ...
}
```



```javascript
function Dog() {
    // ...
}
Dog.prototype = new Animal();
```



```javascript
function Cat() {
    // ...
}
Cat.prototype = new Animal();
```



```javascript
function Collie() {
    // ...
}
Collie.prototype = new Dog();
```



```javascript
function Lab() {
    // ...
}
Lab.prototype = new Dog();
```



```javascript
Animal.prototype.speak = function() {print("an animal makes a sound")};

var lab = new Lab();
lab.speak();  // shows "an animal makes a sound"
```



## Julia

Julia is not really an object-oriented programming language. It support polymorphism and inheriting functionality but not structure. Thus inheritance hierarchies must be made with abstract types. Abstract types can not be instantiated and do not contain any fields. So below Dog is abstract while Collie is concrete type which may contain fields.

```julia

abstract type Animal end
abstract type Dog <: Animal end
abstract type Cat <: Animal end

struct Lab <: Dog end
struct Collie <: Dog end

```



## Kite


```Kite
class Animal [
	#Method goes here
];

class Dog from Animal [
	#Method goes here	
];

class Lab from Dog [
	#Method goes here
];

class collie from Dog [
	#Method goes here
];

```



## Kotlin


```scala
// version 1.0.6

open class Animal {
    override fun toString() = "animal"
}

open class Dog : Animal() {
    override fun toString() = "dog"
}

class Cat : Animal() {
    override fun toString() = "cat"
}

class Labrador : Dog() {
    override fun toString() = "labrador"
}

class Collie : Dog() {
    override fun toString() = "collie"
}

fun main(args: Array<String>) {
    val felix: Animal = Cat()
    val rover: Animal = Dog()
    val bella: Dog = Labrador()
    val casey: Dog = Collie()
    println("Felix is a $felix")
    println("Rover is a $rover")
    println("Bella is a $bella")
    println("Casey is a $casey")
}
```


{{out}}

```txt

Felix is a cat
Rover is a dog
Bella is a labrador
Casey is a collie

```



## Lasso


```Lasso>define animal =
 type {
	data public gender::string
}

define dog => type {
	parent animal
}

define cat => type {
	parent animal
}

define collie => type {
	parent dog
}

define lab => type {
	parent dog
}

local(myanimal = lab)

#myanimal -> gender = 'Male'
#myanimal -> gender
```

-> Male


## Lingo

In Lingo Classes are represented by "parent scripts". Instead of using new() as in the code below, child classes can also use rawNew() when creating an instance of their parent classes. rawNew() creates an instance of a class without calling its initialization function 'new' (constructor).

```lingo
-- parent script "Animal"
-- ...
```



```lingo
-- parent script "Dog"
property ancestor

on new (me)
  me.ancestor = script("Animal").new()
  return me
end
```

  

```lingo
-- parent script "Cat"
property ancestor

on new (me)
  me.ancestor = script("Animal").new()
  return me
end
```



```lingo
-- parent script "Lab"
property ancestor

on new (me)
  me.ancestor = script("Dog").new()
  return me
end
```



```lingo
-- parent script "Collie"
property ancestor

on new (me)
  me.ancestor = script("Dog").new()
  return me
end
```



## Lisaac


```Lisaac
Section Header
+ name := ANIMAL;
// ...
```


```Lisaac
Section Header
+ name := CAT;
Section Inherit
- parent : ANIMAL := ANIMAL;
// ...
```


```Lisaac
Section Header
+ name := DOG;
Section Inherit
- parent : ANIMAL := ANIMAL;
// ...
```


```Lisaac
Section Header
+ name := LAB;
Section Inherit
- parent : DOG := DOG;
// ...
```


```Lisaac
Section Header
+ name := COLLIE;
Section Inherit
- parent : DOG := DOG;
// ...
```



## Logtalk

There is no "class" keyword in Logtalk; an "object" keyword is used instead (Logtalk objects play the role of classes, meta-classes, instances, or prototypes depending on the relations with other objects). 

```logtalk

:- object(thing,
    instantiates(thing)).
:- end_object.

:- object(animal,
    specializes(thing)).
    ...
:- end_object.

:- object(dog,
    specializes(animal)).
    ...
:- end_object.

:- object(cat,
    specializes(animal)).
    ...
:- end_object.

:- object(lab,
    specializes(dog)).
    ...
:- end_object.

:- object(collie,
    specializes(dog)).
    ...
:- end_object.
```




## M2000 Interpreter

There are two types of Inheritance. This is the type which we merge groups. Class functions are global functions. First a class function make a Group and then call a module, passing arguments if any, with same name as class name. The final group has no reference with any kind of class, it is an object of type Group, without pointer (we see it as a variable). We can make pointers to groups, but here we don't need that. We can place groups in containers, and here we place one in B(5). Using variables like IamAnimal (which is  a double with 0 value) and check if exist (using Valid() function) we can check the Inheritance.

In constructor the statement This=Animal() add all members of returned group to This. If a function in This is Final then can't be changed. We can make the "copy" using a third group: M=Animal() : M=This : This= M. First we make M as copy of Animal(), after that we merge This to M, and then we merge M to This. Why to do this? Because we want to leave members in non final stage. So M get the Animal's function, then M take This function and replace animals, and then This take M which have also the IamAnimal member.
Check Cat Class.



```M2000 Interpreter

Module CheckIt {
      Class Animal {
            IamAnimal
            Function objType$ {="Animal"}
            \\ read only
      }
      Class Dog {
            IamDog
            Function Final objType$ {="Dog"}
            Module Dog {
                  This=Animal()
            }
      }
      Class Cat {
            IamCat
            Function objType$ {="Cat"}
            Module Cat {
                  \\ Without using Final in function above
                  M=Animal()
                  M=This
                  This=M
            }      
      }
      Class Labrador {
            IamLabrador
            Function Final objType$ {="Labrador"}
            Module Labrador {
                  This=Dog()
            }
      }
      Class Collie {
            IamCollie
            Function Final objType$ {="Collie"}
            Module Collie {
                  This=Dog()
            }
      }
      Animal=Animal()
      Print Valid(Animal.IamAnimal)=True, Animal.objType$()="Animal"
      A=Collie()
      Dim B(0 to 9)
      B(5)=Cat()
      Print Valid(A.IamAnimal)=True, Valid(B(5).IamAnimal)=True
      Print Valid(A.IamDog)=True, Valid(A.IamCollie)=True
      Print Valid(B(5).IamCat)=True
      Print Valid(B(5).IamDog)=False
      Print A.objType$()="Collie"
      Print B(5).objType$()="Cat"
      \\ with @ we tell to interpreter to check A if has same members of Animal among other members
      Print Valid(@A as Animal)=True
      \\ For expressions, or items from containers we have to use a function
      \\ which copies objects before using in Valid(@..)
      Def ValidObj(X,Y)=Valid(@X as Y)
      Print ValidObj(B(5), Animal)=True 
}
CheckIt

```



## Neko


```Neko
var Animal = $new(null);

var Dog = $new(null);
$objsetproto(Dog, Animal);

var Cat = $new(null);
$objsetproto(Cat, Animal);

var Lab = $new(null);
$objsetproto(Lab, Dog);

var Collie = $new(null);
$objsetproto(Collie, Dog);
```


## Nemerle


```nemerle
class Animal {
    // ...
}
 
class Dog: Animal {
    // ...
}
 
class Lab: Dog {
    // ...
}
 
class Collie: Dog {
    // ...
}
 
class Cat: Animal {
    // ...
}
```



## NetRexx

Class names cosmetically augmented slightly to prevent namespace pollution.

For brevity, all classes are defined within the same source file.  Normally classes exist as separate source units.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

class RInheritSingle public
  method main(args = String[]) public static
    animals = [ -
      RInheritSingle_Animal(), -
      RInheritSingle_Cat(), -
      RInheritSingle_Dog(), -
      RInheritSingle_Lab(), -
      RInheritSingle_Collie() -
      ]

    say 'Object ID'.left(12) 'Class type'.left(24)  'Superclass type'
    say '.'.left(12, '.')    '.'.left(24, '.')      '.'.left(24, '.')
    loop animal over animals
      parse animal.whatAmI() oid ct st
      say oid.left(12) ct.left(24) st
      end animal
    return

class RInheritSingle_Animal private
  properties indirect
    whatThatIs = String
    whatThisIs = String
  method RInheritSingle_Animal() public
    -- Animal specific set-up
    setWhatThatIs(this.getClass().getSuperclass().getSimpleName())
    setWhatThisIs(this.getClass().getSimpleName())
    return
  method hashToString() public
    return '@'(Rexx this.hashCode()).d2x().right(8, 0)
  method whatAmI() public
    iAmText = hashToString() getWhatThisIs() getWhatThatIs()
    return iAmText

class RInheritSingle_Cat private extends RInheritSingle_Animal
  method RInheritSingle_Cat() public
    -- Do Cat specific set-up
    return

class RInheritSingle_Dog private extends RInheritSingle_Animal
  method RInheritSingle_Dog() public
    -- Do Dog specific set-up
    return

class RInheritSingle_Lab private extends RInheritSingle_Dog
  method RInheritSingle_Lab() public
    -- Do Lab specific set-up
    return

class RInheritSingle_Collie private extends RInheritSingle_Dog
  method RInheritSingle_Collie() public
    -- Do Collie specific set-up
    return

```

{{out}}

```txt

Object ID    Class type               Superclass type
............ ........................ ........................
@3F81D405    RInheritSingle_Animal    Object
@51430296    RInheritSingle_Cat       RInheritSingle_Animal
@065EEF88    RInheritSingle_Dog       RInheritSingle_Animal
@42BFCCFC    RInheritSingle_Lab       RInheritSingle_Dog
@3E2AD6A0    RInheritSingle_Collie    RInheritSingle_Dog

```



## Nim


```nim
type
  Animal = object of RootObj
  Dog    = object of Animal
  Cat    = object of Animal
  Lab    = object of Dog
  Collie = object of Dog
```



## Oberon

Tested with [https://miasap.se/obnc OBNC].

```Oberon
MODULE Animals;

   TYPE
      Animal = RECORD END;
      Dog = RECORD (Animal) END;
      Cat = RECORD (Animal) END;
      Lab = RECORD (Dog) END;
      Collie = RECORD (Dog) END;

END Animals.

```


=={{header|Oberon-2}}==
Works with oo2c Version 2

```oberon2

MODULE Animals;
TYPE
  Animal = POINTER TO AnimalDesc;
  AnimalDesc = RECORD END;
  
  Cat = POINTER TO CatDesc;
  CatDesc = RECORD (AnimalDesc) END;
  
  Dog = POINTER TO DogDesc;
  DogDesc = RECORD (AnimalDesc) END;
  
  Lab = POINTER TO LabDesc;
  LabDesc = RECORD (DogDesc) END;
  
  Collie = POINTER TO CollieDesc;
  CollieDesc = RECORD (DogDesc) END;

END Animals.

```



## Objeck


```objeck
class Animal
{ #~ ... ~# }
 
class Dog from Animal
{ #~ ... ~# }
 
class Lab from Dog
{ #~ ... ~# }
 
class Collie from Dog
{ #~ ... ~# }
 
class Cat from Animal
{ #~ ... ~# }
```


=={{header|Objective-C}}==

```objc
@interface Animal : NSObject
{
  // ... 
}
// ...
@end

@interface Dog : Animal
{
  // ... 
}
// ...
@end

@interface Lab : Dog
{
  // ... 
}
// ...
@end

@interface Collie : Dog
{
  // ... 
}
// ...
@end

@interface Cat : Animal
{
  // ... 
}
// ...
@end
```



## OCaml


```ocaml
class animal =
  object (self)
    (*functions go here...*)
  end
```


```ocaml
class dog =
  object (self)
    inherit animal
    (*functions go here...*)
  end
```


```ocaml
class cat =
  object (self)
    inherit animal
    (*functions go here...*)
  end
```


```ocaml
class lab =
  object (self)
    inherit dog
    (*functions go here...*)
  end
```


```ocaml
class collie =
  object (self)
    inherit dog
    (*functions go here...*)
  end
```




## Oforth



```Oforth
Object Class new: Animal
Animal Class new: Cat
Animal Class new: Dog
Dog Class new: Lab
Dog Class new: Collie
```



## ooRexx


```ooRexx

-- subclass of object by default
::class animal

::class cat subclass animal

::class dog subclass animal

::class lab subclass dog

::class collie subclass dog

```




## OxygenBasic


```oxygenbasic

class animal
  method show() as string
  return "Animal "
  end method
end Class

class dog
  from Animal Animal
  method show() as string
  return animal.show()+"dog "
  end method
end Class

class cat
  from animal animal
  method show() as string
  return animal.show()+"cat "
  end method
end Class

class Lab
  from dog dog
  method show() as string
  return dog.show()+"Lab "
  end method
end Class

class Collie
  from dog dog
  method show() as string
  return dog.show()+"Collie "
  end method
end Class


Collie c
print c.show 'result: Animal Dog Collie

```



## Oz


```oz
class Animal
   %% ...
end

class Dog from Animal
   %% ... 
end

class Lab from Dog
   %% ... 
end

class Collie from Dog
   %% ... 
end

class Cat from Animal
   %% ... 
end
```



## Pascal

See [[Inheritance/Single#Delphi | Delphi]]


## Perl



```perl
package Animal;
#functions go here...
1;
```



```perl
package Dog;
use Animal;
@ISA = qw( Animal );
#functions go here...
1;
```



```perl
package Cat;
use Animal;
@ISA = qw( Animal );
#functions go here...
1;
```



```perl
package Lab;
use Dog;
@ISA = qw( Dog );
#functions go here...
1;
```



```perl
package Collie;
use Dog;
@ISA = qw( Dog );
#functions go here...
1;
```


The same using the [http://search.cpan.org/perldoc?MooseX::Declare MooseX::Declare] module:


```perl
use MooseX::Declare;

class Animal {
    # methods go here...
}
class Dog extends Animal {
    # methods go here...
}
class Cat extends Animal {
    # methods go here...
}
class Lab extends Dog {
    # methods go here...
}
class Collie extends Dog {
    # methods go here...
}
```



## Perl 6


{{works with|Rakudo|2015-09-16}}

```perl6
class Animal {}
class Dog is Animal {}
class Cat is Animal {}
class Lab is Dog {}
class Collie is Dog {}

say Collie.^parents;     # undefined type object
say Collie.new.^parents; # instantiated object
```

{{out}}

```txt
((Dog) (Animal))
((Dog) (Animal))
```


The <tt>.^parents</tt> notation indicates a method call to the object's metaobject rather than to the object itself.


## PHP


```php
class Animal {
   // functions go here...
}

class Dog extends Animal {
   // functions go here...
}

class Cat extends Animal {
   // functions go here...
}

class Lab extends Dog {
   // functions go here...
}

class Collie extends Dog {
   // functions go here...
}
```



## PicoLisp


```PicoLisp
(class +Animal)

(class +Dog +Animal)

(class +Cat +Animal)

(class +Lab +Dog)

(class +Collie +Dog)
```


```PicoLisp
: (dep '+Animal)
+Animal
   +Cat
   +Dog
      +Collie
      +Lab
```



## PowerShell

{{works with|PowerShell|5}}

```PowerShell

class Animal {}
class Dog : Animal {}
class Cat: Animal {}
class Lab : Dog {}
class Collie : Dog {}

```



## PureBasic

Although PureBasic is mostly used for procedural coding it has both the ability to interact with object oriented libraries and code and also the capacity to write it if needed.

### Native version


```PureBasic
Interface Animal
  Eat()
  Sleep()
EndInterface

Interface Cat Extends Animal
  ChaseMouse()
EndInterface

Interface Dog Extends Animal
  Bark()
  WagTail()
EndInterface

Interface Lab Extends Dog
  Swim()
EndInterface

Interface Collie Extends Dog  
  HeardSheep()
EndInterface
```


### Simple OOP Version

Using the open-source precompiler [http://www.development-lounge.de/viewtopic.php?t=5915 SimpleOOP].

```PureBasic
Class Animal
EndClass

Class Dog Extends Animal
  Public Method Bark()
  EndMethod
EndClass

Class Cat Extends Animal
  Public Method Sleep()
  EndMethod
EndClass

Class Lab Extends Dog
  Public Method Swim()
  EndMethod
EndClass

Class Collie Extends Dog
  Public Method Fetch()
  EndMethod
EndClass

;- test the code
*Lassie.Collie = NewObject.Collie
*Lassie\Bark()
*Lassie\Fetch()
```



## Python

Unrevised style classes:

```python
class Animal:
  pass #functions go here...

class Dog(Animal):
  pass #functions go here...

class Cat(Animal):
  pass #functions go here...

class Lab(Dog):
  pass #functions go here...

class Collie(Dog):
  pass #functions go here...
```


New style classes:

```python
import time

class Animal(object):
    def __init__(self, birth=None, alive=True):
        self.birth = birth if birth else time.time()
        self.alive = alive
    def age(self):
        return time.time() - self.birth
    def kill(self):
        self.alive = False

class Dog(Animal):
    def __init__(self, bones_collected=0, **kwargs):
        self.bone_collected = bones_collected
        super(Dog, self).__init__(**kwargs)

class Cat(Animal):
    max_lives = 9
    def __init__(self, lives=max_lives, **kwargs):
        self.lives = lives
        super(Cat, self).__init__(**kwargs)
    def kill(self):
        if self.lives>0:
            self.lives -= 1
            if self.lives == 0:
                super(Cat, self).kill()
        else:
            raise ValueError
        return self

class Labrador(Dog):
    def __init__(self, guide_dog=False, **kwargs):
        self.guide_dog=False
        super(Labrador, self).__init__(**kwargs)

class Collie(Dog):
    def __init__(self, sheep_dog=False, **kwargs):
        self.sheep_dog=False
        super(Collie, self).__init__(**kwargs)

lassie = Collie()
felix = Cat()
felix.kill().kill().kill()
mr_winkle = Dog()
buddy = Labrador()
buddy.kill()
print "Felix has",felix.lives, "lives, ","Buddy is %salive!"%("" if buddy.alive else "not ")
```

{{out}}

```txt

Felix has 6 lives,  Buddy is not alive!

```



## R


### S3

Inheritance is implemented by setting the object's class attribute with a character vector.

```R
aCollie <- "woof"
class(aCollie) <- c("Collie", "Dog", "Animal")
```


### S4

Inheritance is implemented by using the 'contains' argument in setClass

```R
setClass("Animal", representation(), prototype())
setClass("Dog", representation(), prototype(), contains="Animal")
setClass("Cat", representation(), prototype(), contains="Animal")
setClass("Collie", representation(), prototype(), contains="Dog")
setClass("Lab", representation(), prototype(), contains="Dog")
```



## Racket



```racket

#lang racket

(define animal% (class object% (super-new)))
(define dog%    (class animal% (super-new)))
(define cat%    (class animal% (super-new)))
(define lab%    (class dog% (super-new)))
(define collie% (class dog% (super-new)))

;; unit tests
(require rackunit)

(check-true (is-a? (new dog%) animal%))
(check-false (is-a? (new collie%) cat%))

```



## REBOL


```REBOL
REBOL [
	Title: "Inheritance"
	URL: http://rosettacode.org/wiki/Inheritance
]

; REBOL provides subclassing through its prototype mechanism:

Animal: make object! [
	legs: 4
]

Dog: make Animal [
	says: "Woof!"
]
Cat: make Animal [
	says: "Meow..."
]

Lab: make Dog []
Collie: make Dog []

; Demonstrate inherited properties:

print ["Cat has" Cat/legs "legs."]

print ["Lab says:" Lab/says]
```


{{out}}

```txt
Cat has 4 legs.
Lab says: Woof!
```



## Ring


```ring

Class Animal
Class Dog from Animal
Class Cat from Animal
Class Lab from Dog
Class Collie from Dog

```



## Ruby

<code>inherited</code> is a method defined on an instance of a <code>Class</code> object.  It is invoked when a new subclass of the current class is defined (i.e. at the <code>end</code> statement of a <code>class</code> definition).

```ruby
class Animal
  #functions go here...
  def self.inherited(subclass)
    puts "new subclass of #{self}: #{subclass}"
  end
end

class Dog < Animal
  #functions go here...
end

class Cat < Animal
  #functions go here...
end

class Lab < Dog
  #functions go here...
end

class Collie < Dog
  #functions go here...
end
```


{{out}}

```txt
new subclass of Animal: Dog
new subclass of Dog: Lab
new subclass of Dog: Collie
new subclass of Animal: Cat
```



## Rust

A type can't inherit properties from other types, but it can implmement any number of traits, which may themselves be subtraits of other traits.

```Rust
trait Animal {}
trait Cat: Animal {}
trait Dog: Animal {}
trait Lab: Dog {}
trait Collie: Dog {}
```



## Scala

Scala has both classes and traits. Classes can only be singly inherited, but both
can inherit a trait multiple times. 
This inheritance can be declared at the point
of instantiation as well, precluding the need to declare a trait or class for the
sole purpose of combining traits. 
For the simple inheritance chain of this task,
any (or all) of the <code>class</code> keywords below can be replaced with <code>trait</code>


```scala
class Animal
class Dog extends Animal
class Cat extends Animal
class Lab extends Dog
class Collie extends Dog
```



## Seed7

[http://seed7.sourceforge.net/manual/objects.htm Seed7 object orientation] is based on interface types and implementation types.
The example below defines a hierarchy of implementation types.


```seed7
$ include "seed7_05.s7i";

const type: Animal is new struct
    # ... 
  end struct;
 
const type: Dog is sub Animal struct
    # ... 
  end struct;
 
const type: Lab is sub Dog struct
    # ...
  end struct;
 
const type: Collie is sub Dog struct
    # ...
  end struct;
 
const type: Cat is sub Animal struct
    # ...
  end struct;
```



## Self

Self is a class-free, object-oriented language, and as such, it uses prototypal inheritance instead of classical inheritance. This is an example of the relevant excerpts from a Self transporter fileout. Normally the object tree would be built and navigated within the graphical Self environment. 

```self
animal = ()
```


```self
dog = (| parent* = animal |)
```


```self
cat = (| parent* = animal |)
```


```self
lab = (| parent* = dog |)
```


```self
collie = (| parent* = dog |)
```




## Sidef


```ruby
class Animal {};
class Dog << Animal {};
class Cat << Animal {};
class Lab << Dog {};
class Collie << Dog {};
```



## Simula


```simula
begin

    class Animal;
        ! instance variables;
    begin
        ! methods;
    end;

    Animal class Dog;
    begin
    end;

    Animal class Cat;
    begin
    end;

    Dog class Lab;
    begin
    end;

    Dog class Collie;
    begin
    end;

end
```



## Slate

 

```slate
define: #Animal &parents: {Cloneable}.
define: #Dog &parents: {Animal}.
define: #Cat &parents: {Animal}.
define: #Lab &parents: {Dog}.
define: #Collie &parents: {Dog}.
```



## Smalltalk

This is an example of the object serialization format used by many varieties of Smalltalk. Normally the class tree would be defined and navigated via a class browser within a graphical Smalltalk environment.

```smalltalk
Object subclass: #Animal
  instanceVariableNames: ' ' "* space separated list of names *"
  classVariableNames: ' '
  poolDictionaries: ' '
  category: ' ' !

"* declare methods here, separated with '!' *"
"* !Animal methodsFor: 'a category'! *"
"* methodName *"
"*    method body! !"

!Animal subclass: #Dog
   "* etc. *" !

!Animal subclass: #Cat
  "* etc. *" !

!Dog subclass: #Lab
  "* etc. *" !

!Dog subclass: #Collie
  "* etc. *" !
```



## Swift


```swift
class Animal {
  // ... 
}

class Dog : Animal {
  // ... 
}

class Lab : Dog {
  // ... 
}

class Collie : Dog {
  // ... 
}

class Cat : Animal {
  // ... 
}
```



## Tcl

{{works with|Tcl|8.6}} or {{libheader|TclOO}}

```tcl
package require TclOO
oo::class create Animal {
   # ...
}
oo::class create Dog {
   superclass Animal
   # ...
}
oo::class create Cat {
   superclass Animal
   # ...
}
oo::class create Collie {
   superclass Dog
   # ...
}
oo::class create Lab {
   superclass Dog
   # ...
}
```



## TXR



### =Inheritance among symbolic exception tags=



```txr
@(defex cat animal)
@(defex lab dog animal)
@(defex collie dog)
```


The second line is a shorthand which defines a lab to be a kind of dog, and at the same time a dog to be a kind of animal.

If we throw an exception of type <code>lab</code>, it can be caught in a catch for a <code>dog</code> or for an <code>animal</code>. Continuing with the query:


```txr
@(try)
@  (throw lab "x")
@(catch animal (arg))
@(end)
```


{{out}} Test:

```txt
$ txr dog-cat.txr
arg="x"
```



### =OOP Inheritance in TXR Lisp=



```txrlisp
(defstruct animal nil
  name
  (:method get-name (me)
    (if me.name me.name (error `get-name: animal @me has no name`)))
  (:method speak (me stream)
    (error "abstract animal cannot speak")))

(defstruct dog animal
  (:method speak (me : (stream *stdout*))
    (put-line `@{me.(get-name)}: bark!` stream)))

(defstruct cat animal
  (:method speak (me : (stream *stdout*))
    (put-line `@{me.(get-name)}: meow!` stream)))

(defstruct lab dog)

(defstruct collie dog)

(let ((pet1 (new collie name "Lassie"))
      (pet2 (new cat name "Max")))
  pet1.(speak)
  pet2.(speak))
```


{{out}}


```txt
Lassie: bark!
Max: meow!
```



## Visual Basic .NET


```vbnet
Class Animal
  ' ...
End Class

Class Dog
  Inherits Animal
  ' ...
End Class

Class Lab
  Inherits Dog
  ' ...
End Class

Class Collie
  Inherits Dog
  ' ...
End Class

Class Cat
  Inherits Animal
  ' ...
End Class
```



## Vorpal


```vorpal
pet = new()
cat = new(pet)
dog = new(pet)
fido = new(dog)
felix = new(cat)
```



## XLISP


```lisp
(define-class animal)

(define-class dog
    (super-class animal))

(define-class cat
    (super-class animal))

(define-class collie
    (super-class dog))

(define-class lab
    (super-class dog))
```

A REPL session:

```lisp
[1] (cat 'superclass)

#<Class:ANIMAL #x57094c8>
[2] (collie 'superclass)

#<Class:DOG #x57094c8>
[3] (animal 'superclass)

#<Class:OBJECT #x57094c8>
[4] (dog 'show)

Object is #<Class:DOG #x57094c8>, Class is #<Class:CLASS #x57094c8>
Instance variables:
  NAME = DOG
  MESSAGES = ()
  IVARS = ()
  CVARS = #<Environment #x5879788>
  SUPERCLASS = #<Class:ANIMAL #x57094c8>
  IVARCNT = 0
  IVARTOTAL = 0
#<Class:DOG #x57094c8>
```



## zkl


```zkl
class Animal{}
class Dog(Animal){} class Cat(Animal){}
class Lab(Dog){} class Collie(Dog){}
Collie.linearizeParents
```

{{out}}

```txt

L(Class(Collie),Class(Dog),Class(Animal))

```


{{Omit From|ALGOL 68|It isn't immediately obvious that ALGOL 68 is object oriented.}}
{{Omit From|AWK}}
{{omit from|Axe}}
{{omit from|Batch File|Not an OO language.}}
{{omit from|M4}}
{{omit from|Mathematica}}
{{omit from|Maxima}}
{{Omit From|Metafont}}
{{omit from|MIPS Assembly}}
{{omit from|ML/I}}
{{omit from|Modula-2}}
{{omit from|PARI/GP}}
{{Omit From|TI-83 BASIC}}
{{Omit From|TI-89 BASIC}}
