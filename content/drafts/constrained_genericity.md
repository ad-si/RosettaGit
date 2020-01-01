+++
title = "Constrained genericity"
description = ""
date = 2019-05-30T18:49:49Z
aliases = []
[extra]
id = 3211
[taxonomies]
categories = []
tags = []
+++

{{task|Object oriented}} [[Category:Type System]]

'''Constrained genericity''' or '''bounded quantification''' means
that a parametrized type or function (see [[parametric polymorphism]])
can only be instantiated on types fulfilling some conditions,
even if those conditions are not used in that function.

Say a type is called "eatable" if you can call the function <tt>eat</tt> on it.
Write a generic type <tt>FoodBox</tt> which contains a collection of objects of
a type given as parameter, but can only be instantiated on eatable types.
The FoodBox shall not use the function eat in any way (i.e. without the explicit restriction, it could be instantiated on any type).
The specification of a type being eatable should be as generic as possible
in your language (i.e. the restrictions on the implementation of eatable types
should be as minimal as possible).
Also explain the restrictions, if any, on the implementation of eatable types,
and show at least one example of an eatable type.


## Ada

Ada allows various constraints to be specified in parameters of generics.
A formal type constrained to be derived from certain base is one of them:

```ada
with Ada.Containers.Indefinite_Vectors;

package Nutrition is
   type Food is interface;
   procedure Eat (Object : in out Food) is abstract;

end Nutrition;

with Ada.Containers;
with Nutrition;

generic
   type New_Food is new Nutrition.Food;
package Food_Boxes is

  package Food_Vectors is
      new Ada.Containers.Indefinite_Vectors
          (  Index_Type   => Positive,
             Element_Type => New_Food
          );

   subtype Food_Box is Food_Vectors.Vector;

end Food_Boxes;
```

The package Nutrition defines an interface of an eatable object, that is, the procedure Eat. Then a generic container package is defined with the elements to be of some type derived from Food. Example of use:

```ada
type Banana is new Food with null record;
overriding procedure Eat (Object : in out Banana) is null;
package Banana_Box is new Food_Boxes (Banana);

type Tomato is new Food with null record;
overriding procedure Eat (Object : in out Tomato) is null;
package Tomato_Box is new Food_Boxes (Tomato);
-- We have declared Banana and Tomato as a Food.
```

The Tomato_Box can only contain tomatoes; the Banana_Box can only contain bananas. You can only create boxes of eatable objects.


## C++

{{works with|C++11}}
Uses static assertion to disallow instantiations on incorrect types

```c++

template<typename T> //Detection helper struct
struct can_eat       //Detects presence of non-const member function void eat()
{
  private:
    template<typename U, void (U::*)()> struct SFINAE {};
    template<typename U> static char Test(SFINAE<U, &U::eat>*);
    template<typename U> static int Test(...);
  public:
    static constexpr bool value = sizeof(Test<T>(0)) == sizeof(char);
};

struct potato
{ void eat(); };

struct brick
{};

template<typename T>
class FoodBox
{
    //Using static assertion to prohibit non-edible types
    static_assert(can_eat<T>::value, "Only edible items are allowed in foodbox");

    //Rest of class definition
};

int main()
{
    FoodBox<potato> lunch;

    //Following leads to compile-time error
    //FoodBox<brick> practical_joke;
}
```


## C#
In C#, type constraints are made on the type hierarchy, so here we make <code>IEatable</code> an interface, with an <code>Eat</code> method.
Types which are eatable would have to implement the
<code>IEatable</code> interface and provide an <code>Eat</code> method.

```c#
interface IEatable
{
    void Eat();
}
```

Type constraints in type parameters can be made via the <code>where</code> keyword, which allows us to qualify T.
In this case, we indicate that the type argument must be a type
that is a subtype of <code>IEatable</code>.

```c#
using System.Collections.Generic;

class FoodBox<T> where T : IEatable
{
    List<T> food;
}
```

For example, an eatable Apple:

```c#
class Apple : IEatable
{
    public void Eat()
    {
        System.Console.WriteLine("Apple has been eaten");
    }
}
```

C# also has the interesting functionality of being able to require that a generic type have a default constructor. This means that the generic type can actually instantiate the objects without ever knowing the concrete type. To do so, we constrain the where clause with an additional term "new()". This must come after any other constraints. In this example, any type with a default constructor that implements IEatable is allowed.

```c#
using System.Collections.Generic

class FoodMakingBox<T> where T : IEatable, new()
{
    List<T> food;

    void Make(int numberOfFood)
    {
        this.food = new List<T>();
        for (int i = 0; i < numberOfFood; i++)
        {
            this.food.Add(new T());
        }
    }
}
```



## Common Lisp

The technique used here is like that in the [[Abstract type]] task.

The task says that this task is only for statically typed languages,
and Common Lisp is dynamically typed.
However, there are many places where type declarations can be provided
to the compiler, and there is user access to the type system
(e.g., a user can ask whether an object is of a particular type).
Via the latter mechanism, one could write a class containing a collection
such that the insert method checked that the object to be inserted
is of an appropriate type.

In this example, we define a class <code>food</code>, and two subclasses, <code>inedible-food</code> and <code>edible-food</code>.
We define a generic function <code>eat</code>,
and specialize it only for <code>edible-food</code>.
We then define a predicate <code>eatable-p</code> which returns true only on objects for which <code>eat</code> methods have been defined.
Then, using <code>[http://www.lispworks.com/documentation/HyperSpec/Body/m_deftp.htm deftype]</code> with a <code>[http://www.lispworks.com/documentation/HyperSpec/Body/t_satisf.htm satisfies]</code> type specifier, we define a type <code>eatable</code> to which only objects satisfying <code>eatable-p</code> belong.  Finally, we define a function <code>make-food-box</code> which takes, in addition to typical array creation arguments, a type specifier.
The array is declared to have elements of the type that is the intersection of <code>food</code> and the provided type.
<code>make-eatable-food-box</code> simply calls <code>make-food-box</code>
with the type <code>eatable</code>.

The only shortcoming here is that the compiler isn't required to enforce the type specifications for the arrays.  A custom insert function, however, could remember the specified type for the collection, and assert that inserted elements are of that type.

```lisp
(defclass food () ())

(defclass inedible-food (food) ())

(defclass edible-food (food) ())

(defgeneric eat (foodstuff)
  (:documentation "Eat the foodstuff."))

(defmethod eat ((foodstuff edible-food))
  "A specialized method for eating edible-food."
  (format nil "Eating ~w." foodstuff))

(defun eatable-p (thing)
  "Returns true if there are eat methods defined for thing."
  (not (endp (compute-applicable-methods #'eat (list thing)))))

(deftype eatable ()
  "Eatable objects are those satisfying eatable-p."
  '(satisfies eatable-p))

(defun make-food-box (extra-type &rest array-args)
  "Returns an array whose element-type is (and extra-type food).
array-args should be suitable for MAKE-ARRAY, and any provided
element-type keyword argument is ignored."
  (destructuring-bind (dimensions &rest array-args) array-args
    (apply 'make-array dimensions
           :element-type `(and ,extra-type food)
           array-args)))

(defun make-eatable-food-box (&rest array-args)
  "Return an array whose elements are declared to be of type (and
eatable food)."
  (apply 'make-food-box 'eatable array-args))
```



## Crystal

Similar to Ruby version, but shows error at compile-time.

```ruby
class Apple
  def eat
  end
end

class Carrot
  def eat
  end
end

class FoodBox(T)
  def initialize(@data : Array(T))
    {% if T.union? %}
    {% raise "All items should be eatable" unless T.union_types.all? &.has_method?(:eat) %}
    {% else %}
    {% raise "Items should be eatable" unless T.has_method?(:eat) %}
    {% end %}
  end
end

FoodBox.new([Apple.new, Apple.new])
FoodBox.new([Apple.new, Carrot.new])
FoodBox.new([Apple.new, Carrot.new, 123])
```


{{out}}

```txt

Error in line 23: All items should be eatable

```



## D


### Template Version


```d
enum IsEdible(T) = is(typeof(T.eat));

struct FoodBox(T) if (IsEdible!T) {
    T[] food;
    alias food this;
}

struct Carrot {
    void eat() {}
}

static struct Car {}

void main() {
    FoodBox!Carrot carrotsBox; // OK
    carrotsBox ~= Carrot();    // Adds a carrot

    //FoodBox!Car carsBox;     // Not allowed
}
```



### Interface Version


```d
interface IEdible { void eat(); }

struct FoodBox(T : IEdible) {
    T[] food;
    alias food this;
}

class Carrot : IEdible {
    void eat() {}
}

class Car {}

void main() {
    FoodBox!Carrot carrotBox; // OK
    //FoodBox!Car carBox;     // Not allowed
}
```



## E

It is surely arguable whether this constitutes an implementation
of the above task:

```e
/** Guard accepting only objects with an 'eat' method */
def Eatable {
    to coerce(specimen, ejector) {
        if (Ref.isNear(specimen) && specimen.__respondsTo("eat", 0)) {
            return specimen
        } else {
            throw.eject(ejector, `inedible: $specimen`)
        }
    }
}

def makeFoodBox() {
    return [].diverge(Eatable) # A guard-constrained list
}
```



## Eiffel


Eiffel has included support for constrained genericty since its earliest implementations (as shown in Bertrand Meyer's paper from OOPSLA '86, available [http://se.inf.ethz.ch/old/people/meyer/publications/acm/geninh.pdf online].)

The "eatable" characteristic is modeled by a deferred class (deferred classes are similar to abstract classes in some other languages).


```eiffel

deferred class
    EATABLE

feature -- Basic operations

    eat
            -- Eat this eatable substance
        deferred
        end
end

```


Class <code lang="eiffel">EATABLE</code> can then be inherited by any other class, with the understanding that the inheriting class will have to provide an implementation for the procedure <code lang="eiffel">eat</code>. Here are two such classes, <code lang="eiffel">APPLE</code> and <code lang="eiffel">PEAR</code>:


```eiffel

class
    APPLE

inherit
    EATABLE

feature -- Basic operations

    eat
            -- Consume
        do
            print ("One apple eaten%N")
        end
end

```




```eiffel

class
    PEAR

inherit
    EATABLE

feature -- Basic operations

    eat
            -- Consume
        do
            print ("One pear eaten%N")
        end
end

```


Instances of the generic class <code lang="eiffel">FOOD_BOX</code> can contain any types of <code lang="eiffel">EATABLE</code> items. The constraint is shown in the formal generics part of the class declaration for <code lang="eiffel">FOOD_BOX</code>:


```eiffel

class
    FOOD_BOX [G -> EATABLE]

inherit
    ARRAYED_LIST [G]

create
    make

end

```


So, any declaration of type <code lang="eiffel">FOOD_BOX</code> can constrain its contents to any particular eatable type. For example:


```eiffel

    my_apple_box: FOOD_BOX [APPLE]

```


The entity <code lang="eiffel">my_apple_box</code> is declared as a <code lang="eiffel">FOOD_BOX</code> which can contain only apples.

Of course, constraining a particular <code lang="eiffel">FOOD_BOX</code> to all types which are eatable is also allowed, and could be appropriate in certain cases, such as:


```eiffel

    my_refrigerator: FOOD_BOX [EATABLE]

```


Here's a small application that uses a <code lang="eiffel">FOOD_BOX</code> constrained to contain only apples:


```eiffel

class
    APPLICATION

create
    make

feature {NONE} -- Initialization

    make
            -- Run application.
        do
            create my_apple_box.make (10)
            create one_apple
            create one_pear
            my_apple_box.extend (one_apple)
--          my_apple_box.extend (one_pear)
            across
                my_apple_box as ic
            loop
                ic.item.eat
            end
        end

feature -- Access

    my_apple_box: FOOD_BOX [APPLE]
            -- My apple box

    one_apple: APPLE
            -- An apple

    one_pear: PEAR
            -- A pear
end

```


Notice that an instance of <code lang="eiffel">PEAR</code> is also created, and a line of code is present as a comment which would attempt to place the pear in the apple box:


```eiffel

--              my_apple_box.extend (one_pear)

```


If the comment mark "--" were removed from this line of code, an compile error would occur because of the attempt to violate <code lang="eiffel">my_apple_bos</code>'s constraint.

=={{header|F_Sharp|F#}}==
It is possible to constrain type parameters in a number of ways,
including inheritance relationships and interface implementation.
But for this task, the natural choice is an explicit member constraint.

```fsharp
type ^a FoodBox                         // a generic type FoodBox
  when ^a: (member eat: unit -> string) // with an explicit member constraint on ^a,
  (items:^a list) =                     // a one-argument constructor
  member inline x.foodItems = items     // and a public read-only property

// a class type that fullfills the member constraint
type Banana() =
  member x.eat() = "I'm eating a banana."

// an instance of a Banana FoodBox
let someBananas = FoodBox [Banana(); Banana()]
```



## Forth

{{works with|Forth}}
Works with any ANS Forth

Needs the FMS-SI (single inheritance) library code located here:
http://soton.mpeforth.com/flag/fms/index.html

```forth
include FMS-SI.f
include FMS-SILib.f
:class Eatable
   :m eat ." successful eat " ;m
;class

\ FoodBox is defined without inspecting for the eat message
:class FoodBox
  object-list eatable-types
  :m init: eatable-types init: ;m
  :m add: ( obj -- )
     dup is-kindOf Eatable
     if   eatable-types add:
     else drop ." not an eatable type "
     then ;m
  :m test
     begin eatable-types each:
     while eat
     repeat ;m
;class

FoodBox aFoodBox
Eatable aEatable
aEatable aFoodBox add:  \ add the e1 object to the object-list
aFoodBox test  \ => successful eat

:class brick
 :m eat cr ." successful eat " ;m
;class

brick abrick  \ create an object that is not eatable
abrick aFoodBox add: \ => not an eatable type

:class apple <super Eatable
;class

apple anapple
anapple aFoodBox add:
aFoodBox test  \ => successful eat successful eat

```



## Fortran

In Fortran all checkes are done at compile time, in particular a dummy argument has to conform class.

```fortran

module cg
    implicit none

    type, abstract :: eatable
    end type eatable

    type, extends(eatable) :: carrot_t
    end type carrot_t

    type :: brick_t; end type brick_t

    type :: foodbox
	class(eatable), allocatable :: food
    contains
        procedure, public :: add_item => add_item_fb
    end type foodbox

contains

    subroutine add_item_fb(this, f)
        class(foodbox), intent(inout) :: this
        class(eatable), intent(in)    :: f
        allocate(this%food, source=f)
    end subroutine add_item_fb
end module cg


program con_gen
    use cg
    implicit none

    type(carrot_t) :: carrot
    type(brick_t)  :: brick
    type(foodbox)  :: fbox

    ! Put a carrot into the foodbox
    call fbox%add_item(carrot)

    ! Try to put a brick in - results in a compiler error
    call fbox%add_item(brick)

end program con_gen


```

{{out}}
ifort -o cg cg.f90

```txt

cg.f90(40): error #6633: The type of the actual argument differs from the type of the dummy argument.   [BRICK]
    call fbox%add_item(brick)

```

gfortran -o cg cg.f90

```txt

cg.f90:41.23:

    call fbox%add_item(brick)
                       1
Error: Type mismatch in argument 'f' at (1); passed TYPE(brick_t) to CLASS(eatable)


```



## Go

Go's interfaces do exactly what this task wants.
Eatable looks like this:

```go
type eatable interface {
    eat()
}
```

And the following is all it takes to define foodbox as a slice of eatables.
The result is that an object of type foodbox can hold objects of any type that implements the eat method (with the function signature specified in eatable.)
The definition of foodbox though, doesn't even need to enumerate the functions of eatable, much less call them.  Whatever is in the interface is okay.

```go
type foodbox []eatable
```

Here is an example of an eatable type.

```go
type peelfirst string

func (f peelfirst) eat() {
    // peel code goes here
    fmt.Println("mm, that", f, "was good!")
}
```

The only thing it takes to make peelfirst eatable is the definition of the eat method.  When the eat method is defined, peelfirst automatically becomes an eatable.  We say it ''satisfies'' the interface.  Notice that "eatable" appears nowhere in the definition of peelfirst or the eat method of peelfirst.

Here is a complete program using these types.

```go
package main

import "fmt"

type eatable interface {
    eat()
}

type foodbox []eatable

type peelfirst string

func (f peelfirst) eat() {
    // peel code goes here
    fmt.Println("mm, that", f, "was good!")
}

func main() {
    fb := foodbox{peelfirst("banana"), peelfirst("mango")}
    f0 := fb[0]
    f0.eat()
}
```

{{out}}

```txt

mm, that banana was good!

```



## Haskell

A ''type class'' defines a set of operations that must be implemented by a type:

```haskell
class Eatable a where
  eat :: a -> String
```

We just require that instances of this type class implement a function <tt>eat</tt> which takes in the type and returns a string (I arbitrarily decided).

The <tt>FoodBox</tt> type could be implemented as follows:

```haskell
data (Eatable a) => FoodBox a = F [a]
```

The stuff before the <tt>=></tt> specify what type classes the type variable <tt>a</tt> must belong to.

We can create an instance of <tt>Eatable</tt> at any time by providing an implementation for the function <tt>eat</tt>. Here we define a new type <tt>Banana</tt>, and make it an instance of <tt>Eatable</tt>.

```haskell
data Banana = Foo -- the implementation doesn't really matter in this case
instance Eatable Banana where
  eat _ = "I'm eating a banana"
```

We can declare existing types to be instances in the exact same way. The following makes <tt>Double</tt> an eatable type:

```haskell
instance Eatable Double where
  eat d = "I'm eating " ++ show d
```

Another way to make an existing type eatable is to declare all instances of another type class instances of this one. Let's assume we have another type class <tt>Food</tt> which looks like this;

```haskell
class Food a where
  munch :: a -> String
```

Then we can make all instances of Food eatable using <tt>munch</tt> for <tt>eat</tt> with the following instance declaration:

```haskell
instance (Food a) => Eatable a where
  eat x = munch x
```


==Icon and {{header|Unicon}}==

Neither Icon nor Unicon are statically typed.
In Unicon, new types can be defined as classes.  The solution shown
follows the Scala approach.

```unicon
import Utils        # From the UniLib package to get the Class class.

class Eatable:Class()
end

class Fish:Eatable(name)
    method eat(); write("Eating "+name); end
end

class Rock:Class(name)
    method eat(); write("Eating "+name); end
end

class FoodBox(A)
initially
    every item := !A do if "Eatable" == item.Type() then next else bad := "yes"
    return /bad
end

procedure main()
    if FoodBox([Fish("salmon")]) then write("Edible") else write("Inedible")
    if FoodBox([Rock("granite")]) then write("Edible") else write("Inedible")
end
```


Sample run:

```txt

->cg
Edible
Inedible
->

```



## J

Implementation:

```j
coclass'Connoisseur'
isEdible=:3 :0
  0<nc<'eat__y'
)

coclass'FoodBox'
create=:3 :0
  collection=: 0#y
)
add=:3 :0"0
  'inedible' assert isEdible_Connoisseur_ y
  collection=: collection, y
  EMPTY
)
```

An edible type would be a class that has a verb with the name 'eat' (the task "eatable" requirement is checked on an object or class reference using the static method <code>isEdible_Connoisseur_</code>).

We have also defined a 'FoodBox' container class which can only contain edible objects. (Our add method returns returns an empty result since its purpose is to add to the container, not to produce a result.)

For example:

```j
coclass'Apple'
eat=:3 :0
  smoutput'delicious'
)
```

And here is a quicky demo of the above:

```j

   lunch=:'' conew 'FoodBox'
   a1=: conew 'Apple'
   a2=: conew 'Apple'
   add__lunch a1
   add__lunch a2
   george=: conew 'Connoisseur'
   add__lunch george
|inedible: assert
```



## Java

{{works with|Java|5}}
In Java type constraints are made on the type hierarchy, so here we make <code>Eatable</code> an interface, with an <code>eat</code> method.
Types which are Eatable would have to implement the
<code>Eatable</code> interface and provide an <code>eat</code> method.

```java5
interface Eatable
{
    void eat();
}
```

Type constraints in type parameters can be made via the <code>extends</code> keyword, indicating in this case that the type argument must be a type that is a subtype of <code>Eatable</code>.

```java5
import java.util.List;

class FoodBox<T extends Eatable>
{
    public List<T> food;
}
```

Similarly a generic method can constrain its type parameters

```java5>public <T extends Eatable
 void foo(T x) { }
// although in this case this is no more useful than just "public void foo(Eatable x)"
```

This <code>T</code> does not necessarily have to be defined in the class declaration. Another method may be declared like this:

```java5
public class Test{
   public <T extends Eatable> void bar(){ }
}
```

which has no indication of where <code>T</code> is coming from. This method could be called like this:

```java5>test.<EatableClass
bar();
```

The <code>foo</code> method from before can figure out <code>T</code> from its parameter, but this <code>bar</code> method needs to be told what T is.


## Julia

{{works with|Julia|0.6}}
Julia allows user defined types with inheritance. Misuse of a type generally produces a compile time error message.

```julia
abstract type Edible end
eat(::Edible) = "Yum!"

mutable struct FoodBox{T<:Edible}
    food::Vector{T}
end

struct Carrot <: Edible
    variety::AbstractString
end

struct Brick
    volume::Float64
end

c = Carrot("Baby")
b = Brick(125.0)
eat(c)
eat(b)
```


{{out}}

```txt
MethodError: no method matching eat(::Brick)
Closest candidates are:
  eat(!Matched::Edible) at console:2
```



## Kotlin

In the following program we define an interface, Eatable, and two classes - Cheese and Meat - which implement it and must therefore implement its eat() method because the interface itself does not provide a default implementation.

We then define a generic class, FoodBox, whose type parameter, T, is constrained to an Eatable type and instantiate it using both the Cheese and Meat types:

```scala
// version 1.0.6

interface Eatable {
    fun eat()
}

class Cheese(val name: String) : Eatable {
    override fun eat() {
       println("Eating $name")
    }

    override fun toString() = name
}

class Meat(val name: String) : Eatable {
    override fun eat() {
       println("Eating $name")
    }

    override fun toString() = name
}

class FoodBox<T: Eatable> {
    private val foodList =  mutableListOf<T>()

    fun add(food: T) {
        foodList.add(food)
    }

    override fun toString() = foodList.toString()
}

fun main(args: Array<String>) {
    val cheddar =  Cheese("cheddar")
    val feta = Cheese("feta")
    val cheeseBox = FoodBox<Cheese>()
    cheeseBox.add(cheddar)
    cheeseBox.add(feta)
    println("CheeseBox contains : $cheeseBox")

    val beef = Meat("beef")
    val ham = Meat("ham")
    val meatBox = FoodBox<Meat>()
    meatBox.add(beef)
    meatBox.add(ham)
    println("MeatBox contains : $meatBox")

    cheddar.eat()
    beef.eat()
    println("Full now!")
}
```


{{out}}

```txt

CheeseBox contains : [cheddar, feta]
MeatBox contains : [beef, ham]
Eating cheddar
Eating beef
Full now!

```



## Morfa

{{trans|D}}

### Template Version


```morfa
import morfa.type.traits;

template < T >
alias IsEdible = HasMember< T, "eat" >;

template < T >
if (IsEdible< T >)
struct FoodBox
{
    var food: T[];
}

struct Carrot
{
    func eat(): void {}
}

struct Car {}

func main(): void
{
    var carrotBox: FoodBox< Carrot >;   // OK
    carrotBox.food ~= Carrot();        // Adds a carrot

    // var carBox: FoodBox< Car >;      // Not allowed
    static assert( not trait(compiles, func() { var carBox: FoodBox< Car >; } ));
}
```




### Interface Version



```morfa
interface IEdible
{
    public func eat(): void;
}

template < T >
if (IsDerivedOf< T, IEdible >)
struct FoodBox
{
    var food: T[];
}

class Carrot: IEdible
{
    public override func eat(): void {}
}

class Car {}

func main(): void
{
    var carrotBox: FoodBox< Carrot >;   // OK

    // var carBox: FoodBox< Car >;      // Not allowed
    static assert( not trait(compiles, func() { var carBox: FoodBox< Car >; } ));
}
```



## Nemerle


```Nemerle
using System.Collections.Generic;

interface IEatable
{
    Eat() : void;
}

class FoodBox[T] : IEnumerable[T]
  where T : IEatable
{
    private _foods : list[T] = [];

    public this() {}

    public this(items : IEnumerable[T])
    {
        this._foods = $[food | food in items];
    }

    public Add(food : T) : FoodBox[T]
    {
        FoodBox(food::_foods);
    }

    public GetEnumerator() : IEnumerator[T]
    {
        _foods.GetEnumerator();
    }
}

class Apple : IEatable
{
    public this() {}

    public Eat() : void
    {
        System.Console.WriteLine("nom..nom..nom");
    }
}

mutable appleBox = FoodBox();
repeat(3) {
    appleBox = appleBox.Add(Apple());
}

foreach (apple in appleBox) apple.Eat();
```

{{out}}

```txt
nom..nom..nom
nom..nom..nom
nom..nom..nom
```



## Objeck

All generic 'T' types associated with the FoodBox must implement the 'Eatable' interface. Generic constrains may either be an interface or a sub-classed type.

```objeck
use Collection.Generic;

interface Eatable {
  method : virtual : Eat() ~ Nil;
}

class FoodBox<T : Eatable> {
  food : List<T>;
}

class Plum implements Eatable {
  method : Eat() ~ Nil {
    "Yummy Plum!"->PrintLine();
  }
}

class Genericity {
  function : Main(args : String[]) ~ Nil {
    plums : FoodBox<Plum>;
  }
}
```




## Nim


```nim
type
  Eatable = concept e
    eat(e)

  FoodBox[e: Eatable] = seq[e]

  Food = object
    name: string
    count: int

proc eat(x: int) = echo "Eating the int: ", x
proc eat(x: Food) = echo "Eating ", x.count, " ", x.name, "s"

var ints = FoodBox[int](@[1,2,3,4,5])
var fs = FoodBox[Food](@[])

fs.add Food(name: "Hamburger", count: 3)
fs.add Food(name: "Cheeseburger", count: 5)

for f in fs:
  eat(f)
```


=={{header|Objective-C}}==
{{works with|Xcode|7}}
Type constraints are made on the type hierarchy, so here we make <code>Eatable</code> a protocol, with an <code>eat</code> method.
Types which are Eatable would have to implement the
<code>Eatable</code> protocol and provide an <code>eat</code> method.

```objc
@protocol Eatable
- (void)eat;
@end
```

Type constraints in type parameters can be made via the <code>:</code> keyword, indicating in this case that the type argument must be a type that is a subtype of <code>id<Eatable></code>.

```objc>@interface FoodBox<T : id<Eatable>
 : NSObject
@end
```



## OCaml

OCaml handles type constraints through ''modules'' and ''module types''.

A module type defines a set of operations that must be implemented by a module:

```ocaml
module type Eatable = sig
  type t
  val eat : t -> unit
end
```

We just require that module instances of this module type describe a type <tt>t</tt> and implement a function <tt>eat</tt> which takes in the type and returns nothing.

The <tt>FoodBox</tt> generic type could be implemented as a ''functor''
(something which takes a module as an argument and returns another module):

```ocaml
module MakeFoodBox(A : Eatable) = struct
  type elt = A.t
  type t = F of elt list
  let make_box_from_list xs = F xs
end
```


We can create a module that is an instance of <tt>Eatable</tt> by specifying a type providing an implementation for the function <tt>eat</tt>. Here we define a module <tt>Banana</tt>, and make it an instance of <tt>Eatable</tt>.

```ocaml
type banana = Foo (* a dummy type *)

module Banana : Eatable with type t = banana = struct
  type t = banana
  let eat _ = print_endline "I'm eating a banana"
end
```


We can also create modules that use an existing type as its <code>t</code>. The following module uses <tt>float</tt> as its type:

```ocaml
module EatFloat : Eatable with type t = float = struct
  type t = float
  let eat f = Printf.printf "I'm eating %f\n%!" f
end
```

Then, to make a FoodBox out of one of these modules, we need to call the functor on the module that specifies the type parameter:

```ocaml
module BananaBox = MakeFoodBox (Banana)
module FloatBox = MakeFoodBox (EatFloat)

let my_box = BananaBox.make_box_from_list [Foo]
let your_box = FloatBox.make_box_from_list [2.3; 4.5]
```

Unfortunately, it is kind of cumbersome in that, for every type parameter
we want to use for this generic type, we will have to explicitly create a module
for the resulting type (i.e. <tt>BananaBox</tt>, <tt>FloatBox</tt>).
And the operations on that resulting type (i.e. <tt>make_box_from_list</tt>)
are tied to each specific module.


## ooRexx

ooRexx methods, routines, and collections are all untyped,
so there are no language-level checks for type matches.
Tests for identity need to be performed at runtime using mechanisms
such as the object isA method.

```ooRexx
call dinnerTime "yogurt"
call dinnerTime .pizza~new
call dinnerTime .broccoli~new


-- a mixin class that defines the interface for being "food", and
-- thus expected to implement an "eat" method
::class food mixinclass object
::method eat abstract

::class pizza subclass food
::method eat
  Say "mmmmmmmm, pizza".

-- mixin classes can also be used for multiple inheritance
::class broccoli inherit food
::method eat
  Say "ugh, do I have to?".

::routine dinnerTime
  use arg dish
  -- ooRexx arguments are typeless, so tests for constrained
  -- types must be peformed at run time.  The isA method will
  -- check if an object is of the required type
  if \dish~isA(.food) then do
     say "I can't eat that!"
     return
  end
  else dish~eat
```

{{out}}

```txt
I can't eat that!
mmmmmmmm, pizza.
ugh, do I have to?.
```



## OxygenBasic

Generic but not too generic I trust.

```oxygenbasic

macro Gluttony(vartype, capacity, foodlist)
'
### ====================================


typedef vartype physical

enum food foodlist

type ActualFood
  sys      name
  physical size
  physical quantity
end type

Class foodbox
'
### ======

has ActualFood Item[capacity]
sys max

method put(sys f, physical s,q)
  max++
  Item[max]<=f,s,q
end method

method GetNext(ActualFood *Stuff)
  if max then
    copy @stuff,@Item[max], sizeof Item
    max--
  end if
end method

end class

Class Gourmand
'
### =======

physical WeightGain, SleepTime

method eats(ActualFood *stuff)
  WeightGain+=stuff.size*stuff.quantity*0.75
  stuff.size=0
  stuff.quantity=0
end method

end class

end macro


'IMPLEMENTATION
'
### ========



Gluttony (
double,100,{
oyster,trout,bloater,
chocolate,truffles,
cheesecake,cream,pudding,pie
})

% small  1
% medium 2
% large  3
% huge   7

% none    0
% single  1
% few     3
% several 7
% many    12

'INSTANCE
'
### ==


FoodBox  Hamper
Gourmand MrG

'TEST
'====

Hamper.put food.pudding,large,several
Hamper.put food.pie,huge,few
ActualFood Course
Hamper.GetNext Course
MrG.eats Course

print MrG.WeightGain 'result 15.75

```



## Perl 6

{{works with|Rakudo|2016.01}}


```perl6
subset Eatable of Any where { .^can('eat') };

class Cake { method eat() {...} }

role FoodBox[Eatable] {
    has %.foodbox;
}

class Yummy does FoodBox[Cake] { }      # composes correctly
# class Yucky does FoodBox[Int] { }     # fails to compose

my Yummy $foodbox .= new;
say $foodbox.perl;
```

{{out}}
<lang>Yummy.new(foodbox => {})
```



## PicoLisp


```PicoLisp
(class +Eatable)

(dm eat> ()
   (prinl "I'm eatable") )


(class +FoodBox)
# obj

(dm set> (Obj)
   (unless (method 'eat> Obj)                # Check if the object is eatable
      (quit "Object is not eatable" Obj) )
   (=: obj Obj) )                            # If so, set the object


(let (Box (new '(+FoodBox))  Eat (new '(+Eatable))  NoEat (new '(+Bla)))
   (set> Box Eat)       # Works
   (set> Box NoEat) )   # Gives an error
```

{{out}}

```txt
$384320489 -- Object is not eatable

? (show Box)
$384320487 (+FoodBox)
   obj $384320488

? (show Box 'obj)
$384320488 (+Eatable)

? (show NoEat)
$384320489 (+Bla)
```



## Racket


<code>edible&lt;%></code> objects simply need to state that they implement
the interface in the second argument to <code>class*</code>.
By doing so they will be forced to implement <code>eat</code>.


```racket
#lang racket
(module+ test (require tests/eli-tester))

;; This is all that an object should need to properly implement.
(define edible<%>
  (interface () [eat (->m void?)]))

(define (generic-container<%> containee/c)
  (interface ()
    [contents  (->m (listof containee/c))]
    [insert    (->m containee/c void?)]
    [remove-at (->m exact-nonnegative-integer? containee/c)]
    [count     (->m exact-nonnegative-integer?)]))

(define ((generic-box-mixin containee/c) %)
  (->i ([containee/c contract?])
       (rv (containee/c) (implementation?/c (generic-container<%> containee/c))))
  (class* % ((generic-container<%> containee/c))
    (super-new)
    (define l empty)
    (define/public (contents) l)
    (define/public (insert o) (set! l (cons o l)))
    (define/public (remove-at i)
      (begin0 (list-ref l i)
              (append (take l i) (drop l (add1 i)))))
    (define/public (count) (length l))))

;; As I understand it, a "Food Box" from the task is still a generic... i.e.
;; you will specify it down ;; to an "apple-box%" so: food-box%-generic is still
;; generic. food-box% will take any kind of food.
(define/contract (food-box-mixin T%)
  (-> (or/c (λ (i) (eq? edible<%> i)) (implementation?/c edible<%>))
   (make-mixin-contract))
  (generic-box-mixin (and/c (is-a?/c edible<%>) (is-a?/c T%))))

(module+ test

  (define integer-box% ((generic-box-mixin integer?) object%))
  (define integer-box  (new integer-box%))

  (define apple%
    (class* object% (edible<%>)
      (super-new)
      (define/public (eat)
        (displayln "nom!"))))

  (define banana%
    (class* object% (edible<%>)
      (super-new)
      (define/public (eat)
        (displayln "peel.. peel... nom... nom!"))))

  (define semolina%
    (class* object% () ; <-- empty interfaces clause
      (super-new)
      ;; you can call eat on it... but it's not explicitly (or even vaguely)
      ;; edible<%>
      (define/public (eat) (displayln "blech!"))))

  ;; this will take any object that is edible<%> and edible<%> (therefore all
  ;; edible<%> objects)
  (define any-food-box (new ((food-box-mixin edible<%>) object%)))

  ;; this will take any object that is edible and an apple<%>
  ;; (therefore only apple<%>s)
  (define apple-food-box (new ((food-box-mixin apple%) object%)))

  (test
   ;; Test generic boxes
   (send integer-box insert 22)
   (send integer-box insert "a string") =error> exn:fail:contract?

   ;; Test the food box that takes any edible<%>
   (send any-food-box insert (new apple%))
   (send any-food-box insert (new banana%))
   (send any-food-box insert (new semolina%)) =error> exn:fail:contract?

   ;; Test the food box that takes any apple%
   (send apple-food-box insert (new apple%))
   (send apple-food-box insert (new banana%)) =error> exn:fail:contract?
   (send apple-food-box insert (new semolina%)) =error> exn:fail:contract?
   (send apple-food-box count) => 1

   ;; Show that you cannot make a food-box from the non-edible<%> semolina cannot
   (implementation? semolina% edible<%>) => #f
   (new ((food-box-mixin semolina%) object%)) =error> exn:fail:contract?))
```


All the tests pass. Look at the tests to see what generates an exception (i.e.
not allowed at runtime) and what does not.


## Ruby


```ruby
class Foodbox
  def initialize (*food)
    raise ArgumentError, "food must be eadible" unless  food.all?{|f| f.respond_to?(:eat)}
    @box = food
  end
end

class Fruit
  def eat; end
end

class Apple < Fruit; end

p Foodbox.new(Fruit.new, Apple.new)
# => #<Foodbox:0x00000001420c88 @box=[#<Fruit:0x00000001420cd8>, #<Apple:0x00000001420cb0>]>

p Foodbox.new(Apple.new, "string can't eat")
# => test1.rb:3:in `initialize': food must be eadible (ArgumentError)

```



## Rust


```rust

// This declares the "Eatable" constraint. It could contain no function.
trait Eatable {
    fn eat();
}

// This declares the generic "FoodBox" type,
// whose parameter must satisfy the "Eatable" constraint.
// The objects of this type contain a vector of eatable objects.
struct FoodBox<T: Eatable> {
    _data: Vec<T>,
}

// This implements the functions associated with the "FoodBox" type.
// This statement is not required, but here it is used
// to declare a handy "new" constructor.
impl<T: Eatable> FoodBox<T> {
    fn new() -> FoodBox<T> {
        FoodBox::<T> { _data: Vec::<T>::new() }
    }
}

// This declares a simple type.
struct Banana {}

// This makes the "Banana" type satisfy the "Eatable" constraint.
// For that, every declaration inside the declaration of "Eatable"
// must be implemented here.
impl Eatable for Banana {
    fn eat() {}
}

// This makes also the primitive "char" type satisfy the "Eatable" constraint.
impl Eatable for char {
    fn eat() {}
}

fn main() {
    // This instantiate a "FoodBox" parameterized by the "Banana" type.
    // It is allowed as "Banana" implements "Eatable".
    let _fb1 = FoodBox::<Banana>::new();

    // This instantiate a "FoodBox" parameterized by the "char" type.
    // It is allowed, as "char" implements "Eatable".
    let _fb2 = FoodBox::<char>::new();

    // This instantiate a "FoodBox" parameterized by the "bool" type.
    // It is NOT allowed, as "bool" does not implement "Eatable".
    //let _fb3 = FoodBox::<bool>::new();
}

```



## Sather


```sather
abstract class $EDIBLE is
  eat;
end;

class FOOD < $EDIBLE is
  readonly attr name:STR;
  eat is
    #OUT + "eating " + self.name + "\n";
  end;
  create(name:STR):SAME is
    res ::= new;
    res.name := name;
    return res;
  end;
end;

class CAR is
  readonly attr name:STR;
  create(name:STR):SAME is
    res ::= new;
    res.name := name;
    return res;
  end;
end;

class FOODBOX{T < $EDIBLE} is
  private attr list:LLIST{T};
  create:SAME is
    res ::= new;
    res.list := #;
    return res;
  end;
  add(c :T) is
    self.list.insert_back(c);
  end;
  elt!:T is loop yield self.list.elt!; end; end;
end;

class MAIN is
  main is
    box  ::= #FOODBOX{FOOD}; -- ok
    box.add(#FOOD("Banana"));
    box.add(#FOOD("Amanita Muscaria"));

    box2 ::= #FOODBOX{CAR};  -- not ok
    box2.add(#CAR("Punto")); -- but compiler let it pass!

    -- eat everything
    loop box.elt!.eat; end;
  end;
end;
```

The GNU Sather compiler v1.2.3 let the "box2" pass, even though it shouldn't. Read e.g. [http://www.gnu.org/software/sather/docs-1.2/tutorial/parameterized1751.html this tutorial's section]


## Scala

Scala can constrain types in many different ways.
This specific constrain, for the type to contain a particular method,
can be written as this:

```scala
type Eatable = { def eat: Unit }

class FoodBox(coll: List[Eatable])

case class Fish(name: String) {
  def eat {
    println("Eating "+name)
  }
}

val foodBox = new FoodBox(List(new Fish("salmon")))
```

A more extensive discussion on genericity in Scala and some of the constrains
that can be imposed can be found on [[Parametric Polymorphism]].


## Sidef


```ruby
class FoodBox(*food { .all { .respond_to(:eat) } }) { }

class Fruit { method eat { ... } }
class Apple < Fruit {  }

say FoodBox(Fruit(), Apple()).dump  #=> FoodBox(food: [Fruit(), Apple()])
say FoodBox(Apple(), "foo")         #!> ERROR: class `FoodBox` !~ (Apple, String)
```



## Swift

Here we make <code>Eatable</code> a protocol, with an <code>eat</code> method.
Types which are Eatable would have to conform to the <code>Eatable</code> protocol
and provide an <code>eat</code> method.

```swift
protocol Eatable {
    func eat()
}
```

Type constraints in type parameters can be made via the <code>:</code> syntax, indicating in this case that the type argument must be a type that is
a subtype of <code>Eatable</code>.

```swift>struct FoodBox<T: Eatable
 {
    var food: [T]
}
```

Similarly a generic function or method can constrain its type parameters

```swift>func foo<T: Eatable
(x: T) { }
// although in this case this is no more useful than just "func foo(x: Eatable)"
```



## zkl

zkl isn't statically typed so the test is done at runtime.

This is a slightly different take on the task, keeping the editables and rejecting the garbage.

```zkl
class Eatable{ var v;
   fcn eat{ println("munching ",self.topdog.name); }
}
class FoodBox{
   fcn init(food1,food2,etc){
      editable,garbage:=vm.arglist.filter22("isChildOf",Eatable);
      var contents=editable;
      if(garbage) println("Rejecting: ",garbage);
   }
}
```


```zkl
class Apple(Eatable){} class Nuts(Eatable){} class Foo{}
FoodBox(Apple,"boogers",Nuts,Foo).contents.apply2("eat");
```

{{out}}

```txt

Rejecting: L("boogers",Class(Foo))
munching Apple
munching Nuts

```


<!-- Place omit from templates below here -->
{{omit from|ALGOL 68|it isn't immediately obvious that ALGOL 68 is object oriented}}
{{omit from|BBC BASIC}}
{{omit from|C|type system doesn't support this}}
{{omit from|J|not statically typed}}
{{omit from|JavaScript}}
{{omit from|Oz}}
{{omit from|PARI/GP}}
{{omit from|Mathematica}}
{{omit from|Maxima}}
{{omit from|Perl|not statically typed}}
{{omit from|Python|not statically typed}}
{{omit from|Ruby|not statically typed}}
{{omit from|Tcl|not statically typed}}
{{omit from|TI-89 BASIC|not statically typed, nor are user-defined types supported}}
{{omit from|Factor}}
{{omit from|Lua|not statically typed}}
{{omit from|Io|not statically typed}}
{{omit from|Scheme|not statically typed}}
{{omit from|Clojure|not statically typed}}
{{omit from|ZX Spectrum Basic|Does not support user defined datatypes}}
