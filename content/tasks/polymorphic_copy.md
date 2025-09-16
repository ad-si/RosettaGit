+++
title = "Polymorphic copy"
description = ""
date = 2019-10-22T03:39:08Z
aliases = []
[extra]
id = 3094
[taxonomies]
categories = ["task", "Object oriented"]
tags = []
languages = [
  "ada",
  "aikido",
  "bbc_basic",
  "c",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "e",
  "echolisp",
  "elena",
  "factor",
  "forth",
  "fortran",
  "go",
  "groovy",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "miniscript",
  "netrexx",
  "nim",
  "ocaml",
  "oorexx",
  "oxygenbasic",
  "oz",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "python",
  "racket",
  "rexx",
  "ruby",
  "scala",
  "sidef",
  "slate",
  "swift",
  "tcl",
]
+++

## Task

An object is [[polymorphism|polymorphic]] when its specific type may vary.
The types a specific value may take, is called ''class''.

It is trivial to copy an object if its type is known:

```c
int x;
int y = x;
```

Here x is not polymorphic, so y is declared of same type (''int'') as x.
But if the specific type of x were unknown, then y could not be declared of any specific type.

The task: let a polymorphic object contain an instance of some specific type S derived from a type T.
The type T is known.
The type S is possibly unknown until [[run time]].
The objective is to create an exact copy of such polymorphic object (not to create a [[reference]], nor a pointer to).
Let further the type T have a method overridden by S.
This method is to be called on the copy to demonstrate that the specific type of the copy is indeed S.


## Ada


```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Polymorphic_Copy is
   package Base is
      type T is tagged null record;
      type T_ptr is access all T'Class;
      function Name (X : T) return String;
   end Base;
   use Base;

   package body Base is
      function Name (X : T) return String is
      begin
         return "T";
      end Name;
   end Base;

      -- The procedure knows nothing about S
   procedure Copier (X : T'Class) is
      Duplicate : T'Class := X;  -- A copy of X
   begin
      Put_Line ("Copied " & Duplicate.Name); -- Check the copy
   end Copier;

      -- The function knows nothing about S and creates a copy on the heap
   function Clone (X : T'Class) return T_ptr is
   begin
      return new T'Class(X);
   end Copier;

   package Derived is
      type S is new T with null record;
      overriding function Name (X : S) return String;
   end Derived;
   use Derived;

   package body Derived is
      function Name (X : S) return String is
      begin
         return "S";
      end Name;
   end Derived;

   Object_1 : T;
   Object_2 : S;
   Object_3 : T_ptr := Clone(T);
   Object_4 : T_ptr := Clone(S);
begin
   Copier (Object_1);
   Copier (Object_2);
   Put_Line ("Cloned " & Object_3.all.Name);
   Put_Line ("Cloned " & Object_4.all.Name);
end Test_Polymorphic_Copy;
```

The procedure Copier does not know the specific type of its argument.
Nevertheless it creates an object Duplicate of exactly same type.
```txt

Copied T
Copied S
Cloned T
Cloned S

```



## Aikido

Aikido has a native <code>clone</code> function that creates a (deep or shallow) clone of any variable of any type.

```aikido

class T {
    public function print {
        println ("class T")
    }
}

class S extends T {
    public function print {
        println ("class S")
    }
}

var t = new T()
var s = new S()
println ("before copy")
t.print()
s.print()

var tcopy = clone (t, false)
var scopy = clone (s, false)
println ("after copy")
tcopy.print()
scopy.print()


```

 before copy
 class T
 class S
 after copy
 class T
 class S


## BBC BASIC

```bbcbasic
      INSTALL @lib$ + "CLASSLIB"

      REM Create parent class T:
      DIM classT{array#(0), setval, retval}
      DEF classT.setval (n%,v) classT.array#(n%) = v : ENDPROC
      DEF classT.retval (n%) = classT.array#(n%)
      PROC_class(classT{})

      REM Create class S derived from T, known only at run-time:
      RunTimeSize% = RND(100)
      DIM classS{array#(RunTimeSize%)}
      PROC_inherit(classS{}, classT{})
      DEF classS.retval (n%) = classS.array#(n%) ^ 2 : REM Overridden method
      PROC_class(classS{})

      REM Create an instance of class S:
      PROC_new(myobject{}, classS{})

      REM Now make a copy of the instance:
      DIM mycopy{} = myobject{}
      mycopy{} = myobject{}
      PROC_discard(myobject{})

      REM Test the copy (should print 123^2):
      PROC(mycopy.setval)(RunTimeSize%, 123)
      result% = FN(mycopy.retval)(RunTimeSize%)
      PRINT result%
      END
```

```txt

     15129

```



## C

Since C doesn't support classes, this is not quite so trivial.
Normally the code below would be split into a number of files.

Specificially there would be a header (.h) file and a source (.c) file for each of - BaseObj, Dog, and Ferret.

The code in "main" would also be a separate source file which would include Dog.h and Ferret.h header files (which would thems elves include the BaseObj.h header file.)
A better example of object oriented support in 'C' can be found in the source for the XtIntrinsics library of X11.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct object *BaseObj;
typedef struct sclass *Class;
typedef void (*CloneFctn)(BaseObj s, BaseObj clo);
typedef const char * (*SpeakFctn)(BaseObj s);
typedef void (*DestroyFctn)(BaseObj s);

typedef struct sclass {
    size_t csize;		/* size of the class instance */
    const char  *cname;		/* name of the class */
    Class  parent;		/* parent class */

    CloneFctn clone;		/* clone function */
    SpeakFctn speak;		/* speak function */
    DestroyFctn del;		/* delete the object */
} sClass;

typedef struct object {
    Class class;
} SObject;

static
BaseObj obj_copy( BaseObj s, Class c )
{
    BaseObj clo;
    if (c->parent)
        clo = obj_copy( s, c->parent);
    else
        clo = malloc( s->class->csize );

    if (clo)
        c->clone( s, clo );
    return clo;
}

static
void obj_del( BaseObj s, Class c )
{
    if (c->del)
        c->del(s);
    if (c->parent)
        obj_del( s, c->parent);
    else
        free(s);
}

BaseObj ObjClone( BaseObj s )
{ return obj_copy( s, s->class ); }

const char * ObjSpeak( BaseObj s )
{
    return s->class->speak(s);
}

void ObjDestroy( BaseObj s )
{ if (s) obj_del( s, s->class ); }

/* * * * * * */
static
void baseClone( BaseObj s, BaseObj clone)
{
    clone->class = s->class;
}

static
const char *baseSpeak(BaseObj s)
{
    return "Hello, I'm base object";
}

sClass boc = { sizeof(SObject), "BaseObj", NULL,
    &baseClone, &baseSpeak, NULL };
Class BaseObjClass = &boc;

/* * * * * * */
/* Dog - a derived class */

typedef struct sDogPart {
    double weight;
    char color[32];
    char name[24];
} DogPart;

typedef struct sDog *Dog;

struct sDog {
    Class   class;		// parent structure
    DogPart dog;
};

static
void dogClone( BaseObj s, BaseObj c)
{
    Dog src = (Dog)s;
    Dog clone = (Dog)c;
    clone->dog = src->dog;	/* no pointers so strncpys not needed */
}

static
const char *dogSpeak( BaseObj s)
{
    Dog d = (Dog)s;
    static char  response[90];
    sprintf(response, "woof! woof! My name is %s. I'm a %s %s",
            d->dog.name, d->dog.color, d->class->cname);
    return response;
}


sClass dogc = { sizeof(struct sDog), "Dog", &boc,
    &dogClone, &dogSpeak, NULL };
Class DogClass = &dogc;

BaseObj NewDog( const char *name, const char *color, double weight )
{
    Dog dog = malloc(DogClass->csize);
    if (dog) {
        DogPart *dogp = &dog->dog;
        dog->class = DogClass;
        dogp->weight = weight;
        strncpy(dogp->name, name, 23);
        strncpy(dogp->color, color, 31);
    }
    return (BaseObj)dog;
}

/* * * * * * * * * */
/* Ferret - a derived class */

typedef struct sFerretPart {
    char color[32];
    char name[24];
    int  age;
} FerretPart;

typedef struct sFerret *Ferret;

struct sFerret {
    Class   class;		// parent structure
    FerretPart ferret;
};

static
void ferretClone( BaseObj s, BaseObj c)
{
    Ferret src = (Ferret)s;
    Ferret clone = (Ferret)c;
    clone->ferret = src->ferret;  /* no pointers so strncpys not needed */
}

static
const char *ferretSpeak(BaseObj s)
{
    Ferret f = (Ferret)s;
    static char  response[90];
    sprintf(response, "My name is %s. I'm a %d mo. old %s wiley %s",
            f->ferret.name, f->ferret.age, f->ferret.color,
            f->class->cname);
    return response;
}

sClass ferretc = { sizeof(struct sFerret), "Ferret", &boc,
    &ferretClone, &ferretSpeak, NULL };
Class FerretClass = &ferretc;

BaseObj NewFerret( const char *name, const char *color, int age )
{
    Ferret ferret = malloc(FerretClass->csize);
    if (ferret) {
        FerretPart *ferretp = &(ferret->ferret);
        ferret->class = FerretClass;
        strncpy(ferretp->name, name, 23);
        strncpy(ferretp->color, color, 31);
        ferretp->age = age;
    }
    return (BaseObj)ferret;
}

/* * Now you really understand why Bjarne created C++ * */

int main()
{
    BaseObj  o1;
    BaseObj  kara = NewFerret( "Kara", "grey", 15 );
    BaseObj  bruce = NewDog("Bruce", "yellow", 85.0 );
    printf("Ok created things\n");

    o1 = ObjClone(kara );
    printf("Karol says %s\n", ObjSpeak(o1));
    printf("Kara says %s\n", ObjSpeak(kara));
    ObjDestroy(o1);

    o1 = ObjClone(bruce );
    strncpy(((Dog)o1)->dog.name, "Donald", 23);
    printf("Don says %s\n", ObjSpeak(o1));
    printf("Bruce says %s\n", ObjSpeak(bruce));
    ObjDestroy(o1);
    return 0;
}
```


## C#

```c#
using System;

class T
{
    public virtual string Name()
    {
        return "T";
    }

    public virtual T Clone()
    {
        return new T();
    }
}

class S : T
{
    public override string Name()
    {
        return "S";
    }

    public override T Clone()
    {
        return new S();
    }
}

class Program
{
    static void Main()
    {
        T original = new S();
        T clone = original.Clone();

        Console.WriteLine(original.Name());
        Console.WriteLine(clone.Name());
    }
}
```

<lang>S
S
```



## C++


```cpp
#include <iostream>

class T
{
public:
  virtual void identify() { std::cout << "I am a genuine T" << std::endl; }
  virtual T* clone() { return new T(*this); }
  virtual ~T() {}
};

class S: public T
{
public:
  virtual void identify() { std::cout << "I am an S" << std::endl; }
  virtual S* clone() { return new S(*this); }
};

class X // the class of the object which contains a T or S
{
public:
  // by getting the object through a pointer to T, X cannot know if it's an S or a T
  X(T* t): member(t) {}

  // copy constructor
  X(X const& other): member(other.member->clone()) {}

  // copy assignment operator
  X& operator=(X const& other)
  {
    T* new_member = other.member->clone();
    delete member;
    member = new_member;
  }

  // destructor
  ~X() { delete member; }

  // check what sort of object it contains
  void identify_member() { member->identify(); }

private:
  T* member;
};

int main()
{
  X original(new S);      // construct an X and give it an S,
  X copy = original;      // copy it,
  copy.identify_member(); // and check what type of member it contains
}
```



## Common Lisp


### With Structures

With structures, <code>[http://www.lispworks.com/documentation/HyperSpec/Body/f_cp_stu.htm copy-structure]</code> performs the right kind of copy.  The object and its copy are compared under <code>eq</code>, <code>eql</code>, <code>equal</code>, and <code>equalp</code> to demonstrate that "The objective is to create an exact copy of such polymorphic object (not to create a reference, nor a pointer to)."


```lisp
(defstruct super foo)

(defstruct (sub (:include super)) bar)

(defgeneric frob (thing))

(defmethod frob ((super super))
  (format t "~&Super has foo = ~w." (super-foo super)))

(defmethod frob ((sub sub))
  (format t "~&Sub has foo = ~w, bar = ~w."
          (sub-foo sub) (sub-bar sub)))
```



```txt
> (let* ((sub1 (make-sub :foo 'foo :bar 'bar))
         (sub2 (copy-structure sub1)))
    (frob sub1)
    (frob sub2)
    (format t "~&eq: ~w;  eql: ~w;  equal: ~w;  equalp: ~w"
            (eq sub1 sub2) (eql sub1 sub2)
            (equal sub1 sub2) (equalp sub1 sub2)))
Sub has foo = FOO, bar = BAR.
Sub has foo = FOO, bar = BAR.
eq: NIL;  eql: NIL;  equal: NIL;  equalp: T
NIL
```



### With Sequences


The same technique works for <code>[http://www.lispworks.com/documentation/HyperSpec/Body/t_seq.htm sequence]</code> and its subclasses (e.g., <code>[http://www.lispworks.com/documentation/HyperSpec/Body/t_string.htm string]</code>, <code>[http://www.lispworks.com/documentation/HyperSpec/Body/t_seq.htm list]</code>) when <code>[http://www.lispworks.com/documentation/HyperSpec/Body/f_cp_seq.htm copy-seq]</code> is used rather than <code>copy-structure</code>.


```lisp
(defmethod frob ((sequence sequence))
  (format t "~&sequence has ~w elements" (length sequence)))

(defmethod frob ((string string))
  (format t "~&the string has ~w elements" (length string)))
```



```txt
> (let* ((hw1 "hello world")
         (hw2 (copy-seq hw1)))
    (frob hw1)
    (frob hw2)
    (format t "~&eq: ~w;  eql: ~w;  equal: ~w;  equalp: ~w"
            (eq hw1 hw2) (eql hw1 hw2)
            (equal hw1 hw2) (equalp hw1 hw2)))
the string has 11 elements
the string has 11 elements
eq: NIL;  eql: NIL;  equal: T;  equalp: T
NIL
```



## D


If we assume there are no data members, this will be quite short and simple:

```d
class T {
    override string toString() { return "I'm the instance of T"; }
    T duplicate() { return new T; }
}

class S : T {
    override string toString() { return "I'm the instance of S"; }

    override T duplicate() { return new S; }
}

void main () {
    import std.stdio;
    T orig = new S;
    T copy = orig.duplicate();

    writeln(orig);
    writeln(copy);
}
```

```txt
I'm the instance of S
I'm the instance of S
```


However this doesn't happen often in reality. If we want to copy data fields we should have something like copy constructor, that will
do the deep copy.

```d
class T {
    this(T t = null) {} // Constructor that will be used for copying.

    override string toString() { return "I'm the instance of T"; }

    T duplicate() { return new T(this); }

    bool custom(char c) { return false; }
}

class S : T {
    char[] str;

    this(S s = null) {
        super(s);
        if (s is null)
            str = ['1', '2', '3']; // All newly created will get that.
        else
            str = s.str.dup; // Do the deep-copy.
    }

    override string toString() {
        return "I'm the instance of S p: " ~ str.idup;
    }

    override T duplicate() { return new S(this); }

    // Additional procedure, just to test deep-copy.
    override bool custom(char c) {
        if (str !is null)
            str[0] = c;
        return str is null;
    }
}

void main () {
    import std.stdio;
    T orig = new S;
    orig.custom('X');

    T copy = orig.duplicate();
    orig.custom('Y');

    orig.writeln;
    copy.writeln; // Should have 'X' at the beginning.
}
```

```txt
I'm the instance of S p: Y23
I'm the instance of S p: X23
```




## Delphi

```delphi
program PolymorphicCopy;

type
  T = class
    function Name:String; virtual;
    function Clone:T; virtual;
  end;

  S = class(T)
    function Name:String; override;
    function Clone:T; override;
  end;

function T.Name :String; begin Exit('T')     end;
function T.Clone:T;      begin Exit(T.Create)end;

function S.Name :String; begin Exit('S')     end;
function S.Clone:T;      begin Exit(S.Create)end;

procedure Main;
var
  Original, Clone :T;
begin
  Original := S.Create;
  Clone    := Original.Clone;

  WriteLn(Original.Name);
  WriteLn(Clone.Name);
end;

begin
  Main;
end.
```

<lang>S
S
```



## E


In E, a generic copy for all objects can be built out of the serialization facility, by connecting an object recognizer to an object builder without even using any intermediate serialized form:


```e
def deSubgraphKit := <elib:serial.deSubgraphKit>


def copy(object) {
    return deSubgraphKit.recognize(object, deSubgraphKit.makeBuilder())
}
```


Since E does not have any static dispatch, this cannot be non-polymorphic without also being non-generic.

An example showing that it does indeed make copies follows. (For the task description, let <var>S</var> be the type of all serializable E objects, <var>T</var> be the <code>[http://wiki.erights.org/wiki/FlexList FlexList]</code> type (result of <code>diverge</code>), and the overriden method be <code> [http://wiki.erights.org/wiki/FlexList#push/1 push]</code>.


```e
? def a := [1].diverge()
# value: [1].diverge()

? def b := copy(a)
# value: [1].diverge()

? b.push(2)
? a
# value: [1].diverge()

? b
# value: [1, 2].diverge()
```


See also: [[Deepcopy#E]]


## EchoLisp


```scheme

(lib 'types)
(lib 'struct)

(struct T (integer:x)) ;; super class
(struct S T (integer:y)) ;; sub class
(struct K (T:box)) ;; container class, box must be of type T, or derived

(define k-source (K (S 33 42)))
(define k-copy (copy k-source))

k-source
    → #<K> (#<S> (33 42)) ;; new container, with a S in box
k-copy
    → #<K> (#<S> (33 42)) ;; copied S type

(set-S-y! (K-box k-source) 666) ;; modify k-source.box.y

k-source
    → #<K> (#<S> (33 666)) ;; modified
k-copy
    → #<K> (#<S> (33 42))  ;; unmodified

(K "string-inside") ;; trying to put a string in the container box
😡 error: T : type-check failure : string-inside → 'K:box'

```


## Elena

ELENA 4.x :

```elena
import extensions;

class T
{
    Name = "T";

    T clone() = new T();
}

class S : T
{
    Name = "S";

    T clone() = new S();
}

public program()
{
    T original := new S();
    T clone := original.clone();

    console.printLine(original.Name);
    console.printLine(clone.Name)
}
```

```txt

S
S

```


=={{header|F_Sharp|F#}}==
.NET objects have a protected method <code>MemberwiseClone</code>. This method creates a shallow copy of an object. Value type members are copied, i.e. not shared between clones. Reference type members, on the other hand, will reference the same objects after cloning.


```fsharp
type T() =
  // expose protected MemberwiseClone method (and downcast the result)
  member x.Clone() = x.MemberwiseClone() :?> T
  // virtual method Print with default implementation
  abstract Print : unit -> unit
  default x.Print() = printfn "I'm a T!"

type S() =
  inherit T()
  override x.Print() = printfn "I'm an S!"

let s = new S()
let s2 = s.Clone()  // the static type of s2 is T, but it "points" to an S
s2.Print() // prints "I'm an S!"
```


## Factor

shallow copy is achieved with the "clone" word.
Deep copy is achieved with the serializing words "object>bytes" and "bytes>object".

Pasting the following in a repl:

```factor
USING: classes kernel prettyprint serialize ;
TUPLE: A ;
TUPLE: C < A ;
: serial-clone ( obj -- obj' ) object>bytes bytes>object ;

C new
[ clone ]
[ serial-clone ] bi [ class . ] bi@
```

    C
    C

## Forth

There are numerous, mutually incompatible object oriented frameworks for Forth. This one works with the FOOS preprocessor extension of [[4tH]].

```forth
include lib/memcell.4th
include 4pp/lib/foos.4pp
                                       ( a1 -- a2)
:token fork dup allocated dup (~~alloc) swap >r swap over r> smove ;
                                       \ allocate an empty object
:: T()                                 \ super class T
   class
     method: print                     \ print message
     method: clone                     \ clone yourself
   end-class {
                                       \ implementing methods
     :method { ." class T" cr } ; defines print
     fork defines clone                ( -- addr)
   }
;

:: S()                                 \ class S
   extends T()                         \ derived from T
   end-extends {                       \ print message
     :method { ." class S" cr } ; defines print
   }                                   \ clone yourself
;

new T() t                              \ create a new object t
new S() s                              \ create a new object s

." before copy" cr
t => print                             \ use "print" methods
s => print

t => clone to tcopy                    \ cloning t, spawning tcopy
s => clone to scopy                    \ cloning s, spawning scopy

." after copy" cr
tcopy => print                         \ use "print" methods
scopy => print
```



Works with any ANS Forth

Needs the FMS-SI (single inheritance) library code located here:
http://soton.mpeforth.com/flag/fms/index.html

```forth
include FMS-SI.f

:class T
 ivar container  \ can contain an object of any type
 :m put ( obj -- ) container ! ;m
 :m init: self self put ;m \ initially container holds self
 :m print ." class is T" ;m
 :m print-container container @ print ;m
;class

:class S <super T    \ subclass S from T
 :m print ." class is S" ;m \ override T's print method
;class

: ecopy {: obj1 -- obj2 :} \ make an exact copy of obj
  obj1 dup >class dfa @
  obj1 heap: dup >r swap move r> ;

T obj-t  \ instantiate a T object
obj-t print-container \ class is T

S obj-s  \ instantiate an S object
obj-s ecopy obj-t put  \ make an exact copy of S object and store in T object

obj-t print-container \ class is S

```


## Fortran

Tested with GNU gfortran 5.2.1 and INTEL ifort 16.

```Fortran

!-----------------------------------------------------------------------
!Module polymorphic_copy_example_module
!-----------------------------------------------------------------------
module polymorphic_copy_example_module
   implicit none
   private ! all by default
   public :: T,S

   type, abstract :: T
   contains
      procedure (T_procedure1), deferred, pass :: identify
      procedure (T_procedure2), deferred, pass :: duplicate
   end type T


   abstract interface
      subroutine T_procedure1(this)
         import  :: T
         class(T), intent(inout) :: this
      end subroutine T_procedure1
      function T_procedure2(this) result(Tobj)
         import  :: T
         class(T), intent(inout) :: this
         class(T), allocatable :: Tobj
      end function T_procedure2
   end interface

   type, extends(T) :: S
   contains
      procedure, pass :: identify
      procedure, pass :: duplicate
   end type S

 contains

   subroutine  identify(this)
      implicit none
      class(S), intent(inout) :: this
      write(*,*) "S"
   end subroutine identify

   function duplicate(this) result(obj)
      class(S), intent(inout) :: this
      class(T), allocatable :: obj
      allocate(obj, source = S())
   end function duplicate

end module polymorphic_copy_example_module

!-----------------------------------------------------------------------
!Main program test
!-----------------------------------------------------------------------
program    test
   use polymorphic_copy_example_module
   implicit none

   class(T), allocatable :: Sobj
   class(T), allocatable :: Sclone

   allocate(Sobj, source = S())
   allocate(Sclone, source = Sobj % duplicate())

   call Sobj % identify()
   call Sclone % identify()

end program test

```



## Go

Go does not have the terminology of polymorphism, classes, inheritance, derived types, or overriding.

It does however, have types, structs, interfaces, and methods.  These language features are expressive enough to allow this task to be implemented.

In addition to the types t and s called for by the task description, I define two other types:

i, an interface type, is needed as a common "base" definition for the method.
This is not Go terminology, and in fact notice that i is not even referenced anywhere in the definitions of types s or t.
A "parent" method definition is called for by the task however, and interfaces are Go's way of doing this.

r, another type defined similarly to s, I added to illustrate that this analog of method inheritance and overriding is valid.  (In Go terminology, that the method set of a type with an anonymous field includes the method set of the anonymous field.)

You can see in the output that interface values of type r access t's identify method.
Values of type s would as well, except s has it's own identify method which takes precedence.

```go
package main

import (
    "fmt"
    "reflect"
)

// interface types provide polymorphism, but not inheritance.
type i interface {
    identify() string
}

// "base" type
type t float64

// "derived" type.  in Go terminology, it is simply a struct with an
// anonymous field.  fields and methods of anonymous fields however,
// can be accessed without additional qualification and so are
// "inherited" in a sense.
type s struct {
    t
    kōan string
}

// another type with an "embedded" t field.  (t is the *type*, this field
// has no *name*.)
type r struct {
    t
    ch chan int
}

// a method on t.  this method makes t satisfy interface i.
// since a t is embedded in types s and r, they automatically "inherit"
// the method.
func (x t) identify() string {
    return "I'm a t!"
}

// the same method on s.  although s already satisfied i, calls to identify
// will now find this method rather than the one defined on t.
// in a sense it "overrides" the method of the "base class."
func (x s) identify() string {
    return "I'm an s!"
}

func main() {
    // three variables with different types, initialized from literals.
    var t1 t = 5
    var s1 s = s{6, "one"}
    var r1 r = r{t: 7}

    // variables declared with the same type.  initial value is nil.
    var i1, i2, i3 i
    fmt.Println("Initial (zero) values of interface variables:")
    fmt.Println("i1:", i1)
    fmt.Println("i2:", i2)
    fmt.Println("i3:", i3)

    // in the terminology of the Go language reference, i1, i2, and i3
    // still have static type i, but now have different dynamic types.
    i1, i2, i3 = t1, s1, r1
    fmt.Println("\nPolymorphic:")
    fmt.Println("i1:", i1, "/", i1.identify(), "/", reflect.TypeOf(i1))
    fmt.Println("i2:", i2, "/", i2.identify(), "/", reflect.TypeOf(i2))
    fmt.Println("i3:", i3, "/", i3.identify(), "/", reflect.TypeOf(i3))

    // copy: declare and assign in one step using "short declaration."
    i1c, i2c, i3c := i1, i2, i3

    // modify first set of polymorphic variables.
    i1, i2, i3 = s{3, "dog"}, r{t: 1}, t(2)

    // demonstrate that copies are distinct from first set
    // and that types are preserved.
    fmt.Println("\nFirst set now modified:")
    fmt.Println("i1:", i1, "/", i1.identify(), "/", reflect.TypeOf(i1))
    fmt.Println("i2:", i2, "/", i2.identify(), "/", reflect.TypeOf(i2))
    fmt.Println("i3:", i3, "/", i3.identify(), "/", reflect.TypeOf(i3))

    fmt.Println("\nCopies made before modifications:")
    fmt.Println("i1c:", i1c, "/", i1c.identify(), "/", reflect.TypeOf(i1c))
    fmt.Println("i2c:", i2c, "/", i2c.identify(), "/", reflect.TypeOf(i2c))
    fmt.Println("i3c:", i3c, "/", i3c.identify(), "/", reflect.TypeOf(i3c))
}
```

```txt

Initial (zero) values of interface variables:
i1: <nil>
i2: <nil>
i3: <nil>

Polymorphic:
i1: 5 / I'm a t! / main.t
i2: {6 one} / I'm an s! / main.s
i3: {7 0x0} / I'm a t! / main.r

First set now modified:
i1: {3 dog} / I'm an s! / main.s
i2: {1 0x0} / I'm a t! / main.r
i3: 2 / I'm a t! / main.t

Copies made before modifications:
i1c: 5 / I'm a t! / main.t
i2c: {6 one} / I'm an s! / main.s
i3c: {7 0x0} / I'm a t! / main.r

```



## Groovy

{{trans|Java}} (more or less)
Solution:

```groovy
class T implements Cloneable {
    String property
    String name() { 'T' }
    T copy() {
        try { super.clone() }
        catch(CloneNotSupportedException e) { null }
    }
    @Override
    boolean equals(that) { this.name() == that?.name() && this.property == that?.property }
}

class S extends T {
    @Override String name() { 'S' }
}
```


Test:

```groovy
T obj1 = new T(property: 'whatever')
S obj2 = new S(property: 'meh')

def objA = obj1.copy()
def objB = obj2.copy()

assert objA.class == T
assert objA == obj1 && ! objA.is(obj1) // same values, not same instance

assert objB.class == S
assert objB == obj2 && ! objB.is(obj2) // same values, not same instance

println "objA:: name: ${objA.name()}, property: ${objA.property}"
println "objB:: name: ${objB.name()}, property: ${objB.property}"
```


```txt
objA:: name: T, property: whatever
objB:: name: S, property: meh
```


=={{header|Icon}} and {{header|Unicon}}==

Icon and Unicon do no compile-time type checks.
The deepcopy procedure from the Deep Copy task is sufficient.
The sample code shown below is Unicon-specific only because it is copying an object (Icon has no object-oriented support).
The deepcopy procedure is identical in both languages.


```unicon
class T()
    method a(); write("This is T's a"); end
end

class S: T()
    method a(); write("This is S's a"); end
end

procedure main()
    write("S:",deepcopy(S()).a())
end

procedure deepcopy(A, cache)  #: return a deepcopy of A
    local k

    /cache := table()        # used to handle multireferenced objects
    if \cache[A] then return cache[A]

    case type(A) of {
        "table"|"list": {
            cache[A] := copy(A)
            every cache[A][k := key(A)] := deepcopy(A[k], cache)
            }
        "set": {
            cache[A] := set()
            every insert(cache[A], deepcopy(!A, cache))
            }
        default: {           # records and objects (encoded as records)
            cache[A] := copy(A)
            if match("record ",image(A)) then {
                every cache[A][k := key(A)] := deepcopy(A[k], cache)
                }
            }
        }
    return .cache[A]
end
```


Sample run:

```txt

->polycopy
This is S's a
->

```



## J


Most of the time, J will automatically make a copy for you on dereference.
So expressions of the form
   def=: abc
are sufficient.

However, if you are working with mapped files, this lazy copy mechanism does not work, and neither will the built in identify functions.  In those cases, you can use
   def=: {.,: abc

Note that objects cannot be dereferenced.  If you need polymorphic copy in J, you probably should not be using objects for that purpose.


## JavaScript

Copied from [http://keithdevens.com/weblog/archive/2007/Jun/07/javascript.clone here]:

```javascript
function clone(obj){
    if (obj == null || typeof(obj) != 'object')
        return obj;

    var temp = {};
    for (var key in obj)
        temp[key] = clone(obj[key]);
    return temp;
}
```



## Java

Here we implement a "copy" method once in the base class because there is by default no public way to copy an object from outside the class in Java. (There is a protected, not public, "clone" method inherited from Object.)
The way to clone polymorphically is to obtain a copy from the superclass's <code>clone()</code> method, and then perform custom copying operations specific to this class.
If this pattern is followed, the calls will eventually pass all the way up to <code>Object</code>'s <code>clone()</code> method, which performs a polymorphic copy.
If you do not follow this pattern, and simply use a constructor, like <code>new T()</code>, then the copy won't be polymorphic.

```java
class T implements Cloneable {
    public String name() { return "T"; }
    public T copy() {
        try {
            return (T)super.clone();
        } catch (CloneNotSupportedException e) {
            return null;
        }
    }
}

class S extends T {
    public String name() { return "S"; }
}

public class PolymorphicCopy {
    public static T copier(T x) { return x.copy(); }
    public static void main(String[] args) {
        T obj1 = new T();
        S obj2 = new S();
        System.out.println(copier(obj1).name()); // prints "T"
        System.out.println(copier(obj2).name()); // prints "S"
    }
}
```



## Julia

To perform as required in the exercise, mutable structs must be used to keep the deepcopy
procedure from being optimized away as a mere reference to the first struct.

```julia

abstract type Jewel end

mutable struct RoseQuartz <: Jewel
    carats::Float64
    quality::String
end

mutable struct Sapphire <: Jewel
    color::String
    carats::Float64
    quality::String
end

color(j::RoseQuartz) = "rosepink"
color(j::Jewel) = "Use the loupe."
color(j::Sapphire) = j.color

function testtypecopy()
    a = Sapphire("blue", 5.0, "good")
    b = RoseQuartz(3.5, "excellent")

    j::Jewel = deepcopy(b)

    println("a is a Jewel? ", a isa Jewel)
    println("b is a Jewel? ", a isa Jewel)
    println("j is a Jewel? ", a isa Jewel)
    println("a is a Sapphire? ", a isa Sapphire)
    println("a is a RoseQuartz? ", a isa RoseQuartz)
    println("b is a Sapphire? ", b isa Sapphire)
    println("b is a RoseQuartz? ", b isa RoseQuartz)
    println("j is a Sapphire? ", j isa Sapphire)
    println("j is a RoseQuartz? ", j isa RoseQuartz)

    println("The color of j is ", color(j), ".")
    println("j is the same as b? ", j == b)
end

testtypecopy()

```
```txt

 a is a Jewel? true
 b is a Jewel? true
 j is a Jewel? true
 a is a Sapphire? true
 a is a RoseQuartz? false
 b is a Sapphire? false
 b is a RoseQuartz? true
 j is a Sapphire? false
 j is a RoseQuartz? true
 The color of j is rosepink.
 j is the same as b? false

```




## Kotlin


```scala
// version 1.1.2

open class Animal(val name: String, var age: Int) {
    open fun copy() = Animal(name, age)

    override fun toString() = "Name: $name, Age: $age"
}

class Dog(name: String, age: Int, val breed: String) : Animal(name, age) {
    override fun copy() = Dog(name, age, breed)

    override fun toString() = super.toString() + ", Breed: $breed"
}

fun main(args: Array<String>) {
    val a: Animal = Dog("Rover", 3, "Terrier")
    val b: Animal = a.copy()  // calls Dog.copy() because runtime type of 'a' is Dog
    println("Dog 'a' = $a")   // implicitly calls Dog.toString()
    println("Dog 'b' = $b")   // ditto
    println("Dog 'a' is ${if (a === b) "" else "not"} the same object as Dog 'b'")
}
```


```txt

Dog 'a' = Name: Rover, Age: 3, Breed: Terrier
Dog 'b' = Name: Rover, Age: 3, Breed: Terrier
Dog 'a' is not the same object as Dog 'b'

```



## MiniScript


```MiniScript
T = {}
T.foo = function()
    return "This is an instance of T"
end function

S = new T
S.foo = function()
    return "This is an S for sure"
end function

instance = new S
print "instance.foo: " + instance.foo

copy = {}
copy = copy + instance  // copies all elements
print "copy.foo: " + copy.foo

// And to prove this is a copy, and not a reference:
instance.bar = 1
copy.bar = 2
print "instance.bar: " + instance.bar
print "copy.bar: " + copy.bar
```

```txt
instance.foo: This is an S for sure
copy.foo: This is an S for sure
instance.bar: 1
copy.bar: 2
```



## NetRexx

```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

-- -----------------------------------------------------------------------------
class RCPolymorphicCopy public

method copier(x = T) public static returns T
  return x.copy

method main(args = String[]) public constant
  obj1 = T()
  obj2 = S()
  System.out.println(copier(obj1).name) -- prints "T"
  System.out.println(copier(obj2).name) -- prints "S"
  return

-- -----------------------------------------------------------------------------
class RCPolymorphicCopy.T public implements Cloneable

method name returns String
  return T.class.getSimpleName

method copy public returns T
  dup = T

  do
    dup = T super.clone
  catch ex = CloneNotSupportedException
    ex.printStackTrace
  end

  return dup

-- -----------------------------------------------------------------------------
class RCPolymorphicCopy.S public extends RCPolymorphicCopy.T

method name returns String
  return S.class.getSimpleName

```



## Nim


```nim
type
  T = ref object of TObject
    myValue: string
  S1 = ref object of T
  S2 = ref object of T

method speak(x: T)  = echo "T Hello ", x.myValue
method speak(x: S1) = echo "S1 Meow ", x.myValue
method speak(x: S2) = echo "S2 Woof ", x.myValue

echo "creating initial objects of types S1, S2, and T"
var a = S1(myValue: "Green")
a.speak
var b = S2(myValue: "Blue")
b.speak
var u = T(myValue: "Blue")
u.speak

echo "Making copy of a as u, colors and types should match"
u.deepCopy(a)
u.speak
a.speak

echo "Assigning new color to u, A's color should be unchanged."
u.myValue = "Orange"
u.speak
a.speak

echo "Assigning u to reference same object as b, colors and types should match"
u = b
u.speak
b.speak

echo "Assigning new color to u. Since u,b references same object b's color changes as well"
u.myValue = "Yellow"
u.speak
b.speak
```

```txt
creating initial objects of types S1, S2, and T
S1 Meow Green
S2 Woof Blue
T Hello Blue
Making copy of a as u, colors and types should match
S1 Meow Green
S1 Meow Green
Assigning new color to u, A's color should be unchanged.
S1 Meow Orange
S1 Meow Green
Assigning u to reference same object as b, colors and types should match
S2 Woof Blue
S2 Woof Blue
Assigning new color to u. Since u,b references same object b's color changes as well
S2 Woof Yellow
S2 Woof Yellow
```


=={{header|Objective-C}}==
All objects inherit the <code>copy</code> method from <code>NSObject</code>, which performs copying.
But they must implement the <code>NSCopying</code> protocol (which involves implementing the <code>copyWithZone:</code> method) to actually specify how to copy.
Calling <code>copy</code> on an object that does not implement <code>copyWithZone:</code> will result in an error.

Generally, to implement copying, if your parent class does not implement <code>NSCopying</code>, you explicitly allocate a new object (using the class object <code>[self class]</code> instead of hard-coding the name of a particular class, in order to be polymorphic), and initialize it with your object's data.
If your parent class already implements <code>NSCopying</code>, and you wish to customize the copying of your class's fields, then you should get a copy from your parent object's <code>copyWithZone:</code> method, and then perform custom initialization on the copy.


```objc
@interface T : NSObject
- (void)identify;
@end

@implementation T
- (void)identify {
    NSLog(@"I am a genuine T");
}
- (id)copyWithZone:(NSZone *)zone {
    T *copy = [[[self class] allocWithZone:zone] init]; // call an appropriate constructor here
                                                        // then copy data into it as appropriate here
                                                        // make sure to use "[[self class] alloc..." and
                                                        // not "[T alloc..." to make it polymorphic
    return copy;
}
@end

@interface S : T
@end

@implementation S
- (void)identify
{
    NSLog(@"I am an S");
}
@end

int main()
{
    @autoreleasepool {

        T *original = [[S alloc] init];
        T *another = [original copy];
        [another identify]; // logs "I am an S"

    }
    return 0;
}
```


Analogously, there is a <code>mutableCopy</code> method to get a mutable copy of the current object (e.g. if you have an NSArray object and you want an NSMutableArray with the same contents).
In this case it would have to implement the <code>NSMutableCopying</code> protocol (which involves implementing the <code>mutableCopyWithZone:</code> method) to specify how to copy.


## OCaml

I decided not to use classes and inheritance here because structural subtyping is more natural in OCaml. Oo.copy is polymorphic over all object types.

```ocaml
let obj1 =
  object
    method name = "T"
  end

let obj2 =
  object
    method name = "S"
  end

let () =
  print_endline (Oo.copy obj1)#name; (* prints "T" *)
  print_endline (Oo.copy obj2)#name; (* prints "S" *)
```



## ooRexx

All ooRexx objects have a copy method inherited from the object class that performs a shallow copy of the object state.

```ooRexx

s = .s~new
s2 = s~copy   -- makes a copy of the first
if s == s2 then say "copy didn't work!"
if s2~name == "S" then say "polymorphic copy worked"

::class t
::method name
  return "T"

::class s subclass t
::method name
  return "S"

```



## OxygenBasic


```oxygenbasic

'======
class T
'======

float vv

method constructor(float a=0) {vv=a}
method destructor    {}
method copy as T     {new T ob : ob<=vv : return ob}
method mA() as float {return vv*2}
method mB() as float {return vv*3}

end class


'======
class S
'======

has T

method mB() as float {return vv*4} 'ovveride

end class

'====
'TEST
'====

new T objA(10.5)

let objB = cast S objA.copy

print objA.mb 'result 31.5
print objB.mb 'result 42

del objA : del objB

```



## Oz

We need to derive from the class <code>ObjectSupport.reflect</code> in order to have a <code>clone</code> method.

```oz
declare
  class T from ObjectSupport.reflect
     meth init
        skip
     end

     meth name($)
        'T'
     end
  end

  class S from T
     attr a
     feat f

     meth name($)
        'S'
     end

     meth getA($) @a end
     meth setA(V) a := V end
  end

  Obj = {New S init}
  Copy = {Obj clone($)}
in
  %% Some assertions:

  %% Copy is really an S:
  {Copy name($)} = 'S'

  %% Copy is not just a reference to the same object:
  {System.eq Obj Copy} = false

  %% Not a deep copy. Feature f has the same identity for both objects:
  {System.eq Obj.f Copy.f} = true

  %% However, both have their own distinct attributes:
  {Obj setA(13)}
  {Copy setA(14)}
  {Obj getA($)} \= {Copy getA($)} = true
```


Oz is not a pure object-oriented language. In fact, most values are not objects.
For immutable data it usually does not make sense to clone it because it does not have an identity beyond its value.
For mutable data types there are clone functions available (<code>Dictionary.clone</code>, <code>Array.clone</code>, <code>BitArray.clone</code>).


## Perl


```Perl
package T;
sub new {
        my $cls = shift;
        bless [ @_ ], $cls
}

sub set_data {
        my $self = shift;
        @$self = @_;
}

sub copy {
        my $self = shift;
        bless [ @$self ], ref $self;
}

sub manifest {
        my $self = shift;
        print "type T, content: @$self\n\n";
}

package S;
our @ISA = 'T';
# S is inheriting from T.
# 'manifest' method is overriden, while 'new', 'copy' and
# 'set_data' are all inherited.
sub manifest {
        my $self = shift;
        print "type S, content: @$self\n\n";
}

package main;

print "# creating \$t as a T\n";
my $t = T->new('abc');
$t->manifest;

print "# creating \$s as an S\n";
my $s = S->new('SPQR');
$s->manifest;

print "# make var \$x as a copy of \$t\n";
my $x = $t->copy;
$x->manifest;

print "# now as a copy of \$s\n";
$x = $s->copy;
$x->manifest;

print "# show that this copy is indeed a separate entity\n";
$x->set_data('totally different');
print "\$x is: ";
$x->manifest;
```


## Perl 6


```perl6
my Cool $x = 22/7 but role Fink { method brag { say "I'm a cool {self.WHAT.perl}!" }}
my Cool $y = $x.clone;
$y.brag;
```

```txt
I'm a cool Rat+Fink!
```



## Phix

The object type is the ultimate polymorph - there is <i>nothing</i> that you can store anywhere else that you cannot store in an object.

For this demonstration (not that this really proves much), our types T and S (overkill really) are just going to contain a string name and a method (routine_id).

```Phix
enum NAME, METHOD

procedure me_t()
    puts(1,"I is a T\n")
end procedure
constant r_t = routine_id("me_t")

procedure me_s()
    puts(1,"I is an S\n")
end procedure
constant r_s = routine_id("me_s")

type T(object o)
    -- as o[METHOD] can be overidden, don't verify it!
    return sequence(o) and length(o)=2 and string(o[NAME]) and integer(o[METHOD])
end type

type S(T t)
    return t[METHOD] = r_s
end type

S this = {"S",r_s}
T that = {"T",r_t}

call_proc(that[METHOD],{})
that = this
call_proc(that[METHOD],{})
```

```txt

I is a T
I is an S

```



## PHP


```php
<?php
class T {
      function name() { return "T"; }
}

class S {
      function name() { return "S"; }
}

$obj1 = new T();
$obj2 = new S();
$obj3 = clone $obj1;
$obj4 = clone $obj2;
echo $obj3->name(), "\n"; // prints "T"
echo $obj4->name(), "\n"; // prints "S"
?>
```



## PicoLisp

Any object can be copied by transferring the value and the property list.
If we create an object 'A':

```PicoLisp
: (setq A (new '(+Cls1 +Cls2) 'attr1 123  'attr2 "def"  'attr3 (4 2 0)  'attr4 T))
-> $385603635

: (show A)
$385603635 (+Cls1 +Cls2)
   attr4
   attr3 (4 2 0)
   attr2 "def"
   attr1 123
-> $385603635
```

Then we can easily copy it to a new object 'B':

```PicoLisp
(putl (setq B (new (val A))) (getl A))
```

Inspecting 'B':

```PicoLisp
: (show B)
$385346595 (+Cls1 +Cls2)
   attr1 123
   attr2 "def"
   attr3 (4 2 0)
   attr4
-> $385346595
```



## Python


```python
import copy

class T:
   def classname(self):
      return self.__class__.__name__

   def __init__(self):
      self.myValue = "I'm a T."

   def speak(self):
      print self.classname(), 'Hello', self.myValue

   def clone(self):
      return copy.copy(self)

class S1(T):
   def speak(self):
      print self.classname(),"Meow", self.myValue

class S2(T):
   def speak(self):
      print self.classname(),"Woof", self.myValue


print "creating initial objects of types S1, S2, and T"
a = S1()
a.myValue = 'Green'
a.speak()

b = S2()
b.myValue = 'Blue'
b.speak()

u = T()
u.myValue = 'Purple'
u.speak()

print "Making copy of a as u, colors and types should match"
u = a.clone()
u.speak()
a.speak()
print "Assigning new color to u, A's color should be unchanged."
u.myValue = "Orange"
u.speak()
a.speak()

print "Assigning u to reference same object as b, colors and types should match"
u = b
u.speak()
b.speak()
print "Assigning new color to u. Since u,b references same object b's color changes as well"
u.myValue = "Yellow"
u.speak()
b.speak()
```

```txt

creating initial objects of types S1, S2, and T
S1 Meow Green
S2 Woof Blue
T Hello Purple
Making copy of a as u, colors and types should match
S1 Meow Green
S1 Meow Green
Assigning new color to u, A's color should be unchanged.
S1 Meow Orange
S1 Meow Green
Assigning u to reference same object as b, colors and types should match
S2 Woof Blue
S2 Woof Blue
Assigning new color to u. Since u,b references same object b's color changes as well
S2 Woof Yellow
S2 Woof Yellow
```


The foregoing example uses the Python standard library ''copy'' module.
The task, as stated,does not provide insight as to what should happen should the object contain lists of other objects.
It could be necessary to use "deep copy" instead of copy. (using ''copy.deepcopy'').
The distinction is important for complex objects containing references to other objects (for instances lists, tuples or dictionaries containing other lists, tuples or dictionaries as elements).
The described task, as presented, offers no guidance on this matter.

In many cases the most portable and robust "copy" would be made by serializing the source object and then de-serializing it back into the target.
Under Python this would best be done with the ''pickle'' or ''cPickle'' standard modules.


```python
import cPickle as pickle

source = {'a': [1, 2.0, 3, 4+6j],
         'b': ('string', u'Unicode string'),
         'c': None}

target = pickle.loads(pickle.dumps(source))
```


In this example we use the ''cPickle'' module which is an implementation of the pickle features coded in C for optimal performance.
We import it as ''pickle'' since we intend to use only those features which are common to both implementations.
(The pure Python implementation is retained for those who which to create their own classes derived therefrom).
The ''dumps()'' and ''loads()'' methods dump the data structures to a string and load them from a string, respectively.  (The more common use of ''pickle'' is to serialize the data out to a file or over a network connection using file-like ''.read()'' and ''.write()'' methods.
For those we'd use the ''pickle.dump()'' and ''pickle.load()'' methods).

For the simplest cases one can use simple Python introspection to copy simple objects:


```python
target = source.__class__()  # Create an object of the same type
if hasattr(source, 'items') and callable(source.items):
    for key,value in source.items:
        target[key] = value
elif hasattr(source, '__len__'):
    target = source[:]
else:  # Following is not recommended. (see below).
    target = source
```


This example handles dictionaries (and anything that implements a sufficiently dictionary like interface to support the ''items()'' method along with the ''__setitem__()'' method. (statements of the form '''''x[y] = z''''' in Python are implicitly calling the ''__setitem__()'' method of the "x" object, passing it a key of "y" and a value of "z."
Similarly this code tests if an item is a sequence (one can call the "len()" built-in function on it) and, if so, uses a slice assignment to perform a shallow copy.
For any other type of object a simple binding is performed.
Technically this last case will not "copy" anything ... it will create a new name binding to the object to which "source" was already a reference.
The earlier binding of a "blank" instance of the source's __class__ will be replaced.
So the trick of creating the blank object of the same type is only meaningful for the other types.
In the cases of strings, integers and other numbers the objects themselves are immutable and the bindings are all dynamic (so the entire task is moot with respect to them).


## Racket


### Using prefab structures

This method is useful only for prefab structures.

```Racket
#lang racket/base

(define (copy-prefab-struct str)
  (apply make-prefab-struct (vector->list (struct->vector str))))

(struct point (x y) #:prefab)
(struct point/color point (color) #:prefab)


(let* ([original (point 0 0)]
       [copied (copy-prefab-struct original)])
  (displayln copied)
  (displayln (eq? original copied)))

(let* ([original (point/color 0 0 'black)]
       [copied (copy-prefab-struct original)])
  (displayln copied)
  (displayln (eq? original copied)))
```

```txt
#s(struct:point 0 0)
#f
#s(struct:point/color 0 0 black)
#f

```



### Using transparent structures

This method can be applied to prefab or transparent structures, or using a powerful enough inspector. The example uses transparent structures.

It’s also possible to copy other structures using generics or structure-type-properties to implement a “magic”-like generic method.

```Racket
#lang racket/base

(define (copy-struct str)
  (define-values (str-struct-info _) (struct-info str))
  (define str-maker (struct-type-make-constructor str-struct-info))
  (apply str-maker (cdr (vector->list (struct->vector str)))))

(struct point (x y) #:transparent)
(struct point/color point (color) #:transparent)

(let* ([original (point 0 0)]
       [copied (copy-struct original)])
  (displayln copied)
  (displayln (eq? original copied)))

(let* ([original (point/color 0 0 'black)]
       [copied (copy-struct original)])
  (displayln copied)
  (displayln (eq? original copied)))
```

```txt
#s(struct:point 0 0)
#f
#s(struct:point/color 0 0 black)
#f

```



### Using classes

There is no build-in clone method, so the class (or the interface) must implement it.

```Racket
;#lang racket

(define point%
  (class object%
    (super-new)
    (init-field x y)
    (define/public (clone) (new this% [x x] [y y]))
    (define/public (to-list) (list this% x y))))

(define point/color%
  (class point%
    (super-new)
    (inherit-field x y)
    (init-field color)
    (define/override (clone) (new this% [x x] [y y] [color color]))
    (define/override (to-list) (list this% x y color))))
```

```txt
(#<class:point%> 0 0)
#f
(#<class:point/color%> 0 0 black)
#f
```



## REXX

In the REXX language, every variable is a string   (whether or not they contain characters or numerals).

However, a variables' type (by REXX's definition) can be determined/inferred from it's attributes (datatypes).

```rexx
/*REXX program to  copy  (polymorphically)  one variable's value into another variable. */
b= 'old value.'
a= 123.45
b= a                                             /*copy a variable's value into another.*/
if a==b  then say "copy did work."
         else say "copy didn't work."            /*didn't work, maybe ran out of storage*/
                                                 /*stick a fork in it,  we're all done. */
```

Programming note:   Most REXXes will raise a syntax error if an assignment (copy) fails, but it's not guaranteed to do so.




## Ruby

All Ruby objects inherit two methods for copying themselves: "clone" and "dup".
I don't really understand the difference between them.

```ruby
class T
  def name
    "T"
  end
end

class S
  def name
    "S"
  end
end

obj1 = T.new
obj2 = S.new
puts obj1.dup.name # prints "T"
puts obj2.dup.name # prints "S"
```



## Scala

<lang>object PolymorphicCopy {

  def main(args: Array[String]) {
    val a: Animal = Dog("Rover", 3, "Terrier")
    val b: Animal = a.copy() // calls Dog.copy() because runtime type of 'a' is Dog
    println(s"Dog 'a' = $a") // implicitly calls Dog.toString()
    println(s"Dog 'b' = $b") // ditto
    println(s"Dog 'a' is ${if (a == b) "" else "not"} the same object as Dog 'b'")
  }

  case class Animal(name: String, age: Int) {

    override def toString = s"Name: $name, Age: $age"
  }

  case class Dog(override val name: String, override val age: Int, breed: String) extends Animal(name, age) {

    override def toString = super.toString() + s", Breed: $breed"
  }

}
```


## Sidef

''Object.dclone()'' makes a deep clone of any mutable object and returns it to the caller.

```ruby
class T(value) {
    method display {
        say value;
    }
}

class S(value) < T {
    method display {
        say value;
    }
}

var obj1 = T("T");
var obj2 = S("S");
var obj3 = obj2.dclone;         # make a deep clone of obj2

obj1.value = "foo";             # change the value of obj1
obj2.value = "bar";             # change the value of obj2

obj1.display;                   # prints "foo"
obj2.display;                   # prints "bar"
obj3.display;                   # prints "S"
```



## Slate


All objects in Slate may be <tt>clone</tt>d unless they are clones of <tt>Oddball</tt> (like <tt>True</tt>, <tt>False</tt>, and <tt>Nil</tt>) or are word-size Integers (which are encoded as tagged pointer values).
This is a shallow copy where no values held in the object's slots are replaced with copies.
There is also a <tt>copy</tt> method which is universal and overridden to perform deep copies as appropriate - copying continues via recursion through slot values and should be modified on any type where equality (<tt>=</tt>) is overridden.


```slate
define: #T &parents: {Cloneable}.

define: #S &parents: {Cloneable}.

define: #obj1 -> T clone.
define: #obj2 -> S clone.

obj1 printName.
obj2 printName.
```



## Swift


```swift
class T {
  required init() { } // constructor used in polymorphic initialization must be "required"
  func identify() {
    println("I am a genuine T")
  }
  func copy() -> T {
    let newObj : T = self.dynamicType() // call an appropriate constructor here
    // then copy data into newObj as appropriate here
    // make sure to use "self.dynamicType(...)" and
    // not "T(...)" to make it polymorphic
    return newObj
  }
}

class S : T {
  override func identify()  {
    println("I am an S")
  }
}

let original : T = S()
let another : T = original.copy()
println(original === another) // prints "false" (i.e. they are different objects)
another.identify() // prints "I am an S"
```



## Tcl

Tcl values are logically immutable, and are passed around by reference with copies being taken as and when it is necessary to do so to maintain the immutable model.
Hence an effective copy of any value is just:

```tcl
set varCopy $varOriginal
```

With objects, slightly more work is required because they are normally passed around by name/reference.


```tcl
oo::class create CanClone {
    method clone {{name {}}} {
        # Make a bare, optionally named, copy of the object
        set new [oo::copy [self] {*}[expr {$name eq "" ? {} : [list $name]}]]

        # Reproduce the basic variable state of the object
        set newns [info object namespace $new]
        foreach v [info object vars [self]] {
            namespace upvar [namespace current] $v v1
            namespace upvar $newns $v v2
            if {[array exists v1]} {
                array set v2 [array get v1]
            } else {
                set v2 $v1
            }
        }
        # Other object state is possible like open file handles. Cloning that is
        # properly left to subclasses, of course.

        return $new
    }
}

# Now a demonstration
oo::class create Example {
    superclass CanClone
    variable Count Name
    constructor {name} {set Name $name;set Count 0}
    method step {} {incr Count;return}
    method print {} {puts "this is $Name in [self], stepped $Count times"}
    method rename {newName} {set Name $newName}
}
set obj1 [Example new "Abracadabra"]
$obj1 step
$obj1 step
$obj1 print
set obj2 [$obj1 clone]
$obj2 step
$obj2 print
$obj2 rename "Hocus Pocus"
$obj2 print
$obj1 print
```

{{out}} (object names might vary if you run it):

```txt

this is Abracadabra in ::oo::Obj5, stepped 2 times
this is Abracadabra in ::oo::Obj6, stepped 3 times
this is Hocus Pocus in ::oo::Obj6, stepped 3 times
this is Abracadabra in ::oo::Obj5, stepped 2 times

```


{{Omit From|ALGOL 68}} <!-- it isn't immediately obvious that ALGOL 68 is object oriented -->
