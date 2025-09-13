+++
title = "Inheritance/Multiple"
description = ""
date = 2019-05-06T21:35:52Z
aliases = []
[extra]
id = 2840
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
+++

## Task

[[Category:Object oriented]] [[Category:Type System]]
Multiple inheritance allows to specify that one [[classes | class]] is a subclass of several other classes.
Some languages allow multiple [[inheritance]] for arbitrary classes,
others restrict it to interfaces, some don't allow it at all.

Write two classes (or interfaces) <tt>Camera</tt> and <tt>MobilePhone</tt>,
then write a class <tt>CameraPhone</tt> which is both a <tt>Camera</tt> and
a <tt>MobilePhone</tt>.

There is no need to implement any functions for those classes.


## Ada

Ada 2005 has added interfaces, allowing a limited form of multiple inheritance.

```ada
package Multiple_Interfaces is
   type Camera is tagged null record;
   type Mobile_Phone is limited Interface;
   type Camera_Phone is new Camera and Mobile_Phone with null record;
end Multiple_Interfaces;
```

## Aikido

Aikido does not support multiple inheritance, but does allow multiple implementation of interfaces.

```aikido
interface Camera {
}

interface Mobile_Phone {
}

class Camera_Phone implements Camera, Mobile_Phone {
}
```



## BBC BASIC

```bbcbasic
      INSTALL @lib$+"CLASSLIB"

      DIM Camera{TakePicture}
      PROC_class(Camera{})

      DIM MobilePhone{MakeCall}
      PROC_class(MobilePhone{})

      DIM CameraPhone{methods}
      PROC_inherit(CameraPhone{}, Camera{})
      PROC_inherit(CameraPhone{}, MobilePhone{})
      PROC_class(CameraPhone{})
```



## C++


```cpp
class Camera
{
  // ...
};

class MobilePhone
{
  // ...
};

class CameraPhone:
  public Camera,
  public MobilePhone
{
  // ...
};
```


## C#
In C# you may inherit from only one class, but you can inherit
from multiple interfaces.
Also, in C# it is standard practice to start all interface names
with a capital 'I' so I have altered the name of the interface.
In the example we inherit from a class and an interface.


```c#
interface ICamera {
    // ...
}

class MobilePhone {
    // ...
}

class CameraPhone: ICamera, MobilePhone {
    // ...
}
```



## Clojure


```Clojure
(defprotocol Camera)

(defprotocol MobilePhone)

(deftype CameraPhone []
  Camera
  MobilePhone)
```



## COBOL


```cobol
       CLASS-ID. Camera.
           *> ...
       END CLASS Camera.

       CLASS-ID. Mobile-Phone.
           *> ...
       END CLASS Mobile-Phone.

       CLASS-ID. Camera-Phone INHERITS Camera, Mobile-Phone.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           CLASS Camera
           CLASS Mobile-Phone.

           *> ...
       END CLASS Camera-Phone.
```



## Common Lisp



```lisp
(defclass camera () ())
(defclass mobile-phone () ())
(defclass camera-phone (camera mobile-phone) ())
```



## D


While D does not have multiple base class inheritance, you can inherit from multiple interfaces.


```d
interface Camera {
    // member function prototypes and static methods
}

interface MobilePhone {
    // member function prototypes and static methods
}

class CameraPhone: Camera, MobilePhone {
    // member function implementations for Camera,
    //   MobilePhone, and CameraPhone
}
```


D also supports the [[non-virtual interface]] pattern, where an interface may have non-virtual methods with defined implementations.


```d
interface Camera {
    // A virtual function.
    Image takePhoto();

    // A non-virtual function.
    final Image[] takeSeveralPhotos(int count) {
        auto result = new Image[count];
        foreach (ref img; result) {
            img = takePhoto();
        }
    }
}
```


In addition, D's alias this feature allows one to create a type that, while it does not technically derive from two different classes, behaves as if it did.


```d
class A {
    string foo() {
        return "I am an A.";
    }
}
class B {
    string foo() {
        return "I am a B.";
    }
}

class C : A {
    string className = "C";
    override string foo() {
        return "I am a "~className~", and thus an A.";
    }
    @property
    BWrapper asB() {
        return new BWrapper();
    }
    alias asB this;
    class BWrapper : B {
        override string foo() {
            return "I am a "~className~", disguised as a B.";
        }
    }
}

unittest {
    import std.stdio : writeln;

    auto c = new C();
    A a = c;
    B b = c;

    writeln(a.foo());
    writeln(b.foo());
}
```


You can currently only have a single alias this, but multiple alias this is planned. Nested alias this works today, but is somewhat finicky.

Lastly, D has template and string mixins. These can be used for static polymorphism, where a piece of code is written once and has a single definition, but is used in multiple places. It does not enable any sort of dynamic polymorphism that is not covered above.


```d
template registerable() {
    void register() { /* implementation */ }
}

string makeFunction(string s) {
    return `string `~s~`(){ return "`~s~`";}`;
}

class Foo {
    mixin registerable!();
    mixin(makeFunction("myFunction"));
}

unittest {
    import std.stdio : writeln;
    Foo foo = new Foo;
    foo.register();
    writeln(foo.myFunction());
}
```


Using D's [[Compile-time calculation|CTFE]] and [[reflection]] capabilities, string mixins can copy the interface of other types, and thus be used for proxies and mocks.


## Delphi

Delphi doesn't support multiple inheritance, but it does have multiple interfaces.


```Delphi
type
  ICamera = Interface
    // ICamera methods...
  end;

  IMobilePhone = Interface
    // IMobilePhone methods...
  end;

  TCameraPhone = class(TInterfacedObject, ICamera, IMobilePhone)
    // ICamera and IMobilePhone methods...
  end;
```



## DWScript


See [[Inheritance/Multiple#Delphi|Delphi]].


## E


E does not have multiple inheritance as a built-in feature.
In fact, E only has inheritance at all as a light syntactic sugar over delegation (message forwarding).
However, using that facility it is possible to implement multiple inheritance.

This is a quick simple implementation of multiple inheritance.
It simply searches (depth-first and inefficiently) the inheritance tree for a method;
it does not do anything about [[wp:Diamond inheritance|diamond inheritance]].
These shortcomings could be fixed if more powerful multiple inheritance were needed.


```e
def minherit(self, supers) {
    def forwarder match [verb, args] {
        escape __return {
            if (verb == "__respondsTo") {
                def [verb, arity] := args
                for super ? (super.__respondsTo(verb, arity)) in supers {
                    return true
                }
                return false
            } else if (verb == "__getAllegedType") {
                # XXX not a complete implementation
                return supers[0].__getAllegedType()
            } else {
                def arity := args.size()
                for super ? (super.__respondsTo(verb, arity)) in supers {
                    return E.call(super, verb, args)
                }
                throw(`No parent of $self responds to $verb/$arity`)
            }
        }
    }
    return forwarder
}
```


The task example:


```e
def makeCamera(self) {
    return def camera extends minherit(self, []) {
        to takesPictures() { return true }
    }
}

def makeMobilePhone(self) {
    return def mobilePhone extends minherit(self, []) {
        to makesCalls() { return true }
        to internalMemory() { return 64*1024 }
    }
}

def makeCameraPhone(self) {
    return def cameraPhone extends minherit(self, [
        makeCamera(self),
        makeMobilePhone(self),
    ]) {
        to internalMemory() {
            return super.internalMemory() + 32 * 1024**2
        }
    }
}
```


And testing that it works as intended:


```e

? def p := makeCameraPhone(p)
> [p.takesPictures(), p.makesCalls(), p.internalMemory()]
# value: [true, true, 33619968]
```



## Eiffel

Having two class—one for CAMERA and the other for a MOBILE_PHONE ...

```eiffel
class
    CAMERA
end
```


```eiffel
class
    MOBILE_PHONE
end
```


###  Now Multiple Inherit

We can create a new CAMERA_PHONE, which inherits directly from both CAMERA and MOBILE_PHONE.

```eiffel
class
    CAMERA_PHONE
inherit
    CAMERA
    MOBILE_PHONE
end
```

NOTE: There is no reasonable limit to the number of classes we can inherit from in a single class. The compiler helps us to navigate issues like repeated inheritance and the "diamond of death" easily and quickly.


## Elena

ELENA only permits inheritance from one parent class. However, mixins are supported

```elena
singleton CameraFeature
{
    cameraMsg
        = "camera";
}

class MobilePhone
{
    mobileMsg
        = "phone";
}

class CameraPhone : MobilePhone
{
    dispatch() => CameraFeature;
}

public program()
{
   var cp := new CameraPhone();

   console.writeLine(cp.cameraMsg);
   console.writeLine(cp.mobileMsg)
}
```

Alternatively a group object may be created

```elena
import system'dynamic;

class CameraFeature
{
    cameraMsg
        = "camera";
}

class MobilePhone
{
    mobileMsg
        = "phone";
}

singleton CameraPhone
{
   new() = new MobilePhone().mixInto(new CameraFeature());
}

public program()
{
   var cp := CameraPhone.new();

   console.writeLine(cp.cameraMsg);
   console.writeLine(cp.mobileMsg)
}
```



## Factor


```factor
TUPLE: camera ;
TUPLE: mobile-phone ;
UNION: camera-phone camera mobile-phone ;
```



## Fantom


Fantom only permits inheritance from one parent class.
However, Fantom supports 'mixins': a mixin is a collection
of implemented methods to be added to the child class.
Any number of mixins can be added to any given child class.
It is an error for method names to conflict.


```fantom
// a regular class
class Camera
{
  Str cameraMsg ()
  {
    "camera"
  }
}

// a mixin can only contain methods
mixin MobilePhone
{
  Str mobileMsg ()
  {
    "mobile phone"
  }
}

// class inherits from Camera, and mixes in the methods from MobilePhone
class CameraPhone : Camera, MobilePhone
{
}

class Main
{
  public static Void main ()
  {
    cp := CameraPhone ()
    echo (cp.cameraMsg)
    echo (cp.mobileMsg)
  }
}
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' FB does not currently support multiple inheritance. Composition has to be used instead if one wants
' to (effectively) inherit from more than one class. In some cases, this might arguably be a better
' solution anyway.

Type Camera Extends Object  ' if virtual methods etc needed
  ' ...
End Type

Type Phone Extends Object
  ' ...
End Type

Type CameraPhone Extends Phone ' single inheritance
  cam As Camera ' using composition here
  ' other stuff
End Type
```


=={{header|F_Sharp|F#}}==
A class can only inherit from one other class, but it can implement any number of interfaces.


```fsharp
type Picture = System.Drawing.Bitmap // (a type synonym)

// an interface type
type Camera =
  abstract takePicture : unit -> Picture

// an interface that inherits multiple interfaces
type Camera2 =
  inherits System.ComponentModel.INotifyPropertyChanged
  inherits Camera

// a class with an abstract method with a default implementation
// (usually called a virtual method)
type MobilePhone() =
  abstract makeCall : int[] -> unit
  default x.makeCall(number) = () // empty impl

// a class that inherits from another class and implements an interface
type CameraPhone() =
  inherit MobilePhone()
  interface Camera with
    member x.takePicture() = new Picture(10, 10)
```



## Go

Go abandons traditional object oriented concepts of inheritance hierarchies, yet it does have features for composing both structs and interfaces.

```go
// Example of composition of anonymous structs
package main

import "fmt"

// Two ordinary structs
type camera struct {
    optics, sensor string
}

type mobilePhone struct {
    sim, firmware string
}

// Fields are anonymous because only the type is listed.
// Also called an embedded field.
type cameraPhone struct {
    camera
    mobilePhone
}

func main() {
    // Struct literals must still reflect the nested structure
    htc := cameraPhone{camera{optics: "zoom"}, mobilePhone{firmware: "3.14"}}

    // But fields of anonymous structs can be referenced without qualification.
    // This provides some effect of the two parent structs being merged, as
    // with multiple inheritance in some other programming languages.
    htc.sim = "XYZ"
    fmt.Println(htc)
}
```

{{out}} (Note sensor field still blank)

```txt
```


```go
// Example of composition of interfaces.
// Types implement interfaces simply by implementing functions.
// The type does not explicitly declare the interfaces it implements.
package main

import "fmt"

// Two interfaces.
type camera interface {
    photo()
}

type mobilePhone interface {
    call()
}

// Compose interfaces.  cameraPhone interface now contains whatever
// methods are in camera and mobilePhone.
type cameraPhone interface {
    camera
    mobilePhone
}

// User defined type.
type htc int

// Once the htc type has this method defined on it, it automatically satisfies
// the camera interface.
func (htc) photo() {
    fmt.Println("snap")
}

// And then with this additional method defined, it now satisfies both the
// mobilePhone and cameraPhone interfaces.
func (htc) call() {
    fmt.Println("omg!")
}

func main() {
    // type of i is the composed interface.  The assignment only compiles
    // because static type htc satisfies the interface cameraPhone.
    var i cameraPhone = new(htc)
    // interface functions can be called without reference to the
    // underlying type.
    i.photo()
    i.call()
}
```

```txt

snap
omg!

```



## Groovy

Same inheritance rules as [[Inheritance/Multiple#Java|Java]].


## Haskell


```haskell
class Camera a
class MobilePhone a
class (Camera a, MobilePhone a) => CameraPhone a
```


==Icon and {{header|Unicon}}==
Icon does not support classes or inheritance.
An intermediate language called Idol was developed as proof of concept
for extending Icon.
This became one of the major addons contributing to Unicon.


```unicon
class Camera (instanceVars)
    # methods...
    # initializer...
end

class Phone (instanceVars)
    # methods...
    # initializer...
end

class CameraPhone : Camera, Phone (instanceVars)
    # methods...
    # initialiser...
end
```



## Io


```Io
Camera := Object clone
Camera click := method("Taking snapshot" println)

MobilePhone := Object clone
MobilePhone call := method("Calling home" println)

CameraPhone := Camera clone
CameraPhone appendProto(MobilePhone)

myPhone := CameraPhone clone
myPhone click	// --> "Taking snapshot"
myPhone call	// --> "Calling home"
```

In Io each object has an internal list of prototype objects it inherits from.  You can add to this list with <code>appendProto</code>.

## Ioke


```ioke
Camera = Origin mimic
MobilePhone = Origin mimic
CameraPhone = Camera mimic mimic!(MobilePhone)
```



## J


```j
coclass 'Camera'

create=: verb define
  NB. creation-specifics for a camera go here
)

destroy=: codestroy

NB. additional camera methods go here

coclass 'MobilePhone'

create=: verb define
  NB. creation-specifics for a mobile phone go here
)

destroy=: codestroy

NB. additional phone methods go here

coclass 'CameraPhone'
coinsert 'Camera MobilePhone'

create=: verb define
  create_Camera_ f. y
  create_MobilePhone_ f. y
  NB. creation details specific to a camera phone go here
)

destroy=: codestroy

NB. additional camera-phone methods go here
```

The adverb Fix (f.) is needed as shown so the superclass constructors
get executed in the object, not in the superclass.


## Java

Java does not allow multiple inheritance, but you can "implement" multiple interfaces. All methods in interfaces are abstract (they don't have an implementation).
When you implement an interface you need to implement the specified methods.

```java
public interface Camera{
   //functions here with no definition...
   //ex:
   //public void takePicture();
}
```


```java
public interface MobilePhone{
   //functions here with no definition...
   //ex:
   //public void makeCall();
}
```


```java
public class CameraPhone implements Camera, MobilePhone{
   //functions here...
}
```


## Julia


Julia supports inheritance via abstract types. In Julia, multiple dispatch allows objects of different types to have the same function interfaces. Julia also can support traits via parameters in type declarations or with macros. This makes multiple inheritance in Julia mostly unnecessary, except for the inconvenience of composing the data in a mixed type when declaring multiple similar types, for which there are macros.<br /> <br />For example, the functions <code> dialnumber(equipment, name) </code> and <code> video(equipment, filename) </code> could be used as generic interfaces to implement methods for a <code>Telephone</code>, a <code>Camera</code>, and a <code>SmartPhone</code>, and Julia would dispatch according to the type of the equipment.
```julia

abstract type Phone end

struct DeskPhone <: Phone
    book::Dict{String,String}
end

abstract type Camera end

struct kodak
    roll::Vector{Array{Int32,2}}
end

struct CellPhone <: Phone
    book::Dict{String,String}
    roll::Vector{AbstractVector}
end

function dialnumber(phone::CellPhone)
    println("beep beep")
end

function dialnumber(phone::Phone)
    println("tat tat tat tat")
end

function snap(camera, img)
    println("click")
    push!(roll, img)
end

dphone = DeskPhone(Dict(["information" => "411"]))
cphone = CellPhone(Dict(["emergency" => "911"]), [[]])

dialnumber(dphone)
dialnumber(cphone)

```
```txt

tat tat tat tat
beep beep

```



## Kotlin

Interfaces in Kotlin are very similar to Java 8. They can contain declarations of abstract methods, as well as method implementations.<br/>
What makes them different from abstract classes is that interfaces cannot store state. They can have properties but these need
to be abstract or to provide accessor implementations.


```scala
interface Camera {
    val numberOfLenses : Int
}

interface MobilePhone {
    fun charge(n : Int) {
        if (n >= 0)
            battery_level = (battery_level + n).coerceAtMost(100)
    }

    var battery_level : Int
}

data class CameraPhone(override val numberOfLenses : Int = 1, override var battery_level: Int) : Camera, MobilePhone
data class TwinLensCamera(override val numberOfLenses : Int = 2) : Camera

fun main(args: Array<String>) {
    val c = CameraPhone(1, 50)
    println(c)
    c.charge(35)
    println(c)
    c.charge(78)
    println(c)
    println(listOf(c.javaClass.superclass) + c.javaClass.interfaces)
    val c2 = TwinLensCamera()
    println(c2)
    println(listOf(c2.javaClass.superclass) + c2.javaClass.interfaces)
}
```

```txt
CameraPhone(numberOfLenses=1, battery_level=50)
CameraPhone(numberOfLenses=1, battery_level=85)
CameraPhone(numberOfLenses=1, battery_level=100)
[class java.lang.Object, interface multiple_inheritance.Camera, interface multiple_inheritance.MobilePhone]
TwinLensCamera(numberOfLenses=2)
[class java.lang.Object, interface multiple_inheritance.Camera]
```



## Lasso

Lasso only allow single inheritance.
But it supports the use of multiple traits
and trays hand down the methods it has implemented provided that the type
fulfills the requirements for the trait. [http://lassoguide.com/language/traits.html http://lassoguide.com/language/traits.html]

```Lasso
define trait_camera => trait {
	require zoomfactor

	provide has_zoom() => {
		return .zoomfactor > 0
	}

}

define trait_mobilephone => trait {
	require brand

	provide is_smart() => {
		return .brand == 'Apple'
	}

}

define cameraphone => type {

	trait {
		import trait_camera, trait_mobilephone
	}

	data public zoomfactor::integer = 0,
		public brand::string

}

local(mydevice = cameraphone)

#mydevice -> brand = 'Apple'
#mydevice -> zoomfactor = 0

#mydevice -> has_zoom
'<br />'
#mydevice -> is_smart
```

-> false

true


## Lingo

Lingo does not support multiple inheritance. But its slightly idiosyncratic inheritance implementation based on "ancestors" allows to assign/change inheritance relations at runtime. So a similar (but not identical) effect can be achieved by something like this:

```lingo
-- parent script "Camera"
property resolution

on new (me)
  me.resolution = "1024x768"
  return me
end

on snap (me)
  put "SNAP!"
end
```



```lingo
-- parent script "MobilePhone"
property ringtone

on new (me)
  me.ringtone = "Bell"
  return me
end

on ring (me, n)
  repeat with i = 1 to n
    put "RING!!!"
  end repeat
end
```



```lingo
-- parent script "CameraPhone"
property ancestor

on new (me)
  c = script("Camera").new()
  mp = script("MobilePhone").new()

  -- make the Camera instance a parent of the MobilePhone instance
  mp.setProp(#ancestor, c)

  -- make the MobilePhone instance a parent of this CameraPhone instance
  me.ancestor = mp

  return me
end
```


Usage:

```lingo
cp = script("CameraPhone").new()

cp.snap()
-- "SNAP!"

cp.ring(3)
-- "RING!!!"
-- "RING!!!"
-- "RING!!!"

put cp.resolution
-- "1024x768"

put cp.ringtone
-- "Bell"
```



## Logtalk

Logtalk supports multiple inheritance.
There is no "class" keyword in Logtalk;
an "object" keyword is used instead (Logtalk objects play the role of classes, meta-classes, instances, or prototypes depending on the relations with other objects).

```logtalk
:- object(camera,
    ...).
    ...
:- end_object.
```



```logtalk
:- object(mobile_phone,
    ...).
    ...
:- end_object.
```



```logtalk
:- object(camera_phone,
    specializes(camera, mobile_phone),
    ...).
    ...
:- end_object.
```



## Lua

Lua is prototype-based. A table cannot have more than one metatable,
but it can reference more than one in its __index metamethod,
by making it a closure.


```lua
function setmetatables(t,mts) --takes a table and a list of metatables
  return setmetatable(t,{__index = function(self, k)
    --collisions are resolved in this implementation by simply taking the first one that comes along.
    for i, mt in ipairs(mts) do
      local member = mt[k]
      if member then return member end
    end
  end})
end

camera = {}
mobilephone = {}
cameraphone = setemetatables({},{camera,mobilephone})
```



## M2000 Interpreter


```M2000 Interpreter

Module CheckIt {
      Class Camera {
            IamCamera
      }
      Class MobilePhone {
            IamMobilePhone
      }
      Class CameraPhone {
            Module CameraPhone {
            \\ we can use : This=Camera() : This=MobilePhone()
            \\ but we have to use Final for Functions/Modules in This
            \\ So using M, a new local variable, we can accumulate Inheritance
                  M=Camera()
                  M=MobilePhone()
                  M=This
                  This=M
            }
      }
      CP1=CameraPhone()
      Print Valid(CP1.IamCamera)=True, Valid(CP1.IamMobilePhone)=true
}
CheckIt

```



## Nemerle

Like C#, Nemerle only allows pseudo-multiple inheritance through interfaces.
In Nemerle, the base class must be listed before any interfaces.

```nemerle
interface ICamera {
    // ...
}

class MobilePhone {
    // ...
}

class CameraPhone: MobilePhone, ICamera {
    // ...
}
```



## NetRexx

Like [[Java]], NetRexx doesn't allow true multiple inheritance but instead restricts that capability to interfaces.
NetRexx permits the ''implementation'' of multiple interfaces.
All methods in interfaces are implicitly abstract, thus when you implement an interface you must implement its specified methods.

In this sample the class/interface names are augmented over those required in the task to prevent namespace pollution.
The sample also provides a complete working implementation to demonstrate the capability.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

class RInheritMultiple public
  method main(args = String[]) public static
    iPhone = RInheritMultiple_CameraPhone()
    if iPhone <= RInheritMultiple_Camera then -
      say -
      'Object' hashToString(iPhone) '['iPhone.getClass().getSimpleName()']' -
      'is a' RInheritMultiple_Camera.class.getSimpleName()
    if iPhone <= RInheritMultiple_MobilePhone then -
      say -
      'Object' hashToString(iPhone) '['iPhone.getClass().getSimpleName()']' -
      'is a' RInheritMultiple_MobilePhone.class.getSimpleName()
    say iPhone.snap()
    say iPhone.call()
    return
  method hashToString(that = Object) public static
    return '@'(Rexx that.hashCode()).d2x().right(8, 0)

class RInheritMultiple_Camera private interface
  -- properties follow...
  shutter = 'click...'
  -- method prototypes follow
  method snap() public returns Rexx

class RInheritMultiple_MobilePhone private interface
  -- properties follow...
  ringTone = 'ring...'
  -- method prototypes follow
  method call() public returns Rexx

class RInheritMultiple_CameraPhone private -
  implements -
    RInheritMultiple_Camera, -
    RInheritMultiple_MobilePhone -
  uses -
    RInheritMultiple_Camera, -
    RInheritMultiple_MobilePhone
  method RInheritMultiple_CameraPhone() public
    return
  -- method implementations follow
  method snap() public
    return shutter
  method call() public
    return ringTone
```

```txt

Object @7F546C85 [RInheritMultiple_CameraPhone] is a RInheritMultiple_Camera
Object @7F546C85 [RInheritMultiple_CameraPhone] is a RInheritMultiple_MobilePhone
click...
ring...
```


=={{header|Objective-C}}==
Like Java, Objective-C does not allow multiple inheritance,
but a class can "conform to" multiple protocols.
All methods in protocols are abstract (they don't have an implementation).
When you conform to a protocol you need to implement the specified methods.

If you simply want to combine the functionality (method implementations)
of multiple classes, you can use message forwarding to mimic the functionality of those classes without actually inheriting them, as described in [http://support.apple.com/kb/TA45894 this guide]:


```objc
@interface Camera : NSObject {
}
@end

@implementation Camera
@end

@interface MobilePhone : NSObject {
}
@end

@implementation MobilePhone
@end

@interface CameraPhone : NSObject {
  Camera *camera;
  MobilePhone *phone;
}
@end

@implementation CameraPhone

-(instancetype)init {
  if ((self = [super init])) {
    camera = [[Camera alloc] init];
    phone = [[MobilePhone alloc] init];
  }
  return self;
}

-(void)forwardInvocation:(NSInvocation *)anInvocation {
  SEL aSelector = [anInvocation selector];
  if ([camera respondsToSelector:aSelector])
    [anInvocation invokeWithTarget:camera];
  else if ([phone respondsToSelector:aSelector])
    [anInvocation invokeWithTarget:phone];
  else
    [self doesNotRecognizeSelector:aSelector];
}

-(NSMethodSignature *)methodSignatureForSelector:(SEL)aSelector {
  return [camera methodSignatureForSelector:aSelector]
  ?: [phone methodSignatureForSelector:aSelector]
  ?: [super methodSignatureForSelector:aSelector];
}

-(BOOL)respondsToSelector:(SEL)aSelector {
  return [camera respondsToSelector:aSelector]
  || [phone respondsToSelector:aSelector]
  || [super respondsToSelector:aSelector];
}

@end
```


Caveat: the CameraPhone class will still technically not inherit from
either the Camera or MobilePhone classes, so testing a CameraPhone object with <code>-isKindOfClass:</code> with the Camera or MobilePhone classes will still fail.


## OCaml


```ocaml
class camera =
  object (self)
    (*functions go here...*)
  end
```


```ocaml
class mobile_phone =
  object (self)
    (*functions go here...*)
  end
```


```ocaml
class camera_phone =
  object (self)
    inherit camera
    inherit mobile_phone
    (*functions go here...*)
  end
```




## Oforth


Oforth does not implement multiple inheritance. It allows only one parent class.

Oforth implements properties (like Comparable, Indexable, ...). A property can have attributes and methods.
A class can have multiple properties.

If Camera and MobilePhone are designed as properties, we can write :


```Oforth
Property new: Camera
Property new: MobilePhone

Object Class new: CameraPhone
CameraPhone is: Camera
CameraPhone is: MobilePhone
```



## ooRexx

ooRexx classes have a single superclass and can inherit from multiple mixins.
Mixins are more than just interfaces.
They can contain concrete method implementations and also create instance variables (scoped as private variables to the mixin methods).

```ooRexx

-- inherited classes must be created as mixinclasses.
::class phone mixinclass object

::class camera mixinclass object

-- not a direct subclass of either, but inherits both
::class cameraphone inherit phone camera

-- could also be
::class cameraphone1 subclass phone inherit camera

-- or

::class cameraphone2 subclass camera inherit phone
```



## OxygenBasic


```oxygenbasic
class Camera
  string cbuf
  method TakePhoto()
  end method
  method ViewPhoto()
  end method
end class

class MobilePhone
  string pbuf
  method MakeCall()
  end method
  method TakeCall()
  end method
end class

class CameraPhone
  has Camera,MobilePhone
end class

CameraPhone cp

cp.ViewPhoto
cp.MakeCall
```



## Oz


```oz
class Camera end

class MobilePhone end

class CameraPhone from Camera MobilePhone end
```



## Pascal

See [[Inheritance/Multiple#Delphi | Delphi]]


## Perl


```perl
package Camera;
#functions go here...
1;
```



```perl
package MobilePhone;
#functions go here...
1;
```



```perl
package CameraPhone;
use Camera;
use MobilePhone;
@ISA = qw( Camera MobilePhone );
#functions go here...
1;
```


or


```perl
package CameraPhone;
use base qw/Camera MobilePhone/;
#functions go here...
```


The same using the [http://search.cpan.org/perldoc?MooseX::Declare MooseX::Declare] extention:

```perl
use MooseX::Declare;

class Camera {
    # methods ...
}
class MobilePhone {
    # methods ...
}
class CameraPhone extends(Camera, MobilePhone) {
    # methods ...
}
```



## Perl 6


```perl6
class Camera {}
class MobilePhone {}
class CameraPhone is Camera is MobilePhone {}

say CameraPhone.^mro;     # undefined type object
say CameraPhone.new.^mro; # instantiated object
```

```txt
CameraPhone() Camera() MobilePhone() Any() Mu()
CameraPhone() Camera() MobilePhone() Any() Mu()
```


The <tt>.^mro</tt> is not an ordinary method call,
but a call to the object's metaobject
that returns the method resolution order for this type.


## PicoLisp


```PicoLisp
(class +Camera)

(class +MobilePhone)
```



## Pop11


```pop11
;;; load object support
lib objectclass;

define :class Camera;
   ;;; slots go here
enddefine;

define :class MobilePhone;
   ;;; slots go here
enddefine;

define :class CameraPhone is Camera, MobilePhone;
   ;;; extra slots go here
enddefine;

;;; methods go here
```



## PowerShell

```PowerShell

class Camera {}
class MobilePhone {}
class CameraPhone : Camera, MobilePhone {}

```




## PureBasic

Using the open-source precompiler [http://www.development-lounge.de/viewtopic.php?t=5915 SimpleOOP].

```PureBasic
Class Camera
EndClass

Class Mobil
EndClass

Class CameraMobile Extends Camera Extends Mobil
EndClass
```



## Python


```python
class Camera:
  pass #functions go here...
```



```python
class MobilePhone:
  pass #functions go here...
```



```python
class CameraPhone(Camera, MobilePhone):
  pass #functions go here...
```



## Racket


Racket allows multiple inheritance with interfaces, but not classes.
Mixins can be used to achieve some of the benefits of multiple inheritance.


```racket
#lang racket

(define camera<%>       (interface ()))
(define mobile-phone<%> (interface ()))

(define camera-phone%
  (class* object% (camera<%> mobile-phone<%>)
    (super-new)
    ;; implement methods here
    ))
```



## Ring


```ring

# Project : Inheritance/Multiple

mergemethods(:CameraPhone,:MobilePhone)

o1 = new CameraPhone
? o1
? o1.testCamera()
? o1.testMobilePhone()

func AddParentClassAttributes oObject,cClass
    # Add Attributes
        cCode = "oTempObject = new " + cClass
        eval(cCode)
        for cAttribute in Attributes(oTempObject)
            AddAttribute(oObject,cAttribute)
            cCode = "oObject." + cAttribute + " = oTempObject." + cAttribute
            eval(cCode)
        next


class Camera
    Name = "Camera"
    func testCamera
        ? "Message from testCamera"

class MobilePhone
    Type = "Android"
    func testMobilePhone
        ? "Message from MobilePhone"

class CameraPhone from Camera

    # Add MobilePhone Attributes
        AddParentClassAttributes(self,:MobilePhone)

```

Output:

```txt

name: Camera
type: Android
Message from testCamera
Message from MobilePhone

```



## Ruby

Ruby does not have multiple inheritance, but you can mix modules into classes:

```ruby
module Camera
  # define methods here
end
class MobilePhone
  # define methods here
end
class CameraPhone < MobilePhone
  include Camera
  # define methods here
end
```



## Rust


```Rust
trait Camera {}
trait MobilePhone {}
trait CameraPhone: Camera + MobilePhone {}
```



## Scala


```scala
trait Camera
trait MobilePhone
class CameraPhone extends Camera with MobilePhone
```



## Self

Self is a class-free, object-oriented language, and as such, it uses prototypal inheritance instead of classical inheritance. This is an example of the relevant excerpts from a Self transporter fileout. Normally the object tree would be built and navigated within the graphical Self environment.

```self
camera = ()
```


```self
mobilePhone = ()
```


```self
cameraPhone = (| cameraParent* = camera. mobilePhoneParent* = mobilePhone |)
```



## Sidef


```ruby
class Camera {};
class MobilePhone {};
class CameraPhone << Camera, MobilePhone {};
```



## Slate



```slate
define: #Camera.
define: #MobilePhone.
define: #CameraPhone &parents: {Camera. MobilePhone}.
```


## Swift

Like Objective-C, Swift does not allow multiple inheritance. However, you can conform to multiple protocols.

```Swift
protocol Camera {

}

protocol Phone {

}

class CameraPhone: Camera, Phone {

}
```



## Tcl

```tcl
package require TclOO

oo::class create Camera
oo::class create MobilePhone
oo::class create CameraPhone {
    superclass Camera MobilePhone
}
```




## zkl


```zkl
class Camera{} class MobilePhone{}
class CameraPhone(Camera,MobilePhone){}
CameraPhone.linearizeParents
```

{{out}}Show the class search order

```txt
L(Class(CameraPhone),Class(Camera),Class(MobilePhone))
```


