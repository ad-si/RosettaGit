+++
title = "Classes"
description = ""
date = 2019-10-18T04:03:03Z
aliases = []
[extra]
id = 1844
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
languages = [
  "11l",
  "actionscript",
  "ada",
  "aikido",
  "algol_68",
  "amigae",
  "arturo",
  "autohotkey",
  "basic",
  "bbc_basic",
  "blz",
  "bracmat",
  "c",
  "clojure",
  "cobol",
  "coco",
  "coffeescript",
  "common_lisp",
  "component_pascal",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "delphi",
  "dm",
  "dwscript",
  "e",
  "echolisp",
  "eiffel",
  "elena",
  "erre",
  "factor",
  "falcon",
  "fancy",
  "fantom",
  "forth",
  "fortran",
  "freebasic",
  "glsl",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lasso",
  "lfe",
  "lingo",
  "lisaac",
  "logtalk",
  "lua",
  "m2000_interpreter",
  "matlab",
  "miniscript",
  "nanoquery",
  "nemerle",
  "netrexx",
  "nim",
  "objeck",
  "object_pascal",
  "ocaml",
  "oforth",
  "ol",
  "oorexx",
  "oxygenbasic",
  "oz",
  "pascal",
  "perl",
  "perl_6",
  "phl",
  "php",
  "picolisp",
  "pop11",
  "powershell",
  "processing",
  "purebasic",
  "python",
  "r",
  "racket",
  "rapidq",
  "raven",
  "realbasic",
  "rebol",
  "ring",
  "ruby",
  "rust",
  "sather",
  "scala",
  "scheme",
  "sidef",
  "simula",
  "slate",
  "smalltalk",
  "supercollider",
  "swift",
  "tcl",
  "tiscript",
  "txr",
  "unix_shell",
  "vala",
  "vba",
  "visual_basic_.net",
  "visual_foxpro",
  "xlisp",
  "zkl",
  "zonnon",
]
+++

In [[object-oriented programming]] '''class''' is a set (a [[wp:Transitive_closure|transitive closure]]) of types bound by the relation of [[inheritance]]. It is said that all types derived from some base type T and the type T itself form a class T.

The first type T from the class T sometimes is called the '''root type''' of the class.

A class of types itself, as a type, has the values and operations of its own.
The operations of are usually called '''methods''' of the root type.
Both operations and values are called [[polymorphism | polymorphic]].

A polymorphic operation (method) selects an implementation depending on the actual specific type of the polymorphic argument.

The action of choice the type-specific implementation of a polymorphic operation is called '''dispatch'''. Correspondingly, polymorphic operations are often called '''dispatching''' or '''virtual'''.
Operations with multiple arguments and/or the results of the class are called '''multi-methods'''.
A further generalization of is the operation with arguments and/or results from different classes.

* single-dispatch languages are those that allow only one argument or result to control the dispatch. Usually it is the first parameter, often hidden, so that a prefix notation ''x''.''f''() is used instead of mathematical ''f''(''x'').
* multiple-dispatch languages allow many arguments and/or results to control the dispatch.



A polymorphic value has a type tag indicating its specific type from the class and the corresponding specific value of that type.
This type is sometimes called '''the most specific type''' of a [polymorphic] value.
The type tag of the value is used in order to resolve the dispatch.
The set of polymorphic values of a class is a transitive closure of the sets of values of all types from that class.

In many [[object-oriented programming | OO]] languages
the type of the class of T and T itself are considered equivalent.
In some languages they are distinct (like in [[Ada]]).
When class T and T are equivalent, there is no way to distinguish
polymorphic and specific values.


## Task

Create a basic class with a method, a constructor, an instance variable and how to instantiate it.





## 11l


```11l
T MyType
   Int public_variable // member variable = instance variable
   . Int private_variable

   F () // constructor
      .private_variable = 0

   F someMethod() // member function = method
      .private_variable = 1
      .public_variable = 10
```


Note that member functions in 11l by default are not polymorphic; if you want a polymorphic member function, you have to mark it as virtual. Example:

```11l
T MyType
   F.virtual.new someMethod() -> N // this is polymorphic
      print()
```



## ActionScript


```actionscript
package {
    public class MyClass {

        private var myVariable:int;  // Note: instance variables are usually "private"

        /**
         * The constructor
         */
        public function MyClass() {
            // creates a new instance
        }

        /**
         * A method
         */
        public function someMethod():void {
            this.myVariable = 1; // Note: "this." is optional
            // myVariable = 1; works also
        }
    }
}
```



## Ada

Class is used in many languages to provide both encapsulation, or grouping of data and actions, and type definition. Ada packages provide encapsulation or grouping while type definitions are done using the ''type'' reserved word. Types participating in inheritance are named ''tagged'' record types.

A package specification has the following form:

```ada
package My_Package is
   type My_Type is tagged private;
   procedure Some_Procedure(Item : out My_Type);
    function Set(Value : in Integer) return My_Type;
private
   type My_Type is tagged record
      Variable : Integer := -12;
   end record;
end My_Package;
```

The type declaration at the top of the package gives public visibility to the private tagged type My_Type. Since My_Type is declared to be private, the public has no visibility of its structure. The type must be treated as a black box. The private section of the package specification includes the actual tagged record definition. Note that the data member Variable is initialized to -12. This corresponds to a default constructor for the type.

The package body must contain the implementation of the procedures and functions declared in the package specification.

```ada
 package body My_Package is
    procedure Some_Procedure(Item : out My_Type) is
    begin
       Item := 2 * Item;
    end Some_Procedure;

    function Set(Value : Integer) return My_Type is
       Temp : My_Type;
    begin
       Temp.Variable := Value;
       return Temp;
    end Set;
end My_Package;
```

The Set function acts as a conversion constructor for My_Type.

An instance is typically created outside the package:

```ada
with My_Package; use My_Package;

procedure Main is
   Foo : My_Type; -- Foo is created and initialized to -12
begin
   Some_Procedure(Foo); -- Foo is doubled
   Foo := Set(2007); -- Foo.Variable is set to 2007
end Main;
```



## Aikido

Aikido provides classes with single inheritance and multiple interface implementation.  A class takes a set of constructor arguments and provides a set of public functions, operators, classes, monitors and threads.

```aikido
class Circle (radius, x, y) extends  Shape (x, y) implements Drawable {
    var myvec = new Vector (x, y)

    public function draw() {
        // draw the circle
    }
}
```



## ALGOL 68

{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - had to remove calls to ''space'' and even then code generates "Run time fault (aborting): attempt to jump to missing ELSE/OUT clause"
}}
The following code is experimental.  Basically ALGOL 68 is not object oriented, so the task to create (and use of) objects is tedious due to the lack of certain constructs, especially the lack of OO syntactic sugar.

For further details:
* [http://archive.computerhistory.org/resources/text/algol/algol_bulletin/A26/P22.HTM Ross, D.T., "Features Essential for a Workable Algol X," ALGOL Bulletin, No. 26, August 1967, pp. 6-12, and ACM SIGPLAN Notices, Vol.2, No.11, November 1967.]
Other examples of this experimental approach are located at pages: [[Life in two dimensions#ALGOL 68|Life in two dimensions]], [[Playing Cards#ALGOL 68|Playing Cards]] and [[Stack#ALGOL 68|Stack]].


```algol68
MODE MYDATA = STRUCT(
    INT name1
);
STRUCT(
    INT name2,
    PROC (REF MYDATA)REF MYDATA new,
    PROC (REF MYDATA)VOID init,
    PROC (REF MYDATA)VOID some method
) class my data;
class my data := (
  # name2 := # 2, # Class attribute #

  # PROC new := # (REF MYDATA new)REF MYDATA:(
        (init OF class my data)(new);
        new
   ),

  # PROC init := # (REF MYDATA self)VOID:(
        """ Constructor  (Technically an initializer rather than a true 'constructor') """;
        name1 OF self := 0 # Instance attribute #
    ),

  # PROC some method := # (REF MYDATA self)VOID:(
        """ Method """;
        name1 OF self := 1;
        name2 OF class my data := 3
    )
);

# class name, invoked as a function is the constructor syntax #
REF MYDATA my data = (new OF class my data)(LOC MYDATA);

MODE GENDEROPT = UNION(STRING, VOID);
MODE AGEOPT = UNION(INT, VOID);

MODE MYOTHERDATA = STRUCT(
    STRING name,
    GENDEROPT gender,
    AGEOPT age
);
STRUCT (
    INT count,
    PROC (REF MYOTHERDATA, STRING, GENDEROPT, AGEOPT)REF MYOTHERDATA new,
    PROC (REF MYOTHERDATA, STRING, GENDEROPT, AGEOPT)VOID init,
    PROC (REF MYOTHERDATA)VOID del
) class my other data;
class my other data := (
  # count := # 0,  # Population of "(init OF class my other data)" objects #
# PROC new := # (REF MYOTHERDATA new, STRING name, GENDEROPT gender, AGEOPT age)REF MYOTHERDATA:(
          (init OF class my other data)(new, name, gender, age);
          new
      ),

  # PROC init := # (REF MYOTHERDATA self, STRING name, GENDEROPT gender, AGEOPT age)VOID:(
        """ One initializer required, others are optional (with different defaults) """;
        count OF class my other data +:= 1;
        name OF self := name;
        gender OF self := gender;
        CASE gender OF self IN
            (VOID):gender OF self := "Male"
        ESAC;
        age OF self := age
    ),

  # PROC del := # (REF MYOTHERDATA self)VOID:(
        count OF class my other data -:= 1
    )
);

PROC attribute error := STRING: error char; # mend the error with the "error char" #

# Allocate the instance from HEAP #
REF MYOTHERDATA person1 =  (new OF class my other data)(HEAP MYOTHERDATA, "John", EMPTY, EMPTY);
print (((name OF person1), ": ",
        (gender OF person1|(STRING gender):gender|attribute error), " "));  # "John Male" #
print (((age OF person1|(INT age):age|attribute error), new line)); # Raises AttributeError exception! #

# Allocate the instance from LOC (stack) #
REF MYOTHERDATA person2 = (new OF class my other data)(LOC MYOTHERDATA, "Jane", "Female", 23);
print (((name OF person2), ": ",
        (gender OF person2|(STRING gender):gender|attribute error), " "));
print (((age OF person2|(INT age):age|attribute error), new line))  # "Jane Female 23" #
```

```txt

John: Male *
Jane: Female         +23

```



## AmigaE


```amigae
OBJECT a_class
  varA, varP
ENDOBJECT

-> this could be used like a constructor
PROC init() OF a_class
  self.varP := 10
  self.varA := 2
ENDPROC

-> the special proc end() is for destructor
PROC end() OF a_class
-> nothing to do here...
ENDPROC

-> a not so useful getter
PROC getP() OF a_class IS self.varP

PROC main()
  DEF obj : PTR TO a_class
  NEW obj.init()
  WriteF('\d\n', obj.varA)   -> this can be done, while
                             -> varP can't be accessed directly
  WriteF('\d\n', obj.varP)   -> or
  WriteF('\d\n', obj.getP())
  END obj
ENDPROC
```



## Arturo



```arturo
// Let's define our class

Person #{
	// first the class variables
	name 	""
	surname	""
	age		0

	// then the constructor (optional)
	init [n,s,a]{
		name 	n
		surname s
		age		a
	}

	// then another method (optional again, ofc)
	sayHello {
		print "Hello " + name + "!"
	}
}

// Let's create a new Person object
person $(new ~Person "John" "Doe" 33)

// Let's invoke an object method
person.sayHello

// Let's access some object variable
print "the person's age is: " + person.age

// Let's change some of the object's properties
person.surname "boom"

// And let's print our object again
// to see what we've done
log person
```


```txt
Hello John!
the person's age is: 33
#{
	age             33
	init            <function: 0x1015A8D40>
	name            "John"
	sayHello        <function: 0x1015A8DA0>
	surname         "boom"
}
```



## AutoHotkey

AutoHotkey_L is prototype-based. However, for convenience, class-syntax may be used to create a base object.

```AutoHotkey
obj := new MyClass
obj.WhenCreated()

class MyClass {
; Instance Variable #1
   time := A_Hour ":" A_Min ":" A_Sec

; Constructor
   __New() {
      MsgBox, % "Constructing new object of type: " this.__Class
      FormatTime, date, , MM/dd/yyyy
   ; Instance Variable #2
      this.date := date
   }
; Method
   WhenCreated() {
      MsgBox, % "Object created at " this.time " on " this.date
   }
}
```



## BASIC

```basic
  DECLARE SUB MyClassDelete (pthis AS MyClass)
  DECLARE SUB MyClassSomeMethod (pthis AS MyClass)
  DECLARE SUB MyClassInit (pthis AS MyClass)

  TYPE MyClass
    Variable AS INTEGER
  END TYPE

  DIM obj AS MyClass
  MyClassInit obj
  MyClassSomeMethod obj

  SUB MyClassInit (pthis AS MyClass)
    pthis.Variable = 0
  END SUB

  SUB MyClassSomeMethod (pthis AS MyClass)
    pthis.Variable = 1
  END SUB
```



## BBC BASIC

```bbcbasic
      INSTALL @lib$+"CLASSLIB"

      REM Declare the class:
      DIM MyClass{variable, @constructor, _method}
      DEF MyClass.@constructor MyClass.variable = PI : ENDPROC
      DEF MyClass._method = MyClass.variable ^ 2

      REM Register the class:
      PROC_class(MyClass{})

      REM Instantiate the class:
      PROC_new(myclass{}, MyClass{})

      REM Call the method:
      PRINT FN(myclass._method)

      REM Discard the instance:
      PROC_discard(myclass{})
```



## blz


```blz

# Constructors can take parameters (that automatically become properties)
constructor Ball(color, radius)

	# Objects can also have functions (closures)
	:volume
		return 4/3 * {pi} * (radius ** 3)
	end
	:show
		return "a " + color + " ball with radius " + radius
	end

end

red_ball = Ball("red", 2)
print(red_ball)
# => a red ball with radius 2

```



## Bracmat

Bracmat has no class-inheritance. Any object can function as a template for creating other objects.

```bracmat
( ( resolution
  =   (x=)
      (y=)
      (new=.!arg:(?(its.x),?(its.y)))
  )
& new$(resolution,640,480):?VGA
& new$(resolution,1920,1080):?1080p
& out$("VGA: horizontal " !(VGA..x) " vertical " !(VGA..y)));
```

```txt
VGA: horizontal  640  vertical  480
```



## C

```c

#include <stdlib.h>

typedef struct sMyClass
{
  int variable;
} *MyClass;

MyClass  MyClass_new()
{
  MyClass pthis = malloc(sizeof *pthis);
  pthis->variable = 0;
  return pthis;
}

void MyClass_delete(MyClass* pthis)
{
  if (pthis)
  {
    free(*pthis);
    *pthis = NULL;
  }
}

void MyClass_someMethod(MyClass pthis)
{
  pthis->variable = 1;
}

MyClass obj = MyClass_new();
MyClass_someMethod(obj);
MyClass_delete(&obj);
```



## C++

```cpp
class MyClass
{
public:
  void someMethod(); // member function = method
  MyClass(); // constructor
private:
  int variable; // member variable = instance variable
};

// implementation of constructor
MyClass::MyClass():
  variable(0)
{
  // here could be more code
}

// implementation of member function
void MyClass::someMethod()
{
  variable = 1; // alternatively: this->variable = 1
}

// Create an instance as variable
MyClass instance;

// Create an instance on free store
MyClass* pInstance = new MyClass;
// Instances allocated with new must be explicitly destroyed when not needed any more:
delete pInstance;
```


Note: <tt>MyClass instance();</tt> would ''not'' define an instance, but declare a function returning an instance. Accidentally declaring functions when object definitions are wanted is a rather common bug in C++.

Functions can also be defined inline:


```cpp
class MyClass
{
public:
  MyClass(): variable(0) {}
  void someMethod() { variable = 1; }
private:
  int variable;
};
```


Note that member functions in C++ by default are ''not'' polymorphic; if you want a polymorphic member function, you have to mark it as virtual. In that case, you should also add a virtual destructor, even if that is empty. Example:


```cpp
class MyClass
{
public:
  virtual void someMethod(); // this is polymorphic
  virtual ~MyClass(); // destructor
};
```


## C#

```c#
public class MyClass
{
    public MyClass()
    {
    }
    public void SomeMethod()
    {
    }
    private int _variable;
    public int Variable
    {
        get { return _variable; }
        set { _variable = value; }
    }
    public static void Main()
    {
        // instantiate it
        MyClass instance = new MyClass();
        // invoke the method
        instance.SomeMethod();
        // set the variable
        instance.Variable = 99;
        // get the variable
        System.Console.WriteLine( "Variable=" + instance.Variable.ToString() );
    }
}
```



## Clojure

Clojure gives you several options, and to help you decide which is more appropriate to use, see the [https://github.com/cemerick/clojure-type-selection-flowchart/ Clojure type selection flowchart].

defrecord example:

```clojure

; You can think of this as an interface
(defprotocol Foo (getFoo [this]))

; Generates Example1 Class with foo as field, with method that returns foo.
(defrecord Example1 [foo] Foo (getFoo [this] foo))

; Create instance and invoke our method to return field value
(-> (Example1. "Hi") .getFoo)
"Hi"
```



## COBOL

<!-- This took far too long to create and to get to compile. -->

```cobol
       IDENTIFICATION DIVISION.
       CLASS-ID. my-class INHERITS base.

       *> The 'INHERITS base' and the following ENVIRONMENT DIVISION
       *> are optional (in Visual COBOL).
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           CLASS base.

           *> There is no way (as far as I can tell) of creating a
           *> constructor. However, you could wrap it with another
           *> method to achieve the desired effect.
           *>...

           OBJECT.
               *> Instance data
               DATA DIVISION.
               WORKING-STORAGE SECTION.
               01  instance-variable PIC 9(8).

               *> Properties can have getters and setters automatically
               *> generated.
               01  a-property        PIC 9(8) PROPERTY.

               PROCEDURE DIVISION.

               METHOD-ID. some-method.
               PROCEDURE DIVISION.
                   *> ...
               END METHOD some-method.
           END OBJECT.
       END CLASS my-class.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. example-class-use.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           *> These declarations brings the class and property into
           *> scope.
           CLASS my-class
           PROPERTY a-property.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       *> Declaring a my-class reference variable.
       01  instance USAGE OBJECT REFERENCE my-class.

       PROCEDURE DIVISION.

           *> Invoking a static method or (in this case) a constructor.
           INVOKE my-class "new" RETURNING instance

           *> Invoking an instance method.
           INVOKE instance "some-method"

           *> Using the setter and getter of a-property.
           MOVE 5 TO a-property OF instance
           DISPLAY a-property OF instance

           GOBACK
           .

       END PROGRAM example-class-use.
```



## Coco


```coco
class Rectangle
  # The constructor is defined as a bare function. This
  # constructor accepts one argument and automatically assigns it
  # to an instance variable.
  (@width) ->

  # Another instance variable.
  length: 10

  # A method.
  area: ->
    @width * @length

# Instantiate the class using the 'new' operator.
rect = new Rectangle 2
```



## CoffeeScript


```coffeescript
# Create a basic class
class Rectangle
  # Constructor that accepts one argument
  constructor: (@width) ->

  # An instance variable
  length: 10

  # A method
  area: () ->
    @width * @length

# Instantiate the class using the new operator
rect = new Rectangle 2
```



## Common Lisp


```lisp
(defclass circle ()
  ((radius :initarg :radius
           :initform 1.0
           :type number
           :reader radius)))

(defmethod area ((shape circle))
  (* pi (expt (radius shape) 2)))

> (defvar *c* (make-instance 'circle :radius 2))
> (area *c*)
12.566370614359172d0
```



## Component Pascal

BlackBox Component Builder

Module that defines a Class

```oberon2

MODULE Point;
IMPORT
	Strings;
TYPE
	Instance* = POINTER TO LIMITED RECORD
		x-, y- : LONGINT; (* Instance variables *)
	END;

	PROCEDURE (self: Instance) Initialize*(x,y: LONGINT), NEW;
	BEGIN
		self.x := x;
		self.y := y
	END Initialize;

	(* constructor *)
	PROCEDURE New*(x, y: LONGINT): Instance;
	VAR
		point: Instance;
	BEGIN
		NEW(point);
		point.Initialize(x,y);
		RETURN point
	END New;

	(* methods *)
	PROCEDURE (self: Instance) Add*(other: Instance): Instance, NEW;
	BEGIN
		RETURN New(self.x + other.x,self.y + other.y);
	END Add;

	PROCEDURE (self: Instance) ToString*(): POINTER TO ARRAY OF CHAR, NEW;
	VAR
		xStr,yStr: ARRAY 64 OF CHAR;
		str: POINTER TO ARRAY OF CHAR;
	BEGIN
		Strings.IntToString(self.x,xStr);
		Strings.IntToString(self.y,yStr);
		NEW(str,128);str^ := "@(" +xStr$ + "," + yStr$ + ")";
		RETURN str
	END ToString;
END Point.

```

Module that uses previous class

```oberon2

MODULE DrivePoint;
IMPORT
	Point,
	StdLog;

PROCEDURE Do*;
VAR
	p,q: Point.Instance;
BEGIN
	p := Point.New(1,2);
	q := Point.New(2,1);
	StdLog.String(p.ToString() + " + " + q.ToString() + " = " + p.Add(q).ToString());StdLog.Ln;
	StdLog.String("p.x:> ");StdLog.Int(p.x);StdLog.Ln;
	StdLog.String("p.y:> ");StdLog.Int(p.y);StdLog.Ln
END Do;

END DrivePoint.

```

Execute: ^Q DrivePoint.Do<br/>
```txt

@(1,2) + @(2,1) = @(3,3)
p.x:>  1
p.y:>  2

```



## Crystal

```crystal
class MyClass

  def initialize
    @instance_var = 0
  end

  def add_1
    @instance_var += 1
  end

end

my_class = MyClass.new

```


## D


```d
import std.stdio;

class MyClass {
    //constructor (not necessary if empty)
    this() {}

    void someMethod() {
        variable = 1;
    }

    // getter method
    @property int variable() const {
        return variable_;
    }

    // setter method
    @property int variable(int newVariable) {
        return variable_ = newVariable;
    }

    private int variable_;
}

void main() {
    // On default class instances are allocated on the heap
    // The GC will manage their lifetime
    auto obj = new MyClass();

    // prints 'variable = 0', ints are initialized to 0 by default
    writeln("variable = ", obj.variable);

    // invoke the method
    obj.someMethod();

    // prints 'variable = 1'
    writeln("variable = ", obj.variable);

    // set the variable using setter method
    obj.variable = 99;

    // prints 'variable = 99'
    writeln("variable = ", obj.variable);
}
```



## Delphi


```Delphi
program SampleClass;

{$APPTYPE CONSOLE}

type
  TMyClass = class
  private
    FSomeField: Integer; // by convention, fields are usually private and exposed as properties
  public
    constructor Create;
    destructor Destroy; override;
    procedure SomeMethod;
    property SomeField: Integer read FSomeField write FSomeField;
  end;

constructor TMyClass.Create;
begin
  FSomeField := -1
end;

destructor TMyClass.Destroy;
begin
  // free resources, etc

  inherited Destroy;
end;

procedure TMyClass.SomeMethod;
begin
  // do something
end;


var
  lMyClass: TMyClass;
begin
  lMyClass := TMyClass.Create;
  try
    lMyClass.SomeField := 99;
    lMyClass.SomeMethod();
  finally
    lMyClass.Free;
  end;
end.
```



## DM

In DM, all "classes" are part of the "object tree". Instance variables, procs (functions), ... are all defined inside this "tree".
Adding elements (procs, variables, classes) to this tree is done by defining the name and such.


```DM>s</lang


This declares a type "/s" at the root of the tree, which can now be instantiated.

A more complicated example:


```DM

// Declare the class "/foo"
foo
    // Everything inside the indented block is relative to the parent, "/foo" here.
    // Instance variable "bar", with a default value of 0
    // Here, var/bar is relative to /foo, thus it becomes "/foo/var/bar" ultimately.
    var/bar = 0

    // The "New" proc is the constructor.
    New()
        // Constructor code.

    // Declares a proc called "Baz" on /foo
    proc/baz()
        // Do things.

// Instantiation code.
// Overriding /client/New() means it is ran when a client connects.
/client/New()
    ..()
    var/foo/x = new /foo()
    x.bar = 10 // Assign to the instance variable.
    x.baz() // Call "baz" on our instance.

```


This is enough to declare a


## DWScript

Methods can be implemented inline or out-of-line, this sample illustrates both.

```Delphi
type
  TMyClass = class
  private
    FSomeField: Integer; // by convention, fields are usually private and exposed as properties
  public
    constructor Create;
    begin
       FSomeField := -1;
    end;
    procedure SomeMethod;
    property SomeField: Integer read FSomeField write FSomeField;
  end;

procedure TMyClass.SomeMethod;
begin
  // do something
end;


var lMyClass: TMyClass;

lMyClass := new TMyClass; // can also use TMyClass.Create

lMyClass.SomeField := 99;
lMyClass.SomeMethod;
```



## E

In E, classes, constructors, and instance variables are not built into the language. This is an example of the basic convention; different cases may call for objects built in different ways.


```e
def makeColor(name :String) {
    def color {
        to colorize(thing :String) {
          return `$name $thing`
        }
    }
    return color
}
```


Example interactive session creating and using it:


```e
? def red := makeColor("red")
# value: <color>

? red.colorize("apple")
# value: "red apple"
```



## Eiffel


###  The Most Basic Form of Class

The shortest way to write an Eiffel class is to have the class keyword, followed by the name of the class (all caps), and ending with the end keyword.

```Eiffel

class MY_CLASS
end

```


=== Add a Creation Procedure (Constructor) ===

```Eiffel

class MY_CLASS

create
   make

feature {NONE} -- Initialization

   make
       -- This is a creation procedure or "Constructor".
    do
       create my_string.make_empty
    end

end

```

=== Add Multiple Creation Procedures (Constructors) ===
In Eiffel, you may have more than one creation procedure (or "Constructor").

```Eiffel

class MY_CLASS

create                      -- Here we are declaring ...
   make,                    -- In the Feature group (below) we are coding
   make_this_way,           -- each of these declared creation procedures.
   make_that_way,           -- We can have as many constructors as we need.
   make_another_way,
   a_name_other_than_make

feature {NONE} -- Initialization

   make
       -- This is a creation procedure or "Constructor".
    do
       -- Initialization code goes here ...
    end

   make_this_way
       -- Make this way, rather than a plain ole "make".
    do
       -- Initialization code goes here ...
    end

   make_that_way
       -- Create that way rather than this way (above).
    do
       -- Initialization code goes here ...
    end

   make_another_way
       -- And still another way to create MY_CLASS.
    do
       -- Initialization code goes here ...
    end

   a_name_other_than_make
       -- There is no requirement to use the word "make".
       -- The word "make" is just a naming convention.
    do
       -- Initialization code goes here ...
    end

end

```


=== Add some Properties & Methods ===
Below, we've added three attributes (i.e. "Properties"). The "make" is not only a "Constructor" (Creation Procedure), but also an example of a "Method".

```Eiffel

class MY_CLASS

create
   make

feature {NONE} -- Initialization

   make
       -- This is a creation procedure or "Constructor".
    do
       create my_string.make_empty
    end

feature -- Access (Properties)

   my_string: STRING
         -- This is a comment about `my_string', which is a "Property".

   my_integer: INTEGER
         -- Unlike `my_string' (above), the INTEGER type is an "Expanded Type".
         -- This means INTEGER objects know how to self-initialize.

   my_date: DATE
         -- This attribute (or "Property") will need to be initialized.
         -- One way to do that is to make a self-initializing attribute, thus ...
      attribute
         create Result.make_now
      end

feature -- Basic Operations (Methods)

   do_something
         -- Loop over and print the numbers 1 to 100 to the console.
     do
        across 1 |..| 100 as i loop print (i.out) end
     end

   do_something_else
         -- Set a and b and print the result.
     local
        a, b, c: INTEGER
     do
        a := 1
        b := 2
        c := a + b
     end

end

```



## EchoLisp


```lisp

(lib 'gloops) ; load oo library

(define-class Person null (name (age :initform 66)))
(define-method tostring (Person) (lambda (p)  ( format "🚶 %a " p.name)))
(define-method mailto (Person Person) (lambda( p o) (printf "From %a to️  %a : ..." p o)))

;; define a sub-class of Person with same methods
(define-class Writer (Person)  (books))
(define-method tostring (Writer) (lambda (w)( format "🎩 %a" w.name)))
(define-method mailto (Person Writer)
	(lambda (p w) (printf " From %a (age %d). Dear writer of %a ..." p p.age  w.books )))


```

```lisp

;; instantiate
(define simone     (make-instance Person  :name  'simone :age 42)) ;; slots values by name
(define antoinette (make-instance Person :name 'antoinette :age 37))
(define albert     (Person "albert" 33)) ;; quick way : slots values in order
(define simon (make-instance Writer :name "simon"   :books '(my-life my-bike)))


(mailto simone simon) ;; method Person-Writer
(mailto simone antoinette) ;; method Person-Person
(mailto simon albert) ;; no method Writer-Person : call 'super' Person-Person
(mailto simon simon) ;; no mehod Writer-Writer : call 'super' Person-Writer
   →
   From 🚶 simone (age 42). Dear writer of (my-life my-bike) ...
   From 🚶 simone to️ 🚶 antoinette : ...
   From 🎩 simon to️ 🚶 albert : ...
   From 🎩 simon (age 66). Dear writer of (my-life my-bike) ...

```



## Elena

ELENA 4.x :

```elena
import extensions;

class MyClass
{
    prop int Variable;

    someMethod()
    {
        Variable := 1
    }

    constructor()
    {
    }
}

public program()
{
    // instantiate the class
    var instance := new MyClass();

    // invoke the method
    instance.someMethod();

    // get the variable
    console.printLine("Variable=",instance.Variable)
}
```



## ERRE

ERRE isn't OOP-oriented, but with new PC version 3.0 is possibile to define classes and instance variables, like in this example:

```ERRE
PROGRAM CLASS2_DEMO

CLASS QUADRATO

    LOCAL LATO

    PROCEDURE GETLATO(L)
       LATO=L
    END PROCEDURE

    PROCEDURE AREA(->A)
       A=LATO*LATO
    END PROCEDURE

    PROCEDURE PERIMETRO(->P)
       P=4*LATO
    END PROCEDURE

END CLASS

NEW P:QUADRATO,Q:QUADRATO

BEGIN
    P_GETLATO(10)
    P_AREA(->AREAP)
    PRINT(AREAP)
    Q_GETLATO(20)
    Q_PERIMETRO(->PERIMETROQ)
    PRINT(PERIMETROQ)
END PROGRAM

```

The answers is
 100
 80

=={{header|F_Sharp|F#}}==
A minimal example as required by the task description:

```fsharp
type MyClass(init) =      // constructor with one argument: init
  let mutable var = init  // a private instance variable
  member x.Method() =     // a simple method
    var <- var + 1
    printfn "%d" var

// create an instance and use it
let myObject = new MyClass(42)
myObject.Method()
```


A somewhat more meaningful example, inspired by the Haskell version:

```fsharp
open System

type Shape =
  abstract Perimeter: unit -> float
  abstract Area: unit -> float

type Circle(radius) =
  interface Shape with
    member x.Perimeter() = 2.0 * radius * Math.PI
    member x.Area() = Math.PI * radius**2.0

type Rectangle(width, height) =
  interface Shape with
    member x.Perimeter() = 2.0 * width + 2.0 * height
    member x.Area() = width * height
```



## Falcon

Falcon classes are a mix of data and code that can be used to instantiate objects. Classes are defined below. Note: inh1...inhN can also be passed the param_list.

```falcon
class class_name[ ( param_list ) ] [ from inh1[, inh2, ..., inhN] ]
    [ static block ]
    [ properties declaration ]
    [init block]
    [method list]
end
```

Example of a class:

```falcon
class mailbox( max_msg )

   capacity = max_msg * 10
   name = nil
   messages = []

    init
      printl( "Box now ready for ", self.capacity, " messages." )
    end

    function slot_left()
      return  self.capacity - len( self.messages )
    end

end
```


Instantiation:

```falcon
m = mailbox( 10 )
// Ouputs: Box now ready for 100 messages.
```



## Factor


```factor
TUPLE: my-class foo bar baz ;
M: my-class quux foo>> 20 + ;
C: <my-class> my-class
10 20 30 <my-class> quux ! result: 30
TUPLE: my-child-class < my-class quxx ;
C: <my-child-class> my-child-class
M: my-child-class foobar 20 >>quux ;
20 20 30 <my-child-class> foobar quux ! result: 30
```



## Fancy


```fancy
class MyClass {
  read_slot: 'instance_var # creates getter method for @instance_var
  @@class_var = []

  def initialize {
    # 'initialize' is the constructor method invoked during 'MyClass.new' by convention
    @instance_var = 0
  }

  def some_method {
    @instance_var = 1
    @another_instance_var = "foo"
  }

  # define class methods: define a singleton method on the class object
  def self class_method {
    # ...
  }

  # you can also name the class object itself
  def MyClass class_method {
    # ...
  }
}

myclass = MyClass new
```



## Fantom


```Fantom
class MyClass
{
  // an instance variable
  Int x

  // a constructor, providing default value for instance variable
  new make (Int x := 1)
  {
    this.x = x
  }

  // a method, return double the number x
  public Int double ()
  {
    return 2 * x
  }
}

class Main
{
  public static Void main ()
  {
    a := MyClass (2)  // instantiates the class, with x = 2
    b := MyClass()    // instantiates the class, x defaults to 1
    c := MyClass { x = 3 }  // instantiates the class, sets x to 3
  }
}
```



## Forth

ANSI Forth has no object oriented features, but as Forth is a very easy language to extend, many object oriented programming systems have been implemented for it over the years. WinForth has one such system, which is described here.

Declare a class


```forth
:class MyClass <super Object

  int memvar

  :m ClassInit: ( -- )
       ClassInit: super
       1 to memvar ;m

  :m ~: ( -- )  ." Final " show: [ Self ] ;m

  :m set: ( n -- )  to memvar ;m
  :m show: ( -- ) ." Memvar = " memvar . ;m

;class
```


Allocate a static object

```forth>MyClass newInstance</lang


Allocate a dynamic object, saving its pointer in a global variable.

```forth>New> MyClass  value newInstance</lang


Call member functions

```forth
10 set: newInstance
show: newInstance
```


Free a dynamically allocated object


```forth
newInstance dispose
0 to newInstance   \ no dangling pointers!
```


Example of dynamic allocation and local variable use"


```forth
: test { \ obj -- }
    New> MyClass to obj
      show: obj
      1000 set: obj
    obj dispose ;
```



Works with any ANS Forth

Needs the FMS-SI (single inheritance) library code located here:
http://soton.mpeforth.com/flag/fms/index.html

```forth
include FMS-SI.f

:class foo   \ begin class foo definition
 ivar x                   \ declare an instance variable named x
 :m put ( n -- ) x ! ;m   \ a method/message definition
 :m init: 10 self put ;m  \ the constructor method
 :m print x ? ;m          \ a print method for x
;class                    \ end class foo definition

foo f1    \ instantiate a foo object, in the dictionary, named f1
f1 print  \ 10   send the print message to object f1
20 f1 put \ send a message with one parameter to the object
f1 print \ 20


: bar  \ bar is a normal Forth function definition
  heap> foo  \ instantiate a nameless object in the heap
  dup print
  30 over put
  dup print
  <free ;  \ destroy the heap object

: bar'  \ bar' is an alternative to bar that uses a local variable
  heap> foo  {: f :}
  f print
  30 f put
  f print
  f <free ;

bar  \ 10 30
bar' \ 10 30

```



## Fortran

Creating abstract derived type (abstract class), extended derived types, using constructor and finalization, pointers etc. Works with gfortran 5.0 and intel ifort 15.0.2


```fortran

!-----------------------------------------------------------------------
!Module accuracy defines precision and some constants
!-----------------------------------------------------------------------
module accuracy_module
   implicit none
   integer, parameter, public :: rdp = kind(1.d0)
   ! constants
   real(rdp), parameter :: pi=3.141592653589793238462643383279502884197_rdp
end module accuracy_module

!-----------------------------------------------------------------------
!Module typedefs_module contains abstract derived type and extended type definitions.
! Note that a reserved word "class" in Fortran is used to describe
! some polymorphic variable  whose data type may vary at run time.
!-----------------------------------------------------------------------
module typedefs_module
   use accuracy_module
   implicit none

   private ! all
   public :: TPoint, TShape, TCircle, TRectangle, TSquare ! public only these defined derived types

   ! abstract derived type
   type, abstract :: TShape
      real(rdp) :: area
      character(len=:),allocatable :: name
   contains
      ! deferred method i.e. abstract method =  must be overridden in extended type
      procedure(calculate_area), deferred,pass :: calculate_area
   end type TShape
   ! just declaration of the abstract method/procedure for TShape type
   abstract interface
      function  calculate_area(this)
         use accuracy_module
         import TShape !imports TShape type from host scoping unit and makes it accessible here
         implicit none
         class(TShape) :: this
         real(rdp) :: calculate_area

      end function calculate_area
   end interface

   ! auxiliary derived type
   type TPoint
      real(rdp) :: x,y
   end type TPoint

   ! extended derived type
   type, extends(TShape) :: TCircle
      real(rdp) :: radius
      real(rdp), private :: diameter
      type(TPoint) :: centre
   contains
      procedure, pass :: calculate_area => calculate_circle_area
      procedure, pass :: get_circle_diameter
      final :: finalize_circle
   end type TCircle

   ! extended derived type
   type, extends(TShape) :: TRectangle
      type(TPoint) :: A,B,C,D
   contains
      procedure, pass :: calculate_area => calculate_rectangle_area
      final :: finalize_rectangle
   end type TRectangle

   ! extended derived type
   type, extends(TRectangle) :: TSquare
   contains
      procedure, pass :: calculate_area => calculate_square_area
      final :: finalize_square
   end type TSquare

 contains

   ! finalization subroutines for each type
   ! They called recursively, i.e. finalize_rectangle
   ! will be called after finalize_square subroutine
   subroutine finalize_circle(x)
      type(TCircle), intent(inout) :: x
      write(*,*) "Deleting TCircle object"
   end subroutine finalize_circle

   subroutine finalize_rectangle(x)
      type(TRectangle), intent(inout) :: x
      write(*,*) "Deleting also TRectangle object"
   end subroutine finalize_rectangle

   subroutine finalize_square(x)
      type(TSquare), intent(inout) :: x
      write(*,*) "Deleting TSquare object"
   end subroutine finalize_square

   function calculate_circle_area(this)
      implicit none
      class(TCircle) :: this
      real(rdp) :: calculate_circle_area
      this%area = pi * this%radius**2
      calculate_circle_area = this%area
   end function calculate_circle_area

   function calculate_rectangle_area(this)
      implicit none
      class(TRectangle) :: this
      real(rdp) :: calculate_rectangle_area
      ! here could be more code
      this%area = 1
      calculate_rectangle_area = this%area
   end function calculate_rectangle_area

   function calculate_square_area(this)
      implicit none
      class(TSquare) :: this
      real(rdp) :: calculate_square_area
      ! here could be more code
      this%area = 1
      calculate_square_area = this%area
   end function calculate_square_area

   function  get_circle_diameter(this)
      implicit none
      class(TCircle) :: this
      real(rdp) :: get_circle_diameter
      this % diameter = 2.0_rdp * this % radius
      get_circle_diameter = this % diameter
   end function get_circle_diameter

end module typedefs_module

!-----------------------------------------------------------------------
!Main program
!-----------------------------------------------------------------------
program    rosetta_class
   use accuracy_module
   use typedefs_module
   implicit none

   ! we need this subroutine in order to show the finalization
   call test_types()

 contains

   subroutine test_types()
      implicit none
      ! declare object of type TPoint
      type(TPoint), target :: point
      ! declare object of type TCircle
      type(TCircle),target :: circle
      ! declare object of type TSquare
      type(TSquare),target :: square

      ! declare pointers
      class(TPoint), pointer :: ppo
      class(TCircle), pointer :: pci
      class(TSquare), pointer :: psq

      !constructor
      point = TPoint(5.d0,5.d0)
      ppo => point
      write(*,*) "x=",point%x,"y=",point%y

      pci => circle

      pci % radius = 1
      write(*,*) pci % radius
      ! write(*,*) pci % diameter !No,it is a PRIVATE component
      write(*,*) pci % get_circle_diameter()
      write(*,*) pci % calculate_area()
      write(*,*) pci % area

      psq => square

      write(*,*) psq % area
      write(*,*) psq % calculate_area()
      write(*,*) psq % area
   end subroutine test_types

end program rosetta_class


```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Type MyClass
  Private:
    myInt_ As Integer
  Public:
    Declare Constructor(myInt_ As Integer)
    Declare Property MyInt() As Integer
    Declare Function Treble() As Integer
End Type

Constructor MyClass(myInt_ As Integer)
   This.myInt_ = myInt_
End Constructor

Property MyClass.MyInt() As Integer
  Return myInt_
End Property

Function MyClass.Treble() As Integer
  Return 3 * myInt_
End Function

Dim mc As MyClass = MyClass(24)
Print mc.MyInt, mc.Treble()
Print "Press any key to quit the program"
Sleep
```


```txt

 24            72

```



## GLSL

There are no classes in GLSL, but they can be simulated using structs:

```glsl

struct Rectangle{
    float width;
    float height;
};

Rectangle new(float width,float height){
    Rectangle self;
    self.width = width;
    self.height = height;
    return self;
}

float area(Rectangle self){
    return self.width*self.height;
}

float perimeter(Rectangle self){
    return (self.width+self.height)*2.0;
}

```



## Go

The task describes several concepts concerning class methods before giving some task requirements.  The following code satisfies the task requirements.  The concepts described however, are more involved.  A discussion of these concepts follows.

```go
package main

import "fmt"

// a basic "class."
// In quotes because Go does not use that term or have that exact concept.
// Go simply has types that can have methods.
type picnicBasket struct {
    nServings int // "instance variables"
    corkscrew bool
}

// a method (yes, Go uses the word method!)
func (b *picnicBasket) happy() bool {
    return b.nServings > 1 && b.corkscrew
}

// a "constructor."
// Also in quotes as Go does not have that exact mechanism as part of the
// language.  A common idiom however, is a function with the name new<Type>,
// that returns a new object of the type, fully initialized as needed and
// ready to use.  It makes sense to use this kind of constructor function when
// non-trivial initialization is needed.  In cases where the concise syntax
// shown is sufficient however, it is not idiomatic to define the function.
// Rather, code that needs a new object would simply contain &picnicBasket{...
func newPicnicBasket(nPeople int) *picnicBasket {
    // arbitrary code to interpret arguments, check resources, etc.
    // ...
    // return data new object.
    // this is the concise syntax.  there are other ways of doing it.
    return &picnicBasket{nPeople, nPeople > 0}
}

// how to instantiate it.
func main() {
    var pb picnicBasket          // create on stack (probably)
    pbl := picnicBasket{}        // equivalent to above
    pbp := &picnicBasket{}       // create on heap.  pbp is pointer to object.
    pbn := new(picnicBasket)     // equivalent to above
    forTwo := newPicnicBasket(2) // using constructor
    // equivalent to above.  field names, called keys, are optional.
    forToo := &picnicBasket{nServings: 2, corkscrew: true}

    fmt.Println(pb.nServings, pb.corkscrew)
    fmt.Println(pbl.nServings, pbl.corkscrew)
    fmt.Println(pbp)
    fmt.Println(pbn)
    fmt.Println(forTwo)
    fmt.Println(forToo)
}
```

```txt

0 false
0 false
&{0 false}
&{0 false}
&{2 true}
&{2 true}

```

'''Transitive closure based on inheritance'''

Method polymorphism exists in Go with a type called ''interface''.  A Go interface is a method set.  A type is said to ''satisfy'' an interface if it has defined on it all methods of the interface.  If interface A is a subset of interface B then A satisfies B, so a transitive closure exists on this concept of satisfying an interface.

Inheritance is not involved however.  A type satisfies an interface automatically when all methods of the interface are defined on the type.  Inheritance is not involved because the interface being satisfied is not mentioned in any way, either in the type satisfying the interface or in its methods.  The writer of a type and methods need not even be aware that the interface being satisfied exists.

'''Root type'''

The empty interface, with an empty method set and written as interface{}, is the “root type” in this context of transitive closure of interface satisfaction.  All types satisfy the empty interface, since it makes no requirements that any methods be defined at all.  interface{} is often used in go as a type that can hold a data value of any type.  For example, note how fmt.Println, used above, accepts arguments of any type.  It's (variadic) argument is of type interface{}.

'''Polymorphic dispatch'''

This happens when a method is called through an interface.  Consider this code in addition to the example above.

```go
import reflect

type happinessTester interface {
    happy() bool
}

type bottleOfWine struct {
    USD   float64
    empty bool
}

func (b *bottleOfWine) happy() bool {
    return b.USD > 10 && !b.empty
}

func main() {
    partySupplies := []happinessTester{
        &picnicBasket{2, true},
        &bottleOfWine{USD: 6},
    }
    for _, ps := range partySupplies {
        fmt.Printf("%s: happy? %t\n",
            reflect.Indirect(reflect.ValueOf(ps)).Type().Name(),
            ps.happy())
    }
}
```

On the last line, in the call to ps.happy(), ps is of the interface type happinessTester.  The method actually called is based on the underlying concrete type.
For the method call, this is called the receiver type and the variable b (in both happy methods) is called the receiver.  Dispatch is based on this single receiver so Go is a single dispatch kind of language.

'''Type tag'''

Go maintains something equivalent in its internal representation of interfaces.  In place of direct access to internal data, Go's reflect package provides a number of functions for inspecting types and returning useful information.
Shown above for example, is code recovering the name of the concrete type underlying the dynamic type of the interface.

{{out}} for second example:

```txt

picnicBasket: happy? true
bottleOfWine: happy? false

```


'''Distinction of types and classes'''

To the extent that an interface represents a class, it is distinct from a type that satisfies it.  Interface is one kind of type, but an object of any type can satisfy an interface.  The two types&mdash;the interface type and the type satisfying the interface&mdash;are distinct.


## Groovy

A class:

```groovy
/** Ye olde classe declaration */
class Stuff {
    /** Heare bee anne instance variable declared */
    def guts

    /** This constuctor converts bits into Stuff */
    Stuff(injectedGuts) {
        guts = injectedGuts
    }

    /** Brethren and sistren, let us flangulate with this fine flangulating method */
    def flangulate() {
        println "This stuff is flangulating its guts: ${guts}"
    }
}
```


A demonstration:

```groovy
def stuff = new Stuff('''
I have made mistakes in the past.
I have made mistakes in the future.
    -- Vice President Dan Quayle
''')

stuff.flangulate()

stuff.guts = '''
Our enemies are innovative and resourceful, and so are we.
They never stop thinking about new ways to harm our country and our people,
and neither do we.
    -- President George W. Bush
'''

stuff.flangulate()
```


```txt
This stuff is flangulating its guts:
I have made mistakes in the past.
I have made mistakes in the future.
    -- Vice President Dan Quayle

This stuff is flangulating its guts:
Our enemies are innovative and resourceful, and so are we.
They never stop thinking about new ways to harm our country and our people,
and neither do we.
    -- President George W. Bush
```



## Haskell

Haskell is entirely statically typed; that is, the type of every expression is completely determined at compile-time.
Hence, the usual approach to object-oriented programming, in which the actual method invoked by a method call isn't determined until runtime (think of C++'s virtual functions), is impossible in Haskell 98.
Haskell's type classes allow for polymorphic functions, but all the polymorphism happens at compile-time (think of C++ templates) without the use of language extensions (existential types).

```haskell
class Shape a where
    perimeter :: a -> Double
    area      :: a -> Double
{- A type class Shape. Types belonging to Shape must support two
methods, perimeter and area. -}

data Rectangle = Rectangle Double Double
{- A new type with a single constructor. In the case of data types
which have only one constructor, we conventionally give the
constructor the same name as the type, though this isn't mandatory. -}

data Circle = Circle Double

instance Shape Rectangle where
    perimeter (Rectangle width height) = 2 * width + 2 * height
    area      (Rectangle width height) = width * height
{- We made Rectangle an instance of the Shape class by
implementing perimeter, area :: Rectangle -> Int. -}

instance Shape Circle where
    perimeter (Circle radius) = 2 * pi * radius
    area      (Circle radius) = pi * radius^2

apRatio :: Shape a => a -> Double
{- A simple polymorphic function. -}
apRatio shape = area shape / perimeter shape

main = do
    print $ apRatio $ Circle 5
    print $ apRatio $ Rectangle 5 5
{- The correct version of apRatio (and hence the correct
implementations of perimeter and area) is chosen based on the type
of the argument. -}
```


The primary way to simulate run-time polymorphism in Haskell is to use a single algebraic data type with multiple constructors, rather than several types belonging to a single class.


```haskell
data Shape = Rectangle Double Double | Circle Double
{- This Shape is a type rather than a type class. Rectangle and
Circle are its constructors. -}

perimeter :: Shape -> Double
{- An ordinary function, not a method. -}
perimeter (Rectangle width height) = 2 * width + 2 * height
perimeter (Circle radius)          = 2 * pi * radius

area :: Shape -> Double
area (Rectangle width height) = width * height
area (Circle radius)          = pi * radius^2

apRatio :: Shape -> Double
{- Technically, this version of apRatio is monomorphic. -}
apRatio shape = area shape / perimeter shape

main = do
    print $ apRatio $ Circle 5
    print $ apRatio $ Rectangle 5 5
{- The value returned by apRatio is determined by the return values
of area and perimeter, which just happen to be defined differently
for Rectangles and Circles. -}
```


== Icon and {{header|Unicon}} ==
Unicon supports classes.

```Unicon
class Example (x) # 'x' is a field in class

  # method definition
  method double ()
    return 2 * x
  end

  # 'initially' block is called on instance construction
  initially (x)
    if /x # if x is null (not given), then set field to 0
      then self.x := 0
      else self.x := x
end

procedure main ()
  x1 := Example ()  # new instance with default value of x
  x2 := Example (2) # new instance with given value of x
  write (x1.x)
  write (x2.x)
  write (x2.double ()) # call a method
end
```



## J

'''Class definition:'''

```j
coclass 'exampleClass'

exampleMethod=: monad define
 1+exampleInstanceVariable
)

create=: monad define
 'this is the constructor'
)

exampleInstanceVariable=: 0
```


'''Instantiation:'''

```j
   exampleObject=: conew 'exampleClass'
```


Note that all J system defined utilities designed specifically to work on classes and objects have names which begin with the prefix <code>co</code>.


## Java


```java
public class MyClass{

  // instance variable
  private int variable;  // Note: instance variables are usually "private"

  /**
  * The constructor
  */
  public MyClass(){
    // creates a new instance
  }

  /**
  * A method
  */
  public void someMethod(){
   this.variable = 1;
  }
}
```

Note: "this." in someMethod is optional. "variable = 1;" works also. If a parameter also named "variable" came into someMethod, using "this" specifies using the instance variable rather than the local method variable. Instantiate this class using:

```java
new MyClass();
```



## JavaScript


### ES5

JavaScript is [[wp:Prototype-based programming|prototype-based]], so it doesn't have classes per se. Thinking in classes when coding JavaScript will only hinder you in the long run, but here's an example of JavaScript OO:


```javascript
//Constructor function.
function Car(brand, weight) {
  this.brand = brand;
  this.weight = weight || 1000; // Resort to default value (with 'or' notation).
}
Car.prototype.getPrice = function() { // Method of Car.
  return this.price;
}

function Truck(brand, size) {
  this.car = Car;
  this.car(brand, 2000); // Call another function, modifying the "this" object (e.g. "superconstructor".)
  this.size = size; // Custom property for just this object.
}
Truck.prototype = Car.prototype; // Also "import" the prototype from Car.

var cars = [ // Some example car objects.
  new Car("Mazda"),
  new Truck("Volvo", 2)
];
for (var i=0; i<cars.length; i++) {
  alert(cars[i].brand + " " + cars[i].weight + " " + cars[i].size + ", " +
      (cars[i] instanceof Car) + " " + (cars[i] instanceof Truck));
}
```

The alerts shows us:

```txt

Mazda 1000 undefined, true true
Volvo 2000 2, true true

```

The reason Car shows as instanceof Truck is because we've overwritten Truck.prototype with Car.prototype. It's probably not the best way to do it, but it suffices for most cases.


### ES6


```javascript
class Car {
  /**
   * A few brands of cars
   * @type {string[]}
   */
  static brands = ['Mazda', 'Volvo'];

  /**
   * Weight of car
   * @type {number}
   */
  weight = 1000;

  /**
   * Brand of car
   * @type {string}
   */
  brand;

  /**
   * Price of car
   * @type {number}
   */
  price;

  /**
   * @param {string} brand - car brand
   * @param {number} weight - mass of car
   */
  constructor(brand, weight) {
    if (brand) this.brand = brand;
    if (weight) this.weight = weight
  }

  /**
   * Drive
   * @param distance - distance to drive
   */
  drive(distance = 10) {
    console.log(`A ${this.brand} ${this.constructor.name} drove ${distance}cm`);
  }

  /**
   * Formatted stats string
   */
  get formattedStats() {
    let out =
      `Type: ${this.constructor.name.toLowerCase()}`
      + `\nBrand: ${this.brand}`
      + `\nWeight: ${this.weight}`;

    if (this.size) out += `\nSize: ${this.size}`;

    return out
  }
}

class Truck extends Car {
  /**
   * Size of truck
   * @type {number}
   */
  size;

  /**
   * @param {string} brand - car brand
   * @param {number} size - size of car
   */
  constructor(brand, size) {
    super(brand, 2000);
    if (size) this.size = size;
  }
}

let myTruck = new Truck('Volvo', 2);
console.log(myTruck.formattedStats);
myTruck.drive(40);
```


Output:

```txt
Type: truck
Brand: Volvo
Weight: 2000
Size: 2
A Volvo Truck drove 40cm
```



## Julia

Julia has inheritable types and abstract classes but does not have multiple inheritance.
Multiple dispatch is a core feature of the language.


```julia
abstract type Mammal end
habitat(::Mammal) = "planet Earth"

struct Whale <: Mammal
    mass::Float64
    habitat::String
end
Base.show(io::IO, ::Whale) = print(io, "a whale")
habitat(w::Whale) = w.habitat

struct Wolf <: Mammal
    mass::Float64
end
Base.show(io::IO, ::Wolf) = print(io, "a wolf")

arr = [Whale(1000, "ocean"), Wolf(50)]
println("Type of $arr is ", typeof(arr))
for a in arr
    println("Habitat of $a: ", habitat(a))
end
```


```txt
Type of Mammal[a whale, a wolf] is Array{Mammal,1}
Habitat of a whale: ocean
Habitat of a wolf: planet Earth
```



## Kotlin


```scala
class MyClass(val myInt: Int) {
   fun treble(): Int = myInt * 3
}

fun main(args: Array<String>) {
    val mc = MyClass(24)
    print("${mc.myInt}, ${mc.treble()}")
}
```


```txt

24, 72

```



## Lasso

In Lasso, a "class" is termed a "type"


```Lasso

define mytype => type {
	data
		public id::integer		= 0,
		public val::string		= '',
		public rand::integer	= 0

	public onCreate() => {
		// "onCreate" runs when instance created, populates .rand
		.rand = math_random(50,1)
	}
	public asString() => {
		return 'has a value of: "'+.val+'" and a rand number of "'+.rand+'"'
	}

}

local(x = mytype)
#x->val = '99 Bottles of beer'
#x->asString // outputs 'has a value of: "99 Bottles of beer" and a rand number of "48"'
```



## LFE



```lisp

(defmodule simple-object
  (export all))

(defun fish-class (species)
  "
  This is the constructor used internally, once the children and fish id are
  known.
  "
  (let ((habitat '"water"))
    (lambda (method-name)
      (case method-name
        ('habitat
          (lambda (self) habitat))
        ('species
          (lambda (self) species))))))

(defun get-method (object method-name)
  "
  This is a generic function, used to call into the given object (class
  instance).
  "
  (funcall object method-name))

; define object methods
(defun get-habitat (object)
  "Get a variable set in the class."
  (funcall (get-method object 'habitat) object))

(defun get-species (object)
  "Get a variable passed when constructing the object."
  (funcall (get-method object 'species) object))

```


Usage from the LFE REPL:

```lisp

> (slurp '"simple-object.lfe")
#(ok simple-object)
> (set my-fish (fish-class '"Carp"))
#Fun<lfe_eval.10.91765564>
> (get-habitat my-fish)
"water"
> (get-species my-fish)
"Carp"

```



## Lingo

'''Class definition:'''

```lingo
----------------------------------------
-- @desc      Class "MyClass"
-- @file      parent script "MyClass"
----------------------------------------

-- instance variable
property _myvar

-- constructor
on new (me)
  me._myvar = 23
  return me
end

-- a method
on doubleAndPrint (me)
  me._myvar = me._myvar * 2
  put me._myvar
end
```


'''Instantiation:'''

```lingo
foo = script("MyClass").new()
foo.doubleAndPrint()
-- 46
```



## Lisaac


```Lisaac
Section Header

+ name := SAMPLE;

Section Inherit

- parent : OBJECT := OBJECT;

Section Private

+ variable : INTEGER <- 0;

Section Public

- some_method <- (
  variable := 1;
);

- main <- (
  + sample : SAMPLE;

  sample := SAMPLE.clone;
  sample.some_method;
);
```



## Logtalk

The definition of classes in Logtalk require the use of meta-classes. In order to avoid infinite regression, we use here the usual trick of making a class an instance of itself. The class meta-class holds the constructor method, allowing the class to accept a message for creating a new instance. The class itself defines the methods and variables of its instances.

```logtalk
:- object(metaclass,
    instantiates(metaclass)).

    :- public(new/2).
    new(Instance, Value) :-
        self(Class),
        create_object(Instance, [instantiates(Class)], [], [state(Value)]).

:- end_object.

:- object(class,
    instantiates(metaclass)).

    :- public(method/1).
    method(Value) :-
        ::state(Value).

    :- private(state/1).

:- end_object.
```

A simple usage example after compiling and loading the above code:

```logtalk
| ?- class::new(Instance, 1).
Instance = o1
yes

| ?- o1::method(Value).
Value = 1
yes
```



## Lua

Classes in Lua are implemented with metatables. This doesn't implement a full system, but it gets the basic idea:

```lua
myclass = setmetatable({
__index = function(z,i) return myclass[i] end, --this makes class variables a possibility
setvar = function(z, n) z.var = n end
}, {
__call = function(z,n) return setmetatable({var = n}, myclass) end
})

instance = myclass(3)

print(instance.var) -->3

instance:setvar(6)

print(instance.var) -->6
```



## M2000 Interpreter


```M2000 Interpreter

Class zz {
      module bb {
            Superclass A {
                  unique:
                  counter
            }
            Superclass B1 {
                  unique:
                  counter
            }
            Superclass B2 {
                  unique:
                  counter
            }
            \\ We can make a group Alfa with a member, another group Beta
            \\ Group Beta can't see parent group, but can see own member groups
            \\ Group Alfa can see everything in nested groups, in any level,
            \\ but can't see inside modules/functions/operator/value/set
            Group Alfa {
                  Group Beta { }
            }
            Alfa=A
            Alfa.Beta=B1
            \\ we make 3 groups for marshaling counters
            \\ each group get a superclass
            Marshal1=A
            Marshal2=B1
            Marshal3=B2
            \\ Now we want to add functionality7
            \\ Inc module to add 1 to counter
            \\ a Value function to return counter
            \\ Without Value a group return a copy
            \\ If a group has a value then we can get copy using Group(nameofgroup)
            \\ just delete Group Marshal1 and remove Rem when we make Marshal1 using a class function
            Group Marshal1 {
                  Module Inc {
                        For SuperClass {.counter++}
                  }
                  Value {
                        For SuperClass {=.counter}
                  }
            }
            Class AnyMarshal {
                  Module Inc {
                        For SuperClass {.counter++}
                  }
                  Value {
                        For SuperClass {=.counter}
                  }
            }
            \\ here we merge groups
            Rem : Marshal1=AnyMarshal()
            Marshal2=AnyMarshal()
            Marshal3=AnyMarshal()

            \\ So now we see counters (three zero)
            Print Marshal1, Marshal2, Marshal3 \\ 0, 0, 0
            \\ Now we prepare Alfa and Alfa.Beta groups
            Group Alfa {
                  Group Beta {
                        Function SuperClass.Counter {
                              For SuperClass {
                                    =.counter
                              }
                        }
                  }
                  Module PrintData {
                        For SuperClass {
                              Print .counter, This.Beta.SuperClass.Counter()
                        }
                  }
            }
            \\ some marshaling to counters
            Marshal1.inc
            Marshal2.inc
            Marshal2.inc
            Marshal3.inc
            \\ lets print results
            Print Marshal1, Marshal2, Marshal3 \\ 1   2   1
            \\ Calling Alfa.PrintData
            Alfa.PrintData  \\ 1   2
            \\ Merging a group in a group make a change to superclass pointer inside group
            Alfa.Beta=B2 \\ change supeclass
            Alfa.PrintData  \\ 1   1
            For i=1 to 10 : Marshal3.inc : Next i
            Alfa.PrintData  \\ 1   11
            Alfa.Beta=B1 \\ change supeclass
            Alfa.PrintData  \\ 1   2
            Epsilon=Alfa
            Print Valid(@alfa as epsilon), Valid(@alfa.beta as epsilon.beta) \\ -1   -1
            Epsilon.PrintData \\ 1 2
            Alfa.Beta=B2 \\ change supeclass
            Alfa.PrintData  \\ 1   11
            Epsilon.PrintData \\ 1 2
            \\ validation being for top group superclass and all members if are same
            \\ but not for inner superclasses. This maybe change in later revisions of language.
            Print Valid(@alfa as epsilon), Valid(@alfa.beta as epsilon.beta) \\ -1  0

      }
}
Dim A(10)
A(3)=zz()
A(3).bb

```




## MATLAB

There are two ways to declare classes in MATLAB: with a classdef or without it. First you must create a folder named after the class type that you are defining with an "@" appended to the front, e.g. "@LinkedList", in your MATLAB root directory. In this folder you put all of the class methods and, if you have it, the classdef. Any MATLAB buitlin methods can be overloaded for any class you define. For example, if you want to overload the "+" operator, create an .m file in the class folder named "plus.m". Furthermore, all class variables have to be generated in the class constructor if a classdef is not going to be used.

Below are two examples of classes declared in MATLAB. GenericClass is defined without a classdef. GenericClass2 is defined with a classdef. The classes both do the exact same thing, the only difference between them is how they are defined.

@GenericClass

GenericClass.m: Class Constructor

```MATLAB
function GenericClassInstance = GenericClass(varargin)

        if isempty(varargin) %No input arguments
            GenericClassInstance.classVariable = 0; %Generates a struct
        else
            GenericClassInstance.classVariable = varargin{1}; %Generates a struct
        end

        %Converts the struct to a class of type GenericClass
        GenericClassInstance = class(GenericClassInstance,'GenericClass');

end
```

getValue.m:

```MATLAB
%Get function
function value = getValue(GenericClassInstance)
    value = GenericClassInstance.classVariable;
end
```

setValue.m:

```MATLAB
%Set function
function GenericClassInstance = setValue(GenericClassInstance,newValue)
   GenericClassInstance.classVariable = newValue;
end
```

display.m: This method overloads the "disp()" command

```MATLAB
function display(GenericClassInstance)
    disp(sprintf('%f',GenericClassInstance.classVariable));
end
```


Sample Usage:

```MATLAB>>
 myClass = GenericClass(3)
3.000000
>> myClass = setValue(myClass,pi)
3.141593
>> getValue(myClass)

ans =

   3.141592653589793
```


@GenericClass2
GenericClass2.m: This is the classdef, it includes the class constructor as well as class variables and methods.

```MATLAB
classdef GenericClass2

    properties
        classVariable
    end %properties

    methods

        %Class constructor
        function objectInstance = GenericClass2(varargin)
            if isempty(varargin) %No input arguments
                objectInstance.classVariable = 0;
            else
                objectInstance.classVariable = varargin{1};
            end
        end

        %Set function
        function setValue(GenericClassInstance,newValue)
            GenericClassInstance.classVariable = newValue;

            %MATLAB magic that changes the object in the scope that called
            %this set function.
            assignin('caller',inputname(1),GenericClassInstance);
        end

    end %methods
end
```

getValue.m:

```MATLAB
%Get function
function value = getValue(GenericClassInstance)
    value = GenericClassInstance.classVariable;
end
```

display.m: This method overloads the "disp()" command

```MATLAB
function display(GenericClassInstance)
    disp(sprintf('%f',GenericClassInstance.classVariable));
end
```


Sample Usage:

```MATLAB>>
 myClass = GenericClass2(3)
3.000000
>> setValue(myClass,pi)
>> getValue(myClass)

ans =

   3.141592653589793
```




## MiniScript


```MiniScript
// MiniScript is prototype based
Weapon = { "name": "Sword", "damage": 3 }
Weapon.slice = function()
    print "Did " + self.damage + " damage with " + self.name
end function

wep = new Weapon  // Same as: wep = { "__isa": Weapon }

wep.name = "Lance"

wep.slice
```



## Nanoquery


```nanoquery
class MyClass
	declare $name

	// constructors are methods with the same name as the class
	def MyClass($name)
		$name = $name
	end

	def getName()
		return $name
	end
end

// instantiate a new MyClass object
$inst = new("MyClass", "name string goes here")

// display the name value
println $inst.getName()
```



## Nemerle


```Nemerle
public class MyClass
{
    public this() { }  // the constructor in Nemerle is always named 'this'

    public MyVariable : int
    {
        get;
        set;
    }

    public MyMethod() : void
    {
    }

}

def myInstance = MyClass();                                           // no 'new' keyword needed
myInstance.MyVariable = 42;                                           // set MyVariable
System.Console.WriteLine($"My variable is $(myInstance.MyVariable)")  // get MyVariable
```



## NetRexx


```rexx
class ClassExample

  properties private -- class scope
  foo = int

  properties public  -- publicly visible
  bar = boolean

  properties indirect -- generates bean patterns
  baz = String()

  method main(args=String[]) static -- main method
    clsex = ClassExample()   -- instantiate
    clsex.foo = 42
    clsex.baz = 'forty-two'
    clsex.bar = 0 -- boolean false
    clsex.test(clsex.foo)
    clsex.test(clsex.bar)
    clsex.test(clsex.baz)

  method test(s=int)
    aap = 1 -- local (stack) variable
    say s aap

  method test(s=String)
    noot = 2
    say s noot

  method test(s=boolean)
    mies = 3
    say s mies
```




## Nim

```nim
type MyClass = object
  name: int

proc initMyClass(): MyClass =
  result.name = 2

proc someMethod(m: var MyClass) =
  m.name = 1

var mc = initMyClass()
mc.someMethod()

type
  Gender = enum male, female, other

  MyOtherClass = object
    name: string
    gender: Gender
    age: Natural

proc initMyOtherClass(name; gender = female; age = 50): auto =
  MyOtherClass(name: name, gender: gender, age: age)

var person1 = initMyOtherClass("Jane")
echo person1.name, " ", person1.gender, " ", person1.age # Jane female 50
var person2 = initMyOtherClass("John", male, 23)
echo person2.name, " ", person2.gender, " ", person2.age # John male 23
```


=={{header|Oberon-2}}==
```oberon2
MODULE M;

   TYPE
      T = POINTER TO TDesc;
      TDesc = RECORD
         x: INTEGER
      END;

   PROCEDURE New*(): T;
      VAR t: T;
   BEGIN
      NEW(t); t.x := 0;
      RETURN t
   END New;


   PROCEDURE (t: T) Increment*;
   BEGIN
      INC(t.x)
   END Increment;

END M.
```


Exported procedures are marked with an asterisk (*). There is nothing special about the constructor New, it is just a function that returns a new object of type T. The name of the method receiver can also be chosen freely. INC is a predeclared procedure that increments its argument.


## Objeck


```objeck
bundle Default {
  class MyClass {
    @var : Int;

    New() {
    }

    method : public : SomeMethod() ~ Nil {
    	@var := 1;
    }

    method : public : SetVar(var : Int) ~ Nil {
      @var := var;
    }

    method : public : GetVar() ~ Int {
      return @var;
    }
  }

  class Test {
    function : Main(args : String[]) ~ Nil {
      inst := MyClass->New();
      inst->GetVar()->PrintLine();

      inst->SomeMethod();
      inst->GetVar()->PrintLine();

      inst->SetVar(15);
      inst->GetVar()->PrintLine();
    }
  }
}
```



## Object Pascal

:''Note: This is not part of standard Pascal, but Turbo Pascal specific''


```pascal
type
 MyClass = object
            variable: integer;
            constructor init;
            destructor done;
            procedure someMethod;
           end;

constructor MyClass.init;
 begin
  variable := 0;
 end;

procedure MyClass.someMethod;
 begin
  variable := 1;
 end;

var
 instance: MyClass; { as variable }
 pInstance: ^MyClass; { on free store }

begin
 { create instances }
 instance.init;
 new(pInstance, init); { alternatively: pInstance := new(MyClass, init); }

 { call method }
 instance.someMethod;
 pInstance^.someMethod;

 { get rid of the objects }
 instance.done;
 dispose(pInstance, done);
end;
```


=={{header|Objective-C}}==
Interface:

```objc
// There are no class variables, so static variables are used.
static int myClassVariable = 0;

@interface MyClass : NSObject
{
    int variable; // instance variable
}

- (int)variable; // Typical accessor - you should use the same name as the variable

@end
```


Implementation:


```objc
@implementation MyClass

// Was not declared because init is defined in NSObject
- (instancetype)init
{
    if (self = [super init]) {
        variable = 0; // not strictly necessary as all instance variables are initialized to zero
    }
    return self;
}

- (int)variable
{
    return variable;
}

@end
```


Using the class:


```objc
// Creating an instance
MyClass *mc = [[MyClass alloc] init];

// Sending a message
[mc variable];
```



## OCaml


```ocaml
class my_class =
  object (self)
    val mutable variable = 0
    method some_method = variable <- 1
  end
```


Using the class:

```txt

# let instance = new my_class;;
val instance : my_class = <obj>
# instance#some_method;;
- : unit = ()

```



## Oforth

Class creation

```Oforth
Object Class new: MyClass(att)
MyClass method: initialize(v)  v := att ;
```


Usage : instantiation

```Oforth
MyClass new("some value")
```



## Ol

Otus Lisp have no classes support.


## ooRexx

ooRexx classes are defined using directives.  Only methods of the class can directly access instance variables to avoid fragile base class
problems, methods can only access variables at the level of the class hierarchy they are defined. ::attribute directives create setter and getter methods that allow instance variables to be accessed in other contexts.


```ooRexx
p = .point~new
c = .circle~new

p~print
c~print

::class point
::method init
  expose x y
  use strict arg x = 0, y = 0   -- defaults to 0 for any non-specified coordinates

::attribute x
::attribute y

::method print
  expose x y
  say "A point at location ("||x","y")"

::class circle subclass point
::method init
  expose radius
  use strict arg x = 0, y = 0, radius = 0
  self~init:super(x, y)        -- call superclass constructor

::attribute radius

::method print
  expose radius
  say "A circle of radius" radius "centered at location ("||self~x","self~y")"
```



## OxygenBasic

Example of a dynamic object. (statically defined objects do not require specific constructors and destructors.)

Parameter polymorphism is supported both by method overloading and also by automatic type conversion between integers, floats, strings and other primitives.


```oxygenbasic


class SuperString

indexbase 1

union
  bstring s
  sys     bs
  sys     *y
  int     *i
  byte    *b
  float   *f
end union

method space(sys n)
  s=space n
end method

method delete()
  freememory bs : bs=0
end method

method clear()
  sys j, le=length
  if le then
    for j=1 to le : b[j]=0 : next
  end if
end method

method length() as sys
  if bs then return i[0]
end method

method resize(sys n)
  sys le=length
  if n<le
    s=left s,n
  elseif n>le
    s+=nuls n-le
  end if
end method

method fill(string f)
  sys j, ls=length, lf=len f
  for j=1 to ls step lf
    mid s,j,f
  next
end method

method constructor()
end method

method destructor
  delete
end method

end class


'#recordof SuperString

'=====
'TESTS
'=====

new SuperString ss
'
ss.space 100
ss.resize 8
ss.fill "abc"
'
print ss.s    'result abcabcab
print ss.b[3] 'result 99: ascii for 'c'
'
del ss

```



## Oz

Classes are created at runtime and first-class values.

```oz
declare
  class Something
     feat
        name %% immutable, public attribute (called a "feature")
     attr
        count %% mutable, private attribute

     %% public method which is used as an initializer
     meth init(N)
        self.name = N
        count := 0
     end

     %% public method
     meth increase
        count := @count + 1
     end
  end
in
  %% create an instance
  Object = {New Something init("object")}

  %% call a method
  {Object increase}
```



## Pascal

See [[Classes#Delphi | Delphi]]


## Perl

The implementation (there are no declarations) of a class using the [http://search.cpan.org/perldoc?perlobj standard] object system:

```perl
{
     # a class is a package (i.e. a namespace) with methods in it
    package MyClass;

     # a constructor is a function that returns a blessed reference
    sub new {
        my $class = shift;
        bless {variable => 0}, $class;
         # the instance object is a hashref in disguise.
         # (it can be a ref to anything.)
    }

     # an instance method is a function that takes an object as first argument.
     # the -> invocation syntax takes care of that nicely, see Usage paragraph below.
    sub some_method {
        my $self = shift;
        $self->{variable} = 1;
    }
}
```


This is the same using the [http://search.cpan.org/perldoc?Moose Moose] object system:

```perl
{
    package MyClass;
    use Moose;

    has 'variable' => (is => 'rw', default => 0);
    # constructor and accessor methods are added automatically

    sub some_method {
        my $self = shift;
        $self->variable(1);
    }
}
```


This is the same class using the [http://search.cpan.org/perldoc?MooseX::Declare MooseX::Declare] extention:

```perl
use MooseX::Declare;
class MyClass {
    has 'variable' => (is => 'rw', default => 0);
    method some_method {
        $self->variable(1);
    }
}
```


All of the above classes can be used the same way:

```perl
my $instance = MyClass->new;    # invoke constructor method

$instance->some_method;    # invoke method on object instance
 # instance deallocates when the last reference falls out of scope
```



## Perl 6

```perl6
class Camel { has Int $.humps = 1; }

my Camel $a .= new;
say $a.humps;  # Automatically generated accessor method.

my Camel $b .= new: humps => 2;
say $b.humps;
```


A more complex example:


```perl6
class Butterfly {
    has Int $!age;   # With the ! twigil, no public accessor method is generated
    has Str $.name;
    has Str $.color;
    has Bool $.wings;

    submethod BUILD(:$!name = 'Camelia', :$!age = 2, :$!color = 'pink') {
        # BUILD is called by bless. Its primary use is to to control
        # object initialization.
        $!wings = $!age > 1;
    }

    method flap() {
        say ($.wings
          ?? 'Watch out for that hurricane!'
          !! 'No wings to flap.');
    }
}

my Butterfly $a .= new: age => 5;
say "Name: {$a.name}, Color: {$a.color}";
$a.flap;

my Butterfly $b .= new(name => 'Osgood', age => 4);
say "Name: {$b.name}, Color: {$b.color}";
$b.flap;
```



## PHL



```phl
module classes;

extern printf;

class @MyClass {
	field @Integer myField { get:get_myField, set:set_myField };

	new [
		this.set_myField(2);
	]

	@Void method [
		this.set_myField(this::get_myField + 1);
	]
};

@Integer main [
	var obj = new @MyClass;
	printf("obj.myField: %i\n", obj::get_myField);
	obj::method;
	printf("obj.myField: %i\n", obj::get_myField);
	return 0;
]
```



## PHP


```php
class MyClass {
    public static $classVar;
    public $instanceVar; // can also initialize it here
    function __construct() {
        $this->instanceVar = 0;
    }
    function someMethod() {
        $this->instanceVar = 1;
        self::$classVar = 3;
    }
}
$myObj = new MyClass();
```

<!-- TODO ;;  2008-01-24 07:35 fill this in when i get a few spare moments
/*
Usage:

$T = new Table();

$T->SetTitle("P2 Computers");
$T->SetSubtitle("Chancellor's Office, contact shereej@uoregon.edu");
$T->SetFooter("Note: sldkfjskldj f");

$header = array("Caption","Condition","Quantity","Price");
$T->SetHeadElement($header);

$data = array("Dell GX100 Computers","used",3,"");
$T->AddRow($data);

$T->Output();
*/
-->


## PicoLisp


```PicoLisp
(class +Rectangle)
# dx dy

(dm area> ()  # Define a a method that calculates the rectangle's area
   (* (: dx) (: dy)) )

(println  # Create a rectangle, and print its area
   (area> (new '(+Rectangle) 'dx 3 'dy 4)) )
```



## Pop11

Object system is implemented as a library, so we must first load it.

```pop11
uses objectclass;
define :class MyClass;
    slot value = 1;
enddefine;
```


Defining class MyClass automatically defines two constructors, newMyClass and consMyClass and slot (instance variable) accessors, so we can immediately start using our new class:


```pop11
;;; Construct instance with default slot values
lvars instance1 = newMyClass();
;;; Construct instance with explicitely given slot values
lvars instance2 = consMyClass(15);
;;; Print slot value using dot notation
instance1.value =>
instance2.value =>
;;; Print slot value using funtional notation
value(instance1) =>
;;; Change slot value
12 -> value(instance1);
;;; Print it
value(instance1) =>
```


We can add methods at any time (even after creating an instance):


```pop11
define :method reset(x : MyClass);
   0 -> value(x);
enddefine;
reset(instance1);
;;; Print it
instance1 =>
```



## PowerShell

Prior to PowerShell 5, native class definition was not supported in PowerShell. But you could define classes in PowerShell using C#, JavaScript, or VisualBasic.

```powershell

Add-Type -Language CSharp -TypeDefinition @'
public class MyClass
{
    public MyClass()
    {
    }
    public void SomeMethod()
    {
    }
    private int _variable;
    public int Variable
    {
        get { return _variable; }
        set { _variable = value; }
    }
    public static void Main()
    {
        // instantiate it
        MyClass instance = new MyClass();
        // invoke the method
        instance.SomeMethod();
        // set the variable
        instance.Variable = 99;
        // get the variable
        System.Console.WriteLine( "Variable=" + instance.Variable.ToString() );
    }
}
'@

```

<b>Basic syntax</b>

```PowerShell

class MyClass
{
[type]$MyProperty1
[type]$MyProperty2 = "Default value"

    # Constructor
    MyClass( [type]$MyParameter1, [type]$MyParameter2 )
    {
    # Code
    }

    # Method ( [returntype] defaults to [void] )
    [returntype] MyMethod( [type]$MyParameter3, [type]$MyParameter4 )
    {
    # Code
    }
}

```

<b>Example class</b>

```PowerShell

class Banana
{
# Properties
[string]$Color
[boolean]$Peeled

# Default constructor
Banana()
    {
    $This.Color = "Green"
    }

# Constructor
Banana( [boolean]$Peeled )
    {
    $This.Color = "Green"
    $This.Peeled = $Peeled
    }

# Method
Ripen()
    {
    If ( $This.Color -eq "Green" ) { $This.Color = "Yellow" }
    Else { $This.Color = "Brown" }
    }

# Method
[boolean] IsReadyToEat()
    {
    If ( $This.Color -eq "Yellow" -and $This.Peeled ) { return $True }
    Else { return $False }
    }
}

```

<b>Using the example class</b>

```PowerShell

$MyBanana   = [banana]::New()
$YourBanana = [banana]::New( $True )
$YourBanana.Ripen()
If ( -not $MyBanana.IsReadyToEat() -and $YourBanana.IsReadyToEat() )
    { $MySecondBanana = $YourBanana }

```



## Processing


```java
class ProgrammingLanguage
{
   // instance variable:
   private String name;
   // constructor (let's use it to give the instance variable a value):
   public ProgrammingLanguage(String name)
   {
      this.name = name;
      // note use of "this" to distinguish the instance variable from the argument
   }
   // a method:
   public void sayHello()
   {
      println("Hello from the programming language " + name);
      // the method has no argument or local variable called "name", so we can omit the "this"
   }
}
```

How to use it:

```java
// instantiate the class:
ProgrammingLanguage processing = new ProgrammingLanguage("Processing");

// call the method:
processing.sayHello();
```

```txt
Hello from the programming language Processing
```



## PureBasic


### Generic version


```PureBasic
Interface OO_Interface ; Interface for any value of this type
  Get.i()
  Set(Value.i)
  ToString.s()
  Destroy()
EndInterface

Structure OO_Structure ; The *VTable structure
  Get.i
  Set.i
  ToString.i
  Destroy.i
EndStructure

Structure OO_Var
  *VirtualTable.OO_Structure
  Value.i
EndStructure

Procedure OO_Get(*Self.OO_Var)
  ProcedureReturn *Self\Value
EndProcedure

Procedure OO_Set(*Self.OO_Var, n)
  *Self\Value = n
EndProcedure

Procedure.s OO_ToString(*Self.OO_Var)
  ProcedureReturn Str(*Self\Value)
EndProcedure

Procedure Create_OO()
  *p.OO_Var=AllocateMemory(SizeOf(OO_Var))
  If *p
    *p\VirtualTable=?VTable
  EndIf
  ProcedureReturn *p
EndProcedure

Procedure OO_Destroy(*Self.OO_Var)
  FreeMemory(*Self)
EndProcedure

DataSection
  VTable:
  Data.i @OO_Get()
  Data.i @OO_Set()
  Data.i @OO_ToString()
  Data.i @OO_Destroy()
EndDataSection

;- Test the code
*Foo.OO_Interface = Create_OO()
*Foo\Set(341)
MessageRequester("Info", "Foo = " + *Foo\ToString() )
*Foo\Destroy()
```



### Simple OOP Version

Using the open-source precompiler [http://www.development-lounge.de/viewtopic.php?t=5915 SimpleOOP].

```PureBasic
Class Foo
  Private Value.i

  BeginPublic
    Method Init()
      ; Any needed code goes here
    EndMethod

    Method Release()
      ; Any code befoe freeing the object goes here
    EndMethod

    Method Get()
      MethodReturn This\Value
    EndMethod

    Method Set(n)
      This\Value = n
    EndMethod

    Method.s ToString()
      MethodReturn Str(This\Value)
    EndMethod
  EndPublic
EndClass

;- Test the code
*Demo.foo = NewObject.foo()
*Demo\Set(4)
MessageRequester("Info", "Val= " + *Demo\ToString())
```



## Python


```python
class MyClass:
    name2 = 2 # Class attribute

    def __init__(self):
        """
        Constructor  (Technically an initializer rather than a true "constructor")
        """
        self.name1 = 0 # Instance attribute

    def someMethod(self):
        """
        Method
        """
        self.name1 = 1
        MyClass.name2 = 3


myclass = MyClass() # class name, invoked as a function is the constructor syntax.

class MyOtherClass:
    count = 0  # Population of "MyOtherClass" objects
    def __init__(self, name, gender="Male", age=None):
        """
        One initializer required, others are optional (with different defaults)
        """
        MyOtherClass.count += 1
        self.name = name
        self.gender = gender
        if age is not None:
            self.age = age
    def __del__(self):
        MyOtherClass.count -= 1

person1 = MyOtherClass("John")
print person1.name, person1.gender  # "John Male"
print person1.age                   # Raises AttributeError exception!
person2 = MyOtherClass("Jane", "Female", 23)
print person2.name, person2.gender, person2.age  # "Jane Female 23"
```


Python allows for very flexible argument passing including normal named parameters, defaulted/optional named parameters, up to one "varargs" tuple, and any number of keywords arguments (which are all passed in the form of a single dictionary (associative array), and any non-ambiguous combination of these). All types of argument passing for functions can also be used for object instantiation/initialization (passed to the special ''__init__()'' method) as shown.

New-style classes inherit from "object" or any descendant of the "object" class:


```python
class MyClass(object):
    ...
```


These "new-style" classes support some features which were unavailable in "classic classes".  New features include a ''__new__()'' with lower level control over object instantiation, metaclass support, static methods, class methods, "properties" (managed attributes) and "slots" (attribute restrictions).


## R

R has (at least) 5 different object oriented systems.  S3 and S4 correspond to different versions of the S language, from which R was derived.  See, for example, [http://www.r-project.org/conferences/useR-2004/Keynotes/Leisch.pdf this presentation by Freidrich Leisch] for a more thorough introduction to S3 and S4 classes.  Both these class systems are in use, and ship with the standard R distribution.  The [http://www.omegahat.org/RSOOP/ OOP], [http://cran.r-project.org/web/packages/R.oo/index.html R.oo] and [http://cran.r-project.org/web/packages/proto/index.html proto] packages provide other systems.


### S3

S3 provides a very simple class system designed to be easily used interactively.

```R
#You define a class simply by setting the class attribute of an object
circS3 <- list(radius=5.5, centre=c(3, 4.2))
class(circS3) <- "circle"

#plot is a generic function, so we can define a class specific method by naming it plot.classname
plot.circle <- function(x, ...)
{
   t <- seq(0, 2*pi, length.out=200)
   plot(x$centre[1] + x$radius*cos(t),
      x$centre[2] + x$radius*sin(t),
      type="l", ...)
}
plot(circS3)
```



### S4

S4 is a more formal class system that provides validity checking and a way to define different methods for different input signatures.

```R
setClass("circle",
   representation(
      radius="numeric",
      centre="numeric"),
   prototype(
      radius=1,
      centre=c(0,0)))
#Instantiate class with some arguments
circS4 <- new("circle", radius=5.5)
#Set other data slots (properties)
circS4@centre <- c(3,4.2)

#Define a method
setMethod("plot", #signature("circle"),
   signature(x="circle", y="missing"),
   function(x, ...)
   {
      t <- seq(0, 2*pi, length.out=200)
      #Note the use of @ instead of $
      plot(x@centre[1] + x@radius*cos(t),
         x@centre[2] + x@radius*sin(t),
         type="l", ...)
   })
plot(circS4)
```



## Racket


Racket programs heavily use functions, but classes and objects are available as well:


```racket

#lang racket

(define fish%
  (class object%
    (super-new)

    ;; an instance variable & constructor argument
    (init-field size)

    ;; a new method
    (define/public (eat)
      (displayln "gulp!"))))

;; constructing an instance
(new fish% [size 50])

```



## RapidQ


```rapidq
TYPE MyClass EXTENDS QObject
    Variable AS INTEGER

    CONSTRUCTOR
        Variable = 0
    END CONSTRUCTOR

    SUB someMethod
        MyClass.Variable = 1
    END SUB
END TYPE

' create an instance
DIM instance AS MyClass

' invoke the method
instance.someMethod
```



## Raven

Build classes:

```raven
class Alpha
    'I am Alpha.' as greeting
    define say_hello
        greeting print

class Beta extend Alpha
    'I am Beta!' as greeting
```


Execute classes to create objects:

```raven
Alpha as alpha
Beta as beta
```


Call methods:

```raven
alpha.say_hello
beta.say_hello
```


Result:

```raven
I am Alpha.
I am Beta!
```



## REALbasic

This class "contains" a number ('TheNumber'). The Number methods allow read and write access to the number, and provide an example of method overloading as well as use of the "Assigns" keyword.


```vb

Class NumberContainer
  Private TheNumber As Integer
  Sub Constructor(InitialNumber As Integer)
    TheNumber = InitialNumber
  End Sub

  Function Number() As Integer
    Return TheNumber
  End Function

  Sub Number(Assigns NewNumber As Integer)
    TheNumber = NewNumber
  End Sub
End Class

```



```vb

Dim num As New NumberContainer(1) ' call the constructor
num.Number = num.Number + 5 ' call both Number methods

```



## REBOL


```REBOL
rebol [
    Title: "Classes"
    URL: http://rosettacode.org/wiki/Classes
]

; Objects are derived from the base 'object!' type. REBOL uses a
; prototyping object system, so any object can be treated as a class,
; from which to derive others.

cowboy: make object! [
	name: "Tex"  ; Instance variable.
	hi: does [   ; Method.
		print [self/name ": Howdy!"]]
]

; I create two instances of the 'cowboy' class.

tex: make cowboy []
roy: make cowboy [
	name: "Roy"  ; Override 'name' property.
]

print "Say 'hello', boys:"  tex/hi  roy/hi
print ""

; Now I'll subclass 'cowboy'. Subclassing looks a lot like instantiation:

legend: make cowboy [
	deed: "..."
	boast: does [
		print [self/name ": I once" self/deed "!"]]
]

; Instancing the legend:

pecos: make legend [name: "Pecos Bill"  deed: "lassoed a twister"]

print "Howdy, Pecos!"  pecos/hi
print "Tell us about yourself?"  pecos/boast
```


```txt
Say 'hello', boys:
Tex : Howdy!
Roy : Howdy!

Howdy, Pecos!
Pecos Bill : Howdy!
Tell us about yourself?
Pecos Bill : I once lassoed a twister !
```


[[wp:Pecos_Bill|Context...]]


## Ring


Simple program to define class and create an object


```ring

New point { x=10  y=20  z=30  print() }
Class Point x y z func print see x + nl + y + nl + z + nl

```


The previous program can be written in another way


```ring

New point                       # create new object using the point class
{                               # access the new object attributes and methods
        x = 10                  # set the x attribute to 10
        y = 20                  # set the y attribute to 20
        z = 30                  # set the z attribute to 30
        print()                 # call the print method
}                               # end of object access


Class Point                     # define the Point class
        x y z                   # the class contains three attributes x, y & z
        func print              # define the print method
                see x + nl +    # print the x attribute
                    y + nl +    # print the y attribute
                    z + nl      # print the z attribute

```




## Ruby


```ruby
class MyClass

  def initialize
    @instance_var = 0
  end

  def add_1
    @instance_var += 1
  end

end

my_class = MyClass.new #allocates an object and calls it's initialize method, then returns it.

```



## Rust


```rust

struct MyClass {
    variable: i32, // member variable = instance variable
}

impl MyClass {
    // member function = method, with its implementation
    fn some_method(&mut self) {
        self.variable = 1;
    }

    // constructor, with its implementation
    fn new() -> MyClass {
        // Here could be more code.
        MyClass { variable: 0 }
    }
}

fn main () {
    // Create an instance in the stack.
    let mut instance = MyClass::new();

    // Create an instance in the heap.
    let mut p_instance = Box::<_>::new(MyClass::new());

    // Invoke method on both istances,
    instance.some_method();
    p_instance.some_method();

    // Both instances are automatically deleted when their scope ends.
}

```



## Sather


```sather
class CLASSTEST is
  readonly attr x:INT; -- give a public getter, not a setter
  private attr y:INT;  -- no getter, no setter
  attr z:INT;          -- getter and setter

  -- constructor
  create(x, y, z:INT):CLASSTEST is
    res :CLASSTEST := new; -- or res ::= new
    res.x := x;
    res.y := y;
    res.z := z;
    return res;
  end;

  -- a getter for the private y summed to s
  getPrivateY(s:INT):INT is
    -- y is not shadowed so we can write y instead of
    -- self.y
    return y + s;
  end;
end;
```



```sather
class MAIN is
  main is
    test ::= #CLASSTEST(1, 2, 3);
    -- the previous line is syntactic sugar for
    -- test :CLASSTEST := CLASSTEST::create(1, 2, 3);
    #OUT + test.z + "\n"; -- we can access z
    test.z := 25;         -- we can set z
    #OUT + test.x + "\n"; -- we can get x
    -- test.x := 5;          -- we cannot set x
    #OUT + test.getPrivateY(0) + "\n";
  end;
end;
```



## Scala

Scala can be highly object-oriented and if so the task is trivial. In some cases the constructor and instance variables do not have to be explicitly declared; this example shows two ways each to make constructors and instance variables.

```Scala
/** This class implicitly includes a constructor which accepts an Int and
 *  creates "val variable1: Int" with that value.
 */
class MyClass(val myMethod: Int) { // Acts like a getter, getter automatically generated.
  var variable2 = "asdf" // Another instance variable; a public var this time
  def this() = this(0) // An auxilliary constructor that instantiates with a default value
}

object HelloObject {
  val s = "Hello" // Not private, so getter auto-generated
}

/** Demonstrate use of our example class.
 */
object Call_an_object_method extends App {
  val s = "Hello"
  val m = new MyClass()
  val n = new MyClass(3)

  println(HelloObject.s) // prints "Hello" by object getterHelloObject

  println(m.myMethod) // prints 0
  println(n.myMethod) // prints 3
}
```



## Scheme

From [http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-20.html#%_sec_3.1.1 Structure and Interpretation of Computer Programs]

```Scheme
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)
```



## Sidef


```ruby
class MyClass(instance_var) {
    method add(num) {
        instance_var += num;
    }
}

var obj = MyClass(3);   # `instance_var` will be set to 3
obj.add(5);             # calls the add() method
say obj.instance_var;   # prints the value of `instance_var`: 8
```



## Simula

As the first object-oriented language, Simula introduced both the term <tt>class</tt> and the <tt>object.method(arguments)</tt> syntax that many other languages on this page employ. Notice that the object must be declared using <tt>ref</tt> (reference, i.e. pointer) before it can be instantiated.

```simula
BEGIN
    CLASS MyClass(instanceVariable);
    INTEGER instanceVariable;
    BEGIN
        PROCEDURE doMyMethod(n);
        INTEGER n;
        BEGIN
            Outint(instanceVariable, 5);
            Outtext(" + ");
            Outint(n, 5);
            Outtext(" = ");
            Outint(instanceVariable + n, 5);
            Outimage
        END;
    END;
    REF(MyClass) myObject;
    myObject :- NEW MyClass(5);
    myObject.doMyMethod(2)
END
```

```txt
    5 +     2 =     7
```



## Slate

Slate objects operate as prototypes with multi-methods:

```slate
prototypes define: #MyPrototype &parents: {Cloneable} &slots: #(instanceVar).
MyPrototype traits addSlot: #classVar.

x@(MyPrototype traits) new
[
  x clone `>> [instanceVar: 0. ]
].

x@(MyPrototype traits) someMethod
[
  x instanceVar = 1 /\ (x classVar = 3)
].
```



## Smalltalk


```smalltalk
Object subclass: #MyClass
  instanceVariableNames: 'instanceVar'
  classVariableNames: 'classVar'
  poolDictionaries: ''
  category: 'Testing' !

!MyClass class methodsFor: 'instance creation'!
new
  ^self basicNew  instanceVar := 0 ! !

!MyClass methodsFor: 'testing'!
someMethod
  ^self instanceVar = 1; classVar = 3 ! !

MyClass new someMethod!
```



## SuperCollider


```SuperCollider




SpecialObject {

	classvar a = 42, <b = 0, <>c;    // Class variables. 42 and 0 are default values.
	var <>x, <>y;           // Instance variables.
	// Note: variables are private by default. In the above, "<" creates a getter, ">" creates a setter

	*new { |value|
		^super.new.init(value)       // constructor is a class method. typically calls some instance method to set up, here "init"
	}

	init { |value|
		x = value;
		y = sqrt(squared(a) + squared(b))
	}

	// a class method
	*randomizeAll {
		a = 42.rand;
		b = 42.rand;
		c = 42.rannd;
	}

	// an instance method
	coordinates {
		^Point(x, y) // The "^" means to return the result. If not specified, then the object itself will be returned ("^this")
	}


}




```


Call it:


```SuperCollider

SpecialObject.randomizeAll;
a = SpecialObject(8);
a.coordinates;

```



## Swift


```swift
class MyClass{

  // stored property
  var variable : Int

  /**
  * The constructor
  */
  init() {
    self.variable = 42
  }

  /**
  * A method
  */
  func someMethod() {
    self.variable = 1
  }
}
```

Instantiate this class using:

```swift
MyClass()
```



## Tcl

```Tcl
package require TclOO
oo::class create summation {
    variable v
    constructor {} {
        set v 0
    }
    method add x {
        incr v $x
    }
    method value {} {
        return $v
    }
    destructor {
        puts "Ended with value $v"
    }
}
set sum [summation new]
puts "Start with [$sum value]"
for {set i 1} {$i <= 10} {incr i} {
    puts "Add $i to get [$sum add $i]"
}
$sum destroy
```



## TIScript

TIScript is [[wp:Prototype-based programming|prototype-based]] and yet it has classes. Object that was created as an instance of one class can be transformed to the instance of another class by changing its obj.prototype field.


```javascript
class Car
{
  //Constructor function.
  function this(brand, weight, price = 0) {
    this.brand = brand;
    this.weight = weight || 1000; // Resort to default value (with 'or' notation).
    this._price = price;
  }
  property price(v) // computable property, special kind of member function
  {
    get { return this._price; } // getter section
    set { this._price = v; }    // setter section
  }
  function toString() { // member function, method of a Car.
    return String.printf("<%s>",this.brand);
  }
}

class Truck : Car
{
  function this(brand, size) {
    super(brand, 2000); // Call of constructor of super class (Car here)
    this.size = size; // Custom property for just this object.
  }
}

var cars = [ // Some example car objects.
  new Car("Mazda"),
  new Truck("Volvo", 2, 30000)
];
for (var (i,car) in cars) // TIScript allows enumerate indexes and values
{
  stdout.printf("#%d %s $%d %v %v, %v %v", i, car.brand, car.price, car.weight, car.size,
                 car instanceof Car, car instanceof Truck);
}
```

{{out}} from console :

```txt

#1 Mazda 1000 $0 undefined, true false
#2 Volvo 2000 $30000 2, true true

```



## TXR


```txrlisp
(defstruct shape ()
  cached-area

  (:init (self)
    (put-line `@self is born!`))

  (:fini (self)
    (put-line `@self says goodbye!`))

  (:method area (self)
    (or self.cached-area
        (set self.cached-area self.(calc-area)))))

(defstruct circle shape
  (radius 1.0)

  (:method calc-area (self)
    (* %pi% self.radius self.radius)))

(defstruct square shape
  (length 1.0)

  (:method calc-area (self)
    (* self.length self.length)))
```


```txt
$ txr -i shapes.tl
1> (let ((s (new circle)))
     s.(area))
#S(circle cached-area nil radius nil) is born!
3.14159265358979
2> (sys:gc)
#S(circle cached-area 3.14159265358979 radius 1.0) says goodbye!
t
3>
```


'''Notes:'''

* <code>defstruct</code> and <code>new</code> are macros which compile to invocations of the functions <code>make-struct-type</code> and <code>make-struct</code>.
* The <code>obj.fun(x, y)</code> syntax is "halfway Lispified", and looks like <code>obj.(fun x y)</code>. This denotes a method call: the function <code>fun</code> is retrieved from the object, and passed the arguments <code>(obj x y)</code>.
* The notation <code>obj.[fun x y]</code> is similar, but will '''not''' pass <code>obj</code> to fun; it is for calling static functions (class utility functions that don't require an instance).
* <code>a.b.c.d</code> in TXR Lisp is a syntactic sugar for the expression <code>(qref a b c d)</code>, where the elements may be compound expressions. Thus <code>obj.(a b).c</code> is <code>(qref obj (a b) c)</code>.
* There must be no whitespace around the dot: <code>(a . b)</code> is the consing dot whereas <code>(a.b)</code> is the syntax <code>((qref a b))</code>.
* Ambiguity with floating-point numbers isn't allowed. For instance, <code>a.b.1</code> elicits an error from the parser (lexical scanner actually).


## UNIX Shell

ksh93 has "type variables" which essentially declares a class.

```bash
typeset -T Summation_t=(
    integer sum

    # the constructor
    function create {
        _.sum=0
    }

    # a method
    function add {
        (( _.sum += $1 ))
    }
)

Summation_t s
for i in 1 2 3 4 5; do
    s.add $i
done
print ${s.sum}
```



## Vala


```vala
public class MyClass : Object {
    // Instance variable
    public int variable;

    // Method
    public void some_method() {
        variable = 24;
    }

    // Constructor
    public MyClass() {
        variable = 42;
    }
}
void main() {
    // Class instance
    MyClass instance = new MyClass();
    print("%d\n", instance.variable);
    instance.some_method();
    print("%d\n", instance.variable);
    instance.variable = 84;
    print("%d\n", instance.variable);
}
```



## VBA


### Defining a class

In Visual Basic for Applications a class is defined in a separate Class Module. The name of the class module is the name of the class.

For each property you must supply a "Property Let" routine to set the property (or "Property Set" if the property refers to an object), and a "Property Get" function to get the property.
Methods are represented by Functions in the class module.
A class module ''can'' have a constructor - a sub with the special name <code>Class_Initialize</code> - and a destructor with the special name <code>Class_Terminate</code>.

This is the contents of a class module "Foo" (like in the Visual Basic .NET example below):


```vb
Private Const m_default = 10
Private m_bar As Integer

Private Sub Class_Initialize()
  'constructor, can be used to set default values
  m_bar = m_default
End Sub

Private Sub Class_Terminate()
  'destructor, can be used to do some cleaning up
  'here we just print a message
  Debug.Print "---object destroyed---"
End Sub
Property Let Bar(value As Integer)
  m_bar = value
End Property

Property Get Bar() As Integer
  Bar = m_bar
End Property

Function DoubleBar()
  m_bar = m_bar * 2
End Function

Function MultiplyBar(x As Integer)
  'another method
  MultiplyBar = m_bar * x
  'Note: instead of using the instance variable m_bar we could refer to the Bar property of this object using the special word "Me":
  '  MultiplyBar = Me.Bar * x
End Function
```



### Using an object

Objects (e.g. of class Foo) are created and used in "normal" modules.

```vb
Public Sub foodemo()
'declare and create separately
Dim f As Foo
Dim f0 As Foo

Set f = New Foo

'set property
f.Bar = 25
'call method
f.DoubleBar
'alternative
Call f.DoubleBar
Debug.Print "f.Bar is "; f.Bar
Debug.Print "Five times f.Bar is "; f.MultiplyBar(5)

'declare and create at the same time
Dim f2 As New Foo
Debug.Print "f2.Bar is "; f2.Bar 'prints default value

'destroy an object
Set f = Nothing

'create an object or not, depending on a random number:
If Rnd() < 0.5 Then
  Set f0 = New Foo
End If
'check if object actually exists
If f0 Is Nothing Then
  Debug.Print "object f0 does not exist"
Else
  Debug.Print "object f0 was created"
End If
'at the end of execution all remaining objects created in this sub will be released.
'this will trigger one or two "object destroyed" messages
'depending on whether f0 was created...
End Sub
```


```txt
foodemo
f.Bar is  100
Five times f.Bar is  500
f2.Bar is  10
---object destroyed---
object f0 was created
---object destroyed---
---object destroyed---

```



## Visual Basic .NET


### Defining a class


```vbnet
Class Foo
   Private m_Bar As Integer

   Public Sub New()

   End Sub

   Public Sub New(ByVal bar As Integer)
       m_Bar = bar
   End Sub

   Public Property Bar() As Integer
       Get
           Return m_Bar
       End Get
       Set(ByVal value As Integer)
           m_Bar = value
       End Set
   End Property

   Public Sub DoubleBar()
       m_Bar *= 2
   End Sub

   Public Function MultiplyBar(ByVal x As Integer) As Integer
       Return x * Bar
   End Function

End Class
```



### Using an object


```vbnet
'Declare and create separately
Dim foo1 As Foo
foo1 = New Foo

'Declare and create at the same time
Dim foo2 As New Foo

'... while passing constructor parameters
Dim foo3 As New Foo(5)

'... and them immediately set properties
Dim foo4 As New Foo With {.Bar = 10}

'Calling a method that returns a value
Console.WriteLine(foo4.MultiplyBar(20))

'Calling a method that performs an action
foo4.DoubleBar()

'Reading/writing properties
Console.WriteLine(foo4.Bar)
foo4.Bar = 1000
```



## Visual FoxPro

Visual FoxPro has a large number of base classes - Session is one of them.

```vfp

LOCAL o1 As MyClass, o2 As MyClass
*!* Instantiate o1
o1 = NEWOBJECT("MyClass")
o1.ShowInstance()
*!* Instantiate o2
o2 = CREATEOBJECT("MyClass", 2)
o2.ShowInstance()


DEFINE CLASS MyClass As Session
*!* Custom property (protected)
PROTECTED nInstance
nInstance = 0

*!* Constructor
PROCEDURE Init(tnInstance As Integer)
IF VARTYPE(tnInstance) = "N"
    THIS.nInstance = tnInstance
ELSE
    THIS.nInstance = THIS.nInstance + 1
ENDIF
ENDPROC

*!* Custom Method
PROCEDURE ShowInstance
? "Instance", THIS.nInstance
ENDPROC
ENDDEFINE

```

```txt

Instance 1
Instance 2

```



## XLISP


```xlisp
(DEFINE-CLASS PROGRAMMING-LANGUAGE
    (INSTANCE-VARIABLES NAME YEAR))

(DEFINE-METHOD (PROGRAMMING-LANGUAGE 'INITIALIZE X)
    (SETQ NAME X)
    SELF)

(DEFINE-METHOD (PROGRAMMING-LANGUAGE 'WAS-CREATED-IN X)
    (SETQ YEAR X))

(DEFINE-METHOD (PROGRAMMING-LANGUAGE 'DESCRIBE)
    `(THE PROGRAMMING LANGUAGE ,NAME WAS CREATED IN ,YEAR))

(DEFINE LISP (PROGRAMMING-LANGUAGE 'NEW 'LISP))

(LISP 'WAS-CREATED-IN 1958)

(DISPLAY (LISP 'DESCRIBE))
(NEWLINE)
```

```txt
(THE PROGRAMMING LANGUAGE LISP WAS CREATED IN 1958)
```



## zonnon


```zonnon

module Graphics;
type
	{ref,public} (* class *)
	Point = object(ord,abs: integer)
	var
		(* instance variables *)
		{public,immutable} x,y: integer;

	(* method *)
	procedure {public} Ord():integer;
	begin
		return y
	end Ord;

	(* method *)
	procedure {public} Abs():integer;
	begin
		return x
	end Abs;

	(* constructor *)
	begin
		self.x := ord;
		self.y := abs;
	end Point;
end Graphics.

module Main;
import Graphics;
var
	p: Graphics.Point;

procedure Write(p: Graphics.Point);
begin
	writeln('[':1,p.x:3,',':1,p.y:3,']':1)
end Write;

begin
	p := new Graphics.Point(12,12);
	Write(p);
	writeln("Abs: ":4,p.Abs():3," Ord: ":5,p.Ord():3);
end Main.

```

```txt

[ 12, 12]
Abs:  12 Ord:  12

```


## zkl


```zkl
class C{ // define class named "C", no parents or attributes
   println("starting construction"); // all code outside functions is wrapped into the constructor
   var v;    // instance data for this class
   fcn init(x) // initializer for this class, calls constructor
      { v = x }
   println("ending construction of ",self);
}
c1:=C(5);  // create a new instance of C
c2:=c1("hoho"); // create another instance of C
println(C.v," ",c1.v," ",c2.v);
```

```txt

starting construction
ending construction of Class(C)
starting construction
ending construction of Class(C)
Void 5 hoho

```


```zkl
C.__constructor(); // run base class constructor for giggles
C.init(456);  // initialize base class without creating instance
println(C.v," ",c1.v);
```

```txt

starting construction
ending construction of Class(C)
starting construction
ending construction of Class(C)
456 5

```


