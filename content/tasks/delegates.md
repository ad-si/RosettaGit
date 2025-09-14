+++
title = "Delegates"
description = ""
date = 2019-10-17T19:11:57Z
aliases = []
[extra]
id = 2148
[taxonomies]
categories = ["task", "Object oriented"]
tags = []
languages = [
  "ada",
  "aime",
  "aikido",
  "algol_68",
  "c",
  "csharp",
  "cpp",
  "clojure",
  "coffeescript",
  "common_lisp",
  "d",
  "dart",
  "delphi",
  "e",
  "elena",
  "forth",
  "go",
  "io",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "latitude",
  "logtalk",
  "m2000_interpreter",
  "ngs",
  "objeck",
  "oforth",
  "oorexx",
  "oxygenbasic",
  "oz",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pop11",
  "python",
  "racket",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "smalltalk",
  "swift",
  "tcl",
  "vorpal",
  "zkl",
]
+++

## Task

A delegate is a helper object used by another object. The delegator may send the delegate certain messages, and provide a default implementation when there is no delegate or the delegate does not respond to a message. This pattern is heavily used in [http://developer.apple.com/documentation/Cocoa/Conceptual/CocoaFundamentals/CocoaDesignPatterns/chapter_5_section_3.html#//apple_ref/doc/uid/TP40002974-CH6-DontLinkElementID_93 Cocoa framework on Mac OS X]. See also [[wp:Delegation pattern]].

Objects responsibilities:

Delegator:
* Keep an optional delegate instance.
* Implement "operation" method, returning the delegate "thing" if the delegate respond to "thing", or the string "default implementation".

Delegate:
* Implement "thing" and return the string "delegate implementation"

Show how objects are created and used. First, without a delegate, then with a delegate that does not implement "thing", and last with a delegate that implements "thing".


## Ada

All that is needed in order to implement this is a common base type. The delegator holds a pointer to an "untyped" object from the base class. Querying if the target implements the delegate interface is done using run-time type identification.

```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Delegation is
   package Things is
      -- We need a common root for our stuff
      type Object is tagged null record;
      type Object_Ptr is access all Object'Class;

      -- Objects that have operation thing
      type Substantial is new Object with null record;
      function Thing (X : Substantial) return String;

      -- Delegator objects
      type Delegator is new Object with record
         Delegate : Object_Ptr;
      end record;
      function Operation (X : Delegator) return String;

      No_Thing  : aliased Object;      -- Does not have thing
      Has_Thing : aliased Substantial; -- Has one
   end Things;

   package body Things is
      function Thing (X : Substantial) return String is
      begin
         return "delegate implementation";
      end Thing;

      function Operation (X : Delegator) return String is
      begin
         if X.Delegate /= null and then X.Delegate.all in Substantial'Class then
            return Thing (Substantial'Class (X.Delegate.all));
         else
            return "default implementation";
         end if;
      end Operation;
   end Things;

   use Things;

   A : Delegator; -- Without a delegate
begin
   Put_Line (A.Operation);
   A.Delegate := No_Thing'Access; -- Set no thing
   Put_Line (A.Operation);
   A.Delegate := Has_Thing'Access; -- Set a thing
   Put_Line (A.Operation);
end Delegation;
```

Sample output:

```txt

default implementation
default implementation
delegate implementation

```



## Aime


```aime
text
thing(void)
{
    return "delegate implementation";
}

text
operation(record delegator)
{
    text s;

    if (r_key(delegator, "delegate")) {
        if (r_key(delegator["delegate"], "thing")) {
            s = call(r_query(delegator["delegate"], "thing"));
        } else {
            s = "default implementation";
        }
    } else {
        s = "default implementation";
    }

    return s;
}

integer
main(void)
{
    record delegate, delegator;

    o_text(operation(delegator));
    o_byte('\n');

    r_link(delegator, "delegate", delegate);
    o_text(operation(delegator));
    o_byte('\n');

    r_put(delegate, "thing", thing);
    o_text(operation(delegator));
    o_byte('\n');

    return 0;
}
```



## Aikido


```aikido

class Delegator {
    public generic delegate = none

    public function operation {
        if (typeof(delegate) == "none") {
            return "default implementation"
        }
        return delegate()
    }
}

function thing {
    return "delegate implementation"
}

// default, no delegate
var d = new Delegator()
println (d.operation())

// delegate
var d1 = new Delegator()
d1.delegate = thing
println (d1.operation())


```



## ALGOL 68

As Algol 68 doesn't have classes, we supply a non-OO approximation, similar to the C version.

```algol68
# An Algol 68 approximation of delegates                                #

# The delegate mode - the delegate is a STRUCT with a single field      #
# that is a REF PROC STRING. If this is NIL, it doesn't implement       #
# thing                                                                 #
MODE DELEGATE  = STRUCT( REF PROC STRING thing );


# A delegator mode that will invoke the delegate's thing method         #
# - if there is a delegate and the delegate has a thing method          #
MODE DELEGATOR = STRUCT( REF DELEGATE delegate
                       , PROC( REF DELEGATE )STRING thing
                       );

# constructs a new DELEGATE with the specified PROC as its thing        #
# Algol 68 HEAP is like "new" in e.g. Java, but it can't take           #
# parameters, so this PROC does the equivalent                          #
PROC new delegate = ( REF PROC STRING thing )REF DELEGATE:
    BEGIN
        REF DELEGATE result = HEAP DELEGATE;
        thing OF result := thing;

        result
    END # new delegate #
;

# constructs a new DELEGATOR with the specified DELEGATE                #
PROC new delegator = ( REF DELEGATE delegate )REF DELEGATOR:
    HEAP DELEGATOR := ( delegate
                      , # anonymous PROC to invoke the delegate's thing #
                        ( REF DELEGATE delegate )STRING:
                            IF delegate IS REF DELEGATE(NIL)
                            THEN
                                # we have no delegate #
                                "default implementation"

                            ELIF thing OF delegate IS REF PROC STRING(NIL)
                            THEN
                                # the delegate doesn't have an implementation #
                                "default implementation"

                            ELSE
                                # the delegate can thing #
                                thing OF delegate

                            FI
                      )
;


# invokes the delegate's thing via the delagator                        #
# Because the PROCs of a STRUCT don't have an equivalent of e.g. Java's #
# "this", we have to explicitly pass the delegate as a parameter        #
PROC invoke thing = ( REF DELEGATOR delegator )STRING:
    # the following is Algol 68 for what would be written in Java as    #
    #                           "delegator.thing( delegator.delegate )" #
    ( thing OF delegator )( delegate OF delegator )
;

main:
(

    print( ( "No delegate           : "
           , invoke thing( new delegator( NIL ) )
           , newline
           , "Delegate with no thing: "
           , invoke thing( new delegator( new delegate( NIL ) ) )
           , newline
           , "Delegate with a thing : "
           , invoke thing( new delegator( new delegate( HEAP PROC STRING := STRING: ( "delegate implementation" ) ) ) )
           , newline
           )
         )

)

```

```txt

No delegate           : default implementation
Delegate with no thing: default implementation
Delegate with a thing : delegate implementation

```



## C

As best you can do, without support for classes.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef const char * (*Responder)( int p1);

typedef struct sDelegate {
    Responder operation;
} *Delegate;

/* Delegate class constructor */
Delegate NewDelegate( Responder rspndr )
{
    Delegate dl = malloc(sizeof(struct sDelegate));
    dl->operation = rspndr;
    return dl;
}

/* Thing method of Delegate */
const char *DelegateThing(Delegate dl, int p1)
{
    return  (dl->operation)? (*dl->operation)(p1) : NULL;
}

/** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
typedef struct sDelegator {
    int     param;
    char    *phrase;
    Delegate delegate;
} *Delegator;

const char * defaultResponse( int p1)
{
    return "default implementation";
}

static struct sDelegate defaultDel = { &defaultResponse };

/* Delegator class constructor */
Delegator NewDelegator( int p, char *phrase)
{
    Delegator d  = malloc(sizeof(struct sDelegator));
    d->param = p;
    d->phrase = phrase;
    d->delegate = &defaultDel;	/* default delegate */
    return d;
}

/* Operation method of Delegator */
const char *Delegator_Operation( Delegator theDelegator, int p1, Delegate delroy)
{
    const char *rtn;
    if (delroy) {
        rtn = DelegateThing(delroy, p1);
        if (!rtn) {			/* delegate didn't handle 'thing' */
            rtn = DelegateThing(theDelegator->delegate, p1);
        }
    }
    else 		/* no delegate */
        rtn = DelegateThing(theDelegator->delegate, p1);

    printf("%s\n", theDelegator->phrase );
    return rtn;
}

/** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
const char *thing1( int p1)
{
    printf("We're in thing1 with value %d\n" , p1);
    return "delegate implementation";
}

int main()
{
    Delegate del1 = NewDelegate(&thing1);
    Delegate del2 = NewDelegate(NULL);
    Delegator theDelegator = NewDelegator( 14, "A stellar vista, Baby.");

    printf("Delegator returns %s\n\n",
            Delegator_Operation( theDelegator, 3, NULL));
    printf("Delegator returns %s\n\n",
            Delegator_Operation( theDelegator, 3, del1));
    printf("Delegator returns %s\n\n",
            Delegator_Operation( theDelegator, 3, del2));
    return 0;
}
```



## C#


```c#
using System;

interface IOperable
{
    string Operate();
}

class Inoperable
{
}

class Operable : IOperable
{
    public string Operate()
    {
        return "Delegate implementation.";
    }
}

class Delegator : IOperable
{
    object Delegate;

    public string Operate()
    {
        var operable = Delegate as IOperable;
        return operable != null ? operable.Operate() : "Default implementation.";
    }

    static void Main()
    {
        var delegator = new Delegator();
        foreach (var @delegate in new object[] { null, new Inoperable(), new Operable() })
        {
            delegator.Delegate = @delegate;
            Console.WriteLine(delegator.Operate());
        }
    }
}
```

Output:

```txt
Default implementation.
Default implementation.
Delegate implementation.
```



## C++

Delegates in the C# or D style are available in C++ through std::tr1::function class template. These delegates don't exactly match this problem statement though, as they only support a single method call (which is operator()), and so don't support querying for support of particular methods.


```Cpp

#include <tr1/memory>
#include <string>
#include <iostream>
#include <tr1/functional>

using namespace std;
using namespace std::tr1;
using std::tr1::function;

// interface for all delegates
class IDelegate
{
public:
    virtual ~IDelegate() {}
};

//interface for delegates supporting thing
class IThing
{
public:
    virtual ~IThing() {}
    virtual std::string Thing() = 0;
};

// Does not handle Thing
class DelegateA : virtual public IDelegate
{
};

// Handles Thing
class DelegateB : public IThing, public IDelegate
{
    std::string Thing()
    {
        return "delegate implementation";
    }
};

class Delegator
{
public:
    std::string Operation()
    {
        if(Delegate) //have delegate
           if (IThing * pThing = dynamic_cast<IThing*>(Delegate.get()))
            //delegate provides IThing interface
            return pThing->Thing();

        return "default implementation";
    }

    shared_ptr<IDelegate> Delegate;
};

int main()
{
    shared_ptr<DelegateA> delegateA(new DelegateA());
    shared_ptr<DelegateB> delegateB(new DelegateB());
    Delegator delegator;

    // No delegate
    std::cout << delegator.Operation() << std::endl;

    // Delegate doesn't handle "Thing"
    delegator.Delegate = delegateA;
    std::cout << delegator.Operation() << std::endl;

    // Delegate handles "Thing"
    delegator.Delegate = delegateB;
    std::cout << delegator.Operation() << std::endl;

/*
Prints:

  default implementation
  default implementation
  delegate implementation
 */
}

```


## Clojure


```clojure
(defprotocol Thing
  (thing [_]))

(defprotocol Operation
  (operation [_]))

(defrecord Delegator [delegate]
  Operation
  (operation [_] (try (thing delegate) (catch IllegalArgumentException e "default implementation"))))

(defrecord Delegate []
  Thing
  (thing [_] "delegate implementation"))
```


```txt

; without a delegate
=> (operation (Delegator. nil))
"default implementation"

; with a delegate that does not implement "thing"
=> (operation (Delegator. (Object.)))
"default implementation"

; with a delegate that implements "thing"
=> (operation (Delegator. (Delegate.)))
"delegate implementation"

```



## CoffeeScript

```coffeescript

class Delegator
  operation: ->
    if @delegate and typeof (@delegate.thing) is "function"
      return @delegate.thing()
    "default implementation"

class Delegate
  thing: ->
    "Delegate Implementation"

testDelegator = ->
  # Delegator with no delegate.
  a = new Delegator()
  console.log a.operation()

  # Delegator with delegate not implementing "thing"
  a.delegate = "A delegate may be any object"
  console.log a.operation()

  # Delegator with delegate that does implement "thing"
  a.delegate = new Delegate()
  console.log a.operation()

testDelegator()

```

output
<lang>
 > coffee foo.coffee
default implementation
default implementation
Delegate Implementation

```



## Common Lisp

In CLOS, methods exist apart from classes, and are specialized based on the types of their arguments.  This example defines two classes (delegator and delegate), and a thing generic method which is specialized in three ways: (1) for 'any' argument, providing a default method; (2) for delegators, where thing is recursively applied to the delegator's delegate (if there is one); and (3) for delegates.


```lisp
(defgeneric thing (object)
  (:documentation "Thing the object."))

(defmethod thing (object)
  "default implementation")

(defclass delegator ()
  ((delegate
    :initarg :delegate
    :reader delegator-delegate)))

(defmethod thing ((delegator delegator))
  "If delegator has a delegate, invoke thing on the delegate,
otherwise return \"no delegate\"."
  (if (slot-boundp delegator 'delegate)
    (thing (delegator-delegate delegator))
    "no delegate"))

(defclass delegate () ())

(defmethod thing ((delegate delegate))
  "delegate implementation")

(let ((d1 (make-instance 'delegator))
      (d2 (make-instance 'delegator :delegate nil))
      (d3 (make-instance 'delegator :delegate (make-instance 'delegate))))
  (assert (string= "no delegate" (thing d1)))
  (assert (string= "default implementation" (thing d2)))
  (assert (string= "delegate implementation" (thing d3))))
```



## D

D has built-in delegates, so we can skip creating an additional
''Delegate'' object and pass a real delegate directly to '''Delegator'''.


```d
class Delegator {
    string delegate() hasDelegate;

    string operation() {
        if (hasDelegate is null)
            return "Default implementation";
        return hasDelegate();
    }

    typeof(this) setDg(string delegate() dg) {
        hasDelegate = dg;
        return this;
    }
}

void main() {
    import std.stdio;
    auto dr = new Delegator;
    string delegate() thing = () => "Delegate implementation";

    writeln(dr.operation());
    writeln(dr.operation());
    writeln(dr.setDg(thing).operation());
}
```

```txt
Default implementation
Default implementation
Delegate implementation
```



### Version using Tango

```d
import tango.io.Stdout;

class Delegator
{
    private char[] delegate() hasDelegate;
public:
    char[] operation() {
        if (hasDelegate is null)
            return "default implementation";
        return hasDelegate();
    }

    typeof(this) setDg(char[] delegate() dg)
    {
        hasDelegate = dg;
        return this;
    }
}

int main(char[][] args)
{
    auto dr = new Delegator();
    auto thing = delegate char[]() { return "delegate implementation"; };

    Stdout ( dr.operation ).newline;
    Stdout ( dr.operation ).newline;
    Stdout ( dr.setDg(thing).operation ).newline;
    return 0;
}
```



## Dart

I didn't find a way to check for existing methods, so the version with Object doesn't work yet. The code is adapted from the Java version, but using var instead of an Interface

```dart
class Delegator {
  var delegate;

  String operation() {
    if (delegate == null)
      return "default implementation";
    else
      return delegate.thing();
  }
}

class Delegate {
  String thing() => "delegate implementation";
}

main() {
  // Without a delegate:
  Delegator a = new Delegator();
  Expect.equals("default implementation",a.operation());

  // any object doesn't work unless we can check for existing methods
  // a.delegate=new Object();
  // Expect.equals("default implementation",a.operation());

  // With a delegate:
  Delegate d = new Delegate();
  a.delegate = d;
  Expect.equals("delegate implementation",a.operation());
}
```



## Delphi

Translation of the Java example found at [http://en.wikipedia.org/wiki/Delegation_pattern Wikipedia].

```Delphi
unit Printer;

interface

type
  // the "delegate"
  TRealPrinter = class
  public
    procedure Print;
  end;

  // the "delegator"
  TPrinter = class
  private
    FPrinter: TRealPrinter;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Print;
  end;

implementation

{ TRealPrinter }

procedure TRealPrinter.Print;
begin
   Writeln('Something...');
end;

{ TPrinter }

constructor TPrinter.Create;
begin
  inherited Create;
  FPrinter:= TRealPrinter.Create;
end;

destructor TPrinter.Destroy;
begin
  FPrinter.Free;
  inherited;
end;

procedure TPrinter.Print;
begin
  FPrinter.Print;
end;

end.
```


```Delphi
program Delegate;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Printer in 'Printer.pas';

var
  PrinterObj: TPrinter;
begin
  PrinterObj:= TPrinter.Create;
  try
    PrinterObj.Print;
    Readln;
  finally
    PrinterObj.Free;
  end;
end.
```



## E



```e
def makeDelegator {
    /** construct without an explicit delegate */
    to run() {
        return makeDelegator(null)
    }

    /** construct with a delegate */
    to run(delegateO) { # suffix because "delegate" is a reserved keyword
        def delegator {
            to operation() {
                return if (delegateO.__respondsTo("thing", 0)) {
                           delegateO.thing()
                       } else {
                           "default implementation"
                       }
            }
        }
        return delegator
    }
}

? def delegator := makeDelegator()
> delegator.operation()
# value: "default implementation"

? def delegator := makeDelegator(def doesNotImplement {})
> delegator.operation()
# value: "default implementation"

? def delegator := makeDelegator(def doesImplement {
>     to thing() { return "delegate implementation" }
> })
> delegator.operation()
# value: "default implementation"
```




## Elena

ELENA 4.x :

Using multi methods:

```elena
import extensions;
import system'routines;

interface IOperable
{
    abstract operate() {}
}

class Operable : IOperable
{
    constructor() {}

    operate()
        = "delegate implementation";
}

class Delegator
{
    object theDelegate;

    set Delegate(object)
    {
        theDelegate := object
    }

    internal operate(operable)
        = "default implementation";

    internal operate(IOperable operable)
        = operable.operate();

    operate()
        <= operate(theDelegate);
}

public program()
{
    var delegator := new Delegator();

    new::(nil, new Object(), new Operable()).forEach:(o)
    {
       delegator.Delegate := o;

       console.printLine(delegator.operate())
    }
}
```

Generic solution:

```elena
import extensions;
import system'routines;

class Operable
{
    Operable = self;

    operate()
        = "delegate implementation";
}

class Delegator
{
    prop object Delegate;

    constructor()
    {
        Delegate := nil
    }

    operate()
    {
        // if the object does not support "get&operable" message - returns nil
        var operable := Delegate.Operable \ back:nil;

        if (nil == operable)
        {
            ^ "default implementation"
        }
        else
        {
            ^ operable.operate()
        }
    }
}

public program()
{
    var delegator := new Delegator();

    new::(nil, new Object(), new Operable()).forEach:(o)
    {
       delegator.Delegate := o;

       console.printLine(delegator.operate())
    }
}
```

```txt

default implementation
default implementation
delegate implementation

```


=={{header|F_Sharp|F#}}==

```fsharp
type Delegator() =
  let defaultOperation() = "default implementation"
  let mutable del = null

  // write-only property "Delegate"
  member x.Delegate with set(d:obj) = del <- d

  member x.operation() =
    if del = null then
      defaultOperation()
    else
      match del.GetType().GetMethod("thing", [||]) with
      | null -> defaultOperation()
      | thing -> thing.Invoke(del, [||]) :?> string

type Delegate() =
  member x.thing() = "delegate implementation"

let d = new Delegator()
assert (d.operation() = "default implementation")

d.Delegate <- "A delegate may be any object"
assert (d.operation() = "default implementation")

d.Delegate <- new Delegate()
assert (d.operation() = "delegate implementation")
```



## Forth

Works with any ANS Forth

Needs the FMS-SI (single inheritance) library code located here:
http://soton.mpeforth.com/flag/fms/index.html

```forth
include FMS-SI.f

:class delegate
  :m thing ." delegate implementation" ;m
;class

delegate slave

:class delegator
  ivar del  \ object container
  :m !: ( n -- ) del ! ;m
  :m init: 0 del ! ;m
  :m default ." default implementation" ;m
  :m operation
     del @ 0= if self default exit then
     del @ has-meth thing
     if del @ thing
     else self default
     then ;m
;class

delegator master

\ First, without a delegate
master operation \ => default implementation

\ then with a delegate that does not implement "thing"
object o
o master !:
master operation \ => default implementation

\ and last with a delegate that implements "thing"
slave master !:
master operation \ => delegate implementation

```



## Go



```go
package main
import "fmt"

type Delegator struct {
    delegate interface{} // the delegate may be any type
}

// interface that represents anything that supports thing()
type Thingable interface {
    thing() string
}

func (self Delegator) operation() string {
    if v, ok := self.delegate.(Thingable); ok {
        return v.thing()
    }
    return "default implementation"
}

type Delegate int // any dummy type

func (Delegate) thing() string {
    return "delegate implementation"
}

func main() {
    // Without a delegate:
    a := Delegator{}
    fmt.Println(a.operation()) // prints "default implementation"

    // With a delegate that does not implement "thing"
    a.delegate = "A delegate may be any object"
    fmt.Println(a.operation()) // prints "default implementation"

    // With a delegate:
    var d Delegate
    a.delegate = d
    fmt.Println(a.operation()) // prints "delegate implementation"
}
```



## Io

```Io
Delegator := Object clone do(
    delegate ::= nil
    operation := method(
        if((delegate != nil) and (delegate hasSlot("thing")),
            delegate thing,
            "default implementation"
        )
    )
)

Delegate := Object clone do(
    thing := method("delegate implementation")
)

a := clone Delegator
a operation println

a setDelegate("A delegate may be any object")
a operation println

a setDelegate(Delegate clone)
a operation println
```

```txt
default implementation
default implementation
delegate implementation

```



## J

Life becomes slightly cleaner if we delegate to ourselves in the absence of some other delegate.


```J
coclass 'delegator'
  operation=:3 :'thing__delegate ::thing y'
  thing=: 'default implementation'"_
  setDelegate=:3 :'delegate=:y'  NB. result is the reference to our new delegate
  delegate=:<'delegator'

coclass 'delegatee1'

coclass 'delegatee2'
  thing=: 'delegate implementation'"_

NB. set context in case this script was used interactively, instead of being loaded
cocurrent 'base'
```


Example use:


```J
   obj=:conew'delegator'
   operation__obj''
default implementation
   setDelegate__obj conew'delegatee1'
┌─┐
│4│
└─┘
   operation__obj''
default implementation
   setDelegate__obj conew'delegatee2'
┌─┐
│5│
└─┘
   operation__obj''
delegate implementation
```



## Java

This implementation uses an interface called Thingable to specify the type of delegates that respond to thing(). The downside is that any delegate you want to use has to explicitly declare to implement the interface. The upside is that the type system guarantees that when the delegate is non-null, it must implement the "thing" method.


```java
interface Thingable {
    String thing();
}

class Delegator {
    public Thingable delegate;

    public String operation() {
        if (delegate == null)
            return "default implementation";
        else
            return delegate.thing();
    }
}

class Delegate implements Thingable {
    public String thing() {
        return "delegate implementation";
    }
}

// Example usage
// Memory management ignored for simplification
public class DelegateExample {
    public static void main(String[] args) {
        // Without a delegate:
        Delegator a = new Delegator();
        assert a.operation().equals("default implementation");

        // With a delegate:
        Delegate d = new Delegate();
        a.delegate = d;
        assert a.operation().equals("delegate implementation");

        // Same as the above, but with an anonymous class:
        a.delegate = new Thingable() {
                public String thing() {
                    return "anonymous delegate implementation";
                }
            };
        assert a.operation().equals("anonymous delegate implementation");
    }
}
```


```java
package delegate;

@FunctionalInterface
public interface Thingable {
  public String thing();
}
```



```java
package delegate;

import java.util.Optional;

public interface Delegator {
  public Thingable delegate();
  public Delegator delegate(Thingable thingable);

  public static Delegator new_() {
    return $Delegator.new_();
  }

  public default String operation() {
    return Optional.ofNullable(delegate())
      .map(Thingable::thing)
      .orElse("default implementation")
    ;
  }
}
```



```java
package delegate;

@FunctionalInterface
/* package */ interface $Delegator extends Delegator {
  @Override
  public default Delegator delegate(Thingable thingable) {
    return new_(thingable);
  }

  public static $Delegator new_() {
    return new_(() -> null);
  }

  public static $Delegator new_(Thingable thingable) {
    return () -> thingable;
  }
}
```



```java
package delegate;

public final class Delegate implements Thingable {
  @Override
  public String thing() {
    return "delegate implementation";
  }
}
```



```java
package delegate;

// Example usage
// Memory management ignored for simplification
public interface DelegateTest {
  public static String thingable() {
    return "method reference implementation";
  }

  public static void main(String... arguments) {
    // Without a delegate:
    Delegator d1 = Delegator.new_();
    assert d1.operation().equals("default implementation");

    // With a delegate:
    Delegator d2 = d1.delegate(new Delegate());
    assert d2.operation().equals("delegate implementation");

    // Same as the above, but with an anonymous class:
    Delegator d3 = d2.delegate(new Thingable() {
      @Override
      public String thing() {
        return "anonymous delegate implementation";
      }
    });
    assert d3.operation().equals("anonymous delegate implementation");

    // Same as the above, but with a method reference:
    Delegator d4 = d3.delegate(DelegateTest::thingable);
    assert d4.operation().equals("method reference implementation");

    // Same as the above, but with a lambda expression:
    Delegator d5 = d4.delegate(() -> "lambda expression implementation");
    assert d5.operation().equals("lambda expression implementation");
  }
}
```



## JavaScript

```javascript
function Delegator() {
  this.delegate = null ;
  this.operation = function(){
    if(this.delegate && typeof(this.delegate.thing) == 'function')
      return this.delegate.thing() ;
    return 'default implementation' ;
  }
}

function Delegate() {
  this.thing = function(){
    return 'Delegate Implementation' ;
  }
}

function testDelegator(){
  var a = new Delegator() ;
  document.write(a.operation() + "\n") ;

  a.delegate = 'A delegate may be any object' ;
  document.write(a.operation() + "\n") ;

  a.delegate = new Delegate() ;
  document.write(a.operation() + "\n") ;
}
```



## Julia

'''Module''':

```julia
module Delegates

export Delegator, Delegate

struct Delegator{T}
    delegate::T
end

struct Delegate end

operation(x::Delegator) = thing(x.delegate)
thing(::Any) = "default implementation"
thing(::Delegate) = "delegate implementation"

end  # module Delegates
```


'''Main''':

```julia
using .Delegates

a = Delegator(nothing)
b = Delegator("string")

d = Delegate()
c = Delegator(d)

@show Delegates.operation(a)
@show Delegates.operation(b)
@show Delegates.operation(c)
```


```txt
Delegates.operation(a) = "default implementation"
Delegates.operation(b) = "default implementation"
Delegates.operation(c) = "delegate implementation"
```



## Kotlin

Whilst Kotlin supports class delegation 'out of the box', the delegate and delegator both have to implement a particular interface and the delegate cannot be optional or null.

The first two scenarios are not therefore strictly possible though the second can be simulated by passing a 'responds' parameter to the delegate class constructor.

```scala
// version 1.1.51

interface Thingable {
    fun thing(): String?
}

class Delegate(val responds: Boolean) : Thingable {
    override fun thing() = if (responds) "delegate implementation" else null
}

class Delegator(d: Delegate) : Thingable by d {
    fun operation() = thing() ?: "default implementation"
}

fun main(args: Array<String>) {
    // delegate doesn't respond to 'thing'
    val d = Delegate(false)
    val dd = Delegator(d)
    println(dd.operation())

    // delegate responds to 'thing'
    val d2 = Delegate(true)
    val dd2 = Delegator(d2)
    println(dd2.operation())
}
```


```txt

default implementation
delegate implementation

```



## Latitude


```latitude
Delegator ::= Object clone tap {
  self delegate := Nil.
  self clone := {
    Parents above (parent self, 'clone) call tap {
      self delegate := #'(self delegate).
    }.
  }.
  self operation := {
    localize.
    if { this delegate slot? 'thing. } then {
      this delegate thing.
    } else {
      "default implementation".
    }.
  }.
}.

Delegate ::= Object clone tap {
  self thing := "delegate implementation".
}.

;; No delegate
foo := Delegator clone.
println: foo operation. ;; "default implementation"

;; Delegate which lacks `thing`
foo delegate := Object.
println: foo operation. ;; "default implementation"

;; Delegate which implements `thing`
foo delegate := Delegate.
println: foo operation. ;; "delegate implementation"
```



## Logtalk

We use prototypes instead of classes for simplicity.

```logtalk
% define a category for holding the interface
% and implementation for delegator objects

:- category(delegator).

    :- public(delegate/1).
    :- public(set_delegate/1).

    :- private(delegate_/1).
    :- dynamic(delegate_/1).

    delegate(Delegate) :-
        ::delegate_(Delegate).

    set_delegate(Delegate) :-
        ::retractall(delegate_(Delegate)),
        ::assertz(delegate_(Delegate)).

:- end_category.

% define a simpler delegator object, with a
% method, operation/1, for testing delegation

:- object(a_delegator,
    imports(delegator)).

    :- public(operation/1).

    operation(String) :-
        (   ::delegate(Delegate), Delegate::current_predicate(thing/1) ->
            % a delegate is defined that understands the method thing/1
            Delegate::thing(String)
        ;   % otherwise just use the default implementation
            String = 'default implementation'
        ).

:- end_object.

% define an interface for delegate objects

:- protocol(delegate).

    :- public(thing/1).

:- end_protocol.

% define a simple delegate

:- object(a_delegate,
    implements(delegate)).

    thing('delegate implementation').

:- end_object.

% define a simple object that doesn't implement the "delegate" interface

:- object(an_object).

:- end_object.

% test the delegation solution when this file is compiled and loaded

:- initialization((
    % without a delegate:
    a_delegator::operation(String1),
    String1 == 'default implementation',
    % with a delegate that does not implement thing/1:
    a_delegator::set_delegate(an_object),
    a_delegator::operation(String2),
    String2 == 'default implementation',
    % with a delegate that implements thing/1:
    a_delegator::set_delegate(a_delegate),
    a_delegator::operation(String3),
    String3 == 'delegate implementation'
)).
```



## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
	\\ there are some kinds of objects in M2000, one of them is the Group, the user object
	\\ the delegate is a pointer to group
	\\ 1. We pass parameters to function operations$(), $ means that this function return string value
	\\ 2. We see how this can be done with pointers to group
	global doc$  \\ first define a global (for this module) to log output
	document doc$="Output:"+{
	}
	class Delegator {
	private:
		group delegate
		group null
	public:
		function operation$ {
			if not .delegate is .null then
				try ok {
					ret$="Delegate implementation:"+.delegate=>operation$(![])
					\\ [] is the stack of values (leave empty stack), ! used to place this to callee stack
				}
				if not ok or error then ret$="No implementation"
			else
				ret$= "Default implementation"
			end if
			\\ a global  variable and all group members except arrays use <= not =. Simple = used for declaring local variables
			doc$<=ret$+{
			}
			=ret$
		}
	class:
		Module Delegator {
				class none {}
				.null->none()
				If match("G") then .delegate->(group) else .delegate<=.null
		}
	}

	Class Thing {
		function operation$(a,b) {
			=str$(a*b)
		}
	}
	Module CallbyReference (&z as group) {
		Print Z.operation$(5,30)
	}
	Module CallbyValue (z as group) {
		Print Z.operation$(2,30)
	}
	Module CallbyReference2 (&z as pointer) {
		Print Z=>operation$(5,30)
	}
	Module CallbyValue2 (z as pointer) {
		Print Z=>operation$(2,30)
	}
	\\ Normal Group  ' no logging to doc$
	N=Thing()
	Print N.operation$(10,20)
	CallbyReference &N
	CallbyValue N
	N1->N    ' N1 is a pointer to a named group
	Print N1=>operation$(10,20)
	CallbyReference2 &N1
	CallbyValue2 N1
	N1->(N)  ' N1 now is a pointer to a float group (a copy of N)
 	Print N1=>operation$(10,20)
	CallbyReference2 &N1
	CallbyValue2 N1
	\\ using named groups (A is a group, erased when this module exit)
	A=Delegator()
	B=Delegator(Thing())
	Print A.operation$(10,20)
	Print B.operation$(10,20)
	A=B
	CallbyReference &A
	CallbyValue A
	\\ M2000 has two kinds of pointers to groups
	\\ one is a pointer to a no named group (a float group)
	\\ a float group leave until no pointer refer to it
	\\ using pointers to groups (A1 is a pointer to Group)
	A1->Delegator()
	B1->Delegator(Thing())
	Print A1=>operation$(10,20)
	Print B1=>operation$(10,20)
	A1=B1
	CallbyReference2 &A1
	CallbyValue2 A1
	\\ Second type is a pointer to a named group
	\\ the pointer hold a weak reference to named group
	\\ so a returned pointer of thid kind can be invalid if actual reference not exist
	A=Delegator() ' copy a float group to A
	A1->A
	B1->B
	Print A1=>operation$(10,20)
	Print B1=>operation$(10,20)
	A1=B1
	CallbyReference2 &A1
	CallbyValue2 A1
	Group Something {
	}
	B=Delegator(Something)
	Print B.operation$(10,20)
	CallbyReference &B
	CallbyValue B
	Report Doc$
	Clipboard Doc$
}
Checkit


```

```txt

 200
 150
 60
 200
 150
 60
 200
 150
 60
Output:
Default implementation
Delegate implementation: 200
Delegate implementation: 150
Delegate implementation: 60
Default implementation
Delegate implementation: 200
Delegate implementation: 150
Delegate implementation: 60
Default implementation
Delegate implementation: 200
Delegate implementation: 150
Delegate implementation: 60
No implementation
No implementation
No implementation

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
delegator[del_]@operate :=
  If[StringQ[del@operate], del@operate, "default implementation"];
del1 = Null;
del2@banana = "phone";
del3@operate = "delegate implementation";
Print[delegator[#]@operate] & /@ {del1, del2, del3};
```

```txt
default implementation
default implementation
delegate implementation
```



## NGS


```NGS
{
	type Delegator

	F init(d:Delegator) d.delegate = null

	F default_impl(d:Delegator) 'default implementation'

	F operation(d:Delegator) default_impl(d)

	F operation(d:Delegator) {
		guard defined thing
		guard thing is Fun
		try {
			d.delegate.thing()
		}
		catch(e:ImplNotFound) {
			# Might be unrelated exception, so check and optionally rethrow
			e.callable !== thing throws e
			default_impl(d)
		}
	}

	F operation(d:Delegator) {
		guard d.delegate is Null
		default_impl(d)
	}


	a = Delegator()
	echo(a.operation())

	# There is no method thing(s:Str)
	a.delegate = "abc"
	echo(a.operation())

	# ... now there is method thing(s:Str)
	F thing(s:Str) 'delegate implementation'
	echo(a.operation())

}
```

```txt
default implementation
default implementation
delegate implementation
```



## Objeck

```objeck
interface Thingable {
  method : virtual : public : Thing() ~ String;
}

class Delegator {
  @delegate : Thingable;

  New() {
  }

  method : public : SetDelegate(delegate : Thingable) ~ Nil {
    @delegate := delegate;
  }

  method : public : Operation() ~ String {
    if(@delegate = Nil) {
      return "default implementation";
    }
    else {
      return @delegate->Thing();
    };
  }
}

class Delegate implements Thingable {
  New() {
  }

  method : public : Thing() ~ String {
    return "delegate implementation";
  }
}

class Example {
  function : Main(args : String[]) ~ Nil {
    # Without a delegate:
    a := Delegator->New();
    Runtime->Assert(a->Operation()->Equals("default implementation"));

    # With a delegate:
    d := Delegate->New();
    a->SetDelegate(d);
    Runtime->Assert(a->Operation()->Equals("delegate implementation"));

    # Same as the above, but with an anonymous class:
    a->SetDelegate(Base->New() implements Thingable {
      method : public : Thing() ~ String {
        return "anonymous delegate implementation";
      }
    });

    Runtime->Assert(a->Operation()->Equals("anonymous delegate implementation"));
  }
}

```


=={{header|Objective-C}}==
Classic Objective-C
```objc>#import <Foundation/Foundation.h


@interface Delegator : NSObject {

    id delegate;
}

- (id)delegate;
- (void)setDelegate:(id)obj;
- (NSString *)operation;

@end

@implementation Delegator

- (id)delegate {

    return delegate;
}

- (void)setDelegate:(id)obj {

    delegate = obj; // Weak reference
}

- (NSString *)operation {

    if ([delegate respondsToSelector:@selector(thing)])
        return [delegate thing];

    return @"default implementation";
}

@end

// Any object may implement these
@interface NSObject (DelegatorDelegating)

- (NSString *)thing;

@end

@interface Delegate : NSObject

// Don't need to declare -thing because any NSObject has this method

@end

@implementation Delegate

- (NSString *)thing {

    return @"delegate implementation";
}

@end

// Example usage
// Memory management ignored for simplification
int main() {

    // Without a delegate:
    Delegator *a = [[Delegator alloc] init];
    NSLog(@"%d\n", [[a operation] isEqualToString:@"default implementation"]);

    // With a delegate that does not implement thing:
    [a setDelegate:@"A delegate may be any object"];
    NSLog(@"%d\n", [[a operation] isEqualToString:@"delegate implementation"]);

    // With a delegate that implements "thing":
    Delegate *d = [[Delegate alloc] init];
    [a setDelegate:d];
    NSLog(@"%d\n", [[a operation] isEqualToString:@"delegate implementation"]);

    return 0;
}
```


Objective-C 2.0, modern runtime, Automatic Reference Counting, Autosynthesize (LLVM 4.0+)
```objc>#import <Foundation/Foundation.h


// Formal protocol for the delegate
@protocol DelegatorDelegatingProtocol
    - (NSString *)thing;
@end

@interface Delegator : NSObject
    @property (weak) id delegate;
    - (NSString *)operation;
@end
@implementation Delegator
    - (NSString *)operation {
        if ([self.delegate respondsToSelector: @selector(thing)])
            return [self.delegate thing];

        return @"default implementation";
    }
@end

@interface Delegate : NSObject
    <DelegatorDelegatingProtocol>
@end
@implementation Delegate
    - (NSString *)thing { return @"delegate implementation"; }
@end

// Example usage with Automatic Reference Counting
int main() {
    @autoreleasepool {
        // Without a delegate:
        Delegator *a = [Delegator new];
        NSLog(@"%@", [a operation]);    // prints "default implementation"

        // With a delegate that does not implement thing:
        a.delegate = @"A delegate may be any object";
        NSLog(@"%@", [a operation]);    // prints "default implementation"

        // With a delegate that implements "thing":
        Delegate *d = [Delegate new];
        a.delegate = d;
        NSLog(@"%@", [a operation]);    // prints "delegate implementation"
    }
    return 0;
}
```



## Oforth


```Oforth
Object Class new: Delegate1

Object Class new: Delegate2
Delegate2 method: thing  "Delegate implementation" println ;

Object Class new: Delegator(delegate)
Delegator method: initialize  := delegate ;

Delegator method: operation
   @delegate respondTo(#thing) ifTrue: [ @delegate thing return ]
   "Default implementation" println ;
```

Usage :


```Oforth
Delegator new(null) operation
Default implementation

Delegator new(Delegate1 new) operation
Default implementation

Delegator new(Delegate2 new) operation
Delegate implementation
```



## ooRexx


```ooRexx

delegator = .delegator~new   -- no delegate
say delegator~operation
-- an invalid delegate type
delegator~delegate = "Some string"
say delegator~operation
-- a good delegate
delegator~delegate = .thing~new
say delegator~operation
-- a directory object with a thing entry defined
d = .directory~new
d~thing = "delegate implementation"
delegator~delegate = d
say delegator~operation

-- a class we can use as a delegate
::class thing
::method thing
  return "delegate implementation"

::class delegator
::method init
  expose delegate
  use strict arg delegate = .nil

::attribute delegate

::method operation
  expose delegate
  if delegate == .nil then return "default implementation"

  -- Note:  We could use delegate~hasMethod("THING") to check
  -- for a THING method, but this will fail of the object relies
  -- on an UNKNOWN method to handle the method.  By trapping
  -- NOMETHOD conditions, we can allow those calls to go
  -- through
  signal on nomethod
  return delegate~thing

nomethod:
  return "default implementation"

```



## OxygenBasic


```oxygenbasic

class DelegateA 'not implmenting thing()
'
### ========

'
string message

end class

class DelegateB 'implementing thing()
'
### ========

'
string message

method thing() as string
return message
end method
'
end class


Class Delegator
'
### ========

'
has DelegateA dgA
has DelegateB dgB
'
method operation() as DelegateB
dgB.message="Delegate Implementation"
return @dgB
end method

method thing() as string
return "not using Delegate"
end method
'
end class

'====
'TEST
'====

Delegator dgr
let dg=dgr.operation
print dgr.thing 'result "not using Delegate"
print dg.thing  'result "Delegate Implementation"

```



## Oz

```oz
declare
  class Delegator from BaseObject
     attr
	delegate:unit

     meth set(DG)
	{Object.is DG} = true %% assert: DG must be an object
	delegate := DG
     end

     meth operation($)
	if @delegate == unit then
	   {self default($)}
	else
	   try
	      {@delegate thing($)}
	   catch error(object(lookup ...) ...) then
	      %% the delegate did not understand the message
	      {self default($)}
	   end
	end
     end

     meth default($)
	"default implementation"
     end
  end

  class Delegate from BaseObject
     meth thing($)
	"delegate Implementation"
     end
  end

  A = {New Delegator noop}
in
  {System.showInfo {A operation($)}}

  {A set({New BaseObject noop})}
  {System.showInfo {A operation($)}}

  {A set({New Delegate noop})}
  {System.showInfo {A operation($)}}
```



## Pascal

See [[Delegates#Delphi | Delphi]]


## Perl

```perl
use strict;

package Delegator;
sub new {
   bless {}
}
sub operation {
   my ($self) = @_;
   if (defined $self->{delegate} && $self->{delegate}->can('thing')) {
      $self->{delegate}->thing;
   } else {
      'default implementation';
   }
}
1;

package Delegate;
sub new {
   bless {};
}
sub thing {
   'delegate implementation'
}
1;


package main;
# No delegate
my $a = Delegator->new;
$a->operation eq 'default implementation' or die;

# With a delegate that does not implement "thing"
$a->{delegate} = 'A delegate may be any object';
$a->operation eq 'default implementation' or die;

# With delegate that implements "thing"
$a->{delegate} = Delegate->new;
$a->operation eq 'delegate implementation' or die;

```


Using Moose.


```perl

use 5.010_000;

package Delegate::Protocol
use Moose::Role;
# All methods in the Protocol is optional
#optional  'thing';
# If we wanted to have a required method, we would state:
# requires 'required_method';
#

package Delegate::NoThing;
use Moose;
with 'Delegate::Protocol';

package Delegate;
use Moose;

#  The we confirm to Delegate::Protocol
with 'Delegate::Protocol';
sub thing { 'delegate implementation' };

package Delegator;
use Moose;

has delegate => (
     is      => 'rw',
    does => 'Delegate::Protocol', # Moose insures that the delegate confirms to the protocol.
   predicate => 'hasDelegate'
);

sub operation {

    my ($self) = @_;
    if( $self->hasDelegate  && $self->delegate->can('thing') ){
        return $self->delegate->thing() . $postfix; # we are know that delegate has thing.
    } else {
        return 'default implementation';
   }
};

package main;
use strict;

# No delegate
my $delegator = Delegator->new();
$delegator->operation eq 'default implementation' or die;

# With a delegate that does not implement "thing"
$delegator->delegate(  Delegate::NoThing->new );
$delegator->operation eq 'default implementation' or die;

# With delegate that implements "thing"
$delegator->delegate(  Delegate->new );
$delegator->operation eq 'delegate implementation' or die;


```



## Perl 6


```perl6
class Non-Delegate  { }

class Delegate {
	method thing {
		return "delegate implementation"
	}
}

class Delegator {
	has $.delegate is rw;

	method operation {
		$.delegate.^can( 'thing' ) ?? $.delegate.thing
		!! "default implementation"
	}
}

my Delegator $d .= new;

say "empty: "~$d.operation;

$d.delegate = Non-Delegate.new;

say "Non-Delegate: "~$d.operation;

$d.delegate = Delegate.new;

say "Delegate: "~$d.operation;
```



## Phix

Phix is not object orientated, instead I would just use a routine_id for this sort of thing.

I will admit that the whole concept of "no delegate/with one that does not implement" makes no sense whatsoever to me.

While I've shown this using a single rid, you could of course hold an entire sequence of them or even better a dictionary and use named lookups for rids in that.

```Phix
enum OTHER, OPERATION

function operation(object o)
    integer rid = o[OPERATION]
    if rid!=NULL then
        return call_func(rid,{})
    end if
    return "no implementation"
end function

function xthing()
    return "default implementation"
end function

function newX()
    return {1,routine_id("xthing"),2}
end function

function newY()
    object res = newX()
    res[OTHER] = "something else"
    -- remove delegate:
    res[OPERATION] = NULL
    return res
end function

function zthing()
    return "delegate implementation"
end function

function newZ()
    object res = newX()
    -- replace delegate:
    res[OPERATION] = routine_id("zthing")
    return res
end function

object x = newX(),
       y = newY(),
       z = newZ()

?operation(x)
?operation(y)
?operation(z)
```


```txt

"default implementation"
"no implementation"
"delegate implementation"

```

Obviously, you can explictly test for rid=NULL as shown, or remove that test and catch exceptions, ie:

```Phix
?operation(x)
try -- (since rid=NULL check commented out)
    ?operation(y)
catch e
    ?"oops, no implementation"
end try
?operation(z)
```



## PHP

```php
class Delegator {
  function __construct() {
    $this->delegate = NULL ;
  }
  function operation() {
    if(method_exists($this->delegate, "thing"))
      return $this->delegate->thing() ;
    return 'default implementation' ;
  }
}

class Delegate {
  function thing() {
    return 'Delegate Implementation' ;
  }
}

$a = new Delegator() ;
print "{$a->operation()}\n" ;

$a->delegate = 'A delegate may be any object' ;
print "{$a->operation()}\n" ;

$a->delegate = new Delegate() ;
print "{$a->operation()}\n" ;
```



## PicoLisp


```PicoLisp
(class +Delegator)
# delegate

(dm operation> ()
   (if (: delegate)
      (thing> @)
      "default implementation" ) )


(class +Delegate)
# thing

(dm T (Msg)
   (=: thing Msg) )

(dm thing> ()
   (: thing) )


(let A (new '(+Delegator))
   # Without a delegate
   (println (operation> A))

   # With delegate that does not implement 'thing>'
   (put A 'delegate (new '(+Delegate)))
   (println (operation> A))

   # With delegate that implements 'thing>'
   (put A 'delegate (new '(+Delegate) "delegate implementation"))
   (println (operation> A)) )
```

Output:

```txt
"default implementation"
NIL
"delegate implementation"
```



## Pop11


```pop11
uses objectclass;
define :class Delegator;
    slot delegate = false;
enddefine;

define :class Delegate;
enddefine;

define :method thing(x : Delegate);
   'delegate implementation'
enddefine;

define :method operation(x : Delegator);
if delegate(x) and fail_safe(delegate(x), thing) then
   ;;; Return value is on the stack
else
   'default implementation'
endif;
enddefine;

;;; Default, without a delegate
lvars a = newDelegator();
operation(a) =>

;;; a delegating to itself (works because Delegator does not
;;; implement thing)
a -> delegate(a);
operation(a) =>

;;; delegating to a freshly created Delegate
newDelegate() -> delegate(a);
operation(a) =>
```



## Python


```python
class Delegator:
   def __init__(self):
      self.delegate = None
   def operation(self):
       if hasattr(self.delegate, 'thing') and callable(self.delegate.thing):
          return self.delegate.thing()
       return 'default implementation'

class Delegate:
   def thing(self):
      return 'delegate implementation'

if __name__ == '__main__':

   # No delegate
   a = Delegator()
   assert a.operation() == 'default implementation'

   # With a delegate that does not implement "thing"
   a.delegate = 'A delegate may be any object'
   assert a.operation() == 'default implementation'

   # With delegate that implements "thing"
   a.delegate = Delegate()
   assert a.operation() == 'delegate implementation'
```



## Racket


<code>[[http://docs.racket-lang.org/reference/objectutils.html?q=is-a%3F#%28def._%28%28lib._racket%2Fprivate%2Fclass-internal..rkt%29._object-method-arity-includes~3f%29%29 object-method-arity-includes?]]</code>
tests for the existence of the method in an object.
<code>[[http://docs.racket-lang.org/reference/objectutils.html?q=is-a%3F#%28def._%28%28lib._racket%2Fprivate%2Fclass-internal..rkt%29._is-a~3f%29%29 is-a?]]</code>
can be used to test for a class instance or interface implementor; and is
probably more likely to be used in anger. But
<code>object-method-arity-includes?</code> can be used generally; and actually
follows the requirement of the task better.


```racket
#lang racket
;; Delegates. Tim Brown 2014-10-16

(define delegator%
  (class object%
    (init-field [delegate #f])
    (define/public (operation)
      (cond [(and (object? delegate) (object-method-arity-includes? delegate 'thing 0))
             (send delegate thing)]
            [else "default implementation"]))
    (super-new)))

(define non-thinging-delegate% (class object% (super-new)))

(define thinging-delegate%
  (class object%
    (define/public (thing) "delegate implementation")
    (super-new)))

(module+ test
  (require tests/eli-tester)
  (define delegator-1 (new delegator%))
  (define delegator-2 (new delegator%))
  (define non-thinging-delegate (new non-thinging-delegate%))
  (define thinging-delegate     (new thinging-delegate%))

  (test
   (send delegator-1 operation) => "default implementation"
   (send delegator-2 operation) => "default implementation"
   (set-field! delegate delegator-1 non-thinging-delegate) => (void)
   (set-field! delegate delegator-2 thinging-delegate)     => (void)
   (send delegator-1 operation) => "default implementation"
   (send delegator-2 operation) => "delegate implementation"
   (send (new delegator% [delegate thinging-delegate]) operation) => "delegate implementation"))

```


All the tests pass. Believe me.


## Ruby

```ruby
class Delegator
   attr_accessor :delegate
   def operation
      if @delegate.respond_to?(:thing)
         @delegate.thing
      else
         'default implementation'
      end
   end
end

class Delegate
   def thing
      'delegate implementation'
   end
end

if __FILE__ == $PROGRAM_NAME

   # No delegate
   a = Delegator.new
   puts a.operation # prints "default implementation"

   # With a delegate that does not implement "thing"
   a.delegate = 'A delegate may be any object'
   puts a.operation # prints "default implementation"

   # With delegate that implements "thing"
   a.delegate = Delegate.new
   puts a.operation # prints "delegate implementation"
end
```


Using Forwardable lib


```ruby
require 'forwardable'

class Delegator; extend Forwardable
  attr_accessor :delegate
  def_delegator :@delegate, :thing, :delegated

  def initialize
    @delegate = Delegate.new()
  end
end

class Delegate
  def thing
    'Delegate'
  end
end

a = Delegator.new
puts a.delegated # prints "Delegate"

```



## Rust

Requiring delegates to implement Thingable:

```Rust
trait Thingable {
    fn thing(&self) -> &str;
}

struct Delegator<T>(Option<T>);

struct Delegate {}

impl Thingable for Delegate {
    fn thing(&self) -> &'static str {
        "Delegate implementation"
    }
}

impl<T: Thingable> Thingable for Delegator<T> {
    fn thing(&self) -> &str {
        self.0.as_ref().map(|d| d.thing()).unwrap_or("Default implmementation")
    }
}

fn main() {
    let d: Delegator<Delegate> = Delegator(None);
    println!("{}", d.thing());

    let d: Delegator<Delegate> = Delegator(Some(Delegate {}));
    println!("{}", d.thing());
}
```

```txt
Default implmementation
Delegate implementation
```


Using nightly-only specialization feature:

```Rust
#![feature(specialization)]

trait Thingable {
    fn thing(&self) -> &str;
}

struct Delegator<T>(Option<T>);

struct Delegate {}

impl Thingable for Delegate {
    fn thing(&self) -> &'static str {
        "Delegate implementation"
    }
}

impl<T> Thingable for Delegator<T> {
    default fn thing(&self) -> &str {
        "Default implementation"
    }
}

impl<T: Thingable> Thingable for Delegator<T> {
    fn thing(&self) -> &str {
        self.0.as_ref().map(|d| d.thing()).unwrap_or("Default implmementation")
    }
}

fn main() {
    let d: Delegator<i32> = Delegator(None);
    println!("{}", d.thing());

    let d: Delegator<i32> = Delegator(Some(42));
    println!("{}", d.thing());

    let d: Delegator<Delegate> = Delegator(None);
    println!("{}", d.thing());

    let d: Delegator<Delegate> = Delegator(Some(Delegate {}));
    println!("{}", d.thing());
}
```

```txt
Default implementation
Default implementation
Default implmementation
Delegate implementation
```



## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/cCYD9tQ/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/2TWYJifpTuOVhrAfWP51oA Scastie (remote JVM)].

```Scala
trait Thingable {
  def thing: String
}

class Delegator {
  var delegate: Thingable = _

  def operation: String = if (delegate == null) "default implementation"
  else delegate.thing
}

class Delegate extends Thingable {
  override def thing = "delegate implementation"
}

// Example usage
// Memory management ignored for simplification
object DelegateExample extends App {

  val a = new Delegator
  assert(a.operation == "default implementation")
  // With a delegate:
  val d = new Delegate
  a.delegate = d
  assert(a.operation == "delegate implementation")
  // Same as the above, but with an anonymous class:
  a.delegate = new Thingable() {
    override def thing = "anonymous delegate implementation"
  }
  assert(a.operation == "anonymous delegate implementation")

}
```


## Sidef


```ruby
class NonDelegate { }

class Delegate {
    method thing {
        return "delegate implementation"
    }
}

class Delegator (delegate = null) {
    method operation {

        if (delegate.respond_to(:thing)) {
            return delegate.thing
        }

        return "default implementation"
    }
}

var d = Delegator()
say "empty: #{d.operation}"
d.delegate = NonDelegate()
say "NonDelegate: #{d.operation}"
d.delegate = Delegate()
say "Delegate: #{d.operation}"
```

 empty: default implementation
 NonDelegate: default implementation
 Delegate: delegate implementation


## Smalltalk

Definition of the thingy:

```smalltalk
Object
 subclass:#Thingy
 instanceVariableNames:''

thing
    ^ 'thingy implementation'
```

Definition of the delegator:

```smalltalk
Object
 subclass:#Delegator
 instanceVariableNames:'delegate'

delegate:something
    delegate := something

operation
    ^ delegate
        perform:#thing ifNotUnderstood:'default implementation'.
```

Sample use:

```smalltalk
|d|
d := Delegator new.
d operation.
-> 'default implementation'

d delegate:(Thingy new).
d operation.
-> 'thingy implementation'
```



## Swift

Allowing the delegate to be any type and taking advantage of dynamism of method lookup:

```swift
import Foundation

protocol Thingable { // prior to Swift 1.2, needs to be declared @objc
  func thing() -> String
}

class Delegator {
  weak var delegate: AnyObject?
  func operation() -> String {
    if let f = self.delegate?.thing {
      return f()
    } else {
      return "default implementation"
    }
  }
}

class Delegate {
  dynamic func thing() -> String { return "delegate implementation" }
}

// Without a delegate:
let a = Delegator()
println(a.operation())    // prints "default implementation"

// With a delegate that does not implement thing:
a.delegate = "A delegate may be any object"
println(a.operation())    // prints "default implementation"

// With a delegate that implements "thing":
let d = Delegate()
a.delegate = d
println(a.operation())    // prints "delegate implementation"
```



Alternately, requiring the delegate to conform to a given protocol:

```swift
protocol Thingable : class {
  func thing() -> String
}

class Delegator {
  weak var delegate: Thingable?
  func operation() -> String {
    if let d = self.delegate {
      return d.thing()
    } else {
      return "default implementation"
    }
  }
}

class Delegate : Thingable {
  func thing() -> String { return "delegate implementation" }
}

// Without a delegate:
let a = Delegator()
println(a.operation())    // prints "default implementation"

// With a delegate:
let d = Delegate()
a.delegate = d
println(a.operation())    // prints "delegate implementation"
```



## Tcl

Uses [[Assertions#Tcl]]

```tcl
package require TclOO

oo::class create Delegate {
    method thing {} {
        return "delegate impl."
    }
    export thing
}

oo::class create Delegator {
    variable delegate
    constructor args {
        my delegate {*}$args
    }

    method delegate args {
        if {[llength $args] == 0} {
            if {[info exists delegate]} {
                return $delegate
            }
        } elseif {[llength $args] == 1} {
            set delegate [lindex $args 0]
        } else {
            return -code error "wrong # args: should be \"[self] delegate ?target?\""
        }
    }

    method operation {} {
        try {
            set result [$delegate thing]
        } on error e {
            set result "default implementation"
        }
        return $result
    }
}

# to instantiate a named object, use: class create objname; objname aMethod
# to have the class name the object:  set obj [class new]; $obj aMethod

Delegator create a
set b [Delegator new "not a delegate object"]
set c [Delegator new [Delegate new]]

assert {[a operation] eq "default implementation"}   ;# a "named" object, hence "a ..."
assert {[$b operation] eq "default implementation"}  ;# an "anonymous" object, hence "$b ..."
assert {[$c operation] ne "default implementation"}

# now, set a delegate for object a
a delegate [$c delegate]
assert {[a operation] ne "default implementation"}

puts "all assertions passed"
```


To code the <code>operation</code> method without relying on catching an exception, but strictly by using introspection:

```tcl
method operation {} {
    if { [info exists delegate] &&
         [info object isa object $delegate] &&
         "thing" in [info object methods $delegate -all]
    } then {
        set result [$delegate thing]
    } else {
        set result "default implementation"
    }
}
```



## Vorpal

Delegate objects can be an array of delegates or as a single delegate.

```vorpal
a = new()
a.f = method(){
        .x.print()
}

c = new()
c.g = method(){
        (.x + 1).print()
}

# array of delegates
b = new()
b.delegate = new()
b.delegate[0] = a
b.delegate[1] = c
b.x = 3
b.f()
b.g()

# single delegate
d = new()
d.delegate = a
d.x = 7
d.f()
```


The resulting output:

```txt

3
4
7

```



## zkl

```zkl
class Thingable{ var thing; }

class Delegator{
   var delegate;
   fcn operation{
      if (delegate) delegate.thing;
      else "default implementation"
   }
}

class Delegate(Thingable){ thing = "delegate implementation" }
```



```zkl
    // Without a delegate:
a:= Delegator();
a.operation().println(); //--> "default implementation"

    // With a delegate:
a.delegate = Delegate();
a.operation().println(); //-->"delegate implementation"
```

A second example

```zkl
class [static] Logger{ // Only one logging resource
   var [mixin=File] dst; // File like semantics, eg Data, Pipe
   dst = File.DevNull;
   // initially, the logger does nothing
   fcn log(msg){dst.writeln(vm.pasteArgs())}
}
```



```zkl
Logger.log("this is a test"); //-->nada
Logger.dst=Console;
Logger.log("this is a test 2"); //-->writes to Console

class B(Logger){ log("Hello from ",self,"'s constructor"); }
B(); //-->Hello from Class(B)'s constructor
```


The base class B was constructed at startup,
so the first Hello went to DevNull as all base classes are created before
code runs (base classes are used to create class instances, eg B()).


<!-- Omit From|ALGOL 68 although it isn't immediately obvious that ALGOL 68 is object oriented, if C can do it... -->
