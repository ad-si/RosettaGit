+++
title = "Singleton"
description = ""
date = 2019-10-10T14:08:12Z
aliases = []
[extra]
id = 2501
[taxonomies]
categories = ["task", "Object oriented"]
tags = []
languages = [
  "action_script",
  "ada",
  "auto_hotkey",
  "c",
  "c_plus_plus",
  "c_sharp",
  "common_lisp",
  "d",
  "e",
  "eiffel",
  "elena",
  "erlang",
  "factor",
  "forth",
  "go",
  "groovy",
  "io",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lasso",
  "lingo",
  "logtalk",
  "net_rexx",
  "nim",
  "objeck",
  "oforth",
  "oo_rexx",
  "oxygen_basic",
  "oz",
  "perl",
  "perl_6",
  "php",
  "picolisp",
  "python",
  "purebasic",
  "racket",
  "ruby",
  "scala",
  "sidef",
  "slate",
  "smalltalk",
  "swift",
  "tcl",
  "tern",
  "vala",
  "zkl",
]
+++

## Task

A Global Singleton is a class of which only one instance exists within a program.

Any attempt to use non-static members of the class involves performing operations on this one instance.





## ActionScript


```actionscript
package
{
    public class Singleton
    {

        private static var instance:Singleton;

        // ActionScript does not allow private or protected constructors.
        public function Singleton(enforcer:SingletonEnforcer) {

        }

        public static function getInstance():Singleton {
            if (instance == null) instance = new Singleton(new SingletonEnforcer());
            return instance;
        }
    }
}

internal class SingletonEnforcer {}
```



## Ada


### Non Thread Safe


```ada
package Global_Singleton is
   procedure Set_Data (Value : Integer);
   function Get_Data return Integer;
private
   type Instance_Type is record
      -- Define instance data elements
      Data : Integer := 0;
   end record;
   Instance : Instance_Type;
end Global_Singleton;
```



```ada
package body Global_Singleton is

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data (Value : Integer) is
   begin
      Instance.Data := Value;
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data return Integer is
   begin
      return Instance.Data;
   end Get_Data;

end Global_Singleton;
```



### Thread Safe


```ada
package Protected_Singleton is
   procedure Set_Data (Value : Integer);
   function Get_Data return Integer;
private
   protected Instance is
      procedure Set(Value : Integer);
      function Get return Integer;
   private
      Data : Integer := 0;
   end Instance_Type;
end Protected_Singleton;
```



```ada
package body Protected_Singleton is

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data (Value : Integer) is
   begin
      Instance.Set(Value);
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data return Integer is
   begin
      return Instance.Get;
   end Get_Data;

   --------------
   -- Instance --
   --------------

   protected body Instance is

      ---------
      -- Set --
      ---------

      procedure Set (Value : Integer) is
      begin
         Data := Value;
      end Set;

      ---------
      -- Get --
      ---------

      function Get return Integer is
      begin
         return Data;
      end Get;

   end Instance;

end Protected_Singleton;
```



## AutoHotkey

Translation of python borg pattern

```AutoHotkey
b1 := borg()
b2 := borg()
msgbox % "b1 is b2? " . (b1 == b2)
b1.datum := 3
msgbox % "b1.datum := 3`n...`nb1 datum: " b1.datum "`nb2 datum: " b2.datum ; is 3 also
msgbox % "b1.datum is b2.datum ? " (b1.datum == b2.datum)
return


borg(){
   static borg
   If !borg
      borg := Object("__Set", "Borg_Set"
                   , "__Get", "Borg_Get")
   return object(1, borg, "base", borg)
}


Borg_Get(brg, name)
{
  Return brg[1, name]
}

Borg_Set(brg, name, val)
{
  brg[1, name] := val
  Return val
}
```



## C

Since C doesn't really support classes anyhow, there's not much to do.  If you want somethin akin to a singleton, what you do is first declare the interface functions in a header (.h) file.

```c
#ifndef SILLY_H
#define SILLY_H

extern void JumpOverTheDog( int numberOfTimes);
extern int  PlayFetchWithDog( float weightOfStick);

#endif
```

Then in a separate C source (.c) file, define your structures, variables and functions.

```c
...
#include "silly.h"

struct sDog {
   float max_stick_weight;
   int   isTired;
   int   isAnnoyed;
};

static struct sDog lazyDog = { 4.0, 0,0 };

/* define functions used by the functions in header as static */
static int RunToStick( )
{...
}
/* define functions declared in the header file. */

void JumpOverTheDog(int numberOfTimes)
{ ...
   lazyDog.isAnnoyed = TRUE;
}
int PlayFetchWithDog( float weightOfStick )
{ ...
   if(weightOfStick < lazyDog.max_stick_weight){...
}
```

Code using the singleton includes the header and cannot create a
struct sDog as the definition is only in the C source (or other header privately included by the silly.c source).  Only the functions declared in the header may be used externally.

```c
...
#include "silly.h"
...
/* code using the dog methods */
   JumpOverTheDog( 4);
   retrieved = PlayFetchWithDog( 3.1);
...
```



## C++

A generic singleton template class (implemented via the "Curiously Recurring Template Pattern"[https://en.wikipedia.org/wiki/Curiously_recurring_template_pattern]). Warning: if using a version of C++ prior to C++11, a [[Mutex#C|mutex]] (or similar) is required to access static variables within a multi-threaded program.


```cpp

#include <stdexcept>

template <typename Self>
class singleton
{
protected:
	static Self*
		sentry;
public:
	static Self&
		instance()
	{
		return *sentry;
	}
	singleton()
	{
		if(sentry)
			throw std::logic_error("Error: attempt to instantiate a singleton over a pre-existing one!");
		sentry = (Self*)this;
	}
	virtual ~singleton()
	{
		if(sentry == this)
			sentry = 0;
	}
};
template <typename Self>
Self*
	singleton<Self>::sentry = 0;

/*
	Example usage:
*/

#include <iostream>
#include <string>

using namespace
	std;

class controller : public singleton<controller>
{
public:
	controller(string const& name)
	: name(name)
	{
		trace("begin");
	}
	~controller()
	{
		trace("end");
	}
	void
		work()
	{
		trace("doing stuff");
	}
	void
		trace(string const& message)
	{
		cout << name << ": " << message << endl;
	}
	string
		name;
};
int
	main()
{
	controller*
		first = new controller("first");
	controller::instance().work();
	delete first;
/*
	No problem, our first controller no longer exists...
*/
	controller
		second("second");
	controller::instance().work();
	try
	{
	/*
		Never happens...
	*/
		controller
			goner("goner");
		controller::instance().work();
	}
	catch(exception const& error)
	{
		cout << error.what() << endl;
	}
	controller::instance().work();
/*
	Never happens (and depending on your system this may or may not print a helpful message!)
*/
	controller
		goner("goner");
	controller::instance().work();
}

```


## C#
===First attempt at thread-safety using locking.===
Performance suffers because the lock is acquired every time Instance is accessed.<br />
This implementation is extremely slow and should not be used (but is seen often).

```c#
public sealed class Singleton1 //Lazy: Yes ||| Thread-safe: Yes ||| Uses locking: Yes
{
    private static Singleton1 instance;
    private static readonly object lockObj = new object();

    public static Singleton1 Instance {
        get {
            lock(lockObj) {
                if (instance == null) {
                    instance = new Singleton1();
                }
            }
            return instance;
        }
    }
}
```


===Fixes excessive locking by double-checking for null.===
Still uses locking and implementation is ugly and verbose.

```c#
public sealed class Singleton2 //Lazy: Yes ||| Thread-safe: Yes ||| Uses locking: Yes, but only once
{
    private static Singleton2 instance;
    private static readonly object lockObj = new object();

    public static Singleton2 Instance {
        get {
            if (instance == null) {
                lock(lockObj) {
                    if (instance == null) {
                        instance = new Singleton2();
                    }
                }
            }
            return instance;
        }
    }
}
```



### Really simple implementation without locking.

It still is not completely lazy. If there are other static members, accessing any of those will still cause initialization.

```c#
public sealed class Singleton3 //Lazy: Yes, but not completely ||| Thread-safe: Yes ||| Uses locking: No
{
    private static Singleton3 Instance { get; } = new Singleton3();

    static Singleton3() { }
}
```



### Truly lazy by using an inner class.

This version is completely lazy but the code looks more complicated than it needs to be.

```c#
public sealed class Singleton4 //Lazy: Yes ||| Thread-safe: Yes ||| Uses locking: No
{
    public static Singleton4 Instance => SingletonHolder.instance;

    private class SingletonHolder
    {
        static SingletonHolder() { }

        internal static readonly Singleton4 instance = new Singleton4();
    }
}
```



### Using Lazy<T>

C# has a dedicated type for lazy initialization: Lazy<T>.<br />
It makes implementing a Singleton really easy. Recommended.

```c#
public sealed class Singleton5 //Lazy: Yes ||| Thread-safe: Yes ||| Uses locking: No
{
    private static readonly Lazy<Singleton5> lazy = new Lazy<Singleton5>(() => new Singleton5());

    public static Singleton5 Instance => lazy.Value;
}
```


=={{header|Caché ObjectScript}}==

In Caché, each job runs in a self-contained execution environment (i.e. a separate process instead of a thread).  However, it is possible for each process to share data through multidimensional storage (global variables).  This is because when the Caché virtual machine starts, it allocates a single, large chunk of shared memory to allow all Caché processes to access this data simultaneously.  However, it is the responsibility of the application developer to ensure read and write access to objects is properly co-ordinated (or 'synchronized') between processes to prevent concurrency problems.  Also, Caché defines any global variable whose name starts with 'CacheTemp' as being temporary, which means changes are not usually written to disk and are instead maintained within the in-memory buffer pool.


```cos

/// The <CLASS>Singleton</CLASS> class represents a global singleton object that can
/// be instantiated by multiple processes.  The 'Get' class method is used to obtain
/// an in-memory object reference and the 'Set' method is used to save any changes to
/// state.  See below for an example.
///
/// <EXAMPLE>
/// Set one=##class(Singleton).Get(,.sc)
/// Set one.GlobalProperty="Some Value"
/// Set sc=one.Set()
/// </EXAMPLE>
///
/// This class can also be extended.
Class User.Singleton Extends %SerialObject
{

Property GlobalProperty As %String;

/// Refer to <LINK href=/AboutConcurrency.html>About Concurrency</LINK> for more details
/// on the optional <var>pConcurrency</var> argument.
ClassMethod Get(pConcurrency As %Integer = -1, Output pStatus As %Status = {$$$OK}) As Singleton [ Final ]
{
	// check if singleton object already instantiated
	Set oRef = ""
	For  {
		Set oRef = $ZObjNext(oRef) If oRef = "" Quit
		If oRef.%ClassName(1) = ..%ClassName(1) Quit
	}
	If $IsObject(oRef) Quit oRef

	// determine what lock needs to be applied
	If '$IsValidNum(pConcurrency, 0, -1, 4) {
		Set pStatus = $$$ERROR($$$LockTypeInvalid, pConcurrency)
		Quit $$$NULLOREF
	}
	If pConcurrency = -1 Set pConcurrency = $Xecute("Quit "_..#DEFAULTCONCURRENCY)

	// acquire lock for global singleton object
	Set lockTO = $ZUtil(115,4), lockOK = 1
	If pConcurrency<4, pConcurrency {
		Lock +^CacheTempUser("Singleton", ..%ClassName(1))#"S":lockTO Set lockOK = $Test
	} ElseIf pConcurrency = 4 {
		Lock +^CacheTempUser("Singleton", ..%ClassName(1)):lockTO Set lockOK = $Test
	}
	If 'lockOK {
		If pConcurrency = 4 {
			Set pStatus = $$$ERROR($$$LockFailedToAcquireExclusive, ..%ClassName(1))
		} Else {
			Set pStatus = $$$ERROR($$$LockFailedToAcquireRead, ..%ClassName(1))
		}
		Quit $$$NULLOREF
	}

	// retrieve global singleton object and deserialise
	Set oId = $Get(^CacheTempUser("Singleton", ..%ClassName(1)))
	Set oRef = ..%Open(oId)  //,, .pStatus)
	If '$IsObject(oRef) Set pStatus = $$$ERROR($$$GeneralError, "Failed to load singleton object.")

	// release temporary lock
	If (pConcurrency = 1) || (pConcurrency = 2) {
		Lock -^CacheTempUser("Singleton", ..%ClassName(1))#"S"
	}

	// singleton object failed to load
	If $$$ISERR(pStatus) {
		// release retained lock
		If pConcurrency = 3 {
			Lock -^CacheTempUser("Singleton", ..%ClassName(1))#"S"
		}
		If pConcurrency = 4 {
			Lock -^CacheTempUser("Singleton", ..%ClassName(1))
		}
		Quit $$$NULLOREF
	}

	// store concurrency state and return in-memory object reference
	Set oRef.Concurrency = pConcurrency
	Quit oRef
}

Method Set() As %Status [ Final ]
{
	// check for version change
	Set oId0 = $Get(^CacheTempUser("Singleton", ..%ClassName(1)))
	Set oRef0 = ..%Open(oId0)  //,, .sc)
	If '$IsObject(oRef0) Quit $$$ERROR($$$GeneralError, "Failed to load singleton object.")
	If oRef0.Version = ..Version {
		Set ..Version = ..Version + 1
	} Else {
		Quit $$$ERROR($$$ConcurrencyVersionMismatch, ..%ClassName(1))
	}

	// serialise local singleton object and check status code
	Set sc = ..%GetSwizzleObject(,.oId) If $$$ISERR(sc) Quit sc

	// acquire exclusive lock on global singleton object
	Set lockTO = $ZUtil(115,4)
	Lock +^CacheTempUser("Singleton", ..%ClassName(1)):lockTO
	If '$Test Quit $$$ERROR($$$LockFailedToAcquireExclusive, ..%ClassName(1))

	// update global singleton object and release lock
	Set ^CacheTempUser("Singleton", ..%ClassName(1)) = oId
	Lock -^CacheTempUser("Singleton", ..%ClassName(1))
	Quit $$$OK
}

Method %OnNew() As %Status [ Final, Internal ]
{
	// do not allow constructor method to be called
	Quit $$$ERROR($$$GeneralError, "Can't instantiate directly.")
}

Method %OnConstructClone() As %Status [ Final, Internal ]
{
	// do not allow singleton object to be cloned
	Quit $$$ERROR($$$GeneralError, "Can't clone instance.")
}

Method %OnClose() As %Status [ Final, Internal ]
{
	// reference count for singleton object is now zero, so
	// release lock on global singleton object, if applicable
	If ..Concurrency = 3 Lock -^CacheTempUser("Singleton", ..%ClassName(1))#"S"
	If ..Concurrency = 4 Lock -^CacheTempUser("Singleton", ..%ClassName(1))
	Quit $$$OK
}

Property Concurrency As %Integer [ Final, Private, Transient ];

Property Version As %Integer [ Final, Private ];

}

```


```txt

USER>Set one=##class(Singleton).Get()
USER>Set one.GlobalProperty="Some Value"
USER>Set sc=one.Set()

```



## Common Lisp

Since Common Lisp uses ''generic functions'' for dispatch, creating a class is not necessary. If the superclasses of the singleton are not important, the simplest thing to do is to use a particular symbol; methods use ''eql specializers'' to be applicable to only that object.

For a simple example, the following program constructs English sentences without worrying about extra space occurring at points where no text (<code>the-empty-phrase</code>, our singleton) is inserted.

```lisp
(defgeneric concat (a b)
  (:documentation "Concatenate two phrases."))

(defclass nonempty-phrase ()
  ((text :initarg :text :reader text)))

(defmethod concat ((a nonempty-phrase) (b nonempty-phrase))
  (make-instance 'nonempty-phrase :text (concatenate 'string (text a) " " (text b))))

(defmethod concat ((a (eql 'the-empty-phrase)) b)
  b)

(defmethod concat (a (b (eql 'the-empty-phrase)))
  a)

(defun example ()
  (let ((before (make-instance 'nonempty-phrase :text "Jack"))
        (mid (make-instance 'nonempty-phrase :text "went"))
        (after (make-instance 'nonempty-phrase :text "to fetch a pail of water")))
    (dolist (p (list 'the-empty-phrase
                     (make-instance 'nonempty-phrase :text "and Jill")))
      (dolist (q (list 'the-empty-phrase
                       (make-instance 'nonempty-phrase :text "up the hill")))
        (write-line (text (reduce #'concat (list before p mid q after))))))))
```

Thread safety is irrelevant since the singleton is created at load time, not first access.


## D


```d
module singleton ;
import std.stdio ;
import std.thread ;
import std.random ;
import std.c.time ;

class Dealer {
  private static Dealer me ;
  static Dealer Instance() {
    writefln("   Calling Dealer... ") ;
    if(me is null) // Double Checked Lock
      synchronized  // this part of code can only be executed by one thread a time
        if(me is null)
          me = new Dealer ;
    return me ;
  }
  private static string[] str = ["(1)Enjoy", "(2)Rosetta", "(3)Code"] ;
  private int state ;
  private this() {
    for(int i = 0 ; i < 3 ; i++) {
      writefln("...calling Dealer... ") ;
      msleep(rand() & 2047) ;
    }
    writefln(">>Dealer is called to come in!") ;
    state = str.length - 1 ;
  }
  Dealer nextState() {
    synchronized(this) // accessed to Object _this_ is locked ... is it necessary ???
      state = (state + 1) % str.length ;
    return this ;
  }
  string toString() { return str[state] ; }
}

class Coder : Thread {
  private string name_ ;
  Coder hasName(string name) {  name_ = name ; return this ; }
  override int run() {
    msleep(rand() & 1023) ;
    writefln(">>%s come in.", name_) ;
    Dealer single = Dealer.Instance ;
    msleep(rand() & 1023) ;
    for(int i = 0 ; i < 3 ; i++) {
      writefln("%9s got %-s", name_, single.nextState) ;
      msleep(rand() & 1023) ;
    }
    return 0 ;
  }
}

void main() {
  Coder x = new Coder ;
  Coder y = new Coder ;
  Coder z = new Coder ;

  x.hasName("Peter").start() ;
  y.hasName("Paul").start() ;
  z.hasName("Mary").start() ;

  x.wait ;  y.wait ;  z.wait ;
}
```

```txt
>>Mary come in.
   Calling Dealer...
...calling Dealer...
>>Peter come in.
   Calling Dealer...
>>Paul come in.
   Calling Dealer...
...calling Dealer...
...calling Dealer...
>>Dealer is called to come in!
     Mary got (1)Enjoy
    Peter got (2)Rosetta
     Mary got (3)Code
     Paul got (1)Enjoy
    Peter got (2)Rosetta
     Paul got (3)Code
     Paul got (1)Enjoy
     Mary got (2)Rosetta
    Peter got (3)Code
```


=={{header|Delphi}} and {{header|Pascal}}==
Detailed explanation [http://www.yanniel.info/2010/10/singleton-pattern-delphi.html here]. (Delphi started out as an object-oriented version of Pascal.)

```Delphi
unit Singleton;

interface

type
  TSingleton = class
  private
    //Private fields and methods here...

     class var _instance: TSingleton;
  protected
    //Other protected methods here...
  public
    //Global point of access to the unique instance
    class function Create: TSingleton;

    destructor Destroy; override;

    //Other public methods and properties here...
  end;

implementation

{ TSingleton }

class function TSingleton.Create: TSingleton;
begin
  if (_instance = nil) then
    _instance:= inherited Create as Self;

  result:= _instance;
end;

destructor TSingleton.Destroy;
begin
  _instance:= nil;
  inherited;
end;

end.
```



## E

Since E uses closure-style objects rather than classes, a singleton is simply an object which is defined at the top level of the program, not inside any method. There are no thread-safety issues since the singleton, like every other object, belongs to some particular [http://www.erights.org/elib/concurrency/vat.html vat] (but can be remotely invoked from other vats).

```e
def aSingleton {
  # ...
}
```



## Eiffel

===Non-Thread Safe===
Taken from [http://www.jot.fm/issues/issue_2004_04/article5/ this dated site]

'''Implementation:'''

```Eiffel
class
	SINGLETON
create {SINGLETON_ACCESS}
	default_create
feature
	-- singleton features go here
end
```


```Eiffel
frozen class
	SINGLETON_ACCESS
feature
	singleton: SINGLETON
		once ("PROCESS")
			create Result
		ensure
			Result /= Void
		end
end
```

'''Usage:'''

```Eiffel
s: SINGLETON -- declaration somewhere

s := (create{SINGLETON_ACCESS}).singleton -- in some routine
```


## Elena

Stateless singleton

```elena

singleton Singleton
{
    // ...
}

```

Normal singleton

```elena
class Singleton
{
    object theField;

    // ...
}

static singleton = new Singleton();
```



## Erlang

Erlang is not object-oriented, so there is no such thing as a singleton class. The singleton is something of an anti-pattern in Erlang, so if you are tempted to do this, there is probably a better architecture. If you do want something akin to a singleton, you start and register a process that maintains its state in a message loop and provides its state to anyone that wants it or needs to change it. Since this is done with message passing, it's safe for concurrent use.

```Erlang
-module(singleton).

-export([get/0, set/1, start/0]).

-export([loop/1]).

% spec singleton:get() -> {ok, Value::any()} | not_set
get() ->
     ?MODULE ! {get, self()},
     receive
	{ok, not_set} -> not_set;
        Answer -> Answer
     end.

% spec singleton:set(Value::any()) -> ok
set(Value) ->
    ?MODULE ! {set, self(), Value},
    receive
        ok -> ok
    end.

start() ->
    register(?MODULE, spawn(?MODULE, loop, [not_set])).

loop(Value) ->
    receive
        {get, From} ->
             From ! {ok, Value},
             loop(Value);
        {set, From, NewValue} ->
             From ! ok,
             loop(NewValue)
        end.
```


Here is an example of how to use it (from the shell). It assumes singleton:start/0 was already called from the supervisor tree (as would be typical if you were using something like this).


```Erlang>1
 singleton:get().
not_set
2> singleton:set(apple).
ok
3> singleton:get().
{ok,apple}
4> singleton:set("Pear").
ok
5> singleton:get().
{ok,"Pear"}
6> singleton:set(42).
ok
7> singleton:get().
{ok,42}
```



## Factor


```factor
USING: classes.singleton kernel io prettyprint ;
IN: singleton-demo

SINGLETON: bar
GENERIC: foo ( obj -- )
M: bar foo drop "Hello!" print ;
```

    ( scratchpad ) bar foo
    Hello!


## Forth

Works with any ANS Forth

Needs the FMS-SI (single inheritance) library code located here:
http://soton.mpeforth.com/flag/fms/index.html

```forth
include FMS-SI.f

\ A singleton is created by using normal Forth data
\ allocation words such as value or variable as instance variables.
\ Any number of instances of a singleton class may be
\ instantiated but messages will all operate on the same shared data
\ so it is the same as if only one object has been created.
\ The data name space will remain private to the class.

:class singleton
  0 value a
  0 value b
  :m printa a . ;m
  :m printb b . ;m
  :m add-a ( n -- ) a + to a ;m
  :m add-b ( n -- ) b + to b ;m
;class

singleton s1
singleton s2
singleton s3

4 s1 add-a
9 s2 add-b
s3 printa \ => 4
s3 printb \ => 9
s1 printb \ => 9
s2 printa \ => 4

```



## Go

'''sync.Once'''

From the Go standard library, sync.Once provides a way to ensure that some "step," effectively an initialization step, is performed no more than once even if it might be attempted from multiple concurrent goroutines.  This capability might be considered similar to some mechanism ensuring that singleton constructor code is only run once.

```go
package main

import (
    "log"
    "math/rand"
    "sync"
    "time"
)

var (
    instance string
    once     sync.Once // initialize instance with once.Do
)

func claim(color string, w *sync.WaitGroup) {
    time.Sleep(time.Duration(rand.Intn(1e8))) // hesitate up to .1 sec
    log.Println("trying to claim", color)
    once.Do(func() { instance = color })
    log.Printf("tried %s. instance: %s", color, instance)
    w.Done()
}

func main() {
    rand.Seed(time.Now().Unix())
    var w sync.WaitGroup
    w.Add(2)
    go claim("red", &w) // these two attempts run concurrently
    go claim("blue", &w)
    w.Wait()
    log.Println("after trying both, instance =", instance)
}
```

```txt

2016/07/01 20:36:02 trying to claim red
2016/07/01 20:36:02 tried red. instance: red
2016/07/01 20:36:02 trying to claim blue
2016/07/01 20:36:02 tried blue. instance: red
2016/07/01 20:36:02 after trying both, instance = red

```


'''Packages as singletons'''

Go packages are singletons, in a way.  Go does not use the word "class," and while Go structs might seem most like classes of other languages, Go packages are also like classes in that they represent an organization of declarations, including data and functions.  All declarations in a package form a single ''package block.''  This block is delimited syntactically, has an associated identifier, and its members are accessed by this package identifier.  This is much like classes in other languages.

Because packages cannot be imported multiple times, data declared at package level will only ever have a single instance, and the package as a whole serves as a singleton.

```go
package singlep

// package level data declarations serve as singleton instance variables
var X, Y int

// package level initialization can serve as constructor code
func init() {
    X, Y = 2, 3
}

// package level functions serve as methods for a package-as-a-singleton
func F() int {
    return Y - X
}
```

Example program using the package:

```go
package main

import (
    "fmt"
    "singlep"
)

func main() {
    // dot selector syntax references package variables and functions
    fmt.Println(singlep.X, singlep.Y)
    fmt.Println(singlep.F())
}
```

```txt

2 3
1

```


'''Package data initialization with sync.Once'''

This example combines the two previous concepts and also shows some additional concepts.  It has packages imported with a "diamond" dependency.  While both <code>red</code> and <code>blue</code> import <code>single</code>, only a single variable <code>color</code> will exist in memory.  The <code>init()</code> mechanism shown above actually runs before <code>main()</code>.  In contrast, the <code>sync.Once</code> mechanism can serve as constructor code after <code>main()</code> begins.


```go
package single

import (
    "log"
    "sync"
)

var (
    color string
    once  sync.Once
)

func Color() string {
    if color == "" {
        panic("color not initialized")
    }
    return color
}

func SetColor(c string) {
    log.Println("color initialization")
    once.Do(func() { color = c })
    log.Println("color initialized to", color)
}
```


```go
package red

import (
    "log"

    "single"
)

func SetColor() {
    log.Println("trying to set red")
    single.SetColor("red")
}
```


```go
package blue

import (
    "log"

    "single"
)

func SetColor() {
    log.Println("trying to set blue")
    single.SetColor("blue")
}
```


```go
package main

import (
    "log"
    "math/rand"
    "time"

    "blue"
    "red"
    "single"
)

func main() {
    rand.Seed(time.Now().Unix())
    switch rand.Intn(3) {
    case 1:
        red.SetColor()
        blue.SetColor()
    case 2:
        blue.SetColor()
        red.SetColor()
    }
    log.Println(single.Color())
}
```

```txt

2016/07/01 20:52:18 trying to set red
2016/07/01 20:52:18 color initialization
2016/07/01 20:52:18 color initialized to red
2016/07/01 20:52:18 trying to set blue
2016/07/01 20:52:18 color initialization
2016/07/01 20:52:18 color initialized to red
2016/07/01 20:52:18 red

```



## Groovy


```groovy
@Singleton
class SingletonClass {

    def invokeMe() {
        println 'invoking method of a singleton class'
    }

    static void main(def args) {
        SingletonClass.instance.invokeMe()
    }
}
```

```txt
invoking method of a singleton class
```


==Icon and {{header|Unicon}}==
Icon is not object oriented, but Unicon supports O-O programming.

```unicon
class Singleton
   method print()
       write("Hi there.")
   end
   initially
       write("In constructor!")
       Singleton := create |self
end

procedure main()
   Singleton().print()
   Singleton().print()
end
```


This Unicon example uses a number of Icon features.
* The class descriptor Singleton is a first-class global object.
* The create keyword yields a co-routine which can be activated like a function call.
* The monadic operator | repeatedly yields the iteration of it's argument - in this case, it yields the object created (self).
* The initializer of each object actually replaces the global object Singleton with a coroutine that returns ... the first object created. Therefore there is no further access to the true Singleton constructor; future attempts to create the object instead just activates the co-routine.

NOTE: this could be subverted by capturing a reference to Singleton prior to the first object construction.


## Io

Io does not have globals.  But it is easy to make singleton objects:

```io
Singleton := Object clone
Singleton clone = Singleton
```



## J

In J, all classes are singletons though their objects are not.  (Class names may be used in any context where object references may be used, and object references can be used in almost every context where a class name may be used.)

Singletons should not have a constructor so any attempt to construct an instance of a singleton (dyadic <code>conew</code>) would fail.  Other than that, singletons are defined like any other class in J.


## Java

===Thread-safe===
[[wp:Double-checked locking]]; only use with Java 1.5+

```java
class Singleton
{
    private static Singleton myInstance;
    public static Singleton getInstance()
    {
        if (myInstance == null)
        {
            synchronized(Singleton.class)
            {
                if (myInstance == null)
                {
                    myInstance = new Singleton();
                }
            }
        }

        return myInstance;
    }

    protected Singleton()
    {
        // Constructor code goes here.
    }

    // Any other methods
}
```


===Thread-Safe Lazy-Loaded===
This is the [[wp:Initialization-on-demand holder idiom]].

```java
public class Singleton {
    private Singleton() {
        // Constructor code goes here.
    }

    private static class LazyHolder {
        private static final Singleton INSTANCE = new Singleton();
    }

    public static Singleton getInstance() {
        return LazyHolder.INSTANCE;
    }
}
```


===Non-Thread-Safe===

```java
class Singleton
{
    private static Singleton myInstance;
    public static Singleton getInstance()
    {
        if (myInstance == null)
        {
            myInstance = new Singleton();
        }

        return myInstance;
    }

    protected Singleton()
    {
        // Constructor code goes here.
    }

    // Any other methods
}
```



## JavaScript


```JavaScript
function Singleton() {
	if(Singleton._instance) return Singleton._instance;
	this.set("");
	Singleton._instance = this;
}

Singleton.prototype.set = function(msg) { this.msg = msg; }
Singleton.prototype.append = function(msg) { this.msg += msg; }
Singleton.prototype.get = function() { return this.msg; }


var a = new Singleton();
var b = new Singleton();
var c = new Singleton();

a.set("Hello");
b.append(" World");
c.append("!!!");

document.write( (new Singleton()).get() );
```



## Julia

Julia allows singletons as type declarations without further specifiers. There can be only one instance of such a type, and if more than one variable is bound to such a type they are actually all bound to the same instance in memory:

```julia

struct IAmaSingleton end

x = IAmaSingleton()
y = IAmaSingleton()

println("x == y is $(x == y) and x === y is $(x === y).")

```



## Kotlin

Kotlin has built-in support for singletons via object declarations. To refer to the singleton, we simply use its name which can be any valid identifier other than a keyword:

```scala
// version 1.1.2

object Singleton {
    fun speak() = println("I am a singleton")
}

fun main(args: Array<String>) {
    Singleton.speak()
}
```


```txt

I am a singleton

```



## Lasso


Lasso supports singletons on two levels.


### Server wide singleton



```Lasso
// Define the thread if it doesn't exist
// New definition supersede any current threads.

not ::serverwide_singleton->istype
? define serverwide_singleton => thread {
    data public switch = 'x'
}

local(
    a = serverwide_singleton,
    b = serverwide_singleton,
)

#a->switch = 'a'
#b->switch = 'b'

#a->switch // b
```



### Thread level singleton



```Lasso
// Define thread level singleton

define singleton => type {
    data public switch = 'x'
    public oncreate => var(.type)->isa(.type) ? var(.type) | var(.type) := self
}

local(
    a = singleton,
    b = singleton,
)

#a->switch = 'a'
#b->switch = 'b'

#a->switch // b
```



## Lingo

In Lingo a Singleton class can be implemented like this:

```lingo
-- parent script "SingletonDemo"

property _instance
property _someProperty

----------------------------------------
-- @constructor
----------------------------------------
on new (me)
  if not voidP(me.script._instance) then return me.script._instance
  me.script._instance = me
  me._someProperty = 0
  return me
end

----------------------------------------
-- sample method
----------------------------------------
on someMethod (me, x)
  me._someProperty = me._someProperty + x
  return me._someProperty
end
```



## Logtalk

Logtalk supports both classes and prototypes. A prototype is a much simpler solution for defining a singleton object than defining a class with only an instance.

```logtalk
:- object(singleton).

    :- public(value/1).
    value(Value) :-
        state(Value).

    :- public(set_value/1).
    set_value(Value) :-
        retract(state(_)),
        assertz(state(Value)).

    :- private(state/1).
    :- dynamic(state/1).
    state(0).

:- end_object.
```

A simple usage example after compiling and loading the code above:

```logtalk
| ?- singleton::value(Value).
Value = 0
yes

| ?- singleton::(set_value(1), value(Value)).
Value = 1
yes
```



## NetRexx

Uses a static field to avoid synchronization problems and the ''flawed'' &quot;double-checked locking&quot; idiom in JVMs.  See [http://www.ibm.com/developerworks/java/library/j-dcl/index.html www.ibm.com/developerworks/java/library/j-dcl/index.html] for a detailed explanation.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

import java.util.random

class RCSingleton public

  method main(args = String[]) public static
    RCSingleton.Testcase.main(args)
    return

  -- ---------------------------------------------------------------------------
  class RCSingleton.Instance public

    properties private static
      _instance = Instance()

    properties private
      _refCount = int
      _random   = Random

    method Instance() private
      this._refCount = 0
      this._random = Random()
      return

    method getInstance public static returns RCSingleton.Instance
      return _instance

  method getRandom public returns Random
    return _random

    method addRef public protect
      _refCount = _refCount + 1
      return

    method release public protect
      if _refCount > 0 then
        _refCount = _refCount - 1
      return

    method getRefCount public protect returns int
      return _refCount

  -- ---------------------------------------------------------------------------
  class RCSingleton.Testcase public implements Runnable

  properties private
    _instance = RCSingleton.Instance

  method run public
    say threadInfo'|-'
    thud = Thread.currentThread
    _instance = RCSingleton.Instance.getInstance
    thud.yield
    _instance.addRef
    say threadInfo'|'_instance.getRefCount
    thud.yield
    do
      thud.sleep(_instance.getRandom.nextInt(1000))
    catch ex = InterruptedException
      ex.printStackTrace
      end
    _instance.release
    say threadInfo'|'_instance.getRefCount
    return

  method main(args = String[]) public static
    threads = [ Thread -
      Thread(Testcase()), Thread(Testcase()), Thread(Testcase()), -
      Thread(Testcase()), Thread(Testcase()), Thread(Testcase()) ]
    say threadInfo'|-'
    mn = Testcase()
    mn._instance = RCSingleton.Instance.getInstance
    say mn.threadInfo'|'mn._instance.getRefCount
    mn._instance.addRef
    say mn.threadInfo'|'mn._instance.getRefCount
    do
      loop tr over threads
        (Thread tr).start
        end tr
      Thread.sleep(400)
    catch ex = InterruptedException
      ex.printStackTrace
      end
    mn._instance.release
    say mn.threadInfo'|'mn._instance.getRefCount
    return

  method threadInfo public static returns String
    trd = Thread.currentThread
    tid = trd.getId
    hc  = trd.hashCode
    info = Rexx(trd.getName).left(16, '_')':' -
        || Rexx(Long.toString(tid)).right(10, 0)':' -
        || '@'Rexx(Integer.toHexString(hc)).right(8, 0)
    return info


```


<pre style="height:30ex;overflow:scroll">
main____________:0000000001:@035a8767|-
main____________:0000000001:@035a8767|0
main____________:0000000001:@035a8767|1
Thread-1________:0000000010:@22998b08|-
Thread-1________:0000000010:@22998b08|2
Thread-2________:0000000011:@7a6d084b|-
Thread-2________:0000000011:@7a6d084b|3
Thread-3________:0000000012:@2352544e|-
Thread-4________:0000000013:@457471e0|-
Thread-5________:0000000014:@7ecec0c5|-
Thread-6________:0000000015:@3dac2f9c|-
Thread-3________:0000000012:@2352544e|4
Thread-4________:0000000013:@457471e0|5
Thread-5________:0000000014:@7ecec0c5|6
Thread-6________:0000000015:@3dac2f9c|7
Thread-5________:0000000014:@7ecec0c5|6
main____________:0000000001:@035a8767|5
Thread-3________:0000000012:@2352544e|4
Thread-1________:0000000010:@22998b08|3
Thread-6________:0000000015:@3dac2f9c|2
Thread-2________:0000000011:@7a6d084b|1
Thread-4________:0000000013:@457471e0|0

```



## Nim

In the file <code>singleton.nim</code> we don't export the type, so new objects can't be created:

```nim
type Singleton = object # Singleton* would export
  foo*: int

var single* = Singleton(foo: 0)
```

Then in another file we can use the singleton object:

```nim
import singleton

single.foo = 12
echo single.foo
```



## Objeck


```objeck
class Singleton {
  @singleton : static : Singleton;

  New : private () {
  }

  function : GetInstance() ~ Singleton {
    if(@singleton <> Nil) {
      @singleton := Singleton->New();
    };

    return @singleton;
  }

  method : public : DoStuff() ~ Nil {
    ...
  }
}
```


=={{header|Objective-C}}==
===Non-Thread-Safe===
(Using Cocoa/OpenStep's NSObject as a base class)

```objc
// SomeSingleton.h
@interface SomeSingleton : NSObject
{
  // any instance variables
}

+ (SomeSingleton *)sharedInstance;

@end
```



```objc
// SomeSingleton.m
@implementation SomeSingleton

+ (SomeSingleton *) sharedInstance
{
   static SomeSingleton *sharedInstance = nil;
   if (!sharedInstance) {
      sharedInstance = [[SomeSingleton alloc] init];
   }
   return sharedInstance;
}

- (id)copyWithZone:(NSZone *)zone
{
    return self;
}

- (id)retain
{
    return self;
}

- (unsigned)retainCount
{
    return UINT_MAX;
}

- (oneway void)release
{
    // prevent release
}

- (id)autorelease
{
    return self;
}

@end
```


===Thread-Safe===
Same as above except:

```objc
+ (SomeSingleton *) sharedInstance
{
   static SomeSingleton *sharedInstance = nil;
   @synchronized(self) {
      if (!sharedInstance) {
         sharedInstance = [[SomeSingleton alloc] init];
      }
   }
   return sharedInstance;
}
```



### With GCD

Same as above except:

```objc
+ (SomeSingleton *) sharedInstance
{
   static SomeSingleton *sharedInstance = nil;
   static dispatch_once_t onceToken;
   dispatch_once(&onceToken, ^{
      sharedInstance = [[SomeSingleton alloc] init];
   });
   return sharedInstance;
}
```



### With class methods

It's possible to accomplish the same thing with class methods of some class, rather than instance methods on the instance of a singleton class. Data that needs to be kept as "instance variables" would instead be kept as <code>static</code> (file-local) global variables. "Initialization" of the singleton object would be done in the <code>+initialize</code> method, which is guaranteed to be called at most once for every class, the first time the class is messaged. This way, the singleton is also "lazy loaded" as needed.

In other words, here the class object serves as the singleton object. The "singleton class" is the metaclass of the class. The downside of this approach is that the "singleton class" (the metaclass of the class) cannot be made to explicitly inherit from a class of the user's choice, or implement a protocol of the user's choice. Also, there is no way to prevent subclasses of the class from being made, thus effectively creating "multiple instances" of the singleton class. Also, one cannot declare properties on the singleton (the class object).



## Oforth


Oforth does not have global variables, class attributes or some kind of shared mutable memory that can be updated by different tasks.

In Oforth, singleton is an anti-pattern because it needs synchronisation in order to be safe between parallel tasks.

If the goal is to keep and update a value in a safe way, a channel can be used.

For instance, this Sequence class creates instances that increment an integer and send it. If a task tries to get the next value before it is incremented, it will wait until the channel is no more empty and holds the new value. This won't work if the value is a mutable value (you will get an exception if you try to send a mutable object into channel). A mutable object can't be shared between tasks. Here we send a new integer each time.


```Oforth
Object Class new: Sequence(channel)
Sequence method: initialize(initialValue)
   Channel newSize(1) := channel
   @channel send(initialValue) drop ;

Sequence method: nextValue  @channel receive dup 1 + @channel send drop ;
```


Usage :

```Oforth
import: parallel

: testSequence
| s i |
   Sequence new(0) ->s
   100 loop: i [ #[ s nextValue println ] & ] ;
```



## ooRexx


```ooRexx

a = .singleton~new
b = .singleton~new

a~foo = "Rick"
if a~foo \== b~foo then say "A and B are not the same object"

::class singleton
-- initialization method for the class
::method init class
  expose singleton
  -- mark this as unallocated.  We could also just allocate
  -- the singleton now, but better practice is probably wait
  -- until it is requested
  singleton = .nil

-- override the new method.  Since this is a guarded
-- method by default, this is thread safe
::method new class
  expose singleton
  -- first request?  Do the real creation now
  if singleton == .nil then do
     -- forward to the super class.  We use this form of
     -- FORWARD rather than explicit call ~new:super because
     -- this takes care of any arguments passed to NEW as well.
     forward class(super) continue
     singleton = result
  end
  return singleton

-- an attribute that can be used to demonstrate this really is
a singleton.
::attribute foo

```



## OxygenBasic

The singleton contains static members only. It may be instantiated any number of times, but the members will all be shared.

```oxygenbasic

class singleton
static sys     a,b,c
static string  s,t,u
static double  x,y,z
end class

'TEST
'====

singleton A
singleton B
A.c=3
print B.c      'result 3
print sizeof B 'result 0

```



## Oz

Singleton is not a common pattern in Oz programs. It can be implemented by limiting the scope of the class definition such that only the <code>GetInstance</code> function has access to it.

```oz
declare
   local
      class Singleton
	 meth init
	    skip
	 end
      end
      L = {NewLock}
      Instance
   in
      fun {GetInstance}
	 lock L then
	    if {IsFree Instance} then
	       Instance = {New Singleton init}
	    end
	    Instance
	 end
      end
   end
```

This will work as long as all functors are linked with <code>import</code> statements. If you use multiple calls to <code>Module.link</code> instead, you will get multiple instances of the "Singleton".


## Perl


```Perl
package Singleton;
use strict;
use warnings;

my $Instance;

sub new {
    my $class = shift;
    $Instance ||= bless {}, $class; # initialised once only
}

sub name {
    my $self = shift;
    $self->{name};
}

sub set_name {
    my ($self, $name) = @_;
    $self->{name} = $name;
}

package main;

my $s1 = Singleton->new;
$s1->set_name('Bob');
printf "name: %s, ref: %s\n", $s1->name, $s1;

my $s2 = Singleton->new;
printf "name: %s, ref: %s\n", $s2->name, $s2;
```



## Perl 6


```perl6
class Singleton {
    # We create a lexical variable in the class block that holds our single instance.
    my Singleton $instance = Singleton.bless; # You can add initialization arguments here.
    method new {!!!} # Singleton.new dies.
    method instance { $instance; }
}
```



## PHP


```PHP
class Singleton {
  protected static $instance = null;
  public $test_var;
  private function __construct(){
    //Any constructor code
  }
  public static function getInstance(){
    if (is_null(self::$instance)){
      self::$instance = new self();
    }
    return self::$instance;
  }
}

$foo = Singleton::getInstance();
$foo->test_var = 'One';

$bar = Singleton::getInstance();
echo $bar->test_var; //Prints 'One'

$fail = new Singleton(); //Fatal error
```



## PicoLisp

As there is no physical difference between classes and objects, we can use the
class symbol itself.

```PicoLisp
(class +Singleton)

(dm message1> ()
   (prinl "This is method 1 on " This) )

(dm message2> ()
   (prinl "This is method 2 on " This) )
```

```txt
: (message1> '+Singleton)
This is method 1 on +Singleton
-> +Singleton

: (message2> '+Singleton)
This is method 2 on +Singleton
-> +Singleton
```



## Python


### per Borg Design

In Python we use the [http://code.activestate.com/recipes/66531/ Borg pattern] to share state between instances rather than concentrate on identity.

Every instance of the Borg class will share the same state:

```python>>>
 class Borg(object):
	__state = {}
	def __init__(self):
		self.__dict__ = self.__state
	# Any other class names/methods


>>> b1 = Borg()
>>> b2 = Borg()
>>> b1 is b2
False
>>> b1.datum = range(5)
>>> b1.datum
[0, 1, 2, 3, 4]
>>> b2.datum
[0, 1, 2, 3, 4]
>>> b1.datum is b2.datum
True
>>> # For any datum!
```



### per MetaClass/AbstractBaseClass


An approximation of the singleton can be made using only class attributes to store data instead of the instance attributes, providing at least one abstract instance method (class can not be instantiated then) and making the rest of the methods being class methods. E.g.


```python

import abc

class Singleton(object):
    """
    Singleton class implementation
    """
    __metaclass__ = abc.ABCMeta

    state = 1 #class attribute to be used as the singleton's attribute

    @abc.abstractmethod
    def __init__(self):
        pass #this prevents instantiation!

    @classmethod
    def printSelf(cls):
        print cls.state #prints out the value of the singleton's state

#demonstration
if __name__ == "__main__":
    try:
        a = Singleton() #instantiation will fail!
    except TypeError as err:
        print err
    Singleton.printSelf()
    print Singleton.state
    Singleton.state = 2
    Singleton.printSelf()
    print Singleton.state

```

When executed this code should print out the following:



Can't instantiate abstract class Singleton with abstract methods __init__

1

1

2

2



So, instantiation is not possible. Only a single object is available, and it behaves as a singleton.




### per MetaClass




```python

class Singleton(type):
    _instances = {}
    def __call__(cls, *args, **kwargs):
        if cls not in cls._instances:
            cls._instances[cls] = super(Singleton, cls).__call__(*args, **kwargs)
        return cls._instances[cls]

class Logger(object):
    __metaclass__ = Singleton

```


or in Python3



```python

class Logger(metaclass=Singleton):
    pass

```



## PureBasic


### Native version

Thread safe version.

```PureBasic
Global SingletonSemaphore=CreateSemaphore(1)

Interface OO_Interface    ; Interface for any value of this type
  Get.i()
  Set(Value.i)
  Destroy()
EndInterface

Structure OO_Structure ; The *VTable structure
  Get.i
  Set.i
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

Procedure CreateSingleton()
  If TrySemaphore(SingletonSemaphore)
    *p.OO_Var = AllocateMemory(SizeOf(OO_Var))
    If *p
      *p\VirtualTable = ?VTable
    EndIf
  EndIf
  ProcedureReturn *p
EndProcedure

Procedure OO_Destroy(*Self.OO_Var)
  FreeMemory(*Self)
  SignalSemaphore(SingletonSemaphore)
EndProcedure

DataSection
  VTable:
  Data.i @OO_Get()
  Data.i @OO_Set()
  Data.i @OO_Destroy()
EndDataSection
```


### Simple OOP extension

Using the open-source precompiler [http://www.development-lounge.de/viewtopic.php?t=5915 SimpleOOP].

```PureBasic
Singleton Class Demo
  BeginPrivate
    Name$
    X.i
  EndPrivate

  Public Method Init(Name$)
    This\Name$ = Name$
  EndMethod

  Public Method GetX()
    MethodReturn This\X
  EndMethod

  Public Method SetX(n)
    This\X = n
  EndMethod

  Public Method Hello()
    MessageRequester("Hello!", "I'm "+This\Name$)
  EndMethod

EndClass
```



## Racket


Singletons are not very useful in Racket, because functions that use module state are more straightforward. However, classes are first class values, and therefore they follow the same rules as all other bindings.  For example, a class can be made and instantiated but not provided to client files:

```racket

#lang racket
(provide instance)
(define singleton%
  (class object%
    (super-new)))
(define instance (new singleton%))

```


Or better, not name the class at all:

```racket

#lang racket
(provide instance)
(define instance
  (new (class object%
         (define/public (foo) 123)
         (super-new))))

```



## Ruby


```ruby
require 'singleton'
class MySingleton
   include Singleton
   # constructor and/or methods go here
end

a = MySingleton.instance # instance is only created the first time it is requested
b = MySingleton.instance
puts a.equal?(b) # outputs "true"
```



## Scala

The '''object''' construct in Scala is a singleton.

```scala
object Singleton {
  // any code here gets executed as if in a constructor
}
```



## Sidef


```ruby
class Singleton(name) {
    static instance;

    method new(name) {
        instance := Singleton.bless(Hash(:name => name));
    }
    method new {
        Singleton.new(nil);
    }
}

var s1 = Singleton('foo');
say s1.name;                #=> 'foo'
say s1.object_id;           #=> '30424504'

var s2 = Singleton();
say s2.name;                #=> 'foo'
say s2.object_id;           #=> '30424504'

s2.name = 'bar';            # change name in s2
say s1.name;                #=> 'bar'
```



## Slate

Clones of Oddball themselves may not be cloned.
Methods and slots may still be defined on them:

```slate
define: #Singleton &builder: [Oddball clone]
```



## Smalltalk


```smalltalk

SomeClass class>>sharedInstance

       SharedInstance ifNil: [SharedInstance := self basicNew initialize].
       ^ SharedInstance

```



## Swift


```swift


class SingletonClass {

static let sharedInstance = SingletonClass()

    ///Override the init method and make it private
    private override init(){
    // User can do additional manipulations here.
    }
}
// Usage
let sharedObject = SingletonClass.sharedInstance

```



## Tcl

ref http://wiki.tcl.tk/21595

```tcl
package require TclOO

# This is a metaclass, a class that defines the behavior of other classes
oo::class create singleton {
   superclass oo::class
   variable object
   unexport create ;# Doesn't make sense to have named singletons
   method new args {
      if {![info exists object]} {
         set object [next {*}$args]
      }
      return $object
   }
}

singleton create example {
   method counter {} {
      my variable count
      return [incr count]
   }
}
```

Demonstrating in an interactive shell:

```tcl
% set a [example new]
::oo::Obj20
% set b [example new]  ;# note how this returns the same object name
::oo::Obj20
% expr {$a == $b}
1
% $a counter
1
% $b counter
2
% $a counter
3
% $b counter
4
```



## Tern

Tern has built-in support for singletons via module declarations.

```tern
module Singleton {
   speak() {
      println("I am a singleton");
   }
}

Singleton.speak();
```


```txt

I am a singleton

```



## Vala


```Vala
public class Singleton : Object {
    static Singleton? instance;

    // Private constructor
    Singleton() {

    }

    // Public constructor
    public static Singleton get_instance() {
        if (instance == null) {
            instance = new Singleton();
        }
        return instance;
    }
}

void main() {
    Singleton a = Singleton.get_instance();
    Singleton b = Singleton.get_instance();
    if (a == b) {
        print("Equal.\n");
    }
}
```



## zkl

A class declared static only has one instance, ever.
However, a class with the same name & structure could be created in another scope.

```zkl
class [static] Borg{ var v }
b1 := Borg; b2 := Borg();
b1 == b2 //--> True
b1.v=123; b2.v.println(); //--> 123
```

