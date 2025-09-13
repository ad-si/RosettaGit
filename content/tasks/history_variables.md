+++
title = "History variables"
description = ""
date = 2019-10-18T10:46:01Z
aliases = []
[extra]
id = 9987
[taxonomies]
categories = ["task"]
tags = []
+++

''Storing the history of objects in a program is a common task.
Maintaining the history of an object in a program has traditionally required programmers either to write specific code for handling the historical data, or to use a library which supports history logging.''

''History variables are variables in a programming language which store not only their current value, but also the values they have contained in the past. Some existing languages do provide support for history variables. However these languages typically have many limits and restrictions on use of history variables.
''

[http://www.bod.com/index.php?id=3435&objk_id=148050 "History Variables:
The Semantics, Formal Correctness, and Implementation of History Variables
in an Imperative Programming Language" by Mallon and Takaoka]

Concept also discussed on [http://lambda-the-ultimate.org/node/3111 LtU] and [http://www.patents.com/us-7111283.html Patents.com].

## Task

Demonstrate History variable support:
* enable history variable support (if needed)
* define a history variable
* assign three values
* non-destructively display the history
* recall the three values.


For extra points, if the language of choice does not support history variables,
demonstrate how this might be implemented.





## Ada


Ada does not natively support history variables -- we have to implement them.

Furthermore, Ada is a strongly typed language -- that means, we would need
to write a history variable type for every basic item type.
Instead, we write a single generic package "History_Variables" that works for any item type.


===Generic Package "History_Variables"===

Specification:


```Ada
private with Ada.Containers.Indefinite_Vectors;
generic
   type Item_Type (<>) is private;
package History_Variables is

   type Variable is tagged limited private;

   -- set and get current value
   procedure Set(V: in out Variable; Item: Item_Type);
   function Get(V: Variable) return Item_Type;

   -- number of items in history (including the current one)
   function Defined(V: Variable) return Natural;

   -- non-destructively search for old values
   function Peek(V: Variable; Generation: Natural := 1) return Item_Type;
   -- V.Peek(0) returns current value; V.Peek(1) the previous value, etc.
   -- when calling V.Peek(i), i must be in 0 .. V.Defined-1, else Constraint_Error is raised

   -- destructively restore previous value
   procedure Undo(V: in out Variable);
   -- old V.Peek(0) is forgotten, old V.Peek(i) is new V.Peek(i-1), etc.
   -- accordingly, V.Defined decrements by 1
   -- special case: if V.Defined=0 then V.Undo does not change V

private
   package Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Item_Type);

   type Variable is tagged limited record
      History: Vectors.Vector;
   end record;
end History_Variables;
```


The implementation of "History_Variables":


```Ada
package body History_Variables is

   -- set and get
   procedure Set(V: in out Variable; Item: Item_Type) is
   begin
      V.History.Prepend(Item);
   end Set;

   function Get(V: Variable) return Item_Type is
   begin
      return V.History.First_Element;
   end Get;

   -- number of items in history (including the current one)
   function Defined(V: Variable) return Natural is
   begin
      return (1 + V.History.Last_Index) - V.History.First_Index;
   end Defined;

   -- non-destructively search
   function Peek(V: Variable; Generation: Natural := 1) return Item_Type is
      Index: Positive  := V.History.First_Index + Generation;
   begin
      if Index > V.History.Last_Index then
         raise Constraint_Error;
      end if;
      return V.History.Element(Index);
   end Peek;

   procedure  Undo(V: in out Variable) is
   begin
      V.History.Delete_First;
   end Undo;

end History_Variables;
```




### Sample 1: The History of an Integer Variable



```Ada
with Ada.Text_IO, History_Variables;

procedure Test_History is

   package Int_With_Hist is new History_Variables(Integer);

   -- define a history variable
   I: Int_With_Hist.Variable;

   Sum: Integer := 0;

begin

   -- assign three values
   I.Set(3);
   I.Set(I.Get + 4);
   I.Set(9);

   -- non-destructively display the history
   for N in reverse 0 .. I.Defined-1 loop
      Ada.Text_IO.Put(Integer'Image(I.Peek(N)));
   end loop;
   Ada.Text_IO.New_Line;

   -- recall the three values
   while I.Defined > 0 loop
      Sum := Sum + I.Get;
      I.Undo;
   end loop;
   Ada.Text_IO.Put_Line(Integer'Image(Sum));

end Test_History;
```


```txt
 3 7 9
 19
```




### Sample 2: The History of a String



```Ada
with Ada.Text_IO, History_Variables;

procedure Test_History is

   package Str_With_Hist is new History_Variables(String);

   -- define a history variable
   S: Str_With_Hist.Variable;

   Sum: Integer := 0;

begin

   -- assign three values
   S.Set("one");
   S.Set(S.Get & S.Get); --"oneone"
   S.Set("three");

   -- non-destructively display the history
   for N in reverse 0 .. S.Defined-1 loop
      Ada.Text_IO.Put(S.Peek(Generation => N) &" ");
   end loop;
   Ada.Text_IO.New_Line;

   -- recall the three values
   while S.Defined > 0 loop
      Sum := Sum + S.Get'Length;
      S.Undo;
   end loop;
   Ada.Text_IO.Put_Line(Integer'Image(Sum));

end Test_History;
```


```txt
one oneone three
 14
```



## ALGOL W

Algol W does not have history variables as standard, as with other languages here, we can add them using a simple linked list.

As Algol W does not have generic types, a separate history variable type would be required for each type of variable.

```algolw
begin
    % implements integer history variables                                 %
    % similar history types could be defined for other types of variable   %
    record HInteger ( integer iValue; reference(HInteger) iPrev );
    % sets the latest value of hv to n                                     %
    procedure setIhv ( reference(HInteger) value result hv; integer value n ) ; hv := HInteger( n, hv );
    % declare an integer history variable                                  %
    reference(HInteger) hv;
    % initialise the history to a null value                               %
    hv := null;
    % assign three values                                                  %
    setIhv( hv, 1 );
    setIhv( hv, 2 );
    setIhv( hv, 3 );
    % show the history of hv                                               %
    begin
        reference(HInteger) h;
        write( "hv history: " );
        h := hv;
        while h not = null do begin
            writeon( i_w := 3, s_w := 0, iValue(h), " " );
            h := iPrev(h)
        end while_h_ne_null
    end;
    % remove the values from hv, summing them as in the Ada sample         %
    begin
        integer s;
        s := 0;
        while hv not = null do begin
            s  := s + iValue(hv);
            hv := iPrev(hv)
        end while_hv_ne_null ;
        write( "Sum of the historic values: ", s )
    end
end.
```

```txt

hv history:   3   2   1
Sum of the historic values:              6

```



## AspectJ

AspectJ implementation for Java 7.
Type of the history variable (Java class):

```java
public class HistoryVariable
{
    private Object value;

    public HistoryVariable(Object v)
    {
        value = v;
    }

    public void update(Object v)
    {
        value = v;
    }

    public Object undo()
    {
        return value;
    }

    @Override
    public String toString()
    {
        return value.toString();
    }

    public void dispose()
    {
    }
}
```

Aspect (HistoryHandling.aj):

```java
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

public privileged aspect HistoryHandling
{
    before() : execution(HistoryVariable.new(..))
    {
        history.put((HistoryVariable) thisJoinPoint.getTarget(), new LinkedList<>());
    }

    after() : execution(void HistoryVariable.dispose())
    {
        history.remove(thisJoinPoint.getTarget());
    }

    before(Object v) : execution(void HistoryVariable.update(Object)) && args(v)
    {
        final HistoryVariable hv = (HistoryVariable) thisJoinPoint.getThis();
        history.get(hv).add(hv.value);
    }

    after() : execution(Object HistoryVariable.undo())
    {
        final HistoryVariable hv = (HistoryVariable) thisJoinPoint.getThis();
        final Deque<Object> q = history.get(hv);
        if (!q.isEmpty())
            hv.value = q.pollLast();
    }

    String around() : this(HistoryVariable) && execution(String toString())
    {
        final HistoryVariable hv = (HistoryVariable) thisJoinPoint.getThis();
        final Deque<Object> q = history.get(hv);
        if (q == null)
            return "<disposed>";
        else
            return "current: "+ hv.value + ", previous: " + q.toString();
    }

    private Map<HistoryVariable, Deque<Object>> history = new HashMap<>();
}
```

Usage:

```java
public final class Main
{
    public static void main(final String[] args)
    {
        HistoryVariable hv = new HistoryVariable("a");
        hv.update(90);
        hv.update(12.1D);
        System.out.println(hv.toString());
        System.out.println(hv.undo());
        System.out.println(hv.undo());
        System.out.println(hv.undo());
        System.out.println(hv.undo());
        System.out.println(hv.toString());
        hv.dispose();
        System.out.println(hv.toString());
    }
}
```


```txt
current: 12.1, previous: [a, 90]
12.1
90
a
a
current: a, previous: []
<disposed>
```



## AutoHotkey

AutoHotkey records a history of your keypresses, but not your variables. The closest you can come is with a class:
```ahk
HV := new HistoryVariable
HV.var := 1
HV.var := 2
HV.var := "Latest value"
Msgbox % HV.var "`n" HV[2] "`n" HV[3]

class HistoryVariable {
	__Set(aName, aValue) {
		this.Insert(1,aValue)
		Return aValue
	}
	__Get(aName) {
		Return this[1]
	}
}
```



## C#


```c sharp
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace History
{
    class Program
    {
        static void Main(string[] args)
        {
            var h = new HistoryObject();
            h.Value = 5;
            h.Value = "foo";
            h.Value += "bar";

            var history = h.ToArray();

            for (int i = 0; i < history.Length; i++)
            {
                Console.Write("{0}{1}", history[i], ((i >= history.Length - 1) ? "\n" : " <- "));
            }

            h.Undo();
            h.Undo();
            h.Undo();

            Console.WriteLine(h.Value);
        }

        private class HistoryObject : IEnumerable<object>
        {
            public HistoryObject()
            {
                _history = new Stack<object>(); // Initiates the history stack.
            }

            public object Value
            {
                get // Returns the top value from the history if there is one. Otherwise null.
                {
                    if (_history.Count > 0)
                        return _history.Peek();
                    return null;
                }
                set { _history.Push(value); } // Adds the specified value to the history.
            }

            public void Undo()
            {
                if (_history.Count > 0)
                    _history.Pop(); // Removes the current value from the history.
            }

            // History stack that will hold all previous values of the object.
            private readonly Stack<object> _history;

            public IEnumerator<object> GetEnumerator()
            {
                return _history.GetEnumerator();
            }

            IEnumerator IEnumerable.GetEnumerator()
            {
                return GetEnumerator();
            }
        }
    }
}
```


```txt
foobar <- foo <- 5

```



## Clojure

Clojure does not have history variables, but it can be accomplished
via a watcher function that can track changes on a variable.


```clojure

(def a (ref 0))
(def a-history (atom [@a])) ; define a history vector to act as a stack for changes on variable a
(add-watch a :hist (fn [key ref old new] (swap! a-history conj new)))

```


```txt
user=> (dosync (ref-set a 1))
1
user=> (dosync (ref-set a 2))
2
user=> (dosync (ref-set a 3))
3
user=> @a-history
[0 1 2 3]
user=> (dotimes [n 3] (println (peek @a-history)) (swap! a-history pop))
3
2
1
nil
user=> @a-history
[0]

```



## Common Lisp

In Lisp the list is a natural and simple data structure for representing variables with history. By adding some simple macros we can make a small DSL that hides the list and makes it look like a history variable is it's own special thing.


```lisp
(defmacro make-hvar (value)
  `(list ,value))

(defmacro get-hvar (hvar)
  `(car ,hvar))

(defmacro set-hvar (hvar value)
  `(push ,value ,hvar))

;; Make sure that setf macro can be used
(defsetf get-hvar set-hvar)

(defmacro undo-hvar (hvar)
  `(pop ,hvar))

(let ((v (make-hvar 1)))
  (format t "Initial value = ~a~%" (get-hvar v))
  (set-hvar v 2)
  (setf (get-hvar v) 3) ;; Alternative using setf
  (format t "Current value = ~a~%" (get-hvar v))
  (undo-hvar v)
  (undo-hvar v)
  (format t "Restored value = ~a~%" (get-hvar v)))

```

```txt
Initial value = 1
Current value = 3
Restored value = 1

```



## D

D does not have history variables. The following implementation provides a generic HistoryVariable that protects the historical values by defining them as 'const'.


```d
import std.stdio, std.array, std.string, std.datetime, std.traits;

/// A history variable.
struct HistoryVariable(T) {
    /// A value in a point in time.
    static struct HistoryValue {
        SysTime time;
        T value;

        // Alternative to the more common toString.
        //void toString(scope void delegate(string) output) const {
        void toString(scope void delegate(const(char)[]) output)const {
            output(format("%s; %s", time, value));
        }
    }

    const(HistoryValue)[] values;

    private void addValue(T value) {
        values ~= HistoryValue(Clock.currTime(), value);
    }

    void opAssign(T value) {
        addValue(value);
    }

    @property T currentValue() const pure nothrow {
        return values.back.value;
    }

    alias currentValue this;

    @property auto history() const pure nothrow {
        return values;
    }

    /**
    Demonstrating D's compile-time reflection features. The member
    functions that are in this 'static if' block would be added for
    types T that are arrays (including strings). */
    static if (isArray!T) {
        // Append-with-assign operator.
        void opOpAssign(string op : "~")(T element) {
            addValue(currentValue ~ element);
        }

        // Similar implementation for other array operators...
    }
}

void main() {
    HistoryVariable!int x;
    x = 1;
    x = 2;
    x = 3;
    writefln("%(%s\n%)\n", x.history);

    HistoryVariable!string s;
    s = "hello";
    s ~= " world";
    s = "goodby";
    writefln("%(%s\n%)", s.history);
}
```

```txt
2013-Jan-19 23:04:55.1660302; 1
2013-Jan-19 23:04:55.1660407; 2
2013-Jan-19 23:04:55.1660424; 3

2013-Jan-19 23:04:55.1662551; hello
2013-Jan-19 23:04:55.1662581; hello world
2013-Jan-19 23:04:55.1662596; goodby

```


## EchoLisp

No native support. We implement an anonymous stack associated with the variable, and a few syntax rules to define the needed operations.

```lisp

(define-syntax-rule (make-h-var name) (define name (stack (gensym))))
(define-syntax-rule (h-get name) (stack-top name))
(define-syntax-rule (h-set name value) (push name value))
(define-syntax-rule (h-undo name)
	(begin
	(pop name)
	(when ( stack-empty? name) (error "no more values" 'name))
	(stack-top name)))

(define-syntax-rule (h-values name) (stack->list name))
;; usage
(make-h-var x)→ x

(h-set x 42) → 42
(h-set x 666)→ 666
(h-set x 'elvis)→ elvis
(h-values x) → (42 666 elvis) ;; historized values

(h-get x) → elvis
(h-undo x)→ 666
(h-undo x) → 42
(h-undo x) → ❌ error: no more values x

```


## Elena

ELENA 4.1 :

```elena
import extensions;
import system'collections;
import system'routines;
import extensions'routines;

class HistoryVariable
{
    Stack  previous := new Stack();
    object value;

    prop Value
    {
        get() = value;

        set(value)
        {
            if ((this value) != nil)
            {
                previous.push(this value)
            };

            this value := value
        }
    }

    undo()
    {
        ifnot (previous.isEmpty())
        {
            value := previous.pop()
        }
        else
        {
            value := nil
        }
    }

    enumerator() => previous;

    get string Printable() => this value;
}

public program()
{
    var o := new HistoryVariable();
    o.Value := 5;
    o.Value := "foo";
    o.Value := o.Value + "bar";

    console.printLine(o);

    o.forEach:printingLn;

    o.undo().undo().undo();

    console.printLine(o.Value ?? "nil")
}
```

```txt

foobar
foo
5
nil

```



## Erlang

If we consider the usage of history variables, like not losing the old value by mistake or having a way to get an old value, then Erlang can really help. Being single assignment Erlang will not allow you to lose an old value, and it is certain that the previous values are always there. The downside is that you must handle this manually.

```txt

1> V1 = "123".
"123"
2> V1 = V1 ++ "qwe".
** exception error: no match of right hand side value "123qwe"
3> V2 = V1 ++ "qwe".
"123qwe"
4> V3 = V2 ++ "ASD".
"123qweASD"
5> V1.
"123"
6> V2.
"123qwe"
7> V3.
"123qweASD"

```



## Factor


```factor
USING: accessors combinators formatting kernel models.history ;

1 <history> {
    [ add-history ]
    [ value>> "Initial value: %u\n" printf ]
    [ 2 >>value add-history ]
    [ 3 swap value<< ]
    [ value>> "Current value: %u\n" printf ]
    [ go-back ]
    [ go-back ]
    [ value>> "Restored value: %u\n" printf ]
} cleave
```

```txt

Initial value: 1
Current value: 3
Restored value: 1

```



## Forth

Forth does not have history variables, but it is trivial to define them. This one uses a linked list. The history variable is initialized to zero to avoid expensive sanity checks in ''H@''. ''H--'' undoes the assignment of a value and effectively returns a history variable to its former state.

```forth
: history create here cell+ , 0 , -1 , ;
: h@ @ @ ;
: h! swap here >r , dup @ , r> swap ! ;
: .history @ begin dup cell+ @ -1 <> while dup ? cell+ @ repeat drop ;
: h-- dup @ cell+ @ dup -1 = if abort" End of history" then swap ! ;
```


A sample session:

```txt

history z  ok
23 z h!  ok
z h@ . 23  ok
34 z h!  ok
z h@ . 34  ok
45 z h!  ok
z h@ . 45  ok
z .history 45 34 23  ok
z dup h-- h@ . 34  ok
z dup h-- h@ . 23  ok
z dup h-- h@ . 0  ok
z dup h-- h@ .
:86: End of history

```



## Go

We're all in this for the extra points.  Mallon and Takota seem happy with sequences, but time and timestamps were mentioned on LtU.  Beyond a separate sequence for each history variable, timestamps enable multiple variables to be seen in a common temporal sequence.  In Go, with it's attention to concurrency, this might be done with flexibility for proper handling of shared variables, and efficient handling of variables limited to a single thread.

```go
package main

import (
    "fmt"
    "sort"
    "sync"
    "time"
)

// data type for history variable (its an int)
type history struct {
    timestamp tsFunc
    hs        []hset
}

// data type for timestamp generator
type tsFunc func() time.Time

// data type for a "set" event
type hset struct {
    int           // new value
    t   time.Time // timestamp
}

// newHistory creates a history variable
func newHistory(ts tsFunc) history {
    return history{ts, []hset{{t: ts()}}}
}

// int returns the current value
func (h history) int() int {
    return h.hs[len(h.hs)-1].int
}

// set does what you expect and returns the timestamp recorded for the event
func (h *history) set(x int) time.Time {
    t := h.timestamp()
    h.hs = append(h.hs, hset{x, t})
    return t
}

// dump displays a complete history
func (h history) dump() {
    for _, hs := range h.hs {
        fmt.Println(hs.t.Format(time.StampNano), hs.int)
    }
}

// recall recalls the value stored in the history variable at time t.
// if the variable had not been created yet, ok is false.
func (h history) recall(t time.Time) (int, /*ok*/ bool) {
    i := sort.Search(len(h.hs), func(i int) bool {
        return h.hs[i].t.After(t)
    })
    if i > 0 {
        return h.hs[i-1].int, true
    }
    return 0, false
}

// newTimestamper returns a function that generates unique timestamps.
// Use a single timestamper for multiple history variables to preserve
// an unambiguous sequence of assignments across the multiple history
// variables within a single goroutine.
func newTimestamper() tsFunc {
    var last time.Time
    return func() time.Time {
        if t := time.Now(); t.After(last) {
            last = t
        } else {
            last.Add(1)
        }
        return last
    }
}

// newProtectedTimestamper generates unique timestamps for concurrent
// goroutines.
func newProtectedTimestamper() tsFunc {
    var last time.Time
    var m sync.Mutex
    return func() (t time.Time) {
        t = time.Now()
        m.Lock() // m protects last
        if t.After(last) {
            last = t
        } else {
            last.Add(1)
            t = last
        }
        m.Unlock()
        return
    }
}

func main() {
    // enable history variable support appropriate for single goroutine.
    ts := newTimestamper()
    // define a history variable
    h := newHistory(ts)
    // assign three values.  (timestamps kept for future reference.)
    ref := []time.Time{h.set(3), h.set(1), h.set(4)}
    // non-destructively display history
    fmt.Println("History of variable h:")
    h.dump()
    // recall the three values.  (this is non-destructive as well, but
    // different than the dump in that values are recalled by time.)
    fmt.Println("Recalling values:")
    for _, t := range ref {
        rv, _ := h.recall(t)
        fmt.Println(rv)
    }
}
```

```txt

History of variable h:
Dec  3 18:51:17.292260000 0
Dec  3 18:51:17.292262000 3
Dec  3 18:51:17.292264000 1
Dec  3 18:51:17.292270000 4
Recalling values:
3
1
4

```



## Haskell


There are no native Haskell history variables, but they are simple to implement.


```haskell
import Data.IORef

newtype HVar a = HVar (IORef [a])

newHVar :: a -> IO (HVar a)
newHVar value = fmap HVar (newIORef [value])

readHVar :: HVar a -> IO a
readHVar (HVar ref) = fmap head (readIORef ref)

writeHVar :: a -> HVar a -> IO ()
writeHVar value (HVar ref) = modifyIORef ref (value:)

undoHVar :: HVar a -> IO ()
undoHVar (HVar ref) = do
    (_ : history) <- readIORef ref
    writeIORef ref history

getHistory :: HVar a -> IO [a]
getHistory (HVar ref) = readIORef ref

-- Testing
main :: IO ()
main = do
    var <- newHVar 0
    writeHVar 1 var
    writeHVar 2 var
    writeHVar 3 var
    getHistory var >>= print
    undoHVar var
    undoHVar var
    undoHVar var
```



## J


J does not natively support "history variables", but the functionality is easy to add:


```j
varref_hist_=:'VAR','_hist_',~]
set_hist_=:4 :0
  V=.varref x
  if.0>nc<V do.(<V)=:''end.
  (<V)=.V~,<y
  y
)
getall_hist_=:3 :0
  (varref y)~
)
length_hist_=: #@getall
get_hist_=: _1 {:: getall
```


Example use:


```j
   'x' set_hist_ 9
9
   'x' set_hist_ 10
10
   'x' set_hist_ 11
11
   get_hist_ 'x'
11
   length_hist_ 'x'
3
   getall_hist_ 'x'
┌─┬──┬──┐
│9│10│11│
└─┴──┴──┘
```


Note that each value is contained in a box, so different values do not need to be type compatible with each other.  If this is considered a defect then assertions could be added to enforce type compatibility across assignments.

Note that only nouns are supported here:  If you want to store verbs using this mechanism you will need to use their gerunds.

Finally, note that the distinction between relevant history and irrelevant history depend on the application, and perhaps on the user. Also, necessity suggests that a lot of the history detail will be irrelevant for many purposes.


## Java

Java does not support history variables, but they are easy to implement using the lists that come with Java's Collections framework.

=
## Java Integer with History
=
This implementation does not allow the History Variable to be "empty". It can be assigned one or multiple "nulls", but it can never have an undefined value (such as being created and not initialized).

```Java
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

/**
 * A class for an "Integer with a history".
 * <p>
 * Note that it is not possible to create an empty Variable (so there is no "null") with this type. This is a design
 * choice, because if "empty" variables were allowed, reading of empty variables must return a value. Null is a
 * bad idea, and Java 8's Optional<T> (which is somewhat like the the official fix for the null-bad-idea) would
 * make things more complicated than an example should be.
 */
public class IntegerWithHistory {

    /**
     * The "storage Backend" is a list of all values that have been ever assigned to this variable. The List is
     * populated front to back, so a new value is inserted at the start (position 0), and older values move toward the end.
     */
    private final List<Integer> history;

    /**
     * Creates this variable and assigns the initial value
     *
     * @param value initial value
     */
    public IntegerWithHistory(Integer value) {
        history = new LinkedList<>();
        history.add(value);
    }

    /**
     * Sets a new value, pushing the older ones back in the history
     *
     * @param value the new value to be assigned
     */
    public void set(Integer value) {
        //History is populated from the front to the back, so the freshest value is stored a position 0
        history.add(0, value);
    }

    /**
     * Gets the current value. Since history is populuated front to back, the current value is the first element
     * of the history.
     *
     * @return the current value
     */
    public Integer get() {
        return history.get(0);
    }

    /**
     * Gets the entire history all values that have been assigned to this variable.
     *
     * @return a List of all values, including the current one, ordered new to old
     */
    public List<Integer> getHistory() {
        return Collections.unmodifiableList(this.history);
    }

    /**
     * Rolls back the history one step, so the current value is removed from the history and replaced by it's predecessor.
     * This is a destructive operation! It is not possible to rollback() beyond the initial value!
     *
     * @return the value that had been the current value until history was rolled back.
     */
    public Integer rollback() {
        if (history.size() > 1) {
            return history.remove(0);
        } else {
            return history.get(0);
        }
    }
}

```

Test Program:

```Java
public class TestIntegerWithHistory {

    public static void main(String[] args) {

        //creating and setting three different values
        IntegerWithHistory i = new IntegerWithHistory(3);
        i.set(42);
        i.set(7);

        //looking at current value and history
        System.out.println("The current value of i is :" + i.get());
        System.out.println("The history of i is :" + i.getHistory());

        //demonstrating rollback
        System.out.println("Rolling back:");
        System.out.println("returns what was the current value: " + i.rollback());
        System.out.println("after rollback: " + i.get());
        System.out.println("returns what was the current value: " + i.rollback());
        System.out.println("after rollback: " + i.get());
        System.out.println("Rolling back only works to the original value: " + i.rollback());
        System.out.println("Rolling back only works to the original value: " + i.rollback());
        System.out.println("So there is no way to 'null' the variable: " + i.get());

    }
}
```


```txt

The current value of i is :7
The history of i is :[7, 42, 3]
Rolling back:
returns what was the current value: 7
after rollback: 42
returns what was the current value: 42
after rollback: 3
Rolling back only works to the original value: 3
Rolling back only works to the original value: 3
So there is no way to 'null' the variable: 3

```


==={{header|Java History for "any class" using Generics}}===
This implementation does not allow the History Variable to be "empty". It can be assigned one or multiple "nulls", but it can never have an undefined value (such as being created and not initialized).

```java></lang

Test Program using a "String with History":

```java
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

/**
 * A Generic class for an "Anything with a history".
 * <p>
 * Note that it is not possible to create an empty Variable (so there is no "null") with this type. This is a design
 * choice, because if "empty" variables were allowed, reading of empty variables must return a value. Null is a
 * bad idea, and Java 8's Optional<T> (which is somewhat like the the official fix for the null-bad-idea) would
 * make things more complicated than an example should be.
 * <p>
 * Also note that this "really works" only with constant Ts. If somebody keeps a reference to an assigned value,
 * and is able to modify the state of this value through the reference , this will not be reflected in the history!
 */
public class WithHistory<T> {

    /**
     * The "storage Backend" is a list of all values that have been ever assigned to this variable. The List is
     * populated front to back, so a new value is inserted at the start (position 0), and older values move toward the end.
     */
    private final List<T> history;

    /**
     * Creates this variable and assigns the initial value
     *
     * @param value initial value
     */
    public WithHistory(T value) {
        history = new LinkedList<>();
        history.add(value);
    }

    /**
     * Sets a new value, pushing the older ones back in the history
     *
     * @param value the new value to be assigned
     */
    public void set(T value) {
        //History is populated from the front to the back, so the freshest value is stored a position 0
        history.add(0, value);
    }

    /**
     * Gets the current value. Since history is populuated front to back, the current value is the first element
     * of the history.
     *
     * @return the current value
     */
    public T get() {
        return history.get(0);
    }

    /**
     * Gets the entire history all values that have been assigned to this variable.
     *
     * @return a List of all values, including the current one, ordered new to old
     */
    public List<T> getHistory() {
        return Collections.unmodifiableList(this.history);
    }

    /**
     * Rolls back the history one step, so the current value is removed from the history and replaced by it's predecessor.
     * This is a destructive operation! It is not possible to rollback() beyond the initial value!
     *
     * @return the value that had been the cueent value until history was rolled back.
     */
    public T rollback() {
        if (history.size() > 1) {
            return history.remove(0);
        } else {
            return history.get(0);
        }
    }
}

```

```txt

The current value is :And now to something completely different.
The history is:[And now to something completely different., See you later History!, Hello History!]
Rolling back:
returns what was the current value: And now to something completely different.
after rollback: See you later History!
returns what was the current value: See you later History!
after rollback: Hello History!
Rolling back only works to the original value: Hello History!
Rolling back only works to the original value: Hello History!
So there is no way to 'null' the variable: Hello History!


```


## Julia

Julia currently does not support overloading the assignment "=" operator.

```Julia
mutable struct Historied
    num::Number
    history::Vector{Number}
    Historied(n) = new(n, Vector{Number}())
end

assign(y::Historied, z) = (push!(y.history, y.num); y.num = z; y)

x = Historied(1)

assign(x, 3)
assign(x, 5)
assign(x, 4)

println("Past history of variable x: $(x.history). Current value is $(x.num)")

```
```txt
Past history of variable x: Number[1, 3, 5]. Current value is 4
```



## Kotlin


```scala
// version 1.1.4

class HistoryVariable<T>(initialValue: T) {
    private val history = mutableListOf<T>()

    var currentValue: T
        get() = history[history.size - 1]
        set(value) {
           history.add(value)
        }

    init {
        currentValue = initialValue
    }

    fun showHistory() {
        println("The variable's history, oldest values first, is:")
        for (item in history) println(item)
    }
}

fun main(args: Array<String>) {
    val v = HistoryVariable(1)
    v.currentValue = 2
    v.currentValue = 3
    v.showHistory()
    println("\nCurrentvalue is ${v.currentValue}")
}
```


```txt

The variable's history, oldest values first, is:
1
2
3

Currentvalue is 3

```



## M2000 Interpreter

We can make objects type of Group to act as History Variables. Each group has a pointer to a stack object (a linked list).
First we see how we work with Stack (each module/function see a "current stack", we can make pointers to new stacks, but we can't get pointer of current stack). There are some statement no demonstrate here, such as Shift and ShiftBack (move top to a place, and move back to top), and Over (make new items by copying items).


```M2000 Interpreter

Flush ' empty curtrent stack
\\ a is a pointer to a new stack object
a=stack:=1,2@,3%
Print Len(A)=3
For i=1 to Len(a) {
      Print StackType$(a, i)="Number"
}
b=stack
Print len(b)=0
Push 1, 2 \\ to current stack
Stack b {
      \\ make b the current stack
      Data 1, 2, 3
}  ' Data add to bottom
\\ now current stack get the old object
Stack b {Push 1, 2, 3 } ' Push add to top, so 3 is at top
Stack b {
      While not empty {
            Read k  ' Read used to pop value to a variable
            \\ number pop value in an expression
            Print  k, number,  ' 3 2 1 1 2 3
      }
      Print
}
z=[]  ' [] pop all values from current stack to a new stack, and return a pointer
Print z  ' 2 1
z=Stack(a, z,a, z)  ' merge stack objects, to a new one, preserve a and z
Print Len(z)
\\ empty a using a new object
a=stack
b=stack:=1,2,3
\\ z has two stack objects
z=stack:= a, b
Stack b { data 1000}
b=stack ' b point to a new stack
\\ we get pointer back
b=stackitem(z, 2)
Print stackitem(b, 4)=1000

```


So now lets see the code:


```M2000 Interpreter

Module CheckHistoryVariables {
      Class History {
      Private:
            myvalue=stack
      Public:
            Group Count {
                  Value {
                        link parent myvalue to m
                        =Len(m)
                  }
            }
            Function CopyMe {
                  m=This
                  .myvalue<=stack(.myvalue)
                  =Group(m)
            }
            Group CopyValues {
                  Value {
                        link parent myvalue to m
                        =Stack(m)
                  }
            }
            Function Values (x as long) {
                  if x>=0 and x<=len(.myvalue) then  =StackItem(.myvalue, x)
                  Else Error "Fault index"
            }
            Module Back {
                  if len(.myvalue)<2 then exit
                 Stack .myvalue {Drop}
            }
            Operator "++" {
                  Stack .myvalue {
                        Push stackitem()+1
                  }
            }
            Operator "--" {
                  Stack .myvalue {
                        Push stackitem()-1
                  }
            }
            Operator "+="  (x) {
                  Stack .myvalue {
                        Push stackitem()+x
                  }       }
            Operator "-=" (x) {
                  Stack .myvalue {
                        Push stackitem()-x
                  }
            }
            Operator "/=" (x){
                  if x==0 then error "division by zero"
                  Stack .myvalue {
                        Push stackitem()/x
                  }
            }
            Operator "*=" (x){
                  Stack .myvalue {
                        Push stackitem()*x
                  }
            }
            Value {
                  =StackItem(.myvalue)
            }
            Set {
                   Read x : Stack .myvalue { Push x}
            }
      Class:
            Module History {
                  If Match("N") then Read x : Stack .myvalue { Push x}
            }
      }

      N=History()
      N=1
      N=2
      N=3
      Print N, N.Values(2), N.Values(3), N.Count
      \\ Get a copy of history
      m=N.CopyValues
      Print len(m)=3
      \\ just show all
      Print m  ' 3 2 1
      \\ or iterate from last to first
      k=each(m, -1, 1)
      While k {
            Print Stackitem(k)  \\  1 NL 2 NL 3  (NL = new line)
      }
      N1=N.CopyMe()
      N=5
      N1=4
      N=6
      Print N
      Print N1.Count=4, N.Count=5
      Print N1, N
      Print N.CopyValues ' 6 5 3 2 1
      Print N1.CopyValues ' 4 3 2 1
      Print N1<N
      N=N+1
      Print N.Count=6
      Print N.CopyValues ' 6 5 3 2 1
      Print "Go Back"
      While N.Count>1 {
            N.Back
            Print N
      }
      N+=10
      N*=2
      Print N.CopyValues  ' 22 11 1
}
CheckHistoryVariables

\\ for strings
Module CheckStringHistoryVariables {
      Class History$ {
      Private:
            myvalue=stack
      Public:
            Group Count {
                  Value {
                        link parent myvalue to m
                        =Len(m)
                  }
            }
            Function CopyMe {
                  m=This
                  .myvalue<=stack(.myvalue)
                  =Group(m)
            }
            Group CopyValues {
                  Value {
                        link parent myvalue to m
                        =Stack(m)
                  }
            }
            Function Values$ (x as long) {
                  if x>=0 and x<=len(.myvalue) then  =StackItem$(.myvalue, x)
                  Else Error "Fault index"
            }
            Module Back {
                  if len(.myvalue)<2 then exit
                 Stack .myvalue {Drop}
            }
            Operator "+="  (x$) {
                  Stack .myvalue {
                        Push stackitem$()+x$
                  }       }
            Value {
                  =StackItem$(.myvalue)
            }
            Set {
                   Read x$ : Stack .myvalue { Push x$}
            }
      Class:
            Module History {
                  If Match("S") then Read x$ : Stack .myvalue { Push x$}
            }
      }
      N$=History$("First")
      N$="Second"
      N$="Third"
      Print N.Count=3
      M=N.CopyValues
      K=Each(M, -1, 1)
      While K {
            Print StackItem$(K)
      }
      N$+=" and this"
      Print N.Count=4
      Print N.CopyValues
      N.Back
      Print N.Count=3
      Print N.CopyValues
      Print N.Values$(2)="Second"
      Input "New value:", N$
      Print N$
      Print N.Count=4
      Print N.CopyValues
}
CheckStringHistoryVariables

```


=={{header|Oberon-2}}==

```oberon2

MODULE HVar;
IMPORT Out, Conv;
TYPE
	(* Generic Object *)
	Object* = POINTER TO ObjectDesc;
	ObjectDesc = RECORD
		Show: PROCEDURE(o:Object);
		AsString: PROCEDURE(o: Object; VAR str: ARRAY OF CHAR);
	END;

	(* Integers *)
	Integer = POINTER TO IntegerDesc;
	IntegerDesc = RECORD (ObjectDesc)
		val: INTEGER;
	END;

	(* Chars *)
	Char = POINTER TO CharDesc;
	CharDesc = RECORD (ObjectDesc)
		val: CHAR;
	END;

	Node = POINTER TO NodeDesc;
	NodeDesc = RECORD
		val: Object;
		next: Node;
	END;

	(* History Variable *)
	HVar* = POINTER TO HVarDesc;
	HVarDesc = RECORD
		first, last: Node;
		size-: INTEGER;
	END;

	PROCEDURE CharAsString(o:Object; VAR dst: ARRAY OF CHAR);
	BEGIN
		IF LEN(dst) >= 2 THEN
			dst[1] := 0X;
		END;
		dst[0] := o(Char)^.val
	END CharAsString;

	PROCEDURE IntAsString(o:Object; VAR dst: ARRAY OF CHAR);
	BEGIN
		Conv.ConvInt(o(Integer)^.val,dst);
	END IntAsString;

	PROCEDURE ShowInt(o:Object);
	BEGIN
		Out.Int(o(Integer)^.val,10);
	END ShowInt;

	PROCEDURE ShowChar(o:Object);
	BEGIN
		Out.Char(o(Char)^.val);
	END ShowChar;

	PROCEDURE BoxChar(val: CHAR): Char;
	VAR
		c: Char;
	BEGIN
		NEW(c);
		c^.val := val;
		c^.Show := ShowChar;
		c^.AsString := CharAsString;
		RETURN c;
	END BoxChar;

	PROCEDURE BoxInt(val:INTEGER): Integer;
	VAR
		i: Integer;
	BEGIN
		NEW(i);
		i^.val := val;
		i^.Show := ShowInt;
		i^.AsString := IntAsString;
		RETURN i;
	END BoxInt;

	PROCEDURE InitNode(): Node;
	VAR
		l: Node;
	BEGIN
		NEW(l);
		l.val := NIL;
		l.next := NIL;
		RETURN l;
	END InitNode;

	PROCEDURE InitHVar(): HVar;
	VAR
		hv: HVar;
	BEGIN
		NEW(hv);
		hv.first := NIL;
		hv.last := NIL;
		hv.size := 0;
		RETURN hv;
	END InitHVar;

	PROCEDURE (v: HVar) Value(): Object;
	BEGIN
		RETURN v^.first^.val;
	END Value;

	PROCEDURE (v: HVar) Set(o: Object);
	VAR
		l: Node;
	BEGIN
		l := InitNode();
		l^.val := o;
		IF (v^.first = v^.last) & (v^.first = NIL) THEN
			v^.first := l;
			v^.last := l;
			INC(v^.size);
		ELSIF (v^.first = v^.last) & (v^.first # NIL) THEN
			v^.first^.next := l;
			v^.last := v^.first.next;
			INC(v^.size);
		ELSIF (v^.first # v^.last) THEN
			v^.last^.next := l;
			v^.last := l;
			INC(v^.size);
		END
	END Set;

	PROCEDURE (v: HVar) Undo(): Object;
	VAR
		o: Object;
	BEGIN
		IF (v^.first = v^.last) & (v^.first = NIL) THEN
			o := NIL;
		ELSIF (v^.first = v^.last) & (v^.first # NIL) THEN
			o := v^.first^.val;
			v^.first := NIL;
			v^.last := NIL;
			DEC(v^.size);
		ELSE
			o := v^.first^.val;
			v^.first := v^.first^.next;
			DEC(v^.size);
		END;
		RETURN o;
	END Undo;

	PROCEDURE (v: HVar) Print();
	VAR
		iter : Node;
	BEGIN
		iter := v.first;
		WHILE(iter # NIL) DO
			iter^.val^.Show(iter^.val);
			iter := iter^.next;
		END;
		Out.Ln;
	END Print;

	PROCEDURE ShowVal(val: Object);
	VAR
		s: ARRAY 128 OF CHAR;
	BEGIN
		val^.AsString(val,s);
		Out.String("> ");Out.String(s);Out.Ln;
	END ShowVal;
VAR
	history: HVar;

BEGIN
	history := InitHVar();
	history.Set(BoxChar(64X));
	history.Set(BoxInt(10));
	history.Set(BoxInt(15));
	history.Set(BoxInt(20));
	history.Print();
	ShowVal(history.Undo());
	ShowVal(history.Undo());
	ShowVal(history.Undo());
END HVar.

```



## OCaml

The easiest solution is to use the Stack module coming with OCaml's standard library:

```ocaml

open Stack
(* The following line is only for convenience when typing code *)
module H = Stack

let show_entry e =
    Printf.printf "History entry: %5d\n" e

let () =
  let  hs = H.create() in
  H.push 111 hs ;
  H.push 4 hs ;
  H.push 42 hs ;
  H.iter show_entry hs;
  hs |> H.pop |> Printf.printf "%d\n";
  hs |> H.pop |> Printf.printf "%d\n";
  hs |> H.pop |> Printf.printf "%d\n"

```


```txt

History entry:    42
History entry:     4
History entry:   111
42
4
111

```



## OxygenBasic

Simple history class for fixed length types that do not contain volatile pointer members.

```oxygenbasic

'
### ======

class History
'
### ======


indexbase 0

string buf
sys    ii,ld,pb

method constructor(sys n=1000, l=sizeof sys) {buf=nuls n*l : pb=strptr buf : ld=l : ii=0}
method destructor () {clear}
'
method setup(sys n=1000, l=sizeof sys) {buf=nuls n*l : pb=strptr buf : ld=l : ii=0}
method clear()                {buf="" : pb=0 : ld=0 : ii=0}
method max  (sys i)           {if i>ii{ii=i}}
method count() as sys         {return ii}
method size () as sys         {return ld}
'
method get  (any*p,i)         {copy @p, pb+i*ld,ld }           'out
method add  (any*p)           {copy pb+ii*ld,@p,ld : ii++}     'in
method put  (any*p,sys i)     {copy pb+i*ld,@p,ld : max i}     'in
'
end class

'====
'TEST
'====

'this works for fixed length types

'it will not work for types containing
'volatile pointers. eg: string members

type vector double x,y,z

vector v

new History hv(1000,sizeof v) 'give number of records and variable size

sys i
for i=0 to 9
  v<=i,i*10,i*100 'assign new values to vector
  hv.add v      'add to history
next

string tab=chr(9) : cr=chr(13)+chr(10)
string pr="Data History of v" cr cr
pr+="n" tab "x" tab "y" tab "z" cr
vector sv
'
for i=hv.count()-1 to 0 step -1
  hv.get sv,i
  pr+=i tab sv.x tab sv.y tab sv.z cr
next

print pr 'result '9,90,900 : 8,80,800 ...

del hv

```



## PARI/GP


```parigp
default(histsize, 1000) \\ or some other positive number to suit
1+7
sin(Pi)
2^100
\a1 \\ display history item #1, etc.
% \\ alternate syntax
%1 \\ alternate syntax
\a2
\a3
[%1, %2, %3] \\ or any other command using these values
```



## Peloton


```sgml>Turn history on <@ DEFHST
__on</@>
Notify Protium we are interested in the variable mv
<@ DEFHST>mv</@>
Assign a value: <@ LETVARLIT>mv|first value</@><@ SAYVAR>mv</@>
Reassign the value: <@ LETVARLIT>mv|second value</@><@ SAYVAR>mv</@>
Reassign the value: <@ LETVARLIT>mv|third value</@><@ SAYVAR>mv</@>
Dump history <@ SAYDMPHSTVAR>mv</@>
Current value: <@ SAYVAR>mv</@>
Undo once: <@ ACTUNDVAR>mv</@><@ SAYVAR>mv</@>
Undo twice: <@ ACTUNDVAR>mv</@><@ SAYVAR>mv</@>
Turn history off <@ DEFHST>__off</@>
```


Same code, Simplified Chinese dialect

```sgml
Turn history on <# 定义变量史>__on</#>
Notify Protium we are interested in the variable mv
<# 定义变量史>mv</#>
Assign a value: <# 指定变量字串>mv|first value</#><# 显示变量>mv</#>
Reassign the value: <# 指定变量字串>mv|second value</#><# 显示变量>mv</#>
Reassign the value: <# 指定变量字串>mv|third value</#><# 显示变量>mv</#>
Dump history <# 显示全内容变量史变量>mv</#>
Current value: <# 显示变量>mv</#>
Undo once: <# 运行撤消变量>mv</#><# 显示变量>mv</#>
Undo twice: <# 运行撤消变量>mv</#><# 显示变量>mv</#>
Turn history off <# 定义变量史>__off</#>
```


```txt
Turn history on
Notify Protium we are interested in the variable mv

Assign a value: first value
Reassign the value: second value
Reassign the value: third value
Dump history third value^second value^first value^
Current value: third value
Undo once: second value
Undo twice: first value
Turn history off

```



## Perl

Implemented via tie (and what's the usefulness of this?)

```Perl
package History;

sub TIESCALAR {
	my $cls = shift;
	my $cur_val = shift;
	return bless [];
}

sub FETCH {
	return shift->[-1]
}

sub STORE {
	my ($var, $val) = @_;
	push @$var, $val;
	return $val;
}

sub get(\$) { @{tied ${+shift}} }
sub on(\$) { tie ${+shift}, __PACKAGE__ }
sub off(\$) { untie ${+shift} }
sub undo(\$) { pop @{tied ${+shift}} }

package main;

my $x = 0;
History::on($x);

for ("a" .. "d") { $x = $_ }

print "History: @{[History::get($x)]}\n";

for (1 .. 3) {
	print "undo $_, ";
	History::undo($x);
	print "current value: $x\n";
}

History::off($x);
print "\$x is: $x\n";
```

{{out}}<lang>History: a b c d
undo 1, current value: c
undo 2, current value: b
undo 3, current value: a
$x is: a
```


## Perl 6

```perl6
class HistoryVar {
    has @.history;
    has $!var handles <Str gist FETCH Numeric>;
    method STORE($val) is rw {
        push @.history, [now, $!var];
        $!var = $val;
    }
}

my \foo = HistoryVar.new;

foo = 1;
foo = 2;
foo += 3;
foo = 42;

.say for foo.history;
say "Current value: {foo}";
```

```txt
[Instant:1523396079.685629 (Any)]
[Instant:1523396079.686844 1]
[Instant:1523396079.687130 2]
[Instant:1523396079.687302 5]
Current value: 42

```



## Phix

No native support, but trivial to implement.

If you only need the history for a single variable, you can just do this:

```Phix
sequence history = {}

type hvt(object o)
    history = append(history,o)
    return true
end type

hvt test = 1
test = 2
test = 3
?{"current",test}
?{"history",history}
```

```txt

{"current",3}
{"history",{1,2,3}}

```

Multiple history variables would require that routines must be invoked to create, update, and inspect them.

Writing this as a separate reusable component (but omitting destroy/freelist handling for simplicity):

```Phix
-- history.e
sequence histories = {}

global function new_history_id(object v)
    histories = append(histories,{v})
    return length(histories)
end function

global procedure set_hv(integer hv, object v)
    histories[hv] = append(histories[hv],v)
end procedure

global function get_hv(integer hv)
    return histories[hv][$]
end function

global function get_hv_full_history(integer hv)
    return histories[hv]
end function
```

And use it like this

```Phix
include history.e

constant test2 = new_history_id(1)
set_hv(test2, 2)
set_hv(test2, 3)
?{"current",get_hv(test2)}
?{"history",get_hv_full_history(test2)}
```

Same output. Of course test2 does not ''have'' to be a constant, but it may help.


## PicoLisp


```PicoLisp
(de setH ("Var" Val)
   (when (val "Var")
      (with "Var"
         (=: history (cons @ (: history))) ) )
   (set "Var" Val) )

(de restoreH ("Var")
   (set "Var" (pop (prop "Var" 'history))) )
```

Test:

```txt
: (setH 'A "Hello world")
-> "Hello world"

: (setH 'A '(a b c d))
-> (a b c d)

: (setH 'A 123)
-> 123

: A
-> 123

: (get 'A 'history)
-> ((a b c d) "Hello world")

: (restoreH 'A)
-> (a b c d)

: (restoreH 'A)
-> "Hello world"

: A
-> "Hello world"

: (restoreH 'A)
-> NIL
```



## PL/I


```PL/I

declare t float controlled;

do i = 1 to 5; /* a loop to read in and save five values. */
allocate t; get (t);
end;

do while (allocation(t) > 0); /* a loop to retrieve the values. */
put (t); free t;
end;

```



## PureBasic


```PureBasic
; integer history variable

Structure historyint
    List value.i()
EndStructure

Procedure SetInt (*var.historyint, val.i)
    AddElement(*var\value())

    If (ListSize(*var\value()) = 1)
        *var\value() = 0
        AddElement(*var\value())

    EndIf

    *var\value() = val
EndProcedure

Procedure ShowHistory (*var.historyint)
    ForEach *var\value()
        Debug *var\value()
    Next
EndProcedure

Procedure UndoInt (*var.historyint)
    If (ListSize(*var\value()) = 1)
        ProcedureReturn
    EndIf

    DeleteElement(*var\value())
EndProcedure

;----------------------------------------------

Define x.historyint

For i = 0 To 3
    setint(x, Random(50)+100 )
Next

Debug "x history:"
ShowHistory(x)

Debug ""

For i = 0 To 3
    UndoInt(x)
    Debug "undo, x = "+Str(x\value())
Next

```


<pre style="overflow:scroll">
x history:
0
131
109
143
108

undo, x = 143
undo, x = 109
undo, x = 131
undo, x = 0

```



## Python



```Python
import sys

HIST = {}

def trace(frame, event, arg):
    for name,val in frame.f_locals.items():
        if name not in HIST:
            HIST[name] = []
        else:
            if HIST[name][-1] is val:
                continue
        HIST[name].append(val)
    return trace

def undo(name):
    HIST[name].pop(-1)
    return HIST[name][-1]

def main():
    a = 10
    a = 20

    for i in range(5):
        c = i

    print "c:", c, "-> undo x3 ->",
    c = undo('c')
    c = undo('c')
    c = undo('c')
    print c
    print 'HIST:', HIST

sys.settrace(trace)
main()
```

{{out}}<lang>c: 4 -> undo x3 -> 1
HIST: {'a': [10, 20], 'i': [0, 1, 2, 3, 4], 'c': [0, 1], 'name': ['c']}
```



## Racket


Racket does not come with history variables, but they can be provided as a library as follows:


```racket

#lang racket

(require rackunit)

(struct hvar (current history) #:mutable)

(define (make-hvar v) (hvar v empty))

(define (hvar-set! hv new)
  (match-define (hvar cur hist) hv)
  (set-hvar-history! hv (cons cur hist))
  (set-hvar-current! hv new))

(define (hvar-undo! hv)
  (match-define (hvar cur (cons old hist)) hv)
  (set-hvar-current! hv old)
  (set-hvar-history! hv hist))

;; unit tests
(define hv (make-hvar 0))
(hvar-set! hv 1)
(check-equal? (hvar-current hv) 1)
(hvar-set! hv 2)
(hvar-set! hv 3)
(check-equal? (hvar-history hv) '(2 1 0))
(hvar-undo! hv)
(hvar-undo! hv)
(hvar-undo! hv)
(check-equal? (hvar-current hv) 0)

```



## REXX


### Version 1

The REXX language doesn't support histories, but can be coded with little trouble.


The history list part of the &nbsp' '''varSet'''   subroutine could be separated into its

own if you wanted to keep the subroutine's function pure.

```rexx
/*REXX program demonstrates a method to track history of assignments to a REXX variable.*/
varSet!.=0                                       /*initialize the all of the VARSET!'s. */
call varSet 'fluid',min(0,-5/2,-1)     ;    say 'fluid=' fluid
call varSet 'fluid',3.14159            ;    say 'fluid=' fluid
call varSet 'fluid',' Santa  Claus'    ;    say 'fluid=' fluid
call varSet 'fluid',,999
say 'There were' result "assignments (sets) for the FLUID variable."
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
varSet: arg ?x;  parse arg ?z, ?v, ?L            /*obtain varName, value, optional─List.*/
if ?L=='' then do                                /*not la ist,  so set the  X  variable.*/
               ?_=varSet!.0.?x+1                 /*bump the history count  (# of SETs). */
               varSet!.0.?x=?_                   /*   ... and store it in the "database"*/
               varSet!.?_.?x=?v                  /*   ... and store the  SET  value.    */
               call value(?x),?v                 /*now,  set the real  X  variable.     */
               return ?v                         /*also, return the value for function. */
               end
say                                              /*show a blank line for readability.   */
            do ?j=1 to ?L while ?j<=varSet!.0.?x /*display the list of  "set"  history. */
            say 'history entry' ?j "for var" ?z":" varSet!.?J.?x
            end   /*?j*/
return ?j-1                                      /*return the number of assignments.    */
```

'''output'''

```txt

fluid= -2.5
fluid= 3.14159
fluid=  Santa  Claus

history entry 1 for var fluid: -2.5
history entry 2 for var fluid: 3.14159
history entry 3 for var fluid:  Santa  Claus
There were 3 assignments (sets) for the FLUID variable.

```



### Version 2


```rexx

/* REXX ***************************************************************
* Demonstrate how the history of assignments can be kept and shown
* 13.07.2012 Walter Pachl  Rewrite of Version 1 for (my) readability
*   varset.i.varu contains the ith value assigned to var
*   varset.0.varu contains the number of assignments so far
**********************************************************************/
varset.=0                          /*initialize the assignment count */

call varset 'fluid',min(0,-5/2,-1)   ; say 'fluid=' fluid
call varset 'fluid',3.14159          ; say 'fluid=' fluid
call varset 'fluid',3.14159          ; say 'fluid=' fluid
call varset 'fluid',' Santa  Claus'  ; say 'fluid=' fluid
call varset 'FLUID',' Easter Rabbit' ; say 'fluid=' fluid

say 'There were' varset('fluid',,'L'),
    'assignments (sets) for the FLUID variable.'
exit

varset: Procedure Expose varset.
/**********************************************************************
* record values assigned to var (=varu as Rexx is case insensitive)
* Invoke with list<>'' to list the sequence of assigned values
* and return the number of assignments made (using this routine)
**********************************************************************/
  Parse Upper Arg varu              /* name of variable in uppercase */
  Parse arg var,value,list          /*varName, value, optional-List. */

  if list='' then do                /*not list, so set the X variable*/
    z=varset.0.varu+1               /*bump the history count (SETs). */
    varset.z.varu=value             /* ... and store it in "database"*/
    varset.0.varu=z                 /*the new history count          */
    call value var,value            /*now assign the value to var    */
    end
  else Do
    Say ''                          /*show blank line for readability*/
    do i=1 to varset.0.varu         /*list the assignment history    */
      say 'history entry' i "for var" var":" varset.i.varu
      end
    end
  Return varset.0.varu           /*return the number of assignments. */

```



## Ruby

This uses trace_var, which only works for global variables:

```ruby
foo_hist = []
trace_var(:$foo){|v| foo_hist.unshift(v)}

$foo = "apple"
$foo = "pear"
$foo = "banana"

p foo_hist # => ["banana", "pear", "apple"]

```



## Rust


```Rust
#[derive(Clone, Debug)]
struct HVar<T> {
    history: Vec<T>,
    current: T,
}

impl<T> HVar<T> {
    fn new(val: T) -> Self {
        HVar {
            history: Vec::new(),
            current: val,
        }
    }

    fn get(&self) -> &T {
        &self.current
    }

    fn set(&mut self, val: T) {
        self.history.push(std::mem::replace(&mut self.current, val));
    }

    fn history(&self) -> (&[T], &T) {
        (&self.history, &self.current)
    }

    fn revert(&mut self) -> Option<T> {
        self.history
            .pop()
            .map(|val| std::mem::replace(&mut self.current, val))
    }
}

fn main() {
    let mut var = HVar::new(0);
    var.set(1);
    var.set(2);
    println!("{:?}", var.history());
    println!("{:?}", var.revert());
    println!("{:?}", var.revert());
    println!("{:?}", var.revert());
    println!("{:?}", var.get());
}
```

```txt
([0, 1], 2)
Some(2)
Some(1)
None
0
```



## Scala

Scala doesn't have a native support for history variables, but it's quite easy to implement them. The following class uses same conventions as ML's mutable reference cells. (i.e. <code>!</code> as accessor, and <code>:=</code> as mutator.)

```scala
class HVar[A](initialValue: A) extends Proxy {
  override def self = !this
  override def toString = "HVar(" + !this + ")"

  def history = _history

  private var _history = List(initialValue)
  def unary_! = _history.head
  def :=(newValue: A): Unit = {
    _history = newValue :: _history
  }
  def modify(f: A => A): Unit = {
    _history = f(!this) :: _history
  }
  def undo: A = {
    val v = !this
    _history = _history.tail
    v
  }
}
```

Usage:

```scala>scala
 val h = new HVar(3)
h: HVar[Int] = HVar(3)

scala> h := 11

scala> h := 90

scala> !h
res32: Int = 90

scala> h.history
res33: List[Int] = List(90, 11, 3)

scala> h.undo
res34: Int = 90

scala> h.undo
res35: Int = 11

scala> h.undo
res36: Int = 3
```



## Sidef

Implemented as a class:

```ruby
class HistoryVar(v) {

    has history = []
    has variable = v

    method ≔(value) {
        history << variable
        variable = value
    }

    method to_s {
        "#{variable}"
    }

    method AUTOLOAD(_, name, *args) {
        variable.(name)(args...)
    }
}

var foo = HistoryVar(0)

foo ≔ 1
foo ≔ 2
foo ≔ foo+3
foo ≔ 42

say "History: #{foo.history}"
say "Current value: #{foo}"
```

```txt

History: [0, 1, 2, 5]
Current value: 42

```



## Smalltalk

Smalltalk doesn't have native support for history variables. It could be implemented using (implementation dependent) tracing facilities, or by changing the code generator (which is part of the library and open for change).
The following implementation is portable and in the spirit of the Lisp implementation above, and defines a new "assignment operator":

```smalltalk
Object subclass:'HVar'
    instanceVariableNames:'values'
    classVariableNames:''
    poolDictionaries:''
    category:'example'.
!

!HVar methodsFor:'accessing'!

<-- value
    (values ifNil:[values := OrderedCollection new]) add:value.
    ^  value
!

value
    ^ values last
!

undo
    values removeLast.
!

history
    ^ history
! !

|x|

x := HVar new.
x <-- 1.
x value.
x <-- 2.
x <-- (x value + 1).
x value.
x history.

```


## Swift

Swift does not support history variables. However, you can add a watcher that can track when the variable will change.

```Swift
var historyOfHistory = [Int]()
var history:Int = 0 {
    willSet {
        historyOfHistory.append(history)
    }
}

history = 2
history = 3
history = 4
println(historyOfHistory)

```


another approach, using generics:
plug this code into a Playground to see the output
```swift


struct History <T> {

    private var _history = [T]()
    var history : [T] {
        return _history
    }

    var now : T {
        return history.last!
    }

    init(_ firstValue:T) {
        _history = [firstValue]
    }

    mutating func set(newValue:T) {
        _history.append(newValue)
    }

    mutating func undo() -> T {
        guard _history.count > 1 else { return _history.first! }
        _history.removeLast()
        return _history.last!
    }
}

var h = History("First")
h.set("Next")
h.set("Then")
h.set("Finally")
h.history   // output ["First", "Next", "Then", "Finally"]

h.now       // outputs "Finally"
h.undo()    // outputs "Then"
h.undo()    // outputs "Next"
h.undo()    // outputs "First"
h.undo()    // outputs "First", since it can't roll back any further
h.undo()    // outputs "First"


```



## Tcl

Though Tcl's variables don't have history by default, it can be added easily through the use of traces:

```tcl
# Define the history machinery
proc histvar {varName operation} {
    upvar 1 $varName v ___history($varName) history
    switch -- $operation {
	start {
	    set history {}
	    if {[info exist v]} {
		lappend history $v
	    }
	    trace add variable v write [list histvar.write $varName]
	    trace add variable v read [list histvar.read $varName]
	}
	list {
	    return $history
	}
	undo {
	    set history [lrange $history 0 end-1]
	}
	stop {
	    unset history
	    trace remove variable v write [list histvar.write $varName]
	    trace remove variable v read [list histvar.read $varName]
	}
    }
}
proc histvar.write {key varName args} {
    upvar 1 $varName v ___history($key) history
    lappend history $v
}
proc histvar.read {key varName args} {
    upvar 1 $varName v ___history($key) history
    set v [lindex $history end]
}
```

Demonstrating how to use it:

```tcl
# Enable history for foo
histvar foo start
set foo {a b c d}
set foo 123
set foo "quick brown fox"
puts $foo
puts foo-history=[join [histvar foo list] ", "]
puts $foo
histvar foo undo
puts $foo
histvar foo undo
puts $foo
histvar foo stop
```

```txt

quick brown fox
foo-history=a b c d, 123, quick brown fox
quick brown fox
123
a b c d

```



## zkl

No native support, here is a something that can be done with a class:

```zkl
class HistoryVar{
   var [private] _v, _history=List(), maxSz;
   fcn init(v,maxEntries=3){ maxSz=maxEntries; set(v) }
   fcn set(v){
      _v=v; _history.append(T(Time.Clock.time,v));
      if(_history.len()>maxSz) _history.del(0);
      self
   }
   fcn get(n=0){  // look back into history
      z:=_history.len();
      n=(if(n>=z) 0 else z-n-1);
      _history[n][1]
   }
   var [proxy] v=fcn{ _v };
   var [proxy] history=
      fcn{ _history.pump(List,fcn([(t,v)]){ T(Time.Date.ctime(t),v) }) };
   fcn __opAdd(x){ set(_v + x); self }
}
```


```zkl
hv:=HistoryVar(123);
hv+4;
hv.set("Shuttle prepared for liftoff");
hv+": orbit achived";
hv.history.concat("\n").println();
hv.get(3).println("<-- value two changes ago");
```

```txt

L("Thu Oct 13 13:48:37 2016",127)
L("Thu Oct 13 13:48:37 2016","Shuttle prepared for liftoff")
L("Thu Oct 13 13:48:37 2016","Shuttle prepared for liftoff: orbit achived")
127<-- value two changes ago

```


