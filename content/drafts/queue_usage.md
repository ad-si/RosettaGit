+++
title = "Queue/Usage"
description = ""
date = 2019-10-12T07:53:01Z
aliases = []
[extra]
id = 3287
[taxonomies]
categories = []
tags = []
+++

{{task|Data Structures}}{{Data structure}}
[[File:Fifo.gif|frame|right|Illustration of FIFO behavior]]

;Task:
Create a queue data structure and demonstrate its operations.

(For implementations of queues, see the [[FIFO]] task.)


Operations:
::*   push                (aka ''enqueue'') - add element
::*   pop            (aka ''dequeue'') - pop first element
::*   empty                    - return truth value when empty



{{Template:See also lists}}





## 8th


```forth

10 q:new  \ create a new queue 10 deep
123 q:push
341 q:push  \ push 123, 341 onto the queue
q:pop . cr  \ displays 123
q:len . cr  \ displays 1
q:pop . cr  \ displays 341
q:len . cr  \ displays 0

```


## Ada


```ada
with FIFO;
with Ada.Text_Io; use Ada.Text_Io;

procedure Queue_Test is
   package Int_FIFO is new FIFO (Integer);
   use Int_FIFO;
   Queue : FIFO_Type;
   Value : Integer;
begin
   Push (Queue, 1);
   Push (Queue, 2);
   Push (Queue, 3);
   Pop (Queue, Value);
   Pop (Queue, Value);
   Push (Queue, 4);
   Pop (Queue, Value);
   Pop (Queue, Value);
   Push (Queue, 5);
   Pop (Queue, Value);
   Put_Line ("Is_Empty " & Boolean'Image (Is_Empty (Queue)));
end Queue_Test;
```

Sample output:

```txt
Is_Empty TRUE
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - one extension to language used - PRAGMA READ - a non standard feature similar to C's #include directive.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.7 algol68g-2.7].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}

'''File: prelude/link.a68''' c.f. [[Queue/Definition#ALGOL 68|Queue/Definition]]

'''File: prelude/queue_base.a68''' c.f. [[Queue/Definition#ALGOL 68|Queue/Definition]]

'''File: test/data_stigler_diet.a68'''
```algol68
# -*- coding: utf-8 -*- #
MODE DIETITEM = STRUCT(
  STRING food, annual quantity, units, REAL cost
);

# Stigler's 1939 Diet ... #
FORMAT diet item fmt = $g": "g" "g" = $"zd.dd$;
[]DIETITEM stigler diet = (
  ("Cabbage",           "111","lb.",  4.11),
  ("Dried Navy Beans",  "285","lb.", 16.80),
  ("Evaporated Milk",    "57","cans", 3.84),
  ("Spinach",            "23","lb.",  1.85),
  ("Wheat Flour",       "370","lb.", 13.33),
  ("Total Annual Cost",    "","",    39.93)
)
```
'''File: test/queue.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

MODE OBJVALUE = DIETITEM;
PR read "prelude/link.a68" PR;# c.f. [[rc:Queue/Definition]] #
PR read "prelude/queue_base.a68" PR; # c.f. [[rc:Queue/Definition]] #

PR read "test/data_stigler_diet.a68" PR;
OBJQUEUE example queue; obj queue init(example queue);

FOR i TO UPB stigler diet DO
# obj queue put(example queue, stigler diet[i]) or ... #
  stigler diet[i] +=: example queue
OD;

printf($"Get remaining values from queue:"l$);
WHILE NOT obj queue is empty(example queue) DO
# OR example queue ISNT obj queue empty #
  printf((diet item fmt, obj queue get(example queue), $l$))
OD
```
'''Output:'''

```txt

Get remaining values from queue:
Cabbage: 111 lb. = $ 4.11
Dried Navy Beans: 285 lb. = $16.80
Evaporated Milk: 57 cans = $ 3.84
Spinach: 23 lb. = $ 1.85
Wheat Flour: 370 lb. = $13.33
Total Annual Cost:   = $39.93

```

'''See also:''' [[Stack#ALGOL_68|Stack]]


## AppleScript


```AppleScript
on push(StackRef, value)
    set StackRef's contents to {value} & StackRef's contents
    return StackRef
end push

on pop(StackRef)
    set R to missing value
    if StackRef's contents ≠ {} then
        set R to StackRef's contents's item 1
        set StackRef's contents to {} & rest of StackRef's contents
    end if
    return R
end pop

on isStackEmpty(StackRef)
    if StackRef's contents = {} then return true
    return false
end isStackEmpty


set theStack to {}
repeat with i from 1 to 5
    push(a reference to theStack, i)
    log result
end repeat
repeat until isStackEmpty(theStack) = true
    pop(a reference to theStack)
    log result
end repeat
```
Output (in Script Editor Event Log):
```txt
  (*1*)
    (*2, 1*)
    (*3, 2, 1*)
    (*4, 3, 2, 1*)
    (*5, 4, 3, 2, 1*)
    (*5*)
    (*4*)
    (*3*)
    (*2*)
    (*1*)

```



## App Inventor

This Rosetta Code Task requires that the queue operations of push (enqueue), pop (dequeue) and empty be demonstrated with App Inventor.

This is easy to do as those operations are basically available in a slightly different form as list operations.

In addition for this example, I added a top function to view the first item in the queue.



The solution is a complete (although greatly simplified) hamburger restaurant where the customers and orders are the queues.


Customers enter the restaurant at random intervals between 2 and 10 seconds (Customers Clock Timer)

Each customer will request a random item from the menu.

If the item is not available, the customer leaves.

If that item is available (there are only 30 of each item) then the order is placed and payment is accepted (push|enqueue Customer, push|enqueue Order).

Once an order is placed, the customer must wait for the meal to be prepared -- each menu item takes a different number of seconds to prepare (Orders Clock Timer.)

Once the item is prepared, their customer name and the ordered item are removed from the queues (pop|dequeue Customer, pop|dequeue Order).

If there are no pending orders, (empty Orders queue) the cook just waits for one to be placed (the orders clock continues to run to poll for new orders by testing if the Orders queue is not empty.)

Eventually, all items will have been sold, and the store manager will empty the cash register and fly to Tahiti with the waitress.

The eager --  but destined to be frustrated customers -- will continue to request their random items, forever. :)

[https://lh6.googleusercontent.com/-dTvs9totvDE/Uu3ZiFeE90I/AAAAAAAAJ-w/lJBVHOd-p0g/s1600/Untitled.png CLICK HERE TO VIEW THE CODE BLOCKS AND ANDROID APP SCREEN]
---
END


## Arturo



```arturo
Queue #{
	list #()

	push {
		list list+&
	}

	pop {
		if $(empty) {
			panic "trying to pop from an empty queue!"
		}

		first_item $(first list)
		list $(deleteBy list 0)
		return first_item
	}

	empty {
		$(size list)=0
	}

	inspect {
		log this
	}
}

q $(new ~Queue)

q.push "one"
q.push "two"
q.push "three"

q.inspect

print "popped = " + $(q.pop)
print "is it empty? = " + $(q.empty)
```


{{out}}


```txt
#{
	empty           <function: 0x1093917A0>
	inspect         <function: 0x109391800>
	list            #(
	                	"one"
	                	"two"
	                	"three"
	                )
	pop             <function: 0x109391740>
	push            <function: 0x1093916E0>
}
popped = one
is it empty? = false
```



## Astro


```python
let my_queue = Queue()

my_queue.push!('foo')
my_queue.push!('bar')
my_queue.push!('baz')

print my_queue.pop!() # 'foo'
print my_queue.pop!() # 'bar'
print my_queue.pop!() # 'baz'
```



## AutoHotkey


```autohotkey
push("qu", 2), push("qu", 44), push("qu", "xyz") ; TEST

MsgBox % "Len = " len("qu") ; Number of entries
While !empty("qu")          ; Repeat until queue is not empty
    MsgBox % pop("qu")      ; Print popped values (2, 44, xyz)
MsgBox Error = %ErrorLevel% ; ErrorLevel =  0: OK
MsgBox % pop("qu")          ; Empty
MsgBox Error = %ErrorLevel% ; ErrorLevel = -1: popped too much
MsgBox % "Len = " len("qu") ; Number of entries

push(queue,_) {             ; push _ onto queue named "queue" (!=_), _ string not containing |
    Global
    %queue% .= %queue% = "" ? _ : "|" _
}

pop(queue) {                ; pop value from queue named "queue" (!=_,_1,_2)
    Global
    RegExMatch(%queue%, "([^\|]*)\|?(.*)", _)
    Return _1, ErrorLevel := -(%queue%=""), %queue% := _2
}

empty(queue) {              ; check if queue named "queue" is empty
    Global
    Return %queue% = ""
}

len(queue) {                ; number of entries in "queue"
    Global
    StringReplace %queue%, %queue%, |, |, UseErrorLevel
    Return %queue% = "" ? 0 : ErrorLevel+1
}
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      FIFOSIZE = 1000

      FOR n = 3 TO 5
        PRINT "Push ";n : PROCenqueue(n)
      NEXT
      PRINT "Pop " ; FNdequeue
      PRINT "Push 6" : PROCenqueue(6)
      REPEAT
        PRINT "Pop " ; FNdequeue
      UNTIL FNisempty
      PRINT "Pop " ; FNdequeue
      END

      DEF PROCenqueue(n) : LOCAL f%
      DEF FNdequeue : LOCAL f% : f% = 1
      DEF FNisempty : LOCAL f% : f% = 2
      PRIVATE fifo(), rptr%, wptr%
      DIM fifo(FIFOSIZE-1)
      CASE f% OF
        WHEN 0:
          wptr% = (wptr% + 1) MOD FIFOSIZE
          IF rptr% = wptr% ERROR 100, "Error: queue overflowed"
          fifo(wptr%) = n
        WHEN 1:
          IF rptr% = wptr% ERROR 101, "Error: queue empty"
          rptr% = (rptr% + 1) MOD FIFOSIZE
          = fifo(rptr%)
        WHEN 2:
          = (rptr% = wptr%)
      ENDCASE
      ENDPROC
```

'''Output:'''

```txt

Push 3
Push 4
Push 5
Pop 3
Push 6
Pop 4
Pop 5
Pop 6
Pop
Error: queue empty

```



## Bracmat

Below, <code>queue</code> is the name of a class with a data member <code>list</code> and three methods <code>enqueue</code>, <code>dequeue</code> and <code>empty</code>.

No special provision is implemented to "throw and exception" in case you try to dequeue from and empty queue, because, in Bracmat, evaluation of an expression, besides resulting in an evaluated expression, always also either "succeeds" or "fails". (There is, in fact, a third possibility, "ignore", telling Bracmat to close an eye even though an evaluation didn't succeed.) So in the example below, the last dequeue operation fails and the program continues on the right hand side of the bar (<code>|</code>) operator

```bracmat
  ( queue
  =   (list=)
      (enqueue=.(.!arg) !(its.list):?(its.list))
      ( dequeue
      =   x
        .   !(its.list):?(its.list) (.?x)
          & !x
      )
      (empty=.!(its.list):)
  )
& new$queue:?Q
& (   (Q..enqueue)$1
    & (Q..enqueue)$2
    & (Q..enqueue)$3
    & out$((Q..dequeue)$)
    & (Q..enqueue)$4
    & out$((Q..dequeue)$)
    & out$((Q..dequeue)$)
    &   out
      $ ( The
          queue
          is
          ((Q..empty)$&|not)
          empty
        )
    & out$((Q..dequeue)$)
    &   out
      $ ( The
          queue
          is
          ((Q..empty)$&|not)
          empty
        )
    & out$((Q..dequeue)$)
    & out$Success!
  | out$"Attempt to dequeue failed"
  )
;
```

Output:

```txt
1
2
3
The queue is not empty
4
The queue is empty
Attempt to dequeue failed
```



## C

See [[FIFO]] for the needed code.

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include <sys/queue.h>

/* #include "fifolist.h" */

int main()
{
  int i;
  FIFOList head;

  TAILQ_INIT(&head);

  /* insert 20 integer values */
  for(i=0; i < 20; i++) {
    m_enqueue(i, &head);
  }

  /* dequeue and print */
  while( m_dequeue(&i, &head) )
    printf("%d\n", i);

  fprintf(stderr, "FIFO list %s\n",
      ( m_dequeue(&i, &head) ) ?
      "had still an element" :
      "is void!");

  exit(0);
}
```



## C++

Note that with C++'s standard queue, accessing the first element of the queue and removing it are two separate operations, <tt>front()</tt> and <tt>pop()</tt>.

```cpp
#include <queue>
#include <cassert> // for run time assertions

int main()
{
  std::queue<int> q;
  assert( q.empty() );        // initially the queue is empty

  q.push(1);                  // add an element
  assert( !q.empty() );       // now the queue isn't empty any more
  assert( q.front() == 1 );   // the first element is, of course, 1

  q.push(2);                  // add another element
  assert( !q.empty() );       // it's of course not empty again
  assert( q.front() == 1 );   // the first element didn't change

  q.push(3);                  // add yet an other element
  assert( !q.empty() );       // the queue is still not empty
  assert( q.front() == 1 );   // and the first element is still 1

  q.pop();                    // remove the first element
  assert( !q.empty() );       // the queue is not yet empty
  assert( q.front() == 2);    // the first element is now 2 (the 1 is gone)

  q.pop();
  assert( !q.empty() );
  assert( q.front() == 3);

  q.push(4);
  assert( !q.empty() );
  assert( q.front() == 3);

  q.pop();
  assert( !q.empty() );
  assert( q.front() == 4);

  q.pop();
  assert( q.empty() );

  q.push(5);
  assert( !q.empty() );
  assert( q.front() == 5);

  q.pop();
  assert( q.empty() );
}
```


Note that the container used to store the queue elements can be specified explicitly; to use a linked linst instead of a deque (the latter is the default), just replace the definition of <tt>q</tt> to

```cpp
  std::queue<int, std::list<int> >
```


(and add <tt>#include <list></tt>, of course). Also note that the containers can be used directly; in that case <tt>push</tt> and <tt>pop</tt> have to be replaced by <tt>push_back</tt> and <tt>pop_front</tt>.


## C sharp

In C# we can use the Queue<T> class in the .NET 2.0 framework.

```csharp
using System;
using System.Collections.Generic;

namespace RosettaCode
{
    class Program
    {
        static void Main()
        {
            // Create a queue and "push" items into it
            Queue<int> queue  = new Queue<int>();
            queue.Enqueue(1);
            queue.Enqueue(3);
            queue.Enqueue(5);

            // "Pop" items from the queue in FIFO order
            Console.WriteLine(queue.Dequeue()); // 1
            Console.WriteLine(queue.Dequeue()); // 3
            Console.WriteLine(queue.Dequeue()); // 5

            // To tell if the queue is empty, we check the count
            bool empty = queue.Count == 0;
            Console.WriteLine(empty); // "True"

            // If we try to pop from an empty queue, an exception
            // is thrown.
            try
            {
                queue.Dequeue();
            }
            catch (InvalidOperationException exception)
            {
                Console.WriteLine(exception.Message); // "Queue empty."
            }
        }
    }
}
```



## Clojure

Using the implementation from [[FIFO]]:

```lisp
(def q (make-queue))

(enqueue q 1)
(enqueue q 2)
(enqueue q 3)

(dequeue q) ; 1
(dequeue q) ; 2
(dequeue q) ; 3

(queue-empty? q) ; true
```

Or use a java implementation:

```lisp
(def q (java.util.LinkedList.))

(.add q 1)
(.add q 2)
(.add q 3)

(.remove q) ; 1
(.remove q) ; 2
(.remove q) ; 3

(.isEmpty q) ; true
```



## CoffeeScript


```coffeescript

# We build a Queue on top of an ordinary JS array, which supports push
# and shift.  For simple queues, it might make sense to just use arrays
# directly, but this code shows how to encapsulate the array behind a restricted
# API.  For very large queues, you might want a more specialized data
# structure to implement the queue, in case arr.shift works in O(N) time, which
# is common for array implementations.  On my laptop I start noticing delay
# after about 100,000 elements, using node.js.
Queue = ->
  arr = []
  enqueue: (elem) ->
    arr.push elem
  dequeue: (elem) ->
    throw Error("queue is empty") if arr.length == 0
    arr.shift elem
  is_empty: (elem) ->
    arr.length == 0

# test
do ->
  q = Queue()
  for i in [1..100000]
    q.enqueue i

  console.log q.dequeue() # 1
  while !q.is_empty()
    v = q.dequeue()
  console.log v # 1000

  try
    q.dequeue() # throws Error
  catch e
    console.log "#{e}"

```

output
<lang>
> coffee queue.coffee
1
100000
Error: queue is empty

```



## Common Lisp

Using the implementation from [[FIFO]].


```lisp
(let ((queue (make-queue)))
  (enqueue 38 queue)
  (assert (not (queue-empty-p queue)))
  (enqueue 23 queue)
  (assert (eql 38 (dequeue queue)))
  (assert (eql 23 (dequeue queue)))
  (assert (queue-empty-p queue)))
```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE UseQueue;
IMPORT
	Queue,
	Boxes,
	StdLog;

PROCEDURE Do*;
VAR
	q: Queue.Instance;
	b: Boxes.Box;
BEGIN
	q := Queue.New(10);
	q.Push(Boxes.NewInteger(1));
	q.Push(Boxes.NewInteger(2));
	q.Push(Boxes.NewInteger(3));
	b := q.Pop();
	b := q.Pop();
	q.Push(Boxes.NewInteger(4));
	b := q.Pop();
	b := q.Pop();
	StdLog.String("Is empty:> ");StdLog.Bool(q.IsEmpty());StdLog.Ln
END Do;
END UseQueue.

```

Execute: ^Q UseQueue.Do<br/>
Output:

```txt

Is empty:  $TRUE

```



## D


```d
class LinkedQueue(T) {
    private static struct Node {
        T data;
        Node* next;
    }

    private Node* head, tail;

    bool empty() { return head is null; }

    void push(T item) {
        if (empty())
            head = tail = new Node(item);
        else {
            tail.next = new Node(item);
            tail = tail.next;
        }
    }

    T pop() {
        if (empty())
            throw new Exception("Empty LinkedQueue.");
        auto item = head.data;
        head = head.next;
        if (head is tail) // Is last one?
            // Release tail reference so that GC can collect.
            tail = null;
        return item;
    }

    alias push enqueue;
    alias pop dequeue;
}

void main() {
    auto q = new LinkedQueue!int();
    q.push(10);
    q.push(20);
    q.push(30);
    assert(q.pop() == 10);
    assert(q.pop() == 20);
    assert(q.pop() == 30);
    assert(q.empty());
}
```



### Faster Version

This versions creates a circular queue able to grow. Define "queue_usage2_main" to run the main and its tests.

```d
module queue_usage2;

import std.traits: hasIndirections;

struct GrowableCircularQueue(T) {
    public size_t length;
    private size_t first, last;
    private T[] A = [T.init];

    this(T[] items...) pure nothrow @safe {
        foreach (x; items)
            push(x);
    }

    @property bool empty() const pure nothrow @safe @nogc {
        return length == 0;
    }

    @property T front() pure nothrow @safe @nogc {
        assert(length != 0);
        return A[first];
    }

    T opIndex(in size_t i) pure nothrow @safe @nogc {
        assert(i < length);
        return A[(first + i) & (A.length - 1)];
    }

    void push(T item) pure nothrow @safe {
        if (length >= A.length) { // Double the queue.
            immutable oldALen = A.length;
            A.length *= 2;
            if (last < first) {
                A[oldALen .. oldALen + last + 1] = A[0 .. last + 1];
                static if (hasIndirections!T)
                    A[0 .. last + 1] = T.init; // Help for the GC.
                last += oldALen;
            }
        }
        last = (last + 1) & (A.length - 1);
        A[last] = item;
        length++;
    }

    @property T pop() pure nothrow @safe @nogc {
        assert(length != 0);
        auto saved = A[first];
        static if (hasIndirections!T)
            A[first] = T.init; // Help for the GC.
        first = (first + 1) & (A.length - 1);
        length--;
        return saved;
    }
}

version (queue_usage2_main) {
    void main() {
        GrowableCircularQueue!int q;
        q.push(10);
        q.push(20);
        q.push(30);
        assert(q.pop == 10);
        assert(q.pop == 20);
        assert(q.pop == 30);
        assert(q.empty);

        uint count = 0;
        foreach (immutable i; 1 .. 1_000) {
            foreach (immutable j; 0 .. i)
                q.push(count++);
            foreach (immutable j; 0 .. i)
                q.pop;
        }
    }
}
```



## Delphi

Generics were added in Delphi2009.


```Delphi
program QueueUsage;

{$APPTYPE CONSOLE}

uses Generics.Collections;

var
  lStringQueue: TQueue<string>;
begin
  lStringQueue := TQueue<string>.Create;
  try
    lStringQueue.Enqueue('First');
    lStringQueue.Enqueue('Second');
    lStringQueue.Enqueue('Third');

    Writeln(lStringQueue.Dequeue);
    Writeln(lStringQueue.Dequeue);
    Writeln(lStringQueue.Dequeue);

    if lStringQueue.Count = 0 then
      Writeln('Queue is empty.');
  finally
    lStringQueue.Free;
  end
end.
```


Output:

```txt
First
Second
Third
Queue is empty.
```

=={{header|Déjà Vu}}==
This uses the definition from [[Queue/Definition#Déjà Vu]]

```dejavu
local :Q queue
!. empty Q
enqueue Q "HELLO"
enqueue Q 123
enqueue Q "It's a magical place"
!. empty Q
!. dequeue Q
!. dequeue Q
!. dequeue Q
!. empty Q
!. dequeue Q
```

{{out}}

```txt
true
false
"HELLO"
123
"It's a magical place"
true
Wrong value: popping from empty queue in Raise:
compiler.deja:857
queue.deja:28
queue.deja:10 in dequeue
```



## E

Using the implementation from [[FIFO]].


```e
def [reader, writer] := makeQueue()
require(escape empty { reader.dequeue(empty); false } catch _ { true })
writer.enqueue(1)
writer.enqueue(2)
require(reader.dequeue(throw) == 1)
writer.enqueue(3)
require(reader.dequeue(throw) == 2)
require(reader.dequeue(throw) == 3)
require(escape empty { reader.dequeue(empty); false } catch _ { true })
```


E also has queues in the standard library such as <code>&lt;import:org.erights.e.examples.concurrency.makeQueue></code>, but they are designed for concurrency purposes and do not report emptiness but rather return a promise for the next element.

## Elena

ELENA 4.x :

```elena
import system'collections;
import extensions;

public program()
{
    // Create a queue and "push" items into it
    var queue := new Queue();

    queue.push:1;
    queue.push:3;
    queue.push:5;

    // "Pop" items from the queue in FIFO order
    console.printLine(queue.pop()); // 1
    console.printLine(queue.pop()); // 3
    console.printLine(queue.pop()); // 5

    // To tell if the queue is empty, we check the count
    console.printLine("queue is ",(queue.Length == 0).iif("empty","nonempty"));

    // If we try to pop from an empty queue, an exception
    // is thrown.
    queue.pop() | on:(e){ console.writeLine:"Queue empty." }
}
```



## Elisa

A generic component for Queues and its usage are described in [[Queue/Definition]]


## Elixir

Here a list is used as Queue.

```Elixir

defmodule Queue do
  def empty?([]), do: true
  def empty?(_), do: false

  def pop([h|t]), do: {h,t}

  def push(q,t), do: q ++ [t]

  def front([h|_]), do: h
end

```

Example:
<lang>
iex(2)> q = [1,2,3,4,5]
[1, 2, 3, 4, 5]
iex(3)> Queue.push(q,10)
[1, 2, 3, 4, 5, 10]
iex(4)> front=Queue.front(q)
1
iex(5)> Queue.empty?(q)
false
iex(6)> Queue.pop(q)
{1, [2, 3, 4, 5]}
iex(7)> l=[]
[]
iex(8)> Queue.empty?(l)
true

```



## Erlang

All functions, from the shell:

```Erlang>1
 Q = fifo:new().
{fifo,[],[]}
2> fifo:empty(Q).
true
3> Q2 = fifo:push(Q,1).
{fifo,[1],[]}
4> Q3 = fifo:push(Q2,2).
{fifo,[2,1],[]}
5> fifo:empty(Q3).
false
6> fifo:pop(Q3).
{1,{fifo,[],[2]}}
7> {Popped, Q} = fifo:pop(Q2).
{1,{fifo,[],[]}}
8> fifo:pop(fifo:new()).
** exception error: 'empty fifo'
     in function  fifo:pop/1
```


Crashing is the normal expected behavior in Erlang: let it crash, a supervisor will take responsibility of restarting processes, or the caller will take care of it. Only program for the successful cases.


## Factor

For this task, we'll use Factor's <code>deque</code> vocabulary (short for double-ended queue). The <code>deque</code> class is a mixin, one of whose instances is <code>dlist</code> (double-linked list). Hence, the deque protocol works with double-linked lists. When using a deque as a queue, the convention is to queue elements with <code>push-front</code> and deque them with <code>pop-back</code>.

```factor
USING: combinators deques dlists kernel prettyprint ;
IN: rosetta-code.queue-usage

DL{ } clone {                ! make new queue
    [ [ 1 ] dip push-front ] ! push 1
    [ [ 2 ] dip push-front ] ! push 2
    [ [ 3 ] dip push-front ] ! push 3
    [ .                    ] ! DL{ 3 2 1 }
    [ pop-back drop        ] ! pop 1 (and discard)
    [ pop-back drop        ] ! pop 2 (and discard)
    [ pop-back drop        ] ! pop 3 (and discard)
    [ deque-empty? .       ] ! t
} cleave
```

Alternatively, batch operations can be used.

```factor
DL{ } clone {
    [ [ { 1 2 3 } ] dip push-all-front ] ! push all from sequence
    [ .                                ] ! DL{ 3 2 1 }
    [ [ drop ] slurp-deque             ] ! pop and discard all
    [ deque-empty? .                   ] ! t
} cleave
```



## Fantom


Using definition of Queue in: [[Queue/Definition]] task.


```fantom

class Main
{
  public static Void main ()
  {
    q := Queue()
    q.push (1)
    q.push ("a")
    echo ("Is empty? " + q.isEmpty)
    echo ("Element: " + q.pop)
    echo ("Element: " + q.pop)
    echo ("Is empty? " + q.isEmpty)
    try { q.pop } catch (Err e) { echo (e.msg) }
  }
}

```


Output:

```txt

Is empty? false
Element: 1
Element: a
Is empty? true
queue is empty

```



## Forth

Forth is a low level language the runs on a virtual machine with 2 stacks. One stack for Parameters and the second is the call/return stack. Coding begins at an almost assembler like level but the work results in a higher level language.

In this demonstration code we show a feature of Forth that is one of the earliest examples of simple object creation using the word CREATE.  With this mechanism we create a queue constructor that can build queue data structures of different sizes. Then we create two operators that enqueue a byte and dequeue a byte. The queue's address is passed to these operators on the data stack.
<BR>Implementations in other languages or libraries might use a linked list that could potentially consume all memory. Creating a static circular queue is more typical for Forth where it is commonly used in embedded high reliability systems. The code here makes use of the fact that if the queue size is a power of 2, the circular wrap around can be implemented without an IF statement, and uses logical AND with binary mask to wrap around.
<BR> NOTE: We also used a more Forth like naming convention QC@ (queue char fetch) and QC! (queue char store) rather than PUSH and POP which as stack users we felt were more appropriate for a Stack than a Queue.

A simpler implementation, where you only need 1 queue can be seen here: http://rosettacode.org/wiki/Queue/Definition#Forth<BR>
And a Forth version using some new features of Forth 2012, dynamic memory allocation and a linked list can be seen here:<BR> http://rosettacode.org/wiki/Queue/Definition#Linked_list_version

<lang>: cqueue: ( n -- <text>)
    create                                                 \ compile time: build the data structure in memory
        dup
        dup 1- and abort" queue size must be power of 2"
        0 ,                                                \ write pointer "HEAD"
        0 ,                                                \ read  pointer "TAIL"
        0 ,                                                \ byte counter
        dup 1- ,                                           \ mask value used for wrap around
        allot ;                                            \ run time: returns the address of this data structure

\ calculate offsets into the queue data structure
: ->head ( q -- adr )      ;                               \ syntactic sugar
: ->tail ( q -- adr ) cell+   ;
: ->cnt  ( q -- adr ) 2 cells +   ;
: ->msk  ( q -- adr ) 3 cells +   ;
: ->data ( q -- adr ) 4 cells +   ;

: head++ ( q -- )                                         \ circular increment head pointer of a queue
         dup >r ->head @ 1+  r@ ->msk @ and r> ->head ! ;

: tail++ ( q -- )                                         \ circular increment tail pointer of a queue
        dup >r  ->tail @ 1+  r@ ->msk @ and r> ->tail ! ;

: qempty ( q -- flag)
        dup ->head off   dup ->tail off  dup ->cnt  off    \ reset all fields to "off" (zero)
        ->cnt @ 0=  ;                                      \ per the spec qempty returns a flag

: cnt=msk?   ( q -- flag)  dup >r  ->cnt @ r> ->msk @ = ;
: ?empty     ( q -- )     ->cnt @ 0=  abort" queue is empty" ;
: ?full      ( q -- )     cnt=msk? abort" queue is full" ;
: 1+!   ( adr -- )  1 swap +! ;                            \ increment contents of adr
: 1-!   ( adr -- ) -1 swap +! ;				   \ decrement contents of adr

: qc@    ( queue -- char )                                 \ fetch next char in queue
       dup >r ?empty                                       \ abort if empty
       r@ ->cnt 1-!                                        \ decr. the counter
       r@ tail++
       r@ ->data  r> ->tail @ + c@ ;                       \ calc. address and fetch the byte


: qc!    ( char queue -- )
       dup >r ?full                                        \ abort if q full
       r@ ->cnt 1+!                                        \ incr. the counter
       r@ head++
       r@ ->data  r> ->head @ + c! ;                       \ data+head = adr, and store the char
```


Create 2 Queues and test the operators at the Forth console interactively

```txt

64 cqueue: XQ ok
32 cqueue: YQ ok

char A XQ qc! ok
char B XQ qc! ok
char C XQ qc! ok

XQ qc@ emit A ok
XQ qc@ emit B ok
XQ qc@ emit C ok
XQ qc@ emit
   ^^^
Queue is empty

YQ qc@ emit
   ^^^
Queue is empty

```


=== Version for the [[FIFO#Linked_list_version|Linked List implementation]] ===


```forth

make-queue constant q1
make-queue constant q2
q1 empty? .
5 q1 enqueue
q1 empty? .
7 q1 enqueue
9 q1 enqueue
q2 empty? .
3 q2 enqueue
q2 empty? .
q1 dequeue .
q1 dequeue .
q1 dequeue .
q1 empty? .
q2 dequeue .
q2 empty? .

```



## Fortran

{{works with|Fortran|90 and later}}


```fortran
module fifo_nodes
  type fifo_node
     integer :: datum
     ! the next part is not variable and must be present
     type(fifo_node), pointer :: next
     logical :: valid
  end type fifo_node
end module fifo_nodes

program FIFOTest
  use fifo
  implicit none

  type(fifo_head) :: thehead
  type(fifo_node), dimension(5) :: ex, xe
  integer :: i

  call new_fifo(thehead)

  do i = 1, 5
     ex(i)%datum = i
     call fifo_enqueue(thehead, ex(i))
  end do

  i = 1
  do
     call fifo_dequeue(thehead, xe(i))
     print *, xe(i)%datum
     i = i + 1
     if ( fifo_isempty(thehead) ) exit
  end do

end program FIFOTest
```




## FreeBASIC

As FreeBASIC does not have a built-in Queue type, I am reusing the type I wrote for the [[Queue/Definition]] task:

```freebasic
' FB 1.05.0 Win64

#Include "queue_rosetta.bi"  '' include macro-based generic Queue type used in earlier task

Declare_Queue(String) '' expand Queue type for Strings

Dim stringQueue As Queue(String)
With stringQueue  '' push some strings into the Queue
  .push("first")
  .push("second")
  .push("third")
  .push("fourth")
  .push("fifth")
End With
Print "Number of Strings in the Queue :" ; stringQueue.count
Print "Capacity of string Queue       :" ; stringQueue.capacity
Print
' now pop them
While Not stringQueue.empty
  Print stringQueue.pop(); " popped"
Wend
Print
Print "Number of Strings in the Queue :" ; stringQueue.count
Print "Capacity of string Queue       :" ; stringQueue.capacity   '' capacity should be unchanged
Print "Is Queue empty now             : "; stringQueue.empty
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

Number of Strings in the Queue : 5
Capacity of string Queue       : 8

first popped
second popped
third popped
fourth popped
fifth popped

Number of Strings in the Queue : 0
Capacity of string Queue       : 8
Is Queue empty now             : true

```



## Go


### With Queue/Definition code

Solution using [[Queue/Definition#Go|package]] from the [[Queue/Definition]] task:

```go
package main

import (
    "fmt"
    "queue"
)

func main() {
    q := new(queue.Queue)
    fmt.Println("empty?", q.Empty())

    x := "black"
    fmt.Println("push", x)
    q.Push(x)

    fmt.Println("empty?", q.Empty())
    r, ok := q.Pop()
    if ok {
        fmt.Println(r, "popped")
    } else {
        fmt.Println("pop failed")
    }

    var n int
    for _, x := range []string{"blue", "red", "green"} {
        fmt.Println("pushing", x)
        q.Push(x)
        n++
    }

    for i := 0; i < n; i++ {
        r, ok := q.Pop()
        if ok {
            fmt.Println(r, "popped")
        } else {
            fmt.Println("pop failed")
        }
    }
}
```

Output:

```txt

empty? true
push black
empty? false
black popped
pushing blue
pushing red
pushing green
blue popped
red popped
green popped

```



### With channels

Go buffered channels are FIFO, and better, are concurrency-safe (if you have an application for that.)  Code below is same as code above only with Go channels rather than the home made queue implementation.  Note that you don't have to start concurrent goroutines to use channels, they are useful all on their own.  Other differences worth noting:  Buffered channels are not dynamically resizable.  This is a good thing, as queues that can grow without limit allow ugly bugs that consume memory and grind to a halt.  Also blocking operations (as seen here with push) are probably a bad idea with a single goroutine.  Much safer to use non-blocking operations that handle success and failure (the way pop is done here.)

```go
package main

import "fmt"

func main() {
    q := make(chan string, 3)
    fmt.Println("empty?", len(q) == 0)

    x := "black"
    fmt.Println("push", x)
    q <- x

    fmt.Println("empty?", len(q) == 0)
    select {
    case r := <-q:
        fmt.Println(r, "popped")
    default:
        fmt.Println("pop failed")
    }

    var n int
    for _, x := range []string{"blue", "red", "green"} {
        fmt.Println("pushing", x)
        q <- x
        n++
    }

    for i := 0; i < n; i++ {
        select {
        case r := <-q:
            fmt.Println(r, "popped")
        default:
            fmt.Println("pop failed")
        }
    }
}
```


### With linked lists


```go
package main

import (
    "fmt"
    "container/list"
)

func main() {
    q := list.New()
    fmt.Println("empty?", q.Len() == 0)

    x := "black"
    fmt.Println("push", x)
    q.PushBack(x)

    fmt.Println("empty?", q.Len() == 0)
    if e := q.Front(); e != nil {
        r := q.Remove(e)
        fmt.Println(r, "popped")
    } else {
        fmt.Println("pop failed")
    }

    var n int
    for _, x := range []string{"blue", "red", "green"} {
        fmt.Println("pushing", x)
        q.PushBack(x)
        n++
    }

    for i := 0; i < n; i++ {
        if e := q.Front(); e != nil {
            r := q.Remove(e)
            fmt.Println(r, "popped")
        } else {
            fmt.Println("pop failed")
        }
    }
}
```



## Groovy


Solution:

```groovy
def q = new LinkedList()
```


Test:

```groovy
assert q.empty
println q
// "push" adds to end of "queue" list
q.push('Stuart')
println q
assert !q.empty
// "add" adds to end of "queue" list
q.add('Pete')
println q
assert !q.empty
// left shift operator ("<<") adds to end of "queue" list
q << 'John'
println q
assert !q.empty
// add assignment ("+=") adds the list elements
// to the end of the "queue" list in list order
q += ['Paul', 'George']
println q
assert !q.empty
// "poll" removes and returns the first element in the
// "queue" list ("pop" exists for Groovy lists, but it
// removes and returns the LAST element for "Stack"
// semantics). "poll" only exists in objects that
// implement java.util.Queue, like java.util.LinkedList
assert q.poll() == 'Stuart'
println q
assert !q.empty
assert q.poll() == 'Pete'
println q
assert !q.empty
q << 'Ringo'
println q
assert !q.empty
assert q.poll() == 'John'
println q
assert !q.empty
assert q.poll() == 'Paul'
println q
assert !q.empty
assert q.poll() == 'George'
println q
assert !q.empty
assert q.poll() == 'Ringo'
println q
assert q.empty
assert q.poll() == null
```


Output:

```txt
[]
[Stuart]
[Stuart, Pete]
[Stuart, Pete, John]
[Stuart, Pete, John, Paul, George]
[Pete, John, Paul, George]
[John, Paul, George]
[John, Paul, George, Ringo]
[Paul, George, Ringo]
[George, Ringo]
[Ringo]
[]
```



## Haskell

Running the code from [[Queue/Definition#Haskell]] through GHC's interpreter.


```Haskell

Prelude> :l fifo.hs
[1 of 1] Compiling Main             ( fifo.hs, interpreted )
Ok, modules loaded: Main.
*Main> let q = emptyFifo
*Main> isEmpty q
True
*Main> let q' = push q 1
*Main> isEmpty q'
False
*Main> let q'' = foldl push q' [2..4]
*Main> let (v,q''') = pop q''
*Main> v
Just 1
*Main> let (v',q'''') = pop q'''
*Main> v'
Just 2
*Main> let (v'',q''''') = pop q''''
*Main> v''
Just 3
*Main> let (v''',q'''''') = pop q'''''
*Main> v'''
Just 4
*Main> let (v'''',q''''''') = pop q''''''
*Main> v''''
Nothing

```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon provide built-in queue and stack functions.

```Icon
procedure main(arglist)
queue := []
write("Usage:\nqueue x x x - x - - - - -\n\t- pops elements\n\teverything else pushes")
write("Queue is:")
every x := !arglist do {
   case x of {
      "-"     : pop(queue)  | write("pop(empty) failed.")    # pop if the next arglist[i] is a -
      default : put(queue,x)                                 # push arglist[i]
      }
   if empty(queue) then writes("empty")
   else every writes(!queue," ")
   write()
   }
end

procedure empty(X)        #: fail if X is not empty
if *X = 0 then return
end
```


Sample output:
```txt
queue - 1 3 4 5 6 - - - - - - - -
Usage:
queue x x x - x - - - - -
        - pops elements
        everything else pushes
Queue is:
pop(empty) failed.
empty
1
1 3
1 3 4
1 3 4 5
1 3 4 5 6
3 4 5 6
4 5 6
5 6
6
empty
pop(empty) failed.
empty
pop(empty) failed.
empty
pop(empty) failed.
empty

```



## J

Using object-oriented FIFO queue implementation from [[FIFO#J|FIFO]]

This is an interactive J session:


```j
   queue=: conew 'fifo'
   isEmpty__queue ''
1
   push__queue 9
9
   push__queue 8
8
   push__queue 7
7
   isEmpty__queue ''
0
   pop__queue ''
9
   pop__queue ''
8
   pop__queue ''
7
   isEmpty__queue ''
1
```


Using function-level FIFO queue implementation from [[FIFO#J|FIFO]]

This is an interactive J session:

```j
   is_empty make_empty _
1
   first_named_state =: push 9 onto make_empty _
   newer_state =: push 8 onto first_named_state
   this_state =: push 7 onto newer_state
   is_empty this_state
0
   tell_queue this_state
9 8 7
   tell_atom pop this_state
9
   tell_atom pop pop this_state
8
   tell_atom pop pop pop this_state
7
   is_empty pop pop pop this_state
1
```



## Java

{{works with|Java|1.5+}}
LinkedList can always be used as a queue or stack, but not in conjunction with the Stack object provided by Java. To use a LinkedList as a stack, use the <tt>push</tt> and <tt>pop</tt> methods. A LinkedList can also be used as a double-ended queue (deque); LinkedList has implemented the Deque interface since Java 1.6+.

```java
import java.util.LinkedList;
import java.util.Queue;
...
Queue<Integer> queue = new LinkedList<Integer>();
System.out.println(queue.isEmpty());      // empty test - true
// queue.remove();       // would throw NoSuchElementException
queue.add(1);
queue.add(2);
queue.add(3);
System.out.println(queue);                // [1, 2, 3]
System.out.println(queue.remove());       // 1
System.out.println(queue);                // [2, 3]
System.out.println(queue.isEmpty());      // false
```


You can also use "offer" and "poll" methods instead of "add" and "remove", respectively. They indicate errors with the return value instead of throwing an exception.

{{works with|Java|1.4}}

```java
import java.util.LinkedList;
...
LinkedList queue = new LinkedList();
System.out.println(queue.isEmpty());      // empty test - true
queue.add(new Integer(1));
queue.add(new Integer(2));
queue.add(new Integer(3));
System.out.println(queue);                // [1, 2, 3]
System.out.println(queue.removeFirst());  // 1
System.out.println(queue);                // [2, 3]
System.out.println(queue.isEmpty());      // false
```



## JavaScript

JavaScript arrays can be used as FIFOs.

```javascript
var f = new Array();
print(f.length);
f.push(1,2);         // can take multiple arguments
f.push(3);
f.shift();
f.shift();
print(f.length);
print(f.shift())
print(f.length == 0);
print(f.shift());
```


outputs:

```txt
0
1
3
true
undefined
```



## Julia

{{works with|Julia|0.6}}


```julia
using DataStructures

queue = Queue(String)
@show enqueue!(queue, "foo")
@show enqueue!(queue, "bar")
@show dequeue!(queue) # -> foo
@show dequeue!(queue) # -> bar
```



## Kotlin

The related [[Queue/Definition]] task, where we wrote our own Queue class, intimated that we should use the language's built-in queue for this task so that's what I'm going to do here, using Java collection types as Kotlin doesn't have a Queue type in its standard library:

```scala
// version 1.1.2

import java.util.*

fun main(args: Array<String>) {
    val q: Queue<Int> = ArrayDeque<Int>()
    (1..5).forEach { q.add(it) }
    println(q)
    println("Size of queue = ${q.size}")
    print("Removing: ")
    (1..3).forEach { print("${q.remove()} ") }
    println("\nRemaining in queue: $q")
    println("Head element is now ${q.element()}")
    q.clear()
    println("After clearing, queue is ${if(q.isEmpty()) "empty" else "not empty"}")
    try {
        q.remove()
    }
    catch (e: NoSuchElementException) {
        println("Can't remove elements from an empty queue")
    }
}
```


{{out}}

```txt

[1, 2, 3, 4, 5]
Size of queue = 5
Removing: 1 2 3
Remaining in queue: [4, 5]
Head element is now 4
After clearing, queue is empty
Can't remove elements from an empty queue

```



## Lasso

Lasso has a queue type that uses the following for the operators:

```txt

 push: queue->insert
  pop: queue->get
empty: queue->size == 0

```

Example:

```lasso

local(queue) = queue
#queue->size
// => 0

#queue->insert('a')
#queue->insert('b')
#queue->insert('c')
#queue->size
// => 3

loop(#queue->size) => {
  stdoutnl(#queue->get)
}
// =>
// a
// b
// c

#queue->size == 0
// => true

```




## Logo

{{works with|UCB Logo}}
UCB Logo comes with a protocol for treating lists as queues.


```logo
make "fifo []
print empty? :fifo    ; true
queue "fifo 1
queue "fifo 2
queue "fifo 3
show :fifo            ; [1 2 3]
print dequeue "fifo   ; 1
show :fifo            ; [2 3]
print empty? :fifo    ; false
```


## Lua

Uses the queue-definition given at [[Queue/Definition#Lua]]

```lua
q = Queue.new()
Queue.push( q, 5 )
Queue.push( q, "abc" )

while not Queue.empty( q ) do
    print( Queue.pop( q ) )
end
```


One can also just use a regular Lua table (shown here in interactive mode):


```lua>
 -- create queue:
> q = {}
> -- push:
> q[#q+1] = "first"
> q[#q+1] = "second"
> q[#q+1] = "third"
> -- pop:
> =table.remove(q, 1)
first
> =table.remove(q, 1)
second
> =table.remove(q, 1)
third
> -- empty?
> =#q == 0
true
```



## Maple

There are more builtin operations like reverse(), length(),etc.

```Maple
q := queue[new]();
queue[enqueue](q,1);
queue[enqueue](q,2);
queue[enqueue](q,3);
queue[empty](q);
>>>false
queue[dequeue](q);
>>>1
queue[dequeue](q);
>>>2
queue[dequeue](q);
>>>3
queue[empty](q);
>>>true
```




## Mathematica


```Mathematica
Empty[a_] := If[Length[a] == 0, True, False]
SetAttributes[Push, HoldAll]; Push[a_, elem_] := AppendTo[a, elem]
SetAttributes[Pop, HoldAllComplete]; Pop[a_] := If[EmptyQ[a], False, b = First[a]; Set[a, Most[a]]; b]

Queue = {}
-> {}
Empty[Queue]
-> True
Push[Queue, "1"]
-> {"1"}
EmptyQ[Queue]
->False
Pop[Queue]
->1
Pop[Queue]
->False
```



## Nemerle

The <tt>Nemerle.Collections</tt> namespace contains an implementation of a Queue.

```Nemerle
mutable q = Queue(); // or use immutable version as per Haskell example
def empty = q.IsEmpty(); // true at this point
q.Push(empty); // or Enqueue(), or Add()
def a = q.Pop(); // or Dequeue() or Take()
```



## NetRexx

This example demonstrates the <code>push</code>, <code>pop</code> and <code>empty</code> operations from an implementation of a queue as specified for the task.

The demonstration employs an in-line deployment of a queue object having as it's underlying implementation a <code>java.util.Deque</code> interface instanciated as a <code>java.util.ArrayDeque</code>.  Typically this queue implementation would reside outside of the demonstration program and be imported at run-time rather than within the body of this source.

```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

-- Queue Usage Demonstration Program -------------------------------------------
method main(args = String[]) public constant
  kew = RCQueueImpl()
  do
    say kew.pop()
  catch ex = IndexOutOfBoundsException
    say ex.getMessage
    say
  end

  melancholyDane = ''
  melancholyDane[0] = 4
  melancholyDane[1] = 'To be'
  melancholyDane[2] = 'or'
  melancholyDane[3] = 'not to be?'
  melancholyDane[4] = 'That is the question.'

  loop p_ = melancholyDane[0] to 1 by -1
    kew.push(melancholyDane[p_])
    end p_

  loop while \kew.empty
    popped = kew.pop
    say popped '\-'
    end
  say; say

  -- demonstrate stowing something other than a text string in the queue
  kew.push(melancholyDane)
  md = kew.pop
  loop l_ = 1 to md[0]
    say md[l_] '\-'
    end l_
  say

  return

-- Queue implementation --------------------------------------------------------
class RCQueueImpl
  properties private
    qqq = Deque

method RCQueueImpl() public
  qqq = ArrayDeque()
  return

method push(stuff) public
  qqq.push(stuff)
  return

method pop() public returns Rexx signals IndexOutOfBoundsException
  if qqq.isEmpty then signal IndexOutOfBoundsException('The queue is empty')
  return Rexx qqq.pop()

method empty() public binary returns boolean
  return qqq.isEmpty

method isTrue public constant binary returns boolean
  return 1 == 1

method isFalse public constant binary returns boolean
  return \isTrue

```

;Output

```txt

The queue is empty

To be or not to be? That is the question.

To be or not to be? That is the question.

```



## Nim


```nim
import queues

var deq: TQueue[int] = initQueue[int]()

deq.enqueue(26)
deq.add(99)     # same as enqueue()
deq.enqueue(2)
echo("Dequeue size: ", deq.len())
echo("De-queue: ", deq.dequeue())
echo("De-queue: ", deq.dequeue())
echo("De-queue: ", deq.dequeue())
#echo("De-queue: ", deq.dequeue())     # dequeue an empty dequeue raises [EAssertionFailed]
```

{{out}}

```txt
Dequeue size: 3
De-queue: 26
De-queue: 99
De-queue: 2
```



## Objeck


```objeck

class Test {
  function : Main(args : String[]) ~ Nil {
    q := Struct.IntQueue->New();
    q->Add(1);
    q->Add(2);
    q->Add(3);

    q->Remove()->PrintLine();
    q->Remove()->PrintLine();
    q->Remove()->PrintLine();

    q->IsEmpty()->PrintLine();
  }
}

```



## OCaml


```ocaml
# let q = Queue.create ();;
val q : '_a Queue.t = <abstr>
# Queue.is_empty q;;
- : bool = true
# Queue.add 1 q;;
- : unit = ()
# Queue.is_empty q;;
- : bool = false
# Queue.add 2 q;;
- : unit = ()
# Queue.add 3 q;;
- : unit = ()
# Queue.peek q;;
- : int = 1
# Queue.length q;;
- : int = 3
# Queue.iter (Printf.printf "%d, ") q; print_newline ();;
1, 2, 3,
- : unit = ()
# Queue.take q;;
- : int = 1
# Queue.take q;;
- : int = 2
# Queue.peek q;;
- : int = 3
# Queue.length q;;
- : int = 1
# Queue.add 4 q;;
- : unit = ()
# Queue.take q;;
- : int = 3
# Queue.peek q;;
- : int = 4
# Queue.take q;;
- : int = 4
# Queue.is_empty q;;
- : bool = true
```



## Oforth


Using FIFO implementation :


```oforth
: testQueue
| q i |
   Queue new ->q
   20 loop: i [ i q push ]
   while ( q empty not ) [ q pop . ] ;
```



## ooRexx

ooRexx includes a built-in queue class.

```ooRexx

q = .queue~new      -- create an instance
q~queue(3)          -- adds to the end, but this is at the front
q~push(1)           -- push on the front
q~queue(2)          -- add to the end
say q~pull q~pull q~pull q~isempty  -- should display all and be empty

```

Output:

```txt

1 3 2 1

```



## Oz


```oz
declare
  [Queue] = {Link ['x-oz://system/adt/Queue.ozf']}
  MyQueue = {Queue.new}
in
  {MyQueue.isEmpty} = true
  {MyQueue.put foo}
  {MyQueue.put bar}
  {MyQueue.put baz}
  {MyQueue.isEmpty} = false
  {Show {MyQueue.get}}  %% foo
  {Show {MyQueue.get}}  %% bar
  {Show {MyQueue.get}}  %% baz
```



## Perl

Perl has built-in support to these operations:

```perl
@queue = (); # we will simulate a queue in a array

push @queue, (1..5); # enqueue numbers from 1 to 5

print shift @queue,"\n"; # dequeue

print "array is empty\n" unless @queue; # is empty ?

print $n while($n = shift @queue); # dequeue all
print "\n";
print "array is empty\n" unless @queue; # is empty ?
```

Output:
<lang>1
2345
array is empty
```



## Perl 6


Perl 6 maintains the same list operators of Perl, for this task, the operations are:
<lang>push (aka enqueue) -- @list.push
pop (aka dequeue)  -- @list.shift
empty              -- !@list.elems
```

but there's also @list.pop which removes a item from the end,
and @list.unshift which add a item on the start of the list.<br/>
Example:

```perl6>my @queue = < a
;

@queue.push('b', 'c'); # [ a, b, c ]

say @queue.shift; # a
say @queue.pop; # c

say @queue.perl; # [ b ]
say @queue.elems; # 1

@queue.unshift('A'); # [ A, b ]
@queue.push('C'); # [ A, b, C ]
```



## Phix

Using the implementation from [[Queue/Definition#Phix|Queue/Definition]]

```Phix
?empty()                -- 1
push(5)
?empty()                -- 0
push(6)
?pop()                  -- 5
?pop()                  -- 6
?empty()                -- 1
```



## PHP

{{works with|PHP|5.3+}}

```php
<?php
$queue = new SplQueue;
echo $queue->isEmpty() ? 'true' : 'false', "\n";  // empty test - returns true
// $queue->dequeue();                             // would raise RuntimeException
$queue->enqueue(1);
$queue->enqueue(2);
$queue->enqueue(3);
echo $queue->dequeue(), "\n";                     // returns 1
echo $queue->isEmpty() ? 'true' : 'false', "\n";  // returns false
?>
```



## PicoLisp

Using the implementation from [[FIFO]]:

```PicoLisp
(println (fifo 'Queue))    # Retrieve the number '1'
(println (fifo 'Queue))    # Retrieve an internal symbol 'abc'
(println (fifo 'Queue))    # Retrieve a transient symbol "abc"
(println (fifo 'Queue))    # and a list (abc)
(println (fifo 'Queue))    # Queue is empty -> NIL
```

Output:

```txt
1
abc
"abc"
(a b c)
NIL
```



## PL/I


```PL/I

test: proc options (main);


   /* To implement a queue. */
   define structure
      1 node,
         2 value fixed,
         2 link handle(node);
   declare (head, tail, t) handle (node);
   declare null builtin;
   declare i fixed binary;

   head, tail = bind(:node, null:);

   do i = 1 to 10; /* Add ten items to the tail of the queue. */
      if head = bind(:node, null:) then
         do;
            head,tail = new(:node:);
            get list (head => value);
            put skip list (head => value);
            head => link = bind(:node, null:); /* A NULL link */
         end;
      else
         do;
            t = new(:node:);
            tail => link = t; /* Point the tail to the new node. */
            tail = t;
            tail => link = bind(:node, null:); /* Set the tail link to NULL */
            get list (tail => value) copy;
            put skip list (tail => value);
         end;
   end;

   /* Pop all the items in the queue. */
   put skip list ('The queue has:');
   do while (head ^= bind(:node, null:));
      put skip list (head => value);
      head = head => link;
   end;
end test;

```

The output:
<lang>
       1
       3
       5
       7
       9
      11
      13
      15
      17
      19
The queue has:
       1
       3
       5
       7
       9
      11
      13
      15
      17
      19

```



## PostScript

{{libheader|initlib}}

```postscript

 [1 2 3 4 5] 6 exch tadd
 = [1 2 3 4 5 6]
 uncons
 = 1 [2 3 4 5 6]
 [] empty?
 =true

```



## PowerShell


{{works with|PowerShell|4.0}}


```PowerShell

[System.Collections.ArrayList]$queue = @()
# isEmpty?
if ($queue.Count -eq 0) {
    "isEmpty? result : the queue is empty"
} else {
    "isEmpty? result : the queue is not empty"
}
"the queue contains : $queue"
$queue += 1                    # push
"push result : $queue"
$queue += 2                    # push
$queue += 3                    # push
"push result : $queue"

$queue.RemoveAt(0)             # pop
"pop result : $queue"

$queue.RemoveAt(0)             # pop
"pop result : $queue"

if ($queue.Count -eq 0) {
    "isEmpty? result : the queue is empty"
} else {
    "isEmpty? result : the queue is not empty"
}
"the queue contains : $queue"

```

<b>Output:</b>

```txt

isEmpty? result : the queue is empty
the queue contains :
push result : 1
push result : 1 2 3
pop result : 2 3
pop result : 3
isEmpty? result : the queue is not empty
the queue contains : 3

```




### PowerShell using the .NET Queue Class

Declare a new queue:

```PowerShell

$queue = New-Object -TypeName System.Collections.Queue
#or
$queue = [System.Collections.Queue] @()

```

Show the methods and properties of the queue object:

```PowerShell

Get-Member -InputObject $queue

```

{{Out}}

```txt

   TypeName: System.Collections.Queue

Name           MemberType Definition
----           ---------- ----------
Clear          Method     void Clear()
Clone          Method     System.Object Clone(), System.Object ICloneable.Clone()
Contains       Method     bool Contains(System.Object obj)
CopyTo         Method     void CopyTo(array array, int index), void ICollection.CopyTo(array array, int index)
Dequeue        Method     System.Object Dequeue()
Enqueue        Method     void Enqueue(System.Object obj)
Equals         Method     bool Equals(System.Object obj)
GetEnumerator  Method     System.Collections.IEnumerator GetEnumerator(), System.Collections.IEnumerator IEnumerable.GetEnumerator()
GetHashCode    Method     int GetHashCode()
GetType        Method     type GetType()
Peek           Method     System.Object Peek()
ToArray        Method     System.Object[] ToArray()
ToString       Method     string ToString()
TrimToSize     Method     void TrimToSize()
Count          Property   int Count {get;}
IsSynchronized Property   bool IsSynchronized {get;}
SyncRoot       Property   System.Object SyncRoot {get;}

```

Put some stuff in the queue:

```PowerShell

1,2,3 | ForEach-Object {$queue.Enqueue($_)}

```

Take a peek at the head of the queue:

```PowerShell

$queue.Peek()

```

{{Out}}

```txt

1

```

Pop the head of the queue:

```PowerShell

$queue.Dequeue()

```

{{Out}}

```txt

1

```

Clear the queue:

```PowerShell

$queue.Clear()

```

Test if queue is empty:

```PowerShell

if (-not $queue.Count) {"Queue is empty"}

```

{{Out}}

```txt

Queue is empty

```



## Prolog

Works with SWI-Prolog.


```Prolog
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% definitions of queue
empty(U-V) :-
    unify_with_occurs_check(U, V).

push(Queue, Value, NewQueue) :-
    append_dl(Queue, [Value|X]-X, NewQueue).


pop([X|V]-U, X, V-U) :-
    \+empty([X|V]-U).



append_dl(X-Y, Y-Z, X-Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% use of queue
queue :-
    % create an empty queue
    empty(Q),
    format('Create queue ~w~n~n', [Q]),

    % add numbers 1 and 2
    write('Add numbers 1 and 2 : '),
    push(Q, 1, Q1),
    push(Q1, 2, Q2),

    % display queue
    format('~w~n~n', [Q2]),

    % pop element
    pop(Q2, V, Q3),

    % display results
    format('Pop : Value ~w Queue : ~w~n~n', [V, Q3]),

    % test the queue
    write('Test of the queue : '),
    (   empty(Q3) -> writeln('Queue empy'); writeln('Queue not empty')), nl,

    % pop the elements
    write('Pop the queue : '),
    pop(Q3, V1, Q4),
    format('Value ~w Queue : ~w~n~n', [V1, Q4]),

    write('Pop the queue : '),
    pop(Q4, _V, _Q5).

```

Output :

```txt
?- queue.
Create queue _G132-_G132

Add numbers 1 and 2 : [1,2|_G148]-_G148

Pop : Value 1 Queue : [2|_G148]-_G148

Test of the queue : Queue not empty

Pop the queue : Value 2 Queue : _G148-_G148

Pop the queue :
false.

```


## PureBasic


```PureBasic
NewList MyStack()

Procedure Push(n)
  Shared MyStack()
  LastElement(MyStack())
  AddElement(MyStack())
  MyStack()=n
EndProcedure

Procedure Pop()
  Shared MyStack()
  Protected n
  If FirstElement(MyStack())  ; e.g. Stack not empty
    n=MyStack()
    DeleteElement(MyStack(),1)
  EndIf
  ProcedureReturn n
EndProcedure

Procedure Empty()
  Shared MyStack()
  If  ListSize(MyStack())=0
    ProcedureReturn #True
  EndIf
  ProcedureReturn #False
EndProcedure

;----   Example of implementation ----
Push(3)
Push(1)
Push(4)
Push(1)
Push(5)
While Not Empty()
  Debug Pop()
Wend

```


'''Outputs
 3
 1
 4
 1
 5


## Python


```python
import Queue
my_queue = Queue.Queue()
my_queue.put("foo")
my_queue.put("bar")
my_queue.put("baz")
print my_queue.get()  # foo
print my_queue.get()  # bar
print my_queue.get()  # baz
```



## Racket


```Racket
#lang racket

(require data/queue)

(define queue (make-queue))

(enqueue! queue 'black)
(queue-empty? queue) ; #f

(enqueue! queue 'red)
(enqueue! queue 'green)

(dequeue! queue) ; 'black
(dequeue! queue) ; 'red
(dequeue! queue) ; 'green

(queue-empty? queue) ; #t
```



## REBOL


See [[FIFO#REBOL]] for implementation. Example repeated here for completeness.


```REBOL
; Create and populate a FIFO:

q: make fifo []
q/push 'a
q/push 2
q/push USD$12.34              ; Did I mention that REBOL has 'money!' datatype?
q/push [Athos Porthos Aramis] ; List elements pushed on one by one.
q/push [[Huey Dewey Lewey]]   ; This list is preserved as a list.

; Dump it out, with narrative:

print rejoin ["Queue is "  either q/empty [""]["not "]  "empty."]
while [not q/empty][print ["  " q/pop]]
print rejoin ["Queue is "  either q/empty [""]["not "]  "empty."]
print ["Trying to pop an empty queue yields:" q/pop]
```


Output:


```txt
Queue is not empty.
   a
   2
   USD$12.34
   Athos
   Porthos
   Aramis
   Huey Dewey Lewey
Queue is empty.
Trying to pop an empty queue yields: none
```



## REXX

The REXX language was developed under IBM VM/CMS operating system, and CMS had a stack mechanism built-into the

operating system, so REXX utilized that resource.

The   '''queue'''   instruction adds an entry to the bottom of the stack (FIFO),

the   '''push'''   instruction adds an entry to the top of the stack (LIFO).

The   '''queued'''   function returns the number of entries in the stack.

The   '''pull'''   or   '''parse pull'''   removes an entry from the top of the stack.

There are other instructions to manipulate the stack by "creating" multiple (named) stacks.

The entries in the stack may be anything, including "nulls".

```rexx
/*REXX program demonstrates four  queueing  operations:   push,  pop,  empty,  query.   */
say '══════════════════════════════════ Pushing five values to the stack.'
        do j=1  for 4                            /*a  DO  loop to  PUSH  four values.   */
        call push  j * 10                        /*PUSH   (aka:   enqueue to the stack).*/
        say 'pushed value:'    j * 10            /*echo the pushed value.               */
        if j\==3  then iterate                   /*Not equal 3?   Then use a new number.*/
        call push                                /*PUSH   (aka:   enqueue to the stack).*/
        say 'pushed a "null" value.'             /*echo what was  pushed  to the stack. */
        end   /*j*/
say '══════════════════════════════════ Quering the stack  (number of entries).'
        say  queued()  ' entries in the stack.'
say '══════════════════════════════════ Popping all values from the stack.'
        do k=1  while  \empty()                  /*EMPTY (returns  TRUE  [1]  if empty).*/
        call pop                                 /*POP   (aka:  dequeue from the stack).*/
        say k': popped value='  result           /*echo the popped value.               */
        end   /*k*/
say '══════════════════════════════════ The stack is now empty.'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
push:   queue arg(1);              return        /*(The  REXX  QUEUE   is FIFO.)        */
pop:    procedure;  parse pull x;  return x      /*REXX   PULL   removes a stack item.  */
empty:  return queued()==0                       /*returns the status of the stack.     */
```

{{out|output|text=:}}

```txt

══════════════════════════════════ Pushing five values to the stack.
pushed value: 10
pushed value: 20
pushed value: 30
pushed a "null" value.
pushed value: 40
══════════════════════════════════ Quering the stack  (number of entries).
5  entries in the stack.
══════════════════════════════════ Popping all values from the stack.
1: popped value= 10
2: popped value= 20
3: popped value= 30
4: popped value=
5: popped value= 40
══════════════════════════════════ The stack is now empty.

```



## Ruby

Sample usage at [[FIFO#Ruby]]


## Scala


```scala
val q=scala.collection.mutable.Queue[Int]()
println("isEmpty = " + q.isEmpty)
try{q dequeue} catch{case _:java.util.NoSuchElementException => println("dequeue(empty) failed.")}
q enqueue 1
q enqueue 2
q enqueue 3
println("queue   = " + q)
println("front   = " + q.front)
println("dequeue = " + q.dequeue)
println("dequeue = " + q.dequeue)
println("isEmpty = " + q.isEmpty)
```

Output:

```txt
isEmpty = true
dequeue(empty) failed.
queue   = Queue(1, 2, 3)
front   = 1
dequeue = 1
dequeue = 2
isEmpty = false
```



## Sidef

Using the class defined at [[FIFO#Sidef]]

```ruby
var f = FIFO();
say f.empty;        # true
f.push('foo');
f.push('bar', 'baz');
say f.pop;          # foo
say f.empty;        # false

var g = FIFO('xxx', 'yyy');
say g.pop;          # xxx
say f.pop;          # bar
```



## Standard ML

{{works with|SML/NJ}}
; Functional interface

```sml
- open Fifo;
opening Fifo
  datatype 'a fifo = ...
  exception Dequeue
  val empty : 'a fifo
  val isEmpty : 'a fifo -> bool
  val enqueue : 'a fifo * 'a -> 'a fifo
  val dequeue : 'a fifo -> 'a fifo * 'a
  val next : 'a fifo -> ('a * 'a fifo) option
  val delete : 'a fifo * ('a -> bool) -> 'a fifo
  val head : 'a fifo -> 'a
  val peek : 'a fifo -> 'a option
  val length : 'a fifo -> int
  val contents : 'a fifo -> 'a list
  val app : ('a -> unit) -> 'a fifo -> unit
  val map : ('a -> 'b) -> 'a fifo -> 'b fifo
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a fifo -> 'b
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a fifo -> 'b
- val q = empty;
val q = Q {front=[],rear=[]} : 'a fifo
- isEmpty q;
val it = true : bool
- val q' = enqueue (q, 1);
val q' = Q {front=[],rear=[1]} : int fifo
- isEmpty q';
val it = false : bool
- val q'' = List.foldl (fn (x, q) => enqueue (q, x)) q' [2, 3, 4];
val q'' = Q {front=[],rear=[4,3,2,1]} : int fifo
- peek q'';
val it = SOME 1 : int option
- length q'';
val it = 4 : int
- contents q'';
val it = [1,2,3,4] : int list
- val (q''', v) = dequeue q'';
val q''' = Q {front=[2,3,4],rear=[]} : int fifo
val v = 1 : int
- val (q'''', v') = dequeue q''';
val q'''' = Q {front=[3,4],rear=[]} : int fifo
val v' = 2 : int
- val (q''''', v'') = dequeue q'''';
val q''''' = Q {front=[4],rear=[]} : int fifo
val v'' = 3 : int
- val (q'''''', v''') = dequeue q''''';
val q'''''' = Q {front=[],rear=[]} : int fifo
val v''' = 4 : int
- isEmpty q'''''';
val it = true : bool
```


{{works with|SML/NJ}}
; Imperative interface

```sml
- open Queue;
opening Queue
  type 'a queue
  exception Dequeue
  val mkQueue : unit -> 'a queue
  val clear : 'a queue -> unit
  val isEmpty : 'a queue -> bool
  val enqueue : 'a queue * 'a -> unit
  val dequeue : 'a queue -> 'a
  val next : 'a queue -> 'a option
  val delete : 'a queue * ('a -> bool) -> unit
  val head : 'a queue -> 'a
  val peek : 'a queue -> 'a option
  val length : 'a queue -> int
  val contents : 'a queue -> 'a list
  val app : ('a -> unit) -> 'a queue -> unit
  val map : ('a -> 'b) -> 'a queue -> 'b queue
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a queue -> 'b
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a queue -> 'b
- val q : int queue = mkQueue ();
val q = - : int queue
- isEmpty q;
val it = true : bool
- enqueue (q, 1);
val it = () : unit
- isEmpty q;
val it = false : bool
- enqueue (q, 2);
val it = () : unit
- enqueue (q, 3);
val it = () : unit
- peek q;
val it = SOME 1 : int option
- length q;
val it = 3 : int
- contents q;
val it = [1,2,3] : int list
- dequeue q;
val it = 1 : int
- dequeue q;
val it = 2 : int
- peek q;
val it = SOME 3 : int option
- length q;
val it = 1 : int
- enqueue (q, 4);
val it = () : unit
- dequeue q;
val it = 3 : int
- peek q;
val it = SOME 4 : int option
- dequeue q;
val it = 4 : int
- isEmpty q;
val it = true : bool
```



## Stata

See [[Singly-linked list/Element definition#Stata]].

## Tcl

See [[FIFO#Tcl|FIFO]] for operation implementations:

```tcl
set Q [list]
empty Q     ;# ==> 1 (true)
push Q foo
empty Q     ;# ==> 0 (false)
push Q bar
peek Q      ;# ==> foo
pop Q       ;# ==> foo
peek Q      ;# ==> bar
```



## UNIX Shell

{{works with|ksh93}}
See [[Queue/Definition#UNIX_Shell|Queue/Definition]] for implementation:

```bash
# any valid variable name can be used as a queue without initialization

queue_empty foo && echo foo is empty || echo foo is not empty

queue_push foo bar
queue_push foo baz
queue_push foo "element with spaces"

queue_empty foo && echo foo is empty || echo foo is not empty

print "peek: $(queue_peek foo)"; queue_pop foo
print "peek: $(queue_peek foo)"; queue_pop foo
print "peek: $(queue_peek foo)"; queue_pop foo
print "peek: $(queue_peek foo)"; queue_pop foo
```


Output:

```txt
foo is empty
foo is not empty
peek: bar
peek: baz
peek: element with spaces
peek:
queue foo is empty
```



## VBA

See [[Queue/Definition#VBA]] for implementation.
The FiFo queue has been implemented with Collection. queue.count will return number of items in the queue. queue(i) will return the i-th item in the queue.

```vb
Public Sub fifo()
    push "One"
    push "Two"
    push "Three"
    Debug.Print pop, pop, pop, empty_
End Sub
```
{{out}}

```txt
One           Two           Three         True
```


## Wart

See [[FIFO#Wart|FIFO]] for implementation.

```txt
q <- (queue)
empty? q
=> 1
enq 1 q
empty? q
=> nil
enq 2 q
len q
=> 2
deq q
len q
=> 1
```



## XPL0


```XPL0
include c:\cxpl\codes;
def Size=8;
int Fifo(Size);
int In, Out;            \fill and empty indexes into Fifo

proc Push(A);           \Add integer A to queue
int  A;                 \(overflow not detected)
[Fifo(In):= A;
In:= In+1;
if In >= Size then In:= 0;
];

func Pop;               \Return first integer in queue
int  A;
[if Out=In then                   \if popping empty queue
    [Text(0, "Error");  exit 1];  \ then exit program with error code 1
A:= Fifo(Out);
Out:= Out+1;
if Out >= Size then Out:= 0;
return A;
];

func Empty;             \Return 'true' if queue is empty
return In = Out;

[In:= 0;  Out:= 0;
Push(0);
Text(0, if Empty then "true" else "false");  CrLf(0);
IntOut(0, Pop);  CrLf(0);
Push(1);
Push(2);
Push(3);
IntOut(0, Pop);  CrLf(0);
IntOut(0, Pop);  CrLf(0);
IntOut(0, Pop);  CrLf(0);
Text(0, if Empty then "true" else "false");  CrLf(0);

\A 256-byte queue is built in as device 8:
OpenI(8);  OpenO(8);
ChOut(8, ^0);                   \push
ChOut(0, ChIn(8));  CrLf(0);    \pop
ChOut(8, ^1);                   \push
ChOut(8, ^2);                   \push
ChOut(8, ^3);                   \push
ChOut(0, ChIn(8));  CrLf(0);    \pop
ChOut(0, ChIn(8));  CrLf(0);    \pop
ChOut(0, ChIn(8));  CrLf(0);    \pop
]
```


Output:

```txt

false
0
1
2
3
true
0
1
2
3

```



## Yabasic


```Yabasic
sub push(x$)
    queue$ = queue$ + x$ + "#"
end sub

sub pop$()
    local i, r$

    if queue$ <> "" then
        i = instr(queue$, "#")
        if i then
            r$ = left$(queue$, i-1)
            stack$ = right$(queue$, len(queue$) - i)
        else
            r$ = queue$
            queue$ = ""
        end if
        return r$
    else
        print "--Queue is empty--"
    end if
end sub

sub empty()
    return queue$ = ""
end sub

//
### ===== test =====


for n = 3 to 5
    print "Push ", n : push(str$(n))
next

print "Pop ", pop$()

print "Push ", 6 : push(str$(6))

while(not empty())
    print "Pop ", pop$()
wend

print "Pop ", pop$()
```


{{out}}

```txt
Push 3
Push 4
Push 5
Pop 3
Push 6
Pop 4
Pop 5
Pop 6
Pop --Queue is empty--
```



## zkl

See [[FIFO#zkl|FIFO]] for implementation.

```txt

q:=Queue();
q.empty();   //-->True
q.push(1,2,3);
q.pop();     //-->1
q.empty();   //-->False
q.pop();q.pop();q.pop(); //-->IndexError thrown

```

Lists support these semantics, so if you don't want the overhead of a Queue class:

```txt

q:=List();
q.len();    //-->0
q.append(1,2,3);
q.pop(0);   //-->1
q.len();    //-->2
q;          //-->L(2,3)
q.pop(0);q.pop(0);q.pop(0); //-->IndexError thrown
q;          //-->L()

```


{{omit from|GUISS}}
