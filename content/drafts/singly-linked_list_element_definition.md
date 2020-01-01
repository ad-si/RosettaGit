+++
title = "Singly-linked list/Element definition"
description = ""
date = 2019-08-25T17:44:12Z
aliases = []
[extra]
id = 2005
[taxonomies]
categories = []
tags = []
+++

{{task|Data Structures}}Define the data structure for a [[singly-linked list]] element. Said element should contain a data member capable of holding a numeric value, and the link to the next element should be mutable.

{{Template:See also lists}}

## 360 Assembly

The program uses DSECT and USING pseudo instruction to define a node.

```360asm
*        Singly-linked list/Element definition  07/02/2017
LISTSINA CSECT
         USING  LISTSINA,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         " <-
         ST     R15,8(R13)         " ->
         LR     R13,R15            " addressability
*        Allocate A
         GETMAIN RU,LV=12          get storage
         USING  NODE,R11           make storage addressable
         LR     R11,R1             "
         MVC    VAL,=CL8'A'        val='A'
         MVC    NEXT,=A(0)
         DROP   R11                base no longer needed
         ST     R11,A              A=@A
*        Init LIST
         ST     R11,LIST           init LIST with A
*        Allocate C
         GETMAIN RU,LV=12          get storage
         USING  NODE,R11           make storage addressable
         LR     R11,R1             "
         MVC    VAL,=CL8'C'        val='C'
         MVC    NEXT,=A(0)
         DROP   R11                base no longer needed
         ST     R11,C              C=@C
*        Insert C After A
         MVC    P1,A
         MVC    P2,C
         LA     R1,PARML
         BAL    R14,INSERTAF
*        Allocate B
         GETMAIN RU,LV=12          get storage
         USING  NODE,R11           make storage addressable
         LR     R11,R1             "
         MVC    VAL,=CL8'B'        val='B'
         MVC    NEXT,=A(0)
         DROP   R11                base no longer needed
         ST     R11,B              B=@B
*        Insert B After A
         MVC    P1,A
         MVC    P2,B
         LA     R1,PARML
         BAL    R14,INSERTAF
*        List all
         L      R11,LIST
         USING  NODE,R11           address node
LOOP     C      R11,=A(0)
         BE     ENDLIST
         XPRNT  VAL,8
         L      R11,NEXT
         B      LOOP
ENDLIST  DROP   R11
         FREEMAIN A=A,LV=12        free A
         FREEMAIN A=B,LV=12        free B
         FREEMAIN A=C,LV=12        free C
RETURN   L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    " restore
         XR     R15,R15            " rc=0
         BR     R14                exit
LIST     DS     A                  list head
A        DS     A
B        DS     A
C        DS     A
PARML    DS     0A
P1       DS     A
P2       DS     A
INSERTAF CNOP   0,4
         L      R2,0(R1)           @A
         L      R3,4(R1)           @B
         USING  NODE,R2            ->A
         L      R4,NEXT            @C
         DROP   R2
         USING  NODE,R3            ->B
         ST     R4,NEXT            B.NEXT=@C
         DROP   R3
         USING  NODE,R2            ->A
         ST     R3,NEXT            A.NEXT=@B
         DROP   R2
         BR     R14                return
         LTORG                     all literals
NODE     DSECT                     node (size=12)
VAL      DS     CL8
NEXT     DS     A
         YREGS
         END    LISTSINA
```

{{out}}

```txt

A
B
C

```



## ACL2

The built in pair type, <code>cons</code>, is sufficient for defining a linked list. ACL2 does not have mutable variables, so functions must instead return a copy of the original list.


```Lisp
(let ((elem 8)
      (next (list 6 7 5 3 0 9)))
  (cons elem next))
```


Output:

```txt
(8 6 7 5 3 0 9)
```



## ActionScript


```ActionScript
package
{
	public class Node
	{
		public var data:Object = null;
		public var link:Node = null;

		public function Node(obj:Object)
		{
			data = obj;
		}
	}
}
```


## Ada



```ada
type Link;
type Link_Access is access Link;
type Link is record
   Next : Link_Access := null;
   Data : Integer;
end record;
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.7 algol68g-2.7].}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}
'''File: prelude/single_link.a68'''
```algol68
# -*- coding: utf-8 -*- #
CO REQUIRES:
  MODE OBJVALUE = ~ # Mode/type of actual obj to be stacked #
END CO

MODE OBJNEXTLINK = STRUCT(
  REF OBJNEXTLINK next,
  OBJVALUE value # ... etc. required #
);

PROC obj nextlink new = REF OBJNEXTLINK:
  HEAP OBJNEXTLINK;

PROC obj nextlink free = (REF OBJNEXTLINK free)VOID:
  next OF free := obj stack empty # give the garbage collector a BIG hint #
```
'''See also:''' [[Stack#ALGOL_68|Stack]]


## ALGOL W


```algolw
    % record type to hold a singly linked list of integers                    %
    record ListI ( integer iValue; reference(ListI) next );

    % declare a variable to hold a list                                       %
    reference(ListI) head;

    % create a list of integers                                               %
    head := ListI( 1701, ListI( 9000, ListI( 42, ListI( 90210, null ) ) ) );
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program defList.s   */

/* Constantes    */
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ READ,   3
.equ WRITE,  4

.equ NBELEMENTS,      100                @ list size

/*******************************************/
/* Structures                               */
/********************************************/
/* structure linkedlist*/
    .struct  0
llist_next:                             @ next element
    .struct  llist_next + 4
llist_value:                            @ element value
    .struct  llist_value + 4
llist_fin:
/* Initialized data */
.data
szMessInitListe:         .asciz "List initialized.\n"
szCarriageReturn:        .asciz "\n"
/* datas error display */
szMessErreur:            .asciz "Error detected.\n"

/* UnInitialized data */
.bss
lList1:                  .skip llist_fin * NBELEMENTS    @ list memory place

/*  code section */
.text
.global main
main:
    ldr r0,iAdrlList1
    mov r1,#0
    str r1,[r0,#llist_next]
    ldr r0,iAdrszMessInitListe
    bl affichageMess

100:                                    @ standard end of the program
    mov r7, #EXIT                       @ request to exit program
    svc 0                               @ perform system call
iAdrszMessInitListe:       .int szMessInitListe
iAdrszMessErreur:          .int szMessErreur
iAdrszCarriageReturn:      .int szCarriageReturn
iAdrlList1:                .int lList1
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                       @ save  registers
    mov r2,#0                                   @ counter length */
1:                                              @ loop length calculation
    ldrb r1,[r0,r2]                             @ read octet start position + index
    cmp r1,#0                                   @ if 0 its over
    addne r2,r2,#1                              @ else add 1 in the length
    bne 1b                                      @ and loop
                                                @ so here r2 contains the length of the message
    mov r1,r0                                   @ address message in r1
    mov r0,#STDOUT                              @ code to write to the standard output Linux
    mov r7, #WRITE                              @ code call system "write"
    svc #0                                      @ call system
    pop {r0,r1,r2,r7,lr}                        @ restaur registers
    bx lr                                       @ return

```


## AutoHotkey


```AutoHotkey
element = 5 ; data
element_next = element2  ; link to next element
```



## AWK


Awk only has global associative arrays, which will be used for the list. Numerical indexes into the array will serve as node pointers. A list element will have the next node pointer separated from the value by the pre-defined SUBSEP value. A function will be used to access a node's next node pointer or value given a node pointer (array index). The first array element will serve as the list head.


```awk

BEGIN {
    NIL = 0
    HEAD = 1
    LINK = 1
    VALUE = 2

    delete list
    initList()
}

function initList() {
    delete list
    list[HEAD] = makeNode(NIL, NIL)
}

function makeNode(link, value) {
    return link SUBSEP value
}

function getNode(part, nodePtr,    linkAndValue) {
    split(list[nodePtr], linkAndValue, SUBSEP)
    return linkAndValue[part]
}

```



## Axe


```axe
Lbl LINK
r₂→{r₁}ʳ
0→{r₁+2}ʳ
r₁
Return

Lbl NEXT
{r₁+2}ʳ
Return

Lbl VALUE
{r₁}ʳ
Return
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      DIM node{pNext%, iData%}

```



## Bracmat

Data mutation is not Bracmatish, but it can be done. Here is a datastructure for a mutable data value and for a mutable reference.

```bracmat
link =
  (next=)
  (data=)
```

Example of use:

```bracmat
  new$link:?link1
& new$link:?link2
& first thing:?(link1..data)
& secundus:?(link2..data)
& '$link2:(=?(link1..next))
& !(link1..next..data)
```

The last line returns

```txt
secundus
```



## C


```c
struct link {
  struct link *next;
  int data;
};
```



## C++


The simplest C++ version looks basically like the C version:


```cpp
struct link
{
  link* next;
  int data;
};
```


Initialization of links on the heap can be simplified by adding a constructor:


```cpp
struct link
{
  link* next;
  int data;
  link(int a_data, link* a_next = 0): next(a_next), data(a_data) {}
};
```


With this constructor, new nodes can be initialized directly at allocation; e.g. the following code creates a complete list with just one statement:


```cpp
 link* small_primes = new link(2, new link(3, new link(5, new link(7))));
```


However, C++ also allows to make it generic on the data type (e.g. if you need large numbers, you might want to use a larger type than int, e.g. long on 64-bit platforms, long long on compilers that support it, or even a bigint class).


```cpp
template<typename T>

struct link
{
  link* next;
  T data;
  link(T a_data, link* a_next = 0): next(a_next), data(a_data) {}
};
```


Note that the generic version works for any type, not only integral types.

=={{header|C sharp|C#}}==


```csharp
class LinkedListNode
{
    public int Value { get; set; }
    public LinkedListNode Next { get; set; }

    // A constructor is not necessary, but could be useful.
    public Link(int value, LinkedListNode next = null)
    {
        Item = value;
        Next = next;
    }
}
```


A generic version:

```csharp>class LinkedListNode<T

{
    public T Value { get; set; }
    public LinkedListNode Next { get; set; }

    public Link(T value, LinkedListNode next = null)
    {
        Item = value;
        Next = next;
    }
}
```


The most C-like possible version is basically C.

```csharp
unsafe struct link {
    public link* next;
    public int data;
};
```



## Clojure

As with other LISPs, this is built in.  Clojure provides a nice abstraction of lists with its use of: [http://clojure.org/sequences sequences] (also called seqs).


```clojure
(cons 1 (cons 2 (cons 3 nil)))  ; =>(1 2 3)
```


Note: this is an immutable data structure.  With cons you are '''cons'''tructing a new seq.


## Common Lisp


The built-in <code>cons</code> type is used to construct linked lists. Using another type would be unidiomatic and inefficient.


```lisp
(cons 1 (cons 2 (cons 3 nil))   => (1 2 3)
```



## Clean


```clean
import StdMaybe

:: Link t = { next :: Maybe (Link t), data :: t }
```



## D

Generic template-based node element.


```d
struct SLinkedNode(T) {
    T data;
    typeof(this)* next;
}

void main() {
    alias SLinkedNode!int N;
    N* n = new N(10);
}
```

Also the Phobos library contains a singly-linked list, std.container.SList. Tango contains tango.util.collection.LinkSeq.


## Delphi


A simple one way list. I use a generic pointer for the data that way it can point to any structure, individual variable or whatever. Note that in Standard Pascal, there are no generic pointers, therefore one has to settle for a specific data type there.


```delphi
Type
  pOneWayList = ^OneWayList;
  OneWayList = record
                pData : pointer ;
                Next  : pOneWayList ;
               end;
```



## E



```e
interface LinkedList guards LinkedListStamp {}
def empty implements LinkedListStamp {
    to null() { return true }
}
def makeLink(value :int, var next :LinkedList) {
    def link implements LinkedListStamp {
        to null() { return false }
        to value() { return value }
        to next() { return next }
        to setNext(new) { next := new }
    }
    return link
}
```


## Elena


```elena
class Link
{
    prop int  Item;
    prop Link Next;

    constructor(int item, Link next)
    {
        Item := item;
        Next := next
    }
}
```



## Erlang

Lists are builtin, but Erlang is single assignment. Here we need mutable link to next element. Mutable in Erlang usually means a process, so:

```Erlang

new( Data ) -> erlang:spawn( fun() -> loop( Data, nonext ) end ).

```

For the whole module see [[Singly-linked_list/Element_insertion]]


## Factor

<lang>TUPLE: linked-list data next ;

: <linked-list> ( data -- linked-list )
    linked-list new swap >>data ;
```



## Fantom



```fantom

class Node
{
  const Int value  // keep value fixed
  Node? successor  // allow successor to change, also, can be 'null', for end of list

  new make (Int value, Node? successor := null)
  {
    this.value = value
    this.successor = successor
  }
}

```



## Forth


Idiomatically,


```forth
0 value numbers
: push ( n -- )
  here swap numbers , , to numbers ;
```


NUMBERS is the head of the list, initially nil (= 0); PUSH adds an element to the list; list cells have the structure {Link,Number}.  Speaking generally, Number can be anything and list cells can be as long as desired (e.g., {Link,N1,N2} or {Link,Count,"a very long string"}), but the link is always first - or rather, a link always points to the next link, so that NEXT-LIST-CELL is simply fetch (@).  Some operations:


```forth
: length ( list -- u )
  0 swap begin dup while 1 under+ @ repeat drop ;

: head ( list -- x )
  cell+ @ ;

: .numbers ( list -- )
  begin dup while dup head . @ repeat drop ;
```


Higher-order programming, simple continuations, and immediate words can pull out the parallel code of LENGTH and .NUMBERS .  Anonymous and dynamically allocated lists are as straightforward.


## Fortran

In ISO Fortran 95 or later:

```fortran
type node
   real :: data
   type( node ), pointer :: next => null()
end type node
!
!. . . .
!
type( node ) :: head
```



## Go


```go
type Ele struct {
    Data interface{}
    Next *Ele
}

func (e *Ele) Append(data interface{}) *Ele {
    if e.Next == nil {
        e.Next = &Ele{data, nil}
    } else {
        tmp := &Ele{data, e.Next}
        e.Next = tmp
    }
    return e.Next
}

func (e *Ele) String() string {
    return fmt.Sprintf("Ele: %v", e.Data)
}
```



## Groovy

Solution:

```groovy
class ListNode {
    Object payload
    ListNode next
    String toString() { "${payload} -> ${next}" }
}
```


Test:

```groovy
def n1 = new ListNode(payload:25)
n1.next = new ListNode(payload:88)

println n1
```


Output:

```txt
25 -> 88 -> null
```



## Haskell


This task is not idiomatic for Haskell. Usually, all data in pure functional programming is immutable, and deconstructed through [[Pattern Matching]]. The Prelude already contains a parametrically polymorphic list type that can take any data member type, including numeric values. These lists are then used very frequently. Because of this, lists have additional special syntactic sugar.

An equivalent declaration for such a list type without the special syntax would look like this:


```haskell
 data List a = Nil | Cons a (List a)
```


A declaration like the one required in the task, with an integer as element type and a mutable link, would be


```haskell
 data IntList s = Nil | Cons Integer (STRef s (IntList s))
```


but that would be really awkward to use.

== Icon and Unicon ==

The Icon version works in both Icon and Unicon.  Unicon also permits a class-based definition.

=== {{header|Icon}} ===


```Icon

record Node (value, successor)

```


=== {{header|Unicon}} ===


```Unicon

class Node (value, successor)
  initially (value, successor)
    self.value := value
    self.successor := successor
end

```


With either the record or the class definition, new linked lists are easily created and manipulated:


```Icon

procedure main ()
  n := Node(1, Node (2))
  write (n.value)
  write (n.successor.value)
end

```



## J


This task is not idomatic in J -- J has lists natively and while using lists to emulate lists is quite possible, it creates additional overhead at every step of the way.  (J's native lists are probably best thought of as arrays with values all adjacent to each other, though they also support constant time append.)

However, for illustrative purposes:


```J
list=: 0 2$0
list
```


This creates and then displays an empty list, with zero elements. The first number in an item is (supposed to be) the index of the next element of the list (_ for the final element of the list). The second number in an item is the numeric value stored in that list item. The list is named and names are mutable in J which means links are mutable.

To create such a list with one element which contains number 42, we can do the following:


```J
   list=: ,: _ 42
   list
_ 42
```


Now list contains one item, with index of the next item and value.

Note: this solution exploits the fact that, in this numeric case, data types for index and for node content are the same. If we need to store, for example, strings in the nodes, we should do something different, for example:


```J
   list=: 0 2$a: NB. creates list with 0 items
   list
   list=: ,: (<_) , <'some text' NB. creates list with 1 item
   list
+-+---------+
|_|some text|
+-+---------+
```



## Java


The simplest Java version looks basically like the C++ version:


```java
class Link
{
    Link next;
    int data;
}
```


Initialization of links on the heap can be simplified by adding a constructor:


```java
class Link
{
    Link next;
    int data;
    Link(int a_data, Link a_next) { next = a_next; data = a_data; }
}
```


With this constructor, new nodes can be initialized directly at allocation; e.g. the following code creates a complete list with just one statement:


```java
 Link small_primes = new Link(2, new Link(3, new Link(5, new Link(7, null))));
```


{{works with|Java|1.5+}}
However, Java also allows to make it generic on the data type. This will only work on reference types, not primitive types like int or float (wrapper classes like Integer and Float are available).


```java>class Link<T

{
  Link<T> next;
  T data;
  Link(T a_data, Link<T> a_next) { next = a_next; data = a_data; }
}
```



## JavaScript


```javascript
function LinkedList(value, next) {
    this._value = value;
    this._next = next;
}
LinkedList.prototype.value = function() {
    if (arguments.length == 1)
        this._value = arguments[0];
    else
        return this._value;
}
LinkedList.prototype.next = function() {
    if (arguments.length == 1)
        this._next = arguments[0];
    else
        return this._next;
}

// convenience function to assist the creation of linked lists.
function createLinkedListFromArray(ary) {
    var head = new LinkedList(ary[0], null);
    var prev = head;
    for (var i = 1; i < ary.length; i++) {
        var node = new LinkedList(ary[i], null);
        prev.next(node);
        prev = node;
    }
    return head;
}

var head = createLinkedListFromArray([10,20,30,40]);
```



## Julia

{{works with|Julia|0.6}}


```julia
abstract type AbstractNode{T} end

struct EmptyNode{T} <: AbstractNode{T} end
mutable struct Node{T} <: AbstractNode{T}
    data::T
    next::AbstractNode{T}
end
Node{T}(x) where T = Node{T}(x::T, EmptyNode{T}())

mutable struct LinkedList{T}
    head::AbstractNode{T}
end
LinkedList{T}() where T = LinkedList{T}(EmptyNode{T}())
LinkedList() = LinkedList{Any}()

Base.isempty(ll::LinkedList) = ll.head isa EmptyNode
function lastnode(ll::LinkedList)
    if isempty(ll) throw(BoundsError()) end
    nd = ll.head
    while !(nd.next isa EmptyNode)
        nd = nd.next
    end
    return nd
end

function Base.push!(ll::LinkedList{T}, x::T) where T
    nd = Node{T}(x)
    if isempty(ll)
        ll.head = nd
    else
        tail = lastnode(ll)
        tail.next = nd
    end
    return ll
end
function Base.pop!(ll::LinkedList{T}) where T
    if isempty(ll)
        throw(ArgumentError("list must be non-empty"))
    elseif ll.head.next isa EmptyNode
        nd = ll.head
        ll.head = EmptyNode{T}()
    else
        nx = ll.head
        while !isa(nx.next.next, EmptyNode)
            nx = nx.next
        end
        nd = nx.next
        nx.next = EmptyNode{T}()
    end
    return nd.data
end

lst = LinkedList{Int}()
push!(lst, 1)
push!(lst, 2)
push!(lst, 3)
pop!(lst) # 3
pop!(lst) # 2
pop!(lst) # 1
```



## Kotlin


```scala
// version 1.1.2

class Node<T: Number>(var data: T, var next: Node<T>? = null) {
    override fun toString(): String {
        val sb = StringBuilder(this.data.toString())
        var node = this.next
        while (node != null) {
            sb.append(" -> ", node.data.toString())
            node = node.next
        }
        return sb.toString()
    }
}

fun main(args: Array<String>) {
    val n = Node(1, Node(2, Node(3)))
    println(n)
}
```


{{out}}

```txt

1 -> 2 -> 3

```



## Logo

As with other list-based languages, simple lists are represented easily in Logo.


```logo
fput item list ; add item to the head of a list

first list  ; get the data
butfirst list ; get the remainder
bf list       ; contraction for "butfirst"
```


These return modified lists, but you can also destructively modify lists. These are normally not used because you might accidentally create cycles in the list.


```logo
.setfirst list value
.setbf list remainder
```



## Mathematica


```Mathematica
Append[{}, x]
-> {x}
```


=={{header|Modula-2}}==


```modula2
TYPE
  Link = POINTER TO LinkRcd;
  LinkRcd = RECORD
    Next: Link;
    Data: INTEGER
  END;
```


=={{header|Modula-3}}==

```modula3
TYPE
  Link = REF LinkRcd;
  LinkRcd = RECORD
    Next: Link;
    Data: INTEGER
  END;
```



## Nim


```nim
type Node[T] = ref object
  next: Node[T]
  data: T

proc newNode[T](data: T): Node[T] =
  Node[T](data: data)

var a = newNode 12
var b = newNode 13
var c = newNode 14
```


=={{header|Objective-C}}==


```objc>#import <Foundation/Foundation.h


@interface RCListElement<T> : NSObject
{
  RCListElement<T> *next;
  T datum;
}
- (RCListElement<T> *)next;
- (T)datum;
- (RCListElement<T> *)setNext: (RCListElement<T> *)nx;
- (void)setDatum: (T)d;
@end

@implementation RCListElement
- (RCListElement *)next
{
  return next;
}
- (id)datum
{
  return datum;
}
- (RCListElement *)setNext: (RCListElement *)nx
{
  RCListElement *p = next;
  next = nx;
  return p;
}
- (void)setDatum: (id)d
{
  datum = d;
}
@end
```



## OCaml


This task is not idiomatic for OCaml. OCaml already contains a built-in parametrically polymorphic list type that can take any data member type, including numeric values. These lists are then used very frequently. Because of this, lists have additional special syntactic sugar.  OCaml's built-in lists, like most functional data structures, are immutable, and are deconstructed through [[Pattern Matching]].

An equivalent declaration for such a list type without the special syntax would look like this:


```ocaml
 type 'a list = Nil | Cons  of 'a * 'a list
```


A declaration like the one required in the task, with an integer as element type and a mutable link, would be


```ocaml
 type int_list = Nil | Cons  of int * int_list ref
```


but that would be really awkward to use.


## Oforth



```Oforth
Collection Class new: LinkedList(data, mutable next)
```



## ooRexx


The simplest ooRexx version is similar in form to the Java or C++ versions:

```ooRexx

list = .linkedlist~new
index = list~insert("abc")   -- insert a first item, keeping the index
list~insert("def")           -- adds to the end
list~insert("123", .nil)     -- adds to the begining
list~insert("456", index)    -- inserts between "abc" and "def"
list~remove(index)           -- removes "abc"

say "Manual list traversal"
index = list~first           -- demonstrate traversal
loop while index \== .nil
    say index~value
    index = index~next
end

say
say "Do ... Over traversal"
do value over list
    say value
end

-- the main list item, holding the anchor to the links.
::class linkedlist
::method init
  expose anchor

  -- create this as an empty list
  anchor = .nil

-- return first link element
::method first
  expose anchor
  return anchor

-- return last link element
::method last
  expose anchor

  current = anchor
  loop while current \= .nil
      -- found the last one
      if current~next == .nil then return current
      current = current~next
  end
  -- empty
  return .nil

-- insert a value into the list, using the convention
-- followed by the built-in list class.  If the index item
-- is omitted, add to the end.  If the index item is .nil,
-- add to the end.  Otherwise, just chain to the provided link.
::method insert
  expose anchor
  use arg value

  newLink = .link~new(value)
  -- adding to the end
  if arg() == 1 then do
      if anchor == .nil then anchor = newLink
      else self~last~insert(newLink)
  end
  else do
      use arg ,index
      if index == .nil then do
         if anchor \== .nil then newLink~next = anchor
         anchor = newLink
      end
      else index~insert(newLink)
  end
  -- the link item serves as an "index"
  return newLink

-- remove a link from the chain
::method remove
  expose anchor

  use strict arg index

  -- handle the edge case
  if index == anchor then anchor = anchor~next
  else do
      -- no back link, so we need to scan
      previous = self~findPrevious(index)
      -- invalid index, don't return any item
      if previous == .nil then return .nil
      previous~next = index~next
  end
  -- belt-and-braces, remove the link and return the value
  index~next = .nil
  return index~value

-- private method to find a link predecessor
::method findPrevious private
  expose anchor
  use strict arg index

  -- we're our own precessor if this first
  if index == anchor then return self

  current = anchor
  loop while current \== .nil
      if current~next == index then return current
      current = current~next
  end
  -- not found
  return .nil

-- helper method to allow DO ... OVER traversal
::method makearray
  expose anchor
  array = .array~new

  current = anchor
  loop while current \= .nil
      array~append(current~value)
      current = current~next
  end
  return array

::class link
::method init
  expose value next
  -- by default, initialize both data and next to empty.
  use strict arg value = .nil, next = .nil

-- allow external access to value and next link
::attribute value
::attribute next

::method insert
  expose next
  use strict arg newNode
  newNode~next = next
  next = newNode



```


A link element can hold a reference to any ooRexx object.


## Pascal



```pascal
type
  PLink = ^TLink;
  TLink = record
    FNext: PLink;
    FData: integer;
  end;
```



## Perl

Just use an array. You can traverse and splice it any way. Linked lists are way too low level.

However, if all you got is an algorithm in a foreign language, you can use references to accomplish the translation.

```perl
my %node = (
    data => 'say what',
    next => \%foo_node,
);
$node{next} = \%bar_node;  # mutable
```


## Perl 6



### =With <tt>Pair</tt>=


A <tt>Pair</tt> (constructed with the <code>=></code> operator) can be treated as a cons cell, and thus used to build a linked lists:


```perl6
my $elem = 42 => $nextelem;
```


However, because this is not the primary purpose of the <tt>Pair</tt> type, it suffers from the following limitations:

* The naming of <tt>Pair</tt>'s accessor methods is not idiomatic for this use case (<code>.key</code> for the cell's value, and <code>.value</code> for the link to the next cell).
* A <tt>Pair</tt> (unlike an <tt>Array</tt>) does not automatically wrap its keys/values in item containers &ndash; so each cell of the list will be immutable once created, making element insertion/deletion impossible (except inserting at the front).
* It provides no built-in convenience methods for iterating/modifying/transforming such a list.


### =With custom type=


For more flexibility, one would create a custom type:


```perl6
class Cell {
    has      $.value is rw;
    has Cell $.next  is rw;

    # ...convenience methods here...
}

sub cons ($value, $next) { Cell.new(:$value, :$next) }

my $list = cons 10, (cons 20, (cons 30, Nil));
```



## Phix

In Phix, types are used for validation and debugging rather than specification purposes. For extensive run-time checking you could use something like

```Phix
enum NEXT,DATA
type slnode(object x)
    return (sequence(x) and length(x)=DATA and myotherudt(x[DATA]) and integer(x[NEXT])
end type
```

But more often you would just use the builtin sequences. It is worth noting that while "node lists", such as
{{2},{'A',3},{'B',4},{'C',0}} are one way to hold a linked list (with the first element a dummy header),
both "parallel/tag lists" such as {{'A','B','C'},{2,3,0}} and "flat lists" such as {'A',3,'B',5,'C',0} are
generally more efficient, and the latter is heavily used in the compiler itself (for the ternary lookup
tree, intermediate code, and the pre-packed machine code binary).

Memory is automatically reclaimed the moment items are no longer needed.

See also [[Singly-linked_list/Element_removal#Phix]] for some working code


## PicoLisp

In PicoLisp, the singly-linked list is the most important data structure. Many
built-in functions deal with linked lists. A list consists of interconnected
"cells". Cells are also called "cons pairs", because they are constructed with
the function '[http://software-lab.de/doc/refC.html#cons cons]'.

Each cell consists of two parts: A CAR and a CDR. Both may contain (i.e. point
to) arbitrary data (numbers, symbols, other cells, or even to itself). In the
case of a linked list, the CDR points to the rest of the list.

The CAR of a cell can be manipulated with
'[http://software-lab.de/doc/refS.html#set set]'
and the CDR with '[http://software-lab.de/doc/refC.html#con con]'.


## PL/I


```PL/I

declare 1 node based (p),
          2 value fixed,
          2 link pointer;

```



## Pop11


List are built in into Pop11, so normally on would just use them:


```pop11
;;; Use shorthand syntax to create list.
lvars l1 = [1 2 three 'four'];
;;; Allocate a single list node, with value field 1 and the link field
;;; pointing to empty list
lvars l2 = cons(1, []);
;;; print first element of l1
front(l1) =>
;;; print the rest of l1
back(l1) =>
;;; Use index notation to access third element
l1(3) =>
;;; modify link field of l2 to point to l1
l1 -> back(l2);
;;; Print l2
l2 =>
```


If however one wants to definite equivalent user-defined type, one can do this:


```pop11
uses objectclass;
define :class ListNode;
    slot value = [];
    slot next = [];
enddefine;
;;; Allocate new node and assign to l1
newListNode() -> l1;
;;; Print it
l1 =>
;;; modify value
1 -> value(l1);
l1 =>
;;; Allocate new node with initialized values and assign to link field
;;; of l1
consListNode(2, []) -> next(l1);
l1 =>
```



## PureBasic



```PureBasic
Structure MyData
  *next.MyData
  Value.i
EndStructure
```



## Python


The Node class implements also iteration for more Pythonic iteration over linked lists.


```python
class LinkedList(object):
     """USELESS academic/classroom example of a linked list implemented in Python.
        Don't ever consider using something this crude!  Use the built-in list() type!
     """
	class Node(object):
		def __init__(self, item):
			self.value  = item
			self.next = None
	def __init__(self, item=None):
		if item is not None:
			self.head = Node(item); self.tail = self.head
		else:
			self.head = None; self.tail = None
	def append(self, item):
		if not self.head:
			self.head = Node(item)
			self.tail = self.head
		elif self.tail:
			self.tail.next = Node(item)
			self.tail = self.tail.next
		else:
			self.tail = Node(item)
	def __iter__(self):
		cursor = self.head
		while cursor:
			yield cursor.value
			cursor = cursor.next
```


'''Note:''' As explained in this class' docstring implementing linked lists and nodes in Python is an utterly pointless academic exercise.  It may give on the flavor of the elements that would be necessary in some other programming languages (''e.g.'' using Python as "executable psuedo-code").  Adding methods for finding, counting, removing and inserting elements is left as an academic exercise to the reader.  For any practical application use the built-in ''list()'' or ''dict()'' types as appropriate.


## Racket


Unlike other Lisp dialects, Racket's <tt>cons</tt> cells are immutable, so they cannot be used to satisfy this task.  However, Racket also includes mutable pairs which are still the same old mutable singly-linked lists.


```Racket

#lang racket
(mcons 1 (mcons 2 (mcons 3 '()))) ; a mutable list

```



## REXX

The REXX language doesn't have any native linked lists, but they can be created easily.

The values of a REXX linked list can be anything (nulls, character strings, including any type/kind of number, of course).

```rexx
/*REXX program demonstrates how to create and show a single-linked list.*/
@.=0                                   /*define a null linked list.     */
call set@ 3                            /*linked list:  12 Proth Primes. */
call set@ 5
call set@ 13
call set@ 17
call set@ 41
call set@ 97
call set@ 113
call set@ 193
call set@ 241
call set@ 257
call set@ 353
call set@ 449
w=@.max_width                          /*use the maximum width of nums. */
call list@                             /*list all the elements in list. */
exit                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────LIST@ subroutine────────────────────*/
list@: say;  w=max(7, @.max_width )    /*use the max width of nums or 7.*/
say center('item',6)        center('value',w)        center('next',6)
say center(''    ,6,'─')    center(''     ,w,'─')    center(''    ,6,'─')
p=1
        do j=1  until p==0      /*show all entries of linked list*/
        say  right(j,6)   right(@.p._value,w)   right(@.p._next,6)
        p=@.p._next
        end   /*j*/
return
/*──────────────────────────────────SET@ subroutine─────────────────────*/
set@: procedure expose @.; parse arg y /*get element to be added to list*/
_=@._last                              /*set the previous last element. */
n=_+1                                  /*bump last ptr in linked list.  */
@._._next=n                            /*set the  next  pointer value.  */
@._last=n                              /*define next item in linked list*/
@.n._value=y                           /*set item to the value specified*/
@.n._next=0                            /*set the  next  pointer value.  */
@..y=n                                 /*set a locator pointer to self. */
@.max_width=max(@.max_width,length(y)) /*set maximum width of any value.*/
return                                 /*return to invoker of this sub. */
```

'''output'''

```txt

 item   value   next
────── ─────── ──────
     1       3      2
     2       5      3
     3      13      4
     4      17      5
     5      41      6
     6      97      7
     7     113      8
     8     193      9
     9     241     10
    10     257     11
    11     353     12
    12     449      0

```



## Ruby



```ruby
class ListNode
  attr_accessor :value, :succ

  def initialize(value, succ=nil)
    self.value = value
    self.succ = succ
  end

  def each(&b)
    yield self
    succ.each(&b) if succ
  end

  include Enumerable

  def self.from_array(ary)
    head = self.new(ary[0], nil)
    prev = head
    ary[1..-1].each do |val|
      node = self.new(val, nil)
      prev.succ = node
      prev = node
    end
    head
  end
end

list = ListNode.from_array([1,2,3,4])
```



## Rust

Rust's <code>Option<T></code> type make the definition of a singly-linked list trivial. The use of <code>Box<T></code> (an owned pointer) is necessary because it has a known size, thus making sure the struct that contains it can have a finite size.

```Rust> struct Node<T
 {
    elem: T,
    next: Option<Box<Node<T>>>,
}
```


However, the above example would not be suitable for a library because, first and foremost, it is private by default but simply making it public would not allow for any encapsulation.


```Rust>type Link<T> = Option<Box<Node<T>>
; // Type alias
pub struct List<T> { // User-facing interface for list
    head: Link<T>,
}

struct Node<T> { // Private implementation of Node
    elem: T,
    next: Link<T>,
}

impl<T> List<T> {
    #[inline]
    pub fn new() -> Self { // List constructor
        List { head: None }
    // Add other methods here
}
```


Then a separate program could utilize the basic implementation above like so:

```rust
extern crate LinkedList; // Name is arbitrary here

use LinkedList::List;

fn main() {
    let list = List::new();
    // Do stuff
}
```



## Run BASIC


```runbasic
data = 10
link = 10
dim node{data,link}
```



## Scala

Immutable lists that you can use with pattern matching.


```scala

sealed trait List[+A]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
case object Nil extends List[Nothing]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
}

```


Basic usage


```scala

def main(args: Array[String]): Unit = {
  val words = List("Rosetta", "Code", "Scala", "Example")
}

```



## Scheme


Scheme, like other Lisp dialects, has extensive support for singly-linked lists. The element of such a list is known as a ''cons-pair'', because you use the <tt>cons</tt> function to construct it:

```scheme
(cons value next)
```


The value and next-link parts of the pair can be deconstructed using the <tt>car</tt> and <tt>cdr</tt> functions, respectively:

```scheme
(car my-list) ; returns the first element of the list
(cdr my-list) ; returns the remainder of the list
```


Each of these parts are mutable and can be set using the <tt>set-car!</tt> and <tt>set-cdr!</tt> functions, respectively:

```scheme
(set-car! my-list new-elem)
(set-cdr! my-list new-next)
```



## Sidef


```ruby
var node = Hash.new(
    data => 'say what',
    next => foo_node,
);

node{:next} = bar_node;  # mutable
```




## SSEM

At the machine level, an element of a linked list can be represented using two successive words of storage where the first holds an item of data and the second holds either (a) the address where the next such pair of words will be found, or (b) a special <tt>NIL</tt> address indicating that we have reached the end of the list. Here is one way in which the list <tt>'(1 2 3)</tt> could be represented in SSEM code:

```ssem
01000000000000000000000000000000  26. 2
01111000000000000000000000000000  27. 30
10000000000000000000000000000000  28. 1
01011000000000000000000000000000  29. 26
11000000000000000000000000000000  30. 3
00000000000000000000000000000000  31. 0
```

Notice that the physical location of the pairs in storage can vary arbitrarily, and that (in this implementation) <tt>NIL</tt> is represented by zero. For an example showing how this list can be accessed, see [[Singly-Linked List (traversal)#SSEM]].


## Stata


There are several tasks in Rosetta Code all related to the linked-list structure. We will define here the required structure and all necessary functions, to make it easier to see how they are related to each over.

For instance, a stack and a queue are easily implemented with a linked-list:
* for a stack (LIFO), insert at head and pop at head,
* for a queue (FIFO), instert at tail and pop at head.

All the following is to be run in Mata.


###  Structures

Here we define two [https://www.stata.com/help.cgi?m2_struct structures]: one to hold a list item, another to hold the list [https://www.stata.com/help.cgi?m2_pointer pointers]: we store both the head and the tail, in order to be able to insert an element at both ends. An empty list has both head and tail set to NULL.


```stata
struct item {
	transmorphic scalar value
	pointer(struct item scalar) scalar next
}

struct list {
	pointer(struct item scalar) scalar head, tail
}
```



###  Test if empty


```stata
real scalar list_empty(struct list scalar a) {
	return(a.head == NULL)
}
```


Note that when a structure value is created, here for instance with <code>a = list()</code>, the elements are set to default values (zero real scalar, NULL pointer...). Hence, a newly created list is always empty.


###  Insertion

We can insert an element either before head or after tail. We can also insert after a given list item, but we must make sure the tail pointer of the list is updated if necessary.


```stata
void function list_insert(struct list scalar a, transmorphic scalar x) {
	struct item scalar i
	i.value = x
	if (a.head == NULL) {
		i.next = NULL
		a.head = a.tail = &i
	} else {
		i.next = a.head
		a.head = &i
	}
}

void function list_insert_end(struct list scalar a, transmorphic scalar x) {
	struct item scalar i
	i.value = x
	i.next = NULL
	if (a.head==NULL) {
		a.head = a.tail = &i
	} else {
		(*a.tail).next = &i
		a.tail = &i
	}
}

void function list_insert_after(struct list scalar a,
	pointer(struct item scalar) scalar p,
	transmorphic scalar x) {

	struct item scalar i
	i.value = x
	i.next = (*p).next
	(*p).next = &i
	if (a.tail == p) {
		a.tail = &i
	}
}
```



###  Traversal


Here are functions to compute the list length, and to print its elements. Here we assume list elements are either strings or real numbers, but one could write a more general function.


```stata
real scalar list_length(struct list scalar a) {
	real scalar n
	pointer(struct item scalar) scalar p

	n = 0
	for (p = a.head; p != NULL; p = (*p).next) {
		n++
	}
	return(n)
}

void list_show(struct list scalar a) {
	pointer(struct item scalar) scalar p

	for (p = a.head; p != NULL; p = (*p).next) {
		if (eltype((*p).value) == "string") {
			printf("%s\n", (*p).value);
		} else {
			printf("%f\n", (*p).value);
		}
	}
}
```



###  Return nth item

The function returns a pointer to the nth list item. If there are not enough elements, NULL is returned.


```stata
pointer(struct item scalar) scalar list_get(struct list scalar a,
	real scalar n) {

	pointer(struct item scalar) scalar p
	real scalar i

	p = a.head
	for (i = 1; p != NULL & i < n; i++) {
		p = (*p).next
	}
	return(p)
}
```



###  Remove and return first element


The following function "pops" the first element of the list. If the list is empty, Mata will throw an error.


```stata
transmorphic scalar list_pop(struct list scalar a) {
	transmorphic scalar x
	if (a.head == NULL) {
		_error("empty list")
	}
	x = (*a.head).value
	if (a.head == a.tail) {
		a.head = a.tail = NULL
	} else {
		a.head = (*a.head).next
	}
	return(x)
}
```



###  Remove and return nth element



```stata
transmorphic scalar list_remove(struct list scalar a,
	real scalar n) {

	pointer(struct item scalar) scalar p, q
	real scalar i
	transmorphic scalar x

	p = a.head
	if (n == 1) {
		if (p == NULL) {
			_error("empty list")
		}
		x = (*p).value
		if (p == a.tail) {
			a.head = a.tail = NULL
		} else {
			a.head = (*p).next
		}
	} else {
		for (i = 2; p != NULL & i < n; i++) {
			p = (*p).next
		}
		if (p == NULL) {
			_error("too few elements in list")
		}
		q = (*p).next
		if (q == NULL) {
			_error("too few elements in list")
		}
		x = (*q).value
		if (q == a.tail) {
			a.tail = p
		}
		(*p).next = (*q).next
	}
	return(x)
}
```



###  Examples


Adding to the head:


```stata
a = list()
list_insert(a, 10)
list_insert(a, 20)
list_insert(a, 30)
list_length(a)
list_show(a);
```


'''Output'''


```txt

30
20
10

```


Adding to the tail:


```stata
a = list()
list_insert_end(a, 10)
list_insert_end(a, 20)
list_insert_end(a, 30)
list_length(a)
list_show(a);
```


'''Output'''


```txt

10
20
30

```


Adding after an element:


```stata
list_insert_after(a, list_get(a, 2), 40)
list_show(a);
```


'''Output'''


```txt

10
20
40
30

```


Pop the first element:


```stata
list_pop(a)
```


'''Output'''


```txt

10

```


=== Linked-list task ===


```stata
a = list()
list_insert_end(a, "A")
list_insert_end(a, "B")
list_insert_after(a, list_get(a, 1), "C")
list_show(a)
```


'''Output'''


```txt

A
C
B

```



###  Stack behavior



```stata
a = list()
for (i = 1; i <= 4; i++) {
	list_insert(a, i)
}
while (!list_empty(a)) {
	printf("%f\n", list_pop(a))
}
```


'''Output'''


```txt

4
3
2
1

```



###  Queue behavior



```stata
a = list()
for (i = 1; i <= 4; i++) {
	list_insert_end(a, i)
}
while (!list_empty(a)) {
	printf("%f\n", list_pop(a))
}
```


'''Output'''


```txt

1
2
3
4

```



## Swift


```swift>class Node<T
{
    var data:T?=nil
    var next:Node?=nil
    init(input:T){
        data=input
        next=nil
    }
}

```



## Tcl

While it is highly unusual to implement linked lists in Tcl, since the language has a built-in list type (that internally uses arrays of references), it is possible to simulate it with objects.

{{Works with|Tcl|8.6}} or {{libheader|TclOO}}

```tcl
oo::class create List {
    variable content next
    constructor {value {list ""}} {
        set content $value
        set next $list
    }
    method value args {
        set content {*}$args
    }
    method attach {list} {
        set next $list
    }
    method detach {} {
        set next ""
    }
    method next {} {
        return $next
    }
    method print {} {
        for {set n [self]} {$n ne ""} {set n [$n next]} {
            lappend values [$n value]
        }
        return $values
    }
}
```



## X86 Assembly

------------------------------------------------------------------------------
This file will be included in the singly-linked list operation implementations

```x86asm

; x86_64 Linux NASM
; Linked_List_Definition.asm

%ifndef LinkedListDefinition
%define LinkedListDefinition

struc link
  value: resd 1
  next: resq 1
  linkSize:
endstruc

%endif

```

------------------------------------------------------------------------------

{{works with|NASM}}


```asm

struct link
.next: resd 1
.data: resd 1
endstruc

```

Of course, ASM not natively having structures we can simply do..

```asm

link resb 16

```

Which would reserve 16 bytes(2 dwords). We could just simply think of it in the form of a structure.


{{works with|MASM}}

```asm

link struct
next dd ?
data dd ?
link ends

```

{{works with|FASM}}

```asm
struc link next,data
{
    .next dd next
    .data dd data
}
```



## XPL0


```XPL0
include c:\cxpl\codes;                  \intrinsic 'code' declarations
def IntSize=4;                          \number of bytes in an integer
def Size=10;                            \number of nodes in this linked list
int Link, List, Node;
[Link:= 0;                              \build linked list, starting at the end
for Node:= 0 to Size-1 do
        [List:= Reserve(IntSize*2);     \get some memory to hold link and data
        List(0):= Link;
        List(1):= Node*Node;            \insert example data
        Link:= List;                    \Link now points to newly created node
        ];
Node:= List;                            \traverse the linked list
repeat  IntOut(0, Node(1));  CrLf(0);   \display the example data
        Node:= Node(0);                 \move to next node
until   Node=0;                         \end of the list
]
```



## zkl

Lists are a core element in zkl, both mutable and immutable. They are heterogeneous and can hold any object. They can be recursive.

```zkl
List(1,"two",3.14); L(1,"two",3.14);
ROList(fcn{"foobar"}); T('+);
```


{{omit from|GUISS}}
