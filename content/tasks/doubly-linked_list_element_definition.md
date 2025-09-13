+++
title = "Doubly-linked list/Element definition"
description = ""
date = 2019-08-30T19:01:15Z
aliases = []
[extra]
id = 2053
[taxonomies]
categories = ["task", "Data Structures"]
tags = []
+++

## Task

Define the data structure for a [[Linked_List#Doubly-Linked_List|doubly-linked list]] element.

The element should include a data member to hold its value and pointers to both the next element in the list and the previous element in the list.

The pointers should be mutable.


## Ada


```ada
type Link;
type Link_Access is access Link;
type Link is record
  Next : Link_Access := null;
  Prev : Link_Access := null;
  Data : Integer;
end record;
```

Using generics, the specification might look like this:

```ada
generic
   type Element_Type is private;
package Linked_List is
   type List_Type is limited private;
...
private
   type List_Element;
   type List_Element_Ptr is access list_element;
   type List_Element is
      record
	 Prev : List_Element_Ptr;
	 Data : Element_Type;
	 Next : List_Element_Ptr;
      end record;
   type List_Type is
      record
	 Head        : List_Element_Ptr;     -- Pointer to first element.
	 Tail        : List_Element_Ptr;     -- Pointer to last element.
	 Cursor      : List_Element_Ptr;     -- Pointer to cursor element.
	 Count       : Natural := 0;         -- Number of items in list.
	 Traversing  : Boolean := False;     -- True when in a traversal.
      end record;
end Linked_List;
```

In Ada 2005 this example can be written without declaration of an access type:

```ada
type Link is limited record
   Next : not null access Link := Link'Unchecked_Access;
   Prev : not null access Link := Link'Unchecked_Access;
   Data : Integer;
end record;
```

Here the list element is created already pointing to itself, so that no further initialization is required. The type of the element is marked as ''limited'' indicating that such elements have referential semantics and cannot be copied.

Ada's standard container library includes a generic doubly linked list. The structure of the link element is private.



## ALGOL 68

'''File: prelude/link.a68'''
```algol68
# -*- coding: utf-8 -*- #
CO REQUIRES:
  MODE OBJVALUE = ~ # Mode/type of actual obj to be queued #
END CO

MODE OBJLINK = STRUCT(
  REF OBJLINK next,
  REF OBJLINK prev,
  OBJVALUE value # ... etc. required #
);

PROC obj link new = REF OBJLINK: HEAP OBJLINK;

PROC obj link free = (REF OBJLINK free)VOID:
   prev OF free := next OF free := obj queue empty # give the garbage collector a big hint #
```
'''See also:''' [[Queue/Usage#ALGOL_68|Queue/Usage]]


## ARM Assembly

```ARM Assembly


/* ARM assembly Raspberry PI  */

/* structure Node Doublylinked List*/
    .struct  0
NDlist_next:                    @ next element
    .struct  NDlist_next + 4
NDlist_prev:                    @ previous element
    .struct  NDlist_prev + 4
NDlist_value:                   @ element value or key
    .struct  NDlist_value + 4
NDlist_fin:

```



## AutoHotkey

see [[Doubly-linked list/AutoHotkey]]


## Axe


```axe
Lbl LINK
r₂→{r₁}ʳ
0→{r₁+2}ʳ
0→{r₁+4}ʳ
r₁
Return

Lbl NEXT
{r₁+2}ʳ
Return

Lbl PREV
{r₁+4}ʳ
Return

Lbl VALUE
{r₁}ʳ
Return
```



## BBC BASIC

```bbcbasic
      DIM node{pPrev%, pNext%, iData%}

```



## Bracmat


```bracmat
link=(prev=) (next=) (data=)
```



## C


```c
struct link
{
  struct link *next;
  struct link *prev;
  void  *data;
  size_t type;
};
```



## C++

C++ has doubly linked list class template in standard library. However actual list noded are treated as implementation detail and encapsulated inside list. If we were to reimplement list, then node could look like that:

```cpp
template <typename T>

struct Node
{
    Node* next;
    Node* prev;
    T data;
};
```


## C#

```c#
class Link
{
    public int Item { get; set; }
    public Link Prev { get; set; }
    public Link Next { get; set; }

    //A constructor is not neccessary, but could be useful
    public Link(int item, Link prev = null, Link next = null) {
        Item = item;
        Prev = prev;
        Next = next;
    }
}
```



## Clojure


This sort of mutable structure is not idiomatic in Clojure.  [[../Definition#Clojure]] or a finger tree implementation would be better.


```Clojure
(defrecord Node [prev next data])

(defn new-node [prev next data]
  (Node. (ref prev) (ref next) data))
```



## Common Lisp



```lisp
(defstruct dlist head tail)
(defstruct dlink content prev next)
```


See the functions on the [[Doubly-Linked List]] page for the usage of these structures.


## D

A default constructor is implicit:

```d
struct Node(T) {
    T data;
    typeof(this)* prev, next;
}

void main() {
    alias N = Node!int;
    N* n = new N(10);
}
```



## Delphi


```d
struct Node(T) {

type

    pList = ^List ;

    list = record
       data : pointer ;
       prev : pList ;
       next : pList ;
    end;

}
```



## E


This does no type-checking, under the assumption that it is being used by a containing doubly-linked list object which enforces that invariant along with others such as that <code>element.getNext().getPrev() == element</code>. See [[Doubly-Linked List#E]] for an actual implementation (which uses slightly more elaborate nodes than this).


```e
def makeElement(var value, var next, var prev) {
    def element {
        to setValue(v) { value := v }
        to getValue() { return value }

        to setNext(n) { next := n }
        to getNext() { return next }

        to setPrev(p) { prev := p }
        to getPrev() { return prev }
    }

    return element
}
```



## Erlang

Using the code in [[Doubly-linked_list/Definition]] the element is defined by:

```Erlang

new( Data ) -> erlang:spawn( fun() -> loop( Data, noprevious, nonext ) end ).

```



## Fortran

In ISO Fortran 95 or later:

```fortran
type node
   real :: data
   type(node), pointer :: next => null(), previous => null()
end type node
!
! . . . .
!
type( node ), target :: head
```



## F#


```fsharp

type 'a DLElm = {
    mutable prev: 'a DLElm option
    data: 'a
    mutable next: 'a DLElm option
}

```



## Go


```go
type dlNode struct {
    string
    next, prev *dlNode
}
```

Or, using the [http://golang.org/pkg/container/list/#Element container/list] package:

```go
import "container/list"

var node list.Element
// and using: node.Next(), node.Prev(), node.Value
```



## Haskell

Haskell in general doesn't have mutability so the following 'mutator' functions use lazy evaluation instead.

Note that unlike naive pointer manipulation which could corrupt the doubly-linked list, updateLeft and updateRight will always yield a well-formed data structure.


```haskell

data DList a = Leaf | Node (DList a) a (DList a)

updateLeft _ Leaf = Leaf
updateLeft Leaf (Node _ v r) = Node Leaf v r
updateLeft new@(Node nl _ _) (Node _ v r) = current
    where current = Node prev v r
          prev = updateLeft nl new

updateRight _ Leaf = Leaf
updateRight Leaf (Node l v _) = Node l v Leaf
updateRight new@(Node _ _ nr) (Node l v _) = current
    where current = Node l v next
          next = updateRight nr new

```


==Icon and {{header|Unicon}}==

Uses Unicon classes.


```Unicon

class DoubleLink (value, prev_link, next_link)
  initially (value, prev_link, next_link)
    self.value := value
    self.prev_link := prev_link    # links are 'null' if not given
    self.next_link := next_link
end

```



## J


As discussed in [[Doubly-linked_list/Definition#J]], doubly linked lists are antithetical to J's design.  Defining individual elements as independent structures is even worse.  Now each element of the list must contain three arrays (everything in J is an array), all so that we can implement a list.

Yo Dawg, we heard you like lists, so we put lists in your lists so you can list while you list.

Nevertheless, this is doable, though it necessarily departs from the definition specified at [[Doubly-linked_list/Definition#J]].


```j
coclass'DoublyLinkedListElement'
create=:3 :0
  this=:coname''
  'predecessor successor data'=:y
  successor__predecessor=: predecessor__successor=: this
)
```


Here, when we create a new list element, we need to specify its successor node and its predecessor node and the data to be stored in the node.  To start a new list we will need a node that can be the head and the tail of the list -- this will be the successor node for the last element of the list and the predecessor node for the first element of the list:


```j
coclass'DoublyLinkedListHead'
create=:3 :0
  predecessor=:successor=:this=: coname''
)
```



## Java

```java>public class Node<T
 {
   private T element;
   private Node<T> next, prev;

   public Node<T>(){
      next = prev = element = null;
   }

   public Node<T>(Node<T> n, Node<T> p, T elem){
      next = n;
      prev = p;
      element = elem;
   }

   public void setNext(Node<T> n){
      next = n;
   }

   public Node<T> getNext(){
      return next;
   }

   public void setElem(T elem){
      element = elem;
   }

   public T getElem(){
      return element;
   }

   public void setNext(Node<T> n){
      next = n;
   }

   public Node<T> setPrev(Node<T> p){
      prev = p;
   }

   public getPrev(){
      return prev;
   }
}
```


For use with [[Java]] 1.4 and below, delete all "<T>"s and replace T's with "Object".


## JavaScript

Inherits from LinkedList (see [[Singly-Linked_List_(element)#JavaScript]])

```javascript
function DoublyLinkedList(value, next, prev) {
    this._value = value;
    this._next = next;
    this._prev = prev;
}
// from LinkedList, inherit: value(), next(), traverse(), print()
DoublyLinkedList.prototype = new LinkedList();

DoublyLinkedList.prototype.prev = function() {
    if (arguments.length == 1)
        this._prev = arguments[0];
    else
        return this._prev;
}

function createDoublyLinkedListFromArray(ary) {
    var node, prev, head = new DoublyLinkedList(ary[0], null, null);
    prev = head;
    for (var i = 1; i < ary.length; i++) {
        node = new DoublyLinkedList(ary[i], null, prev);
        prev.next(node);
        prev = node;
    }
    return head;
}

var head = createDoublyLinkedListFromArray([10,20,30,40]);
```



## Julia

```julia
abstract type AbstractNode{T} end

struct EmptyNode{T} <: AbstractNode{T} end
mutable struct Node{T} <: AbstractNode{T}
    value::T
    pred::AbstractNode{T}
    succ::AbstractNode{T}
end
```



## Kotlin


```scala
// version 1.1.2

class Node<T: Number>(var data: T, var prev: Node<T>? = null, var next: Node<T>? = null) {
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
    val n1 = Node(1)
    val n2 = Node(2, n1)
    n1.next = n2
    val n3 = Node(3, n2)
    n2.next = n3
    println(n1)
    println(n2)
    println(n3)
}
```


```txt

1 -> 2 -> 3
2 -> 3
3

```


=={{header|Modula-2}}==


```modula2
TYPE
  Link = POINTER TO LinkRcd;
  LinkRcd = RECORD
    Prev, Next: Link;
    Data: INTEGER
  END;
```



## Nim


```nim
type
  Node[T] = ref TNode[T]

  TNode[T] = object
    next, prev: Node[T]
    data: T
```

=={{header|Oberon-2}}==

```oberon2

MODULE Box;
TYPE
        Object* = POINTER TO ObjectDesc;
	ObjectDesc* = (* ABSTRACT *) RECORD
	END;

        (* ... *)
END Box.

MODULE Collections;
TYPE
	Node* = POINTER TO NodeDesc;
	NodeDesc* = (* ABSTRACT *) RECORD
		prev-,next-: Node;
                value-: Box.Object;
	END;

        (* ... *)
END Collections.

```


## Objeck


```objeck
class ListNode {
  @value : Base;
  @next : ListNode;
  @previous: ListNode;

  New(value : Base) {
    @value := value;
  }

  method : public : Set(value : Base) ~ Nil {
    @value := value;
  }

  method : public : Get() ~ Base {
    return @value;
  }

  method : public : SetNext(next :  Collection.ListNode) ~ Nil {
    @next := next;
  }

  method : public : GetNext() ~ ListNode {
    return @next;
  }

  method : public : SetPrevious(previous :  Collection.ListNode) ~ Nil {
    @previous := previous;
  }

  method : public : GetPrevious() ~ ListNode {
    return @previous;
  }
}
```



## OCaml


### Imperative


```ocaml
type 'a dlink = {
  mutable data: 'a;
  mutable next: 'a dlink option;
  mutable prev: 'a dlink option;
}

let dlink_of_list li =
  let f prev_dlink x =
    let dlink = {
      data = x;
      prev = None;
      next = prev_dlink }
    in
    begin match prev_dlink with
    | None -> ()
    | Some prev_dlink ->
        prev_dlink.prev <- Some dlink
    end;
    Some dlink
  in
  List.fold_left f None (List.rev li)
;;

let list_of_dlink =
  let rec aux acc = function
  | None -> List.rev acc
  | Some{ data = d;
          prev = _;
          next = next } -> aux (d::acc) next
  in
  aux []
;;

let iter_forward_dlink f =
  let rec aux = function
  | None -> ()
  | Some{ data = d;
          prev = _;
          next = next } -> f d; aux next
  in
  aux
;;
```



```ocaml
# let dl = dlink_of_list [1;2;3;4;5] in
  iter_forward_dlink (Printf.printf "%d\n") dl ;;
1
2
3
4
5
- : unit = ()
```



### Functional


The previous implementation is the strict equivalent of the other
examples of this page and its task, but in regular OCaml these kind of imperative structures can be advantageously replaced by a functional equivalent, that can be use in the same area, which is to have a list of elements and be able to point to one of these. We can use this type:


```ocaml
type 'a nav_list = 'a list * 'a * 'a list
```


The middle element is the pointed item, and the two lists are the
previous and the following items.
Here are the associated functions:

```ocaml
let nav_list_of_list = function
  | hd::tl -> [], hd, tl
  | [] -> invalid_arg "empty list"

let current = function
  | _, item, _ -> item

let next = function
  | prev, item, next::next_tl ->
      item::prev, next, next_tl
  | _ ->
      failwith "end of nav_list reached"

let prev = function
  | prev::prev_tl, item, next ->
      prev_tl, prev, item::next
  | _ ->
      failwith "begin of nav_list reached"
```


```ocaml
# let nl = nav_list_of_list [1;2;3;4;5] ;;
val nl : 'a list * int * int list = ([], 1, [2; 3; 4; 5])
# let nl = next nl ;;
val nl : int list * int * int list = ([1], 2, [3; 4; 5])
# let nl = next nl ;;
val nl : int list * int * int list = ([2; 1], 3, [4; 5])

# current nl ;;
- : int = 3
```



## Oforth


Complete definition is here : [[../Definition#Oforth]]


```oforth
Object Class new: DNode(value, mutable prev, mutable next)
```



## Oz

We show how to create a new node as a record value.

```oz
fun {CreateNewNode Value}
   node(prev:{NewCell _}
	next:{NewCell _}
	value:Value)
end
```

Note: this is for illustrative purposes only. In a real Oz program, you would use one of the existing data types.


## Pascal



```pascal
type link_ptr = ^link;
     data_ptr = ^data; (* presumes that type 'data' is defined above *)
     link = record
              prev: link_ptr;
              next: link_ptr;
              data: data_ptr;
            end;
```



## Perl



```perl
my %node = (
     data => 'say what',
     next => \%foo_node,
     prev => \%bar_node,
);
$node{next} = \%quux_node;  # mutable
```


## Perl 6



```perl6
role DLElem[::T] {
    has DLElem[T] $.prev is rw;
    has DLElem[T] $.next is rw;
    has T $.payload = T;

    method pre-insert(T $payload) {
        die "Can't insert before beginning" unless $!prev;
        my $elem = ::?CLASS.new(:$payload);
        $!prev.next = $elem;
        $elem.prev = $!prev;
        $elem.next = self;
        $!prev = $elem;
        $elem;
    }

    method post-insert(T $payload) {
        die "Can't insert after end" unless $!next;
        my $elem = ::?CLASS.new(:$payload);
        $!next.prev = $elem;
        $elem.next = $!next;
        $elem.prev = self;
        $!next = $elem;
        $elem;
    }

    method delete {
        die "Can't delete a sentinel" unless $!prev and $!next;
        $!next.prev = $!prev;
        $!prev.next = $!next;   # conveniently returns next element
    }
}
```



## Phix

In Phix, types are used for validation and debugging rather than specification purposes. For extensive run-time checking you could use

```Phix
enum NEXT,PREV,DATA
type slnode(object x)
    return (sequence(x) and length(x)=DATA and <i>udt</i>(x[DATA]) and integer(x[NEXT] and integer(x[PREV]))
end type
```

But more often you would just use the builtin sequences. See also [[Singly-linked_list/Element_definition#Phix|Singly-linked_list/Element_definition]].

Memory is automatically reclaimed the moment items are no longer needed.


## PicoLisp

We use (in addition to the header structure described in
[[Doubly-linked list/Definition#PicoLisp]])
two cells per doubly-linked list element:

         +-----+-----+     +-----+-----+
         | Val |  ---+---> |  |  |  ---+---> next
         +-----+-----+     +--+--+-----+
                              |
                     prev <---+

With that, 'cddr' can be used to access the next, and 'cadr' to access the
previous element.

```PicoLisp
(de 2tail (X DLst)
   (let L (cdr DLst)
      (con DLst (cons X L NIL))
      (if L
         (con (cdr L) (cdr DLst))
         (set DLst (cdr DLst)) ) ) )

(de 2head (X DLst)
   (let L (car DLst)                  # Get current data list
      (set DLst (cons X NIL L))       # Prepend two new cons pairs
      (if L                           # Unless DLst was empty
         (set (cdr L) (car DLst))     # set new 'prev' link
         (con DLst (car DLst)) ) ) )  # otherwise set 'end' link

# We prepend 'not' to the list in the previous example
(2head 'not *DLst)
```

For output of the example data, see [[Doubly-linked list/Traversal#PicoLisp]].


## PL/I


```PL/I

define structure
   1 Node,
      2 value        fixed decimal,
      2 back_pointer handle(Node),
      2 fwd_pointer  handle(Node);

P = NEW(: Node :); /* Creates a node, and lets P point at it.       */
get (P => value);  /* Reads in a value to the node we just created. */

/* Assuming that back_pointer and fwd_pointer point at other nodes, */
/* we can say ...                                                   */
P = P => fwd_pointer; /* P now points at the next node.             */
...
P = P => back_pointer; /* P now points at the previous node.        */

```



## Pop11



```pop11
uses objectclass;
define :class Link;
    slot next = [];
    slot prev = [];
    slot data = [];
enddefine;
```


## PureBasic


```PureBasic
Structure node
  *prev.node
  *next.node
  value.i
EndStructure
```



## Python



```python
class Node(object):
     def __init__(self, data = None, prev = None, next = None):
         self.prev = prev
         self.next = next
         self.data = data
     def __str__(self):
         return str(self.data)
     def __repr__(self):
         return repr(self.data)
     def iter_forward(self):
         c = self
         while c != None:
             yield c
             c = c.next
     def iter_backward(self):
         c = self
         while c != None:
             yield c
             c = c.prev
```



## Racket



```racket

(define-struct dlist (head tail) #:mutable)
(define-struct dlink (content prev next) #:mutable)

```


See the functions on the [[Doubly-Linked List]] page for the usage of these structures.


## REXX

REXX doesn't have linked lists, as there are no pointers (or handles).

However, linked lists can be simulated with lists in REXX.

        ╔═════════════════════════════════════════════════════════════════════════╗
        ║        ☼☼☼☼☼☼☼☼☼☼☼ Functions of the  List Manager ☼☼☼☼☼☼☼☼☼☼☼           ║
        ║   @init      ─── initializes the List.                                  ║
        ║                                                                         ║
        ║   @size      ─── returns the size of the List  [could be a  0  (zero)]. ║
        ║                                                                         ║
        ║   @show      ─── shows (displays) the complete List.                    ║
        ║   @show k,1  ─── shows (displays) the  Kth  item.                       ║
        ║   @show k,m  ─── shows (displays)  M  items,  starting with  Kth  item. ║
        ║   @show ,,─1 ─── shows (displays) the complete List backwards.          ║
        ║                                                                         ║
        ║   @get  k    ─── returns the  Kth  item.                                ║
        ║   @get  k,m  ─── returns the  M  items  starting with the  Kth  item.   ║
        ║                                                                         ║
        ║   @put  x    ─── adds the  X  items to the  end  (tail) of the List.    ║
        ║   @put  x,0  ─── adds the  X  items to the start (head) of the List.    ║
        ║   @put  x,k  ─── adds the  X  items to before of the  Kth  item.        ║
        ║                                                                         ║
        ║   @del  k    ─── deletes the item  K.                                   ║
        ║   @del  k,m  ─── deletes the   M  items  starting with item  K.         ║
        ╚═════════════════════════════════════════════════════════════════════════╝

```rexx
/*REXX program implements various List Manager functions  (see the documentation above).*/
call sy 'initializing the list.'            ;  call @init
call sy 'building list: Was it a cat I saw' ;  call @put "Was it a cat I saw"
call sy 'displaying list size.'             ;  say  "list size="@size()
call sy 'forward list'                      ;  call @show
call sy 'backward list'                     ;  call @show ,,-1
call sy 'showing 4th item'                  ;  call @show 4,1
call sy 'showing 5th & 6th items'           ;  call @show 5,2
call sy 'adding item before item 4: black'  ;  call @put "black",4
call sy 'showing list'                      ;  call @show
call sy 'adding to tail: there, in the ...' ;  call @put "there, in the shadows, stalking its prey (and next meal)."
call sy 'showing list'                      ;  call @show
call sy 'adding to head: Oy!'               ;  call @put  "Oy!",0
call sy 'showing list'                      ;  call @show
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
p:       return word(arg(1), 1)                  /*pick the first word out of many items*/
sy:      say;   say left('', 30) "───" arg(1) '───';              return
@init:   $.@=;    @adjust: $.@=space($.@);   $.#=words($.@);      return
@hasopt: arg o;                                                   return pos(o, opt)\==0
@size:   return $.#
/*──────────────────────────────────────────────────────────────────────────────────────*/
@del:    procedure expose $.;     arg k,m;          call @parms 'km'
         _=subword($.@, k, k-1)   subword($.@, k+m)
         $.@=_;                   call @adjust;                                return
/*──────────────────────────────────────────────────────────────────────────────────────*/
@get:    procedure expose $.;     arg k,m,dir,_
         call @parms 'kmd'
                                  do j=k  for m  by dir  while  j>0  &  j<=$.#
                                  _=_ subword($.@, j, 1)
                                  end   /*j*/
         return strip(_)
/*──────────────────────────────────────────────────────────────────────────────────────*/
@parms:  arg opt                                 /*define a variable based on an option.*/
         if @hasopt('k')  then k=min($.#+1, max(1, p(k 1)))
         if @hasopt('m')  then m=p(m 1)
         if @hasopt('d')  then dir=p(dir 1);                                   return
/*──────────────────────────────────────────────────────────────────────────────────────*/
@put:    procedure expose $.;     parse arg x,k;        k=p(k $.#+1);      call @parms 'k'
         $.@=subword($.@, 1, max(0, k-1))   x   subword($.@, k);           call @adjust
         return
/*──────────────────────────────────────────────────────────────────────────────────────*/
@show:   procedure expose $.;     parse arg k,m,dir;    if dir==-1  &  k==''   then k=$.#
         m=p(m $.#);              call @parms 'kmd';    say @get(k,m, dir);    return
```

'''output'''

```txt

                               ─── initializing the list. ───

                               ─── building list: Was it a cat I saw ───

                               ─── displaying list size. ───
list size=6

                               ─── forward list ───
Was it a cat I saw

                               ─── backward list ───
saw I cat a it Was

                               ─── showing 4th item ───
cat

                               ─── showing 6th & 6th items ───
I saw

                               ─── adding item before item 4: black ───

                               ─── showing list ───
Was it a black cat I saw

                               ─── adding to tail: there, in the ... ───

                               ─── showing list ───
Was it a black cat I saw there, in the shadows, stalking its prey (and next meal).

                               ─── adding to head: Oy! ───

                               ─── showing list ───
Oy! Was it a black cat I saw there, in the shadows, stalking its prey (and next meal).

```



## Ruby

Extending [[Singly-Linked List (element)#Ruby]]

```ruby
class DListNode < ListNode
  attr_accessor :prev
  # accessors :succ and :value are inherited

  def initialize(value, prev=nil, succ=nil)
    @value = value
    @prev = prev
    @prev.succ = self if prev
    @succ = succ
    @succ.prev = self if succ
  end

  def self.from_values(*ary)
    ary << (f = ary.pop)
    ary.map! {|i| new i }
    ary.inject(f) {|p, c| p.succ = c; c.prev = p; c }
  end
end

list = DListNode.from_values 1,2,3,4
```



## Rust




###  Simply using the standard library


```rust
use std::collections::LinkedList;
fn main() {
     // Doubly linked list containing 32-bit integers
     let list = LinkedList::<i32>::new();
}
```


=== The behind-the-scenes implementation ===
Doubly linked lists present a problem in Rust due to its ownership model. There cannot be two mutable references to the same object, so what are we to do? Below are the relevant lines (with added comments) from the <code>std</code> implementation ([https://doc.rust-lang.org/std/collections/struct.LinkedList.html Documentation] [https://github.com/rust-lang/rust/blob/master/src/libcollections/linked_list.rs Source]).

The standard library uses the (currently) unstable `Shared<T>` type which indicates that the ownership of its contained type has shared ownership. It is guaranteed not to be null, is variant over <code>T</code> (meaning that an <code>&Shared<&'static T></code> may be used where a <code>&Shared<&'a T></code> is expected, indicates to the compiler that it may own a <code>T</code>) and may be dereferenced to a mutable pointer (<code>*mut T</code>). All of the above may be accomplished in standard stable Rust, except for the non-null guarantee which allows the compiler to make a few extra optimizations.


```rust>pub struct LinkedList<T
 {
    head: Option<Shared<Node<T>>>,
    tail: Option<Shared<Node<T>>>,
    len: usize,
    marker: PhantomData<Box<Node<T>>>, // Indicates that we logically own a boxed (owned pointer) Node<T>>
}

struct Node<T> {
    next: Option<Shared<Node<T>>>,
    prev: Option<Shared<Node<T>>>,
    element: T,
}
```



## Sidef


```ruby
var node = Hash.new(
     data => 'say what',
     next => foo_node,
     prev => bar_node,
);

node{:next} = quux_node;  # mutable
```



## Tcl

```tcl
oo::class create List {
    variable content next prev
    constructor {value {list ""}} {
        set content $value
        set next $list
        set prev ""
        if {$next ne ""} {
            $next previous [self]
        }
    }
    method value args {
        set content {*}$args
    }
    method next args {
        set next {*}$args
    }
    method previous args {
        set prev {*}$args
    }
}
```



## Visual Basic .NET



```vbnet
Public Class Node(Of T)
   Public Value As T
   Public [Next] As Node(Of T)
   Public Previous As Node(Of T)
End Class
```



## zkl


```zkl
class Node{
   fcn init(_value,_prev=Void,_next=Void)
      { var value=_value, prev=_prev, next=_next; }
   fcn toString{ value.toString() }
}
```


```zkl
a,b:=Node(1),Node("three");
a.next=b; b.prev=a;
println(a.next,"  ",b.prev);
```

```txt

three  1

```


{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have user-defined data structures or objects. -->
