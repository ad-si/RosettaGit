+++
title = "Doubly-linked list/Element insertion"
description = ""
date = 2018-11-28T06:33:25Z
aliases = []
[extra]
id = 2090
[taxonomies]
categories = ["task", "Data Structures"]
tags = []
languages = [
  "ada",
  "algol_68",
  "autohotkey",
  "axe",
  "bbc_basic",
  "c",
  "cpp",
  "csharp",
  "clojure",
  "common_lisp",
  "d",
  "e",
  "erlang",
  "fortran",
  "go",
  "haskell",
  "j",
  "javascript",
  "julia",
  "kotlin",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "oz",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "pop11",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "tcl",
  "visual_basic_net",
  "zkl"
]
+++

## Task

{{task|Data Structures}}Use the link structure defined in [[Doubly-Linked List (element)]] to define a procedure for inserting a link into a doubly-linked list. Call this procedure to insert element C into a list {A,B}, between elements A and B.

This is much like inserting into a [[Singly-Linked List (element insertion)|Singly-Linked List]], but with added assignments so that the backwards-pointing links remain correct.

## Ada


Define the procedure:

```ada
procedure Insert (Anchor : Link_Access; New_Link : Link_Access) is
begin
   if Anchor /= Null and New_Link /= Null then
      New_Link.Next := Anchor.Next;
      New_Link.Prev := Anchor;
      if New_Link.Next /= Null then
         New_Link.Next.Prev := New_Link;
      end if;
      Anchor.Next := New_Link;
   end if;
end Insert;
```

Create links and form the list.


```ada
procedure Make_List is
   A, B, C : Link_Access;
begin
   A := new Link;
   B := new Link;
   C := new Link;
   A.Data := 1;
   B.Data := 2;
   C.Data := 2;
   Insert(Anchor => A, New_Link => B); -- The list is (A, B)
   Insert(Anchor => A, New_Link => C); -- The list is (A, C, B)
end Make_List;
```


Element insertion using the generic doubly linked list defined in the standard Ada containers library.


```ada
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure List_Insertion is
   package String_List is new Ada.Containers.Doubly_Linked_Lists(Unbounded_String);
   use String_List;
   procedure Print(Position : Cursor) is
   begin
      Put_Line(To_String(Element(Position)));
   end Print;
   The_List : List;
begin
   The_List.Append(To_Unbounded_String("A"));
   The_List.Append(To_Unbounded_String("B"));
   The_List.Insert(Before => The_List.Find(To_Unbounded_String("B")),
      New_Item => To_Unbounded_String("C"));
   The_List.Iterate(Print'access);
end List_Insertion;
```



## ALGOL 68

```algol68
#!/usr/local/bin/a68g --script #

# SEMA do link OF splice = LEVEL 1 #
MODE LINK = STRUCT (
  REF LINK prev,
  REF LINK next,
  DATA value
);

# BEGIN rosettacode task specimen code:
  can handle insert both before the first, and after the last link #

PROC insert after = (REF LINK #self,# prev, DATA new data)LINK: (
# DOWN do link OF splice OF self; to make thread safe #
  REF LINK next = next OF prev;
  HEAP LINK new link := LINK(prev, next, new data);
  next OF prev := prev OF next := new link;
# UP do link OF splice OF self; #
  new link
);

PROC insert before = (REF LINK #self,# next, DATA new data)LINK:
  insert after(#self,# prev OF next, new data);

# END rosettacode task specimen code #

# Test case: #
MODE DATA = STRUCT(INT year elected, STRING name);
FORMAT data fmt = $dddd": "g"; "$;

test:(

# manually initialise the back splices #
  LINK presidential splice;
  presidential splice := (presidential splice, presidential splice, SKIP);

# manually build the chain #
  LINK previous, incumbent, elect;
  previous := (presidential splice, incumbent,          DATA(1993, "Clinton"));
  incumbent:= (previous,           elect,              DATA(2001, "Bush"   ));
  elect    := (incumbent,          presidential splice, DATA(2008, "Obama"  ));

  insert after(incumbent, LOC DATA := DATA(2004, "Cheney"));

  REF LINK node := previous;
  WHILE REF LINK(node) ISNT presidential splice DO
    printf((data fmt, value OF node));
    node := next OF node
  OD;
  print(new line)
)
```

Output:

```txt

1993: Clinton; 2001: Bush; 2004: Cheney; 2008: Obama;

```



## AutoHotkey

see [[Doubly-linked list/AutoHotkey]]


## Axe


```axe
Lbl INSERT
{r₁+2}ʳ→{r₂+2}ʳ
r₁→{r₂+4}ʳ
r₂→{{r₂+2}ʳ+4}ʳ
r₂→{r₁+2}ʳ
r₁
Return
```



## BBC BASIC

```bbcbasic
      DIM node{pPrev%, pNext%, iData%}
      DIM a{} = node{}, b{} = node{}, c{} = node{}

      a.pNext% = b{}
      a.iData% = 123
      b.pPrev% = a{}
      b.iData% = 456
      c.iData% = 789

      PROCinsert(a{}, c{})
      END

      DEF PROCinsert(here{}, new{})
      LOCAL temp{} : DIM temp{} = node{}
      new.pNext% = here.pNext%
      new.pPrev% = here{}
      !(^temp{}+4) = new.pNext%
      temp.pPrev% = new{}
      here.pNext% = new{}
      ENDPROC

```



## C

Define the function:

```c
void insert(link* anchor, link* newlink) {
  newlink->next = anchor->next;
  newlink->prev = anchor;
  (newlink->next)->prev = newlink;
  anchor->next = newlink;
}
```


Production code should also include checks that the passed links are valid (e.g. not null pointers). There should also be code to handle special cases, such as when *anchor is the end of the existing list (i.e. anchor->next is a null pointer).


To call the function:

Create links, and form the list:

```c
link a, b, c;
a.next = &b;
a.prev = null;
a.data = 1;
b.next = null;
b.prev = &a;
b.data = 3;
c.data = 2;
```


This list is now {a,b}, and c is separate.

Now call the function:

```c
insert(&a, &c);
```


This function call changes the list from {a,b} to {a,b,c}.



## C++

C++ already has own linked list structure. If we were to roll our own linked list, we could implement function inserting new value after specific node like that:

```cpp
template <typename T>

void insert_after(Node<T>* N, T&& data)
{
    auto node = new Node<T>{N, N->next, std::forward(data)};
    if(N->next != nullptr)
        N->next->prev = node;
    N->next = node;
}
```



## C#
The function handles creation of nodes in addition to inserting them.

```c#
static void InsertAfter(Link prev, int i)
{
    if (prev.next != null)
    {
        prev.next.prev = new Link() { item = i, prev = prev, next = prev.next };
        prev.next = prev.next.prev;
    }
    else
        prev.next = new Link() { item = i, prev = prev };
}
```

Example use:

```c#
static void Main()
{
    //Create A(5)->B(7)
    var A = new Link() { item = 5 };
    InsertAfter(A, 7);
    //Insert C(15) between A and B
    InsertAfter(A, 15);
}
```



## Clojure


This sort of mutable structure is not idiomatic in Clojure.  [[../Definition#Clojure]] or a finger tree implementation would be better.


```Clojure
(defrecord Node [prev next data])

(defn new-node [prev next data]
  (Node. (ref prev) (ref next) data))

(defn new-list [head tail]
  (List. (ref head) (ref tail)))

(defn insert-between [node1 node2 new-node]
  (dosync
   (ref-set (:next node1) new-node)
   (ref-set (:prev new-node) node1)
   (ref-set (:next new-node) node2)
   (ref-set (:prev node2) new-node)))

(set! *print-level* 1)

;; warning: depending on the value of *print-level*
;;   this could cause a stack overflow when printing

(let [h (new-node nil nil :A)
      t (new-node nil nil :B)]
  (insert-between h t (new-node nil nil :C))
  (new-list h t))
```



## Common Lisp

Code is on the [[Doubly-Linked List]] page, in function <code>insert-between</code>.


## D


```d
import std.stdio;

struct Node(T) {
    T data;
    typeof(this)* prev, next;
}

/// If prev is null, prev gets to point to a new node.
void insertAfter(T)(ref Node!T* prev, T item) pure nothrow {
    if (prev) {
        auto newNode = new Node!T(item, prev, prev.next);
        prev.next = newNode;
        if (newNode.next)
            newNode.next.prev = newNode;
    } else
        prev = new Node!T(item);
}

void show(T)(Node!T* list) {
    while (list) {
        write(list.data, " ");
        list = list.next;
    }
    writeln;
}

void main() {
    Node!(string)* list;
    insertAfter(list, "A");
    list.show;
    insertAfter(list, "B");
    list.show;
    insertAfter(list, "C");
    list.show;
}
```

```txt
A
A B
A C B
```



## E


```e
def insert(after, value) {
    def newNode := makeElement(value, after, after.getNext())
    after.getNext().setPrev(newNode)
    after.setNext(newNode)
}
```



```e
insert(A_node, C)
```



## Erlang

Using the code in [[Doubly-linked_list/Definition]].
```txt

2> doubly_linked_list:task().
foreach_next a
foreach_next c
foreach_next b
foreach_previous b
foreach_previous c
foreach_previous a

```



## Fortran

In ISO Fortran 95 or later:

```fortran
module dlList
    public :: node, insertAfter, getNext

    type node
        real :: data
        type( node ), pointer :: next => null()
        type( node ), pointer :: previous => null()
    end type node

contains
    subroutine insertAfter(nodeBefore, value)
        type( node ), intent(inout), target :: nodeBefore
        type( node ), pointer :: newNode
        real, intent(in) :: value

        allocate( newNode )
        newNode%data = value
        newNode%next => nodeBefore%next
        newNode%previous => nodeBefore

        if (associated( newNode%next )) then
            newNode%next%previous => newNode
        end if
        newNode%previous%next => newNode
    end subroutine insertAfter

    subroutine delete(current)
        type( node ), intent(inout), pointer :: current

        if (associated( current%next )) current%next%previous => current%previous
        if (associated( current%previous )) current%previous%next => current%next
        deallocate(current)
    end subroutine delete
end module dlList

program dlListTest
    use dlList
    type( node ), target :: head
    type( node ), pointer :: current, next

    head%data = 1.0
    current => head
    do i = 1, 20
       call insertAfter(current, 2.0**i)
       current => current%next
    end do

    current => head
    do while (associated(current))
        print *, current%data
        next => current%next
        if (.not. associated(current, head)) call delete(current)
        current => next
    end do
end program dlListTest
```

Output:

```fortran
1.
2.
4.
8.
16.
32.
64.
128.
256.
512.
1024.
2048.
4096.
8192.
16384.
32768.
65536.
131072.
262144.
524288.
1048576.
```



## Go


```go
package main

import "fmt"

type dlNode struct {
    string
    next, prev *dlNode
}

type dlList struct {
    head, tail *dlNode
}

func (list *dlList) String() string {
    if list.head == nil {
        return fmt.Sprint(list.head)
    }
    r := "[" + list.head.string
    for p := list.head.next; p != nil; p = p.next {
        r += " " + p.string
    }
    return r + "]"
}

func (list *dlList) insertTail(node *dlNode) {
    if list.tail == nil {
        list.head = node
    } else {
        list.tail.next = node
    }
    node.next = nil
    node.prev = list.tail
    list.tail = node
}

func (list *dlList) insertAfter(existing, insert *dlNode) {
    insert.prev = existing
    insert.next = existing.next
    existing.next.prev = insert
    existing.next = insert
    if existing == list.tail {
        list.tail = insert
    }
}

func main() {
    dll := &dlList{}
    fmt.Println(dll)
    a := &dlNode{string: "A"}
    dll.insertTail(a)
    dll.insertTail(&dlNode{string: "B"})
    fmt.Println(dll)
    dll.insertAfter(a, &dlNode{string: "C"})
    fmt.Println(dll)
}
```

Output:

```txt

<nil>
[A B]
[A C B]

```



## Haskell

Using the algebraic data type and update functions from [[Doubly-Linked_List_(element)#Haskell]].


```haskell

insert _  Leaf            = Leaf
insert nv l@(Node pl v r) = (\(Node c _ _) -> c) new
    where new   = updateLeft left . updateRight right $ Node l nv r
          left  = Node pl v new
          right = case r of
                      Leaf       -> Leaf
                      Node _ v r -> Node new v r

```


==Icon and {{header|Unicon}}==

Uses Unicon classes.


```Unicon

class DoubleLink (value, prev_link, next_link)

  # insert given node after this one, losing its existing connections
  method insert_after (node)
    node.prev_link := self
    if (\next_link) then next_link.prev_link := node
    node.next_link := next_link
    self.next_link := node
  end

  initially (value, prev_link, next_link)
    self.value := value
    self.prev_link := prev_link    # links are 'null' if not given
    self.next_link := next_link
end

```



## J


Using the list element definition at [[Doubly-linked_list/Element_definition#J]]

  (pred;succ;<data) conew 'DoublyLinkedListElement'

This creates a new node containing the specified data between the nodes pred and succ.


## JavaScript

See [[Doubly-Linked_List_(element)#JavaScript]]

```javascript
DoublyLinkedList.prototype.insertAfter = function(searchValue, nodeToInsert) {
    if (this._value == searchValue) {
        var after = this.next();
        this.next(nodeToInsert);
        nodeToInsert.prev(this);
        nodeToInsert.next(after);
        after.prev(nodeToInsert);
    }
    else if (this.next() == null)
        throw new Error(0, "value '" + searchValue + "' not found in linked list.")
    else
        this.next().insertAfter(searchValue, nodeToInsert);
}

var list = createDoublyLinkedListFromArray(['A','B']);
list.insertAfter('A', new DoublyLinkedList('C', null, null));
```



## Julia


```julia
mutable struct DLNode{T}
    value::T
    pred::Union{DLNode{T}, Nothing}
    succ::Union{DLNode{T}, Nothing}
    DLNode(v) = new{typeof(v)}(v, nothing, nothing)
end

function insertpost(prevnode, node)
    succ = prevnode.succ
    prevnode.succ = node
    node.pred = prevnode
    node.succ = succ
    if succ != nothing
        succ.pred =  node
    end
    node
end

function printfromroot(root)
    print(root.value)
    while root.succ != nothing
        root = root.succ
        print(" -> $(root.value)")
    end
    println()
end

node1 = DLNode(1)
printfromroot(node1)
node2 = DLNode(2)
node3 = DLNode(3)
insertpost(node1, node2)
printfromroot(node1)
insertpost(node1, node3)
printfromroot(node1)

```
 {{output}}
```txt

1
1 -> 2
1 -> 3 -> 2

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

fun <T: Number> insert(after: Node<T>, new: Node<T>) {
    new.next = after.next
    if (after.next != null) after.next!!.prev = new
    new.prev = after
    after.next = new
}

fun main(args: Array<String>) {
    val a = Node(1)
    val b = Node(3, a)
    a.next = b
    println("Before insertion : $a")
    val c = Node(2)
    insert(after = a, new = c)
    println("After  insertion : $a")
}
```


```txt

Before insertion : 1 -> 3
After  insertion : 1 -> 2 -> 3

```



## Nim


```nim
proc insertAfter[T](l: var List[T], r, n: Node[T]) =
  n.prev = r
  n.next = r.next
  n.next.prev = n
  r.next = n
  if r == l.tail: l.tail = n
```


=={{header|Oberon-2}}==

```oberon2

        PROCEDURE (dll: DLList) InsertAfter*(p: Node; o: Box.Object);
	VAR
		n: Node;
	BEGIN
		n := NewNode(o);
		n.prev := p;
		n.next := p.next;
		IF p.next # NIL THEN p.next.prev := n END;
		p.next := n;
		IF p = dll.last THEN dll.last := n END;
		INC(dll.size)
	END InsertAfter;

```



## Objeck


```objeck
method : public : native : AddBack(value : Base) ~ Nil {
  node := ListNode->New(value);
  if(@head = Nil) {
    @head := node;
    @tail := @head;
  }
  else {
    @tail->SetNext(node);
    node->SetPrevious(@tail);
    @tail := node;
  };
}
```



## OCaml


### with dlink


```ocaml
(* val _insert : 'a dlink -> 'a dlink -> unit *)
let _insert anchor newlink =
  newlink.next <- anchor.next;
  newlink.prev <- Some anchor;
  begin match newlink.next with
  | None -> ()
  | Some next ->
      next.prev <-Some newlink;
  end;
  anchor.next <- Some newlink;;

(* val insert : 'a dlink option -> 'a -> unit *)
let insert dl v =
  match dl with
  | (Some anchor) -> _insert anchor {data=v; prev=None; next=None}
  | None  -> invalid_arg "dlink empty";;
```


testing in the top level:


```ocaml
# type t = A | B | C ;;
type t = A | B | C

# let dl = dlink_of_list [A; B] in
  insert dl C;
  list_of_dlink dl ;;
- : t list = [A; C; B]
```


===with nav_list===


```ocaml
(* val insert : 'a nav_list -> 'a -> 'a nav_list *)
let insert (prev, cur, next) v = (prev, cur, v::next)
```


testing in the top level:

```ocaml
# type t = A | B | C ;;
type t = A | B | C

# let nl = nav_list_of_list [A; B] ;;
val nl : 'a list * t * t list = ([], A, [B])

# insert nl C ;;
- : 'a list * t * t list = ([], A, [C; B])
```



## Oforth


Complete definition is here : [[../Definition#Oforth]]


```oforth
: test      // ( -- aDList )
| dl |
   DList new ->dl
   dl insertFront("A")
   dl insertBack("B")
   dl head insertAfter(DNode new("C", null , null))
   dl ;
```


```txt

>test .s
[1] (DList) [A, C, B]

```



## Oz

Warning: Do not use in real programs.

```oz
declare
  fun {CreateNewNode Value}
     node(prev:{NewCell nil}
          next:{NewCell nil}
          value:Value)
  end

  proc {InsertAfter Node NewNode}
     Next = Node.next
  in
     (NewNode.next) := @Next
     (NewNode.prev) := Node
     case @Next of nil then skip
     [] node(prev:NextPrev ...) then
        NextPrev := NewNode
     end
     Next := NewNode
  end

  A = {CreateNewNode a}
  B = {CreateNewNode b}
  C = {CreateNewNode c}
in
  {InsertAfter A B}
  {InsertAfter A C}
```



## Pascal



```pascal
procedure insert_link( a, b, c: link_ptr );
begin
  a^.next := c;
  if b <> nil then b^.prev := c;
  c^.next := b;
  c^.prev := a;
end;
```


Actually, a more likely real world scenario is 'insert after A'.  So...


```pascal
procedure realistic_insert_link( a, c: link_ptr );
begin
  if a^.next <> nil then a^.next^.prev := c;  (* 'a^.next^' is another way of saying 'b', if b exists *)
  c^.next := a^.next;
  a^.next := c;
  c^.prev := a;
end;
```



## Perl


```perl
my %node_model = (
        data => 'something',
        prev => undef,
        next => undef,
);

sub insert
{
        my ($anchor, $newlink) = @_;
        $newlink->{next} = $anchor->{next};
        $newlink->{prev} = $anchor;
        $newlink->{next}->{prev} = $newlink;
        $anchor->{next} = $newlink;
}

# create the list {A,B}
my $node_a = { %node_model };
my $node_b = { %node_model };

$node_a->{next} = $node_b;
$node_b->{prev} = $node_a;

# insert element C into a list {A,B}, between elements A and B.
my $node_c = { %node_model };
insert($node_a, $node_c);
```


## Perl 6

Using the generic definitions from [[Doubly-linked_list/Definition#Perl_6]]:

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

role DLList[::DLE] {
    has DLE $.first;
    has DLE $.last;

    submethod BUILD {
	$!first = DLE.new;
	$!last = DLE.new;
	$!first.next = $!last;
	$!last.prev = $!first;
    }

    method list { ($!first.next, *.next ...^ !*.next).map: *.payload }
    method reverse { ($!last.prev, *.prev ...^ !*.prev).map: *.payload }
}

class DLElem_Str does DLElem[Str] {}
class DLList_Str does DLList[DLElem_Str] {}

my $sdll = DLList_Str.new;
my $b = $sdll.first.post-insert('A').post-insert('B');
$b.pre-insert('C');
say $sdll.list;  # A C B
```

```txt
A C B
```



## Phix

See also [[Doubly-linked_list/Traversal#Phix|Doubly-linked_list/Traversal]].

```Phix
enum NEXT,PREV,DATA
constant empty_dll = {{1,1}}
sequence dll = empty_dll

procedure insert_after(object data, integer pos=1)
integer prv = dll[pos][PREV]
    dll = append(dll,{pos,prv,data})
    if prv!=0 then
        dll[prv][NEXT] = length(dll)
    end if
    dll[pos][PREV] = length(dll)
end procedure

insert_after("ONE")
insert_after("TWO")
insert_after("THREE")

?dll
```

```txt

```



## PicoLisp

This works with the structures described in
[[Doubly-linked list/Definition#PicoLisp]] and
[[Doubly-linked list/Element definition#PicoLisp]].

```PicoLisp
# Insert an element X at position Pos
(de 2insert (X Pos DLst)
   (let (Lst (nth (car DLst) (dec (* 2 Pos)))  New (cons X (cadr Lst) Lst))
      (if (cadr Lst)
         (con (cdr @) New)
         (set DLst New) )
      (if (cdr Lst)
         (set @ New)
         (con DLst New) ) ) )

(setq *DL (2list 'A 'B))      # Build a two-element doubly-linked list
(2insert 'C 2 *DL)            # Insert C at position 2
```

For output of the example data, see [[Doubly-linked list/Traversal#PicoLisp]].


## PL/I


```PL/I

define structure
   1 Node,
      2 value        fixed decimal,
      2 back_pointer handle(Node),
      2 fwd_pointer  handle(Node);

/* Given that 'Current" points at some node in the linked list :    */

P = NEW (: Node :); /* Create a node. */
get (P => value);
P => fwd_pointer = Current => fwd_pointer;
                    /* Points the new node at the next one.         */
Current => fwd_pinter = P;
                    /* Points the current node at the new one.      */
P => back_pointer = Current;
                    /* Points the new node back at the current one. */
Q = P => fwd_pointer;
Q => back_pointer = P;
                    /* Points the next node to the new one.         */

```



## Pop11


```pop11
define insert_double(list, element);
   lvars tmp;
   if list == [] then
      ;;; Insertion into empty list, return element
      element
   else
      next(list) -> tmp;
      list -> prev(element);
      tmp -> next(element);
      element -> next(list);
      if tmp /= [] then
         element -> prev(tmp)
      endif;
      ;;; return original list
      list
   endif;
enddefine;

lvars A = newLink(), B = newLink(), C = newLink();
;;; Build the list of A and B
insert_double(A, B) -> _;
;;; insert C between A and b
insert_double(A, C) -> _;
```


## PureBasic


```PureBasic
Structure node
  *prev.node
  *next.node
  value.c ;use character type for elements in this example
EndStructure

Procedure insertAfter(element.c, *c.node)
  ;insert new node *n after node *c and set it's value to element
  Protected *n.node = AllocateMemory(SizeOf(node))
  If *n
    *n\value = element
    *n\prev = *c
    If *c
      *n\next = *c\next
      *c\next = *n
    EndIf
  EndIf
  ProcedureReturn *n
EndProcedure

Procedure displayList(*dl.node)
  Protected *n.node = *dl
  Repeat
    Print(Chr(*n\Value) + " ")
    *n = *n\next
  Until *n = #Null
EndProcedure

If OpenConsole()
  Define dl.node, *n.node

  *n = insertAfter('A',dl)
  insertAfter('B',*n)
  insertAfter('C',*n)

  displayList(dl)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
A C B
```



## Python



```python
def insert(anchor, new):
    new.next = anchor.next
    new.prev = anchor
    anchor.next.prev = new
    anchor.next = new
```



## Racket

Code is on the [[Doubly-Linked List]] page, in function <code>insert-between</code>.



## REXX

REXX doesn't have linked lists, as there are no pointers (or handles).

However, linked lists can be simulated with lists in REXX.

```rexx
/*REXX program that implements various   List Manager   functions.      */
/*┌────────────────────────────────────────────────────────────────────┐
┌─┘                    Functions of the  List Manager                  └─┐
│                                                                        │
│ @init      ─── initializes the List.                                   │
│                                                                        │
│ @size      ─── returns the size of the List  [could be  0  (zero)].    │
│                                                                        │
│ @show      ─── shows (displays) the complete List.                     │
│ @show k,1  ─── shows (displays) the  Kth  item.                        │
│ @show k,m  ─── shows (displays)   M  items,  starting with  Kth  item. │
│ @show ,,─1 ─── shows (displays) the complete List backwards.           │
│                                                                        │
│ @get  k    ─── returns the  Kth  item.                                 │
│ @get  k,m  ─── returns the  M  items  starting with the  Kth  item.    │
│                                                                        │
│ @put  x    ─── adds the  X  items to the  end  (tail) of the List.     │
│ @put  x,0  ─── adds the  X  items to the start (head) of the List.     │
│ @put  x,k  ─── adds the  X  items to before of the  Kth  item.         │
│                                                                        │
│ @del  k    ─── deletes the item  K.                                    │
│ @del  k,m  ─── deletes the   M  items  starting with item  K.          │
└─┐                                                                    ┌─┘
  └────────────────────────────────────────────────────────────────────┘*/
call sy 'initializing the list.'           ; call @init
call sy 'building list: Was it a cat I saw'; call @put 'Was it a cat I saw'
call sy 'displaying list size.'            ; say 'list size='@size()
call sy 'forward list'                     ; call @show
call sy 'backward list'                    ; call @show ,,-1
call sy 'showing 4th item'                 ; call @show 4,1
call sy 'showing 6th & 6th items'          ; call @show 5,2
call sy 'adding item before item 4: black' ; call @put 'black',4
call sy 'showing list'                     ; call @show
call sy 'adding to tail: there, in the ...'; call @put 'there, in the shadows, stalking its prey (and next meal).'
call sy 'showing list'                     ; call @show
call sy 'adding to head: Oy!'              ; call @put  'Oy!',0
call sy 'showing list'                     ; call @show
exit                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────subroutines─────────────────────────*/
p:       return word(arg(1),1)
sy:      say; say left('',30) "───" arg(1) '───'; return
@hasopt: arg o; return pos(o,opt)\==0
@size:   return $.#
@init:   $.@=''; $.#=0; return 0
@adjust: $.@=space($.@); $.#=words($.@); return 0

@parms:  arg opt
         if @hasopt('k') then k=min($.#+1,max(1,p(k 1)))
         if @hasopt('m') then m=p(m 1)
         if @hasopt('d') then dir=p(dir 1)
         return

@show:   procedure expose $.;   parse arg k,m,dir
         if dir==-1 & k=='' then k=$.#
         m=p(m $.#);
         call @parms 'kmd'
         say @get(k,m,dir);
         return 0

@get:    procedure expose $.;   arg k,m,dir,_
         call @parms 'kmd'
                             do j=k for m by dir while j>0 & j<=$.#
                             _=_ subword($.@,j,1)
                             end   /*j*/
         return strip(_)

@put:    procedure expose $.;   parse arg x,k
         k=p(k $.#+1)
         call @parms 'k'
         $.@=subword($.@,1,max(0,k-1)) x subword($.@,k)
         call @adjust
         return 0

@del:    procedure expose $.;   arg k,m
         call @parms 'km'
         _=subword($.@,k,k-1) subword($.@,k+m)
         $.@=_
         call @adjust
         return
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


```ruby
class DListNode
  def insert_after(search_value, new_value)
    if search_value == value
      new_node = self.class.new(new_value, nil, nil)
      next_node = self.succ
      self.succ = new_node
      new_node.prev = self
      new_node.succ = next_node
      next_node.prev = new_node
    elsif self.succ.nil?
      raise StandardError, "value #{search_value} not found in list"
    else
      self.succ.insert_after(search_value, new_value)
    end
  end
end

head = DListNode.from_array([:a, :b])
head.insert_after(:a, :c)
```



## Rust


###  Simply using the standard library


```rust
use std::collections::LinkedList;
fn main() {
    let mut list = LinkedList::new();
    list.push_front(8);
}
```


=== The behind-the-scenes implementation ===
This expands upon the implementation defined in [[Doubly-linked list/Element definition#The_behind-the-scenes_implementation]] and consists of the relevant lines from the LinkedList implementation in the Rust standard library.


```rust
impl<T> Node<T
 {
    fn new(v: T) -> Node<T> {
        Node {value: v, next: None, prev: Rawlink::none()}
    }
}

impl<T> Rawlink<T> {
    fn none() -> Self {
        Rawlink {p: ptr::null_mut()}
    }

    fn some(n: &mut T) -> Rawlink<T> {
        Rawlink{p: n}
    }
}

impl<'a, T> From<&'a mut Link<T>> for Rawlink<Node<T>> {
    fn from(node: &'a mut Link<T>) -> Self {
        match node.as_mut() {
            None => Rawlink::none(),
            Some(ptr) => Rawlink::some(ptr)
        }
    }
}


fn link_no_prev<T>(mut next: Box<Node<T>>) -> Link<T> {
    next.prev = Rawlink::none();
    Some(next)
}

impl<T> LinkedList<T> {
    #[inline]
    fn push_front_node(&mut self, mut new_head: Box<Node<T>>) {
        match self.list_head {
            None => {
                self.list_head = link_no_prev(new_head);
                self.list_tail = Rawlink::from(&mut self.list_head);
            }
            Some(ref mut head) => {
                new_head.prev = Rawlink::none();
                head.prev = Rawlink::some(&mut *new_head);
                mem::swap(head, &mut new_head);
                head.next = Some(new_head);
            }
        }
        self.length += 1;
    }
    pub fn push_front(&mut self, elt: T) {
        self.push_front_node(Box::new(Node::new(elt)));
    }
}
```



## Tcl

See [[Doubly-Linked List (element)#Tcl|Doubly-Linked List (element)]] for caveats about linked lists in Tcl.


```tcl
oo::define List {
    method insertBefore {elem} {
        $elem next [self]
        $elem previous $prev
        if {$prev ne ""} {
            $prev next $elem
        }
        set prev $elem
    }
    method insertAfter {elem} {
        $elem previous [self]
        $elem next $next
        if {$next ne ""} {
            $next previous $elem
        }
        set next $elem
    }
}
```

Demonstration:

```tcl
set B [List new 3]
set A [List new 1 $B]
set C [List new 2]
$A insertAfter $C
puts [format "{%d,%d,%d}" [$A value] [[$A next] value] [[[$A next] next] value]]
```



## Visual Basic .NET



```vbnet
Public Sub Insert(ByVal a As Node(Of T), ByVal b As Node(Of T), ByVal c As T)
    Dim node As New Node(Of T)(value)
    a.Next = node
    node.Previous = a
    b.Previous = node
    node.Next = b
End Sub
```



## zkl


```zkl
class Node{
   fcn init(_value,_prev=Void,_next=Void)
      { var value=_value, prev=_prev, next=_next; }
   fcn toString{ value.toString() }
   fcn append(value){
      b,c := Node(value,self,next),next;
      next=b;
      if(c) c.prev=b;
      b
   }
}
```


```zkl
a:=Node("a");
a.append("b").append("c");
println(a,"  ",a.next,"  ",a.next.next);
```

```txt

a  b  c

```


{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have user-defined data structures or objects. -->
