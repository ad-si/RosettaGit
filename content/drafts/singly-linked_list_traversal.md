+++
title = "Singly-linked list/Traversal"
description = ""
date = 2019-10-17T04:52:14Z
aliases = []
[extra]
id = 2183
[taxonomies]
categories = []
tags = []
+++

{{task|Data Structures}}[[Category:Iteration]]
Traverse from the beginning of a singly-linked list to the end.

{{Template:See also lists}}





## ACL2

The standard list data type is a singly linked list.

```Lisp
(defun traverse (xs)
   (if (endp xs)
       (cw "End.~%")
       (prog2$ (cw "~x0~%" (first xs))
               (traverse (rest xs)))))
```



## ActionScript

See [[Singly-Linked List (element)#ActionScript|Singly-Linked List (element) in ActionScript]]

```ActionScript
var A:Node;
//...
for(var i:Node = A; i != null; i = i.link)
{
	doStuff(i);
}
```



## Ada

The Ada standard container library provides a doubly-linked list. List traversal is demonstrated for the forward links.


```ada
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_Io; use Ada.Text_Io;

procedure Traversal_Example is
   package Int_List is new Ada.Containers.Doubly_Linked_Lists(Integer);
   use Int_List;
   procedure Print(Position : Cursor) is
   begin
      Put_Line(Integer'Image(Element(Position)));
   end Print;
   The_List : List;
begin
   for I in 1..10 loop
      The_List.Append(I);
   end loop;
   -- Traverse the list, calling Print for each value
   The_List.Iterate(Print'access);
end traversal_example;
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}
Linked lists are not built into ALGOL 68 ''per se'', nor any available standard library.  However Linked lists are presented in standard text 
book examples.  Or can be manually constructed, eg:

```algol68
MODE STRINGLIST = STRUCT(STRING value, REF STRINGLIST next);

STRINGLIST list := ("Big",
  LOC STRINGLIST := ("fjords",
    LOC STRINGLIST := ("vex",
      LOC STRINGLIST := ("quick",
        LOC STRINGLIST := ("waltz",
          LOC STRINGLIST := ("nymph",NIL))))));

REF STRINGLIST node := list;
WHILE node ISNT REF STRINGLIST(NIL) DO
  print((value OF node, space));
  node := next OF node
OD;
print(newline)
```
 
{{out}}

```txt

Big fjords vex quick waltz nymph

```



## ALGOL W


```algolw
begin
    % record type to hold a singly linked list of integers                    %
    record ListI ( integer iValue; reference(ListI) next );

    % inserts a new value after the specified element of a list               %
    procedure insert( reference(ListI) value list
                    ; integer          value newValue
                    ) ;
        next(list) := ListI( newValue, next(list) );

    % declare variables to hold the list                                      %
    reference(ListI) head, pos;

    % create a list of integers                                               %
    head := ListI( 1701, ListI( 9000, ListI( 42, ListI( 90210, null ) ) ) );

    % insert a new value into the list                                        %
    insert( next(head), 4077 );

    % traverse the list                                                       %
    pos := head;

    while pos not = null do begin
        write( iValue(pos) );
        pos := next(pos);
    end;

end.
```

{{out}}

```txt

          1701  
          9000  
          4077  
            42  
         90210  

```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program afficheList.s   */

/* Constantes    */
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ READ,   3
.equ WRITE,  4

.equ NBELEMENTS,      100              @ list size

/*******************************************/
/* Structures                               */
/********************************************/
/* structure linkedlist*/
    .struct  0
llist_next:                            @ next element
    .struct  llist_next + 4 
llist_value:                           @ element value
    .struct  llist_value + 4 
llist_fin:
/* Initialized data */
.data
szMessInitListe:         .asciz "List initialized.\n"
szCarriageReturn:        .asciz "\n"
/* datas error display */
szMessErreur:            .asciz "Error detected.\n"
/* datas message display */
szMessResult:            .ascii "Element No :"
sNumElement:             .space 12,' '
                         .ascii " value :  "
sValue:                  .space 12,' '
                         .asciz "\n"

/* UnInitialized data */
.bss 
lList1:              .skip llist_fin * NBELEMENTS    @ list memory place 
/*  code section */
.text
.global main 
main: 
    ldr r0,iAdrlList1
    mov r1,#0                           @ list init
    str r1,[r0,#llist_next]
    ldr r0,iAdrszMessInitListe
    bl affichageMess
    ldr r0,iAdrlList1
    mov r1,#2
    bl insertElement                    @ add element value 2
    ldr r0,iAdrlList1
    mov r1,#5
    bl insertElement                    @ add element value 5
    @                                   @ display elements of list
    ldr r3,iAdrlList1
    mov r2,#0                           @ ident element
1:
    ldr r0,[r3,#llist_next]             @ end list ?
    cmp r0,#0
    beq 100f                            @ yes
    add r2,#1
    mov r0,r2                           @ display No element and value
    ldr r1,iAdrsNumElement
    bl conversion10S
    ldr r0,[r3,#llist_value]
    ldr r1,iAdrsValue
    bl conversion10S
    ldr r0,iAdrszMessResult
    bl affichageMess
    ldr r3,[r3,#llist_next]             @ next element
    b 1b                                @ and loop
100:                                    @ standard end of the program
    mov r7, #EXIT                       @ request to exit program
    svc 0                               @ perform system call
iAdrszMessInitListe:       .int szMessInitListe
iAdrszMessErreur:          .int szMessErreur
iAdrszCarriageReturn:      .int szCarriageReturn
iAdrlList1:                .int lList1
iAdrszMessResult:          .int szMessResult
iAdrsNumElement:           .int sNumElement
iAdrsValue:                .int sValue

/******************************************************************/
/*     insert element at end of list                          */ 
/******************************************************************/
/* r0 contains the address of the list */
/* r1 contains the value of element  */
/* r0 returns address of element or - 1 if error */
insertElement:
    push {r1-r3,lr}                       @ save  registers 
    mov r2,#llist_fin * NBELEMENTS
    add r2,r0                             @ compute address end list
1:                                        @ start loop 
    ldr r3,[r0,#llist_next]               @ load next pointer
    cmp r3,#0                             @ = zero
    movne r0,r3                           @ no -> loop with pointer
    bne 1b
    add r3,r0,#llist_fin                  @ yes -> compute next free address
    cmp r3,r2                             @ > list end 
    movge r0,#-1                          @ yes -> error
    bge 100f
    str r3,[r0,#llist_next]               @ store next address in current pointer
    str r1,[r0,#llist_value]              @ store element value
    mov r1,#0
    str r1,[r3,#llist_next]               @ init next pointer in next address

100:
    pop {r1-r3,lr}                        @ restaur registers
    bx lr                                 @ return
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
/***************************************************/
/*  Converting a register to a signed decimal      */
/***************************************************/
/* r0 contains value and r1 area address    */
conversion10S:
    push {r0-r4,lr}       @ save registers
    mov r2,r1             @ debut zone stockage
    mov r3,#'+'           @ par defaut le signe est +
    cmp r0,#0             @ negative number ? 
    movlt r3,#'-'         @ yes
    mvnlt r0,r0           @ number inversion
    addlt r0,#1
    mov r4,#10            @ length area
1:                        @ start loop
    bl divisionpar10U
    add r1,#48            @ digit
    strb r1,[r2,r4]       @ store digit on area
    sub r4,r4,#1          @ previous position
    cmp r0,#0             @ stop if quotient = 0
    bne 1b	

    strb r3,[r2,r4]       @ store signe 
    subs r4,r4,#1         @ previous position
    blt  100f             @ if r4 < 0 -> end

    mov r1,#' '           @ space
2:
    strb r1,[r2,r4]       @store byte space
    subs r4,r4,#1         @ previous position
    bge 2b                @ loop if r4 > 0
100: 
    pop {r0-r4,lr}        @ restaur registers
    bx lr  
/***************************************************/
/*   division par 10   unsigned                    */
/***************************************************/
/* r0 dividende   */
/* r0 quotient    */
/* r1 remainder   */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0                                          @ save value
    //mov r3,#0xCCCD                                   @ r3 <- magic_number lower  raspberry 3
    //movt r3,#0xCCCC                                  @ r3 <- magic_number higter raspberry 3
    ldr r3,iMagicNumber                                @ r3 <- magic_number    raspberry 1 2
    umull r1, r2, r3, r0                               @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0) 
    mov r0, r2, LSR #3                                 @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2                               @ r2 <- r0 * 5 
    sub r1,r4,r2, lsl #1                               @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                                              @ leave function 
iMagicNumber:  	.int 0xCCCCCCCD

```


## AutoHotkey


```AutoHotkey
a = 1
a_next = b
b = 2
b_next = c
c = 3

traverse("a")
return

traverse(element)
{
  MsgBox % element . "= " . %element%
  name := element . "_next"
  while, %name%
  {
  element := %name%
  msgbox % %name% . "= " . %element%
  name := %name% . "_next"
  }
}
```



## Axe


```axe
LINK(L₁,1)→A
LINK(L₁+10,2)→B
LINK(L₁+50,3)→C
 
INSERT(A,B)
INSERT(A,C)
 
A→I
While I≠0
 Disp VALUE(I)▶Dec,i
 NEXT(I)→I
End
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      DIM node{pNext%, iData%}
      DIM a{} = node{}, b{} = node{}, c{} = node{}
      
      a.pNext% = b{}
      a.iData% = 123
      b.iData% = 789
      c.iData% = 456
      
      PROCinsert(a{}, c{})
      
      PRINT "Traverse list:"
      pnode% = a{}
      REPEAT
        !(^node{}+4) = pnode%
        PRINT node.iData%
        pnode% = node.pNext%
      UNTIL pnode% = 0
      
      END
      
      DEF PROCinsert(here{}, new{})
      new.pNext% = here.pNext%
      here.pNext% = new{}
      ENDPROC

```

{{out}}

```txt
Traverse list:
       123
       456
       789
```



## C

See [[Singly-Linked List (element)#C|Singly-Linked List (element) in C]].

```c
struct link *first;
// ...
struct link *iter;
for(iter = first; iter != NULL; iter = iter->next) {
  // access data, e.g. with iter->data
}
```



## C++


{{works with|C++11}}
For each traversal version.

```cpp>#include <iostream

#include <forward_list>

int main()
{
    std::forward_list<int> list{1, 2, 3, 4, 5};
    for (int e : list)
        std::cout << e << std::endl;
}
```


=={{header|C sharp|C#}}==
Uses the generic version of the node type located [[Singly-linked_list/Element_definition#C#|here]].


```csharp
var current = [head of list to traverse]
while(current != null)
{
    // Do something with current.Value.

    current = current.Next;
}
```


Alternatively, as a for loop:

```csharp
for (var current = [head of list to traverse]; current != null; current = current.Next)
{
    // Do something with current.Value.
}
```



## Clojure


```lisp
(doseq [x xs] (println x))
```



## Common Lisp


```lisp
(dolist (x list)
  (print x))
```


Using LOOP:


```lisp
(loop for x in list do (print x))
```


Using MAPC


```lisp
(mapc #'print list)
```


Using MAP


```lisp
(map nil #'print list)
```



Not using builtin list iteration:


```lisp
(loop for ref = list then (rest ref)
      until (null ref)
      do (print (first ref)))
```



## Computer/zero Assembly

A linked list can be implemented as a chain of CONS cells, where each cell is made up of two neighbouring memory locations: the CAR, storing an item of data, and the CDR, storing the address of the next cell in the list. The CDR of the last cell contains not an address but a special <tt>NIL</tt> value that is guaranteed not to be a valid address; in this implementation, we use 0 to represent <tt>NIL</tt>. The order of CONS cells in memory is of course entirely unimportant. For the sake of example, this program traverses the list <tt>'(1 2 3 4 5 6)</tt> and halts with the final value in the accumulator. The program is reasonably straightforward, but it does make some use of instruction arithmetic (self-modifying code).

```czasm
start:  LDA  load
        ADD  car   ; head of list
        STA  ldcar

        ADD  one
        STA  ldcdr

ldcar:  NOP
        STA  value

ldcdr:  NOP
        BRZ  done  ; 0 == NIL
        STA  car

        JMP  start

done:   LDA  value
        STP

load:   LDA  0
value:       0
car:         28    ; head of list
one:         1

20,21:       6, 0
22,23:       2, 26
24,25:       5, 20
26,27:       3, 30
28,29:       1, 22
30,31:       4, 24
```



## D


```d
struct SLinkedNode(T) {
    T data;
    typeof(this)* next;
}

void main() {
    import std.stdio;

    alias N = SLinkedNode!int;
    auto lh = new N(1, new N(2, new N(3, new N(4))));

    for (auto p = lh; p; p = p.next)
        write(p.data, " ");
    writeln();
}
```

{{out}}

```txt
1 2 3 4 
```


### Alternative Version

Using tango's collections (written by Doug Lea, ported to D):

```d
import tango.io.Stdout;
import tango.util.collection.LinkSeq;

void main() {
    auto m = new LinkSeq!(char[]);
    m.append("alpha");
    m.append("bravo");
    m.append("charlie");
    foreach (val; m)
        Stdout(val).newline;
}
```



## Delphi


```delphi
uses system ;
 
type
 
   // declare the list pointer type
   plist = ^List ;
 
   // declare the list type, a generic data pointer prev and next pointers
   List = record
      data : pointer ;
      next : pList ;
   end;
 
// since this task is just showing the traversal I am not allocating the memory and setting up the root node etc.
// Note the use of the carat symbol for de-referencing the pointer.
 
begin   
 
   // beginning to end
   while not (pList^.Next = NIL) do pList := pList^.Next ;
 
end;
```



## Dyalect


Dyalect doesn't support linked lists out of the box, but it is fairly simple to implement one:


```dyalect
type List = List(value, tail) | Nil()

static func List.fromArray(xs) {
    var list = List.Nil()
    var len = xs.len()
    for i in (len-1)..0 {
        list = List(xs[i], list)
    }
    return list
}
func List.iter() {
    var xs = valueof(this)
    while true {
        yield xs.value
        if xs.tail is Nil() {
            break
        }
        xs = valueof(xs.tail)
    }
}

var xs = List.fromArray([1..10])

for x in xs {
    print(x)
}
```


It is also possible to provide an ad hoc solution to the problem:

{{trans|E}}


```dyalect
var xs = (1, (2, (3, (4, (5, (6, (7, (8, (9, (10, nil))))))))))

while xs is (value, tail) {
    print(value)
    xs = tail
}
```


Here a linked list is emulated using tuples.


## E

Using a list made from tuples:

```e
var linkedList := [1, [2, [3, [4, [5, [6, [7, null]]]]]]]

while (linkedList =~ [value, next]) {
    println(value)
    linkedList := next
}
```


Using a list made from the structure defined at [[Singly-Linked List (element)#E|Singly-Linked List (element)]]:

```e
var linkedList := makeLink(1, makeLink(2, makeLink(3, empty)))

while (!(linkedList.null())) {
    println(linkedList.value())
    linkedList := linkedList.next()
}
```



## EchoLisp

Lists - linked-lists - are '''the''' fundamental data type in EchoLisp. A lot of fuctions exist to scan lists or operate on successive elements.

```lisp

(define friends '( albert ludwig elvis 🌀))

(for-each write friends)→  albert ludwig elvis 🌀

; for loop
(for ((friend friends)) (write friend)) → albert ludwig elvis 🌀

; map a function
(map string-upcase friends)  → ("ALBERT" "LUDWIG" "ELVIS" "🌀")
(map string-randcase friends) → ("ALBerT" "LudWIG" "elVis" "🌀")

; recursive way
(define (rscan L)
    (unless (null? L) 
        (write (first L)) 
         (rscan (rest L))))

(rscan friends)  →  albert ludwig elvis 🌀

; folding a list
; check that ∑ 1..n = n (n+1)/2

(define L (iota 1001))
(foldl + 0 L) → 500500 ; 1000 * 1001 / 2


```



## Ela


```ela
traverse [] = []
traverse (x::xs) = x :: traverse xs
```


This function traverses a list and constructs a new list at the same time. For a list in Ela it is the same as identity function, e.g. traverse [1,2,3] == [1,2,3]. However it can be useful in some cases. For example, to enforce a lazy list:


```ela
xs = [& x \\ x <- [1..1000]]//lazy list

traverse xs
```


## Elena

Simple iteration with a while loop.

```elena

while(nil != current){ 
    console printLine(current.Item);
    current := current.Next
}
```



## Erlang

Use built in functions like lists:map/2 and lists:foldl/3.

```txt

1> lists:map( fun erlang:is_integer/1, [1,2,3,a,b,c] ).
[true,true,true,false,false,false]
4> lists:foldl( fun erlang:'+'/2, 0, [1,2,3] ).
6

```



## Factor


```factor
: list-each ( linked-list quot: ( data -- ) -- )
    [ [ data>> ] dip call ]
    [ [ next>> ] dip over [ list-each ] [ 2drop ] if ] 2bi ; inline recursive

SYMBOLS: A B C ;

A <linked-list>
[ C <linked-list> list-insert ] keep
[ B <linked-list> list-insert ] keep

[ . ] list-each
```

{{out}}

```txt

A
B
C

```



## Fantom

Using the definitions from [[Singly-Linked_List_(element_insertion)]]:

```fantom
    // traverse the linked list
    Node? current := a
    while (current != null)
    {
      echo (current.value)
      current = current.successor
    }
```



## Forth


```forth
: last ( list -- end )
  begin dup @ while @ repeat ;
```

And here is a function to walk a list, calling an XT on each data cell:

```forth
: walk ( a xt -- )
   >r begin ?dup while
     dup cell+ @ r@ execute
   @ repeat r> drop ;
```


Testing code:
 A ' emit walk <em>ABC ok</em>


## Fortran

Fortran 95. See [[Singly-Linked List (element)#Fortran|Singly-Linked List (element) in Fortran]].

```fortran
subroutine traversal(list,proc)
   type(node), target    :: list
   type(node), pointer   :: current
   interface
      subroutine proc(node)
         real, intent(in) :: node
      end subroutine proc
   end interface
   current => list
   do while ( associated(current) )
      call proc(current%data)
      current => current%next
   end do
end subroutine traversal
```

Print data from all nodes of a singly-linked list:

```fortran
subroutine printNode(data)
   real, intent(in)  :: data
   write (*,*) data
end subroutine

subroutine printAll(list)
   type(node), intent(in)  :: list
   call traversal(list,printNode)
end subroutine printAll
```



## Go

See [[Singly-Linked List (element)#Go|Singly-Linked List (element) in Go]].

```go
start := &Ele{"tacos", nil}
end := start.Append("burritos")
end = end.Append("fajitas")
end = end.Append("enchilatas")
for iter := start; iter != nil; iter = iter.Next {
    fmt.Println(iter)
}
```



## Haskell

Lists are ubiquitous in Haskell, simply use Haskell's <em>map</em> library function: 

```haskell
map (>5) [1..10] -- [False,False,False,False,False,True,True,True,True,True]

map (++ "s") ["Apple", "Orange", "Mango", "Pear"] -- ["Apples","Oranges","Mangos","Pears"]

foldr (+) 0 [1..10] -- prints 55

traverse :: [a] -> [a]
traverse list = map func list
	where func a = -- ...do something with a
```


Note that the <em>traverse</em> function is polymorphic; denoted by <em>traverse :: [a] -> [a]</em> where <em>a</em> can be of any type.

=={{header|Icon}} and {{header|Unicon}}==
Using either the record or class-based definitions from [[Singly-Linked List (element)#Icon_and_Unicon|Singly-Linked List (element) in Icon and Unicon]]:

```Icon
procedure main ()
  ns := Node(1, Node(2, Node (3)))
  until /ns do { # repeat until ns is null
    write (ns.value)
    ns := ns.successor
  }
end
```

Prints the numbers 1, 2, 3 in turn.


## J

Using the implementation mentioned at [[Singly-Linked List (element)#J|Singly-Linked List (element) in J]], we can apply a function <tt>foo</tt> to each node the following way:

```J
foo"0 {:"1 list
```



## Java

{{works with|Java|1.5+}}
For Java.util.LinkedList<T>, use a for each loop (from [[Loop Structures]]):

```java>LinkedList<Type> list = new LinkedList<Type
();

for(Type i: list){
  //each element will be in variable "i"
  System.out.println(i);
}
```

Note that <code>java.util.LinkedList</code> can also perform as a stack, queue, or doubly-linked list.


## JavaScript

Extending [[Singly-Linked_List_(element)#JavaScript]]

```javascript
LinkedList.prototype.traverse = function(func) {
    func(this);
    if (this.next() != null)
        this.next().traverse(func);
}

LinkedList.prototype.print = function() {
    this.traverse( function(node) {print(node.value())} );
}

var head = createLinkedListFromArray([10,20,30,40]);
head.print();
```

Uses the <code>print()</code> function from [[Rhino]]


Alternatively, translating the [[#Haskell | Haskell]] examples in terms of JavaScript's Array.map, Array.reduce, and Array.forEach:


```JavaScript
var map = function (fn, list) {
        return list.map(fn);
    },

    foldr = function (fn, acc, list) {
        var listr = list.slice();
        listr.reverse();

        return listr.reduce(fn, acc);
    },

    traverse = function (list, fn) {
        return list.forEach(fn);
    };

var range = function (m, n) {
    return Array.apply(null, Array(n - m + 1)).map(
        function (x, i) {
            return m + i;
        }
    );
};

//      --> [false, false, false, false, false, true, true, true, true, true]
map(function (x) {
    return x > 5;
}, range(1, 10));

//      --> ["Apples", "Oranges", "Mangos", "Pears"]
map(function (x) {
    return x + 's';
}, ["Apple", "Orange", "Mango", "Pear"])

//      --> 55
foldr(function (acc, x) {
    return acc + x;
}, 0, range(1, 10))


traverse(["Apple", "Orange", "Mango", "Pear"], function (x) {
    console.log(x);
})
/* Apple */
/* Orange */
/* Mango */
/* Pear */
```



## jq

In practice, jq's arrays would probably be used whenever a singly-linked list might otherwise be used, but to illustrate traversal through a linked list, let us use JSON objects with keys "car" and "cdr", where a "cdr" value of null indicates the end of the linked list.

For example:

```jq
def test_list:
  { "car": 1, "cdr": { "car": 2, "cdr": { "car": 3, "cdr": null }}};

```


To illustrate iteration along the links:
 def traverse: .car, (.cdr | if . == null then empty else traverse end);

A more mind-bending approach is to use recurse/1:
  def traverse2: recurse( .cdr ) | .car;

'''test_list | traverse''' and '''test_list | traverse2''' produce identical results: a stream of the "car" values.


## Julia

{{works with|Julia|0.6}}
Julia let you implement list traversal very easily: see [[Singly-linked_list/Element_definition#Julia]] for the <tt>LinkedList</tt> struct definition.


```julia
Base.start(ll::LinkedList) = ll.head
Base.done(ll::LinkedList{T}, st::AbstractNode{T}) where T = st isa EmptyNode
Base.next(ll::LinkedList{T}, st::AbstractNode{T}) where T = st.data, st.next

lst = LinkedList{Int}()
push!(lst, 1)
push!(lst, 2)
push!(lst, 3)

for n in lst
    print(n, " ")
end
```



## Kotlin

Lists in Kotlin may be instanciated from Java classes or from Kotlin methods or extensions.

```scala
fun main(args: Array<String>) {
    val list = IntRange(1, 50).toList()

    // classic traversal:
    for (i in list) { print("%4d ".format(i)); if (i % 10 == 0) println() }

    // list iterator:
    list.asReversed().forEach { print("%4d ".format(it)); if (it % 10 == 1) println() }
}
```

{{out}}

```txt
   1    2    3    4    5    6    7    8    9   10 
  11   12   13   14   15   16   17   18   19   20 
  21   22   23   24   25   26   27   28   29   30 
  31   32   33   34   35   36   37   38   39   40 
  41   42   43   44   45   46   47   48   49   50 
  50   49   48   47   46   45   44   43   42   41 
  40   39   38   37   36   35   34   33   32   31 
  30   29   28   27   26   25   24   23   22   21 
  20   19   18   17   16   15   14   13   12   11 
  10    9    8    7    6    5    4    3    2    1
```


## Limbo

Lists are a built-in type in Limbo.

```Limbo
implement Command;

include "sys.m";
sys: Sys;

include "draw.m";

include "sh.m";

init(nil: ref Draw->Context, nil: list of string)
{
	sys = load Sys Sys->PATH;

	l := list of {1, 2, 3, 4, 5};

	# the unary 'tl' operator gets the tail of a list
	for (; l != nil; l = tl l)
		sys->print("%d\n", hd l);
		# the unary 'hd' operator gets the head of a list
}
```



## Logo

LAST is already a Logo built-in, but it could be defined this way:

```logo
to last :list
  if empty? bf :list [output first :list]
  output last bf :list
end
```



## Logtalk

The built-in list type can be viewed as a singly linked list. 
Traversing can be trivially done using a tail-recursive predicate:

```logtalk

:- object(singly_linked_list).

    :- public(show/0).

    show :-
        traverse([1,2,3]).

    traverse([]).
    traverse([Head| Tail]) :-
        write(Head), nl,
        traverse(Tail).

:- end_object.

```

{{out}}

```text

| ?- singly_linked_list::show.
1
2
3
yes

```



## Mathematica


```Mathematica
Print /@ {"rosettacode", "kitten", "sitting", "rosettacode",  "raisethysword"}
->
rosettacode
kitten
sitting
rosettacode
raisethysword
```


=={{header|MATLAB}} / {{header|Octave}}==

Matlab and Octave do not have pointers. 
Linked lists are implemented as vectors (i.e. arrays of size 1xN)

```Matlab
list = 1:10; 
    for k=1:length(list)
        printf('%i\n',list(k))	
    end; 
```


It is recommended to avoid loops and "vectorize" the code:


```Matlab
  printf('%d\n', list(:));    
```



## MiniScript

We're choosing here to use the built-in list type, rather than make our own from scratch, since this is more representative of how one is likely to actually use MiniScript.

```MiniScript
myList = [2, 4, 6, 8]
for i in myList
    print i
end for
```

{{out}}

```txt

2
4
6
8

```



## NewLISP


```NewLISP
(dolist (x '(a b c d e))
  (println x))
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

proc insertAppend(a, n: var Node) =
  n.next = a.next
  a.next = n

a.insertAppend(b)
b.insertAppend(c)

iterator items(a: Node) =
  var x = a
  while x != nil:
    yield x
    x = x.next

for item in a:
  echo item.data
```



## Objeck


```objeck

for(node := head; node <> Nil; node := node->GetNext();) {
  node->GetValue()->PrintLine();
};

```


=={{header|Objective-C}}==
(See [[Singly-Linked List (element)]])

```objc
RCListElement *current;
for(current=first_of_the_list; current != nil; current = [current next] )
{
  // to get the "datum":
  // id dat_obj = [current datum];
}
```



## OCaml


```ocaml
# let li = ["big"; "fjords"; "vex"; "quick"; "waltz"; "nymph"] in
  List.iter print_endline li ;;
big
fjords
vex
quick
waltz
nymph
- : unit = ()
```



## Oforth


See [[Singly-linked_list/Element_insertion#Oforth|Singly-Linked List/Element_insertion in Oforth]] for the full class definition.
 
Because forEachNext is defined, a linked list responds to all methods defined for Collections : apply, map, filter, ....


```Oforth
: testLink LinkedList new($A, null) dup add($B) dup add($C) ;
testLink apply(#println)
```


{{out}}

```txt

A
C
B

```



## ooRexx

See [[Singly-linked_list/Element_definition#ooRexx|Singly-Linked List/Element Definition in ooRexx]] for the full class definition.

```ooRexx
list=.list~of('A','B','X')
say "Manual list traversal"
index=list~first
loop while index \== .nil
    say list~at(index)
    index = list~next(index)
end

say
say "Do ... Over traversal"
do value over list
    say value
end
```

{{out}}

```txt
Manual list traversal
A
B
X

Do ... Over traversal
A
B
X

```



## Pascal

See [[Singly-linked_list#Delphi | Delphi]]


## Peloton

This makes a list of the Chinese Public Holiday and lists them first till last and then last till first. 

```sgml><@ LETCNSLSTLIT
public holidays|開國紀念日^和平紀念日^婦女節、兒童節合併假期^清明節^國慶日^春節^端午節^中秋節^農曆除夕</@>
<@ OMT>From First to Last</@>
<@ ITEFORSZELSTLIT>public holidays|
<@ SAYLST>...</@><@ ACTMOVFWDLST>...</@>
</@>
<@ OMT>From Last to First (pointer is still at end of list)</@>
<@ ITEFORSZELSTLIT>public holidays|
<@ SAYLST>...</@><@ ACTMOVBKWLST>...</@>
</@>
```

This variable length Simplified Chinese rendition of the same code is

```sgml
<# 指定构造列表字串>public holidays|開國紀念日^和平紀念日^婦女節、兒童節合併假期^清明節^國慶日^春節^端午節^中秋節^農曆除夕</#>
<# 忽略>From First to Last</#>
<# 迭代迭代次数结构大小列表字串>public holidays|
<# 显示列表>...</#><# 运行移位指针向前列表>...</#>
</#>
<# 忽略>From Last to First (pointer is still at end of list)</#>
<# 迭代迭代次数结构大小列表字串>public holidays|
<# 显示列表>...</#><# 运行移位指针向后列表>...</#>
</#>
```



## Perl

We use Class::Tiny to get OO functionality with minimal effort.

```perl
package SSL_Node;
use strict;
use Class::Tiny qw( val next );

sub BUILD {
    my $self = shift;
    exists($self->{val}) or die "Must supply 'val'";
    if (exists $self->{next}) {
        ref($self->{next}) eq 'SSL_Node'
            or die "If supplied, 'next' must be an SSL_Node";
    }
    return;
}

package main;
use strict;
# Construct an example list,
my @vals = 1 .. 10;
my $countdown = SSL_Node->new(val => shift(@vals));
while (@vals) {
    my $head = SSL_Node->new(val => shift(@vals), next => $countdown);
    $countdown = $head;
}
# ...then traverse it.
my $node = $countdown;
while ($node) {
    print $node->val, "... ";
    $node = $node->next;
}
print "\n";
```

{{out}}

```txt
10... 9... 8... 7... 6... 5... 4... 3... 2... 1...

```



## Perl 6


### With <tt>Pair</tt>


Built-in list processing in Perl is not specifically based on singly-linked lists, 
but works at a higher abstraction level that encapsulates such implementation choices.  Nonetheless, it's trivial to use the <tt>Pair</tt> type to build what is essentially a Lisp-style cons list, and in fact, the <tt>=></tt> pair constructor is right associative for precisely that reason.
We traverse such a list here using a 3-part loop:

```perl6
my $list = 1 => 2 => 3 => 4 => 5 => 6 => Mu;

loop (my $l = $list; $l; $l.=value) {
    say $l.key;
}
```

{{out}}

```txt
1
2
3
4
5
6
```

It would be pretty easy to make such lists iterable as normal Perl lists, 
if anyone really cared to...

Well, shoot, let's just go ahead and do it.  
We'll pretend the <tt>Pair</tt> type is really a list type.  
(And we show how you turn an ordinary list into a cons list using a reduction.  
Note how the <tt>[=>]</tt> reduction is also right associative, 
just like the base operator.)

```perl6
use MONKEY-TYPING;
augment class Pair {
    method traverse () {
        gather loop (my $l = self; $l; $l.=value) {
            take $l.key;
        }
    }
}

my $list = [=>] 'Ⅰ' .. 'Ⅻ', Mu;
say ~$list.traverse;
```


{{out}}

```txt
Ⅰ Ⅱ Ⅲ Ⅳ Ⅴ Ⅵ Ⅶ Ⅷ Ⅸ Ⅹ Ⅺ Ⅻ
```



### With custom type


Extending <tt>class Cell</tt> from [[Singly-linked_list/Element_definition#Perl_6]]:


```perl6
    method Seq {
        self, *.next ...^ !*
    }
```


Usage:


```perl6
my $list = (cons 10, (cons 20, (cons 30, Nil)));

for $list.Seq -> $cell {
    say $cell.value;
}
```

{{out}}

```txt
10
20
30
```



## Phix

See also [[Singly-linked_list/Element_removal#Phix|Removal]].

```Phix
enum NEXT,DATA
constant empty_sll = {{1}}
sequence sll = empty_sll

procedure insert_after(object data, integer pos=length(sll))
    sll = append(sll,{sll[pos][NEXT],data})
    sll[pos][NEXT] = length(sll)
end procedure

insert_after("ONE")
insert_after("TWO")
insert_after("THREE")

?sll

procedure show()
integer idx = sll[1][NEXT]
    while idx!=1 do
        ?sll[idx][DATA]
        idx = sll[idx][NEXT]
    end while
end procedure
show()
```

{{out}}

```txt

{{2},{3,"ONE"},{4,"TWO"},{1,"THREE"}}
"ONE"
"TWO"
"THREE"

```



## PicoLisp

We might use map functions

```PicoLisp
(mapc println '(a "cde" (X Y Z) 999))
```

or flow control functions

```PicoLisp
(for X '(a "cde" (X Y Z) 999)
   (println X) )
```

{{out}} for both cases:

```txt
a
"cde"
(X Y Z)
999
```



## PL/I


```pli
*process source attributes xref or(!);
 /*********************************************************************
 * 25.10.2013 Walter Pachl
 * 'set dd:in=d:\sll.txt,recsize(80)'
 * 'sll'
 *********************************************************************/
 sll: Proc Options(main);
 Dcl in       Record Input;
 Dcl sysprint Print;
 Dcl 1 elem Based(p),
      2 next Ptr Init(null()),
      2 value Char(20) Var;
 Dcl head Ptr;
 Dcl p    Ptr;
 Dcl prev Ptr;
 Dcl i    Bin Fixed(31);
 Dcl rec  Char(80) Var;
 Dcl null Builtin;
 On Endfile(in) goto show;
 Do i=1 By 1;
   Read File(in) Into(rec);
   alloc elem set(p);
   If i=1 Then Do;
     head=p;
     prev=head;
     value=rec;
     End;
   Else Do;
     prev->next=p;
     prev=p;
     value=rec;
     End;
   End;

 show:
   p=head;
   Do i=1 By 1 while(p^=null());
     Put Edit(i,p->value)(skip,f(3),x(1),a);
     p=p->next;
     End;
 End;
```

{{out}}

```txt
  1 Walter
  2 Pachl
  3 wrote
  4 this
```



## PureBasic


```PureBasic
Procedure traverse(*node.MyData)
  While *node
    ;access data, i.e. PrintN(Str(*node\Value)) 
    *node = *node\next
  Wend
EndProcedure

;called using
traverse(*firstnode.MyData)
```



## Python


```python
for node in lst:
    print node.value
```

Any Python class can define ''next()'' and ''__iter__()'' methods so that it can be used with the normal ''for'' iteration syntax.  
In this example the "lst" could be an instance of any Python list, tuple, dictionary, or any sort of object which defines an iterator.  
It could also be a generator (a type of function which ''yields'' results upon each successive invocation).  
The notion of a "singly linked list" is somewhat more primitive than normal Python built-in objects.

```python
class LinkedList(object):
  """USELESS academic/classroom example of a linked list implemented in Python.
     Don't ever consider using something this crude!  Use the built-in list() type!
  """
  def __init__(self, value, next):
    self.value = value;
    self.next = next
  def __iter__(self):
    node = self
    while node != None:
      yield node.value
      node = node.next;

lst = LinkedList("big",  next=
  LinkedList(value="fjords",next=
    LinkedList(value="vex",   next=
      LinkedList(value="quick", next=
        LinkedList(value="waltz", next=
          LinkedList(value="nymph", next=None))))));

for value in lst:
  print value,;
print
```

{{out}}

```txt

big fjords vex quick waltz nymph

```



## Racket


Since singly-linked lists that are made of <tt>cons</tt> cells are one of the most common primitive types in Racket, there is a lot of built-in functionality that scans these lists:


```Racket

#lang racket

(define l (list 1 2 3))

;; scan the list and collect a list of function results
(map add1 l)

;; scan the list and run some function on each element for its side-effect
(for-each displayln l)

;; scan a list and sum up its elements
(foldl + 0 l)

;; same as the above three, using a more modern syntax that is often
;; more convenient
(for/list ([x (in-list l)]) (add1 x))
(for ([x (in-list l)]) (displayln x))
(for/fold ([sum 0]) ([x (in-list l)]) (+ x sum))

;; the same as the first, but make up a vector of results
(for/vector ([x (in-list l)]) (add1 x))

;; there is less support for mutable pairs, but it's still extensive
;; enough to cover all the basics
(require racket/mpair)
(define ml (mlist 1 2 3))
(mmap add1 ml)
(mfor-each displayln ml)
(for ([x (in-mlist ml)]) (displayln x))

```



## Retro


```Retro
: traverse ( l- ) repeat @ 0; again ;
```

Or, using combinators:

```Retro
last [ drop ] ^types'LIST each@
```

With combinators you can also perform an operation on each element in a linked list:

```Retro
last [ @d->name puts space ] ^types'LIST each@
```



## REXX


```rexx
/* REXX ********************************************************************
* 25.10.2013 Walter Pachl
*********************************************************************/
in='d:\sll.txt'
Do i=1 By 1 while lines(in)>0
  rec=linein(in)
  elem.i.val=rec
  elem.i.next=0
  ip=i-1
  elem.ip.next=i
  End;
c=1
Do While c<>0
  Say c elem.c.val
  c=elem.c.next
  End
```



## Ruby

referring to [[Singly-Linked List (element)#Ruby]] and [[Singly-Linked List (element insertion)#Ruby]]

```ruby
head = ListNode.new("a", ListNode.new("b", ListNode.new("c")))
head.insertAfter("b", "b+")

# then:
head.each {|node| print node.value, ","}
puts

# or
current = head
begin
  print current.value, ","
end while current = current.succ
puts
```

{{out}}

```txt
a,b,b+,c,
a,b,b+,c,
```



## Run BASIC


```runbasic
list$ = "now is the time for all good men"
for lnk = 1 to 8
 print lnk;"->";word$(list$,lnk)
next lnk
```

{{out}}

```txt
1->now
2->is
3->the
4->time
5->for
6->all
7->good
8->men
```



## Rust

Extending [[Singly-Linked List (element)#Rust]]. Please see that page for the Linked List struct declarations.

In Rust, there are three ways to pass something: by value (which forfeits ownership), by reference (there can be infinitely many immutable references to an object), or by mutable reference (there may only be one mutable reference and no other immutable ones).

The following will demonstrate iteration all three ways.


```rust
// 
//
// Iteration by value (simply empties the list as the caller now owns all values)
//
//
pub struct IntoIter<T>(List<T>);

impl<T> Iterator for IntoIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.head.take().map(|node| { 
            let node = *node;
            self.0.head = node.next;
            node.elem
        })
    }
}

//
//
// Iteration by immutable reference
//
//

pub struct Iter<'a, T: 'a> {
    next: Option<&'a Node<T>>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        self.next.take().map(|node| {
            self.next = node.next.as_ref().map(|node| &**node);
            &node.elem
        })
    }
}

//
//
// Iteration by mutable reference
//
//

pub struct IterMut<'a, T: 'a> {
    next: Option<&'a mut Node<T>>,
}

impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;
    fn next(&mut self) -> Option<Self::Item> {
        self.next.take().map(|node| {
            self.next = node.next.as_mut().map(|node| &mut **node);
            &mut node.elem
        })
    }
}

//
//
// Methods implemented for List<T>
//
//

impl<T> List<T> {
    pub fn into_iter(self) -> IntoIter<T> {
        IntoIter(self)
    }

    pub fn iter<'a>(&'a self) -> Iter<'a,T> {
        Iter { next: self.head.as_ref().map(|node| &**node) }
    }

    pub fn iter_mut(&mut self) -> IterMut<T> {
        IterMut { next: self.head.as_mut().map(|node| &mut **node) }
    }

}
```



## Scala

You can use pattern matching for traversing a list.


```scala

/*
Here is a basic list definition

sealed trait List[+A]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
case object Nil extends List[Nothing]
*/

def traverse[A](as: List[A]): Unit = as match {
  case Nil => print("End")
  case Cons(h, t) => {
    print(h + " ")
    traverse(t)
  }
}

```



## Scheme


```scheme
(define (traverse seq func)
  (if (null? seq)
      '()
      (begin
        (func (car seq))
        (traverse (cdr seq) func))))
```



## Sidef


```ruby
var list = 'a':'b':'c':nil;
#var list = ['a', ['b', ['c']]];
#var list = Pair.new('a', Pair.new('b', Pair.new('c', nil)));

for (var l = list; l != nil; l = l[1]) {
    say l[0];
}
```

{{out}}

```txt

a
b
c

```



## SSEM

Linked lists are a comparatively easy data structure to implement in machine language, although the SSEM does not really have enough storage to make them practically useful. A linked list consists of any number of cons cells, i.e. pairs of successive words in storage where the first word holds a data item and the second holds either a pointer to the next pair or else a special nil value—represented here by 0, although any negative address would also work—indicating we have reached the end of the list. The pairs or cons cells can be scattered arbitrarily through the available storage space. This program traverses the list <tt>'(1 2 3)</tt>, and halts with the last value in the accumulator. It makes some use of instruction arithmetic (self-modifying code).

```ssem
11101000000000100000000000000000   0. -23 to c
10011000000000010000000000000000   1. Sub. 25
10010000000001100000000000000000   2. c to 9
10101000000000010000000000000000   3. Sub. 21
11010000000001100000000000000000   4. c to 11
10010000000000100000000000000000   5. -9 to c
10010000000001100000000000000000   6. c to 9
11010000000000100000000000000000   7. -11 to c
11010000000001100000000000000000   8. c to 11
00000000000000000000000000000000   9. to be generated at run time
00101000000001100000000000000000  10. c to 20
00000000000000000000000000000000  11. to be generated at run time
00000000000000110000000000000000  12. Test
00011000000000000000000000000000  13. 24 to CI
10011000000001100000000000000000  14. c to 25
10011000000000100000000000000000  15. -25 to c
10011000000001100000000000000000  16. c to 25
01101000000000000000000000000000  17. 22 to CI
00101000000000100000000000000000  18. -20 to c
00000000000001110000000000000000  19. Stop
00000000000000000000000000000000  20. variable: negation of car
10000000000000000000000000000000  21. constant 1
11111111111111111111111111111111  22. constant -1
00000000000000100000000000000000  23. -0 to c
10001000000000000000000000000000  24. constant 17 (jump target)
00111000000000000000000000000000  25. 28 (pointer variable)
01000000000000000000000000000000  26. 2
01111000000000000000000000000000  27. pointer: 30
10000000000000000000000000000000  28. 1
01011000000000000000000000000000  29. pointer: 26
11000000000000000000000000000000  30. 3
00000000000000000000000000000000  31. 0 (nil)
```

SSEM programs can be difficult to take in: the constant negations, subtractions, and indirect jumps often obscure the underlying algorithm. To clarify what is going on, here is a pseudocode version of the same program:

```txt
start:    load         loadZero
          add          pointer
          store        loadCar
          add          #1
          store        loadCdr
loadCar:  ; generated at run time
          store        value
loadCdr:  ; generated at run time
          branchOnZero end
          store        pointer
          jump         start
end:      load         value
          halt
value:            0 ; variable
loadZero: load         #0
pointer:          28

26 and 27:        (2 . 30)
28 and 29:        (1 . 26)
30 and 31:        (3 . 0)
```



## Stata

See [[Singly-linked list/Element definition#Stata]].


## Tcl

Using the class definition from [[Singly-Linked List (element)#Tcl|Singly-Linked List (element)]] (and bearing in mind the general notes on lists given there) we'll modify that class so that lists have an iteration method...
{{works with|Tcl|8.6}}

```tcl
oo::define List {
    method for {varName script} {
        upvar 1 $varName var
        set elem [self]
        while {$elem ne ""} {
            set var [$elem value]
            uplevel 1 $script
            set elem [$elem next]
        }
    }
}
```

Now, a demonstration...

```tcl
set list {}
foreach n {1 3 5 7 2 4 6 8} {
    set list [List new $n $list]
}
$list for x {
    puts "we have a $x in the list"
}
```



## Trith


```trith
[1 2 3 4 5] [print] each
```



## Visual Basic .NET


```vbnet
Private Sub Iterate(ByVal list As LinkedList(Of Integer))
    Dim node = list.First
    Do Until node Is Nothing
        node = node.Next
    Loop
    End Sub
```



## Wart


```wart
each x '(1 2 3)
  prn x
```



## XPL0


```XPL0
repeat Node:= Node(0) until Node=0
```



## zkl


```zkl
foreach n in (List(1,2,3) {...}
List(1,2,3).pump(...) // traverse and munge elements, generalized apply/map
List(1,2,3).filter(...)
List(1,2,3).filter22(...) // partition list
List(1,2,3).reduce(...)
List(1,2,3).apply(...)
List(1,2,3).sum()
List(1,2,3).run()  // treat each element as f, perform f()
List(1,2,3).enumerate()
List(1,2,3).reverse()
List(1,2,3).concat()
List(1,2,3).shuffle()
```

