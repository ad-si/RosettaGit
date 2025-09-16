+++
title = "Singly-linked list/Element insertion"
description = ""
date = 2019-08-28T20:52:00Z
aliases = []
[extra]
id = 2007
[taxonomies]
categories = ["task", "Data Structures"]
languages = [
  "360_assembly",
  "acl2",
  "algol_68",
  "algol_w",
  "arm_assembly",
  "actionscript",
  "ada",
  "auto_hotkey",
  "axe",
  "bbc_basic",
  "c",
  "c_sharp",
  "c_plus_plus",
  "clojure",
  "common_lisp",
  "d",
  "delphi",
  "e",
  "echo_lisp",
  "elena",
  "erlang",
  "factor",
  "forth",
  "fortran",
  "go",
  "groovy",
  "haskell",
  "icon",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "logo",
  "mathematica",
  "nim",
  "ocaml",
  "oforth",
  "pl_i",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "pico_lisp",
  "pop11",
  "purebasic",
  "python",
  "rexx",
  "racket",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "sidef",
  "stata",
  "tcl",
  "x86_assembly",
  "oo_rexx",
  "zkl",
]
tags = []
+++

## Task

{{task|Data Structures}}Using the link element defined in [[Singly-Linked List (element)]], define a method to insert an element into a [[singly-linked list]] following a given element.

Using this method, insert an element C into a list comprised of elements A->B, following element A.

## 360 Assembly

The program uses one ASSIST macro (XPRNT) to keep the code as short as possible.

```360asm
*        Singly-linked list - Insert after  01/02/2017
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

```txt

A
B
C

```



## ACL2


```Lisp
(defun insert-after (x e xs)
   (cond ((endp xs)
          nil)
         ((equal x (first xs))
          (cons (first xs)
                (cons e (rest xs))))
         (t (cons (first xs)
                  (insert-after x e (rest xs))))))
```


Example:

```txt
&gt;(insert-after 'A 'C '(A B))
(A C B)
```



## ActionScript

Insertion method:

```ActionScript
package
{
	public class Node
	{
		public var data:Object = null;
		public var link:Node = null;

		public function insert(node:Node):void
		{
			node.link = link;
			link = node;
		}
	}
}
```

Usage:

```ActionScript
import Node;

var A:Node = new Node(1);
var B:Node = new Node(2);
var C:Node = new Node(3);
A.insert(B);
A.insert(C);
```



## Ada

We must create a context clause making the predefined generic procedure Ada.Unchecked_Deallocation visible to this program.

```ada
with Ada.Unchecked_Deallocation;
-- Define the link type
procedure Singly_Linked is

   type Link;
   type Link_Access is access Link;
   type Link is record
      Data : Integer;
      Next : Link_Access := null;
   end record;
   -- Instantiate the generic deallocator for the link type
   procedure Free is new Ada.Unchecked_Deallocation(Link, Link_Access);

   -- Define the procedure
   procedure Insert_Append(Anchor : Link_Access; Newbie : Link_Access) is
   begin
      if Anchor /= null and Newbie /= null then
         Newbie.Next := Anchor.Next;
         Anchor.Next := Newbie;
      end if;
   end Insert_Append;

   -- Create the link elements
   A : Link_Access := new Link'(1, null);
   B : Link_Access := new Link'(2, null);
   C : Link_Access := new Link'(3, null);
-- Execute the program
begin
   Insert_Append(A, B);
   Insert_Append(A, C);
   Free(A);
   Free(B);
   Free(C);
end Singly_Linked;
```


## ALGOL 68

Linked lists are not built into ALGOL 68 ''per se'', nor any available
standard library.  However Linked lists are presented in standard text
book examples.  Or can be manually constructed, eg:

```algol68
MODE STRINGLIST = STRUCT(STRING value, REF STRINGLIST next);

STRINGLIST list := ("Big",
  LOC STRINGLIST := ("fjords",
    LOC STRINGLIST := ("vex",
      LOC STRINGLIST := ("quick",
        LOC STRINGLIST := ("waltz",
          LOC STRINGLIST := ("nymph",NIL))))));

PROC insert = (REF STRINGLIST list, node)VOID: (
  next OF node := next OF list;
  next OF list := node
);

STRINGLIST very := ("VERY", NIL);

# EXAMPLE OF INSERTION #
insert(next OF next OF list, very );

REF STRINGLIST node := list;
WHILE REF STRINGLIST(node) ISNT NIL DO
  print((value OF node, space));
  node := next OF node
OD;
print((newline))
```

Output:
```txt
Big fjords vex VERY quick waltz nymph
```



## ALGOL W


```algolw
    % inserts a new value after the specified element of a list               %
    procedure insert( reference(ListI) value list
                    ; integer          value newValue
                    ) ;
        next(list) := ListI( newValue, next(list) );

    % declare a variable to hold a list                                       %
    reference(ListI) head;

    % create a list of integers                                               %
    head := ListI( 1701, ListI( 9000, ListI( 42, ListI( 90210, null ) ) ) );

    % insert a new value into the list                                        %
    insert( next(head), 4077 );
```


## ARM Assembly

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program insertList.s   */

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
llist_next:                               @ next element
    .struct  llist_next + 4
llist_value:                              @ element value
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
b_next = 0
c = 3
insert_after("c", "a")
Listvars
msgbox
return

insert_after(new, old)
{
  local temp
  temp := %old%_next
  %old%_next := new
  %new%_next := temp
}
```



## Axe


```axe
Lbl INSERT
{r₁+2}ʳ→{r₂+2}ʳ
r₂→{r₁+2}ʳ
r₁
Return
```



## BBC BASIC

```bbcbasic
      DIM node{pNext%, iData%}
      DIM a{} = node{}, b{} = node{}, c{} = node{}

      a.pNext% = b{}
      a.iData% = 123
      b.iData% = 789
      c.iData% = 456

      PROCinsert(a{}, c{})
      END

      DEF PROCinsert(here{}, new{})
      new.pNext% = here.pNext%
      here.pNext% = new{}
      ENDPROC

```



## C


Define the method:


```c
void insert_append (link *anchor, link *newlink) {
  newlink->next = anchor->next;
  anchor->next = newlink;
}
```


Note that in a production implementation, one should check anchor and newlink to ensure they're valid values. (I.e., not NULL.)

And now on to the code.

Create our links.

```c
link *a, *b, *c;
a = malloc(sizeof(link));
b = malloc(sizeof(link));
c = malloc(sizeof(link));
a->data = 1;
b->data = 2;
c->data = 3;
```


Prepare our initial list

```c
 insert_append (a, b);
```


Insert element c after element a

```c
 insert_append (a, c);
```


Remember to free the memory once we're done.

```c
 free (a);
 free (b);
 free (c);
```



## C++

This uses the generic version of the link node. Of course, normally this would be just some implementation detail inside some list class, not to be used directly by client code.


```cpp
template<typename T>

void insert_after(link<T>* list_node, link<T>* new_node)
{
  new_node->next = list_node->next;
  list_node->next = new_node;
};
```


Here's the example code using that method:

The following code creates the links. As numeric values I've just taken the corresponding character values.

```cpp
link<int>
* a = new link<int>('A', new link<int>('B'));
link<int>* c = new link<int>('C');
```


Now insert c after a:

```cpp
 insert_after(a, c);
```


Finally destroy the list:

```cpp
while (a)
{
  link<int>* tmp = a;
  a = a->next;
  delete tmp;
}
```


## C#
Uses the generic version of the node type located [[Singly-linked_list/Element_definition#C#|here]].

Creates nodes and inserts them from the data passed.

```csharp
static void InsertAfter<T>
(LinkedListNode<T> prev, T value)
{
    prev.Next = new Link() { Value = value, Next = prev.Next };
}
```



```c#
static void Main()
{
    //Create A(5)->B(7)
    var A = new LinkedListNode<int>() { Value = 5 };
    InsertAfter(A, 7);
    //Insert C between A and B
    InsertAfter(A, 15);
}
```



## Clojure



```lisp
(defn insert-after [new old ls]
  (cond (empty? ls) ls
        (= (first ls) old) (cons old (cons new (rest ls)))
        :else (cons (first ls) (insert-after new old (rest ls)))))
```


And the test:

```lisp
user=>
 (insert-after 'c 'a '(a b))
(a c b)
```



## Common Lisp


For many list manipulations in Common Lisp, there are both destructive and non-destructive versions.  <code>insert-after</code> is non-destructive, copying the structure of list up to and including the occurrence of the old-element, and sharing the list structure afterward. <code>ninsert-after</code> may modify the structure of the input list.


```lisp
(defun insert-after (new-element old-element list &key (test 'eql))
  "Return a list like list, but with new-element appearing after the
first occurence of old-element. If old-element does not appear in
list, then a list returning just new-element is returned."
  (if (endp list) (list new-element)
    (do ((head (list (first list)) (cons (first tail) head))
         (tail (rest list) (rest tail)))
        ((or (endp tail) (funcall test old-element (first head)))
         (nreconc head (cons new-element tail))))))

(defun ninsert-after (new-element old-element list &key (test 'eql))
  "Like insert-after, but modifies list in place.  If list is empty, a
new list containing just new-element is returned."
  (if (endp list) (list new-element)
    (do ((prev list next)
         (next (cdr list) (cdr next)))
        ((or (null next) (funcall test old-element (car prev)))
         (rplacd prev (cons new-element next))
         list))))
```


A simpler implementation that traverses the list a bit more can also be written.  This takes advantage of the fact that member returns the tail of the list beginning with the first occurrence of an item, and that ldiff copies as much of its list argument as necessary.


```lisp
(defun simple-insert-after (new-element old-element list &key (test 'eql))
  (let ((tail (rest (member old-element list :test test))))
    (nconc (ldiff list tail)
           (cons new-element tail))))
```


Lastly, here is a recursive version. Case 3 could be optimized by only doing the rplacd operation when the recursive call returns a tail whose first cell is now different compared to that of the previous tail. (I.e. the recursive call has immediately hit case 1 or 2 which allocate new structure.)


```lisp
(defun insert-after (list new existing &key (test #'eql))
"Insert item new into list, before existing, or at the end if existing
is not present. The default comparison test function is EQL. This
function destroys the original list and returns the new list."
  (cond
    ;; case 1: list is empty: just return list of new
    ((endp list)
     (list new))
    ;; case 2: existing element is first element of list
    ((funcall test (car list) existing)
     `(,(car list) ,new ,@(cdr list)))
    ;; case 3: recurse: insert the element into the rest of the list,
    ;; and make that list the new rest.
    (t (rplacd list (insert-before (cdr list) new existing :test test))
       list)))
```



## D


```d
struct SLinkedNode(T) {
    T data;
    typeof(this)* next;
}

void insertAfter(T)(SLinkedNode!T* listNode, SLinkedNode!T* newNode) {
    newNode.next = listNode.next;
    listNode.next = newNode;
}

void main() {
    alias N = SLinkedNode!char;

    auto lh = new N('A', new N('B'));
    auto c = new N('C');

    // Inserts C after A, creating the (A C B) list:
    insertAfter(lh, c);

    // The GC will collect the memory.
}
```



## Delphi


A simple insertion into a one way list. I use a generic pointer for the data that way it can point to any structure, individual variable or whatever. '''NOTE:''' For original versions of Turbo Pascal, substitute the MemAvail Function for the Try Except block as this does not exist in this version of the pascal language. Also, Turbo Pascal doesn't have C++-style comments, therefore those have to be replaced with Pascal style comments, i.e. { ... } or (* ... *).


```delphi
// Using the same type defs from the one way list example.

Type

  // The pointer to the list structure
  pOneWayList = ^OneWayList;

  // The list structure
  OneWayList = record
                 pData : pointer ;
                 Next  : pOneWayList ;
               end;

// I will illustrate a simple function that will return a pointer to the
// new node or it will return NIL.  In this example I will always insert
// right, to keep the code clear.  Since I am using a function all operations
// for the new node will be conducted on the functions result.  This seems
// somewhat counter intuitive, but it is the simplest way to accomplish this.

Function InsertNode(VAR CurrentNode:pOneWayList): pOneWayList
begin

    // I try not to introduce different parts of the language, and keep each
    // example to just the code required.  in this case it is important to use
    // a try/except block.  In any OS that is multi-threaded and has many apps
    // running at the same time, you cannot rely on a call to check memory available
    // and then attempting to allocate.  In the time between the two, another
    // program may have grabbed the memory you were trying to get.

    Try
      // Try to allocate enough memory for a variable the size of OneWayList
      GetMem(Result,SizeOf(OneWayList));
    Except
      On EOutOfMemoryError do
         begin
           Result := NIL
           exit;
         end;
    end;

    // Initialize the variable.
    Result.Next  := NIL ;
    Reuslt.pdata := NIL ;

    // Ok now we will insert to the right.

    // Is the Next pointer of CurrentNode Nil?  If it is we are just tacking
    // on to the end of the list.

    if CurrentNode.Next = NIL then
       CurrentNode.Next := Result
    else
      // We are inserting into the middle of this list
      Begin
         Result.Next      := CurrentNode.Next ;
         CurrentNode.Next := result ;
      end;
end;
```



## E



```e
def insertAfter(head :LinkedList ? (!head.null()),
                new  :LinkedList ? (new.next().null())) {
    new.setNext(head.next())
    head.setNext(new)
}

def a := makeLink(1, empty)
def b := makeLink(2, empty)
def c := makeLink(3, empty)

insertAfter(a, b)
insertAfter(a, c)

var x := a
while (!x.null()) {
    println(x.value())
    x := x.next()
}
```



## EchoLisp

Lists are mutable, and we use the destructive - and dangerous - set-cdr! operator which modifies the 'rest' part of a list or sub-list.

```lisp

(define (insert-after lst target item)
    (when (null? lst) (error "cannot insert in" null))
    (let [(sub-list (member target lst))]
        (if sub-list (set-cdr! sub-list (cons item (cdr sub-list))) ; make chirurgy if found
        (nconc lst item)))) ; else append item

(define L '(a b))
(insert-after L 'a 'c)
    L  → (a c b)
(insert-after L 'x 'y)
    L → (a c b y)

```


## Elena


```elena
singleton linkHelper
{
    insertAfter(Link prev, IntNumber i)
    {
        prev.Next := new Link(i, prev.Next)
    }
}
```



## Erlang

Lists are builtin, but Erlang is single assignment. Here we need mutable link to next element. Mutable in Erlang usually means a process, so:

```Erlang

-module( singly_linked_list ).

-export( [append/2, foreach/2, free/1, insert/3, new/1, task/0] ).

append( New, Start ) -> Start ! {append, New}.

foreach( Fun, Start ) -> Start ! {foreach, Fun}.

free( Element ) -> Element ! {free}.

insert( New, After, Start ) -> Start ! {insert, New, After}.

new( Data ) -> erlang:spawn( fun() -> loop( Data, nonext ) end ).

task() ->
    A = new( a ),
    B = new( b ),
    append( B, A ),
    C = new( c ),
    insert( C, A, A ),
    foreach( fun(Data) -> io:fwrite("~p~n", [Data]) end, A ).



loop( Data, Next ) ->
      My_pid = erlang:self(),
      receive
      {append, New} ->
             New_next = loop_append( New, Next ),
             loop( Data, New_next );
      {foreach, Fun} ->
                catch Fun( Data ),
		loop_foreach( Fun, Next ),
                loop( Data, Next );
      {free} ->
             ok;
      {insert, New, My_pid} ->
             append( Next, New ),
             loop( Data, New );
      {insert, New, After} ->
             Next ! {insert, New, After},
             loop( Data, Next )
        end.

loop_append( New, nonext ) -> New;
loop_append( New, Next ) ->
        Next ! {append, New},
        Next.

loop_foreach( _Fun, nonext ) -> ok;
loop_foreach( Fun, Next ) -> Next ! {foreach, Fun}.

```

```txt

4> singly_linked_list:task().
a
c
b

```



## Factor


```factor
: list-append ( previous new -- )
    [ swap next>> >>next drop ] [ >>next drop ] 2bi ;

SYMBOLS: A B C ;

A <linked-list>
[ C <linked-list> list-append ] keep
[ B <linked-list> list-append ] keep
.
```

Output:

```txt

T{ linked-list
    { data A }
    { next
        T{ linked-list
            { data B }
            { next T{ linked-list { data C } } }
        }
    }
}

```



## Fantom


Extending Node class from [[Singly-Linked_List_(element)]]:


```fantom

class Node
{
  const Int value
  Node? successor // can be null, for end of series

  new make (Int value, Node? successor := null)
  {
    this.value = value
    this.successor = successor
  }

  // insert method for this problem
  public Void insert (Node newNode)
  {
    newNode.successor = this.successor
    this.successor = newNode
  }
}

// simple class to test putting 'c' between 'a' and 'b'
class Main
{
  public static Void main ()
  {
    c := Node (2)
    b := Node (3)
    a := Node (1, b)
    a.insert (c)

    echo (a.value)
    echo (a.successor.value)
    echo (a.successor.successor.value)
  }
}

```


Output:

```txt

1
2
3

```



## Forth


Using the linked list concept described in the [[Singly-Linked_List_(element)]] topic:

```forth
\ Create the list and some list elements
create A   0 , char A ,
create B   0 , char B ,
create C   0 , char C ,
```


Now insert b after a and c after b, giving a->b->c

```forth
B A chain
C B chain
```


Here is an abbreviated version of the definition of 'chain' from the other article:

```forth
 : chain ( a b -- )   2dup  @ swap !  ! ;
```



## Fortran

In ISO Fortran 95 or later:

```fortran
elemental subroutine addAfter(nodeBefore,value)
   type (node), intent(inout) :: nodeBefore
   real, intent(in)           :: value
   type (node), pointer       :: newNode

   allocate(newNode)
   newNode%data = value
   newNode%next => nodeBefore%next
   nodeBefore%next => newNode
end subroutine addAfter
```



## Go


```go
package main

import "fmt"

type Ele struct {
    Data interface{}
    Next *Ele
}

func (e *Ele) insert(data interface{}) {
    if e == nil {
        panic("attept to modify nil")
    }
    e.Next = &Ele{data, e.Next}
}

func (e *Ele) printList() {
    if e == nil {
        fmt.Println(nil)
        return
    }
    fmt.Printf("(%v", e.Data)
    for {
        e = e.Next
        if e == nil {
            fmt.Println(")")
            return
        }
        fmt.Print(" ", e.Data)
    }
}

func main() {
    h := &Ele{"A", &Ele{"B", nil}}
    h.printList()
    h.insert("C")
    h.printList()
}
```

Output:

```txt

(A B)
(A C B)

```



## Groovy

Solution (uses ListNode from [[Singly-Linked List (element)#Groovy]]):

```groovy
class NodeList {
    private enum Flag { FRONT }
    private ListNode head
    void insert(value, insertionPoint=Flag.FRONT) {
        if (insertionPoint == Flag.FRONT) {
            head = new ListNode(payload: value, next: head)
        } else {
            def node = head
            while (node.payload != insertionPoint) {
                node = node.next
                if (node == null) {
                    throw new IllegalArgumentException(
                        "Insertion point ${afterValue} not already contained in list")
                }
            }
            node.next = new ListNode(payload:value, next:node.next)
        }
    }
    String toString() { "${head}" }
}
```


Test:

```groovy
def list = new NodeList()
list.insert('B')
list.insert('A')
println list

list.insert('C', 'A')
println list
```


Output:

```txt
A -> B -> null
A -> C -> B -> null
```



## Haskell

This kind of list manipulation is [[unidiomatic]] Haskell. But you can try the following:

```haskell
insertAfter a b (c:cs) | a==c = a : b : cs
                       | otherwise = c : insertAfter a b cs
insertAfter _ _ [] = error "Can't insert"
```


==Icon and Unicon==

The Icon solution works for both Icon and Unicon, but Unicon permits a class-based solution.

=
## Icon
=


```Icon

record Node (value, successor)

procedure insert_node (node, newNode)
  newNode.successor := node.successor
  node.successor := newNode
end

```


=
## Unicon
=


```Unicon

class Node (value, successor)

  method insert (node)
    node.successor := self.successor
    self.successor := node
  end

  initially (value, successor)
    self.value := value
    self.successor := successor
end

```



## J



```J
list=: 1 65,:_ 66
A=:0  NB. reference into list
B=:1  NB. reference into list
insertAfter=: monad define
   'localListName localListNode localNewValue'=. y
   localListValue=: ".localListName
   localOldLinkRef=: <localListNode,0
   localNewLinkRef=: #localListValue
   localNewNode=: (localOldLinkRef { localListValue), localNewValue
   (localListName)=: (localNewLinkRef localOldLinkRef} localListValue), localNewNode
)
```


With these definitions:

    insertAfter 'list';A;67

updates the list inserting the value for C after the value for A.

That said, note that the underlying mechanism is rather silly, for J.  Linked lists are only interesting in J for illustrative purposes, and should not be used in code that anyone cares about.  I have supplied a correspondingly verbose implementation.


## Java

Extending [[Singly-Linked_List_(element)#Java]]

```Java
void insertNode(Node<T> anchor_node, Node<T> new_node)
{
    new_node.next = anchor_node.next;
    anchor_node.next = new_node;
}
```

Java allows the use of generics to allow the data type to be determined at compile time. This will only work on reference types, not primitive types like int or float (wrapper classes like Integer and Float are available).


## JavaScript

Extending [[Singly-Linked_List_(element)#JavaScript]]

```javascript
LinkedList.prototype.insertAfter = function(searchValue, nodeToInsert) {
    if (this._value == searchValue) {
        nodeToInsert.next(this.next());
        this.next(nodeToInsert);
    }
    else if (this.next() == null)
        throw new Error(0, "value '" + searchValue + "' not found in linked list.")
    else
        this.next().insertAfter(searchValue, nodeToInsert);
}
var list = createLinkedListFromArray(['A','B']);
list.insertAfter('A', new LinkedList('C', null));
```



## Julia

See the <tt>LinkedList</tt> implemented at [[Singly-linked_list/Element_definition#Julia]].


```julia
function Base.insert!(ll::LinkedList{T}, index::Integer, item::T) where T
    if index == 1
        if isempty(ll)
            return push!(ll, item)
        else
            ll.head = Node{T}(item, ll.head)
        end
    else
        nd = ll.head
        while index > 2
            if nd.next isa EmptyNode
                throw(BoundsError())
            else
                nd = nd.next
                index -= 1
            end
        end
        nd.next = Node{T}(item, nd.next)
    end
    return ll
end
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

fun <T: Number> insertAfter(prev: Node<T>, new: Node<T>) {
    new.next = prev.next
    prev.next = new
}

fun main(args: Array<String>) {
    val b = Node(3)
    val a = Node(1, b)
    println("Before insertion : $a")
    val c = Node(2)
    insertAfter(a, c)
    println("After  insertion : $a")
}
```


```txt

Before insertion : 1 -> 3
After  insertion : 1 -> 2 -> 3

```



## Logo


```logo
to insert :after :list :value
  localmake "tail member :after :list
  if not empty? :tail [.setbf :tail fput :value bf :tail]
  output :list
end

show insert 5 [3 5 1 8] 2
```

 [3 5 2 1 8]


## Mathematica


```Mathematica
Append[{a, b}, c]
->{a, b, c}
```


=={{header|Modula-3}}==

```modula3
MODULE SinglyLinkedList EXPORTS Main;

TYPE
  Link = REF LinkRcd;
  LinkRcd = RECORD
    Next: Link;
    Data: INTEGER
  END;

  PROCEDURE InsertAppend(anchor, next: Link) =
  BEGIN
    IF anchor # NIL AND next # NIL THEN
      next.Next := anchor.Next;
      anchor.Next := next
    END
  END InsertAppend;

VAR
  a: Link := NEW(Link, Next := NIL, Data := 1);
  b: Link := NEW(Link, Next := NIL, Data := 2);
  c: Link := NEW(Link, Next := NIL, Data := 3);

BEGIN
  InsertAppend(a, b);
  InsertAppend(a, c)
END SinglyLinkedList.
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
```



## OCaml

This kind of list manipulation is unidiomatic OCaml. But you can try the following:

```ocaml
let rec insert_after a b = function
   c :: cs when a = c -> a :: b :: cs
 | c :: cs            -> c :: insert_after a b cs
 | []                 -> raise Not_found
```




## Oforth


Method forEachNext is defined in order to traverse the LinkedList. This method is used by println (as a LinkedLIst is defined as a subclass of Collection).


```Oforth
Collection Class new: LinkedList(data, mutable next)

LinkedList method: initialize   := next := data ;
LinkedList method: data   @data ;
LinkedList method: next   @next ;
LinkedList method: add(e) e @next LinkedList new := next ;

LinkedList method: forEachNext
   dup ifNull: [ drop self ]
   dup 1 ifEq: [ drop false return ]
   dup next dup ifNull: [ drop 1 ]
   swap data true ;

: testLink  LinkedList new($A, null) dup add($B) dup add($C) ;

testLink println
```


```txt

[A, C, B]

```



## ooRexx

See [[Singly-linked_list/Element_definition#ooRexx|Single-linked list/Element definition]] for full class definition.

```ooRexx

list = .linkedlist~new
index = list~insert("abc")   -- insert a first item, keeping the index
list~insert("def")           -- adds to the end
list~insert("123", .nil)     -- adds to the begining
list~insert("456", index)    -- inserts between "abc" and "def"
list~remove(index)           -- removes "abc"

```



## Pascal

Note: This code uses only Standard Pascal features. For code using features only available in modern Pascal versions, see above under "[Delphi / Object Pascal / >>Turbo Pascal<<]"

Since Standard Pascal doesn't know a generic pointer type, and also no generic types, one has to settle for a specific data type for the linked list. Since the task mentions node names "A", "B", "C", here a char is chosen. Of course any data type (including pointers to a specific data type) could have been used here.


```pascal
type
  pCharNode = ^CharNode;
  CharNode = record
               data: char;
               next: pCharNode;
             end;

(* This procedure inserts a node (newnode) directly after another node which is assumed to already be in a list.
  It does not allocate a new node, but takes an already allocated node, thus allowing to use it (together with
  a procedure to remove a node from a list) for splicing a node from one list to another. *)
procedure InsertAfter(listnode, newnode: pCharNode);
begin
  newnode^.next := listnode^.next;
  listnode^.next := newnode;
end;
```

Usage example:

```pascal
var
  A, B: pCharNode;
begin
  (* build the two-component list A->C manually *)
  new(A);
  A^.data := 'A';
  new(A^.next);
  A^.next^.data := 'C';
  A^.next^.next := nil;

  (* create the node to be inserted. The initialization of B^.next isn't strictly necessary
    (it gets overwritten anyway), but it's good style not to leave any values undefined. *)
  new(B);
  node^.data := 'B';
  node^.next := nil;

  (* call the above procedure to insert node B after node A *)
  InsertAfter(A, B);

  (* delete the list *)
  while A <> nil do
   begin
    B := A;
    A := A^.next;
    dispose(B);
   end
end.
```



## Perl

If you don't really need the constant-time insertion property of singly linked lists, just use an array. You can traverse and splice it any way.

```perl
my @l  = ($A, $B);
push @l, $C, splice @l, 1;
```

However, if you really need a linked list, or all you got is an algorithm in a foreign language, you can use references to accomplish the translation.

```perl
sub insert_after {
  # first argument: node to insert after
  # second argument: node to insert
  $_[1]{next} = $_[0]{next};
  $_[0]{next} = $_[1];
}

my %B = (
    data => 3,
    next => undef, # not a circular list
);
my %A = (
    data => 1,
    next => \%B,
);
my %C = (
    data => 2,
);
insert_after \%A, \%C;
```

Note that you don't have to name your new nodes. The following works just as well:

```perl
 insert_after \%A, { data => 2 };
```

Note the curly braces instead of round parentheses.

It is straightforward to extend the function to take an arbitrary number of list nodes to insert:

```perl
sub insert_after {
  my $node = $_[0];
  my $next = $node->{next};
  shift;
  while (defined $_[0]) {
    $node->{next} = $_[0];
    $node = $node->{next};
    shift;
  }
  $node->{next} = $next;
}
```

With this, it's rather easy to build a list:

```perl
my %list = ( data => 'A' );
insert_after \%list, { data => 'B' }, { data => 'C' };
```

List handling is simplified if the variables themselves contain references. For example:

```perl
my $list2;

# create a new list ('A'. 'B', 'C') and store it in $list2
insert_after $list2 = { data => 'A' }, { data => 'B' }, { data => 'C' };

# append two new nodes ('D', 'E') after the first element
insert_after $list2, { data => 'A2' }, { data => 'A3' };

# append new nodes ('A2a', 'A2b') after the second element (which now is 'A2')
insert_after $list2->{next}, { data => 'A2a' }, { data => 'A2b' };
```


## Perl 6


Extending <tt>class Cell</tt> from [[Singly-linked_list/Element_definition#Perl_6]]:


```perl6
    method insert ($value) {
        $.next = Cell.new(:$value, :$.next)
    }
```



## Phix

See also [[Singly-linked_list/Traversal#Phix|Traversal]] and [[Singly-linked_list/Element_removal#Phix|Removal]].

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
```

```txt

```



## PicoLisp

Destructive operation

```PicoLisp
(de insertAfter (Item Lst New)
   (when (member Item Lst)
      (con @ (cons New (cdr @))) )
   Lst )
```

Non-destructive operation

```PicoLisp
(de insertAfter (Item Lst New)
   (if (index Item Lst)
      (conc (cut @ 'Lst) (cons New Lst))
      Lst ) )
```

Output in both cases:

```txt
: (insertAfter 'A '(A B) 'C)
-> (A C B)

: (insertAfter 'A '(X Y Z A B D E) 'C)
-> (X Y Z A C B D E)
```



## PL/I


```PL/I

/* Let H be a pointer to a node in a one-way-linked list. */
/* Insert an element, whose value is given by variable V, following that node. */

allocate node set (Q);
node.p = H; /* The new node now points at the list where we want to insert it. */
node.value = V;
H->p = Q;   /* Break the list at H, and point it at the new node. */

```



## Pop11


In Pop11 one normally uses built-in lists:


```pop11
define insert_into_list(anchor, x);
   cons(x, back(anchor)) -> back(anchor);
enddefine;
;;; Build inital list
lvars l1 = cons("a", []);
insert_into_list(l1, "b");
;;; insert c
insert_into_list(l1,  "c");
```


If one wants one can use user-defined list node (for convenience we repeat definition of list node):


```pop11
uses objectclass;
define :class ListNode;
    slot value = [];
    slot next = [];
enddefine;

define insert_into_List(anchor, x);
   consListNode(x, next(anchor)) -> next(anchor);
enddefine;
;;; Build inital list
lvars l2 = consListNode("a", []);
insert_into_List(l2, "b");
;;; insert c
insert_into_List(l2,  "c");
```


Note that user-defined case differs from built-in case only because of names.

## PureBasic


```PureBasic
Procedure insertAfter(Value, *node.MyData = #Null)
  Protected *newNode.MyData = AllocateMemory(SizeOf(MyData))
  If *newNode
    If *node
      *newNode\next = *node\next
      *node\next = *newNode
    EndIf
    *newNode\Value = Value
  EndIf
  ProcedureReturn *newNode ;return pointer to newnode
EndProcedure


Define *SL_List.MyData, a = 1, b = 2, c = 3

*SL_List = insertAfter(a) ;start the list
insertAfter(b, *SL_List) ;insert after head of list
insertAfter(c, *SL_List) ;insert after head of list and before tail
```



## Python


```python
def chain_insert(lst, at, item):
    while lst is not None:
        if lst[0] == at:
            lst[1] = [item, lst[1]]
            return
        else:
            lst = lst[1]
    raise ValueError(str(at) + " not found")

chain = ['A', ['B', None]]
chain_insert(chain, 'A', 'C')
print chain
```

Output:

```python
['A', ['C', ['B', None]]]
```



## Racket



```Racket

#lang racket

;; insert b after a in a mutable list (assumes that a is in the input list)
(define (insert-after! list a b)
  (if (equal? (mcar list) a)
    (set-mcdr! list (mcons b (mcdr list)))
    (insert-after! (mcdr list) a b)))

(define l (mcons 1 (mcons 2 (mcons 3 '()))))
(insert-after! l 2 2.5)
l ; -> (mcons 1 (mcons 2 (mcons 2.5 (mcons 3))))

```



## REXX


```rexx
/*REXX program demonstrates how to create and show a single-linked list.*/
@.=0                                   /*define a null linked list.     */
call set@ 3                            /*linked list:  12 Proth primes. */
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
call list@
     after = 97                        /* ◄──── REXX code to do insert. */
     newVal=100                        /* ◄────   "    "   "  "    "    */
     #=@..after                        /* ◄────   "    "   "  "    "    */
     call ins@ #,newVal                /* ◄────   "    "   "  "    "    */
say
say 'a new value of' newval "has been inserted after element value:" after
call list@
exit                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────INS@ subroutine─────────────────────*/
ins@:  procedure expose @.;  parse arg #,y
@._last=@._last+1                      /*bump number of list elements.  */
_=@._last
@._._value=y                           /*define new value list element. */
@._._next=@.#._next
@.#._next=_
@..y=_                                 /*set a locator pointer to self. */
@.max_width=max(@.max_width,length(y)) /*set maximum width of any value.*/
return                                 /*return to invoker of this sub. */
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

a new value of 100 has been inserted after element value: 97

 item   value   next
────── ─────── ──────
     1       3      2
     2       5      3
     3      13      4
     4      17      5
     5      41      6
     6      97     13
     7     100      7
     8     113      8
     9     193      9
    10     241     10
    11     257     11
    12     353     12
    13     449      0

```



## Ruby


```ruby
class ListNode
  def insert_after(search_value, new_value)
    if search_value == value
      self.succ = self.class.new(new_value, succ)
    elsif self.succ.nil?
      raise StandardError, "value #{search_value} not found in list"
    else
      self.succ.insert_after(search_value, new_value)
    end
  end
end

list = ListNode.new(:a, ListNode.new(:b))
list.insert_after(:a, :c)
```



## Rust


Extending [[Singly-Linked List (element)#Rust]]. Please see that page for the Linked List struct declarations.

```rust
impl<T> List<T>
 {
    pub fn new() -> Self {
        List { head: None }
    }

    pub fn push(&mut self, elem: T) {
    let new_node = Box::new(Node {
        elem: elem,
        next: self.head.take(),
    });
    self.head = Some(new_node);
}
```



## Scala

In Scala (and functional programming) we create a new list instead of modifying existing one.

```scala

/*
Here is a basic list definition

sealed trait List[+A]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
case object Nil extends List[Nothing]
*/

object List {
  def add[A](as: List[A], a: A): List[A] = Cons(a, as)
}

```



## Scheme

Non-mutating:

```scheme
(define (insert-after a b lst)
  (if (null? lst)
      lst       ; This should be an error, but we will just return the list untouched
      (let ((c (car lst))
            (cs (cdr lst)))
        (if (equal? a c)
            (cons a (cons b cs))
            (cons c (insert-after a b cs))))))
```


Mutating:

```scheme
(define (insert-after! a b lst)
  (let ((pos (member a lst)))
    (if pos
        (set-cdr! pos (cons b (cdr pos))))))
```



## Sidef


```ruby
func insert_after(a,b) {
    b{:next} = a{:next};
    a{:next} = b;
}

var B = Hash.new(
    data => 3,
    next => nil,    # not a circular list
);
var A = Hash.new(
    data => 1,
    next => B,
);
var C = Hash.new(
    data => 2,
);

insert_after(A, C);
```



## Stata


See [[Singly-linked list/Element definition#Stata]].


## Tcl


This task is extremely against the nature of the Tool Command Language. There are built-in lists, which are first-class citizens. The command <tt>linsert</tt> for inserting in such a list is already there, but it returns a new list instead of modifying an existing one. To emulate this, the <i>name</i> of the list (instead of its value) has to be handed over to the procedure and the procedure has to be given access to the variable using the <tt>upvar</tt> construction.

Additionally, the inserting point is usually given by the <i>index</i> of the element, which is to <i>follow</i> the new element, so the insertion always happens <i>before</i>. Since references and pointers don't exist in Tcl, using an existing element (which can only be a value) to determine the position of insertion, is not a good idea, because any value may appear several times in the list.

No error checking is included.


```tcl

proc insertIntoList {existingList predecessor newElement} {
  upvar $existingList exList
  set exList [linsert $exList [expr [lsearch -exact $exList $predecessor] + 1] $newElement]
}

set list {A B}
insertIntoList list A C
puts $list

```

```txt

A C B

```



## X86 Assembly


```x86asm

; x86_64 Linux NASM
; Linked_List_Insert.asm

%ifndef INSERT
%define INSERT

%include "Linked_List_Definition.asm" ; see LL def task
%include "Heap_Alloc.asm" ; see memory allocation task

section .text

; rdi - link to insert after
; rsi - value that the new link will hold
Insert_After:
  push rdi
  push rsi
  mov rdi, linkSize
  call alloc
  cmp rax, 0
  je Memory_Allocation_Failure_Exception
  pop rdi
  mov dword [rax + value], edi
  pop rdi
  mov rsi, qword [rdi + next]
  mov qword [rax + next], rsi
  mov qword [rdi + next], rax
  ret

%endif

```



## zkl

In place:

```zkl
L("a","b","c").insert(1,"foo") //-->L("a","foo","b","c")
a:=L("a","b","c"); a.insert(a.find("b"),"foo") //-->L("a","foo","b","c")
```

Create a new list:

```zkl
a:=ROList("a","b","c");
n:=a.index("b"); a[0,n].append("foo").extend(a[n,*]) //-->ROList("a","foo","b","c")
```
