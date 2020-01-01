+++
title = "Doubly-linked list/Traversal"
description = ""
date = 2019-08-30T20:02:48Z
aliases = []
[extra]
id = 4888
[taxonomies]
categories = []
tags = []
+++

{{task|Data Structures}}
[[Category:Iteration]]

Traverse from the beginning of a [[Doubly-linked list/Definition|doubly-linked list]] to the end, and from the end to the beginning.


{{Template:See also lists}}





## Ada

{{works with|Ada 2005}}

```Ada
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

procedure Traversing is
   package Char_Lists is new Ada.Containers.Doubly_Linked_Lists (Character);

   procedure Print (Position : in Char_Lists.Cursor) is
   begin
      Ada.Text_IO.Put (Char_Lists.Element (Position));
   end Print;

   My_List : Char_Lists.List;
begin
   My_List.Append ('R');
   My_List.Append ('o');
   My_List.Append ('s');
   My_List.Append ('e');
   My_List.Append ('t');
   My_List.Append ('t');
   My_List.Append ('a');

   My_List.Iterate (Print'Access);
   Ada.Text_IO.New_Line;

   My_List.Reverse_Iterate (Print'Access);
   Ada.Text_IO.New_Line;
end Traversing;
```



## ALGOL 68

LinkedList.alg:
```algol68
# Node struct - contains next and prev NODE pointers and DATA #
MODE NODE = STRUCT(
    DATA data,
    REF NODE prev,
    REF NODE next
    );

# List structure - contains head and tail NODE pointers #
MODE LIST = STRUCT(
    REF NODE head,
    REF NODE tail
    );

# --- PREPEND - Adds a node to the beginning of the list ---#
PRIO PREPEND = 1;
OP   PREPEND = (REF LIST list, DATA data) VOID:
(
    HEAP NODE n := (data, NIL, NIL);
    IF head OF list IS REF NODE(NIL) THEN
        head OF list := tail OF list := n
    ELSE
        next OF n := head OF list;
        prev OF head OF list := head OF list := n
    FI
);
#--- APPEND - Adds a node to the end of the list ---#
PRIO APPEND = 1;
OP   APPEND = (REF LIST list, DATA data) VOID:
(
    HEAP NODE n := (data, NIL, NIL);
    IF head OF list IS REF NODE(NIL) THEN
        head OF list := tail OF list := n
    ELSE
        prev OF n := tail OF list;
        next OF tail OF list := tail OF list := n
    FI
);

#--- REMOVE_FIRST - removes & returns node at end of the list ---#
PRIO REMOVE_FIRST = 1;
OP   REMOVE_FIRST = (REF LIST list) DATA:
(
    IF head OF list ISNT REF NODE(NIL) THEN
        DATA d := data OF head OF list;
        prev OF next OF head OF list := NIL;
        head OF list := next OF head OF list;
        d # return d #
    FI
);
#--- REMOVE_LAST: removes & returns node at front of list ---   #
PRIO REMOVE_LAST = 1;
OP   REMOVE_LAST = (REF LIST list) DATA:
(
    IF head OF list ISNT REF NODE(NIL) THEN
        DATA d := data OF tail OF list;
        next OF prev OF tail OF list := NIL;
        tail OF list := prev OF tail OF list;
        d # return d #
    FI
);
#--- PURGE - removes all elements from the list ---#
PRIO PURGE = 2;
OP   PURGE = (REF LIST list) VOID:
(
    head OF list := tail OF list := NIL
);

#--- returns the data at the end of the list ---#
PRIO LAST_IN = 2;
OP LAST_IN = (REF LIST list) DATA: (
    IF head OF list ISNT REF NODE(NIL) THEN
        data OF tail OF list
    FI
);

#--- returns the data at the front of the list ---#
PRIO FIRST_IN  = 2;
OP FIRST_IN = (REF LIST list) DATA: (
    IF head OF list ISNT REF NODE(NIL) THEN
        data OF head OF list
    FI
);

#--- Traverses through the list forwards ---#
PROC forward traversal = (LIST list) VOID:
(
    REF NODE travel := head OF list;
    WHILE travel ISNT REF NODE(NIL) DO
        list visit(data OF travel);
        travel := next OF travel
    OD
);

#--- Traverses through the list backwards ---#
PROC backward traversal = (LIST list) VOID:
(
    REF NODE travel := tail OF list;
    WHILE travel ISNT REF NODE(NIL) DO
        list visit(data OF travel);
        travel := prev OF travel
    OD
)
```


main.alg:
```algol68
PR READ "LinkedList.alg" PR;

MODE EMPLOYEE = STRUCT(STRING name, INT salary, INT years);
MODE DATA = EMPLOYEE; #Sets the data type that is in the list#

# Function that traversals call for each node in list  #
PROC list visit = (REF DATA data) VOID:
(
    print((
        "EMPLOYEE NAME  :  ", name OF data ,  newline,
        "         SALARY: " , salary OF data, newline,
        "         YEARS : " , years OF data,  newline
    ))
);

#***************************************************************#
main:
(
    EMPLOYEE empl;
    name OF empl := "one";
    salary OF empl := 100;
    years OF empl := 10;

    LIST list := (NIL, NIL);

    list PREPEND empl;
    name OF empl := "two";
    salary OF empl := 200;
    years OF empl := 20;
    list APPEND empl;
    name OF empl := "three";
    salary OF empl := 300;
    years OF empl := 30;
    list APPEND empl;
    salary OF empl := 400;
    years OF empl := 40;
    name OF empl := "four";
    list APPEND empl;

    forward traversal(list);
    PURGE list;
    forward traversal(list)
)
```

{{out}}

```txt

EMPLOYEE NAME  :  one
         SALARY:        +100
         YEARS :         +10
EMPLOYEE NAME  :  two
         SALARY:        +200
         YEARS :         +20
EMPLOYEE NAME  :  three
         SALARY:        +300
         YEARS :         +30
EMPLOYEE NAME  :  four
         SALARY:        +400
         YEARS :         +40

```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program transDblList.s   */
/* REMARK 1 : this program use routines in a include file
   see task Include a file language arm assembly
   for the routine affichageMess conversion10S
   see at end of this program the instruction include */

/* Constantes    */
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ READ,   3
.equ WRITE,  4

/*******************************************/
/* Structures                               */
/********************************************/
/* structure Doublylinkedlist*/
    .struct  0
dllist_head:                    @ head node
    .struct  dllist_head + 4
dllist_tail:                    @ tail node
    .struct  dllist_tail  + 4
dllist_fin:
/* structure Node Doublylinked List*/
    .struct  0
NDlist_next:                    @ next element
    .struct  NDlist_next + 4
NDlist_prev:                    @ previous element
    .struct  NDlist_prev + 4
NDlist_value:                   @ element value or key
    .struct  NDlist_value + 4
NDlist_fin:
/* Initialized data */
.data
szMessInitListe:         .asciz "List initialized.\n"
szMessListInv:           .asciz "Display list inverse :\n"
szCarriageReturn:        .asciz "\n"
szMessErreur:            .asciz "Error detected.\n"
/* datas message display */
szMessResult:            .ascii "Result value :"
sValue:                  .space 12,' '
                         .asciz "\n"
/* UnInitialized data */
.bss
dllist1:              .skip dllist_fin    @ list memory place

/*  code section */
.text
.global main
main:
    ldr r0,iAdrdllist1
    bl newDList                      @ create new list
    ldr r0,iAdrszMessInitListe
    bl affichageMess
    ldr r0,iAdrdllist1               @ list address
    mov r1,#10                       @ value
    bl insertHead                    @ insertion at head
    cmp r0,#-1
    beq 99f
    ldr r0,iAdrdllist1
    mov r1,#20
    bl insertTail                    @ insertion at tail
    cmp r0,#-1
    beq 99f
    ldr r0,iAdrdllist1               @ list address
    mov r1,#10                       @ value to after insered
    mov r2,#15                       @ new value
    bl insertAfter
    cmp r0,#-1
    beq 99f
    ldr r0,iAdrdllist1               @ list address
    mov r1,#20                       @ value to after insered
    mov r2,#25                       @ new value
    bl insertAfter
    cmp r0,#-1
    beq 99f
    ldr r0,iAdrdllist1
    bl transHeadTail                 @ display value head to tail
    ldr r0,iAdrszMessListInv
    bl affichageMess
    ldr r0,iAdrdllist1
    bl transTailHead                 @ display value tail to head
    b 100f
99:
    ldr r0,iAdrszMessErreur
    bl affichageMess
100:                                 @ standard end of the program
    mov r7, #EXIT                    @ request to exit program
    svc 0                            @ perform system call
iAdrszMessInitListe:       .int szMessInitListe
iAdrszMessErreur:          .int szMessErreur
iAdrszMessListInv:         .int szMessListInv
iAdrszCarriageReturn:      .int szCarriageReturn
iAdrdllist1:               .int dllist1
/******************************************************************/
/*     create new list                         */
/******************************************************************/
/* r0 contains the address of the list structure */
newDList:
    push {r1,lr}                         @ save  registers
    mov r1,#0
    str r1,[r0,#dllist_tail]
    str r1,[r0,#dllist_head]
    pop {r1,lr}                          @ restaur registers
    bx lr                                @ return
/******************************************************************/
/*     list is empty ?                         */
/******************************************************************/
/* r0 contains the address of the list structure */
/* r0 return 0 if empty  else return 1 */
isEmpty:
    //push {r1,lr}                         @ save  registers
    ldr r0,[r0,#dllist_head]
    cmp r0,#0
    movne r0,#1
    //pop {r1,lr}                          @ restaur registers
    bx lr                                @ return
/******************************************************************/
/*     insert value at list head                        */
/******************************************************************/
/* r0 contains the address of the list structure */
/* r1 contains value */
insertHead:
    push {r1-r4,lr}                         @ save  registers
    mov r4,r0                            @ save address
    mov r0,r1                            @ value
    bl createNode
    cmp r0,#-1                           @ allocation error ?
    beq 100f
    ldr r2,[r4,#dllist_head]             @ load address first node
    str r2,[r0,#NDlist_next]             @ store in next pointer on new node
    mov r1,#0
    str r1,[r0,#NDlist_prev]             @ store zero in previous pointer on new node
    str r0,[r4,#dllist_head]             @ store address new node in address head list
    cmp r2,#0                            @ address first node is null ?
    strne r0,[r2,#NDlist_prev]           @ no store adresse new node in previous pointer
    streq r0,[r4,#dllist_tail]           @ else store new node in tail address
100:
    pop {r1-r4,lr}                          @ restaur registers
    bx lr                                @ return
/******************************************************************/
/*     insert value at list tail                        */
/******************************************************************/
/* r0 contains the address of the list structure */
/* r1 contains value */
insertTail:
    push {r1-r4,lr}                         @ save  registers
    mov r4,r0                               @ save list address
    mov r0,r1                               @ value
    bl createNode                           @ new node
    cmp r0,#-1
    beq 100f                                @ allocation error
    ldr r2,[r4,#dllist_tail]                @ load address last node
    str r2,[r0,#NDlist_prev]                @ store in previous pointer on new node
    mov r1,#0                               @ store null un next pointer
    str r1,[r0,#NDlist_next]
    str r0,[r4,#dllist_tail]                @ store address new node on list tail
    cmp r2,#0                               @ address last node is null ?
    strne r0,[r2,#NDlist_next]              @ no store address new node in next pointer
    streq r0,[r4,#dllist_head]              @ else store in head list
100:
    pop {r1-r4,lr}                          @ restaur registers
    bx lr                                   @ return
/******************************************************************/
/*     insert value after other element                        */
/******************************************************************/
/* r0 contains the address of the list structure */
/* r1 contains value to search*/
/* r2 contains value to insert */
insertAfter:
    push {r1-r5,lr}                         @ save  registers
    mov r4,r0
    bl searchValue                          @ search node with this value in r1
    cmp r0,#-1
    beq 100f                                @ not found -> error
    mov r5,r0                               @ save address of node find
    mov r0,r2                               @ new value
    bl createNode                           @ create new node
    cmp r0,#-1
    beq 100f                                @ allocation error
    ldr r2,[r5,#NDlist_next]                @ load pointer next of find node
    str r0,[r5,#NDlist_next]                @ store new node in pointer next
    str r5,[r0,#NDlist_prev]                @ store address find node in previous pointer on new node
    str r2,[r0,#NDlist_next]                @ store pointer next of find node on pointer next on new node
    cmp r2,#0                               @ next pointer is null ?
    strne r0,[r2,#NDlist_prev]              @ no store address new node in previous pointer
    streq r0,[r4,#dllist_tail]              @ else store in list tail
100:
    pop {r1-r5,lr}                          @ restaur registers
    bx lr                                @ return
/******************************************************************/
/*     search value                                               */
/******************************************************************/
/* r0 contains the address of the list structure */
/* r1 contains the value to search  */
/* r0 return address of node or -1 if not found */
searchValue:
    push {r2,lr}                         @ save  registers
    ldr r0,[r0,#dllist_head]             @ load first node
1:
    cmp r0,#0                            @ null -> end search not found
    moveq r0,#-1
    beq 100f
    ldr r2,[r0,#NDlist_value]            @ load node value
    cmp r2,r1                            @ equal ?
    beq 100f
    ldr r0,[r0,#NDlist_next]             @ load addresse next node
    b 1b                                 @ and loop
100:
    pop {r2,lr}                          @ restaur registers
    bx lr                                @ return
/******************************************************************/
/*     transversal for head to tail                                               */
/******************************************************************/
/* r0 contains the address of the list structure */
transHeadTail:
    push {r2,lr}                         @ save  registers
    ldr r2,[r0,#dllist_head]             @ load first node
1:
    ldr r0,[r2,#NDlist_value]
    ldr r1,iAdrsValue
    bl conversion10S
    ldr r0,iAdrszMessResult
    bl affichageMess
    ldr r2,[r2,#NDlist_next]
    cmp r2,#0
    bne 1b
100:
    pop {r2,lr}                          @ restaur registers
    bx lr                                @ return
iAdrszMessResult:          .int szMessResult
iAdrsValue:                .int sValue
/******************************************************************/
/*     transversal for tail to head                                               */
/******************************************************************/
/* r0 contains the address of the list structure */
transTailHead:
    push {r2,lr}                         @ save  registers
    ldr r2,[r0,#dllist_tail]             @ load last node
1:
    ldr r0,[r2,#NDlist_value]
    ldr r1,iAdrsValue
    bl conversion10S
    ldr r0,iAdrszMessResult
    bl affichageMess
    ldr r2,[r2,#NDlist_prev]
    cmp r2,#0
    bne 1b
100:
    pop {r2,lr}                          @ restaur registers
    bx lr                                @ return
/******************************************************************/
/*     Create new node                                            */
/******************************************************************/
/* r0 contains the value */
/* r0 return node address or -1 if allocation error*/
createNode:
    push {r1-r7,lr}                         @ save  registers
    mov r4,r0                            @ save value
    @ allocation place on the heap
    mov r0,#0                                   @ allocation place heap
    mov r7,#0x2D                                @ call system 'brk'
    svc #0
    mov r5,r0                                   @ save address heap for output string
    add r0,#NDlist_fin                            @ reservation place one element
    mov r7,#0x2D                                @ call system 'brk'
    svc #0
    cmp r0,#-1                                  @ allocation error
    beq 100f
    mov r0,r5
    str r4,[r0,#NDlist_value]                   @ store value
    mov r2,#0
    str r2,[r0,#NDlist_next]                    @ store zero to pointer next
    str r2,[r0,#NDlist_prev]                    @ store zero to pointer previous
100:
    pop {r1-r7,lr}                          @ restaur registers
    bx lr                                @ return
/***************************************************/
/*      ROUTINES INCLUDE                 */
/***************************************************/
.include "../affichage.inc"


```


## AutoHotkey

see [[Doubly-linked list/AutoHotkey]]


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

Disp "-----",i

B→I
While I≠0
 Disp VALUE(I)▶Dec,i
 PREV(I)→I
End
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      DIM node{pPrev%, pNext%, iData%}
      DIM a{} = node{}, b{} = node{}, c{} = node{}

      a.pNext% = b{}
      a.iData% = 123
      b.pPrev% = a{}
      b.iData% = 789
      c.iData% = 456

      PROCinsert(a{}, c{})

      PRINT "Traverse forwards:"
      pnode% = a{}
      REPEAT
        !(^node{}+4) = pnode%
        PRINT node.iData%
        pnode% = node.pNext%
      UNTIL pnode% = 0

      PRINT "Traverse backwards:"
      pnode% = b{}
      REPEAT
        !(^node{}+4) = pnode%
        PRINT node.iData%
        pnode% = node.pPrev%
      UNTIL pnode% = 0

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

Output:

```txt
Traverse forwards:
       123
       456
       789
Traverse backwards:
       789
       456
       123
```



## C


```c
// A doubly linked list of strings;
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct sListEntry {
    const char *value;
    struct sListEntry *next;
    struct sListEntry *prev;
} *ListEntry, *LinkedList;

typedef struct sListIterator{
    ListEntry  link;
    LinkedList head;
} *LIterator;

LinkedList NewList() {
    ListEntry le = malloc(sizeof(struct sListEntry));
    if (le) {
        le->value = NULL;
        le->next = le->prev = NULL;
    }
    return le;
}

int LL_Append(LinkedList ll, const char *newVal)
{
    ListEntry le = malloc(sizeof(struct sListEntry));
    if (le) {
        le->value = strdup(newVal);
        le->prev = ll->prev;
        le->next = NULL;
        if (le->prev)
            le->prev->next = le;
        else
            ll->next = le;
        ll->prev = le;
    }
    return (le!= NULL);
}

int LI_Insert(LIterator iter, const char *newVal)
{
    ListEntry crnt = iter->link;
    ListEntry le = malloc(sizeof(struct sListEntry));
    if (le) {
        le->value = strdup(newVal);
        if ( crnt == iter->head) {
            le->prev = NULL;
            le->next = crnt->next;
            crnt->next = le;
            if (le->next)
                le->next->prev = le;
            else
                crnt->prev = le;
        }
        else {
            le->prev = ( crnt == NULL)? iter->head->prev : crnt->prev;
            le->next = crnt;
            if (le->prev)
                le->prev->next = le;
            else
                iter->head->next = le;
            if (crnt)
                crnt->prev = le;
            else
                iter->head->prev = le;
        }
    }
    return (le!= NULL);
}

LIterator LL_GetIterator(LinkedList ll )
{
    LIterator liter = malloc(sizeof(struct sListIterator));
    liter->head = ll;
    liter->link = ll;
    return liter;
}

#define LLI_Delete( iter ) \
    {free(iter); \
    iter = NULL;}

int LLI_AtEnd(LIterator iter)
{
    return iter->link == NULL;
}
const char *LLI_Value(LIterator iter)
{
    return (iter->link)? iter->link->value: NULL;
}
int LLI_Next(LIterator iter)
{
    if (iter->link) iter->link = iter->link->next;
    return(iter->link != NULL);
}
int LLI_Prev(LIterator iter)
{
    if (iter->link) iter->link = iter->link->prev;
    return(iter->link != NULL);
}

int main()
{
    static const char *contents[] = {"Read", "Orage", "Yeller",
                                     "Glean", "Blew", "Burple"};
    int ix;
    LinkedList ll = NewList();    //new linked list
    LIterator iter;

    for (ix=0; ix<6; ix++)        //insert contents
        LL_Append(ll, contents[ix]);

    iter = LL_GetIterator(ll);    //get an iterator
    printf("forward\n");
    while(LLI_Next(iter))         //iterate forward
        printf("value=%s\n", LLI_Value(iter));
    LLI_Delete(iter);             //delete iterator

    printf("\nreverse\n");
    iter = LL_GetIterator(ll);
    while(LLI_Prev(iter))         //iterate reverse
        printf("value=%s\n", LLI_Value(iter));
    LLI_Delete(iter);
                        //uhhh-- delete list??
    return 0;
}
```



## C++

{{works with|C++11}}

```cpp
#include <iostream>
#include <list>

int main ()
{
    std::list<int> numbers {1, 5, 7, 0, 3, 2};
    for(const auto& i: numbers)
        std::cout << i << ' ';
    std::cout << '\n';
}
```

{{out}}

```txt

1 5 7 0 3 2

```



## C sharp


```csharp
using System;
using System.Collections.Generic;

namespace RosettaCode.DoublyLinkedList
{
    internal static class Program
    {
        private static void Main()
        {
            var list = new LinkedList<char>("hello");

            var current = list.First;
            do
            {
                Console.WriteLine(current.Value);
            } while ((current = current.Next) != null);

            Console.WriteLine();

            current = list.Last;
            do
            {
                Console.WriteLine(current.Value);
            } while ((current = current.Previous) != null);
        }
    }
}
```

Output:

```txt

h
e
l
l
o

o
l
l
e
h
```



## Clojure

Given the definition in [[../Definition#Clojure]],

```Clojure
(def dl (double-list [:a :b :c :d]))
;=> #'user/dl

((juxt seq rseq) dl)
;=> [(:a :b :c :d) (:d :c :b :a)]

(take-while identity (iterate get-next (get-head dl)))
;=> (#:double_list.Node{:prev nil,          :next #<Object...>, :data :a, :key #<Object...>}
;=>  #:double_list.Node{:prev #<Object...>, :next #<Object...>, :data :b, :key #<Object...>}
;=>  #:double_list.Node{:prev #<Object...>, :next #<Object...>, :data :c, :key #<Object...>}
;=>  #:double_list.Node{:prev #<Object...>, :next nil,          :data :d, :key #<Object...>})

(take-while identity (iterate get-prev (get-tail dl)))

;=> (#:double_list.Node{:prev #<Object...>, :next nil,          :data :d, :key #<Object...>}
;=>  #:double_list.Node{:prev #<Object...>, :next #<Object...>, :data :c, :key #<Object...>}
;=>  #:double_list.Node{:prev #<Object...>, :next #<Object...>, :data :b, :key #<Object...>}
;=>  #:double_list.Node{:prev nil,          :next #<Object...>, :data :a, :key #<Object...>})
```



## D

===Standard Doubly-linked List===

```d
void main() {
    import std.stdio, std.container, std.range;

    auto dll = DList!dchar("DCBA"d.dup);

    dll[].writeln;
    dll[].retro.writeln;
}
```

{{out}}

```txt
ABCD
DCBA
```


===User-defined Doubly-linked list===
Same output.

```d
struct Node(T) {
    T data;
    typeof(this)* prev, next;
}

void prepend(T)(ref Node!T* head, in T item) pure nothrow {
    auto newNode = new Node!T(item, null, head);
    if (head)
        head.prev = newNode;
    head = newNode;
}

void main() {
    import std.stdio;

    Node!char* head;
    foreach (char c; "DCBA")
        head.prepend(c);

    auto last = head;
    for (auto p = head; p; p = p.next) {
        p.data.write;
        last = p;
    }
    writeln;

    for (auto p = last; p; p = p.prev)
        p.data.write;
    writeln;
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
      prev : pList ;
      next : pList ;
   end;

// since this task is just showing the traversal I am not allocating the memory and setting up the root node etc.
// Note the use of the carat symbol for de-referencing the pointer.

begin

   // beginning to end
   while not (pList^.Next = NIL) do pList := pList^.Next ;

   // end to beginning
   while not (pList^.prev = NIL) do pList := pList^.prev ;

end;
```



## E

{{incorrect|E|Doesn't work, probably due to a bug in the list definition: runs over the beginning of the list. Needs debugging.}}
Given the definition in [[../Definition#E]],

```e
def traverse(list) {
    var node := list.atFirst()
    while (true) {
        println(node[])
        if (node.hasNext()) {
            node := node.next()
        } else {
            break
        }
    }
    while (true) {
        println(node[])
        if (node.hasPrev()) {
            node := node.prev()
        } else {
            break
        }
    }
}
```



```e
? def list := makeDLList()
# value: <>

? list.push(1)
? list.push(2)
? list.push(3)

? traverse(list)
1
2
3
3
2
1
```



## Erlang

The task in [[Doubly-linked_list/Element_insertion]] uses traversal as proof that the insertion worked.


## Fortran

see [[Doubly-linked list/Definition#Fortran]]


## F#


```fsharp

open System.Collections.Generic

let first (l: LinkedList<char>) = l.First
let last (l: LinkedList<char>) = l.Last

let next (l: LinkedListNode<char>) = l.Next
let prev (l: LinkedListNode<char>) = l.Previous

let traverse g f (ls: LinkedList<char>) =
    let rec traverse (l: LinkedListNode<char>) =
        match l with
        | null -> ()
        | _ ->
            printf "%A" l.Value
            traverse (f l)
    traverse (g ls)

let traverseForward = traverse first next
let traverseBackward = traverse last prev

let cs = LinkedList(['a'..'z'])

traverseForward cs
printfn ""
traverseBackward cs

```

{{out}}

```txt

'a''b''c''d''e''f''g''h''i''j''k''l''m''n''o''p''q''r''s''t''u''v''w''x''y''z'
'z''y''x''w''v''u''t''s''r''q''p''o''n''m''l''k''j''i''h''g''f''e''d''c''b''a'

```



## Go

Code is identical to that for task Doubly-linked list/Element insertion except for addition of section at the end of main noted "traverse from end to beginning."  Traversal from beginning to end is adequately demonstrated by the String method of dlList.

Also note that there is a doubly linked list in the Go standard library in package container/list.

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

    // traverse from end to beginning
    fmt.Print("From tail:")
    for p := dll.tail; p != nil; p = p.prev {
        fmt.Print(" ", p.string)
    }
    fmt.Println("")
}
```

Output:

```txt

<nil>
[A B]
[A C B]
From tail: B C A

```



## Haskell


```haskell
main = print . traverse True $ create [10,20,30,40]

data DList a = Leaf | Node { prev::(DList a), elt::a, next::(DList a) }

create = go Leaf
    where go _    []     = Leaf
          go prev (x:xs) = current
              where current = Node prev x next
                    next    = go current xs

isLeaf Leaf    = True
isLeaf _       = False

lastNode Leaf  = Leaf
lastNode dl    = until (isLeaf.next) next dl

traverse _    Leaf            = []
traverse True (Node l v Leaf) = v : v : traverse False l
traverse dir  (Node l v r)    = v : traverse dir (if dir then r else l)
```


==Icon and {{header|Unicon}}==
Uses Unicon classes.

```Unicon
class DoubleLink (value, prev_link, next_link)

  # insert given node after this one, removing its existing connections
  method insert_after (node)
    node.prev_link := self
    if (\next_link) then next_link.prev_link := node
    node.next_link := next_link
    self.next_link := node
  end

  # use a generator to traverse
  # - keep suspending the prev/next link until a null node is reached
  method traverse_backwards ()
    current := self
    while \current do {
      suspend current
      current := current.prev_link
    }
  end

  method traverse_forwards ()
    current := self
    while \current do {
      suspend current
      current := current.next_link
    }
  end

  initially (value, prev_link, next_link)
    self.value := value
    self.prev_link := prev_link    # links are 'null' if not given
    self.next_link := next_link
end

procedure main ()
  l1 := DoubleLink (1)
  l2 := DoubleLink (2)
  l1.insert_after (l2)
  l1.insert_after (DoubleLink (3))

  write ("Traverse from beginning to end")
  every (node := l1.traverse_forwards ()) do
    write (node.value)

  write ("Traverse from end to beginning")
  every (node := l2.traverse_backwards ()) do
    write (node.value)
end
```


Output:

```txt

Traverse from beginning to end
1
3
2
Traverse from end to beginning
2
3
1

```



## J


```j
traverse=:1 :0
  work=. result=. conew 'DoublyLinkedListHead'
  current=. y
  while. y ~: current=. successor__current do.
    work=. (work;result;<u data__current) conew 'DoublyLinkedListElement'
  end.
  result
)
```


This traverses a doubly linked list, applying the verb u to the data in each list element and creates a new doubly linked list containing the results.  A reference to the new doubly linked list is returned.


## Java

{{works with|Java|8 or higher}}


```java

package com.rosettacode;

import java.util.LinkedList;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class DoubleLinkedListTraversing {

  public static void main(String[] args) {

    final LinkedList<String> doubleLinkedList =
        IntStream.range(1, 10)
            .mapToObj(String::valueOf)
            .collect(Collectors.toCollection(LinkedList::new));

    doubleLinkedList.iterator().forEachRemaining(System.out::print);
    System.out.println();
    doubleLinkedList.descendingIterator().forEachRemaining(System.out::print);
  }
}
```


{{out}}

```txt
123456789
987654321
```



## JavaScript

See [[Doubly-Linked List (element)#JavaScript]]. The <code>traverse()</code> and <code>print()</code> functions have been inherited from [[Singly-Linked List (traversal)#JavaScript]].

```javascript
DoublyLinkedList.prototype.getTail = function() {
    var tail;
    this.traverse(function(node){tail = node;});
    return tail;
}
DoublyLinkedList.prototype.traverseBackward = function(func) {
    func(this);
    if (this.prev() != null)
        this.prev().traverseBackward(func);
}
DoublyLinkedList.prototype.printBackward = function() {
    this.traverseBackward( function(node) {print(node.value())} );
}

var head = createDoublyLinkedListFromArray([10,20,30,40]);
head.print();
head.getTail().printBackward();
```


outputs:

```txt
10
20
30
40
40
30
20
10
```

Uses the <code>print()</code> function from [[Rhino]] or [[SpiderMonkey]].


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

first(nd) = (while nd.pred != nothing nd = nd.prev end; nd)
last(nd) = (while nd.succ != nothing nd = nd.succ end; nd)

function printconnected(nd; fromtail = false)
    if fromtail
        nd = last(nd)
        print(nd.value)
        while nd.pred != nothing
            nd = nd.pred
            print(" -> $(nd.value)")
        end
    else
        nd = first(nd)
        print(nd.value)
        while nd.succ != nothing
            nd = nd.succ
            print(" -> $(nd.value)")
        end
    end
    println()
end

node1 = DLNode(1)
node2 = DLNode(2)
node3 = DLNode(3)
insertpost(node1, node2)
insertpost(node2, node3)
print("From beginning to end: "); printconnected(node1)
print("From end to beginning: "); printconnected(node1, fromtail = true)

```
 {{output}}
```txt

From beginning to end: 1 -> 2 -> 3
From end to beginning: 3 -> 2 -> 1

```



## Kotlin

To complete this task, we just need to add a couple of traversal functions to the class we defined in the [[Doubly-linked list/Definition]] task:

```scala
// version 1.1.2

class LinkedList<E> {
    class Node<E>(var data: E, var prev: Node<E>? = null, var next: Node<E>? = null) {
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

    var first: Node<E>? = null
    var last:  Node<E>? = null

    fun addFirst(value: E) {
        if (first == null) {
            first = Node(value)
            last =  first
        }
        else {
            val node = first!!
            first = Node(value, null, node)
            node.prev = first
        }
    }

    fun addLast(value: E) {
        if (last == null) {
            last = Node(value)
            first = last
        }
        else {
            val node = last!!
            last = Node(value, node, null)
            node.next = last
        }
    }

    fun insert(after: Node<E>?, value: E) {
        if (after == null)
            addFirst(value)
        else if (after == last)
            addLast(value)
        else {
            val next = after.next
            val new = Node(value, after, next)
            after.next = new
            if (next != null) next.prev = new
        }
    }

    override fun toString() = first.toString()

    fun firstToLast() = first?.toString() ?: ""

    fun lastToFirst(): String {
        if (last == null) return ""
        val sb = StringBuilder(last.toString())
        var node = last!!.prev
        while (node != null) {
             sb.append(" -> ", node.data.toString())
             node = node.prev
        }
        return sb.toString()
    }
}

fun main(args: Array<String>) {
    val ll = LinkedList<Int>()
    ll.addFirst(1)
    ll.addLast(4)
    ll.insert(ll.first, 2)
    ll.insert(ll.last!!.prev, 3)
    println("First to last : ${ll.firstToLast()}")
    println("Last to first : ${ll.lastToFirst()}")
}
```


{{out}}

```txt

First to last : 1 -> 2 -> 3 -> 4
Last to first : 4 -> 3 -> 2 -> 1

```



## Liberty BASIC


```lb

struct block,nxt as ulong,prev as ulong,nm as char[20],age as long'Our structure of the blocks in our list.

global hHeap
global hFirst
global hLast
global blockCount
global blockSize
blockSize=len(block.struct)


    call Init
    if hHeap=0 then
        print "Error occured! Could not create heap, exiting..."
        end
    end if

    FirstUser=New("David",20)
    notImportant=New("Jessica",35)
    notImportant=New("Joey",38)
    MiddleUser=New("Jack",56)
    notImportant=New("Amy",17)
    notImportant=New("Bob",28)
    LastUser=New("Kenny",56)


    print "-Traversing the list forwards"

    hCurrent=hFirst
    while hCurrent<>0
        print tab(2);dechex$(hCurrent);"   ";Block.name$(hCurrent);"   ";Block.age(hCurrent)
        hCurrent=Block.next(hCurrent)
    wend

    print
    print "-Deleting first, middle, and last person."

    call Delete FirstUser'1
    call Delete MiddleUser'2
    call Delete LastUser'3

    print
    print "-Traversing the list backwards"
    hCurrent=hLast
    while hCurrent<>0
        print tab(2);dechex$(hCurrent);"   ";Block.name$(hCurrent);"   ";Block.age(hCurrent)
        hCurrent=Block.prev(hCurrent)
    wend

call Uninit

end


function Block.next(hBlock)
    calldll #kernel32,"RtlMoveMemory",block as struct,hBlock as ulong,blockSize as long,ret as void
    Block.next=block.nxt.struct
end function

function Block.prev(hBlock)
    calldll #kernel32,"RtlMoveMemory",block as struct,hBlock as ulong,blockSize as long,ret as void
    Block.prev=block.prev.struct
end function

function Block.age(hBlock)
    calldll #kernel32,"RtlMoveMemory",block as struct,hBlock as ulong,blockSize as long,ret as void
    Block.age=block.age.struct
end function

function Block.name$(hBlock)
    calldll #kernel32,"RtlMoveMemory",block as struct,hBlock as ulong,blockSize as long,ret as void
    Block.name$=block.nm.struct
end function

sub Block.age hBlock,age
    calldll #kernel32,"RtlMoveMemory",block as struct,hBlock as ulong,blockSize as long,ret as void
    block.age.struct=age
    calldll #kernel32,"RtlMoveMemory",hBlock as ulong,block as struct,blockSize as long,ret as void
end sub

sub Block.name hBlock,name$
    calldll #kernel32,"RtlMoveMemory",block as struct,hBlock as ulong,blockSize as long,ret as void
    block.nm.struct=name$
    calldll #kernel32,"RtlMoveMemory",hBlock as ulong,block as struct,blockSize as long,ret as void
end sub

sub Block.next hBlock,nxt
    calldll #kernel32,"RtlMoveMemory",block as struct,hBlock as ulong,blockSize as long,ret as void
    block.nxt.struct=nxt
    calldll #kernel32,"RtlMoveMemory",hBlock as ulong,block as struct,blockSize as long,ret as void
end sub

sub Block.prev hBlock,prev
    calldll #kernel32,"RtlMoveMemory",block as struct,hBlock as ulong,blockSize as long,ret as void
    block.prev.struct=prev
    calldll #kernel32,"RtlMoveMemory",hBlock as ulong,block as struct,blockSize as long,ret as void
end sub

function New(name$,age)
    calldll #kernel32,"HeapAlloc",hHeap as ulong,_HEAP_ZERO_MEMORY as ulong,blockSize as long,New as ulong
    if New<>0 then
        blockCount=blockCount+1
        if hFirst=0 then
            hFirst=New
            hLast=New
        else
            call Block.next hLast,New
            call Block.prev New,hLast
            hLast=New
        end if
        call Block.name New,name$
        call Block.age New,age
    end if
end function

sub Delete hBlock
    if hBlock<>0 then
        blockCount=blockCount-1
        if blockCount=0 then
            hFirst=0
            hLast=0
        else
            if hBlock=hFirst then
                hFirst=Block.next(hBlock)
                call Block.prev hFirst,0
            else
                if hBlock=hLast then
                    hLast=Block.prev(hBlock)
                    call Block.next hLast,0
                else
                    call Block.next Block.prev(hBlock),Block.next(hBlock)
                    call Block.prev Block.next(hBlock),Block.prev(hBlock)
                end if
            end if
        end if
        calldll #kernel32,"HeapFree",hHeap as ulong,0 as long,hBlock as ulong,ret as void
    end if
end sub


sub Init
    calldll #kernel32,"HeapCreate",0 as long,10000 as long,0 as long,hHeap as ulong
end sub

sub Uninit
    calldll #kernel32,"HeapDestroy",hHeap as ulong,ret as void
end sub

```



## Nim


```nim
type
  List[T] = object
    head, tail: Node[T]

  Node[T] = ref TNode[T]

  TNode[T] = object
    next, prev: Node[T]
    data: T

proc initList[T](): List[T] = discard

proc newNode[T](data: T): Node[T] =
  new(result)
  result.data = data

proc prepend[T](l: var List[T], n: Node[T]) =
  n.next = l.head
  if l.head != nil: l.head.prev = n
  l.head = n
  if l.tail == nil: l.tail = n

proc append[T](l: var List[T], n: Node[T]) =
  n.next = nil
  n.prev = l.tail
  if l.tail != nil:
    l.tail.next = n
  l.tail = n
  if l.head == nil:
    l.head = n

proc insertAfter[T](l: var List[T], r, n: Node[T]) =
  n.prev = r
  n.next = r.next
  n.next.prev = n
  r.next = n
  if r == l.tail: l.tail = n

proc remove[T](l: var List[T], n: Node[T]) =
  if n == l.tail: l.tail = n.prev
  if n == l.head: l.head = n.next
  if n.next != nil: n.next.prev = n.prev
  if n.prev != nil: n.prev.next = n.next

proc `$`[T](l: var List[T]): string =
  result = ""
  var n = l.head
  while n != nil:
    if result.len > 0: result.add(" -> ")
    result.add($n.data)
    n = n.next

iterator traverseForward[T](l: List[T]): T =
  var n = l.head
  while n != nil:
    yield n.data
    n = n.next

iterator traverseBackward[T](l: List[T]): T =
  var n = l.tail
  while n != nil:
    yield n.data
    n = n.prev

var l = initList[int]()
var n = newNode(12)
var m = newNode(13)
var i = newNode(14)
var j = newNode(15)
l.append(n)
l.prepend(m)
l.insertAfter(m, i)
l.prepend(j)
l.remove(m)

for i in l.traverseForward():
  echo "> ", i

for i in l.traverseBackward():
  echo "< ", i
```

=={{header|Oberon-2}}==

```oberon2

MODULE Collections;
IMPORT Box;

TYPE
  Action = PROCEDURE (o: Box.Object);

        PROCEDURE (dll: DLList) GoForth*(do: Action);
	VAR
		iter: Node;
	BEGIN
		iter := dll.first;
		WHILE iter # NIL DO
			do(iter.value);
			iter := iter.next
		END
	END GoForth;

	PROCEDURE (dll: DLList) GoBack*(do: Action);
	VAR
		iter: Node;
	BEGIN
		ASSERT(dll.last # NIL);
		iter := dll.last;
		WHILE iter # NIL DO
			do(iter.value);
			iter := iter.prev
		END
	END GoBack;

END Collections.

```


## Objeck


```objeck

class Traverse {
   function : Main(args : String[]) ~ Nil {
      list := Collection.IntList->New();
      list->Insert(100);
      list->Insert(50);
      list->Insert(25);
      list->Insert(10);
      list->Insert(5);

      "-- forward --"->PrintLine();
      list->Rewind();
      while(list->More()) {
         list->Get()->PrintLine();
         list->Next();
      };

      "-- backward --"->PrintLine();
      list->Forward();
      while(list->More()) {
         list->Get()->PrintLine();
         list->Previous();
      };
   }
}

```



```txt

-- forward --
100
5
10
25
50
-- backward --
50
25
10
5
100

```



## Oforth


Complete definition is here : [[../Definition#Oforth]]

Defining #forEachNext and #forEachPrev allow to traverse this double linked list using #forEach: and #revEach: syntax


```oforth
DList method: forEachNext
   dup ifNull: [ drop @head ifNull: [ false ] else: [ @head @head true] return ]
   next dup ifNull: [ drop false ] else: [ dup true ] ;

DList method: forEachPrev
   dup ifNull: [ drop @tail ifNull: [ false ] else: [ @tail @tail true] return ]
   prev dup ifNull: [ drop false ] else: [ dup true ] ;

: test
| dl n |
   DList new ->dl
   dl insertFront("A")
   dl insertBack("B")
   dl head insertAfter(DNode new("C", null , null))

   "Traversal (beginning to end) : " println
   dl forEach: n [ n . ]

   "\nTraversal (end to beginning) : " println
   dl revEach: n [ n . ] ;
```


{{out}}

```txt

>test
Traversal (beginning to end) :
A C B
Traversal (end to beginning) :
B C A ok

```



## Oz

Warning: Highly unidiomatic code. (Use built-in lists instead.)

```oz
declare
  proc {Walk Node Action}
     case Node of nil then skip
     [] node(value:V next:N ...) then
	{Action V}
	{Walk @N Action}
     end
  end

  proc {WalkBackwards Node Action}
     Tail = {GetLast Node}
     proc {Loop N}
	case N of nil then skip
	[] node(value:V prev:P ...) then
	   {Action V}
	   {Loop @P}
	end
     end
  in
     {Loop Tail}
  end

  fun {GetLast Node}
     case @(Node.next) of nil then Node
     [] NextNode=node(...) then {GetLast NextNode}
     end
  end

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
  {Walk A Show}
  {WalkBackwards A Show}
```



## Pascal

See [[Doubly-linked_list/Traversal#Delphi | Delphi]]

## Perl 6

Since the list routines are supplied by the generic roles defined in [[Doubly-linked_list/Definition#Perl_6]], it suffices to say:

```perl6
say $dll.list;
say $dll.reverse;
```

These automatically return just the payloads, hiding the elements containing the forward and backward pointers.


## Phix


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

procedure show(integer d)
integer idx = dll[1][d]
    while idx!=1 do
        ?dll[idx][DATA]
        idx = dll[idx][d]
    end while
end procedure
show(NEXT)
?"=="
show(PREV)
```

{{out}}

```txt

{{2,4},{3,1,"ONE"},{4,2,"TWO"},{1,3,"THREE"}}
"ONE"
"TWO"
"THREE"
"=="
"THREE"
"TWO"
"ONE"

```



## PicoLisp


```PicoLisp
# Print the elements a doubly-linked list
(de 2print (DLst)
   (for (L (car DLst) L (cddr L))
      (printsp (car L)) )
   (prinl) )

# Print the elements a doubly-linked list in reverse order
(de 2printReversed (DLst)
   (for (L (cdr DLst) L (cadr L))
      (printsp (car L)) )
   (prinl) )
```

Output for the example data produced in
[[Doubly-linked list/Definition#PicoLisp]] and
[[Doubly-linked list/Element definition#PicoLisp]]:

```txt
: (2print *DLst)                 # Print the list
not was it a cat I saw

: (2printReversed *DLst)         # Print it in reversed order
saw I cat a it was not
```

Output for the example data produced in
[[Doubly-linked list/Element insertion#PicoLisp]]:

```txt
: (2print *DL)                   # Print the list
A C B

: (2printReversed *DL)           # Print it in reversed order
B C A
```



## PL/I


```PL/I
/* To implement a doubly-linked list -- i.e., a 2-way linked list. */
doubly_linked_list: proc options (main);

   define structure
      1 node,
         2 value fixed,
         2 fwd_pointer handle(node),
         2 back_pointer handle(node);

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
            head => back_pointer,
            head => fwd_pointer = bind(:node, null:); /* A NULL link */
         end;
      else
         do;
            t = new(:node:);
            t => back_pointer = tail; /* Point the new tail back to the old */
                                      /* tail.                              */
            tail => fwd_pointer = t;  /* Point the tail to the new node.    */
            t => back_pointer = tail; /* Point the new tail back to the old */
                                      /*  tail.                             */
            tail = t;                 /* Point at the new tail.             */
            tail => fwd_pointer = bind(:node, null:);
                                      /* Set the tail link to NULL          */
            get list (tail => value) copy;
            put skip list (tail => value);
         end;
   end;

   if head = bind(:node, null:) then return; /* Empty list. */

   /* Traverse the list from the head. */
   put skip list ('In a forwards direction, the list has:');
   t = head;
   do while (t ^= bind(:node, null:));
      put skip list (t => value);
      t = t => fwd_pointer;
   end;
   /* Traverse the list from the tail to the head. */
   put skip list ('In the reverse direction, the list has:');
   t = tail;
   do while (t ^= bind(:node, null:));
      put skip list (t => value);
      t = t => back_pointer;
   end;
end doubly_linked_list;
```

Output:

```txt

In a forwards direction, the list has:
       1
       2
       3
       4
       5
      16
       7
       8
       9
      10
In the reverse direction, the list has:
      10
       9
       8
       7
      16
       5
       4
       3
       2
       1
```



## PureBasic


```PureBasic
NewList MyData.i() ; Create a double linked list holding a single value (integer)

;Set up a randomly long linked list in the range 25-125 elements
For i=0 To (Random(100)+25)
  AddElement(MyData())        ; Create a new tailing element
  MyData()=Random(314)        ; Inert a vale into it
Next
;
;Traverse from the beginning of a doubly-linked list to the end.
FirstElement(MyData())
Repeat
  Debug MyData()              ; Present the value in the current cell
Until Not NextElement(MyData())
;
;Traverse from the end to the beginning.
LastElement(MyData())
Repeat
   Debug MyData()             ; Present the value in the current cell
Until Not PreviousElement(MyData())
```



## Python

This provides two solutions.  One that explicitly builds a linked list and traverses it two ways, and another which uses pythons combined list/array class.  Unless two lists explicitly needed to be spliced together in O(1) time, or a double linked list was needed for some other reason, most python programmers would probably use the second solution.

```python
class List:
    def __init__(self, data, next=None, prev=None):
        self.data = data
        self.next = next
        self.prev = prev

    def append(self, data):
        if self.next == None:
            self.next = List(data, None, self)
            return self.next
        else:
            return self.next.append(data)

# Build the list
tail = head = List(10)
for i in [ 20, 30, 40 ]:
    tail = tail.append(i)

# Traverse forwards
node = head
while node != None:
    print(node.data)
    node = node.next

# Traverse Backwards
node = tail
while node != None:
    print(node.data)
    node = node.prev
```


This produces the desired output.  However, I expect most python programmers would do the following instead:


```python
l = [ 10, 20, 30, 40 ]
for i in l:
    print(i)
for i in reversed(l):    # reversed produces an iterator, so only O(1) memory is used
    print(i)
```


Double-ended queues in python are provided by the collections.deque class and the array/list type can perform all the operations of a C++ vector (and more), so building one's own doubly-linked list would be restricted to very specialized situations.


## Racket

See  [[Doubly-Linked List]] for the structure definitions.

These functions traverse the list in the two directions.
They create a native (singly-linked) list by adding to the front, so they traverse in the reverse order from the desired result order.

```racket
(define (dlist-elements dlist)
  (let loop ([elements '()] [dlink (dlist-tail dlist)])
    (if dlink
        (loop (cons (dlink-content dlink) elements) (dlink-prev dlink))
        elements)))

(define (dlist-elements/reverse dlist)
  (let loop ([elements '()] [dlink (dlist-head dlist)])
    (if dlink
        (loop (cons (dlink-content dlink) elements) (dlink-next dlink))
        elements)))
```



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
<pre style="height:30ex;overflow:scroll">
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
Was it a black cat I saw there, in the shadows, stalking its prey (and next meal

                               ─── adding to head: Oy! ───

                               ─── showing list ───
Oy! Was it a black cat I saw there, in the shadows, stalking its prey (and next

```



## Ring


```ring

# Project : Doubly-linked list/Traversal

trav = [123, 789, 456]
travfor = sort(trav)
see "Traverse forwards:" + nl
see travfor
see nl
travback = reverse(travfor)
see "Traverse backwards:" + nl
see travback
see nl

```

Output:

```txt

Traverse forwards:
123
456
789

Traverse backwards:
789
456
123

```



## Ruby


```ruby
class DListNode
  def get_tail
    # parent class (ListNode) includes Enumerable, so the find method is available to us
    self.find {|node| node.succ.nil?}
  end

  def each_previous(&b)
    yield self
    self.prev.each_previous(&b) if self.prev
  end
end

head = DListNode.from_array([:a, :b, :c])
head.each {|node| p node.value}
head.get_tail.each_previous {|node| p node.value}
```



## Salmon

Without explicit type information:

```Salmon
class item(init_data)
  {
    variable next,
             previous,
             data := init_data;
  };

function prepend(tail, new_data)
  {
    immutable result := item(new_data);
    result.next := tail;
    result.previous := null;
    if (tail != null)
        tail.previous := result;;
    return result;
  };

variable my_list := null;
my_list := prepend(my_list, "R");
my_list := prepend(my_list, "o");
my_list := prepend(my_list, "s");
my_list := prepend(my_list, "e");
my_list := prepend(my_list, "t");
my_list := prepend(my_list, "t");
my_list := prepend(my_list, "a");

"Items in the list going forward:"!
variable follow := my_list;
while (true)
  {
    follow.data!
    if (follow.next == null)
        break;;
  }
step
    follow := follow.next;;

"Items in the list going backward:"!
while (follow != null)
    follow.data!
step
    follow := follow.previous;;
```


With explicit type information:

```Salmon
class item(init_data : string)
  {
    variable next: item | {null},
             previous : item | {null},
             data : string := init_data;
  };

function prepend(tail : item | {null}, new_data : string) returns item
  {
    immutable result := item(new_data);
    result.next := tail;
    result.previous := null;
    if (tail != null)
        tail.previous := result;;
    return result;
  };

variable my_list : item | {null} := null;
my_list := prepend(my_list, "R");
my_list := prepend(my_list, "o");
my_list := prepend(my_list, "s");
my_list := prepend(my_list, "e");
my_list := prepend(my_list, "t");
my_list := prepend(my_list, "t");
my_list := prepend(my_list, "a");

"Items in the list going forward:"!
variable follow : item | {null} := my_list;
while (true)
  {
    follow.data!
    if (follow.next == null)
        break;;
  }
step
    follow := follow.next;;

"Items in the list going backward:"!
while (follow != null)
    follow.data!
step
    follow := follow.previous;;
```


Both of these produce the following output:

```txt

Items in the list going forward:
a
t
t
e
s
o
R
Items in the list going backward:
R
o
s
e
t
t
a

```



## Scala


```Scala
import java.util

object DoublyLinkedListTraversal extends App {

  private val ll = new util.LinkedList[String]

  private def traverse(iter: util.Iterator[String]) =
    while (iter.hasNext) iter.next

  traverse(ll.iterator)
  traverse(ll.descendingIterator)
}
```


## Tcl

Assuming that the <code>List</code> class from [[Doubly-Linked List (element)#Tcl|this other task]] is already present...

```tcl
# Modify the List class to add the iterator methods
oo::define List {
    method foreach {varName script} {
        upvar 1 $varName v
        for {set node [self]} {$node ne ""} {set node [$node next]} {
            set v [$node value]
            uplevel 1 $script
        }
    }
    method revforeach {varName script} {
        upvar 1 $varName v
        for {set node [self]} {$node ne ""} {set node [$node previous]} {
            set v [$node value]
            uplevel 1 $script
        }
    }
}

# Demonstrating...
set first [List new a [List new b [List new c [set last [List new d]]]]]
puts "Forward..."
$first foreach char { puts $char }
puts "Backward..."
$last revforeach char { puts $char }
```

Which produces this output:

```txt
Forward...
a
b
c
d
Backward...
d
c
b
a
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
a,c := Node("a"), a.append("b").append("c");
n:=a; while(n){ print(n,"  "); n=n.next }
println();
n:=c; while(n){ print(n,"  "); n=n.prev }
println();
```

{{out}}

```txt

a  b  c
c  b  a

```


{{omit from|ACL2}}
{{omit from|GUISS}}
