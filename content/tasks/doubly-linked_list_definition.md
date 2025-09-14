+++
title = "Doubly-linked list/Definition"
description = ""
date = 2019-08-30T19:45:05Z
aliases = []
[extra]
id = 3273
[taxonomies]
categories = ["task", "Data Structures"]
tags = []
languages = [
  "ada",
  "algol_68",
  "arm_assembly",
  "autohotkey",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
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
  "m2000_interpreter",
  "nim",
  "objeck",
  "oforth",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "tcl",
  "visual_basic_dotnet",
  "zkl",
]
+++

## Task

Define the data structure for a complete Doubly Linked List.

* The structure should support adding elements to the head, tail and middle of the list.
* The structure should not allow circular loops


## Ada

Examples already in other doubly-linked list tasks: see [[Doubly-linked list/Element insertion#Ada]] and [[Doubly-linked list/Traversal#Ada]].

Ada 2005 defines doubly-linked lists in [http://www.adaic.com/standards/05rm/html/RM-A-18-3.html A.18.3 The Package Containers.Doubly_Linked_Lists].


## ALGOL 68

'''File: prelude/Doubly-linked_list_Link.a68'''
```algol68
# -*- coding: utf-8 -*- #
COMMENT REQUIRES:
  MODE VALUE = ~;
# For example: #
  MODE VALUE = UNION(INT, REAL, COMPL)
END COMMENT

MODE LINKNEW = STRUCT (
  LINK next, prev,
  VALUE value
);

MODE LINK = REF LINKNEW;

SKIP
```
'''File: prelude/Doubly-linked_list_Operator.a68'''
```algol68
# -*- coding: utf-8 -*- #
MODE LISTNEW = LINKNEW;
MODE LIST = REF LISTNEW;

OP LISTINIT = (LIST self)LIST: (
  self := (self, self, ~);
  self
);

OP ISEMPTY = (LIST self)BOOL:
  (LIST(prev OF self) :=: LIST(self)) AND (LIST(self) :=: LIST(next OF self));

OP HEAD = (LIST self)LINK: next OF self;

OP TAIL = (LIST self)LINK: prev OF self;

# insert after #
OP +:= = (LINK cursor, LINK link)LINK: (
  next OF link := next OF cursor;
  prev OF link := cursor;
  next OF cursor := link;
  prev OF next OF link := link;
  link
);

# insert before #
OP +=: = (LINK link, LINK cursor)LINK: prev OF cursor +:= link;

# delete current and step forward #
OP -:= = (LIST ignore, LINK link)LINK: (
  next OF prev OF link := next OF link;
  prev OF next OF link := prev OF link;
  next OF link := prev OF link := NIL; # garbage collection hint #
  link
);

# delete current and step backward #
PRIO -=: = 1;
OP -=: = (LIST link, LIST ignore)LINK: (
  ignore -:= link; prev OF link
);

PRIO ISIN = 1; # low priority #

OP ISIN = (LINK link, LIST self)BOOL:
  link ISNT LINK(self);

SKIP
```
'''File: test/Doubly-linked_list_Operator_Usage.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #
MODE VALUE = STRING; # user defined data type #
PR READ "prelude/Doubly-linked_list_Link.a68" PR;
PR READ "prelude/Doubly-linked_list_Operator.a68" PR;

main: (

    []VALUE sample = ("Was", "it", "a", "cat", "I", "saw");
    LIST example list := LISTINIT HEAP LISTNEW;
    LINK this;

# Add some data to a list #
    FOR i TO UPB sample DO
        this := HEAP LINKNEW;
        value OF this := sample[i];
        TAIL example list +:= this
    OD;

# Iterate throught the list forward #
    this := HEAD example list;
    print("Iterate forward: ");
    WHILE this ISIN example list DO
        print((value OF this, " "));
        this := next OF this
    OD;
    print(new line);

# Iterate throught the list backward #
    this := TAIL example list;
    print("Iterate backward: ");
    WHILE this ISIN example list DO
        print((value OF this, " "));
        this := prev OF this
    OD;
    print(new line);

# Finally empty the list #
    print("Empty from tail: ");
    WHILE NOT ISEMPTY example list DO
          this := (example list -:= TAIL example list);
          print((value OF this, " "))
    OD;
    print(new line)
)
```

```txt

Iterate forward: Was it a cat I saw
Iterate backward: saw I cat a it Was
Empty from tail: saw I cat a it Was

```


## ARM Assembly

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program defDblList.s   */

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
szCarriageReturn:        .asciz "\n"
szMessErreur:            .asciz "Error detected.\n"

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

    b 100f
99:
    ldr r0,iAdrszMessErreur
    bl affichageMess
100:                                    @ standard end of the program
    mov r7, #EXIT                       @ request to exit program
    svc 0                               @ perform system call
iAdrszMessInitListe:       .int szMessInitListe
iAdrszMessErreur:          .int szMessErreur
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
    pop {r1-r4,lr}                       @ restaur registers
    bx lr                                @ return
/******************************************************************/
/*     insert value at list tail                        */
/******************************************************************/
/* r0 contains the address of the list structure */
/* r1 contains value */
insertTail:
    push {r1-r4,lr}                         @ save  registers
    mov r4,r0                            @ save list address
    mov r0,r1                            @ value
    bl createNode                        @ create new node
    cmp r0,#-1
    beq 100f                             @ allocation error
    ldr r2,[r4,#dllist_tail]             @ load address last node
    str r2,[r0,#NDlist_prev]             @ store in previous pointer on new node
    mov r1,#0                            @ store null un next pointer
    str r1,[r0,#NDlist_next]
    str r0,[r4,#dllist_tail]             @ store address new node on list tail
    cmp r2,#0                            @ address last node is null ?
    strne r0,[r2,#NDlist_next]           @ no store address new node in next pointer
    streq r0,[r4,#dllist_head]           @ else store in head list
100:
    pop {r1-r4,lr}                          @ restaur registers
    bx lr                                @ return
/******************************************************************/
/*     insert value after other element                        */
/******************************************************************/
/* r0 contains the address of the list structure */
/* r1 contains value to search*/
/* r2 contains value to insert */
insertAfter:
    push {r1-r5,lr}                         @ save  registers
    mov r4,r0                               @ save list address
    bl searchValue                          @ search this value in r1
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
    bx lr                                   @ return
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

see [[Doubly-linked list/AutoHotkey]]


## C



```c
/* double linked list */
#include <stdio.h>
#include <stdlib.h>

struct List {
   struct MNode *head;
   struct MNode *tail;
   struct MNode *tail_pred;
};

struct MNode {
   struct MNode *succ;
   struct MNode *pred;
};

typedef struct MNode *NODE;
typedef struct List *LIST;

/*
** LIST l = newList()
** create (alloc space for) and initialize a list
*/
LIST newList(void);

/*
** int isEmpty(LIST l)
** test if a list is empty
*/
int isEmpty(LIST);

/*
** NODE n = getTail(LIST l)
** get the tail node of the list, without removing it
*/
NODE getTail(LIST);

/*
** NODE n = getHead(LIST l)
** get the head node of the list, without removing it
*/
NODE getHead(LIST);

/*
** NODE rn = addTail(LIST l, NODE n)
** add the node n to the tail of the list l, and return it (rn==n)
*/
NODE addTail(LIST, NODE);

/*
** NODE rn = addHead(LIST l, NODE n)
** add the node n to the head of the list l, and return it (rn==n)
*/
NODE addHead(LIST, NODE);

/*
** NODE n = remHead(LIST l)
** remove the head node of the list and return it
*/
NODE remHead(LIST);

/*
** NODE n = remTail(LIST l)
** remove the tail node of the list and return it
*/
NODE remTail(LIST);

/*
** NODE rn = insertAfter(LIST l, NODE r, NODE n)
** insert the node n after the node r, in the list l; return n (rn==n)
*/
NODE insertAfter(LIST, NODE, NODE);

/*
** NODE rn = removeNode(LIST l, NODE n)
** remove the node n (that must be in the list l) from the list and return it (rn==n)
*/
NODE removeNode(LIST, NODE);


LIST newList(void)
{
    LIST tl = malloc(sizeof(struct List));
    if ( tl != NULL )
    {
       tl->tail_pred = (NODE)&tl->head;
       tl->tail = NULL;
       tl->head = (NODE)&tl->tail;
       return tl;
    }
    return NULL;
}

int isEmpty(LIST l)
{
   return (l->head->succ == 0);
}

NODE getHead(LIST l)
{
  return l->head;
}

NODE getTail(LIST l)
{
  return l->tail_pred;
}


NODE addTail(LIST l, NODE n)
{
    n->succ = (NODE)&l->tail;
    n->pred = l->tail_pred;
    l->tail_pred->succ = n;
    l->tail_pred = n;
    return n;
}

NODE addHead(LIST l, NODE n)
{
    n->succ = l->head;
    n->pred = (NODE)&l->head;
    l->head->pred = n;
    l->head = n;
    return n;
}

NODE remHead(LIST l)
{
   NODE h;
   h = l->head;
   l->head = l->head->succ;
   l->head->pred = (NODE)&l->head;
   return h;
}

NODE remTail(LIST l)
{
   NODE t;
   t = l->tail_pred;
   l->tail_pred = l->tail_pred->pred;
   l->tail_pred->succ = (NODE)&l->tail;
   return t;
}

NODE insertAfter(LIST l, NODE r, NODE n)
{
   n->pred = r; n->succ = r->succ;
   n->succ->pred = n; r->succ = n;
   return n;
}

NODE removeNode(LIST l, NODE n)
{
   n->pred->succ = n->succ;
   n->succ->pred = n->pred;
   return n;
}
```


Simple test:


```c
/* basic test */

struct IntNode {
  struct MNode node;
  int data;
};

int main()
{
    int i;
    LIST lista;
    struct IntNode *m;
    NODE n;

    lista = newList();
    if ( lista != NULL )
    {
      for(i=0; i < 5; i++)
      {
          m = malloc(sizeof(struct IntNode));
          if ( m != NULL )
          {
             m->data = rand()%64;
             addTail(lista, (NODE)m);
          }
      }
      while( !isEmpty(lista) )
      {
            m = (struct IntNode *)remTail(lista);
            printf("%d\n", m->data);
            free(m);
      }
      free(lista);
    }
}
```



## C++

```cpp
#include <iostream>
#include <list>

int main ()
{
    std::list<int> numbers {1, 5, 7, 0, 3, 2};
    numbers.insert(numbers.begin(), 9); //Insert at the beginning
    numbers.insert(numbers.end(), 4); //Insert at the end
    auto it = std::next(numbers.begin(), numbers.size() / 2); //Iterator to the middle of the list
    numbers.insert(it, 6); //Insert in the middle
    for(const auto& i: numbers)
        std::cout << i << ' ';
    std::cout << '\n';
}
```

```txt

9 1 5 7 6 0 3 2 4

```



## C#


The .NET framework provides the <code>LinkedListNode<T></code> class, which represents an individual node of a linked list, and the <code>LinkedList<T></code> class, which provides abstractions to read and modify a list as if it were a single object. The <code>LinkedListNode<T>.Next</code> and <code>LinkedListNode<T>.Previous</code> properties is read-only, ensuring that all lists must be created using <code>LinkedList<T></code> and that each list can only be mutated using the methods of its <code>LinkedList<T></code> instance (the appropriate .NET accessibility modifiers are used to hide these implementation details).

One node instance is forbidden to be in multiple lists; this is enforced using the <code>LinkedListNode<T>.List</code> property, which is set accordingly when a node is added to a <code>LinkedList<T></code> and set to <code>null</code> when it is removed. This also has the effect of preventing cycles.

Though mutating the ''structure'' of a list can only be done through the <code>LinkedList<T></code> class, mutating the values contained by the nodes of a list is done through its individual <code>LinkedListNode<T></code> instances, as the <code>LinkedListNode<T>.Next</code>.Value property is settable.


```C sharp
using System.Collections.Generic;

class Program
{
    static void Main(string[] args)
    {
        LinkedList<string> list = new LinkedList<string>();
        list.AddFirst(".AddFirst() adds at the head.");
        list.AddLast(".AddLast() adds at the tail.");
        LinkedListNode<string> head = list.Find(".AddFirst() adds at the head.");
        list.AddAfter(head, ".AddAfter() adds after a specified node.");
        LinkedListNode<string> tail = list.Find(".AddLast() adds at the tail.");
        list.AddBefore(tail, "Betcha can't guess what .AddBefore() does.");

        System.Console.WriteLine("Forward:");
        foreach (string nodeValue in list) { System.Console.WriteLine(nodeValue); }

        System.Console.WriteLine("\nBackward:");
        LinkedListNode<string> current = tail;
        while (current != null)
        {
            System.Console.WriteLine(current.Value);
            current = current.Previous;
        }
    }
}
```


```txt
Forward:
.AddFirst() adds at the head.
.AddAfter() adds after a specified node.
Betcha can't guess what .AddBefore() does.
.AddLast() adds at the tail.

Backward:
.AddLast() adds at the tail.
Betcha can't guess what .AddBefore() does.
.AddAfter() adds after a specified node.
.AddFirst() adds at the head.
```



## Clojure


```Clojure
(ns double-list)

(defprotocol PDoubleList
  (get-head [this])
  (add-head [this x])
  (get-tail [this])
  (add-tail [this x])
  (remove-node [this node])
  (add-before [this node x])
  (add-after [this node x])
  (get-nth [this n]))

(defrecord Node [prev next data])

(defn make-node
  "Create an internal or finalized node"
  ([prev next data] (Node. prev next data))
  ([m key] (when-let [node (get m key)]
            (assoc node :m m :key key))))

(defn get-next [node] (make-node (:m node) (:next node)))
(defn get-prev [node] (make-node (:m node) (:prev node)))

(defn- seq* [m start next]
  (seq
   (for [x (iterate #(get m (next %)) (get m start))
         :while x]
     (:data x))))

(defmacro when->
  ([x pred form] `(let [x# ~x] (if ~pred (-> x# ~form) x#)))
  ([x pred form & more] `(when-> (when-> ~x ~pred ~form) ~@more)))

(declare get-nth-key)

(deftype DoubleList [m head tail]
  Object
    (equals [this x]
      (and (instance? DoubleList x)
           (= m (.m ^DoubleList x))))
    (hashCode [this] (hash (or this ())))
  clojure.lang.Sequential
  clojure.lang.Counted
    (count [_] (count m))
  clojure.lang.Seqable
    (seq [_] (seq* m head :next))
  clojure.lang.Reversible
    (rseq [_] (seq* m tail :prev))
  clojure.lang.IPersistentCollection
    (empty [_] (DoubleList. (empty m) nil nil))
    (equiv [this x]
      (and (sequential? x)
           (= (seq x) (seq this))))
    (cons [this x] (.add-tail this x))
  PDoubleList
    (get-head [_] (make-node m head))
    (add-head [this x]
      (let [new-key (Object.)
            m (when-> (assoc m new-key (make-node nil head x))
                head (assoc-in [head :prev] new-key))
            tail (if tail tail new-key)]
        (DoubleList. m new-key tail)))
    (get-tail [_] (make-node m tail))
    (add-tail [this x]
      (if-let [tail (.get-tail this)]
        (.add-after this tail x)
        (.add-head this x)))
    (remove-node [this node]
      (if (get m (:key node))
        (let [{:keys [prev next key]} node
              head (if prev head next)
              tail (if next tail prev)
              m (when-> (dissoc m key)
                  prev (assoc-in [prev :next] next)
                  next (assoc-in [next :prev] prev))]
          (DoubleList. m head tail))
        this))
    (add-after [this node x]
      (if (get m (:key node))
        (let [{:keys [prev next key]} node
              new-key (Object.)
              m (when-> (-> (assoc m new-key  (make-node key next x))
                            (assoc-in , [key :next] new-key))
                  next (assoc-in [next :prev] new-key))
              tail (if next tail new-key)]
          (DoubleList. m head tail))
        this))
    (add-before [this node x]
      (if (:prev node)
        (.add-after this (get-prev node) x)
        (.add-head this x)))
    (get-nth [this n] (make-node m (get-nth-key this n))))

(defn get-nth-key [^DoubleList this n]
  (if (< -1 n (.count this))
    (let [[start next n] (if (< n (/ (.count this) 2))
                           [(.head this) :next n]
                           [(.tail this) :prev (- (.count this) n 1)])]
      (nth (iterate #(get-in (.m this) [% next]) start) n))
    (throw (IndexOutOfBoundsException.))))

(defn double-list
  ([] (DoubleList. nil nil nil))
  ([coll] (into (double-list) coll)))

(defmethod print-method DoubleList [dl w]
  (print-method (interpose '<-> (seq dl)) w))

(defmethod print-method Node [n w]
  (print-method (symbol "#:double_list.Node") w)
  (print-method (into {} (dissoc n :m)) w))
```

Usage:

```Clojure
(use 'double-list)
;=> nil
(def dl (double-list (range 10)))
;=> #'user/dl
dl
;=> (0 <-> 1 <-> 2 <-> 3 <-> 4 <-> 5 <-> 6 <-> 7 <-> 8 <-> 9)
(remove-node dl (get-tail dl))
;=> (0 <-> 1 <-> 2 <-> 3 <-> 4 <-> 5 <-> 6 <-> 7 <-> 8)
dl
;=> (0 <-> 1 <-> 2 <-> 3 <-> 4 <-> 5 <-> 6 <-> 7 <-> 8 <-> 9)
((juxt seq rseq) dl)
;=> [(0 1 2 3 4 5 6 7 8 9) (9 8 7 6 5 4 3 2 1 0)]
(remove-node dl (get-nth dl 5))
;=> (0 <-> 1 <-> 2 <-> 3 <-> 4 <-> 6 <-> 7 <-> 8 <-> 9)
(add-after *1 (get-nth *1 4) 10)
;=> (0 <-> 1 <-> 2 <-> 3 <-> 4 <-> 10 <-> 6 <-> 7 <-> 8 <-> 9)
(get-head *1)
;=> #:double_list.Node{:prev nil, :next #<Object ...>, :data 0, :key <Object ...>}
(get-next *1)
;=> #:double_list.Node{:prev #<Object ...>, :next #<Object ...>, :data 1, :key #<Object ...>}
(get-prev *1)
;=> #:double_list.Node{:prev #<Object ...>, :next #<Object ...>, :data 1, :key #<Object ...>}
```



## Common Lisp



```lisp
(defstruct dlist head tail)
(defstruct dlink content prev next)

(defun insert-between (dlist before after data)
  "Insert a fresh link containing DATA after existing link BEFORE if not nil and before existing link AFTER if not nil"
  (let ((new-link (make-dlink :content data :prev before :next after)))
    (if (null before)
        (setf (dlist-head dlist) new-link)
        (setf (dlink-next before) new-link))
    (if (null after)
        (setf (dlist-tail dlist) new-link)
        (setf (dlink-prev after) new-link))
    new-link))

(defun insert-before (dlist dlink data)
  "Insert a fresh link containing DATA before existing link DLINK"
  (insert-between dlist (dlink-prev dlink) dlink data))

(defun insert-after (dlist dlink data)
  "Insert a fresh link containing DATA after existing link DLINK"
  (insert-between dlist dlink (dlink-next dlink) data))

(defun insert-head (dlist data)
  "Insert a fresh link containing DATA at the head of DLIST"
  (insert-between dlist nil (dlist-head dlist) data))

(defun insert-tail (dlist data)
  "Insert a fresh link containing DATA at the tail of DLIST"
  (insert-between dlist (dlist-tail dlist) nil data))

(defun remove-link (dlist dlink)
  "Remove link DLINK from DLIST and return its content"
  (let ((before (dlink-prev dlink))
        (after (dlink-next dlink)))
    (if (null before)
        (setf (dlist-head dlist) after)
        (setf (dlink-next before) after))
    (if (null after)
        (setf (dlist-tail dlist) before)
        (setf (dlink-prev after) before))))

(defun dlist-elements (dlist)
  "Returns the elements of DLIST as a list"
  (labels ((extract-values (dlink acc)
             (if (null dlink)
                 acc
                 (extract-values (dlink-next dlink) (cons (dlink-content dlink) acc)))))
    (reverse (extract-values (dlist-head dlist) nil))))
```


The following produces <code>(1 2 3 4)</code>.


```lisp
(let ((dlist (make-dlist)))
  (insert-head dlist 1)
  (insert-tail dlist 4)
  (insert-after dlist (dlist-head dlist) 2)
  (let* ((next-to-last (insert-before dlist (dlist-tail dlist) 3))
         (bad-link (insert-before dlist next-to-last 42)))
    (remove-link dlist bad-link))
  (print (dlist-elements dlist)))
```



## D


```d
import std.stdio;

class LinkedList(T)
{
 Node!(T) head, tail;

 /** Iterate in the forward direction. */
 int opApply (int delegate(uint, Node!(T)) dg)
 {
  uint i = 0;
  auto link = head;
  int result = 0;
  while (link)
  {
   result = dg (i, link);
   if (result) return result;
   i++;
   link = link.next;
  }
  return result;
 }

 static LinkedList!(T) fromArray (T[] array)
 {
  Node!(T) link = null;
  auto head = link;
  auto self = new LinkedList!(T);
  foreach (elem; array)
  {
   link = new Node!(T)(null, link, elem, self);
   if (!head)
    head = link;
  }
  return self;
 }
}

class Node(T)
{
 Node!(T) next;
 Node!(T) previous;
 LinkedList!(T) parent;
 T value;

 this (Node!(T) next, Node!(T) previous, T value, LinkedList!(T) parent)
 in
 {
  assert (parent !is null);
 }
 body
 {
  this.next = next;
  if (next)
   next.previous = this;
  if (previous)
   previous.next = this;
  this.previous = previous;
  this.value = value;
  this.parent = parent;

  if (parent.head == next)
   parent.head = this;
  if (parent.tail == previous)
   parent.tail = this;
 }

 /** Insert an element after this one. */
 void insertAfter (T value)
 {
  new Node!(T)(next, this, value, parent);
 }

 /** Insert an element before this one. */
 void insertBefore (T value)
 {
  new Node!(T)(this, previous, value, parent);
 }

 /** Remove the current node from the list. */
 void remove ()
 {
  if (next)
   next.previous = previous;
  if (previous)
   previous.next = next;
  if (parent.tail == this)
   parent.tail = previous;
  if (parent.head == this)
   parent.head = next;
 }
}

void main ()
{
 string[] sample = ["was", "it", "a", "cat", "I", "saw"];
 auto list = LinkedList!string.fromArray (sample);
 for (auto elem = list.head; elem; elem = elem.next)
 {
  writef ("%s ", elem.value);
  if (elem.value == "it") elem.insertAfter("really");
 }
 writeln;
 for (auto elem = list.tail; elem; elem = elem.previous)
 {
  writef ("%s ", elem.value);
 }
 writeln;
}
```

```txt

Iterate forward: Was it really a cat I saw
Iterate backward: saw I cat a really it Was
Empty from tail: saw I cat a really it Was

```



## E


```e
def makeDLList() {
    def firstINode
    def lastINode

    def makeNode(var value, var prevI, var nextI) {
        # To meet the requirement that the client cannot create a loop, the
        # inter-node refs are protected: clients only get the external facet
        # with invariant-preserving operations.
        def iNode

        def node { # external facet

            to get() { return value }
            to put(new) { value := new }

            /** Return the value of the element of the list at the specified offset
                from this element. */
            to get(index :int) {
                if (index > 0 && node.hasNext()) {
                    return nextI.node().get(index - 1)
                } else if (index < 0 && node.hasPrev()) {
                    return prevI.node().get(index + 1)
                } else if (index <=> 0) {
                    return value
                } else {
                    throw("index out of range in dlList")
                }
            }
            to hasPrev() {
                return prevI != firstINode && prevI != null
            }
            to prev() {
                if (!node.hasPrev()) {
                    throw("there is no previous node")
                }
                return prevI.node()
            }
            to hasNext() {
                return nextI != lastINode && nextI != null
            }
            to next() {
                if (!node.hasNext()) {
                    throw("there is no next node")
                }
                return nextI.node()
            }
            to remove() {
                if (prevI == null || nextI == null) { return }
                prevI.setNextI(nextI)
                nextI.setPrevI(prevI)
                prevI := null
                nextI := null
            }
            to insertAfter(newValue) {
                def newI := makeNode(newValue, iNode, nextI)
                nextI.setPrevI(newI)
                nextI := newI
            }
            to insertBefore(newValue) {
                prevI.node().insertAfter(newValue)
            }
        }

        bind iNode { # internal facet
            to node() { return node }
            to nextI() { return nextI }
            to prevI() { return prevI }
            to setNextI(new) { nextI := new }
            to setPrevI(new) { prevI := new }
        }

        return iNode
    } # end makeNode

    bind firstINode := makeNode(null, Ref.broken("no first prev"), lastINode)
    bind lastINode := makeNode(null, firstINode, Ref.broken("no last next"))

    def dlList {
        to __printOn(out) {
            out.print("<")
            var sep := ""
            for x in dlList {
                out.print(sep)
                out.quote(x)
                sep := ", "
            }
            out.print(">")
        }
        to iterate(f) {
            var n := firstINode
            while (n.node().hasNext()) {
                n := n.nextI()
                f(n.node(), n.node()[])
            }
        }
        to atFirst() { return firstINode.nextI().node() }
        to atLast() { return lastINode.prevI().node() }
        to insertFirst(new) { return firstINode.node().insertAfter(new) }
        to push(new) { return lastINode.node().insertBefore(new) }

        /** Return the node which has the specified value */
        to nodeOf(value) {
            for node => v ? (v == value) in dlList { return node }
        }
    }
    return dlList
}
```



```e
? def list := makeDLList()
# value: <>

? list.push(1)
? list
# value: <1>

? list.push(10)
? list.push(100)
? list
# value: <1, 10, 100>

? list.atFirst().insertAfter(5)
? list
# value: <1, 5, 10, 100>

? list.insertFirst(0)
? list
# value: <0, 1, 5, 10, 100>

? list.atLast().prev().remove()
? list
# value: <0, 1, 5, 100>

? list.atLast()[] := 10
? list
# value: <0, 1, 5, 10>

? for x in 11..20 { list.push(x) }
? list
# value: <0, 1, 5, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20>
```



## Erlang

As with [[Singly-linked_list/Element_insertion]] a process is used to get mutability in Erlang's single assignment world.

```Erlang

-module( doubly_linked_list ).

-export( [append/2, foreach_next/2, foreach_previous/2, free/1, insert/3, new/1, task/0] ).

append( New, Start ) -> Start ! {append, New}.

foreach_next( Fun, Start ) -> Start ! {foreach_next, Fun}.

foreach_previous( Fun, Start ) -> Start ! {foreach_previous, Fun}.

free( Element ) -> Element ! {free}.

insert( New, After, Start ) -> Start ! {insert, New, After}.

new( Data ) -> erlang:spawn( fun() -> loop( Data, noprevious, nonext ) end ).

task() ->
    A = new( a ),
    B = new( b ),
    append( B, A ),
    C = new( c ),
    insert( C, A, A ),
    foreach_next( fun(Data) -> io:fwrite("foreach_next ~p~n", [Data]) end, A ),
    timer:sleep( 100 ),
    foreach_previous( fun(Data) -> io:fwrite("foreach_previous ~p~n", [Data]) end, B ).



loop( Data, Previous, Next ) ->
      My_pid = erlang:self(),
      receive
      {append, New} ->
             New_next = loop_append( New, Next, My_pid ),
             loop( Data, Previous, New_next );
      {foreach_next, Fun} ->
                catch Fun( Data ),
                loop_foreach_next( Fun, Next ),
                loop( Data, Previous, Next );
      {foreach_previous, Fun} ->
                catch Fun( Data ),
                loop_foreach_previous( Fun, Previous ),
                loop( Data, Previous, Next );
      {free} ->
             ok;
      {insert, New, My_pid} ->
	     New ! {previous, My_pid},
             loop_append( Next, New, My_pid ),
             loop( Data, Previous, New );
      {insert, New, After} ->
             Next ! {insert, New, After},
             loop( Data, Previous, Next );
      {previous, New_previous} ->
             loop( Data, New_previous, Next )
        end.

loop_append( New, nonext, My_pid ) ->
        New ! {previous, My_pid},
        New;
loop_append( New, Next, _My_pid ) ->
        Next ! {append, New},
        Next.

loop_foreach_next( _Fun, nonext ) -> ok;
loop_foreach_next( Fun, Next ) -> Next ! {foreach_next, Fun}.

loop_foreach_previous( _Fun, noprevious ) -> ok;
loop_foreach_previous( Fun, Next ) -> Next ! {foreach_previous, Fun}.

```


=={{header|F Sharp|F#}}==

```fsharp
type DListAux<'T> = {mutable prev: DListAux<'T> option; data: 'T; mutable next: DListAux<'T> option}
type DList<'T> = {mutable front: DListAux<'T> option; mutable back: DListAux<'T> option} //'

let empty() = {front=None; back=None}

let addFront dlist elt =
  match dlist.front with
  | None ->
      let e = Some {prev=None; data=elt; next=None}
      dlist.front <- e
      dlist.back <- e
  | Some e2 ->
      let e1 = Some {prev=None; data=elt; next=Some e2}
      e2.prev <- e1
      dlist.front <- e1

let addBack dlist elt =
  match dlist.back with
  | None -> addFront dlist elt
  | Some e2 ->
      let e1 = Some {prev=Some e2; data=elt; next=None}
      e2.next <- e1
      dlist.back <- e1

let addAfter dlist link elt =
  if link.next = dlist.back then addBack dlist elt else
    let e = Some {prev=Some link; data=elt; next=link.next}
    link.next <- e
```




## Fortran

Tested with g95 and gfortran v. 4.6.

```fortran

module dlist
  implicit none
  type node
     type(node), pointer :: next => null()
     type(node), pointer :: prev => null()
     integer :: data
  end type node

  type dll
     type(node), pointer :: head => null()
     type(node), pointer :: tail => null()
     integer :: num_nodes = 0
  end type dll

  public  :: node, dll, append, prepend, insert, dump, reverse_dump, tidy
  private :: init

contains
  ! Create a new doubly-linked list
  elemental type(dll) function new_dll()
    new_dll = dll(null(),null(),0)
    return
  end function new_dll

  ! Append an element to the end of the list
  elemental subroutine append(dl2, value)
    type(dll), intent(inout) :: dl2
    integer, intent(in)      :: value

    type(node), pointer :: np

    ! If the list is empty
    if (dl2%num_nodes == 0) then
       call init(dl2, value)
       return
    end if

    ! Add new element to the end
    dl2%num_nodes = dl2%num_nodes + 1
    np => dl2%tail
    allocate(dl2%tail)
    dl2%tail%data = value
    dl2%tail%prev => np
    dl2%tail%prev%next => dl2%tail
  end subroutine append

  ! Prepend an element to the beginning of the list
  elemental subroutine prepend(dl2, value)
    type(dll), intent(inout) :: dl2
    integer, intent(in)      :: value

    type(node), pointer :: np

    if (dl2%num_nodes == 0) then
       call init(dl2, value)
       return
    end if

    dl2%num_nodes = dl2%num_nodes + 1
    np => dl2%head
    allocate(dl2%head)
    dl2%head%data = value
    dl2%head%next => np
    dl2%head%next%prev => dl2%head
  end subroutine prepend

  ! Insert immediately before the given index
  elemental subroutine insert(dl2, index, value)
    type(dll), intent(inout) :: dl2
    integer, intent(in)      :: index
    integer, intent(in)      :: value

    type(node), pointer :: element
    type(node), pointer :: np1, np2
    integer             :: i

    if (dl2%num_nodes == 0) then
       call init(dl2, value)
       return
    end if

    ! If index is beyond the end then append
    if (index > dl2%num_nodes) then
       call append(dl2, value)
       return
    end if

    ! If index is less than 1 then prepend
    if (index <= 1) then
       call prepend(dl2, value)
       return
    end if

    ! Find the node at position 'index' counting from 1
    np1 => dl2%head
    do i=1, index-2
       np1 => np1%next
    end do
    np2 => np1%next

    ! Create the new node
    allocate(element)
    element%data = value

    ! Connect it up
    element%prev => np1
    element%next => np2
    np1%next => element
    np2%prev => element
    dl2%num_nodes = dl2%num_nodes + 1
  end subroutine insert

  subroutine dump(dl2)
    type(dll), intent(in) :: dl2
    type(node), pointer :: current
    integer :: i

    write(*,fmt='(a,i0,a)',advance='no') 'Doubly-linked list has ',dl2%num_nodes,' element - fwd = '
    current => dl2%head
    i = 1
    write(*,fmt='(i0,a)',advance='no') current%data,', '
    do
       current => current%next
       if (.not. associated(current)) then
          exit
       end if
       i = i + 1
       if (i == dl2%num_nodes) then
          write(*,'(i0)') current%data
       else
          write(*,fmt='(i0,a)',advance='no') current%data,', '
       end if
    end do
  end subroutine dump

  subroutine reverse_dump(dl2)
    type(dll), intent(in) :: dl2
    type(node), pointer :: current
    integer :: i

    write(*,fmt='(a,i0,a)',advance='no') 'Doubly-linked list has ',dl2%num_nodes,' element - bwd = '
    current => dl2%tail
    write(*,fmt='(i0,a)',advance='no') current%data,', '
    i = 1
    do
       current => current%prev
       if (.not. associated(current)) then
          exit
       end if
       i = i + 1
       if (i == dl2%num_nodes) then
          write(*,'(i0)') current%data
       else
          write(*,fmt='(i0,a)',advance='no') current%data,', '
       end if
    end do
  end subroutine reverse_dump

  ! Deallocate all allocated memory
  elemental subroutine tidy(dl2)
    type(dll), intent(inout) :: dl2
    type(node), pointer :: current, last

    current => dl2%head
    do
       last => current
       current => current%next
       if (associated(last)) then
          deallocate(last)
       end if
       if (associated(current, dl2%tail)) then
          deallocate(current)
          exit
       end if
    end do
  end subroutine tidy

  elemental subroutine init(dl2, value)
    type(dll), intent(inout) :: dl2
    integer, intent(in)      :: value
    allocate(dl2%head)
    dl2%tail => dl2%head
    dl2%tail%data = value
    dl2%num_nodes = 1
    return
  end subroutine init

end module dlist

program dl
  use dlist
  implicit none

  type(dll) :: mydll

  mydll = new_dll()
  call append(mydll, 5)
  call append(mydll, 7)
  call prepend(mydll, 3)
  call prepend(mydll, 1)
  call insert(mydll, 3, 4)
  call dump(mydll)

  call reverse_dump(mydll)

  call tidy(mydll)
end program dl

```


```txt

Doubly-linked list has 5 element - fwd = 1, 3, 4, 5, 7
Doubly-linked list has 5 element - bwd = 7, 5, 4, 3, 1

```



## Go

Go has nothing like an enforced invariant.  Responsibility for preventing circular loops must be shared by all code that modifies the list.  Given that, the following declaration ''enables'' code to do that efficiently.

```go
type dlNode struct {
    int
    next, prev *dlNode
}

// Field 'members' allows loops to be prevented.  All nodes
// inserted should be added to members.  Code that operates
// on the list can check any pointer against members to
// find out if the pointer is already in the list.
type dlList struct {
    members map[*dlNode]int
    head, tail **dlNode
}
```

Or, just use the [http://golang.org/pkg/container/list/#Element container/list] package:

```go
package main

import "fmt"
import "container/list"

func main() {
        // Create a new list and put some values in it.
        l := list.New()
        e4 := l.PushBack(4)
        e1 := l.PushFront(1)
        l.InsertBefore(3, e4)
        l.InsertAfter("two", e1)

        // Iterate through list and print its contents.
        for e := l.Front(); e != nil; e = e.Next() {
            fmt.Println(e.Value)
        }
}
```



## Haskell

For an efficient implementation, see the <code>Data.FDList</code> module provided by [http://hackage.haskell.org/package/liboleg liboleg]. But before using doubly linked lists at all, see [http://stackoverflow.com/questions/1844195/doubly-linked-list-in-a-purely-functional-programming-language this discussion on Stack Overflow].


```haskell
import qualified Data.Map as M

type NodeID = Maybe Rational
data Node a = Node
   {vNode :: a,
    pNode, nNode :: NodeID}
type DLList a = M.Map Rational (Node a)

empty = M.empty

singleton a = M.singleton 0 $ Node a Nothing Nothing

fcons :: a -> DLList a -> DLList a
fcons a list | M.null list = singleton a
             | otherwise   = M.insert newid new $
                             M.insert firstid changed list
  where (firstid, Node firstval _ secondid) = M.findMin list
        newid = firstid - 1
        new     = Node a        Nothing      (Just firstid)
        changed = Node firstval (Just newid) secondid

rcons :: a -> DLList a -> DLList a
rcons a list | M.null list = singleton a
             | otherwise   = M.insert lastid changed $
                             M.insert newid new list
  where (lastid, Node lastval penultimateid _) = M.findMax list
        newid = lastid + 1
        changed = Node lastval penultimateid (Just newid)
        new     = Node a       (Just lastid) Nothing

mcons :: a -> Node a -> Node a -> DLList a -> DLList a
mcons a n1 n2 = M.insert n1id left .
    M.insert midid mid . M.insert n2id right
  where Node n1val farleftid   (Just n2id) = n1
        Node n2val (Just n1id) farrightid  = n2
        midid = (n1id + n2id) / 2   -- Hence the use of Rationals.
        mid = Node a (Just n1id) (Just n2id)
        left  = Node n1val farleftid    (Just midid)
        right = Node n2val (Just midid) farrightid

firstNode :: DLList a -> Node a
firstNode = snd . M.findMin

lastNode :: DLList a -> Node a
lastNode = snd . M.findMax

nextNode :: DLList a -> Node a -> Maybe (Node a)
nextNode l n = nNode n >>= flip M.lookup l

prevNode :: DLList a -> Node a -> Maybe (Node a)
prevNode l n = pNode n >>= flip M.lookup l

fromList = foldr fcons empty

toList = map vNode . M.elems
```


An example of use:


```haskell
main = putStrLn $ toList l
  where l = mcons 'M' n1 n2 x
        x = rcons 'Z' $ fcons 'a' $ fcons 'q' $ singleton 'w'
        n1 = firstNode x
        Just n2 = nextNode x n1
```


==Icon and {{header|Unicon}}==

Uses Unicon's classes.

The DoubleList is made from elements of DoubleLink. [[Doubly-Linked List (element)#Icon_and_Unicon]], [[Doubly-Linked List (element insertion)#Icon_and_Unicon]] and [[Doubly-Linked List (traversal)#Icon_and_Unicon]]


```Unicon

class DoubleList (item)

  method head ()
    node := item
    every (node := node.traverse_backwards ()) # move to start of list
    return node
  end

  method tail ()
    node := item
    every (node := node.traverse_forwards ()) # move to end of list
    return node
  end

  method insert_at_head (value)
    head().insert_before (DoubleLink(value))
  end

  method insert_at_tail (value)
    tail().insert_after (DoubleLink (value))
  end

  # insert a node for new_value after that for target_value,
  # i.e. in the middle of the list
  method insert_after (target_value, new_value)
    node := head ()
    every node := head().traverse_forwards () do
      if (node.value = target_value)
        then {
          node.insert_after (DoubleLink (new_value))
         break
        }
  end

  # constructor initiates a list making a node from given value
  initially (value)
    self.item := DoubleLink (value)
end

```


An <code>insert_before</code> method was added to the DoubleLink class:


```Unicon

  # insert given node before this one, losing its existing connections
  method insert_before (node)
    if (\prev_link) then prev_link.next_link := node
    node.prev_link := prev_link
    node.next_link := self
    self.prev_link := node
  end

```


To test the double-linked list:


```Unicon

procedure main ()
  dlist := DoubleList (5)
  every i := 4 to 1 by -1 do
    dlist.insert_at_head (i)
  every i := 6 to 10 do
    dlist.insert_at_tail (i)

  dlist.insert_after (3, 11)

  every node := dlist.head().traverse_forwards () do
    write (node.value)
end

```


```txt

1
2
3
11
4
5
6
7
8
9
10

```



## J


Doubly linked lists are antithetical to J.

First, J already has a built in list data type which is heavily optimized, and micromanaging issues like list traversal bypasses all of that design and architecture.

Second, an implementation of "doubly linked" conflicts with the "once and only once" character of many good implementations.  In a doubly linked list order must be specified redundantly and that redundancy creates maintenance costs which are justified only in rare cases.

So, first, here is a native J list:

   list=: 2 3 5 7 11

To implement a doubly linked list, one could create a list of successor indices and another list of predecessor indices.

First, let us define a different order for our list element, so we can easily show that our doubly linked list is logically distinct from the built in list.  If we use "alphabeted order by names of numbers" we would have the list 11 5 7 3 2

   data=:11 5 7 3 2

 3 is followed by 2
 5 is followed by 7
 7 is followed by 3
 11 is followed by 5

and

 2 is preceded by 3
 3 is preceded by 7
 5 is preceded by 11
 7 is preceded by 5

To represent this in J, we can define additional lists with the successor index and predecessor index for each node:

   successors=:   _ 0 3 1 2
   predecessors=: 1 3 4 2 __

Note that the successor for the end of the list is _ and the successor for the beginning of the list is __

To check for loops, look for repeated indices in either of these ordering lists.  To add an element to the doubly linked list, you would add an element to the data list, and then update the successor and predecessor list by appending to the end the index of the item designated as the successor/predecessor of the new item and replacing the previous holder of that value with the newly valid index.

Finally, note that we can remove elements from the doubly linked list without removing them from the data list.  We might wish to chain removed elements together to facilitate re-use of their positions.  If we want to do this, we will need a place to start:

   garbage=: __

When we delete an item we place the old garbage value as its successor index and we define the garbage variable to be the index we just deleted.  And when adding to the list we first check if garbage has a valid index and if so we take over that position in the structure and update garbage with the previous value of the successor.

Needless to say, this approach is expensive and inefficient. (But, granted, there will be cases where the cost is worth the expense.)

That said, note also that while the native J lists do not support cycles or loops, this high-cost substitute is general enough to support them.


## JavaScript

See [[Doubly-Linked List (element)#JavaScript]], [[Doubly-Linked List (element insertion)#JavaScript]] and [[Doubly-Linked List (traversal)#JavaScript]]



## Julia

Regarding the avoidance or circular loops part of this task, a call to
```julia
show(DLNode)
```
 reveals that Julia considers all of the nodes of doubly linked lists of this kind to contain circular references to their adjacent nodes.

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

function insertpre(postnode, node)
    pred = postnode.pred
    postnode.pred = node
    node.succ = postnode
    node.pred = pred
    if pred != nothing
        pred.succ = node
    end
    node
end

function delete(nd)
    if nd != nothing
        pred = nd.pred
        succ = nd.succ
        if pred != nothing pred.succ = succ end
        if succ != nothing succ.pred = pred end
    end
    nothing
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
node4 = DLNode(4)
insertpost(node1, node2)
insertpre(node2, node4)
insertpost(node2, node3)
println("First value is ", first(node1).value, " and last value is ", last(node1).value)
print("From beginning to end: "); printconnected(node1)
delete(node4)
print("From end to beginning post deletion: "); printconnected(node1, fromtail = true)

```
 {{output}}
```txt

First value is 1 and last value is 3
From beginning to end: 1 -> 4 -> 2 -> 3
From end to beginning post deletion: 3 -> 2 -> 1

```



## Kotlin

Rather than use the java.util.LinkedList<E> class, we will write our own simple LinkedList<E> class for this task:

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
}

fun main(args: Array<String>) {
    val ll = LinkedList<Int>()
    ll.addFirst(1)
    ll.addLast(4)
    ll.insert(ll.first, 2)
    ll.insert(ll.last!!.prev, 3)
    println(ll)
}
```


```txt

1 -> 2 -> 3 -> 4

```



## M2000 Interpreter

M2000 from 9th version has pointers for groups. Also there is IS operator to check if two pointers are the same. There is no Null pointer except ->0 for groups which decrement object reference counter. So here we use a Null class to define an empty group and then we make a global pointer Null to hold that and compare it with Is operator later.

To check that all works we have to use deconstructor ( Remove {}) which call with Clear statement if only one reference exist. So when we remove from list nodes we check it if destroyed, visually.

We can use ? as print.

There is no garbage collector for pointers for groups, except the reference counter. We can use groups without pointers and we place them in containers, and use indexes or keys for those containers as pointers. Using groups with no pointers make each group unique, so there is no circular dependency between two groups. Containers have garbage collector.

Using pointers inside groups make harder to program.

Groups are two kinds in M2000. The named group (or static in a way) and the float group (unnamed). Pointers for groups are also two types, internal, but we handle it as one type. If we get a pointer to a named group, actually we get a weak reference. If this named grouped erased then weak pointer can't work, we get error. If we get a pointer from an expression we get a real pointer (with counting reference). To get a real pointer from a named group is not possible. We can use A->(namedGroup) to get a pointer of a copy of namedGroup. Then we can use namedGroup=Group(A) to merge a copy of A to namedGroup (same members get new values, new members added, unique members of namedGroup stay as is).



```M2000 Interpreter

Module Checkit {
      Form 80, 50
      Class Null {}
      Global Null->Null()
      Class Node {
            group pred, succ
            dat=0
            Remove {
                  Print "destroyed", .dat
            }
            class:
            module Node {
                  .pred->Null
                  .succ->Null
                  if match("N") Then Read .dat
            }
      }
      Class LList {
            Group Head, Tail
            Module PushTail(k as pointer) {
                  if .Tail is Null then {
                        .Head<=k
                        .Tail<=k
                  } else {
                        n=.Tail
                        .Tail<=k
                        k=>pred=n=>pred
                        n=>pred=k
                        k=>succ=n
                  }
            }
            Function RemoveTail {
                  n=.Tail
                  if n is .Head then {
                        .Head->Null
                        .Tail->Null
                  } Else {
                        .Tail<=n=>succ
                        .Tail=>pred=n=>pred
                        n=>pred->Null
                  }
                  for n {
                        .succ->Null
                        .pred->Null
                  }
                  =n
            }
            Module PushHead(k as pointer) {
                  if .head is Null then {
                        .Head<=k
                        .Tail<=k
                  } else {
                        n=.head
                        .head<=k
                        k=>succ=n=>succ
                        n=>succ=k
                        k=>pred=n
                  }
            }
            Function RemoveHead {
                  n=.Head
                  if n is .Tail then {
                        .Head->Null
                        .Tail->Null
                  } Else {
                      .Head<=n=>pred
                      .Head=>succ=n=>succ
                       n=>succ->Null
                   }
                  for n {
                        .succ->Null
                        .pred->Null
                  }
                  =n
            }
            Module RemoveNode(k as pointer) {
                  pred=k=>pred
                  succ=k=>succ
                  if pred is succ then {
                        if .head is k else Error "Can't remove this node"
                        k=.RemoveHead()
                        clear k
                  } else {
                       pred=>succ=succ
                       succ=>pred=pred
                  }
            }
            Module InsertAfter(k as pointer, n as pointer) {
                  pred=k=>pred
                  n=>pred=pred
                  n=>succ=k
                  pred=>succ=n
                  k=>pred=n
            }
            Function IsEmpty {
                  = .Head is null or .tail is null
            }
      class:
            Module LList {
                  .Head->Null
                  .Tail->Null
            }
      }
      m->Node(100)

      L=LList()
      L.PushTail m
      If not L.Head is Null then Print L.Head=>dat=100
      for i=101 to 103 {
            m->Node(i)
            L.PushTail m
            Print "ok....", i
      }
      for i=104 to 106 {
            m->Node(i)
            L.PushHead m
            Print "ok....", i
      }

      Print "Use Head to display from last to first"
      m=L.Head
      do {
            Print m=>dat
            m=m=>pred
      } Until m is null
      Print "ok, now find 3rd and remove it"
      m1=L.Head
      i=1
      Index=3
      While i<Index {
            if m1 is null then exit
            m1=m1=>pred
            i++
      }
      If i<>Index then {
            Print "List has less than "; Index;" Items"
      } Else {
            Print "First add one new node"
                  newNode->Node(1000)
                  L.InsertAfter m1, newNode
                  L.RemoveNode m1
                  clear m1  ' last time m1 used here
                  newNode=Null
            Print "ok.............."
      }
      Print "Use Tail to display from first to last"
      m=L.Tail
      do {
            Print m=>dat
            m=m=>succ
      } Until m is null


      useother=True
      While not L.IsEmpty(){
            For This {
                  \\ we have to use a temporary variable name, here A
                         A=If(useother->L.RemoveTail(),L.RemoveHead())
                         ? A=>dat
                        useother~
                        \\ now we can try to perform removing
                        clear A
             }
      }
      Print "list is empty:"; L.IsEmpty()
}
Checkit

```



## Nim

Nim has a doubly linked list already in the lists module of the standard library.

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
echo l

var l2 = initList[string]()
l2.prepend newNode("hello")
l2.append newNode("world")
echo l2
```

```txt
15 -> 14 -> 12
hello -> world
```

=={{header|Oberon-2}}==

```oberon2

IMPORT Basic;
TYPE
	Node* = POINTER TO NodeDesc;
	NodeDesc* = (* ABSTRACT *) RECORD
		prev-,next-: Node;
	END;

	DLList* = POINTER TO DLListDesc;
	DLListDesc* = RECORD
		first-,last-: Node;
		size-: INTEGER;
	END;

```



## Objeck


```objeck

use Collection;

class Program {
  function : Main(args : String[]) ~ Nil {
    list := List->New();
    list->AddFront("first");
    list->AddBack("last");
    list->Insert("middle");

    list->Forward();
    do {
      list->Get()->As(String)->PrintLine();
      list->Previous();
    }
    while(list->Get() <> Nil);
  }
}
```



## Oforth



```oforth
Object Class new: DNode(value, mutable prev, mutable next)

DNode method: initialize  := next := prev := value ;
DNode method: value  @value ;
DNode method: prev  @prev ;
DNode method: next  @next ;
DNode method: setPrev := prev ;
DNode method: setNext  := next ;
DNode method: <<  @value << ;

DNode method: insertAfter(node)
   node setPrev(self)
   node setNext(@next)
   @next ifNotNull: [ @next setPrev(node) ]
   node := next ;

// Double linked list definition
Collection Class new: DList(mutable head, mutable tail)
DList method: head  @head ;
DList method: tail  @tail ;

DList method: insertFront(v)
| p |
   @head ->p
   DNode new(v, null, p) := head
   p ifNotNull: [ p setPrev(@head) ]
   @tail ifNull: [ @head := tail ] ;

DList method: insertBack(v)
| n |
   @tail ->n
   DNode new(v, n, null) := tail
   n ifNotNull: [ n setNext(@tail) ]
   @head ifNull: [ @tail := head ] ;

DList method: forEachNext
   dup ifNull: [ drop @head ifNull: [ false ] else: [ @head @head true] return ]
   next dup ifNull: [ drop false ] else: [ dup true ] ;

DList method: forEachPrev
   dup ifNull: [ drop @tail ifNull: [ false ] else: [ @tail @tail true] return ]
   prev dup ifNull: [ drop false ] else: [ dup true ] ;

: test      // ( -- aDList )
| dl dn |
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



## Perl 6

This shows a complete example.  (Other entries in the section focus on aspects of this solution.)

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
	$!prev.next = $!next;	# conveniently returns next element
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

class DLElem_Int does DLElem[Int] {}
class DLList_Int does DLList[DLElem_Int] {}

my $dll = DLList_Int.new;

$dll.first.post-insert(1).post-insert(2).post-insert(3);
$dll.first.post-insert(0);

$dll.last.pre-insert(41).pre-insert(40).prev.delete;  # (deletes 3)
$dll.last.pre-insert(42);

say $dll.list;     # 0 1 2 40 41 42
say $dll.reverse;  # 42 41 40 2 1 0
```

```txt
0 1 2 40 41 42
42 41 40 2 1 0
```



## Phix

See [[Doubly-linked_list/Traversal#Phix|Doubly-linked_list/Traversal]] for a complete example.


## PicoLisp

For the list of double-cell structures described in
[[Doubly-linked list/Element definition#PicoLisp]],
we define a header structure, containing one pointer to the start
and one to the end of the list.

            +------------> start
            |
         +--+--+-----+
         |  |  |  ---+---> end
         +-----+-----+

```PicoLisp
# Build a doubly-linked list
(de 2list @
   (let Prev NIL
      (let L
         (make
            (while (args)
               (setq Prev (chain (list (next) Prev))) ) )
         (cons L Prev) ) ) )

(setq *DLst (2list 'was 'it 'a 'cat 'I 'saw))
```

For output of the example data, see [[Doubly-linked list/Traversal#PicoLisp]].


## PL/I


```PL/I

define structure
   1 Node,
      2 value        fixed decimal,
      2 back_pointer handle(Node),
      2 fwd_pointer  handle(Node);

```



## PowerShell

Create and populate the list:

```PowerShell

$list = New-Object -TypeName 'Collections.Generic.LinkedList[PSCustomObject]'

for($i=1; $i -lt 10; $i++)
{
   $list.AddLast([PSCustomObject]@{ID=$i; X=100+$i;Y=200+$i}) | Out-Null
}

$list

```

```txt

ID   X   Y
--   -   -
 1 101 201
 2 102 202
 3 103 203
 4 104 204
 5 105 205
 6 106 206
 7 107 207
 8 108 208
 9 109 209

```

Insert a value at the head:

```PowerShell

$list.AddFirst([PSCustomObject]@{ID=123; X=123;Y=123}) | Out-Null

$list

```

```txt

 ID   X   Y
 --   -   -
123 123 123
  1 101 201
  2 102 202
  3 103 203
  4 104 204
  5 105 205
  6 106 206
  7 107 207
  8 108 208
  9 109 209

```

Insert a value in the middle:

```PowerShell

$current = $list.First

while(-not ($current -eq $null))
{
   If($current.Value.X -eq 105)
   {
       $list.AddAfter($current, [PSCustomObject]@{ID=345;X=345;Y=345}) | Out-Null
       break
   }

   $current = $current.Next
}

$list

```

```txt

 ID   X   Y
 --   -   -
123 123 123
  1 101 201
  2 102 202
  3 103 203
  4 104 204
  5 105 205
345 345 345
  6 106 206
  7 107 207
  8 108 208
  9 109 209

```

Insert a value at the end:

```PowerShell

$list.AddLast([PSCustomObject]@{ID=789; X=789;Y=789}) | Out-Null

$list

```

```txt

 ID   X   Y
 --   -   -
123 123 123
  1 101 201
  2 102 202
  3 103 203
  4 104 204
  5 105 205
345 345 345
  6 106 206
  7 107 207
  8 108 208
  9 109 209
789 789 789

```



## PureBasic


```PureBasic
DataSection
  ;the list of words that will be added to the list
  words:
  Data.s "One", "Two", "Three", "Four", "Five", "Six", "EndOfData"
EndDataSection


Procedure displayList(List x.s(), title$)
  ;display all elements from list of strings
  Print(title$)
  ForEach x()
    Print(x() + " ")
  Next
  PrintN("")
EndProcedure


OpenConsole()

NewList a.s() ;create a new list of strings

;add words to the head of list
Restore words
Repeat
  Read.s a$
  If a$ <> "EndOfData"
    ResetList(a()) ;Move to head of list
    AddElement(a())
    a() = a$
  EndIf
Until a$ = "EndOfData"
displayList(a(),"Insertion at Head: ")


ClearList(a())
;add words to the tail of list
Restore words
LastElement(a()) ;Move to the tail of the list
Repeat
  Read.s a$
  If a$ <> "EndOfData"
    AddElement(a()) ;after insertion the new position is still at the tail
    a() = a$
  EndIf
Until a$ = "EndOfData"
displayList(a(),"Insertion at Tail: ")


ClearList(a())
;add words to the middle of list
Restore words
ResetList(a()) ;Move to the tail of the list
Repeat
  Read.s a$
  If a$ <> "EndOfData"
    c = CountList(a())
    If c > 1
      SelectElement(a(),Random(c - 2)) ;insert after a random element but before tail
    Else
      FirstElement(a())
    EndIf
    AddElement(a())
    a() = a$
  EndIf
Until a$ = "EndOfData"
displayList(a(),"Insertion in Middle: ")

Repeat: Until Inkey() <> ""
```

```txt

Insertion at Head: Six Five Four Three Two One
Insertion at Tail: One Two Three Four Five Six
Insertion at Middle: One Five Six Three Four Two

```



## Python

In the high level language Python, its <code>list</code> native datatype should be used. It automatically preserves the integrity of the list w.r.t. loops and allows insertion at any point using [http://docs.python.org/library/stdtypes.html#typesseq-mutable list.insert()] via an integer index into the list rather than a machine-code level pointer to a list element.


## Racket

The following is a port of the Common Lisp solution. The ouput is '(1 2 3 4).


```racket

#lang racket
(define-struct dlist (head tail) #:mutable #:transparent)
(define-struct dlink (content prev next) #:mutable #:transparent)

(define (insert-between dlist before after data)
  ; Insert a fresh link containing DATA after existing link
  ; BEFORE if not nil and before existing link AFTER if not nil
  (define new-link (make-dlink data before after))
  (if before
      (set-dlink-next! before new-link)
      (set-dlist-head! dlist new-link))
  (if after
      (set-dlink-prev! after new-link)
      (set-dlist-tail! dlist new-link))
    new-link)

(define (insert-before dlist dlink data)
  ; Insert a fresh link containing DATA before existing link DLINK
  (insert-between dlist (dlink-prev dlink) dlink data))

(define (insert-after dlist dlink data)
  ; Insert a fresh link containing DATA after existing link DLINK
  (insert-between dlist dlink (dlink-next dlink) data))

(define (insert-head dlist data)
  ; Insert a fresh link containing DATA at the head of DLIST
  (insert-between dlist #f (dlist-head dlist) data))

(define (insert-tail dlist data)
  ; Insert a fresh link containing DATA at the tail of DLIST
  (insert-between dlist (dlist-tail dlist) #f data))

(define (remove-link dlist dlink)
  ; Remove link DLINK from DLIST and return its content
  (let ((before (dlink-prev dlink))
        (after (dlink-next dlink)))
    (if before
        (set-dlink-next! before after)
        (set-dlist-head! dlist after))
    (if after
        (set-dlink-prev! after before)
        (set-dlist-tail! dlist before))))

(define (dlist-elements dlist)
  ; Returns the elements of DLIST as a list
  (define (extract-values dlink acc)
    (if dlink
        (extract-values (dlink-next dlink) (cons (dlink-content dlink) acc))
        acc))
  (reverse (extract-values (dlist-head dlist) '())))

(let ((dlist (make-dlist #f #f)))
  (insert-head dlist 1)
  (insert-tail dlist 4)
  (insert-after dlist (dlist-head dlist) 2)
  (let* ((next-to-last (insert-before dlist (dlist-tail dlist) 3))
         (bad-link (insert-before dlist next-to-last 42)))
    (remove-link dlist bad-link))
  (dlist-elements dlist))

```




## REXX


```txt

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

```

REXX doesn't have linked lists, as there are no pointers (or handles).

However, linked lists can be simulated with lists in REXX.

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

                               ─── showing 5th & 6th items ───
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



## Ring


```ring

# Project : Doubly-linked list/Definition

test = [1, 5, 7, 0, 3, 2]
insert(test, 0, 9)
insert(test, len(test), 4)
item = len(test)/2
insert(test, item, 6)
showarray(test)

func showarray(vect)
        svect = ""
        for n = 1 to len(vect)
              svect = svect + vect[n] + " "
        next
        svect = left(svect, len(svect) - 1)
        see svect

```

Output:

```txt

9 1 5 7 6 0 3 2 4

```



## Ruby

See [[Doubly-Linked List (element)#Ruby]], [[Doubly-Linked List (element insertion)#Ruby]] and [[Doubly-Linked List (traversal)#Ruby]]


## Tcl

This task was earlier marked as unfeasible for Tcl.
Tcl lists are compact arrays of pointers to values.
However, on very long lists, insertions and deletions
(if not at end) may require copying a large amount of data.
In such cases, the implementation below may be helpful.
It provides a single ''dl'' command, which is called
with the name of a DList, a method name, and possibly
more arguments as required. The testcases below should
give a good idea.
The ''asList'' and ''asList2'' methods demonstrate
forward and backward traversal.

See also [[Doubly-Linked List (element)]] for a TclOO-based version.


```Tcl
package require Tcl 8.4
proc dl {_name cmd {where error} {value ""}} {
    upvar 1 $_name N
    switch -- $cmd {
        insert {
            if ![info exists N()] {set N() {"" "" 0}}
            set id [lindex $N() 2]
            lset N() 2 [incr id]
            switch -- $where {
                head {
                    set prev {}
                    set next [lindex $N() 0]
                    lset N() 0 $id
                }
                end {
                    set prev [lindex $N() 1]
                    set next {}
                    lset N() 1 $id
                }
                default {
                    set prev $where
                    set next [lindex $N($where) 1]
                    lset N($where) 1 $id
                }
            }
            if {$prev ne ""} {lset N($prev) 1 $id}
            if {$next ne ""} {lset N($next) 0 $id}
            if {[lindex $N() 1] eq ""} {lset N() 1 $id}
            set N($id) [list $prev $next $value]
            return $id
        }
        delete {
            set i $where
            if {$where eq "head"} {set i [dl N head]}
            if {$where eq "end"}  {set i [dl N end]}
            foreach {prev next} $N($i) break
            if {$prev ne ""} {lset N($prev) 1 $next}
            if {$next ne ""} {lset N($next) 0 $prev}
            if {[dl N head] == $i} {lset N() 0 $next}
            if {[dl N end] == $i}  {lset N() 1 $prev}
            unset N($i)
        }
        findfrom {
            if {$where eq "head"} {set where [dl N head]}
            for {set i $where} {$i ne ""} {set i [dl N next $i]} {
                if {[dl N get $i] eq $value} {return $i}
            }
        }
        get    {lindex $N($where) 2}
        set    {lset   N($where) 2 $value; set value}
        head   {lindex $N() 0}
        end    {lindex $N() 1}
        next   {lindex $N($where) 1}
        prev   {lindex $N($where) 0}
        length {expr {[array size N]-1}}
        asList {
            set res {}
            for {set i [dl N head]} {$i ne ""} {set i [dl N next $i]} {
                lappend res [dl N get $i]
            }
            return $res
        }
        asList2 {
            set res {}
            for {set i [dl N end]} {$i ne ""} {set i [dl N prev $i]} {
                lappend res [dl N get $i]
            }
            return $res
        }
    }
}
```


```tcl
# Testing code
set testcases [split {
    dl D insert head foo
    dl D insert end  bar
    dl D insert head hello
    dl D set [dl D head] hi
    dl D insert end  grill
    set i [dl D findfrom head bar]
    dl D set    $i BAR
    dl D insert $i and
    dl D length
    dl D asList2
    dl D delete $i
    dl D findfrom head nix
    dl D delete head
    dl D delete end
    dl D delete end
    dl D delete head
    dl D length
} \n]
foreach case $testcases {
    if {[string trim $case] ne ""} {
        puts " $case -> [eval $case] : [dl D asList]"
        if {[lsearch $argv -p] >= 0} {parray D}
    }
}
```



## Visual Basic .NET



```vbnet
Public Class DoubleLinkList(Of T)
   Private m_Head As Node(Of T)
   Private m_Tail As Node(Of T)

   Public Sub AddHead(ByVal value As T)
       Dim node As New Node(Of T)(Me, value)

       If m_Head Is Nothing Then
           m_Head = Node
           m_Tail = m_Head
       Else
           node.Next = m_Head
           m_Head = node
       End If

   End Sub

   Public Sub AddTail(ByVal value As T)
       Dim node As New Node(Of T)(Me, value)

       If m_Tail Is Nothing Then
           m_Head = node
           m_Tail = m_Head
       Else
           node.Previous = m_Tail
           m_Tail = node
       End If
   End Sub

   Public ReadOnly Property Head() As Node(Of T)
       Get
           Return m_Head
       End Get
   End Property

   Public ReadOnly Property Tail() As Node(Of T)
       Get
           Return m_Tail
       End Get
   End Property

   Public Sub RemoveTail()
       If m_Tail Is Nothing Then Return

       If m_Tail.Previous Is Nothing Then 'empty
           m_Head = Nothing
           m_Tail = Nothing
       Else
           m_Tail = m_Tail.Previous
           m_Tail.Next = Nothing
       End If
   End Sub

   Public Sub RemoveHead()
       If m_Head Is Nothing Then Return

       If m_Head.Next Is Nothing Then 'empty
           m_Head = Nothing
           m_Tail = Nothing
       Else
           m_Head = m_Head.Next
           m_Head.Previous = Nothing
       End If
   End Sub

End Class

Public Class Node(Of T)
   Private ReadOnly m_Value As T
   Private m_Next As Node(Of T)
   Private m_Previous As Node(Of T)
   Private ReadOnly m_Parent As DoubleLinkList(Of T)

   Public Sub New(ByVal parent As DoubleLinkList(Of T), ByVal value As T)
       m_Parent = parent
       m_Value = value
   End Sub

   Public Property [Next]() As Node(Of T)
       Get
           Return m_Next
       End Get
       Friend Set(ByVal value As Node(Of T))
           m_Next = value
       End Set
   End Property

   Public Property Previous() As Node(Of T)
       Get
           Return m_Previous
       End Get
       Friend Set(ByVal value As Node(Of T))
           m_Previous = value
       End Set
   End Property

   Public ReadOnly Property Value() As T
       Get
           Return m_Value
       End Get
   End Property

   Public Sub InsertAfter(ByVal value As T)
       If m_Next Is Nothing Then
           m_Parent.AddTail(value)
       ElseIf m_Previous Is Nothing Then
           m_Parent.AddHead(value)
       Else
           Dim node As New Node(Of T)(m_Parent, value)
           node.Previous = Me
           node.Next = Me.Next
           Me.Next.Previous = node
           Me.Next = node
       End If
   End Sub

   Public Sub Remove()
       If m_Next Is Nothing Then
           m_Parent.RemoveTail()
       ElseIf m_Previous Is Nothing Then
           m_Parent.RemoveHead()
       Else
           m_Previous.Next = Me.Next
           m_Next.Previous = Me.Previous
       End If
   End Sub

End Class
```



## zkl


```zkl
class Node{
   fcn init(_value,_prev=Void,_next=Void)
      { var value=_value, prev=_prev, next=_next; }
   fcn toString{ value.toString() }
   fcn append(value){  // loops not allowed: create a new Node
      b,c := Node(value,self,next),next;
      next=b;
      if(c) c.prev=b;
      b
   }
   fcn delete{
      if(prev) prev.next=next;
      if(next) next.prev=prev;
      self
   }
   fcn last  { n,p := self,self; while(n){ p,n = n,n.next } p }
   fcn first { n,p := self,self; while(n){ p,n = n,n.prev } p }
   fcn walker(forward=True){
      dir:=forward and "next" or "prev";
      Walker(fcn(rn,dir){
         if(not (n:=rn.value)) return(Void.Stop);
	 rn.set(n.setVar(dir));
         n.value;
      }.fp(Ref(self),dir))
   }
}
```


```zkl
a:=Node("a");
a.append("c").append("d");
a.last().append("e");
a.last().first().append("b");
foreach n in (a){ print(n,"  ") } println();
foreach n in (a.last().walker(False)){ print(n,"  ") } println();
```

```txt

a  b  c  d  e
e  d  c  b  a

```



{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have user-defined data structures or objects. -->
