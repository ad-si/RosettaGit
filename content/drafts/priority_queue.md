+++
title = "Priority queue"
description = ""
date = 2019-07-30T16:46:40Z
aliases = []
[extra]
id = 10233
[taxonomies]
categories = []
tags = []
+++

{{task}}
A [[wp:Priority queue|priority queue]] is somewhat similar to a [[Queue|queue]], with an important distinction: each item is added to a priority queue with a priority level, and will be later removed from the queue with the highest priority element first. That is, the items are (conceptually) stored in the queue in priority order instead of in insertion order.

'''Task:''' Create a priority queue.  The queue must support at least two operations:
# Insertion.  An element is added to the queue with a priority (a numeric value).
# Top item removal.  Deletes the element or one of the elements with the current top priority and return it.

Optionally, other operations may be defined, such as peeking (find what current top priority/top element is), merging (combining two priority queues into one), etc.

To test your implementation, insert a number of elements into the queue, each with some random priority.  Then dequeue them sequentially; now the elements should be sorted by priority.  You can use the following task/priority items as input data:
 '''Priority'''    '''Task'''
   3        Clear drains
   4        Feed cat
   5        Make tea
   1        Solve RC tasks
   2        Tax return

The implementation should try to be efficient.  A typical implementation has O(log n) insertion and extraction time, where n is the number of items in the queue.  You may choose to impose certain limits such as small range of allowed priority levels, limited capacity, etc.  If so, discuss the reasons behind it.


## Ada

{{works with|Ada 2012}}
Ada 2012 includes container classes for priority queues.


```Ada
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;
with Ada.Strings.Unbounded;

procedure Priority_Queues is
   use Ada.Containers;
   use Ada.Strings.Unbounded;
   type Queue_Element is record
      Priority : Natural;
      Content  : Unbounded_String;
   end record;
   function Get_Priority (Element : Queue_Element) return Natural is
   begin
      return Element.Priority;
   end Get_Priority;
   function Before (Left, Right : Natural) return Boolean is
   begin
      return Left > Right;
   end Before;
   package String_Queues is new Synchronized_Queue_Interfaces
     (Element_Type => Queue_Element);
   package String_Priority_Queues is new Unbounded_Priority_Queues
     (Queue_Interfaces => String_Queues,
      Queue_Priority => Natural);

   My_Queue : String_Priority_Queues.Queue;
begin
   My_Queue.Enqueue (New_Item => (Priority => 3, Content => To_Unbounded_String ("Clear drains")));
   My_Queue.Enqueue (New_Item => (Priority => 4, Content => To_Unbounded_String ("Feed cat")));
   My_Queue.Enqueue (New_Item => (Priority => 5, Content => To_Unbounded_String ("Make tea")));
   My_Queue.Enqueue (New_Item => (Priority => 1, Content => To_Unbounded_String ("Solve RC tasks")));
   My_Queue.Enqueue (New_Item => (Priority => 2, Content => To_Unbounded_String ("Tax return")));

   declare
      Element : Queue_Element;
   begin
      while My_Queue.Current_Use > 0 loop
         My_Queue.Dequeue (Element => Element);
         Ada.Text_IO.Put_Line (Natural'Image (Element.Priority) & " => " & To_String (Element.Content));
      end loop;
   end;
end Priority_Queues;
```


{{out}}

```txt

```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program priorqueue.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

.equ  NBMAXIELEMENTS,    100

/*******************************************/
/* Structures                               */
/********************************************/
/* example structure  item  */
    .struct  0
item_priority:                     @ priority
    .struct  item_priority + 4
item_address:                      @ string address
    .struct  item_address + 4
item_fin:
/* example structure heap  */
    .struct  0
heap_size:                         @ heap size
    .struct  heap_size + 4
heap_items:                        @ structure of items
    .struct  heap_items + (item_fin * NBMAXIELEMENTS)
heap_fin:


/*********************************/
/* Initialized data              */
/*********************************/
.data
szMessEmpty:       .asciz "Empty queue. \n"
szMessNotEmpty:    .asciz "Not empty queue. \n"
szMessError:       .asciz "Error detected !!!!. \n"
szMessResult:      .ascii "Priority : "                    @ message result
sMessPriority:        .fill 11, 1, ' '
                   .asciz " : "

szString1:         .asciz "Clear drains"
szString2:         .asciz "Feed cat"
szString3:         .asciz "Make tea"
szString4:         .asciz "Solve RC tasks"
szString5:         .asciz "Tax return"
szCarriageReturn:  .asciz "\n"
/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
.align 4
Queue1:                .skip heap_fin      @ queue memory place
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                       @ entry of program
    ldr r0,iAdrQueue1                       @ queue structure address
    bl isEmpty
    cmp r0,#0
    beq 1f
    ldr r0,iAdrszMessEmpty
    bl affichageMess                        @ display message empty
    b 2f
1:
    ldr r0,iAdrszMessNotEmpty
    bl affichageMess                        @ display message not empty
2:
    @ init item 1
    ldr r0,iAdrQueue1                       @ queue structure address
    mov r1,#3                               @ priority
    ldr r2,iAdrszString1
    bl pushQueue                            @ add item in queue
    cmp r0,#-1                              @ error ?
    beq 99f

    ldr r0,iAdrQueue1                       @ queue structure address
    bl isEmpty
    cmp r0,#0                               @ not empty
    beq 3f
    ldr r0,iAdrszMessEmpty
    bl affichageMess                        @ display message empty
    b 4f
3:
    ldr r0,iAdrszMessNotEmpty
    bl affichageMess                        @ display message not empty

4:
    @ init item 2
    ldr r0,iAdrQueue1                       @ queue structure address
    mov r1,#4                               @ priority
    ldr r2,iAdrszString2
    bl pushQueue                            @ add item in queue
    cmp r0,#-1                              @ error ?
    beq 99f
    @ init item 3
    ldr r0,iAdrQueue1                       @ queue structure address
    mov r1,#5                               @ priority
    ldr r2,iAdrszString3
    bl pushQueue                            @ add item in queue
    cmp r0,#-1                              @ error ?
    beq 99f
    @ init item 4
    ldr r0,iAdrQueue1                       @ queue structure address
    mov r1,#1                               @ priority
    ldr r2,iAdrszString4
    bl pushQueue                            @ add item in queue
    cmp r0,#-1                              @ error ?
    beq 99f
    @ init item 5
    ldr r0,iAdrQueue1                       @ queue structure address
    mov r1,#2                               @ priority
    ldr r2,iAdrszString5
    bl pushQueue                            @ add item in queue
    cmp r0,#-1                              @ error ?
    beq 99f
5:
    ldr r0,iAdrQueue1                       @ queue structure address
    bl popQueue                             @ return item
    cmp r0,#-1                              @ end ?
    beq 100f
    mov r2,r1                               @ save string address
    ldr r1,iAdrsMessPriority                @ conversion priority
    bl conversion10                         @ decimal conversion
    ldr r0,iAdrszMessResult
    bl affichageMess                        @ display message
    mov r0,r2                               @ string address
    bl affichageMess                        @ display message
    ldr r0,iAdrszCarriageReturn
    bl affichageMess

    b 5b                                    @ loop
99:
    @ error
    ldr r0,iAdrszMessError
    bl affichageMess
100:                                        @ standard end of the program
    mov r0, #0                              @ return code
    mov r7, #EXIT                           @ request to exit program
    svc #0                                  @ perform the system call

iAdrQueue1:               .int Queue1
iAdrszString1:            .int szString1
iAdrszString2:            .int szString2
iAdrszString3:            .int szString3
iAdrszString4:            .int szString4
iAdrszString5:            .int szString5
iAdrszMessError:          .int szMessError
iAdrszMessEmpty:          .int szMessEmpty
iAdrszMessNotEmpty:       .int szMessNotEmpty
iAdrszMessResult:         .int szMessResult
iAdrszCarriageReturn:     .int szCarriageReturn
iAdrsMessPriority:        .int sMessPriority

/******************************************************************/
/*     test if queue empty                                        */
/******************************************************************/
/* r0 contains the address of queue structure */
isEmpty:
    push {r1,lr}                            @ save  registres
    ldr r1,[r0,#heap_size]                  @ heap size
    cmp r1,#0
    moveq r0,#1                             @ empty queue
    movne r0,#0                             @ not empty
    pop {r1,lr}                             @ restaur registers
    bx lr                                   @ return
/******************************************************************/
/*     add item  in queue                                         */
/******************************************************************/
/* r0 contains the address of queue structure */
/* r1 contains the priority of item           */
/* r2 contains the string address             */
pushQueue:
    push {r1-r9,lr}                         @ save  registres
    ldr r3,[r0,#heap_size]                  @ heap size
    cmp r3,#0                               @ heap empty ?
    bne 1f
    add r4,r0,#heap_items                   @ address of item structure
    str r1,[r4,#item_priority]              @ store in first item
    str r2,[r4,#item_address]
    mov r3,#1                               @ heap size
    str r3,[r0,#heap_size]                  @ new heap size
    b 100f
1:
    mov r4,r3                               @ maxi index
    lsr r5,r4,#1                            @ current index = maxi / 2
    mov r8,r1                               @ save priority
    mov r9,r2                               @ save string address
2:                                          @ insertion loop
    cmp r4,#0                               @ end loop ?
    ble 3f
    mov r6,#item_fin                        @ item size
    mul r6,r5,r6                            @ item shift
    add r6,r0
    add r6,#heap_items                      @ compute address item
    ldr r7,[r6,#item_priority]              @ load priority
    cmp r7,r8                               @ compare priority
    ble 3f                                  @ <=  end loop
    mov r1,r4                               @ last index
    mov r2,r5                               @ current index
    bl exchange
    mov r4,r5                               @ last index = current index
    lsr r5,#1                               @ current index / 2
    b 2b
3:                                          @ store item at last index find
    mov r6,#item_fin                        @ item size
    mul r6,r4,r6                            @ item shift
    add r6,r0
    add r6,#heap_items                      @ item address
    str r8,[r6,#item_priority]
    str r9,[r6,#item_address]
    add r3,#1                               @ increment heap size
    cmp r3,#NBMAXIELEMENTS                  @ maxi ?
    movge r0,#-1                            @ yes -> error
    bge 100f
    str r3,[r0,#heap_size]                  @ store new size
100:
    pop {r1-r9,lr}                          @ restaur registers
    bx lr                                   @ return
/******************************************************************/
/*     swap two elements of table                                  */
/******************************************************************/
/* r0 contains the address of table */
/* r1 contains the first index */
/* r2 contains the second index */
exchange:
    push {r3-r6,lr}                         @ save registers
    add r5,r0,#heap_items                   @ address items begin
    mov r3,#item_fin                        @ item size
    mul r4,r1,r3                            @ compute item 1 shift
    add r4,r5                               @ compute item 1 address
    mul r6,r2,r3                            @ compute item 2 shift
    add r6,r5                               @ compute item 2 address
    ldr r5,[r4,#item_priority]              @ exchange
    ldr r3,[r6,#item_priority]
    str r3,[r4,#item_priority]
    str r5,[r6,#item_priority]
    ldr r5,[r4,#item_address]
    ldr r3,[r6,#item_address]
    str r5,[r6,#item_address]
    str r3,[r4,#item_address]

100:
    pop {r3-r6,lr}
    bx lr                                              @ return
/******************************************************************/
/*     move one element of table                                  */
/******************************************************************/
/* r0 contains the address of table */
/* r1 contains the origin index */
/* r2 contains the destination index */
moveItem:
    push {r3-r6,lr}                         @ save registers
    add r5,r0,#heap_items                   @ address items begin
    mov r3,#item_fin                        @ item size
    mul r4,r1,r3                            @ compute item 1 shift
    add r4,r5                               @ compute item 1 address
    mul r6,r2,r3                            @ compute item 2 shift
    add r6,r5                               @ compute item 2 address
    ldr r5,[r4,#item_priority]              @ exchange
    str r5,[r6,#item_priority]
    ldr r5,[r4,#item_address]
    str r5,[r6,#item_address]

100:
    pop {r3-r6,lr}
    bx lr                                   @ return


/******************************************************************/
/*     pop queue                                                  */
/******************************************************************/
/* r0 contains the address of queue structure */
/* r0 return priority        */
/* r1 return string address   */
popQueue:
    push {r2-r10,lr}                        @ save  registres
    mov r1,r0                               @ save address queue
    bl isEmpty                              @ control if empty queue
    cmp r0,#1                               @ yes -> error
    moveq r0,#-1
    beq 100f
    @ save données à retourner
    mov r0,r1                               @ restaur address queue
    add r4,r0,#heap_items                   @ address of item structure
    ldr r8,[r4,#item_priority]              @ save priority first item
    ldr r9,[r4,#item_address]               @ save address string first item
    ldr r3,[r0,#heap_size]                  @ heap size
    sub r7,r3,#1                            @ last item
    mov r1,r7
    mov r2,#0                               @ first item
    bl moveItem                             @ move last item in first item

    cmp r7,#1                               @ one only item ?
    beq 10f                                 @ yes -> end
    mov r4,#0                               @ first  index
1:
    cmp r4,r7                               @ = last index
    bge 10f                                 @ yes -> end
    mov r5,r7                               @ last index
    cmp r4,#0                               @ init current index
    moveq r6,#1                             @ = 1
    lslne r6,r4,#1                          @ else = first index * 2
    cmp r6,r7                               @ current index > last index
    bgt 2f                                  @ yes
                                            @ no compar priority current item last item
    mov r1,#item_fin
    mul r1,r6,r1
    add r1,r0
    add r1,#heap_items                      @ address of current item structure
    ldr r1,[r1,#item_priority]
    mov r10,#item_fin
    mul r10,r5,r10
    add r10,r0
    add r10,#heap_items                     @ address of last item structure
    ldr r10,[r10,#item_priority]
    cmp r1,r10
    movlt r5,r6
2:
    add r10,r6,#1                           @ increment current index
    cmp r10,r7                              @ end ?
    bgt 3f                                  @ yes
    mov r1,#item_fin                        @ no compare priority
    mul r1,r10,r1
    add r1,r0
    add r1,#heap_items                     @ address of item structure
    ldr r1,[r1,#item_priority]
    mov r2,#item_fin
    mul r2,r5,r2
    add r2,r0
    add r2,#heap_items                     @ address of item structure
    ldr r2,[r2,#item_priority]
    cmp r1,r2
    movlt r5,r10
3:
    mov r1,r5                              @ move item
    mov r2,r4
    bl moveItem
    mov r4,r5
    b 1b                                   @ and loop
10:
    sub r3,#1
    str r3,[r0,#heap_size]                 @ new heap size
    mov r0,r8                              @ return priority
    mov r1,r9                              @ return string address
100:
    pop {r2-r10,lr}                        @ restaur registers
    bx lr                                  @ return
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                   @ save  registres
    mov r2,#0                               @ counter length
1:                                          @ loop length calculation
    ldrb r1,[r0,r2]                         @ read octet start position + index
    cmp r1,#0                               @ if 0 its over
    addne r2,r2,#1                          @ else add 1 in the length
    bne 1b                                  @ and loop
                                            @ so here r2 contains the length of the message
    mov r1,r0                               @ address message in r1
    mov r0,#STDOUT                          @ code to write to the standard output Linux
    mov r7, #WRITE                          @ code call system "write"
    svc #0                                  @ call systeme
    pop {r0,r1,r2,r7,lr}                    @ restaur registers */
    bx lr                                   @ return
/******************************************************************/
/*     Converting a register to a decimal                                 */
/******************************************************************/
/* r0 contains value and r1 address area   */
.equ LGZONECAL,   10
conversion10:
    push {r1-r4,lr}                         @ save registers
    mov r3,r1
    mov r2,#LGZONECAL
1:                                          @ start loop
    bl divisionpar10                        @ r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                              @ digit
    strb r1,[r3,r2]                         @ store digit on area
    cmp r0,#0                               @ stop if quotient = 0
    subne r2,#1                               @ previous position
    bne 1b                                  @ else loop
                                            @ end replaces digit in front of area
    mov r4,#0
2:
    ldrb r1,[r3,r2]
    strb r1,[r3,r4]                         @ store in area begin
    add r4,#1
    add r2,#1                               @ previous position
    cmp r2,#LGZONECAL                       @ end
    ble 2b                                  @ loop
    mov r1,#' '
3:
    strb r1,[r3,r4]
    add r4,#1
    cmp r4,#LGZONECAL                       @ end
    ble 3b
100:
    pop {r1-r4,lr}                          @ restaur registres
    bx lr                                   @return
/***************************************************/
/*   division par 10   signé                       */
/* Thanks to http://thinkingeek.com/arm-assembler-raspberry-pi/*
/* and   http://www.hackersdelight.org/            */
/***************************************************/
/* r0 dividende   */
/* r0 quotient */
/* r1 remainder  */
divisionpar10:
  /* r0 contains the argument to be divided by 10 */
    push {r2-r4}                           @ save registers  */
    mov r4,r0
    mov r3,#0x6667                         @ r3 <- magic_number  lower
    movt r3,#0x6666                        @ r3 <- magic_number  upper
    smull r1, r2, r3, r0                   @ r1 <- Lower32Bits(r1*r0). r2 <- Upper32Bits(r1*r0)
    mov r2, r2, ASR #2                     @ r2 <- r2 >> 2
    mov r1, r0, LSR #31                    @ r1 <- r0 >> 31
    add r0, r2, r1                         @ r0 <- r2 + r1
    add r2,r0,r0, lsl #2                   @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1                   @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2-r4}
    bx lr                                  @ return

```

{{out}}

```txt

Empty queue.
Not empty queue.
Priority : 1           : Solve RC tasks
Priority : 2           : Tax return
Priority : 3           : Clear drains
Priority : 4           : Feed cat
Priority : 5           : Make tea

```


## AutoHotkey


```AutoHotkey
;-----------------------------------
PQ_TopItem(Queue,Task:=""){					; remove and return top priority item
	TopPriority := PQ_TopPriority(Queue)
	for T, P in Queue
		if (P = TopPriority) && ((T=Task)||!Task)
			return T , Queue.Remove(T)
	return 0
}
;-----------------------------------
PQ_AddTask(Queue,Task,Priority){				; insert and return new task
	for T, P in Queue
		if (T=Task) || !(Priority && Task)
			return 0
	return Task,	Queue[Task] := Priority
}
;-----------------------------------
PQ_DelTask(Queue, Task){					; delete and return task
	for T, P in Queue
		if (T = Task)
			return Task,	Queue.Remove(Task)
}
;-----------------------------------
PQ_Peek(Queue){							; peek and return top priority task(s)
	TopPriority := PQ_TopPriority(Queue)
	for T, P in Queue
		if (P = TopPriority)
			PeekList .= (PeekList?"`n":"") "`t" T
	return PeekList
}
;-----------------------------------
PQ_Check(Queue,Task){						; check task and return its priority
	for T, P in Queue
		if (T = Task)
			return P
	return 0
}
;-----------------------------------
PQ_Edit(Queue,Task,Priority){					; Update task priority and return its new priority
	for T, P in Queue
		if (T = Task)
			return Priority,	Queue[T]:=Priority
	return 0
}
;-----------------------------------
PQ_View(Queue){							; view current Queue
	for T, P in Queue
		Res .= P " : " T "`n"
	Sort, Res, FMySort
	return "Priority Queue=`n" Res
}
MySort(a,b){
	RegExMatch(a,"(\d+) : (.*)", x), RegExMatch(b,"(\d+) : (.*)", y)
	return x1>y1?1:x1<y1?-1: x2>y2?1:x2<y2?-1: 0
}
;-----------------------------------
PQ_TopPriority(Queue){						; return queue's top priority
	for T, P in Queue
		TopPriority := TopPriority?TopPriority:P	, TopPriority := TopPriority<P?TopPriority:P
	return, TopPriority
}
```

Examples:
```AutoHotkey
data =
(
3	Clear drains
1	test
4	Feed cat
5	Make tea
1	Solve RC tasks
2	Tax return
)
PQ:=[] 								; Create Priority Queue PQ[Task, Priority]
loop, parse, data, `n, `r
	F:= StrSplit(A_LoopField, "`t")	, PQ[F[2]] := F[1]
PQ_View(PQ)
MsgBox, 262208,, % "Top Priority item(s)=`n" 			PQ_Peek(PQ)	"`n`n" PQ_View(PQ)
MsgBox, 262208,, % "Add : " 					PQ_AddTask(PQ, "AutoHotkey", 2)	"`n`n" PQ_View(PQ)
MsgBox, 262208,, % "Remove Top Item : " 			PQ_TopItem(PQ) "`n`n" PQ_View(PQ)
MsgBox, 262208,, % "Remove specific top item : " 		PQ_TopItem(PQ,"test") "`n`n" PQ_View(PQ)
MsgBox, 262208,, % "Delete Item : " 				PQ_DelTask(PQ, "Clear drains")	"`n`n" PQ_View(PQ)
MsgBox, 262208,, % (Task:="Tax return") " new priority = "	PQ_Edit(PQ,task, 7)	"`n`n" PQ_View(PQ)
MsgBox, 262208,, % (Task:="Feed cat")  " priority = " 		PQ_Check(PQ,task)"`n`n" PQ_View(PQ)
^Esc::
ExitApp
```



## Axiom

Axiom already has a heap domain for ordered sets.
We define a domain for ordered key-entry pairs and then define a priority queue using the heap domain over the pairs:

```Axiom
)abbrev Domain ORDKE OrderedKeyEntry
OrderedKeyEntry(Key:OrderedSet,Entry:SetCategory): Exports == Implementation where
  Exports == OrderedSet with
    construct: (Key,Entry) -> %
    elt: (%,"key") -> Key
    elt: (%,"entry") -> Entry
  Implementation == add
    Rep := Record(k:Key,e:Entry)
    x,y: %
    construct(a,b) == construct(a,b)$Rep @ %
    elt(x,"key"):Key == (x@Rep).k
    elt(x,"entry"):Entry == (x@Rep).e
    x < y == x.key < y.key
    x = y == x.key = y.key
    hash x == hash(x.key)
    if Entry has CoercibleTo OutputForm then
      coerce(x):OutputForm == bracket [(x.key)::OutputForm,(x.entry)::OutputForm]
)abbrev Domain PRIORITY PriorityQueue
S ==> OrderedKeyEntry(Key,Entry)
PriorityQueue(Key:OrderedSet,Entry:SetCategory): Exports == Implementation where
  Exports == PriorityQueueAggregate S with
    heap : List S  -> %
    setelt: (%,Key,Entry) -> Entry
  Implementation == Heap(S) add
    setelt(x:%,key:Key,entry:Entry) ==
      insert!(construct(key,entry)$S,x)
      entry
```
For an example:
```Axiom
pq := empty()$PriorityQueue(Integer,String)
pq(3):="Clear drains";
pq(4):="Feed cat";
pq(5):="Make tea";
pq(1):="Solve RC tasks";
pq(2):="Tax return";
[extract!(pq) for i in 1..#pq]
```

{{out}}

```txt

   [[5,"Make tea"], [4,"Feed cat"], [3,"Clear drains"], [2,"Tax return"],
    [1,"Solve RC tasks"]]
                                  Type: List(OrderedKeyEntry(Integer,String))
```


## Batch File

Batch has only a data structure, the environment that incidentally sorts itself automatically by key. The environment has a limit of 64K

```Batch File

@echo off
setlocal enabledelayedexpansion

call :push 10  "item ten"
call :push 2   "item two"
call :push 100 "item one hundred"
call :push 5   "item five"

call :pop & echo !order! !item!
call :pop & echo !order! !item!
call :pop & echo !order! !item!
call :pop & echo !order! !item!
call :pop & echo !order! !item!

goto:eof


:push
set temp=000%1
set queu%temp:~-3%=%2
goto:eof

:pop
set queu >nul 2>nul
if %errorlevel% equ 1 (set order=-1&set item=no more items & goto:eof)
for /f "tokens=1,2 delims==" %%a in ('set queu') do set %%a=& set order=%%a& set item=%%~b& goto:next
:next
set order= %order:~-3%
goto:eof
```

{{out}}

```txt

 002 item two
 005 item five
 010 item ten
 100 item one hundred
-1 no more items

```



## C

Using a dynamic array as a binary heap.  Stores integer priority and a character pointer.  Supports push and pop.

```c
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int priority;
    char *data;
} node_t;

typedef struct {
    node_t *nodes;
    int len;
    int size;
} heap_t;

void push (heap_t *h, int priority, char *data) {
    if (h->len + 1 >= h->size) {
        h->size = h->size ? h->size * 2 : 4;
        h->nodes = (node_t *)realloc(h->nodes, h->size * sizeof (node_t));
    }
    int i = h->len + 1;
    int j = i / 2;
    while (i > 1 && h->nodes[j].priority > priority) {
        h->nodes[i] = h->nodes[j];
        i = j;
        j = j / 2;
    }
    h->nodes[i].priority = priority;
    h->nodes[i].data = data;
    h->len++;
}

char *pop (heap_t *h) {
    int i, j, k;
    if (!h->len) {
        return NULL;
    }
    char *data = h->nodes[1].data;

    h->nodes[1] = h->nodes[h->len];

    h->len--;

    i = 1;
    while (i!=h->len+1) {
        k = h->len+1;
        j = 2 * i;
        if (j <= h->len && h->nodes[j].priority < h->nodes[k].priority) {
            k = j;
        }
        if (j + 1 <= h->len && h->nodes[j + 1].priority < h->nodes[k].priority) {
            k = j + 1;
        }
        h->nodes[i] = h->nodes[k];
        i = k;
    }
    return data;
}

int main () {
    heap_t *h = (heap_t *)calloc(1, sizeof (heap_t));
    push(h, 3, "Clear drains");
    push(h, 4, "Feed cat");
    push(h, 5, "Make tea");
    push(h, 1, "Solve RC tasks");
    push(h, 2, "Tax return");
    int i;
    for (i = 0; i < 5; i++) {
        printf("%s\n", pop(h));
    }
    return 0;
}

```

{{output}}

```txt
Solve RC tasks
Tax return
Clear drains
Feed cat
Make tea
```



## C++

The C++ standard library contains the <code>std::priority_queue</code> opaque data structure. It implements a max-heap.


```cpp
#include <iostream>
#include <string>
#include <queue>
#include <utility>

int main() {
  std::priority_queue<std::pair<int, std::string> > pq;
  pq.push(std::make_pair(3, "Clear drains"));
  pq.push(std::make_pair(4, "Feed cat"));
  pq.push(std::make_pair(5, "Make tea"));
  pq.push(std::make_pair(1, "Solve RC tasks"));
  pq.push(std::make_pair(2, "Tax return"));

  while (!pq.empty()) {
    std::cout << pq.top().first << ", " << pq.top().second << std::endl;
    pq.pop();
  }

  return 0;
}
```


{{out}}

```txt

5, Make tea
4, Feed cat
3, Clear drains
2, Tax return
1, Solve RC tasks

```


Alternately, you can use a pre-existing container of yours
and use the heap operations to manipulate it:


```cpp
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <utility>

int main() {
  std::vector<std::pair<int, std::string> > pq;
  pq.push_back(std::make_pair(3, "Clear drains"));
  pq.push_back(std::make_pair(4, "Feed cat"));
  pq.push_back(std::make_pair(5, "Make tea"));
  pq.push_back(std::make_pair(1, "Solve RC tasks"));

  // heapify
  std::make_heap(pq.begin(), pq.end());

  // enqueue
  pq.push_back(std::make_pair(2, "Tax return"));
  std::push_heap(pq.begin(), pq.end());

  while (!pq.empty()) {
    // peek
    std::cout << pq[0].first << ", " << pq[0].second << std::endl;
    // dequeue
    std::pop_heap(pq.begin(), pq.end());
    pq.pop_back();
  }

  return 0;
}
```


{{out}}

```txt

5, Make tea
4, Feed cat
3, Clear drains
2, Tax return
1, Solve RC tasks

```



## C#


```c#
using System;

namespace PriorityQueue
{
    class Program
    {
        static void Main(string[] args)
        {
            PriorityQueue PQ = new PriorityQueue();
            PQ.push(3, "Clear drains");
            PQ.push(4, "Feed cat");
            PQ.push(5, "Make tea");
            PQ.push(1, "Solve RC tasks");
            PQ.push(2, "Tax return");

            while (!PQ.Empty)
            {
                var Val = PQ.pop();
                Console.WriteLine(Val[0] + " : " + Val[1]);
            }
            Console.ReadKey();
        }
    }

    class PriorityQueue
    {
        private System.Collections.SortedList PseudoQueue;

        public bool Empty
        {
            get
            {
                return PseudoQueue.Count == 0;
            }
        }

        public PriorityQueue()
        {
            PseudoQueue = new System.Collections.SortedList();
        }

        public void push(object Priority, object Value)
        {
            PseudoQueue.Add(Priority, Value);
        }

        public object[] pop()
        {
            object[] ReturnValue = { null, null };
            if (PseudoQueue.Count > 0)
            {
                ReturnValue[0] = PseudoQueue.GetKey(0);
                ReturnValue[1] = PseudoQueue.GetByIndex(0);

                PseudoQueue.RemoveAt(0);
            }
            return ReturnValue;
        }
    }
}
```


'''Min Heap Priority Queue'''

{{works with|C sharp|C#|3.0+/DotNet 3.5+}}
The above code is not really a true Priority Queue as it does not allow duplicate keys; also, the SortedList on which it is based does not have O(log n) insertions and removals for random data as a true Priority Queue does.  The below code implements a true Min Heap Priority Queue:

```c#
namespace PriorityQ {
  using KeyT = UInt32;
  using System;
  using System.Collections.Generic;
  using System.Linq;
  class Tuple<K, V> { // for DotNet 3.5 without Tuple's
    public K Item1; public V Item2;
    public Tuple(K k, V v) { Item1 = k; Item2 = v; }
    public override string ToString() {
      return "(" + Item1.ToString() + ", " + Item2.ToString() + ")";
    }
  }
  class MinHeapPQ<V> {
    private struct HeapEntry {
      public KeyT k; public V v;
      public HeapEntry(KeyT k, V v) { this.k = k; this.v = v; }
    }
    private List<HeapEntry> pq;
    private MinHeapPQ() { this.pq = new List<HeapEntry>(); }
    private bool mt { get { return pq.Count == 0; } }
    private int sz {
      get {
        var cnt = pq.Count;
        return (cnt == 0) ? 0 : cnt - 1;
      }
    }
    private Tuple<KeyT, V> pkmn {
      get {
        if (pq.Count == 0) return null;
        else {
          var mn = pq[0];
          return new Tuple<KeyT, V>(mn.k, mn.v);
        }
      }
    }
    private void psh(KeyT k, V v) { // add extra very high item if none
      if (pq.Count == 0) pq.Add(new HeapEntry(UInt32.MaxValue, v));
      var i = pq.Count; pq.Add(pq[i - 1]); // copy bottom item...
      for (var ni = i >> 1; ni > 0; i >>= 1, ni >>= 1) {
        var t = pq[ni - 1];
        if (t.k > k) pq[i - 1] = t; else break;
      }
      pq[i - 1] = new HeapEntry(k, v);
    }
    private void siftdown(KeyT k, V v, int ndx) {
      var cnt = pq.Count - 1; var i = ndx;
      for (var ni = i + i + 1; ni < cnt; ni = ni + ni + 1) {
        var oi = i; var lk = pq[ni].k; var rk = pq[ni + 1].k;
        var nk = k;
        if (k > lk) { i = ni; nk = lk; }
        if (nk > rk) { ni += 1; i = ni; }
        if (i != oi) pq[oi] = pq[i]; else break;
      }
      pq[i] = new HeapEntry(k, v);
    }
    private void rplcmin(KeyT k, V v) {
      if (pq.Count > 1) siftdown(k, v, 0);
    }
    private void dltmin() {
      var lsti = pq.Count - 2;
      if (lsti <= 0) pq.Clear();
      else {
        var lkv = pq[lsti];
        pq.RemoveAt(lsti); siftdown(lkv.k, lkv.v, 0);
      }
    }
    private void reheap(int i) {
      var lfti = i + i + 1;
      if (lfti < sz) {
        var rghti = lfti + 1; reheap(lfti); reheap(rghti);
        var ckv = pq[i]; siftdown(ckv.k, ckv.v, i);
      }
    }
    private void bld(IEnumerable<Tuple<KeyT, V>> sq) {
      var sqm = from e in sq
                select new HeapEntry(e.Item1, e.Item2);
      pq = sqm.ToList<HeapEntry>();
      var sz = pq.Count;
      if (sz > 0) {
        var lkv = pq[sz - 1];
        pq.Add(new HeapEntry(KeyT.MaxValue, lkv.v));
        reheap(0);
      }
    }
    private IEnumerable<Tuple<KeyT, V>> sq() {
      return from e in pq
             where e.k != KeyT.MaxValue
             select new Tuple<KeyT, V>(e.k, e.v); }
    private void adj(Func<KeyT, V, Tuple<KeyT, V>> f) {
      var cnt = pq.Count - 1;
      for (var i = 0; i < cnt; ++i) {
        var e = pq[i];
        var r = f(e.k, e.v);
        pq[i] = new HeapEntry(r.Item1, r.Item2);
      }
      reheap(0);
    }
    public static MinHeapPQ<V> empty { get { return new MinHeapPQ<V>(); } }
    public static bool isEmpty(MinHeapPQ<V> pq) { return pq.mt; }
    public static int size(MinHeapPQ<V> pq) { return pq.sz; }
    public static Tuple<KeyT, V> peekMin(MinHeapPQ<V> pq) { return pq.pkmn; }
    public static MinHeapPQ<V> push(KeyT k, V v, MinHeapPQ<V> pq) {
      pq.psh(k, v); return pq; }
    public static MinHeapPQ<V> replaceMin(KeyT k, V v, MinHeapPQ<V> pq) {
      pq.rplcmin(k, v); return pq; }
    public static MinHeapPQ<V> deleteMin(MinHeapPQ<V> pq) { pq.dltmin(); return pq; }
    public static MinHeapPQ<V> merge(MinHeapPQ<V> pq1, MinHeapPQ<V> pq2) {
      return fromSeq(pq1.sq().Concat(pq2.sq())); }
    public static MinHeapPQ<V> adjust(Func<KeyT, V, Tuple<KeyT, V>> f, MinHeapPQ<V> pq) {
      pq.adj(f); return pq; }
    public static MinHeapPQ<V> fromSeq(IEnumerable<Tuple<KeyT, V>> sq) {
      var pq = new MinHeapPQ<V>(); pq.bld(sq); return pq; }
    public static Tuple<Tuple<KeyT, V>, MinHeapPQ<V>> popMin(MinHeapPQ<V> pq) {
      var rslt = pq.pkmn; if (rslt == null) return null;
      pq.dltmin(); return new Tuple<Tuple<KeyT, V>, MinHeapPQ<V>>(rslt, pq); }
    public static IEnumerable<Tuple<KeyT, V>> toSeq(MinHeapPQ<V> pq) {
      for (; !pq.mt; pq.dltmin()) yield return pq.pkmn; }
    public static IEnumerable<Tuple<KeyT, V>> sort(IEnumerable<Tuple<KeyT, V>> sq) {
      return toSeq(fromSeq(sq)); }
  }
}
```


The above class code offers a full set of static methods and properties:
  1.  "empty" to create a new empty queue,
  2.  "isEmpty" to test if a queue is empty,
  3.  "size" to get the number of elements in the queue,
  4.  "peekMin" to retrieve the lowest priority key/value pair entry as a Tuple (possibly null for empty queues),
  5.  "push" to insert an entry,
  6.  "deleteMin" to remove the lowest priority entry,
  7.  "replaceMin" to replace the lowest priority and adjust the queue according to the value (faster than a "deleteMin" followed by a "push"),
  8.  "adjust" to apply a function to every key/value entry pair and reheapify the result,
  9.  "merge" to merge two queues into a single reheapified result,
  10. "fromSeq" to build a queue from a sequence of key/value pair tuples,
  11. "popMin" which is a convenience function combining a "peekMin" with a "deleteMin", returning null if the queue is empty and a tuple of the result otherwise,
  12. "toSeq" to output an ordered sequence of the queue contents as Tuple's of the key/value pairs, and
  13. "sort" which is a convenience function combining "fromSeq" followed by "toSeq".

The first four are all O(1) and the remainder O(log n) except "adjust" and "fromSeq" are O(n), "merge" is O(m + n) where m and n are the sizes of the two queues, and "toSeq" and "sort" are O(n log n); "replaceMin" is still O(log n) but faster than a "deleteMin" followed by a "push" by a constant factor.

Note that the Key type "KeyT" is not generic in order to give better comparison efficiency than using generic comparison using the IComparible interface but can be changed to different numeric types using the "using KeyT = ???" type alias.

The above code can be tested as per the page specification by the following code:

```c#
    static void Main(string[] args) {
      Tuple<uint, string>[] ins = { new Tuple<uint,string>(3u, "Clear drains"),
                                    new Tuple<uint,string>(4u, "Feed cat"),
                                    new Tuple<uint,string>(5u, "Make tea"),
                                    new Tuple<uint,string>(1u, "Solve RC tasks"),
                                    new Tuple<uint,string>(2u, "Tax return") };

      var spq = ins.Aggregate(MinHeapPQ<string>.empty, (pq, t) => MinHeapPQ<string>.push(t.Item1, t.Item2, pq));
      foreach (var e in MinHeapPQ<string>.toSeq(spq)) Console.WriteLine(e); Console.WriteLine();

      foreach (var e in MinHeapPQ<string>.sort(ins)) Console.WriteLine(e); Console.WriteLine();

      var npq = MinHeapPQ<string>.fromSeq(ins);
      foreach (var e in MinHeapPQ<string>.toSeq(MinHeapPQ<string>.merge(npq, npq)))
        Console.WriteLine(e); Console.WriteLine();

      var npq = MinHeapPQ<string>.fromSeq(ins);
      foreach (var e in MinHeapPQ<string>.toSeq(MinHeapPQ<string>.merge(npq, npq)))
        Console.WriteLine(e);

      foreach (var e in MinHeapPQ<string>.toSeq(MinHeapPQ<string>.adjust((k, v) => new Tuple<uint,string>(6u - k, v), npq)))
        Console.WriteLine(e); Console.WriteLine();
    }
```


It tests building the queue the slow way using repeated "push"'s - O(n log n), the faster "fromSeq" (included in the "sort") - O(n), and also tests the "merge" and "adjust" methods.

The output of the above test is as follows:
{{output}}

```txt
(1, Solve RC tasks)
(2, Tax return)
(3, Clear drains)
(4, Feed cat)
(5, Make tea)

(1, Solve RC tasks)
(2, Tax return)
(3, Clear drains)
(4, Feed cat)
(5, Make tea)

(1, Solve RC tasks)
(1, Solve RC tasks)
(2, Tax return)
(2, Tax return)
(3, Clear drains)
(3, Clear drains)
(4, Feed cat)
(4, Feed cat)
(5, Make tea)
(5, Make tea)

(1, Make tea)
(2, Feed cat)
(3, Clear drains)
(4, Tax return)
(5, Solve RC tasks)
```



## Common Lisp

In this task were implemented to versions of the functions, the non-destructive ones that will not change the state of the priority queue and the destructive ones that will change. The destructive ones work very similarly to the 'pop' and 'push' functions.

```lisp

;priority-queue's are implemented with association lists
(defun make-pq (alist)
  (sort (copy-alist alist) (lambda (a b) (< (car a) (car b)))))
;
;Will change the state of pq
;
(define-modify-macro insert-pq (pair)
                     (lambda (pq pair) (sort-alist (cons pair pq))))

(define-modify-macro remove-pq-aux () cdr)

(defmacro remove-pq (pq)
  `(let ((aux (copy-alist ,pq)))
     (REMOVE-PQ-AUX ,pq)
     (car aux)))
;
;Will not change the state of pq
;
(defun insert-pq-non-destructive (pair pq)
  (sort-alist (cons pair pq)))

(defun remove-pq-non-destructive (pq)
  (cdr pq))
;testing
(defparameter a (make-pq '((1 . "Solve RC tasks") (3 . "Clear drains") (2 . "Tax return") (5 . "Make tea"))))
(format t "~a~&" a)
(insert-pq a '(4 . "Feed cat"))
(format t "~a~&" a)
(format t "~a~&" (remove-pq a))
(format t "~a~&" a)
(format t "~a~&" (remove-pq a))
(format t "~a~&" a)

```

{{out}}

```txt

((1 . Solve RC tasks) (2 . Tax return) (3 . Clear drains) (5 . Make tea))
((1 . Solve RC tasks) (2 . Tax return) (3 . Clear drains) (4 . Feed cat) (5 . Make tea))
(1 . Solve RC tasks)
((2 . Tax return) (3 . Clear drains) (4 . Feed cat) (5 . Make tea))
(2 . Tax return)
((3 . Clear drains) (4 . Feed cat) (5 . Make tea))

```



## Clojure



```clojure>user=
 (use 'clojure.data.priority-map)

; priority-map can be used as a priority queue
user=> (def p (priority-map "Clear drains" 3, "Feed cat" 4, "Make tea" 5, "Solve RC tasks" 1))
#'user/p
user=> p
{"Solve RC tasks" 1, "Clear drains" 3, "Feed cat" 4, "Make tea" 5}

; You can use assoc or conj to add items
user=> (assoc p "Tax return" 2)
{"Solve RC tasks" 1, "Tax return" 2, "Clear drains" 3, "Feed cat" 4, "Make tea" 5}

; peek to get first item, pop to give you back the priority-map with the first item removed
user=> (peek p)
["Solve RC tasks" 1]

; Merge priority-maps together
user=> (into p [["Wax Car" 4]["Paint Fence" 1]["Sand Floor" 3]])
{"Solve RC tasks" 1, "Paint Fence" 1, "Clear drains" 3, "Sand Floor" 3, "Wax Car" 4, "Feed cat" 4, "Make tea" 5}
```



## CoffeeScript


```coffeescript

PriorityQueue = ->
  # Use closure style for object creation (so no "new" required).
  # Private variables are toward top.
  h = []

  better = (a, b) ->
    h[a].priority < h[b].priority

  swap = (a, b) ->
    [h[a], h[b]] = [h[b], h[a]]

  sift_down = ->
    max = h.length
    n = 0
    while n < max
      c1 = 2*n + 1
      c2 = c1 + 1
      best = n
      best = c1 if c1 < max and better(c1, best)
      best = c2 if c2 < max and better(c2, best)
      return if best == n
      swap n, best
      n = best

  sift_up = ->
    n = h.length - 1
    while n > 0
      parent = Math.floor((n-1) / 2)
      return if better parent, n
      swap n, parent
      n = parent

  # now return the public interface, which is an object that only
  # has functions on it
  self =
    size: ->
      h.length

    push: (priority, value) ->
      elem =
        priority: priority
        value: value
      h.push elem
      sift_up()

    pop: ->
      throw Error("cannot pop from empty queue") if h.length == 0
      value = h[0].value
      last = h.pop()
      if h.length > 0
        h[0] = last
        sift_down()
      value

# test
do ->
  pq = PriorityQueue()
  pq.push 3, "Clear drains"
  pq.push 4, "Feed cat"
  pq.push 5, "Make tea"
  pq.push 1, "Solve RC tasks"
  pq.push 2, "Tax return"

  while pq.size() > 0
    console.log pq.pop()

  # test high performance
  for n in [1..100000]
    priority = Math.random()
    pq.push priority, priority

  v = pq.pop()
  console.log "First random element was #{v}"
  while pq.size() > 0
    new_v = pq.pop()
    throw Error "Queue broken" if new_v < v
    v = new_v
  console.log "Final random element was #{v}"

```


output

<lang>
> coffee priority_queue.coffee
Solve RC tasks
Tax return
Clear drains
Feed cat
Make tea
First random element was 0.00002744467929005623
Final random element was 0.9999718656763434

```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE PQueues;
IMPORT StdLog,Boxes;

TYPE
  Rank* = POINTER TO RECORD
    p-: LONGINT; (* Priority *)
    value-: Boxes.Object
  END;

  PQueue* = POINTER TO RECORD
    a: POINTER TO ARRAY OF Rank;
    size-: LONGINT;
  END;

  PROCEDURE NewRank*(p: LONGINT; v: Boxes.Object): Rank;
  VAR
    r: Rank;
  BEGIN
    NEW(r);r.p := p;r.value := v;
    RETURN r
  END NewRank;

  PROCEDURE NewPQueue*(cap: LONGINT): PQueue;
  VAR
    pq: PQueue;
  BEGIN
    NEW(pq);pq.size := 0;
    NEW(pq.a,cap);pq.a[0] := NewRank(MIN(INTEGER),NIL);
    RETURN pq
  END NewPQueue;

  PROCEDURE (pq: PQueue) Push*(r:Rank), NEW;
  VAR
    i: LONGINT;
  BEGIN
    INC(pq.size);
    i := pq.size;
    WHILE r.p < pq.a[i DIV 2].p DO
      pq.a[i] := pq.a[i DIV 2];i := i DIV 2
    END;
    pq.a[i] := r
  END Push;

  PROCEDURE (pq: PQueue) Pop*(): Rank,NEW;
  VAR
    r,y: Rank;
    i,j: LONGINT;
    ok: BOOLEAN;
  BEGIN
    r := pq.a[1]; (* Priority object *)
    y := pq.a[pq.size]; DEC(pq.size); i := 1; ok := FALSE;
    WHILE (i <= pq.size DIV 2) & ~ok DO
      j := i + 1;
      IF (j < pq.size) & (pq.a[i].p > pq.a[j + 1].p) THEN INC(j) END;
      IF y.p > pq.a[j].p THEN pq.a[i] := pq.a[j]; i := j ELSE ok := TRUE END
    END;
    pq.a[i] := y;
    RETURN r
  END Pop;

  PROCEDURE (pq: PQueue) IsEmpty*(): BOOLEAN,NEW;
  BEGIN
    RETURN pq.size = 0
  END IsEmpty;

  PROCEDURE Test*;
  VAR
    pq: PQueue;
    r: Rank;
    PROCEDURE ShowRank(r:Rank);
    BEGIN
      StdLog.Int(r.p);StdLog.String(":> ");StdLog.String(r.value.AsString());StdLog.Ln;
    END ShowRank;
  BEGIN
    pq := NewPQueue(128);
    pq.Push(NewRank(3,Boxes.NewString("Clear drains")));
    pq.Push(NewRank(4,Boxes.NewString("Feed cat")));
    pq.Push(NewRank(5,Boxes.NewString("Make tea")));
    pq.Push(NewRank(1,Boxes.NewString("Solve RC tasks")));
    pq.Push(NewRank(2,Boxes.NewString("Tax return")));
    ShowRank(pq.Pop());
    ShowRank(pq.Pop());
    ShowRank(pq.Pop());
    ShowRank(pq.Pop());
    ShowRank(pq.Pop());
  END Test;

END PQueues.

```

Interface extracted from the implementation

```oberon2

DEFINITION PQueues;

  IMPORT Boxes;

  TYPE
    PQueue = POINTER TO RECORD
      size-: LONGINT;
      (pq: PQueue) IsEmpty (): BOOLEAN, NEW;
      (pq: PQueue) Pop (): Rank, NEW;
      (pq: PQueue) Push (r: Rank), NEW
    END;

    Rank = POINTER TO RECORD
      p-: LONGINT;
      value-: Boxes.Object
    END;

  PROCEDURE NewPQueue (cap: LONGINT): PQueue;
  PROCEDURE NewRank (p: LONGINT; v: Boxes.Object): Rank;
  PROCEDURE Test;

END PQueues.

```

Execute: ^Q PQueues.Test<br/>
Output:

```txt

 1:> Solve RC tasks
 2:> Tax return
 3:> Clear drains
 4:> Feed cat
 5:> Make tea

```



## D


```d
import std.stdio, std.container, std.array, std.typecons;

void main() {
    alias tuple T;
    auto heap = heapify([T(3, "Clear drains"),
                         T(4, "Feed cat"),
                         T(5, "Make tea"),
                         T(1, "Solve RC tasks"),
                         T(2, "Tax return")]);

    while (!heap.empty) {
        writeln(heap.front);
        heap.removeFront();
    }
}
```

{{out}}

```txt
Tuple!(int,string)(5, "Make tea")
Tuple!(int,string)(4, "Feed cat")
Tuple!(int,string)(3, "Clear drains")
Tuple!(int,string)(2, "Tax return")
Tuple!(int,string)(1, "Solve RC tasks")
```



## EchoLisp

We use the built-in binary tree library. Each tree node has a datum (key . value). The functions  (bin-tree-pop-first tree) and (bin-tree-pop-last tree) allow to extract the node with highest or lowest priority.

```lisp

(lib 'tree)
(define tasks (make-bin-tree 3 "Clear drains"))
(bin-tree-insert tasks 2 "Tax return")
(bin-tree-insert tasks 5 "Make tea")
(bin-tree-insert tasks 1 "Solve RC tasks")
(bin-tree-insert tasks 4 "Feed 🐡")

(bin-tree-pop-first tasks) → (1 . "Solve RC tasks")
(bin-tree-pop-first tasks) → (2 . "Tax return")
(bin-tree-pop-first tasks) → (3 . "Clear drains")
(bin-tree-pop-first tasks) → (4 . "Feed 🐡")
(bin-tree-pop-first tasks) → (5 . "Make tea")
(bin-tree-pop-first tasks) → null

;; similarly
(bin-tree-pop-last tasks) → (5 . "Make tea")
(bin-tree-pop-last tasks) → (4 . "Feed 🐡")
; etc.

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Priority do
  def create, do: :gb_trees.empty

  def insert( element, priority, queue ), do: :gb_trees.enter( priority, element, queue )

  def peek( queue ) do
    {_priority, element, _new_queue} = :gb_trees.take_smallest( queue )
    element
  end

  def task do
    items = [{3, "Clear drains"}, {4, "Feed cat"}, {5, "Make tea"}, {1, "Solve RC tasks"}, {2, "Tax return"}]
    queue = Enum.reduce(items, create, fn({priority, element}, acc) -> insert( element, priority, acc ) end)
    IO.puts "peek priority: #{peek( queue )}"
    Enum.reduce(1..length(items), queue, fn(_n, q) -> write_top( q ) end)
  end

  def top( queue ) do
    {_priority, element, new_queue} = :gb_trees.take_smallest( queue )
    {element, new_queue}
  end

  defp write_top( q ) do
    {element, new_queue} = top( q )
    IO.puts "top priority: #{element}"
    new_queue
  end
end

Priority.task
```


{{out}}

```txt

peek priority: Solve RC tasks
top priority: Solve RC tasks
top priority: Tax return
top priority: Clear drains
top priority: Feed cat
top priority: Make tea

```



## Erlang

Using built in gb_trees module, with the suggested interface for this task.

```Erlang

-module( priority_queue ).

-export( [create/0, insert/3, peek/1, task/0, top/1] ).

create() -> gb_trees:empty().

insert( Element, Priority, Queue ) -> gb_trees:enter( Priority, Element, Queue ).

peek( Queue ) ->
  {_Priority, Element, _New_queue} = gb_trees:take_smallest( Queue ),
  Element.

task() ->
  Items = [{3, "Clear drains"}, {4, "Feed cat"}, {5, "Make tea"}, {1, "Solve RC tasks"}, {2, "Tax return"}],
  Queue = lists:foldl( fun({Priority, Element}, Acc) -> insert( Element, Priority, Acc ) end, create(), Items ),
  io:fwrite( "peek priority: ~p~n", [peek( Queue )] ),
  lists:foldl( fun(_N, Q) -> write_top( Q ) end, Queue, lists:seq(1, erlang:length(Items)) ).

top( Queue ) ->
  {_Priority, Element, New_queue} = gb_trees:take_smallest( Queue ),
  {Element, New_queue}.



write_top( Q ) ->
  {Element, New_queue} = top( Q ),
  io:fwrite( "top priority: ~p~n", [Element] ),
  New_queue.

```

{{out}}

```txt

12> priority_queue:task().
peek priority: "Solve RC tasks"
top priority: "Solve RC tasks"
top priority: "Tax return"
top priority: "Clear drains"
top priority: "Feed cat"
top priority: "Make tea"

```



## F Sharp


The below codes all provide the standard priority queue functions of "peekMin", "push", and "deleteMin"; as well, "replaceMin" which can be much more efficient that a "deleteMin" followed by a "push" for some types of queues), "popMin" (generally a convenience function for "peekMin" followed by "deleteMin"), "adjust" for applying a function to all queue entries and reheapifying, "fromSeq" for building a queue from a sequence, "toSeq" for outputting a sorted sequence of the queue contents, and "sort" which is a convenience function combining the latter two functions are provided. Finally, the queue's all provide a "merge" function to combine two queues into one, and an "adjust" function which applies a function to every heap element and reheapifies.

### Functional


'''Binomial Heap Priority Queue'''

The following Binomial Heap Priority Queue code has been adapted [http://cs.hubfs.net/topic/None/56608 from a version by "DeeJay"] updated for changes in F# over the intervening years, and implementing the O(1) "peekMin" mentioned in that post; in addition to the above standard priority queue functions, it also implements the "merge" function for which the Binomial Heap is particularly suited, taking O(log n) time rather than the usual O(n) (or worse) time:

```fsharp>[<RequireQualifiedAccess
]
module PriorityQ =

//  type 'a treeElement = Element of uint32 * 'a
  type 'a treeElement = struct val k:uint32 val v:'a new(k,v) = { k=k;v=v } end

  type 'a tree = Node of uint32 * 'a treeElement * 'a tree list

  type 'a heap = 'a tree list

  [<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
  [<NoEquality; NoComparison>]
  type 'a outerheap = | HeapEmpty | HeapNotEmpty of 'a treeElement * 'a heap

  let empty = HeapEmpty

  let isEmpty = function | HeapEmpty -> true | _ -> false

  let inline private rank (Node(r,_,_)) = r

  let inline private root (Node(_,x,_)) = x

  exception Empty_Heap

  let peekMin = function | HeapEmpty -> None
                         | HeapNotEmpty(min, _) -> Some (min.k, min.v)

  let rec private findMin heap =
    match heap with | [] -> raise Empty_Heap //guarded so should never happen
                    | [node] -> root node,[]
                    | topnode::heap' ->
                      let min,subheap = findMin heap' in let rtn = root topnode
                      match subheap with
                        | [] -> if rtn.k > min.k then min,[] else rtn,[]
                        | minnode::heap'' ->
                          let rmn = root minnode
                          if rtn.k <= rmn.k then rtn,heap
                          else rmn,minnode::topnode::heap''

  let private mergeTree (Node(r,kv1,ts1) as tree1) (Node (_,kv2,ts2) as tree2) =
    if kv1.k > kv2.k then Node(r+1u,kv2,tree1::ts2)
    else Node(r+1u,kv1,tree2::ts1)

  let rec private insTree (newnode: 'a tree) heap =
    match heap with
      | [] -> [newnode]
      | topnode::heap' -> if (rank newnode) < (rank topnode) then newnode::heap
                          else insTree (mergeTree newnode topnode) heap'

  let push k v = let kv = treeElement(k,v) in let nn = Node(0u,kv,[])
                   function | HeapEmpty -> HeapNotEmpty(kv,[nn])
                            | HeapNotEmpty(min,heap) -> let nmin = if k > min.k then min else kv
                                                        HeapNotEmpty(nmin,insTree nn heap)

  let rec private merge' heap1 heap2 = //doesn't guaranty minimum tree node as head!!!
    match heap1,heap2 with
      | _,[] -> heap1
      | [],_ -> heap2
      | topheap1::heap1',topheap2::heap2' ->
        match compare (rank topheap1) (rank topheap2) with
          | -1 -> topheap1::merge' heap1' heap2
          | 1 -> topheap2::merge' heap1 heap2'
          | _ -> insTree (mergeTree topheap1 topheap2) (merge' heap1' heap2')

  let merge oheap1 oheap2 = match oheap1,oheap2 with
                              | _,HeapEmpty -> oheap1
                              | HeapEmpty,_ -> oheap2
                              | HeapNotEmpty(min1,heap1),HeapNotEmpty(min2,heap2) ->
                                  let min = if min1.k > min2.k then min2 else min1
                                  HeapNotEmpty(min,merge' heap1 heap2)

  let rec private removeMinTree = function
                          | [] -> raise Empty_Heap // will never happen as already guarded
                          | [node] -> node,[]
                          | t::ts -> let t',ts' = removeMinTree ts
                                     if (root t).k <= (root t').k then t,ts else t',t::ts'

  let deleteMin =
    function | HeapEmpty -> HeapEmpty
             | HeapNotEmpty(_,heap) ->
               match heap with
                 | [] -> HeapEmpty // should never occur: non empty heap with no elements
                 | [Node(_,_,heap')] -> match heap' with
                                          | [] -> HeapEmpty
                                          | _ -> let min,_ = findMin heap'
                                                 HeapNotEmpty(min,heap')
                 | _::_ -> let Node(_,_,ts1),ts2 = removeMinTree heap
                           let nheap = merge' (List.rev ts1) ts2 in let min,_ = findMin nheap
                           HeapNotEmpty(min,nheap)

  let replaceMin k v pq = push k v (deleteMin pq)

  let fromSeq sq = Seq.fold (fun pq (k, v) -> push k v pq) empty sq

  let popMin pq = match peekMin pq with
                      | None -> None
                      | Some(kv) -> Some(kv, deleteMin pq)

  let toSeq pq = Seq.unfold popMin pq

  let sort sq = sq |> fromSeq |> toSeq

  let adjust f pq = pq |> toSeq |> Seq.map (fun (k, v) -> f k v) |> fromSeq
```


"isEmpty", "empty", and "peekMin" all have O(1) performance, "push" is O(1) amortized performance with O(log n) worst case, and the rest are O(log n) except for "fromSeq" (and thus "sort" and "adjust") which have O(n log n) performance since they use repeated "deleteMin" with one per entry.

No "size" function is provided, but it would be implemented by summing the total size of all the nested tree lists, which each have a "Count" property and thus would be quite fast.

Note that the current "adjust" function is horribly inefficient as it outputs the original queue as a sorted sequence (O(n log n) time complexity), maps the adjusting function to each element, and rebuilds the queue be repeated "push" operations of the resulting sequence.  This could be improved by re-writing to output the sequence in unsorted order (using an internal function that doesn't use repeated "deleteMin" operations) and then rebuilding from the adjusted sequence; doing this would make the "adjust" operation take O(n) amortized time.

The "sort" function also uses a similar technique of building a queue from a sequence by repeated "push" operations (however, those only take O(n) amortized time for the Binomial Heap), then outputting a sorted sequence by repeated "popMin" operations for a combined O(n log n) time complexity.

'''Min Heap Priority Queue'''

The following code implementing a Min Heap Priority Queue is adapted from the [http://www.cl.cam.ac.uk/~lp15/MLbook/programs/sample4.sml ML PRIORITY_QUEUE code by Lawrence C. Paulson] including separating the key/value pairs as separate entries in the data structure for better comparison efficiency; it implements an efficient "fromSeq" function using reheapify for which the Min Heap is particularly suited as it has only O(n) instead of O(n log n) computational time complexity, which method is also used for the "adjust" and "merge" functions:

```fsharp>[<RequireQualifiedAccess
]
module PriorityQ =

  type HeapEntry<'V> = struct val k:uint32 val v:'V new(k,v) = {k=k;v=v} end
  [<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
  [<NoEquality; NoComparison>]
  type PQ<'V> =
         | Mt
         | Br of HeapEntry<'V> * PQ<'V> * PQ<'V>

  let empty = Mt

  let isEmpty = function | Mt -> true
                         | _  -> false

  // Return number of elements in the priority queue.
  // /O(log(n)^2)/
  let rec size = function
    | Mt -> 0
    | Br(_, ll, rr) ->
        let n = size rr
        // rest n p q, where n = size ll, and size ll - size rr = 0 or 1
        // returns 1 + size ll - size rr.
        let rec rest n pl pr =
          match pl with
            | Mt -> 1
            | Br(_, pll, plr) ->
                match pr with
                  | Mt -> 2
                  | Br(_, prl, prr) ->
                      let nm1 = n - 1 in let d = nm1 >>> 1
                      if (nm1 &&& 1) = 0
                        then rest d pll prl // subtree sizes: (d or d+1), d; d, d
                        else rest d plr prr // subtree sizes: d+1, (d or d+1); d+1, d
        2 * n + rest n ll rr

  let peekMin = function | Br(kv, _, _) -> Some(kv.k, kv.v)
                         | _            -> None

  let rec push wk wv =
    function | Mt -> Br(HeapEntry(wk, wv), Mt, Mt)
             | Br(vkv, ll, rr) ->
                 if wk <= vkv.k then
                   Br(HeapEntry(wk, wv), push vkv.k vkv.v rr, ll)
                 else Br(vkv, push wk wv rr, ll)

  let inline private siftdown wk wv pql pqr =
    let rec sift pl pr =
      match pl with
        | Mt -> Br(HeapEntry(wk, wv), Mt, Mt)
        | Br(vkvl, pll, plr) ->
            match pr with
              | Mt -> if wk <= vkvl.k then Br(HeapEntry(wk, wv), pl, Mt)
                      else Br(vkvl, Br(HeapEntry(wk, wv), Mt, Mt), Mt)
              | Br(vkvr, prl, prr) ->
                  if wk <= vkvl.k && wk <= vkvr.k then Br(HeapEntry(wk, wv), pl, pr)
                  elif vkvl.k <= vkvr.k then Br(vkvl, sift pll plr, pr)
                  else Br(vkvr, pl, sift prl prr)
    sift pql pqr

  let replaceMin wk wv = function | Mt -> Mt
                                  | Br(_, ll, rr) -> siftdown wk wv ll rr

  let deleteMin = function
        | Mt -> Mt
        | Br(_, ll, Mt) -> ll
        | Br(vkv, ll, rr) ->
          let rec leftrem = function | Mt -> vkv, Mt // should never happen
                                     | Br(kvd, Mt, _) -> kvd, Mt
                                     | Br(vkv, Br(kvd, _, _), Mt) ->
                                                 kvd, Br(vkv, Mt, Mt)
                                     | Br(vkv, pl, pr) -> let kvd, pqd = leftrem pl
                                                          kvd, Br(vkv, pr, pqd)
          let (kvd, pqd) = leftrem ll
          siftdown kvd.k kvd.v rr pqd;

  let adjust f pq =
        let rec adj = function
              | Mt -> Mt
              | Br(vkv, ll, rr) -> let nk, nv = f vkv.k vkv.v
                                   siftdown nk nv (adj ll) (adj rr)
        adj pq

  let fromSeq sq =
    if Seq.isEmpty sq then Mt
    else let nmrtr = sq.GetEnumerator()
         let rec build lvl = if lvl = 0 || not (nmrtr.MoveNext()) then Mt
                             else let ck, cv = nmrtr.Current
                                  let lft = lvl >>> 1
                                  let rght = (lvl - 1) >>> 1
                                  siftdown ck cv (build lft) (build rght)
         build (sq |> Seq.length)

  let merge (pq1:PQ<_>) (pq2:PQ<_>) = // merges without using a sequence
    match pq1 with
      | Mt -> pq2
      | _ ->
        match pq2 with
          | Mt -> pq1
          | _ ->
            let rec zipper lvl pq rest =
              if lvl = 0 then Mt, pq, rest else
              let lft = lvl >>> 1 in let rght = (lvl - 1) >>> 1
              match pq with
                | Mt ->
                  match rest with
                    | [] | Mt :: _ -> Mt, pq, [] // Mt in list never happens
                    | Br(kv, ll, Mt) :: tl ->
                        let pl, pql, rstl = zipper lft ll tl
                        let pr, pqr, rstr = zipper rght pql rstl
                        siftdown kv.k kv.v pl pr, pqr, rstr
                    | Br(kv, ll, rr) :: tl ->
                        let pl, pql, rstl = zipper lft ll (rr :: tl)
                        let pr, pqr, rstr = zipper rght pql rstl
                        siftdown kv.k kv.v pl pr, pqr, rstr
                | Br(kv, ll, Mt) ->
                    let pl, pql, rstl = zipper lft ll rest
                    let pr, pqr, rstr = zipper rght pql rstl
                    siftdown kv.k kv.v pl pr, pqr, rstr
                | Br(kv, ll, rr) ->
                    let pl, pql, rstl = zipper lft ll (rr :: rest)
                    let pr, pqr, rstr = zipper rght pql rstl
                    siftdown kv.k kv.v pl pr, pqr, rstr
            let sz = size pq1 + size pq2
            let pq, _, _ = zipper sz pq1 [pq2] in pq

  let popMin pq = match peekMin pq with
                      | None -> None
                      | Some(kv) -> Some(kv, deleteMin pq)

  let toSeq pq = Seq.unfold popMin pq

  let sort sq = sq |> fromSeq |> toSeq
```


The above code implements a "merge" function so that no internal sequence generation is necessary as generation of sequence iterators is quite inefficient in F# for a combined O(n) computational time complexity but a considerable reduction in the constant factor overhead.

Other than the "merge" function, the Min Heap Priority Queue has the same time complexity as for the Binomial Heap Priority Queue above except that "push" has O(log n) performance rather than the amortized O(1) performance; however, the Binomial Heap Priority Queue is generally a constant factor slower due to more complex operations.  The Binomial Heap Priority Queue is generally more suited when used where merging of large queues or frequent "push" operations are used; the Min Heap Priority Queue is more suitable for use when replacing the value at the minimum entry of the queue is frequently required, especially when the adjusted value is not displaced very far down the queue on average.


### Imperative


'''Min Heap Priority Queue'''

As the Min Heap is usually implemented as a [http://opendatastructures.org/versions/edition-0.1e/ods-java/10_1_BinaryHeap_Implicit_Bi.html mutable array binary heap] after a genealogical tree based model invented by [https://en.wikipedia.org/wiki/Michael_Eytzinger Michael Eytzinger] over 400 years ago, the following "ugly imperative" code implements the Min Heap Priority Queue this way; note that the code could be implemented not using "ugly" mutable state variables other than the contents of the array (DotNet List which implements a growable array) but in this case the code would be considerably slower as in not much faster or slower than the functional version since using mutable side effects greatly reduces the number of operations:

```fsharp>[<RequireQualifiedAccess
]
module PriorityQ =

  type HeapEntry<'T> = struct val k:uint32 val v:'T new(k,v) = { k=k;v=v } end
  type MinHeapTree<'T> = ResizeArray<HeapEntry<'T>>

  let empty<'T> = MinHeapTree<HeapEntry<'T>>()

  let isEmpty (pq: MinHeapTree<_>) = pq.Count = 0

  let size (pq: MinHeapTree<_>) = let cnt = pq.Count
                                  if cnt = 0 then 0 else cnt - 1

  let peekMin (pq:MinHeapTree<_>) = if pq.Count > 1 then let kv = pq.[0]
                                                         Some (kv.k, kv.v) else None

  let push k v (pq:MinHeapTree<_>) =
    if pq.Count = 0 then pq.Add(HeapEntry(0xFFFFFFFFu,v)) //add an extra entry so there's always a right max node
    let mutable nxtlvl = pq.Count in let mutable lvl = nxtlvl <<< 1 //1 past index of value added times 2
    pq.Add(pq.[nxtlvl - 1]) //copy bottom entry then do bubble up while less than next level up
    while ((lvl <- lvl >>> 1); nxtlvl <- nxtlvl >>> 1; nxtlvl <> 0) do
      let t = pq.[nxtlvl - 1] in if t.k > k then pq.[lvl - 1] <- t else lvl <- lvl <<< 1; nxtlvl <- 0 //causes loop break
    pq.[lvl - 1] <-  HeapEntry(k,v); pq

  let inline private siftdown k v ndx (pq: MinHeapTree<_>) =
    let mutable i = ndx in let mutable ni = i in let cnt = pq.Count - 1
    while (ni <- ni + ni + 1; ni < cnt) do
      let lk = pq.[ni].k in let rk = pq.[ni + 1].k in let oi = i
      let k = if k > lk then i <- ni; lk else k in if k > rk then ni <- ni + 1; i <- ni
      if i <> oi then pq.[oi] <- pq.[i] else ni <- cnt //causes loop break
    pq.[i] <- HeapEntry(k,v)

  let replaceMin k v (pq:MinHeapTree<_>) = siftdown k v 0 pq; pq

  let deleteMin (pq:MinHeapTree<_>) =
    let lsti = pq.Count - 2
    if lsti <= 0 then pq.Clear(); pq else
    let lstkv = pq.[lsti]
    pq.RemoveAt(lsti)
    siftdown lstkv.k lstkv.v 0 pq; pq

  let adjust f (pq:MinHeapTree<_>) = //adjust all the contents using the function, then re-heapify
    let cnt = pq.Count - 1
    let rec adj i =
      let lefti = i + i + 1 in let righti = lefti + 1
      let ckv = pq.[i] in let (nk, nv) = f ckv.k ckv.v
      if righti < cnt then adj righti
      if lefti < cnt then adj lefti; siftdown nk nv i pq
      else pq.[i] <- HeapEntry(nk, nv)
    adj 0; pq

  let fromSeq sq =
    if Seq.isEmpty sq then empty
    else let pq = new MinHeapTree<_>(sq |> Seq.map (fun (k, v) -> HeapEntry(k, v)))
         let sz = pq.Count in let lkv = pq.[sz - 1]
         pq.Add(HeapEntry(UInt32.MaxValue, lkv.v))
         let rec build i =
           let lefti = i + i + 1
           if lefti < sz then
             let righti = lefti + 1 in build lefti; build righti
             let ckv = pq.[i] in siftdown ckv.k ckv.v i pq
         build 0; pq

  let merge (pq1:MinHeapTree<_>) (pq2:MinHeapTree<_>) =
    if pq2.Count = 0 then pq1 else
    if pq1.Count = 0 then pq2 else
    let pq = empty
    pq.AddRange(pq1); pq.RemoveAt(pq.Count - 1)
    pq.AddRange(pq2)
    let sz = pq.Count - 1
    let rec build i =
      let lefti = i + i + 1
      if lefti < sz then
        let righti = lefti + 1 in build lefti; build righti
        let ckv = pq.[i] in siftdown ckv.k ckv.v i pq
    build 0; pq

  let popMin pq = match peekMin pq with
                   | None     -> None
                   | Some(kv) -> Some(kv, deleteMin pq)

  let toSeq pq = Seq.unfold popMin pq

  let sort sq = sq |> fromSeq |> toSeq
```


The comments for the above code are the same as for the functional version; the main difference is that the imperative code takes about two thirds of the time on average.

All of the above codes can be tested under the F# REPL using the following:

```fsharp>
 let testseq = [| (3u, "Clear drains");
                   (4u, "Feed cat");
                   (5u, "Make tea");
                   (1u, "Solve RC tasks");
                   (2u, "Tax return") |] |> Array.toSeq
  let testpq = testseq |> MinHeap.fromSeq
  testseq |> Seq.fold (fun pq (k, v) -> MinHeap.push k v pq) MinHeap.empty
  |> MinHeap.toSeq |> Seq.iter (printfn "%A") // test slow build
  printfn ""
  testseq |> MinHeap.fromSeq |> MinHeap.toSeq // test fast build
   |> Seq.iter (printfn "%A")
  printfn ""
  testseq |> MinHeap.sort |> Seq.iter (printfn "%A") // convenience function
  printfn ""
  MinHeap.merge testpq testpq // test merge
  |> MinHeap.toSeq |> Seq.iter (printfn "%A")
  printfn ""
  testpq |> MinHeap.adjust (fun k v -> uint32 (MinHeap.size testpq) - k, v)
  |> MinHeap.toSeq |> Seq.iter (printfn "%A") // test adjust;;
```


to produce the following output:
{{output}}

```txt
(1u, "Solve RC tasks")
(2u, "Tax return")
(3u, "Clear drains")
(4u, "Feed cat")
(5u, "Make tea")

(1u, "Solve RC tasks")
(2u, "Tax return")
(3u, "Clear drains")
(4u, "Feed cat")
(5u, "Make tea")

(1u, "Solve RC tasks")
(2u, "Tax return")
(3u, "Clear drains")
(4u, "Feed cat")
(5u, "Make tea")

(1u, "Solve RC tasks")
(1u, "Solve RC tasks")
(2u, "Tax return")
(2u, "Tax return")
(3u, "Clear drains")
(3u, "Clear drains")
(4u, "Feed cat")
(4u, "Feed cat")
(5u, "Make tea")
(5u, "Make tea")

(0u, "Make tea")
(1u, "Feed cat")
(2u, "Clear drains")
(3u, "Tax return")
(4u, "Solve RC tasks")
val it : unit = ()
```


Note that the code using "fromSeq" instead of repeated "push" operations to build a queue is considerably faster for large random-order entry sequences.

Also note that the imperative version modifies the state of the "testpq" binding for modification operations such as "push" and "deleteMin" or operations derived from those; this means that if the last two tests were reversed then the "merge" would be passed zero sized queues since the "testpq" would have been reduced by the "toSeq" operation (which effectively uses repeated "deleteMin" functions).


## Factor

Factor has priority queues implemented in the library: documentation is available at http://docs.factorcode.org/content/article-heaps.html (or by typing "heaps" help interactively in the listener).

```factor
<min-heap> [ {
    { 3 "Clear drains" }
    { 4 "Feed cat" }
    { 5 "Make tea" }
    { 1 "Solve RC tasks" }
    { 2 "Tax return" }
  } swap heap-push-all
] [
  [ print ] slurp-heap
] bi
```


output:

```factor
Solve RC tasks
Tax return
Clear drains
Feed cat
Make tea
```



## Fortran


```Fortran
module priority_queue_mod
implicit none

type node
  character (len=100)              :: task
  integer                          :: priority
end type

type queue
  type(node), allocatable :: buf(:)
  integer                 :: n = 0
contains
  procedure :: top
  procedure :: enqueue
  procedure :: siftdown
end type

contains

subroutine siftdown(this, a)
  class (queue)           :: this
  integer                 :: a, parent, child
  associate (x => this%buf)
  parent = a
  do while(parent*2 <= this%n)
    child = parent*2
    if (child + 1 <= this%n) then
      if (x(child+1)%priority > x(child)%priority ) then
        child = child +1
      end if
    end if
    if (x(parent)%priority < x(child)%priority) then
      x([child, parent]) = x([parent, child])
      parent = child
    else
      exit
    end if
  end do
  end associate
end subroutine

function top(this) result (res)
  class(queue) :: this
  type(node)   :: res
  res = this%buf(1)
  this%buf(1) = this%buf(this%n)
  this%n = this%n - 1
  call this%siftdown(1)
end function

subroutine enqueue(this, priority, task)
  class(queue), intent(inout) :: this
  integer                     :: priority
  character(len=*)            :: task
  type(node)                  :: x
  type(node), allocatable     :: tmp(:)
  integer                     :: i
  x%priority = priority
  x%task = task
  this%n = this%n +1
  if (.not.allocated(this%buf)) allocate(this%buf(1))
  if (size(this%buf)<this%n) then
    allocate(tmp(2*size(this%buf)))
    tmp(1:this%n-1) = this%buf
    call move_alloc(tmp, this%buf)
  end if
  this%buf(this%n) = x
  i = this%n
  do
    i = i / 2
    if (i==0) exit
    call this%siftdown(i)
  end do
end subroutine
end module

program main
  use priority_queue_mod

  type (queue) :: q
  type (node)  :: x

  call q%enqueue(3, "Clear drains")
  call q%enqueue(4, "Feed cat")
  call q%enqueue(5, "Make Tea")
  call q%enqueue(1, "Solve RC tasks")
  call q%enqueue(2, "Tax return")

  do while (q%n >0)
    x = q%top()
    print "(g0,a,a)", x%priority, " -> ", trim(x%task)
  end do

end program

! Output:
! 5 -> Make Tea
! 4 -> Feed cat
! 3 -> Clear drains
! 2 -> Tax return
! 1 -> Solve RC tasks

```



## FunL


```funl
import util.ordering
native scala.collection.mutable.PriorityQueue

data Task( priority, description )

def comparator( Task(a, _), Task(b, _) )
  | a > b     = -1
  | a < b     =  1
  | otherwise =  0

q = PriorityQueue( ordering(comparator) )

q.enqueue(
  Task(3, 'Clear drains'),
  Task(4, 'Feed cat'),
  Task(5, 'Make tea'),
  Task(1, 'Solve RC tasks'),
  Task(2, 'Tax return')
  )

while not q.isEmpty()
  println( q.dequeue() )
```


{{out}}


```txt

Task(1, Solve RC tasks)
Task(2, Tax return)
Task(3, Clear drains)
Task(4, Feed cat)
Task(5, Make tea)

```



## Go

Go's standard library contains the <code>container/heap</code> package, which which provides operations to operate as a heap any data structure that contains the <code>Push</code>, <code>Pop</code>, <code>Len</code>, <code>Less</code>, and <code>Swap</code> methods.


```go
package main

import (
    "fmt"
    "container/heap"
)

type Task struct {
    priority int
    name     string
}

type TaskPQ []Task

func (self TaskPQ) Len() int { return len(self) }
func (self TaskPQ) Less(i, j int) bool {
    return self[i].priority < self[j].priority
}
func (self TaskPQ) Swap(i, j int) { self[i], self[j] = self[j], self[i] }
func (self *TaskPQ) Push(x interface{}) { *self = append(*self, x.(Task)) }
func (self *TaskPQ) Pop() (popped interface{}) {
    popped = (*self)[len(*self)-1]
    *self = (*self)[:len(*self)-1]
    return
}

func main() {
    pq := &TaskPQ{{3, "Clear drains"},
        {4, "Feed cat"},
        {5, "Make tea"},
        {1, "Solve RC tasks"}}

    // heapify
    heap.Init(pq)

    // enqueue
    heap.Push(pq, Task{2, "Tax return"})

    for pq.Len() != 0 {
        // dequeue
        fmt.Println(heap.Pop(pq))
    }
}
```


output:

```txt

{1 Solve RC tasks}
{2 Tax return}
{3 Clear drains}
{4 Feed cat}
{5 Make tea}

```



## Groovy

Groovy can use the built in java PriorityQueue class

```groovy
import groovy.transform.Canonical

@Canonical
class Task implements Comparable<Task> {
    int priority
    String name
    int compareTo(Task o) { priority <=> o?.priority }
}

new PriorityQueue<Task>().with {
    add new Task(priority: 3, name: 'Clear drains')
    add new Task(priority: 4, name: 'Feed cat')
    add new Task(priority: 5, name: 'Make tea')
    add new Task(priority: 1, name: 'Solve RC tasks')
    add new Task(priority: 2, name: 'Tax return')

    while (!empty) { println remove() }
}
```


Output:

```txt
Task(1, Solve RC tasks)
Task(2, Tax return)
Task(3, Clear drains)
Task(4, Feed cat)
Task(5, Make tea)
```



## Haskell

One of the best Haskell implementations of priority queues (of which there are many) is [http://hackage.haskell.org/package/pqueue pqueue], which implements a binomial heap.

```haskell
import Data.PQueue.Prio.Min

main = print (toList (fromList [(3, "Clear drains"),(4, "Feed cat"),(5, "Make tea"),(1, "Solve RC tasks"), (2, "Tax return")]))
```


Although Haskell's standard library does not have a dedicated priority queue structure, one can (for most purposes) use the built-in <code>Data.Set</code> data structure as a priority queue, as long as no two elements compare equal (since Set does not allow duplicate elements). This is the case here since no two tasks should have the same name. The complexity of all basic operations is still O(log n) although that includes the "elemAt 0" function to retrieve the first element of the ordered sequence if that were required; "fromList" takes O(n log n) and "toList" takes O(n) time complexity.  Alternatively, a <code>Data.Map.Lazy</code> or <code>Data.Map.Strict</code> can be used in the same way with the same limitations.

```haskell
import qualified Data.Set as S

main = print (S.toList (S.fromList [(3, "Clear drains"),(4, "Feed cat"),(5, "Make tea"),(1, "Solve RC tasks"), (2, "Tax return")]))
```

{{out}}

```txt
[(1,"Solve RC tasks"),(2,"Tax return"),(3,"Clear drains"),(4,"Feed cat"),(5,"Make tea")]
```


Alternatively, a homemade min heap implementation:

```haskell
data MinHeap a = Nil | MinHeap { v::a, cnt::Int, l::MinHeap a, r::MinHeap a }
  deriving (Show, Eq)

hPush :: (Ord a) => a -> MinHeap a -> MinHeap a
hPush x Nil = MinHeap {v = x, cnt = 1, l = Nil, r = Nil}
hPush x h = if x < vv -- insert element, try to keep the tree balanced
  then if hLength (l h) <= hLength (r h)
    then MinHeap { v=x, cnt=cc, l=hPush vv ll, r=rr }
    else MinHeap { v=x, cnt=cc, l=ll, r=hPush vv rr }
  else if hLength (l h) <= hLength (r h)
    then MinHeap { v=vv, cnt=cc, l=hPush x ll, r=rr }
    else MinHeap { v=vv, cnt=cc, l=ll, r=hPush x rr }
  where (vv, cc, ll, rr) = (v h, 1 + cnt h, l h, r h)

hPop :: (Ord a) => MinHeap a -> (a, MinHeap a)
hPop h = (v h, pq) where -- just pop, heed not the tree balance
  pq  | l h == Nil = r h
    | r h == Nil = l h
    | v (l h) <= v (r h) = let (vv,hh) = hPop (l h) in
      MinHeap {v = vv, cnt = hLength hh + hLength (r h),
        l = hh, r = r h}
    | otherwise = let (vv,hh) = hPop (r h) in
      MinHeap {v = vv, cnt = hLength hh + hLength (l h),
        l = l h, r = hh}

hLength :: (Ord a) => MinHeap a -> Int
hLength Nil = 0
hLength h = cnt h

hFromList :: (Ord a) => [a] -> MinHeap a
hFromList = foldl (flip hPush) Nil

hToList :: (Ord a) => MinHeap a -> [a]
hToList = unfoldr f where
  f Nil = Nothing
  f h = Just $ hPop h

main = mapM_ print $ hToList $ hFromList [
  (3, "Clear drains"),
  (4, "Feed cat"),
  (5, "Make tea"),
  (1, "Solve RC tasks"),
  (2, "Tax return")]
```


The above code is a Priority Queue but isn't a [https://en.wikipedia.org/wiki/Binary_heap Min Heap based on a Binary Heap] for the following reasons:  1) it does not preserve the standard tree structure of the binary heap and 2) the tree balancing can be completely destroyed by some combinations of "pop" operations.  The following code is a true purely functional Min Heap implementation and as well implements the extra optional features of Min Heap's that it can build a new Min Heap from a list in O(n) amortized time rather than the O(n log n) amortized time (for a large number of randomly ordered entries) by simply using repeated "push" operations; as well as the standard "push", "peek", "delete" and "pop" (combines the previous two).  As well as the "fromList", "toList", and "sort" functions (the last combines the first two), it also has an "isEmpty" function to test for the empty queue, an "adjust" function that applies a function to every entry in the queue and reheapifies in O(n) amortized time and also the "replaceMin" function which is about twice as fast on the average as combined "delete" followed by "push" operations:

```haskell
data MinHeap kv = MinHeapEmpty
                  | MinHeapLeaf !kv
                  | MinHeapNode !kv {-# UNPACK #-} !Int !(MinHeap a) !(MinHeap a)
  deriving (Show, Eq)

emptyPQ :: MinHeap kv
emptyPQ = MinHeapEmpty

isEmptyPQ :: PriorityQ kv -> Bool
isEmptyPQ Mt = True
isEmptyPQ _  = False

sizePQ :: (Ord kv) => MinHeap kv -> Int
sizePQ MinHeapEmpty = 0
sizePQ (MinHeapLeaf _) = 1
sizePQ (MinHeapNode _ cnt _ _) = cnt

peekMinPQ :: MinHeap kv -> Maybe kv
peekMinPQ MinHeapEmpty = Nothing
peekMinPQ (MinHeapLeaf v) = Just v
peekMinPQ (MinHeapNode v _ _ _) = Just v

pushPQ :: (Ord kv) => kv -> MinHeap kv -> MinHeap kv
pushPQ kv pq = insert kv 0 pq where -- insert element, keeping the tree balanced
  insert kv _ MinHeapEmpty = MinHeapLeaf kv
  insert kv _ (MinHeapLeaf vv) = if kv <= vv
      then MinHeapNode kv 2 (MinHeapLeaf vv) MinHeapEmpty
      else MinHeapNode vv 2 (MinHeapLeaf kv) MinHeapEmpty
  insert kv msk (MinHeapNode vv cc ll rr) = if kv <= vv
      then if nmsk >= 0
        then MinHeapNode kv nc (insert vv nmsk ll) rr
        else MinHeapNode kv nc ll (insert vv nmsk rr)
      else if nmsk >= 0
        then MinHeapNode vv nc (insert kv nmsk ll) rr
        else MinHeapNode vv nc ll (insert kv nmsk rr)
    where nc = cc + 1
          nmsk = if msk /= 0 then msk `shiftL` 1 -- walk path to next
                 else let s = floor $ (log $ fromIntegral nc) / log 2 in
                      (nc `shiftL` ((finiteBitSize cc) - s)) .|. 1 --never 0 again

siftdown :: (Ord kv) => kv -> Int -> MinHeap kv -> MinHeap kv -> MinHeap kv
siftdown kv cnt lft rght = replace cnt lft rght where
  replace cc ll rr = case rr of -- adj to put kv in current left/right
    MinHeapEmpty -> -- means left is a MinHeapLeaf
      case ll of { (MinHeapLeaf vl) ->
        if kv <= vl
          then MinHeapNode kv 2 ll MinHeapEmpty
          else MinHeapNode vl 2 (MinHeapLeaf kv) MinHeapEmpty }
    MinHeapLeaf vr ->
      case ll of
        MinHeapLeaf vl -> if vl <= vr
          then if kv <= vl then MinHeapNode kv cc ll rr
               else MinHeapNode vl cc (MinHeapLeaf kv) rr
          else if kv <= vr then MinHeapNode kv cc ll rr
               else MinHeapNode vr cc ll (MinHeapLeaf kv)
        MinHeapNode vl ccl lll rrl -> if vl <= vr
          then if kv <= vl then MinHeapNode kv cc ll rr
               else MinHeapNode vl cc (replace ccl lll rrl) rr
          else if kv <= vr then MinHeapNode kv cc ll rr
               else MinHeapNode vr cc ll (MinHeapLeaf kv)
    MinHeapNode vr ccr llr rrr -> case ll of
      (MinHeapNode vl ccl lll rrl) -> -- right is node, so is left
        if vl <= vr then
          if kv <= vl then MinHeapNode kv cc ll rr
          else MinHeapNode vl cc (replace ccl lll rrl) rr
        else if kv <= vr then MinHeapNode kv cc ll rr
             else MinHeapNode vr cc ll (replace ccr llr rrr)

replaceMinPQ :: (Ord kv) => a -> MinHeap kv -> MinHeap kv
replaceMinPQ _ MinHeapEmpty = MinHeapEmpty
replaceMinPQ kv (MinHeapLeaf _) = MinHeapLeaf kv
replaceMinPQ kv (MinHeapNode _ cc ll rr) = siftdown kv cc ll rr where

deleteMinPQ :: (Ord kv) => MinHeap kv -> MinHeap kv
deleteMinPQ MinHeapEmpty = MinHeapEmpty -- remove min keeping tree balanced
deleteMinPQ pq = let (dkv, npq) = delete 0 pq in
                 replaceMinPQ dkv npq where
  delete _ (MinHeapLeaf vv) = (vv, MinHeapEmpty)
  delete msk (MinHeapNode vv cc ll rr) =
      if rr == MinHeapEmpty -- means left is MinHeapLeaf
        then case ll of (MinHeapLeaf vl) -> (vl, MinHeapLeaf vv)
      else if nmsk >= 0 -- means only deal with left
             then let (dv, npq) = delete nmsk ll in
                  (dv, MinHeapNode vv (cc - 1) npq rr)
             else let (dv, npq) = delete nmsk rr in
                  (dv, MinHeapNode vv (cc - 1) ll npq)
    where nmsk = if msk /= 0 then msk `shiftL` 1 -- walk path to last
                 else let s = floor $ (log $ fromIntegral cc) / log 2 in
                      (cc `shiftL` ((finiteBitSize cc) - s)) .|. 1 --never 0 again

adjustPQ :: (Ord kv) => (kv -> kv) -> MinHeap kv -> MinHeap kv
adjustPQ f pq = adjust pq where -- applies function to every element and reheapifies
  adjust MinHeapEmpty = MinHeapEmpty
  adjust (MinHeapLeaf v) = MinHeapLeaf (f v)
  adjust (MinHeapNode vv cc ll rr) = siftdown (f vv) cc (adjust ll) (adjust rr)

fromListPQ :: (Ord kv) => [kv] -> MinHeap kv
-- fromListPQ = foldl (flip pushPQ) MinHeapEmpty -- O(n log n) time; slow
fromListPQ [] = MinHeapEmpty -- O(n) time using "adjust id" which is O(n)
fromListPQ xs = let (_, pq) = build 1 xs in pq where
  sz = length xs
  szd2 = sz `div` 2
  build _ [] = ([], MinHeapEmpty)
  build lvl (x:xs') = if lvl > szd2 then (xs', MinHeapLeaf x)
                      else let nlvl = lvl + lvl in
                           let (xrl, pql) = build nlvl xs' in
                           let (xrr, pqr) = if nlvl >= sz
                                 then (xrl, MinHeapEmpty) -- no right leaf
                                 else build (nlvl + 1) xrl in
                           let cnt = sizePQ pql + sizePQ pqr + 1 in
                           (xrr, siftdown x cnt pql pqr)

popMinPQ :: (Ord kv) => MinHeap kv -> Maybe (kv, MinHeap kv)
popMinPQ pq = case peekMinPQ pq of
                Nothing -> Nothing
                Just v -> Just (v, deleteMinPQ pq)

toListPQ :: (Ord kv) => MinHeap kv -> [kv]
toListPQ = unfoldr f where
  f MinHeapEmpty = Nothing
  f pq = popMinPQ pq

sortPQ :: (Ord kv) => [kv] -> [kv]
sortPQ ls = toListPQ $ fromListPQ ls
```


If one is willing to forgo the fast O(1) "size" function and to give up strict conformance to the Heap tree structure (where rather than building each new level until each left node is full to that level before increasing level to the right, a new level is built by promoting leaves to branches only containing left leaves until all branches have left leaves before filling any right leaves of that level) although having even better tree balancing and therefore at least as high efficiency, one can use the following code adapted from the [http://www.cl.cam.ac.uk/~lp15/MLbook/programs/sample4.sml ''ML'' PRIORITY_QUEUE code by Lawrence C. Paulson] including separating the key/value pairs as separate entries in the data structure for better comparison efficiency; as noted in the code comments, a "size" function to output the number of elements in the queue (fairly quickly in O((log n)^2)), an "adjust" function to apply a function to all elements and reheapify in O(n) time, and a "merge" function to merge two queues has been added to the ML code:

```haskell
data PriorityQ k v = Mt
                     | Br !k v !(PriorityQ k v) !(PriorityQ k v)
  deriving (Eq, Ord, Read, Show)

emptyPQ :: PriorityQ k v
emptyPQ = Mt

isEmptyPQ :: PriorityQ k v -> Bool
isEmptyPQ Mt = True
isEmptyPQ _  = False

-- The size function isn't from the ML code, but an implementation was
-- suggested by Bertram Felgenhauer on Haskell Cafe, so it is included.

-- Return number of elements in the priority queue.
-- /O(log(n)^2)/
sizePQ :: PriorityQ k v -> Int
sizePQ Mt = 0
sizePQ (Br _ _ pl pr) = 2 * n + rest n pl pr where
  n = sizePQ pr
  -- rest n p q, where n = sizePQ q, and sizePQ p - sizePQ q = 0 or 1
  -- returns 1 + sizePQ p - sizePQ q.
  rest :: Int -> PriorityQ k v -> PriorityQ k v -> Int
  rest 0 Mt _ = 1
  rest 0 _  _ = 2
  rest n (Br _ _ ll lr) (Br _ _ rl rr) = case r of
      0 -> rest d ll rl -- subtree sizes: (d or d+1), d; d, d
      1 -> rest d lr rr -- subtree sizes: d+1, (d or d+1); d+1, d
    where m1 = n - 1
          d = m1 `shiftR` 1
          r = m1 .&. 1

peekMinPQ :: PriorityQ k v -> Maybe (k, v)
peekMinPQ Mt           = Nothing
peekMinPQ (Br k v _ _) = Just (k, v)

pushPQ :: Ord k => k -> v -> PriorityQ k v -> PriorityQ k v
pushPQ wk wv Mt           = Br wk wv Mt Mt
pushPQ wk wv (Br vk vv pl pr)
             | wk <= vk   = Br wk wv (pushPQ vk vv pr) pl
             | otherwise  = Br vk vv (pushPQ wk wv pr) pl

siftdown :: Ord k => k -> v -> PriorityQ k v -> PriorityQ k v -> PriorityQ k v
siftdown wk wv Mt _          = Br wk wv Mt Mt
siftdown wk wv (pl @ (Br vk vv _ _)) Mt
    | wk <= vk               = Br wk wv pl Mt
    | otherwise              = Br vk vv (Br wk wv Mt Mt) Mt
siftdown wk wv (pl @ (Br vkl vvl pll plr)) (pr @ (Br vkr vvr prl prr))
    | wk <= vkl && wk <= vkr = Br wk wv pl pr
    | vkl <= vkr             = Br vkl vvl (siftdown wk wv pll plr) pr
    | otherwise              = Br vkr vvr pl (siftdown wk wv prl prr)

replaceMinPQ :: Ord k => k -> v -> PriorityQ k v -> PriorityQ k v
replaceMinPQ wk wv Mt             = Mt
replaceMinPQ wk wv (Br _ _ pl pr) = siftdown wk wv pl pr

deleteMinPQ :: (Ord k) =>  PriorityQ k v -> PriorityQ k v
deleteMinPQ Mt             = Mt
deleteMinPQ (Br _ _ pr Mt) = pr
deleteMinPQ (Br _ _ pl pr) = let (k, v, npl) = leftrem pl in
                             siftdown k v pr npl where
  leftrem (Br k v Mt Mt)             = (k, v, Mt)
  leftrem (Br vk vv (Br k v _ _) Mt) = (k, v, Br vk vv Mt Mt)
  leftrem (Br vk vv pl pr)           = let (k, v, npl) = leftrem pl in
                                       (k, v, Br vk vv pr npl)

-- the following function has been added to the ML code to apply a function
--   to all the entries in the queue and reheapify in O(n) time
adjustPQ :: (Ord k) => (k -> v -> (k, v)) -> PriorityQ k v -> PriorityQ k v
adjustPQ f pq = adjust pq where -- applies function to every element and reheapifies
  adjust Mt               = Mt
  adjust (Br vk vv pl pr) = let (k, v) = f vk vv in
                            siftdown k v (adjust pl) (adjust pr)

fromListPQ :: (Ord k) => [(k, v)] -> PriorityQ k v
-- fromListPQ = foldl (flip pushPQ) Mt -- O(n log n) time; slow
fromListPQ [] = Mt -- O(n) time using adjust-from-bottom which is O(n)
fromListPQ xs = let (pq, _) = build (length xs) xs in pq where
  build 0 xs             = (Mt, xs)
  build lvl ((k, v):xs') = let (pl, xrl) = build (lvl `shiftR` 1) xs'
                               (pr, xrr) = build ((lvl - 1) `shiftR` 1) xrl in
                           (siftdown k v pl pr, xrr)

-- the following function has been added to merge two queues in O(m + n) time
--   where m and n are the sizes of the two queues
mergePQ :: (Ord k) => PriorityQ k v -> PriorityQ k v -> PriorityQ k v
mergePQ pq1 Mt = pq1 -- from concatenated "dumb" list
mergePQ Mt pq2 = pq2 -- in O(m + n) time where m,n are sizes pq1,pq2
mergePQ pq1 pq2 = fromListPQ (zipper pq1 $ zipper pq2 []) where
  zipper (Br wk wv Mt _) appndlst  = (wk, wv) : appndlst
  zipper (Br wk wv pl Mt) appndlst = (wk, wv) : zipper pl appndlst
  zipper (Br wk wv pl pr) appndlst = (wk, wv) : zipper pl (zipper pr appndlst)

popMinPQ :: (Ord k) => PriorityQ k v -> Maybe ((k, v), PriorityQ k v)
popMinPQ pq = case peekMinPQ pq of
                Nothing -> Nothing
                Just kv -> Just (kv, deleteMinPQ pq)

toListPQ :: (Ord k) => PriorityQ k v -> [(k, v)]
toListPQ Mt                  = [] -- unfoldr popMinPQ
toListPQ pq @ (Br vk vv _ _) = (vk, vv) : (toListPQ $ deleteMinPQ pq)

sortPQ :: (Ord k) => [(k, v)] -> [(k, v)]
sortPQ ls = toListPQ $ fromListPQ ls
```


The above codes compile but do not run with GHC Haskell version 7.8.3 using the LLVM back end with LLVM version 3.4 and full optimization turned on under Windows 32; they were tested under Windows 64 and 32 using the Native Code Generator back end with full optimization.  With GHC Haskell version 7.10.1 they compile and run with or without LLVM 3.5.1 for 32-bit Windows (64-bit GHC Haskell under Windows does not run with LLVM for version 7.10.1), with a slight execution speed advantage to using LLVM.

Min Heaps are faster than Priority Queue's based on Binomial Heaps (or Leftist or Skewed Heaps) when one mainly requires fast replacement of the head of the queue without many fresh "push" operations; Binomial Heap based versions (or Leftist or Skewed Heap based versions) are faster for merging of a series of large queues into one and for algorithms that have a lot of "push" operations of random entries.  Both have O(log n) average "push" and "pop" time complexity with O(1) for "peek", but Binomial Heap based queues (and the others) tend to be somewhat slower by a constant factor due to more complex operations.

Min Heaps are also faster than the use of balanced tree Set's or Map's where many references are made to the next element in the queue (O(1) complexity rather than O(log n)) or where frequent modification and reinsertion of the next element in the queue is required (still O(log n) but faster by a constant factor greater than two on average) and generally faster by a constant factor as operations near the top of the queue don't have to traverse the entire tree structure; O(log n) is worst case time complexity for "replace" operations not average.

The above codes when tested with the following "main" function (with a slight modification for the first test when the combined kv entry is used):

```haskell
testList = [ (3, "Clear drains"),
             (4, "Feed cat"),
             (5, "Make tea"),
             (1, "Solve RC tasks"),
             (2, "Tax return") ]

testPQ = fromListPQ testList

main = do -- slow build
  mapM_ print $ toListPQ $ foldl (\pq (k, v) -> pushPQ k v pq) emptyPQ testList
  putStrLn "" -- fast build
  mapM_ print $ toListPQ $ fromListPQ testList
  putStrLn "" -- combined fast sort
  mapM_ print $ sortPQ testList
  putStrLn "" -- test merge
  mapM_ print $ toListPQ $ mergePQ testPQ testPQ
  putStrLn "" -- test adjust
  mapM_ print $ toListPQ $ adjustPQ (\x y -> (x * (-1), y)) testPQ
```


has the output as follows:
{{output}}

```txt
(1,"Solve RC tasks")
(2,"Tax return")
(3,"Clear drains")
(4,"Feed cat")
(5,"Make tea")

(1,"Solve RC tasks")
(2,"Tax return")
(3,"Clear drains")
(4,"Feed cat")
(5,"Make tea")

(1,"Solve RC tasks")
(2,"Tax return")
(3,"Clear drains")
(4,"Feed cat")
(5,"Make tea")

(1,"Solve RC tasks")
(1,"Solve RC tasks")
(2,"Tax return")
(2,"Tax return")
(3,"Clear drains")
(3,"Clear drains")
(4,"Feed cat")
(4,"Feed cat")
(5,"Make tea")
(5,"Make tea")

(-5,"Make tea")
(-4,"Feed cat")
(-3,"Clear drains")
(-2,"Tax return")
(-1,"Solve RC tasks")
```


but the first method uses the slower way of building a queue.

=={{header|Icon}} and {{header|Unicon}}==
This solution uses classes provided by the UniLib package.
<tt>Heap</tt> is an implementation of a priority queue and
<tt>Closure</tt> is used to allow the queue to order lists based on
their first element.  The solution only works in Unicon.

```Unicon
import Utils   # For Closure class
import Collections      # For Heap (dense priority queue) class

procedure main()
   pq := Heap(, Closure("[]",Arg,1) )
   pq.add([3, "Clear drains"])
   pq.add([4, "Feed cat"])
   pq.add([5, "Make tea"])
   pq.add([1, "Solve RC tasks"])
   pq.add([2, "Tax return"])

   while task := pq.get() do write(task[1]," -> ",task[2])
end

```

Output when run:

```txt

1 -> Solve RC tasks
2 -> Tax return
3 -> Clear drains
4 -> Feed cat
5 -> Make tea

```



## J


Implementation:


```j
coclass 'priorityQueue'

PRI=: ''
QUE=: ''

insert=:4 :0
  p=. PRI,x
  q=. QUE,y
  assert. p -:&$ q
  assert. 1 = #$q
  ord=: \: p
  QUE=: ord { q
  PRI=: ord { p
  i.0 0
)

topN=:3 :0
  assert y<:#PRI
  r=. y{.QUE
  PRI=: y}.PRI
  QUE=: y}.QUE
  r
)
```


Efficiency is obtained by batching requests.  Size of batch for insert is determined by size of arguments.  Size of batch for topN is its right argument.

Example:


```j
   Q=: conew'priorityQueue'
   3 4 5 1 2 insert__Q 'clear drains';'feed cat';'make tea';'solve rc task';'tax return'
   >topN__Q 1
make tea
   >topN__Q 4
feed cat
clear drains
tax return
solve rc task
```



## Java

Java has a <code>PriorityQueue</code> class. It requires either the elements implement <code>Comparable</code>, or you give it a custom <code>Comparator</code> to compare the elements.


```java
import java.util.PriorityQueue;

class Task implements Comparable<Task> {
    final int priority;
    final String name;

    public Task(int p, String n) {
        priority = p;
        name = n;
    }

    public String toString() {
        return priority + ", " + name;
    }

    public int compareTo(Task other) {
        return priority < other.priority ? -1 : priority > other.priority ? 1 : 0;
    }

    public static void main(String[] args) {
        PriorityQueue<Task> pq = new PriorityQueue<Task>();
        pq.add(new Task(3, "Clear drains"));
        pq.add(new Task(4, "Feed cat"));
        pq.add(new Task(5, "Make tea"));
        pq.add(new Task(1, "Solve RC tasks"));
        pq.add(new Task(2, "Tax return"));

        while (!pq.isEmpty())
            System.out.println(pq.remove());
    }
}
```

{{out}}

```txt

1, Solve RC tasks
2, Tax return
3, Clear drains
4, Feed cat
5, Make tea

```



## jq

Since jq is a functional language, the priority queue must be represented explicitly as data; in the following, we use a JSON object with keys as priorities (strings). Since a given priority level may have more than task, we use arrays to hold the values.

The special key "priorities" is used to store the priorities in a sorted array. Since "sort" is fast we will use that rather than optimizing insertion in the priorities array.

We assume that if an item of a given priority is already in the priority queue, there is no need to add it again.
```jq
# In the following, pq stands for "priority queue".

# Add an item with the given priority (an integer,
# or a string representing an integer)
# Input: a pq
def pq_add(priority; item):
  (priority|tostring) as $p
  | if .priorities|index($p) then
      if (.[$p] | index(item)) then . else .[$p] += [item] end
    else .[$p] = [item] | .priorities = (.priorities + [$p] | sort)
    end ;

# emit [ item, pq ]
# Input: a pq
def pq_pop:
  .priorities as $keys
  | if ($keys|length) == 0 then [ null, . ]
    else
      if (.[$keys[0]] | length) == 1
      then .priorities =  .priorities[1:]
      else .
      end
      | [ (.[$keys[0]])[0], (.[$keys[0]] = .[$keys[0]][1:]) ]
    end ;

# Emit the item that would be popped, or null if there is none
# Input: a pq
def pq_peep:
  .priorities as $keys
  | if ($keys|length) == 0 then null
    else (.[$keys[0]])[0]
    end ;

# Add a bunch of tasks, presented as an array of arrays
# Input: a pq
def pq_add_tasks(list):
  reduce list[] as $pair (.; . + pq_add( $pair[0]; $pair[1]) ) ;

# Pop all the tasks, producing a stream
# Input: a pq
def pq_pop_tasks:
  pq_pop as $pair
  | if $pair[0] == null then empty
    else $pair[0], ( $pair[1] | pq_pop_tasks )
    end ;

# Input: a bunch of tasks, presented as an array of arrays
def prioritize:
  . as $list | {} | pq_add_tasks($list) | pq_pop_tasks ;

```

The specific task:

```jq

[ [3,     "Clear drains"],
  [4,     "Feed cat"],
  [5,     "Make tea"],
  [1,     "Solve RC tasks"],
  [2,     "Tax return"]
 ] | prioritize

```

{{Out}}
 "Solve RC tasks"
 "Tax return"
 "Clear drains"
 "Feed cat"
 "Make tea"


## Julia

Julia has built-in support for priority queues, though the <code>PriorityQueue</code> type is not exported by default.  Priority queues are a specialization of the <code>Dictionary</code> type having ordered values, which serve as the priority.  In addition to all of the methods of standard dictionaries, priority queues support: <code>enqueue!</code>, which adds an item to the queue, <code>dequeue!</code> which removes the lowest priority item from the queue, returning its key, and <code>peek</code>, which returns the (key, priority) of the lowest priority entry in the queue.  The ordering behavior of the queue, which by default is its value sort order (typically low to high), can be set by passing an order directive to its constructor.  For this task, <code>Base.Order.Reverse</code> is used to set-up the <code>task</code> queue to return tasks from high to low priority.

```Julia

using Base.Collections

test = ["Clear drains" 3;
        "Feed cat" 4;
        "Make tea" 5;
        "Solve RC tasks" 1;
        "Tax return" 2]

task = PriorityQueue(Base.Order.Reverse)
for i in 1:size(test)[1]
    enqueue!(task, test[i,1], test[i,2])
end

println("Tasks, completed according to priority:")
while !isempty(task)
    (t, p) = peek(task)
    dequeue!(task)
    println("    \"", t, "\" has priority ", p)
end
```


{{out}}

```txt
Tasks, completed according to priority:
    "Make tea" has priority 5
    "Feed cat" has priority 4
    "Clear drains" has priority 3
    "Tax return" has priority 2
    "Solve RC tasks" has priority 1

```



## Kotlin

{{trans|Java}}

```scala
import java.util.PriorityQueue

internal data class Task(val priority: Int, val name: String) : Comparable<Task> {
    override fun compareTo(other: Task) = when {
        priority < other.priority -> -1
        priority > other.priority -> 1
        else -> 0
    }
}

private infix fun String.priority(priority: Int) = Task(priority, this)

fun main(args: Array<String>) {
    val q = PriorityQueue(listOf("Clear drains" priority 3,
                                 "Feed cat" priority 4,
                                 "Make tea" priority 5,
                                 "Solve RC tasks" priority 1,
                                 "Tax return" priority 2))
    while (q.any()) println(q.remove())
}
```

{{out}}

```txt
Task(priority=1, name=Solve RC tasks)
Task(priority=2, name=Tax return)
Task(priority=3, name=Clear drains)
Task(priority=4, name=Feed cat)
Task(priority=5, name=Make tea)
```



## Lasso


```lasso>define priorityQueue =
 type {
    data
        store        = map,
        cur_priority = void

    public push(priority::integer, value) => {
        local(store) = .`store`->find(#priority)

        if(#store->isA(::array)) => {
            #store->insert(#value)
            return
        }
        .`store`->insert(#priority=array(#value))

        .`cur_priority`->isA(::void) or #priority < .`cur_priority`
            ? .`cur_priority` = #priority
    }

    public pop => {
        .`cur_priority` == void
            ? return void

        local(store)  = .`store`->find(.`cur_priority`)
        local(retVal) =  #store->first

        #store->removeFirst&size > 0
            ? return #retVal

        // Need to find next priority
        .`store`->remove(.`cur_priority`)

        if(.`store`->size == 0) => {
            .`cur_priority` = void
        else
            // There are better / faster ways to do this
            // The keys are actually already sorted, but the order of
            // storage in a map is not actually defined, can't rely on it
            .`cur_priority` = .`store`->keys->asArray->sort&first
        }

        return #retVal
    }

    public isEmpty => (.`store`->size == 0)

}

local(test) = priorityQueue

#test->push(2,`e`)
#test->push(1,`H`)
#test->push(5,`o`)
#test->push(2,`l`)
#test->push(5,`!`)
#test->push(4,`l`)

while(not #test->isEmpty) => {
    stdout(#test->pop)
}
```


{{out}}

```txt
Hello!
```



## Lua


This implementation uses a table with priorities as keys and queues as values. Queues for each priority are created when putting items as needed and are shrunk as necessary when popping items and removed when they are empty. Instead of using a plain array table for each queue, the technique shown in the Lua implementation from the [[Queue/Definition#Lua | Queue]] task is used. This avoids having to use <code>table.remove(t, 1)</code> to get and remove the first queue element, which is rather slow for big tables.


```lua
PriorityQueue = {
    __index = {
        put = function(self, p, v)
            local q = self[p]
            if not q then
                q = {first = 1, last = 0}
                self[p] = q
            end
            q.last = q.last + 1
            q[q.last] = v
        end,
        pop = function(self)
            for p, q in pairs(self) do
                if q.first <= q.last then
                    local v = q[q.first]
                    q[q.first] = nil
                    q.first = q.first + 1
                    return p, v
                else
                    self[p] = nil
                end
            end
        end
    },
    __call = function(cls)
        return setmetatable({}, cls)
    end
}

setmetatable(PriorityQueue, PriorityQueue)

-- Usage:
pq = PriorityQueue()

tasks = {
    {3, 'Clear drains'},
    {4, 'Feed cat'},
    {5, 'Make tea'},
    {1, 'Solve RC tasks'},
    {2, 'Tax return'}
}

for _, task in ipairs(tasks) do
    print(string.format("Putting: %d - %s", unpack(task)))
    pq:put(unpack(task))
end

for prio, task in pq.pop, pq do
    print(string.format("Popped: %d - %s", prio, task))
end
```


'''Output:'''

    Putting: 3 - Clear drains
    Putting: 4 - Feed cat
    Putting: 5 - Make tea
    Putting: 1 - Solve RC tasks
    Putting: 2 - Tax return
    Popped: 1 - Solve RC tasks
    Popped: 2 - Tax return
    Popped: 3 - Clear drains
    Popped: 4 - Feed cat
    Popped: 5 - Make tea

The implementation is faster than the Python implementations below using <code>queue.PriorityQueue</code> or <code>heapq</code>, even when comparing the standard Lua implementation against [[PyPy]] and millions of tasks are added to the queue. With LuaJIT it is yet faster. The following code measures the time needed to add 10<sup>7</sup> tasks with a random priority between 1 and 1000 and to retrieve them from the queue again in order.


```lua
-- Use socket.gettime() for benchmark measurements
-- since it has millisecond precision on most systems
local socket = require("socket")

n = 10000000 -- number of tasks added (10^7)
m = 1000     -- number different priorities

local pq = PriorityQueue()

print(string.format("Adding %d tasks with random priority 1-%d ...", n, m))
start = socket.gettime()

for i = 1, n do
    pq:put(math.random(m), i)
end

print(string.format("Elapsed: %.3f ms.", (socket.gettime() - start) * 1000))

print("Retrieving all tasks in order...")
start = socket.gettime()

local pp = 0
local pv = 0

for i = 1, n do
    local p, task = pq:pop()

    -- check that tasks are popped in ascending priority
    assert(p >= pp)

    if pp == p then
        -- check that tasks within one priority maintain the insertion order
        assert(task > pt)
    end

    pp = p
    pt = task
end

print(string.format("Elapsed: %.3f ms.", (socket.gettime() - start) * 1000))
```



## M2000 Interpreter

For these three examples, we can use same priorities, so if a priority exist then the new insertion not alter the top item (which we pop or peek from queue).


### Using unordered array


```M2000 Interpreter

Module UnOrderedArray {
      Class PriorityQueue {
      Private:
            Dim Item()
            many=0, level=0, first
            cmp = lambda->0
            Module Reduce {
                  if .many<.first*2 then exit
                  If .level<.many/2 then .many/=2 : Dim .Item(.many)
            }
      Public:
            Module Clear {
              Dim .Item() \\ erase all
              .many<=0 \\ default
              .Level<=0
            }
            Module PriorityQueue {
                  If .many>0 then Error "Clear List First"
                  Read .many, .cmp
                  .first<=.many
                  Dim .Item(.many)
            }
            Module Add {
                 If .level=.many Then {
                       If .many=0 then Error "Define Size First"
                        Dim .Item(.many*2)
                        .many*=2
                 }
                 Read Item
                 If .level=0 Then {
                       .Item(0)=Item
                 } Else.if .cmp(.Item(0), Item)=-1 Then { \\ Item is max
                       .Item(.level)=Item
                       swap .Item(0), .Item(.level)
                 } Else .Item(.level)=Item
                 .level++
            }
            Function Peek {
                  If .level=0 Then error "empty"
                  =.Item(0)
            }
            Function Poll {
                  If .level=0 Then error "empty"
                  =.Item(0)
                  If .level=2 Then {
                  swap .Item(0), .Item(1)
                  .Item(1)=0
                  .Level<=1
                  } Else.If .level>2 Then {
                        .Level--
                        Swap .Item(.level), .Item(0)
                        .Item(.level)=0
                        For I=.level-1 to 1 {
                              If .cmp(.Item(I), .Item(I-1))=1 Then Swap .Item(I), .Item(I-1)
                        }
                  } else .level<=0 : .Item(0)=0
                  .Reduce
            }
            Module Remove {
                  If .level=0 Then error "empty"
                  Read Item
                  k=true
                  If .cmp(.Item(0), Item)=0 Then {
                        Item=.Poll()
                        K~  \\ k=false
                  } Else.If .Level>1 Then {
                        I2=.Level-1
                            For I=1 to I2 {
                                    If k Then {
                                           If .cmp(.Item(I), Item)=0 Then {
                                                 If I<I2 Then Swap .Item(I), .Item(I2)
                                                 .Item(I2)=0
                                                 k=false
                                           }
                                    } else exit
                              }
                       .Level--
                  }
                  If k Then Error "Not Found"
                  .Reduce
            }
            Function Size {
                  If .many=0 then Error "Define Size First"
                  =.Level
            }
      }

      Class Item { X, S$
            Module Item { Read .X, .S$}
      }
      Function PrintTop {
            M=Queue.Peek() : Print "Item ";M.X, M.S$
      }
      Comp=Lambda -> { Read A,B : =COMPARE(A.X,B.X)}

      Queue=PriorityQueue(100,Comp)
      Queue.Add Item(3, "Clear drains")
      Call Local PrintTop()
      Queue.Add Item(4  ,"Feed cat")
      Call Local PrintTop()
      Queue.Add Item(5  ,"Make tea")
      Call Local PrintTop()
      Queue.Add Item(1  ,"Solve RC tasks")
      Call Local PrintTop()
      Queue.Add Item(2  ,"Tax return")
      Call Local PrintTop()
      Print "remove items"
      While true {
            MM=Queue.Poll()
            Print MM.X, MM.S$
            Print "Size="; Queue.Size()
            If Queue.Size()=0 Then exit
            Call Local PrintTop()
      }
}
UnOrderedArray

```



### Using a stack with arrays as elements

Every insertion push item using binary search in proper position. Pop is very fast.

```M2000 Interpreter

Module PriorityQueue {
      a= ( (3, "Clear drains"), (4 ,"Feed cat"), ( 5 , "Make tea"), ( 1 ,"Solve RC tasks"), ( 2 , "Tax return"))
      b=stack
      comp=lambda (a, b) ->{
            =array(a, 0)<array(b, 0)
      }
      module InsertPQ (a, n, &comp) {
            if len(a)=0 then stack a {data n} : exit
            if comp(n, stackitem(a)) then stack a {push n} : exit
             stack a {
                  push n
                  t=2: b=len(a)
                   m=b
                   while t<=b {
                         t1=m
                        m=(b+t) div 2
                        if m=0 then  m=t1 : exit
                        If comp(stackitem(m),n) then t=m+1:  continue
                        b=m-1
                        m=b
                  }
                  if m>1 then shiftback m
            }
      }

      n=each(a)
      while n {
            InsertPq b, array(n), &comp
      }

      n1=each(b)
      while n1 {
            m=stackitem(n1)
            Print array(m, 0), array$(m, 1)
      }

      \\ Peek topitem (without popping)
      Print Array$(stackitem(b), 1)
      \\ Pop item
      Stack b {
            Read old
      }
      Print Array$(old, 1)
      Function Peek$(a) {=Array$(stackitem(a), 1)}
      Function Pop$(a) {
            stack a {
                  =Array$(stackitem(), 1)
                   drop
            }
      }
      Print Peek$(b)
      Print Pop$(b)
      Function IsEmpty(a) {
        =len(a)=0
      }
      While not IsEmpty(b) {
            Print pop$(b)
      }
}
PriorityQueue


```



### Using a stack with Groups as elements

This is the same as previous but now we use a group (a user object for M2000). InsertPQ is the same as before. Lambda comp has change only. We didn't use pointers to groups. All groups here works as values, so when we get a peek we get a copy of group in top position. All members of a group may not values, so if we have a pointer to group then we get a copy of that pointer, but then we can make changes and that changes happen for the group which we get the copy.


```M2000 Interpreter

Module PriorityQueueForGroups {
      class obj {
            x, s$
            class:
            module obj (.x, .s$) {}
      }
      Flush  ' empty current stack
      Data obj(3, "Clear drains"), obj(4 ,"Feed cat"), obj( 5 , "Make tea"), obj( 1 ,"Solve RC tasks"), obj( 2 , "Tax return")
      b=stack
      comp=lambda (a, b) ->{
                  =a.x<b.x
      }
      module InsertPQ (a, n, &comp) {
            if len(a)=0 then stack a {data n} : exit
            if comp(n, stackitem(a)) then stack a {push n} : exit
             stack a {
                  push n
                  t=2: b=len(a)
                   m=b
                   while t<=b {
                         t1=m
                        m=(b+t) div 2
                        if m=0 then  m=t1 : exit
                        If comp(stackitem(m),n) then t=m+1:  continue
                        b=m-1
                        m=b
                  }
                  if m>1 then shiftback m
            }
      }
      b=stack
      While not empty {
            InsertPQ b, Group, &comp    ' Group pop a group from current stack
      }
       n1=each(b)
      while n1 {
            m=stackitem(n1)
            Print m.x, m.s$
      }
      Function Peek$(a) {m=stackitem(a) : =m.s$}
      Print Peek$(b)

      Function Pop$(a) {
            stack a {
                  m=stackitem()
                  =m.s$
                   drop
            }
      }
      Function IsEmpty(a) {
              =len(a)=0
      }
      While not isEmpty(b) {
            Print Pop$(b)
      }
}
PriorityQueueForGroups

```



## Mathematica


```mathematica
push = Function[{queue, priority, item},
   queue = SortBy[Append[queue, {priority, item}], First], HoldFirst];
pop = Function[queue,
   If[Length@queue == 0, Null,
    With[{item = queue[[-1, 2]]}, queue = Most@queue; item]],
   HoldFirst];
peek = Function[queue,
   If[Length@queue == 0, Null, Max[queue[[All, 1]]]], HoldFirst];
merge = Function[{queue1, queue2},
   SortBy[Join[queue1, queue2], First], HoldAll];
```


Example:


```mathematica
queue = {};
push[queue, 3, "Clear drains"];
push[queue, 4, "Feed cat"];
push[queue, 5, "Make tea"];
push[queue, 1, "Solve RC tasks"];
push[queue, 2, "Tax return"];
Print[peek[queue]];
Print[pop[queue]];
queue1 = {};
push[queue1, 6, "Drink tea"];
Print[merge[queue, queue1]];
```


Output:


```txt
5

Make tea

{{1,Solve RC tasks},{2,Tax return},{3,Clear drains},{4,Feed cat},{6,Drink tea}}
```



## Maxima


```maxima
/* Naive implementation using a sorted list of pairs [key, [item[1], ..., item[n]]].
The key may be any number (integer or not). Items are extracted in FIFO order. */

defstruct(pqueue(q = []))$

/* Binary search */

find_key(q, p) := block(
   [i: 1, j: length(q), k, c],
   if j = 0 then false
   elseif (c: q[i][1]) >= p then
      (if c = p then i else false)
   elseif (c: q[j][1]) <= p then
      (if c = p then j else false)
   else catch(
      while j >= i do (
         k: quotient(i + j, 2),
         if (c: q[k][1]) = p then throw(k)
         elseif c < p then i: k + 1 else j: k - 1
      ),
      false
   )
)$

pqueue_push(pq, x, p) := block(
   [q: pq@q, k],
   k: find_key(q, p),
   if integerp(k) then q[k][2]: endcons(x, q[k][2])
   else pq@q: sort(cons([p, [x]], q)),
   'done
)$

pqueue_pop(pq) := block(
   [q: pq@q, v, x],
   if emptyp(q) then 'fail else (
      p: q[1][1],
      v: q[1][2],
      x: v[1],
      if length(v) > 1 then q[1][2]: rest(v) else pq@q: rest(q),
      x
   )
)$

pqueue_print(pq) := block([t], while (t: pqueue_pop(pq)) # 'fail do disp(t))$


/* An example */

a: new(pqueue)$

pqueue_push(a, "take milk", 4)$
pqueue_push(a, "take eggs", 4)$
pqueue_push(a, "take wheat flour", 4)$
pqueue_push(a, "take salt", 4)$
pqueue_push(a, "take oil", 4)$
pqueue_push(a, "carry out crepe recipe", 5)$
pqueue_push(a, "savour !", 6)$
pqueue_push(a, "add strawberry jam", 5 + 1/2)$
pqueue_push(a, "call friends", 5 + 2/3)$
pqueue_push(a, "go to the supermarket and buy food", 3)$
pqueue_push(a, "take a shower", 2)$
pqueue_push(a, "get dressed", 2)$
pqueue_push(a, "wake up", 1)$
pqueue_push(a, "serve cider", 5 + 3/4)$
pqueue_push(a, "buy also cider", 3)$

pqueue_print(a);
"wake up"
"take a shower"
"get dressed"
"go to the supermarket and buy food"
"buy also cider"
"take milk"
"take butter"
"take flour"
"take salt"
"take oil"
"carry out recipe"
"add strawberry jam"
"call friends"
"serve cider"
"savour !"
```



## Mercury

Mercury comes with an efficient, albeit simple, priority queue in its standard library.  The build_pqueue/2 predicate in the code below inserts the test data in arbitrary order.  display_pqueue/3, in turn, removes one K/V pair at a time, displaying the value.  Compiling and running the supplied program results in all tasks being displayed in priority order as expected.


```mercury
:- module test_pqueue.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module pqueue.
:- import_module string.

:- pred build_pqueue(pqueue(int,string)::in, pqueue(int,string)::out) is det.
build_pqueue(!PQ) :-
  pqueue.insert(3, "Clear drains",   !PQ),
  pqueue.insert(4, "Feed cat",       !PQ),
  pqueue.insert(5, "Make tea",       !PQ),
  pqueue.insert(1, "Solve RC tasks", !PQ),
  pqueue.insert(2, "Tax return",     !PQ).

:- pred display_pqueue(pqueue(int, string)::in, io::di, io::uo) is det.
display_pqueue(PQ, !IO) :-
  ( pqueue.remove(K, V, PQ, PQO) ->
      io.format("Key = %d, Value = %s\n", [i(K), s(V)], !IO),
      display_pqueue(PQO, !IO)
  ;
      true
  ).

main(!IO) :-
  build_pqueue(pqueue.init, PQO),
  display_pqueue(PQO, !IO).
```



## Nim

{{trans|C}}

```nim
type
  PriElem[T] = tuple
    data: T
    pri: int

  PriQueue[T] = object
    buf: seq[PriElem[T]]
    count: int

# first element not used to simplify indices
proc initPriQueue[T](initialSize = 4): PriQueue[T] =
  result.buf.newSeq(initialSize)
  result.buf.setLen(1)
  result.count = 0

proc add[T](q: var PriQueue[T], data: T, pri: int) =
  var n = q.buf.len
  var m = n div 2
  q.buf.setLen(n + 1)

  # append at end, then up heap
  while m > 0 and pri < q.buf[m].pri:
    q.buf[n] = q.buf[m]
    n = m
    m = m div 2

  q.buf[n] = (data, pri)
  q.count = q.buf.len - 1

proc pop[T](q: var PriQueue[T]): PriElem[T] =
  assert q.buf.len > 1
  result = q.buf[1]

  var qn = q.buf.len - 1
  var n = 1
  var m = 2
  while m < qn:
    if m + 1 < qn and q.buf[m].pri > q.buf[m+1].pri:
      inc m

    if q.buf[qn].pri <= q.buf[m].pri:
      break

    q.buf[n] = q.buf[m]
    n = m
    m = m * 2

  q.buf[n] = q.buf[qn]
  q.buf.setLen(q.buf.len - 1)
  q.count = q.buf.len - 1

var p = initPriQueue[string]()
p.add("Clear drains", 3)
p.add("Feed cat", 4)
p.add("Make tea", 5)
p.add("Solve RC tasks", 1)
p.add("Tax return", 2)

while p.count > 0:
  echo p.pop()
```

{{out}}

```txt
(data: Solve RC tasks, pri: 1)
(data: Tax return, pri: 2)
(data: Clear drains, pri: 3)
(data: Feed cat, pri: 4)
(data: Make tea, pri: 5)
```


''' Using Nim HeapQueue'''

```Nim
import HeapQueue

var pq = newHeapQueue[(int, string)]()

pq.push((3, "Clear drains"))
pq.push((4, "Feed cat"))
pq.push((5, "Make tea"))
pq.push((1, "Solve RC tasks"))
pq.push((2, "Tax return"))

while pq.len() > 0:
    echo pq.pop()
```


{{out}}

```txt
(Field0: 1, Field1: "Solve RC tasks")
(Field0: 2, Field1: "Tax return")
(Field0: 3, Field1: "Clear drains")
(Field0: 4, Field1: "Feed cat")
(Field0: 5, Field1: "Make tea")
```


''' Using Nim tables'''

```Nim
import tables

var
  pq = initTable[int, string]()

proc main() =
  pq.add(3, "Clear drains")
  pq.add(4, "Feed cat")
  pq.add(5, "Make tea")
  pq.add(1, "Solve RC tasks")
  pq.add(2, "Tax return")

  for i in countUp(1,5):
    if pq.hasKey(i):
      echo i, ": ", pq[i]
      pq.del(i)

main()
```

{{out}}

```txt
1: Solve RC tasks
2: Tax return
3: Clear drains
4: Feed cat
5: Make tea
```


=={{header|Objective-C}}==
{{works with|Cocoa}}
The priority queue used in this example is not actually written in Objective-C. It is part of Apple's (C-based) Core Foundation library, which is included with in Cocoa on Mac OS X and iOS. Its interface is a C function interface, which makes the code very ugly. Core Foundation is not included in GNUStep or other Objective-C APIs.


```objc>#import <Foundation/Foundation.h


const void *PQRetain(CFAllocatorRef allocator, const void *ptr) {
  return (__bridge_retained const void *)(__bridge id)ptr;
}
void PQRelease(CFAllocatorRef allocator, const void *ptr) {
  (void)(__bridge_transfer id)ptr;
}
CFComparisonResult PQCompare(const void *ptr1, const void *ptr2, void *unused) {
  return [(__bridge id)ptr1 compare:(__bridge id)ptr2];
}

@interface Task : NSObject {
  int priority;
  NSString *name;
}
- (instancetype)initWithPriority:(int)p andName:(NSString *)n;
- (NSComparisonResult)compare:(Task *)other;
@end

@implementation Task
- (instancetype)initWithPriority:(int)p andName:(NSString *)n {
  if ((self = [super init])) {
    priority = p;
    name = [n copy];
  }
  return self;
}
- (NSString *)description {
  return [NSString stringWithFormat:@"%d, %@", priority, name];
}
- (NSComparisonResult)compare:(Task *)other {
  if (priority == other->priority)
    return NSOrderedSame;
  else if (priority < other->priority)
    return NSOrderedAscending;
  else
    return NSOrderedDescending;
}
@end

int main (int argc, const char *argv[]) {
  @autoreleasepool {

    CFBinaryHeapCallBacks callBacks = {0, PQRetain, PQRelease, NULL, PQCompare};
    CFBinaryHeapRef pq = CFBinaryHeapCreate(NULL, 0, &callBacks, NULL);

    CFBinaryHeapAddValue(pq, [[Task alloc] initWithPriority:3 andName:@"Clear drains"]);
    CFBinaryHeapAddValue(pq, [[Task alloc] initWithPriority:4 andName:@"Feed cat"]);
    CFBinaryHeapAddValue(pq, [[Task alloc] initWithPriority:5 andName:@"Make tea"]);
    CFBinaryHeapAddValue(pq, [[Task alloc] initWithPriority:1 andName:@"Solve RC tasks"]);
    CFBinaryHeapAddValue(pq, [[Task alloc] initWithPriority:2 andName:@"Tax return"]);

    while (CFBinaryHeapGetCount(pq) != 0) {
      Task *task = (id)CFBinaryHeapGetMinimum(pq);
      NSLog(@"%@", task);
      CFBinaryHeapRemoveMinimumValue(pq);
    }

    CFRelease(pq);

  }
  return 0;
}
```


log:

```txt

2011-08-22 07:46:19.250 Untitled[563:903] 1, Solve RC tasks
2011-08-22 07:46:19.255 Untitled[563:903] 2, Tax return
2011-08-22 07:46:19.256 Untitled[563:903] 3, Clear drains
2011-08-22 07:46:19.257 Untitled[563:903] 4, Feed cat
2011-08-22 07:46:19.258 Untitled[563:903] 5, Make tea

```



## OCaml


Holger Arnold's [http://holgerarnold.net/software/ OCaml base library] provides a [http://holgerarnold.net/software/ocaml/doc/base/PriorityQueue.html PriorityQueue] module.


```ocaml
module PQ = Base.PriorityQueue

let () =
  let tasks = [
    3, "Clear drains";
    4, "Feed cat";
    5, "Make tea";
    1, "Solve RC tasks";
    2, "Tax return";
  ] in
  let pq = PQ.make (fun (prio1, _) (prio2, _) -> prio1 > prio2) in
  List.iter (PQ.add pq) tasks;
  while not (PQ.is_empty pq) do
    let _, task = PQ.first pq in
    PQ.remove_first pq;
    print_endline task
  done
```


testing:

```txt
$ ocaml -I +pcre pcre.cma base.cma pq.ml
Make tea
Feed cat
Clear drains
Tax return
Solve RC tasks
```


Although OCaml's standard library does not have a dedicated priority queue structure, one can (for most purposes) use the built-in Set data structure as a priority queue, as long as no two elements compare equal (since Set does not allow duplicate elements). This is the case here since no two tasks should have the same name. Note that Set is a functional, persistent data structure, so we derive new priority queues from the old ones functionally, rather than modifying them imperatively; the complexity is still O(log n).
{{works with|OCaml|4.02+}}

```ocaml
module PQSet = Set.Make
  (struct
     type t = int * string (* pair of priority and task name *)
     let compare = compare
   end);;

let () =
  let tasks = [
    3, "Clear drains";
    4, "Feed cat";
    5, "Make tea";
    1, "Solve RC tasks";
    2, "Tax return";
  ] in
  let pq = PQSet.of_list tasks in
  let rec aux pq' =
    if not (PQSet.is_empty pq') then begin
      let prio, name as task = PQSet.min_elt pq' in
      Printf.printf "%d, %s\n" prio name;
      aux (PQSet.remove task pq')
    end
  in aux pq
```

{{out}}

```txt

1, Solve RC tasks
2, Tax return
3, Clear drains
4, Feed cat
5, Make tea

```



## Perl

There are a few implementations on CPAN.  Following uses <code>Heap::Priority</code>[http://search.cpan.org/~fwojcik/Heap-Priority-0.11/Priority.pm]

```perl
use 5.10.0;
use strict;
use Heap::Priority;

my $h = new Heap::Priority;

$h->highest_first(); # higher or lower number is more important
$h->add(@$_) for ["Clear drains",   3],
     ["Feed cat",     4],
     ["Make tea",     5],
     ["Solve RC tasks", 1],
     ["Tax return",     2];

say while ($_ = $h->pop);
```
output<lang>Make tea
Feed cat
Clear drains
Tax return
Solve RC tasks
```



## Perl 6

This is a rather simple implementation. It requires the priority to be a positive integer value, with lower values being higher priority. There isn't a hard limit on how many priority levels you can have, though more than a few dozen is probably not practical.

The tasks are stored internally as an array of FIFO buffers, so multiple tasks of the same priority level will be returned in the order they were stored.


```perl6
class PriorityQueue {
    has @!tasks;

    method insert (Int $priority where * >= 0, $task) {
        @!tasks[$priority].push: $task;
    }

    method get { @!tasks.first(?*).shift }

    method is-empty { ?none @!tasks }
}

my $pq = PriorityQueue.new;

for (
    3, 'Clear drains',
    4, 'Feed cat',
    5, 'Make tea',
    9, 'Sleep',
    3, 'Check email',
    1, 'Solve RC tasks',
    9, 'Exercise',
    2, 'Do taxes'
) -> $priority, $task {
    $pq.insert( $priority, $task );
}

say $pq.get until $pq.is-empty;
```


{{out}}

```txt
Solve RC tasks
Do taxes
Clear drains
Check email
Feed cat
Make tea
Sleep
Exercise
```



## Phix

Dictionary based solution. Allows duplicate tasks, FIFO within priority, and uses a callback-style method of performing tasks.

Assumes 5 is the highest priority and should be done first, for 1 first just delete the ",true" on traverse_dict calls.

```Phix
integer tasklist = new_dict()

procedure add_task(integer priority, string desc)
    integer k = getd_index(priority,tasklist)
    if k=0 then
        putd(priority,{desc},tasklist)
    else
        sequence descs = getd_by_index(k,tasklist)
        putd(priority,append(descs,desc),tasklist)
    end if
end procedure

function list_task_visitor(integer priority, sequence descs, integer /*user_data*/)
    ?{priority,descs}
    return 1
end function

procedure list_tasks()
    traverse_dict(routine_id("list_task_visitor"), 0, tasklist,true)
end procedure

function pop_task_visitor(integer priority, sequence descs, integer rid)
    string desc = descs[1]
    descs = descs[2..$]
    if length(descs)=0 then
        deld(priority,tasklist)
    else
        putd(priority,descs,tasklist)
    end if
    call_proc(rid,{priority,desc})
    return 0
end function

procedure pop_task(integer rid)
    if dict_size(tasklist)!=0 then
        traverse_dict(routine_id("pop_task_visitor"), rid, tasklist,true)
    end if
end procedure

add_task(3,"Clear drains")
add_task(4,"Feed cat")
add_task(5,"Make tea")
add_task(1,"Solve RC tasks")
add_task(2,"Tax return")

procedure do_task(integer priority, string desc)
    ?{priority,desc}
end procedure

list_tasks()
?"==="
pop_task(routine_id("do_task"))
?"==="
list_tasks()
```

{{out}}

```txt

{5,{"Make tea"}}
{4,{"Feed cat"}}
{3,{"Clear drains"}}
{2,{"Tax return"}}
{1,{"Solve RC tasks"}}
"==="
{5,"Make tea"}
"==="
{4,{"Feed cat"}}
{3,{"Clear drains"}}
{2,{"Tax return"}}
{1,{"Solve RC tasks"}}

```


{{trans|Nim}}
(I needed this for [[Taxicab_numbers#Phix|Taxicab_numbers]])

The bulk of this code now forms builtins/pqueue.e (not yet properly documented)

```Phix
sequence pq = {}

constant PRIORITY = 2

procedure pq_add(sequence item)
-- item is {object data, object priority}
    integer n = length(pq)+1,
            m = floor(n/2)
    pq &= 0
    -- append at end, then up heap
    while m>0 and item[PRIORITY]<pq[m][PRIORITY] do
        pq[n] = pq[m]
        n = m
        m = floor(m/2)
    end while
    pq[n] = item
end procedure

function pq_pop()
    sequence result = pq[1]

    integer qn = length(pq),
            n = 1,
            m = 2
    while m<qn do
        if m+1<qn and pq[m][PRIORITY]>pq[m+1][PRIORITY] then
            m += 1
        end if
        if pq[qn][PRIORITY]<=pq[m][PRIORITY] then exit end if
        pq[n] = pq[m]
        n = m
        m = m * 2
    end while
    pq[n] = pq[qn]
    pq = pq[1..$-1]
    return result
end function

constant set = shuffle({{"Clear drains", 3},
                        {"Feed cat", 4},
                        {"Make tea", 5},
                        {"Solve RC tasks", 1},
                        {"Tax return", 2}})
for i=1 to length(set) do
    pq_add(set[i])
    pq_add(set[rand(length(set))])
end for

while length(pq) do
    ?pq_pop()
end while
```

{{out}}
(with an initial set_rand(iff(machine_bits()=32?654:26)) to make it slightly more amusing)

```txt

{"Solve RC tasks",1}
{"Tax return",2}
{"Tax return",2}
{"Clear drains",3}
{"Feed cat",4}
{"Feed cat",4}
{"Feed cat",4}
{"Feed cat",4}
{"Feed cat",4}
{"Make tea",5}

```



## PHP

{{works with|PHP|5.3+}}
PHP's <code>SplPriorityQueue</code> class implements a max-heap. PHP also separately has <code>SplHeap</code>, <code>SplMinHeap</code>, and <code>SplMaxHeap</code> classes.

```php
<?php
$pq = new SplPriorityQueue;

$pq->insert('Clear drains', 3);
$pq->insert('Feed cat', 4);
$pq->insert('Make tea', 5);
$pq->insert('Solve RC tasks', 1);
$pq->insert('Tax return', 2);

// This line causes extract() to return both the data and priority (in an associative array),
// Otherwise it would just return the data
$pq->setExtractFlags(SplPriorityQueue::EXTR_BOTH);

while (!$pq->isEmpty()) {
    print_r($pq->extract());
}
?>
```


Output:

```txt

Array
(
    [data] => Make tea
    [priority] => 5
)
Array
(
    [data] => Feed cat
    [priority] => 4
)
Array
(
    [data] => Clear drains
    [priority] => 3
)
Array
(
    [data] => Tax return
    [priority] => 2
)
Array
(
    [data] => Solve RC tasks
    [priority] => 1
)

```


{{works with|PHP|5.3+}}
The difference between <code>SplHeap</code> and <code>SplPriorityQueue</code> is that <code>SplPriorityQueue</code> takes the data and the priority as two separate arguments, and the comparison is only made on the priority; whereas <code>SplHeap</code> takes only one argument (the element), and the comparison is made on that directly. In all of these classes it is possible to provide a custom comparator by subclassing the class and overriding its <code>compare</code> method.

```php
<?php
$pq = new SplMinHeap;

$pq->insert(array(3, 'Clear drains'));
$pq->insert(array(4, 'Feed cat'));
$pq->insert(array(5, 'Make tea'));
$pq->insert(array(1, 'Solve RC tasks'));
$pq->insert(array(2, 'Tax return'));

while (!$pq->isEmpty()) {
    print_r($pq->extract());
}
?>
```


Output:

```txt

Array
(
    [0] => 1
    [1] => Solve RC tasks
)
Array
(
    [0] => 2
    [1] => Tax return
)
Array
(
    [0] => 3
    [1] => Clear drains
)
Array
(
    [0] => 4
    [1] => Feed cat
)
Array
(
    [0] => 5
    [1] => Make tea
)

```



## PicoLisp

The following implementation imposes no limits. It uses a [http://software-lab.de/doc/refI.html#idx binary tree] for storage. The priority levels may be numeric, or of any other type.

```PicoLisp
# Insert item into priority queue
(de insertPQ (Queue Prio Item)
   (idx Queue (cons Prio Item) T) )

# Remove and return top item from priority queue
(de removePQ (Queue)
   (cdar (idx Queue (peekPQ Queue) NIL)) )

# Find top element in priority queue
(de peekPQ (Queue)
   (let V (val Queue)
      (while (cadr V)
         (setq V @) )
      (car V) ) )

# Merge second queue into first
(de mergePQ (Queue1 Queue2)
   (balance Queue1 (sort (conc (idx Queue1) (idx Queue2)))) )
```

Test:

```PicoLisp
# Two priority queues
(off Pq1 Pq2)

# Insert into first queue
(insertPQ 'Pq1 3 '(Clear drains))
(insertPQ 'Pq1 4 '(Feed cat))

# Insert into second queue
(insertPQ 'Pq2 5 '(Make tea))
(insertPQ 'Pq2 1 '(Solve RC tasks))
(insertPQ 'Pq2 2 '(Tax return))

# Merge second into first queue
(mergePQ 'Pq1 'Pq2)

# Remove and print all items from first queue
(while Pq1
   (println (removePQ 'Pq1)) )
```

Output:

```txt
(Solve RC tasks)
(Tax return)
(Clear drains)
(Feed cat)
(Make tea)
```



## Prolog

SWI-Prolog has a library <b>heaps.pl</b>, written by <b>Lars Buitinck</b> that implements priority queues.

Informations here : http://www.swi-prolog.org/pldoc/doc/swi/library/heaps.pl

Example of use :

```Prolog
priority-queue :-
  TL0 = [3-'Clear drains',
         4-'Feed cat'],

  % we can create a priority queue from a list
  list_to_heap(TL0, Heap0),

  % alternatively we can start from an empty queue
  % get from empty_heap/1.

  % now we add the other elements
  add_to_heap(Heap0, 5, 'Make tea', Heap1),
  add_to_heap(Heap1, 1, 'Solve RC tasks', Heap2),
  add_to_heap(Heap2, 2, 'Tax return', Heap3),

  % we list the content of the heap:
  heap_to_list(Heap3, TL1),
  writeln('Content of the queue'), maplist(writeln, TL1),
  nl,

  % now we retrieve the minimum-priority pair
  get_from_heap(Heap3, Priority, Key, Heap4),
  format('Retrieve top of the queue : Priority ~w, Element ~w~n', [Priority, Key]),
  nl,

  % we list the content of the heap:
  heap_to_list(Heap4, TL2),
  writeln('Content of the queue'), maplist(writeln, TL2).

```

The output :

```txt
1 ?- priority-queue.
Content of the queue
1-Solve RC tasks
2-Tax return
3-Clear drains
4-Feed cat
5-Make tea

Retrieve top of the queue : Priority 1, Element Solve RC tasks

Content of the queue
2-Tax return
3-Clear drains
4-Feed cat
5-Make tea
true.

```



## PureBasic

The priority queue is implemented using a binary heap array and a map.
The map stores the elements of a given priority in a FIFO list.
Priorities can be any signed 32 value.

```purebasic
Structure taskList
  List description.s()  ;implements FIFO queue
EndStructure

Structure task
  *tl.tList  ;pointer to a list of task descriptions
  Priority.i ;tasks priority, lower value has more priority
EndStructure

Structure priorityQueue
  maxHeapSize.i ;increases as needed
  heapItemCount.i  ;number of elements currently in heap
  Array heap.task(0) ;elements hold FIFO queues ordered by priorities, lowest first
  map heapMap.taskList() ;holds lists of tasks with the same priority that are FIFO queues
EndStructure

Procedure insertPQ(*PQ.priorityQueue, description.s, p)
  If FindMapElement(*PQ\heapMap(), Str(p))
    LastElement(*PQ\heapMap()\description())
    AddElement(*PQ\heapMap()\description())
    *PQ\heapMap()\description() = description
  Else
    Protected *tl.taskList = AddMapElement(*PQ\heapMap(), Str(p))
    AddElement(*tl\description())
    *tl\description() = description

    Protected pos = *PQ\heapItemCount

    *PQ\heapItemCount + 1
    If *PQ\heapItemCount > *PQ\maxHeapSize
      Select *PQ\maxHeapSize
        Case 0
          *PQ\maxHeapSize = 128
        Default
          *PQ\maxHeapSize * 2
      EndSelect
      Redim *PQ\heap.task(*PQ\maxHeapSize)
    EndIf

    While pos > 0 And p < *PQ\heap((pos - 1) / 2)\Priority
      *PQ\heap(pos) = *PQ\heap((pos - 1) / 2)
      pos = (pos - 1) / 2
    Wend

    *PQ\heap(pos)\tl = *tl
    *PQ\heap(pos)\Priority = p
  EndIf
EndProcedure

Procedure.s removePQ(*PQ.priorityQueue)
  Protected *tl.taskList = *PQ\heap(0)\tl, description.s
  FirstElement(*tl\description())
  description = *tl\description()
  If ListSize(*tl\description()) > 1
    DeleteElement(*tl\description())
  Else
    DeleteMapElement(*PQ\heapMap(), Str(*PQ\heap(0)\Priority))

    *PQ\heapItemCount - 1
    *PQ\heap(0) = *PQ\heap(*PQ\heapItemCount)

    Protected pos
    Repeat
      Protected child1 = 2 * pos + 1
      Protected child2 = 2 * pos + 2
      If child1 >= *PQ\heapItemCount
        Break
      EndIf

      Protected smallestChild
      If child2 >= *PQ\heapItemCount
        smallestChild = child1
      ElseIf *PQ\heap(child1)\Priority <= *PQ\heap(child2)\Priority
        smallestChild = child1
      Else
        smallestChild = child2
      EndIf

      If (*PQ\heap(smallestChild)\Priority >= *PQ\heap(pos)\Priority)
        Break
      EndIf
      Swap *PQ\heap(pos)\tl, *PQ\heap(smallestChild)\tl
      Swap *PQ\heap(pos)\Priority, *PQ\heap(smallestChild)\Priority
      pos = smallestChild
    ForEver
  EndIf

  ProcedureReturn description
EndProcedure

Procedure isEmptyPQ(*PQ.priorityQueue) ;returns 1 if empty, otherwise returns 0
  If *PQ\heapItemCount
    ProcedureReturn 0
  EndIf
  ProcedureReturn 1
EndProcedure

If OpenConsole()
  Define PQ.priorityQueue
  insertPQ(PQ, "Clear drains", 3)
  insertPQ(PQ, "Answer Phone 1", 8)
  insertPQ(PQ, "Feed cat", 4)
  insertPQ(PQ, "Answer Phone 2", 8)
  insertPQ(PQ, "Make tea", 5)
  insertPQ(PQ, "Sleep", 9)
  insertPQ(PQ, "Check email", 3)
  insertPQ(PQ, "Solve RC tasks", 1)
  insertPQ(PQ, "Answer Phone 3", 8)
  insertPQ(PQ, "Exercise", 9)
  insertPQ(PQ, "Answer Phone 4", 8)
  insertPQ(PQ, "Tax return", 2)

  While Not isEmptyPQ(PQ)
    PrintN(removePQ(PQ))
  Wend

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
Solve RC tasks
Tax return
Clear drains
Check email
Feed cat
Make tea
Answer Phone 1
Answer Phone 2
Answer Phone 3
Answer Phone 4
Sleep
Exercise
```



## Python


### Using PriorityQueue

Python has the class [http://docs.python.org/release/3.2/library/queue.html#queue.PriorityQueue queue.PriorityQueue] in its standard library.

The data structures in the "queue" module are synchronized multi-producer, multi-consumer queues for multi-threaded use. They can however handle this task:

```python>>>
 import queue
>>> pq = queue.PriorityQueue()
>>> for item in ((3, "Clear drains"), (4, "Feed cat"), (5, "Make tea"), (1, "Solve RC tasks"), (2, "Tax return")):
  pq.put(item)


>>> while not pq.empty():
  print(pq.get_nowait())


(1, 'Solve RC tasks')
(2, 'Tax return')
(3, 'Clear drains')
(4, 'Feed cat')
(5, 'Make tea')
>>>
```


;Help text for queue.PriorityQueue:

```python>>>
 import queue
>>> help(queue.PriorityQueue)
Help on class PriorityQueue in module queue:

class PriorityQueue(Queue)
 |  Variant of Queue that retrieves open entries in priority order (lowest first).
 |
 |  Entries are typically tuples of the form:  (priority number, data).
 |
 |  Method resolution order:
 |      PriorityQueue
 |      Queue
 |      builtins.object
 |
 |  Methods inherited from Queue:
 |
 |  __init__(self, maxsize=0)
 |
 |  empty(self)
 |      Return True if the queue is empty, False otherwise (not reliable!).
 |
 |      This method is likely to be removed at some point.  Use qsize() == 0
 |      as a direct substitute, but be aware that either approach risks a race
 |      condition where a queue can grow before the result of empty() or
 |      qsize() can be used.
 |
 |      To create code that needs to wait for all queued tasks to be
 |      completed, the preferred technique is to use the join() method.
 |
 |  full(self)
 |      Return True if the queue is full, False otherwise (not reliable!).
 |
 |      This method is likely to be removed at some point.  Use qsize() >= n
 |      as a direct substitute, but be aware that either approach risks a race
 |      condition where a queue can shrink before the result of full() or
 |      qsize() can be used.
 |
 |  get(self, block=True, timeout=None)
 |      Remove and return an item from the queue.
 |
 |      If optional args 'block' is true and 'timeout' is None (the default),
 |      block if necessary until an item is available. If 'timeout' is
 |      a positive number, it blocks at most 'timeout' seconds and raises
 |      the Empty exception if no item was available within that time.
 |      Otherwise ('block' is false), return an item if one is immediately
 |      available, else raise the Empty exception ('timeout' is ignored
 |      in that case).
 |
 |  get_nowait(self)
 |      Remove and return an item from the queue without blocking.
 |
 |      Only get an item if one is immediately available. Otherwise
 |      raise the Empty exception.
 |
 |  join(self)
 |      Blocks until all items in the Queue have been gotten and processed.
 |
 |      The count of unfinished tasks goes up whenever an item is added to the
 |      queue. The count goes down whenever a consumer thread calls task_done()
 |      to indicate the item was retrieved and all work on it is complete.
 |
 |      When the count of unfinished tasks drops to zero, join() unblocks.
 |
 |  put(self, item, block=True, timeout=None)
 |      Put an item into the queue.
 |
 |      If optional args 'block' is true and 'timeout' is None (the default),
 |      block if necessary until a free slot is available. If 'timeout' is
 |      a positive number, it blocks at most 'timeout' seconds and raises
 |      the Full exception if no free slot was available within that time.
 |      Otherwise ('block' is false), put an item on the queue if a free slot
 |      is immediately available, else raise the Full exception ('timeout'
 |      is ignored in that case).
 |
 |  put_nowait(self, item)
 |      Put an item into the queue without blocking.
 |
 |      Only enqueue the item if a free slot is immediately available.
 |      Otherwise raise the Full exception.
 |
 |  qsize(self)
 |      Return the approximate size of the queue (not reliable!).
 |
 |  task_done(self)
 |      Indicate that a formerly enqueued task is complete.
 |
 |      Used by Queue consumer threads.  For each get() used to fetch a task,
 |      a subsequent call to task_done() tells the queue that the processing
 |      on the task is complete.
 |
 |      If a join() is currently blocking, it will resume when all items
 |      have been processed (meaning that a task_done() call was received
 |      for every item that had been put() into the queue).
 |
 |      Raises a ValueError if called more times than there were items
 |      placed in the queue.
 |
 |  ----------------------------------------------------------------------
 |  Data descriptors inherited from Queue:
 |
 |  __dict__
 |      dictionary for instance variables (if defined)
 |
 |  __weakref__
 |      list of weak references to the object (if defined)

>>>
```



### Using heapq

Python has the [http://docs.python.org/release/3.2/library/heapq.html heapq] module in its standard library.

Although one can use the heappush method to add items individually to a heap similar to the method used in the PriorityQueue example above, we will instead transform the list of items into a heap in one go then pop them off one at a time as before.

```python>>>
 from heapq import heappush, heappop, heapify
>>> items = [(3, "Clear drains"), (4, "Feed cat"), (5, "Make tea"), (1, "Solve RC tasks"), (2, "Tax return")]
>>> heapify(items)
>>> while items:
  print(heappop(items))


(1, 'Solve RC tasks')
(2, 'Tax return')
(3, 'Clear drains')
(4, 'Feed cat')
(5, 'Make tea')
>>>
```


;Help text for module heapq:

```python>>>
 help('heapq')
Help on module heapq:

NAME
    heapq - Heap queue algorithm (a.k.a. priority queue).

DESCRIPTION
    Heaps are arrays for which a[k] <= a[2*k+1] and a[k] <= a[2*k+2] for
    all k, counting elements from 0.  For the sake of comparison,
    non-existing elements are considered to be infinite.  The interesting
    property of a heap is that a[0] is always its smallest element.

    Usage:

    heap = []            # creates an empty heap
    heappush(heap, item) # pushes a new item on the heap
    item = heappop(heap) # pops the smallest item from the heap
    item = heap[0]       # smallest item on the heap without popping it
    heapify(x)           # transforms list into a heap, in-place, in linear time
    item = heapreplace(heap, item) # pops and returns smallest item, and adds
                                   # new item; the heap size is unchanged

    Our API differs from textbook heap algorithms as follows:

    - We use 0-based indexing.  This makes the relationship between the
      index for a node and the indexes for its children slightly less
      obvious, but is more suitable since Python uses 0-based indexing.

    - Our heappop() method returns the smallest item, not the largest.

    These two make it possible to view the heap as a regular Python list
    without surprises: heap[0] is the smallest item, and heap.sort()
    maintains the heap invariant!

FUNCTIONS
    heapify(...)
        Transform list into a heap, in-place, in O(len(heap)) time.

    heappop(...)
        Pop the smallest item off the heap, maintaining the heap invariant.

    heappush(...)
        Push item onto heap, maintaining the heap invariant.

    heappushpop(...)
        Push item on the heap, then pop and return the smallest item
        from the heap. The combined action runs more efficiently than
        heappush() followed by a separate call to heappop().

    heapreplace(...)
        Pop and return the current smallest value, and add the new item.

        This is more efficient than heappop() followed by heappush(), and can be
        more appropriate when using a fixed-size heap.  Note that the value
        returned may be larger than item!  That constrains reasonable uses of
        this routine unless written as part of a conditional replacement:

            if item > heap[0]:
                item = heapreplace(heap, item)

    merge(*iterables)
        Merge multiple sorted inputs into a single sorted output.

        Similar to sorted(itertools.chain(*iterables)) but returns a generator,
        does not pull the data into memory all at once, and assumes that each of
        the input streams is already sorted (smallest to largest).

        >>> list(merge([1,3,5,7], [0,2,4,8], [5,10,15,20], [], [25]))
        [0, 1, 2, 3, 4, 5, 5, 7, 8, 10, 15, 20, 25]

    nlargest(n, iterable, key=None)
        Find the n largest elements in a dataset.

        Equivalent to:  sorted(iterable, key=key, reverse=True)[:n]

    nsmallest(n, iterable, key=None)
        Find the n smallest elements in a dataset.

        Equivalent to:  sorted(iterable, key=key)[:n]

DATA
    __about__ = 'Heap queues\n\n[explanation by François Pinard]\n\nH... t...
    __all__ = ['heappush', 'heappop', 'heapify', 'heapreplace', 'merge', '...

FILE
    c:\python32\lib\heapq.py


>>>
```



## R

Using closures:

```R
PriorityQueue <- function() {
  keys <- values <- NULL
  insert <- function(key, value) {
    ord <- findInterval(key, keys)
    keys <<- append(keys, key, ord)
    values <<- append(values, value, ord)
  }
  pop <- function() {
    head <- list(key=keys[1],value=values[[1]])
    values <<- values[-1]
    keys <<- keys[-1]
    return(head)
  }
  empty <- function() length(keys) == 0
  environment()
}

pq <- PriorityQueue()
pq$insert(3, "Clear drains")
pq$insert(4, "Feed cat")
pq$insert(5, "Make tea")
pq$insert(1, "Solve RC tasks")
pq$insert(2, "Tax return")
while(!pq$empty()) {
  with(pq$pop(), cat(key,":",value,"\n"))
}
```
With output:
```R
1 : Solve RC tasks
2 : Tax return
3 : Clear drains
4 : Feed cat
5 : Make tea
```
A similar implementation using R5 classes:
```R
PriorityQueue <-
    setRefClass("PriorityQueue",
                fields = list(keys = "numeric", values = "list"),
                methods = list(
                    insert = function(key,value) {
                        insert.order <- findInterval(key, keys)
                        keys <<- append(keys, key, insert.order)
                        values <<- append(values, value, insert.order)
                    },
                    pop = function() {
                        head <- list(key=keys[1],value=values[[1]])
                        keys <<- keys[-1]
                        values <<- values[-1]
                        return(head)
                    },
                    empty = function() length(keys) == 0
                ))
```
The only change in the example would be in the instantiation:
```R
pq <- PriorityQueue$new()
```
.


## Racket

This solution implements priority queues on top of heaps.

```racket

#lang racket
(require data/heap)

(define pq (make-heap (λ(x y) (<= (second x) (second y)))))

(define (insert! x pri)
  (heap-add! pq (list pri x)))

(define (remove-min!)
  (begin0
    (first (heap-min pq))
    (heap-remove-min! pq)))

(insert! 3 "Clear drains")
(insert! 4 "Feed cat")
(insert! 5 "Make tea")
(insert! 1 "Solve RC tasks")
(insert! 2 "Tax return")

(remove-min!)
(remove-min!)
(remove-min!)
(remove-min!)
(remove-min!)

```

Output:

```racket

"Solve RC tasks"
"Tax return"
"Clear drains"
"Feed cat"
"Make tea"

```



## REXX


### version 1

Programming note:   this REXX version allows any number (with or without decimals, say, '''5.7''') for the priority, including negative numbers.

```rexx
/*REXX program implements a  priority queue   with  insert/display/delete  the top task.*/
#=0;   @.=                                       /*0 tasks;  nullify the priority queue.*/
say '══════ inserting tasks.';     call .ins  3  "Clear drains"
                                   call .ins  4  "Feed cat"
                                   call .ins  5  "Make tea"
                                   call .ins  1  "Solve RC tasks"
                                   call .ins  2  "Tax return"
                                   call .ins  6  "Relax"
                                   call .ins  6  "Enjoy"
say '══════ showing tasks.';       call .show
say '══════ deletes top task.';    say .del()    /*delete the top task.                 */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
.del:  procedure expose @. #; arg p;  if p=''  then p=.top();    y=@.p;   @.p=;   return y
.ins:  procedure expose @. #; #=#+1;  @.#=arg(1);    return #          /*entry, P, task.*/
.show: procedure expose @. #; do j=1  for #;  _=@.j;  if _\==''  then say _;  end;  return
/*──────────────────────────────────────────────────────────────────────────────────────*/
.top:  procedure expose @. #;      top=;              top#=
                   do j=1  for #;  _=word(@.j, 1);    if _==''  then iterate
                   if top=='' | _>top  then do;    top=_;    top#=j;    end
                   end   /*j*/
       return top#
```

{{out|output}}

```txt

══════ inserting tasks.
══════ showing tasks.
3 Clear drains
4 Feed cat
5 Make tea
1 Solve RC tasks
2 Tax return
6 Relax
6 Enjoy
══════ deletes top task.
6 Relax

```



### version 2


```rexx
/*REXX pgm implements a priority queue; with insert/show/delete top task*/
n=0
task.=0 /* for the sake of task.0done.* */
say '------ inserting tasks.';     call ins_task 3 'Clear drains'
                                   call ins_task 4 'Feed cat'
                                   call ins_task 5 'Make tea'
                                   call ins_task 1 'Solve RC tasks'
                                   call ins_task 2 'Tax return'
                                   call ins_task 6 'Relax'
                                   call ins_task 6 'Enjoy'
say '------ Showing tasks.';       call show_tasks
say '------ Show and delete top task.'
todo=n  /* tasks to be done             */
do While todo>0
  Say top()
  End
exit

ins_task: procedure expose n task.
n=n+1
Parse Arg task.0pri.n task.0txt.n
Return

show_tasks: procedure expose task. n
do i=1 To n
  Say task.0pri.i task.0txt.i
  End
Return

top: procedure expose n task. todo /* get top task and mark it 'done' */
high=0
Do i=1 To n
  If task.0pri.i>high &,
     task.0done.i=0 Then Do
    j=i
    high=task.0pri.i
    End
  End
res=task.0pri.j task.0txt.j
task.0done.j=1
todo=todo-1
return res
```

{{out}}

```txt
------ inserting tasks.
------ Showing tasks.
3 Clear drains
4 Feed cat
5 Make tea
1 Solve RC tasks
2 Tax return
6 Relax
6 Enjoy
------ Show and delete top task.
6 Relax
6 Enjoy
5 Make tea
4 Feed cat
3 Clear drains
2 Tax return
1 Solve RC tasks
```



## Ruby

A naive, inefficient implementation

```ruby
class PriorityQueueNaive
  def initialize(data=nil)
    @q = Hash.new {|h, k| h[k] = []}
    data.each {|priority, item| @q[priority] << item}  if data
    @priorities = @q.keys.sort
  end

  def push(priority, item)
    @q[priority] << item
    @priorities = @q.keys.sort
  end

  def pop
    p = @priorities[0]
    item = @q[p].shift
    if @q[p].empty?
      @q.delete(p)
      @priorities.shift
    end
    item
  end

  def peek
    unless empty?
      @q[@priorities[0]][0]
    end
  end

  def empty?
    @priorities.empty?
  end

  def each
    @q.each do |priority, items|
      items.each {|item| yield priority, item}
    end
  end

  def dup
    @q.each_with_object(self.class.new) do |(priority, items), obj|
      items.each {|item| obj.push(priority, item)}
    end
  end

  def merge(other)
    raise TypeError  unless self.class == other.class
    pq = dup
    other.each {|priority, item| pq.push(priority, item)}
    pq                  # return a new object
  end

  def inspect
    @q.inspect
  end
end

test = [
  [6, "drink tea"],
  [3, "Clear drains"],
  [4, "Feed cat"],
  [5, "Make tea"],
  [6, "eat biscuit"],
  [1, "Solve RC tasks"],
  [2, "Tax return"],
]

pq = PriorityQueueNaive.new
test.each {|pr, str| pq.push(pr, str) }
until pq.empty?
  puts pq.pop
end

puts
test2 = test.shift(3)
p pq1 = PriorityQueueNaive.new(test)
p pq2 = PriorityQueueNaive.new(test2)
p pq3 = pq1.merge(pq2)
puts "peek : #{pq3.peek}"
until pq3.empty?
  puts pq3.pop
end
puts "peek : #{pq3.peek}"
```


{{out}}

```txt

Solve RC tasks
Tax return
Clear drains
Feed cat
Make tea
drink tea
eat biscuit

{5=>["Make tea"], 6=>["eat biscuit"], 1=>["Solve RC tasks"], 2=>["Tax return"]}
{6=>["drink tea"], 3=>["Clear drains"], 4=>["Feed cat"]}
{5=>["Make tea"], 6=>["eat biscuit", "drink tea"], 1=>["Solve RC tasks"], 2=>["Tax return"], 3=>["Clear drains"], 4=>["Feed cat"]}
peek : Solve RC tasks
Solve RC tasks
Tax return
Clear drains
Feed cat
Make tea
eat biscuit
drink tea
peek :

```



## Run BASIC


```runbasic
sqliteconnect #mem, ":memory:"
#mem execute("CREATE TABLE queue (priority float,descr text)")

' --------------------------------------------------------------
' Insert items into the que
' --------------------------------------------------------------
#mem execute("INSERT INTO queue VALUES (3,'Clear drains')")
#mem execute("INSERT INTO queue VALUES (4,'Feed cat')")
#mem execute("INSERT INTO queue VALUES (5,'Make tea')")
#mem execute("INSERT INTO queue VALUES (1,'Solve RC tasks')")
#mem execute("INSERT INTO queue VALUES (2,'Tax return')")

'--------------- insert priority between 4 and 5 -----------------
#mem execute("INSERT INTO queue VALUES (4.5,'My Special Project')")

what$ = " -------------- Find first priority ---------------------"
mem$ = "SELECT * FROM queue ORDER BY priority LIMIT 1"
gosub [getQueue]

what$ = " -------------- Find last priority ---------------------"
mem$ = "SELECT * FROM queue ORDER BY priority desc LIMIT 1"
gosub [getQueue]

what$ = " -------------- Delete Highest Priority ---------------------"
mem$ = "DELETE FROM queue WHERE priority = (select max(q.priority) FROM queue as q)"
#mem  execute(mem$)

what$ = " -------------- List Priority Sequence ---------------------"
mem$ = "SELECT * FROM queue ORDER BY priority"
gosub [getQueue]
end


[getQueue]
print what$
#mem  execute(mem$)
rows    = #mem ROWCOUNT()
print "Priority    Description"
for i = 1 to rows
  #row     = #mem #nextrow()
  priority = #row priority()
  descr$   = #row descr$()
print priority;"         ";descr$
next i
RETURN
```

{{out}}

```txt
 -------------- Find first priority ---------------------
Priority    Description
1.0         Solve RC tasks
 -------------- Find last priority ---------------------
Priority    Description
5.0         Make tea
 -------------- List Priority Sequence ---------------------
Priority    Description
1.0         Solve RC tasks
2.0         Tax return
3.0         Clear drains
4.0         Feed cat
4.5         My Special Project
```



## Rust


```rust
use std::collections::BinaryHeap;
use std::cmp::Ordering;
use std::borrow::Cow;

#[derive(Eq, PartialEq)]
struct Item<'a> {
    priority: usize,
    task: Cow<'a, str>, // Takes either borrowed or owned string
}

impl<'a> Item<'a> {
    fn new<T>(p: usize, t: T) -> Self
        where T: Into<Cow<'a, str>>
    {
        Item {
            priority: p,
            task: t.into(),
        }
    }
}

// Manually implpement Ord so we have a min heap
impl<'a> Ord for Item<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        other.priority.cmp(&self.priority)
    }
}

// PartialOrd is required by Ord
impl<'a> PartialOrd for Item<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}


fn main() {
    let mut queue = BinaryHeap::with_capacity(5);
    queue.push(Item::new(3, "Clear drains"));
    queue.push(Item::new(4, "Feed cat"));
    queue.push(Item::new(5, "Make tea"));
    queue.push(Item::new(1, "Solve RC tasks"));
    queue.push(Item::new(2, "Tax return"));

    for item in queue {
        println!("{}", item.task);
    }
}
```

{{out}}

```txt
Solve RC tasks
Tax return
Make tea
Feed cat
Clear drains
```



## Scala

Scala has a class PriorityQueue in its standard library.

```scala
import scala.collection.mutable.PriorityQueue
case class Task(prio:Int, text:String) extends Ordered[Task] {
  def compare(that: Task)=that.prio compare this.prio
}

//test
var q=PriorityQueue[Task]() ++ Seq(Task(3, "Clear drains"), Task(4, "Feed cat"),
  Task(5, "Make tea"), Task(1, "Solve RC tasks"), Task(2, "Tax return"))
while(q.nonEmpty) println(q dequeue)
```

Output:

```txt
Task(1,Solve RC tasks)
Task(2,Tax return)
Task(3,Clear drains)
Task(4,Feed cat)
Task(5,Make tea)
```

Instead of deriving the class from Ordering an implicit conversion could be provided.

```scala
case class Task(prio:Int, text:String)
implicit def taskOrdering=new Ordering[Task] {
  def compare(t1:Task, t2:Task):Int=t2.prio compare t1.prio
}
```



## Sidef

{{trans|Perl 6}}

```ruby
class PriorityQueue {
    has tasks = []

    method insert (Number priority { _ >= 0 }, task) {
        for n in range(tasks.len, priority) {
            tasks[n] = []
        }
        tasks[priority].append(task)
    }

    method get      { tasks.first { !.is_empty } -> shift }
    method is_empty { tasks.all   {  .is_empty } }
}

var pq = PriorityQueue()

[
    [3, 'Clear drains'],
    [4, 'Feed cat'],
    [5, 'Make tea'],
    [9, 'Sleep'],
    [3, 'Check email'],
    [1, 'Solve RC tasks'],
    [9, 'Exercise'],
    [2, 'Do taxes'],
].each { |pair|
    pq.insert(pair...)
}

say pq.get while !pq.is_empty
```


{{out}}

```txt

Solve RC tasks
Do taxes
Clear drains
Check email
Feed cat
Make tea
Sleep
Exercise

```



## Standard ML

{{works with|SML/NJ}}
Note: this is a max-heap


```sml
structure TaskPriority = struct
  type priority = int
  val compare = Int.compare
  type item = int * string
  val priority : item -> int = #1
end

structure PQ = LeftPriorityQFn (TaskPriority)
;

let
  val tasks = [
    (3, "Clear drains"),
    (4, "Feed cat"),
    (5, "Make tea"),
    (1, "Solve RC tasks"),
    (2, "Tax return")]
  val pq = foldl PQ.insert PQ.empty tasks
  (* or val pq = PQ.fromList tasks *)
  fun aux pq' =
    case PQ.next pq' of
      NONE => ()
    | SOME ((prio, name), pq'') => (
        print (Int.toString prio ^ ", " ^ name ^ "\n");
        aux pq''
      )
in
  aux pq
end
```


testing:

```txt

5, Make tea
4, Feed cat
3, Clear drains
2, Tax return
1, Solve RC tasks

```



## Swift

You can use <code>CFBinaryHeap</code> from Core Foundation, but it is super ugly due to the fact that <code>CFBinaryHeap</code> operates on generic pointers, and you need to convert back and forth between that and objects.
{{works with|Swift|2.x}}

```swift
class Task : Comparable, CustomStringConvertible {
  var priority : Int
  var name: String
  init(priority: Int, name: String) {
    self.priority = priority
    self.name = name
  }
  var description: String {
    return "\(priority), \(name)"
  }
}
func ==(t1: Task, t2: Task) -> Bool {
  return t1.priority == t2.priority
}
func <(t1: Task, t2: Task) -> Bool {
  return t1.priority < t2.priority
}

struct TaskPriorityQueue {
  let heap : CFBinaryHeapRef = {
    var callBacks = CFBinaryHeapCallBacks(version: 0, retain: {
      UnsafePointer(Unmanaged<Task>.fromOpaque(COpaquePointer($1)).retain().toOpaque())
      }, release: {
        Unmanaged<Task>.fromOpaque(COpaquePointer($1)).release()
      }, copyDescription: nil, compare: { (ptr1, ptr2, _) in
        let t1 : Task = Unmanaged<Task>.fromOpaque(COpaquePointer(ptr1)).takeUnretainedValue()
        let t2 : Task = Unmanaged<Task>.fromOpaque(COpaquePointer(ptr2)).takeUnretainedValue()
        return t1 == t2 ? CFComparisonResult.CompareEqualTo : t1 < t2 ? CFComparisonResult.CompareLessThan : CFComparisonResult.CompareGreaterThan
    })
    return CFBinaryHeapCreate(nil, 0, &callBacks, nil)
  }()
  var count : Int { return CFBinaryHeapGetCount(heap) }
  mutating func push(t: Task) {
    CFBinaryHeapAddValue(heap, UnsafePointer(Unmanaged.passUnretained(t).toOpaque()))
  }
  func peek() -> Task {
    return Unmanaged<Task>.fromOpaque(COpaquePointer(CFBinaryHeapGetMinimum(heap))).takeUnretainedValue()
  }
  mutating func pop() -> Task {
    let result = Unmanaged<Task>.fromOpaque(COpaquePointer(CFBinaryHeapGetMinimum(heap))).takeUnretainedValue()
    CFBinaryHeapRemoveMinimumValue(heap)
    return result
  }
}

var pq = TaskPriorityQueue()

pq.push(Task(priority: 3, name: "Clear drains"))
pq.push(Task(priority: 4, name: "Feed cat"))
pq.push(Task(priority: 5, name: "Make tea"))
pq.push(Task(priority: 1, name: "Solve RC tasks"))
pq.push(Task(priority: 2, name: "Tax return"))

while pq.count != 0 {
  print(pq.pop())
}
```


{{out}}

```txt

1, Solve RC tasks
2, Tax return
3, Clear drains
4, Feed cat
5, Make tea

```



## Tcl

{{tcllib|struct::prioqueue}}

```tcl
package require struct::prioqueue

set pq [struct::prioqueue]
foreach {priority task} {
    3 "Clear drains"
    4 "Feed cat"
    5 "Make tea"
    1 "Solve RC tasks"
    2 "Tax return"
} {
    # Insert into the priority queue
    $pq put $task $priority
}
# Drain the queue, in priority-sorted order
while {[$pq size]} {
    # Remove the front-most item from the priority queue
    puts [$pq get]
}
```

Which produces this output:

```txt

Make tea
Feed cat
Clear drains
Tax return
Solve RC tasks

```



## VBA


```VB
Type Tuple
    Priority As Integer
    Data As String
End Type
Dim a() As Tuple
Dim n As Integer 'number of elements in array, last element is n-1
Private Function Left(i As Integer) As Integer
    Left = 2 * i + 1
End Function
Private Function Right(i As Integer) As Integer
    Right = 2 * i + 2
End Function
Private Function Parent(i As Integer) As Integer
    Parent = (i - 1) \ 2
End Function
Private Sub Add(fPriority As Integer, fData As String)
    n = n + 1
    If n > UBound(a) Then ReDim Preserve a(2 * n)
    a(n - 1).Priority = fPriority
    a(n - 1).Data = fData
    bubbleUp (n - 1)
End Sub
Private Sub Swap(i As Integer, j As Integer)
    Dim t As Tuple
    t = a(i)
    a(i) = a(j)
    a(j) = t
End Sub
Private Sub bubbleUp(i As Integer)
    Dim p As Integer
    p = Parent(i)
    Do While i > 0 And a(i).Priority < a(p).Priority
        Swap i, p
        i = p
        p = Parent(i)
    Loop
End Sub
Private Function Remove() As Tuple
    Dim x As Tuple
    x = a(0)
    a(0) = a(n - 1)
    n = n - 1
    trickleDown 0
    If 3 * n < UBound(a) Then ReDim Preserve a(UBound(a) \ 2)
    Remove = x
End Function
Private Sub trickleDown(i As Integer)
    Dim j As Integer, l As Integer, r As Integer
    Do
        j = -1
        r = Right(i)
        If r < n And a(r).Priority < a(i).Priority Then
            l = Left(i)
            If a(l).Priority < a(r).Priority Then
                j = l
            Else
                j = r
            End If
        Else
            l = Left(i)
            If l < n And a(l).Priority < a(i).Priority Then j = l
        End If
        If j >= 0 Then Swap i, j
        i = j
    Loop While i >= 0
End Sub
Public Sub PQ()
    ReDim a(4)
    Add 3, "Clear drains"
    Add 4, "Feed cat"
    Add 5, "Make tea"
    Add 1, "Solve RC tasks"
    Add 2, "Tax return"
    Dim t As Tuple
    Do While n > 0
        t = Remove
        Debug.Print t.Priority, t.Data
    Loop
End Sub
```
{{out}}
```txt
1            Solve RC tasks
2            Tax return
3            Clear drains
4            Feed cat
5            Make tea

```


## XLISP


It does not seem necessary that <i>every</i> queue should support arbitrarily many distinct priority levels, so long as <i>each particular</i> queue supports as many levels as the user anticipates needing. We therefore store a priority queue as a fixed-length vector of queues and allow the user to pass the least urgent level needed (counting from 0 as the most urgent) as a parameter when a new priority queue is instantiated.

A vector can be efficiently indexed into, and we can eliminate a lot of searching by providing for each priority queue to know its most urgent priority level at any given time. The <code>'POP</code> method can then return the first item stored at that level, without needing to search. If this operation leaves that level empty, however, it does need to search for the next non-empty level. The worst case would be popping from a queue that contained only one item, at the most urgent priority level: the program would have to search down all the levels looking for one that wasn't empty. In the nature of a priority queue, however, this case is probably unusual.

The <code>'PUSH</code> method never needs to search down the levels. The efficiency bottleneck here is probably the implementation of <code>NCONC</code> (used for adding the new item to the end of the queue at the relevant level). A priority <i>stack</i>, with first in / last out at each priority level rather than first in / first out, would be faster.


```lisp
(define-class priority-queue
	(instance-variables queue lowest-priority most-urgent) )

(define-method (priority-queue 'initialize limit)
	(defun setup (x)
		(vector-set! queue x nil)
		(if (< x limit)
			(setup (+ x 1)) ) )
	(setq lowest-priority limit)
	(setq most-urgent limit)
	(setq queue (make-vector (+ limit 1)))
	(setup 0)
	self )

(define-method (priority-queue 'push item priority)
	(if (and (integerp priority) (>= priority 0) (<= priority lowest-priority))
		(progn
			(setq most-urgent (min priority most-urgent))
			(vector-set! queue priority (nconc (vector-ref queue priority) (cons item nil))) ) ) )

(define-method (priority-queue 'pop)
	(defun find-next (q)
		(if (or (= q lowest-priority) (not (null (vector-ref queue q))))
			q
			(find-next (+ q 1)) ) )
	(define item (car (vector-ref queue most-urgent)))
	(vector-set! queue most-urgent (cdr (vector-ref queue most-urgent)))
	(setq most-urgent (find-next most-urgent))
	item )

(define-method (priority-queue 'peek)
	(car (vector-ref queue most-urgent)) )

(define-method (priority-queue 'emptyp)
	(and (= most-urgent lowest-priority) (null (vector-ref queue most-urgent))) )
```


The example uses strings, but the data items stored in the priority queue can be of any type (including the empty list—or even other priority queues).

```lisp
(define pq (priority-queue 'new 5))

(pq 'push "Clear drains" 3)
(pq 'push "Feed cat" 4)
(pq 'push "Make tea" 5)
(pq 'push "Solve RC tasks" 1)
(pq 'push "Tax return" 2)
```

{{out}}
Items are popped beginning from the most urgent:

```lisp
[1] (pq 'pop)

"Solve RC tasks"
[2] (pq 'pop)

"Tax return"
```

Within each priority level, new items are pushed onto the end and popped from the beginning of the list (a queue is a first in / first out data structure):

```lisp
[3] (pq 'push "Answer emails" 4)

("Feed cat" "Answer emails")
```

Attempting to push with an invalid priority value returns the empty list, i.e. false:

```lisp
[4] (pq 'push "Weed garden" 17)

()
```

<code>'EMPTYP</code> returns false if the priority queue is not empty:

```lisp
[5] (pq 'emptyp)

()
```

<code>'PEEK</code> non-destructively returns the item that would be popped if you called <code>'POP</code>:

```lisp
[6] (pq 'peek)

"Clear drains"
```

If you want to examine a whole priority queue, the built-in <code>'SHOW</code> method allows you to do so:

```scheme
[7] (pq 'show)

Object is #<Object:PRIORITY-QUEUE #x4e2cba8>, Class is #<Class:PRIORITY-QUEUE #x4e254c8>
Instance variables:
  QUEUE = #(() () () ("Clear drains") ("Feed cat" "Answer emails") ("Make tea"))
  LOWEST-PRIORITY = 5
  MOST-URGENT = 3
#<Object:PRIORITY-QUEUE #x4e2cba8>
```

Once all the items have been popped, the priority queue is empty and <code>'EMPTYP</code> then returns true:

```lisp
[8] (pq 'pop)

"Clear drains"
[9] (pq 'pop)

"Feed cat"
[10] (pq 'pop)

"Answer emails"
[11] (pq 'pop)

"Make tea"
[12] (pq 'emptyp)

#T
```

Attempting to pop from an empty priority queue returns false:

```lisp
[13] (pq 'pop)

()
```



## zkl

This solution uses a [hopefully small] fixed number of priorities, each of which has an unordered list of tasks. This allows O(1) insertions, O(p) for retrievals (p is the number of priorities).

```zkl
class PQ{
   fcn init(numLevels=10){  // 0..numLevels, bigger # == lower priorty
      var [const] queue=(1).pump(numLevels+1,List.createLong(numLevels).write,L().copy);
   }
   fcn add(item,priorty){ queue[priorty].append(item); }
   fcn peek{ if(q:=queue.filter1()) return(q[-1]);   Void }// -->Void if empty
   fcn pop { if(q:=queue.filter1()) return(q.pop()); Void }// -->Void if empty
   var [private] state=L();
   fcn [private] next{ // iterate
      qi,ii:=state;
      foreach n in ([qi..queue.len()-1]){
         q:=queue[n];
	 if(ii>=q.len()) ii=0;
	 else{ state.clear().append(n,ii+1); return(q[ii]) }
      }
      Void.Stop
   }
   fcn walker{ state.clear().append(0,0); Walker(next) } // iterator front end
   fcn toString{ "PQ(%d) items".fmt(queue.reduce(fcn(sum,q){ sum+q.len() },0)) }
}
```


```zkl
pq:=PQ();
foreach x in
      (T("Clear drains",3, "Feed cat",4, "Make tea",5, "Solve RC tasks",1, "Tax return",2,
         "Clean room",10,"Wash cat",10)){
   pq.add(x,__xWalker.next())
}
pq.println();
println("Number 1 thing to do: ",pq.peek());
println("Top 2 things to do: ",pq.walker().walk(2));
println("Do this next year: ",pq.walker().walk()[-1]);
println("ToDo list:");
foreach item in (pq){ item.println() }
pq.println();
```

{{out}}

```txt

PQ(7) items
Number 1 thing to do: Solve RC tasks
Top 2 things to do: L("Solve RC tasks","Tax return")
Do this next year: Wash cat
ToDo list:
Solve RC tasks
Tax return
Clear drains
Feed cat
Make tea
Clean room
Wash cat
PQ(7) items

```

