+++
title = "Queue/Definition"
description = ""
date = 2019-10-12T07:51:53Z
aliases = []
[extra]
id = 2188
[taxonomies]
categories = []
tags = []
+++

{{task|Data Structures}}
{{Data structure}}
[[File:Fifo.gif|frame|right|Illustration of FIFO behavior]]

;Task:
Implement a FIFO queue.

Elements are added at one side and popped from the other in the order of insertion.


Operations:
*   push     (aka ''enqueue'')   - add element
*   pop        (aka ''dequeue'')   - pop first element
*   empty    - return truth value when empty


Errors:
*   handle the error of trying to pop from an empty queue (behavior depends on the language and platform)


;See:
*   [[Queue/Usage]]   for the built-in FIFO or queue of your language or standard library.


{{Template:See also lists}}





## ACL2


```Lisp
(defun enqueue (x xs)
   (cons x xs))

(defun dequeue (xs)
   (declare (xargs :guard (and (consp xs)
                               (true-listp xs))))
   (if (or (endp xs) (endp (rest xs)))
       (mv (first xs) nil)
       (mv-let (x ys)
               (dequeue (rest xs))
          (mv x (cons (first xs) ys)))))

(defun empty (xs)
   (endp xs))
```



## Ada

The first example below demonstrates a FIFO created for single-threaded computing. This version has the advantage of using a minimum of memory per FIFO element, and being very fast.

The interface specification for a FIFO is described in the package specification.

```ada
generic
   type Element_Type is private;
package Fifo is
   type Fifo_Type is private;
   procedure Push(List : in out Fifo_Type; Item : in Element_Type);
   procedure Pop(List : in out Fifo_Type; Item : out Element_Type);
   function Is_Empty(List : Fifo_Type) return Boolean;
   Empty_Error : exception;
private
   type Fifo_Element;
   type Fifo_Ptr is access Fifo_Element;
   type Fifo_Type is record
      Head : Fifo_Ptr := null;
      Tail : Fifo_Ptr := null;
   end record;
   type Fifo_Element is record
      Value : Element_Type;
      Next  : Fifo_Ptr := null;
   end record;
end Fifo;
```

The FIFO implementation is described in the package body:

```ada
with Ada.Unchecked_Deallocation;

package body Fifo is

   ----------
   -- Push --
   ----------

   procedure Push (List : in out Fifo_Type; Item : in Element_Type) is
      Temp : Fifo_Ptr := new Fifo_Element'(Item, null);
   begin
      if List.Tail = null then
         List.Tail := Temp;
      end if;
      if List.Head /= null then
        List.Head.Next := Temp;
      end if;
      List.Head := Temp;
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (List : in out Fifo_Type; Item : out Element_Type) is
      procedure Free is new Ada.Unchecked_Deallocation(Fifo_Element, Fifo_Ptr);
      Temp : Fifo_Ptr := List.Tail;
   begin
      if List.Head = null then
         raise Empty_Error;
      end if;
      Item := List.Tail.Value;
      List.Tail := List.Tail.Next;
      if List.Tail = null then
         List.Head := null;
      end if;
      Free(Temp);
   end Pop;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (List : Fifo_Type) return Boolean is
   begin
      return List.Head = null;
   end Is_Empty;

end Fifo;
```

A "main" procedure for this program is:

```ada
with Fifo;
with Ada.Text_Io; use Ada.Text_Io;

procedure Fifo_Test is
   package Int_Fifo is new Fifo(Integer);
   use Int_Fifo;
   My_Fifo : Fifo_Type;
   Val : Integer;
begin
   for I in 1..10 loop
      Push(My_Fifo, I);
   end loop;
   while not Is_Empty(My_Fifo) loop
      Pop(My_Fifo, Val);
      Put_Line(Integer'Image(Val));
   end loop;
end Fifo_Test;
```

The following implementation produces equivalent functionality by deriving from the standard Ada  Container type Doubly_Linked_Lists.

This example needs fewer lines of code on the part of the application programmer, but the implementation is less efficient than the previous example. Each element has all the data members needed for a doubly linked list. It also links in all the functionality of a doubly linked list. Most of that functionality is unneeded in a FIFO.

```ada

 with Ada.Containers.Doubly_Linked_Lists;
 generic
    type Element_Type is private;
 package Generic_Fifo is
    type Fifo_Type is tagged private;
    procedure Push(The_Fifo : in out Fifo_Type; Item : in Element_Type);
    procedure Pop(The_Fifo : in out Fifo_Type; Item : out Element_Type);
    Empty_Error : Exception;
 private
    package List_Pkg is new Ada.Containers.Doubly_Linked_Lists(Element_Type);
    use List_Pkg;
    Type Fifo_Type is new List with null record;
 end Generic_Fifo;

```


```ada

 package body Generic_Fifo is

    ----------
    -- Push --
    ----------

    procedure Push (The_Fifo : in out Fifo_Type; Item : in Element_Type) is
    begin
       The_Fifo.Prepend(Item);
    end Push;

    ---------
    -- Pop --
    ---------

    procedure Pop (The_Fifo : in out Fifo_Type; Item : out Element_Type) is
    begin
       if Is_Empty(The_Fifo) then
          raise Empty_Error;
       end if;
       Item := The_Fifo.Last_Element;
       The_Fifo.Delete_Last;
    end Pop;

 end Generic_Fifo;
```


```ada
with Generic_Fifo;
with Ada.Text_Io; use Ada.Text_Io;

procedure Generic_Fifo_Test is
   package Int_Fifo is new Generic_Fifo(Integer);
   use Int_Fifo;
   My_Fifo : Fifo_Type;
   Val : Integer;
begin
   for I in 1..10 loop
      My_Fifo.Push(I);
   end loop;
   while not My_Fifo.Is_Empty loop
      My_Fifo.Pop(Val);
      Put_Line(Integer'Image(Val));
   end loop;
end Generic_Fifo_Test;
```

The function Is_Empty is inherited from the Lists type.

The next two examples provide simple FIFO functionality for concurrent tasks. The buffer in each example holds a single value. When running concurrent tasks, one writing to the buffer, and one reading from the buffer, either the writer will be faster than the reader, or the reader will be faster than the writer. If the writer is faster a dynamic FIFO will grow to consume all available memory on the computer. If the reader is faster the FIFO will either contain a single value or it will be empty. In either case, no implementation is more efficient than a single element buffer.

If we wish for the reader to read every value written by the writer we must synchronize the tasks. The writer can only write a new value when the buffer contains a stale value. The reader can only read a value when the value is fresh. This synchronization forces the two tasks to run at the same speed.

```ada
generic
   type Element_Type is private;
package Synchronous_Fifo is
   protected type Fifo is
      entry Push(Item : Element_Type);
      entry Pop(Item : out Element_Type);
   private
      Value : Element_Type;
      Is_New : Boolean := False;
   end Fifo;
end Synchronous_Fifo;
```


```ada
package body Synchronous_Fifo is

   ----------
   -- Fifo --
   ----------

   protected body Fifo is

      ---------
      -- Push --
      ---------

      entry Push (Item : Element_Type) when not Is_New is
      begin
         Value := Item;
         Is_New := True;
      end Push;

      ---------
      -- Pop --
      ---------

      entry Pop (Item : out Element_Type) when Is_New is
      begin
         Item := Value;
         Is_New := False;
      end Pop;

   end Fifo;

end Synchronous_Fifo;
```


```ada
with Synchronous_Fifo;
with Ada.Text_Io; use Ada.Text_Io;

 procedure Synchronous_Fifo_Test is
    package Int_Fifo is new Synchronous_Fifo(Integer);
    use Int_Fifo;
    Buffer : Fifo;

    task Writer is
       entry Stop;
    end Writer;

    task body Writer is
       Val : Positive := 1;
    begin
       loop
          select
             accept Stop;
             exit;
          else
             select
                Buffer.Push(Val);
                Val := Val + 1;
             or
                delay 1.0;
             end select;
          end select;
       end loop;
    end Writer;

    task Reader is
       entry Stop;
    end Reader;

    task body Reader is
       Val : Positive;
    begin
       loop
          select
             accept Stop;
             exit;
          else
             select
                Buffer.Pop(Val);
                Put_Line(Integer'Image(Val));
             or
                 delay 1.0;
            end select;
          end select;
       end loop;
    end Reader;
 begin
    delay 0.1;
    Writer.Stop;
    Reader.Stop;
 end Synchronous_Fifo_Test;
```

Another choice is to cause the two tasks to run independently. The writer can write whenever it is scheduled. The reader reads whenever it is scheduled, after the writer writes the first valid value.

In this example the writer writes several values before the reader reads a value. The reader will then read that same value several times before the writer is scheduled to write more values.

In a fully asynchronous system the reader only samples the values written by the writer. There is no control over the number of values not sampled by the reader, or over the number of times the reader reads the same value.

```ada
generic
   type Element_Type is private;
package Asynchronous_Fifo is
   protected type Fifo is
      procedure Push(Item : Element_Type);
      entry Pop(Item : out Element_Type);
   private
      Value : Element_Type;
      Valid : Boolean := False;
   end Fifo;
end Asynchronous_Fifo;
```

You may notice that the protected type specification is remarkably similar to the synchronous example above. The only important difference is that Push is declared to be an Entry in the synchronous example while it is a procedure in the asynchronous example. Entries only execute when their boundary condition evaluates to TRUE. Procedures execute unconditionally.

```ada
package body Asynchronous_Fifo is

   ----------
   -- Fifo --
   ----------

   protected body Fifo is

      ----------
      -- Push --
      ----------

      procedure Push (Item : Element_Type) is
      begin
          Value := Item;
         Valid := True;
      end Push;

      ---------
      -- Pop --
      ---------

      entry Pop (Item : out Element_Type) when Valid is
      begin
         Item := Value;
      end Pop;

   end Fifo;

end Asynchronous_Fifo;
```


```ada
with Asynchronous_Fifo;
with Ada.Text_Io; use Ada.Text_Io;

 procedure Asynchronous_Fifo_Test is
    package Int_Fifo is new Asynchronous_Fifo(Integer);
    use Int_Fifo;
    Buffer : Fifo;

    task Writer is
       entry Stop;
    end Writer;

    task body Writer is
       Val : Positive := 1;
    begin
       loop
          select
             accept Stop;
             exit;
          else
             Buffer.Push(Val);
             Val := Val + 1;
          end select;
       end loop;
    end Writer;

    task Reader is
       entry Stop;
    end Reader;

    task body Reader is
       Val : Positive;
    begin
       loop
          select
             accept Stop;
             exit;
          else
             Buffer.Pop(Val);
             Put_Line(Integer'Image(Val));
          end select;
       end loop;
    end Reader;
 begin
    delay 0.1;
    Writer.Stop;
    Reader.Stop;
 end Asynchronous_Fifo_Test;
```


## ALGOL 68

{{works with|ALGOL 68|Revision 1 - one extension to language used - PRAGMA READ - a non standard feature similar to C's #include directive.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.7 algol68g-2.7].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
'''File: prelude/queue_base.a68'''
```algol68
# -*- coding: utf-8 -*- #
CO REQUIRES:
  MODE OBJLINK = STRUCT(
    REF OBJLINK next,
    REF OBJLINK prev,
    OBJVALUE value # ... etc. required #
  );
  PROC obj link new = REF OBJLINK: ~;
  PROC obj link free = (REF OBJLINK free)VOID: ~
END CO

# actually a pointer to the last LINK, there ITEMs are ADDED/get #
MODE OBJQUEUE = REF OBJLINK;

OBJQUEUE obj queue empty = NIL;

BOOL obj queue par = FALSE; # make code thread safe #
SEMA obj queue sema = LEVEL ABS obj queue par;
# Warning: 1 SEMA for all queues of type obj, i.e. not 1 SEMA per queue #

PROC obj queue init = (REF OBJQUEUE self)REF OBJQUEUE:
  self := obj queue empty;

PROC obj queue put = (REF OBJQUEUE self, OBJVALUE obj)REF OBJQUEUE: (
  REF OBJLINK out = obj link new;
  IF obj queue par THEN DOWN obj queue sema FI;
  IF self IS obj queue empty THEN
    out := (out, out, obj) # self referal #
  ELSE # join into list #
    out := (self, prev OF self, obj);
    next OF prev OF out := prev OF next OF out := out
  FI;
  IF obj queue par THEN UP obj queue sema FI;
  self := out
);

# define a useful prepend/put/plusto (+=:) operator... #
PROC obj queue plusto = (OBJVALUE obj, REF OBJQUEUE self)OBJQUEUE: obj queue put(self,obj);
OP +=: = (OBJVALUE obj, REF OBJQUEUE self)REF OBJQUEUE: obj queue put(self,obj);
# a potential append/plusab (+:=) operator...
OP (REF OBJQUEUE, OBJVALUE)OBJQUEUE +:= = obj queue plusab;
#

# see if the program/coder wants the OBJ problem mended... #
PROC (REF OBJQUEUE #self#)BOOL obj queue index error mended
  := (REF OBJQUEUE self)BOOL: (abend("obj queue index error"); stop);

PROC on obj queue index error = (REF OBJQUEUE self, PROC(REF OBJQUEUE #self#)BOOL mended)VOID:
  obj queue index error mended := mended;

PROC obj queue get = (REF OBJQUEUE self)OBJVALUE: (
# DOWN obj queue sema; #
  IF self IS obj queue empty THEN
    IF NOT obj queue index error mended(self) THEN abend("obj stack index error") FI FI;
  OBJQUEUE old tail = prev OF self;
  IF old tail IS self THEN # free solo member #
    self := obj queue empty
  ELSE # free self/tail member #
    OBJQUEUE new tail = prev OF old tail;
    next OF new tail := self;
    prev OF self := new tail
  FI;
#;UP obj queue sema #
  OBJVALUE out = value OF old tail;
# give a recovery hint to the garbage collector #
  obj link free(old tail);
  out
);

PROC obj queue is empty = (REF OBJQUEUE self)BOOL:
  self IS obj queue empty;

SKIP
```
'''See also:''' [[Queue/Usage#ALGOL_68|Queue/Usage]]


## ALGOL W


```algolw
begin
    % define a Queue type that will hold StringQueueElements %
    record StringQueue ( reference(StringQueueElement) front, back );
    % define the StringQueueElement type %
    record StringQueueElement ( string(8)                     element
                              ; reference(StringQueueElement) next
                              );
    % we would need separate types for other element types   %
    % adds s to the end of the StringQueue q                 %
    procedure pushString ( reference(StringQueue) value q
                         ; string(8)              value e
                         ) ;
    begin
        reference(StringQueueElement) newElement;
        newElement := StringQueueElement( e, null );
        if front(q) = null then begin
            % adding to an empty queue %
            front(q) := newElement;
            back(q)  := newElement
            end
        else begin
            % the queue is not empty %
            next(back(q)) := newElement;
            back(q)       := newElement
        end
    end pushString ;
    % removes an element from the front of the StringQueue q %
    % asserts the queue is not empty, which will stop the    %
    % program if it is                                       %
    string(8) procedure popString ( reference(StringQueue) value q ) ;
    begin
        string(8) v;
        assert( not isEmptyStringQueue( q ) );
        v        := element(front(q));
        front(q) := next(front(q));
        if front(q) = null then % just popped the last element % back(q) := null;
        v
    end popStringQueue ;
    % returns true if the StringQueue q is empty, false otherwise %
    logical procedure isEmptyStringQueue ( reference(StringQueue) value q ) ; front(q) = null;

    begin % test the StringQueue operations %
        reference(StringQueue) q;
        q := StringQueue( null, null );
        pushString( q, "fred"   );
        pushString( q, "whilma" );
        pushString( q, "betty"  );
        pushString( q, "barney" );
        while not isEmptyStringQueue( q ) do write( popString( q ) )
    end
end.
```

{{out}}

```txt

fred
whilma
betty
barney

```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program defqueue.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

.equ  NBMAXIELEMENTS,    100

/*******************************************/
/* Structures                               */
/********************************************/
/* example structure  for value of item  */
    .struct  0
value_ident:                     @ ident
    .struct  value_ident + 4
value_value1:                    @ value 1
    .struct  value_value1 + 4
value_value2:                    @ value 2
    .struct  value_value2 + 4
value_fin:
/* example structure  for queue  */
    .struct  0
queue_ptdeb:                     @ begin pointer of item
    .struct  queue_ptdeb + 4
queue_ptfin:                     @ end pointer of item
    .struct  queue_ptfin + 4
queue_stvalue:                   @ structure of value item
    .struct  queue_stvalue + (value_fin * NBMAXIELEMENTS)
queue_fin:


/*********************************/
/* Initialized data              */
/*********************************/
.data
szMessEmpty:       .asciz "Empty queue. \n"
szMessNotEmpty:    .asciz "Not empty queue. \n"
szMessError:       .asciz "Error detected !!!!. \n"
szMessResult:      .ascii "Ident :"                    @ message result
sMessIdent:        .fill 11, 1, ' '
                    .ascii " value 1 :"
sMessValue1:       .fill 11, 1, ' '
                    .ascii " value 2 :"
sMessValue2:       .fill 11, 1, ' '
                    .asciz "\n"

szCarriageReturn:  .asciz "\n"
/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
.align 4
Queue1:                .skip queue_fin      @ queue memory place
stItem:                .skip value_fin      @ value item memory place
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
    ldr r0,iAdrstItem
    mov r1,#1
    str r1,[r0,#value_ident]
    mov r1,#11
    str r1,[r0,#value_value1]
    mov r1,#12
    str r1,[r0,#value_value2]

    ldr r0,iAdrQueue1                       @ queue structure address
    ldr r1,iAdrstItem
    bl pushQueue                            @ add item in queue
    cmp r0,#-1                              @ error ?
    beq 99f
    @ init item 2
    ldr r0,iAdrstItem
    mov r1,#2
    str r1,[r0,#value_ident]
    mov r1,#21
    str r1,[r0,#value_value1]
    mov r1,#22
    str r1,[r0,#value_value2]

    ldr r0,iAdrQueue1                       @ queue structure address
    ldr r1,iAdrstItem
    bl pushQueue                            @ add item in queue
    cmp r0,#-1
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
    ldr r0,iAdrQueue1                       @ queue structure address
    bl popQueue                             @ return address item
    cmp r0,#-1                              @ error ?
    beq 99f
    mov r2,r0                               @ save item pointer
    ldr r0,[r2,#value_ident]
    ldr r1,iAdrsMessIdent                   @ display ident
    bl conversion10                         @ decimal conversion
    ldr r0,[r2,#value_value1]
    ldr r1,iAdrsMessValue1                  @ display value 1
    bl conversion10                         @ decimal conversion
    ldr r0,[r2,#value_value2]
    ldr r1,iAdrsMessValue2                  @ display value 2
    bl conversion10                         @ decimal conversion
    ldr r0,iAdrszMessResult
    bl affichageMess                        @ display message
    b 4b                                    @ loop

99:
    @ error
    ldr r0,iAdrszMessError
    bl affichageMess
100:                                        @ standard end of the program
    mov r0, #0                              @ return code
    mov r7, #EXIT                           @ request to exit program
    svc #0                                  @ perform the system call

iAdrQueue1:               .int Queue1
iAdrstItem:               .int stItem
iAdrszMessError:          .int szMessError
iAdrszMessEmpty:          .int szMessEmpty
iAdrszMessNotEmpty:       .int szMessNotEmpty
iAdrszMessResult:         .int szMessResult
iAdrszCarriageReturn:     .int szCarriageReturn
iAdrsMessIdent:           .int sMessIdent
iAdrsMessValue1:          .int sMessValue1
iAdrsMessValue2:          .int sMessValue2
/******************************************************************/
/*     test if queue empty                                        */
/******************************************************************/
/* r0 contains the address of queue structure */
isEmpty:
    push {r1,r2,lr}                         @ save  registres
    ldr r1,[r0,#queue_ptdeb]                @ begin pointer
    ldr r2,[r0,#queue_ptfin]                @ begin pointer
    cmp r1,r2
    moveq r0,#1                             @ empty queue
    movne r0,#0                             @ not empty
    pop {r1,r2,lr}                          @ restaur registers
    bx lr                                   @ return
/******************************************************************/
/*     add item  in queue                                         */
/******************************************************************/
/* r0 contains the address of queue structure */
/* r1 contains the address of item            */
pushQueue:
    push {r1-r4,lr}                         @ save  registres
    add r2,r0,#queue_stvalue                @ address of values structure
    ldr r3,[r0,#queue_ptfin]                @ end pointer
    add r2,r3                               @ free address of queue
    ldr r4,[r1,#value_ident]                @ load ident item
    str r4,[r2,#value_ident]                @ and store in queue
    ldr r4,[r1,#value_value1]               @ idem
    str r4,[r2,#value_value1]
    ldr r4,[r1,#value_value2]
    str r4,[r2,#value_value2]
    add r3,#value_fin
    cmp r3,#value_fin * NBMAXIELEMENTS
    moveq r0,#-1                            @ error
    beq 100f
    str r3,[r0,#queue_ptfin]                @ store new end pointer
100:
    pop {r1-r4,lr}                          @ restaur registers
    bx lr                                   @ return
/******************************************************************/
/*     pop queue                                                  */
/******************************************************************/
/* r0 contains the address of queue structure */
popQueue:
    push {r1,r2,lr}                         @ save  registres
    mov r1,r0                               @ control if empty queue
    bl isEmpty
    cmp r0,#1                               @ yes -> error
    moveq r0,#-1
    beq 100f
    mov r0,r1
    ldr r1,[r0,#queue_ptdeb]                @ begin pointer
    add r2,r0,#queue_stvalue                @ address of begin values item
    add r2,r1                               @ address of item
    add r1,#value_fin
    str r1,[r0,#queue_ptdeb]                @ store nex begin pointer
    mov r0,r2                               @ return pointer item
100:
    pop {r1,r2,lr}                          @ restaur registers
    bx lr                                   @ return
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

{{output}}
Empty queue.

Not empty queue.

Ident :1           value 1 :11          value 2 :12

Ident :2           value 1 :21          value 2 :22

Error detected !!!!.


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


## AWK


```awk
#!/usr/bin/awk -f

BEGIN {
    delete q
    print "empty? " emptyP()
    print "push " push("a")
    print "push " push("b")
    print "empty? " emptyP()
    print "pop " pop()
    print "pop " pop()
    print "empty? " emptyP()
    print "pop " pop()
}

function push(n) {
    q[length(q)+1] = n
    return n
}

function pop() {
    if (emptyP()) {
        print "Popping from empty queue."
        exit
    }
    r = q[length(q)]
    delete q[length(q)]
    return r
}

function emptyP() {
    return length(q) == 0
}

```


{{out}}

```txt

 empty? 1
 push a
 push b
 empty? 0
 pop b
 pop a
 empty? 1
 Popping from empty queue.

```



## Batch File


This solution uses an environment variable naming convention to implement a queue as a pseudo object containing a pseudo dynamic array and head and tail attributes, as well as an empty "method" that is a sort of macro.
The implementation depends on delayed expansion being enabled at the time of each call to a queue function.
More complex variations can be written that remove this limitation.


```dos

@echo off
setlocal enableDelayedExpansion

:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: FIFO queue usage

:: Define the queue
call :newQueue myQ

:: Populate the queue
for %%A in (value1 value2 value3) do call :enqueue myQ %%A

:: Test if queue is empty by examining the tail "attribute"
if myQ.tail==0 (echo myQ is empty) else (echo myQ is NOT empty)

:: Peek at the head of the queue
call:peekQueue myQ val && echo a peek at the head of myQueue shows !val!

:: Process the first queue value
call :dequeue myQ val && echo dequeued myQ value=!val!

:: Add some more values to the queue
for %%A in (value4 value5 value6) do call :enqueue myQ %%A

:: Process the remainder of the queue
:processQueue
call :dequeue myQ val || goto :queueEmpty
echo dequeued myQ value=!val!
goto :processQueue
:queueEmpty

:: Test if queue is empty using the empty "method"/"macro". Use of the
:: second IF statement serves to demonstrate the negation of the empty
:: "method". A single IF could have been used with an ELSE clause instead.
if %myQ.empty% echo myQ is empty
if not %myQ.empty% echo myQ is NOT empty
exit /b

:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: FIFO queue definition

:newQueue qName
set /a %~1.head=1, %~1.tail=0
:: Define an empty "method" for this queue as a sort of macro
set "%~1.empty=^!%~1.tail^! == 0"
exit /b

:enqueue qName value
set /a %~1.tail+=1
set %~1.!%~1.tail!=%2
exit /b

:dequeue qName returnVar
:: Sets errorlevel to 0 if success
:: Sets errorlevel to 1 if failure because queue was empty
if !%~1.tail! equ 0 exit /b 1
for %%N in (!%~1.head!) do (
  set %~2=!%~1.%%N!
  set %~1.%%N=
)
if !%~1.head! == !%~1.tail! (set /a "%~1.head=1, %~1.tail=0") else set /a %~1.head+=1
exit /b 0

:peekQueue qName returnVar
:: Sets errorlevel to 0 if success
:: Sets errorlevel to 1 if failure because queue was empty
if !%~1.tail! equ 0 exit /b 1
for %%N in (!%~1.head!) do set %~2=!%~1.%%N!
exit /b 0

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
```


Normally you would seldom use a class as depicted above, because the operations are so simple that you probably use them directly. Bracmat lists allow prepending as well as appending elements, and single elements can be removed from the beginning or from the end of a list.

Appending an element to a long list and removing an element from the end of a long list are quite expensive operations, with a cost <i>O</i>(<i>n</i>), where <i>n</i> is the number of elements in the queue.



## C


### Dynamic array

Dynamic array working as a circular buffer.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int DATA; /* type of data to store in queue */
typedef struct {
    DATA *buf;
    size_t head, tail, alloc;
} queue_t, *queue;

queue q_new()
{
    queue q = malloc(sizeof(queue_t));
    q->buf = malloc(sizeof(DATA) * (q->alloc = 4));
    q->head = q->tail = 0;
    return q;
}

int empty(queue q)
{
    return q->tail == q->head;
}

void enqueue(queue q, DATA n)
{
    if (q->tail >= q->alloc) q->tail = 0;
    q->buf[q->tail++] = n;

    // Fixed bug where it failed to resizes
    if (q->tail == q->alloc) {  /* needs more room */
        q->buf = realloc(q->buf, sizeof(DATA) * q->alloc * 2);
        if (q->head) {
            memcpy(q->buf + q->head + q->alloc, q->buf + q->head,
                sizeof(DATA) * (q->alloc - q->head));
            q->head += q->alloc;
        } else
            q->tail = q->alloc;
        q->alloc *= 2;
    }
}

int dequeue(queue q, DATA *n)
{
    if (q->head == q->tail) return 0;
    *n = q->buf[q->head++];
    if (q->head >= q->alloc) { /* reduce allocated storage no longer needed */
        q->head = 0;
        if (q->alloc >= 512 && q->tail < q->alloc / 2)
            q->buf = realloc(q->buf, sizeof(DATA) * (q->alloc/=2));
    }
    return 1;
}
```



### Doubly linked list


```c
#include <stdio.h>
#include <stdlib.h>

typedef struct node_t node_t, *node, *queue;
struct node_t { int val; node prev, next; };

#define HEAD(q) q->prev
#define TAIL(q) q->next
queue q_new()
{
    node q = malloc(sizeof(node_t));
    q->next = q->prev = 0;
    return q;
}

int empty(queue q)
{
    return !HEAD(q);
}

void enqueue(queue q, int n)
{
    node nd = malloc(sizeof(node_t));
    nd->val = n;
    if (!HEAD(q)) HEAD(q) = nd;
    nd->prev = TAIL(q);
    if (nd->prev) nd->prev->next = nd;
    TAIL(q) = nd;
    nd->next = 0;
}

int dequeue(queue q, int *val)
{
    node tmp = HEAD(q);
    if (!tmp) return 0;
    *val = tmp->val;

    HEAD(q) = tmp->next;
    if (TAIL(q) == tmp) TAIL(q) = 0;
    free(tmp);

    return 1;
}

```


'''Test code'''
This main function works with both implementions above.

```c
int main()
{
    int i, n;
    queue q = q_new();

    for (i = 0; i < 100000000; i++) {
        n = rand();
        if (n > RAND_MAX / 2) {
        //  printf("+ %d\n", n);
            enqueue(q, n);
        } else {
            if (!dequeue(q, &n)) {
            //  printf("empty\n");
                continue;
            }
        //  printf("- %d\n", n);
        }
    }
    while (dequeue(q, &n));// printf("- %d\n", n);

    return 0;
}
```


Of the above two programs, for int types the array method is about twice as fast for the test code given.  The doubly linked list is marginally faster than the <code>sys/queue.h</code> below.


### sys/queue.h

Using the <tt>sys/queue.h</tt>, which is not POSIX.1-2001 (but it is BSD). The example allows to push/pop int values, but instead of <tt>int</tt> one can use <tt>void *</tt> and push/pop any kind of "object" (of course changes to the commodity functions <tt>m_queue</tt> and <tt>m_dequeue</tt> are needed)


```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include <sys/queue.h>

struct entry {
  int value;
  TAILQ_ENTRY(entry) entries;
};

typedef struct entry entry_t;

TAILQ_HEAD(FIFOList_s, entry);

typedef struct FIFOList_s FIFOList;


bool m_enqueue(int v, FIFOList *l)
{
  entry_t *val;
  val = malloc(sizeof(entry_t));
  if ( val != NULL ) {
    val->value = v;
    TAILQ_INSERT_TAIL(l, val, entries);
    return true;
  }
  return false;
}

bool m_dequeue(int *v, FIFOList *l)
{
  entry_t *e = l->tqh_first;
  if ( e != NULL ) {
    *v = e->value;
    TAILQ_REMOVE(l, e, entries);
    free(e);
    return true;
  }
  return false;
}

bool isQueueEmpty(FIFOList *l)
{
  if ( l->tqh_first == NULL ) return true;
  return false;
}
```



## C++

{{works with|g++|4.1.2 20061115 (prerelease) (Debian 4.1.1-21)}}
C++ already has a class <code>queue</code> in the standard library, however the following is a simple implementation based on a singly linkes list. Note that an empty queue is internally represented by <code>head == 0</code>, therefore it doesn't matter that the <code>tail</code> value is invalid in that case.

```cpp
namespace rosettacode
{
  template<typename T> class queue
  {
  public:
    queue();
    ~queue();
    void push(T const& t);
    T pop();
    bool empty();
  private:
    void drop();
    struct node;
    node* head;
    node* tail;
  };

  template<typename T> struct queue<T>::node
  {
    T data;
    node* next;
    node(T const& t): data(t), next(0) {}
  };

  template<typename T>
   queue<T>::queue():
    head(0)
  {
  }

  template<typename T>
   inline void queue<T>::drop()
  {
    node* n = head;
    head = head->next;
    delete n;
  }

  template<typename T>
   queue<T>::~queue()
  {
    while (!empty())
      drop();
  }

  template<typename T>
   void queue<T>::push(T const& t)
  {
    node*& next = head? tail->next : head;
    next = new node(t);
    tail = next;
  }

  template<typename T>
   T queue<T>::pop()
  {
    T tmp = head->data;
    drop();
    return tmp;
  }

  template<typename T>
   bool queue<T>::empty()
  {
    return head == 0;
  }
}
```



## C#

Compatible with C# 3.0 specification, requires System library for exceptions (from either .Net or Mono). A FIFO class in C# using generics and nodes.

```csharp>public class FIFO<T

{
  class Node
  {
    public T Item { get; set; }
    public Node Next { get; set; }
  }
  Node first = null;
  Node last = null;
  public void push(T item)
  {
    if (empty())
    {
      //Uses object initializers to set fields of new node
      first = new Node() { Item = item, Next = null };
      last = first;
    }
    else
    {
      last.Next = new Node() { Item = item, Next = null };
      last = last.Next;
    }
  }
  public T pop()
  {
    if (first == null)
      throw new System.Exception("No elements");
    if (last == first)
      last = null;
    T temp = first.Item;
    first = first.Next;
    return temp;
  }
  public bool empty()
  {
    return first == null;
  }
}
```



## Clojure

The "pop" function implies mutating the input, but since Clojure data structures are immutable we use a mutable reference to an immutable data structure; in this case an <tt>atom</tt> holding a vector:


```clojure
(defn make-queue []
  (atom []))

(defn enqueue [q x]
  (swap! q conj x))

(defn dequeue [q]
  (if-let [[f & r] (seq @q)]
    (do (reset! q r) f)
    (throw (IllegalStateException. "Can't pop an empty queue."))))

(defn queue-empty? [q]
  (empty? @q))
```


The implementation is thread-safe if there is at most one writer thread, i.e. only one thread ever calls <code>dequeue</code> on a given queue.


## CoffeeScript


```coffeescript

# Implement a fifo as an array of arrays, to
# greatly amortize dequeue costs, at some expense of
# memory overhead and insertion time.  The speedup
# depends on the underlying JS implementation, but
# it's significant on node.js.
Fifo = ->
  max_chunk = 512
  arr = [] # array of arrays
  count = 0

  self =
    enqueue: (elem) ->
      if count == 0 or arr[arr.length-1].length >= max_chunk
        arr.push []
      count += 1
      arr[arr.length-1].push elem
    dequeue: (elem) ->
      throw Error("queue is empty") if count == 0
      val = arr[0].shift()
      count -= 1
      if arr[0].length == 0
        arr.shift()
      val
    is_empty: (elem) ->
      count == 0

# test
do ->
  max = 5000000
  q = Fifo()
  for i in [1..max]
    q.enqueue
      number: i

  console.log q.dequeue()
  while !q.is_empty()
    v = q.dequeue()
  console.log v

```

{{out}}

```txt

> time coffee fifo.coffee
{ number: 1 }
{ number: 5000000 }

real    0m2.394s
user    0m2.089s
sys 0m0.265s

```



## Common Lisp


This defines a queue structure that stores its items in a list, and maintains a tail pointer (i.e., a pointer to the last cons in the list).  Note that dequeuing the last item in the queue does not clear the tail pointer—enqueuing into the resulting empty queue will correctly reset the tail pointer.


```lisp
(defstruct (queue (:constructor %make-queue))
  (items '() :type list)
  (tail '() :type list))

(defun make-queue ()
  "Returns an empty queue."
  (%make-queue))

(defun queue-empty-p (queue)
  "Returns true if the queue is empty."
  (endp (queue-items queue)))

(defun enqueue (item queue)
  "Enqueue item in queue. Returns the queue."
  (prog1 queue
    (if (queue-empty-p queue)
      (setf (queue-items queue) (list item)
            (queue-tail queue) (queue-items queue))
      (setf (cdr (queue-tail queue)) (list item)
            (queue-tail queue) (cdr (queue-tail queue))))))

(defun dequeue (queue)
  "Dequeues an item from queue. Signals an error if queue is empty."
  (if (queue-empty-p queue)
    (error "Cannot dequeue from empty queue.")
    (pop (queue-items queue))))
```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE Queue;
IMPORT
	Boxes;
TYPE
	Instance* = POINTER TO LIMITED RECORD
		size: LONGINT;
		first,last: LONGINT;
		_queue: POINTER TO ARRAY OF Boxes.Box;
	END;

	PROCEDURE (self: Instance) Initialize(capacity: LONGINT),NEW;
	BEGIN
		self.size := 0;
		self.first := 0;
		self.last := 0;
		NEW(self._queue,capacity)
	END Initialize;

	PROCEDURE New*(capacity: LONGINT): Instance;
	VAR
		aQueue: Instance;
	BEGIN
		NEW(aQueue);aQueue.Initialize(capacity);RETURN aQueue
	END New;

	PROCEDURE (self: Instance) IsEmpty*(): BOOLEAN, NEW;
	BEGIN
		RETURN self.size = 0;
	END IsEmpty;

	PROCEDURE (self: Instance) Capacity*(): LONGINT, NEW;
	BEGIN
		RETURN LEN(self._queue)
	END Capacity;

	PROCEDURE (self: Instance) Size*(): LONGINT, NEW;
	BEGIN
		RETURN self.size
	END Size;

	PROCEDURE (self: Instance) IsFull*(): BOOLEAN, NEW;
	BEGIN
		RETURN self.size = self.Capacity()
	END IsFull;

	PROCEDURE (self: Instance) Push*(b: Boxes.Box), NEW;
	VAR
		i, j, newCapacity, oldSize: LONGINT;
		queue: POINTER TO ARRAY OF Boxes.Box;
	BEGIN
		INC(self.size);
		self._queue[self.last] := b;
		self.last := (self.last + 1) MOD self.Capacity();
		IF self.IsFull() THEN
			(* grow queue *)
			newCapacity := self.Capacity() + (self.Capacity() DIV 2);
			(* new queue *)
			NEW(queue,newCapacity);
			(* move data from old to new queue *)
			i := self.first; j := 0; oldSize := self.Capacity() - self.first + self.last;
			WHILE (j < oldSize) & (j < newCapacity - 1) DO
				queue[j] := self._queue[i];
				i := (i + 1) MOD newCapacity;INC(j)
			END;
			self._queue := queue;self.first := 0;self.last := j
		END
	END Push;

	PROCEDURE (self: Instance) Pop*(): Boxes.Box, NEW;
	VAR
		b: Boxes.Box;
	BEGIN
		ASSERT(~self.IsEmpty());
		DEC(self.size);
		b := self._queue[self.first];
		self._queue[self.first] := NIL;
		self.first := (self.first + 1) MOD self.Capacity();
		RETURN b
	END Pop;

END Queue.

```

Interface extracted from implementation

```oberon2

DEFINITION Queue;

	IMPORT Boxes;

	TYPE
		Instance = POINTER TO LIMITED RECORD
			(self: Instance) Capacity (): LONGINT, NEW;
			(self: Instance) IsEmpty (): BOOLEAN, NEW;
			(self: Instance) IsFull (): BOOLEAN, NEW;
			(self: Instance) Pop (): Boxes.Box, NEW;
			(self: Instance) Push (b: Boxes.Box), NEW;
			(self: Instance) Size (): LONGINT, NEW
		END;

	PROCEDURE New (capacity: LONGINT): Instance;

END Queue.


```



## D

See code here: http://rosettacode.org/wiki/Queue/Usage#D

=={{header|Déjà Vu}}==
This uses a dictionary to have a sort of [[wp:Circular_buffer|circular buffer]] of infinite size.

```dejavu
queue:
	{ :start 0 :end 0 }

enqueue q item:
	set-to q q!end item
	set-to q :end ++ q!end

dequeue q:
	if empty q:
		Raise :value-error "popping from empty queue"
	q! q!start
	delete-from q q!start
	set-to q :start ++ q!start

empty q:
	= q!start q!end
```



## E


This uses a linked list representation of queues, hanging onto both ends of the list, except that the next-link reference is an E ''promise'' rather than a mutable slot.

Also, according to E design principles, the read and write ends of the queue are separate objects. This has two advantages; first, it implements [http://wiki.erights.org/wiki/POLA POLA] by allowing only the needed end of the queue to be handed out to its users; second, if the reader end is garbage collected the contents of the queue automatically will be as well (rather than accumulating if the writer continues writing).


```e
def makeQueue() {
  def [var head, var tail] := Ref.promise()

  def writer {
    to enqueue(value) {
      def [nh, nt] := Ref.promise()
      tail.resolve([value, nh])
      tail := nt
    }
  }

  def reader {
    to empty() { return !Ref.isResolved(head) }

    to dequeue(whenEmpty) {
      if (Ref.isResolved(head)) {
        def [value, next] := head
        head := next
        return value
      } else {
        throw.eject(whenEmpty, "pop() of empty queue")
      }
    }
  }

  return [reader, writer]
}
```



## EchoLisp

There is no native queue type in EchoLisp. make-Q implements queues in message passing style, using vector operations. Conversions from-to lists are also provided.

```lisp

;; put info string in permanent storage for later use
(info 'make-Q
"usage: (define q (make-Q)) ;  (q '[top | empty? | pop | push value | to-list | from-list list])")

;; make-Q
(define (make-Q)
  (let ((q (make-vector 0)))
    (lambda (message . args)
      (case message
        ((empty?) (vector-empty? q))
        ((top) (if (vector-empty? q) (error  'Q:top:empty q) (vector-ref q 0)))
        ((push) (vector-push q (car args)))
        ((pop) (if (vector-empty? q) (error 'Q:pop:empty q) (vector-shift q)))
        ((to-list) (vector->list q))
        ((from-list) (set! q (list->vector (car args))) q )
        (else (info 'make-Q) (error  "Q:bad message:" message )))))) ; display info if unknown message

;;
(define q (make-Q))
(q 'empty?) → #t
(q 'push 'first) → first
(q 'push 'second) → second
(q 'pop) → first
(q 'pop) → second
(q 'top)
"💬 error: Q:top:empty #()"
(q 'from-list '( 6 7 8)) → #( 6 7 8)
(q 'top) → 6
(q 'pop) → 6
(q 'to-list)→ (7 8)
(q 'delete)
"💭 error: Q:bad message: delete"

;; save make-Q
(local-put 'make-Q)

```


## Elena

ELENA 4.x :

```elena
import extensions;

template queue<T>
{
    T[]  theArray;
    int  theTop;
    int  theTale;

    constructor()
    {
        theArray := new T[](8);
        theTop := 0;
        theTale := 0;
    }

    bool empty()
        = theTop == theTale;

    push(T object)
    {
        if (theTale > theArray.Length)
        {
            theArray := theArray.reallocate(theTale)
        };

        theArray[theTale] := object;

        theTale += 1
    }

    T pop()
    {
        if (theTale == theTop)
            { InvalidOperationException.new:"Queue is empty".raise() };

        T item := theArray[theTop];

        theTop += 1;

        ^ item
    }
}

public program()
{
    queue<int> q := new queue<int>();
    q.push(1);
    q.push(2);
    q.push(3);
    console.printLine(q.pop());
    console.printLine(q.pop());
    console.printLine(q.pop());
    console.printLine("a queue is ", q.empty().iif("empty","not empty"));
    console.print("Trying to pop:");
    try
    {
        q.pop()
    }
    catch(Exception e)
    {
        console.printLine(e.Message)
    }
}
```

{{out}}

```txt

1
2
3
a queue is empty
Trying to pop:Queue is empty

```



## Elisa


This is a generic Queue component based on bi-directional lists. See how in Elisa these [http://jklunder.home.xs4all.nl/elisa/part01/doc080.html lists] are defined.


```Elisa

component GenericQueue ( Queue, Element );
 type Queue;
      Queue (MaxLength = integer) -> Queue;
      Length( Queue )           -> integer;
      Empty ( Queue )           -> boolean;
      Full ( Queue )            -> boolean;
      Push ( Queue, Element)    -> nothing;
      Pull ( Queue )            -> Element;
begin
      Queue (MaxLength) = Queue:[ MaxLength; length:=0; list=alist(Element) ];
      Length ( queue ) = queue.length;
      Empty ( queue ) = (queue.length <= 0);
      Full ( queue ) = (queue.length >= queue.MaxLength);

      Push ( queue, element ) =
                   [ exception (Full(queue), "Queue Overflow");
                     queue.length:= queue.length + 1;
                     add (queue.list, element)];
      Pull ( queue ) =
                   [ exception (Empty(queue), "Queue Underflow");
                     queue.length:= queue.length - 1;
                     remove(first(queue.list))];
end component GenericQueue;

```

In the following tests we will also show how the internal structure of the queue can be made visible to support debugging.

```Elisa

use GenericQueue (QueueofPersons, Person);
type Person = text;
Q = QueueofPersons(25);

Push (Q, "Peter");
Push (Q, "Alice");
Push (Q, "Edward");
Q?
QueueofPersons:[MaxLength = 25;
                length = 3;
                list = { "Peter",
                         "Alice",
                         "Edward"}]
Pull (Q)?
"Peter"

Pull (Q)?
"Alice"

Pull (Q)?
"Edward"

Q?
QueueofPersons:[MaxLength = 25;
                length = 0;
                list = { }]

Pull (Q)?
***** Exception: Queue Underflow

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Queue do
  def new, do: {Queue, [], []}

  def push({Queue, input, output}, x), do: {Queue, [x|input], output}

  def pop({Queue, [], []}), do: (raise RuntimeError, message: "empty Queue")
  def pop({Queue, input, []}), do: pop({Queue, [], Enum.reverse(input)})
  def pop({Queue, input, [h|t]}), do: {h, {Queue, input, t}}

  def empty?({Queue, [], []}), do: true
  def empty?({Queue, _, _}), do: false
end
```


Example:

```txt

iex(1)> c("queue.ex")
[Queue]
iex(2)> q = Queue.new
{Queue, [], []}
iex(3)> Queue.empty?(q)
true
iex(4)> q2 = Queue.push(q,1)
{Queue, [1], []}
iex(5)> q3 = Queue.push(q2,2)
{Queue, [2, 1], []}
iex(6)> Queue.empty?(q3)
false
iex(7)> Queue.pop(q3)
{1, {Queue, [], [2]}}
iex(8)> {popped, ^q} = Queue.pop(q2)
{1, {Queue, [], []}}
iex(9)> Queue.pop(Queue.new)
** (RuntimeError) empty Queue
    queue.ex:6: Queue.pop/1

```



## Erlang

The standard way to manage fifo in functional programming is to use a pair of list for the fifo queue, one is the input, the other is the output.
When the output is empty just take the input list and reverse it.

```Erlang
-module(fifo).
-export([new/0, push/2, pop/1, empty/1]).

new() -> {fifo, [], []}.

push({fifo, In, Out}, X) -> {fifo, [X|In], Out}.

pop({fifo, [], []}) -> erlang:error('empty fifo');
pop({fifo, In, []}) -> pop({fifo, [], lists:reverse(In)});
pop({fifo, In, [H|T]}) -> {H, {fifo, In, T}}.

empty({fifo, [], []}) -> true;
empty({fifo, _, _}) -> false.
```


Note that there exists a 'queue' module in the standard library handling this for you in the first place


## ERRE

With ERRE 3.0 you can use a class to define the task (in C-64 version you can simply use procedures):

```ERRE
PROGRAM CLASS_DEMO

CLASS QUEUE

   LOCAL SP
   LOCAL DIM STACK[100]

   FUNCTION ISEMPTY()
      ISEMPTY=(SP=0)
   END FUNCTION

   PROCEDURE INIT
      SP=0
   END PROCEDURE

   PROCEDURE POP(->XX)
      XX=STACK[SP]
      SP=SP-1
   END PROCEDURE

   PROCEDURE PUSH(XX)
      SP=SP+1
      STACK[SP]=XX
   END PROCEDURE

END CLASS

NEW PILA:QUEUE

BEGIN
    PILA_INIT  ! constructor
    FOR N=1 TO 4 DO  ! push 4 numbers
       PRINT("Push";N)
       PILA_PUSH(N)
    END FOR
    FOR I=1 TO 5 DO  ! pop 5 numbers
       IF NOT PILA_ISEMPTY() THEN
           PILA_POP(->N)
           PRINT("Pop";N)
         ELSE
           PRINT("Queue is empty!")
       END IF
    END FOR
    PRINT("* End *")
END PROGRAM
```

{{out}}

```txt
Push 1
Push 2
Push 3
Push 4
Pop 4
Pop 3
Pop 2
Pop 1
Queue is empty!
* End *
```



## Factor

{{trans|Java}}

```factor
USING: accessors kernel ;
IN: rosetta-code.queue-definition

TUPLE: queue head tail ;
TUPLE: node value next ;

: <queue> ( -- queue ) queue new ;
: <node> ( obj -- node ) node new swap >>value ;

: empty? ( queue -- ? ) head>> >boolean not ;

: enqueue ( obj queue -- )
    [ <node> ] dip 2dup dup empty?
    [ head<< ] [ tail>> next<< ] if tail<< ;

: dequeue ( queue -- obj )
    dup empty? [ "Cannot dequeue empty queue." throw ] when
    [ head>> value>> ] [ head>> next>> ] [ head<< ] tri ;
```



## Fantom



```fantom

class Queue
{
  List queue := [,]

  public Void push (Obj obj)
  {
    queue.add (obj)  // add to right of list
  }

  public Obj pop ()
  {
    if (queue.isEmpty)
      throw (Err("queue is empty"))
    else
    {
      return queue.removeAt(0) // removes left-most item
    }
  }

  public Bool isEmpty ()
  {
    queue.isEmpty
  }
}

```



## Forth

This is a FIFO implemented as a circular buffer, as is often found between communicating processes such the interrupt and user parts of a device driver. In practice, the get/put actions would block instead of aborting if the queue is empty/full.


```forth
1024 constant size
create buffer size cells allot
here constant end
variable head  buffer head !
variable tail  buffer tail !
variable used       0 used !

: empty?  used @ 0= ;
: full?   used @ size = ;

: next ( ptr -- ptr )
  cell+  dup end = if drop buffer then ;

: put ( n -- )
  full? abort" buffer full"
  \ begin full? while pause repeat
  tail @ !  tail @ next tail !   1 used +! ;

: get ( -- n )
  empty? abort" buffer empty"
  \ begin empty? while pause repeat
  head @ @  head @ next head !  -1 used +! ;
```



###  Linked list version


Using Forth-2012 structure words and ALLOCATE/FREE.  In spirit quite similar to the Java variant below, with one difference: Here we use addresses of fields (not possible in Java), which often makes things simpler than in Java (fewer special cases at boundaries), but in this case it does not.  Where the Java version has a special case on enqueue, this version has a special case on dequeue:


```forth

0
  field: list-next
  field: list-val
constant list-struct

: insert ( x list-addr -- )
    list-struct allocate throw >r
    swap r@ list-val !
    dup @ r@ list-next !
    r> swap ! ;

: remove ( list-addr -- x )
    >r r@ @ ( list-node )
    r@ @ dup list-val @ ( list-node x )
    swap list-next @ r> !
    swap free throw ;

0
  field: queue-last \ points to the last entry (head of the list)
  field: queue-nextaddr \ points to the pointer to the next-inserted entry
constant queue-struct

: init-queue ( queue -- )
    >r 0 r@ queue-last !
    r@ queue-last r> queue-nextaddr ! ;

: make-queue ( -- queue )
    queue-struct allocate throw dup init-queue ;

: empty? ( queue -- f )
    queue-last @ 0= ;

: enqueue ( x queue -- )
    dup >r queue-nextaddr @ insert
    r@ queue-nextaddr @ @ list-next r> queue-nextaddr ! ;

: dequeue ( queue -- x )
    dup empty? abort" dequeue applied to an empty queue"
    dup queue-last remove ( queue x )
    over empty? if
        over init-queue then
    nip ;

```



## Fortran

{{works with|Fortran|90 and later}}

See [[FIFO (usage)]] for an example of <code>fifo_nodes</code>


```fortran
module FIFO
  use fifo_nodes
! fifo_nodes must define the type fifo_node, with the two field
! next and valid, for queue handling, while the field datum depends
! on the usage (see [[FIFO (usage)]] for an example)
!  type fifo_node
!     integer :: datum
!     ! the next part is not variable and must be present
!     type(fifo_node), pointer :: next
!     logical :: valid
!  end type fifo_node

  type fifo_head
     type(fifo_node), pointer :: head, tail
  end type fifo_head

contains

  subroutine new_fifo(h)
    type(fifo_head), intent(out) :: h
    nullify(h%head)
    nullify(h%tail)
  end subroutine new_fifo

  subroutine fifo_enqueue(h, n)
    type(fifo_head), intent(inout) :: h
    type(fifo_node), intent(inout), target :: n

    if ( associated(h%tail) ) then
       h%tail%next => n
       h%tail => n
    else
       h%tail => n
       h%head => n
    end if

    nullify(n%next)
  end subroutine fifo_enqueue

  subroutine fifo_dequeue(h, n)
    type(fifo_head), intent(inout) :: h
    type(fifo_node), intent(out), target :: n

    if ( associated(h%head) ) then
       n = h%head
       if ( associated(n%next) ) then
          h%head => n%next
       else
          nullify(h%head)
          nullify(h%tail)
       end if
       n%valid = .true.
    else
       n%valid = .false.
    end if
    nullify(n%next)
  end subroutine fifo_dequeue

  function fifo_isempty(h) result(r)
    logical :: r
    type(fifo_head), intent(in) :: h
    if ( associated(h%head) ) then
       r = .false.
    else
       r = .true.
    end if
  end function fifo_isempty

end module FIFO
```



## FreeBASIC

We first use a macro to define a generic Queue type :

```freebasic
' FB 1.05.0 Win64

' queue_rosetta.bi
' simple generic Queue type

#Define Queue(T) Queue_##T

#Macro Declare_Queue(T)
Type Queue(T)
 Public:
    Declare Constructor()
    Declare Destructor()
    Declare Property capacity As Integer
    Declare Property count As Integer
    Declare Property empty As Boolean
    Declare Property front As T
    Declare Function pop() As T
    Declare Sub push(item As T)
  Private:
    a(any) As T
    count_ As Integer = 0
    Declare Function resize(size As Integer) As Integer
End Type

Constructor Queue(T)()
  Redim a(0 To 0) '' create a default T instance for various purposes
End Constructor

Destructor Queue(T)()
  Erase a
End Destructor

Property Queue(T).capacity As Integer
  Return UBound(a)
End Property

Property Queue(T).count As Integer
  Return count_
End Property

Property Queue(T).empty As Boolean
  Return count_ = 0
End Property

Property Queue(T).front As T
  If count_ > 0 Then
    Return a(1)
  End If
  Print "Error: Attempted to access 'front' element of an empty queue"
  Return a(0)  '' return default element
End Property

Function Queue(T).pop() As T
  If count_ > 0 Then
    Dim value As T = a(1)
    If count_ > 1 Then  '' move remaining elements to fill space vacated
      For i As Integer = 2 To count_
        a(i - 1) = a(i)
      Next
    End If
    a(count_) = a(0)  '' zero last element
    count_ -= 1
    Return value
  End If
  Print "Error: Attempted to remove 'front' element of an empty queue"
  Return a(0)  '' return default element
End Function

Sub Queue(T).push(item As T)
  Dim size As Integer = UBound(a)
  count_ += 1
  If count_ >  size Then
    size = resize(size)
    Redim Preserve a(0 to size)
  End If
  a(count_) = item
End Sub

Function Queue(T).resize(size As Integer) As Integer
  If size = 0 Then
    size = 4
  ElseIf size <= 32 Then
    size  = 2 * size
  Else
    size += 32
  End If
  Return size
End Function
#EndMacro
```

We now use this type to create a Queue of Cat instances :

```freebasic
' FB 1.05.0 Win64

#Include "queue_rosetta.bi"

Type Cat
  name As String
  age As Integer
  Declare Constructor
  Declare Constructor(name_ As string, age_ As integer)
  Declare Operator Cast() As String
end type

Constructor Cat  '' default constructor
End Constructor

Constructor Cat(name_ As String, age_ As Integer)
  name = name_
  age = age_
End Constructor

Operator Cat.Cast() As String
  Return "[" + name + ", " + Str(age) + "]"
End Operator

Declare_Queue(Cat) '' expand Queue type for Cat instances

Dim CatQueue As Queue(Cat)

Var felix  = Cat("Felix", 8)
Var sheba  = Cat("Sheba", 4)
Var fluffy = Cat("Fluffy", 2)
With CatQueue  '' push these Cat instances into the Queue
  .push(felix)
  .push(sheba)
  .push(fluffy)
End With
Print "Number of Cats in the Queue :" ; CatQueue.count
Print "Capacity of Cat Queue       :" ; CatQueue.capacity
Print "Front Cat                   : "; CatQueue.front
CatQueue.pop()
Print "Front Cat now               : "; CatQueue.front
Print "Number of Cats in the Queue :" ; CatQueue.count
CatQueue.pop()
Print "Front Cat now               : "; CatQueue.front
Print "Number of Cats in the Queue :" ; CatQueue.count
Print "Is Queue empty now          : "; CatQueue.empty
catQueue.pop()
Print "Number of Cats in the Queue :" ; CatQueue.count
Print "Is Queue empty now          : "; CatQueue.empty
catQueue.pop()
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

Number of Cats in the Queue : 3
Capacity of Cat Queue       : 4
Front Cat                   : [Felix, 8]
Front Cat now               : [Sheba, 4]
Number of Cats in the Queue : 2
Front Cat now               : [Fluffy, 2]
Number of Cats in the Queue : 1
Is Queue empty now          : false
Number of Cats in the Queue : 0
Is Queue empty now          : true
Error: Attempted to remove 'front' element of an empty queue

```



## Free Pascal


```pascal
program queue;
 {$IFDEF FPC}{$MODE DELPHI}{$IFDEF WINDOWS}{$APPTYPE CONSOLE}{$ENDIF}{$ENDIF}
 {$ASSERTIONS ON}
uses Generics.Collections;

var
  lQueue: TQueue<Integer>;
begin
  lQueue := TQueue<Integer>.Create;
  try
    lQueue.EnQueue(1);
    lQueue.EnQueue(2);
    lQueue.EnQueue(3);
    Write(lQueue.DeQueue:2);   // 1
    Write(lQueue.DeQueue:2);   // 2
    Writeln(lQueue.DeQueue:2); // 3
    Assert(lQueue.Count = 0, 'Queue is not empty'); // should be empty
  finally
    lQueue.Free;
  end;
end.
```


```txt

Output:
 1 2 3
```



## GAP


```gap
Enqueue := function(v, x)
    Add(v[1], x);
end;

Dequeue := function(v)
    if IsEmpty(v[2]) then
        if IsEmpty(v[1]) then
            return fail;
        else
            v[2] := Reversed(v[1]);
            v[1] := [];
        fi;
    fi;
    return Remove(v[2]);
end;


# a new queue
v := [[], []];

Enqueue(v, 3);
Enqueue(v, 4);
Enqueue(v, 5);
Dequeue(v);
# 3
Enqueue(v, 6);
Dequeue(v);
# 4
Dequeue(v);
# 5
Dequeue(v);
# 6
Dequeue(v);
# fail
```



## Go

Hard coded to be a queue of strings.  Implementation is a circular buffer which grows as needed.

```go

package queue

// int queue
// the zero object is a valid queue ready to be used.
// items are pushed at tail, popped at head.
// tail = -1 means queue is full
type Queue struct {
    b []string
    head, tail int
}

func (q *Queue) Push(x string) {
    switch {
    // buffer full. reallocate.
    case q.tail < 0:
        next := len(q.b)
        bigger := make([]string, 2*next)
        copy(bigger[copy(bigger, q.b[q.head:]):], q.b[:q.head])
        bigger[next] = x
        q.b, q.head, q.tail = bigger, 0, next+1
    // zero object. make initial allocation.
    case len(q.b) == 0:
        q.b, q.head, q.tail = make([]string, 4), 0 ,1
        q.b[0] = x
    // normal case
    default:
        q.b[q.tail] = x
        q.tail++
        if q.tail == len(q.b) {
            q.tail = 0
        }
        if q.tail == q.head {
            q.tail = -1
        }
    }
}

func (q *Queue) Pop() (string, bool) {
    if q.head == q.tail {
        return "", false
    }
    r := q.b[q.head]
    if q.tail == -1 {
        q.tail = q.head
    }
    q.head++
    if q.head == len(q.b) {
        q.head = 0
    }
    return r, true
}

func (q *Queue) Empty() bool {
    return q.head == q.tail
}

```



## Groovy

Solution:

```groovy
class Queue {
    private List buffer

    public Queue(List buffer =  new LinkedList()) {
        assert buffer != null
        assert buffer.empty
        this.buffer = buffer
    }

    def push (def item) { buffer << item }
    final enqueue = this.&push

    def pop() {
        if (this.empty) throw new NoSuchElementException('Empty Queue')
        buffer.remove(0)
    }
    final dequeue = this.&pop

    def getEmpty() { buffer.empty }

    String toString() { "Queue:${buffer}" }
}
```


Test:

```groovy
def q = new Queue()
assert q.empty

['Crosby', 'Stills'].each { q.push(it) }
assert !q.empty
['Nash', 'Young'].each { q.enqueue(it) }
println q
assert !q.empty
assert q.pop() == 'Crosby'
println q
assert !q.empty
assert q.dequeue() == 'Stills'
println q
assert !q.empty
assert q.pop() == 'Nash'
println q
assert !q.empty
q.push('Crazy Horse')
println q
assert q.dequeue() == 'Young'
println q
assert !q.empty
assert q.pop() == 'Crazy Horse'
println q
assert q.empty
try { q.pop() } catch (NoSuchElementException e) { println e }
try { q.dequeue() } catch (NoSuchElementException e) { println e }
```


{{out}}

```txt
Queue:[Crosby, Stills, Nash, Young]
Queue:[Stills, Nash, Young]
Queue:[Nash, Young]
Queue:[Young]
Queue:[Young, Crazy Horse]
Queue:[Crazy Horse]
Queue:[]
java.util.NoSuchElementException: Empty Queue
java.util.NoSuchElementException: Empty Queue
```



## Haskell


The standard way to manage fifo in functional programming is to use a pair of list for the fifo queue, one is the input, the other is the output.
When the output is empty just take the input list and reverse it.


```haskell
data Fifo a = F [a] [a]

emptyFifo :: Fifo a
emptyFifo = F [] []

push :: Fifo a -> a -> Fifo a
push (F input output) item = F (item:input) output

pop :: Fifo a -> (Maybe a, Fifo a)
pop (F input (item:output)) = (Just item, F input output)
pop (F []    []           ) = (Nothing, F [] [])
pop (F input []           ) = pop (F [] (reverse input))

isEmpty :: Fifo a -> Bool
isEmpty (F [] []) = True
isEmpty _         = False

```


== Icon and Unicon ==

=
## Icon
=

The following works in both Icon and Unicon:


```icon

# Use a record to hold a Queue, using a list as the concrete implementation
record Queue(items)

procedure make_queue ()
  return Queue ([])
end

procedure queue_push (queue, item)
  put (queue.items, item)
end

# if the queue is empty, this will 'fail' and return nothing
procedure queue_pop (queue)
  return pop (queue.items)
end

procedure queue_empty (queue)
  return *queue.items = 0
end

# procedure to test class
procedure main ()
  queue := make_queue()

  # add the numbers 1 to 5
  every (item := 1 to 5) do
    queue_push (queue, item)

  # pop them in the added order, and show a message when queue is empty
  every (1 to 6) do {
    write ("Popped value: " || queue_pop (queue))
    if (queue_empty (queue)) then write ("empty queue")
  }
end

```


{{out}}

```txt
Popped value: 1
Popped value: 2
Popped value: 3
Popped value: 4
Popped value: 5
empty queue
empty queue

```


=
## Unicon
=

Unicon also provides classes:


```Unicon

# Use a class to hold a Queue, with a list as the concrete implementation
class Queue (items)
  method push (item)
    put (items, item)
  end

  # if the queue is empty, this will 'fail' and return nothing
  method take ()
    return pop (items)
  end

  method is_empty ()
    return *items = 0
  end

  initially () # initialises the field on creating an instance
    items := []
end

procedure main ()
  queue := Queue ()

  every (item := 1 to 5) do
    queue.push (item)

  every (1 to 6) do {
    write ("Popped value: " || queue.take ())
    if queue.is_empty () then write ("empty queue")
  }
end

```


Produces the same output as above.


## J

Object oriented technique, using mutable state:


```J
queue_fifo_=: ''

pop_fifo_=: verb define
  r=. {. ::] queue
  queue=: }.queue
  r
)

push_fifo_=: verb define
  queue=: queue,y
  y
)

isEmpty_fifo_=: verb define
  0=#queue
)
```


Function-level technique, with no reliance on mutable state:


```J
pop        =: ( {.^:notnull  ;  }. )@: > @: ]  /
push       =: ( ''  ;  ,~ )& >  /
tell_atom  =: >& {.
tell_queue =: >& {:
is_empty   =: '' -: 1 tell_queue

make_empty =: a: , a: [ ]
onto       =: [ ; }.@]

notnull    =: 0 ~: #
```


See also [[FIFO (usage)#J]]


## Java

{{works with|Java|1.5+}}
This task could be done using a LinkedList from java.util, but here is a user-defined version with generics:

```java>public class Queue<E
{
    Node<E> head = null, tail = null;

    static class Node<E>{
        E value;
        Node<E> next;

        Node(E value, Node<E> next){
            this.value= value;
            this.next= next;
        }

    }

    public Queue(){
    }

    public void enqueue(E value){ //standard queue name for "push"
        Node<E> newNode= new Node<E>(value, null);
        if(empty()){
            head= newNode;
        }else{
            tail.next = newNode;
        }
        tail= newNode;
    }

    public E dequeue() throws java.util.NoSuchElementException{//standard queue name for "pop"
        if(empty()){
            throw new java.util.NoSuchElementException("No more elements.");
        }
        E retVal= head.value;
        head= head.next;
        return retVal;
    }

    public boolean empty(){
        return head == null;
    }
}
```



## JavaScript

Most of the time, the built-in Array suffices. However, if you explicitly want to limit the usage to FIFO operations, it's easy to implement such a constructor.

=== Using built-in Array ===

```javascript
var fifo = [];
fifo.push(42); // Enqueue.
fifo.push(43);
var x = fifo.shift(); // Dequeue.
alert(x); // 42
```



###  Custom constructor function


```javascript
function FIFO() {
    this.data = new Array();

    this.push  = function(element) {this.data.push(element)}
    this.pop   = function() {return this.data.shift()}
    this.empty = function() {return this.data.length == 0}

    this.enqueue = this.push;
    this.dequeue = this.pop;
}
```


## jq

Note that since jq is a purely functional language, the entity
representing a queue must be presented as an input to any function
that is to operate on it.

The definition of pop as given below is idiomatic in jq but implies
that popping an empty queue yields [null, []] rather than an error.  An
alternative definition, pop_or_error, is also given to illustrate
how an error condition can be generated.

```jq
# An empty queue:
def fifo: [];

def push(e): [e] + .;

def pop: [.[0], .[1:]];

def pop_or_error: if length == 0 then error("pop_or_error") else pop end;

def empty: length == 0;
```

'''Examples''':

```jq
fifo | pop  # produces [null,[]]

fifo
 | push(42) # enqueue
 | push(43) # enqueue
 | pop      # dequeue
 | .[0]     # the value
# produces 43

fifo|push(1) as $q1
| fifo|push(2) as $q2
| [($q1|pop|.[0]), ($q2|pop|.[0])]
# produces: [1, 2]
```



## Julia

Julia provides a variety of queue-like methods for vectors, making the solution to this task rather straightforward.  Define a <tt>Queue</tt> in terms of a one dimensional array, and provide its methods using the appropriate vector operations.  To adhere to Julia naming conventions, the queue operations are named "push!", "pop!" and "isempty" rather than "push", "pop" and "empty".

```Julia

type Queue{T}
    a::Array{T,1}
end

Queue() = Queue(Any[])
Queue(a::DataType) = Queue(a[])
Queue(a) = Queue(typeof(a)[])

Base.isempty(q::Queue) = isempty(q.a)

function Base.pop!{T}(q::Queue{T})
    !isempty(q) || error("queue must be non-empty")
    pop!(q.a)
end

function Base.push!{T}(q::Queue{T}, x::T)
    unshift!(q.a, x)
    return q
end

function Base.push!{T}(q::Queue{Any}, x::T)
    unshift!(q.a, x)
    return q
end

```


{{out}}
It is easiest to use the REPL to show a <tt>Queue</tt> in action.


```txt

julia> q = Queue()
Queue{Any}({})

julia> isempty(q)
true

julia> push!(q, 1)
Queue{Any}({1})

julia> isempty(q)
false

julia> push!(q, "two")
Queue{Any}({"two",1})

julia> push!(q, 3.0)
Queue{Any}({3.0,"two",1})

julia> push!(q, false)
Queue{Any}({false,3.0,"two",1})

julia> pop!(q)
1

julia> pop!(q)
"two"

julia> pop!(q)
3.0

julia> pop!(q)
false

julia> pop!(q)
ERROR: queue must be non-empty
 in pop! at none:2

```



## Kotlin


```scala
// version 1.1.2

import java.util.LinkedList

class Queue<E> {
    private val data = LinkedList<E>()

    val size get() = data.size

    val empty get() = size == 0

    fun push(element: E) = data.add(element)

    fun pop(): E {
        if (empty) throw RuntimeException("Can't pop elements from an empty queue")
        return data.removeFirst()
    }

   val top: E
        get() {
            if (empty) throw RuntimeException("Empty queue can't have a top element")
            return data.first()
        }

    fun clear() = data.clear()

    override fun toString() = data.toString()
}

fun main(args: Array<String>) {
    val q = Queue<Int>()
    (1..5).forEach { q.push(it) }
    println(q)
    println("Size of queue = ${q.size}")
    print("Popping: ")
    (1..3).forEach { print("${q.pop()} ") }
    println("\nRemaining in queue: $q")
    println("Top element is now ${q.top}")
    q.clear()
    println("After clearing, queue is ${if(q.empty) "empty" else "not empty"}")
    try {
        q.pop()
    }
    catch (e: Exception) {
        println(e.message)
    }
}
```


{{out}}

```txt

[1, 2, 3, 4, 5]
Size of queue = 5
Popping: 1 2 3
Remaining in queue: [4, 5]
Top element is now 4
After clearing, queue is empty
Can't pop elements from an empty queue

```



## LabVIEW

{{VI solution|LabVIEW_Queue_Definition.png}}


## Lasso

Definition:

```lasso>define myqueue =
 type {
    data store = list

    public onCreate(...) => {
        if(void != #rest) => {
            with item in #rest do .`store`->insert(#item)
        }
    }

    public push(value) => .`store`->insertLast(#value)

    public pop => {
        handle => {
            .`store`->removefirst
        }

        return .`store`->first
    }

    public isEmpty => (.`store`->size == 0)
}
```


Usage:

```lasso
local(q) = myqueue('a')
#q->isEmpty
// => false

#q->push('b')
#q->pop
// => a
#q->pop
// => b

#q->isEmpty
// => true
#q->pop
// => void
```




## Lua


```lua
Queue = {}

function Queue.new()
    return { first = 0, last = -1 }
end

function Queue.push( queue, value )
    queue.last = queue.last + 1
    queue[queue.last] = value
end

function Queue.pop( queue )
    if queue.first > queue.last then
        return nil
    end

    local val = queue[queue.first]
    queue[queue.first] = nil
    queue.first = queue.first + 1
    return val
end

function Queue.empty( queue )
    return queue.first > queue.last
end
```



## M2000 Interpreter

A Stack object can be used as LIFO or FIFO. Data push to bottom of stack. Read pop a value to a variable from top of stack.

```M2000 Interpreter

Module Checkit {
      a=Stack
      Stack a {
            Data 100,200, 300
      }
      Stack a {
            While not empty {
                  Read N
                  Print N
            }
      }
}
Checkit

```



## Mathematica


```Mathematica
EmptyQ[a_] := Length[a] == 0
SetAttributes[Push, HoldAll]; Push[a_, elem_] := AppendTo[a, elem]
SetAttributes[Pop, HoldAllComplete]; Pop[a_] := If[EmptyQ[a], False, b = First[a]; Set[a, Most[a]]; b]
```


=={{header|MATLAB}} / {{header|Octave}}==

Here is a simple implementation of a queue, that works in Matlab and Octave.

```matlab
myfifo = {};

% push
myfifo{end+1} = x;

% pop
x = myfifo{1};  myfifo{1} = [];

% empty
isempty(myfifo)
```


Below is another solution, that encapsulates the fifo within the object-orientated "class" elements supported by Matlab. For this to work it must be saved in a file named "FIFOQueue.m" in a folder named "@FIFOQueue" in your current Matlab directory.

```MATLAB
%This class impliments a standard FIFO queue.
classdef FIFOQueue

    properties
        queue
    end

    methods

        %Class constructor
        function theQueue = FIFOQueue(varargin)

            if isempty(varargin) %No input arguments

                %Initialize the queue state as empty
                theQueue.queue = {};
            elseif (numel(varargin) > 1) %More than 1 input arg

                %Make the queue the list of input args
                theQueue.queue = varargin;
            elseif iscell(varargin{:}) %If the only input is a cell array

                %Make the contents of the cell array the elements in the queue
                theQueue.queue = varargin{:};
            else %There is one input argument that is not a cell

                %Make that one arg the only element in the queue
                theQueue.queue = varargin;
            end

        end

        %push() - pushes a new element to the end of the queue
        function push(theQueue,varargin)

            if isempty(varargin)
                theQueue.queue(end+1) = {[]};
            elseif (numel(varargin) > 1) %More than 1 input arg

                %Make the queue the list of input args
                theQueue.queue( end+1:end+numel(varargin) ) = varargin;
            elseif iscell(varargin{:}) %If the only input is a cell array

                %Make the contents of the cell array the elements in the queue
                theQueue.queue( end+1:end+numel(varargin{:}) ) = varargin{:};
            else %There is one input argument that is not a cell

                %Make that one arg the only element in the queue
                theQueue.queue{end+1} = varargin{:};
            end

            %Makes changes to the queue permanent
            assignin('caller',inputname(1),theQueue);

        end

        %pop() - pops the first element off the queue
        function element = pop(theQueue)

            if empty(theQueue)
                error 'The queue is empty'
            else
                %Returns the first element in the queue
                element = theQueue.queue{1};

                %Removes the first element from the queue
                theQueue.queue(1) = [];

                %Makes changes to the queue permanent
                assignin('caller',inputname(1),theQueue);
            end
        end

        %empty() - Returns true if the queue is empty
        function trueFalse = empty(theQueue)

            trueFalse = isempty(theQueue.queue);

        end

    end %methods
end
```


Sample usage:

```MATLAB>>
 myQueue = FIFOQueue({'hello'})

myQueue =

    FIFOQueue

>> push(myQueue,'jello')
>> pop(myQueue)

ans =

hello

>> pop(myQueue)

ans =

jello

>> pop(myQueue)
??? Error using ==> FIFOQueue.FIFOQueue>FIFOQueue.pop at 61
The queue is empty
```



## Maxima


```maxima
defstruct(queue(in=[], out=[]))$

enqueue(x, q) := (q@in: cons(x, q@in), done)$

dequeue(q) := (if not emptyp(q@out) then first([first(q@out), q@out: rest(q@out)])
elseif emptyp(q@in) then 'fail
else (q@out: reverse(q@in), q@in: [], first([first(q@out), q@out: rest(q@out)])))$

q:new(queue);    /* queue([], []) */
enqueue(1, q)$
enqueue(2, q)$
enqueue(3, q)$
dequeue(q);      /* 1 */
enqueue(4, q)$
dequeue(q);      /* 2 */
dequeue(q);      /* 3 */
dequeue(q);      /* 4 */
dequeue(q);      /* fail */
```



## NetRexx

Unlike [[#REXX|Rexx]], NetRexx does not include built&ndash;in support for queues but the language's ability to access the [[Java]] SDK permits use of any number of Java's &quot;Collection&quot; classes.
The following sample implements a stack via the <code>ArrayDeque</code> double&ndash;ended queue.

```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

mqueue = ArrayDeque()

viewQueue(mqueue)

a = "Fred"
mqueue.push('')      /* Puts an empty line onto the queue */
mqueue.push(a 2)     /* Puts "Fred 2" onto the queue */
viewQueue(mqueue)

a = "Toft"
mqueue.add(a 2)      /* Enqueues "Toft 2" */
mqueue.add('')       /* Enqueues an empty line behind the last */
viewQueue(mqueue)

loop q_ = 1 while mqueue.size > 0
  parse mqueue.pop.toString line
  say q_.right(3)':' line
  end q_
viewQueue(mqueue)

return

method viewQueue(mqueue = Deque) private static

   If mqueue.size = 0 then do
    Say 'Queue is empty'
    end
  else do
    Say 'There are' mqueue.size 'elements in the queue'
    end

  return

```


<pre style="height: 20ex; overflow: scroll;">
Queue is empty
There are 2 elements in the queue
There are 4 elements in the queue
  1: Fred 2
  2:
  3: Toft 2
  4:
Queue is empty

```



## Nim


```nim
import queues

# defining push & pop (obviously optional)
proc push*[T](q: var TQueue[T]; item: T) =
    add(q,item)
proc pop*[T](q: var TQueue[T]): T =
    result = dequeue(q)

var fifo: TQueue[int] = initQueue[int]()

fifo.push(26)
fifo.push(99)
fifo.push(2)
echo("Fifo size: ", fifo.len())
echo("Popping: ", fifo.pop())
echo("Popping: ", fifo.pop())
echo("Popping: ", fifo.pop())
#echo("Popping: ", fifo.pop())     # popping an empty stack raises [EAssertionFailed]
```

{{out}}

```txt
Fifo size: 3
Popping: 26
Popping: 99
Popping: 2
```



## OCaml


The standard way to manage fifo in functional programming is to use a pair of list for the fifo queue, one is the input, the other is the output.
When the output is empty just take the input list and reverse it.


```ocaml
module FIFO : sig
  type 'a fifo
  val empty: 'a fifo
  val push: fifo:'a fifo -> item:'a -> 'a fifo
  val pop: fifo:'a fifo -> 'a * 'a fifo
  val is_empty: fifo:'a fifo -> bool
end = struct
  type 'a fifo = 'a list * 'a list
  let empty = [], []
  let push ~fifo:(input,output) ~item = (item::input,output)
  let is_empty ~fifo =
    match fifo with
    | [], [] -> true
    | _ -> false
  let rec pop ~fifo =
    match fifo with
    | input, item :: output -> item, (input,output)
    | [], [] -> failwith "empty fifo"
    | input, [] -> pop ([], List.rev input)
end
```


and a session in the top-level:


```ocaml
# open FIFO;;
# let q = empty ;;
val q : '_a FIFO.fifo = <abstr>
# is_empty q ;;
- : bool = true
# let q = push q 1 ;;
val q : int FIFO.fifo = <abstr>
# is_empty q ;;
- : bool = false

# let q =
    List.fold_left push q [2;3;4] ;;
val q : int FIFO.fifo = <abstr>

# let v, q = pop q ;;
val v : int = 1
val q : int FIFO.fifo = <abstr>
# let v, q = pop q ;;
val v : int = 2
val q : int FIFO.fifo = <abstr>
# let v, q = pop q ;;
val v : int = 3
val q : int FIFO.fifo = <abstr>
# let v, q = pop q ;;
val v : int = 4
val q : int FIFO.fifo = <abstr>
# let v, q = pop q ;;
Exception: Failure "empty fifo".
```


The standard ocaml library also provides a
[http://caml.inria.fr/pub/docs/manual-ocaml/libref/Queue.html FIFO module],
but it is ''imperative'', unlike the implementation above which is ''functional''.


## Oforth


If queue is empty, null is returned.


```Oforth
Object Class new: Queue(mutable l)

Queue method: initialize  ListBuffer new := l ;
Queue method: empty       @l isEmpty ;
Queue method: push        @l add ;
Queue method: pop         @l removeFirst ;
```



## OxygenBasic

This buffer pushes any primitive data (auto converted to strings), and pops strings. The buffer can expand or contract according to usage.

```oxygenbasic

'FIRST IN FIRST OUT

'
### ====

Class Queue
'
### ====


string buf
sys    bg,i,le

method Encodelength(sys ls)
int p at (i+strptr buf)
p=ls
i+=sizeof int
end method

method push(string s)
ls=len s
if i+ls+8>le then
  buf+=nuls 8000+ls*2 : le=len buf 'expand buf
end if
EncodeLength ls
mid buf,i+1,s
i+=ls
'EncodeLength ls
end method

method GetLength() as sys
  if bg>=i then return -1 'buffer empty
  int p at (bg+strptr buf)
  bg+=sizeof int
  return p
end method

method pop(string *s) as sys
sys ls=GetLength
if ls<0 then s="" : return ls 'empty buffer
s=mid buf,bg+1,ls
bg+=ls
if bg>1e6 then
  buf=mid buf,bg+1 : bg=0 : le=len buf : i-=bg 'shrink buf
end if
end method

method clear()
  buf="" : le="" : bg=0 : i=0
end method

end class

'====
'TEST
'====

Queue fifo
string     s
'
fifo.push "HumptyDumpty"
fifo.push "Sat on a wall"
'
sys er
do
  er=fifo.pop s
  if er then print "(buffer empty)" : exit do
  print s
end do

```



## Oz

The semantics of the built-in "Port" datatype is essentially that of a thread-safe queue. We can implement the specified queue type as operations on a pair of a port and a mutable reference to the current read position of the associated stream.

It seems natural to make "Pop" a blocking operation (i.e. it waits for a new value if the queue is currently empty).

The implementation is thread-safe if there is only one reader thread. When multiple reader threads exist, it is possible that a value is popped more than once.


```oz
declare
  fun {NewQueue}
     Stream
     WritePort = {Port.new Stream}
     ReadPos = {NewCell Stream}
  in
     WritePort#ReadPos
  end

  proc {Push WritePort#_ Value}
     {Port.send WritePort Value}
  end

  fun {Empty _#ReadPos}
     %% the queue is empty if the value at the current
     %% read position is not determined
     {Not {IsDet @ReadPos}}
  end

  fun {Pop _#ReadPos}
     %% blocks if empty
     case @ReadPos of X|Xr then
        ReadPos := Xr
        X
     end
  end

  Q = {NewQueue}
in
  {Show {Empty Q}}
  {Push Q 42}
  {Show {Empty Q}}
  {Show {Pop Q}}
  {Show {Empty Q}}
```


There is also a [http://www.mozart-oz.org/home/doc/mozart-stdlib/adt/queue.html queue datatype] in the Mozart standard library.


## Pascal


{{works with|Free Pascal|2.2.0}}
{{works with|GNU Pascal|20060325, based on gcc-3.4.4}}

This program should be Standard Pascal compliant (i.e. it doesn't make use of the advanced/non-standard features of FreePascal or GNU Pascal).


```pascal
program fifo(input, output);

type
 pNode = ^tNode;
 tNode = record
          value: integer;
          next:  pNode;
         end;

 tFifo = record
          first, last: pNode;
         end;

procedure initFifo(var fifo: tFifo);
 begin
  fifo.first := nil;
  fifo.last := nil
 end;

procedure pushFifo(var fifo: tFifo; value: integer);
 var
  node: pNode;
 begin
  new(node);
  node^.value := value;
  node^.next := nil;
  if fifo.first = nil
   then
    fifo.first := node
   else
    fifo.last^.next := node;
  fifo.last := node
 end;

function popFifo(var fifo: tFifo; var value: integer): boolean;
 var
  node: pNode;
 begin
  if fifo.first = nil
   then
    popFifo := false
   else
    begin
     node := fifo.first;
     fifo.first := fifo.first^.next;
     value := node^.value;
     dispose(node);
     popFifo := true
    end
 end;

procedure testFifo;
 var
  fifo: tFifo;
 procedure testpop(expectEmpty: boolean; expectedValue: integer);
  var
   i: integer;
  begin
   if popFifo(fifo, i)
    then
     if expectEmpty
      then
       writeln('Error! Expected empty, got ', i, '.')
      else
       if i = expectedValue
        then
         writeln('Ok, got ', i, '.')
        else
         writeln('Error! Expected ', expectedValue, ', got ', i, '.')
    else
     if expectEmpty
       then
        writeln('Ok, fifo is empty.')
       else
        writeln('Error! Expected ', expectedValue, ', found fifo empty.')
  end;
 begin
  initFifo(fifo);
  pushFifo(fifo, 2);
  pushFifo(fifo, 3);
  pushFifo(fifo, 5);
  testpop(false, 2);
  pushFifo(fifo, 7);
  testpop(false, 3);
  testpop(false, 5);
  pushFifo(fifo, 11);
  testpop(false, 7);
  testpop(false, 11);
  pushFifo(fifo, 13);
  testpop(false, 13);
  testpop(true, 0);
  pushFifo(fifo, 17);
  testpop(false, 17);
  testpop(true, 0)
 end;

begin
 writeln('Testing fifo implementation ...');
 testFifo;
 writeln('Testing finished.')
end.
```




## Perl

Lists are a central part of Perl. To implement a FIFO using OO will to many Perl programmers seem a bit awkward.


```perl
use Carp;
sub mypush (\@@) {my($list,@things)=@_; push @$list, @things}
sub mypop  (\@)  {my($list)=@_; @$list or croak "Empty"; shift @$list }
sub empty  (@)   {not @_}
```


Example:


```perl
my @fifo=qw(1 2 3 a b c);

mypush @fifo, 44, 55, 66;
mypop @fifo for 1 .. 6+3;
mypop @fifo; #empty now
```



## Perl 6

{{Works with|rakudo|2018.03}}
We could build a new container class to do FIFO pretty easily, but Arrays already do everything needed by a FIFO queue, so it is easier to just compose a Role on the existing Array class.

```perl6
role FIFO {
    method enqueue ( *@values ) { # Add values to queue, returns the number of values added.
        self.push: @values;
        return @values.elems;
    }
    method dequeue ( ) {         # Remove and return the first value from the queue.
                                 # Return Nil if queue is empty.
        return self.elems ?? self.shift !! Nil;
    }
    method is-empty ( ) {        # Check to see if queue is empty. Returns Boolean value.
        return self.elems == 0;
    }
}

# Example usage:

my @queue does FIFO;

say @queue.is-empty;                         # -> Bool::True
for <A B C> -> $i { say @queue.enqueue: $i } # 1 \n 1 \n 1
say @queue.enqueue: Any;                     # -> 1
say @queue.enqueue: 7, 8;                    # -> 2
say @queue.is-empty;                         # -> Bool::False
say @queue.dequeue;                          # -> A
say @queue.elems;                            # -> 4
say @queue.dequeue;                          # -> B
say @queue.is-empty;                         # -> Bool::False
say @queue.enqueue('OHAI!');                 # -> 1
say @queue.dequeue until @queue.is-empty;    # -> C \n Any() \n [7 8] \n OHAI!
say @queue.is-empty;                         # -> Bool::True
say @queue.dequeue;                          # ->
```



## Phix


```Phix
sequence queue = {}

procedure push(object what)
    queue = append(queue,what)
end procedure

function pop()
    object what = queue[1]
    queue = queue[2..$]
    return what
end function

function empty()
    return length(queue)=0
end function
```



## PHP


```PHP
class Fifo {
  private $data = array();
  public function push($element){
    array_push($this->data, $element);
  }
  public function pop(){
    if ($this->isEmpty()){
      throw new Exception('Attempt to pop from an empty queue');
    }
    return array_shift($this->data);
  }

  //Alias functions
  public function enqueue($element) { $this->push($element); }
  public function dequeue() { return $this->pop(); }

  //Note: PHP prevents a method name of 'empty'
  public function isEmpty(){
    return empty($this->data);
  }
}
```


Example:


```PHP
$foo = new Fifo();
$foo->push('One');
$foo->enqueue('Two');
$foo->push('Three');

echo $foo->pop();     //Prints 'One'
echo $foo->dequeue(); //Prints 'Two'
echo $foo->pop();     //Prints 'Three'
echo $foo->pop();     //Throws an exception

```



## PicoLisp

The built-in function 'fifo' maintains a queue in a circular list, with direct
access to the first and the last cell

```PicoLisp
(off Queue)                # Clear Queue
(fifo 'Queue 1)            # Store number '1'
(fifo 'Queue 'abc)         # an internal symbol 'abc'
(fifo 'Queue "abc")        # a transient symbol "abc"
(fifo 'Queue '(a b c))     # and a list (a b c)
Queue                      # Show the queue
```

{{out}}

```txt
->((a b c) 1 abc "abc" .)
```



## PL/I


```pli

/* To push a node onto the end of the queue. */
push: procedure (tail);
   declare tail handle (node), t handle (node);
   t = new(:node:);
   get (t => value);
   if tail ^= bind(:null, node:) then
      tail => link = t;
      /* If the queue was non-empty, points the tail of the queue */
      /* to the new node. */
   tail = t; /* Point "tail" at the end of the queue. */
   tail => link = bind(:node, null:);
end push;

/* To pop a node from the head of the queue. */
pop: procedure (head, val);
   declare head handle (node), val fixed binary;
   if head = bind(:node, null:) then signal error;
   val = head => value;
   head = head => pointer; /* pops the top node. */
   if head = bind(:node, null:) then tail = head;
      /* (If the queue is now empty, make tail null also.) */
end pop;

/* Queue status: the EMPTY function, returns true for empty queue. */
empty: procedure (h) returns (bit(1));
   declare h handle (Node);
   return (h = bind(:Node, null:) );
end empty;

```



## PostScript

{{libheader|initlib}}

```postscript

% our queue is just [] and empty? is already defined.
/push {exch tadd}.
/pop {uncons exch}.

```



## PowerShell

{{works with|PowerShell|2}}<br/>
PowerShell can natively use the .Net Queue class.

```PowerShell

$Q = New-Object System.Collections.Queue

$Q.Enqueue( 1 )
$Q.Enqueue( 2 )
$Q.Enqueue( 3 )

$Q.Dequeue()
$Q.Dequeue()

$Q.Count -eq 0
$Q.Dequeue()
$Q.Count -eq 0

try
{ $Q.Dequeue() }
catch [System.InvalidOperationException]
{ If ( $_.Exception.Message -eq 'Queue empty.' ) { 'Caught error' } }
```

{{out}}

```txt
1
2
False
3
True
Caught error
```



## Prolog

Works with SWI-Prolog.
One can push any data in queue.

```Prolog
empty(U-V) :-
    unify_with_occurs_check(U, V).

push(Queue, Value, NewQueue) :-
    append_dl(Queue, [Value|X]-X, NewQueue).

% when queue is empty pop fails.
pop([X|V]-U, X, V-U) :-
    \+empty([X|V]-U).

append_dl(X-Y, Y-Z, X-Z).

```



## PureBasic

For FIFO function PureBasic normally uses linked lists.
Usage as described above could look like;

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
  Else
    Debug "Pop(), out of range. Error at line "+str(#PB_Compiler_Line)
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
While Not Empty()
  Debug Pop()
Wend
;----   Now an extra Pop(), e.g. one to many ----
Debug Pop()
```


{{out}}

```txt

 3
 1
 4
 Pop(), out of range. Error at line 17
 0

```



## Python

A python list can be used as a simple FIFO by simply using only it's ''.append()'' and ''.pop()'' methods and only using ''.pop(0)'' to consistently pull the head off the list.  (The default ''.pop()'' pulls off the tail, and using that would treat the list as a [[stack]].

To encapsulate this behavior into a class and provide the task's specific API we can simply use:


```python
   class FIFO(object):
       def __init__(self, *args):
           self.contents = list(args)
       def __call__(self):
           return self.pop()
       def __len__(self):
           return len(self.contents)
       def pop(self):
           return self.contents.pop(0)
       def push(self, item):
           self.contents.append(item)
       def extend(self,*itemlist):
           self.contents += itemlist
       def empty(self):
           return bool(self.contents)
       def __iter__(self):
           return self
       def next(self):
           if self.empty():
               raise StopIteration
           return self.pop()

if __name__ == "__main__":
    # Sample usage:
    f = FIFO()
    f.push(3)
    f.push(2)
    f.push(1)
    while not f.empty():
        print f.pop(),
    # >>> 3 2 1
    # Another simple example gives the same results:
    f = FIFO(3,2,1)
    while not f.empty():
        print f(),
    # Another using the default "truth" value of the object
    # (implicitly calls on the length() of the object after
    # checking for a __nonzero__ method
    f = FIFO(3,2,1)
    while f:
        print f(),
    # Yet another, using more Pythonic iteration:
    f = FIFO(3,2,1)
    for i in f:
        print i,
```


This example does add to a couple of features which are easy in Python and allow this FIFO class to be used in ways that Python programmers might find more natural.  Our ''__init__'' accepts and optional list of initial values, we add ''__len__'' and ''extend'' methods which simply wrap the corresponding list methods; we define a ''__call__'' method to show how one can make objects "callable" as functions, and we define ''__iter__'' and ''next()'' methods to facilitate using these FIFO objects with Python's prevalent iteration syntax (the ''for'' loop).  The ''empty'' method could be implemented as simply an alias for ''__len__'' --- but we've chosen to have it more strictly conform to the task specification.  Implementing the ''__len__'' method allows code using this object to test of emptiness using normal Python idioms for "truth" (any non-empty container is considered to be "true" and any empty container evaluates as "false").

These additional methods could be omitted and some could have been dispatched to the "contents" object by defining a ''__getattr__'' method.  (All methods that are note defined could be relayed to the contained list).  This would allow us to skip our definitions of extend, __iter__, and __len__, and would allow contents of these objects to be access by indexes and slices as well as supporting all other list methods.

That sort of wrapper looks like:


```python
class FIFO:  ## NOT a new-style class, must not derive from "object"
   def __init__(self,*args):
       self.contents = list(args)
   def __call__(self):
       return self.pop()
   def empty(self):
       return bool(self.contents)
   def pop(self):
       return self.contents.pop(0)
   def __getattr__(self, attr):
       return getattr(self.contents,attr)
   def next(self):
       if not self:
           raise StopIteration
       return self.pop()
```


As noted in the contents this must NOT be a new-style class, it must NOT but sub-classed from ''object'' nor any of its descendents.  (A new-style implementation using __getattribute__ would be possible)

{{works with|Python|2.4+}}

Python 2.4 and later includes a [http://docs.python.org/lib/deque-objects.html deque class], supporting thread-safe, memory efficient appends and pops from either side of the deque with approximately the same O(1) performance in either direction. For other options see [http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/68436 Python Cookbook].


```python
from collections import deque
fifo = deque()
fifo. appendleft(value) # push
value = fifo.pop()
not fifo # empty
fifo.pop() # raises IndexError when empty
```



## R


### Simple functional implementation

This simple implementation provides three functions that act on a variable in the global environment (user workspace) named ''l''.  the push and pop functions display the new status of ''l'', but return NULL silently.

```R
empty <- function() length(l) == 0
push <- function(x)
{
   l <<- c(l, list(x))
   print(l)
   invisible()
}
pop <- function()
{
   if(empty()) stop("can't pop from an empty list")
   l[[1]] <<- NULL
   print(l)
   invisible()
}
l <- list()
empty()
# [1] TRUE
push(3)
# [[1]]
# [1] 3
push("abc")
# [[1]]
# [1] 3
# [[2]]
# [1] "abc"
push(matrix(1:6, nrow=2))
# [[1]]
# [1] 3
# [[2]]
# [1] "abc"
# [[3]]
#      [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    4    6
empty()
# [1] FALSE
pop()
# [[1]]
# [1] 3
# [[2]]
# [1] "abc"
pop()
# [[1]]
# [1] 3
pop()
# list()
pop()
# Error in pop() : can't pop from an empty list
```


The problem with this is that the functions aren't related to the FIFO object (the list ''l''), and they require the list to exist in the global environment.  (This second problem is possible to get round by passing ''l'' into the function and then returning it, but that is extra work.)


### Message passing



```r
# The usual Scheme way : build a function that takes commands as parameters (it's like message passing oriented programming)
queue <- function() {
    v <- list()
    f <- function(cmd, val=NULL) {
        if(cmd == "push") {
            v <<- c(v, val)
            invisible()
        } else if(cmd == "pop") {
            if(length(v) == 0) {
                stop("empty queue")
            } else {
                x <- v[[1]]
                v[[1]] <<- NULL
                x
            }
        } else if(cmd == "length") {
            length(v)
        } else if(cmd == "empty") {
            length(v) == 0
        } else {
            stop("unknown command")
        }
    }
    f
}

# Create two queues
a <- queue()
b <- queue()
a("push", 1)
a("push", 2)
b("push", 3)
a("push", 4)
b("push", 5)

a("pop")
# [1] 1
b("pop")
# [1] 3
```



### Object oriented implementation

{{libheader|proto}}
A better solution is to use the object oriented facility in the proto package.  (R does have it's own native object oriented code, though the proto package is often nicer to use.)


```R
library(proto)

fifo <- proto(expr = {
   l <- list()
   empty <- function(.) length(.$l) == 0
   push <- function(., x)
   {
      .$l <- c(.$l, list(x))
      print(.$l)
      invisible()
   }
   pop <- function(.)
   {
      if(.$empty()) stop("can't pop from an empty list")
      .$l[[1]] <- NULL
      print(.$l)
      invisible()
   }
})

#The following code provides output that is the same as the previous example.
fifo$empty()
fifo$push(3)
fifo$push("abc")
fifo$push(matrix(1:6, nrow=2))
fifo$empty()
fifo$pop()
fifo$pop()
fifo$pop()
fifo$pop()
```



## Racket


Racket comes with a queue implementation in the <tt>data/queue</tt> library.
Here's an explicit implementation:


```Racket

#lang racket

(define (make-queue) (mcons #f #f))
(define (push! q x)
  (define new (mcons x #f))
  (if (mcar q) (set-mcdr! (mcdr q) new) (set-mcar! q new))
  (set-mcdr! q new))
(define (pop! q)
  (define old (mcar q))
  (cond [(eq? old (mcdr q)) (set-mcar! q #f) (set-mcdr! q #f)]
        [else (set-mcar! q (mcdr old))])
  (mcar old))
(define (empty? q)
  (not (mcar q)))

(define Q (make-queue))
(empty? Q) ; -> #t
(push! Q 'x)
(empty? Q) ; -> #f
(for ([x 3]) (push! Q x))
(pop! Q)   ; -> 'x
(list (pop! Q) (pop! Q) (pop! Q)) ; -> '(0 1 2)

```


And this is an implementation of a functional queue.

```racket

#lang racket
;; Invariants:
;; The elements in the queue are (append front (reverse back)).
;; Front is always non-empty (except for the empty queue).
(struct queue (front back))

(define empty (queue '() '()))

(define (push x q)
  (if (null? (queue-front q))
      (queue (reverse (cons x (queue-back q))) '())
      (queue (queue-front q) (cons x (queue-back q)))))

(define (empty? q)
  (null? (queue-front q)))

(define (pop q)
  (cond [(empty? q) (error 'pop "the queue is empty")]
        [(not (null? (queue-front q)))
         (if (null? (rest (queue-front q)))
             (queue (reverse (queue-back q)) '())
             (queue (rest (queue-front q)) (queue-back q)))]
        [else (queue (reverse (queue-back q)) '())]))

(define (first q)
  (cond [(empty? q) (error 'first "the queue is empty")]
        [(car (queue-front q))]))

;; Example:
(first (pop (pop (for/fold ([q empty]) ([x '(1 2 3 4)])
                   (push x q)))))
;; => 3

```



## REBOL


```REBOL
rebol [
    Title: "FIFO"
    URL: http://rosettacode.org/wiki/FIFO
]

; Define fifo class:

fifo: make object! [
    queue: copy []
    push:  func [x][append queue x]
    pop:   func [/local x][   ; Make 'x' local so it won't pollute global namespace.
        if empty [return none]
        x: first queue  remove queue  x]
    empty: does [empty? queue]
]

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


{{out}}

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

Support for '''LIFO''' &amp; '''FIFO''' queues is built into the [[REXX|Rexx]] language.

The following are supported in REXX:
*   '''PUSH'''      (lifo)
*   '''QUEUE'''          (fifo)
*   '''PULL'''    --- which is a short version of:
*   '''PARSE UPPER PULL'''
*   '''PARSE LOWER PULL'''   --- supported by some newer REXXes
*   '''PARSE PULL'''
*   '''QUEUED()'''   [a BIF which returns the number of queued entries.]

<!-- The    LINES()   BIF has nothing to do with the number of entries queued,
     it has to do with the number of lines not yet read in an open (for read) file.
  --                                                             Gerard Schildberger. -->

```rexx
/*REXX program to demonstrate FIFO queue usage by some simple operations*/
call viewQueue
a="Fred"
push                                   /*puts a  "null" on top of queue.*/
push a 2                               /*puts  "Fred 2" on top of queue.*/
call viewQueue

queue "Toft 2"                         /*put  "Toft 2"  on queue bottom.*/
queue                                  /*put a "null"   on queue bottom.*/
call viewQueue
                  do n=1  while queued()\==0
                  parse pull xxx
                  say "queue entry" n': ' xxx
                  end   /*n*/
call viewQueue
exit                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────viewQueue subroutine────────────────*/
viewQueue:  if queued()==0 then say 'Queue is empty'
                           else say 'There are' queued() 'elements in the queue'
return
```

'''output'''

```txt

Queue is empty
There are 2 elements in the queue
There are 4 elements in the queue
  queue entry 1:  Fred 2
  queue entry 2:
  queue entry 3:  Toft 2
  queue entry 4:
Queue is empty

```



## Ring


```ring

# Project : Queue/Definition

load "stdlib.ring"
oQueue = new Queue
for n = 5 to 7
     see "Push: " + n + nl
     oQueue.add(n)
next
see "Pop: " + oQueue.remove() + nl
see "Push: 8" + nl
oQueue.add(8)
see "Pop: " + oQueue.remove() + nl
see "Pop: " + oQueue.remove() + nl
see "Pop: " + oQueue.remove() + nl
if len(oQueue) != 0
   oQueue.print()
else
   see "Error: queue is empty" + nl
ok

```

Output:

```txt

Push: 5
Push: 6
Push: 7
Pop: 5
Push: 8
Pop: 6
Pop: 7
Pop: 8
Error: queue is empty

```



## Ruby

The core class ''Array'' already implements all queue operations, so this class ''FIFO'' delegates everything to methods of ''Array''.


```ruby
require 'forwardable'

# A FIFO queue contains elements in first-in, first-out order.
# FIFO#push adds new elements to the end of the queue;
# FIFO#pop or FIFO#shift removes elements from the front.
class FIFO
  extend Forwardable

  # Creates a FIFO containing _objects_.
  def self.[](*objects)
    new.push(*objects)
  end

  # Creates an empty FIFO.
  def initialize; @ary = []; end

  # Appends _objects_ to the end of this FIFO. Returns self.
  def push(*objects)
    @ary.push(*objects)
    self
  end
  alias << push
  alias enqueue push

  ##
  # :method: pop
  # :call-seq:
  #   pop -> obj or nil
  #   pop(n) -> ary
  #
  # Removes an element from the front of this FIFO, and returns it.
  # Returns nil if the FIFO is empty.
  #
  # If passing a number _n_, removes the first _n_ elements, and returns
  # an Array of them. If this FIFO contains fewer than _n_ elements,
  # returns them all. If this FIFO is empty, returns an empty Array.
  def_delegator :@ary, :shift, :pop
  alias shift pop
  alias dequeue shift

  ##
  # :method: empty?
  # Returns true if this FIFO contains no elements.
  def_delegator :@ary, :empty?

  ##
  # :method: size
  # Returns the number of elements in this FIFO.
  def_delegator :@ary, :size
  alias length size

  # Converts this FIFO to a String.
  def to_s
    "FIFO#{@ary.inspect}"
  end
  alias inspect to_s
end
```



```ruby
f = FIFO.new
f.empty?                           # => true
f.pop                              # => nil
f.pop(2)                           # => []
f.push(14)                         # => FIFO[14]
f << "foo" << [1,2,3]              # => FIFO[14, "foo", [1, 2, 3]]
f.enqueue("bar", Hash.new, "baz")
# => FIFO[14, "foo", [1, 2, 3], "bar", {}, "baz"]
f.size                             # => 6
f.pop(3)                           # => [14, "foo", [1, 2, 3]]
f.dequeue                          # => "bar"
f.empty?                           # => false
g = FIFO[:a, :b, :c]
g.pop(2)                           # => [:a, :b]
g.pop(2)                           # => [:c]
g.pop(2)                           # => []
```




## Rust


### Using the standard library

The standard library has a double-ended queue implementation (<code>VecDeque<T></code>) which will work here.

```rust
use std::collections::VecDeque;
fn main() {
    let mut stack = VecDeque::new();
    stack.push_back("Element1");
    stack.push_back("Element2");
    stack.push_back("Element3");

    assert_eq!(Some(&"Element1"), stack.front());
    assert_eq!(Some("Element1"), stack.pop_front());
    assert_eq!(Some("Element2"), stack.pop_front());
    assert_eq!(Some("Element3"), stack.pop_front());
    assert_eq!(None, stack.pop_front());
}
```


### A simple implementation

This shows the implementation of a singly-linked queue with <code>dequeue</code> and <code>enqueue</code>. There are two <code>peek</code> implementations, one returns an immutable reference, the other returns a mutable one. This implementation also shows iteration over the Queue by value (consumes queue), immutable reference, and mutable reference.

```rust
use std::ptr;

pub struct Queue<T> {
    head: Link<T>,
    tail: *mut Item<T>, // Raw, C-like pointer. Cannot be guaranteed safe
}

type Link<T> = Option<Box<Item<T>>>;

struct Item<T> {
    elem: T,
    next: Link<T>,
}

pub struct IntoIter<T>(Queue<T>);

pub struct Iter<'a, T:'a> {
    next: Option<&'a Item<T>>,
}

pub struct IterMut<'a, T: 'a> {
    next: Option<&'a mut Item<T>>,
}


impl<T> Queue<T> {
    pub fn new() -> Self {
        Queue { head: None, tail: ptr::null_mut() }
    }

    pub fn enqueue(&mut self, elem: T) {
        let mut new_tail = Box::new(Item {
            elem: elem,
            next: None,
        });

        let raw_tail: *mut _ = &mut *new_tail;

        if !self.tail.is_null() {
            unsafe {
                (*self.tail).next = Some(new_tail);
            }
        } else {
            self.head = Some(new_tail);
        }

        self.tail = raw_tail;
    }

    pub fn dequeue(&mut self) -> Option<T> {
        self.head.take().map(|head| {
            let head = *head;
            self.head = head.next;

            if self.head.is_none() {
                self.tail = ptr::null_mut();
            }

            head.elem
        })
    }

    pub fn peek(&self) -> Option<&T> {
        self.head.as_ref().map(|item| {
            &item.elem
        })
    }

    pub fn peek_mut(&mut self) -> Option<&mut T> {
        self.head.as_mut().map(|item| {
            &mut item.elem
        })
    }

    pub fn into_iter(self) -> IntoIter<T> {
        IntoIter(self)
    }

    pub fn iter(&self) -> Iter<T> {
        Iter { next: self.head.as_ref().map(|item| &**item) }
    }

    pub fn iter_mut(&mut self) -> IterMut<T> {
        IterMut { next: self.head.as_mut().map(|item| &mut **item) }
    }
}

impl<T> Drop for Queue<T> {
    fn drop(&mut self) {
        let mut cur_link = self.head.take();
        while let Some(mut boxed_item) = cur_link {
            cur_link = boxed_item.next.take();
        }
    }
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.dequeue()
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.map(|item| {
            self.next = item.next.as_ref().map(|item| &**item);
            &item.elem
        })
    }
}

impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.take().map(|item| {
            self.next = item.next.as_mut().map(|item| &mut **item);
            &mut item.elem
        })
    }
}
```



## Scala


```scala
class Queue[T] {
  private[this] class Node[T](val value:T) {
    var next:Option[Node[T]]=None
    def append(n:Node[T])=next=Some(n)
  }
  private[this] var head:Option[Node[T]]=None
  private[this] var tail:Option[Node[T]]=None

  def isEmpty=head.isEmpty

  def enqueue(item:T)={
    val n=new Node(item)
    if(isEmpty) head=Some(n) else tail.get.append(n)
    tail=Some(n)
  }

  def dequeue:T=head match {
    case Some(item) => head=item.next; item.value
    case None => throw new java.util.NoSuchElementException()
  }

  def front:T=head match {
    case Some(item) => item.value
    case None => throw new java.util.NoSuchElementException()
  }

  def iterator: Iterator[T]=new Iterator[T]{
    private[this] var it=head;
    override def hasNext=it.isDefined
    override def next:T={val n=it.get; it=n.next; n.value}
  }

  override def toString()=iterator.mkString("Queue(", ", ", ")")
}
```

Usage:

```scala
val q=new Queue[Int]()
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

{{out}}

```txt
isEmpty = true
dequeue(empty) failed.
queue   = Queue(1, 2, 3)
front   = 1
dequeue = 1
dequeue = 2
isEmpty = false
```



## Scheme


Using a vector for mutable data. Can be optimized by using an extra slot
in the vector to hold tail pointer to avoid the append call.


```scheme
(define (make-queue)
  (make-vector 1 '()))

(define (push a queue)
  (vector-set! queue 0 (append (vector-ref queue 0) (list a))))

(define (empty? queue)
  (null? (vector-ref queue 0)))

(define (pop queue)
  (if (empty? queue)
      (error "can not pop an empty queue")
      (let ((ret (car (vector-ref queue 0))))
        (vector-set! queue 0 (cdr (vector-ref queue 0)))
        ret)))

```



###  Message passing


```scheme
(define (make-queue)
(let ((q (cons '() '())))
(lambda (cmd . arg)
(case cmd
    ((empty?) (null? (car q)))
    ((put) (let ((a (cons (car arg) '())))
        (if (null? (car q))
            (begin (set-car! q a) (set-cdr! q a))
            (begin (set-cdr! (cdr q) a) (set-cdr! q a)))))
    ((get) (if (null? (car q)) 'empty
        (let ((x (caar q)))
            (set-car! q (cdar q))
            (if (null? (car q)) (set-cdr! q '()))
            x)))
))))

(define q (make-queue))
(q 'put 1)
(q 'put 6)
(q 'get)
; 1
(q 'get)
; 6
(q 'get)
; empty
```



## Sidef

Implemented as a class:

```ruby
class FIFO(*array) {
    method pop {
        array.is_empty && die "underflow";
        array.shift;
    }
    method push(*items) {
        array += items;
        self;
    }
    method empty {
        array.len == 0;
    }
}
```



## Slate

Toy code based on Slate's Queue standard library (which is optimized for FIFO access):

```slate
collections define: #Queue &parents: {ExtensibleArray}.

q@(Queue traits) isEmpty [resend].
q@(Queue traits) push: obj [q addLast: obj].
q@(Queue traits) pop [q removeFirst].
q@(Queue traits) pushAll: c [q addAllLast: c].
q@(Queue traits) pop: n [q removeFirst: n].
```



## Smalltalk

{{works with|GNU Smalltalk}}

An OrderedCollection can be easily used as a FIFO queue.


```smalltalk
OrderedCollection extend [
   push: obj [ ^(self add: obj) ]
   pop [
       (self isEmpty) ifTrue: [
          SystemExceptions.NotFound signalOn: self
                reason: 'queue empty'
       ] ifFalse: [
          ^(self removeFirst)
       ]
   ]
]

|f|
f := OrderedCollection new.
f push: 'example'; push: 'another'; push: 'last'.
f pop printNl.
f pop printNl.
f pop printNl.
f isEmpty printNl.
f pop. "queue empty error"
```



## Standard ML

Here is the signature for a basic queue:

```Standard ML

signature QUEUE =
sig
  type 'a queue

  val empty_queue: 'a queue

  exception Empty

  val enq: 'a queue -> 'a -> 'a queue
  val deq: 'a queue -> ('a * 'a queue)
  val empty: 'a queue -> bool
end;

```

A very basic implementation of this signature backed by a list is as follows:

```Standard ML

structure Queue:> QUEUE =
struct
  type 'a queue = 'a list

  val empty_queue = nil

  exception Empty

  fun enq q x = q @ [x]

  fun deq nil = raise Empty
  |   deq (x::q) = (x, q)

  fun empty nil = true
  |   empty _ = false
end;

```




## Stata

See [[Singly-linked list/Element definition#Stata]].

## Tcl

Here's a simple implementation using a list:

```tcl
proc push {stackvar value} {
    upvar 1 $stackvar stack
    lappend stack $value
}
proc pop {stackvar} {
    upvar 1 $stackvar stack
    set value [lindex $stack 0]
    set stack [lrange $stack 1 end]
    return $value
}
proc size {stackvar} {
    upvar 1 $stackvar stack
    llength $stack
}
proc empty {stackvar} {
    upvar 1 $stackvar stack
    expr {[size stack] == 0}
}
proc peek {stackvar} {
    upvar 1 $stackvar stack
    lindex $stack 0
}

set Q [list]
empty Q ;# ==> 1 (true)
push Q foo
empty Q ;# ==> 0 (false)
push Q bar
peek Q ;# ==> foo
pop Q ;# ==> foo
peek Q ;# ==> bar
```


{{tcllib|struct::queue}}

```tcl
package require struct::queue
struct::queue Q
Q size ;# ==> 0
Q put a b c d e
Q size ;# ==> 5
Q peek ;# ==> a
Q get ;# ==> a
Q peek ;# ==> b
Q pop 4 ;# ==> b c d e
Q size ;# ==> 0
```



## UnixPipes

Uses moreutils

```bash
init() {echo > fifo}
push() {echo $1 >> fifo }
pop() {head -1 fifo ; (cat fifo | tail -n +2)|sponge fifo}
empty() {cat fifo | wc -l}
```

Usage:

```bash
push me; push you; push us; push them
|pop;pop;pop;pop
me
you
us
them
```



## UNIX Shell

{{works with|ksh93}}

```bash
queue_push() {
    typeset -n q=$1
    shift
    q+=("$@")
}

queue_pop() {
    if queue_empty $1; then
        print -u2 "queue $1 is empty"
        return 1
    fi
    typeset -n q=$1
    print "${q[0]}"     # emit the value of the popped element
    q=( "${q[@]:1}" )   # and remove the first element from the queue
}

queue_empty() {
    typeset -n q=$1
    (( ${#q[@]} == 0 ))
}

queue_peek() {
    typeset -n q=$1
    print "${q[0]}"
}
```


Usage:

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


{{out}}

```txt
foo is empty
foo is not empty
peek: bar
peek: baz
peek: element with spaces
peek:
queue foo is empty
```



## V

V doesn't have mutable data. Below is an function interface for a fifo.


```v
[fifo_create []].
[fifo_push swap cons].
[fifo_pop [[*rest a] : [*rest] a] view].
[fifo_empty? dup empty?].
```


Using it

```v
|fifo_create 3 fifo_push 4 fifo_push 5 fifo_push ??
=[5 4 3]
|fifo_empty? puts
=false
|fifo_pop put fifo_pop put fifo_pop put
=3 4 5
|fifo_empty? puts
 =true
```



## VBA


```vb
Public queue As New Collection

Private Sub push(what As Variant)
    queue.Add what
End Sub

Private Function pop() As Variant
    If queue.Count > 0 Then
        what = queue(1)
        queue.Remove 1
    Else
        what = CVErr(461)
    End If
    pop = what
End Function

Private Function empty_()
    empty_ = queue.Count = 0
End Function
```


## VBScript

Using an ArrayList.

```vb
' Queue Definition - VBScript
Option Explicit
Dim queue, i, x
Set queue = CreateObject("System.Collections.ArrayList")
If Not empty_(queue) Then Wscript.Echo queue.Count
push queue, "Banana"
push queue, "Apple"
push queue, "Pear"
push queue, "Strawberry"
Wscript.Echo "Count=" & queue.Count
Wscript.Echo pull(queue) & " - Count=" & queue.Count '
Wscript.Echo "Head=" & queue.Item(0)
Wscript.Echo "Tail=" & queue.Item(queue.Count-1)
Wscript.Echo queue.IndexOf("Pear", 0)
For i=1 To queue.Count
	Wscript.Echo join(queue.ToArray(), ", ")
	x = pull(queue)
Next 'i

Sub push(q, what)
    q.Add what
End Sub 'push

Function pull(q)
	Dim what
    If q.Count > 0 Then
        what = q(0)
        q.RemoveAt 0
    Else
        what = ""
    End If
    pull = what
End Function 'pull

Function empty_(q)
    empty_ = q.Count = 0
End Function 'empty_

```

{{out}}

```txt

Count=4
Banana - Count=3
Head=Apple
Tail=Strawberry
1
Apple, Pear, Strawberry
Pear, Strawberry
Strawberry

```



## Wart

Wart defines queues as lists with a pointer to the last element saved for constant-time enqueuing:

```python
def (queue seq)
  (tag queue (list seq lastcons.seq len.seq))

def (enq x q)
  do1 x
    let (l last len) rep.q
      rep.q.2 <- (len + 1)
      if no.l
        rep.q.1 <- (rep.q.0 <- list.x)
        rep.q.1 <- (cdr.last <- list.x)

def (deq q)
  let (l last len) rep.q
    ret ans car.l
      unless zero?.len
        rep.q.2 <- (len - 1)
      rep.q.0 <- cdr.l

def (len q) :case (isa queue q)
  rep.q.2
```


<code>empty?</code> relies on <code>len</code> by default, so there's no need to separately override it.


## XLISP

A queue is similar to a stack, except that values are pushed onto and popped from different "ends" of it (whereas in a stack it is the same end for both operations). This implementation is based on the XLISP implementation of a stack, therefore, but with a <tt>push</tt> method that appends a new value to the end rather than sticking it onto the front. Attempting to pop from an empty queue will return the empty list, equivalent to Boolean "false".

```lisp
(define-class queue
    (instance-variables vals))

(define-method (queue 'initialize)
    (setq vals '())
    self)

(define-method (queue 'push x)
    (setq vals (nconc vals (cons x nil))))

(define-method (queue 'pop)
    (define val (car vals))
    (setq vals (cdr vals))
    val)

(define-method (queue 'emptyp)
    (null vals))
```

A sample REPL session:

```lisp
[1] (define my-queue (queue 'new))

MY-QUEUE
[2] (my-queue 'push 1)

(1)
[3] (my-queue 'push 2)

(1 2)
[4] (my-queue 'emptyp)

()
[5] (my-queue 'pop)

1
[6] (my-queue 'pop)

2
[7] (my-queue 'emptyp)

#T
[8] (my-queue 'pop)

()
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



## zkl


```zkl
class Queue{
   var [const] q=List();
   fcn push  { q.append(vm.pasteArgs()) }
   fcn pop   { q.pop(0) }
   fcn empty { q.len()==0 }
}
```


```zkl
q:=Queue();
q.push(1,2,3);
q.pop();     //-->1
q.empty();   //-->False
q.pop();q.pop();q.pop() //-->IndexError thrown
```

