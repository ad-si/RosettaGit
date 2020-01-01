+++
title = "Singly-linked list/Element removal"
description = ""
date = 2019-09-22T20:52:16Z
aliases = []
[extra]
id = 21176
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

;Task:

Define a method to remove an element from a singly-linked list and demonstrate its use.

You may wish to use the link element defined in [[Singly-Linked List (element)]] for the purposes of this task.


## ALGOL 68

Using the STRINGLIST from the Singly-linked list Element traversal task.
{{Trans|ALGOL_W}}

```algol68
# removes the specified element from the list, modifying list if necessary #
PRIO REMOVE = 1;
OP   REMOVE = ( REF STRINGLIST list, REF STRINGLIST element )REF STRINGLIST:
     IF element ISNT REF STRINGLIST(NIL) THEN
         # have an element to remove #
         IF REF STRINGLIST(list) IS REF STRINGLIST(element) THEN
             # remove the head #
             list := next OF list
         ELSE
             # not removing the head element #
             REF STRINGLIST list pos := list;
             WHILE IF   REF STRINGLIST(list pos) IS REF STRINGLIST(NIL) THEN FALSE
                   ELSE REF STRINGLIST(next OF list pos) ISNT REF STRINGLIST(element)
                   FI
             DO
                 list pos := next OF list pos
             OD;
             IF REF STRINGLIST(list pos) ISNT REF STRINGLIST(NIL) THEN
                 # found the element #
                 next OF list pos := next OF next OF list pos
             FI
         FI;
         list
     ELSE
         list
     FI # REMOVE # ;

MODE STRINGLIST = STRUCT(STRING value, REF STRINGLIST next);
STRINGLIST list := ("Big",
    LOC STRINGLIST := ("fjords",
      LOC STRINGLIST := ("vex",
        LOC STRINGLIST := ("quick",
          LOC STRINGLIST := ("waltz",
            LOC STRINGLIST := ("nymph",NIL))))));

# remove the third and then the first element from the STRINGLIST #

( list REMOVE next OF next OF list ) REMOVE list;


```



## ALGOL W

Uses the ListI record from the Singly Linked List task.

```algolw

    % deletes the specified element from the list                             %
    %         if the element to remove is null or not in the list,            %
    %            nothing happens                                              %
    procedure Remove( reference(ListI) value result list
                    ; reference(ListI) value        element
                    ) ;
        if element not = null then begin
            % have an element to remove %
            if list = element then % remove the head % list := next(list)
            else begin
                % not removing the head element %
                Reference(ListI) listPos;
                listPos := list;
                while listPos not = null and next(listPos) not = element do listPos := next(listPos);
                if listPos not = null then % found the element % next(listPos) := next(next(listPos))
            end
        end Remove ;

    % declare a ListI list %
    reference(ListI) head;

    % ... add some elements to the list here ... %

    % remove the third element from a list %
    Remove( head, next(next(head)) );

    % remove the first element from a list %
    Remove( head, head );


```



## C

This implementation takes up integers from the command line and then asks which element has to be removed. List is printed before and after removal, usage printed on incorrect invocation.

```C

#include<stdlib.h>
#include<stdio.h>

typedef struct elem{
	int data;
	struct elem* next;
}cell;

typedef cell* list;

list addToList(list a,int num){

	list iter, temp;
	int i=0;

	if(a==NULL){
		a = (list)malloc(sizeof(cell));
		a->data = num;
		a->next = NULL;
	}
	else{
		iter = a;

		while(iter->next!=NULL){
			iter = iter->next;
		}

		temp = (list)malloc(sizeof(cell));
		temp->data = num;
		temp->next = NULL;

		iter->next = temp;
	}
	return a;
}

list deleteFromList(list a,int pos){

	int i=1;
	list temp,iter;

	if(a!=NULL){
		iter = a;

		if(pos==1){
			a = a->next;
			iter->next = NULL;
			free(iter);
		}

		else{
			while(i++!=pos-1)
				iter = iter->next;
			temp = iter->next;
			iter->next = temp->next;
			temp->next = NULL;
			free(temp);
		}
	}
	return a;
}

void printList(list a){
	list temp = a;

	printf("List contains following elements : \n");

	while(temp!=NULL){
		printf("%d ",temp->data);
		temp = temp->next;
	}
}

int main(int argC,char* argV[])
{
	list a = NULL;
	int i;

	if(argC == 1)
		printf("Usage : %s <list of integers to be inserted into the list>",argV[0]);
	else{
		for(i=2;i<=argC;i++)
			a = addToList(a,atoi(argV[i-1]));

		printList(a);

		do{
			printf("\nEnter position of element to be removed (1-%d) : ",argC-1);

			scanf("%d",&i);

			if(i>0 && i<=argC-1){
				a = deleteFromList(a,i);
				printList(a);
			}
		}while(i>argC-1||i<=0);
	}
	return 0;
}

```

Invocation, interaction and output :

```txt

C:\rosettaCode>linkedList.exe 1 2 3
List contains following elements :
1 2 3
Enter position of element to be removed (1-3) : 2
List contains following elements :
1 3

```



## C#


===Tasteful & unsafe===
{{trans|Taste|version=273489}}
Semantically identical translation of Torvalds' tasteful C version using C# unsafe pointers:


```c#
using System;
using System.Runtime.InteropServices;

static unsafe class Program
{
    ref struct LinkedListNode
    {
        public int Value;
        public LinkedListNode* Next;
        public override string ToString() => this.Value + (this.Next == null ? string.Empty : " -> " + this.Next->ToString());
    }

    static void Remove(LinkedListNode** head, LinkedListNode* entry)
    {
        // The "indirect" pointer points to the
        // *address* of the thing we'll update

        LinkedListNode** indirect = head;

        // Walk the list, looking for the thing that
        // points to the entry we want to remove

        while (*indirect != entry)
            indirect = &(*indirect)->Next;

        // .. and just remove it
        *indirect = entry->Next;
    }

    static void Main()
    {
        // Allocate like real C!
        var head = (LinkedListNode*)Marshal.AllocHGlobal(sizeof(LinkedListNode));
        head->Value = 1;
        head->Next = (LinkedListNode*)Marshal.AllocHGlobal(sizeof(LinkedListNode));
        head->Next->Value = 2;
        head->Next->Next = null;

        LinkedListNode copy = *head;

        Console.WriteLine("original:                    " + head->ToString());

        Remove(&head, head);
        Console.WriteLine("after removing head:         " + head->ToString());

        head = &copy;
        Console.WriteLine("restored from copy:          " + head->ToString());

        Remove(&head, head->Next);
        Console.WriteLine("after removing second node:  " + head->ToString());
    }
}
```


{{out}}

```txt
original:                    1 -> 2
after removing head:         2
restored from copy:          1 -> 2
after removing second node:  1
```



## Fortran

This sort of thing has long been done in Fortran via the standard trick of fiddling with arrays, and using array indices as the equivalent of the memory addresses of nodes. The task makes no mention of there being any content associated with the links of the linked-list; this would be supplied via auxiliary arrays or disc file records, ''etc''. With F90 and later, one can define compound data aggregates, so something like LL.NEXT would hold the link to the next element and LL.STUFF would hold the cargo, with LL being an array of such a compound entity rather than separate simple arrays such as LLNEXT and LLSTUFF.

F90 offers further opportunities, whereby instead of LL being an array of some size defined before it is used, it would instead consist of single items each containing the cargo for one item plus a link to the address of another item, with items allocated as the need arises. This however involves a lot of additional syntax and lengthy words such as ALLOCATE, all distracting from the exhibition of a solution, which is simple...

For convenience, rather than have the "head" pointer to the first or head element of the linked list be a separate variable, it is found as element zero of the LIST array that holds the links. Because this element is accessible in the same way as the other links in the array representing the linked-list, no special code is needed when it is the head entry that is to be removed and thus it is the pointer to it that must be changed. However, defining arrays starting at index zero is a feature of F90, and having subroutines recognise that their array parameter starts at index zero requires the MODULE protocol. Previously, arrays started with index one, and the code would just have to recognise this with the appropriate offsets, thus, the first element available for an item would be at index two, not one, and so forth. On the other hand, the zero element just needs its link, and any associated cargo would represented wasted storage. If that cargo were to be held in a disc file there would be no such waste, as record numbers start with one, not zero. But, if the linked-list is to be stored entirely in a disc file, the first record has to be reserved to hold the link to the head record and the first available storage record is number two, just as with an array starting at one, not zero. Indeed, a more comprehensive solution would probably reserve the first record as a header, containing a finger to the start of the linked-list, another finger to the start of the "available" (i.e. deleted and thus reusable) linked-list of records, and a record counter to identify the last record in the file so that if the "available" list is empty, the file can be extended by one record to hold a new entry.

Having a value of zero signify that there is no follower is the obvious choice for ending a linked-list. When addresses are being tossed about, this might be dressed up via words such as NULL rather than a literal zero just in case a "null address" does not manifest as a zero value.
```Fortran
      MODULE SIMPLELINKEDLIST	!Play with an array. Other arrays might hold content.
       CONTAINS			!Demonstration only!
        SUBROUTINE LLREMOVE(LINK,X)	!Remove entry X from the links in LINK.
         INTEGER LINK(0:)	!The links.
         INTEGER X		!The "address" or index, of the unwanted one.
         INTEGER IT		!A stepper.
          IT = 0		!This list element fingers the start of the list..
          DO WHILE(LINK(IT).GT.0)	!While a live follower,
            IF (LINK(IT).EQ.X) THEN		!Is that follower unwanted?
              LINK(IT) = LINK(LINK(IT))		!Yes! Step over it!
              RETURN				!Done. Escape!
            END IF			!But if the follower survives,
            IT = LINK(IT)		!Advance to finger it.
          END DO		!And try afresh.
        END SUBROUTINE LLREMOVE	!No checks for infinite loops!

        SUBROUTINE LLFOLLOW(LINK)	!Show the sequence.
         INTEGER LINK(0:)	!The links.
          IT = 0			!Start by fingering the head.
          WRITE (6,1) "Head",IT,LINK(IT)	!Show it.
    1     FORMAT (A6,I3," -->",I3)		!This will do.
    2     IT = LINK(IT)		!Advance.
          IF (IT.LE.0) RETURN		!Done yet?
          WRITE (6,1) "at",IT,LINK(IT)	!Nope. Show.
          GO TO 2			!And try afresh.
        END SUBROUTINE LLFOLLOW	!No checks for infinite loops!
      END MODULE SIMPLELINKEDLIST	!A bit trickier with bidirectional links.

      PROGRAM POKE
      USE SIMPLELINKEDLIST	!Just so.
      INTEGER LINK(0:5)		!This will suffice.
      DATA LINK/3, 2,4,1,5,0/	!Set the head and its followers.

      WRITE (6,*) "A linked-list, no cargo."
      CALL LLFOLLOW(LINK)

      WRITE (6,*) "The element at one suffers disfavour."
      CALL LLREMOVE(LINK,1)
      CALL LLFOLLOW(LINK)

      WRITE (6,*) "Off with the head!"
      CALL LLREMOVE(LINK,LINK(0))	!LINK(0) fingers the head element.
      CALL LLFOLLOW(LINK)

      WRITE (6,*) "And off with the tail."
      CALL LLREMOVE(LINK,5)		!The tail element is not tracked.
      CALL LLFOLLOW(LINK)		!But, I know where it was, in this example.

      END
```

Output:

```txt

 A linked-list, no cargo.
  Head  0 -->  3
    at  3 -->  1
    at  1 -->  2
    at  2 -->  4
    at  4 -->  5
    at  5 -->  0
 The element at one suffers disfavour.
  Head  0 -->  3
    at  3 -->  2
    at  2 -->  4
    at  4 -->  5
    at  5 -->  0
 Off with the head!
  Head  0 -->  2
    at  2 -->  4
    at  4 -->  5
    at  5 -->  0
 And off with the tail.
  Head  0 -->  2
    at  2 -->  4
    at  4 -->  0

```


Although this will survive not finding a match for X and the linked-list being empty (because the link to the head is null, being zero, and LINK(0) ''is'' zero) there is no attempt to prevent infinite loops (such as when an item is linked to itself), nor checks for valid bounds to a link. Further, the unlinked element is simply abandoned. ''This is a memory leak!'' It should be transferred to some sort of "available" linked-list for potential re-use later. If instead the elements were separately-allocated pieces of storage, such storage should be diligently de-allocated for potential re-use later.

Although in this example the linked-list is held in an array, and array elements can be accessed at random, the key difficulty is that to unlink an element, the element fingering that has to be linked to the follower of the to-be-unlinked element, and to find the parent of a randomly-selected item will require a search through the links. By following the links, this information is in hand when the unwanted item is found. On the other hand, if the caller could be persuaded to identify the node to be removed by fingering the node that points to it, no chase is needed and the code becomes <code>LINK(X) = LINK(LINK(X))</code>, hardly worth the trouble of devising a subroutine - unless it added the unlinked node to an "available" list, provided checking and debugging output, ''etc''.

In messing with linked-lists, one must give close attention to just how an element is identified. Is element X (for removal) the X'th element in sequence along the linked list (first, second, third, etc.), or, is it the element at at a specified memory address or index position X in the LIST array (as here), or, is it the element whose cargo matches X?

The code involves repeated mention of <code>LINK(IT)</code> and for those who do not have total faith in the brilliance of code generated by a compiler, one could try
```Fortran
          IT = 0		!This list element fingers the start of the list..
    1     NEXT = LINK(IT)	!This is the node of interest.
          IF (NEXT.GT.0) THEN	!Is it a live node?
            IF (NEXT.EQ.X) THEN		!Yes. Is it the unwanted one?
              LINK(IT) = LINK(NEXT)		!Yes! Step over it!
              RETURN				!Done. Escape!
            END IF			!But if the follower survives,
            IT = NEXT			!Advance to finger it.
            GO TO 1			!And try afresh.
          END IF		!So much for that node.

```

The introduction of a mnemonic "NEXT" might help the interpretation of the code, but one must be careful about phase: NEXT is the "nextness" for IT which fingers node NEXT which is the candidate for matching against X, not IT. Alternatively, use "FROM" for IT and "IT" for NEXT, being careful to keep it straight.

And ... there is a blatant GO TO (aside from the equivalent concealed via RETURN) but using a WHILE-loop would require a repetition of NEXT = LINK(IT). If Fortran were to enable assignment within an expression (as in Algol) then
```Fortran
      IT = 0		!This list element fingers the start of the list..
      DO WHILE((NEXT = LINK(IT)).GT.0)	!Finger the follower of IT.
        IF (NEXT.EQ.X) THEN		!Is it the unwanted one?
          LINK(IT) = LINK(NEXT)			!Yes! Step over it!
          RETURN				!Done. Escape!
        END IF				!But if not,
        IT = NEXT			!Advance to the follower.
      END DO				!Ends when node IT's follower is null.

```

No label, no nasty literal GO TO - even though an "END DO" hides one.


## Go

This reuses code from other singly-linked list tasks.

```go
package main

import "fmt"

type Ele struct {
    Data interface{}
    Next *Ele
}

var head *Ele

func (e *Ele) Append(data interface{}) *Ele {
    if e == nil {
        return e
    }
    if e.Next == nil {
        e.Next = &Ele{data, nil}
    } else {
        e.Next = &Ele{data, e.Next}
    }
    return e.Next
}

// Removes first element with given data value from the list.
// If this is 'head' element, resets 'head' to next element.
// Does nothing if data value is not present.
func (e *Ele) Remove(data interface{}) {
    if e == nil {
        return
    }
    if e.Data == data {
        if e == head {
            head = e.Next
        }
        e.Next = nil
        return
    }
    prev := e
    for iter := e.Next; iter != nil; iter = iter.Next {
        if iter.Data == data {
            prev.Next = iter.Next
            iter.Next = nil
            return
        }
        prev = iter
    }
}

func (e *Ele) String() string {
    return fmt.Sprintf("%v", e.Data)
}

func (e *Ele) Traverse() {
    if e == nil {
        fmt.Println(e)
        return
    }
    for iter := e; iter != nil; iter = iter.Next {
        fmt.Println(iter)
    }
}

func main() {
    head = &Ele{"tacos", nil}
    next := head.Append("burritos")
    next = next.Append("fajitas")
    next = next.Append("enchilatas")

    fmt.Println("Before any removals:")
    head.Traverse()

    head.Remove("fajitas")
    fmt.Println("\nAfter removing fajitas:")
    head.Traverse()

    head.Remove("tacos")
    fmt.Println("\nAfter removing tacos:")
    head.Traverse()

    head.Remove("enchilatas")
    fmt.Println("\nAfter removing enchilatas:")
    head.Traverse()

    head.Remove("burritos")
    fmt.Println("\nAfter removing burritos:")
    head.Traverse()
}
```


{{out}}

```txt

Before any removals:
tacos
burritos
fajitas
enchilatas

After removing fajitas:
tacos
burritos
enchilatas

After removing tacos:
burritos
enchilatas

After removing enchilatas:
burritos

After removing burritos:
<nil>

```



## Julia

{{works with|Julia|0.6}}
See the <tt>LinkedList</tt> defined at [[Singly-linked_list/Element_definition#Julia]].

```julia
function Base.deleteat!(ll::LinkedList, index::Integer)
    if isempty(ll) throw(BoundsError()) end
    if index == 1
        ll.head = ll.head.next
    else
        nd = ll.head
        index -= 1
        while index > 1 && !isa(nd.next, EmptyNode)
            nd = nd.next
            index -= 1
        end
        if nd.next isa EmptyNode throw(BoundsError()) end
        nx = nd.next
        nd.next = nd.next.next
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

fun <T: Number> remove(first: Node<T>, removal: Node<T>) {
    if (first === removal)
        first.next = null
    else {
        var node: Node<T>? = first
        while (node != null) {
            if (node.next === removal) {
                val next = removal.next
                removal.next = null
                node.next = next
                return
            }
            node = node.next
        }
    }
}

fun main(args: Array<String>) {
    val b = Node(3)
    val a = Node(1, b)
    println("Before insertion  : $a")
    val c = Node(2)
    insertAfter(a, c)
    println("After  insertion  : $a")
    remove(a, c) // remove node we've just inserted
    println("After 1st removal : $a")
    remove(a, b) // remove last node
    println("After 2nd removal : $a")
}
```


{{out}}

```txt

Before insertion  : 1 -> 3
After  insertion  : 1 -> 2 -> 3
After 1st removal : 1 -> 3
After 2nd removal : 1

```



## Perl 6


Extending <tt>class Cell</tt> from [[Singly-linked_list/Element_definition#Perl_6]]:


```perl6
    method delete ($value --> Cell) {
        my $prev = Nil;
        my $cell = self;
        my $new-head = self;

        while $cell {
            my $next = $cell.next;
            if $cell.value == $value {
                $prev.next = $next if $prev;
                $cell.next = Nil;
                $new-head = $next if $cell === $new-head;
            }
            else {
                $prev = $cell;
            }
            $cell = $next;
        }

        return $new-head;
    }
```


Usage:


```perl6
my $list = cons 10, (cons 20, (cons 10, (cons 30, Nil)));

$list = $list.delete(10);
```



## Phix

Note that singly-linked lists are a bit alien to Phix, since the core sequence type is so versatile.

While [[Singly-linked_list/Traversal#Phix]] inserts at the end, I thought I'd mix things up and try an in-order insert here
and then mix them up again and remove items in a random order. Obviously remove_item() forms the meat
of the task requirement; insert_inorder(), show(), and test() aren't really and can be skipped.

```Phix
enum NEXT,DATA
sequence sll = {}
integer sll_head = 0,
        sll_free = 0

procedure insert_inorder(object data)
    if length(sll)=0 or sll_head=0 then
        sll = {{0,data}}
        sll_head = 1
        sll_free = 0
    else
        integer this = sll_head, next, flag = 0
        object node = 0
        while 1 do
            next = sll[this][NEXT]
            if data<sll[this][DATA] then
                flag = 1
                node = sll[this]
            elsif next=0 then
                flag = 2
                node = {0,data}
            end if
            if flag then
                if sll_free then
                    next = sll_free
                    sll_free = sll[next][NEXT]
                    sll[next] = node
                else
                    sll = append(sll,node)
                    next = length(sll)
                end if
                sll[this][NEXT] = next
                if flag=1 then
                    sll[this][DATA] = data
                end if
                exit
            end if
            this = next
        end while
    end if
end procedure

procedure remove_item(object data)
    integer idx = sll_head, prev
    while idx do
        if sll[idx][DATA]=data then
            if idx=sll_head then
                sll_head = sll[idx][NEXT]
            else
                sll[prev][NEXT] = sll[idx][NEXT]
            end if
            sll[idx][NEXT] = sll_free
            sll[idx][DATA] = 0
            sll_free = idx
            exit
        end if
        prev = idx
        idx = sll[idx][NEXT]
    end while
end procedure

procedure show()
integer idx = sll_head
sequence list = {}
    while idx do
        list = append(list,sll[idx][DATA])
        idx = sll[idx][NEXT]
    end while
    ?list
end procedure

enum ADD,REMOVE
procedure test(integer mode, sequence list)
    ?{{"add","remove"}[mode],list}
    for i=1 to length(list) do
        if mode=ADD then
            insert_inorder(list[i])
        else
            remove_item(list[i])
        end if
        show()
    end for
end procedure

sequence list = {"1","2","3","4"}
test(ADD,shuffle(list))
test(REMOVE,shuffle(list))
```

{{out}}

```txt

{"add",{"4","2","3","1"}}
{"4"}
{"2","4"}
{"2","3","4"}
{"1","2","3","4"}
{"remove",{"1","3","2","4"}}
{"2","3","4"}
{"2","4"}
{"4"}
{}

```



## Racket


This is written entirely in terms of car and cdr (the linked list/pair primitives in Racket and Scheme). Usually, you'd have reverse and append available to you... but, again, it's interesting to see how they are implemented (by me, at least)


```racket
#lang racket/base

(define (rev l (acc null))
    (if (null? l)
        acc
        (rev (cdr l) (cons (car l) acc))))

(define (++ l m)
    (if (null? l)
        m
        (let recur ((l-rev (rev l)) (acc m))
          (if (null? l-rev)
              acc
              (recur (cdr l-rev) (cons (car l-rev) acc))))))

(define (remove-at l i (acc null))
  (cond
    [(null? l) (rev acc)]
    [(positive? i) (remove-at (cdr l) (sub1 i) (cons (car l) acc))]
    [else (++ (rev acc) (cdr l))]))

(displayln (remove-at '(1 2 3) 0))
(displayln (remove-at '(1 2 3) 1))
(displayln (remove-at '(1 2 3) 2))
(displayln (remove-at '(1 2 3) 3))
```


{{out}}

```txt
(2 3)
(1 3)
(1 2)
(1 2 3)
```



## Visual Basic .NET


The contract requirement for these functions is:

- that the entry to be removed is not Nothing
- the entry is present in the list.
- the list Head is not Nothing

The contract ensures:

- The entry has been removed.


```vbnet


    Module Module1

      Public Class ListEntry
        Public value As String
        Public [next] As ListEntry
      End Class

      Public Head As ListEntry

      ''' <summary>
      ''' Straight translation of Torvalds' tasteless version.
      ''' </summary>
      ''' <param name="entry"></param>
      Sub RemoveListEntryTasteless(entry As ListEntry)

        Dim prev As ListEntry = Nothing
        Dim walk = Head

        ' Walk the list
        While walk IsNot entry
          prev = walk
          walk = walk.next
        End While

        ' Remove the entry by updating the head or the previous entry.
        If prev Is Nothing Then
          Head = entry.next
        Else
          prev.next = entry.next
        End If
      End Sub

      ''' <summary>
      ''' Straight translation of Torvalds' tasteful version.
      ''' </summary>
      ''' <param name="entry"></param>
      Sub RemoveListEntryTastefull(entry As ListEntry)

        Dim indirect = New ListEntry
        indirect.next = Head

        ' Walk the list looking for the thing that points at the thing that we
        ' want to remove.
        While indirect.next IsNot entry
          indirect = indirect.next
        End While

        ' ... and just remove it.
        indirect.next = entry.next

      End Sub

End Module


```

[[Category:VBA examples needing attention]]
