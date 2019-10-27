+++
title = "Deepcopy"
description = ""
date = 2018-11-30T23:52:37Z
aliases = []
[extra]
id = 10109
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Demonstrate how to copy data structures containing complex heterogeneous and cyclic semantics. 

This is often referred to as [[wp:Deep_copy#Deep_copy|deep copying]], and is normally required where structures are mutable and to ensure that independent copies can be manipulated without side-effects.

If this facility is not built into the language, it is permissible to use functions from a common library, or a coded procedure.


The task should show:

* Relevant semantics of structures, such as their [[wp:Homogeneity and heterogeneity|homogeneous or heterogeneous]] properties, or containment of (self- or mutual-reference) cycles.

* Any limitations of the method.

* That the structure and its copy are different.

* Suitable links to external documentation for common libraries.





## Aime


```aime
list L1, L2;

# Lists are heterogeneous:
l_append(L1, 3);
l_append(L1, "deep");

# and may contain self references.
# A self references in the last position:
l_link(L1, -1, L1);

# List may also contain mutual references.
# Create a new list in the last position:
l_n_list(L1, -1);
# Add a reference to the top level list to the nested list:
l_link(l_q_list(L1, -1), -1, L1);

# There are no limitations to the deep copy method:
l_copy(L2, L1);

# Modify the string in the original list,
# via the self reference in the 3rd position
l_r_text(l_q_list(L1, 2), 1, "copy");

# Show the string in the two lists:
o_text(l_query(L2, 1));
o_text(l_query(L1, 1));
o_byte('\n');

# And again, via the included self references:
o_text(l_query(l_query(L2, 2), 1));
o_text(l_query(l_query(L1, 2), 1));
o_byte('\n');
```

{{out}}

```txt
deepcopy
deepcopy
```



## AutoHotkey

{{works with|AutoHotkey L}}
http://www.autohotkey.com/board/topic/85201-array-deep-copy-treeview-viewer-and-more/

```autohotkey
DeepCopy(Array, Objs=0)
{
    If !Objs
        Objs := Object()
    Obj := Array.Clone() ; produces a shallow copy in that any sub-objects are not cloned
    Objs[&Array] := Obj ; Save this new array - & returns the address of Array in memory
    For Key, Val in Obj
        If (IsObject(Val)) ; If it is a subarray
            Obj[Key] := Objs[&Val] ; If we already know of a reference to this array
            ? Objs[&Val] ; Then point it to the new array (to prevent infinite recursion on self-references
            : DeepCopy(Val,Objs) ; Otherwise, clone this sub-array
    Return Obj
}
```



## Babel


Any pure Babel object, however complex, can be deep-copied with the cp operator. By contrast, the dup operator makes a shallow copy of any object (duplicates the reference on top-of-stack). In the examples below, the sd utility is the "stack dump" and shows the Babel stack.

Here is an example of shallow-copy - modifying one object modifies both because they are really just references to one underlying object:


```babel>babel
 [1 2 3] dup dup 0 7 0 1 move sd !
---TOS---
[val 0x7 0x2 0x3 ]
[val 0x7 0x2 0x3 ]
---BOS---
```


Deep-copy is proved by the ability to separately modify two objects:


```babel>babel
 clear [1 2 3] dup cp dup 0 7 0 1 move sd !
---TOS---
[val 0x7 0x2 0x3 ]
[val 0x1 0x2 0x3 ]
---BOS---
```


You can deep-copy any pure-Babel object with cp. Here is a list-of-lists, we copy it using cp:


```babel>babel
 ((1 2) (3 4) (5 6)) cp
babel> {lsnum !} each
( 1 2 )
( 3 4 )
( 5 6 )
```


Here is a list-of-maps, we copy it using cp:


```babel>babel
 ([map "foo" 3 "bar" 17] [map "foo" 4 "bar" 18] [map "foo" 5 "bar" 19] [map "foo" 0 "bar" 20]) cp 
babel> 2 ith "bar" lumap ! itod say !
19
```


Here is Babel code, we copy it using cp:


```babel>babel
 { { 1 randlf 100 rem itod << " " << } 20 times } cp
babel> eval
86 51 50 43 82 76 13 78 33 45 11 35 84 25 80 36 33 81 43 24
```



## C


### Structures without pointers

Structures without pointers can be copied like ''standard'' C variables such as int, float, char etc. The level of nesting is irrelevant. As the following implementation shows, a simple assignment is enough :

```C

#include<stdio.h>

typedef struct{
	int a;
}layer1;

typedef struct{
	layer1 l1;
	float b,c;
}layer2;

typedef struct{
	layer2 l2;
	layer1 l1;
	int d,e;
}layer3;

void showCake(layer3 cake){
	printf("\ncake.d = %d",cake.d);
	printf("\ncake.e = %d",cake.e);
	printf("\ncake.l1.a = %d",cake.l1.a);
	printf("\ncake.l2.b = %f",cake.l2.b);
	printf("\ncake.l2.l1.a = %d",cake.l2.l1.a);
}

int main()
{
	layer3 cake1,cake2;
	
	cake1.d = 1;
	cake1.e = 2;
	cake1.l1.a = 3;
	cake1.l2.b = 4;
	cake1.l2.l1.a = 5;
	
	printf("Cake 1 is : ");
	showCake(cake1);
	
	cake2 = cake1;
	
	cake2.l2.b += cake2.l2.l1.a;
	
	printf("\nCake 2 is : ");
	showCake(cake2);
	
	return 0;
}

```

Output:

```txt

Cake 1 is :
cake.d = 1
cake.e = 2
cake.l1.a = 3
cake.l2.b = 4.000000
cake.l2.l1.a = 5
Cake 2 is :
cake.d = 1
cake.e = 2
cake.l1.a = 3
cake.l2.b = 9.000000
cake.l2.l1.a = 5

```


### Structures with pointers

Structures with pointers which are usually used to represent data structures such as Linked lists, Stacks, Trees, Graphs etc. have to be copied element by element. A simple assignment as in the above example will not be a copy at all. It will be two pointers pointing towards the same memory location.

```C

#include<stdlib.h>
#include<stdio.h>

typedef struct elem{
	int data;
	struct elem* next;
}cell;

typedef cell* list;

void addToList(list *a,int num){
	list temp, holder;
	
	if(*a==NULL){
		*a = (list)malloc(sizeof(cell));
		(*a)->data = num;
		(*a)->next = NULL;
	}
	else{
		temp = *a;
		
		while(temp->next!=NULL)
			temp = temp->next;
		
		holder = (list)malloc(sizeof(cell));
		holder->data = num;
		holder->next = NULL;
		
		temp->next = holder;
	}
}

list copyList(list a){
	list b, tempA, tempB, temp;
	
	if(a!=NULL){
		b = (list)malloc(sizeof(cell));
		b->data = a->data;
		b->next = NULL;
		
		tempA = a->next;
		tempB = b;
		
		while(tempA!=NULL){
			temp = (list)malloc(sizeof(cell));
			temp->data = tempA->data;
			temp->next = NULL;
		
			tempB->next = temp;
			tempB = temp;
		
			tempA = tempA->next;
		}
	}
	
	return b;
}

void printList(list a){
	list temp = a;
	
	while(temp!=NULL){
		printf("%d,",temp->data);
		temp = temp->next;
	}
	printf("\b");
}

int main()
{
	list a,b;
	int i;
	
	for(i=1;i<=5;i++)
		addToList(&a,i);
	
	printf("List a is : ");
	
	printList(a);
	
	b = copyList(a);
	
	free(a);
	
	printf("\nList a destroyed, List b is : ");
	
	printList(b);
	
	return 0;
}

```

Output:

```txt

List a is : 1,2,3,4,5,
List a destroyed, List b is : 1,2,3,4,5,

```



## C sharp


```csharp
using System;

namespace prog
{
	class MainClass
	{
		class MyClass : ICloneable
		{
			public MyClass() { f = new int[3]{2,3,5}; c = '1'; }
			
			public object Clone()
			{				
				MyClass cpy = (MyClass) this.MemberwiseClone();
				cpy.f = (int[]) this.f.Clone();			
				return cpy;
			}
			
			public char c;
			public int[] f;
		}
		
		public static void Main( string[] args )
		{
			MyClass c1 = new MyClass();
			MyClass c2 = (MyClass) c1.Clone();
		}
	}
}
```



## Common Lisp


Numerous approaches can be demonstrated here. Here is a quick and dirty way to copy circular structure.

Common Lisp has a printed notation which preserves circularity and shared substructure. This way of printing is in effect when the dynamic variable <code>*print-circle*</code> is set true.

We can copy a structure by printing it this way to a string and then reading the resulting string back to data.

The circular notation consists of the two elements <code>#num= obj</code> and <code>#<num>#</code>.
For instance <code>#42=(a b)</code> denotes the list <code>(a b)</code> and furthermore, it associates it with the number 42. Then, later in the same form, #42# denotes an additional occurence of the same <code>(a b)</code> object. So for instance, a cons cell whose <code>car</code> is 1, and whose <code>cdr</code> points back to that cons cell is written <code>#1=(1 . #1#)</code>. 


```lisp
$ clisp -q
[1]> (setf *print-circle* t)
T
[2]> (let ((a (cons 1 nil))) (setf (cdr a) a)) ;; create circular list
#1=(1 . #1#)
[3]> (read-from-string "#1=(1 . #1#)") ;; read it from a string
#1=(1 . #1#) ;; a similar circular list is returned
```


=={{header|Déjà Vu}}==

```dejavu
local :(copy-action) {}

(copy-action)!list obj cache:
	local :new []
	set-to cache obj new
	for i range 0 -- len obj:
		push-to new (deepcopy) @obj! i cache
	return new

(copy-action)!dict obj cache:
	local :new {}
	set-to cache obj new
	for key in keys obj:
		set-to new (deepcopy) @key cache (deepcopy) @obj! @key cache
	return new

labda obj cache:
	set-to cache @obj dup copy @obj
set-default (copy-action)

(deepcopy) obj cache:
	if has cache obj:
		return @cache! @obj
	(copy-action)! type @obj @obj cache

deepcopy obj:
	(deepcopy) obj {}

#example usage:
#a reasonably complicated object:
set :A { :foo [ "bar" ] [] [ & 1 2 & 3 4 ] }
set :B deepcopy A

!. A
!. B

push-to get-from B :foo "HODOR"

!. A
!. B

#it works with cycles:
set :C push-through dup []
set :D deepcopy C

!. C
!. D

push-to C 7

!. C
!. D
```

{{out}}

```txt
{ :foo [ "bar" ] [ ] [ & 1 2 & 3 4 ] }
{ [ ] [ & 1 2 & 3 4 ] :foo [ "bar" ] }
{ :foo [ "bar" ] [ ] [ & 1 2 & 3 4 ] }
{ [ ] [ & 1 2 & 3 4 ] :foo [ "bar" "HODOR" ] }
[ [ [ [ [...] ] ] ] ]
[ [ [ [ [...] ] ] ] ]
[ [ [ [ [...] 7 ] 7 ] 7 ] 7 ]
[ [ [ [ [...] ] ] ] ]
```



## E


In E, serialization is generalized to transforming object graphs from one representation to another. Deep copying, therefore, consists of transforming a live object graph into a live object graph, by connecting <code>deSubgraphKit</code>'s output to its input. No intermediate serialized form is needed.


```e>def deSubgraphKit := <elib:serial.deSubgraphKit

def deepcopy(x) {
  return deSubgraphKit.recognize(x, deSubgraphKit.makeBuilder())
}
```


As befits a serialization system, this deep copy may operate on any serializable structure, whether standard or user-defined, and the structure may contain cycles.


```e
? def x := ["a" => 1, "b" => [x].diverge()]
# value: ["a" => 1, "b" => [<***CYCLE***>].diverge()]

? def y := deepcopy(x)
# value: ["a" => 1, "b" => [<***CYCLE***>].diverge()]

? y["b"].push(2)

? y
# value: ["a" => 1, "b" => [<***CYCLE***>, 2].diverge()]

? x
# value: ["a" => 1, "b" => [<***CYCLE***>].diverge()]

? y["b"][0] == y
# value: true

? y["b"][0] == x
# value: false

? x["b"][0] == x
# value: true
```


(<code>.diverge()</code> produces mutable data structures, and <code>&lt;***CYCLE***></code> is what is printed when printing some object meets itself again.)

See also: [[Polymorphic copy#E]]


## Erlang

Until somebody explains how to create cyclic data structures in Erlang I can show heterogeneous data.
{{out}}

```txt

16> D.
{dict,4,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[["qwe",49,50,51],[p|<0.32.0>]],
        [[a|b]],
        [],[],[],[],[],[],[],[],[],
        [[1|2]],
        [],[],[],[]}}}
        [],[],[],[],[],[],[],[],[],
        [[1|2]],
        [],[],[],[]}}}
17> D2 = D.
18> D2.
{dict,4,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[["qwe",49,50,51],[p|<0.32.0>]],
        [[a|b]],
        [],[],[],[],[],[],[],[],[],
        [[1|2]],
        [],[],[],[]}}}

```



## Go

Go does not have direct support for deep copy.  To make deep copies of specific data structures, it is most efficient to write your own copy function and just copy what needs to be copied.

'''Heterogeneous:'''

```go
package main

import "fmt"

// a complex data structure
type cds struct {
    i int            // no special handling needed for deep copy
    s string         // no special handling
    b []byte         // copied easily with append function
    m map[int]bool   // deep copy requires looping
}

// a method
func (c cds) deepcopy() *cds {
    // copy what you can in one line
    r := &cds{c.i, c.s, append([]byte{}, c.b...), make(map[int]bool)}
    // populate map with a loop
    for k, v := range c.m {
        r.m[k] = v
    }
    return r
}

// demo
func main() {
    // create and populate a structure 
    c1 := &cds{1, "one", []byte("unit"), map[int]bool{1: true}}
    fmt.Println(c1)      // show it
    c2 := c1.deepcopy()  // copy it
    fmt.Println(c2)      // show copy
    c1.i = 0             // change original
    c1.s = "nil"
    copy(c1.b, "zero")
    c1.m[1] = false
    fmt.Println(c1)      // show changes
    fmt.Println(c2)      // show copy unaffected
}
```

Output:

```txt

&{1 one [117 110 105 116] map[1:true]}
&{1 one [117 110 105 116] map[1:true]}
&{0 nil [122 101 114 111] map[1:false]}
&{1 one [117 110 105 116] map[1:true]}

```

'''Cyclic:'''

DIY here requires you to code a traversal algorithm.

```go
package main

import "fmt"

// a type that allows cyclic structures
type node []*node

// recursively print the contents of a node
func (n *node) list() {
    if n == nil {
        fmt.Println(n)
        return
    }
    listed := map[*node]bool{nil: true}
    var r func(*node)
    r = func(n *node) {
        listed[n] = true
        fmt.Printf("%p -> %v\n", n, *n)
        for _, m := range *n {
            if !listed[m] {
                r(m)
            }
        }
    }
    r(n)
}

// construct a deep copy of a node
func (n *node) ccopy() *node {
    if n == nil {
        return n
    }
    cc := map[*node]*node{nil: nil}
    var r func(*node) *node
    r = func(n *node) *node {
        c := make(node, len(*n))
        cc[n] = &c
        for i, m := range *n {
            d, ok := cc[m]
            if !ok {
                d = r(m)
            }
            c[i] = d
        }
        return &c
    }
    return r(n)
}

func main() {
    a := node{nil}
    c := &node{&node{&a}}
    a[0] = c
    c.list()
    cc := c.ccopy()
    fmt.Println("copy:")
    cc.list()
    fmt.Println("original:")
    c.list()
}
```

{{out}}

```txt

0xc42000a4a0 -> [0xc42000a4c0]
0xc42000a4c0 -> [0xc42000a480]
0xc42000a480 -> [0xc42000a4a0]
copy:
0xc42000a580 -> [0xc42000a5a0]
0xc42000a5a0 -> [0xc42000a5c0]
0xc42000a5c0 -> [0xc42000a580]
original:
0xc42000a4a0 -> [0xc42000a4c0]
0xc42000a4c0 -> [0xc42000a480]
0xc42000a480 -> [0xc42000a4a0]

```

'''General heterogeneous:'''

If you still want a generalized deep copy, one can be cobbled with an os.Pipe and the gob package, which does type safe serialization.  The deepcopy function shown below works on arbitrary data with a few limitations.  It handles data types with recursive or cyclic definitions, but does not handle cycles in the data itself.  For example, it handles a linked list, but not a ring data structure.  Another limitation is that struct fields must be exported.  (That is, fields must start with an upper case letter.  This makes the field visible outside the package.)

```go
package main

import (
    "encoding/gob"
    "fmt"
    "os"
)

// capability requested by task
func deepcopy(dst, src interface{}) error {
    r, w, err := os.Pipe()
    if err != nil {
        return err
    }
    enc := gob.NewEncoder(w)
    err = enc.Encode(src)
    if err != nil {
        return err
    }
    dec := gob.NewDecoder(r)
    return dec.Decode(dst)
}

// define linked list type, an example of a recursive type
type link struct {
    Value string
    Next  *link
}

// method satisfies stringer interface for fmt.Println
func (l *link) String() string {
    if l == nil {
        return "nil"
    }
    s := "(" + l.Value
    for l = l.Next; l != nil; l = l.Next {
        s += " " + l.Value
    }
    return s + ")"
}

func main() {
    // create a linked list with two elements
    l1 := &link{"a", &link{Value: "b"}}
    // print original
    fmt.Println(l1)
    // declare a variable to hold deep copy
    var l2 *link
    // copy
    if err := deepcopy(&l2, l1); err != nil {
        fmt.Println(err)
        return
    }
    // print copy
    fmt.Println(l2)
    // now change contents of original list
    l1.Value, l1.Next.Value = "c", "d"
    // show that it is changed
    fmt.Println(l1)
    // show that copy is unaffected
    fmt.Println(l2)
}
```

Output:

```txt

(a b)
(a b)
(c d)
(a b)

```


=={{header|Icon}} and {{header|Unicon}}==

Unicon and Icon support heterogeneous structures with loops.  The Unicon book has an example of a simple algorithm for producing a deep copy of a structured value (set, list, table, or record); however, that code did not handle graph structures that are not trees. The code for deepcopy below from Unilib is a modification that addresses loops. 

The code requires modification to run under Icon as Unicon extended key(X) to operate on lists and records not just tables.


```Unicon
procedure deepcopy(A, cache)  #: return a deepcopy of A
    local k

    /cache := table()        # used to handle multireferenced objects
    if \cache[A] then return cache[A]

    case type(A) of {
        "table"|"list": {
            cache[A] := copy(A)
            every cache[A][k := key(A)] := deepcopy(A[k], cache)
            }
        "set": {
            cache[A] := set()
            every insert(cache[A], deepcopy(!A, cache))
            }
        default: {           # records and objects (encoded as records)
            cache[A] := copy(A)
            if match("record ",image(A)) then {
                every cache[A][k := key(A)] := deepcopy(A[k], cache)
                }
            }
        }
    return .cache[A]
end
```


The following code demonstrates deepcopy usage and that the resulting structure is different from the original by comparing assignment, copy, and deepcopy.


```Icon
link printf,ximage

procedure main()

   knot    := makeknot()     # create a structure with loops
   knota   := knot           # copy by assignment (reference)
   knotc   := copy(knot)     # built-in copy (shallow)
   knotdc  := deepcopy(knot) # deep copy

    
   showdeep("knota  (assignment) vs. knot",knota,knot) 
   showdeep("knotc  (copy)  vs. knot",knotc,knot) 
   showdeep("knotdc (deepcopy)  vs. knot",knotdc,knot) 

   xdump("knot   (original)",knot)
   xdump("knota  (assignment)",knota)
   xdump("knotc  (copy)",knotc)
   xdump("knotdc (deepcopy)",knotdc)
end

record rec1(a,b,c)      # record for example

class Class1(a1,a2)     # class - looks like a record under the covers
   method one()
      self.a1 := 1
      return
   end
initially  
   self.a1 := 0
end


procedure makeknot()    #: return a homogeneous structure with loops
   L := [9,8,7]
   T := table()
   T["a"] := 1
   R := rec1(T)
   S := set(R)
   C := Class1()
   C.one()
   T["knot"] := [L,R,S,C]
   put(L,R,S,T,C)
   return L
end

procedure showdeep(tag,XC,X)    #: demo to show (non-)equivalence of list elements
   printf("Analysis of copy depth for %s:\n",tag)
   showequiv(XC,X)
   every showequiv(XC[i := 1 to *X],X[i])
end

procedure showequiv(x,y)       #: show (non-)equivalence of two values
   return printf("   %i %s %i\n",x,if x === y then "===" else "~===",y)
end
```


{{libheader|Unicon Code Library}}
[https://tapestry.tucson.az.us/twiki/bin/view/Main/DeepCopy DeepCopy.icn]
{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides printf] 
[http://www.cs.arizona.edu/icon/library/src/procs/ximage.icn ximage.icn provides xdump] 

The sample of output below compares all elements of a copy of a structure against the original.  Immutable types like numbers, strings, and csets will show as the same (i.e. ===) and different mutable types will show as not the same (i.e. ~===).  This clearly shows the difference between assignment, copy, and deepcopy.

```txt
Analysis of copy depth for knota  (assignment) vs. knot:
   list_11(7) === list_11(7)
   9 === 9
   8 === 8
   7 === 7
   record rec1_2(3) === record rec1_2(3)
   set_2(1) === set_2(1)
   table_2(2) === table_2(2)
   record Class1__state_2(4) === record Class1__state_2(4)
Analysis of copy depth for knotc  (copy)  vs. knot:
   list_13(7) ~=== list_11(7)
   9 === 9
   8 === 8
   7 === 7
   record rec1_2(3) === record rec1_2(3)
   set_2(1) === set_2(1)
   table_2(2) === table_2(2)
   record Class1__state_2(4) === record Class1__state_2(4)
Analysis of copy depth for knotdc (deepcopy)  vs. knot:
   list_14(7) ~=== list_11(7)
   9 === 9
   8 === 8
   7 === 7
   record rec1_3(3) ~=== record rec1_2(3)
   set_3(1) ~=== set_2(1)
   table_4(2) ~=== table_2(2)
   record Class1__state_3(4) ~=== record Class1__state_2(4)
...
```


Another way to show the difference in the structures is to use the xdump procedure will produce the following in stderr (&errout):
```txt
knot   (original)"
L11 := list(7)
   L11[1] := 9
   L11[2] := 8
   L11[3] := 7
   L11[4] := R_rec1_2 := rec1()
      R_rec1_2.a := T2 := table(&null)
         T2["a"] := 1
         T2["knot"] := L12 := list(4)
            L12[1] := L11
            L12[2] := R_rec1_2
            L12[3] := S2 := set()
               insert(S2,R_rec1_2)
            L12[4] := R_Class1__state_2 := Class1__state()
               R_Class1__state_2.a1 := 1
   L11[5] := S2
   L11[6] := T2
   L11[7] := R_Class1__state_2
...
```



## J


J uses pass by value semantics (typically implemented as copy on write) so Deepcopy is trivial -- values inside the language are immutable.


```j
   a=:b=: 2 2 2 2 2  NB. two copies of the same array
   b=: 3 (2)} b  NB. modify one of the arrays
   b
2 2 3 2 2
   a
2 2 2 2 2
```


That said, J can reference values outside the language.  But Deepcopy of those values is, by definition, outside the scope of the language.  Usually, bringing the values into the language is sufficient.

Another possible exception would be classes and objects (which are not values but collections of references to values).  But as a general rule copying of an object should be delegated to the object itself rather than imposed from the outside.  Also, "deepcopy" violates the Law of Demeter as well as the concept of black-box reuse -- if you need deepcopy in J, you probably should not be representing your data structure as objects.


## JavaScript

You can use JSON for ordinary objects.

```JavaScript

var deepcopy = function(o){
  return JSON.parse(JSON.stringify(src));
};

var src = {foo:0,bar:[0,1]};
print(JSON.stringify(src));
var dst = deepcopy(src);
print(JSON.stringify(src));

```

You can go further if you have <code>uneval()</code>.  You can even deep copy objects with cyclic references.

```JavaScript

var deepcopy = function(o){
  return eval(uneval(o));
};
var src = {foo:0,bar:[0,1]};
src['baz'] = src;
print(uneval(src));
var dst = deepcopy(src);
print(uneval(src));

```


## jq

The distinction between "deep" and "shallow" copying is as irrelevant in a jq program as in elementary arithmetic.  There is only one "equality" operator in jq and it is defined in terms of equality of values.

In jq, a variable is merely a reference to a value, so in the pipeline:

 [1,2] | . as $x | . as $y

both $x and $y are simply references to [1,2].  The same applies to function parameters.

jq's data values are JSON values, and in particular there are no pointers, and there is no way to create self-referential data structures as is possible, for example, in JavaScript.  In JavaScript, the following pair of assignments would create a self-referential array:

 x=[1,2]; x[0]=x;                     // javascript

In jq, the superficially similar sequence:

 [1,2] as $x | ($x|setpath([0];$x))   # jq

merely emits [[1,2], 2].


## Julia


```julia
# v0.6.0

cp = deepcopy(obj)
```



## Kotlin

The JDK has an Object.clone() method but it only produces a 'shallow' copy.

If a 'deep' copy is needed, then usually a custom method would be written.

However, an easy way to produce a deep copy for any object, including one which contains cyclic or null references, is to serialize it to memory and then deserialize it back to a new object. A drawback is that the class of the object to be serialized needs to be marked as implementing the Serializable interface (albeit it contains no members).

The serialization approach is used below.

```scala
// Version 1.2.31

import java.io.Serializable
import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import java.io.ObjectOutputStream
import java.io.ObjectInputStream

fun <T : Serializable> deepCopy(obj: T?): T? {
    if (obj == null) return null
    val baos = ByteArrayOutputStream()
    val oos  = ObjectOutputStream(baos)
    oos.writeObject(obj)
    oos.close()
    val bais = ByteArrayInputStream(baos.toByteArray())
    val ois  = ObjectInputStream(bais)
    @Suppress("unchecked_cast")
    return ois.readObject() as T
} 

class Person(
    val name: String,
    var age: Int,
    val sex: Char,
    var income: Double,
    var partner: Person?
) : Serializable

fun printDetails(p1: Person, p2: Person?, p3: Person, p4: Person?) {
    with (p3) {
        println("Name    : $name")
        println("Age     : $age")
        println("Sex     : $sex")
        println("Income  : $income")
        if (p4 == null) {
            println("Partner : None")
        }
        else {
            println("Partner :-")
            with (p4) {
                println("  Name   : $name")
                println("  Age    : $age")
                println("  Sex    : $sex")
                println("  Income : $income")
            }
        }
        println("\nSame person as original '$name' == ${p1 === p3}")
        if (p4 != null) {
            println("Same person as original '${p2!!.name}' == ${p2 === p4}")
        }
    }
    println()
}

fun main(args: Array<String>) {
    var p1 = Person("John", 35, 'M', 50000.0, null)
    val p2 = Person("Jane", 32, 'F', 25000.0, p1)
    p1.partner = p2
    var p3 = deepCopy(p1)
    val p4 = p3!!.partner
    printDetails(p1, p2, p3, p4)

    println("..or, say, after 2 years have elapsed:-\n")
    with (p1) {
        age = 37
        income = 55000.0
        partner = null
    }
    p3 = deepCopy(p1)
    printDetails(p1, null, p3!!, null)
}
```


{{out}}

```txt

Name    : John
Age     : 35
Sex     : M
Income  : 50000.0
Partner :-
  Name   : Jane
  Age    : 32
  Sex    : F
  Income : 25000.0

Same person as original 'John' == false
Same person as original 'Jane' == false

..or, say, after 2 years have elapsed:-

Name    : John
Age     : 37
Sex     : M
Income  : 55000.0
Partner : None

Same person as original 'John' == false

```



## Lasso

Every Lasso type has an ascopy and ascopydeep method.


```Lasso
local(copy) = #myobject->ascopydeep
```



## Lingo


```lingo
-- Supports lists, property lists, images, script instances and scalar values (integer, float, string, symbol).
on deepcopy (var, cycleCheck)   
  case ilk(var) of
  #list, #propList, #image: 
    return var.duplicate()
  #instance:
    if string(var) starts "<Xtra " then return var -- deep copy makes no sense for Xtra instances
    if voidP(cycleCheck) then cycleCheck = [:]
    if not voidP(cycleCheck[var]) then return cycleCheck[var]
    copy = var.script.rawNew()
    cycleCheck[var] = copy
    repeat with i = 1 to var.count
      copy.setProp(var.getPropAt(i), deepcopy(var[i], cycleCheck))
    end repeat
    return copy
  otherwise: 
    return var
  end case
end
```


```lingo
val = [#foo:42, "bar":[1,2,3, "Hello world!"]]
put deepcopy(val)
-- [#foo: 42, "bar": [1, 2, 3, "Hello world!"]]

val = script("MyClass").new()
val.foo = 42
val.bar = [1, 2, 3, "Hello world!"]]
copy = deepcopy(val)
```



## Lua

These are simple functions which illustrate the possibility to use tables as table keys in Lua.
More robust implementation, such as [https://gist.github.com/Deco/3985043#file-deepcopy-lua this],
would also deep-copy metatables, function upvalues, etc.


### Recursive



```Lua
function _deepcopy(o, tables)
 
  if type(o) ~= 'table' then
    return o
  end
 
  if tables[o] ~= nil then
    return tables[o]
  end
 
  local new_o = {}
  tables[o] = new_o
 
  for k, v in next, o, nil do
    local new_k = _deepcopy(k, tables)
    local new_v = _deepcopy(v, tables)
    new_o[new_k] = new_v
  end
 
  return new_o
end
 
function deepcopy(o)
  return _deepcopy(o, {})
end
```


{{out}}

```txt
> a = {'beans', 'sausage', 'eggs', {table='wood', fork='silver', dish='china'}}
> aa = deepcopy(a)
> 
> a   
table: 0x55ef852b8390
> aa
table: 0x55ef852b4e50
> 
> aa[2]
sausage
> aa[2] = 'bacon'
> aa[2]
bacon
> a[2]
sausage
> 
> a[4]
table: 0x55ef852b8880
> aa[4]
table: 0x55ef8528d6d0
> aa[4].dish 
china
```

Works with self-referencing / cyclic tables as well:

```txt
> z = {}
> z[1] = {}
> z[1][2] = {}
> z[1][2][3] = z
> zz = deepcopy(z)
> z  
table: 0x557659e84c00
> zz
table: 0x557659e59630
> z[1]
table: 0x557659e85160
> zz[1]
table: 0x557659e596c0
> z[1][2]
table: 0x557659e85790
> zz[1][2]
table: 0x557659e597a0
> z[1][2][3]
table: 0x557659e84c00
> zz[1][2][3]
table: 0x557659e59630
```

Table keys are also deep-cloned:

```txt
> q = {}
> q[q] = q
> qq = deepcopy(q)
> qq[q]
nil
> qq[qq]
table: 0x557659e859f0
> qq[qq] == qq
true
```



===Non-recursive===

These functions have a string argument <code>mode</code> (like <code>__mode</code> in a metatable),
which determines whether keys, values, both or neither will be deep-copied (defaults to values).


====Breadth-first====


```Lua
function deepcopy(o, mode)

  if type(o) ~= 'table' then
    return o
  end

  mode = mode and mode:lower() or 'v'
  local deep_keys   = mode:find('k')
  local deep_values = mode:find('v')

  local new_t = {}
  local stack = {o}
  local tables = {[o] = new_t}

  local function copy(v)
    if type(v) ~= 'table' then
      return v
    end
    if tables[v] == nil then
      tables[v] = {}
      stack[#stack+1] = v
    end
    return tables[v]
  end

  while #stack ~= 0 do
    local t = table.remove(stack)
    local new_t = tables[t]
    
    for k,v in next, t, nil do
      if deep_keys   then k = copy(k) end
      if deep_values then v = copy(v) end
      new_t[k] = v
    end
  end

  return new_t
end
```



```txt
> q = {}
> q[q] = q
> q1 = deepcopy(q)
> q2 = deepcopy(q, 'kv')
> q1[q] == q1
true
> q1[q1] == q1
false
> q2[q2] == q2
true
```



====Depth-first====

When the current source table <code>t</code> contains either value (<code>v</code>) or key
(<code>k</code>) which is a table, then the source table <code>t</code> is pushed onto stack,
together with the current key <code>k</code>. The key/value then becomes the current source
table and current key is reset to <code>nil</code>. In other words, the current "context" is
saved and replaced by a new one, consisting of the subtable (<code>k</code> or <code>v</code>)
and a <code>nil</code> key (<code>nil</code>, because we must start traversing the subtable
from the beginning).

If both key <code>k</code> and value <code>v</code> are tables (and <code>mode</code> is
<code>"kv"</code>), then an additional value <code>0</code> is pushed onto stack to indicate
that both key and value must be copied. The key table <code>k</code> is then copied, and after
copying it and seeing <code>0</code> on top of the stack, the value table <code>v</code>
is copied. Then, the context is restored and copying of the parent table continues.


```Lua
function deepcopy(o, mode)

  if type(o) ~= 'table' then
    return o
  end

  mode = mode and mode:lower() or 'v'
  local deep_keys   = mode:find('k')
  local deep_values = mode:find('v')

  local tables = {[o] = {}} -- list of known tables (to handle circular tables)
  local stack = {o}         -- first table which will be popped from stack is the root table
                            --   and the key must be `nil` because we are at the beginning
                            --   of the root table. since it's `nil`, we don't need to put it
                            --   on the stack - `table.remove()` will by default return `nil`
                            --   when called on an empty table.

  while #stack ~= 0 do
    local t, new_t, k, v = table.remove(stack)  -- assigns only to `t`,
                                                --   other variables are set to `nil`
    if t ~= 0 then
      k = table.remove(stack) -- restore the context

    else  -- we finished copying the key, now copy the value
      t = stack[#stack]    -- get the parent table to retrieve the value from
      k = stack[#stack-1]  -- get saved key
      t = t[k]  -- retrieve the value from the parent table and set it as the current table
      k = nil   -- reset key (start traversing the value table from the beginning)
    end
    new_t = tables[t] -- get the new table from the list of known tables

    if k ~= nil then  -- this is always true except for
                      --  1. when we just popped the root table `o`
                      --  2. when we just finished copying the key table
                      --     and now we have to copy the value table
      local v = t[k]  -- get the original value
                      -- if we want to deep-copy keys, then get its copy from the list
                      --   of known tables. if it's not there, then it isn't a table,
                      --   so keep its original value. same goes for the value.
      if deep_keys   then k = tables[k] or k end
      if deep_values then v = tables[v] or v end
      new_t[k] = v    -- put value into the new table
    end

    k,v = next(t,k) -- in case we have just started traversing the root table `o`, this retrieves
                    --   the first key and value, as well as in case we have just finished copying
                    --   the key table and are now copying the value table. otherwise, it continues
                    --   where we left off when we descended into subtable.

    while k ~= nil do
      -- we need to deep-copy the key/value only if
      --  1. we want to do it (eg. `mode` specifies to deep-copy keys/values), AND
      --  2. it is a table, AND
      --  3. we haven't copied it already (and are not copying it right now)
      local copy_key   = deep_keys   and type(k) == 'table' and not tables[k]
      local copy_value = deep_values and type(v) == 'table' and not tables[v]

      if not copy_key and not copy_value then -- boring stuff
        -- if either `deep_keys` is `nil` (we don't want to deep-copy keys)
        --   or `tables[k]` is `nil` (the key is not a table), then keep the key's original value,
        --   otherwise use the value saved in `tables`. same goes for the value.
        local new_k = deep_keys   and tables[k] or k
        local new_v = deep_values and tables[v] or v
        new_t[new_k] = new_v  -- put the value into the new table

      else -- copy_key or copy_value
        stack[#stack+1] = k -- save current context
        stack[#stack+1] = t

        if copy_key then
          t = k   -- descend into the key table
          if copy_value then
            stack[#stack+1] = 0 -- remember we have to copy the value table as well
            tables[v] = {}      -- create new table for the value beforehand
          end
        else -- copy only the value
          t = v  -- descent into the value table
        end

        new_t = {}        -- create new table
        tables[t] = new_t -- add it to the list of known tables
        k = nil           -- reset the key
      end

      k,v = next(t,k) -- get next key/value (or first, in case we just descended into a subtable)
    end
  end

  return tables[o]  -- return the copy corresponding to the root table `o`
end
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Everything in Mathematica is a value type.

```Mathematica
a = {"foo", \[Pi], {<|
     "deep" -> {# + 
         1 &, {{"Mathematica"}, {{"is"}, {"a"}}, {{{"cool"}}}, \
{{"programming"}, {"language!"}}}}|>}};
b = a;
a[[2]] -= 3;
a[[3, 1, 1, 1]] = #^2 &;
Print[a];
Print[b];
```

{{out}}

```txt
{foo, -3 + π, {<|deep -> {#1^2 &, {{Mathematica}, {{is}, {a}}, {{{cool}}}, {{programming}, {language!}}}}|>}}
{foo, π, {<|deep -> {#1+1&, {{Mathematica}, {{is}, {a}}, {{{cool}}}, {{programming}, {language!}}}}|>}}
```



## Nim

Works with Nim 0.9.5:

```nim
deepCopy(newObj, obj)
```

For example with binary trees:

```nim
import queues, sequtils

type
  Node[T] = ref TNode[T]
  TNode[T] = object
    data: T
    left, right: Node[T]

proc newNode[T](data: T; left, right: Node[T] = nil): Node[T] =
  Node[T](data: data, left: left, right: right)

proc preorder[T](n: Node[T]): seq[T] =
  if n == nil: @[]
  else: @[n.data] & preorder(n.left) & preorder(n.right)

var tree = 1.newNode(
             2.newNode(
               4.newNode(
                 7.newNode),
               5.newNode),
             3.newNode(
               6.newNode(
                 8.newNode,
                 9.newNode)))

var tree2: Node[int]
tree2.deepCopy tree
tree2.data = 10
tree2.left.data = 20
tree2.right.left.data = 90

echo "Tree2:"
echo preorder tree2

echo "Tree:"
echo preorder tree
```

Output:

```txt
Tree2:
@[10, 20, 4, 7, 5, 3, 90, 8, 9]
Tree:
@[1, 2, 4, 7, 5, 3, 6, 8, 9]
```



## OCaml


This code is just provided in order to achieve this task, but an OCaml programmer wouldn't use this kind of code, because this <code>copy</code> function is made generic due to the use of the [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Obj.html <code>Obj</code>] module, and it is not recommanded to use it.


```ocaml
let rec copy t =
  if Obj.is_int t then t else
    let tag = Obj.tag t in
    if tag = Obj.double_tag then t else
    if tag = Obj.closure_tag then t else
    if tag = Obj.string_tag then Obj.repr (String.copy (Obj.obj t)) else
    if tag = 0 || tag = Obj.double_array_tag then begin
      let size = Obj.size t in
      let r = Obj.new_block tag size in
      for i = 0 to pred size do
        Obj.set_field r i (copy (Obj.field t i))
      done;
      r
    end else failwith "copy" ;;

let copy (v : 'a) : 'a = Obj.obj (copy (Obj.repr v))
```


OCaml programmers will prefer to use specialised copy functions for each mutable types. For base types like strings and arrays, the standard library provides copy functions: <code>String.copy</code> and <code>Array.copy</code>. For mutable user-defined data structures, we will create a copy function based on these previous copy functions. For example in the module [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Hashtbl.html <code>Hashtbl</code>], the type is a record containing an integer and an array, so the copy function is defined as below:

```ocaml
let copy h =
  { size = h.size;
    data = Array.copy h.data }
```



## PARI/GP

All copies in GP are deep; this is one of its major inefficiencies when working with large objects.

In PARI, this is accomplished with the command <code>gcopy</code> rather than <code>shallowcopy</code> or <code>leafcopy</code>. The function takes and returns a <code>GEN</code>. See section 10.6 of the [http://pari.math.u-bordeaux.fr/pub/pari/manuals/2.5.0/libpari.pdf User's Guide to the PARI Library].

=={{Header|Perl}}==

use [http://search.cpan.org/perldoc?Storable Storable]; <code>Storable::dclone()</code> is exactly what you are looking for.


```Perl

#!/usr/bin/perl
use strict;
use warnings;
use Storable;
use Data::Dumper;

my $src = { foo => 0, bar => [0, 1] };
$src->{baz} = $src;
my $dst = Storable::dclone($src);
print Dumper($src);
print Dumper($dst);

```



## Perl 6


Perl 6 doesn't currently provide a proper mechanism for deep copies, but depending on your requirements you could use one of these work-arounds:



'''1) Use <code>.deepmap(*.clone)</code>:'''

<tt>.deepmap</tt> constructs a copy of the data structure, and <tt>.clone</tt> makes a shallow copy of each leaf node. Limitations:
* Hangs indefinitely when given a self-referential data structure.
* Descends only into <tt>Iterable</tt> collections (like <tt>Array</tt>/<tt>Hash</tt>), which means that a <tt>Pair</tt> or a typical custom object would be considered a leaf node.


```perl6
my %x = foo => 0, bar => [0, 1];
my %y = %x.deepmap(*.clone);

%x<bar>[1]++;
say %x;
say %y;
```


{{out}}

```txt

{bar => [0 2], foo => 0}
{bar => [0 1], foo => 0}

```




'''2) Use <code>.perl.EVAL</code>:'''

<tt>.perl</tt> serializes the data structure to Perl 6 code, and <tt>.EVAL</tt> deserializes it. Limitations:
* Doesn't work correctly if the data structure contains elements that can't be properly serialized, such as closures or file handles.


```perl6
use MONKEY-SEE-NO-EVAL;

my %x = foo => 0, bar => [0, 1];
my %y = %x.perl.EVAL;

%x<bar>[1]++;
say %x;
say %y;
```


{{out}}

```txt

{bar => [0 2], foo => 0}
{bar => [0 1], foo => 0}

```



## Phix

Handled natively. Phix uses reference counting with copy-on-write semantics; the initial copy is fast even for huge complex and deeply nested structures (copying a single machine-word-sized reference and incrementing a single machine-word-sized reference count), and when a shared object (anything with a refcount>1) is modified, an internal clone of the minimum necessary levels occurs, with all the rest of the structure remaining shared (but obviously still properly protected in the same way).

```Phix
object a, b
    a = {1,{2,3},"four",{5.6,7,{8.9}}}
    b = a
    b[3] = 4
?a
?b
```

{{out}}

```txt

{1,{2,3},"four",{5.6,7,{8.9}}}
{1,{2,3},4,{5.6,7,{8.9}}}

```

It is worth noting that this mechanism is also used for parameter passing, and when a local variable is both passed as a parameter and assigned on return, automatic pass-by-reference handling kicks in to avoid any unnecessary internal cloning.

Should you for some (probably misguided) reason really want it, a recursive clone function might look like this:

```Phix
function deep_copy(object o)
object res
    if atom(o) then
        res = o
    else
        res = repeat(' ',length(o))
        for i=1 to length(o) do
            res[i] = deep_copy(o[i])
        end for
    end if
    return res
end function

object c = deep_copy(b)
?c
```

Or you could just serialize and deserialize:

```Phix
include builtins\serialize.e
object d = deserialize(serialize(a))
?d
```

{{out}}

```txt

{1,{2,3},4,{5.6,7,{8.9}}}
{1,{2,3},"four",{5.6,7,{8.9}}}

```


=={{Header|PHP}}==

PHP provides the <code>clone</code> operator ([http://www.php.net/manual/en/language.oop5.cloning.php docs]) for shallow copying, and allows you to hook into a magic class method called <code>__clone()</code> in your classes to do some of the lifting to create deeper copies, but this method won't create a true deep copy if you don't write the code to manage it in each of the child classes.


```PHP
<?php
class Foo
{
    public function __clone()
    {
        $this->child = clone $this->child;
    }
}

$object = new Foo;
$object->some_value = 1;
$object->child = new stdClass;
$object->child->some_value = 1;

$deepcopy = clone $object;
$deepcopy->some_value++;
$deepcopy->child->some_value++;

echo "Object contains {$object->some_value}, child contains {$object->child->some_value}\n",
     "Clone of object contains {$deepcopy->some_value}, child contains {$deepcopy->child->some_value}\n";
?>
```



Automatically generated deep copies can be created in any situation where your object graph can be serialized (i.e. does not contain any Closures or resources like DB connections or file handles):


```PHP
<?php

// stdClass is a default PHP object
$object = new stdClass;
$object->some_value = 1;
$object->child = new stdClass;
$object->child->some_value = 1;

$deepcopy = unserialize(serialize($object));
$deepcopy->some_value++;
$deepcopy->child->some_value++;

echo "Object contains {$object->some_value}, child contains {$object->child->some_value}\n",
     "Clone of object contains {$deepcopy->some_value}, child contains {$deepcopy->child->some_value}\n";
```



## PicoLisp

A shallow copy can be done with '[http://software-lab.de/doc/refC.html#copy copy]'. This function takes care of cons pairs and lists, no matter whether they are cyclic, or end in NIL or some other data structure.

For a known depth, it might be used in combination with other list functions. For example, to copy a non-cyclic structure of depth 2 with '[http://software-lab.de/doc/refM.html#mapcar mapcar]':

```PicoLisp
(mapcar copy List)
```

Copying non-cyclic structures of arbitrary depth and list-termination could be handled with a custom function (using '[http://software-lab.de/doc/refC.html#cons cons]'):

```PicoLisp
(de deepCopy (X)
   (if (atom X)
      X
      (cons (deepCopy (car X)) (deepCopy (cdr X))) ) )
```

Test:

```PicoLisp
: (setq A '((a . b) (c d e) f g . e))
-> ((a . b) (c d e) f g . e)

: (setq B (deepCopy A))
-> ((a . b) (c d e) f g . e)

: A
-> ((a . b) (c d e) f g . e)

: B
-> ((a . b) (c d e) f g . e)

: (= A B)
-> T              # A and its copy B are structure-equal
: (== A B)
-> NIL            # but they are not identical (pointer-equal)

: (cadr A)
-> (c d e)

: (cadr B)
-> (c d e)

: (== (cadr A) (cadr B))
-> NIL            # The same holds for sub-structures
```

For cyclic structures, the above 'deepCopy' function could be extended, to remember already visited structures and their copies in a mark list:

```PicoLisp
(de deepCopy (X)
   (let Mark NIL
      (recur (X)
         (cond
            ((atom X) X)
            ((asoq X Mark) (cdr @))
            (T
               (prog1 (cons)
                  (push 'Mark (cons X @))
                  (set @ (recurse (car X)))
                  (con @ (recurse (cdr X))) ) ) ) ) ) )
```

Test:

```PicoLisp
: (setq A '(a b .)  B (deepCopy A))
-> (a b .)
: A
-> (a b .)
: B
-> (a b .)

: (= A B)
-> T              # A and its copy B are structure-equal

: (== A B)
-> NIL            # but they are not identical (pointer-equal)
```



## Python


```Python
import copy
deepcopy_of_obj = copy.deepcopy(obj)
```



## Racket


Unlike most Lisps, Racket's pairs are immutable, but they still support sharing
and cycles using <tt>shared</tt> (or at the lower level, via
<tt>make-reader-graph</tt>).  This would make the implementation a little more
complicated, but it's much easier to just dump the structure out and re-read it
to get a new copy:


```Racket

#lang racket

(define (deepcopy x)
  ;; make sure that all sharings are shown
  (parameterize ([print-graph #t]) (read (open-input-string (format "~s" x)))))

(define (try x)
  ;; use the same setting to see that it worked
  (parameterize ([print-graph #t])
    (printf "original: ~s\n" x)
    (printf "deepcopy: ~s\n" (deepcopy x))
    ;; print both also, which shows that they are indeed different
    (printf "both: ~s\n" (list x (deepcopy x)))))
(try (shared ([x (cons 1 x)]) (list x x)))

```


Output:

```txt

original: (#0=(1 . #0#) #0#)
deepcopy: (#0=(1 . #0#) #0#)
both: ((#0=(1 . #0#) #0#) (#1=(1 . #1#) #1#))

```



## Ruby

Rubyists can hack a deep copy by using the core class Marshal. The intermediate form of <code>Marshal.load(Marshal.dump object)</code> saves the object and any descendant objects.


```ruby
# _orig_ is a Hash that contains an Array.
orig = { :num => 1, :ary => [2, 3] }
orig[:cycle] = orig	# _orig_ also contains itself.

# _copy_ becomes a deep copy of _orig_.
copy = Marshal.load(Marshal.dump orig)

# These changes to _orig_ never affect _copy_,
# because _orig_ and _copy_ are disjoint structures.
orig[:ary] << 4
orig[:rng] = (5..6)

# Because of deep copy, orig[:ary] and copy[:ary]
# refer to different Arrays.
p orig	# => {:num=>1, :ary=>[2, 3, 4], :cycle=>{...}, :rng=>5..6}
p copy	# => {:num=>1, :ary=>[2, 3], :cycle=>{...}}

# The original contains itself, and the copy contains itself,
# but the original and the copy are not the same object.
p [(orig.equal? orig[:cycle]),
   (copy.equal? copy[:cycle]),
   (not orig.equal? copy)]	# => [true, true, true]
```


Marshal cannot dump an object that relates to the system (like Dir or IO), relates to the program (like MatchData or Thread), uses an anonymous class or module, or has a singleton method. (<code>ri Marshal.dump</code> documents this restriction.) If Marshal encounters any such object, then the deep copy fails.

Marshal can dump internal objects, but never copies them. The internal objects are <code>nil</code>, <code>false</code>, <code>true</code> and instances of Fixnum or Symbol. For example, <code>Marshal.load(Marshal.dump :sym)</code> returns the original <code>:sym</code>, not a copy. <blockquote style="font-size: smaller;">The internal objects are almost immutable, so there is almost no reason to copy them. Yet, there are esoteric ways to modify them. For example, <code>nil.instance_eval { @i = 1 }</code> would modify <code>nil</code>. A program cannot have another copy of <code>nil</code> to escape such modification. If there was a deep copy of some object that contains <code>nil</code>, then such modification would also affect <code>nil</code> inside such copy.</blockquote>


## Rust

This is what the <code>Clone</code> trait exists for although the depth of the copy is arbitrary and up to the type that implements the trait.


```rust
// The compiler can automatically implement Clone on structs (assuming all members have implemented Clone).
#[derive(Clone)]
struct Tree<T> {
    left: Leaf<T>,
    data: T,
    right: Leaf<T>,
}

type Leaf<T> = Option<Box<Tree<T>>>;

impl<T> Tree<T> {
    fn root(data: T) -> Self {
        Self { left: None, data, right: None }
    }

    fn leaf(d: T) -> Leaf<T> {
        Some(Box::new(Self::root(d)))
    }
}

fn main() {
    let mut tree = Tree::root([4, 5, 6]);
    tree.right = Tree::leaf([1, 2, 3]);
    tree.left = Tree::leaf([7, 8, 9]);

    let newtree = tree.clone();
}
```



## Scheme


```Scheme

(define (deep-copy-1 exp)
  ;; basic version that copies an arbitrary tree made up of pairs
  (cond ((pair? exp)
	 (cons (deep-copy-1 (car exp))
	       (deep-copy-1 (cdr exp))))
	;; cases for extra container data types can be
	;; added here, like vectors and so on
	(else ;; atomic objects
	 (if (string? exp)
	     (string-copy exp)
	     exp))))

(define (deep-copy-2 exp)
  (let ((sharing (make-hash-table)))
    (let loop ((exp exp))
      (cond ((pair? exp)
	     (cond ((get-hash-table sharing exp #f)
		    => (lambda (copy)
			 copy))
		   (else
		    (let ((res (cons #f #f)))
		      (put-hash-table! sharing exp res)
		      (set-car! res (loop (car exp)))
		      (set-cdr! res (loop (cdr exp)))
		      res))))
	    (else
	     (if (string? exp)
		 (string-copy exp)
		 exp))))))

(define t1 '(a b c d))
(define t2 (list #f))
(set-car! t2 t2)
(define t2b (list #f))
(set-car! t2b t2b)
(define t3 (list #f #f))
(set-car! t3 t3)
(set-car! (cdr t3) t3)
(define t4 (list t2 t2b))

;> (print-graph #t)
;> (deep-copy-2 t1)
;(a b c d)
;> (deep-copy-2 t2)
;#0=(#0#)
;> (deep-copy-2 t3)
;#0=(#0# #0#)
;> (deep-copy-2 t4)
;(#0=(#0#) #1=(#1#))

```



## Sidef

''Object.dclone()'' returns a deep clone of any mutable object.

```ruby
var src = Hash(foo => 0, bar => [0,1])

# Add a cyclic reference
src{:baz} = src

# Make a deep clone
var dst = src.dclone

# The address of src
say src.object_id
say src{:baz}.object_id

# The address of dst
say dst.object_id
say dst{:baz}.object_id
```

{{out}}

```txt

15154128
15154128
25296304
25296304

```



## Tcl

Tcl uses an immutable value model that is implemented as copy-on-write at the low level, so deep copies are generally not required. However, they can be achieved by simply appending a letter to the value and stripping it off again:

```tcl
set deepCopy [string range ${valueToCopy}x 0 end-1]
```

For objects (as introduced in Tcl 8.6), there is a command to create a copy:

```tcl
set copiedObject [oo::copy $originalObject]
```


{{omit from|Haskell}}
{{omit from|GUISS}}
{{omit from|PureBasic}}
{{omit from|UNIX Shell|Does not have complex data structures}}
{{omit from|ZX Spectrum Basic}}
