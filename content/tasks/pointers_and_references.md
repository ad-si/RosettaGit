+++
title = "Pointers and references"
description = ""
date = 2019-08-25T05:25:15Z
aliases = []
[extra]
id = 1784
[taxonomies]
categories = ["task", "Basic Data Operations"]
tags = []
languages = [
  "ada",
  "algol_68",
  "autohotkey",
  "bbc_basic",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "e",
  "echolisp",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "lua",
  "m2000_interpreter",
  "nim",
  "ocaml",
  "oforth",
  "oorexx",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "pop11",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "sas",
  "scala",
  "sidef",
  "standard_ml",
  "tcl",
  "toka",
  "vba",
  "xpl0",
  "zkl",
]
+++

## Task

{{Task|Basic Data Operations}}{{basic data operation}}In this task, the goal is to demonstrate common operations on pointers and references. These examples show pointer operations on the stack, which can be dangerous and is rarely done. Pointers and references are commonly used along with [[Memory allocation]] on the [[heap]].


## Ada


### Pointers

In Ada pointer types are called access types. Access types are subject of so-called accessibility checks, which in Ada are intended to prevent appearance of dangling pointers. (Though accessibility checks can be circumvented)

Create a pool specific access type for an integer

```ada
type Int_Access is access Integer;
Int_Acc : Int_Access := new Integer'(5);
```

A pool specific access type can be constrained to never be null. For such pointers the compiler can omit checks of being null upon dereferencing.

```ada
type Safe_Int_Access is not null access Integer;
```

General access types can deal with objects allocated in any pool as well as ones on the stack. For example, create a pointer to a stack-allocated integer

```ada
declare
   type Int_Ptr is access all Integer;
   Ref : Int_Ptr;
   Var : aliased Integer := 3;
   Val : Integer := Var;
begin
   Ref := Var'Access; -- "Ref := Val'Access;" would be a syntax error
```

The attribute 'Access (and also 'Unchecked_Access) is used to get a pointer to the object. Note that the object has to be declared ''aliased '' when it has to be referenced by a pointer, which is why one cannot write Val'Access. General access types can also be constrained to exclude null:

```ada
type Safe_Int_Ptr is not null access all Integer;
```



### Addresses

Ada does not provide pointer arithmetic, but does allow evaluation of the address

```ada
Var : Integer;
Var_Address : Address := Var'Address;
```

Addresses support operations of comparison, addition, and subtraction. Ada also supports conversion between address types and a predefined subtype of Integer named Integer_Address. This accommodates the conversion to a linear addressing of any hardware address scheme including address:offset used in the 8086 processor.

Ada allows the specification of a starting address for any object

```ada
-- Demonstrate the overlay of one object on another
A : Integer;
B : Integer;
for B'Address use A'Address;
-- A and B start at the same address
```


### References

References in Ada are achieved through object renaming declarations. A renaming produces a new view to the object:

```ada
type Container is array (Positive range <>) of Element;
for I in Container'Range loop
   declare
      Item : Element renames Container (I);
   begin
      Do_Something(Item); -- Here Item is a reference to Container (I)
   end;
end loop;
```


The forthcoming standard (Ada 2012) allows a more direct way to reference all the items in a container or an array:


```ada
type Container is array (Positive range <>) of Element;
for Item of Container loop
      Do_Something(Item);
end loop;
```



## ALGOL 68

The following code creates a pointer to an INT variable:

```algol68
INT var := 3;
REF INT pointer := var;
```

Access the integer variable through the pointer:

```algol68
INT v = pointer; # sets v to the value of var (i.e. 3) #
REF INT(pointer) := 42; # sets var to 42 #
```

Change the pointer to refer to another object:

```algol68
INT othervar;
pointer := othervar;
```

Change the pointer to not point to any object:

```algol68
pointer := NIL; # 0 cannot be cast to NIL #
```

Get a pointer to the first element of an array:

```algol68
[9]INT array;
pointer := array[LWB array];
```

There is no pointer arithmetic, eg no p +:=3

'''With references:'''

The following code creates a constant reference to an INT variable, effectively an alias:

```algol68
REF INT alias = var;
```

Access the integer variable through the reference:

```algol68
INT v2 = alias; # sets v2 to the value of var, that is, 3 #
alias := 42; # sets var to 42 #
```

Constand references cannot be changed to refer to other objects.

Pointers can be compared, but only for basic equality:

```algol68
printf(($"alias "b("IS","ISNT")" var!"l$, alias IS var));
```

Output: alias IS var!

Get a reference to the first element of an array:

```algol68
[9]INT array2;
REF INT ref3 = array2[LWB array2];
```

Changing the reference to refer to another object of the array is not possible.

ALGOL 68 also allows pointers to slices of rows and/or columns of arrays:

```algol68
[9,9]INT sudoku;
REF [,]INT middle;
middle := sudoku[4:6,4:6];
```


This includes pointers to sliced character arrays:

```algol68
[30]CHAR hay stack := "straw straw needle straw straw";
REF[]CHAR needle = hay stack[13:18];
needle[2:3] := "oo";
print((hay stack))
```

Output: straw straw noodle straw straw

## AutoHotkey

The address of the variable structure itself cannot be changed using built in methods.

```AutoHotkey
VarSetCapacity(var, 100)   ; allocate memory
NumPut(87, var, 0, "Char") ; store 87 at offset 0
MsgBox % NumGet(var, 0, "Char") ; get character at offset 0 (87)
MsgBox % &var   ; address of contents pointed to by var structure
MsgBox % *&var ; integer at address of var contents  (87)
```



## BBC BASIC

```bbcbasic
      REM Pointer to integer variable:
      pointer_to_varA = ^varA%
      !pointer_to_varA = 123456
      PRINT !pointer_to_varA

      REM Pointer to variant variable:
      pointer_to_varB = ^varB
      |pointer_to_varB = PI
      PRINT |pointer_to_varB

      REM Pointer to procedure:
      PROCmyproc : REM conventional call to initialise
      pointer_to_myproc = ^PROCmyproc
      PROC(pointer_to_myproc)

      REM Pointer to function:
      pointer_to_myfunc = ^FNmyfunc
      PRINT FN(pointer_to_myfunc)
      END

      DEF PROCmyproc
      PRINT "Executing myproc"
      ENDPROC

      DEF FNmyfunc
      = "Returned from myfunc"
```


=={{header|C}} and {{header|C++}}==
The following code creates a pointer to an int variable

```c
int var = 3;
int *pointer = &var;
```


Access the integer variable through the pointer:

```c
int v = *pointer; /* sets v to the value of var (i.e. 3) */
*pointer = 42; /* sets var to 42 */
```


Change the pointer to refer to another object

```c
int othervar;
pointer = &othervar;
```


Change the pointer to not point to any object

```c
pointer = NULL; /* needs having stddef.h included */
```

or

```c
pointer = 0; /* actually any constant integer expression evaluating to 0 could be used, e.g. (1-1) will work as well */
```

or

```c
pointer = (void*)0; /* C only, not allowed in C++ */
```


Get a pointer to the first element of an array:

```c
int array[10];
pointer = array;
/* or alternatively: */
pointer = &array[0];
```


Move the pointer to another object in the array

```c
pointer += 3; /* pointer now points to array[3] */
pointer -= 2; /* pointer now points to array[1] */
```


Access another object in the same array through the pointer

```c
v = pointer[3]; /* accesses third-next object, i.e. array[4] */
v = pointer[-1]; /* accesses previous object, i.e. array[0] */
/* or alternatively */
v = *(pointer + 3); /* array[4] */
v = *(pointer - 1); /* array[0] */
```


The following code snippet shows a practical example of using pointers in C with structs. Note there are many ways to access to a value using the * operator.


```c
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int val;
} num;

int addNodes(num **array, int elems);

int main(void) {
    int numElems, i;
    num *arr = NULL;
    numElems = addNodes(&arr, 10);
    for (i = 0; i < numElems; i++) {
        printf("%d) %d\n", i+1, arr[i].val);
    }
    free(arr);
    return 0;
}

int addNodes(num **array, int elems) {
    num *temp = NULL;
    int i;
    for (i = 0; i < elems; i++) {
        temp = realloc(*array, (i+1) * sizeof **array);
        if (temp == NULL) {
            free(*array); return -1;
        } else {
            *array = temp;
        }
        (*array)[i].val = i;
        // Alternatives:
        // ((*array)+i)->val = i; // or
        // (*((*array)+i)).val = i;
    }
    return i;
}
```



## C++


'''With pointers:'''
See 'C' example above.

C++ specific alternative to "The following code creates a pointer to an int variable":

```cpp
int* pointer2(&var);
```


'''With references:'''

The following code create a reference to an int variable:

```cpp
int var = 3;
int& ref = var;
// or alternatively:
int& ref2(var);
```


Access the integer variable through the reference

```cpp
int v = ref; // sets v to the value of var, that is, 3
ref = 42; // sets var to 42
```


References cannot be changed to refer to other objects, and cannot (legally) made to refer to no object.

Get a reference to the first element of an array:

```cpp
int array[10];
int& ref3 = array[0];
```


Changing the reference to refer to another object of the array is not possible.

Accessing another object of the array through the reference:

```cpp
v = (&ref)[3]; // read value of array[3]; however doing this is bad style
```


## C#
This example represents usage of Reference and Value.

```c#

static void Main(string[] args)
{
	int p;

	p = 1;
	Console.WriteLine("Ref Before: " + p);
	Value(ref p);
	Console.WriteLine("Ref After : " + p);

	p = 1;
	Console.WriteLine("Val Before: " + p);
	Value(p);
	Console.WriteLine("Val After : " + p);

	Console.ReadLine();
}

private static void Value(ref int Value)
{
	Value += 1;
}
private static void Value(int Value)
{
	Value += 1;
}

```


Example Result:

```txt

Ref Before: 1
Ref After : 2
Val Before: 1
Val After : 1

```


## COBOL


### Pointers

Pointers are declared like so, optionally with the type or program they will point to:

```cobol
       01  ptr                     USAGE POINTER TO Some-Type.
       01  prog-ptr                USAGE PROGRAM-POINTER "some-program". *> TO is optional
```


<code>USAGE POINTER</code> data items are used in conjunction with <code>BASED</code> data items, with <code>ALLOCATE</code> optionally giving a pointer the address of the allocated memory. Pointers can also be used to free allocated memory, which will cause the pointer to be set to <code>NULL</code>.

```cobol
ALLOCATE heap-item RETURNING ptr
...
FREE ptr
```


<code>USAGE PROGRAM-POINTER</code> data items are used to point to programs and their entry points.
```cobol
SET prog-ptr TO ENTRY "some-program"
```


Both types of pointer support basic pointer arithmetic.

```cobol
SET ptr1 UP BY 10
SET ptr2 DOWN BY LENGTH OF foo
```


Pointers can also be set to point to where other pointers are pointing or to other pointers themselves.

```cobol
SET ptr1 TO ptr2            *> ptr1 points to where ptr2 points
SET ptr2 TO ADDRESS OF ptr3 *> ptr2 points to ptr3
```


To alter the value pointed to by a pointer, the <code>SET</code> statement is needed once again and is used to set the address of <code>BASED</code> or <code>LINKAGE SECTION</code> data items, which can then be used to modify the data.

```cobol
SET ADDRESS OF foo TO ptr
MOVE "bar" TO foo
```



### References

Object references are declared like so, optionally with the class/interface they will reference:

```cobol
       01  obj                     USAGE OBJECT-REFERENCE "some-object".
```


They contain either a reference to an object or <code>NULL</code>.

They are initialised using by invoking a class constructor, and set using the <code>SET</code> statement.

```cobol
INVOKE SomeClass "new" RETURNING obj-ref
SET another-obj-ref TO obj-ref
```


<code>LINKAGE SECTION</code> data items are essentially references, being passed either the address of an argument (<code>BY REFERENCE</code>), the address of a copy of an argument (<code>BY CONTENT</code>) or the address itself, suitable for using with pointers (<code>BY VALUE</code>).


## Common Lisp


See [[Address_of_a_variable#Common Lisp]]


## D



```d
void main() {
    // Take the address of 'var' and placing it in a pointer:
    int var;
    int* ptr = &var;

    // Take the pointer to the first item of an array:
    int[10] data;
    auto p2 = data.ptr;


    // Depending on variable type, D will automatically pass either
    // by value or reference.
    // By value: structs, statically sized arrays, and other
    //           primitives (int, char, etc...);
    // By reference: classes;
    // By kind of reference: dynamically sized arrays, array slices.

    struct S {}
    class C {}

    void foo1(S s) {}        // By value.
    void foo2(C c) {}        // By reference.
    void foo3(int i) {}      // By value.
    void foo4(int[4] i) {}   // By value (unlike C).
    void foo6(int[] i) {}    // Just length-pointer struct by value.
    void foo5(T)(ref T t) {} // By reference regardless of what type
                             // T really is.
}
```



## Delphi


Delphi ( Object Pascal ) fully supports both typed and untyped pointers.

'''Examples of pointers'''

Simple ''untyped'' pointer variable:

Variable Declaration


```delphi
pMyPointer : Pointer ;>
```


Simple pointer to a ''predefined'' type:

Variable Declaration


```delphi
pIntPointer : ^Integer ;
```


A pointer to a ''Record''.  This is the equivalent to a ''Struct'' in C

Type Defintion


```delphi
MyRecord = Record
            FName : string[20];
            LName : string[20];
           end;
```


Variable Declaration


```delphi
pMyRecord : ^MyRecord ;
```


Note that when defining a pointer type, unless otherwise, you may refer to the pointed to type before defining it. For example the following is legal despite tFoo not being defined at the pointer definition:


```delphi
type
  pFoo = ^tFoo; { allowed despite tFoo not yet being defined }
  tFoo = record
           value1, value2: integer;
         end;
```


- Dereferencing a Pointer -

Pointers are dereferenced using the caret '^' symbol. Dereferencing will cause the compiler or RTL to reveal the data that the pointer points to:


```delphi
IntVar := pIntPointer^ ;
```


Dereference with a type cast of an ''untyped'' pointer. It is not legal syntax to simply dereference an untyped pointer, since there is nothing to designate its type. It must therefor be "Type Cast" when the dereference takes place as below.


```delphi
IntVar := integer(MyPointer^);
```


- Pushing a Pointer -

Many programmers are familiar with C's ability to increment pointers of any type by using the '''++''' or '''--''' operators. The equivalent in Delphi is to use either the Inc or Dec standard procedure:


```delphi
Inc(PtrVar);    //increment by one element
Inc(PtrVar, 4); //incremement by four elements
Dec(PtrVar);    //decrement by one element
```


C-style array indexing is also supported by default for PByte (a pointer to a byte), PAnsiChar (a pointer to a singlebyte character) and PWideChar (a pointer to a doublebyte character). For any other typed pointer, it can be enabled using a compiler directive:


```delphi
{$POINTERMATH ON}
PrevIntVar := MyIntPtr[-1];
Rec4 := MyRecPtr[4];
```



## E


E is a [[memory-safe]] [[object-graph]] language, and as such uses only object references; there are no non-reference values. Mutable storage always consists of an object; in particular, an assignable variable has, if necessary, an underlying "slot" object which can be retrieved.

 var x := 0
 def slot := &x # define "slot" to be x's slot

 x := 1         # direct assignment; value is now 1
 slot.put(2)    # via slot object; value is now 2

There are also mutable data structures:

 def flexList := [].diverge()
 flexList.push(1)
 flexList.push(2)
 flexList.snapshot() # returns [1, 2]

User-defined objects can have state by containing it:

 var open := true
 def door {
     to open()  { open := true }
     to close() { open := false }
     to walkThrough() { require(open) }
 }


## EchoLisp

No pointers in EchoLisp. '''Boxes ''' or vectors can be used  to perform call-by-reference operations.

```scheme

(define B (box 42))
    → B ;; box reference
(unbox B)
    → 42 ;; box contents

;; sets new value for box contents
(define ( change-by-ref abox avalue)
    (set-box! abox avalue) )

(change-by-ref B 666)
    → #[box 666]
(unbox B)
    → 666

```



## Forth


Forth is an untyped language, and treats pointers in much the same way that assembly language does.  In Forth, memory is addressable in characters, cells and double cells. Cells are the native word size of the host processor, and are usually either 16 or 32 bits in size. Double cells are twice that size. Address alignment requirements depend on the processor being used to host the Forth system.

To declare a variable that lives in memory:
  variable myvar   \ stores 1 cell
  fvariable myfvar \ stores 1 floating point number (often 8 bytes)
  2variable my2var \ stores 2 cells

This creates the word "myvar", which when executed leaves the address of a cell of memory on the stack. This address may be accessed with @ (fetch) or ! (store):
  1 myvar !  \ place the value 1 into myvar
  myvar @ .  \ prints the value stored in myvar
  1 myvar +! \ add 1 to the value stored in myvar

Other fetch and store operations:
  c@ c!  \ fetch/store a single character
  @  !   \ fetch/store one cell
  2@ 2!  \ fetch/store two cells
  f@ f!  \ fetch/store a floating point value

Forth also has "value", which is a construct similar to other languages' "reference":
  10 value myval  \ create a new word that returns the value 10
  myval .         \ prints 10
  20 to myval     \ Changes myval to return 20 instead of 10

Forth also has a concept analogous to function pointers - the "execution token". ETs are the address of a word's executable code. The ' character, pronounced "tick", returns the execution token of the next word in the input stream. "Execute" calls the XT on the stack. XT's are cells and can be stored and operated on as desired.
  ' myval         \ Leaves the XT of myval on the stack
  ' myval execute \ Equivalent to just typing "myval"

For pointer arithmetic, standard integer math operations may be used. There are some words that are specifically designed to enable portable pointer arithmetic:
  cell        \ Puts the number of bytes in one cell on the stack
  cell+       \ Adds one cell
  10 cells +  \ adds 10 cells
  aligned     \ rounds the address up to the next cell

An contiguous block of memory may be named with CREATE and allocated with ALLOT.
  create array 20 cells allot

As an example, here is some code to add together an array of cells, expanded and commented:
  : add-cells ( addr n -- sum )
     0 -rot      \ make stack into ( 0 addr n )
     bounds ?do  \ bounds converts an <address count> pair into a <high, low> range
        i @      \ using the current loop index as an address, fetch the cell
        +        \ Add it to the top number on the stack
     cell +loop  \ Advance the loop index by the size of one cell
  ;


## Fortran

Since Fortran90, the <tt>pointer</tt> attribute can be given:


```fortran
real, pointer :: pointertoreal
```


A so-declared pointer is in an undetermined state: a pointer should be nullified:


```fortran
nullify(pointertoreal)
```


But Fortran 95 allows to initialize pointers to NULL (unassociated):


```fortran
real, pointer :: apointer => NULL()
```


A pointer can be associated:


```fortran
real, target :: areal
  pointertoreal => areal
```


The target attribute is needed to say that the ''object'' (the "real" datum) can be referenced through a pointer (it seems it works more like an alias rather than a "true" pointer). The existence of an ''association'' can be tested with <tt>associated</tt> (Fortran 95):


```fortran
if ( associated(pointertoreal) ) !...
```


and the association can be nullified as before with <tt>nullify</tt>.

The data a "pointer" points to can be allocated as if the <tt>allocatable</tt> attribute were specified.


```fortran
integer, dimension(:), pointer :: array
  allocate(array(100))
```


That allocate an array of 100 integers; nullifying at this point would give memory leakage; the memory pointed to the array should be deallocated using the <tt>deallocate(array)</tt> code.

The function <tt>associated</tt> (Fortran 95) accepts also the optional argument <tt>target</tt>, to check if the pointer is associated to a particular target (working ''de facto'' like an alias)


```fortran
integer, target :: i
integer, pointer :: pi
!... ...
if ( associated(pi, target=i) ) !...
```


Associating pointer to an array could help accessing the data of the array in a different manner.


```fortran
real, dimension(20), target :: a
real, dimension(20,20), target :: b
real, dimension(:), pointer :: p

p => a(5:20)
! p(1) == a(5), p(2) == a(6) ...
p => b(10,1:20)
! p(1) == b(10,1), p(2) == b(10,2) ...
```


Which is different of course from having e.g.


```fortran
real, dimension(20) :: a
real, dimension(16) :: p

p = a(5:20)
```


In order to create arrays of pointers, the only way is to define a new type:


```fortran
type intpointer
  integer, pointer :: p
end type intpointer

!...
  type(intpointer), dimension(100)  :: parray
```


It declares an array of 100 <tt>intpointer</tt>, i.e. pointers to integer; the <tt>integer, dimension(:), pointer :: pt</tt> says just that pt is a pointer to an array of X integers.

Once a pointer is associated, it can be used as a normal variable/type.

There are not other possible operations on Fortran "pointers" (at least before Fortran 2003).


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Type Cat
  name As String
  age As Integer
End Type

Type CatInfoType As Sub (As Cat Ptr)

Sub printCatInfo(c As Cat Ptr)
  Print "Name "; c->name, "Age"; c-> age
  Print
End Sub

' create Cat object on heap and store a pointer to it
Dim c As Cat Ptr = New Cat

' set fields using the pointer and the "crow's foot" operator
c->name = "Fluffy"
c->age = 9

' print them out through a procedure pointer
Dim cit As CatInfoType = ProcPtr(printCatInfo)
cit(c)

Delete c
c = 0

Dim i As Integer = 3
' create an integer pointer variable and set it to the address of 'i'
Dim pi As Integer Ptr = @i

'change the variable through the pointer
*pi = 4

'print out the result
print "i ="; *pi

'create a reference to the variable i
Dim ByRef As Integer j = i

' set j (and hence i) to a new value
j = 5

' print them out
Print "i ="; i, "j ="; j
Sleep
```


```txt

Name Fluffy   Age 9

i = 4
i = 5         j = 5

```



## Go

Go has pointers but no pointer arithmetic.  In general it disallows many operations that are common in other languages but have proven to be common sources of programming errors.

Short of restrictions though, <tt>*</tt> and <tt>&</tt> are used much as they are in C.


```go
var p *int  // declare p to be a pointer to an int
i = &p      // assign i to be the int value pointed to by p
```


The zero value of a pointer is called nil.  Dereferencing a nil pointer, as in the example above, causes a run-time panic.

Some go types contain pointers in their internal representation and are called reference types.  For example, slices, maps, and channels are always reference types.


```go
var m map[string]int
```


declares m as object of map type, but does not allocate or create anything.  It's internal pointer is nil and attempts to store anything in the map will cause a run-time panic.  The following short declaration declares m the same as above, but it also initializes the internal representation.


```go
m := make(map[string]int)
```


Built-in function make allocates and constructs the internal representation and returns a map object ready to use.

Assignment to a reference type such as a slice, map, or channel does not do a deep copy.  It results in two references to the same underlying data.  For example


```go
b := []byte(“hello world”)
c := b
c[0] = 'H'
fmt.Println(string(b))
```

Output:

```txt

Hello world

```

Go's garbage collector is aware of references created by taking the address of an object.


```go
func three() *int {
    i := 3
    return &i // valid.  no worry, no crash.
}
```


Because of this, a common syntax for allocating a struct is to use & with a literal:


```go
type pt struct {
    x, y int
}

return &pt{22, 79} // allocates a pt object and returns a pointer to it.
```



## Haskell


In Haskell, all normal values are immutable, and for those values pointer and reference operations make no sense. Internally, many values are ''boxed'', that is, they contain an extra indirection which can be thought of as a pointer. But that is all handled transparently by the compiler.

However, Haskell supports imperative update of variables. To ensure that these side-effects are ordered, that has to happen inside a monad. So the predefined state monad ''ST'' makes it possible to allocate a ''reference'' (a simple mutable data structure with one field; not the same meaning as the "references" in other languages) with an initial value, read from it, and write to it:


```haskell

import Data.STRef

example :: ST s ()
example = do
  p <- newSTRef 1
  k <- readSTRef p
  writeSTRef p (k+1)

```


These are all the operations allowed on references. The type system guarantees that a reference can never leave the monad. The IO monad has similar operations ''newIORef'', ''readIORef'', ''writeIORef''. One can also embed the ST monad in the IO monad with 'stToIO'.

=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon do not have a pointer data type and cannot manipulate references.  Declarations are not needed for type enforcement and instances of data types are both [[Icon%2BUnicon/Intro#Self-Descriptive_Safe_Types|Self-Descriptive and Safe]].  Data types additionally fall into either [[Icon%2BUnicon/Intro#Mutable_and_Immutable_Types|Mutable and Immutable Types]].  Program semantics treat immutable types as call by value and mutable types such as lists and tables as call by reference.


## J

J's "boxes" are, in essence, pointers (except with pass-by-value semantics instead of pass-by-reference semantics, and J does not allow "pointer arithmetic" on boxes).

<code><</code> used monadically boxes its argument (much like C's <code>&</code> creates a pointer to a value).

<code>></code> used monadically unboxes its argument (much like C's <code>*</code> dereferences a pointer).

That said, names, in J, refer to their values, and names are generally used when references are needed.


## Java

In Java, all values are either primitives (<tt>int</tt>, <tt>char</tt>, <tt>byte</tt>, <tt>double</tt>, etc.) or they are ''references'' to an object. These references are kind of like pointers in C/C++, but you cannot perform arithmetic on them, nor can you directly dereference pointers or take the address of something. All objects are always accessed through references - you create one using <tt>new Something()</tt>, it returns a reference; and you access an object's fields and methods using the . operator, which takes a reference to the object as its left operand.

If the object that a reference is pointing to is mutable (i.e. it has public fields or it has methods that allows changing of its fields), then it is possible to modify that object's state through the reference; and someone else who has a reference to the same object will see that modification. (Note: this does not change the reference itself, just the object that it points to.) Let us consider a simple class of mutable object:


```java
 public class Foo { public int x = 0; }

 void somefunction() {
     Foo a; // this declares a reference to Foo object; if this is a class field, it is initialized to null
     a = new Foo(); // this assigns a to point to a new Foo object
     Foo b = a; // this declares another reference to point to the same object that "a" points to
     a.x = 5; // this modifies the "x" field of the object pointed to by "a"
     System.out.println(b.x); // this prints 5, because "b" points to the same object as "a"
 }
```


Java is call-by-value. When passing arguments, you are either passing primitives by value or references by value. There is no such thing as "passing an object" as objects are not values in the language. As noted above, if the object that the reference is pointing to is mutable, then it is possible to modify that object's state through the reference. Then if the calling function has a reference to the same object, they will see that modification through the reference. So if you want to reflect changes in an argument back to the caller, one thing that you can do is wrap the argument as a field in an object, then modify the object through the reference.


## Julia

Like Python, Julia's native variables and data types are accessed via references. Julia does have pointers, but they are mainly used for interfaces with other languages, especially C, that use pointers.

Julia's C interface pointer functions are considered "unsafe" because, like C pointers, they may cause data corruption if used incorrectly.

```julia
x = [1, 2, 3, 7]

parr = pointer(x)

xx = unsafe_load(parr, 4)

println(xx)  #  Prints 7

```




## Kotlin

Kotlin JVM has been designed to be 100% interopable with Java and consequently everything that has been said in the Java entry applies equally to it.

In the interests of having a common language, Kotlin JS and Kotlin Native follow the same principles. However, the last named also needs to interoperate with the native languages C and (in due course) Objective-C and so needs to be able to deal with pointers are well.

In general the Kotlin heap (automatic memory management) and the native heap (manual memory management) are kept separate from each other and communication takes place through special Kotlin classes which wrap native pointers or enable references to Kotlin objects or functions to be passed indirectly to native code. In particular:

1. Except where noted below, pointers to C types and arrays (including null pointers) are mapped to CPointer<T>? where T is the corresponding 'lvalue' type;

2. void* is mapped to COpaquePointer which is the supertype of all other pointer types;

3. const char* is mapped to the Kotlin String type;

4. Kotlin function pointers can be converted to C function pointers using the staticCFunction function;

5. References to Kotlin objects can be passed to and from C functions using the StableObjPtr class.

As noted above, pointer types also have an 'lvalue' representation (similar in concept to references in C++). For example, for int* the corresponding 'lvalue' type is IntVar and for double* it is DoubleVar.

These 'lvalue' types are Kotlin classes which wrap a native pointer and the size of the type it points to. They have a 'value' property which enables one to get or set that value and a 'ptr' property which returns the corresponding CPointer object. The latter has in turn a 'pointed' property which returns the lvalue type.

Pointers to different types can be (unsafely) cast to each other (using the reinterpret<T> function) or converted to actual addresses (using the toLong function) and array pointers can be indexed (using the [] operator). Pointers can also be incremented using the + operator or decremented (by adding a negative offset).

The following example may help to make some of this clear. Although the Kotlin types have been specified to help with this, in practice they would usually be omitted and inferred by the compiler.

```scala
// Kotlin Native v0.3

import kotlinx.cinterop.*

fun main(args: Array<String>) {
    // allocate space for an 'int' on the native heap and wrap a pointer to it in an IntVar object
    val intVar: IntVar = nativeHeap.alloc<IntVar>()
    intVar.value = 3                 // set its value
    println(intVar.value)            // print it
    println(intVar.ptr)              // corresponding CPointer object
    println(intVar.rawPtr)           // the actual address wrapped by the CPointer

    // change the value and print that
    intVar.value = 333
    println()
    println(intVar.value)
    println(intVar.ptr)              // same as before, of course

    // implicitly convert to an opaque pointer which is the supertype of all pointer types
    val op: COpaquePointer = intVar.ptr

    // cast opaque pointer to a pointer to ByteVar
    println()
    var bytePtr: CPointer<ByteVar> = op.reinterpret<ByteVar>()
    println(bytePtr.pointed.value)   // value of first byte i.e. 333 - 256 = 77 on Linux
    bytePtr = (bytePtr + 1)!!        // increment pointer
    println(bytePtr.pointed.value)   // value of second byte i.e. 1 on Linux
    println(bytePtr)                 // one byte more than before
    bytePtr = (bytePtr + (-1))!!     // decrement pointer
    println(bytePtr)                 // back to original value
    nativeHeap.free(intVar)          // free native memory

    // allocate space for an array of 3 'int's on the native heap
    println()
    var intArray: CPointer<IntVar> = nativeHeap.allocArray<IntVar>(3)
    for (i in 0..2) intArray[i] = i  // set them
    println(intArray[2])             // print the last element
    nativeHeap.free(intArray)        // free native memory
}
```


Sample output:

```txt

3
CPointer(raw=0xeb85b0)
0xeb85b0

333
CPointer(raw=0xeb85b0)

77
1
CPointer(raw=0xeb85b1)
CPointer(raw=0xeb85b0)

2

```



## M2000 Interpreter

There are three kind of pointers, and two kind of references. Pointers can change, but reference can't change. There are no arithmetic operations to pointers. We can operate for objects that points. References are never nil, but weak references maybe invalid. There is a special container (an object) named Buffer for handling memory blocks and we can get addresses from that and use them to call code (we can make memory blocks for code execution too). So using buffers for data and buffers for code we can use pointers as those in assembly. Also we can use external dll, and pass them addresses too.


### Normal Reference

Read Statement get a weak reference and make it a normal reference. Variable X can't get new reference. We can't get reference from an item in a container (array, inventory, stack)


```M2000 Interpreter

A=10
Module Beta {
      Read &X
      X++
}
Beta &A
Print A=11

```



### =Reference for Functions/lambdas=




```M2000 Interpreter

Module TestFuncRef {
      Function Alfa(x) {
            =x**2
      }
      Def Beta(x)=x*2
      Z=100
      K=Lambda Z (x)->x**3/Z
      Z=0
      Module TestFun {
            Read &Fun()
            Print Fun(10)*3
      }
      TestFun &Alfa()  ' 300
      TestFun &Beta() '60
      TestFun &K()  ' 30
      K=Lambda Z=50 (x)->x**3/Z
      TestFun &K()  ' 60

      M$=Lambda$ t$="9876543210" (x)->Mid$(t$,x+1,1)
      Module TestFun2 (&L$) {
            For i=4 to 7 : Print L$(i): Next i
      }

      TestFun2 &M$
      Module TestFun2 (&L$()) {
            For i=4 to 7 : Print L$(i): Next i
      }
      TestFun2 &M$()
}
TestFuncRef

```



### Weak Reference using Strings

Every time we use the string as weak reference, interpreter make resolve to reference. See point after $, A weak reference can be change any time. We can pass array items using Weak$() to get the right reference.

```M2000 Interpreter

A=10
Dim A(10)=1
Module Beta {
      Read X$
      X$.++
}
Beta &A
Print A=11
Beta Weak$(A(2))
Print A(2)=2

```



### Pointer of Com Object

We can declare objects (Com type) and we get a pointer, but that pointer leave until the end of module, which we create the pointer. We may declare properties for these objects, and also that properties use smart pointers, so the com object has only one pointer, which deleted at the exit of module (where created). Com objects are internal GUI objects. We use Method to call methods and return values. We can use With to read, set values, and to bound properties to names. In the example below we make Title$ as property "Title" of form Form1. We can use Nothing to delete objects (names can't get another object), it is optional, but has to do with the release of resource, and when happen. For forms it is better to use it as in the example below.

```M2000 Interpreter

Module ShowModalFormWithCLock {
      Declare Form1 Form
      With Form1, "Title" as Title$
      Thread {
            Title$=str$(now,"hh:mm:ss")
      } as K interval 500
      Method Form1, "Show", 1
      Declare Form1 Nothing
}
ShowModalFormWithCLock

```



### Pointer of Container

There are three containers, Arrays, Inventories and Stacks. Arrays have two interfaces, the value one and the pointer one. The value one has parenthesis in the name. Containers may have items other containers. Usually containers combined with functions and statements, but because  they are COM objects (but pointers inside work different from other Com objects), we can use statemends Method and With as for com objects.

```M2000 Interpreter

Module CheckArray {
      \\ This is a value type array
      Dim A(10)=1, B()
      B()=A()
      A(1)++
      Print A(1)-B(1)=1
      \\ Z is a pointer of A()
      Z=A()
      \\ On All items add one
      Z++
      Print Array(Z, 1)=3
      Print A(1)=3
      \\ B() get allways a copy
      B()=Z
      Z++
      Print B(1)=3, A(1)=4
      \\ We can make another pointer easy
      \\ M is a pointer and point where Z points
      M=Z
      \\ Print can print all items from a container (leave one column  in a row blank, if item is an object)
      Print A()
      Print B()
      Print M
      Print Z
}
CheckArray

```


### Pointers of Groups

Groups are value types, but we can make pointers for them. Assigning a Group to a Group we get a combination, the group at the left get all the members of the group at the right (except the situation we have define a Set function for Group, and handle the assignment as we wish, or not defined it but define a value function so interpreter treat it as read only and throw error).


```M2000 Interpreter

Module GroupPointers {
      Group Zeta {
            X=10, Y=20, A$=" ok"
            Dim A(10)=1
            Module ShowX10 {
                  Print .X*10, .A$
                  .Y++
            }
      }
      Print Zeta.A(3)
      L=Zeta ' a copy of Zeta to L
      M->Zeta   ' using weak reference inside
      N->(Zeta) ' using real pointer, to a copy of Zeta
      Link Zeta to P  ' P is a reference of Zeta
      List !
      \\ we get a list (not at that order):  Zeta[Group],L[Group], P[Group],  M*[Group], N*[Group], plus all members of Zeta,L, P
      \\ M and N have no operating members, Interpreter make them at demand (using the pointer, for temporary use)
      Modules ?
      \\ We get a list:  Zeta.ShowX10, L.ShowX10, P.ShowX10
      \\ Also we don't have operating modules for M and N, but at demand we get them (for temporary use)
      P.A(2)+=10
      Print P.A(2)=Zeta.A(2) ' True because its member is a reference
      Print L.A(2)=1 ' True
      Print M=>A(2)=Zeta.A(2) 'True
      Print N=>A(2)=Zeta.A(2) 'False
      Print N=>A(2)=L.A(2) ' True
      N->0& ' Null
      N->M ' pointers can point to another group.
      Print N=>A(2)=11
      N=>ShowX10
      Print Zeta.Y=21
}
GroupPointers

```


### =Reference of members of Groups=

We can pass reference from group members, if group has a name.

```M2000 Interpreter

Module CheckGroupRef {
      Group TestMe {
      Private:
            acc=1
      Public:
            Function GetAcc {=.acc}
             Group Y {
                  Property Name$ {Value}="TestMe"
                  Value {
                        link parent [x] to x
                        =100*x
                  }
                  Set {
                     read x
                     Link parent acc to acc
                     acc+=x
                  }
            }
             Property X { Value }=100
             Function GetValue(x) {
                  =.[X]*x
                  .[X]++
            }
      }

      Module GetIt (&Fun()) {
            Print Fun(30)
      }
      GetIt &TestMe.GetValue()  ' 3000
      Print TestMe.X=101
      Module ByRef (&What){
            Print What, What.Name$
            What=30
      }
      ByRef &TestMe.Y
      Print TestMe.GetAcc()=31
}
CheckGroupRef

```



## Lua

Lua does not have pointers but it is worth remembering that assigning a table to a new variable creates a reference to that table, not a copy of it.

```Lua
local table1 = {1,2,3}
local table2 = table1
table2[3] = 4
print(unpack(table1))
```

```txt
1       2       4
```


=={{header|Modula-3}}==
Modula-3 uses references; However, it does allow arithmetic and address operations (but only in unsafe modules).

Here is an example of a traced reference to an integer.

```modula3
TYPE IntRef = REF INTEGER;

VAR intref := NEW(IntRef);

intref^ := 10
```


The <tt>^</tt> character is the dereference operator, and is suffixed to the reference variable name.


### Traced and Untraced

There are two reference types in Modula-3, traced and untraced.  Traced reference types are garbage collected, whereas untraced reference types are not. Address arithmetic and address operations are allowed only on untraced reference types.  The built-in type <tt>ADDRESS</tt> is of type <tt>UNTRACED REF T</tt>.  Untraced references are only allowed in unsafe modules.


### REFANY

The built-in type <tt>REFANY</tt> is of type <tt>REF T</tt>, which is the "root" reference type, and can be a reference of any type.

```modula3
VAR any: REFANY;
any := NEW(REF INTEGER); (* Modula-3 knows that any is now REF INTEGER with a tag added by NEW. *)
```



### TYPECASE

To determine the type of a variable, you can use <tt>TYPECASE</tt>.

```modula3
PROCEDURE Sum(READONLY a: ARRAY OF REFANY): REAL =
  VAR sum := 0.0;
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      TYPECASE a[i] OF
      | NULL => (* skip *)
      | REF INTEGER (e) => sum := sum + FLOAT(e^);
      | REF REAL(e) => sum := sum + e^;
      ELSE (* skip *)
      END;
    END;
    RETURN sum;
  END Sum;
```



## Nim

There are two types of pointers in Nim. Safe, garbage-collected references and unsafe pointers.

Create a safe reference:

```nim
type Foo = ref object
  x, y: float

var f: Foo
new f
```


Accessing the reference:

```nim
echo f[]
```


When accessing values the dereference operator [] can be left out:

```nim
echo f[].x
f[].y = 12
echo f.y
f.x = 13.5
```


Create an (unsafe) pointer to an int variable:

```nim
var x = 3
var p = addr x
```


Access the integer variable through the pointer:

```nim
echo p[]
p[] = 42
```


Change the pointer to refer to another object:

```nim
var y = 12
p = addr y
```


Change the pointer to not point to any object:

```nim
p = nil
```



## OCaml


Like Java, Python, and most other languages, many complex data structures in OCaml are boxed, including records, arrays, objects, algebraic data types, etc. In other words, you always refer to them using a level of indirection. Records and objects can contain mutable fields, and elements of arrays are mutable. So when you pass a mutable record or array, you are actually passing a reference and the contents of the record or array can be modified by the called function.

If you just want a simple mutable "variable" (since variables in OCaml are immutable), you can allocate a ''reference'' (a simple mutable data structure with one field; not the same meaning as the "references" in other languages) with an initial value, read from it, and write to it:


```ocaml

let p = ref 1;; (* create a new "reference" data structure with initial value 1 *)
let k = !p;;    (* "dereference" the reference, returning the value inside *)
p := k + 1;;    (* set the value inside to a new value *)

```


(The OCaml web-site provides this [http://caml.inria.fr/resources/doc/guides/pointers.en.html page] on the subject.)


## Oforth


Oforth does not have pointers and all Oforth objets are implicitly references to objects.

The data stack only holds references to objects. When you use dup, swap, over, ... you work on those references : objects are not copied.

When an object is sent as a parameter to a function or method, its reference is sent. If the object is a mutable object, the function can modify this object.

There is no method to retrieve an object reference.


## ooRexx

In ooRexx, all values are ''references'' to an object. These references are kind of like pointers in C/C++, but you cannot perform arithmetic on them, nor can you directly dereference pointers or take the address of something. All objects are always accessed through references - you create one by sending a NEW message to a class object, which returns a reference; and you access an object's methods using the ~ operator, which takes a reference to the object as its left operand.

If the object that a reference is pointing to is mutable (i.e. it has methods that allows changing of its internal variables), then it is possible to modify that object's state through the reference; and someone else who has a reference to the same object will see that modification. (Note: this does not change the reference itself, just the object that it points to.) Let us consider a simple class of mutable object:


```oorexx

 ::class Foo
 ::method init
   expose x
   x = 0
 ::attribute x

 ::routine somefunction
     a = .Foo~new   -- assigns a to point to a new Foo object
     b = a          -- b and a now point to the same object
     a~x = 5        -- modifies the X variable inside the object pointer to by a
     say b~x        -- displays "5" because b points to the same object as a

```


ooRexx is call-by-value. When passing arguments, you are passing object references by value. There is no such thing as "passing an object" as objects are not values in the language. As noted above, if the object that the reference is pointing to is mutable, then it is possible to modify that object's state through the reference. Then if the calling function has a reference to the same object, they will see that modification through the reference. So if you want to reflect changes in an argument back to the caller, one thing that you can do is wrap the argument as an attribute of an object, then modify the object through the reference.



With the Use Arg instruction, a subroutine get access to the caller's argument object by reference:

```oorexx
a.='not set'
a.3=3
Say 'Before Call sub: a.3='a.3
Call sub a.
Say ' After Call sub: a.3='a.3
Call sub2 a.
Say ' After Call sub2: a.3='a.3
Exit

sub: Procedure
Use Arg a. -- this established access to the caller's object
a.3=27
Return

sub2: Procedure
Parse Arg a. -- this gets the value of the caller's object
a.3=9
Say 'in sub2: a.='a.
Say 'in sub2: a.3='a.3  -- this changes the local a.
Return
```

```txt
Before Call sub: a.3=3
 After Call sub: a.3=27
in sub2: a.=not set
in sub2: a.3=9
 After Call sub2: a.3=27
```



## PARI/GP

GP does not allow user functions with pointers, but some built-in functions like <code>issquare</code> have pointer arguments:

```parigp
n=1;
issquare(9,&n);
print(n); \\ prints 3
```


Pari program can use C pointers normally. In fact, all GEN variables are actually pointers to a space within the Pari stack (or, rarely, the Pari heap).


## Pascal

See [[Pointers_and_references#Delphi | Delphi]]


## Perl

Perl has "references" that roughly correspond with "smart pointers" of C-like languages. Due to reference-counting semantics, they can never point to something that does not exist. Any scalar container (which includes array elements and hash values, but not hash keys) can hold a reference to a data structure.


```perl
 # start with some var definitions
 my $scalar = 'aa';
 my @array = ('bb', 'cc');
 my %hash = ( dd => 'DD', ee => 'EE' );

 # make references
 my $scalarref = \$scalar;
 my $arrayref = \@array;
 my $hashref = \%hash;
```


Using a reference


```perl
 # accessing the value
 print $$scalarref;     # 'aa'
 print @$arrayref;      # 'bbcc'
 print $arrayref->[1];  # 'cc'
 print $hashref->{ee};  # 'EE'

 # changing the value
 $$scalarref = 'a new string'; # changes $scalar
 $arrayref->[0] = 'foo';       # changes the first value of @array
 $hashref->{'dd'} = 'bar';     # changes the value with key 'dd' in %hash
```


You may also create anonymous references:


```perl
 my $scalarref = \'a scalar';
 my $arrayref = ['an', 'array'];
 my $hashref = { firstkey => 'a', secondkey => 'hash' }
```



## Perl 6

In Perl 6 all non-native values are boxed and accessed via implicit references. (This is like Java or Python, but unlike C or Perl 5, which use explicit referencing and dereferencing.)  Variables are references to containers that can contain references to other values.  Basic binding (aliasing) of references to names is supported via the <tt>:=</tt> operator, while assignment to mutable containers implies a dereference from the name to the container, followed by copying of values rather than by duplicating pointers.  (Assignment of a bare object reference copies the reference as if it were a value, but the receiving container automatically dereferences as necessary, so to all appearances you are putting the object itself into the destination rather than its reference, and we just think the object can be in more than one place at the same time.)


```perl6
my $foo = 42;    # place a reference to 42 in $foo's item container
$foo++;          # deref $foo name, then increment the container's contents to 43
$foo.say;        # deref $foo name, then $foo's container, and call a method on 43.

$foo := 42;      # bind a direct ref to 42
$foo++;          # ERROR, cannot modify immutable value

my @bar = 1,2,3; # deref @bar name to array container, then set its values
@bar»++;         # deref @bar name to array container, then increment each value with a hyper
@bar.say;        # deref @bar name to array container, then call say on that, giving 2 3 4

@bar := (1,2,3); # bind name directly to a List
@bar»++;         # ERROR, parcels are not mutable
```

References to hashes and functions work more like arrays, insofar as a method call acts directly on the container, not on what the container contains.  That is, they don't do the extra dereference implied by calling a method on a scalar variable.

To the first approximation, Perl 6 programmers do not think about references much; since everything is a reference, and value semantics are emulated by assign and other mutating operators, the ubiquitous references are largely transparent to the Perl 6 programmer most of the time.


## Phix

Phix does not have pointers, other than for playing with raw allocated memory, typically for interfacing with another language pre-compiled into a dll/so, although for that builtins/cffi.e offers a more grown-up mechanism with seamless 32/64 bit portability. There is no pointer math beyond sizes in bytes.

```Phix
atom addr = allocate(8) -- (assumes 32 bit)
poke4(addr,{NULL,SOME_CONSTANT})
c_proc(xSome_External_Routine,{addr,addr+4})
?peek4s({addr,2})   -- prints {x,y}
free(addr)
```

There are in fact 5 variants of poke: poke1, poke2, poke4, poke8, and pokeN which allows the size to be dynamically specified.
Likewise there are 9 versions of peek: peek1s, peek2s, peek4s, peek8s, peek1u, peek2u, peek4u, peek8u, and peekNS, and again the
latter allows the size and signed flag to be dynamically specified. All variants can peek/poke a single byte/word/dword/qword or
a specified number of consecutive such from the specified address. Note that floating point values need to go via atom_to_float32
or float32_to_atom, where 32 could also be 64 or 80.

You can, of course use pointers via inline assembly (but only as a last resort/if you are mental enough):

```Phix
atom mypi
#ilASM{
        fldpi
    [32]
        lea edi,[mypi]
    [64]
        lea rdi,[mypi]
    []
        call :%pStoreFlt }
```

or

```Phix
string mystring = "mystring"
#ilASM{
    [32]
        mov esi,[mystring]
        lea esi,[ebx+esi*4]     -- byte[esi] is 'm'
    [64]
        mov rsi,[mystring]
        lea rsi,[rbx+rsi*4]     -- byte[rsi] is 'm'
    []
      }
```

Of course in a reference counted language like Phix, playing directly with the innards like that may have dire unexpected side effects.

Phix does not have references, however everything is passed by reference, with copy-on-write semantics. Unless the source and destination are the same, and it is local, so it cannot possibly be referenced elsewhere, in which case automatic pass-by-reference is used, which for Phix means skipping the reference counting. For example:

```Phix
sequence s
    s = myfunc(s)
```

It is a general optimisation, applied by the complier whenever and wherever it can, without the programmer having to do anything special.
Several of the builtins, for instance s = append(s,thing) have a similar optimisation, as long as s occurs on both the rhs and lhs, and less any need for it to be local (since the builtins are non-recursive leaf routines).

Phix takes the view that there should be as few as possible unintended side effects. If you see a line of code such as s = myfunc(...) then you have the full list of all [local] variables it will modify on the lhs, without needing to pick through the parameter list, though of course it might trample on a few external global or static variables. Personally I find that approach often helps me narrow down all the places where a bug might lurk much quicker, and that is certainly worth an occasional bit of extra typing on the lhs.

There is an extreme example in the interpreter. For performance, symtab names are left as a meaningless (to a human) ternary tree index, until an error occurs. At which point pEmit2.e/rebuild_callback() deliberately fudges the reference counting, so that it can replace all said indexes in the symtab with string names, in situ, before repairing the reference count and continuing. It ain't pretty but it works well.


## PHP

Actual Reference or Pointer "objects" don't exist in PHP, but you can simply tie any given variable or array value together with the "=&" operator.  Adding the "&" symbol before a function name and the times a function is called causes the functions return values to be returned by reference.  Adding the "&" synbol before any function parameter, and that parameter is passed by reference into the function.

As an additional note, the "global" keyword, simply references the variable from the $_GLOBALS superglobal array into a variable by the same name in the current scope.  This is an important distinction as other functions that you call can actually re-reference the $_GLOBALS value to a different variable, and your function which used the "global" keyword is no longer actually linked to anything in the $_GLOBALS array.


```php
<?php
/* Assignment of scalar variables */
$a = 1;
$b =& $a; // $b and $a are now linked together
$b = 2; //both $b and $a now equal 2
$c = $b;
$c = 7; //$c is not a reference; no change to $a or $b
unset($a); //won't unset $b, just $a.

/* Passing by Reference in and out of functions */
function &pass_out() {
    global $filestr;  //$exactly equivalent to: $filestr =& $_GLOBALS['filestr'];

    $filestr = get_file_contents("./bigfile.txt");
    return $_GLOBALS['filestr'];
}
function pass_in(&$in_filestr) {
    echo "File Content Length: ". strlen($in_filestr);

    /* Changing $in_filestr also changes the global $filestr and $tmp */
    $in_filestr .= "EDIT";
    echo "File Content Length is now longer: ". strlen($in_filestr);
}

$tmp = &pass_out(); // now $tmp and the global variable $filestr are linked
pass_in($tmp); // changes $tmp and prints the length

?>
```


In most cases, PHP "does the right thing" as pertaining to variables and not duplicating variables in memory that have not been edited.  Internally, as variables are copied from one to another, passed in and out of functions, etc, they are Not actually duplicated in memory, but rather referenced until one of these copies is changed (called "copy on write", see debug_zval_dump()).  So, in the above functions example, if we took out all the ampersands, it would all work perfectly and not duplicate the big string in memory up until the line where we concatenate and add "EDIT" to the end.

PHP Objects are Always passed by reference, so passing an object into and out of a function, will not make multiple cloned instances of that object.  However, arrays are Not objects, and thus, every time they are passed or copied, they need to be passed by reference if you mean for any changes on the child to be applied to the parent.


### See Also

* [http://php.net/manual/en/language.references.php php.net:References Explained]
* [http://php.net/manual/en/function.debug-zval-dump.php php.net:debug_zval_dump]


## PicoLisp

The internal PicoLisp machinery consists completely of pointers. Any data item
(except numbers) is a pointer that points to a cell, which in turn consists of
two pointers ("cons pair").

The pointers are not evident to the programmer. The development environment
presents them as high level structures (symbols or lists). However, the pointer
value (the address) can be obtained with the
'[http://software-lab.de/doc/refA.html#adr adr]' function.

"Dereferencing" a pointer is done with the
'[http://software-lab.de/doc/refC.html#car car]' or
'[http://software-lab.de/doc/refV.html#val val]' functions. They return the data
item at the memory location (CAR or VAL part of a cell). With
'[http://software-lab.de/doc/refS.html#set set]', a value can be stored in the
referred location.

There is no meaningful pointer arithmetics, except functions like
'[http://software-lab.de/doc/refC.html#cdr cdr]' or
'[http://software-lab.de/doc/refN.html#nth nth]', which advance the pointer to
the next (linked to) location(s).

```PicoLisp
: (setq L (1 a 2 b 3 c))         # Create a list of 6 items in 'L'
-> (1 a 2 b 3 c)

: (nth L 4)                      # Get a pointer to the 4th item
-> (b 3 c)

: (set (nth L 4) "Hello")        # Store "Hello" in that location
-> "Hello"

: L                              # Look at the modified list in 'L'
-> (1 a 2 "Hello" 3 c)
```



## PL/I

PL/I has a large set of tools for pointers and references:
* variable attributes: BASED, POINTER, but also AREA and OFFSET
* functions: ADDR
* control statements:  ALLOCATE, FREE
Simple examples:

```pli

dcl i fixed bin(31);
dcl p pointer;
dcl j fixed bin(31) based;
i=5;
p=addr(i);
p->j=p->j+1;   /an other way to say i=i+1 */
put skip edit(i)(F(5));   /* ->  6 */

/* second form */
dcl i fixed bin(31);
dcl j fixed bin(31) based(p);
i=5;
p=addr(i);
j=j+1;   /* an other way to say i=i+1 */
put skip edit(i)(F(5));   /* ->  6 */

/* cascading pointers */
dcl (p,q,s,t) pointer;
dcl (j,k) fixed bin(31) based;
dcl (i1,i2) fixed bin(31);
p=addr(i1); t=addr(i2), q=addr(p); s=addr(t);
q->p->j = s->t->k + 3;   /* to say i1=i2+3 */

```




## Pop11


Pop11 uses reference model, conceptually all Pop11 data consists
of references. Therefore, normally there is no need for explicit
references:

 vars vec1, vec2;
 ;;; Create a vector and assign (reference to) it to vec1
 consvector("a", "b", "c", 3) -> vec1;
 ;;; Copy (reference to) vector
 vec1 -> vec2;
 ;;; Print value of vec1
 vec1 =>
 ;;; Change first element of vec2
 "d" -> vec2(1);
 ;;; Print value of vec1 -- the value changes because vec1 and
 ;;; vec2 reference the same vector
 vec1 =>

However, if one needs extra indirection one can get it:

 vars ref1, ref2;
 ;;; Allocate a reference to number 42
 consref(42) -> ref1;
 ;;; Copy reference
 ref1 -> ref2;
 ;;; print value
 cont(ref2) =>
 ;;; Change referenced value
 17 -> cont(ref2);
 ;;; print value of first reference
 cont(ref1) =>

Standard Pop11 does not allow pointer arithmetics or address operator.
There is a low-level extension (Syspop11) which has C-like pointers
(with pointer arithmetics), but those does not count as "basic data
operation".


## PureBasic


### Pointers

A pointer's name is preceded by an '*' and are variables of type integer that can hold an address (either 32-bit or 64-bit depending on the OS compiled for), and do not differ from other integer variables in this respect.  Pointers can be declared as pointing to structured memory and allow that memory to be de-referenced to allow easier manipulation of its contents.
The following code creates a pointer to an integer variable:

```PureBasic
Define varA.i = 5, varB.i = 0, *myInteger.Integer

*myInteger = @varA      ;set pointer to address of an integer variable
varB = *myInteger\i + 3 ;set variable to the 3 + value of dereferenced pointer, i.e varB = 8
```

Change pointer to refer to another integer variable:

```PureBasic
Define varC.i = 12
*myInteger = @varC
```

Change pointer to not point to anything:

```PureBasic
*myInteger = #Null ;or anything evaluating to zero
```

Get a pointer to the first element of an array, or any desired element:

```PureBasic
Dim myArray(10)
*myInteger = myArray()
;Or alternatively:
*myInteger = @myArray(0)
;any specific element
*myInteger = @myArray(4) ;element 4
```

PureBasic does not provide pointer arithmetic but it does allow integer math operations on its pointers.  This makes it easy to implement a variety of pointer arithmetic with the help of the compiler function <tt>SizeOf()</tt>.

```PureBasic
*myInteger + 3 * SizeOf(Integer) ;pointer now points to myArray(3)
*myInteger - 2 * SizeOf(Integer) ;pointer now points to myArray(1)
```

In a similar manner the compiler function <tt>OffsetOf()</tt> may also be used to obtain the memory offset for an sub-element of a structure if needed:

```PureBasic
Structure employee
  id.i
  name.s
  jobs.s[20] ;array of job descriptions
EndStructure

Dim employees.employee(10) ;an array of employee's

;set a string pointer to the 6th job of the 4th employee
*myString.String = @employees(3) + OffsetOf(employee\jobs) + 5 * SizeOf(String)
```


### Addresses of variables or procedure

Addresses to variables and functions are obtained with the '@' operator.

```PureBasic
*pointer = @varA
*pointer = @myFunction()
```


### Addresses of labels

To find the address of a label, you put a question mark (?) in front of the label name.

```PureBasic
; Getting the address of a lable in the code
text$="'Lab' is at address "+Str(?lab)
MessageRequester("Info",text$)

; Using lables to calculate size
text$="Size of the datasetion is "+Str(?lab2-?lab)+" bytes."
MessageRequester("Info",text$)

; Using above to copy specific datas
Define individes=(?lab2-?lab1)/SizeOf(Integer)
Dim Stuff(individes-1)  ; As PureBasic uses 0-based arrays
CopyMemory(?lab1,@Stuff(),?lab2-?lab1)

DataSection
  lab:
  Data.s "Foo", "Fuu"
  lab1:
  Data.i  3,1,4,5,9,2,1,6
  lab2:
EndDataSection
```



## Python


Python does not have pointers and all Python names (variables) are implicitly references to objects.  Python is a late-binding dynamic language in which "variables" are untyped bindings to objects.  (Thus Pythonistas prefer the term '''name''' instead of "variable" and the term '''bind''' in lieu of "assign").


```python
 # Bind a literal string object to a name:
 a = "foo"
 # Bind an empty list to another name:
 b = []
 # Classes are "factories" for creating new objects: invoke class name as a function:
 class Foo(object):
     pass
 c = Foo()
 # Again, but with optional initialization:
 class Bar(object):
     def __init__(self, initializer = None)
         # "initializer is an arbitrary identifier, and "None" is an arbitrary default value
         if initializer is not None:
            self.value = initializer
 d = Bar(10)
 print d.value
 # Test if two names are references to the same object:
 if a is b: pass
 # Alternatively:
 if id(a) == id(b): pass
 # Re-bind a previous used name to a function:
 def a(fmt, *args):
     if fmt is None:
         fmt = "%s"
      print fmt % (args)
 # Append reference to a list:
 b.append(a)
 # Unbind a reference:
 del(a)
 # Call (anymous function object) from inside a list
 b[0]("foo")  # Note that the function object we original bound to the name "a" continues to exist
              # even if its name is unbound or rebound to some other object.
```


[Note: in some ways this task is meaningless for Python given the nature of its "variable" binding semantics].


## Racket


As in many other functional languages, Racket doesn't have pointers to its own values.  Instead, Racket uses "boxes" that are similar to "ref" types in *ML:

```racket

#lang racket

(define (inc! b) (set-box! b (add1 (unbox b))))

(define b (box 0))
(inc! b)
(inc! b)
(inc! b)
(unbox b) ; => 3

```


In addition, Racket has a representation of traditional C pointers as part of its FFI, but this is intended only for dealing with foreign code.


## REXX

Classic REXX doesn't pass values by references, instead, it passes by value.   In general, Classic REXX doesn't have pointers.   However, there are some REXXes that have support libraries that make use of pointer arguments to provide services via functions.

While it is possible to chase chains, the internal structure of REXX variables (and other values) are not defined as part of the REXX language.


## SAS


```sas
/* Using ADDR to get memory address, and PEEKC / POKE. There is also PEEK for numeric values. */
data _null_;
length a b c $4;
adr_a=addr(a);
adr_b=addr(b);
adr_c=addr(c);
a="ABCD";
b="EFGH";
c="IJKL";
b=peekc(adr_a,1);
call poke(b,adr_c,1);
put a b c;
run;
```



## Scala

Scala uses call by value exclusively, except that the value is either a primitive or a pointer to an object. If your object contains mutable fields, then there is very little substantive difference between this and call by reference.

Since you are always passing pointers to objects not the objects themselves, you don't have the problem of having to repeatedly copy a giant object.

Incidentally, Scala's call by name is implemented using call by value, with the value being a (pointer to a) function object that returns the result of the expression

## Sidef

A simple example of passing a variable-reference to a function:

```ruby
func assign2ref(ref, value) {
    *ref = value;
}

var x = 10;
assign2ref(\x, 20);
say x;      # x is now 20
```



## Standard ML


Like OCaml, Standard ML doesn't have pointers but have a "ref" mutable data structure which is the basis of mutability in Standard ML:


```sml

val p = ref 1; (* create a new "reference" data structure with initial value 1 *)
val k = !p;    (* "dereference" the reference, returning the value inside *)
p := k + 1;    (* set the value inside to a new value *)

```



## Tcl

Tcl does not have pointers, however, if required, a similar level of indirection can be had by storing a variable name in another variable, e.g.:

```tcl
set var 3
set pointer var; # assign name "var" not value 3
set pointer;     # returns "var"
set $pointer;    # returns 3
set $pointer 42; # variable var now has value 42
```

In practice it's safer and more convenient to use array keys or the <tt>upvar</tt> command, e.g.:

```tcl
set arr(var) 3
set pointer var
set arr($pointer);         # returns 3
set arr($pointer) 42;      # arr(var) now has value 42

set var 3
set pointer var
upvar 0 $pointer varAlias; # varAlias is now the same variable as var
set varAlias 42;           # var now has value 42
```

This second technique is most commonly used between stack levels (i.e., with the first argument to <tt>upvar</tt> being 1) so that a local variable of one procedure can be manipulated in another utility procedure that it calls.


## Toka

Toka is an untyped language, and treats pointers in a manner similar to that of assembly language. In Toka, memory can be addressed in characters and cells. Cells are the native word size of the host processor, and are generally either 32 or 64 bits in size. Address alignment is dependent on the host processor.

To declare a variable that lives in memory:

 variable myvar   #! stores 1 cell

This creates the word "myvar", which when executed leaves the address of a cell of memory on the stack. This address may be accessed with @ (fetch) or ! (store):

 1 myvar !  #! place the value 1 into myvar
 myvar @ .  #! prints the value stored in myvar
 1 myvar +! #! add 1 to the value stored in myvar

Other fetch and store operations:
 c@ c!  \ fetch/store a single character
 @  !   \ fetch/store one cell

Toka also has an optional "value", which is a construct similar to other languages' "reference":

 needs values
 10 value myval  #! create a new word that returns the value 10
 myval .         #! prints 10
 20 to myval     #! Changes myval to return 20 instead of 10

Toka has a concept similar to that of function pointers. The word ` returns the address of a quote which can be invoked or passed around.

 ` myval         #! Leaves the address of myval on the stack
 ` myval invoke  #! Equivalent to just typing "myval"

For pointer arithmetic, standard integer math operations may be used. There are some words that are specifically designed to enable portable pointer arithmetic:

 cell-size   #! Puts the number of bytes in one cell on the stack
 cell+       #! Adds one cell
 10 cells +  #! Adds 10 cells
 char-size   #! Puts the number of bytes in one character on the stack
 char+       #! Adds one char
 10 chars +  #! Adds 10 chars


## XPL0


```XPL0
\Paraphrasing the C example:
\This creates a pointer to an integer variable:
int Var, Ptr, V;
Ptr:= @Var;

\Access the integer variable through the pointer:
Var:= 3;
V:= Ptr(0);     \set V to the value of Var, i.e. 3
Ptr(0):= 42;    \set Var to 42

\Change the pointer to refer to another integer variable:
int OtherVar;
Ptr:= @OtherVar;

\Change the pointer to not point to anything:
Ptr:= 0;        \or any integer expression that evaluates to 0

\Set the pointer to the first item of an array:
int Array(10);
Ptr:= Array;
\Or alternatively:
Ptr:= @Array(0);

\Move the pointer to another item in the array:
def IntSize = 4;        \number of bytes in an integer
Ptr:= Ptr + 3*IntSize;  \pointer now points to Array(3)
Ptr:= Ptr - 2*IntSize;  \pointer now points to Array(1)

\Access an item in the array using the pointer:
V:= Ptr(3);     \get third item after Array(1), i.e. Array(4)
V:= Ptr(-1);    \get item immediately preceding Array(1), i.e. Array(0)
]
```



## VBA

Formally VBA does not have pointers or references. Objects are created with the "New" keyword, and assignments require the keyword "Set". Getting the memory address of a pointer is not documented in VBA.

```vb
Dim samplevariable as New Object
Dim anothervariable as Object
Set anothervariable = sameplevariable
```


## zkl

Pointers don't exist and neither do conventional references but mutable containers can act like references: List, Ref (strong reference), GarbageMan.WeakRef (weak reference) and others.

```zkl
fcn f(r){r.inc()} r:= Ref(1); f(r); r.value; //-->2
```

A Ref can hold any value. inc is a special case convenience for numbers.

```zkl
fcn f(lst){lst.append(5)} f(L()); //-->L(5)
```

Weak refs can hold anything the garbage collect can see. You can only look at the value, not change it. If it should hold a Ref or List, you can change those. The contents of a Weak Ref will vaporize sometime after the contents are no longer visible.


