+++
title = "Memory allocation"
description = ""
date = 2019-08-20T14:37:43Z
aliases = []
[extra]
id = 4248
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}

;Task:
Show how to explicitly allocate and deallocate blocks of memory in your language. 

Show access to different types of memory (i.e., [[heap]], [[system stack|stack]], shared, foreign) if applicable.





## 360 Assembly


```360 Assembly
                                                    
* Request to Get Storage Managed by "GETMAIN" Supervisor Call (SVC 4) 
       LA    1,PLIST       Point Reg 1 to GETMAIN/FREEMAIN Parm List 
       SVC   4             Issue GETMAIN SVC 
       LTR   15,15         Register 15 = 0? 
       BZ    GOTSTG         Yes: Got Storage 
*      [...]                 No: Handle GETMAIN Failure 
GOTSTG L     2,STG@        Load Reg (any Reg) with Addr of Aquired Stg 
*      [...]               Continue 
* Request to Free Storage Managed by "FREEMAIN" Supervisor Call (SVC 5) 
       LA    1,PLIST       Point Reg 1 to GETMAIN/FREEMAIN Parm List 
       SVC   5             Issue FREEMAIN SVC 
       LTR   15,15         Register 15 = 0? 
       BZ    STGFRE         Yes: Storage Freed 
*      [...]                 No: Handle FREEMAIN Failure 
STGFRE EQU   *             Storage Freed 
*      [...]               Continue
*
STG@   DS    A             Address of Stg Area (Aquired or to be Freed) 
PLIST  EQU   *             10-Byte GETMAIN/FREEMAIN Parameter List 
       DC    A(256)        Number of Bytes; Max=16777208 ((2**24)-8) 
       DC    A(STG@)       Pointer to Address of Storage Area
       DC    X'0000'       (Unconditional Request; Subpool 0) 

```



## Ada


### Stack

[[Stack]] in [[Ada]] is allocated by declaration of an object in some scope of a block or else a subprogram:

```ada
declare
   X : Integer; -- Allocated on the stack
begin
   ...
end; -- X is freed
```


### Heap

[[Heap]] is allocated with the allocator '''new''' on the context where a pool-unspecific pointer is expected:

```ada
declare
   type Integer_Ptr is access Integer;
   Ptr : Integer_Ptr := new Integer; -- Allocated in the heap
begin
   ...
end; -- Memory is freed because Integer_Ptr is finalized
```

The memory allocated by '''new''' is freed when:
* the type of the pointer leaves the scope;
* the memory pool is finalized
* an instance of Ada.Unchecked_Deallocation is explicitly called on the pointer

```ada
declare
   type Integer_Ptr is access Integer;
   procedure Free is new Ada.Unchecked_Deallocation (Integer, Integer_Ptr)
   Ptr : Integer_Ptr := new Integer; -- Allocated in the heap
begin
   Free (Ptr); -- Explicit deallocation
   ...
end;
```


### User pool

The allocator '''new''' also allocates memory in the user-defined storage pool when the pointer bound to the pool.

### External memory

An object can be specified as allocated at the specific memory location, see [[Machine Address|machine address]].


### Implicit allocation

Elaboration of compilation units may result in allocation of the objects declared in these units. For example:

```ada
package P is
   X : Integer; -- Allocated in the result the package elaboration
end P;
```

The memory required by the object may be allocated statically or dynamically depending on the time of elaboration and its context. Objects declared in the library level packages are equivalent to what in some languages is called ''static'' object.


## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}
Given:

```algol68
MODE MYSTRUCT = STRUCT(INT i, j, k, REAL r, COMPL c);
```


### Stack


```algol68>REF MYSTRUCT l = LOC MYSTRUCT;</lang


### Heap


```algol68>REF MYSTRUCT h = HEAP MYSTRUCT;</lang


### User pool


```algol68
[666]MYSTRUCT pool; 
INT new pool := LWB pool-1;
REF MYSTRUCT p = pool[new pool +:=1];
```


### External memory

Without extensions it is not possible to access external memory.  However most implementations have such an extension!

### Implicit allocation


```algol68>MYSTRUCT i;</lang



## ALGOL W

Algol W has garbage collected dynamic allocation for record structures.

```algolw
begin
    % define a record structure - instances must be created dynamically %
    record Element ( integer atomicNumber; string(16) name );
    reference(Element) X;
    % allocate and initialise memory for X - heap storage is the only option %
    X := Element( 1, "Hydrogen" );
    % allocate new memory for X, the original could now be garbage collected %
    X := Element( 2, "Helium" )
    % the memory allocated will now be garbage collected - there is no explicit de-allocation %
end.
```



## AutoHotkey


```AutoHotkey
VarSetCapacity(Var, 10240000)  ; allocate 10 megabytes
VarSetCapacity(Var, 0)  ; free it
```



## Axe

Axe does not provide runtime support for a heap, so memory must be allocated statically.

```axe
Buff(100)→Str1
.Str1 points to a 100-byte memory region allocated at compile time
```


The optional second parameter to Buff() allows you to specify the byte to be filled with (default is zero).


## BBC BASIC


### Heap


```bbcbasic
      size% = 12345
      DIM mem% size%-1
      PRINT ; size% " bytes of heap allocated at " ; mem%
```

Memory allocated from the heap is only freed on program termination or CLEAR.

### Stack


```bbcbasic
      size% = 12345
      PROCstack(size%)
      END
      
      DEF PROCstack(s%)
      LOCAL mem%
      DIM mem% LOCAL s%-1
      PRINT ; s% " bytes of stack allocated at " ; mem%
      ENDPROC
```

Memory allocated from the stack is freed on exit from the FN or PROC.


## Bracmat

As a rule, memory allocation and deallocation is done implicitly.
 
If Bracmat is linked as a library to a C or C++ program there may exist situations where explicitly allocating and deallocating memory is necessary, for example if a call-back C-function expects a pointer to a block of data. Another application of explicitly allocated memory is for storing data that may contain null bytes.

The Bracmat functions <code>alc$</code> and <code>fre$</code> call the C-functions <code>malloc()</code> and <code>free()</code>, respectively. Writing and reading to and from allocated memory is done with the poke and peek functions <code>pok$</code> and <code>pee$</code>. These funtions write and read in chunks of 1 (default), 2 or 4 bytes. No need to say that all these low-level functions easily can create havoc and should be disabled in serious applications that don't need them. (There are compiler preprocessor macros to do that.)

```bracmat
( alc$2000:?p           {allocate 2000 bytes}
& pok$(!p,123456789,4)  { poke a large value as a 4 byte integer } 
& pok$(!p+4,0,4)        { poke zeros in the next 4 bytes } 
& out$(pee$(!p,1))      { peek the first byte }
& out$(pee$(!p+2,2))    { peek the short int located at the third and fourth byte }
& out$(pee$(!p,4))      { peek the first four bytes }
& out$(pee$(!p+6,2))    { peek the two bytes from the zeroed-out range }
& out$(pee$(!p+1000,2)) { peek some uninitialized data }
& fre$!p                { free the memory }
&);
```

{{out}}

```txt
21
1883
123456789
0
0
```



## C

The functions <tt>malloc</tt>, <tt>calloc</tt> and <tt>realloc</tt> take memory from the heap. This memory ''should'' be released with <tt>free</tt> and it's suitable for sharing memory among threads.


```c>#include <stdlib.h


/* size of "members", in bytes */
#define SIZEOF_MEMB (sizeof(int))
#define NMEMB 100

int main()
{
  int *ints = malloc(SIZEOF_MEMB*NMEMB);
  /* realloc can be used to increase or decrease an already
     allocated memory (same as malloc if ints is NULL) */
  ints = realloc(ints, sizeof(int)*(NMEMB+1));
  /* calloc set the memory to 0s */
  int *int2 = calloc(NMEMB, SIZEOF_MEMB);
  /* all use the same free */
  free(ints); free(int2);
  return 0;
}
```


Variables declared inside a block (a function or inside a function) take room from the stack and survive until the "block" is in execution (and their scope is local).


```c
int func()
{
  int ints[NMEMB]; /* it resembles malloc ... */
  int *int2;       /* here the only thing allocated on the stack is a pointer */
  char intstack[SIZEOF_MEMB*NMEMB]; /* to show resemblance to malloc */
  int2 = (int *)intstack;           /* but this is educative, do not do so unless... */

  {
    const char *pointers_to_char[NMEMB];
    /* use pointers_to_char */
    pointers_to_char[0] = "educative";
  } /* outside the block, the variable "disappears" */

  /* here we can use ints, int2, intstack vars, which are not seen elsewhere of course */

  return 0;
}
```


{{works with|gcc}}

The libc provided by [[gcc]] (and present on other "systems" too) has the <tt>alloca</tt> function which allows to ask for memory on the stack explicitly; the memory is deallocated when the function that asked for the memory ends (it is, in practice, the same behaviour for automatic variables). The usage is the same as for functions like <tt>malloc</tt>


```c>#include <alloca.h

int *funcA()
{
  int *ints = alloca(SIZEOF_MEMB*NMEMB);
  ints[0] = 0;                                  /* use it */
  return ints; /* BUT THIS IS WRONG! It is not like malloc: the memory
                  does not "survive"! */
}
```


Variables declared outside any block and function or inside a function but prepended with the attribute <tt>static</tt> live as long as the program lives and the memory for them is statically given (e.g. through a .bss block).


```c
/* this is global */
int integers[NMEMB]; /* should be initialized with 0s */

int funcB()
{
  static int ints[NMEMB]; /* this is "static", i.e. the memory "survive" even
                             when the function exits, but the symbol's scope is local */ 
  return integers[0] + ints[0];
}

void funcC(int a)
{
  integers[0] = a;
}
```


=={{header|C sharp|C#}}==
C# is a managed language, so memory allocation is usually not done manually.  However, in unsafe code it is possible to declare and operate on pointers.

```csharp
using System;
using System.Runtime.InteropServices;

public unsafe class Program
{
    public static unsafe void HeapMemory()
    {
        const int HEAP_ZERO_MEMORY = 0x00000008;
        const int size = 1000;
        int ph = GetProcessHeap();
        void* pointer = HeapAlloc(ph, HEAP_ZERO_MEMORY, size);
        if (pointer == null)
            throw new OutOfMemoryException();
        Console.WriteLine(HeapSize(ph, 0, pointer));
        HeapFree(ph, 0, pointer);
    }

    public static unsafe void StackMemory()
    {
        byte* buffer = stackalloc byte[1000];
        // buffer is automatically discarded when the method returns
    }
    public static void Main(string[] args)
    {
        HeapMemory();
        StackMemory();
    }
    [DllImport("kernel32")]
    static extern void* HeapAlloc(int hHeap, int flags, int size);
    [DllImport("kernel32")]
    static extern bool HeapFree(int hHeap, int flags, void* block);
    [DllImport("kernel32")]
    static extern int GetProcessHeap();
    [DllImport("kernel32")]
    static extern int HeapSize(int hHeap, int flags, void* block);

}
```



## C++

While the C allocation functions are also available in C++, their use is discouraged. Instead, C++ provides <code>new</code> and <code>delete</code> for memory allocation and deallocation. Those function don't just allocate memory, but also initialize objects. Also, deallocation is coupled with destruction.

```cpp>#include <string


int main()
{
  int* p;

  p = new int;    // allocate a single int, uninitialized
  delete p;       // deallocate it

  p = new int(2); // allocate a single int, initialized with 2
  delete p;       // deallocate it

  std::string* p2;

  p2 = new std::string; // allocate a single string, default-initialized
  delete p2;            // deallocate it

  p = new int[10]; // allocate an array of 10 ints, uninitialized
  delete[] p;      // deallocation of arrays must use delete[]

  p2 = new std::string[10]; // allocate an array of 10 strings, default-initialized
  delete[] p2;              // deallocate it
}
```

Note that memory allocated with C allocation functions (<code>malloc</code>, <code>calloc</code>, <code>realloc</code>) must always be deallocated with <code>free</code>, memory allocated with non-array <code>new</code> must always be deallocated with <code>delete</code>, and memory allocated with array <code>new</code> must always deallocated with <code>delete[]</code>. Memory allocated with new also cannot be resized with <code>realloc</code>.

Note that use of the array form is seldom a good idea; in most cases, using a standard container (esp. <code>std::vector</code>) is a better idea, because it manages the memory for you, it allows you to define an initial value to set in the array (new[] always default-initializes), and like malloc, but unlike array new, it allows resizing (and unlike realloc, it correctly handles construction/destruction when resizing).

Besides the new expressions shown above, pure memory allocation/deallocation without object initialization/destruction can also be done through <code>operator new</code>:

```cpp
int main()
{
  void* memory = operator new(20); // allocate 20 bytes of memory
  operator delete(memory);         // deallocate it
}
```


There's also a placement form of new, which allows to construct objects at an arbitrary adress (provided it is correctly aligned, and there's enough memory):

```cpp>#include <new


int main()
{
  union
  {
    int alignment_dummy; // make sure the block is correctly aligned for ints
    char data[2*sizeof(int)]; // enough space for 10 ints
  };
  int* p = new(&data) int(3); // construct an int at the beginning of data
  new(p+1) int(5); // construct another int directly following
}
```

Indeed, code like <code>int* p = new int(3);</code> is roughly (but not exactly) equivalent to the following sequence:

```cpp
void* memory_for_p = operator new(sizeof(int));
int* p = new(memory_for_p) int(3);
```


Normally, new throws an exception if the allocation fails. there's a non-throwing variant which returns a null pointer instead:

```cpp>#include <new


int* p = new(std::nothrow) int(3);
```

Note that the nothrow variant does <em>not</em> prevent any exceptions to be thrown from the constructor of an object created with new. It only prevents exceptions due to memory allocation failure.

It is also possible to implement user-defined variations of operator new. One possibility is to define class-based operator new/operator delete:

```cpp>#include <cstddef

#include <cstdlib>
#include <new>

class MyClass
{
public:
  void* operator new(std::size_t size)
  {
    void* p = std::malloc(size);
    if (!p) throw std::bad_alloc();
    return p;
  }
  void operator delete(void* p)
  {
    free(p);
  }
};

int main()
{
  MyClass* p = new MyClass; // uses class specific operator new
  delete p;                 // uses class specific operator delete

  int* p2 = new int; // uses default operator new
  delete p2;         // uses default operator delete
}
```


Another possibility is to define new arguments for placement new syntax, e.g.

```cpp
class arena { /* ... */ };

void* operator new(std::size_t size, arena& a)
{
  return arena.alloc(size);
}

void operator delete(void* p, arena& a)
{
  arena.dealloc(p);
}

arena whatever(/* ... */);

int* p = new(whatever) int(3); // uses operator new from above to allocate from the arena whatever
```

Note that there is ''no'' placement delete syntax; the placement operator delete is invoked by the compiler only in case the constructor of the newed object throws. Therefore for placement newed object deletion the two steps must be done explicitly:

```cpp
class MyClass { /*...*/ };

int main()
{
  MyClass* p = new(whatever) MyClass; // allocate memory for myclass from arena and construct a MyClass object there
  // ...
  p->~MyClass(); // explicitly destruct *p
  operator delete(p, whatever); // explicitly deallocate the memory
}
```



## COBOL

In some implementations, programs with the <code>INITIAL</code> clause will have data in the <code>WORKING-STORAGE SECTION</code> stored on the stack. However, the COBOL standard does not specify where the data in a program should be stored.

Manual memory allocation is primarily done using <code>ALLOCATE</code> and <code>FREE</code>. They are used with data items with a <code>BASED</code> clause, which indicates that the data will be allocated at runtime. A <code>BASED</code> data item cannot be used before it has been allocated or after it has been freed. Example usage:

```cobol
       PROGRAM-ID. memory-allocation.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  based-data              PIC X(20) VALUE "Hello, World!"
                                   BASED.

       PROCEDURE DIVISION.
           *> INITIALIZED sets the data item to the VALUE.
           ALLOCATE based-data INITIALIZED
           DISPLAY based-data
           FREE based-data

           GOBACK
           .
```


{{out}}

```txt
Hello, World!
```



## Common Lisp

Objects are typically automatically allocated and deallocated in Common Lisp.  However, the <code>[http://www.lispworks.com/documentation/HyperSpec/Body/d_dynami.htm dynamic-extent]</code> declaration can be used to inform the compiler that the values a variable assumes can be stack-allocated.

{{works with|LispWorks}}

This behavior can be observed with the (not good for actual use) code:


```lisp
(defun show-allocation ()
  (let ((a (cons 1 2))
        (b (cons 1 2)))
    (declare (dynamic-extent b))
    (list a b)))
```



```lisp
(show-allocation)
```

produces 

```txt

((1 . 2) #<unknown object, header / pointer: 7FFFFFFC /21B3BF09 21B3BF09>)

```



## D


```d
// D is a system language so its memory management is refined.
// D supports thread-local memory on default, global memory, memory
// allocated on the stack, the C heap, or the D heap managed by a
// garbage collector, both manually and automatically.

// This program looks scary because its purpose is to show all the
// variety. But lot of this stuff is only for special situations
// (like alloca), and it's not necessary in most user code.

enum int nInts = 10; // Compile-time constant.

// This is thread-local:
int[nInts] data1;

// This is global:
__gshared int[nInts] data2;

void main() {
    // Static memory, it's thread-local but its name is usable
    // only locally:
    static int[nInts] data3;

    // Static memory, it's global but its name is usable only locally:
    __gshared static int[nInts] data4;

    // ----------------------
    // D supports the functions that manage memory of the C heap:
    import core.stdc.stdlib: malloc, calloc, realloc, free, alloca;

    // Allocates space for some integers on the heap,
    // the memory is not initialized:
    auto ptr1 = cast(int*)malloc(nInts * int.sizeof);
    if (ptr1 == null)
        return;

    // Increases the space for one more integer, the new space
    // is not initialized, but the old space is not modified:
    ptr1 = cast(int*)realloc(ptr1, (nInts + 1) * int.sizeof);
    if (ptr1 == null)
        return;

    // calloc allocates on the heap and zeros the memory:
    auto ptr2 = cast(int*)calloc(nInts, int.sizeof);
    if (ptr2 == null)
        return;

    // You can create a slice from a pointer:
    auto slice1 = ptr2[0 .. nInts];

    // Frees the memory:
    free(ptr2);
    free(ptr1);

    // ----------------------
    import core.stdc.stdio: puts;

    static struct Test {
        ~this() { puts("Test destructor"); }
    }

    // Memory allocated on the stack:
    Test[2] array1;

    {
        // More memory allocated on the stack:
        Test[2] array2;
        // Here array2 is removed from the stack,
        // and all array2 destructors get called.
    }
    puts("Block end.");

    // alloca is supported in D. It's similar to malloc but the
    // memory is allocated on the stack:
    int* ptr3 = cast(int*)alloca(nInts * int.sizeof);

    // You can create a slice from the pointer:
    auto slice2 = ptr3[0 .. nInts];

    // Do not free the memory allocated with alloca:
    // free(ptr3);

    // ----------------------
    // Allocates a dynamic array on the D heap managed by
    // the D garbage collector:
    auto array3 = new int[nInts];

    // Try to reserve capacity for a dynamic array on the D heap:
    int[] array4;
    array4.reserve(nInts);
    assert(array4.capacity >= nInts);
    assert(array4.length == 0);

    // Appends one integer to the dynamic array:
    array4 ~= 100;

    // Assume that it is safe to append to this array. Appends made
    // to this array after calling this function may append in place,
    // even if the array was a slice of a larger array to begin with:
    array4.assumeSafeAppend;
    array4 ~= 200;
    array4 ~= 300;
    assert(array4.length == 3);
    // See here for more info:
    // http://dlang.org/d-array-article.html


    // Allocates a struct and a class on the D GC heap:
    static class Foo { int x; }
    Test* t = new Test; // This destructor will not be called.
    Foo f1 = new Foo; // f1 is a class reference.

    // Optional. Destroys the given object and puts it in
    // an invalid state:
    f1.destroy;

    import std.typecons: scoped;

    // Allocates a class on the stack, unsafe:
    auto f3 = scoped!Foo();

    // ----------------------
    import core.memory: GC;

    // Allocates an aligned block from the GC, initialized to zero.
    // Plus it doesn't scan through this block on collect.
    auto ptr4 = cast(int*)GC.calloc(nInts * int.sizeof,
                                    GC.BlkAttr.NO_SCAN);

    // No need to test for this, because GC.calloc usually
    // throws OutOfMemoryError if it can't allocate.
    // if (ptr4 == null)
    //    exit(1);

    GC.free(ptr4); // This is optional.
}
```

{{out}}

```txt
Test destructor
Test destructor
Block end.
Test destructor
Test destructor

```



## E

E is a memory-safe language and does not generally work with explicit deallocation. As in Python and Java, you can create arrays of specific data types which will, by any decent implementation, be compactly represented.


```e>? <elib:tables.makeFlexList
.fromType(<type:java.lang.Byte>, 128)
# value: [].diverge()
```

The above creates an array with an initial capacity of 128 bytes (1 kilobit) of storage (though it does not have any elements). (The Java type name is left-over from E's Java-scripting history and will eventually be deprecated in favor of a more appropriate name.) The array will be deallocated when there are no references to it.


## Erlang

Erlang has memory management. To manually allocate memory in the code, spawn a process of a minimal size. The memory is taken from the heap. Gives a minimum heap size in words.

```txt

Pid = erlang:spawn_opt( Fun, [{min_heap_size, 1024}] ).

```

The memory is returned when the process dies/is killed.

```txt

erlang:exit( Pid, kill ).

```



## Factor

Factor has a garbage collector, so most of the time you don't need to exlicitly allocate storage. But it is sometimes necessary (f.e. when communicating with FFI code).

To just allocate some bytes, <code>malloc</code> is used. This memory has to be <code>free</code>d again of course.

```factor
2000 malloc  (...do stuff..)  free
```


To increase safety and reduce memory leaks, there are specialized words available to help you manage your memory. If you use <code>&free</code> together with <code>with-destructors</code> your memory gets freed even in the presence of exceptions.

```factor
STRUCT: foo { a int } { b foo* } ;

[
    foo malloc-struct &free ! gets freed at end of the current with-destructors scope
    ! do stuff
] with-destructors
```

Memory allocated with any of these malloc variants resides in the (non-garbage-collected) heap.


## Forth

Forth has two main bulk memory areas, each with their own word sets and semantics for allocation and deallocation.


### Dictionary

All Forth implementations have a stack-like memory space called the ''dictionary''. It is used both for code definitions and data structures.

```forth
unused .  \ memory available for use in dictionary
here .    \ current dictionary memory pointer
: mem, ( addr len -- ) here over allot swap move ;
: s, ( str len -- ) here over char+ allot place align ;   \ built-in on some forths
: ,"  [char] " parse s, ;
variable num
create array  60 cells allot
create struct  0 , 10 ,  char A c,  ," string"
unused .
here .
```


Dictionary space is meant for static code definitions and supporting data structures, so it is not as easy to deallocate from it. For ad-hoc allocations without intervening definitions, you may give a negative value to ALLOT to reclaim the space.  You may also lay down a named MARKER to reclaim the space used by all subsequent definitions.

```forth
marker foo
: temp  ... ;
create dummy 300 allot
-150 allot      \ trim the size of dummy by 150 bytes
foo    \ removes foo, temp, and dummy from the list of definitions
```



### Heap

Most Forth implementations also give access to a larger random-access memory heap.

```forth
4096 allocate throw  ( addr )
dup 4096 erase
( addr ) free throw
```



## Fortran


```fortran
program allocation_test
    implicit none
    real, dimension(:), allocatable :: vector
    real, dimension(:, :), allocatable :: matrix
    real, pointer :: ptr
    integer, parameter :: n = 100 ! Size to allocate

    allocate(vector(n))      ! Allocate a vector
    allocate(matrix(n, n))   ! Allocate a matrix
    allocate(ptr)            ! Allocate a pointer

    deallocate(vector)       ! Deallocate a vector
    deallocate(matrix)       ! Deallocate a matrix
    deallocate(ptr)          ! Deallocate a pointer
end program allocation_test
```



## Go

All memory in Go is transparently managed by the runtime and the language specification does not even contain the words stack or heap.  Behind the scenes it has a single shared heap and a stack for each goroutine.  Stacks for goroutines are initially 4K, but grow dyanamically as needed.  Function parameters and variables declared within a function typically live on the stack, but the runtime will freely move them to the heap as needed.  For example, in

```go
func inc(n int) {
    x := n + 1
    println(x)
}
```

Parameter n and variable x will exist on the stack.

```go
func inc(n int) *int {
    x := n + 1
    return &x
}
```

In the above, however, storage for x will be allocated on the heap because this storage is still referenced after inc returns.

In general, taking the address of an object allocates it on the heap.  A conseqence is that given

```go
type s struct{a, b int}
```

the following two expressions are equivalent.

```go
&s{}
```


```go
new(s)
```

Yes, new allocates on the heap.

A similar allocating function is make, used for slices, maps, channels.  These types reference allocated memory as part of their implementation but do not directly expose this allocated memory.  Make both allocates memory and initializes the internal representation of the object so that it can be used.

Examples,

```go
make([]int, 3)
make(map[int]int)
make(chan int)
```



## Haskell


You usually only need to do low-level memory management in Haskell when interfacing with code written in other languages (particularly C).  At its most basic level, Haskell provides malloc()/free()-like operations in the IO monad.


```Haskell
import Foreign

bytealloc :: IO ()
bytealloc = do
  a0 <- mallocBytes 100 -- Allocate 100 bytes
  free a0 -- Free them again
  allocaBytes 100 $ \a -> -- Allocate 100 bytes; automatically
                          -- freed when closure finishes
    poke (a::Ptr Word32) 0
```


Slightly more sophisticated functions are available for automatically determining the amount of memory necessary to store a value of a specific type.  The type is determined by type inference (in this example I use explicit manual type annotations, because poke is polymorphic).


```Haskell
import Foreign

typedalloc :: IO ()
typedalloc = do
  w <- malloc
  poke w (100 :: Word32)
  free w
  alloca $ \a -> poke a (100 :: Word32)
```


By the typing rules of Haskell, w must have the type 'Ptr Word32' (pointer to 32-bit word), which is how malloc knows how much memory to allocate.

=={{header|Icon}} and {{header|Unicon}}==

Icon and Unicon provide fully automatic memory allocation.  Memory is allocated when each
structure is created and reclaimed after it is no longer referenced.  For example:

```unicon

    t := table() # The table's memory is allocated
                 #... do things with t
    t := &null   # The table's memory can be reclaimed

```

For structures whose only reference is held in a local variable, that reference is removed
when the local context is exited (i.e. when procedures return) and the storage is then
available for reclaiming.

The actual reclamation of unreferenced structures takes place when garbage collection occurs.


## J


Example of explicit [http://www.jsoftware.com/help/user/memory_management.htm memory allocation]:


```J
   require 'dll'
   mema 1000
57139856
```


Here, 57139856 is the result of mema -- it refers to 1000 bytes of memory.

To free it:


```J>memf 57139856</lang



## Java

You don't get much control over memory in Java, but here's what you can do:

```java
//All of these objects will be deallocated automatically once the program leaves
//their scope and there are no more pointers to the objects
Object foo = new Object(); //Allocate an Object and a reference to it
int[] fooArray = new int[size]; //Allocate all spaces in an array and a reference to it
int x = 0; //Allocate an integer and set its value to 0
```

There is no real destructor in Java as there is in C++, but there is the <tt>finalize</tt> method. From the [http://java.sun.com/javase/6/docs/api/java/lang/Object.html#finalize() Java 6 JavaDocs]:

''The general contract of finalize is that it is invoked if and when the JavaTM virtual machine has determined that there is no longer any means by which this object can be accessed by any thread that has not yet died, except as a result of an action taken by the finalization of some other object or class which is ready to be finalized. The finalize method may take any action, including making this object available again to other threads; the usual purpose of finalize, however, is to perform cleanup actions before the object is irrevocably discarded. For example, the finalize method for an object that represents an input/output connection might perform explicit I/O transactions to break the connection before the object is permanently discarded.''

```java
public class Blah{
   //...other methods/data members...
   protected void finalize() throws Throwable{
      //Finalization code here
   }
   //...other methods/data members...
}
```


Note, though, that there is '''''no guarantee''''' that the <tt>finalize</tt> method will ever be called, as this trivial program demonstrates:

```java
public class NoFinalize {
    public static final void main(String[] params) {
        NoFinalize nf = new NoFinalize();
    }
    public NoFinalize() {
        System.out.println("created");
    }
    @Override
    protected void finalize() {
        System.out.println("finalized");
    }
}
```


When run using Sun's JVM implementation, the above simply outputs "created". Therefore, you cannot rely on <tt>finalize</tt> for cleanup.


## Julia

Julia has memory management. Objects are freed from memory automatically. Because arrays such as vectors and matrices, unlike lists, can have fixed size allocations in memory, these can be allocated implicitly with a call to a function returning a vector, or explicitly by assigning the memory to a variable:

```julia

matrix = Array{Float64,2}(100,100)
matrix[31,42] = pi

```



## Kotlin

In the version of Kotlin which targets the JVM, the latter takes care of memory allocation when objects are created together with the automatic deallocation of heap objects which there are no longer used via its garbage collector.

Consequently, manual intervention in the allocation or deallocation of objects is not possible though, as in Java (and subject to the problems mentioned in the entry therefor), it is possible to override the finalize() method to provide custom clean-up preceding garbage collection.

Variables of primitive types (Byte, Short, Int, Long, Float, Double, Char and Boolean) hold their values directly and variables of other types contain a reference to where the corresponding object is allocated on the heap.

All types (including primitive types) are either non-nullable (no suffix) or nullable (use a suffix of '?'). Only the latter can be assigned a value of 'null'. Values of nullable primitive types are 'boxed' i.e. stored as heap objects and variables of those types therefore contain a reference to the heap object rather than the value itself.

In addition, Kotlin has a Nothing type which has no instances and is a sub-type of every other type. There is also a nullable Nothing? type whose only value is 'null' and so, technically, this is the type of 'null' itself.

Some examples may help to make all this clear. In the interests of clarity, types have been specified for all variables though, in practice, this would be unnecessary in those cases where the variable's type can be inferred from the value assigned to it when it is declared. 'val' variables are read-only but 'var' variables are read/write.

```scala
// version 1.1.2

class MyClass(val myInt: Int) {
    // in theory this method should be called automatically prior to GC
    protected fun finalize() {
        println("MyClass being finalized...")
    }
}

fun myFun() {
    val mc: MyClass = MyClass(2)   // new non-nullable MyClass object allocated on the heap
    println(mc.myInt)
    var mc2: MyClass? = MyClass(3) // new nullable MyClass object allocated on the heap
    println(mc2?.myInt)
    mc2 = null                     // allowed as mc2 is nullable
    println(mc2?.myInt)
    // 'mc' and 'mc2' both become eligible for garbage collection here as no longer used
}

fun main(args: Array<String>) {
    myFun()
    Thread.sleep(3000)  // allow time for GC to execute
    val i: Int  = 4     // new non-nullable Int allocated on stack
    println(i)
    var j: Int? = 5     // new nullable Int allocated on heap
    println(j)
    j = null            // allowed as 'j' is nullable
    println(j)
    // 'j' becomes eligible for garbage collection here as no longer used
}
```

When this is run, notice that finalize() is not called - at least on my machine (running UBuntu 14.04, Oracle JDK 8): 
{{out}}

```txt

2
3
null
4
5
null

```



## Lingo

Lingo does not allow direct memory allocation and has no direct access to memory types like heap, stack etc. But indirectly the ByteArray data type can be used to allocate memory that then later can be filled with custom data:

```lingo
-- Create a ByteArray of 100 Kb (pre-filled with 0 bytes)
ba = byteArray(102400)

-- Lingo uses garbage-collection, so allocated memory is released when no more references exist. 
-- For the above variable ba, this can be achieved by calling: 
ba = VOID
```



## M2000 Interpreter

Buffer is an object which hold a block of memory in heap. There are two types, the default and the Code type. In code type we can execute code, but at execution time we can't write to that block. So to get results from machine code we have to use a default type buffer (for data). Buffers used to read/write to binary files too.

[http://www.rosettacode.org/wiki/Machine_code#M2000_Interpreter See example for Machine Code]

If we use a wrong offset, buffer return error and locked (can't be used until erased)

Variable which hold the buffer is a pointer to buffer. The buffer erased when no more pointer points to it. We can use pointer as return value, or pushing to stack of values. We can use buffers as members of groups. A copy of group just copy the pointer. We can use buffers as closures in lambda functions, and a copy of lambda which have a closure of a buffer make a copy of pointer too (so two or more lambda function may use same memory allocation to read/write)

Buffers used with a type as a meter of bytes for each element, here in the example we say we have bytes. We can use Byte (1 bytes), Integer (2 bytes), Long (4 bytes), Double (8 bytes), or a structure (we can define structures, with pointer to strings also, as BSTR type). So if we use Integer as meter then Mem1(1)-Mem1(0) return 2 (2 bytes).
Data Byte, Integer, Long are unsigned.

We can redim buffers, but we can't change the meter. Structures can have unions to use different same data.

We can use Eval$(Mem1) to get a copy of a buffer in a string.
Statement Return used to place in many offsets data in one statement.

```M2000 Interpreter

Module Checkit {
      Buffer Clear Mem1 as Byte*12345
      Print Len(Mem1)
      Hex Mem1(0) ' print in Hex address of first element
      Print Mem1(Len(Mem1)-1)-Mem1(0)+1=12345
      Buffer Mem1 as Byte*20000  ' redim block
      Print Mem1(Len(Mem1)-1)-Mem1(0)+1=20000
      Try {
            Print Mem1(20000)  ' it is an error
      }
      Print Error$  ' return message: Buffer Locked, wrong use of pointer
}
Checkit 

```



## Maple

Maple is a garbage-collected language, so there is no direct control over the lifetime of objects, once allocated.  When an object is allocated, it remains in memory until it is no longer reachable; then it is garbage-collected.

You can allocate a large block of memory by creating an Array

```Maple
a := Array( 1 .. 10^6, datatype = integer[1] ):

```

Now you can use the storage in the Array assigned to <tt>a</tt> as you see fit.  To ensure that <tt>a</tt> is garbage collected at the earliest opportunity, unassign the name <tt>a</tt>:

```Maple
unassign(  a ):
```

or

```Maple
a := 'a':
```



## Mathematica

Mathematica allocates memory and garbage collects it.

If desired, memory can be optimized/reclaimed somewhat by calling Share[] or ClearSystemCache[].

=={{header|MATLAB}} / {{header|Octave}}==

Matlab and Octave allocate memory when a new variable or a local variable is generated. Arrays are automatically extended as needed. However, extending the array might require to re-allocate the whole array. Therefore, pre-allocating memory can provide a significant performance improvement. 


```MATLAB
 
   A = zeros(1000); 	% allocates memory for a 1000x1000 double precision matrix. 
   clear A;		% deallocates memory 

   b = zeros(1,100000);	% pre-allocate memory to improve performance
   for k=1:100000,
	b(k) = 5*k*k-3*k+2;
   end 
 
```



## Maxima


```maxima
/* Maxima allocates memory dynamically and uses a garbage collector.
Here is how to check available memory */

room();
  3221/3221   72.3%       2 CONS RATIO COMPLEX STRUCTURE
   272/307    61.6%         FIXNUM SHORT-FLOAT CHARACTER RANDOM-STATE READTABLE SPICE
   226/404    90.8%         SYMBOL STREAM
     1/2      37.2%         PACKAGE
   127/373    44.9%         ARRAY HASH-TABLE VECTOR BIT-VECTOR PATHNAME CCLOSURE CLOSURE
   370/370    49.1%       1 STRING
   325/440     8.2%         CFUN BIGNUM LONG-FLOAT
    31/115    98.9%         SFUN GFUN VFUN AFUN CFDATA
  1188/1447                 contiguous (478 blocks)
       11532                hole
       5242    5.0%         relocatable
      4573 pages for cells
     22535 total pages
     97138 pages available
     11399 pages in heap but not gc'd + pages needed for gc marking
    131072 maximum pages
```



## Nim

Usually in Nim we have automated memory allocation and garbage collection, but we can still manually get a block of memory:

```nim
# Allocate thread local heap memory
var a = alloc(1000)
dealloc(a)

# Allocate memory block on shared heap
var b = allocShared(1000)
deallocShared(b)

# Allocate and Dellocate a single int on the thread local heap
var p = create(int, sizeof(int)) # allocate memory
# create zeroes memory; createU does not.
echo p[]                         # 0
p[] = 123                        # assign a value
echo p[]                         # 123
discard resize(p, 0)             # deallocate it
# p is now invalid. Let's set it to nil
p = nil                          # set pointer to nil
echo isNil(p)                    # true

```



## Objeck

In Objeck space for local variables is allocated when a method/function is called and deallocated when a method/function exits.  Objects and arrays are allocated from the heap and their memory is managed by the memory manager.  The memory manager attempts to collect memory when an allocation threshold is met or exceeded.  The memory manger uses a mark and sweep [[Garbage collection|garbage collection]] algorithm.

```objeck
foo := Object->New(); // allocates an object on the heap
foo_array := Int->New[size]; // allocates an integer array on the heap
x := 0; // allocates an integer on the stack
```



## Oforth


All memory in Oforth is managed by the garbage collector. Objects are all allocated on the heap using new on a class.

Stack can't be accessed and objects are never allocated on the stack : the stack holds only references to objects stored into local variables or parameters.

The data stack holds only references to objects allocated on the heap.

You can allocate a block of memory (on the heap) by creating a MemBuffer object, which is an array of bytes.


## PARI/GP

All accessible memory in GP is on PARI's heap.  Its size can be changed:

```parigp
allocatemem(100<<20)
```

to allocate 100 MB.

Memory allocation in PARI is somewhat subtle. See section 4.2.5 (also skim 4.3 and 5.4) in the User's Guide to the PARI Library.


## Pascal



###  Stack 


Variables declared in procedures and functions are allocated on the stack.

Their scope is local to the respective procedure/function and their memory is freed with the end of the procedure/function.


###  Heap 


Dynamicly created objects (dynamic arrays, class instantiations, ...) are allocated on the heap.

Their creation and destruction is done explicitly.

```pascal
type
  TByteArray = array of byte;
var
  A: TByteArray;
begin
  setLength(A,1000);
...
  setLength(A,0);
end;
```



```pascal
type
  Tcl = class
    dummy: longint;
  end;
var
  c1: cl;
begin
  c1:=Tcl.create;
...
  c1.destroy;
end;
```



## Perl

In general, memory allocation and de-allocation isn't something you can or should be worrying about much in Perl.
Perl manages its own heap quite well, and it is exceedingly rare that anything goes wrong. As long as the OS has memory to give,
a perl process can use as much as it needs.

Memory allocated to lexicals, i.e. <tt>my()</tt>, variables cannot be reclaimed or reused even if they go out of scope
(it is reserved in case the variables come back into scope). You can 'hint' that memory allocated to global variables
can be reused (within your program) by using <tt>undef</tt> and <tt>delete</tt>, but you really have little control over
when/if that happens.


## Perl 6

Like Perl 5, Perl 6 is intended to run largely stackless, so all allocations are really on the heap, including activation records.  Allocations are managed automatically.  It is easy enough to allocate a memory buffer of a particular size however, if you really need it:

```perl6
my $buffer = Buf.new(0 xx 1024);
```


## Phix

In normal use, memory management is fully automatic in Phix. However you may need to explicitly allocate memory when interfacing to C etc.
By default (for compatibility with legacy code) cleanup must be performed manually, but there is an optional flag on both the memory
allocation routines (allocate and allocate_string) to automate that for you, or you could even roll your own via delete_routine().

```Phix
atom addr = allocate(512)                  -- limit is 1,610,612,728 bytes on 32-bit systems
...
free(addr)
atom addr2 = allocate(512,1)               -- automatically freed when addr2 drops out of scope or re-assigned
atom addr3 = allocate_string("a string",1) -- automatically freed when addr3 drops out of scope or re-assigned
```

Behind the scenes, the Phix stack is actually managed as a linked list of virtual stack blocks allocated on the heap, and as such it
would be utterly pointless and quite probably extremely tricky to mess with.


## PicoLisp

Only the heap can be explicitly controlled in PicoLisp. Usually this is not necessary, as it happens automatically.

But if desired, memory can be pre-allocated by calling [http://software-lab.de/doc/refG.html#gc gc] with a single numeric argument, specifying the desired number of megabytes that should be reserved. When that argument is zero, the heap size is decreased (as far as possible).


## PL/I

PL/I can allocate memory in three ways

* On the stack: this happens with variables declared as automatic in procedures and begin blocks. The variables are automatically freed when at exit of the procedure or block. If an ''init'' clause is specified, the variable will also be (re-)initialized upon entry to the procedure or block. If no ''init'' clause is specified, the variable will most likely contain garbage. An example:


```pli

 mainproc: proc options(main) reorder;

 subproc: proc;
 dcl subvar char init ('X');

 put skip data(subvar);
 subvar = 'Q';
 end subproc;

 call subproc();
 call subproc();
 end mainproc;
```


Result:

```txt
subvar='X';
subvar='X';
```


* On the heap: if a variable is declared with the ''ctl'' (or in full, ''controlled'') attribute, it will be allocated from the heap and multiple generations of the same variable can exist, although only the last one allocated can be accessed directly. An example:


```pli

 mainproc: proc options(main) reorder;
 dcl ctlvar char ctl;

 alloc ctlvar;
 ctlvar = 'A';
 alloc ctlvar;
 ctlvar = 'B';
 alloc ctlvar;
 ctlvar = 'C';

 put skip data(ctlvar);
 free ctlvar;
 put skip data(ctlvar);
 free ctlvar;
 put skip data(ctlvar);
 free ctlvar;
 end mainproc;
```


Result:

```txt
ctlvar='C';
ctlvar='B';
ctlvar='A';
```


* On the heap: if a variable is declared with the ''based'' attribute, it will be allocated from the heap and multiple generations of the same variable can exist. This type of variable is often used in linked lists. An example:


```pli

 mainproc: proc options(main) reorder;
 dcl list_ptr ptr            init (sysnull());
 dcl list_top ptr            init (sysnull());
 dcl list_end ptr            init (addr(list_top));
 dcl i        fixed bin (31);
                                                   
 dcl 1 list based(list_ptr),
       2 list_nxt  ptr            init (sysnull()),
       2 list_data fixed bin (31) init (i);

 /*
 * Allocate the list
 */
 do i = 1 to 4;
   alloc list;
   list_end -> list_nxt = list_ptr;
   list_end             = list_ptr;
 end;

 /*
 * Print the list
 */
  do list_ptr = list_top repeat list_nxt
                        while(list_ptr ^= sysnull());
   put skip list(list_data);
 end;
 end mainprog;
```

 
Result:

```txt
1
2
3
4
```



## PureBasic


```PureBasic
*buffer=AllocateMemory(20)

*newBuffer = ReAllocateMemory(*buffer, 2000) ;increase size of buffer
;*buffer value is still valid if newBuffer wasn't able to be reallocated
If *newBuffer <> 0
  *buffer = *newBuffer : *newBuffer = 0
EndIf 

FreeMemory(*buffer)


size=20
; allocate an image for use with image functions
CreateImage(1,size,size)
FreeImage(1)
```


Memory for strings is handled automatically from a separate memory heap.  The automatic handling of string memory includes garbage collection and the freeing of string memory.


## Python

Python has the [http://docs.python.org/library/array.html array module]:
This module defines an object type which can compactly represent an array of basic values: characters, integers, floating point numbers.  Arrays are sequence types and behave very much like lists, except that the type of objects stored in them is constrained.  The type is specified at object creation time by using a <em>type code</em>, which is a single character.  The following type codes are defined:

<table class="docutils" border="1">
<tr><th class="head">Type code</th>
<th class="head">C Type</th>
<th class="head">Python Type</th>
<th class="head">Minimum size in bytes</th>
</tr>

<tr><td><tt class="docutils literal"><span class="pre">'c'</span></tt></td>
<td>char</td>

<td>character</td>
<td>1</td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">'b'</span></tt></td>
<td>signed char</td>
<td>int</td>
<td>1</td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">'B'</span></tt></td>
<td>unsigned char</td>

<td>int</td>
<td>1</td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">'u'</span></tt></td>
<td>Py_UNICODE</td>
<td>Unicode character</td>
<td>2</td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">'h'</span></tt></td>
<td>signed short</td>

<td>int</td>
<td>2</td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">'H'</span></tt></td>
<td>unsigned short</td>
<td>int</td>
<td>2</td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">'i'</span></tt></td>
<td>signed int</td>

<td>int</td>
<td>2</td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">'I'</span></tt></td>
<td>unsigned int</td>
<td>long</td>
<td>2</td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">'l'</span></tt></td>
<td>signed long</td>

<td>int</td>
<td>4</td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">'L'</span></tt></td>
<td>unsigned long</td>
<td>long</td>
<td>4</td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">'f'</span></tt></td>
<td>float</td>

<td>float</td>
<td>4</td>
</tr>
<tr><td><tt class="docutils literal"><span class="pre">'d'</span></tt></td>
<td>double</td>
<td>float</td>
<td>8</td>
</tr>

</table>
<p>The actual representation of values is determined by the machine architecture
(strictly speaking, by the C implementation).  The actual size can be accessed
through the <tt>itemsize</tt> attribute.  The values stored  for <tt class="docutils literal"><span class="pre">'L'</span></tt> and

<tt class="docutils literal"><span class="pre">'I'</span></tt> items will be represented as Python long integers when retrieved,
because Python’s plain integer type cannot represent the full range of C’s
unsigned (long) integers.</p>

'''Example'''

```python>>>
 from array import array
>>> argslist = [('l', []), ('c', 'hello world'), ('u', u'hello \u2641'),
	('l', [1, 2, 3, 4, 5]), ('d', [1.0, 2.0, 3.14])]
>>> for typecode, initializer in argslist:
	a = array(typecode, initializer)
	print a
	del a

	
array('l')
array('c', 'hello world')
array('u', u'hello \u2641')
array('l', [1, 2, 3, 4, 5])
array('d', [1.0, 2.0, 3.1400000000000001])
>>>
```



## R


```rsplus
x=numeric(10)  # allocate a numeric vector of size 10 to x
rm(x)  # remove x

x=vector("list",10) #allocate a list of length 10
x=vector("numeric",10) #same as x=numeric(10), space allocated to list vector above now freed
rm(x)  # remove x
```



## Racket

Racket doesn't allow direct memory allocation, although it supports some things

```Racket
#lang racket
(collect-garbage) ; This function forces a garbage collection

(current-memory-use) ;Gives an estimate on the memory use based on the last garbage collection

(custodian-require-memory <limit-custodian>
                          <amount>
                          <stop-custodian>) ; Registers a check on required memory for the <limit-custodian>
                                            ; If amount of bytes can't be reached, <stop-custodian> is shutdown

(custodian-limit-memory <custodian> <amount>) ; Register a limit on memory for the <custodian>
```


Custodians manage threads, ports, sockets, etc.
A bit of information about them is available [http://docs.racket-lang.org/reference/eval-model.html?q=memory&q=custodian&q=computer&q=pointer#%28part._custodian-model%29 here]


## Retro

Retro's memory is directly accessible via '''fetch''' and '''store'''. This is used for all functions and data structures. A variable, '''Heap''', points to the next free address. '''allot''' can be used to allocate or free memory. The amount of memory varies by the runtime, and can be accessed via the '''EOM''' constant.


```Retro
display total memory available

~~~
EOM n:put
~~~

display unused memory

~~~
EOM here - n:put
~~~

display next free address

~~~
here n:put
~~~

allocate 1000 cells

~~~
#1000 allot
~~~

free 500 cells

~~~
#-500 allot
~~~
```



## REXX

There is no explicit memory allocation in the REXX language,   variables are allocated as they are assigned   (or re-assigned).

Most REXX interpreters will obtain a chunk (block) of free storage, and then allocate storage out of that pool if possible, if not, obtain more storage.

One particular implementation of REXX has predefined and limited amount of free storage.

```rexx
Axtec_god.3='Quetzalcoatl ("feathered serpent"), god of learning, civilization, regeneration, wind and storms'
```

There is no explicit way to de-allocate memory, but there is a DROP statement that "un-defines" a REXX variable and it's memory is then free to be used for other variables, provided that free memory isn't too fragmented.

```rexx
drop  xyz  NamesRoster  j  k  m  caves  names.  Axtec_god.  Hopi  Hopi

/* it's not considered an error to DROP a variable that isn't defined.*/
```

Any variables (that are non-exposed) which are defined (allocated) in a procedure/subroutine/function will be un-allocated at the termination (completion, when it RETURNS or EXITs) of the procedure/subroutine/function.





## Ring


```ring

cVar = "     "  # create variable contains string of 5 bytes
cVar = NULL     # destroy the 5 bytes string !

```



## Ruby

Class#allocate explicitly allocates memory for a new object, inside the [[garbage collection|garbage-collected heap]]. Class#allocate never calls #initialize.


```ruby
class Thingamajig
  def initialize
    fail 'not yet implemented'
  end
end
t = Thingamajig.allocate
```



## Rust

The method shown below will be deprecated soon in favor of the `std::alloc::Alloc` trait. Follow the progress on github's issue tracker:

https://github.com/rust-lang/rust/issues/32838


```rust
// we have to use `unsafe` here because 
// we will be dereferencing a raw pointer
unsafe {
    use std::alloc::{Layout, alloc, dealloc};
    // define a layout of a block of memory
    let int_layout = Layout::new::<i32>();

    // memory is allocated here
    let ptr = alloc(int_layout);

    // let us point to some data
    *ptr = 123;
    assert_eq!(*ptr, 123);

    // deallocate `ptr` with associated layout `int_layout`
    dealloc(ptr, int_layout);
}
```



## Scala

The same as Java applies to Scala, because the VM will take of memory allocation by means of the Memory Manager. In Scala it's not a programmer concern.


## Sinclair ZX81 BASIC

Ordinary variables spring into existence when they are first assigned to; arrays need to be <code>DIM</code>ensioned first. There is no easy way to remove a <i>particular</i> named variable, but a <code>CLEAR</code> statement removes <i>all</i> user-defined variables. This can be useful under two sets of circumstances: (1) if the program has, say, a setting-up section whose variables will not be needed again, so that their storage space can be reclaimed, or (2) if you are editing a larger program—variables persist even after the program has finished running, so <code>CLEAR</code>ing them frees up some memory and may make viewing and editing your program more comfortable.

If you want to allocate an arbitrary block of bytes that the interpreter will not interfere with, there are two ways to do it. The first is by altering the system variable <tt>RAMTOP</tt>: this is a 16-bit value stored in little-endian format at addresses 16388 and 16389, and tells BASIC the highest byte it can use. On a 1k ZX81, <tt>RAMTOP</tt> equals 17408 (until you change it); to find it on your system, use

```basic
PRINT PEEK 16388+256*16389
```

You can then use <code>POKE</code> statements to reset <tt>RAMTOP</tt> to a lower value, thus reserving the space above it.

The second approach, suitable especially if you want to reserve a few bytes for a small machine code subroutine, is to hide the storage space you want inside a comment. When you enter a <code>REM</code> statement, the interpreter sets aside sufficient bytes to store the text of your comment <i>and then doesn't care what you put in them</i>: if you know the address where the comment is stored, therefore, you can <code>POKE</code> whatever values you like into that space. If the comment is the first line in the program, the <code>REM</code> itself will be at address 16513 and the comment text will begin at 16514 and take up one byte per character. An example, with a trivial machine code routine (it adds 3 and 4):

```basic
10 REM ABCDEFGH
20 LET P$="3E03010400814FC9"
30 LET ADDR=16514
40 POKE ADDR,CODE P$*16+CODE P$(2)-476
50 LET P$=P$(3 TO )
60 LET ADDR=ADDR+1
70 IF P$<>"" THEN GOTO 40
80 CLEAR
90 PRINT USR 16514
```

The <tt>ABCDEFGH</tt> is arbitrary: any other eight characters would work just as well. The string in line <tt>20</tt> is the hex representation of the Z80 code, which could be disassembled as:

```z80asm
3e 03     ld    a, 3
01 04 00  ld    bc,0004
81        add   a, c
4f        ld    c, a
c9        ret
```

Line <tt>40</tt> reads a two-digit hex number and pokes its value into memory. <code>USR</code> ("user sub routine"), in line <tt>90</tt>, is a function that takes the address of a machine language routine, calls it, and returns the contents of the <tt>BC</tt> register pair when the routine terminates. Under normal circumstances, once you were satisfied the machine code program was working correctly you would remove lines <tt>20</tt> to <tt>80</tt>, leaving just the machine code subroutine and the call to it. Note that if you list the program once you have run it, the first line will look something like this:

```basic
10 REM Y▀▀:▖ ▟?TAN
```

Unfortunately, you cannot type that in directly: not all 256 possible values are accessible from the keyboard, so there is no point trying to learn to enter machine code in that form.


## SNOBOL4

In SNOBOL4, simple data values are just created and assigned to variables.  Here, three separate strings are concatenated and stored as a newly allocated string variable:


```snobol4
     newstring = "This is creating and saving" " a new single string " "starting with three separate strings."
```


Empty arrays are created by using the built-in function (the size is determined when it is created):


```snobol4
     newarray = array(100)
```


Empty tables are similarly created using the built-in function (new entries can be simply added at any later time by just storing them into the table):


```snobol4
     newtable = table()
```


User-defined datatypes (usually, multi-field structures) are defined using the data() built-in function (which creates the constructor and field access functions):


```snobol4
     data("listnode(next,prev,datafield1,datafield2)")
```


Then you allocate a new example of the defined listnode data item by using the constructor the data() function created:


```snobol4
     newnode = listnode(,,"string data value1",17)
```


The example thus created can be updated using the field access functions also created by the data() function:


```snobol4
     datafield1(newnode) = "updated data value 1"
```


You don't need to explicitly de-allocate memory.  When you leave a function which has declared local variables, data stored in those local variables is released upon return.  You can also just store a null string into a variable, releasing the value that was stored in that variable previously:


```snobol4>     newnode = </lang
  

SNOBOL4 automatically garbage collects released data items on an as-needed basis, and moves allocated items to consolidate all released space (so memory fragmentation is never a problem).  You can explicitly garbage collect if you really want to:


```snobol4
     collect()
```



## Tcl

Generally speaking you do not perform memory allocation directly in Tcl; scripts leave that under the control of the runtime (which manages the heap, threaded object pools and stacks automatically for you) and just work with values of arbitrary size. Indeed, the sizes of entities are not guaranteed in any case; the number of bytes per character or number does vary over time as the runtime selects the most efficient representation for it.

However the [[Machine Address]] task shows how you can do it directly if necessary.
It just happens that it never actually is necessary to directly allocate memory in Tcl scripts in practice.

More commonly, a package written in [[C]] will be used to manage the memory on behalf of Tcl, with explicit memory management. Here is an example of such a package:

```c>#include <tcl.h


/* A data structure used to enforce data safety */
struct block {
    int size;
    unsigned char data[4];
};

static int
Memalloc(
    ClientData clientData,
    Tcl_Interp *interp,
    int objc, Tcl_Obj *const *objv)
{
    Tcl_HashTable *nameMap = clientData;
    static int nameCounter = 0;
    char nameBuf[30];
    Tcl_HashEntry *hPtr;
    int size, dummy;
    struct block *blockPtr;

    /* Parse arguments */
    if (objc != 2) {
	Tcl_WrongNumArgs(interp, 1, objv, "size");
	return TCL_ERROR;
    }
    if (Tcl_GetIntFromObj(interp, objv[1], &size) != TCL_OK) {
	return TCL_ERROR;
    }
    if (size < 1) {
	Tcl_AppendResult(interp, "size must be positive", NULL);
	return TCL_ERROR;
    }

    /* The ckalloc() function will panic on failure to allocate. */
    blockPtr = (struct block *)
	    ckalloc(sizeof(struct block) + (unsigned) (size<4 ? 0 : size-4));

    /* Set up block */
    blockPtr->size = size;
    memset(blockPtr->data, 0, blockPtr->size);

    /* Give it a name and return the name */
    sprintf(nameBuf, "block%d", nameCounter++);
    hPtr = Tcl_CreateHashEntry(nameMap, nameBuf, &dummy);
    Tcl_SetHashValue(hPtr, blockPtr);
    Tcl_SetObjResult(interp, Tcl_NewStringObj(nameBuf, -1));
    return TCL_OK;
}

static int
Memfree(
    ClientData clientData,
    Tcl_Interp *interp,
    int objc, Tcl_Obj *const *objv)
{
    Tcl_HashTable *nameMap = clientData;
    Tcl_HashEntry *hPtr;
    struct block *blockPtr;

    /* Parse the arguments */
    if (objc != 2) {
	Tcl_WrongNumArgs(interp, 1, objv, "handle");
	return TCL_ERROR;
    }
    hPtr = Tcl_FindHashEntry(nameMap, Tcl_GetString(objv[1]));
    if (hPtr == NULL) {
	Tcl_AppendResult(interp, "unknown handle", NULL);
	return TCL_ERROR;
    }
    blockPtr = Tcl_GetHashValue(hPtr);

    /* Squelch the memory */
    Tcl_DeleteHashEntry(hPtr);
    ckfree((char *) blockPtr);
    return TCL_OK;
}

static int
Memset(
    ClientData clientData,
    Tcl_Interp *interp,
    int objc, Tcl_Obj *const *objv)
{
    Tcl_HashTable *nameMap = clientData;
    Tcl_HashEntry *hPtr;
    struct block *blockPtr;
    int index, byte;

    /* Parse the arguments */
    if (objc != 4) {
	Tcl_WrongNumArgs(interp, 1, objv, "handle index byte");
	return TCL_ERROR;
    }
    hPtr = Tcl_FindHashEntry(nameMap, Tcl_GetString(objv[1]));
    if (hPtr == NULL) {
	Tcl_AppendResult(interp, "unknown handle", NULL);
	return TCL_ERROR;
    }
    blockPtr = Tcl_GetHashValue(hPtr);
    if (Tcl_GetIntFromObj(interp, objv[2], &index) != TCL_OK
	    || Tcl_GetIntFromObj(interp, objv[3], &byte) != TCL_OK) {
	return TCL_ERROR;
    }
    if (index < 0 || index >= blockPtr->size) {
	Tcl_AppendResult(interp, "index out of range", NULL);
	return TCL_ERROR;
    }

    /* Update the byte of the data block */
    blockPtr->data[index] = (unsigned char) byte;
    return TCL_OK;
}

static int
Memget(
    ClientData clientData,
    Tcl_Interp *interp,
    int objc, Tcl_Obj *const *objv)
{
    Tcl_HashTable *nameMap = clientData;
    Tcl_HashEntry *hPtr;
    struct block *blockPtr;
    int index, byte;

    /* Parse the arguments */
    if (objc != 3) {
	Tcl_WrongNumArgs(interp, 1, objv, "handle index");
	return TCL_ERROR;
    }
    hPtr = Tcl_FindHashEntry(nameMap, Tcl_GetString(objv[1]));
    if (hPtr == NULL) {
	Tcl_AppendResult(interp, "unknown handle", NULL);
	return TCL_ERROR;
    }
    blockPtr = Tcl_GetHashValue(hPtr);
    if (Tcl_GetIntFromObj(interp, objv[2], &index) != TCL_OK) {
	return TCL_ERROR;
    }
    if (index < 0 || index >= blockPtr->size) {
	Tcl_AppendResult(interp, "index out of range", NULL);
	return TCL_ERROR;
    }

    /* Read the byte from the data block and return it */
    Tcl_SetObjResult(interp, Tcl_NewIntObj(blockPtr->data[index]));
    return TCL_OK;
}

static int
Memaddr(
    ClientData clientData,
    Tcl_Interp *interp,
    int objc, Tcl_Obj *const *objv)
{
    Tcl_HashTable *nameMap = clientData;
    Tcl_HashEntry *hPtr;
    struct block *blockPtr;
    int addr;

    /* Parse the arguments */
    if (objc != 2) {
	Tcl_WrongNumArgs(interp, 1, objv, "handle");
	return TCL_ERROR;
    }
    hPtr = Tcl_FindHashEntry(nameMap, Tcl_GetString(objv[1]));
    if (hPtr == NULL) {
	Tcl_AppendResult(interp, "unknown handle", NULL);
	return TCL_ERROR;
    }
    blockPtr = Tcl_GetHashValue(hPtr);

    /* This next line is non-portable */
    addr = (int) blockPtr->data;
    Tcl_SetObjResult(interp, Tcl_NewIntObj(addr));
    return TCL_OK;
}

int
Memalloc_Init(Tcl_Interp *interp)
{
    /* Make the hash table */
    Tcl_HashTable *hashPtr = (Tcl_HashTable *) ckalloc(sizeof(Tcl_HashTable));
    Tcl_InitHashTable(hashPtr, TCL_STRING_KEYS);

    /* Register the commands */
    Tcl_CreateObjCommand(interp, "memalloc", Memalloc, hashPtr, NULL);
    Tcl_CreateObjCommand(interp, "memfree", Memfree, hashPtr, NULL);
    Tcl_CreateObjCommand(interp, "memset", Memset, hashPtr, NULL);
    Tcl_CreateObjCommand(interp, "memget", Memget, hashPtr, NULL);
    Tcl_CreateObjCommand(interp, "memaddr", Memaddr, hashPtr, NULL);

    /* Register the package */
    return Tcl_PkgProvide(interp, "memalloc", "1.0");
}
```


The package would then be used like this:

```tcl
package require memalloc

set block [memalloc 1000]
puts "allocated $block at [memaddr $block]"
memset $block 42 79
someOtherCommand [memaddr $block]
puts "$block\[42\] is now [memget $block 42]"
memfree $block
```


Other methods of performing things like memory allocation are also possible.
* Using <code>string repeat</code> or <code>lrepeat</code> to make a group of entities that work like a block of memory (despite not being); these need marshalling code to bridge to foreign function interfaces.
* Using [[:Category:SWIG|SWIG]] or [[:Category:critcl|critcl]] to write a bridge to a standard [[C]] allocator.


## X86 Assembly

This is a bare-bones implementation of a heap memory allocator for x86_64 Linux. We alloctate memory page at a time using brk and divide up the memory in chunks of requested size using a linked list-like block struct. Not optimized for speed or efficiency.


```x86asm
 
; linux x86_64

struc block
free: resb 1 ; whether or not this block is free
size: resb 2 ; size of the chunk of memory
next: resb 8 ; the next chunk after this one
mem:
endstruc

section .data
hStart: dq 0 ; the beginning of our heap space
break: dq 0 ; the current end of our heap space


section .text

Allocate:

  push rdi ; save the size argument

  cmp qword [break], 0 ; if breakpoint is zero this
  je firstAlloc        ; is the first call to allocate

  mov rdi, qword [hStart] ; else address of heap start

  findBlock: ; look for a suitable block of memory

    cmp byte [rdi + free], 2
    je newBlock ; end of heap reached, create new block

    cmp byte [rdi + free], 0
    je skipBlock ; this block taken

    ; this block is free, make
    ; sure it's big enough
    mov bx, word [rdi + size] 
    mov rcx, qword [rsp] ; compare against our size arg
    cmp cx, bx
    jg skipBlock ; keep looking if not big enough

    mov byte [rdi + free], 0 ; else mark as taken
    add rdi, mem
    add rsp, 8 ; discard size arg, we didn't need it
    mov rax, rdi ; return pointer to this block
    ret

    skipBlock:
      mov rsi, qword [rdi + next] ; load next
      mov rdi, rsi                ' block address
      jmp findBlock

    newBlock:
      mov rax, rdi
      add rdi, 1024
      cmp rdi, qword [break]
      jl initAndAllocate
      push rax
      mov rdi, qword [break] ; if we are getting low on
      add rdi, 4096          ; heap space, we ask OS for
      mov rax, 12            ; more memory with brk syscall
      syscall
      cmp rax, qword [break] ; if breakpoint has not increased,
      je allocFail           ; then memory could not be allocated
      mov qword [break], rax 
      pop rax
      jmp initAndAllocate
      

  firstAlloc:    ; extra work has to be done on first
    mov rax, 12  ; call to this subroutine
    mov rdi, 0
    syscall
    mov qword [hStart], rax ; init heap start
    add rax, 4096
    mov rdi, rax
    mov rax, 12   ; get heap memory with sys brk
    syscall
    cmp rax, qword [hStart]
    je allocFail
    mov qword [break], rax
    mov rax, qword [hStart]

  initAndAllocate:
    mov byte [rax + free], 0  ; mark block free
    pop rdi ; pop size arg off stack
    mov word [rax + size], di ; mark it's size
    lea rsi, [rax + mem + rdi]
    mov byte [rsi + free], 2 ; mark heap end block
    mov qword [rax + next], rsi ; mark next block
    add rax, mem ; return pointer to block's memory space
    ret

allocFail: ; exit(1) when allocation fails
  mov rax, 60
  mov rdi, 1
  syscall
  ret

; free this block so it can be
; reused in a subsequent call to allocate
Release:
  sub rdi, mem
  mov byte [rdi + free], 1
  ret


```



## XPL0


```XPL0
int Array(10);          \allocates 10 integers (40 bytes) of heap space
Array2:= Reserve(10*4); \another way to allocate 10 integers of heap space
Array3:= MAlloc(4);     \allocate 4 paragraphs (64 bytes) of conventional memory
...
Release(Array3);        \release this memory so it can be used elsewhere
```


Heap space (for 32-bit XPL0) resides in extended memory (on an
IBM-compatible PC), which is beyond the first 1 megabyte of address
space, where there are typically many megabytes available for arrays and
variables. Heap space acquired in a procedure disappears (is returned to
the general heap pool) when the procedure finishes running.

MAlloc is useful for acquiring blocks of memory that remain after a
procedure finishes. These blocks are only made available for other use if
they are released with a call to the Release intrinsic (or when the
program finishes). If MAlloc is called inside a loop, it will allocate
more memory each time it's called. (It's easy to do this unintentionally
which results in a "memory leak.")

MAlloc is also useful for communicating with MS-DOS routines that require
blocks of memory. This is because it allocates conventional memory, which
is memory that resides in the first 640 kilobytes of address space and
which is accessible to MS-DOS (which runs in 16-bit real mode as opposed
to XPL0 which runs in 32-bit protected mode using a DPMI).


## zkl

Explicit memory management is not possible in zkl; to create/allocate, call the "create" method, the garbage collector reclaims no longer referenced memory. Some objects dynamically resize themselves and some objects take hints about how big they will become.

Extension libraries/DLLs (written in C) can add this capability (the FFI library does this).

```zkl
Data(123);  // this bit bucket expects hold 123 bytes
List.createLong(123);  // this list expects to hold 123 elements
```



## ZX Spectrum Basic


On the ZX Spectrum, memory above the ramtop is protected, so the usual way to reserve memory is to move the ramtop to a lower address using a clear command. The following example assumes that a 48k ZX Spectrum is being used:


```zxbasic
10 REM This code is written for a 48k spectrum
20 CLEAR 65535 - 8192: REM reserve 8192 bytes of memory
30 CLEAR 65535: REM unreserve the memory, moving the ramtop back to the top of the ram
```


{{omit from|ACL2}}
{{omit from|AWK}}
{{omit from|Batch File|No memory management.}}
{{omit from|Clojure}}
{{omit from|GUISS}}
{{omit from|Groovy}}
{{omit from|JavaScript}}
{{omit from|Lily}}
{{omit from|Logtalk}}
{{omit from|M4}}
{{omit from|ML/I}}
{{omit from|Oz}}
{{omit from|Icon}}
{{omit from|Unicon}}
{{omit from|UNIX Shell}}
{{omit from|Unlambda|No explicit memory management.}}

[[Category:Memory management]]
