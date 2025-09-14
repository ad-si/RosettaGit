+++
title = "Arena storage pool"
description = ""
date = 2019-08-12T18:03:11Z
aliases = []
[extra]
id = 4028
[taxonomies]
categories = ["Encyclopedia", "task"]
tags = []
languages = [
  "ada",
  "c",
  "cpp",
  "erlang",
  "fortran",
  "go",
  "j",
  "julia",
  "kotlin",
  "mathematica",
  "oforth",
  "oorexx",
  "oxygenbasic",
  "pari_gp",
  "pascal",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "python",
  "racket",
  "rexx",
  "rust",
  "scala",
  "tcl",
  "zkl",
]
+++

## Task
Dynamically allocated objects take their memory from a [[heap]]. The memory for an object is provided by an '''allocator''' which maintains the storage pool used for the [[heap]]. Often a call to allocator is denoted as

```ada>P := new T</lang

where T is the type of an allocated object and P is a [[reference]] to the object.

The storage pool chosen by the allocator can be determined by either:
* the object type T;
* the type of pointer P.
In the former case objects can be allocated only in one storage pool. In the latter case objects of the type can be allocated in any storage pool or on the [[stack]].

'''Task description'''

The task is to show how allocators and user-defined storage pools are supported by the language. In particular:
# define an arena storage pool. An arena is a pool in which objects are allocated individually, but freed by groups.
# allocate some objects (e.g., integers) in the pool.
Explain what controls the storage pool choice in the language.


## Ada

In [[Ada]] the choice of storage pool is controlled by the type of the pointer. Objects pointed by anonymous access types are allocated in the default storage pool. Pool-specific pointer types may get a pool assigned to them:

```ada
type My_Pointer is access My_Object;
for My_Pointer'Storage_Pool use My_Pool;
```

The following example illustrates implementation of an arena pool. Specification:

```ada
with System.Storage_Elements;  use System.Storage_Elements;
with System.Storage_Pools;     use System.Storage_Pools;

package Arena_Pools is
   type Arena (Size : Storage_Count) is new Root_Storage_Pool with private;
   overriding
      procedure Allocate
                (  Pool      : in out Arena;
                   Address   : out System.Address;
                   Size      : Storage_Count;
                   Alignment : Storage_Count
                );
   overriding
      procedure Deallocate
                (  Pool      : in out Arena;
                   Address   : System.Address;
                   Size      : Storage_Count;
                   Alignment : Storage_Count
                )  is null;
   overriding
      function Storage_Size (Pool : Arena) return Storage_Count;
private
   type Arena (Size : Storage_Count) is new Root_Storage_Pool with record
      Free : Storage_Offset := 1;
      Core : Storage_Array (1..Size);
   end record;
end Arena_Pools;
```

Here is an implementation of the package:

```ada
package body Arena_Pools is
   procedure Allocate
             (  Pool      : in out Arena;
                Address   : out System.Address;
                Size      : Storage_Count;
                Alignment : Storage_Count
             )  is
      Free : constant Storage_Offset :=
         Pool.Free + Alignment - Pool.Core (Pool.Free)'Address mod Alignment + Size;
   begin
      if Free - 1 > Pool.Size then
         raise Storage_Error;
      end if;
      Pool.Free := Free;
      Address := Pool.Core (Pool.Free - Size)'Address;
   end Allocate;

   function Storage_Size (Pool : Arena) return Storage_Count is
   begin
      return Pool.Size;
   end Storage_Size;
end Arena_Pools;
```

The following is a test program that uses the pool:

```ada
with Arena_Pools;
use  Arena_Pools;

procedure Test_Allocator is
   Pool : Arena_Pools.Arena (1024);
   type Integer_Ptr is access Integer;
   for Integer_Ptr'Storage_Pool use Pool;

   X : Integer_Ptr := new Integer'(1);
   Y : Integer_Ptr := new Integer'(2);
   Z : Integer_Ptr;
begin
   Z := new Integer;
   Z.all := X.all + Y.all;
end Test_Allocator;
```



## C

For C, dynamic memory is often used for structures and for arrays when the size of the
array is unknown in advance. 'Objects' in C are pretty much structures, with the structure sometimes including a pointer to a virtual dispatch table.

To use dynamic memory, the header for the standard library must be included in the module.

```c
#include <stdlib.h>
```

Uninitialized memory is allocated using the malloc function. To obtain the amount of memory that needs to be allocated, sizeof is used. Sizeof is not a normal C function, it is evaluated by the compiler to obtain the amount of memory needed.

```c
int *var = malloc(n*sizeof(int));
Typename *var = malloc(sizeof(Typename));
Typename *var = malloc(sizeof var[0]);
```

Since pointers to structures are needed so frequently, often a
typedef will define a type as being a pointer to the associated structure.
Once one gets used to the notation, programs are actually easier to read, as the
variable declarations don't include all the '*'s.

```c
typedef struct mytypeStruct { .... } sMyType, *MyType;

MyType var = malloc(sizeof(sMyType));
```

The calloc() function initializes all allocated memory to zero. It is also often
used for allocating memory for arrays of some type.

```c
/* allocate an array of n MyTypes */
MyType var = calloc(n, sizeof(sMyType));

MyType third = var+3;       /* a reference to the 3rd item allocated */

MyType fourth = &var[4];    /* another way, getting the fourth item */
```

Freeing memory dynamically allocated from the heap is done by calling free().

```c
free(var);
```

One can allocate space on the stack using the alloca() function. You do not
free memory that's been allocated with alloca

```c
Typename *var = alloca(sizeof(Typename));
```

An object oriented approach will define a function for creating a new object of a class.
In these systems, the size of the memory that needs to be allocated for an instance of the
class will often be included in the 'class' record.
See http://rosettacode.org/wiki/Polymorphic%20copy#C

Without using the standard malloc, things get a bit more complicated. For example, here is some code that implements something like it using the mmap system call (for Linux):


```c
#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>

// VERY rudimentary C memory management independent of C library's malloc.

// Linked list (yes, this is inefficient)
struct __ALLOCC_ENTRY__
{
    void * allocatedAddr;
    size_t size;
    struct __ALLOCC_ENTRY__ * next;
};
typedef struct __ALLOCC_ENTRY__ __ALLOCC_ENTRY__;

// Keeps track of allocated memory and metadata
__ALLOCC_ENTRY__ * __ALLOCC_ROOT__ = NULL;
__ALLOCC_ENTRY__ * __ALLOCC_TAIL__ = NULL;

// Add new metadata to the table
void _add_mem_entry(void * location, size_t size)
{

    __ALLOCC_ENTRY__ * newEntry = (__ALLOCC_ENTRY__ *) mmap(NULL, sizeof(__ALLOCC_ENTRY__), PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

    if (__ALLOCC_TAIL__ != NULL)
    {
        __ALLOCC_TAIL__ -> next = newEntry;
        __ALLOCC_TAIL__ = __ALLOCC_TAIL__ -> next;
    }
    else
    {
        // Create new table
        __ALLOCC_ROOT__ = newEntry;
        __ALLOCC_TAIL__ = newEntry;
    }

    __ALLOCC_ENTRY__ * tail = __ALLOCC_TAIL__;
    tail -> allocatedAddr = location;
    tail -> size = size;
    tail -> next = NULL;
    __ALLOCC_TAIL__ = tail;
}

// Remove metadata from the table given pointer
size_t _remove_mem_entry(void * location)
{
    __ALLOCC_ENTRY__ * curNode = __ALLOCC_ROOT__;

    // Nothing to do
    if (curNode == NULL)
    {
        return 0;
    }

    // First entry matches
    if (curNode -> allocatedAddr == location)
    {
        __ALLOCC_ROOT__ = curNode -> next;
        size_t chunkSize = curNode -> size;

        // No nodes left
        if (__ALLOCC_ROOT__ == NULL)
        {
            __ALLOCC_TAIL__ = NULL;
        }
        munmap(curNode, sizeof(__ALLOCC_ENTRY__));

        return chunkSize;
    }

    // If next node is null, remove it
    while (curNode -> next != NULL)
    {
        __ALLOCC_ENTRY__ * nextNode = curNode -> next;

        if (nextNode -> allocatedAddr == location)
        {
            size_t chunkSize = nextNode -> size;

            if(curNode -> next == __ALLOCC_TAIL__)
            {
                __ALLOCC_TAIL__ = curNode;
            }
            curNode -> next = nextNode -> next;
            munmap(nextNode, sizeof(__ALLOCC_ENTRY__));

            return chunkSize;
        }

        curNode = nextNode;
    }

    // Nothing was found
    return 0;
}

// Allocate a block of memory with size
// When customMalloc an already mapped location, causes undefined behavior
void * customMalloc(size_t size)
{
    // Now we can use 0 as our error state
    if (size == 0)
    {
        return NULL;
    }

    void * mapped = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

    // Store metadata
    _add_mem_entry(mapped, size);

    return mapped;
}

// Free a block of memory that has been customMalloc'ed
void customFree(void * addr)
{
    size_t size = _remove_mem_entry(addr);

    munmap(addr, size);
}

int main(int argc, char const *argv[])
{
    int *p1 = customMalloc(4*sizeof(int));  // allocates enough for an array of 4 int
    int *p2 = customMalloc(sizeof(int[4])); // same, naming the type directly
    int *p3 = customMalloc(4*sizeof *p3);   // same, without repeating the type name

    if(p1) {
        for(int n=0; n<4; ++n) // populate the array
            p1[n] = n*n;
        for(int n=0; n<4; ++n) // print it back out
            printf("p1[%d] == %d\n", n, p1[n]);
    }

    customFree(p1);
    customFree(p2);
    customFree(p3);

    return 0;
}
```


This is ''not'' how the real malloc is implemented on Linux. For one, memory leaks cannot be caught by Valgrind, and using a linked list to keep track of allocated blocks is very inefficient.


## C++

In C++, the situation with allocators is quite complex:
* You can define class-specific allocation/deallocation by adding class members <code>operator new</code> and <code>operator delete</code>. Those are then used whenever you use new for that type (or a type derived from it, if it doesn't itself replace operator new), and when you delete an object of that type. Note that arrays and single objects have both separate allocation functions and deallocation functions.
* You can replace the global allocation/deallocation routines, which are used by new/delete whenever there are no class specific functions available.
* You can write operator new/operator delete with additional arguments, both in a class and globally. To use those, you add those parameters after the keyword <code>new</code>, like

```cpp
T* foo = new(arena) T;
```

* In addition, for objects in containers, there's a completely separate allocator interface, where the containers use an allocator object for allocating/deallocating memory.

The following code uses class-specific allocation and deallocation functions:


```cpp
#include <cstdlib>
#include <cassert>
#include <new>

// This class basically provides a global stack of pools; it is not thread-safe, and pools must be destructed in reverse order of construction
// (you definitely want something better in production use :-))
class Pool
{
public:
  Pool(std::size_type sz);
  ~Pool();
  static Pool& current() { return *cur; }
  void* allocate(std::size_type sz, std::size_t alignment);
private:
  char* memory; // char* instead of void* enables pointer arithmetic
  char* free;
  char* end;
  Pool* prev;
  static Pool* cur;

  // prohibit copying
  Pool(Pool const&); // not implemented
  Pool& operator=(Pool const&); // not implemented
};

Pool* pool::cur = 0;

Pool::Pool(std::size_type size):
  memory(static_cast<char*>(::operator new(size))),
  free(memory),
  end(memory + size))
{
  prev = cur;
  cur = this;
}

Pool::~Pool()
{
  ::operator delete(memory);
  cur = prev;
}

void* Pool::allocate(std::size_t size, std::size_t alignment)
{
  char* start = free;

  // align the pointer
  std::size_t extra = (start - memory) % aligment;
  if (extra != 0)
  {
    extra = alignment - extra;
  }

  // test if we can still allocate that much memory
  if (end - free < size + extra)
    throw std::bad_alloc();

  // the free memory now starts after the newly allocated object
  free = start + size + extra;
  return start;
}

// this is just a simple C-like struct, except that it uses a specific allocation/deallocation function.
struct X
{
  int member;
  void* operator new(std::size_t);
  void operator delete(void*) {} // don't deallocate memory for single objects
};

void* X::operator new(std::size_t size)
{
  // unfortunately C++ doesn't offer a portable way to find out alignment
  // however, using the size as alignment is always safe (although usually wasteful)
  return Pool::current().allocate(size, size);
}

// Example program
int main()
{
  Pool my_pool(3*sizeof(X));
  X* p1 = new X; // uses the allocator function defined above
  X* p2 = new X;
  X* p3 = new X;
  delete p3; // doesn't really deallocate the memory because operator delete has an empty body

  try
  {
    X* p4 = new X; // should fail
    assert(false);
  }
  catch(...)
  {
  }

  X* p5 = new X[10]; // uses global array allocation routine because we didn't provide operator new[] and operator delete[]
  delete[] p5; // global array deallocation

  Pool* my_second_pool(1000); // a large pool
  X* p6 = new X; // allocate a new object from that pool
  X* p7 = new X;
  delete my_second_pool // also deallocates the memory for p6 and p7

} // Here my_pool goes out of scope, deallocating the memory for p1, p2 and p3
```



## Erlang

Given automatic memory handling the only way to ask for memory in Erlang is when creating a process. Likewise the only way to manually return memory is by killing a process. So the pool could be built like this. The unit for memory is word, b.t.w.


```Erlang

-module( arena_storage_pool ).

-export( [task/0] ).

task() ->
      Pid = erlang:spawn_opt( fun() -> loop([]) end, [{min_heap_size, 10000}] ),
      set( Pid, 1, ett ),
      set( Pid, "kalle", "hobbe" ),
      V1 = get( Pid, 1 ),
      V2 = get( Pid, "kalle" ),
      true = (V1 =:= ett) and (V2	=:= "hobbe"),
      erlang:exit( Pid, normal ).



get( Pid, Key ) ->
     Pid ! {get, Key, erlang:self()},
     receive
	{value, Value, Pid} -> Value
     end.

loop( List ) ->
      receive
	{set, Key, Value} -> loop( [{Key, Value} | proplists:delete(Key, List)] );
	{get, Key, Pid} ->
	      Pid ! {value, proplists:get_value(Key, List), erlang:self()},
	      loop( List )
	end.

set( Pid, Key, Value ) -> Pid ! {set, Key, Value}.

```



## Fortran

Run-time memory allocation is a latter-day feature in Fortran. In the beginning, a programme would either fit in the available memory or it would not. Any local variables declared in subroutines, especially arrays, would have some storage requirement that had been fixed at compile time, and space would be reserved for all of them whether any subroutine would be invoked or not in a particular run. Fixed array sizes were particularly troublesome in subroutines, as pre-specifying some largeish size for all such arrays would soon exhaust the available memory and this was especially annoying when it was never going to be the case that all the arrays had to be available simultaneously because not all the subroutines would be invoked or be active together in a particular run. Thus, developers of complicated calculations, say involving a lot of matrix manipulation, would be forced towards devising some storage allocation scheme involving scratchpad arrays that would be passed as additional parameters for subroutines to use as working storage, and soon enough one escalated to having a "pool" array, with portions being reserved and passed about the various routines as needed for a given run. Possibly escalating to further schemes involving disc storage and a lot of effort, repaid in suddenly having larger problems solvable.

Fortran 90 standardised two ameliorations. A subroutine can now declare arrays whose size is specified at run time, with storage typically organised via a stack, since on exit from the subroutine such storage is abandoned, which is to say, returned to the system pool. Secondly, within a routine, and not requiring entry into a subroutine (nor a <code>begin ... end;</code> block as in Algol), storage can be explicitly allocated with a specified size for arrays as needed, this time from a "heap" storage pool, and later de-allocated. Again, on exiting the subroutine, storage for such arrays (if declared within the subroutine) is abandoned.

Thus, in a sense, a group of items for which storage has been allocated can have their storage released en-mass by exiting the routine. However, it is not the case that items A, B, C can be allocated in one storage "area" (say called "Able") and another group D, E in a second named area (say "Baker"), and that by discarding "Able" all its components would be de-allocated without the need to name them in tedious detail.

So, for example:
```Fortran
      SUBROUTINE CHECK(A,N)	!Inspect matrix A.
       REAL A(:,:)	!The matrix, whatever size it is.
       INTEGER N	!The order.
       REAL B(N,N)	!A scratchpad, size known on entry..
       INTEGER, ALLOCATABLE::TROUBLE(:)	!But for this, I'll decide later.
       INTEGER M

        M = COUNT(A(1:N,1:N).LE.0)	!Some maximum number of troublemakers.

        ALLOCATE (TROUBLE(1:M**3))	!Just enough.

        DEALLOCATE(TROUBLE)		!Not necessary.
      END SUBROUTINE CHECK		!As TROUBLE is declared within CHECK.
```


Whereas previously a problem might not be solvable via the existing code because of excessive fixed-size storage requirements, now reduced demands can be made and those only for subroutines that are in action. Thus larger problems can be handled without agonising attempts to cut-to-fit, the usage for scratchpads such as B being particularly natural as in Algol from the 1960s. But on the other hand, a run might exhaust the available storage (either via the stack or via the heap) somewhere in the middle of job because its particular execution path made too many requests and the happy anticipation of results is instead met by a mess - and a bigger mess, because larger problems are being attempted.


## Go


```go
package main

import (
    "fmt"
    "runtime"
    "sync"
)

// New to Go 1.3 are sync.Pools, basically goroutine-safe free lists.
// There is overhead in the goroutine-safety and if you do not need this
// you might do better by implementing your own free list.

func main() {
    // Task 1:  Define a pool (of ints).  Just as the task says, a sync.Pool
    // allocates individually and can free as a group.
    p := sync.Pool{New: func() interface{} {
        fmt.Println("pool empty")
        return new(int)
    }}
    // Task 2: Allocate some ints.
    i := new(int)
    j := new(int)
    // Show that they're usable.
    *i = 1
    *j = 2
    fmt.Println(*i + *j) // prints 3
    // Task 2 continued:  Put allocated ints in pool p.
    // Task explanation:  Variable p has a pool as its value.  Another pool
    // could be be created and assigned to a different variable.  You choose
    // a pool simply by using the appropriate variable, p here.
    p.Put(i)
    p.Put(j)
    // Drop references to i and j.  This allows them to be garbage collected;
    // that is, freed as a group.
    i = nil
    j = nil
    // Get ints for i and j again, this time from the pool.  P.Get may reuse
    // an object allocated above as long as objects haven't been garbage
    // collected yet; otherwise p.Get will allocate a new object.
    i = p.Get().(*int)
    j = p.Get().(*int)
    *i = 4
    *j = 5
    fmt.Println(*i + *j) // prints 9
    // One more test, this time forcing a garbage collection.
    p.Put(i)
    p.Put(j)
    i = nil
    j = nil
    runtime.GC()
    i = p.Get().(*int)
    j = p.Get().(*int)
    *i = 7
    *j = 8
    fmt.Println(*i + *j) // prints 15
}
```

{{output}}

```txt

3
9
pool empty
pool empty
15

```



## J

The concepts of pools and allocation is foreign to J, and excessively verbose for most purposes. However, this task can be accomplished by relying on J's facilities for dealing with code written in foreign languages.

For example, you can define a class which allocates a pool of integers:


```j
coclass 'integerPool'
require 'jmf'
create=: monad define
  Lim=: y*SZI_jmf_
  Next=: -SZI_jmf_
  Pool=: mema Lim
)

destroy=: monad define
  memf Pool
  codestroy''
)

alloc=: monad define
  assert.Lim >: Next=: Next+SZI_jmf_
  r=.Pool,Next,1,JINT
  r set y
  r
)

get=: adverb define
  memr m
)

set=: adverb define
  y memw m
)
```


With this script you can then create instances of this class, and use them.  In this case, we will create a pool of three integers:


```j
   pool0=: 3 conew 'integerPool'
   x0=: alloc__pool0 0
   x1=: alloc__pool0 0
   x2=: alloc__pool0 0
   x0 set__pool0 7
   x1 set__pool0 8
   x2 set__pool0 9
   x0 get__pool0 + x1 get__pool0 + x2 get__pool0
24
```


Finally, the pool can be destroyed:


```j
   destroy__pool0 _
```


That said, using J's built-in support for integers (and for using them) usually results in better code.


## Julia

All program elements in Julia are dynamically allocated objects which are garbage collected
as required after they are out of scope. If a specific storage pool is needed in advance, perhaps for
memory efficiency reasons, that pool can be optionally preallocated as an array or other large structure.
For example, a large 1000 X 1000 X 1000 matrix that will need to be changed repeatedly might be
allocated and initialized to zero with:

```julia

matrix = zeros(Float64, (1000,1000,1000))
# use matrix, then when done set variable to 0 to garbage collect the matrix:
matrix = 0 # large memory pool will now be collected when needed

```



## Kotlin

The Kotlin JVM programmer does not generally need to worry about memory management as the JVM automatically takes care of memory allocation for new objects and the freeing of that memory when an object is no longer needed. The latter is accomplished using a garbage collector which runs in the background at intervals determined by the JVM though the programmer can force a collection using the System.gc() function.

Similarly, the Kotlin Native runtime takes care of memory allocation for new Kotlin objects and cleans them up when there are no longer any references to them. Currently, the latter is accomplished using 'automatic reference counting' together with a collector for cyclic references though, in principle, other systems could be plugged in.

However, where interoperation with C is required, it is often necessary to allocate memory on the native heap so that a pointer to it can be passed to a C function. It is the responsibility of the Kotlin Native programmer to allocate this memory and free it when no longer needed to avoid memory leaks.

In general native memory is allocated via the NativePlacement interface together with the alloc() or allocArray() functions. Currently, placement normally takes place using the nativeHeap object (which calls malloc() and free() in the background) though other placement objects (such as the stack) are possible in principle.

To make life easier for the programmer when a number of allocations are required, it is possible for these to take place under the auspices of a MemScope object (an 'arena') which implements NativePlacement and automatically frees up the memory for these objects when they are no longer in scope. The process is automated by the memScoped function which takes a block as a parameter whose receiver is an implicit MemScope object. Here's a very simple example:

```scala
// Kotlin Native v0.5

import kotlinx.cinterop.*

fun main(args: Array<String>) {
    memScoped {
        val intVar1 = alloc<IntVar>()
        intVar1.value = 1
        val intVar2 = alloc<IntVar>()
        intVar2.value = 2
        println("${intVar1.value} + ${intVar2.value} = ${intVar1.value + intVar2.value}")
    }
    // native memory used by intVar1 & intVar2 is automatically freed when memScoped block ends
}
```


{{output}}

```txt

1 + 2 = 3

```



## Mathematica

Mathematica does not allow stack/heap control, so all variables are defined on the heap. However, tags must be given a ''value'' for a meaningful assignment to take place.

```Mathematica
f[x_] := x^2
```



## Oforth


This only way to allocate memory is to ask new class method on a class object. This will create an instance of this class on the heap. The heap is managed by the garbage collector.

The stacks (data stack and execution stack) only holds addresses of these objects. There is no object created on the stacks, apart small integers.

There is no user-defined storage pool and it is not possible to explicitly destroy an object.


```Oforth
Object Class new: MyClass(a, b, c)
MyClass new
```



## ooRexx

In ooRexx:
* Everything is an object.
* Objects are dynamically allocated.
* Unused objects are garbage collected.

Where objects appear from, or disappear to, is treated as an implementation detail.

Statements, such as assignments, class, method, and routine definitions, and ::requires directives can create objects and assign references to them to variables. Objects can also be referred to from other objects e.g. in collections such as lists. When objects are no longer referenced, the objects become candidates for garbage collection. It is not possible to explicitly destroy an object.


## OxygenBasic


```oxygenbasic

'
### ========

Class ArenaPool
'
### ========


string buf
sys    pb,ii

method Setup(sys n) as sys {buf=nuls n : pb=strptr buf : ii=0 : return pb}
method Alloc(sys n) as sys {method=pb+ii : ii+=n}
method Empty()             {buf="" : pb=0 : ii=0}

end class

macro Create(type,name,qty,pool)
  type name[qty] at (pool##.alloc qty * sizeof type)
end macro

'====
'DEMO
'====

ArenaPool pool : pool.setup 1000 * sizeof int

Create int,i,100,pool
Create int,j,100,pool

j[51] <= 1,2,3,4,5

print j[51] j[52] j[53] j[54] j[55] 'result 15

pool.empty

```



## PARI/GP

GP has no particular control over the layout of objects in memory.

PARI allocates objects on the PARI stack by default, but objects can be allocated on the heap if desired.

```C
pari_init(1<<20, 0); // Initialize PARI with a stack size of 1 MB.
GEN four = addii(gen_2, gen_2); // On the stack
GEN persist = gclone(four); // On the heap
```



## Pascal

{{works with|Free_Pascal}}
The procedure New allocates memory on the heap:

```pascal
procedure New (var P: Pointer);
```


The Pointer P is typed and the amount of memory allocated on the heap matches the type. Deallocation is done with the procedure Dispose. In ObjectPascal constructors and destructors can be passed to New and Dispose correspondingly. The following example is from the rtl docs of [[Free_Pascal]]


```pascal
Program Example16;
{ Program to demonstrate the Dispose and New functions. }
Type
  SS = String[20];
  AnObj = Object
    I : integer;
    Constructor Init;
    Destructor Done;
  end;

Var
  P : ^SS;
  T : ^AnObj;

Constructor Anobj.Init;
begin
  Writeln ( ' Initializing an instance of AnObj! ' );
end;

Destructor AnObj.Done;
begin
  Writeln ( ' Destroying an instance of AnObj! ' ) ;
end;

begin
  New ( P );
  P^ := 'Hello, World!';
  Dispose ( P );
{ P is undefined from here on! }
  New ( T, Init );
  T^.i := 0;
  Dispose ( T, Done );
end .
```


Instead of implicit specification of the amount of memory using a type, the explicit amount can directly specified with the procedure getmem (out p: pointer; Size: PtrUInt);


## Perl 6

Perl 6 is a high level language where, to a first approximation, everything is an object. Perl 6 dynamically allocates memory as objects are created and does automatic garbage collection and freeing of memory as objects go out of scope. There is almost no high level control over how memory is managed, it is considered to be an implementation detail of the virtual machine on which it is running.

If you absolutely must take manual control over memory management you would need to use the foreign function interface to call into a language that provides the capability, but even that would only affect items in the scope of that function, not anything in the mainline process.

There is some ability to specify data types for various objects which allows for (but does not guarantee) more efficient memory layout, but again, it is considered to be an implementation detail, the use that the virtual machine makes of that information varies by implementation maturity and capabilities.


## Phix

Phix applications do not generally need to allocate and free memory explicitly, except for use in ffi, and even then the cffi package or
any of the GUI wrappers can handle most or all of it for you automatically. Both arwen and win32lib (both now superceded by pGUI, and note that that both are 32-bit only, with 4-byte alignment) contain arena storage implementations which may be of interest: see eg allocate_Rect() in demo\arwen\Quick_Allocations.ew, which also offers performance benefits via a circular buffer for short-term use, and also w32new_memset()/w32acquire_mem()/w32release_mem() in win32lib.

The simplest approach however is to rely on automatic memory management (as used by pGUI, and first implemented after arwen and win32lib were originally written):

```Phix
atom mem = allocate(size,true)
```

If the optional cleanup flag is non-zero (or true, as above), the memory is automatically released once it is no longer required
(ie when the variable mem drops out of scope or gets overwritten, assuming you have not made a copy of it elsewhere, which would
all be handled quite properly and seamlessly, with the deallocation not happening until all copies were also overwritten
or discarded), otherwise (ie cleanup is zero or omitted) the application should invoke free() manually.

For completeness, here is a very simplistic arena manager, with just a single pool, not that it would be tricky to implement multiple pools:

```Phix
sequence ap = {}
function ap_allocate(integer size)
-- allocate some memory and add it to the arena pool 'ap' for later release
atom res = allocate(size)
    ap = append(ap,res)
    return res
end function
procedure ap_free()
-- free all memory allocated in arena pool 'ap'
    free(ap)
    ap = {}
end procedure
```



## PicoLisp

PicoLisp allocates any kind of data from a single pool, because everything
is built out of a "cell" primitive. Most of this allocation happens
automatically, but can also be done explicitly with
'[http://software-lab.de/doc/refN.html#new new]' or
'[http://software-lab.de/doc/refB.html#box box]'. For memory-allocated
objects, there is no explicit way of freeing them. Database objects can be
freed with '[http://software-lab.de/doc/refZ.html#zap zap]'.


## PL/I

Allocation of storage other than via routine or block entry is via the ALLOCATE statement applied to variables declared with the CONTROLLED attribute. Such storage is obtained from and returned to a single "heap" storage area during the course of execution and not necessarily corresponding to the entry and exit of routines or blocks. However, variables can further be declared as being BASED on some other variable which might be considered to be a storage area that can be manipulated separately. This can be escalated to being based IN the storage area of a named variable, say POOL. In this situation, storage for items declared IN the POOL are allocated and de-allocated within the storage space of POOL (and there may be insufficient space in the POOL, whereupon the AREA error condition is raised) so this POOL, although obtained from the system heap, is treated as if it were a heap as well.

One reason for doing this is that the addressing of entities within the POOL is relative to the address of the POOL so that pointer variables linking items with the POOL do not employ the momentary machine address of the POOL storage. The point of this is that the contents of a POOL may be saved and read back from a suitable disc file (say at the start of a new execution) and although the memory address of the new POOL may well be different from that during the previous usage, addressing within the new POOL remains the same. In other words, a complex data structure can be developed within the POOL then saved and restored simply by writing the POOL and later reading it back, rather than having to unravel the assemblage in some convention that can be reversed to read it back piece-by-piece. Similarly, if the POOL is a CONTROLLED variable, new POOL areas can be allocated and de-allocated at any time, and by de-allocating a POOL, all of its complex content vanishes in one poof.


## Python

In Python:
* Everything is an object.
* Objects are dynamically allocated.
* Unused objects are garbage collected.

Where objects appear from, or disappear to, is treated as an implementation detail.

Statements, such as assignments, class and function definitions, and import statements can create objects and assign names to them which can be seen as assigning a reference to objects. Objects can also be referred to from other objects e.g. in collections such as lists.

When names go out of scope, or objects are explicitly destroyed, references to objects are diminished. Python's implementation keeps track of references to objects and marks objects that have no remaining references so that they become candidates for '[[wp:Garbage collection (computer science)|garbage collection]]' at a later time.


## Racket


As is common with high-level languages, Racket usually deals with memory automatically. By default, this means using a precise generational GC. However, when there's need for better control over allocation, we can use the <tt>malloc()</tt> function via the FFI, and the many variants that are provided by the GC:

```racket

(malloc 1000 'raw)             ; raw allocation, bypass the GC, requires free()-ing
(malloc 1000 'uncollectable)   ; no GC, for use with other GCs that Racket can be configured with
(malloc 1000 'atomic)          ; a block of memory without internal pointers
(malloc 1000 'nonatomic)       ; a block of pointers
(malloc 1000 'eternal)         ; uncollectable & atomic, similar to raw malloc but no freeing
(malloc 1000 'stubborn)        ; can be declared immutable when mutation is done
(malloc 1000 'interior)        ; allocate an immovable block with possible pointers into it
(malloc 1000 'atomic-interior) ; same for atomic chunks
(malloc-immobile-cell v)       ; allocates a single cell that the GC will not move

```



## REXX

In the REXX language, each (internal and external) procedure has its
own storage (memory) to hold local variables and other information
pertaining to a procedure.
<!-- A newline can be made by twice entering an Enter -->
<!-- ... until someone comes along a deletes the blank line(s), causing extremely long lines.  This has happend numerous times, apparently by well-meaning editors.  Gerard Schildberger. -->

Each call to a procedure (to facilitate recursion) has its own
storage.

Garbage collection can be performed after a procedure finishes
executing (either via an <tt> EXIT, </tt> <tt> RETURN, </tt>
or some other external action),
but this isn't specified in the language.

A   '''drop'''   (a REXX verb) will mark a variable
as not defined, but doesn't necessarily deallocate its storage, but the freed
storage can be used by other variables within the program (or procedure).

Essentially, the method used by a particular REXX interpreter isn't
of concern to a programmer as there is but one type of variable
(character), and even (stemmed) arrays aren't preallocated or even
allocated sequentially in virtual (local) storage (as its elements are
defined).

Some REXX interpreters have built-in functions to query how much free
memory is available (these were written when real storage was a premium
during the early DOS days).

```rexx
/*REXX doesn't have declarations/allocations of variables, */
/*     but this is the closest to an allocation:           */

stemmed_array.= 0    /*any undefined element will have this value. */

stemmed_array.1    = '1st entry'
stemmed_array.2    = '2nd entry'
stemmed_array.6000 = 12 ** 2
stemmed_array.dog  = stemmed_array.6000 / 2

drop stemmed_array.
```



## Rust


```rust
#![feature(rustc_private)]

extern crate arena;

use arena::TypedArena;

fn main() {
    // Memory is allocated using the default allocator (currently jemalloc).  The memory is
    // allocated in chunks, and when one chunk is full another is allocated.  This ensures that
    // references to an arena don't become invalid when the original chunk runs out of space.  The
    // chunk size is configurable as an argument to TypedArena::with_capacity if necessary.
    let arena = TypedArena::new();

    // The arena crate contains two types of arenas: TypedArena and Arena.  Arena is
    // reflection-basd and slower, but can allocate objects of any type.  TypedArena is faster, and
    // can allocate only objects of one type.  The type is determined by type inference--if you try
    // to allocate an integer, then Rust's compiler knows it is an integer arena.
    let v1 = arena.alloc(1i32);

    // TypedArena returns a mutable reference
    let v2 = arena.alloc(3);
    *v2 += 38;
    println!("{}", *v1 + *v2);

    // The arena's destructor is called as it goes out of scope, at which point it deallocates
    // everything stored within it at once.
}
```


## Scala

Today's languages as Scala relies on memory management outside the scope of the application programmer. So memory leaks etc. are no issues anymore. This is governed by a Garbage Collector.


## Tcl

Tcl does not really expose the heap itself, and while it is possible to use [http://www.swig.org/ SWIG] or [[Critcl]] to map the implementation-level allocator into the language, this is highly unusual.

However, it is entirely possible to use a pooled memory manager for Tcl's objects.

{{works with|Tcl|8.6}}

The pool engine class itself (a metaclass):

```tcl
package require Tcl 8.6
oo::class create Pool {
    superclass oo::class
    variable capacity pool busy
    unexport create
    constructor args {
	next {*}$args
	set capacity 100
	set pool [set busy {}]
    }
    method new {args} {
	if {[llength $pool]} {
	    set pool [lassign $pool obj]
	} else {
	    if {[llength $busy] >= $capacity} {
		throw {POOL CAPACITY} "exceeded capacity: $capacity"
	    }
	    set obj [next]
	    set newobj [namespace current]::[namespace tail $obj]
	    rename $obj $newobj
	    set obj $newobj
	}
	try {
	    [info object namespace $obj]::my Init {*}$args
	} on error {msg opt} {
	    lappend pool $obj
	    return -options $opt $msg
	}
	lappend busy $obj
	return $obj
    }
    method ReturnToPool obj {
	try {
	    if {"Finalize" in [info object methods $obj -all -private]} {
		[info object namespace $obj]::my Finalize
	    }
	} on error {msg opt} {
	    after 0 [list return -options $opt $msg]
	    return false
	}
	set idx [lsearch -exact $busy $obj]
	set busy [lreplace $busy $idx $idx]
	if {[llength $pool] + [llength $busy] + 1 <= $capacity} {
	    lappend pool $obj
	    return true
	} else {
	    return false
	}
    }
    method capacity {{value {}}} {
	if {[llength [info level 0]] == 3} {
	    if {$value < $capacity} {
		while {[llength $pool] > 0 && [llength $pool] + [llength $busy] > $value} {
		    set pool [lassign $pool obj]
		    rename $obj {}
		}
	    }
	    set capacity [expr {$value >> 0}]
	} else {
	    return $capacity
	}
    }
    method clearPool {} {
	foreach obj $busy {
	    $obj destroy
	}
    }
    method destroy {} {
	my clearPool
	next
    }
    self method create {class {definition {}}} {
	set cls [next $class $definition]
	oo::define $cls method destroy {} {
	    if {![[info object namespace [self class]]::my ReturnToPool [self]]} {
		next
	    }
	}
	return $cls
    }
}
```

Example of how to use:

```tcl
Pool create PoolExample {
    variable int

    method Init value {
	puts stderr "Initializing [self] with $value"
	set int $value
	incr int 0
    }
    method Finalize {} {
	puts stderr "Finalizing [self] which held $int"
    }

    method value {{newValue {}}} {
	if {[llength [info level 0]] == 3} {
	    set int [incr newValue 0]
	} else {
	    return $int
	}
    }
}

PoolExample capacity 10
set objs {}
try {
    for {set i 0} {$i < 20} {incr i} {
	lappend objs [PoolExample new $i]
    }
} trap {POOL CAPACITY} msg {
    puts "trapped: $msg"
}
puts -nonewline "number of objects: [llength $objs]\n\t"
foreach o $objs {
    puts -nonewline "[$o value] "
}
puts ""
set objs [lassign $objs a b c]
$a destroy
$b destroy
$c destroy
PoolExample capacity 9
try {
    for {} {$i < 20} {incr i} {
	lappend objs [PoolExample new $i]
    }
} trap {POOL CAPACITY} msg {
    puts "trapped: $msg"
}
puts -nonewline "number of objects: [llength $objs]\n\t"
foreach o $objs {
    puts -nonewline "[$o value] "
}
puts ""
PoolExample clearPool
```

Produces this output (red text to <tt>stderr</tt>, black text to <tt>stdout</tt>):
 <span style="color:red">Initializing ::oo::Obj4::Obj5 with 0
 Initializing ::oo::Obj4::Obj6 with 1
 Initializing ::oo::Obj4::Obj7 with 2
 Initializing ::oo::Obj4::Obj8 with 3
 Initializing ::oo::Obj4::Obj9 with 4
 Initializing ::oo::Obj4::Obj10 with 5
 Initializing ::oo::Obj4::Obj11 with 6
 Initializing ::oo::Obj4::Obj12 with 7
 Initializing ::oo::Obj4::Obj13 with 8
 Initializing ::oo::Obj4::Obj14 with 9</span>
 trapped: exceeded capacity: 10
 number of objects: 10
         0 1 2 3 4 5 6 7 8 9
 <span style="color:red">Finalizing ::oo::Obj4::Obj5 which held 0
 Finalizing ::oo::Obj4::Obj6 which held 1
 Finalizing ::oo::Obj4::Obj7 which held 2
 Initializing ::oo::Obj4::Obj6 with 10
 Initializing ::oo::Obj4::Obj7 with 11</span>
 trapped: exceeded capacity: 9
 number of objects: 9
         3 4 5 6 7 8 9 10 11
 <span style="color:red">Finalizing ::oo::Obj4::Obj8 which held 3
 Finalizing ::oo::Obj4::Obj9 which held 4
 Finalizing ::oo::Obj4::Obj10 which held 5
 Finalizing ::oo::Obj4::Obj11 which held 6
 Finalizing ::oo::Obj4::Obj12 which held 7
 Finalizing ::oo::Obj4::Obj13 which held 8
 Finalizing ::oo::Obj4::Obj14 which held 9
 Finalizing ::oo::Obj4::Obj6 which held 10
 Finalizing ::oo::Obj4::Obj7 which held 11</span>


## zkl

Memory allocation "just happens", unreachable memory is recovered via garbage collection. The closest thing to explicit memory allocation is Data object, which is a bit bucket you can [optionally] set the size of upon creation. However, it grows as needed. The closest thing to "new" is the create method, which tells an object to create a new instance of itself. For this task:

```zkl
var pool=List();  // pool could be any mutable container
pool.append(Data(0,1234));   // allocate mem blob and add to pool
pool=Void; // free the pool and everything in it.
```


{{omit from|Clojure}}
{{omit from|Erlang|Erlang does not have a program-controllable heap.}}
{{omit from|Haskell|Haskell does not have a program-controllable heap.}}
{{omit from|Io}}
{{omit from|Lily}}
{{omit from|Logtalk}}
{{omit from|M4}}
{{omit from|Maxima}}
{{omit from|ML/I}}
{{omit from|Oz|Oz does not have a program-controllable heap.}}
{{omit from|TI-89 BASIC|Does not have controlled memory allocation.}}
