+++
title = "Multiple distinct objects"
description = ""
date = 2019-03-31T23:07:03Z
aliases = []
[extra]
id = 3317
[taxonomies]
categories = []
tags = []
+++

{{task}}

Create a [[sequence]] (array, list, whatever) consisting of <var>n</var> distinct, initialized items of the same type. <var>n</var> should be determined at runtime.

By ''distinct'' we mean that if they are mutable, changes to one do not affect all others; if there is an appropriate equality operator they are considered unequal; etc. The code need not specify a particular kind of distinction, but do not use e.g. a numeric-range generator which does not generalize.

By ''initialized'' we mean that each item must be in a well-defined state appropriate for its type, rather than e.g. arbitrary previous memory contents in an array allocation. Do not show only an initialization technique which initializes only to "zero" values (e.g. <tt>calloc()</tt> or <tt>int a[n] = {};</tt> in C), unless user-defined types can provide definitions of "zero" for that type.

This task was inspired by the common error of intending to do this, but instead creating a sequence of <var>n</var> references to the ''same'' mutable object; it might be informative to show the way to do that as well, both as a negative example and as how to do it when that's all that's actually necessary.

This task is most relevant to languages operating in the pass-references-by-value style (most object-oriented, garbage-collected, and/or 'dynamic' languages).

See also: [[Closures/Value capture]]


## Ada


```ada
A : array (1..N) of T;
```

Here N can be unknown until run-time. T is any constrained type. In [[Ada]] all objects are always initialized, though some types may have null initialization. When T requires a non-null initialization, it is done for each array element. For example, when T is a [[task]] type, N tasks start upon initialization of A. Note that T can be a ''limited'' type like task. Limited types do not have predefined copy operation. Arrays of non-limited types can also be initialized by aggregates of:

```ada
A : array (1..N) of T := (others => V);
```

Here V is some value or expression of the type T. As an expression V may have side effects, in that case it is evaluated exactly N times, though the order of evaluation is not defined. Also an aggregate itself can be considered as a solution of the task:

```ada
(1..N => V)
```



## Aime


```aime
void
show_sublist(list l)
{
    integer i, v;

    for (i, v in l) {
        o_space(sign(i));
        o_integer(v);
    }
}

void
show_list(list l)
{
    integer i;
    list v;

    for (i, v in l) {
        o_text(" [");
        show_sublist(v);
        o_text("]");
    }

    o_byte('\n');
}

list
multiple_distinct(integer n, object o)
{
    list l;

    call_n(n, l_append, l, o);

    return l;
}

integer
main(void)
{
    list l, z;

    # create a list of integers - `3' will serve as initializer
    l = multiple_distinct(8, 3);

    l_clear(l);

    # create a list of distinct lists - `z' will serve as initializer
    l_append(z, 4);
    l = multiple_distinct(8, z);

    # modify one of the sublists
    l_q_list(l, 3)[0] = 7;

    # display the list of lists
    show_list(l);

    return 0;
}
```

{{out}}

```txt
 [4] [4] [4] [7] [4] [4] [4] [4]
```



## ALGOL 68

{{trans|python}}

{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386}}

```algol68
MODE FOO = STRUCT(CHAR u,l);
INT n := 26;
[n]FOO f;

# Additionally each item can be initialised #
FOR i TO UPB f DO f[i] := (REPR(ABS("A")-1+i), REPR(ABS("a")-1+i))  OD;

print((f, new line))
```

Output:

```txt

AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz

```



## ALGOL W


```algolw
begin
    record    T  ( integer n, m );
    reference(T) singleT;
    integer      numberOfElements;
    singleT          := T( 0, 0 );
    numberOfElements := 3;
    begin
        reference(T) array tArray ( 1 :: numberOfElements );
        % initialise the "right" way            %
        for i := 1 until numberOfElements do begin
            tArray( i )    := T( i, i * 2 );
            m(tArray( i )) := m(tArray( i )) + 1;
        end for_i ;
        write();
        for i := 1 until numberOfElements do writeon( i_w := 1, s_w := 0, n(tArray( i )), ", ", m(tArray( i )), "; " );
        % initialise the "wrong" way            %
        for i := 1 until numberOfElements do begin
            tArray( i )    := singleT;
            m(tArray( i )) := m(tArray( i )) + 1;
        end for_i ;
        write();
        for i := 1 until numberOfElements do writeon( i_w := 1, s_w := 0, n(tArray( i )), ", ", m(tArray( i )), "; " )
    end
end.
```

{{out}}

```txt

1, 3; 2, 5; 3, 7;
0, 3; 0, 3; 0, 3;

```



## AppleScript


```AppleScript
-- MULTIPLE DISTINCT OBJECTS -------------------------------------------------

-- nObjects Constructor -> Int -> [Object]
on nObjects(f, n)
    map(f, enumFromTo(1, n))
end nObjects

-- TEST ----------------------------------------------------------------------
on run
    -- someConstructor :: a -> Int -> b
    script someConstructor
        on |λ|(_, i)
            {index:i}
        end |λ|
    end script

    nObjects(someConstructor, 6)

    --> {{index:1}, {index:2}, {index:3}, {index:4}, {index:5}, {index:6}}
end run

-- GENERIC FUNCTIONS ---------------------------------------------------------

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if m > n then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    repeat with i from m to n by d
        set end of lst to i
    end repeat
    return lst
end enumFromTo

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn
```

{{Out}}

```AppleScript
{{index:1}, {index:2}, {index:3}, {index:4}, {index:5}, {index:6}}
```



## AutoHotkey

{{works with|AutoHotkey_L}}

```AutoHotkey
a := []
Loop, %n%
   a[A_Index] := new Foo()
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      REM Determine object count at runtime:
      n% = RND(1000)

      REM Declare an array of structures; all members are initialised to zero:
      DIM objects{(n%) a%, b$}

      REM Initialise the objects to distinct values:
      FOR i% = 0 TO DIM(objects{()},1)
        objects{(i%)}.a% = i%
        objects{(i%)}.b$ = STR$(i%)
      NEXT

      REM This is how to create an array of pointers to the same object:
      DIM objects%(n%), object{a%, b$}
      FOR i% = 0 TO DIM(objects%(),1)
        objects%(i%) = object{}
      NEXT
```



## Brat

The wrong way, which creates an array of ''n'' references to the same new ''foo'':


```brat>n.of foo.new</lang


The right way, which calls the block ''n'' times and creates an array of new ''foo''s:


```brat
n.of { foo.new }
```



## C


```c
foo *foos = malloc(n * sizeof(*foos));
for (int i = 0; i < n; i++)
  init_foo(&foos[i]);
```


(Or if no particular initialization is needed, skip that part, or use <tt>calloc</tt>.)


## C++

By default C++ has value semantics, so this problem does not present itself unless the programmer deliberately choses to refer to objects though a pointer. Examples are given for both cases.

Using only language primitives:

```cpp
// this assumes T is a default-constructible type (all built-in types are)
T* p = new T[n]; // if T is POD, the objects are uninitialized, otherwise they are default-initialized

//If default initialisation is not what you want, or if T is a POD type which will be uninitialized
for(size_t i = 0; i != n; ++i)
   p[i] = make_a_T(); //or some other expression of type T

// when you don't need the objects any more, get rid of them
delete[] p;
```


Using the standard library

```cpp
#include <vector>
#include <algorithm>
#include <iterator>

// this assumes T is default-constructible
std::vector<T> vec1(n); // all n objects are default-initialized

// this assumes t is a value of type T (or a type which implicitly converts to T)
std::vector<T> vec2(n, t); // all n objects are copy-initialized with t

// To initialise each value differently
std::generate_n(std::back_inserter(vec), n, makeT); //makeT is a function of type T(void)

```


In C++ reference semantics are achieved by holding objects by pointer. Here is an example of the error, and a correct way of achieving distinctness.

These examples assume T has a public copy constructor, and that p is a pointer to T;


```cpp
#include <vector>
#include <tr1/memory>
using namespace std;
using namespace std::tr1;

typedef shared_ptr<T> TPtr_t;
// the following is NOT correct:
std::vector<TPtr_t > bvec_WRONG(n, p); // create n copies of p, which all point to the same opject p points to.

// nor is this:
std::vector<TPtr_t> bvec_ALSO_WRONG(n, TPtr_t(new T(*p)) ); // create n pointers to a single copy of *p

// the correct solution
std::vector<TPtr_t > bvec(n);
for (int i = 0; i < n; ++i)
  bvec[i] = TPtr_t(new T(*p); //or any other call to T's constructor

// another correct solution
// this solution avoids uninitialized pointers at any point
std::vector<TPtr_t> bvec2;
for (int i = 0; i < n; ++i)
  bvec2.push_back(TPtr_t(new T(*p));


```

Of course, also in this case one can use the other sequence containers or plain new/delete instead of <tt>vector</tt>.

## C#


```c#
using System;
using System.Linq;
using System.Collections.Generic;

List<Foo> foos = Enumerable.Range(1, n).Select(x => new Foo()).ToList();
```



## Clojure

An example using pseudo-random numbers:

```clojure>user
 (take 3 (repeat (rand))) ; repeating the same random number three times
(0.2787011365537204 0.2787011365537204 0.2787011365537204)
user> (take 3 (repeatedly rand)) ; creating three different random number
(0.8334795669220695 0.08405601245793926 0.5795448744634744)
user>
```



## Common Lisp

The mistake is often written as one of these:

```lisp
(make-list n :initial-element (make-the-distinct-thing))
(make-array n :initial-element (make-the-distinct-thing))
```

which are incorrect since the form <code>(make-the-distinct-thing)</code> is only evaluated once and the single object is put in every position of the sequence. A commonly used correct version is:

```lisp
(loop repeat n collect (make-the-distinct-thing))
```

which evaluates <code>(make-the-distinct-thing)</code> <var>n</var> times and collects each result in a list.

It is also possible to use <code>[http://www.lispworks.com/documentation/HyperSpec/Body/f_map_in.htm map-into]</code>, the destructive map operation, to do this since it may take zero input sequences; this method can produce any sequence type, such as a vector (array) rather than a list, and takes a function rather than a form to specify the thing created:


```lisp
(map-into (make-list n) #'make-the-distinct-thing)
(map-into (make-array n) #'make-the-distinct-thing)
```



## D

For reference types (classes):

```d
auto fooArray = new Foo[n];
foreach (ref item; fooArray)
    item = new Foo();

```


For value types:

```d
auto barArray = new Bar[n];
barArray[] = initializerValue;
```



## Delphi


Same object accessed multiple times (bad)

```Delphi
var
  i: Integer;
  lObject: TMyObject;
  lList: TObjectList<TMyObject>;
begin
  lList := TObjectList<TMyObject>.Create;
  lObject := TMyObject.Create;
  for i := 1 to 10 do
    lList.Add(lObject);
  // ...
```


Distinct objects (good)

```Delphi
var
  i: Integer;
  lList: TObjectList<TMyObject>;
begin
  lList := TObjectList<TMyObject>.Create;
  for i := 1 to 10 do
    lList.Add(TMyObject.Create);
  // ...
```



## E


[[Category:E examples needing attention]] E needs development of better map/filter/stream facilities. The easiest way to do this so far is with the accumulator syntax, which is officially experimental because we're not satisfied with it as yet.


```e
pragma.enable("accumulator")
...

accum [] for _ in 1..n { _.with(makeWhatever()) }
```



## EchoLisp


```scheme

;; wrong - make-vector is evaluated one time - same vector

(define L (make-list 3 (make-vector 4)))
L    → (#(0 0 0 0) #(0 0 0 0) #(0 0 0 0))
(vector-set! (first L ) 1 '🔴) ;; sets the 'first' vector

L   → (#(0 🔴 0 0) #(0 🔴 0 0) #(0 🔴 0 0))

;; right - three different vectors

(define L(map make-vector (make-list 3 4)))
L    → (#(0 0 0 0) #(0 0 0 0) #(0 0 0 0))
(vector-set! (first L ) 1 '🔵) ;; sets the first vector

L   → (#(0 🔵 0 0) #(0 0 0 0) #(0 0 0 0)) ;; OK

```



## Elixir


```elixir
randoms = for _ <- 1..10, do: :rand.uniform(1000)
```



## Erlang

List comprehension that will create 20 random integers between 1 and 1000. They will only be equal by accident.

```txt

Randoms = [random:uniform(1000) || _ <- lists:seq(1,10)].

```



## Factor

clone is the important word here to have distinct objects. This creates an array of arrays.

```factor
1000 [ { 1 } clone ] replicate
```



## Forth

{{works with|Forth}}
Works with any ANS Forth

Needs the FMS-SI (single inheritance) library code located here:
http://soton.mpeforth.com/flag/fms/index.html

```forth
include FMS-SI.f
include FMS-SILib.f


\ create a list of VAR objects the right way
\ each: returns a unique object reference
o{ 0 0 0 } dup p:   o{ 0 0 0 }
dup each: drop . 10774016
dup each: drop . 10786896
dup each: drop . 10786912


\ create a list of VAR objects the wrong way
\ each: returns the same object reference
var x
object-list2 list
x list add:
x list add:
x list add:
list p: o{ 0 0 0 }
list each: drop . 1301600
list each: drop . 1301600
list each: drop . 1301600

```




## Fortran



```fortran

program multiple
  ! Define a simple type
  type T
     integer :: a = 3
  end type T

  ! Define a type containing a pointer
  type S
     integer, pointer :: a
  end type S

  type(T), allocatable :: T_array(:)
  type(S), allocatable :: S_same(:)
  integer              :: i
  integer, target      :: v
  integer, parameter   :: N = 10

  ! Create 10
  allocate(T_array(N))

  ! Set the fifth one to b something different
  T_array(5)%a = 1

  ! Print them out to show they are distinct
  write(*,'(10i2)') (T_array(i),i=1,N)

  ! Create 10 references to the same object
  allocate(S_same(N))
  v = 5
  do i=1, N
     allocate(S_same(i)%a)
     S_same(i)%a => v
  end do

  ! Print them out - should all be 5
  write(*,'(10i2)') (S_same(i)%a,i=1,N)

  ! Change the referenced object and reprint - should all be 3
  v = 3
  write(*,'(10i2)') (S_same(i)%a,i=1,N)

end program multiple

```


=={{header|F_Sharp|F#}}==
The wrong way:

```fsharp>
List.replicate 3 (System.Guid.NewGuid());;

val it : Guid list =
  [485632d7-1fd6-4d9e-8910-7949d7b2b485; 485632d7-1fd6-4d9e-8910-7949d7b2b485;
   485632d7-1fd6-4d9e-8910-7949d7b2b485]
```


The right way:

```fsharp>
 List.init 3 (fun _ -> System.Guid.NewGuid());;

val it : Guid list =
  [447acb0c-092e-4f85-9c3a-d369e4539dae; 5f41c04d-9bc0-4e96-8165-76b41fe8cd93;
   1086400c-72ff-4763-9bb9-27e17bd4c7d2]
```


## Go

Useful:

```go
func nxm(n, m int) [][]int {
    d2 := make([][]int, n)
    for i := range d2 {
        d2[i] = make([]int, m)
    }
    return d2
}
```

Probably not what the programmer wanted:

```go
func nxm(n, m int) [][]int {
    d1 := make([]int, m)
    d2 := make([][]int, n)
    for i := range d2 {
        d2[i] = d1
    }
    return d2
}
```



## Groovy


Correct Solution:

```groovy
def createFoos1 = { n -> (0..<n).collect { new Foo() } }
```


Incorrect Solution:

```groovy
// Following fails, creates n references to same object
def createFoos2 = {n -> [new Foo()] * n }
```


Test:

```groovy
[createFoos1, createFoos2].each { createFoos ->
    print "Objects distinct for n = "
    (2..<20).each { n ->
        def foos = createFoos(n)
        foos.eachWithIndex { here, i ->
            foos.eachWithIndex { there, j ->
                assert (here == there) == (i == j)
            }
        }
        print "${n} "
    }
    println()
}
```


Output:

```txt
Objects distinct for n = 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
Objects distinct for n = Caught: Assertion failed:

assert (here == there) == (i == j)
        |    |  |      |   | |  |
        |    |  |      |   0 |  1
        |    |  |      false false
        |    |  Foo@19c8ef56
        |    true
        Foo@19c8ef56
```



## Haskell


Below, we are assuming that <tt>makeTheDistinctThing</tt> is a monadic expression (i.e. it has type <code>m a</code> where <code>m</code> is some monad, like <code>IO</code> or <code>ST</code>), and we are talking about distinctness in the context of the monad. Otherwise, this task is pretty meaningless in Haskell, because Haskell is referentially transparent (so two values that are equal to the same expression are necessarily not distinct) and all values are immutable.

```haskell>replicateM n makeTheDistinctThing</lang

in an appropriate do block. If it is distinguished by, say, a numeric label, one could write

```haskell
mapM makeTheDistinctThing [1..n]
```


An incorrect version:

```haskell
do x <- makeTheDistinctThing
   return (replicate n x)
```


=={{header|Icon}} and {{header|Unicon}}==

An incorrect approach uses, e.g., the list constructor procedure with an initial value:


```Icon

  items_wrong := list (10, [])
  # prints '0' for size of each item
  every item := !items_wrong do write (*item)
  # after trying to add an item to one of the lists
  push (items_wrong[1], 2)
  # now prints '1' for size of each item
  every item := !items_wrong do write (*item)

```


A correct approach initialises each element separately:


```Icon

  items := list(10)
  every i := 1 to 10 do items[i] := []

```



## J


```J>i.</lang


Example use:


```J
   i. 4
0 1 2 3
```


J almost always uses pass-by-value, so this topic is not very relevant to J.

Note also that J offers a variety of other ways of generating multiple distinct objects. This just happens to be one of the simplest of them. In essence, though: generating multiple distinct objects is what J *does* - this is an elemental feature of most of the primitives.


## Java

{{works with|Java|1.5+}}
simple array:

```java
Foo[] foos = new Foo[n]; // all elements initialized to null
for (int i = 0; i < foos.length; i++)
    foos[i] = new Foo();

// incorrect version:
Foo[] foos_WRONG = new Foo[n];
Arrays.fill(foos, new Foo());  // new Foo() only evaluated once
```


simple list:

```java5>List<Foo> foos = new ArrayList<Foo
();
for (int i = 0; i < n; i++)
    foos.add(new Foo());

// incorrect:
List<Foo> foos_WRONG = Collections.nCopies(n, new Foo());  // new Foo() only evaluated once
```


Generic version for class given at runtime:

It's not pretty but it gets the job done. The first method here is the one that does the work. The second method is a convenience method so that you can pass in a <tt>String</tt> of the class name. When using the second method, be sure to use the full class name (ex: "java.lang.String" for "String"). <tt>InstantiationException</tt>s will be thrown when instantiating classes that you would not normally be able to call <tt>new</tt> on (abstract classes, interfaces, etc.). Also, this only works on classes that have a no-argument constructor, since we are using <code>newInstance()</code>.

```java5>public static <E> List<E
 getNNewObjects(int n, Class<? extends E> c){
	List<E> ans = new LinkedList<E>();
	try {
		for(int i=0;i<n;i++)
			ans.add(c.newInstance());//can't call new on a class object
	} catch (InstantiationException e) {
		e.printStackTrace();
	} catch (IllegalAccessException e) {
		e.printStackTrace();
	}
	return ans;
}

public static List<Object> getNNewObjects(int n, String className)
throws ClassNotFoundException{
	return getNNewObjects(n, Class.forName(className));
}
```



## JavaScript



### ES5



```javascript
var a = new Array(n);
for (var i = 0; i < n; i++)
  a[i] = new Foo();
```




### ES6



```JavaScript
(n => {

    let nObjects = n => Array.from({
            length: n + 1
        }, (_, i) => {
            // optionally indexed object constructor
            return {
                index: i
            };
        });

    return nObjects(6);

})(6);
```



{{Out}}

```JavaScript
[{"index":0}, {"index":1}, {"index":2}, {"index":3},
{"index":4}, {"index":5}, {"index":6}]
```



## Julia

A potential mistake would be writing:

```Julia

foo() = rand()             # repeated calls change the result with each call
repeat([foo()], outer=5)   # but this only calls foo() once, clones that first value

```

If the effect of calling foo() with every iteration is desired, better to use:

```Julia

[foo() for i in 1:5]       # Code this to call the function within each iteration

```



## jq

jq does not have mutable data types, and therefore in the context of jq, the given task is probably of little interest. However, it is possible to fulfill the task requirements for jq types other than "null" and "boolean":
```jq

def Array(atype; n):
  if   atype == "number" then [ range(0;n) ]
  elif atype == "object" then [ range(0;n)| {"value": . } ]
  elif atype == "array"  then [ range(0;n)| [.] ]
  elif atype == "string" then [ range(0;n)| tostring ]
  elif atype == "boolean" then
    if n == 0 then [] elif n == 1 then [false] elif n==2 then [false, true]
    else error("there are only two boolean values")
    end
  elif atype == "null" then
    if n == 0 then [] elif n == 1 then [null]
    else error("there is only one null value")
    end
  else error("\(atype) is not a jq type")
  end;

# Example:

 Array("object"; 4)
```



## Kotlin


```scala
// version 1.1.2

class Foo {
    val id: Int

    init {
       id = ++numCreated // creates a distict id for each object
    }

    companion object {
        private var numCreated = 0
    }
}

fun main(args: Array<String>) {
    val n = 3  // say

    /* correct approach - creates references to distinct objects */
    val fooList = List(n) { Foo() }
    for (foo in fooList) println(foo.id)

    /* incorrect approach - creates references to same object */
    val f = Foo()
    val fooList2 = List(n) { f }
    for (foo in fooList2) println(foo.id)
}
```


{{out}}

```txt

1
2
3
4
4
4

```



## Logtalk

Using prototypes, we first dynamically create a protocol to declare a predicate and then create ten prototypes implementing that protocol, which one with a different definition for the predicate:

```logtalk

| ?- create_protocol(statep, [], [public(state/1)]),
     findall(
         Id,
         (integer::between(1, 10, N),
          create_object(Id, [implements(statep)], [], [state(N)])),
         Ids
     ).
Ids = [o1, o2, o3, o4, o5, o6, o7, o8, o9, o10].

```

Using classes, we first dynamically create a class (that is its own metaclass) to declare a predicate (and define a default value for it) and then create ten instances of the class, which one with a different definition for the predicate:

```logtalk

| ?- create_object(state, [instantiates(state)], [public(state/1)], [state(0)]),
     findall(
         Id,
         (integer::between(1, 10, N),
          create_object(Id, [instantiates(state)], [], [state(N)])),
         Ids
     ).
Ids = [o1, o2, o3, o4, o5, o6, o7, o8, o9, o10].

```



## Lua


```Lua
-- This concept is relevant to tables in Lua
local table1 = {1,2,3}

-- The following will create a table of references to table1
local refTab = {}
for i = 1, 10 do refTab[i] = table1 end

-- Instead, tables should be copied using a function like this
function copy (t)
    local new = {}
    for k, v in pairs(t) do new[k] = v end
    return new
end

-- Now we can create a table of independent copies of table1
local copyTab = {}
for i = 1, 10 do copyTab[i] = copy(table1) end
```



## M2000 Interpreter


```M2000 Interpreter

Module CheckIt {
      Form 60, 40
      Foo=Lambda Id=1 (m)->{
            class Alfa {
                  x, id
                  Class:
                  Module Alfa(.x, .id) {}
            }
            =Alfa(m, id)
            id++
      }

      Dim A(10)<<Foo(20)
      \\ for each arrayitem call Foo(20)
      TestThis()


      \\  call once foo(20) and result copy to each array item
      Dim A(10)=Foo(20)
      TestThis()

      Bar=Lambda Foo (m)->{
            ->Foo(m)
      }
      \\ Not only the same id, but the same group
      \\ each item is pointer to group
      Dim A(10)=Bar(20)
      TestThis()

      Sub TestThis()
            Local i
            For i=0 to 9 {
                  For A(i){
                        .x++
                         Print .id , .x
                  }
            }
            Print
      End Sub
}
Checkit

```



## Mathematica

The mistake is often written as:

```Mathematica
{x, x, x, x} /. x -> Random[]
```


Here Random[] can be any expression that returns a new value which is incorrect since Random[] is only evaluated once. e.g.
{0.175125, 0.175125, 0.175125, 0.175125}

A correct version is:


```Mathematica
{x, x, x, x} /. x :> Random[]
```


which evaluates Random[] each time e.g.
->{0.514617, 0.0682395, 0.609602, 0.00177382}


## Maxima


```maxima
a: [1, 2]$

b: makelist(copy(a), 3);
[[1,2],[1,2],[1,2]]

b[1][2]: 1000$

b;
[[1,1000],[1,2],[1,2]]
```


=={{header|Modula-3}}==
Similar to the [[Ada]] version above:

```modula3
VAR a: ARRAY[1..N] OF T
```

This creates an array of distinct elements of type <code>T</code>. A type may specify a default value for its fields, so long as the values are compile-time constants. Similarly, an array can initialize its entries to multiple different values, also compile-time constants. Naturally, a program may initialize this data at run-time using a <code>FOR</code> loop.

Modula-3 offers reference and pointer types, so the mistaken way of initializing is quite easy to do for the careless.

The example program below demonstrates each of these methods, including the mistaken way, so is a bit long.

```modula3
MODULE DistinctObjects EXPORTS Main;

IMPORT IO, Random;

VAR

  random := NEW(Random.Default).init();

TYPE

  T = RECORD (* value will initialize to 2 unless otherwise specified *)
    value: INTEGER := 2;
  END;

CONST Size = 3;

VAR

  (* initialize records *)
  t1 := T { 3 };
  t2 := T { 4 };
  t3 :  T;       (* t3's value will be default (2) *)

  (* initialize a reference to T with value 100 *)
  tr := NEW(REF T, value := 100);

  (* initialize an array of records *)
  a := ARRAY[1..Size] OF T { t1, t2, t3 };
  (* initialize an array of integers *)
  b := ARRAY[1..Size] OF INTEGER { -9, 2, 6 };
  (* initialize an array of references to a record -- NOT copied! *)
  c := ARRAY[1..Size] OF REF T { tr, tr, tr };

BEGIN

  (* display the data *)
  FOR i := 1 TO Size DO
    IO.PutInt(a[i].value); IO.Put(" , ");
    IO.PutInt(b[i]);       IO.Put(" , ");
    IO.PutInt(c[i].value); IO.Put(" ; ");
  END;
  IO.PutChar('\n');

  (* re-initialize a's data to random integers *)
  FOR i := 1 TO Size DO a[i].value := random.integer(-10, 10); END;
  (* modify "one" element of c *)
  c[1].value := 0;
  (* display the data *)
  FOR i := 1 TO Size DO
    IO.PutInt(a[i].value); IO.Put(" , ");
    IO.PutInt(b[i]);       IO.Put(" , ");
    IO.PutInt(c[i].value); IO.Put(" ; ");
  END;
  IO.PutChar('\n');

END DistinctObjects.
```

{{out}}
Each line interleaves the initial values of <code>a</code> and <code>b</code>. The first one has default values; the second replaces the values of <code>a</code> with random, "re-initialized" integers. Only <code>a[3]</code> starts with the default value for <code>T</code>; see the seventh number in the first line. On the other hand, the modification of "one" element of <code>c</code> actually modifies every element, precisely because it is a reference and not an object.

```txt

3 , -9 , 100 ; 4 , 2 , 100 ; 2 , 6 , 100 ;
-1 , -9 , 0 ; -9 , 2 , 0 ; 8 , 6 , 0 ;

```



## NGS


Incorrect, same object n times:

```NGS
{ [foo()] * n }
```


Correct:

```NGS
{ foo * n }
```



## Nim

The simplest form of initialization works, but is a bit cumbersome to write:

```nim
proc foo(): string =
  echo "Foo()"
  "mystring"

let n = 100
var ws = newSeq[string](n)
for i in 0 .. <n: ws[i] = foo()
```

If actual values instead of references are stored in the sequence, then objects can be initialized like this. Objects are distinct, but the initializer <code>foo()</code> is called only once, then copies of the resulting object are made:

```nim
proc newSeqWith[T](len: int, init: T): seq[T] =
  result = newSeq[T] len
  for i in 0 .. <len:
    result[i] = init

var xs = newSeqWith(n, foo())
```

To get the initial behaviour, where <code>foo()</code> is called to create each object, a template can be used:

```nim
template newSeqWith2(len: int, init: expr): expr =
  var result {.gensym.} = newSeq[type(init)](len)
  for i in 0 .. <len:
    result[i] = init
  result

var ys = newSeqWith2(n, foo())
```



## OCaml

For arrays:

Incorrect:

```ocaml
Array.make n (new foo);;
(* here (new foo) can be any expression that returns a new object,
   record, array, or string *)
```

which is incorrect since <code>new foo</code> is only evaluated once. A correct version is:

```ocaml
Array.init n (fun _ -> new foo);;
```



## Oforth


The right way : the block sent as parameter is performed n times :


```Oforth
ListBuffer init(10, #[ Float rand ]) println
```


{{out}}

```txt

[0.281516067014556, 0.865269004241814, 0.101437334065733, 0.924166132625347, 0.88135127712
167, 0.176233635448137, 0.963837773505447, 0.570264579328023, 0.385577832707742, 0.9086026
42741616]

```


The "wrong" way : the same value is stored n times into the list buffer


```Oforth
ListBuffer initValue(10, Float rand) println
```


{{out}}

```txt

[0.314870762000671, 0.314870762000671, 0.314870762000671, 0.314870762000671, 0.31487076200
0671, 0.314870762000671, 0.314870762000671, 0.314870762000671, 0.314870762000671, 0.314870
762000671]

```



## ooRexx


```ooRexx
-- get an array of directory objects
array = fillArrayWith(3, .directory)
say "each object will have a different identityHash"
say
loop d over array
    say d d~identityHash
end

::routine fillArrayWith
  use arg size, class

  array = .array~new(size)
  loop i = 1 to size
      -- Note, this assumes this object class can be created with
      -- no arguments
      array[i] = class~new
  end

return array
```

{{out}}

```txt
each object will have a different identityHash

a Directory -140687820712417
a Directory -140687820713425
a Directory -140687820714369
```



## Oz

With lists, it is difficult to do wrong.

```oz
declare
  Xs = {MakeList 5} %% a list of 5 unbound variables
in
  {ForAll Xs OS.rand} %% fill it with random numbers (CORRECT)
  {Show Xs}
```


With arrays on the other hand, it is easy to get wrong:

```oz
declare
  Arr = {Array.new 0 10 {OS.rand}} %% WRONG: contains ten times the same number
in
  %% CORRECT: fill it with ten (probably) different numbers
  for I in {Array.low Arr}..{Array.high Arr} do
     Arr.I := {OS.rand}
  end
```



## Pascal

See [[Multiple_distinct_objects#Delphi | Delphi]]


## Perl

incorrect:

```perl
(Foo->new) x $n
# here Foo->new can be any expression that returns a reference representing
# a new object
```

which is incorrect since <code>Foo->new</code> is only evaluated once.

A correct version is:

```perl
map { Foo->new } 1 .. $n;
```

which evaluates <tt>Foo->new</tt> <var>$n</var> times and collects each result in a list.


## Perl 6


Unlike in Perl 5, the list repetition operator evaluates the left argument thunk each time, so


```perl6
my @a = Foo.new xx $n;
```


produces <code>$n</code> distinct objects.


## Phix

Phix uses shared reference counts with copy-on-write semantics. Creating n references to the same mutable object is in fact the norm,
but does not cause any of the issues implicitly feared in the task description. In fact, it is not possible to create shared references
such that when one is updated they all are, instead store an index to another table that stores the object, rather than the object itself. Also, apart from low-level trickery and interfacing to shared libraries, there are no pointers to normal hll objects. Sequences need not be homogeneous, they can contain any type-mix of elements.

```Phix
sequence s = repeat("x",3*rand(3))
?s
    s[rand(length(s))] = 5
?s
    s[rand(length(s))] &= 'y'
?s
    s[rand(length(s))] = s
?s
```

{{out}}

```txt

{"x","x","x","x","x","x"}
{"x","x","x","x","x",5}
{"xy","x","x","x","x",5}
{"xy",{"xy","x","x","x","x",5},"x","x","x",5}

```

Note that the last statement did not create a circular structure, something that is not possible in Phix, except via index-emulation.

I suppose it is possible that someone could write

```Phix
sequence s = repeat(my_func(),5)
```

and expect my_func() to be invoked 5 times, but for that you need a loop

```Phix
sequence s = repeat(0,5)
for i=1 to length(s) do
    s[i] = my_func()
end for
```



## PicoLisp

Create 5 distinct (empty) objects:

```PicoLisp
: (make (do 5 (link (new))))
-> ($384717187 $384717189 $384717191 $384717193 $384717195)
```

Create 5 anonymous symbols with the values 1 .. 5:

```PicoLisp
: (mapcar box (range 1 5))
-> ($384721107 $384721109 $384721111 $384721113 $384721115)
: (val (car @))
-> 1
: (val (cadr @@))
-> 2
```



## PowerShell

Do some randomization that could easily return three equal values (but each value is a separate value in the array):

```PowerShell

1..3 | ForEach-Object {((Get-Date -Hour ($_ + (1..4 | Get-Random))).AddDays($_ + (1..4 | Get-Random)))} |
       Select-Object -Unique |
       ForEach-Object {$_.ToString()}

```

{{Out}}

```txt

11/18/2016 3:32:16 AM
11/21/2016 3:32:16 AM
11/22/2016 7:32:16 AM

```

Run the same commands a few times and the <code>Select-Object -Unique</code> command filters equal (but separate values):

```PowerShell

1..3 | ForEach-Object {((Get-Date -Hour ($_ + (1..4 | Get-Random))).AddDays($_ + (1..4 | Get-Random)))} |
       Select-Object -Unique |
       ForEach-Object {$_.ToString()}

```

{{Out}}

```txt

11/18/2016 4:32:17 AM
11/21/2016 5:32:17 AM

```



## PureBasic


```PureBasic
n=Random(50)+25
Dim A.i(n)
; Creates a Array of n [25-75] elements depending on the outcome of Random().
; Each element will be initiated to zero.

For i=0 To ArraySize(A())
  A(i)=2*i
Next i
; Set each individual element at a wanted (here 2*i) value and
; automatically adjust accordingly to the unknown length of the Array.

NewList *PointersToA()
For i=0 To ArraySize(A())
  AddElement(*PointersToA())
  *PointersToA()=@A(i)
Next
; Create a linked list of the same length as A() above.
; Each element is then set to point to the Array element
; of the same order.

ForEach *PointersToA()
  Debug PeekI(*PointersToA())
Next
; Verify by sending each value of A() via *PointersToA()
; to the debugger's output.
```



## Python

The mistake is often written as:

```python
[Foo()] * n # here Foo() can be any expression that returns a new object
```

which is incorrect since <tt>Foo()</tt> is only evaluated once. A common correct version is:

```python
[Foo() for i in range(n)]
```

which evaluates <tt>Foo()</tt> <var>n</var> times and collects each result in a list. This last form is also discussed [[Two-dimensional array (runtime)#Python|here]], on the correct construction of a two dimensional array.


## R

The mistake is often written as:

```r
rep(foo(), n)        # foo() is any code returning a value
```

A common correct version is:

```r
replicate(n, foo())
```

which evaluates foo() n times and collects each result in a list.  (Using simplify=TRUE lets the function return an array, where possible.)


## Racket



```racket

#lang racket

;; a list of 10 references to the same vector
(make-list 10 (make-vector 10 0))

;; a list of 10 distinct vectors
(build-list 10 (λ (n) (make-vector 10 0)))

```



## Ruby

The mistake is often written as one of these:

```ruby
[Foo.new] * n         # here Foo.new can be any expression that returns a new object
Array.new(n, Foo.new)
```

which are incorrect since <code>Foo.new</code> is only evaluated once, and thus you now have <var>n</var> references to the ''same'' object. A common correct version is:

```ruby
Array.new(n) { Foo.new }
```

which evaluates <code>Foo.new</code> <var>n</var> times and collects each result in an Array. This last form is also discussed [[Two-dimensional array (runtime)#Ruby|here]], on the correct construction of a two dimensional array.


## Rust


```Rust
use std::rc::Rc;
use std::cell::RefCell;

fn main() {
    let size = 3;

    // Clone the given element to fill out the vector.
    let mut v: Vec<String> = vec![String::new(); size];
    v[0].push('a');
    println!("{:?}", v);

    // Run a given closure to create each element.
    let mut v: Vec<String> = (0..size).map(|i| i.to_string()).collect();
    v[0].push('a');
    println!("{:?}", v);

    // For multiple mutable views of the same thing, use something like Rc and RefCell.
    let v: Vec<Rc<RefCell<String>>> = vec![Rc::new(RefCell::new(String::new())); size];
    v[0].borrow_mut().push('a');
    println!("{:?}", v);
}
```

{{out}}

```txt
["a", "", ""]
["0a", "1", "2"]
[RefCell { value: "a" }, RefCell { value: "a" }, RefCell { value: "a" }]
```



## Scala

Yielding a normal class instance here (rather than a case class instance), as case objects are identical
if created with the same constructor arguments.


```scala
for (i <- (0 until n)) yield new Foo()
```



## Scheme

{{libheader|Scheme/SRFIs}}

There is a standard function make-list which makes a list of size n, but repeats its given value.


```txt

sash[r7rs]> (define-record-type <a> (make-a x) a? (x get-x))
#<unspecified>
sash[r7rs]> (define l1 (make-list 5 (make-a 3)))
#<unspecified>
sash[r7rs]> (eq? (list-ref l1 0) (list-ref l1 1))
#t

```


In SRFI 1, a function list-tabulate is provided which instead calls a function to create a fresh value each time.


```txt

sash[r7rs]> (define l2 (list-tabulate 5 (lambda (i) (make-a i))))
#<unspecified>
sash[r7rs]> (eq? (list-ref l2 0) (list-ref l2 1))
#f
sash[r7rs]> (map get-x l2)
(0 1 2 3 4)

```




## Seed7

The example below defines the local array variable ''fileArray''.
The [http://seed7.sourceforge.net/libraries/array.htm#%28in_integer%29times%28in_baseType%29 times] operator creates a new array value with a specified size.
Finally multiple distinct objects are assigned to the array elements.

```seed7
$ include "seed7_05.s7i";

const func array file: openFiles (in array string: fileNames) is func
  result
    var array file: fileArray is 0 times STD_NULL;  # Define array variable
  local
    var integer: i is 0;
  begin
    fileArray := length(fileNames) times STD_NULL;  # Array size computed at run-time
    for key i range fileArray do
      fileArray[i] := open(fileNames[i], "r");      # Assign multiple distinct objects
    end for;
  end func;

const proc: main is func
  local
    var array file: files is 0 times STD_NULL;
  begin
    files := openFiles([] ("abc.txt", "def.txt", "ghi.txt", "jkl.txt"));
  end func;
```



## Sidef


```ruby
[Foo.new] * n;      # incorrect (only one distinct object is created)
```


```ruby
n.of {Foo.new};     # correct
```



## Smalltalk



```smalltalk
|c|
"Create an ordered collection that will grow while we add elements"
c := OrderedCollection new.
"fill the collection with 9 arrays of 10 elements; elements (objects)
 are initialized to the nil object, which is a well-defined 'state'"
1 to: 9 do: [ :i | c add: (Array new: 10) ].
"However, let us show a way of filling the arrays with object number 0"
c := OrderedCollection new.
1 to: 9 do: [ :i | c add: ((Array new: 10) copyReplacing: nil withObject: 0) ].
"demonstrate that the arrays are distinct: modify the fourth of each"
1 to: 9 do: [ :i | (c at: i) at: 4 put: i ].
"show it"
c do: [ :e | e printNl ].
```



## Swift


```swift
class Foo { }

var foos = [Foo]()
for i in 0..<n {
    foos.append(Foo())
}

// incorrect version:
var foos_WRONG = [Foo](count: n, repeatedValue: Foo())  // Foo() only evaluated once
```



## Tcl

Tcl values are implemented using copy-on-write reference semantics with no (exposed) mechanism for determining whether two values are really references to the same value, which makes this task relatively moot. However, in the case where there is a collection of objects it becomes important to perform the construction correctly (i.e., repeatedly) otherwise it is just the ''name'' of the object that will be copied when it is written to.

{{works with|Tcl|8.6}} or {{libheader|TclOO}}

```Tcl
package require TclOO

# The class that we want to make unique instances of
set theClass Foo

# Wrong version; only a single object created
set theList [lrepeat $n [$theClass new]]

# Right version; objects distinct
set theList {}
for {set i 0} {$i<$n} {incr i} {
    lappend theList [$theClass new]
}
```



## XPL0


```XPL0
code Reserve=3, IntIn=10;
char A;  int  N, I;
[N:= IntIn(8);                  \get number of items from command line
A:= Reserve(N);                 \create array of N bytes
for I:= 0 to N-1 do A(I):= I*3; \initialize items with different values
for I:= 0 to N-1 do A:= I*3;    \error: "references to the same mutable object"
]
```



## Yabasic


```Yabasic
sub test()
	print "Random number: " + str$(ran(100))
end sub

sub repL$(e$, n)
	local i, r$

	for i = 1 to n
		r$ = r$ + "," + e$
	next
	return r$
end sub

dim func$(1)

n = token(repL$("test", 5), func$(), ",")

for i = 1 to n
	execute(func$(i))
next i
```



## zkl

The pump and partial application methods are useful tools for creating initialized lists.

```zkl
n:=3;
n.pump(List) //-->L(0,1,2)

n.pump(List,List) //-->L(0,1,2), not expected
  because the second list can be used to describe a calculation
n.pump(List,List(Void,List)) //--> L(L(),L(),L()) all same
   List(Void,List) means returns List, which is a "known" value
n.pump(List,List.fpM("-")) //--> L(L(),L(),L()) all distinct
   fpM is partial application: call List.create()

n.pump(List,(0.0).random.fp(1)) //--> 3 [0,1) randoms
L(0.902645,0.799657,0.0753809)

n.pump(String) //-->"012", default action is id function

class C{ var n; fcn init(x){n=x} }
n.pump(List,C) //--> L(C,C,C)
n.pump(List,C).apply("n") //-->L(0,1,2) ie all classes distinct
```



{{omit from|GUISS}}
{{omit from|PARI/GP}}
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have mutable objects, so not applicable. -->
