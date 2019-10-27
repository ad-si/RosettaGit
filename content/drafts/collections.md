+++
title = "Collections"
description = ""
date = 2019-10-17T15:01:17Z
aliases = []
[extra]
id = 1635
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}
[[Category:Data Structures]]
{{clarified-review}}

Collections are abstractions to represent sets of values.  

In statically-typed languages, the values are typically of a common data type.


;Task:
Create a collection, and add a few values to it.


{{Template:See also lists}}





## ABAP



```ABAP

REPORT z_test_rosetta_collection.

CLASS lcl_collection DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.
ENDCLASS.

CLASS lcl_collection IMPLEMENTATION.
  METHOD start.
    DATA(itab) = VALUE int4_table( ( 1 ) ( 2 ) ( 3 ) ).

    cl_demo_output=>display( itab ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_collection( )->start( ).

```



## Ada

Ada 95 and earlier offers arrays.  Ada 2005 adds the Ada.Containers package and its children.  Examples of Doubly Linked Lists and Vectors are given.  
Ada 2005 also provides hashed and ordered Maps and Sets (not shown).


### anonymous arrays

In Ada, arrays can be indexed on any range of discrete values.  The example below creates an anonymous array indexed from -3 to -1.  It initializes the three elements of the array at declaration.  Then it reverses their order in the array.  



Anonymous arrays have no type associated with them that is accessible to the programmer.  This means that anonymous arrays cannot be compared in the aggregate to other arrays (even those with the same index structure and contained type) or passed as a parameter to a subprogram.  For these reasons, anonymous arrays are best used as singletons and global constants.


```Ada
procedure Array_Collection is

   A : array (-3 .. -1) of Integer := (1, 2, 3);
   
begin
   
   A (-3) := 3;
   A (-2) := 2;
   A (-1) := 1;
   
end Array_Collection;
```



### array types

Because of the limitations of anonymous arrays noted above, arrays are more typically defined in Ada as array types, as in the example below.


```Ada
procedure Array_Collection is

   type Array_Type is array (1 .. 3) of Integer;
   A : Array_Type := (1, 2, 3);
   
begin
   
   A (1) := 3;
   A (2) := 2;
   A (3) := 1;
   
end Array_Collection;
```



### unconstrained arrays

Dynamic arrays can be created through the use of pointers to unconstrained arrays.  While an unconstrained array's index type is defined, it does not have a pre-defined range of indices - they are specified at the time of declaration or, as would be the case in a dynamic array, at the time the memory for the array is allocated.  The creation of a dynamic array is not shown here, but below is an example declaration of an unconstrained array in Ada.


```Ada
procedure Array_Collection is

   type Array_Type is array (positive range <>) of Integer; -- may be indexed with any positive
                                                            -- Integer value
   A : Array_Type(1 .. 3);  -- creates an array of three integers, indexed from 1 to 3
   
begin
   
   A (1) := 3;
   A (2) := 2;
   A (3) := 1;
   
end Array_Collection;
```



### doubly linked lists


{{works with|Ada 2005}}
{{libheader|Ada.Containers.Doubly_Linked_Lists}}


```Ada
with Ada.Containers.Doubly_Linked_Lists;
use  Ada.Containers;

procedure Doubly_Linked_List is

   package DL_List_Pkg is new Doubly_Linked_Lists (Integer);
   use     DL_List_Pkg;
   
   DL_List : List;
   
begin
   
   DL_List.Append (1);
   DL_List.Append (2);
   DL_List.Append (3);
   
end Doubly_Linked_List;
```



### vectors


{{works with|Ada 2005}}
{{libheader|Ada.Containers.Vectors}}


```Ada
with Ada.Containers.Vectors;
use  Ada.Containers;

procedure Vector_Example is

   package Vector_Pkg is new Vectors (Natural, Integer);
   use     Vector_Pkg;
   
   V : Vector;
   
begin
   
   V.Append (1);
   V.Append (2);
   V.Append (3);
   
end Vector_Example;
```



## Aime

Aime collections include "list"s (sequences) and "record"s (associative arrays).
Both types of collections are heterogenous and resize dynamically.

### Lists

Declaring a list:

```aime>list l;</lang

Adding values to it:

```aime
l_p_integer(l, 0, 7);
l_push(l, "a string");
l_append(l, 2.5);
```

Retrieving values from a list:

```aime
l_query(l, 2)
l_head(l)
l_q_text(l, 1)
l[3]
```



### Records

Declaring a record:

```aime>record r;</lang

Adding values to it:

```aime
r_p_integer(r, "key1", 7);
r_put(r, "key2", "a string");
r["key3"] = .25;
```

Retrieving values from a record: 

```aime
r_query(r, "key1")
r_tail(r)
r["key2"]
```



## ALGOL 68

Arrays are the closest thing to collections available as standard in Algol 68. Collections could be implemented using STRUCTs but there are none as standard. Some examples of arrays:

```algol68
# create a constant array of integers and set its values #
[]INT constant array = ( 1, 2, 3, 4 );
# create an array of integers that can be changed, note the size mst be specified #
# this array has the default lower bound of 1 #
[ 5 ]INT mutable array := ( 9, 8, 7, 6, 5 );
# modify the second element of the mutable array #
mutable array[ 2 ] := -1;
# array sizes are normally fixed when the array is created, however arrays can be #
# declared to be FLEXible, allowing their sizes to change by assigning a new array to them #
# The standard built-in STRING is notionally defined as FLEX[ 1 : 0 ]CHAR in the standard prelude #
# Create a string variable: #
STRING str := "abc";
# assign a longer value to it #
str := "bbc/itv";
# add a few characters to str, +=: adds the text to the beginning, +:= adds it to the end #
"[" +=: str; str +:= "]"; # str now contains "[bbc/itv]" #
# Arrays of any type can be FLEXible: #
# create an array of two integers #
FLEX[ 1 : 2 ]INT fa := ( 0, 0 );
# replace it with a new array of 5 elements #
fa := LOC[ -2 : 2 ]INT;

```



## Apex


### Lists

A list is an ordered collection of elements that are distinguished by their indices
Creating Lists

```apex

// Create an empty list of String
List<String> my_list = new List<String>();
// Create a nested list
List<List<Set<Integer>>> my_list_2 = new List<List<Set<Integer>>>();

```

Access elements in a list

```apex

List<Integer> myList = new List<Integer>(); // Define a new list
myList.add(47);                    // Adds a second element of value 47 to the end 
                                       // of the list
Integer i = myList.get(0);                   // Retrieves the element at index 0
myList.set(0, 1);                           // Adds the integer 1 to the list at index 0
myList.clear();                    // Removes all elements from the list

```

Using Array Notation for One-dimensional list

```apex

String[] colors = new List<String>();
List<String> colors = new String[1];
colors[0] = 'Green';

```



### Sets

A set is an unordered collection of elements that do not contain any duplicates.
Defining a set:

```apex

Set<String> s1 = new Set<String>{'a', 'b + c'}; // Defines a new set with two elements
Set<String> s2 = new Set<String>(s1); // Defines a new set that contains the 
                                     // elements of the set created in the previous step

```

Access elements in a set:

```apex

Set<Integer> s = new Set<Integer>(); // Define a new set
s.add(1);                            // Add an element to the set
System.assert(s.contains(1));        // Assert that the set contains an element
s.remove(1);                         // Remove the element from the set

```

Note the following limitations on sets:
* Unlike Java, Apex developers do not need to reference the algorithm that is used to implement a set in their declarations (for example, HashSet or TreeSet). Apex uses a hash structure for all sets.
* A set is an unordered collection—you can’t access a set element at a specific index. You can only iterate over set elements.
* The iteration order of set elements is deterministic, so you can rely on the order being the same in each subsequent execution of the same code.


### Maps

A map is a collection of key-value pairs where each unique key maps to a single value
Declaring a map:

```apex

Map<String, String> country_currencies = new Map<String, String>();
Map<ID, Set<String>> m = new Map<ID, Set<String>>();
Map<String, String> MyStrings = new Map<String, String>{'a' => 'b', 'c' => 'd'.toUpperCase()};

```

Accessing a Map:

```apex

Map<Integer, String> m = new Map<Integer, String>(); // Define a new map
m.put(1, 'First entry');                  // Insert a new key-value pair in the map
m.put(2, 'Second entry');                  // Insert a new key-value pair in the map
System.assert(m.containsKey(1));  // Assert that the map contains a key
String value = m.get(2);               // Retrieve a value, given a particular key
System.assertEquals('Second entry', value);
Set<Integer> s = m.keySet();       // Return a set that contains all of the keys in the map

```

Map Considerations:
* Unlike Java, Apex developers do not need to reference the algorithm that is used to implement a map in their declarations (for example, HashMap or TreeMap). Apex uses a hash structure for all maps.
* The iteration order of map elements is deterministic. You can rely on the order being the same in each subsequent execution of the same code. However, we recommend to always access map elements by key.
* A map key can hold the null value.
* Adding a map entry with a key that matches an existing key in the map overwrites the existing entry with that key with the new entry.
Map keys of type String are case-sensitive. Two keys that differ only by the case are considered unique and have corresponding distinct Map entries.Subsequently, the Map methods, including put, get, containsKey, and remove treat these keys as distinct.
* Uniqueness of map keys of user-defined types is determined by the equals and hashCode methods, which you provide in your classes. Uniqueness of keys of all other non-primitive types, such as sObject keys, is determined by comparing the objects’ field values.
* A Map object is serializable into JSON only if it uses one of the following data types as a key.
Boolean, Date, DateTime, Decimal, Double, Enum, Id, Integer, Long, String, Time


## Arturo



### Array



```arturo
// initialize array
array #("one" 2 "three" "four")

// add an element to the array
array array + #(5)

print array
```


{{out}}


```txt
#("one" 2 "three" "four" 5)
```



### Dictionary



```arturo
// initialize dictionary
dict #{
	name "john"
	surname "doe"
	age 33

	preferredFood #("fruit" "pizza")
}

// add an element to the dictionary
dict dict + #{ country "Spain" }

print dict
```


{{out}}


```txt
#{
	age             33
	country         "Spain"
	name            "john"
	preferredFood   #(
	                	"fruit"
	                	"pizza"
	                )
	surname         "doe"
}
```



### Class



```arturo
// define a new class-type dictionary 
Person #{
	name ""
	surname ""

	init [n,s]{
		name n
		surname s
	}

	sayHello {
		print "Hello " + name + "!"
	}
}

// create a new Person object and initialize it
person $(new ~Person "John" "Doe")

// use an inner function
!person.sayHello

log person
```


{{out}}


```txt
Hello John!
#{
	init            <function: 0x1057223E0>
	name            "John"
	sayHello        <function: 0x105722440>
	surname         "Doe"
}
```



## AutoHotkey


### Objects

{{works with|AutoHotkey_L}}
[http://l.autohotkey.net/docs/Objects.htm Documentation]

```AutoHotkey
myCol := Object()
mycol.mykey := "my value!"
mycol["mykey"] := "new val!"
MsgBox % mycol.mykey ; new val
```


===Pseudo-arrays===
Documentation: http://www.autohotkey.com/docs/misc/Arrays.htm

```AutoHotkey
Loop 3
   array%A_Index% := A_Index * 9
MsgBox % array1 "  " array2 "  " array3 ; 9  18  27
```


### Structs

Structs are not natively supported in AutoHotkey, however they are often required in DllCalls to C++ Dlls.
This shows how to retrieve values from a RECT structure in AutoHotkey (from the DllCall documentation at http://www.autohotkey.com/docs/commands/DllCall.htm)

```AutoHotkey
VarSetCapacity(Rect, 16)  ; A RECT is a struct consisting of four 32-bit integers (i.e. 4*4=16).
DllCall("GetWindowRect", UInt, WinExist(), UInt, &Rect)  ; WinExist() returns an HWND.
MsgBox % "Left " . NumGet(Rect, 0, true) . " Top " . NumGet(Rect, 4, true)
    . " Right " . NumGet(Rect, 8, true) . " Bottom " . NumGet(Rect, 12, true)
```



## AWK

In awk, the closest thing to collections would be arrays. They are created when needed at assignment

```awk
a[0]="hello"
```

or by splitting a string

```awk
split("one two three",a)
```

Single elements are accessible with the bracket notation, like in C:

```awk
print a[0]
```

One can iterate over the elements of an array:

```awk
for(i in a) print i":"a[i]
```



## Axe


```axe
1→{L₁}
2→{L₁+1}
3→{L₁+2}
4→{L₁+3}
Disp {L₁}►Dec,i
Disp {L₁+1}►Dec,i
Disp {L₁+2}►Dec,i
Disp {L₁+3}►Dec,i
```



## BBC BASIC


### Arrays

In BBC BASIC the only native type of 'collection' is the array; the index starts at zero and the subscript specified in the DIM is the highest value of the index.  Hence in this example an array with two elements is defined:

```bbcbasic
      DIM text$(1)
      text$(0) = "Hello "
      text$(1) = "world!"
```


### Arrays of structures

{{works with|BBC BASIC for Windows}}
When the objects in the collection are not simple scalar types an array of structures may be used:

```bbcbasic
      DIM collection{(1) name$, year%}
      collection{(0)}.name$ = "Richard"
      collection{(0)}.year% = 1952
      collection{(1)}.name$ = "Sue"
      collection{(1)}.year% = 1950
```


### Linked lists

Although not a native language feature, other types of collections such as linked lists may be constructed:

```bbcbasic
      DIM node{name$, year%, link%}
      list% = 0
      PROCadd(list%, node{}, "Richard", 1952)
      PROCadd(list%, node{}, "Sue", 1950)
      PROClist(list%, node{})
      END
      
      DEF PROCadd(RETURN l%, c{}, n$, y%)
      LOCAL p%
      DIM p% DIM(c{})-1
      !(^c{}+4) = p%
      c.name$ = n$
      c.year% = y%
      c.link% = l%
      l% = p%
      ENDPROC
      
      DEF PROClist(l%, c{})
      WHILE l%
        !(^c{}+4) = l%
        PRINT c.name$, c.year%
        l% = c.link%
      ENDWHILE
      ENDPROC
```



## bc

See [[Arrays#bc|Arrays]] for basic operations on arrays, the only collection type in bc.


## C


See Also [[Foreach#C|foreach]]<BR><BR>
One thing in C language proper that can be said to be a collection is array type.
An array has a length known at compile time.

```c
#define cSize( a )  ( sizeof(a)/sizeof(a[0]) ) /* a.size() */
int ar[10];               /* Collection<Integer> ar = new ArrayList<Integer>(10); */
ar[0] = 1;                /* ar.set(0, 1); */
ar[1] = 2;

int* p;                   /* Iterator<Integer> p; Integer pValue; */
for (p=ar;                /* for( p = ar.itereator(), pValue=p.next(); */
       p<(ar+cSize(ar));  /*        p.hasNext(); */
       p++) {             /*        pValue=p.next() ) { */
  printf("%d\n",*p);      /*   System.out.println(pValue); */
}                         /* } */
```

Please note that c built-in pointer-arithmetic support which helps this logic.  An integer may be 4 bytes, and a char 1 byte: the plus operator (+) is overloaded to multiply a incement by 4 for integer pointers and by 1 for char pointers (etc).

Another construct which can be seen as a collection is a malloced array.  The size of a malloced array is not known at compile time.

```c

int* ar;                  /* Collection<Integer> ar; */
int arSize;
arSize = (rand() % 6) + 1; 
ar = calloc(arSize, sizeof(int) ); /* ar = new ArrayList<Integer>(arSize); */
ar[0] = 1;                /* ar.set(0, 1); */

int* p;                   /* Iterator<Integer> p; Integer pValue; */
for (p=ar;                /* p=ar.itereator(); for( pValue=p.next(); */
       p<(ar+arSize);     /*                         p.hasNext(); */
       p++) {             /*                         pValue=p.next() ) { */
  printf("%d\n",*p);      /*   System.out.println(pValue); */
}                         /* }    */
```


A string is another C language construct (when looked at with its standard libraries) that behaves like a collection.
A C language string is an array of char, and it's size may or may not be known at compile time, however a c string is terminated with a ASCII NUL (which may be stated as a constant, '\0' or ((char)0) in the C language).  The String standard library "class" has many "methods", however instead of being called String.method(), they are usually called strmethod().

Arbitrarily complex data structures can be constructed, normally via language features <code>struct</code> and pointers.  They are everywhere, but not provided by the C language itself per se.


## C++

C++ has a range of different collections optimized for different use cases. Note that in C++, objects of user-defined types are mostly treated just like objects of built-in types; especially there's no different treatment for collections. Thus all collections can simply be demonstrated with the built-in type <tt>int</tt>. For user-defined types, just replace int with the user-defined type. Any type which goes into a collection must be copyable and assignable (which in general is automatically the case unless you explicitly disallow it).

Note however that C++ collections store ''copies'' of the objects given to them, so you'll lose any polymorphic behaviour. If you need polymorphism, use a collection of pointers (or smart pointers like boost::shared_ptr).

===built-in array===
The simplest collection in C++ is the built-in array. Built-in arrays have a fixed size, and except for POD types (i.e. basically any type you culd also write in C), the members are all initialized at array creation time (if no explicit initialization is done, the default constructr is used).


```cpp
int a[5]; // array of 5 ints (since int is POD, the members are not initialized)
a[0] = 1; // indexes start at 0

int primes[10] = { 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 }; // arrays can be initialized on creation

#include <string>
std::string strings[4]; // std::string is no POD, therefore all array members are default-initialized
                        // (for std::string this means initialized with empty strings)
```



### vector

A vector is basically a resizable array. It is optimized for adding/removing elements on the end, and fast access to elements anywhere. Inserting elements at the beginning or in the middle is possible, but in general inefficient.


```cpp>#include <vector


std::vector<int> v;       // empty vector
v.push_back(5);           // insert a 5 at the end
v.insert(v.begin(), 7);   // insert a 7 at the beginning
```



### deque

A deque is optimized for appending and removing elements on both ends ofd the array. Accessing random elements is still efficient, but slightly less than with vector.


```cpp>#include <deque


std::deque<int> d;        // empty deque
d.push_back(5);           // insert a 5 at the end
d.push_front(7);          // insert a 7 at the beginning
d.insert(v.begin()+1, 6); // insert a 6 in the middle
```



### list

A list is optimized for insertion at an arbitrary place (provided you already have an iterator pointing to that place). Element access is efficient only in linear order.


```cpp>#include <list


std::list<int> l;         // empty list
l.push_back(5);           // insert a 5 at the end
l.push_front(7);          // insert a 7 at the beginning
std::list::iterator i = l.begin();
++l;
l.insert(i, 6);           // insert a 6 in the middle
```



### set

A set keeps the inserted elements sorted, and also makes sure that each element occurs only once. Of course, if you want to put something into a set, it must be less-than-comparable, i.e. you must be able to compare which of two objects <tt>a</tt> and <tt>b</tt> is smaller using <tt>a<b</tt> (there's also a way to define sets with an user-defined order, in which case this restriction doesn't apply).


```cpp>#include <set


std::set<int> s;          // empty set
s.insert(5);              // insert a 5
s.insert(7);              // insert a 7 (automatically placed after the 5)
s.insert(5);              // try to insert another 5 (will not change the set)
```



### multiset

A multiset is like a set, except the same element may occur multiple times.


```cpp>#include <multiset


std::multiset<int> m;     // empty multiset
m.insert(5);              // insert a 5
m.insert(7);              // insert a 7 (automatically placed after the 5)
m.insert(5);              // insert a second 5 (now m contains two 5s, followed by one 7)
```


=={{header|C sharp|C#}}==


### Arrays


```csharp

// Creates and initializes a new integer Array
int[] intArray = new int[5] { 1, 2, 3, 4, 5 };
//same as
int[] intArray = new int[]{ 1, 2, 3, 4, 5 };
//same as
int[] intArray = { 1, 2, 3, 4, 5 };

//Arrays are zero-based
string[] stringArr = new string[5];
stringArr[0] = "string";

```



### ArrayList and List

The size of ArrayList is dynamically increased as required. ArrayLists are zero-based.

```csharp

//Create and initialize ArrayList
ArrayList myAl = new ArrayList { "Hello", "World", "!" };

//Create ArrayList and add some values
ArrayList myAL = new ArrayList();
      myAL.Add("Hello");
      myAL.Add("World");
      myAL.Add("!");


```


The List class is the generic equivalent of the ArrayList class.
A List is a strongly typed list of objects that can be accessed by index ( zero-based again).

```csharp

//Create and initialize List
List<string> myList = new List<string> { "Hello", "World", "!" };

//Create List and add some values
List<string> myList2 = new List<string>();
            myList2.Add("Hello");
            myList2.Add("World");
            myList2.Add("!");

```



### Hashtable and Dictionary

Hashtables represent a collection of key/value pairs that are organized based on the hash code of the key. 
Keys must be unique.

```csharp

//Create an initialize Hashtable
Hashtable myHt = new Hashtable() { { "Hello", "World" }, { "Key", "Value" } };

//Create Hashtable and add some Key-Value pairs.
Hashtable myHt2 = new Hashtable();
	myHt2.Add("Hello", "World");
	myHt2.Add("Key", "Value");

```

Dictionary is a generic class.It represents a collection of key/value pairs. Keys must be unique.

```csharp

//Create an initialize Dictionary
Dictionary<string, string> dict = new Dictionary<string, string>() { { "Hello", "World" }, { "Key", "Value" } };
//Create Dictionary and add some Key-Value pairs.
Dictionary<string, string> dict2 = new Dictionary<string, string>();
	dict2.Add("Hello", "World");
	dict2.Add("Key", "Value");

```



## Clojure

Clojure's collections are immutable: rather than modifying an existing collection, you create a new collection based on a previous one but with changes, for example an additional element.


### Hash maps


```Clojure
{1 "a", "Q" 10} ; commas are treated as whitespace
(hash-map 1 "a" "Q" 10) ; equivalent to the above
(let [my-map {1 "a"}]
  (assoc my-map "Q" 10)) ; "adding" an element
```


### Lists


```Clojure
'(1 4 7) ; a linked list
(list 1 4 7)
(cons 1 (cons 4 '(7)))
```



### Vectors


```Clojure
['a 4 11] ; somewhere between array and list
(vector 'a 4 11)
(cons ['a 4] 11) ; vectors add at the *end*
```



### Sets


```Clojure
#{:pig :dog :bear}
(assoc #{:pig :bear} :dog)
(set [:pig :bear :dog])
```



## COBOL

COBOL is very much a ''fixed length'' programming environment.  Hierarchical fixed length records are the main data grouping in many COBOL applications.

Arrays are historically called ''tables'' in COBOL literature and are usually defined within a hierarchy.  Tables are defined with the reserved word phrases OCCURS n TIMES, and OCCURS FROM n TO m TIMES DEPENDING ON x, (commonly referred to as ODO for short).

This example shows a small record layout inside a very small table.  The last line of the output sample is a debug enabled run-time bounds check abend, caused after the table is decreased in size.  The first run, without bounds check, runs to ''an erroneous'' completion; the second, with debug enabled, does not.

{{works with|GnuCOBOL}}

```COBOL
       identification division.
       program-id. collections.

       data division.
       working-storage section.
       01 sample-table.
          05 sample-record occurs 1 to 3 times depending on the-index.
             10 sample-alpha   pic x(4).
             10 filler         pic x value ":".
             10 sample-number  pic 9(4).
             10 filler         pic x value space.
       77 the-index            usage index.

       procedure division.
       collections-main.

       set the-index to 3
       move 1234 to sample-number(1)
       move "abcd" to sample-alpha(1)

       move "test" to sample-alpha(2)

       move 6789 to sample-number(3)
       move "wxyz" to sample-alpha(3)

       display "sample-table    : " sample-table
       display "sample-number(1): " sample-number(1)
       display "sample-record(2): " sample-record(2)
       display "sample-number(3): " sample-number(3)

      *> abend: out of bounds subscript, -debug turns on bounds check
       set the-index down by 1
       display "sample-table    : " sample-table
       display "sample-number(3): " sample-number(3)

       goback.
       end program collections.
```


{{out}}

```txt
prompt$ cobc -xj collections.cob
sample-table    : abcd:1234 test:0000 wxyz:6789
sample-number(1): 1234
sample-record(2): test:0000
sample-number(3): 6789
sample-table    : abcd:1234 test:0000
sample-number(3): 6789

prompt$ cobc -xj -debug collections.cob
sample-table    : abcd:1234 test:0000 wxyz:6789
sample-number(1): 1234
sample-record(2): test:0000
sample-number(3): 6789
sample-table    : abcd:1234 test:0000
collections.cob: 33: libcob: Subscript of 'sample-number' out of bounds: 3

```

 

## Common Lisp



### =hashing=



```lisp
CL-USER> (let ((list '())
               (hash-table (make-hash-table)))
           (push 1 list)
           (push 2 list)
           (push 3 list)
           (format t "~S~%" (reverse list))
           (setf (gethash 'foo hash-table) 42)
           (setf (gethash 'bar hash-table) 69)
           (maphash (lambda (key value)
                      (format t "~S => ~S~%" key value))
                    hash-table)
           ;; or print the hash-table in readable form
           ;; (inplementation-dependent)
           (write hash-table :readably t)
           ;; or describe it
           (describe hash-table)
           ;; describe the list as well
           (describe list))
;; FORMAT on a list
(1 2 3)
;; FORMAT on a hash-table
FOO => 42
BAR => 69
;; WRITE :readably t on a hash-table
#.(SB-IMPL::%STUFF-HASH-TABLE
   (MAKE-HASH-TABLE :TEST 'EQL :SIZE '16 :REHASH-SIZE '1.5
                    :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
   '((BAR . 69) (FOO . 42)))
;; DESCRIBE on a hash-table
#<HASH-TABLE :TEST EQL :COUNT 2 {1002B6F391}>
  [hash-table]

Occupancy: 0.1
Rehash-threshold: 1.0
Rehash-size: 1.5
Size: 16
Synchronized: no
;; DESCRIBE on a list
(3 2 1)
  [list]
; No value
```



### =deque=


In Lisp, a deque can be represented using two list variables which are understood to be opposite to each other. That is to say, the list links (cons cell cdr pointers) go inward into the deque from both ends. For instance the deque (1 2 3 4 5 6) can be represented using (1 2 3) and (6 5 4). Then, it is easy to push items on either end using ordinary list push operations. Popping is also simple, except when the case occurs that either piece runs out of items. A Lisp macro can be provided which takes care of this situation. The implementation below handles the underflow in one deque piece by transferring about one half of the elements from the opposite piece. This keeps the amortized cost for pushes and pops O(1), and prevents the degenerate behavior of bouncing all the elements from one side to the other when pops are requested which alternate between the two ends of the deque.


```lisp

;;; Obtained from Usenet,
;;; Message-ID: <b3b1cc90-2e2b-43c3-b7d9-785ae29870e7@e23g2000prf.googlegroups.com>
;;; Posting by Kaz Kylheku, February 28, 2008.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun bisect-list (list &optional (minimum-length 0))
    (do ((double-skipper (cddr list) (cddr double-skipper))
         (single-skipper list (cdr single-skipper))
         (length 2 (+ length (if (cdr double-skipper) 2 1))))
      ((null double-skipper)
       (cond
         ((< length minimum-length)
          (values list nil))
         ((consp single-skipper)
          (multiple-value-prog1
            (values list (cdr single-skipper))
            (setf (cdr single-skipper) nil)))
         (t (values list nil))))))

  (defun pop-deque-helper (facing-piece other-piece)
    (if (null facing-piece)
      (multiple-value-bind (head tail) (bisect-list other-piece 10)
        (let ((remaining (if tail head))
              (moved (nreverse (or tail head))))
          (values (first moved) (rest moved) remaining)))
      (values (first facing-piece) (rest facing-piece) other-piece))))

(defmacro pop-deque (facing-piece other-piece)
  (let ((result (gensym))
        (new-facing (gensym))
        (new-other (gensym)))
    `(multiple-value-bind (,result ,new-facing ,new-other)
                          (pop-deque-helper ,facing-piece ,other-piece)
       (psetf ,facing-piece ,new-facing
              ,other-piece ,new-other)
       ,result)))

```


Demo:


```txt
[1]> (defvar *front* nil)
*FRONT*
[2]> (defvar *back* nil)
*BACK*
[3]> (push 1 *front*)
(1)
[4]> (push 2 *front*)
(2 1)
[5]> (push 5 *back*)
(5)
[6]> (push 6 *back*)
(6 5)
[7]> (append *front* (reverse *back*)) ;; display the deque!
(2 1 5 6)
[8]> (pop-deque *front* *back*)
2
[9]> (append *front* (reverse *back*)) ;; display the deque!
(1 5 6)
[10]> (pop-deque *back* *front*)
6
[11]> (append *front* (reverse *back*)) ;; display the deque!
(1 5)
[12]> (pop-deque *back* *front*)
5
[13]> (append *front* (reverse *back*)) ;; display the deque!
(1)
[14]> *front*
(1)
[15]> *back*
NIL
[16]> (pop-deque *back* *front*)
1
[17]> *front*
NIL
[18]> *back*
NIL
```



## D


D has static arrays.

```d
int[3] array;
array[0] = 5;
// array.length = 4; // compile-time error
```


D has dynamic arrays.

```d
int[] array;
array ~= 5; // append 5
array.length = 3;
array[3] = 17; // runtime error: out of bounds. check removed in release mode.
array = [2, 17, 3];
writefln(array.sort); // 2, 3, 17
```


D has associative arrays.

```d
int[int] array;
// array ~= 5; // it doesn't work that way!
array[5] = 17;
array[6] = 20;
// prints "[5, 6]" -> "[17, 20]" - although the order is not specified.
writefln(array.keys, " -> ", array.values);
assert(5 in array); // returns a pointer, by the way
if (auto ptr = 6 in array) writefln(*ptr); // 20
```



## E


E has both mutable and immutable builtin collections; the common types are list (array), map (hash table), and set (hash table). This interactive session shows mutable lists and immutable collections of all three types. See also [[Arrays#E]].


```e
? def constList := [1,2,3,4,5]
# value: [1, 2, 3, 4, 5]

? constList.with(6)
# value: [1, 2, 3, 4, 5, 6]

? def flexList := constList.diverge()
# value: [1, 2, 3, 4, 5].diverge()

? flexList.push(6)
? flexList
# value: [1, 2, 3, 4, 5, 6].diverge()

? constList
# value: [1, 2, 3, 4, 5]

? def constMap := [1 => 2, 3 => 4]
# value: [1 => 2, 3 => 4]

? constMap[1]
# value: 2

? def constSet := [1, 2, 3, 2].asSet()
# value: [1, 2, 3].asSet()

? constSet.contains(3)
# value: true
```



## EchoLisp

The collection will be a list, which is not unusual in EchoLisp. We add items - symbols - to the collection, and save it to local storage.

```lisp

(define my-collection ' ( 🌱 ☀️ ☔️ ))
(set! my-collection (cons '🎥 my-collection))
(set! my-collection (cons '🐧 my-collection))
my-collection
  → (🐧 🎥 🌱 ☀️ ☔️)

;; save it
(local-put 'my-collection)
  → my-collection

```


## Elena

ELENA 4.1:

### Arrays


```elena

// Weak array
var stringArr := Array.allocate(5);
stringArr[0] := "string";
     
// initialized array
var intArray := new int[]::(1, 2, 3, 4, 5);

```



### ArrayList and List


```elena

//Create and initialize ArrayList
var myAl := new system'collections'ArrayList().append:"Hello".append:"World".append:"!";
 
//Create and initialize List
var myList := new system'collections'List().append:"Hello".append:"World".append:"!";

```



### Dictionary


```elena

//Create a dictionary
var dict := new system'collections'Dictionary();
dict["Hello"] := "World";
dict["Key"] := "Value";

```



## Elixir

Elixir data types are immutable.
The data contents aren't changed but can get changed new data.
Indexes start from zero ( It is different from Erlang ).


### List

Elixir uses square brackets to specify a list of values. Values can be of any type:

```elixir
empty_list = []
list = [1,2,3,4,5]
length(list)                    #=> 5
[0 | list]                      #=> [0,1,2,3,4,5]
hd(list)                        #=> 1
tl(list)                        #=> [2,3,4,5]
Enum.at(list,3)                 #=> 4
list ++ [6,7]                   #=> [1,2,3,4,5,6,7]
list -- [4,2]                   #=> [1,3,5]
```



### Tuple

Elixir uses curly brackets to define tuples. Like lists, tuples can hold any value:
Tuples store elements contiguously in memory. This means accessing a tuple element per index or getting the tuple size is a fast operation:

```elixir
empty_tuple = {}                #=> {}
tuple = {0,1,2,3,4}             #=> {0, 1, 2, 3, 4}
tuple_size(tuple)               #=> 5
elem(tuple, 2)                  #=> 2
put_elem(tuple,3,:atom)         #=> {0, 1, 2, :atom, 4}
```



### Keyword lists

In Elixir, when we have a list of tuples and the first item of the tuple (i.e. the key) is an atom, we call it a keyword list:

```elixir
list = [{:a,1},{:b,2}]          #=> [a: 1, b: 2]
list == [a: 1, b: 2]            #=> true
list[:a]                        #=> 1
list ++ [c: 3, a: 5]            #=> [a: 1, b: 2, c: 3, a: 5]
```

Keyword lists are important because they have two special characteristics:
# They keep the keys ordered, as specified by the developer.
# They allow a key to be given more than once.


### Map

Whenever you need a key-value store, maps are the “go to” data structure in Elixir.

Compared to keyword lists, we can already see two differences:
# Maps allow any value as a key.
# Maps' keys do not follow any ordering.

```elixir
empty_map = Map.new           #=> %{}
kwlist = [x: 1, y: 2]         #   Key Word List
Map.new(kwlist)               #=> %{x: 1, y: 2}
Map.new([{1,"A"}, {2,"B"}])   #=> %{1 => "A", 2 => "B"}
map = %{:a => 1, 2 => :b}     #=> %{2 => :b, :a => 1}
map[:a]                       #=> 1
map[2]                        #=> :b

# If you pass duplicate keys when creating a map, the last one wins:
%{1 => 1, 1 => 2}             #=> %{1 => 2}

# When all the keys in a map are atoms, you can use the keyword syntax for convenience:
map = %{:a => 1, :b => 2}     #=> %{a: 1, b: 2}
map.a                         #=> 1
%{map | :a => 2}              #=> %{a: 2, b: 2}     update only
```



### Set


```elixir
empty_set = MapSet.new                  #=> #MapSet<[]>
set1 = MapSet.new(1..4)                 #=> #MapSet<[1, 2, 3, 4]>
MapSet.size(set1)                       #=> 4
MapSet.member?(set1,3)                  #=> true
MapSet.put(set1,9)                      #=> #MapSet<[1, 2, 3, 4, 9]>
set2 = MapSet.new([6,4,2,0])            #=> #MapSet<[0, 2, 4, 6]>
MapSet.union(set1,set2)                 #=> #MapSet<[0, 1, 2, 3, 4, 6]>
MapSet.intersection(set1,set2)          #=> #MapSet<[2, 4]>
MapSet.difference(set1,set2)            #=> #MapSet<[1, 3]>
MapSet.subset?(set1,set2)               #=> false
```



### Struct

Structs are extensions built on top of maps that provide compile-time checks and default values.

```elixir
defmodule User do
  defstruct name: "john", age: 27
end
john = %User{}                      #=> %User{age: 27, name: "john"}
john.name                           #=> "john"
%User{age: age} = john              #   pattern matching
age                                 #=> 27
meg = %User{name: "meg"}            #=> %User{age: 27, name: "meg"}
is_map(meg)                         #=> true
```



## Factor


```factor
USING: assocs deques dlists lists lists.lazy sequences sets ;

! ===fixed-size sequences===
{ 1 2 "foo" 3 } ! array
[ 1 2 3 + * ]   ! quotation
"Hello, world!" ! string
B{ 1 2 3 }      ! byte array
?{ f t t }      ! bit array

! Add an element to a fixed-size sequence
{ 1 2 3 } 4 suffix ! { 1 2 3 4 }

! Append a sequence to a fixed-size sequence
{ 1 2 3 } { 4 5 6 } append ! { 1 2 3 4 5 6 }

! Sequences are sets
{ 1 1 2 3 } { 2 5 7 8 } intersect ! { 2 }

! Strings are just arrays of code points
"Hello" { } like ! { 72 101 108 108 111 }
{ 72 101 108 108 111 } "" like ! "Hello"

! 
### resizable sequences

V{ 1 2 "foo" 3 }     ! vector
BV{ 1 2 255 }        ! byte vector
SBUF" Hello, world!" ! string buffer

! Add an element to a resizable sequence by mutation
V{ 1 2 3 } 4 suffix! ! V{ 1 2 3 4 }

! Append a sequence to a resizable sequence by mutation
V{ 1 2 3 } { 4 5 6 } append! ! V{ 1 2 3 4 5 6 }

! Sequences are stacks
V{ 1 2 3 } pop ! 3

! 
### associative mappings

{ { "hamburger" 150 } { "soda" 99 } { "fries" 99 } } ! alist
H{ { 1 "a" } { 2 "b" } } ! hash table

! Add a key-value pair to an assoc
3 "c" H{ { 1 "a" } { 2 "b" } } [ set-at ] keep
! H{ { 1 "a" } { 2 "b" } { "c" 3 } }

! 
### linked lists

T{ cons-state f 1 +nil+ }               ! literal list syntax
T{ cons-state { car 1 } { cdr +nil+ } } ! literal list syntax
                                        ! with car 1 and cdr nil
                                        
! One method of manually constructing a list
1 2 3 4 +nil+ cons cons cons cons
                                        
1 2 2list ! convenience word for list construction
          ! T{ cons-state
          !     { car 1 }
          !     { cdr T{ cons-state { car 2 } { cdr +nil+ } } }
          !  }

{ 1 2 3 4 } sequence>list ! make a list from a sequence

0 lfrom ! a lazy list from 0 to infinity
0 [ 2 + ] lfrom-by ! a lazy list of all even numbers >= 0.

DL{ 1 2 3 } ! double linked list / deque
3 DL{ 1 2 } [ push-front ] keep ! DL{ 3 1 2 }
3 DL{ 1 2 } [ push-back  ] keep ! DL{ 1 2 3 }

! Factor also comes with disjoint sets, interval maps, heaps,
! boxes, directed graphs, locked I/O buffers, trees, and more!
```



## Fancy



### array



```fancy

# creating an empty array and adding values

a = []      # => []
a[0]: 1    # => [1]
a[3]: 2    # => [1, nil, nil, 2]

# creating an array with the constructor
a = Array new     # => []

```



### hash



```fancy
# creating an empty hash

h = <[]>        # => <[]>
h["a"]: 1         # => <["a" => 1]>
h["test"]: 2.4  # => <["a" => 1, "test" => 2.4]>
h[3]: "Hello"   # => <["a" => 1, "test" => 2.4, 3 => "Hello"]>

# creating a hash with the constructor
h = Hash new           # => <[]>

```



## Forth

{{libheader|Forth Foundation Library}}


### Array



```forth
include ffl/car.fs

10 car-create ar           \ create a dynamic array with initial size 10

2 0 ar car-set             \ ar[0] = 2
3 1 ar car-set             \ ar[1] = 3
1 0 ar car-insert          \ ar[0] = 1 ar[1] = 2 ar[2] = 3

```



### Double linked list



```forth
include ffl/dcl.fs

dcl-create dl              \ create a double linked list

3   dl dcl-append
1   dl dcl-prepend
2 1 dl dcl-insert          \ dl[0] = 1 dl[1] = 2 dl[2] = 3

```



### Hashtable



```forth
include ffl/hct.fs

10 hct-create ht           \ create a hashtable with initial size 10 

1 s" one"   ht hct-insert  \ ht["one"] = 1
2 s" two"   ht hct-insert  \ ht["two"] = 2
3 s" three" ht hct-insert  \ ht["three"] = 3

```



## Fortran


### Standard

The only facility for a collection more organised than a collection of separately-named variables (even if with a system for the names) is the array, which is a collection of items of identical type, indexed by an integer only, definitely not by a text as in say Snobol. Thus 
```Fortran
 REAL A(36)   !Declares a one-dimensional array A(1), A(2), ... A(36)
  A(1) = 1           !Assigns a value to the first element.
  A(2) = 3*A(1) + 5  !The second element gets 8.
```

With F90 came a large expansion in the facilities for manipulating arrays. They can now have any lower bound, as in <code>REAL A(-6:+12)</code> and their size can be defined at run time, not just compile time. Further, programmer-defined data aggregates can be defined via the TYPE statement, and arrays of such types can be manipulated. However, type-matching remains rigid: all elements of an array must be of the same type. So, 
```Fortran
 TYPE(MIXED)           !Name the "type".
  INTEGER COUNTER        !Its content is listed.
  REAL WEIGHT,DEPTH
  CHARACTER*28 MARKNAME
  COMPLEX PATH(6)        !The mixed collection includes an array.
 END TYPE MIXED
 TYPE(MIXED) TEMP,A(6) !Declare some items of that type.
```

would define a collection of variables constituting a "type", then a simple variable TEMP whose parts would be accessed via the likes of <code>TEMP.DEPTH</code> or <code>TEMP%DEPTH</code>, and an array of such aggregates where <code> A(3).PATH(1) = (2.7,3.1)</code> assigns a complex number to the first step of the PATH of the third element of array A. The indexing must be associated with the item having an array aspect, but in pl/i <code>A.PATH(3,1)</code> - or other groupings - would be acceptable.

There is a sense in which the CHARACTER type is flexible, in that multiple and different types can be represented as texts so that <code>"Pi, 3.14159, 4*atan(1)"</code> might be considered a collection of three items (yet be contained in one, possibly large variable) and be processed in various ways, or one might prepare a battery of variables and arrays referring to each other and disc files in such a way as to present a database containing a collection of information.


### Coarray

Fortran normally uses only ( ) with no appeal to {[ ]} usage even for complex formulae. The co-array concept of the 1990s that was standardised in F2008 extends the syntax to use [k] to specify the k'th "image" executing in parallel. Loosely, if X is a variable, manipulated as in normal statements, a reference to X[3] would be to that X value held by the third running "image", while X would be in each image a reference to that image's own X value. In other words, there is a collection of X variables.


## FreeBASIC

Although it's possible to build any type of collection (vectors, linked lists, stacks, queues, hashtables etc.)
using FreeBASIC's object oriented features, the only collection type which is built into the language is the array.

This can be fixed size or dynamic, have arbitrary lower and upper bounds, have up to 8 dimensions and any kind of element type (including user defined types). Here are some simple examples:

```freebasic
' FB 1.05.0 Win64

'create fixed size array of integers
Dim a(1 To 5) As Integer = {1, 2, 3, 4, 5}
Print a(2), a(4) 

'create empty dynamic array of doubles
Dim b() As Double
' add two elements by first redimensioning the array to hold this number of elements
Redim b(0 To 1)
b(0) = 3.5 : b(1) = 7.1
Print b(0), b(1)

'create 2 dimensional fixed size array of bytes
Dim c(1 To 2, 1 To 2) As Byte = {{1, 2}, {3, 4}}
Print c(1, 1), c(2,2)
Sleep 
```


{{out}}

```txt

 2             4
 3.5           7.1
 1             4

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=004236066581dd85f39f34e100bf5c40 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siCount As Short
Dim cCollection As Collection = ["0": "zero", "1": "one", "2": "two", "3": "three", "4": "four", 
                                 "5": "five", "6": "six", "7": "seven", "8": "eight", "9": "nine"]

For siCount = 0 To 9
  Print cCollection[Str(siCount)]
Next

End
```

Output:

```txt

zero
one
two
three
four
five
six
seven
eight
nine

```


## Go

===Built in, resizable===
* Slices
* Maps
Built in resizable collections are slices and maps.  The value type for these collections can be any Go type, including interface.  An empty interface can reference an object of any type, providing a kind of polymorphic collection.  Here the variable <tt>a</tt> is a slice of interface{} objects.

```go
package main

import "fmt"

func main() {
    var a []interface{}
    a = append(a, 3)
    a = append(a, "apples", "oranges")
    fmt.Println(a)
}
```

{{out}}

```txt

[3 apples oranges]

```

===Built in, less conventional===
* Go has arrays that can be used as collections, but arrays are declared with constant size and cannot be resized.
* Strings are a special case of slice.  Strings are immutable and are handled specially in other ways.
* A struct with a number of members might be considered a collection in some sense.
* A buffered channel might be closer to the familiar concept of a collection, as it represents a FIFO queue.  Buffered channels have a fixed size and cannot be resized after creation.

### Library

* The container directory of the standard library has the packages heap, list, and ring.
* Anything that implements bufio.ReadWriter can be used as a FIFO queue.  This includes bytes.Buffer, which makes a useful in-memory collection.
* The sort package also contains search functions which perform a binary search on a sorted collection.  For these functions the collection implementation is abstracted through sort.Interface.  It is typically a slice, but could be anything that is indexable with an integer index.


## Groovy

Lists are just variable-length, integer-indexed arrays.

```groovy
def emptyList = []
assert emptyList.isEmpty() : "These are not the items you're looking for"
assert emptyList.size() == 0 : "Empty list has size 0"
assert ! emptyList : "Empty list evaluates as boolean 'false'"

def initializedList = [ 1, "b", java.awt.Color.BLUE ]
assert initializedList.size() == 3
assert initializedList : "Non-empty list evaluates as boolean 'true'"
assert initializedList[2] == java.awt.Color.BLUE : "referencing a single element (zero-based indexing)"
assert initializedList[-1] == java.awt.Color.BLUE : "referencing a single element (reverse indexing of last element)"

def combinedList = initializedList + [ "more stuff", "even more stuff" ]
assert combinedList.size() == 5
assert combinedList[1..3] == ["b", java.awt.Color.BLUE, "more stuff"] : "referencing a range of elements"

combinedList << "even more stuff"
assert combinedList.size() == 6
assert combinedList[-1..-3] == \
        ["even more stuff", "even more stuff", "more stuff"] \
                : "reverse referencing last 3 elements"
println ([combinedList: combinedList])
```


{{out}}

```txt
[combinedList:[1, b, java.awt.Color[r=0,g=0,b=255], more stuff, even more stuff, even more stuff]]
```


Maps are just variable-length, associative arrays. They are not necessarily order preserving.

```groovy
def emptyMap = [:]
assert emptyMap.isEmpty() : "These are not the items you're looking for"
assert emptyMap.size() == 0 : "Empty map has size 0"
assert ! emptyMap : "Empty map evaluates as boolean 'false'"

def initializedMap = [ count: 1, initial: "B", eyes: java.awt.Color.BLUE ]
assert initializedMap.size() == 3
assert initializedMap : "Non-empty map evaluates as boolean 'true'"
assert initializedMap["eyes"] == java.awt.Color.BLUE : "referencing a single element (array syntax)"
assert initializedMap.eyes == java.awt.Color.BLUE : "referencing a single element (member syntax)"
assert initializedMap.height == null : \
        "references to non-existant keys generally evaluate to null (implementation dependent)"

def combinedMap = initializedMap \
        + [hair: java.awt.Color.BLACK, birthdate: Date.parse("yyyy-MM-dd", "1960-05-17") ]
assert combinedMap.size() == 5

combinedMap["weight"] = 185        // array syntax
combinedMap.lastName = "Smith"     // member syntax
combinedMap << [firstName: "Joe"]  // entry syntax
assert combinedMap.size() == 8
assert combinedMap.keySet().containsAll(
        ["lastName", "count", "eyes", "hair", "weight", "initial", "firstName", "birthdate"])
println ([combinedMap: combinedMap])
```


{{out}}

```txt
[combinedMap:[count:1, initial:B, eyes:java.awt.Color[r=0,g=0,b=255], hair:java.awt.Color[r=0,g=0,b=0], birthdate:Tue May 17 00:00:00 CDT 1960, weight:185, lastName:Smith, firstName:Joe]]
```


Sets are unique, not indexed at all (contents can only be discovered by traversal), and are not necessarily order preserving. There is no particular special language support for denoting a Set, although a Set may be initialized from a List, and Sets share many of the same operations and methods that are available in Lists.

```groovy
def emptySet = new HashSet()
assert emptySet.isEmpty() : "These are not the items you're looking for"
assert emptySet.size() == 0 : "Empty set has size 0"
assert ! emptySet : "Empty set evaluates as boolean 'false'"

def initializedSet = new HashSet([ 1, "b", java.awt.Color.BLUE ])
assert initializedSet.size() == 3
assert initializedSet : "Non-empty list evaluates as boolean 'true'"
//assert initializedSet[2] == java.awt.Color.BLUE  // SYNTAX ERROR!!! No indexing of set elements!

def combinedSet = initializedSet + new HashSet([ "more stuff", "even more stuff" ])
assert combinedSet.size() == 5

combinedSet << "even more stuff"
assert combinedSet.size() == 5 : "No duplicate elements allowed!"
println ([combinedSet: combinedSet])
```


{{out}}

```txt
[combinedSet:[1, java.awt.Color[r=0,g=0,b=255], b, even more stuff, more stuff]]
```



## Haskell


### Data.List

The list is typically the first collection type to be encountered in textbooks, but other types may tend to be more efficient, or more flexibly accessed; see the <code>Data</code> hierarchy of [http://www.haskell.org/ghc/docs/latest/html/libraries/ GHC's standard library]. New collection types may be defined with <code>data</code>.


```haskell
[1, 2, 3, 4, 5]
```


To prepend a single element to a list, use the <code>:</code> operator:


```haskell
1 : [2, 3, 4]
```


To concatenate two lists, use <code>++</code>:


```haskell
[1, 2] ++ [3, 4]
```


To concatenate a whole list of lists, use <code>concat</code>:


```haskell
concat [[1, 2], [3, 4], [5, 6, 7]]
```



### Data.Array

Faster retrieval by index:

```haskell
import Data.Array (Array, listArray, Ix, (!))

triples :: Array Int (Char, String, String)
triples =
  listArray (0, 11) $
  zip3
    "鼠牛虎兔龍蛇馬羊猴鸡狗豬" -- 生肖 shengxiao – symbolic animals
    (words "shǔ niú hǔ tù lóng shé mǎ yáng hóu jī gǒu zhū")
    (words "rat ox tiger rabbit dragon snake horse goat monkey rooster dog pig")

indexedItem
  :: Ix i
  => Array i (Char, String, String) -> i -> String
indexedItem a n =
  let (c, w, w1) = a ! n
  in c : unwords ["\t", w, w1]

main :: IO ()
main = (putStrLn . unlines) $ indexedItem triples <$> [2, 4, 6]
```

{{Out}}

```txt
虎     hǔ tiger
龍     lóng dragon
馬     mǎ horse
```



### Data.Map

Flexible key-value indexing and efficient retrieval:

```haskell
import qualified Data.Map as M
import Data.Maybe (isJust)

mapSample :: M.Map String Int
mapSample =
  M.fromList
    [ ("alpha", 1)
    , ("beta", 2)
    , ("gamma", 3)
    , ("delta", 4)
    , ("epsilon", 5)
    , ("zeta", 6)
    ]

maybeValue :: String -> Maybe Int
maybeValue = flip M.lookup mapSample

main :: IO ()
main =
  print $ sequence $ filter isJust (maybeValue <$> ["beta", "delta", "zeta"])
```

{{Out}}

```txt
Just [2,4,6]
```



### Data.Set

Repertoire of efficient set operations:

```haskell
import qualified Data.Set as S

setA :: S.Set String
setA = S.fromList ["alpha", "beta", "gamma", "delta", "epsilon"]

setB :: S.Set String
setB = S.fromList ["delta", "epsilon", "zeta", "eta", "theta"]

main :: IO ()
main = (print . S.toList) (S.intersection setA setB)
```

{{Out}}

```txt
["delta","epsilon"]
```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon have a number of different types that could be considered collections.  For more information see [[Icon%2BUnicon/Intro#Data_Types|Introduction to Icon and Unicon on Rosetta - Data Types]].  

Several data types could be considered collections:

```Icon
#  Creation of collections:
 s := "abccd"                                   # string, an ordered collection of characters, immutable
 c := 'abcd'                                    # cset, an unordered collection of characters, immutable 
 S := set()                                     # set, an unordered collection of unique values, mutable, contents may be of any type
 T := table()                                   # table, an associative array of values accessed via unordered keys, mutable, contents may be of any type
 L := []                                        # list, an ordered collection of values indexed by position 1..n or as stack/queue, mutable, contents may be of any type
 record constructorname(field1,field2,fieldetc) # record, a collection of values stored in named fields, mutable, contents may be of any type (declare outside procedures)
 R := constructorname()                         # record (creation)
```


Adding to these collections can be accomplished as follows:

```Icon

 s ||:= "xyz"                                   # concatenation
 c ++:= 'xyz'                                   # union
 insert(S,"abc")                                # insert 
 T["abc"] := "xyz"                              # insert create/overwrite
 put(L,1)                                       # put (extend), also push
 R.field1 := "xyz"                              # overwrite
```


Additionally, the following operations apply:

```Icon

 S := S ++ S2                                   # union of two sets or two csets
 S ++:= S2                                      # augmented assignment
 L := L ||| L2                                  # list concatenation
 L |||:= L2                                     # augmented assignment
```



## J

J is an array-oriented language -- it treats all data as collections and processes collections natively.  Its built in (primitive) functions are specifically designed to handle collections.

J will, when possible without losing significance of the original value, implicitly convert values to a type which allows them represented in a homogeneous fashion in a collection.  Heterogeneous collections are possible via "boxing" (analogous to a "variant" data type).


```j
   c =: 0 10 20 30 40 NB.  A collection
 
   c, 50              NB.  Append 50 to the collection
0 10 20 30 40 50
   _20 _10 , c        NB.  Prepend _20 _10 to the collection
_20 _10 0 10 20 30 40
  
   ,~  c               NB.  Self-append
0 10 20 30 40 0 10 20 30 40
   ,:~  c              NB.  Duplicate
0 10 20 30 40
0 10 20 30 40
  
   30 e. c             NB.  Is 30 in the collection?
1
   30 i.~c             NB.  Where? 
3
   30 80 e. c          NB.  Don't change anything to test multiple values -- collections are native.
1 0

   2 1 4 2 { c         NB.  From the collection, give me items two, one, four, and two again.
20 10 40 20

   |.c                 NB.  Reverse the collection
40 30 20 10 0
   1+c                 NB.  Increment the collection
1 11 21 31 41          
   c%10                NB.  Decimate the collection (divide by 10)
0 1 2 3 4
 
   {. c                NB.  Give me the first item
0
   {: c                NB.  And the last
40
   3{.c                NB.  Give me the first 3 items
0 10 20
   3}.c                NB.  Throw away the first 3 items  
30 40
   _3{.c               NB.  Give me the last 3 items 
20 30 40
   _3}.c               NB.  (Guess)
0 10

     keys_map_  =:  'one';'two';'three'
     vals_map_  =:  'alpha';'beta';'gamma'
   lookup_map_  =:  a:& $: : (dyad def ' (keys i. y) { vals,x')&boxopen
   exists_map_  =:  verb def 'y e. keys'&boxopen

   exists_map_ 'bad key'
0 
   exists_map_ 'two';'bad key'
1 0

   lookup_map_ 'one'
+-----+
|alpha|
+-----+
   lookup_map_ 'three';'one';'two';'one'
+-----+-----+----+-----+
|gamma|alpha|beta|alpha|
+-----+-----+----+-----+
   lookup_map_ 'bad key'
++
||
++
   'some other default' lookup_map_ 'bad key'
+------------------+
|some other default|
+------------------+
   'some other default' lookup_map_ 'two';'bad key'
+----+------------------+
|beta|some other default|
+----+------------------+

   +/ c                NB. Sum of collection
100
   */ c                NB.  Product of collection
0

   i.5                 NB.  Generate the first 5 nonnegative integers
0 1 2 3 4
   10*i.5              NB.  Looks familiar
0 10 20 30 40
   c = 10*i.5          NB.  Test each for equality
1 1 1 1 1
   c -: 10 i.5         NB.  Test for identicality
1
```



## Java


### Native collection library

{{works with|Java|1.5+}}

When creating a List of any kind in Java (Arraylist or LinkedList), the type of the variable is a style choice. It is sometimes considered good practice to make the pointer of type List and the new object of a List subclass. Doing this will ensure two things: if you need to change the type of list you want you only need to change one line and all of your methods will still work, and you will not be able to use any methods that are specific to the List type you chose. So in this example, all instances of "ArrayList" can be changed to "LinkedList" and it will still work, but you will not be able to use a method like "ensureCapactiy()" because the variable is of type List.


```Java5
List arrayList = new ArrayList();
arrayList.add(new Integer(0));
// alternative with primitive autoboxed to an Integer object automatically
arrayList.add(0); 

//other features of ArrayList
//define the type in the arraylist, you can substitute a proprietary class in the "<>"
List<Integer> myarrlist = new ArrayList<Integer>();

//add several values to the arraylist to be summed later
int sum;
for(int i = 0; i < 10; i++) {
    myarrlist.add(i);
}
```



```Java5
//loop through myarrlist to sum each entry
for ( i = 0; i < myarrlist.size(); i++) {
    sum += myarrlist.get(i);
}
```

or

```Java5
for(int i : myarrlist) {
    sum += i;
}
```



```Java5
//remove the last entry in the ArrayList
myarrlist.remove(myarrlist.size()-1)

//clear the ArrayList
myarrlist.clear();
```

Here is a reference table for characteristics of commonly used <code>Collections</code> classes:
{|class="wikitable"
!Collection class
!random access
!order
!iterator direction
|-
|HashMap
|by key
|hash
|forward (separate iterators for entries, keys and values)
|-
|TreeMap
|by key
|ascending(key)
|forward (separate iterators for entries, keys and values)
|-
|LinkedHashMap
|by key
|insertion
|forward (separate iterators for entries, keys and values)
|-
|LinkedList
|by index
|insertion/to index
|both
|-
|ArrayList
|by index
|insertion/to index (ArrayList also has a defined but expandable size)
|both
|-
|HashSet
|only remove (returns the element that was removed)
|hash
|forward
|-
|TreeSet
|only remove (returns the element that was removed)
|ascending(element)
|forward
|}

###  Using the Scala collection classes

The [[Scala|Scala]] libraries are valid Java byte-code libraries. The collection part of these are rich because the multiple inheritance by traits. E.g. an [http://www.scala-lang.org/api/current/index.html#scala.collection.mutable.ArrayBuffer ArrayBuffer] has properties inherent of 9 traits such as Buffer[A], IndexedSeqOptimized[A, ArrayBuffer[A]], Builder[A, ArrayBuffer[A]], ResizableArray[A] and Serializable. Another collection e.g. TrieMap uses some of these and other added traits. A [http://www.scala-lang.org/api/current/index.html#scala.collection.concurrent.TrieMap TrieMap] -a hashmap- is the most advanced of all. It supports parallel processing without blocking.
```Java5
import scala.Tuple2;
import scala.collection.concurrent.TrieMap;
import scala.collection.immutable.HashSet;
import scala.collection.mutable.ArrayBuffer;

public class Collections {

	public static void main(String[] args) {
		ArrayBuffer<Integer> myarrlist = new ArrayBuffer<Integer>();
		ArrayBuffer<Integer> myarrlist2 = new ArrayBuffer<Integer>(20);

		myarrlist.$plus$eq(new Integer(42)); // $plus$eq is Scala += operator
		myarrlist.$plus$eq(13); // to add an element.
		myarrlist.$plus$eq(-1);

		myarrlist2 = (ArrayBuffer<Integer>) myarrlist2.$minus(-1);

		for (int i = 0; i < 10; i++)
			myarrlist2.$plus$eq(i);

		// loop through myarrlist to sum each entry
		int sum = 0;
		for (int i = 0; i < myarrlist2.size(); i++) {
			sum += myarrlist2.apply(i);
		}
		System.out.println("List is: " + myarrlist2 + " with head: "
				+ myarrlist2.head() + " sum is: " + sum);
		System.out.println("Third element is: " + myarrlist2.apply$mcII$sp(2));

		Tuple2<String, String> tuple = new Tuple2<String, String>("US",
				"Washington");
		System.out.println("Tuple2 is : " + tuple);

		ArrayBuffer<Tuple2<String, String>> capList = new ArrayBuffer<Tuple2<String, String>>();
		capList.$plus$eq(new Tuple2<String, String>("US", "Washington"));
		capList.$plus$eq(new Tuple2<String, String>("France", "Paris"));
		System.out.println(capList);

		TrieMap<String, String> trieMap = new TrieMap<String, String>();
		trieMap.put("US", "Washington");
		trieMap.put("France", "Paris");

		HashSet<Character> set = new HashSet<Character>();

		ArrayBuffer<Tuple2<String, String>> capBuffer = new ArrayBuffer<Tuple2<String, String>>();
		trieMap.put("US", "Washington");

		System.out.println(trieMap);
	}
}

```



## JavaScript


```javascript
var array = [];
array.push('abc');
array.push(123);
array.push(new MyClass);
console.log( array[2] );
```



```javascript
var obj = {};
obj['foo'] = 'xyz'; //equivalent to: obj.foo = 'xyz';
obj['bar'] = new MyClass; //equivalent to: obj.bar = new MyClass;
obj['1x; ~~:-b'] = 'text'; //no equivalent
console.log(obj['1x; ~~:-b']);
```



## jq


jq has three native collection types: JSON objects (implemented as hash
tables over strings), arrays (with index origin equal to 0), and JSON strings.
Since strings in jq can be thought of as arrays of codepoints, this article will focus on
objects and arrays.


### Creation

Collections can be created using JSON syntax (e.g. {"a":1}) or programmatically (e.g. {} | .a = 1).
One of the programmatic approaches to creating JSON objects allows
the key names to be specified as unquoted strings, e.g.

```jq
{"a": 1} == {a: 1}
```


evaluates to true.  Variables can also be used, e.g. the object {"a":1} can also be created by the following
pipeline:

```jq
"a" as $key | 1 as $value | {($key): $value}
```



### Equality

Two arrays are equal if and only if their lengths
and respective elements are equal.  Two objects are equal if and only if they have the same keys
and if the values at corresponding keys are equal.   Note that expressions with repeated keys
are regarded as programmatic expressions: e.g. {"a":1, "a":2} is regarded as shorthand for {"a":1} + {"a":2},
which evaluates to {"a":2}.  That is, "{"a":1, "a":2}" should be regarded as an expression that evaluates
to a JSON object.


### Immutability

Semantically, all jq data types are immutable, but it is often convenient to speak about
modifying an element of a composite structure.  For example, consider the following pipeline:


```jq
[0,1,2] | .[0] = 10
```


The result (or output) of this sequence is [10,1,2], so it is convenient to
speak of the operation ".[0] = 10" as simply a filter that sets the element at 0 to 10.


## Julia

Julia has a wide variety of collections, including vectors, matrices, lists of Any data type, associative arrays, and bitsets. 
There is a slicing notation and list comprehensions similar to those in Python, but the base index is by default 1, not 0. In Julia, a collection is a just variable length array:

```julia


julia> collection = []
0-element Array{Any,1}

julia> push!(collection, 1,2,4,7)
4-element Array{Any,1}:
 1
 2
 4
 7

```



## Kotlin

Apart from arrays whose length is immutable but content mutable, Kotlin distinguishes between mutable and immutable collection types in its standard library. Examples of each are given below. Where possible, the type parameter(s) of generic collection types are inferred from the content.

In addition, Kotlin can also access other types of Java collection such as LinkedList, Queue, Deque and Stack by simply importing the appropriate type:

```scala
import java.util.PriorityQueue

fun main(args: Array<String>) {
    // generic array
    val ga = arrayOf(1, 2, 3)
    println(ga.joinToString(prefix = "[", postfix = "]"))

    // specialized array (one for each primitive type)
    val da = doubleArrayOf(4.0, 5.0, 6.0)
    println(da.joinToString(prefix = "[", postfix = "]"))

    // immutable list
    val li = listOf<Byte>(7, 8, 9)
    println(li)

    // mutable list
    val ml = mutableListOf<Short>()
    ml.add(10); ml.add(11); ml.add(12)
    println(ml)

    // immutable map
    val hm = mapOf('a' to 97, 'b' to 98, 'c' to 99)
    println(hm)

    // mutable map
    val mm = mutableMapOf<Char, Int>()
    mm.put('d', 100); mm.put('e', 101); mm.put('f', 102)
    println(mm)

    // immutable set (duplicates not allowed)
    val se = setOf(1, 2, 3)
    println(se)

    // mutable set (duplicates not allowed)
    val ms = mutableSetOf<Long>()
    ms.add(4L); ms.add(5L); ms.add(6L)
    println(ms)

    // priority queue (imported from Java)
    val pq = PriorityQueue<String>()
    pq.add("First"); pq.add("Second"); pq.add("Third")
    println(pq)
}
```


{{out}}

```txt

[1, 2, 3]
[4.0, 5.0, 6.0]
[7, 8, 9]
[10, 11, 12]
{a=97, b=98, c=99}
{d=100, e=101, f=102}
[1, 2, 3]
[4, 5, 6]
[First, Second, Third]

```



## Lingo

Lingo has 2 collection types: lists (arrays) and property lists (hashes):

```lingo
-- list stuff
l = [1, 2]
l.add(3)
l.add(4)
put l
-- [1, 2, 3, 4]

-- property list stuff
pl = [#foo: 1, #bar: 2]
pl[#foobar] = 3
pl["barfoo"] = 4
put pl
-- [#foo: 1, #bar: 2, #foobar: 3, "barfoo": 4]
```


Lingo is not statically-typed, but if needed, a collection type that only accepts a specific data type can be created by sub-classing one of the 2 available collection types and overwriting its access methods, so that those block any data type other than the one that was passed to the constructor.


## Lisaac


### vector


```Lisaac
+ vector : ARRAY[INTEGER];
vector := ARRAY[INTEGER].create_with_capacity 32 lower 0;
vector.add_last 1;
vector.add_last 2;
```


### hashed set


```Lisaac
+ set : HASHED_SET[INTEGER];
set := HASHED_SET[INTEGER].create;
set.add 1;
set.add 2;
```


### linked list


```Lisaac
+ list : LINKED_LIST[INTEGER];
list := LINKED_LIST[INTEGER].create;
list.add_last 1;
list.add_last 2;
```


### hashed dictionary


```Lisaac
+ dict : HASHED_DICTIONARY[INTEGER/*value*/, STRING_CONSTANT/*key*/];
dict := HASHED_DICTIONARY[INTEGER, STRING_CONSTANT].create;
dict.put 1 to "one";
dict.put 2 to "two";
```



## Logo

Logo has a list-like protocol (first, butfirst, etc.) which works on three different data types:
* members of a list: [one two three]
* items in an array: {one two three}
* characters in a word: "123


## Lua

Lua has only one type of collection, the table. But Lua's table has features of both, traditional arrays and hash maps (dictionaries). You can even mix both within one table. Note, that the numeric indices of Lua's table start at 1, not at 0 as with most other languages.


```lua
collection = {0, '1'}
print(collection[1]) -- prints 0

collection = {["foo"] = 0, ["bar"] = '1'} -- a collection of key/value pairs
print(collection["foo"]) -- prints 0
print(collection.foo) -- syntactic sugar, also prints 0

collection = {0, '1', ["foo"] = 0, ["bar"] = '1'}
```


It is idiomatic in Lua to represent a Set data structure with a table of keys to the true value.


## M2000 Interpreter

===Ordered List (array)===

```M2000 Interpreter

Module Arr {
      \\ array as tuple
      A=(1,2,3,4,5)
      Print Array(A,0)=1
      Print A
      \\ add two arrays
      A=Cons(A, (6,))
      Print Len(A)=6
      Print A
      \\ arrays may have arrays, inventories, stacks as items
      A=((1,2,3),(4,5,6))
      Print Array(Array(A, 0),2)=3
}
Arr

```

===Ordered List (stack)===
A stack may have values inventories,arrays, stacks, groups

```M2000 Interpreter

Module CheckStack {
      \\ ordered collection: Stack
      \\ we can add values to top or bottom,
      \\ we can move values to and from top
      A=Stack:=100,300,600,800,900
      Print StackItem(A, 2)=300, Len(A)=5
      Stack A {
            \\ push to bottom (or end)
            Data 2000, 4000
      }
      Print StackItem(A, 7)=4000, Len(A)=7
      Stack A {
            \\ pop from top
            Read X, Y
            Print X=100, Y=300
      }
      Print StackItem(A,5)=4000, Len(A)=5
      Stack A {
            \\ push to top
            Push 2, 1
            Stack    ' display stack items
      }
      \\ we can make a new stack merging other stacks
      A=Stack(A, stack:=5000,6000,7000)
      Print Len(A)=10
      Stack A { 
            Shift 1,-Len(A)  ' Reverse order
            Stack ' Display
      }
      Stack A {Drop 8}
      Print Len(A)=2
      Flush  ' empty current stack
      Stack A  ' dump A to current stack
      Print Stack.Size=2, Len(A)=0    
}
CheckStack

```


### Inventories as Maps

An Inventory may have values inventories,arrays, stacks, groups

```M2000 Interpreter

Module Maps {
      \\ Inventory as pairs of keys/values
      \\ keys has to be unique
      \\ Empty string "" can be used as key
      \\ Search, Add and Delete in O(1)
      \\ if we use delete we lost the order
      \\ keys can be numbers or strings, either can exist in same inventory. Values can be anything (including objects)
      \\ 0 can be used for string
      \\ Keys must be unique
      \\ a variable which hold an inventory is a pointer type
      Inventory A=10:="A",20:="B",40:="C"
      Print A$(10)="A", A$("20")="B"
      \\ split search from retrieval, using key one time
      If Exist(A,40) Then Print Eval$(A)="C"
      k=Each(A)
      While k {
            \\ print keys as strings and values
            Print Eval$(k, k^), Eval$(k)
      }
      \\ We can use Sort to sort as numbers or text
      Append A, 5:="First"
      Sort A as number
      \\ Print can print an inventory using columns
      Print A     ' First A B C
      Sort A as text
      Print A  ' A B C First
}
Maps

```



### Inventories as Sets


```M2000 Interpreter

Module Sets {
      \\ Inventory as set of keys
      \\ keys has to be unique
      \\ Empty string "" can be used as key
      \\ Search, Add and Delete in O(1)
      \\ if we use delete we lost the order
      \\ keys can be numbers or strings, either can exist in same inventory
      \\ 0 can be used for string
      \\ Keys must be unique
      \\ a variable which hold an inventory is a pointer type
      Inventory A=10,20,40
      If Exist(A,20) Then Print Eval(A)=20
      k=Each(A)
      While k {
            \\ print keys as strings and value same as key (as  number here)
            Print Eval$(k, k^), Eval(k)
      }
      \\ sort is a Quick sort
      Sort Descending A as number
      Print A   ' 40 20 10 
      \\ For no unique keys
      \\ we can't delete from anywhere.
      \\ we can drop some keys from the end only
      \\ Exist() move internal index to last of the same key
      \\ we can give values also (make it as Map)
      Inventory Queue B=1,1,1,2,2,6,10
      Drop B 3
      Print B   ' prints 1 1 1 2
      \\ sort is an insertion sort (stable)
}
Sets

```



### Using a Visual Basic 6 Collection

M2000 Interpreter is a VB6 application. We can use a VB6 collection, but only until the module where we declare it end. This is what this interpreter do with COM objects, it never make a second object reference (we can make other variables as reference, using the same object, and these reference can't changed). We can pass object through stack of values, but for a call inside the M2000 interpreter. In this example we make a lambda function which hold a closure, and this closure works only if the object exist.

We can get a list

```M2000 Interpreter

Module GetC {
      declare c collection
      def decimal aDecimal=3000032131231231312312
      Document doc$
      Print type$(c)
      \\ we get an inventory list of all methods/properties of a com Object
      m=param(c)
      IF LEN(m)>1 THEN {
      For i=0 to len(m)-1
            \\  use index, not key so i! is index
            Doc$=m$(i!)+{
            }  ' we use this block for new line
      Next i
      }
      Report Doc$
      Clipboard Doc$
      \\ so now we have to use it, using Methid to call Add
      Method c, "Add", 100, "Hello"
      Method c, "Add", 2000, "There"
      \\ add a decimal number
      
      Method c, "Add", aDecimal, "Zero"
      Method c, "count" as count
      Print count =3  ' we have three members
      Method C, "_NewEnum" as Item
      Method c, "Item", "Zero" as ZeroItem  ' we get the decimal number
      Print ZeroItem
      Print type$(Item)="Nothing"  ' we have numbers
      k=0
      While Item {
            k++
            print k, eval(item)
      }
      c.item=lambda c (akey$) ->{
            try ok {
                  method c, "item", akey$ as ret
            }
            If type$(Ret)="Empty" Then Error "Key not used"
            =ret
      }
      Print c.item("Hello")
      Try {
            val=c.item("Hello12")
      }
      Print Error$
      Push c
      \\ normaly we can use this line
      \\ but if we omit this, Interpreter do this for us
      Rem : Declare c Nothing
}
\\ we can't use a com object out of scope (because automatic change to Nothing, and the object released)
GetC
Read a
Print type$(a)="Nothing"


```

{Out}

```txt

Function QueryInterface(riid, ppvObj)
Function AddRef
Function Release
Function GetTypeInfoCount(pctinfo)
Function GetTypeInfo(itinfo, lcid, pptinfo)
Function GetIDsOfNames(riid, rgszNames, cNames, lcid, rgdispid)
Function Invoke(dispidMember, riid, lcid, wFlags, pdispparams, pvarResult, pexcepinfo, puArgErr)
Function Item(Index)
Function Add(Item, Key, Before, After)
Function Count
Function Remove(Index)
Function _NewEnum
        True
3000032131231231312312
        True
           1         100
           3        2000
           33000032131231231312312
         100
 Key not used  Problem in lambda in function C.ITEM(
        True

```



## Maple

Defining lists:

```Maple

L1 := [3, 4, 5, 6];
                     L1 := [3, 4, 5, 6]

L2 := [7, 8, 9];
                       L2 := [7, 8, 9]

```

Concatenating two lists:

```Maple

[op(L1), op(L2)]
                    [3, 4, 5, 6, 7, 8, 9]

```


Defining an Array:

```Maple

A1 := Array([3, 4, 5, 6]);
                     A1 := [3, 4, 5, 6]

```


Appending to a Vector:

```Maple

ArrayTools:-Append(A1, 7);
              A1 := [3, 4, 5, 6, 7]

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Lst = {3, 4, 5, 6}
->{3, 4, 5, 6}

PrependTo[ Lst, 2]
->{2, 3, 4, 5, 6}
PrependTo[ Lst, 1]
->{1, 2, 3, 4, 5, 6}

Lst
->{1, 2, 3, 4, 5, 6}

Insert[ Lst, X, 4]
->{1, 2, 3, X, 4, 5, 6}
```


=={{header|MATLAB}} / {{header|Octave}}==
MATLAB cell-arrays perform this function. They are indexed like arrays, but are able to hold any data type. In any cell simultaneously with any other data type. In essence they cell-arrays are sets.

Sample Usage:

```MATLAB>>
 A = {2,'TPS Report'} %Declare cell-array and initialize

A = 

    [2]    'TPS Report'

>> A{2} = struct('make','honda','year',2003)

A = 

    [2]    [1x1 struct]

>> A{3} = {3,'HOVA'} %Create and assign A{3}

A = 

    [2]    [1x1 struct]    {1x2 cell}

>> A{2} %Get A{2}

ans = 

    make: 'honda'
    year: 2003
```

'''Bold text'''


## NetRexx

NetRexx can take advantage of Java's <tt>Collection</tt> classes.  This example uses the <tt>Set</tt> interface backed by a <tt>HashSet</tt>:

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

myVals = [ 'zero', 'one', 'two', 'three', 'four', 'five' ]
mySet = Set
mySet = HashSet()

loop val over myVals
  mySet.add(val)
  end val

loop val over mySet
  say val
  end val

return

```



## Nim


### Array

Length is known at compile time

```nim
var a = [1,2,3,4,5,6,7,8,9]
var b: array[128, int]
b[9] = 10
b[0..8] = a
var c: array['a'..'d', float] = [1.0, 1.1, 1.2, 1.3]
c['b'] = 10000
```



### Seq

Variable length sequences

```nim
var d = @[1,2,3,5,6,7,8,9]
d.add(10)
d.add([11,12,13,14])
d[0] = 0

var e: seq[float] = @[]
e.add(15.5)

var f = newSeq[string]()
f.add("foo")
f.add("bar")
```



### Tuple

Fixed length, named

```nim
var g = (13, 13, 14)
g[0] = 12

var h: tuple[key: string, val: int] = ("foo", 100)

# A sequence of key-val tuples:
var i = {"foo": 12, "bar": 13}
```



### Set

Bit vector of ordinals

```nim
var j: set[char]
j.incl('X')

var k = {'a'..'z', '0'..'9'}

j = j + k
```



### Tables

Hash tables (there are also ordered hash tables and counting hash tables)

```nim
import tables
var l = initTable[string, int]()
l["foo"] = 12
l["bar"] = 13

var m = {"foo": 12, "bar": 13}.toTable
m["baz"] = 14
```



### Sets

Hash sets (also ordered hash sets)

```nim
import sets
var n = initSet[string]()
n.incl("foo")

var o = ["foo", "bar", "baz"].toSet
o.incl("foobar")
```



### Queues


```nim
import queues
var p = initQueue[int]()
p.add(12)
p.add(13)
```



## Objeck


### vector


```objeck

values := IntVector->New();
values->AddBack(7);
values->AddBack(3);
values->AddBack(10);

```



### linked list


```objeck

values := IntList->New();
values->AddBack(7);
values->AddBack(3);
values->AddBack(10);

```



### hash


```objeck

values := StringHash->New();
values->Insert("seven", IntHolder->New(7));
values->Insert("three", IntHolder->New(3));
values->Insert("ten", IntHolder->New(10));

```



### stack


```objeck

values := IntStack->New();
values->Push(7);
values->Push(3);
values->Push(10);

```


=={{header|Objective-C}}==
OpenStep (and ''derivates'' like GNUstep and Cocoa) has several collection classes; here we show
* '''a set''': a collection of unique elements (like mathematical set). Possible operations on a set are not shown;
* '''a counted set''' (also known as '''bag'''): each elements have a counter that says how many time that element appears;
* '''a dictionary''': pairs key-value.
Arrays (indexed by an integer), which are also collections, are not shown here.


```objc>#import <Foundation/Foundation.h


void show_collection(id coll)
{
  for ( id el in coll )
  {
    if ( [coll isKindOfClass: [NSCountedSet class]] ) {
      NSLog(@"%@ appears %lu times", el, [coll countForObject: el]);
    } else if ( [coll isKindOfClass: [NSDictionary class]] ) {
      NSLog(@"%@ -> %@", el, coll[el]);
    } else {
      NSLog(@"%@", el);
    }
  }
  printf("\n");
}

int main()
{
  @autoreleasepool {
  
    // create an empty set
    NSMutableSet *set = [[NSMutableSet alloc] init];
    // populate it
    [set addObject: @"one"];
    [set addObject: @10];
    [set addObjectsFromArray: @[@"one", @20, @10, @"two"] ];
    // let's show it
    show_collection(set);

    // create an empty counted set (a bag)
    NSCountedSet *cset = [[NSCountedSet alloc] init];
    // populate it
    [cset addObject: @"one"];
    [cset addObject: @"one"];
    [cset addObject: @"two"];
    // show it
    show_collection(cset);

    // create a dictionary
    NSMutableDictionary *dict = [[NSMutableDictionary alloc] init];
    // populate it
    dict[@"four"] = @4;
    dict[@"eight"] = @8;
    // show it
    show_collection(dict);

  }
  return EXIT_SUCCESS;
}
```


{{out}} (stripped the left-sided log info):

```txt

two
20
10
one

two appears 1 times
one appears 2 times

eight -> 8
four -> 4
```



## OCaml


Lists are written like so:

```ocaml
[1; 2; 3; 4; 5]
```


To prepend a single element to a list, use the '''::''' operator:

```ocaml
1 :: [2; 3; 4; 5]
```


To concatenate two lists, use '''@''':

```ocaml
[1; 2] @ [3; 4; 5]
```


To concatenate a whole list of lists, use [http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html#VALflatten List.flatten]:


```ocaml
# List.flatten [[1; 2]; [3; 4]; [5; 6; 7]] ;;
- : int list = [1; 2; 3; 4; 5; 6; 7]
```


Being a ''functional'' programming language, the list is one of the most important collection type. And being an ''impure'' functional programming language there are also imperative collection type, as for example, arrays:

```ocaml
[| 1; 2; 3; 4; 5 |]
```


The [http://code.google.com/p/ocaml-extlib/ extlib] also provides a type [http://ocaml-extlib.googlecode.com/svn/doc/apiref/Enum.html Enum.t].


## Oforth


Collection is a class.
Into the lang package, subclasses are : 

   Buffer		A collection of bytes
      Mem		A mutable collection of bytes
   Interval             A first value, a last value and a step.
   Pair                 A collection of 2 elements (with key/value features).
   List			A collection of n elements
      ListBuffer        A mutable collection of n elements that can grow when necessary
   String               A collection of n characters
      Symbol            A collection of n characters that are identity (if they are equal, they are the same object).
      StringBuffer      A mutable collection of n charaters that can grow when necessary

There is no Array collection : an immutable array is a list (which is immutable) an a mutable array is a ListBuffer.

A List (or a Pair) can be created using the following syntax : 


```Oforth
[ 1, 1.2, "abcd", [ 1, 2, 3 ] ]
```


In order to add values to a collection, you have to use a ListBuffer (a mutable collection) :


```Oforth
ListBuffer new dup add(10) dup add("aaa") dup add(Date now) dup add(1.3) println 
```


{{out}}

```txt

[10, aaa, 2015-02-02 14:02:17,047, 1.3]

```



## ooRexx

ooRexx has multiple classes that are collections of other objects with different access and storage characteristics.
;Arrays
ooRexx arrays are sequential lists of object references.  The index values are the numeric position (1-based) within the array.
A given array may be sparse and arrays will be automatically expanded as needed.

```ooRexx

a = .array~new(4)   -- creates an array of 4 items, with all slots empty
say a~size a~items  -- size is 4, but there are 0 items
a[1] = "Fred"       -- assigns a value to the first item
a[5] = "Mike"       -- assigns a value to the fifth slot, expanding the size
say a~size a~items  -- size is now 5, with 2 items

```


;Lists
Lists are non-sparse sequential lists of object references.  Item can be inserted or deleted at any position
and the positions will be adjusted accordingly.  Lists are indexed using index cookies that are assigned when
an entry is added to the list and can be used to access entries or traverse through the list.

```ooRexx

l = .list~new       -- lists have no inherent size
index = l~insert('123')  -- adds an item to this list, returning the index
l~insert('Fred', .nil)   -- inserts this at the beginning
l~insert('Mike')         -- adds this to the end
l~insert('Rick', index)  -- inserts this after '123'
l[index] = l[index] + 1  -- the original item is now '124'
do item over l           -- iterate over the items, displaying them in order
  say item
end

```

{{out}}

```txt

Fred
124
Rick
Mike

```


;Queues
Queues are non-sparse sequential lists of object references.  The index values are by numeric position (1-based), although access to
items is traditionally done by pushing or popping objects.

```ooRexx

q = .queue~of(2,4,6)   -- creates a queue containing 3 items
say q[1] q[3]          -- displays "2 6"
i = q~pull             -- removes the first item
q~queue(i)             -- adds it to the end
say q[1] q[3]          -- displays "4 2"
q[1] = q[1] + 1        -- updates the first item
say q[1] q[3]          -- displays "5 2"

```


;Tables
Tables are collections that create a one-to-one relationship between an index object and a referenced object.
Although frequently used with string indexes, the index object can be of any class, with index identity determined by the "==" method.

```ooRexx

t = .table~new
t['abc'] = 1
t['def'] = 2
say t['abc'] t['def']  -- displays "1 2"

```


;Relations
Relation collections create one-to-many data relationships.  An addition to the collection will always create a new entry.

```ooRexx

t = .table~new        -- a table example to demonstrate the difference
t['abc'] = 1          -- sets an item at index 'abc'
t['abc'] = 2          -- updates that item
say t~items t['abc']  -- displays "1 2"
r = .relation~new
r['abc'] = 1          -- sets an item at index 'abc'
r['abc'] = 2          -- adds an additional item at the same index
say r~items r['abc']  -- displays "2 2" this has two items in it now

do item over r~allAt('abc')   -- retrieves all items at the index 'abc'
  say item
end

```


;Directories
Directory objects are like tables, but the index values must always be string objects.

```ooRexx

d = .directory~new
d['abc'] = 1
d['def'] = 2
say d['abc'] d['def']  -- displays "1 2"

```

Directory objects also support an UNKNOWN method that map messages to directory index entries.  This allows
values to be set as if they were object attributes.  The following example is another way of doing the same as
the first example:

```ooRexx

d = .directory~new
d~abc = 1
d~def = 2
say d~abc d~def  -- displays "1 2"

```

Note that the index entries created in the example are the uppercase 'ABC' and 'DEF'.

;Sets
Sets are unordered collections where the items added to the collection are unique values.  Duplicate additions are collapsed to just a
single item.  Sets are useful for collecting unique occurrences of items.

```ooRexx

s = .set~new
text = "the quick brown fox jumped over the lazy dog"
do word over text~makearray(' ')
   s~put(word)
end

say "text has" text~words", but only" s~items "unique words"

```



## Oz

The most important collection types are lists, records, dictionaries and arrays:

```oz
declare
  %% Lists (immutable, recursive)
  Xs = [1 2 3 4]
  %% Add element at the front (cons)
  Xs0 = 0|Xs
  {Show {Length Xs}} %% output: 4

  %% Records (immutable maps with a label)
  Rec = label(1:2 symbol:3)
  {Show Rec}   %% output: label(2 symbol:3)
  {Show Rec.1} %% output: 2
  %% create a new record with an added field
  Rec2 = {AdjoinAt Rec 2 value}
  {Show Rec2}  %% output: label(2 value symbol:3)

  %% Dictionaries (mutable maps)
  Dict = {Dictionary.new}
  Dict.1 := 1
  Dict.symbol := 3
  {Show Dict.1} %% output: 1

  %% Arrays (mutable with integer keys)
  Arr = {Array.new 1 10 initValue}
  Arr.1 := 3
  {Show Arr.1} %% output: 3
```


There are also [http://www.mozart-oz.org/home/doc/base/tuple.html tuples] (records with consecutive integer keys starting with 1), [http://www.mozart-oz.org/documentation/base/weakdictionary.html weak dictionaries], [http://www.mozart-oz.org/documentation/mozart-stdlib/adt/queue.html queues] and [http://www.mozart-oz.org/documentation/mozart-stdlib/adt/stack.html stacks].


## PARI/GP

Pari has vectors, column vectors, matrices, sets, lists, small vectors, and maps.

```parigp
v = vector(0);
v = [];
cv = vectorv(0);
cv = []~;
m = matrix(1,1);
s = Set(v);
l = List(v);
vs = vectorsmall(0);
M = Map()
```

Adding members:

```parigp
listput(l, "hello world")
v=concat(v, [1,2,3]);
v=concat(v, 4);
mapput(M, "key", "value");
```



## Pascal

Different implementations of Pascal have various containers. 

### Array 


```Pascal
var
  MyArray: array[1..5] of real;
begin
  MyArray[1] := 4.35;
end;
```


### Dynamic Array 


```Pascal
var
  MyArray: array of integer;
begin
  setlength (MyArray, 10);
  MyArray[4] := 99;
end;
```


### Record 


```Pascal
var
  MyRecord: record
              x, y, z: real;
              presence: boolean;
            end;
begin
  MyRecord.x :=  0.3;
  MyRecord.y :=  3.2;
  MyRecord.z := -4.0;
  MyRecord.presence := true;
end;
```



### Set 


```Pascal
type
  days = (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
var
  workDays, week, weekendDays: set of days;
begin
  workdays := [Mon, Tue, Wed, Thu, Fri];
  week := workdays + [Sat, Sun];
  weekendDays := week - workdays;
end;
```



### String 


```Pascal
var
  MyString: String;
begin
  MyString:= 'Some Text';
end;
```


### List 

{{works with|Free_Pascal}}
{{libheader|Classes}}

```Pascal
program ListDemo;
uses
  classes;
var
  MyList: TList;
  a, b, c: integer;
  i: integer;
begin
  a := 1;
  b := 2;
  c := 3;
  MyList := TList.Create;
  MyList.Add(@a);
  MyList.Add(@c);
  MyList.Insert(1, @b);
  for i := MyList.IndexOf(MyList.First) to MyList.IndexOf(MyList.Last) do
    writeln (integer(MyList.Items[i]^));
  MyList.Destroy;
end.
```


{{out}}

```txt
% ./ListDemo
1
2
3
```



### Collection

Example from the documentation of the FreePascal runtime library.
{{works with|Free_Pascal}}
{{libheader|Objects}}

```Pascal
Program ex34;

{ Program to demonstrate the TCollection.AtInsert method }

Uses Objects, MyObject; { For TMyObject definition and registration }

Var C : PCollection;
    M : PMyObject;
    I : Longint;

Procedure PrintField (Dummy : Pointer; P : PMyObject);

begin
  Writeln ('Field : ',P^.GetField);
end;

begin
  Randomize;
  C:=New(PCollection, Init(120, 10));
  Writeln ('Inserting 100 records at random places.');
  For I:=1 to 100 do
    begin
    M:=New(PMyObject, Init);
    M^.SetField(I-1);
    If I=1 then
      C^.Insert(M)
    else
      With C^ do
        AtInsert(Random(Count), M);
    end;
  Writeln ('Values : ');
  C^.Foreach(@PrintField);
  Dispose(C, Done);
end.
```



## Perl

Perl has ''array'' and ''hashes''.


```perl
use strict;
my @c = (); # create an empty "array" collection

# fill it
push @c, 10, 11, 12;
push @c, 65;
# print it
print join(" ",@c) . "\n";

# create an empty hash
my %h = ();
# add some pair
$h{'one'} = 1;
$h{'two'} = 2;
# print it
foreach my $i ( keys %h ) {
    print $i . " -> " . $h{$i} . "\n";
}
```



## Perl 6

Perl 6 has both mutable and immutable containers of various sorts.  Here are some of the most common ones:

### Mutable


```perl6
# Array
my @array = 1,2,3;
@array.push: 4,5,6;

# Hash
my %hash = 'a' => 1, 'b' => 2;
%hash<c d> = 3,4;
%hash.push: 'e' => 5, 'f' => 6;

# SetHash
my $s = SetHash.new: <a b c>;
$s ∪= <d e f>;

# BagHash
my $b = BagHash.new: <b a k l a v a>;
$b ⊎= <a b c>;
```



### Immutable


```perl6
# List
my @list := 1,2,3;
my @newlist := |@list, 4,5,6; # |@list will slip @list into the surrounding list instead of creating a list of lists

# Set
my $set = set <a b c>;
my $newset = $set ∪ <d e f>;

# Bag
my $bag = bag <b a k l a v a>;
my $newbag = $bag ⊎ <b e e f>;
```


===Pair list (cons list)===

```perl6
my $tail = d => e => f => Nil;
my $new = a => b => c => $tail;
```


===P6opaque object (immutable in structure)===

```perl6
class Something { has $.foo; has $.bar };
my $obj = Something.new: foo => 1, bar => 2;
my $newobj = $obj but role { has $.baz = 3 } # anonymous mixin
```



## Phix

Collections can simply be stored as sequences

```Phix
sequence collection = {}
collection = append(collection,"one")
collection = prepend(collection,2)
? collection -- {2,"one"}
```

If you want uniqueness, you could simply use a dictionary with values of 0:

```Phix
setd("one",0)
setd(2,0)
function visitor(object key, object /*data*/, object /*user_data*/)
    ?key
    return 1
end function
traverse_dict(routine_id("visitor"))    -- shows 2, "one"
```



## PHP

PHP has ''associative arrays'' as collection


```php
<?php
$a = array();
# add elements "at the end"
array_push($a, 55, 10, 20);
print_r($a);
# using an explicit key
$a['one'] = 1;
$a['two'] = 2;
print_r($a);
?>
```


{{out}}

```txt
Array
(
    [0] => 55
    [1] => 10
    [2] => 20
)
Array
(
    [0] => 55
    [1] => 10
    [2] => 20
    [one] => 1
    [two] => 2
)
```



## PicoLisp

The direct way in PicoLisp is a linear list (other possibilities could involve
[http://software-lab.de/doc/refI.html#idx index] trees or
[http://software-lab.de/doc/ref.html#symbol property] lists).

```PicoLisp
: (setq Lst (3 4 5 6))
-> (3 4 5 6)

: (push 'Lst 2)
-> 2

: (push 'Lst 1)
-> 1

: Lst
-> (1 2 3 4 5 6)

: (insert 4 Lst 'X)
-> (1 2 3 X 4 5 6)
```



## PL/I


```PL/I

   declare countries character (20) varying controlled;
   allocate countries initial ('Britain');
   allocate countries initial ('America');
   allocate countries initial ('Argentina');

```



## PowerShell

The most common collection types in PowerShell are arrays and hash tables.

### Array

The array index is zero based.

```PowerShell

# Create an Array by separating the elements with commas:
$array = "one", 2, "three", 4

# Using explicit syntax:
$array = @("one", 2, "three", 4)

# Send the values back into individual variables:
$var1, $var2, $var3, $var4 = $array

# An array of several integer ([int]) values:
$array = 0, 1, 2, 3, 4, 5, 6, 7

# Using the range operator (..):
$array = 0..7

# Strongly typed:
[int[]] $stronglyTypedArray = 1, 2, 4, 8, 16, 32, 64, 128

# An empty array:
$array = @()

# An array with a single element:
$array = @("one")

# I suppose this would be a jagged array:
$jaggedArray = @((11, 12, 13),
                 (21, 22, 23),
                 (31, 32, 33))

$jaggedArray | Format-Wide {$_} -Column 3 -Force

$jaggedArray[1][1] # returns 22

# A Multi-dimensional array:
$multiArray = New-Object -TypeName "System.Object[,]" -ArgumentList 6,6

for ($i = 0; $i -lt 6; $i++)
{ 
    for ($j = 0; $j -lt 6; $j++)
    { 
        $multiArray[$i,$j] = ($i + 1) * 10 + ($j + 1)
    }
}

$multiArray | Format-Wide {$_} -Column 6 -Force

$multiArray[2,2] # returns 33

```


### Hash Table

Hash tables come in two varieties: normal and ordered, where of course, the order of entry is retained.

```PowerShell

# An empty Hash Table:
$hash = @{}

# A Hash table populated with some values:
$nfcCentralDivision = @{
    Packers = "Green Bay"
    Bears   = "Chicago"
    Lions   = "Detroit"
}

# Add items to a Hash Table:
$nfcCentralDivision.Add("Vikings","Minnesota")
$nfcCentralDivision.Add("Buccaneers","Tampa Bay")

# Remove an item from a Hash Table:
$nfcCentralDivision.Remove("Buccaneers")

# Searching for items
$nfcCentralDivision.ContainsKey("Packers")
$nfcCentralDivision.ContainsValue("Green Bay")

# A bad value...
$hash1 = @{
    One = 1
    Two = 3
}

# Edit an item in a Hash Table:
$hash1.Set_Item("Two",2)

# Combine Hash Tables:

$hash2 = @{
    Three = 3
    Four  = 4
}

$hash1 + $hash2

# Using the ([ordered]) accelerator the items in the Hash Table retain the order in which they were input:
$nfcCentralDivision = [ordered]@{
    Bears   = "Chicago"
    Lions   = "Detroit"
    Packers = "Green Bay"
    Vikings = "Minnesota"
}

```


### Other Collection Types

PowerShell is a .NET language so '''all''' of the collection types in .NET are available to PowerShell.  The most commonly used would probably be <code>[System.Collections.ArrayList]</code>.

```PowerShell

$list = New-Object -TypeName System.Collections.ArrayList -ArgumentList 1,2,3

# or...

$list = [System.Collections.ArrayList]@(1,2,3)


$list.Add(4) | Out-Null
$list.RemoveAt(2)

```




## Prolog

Traditionally Prolog supports only lists.

```prolog
% create a list
L = [a,b,c,d],
 
% prepend to the list
L2 = [before_a|L],
 
% append to the list
append(L2, ['Hello'], L3),

% delete from list
exclude(=(b), L3, L4).
```

Output:

```txt

L = [a, b, c, d],
L2 = [before_a, a, b, c, d],
L3 = [before_a, a, b, c, d, 'Hello'],
L4 = [before_a, a, c, d, 'Hello'].

```


SWI-Prolog supports some other collection types as built in libraries, the most notable is the Dict.
Dicts can be accessed using a special notation and can be added and removed from in an immutable way.

```prolog
% create an empty dict call 'point'
D1 = point{},

% add a value	
D2 = D1.put(x, 20).put(y, 30).put(z, 20),

% update a value
D3 = D2.put([x=25]),

% remove a value
del_dict(z, D3, _, D4),

% access a value randomly
format('x = ~w, y = ~w~n', [D4.x, D4.y]).
```

Output:

```txt

x = 25, y = 30
D1 = point{},
D2 = point{x:20, y:30, z:20},
D3 = point{x:25, y:30, z:20},
D4 = point{x:25, y:30}.

```



## PureBasic


### Arrays

Creating an [http://www.purebasic.com/documentation/array/index.html Array] of 10 strings (could be any type). PureBasic starts the index with element 0. 

```PureBasic
Dim Text.s(9)

Text(3)="Hello"
Text(7)="World!"
```



### Linked Lists

Create a [http://www.purebasic.com/documentation/linkedlist/index.html Linked List] for strings (could be any type), then add two elements.

```PureBasic
NewList Cars.s()

AddElement(Cars()): Cars()="Volvo"
AddElement(Cars()): Cars()="BMV"
```



### Hash table

Create a [http://www.purebasic.com/documentation/map/index.html Map], e.g. a hash table that could be any type. The size of the dictionary can be defined as needed, otherwise a default value is used.

```PureBasic
NewMap Capitals.s()

Capitals("USA")   = "Washington"
Capitals("Sweden")= "Stockholm"
```



## Python

{{works with|Python|2.5}}
Python supports lists, tuples, dictionaries and now sets as built-in collection types.  See http://docs.python.org/tut/node7.html for further details.

```python
collection = [0, '1']                 # Lists are mutable (editable) and can be sorted in place
x = collection[0]                     # accessing an item (which happens to be a numeric 0 (zero)
collection.append(2)                  # adding something to the end of the list
collection.insert(0, '-1')            # inserting a value into the beginning
y = collection[0]                     # now returns a string of "-1"
collection.extend([2,'3'])            # same as [collection.append(i) for i in [2,'3']] ... but faster
collection += [2,'3']                 # same as previous line
collection[2:6]                       # a "slice" (collection of the list elements from the third up to but not including the sixth)
len(collection)                       # get the length of (number of elements in) the collection
collection = (0, 1)                   # Tuples are immutable (not editable)
collection[:]                         # ... slices work on these too; and this is equivalent to collection[0:len(collection)]
collection[-4:-1]                     # negative slices count from the end of the string
collection[::2]                       # slices can also specify a stride --- this returns all even elements of the collection
collection="some string"              # strings are treated as sequences of characters
x = collection[::-1]                  # slice with negative step returns reversed sequence (string in this case).
collection[::2] == "some string"[::2] # True, literal objects don't need to be bound to name/variable to access slices or object methods
collection.__getitem__(slice(0,len(collection),2))  # same as previous expressions.
collection = {0: "zero", 1: "one"}    # Dictionaries (Hash)
collection['zero'] = 2                # Dictionary members accessed using same syntax as list/array indexes.
collection = set([0, '1'])            # sets (Hash)
```


In addition Python classes support a number of methods allowing them to implement indexing, slicing, and attribute management features as collections. Thus many modules in the Python standard libraries allow one to treat files contents, databases, and other data using the same syntax as the native collection types. Some Python modules (such as Numeric and NumPy) provide low-level implementations of additional collections (such as efficient n-dimensional arrays).


## R

R has several types that can be considered collections.

### Vectors

Numeric (floating point)

```R
numeric(5)
1:10
c(1, 3, 6, 10, 7 + 8, sqrt(441))
```

 [1] 0 0 0 0 0
 [1]  1  2  3  4  5  6  7  8  9 10
 [1]  1  3  6 10 15 21
Integer

```R
integer(5)
c(1L, -2L, 99L);
```

 [1] 0 0 0 0 0
 [1]  1 -2 99
Logical

```R
logical(5)
c(TRUE, FALSE)
```

 [1] FALSE FALSE FALSE FALSE FALSE
 [1]  TRUE FALSE
Character

```R
character(5)
c("abc", "defg", "")
```

 [1] "" "" "" "" ""
 [1] "abc"  "defg" ""

### Arrays and Matrices

These are essentially vectors with a dimension attribute.  Matrices are just arrays with two dimensions (and a different class).

```R
matrix(1:12, nrow=3)

array(1:24, dim=c(2,3,4)) #output not shown
```

      [,1] [,2] [,3] [,4]
 [1,]    1    4    7   10
 [2,]    2    5    8   11
 [3,]    3    6    9   12

### Lists

Lists are collections of other variables (that can include other lists).  

```R
list(a=123, b="abc", TRUE, 1:5, c=list(d=runif(5), e=5+6))
```


```r
$a
[1] 123
$b
[1] "abc"
[[3]]
[1] TRUE
[[4]]
[1] 1 2 3 4 5 
$c
$c$d
[1] 0.6013157 0.5011909 0.7106448 0.3882265 0.1274939
$c$e
[1] 11
```


### Data Frames

Data frames are like a cross between a list and a matrix.  Each row represents one "record", or a collection of variables.

```R
data.frame(name=c("Alice", "Bob", "Carol"), age=c(23, 35, 17))
```

    name age
 1 Alice  23
 2   Bob  35
 3 Carol  17


## Racket


As in other lisps, the simple kind of linked lists are the most common collection-of-values type.

```Racket

#lang racket

;; create a list
(list 1 2 3 4)
;; create a list of size N
(make-list 100 0)
;; add an element to the front of a list (non-destructively)
(cons 1 (list 2 3 4))

```

Racket comes with about 7000 additional types that can be considered as a collection of values, but it's not clear whether this entry is supposed to be a laundry list...


## Raven


Numerically indexed List:


```raven
[ 1 2 3 'abc' ] as a_list
a_list print

list (4 items)
 0 => 1
 1 => 2
 2 => 3
 3 => "abc"
```


String key indexed Hash:


```raven
{ 'a' 1 'b' 2 } as a_hash
a_hash print

hash (2 items)
 a => 1
 b => 2
```


Set items:


```raven
17 a_list 1 set      # set second item
42 a_hash 'b' set    # set item with key 'b'
42 a_hash:b          # shorthand
```


Get items:


```raven
a_list 1 get         # get second item
a_hash 'b' get       # get item with key 'b'
a_hash.b             # shorthand
```


Other stuff:


```raven
42 a_list push       # append an item
a_list pop           # remove last item
42 a_list shove      # prepend an item
a_list shift         # remove first item
42 a_list 1 insert   # insert item second, shuffling others down
a_list 1 remove      # retrieve second item, shuffling others up
```



## REXX

There are several methods to store collections in REXX:
::::*   stemmed arrays   (or simply, arrays)
::::*   lists   or   vectors
::::*   sparse stemmed arrays;   (or simply, sparse arrays)

A collection can be numbers   (integer or floating point numbers),   or any
character strings   (including ''nulls'').

Indeed, even numbers in REXX are stored as characters.


### stemmed arrays

To store (say) a collection of numbers   (or anything,
for that matter)   into a stemmed array:

```rexx
pr. =                             /*define a default for all elements for the array*/

pr.1  = 2                         /*note that this array starts at   1   (one).    */
pr.2  = 3
pr.3  = 5
pr.4  = 7
pr.5  = 11
pr.6  = 13
pr.7  = 17
pr.8  = 19
pr.9  = 23
pr.10 = 29
pr.11 = 31
pr.12 = 37
pr.13 = 41
pr.14 = 43
pr.15 = 47

y.     =     0                    /*define a default for all years  (Y)  to be zero*/
y.1985 =  6020
y.1986 =  7791
y.1987 =  8244
y.1988 = 10075

x = y.2012                        /*the variable  X  will have a value of zero (0).*/

fib.0 =  0                        /*this stemmed arrays will start with  zero (0). */
fib.1 =  1
fib.2 =  1
fib.3 =  2
fib.4 =  3
fib.5 =  5
fib.6 =  8
fib.7 = 17

   do n=-5  to 5                  /*define a stemmed array from   -5    to    5    */
   sawtooth.n = n                 /*the  sawtooth  array is, well, a sawtooth curve*/
   end   /*n*/                    /*note that  eleven  elements will be defined.   */
```

Most often, programmers will assign the   zero   entry to the
number of elements in the stemmed array.

This means that any index of the stemmed array must be positive to be useful for
storing numbers.

```rexx
 pr.0= 15              /*number of (data) entries in the stemmed array. */
```

Programmatically, a simple test could be performed to detect the
end of the array   (if there aren't any   ''null''   values):

```rexx
  do j=1  while pr.j\==''
  say  'prime'   j   "is"   pr.j
  end   /*j*/
                                  /*at this point,  J=16  (because of the   DO     */
                                  /*loop incrementing the index.                   */
j= j-1                            /*J  now has the count of primes stored.         */
```



### lists or vectors

To store (say) a collection of numbers (or anything, for that matter) into a list:

```rexx
primeList = '2 3 5 7 11 13 17 19 23 29 31 37 41 43'        /* or ···  */
primeList =  2 3 5 7 11 13 17 19 23 29 31 37 41 43

                                  /*in this case, the quotes  (')  can be elided.*/

primes= words(primeList)          /*the  WORDS  BIF  counts the number of blank─ */
                                  /*separated words (in this case, prime numbers)*/
                                  /*in the  value  of the variable   "primeList".*/

  do j=1  for primes              /*can also be coded as:      do j=1  to primes */
  say 'prime'    j    "is"    word(primeList, j)
                                  /*this method  (using the   WORD   BIF) isn't  */
                                  /*very efficient for very large arrays  (those */
                                  /*with many many thousands of elements).       */
  end   /*j*/
```

The use of lists (in the above manner) is suitable for words (or numbers) that do not have
leading, embedded, or 

trailing blanks as part of their value.

One workaround is to use some other unique character   (such as an
underbar/underscore   <big>'''_'''</big>   character)   that

that can be substituted, and then later, be translated to a true blank.


### sparse stemmed arrays

To store (for instance) a collection of numbers (or anything, for that matter) into
a sparse stemmed array:

```rexx
pr.   = 0                      /*define a default for all elements for the array.*/
pr.2  = 1
pr.3  = 1
pr.5  = 1
pr.7  = 1
pr.11 = 1
pr.13 = 1
pr.17 = 1
pr.19 = 1
pr.23 = 1
pr.29 = 1
pr.31 = 1
pr.37 = 1
pr.41 = 1
pr.43 = 1
pr.47 = 1
/*──────────────────────────────────────────────────────────────────────────────────────*/
primes=0
         do j=1  for 10000                       /*this method isn't very efficient.    */
         if \pr.j  then iterate                  /*Not prime?   Then skip this element. */
         primes = primes + 1                     /*bump the number of primes  (counter).*/
         end   /*j*/
                                                 /*note that the  10000  is a small "∞".*/
say '# of primes in list:'  primes
/*──────────────────────────────────────────────────────────────────────────────────────*/
#primes=0
         do j=1  for 10000                       /*this method is not very efficient.   */
         if pr.j\==0  then #primes = #primes + 1 /*Not zero?  Bump the number of primes.*/
         end   /*j*/                             /* [↑]  not as idiomatic as 1st program*/

say '# of primes in the list:'  #primes
/*──────────────────────────────────────────────────────────────────────────────────────*/
Ps=0
         do k=1  for 10000                       /*and yet another inefficient method.  */
         if pr.k==0  then iterate                /*Not a prime?  Then skip this element.*/
         Ps = Ps + 1                             /*bump the counter for the # of primes.*/
         say 'prime'  Ps  "is:"  k               /*might as well echo this prime number.*/
         end   /*k*/

say 'The number of primes found in the list is '     Ps
/*──────────────────────────────────────────────────────────────────────────────────────*/
pr.0 = 47                                        /*hardcode the highest prime in array. */
# = 0
         do k=2  to  pr.0                        /*and much more efficient method.      */
         if \pr.k  then iterate                  /*Not a prime?  Then skip this element.*/
         # = # + 1                               /*bump the counter for the # of primes.*/
         say 'prime'  Ps  "is:"  k               /*might as well echo this prime number.*/
         end   /*k*/

say 'The number of primes found in the list is: '    #
```





## Ring


```ring

text = list(2)
text[1] = "Hello "
text[2] = "world!"
see text[1] + text[2] + nl

```

Output:

```txt

Hello world!

```



## Ruby



### Array

Arrays are ordered, integer-indexed collections of any object.

```ruby
# creating an empty array and adding values
a = []              #=> []
a[0] = 1            #=> [1]
a[3] = "abc"        #=> [1, nil, nil, "abc"]
a << 3.14           #=> [1, nil, nil, "abc", 3.14]

# creating an array with the constructor
a = Array.new               #=> []
a = Array.new(3)            #=> [nil, nil, nil]
a = Array.new(3, 0)         #=> [0, 0, 0]
a = Array.new(3){|i| i*2}   #=> [0, 2, 4]
```



### Hash

A Hash is a dictionary-like collection of unique keys and their values. Also called associative arrays, they are similar to Arrays, but where an Array uses integers as its index, a Hash allows you to use any object type.

```ruby
# creating an empty hash
h = {}              #=> {}
h["a"] = 1          #=> {"a"=>1}
h["test"] = 2.4     #=> {"a"=>1, "test"=>2.4}
h[3] = "Hello"      #=> {"a"=>1, "test"=>2.4, 3=>"Hello"}
h = {a:1, test:2.4, World!:"Hello"}
                    #=> {:a=>1, :test=>2.4, :World!=>"Hello"}

# creating a hash with the constructor
h = Hash.new        #=> {}   (default value : nil)
p h[1]              #=> nil
h = Hash.new(0)     #=> {}   (default value : 0)
p h[1]              #=> 0
p h                 #=> {}
h = Hash.new{|hash, key| key.to_s}
                    #=> {}
p h[123]            #=> "123"
p h                 #=> {}
h = Hash.new{|hash, key| hash[key] = "foo#{key}"}
                    #=> {}
p h[1]              #=> "foo1"
p h                 #=> {1=>"foo1"}
```



### Struct

A Struct is a convenient way to bundle a number of attributes together, using accessor methods, without having to write an explicit class.

```ruby
# creating a struct

Person = Struct.new(:name, :age, :sex)

a = Person.new("Peter", 15, :Man)
p a[0]              #=> "Peter"
p a[:age]           #=> 15
p a.sex             #=> :Man
p a.to_a            #=> ["Peter", 15, :Man]
p a.to_h            #=> {:name=>"Peter", :age=>15, :sex=>:Man}

b = Person.new
p b                 #=> #<struct Person name=nil, age=nil, sex=nil>
b.name = "Margaret"
b["age"] = 18
b[-1] = :Woman
p b.values          #=> ["Margaret", 18, :Woman]
p b.members         #=> [:name, :age, :sex]
p b.size            #=> 3

c = Person["Daniel", 22, :Man]
p c.to_h            #=> {:name=>"Daniel", :age=>22, :sex=>:Man}
```



### Set

Set implements a collection of unordered values with no duplicates. This is a hybrid of Array's intuitive inter-operation facilities and Hash's fast lookup.


```ruby
require 'set'

# different ways of creating a set
p s1 = Set[1, 2, 3, 4]          #=> #<Set: {1, 2, 3, 4}>
p s2 = [8, 6, 4, 2].to_set      #=> #<Set: {8, 6, 4, 2}>
p s3 = Set.new(1..4) {|x| x*2}  #=> #<Set: {2, 4, 6, 8}>

# Union
p s1 | s2                       #=> #<Set: {1, 2, 3, 4, 8, 6}>
# Intersection
p s1 & s2                       #=> #<Set: {4, 2}>
# Difference
p s1 - s2                       #=> #<Set: {1, 3}>

p s1 ^ s2                       #=> #<Set: {8, 6, 1, 3}>

p s2 == s3                      #=> true

p s1.add(5)                     #=> #<Set: {1, 2, 3, 4, 5}>
p s1 << 0                       #=> #<Set: {1, 2, 3, 4, 5, 0}>
p s1.delete(3)                  #=> #<Set: {1, 2, 4, 5, 0}>
```



### Matrix and Vector

The Matrix and Vector class represents a mathematical matrix and vector.

```ruby
require 'matrix'

# creating a matrix
p m0 = Matrix.zero(3)       #=> Matrix[[0, 0, 0], [0, 0, 0], [0, 0, 0]]
p m1 = Matrix.identity(3)   #=> Matrix[[1, 0, 0], [0, 1, 0], [0, 0, 1]]
p m2 = Matrix[[11, 12], [21, 22]]
                            #=> Matrix[[11, 12], [21, 22]]
p m3 = Matrix.build(3) {|row, col| row - col}
                            #=> Matrix[[0, -1, -2], [1, 0, -1], [2, 1, 0]]

p m2[0,0]               #=> 11
p m1 * 5                #=> Matrix[[5, 0, 0], [0, 5, 0], [0, 0, 5]]
p m1 + m3               #=> Matrix[[1, -1, -2], [1, 1, -1], [2, 1, 1]]
p m1 * m3               #=> Matrix[[0, -1, -2], [1, 0, -1], [2, 1, 0]]

# creating a Vector
p v1 = Vector[1,3,5]    #=> Vector[1, 3, 5]
p v2 = Vector[0,1,2]    #=> Vector[0, 1, 2]
p v1[1]                 #=> 3
p v1 * 2                #=> Vector[2, 6, 10]
p v1 + v2               #=> Vector[1, 4, 7]

p m1 * v1               #=> Vector[1, 3, 5]
p m3 * v1               #=> Vector[-13, -4, 5]
```



### OpenStruct

An OpenStruct is a data structure, similar to a Hash, that allows the definition of arbitrary attributes with their accompanying values.

```ruby
require 'ostruct'

# creating a OpenStruct
ab = OpenStruct.new
p ab                #=> #<OpenStruct>
ab.foo = 25
p ab.foo            #=> 25
ab[:bar] = 2
p ab["bar"]         #=> 2
p ab                #=> #<OpenStruct foo=25, bar=2>
ab.delete_field("foo")
p ab.foo            #=> nil
p ab                #=> #<OpenStruct bar=2>

p son = OpenStruct.new({ :name => "Thomas", :age => 3 })
                    #=> #<OpenStruct name="Thomas", age=3>
p son.name          #=> "Thomas"
p son[:age]         #=> 3
son.age += 1
p son.age           #=> 4
son.items = ["candy","toy"]
p son.items         #=> ["candy","toy"]
p son               #=> #<OpenStruct name="Thomas", age=4, items=["candy", "toy"]
```



## Rust

Rust has quite a few collections built in.
===Stack-allocated collections===

### =Array=

Arrays (<code>[T]</code>) are stack allocated, fixed size collections of items of the same type.

```rust
let a = [1u8,2,3,4,5]; // a is of type [u8; 5];
let b = [0;256] // Equivalent to `let b = [0,0,0,0,0,0... repeat 256 times]`
```


### =Slice=

Slices (<code>&[T]</code>) are dynamically sized views into contiguous sequences (arrays, vectors, strings)

```rust
let array = [1,2,3,4,5];
let slice = &array[0..2]
println!("{:?}", slice);
```

{{out}}

```txt
[1,2]
```


### =String slice=

String slices are (<code>str</code>) are slices of Unicode characters. Plain <code>str</code>s are almost never seen in Rust. Instead either heap-allocated <code>String</code>s or borrowed string slices (<code>&str</code> which is basically equivalent to a slice of bytes: <code>&[u8]</code>) are more often used. It should be noted that strings are not indexable as they are UTF-8 (meaning that characters are not necessarily of a fixed size) however iterators can be created over codepoints or graphemes.

===Heap-allocated collections===

### =Vector=

Vectors (<code>Vec<T></code>) are a growable list type. According to the Rust documentation, you want to use a Vector if: 

* You want to collect items up to be processed or sent elsewhere later, and don't care about any properties of the actual values being stored.
* You want a sequence of elements in a particular order, and will only be appending to (or near) the end.
* You want a stack.
* You want a resizable array.
* You want a heap-allocated array.


```rust
let mut v = Vec::new();
v.push(1);
v.push(2);
v.push(3);
// Or (mostly) equivalently via a convenient macro in the standard library
let v = vec![1,2,3];
```


### =String=

<code>String</code>s are growable strings stored as a UTF-8 buffer which are just <code>Vec<u8></code>s under the hood. Like <code>str</code>s, they are not indexable (for the same reasons) but iterators can be created over the graphemes, codepoints or bytes therein.

```rust
let x = "abc"; // x is of type &str (a borrowed string slice)
let s = String::from(x);
// or alternatively
let s = x.to_owned();
```



### =VecDequeue=

A growable ring buffer. According to the Rust documentation you should use <code>VecDequeue<T></code> when:
* You want a Vec that supports efficient insertion at both ends of the sequence.
* You want a queue.
* You want a double-ended queue (deque).

### =Linked List=

A doubly-linked list. According to the Rust documentation, you should use it when:
* You want a Vec or VecDeque of unknown size, and can't tolerate amortization.
* You want to efficiently split and append lists.
* You are absolutely certain you really, truly, want a doubly linked list.

### =HashMap=

A hash map implementation which uses linear probing with Robin Hood bucket stealing. According to the Rust documentation, you should use it when: 
* You want to associate arbitrary keys with an arbitrary value.
* You want a cache.
* You want a map, with no extra functionality.

### =BTreeMap=

A map based on a B-Tree. According to the Rust documentation, you should use it when: 
* You're interested in what the smallest or largest key-value pair is.
* You want to find the largest or smallest key that is smaller or larger than something.
* You want to be able to get all of the entries in order on-demand.
* You want a sorted map.

### =HashSet/BTreeSet=

Set implementations that use an empty tuple <code>()</code> as the value of their respective maps (and implement different methods). They should be used when: 
* You just want to remember which keys you've seen.
* There is no meaningful value to associate with your keys.
* You just want a set.

### =BinaryHeap=

A priority queue implemented with a binary heap. You should use it when
* You want to store a bunch of elements, but only ever want to process the "biggest" or "most important" one at any given time.
* You want a priority queue.


## Scala

{{libheader|Scala}}Scala has in his run-time library a rich set set of collections. Due to use of traits is this library easily realized and consistent. Collections provide the same operations on any type where it makes sense to do so. For instance, a string is conceptually a sequence of characters. Consequently, in Scala collections, strings support all sequence operations. The same holds for arrays.

The collections are available in two flavors; immutable (these have no methods to modify or update) and mutable. With these properties they are also available in concurrent version for parallel processing. Switching between sequential and parallel can easily be done by adding a .seq or .par post-fix. 

These examples were taken from a Scala REPL session. The second lines are the REPL responces.
```Scala
Windows PowerShell
Copyright (C) 2012 Microsoft Corporation. All rights reserved.

PS C:\Users\FransAdm> scala
Welcome to Scala version 2.10.1 (Java HotSpot(TM) 64-Bit Server VM, Java 1.7.0_25).
Type in expressions to have them evaluated.
Type :help for more information.

scala> // Immutable collections do not and cannot change the instantiated object

scala> // Lets start with Lists

scala> val list = Nil // Empty List
list: scala.collection.immutable.Nil.type = List()

scala> val list2 = List("one", "two") // List with two elements (Strings)
list2: List[String] = List(one, two)

scala> val list3 = 3 :: list2 // prepend 3 to list2, using a special operator
list3: List[Any] = List(3, one, two)

scala> // The result was a mixture with a Int and Strings, so the common superclass Any is used.

scala> // Let test the Set collection

scala> val set = Set.empty[Char] // Empty Set of Char type
set: scala.collection.immutable.Set[Char] = Set()

scala> val set1 = set + 'c' // add an element
set1: scala.collection.immutable.Set[Char] = Set(c)

scala> val set2 = set + 'a' + 'c' + 'c' // try to add another and  the same element twice
set2: scala.collection.immutable.Set[Char] = Set(a, c)

scala> // Let's look at the most universal map: TrieMap (Cache-aware lock-free concurrent hash trie)

scala> val capital = collection.concurrent.TrieMap("US" -> "Washington", "France" -> "Paris") // This map is mutable
capital: scala.collection.concurrent.TrieMap[String,String] = TrieMap(US -> Washington, France -> Paris)

scala> capital - "France" // This is only an expression, does not modify the map itself
res0: scala.collection.concurrent.TrieMap[String,String] = TrieMap(US -> Washington)

scala> capital += ("Tokio" -> "Japan") // Adding an element, object is changed - not the val capital
res1: capital.type = TrieMap(US -> Washington, Tokio -> Japan, France -> Paris)

scala> capital // Check what we have sofar
res2: scala.collection.concurrent.TrieMap[String,String] = TrieMap(US -> Washington, Tokio -> Japan, France -> Paris)

scala>  val queue = new scala.collection.mutable.Queue[String]
queue: scala.collection.mutable.Queue[String] = Queue()

scala> queue += "first"
res17: queue.type = Queue("first")

scala> queue += "second"
res19: queue.type = Queue("first", "second")

scala>
```


```Scala
    import collection.concurrent.TrieMap

    // super concurrent mutable hashmap
    val map = TrieMap("Amsterdam" -> "Netherlands",
      "New York" -> "USA",
      "Heemstede" -> "Netherlands")

    map("Laussanne") = "Switzerland" // 2 Ways of updating
    map += ("Tokio" -> "Japan")

    assert(map("New York") == "USA")
    assert(!map.isDefinedAt("Gent")) // isDefinedAt is false
    assert(map.isDefinedAt("Laussanne")) // true

    val hash = new TrieMap[Int, Int]
    hash(1) = 2
    hash += (1 -> 2) // same as hash(1) = 2
    hash += (3 -> 4, 5 -> 6, 44 -> 99)
    hash(44) // 99
    hash.contains(33) // false
    hash.isDefinedAt(33) // same as contains
    hash.contains(44) // true
    // iterate over key/value
    //    hash.foreach { case (key, val) => println(  "key " + e._1 + " value " + e._2) } // e is a 2 element Tuple
    // same with for syntax
    for ((k, v) <- hash) println("key " + k + " value " + v)
    //    // items in map where the key is greater than 3
        map.filter { k => k._1 > 3 } //  Map(5 -> 6, 44 -> 99)
    //    // same with for syntax
        for ((k, v) <- map; if k > 3) yield (k, v)
```



## Scheme


### list


```scheme
(list obj ...)
```

returns a newly allocated list of its arguments.

Example:

```scheme
(display (list 1 2 3))
(newline)
(display (list))
(newline)
```

{{out}}

```txt
(1 2 3)
()
```



### cons


```scheme
(cons obj lst)
```

returns a newly allocated list consisting of <code>obj</code> prepended to <code>lst</code>.

Example:

```scheme
(display (cons 0 (list 1 2 3)))
(newline)
```

{{out}}

```txt
(0 1 2 3)
```


### append


```scheme
(append lst ...)
```

returns a newly allocated list consisting of the elements of <code>lst</code> followed by the elements of the other lists.

Example:

```scheme
(display (append (list 1 2 3) (list 4 5 6)))
(newline)
```

{{out}}

```txt
(1 2 3 4 5 6)
```



## Seed7



### set


```seed7
$ include "seed7_05.s7i";

enable_output(set of string);

const proc: main is func
  local
    var set of string: aSet is {"iron", "copper"};
  begin
    writeln(aSet);
    incl(aSet, "silver");
    writeln(aSet);
  end func;
```



### array


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var array string: anArray is [] ("iron", "copper");
    var string: element is "";
  begin
    for element range anArray do
      write(element <& " ");
    end for;
    writeln;
    anArray &:= "silver";
    for element range anArray do
      write(element <& " ");
    end for;
    writeln;
  end func;
```



### hash


```seed7
$ include "seed7_05.s7i";

const type: aHashType is hash [string] string;

const proc: main is func
  local
    var aHashType: aHash is aHashType.value;
    var string: aValue is "";
    var string: aKey is "";
  begin
    aHash @:= ["gold"] "metal";
    aHash @:= ["helium"] "noble gas";
    for aValue key aKey range aHash do
      writeln(aKey <& ": " <& aValue);
    end for;a
  end func;
```



## Setl4



### Set


```setl4

set = new('set 5 10 15 20 25 25')
add(set,30)
show(set)

show.eval('member(set,5)')
show.eval('member(set,6)')

show.eval("exists(set,'eq(this,10)')")
show.eval("forall(set,'eq(this,40)')")

```


### Iter


```setl4

iter = new('iter 1 10 2')
show(iter)
show.eval("eq(set.size(iter),5)")
show.eval('member(iter,5)')

```


### Map


```setl4

map = new('map one:1  two:2 ten:10 forty:40 hundred:100 thousand:1000')
show(map)

show.eval("eq(get(map,'one'),1)")
show.eval("eq(get(map,'one'),6)")
show.eval("exists(map,'eq(get(map,this),2)')")
show.eval("forall(map,'eq(get(map,this),2)')")

```



## Sidef


### Array

Arrays are ordered, integer-indexed collections of any object.

```ruby
# creating an empty array and adding values
var a = []          #=> []
a[0] = 1            #=> [1]
a[3] = "abc"        #=> [1, nil, nil, "abc"]
a << 3.14           #=> [1, nil, nil, "abc", 3.14]
```



### Hash

A Hash is a dictionary-like collection of unique keys and their values. Also called associative arrays, they are similar to Arrays, but where an Array uses integers as its index, a Hash allows you to use any object type, which is automatically converted into a String.

```ruby
# creating an empty hash
var h = Hash()    #=> Hash()
h{:foo} = 1       #=> Hash("foo"=>1)
h{:bar} = 2.4     #=> Hash("foo"=>1, "bar"=>2.4)
h{:bar} += 3      #=> Hash("foo"=>1, "bar"=>5.4)
```



### Pair

A Pair is an array-like collection, but restricted only to two elements.

```ruby
# create a simple pair
var p = Pair('a', 'b')
say p.first;            #=> 'a'
say p.second;           #=> 'b'

# create a pair of pairs
var pair = 'foo':'bar':'baz':();   # => Pair('foo', Pair('bar', Pair('baz', nil)))

# iterate over the values of a pair of pairs
loop {
    say pair.first;                #=> 'foo', 'bar', 'baz'
    pair = pair.second;
    pair == nil && break;
}
```



### Struct

A Struct is a convenient way to bundle a number of attributes together.

```ruby
# creating a struct
struct Person {
    String name,
    Number age,
    String sex
}

var a = Person("John Smith", 41, :man)

a.age += 1                  # increment age
a.name = "Dr. #{a.name}"    # update name

say a.name          #=> "Dr. John Smith"
say a.age           #=> 42
say a.sex           #=> "man"
```



## Slate


```slate
{1. 2. 3. 4. 5} collect: [|:x| x + 1]. "--> {2. 3. 4. 5. 6}"
{1. 2. 3. 4. 5} select: #isOdd `er. "--> {1. 3. 5}"
({3. 2. 7} collect: #+ `er  <- 3) sort. "--> {"SortedArray traitsWindow" 5. 6. 10}"
ExtensibleArray new `>> [addLast: 3. addFirst: 4. ]. "--> {"ExtensibleArray traitsWindow" 4. 3}"
```



## Smalltalk

Smalltalk has several collection classes (indeed the class Collection is the parent of a long list of subclasses), being the word ''collection'' rather generic (an array indexed by integers is a collection too, and in some languages it's the only primitive collection available).

In this code I show how to add elements (which for each collection kind can be mixed) to five kind of Smalltalk collection:

* '''OrderedCollection''': elements are kept in the order they are added;
* '''Bag''': for each element, a count of how many times it appears is kept. So objects appear only once, but we can know how many we added in the bag;
* '''Set''': a set. Elements appear only once, adding an existing object won't change the set; if we want to know if we added the same object several time, we use a Bag;
* '''SortedCollection''': elements-objects are sorted (every comparable object can be added, and if we want different sorting criteria, we can give our custom comparator through sortBlock);
* '''Dictionary''': objects are ''indexed'' by an arbitrary key, e.g. a string


```smalltalk
|anOrdered aBag aSet aSorted aSorted2 aDictionary|

anOrdered := OrderedCollection new.
anOrdered add: 1; add: 5; add: 3.
anOrdered printNl.

aBag := Bag new.
aBag add: 5; add: 5; add: 5; add: 6.
aBag printNl.

aSet := Set new.
aSet add: 10; add: 5; add: 5; add: 6; add: 10.
aSet printNl.

aSorted := SortedCollection new.
aSorted add: 10; add: 9; add: 8; add: 5.
aSorted printNl.

"another sorted with custom comparator: let's sort
 the other collections according to their size (number of
 elements)"
aSorted2 := SortedCollection sortBlock: [ :a :b |
    (a size) < (b size) ].
aSorted2 add: anOrdered; add: aBag; add: aSet; add: aSorted.
aSorted2 printNl.

aDictionary := Dictionary new.
aDictionary at: 'OrderedCollection' put: anOrdered;
            at: 'Bag' put: aBag;
            at: 'Set' put: aSet;
            at: 'SortedCollection' put: { aSorted. aSorted2 }.
aDictionary printNl.
```


Output:
<pre style="overflow:scroll">OrderedCollection (1 5 3 )
Bag(5:3 6:1 )
Set (10 5 6 )
SortedCollection (5 8 9 10 )
SortedCollection (Set (10 5 6 ) OrderedCollection (1 5 3 ) Bag(5:3 6:1 ) SortedCollection (5 8 9 10 ) )
Dictionary (
        'SortedCollection'->(SortedCollection (5 8 9 10 ) SortedCollection (Set (10 5 6 ) OrderedCollection (1 5 3 ) Bag(5:3 6:1 ) SortedCollection (5 8 9 10 ) ) )
        'OrderedCollection'->OrderedCollection (1 5 3 )
        'Set'->Set (10 5 6 )
        'Bag'->Bag(5:3 6:1 )
)
```



## Tcl

Tcl has 3 fundamental collection types: list, array and dictionary.  

A Tcl list is called an array in other languages (an integer-indexed list of values). 

```tcl
set c [list] ;# create an empty list 
# fill it
lappend c 10 11 13
set c [linsert $c 2 "twelve goes here"]
# iterate over it
foreach elem $c {puts $elem}

# pass to a proc
proc show_size {l} {
    puts [llength $l]
}
show_size $c
```


A Tcl array is an associative array (aka hash). Arrays are collections of ''variables'' indexable by name, and not collections of values. An array cannot be passed to a procedure be value:  it must either be passed by name or by its serialized representation.  Tcl arrays also are strictly one-dimensional:  arrays cannot be nested.  However, multi-dimensional arrays can be simulated with cleverly constructed key strings.


```tcl
# create an empty array
array set h {}
# add some pair
set h(one) 1
set h(two) 2
# add more data
array set h {three 3 four 4 more {5 6 7 8}}
# iterate over it in a couple of ways
foreach key [array names h] {puts "$key -> $h($key)"}
foreach {key value} [array get h]  {puts "$key -> $value"}

# pass by name
proc numkeys_byname {arrayName} {
    upvar 1 $arrayName arr
    puts "array $arrayName has [llength [array names arr]] keys"
}
numkeys_byname h

# pass serialized
proc numkeys_bycopy {l} {
    array set arr $l
    puts "array has [llength [array names arr]] keys"
}
numkeys_bycopy [array get h]
```


{{works with|Tcl|8.5}}

A Tcl dictionary is an associative array ''value'' that contains other values.  Hence dictionaries can be nested and arbitrarily deep data structures can be created.


```tcl
# create an empty dictionary
set d [dict create]
dict set d one 1
dict set d two 2
# create another
set e [dict create three 3 four 4]
set f [dict merge $d $e]
dict set f nested [dict create five 5 more [list 6 7 8]]
puts [dict get $f nested more] ;# ==> 6 7 8
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT

collection=*
DATA apple
DATA banana
DATA orange

morestuff=*
DATA peaches
DATA apple

collection=APPEND(collection,morestuff)
TRACE *collection

```

Output:

```txt

collection   = *
           1 = apple
           2 = banana
           3 = orange
           4 = peaches
           5 = apple 

```



## UNIX Shell

"Advanced" unix shells have indexed array and associative array collections.

### Indexed arrays

{{works with|bash}}
{{works with|ksh}}

```bash
a_index=(one two three)           # create an array with a few elements
a_index+=(four five)              # append some elements
a_index[9]=ten                   # add a specific index
for elem in "${a_index[@]}"; do   # interate over the elements
    echo "$elem"
done
for idx in "${!a_index[@]}"; do   # interate over the array indices
    printf "%d\t%s\n" $idx "${a_index[idx]}"
done
```


### Associative arrays

{{works with|bash}}

```bash
declare -A a_assoc=([one]=1 [two]=2 [three]=3)    # create an array with a few elements
a_assoc+=([four]=4 [five]=5)                      # add some elements
a_assoc[ten]=10
for value in "${a_assoc[@]}"; do                  # interate over the values
    echo "$value"
done
for key in "${!a_assoc[@]}"; do                   # interate over the array indices
    printf "%s\t%s\n" "$key" "${a_assoc[$key]}"
done
```

{{works with|ksh}}
Change 
```bash
declare -A
```
 to 
```bash
typeset -A
```



## Ursala


There are several kinds of collections in Ursala that are supported by
having their own operators and type constructors associated with
them. All storage is immutable, but one may "add" to a collection
by invoking a function that returns a new collection from it.
The examples shown below populate the collections with primitive
types expressed literally, but they could also be aggregate
or abstract types, functions, symbolic names or expressions.


### Lists


Lists are written as comma-separated sequences enclosed in
angle brackets, or with the head and tail separated by a colon.

```Ursala
x = <1,5,6>
y = <'foo','bar'>
z = 3:<6,8>
```

This function takes a pair of a new head and an existing list,
and returns one that has the new head "added" to it.

```Ursala
foo ("newhead","existing-list") = "newhead":"existing-list"
```


### Sets


Sets are comma separated sequences enclosed in braces.
The order and multiplicities of elements are ignored, so that the followng
declarations are equivalent.

```Ursala
x = {'a','b'}
y = {'b','a'}
z = {'a','b','a'}
```
                         


### Modules
                   
                                
Modules are lists in a particular form used to represent
key:value pairs, with the key being a character string.

```Ursala
m = <'foo': 1,'bar': 2,'baz': 3>
```
                         
A module or any list of pairs can be reified into a function
(a.k.a., a hash or finite map) and used in any context where a function is usable,
assuming the keys are mutually distinct.


### Trees

Trees are written in the form <math>r</math><code>^:</code><math>s</math>,
where <math>r</math> is the root and <math>s</math> is a list of subtrees, which can
be of any length.

```Ursala
x =

'z'^: <
   'x'^: <
      '7'^: <>,
      '?'^: <'D'^: <>>>,
   'a'^: <'E'^: <>,'j'^: <>>,
   'b'^: <'i'^: <>>,
   'c'^: <>>
```


===A-trees===

A-trees allow faster access than trees by using a different
representation wherein data are stored only in the leaves at a
constant depth.

```Ursala
x = 

[
   4:0: 'foo',
   4:1: 'bar',
   4:2: 'baz',
   4:3: 'volta',
   4:4: 'pramim']
```


### Grids


Grids are similar to lists of A-trees satisfying certain additional
invariants. They represent a rooted, directed graph in which the
nodes are partitioned by levels and edges exist only between nodes
in consecutive levels. This type of data structure is ubiquitous
in financial derivatives applications.

This example shows a grid of floating point numbers. The colon separated numbers (e.g., 4:10)
are used in grids of any type as addresses, with each node including a list of the addresses
of its descendents in the next level.


```Ursala
g =

<
   [0:0: -9.483639e+00^: <4:10,4:14>],
   [
      4:14: -9.681900e+00^: <4:15>,
      4:10: 2.237330e+00^: <4:7>],
   [
      4:15: -2.007562e+00^: <5:5>,
      4:7: 2.419021e+00^: <5:5,5:15>],
   [
      5:15: 8.215451e+00^: <11:118>,
      5:5: 4.067704e+00^: <11:741>],
   [
      11:741: -7.608967e+00^: <8:68>,
      11:118: -1.552837e+00^: <8:68,8:208>],
   [
      8:208: 5.348115e+00^: <4:7,4:9,4:12>,
      8:68: -9.066821e+00^: <4:9,4:12>],
   [
      4:12: -3.460494e+00^: <>,
      4:9: 2.840319e+00^: <>,
      4:7: -2.181140e+00^: <>]>
```



## V

A quote is used for the same purpose in V

```v
[4 3 2 1] 5 swap cons
=[5 4 3 2 1]
```



## Vim Script

Vim Script has two collection types: <code>List</code> and <code>Dictionary</code>.

See [[Arrays#Vim Script|Arrays]] for basic operations on a <code>List</code> and [[Associative_array/Creation#Vim Script|Associative_array/Creation]] for basic operations on a <code>Dictionary</code>.


## VBA

VBA has a built in collection type

```vb
Dim coll As New Collection
coll.Add "apple"
coll.Add "banana"
```



## Visual Basic .NET



```vbnet
Dim toys As New List(Of String)
toys.Add("Car")
toys.Add("Boat")
toys.Add("Train")
```



## Visual FoxPro

Visual FoxPro has a built in Collection class.


```vfp

LOCAL loColl As Collection, o, a1, a2, a3
a1 = CREATEOBJECT("animal", "dog", 4)
a2 = CREATEOBJECT("animal", "chicken", 2)
a3 = CREATEOBJECT("animal", "snake", 0)
loColl = NEWOBJECT("Collection")
loColl.Add(a1)
loColl.Add(a2)
loColl.Add(a3)

FOR EACH o IN loColl FOXOBJECT
    ? o.Name, o.Legs
ENDFOR	

DEFINE CLASS animal As Custom
Legs = 0

PROCEDURE Init(tcName, tnLegs)
THIS.Name = tcName
THIS.Legs = tnLegs
ENDPROC

ENDDEFINE

```



## Wren

Wren has only Map(hash) and List(array)

```wren

var list = [] // Empty Array
list = [1,2,3,4]
list.add(5)
list.clear()
list = [0] * 10
list.count // 10

var map = {}
map["key"] = "value"
map[3] = 31
map.count // 2
map.clear()

for (e in map.keys) {
    // Do stuff
}


```


## zkl


```zkl
Lists: L(1,2,3).append(4); //-->L(1,2,3,4), mutable list
Read only list: ROList(1,2,3).append(4);  // creates two lists

Bit bucket: Data(0,Int,1,2,3) // three bytes
   The "Int" means treat contents as a byte stream
Data(0,Int,"foo ","bar") //-->7 bytes
   Data(0,Int,"foo ").append("bar") //ditto
   Data(0,Int,"foo\n","bar").readln() //-->"foo\n"
Data(0,String,"foo ","bar") //-->9 bytes (2 \0s)
   Data(0,String,"foo ").append("bar").readln() //-->"foo "
```



## MS SmallBasic

Only with LD extension

```MSsmallbasic

ll=LDList.CreateFromValues("")
LDList.Add(ll "Cars")
LDList.Add(ll "Toys")
LDList.Print(ll)

```

result:

List1

1 : Cars

2 : Toys
