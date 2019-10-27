+++
title = "Array Initialization"
description = ""
date = 2018-06-26T17:45:17Z
aliases = []
[extra]
id = 2875
[taxonomies]
categories = []
tags = []
+++

{{DeprecatedTask}}
'''Examples here should be migrated to [[Arrays]] or [[Creating an Associative Array]] and removed from here. If similar code already exists there, simply remove it from here.'''


Demonstrate how to initialize an array variable with data.

See [[Creating_an_Array]] for this topic.

==[[Ada]]==
The array value obtained directly from data is called array aggregate. Considering these array declarations:

```ada

type Vector is array (Integer range <>) of Integer;
type Matrix is array (Integer range <>, Integer range <>) of Integer;
type String is array (Integer range <>) of Character;
type Bits is array (Integer range <>) of Boolean;

```

Initialization by an aggregate using positional and keyed notations:

```ada

X : Vector := (1, 4, 5);   
Y : Vector (1..100) := (2|3 => 1, 5..20 => 2, others => 0);
E : Matrix := ((1, 0), (0, 1));
Z : Matrix (1..20, 1..30) := (others => (others => 0));
S : String := "ABCD";
L : String (1..80) := (others => ' ');
B : Bits := not (1..2 => False); -- Same as (1..2 => True)

```

Note that the array bounds, when unconstrained as in these examples can be either determined by the aggregate, like the initialization of X shows. Or else they can be specified as a constraint, like for example in the initialization of Y. In this case '''others''' choice can be used to specify all unmentioned elements. But in any case, the compiler verifies that all array elements are initialized by the aggregate. Single dimensional arrays of characters can be initialized by character strings, as the variable S shows. Of course, array aggregates can participate in array expressions and these expressions can be used to initialize arrays. The variable B is initialized by an aggregate inversed by the operation '''not'''.
==[[C++]]==

### STL

{{libheader|STL}}STL provides '''std::vector''', which behaves as a dynamically-resizable array.  When an element is added, its value must be set immediately.

```cpp

myVector.push_back(value);

```

Like simple arrays, '''std::vector''' allows the use of the [] operator, and once an element has been added, it can be changed the same way a simple array can.

```cpp

myVector[0] = value;

```

Unlike simple arrays, '''std::vector''' allows you to determing the size of the array.  You can use this set all of the values in the array:

```cpp

// Create a list of numbers from 1 to the size of the vector.
size_t size = myVector.size();
for(int i = 0; i < size; ++i)
  myVector[i] = i + 1;

```

'''std::vector''' also provides iterators, allowing you to iterate through a vector's elements the same way you might any other STL container class.
  // Create a list of numbers from 1 to the size of the vector.

```cpp

std::vector<int> myVector;
int val = 0;
for(std::vector<int>::iterator it = myVector.begin();
    it != myVector.end();
    ++it)
  *it = ++val;

```

A vector can also explicitly be resized:

```cpp

std::vector<int> myVector;
myVector.resize(10);    // now the vector contains 10 elements, all of which are 0
myVector.resize(15, 3); // now the vector contains 15 elements, the 5 new got the value 3
myVector.resize(12);    // the last three elements got removed

```

Also note that a vector can already be filled at construction time:

```cpp

std::vector v1(10);      // a vector of 10 ints, all initialized with 0
std::vector v2(5, 7);    // a vector containing 5 ints, all inizialized with 7
int a[10] = { 2, 3, 5, 7, 11, 13, 17, 19, 23, 29  };
std::vector v3(a, a+10); // a vector containing 10 ints, initialized with the elements of a (i.e. v3[0]==a[0] etc.)
std::vector v4 = v3;     // v4 is a copy of v3

```


==[[F_Sharp|F#]]==
 let a = [| 1; 3; 5; 7; 9 |] // array of integers
 let b = [| 1 .. 10 |] // initialize with range of integers
 let c = [| for n = 1 to 10 do yield n |] // lazy array
 let d = [| "hello"; "world" |] // array of strings

==[[Haskell]]==
To create any new Array of the various array types, you can use this to initialise it with all elements filled with x, and indexes ranging from n to m

```haskell

newArr :: (Ix i) => i -> i -> e -> Array i e
newArr n m x = listArray (n,m) (repeat x)

-----

Prelude Data.Array> newArr 0 10 0
array (0,10) [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0),(9,0),(10,0)]

Prelude Data.Array> newArr (0,0) (5,5) 0
array ((0,0),(5,5)) [((0,0),0),((0,1),0),((0,2),0),((0,3),0),((0,4),0),((0,5),0),((1,0),0),((1,1),0),((1,2),0),((1,3),0),((1,4),0),((1,5),0),((2,0),0),((2,1),0),((2,2),0),((2,3),0),((2,4),0),((2,5),0),((3,0),0),((3,1),0),((3,2),0),((3,3),0),((3,4),0),((3,5),0),((4,0),0),((4,1),0),((4,2),0),((4,3),0),((4,4),0),((4,5),0),((5,0),0),((5,1),0),((5,2),0),((5,3),0),((5,4),0),((5,5),0)]

```


## Scala


```Scala
// immutable maps
var map = Map(1 -> 2, 3 -> 4, 5 -> 6)
map(3) // 4
map = map + (44 -> 99) // maps are immutable, so we have to assign the result of adding elements
map.isDefinedAt(33) // false
map.isDefinedAt(44) // true
```



```scala
// mutable maps (HashSets)
import scala.collection.mutable.HashMap
val hash = new HashMap[Int, Int]
hash(1) = 2
hash += (1 -> 2)  // same as hash(1) = 2
hash += (3 -> 4, 5 -> 6, 44 -> 99)
hash(44) // 99
hash.contains(33) // false
hash.isDefinedAt(33) // same as contains
hash.contains(44) // true
```



```scala
// iterate over key/value
hash.foreach {e => println("key "+e._1+" value "+e._2)} // e is a 2 element Tuple
// same with for syntax
for((k,v) <- hash) println("key " + k + " value " + v)
```



```scala
// items in map where the key is greater than 3
map.filter {k => k._1 > 3} //  Map(5 -> 6, 44 -> 99)
// same with for syntax
for((k, v) <- map; if k > 3) yield (k,v)
```

