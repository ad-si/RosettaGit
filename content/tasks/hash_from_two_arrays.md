+++
title = "Hash from two arrays"
description = ""
date = 2019-09-13T03:57:33Z
aliases = []
[extra]
id = 1637
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
languages = [
  "actionscript",
  "ada",
  "argile",
  "autohotkey",
  "awk",
  "basic256",
  "bbc_basic",
  "bracmat",
  "brat",
  "c",
  "ceylon",
  "clojure",
  "coco",
  "coffeescript",
  "coldfusion",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "e",
  "echolisp",
  "elixir",
  "erlang",
  "factor",
  "falcon",
  "fantom",
  "frink",
  "gambas",
  "go",
  "groovy",
  "harbour",
  "haskell",
  "huginn",
  "ioke",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "k",
  "kotlin",
  "lang5",
  "langur",
  "lasso",
  "lfe",
  "lingo",
  "livecode",
  "lua",
  "m2000_interpreter",
  "maple",
  "mathematica",
  "neko",
  "nemerle",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "oorexx",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pop11",
  "postscript",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "raven",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "sather",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "smalltalk",
  "snobol4",
  "sparkling",
  "standard_ml",
  "swift",
  "tcl",
  "txr",
  "unix_shell",
  "unixpipes",
  "ursala",
  "vala",
  "vbscript",
  "visual_basic",
  "wdte",
  "wortel",
  "zkl",
]
+++

{{omit from|BASIC}} <!-- Does not have hash tables or other map structures. -->
## Task

Using two Arrays of equal length, create a Hash object
where the elements from one array (the keys) are linked
to the elements of the other (the values)


## Related tasks

*   [[Associative arrays/Creation]]





## ActionScript


```actionscript
package
{
    public class MyClass
    {
        public static function main():Void
        {
            var hash:Object = new Object();
            var keys:Array = new Array("a", "b", "c");
            var values:Array = new Array(1, 2, 3);

            for (var i:int = 0; i < keys.length(); i++)
                hash[keys[i]] = values[i];
        }
    }
}
```



## Ada

```ada
with Ada.Strings.Hash;
with Ada.Containers.Hashed_Maps;
with Ada.Text_Io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Hash_Map_Test is
   function Equivalent_Key (Left, Right : Unbounded_String) return Boolean is
   begin
      return Left = Right;
   end Equivalent_Key;

   function Hash_Func(Key : Unbounded_String) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash(To_String(Key));
   end Hash_Func;

   package My_Hash is new Ada.Containers.Hashed_Maps(Key_Type => Unbounded_String,
      Element_Type => Unbounded_String,
      Hash => Hash_Func,
      Equivalent_Keys => Equivalent_Key);

   type String_Array is array(Positive range <>) of Unbounded_String;

   Hash : My_Hash.Map;
   Key_List : String_Array := (To_Unbounded_String("foo"),
      To_Unbounded_String("bar"),
      To_Unbounded_String("val"));

   Element_List : String_Array := (To_Unbounded_String("little"),
      To_Unbounded_String("miss"),
      To_Unbounded_String("muffet"));

begin
   for I in Key_List'range loop
      Hash.Insert(Key => (Key_List(I)),
         New_Item => (Element_List(I)));
   end loop;
   for I in Key_List'range loop
      Ada.Text_Io.Put_Line(To_String(Key_List(I)) & " => " &
         To_String(Hash.Element(Key_List(I))));
   end loop;

end Hash_Map_Test;
```



## Argile

```Argile
use std, array, hash

let keys = @["hexadecimal" "decimal" "octal" "binary"]
let values = @[0xa 11 014 0b1101] (: 10 11 12 13 :)
let hash = new hash of int
for each val int i from 0 to 3
  hash[keys[i]] = values[i]
del hash hash
```



## AutoHotkey


```AutoHotkey
array1 := ["two", "three", "apple"]
array2 := [2, 3, "fruit"]
hash := {}
Loop % array1.maxIndex()
   hash[array1[A_Index]] := array2[A_Index]
MsgBox % hash["apple"] "`n" hash["two"]
```


## AWK

Awk arrays are used for both lists and hash maps.
<!--  http://ideone.com/MsdNUc -->

```awk
# usage:  awk -v list1="i ii iii" -v list2="1 2 3"  -f hash2.awk
BEGIN {
	if(!list1) list1="one two three"
	if(!list2) list2="1 2 3"

        split(list1, a);
        split(list2, b);
        for(i=1;i in a;i++) { c[a[i]] = b[i] };

        for(i in c) print i,c[i]
}
```


```txt

three 3
two 2
one 1

```



## BASIC256

''Solution is at [[Associative_array/Creation#BASIC256]]''.


## BBC BASIC

```bbcbasic
      DIM array1$(4) : array1$() = "0", "1", "2", "3", "4"
      DIM array2$(4) : array2$() = "zero", "one", "two", "three", "four"

      FOR index% = 0 TO DIM(array1$(),1)
        PROCputdict(mydict$, array2$(index%), array1$(index%))
      NEXT
      PRINT FNgetdict(mydict$, "3")
      END

      DEF PROCputdict(RETURN dict$, value$, key$)
      IF dict$ = "" dict$ = CHR$(0)
      dict$ += key$ + CHR$(1) + value$ + CHR$(0)
      ENDPROC

      DEF FNgetdict(dict$, key$)
      LOCAL I%, J%
      I% = INSTR(dict$, CHR$(0) + key$ + CHR$(1))
      IF I% = 0 THEN = "" ELSE I% += LEN(key$) + 2
      J% = INSTR(dict$, CHR$(0), I%)
      = MID$(dict$, I%, J% - I%)
```



## Bracmat


```bracmat
  two three apple:?arr1
& 2 3 fruit:?arr2
& new$hash:?H
&   whl
  ' ( !arr1:%?k ?arr1
    & !arr2:%?v ?arr2
    & (H..insert)$(!k.!v)
    )
& (H..forall)$out
& ;

```

```txt
apple.fruit
three.3
two.2
```



## Brat


```brat
zip = { keys, values |
	h = [:]
	keys.each_with_index { key, index |
		h[key] = values[index]
	}

	h
}

p zip [1 2 3] [:a :b :c]  #Prints [1: a, 2: b, 3: c]
```



## C

There likely exist libraries that can be used for creating hashes that are better than the following
implementation. There are also better functions for obtaining hash values from strings. The
following implementation tries to be somewhat generic to facilitate using alternative key and
value types.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define KeyType const char *
#define ValType int

#define HASH_SIZE 4096

// hash function useful when KeyType is char * (string)
unsigned strhashkey( const char * key, int max)
{
    unsigned h=0;
    unsigned hl, hr;

    while(*key) {
        h += *key;
        hl= 0x5C5 ^ (h&0xfff00000 )>>18;
        hr =(h&0x000fffff );
        h = hl ^ hr ^ *key++;
    }
    return h % max;
}

typedef struct sHme {
    KeyType    key;
    ValType    value;
    struct sHme  *link;
} *MapEntry;

typedef struct he {
    MapEntry  first, last;
} HashElement;

HashElement hash[HASH_SIZE];

typedef void (*KeyCopyF)(KeyType *kdest, KeyType ksrc);
typedef void (*ValCopyF)(ValType *vdest, ValType vsrc);
typedef unsigned (*KeyHashF)( KeyType key, int upperBound );
typedef int (*KeyCmprF)(KeyType key1, KeyType key2);

void HashAddH( KeyType key, ValType value,
        KeyCopyF copyKey, ValCopyF copyVal, KeyHashF hashKey, KeyCmprF keySame )
{
    unsigned hix = (*hashKey)(key, HASH_SIZE);
    MapEntry m_ent;

    for (m_ent= hash[hix].first;
            m_ent && !(*keySame)(m_ent->key,key); m_ent=m_ent->link);
    if (m_ent) {
        (*copyVal)(&m_ent->value, value);
    }
    else {
        MapEntry last;
        MapEntry hme = malloc(sizeof(struct sHme));
        (*copyKey)(&hme->key, key);
        (*copyVal)(&hme->value, value);
        hme->link = NULL;
        last = hash[hix].last;
        if (last) {
//	    printf("Dup. hash key\n");
            last->link = hme;
        }
        else
            hash[hix].first = hme;
        hash[hix].last = hme;
    }
}

int HashGetH(ValType *val, KeyType key, KeyHashF hashKey, KeyCmprF keySame )
{
    unsigned hix = (*hashKey)(key, HASH_SIZE);
    MapEntry m_ent;
    for (m_ent= hash[hix].first;
            m_ent && !(*keySame)(m_ent->key,key); m_ent=m_ent->link);
    if (m_ent) {
        *val = m_ent->value;
    }
    return (m_ent != NULL);
}

void copyStr(const char**dest, const char *src)
{
    *dest = strdup(src);
}
void copyInt( int *dest, int src)
{
    *dest = src;
}
int strCompare( const char *key1, const char *key2)
{
    return strcmp(key1, key2) == 0;
}

void HashAdd( KeyType key, ValType value )
{
    HashAddH( key, value, &copyStr, &copyInt, &strhashkey, &strCompare);
}

int HashGet(ValType *val, KeyType key)
{
    return HashGetH( val, key, &strhashkey, &strCompare);
}

int main()
{
    static const char * keyList[] = {"red","orange","yellow","green", "blue", "violet" };
    static int valuList[] = {1,43,640, 747, 42, 42};
    int ix;

    for (ix=0; ix<6; ix++) {
        HashAdd(keyList[ix], valuList[ix]);
    }
    return 0;
}
```



## C++


```cpp
#include <unordered_map>
#include <string>

int main()
{
  std::string keys[] = { "1", "2", "3" };
  std::string vals[] = { "a", "b", "c" };

  std::unordered_map<std::string, std::string> hash;
  for( int i = 0 ; i < 3 ; i++ )
     hash[ keys[i] ] = vals[i] ;
}
```


```cpp
#include <range/v3/view/zip.hpp>

#include <unordered_map>
#include <string>

int main()
{
  std::string keys[] = { "1", "2", "3" };
  std::string vals[] = { "foo", "bar", "baz" };

  std::unordered_map<std::string, std::string> hash(ranges::view::zip(keys, vals));
}

```



## C#


### C# 1.0


```c#
static class Program
{
    static void Main()
    {
        System.Collections.Hashtable h = new System.Collections.Hashtable();

        string[] keys = { "foo", "bar", "val" };
        string[] values = { "little", "miss", "muffet" };

        System.Diagnostics.Trace.Assert(keys.Length == values.Length, "Arrays are not same length.");

        for (int i = 0; i < keys.Length; i++)
        {
            h.Add(keys[i], values[i]);
        }
    }
}
```


<code>Hashtable.Add</code> throws an exception when a key already exists.

An alternative method to add entries is to use the indexer setter, which replaces the old value associated with a key, if any:

```c#
h[keys[i]] = values[i];
```



### Modern

Uses <code>System.Collections.Generic.Dictionary<TKey, TValue></code>, <code>Enumerable.ToDictionary</code> from LINQ, extension method syntax, and lambda expressions.

<code>Enumerable.Zip</code> truncates the longer of its arguments.


```c#
using System.Linq;

static class Program
{
    static void Main()
    {
        string[] keys = { "foo", "bar", "val" };
        string[] values = { "little", "miss", "muffet" };

        var h = keys
            .Zip(values, (k, v) => (k, v))
            .ToDictionary(keySelector: kv => kv.k, elementSelector: kv => kv.v);
    }
}
```



## Ceylon


```ceylon
shared void run() {
	value keys = [1, 2, 3];
	value items = ['a', 'b', 'c'];
	value hash = map(zipEntries(keys, items));
}
```



## Clojure


```lisp
(zipmap [\a \b \c] [1 2 3])
```



## Coco


```coco
keys = <[apple banana orange grape]>
values = <[red yellow orange purple]>

object = new
    @[keys[i]] = values[i] for i til keys.length
```



## CoffeeScript


```coffeescript

  keys = ['a','b','c']
  values = [1,2,3]
  map = {}
  map[key] = values[i] for key, i in keys

```



## ColdFusion


```ColdFusion
<cfscript>

function makeHash(keyArray, valueArray) {
  var x = 1;
  var result = {};
  for( ; x <= ArrayLen(keyArray); x ++ ) {
    result[keyArray[x]] = valueArray[x];
  }
  return result;
}

keyArray = ['a', 'b', 'c'];
valueArray = [1, 2, 3];
map = makeHash(keyArray, valueArray);
</cfscript>
```



## Common Lisp



```lisp
(defun rosetta-code-hash-from-two-arrays (vector-1 vector-2 &key (test 'eql))
  (assert (= (length vector-1) (length vector-2)))
  (let ((table (make-hash-table :test test :size (length vector-1))))
    (map nil (lambda (k v) (setf (gethash k table) v))
             vector-1 vector-2)
    table))
```


Or, using cl:loop:


```lisp
(defun rosetta-code-hash-from-two-arrays (vector-1 vector-2 &key (test 'eql))
  (loop initially (assert (= (length vector-1) (length vector-2)))
        with table = (make-hash-table :test test :size (length vector-1))
        for k across vector-1
        for v across vector-2
        do (setf (gethash k table) v)
        finally (return table)))
```


In Common Lisp terminology, a vector is a one-dimensional array.


## D


```d
void main() {
    import std.array, std.range;

    immutable hash = ["a", "b", "c"].zip([1, 2, 3]).assocArray;
}
```


=={{header|Déjà Vu}}==

```dejavu
local :h_keys [ :one :two :three ]
local :h_values [ 1 2 3 ]
local :h {}
for item in h_keys:
    set-to h item pop-from h_values

```



## E



```e
def keys := ["one", "two", "three"]
def values := [1, 2, 3]
__makeMap.fromColumns(keys, values)
```



## EchoLisp


```scheme

(lib 'hash)

(define H (make-hash))
(define keys '(elvis simon antoinette))
(define kvalues '("the king" "gallubert" "de gabolde d'Audan"))

(list->hash (map cons keys kvalues) H)
    → #hash:3
(hash-ref H 'elvis)
    → "the king"

```



## Elixir


```elixir
iex(1)> keys = [:one, :two, :three]
[:one, :two, :three]
iex(2)> values = [1, 2, 3]
[1, 2, 3]
iex(3)> Enum.zip(keys, values) |> Enum.into(Map.new)
%{one: 1, three: 3, two: 2}
```



## Erlang


```Erlang

Dictionary = dict:from_list( lists:zip([key1, key2, key3], [value1, 2, 3]) ).

```


=={{header|F Sharp|F#}}==

```fsharp
HashMultiMap(Array.zip [|"foo"; "bar"; "baz"|] [|16384; 32768; 65536|], HashIdentity.Structural)
```



## Factor


```factor
USING: hashtables ;
{ "one" "two" "three" } { 1 2 3 } zip >hashtable
```



## Falcon


```falcon

keys = [ 'a', 'b', 'c', 'd' ]
values = [ 1, 2, 3, 4 ]
hash = [ => ]
for i in [ 0 : keys.len() ]:  hash[ keys[ i ] ] = values[ i ]

```



## Fantom



```fantom

class Main
{
  public static Void main ()
  {
    keys := [1,2,3,4,5]
    values := ["one", "two", "three", "four", "five"]

    // create an empty map
    map := [:]
    // add the key-value pairs to it
    keys.size.times |Int index|
    {
      map.add(keys[index], values[index])
    }
  }
}

```



## Frink

There is a built-in dictionary/hash constructor that takes two arrays as input.

```frink

a = new dict[["a", "b", "c"], [1, 2, 3]]

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=944f0b9cbf60910e7ee7ea4191928a7c Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sValue As String[] = ["Zero", "One", "Two", "Three", "Four", "Five"]
Dim sKey As String[] = [0, 1, 2, 3, 4, 5]
Dim sCol As New Collection
Dim siCount As Short

For siCount = 0 To sKey.max
  sCol.Add(sValue[siCount], sKey[siCount])
Next

For siCount = 0 To sKey.max
  Print Str(sicount) & " = " & sCol[siCount]
Next

End
```

Output:

```txt

0 = Zero
1 = One
2 = Two
3 = Three
4 = Four
5 = Five

```



## Go


```go
package main

import "fmt"

func main() {
    keys := []string{"a", "b", "c"}
    vals := []int{1, 2, 3}
    hash := map[string]int{}
    for i, key := range keys {
        hash[key] = vals[i]
    }
    fmt.Println(hash)
}
```

```txt

map[b:2 a:1 c:3]

```



## Groovy



```groovy
def keys = ['a','b','c']
def vals = ['aaa', 'bbb', 'ccc']
def hash = [:]
keys.eachWithIndex { key, i ->
 hash[key] = vals[i]
}
```


Alternative Version:

```groovy
List.metaClass.hash = { list -> [delegate, list].transpose().collectEntries { [(it[0]): it[1]] } }
```


Test:

```groovy
assert (['a', 'b', 'c'].hash(['aaa', 'bbb', 'ccc'])) == [a: 'aaa', b: 'bbb', c: 'ccc']
```



## Harbour


```visualfoxpro
LOCAL arr1 := { 6, "eight" }, arr2 := { 16, 8 }
LOCAL hash := { => }
LOCAL i, j

FOR EACH i, j IN arr1, arr2
   hash[ i ] := j
NEXT
```



## Haskell

```haskell
import Data.Map

makeMap ks vs = fromList $ zip ks vs
mymap = makeMap ['a','b','c'] [1,2,3]
```



## Huginn


```huginn
from Algorithms import materialize, zip;

main() {
  keys = [1, 2, 3];
  values = ['a', 'b', 'c'];
  hash = materialize( zip( key, values ), lookup );
}
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
link ximage    # to format the structure

procedure main(arglist)                          #: demonstrate hash from 2 lists
local keylist

if *arglist = 0 then arglist := [1,2,3,4]        # ensure there's a list
every put(keylist := [], "key-" || !arglist)     # make keys for each entry

every  (T := table())[keylist[ i := 1 to *keylist ]] := arglist[i]  # create the hash table

write(ximage(T))                                 # show result
end
```



## Ioke


```ioke
{} addKeysAndValues([:a, :b, :c], [1, 2, 3])
```



## J

What exactly is a hash?

We shall interpret 'hash' as "a function between some arbitrary values and some other arbitrary values". (Technically speaking a hash is more of a reference to a collection of techniques for achieving this, with something of an emphasis on an arbitrary and opaque intermediate result, than the actual end result. But people have spoken very glowingly of these techniques so let's pretend that the result actually matters.)

'''Solution:'''

```j
hash=: vals {~ keys&i.
```

'''For example:'''


```j
   keys=: 10?.100
   vals=: > ;:'zero one two three four five six seven eight nine'
   hash=: vals {~ keys&i.

   keys
46 99 23 62 42 44 12 5 68 63
   $vals
10 5

   hash 46
zero
   hash 99
one
   hash 63 5 12 5 23
nine
seven
six
seven
two
```

Here,<code> keys </code>is a list of 10 integers between 0 and 99 chosen arbitrarily (we like to call this "randomly" but there is some mathematical controversy about implementations of randomness) without repetition, and<code> vals </code>is a 10 by 5 character matrix.


## Java


```java
import java.util.HashMap;
public static void main(String[] args){
	String[] keys= {"a", "b", "c"};
	int[] vals= {1, 2, 3};
	HashMap<String, Integer> hash= new HashMap<String, Integer>();

	for(int i= 0; i < keys.length; i++){
	   hash.put(keys[i], vals[i]);
	}
}
```



## JavaScript



### Iterative


```javascript

var keys = ['a', 'b', 'c'];
var values = [1, 2, 3];
var map = {};
for(var i = 0; i < keys.length; i += 1) {
  map[ keys[i] ] = values[i];
}

```



### Iterative Using Foreach


```javascript

function arrToObj(keys, vals) {
  var map = {};
  keys.forEach(function (key, index) {
    map[key] = val[index];
  });
  return map;
}

```



### Using Reduce


```javascript

function arrToObj(keys, vals) {
  return keys.reduce(function(map, key, index) {
    map[key] = vals[index];
    return map;
  }, {});
}

```



## jq

jq only supports hashing of strings.  In the following, accordingly, we assume that
one array (keys) is an array of strings.

```jq
# hash(keys) creates a JSON object with the given keys as keys
# and values taken from the input array in turn.
# "keys" must be an array of strings.
# The input array may be of any length and have values of any type,
# but only the first (keys|length) values will be used;
# the input will in effect be padded with nulls if required.
def hash(keys):
  . as $values
  | reduce range(0; keys|length) as $i
      ( {}; . + { (keys[$i]) : $values[$i] });

[1,2,3] | hash( ["a","b","c"] )
```

```jq
jq -n -f Hash_from_two_arrays.jq
{
  "a": 1,
  "b": 2,
  "c": 3
}
```

To hash an array of distinct integers, the tostring filter can be used, e.g.
 [10,20,30] | hash( [1,2,3] | map(tostring) )
yields:
```jq
{
  "1": 10,
  "2": 20,
  "3": 30
}
```



## Jsish

From Javascript.

```javascript
/* Hash from two arrays, in Jsish */
function hashTwo(k:array, v:array):object {
    var hash = {};
    for (var i = 0; i < k.length; i++) hash[k[i]] = v[i];
    return hash;
}

;hashTwo(['a','b','c'], [1,2,3]);
;hashTwo(['a','b'], [1,[2,4,8],3]);
;hashTwo(['a','b','c'], [1,2]);
;hashTwo([], []);

/*
=!EXPECTSTART!=
hashTwo(['a','b','c'], [1,2,3]) ==> { a:1, b:2, c:3 }
hashTwo(['a','b'], [1,[2,4,8],3]) ==> { a:1, b:[ 2, 4, 8 ] }
hashTwo(['a','b','c'], [1,2]) ==> { a:1, b:2, c:undefined }
hashTwo([], []) ==> {}
=!EXPECTEND!=
*/
```

```txt
prompt$ jsish -u hashTwo.jsi
[PASS] hashTwo.jsi
```

Use '''jsish --U hashTwo.jsi''' to see echo mode test lines.


## Julia

'''Using comprehension''':

```julia
k = ["a", "b", "c"]
v = [1, 2, 3]

Dict(ki => vi for (ki, vi) in zip(k, v))
```


'''Using constructor''':

```julia
Dict(zip(keys, values))
```


'''Specifying types''':

```julia
Dict{String,Int32}(zip(keys, values))
```



## K

The keys in a dictionary must be a symbol.

```K
   a: `zero `one `two  / symbols
   b: 0 1 2

   d:. a,'b  / create the dictionary
.((`zero;0;)
  (`one;1;)
  (`two;2;))

   d[`one]
1
```


Here we use integers as keys (which must be converted to symbols) and strings as values (here also converted to symbols).


```K
   keys: !10   / 0..9
   split:{1_'(&x=y)_ x:y,x}
   vals:split["zero one two three four five six seven eight nine";" "]

   s:{`$$x}  / convert to symbol
   d:. (s'keys),'s'vals
.((`"0";`zero;)
  (`"1";`one;)
  (`"2";`two;)
  (`"3";`three;)
  (`"4";`four;)
  (`"5";`five;)
  (`"6";`six;)
  (`"7";`seven;)
  (`"8";`eight;)
  (`"9";`nine;))

   $d[s 1] / leading "$" converts back to string
"one"
```



## Kotlin


```scala
// version 1.1.0

fun main(args: Array<String>) {
    val names = arrayOf("Jimmy", "Bill", "Barack", "Donald")
    val ages  = arrayOf(92, 70, 55, 70)
    val hash  = mapOf(*names.zip(ages).toTypedArray())
    hash.forEach { println("${it.key.padEnd(6)} aged ${it.value}") }
}
```


```txt

Jimmy  aged 92
Bill   aged 70
Barack aged 55
Donald aged 70

```



## Lasso


```Lasso
local(
	array1	= array('a', 'b', 'c'),
	array2	= array(1, 2, 3),
	hash	= map
)

loop(#array1 -> size) => {
	#hash -> insert(#array1 -> get(loop_count) = #array2 -> get(loop_count))
}

#hash
```

-> map(a = 1, b = 2, c = 3)


## Lang5


```lang5
:>
table  2 compress -1 transpose ;
['one 'two 'three 'four] [1 2 3 4] >table
```



## Langur


###  the easy way


```Langur
writeln toHash ["a", "b", "c", "d"], [1, 2, 3, 4]
```



###  a longer way


```Langur
val .new = foldfrom(
    f(.hash, .key, .value) .hash ~ h{.key: .value},
    h{},
    ["a", "b", "c", "d"],
    [1, 2, 3, 4],
)

writeln .new
```


```txt
h{"d": 4, "a": 1, "b": 2, "c": 3}
```



## LFE


```lisp
(let* ((keys (list 'foo 'bar 'baz))
       (vals (list '"foo data" '"bar data" '"baz data"))
       (tuples (: lists zipwith
                 (lambda (a b) (tuple a b)) keys vals))
       (my-dict (: dict from_list tuples)))
  (: io format '"fetched data: ~p~n" (list (: dict fetch 'baz my-dict))))

```



## Lingo


```lingo
keys = ["a","b","c"]
values = [1,2,3]

props = [:]
cnt = keys.count
repeat with i = 1 to cnt
  props[keys[i]] = values[i]
end repeat

put props
-- ["a": 1, "b": 2, "c": 3]
```



## LiveCode


```LiveCode
put "a,b,c" into list1
put 10,20,30 into list2
split list1 using comma
split list2 using comma
repeat with i=1 to the number of elements of list1
    put list2[i] into list3[list1[i]]
end repeat
combine list3 using comma and colon
put list3

-- ouput
-- a:10,b:20,c:30
```



## Lua


```lua
function(keys,values)
  local t = {}
  for i=1, #keys do
    t[keys[i]] = values[i]
  end
end
```




## M2000 Interpreter


```M2000 Interpreter

Module CheckAll {
      Module CheckVectorType {
            Dim Keys$(4), Values(4)
            Keys$(0):= "one","two","three","four"
            Values(0):=1,2,3,4
            Inventory Dict
            For i=0 to 3 {
                  Append Dict, Keys$(i):=Values(i)
            }
            Print Dict("one")+Dict("four")=Dict("two")+Dict("three")  ' true
      }
      Module CheckVectorType1 {
            Dim Keys$(4), Values$(4)
            Keys$(0):= "one","two","three","four"
            Values$(0):="*","**","***","****"
            Inventory Dict
            For i=0 to 3 {
                  Append Dict, Keys$(i):=Values$(i)
            }
            Print Dict$("one")+Dict$("four")=Dict$("two")+Dict$("three")  ' true
      }
      CheckVectorType
      CheckVectorType1
}
CheckAll

```


This is the real task, using two arrays as arguments in a function which return the hash table (an inventory object). Each pair has a key and a stack object. If a key found more than one we simply add to stack (at the bottom using Data - or at the top using Push). A module PrintKeyItems get the hash, the key to find, and the second array with values, and apply indexes from  hash to array. The MakeHash add indexes using start value of array of values. So we can pass arrays with different start and end index, but they must be one dimension and have same number of items, else we get error



```M2000 Interpreter

Module Checkit {
      Function MakeHash(&a$(), &b$()) {
            if dimension(a$())<>1 or  dimension(b$())<>1 then Error "Only for one dimension arrays"
            if len(a$())<>len(b$()) Then Error "Only for same size arrays"
            start=dimension(a$(),1, 0)
            end=dimension(a$(),1, 1)
            start2=dimension(b$(),1, 0)
            Inventory Hash
            For i=start to end {
                   if Exist(hash, a$(i)) Then {
                         \\ s is a pointer to a stack object
                        s=hash(a$(i))
                        Stack s {Data i-start+start2}
                  } Else Append hash, a$(i):=Stack:=i-start+start2
            }
            =Hash
      }

      Module PrintKeyItems (hash, akey$, &b$()) {
            \\  n=hash(akey$)  ' use this if akey$ allways is a proper key
            \\  and hide these two lines using \\
            if not exist(hash, akey$) then Error "Key not exist"
            n=Eval(hash)
            For i=1 to Len(n) {
                  Print  b$(stackitem(n,i)),
            }
            Print
      }

      Dim a$(2 to 5)
      Dim b$(4 to 7)
      a$(2)="A", "B","A","C"
      b$(4)="A1","B1","A2", "C1"

      MyHash=MakeHash(&a$(), &b$())

      PrintkeyItems Myhash, "A", &b$()    ' print A1 A2
      PrintkeyItems Myhash, "B", &b$()    ' print B1
      PrintkeyItems Myhash, "C", &b$()    ' print C1
}
Checkit

```



## Maple


```Maple
A := [1, 2, 3];
B := ["one", "two", three"];
T := table( zip( `=`, A, B ) );
```



## Mathematica


```Mathematica
Map[(Hash[Part[#, 1]] = Part[#, 2]) &,
 Transpose[{{1, 2, 3}, {"one", "two", "three"}}]]

?? Hash
->Hash[1]=one
->Hash[2]=two
->Hash[3]=three
```


=={{header|MATLAB}} / {{header|Octave}}==
See [[Associative arrays/Creation#MATLAB_.2F_Octave|Associative arrays/Creation]] for clarification of limitations and differences between the two methods.

### MATLAB/Octave: structs


```MATLAB
function s = StructFromArrays(allKeys, allVals)
% allKeys must be cell array of strings of valid field-names
% allVals can be cell array or array of numbers
% Assumes arrays are same size and valid types
    s = struct;
    if iscell(allVals)
        for k = 1:length(allKeys)
            s.(allKeys{k}) = allVals{k};
        end
    else
        for k = 1:length(allKeys)
            s.(allKeys{k}) = allVals(k);
        end
    end
end
```

```txt
>> ages = StructFromArrays({'Joe' 'Bob' 'Sue'}, [21 35 27])

ages =

    Joe: 21
    Bob: 35
    Sue: 27
```



### MATLAB only: containers.Map

containers.Map constructor provides this functionality already.

```txt
>> ages = containers.Map({'Joe' 'Bob' 'Sue'}, [21 35 27]);
>> keys(ages)

ans =

    'Bob'    'Joe'    'Sue'

>> values(ages)

ans =

    [35]    [21]    [27]
```



## Neko


```ActionScript
/**
 <doc><h2>Hash from two arrays, in Neko</h2></doc>
**/

var sprintf = $loader.loadprim("std@sprintf", 2)

var array_keys = $array("one",2,"three",4,"five")
var array_vals = $array("six",7,"eight",9,"zero")
var elements = $asize(array_keys)

var table = $hnew(elements)

var step = elements
while (step -= 1) >= 0 $hadd(table, $hkey(array_keys[step]), array_vals[step])

/*
 $hiter accepts a hashtable and a function that accepts two args, key, val
*/
var show = function(k, v) $print("Hashed key: ", sprintf("%10d", k), " Value: ", v, "\n")
$hiter(table, show)
```


```txt
prompt$ nekoc hash-two-arrays.neko
prompt$ neko hash-two-arrays.n
Hashed key:   13898426 Value: eight
Hashed key:      38662 Value: six
Hashed key:          2 Value: 7
Hashed key:          4 Value: 9
Hashed key:     737454 Value: zero
```



## NetRexx


###  REXX Style

```netrexx
/* NetRexx program ****************************************************
* 04.11.2012 Walter Pachl  derived from REXX
**********************************************************************/
options replace format comments java crossref savelog symbols nobinary
  values='triangle quadrilateral pentagon hexagon heptagon octagon' -
         'nonagon decagon dodecagon'
  keys  ='three four five six seven eight nine ten twelve'
  kcopy=keys
  k=''                                 /* initialize the arrays      */
  v=''
  value='unknown'
  Loop i=1 By 1 While kcopy>''         /* initialize the two arrays  */
    Parse kcopy  ki kcopy;  k[i]=ki
    Parse values vi values; v[i]=vi
    End
  Loop j=1 To i-1
    value[k[j]]=v[j]
    End
  Say 'Enter one of these words:'
  Say ' 'keys
  Parse Ask z
  Say z '->' value[z]
```



###  Java Collections

NetRexx has access to Java's Collection objects too.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

vals = [ 'zero', 'one', 'two', 'three', 'four', 'five' ]
keys = [ 'k0', 'k1', 'k2', 'k3', 'k4', 'k5' ]
hash1 = Rexx
hash2 = Map

hash1 = HashMap()
hash2 = ''
makeHash(hash1, keys, vals) -- using a Map object (overloaded method)
makeHash(hash2, keys, vals) -- using a Rexx object (overloaded method)

return

-- Using a Java collection object
method makeHash(hash = Map, keys = Rexx[], vals = Rexx[]) static
  loop k_ = 0 to keys.length - 1
    hash.put(keys[k_], vals[k_])
    end k_

  key = Rexx
  loop key over hash.keySet()
    say key.right(8)':' hash.get(key)
    end key
  say

  return

-- For good measure a version using the default Rexx object as a hash (associative array)
method makeHash(hash = Rexx, keys = Rexx[], vals = Rexx[]) static
  loop k_ = 0 to keys.length - 1
    hash[keys[k_]] = vals[k_]
    end k_

  loop key over hash
    say key.right(8)':' hash[key]
    end key
  say

  return

```



## Nemerle


```Nemerle
using System;
using System.Console;
using Nemerle.Collections;
using Nemerle.Collections.NCollectionsExtensions;

module AssocArray
{
    Main() : void
    {
        def list1 = ["apples", "oranges", "bananas", "kumquats"];
        def list2 = [13, 34, 12];
        def inventory = Hashtable(ZipLazy(list1, list2));
        foreach (item in inventory)
            WriteLine("{0}:   {1}", item.Key, item.Value);
    }
}
```



## Nim


```nim
import tables, sequtils

let keys = @['a','b','c']
let values = @[1, 2, 3]

let table = toTable zip(keys, values)
```



## Objeck



```objeck

use Structure;

bundle Default {
  class HashOfTwo {
    function : Main(args : System.String[]) ~ Nil {
      keys := ["1", "2", "3"];
      vals := ["a", "b", "c"];
      hash := StringHash->New();
      each(i : vals) {
        hash->Insert(keys[i], vals[i]->As(Base));
      };
    }
  }
}

```


=={{header|Objective-C}}==


```objc
NSArray *keys = @[@"a", @"b", @"c"];
NSArray *values = @[@1, @2, @3];
NSDictionary *dict = [NSDictionary dictionaryWithObjects:values forKeys:keys];
```


=={{header|Oberon-2}}==
Works with oo2c version 2

```oberon2

MODULE HashFromArrays;
IMPORT
  ADT:Dictionary,
  Object:Boxed;
TYPE
  Key= STRING;
  Value= Boxed.LongInt;

  PROCEDURE Do;
  VAR
    a: ARRAY 128 OF Key;
    b: ARRAY 128 OF Value;
    hash: Dictionary.Dictionary(Key,Value);
    i: INTEGER;

  BEGIN
    hash := NEW(Dictionary.Dictionary(Key,Value));
    a[0] := "uno";
    a[1] := "dos";
    a[2] := "tres";
    a[3] := "cuatro";
    b[0] := Boxed.ParseLongInt("1");
    b[1] := Boxed.ParseLongInt("2");
    b[2] := Boxed.ParseLongInt("3");
    b[3] := Boxed.ParseLongInt("4");
    i := 0;
    WHILE (i < LEN(a)) & (a[i] # NIL) DO
      hash.Set(a[i],b[i]);
      INC(i)
    END;

  END Do;
BEGIN
  Do;
END HashFromArrays.

```



## OCaml

The idiomatic solution uses lists rather than arrays.


```ocaml
let keys = [ "foo"; "bar"; "baz" ]
and vals = [ 16384; 32768; 65536 ]
and hash = Hashtbl.create 0;;

List.iter2 (Hashtbl.add hash) keys vals;;
```


The solution is similar with arrays.


```ocaml
let keys = [| "foo"; "bar"; "baz" |]
and vals = [| 16384; 32768; 65536 |]
and hash = Hashtbl.create 0;;

Array.iter2 (Hashtbl.add hash) keys vals;;
```


In either case, an exception is raised if the inputs are different lengths.

If you want to use functional binary search trees instead of hash tables:


```ocaml
module StringMap = Map.Make (String);;

let keys = [ "foo"; "bar"; "baz" ]
and vals = [ 16384; 32768; 65536 ]
and map = StringMap.empty;;

let map = List.fold_right2 StringMap.add keys vals map;;
```



## ooRexx


```ooRexx
array1 = .array~of("Rick", "Mike", "David")
array2 = .array~of("555-9862", "555-5309", "555-6666")

-- if the index items are constrained to string objects, this can
-- be a directory too.
hash = .table~new

loop i = 1 to array1~size
    hash[array1[i]] = array2[i]
end
Say 'Enter a name'
Parse Pull name
Say name '->' hash[name]
```

```txt
Enter a name
Rick
Rick -> 555-9862
```



## Oz


```oz
declare
  fun {ZipRecord Keys Values}
     {List.toRecord unit {List.zip Keys Values MakePair}}
  end

  fun {MakePair A B}
     A#B
  end
in
  {Show {ZipRecord [a b c] [1 2 3]}}
```



## PARI/GP



```parigp
hash(key, value)=Map(matrix(#key,2,x,y,if(y==1,key[x],value[x])));
```



## Pascal

```pascal
program HashFromTwoArrays (Output);

uses
  contnrs;

var
  keys:   array[1..3] of string  = ('a', 'b', 'c');
  values: array[1..3] of integer = ( 1,   2,   3 );
  hash:   TFPDataHashTable;
  i:      integer;

begin
  hash := TFPDataHashTable.Create;
  for i := low(keys) to high(keys) do
    hash.add(keys[i], @values[i]);
  writeln ('Length of hash table: ', hash.Count);
  hash.Destroy;
end.
```

```txt
% ./HashFromTwoArrays
Length of hash table: 3

```



## Perl


```perl
my @keys = qw(a b c);
my @vals = (1, 2, 3);
my %hash;
@hash{@keys} = @vals;
```


Alternatively, using {{libheader|List::MoreUtils}}:


```perl
use List::MoreUtils qw(zip);
my %hash = zip @keys, @vals;
```



## Perl 6


Using the "zipwith" meta-operator on the <tt>=></tt> pair composer:

```perl6
my @keys = <a b c d e>
;
my @values = ^5;

my %hash = @keys Z=> @values;


#Alternatively, by assigning to a hash slice:
%hash{@keys} = @values;


# Or to create an anonymous hash:
%( @keys Z=> @values );


# All of these zip forms trim the result to the length of the shorter of their two input lists.
# If you wish to enforce equal lengths, you can use a strict hyperoperator instead:

quietly # suppress warnings about unused hash
{ @keys »=>« @values };  # Will fail if the lists differ in length
```



## Phix

You could of course make the values in the dictionary be indexes to valuearray instead, as shown commented out.

```Phix
function make_hash(sequence keyarray, sequence valuearray)
integer dict = new_dict()
    for i=1 to length(keyarray) do
        setd(keyarray[i],valuearray[i],dict)
--      setd(keyarray[i],i,dict)
    end for
    return dict
end function

constant keyarray   = {1,"two",PI}
constant valuearray = {"one",2,PI}
integer dict = make_hash(keyarray,valuearray)
?getd(1,dict)
?getd("two",dict)
?getd(PI,dict)
--?valuearray[getd(1,dict)]
```

```txt

"one"
2
3.141592654

```



## PHP

```php
$keys = array('a', 'b', 'c');
$values = array(1, 2, 3);
$hash = array_combine($keys, $values);
```


```php
$keys = array('a', 'b', 'c');
$values = array(1, 2, 3);
$hash = array();
for ($idx = 0; $idx < count($keys); $idx++) {
  $hash[$keys[$idx]] = $values[$idx];
}
```



## PicoLisp


```PicoLisp
(let (Keys '(one two three)  Values (1 2 3))
   (mapc println
      (mapcar cons Keys Values) ) )
```

```txt
(one . 1)
(two . 2)
(three . 3)
```



## Pop11



```pop11
vars keys = { 1 a b c};
vars vals = { 2 3 valb valc};
vars i;
;;; Create hash table
vars ht = newmapping([], 500, 0, true);
;;; Loop over input arrays (vectors)
for i from 1 to length(keys) do
  vals(i) -> ht(keys(i));
endfor;
```



## PostScript

```postscript

% push our arrays
 [/a /b /c /d /e] [1 2 3 4 5]
% create a dict with it
{aload pop} dip let currentdict end
% show that we have created the hash
 {= =} forall

```


## PowerShell


```powershell
function create_hash ([array] $keys, [array] $values) {
    $h = @{}
    if ($keys.Length -ne $values.Length) {
        Write-Error -Message "Array lengths do not match" `
                    -Category InvalidData `
                    -TargetObject $values
    } else {
        for ($i = 0; $i -lt $keys.Length; $i++) {
            $h[$keys[$i]] = $values[$i]
        }
    }
    return $h
}
```



## Prolog



```prolog
% this one with side effect hash table creation

:-dynamic hash/2.

make_hash([],[]).
make_hash([H|Q],[H1|Q1]):-
	assert(hash(H,H1)),
	make_hash(Q,Q1).

:-make_hash([un,deux,trois],[[a,b,c],[d,e,f],[g,h,i]])


% this one without side effects

make_hash_pure([],[],[]).
make_hash_pure([H|Q],[H1|Q1],[hash(H,H1)|R]):-
	make_hash_pure(Q,Q1,R).

:-make_hash_pure([un,deux,trois],[[a,b,c],[d,e,f],[g,h,i]],L),findall(M,(member(M,L),assert(M)),L).
```



## PureBasic


```PureBasic
Dim keys.s(3)
Dim vals.s(3)
NewMap Hash.s()

keys(0)="a" : keys(1)="b" : keys(2)="c" : keys(3)="d"
vals(0)="1" : vals(1)="2" : vals(2)="3" : vals(3)="4"
For n = 0 To 3
    Hash(keys(n))= vals(n)
Next
ForEach Hash()
   Debug Hash()
Next
```



## Python

Shows off the dict comprehensions in Python 3 (that were back-ported to 2.7):

```python
keys = ['a', 'b', 'c']
values = [1, 2, 3]
hash = {key: value for key, value in zip(keys, values)}
```


```python
keys = ['a', 'b', 'c']
values = [1, 2, 3]
hash = dict(zip(keys, values))

# Lazily, Python 2.3+, not 3.x:
from itertools import izip
hash = dict(izip(keys, values))
```


```python
keys = ['a', 'b', 'c']
values = [1, 2, 3]
hash = {}
for k,v in zip(keys, values):
    hash[k] = v
```


The original (Ruby) example uses a range of different types as keys. Here is similar in python (run at the shell):

```python
>>>
 class Hashable(object):
	def __hash__(self):
		return id(self) ^ 0xBEEF


>>> my_inst = Hashable()
>>> my_int = 1
>>> my_complex = 0 + 1j
>>> my_float = 1.2
>>> my_string = "Spam"
>>> my_bool = True
>>> my_unicode = u'Ham'
>>> my_list = ['a', 7]
>>> my_tuple = ( 0.0, 1.4 )
>>> my_set = set(my_list)
>>> def my_func():
	pass

>>> class my_class(object):
	pass

>>> keys = [my_inst, my_tuple, my_int, my_complex, my_float, my_string,
	my_bool, my_unicode, frozenset(my_set), tuple(my_list),
	my_func, my_class]
>>> values = range(12)
>>> d = dict(zip(keys, values))
>>> for key, value in d.items(): print key, ":", value

1 : 6
1j : 3
Ham : 7
Spam : 5
(0.0, 1.3999999999999999) : 1
frozenset(['a', 7]) : 8
1.2 : 4
('a', 7) : 9
<function my_func at 0x0128E7B0> : 10
<class '__main__.my_class'> : 11
<__main__.Hashable object at 0x012AFC50> : 0
>>> # Notice that the key "True" disappeared, and its value got associated with the key "1"
>>> # This is because 1 == True in Python, and dictionaries cannot have two equal keys
```



## R

Assuming that the keys are coercible to character form, we can simply use the names attribute to create a hash.   This example is taken from the [[wp:Hash_table#Separate_chaining|Wikipedia page on hash tables]].

```r
# Set up hash table
keys <- c("John Smith", "Lisa Smith", "Sam Doe", "Sandra Dee", "Ted Baker")
values <- c(152, 1, 254, 152, 153)
names(values) <- keys
# Get value corresponding to a key
values["Sam Doe"]                          # vals["Sam Doe"]
# Get all keys corresponding to a value
names(values)[values==152]                 # "John Smith" "Sandra Dee"
```



## Racket


```racket

(make-hash (map cons '("a" "b" "c" "d") '(1 2 3 4)))
```


Alternatively:

```racket

(define (connect keys vals)  (for/hash ([k keys] [v vals]) (values k v)))
;; Example:
(connect #("a" "b" "c" "d") #(1 2 3 4))

```



## Raven



```raven
[ 'a' 'b' 'c' ] as $keys [ 1 2 3 ] as $vals
$keys $vals combine as $hash
```



## REXX

This REXX version allows multiple keys for a value,   the keys are case sensitive.

```rexx
/*REXX program demonstrates  hashing  of a  stemmed array  (from a key or multiple keys)*/
key.=                                            /*names of the nine regular polygons.  */
vals= 'triangle quadrilateral pentagon hexagon heptagon octagon nonagon decagon dodecagon'
key.1='thuhree  vour          phive    sicks   zeaven   ate     nein    den     duzun'
key.2='three    four          five     six     seven    eight   nine    ten     twelve'
key.3='3        4             5        6       7        8       9       10      12'
key.4='III      IV            V        VI      VII      VIII    IX      X       XII'
key.5='iii      iv            v        vi      vii      viii    ix      x       xii'
hash.='───(not defined)───'                      /* [↑]  blanks added to humorous keys  */
                                                 /*      just because it looks prettier.*/
    do k=1  while key.k\==''
    call hash vals,key.k                         /*hash the   keys   to the   values.   */
    end   /*k*/

parse arg query .                                /*obtain what was specified on the C.L.*/
if query==''  then exit                          /*Nothing?  Then leave Dodge City.     */
say 'key:'  left(query,40)  "value:"  hash.query /*display some stuff to the terminal.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
hash: parse arg @val,@key
                do j=1  for words(@key);          map= word(@key, j)
                                             hash.map= word(@val, j)
                end   /*j*/
      return
```

```txt

key: phive                                    value: pentagon

```

```txt

key: dingsta                                  value: ───(not defined)───

```



## Ring


```ring

# Project : Hash from two arrays

list1="one two three"
list2="1 2 3"
a = str2list(substr(list1," ",nl))
b = str2list(substr(list2," ",nl))
c = list(len(a))
for i=1 to len(b)
     temp = number(b[i])
     c[temp] = a[i]
next
for i = 1 to len(c)
     see c[i] + " " + i + nl
next

```

Output:

```txt

one 1
two 2
three 3

```



## Ruby


```ruby

keys = ['hal',666,[1,2,3]]
vals = ['ibm','devil',123]

hash = Hash[keys.zip(vals)]

p hash  # => {"hal"=>"ibm", 666=>"devil", [1, 2, 3]=>123}

#retrieve the value linked to the key [1,2,3]
puts hash[ [1,2,3] ]  # => 123

```


In Ruby 2.1 the method "to_h" was introduced:

```ruby
keys = ['hal', 666, [1,2,3]]
vals = ['ibm', 'devil', 123]

keys.zip(vals).to_h
```



## Rust


```rust
use std::collections::HashMap;

fn main() {
    let keys = ["a", "b", "c"];
    let values = [1, 2, 3];

    let hash = keys.iter().zip(values.iter()).collect::<HashMap<_, _>>();
    println!("{:?}", hash);
}
```



## Sather


```sather
class ZIPPER{K,E} is
  zip(k:ARRAY{K}, e:ARRAY{E}) :MAP{K, E}
    pre k.size = e.size
  is
    m :MAP{K, E} := #;
    loop m[k.elt!] := e.elt!; end;
    return m;
  end;
end;

class MAIN is

  main is
    keys:ARRAY{STR} := |"one", "three", "four"|;
    values:ARRAY{INT} := |1, 3, 4|;
    m ::= ZIPPER{STR,INT}::zip(keys, values);
    loop
      #OUT + m.pair! + " ";
    end;
    #OUT + "\n";
  end;

end;
```



## Scala


```scala
val keys = List(1, 2, 3)
val values = Array("A", "B", "C") // Array mixed with List
val map = keys.zip(values).toMap  // and other Seq are possible.

// Testing
assert(map == Map(1 ->"A", 2 -> "B", 3 -> "C"))
println("Successfully completed without errors.")
```



## Scheme

Using [http://srfi.schemers.org/srfi-69/srfi-69.html SRFI 69]:

```scheme
(define (lists->hash-table keys values . rest)
  (apply alist->hash-table (map cons keys values) rest))
```



## Seed7



```seed7
$ include "seed7_05.s7i";

const type: numericHash is hash [string] integer;
var numericHash: myHash is numericHash.value;

const proc: main is func
  local
    var array string: keyList is [] ("one", "two", "three");
    var array integer: valueList is [] (1, 2, 3);
    var integer: number is 0;
  begin
    for number range 1 to length(keyList) do
      myHash @:= [keyList[number]] valueList[number];
    end for;
  end func;
```



## Sidef


```ruby
var keys = %w(a b c)
var vals = [1, 2, 3]

var hash = Hash()
hash{keys...} = vals...
say hash
```



## Smalltalk

```smalltalk
Array extend [
  dictionaryWithValues: array [ |d|
    d := Dictionary new.
    1 to: ((self size) min: (array size)) do: [:i|
      d at: (self at: i) put: (array at: i).
    ].
    ^ d
  ]
].


({ 'red' . 'one' . 'two' }
 dictionaryWithValues: { '#ff0000'. 1. 2 }) displayNl.
```


```smalltalk
Dictionary
    withKeys:#('one' 'two' 'three')
    andValues:#('eins' 'zwei' 'drei')
```


```smalltalk
Dictionary withAssociations:{ 'one'->1 . 'two'->2 . 'three'->3 }
```



## SNOBOL4


```SNOBOL4
*       # Fill arrays
        keys = array(5); vals = array(5)
        ks = 'ABCDE'; vs = '12345'
kloop   i = i + 1; ks len(1) . keys<i> = :s(kloop)
vloop   j = j + 1; vs len(1) . vals<j> = :s(vloop)

*       # Create hash
        hash = table(5)
hloop   k = k + 1; hash<keys<k>> = vals<k> :s(hloop)

*       # Test and display
        ts = 'ABCDE'
tloop   ts len(1) . ch = :f(out)
        str = str ch ':' hash<ch> ' ' :(tloop)
out     output = str
end
```


```txt
A:1 B:2 C:3 D:4 E:5
```



## Sparkling


```sparkling
let keys = { "foo", "bar", "baz" };
let vals = { 13, 37, 42 };
var hash = {};
for var i = 0; i < sizeof keys; i++ {
    hash[keys[i]] = vals[i];
}
```



## Standard ML

Using functional binary search trees instead of hash tables:


```sml
structure StringMap = BinaryMapFn (struct
                                     type ord_key = string
                                     val compare = String.compare
                                   end);

val keys = [ "foo", "bar", "baz" ]
and vals = [ 16384, 32768, 65536 ]
and myMap = StringMap.empty;

val myMap = foldl StringMap.insert' myMap (ListPair.zipEq (keys, vals));
```


Using hash tables:


```sml
exception NotFound;

val keys = [ "foo", "bar", "baz" ]
and vals = [ 16384, 32768, 65536 ]
and hash = HashTable.mkTable (HashString.hashString, op=) (42, NotFound);

ListPair.appEq (HashTable.insert hash) (keys, vals);
```



## Swift

```swift
let keys = ["a","b","c"]
let vals = [1,2,3]
var hash = [String: Int]()
for (key, val) in zip(keys, vals) {
  hash[key] = val
}
```



## Tcl


Arrays in Tcl are automatically associative,
i.e. there are no "not hashed arrays".

If we can take "arrays of equal length" to mean
"<i>lists</i> of equal length",
then the task might look like this:

```tcl
set keys   [list fred bob joe]
set values [list barber plumber tailor]
array set arr {}
foreach a $keys b $values { set arr($a) $b }

parray arr
```

```txt

arr(bob)  = plumber
arr(fred) = barber
arr(joe)  = tailor

```


Alternatively, a dictionary could be used: <!-- http://ideone.com/6lI4k5 -->

```tcl
package require Tcl 8.5

set keys   [list fred bob joe]
set values [list barber plumber tailor]

foreach a $keys b $values {
    dict set jobs $a $b
}

puts "jobs: [dict get $jobs]"
```

```txt

jobs: fred barber bob plumber joe tailor

```



## TXR


===One-liner, using quasiquoted hash syntax===


```bash
$ txr -p  '^#H(() ,*[zip #(a b c) #(1 2 3)])))'
#H(() (c 3) (b 2) (a 1))
```


===One-liner, using <code>hash-construct</code> function===


```bash
$ txr -p  '(hash-construct nil [zip #(a b c) #(1 2 3)])))'
#H(() (c 3) (b 2) (a 1))
```



### Explicit construction and stuffing



```txrlisp
(defun hash-from-two (vec1 vec2 . hash-args)
  (let ((table (hash . hash-args)))
    (mapcar (do sethash table) vec1 vec2)
    table))

(prinl (hash-from-two #(a b c) #(1 2 3)))
```



```txt
$ ./txr hash-from-two.tl
#H(() (c 3) (b 2) (a 1))
```



## UnixPipes

Using a sorted file as an associative array (see Creating an associative array for usage.)


```bash
cat <<VAL>
p.values
apple
boy
cow
dog
elephant
VAL

cat <<KEYS >p.keys
a
b
c
d
e
KEYS

paste -d\   <(cat p.values | sort) <(cat p.keys | sort)
```



## UNIX Shell

```bash
keys=( foo bar baz )
values=( 123 456 789 )
declare -A hash

for (( i = 0; i < ${#keys[@]}; i++ )); do
  hash["${keys[i]}"]=${values[i]}
done

for key in "${!hash[@]}"; do
  printf "%s => %s\n" "$key" "${hash[$key]}"
done
```


```txt
bar => 456
baz => 789
foo => 123
```



## Ursala

There is a built-in operator for this.

```Ursala
keys   = <'foo','bar','baz'>
values = <12354,145430,76748>

hash_function = keys-$values
```

test program:

```Ursala
#cast %nL

test = hash_function* <'bar','baz','foo','bar'>
```

```txt
<145430,76748,12354,145430>
```



## Vala

```vala

using Gee;

void main(){
    // mostly copied from C# example
    var hashmap = new HashMap<string, string>();

    string[] arg_keys = {"foo", "bar", "val"};
    string[] arg_values = {"little", "miss", "muffet"};

    if (arg_keys.length	== arg_values.length ){
	for (int i = 0;	i < arg_keys.length; i++){
            hashmap[arg_keys[i]] = arg_values[i];
	}
    }
}

```



## VBScript


VBScript (and Visual Basic in general) calls hashes "dictionary objects".


```vb
Set dict = CreateObject("Scripting.Dictionary")
os = Array("Windows", "Linux", "MacOS")
owner = Array("Microsoft", "Linus Torvalds", "Apple")
For n = 0 To 2
    dict.Add os(n), owner(n)
Next
MsgBox dict.Item("Linux")
MsgBox dict.Item("MacOS")
MsgBox dict.Item("Windows")
```


{{out}} (in message boxes):
 Linus Torvalds
 Apple
 Microsoft


## Visual Basic

The [[Hash from two arrays#VBScript|VBScript]] version can be used
in Visual Basic unchanged, although it requires a reference to
the [[Windows Script Host|Microsoft Scripting Runtime (scrrun.dll)]].

Alternately, instead of a <code>Dictionary</code> object,
you can also use a <code>Collection</code> object,
which serves a similar purpose, without the inclusion
of an additional runtime library.
In fact, the only immediately-obvious difference between this
and the VBScript example is <code>dict</code>'s data type,
and the order that the arguments are passed to the <code>Add</code> method.


```vb
Dim dict As New Collection
os = Array("Windows", "Linux", "MacOS")
owner = Array("Microsoft", "Linus Torvalds", "Apple")
For n = 0 To 2
    dict.Add owner(n), os(n)
Next
Debug.Print dict.Item("Linux")
Debug.Print dict.Item("MacOS")
Debug.Print dict.Item("Windows")
```



## WDTE


```wdte
let a =>
 import 'arrays';
let s => import 'stream';

let toScope keys vals =>
    s.zip (a.stream keys) (a.stream vals)
    ->
        s.reduce (collect (true)) (@ r scope kv =>
            let [k v] => kv;
            set scope k v;
        )
    ;
```


'''Example:'''


```WDTE
toScope
    ['a'; 'b'; 'c']
    [1; 2; 3]
    : scope
-> known
-> a.stream
-> s.map (@ m k => [k; at scope k])
-> s.collect
-- io.writeln io.stdout
;
```


```txt
[[a; 1]; [b; 2]; [c; 3]]
```



## Wortel

Wortel has an inbuilt operator to do this: <code>@hash</code>.

```wortel
@hash ["a" "b" "c"] [1 2 3] ; returns {a 1 b 2 c 3}
```

This function can also be defined as:

```wortel
^(@obj @zip)
```

Example:

```wortel
@let {
  hash ^(@obj @zip)
  !!hash ["a" "b" "c"] [1 2 3]
}
```

```txt
{a 1 b 2 c 3}
```



## zkl


```zkl
keys:=T("a","b","c","d"); vals:=T(1,2,3,4);
d:=keys.zip(vals).toDictionary();
d.println();
d["b"].println();
```

```txt
D(a:1,b:2,c:3,d:4)
2

```

