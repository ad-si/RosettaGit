+++
title = "Associative array/Iteration"
description = ""
date = 2019-10-17T04:38:06Z
aliases = []
[extra]
id = 4633
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}[[Category:Iteration]][[Category:Data Structures]]
Show how to iterate over the key-value pairs of an associative array, and print each pair out.

Also show how to iterate just over the keys, or the values, if there is a separate way to do that in your language.


{{Template:See also lists}}





## 11l


```11l
V d = [‘key1’ = ‘value1’, ‘key2’ = ‘value2’]

L(key, value) d
   print(key‘ = ’value)

L(key) d.keys()
   print(key)

L(value) d.values()
   print(value)
```

{{out}}

```txt

key1 = value1
key2 = value2
key1
key2
value1
value2

```



## 8th

Iterating key,value pairs uses "m:each":

```Forth

{"one": 1, "two": "bad"}
( swap . space . cr )
m:each

```

{{out}}
```txt

one 1
two bad

```


Iterating the keys uses "m:keys":

```Forth

{"one": 1, "two": "bad"} m:keys
( . cr )
a:each

```

{{out}}
```txt

one
two

```



## Ada


```Ada
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Containers.Indefinite_Ordered_Maps;

procedure Test_Iteration is
   package String_Maps is
      new Ada.Containers.Indefinite_Ordered_Maps (String, Integer);
   use String_Maps;
   A     : Map;
   Index : Cursor;
begin
   A.Insert ("hello", 1);
   A.Insert ("world", 2);
   A.Insert ("!",     3);
   Index := A.First;
   while Index /= No_Element loop
      Put_Line (Key (Index) & Integer'Image (Element (Index)));
      Index := Next (Index);
   end loop;
end Test_Iteration;
```

{{out}}

```txt

! 3
hello 1
world 2

```



## ALGOL 68

Algol 68 does not have associative arrays as standard.


This sample defines a simple hash-based implementation with operators to iterate over the array.

```algol68
# associative array handling using hashing                                   #

# the modes allowed as associative array element values - change to suit     #
MODE AAVALUE = STRING;
# the modes allowed as associative array element keys - change to suit       #
MODE AAKEY   = STRING;
# nil element value                                                          #
REF AAVALUE nil value = NIL;

# an element of an associative array                                         #
MODE AAELEMENT = STRUCT( AAKEY key, REF AAVALUE value );
# a list of associative array elements - the element values with a           #
# particular hash value are stored in an AAELEMENTLIST                       #
MODE AAELEMENTLIST = STRUCT( AAELEMENT element, REF AAELEMENTLIST next );
# nil element list reference                                                 #
REF AAELEMENTLIST nil element list = NIL;
# nil element reference                                                      #
REF AAELEMENT     nil element      = NIL;

# the hash modulus for the associative arrays                                #
INT hash modulus = 256;

# generates a hash value from an AAKEY - change to suit                      #
OP HASH = ( STRING key )INT:
BEGIN
    INT result := ABS ( UPB key - LWB key ) MOD hash modulus;
    FOR char pos FROM LWB key TO UPB key DO
        result PLUSAB ( ABS key[ char pos ] - ABS " " );
        result MODAB  hash modulus
    OD;
    result
END; # HASH #

# a mode representing an associative array                                    #
MODE AARRAY = STRUCT( [ 0 : hash modulus - 1 ]REF AAELEMENTLIST elements
                    , INT                                       curr hash
                    , REF AAELEMENTLIST                         curr position
                    );

# initialises an associative array so all the hash chains are empty           #
OP   INIT = ( REF AARRAY array )REF AARRAY:
     BEGIN
         FOR hash value FROM 0 TO hash modulus - 1 DO ( elements OF array )[ hash value ] := nil element list OD;
         array
     END; # INIT #

# gets a reference to the value corresponding to a particular key in an      #
# associative array - the element is created if it doesn't exist             #
PRIO // = 1;
OP   // = ( REF AARRAY array, AAKEY key )REF AAVALUE:
BEGIN
    REF AAVALUE result;
    INT         hash value = HASH key;
    # get the hash chain for the key #
    REF AAELEMENTLIST element := ( elements OF array )[ hash value ];
    # find the element in the list, if it is there #
    BOOL found element := FALSE;
    WHILE ( element ISNT nil element list )
      AND NOT found element
    DO
        found element := ( key OF element OF element = key );
        IF found element
        THEN
            result  := value OF element OF element
        ELSE
            element := next OF element
        FI
    OD;
    IF NOT found element
    THEN
        # the element is not in the list #
        # - add it to the front of the hash chain #
        ( elements OF array )[ hash value ]
                            := HEAP AAELEMENTLIST
                            := ( HEAP AAELEMENT := ( key
                                                   , HEAP AAVALUE := ""
                                                   )
                               , ( elements OF array )[ hash value ]
                               );
        result := value OF element OF ( elements OF array )[ hash value ]
    FI;
    result
END; # // #

# returns TRUE if array contains key, FALSE otherwise                        #
PRIO CONTAINSKEY = 1;
OP   CONTAINSKEY = ( REF AARRAY array, AAKEY key )BOOL:
BEGIN
    # get the hash chain for the key #
    REF AAELEMENTLIST element := ( elements OF array )[ HASH key ];
    # find the element in the list, if it is there #
    BOOL found element := FALSE;
    WHILE ( element ISNT nil element list )
      AND NOT found element
    DO
        found element := ( key OF element OF element = key );
        IF NOT found element
        THEN
            element := next OF element
        FI
    OD;
    found element
END; # CONTAINSKEY #

# gets the first element (key, value) from the array                         #
OP FIRST = ( REF AARRAY array )REF AAELEMENT:
BEGIN
    curr hash     OF array := LWB ( elements OF array ) - 1;
    curr position OF array := nil element list;
    NEXT array
END; # FIRST #

# gets the next element (key, value) from the array                          #
OP NEXT  = ( REF AARRAY array )REF AAELEMENT:
BEGIN
    WHILE ( curr position OF array IS nil element list )
      AND   curr hash     OF array < UPB ( elements OF array )
    DO
        # reached the end of the current element list - try the next         #
        curr hash     OF array +:= 1;
        curr position OF array  := ( elements OF array )[ curr hash OF array ]
    OD;
    IF   curr hash OF array > UPB ( elements OF array )
    THEN
        # no more elements #
        nil element
    ELIF curr position OF array IS nil element list
    THEN
        # reached the end of the table #
        nil element
    ELSE
        # have another element #
        REF AAELEMENTLIST found element = curr position OF array;
        curr position OF array := next OF curr position OF array;
        element OF found element
    FI
END; # NEXT #

# test the associative array #
BEGIN
    # create an array and add some values  #
    REF AARRAY a1 := INIT LOC AARRAY;
    a1 // "k1" := "k1 value";
    a1 // "z2" := "z2 value";
    a1 // "k1" := "new k1 value";
    a1 // "k2" := "k2 value";
    a1 // "2j" := "2j value";
    # iterate over the values #
    REF AAELEMENT e := FIRST a1;
    WHILE e ISNT nil element
    DO
        print( ( "  (" + key OF e + ")[" + value OF e + "]", newline ) );
        e := NEXT a1
    OD
END
```

{{out}}

```txt

  (2j)[2j value]
  (k1)[new k1 value]
  (k2)[k2 value]
  (z2)[z2 value]

```



## Aime


```aime
record r;
text s;

r_put(r, "A", 33);              # an integer value
r_put(r, "C", 2.5);             # a real value
r_put(r, "B", "associative");   # a string value

if (r_first(r, s)) {
    do {
        o_form("key ~, value ~ (~)\n", s, r[s], r_type(r, s));
    } while (rsk_greater(r, s, s));
}
```

{{out}}

```txt
key A, value 33 (integer)
key B, value associative (text)
key C, value 2.5 (real)
```



## App Inventor

Associative arrays in App Inventor are lists of ''key:value'' 'pairs'.

When a list is organized as pairs, the '''lookup in pairs''' block can be used to retrieve an associated value from a key name.

[https://lh3.googleusercontent.com/-Cxw_-XGMRyM/UutOX1bEH9I/AAAAAAAAJ9g/MZotfuSEziY/s1600/CreateIterateLookup.PNG '''<VIEW BLOCKS AND ANDROID APP>''']


## Arturo



```arturo
// create a dictionary
dict #{
	name 	"john"
	surname "doe"
	age 	33
}

// Iterate over key/value pairs
loop dict {
	print "key = " + &0 + ", value = " + &1
}

"----"

// Iterate over keys
loop $(keys dict) {
	print "key = " + &
}

"----"

// Iterate over values
loop $(values dict) {
	print "value = " + &
}
```


{{out}}


```txt
key = surname, value = doe
key = age, value = 33
key = name, value = john
----
key = surname
key = age
key = name
----
value = doe
value = 33
value = john
```



## AutoHotkey

{{works with|AutoHotkey_L}}
From the [http://www.autohotkey.net/~Lexikos/AutoHotkey_L/docs/objects/Enumerator.htm documentation]
```AutoHotkey
; Create an associative array
obj := Object("red", 0xFF0000, "blue", 0x0000FF, "green", 0x00FF00)
enum := obj._NewEnum()
While enum[key, value]
    t .= key "=" value "`n"
MsgBox % t
```



## AWK

In AWK "arrays" are always associative arrays, and the only way to iterate over them is by keys (''indexes'' in the AWK terminology)


```awk
BEGIN {
  a["hello"] = 1
  a["world"] = 2
  a["!"] = 3

  # iterate over keys
  for(key in a) {
    print key, a[key]
  }
}
```



## Babel


In Babel, associative arrays are referred to as maps. To create a map from a list-of-lists:


```babel
births (('Washington' 1732) ('Lincoln' 1809) ('Roosevelt' 1882) ('Kennedy' 1917)) ls2map ! <
```


To iterate over a map, in the primary sense, use the overmap utility. We will copy the map (cp operator) so as not to modify the original:


```babel
births cp dup {1 +} overmap !
```


To see the results, use the valmap operator:


```babel
valmap ! lsnum !
```


{{out}}

```txt
( 1918 1733 1883 1810 )
```


There are many ways to interact with a map in Babel. Most of these begin by converting the map to a list or list-of-lists. To look up a list of specific values from the map, by key, use the lumapls utility:


```babel
births ('Roosevelt' 'Kennedy') lumapls ! lsnum !
```


{{out}}

```txt
( 1882 1917 )
```


To convert the entire map back to a list of key-value pairs:


```babel
births map2ls !
```


To view the list:


```babel
{give swap << " " << itod << "\n" <<} each
```


{{out}}

```txt
Kennedy 1917
Washington 1732
Roosevelt 1882
Lincoln 1809
```


To merge two maps together, use the mapmerge utility:


```babel
foo (("bar" 17) ("baz" 42)) ls2map ! <
births foo mergemap !
```


To view the results:


```babel
births map2ls ! {give swap << " " << itod << "\n" <<} each
```


{{out}}

```txt
baz 42
Kennedy 1917
bar 17
Washington 1732
Roosevelt 1882
Lincoln 1809
```


For more information on maps in Babel, view [https://github.com/claytonkb/clean_babel/blob/master/std.sp std.sp] (see the section titled "map utilities").


## BaCon


```qbasic
DECLARE associative ASSOC STRING

associative("abc") = "first three"
associative("mn") = "middle two"
associative("xyz") = "last three"

LOOKUP associative TO keys$ SIZE amount
FOR i = 0 TO amount - 1
    PRINT keys$[i], ":", associative(keys$[i])
NEXT
```


{{out}}

```txt
prompt$ ./assoc
abc:first three
mn:middle two
xyz:last three
```

LOOKUP creates a numerically indexed array of the keys of the associative array, with the number of elements stored in the field following the SIZE keyword.


## BASIC256

''Solution is at [[Associative_array/Creation#BASIC256]]''.


## BBC BASIC


```bbcbasic
      REM Store some values with their keys:
      PROCputdict(mydict$, "FF0000", "red")
      PROCputdict(mydict$, "00FF00", "green")
      PROCputdict(mydict$, "0000FF", "blue")

      REM Iterate through the dictionary:
      i% = 1
      REPEAT
        i% = FNdict(mydict$, i%, v$, k$)
        PRINT v$, k$
      UNTIL i% = 0
      END

      DEF PROCputdict(RETURN dict$, value$, key$)
      IF dict$ = "" dict$ = CHR$(0)
      dict$ += key$ + CHR$(1) + value$ + CHR$(0)
      ENDPROC

      DEF FNdict(dict$, I%, RETURN value$, RETURN key$)
      LOCAL J%, K%
      J% = INSTR(dict$, CHR$(1), I%)
      K% = INSTR(dict$, CHR$(0), J%)
      value$ = MID$(dict$, I%+1, J%-I%-1)
      key$ = MID$(dict$, J%+1, K%-J%-1)
      IF K% >= LEN(dict$) THEN K% = 0
      = K%
```



## Bracmat


```bracmat
(  new$hash:?myhash
& (myhash..insert)$(title."Some title")
& (myhash..insert)$(formula.a+b+x^7)
& (myhash..insert)$(fruit.apples oranges kiwis)
& (myhash..insert)$(meat.)
& (myhash..insert)$(fruit.melons bananas)
& (myhash..remove)$formula
& (myhash..insert)$(formula.x^2+y^2)
&   (myhash..forall)
  $ (
    =   key value
      .     whl
          ' ( !arg:(?key.?value) ?arg
            & put$("key:" !key "\nvalue:" !value \n)
            )
        & put$\n
    )
);
```

{{out}}

```txt
key: meat
value:

key: title
value: Some title

key: formula
value: x^2+y^2

key: fruit
value: melons bananas
key: fruit
value: apples oranges kiwis

```



## Brat


```brat
h = [ hello: 1 world: 2 :! : 3]

#Iterate over key, value pairs
h.each { k, v |
  p "Key: #{k} Value: #{v}"
}

#Iterate over keys
h.each_key { k |
  p "Key: #{k}"
}

#Iterate over values
h.each_value { v |
  p "Value: #{v}"
}
```



## C

''Solution is at [[Associative arrays/Creation/C]]''.


## C++

{{works with|C++11}}

```cpp
#include <iostream>
#include <map>
#include <string>

int main() {
  std::map<std::string, int> dict {
    {"One", 1},
    {"Two", 2},
    {"Three", 7}
  };

  dict["Three"] = 3;

  std::cout << "One: " << dict["One"] << std::endl;
  std::cout << "Key/Value pairs: " << std::endl;
  for(auto& kv: dict) {
    std::cout << "  " << kv.first << ": " << kv.second << std::endl;
  }

  return 0;
}
```



Pre C++11:

```cpp
std::map<std::string, int> myDict;
myDict["hello"] = 1;
myDict["world"] = 2;
myDict["!"] = 3;

// iterating over key-value pairs:
for (std::map<std::string, int>::iterator it = myDict.begin(); it != myDict.end(); ++it) {
    // the thing pointed to by the iterator is an std::pair<const std::string, int>&
    const std::string& key = it->first;
    int& value = it->second;
    std::cout << "key = " << key << ", value = " << value << std::endl;
}
```



## C sharp


```csharp
using System;
using System.Collections.Generic;

namespace AssocArrays
{
    class Program
    {
        static void Main(string[] args)
        {

            Dictionary<string,int> assocArray = new Dictionary<string,int>();

            assocArray["Hello"] = 1;
            assocArray.Add("World", 2);
            assocArray["!"] = 3;

            foreach (KeyValuePair<string, int> kvp in assocArray)
            {
                Console.WriteLine(kvp.Key + " : " + kvp.Value);
            }

            foreach (string key in assocArray.Keys)
            {
                Console.WriteLine(key);
            }

            foreach (int val in assocArray.Values)
            {
                Console.WriteLine(val.ToString());
            }
        }
    }
}

```



## Ceylon


```ceylon
shared void run() {

	value myMap = map {
		"foo" -> 5,
		"bar" -> 10,
		"baz" -> 15
	};

	for(key in myMap.keys) {
		print(key);
	}

	for(item in myMap.items) {
		print(item);
	}

	for(key->item in myMap) {
		print("``key`` maps to ``item``");
	}

}
```



## Chapel



```chapel
var A = [ "H2O" => "water", "NaCl" => "salt", "O2" => "oxygen" ];

for k in A.domain do
    writeln("have key: ", k);

for v in A do
    writeln("have value: ", v);

for (k,v) in zip(A.domain, A) do
    writeln("have element: ", k, " -> ", v);
```


{{out}}
 have key: O2
 have key: NaCl
 have key: H2O
 have value: oxygen
 have value: salt
 have value: water
 have element: O2 -> oxygen
 have element: NaCl -> salt
 have element: H2O -> water


## Clojure


```clojure

(doseq [[k v] {:a 1, :b 2, :c 3}]
  (println k "=" v))

(doseq [k  (keys {:a 1, :b 2, :c 3})]
  (println k))

(doseq [v  (vals {:a 1, :b 2, :c 3})]
  (println v))

```



## CoffeeScript


```coffeescript
hash =
  a: 'one'
  b: 'two'

for key, value of hash
  console.log key, value

for key of hash
  console.log key

```



## Common Lisp


Common Lisp has three common idioms for associating keys with values: association lists (alists), property lists (plists), and hash tables.

===With association lists (alists)===
The association list is a list of conses, each of whose <code>car</code> is a key and whose <code>cdr</code> is a value.  The standard mapping and print functions can be used to print key/value pairs, keys, and values.


```lisp
;; iterate using dolist, destructure manually
(dolist (pair alist)
  (destructuring-bind (key . value) pair
    (format t "~&Key: ~a, Value: ~a." key value)))

;; iterate and destructure with loop
(loop for (key . value) in alist
      do (format t "~&Key: ~a, Value: ~a." key value))
```


===With property lists (plists)===

Property lists are lists of alternating keys and values, where each value's key is the element of the list immediately following it.  Printing could be done with standard mapping functions, but <code>loop</code>'s destructuring makes things a bit easier.


```lisp
(loop for (key value) on plist :by 'cddr
      do (format t "~&Key: ~a, Value: ~a." key value))
```



### With hash tables


Lisp also has built-in hash tables, and there are several ways to map over these.  The first is <code>maphash</code> which takes a function of two arguments (the key and value) and the hash table.


```lisp
(maphash (lambda (key value)
           (format t "~&Key: ~a, Value: ~a." key value))
         hash-table)
```


The <code>loop</code> construct also supports extracting key/value pairs from hash tables.


```lisp
(loop for key being each hash-key of hash-table using (hash-value value)
      do (format t "~&Key: ~a, Value: ~a." key value))
```


There is also a macro <code>with-hash-table-iterator</code> which locally binds a name to produce associated keys and values of the hash table; while rarely used, it is the most powerful operation.


```lisp
(with-hash-table-iterator (next-entry hash-table)
  (loop
   (multiple-value-bind (nextp key value) (next-entry)
     (if (not nextp)
       (return)
       (format t "~&Key: ~a, Value: ~a." key value)))))
```


### Alternate solution

I use [https://franz.com/downloads/clp/survey Allegro CL 10.1]


```lisp

;; Project : Associative array/Iteration

(setf x (make-array '(3 2)
           :initial-contents '(("hello" 13 ) ("world" 31) ("!" 71))))
(setf xlen (array-dimensions x))
(setf len (car xlen))
(dotimes (n len)
               (terpri)
               (format t "~a" (aref x n 0))
               (format t "~a" " : ")
               (format t "~a" (aref x n 1)))

```

Output:

```txt

hello : 13
world : 31
! : 71

```



## D

{{works with|D|2}}


```d
import std.stdio: writeln;

void main() {
    // the associative array
    auto aa = ["alice":2, "bob":97, "charlie":45];

    // how to iterate key/value pairs:
    foreach (key, value; aa)
        writeln("1) Got key ", key, " with value ", value);
    writeln();

    // how to iterate the keys:
    foreach (key, _; aa)
        writeln("2) Got key ", key);
    writeln();

    // how to iterate the values:
    foreach (value; aa)
        writeln("3) Got value ", value);
    writeln();

    // how to extract the values, lazy:
    foreach (value; aa.byValue())
        writeln("4) Got value ", value);
    writeln();

    // how to extract the keys, lazy:
    foreach (key; aa.byKey())
        writeln("5) Got key ", key);
    writeln();

    // how to extract all the keys:
    foreach (key; aa.keys)
        writeln("6) Got key ", key);
    writeln();

    // how to extract all the values:
    foreach (value; aa.values)
        writeln("7) Got value ", value);
}
```



## Dao


```ruby

dict = { 'def' => 1, 'abc' => 2 }

for( keyvalue in dict ) io.writeln( keyvalue );
for( key in dict.keys(); value in dict.values() ) io.writeln( key, value )
dict.iterate { [key, value]
    io.writeln( key, value )
}

```



## Delphi


```Delphi
program AssociativeArrayIteration;

{$APPTYPE CONSOLE}

uses SysUtils, Generics.Collections;

var
  i: Integer;
  s: string;
  lDictionary: TDictionary<string, Integer>;
  lPair: TPair<string, Integer>;
begin
  lDictionary := TDictionary<string, Integer>.Create;
  try
    lDictionary.Add('foo', 5);
    lDictionary.Add('bar', 10);
    lDictionary.Add('baz', 15);
    lDictionary.AddOrSetValue('foo', 6);

    for lPair in lDictionary do
      Writeln(Format('Pair: %s = %d', [lPair.Key, lPair.Value]));
    for s in lDictionary.Keys do
      Writeln('Key: ' + s);
    for i in lDictionary.Values do
      Writeln('Value: ', i);
  finally
    lDictionary.Free;
  end;
end.
```



## Dyalect



```dyalect
var t = (x: 1, y: 2, z: 3)

for x in t.keys() {
    print("\(x)=\(t[x])")
}
```


{{out}}


```txt
x=1
y=2
z=3
```



## E


In E, the basic iteration protocol and syntax work over key-value pairs. Therefore, any iteration over a ''map'' or other collection is always key-value, though the user may choose to ignore the keys or the values.

The <code>for</code> loop takes either one pattern, for the value, or two, for the key and value; for iterating over keys alone the value may be given an ignore-pattern (<code>_</code>).


```e
def map := [
  "a" => 1,
  "b" => 2,
  "c" => 3,
]

for key => value in map {
  println(`$key $value`)
}

for value in map {     # ignore keys
  println(`. $value`)
}

for key => _ in map {  # ignore values
  println(`$key .`)
}

for key in map.domain() {     # iterate over the set whose values are the keys
  println(`$key .`)
}
```



## EchoLisp


```scheme

(lib 'hash) ;; load hash.lib
(define H (make-hash))
;; fill hash table
(hash-set H 'Simon 42)
(hash-set H 'Albert 666)
(hash-set H 'Antoinette 33)

;; iterate over (key . value ) pairs
(for ([kv H]) (writeln kv))
(Simon . 42)
(Albert . 666)
(Antoinette . 33)

;; iterate over keys
(for ([k (hash-keys H)]) (writeln 'key-> k))
key->     Simon
key->     Albert
key->     Antoinette

;; iterate over values
(for ([v (hash-values H)]) (writeln 'value-> v))
value->     42
value->     666
value->     33

```



## Elena

ELENA 4.x :

```elena
import system'collections;
import system'routines;
import extensions;

public program()
{
    // 1. Create
    var map := new Dictionary();
    map["key"] := "foox";
    map["key"] := "foo";
    map["key2"]:= "foo2";
    map["key3"]:= "foo3";
    map["key4"]:= "foo4";

    // Enumerate
    map.forEach:
        (keyValue){ console.printLine(keyValue.Key," : ",keyValue.Value) }
}
```



###  Strong typed dictionary


```elena
import system'collections;
import system'routines;
import extensions;

public program()
{
    // 1. Create
    auto map := new Map<string,string>();
    map["key"] := "foox";
    map["key"] := "foo";
    map["key2"]:= "foo2";
    map["key3"]:= "foo3";
    map["key4"]:= "foo4";

    // Enumerate
    map.forEach:
        (tuple){ console.printLine(tuple.Item1," : ",tuple.Item2) }
}
```



## Elixir


```elixir
IO.inspect d = Map.new([foo: 1, bar: 2, baz: 3])
Enum.each(d, fn kv -> IO.inspect kv end)
Enum.each(d, fn {k,v} -> IO.puts "#{inspect k} => #{v}" end)
Enum.each(Map.keys(d), fn key -> IO.inspect key end)
Enum.each(Map.values(d), fn value -> IO.inspect value end)
```


{{out}}

```txt

%{bar: 2, baz: 3, foo: 1}
{:bar, 2}
{:baz, 3}
{:foo, 1}
:bar => 2
:baz => 3
:foo => 1
:bar
:baz
:foo
2
3
1

```



## Erlang


```erlang

-module(assoc).
-compile([export_all]).

test_create() ->
    D = dict:new(),
    D1 = dict:store(foo,1,D),
    D2 = dict:store(bar,2,D1),
    print_vals(D2).

print_vals(D) ->
    lists:foreach(fun (K) ->
                          io:format("~p: ~b~n",[K,dict:fetch(K,D)])
                  end, dict:fetch_keys(D)).

```


{{out}}
 32> assoc:test_create().
 bar: 2
 foo: 1
 ok

=={{header|F_Sharp|F#}}==
Iterating over both.

```fsharp

let myMap = [ ("Hello", 1); ("World", 2); ("!", 3) ]

for k, v in myMap do
  printfn "%s -> %d" k v

```


Iterating over either keys or values only can be achieved through use of the _ wildcard token.

```fsharp

// Only prints the keys.
for k, _ in myMap do
    printfn "%s" k

// Only prints the values.
for _, v in myMap do
    printfn "%d" v

```



## Factor


```factor
H{ { "hi" "there" } { "a" "b" } } [ ": " glue print ] assoc-each
```

There's also <code>assoc-map</code>, <code>assoc-find</code>, <code>assoc-filter</code> and many more.


## Fantom


Given a map, <code>each</code> iterates over pairs of values-keys.  <code>keys</code> and <code>vals</code> retrieve a list of keys or values, respectively.


```fantom

class Main
{
  public static Void main ()
  {
    Int:Str map := [1:"alpha", 2:"beta", 3:"gamma"]

    map.keys.each |Int key|
    {
      echo ("Key is: $key")
    }

    map.vals.each |Str value|
    {
      echo ("Value is: $value")
    }

    map.each |Str value, Int key|
    {
      echo ("Key $key maps to $value")
    }
  }
}

```



## Forth

{{libheader|Forth Foundation Library}}


```forth
include ffl/hct.fs
include ffl/hci.fs

\ Create hashtable and iterator in dictionary
10     hct-create htable
htable hci-create hiter

\ Insert entries
1 s" hello" htable hct-insert
2 s" world" htable hct-insert
3 s" !"     htable hct-insert

: iterate
  hiter hci-first
  BEGIN
  WHILE
    ." key = " hiter hci-key type ." , value = " . cr
    hiter hci-next
  REPEAT
;

iterate
```



```forth

\ Written in ANS-Forth; tested under VFX.
\ Requires the novice package: http://www.forth.org/novice.html
\ The following should already be done:
\ include novice.4th
\ include association.4th

\ I would define high-level languages as those that allow programs to be written without explicit iteration. Iteration is a major source of bugs.
\ The example from the FFL library doesn't hide iteration, whereas this example from the novice-package does.


marker AssociationIteration.4th

\ ******
\ ****** The following defines a node in an association (each node is derived from ELEMENT).
\ ******

element
    w field .inventor
constant language           \ describes a programming language

: init-language ( inventor name node -- node )
    init-element >r
    hstr r@ .inventor !
    r> ;

: new-language ( inventor name -- node )
    language alloc
    init-language ;

: show-language ( count node -- )
    >r
    1+                      \ -- count+1
    cr  r@ .key @ count colorless type  ." invented by: "  r@ .inventor @ count type
    rdrop ;

: show-languages-forward ( handle -- )
    0                       \ -- handle count
    swap .root @  ['] show-language  walk>
    cr ." count: " .
    cr ;

: show-languages-backward ( handle -- )
    0                       \ -- handle count
    swap .root @  ['] show-language  <walk
    cr ." count: " .
    cr ;

: kill-language-attachments ( node -- )
    dup .inventor @  dealloc
    kill-key ;

: copy-language-attachments ( src dst -- )
    over .inventor @  hstr
    over .inventor !
    copy-key ;


\ ******
\ ****** The following defines the association itself (the handle).
\ ******

association
constant languages          \ describes a set of programming languages

: init-languages ( record -- record )
    >r
    ['] compare  ['] kill-language-attachments  ['] copy-language-attachments
    r> init-association ;

: new-languages ( -- record )
    languages alloc
    init-languages ;


\ ******
\ ****** The following filters one association into another, including everything that matches a particular inventor.
\ ******

: <filter-inventor> { inventor handle new-handle node -- inventor handle new-handle }
    inventor count  node .inventor @ count  compare  A=B = if
        node handle dup-element  new-handle insert  then
    inventor handle new-handle ;

: filter-inventor ( inventor handle -- new-handle )
    dup similar-association                             \ -- inventor handle new-handle
    over .root @  ['] <filter-inventor>  walk>          \ -- inventor handle new-handle
    nip nip ;

\ ******
\ ****** The following is a demonstration with some sample data.
\ ******


new-languages
    c" Moore, Chuck"                c" Forth     "      new-language  over insert
    c" Ichiah, Jean"                c" Ada       "      new-language  over insert
    c" Wirth, Niklaus"              c" Pascal    "      new-language  over insert
    c" Wirth, Niklaus"              c" Oberon    "      new-language  over insert
    c" McCarthy, John"              c" Lisp      "      new-language  over insert
    c" van Rossum, Guido"           c" Python    "      new-language  over insert
    c" Gosling, Jim"                c" Java      "      new-language  over insert
    c" Ierusalimschy, Roberto"      c" Lua       "      new-language  over insert
    c" Matsumoto, Yukihiro"         c" Ruby      "      new-language  over insert
    c" Pestov, Slava"               c" Factor    "      new-language  over insert
    c" Gosling, James"              c" Java      "      new-language  over insert
    c" Wirth, Niklaus"              c" Modula-2  "      new-language  over insert
    c" Ritchie, Dennis"             c" C         "      new-language  over insert
    c" Stroustrup, Bjarne"          c" C++       "      new-language  over insert
constant some-languages


cr .( everything in SOME-LANGUAGES ordered forward: )

some-languages show-languages-forward


cr .( everything in SOME-LANGUAGES ordered backward: )

some-languages show-languages-backward


cr .( everything in SOME-LANGUAGES invented by Wirth: )

c" Wirth, Niklaus" some-languages filter-inventor           dup show-languages-forward  kill-association


cr .( everything in SOME-LANGUAGES within 'F' and 'L': )

c" F"  c" L"  some-languages  filter within                 dup show-languages-forward  kill-association


cr .( everything in SOME-LANGUAGES not within 'F' and 'L': )

c" F"  c" L"  some-languages  filter without                dup show-languages-forward  kill-association


some-languages kill-association

```

{{out}}
<pre style="height:30ex;overflow:scroll">
everything in SOME-LANGUAGES ordered forward:
Ada       invented by: Ichiah, Jean
C         invented by: Ritchie, Dennis
C++       invented by: Stroustrup, Bjarne
Factor    invented by: Pestov, Slava
Forth     invented by: Moore, Chuck
Java      invented by: Gosling, James
Lisp      invented by: McCarthy, John
Lua       invented by: Ierusalimschy, Roberto
Modula-2  invented by: Wirth, Niklaus
Oberon    invented by: Wirth, Niklaus
Pascal    invented by: Wirth, Niklaus
Python    invented by: van Rossum, Guido
Ruby      invented by: Matsumoto, Yukihiro
count: 13

everything in SOME-LANGUAGES ordered backward:
Ruby      invented by: Matsumoto, Yukihiro
Python    invented by: van Rossum, Guido
Pascal    invented by: Wirth, Niklaus
Oberon    invented by: Wirth, Niklaus
Modula-2  invented by: Wirth, Niklaus
Lua       invented by: Ierusalimschy, Roberto
Lisp      invented by: McCarthy, John
Java      invented by: Gosling, James
Forth     invented by: Moore, Chuck
Factor    invented by: Pestov, Slava
C++       invented by: Stroustrup, Bjarne
C         invented by: Ritchie, Dennis
Ada       invented by: Ichiah, Jean
count: 13

everything in SOME-LANGUAGES invented by Wirth:
Modula-2  invented by: Wirth, Niklaus
Oberon    invented by: Wirth, Niklaus
Pascal    invented by: Wirth, Niklaus
count: 3

everything in SOME-LANGUAGES within 'F' and 'L':
Factor    invented by: Pestov, Slava
Forth     invented by: Moore, Chuck
Java      invented by: Gosling, James
count: 3

everything in SOME-LANGUAGES not within 'F' and 'L':
Ada       invented by: Ichiah, Jean
C         invented by: Ritchie, Dennis
C++       invented by: Stroustrup, Bjarne
Lisp      invented by: McCarthy, John
Lua       invented by: Ierusalimschy, Roberto
Modula-2  invented by: Wirth, Niklaus
Oberon    invented by: Wirth, Niklaus
Pascal    invented by: Wirth, Niklaus
Python    invented by: van Rossum, Guido
Ruby      invented by: Matsumoto, Yukihiro
count: 10

```



## Free Pascal

FPC 3.2.0+. Similar to Delphi:
```pascal
program AssociativeArrayIteration;
{$mode delphi}{$ifdef windows}{$apptype console}{$endif}
uses Generics.Collections;

type
  TlDictionary =  TDictionary<string, Integer>;
  TlPair = TPair<string,integer>;

var
  i: Integer;
  s: string;
  lDictionary: TlDictionary;
  lPair: TlPair;
begin
  lDictionary := TlDictionary.Create;
  try
    lDictionary.Add('foo', 5);
    lDictionary.Add('bar', 10);
    lDictionary.Add('baz', 15);
    lDictionary.AddOrSetValue('foo',6);
    for lPair in lDictionary do
      Writeln('Pair: ',Lpair.Key,' = ',lPair.Value);
    for s in lDictionary.Keys do
      Writeln('Key: ' + s);
    for i in lDictionary.Values do
      Writeln('Value: ', i);
  finally
    lDictionary.Free;
  end;
end.
```


```txt

Pair: foo = 6
Pair: bar = 10
Pair: baz = 15
Key: foo
Key: bar
Key: baz
Value: 6
Value: 10
Value: 15
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=e48bd6ed7e6b583106b8178bca536eea Click this link to run this code]'''

```gambas
Public Sub Main()
Dim cList As Collection = ["2": "quick", "4": "fox", "1": "The", "9": "dog", "7": "the", "5": "jumped", "3": "brown", "6": "over", "8": "lazy"]
Dim siCount As Short
Dim sTemp As String

For Each sTemp In cList
  Print cList.key & "=" & sTemp;;
Next

Print

For siCount = 1 To cList.Count
  Print cList[Str(siCount)];;
Next

End
```

Output:

```txt

2=quick 4=fox 1=The 9=dog 7=the 5=jumped 3=brown 6=over 8=lazy
The quick brown fox jumped over the lazy dog

```



## Go

'''Language:'''

```go
myMap := map[string]int {
	   "hello": 13,
	   "world": 31,
	   "!"    : 71 }

// iterating over key-value pairs:
for key, value := range myMap {
    fmt.Printf("key = %s, value = %d\n", key, value)
}

// iterating over keys:
for key := range myMap {
    fmt.Printf("key = %s\n", key)
}

// iterating over values:
for _, value := range myMap {
    fmt.Printf("value = %d\n", value)
}
```

'''Standard library templates:'''

In addition to the for/range features of the language, the text/template and html/template packages of the standard library have map iteration features.  Some differences worth noting:
* A single assigned value in a template is the map value.  With the language for/range it is the key.
* Templates have no equivalent of _; a dummy variable must be used.
* In a template, if map keys are a comparable basic type, then iteration proceeds in key order.  With the language for/range, iteration is in non-deterministic order.


```go
package main

import (
    "os"
    "text/template"
)

func main() {
    m := map[string]int{
        "hello": 13,
        "world": 31,
        "!":     71,
    }

    // iterating over key-value pairs:
    template.Must(template.New("").Parse(`
{{- range $k, $v := . -}}
key = {{$k}}, value = {{$v}}
{{end -}}
`)).Execute(os.Stdout, m)

    // iterating over keys:
    template.Must(template.New("").Parse(`
{{- range $k, $v := . -}}
key = {{$k}}
{{end -}}
`)).Execute(os.Stdout, m)

    // iterating over values:
    template.Must(template.New("").Parse(`
{{- range . -}}
value = {{.}}
{{end -}}
`)).Execute(os.Stdout, m)
}
```

{{out}}
Note order by key.

```txt

key = !, value = 71
key = hello, value = 13
key = world, value = 31
key = !
key = hello
key = world
value = 71
value = 13
value = 31

```



## Groovy

Solution:

```groovy
def map = [lastName: "Anderson", firstName: "Thomas", nickname: "Neo", age: 24, address: "everywhere"]

println "Entries:"
map.each { println it }

println()
println "Keys:"
map.keySet().each { println it }

println()
println "Values:"
map.values().each { println it }
```


{{out}}

```txt
Entries:
lastName=Anderson
firstName=Thomas
nickname=Neo
age=24
address=everywhere

Keys:
lastName
firstName
nickname
age
address

Values:
Anderson
Thomas
Neo
24
everywhere
```




## Harbour


```visualfoxpro
LOCAL arr := { 6 => 16, "eight" => 8, "eleven" => 11 }
LOCAL x

FOR EACH x IN arr
   // key, value
   ? x:__enumKey(), x
   // or key only
   ? x:__enumKey()
   // or value only
   ? x
NEXT
```



## Haskell

with Data.Map:

```haskell
import qualified Data.Map as M

myMap :: M.Map String Int
myMap = M.fromList [("hello", 13), ("world", 31), ("!", 71)]

main :: IO ()
main =
  (putStrLn . unlines) $
  [ show . M.toList     -- Pairs
  , show . M.keys       -- Keys
  , show . M.elems      -- Values
  ] <*>
  pure myMap
```

{{Out}}

```txt
[("!",71),("hello",13),("world",31)]
["!","hello","world"]
[71,13,31]
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure main()
    t := table()
    every t[a := !"ABCDE"] := map(a)

    every pair := !sort(t) do
        write("\t",pair[1]," -> ",pair[2])

    writes("Keys:")
    every writes(" ",key(t))
    write()

    writes("Values:")
    every writes(" ",!t)
    write()
end
```


{{out}}

```txt
->aai
        A -> a
        B -> b
        C -> c
        D -> d
        E -> e
Keys: C E B D A
Values: c e b d a
```



## Io


```Io
myDict := Map with(
    "hello", 13,
    "world", 31,
    "!"    , 71
)

// iterating over key-value pairs:
myDict foreach( key, value,
    writeln("key = ", key, ", value = ", value)
)

// iterating over keys:
myDict keys foreach( key,
    writeln("key = ", key)
)

// iterating over values:
myDict foreach( value,
    writeln("value = ", value)
)
// or alternatively:
myDict values foreach( value,
    writeln("value = ", value)
)
```



## J


Note that all J operations either iterate over the items of an array or can be made to do so.  So to iterate over some sequence you need to refer to that sequence.

Using the J example from [[Creating an Associative Array]]...

Keys
```J
nl__example 0
```


Values
```J
get__example each nl__example 0
```


Both keys and values
```J
(,&< get__example) each nl__example 0
```


Note that this last is not likely to be useful in any practical context outside of learning the language.


## Java


```java
Map<String, Integer> map = new HashMap<String, Integer>();
map.put("hello", 1);
map.put("world", 2);
map.put("!", 3);

// iterating over key-value pairs:
for (Map.Entry<String, Integer> e : map.entrySet()) {
    String key = e.getKey();
    Integer value = e.getValue();
    System.out.println("key = " + key + ", value = " + value);
}

// iterating over keys:
for (String key : map.keySet()) {
    System.out.println("key = " + key);
}

// iterating over values:
for (Integer value : map.values()) {
    System.out.println("value = " + value);
}
```


Java 8 version


```java
Map<String, Integer> map = new HashMap<>();
map.put("hello", 1);
map.put("world", 2);
map.put("!", 3);

// iterating over key-value pairs:
map.forEach((k, v) -> {
    System.out.printf("key = %s, value = %s%n", k, v);
});

// iterating over keys:
map.keySet().forEach(k -> System.out.printf("key = %s%n", k));

// iterating over values:
map.values().forEach(v -> System.out.printf("value = %s%n", v));
```


{{out}}

```txt
key = !, value = 3
key = world, value = 2
key = hello, value = 1
key = !
key = world
key = hello
value = 3
value = 2
value = 1
```



## JavaScript

JavaScript does not have associative arrays until ECMAScript 6 brings Maps. In versions up to ES5.1, you may add properties to an empty object to achieve the same effect.

```javascript
var myhash = {}; //a new, empty object
myhash["hello"] = 3;
myhash.world = 6; //obj.name is equivalent to obj["name"] for certain values of name
myhash["!"] = 9;

//iterate using for..in loop
for (var key in myhash) {
  //ensure key is in object and not in prototype
  if (myhash.hasOwnProperty(key)) {
    console.log("Key is: " + key + '. Value is: ' + myhash[key]);
  }
}

//iterate using ES5.1 Object.keys() and Array.prototype.Map()
var keys = Object.keys(); //get Array of object keys (doesn't get prototype keys)
keys.map(function (key) {
  console.log("Key is: " + key + '. Value is: ' + myhash[key]);
});
```



## Jq

In jq, there are several ways to iterate over compound structures:
 - functionally, e.g. using map on an array
 - by enumeration, i.e. by generating a stream
 - by performing a reduction

For the sake of brevity, therefore, in the following we will only illustrate the enumerative approach.

With respect to associative arrays (i.e. JSON objects), the fundamental functions are:
 - keys -- for producing an array of the keys (sorted)
 - .[]  -- for producing a stream of the values

In jq > 1.4, keys_unsorted, for producing an array of the keys (in the order of creation), is also available.

```jq
def mydict: {"hello":13, "world": 31, "!": 71};

# Iterating over the keys
mydict | keys[]
# "!"
# "hello"
# "world"

# Iterating over the values:
mydict[]
# 13
# 31
# 71

# Generating a stream of {"key": key, "value": value} objects:
mydict | to_entries[]
# {"key":"hello","value":13}
# {"key":"world","value":31}
# {"key":"!","value":71}

# Generating a stream of [key,value] arrays:
mydict | . as $o | keys[] | [., $o[.]]
#["!",71]
#["hello",13]
#["world",31]

# Generating a stream of [key,value] arrays, without sorting (jq > 1.4 required)
mydict | . as $o | keys_unsorted[] | [., $o[.]]
# ["hello",13]
# ["world",31]
# ["!",71]

```



## Julia

{{works with|Julia|0.6}}

```julia
dict = Dict("hello" => 13, "world" => 31, "!" => 71)

# applying a function to key-value pairs:
foreach(println, dict)

# iterating over key-value pairs:
for (key, value) in dict
    println("dict[$key] = $value")
end

# iterating over keys:
for key in keys(dict)
    @show key
end

# iterating over values:
for value in values(dict)
    @show value
end

```


{{out}}

```txt

key = !, value = 71
key = hello, value = 13
key = world, value = 31
key = !
key = hello
key = world
value = 71
value = 13
value = 31

```



## K

Creating a dictionary.

```K
   d: .((`"hello";1); (`"world";2);(`"!";3))
```


The keys are available via "!".

```K
   !d
`hello `world `"!"

   $!d  / convert keys (symbols) as strings
("hello"
 "world"
 ,"!")
```


Print the key value pairs.

```K
   `0:{,/$x,": ",d[x]}'!d
hello: 1
world: 2
!: 3
```


The values are available via "[]".

```K
   d[]
1 2 3

  {x+1}'d[]
2 3 4
```



## Kotlin


```scala
fun main(a: Array<String>) {
    val map = mapOf("hello" to 1, "world" to 2, "!" to 3)

    with(map) {
        entries.forEach { println("key = ${it.key}, value = ${it.value}") }
        keys.forEach { println("key = $it") }
        values.forEach { println("value = $it") }
    }
}
```

{{Out}}

```txt
key = hello, value = 1
key = world, value = 2
key = !, value = 3
key = hello
key = world
key = !
value = 1
value = 2
value = 3
```



## Lang5


```lang5
: first  0 extract nip ; : second  1 extract nip ; : nip  swap drop ;
: say(*)  dup first " => " 2 compress "" join . second . ;

[['foo 5] ['bar 10] ['baz 20]] 'say apply drop
```



## Lasso


```Lasso

//iterate over associative array
//Lasso maps
	local('aMap' = map('weight' = 112,
					'height' = 45,
					'name' = 'jason'))
	' Map output: \n  '
	#aMap->forEachPair => {^
		//display pair, then show accessing key and value individually
		#1+'\n  '
		#1->first+': '+#1->second+'\n  '
	^}
	//display keys and values separately
	'\n'
	' Map Keys: '+#aMap->keys->join(',')+'\n'
	' Map values: '+#aMap->values->join(',')+'\n'

	//display using forEach
	'\n'
	' Use ForEach to iterate Map keys: \n'
	#aMap->keys->forEach => {^
		#1+'\n'
	^}
	'\n'
	' Use ForEach to iterate Map values: \n'
	#aMap->values->forEach => {^
		#1+'\n'
	^}
	//the {^ ^} indicates that output should be printed (AutoCollect) ,
	// if output is not desired, just { } is used

```



## LFE



### Keys and Values


```lisp

(let ((data '(#(key1 "foo") #(key2 "bar")))
      (hash (: dict from_list data)))
  (: dict fold
    (lambda (key val accum)
      (: io format '"~s: ~s~n" (list key val)))
    0
    hash))

```



### Just Keys


```lisp

(let ((data '(#(key1 "foo") #(key2 "bar")))
      (hash (: dict from_list data)))
  (: lists map
    (lambda (key)
      (: io format '"~s~n" (list key)))
    (: dict fetch_keys hash)))

```



## Liberty BASIC

Needs the sublist library from http://basic.wikispaces.com/SubList+Library since LB does not have built-in associative arrays.

```lb

data "red",      "255 50 50",       "green", "50 255 50",     "blue", "50 50 255"
data "my fave",  "220 120 120",     "black", "0 0 0"

myAssocList$ =""

for i =1 to 5
    read k$
    read dat$
    call sl.Set myAssocList$, k$, dat$
next i

keys$ = ""   ' List to hold the keys in myList$.
keys  = 0

keys = sl.Keys( myAssocList$, keys$)
print " Number of key-data pairs ="; keys

For i = 1 To keys
    keyName$ = sl.Get$( keys$, Str$( i))
    Print "  Key "; i; ":", keyName$, "Data: ", sl.Get$( myAssocList$, keyName$)
Next i

end

```

  Number of key-data pairs =5
  Key 1:      red           Data:         255 50 50
  Key 2:      green         Data:         50 255 50
  Key 3:      blue          Data:         50 50 255
  Key 4:      my fave       Data:         220 120 120
  Key 5:      black         Data:         0 0 0


## Lingo


```lingo
hash = [#key1:"value1", #key2:"value2", #key3:"value3"]

-- iterate over key-value pairs
repeat with i = 1 to hash.count
  put hash.getPropAt(i) & "=" & hash[i]
end repeat

-- iterating over values only can be written shorter
repeat with val in hash
  put val
end repeat
```



## LiveCode


```LiveCode
put 3 into fruit["apples"]
put 5 into fruit["pears"]
put 6 into fruit["oranges"]
put "none" into fruit["bananas"]

put "Keys:" & cr & the keys of fruit & cr into tTmp
put "Values 1:" & tab after tTmp
repeat for each line tKey in the keys of fruit
    put fruit[tkey] & comma after tTmp
end repeat

-- need to copy array as combine will change variable
put fruit into fruit2
combine fruit2 using comma
put cr & "Values2:" & tab after tTmp
repeat for each item f2val in fruit2
    put f2val & comma after tTmp
end repeat

combine fruit using return and ":"
put cr & "Key:Values" & cr & fruit after tTmp
-- alternatively, use same loop as for values 1 with tkey && fruit[tKey]

put tTmp
```

Output

```LiveCode
Keys:
apples
pears
oranges
bananas
Values 1:	3,5,6,none,
Values2:	3,none,6,5,
Key:Values
apples:3
bananas:none
oranges:6
pears:5
```



## Lua


```lua
local t = {
    ["foo"] = "bar",
    ["baz"] = 6,
    fortytwo = 7
}

for key,val in pairs(t) do
    print(string.format("%s: %s", key, val))
end
```


{{out}}

```txt

    fortytwo: 7
    foo: bar
    baz: 6

```

''Note:'' the order in which <code>pairs</code> iterates over non-integer keys is not defined, so the order of lines in the output of the above code may differ from one run to another.


## M2000 Interpreter


```M2000 Interpreter

Module checkit {
      \\ Inventories are objects with keys and values, or keys (used as read only values)
      \\ They use hash function.
      \\ Function TwoKeys return Inventory object (as a pointer to object)
      Function TwoKeys {
            Inventory Alfa="key1":=100, "key2":=200
            =Alfa
      }
      M=TwoKeys()
      Print Type$(M)="Inventory"
      \\ Normal Use:
            \\ Inventories Keys are case sensitive
            \\ M2000 identifiers are not case sensitive
      Print M("key1"), m("key2")
      \\ numeric values can convert to strings
      Print M$("key1"), m$("key2")
      \\ Iteration
      N=Each(M)
      While N {
            Print Eval(N)  ' prints 100, 200 as number
            Print M(N^!)  ' The same using index N^
      }
      N=Each(M)
      While N {
            Print Eval$(N)  ' prints  100, 200 as strings
            Print M$(N^!)  ' The same using index N^
      }
      N=Each(M)
      While N {
            Print Eval$(N, N^)  ' Prints Keys
      }
      \\ double iteration
      Append M, "key3":=500
      N=Each(M, 1, -1)  ' start to end
      N1=Each(M, -1, 1) ' end to start
      \\ 3x3 prints
      While N {
            While N1 {
                  Print format$("{0}*{1}={2}", Eval(N1), Eval(N), Eval(N1)*Eval(N))
            }
      }
      \\ sort results from lower product to greater product (3+2+1, 6 prints only)
      N=Each(M, 1, -1)
      While N {
            N1=Each(M, N^+1, -1)
            While N1 {
                  Print format$("{0}*{1}={2}", Eval(N1), Eval(N), Eval(N1)*Eval(N))
            }
      }
      N=Each(M)
      N1=Each(M,-2, 1)  ' from second from end to start
      \\ print only 2 values. While block ends when one iterator finish
      While N, N1 {
            Print Eval(N1)*Eval(N)
      }
}
Checkit

```



## M4


```M4
divert(-1)
define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2<=$3),1,
   `pushdef(`$1',$2)$4`'popdef(`$1')$0(`$1',incr($2),$3,`$4')')')')
define(`new',`define(`$1[size]key',0)')
define(`asize',`defn(`$1[size]key')')
define(`aget',`defn(`$1[$2]')')
define(`akget',`defn(`$1[$2]key')')
define(`avget',`aget($1,akget($1,$2))')
define(`aset',
   `ifdef($1[$2],
      `',
      `define(`$1[size]key',incr(asize(`$1')))`'define($1[asize(`$1')]key,$2)')`'define($1[$2],$3)')
define(`dquote', ``$@'')
define(`akeyvalue',`dquote(akget($1,$2),aget($1,akget($1,$2)))')
define(`akey',`dquote(akget($1,$2))')
define(`avalue',`dquote(aget($1,akget($1,$2)))')
divert
new(`a')
aset(`a',`wow',5)
aset(`a',`wow',flame)
aset(`a',`bow',7)
key-value pairs
for(`x',1,asize(`a'),
   `akeyvalue(`a',x)
')
keys
for(`x',1,asize(`a'),
   `akey(`a',x)
')
values
for(`x',1,asize(`a'),
   `avalue(`a',x)
')
```


{{out}}

```txt

key-value pairs
`wow',`flame'
`bow',`7'

keys
`wow'
`bow'

values
`flame'
`7'

```



## Maple

Iterate through indices when indices are all simple expressions:

```Maple

> T := table( [ "A" = 1, "B" = 2, "C" = 3, "D" = 4 ] );
> for i in indices( T, nolist ) do print(i ) end:
                                  "A"

                                  "B"

                                  "C"

                                  "D"

```


Iterate through indices when indices may be expression sequences:

```Maple

> T := table( [ "a" = 1, "b" = 2, ("c","d") = 3 ] ):
> for i in indices( T ) do print( i, T[ op( i ) ] ) end:
                                ["a"], 1

                                ["b"], 2

                             ["c", "d"], 3

```


Return all index / entry pairs as equations:

```Maple

> for i in indices( T, pairs ) do print( i) end:
                                "a" = 1

                                "b" = 2

                             ("c", "d") = 3

```



```Maple

> for i in entries( T ) do print( i) end:
                                  [1]

                                  [3]

                                  [2]

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
keys=DownValues[#,Sort->False][[All,1,1,1]]&;
hashes=#/@keys[#]&;

a[2]="string";a["sometext"]=23;
keys[a]
->{2,sometext}
hashes[a]
->{string,23}
```


=={{header|MATLAB}} / {{header|Octave}}==

Associative arrays can be defined as structs in Matlab and Octave.


```Matlab
   keys = fieldnames(hash);
   for k=1:length(keys),
        key = keys{k};
	value = getfield(hash,key);        % get value of key
	hash = setfield(hash,key,-value);  % set value of key
   end;
```


or


```Matlab
   keys = fieldnames(hash);
   for k=1:length(keys),
        key = keys{k};
        value = hash.(key);     % get value of key
        hash.(key) = -value;    % set value of key
   end;
```



## Maxima


```Maxima
h[1]: 6$
h[9]: 2$

/* iterate over values */
for val in listarray(h) do (
  print(val))$

/* iterate over the keys */
for key in rest(arrayinfo(h), 2) do (
  val: arrayapply(h, key),
  print(key, val))$
```



## MiniScript


```MiniScript
d = { 3: "test", "foo": 3 }

for keyVal in d
    print keyVal   // produces results like: { "key": 3, "value": "test" }
end for

for key in d.indexes
    print key
end for

for  val in d.values
    print val
end for
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols

surname = 'Unknown' -- default value
surname['Fred'] = 'Bloggs'
surname['Davy'] = 'Jones'

try = 'Fred'
say surname[try] surname['Bert']

-- extract the keys
loop fn over surname
  say fn.right(10) ':' surname[fn]
  end fn
```



## NewLISP


```NewLISP
;; using an association list:
(setq alist '(("A" "a") ("B" "b") ("C" "c")))

;; list keys
(map first alist)

;; list values
(map last alist)

;; loop over the assocation list:
(dolist (elem alist)
  (println (format "%s -> %s" (first elem) (last elem))))
```



## Nim


```nim

import tables

var t: Table[int,string] = initTable[int,string]()

t[1] = "one"
t[2] = "two"
t[3] = "three"
t.add(4,"four")

echo "t has " & $t.len & " elements"

echo "has t key 4? " & $t.hasKey(4)
echo "has t key 5? " & $t.hasKey(5)

#iterate keys
echo "key iteration:"
for k in t.keys:
  echo "at[" & $k & "]=" & t[k]

#itetate pairs
echo "pair iteration:"
for k,v in t.pairs:
  echo "at[" & $k & "]=" & v

```

{{out}}

```txt

t has 4 elements
has t key 4? true
has t key 5? false
key iteration:
at[1]=one
at[2]=two
at[3]=three
at[4]=four
pair iteration:
at[1]=one
at[2]=two
at[3]=three
at[4]=four

```


=={{header|Oberon-2}}==
{{works with|oo2c Version 2}}

```oberon2

MODULE AssociativeArray;
IMPORT
  ADT:Dictionary,
  Object:Boxed,
  Out;
TYPE
  Key = STRING;
  Value = Boxed.LongInt;

VAR
  assocArray: Dictionary.Dictionary(Key,Value);
  iterK: Dictionary.IterKeys(Key,Value);
  iterV: Dictionary.IterValues(Key,Value);
  aux: Value;
  k: Key;

BEGIN
  assocArray := NEW(Dictionary.Dictionary(Key,Value));
  assocArray.Set("ten",NEW(Value,10));
  assocArray.Set("eleven",NEW(Value,11));

  aux := assocArray.Get("ten");
  Out.LongInt(aux.value,0);Out.Ln;
  aux := assocArray.Get("eleven");
  Out.LongInt(aux.value,0);Out.Ln;Out.Ln;

  (* Iterate keys *)
  iterK := assocArray.IterKeys();
  WHILE (iterK.Next(k)) DO
    Out.Object(k);Out.Ln
  END;

  Out.Ln;

  (* Iterate values *)
  iterV := assocArray.IterValues();
  WHILE (iterV.Next(aux)) DO
    Out.LongInt(aux.value,0);Out.Ln
  END

END AssociativeArray.

```



## Objeck


```objeck

class Iteration {
  function : Main(args : String[]) ~ Nil {
    assoc_array := Collection.StringMap->New();
    assoc_array->Insert("Hello", IntHolder->New(1));
    assoc_array->Insert("World", IntHolder->New(2));
    assoc_array->Insert("!", IntHolder->New(3));

    keys := assoc_array->GetKeys();
    values := assoc_array->GetValues();

    each(i : keys) {
      key := keys->Get(i)->As(String);
      value := assoc_array->Find(key)->As(IntHolder)->Get();
      "key={$key}, value={$value}"->PrintLine();
    };

    "-------------"->PrintLine();

    each(i : keys) {
      key := keys->Get(i)->As(String);
      value := values->Get(i)->As(IntHolder)->Get();
      "key={$key}, value={$value}"->PrintLine();
    };
  }
}

```


=={{header|Objective-C}}==
{{works with|Objective-C|2.0+}}

```objc
NSDictionary *myDict = [NSDictionary dictionaryWithObjectsAndKeys:
                        [NSNumber numberWithInt:13], @"hello",
                        [NSNumber numberWithInt:31], @"world",
                        [NSNumber numberWithInt:71], @"!", nil];

// iterating over keys:
for (id key in myDict) {
    NSLog(@"key = %@", key);
}

// iterating over values:
for (id value in [myDict objectEnumerator]) {
    NSLog(@"value = %@", value);
}
```


{{works with|Objective-C|<2.0}}

```objc
NSDictionary *myDict = [NSDictionary dictionaryWithObjectsAndKeys:
                        [NSNumber numberWithInt:13], @"hello",
                        [NSNumber numberWithInt:31], @"world",
                        [NSNumber numberWithInt:71], @"!", nil];

// iterating over keys:
NSEnumerator *enm = [myDict keyEnumerator];
id key;
while ((key = [enm nextObject])) {
    NSLog(@"key = %@", key);
}

// iterating over values:
enm = [myDict objectEnumerator];
id value;
while ((value = [enm nextObject])) {
    NSLog(@"value = %@", value);
}
```


{{works with|Cocoa|Mac OS X 10.6+}}

```objc
NSDictionary *myDict = [NSDictionary dictionaryWithObjectsAndKeys:
                        [NSNumber numberWithInt:13], @"hello",
                        [NSNumber numberWithInt:31], @"world",
                        [NSNumber numberWithInt:71], @"!", nil];

// iterating over keys and values:
[myDict enumerateKeysAndObjectsUsingBlock: ^(id key, id value, BOOL *stop) {
    NSLog(@"key = %@, value = %@", key, value);
}];
```



## OCaml

Association array:

```ocaml
#!/usr/bin/env ocaml

let map = [| ('A', 1); ('B', 2); ('C', 3) |] ;;

(* iterate over pairs *)
Array.iter (fun (k,v) -> Printf.printf "key: %c - value: %d\n" k v) map ;;

(* iterate over keys *)
Array.iter (fun (k,_) -> Printf.printf "key: %c\n" k) map ;;

(* iterate over values *)
Array.iter (fun (_,v) -> Printf.printf "value: %d\n" v) map ;;

(* in functional programming it is often more useful to fold over the elements *)
Array.fold_left (fun acc (k,v) -> acc ^ Printf.sprintf "key: %c - value: %d\n" k v) "Elements:\n" map ;;
```


Hash table:

```ocaml
let map = Hashtbl.create 42;;
Hashtbl.add map 'A' 1;;
Hashtbl.add map 'B' 2;;
Hashtbl.add map 'C' 3;;

(* iterate over pairs *)
Hashtbl.iter (fun k v -> Printf.printf "key: %c - value: %d\n" k v) map ;;

(* in functional programming it is often more useful to fold over the elements *)
Hashtbl.fold (fun k v acc -> acc ^ Printf.sprintf "key: %c - value: %d\n" k v) map "Elements:\n" ;;
```


Functional binary search tree:

```ocaml
module CharMap = Map.Make (Char);;
let map = CharMap.empty;;
let map = CharMap.add 'A' 1 map;;
let map = CharMap.add 'B' 2 map;;
let map = CharMap.add 'C' 3 map;;

(* iterate over pairs *)
CharMap.iter (fun k v -> Printf.printf "key: %c - value: %d\n" k v) map ;;

(* in functional programming it is often more useful to fold over the elements *)
CharMap.fold (fun k v acc -> acc ^ Printf.sprintf "key: %c - value: %d\n" k v) map "Elements:\n" ;;
```



## Ol


```ol

;;; create sample associative array
(define aa (list->ff '(
   (hello . 1)
   (world . 2)
   (! . 3))))

(print aa)
; ==> #((! . 3) (hello . 1) (world . 2))

;;; simplest iteration over all associative array (using ff-iter, lazy iterator)
(let loop ((kv (ff-iter aa)))
   (cond
      ((null? kv) #true)
      ((pair? kv)
         (print (car kv))
         (loop (cdr kv)))
      (else (loop (force kv)))))
; ==> (! . 3)
; ==> (hello . 1)
; ==> (world . 2)

;;; iteration with returning value (using ff-fold)
(print
   "folding result: "
   (ff-fold
      (lambda (result key value)
         (print "key: " key ", value: " value)
         (+ result 1))
      0
      aa))

; ==> key: !, value: 3
; ==> key: hello, value: 1
; ==> key: world, value: 2
; ==> folding result: 3

;;; same but right fold (using ff-foldr)
(print
   "rfolding result: "
   (ff-foldr
      (lambda (result key value)
         (print "key: " key ", value: " value)
         (+ result 1))
      0
      aa))

; ==> key: world, value: 2
; ==> key: hello, value: 1
; ==> key: !, value: 3
; ==> rfolding result: 3

;;; at least create new array from existing (let's multiply every value by value)
(define bb (ff-map aa
      (lambda (key value)
         (* value value))))
(print bb)

; ==> #((! . 9) (hello . 1) (world . 4))


```



## ooRexx


```oorexx
d = .directory~new
d["hello"] = 1
d["world"] = 2
d["!"] = 3

-- iterating over keys:
loop key over d
    say "key =" key
end

-- iterating over values:
loop value over d~allitems
    say "value =" value
end

-- iterating over key-value pairs:
s = d~supplier
loop while s~available
    say "key =" s~index", value =" s~item
    s~next
end
```

{{out}}

```txt
key = !
key = world
key = hello
value = 3
value = 2
value = 1
key = !, value = 3
key = world, value = 2
key = hello, value = 1
```



## Oz


```oz
declare
  MyMap = unit('hello':13 'world':31 '!':71)
in
  {ForAll {Record.toListInd MyMap} Show}  %% pairs
  {ForAll {Record.arity     MyMap} Show}  %% keys
  {ForAll {Record.toList    MyMap} Show}  %% values
```



## PARI/GP

{{works with|PARI/GP|2.8.1+}}

The keys can be retried from a map with Vec:

```parigp
keys = Vec(M);
```

You can iterate over the values as usual:

```parigp
for(i=1,#keys,
  print(keys[i]," ",mapget(M,keys[i]))
)
```



## Perl


```perl
#! /usr/bin/perl
use strict;

my %pairs = ( "hello" => 13,
	      "world" => 31,
	      "!" => 71 );

# iterate over pairs

# Be careful when using each(), however, because it uses a global iterator
# associated with the hash. If you call keys() or values() on the hash in the
# middle of the loop, the each() iterator will be reset to the beginning. If
# you call each() on the hash somewhere in the middle of the loop, it will
# skip over elements for the "outer" each(). Only use each() if you are sure
# that the code inside the loop will not call keys(), values(), or each().
while ( my ($k, $v) = each %pairs) {
    print "(k,v) = ($k, $v)\n";
}

# iterate over keys
foreach my $key ( keys %pairs ) {
    print "key = $key, value = $pairs{$key}\n";
}
# or (see note about each() above)
while ( my $key = each %pairs) {
    print "key = $key, value = $pairs{$key}\n";
}

# iterate over values
foreach my $val ( values %pairs ) {
    print "value = $val\n";
}
```



## Perl 6

{{works with|Rakudo|2015.12}}


```perl6
my %pairs = hello => 13, world => 31, '!' => 71;

for %pairs.kv -> $k, $v {
    say "(k,v) = ($k, $v)";
}

# Stable order
for %pairs.sort(*.value)>>.kv -> ($k, $v) {
    say "(k,v) = ($k, $v)";
}

{ say "$^a => $^b" } for %pairs.kv;

say "key = $_" for %pairs.keys;

say "value = $_" for %pairs.values;
```



## Phix

The first three lines create a simple dictionary, with keys and values of several different types (string/integer/sequence):

```Phix
setd("one",1)
setd(2,"duo")
setd({3,4},{5,"six"})

function visitor(object key, object data, object /*userdata*/)
    ?{key,data}
    return 1    -- (continue traversal)
end function
traverse_dict(routine_id("visitor"))
```

{{out}}

```txt

{2,"duo"}
{{3,4},{5,"six"}}
{"one",1}

```

You could also use some of the map.e routines. With the same initial three setd() as above:

```Phix
include builtins\map.e
?pairs()
?keys()
?values()
```

{{out}}

```txt

{{2,"duo"},{{3,4},{5,"six"}},{"one",1}}
{2,{3,4},"one"}
{"duo",{5,"six"},1}

```



## PHP


```php
<?php
$pairs = array( "hello" => 1,
		"world" => 2,
		"!"     => 3 );

// iterate over key-value pairs
foreach($pairs as $k => $v) {
  echo "(k,v) = ($k, $v)\n";
}

// iterate over keys
foreach(array_keys($pairs) as $key) {
  echo "key = $key, value = $pairs[$key]\n";
}

// iterate over values
foreach($pairs as $value) {
  echo "values = $value\n";
}
?>
```



## PicoLisp


### Using properties


```PicoLisp
(put 'A 'foo 5)
(put 'A 'bar 10)
(put 'A 'baz 15)

: (getl 'A)                            # Get the whole property list
-> ((15 . baz) (10 . bar) (5 . foo))

: (mapcar cdr (getl 'A))               # Get all keys
-> (baz bar foo)

: (mapcar car (getl 'A))               # Get all values
-> (15 10 5)
```


### Using an index tree


```PicoLisp
(idx 'A (def "foo" 5) T)
(idx 'A (def "bar" 10) T)
(idx 'A (def "baz" 15) T)

: A                                    # Get the whole tree
-> ("foo" ("bar" NIL "baz"))

:  (idx 'A)                            # Get all keys
-> ("bar" "baz" "foo")

:  (mapcar val (idx 'A))               # Get all values
-> (10 15 5)
```



## Pike

note that the order is not alphabetic but depends on the hash value of the keys.
the order is deterministic however.


```Pike

mapping(string:string) m = ([ "A":"a", "B":"b", "C":"c" ]);
foreach(m; string key; string value)
{
    write(key+value);
}
Result: BbAaCc

// only keys
foreach(m; string key;)
{
    write(key);
}
Result: BAC

// only values
foreach(m;; string value)
{
    write(value);
}
Result: bac


```



## PostScript


```postscript

% over keys and values
<</a 1 /b 2 /c 3>> {= =} forall
% just keys
<</a 1 /b 2 /c 3>> {= } forall
% just values
<</a 1 /b 2 /c 3>> {pop =} forall

```



## Potion

We can traverse tables by key or by key and val. We cannot traverse tables only by val.

```potion
mydictionary = (red=0xff0000, green=0x00ff00, blue=0x0000ff)

mydictionary each (key, val): (key, ":", val, "\n") join print.
mydictionary each (key): (key, "\n") join print.
```



## PowerShell

Using the following hash table:

```powershell
$h = @{ 'a' = 1; 'b' = 2; 'c' = 3 }
```

Iterating over the key/value pairs is slightly cumbersome as it requires an explicit call to <code>GetEnumerator</code>:

```powershell
$h.GetEnumerator() | ForEach-Object { Write-Host Key: $_.Name, Value: $_.Value }
```

A <code>foreach</code> statement can also be used:

```powershell
foreach ($e in $h.GetEnumerator()) {
    Write-Host Key: $e.Name, Value: $e.Value
}
```

Iterating over the keys:

```powershell
$h.Keys | ForEach-Object { Write-Host Key: $_ }

foreach ($k in $h.Keys) {
    Write-Host Key: $k
}
```

Iterating over the values:

```powershell
$h.Values | ForEach-Object { Write-Host Value: $_ }

foreach ($v in $h.Values) {
    Write-Host Value: $v
}
```



## Prolog

Following the example at [[Associative_array/Creation#Prolog|Associative Array Creation]]
(with the understanding that using a predicate to store a hash does not prevent a "key"
from having more than one value):


```prolog

assert( mymap(key1,value1) ).
assert( mymap(key2,value1) ).

```


To perform the specific task at hand:

```prolog

?- forall( mymap(Key,Value), writeln( [Key,Value]) ).

[key1,value1]
[key2,value1]

```


In Prolog, however, iteration is "built-in".  For example:

```prolog

?- mymap(key1, Y).
Y = value1.

?- mymap(X, value1).
X = key1 ;
X = key2.

```


To construct the list of keys:

```prolog

?- findall( X, mymap(X,value1), Xs).
Xs = [key1, key2].

```


To construct the list of distinct values:

```prolog

?- findall( Y, mymap(key1,Y), Ys).
Ys = [value1].

```



## PureBasic

Hashes are a built-in type called Map in Purebasic.


```purebasic
NewMap dict.s()
dict("de") = "German"
dict("en") = "English"
dict("fr") = "French"

ForEach dict()
  Debug MapKey(dict()) + ":" + dict()
Next
```



## Python


```python
myDict = { "hello": 13,
	   "world": 31,
	   "!"    : 71 }

# iterating over key-value pairs:
for key, value in myDict.items():
    print ("key = %s, value = %s" % (key, value))

# iterating over keys:
for key in myDict:
    print ("key = %s" % key)
# (is a shortcut for:)
for key in myDict.keys():
    print ("key = %s" % key)

# iterating over values:
for value in myDict.values():
    print ("value = %s" % value)
```



## R


R lacks a native representation of key-value pairs, but different structures allow named elements, which provide similar functionality.


###  environment example



```r>
 env <- new.env()
> env[["x"]] <- 123
> env[["x"]]
```


```txt
[1] 123
```


```r>
 index <- "1"
> env[[index]] <- "rainfed hay"
> for (name in ls(env)) {
+   cat(sprintf('index=%s, value=%s\n', name, env[[name]]))
+ }
```


```txt
index=1, value=rainfed hay
index=x, value=123
```



###  vector example



```r>
 x <- c(hello=1, world=2, "!"=3)
> print(x["!"])
```


```txt
!
3
```


```r>
 print(unname(x["!"]))
```


```txt
[1] 3
```



###  list example



```R>
 a <- list(a=1, b=2, c=3.14, d="xyz")
> print(a$a)
```


```txt
[1] 1
```


```R>
 print(a$d)
```


```txt
[1] "xyz"
```



## Racket

Using the dictionary interface, different data structures can be treated as an associative array in Racket.


```racket

#lang racket

(define dict1 #hash((apple . 5) (orange . 10))) ; hash table
(define dict2 '((apple . 5) (orange . 10)))     ; a-list
(define dict3 (vector "a" "b" "c"))             ; vector (integer keys)

(dict-keys dict1)                   ; => '(orange apple)
(dict-values dict2)                 ; => '(5 10)
(for/list ([(k v) (in-dict dict3)]) ; => '("0 -> a" "1 -> b" "2 -> c")
  (format "~a -> ~a" k v))

```



## REXX


```rexx
/*REXX program demonstrates how to  set and display  values  for an  associative array. */
/*╔════════════════════════════════════════════════════════════════════════════════════╗
  ║ The (below) two REXX statements aren't really necessary,  but it shows how to      ║
  ║ define any and all entries in a associative array so that if a "key" is used that  ║
  ║ isn't defined, it can be displayed to indicate such,  or its value can be checked  ║
  ║ to determine if a particular associative array element has been set (defined).     ║
  ╚════════════════════════════════════════════════════════════════════════════════════╝*/
stateF.= ' [not defined yet] '                   /*sets any/all state  former  capitals.*/
stateN.= ' [not defined yet] '                   /*sets any/all state names.            */
w      = 0                                       /*the maximum  length  of a state name.*/
stateL =
/*╔════════════════════════════════════════════════════════════════════════════════════╗
  ║ The list of states (empty as of now).  It's convenient to have them in alphabetic  ║
  ║ order;  they'll be listed in the order as they are in the REXX program below).     ║
  ║ In REXX,  when a key is used  (for a stemmed array,  as they are called in REXX),  ║
  ║ and the key isn't assigned a value,  the key's  name  is stored (internally)  as   ║
  ║ uppercase  (Latin)  characters  (as in the examples below.   If the  key  has a    ║
  ║ a value, the key's value is used as is  (i.e.:  no upper translation is performed).║
  ║ Actually,  any characters can be used,  including blank(s)  and  non─displayable   ║
  ║ characters  (including   '00'x,   'ff'x,   commas,   periods,   quotes,   ···).    ║
  ╚════════════════════════════════════════════════════════════════════════════════════╝*/
call setSC 'al',  "Alabama"            ,  'Tuscaloosa'
call setSC 'ca',  "California"         ,  'Benicia'
call setSC 'co',  "Colorado"           ,  'Denver City'
call setSC 'ct',  "Connecticut"        ,  'Hartford and New Haven  (jointly)'
call setSC 'de',  "Delaware"           ,  'New-Castle'
call setSC 'ga',  "Georgia"            ,  'Milledgeville'
call setSC 'il',  "Illinois"           ,  'Vandalia'
call setSC 'in',  "Indiana"            ,  'Corydon'
call setSC 'ia',  "Iowa"               ,  'Iowa City'
call setSC 'la',  "Louisiana"          ,  'New Orleans'
call setSC 'me',  "Maine"              ,  'Portland'
call setSC 'mi',  "Michigan"           ,  'Detroit'
call setSC 'ms',  "Mississippi"        ,  'Natchez'
call setSC 'mo',  "Missouri"           ,  'Saint Charles'
call setSC 'mt',  "Montana"            ,  'Virginia City'
call setSC 'ne',  "Nebraska"           ,  'Lancaster'
call setSC 'nh',  "New Hampshire"      ,  'Exeter'
call setSC 'ny',  "New York"           ,  'New York'
call setSC 'nc',  "North Carolina"     ,  'Fayetteville'
call setSC 'oh',  "Ohio"               ,  'Chillicothe'
call setSC 'ok',  "Oklahoma"           ,  'Guthrie'
call setSC 'pa',  "Pennsylvania"       ,  'Lancaster'
call setSC 'sc',  "South Carolina"     ,  'Charlestown'
call setSC 'tn',  "Tennessee"          ,  'Murfreesboro'
call setSC 'vt',  "Vermont"            ,  'Windsor'

       do j=1  for words(stateL)                 /*show all capitals that were defined. */
       $= word(stateL, j)                        /*get the next (USA) state in the list.*/
       say 'the former capital of  ('$") "    left(stateN.$, w)      " was "      stateC.$
       end    /*j*/                              /* [↑]   show states that were defined.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
setSC: parse arg code,name,cap;   upper code     /*get code, name & cap.; uppercase code*/
       stateL= stateL code                       /*keep a list of all the US state codes*/
       stateN.code= name; w= max(w,length(name)) /*define the state's name;  max width. */
       stateC.code= cap                          /*   "    "     "   code to the capital*/
       return                                    /*return to invoker, SETSC is finished.*/
```

{{out|output|text=  when using the internal default input:}}

```txt

the former capital of  (AL)  Alabama         was  Tuscaloosa
the former capital of  (CA)  California      was  Benicia
the former capital of  (CO)  Colorado        was  Denver City
the former capital of  (CT)  Connecticut     was  Hartford and New Haven  (jointly)
the former capital of  (DE)  Delaware        was  New-Castle
the former capital of  (GA)  Georgia         was  Milledgeville
the former capital of  (IL)  Illinois        was  Vandalia
the former capital of  (IN)  Indiana         was  Corydon
the former capital of  (IA)  Iowa            was  Iowa City
the former capital of  (LA)  Louisiana       was  New Orleans
the former capital of  (ME)  Maine           was  Portland
the former capital of  (MI)  Michigan        was  Detroit
the former capital of  (MS)  Mississippi     was  Natchez
the former capital of  (MO)  Missouri        was  Saint Charles
the former capital of  (MT)  Montana         was  Virginia City
the former capital of  (NE)  Nebraska        was  Lancaster
the former capital of  (NH)  New Hampshire   was  Exeter
the former capital of  (NY)  New York        was  New York
the former capital of  (NC)  North Carolina  was  Fayetteville
the former capital of  (OH)  Ohio            was  Chillicothe
the former capital of  (OK)  Oklahoma        was  Guthrie
the former capital of  (PA)  Pennsylvania    was  Lancaster
the former capital of  (SC)  South Carolina  was  Charlestown
the former capital of  (TN)  Tennessee       was  Murfreesboro
the former capital of  (VT)  Vermont         was  Windsor

```

When this example was started, the intention was to list the former capitals by key.   Unfortunately, there's a duplicate capital   (Lancaster).





## Ring


```ring

# Project : Associative array/Iteration

lst = [["hello", 13], ["world", 31], ["!", 71]]
for n = 1 to len(lst)
    see lst[n][1] + " : " + lst[n][2] + nl
next

```

Output:

```txt

hello : 13
world : 31
! : 71

```



## RLaB

Associative arrays are called ''lists'' in RLaB.


```RLaB

x = <<>>;  // create an empty list
x.hello = 1;
x.world = 2;
x.["!"] = 3;

// to iterate over identifiers of a list one needs to use the function ''members''
// the identifiers are returned as a lexicographically ordered string row-vector
// here    ["!", "hello", "world"]
for(i in members(x))
{ printf("%s %g\n", i,  x.[i]); }

// occasionally one needs to check if there exists member of a list
y = members(x);  // y contains ["!", "hello", "world"]
clear(x.["!"]);  // remove member with identifier "!" from the list "x"
for(i in y)
{ printf("%s %g\n", i,  x.[i]); }  // this produces error because x.["!"] does not exist

for(i in y)
{
  if (exist(x.[i]))
  { printf("%s %g\n", i,  x.[i]); }  // we print a member of the list "x" only if it exists
}



```



## Ruby


```ruby
my_dict = { "hello" => 13,
	   "world" => 31,
	   "!"     => 71 }

# iterating over key-value pairs:
my_dict.each {|key, value| puts "key = #{key}, value = #{value}"}
# or
my_dict.each_pair {|key, value| puts "key = #{key}, value = #{value}"}

# iterating over keys:
my_dict.each_key {|key| puts "key = #{key}"}

# iterating over values:
my_dict.each_value {|value| puts "value =#{value}"}
```


another way:

```ruby
for key, value in my_dict
  puts "key = #{key}, value = #{value}"
end

for key in my_dict.keys
  puts "key = #{key}"
end

for value in my_dict.values
  puts "value = #{value}"
end
```


{{out}}

```txt

key = hello, value = 13
key = world, value = 31
key = !, value = 71
key = hello
key = world
key = !
value = 13
value = 31
value = 71

```



## Rust


```rust
use std::collections::HashMap;
fn main() {
    let mut olympic_medals = HashMap::new();
    olympic_medals.insert("United States", (1072, 859, 749));
    olympic_medals.insert("Soviet Union", (473, 376, 355));
    olympic_medals.insert("Great Britain", (246, 276, 284));
    olympic_medals.insert("Germany", (252, 260, 270));
    for (country, medals) in olympic_medals {
        println!("{} has had {} gold medals, {} silver medals, and {} bronze medals",
               country, medals.0, medals.1, medals.2);

    }
}
```

{{out}}
Note that <code>HashMap</code> does not preserve order (if this is important, <code>std::collections::BTreeMap</code> is what you want.)

```txt

Germany has had 252 gold medals, 260 silver medals, and 270 bronze medals
United States has had 1072 gold medals, 859 silver medals, and 749 bronze medals
Soviet Union has had 473 gold medals, 376 silver medals, and 355 bronze medals
Great Britain has had 246 gold medals, 276 silver medals, and 284 bronze medals

```



## Scala


```Scala
val m = Map("Amsterdam" -> "Netherlands", "New York" -> "USA", "Heemstede" -> "Netherlands")

println(f"Key->Value: ${m.mkString(", ")}%s")
println(f"Pairs: ${m.toList.mkString(", ")}%s")
println(f"Keys: ${m.keys.mkString(", ")}%s")
println(f"Values: ${m.values.mkString(", ")}%s")
println(f"Unique values: ${m.values.toSet.mkString(", ")}%s")
```
{{out}}
```txt

Key->Value: Amsterdam -> Netherlands, New York -> USA, Heemstede -> Netherlands
Pairs: (Amsterdam,Netherlands), (New York,USA), (Heemstede,Netherlands)
Keys: Amsterdam, New York, Heemstede
Values: Netherlands, USA, Netherlands
Unique values: Netherlands, USA

```




## Scheme


{{works with|Gauche Scheme}}


```Scheme

;; Create an associative array (hash-table) whose keys are strings:
(define table (hash-table 'string=?
  '("hello" . 0) '("world" . 22) '("!" . 999)))

;; Iterate over the table, passing the key and the value of each entry
;; as arguments to a function:
(hash-table-for-each
  table
  ;; Create by "partial application" a function that accepts 2 arguments,
  ;; the key and the value:
  (pa$ format #t "Key = ~a, Value = ~a\n"))
```


Output:

```txt

Key = !, Value = 999
Key = world, Value = 22
Key = hello, Value = 0

```



```Scheme

;; Iterate over the table and create a list of the keys and the
;; altered values:
(hash-table-map
  table
  (lambda (key val) (list key (+ val 5000))))

;; Create a new table that has the same keys but altered values.
(use gauche.collection)
(map-to <hash-table>
  (lambda (k-v) (cons (car k-v) (+ (cdr k-v) 5000)))
  table)

```


To get a list of the keys or of the values of the table,
use one of the following:


```txt

(hash-table-keys table)
(hash-table-values table)

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const type: dictType is hash [string] integer;
var dictType: myDict is dictType.value;

const proc: main is func
  local
    var string: stri is "";
    var integer: number is 0;
  begin
    myDict @:= ["hello"] 1;
    myDict @:= ["world"] 2;
    myDict @:= ["!"] 3;

    # iterating over key-value pairs:
    for number key stri range myDict do
      writeln("key = " <& number <& ", value = " <& stri);
    end for;

    # iterating over keys:
    for key stri range myDict do
      writeln("key = " <& stri);
    end for;

    # iterating over values:
    for number range myDict do
      writeln("value = " <& number);
    end for;
  end func;
```


{{out}}

```txt

key = 3, value = !
key = 1, value = hello
key = 2, value = world
key = !
key = hello
key = world
value = 3
value = 1
value = 2

```



## Sidef


```ruby
var hash = Hash.new(
    key1 => 'value1',
    key2 => 'value2',
)

# Iterate over key-value pairs
hash.each { |key, value|
    say "#{key}: #{value}";
}

# Iterate only over keys
hash.keys.each { |key|
    say key;
}

# Iterate only over values
hash.values.each { |value|
    say value;
}
```

{{out}}

```txt
key1: value1
key2: value2
key1
key2
value1
value2

```



## Slate

In Slate, all associative mappings inherit from <tt>Mapping</tt>, so they all have the same protocol. Even <tt>Sequence</tt>s obey it, in addition to their own protocol for collections with ordered integer-range keys.

```slate
define: #pairs -> ({'hello' -> 1. 'world' -> 2. '!' -> 3. 'another!' -> 3} as: Dictionary).
pairs keysAndValuesDo: [| :key :value |
  inform: '(k, v) = (' ; key printString ; ', ' ; value printString ; ')'
].

pairs keysDo: [| :key |
  inform: '(k, v) = (' ; key printString ; ', ' ; (pairs at: key) printString ; ')'
].

pairs do: [| :value |
  inform: 'value = ' ; value printString
].
```



## Smalltalk

{{works with|GNU Smalltalk}}


```smalltalk
|pairs|
pairs := Dictionary
	    from: { 'hello' -> 1. 'world' -> 2. '!' -> 3. 'another!' -> 3 }.

"iterate over keys and values"
pairs keysAndValuesDo: [ :k :v |
    ('(k, v) = (%1, %2)' % { k. v }) displayNl
].

"iterate over keys"
pairs keysDo: [ :key |
    ('key = %1, value = %2' % { key. pairs at: key }) displayNl
].

"iterate over values"
pairs do: [ :value |
    ('value = %1' % { value }) displayNl
].
```


We could also obtain a set of keys or a collection of values and iterate over them with "<tt>do:</tt>":


```smalltalk
(pairs keys) do: [ :k | "..." ].
(pairs values) do: [ :v | "..." ].
```



## SNOBOL4


{{works with|Macro Spitbol}}
{{works with|Snobol4+}}
{{works with|CSnobol}}


```SNOBOL4
*       # Create sample table
        t = table()
        t<'cat'> = 'meow'
        t<'dog'> = 'woof'
        t<'pig'> = 'oink'

*       # Convert table to key/value array
        a = convert(t,'array')

*       # Iterate pairs
ploop   i = i + 1; output = a<i,1> ' -> ' a<i,2> :s(ploop)
*       # Iterate keys
kloop   j = j + 1; output = a<j,1> :s(kloop)
*       # Iterate vals
vloop   k = k + 1; output = a<k,2> :s(vloop)
end
```



## Stata


```stata
mata
// Create an associative array
a=asarray_create()
asarray(a,"one",1)
asarray(a,"two",2)

// Loop over entries
loc=asarray_first(a)
do {
	printf("%s %f\n",asarray_key(a,loc),asarray_contents(a,loc))
	loc=asarray_next(a,loc)
} while(loc!=NULL)
end
```



## Swift


```swift
let myMap = [
	   "hello": 13,
	   "world": 31,
	   "!"    : 71 ]

// iterating over key-value pairs:
for (key, value) in myMap {
    println("key = \(key), value = \(value)")
}
```



## Tcl


### With Arrays


```tcl
array set myAry {
    # list items here...
}

# Iterate over keys and values
foreach {key value} [array get myAry] {
    puts "$key -> $value"
}

# Iterate over just keys
foreach key [array names myAry] {
    puts "key = $key"
}

# There is nothing for directly iterating over just the values
# Use the keys+values version and ignore the keys
```


### With Dictionaries

{{works with|Tcl|8.5}}

```tcl
set myDict [dict create ...]; # Make the dictionary

# Iterate over keys and values
dict for {key value} $myDict {
    puts "$key -> $value"
}

# Iterate over keys
foreach key [dict keys $myDict] {
    puts "key = $key"
}

# Iterate over values
foreach value [dict values $myDict] {
    puts "value = $value"
}
```



## TXR



```txrlisp
(defvarl h (hash))

(each ((k '(a b c))
       (v '(1 2 3)))
  (set [h k] v))

(dohash (k v h)
  (put-line `@k -> @v`))
```


{{out|Run}}


```txt
$ txr hash.tl
c -> 3
b -> 2
a -> 1
```



## UNIX Shell

Two shells have associative arrays, but they use different syntax to access their keys.

{{works with|ksh93}}

```bash
typeset -A a=([key1]=value1 [key2]=value2)

# just keys
printf '%s\n' "${!a[@]}"

# just values
printf '%s\n' "${a[@]}"

# keys and values
for key in "${!a[@]}"; do
	printf '%s => %s\n' "$key" "${a[$key]}"
done
```


{{works with|zsh}}

```bash
typeset -A a
a=(key1 value1 key2 value2)

# just keys
print -l -- ${(k)a}

# just values
print -l -- ${(v)a}

# keys and values
printf '%s => %s\n' ${(kv)a}
```



## Vala

{{libheader|Gee}}

```vala

using Gee;

void main(){
    // declare HashMap
    var map = new HashMap<string, double?>();

    // set 3 entries
    map["pi"] = 3.14;
    map["e"] = 2.72;
    map["golden"] = 1.62;

    // iterate over (key,value) pair
    foreach (var elem in map.entries){
        string name = elem.key;
        double num = elem.value;

	stdout.printf("%s,%f\n", name, num);
    }

    // iterate over keys
    foreach (string key in map.keys){
	stdout.printf("%s\n", key);
    }

    // iterate over values
    foreach (double num in map.values){
	stdout.printf("%f\n", num);
    }
}

```


Compile with flag:

```txt

--pkg gee-1.0

```


{{out}}

```txt

e,2.720000
golden,1.620000
pi,3.140000
e
golden
pi
2.720000
1.620000
3.140000

```



## VBA

Dictionaries are similar in VBA and VBScript. Here is how to iterate.


```vb
Option Explicit
Sub Test()
    Dim h As Object, i As Long, u, v, s
    Set h = CreateObject("Scripting.Dictionary")
    h.Add "A", 1
    h.Add "B", 2
    h.Add "C", 3

    'Iterate on keys
    For Each s In h.Keys
        Debug.Print s
    Next

    'Iterate on values
    For Each s In h.Items
        Debug.Print s
    Next

    'Iterate on both keys and values by creating two arrays
    u = h.Keys
    v = h.Items
    For i = 0 To h.Count - 1
        Debug.Print u(i), v(i)
    Next
End Sub
```



## VBScript


```vb

'instantiate the dictionary object
Set dict = CreateObject("Scripting.Dictionary")

'populate the dictionary or hash table
dict.Add 1,"larry"
dict.Add 2,"curly"
dict.Add 3,"moe"

'iterate key and value pairs
For Each key In dict.Keys
	WScript.StdOut.WriteLine key & " - " & dict.Item(key)
Next

```


{{Out}}

```txt

1 - larry
2 - curly
3 - moe

```



## Vim Script


```vim
let dict = {"apples": 11, "oranges": 25, "pears": 4}

echo "Iterating over key-value pairs"
for [key, value] in items(dict)
    echo key " => " value
endfor
echo "\n"

echo "Iterating over keys"
for key in keys(dict)
    echo key
endfor
echo "\n"

echo "Iterating over values"
for value in values(dict)
    echo value
endfor
```


{{Out}}

```txt
Iterating over key-value pairs
oranges  =>  25
pears  =>  4
apples  =>  11

Iterating over keys
oranges
pears
apples

Iterating over values
25
4
11
```



## Wart


```wart
h <- (table 'a 1 'b 2)
each (key val) table
  prn key " " val
```


{{out}}

```txt
a 1
b 2
```



## XPL0


```XPL0
include c:\cxpl\stdlib;
char Dict(10,10);
int  Entries;

proc AddEntry(Letter, Greek);   \Insert entry into associative array
char Letter, Greek;
[Dict(Entries,0):= Letter;
StrCopy(Greek, @Dict(Entries,1));
Entries:= Entries+1;            \(limit checks ignored for simplicity)
];

int I;
[Entries:= 0;
AddEntry(^A, "alpha");
AddEntry(^D, "delta");
AddEntry(^B, "beta");
AddEntry(^C, "gamma");
for I:= 0 to Entries-1 do
    [ChOut(0, Dict(I,0));  ChOut(0, ^ );  Text(0, @Dict(I,1));  CrLf(0)];
]
```


{{out}}

```txt

A alpha
D delta
B beta
C gamma

```



## zkl


```zkl
var d=Dictionary("A","alpha","D","delta", "B","beta", "C", "gamma");
d.keys.pump(Console.print,fcn(k){String(k,",")})
d.values.apply("toUpper").println();
d.makeReadOnly();  // can only iterate over k,v pairs if read only
foreach k,v in (d){print(k,":",v,"; ")}
```

{{out}}

```txt

A,B,C,D,
L("ALPHA","BETA","GAMMA","DELTA")
A:alpha; B:beta; C:gamma; D:delta;

```


{{omit from|Applesoft BASIC}}
{{omit from|Brainfuck}}
{{omit from|Commodore BASIC}}
{{omit from|Integer BASIC}}
{{omit from|TI-89 BASIC}} <!-- No builtin assoc arrays, would not be enlightening to show a defn -->
