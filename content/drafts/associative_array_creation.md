+++
title = "Associative array/Creation"
description = ""
date = 2019-10-17T04:36:08Z
aliases = []
[extra]
id = 1642
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}[[Category:Data Structures]]

;Task:
The goal is to create an [[associative array]] (also known as a dictionary, map, or hash).


Related tasks:
* [[Associative arrays/Iteration]]
* [[Hash from two arrays]]


{{Template:See also lists}}





## 11l


```11l
V dict = [‘key1’ = 1, ‘key2’ = 2]
V value2 = dict[‘key2’]
```



## 8th

8th has 'maps' as built-in data types, and can use JSON to describe them:

```Forth

{ "one" : 1, "two" : "bad" }

```

Alternatively, they can be created in code:

```Forth

m:new "one" 1 m:! "two" "bad" m:!

```



## ActionScript

Because ActionScript does not have associative arrays in the normal sense, Object objects are used instead and keys are simply properties on those objects.

```actionscript
var map:Object = {key1: "value1", key2: "value2"};
trace(map['key1']); // outputs "value1"

// Dot notation can also be used
trace(map.key2); // outputs "value2"

// More keys and values can then be added
map['key3'] = "value3";
trace(map['key3']); // outputs "value3"
```

''Note:  The Object only supports String keys.  To use an object as a key, try the flash.utils.Dictionary class.''


## Ada

{{works with|GNAT|GPL 2007}}

```ada
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

procedure Associative_Array is

   -- Instantiate the generic package Ada.Containers.Ordered_Maps

   package Associative_Int is new Ada.Containers.Ordered_Maps(Unbounded_String, Integer);
   use Associative_Int;

   Color_Map : Map;
   Color_Cursor : Cursor;
   Success : Boolean;
   Value : Integer;
begin

   -- Add values to the ordered map

   Color_Map.Insert(To_Unbounded_String("Red"), 10, Color_Cursor, Success);
   Color_Map.Insert(To_Unbounded_String("Blue"), 20, Color_Cursor, Success);
   Color_Map.Insert(To_Unbounded_String("Yellow"), 5, Color_Cursor, Success);

   -- retrieve values from the ordered map and print the value and key
   -- to the screen

   Value := Color_Map.Element(To_Unbounded_String("Red"));
   Ada.Text_Io.Put_Line("Red:" & Integer'Image(Value));
   Value := Color_Map.Element(To_Unbounded_String("Blue"));
   Ada.Text_IO.Put_Line("Blue:" & Integer'Image(Value));
   Value := Color_Map.Element(To_Unbounded_String("Yellow"));
   Ada.Text_IO.Put_Line("Yellow:" & Integer'Image(Value));
end Associative_Array;
```



## Aikido

Aikido provides a native <em>map</em> for associative arrays.  You can create them using a map literal and you can insert and remove items on the fly.

```aikido

var names = {}   // empty map
names["foo"] = "bar"
names[3] = 4

// initialized map
var names2 = {"foo": bar, 3:4}

// lookup map
var name = names["foo"]
if (typeof(name) == "none") {
    println ("not found")
} else {
    println (name)
}

// remove from map
delete names["foo"]



```



## Aime

Aime records are heterogenous associative arrays.  No creation procedure is required, declaration is fine.

```aime>record r;</lang


```aime
r_put(r, "A", 33);		# an integer value
r_put(r, "C", 2.5);		# a real value
r_put(r, "B", "associative");	# a string value
```



## ALGOL 68

{{trans|C++}}

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of FORMATted transput}}

```algol68
main:(

  MODE COLOR = BITS;
  FORMAT color repr = $"16r"16r6d$;

  # This is an associative array which maps strings to ints #
  MODE ITEM = STRUCT(STRING key, COLOR value);
  REF[]ITEM color map items := LOC[0]ITEM;

  PROC color map find = (STRING color)REF COLOR:(
    REF COLOR out;
# linear search! #
    FOR index FROM LWB key OF color map items TO UPB key OF color map items DO
      IF color = key OF color map items[index] THEN
        out := value OF color map items[index]; GO TO found
      FI
    OD;
      NIL EXIT
    found:
      out
  );

  PROC color map = (STRING color)REF COLOR:(
    REF COLOR out = color map find(color);
    IF out :=: REF COLOR(NIL) THEN # extend color map array #
      HEAP[UPB key OF color map items + 1]ITEM color map append;
      color map append[:UPB key OF color map items] := color map items;
      color map items := color map append;
      value OF (color map items[UPB value OF color map items] := (color, 16r000000)) # black #
    ELSE
      out
    FI
  );

  # First, populate it with some values #
  color map("red") := 16rff0000;
  color map("green") := 16r00ff00;
  color map("blue") := 16r0000ff;
  color map("my favourite color") := 16r00ffff;

  # then, get some values out #
  COLOR color := color map("green"); # color gets 16r00ff00 #
  color := color map("black"); # accessing unassigned values assigns them to 16r0 #

  # get some value out without accidentally inserting new ones #
  REF COLOR value = color map find("green");
  IF value :=: REF COLOR(NIL) THEN
    put(stand error, ("color not found!", new line))
  ELSE
    printf(($"green: "f(color repr)l$, value))
  FI;

  # Now I changed my mind about my favourite color, so change it #
  color map("my favourite color") := 16r337733;

  # print out all defined colors #
  FOR index FROM LWB color map items TO UPB color map items DO
    ITEM item = color map items[index];
    putf(stand error, ($"color map("""g""") = "f(color repr)l$, item))
  OD;

  FORMAT fmt;
  FORMAT comma sep = $"("n(UPB color map items-1)(f(fmt)", ")f(fmt)")"$;

  fmt := $""""g""""$;
  printf(($g$,"keys: ", comma sep, key OF color map items, $l$));
  fmt := color repr;
  printf(($g$,"values: ", comma sep, value OF color map items, $l$))

)
```

{{out}}

```txt

green: 16r00ff00
color map("red") = 16rff0000
color map("green") = 16r00ff00
color map("blue") = 16r0000ff
color map("my favourite color") = 16r337733
color map("black") = 16r000000
keys: ("red", "green", "blue", "my favourite color", "black")
values: (16rff0000, 16r00ff00, 16r0000ff, 16r337733, 16r000000)

```



## Apex

Apex provides a Map datatype that maps unique keys to a single value. Both keys and values can be any data type, including user-defined types. Like Java, equals and hashCode are used to determine key uniqueness for user-defined types. Uniqueness of sObject keys is determined by comparing field values.

Creating a new empty map of String to String:

```apex
// Cannot / Do not need to instantiate the algorithm implementation (e.g, HashMap).
Map<String, String> strMap = new Map<String, String>();
strMap.put('a', 'aval');
strMap.put('b', 'bval');

System.assert( strMap.containsKey('a') );
System.assertEquals( 'bval', strMap.get('b') );
// String keys are case-sensitive
System.assert( !strMap.containsKey('A') );
```


Creating a new map of String to String with values initialized:

```apex
Map<String, String> strMap = new Map<String, String>{
  'a' => 'aval',
  'b' => 'bval'
};

System.assert( strMap.containsKey('a') );
System.assertEquals( 'bval', strMap.get('b') );
```



## APL

{{works with|Dyalog APL}}

```apl
⍝  Create a namespace ("hash")
      X←⎕NS ⍬

      ⍝ Assign some names
      X.this←'that'
      X.foo←88

      ⍝  Access the names
      X.this
that

      ⍝  Or do it the array way
      X.(foo this)
88  that

      ⍝  Namespaces are first class objects
      sales ← ⎕NS ⍬
      sales.(prices quantities) ← (100 98.4 103.4 110.16) (10  12 8  10)
      sales.(revenue ← prices +.× quantities)
      sales.revenue
4109.6
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

print dict
```


{{out}}


```txt
#{ age 33, name "john", surname "doe" }
```



## AutoHotkey


###  True arrays

[[AutoHotkey_L]] has [http://ahkscript.org/docs/Objects.htm Objects] which function as associative arrays.

```AutoHotkey
associative_array := {key1: "value 1", "Key with spaces and non-alphanumeric characters !*+": 23}
MsgBox % associative_array.key1
	. "`n" associative_array["Key with spaces and non-alphanumeric characters !*+"]
```


###  Legacy versions

[[AutoHotkey_Basic]] does not have typical arrays.
However, variable names can be concatenated, simulating associative arrays.

```AutoHotkey
arrayX1 = first
arrayX2 = second
arrayX3 = foo
arrayX4 = bar
Loop, 4
  Msgbox % arrayX%A_Index%
```



## AutoIt

See '''[https://msdn.microsoft.com/en-us/library/x4k5wbx4.aspx here]''' in the MSDN the reference for the Dictionary object that can be used in VBA. The following example shows how to create a dictionary, add/remove keys, change a key or a value, and check the existence of a key.

```AutoIt
; Associative arrays in AutoIt.
; All the required functions are below the examples.

; Initialize an error handler to deal with any COM errors..
global $oMyError = ObjEvent("AutoIt.Error", "AAError")

; first example, simple.
global $simple

; Initialize your array ...
AAInit($simple)

AAAdd($simple, "Appple", "fruit")
AAAdd($simple, "Dog", "animal")
AAAdd($simple, "Silicon", "tetravalent metalloid semiconductor")

ConsoleWrite("It is well-known that Silicon is a " & AAGetItem($simple, "Silicon") & "." & @CRLF)
ConsoleWrite(@CRLF)


; A more interesting example..

$ini_path = "AA_Test.ini"
; Put this prefs section in your ini file..
; [test]
; foo=foo value
; foo2=foo2 value
; bar=bar value
; bar2=bar2 value


global $associative_array
AAInit($associative_array)

; We are going to convert this 2D array into a cute associative array where we
; can access the values by simply using their respective key names..
$test_array = IniReadSection($ini_path, "test")

for $z = 1 to 2 ; do it twice, to show that the items are *really* there!
	for $i = 1 to $test_array[0][0]
		$key_name = $test_array[$i][0]
		ConsoleWrite("Adding '" & $key_name & "'.." & @CRLF)
		; key already exists in "$associative_array", use the pre-determined value..
		if AAExists($associative_array, $key_name) then
			$this_value = AAGetItem($associative_array, $key_name)
			ConsoleWrite("key_name ALREADY EXISTS! : =>" & $key_name & "<=" & @CRLF)
		else
			$this_value = $test_array[$i][1]
			; store left=right value pair in AA
			if $this_value then
				AAAdd($associative_array, $key_name, $this_value)
			endif
		endif
	next
next

ConsoleWrite(@CRLF & "Array Count: =>" & AACount($associative_array) & "<=" & @CRLF)
AAList($associative_array)

ConsoleWrite(@CRLF & "Removing 'foo'..")
AARemove($associative_array, "foo")

ConsoleWrite(@CRLF & "Array Count: =>" & AACount($associative_array) & "<=" & @CRLF)
AAList($associative_array)


AAWipe($associative_array)


; end



func AAInit(ByRef $dict_obj)
$dict_obj = ObjCreate("Scripting.Dictionary")
endfunc

; Adds a key and item pair to a Dictionary object..
func AAAdd(ByRef $dict_obj, $key, $val)
    $dict_obj.Add($key, $val)
    If @error Then return SetError(1, 1, -1)
endfunc

; Removes a key and item pair from a Dictionary object..
func AARemove(ByRef $dict_obj, $key)
	$dict_obj.Remove($key)
	If @error Then return SetError(1, 1, -1)
endfunc

; Returns true if a specified key exists in the associative array, false if not..
func AAExists(ByRef $dict_obj, $key)
	return $dict_obj.Exists($key)
endfunc

; Returns a value for a specified key name in the associative array..
func AAGetItem(ByRef $dict_obj, $key)
	return $dict_obj.Item($key)
endfunc

; Returns the total number of keys in the array..
func AACount(ByRef $dict_obj)
	return $dict_obj.Count
endfunc

; List all the "Key" > "Item" pairs in the array..
func AAList(ByRef $dict_obj)
ConsoleWrite("AAList: =>" & @CRLF)
	local $k = $dict_obj.Keys ; Get the keys
	; local $a = $dict_obj.Items ; Get the items (for reference)
	for $i = 0 to AACount($dict_obj) -1 ; Iterate the array
		ConsoleWrite($k[$i] & " ==> " & AAGetItem($dict_obj, $k[$i]) & @CRLF)
	next
endfunc

; Wipe the array, obviously.
func AAWipe(ByRef $dict_obj)
	$dict_obj.RemoveAll()
endfunc

; Oh oh!
func AAError()
	Local $err = $oMyError.number
	If $err = 0 Then $err = -1
	SetError($err)  ; to check for after this function returns
endfunc

;; End AA Functions.

```



## AWK

Arrays in AWK are indeed associative arrays.

```awk
BEGIN {
  a["red"] = 0xff0000
  a["green"] = 0x00ff00
  a["blue"] = 0x0000ff
  for (i in a) {
    printf "%8s %06x\n", i, a[i]
  }
  # deleting a key/value
  delete a["red"]
  for (i in a) {
    print i
  }
  # check if a key exists
  print ( "red" in a )   # print 0
  print ( "blue" in a )  # print 1
}
```



## Babel



```babel

    (("foo" 13)
    ("bar" 42)
    ("baz" 77)) ls2map !
```



## BaCon


```qbasic
DECLARE associative ASSOC STRING

associative("abc") = "first three"
associative("xyz") = "last three"

PRINT associative("xyz")
```


{{out}}

```txt

prompt$ ./assoc
last three

```

String keys, with ASSOC to a given data type.  Sizing is dynamic.


## BASIC256


```BASIC256
global values$, keys$
dim values$[1]
dim keys$[1]

call updateKey("a","apple")
call updateKey("b","banana")
call updateKey("c","cucumber")

gosub show

print "I like to eat a " + getValue$("c") + " on my salad."

call deleteKey("b")
call updateKey("c","carrot")
call updateKey("e","endive")
gosub show

end

show:
for t = 0 to countKeys()-1
   print getKeyByIndex$(t) + " " + getValueByIndex$(t)
next t
print
return

subroutine updateKey(key$, value$)
   # update or add an item
   i=findKey(key$)
   if i=-1 then
      i = freeKey()
      keys$[i] = key$
   end if
   values$[i] = value$
end subroutine

subroutine deleteKey(key$)
   # delete by clearing the key
   i=findKey(key$)
   if i<>-1 then
      keys$[i] = ""
   end if
end subroutine

function freeKey()
   # find index of a free element in the array
   for n = 0 to keys$[?]-1
      if keys$[n]="" then return n
   next n
   redim keys$[n+1]
   redim values$[n+1]
   return n
end function

function findKey(key$)
   # return index or -1 if not found
   for n = 0 to keys$[?]-1
      if key$=keys$[n] then return n
   next n
   return -1
end function

function getValue$(key$)
   # return a value by the key or "" if not existing
   i=findKey(key$)
   if i=-1 then
      return ""
   end if
   return values$[i]
end function

function countKeys()
   # return number of items
   # remember to skip the empty keys (deleted ones)
   k = 0
   for n = 0 to keys$[?] -1
      if keys$[n]<>"" then k++
   next n
   return k
end function

function getValueByIndex$(i)
   # get a value by the index
   # remember to skip the empty keys (deleted ones)
   k = 0
   for n = 0 to keys$[?] -1
      if keys$[n]<>"" then
         if k=i then return values$[k]
         k++
      endif
   next n
   return ""
end function

function getKeyByIndex$(i)
   # get a key by the index
   # remember to skip the empty keys (deleted ones)
   k = 0
   for n = 0 to keys$[?] -1
      if keys$[n]<>"" then
         if k=i then return keys$[k]
         k++
      endif
   next n
   return ""
end function
```

{{out}}

```txt
a apple
b banana
c cucumber

I like to eat a cucumber on my salad.
a apple
e endive
c carrot
```



## Batch File

This is cheating, I'm sure of it.


```dos
::assocarrays.cmd
@echo off
setlocal ENABLEDELAYEDEXPANSION
set array.dog=1
set array.cat=2
set array.wolf=3
set array.cow=4
for %%i in (dog cat wolf cow) do call :showit array.%%i !array.%%i!
set c=-27
call :mkarray sicko flu 5 measles 6 mumps 7 bromodrosis 8
for %%i in (flu measles mumps bromodrosis) do call :showit "sicko^&%%i" !sicko^&%%i!
endlocal
goto :eof

:mkarray
set %1^&%2=%3
shift /2
shift /2
if "%2" neq "" goto :mkarray
goto :eof

:showit
echo %1 = %2
goto :eof

```


{{out}}

```txt
array.dog = 1
array.cat = 2
array.wolf = 3
array.cow = 4
"sicko&flu" = 5
"sicko&measles" = 6
"sicko&mumps" = 7
"sicko&bromodrosis" = 8
```



## BBC BASIC


```bbcbasic
      REM Store some values with their keys:
      PROCputdict(mydict$, "FF0000", "red")
      PROCputdict(mydict$, "00FF00", "green")
      PROCputdict(mydict$, "0000FF", "blue")

      REM Retrieve some values using their keys:
      PRINT FNgetdict(mydict$, "green")
      PRINT FNgetdict(mydict$, "red")
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

The hash is the only built-in Bracmat class. It is best used for e.g. a large dictionary, when manipulation of a very long list of key/value pairs with pattern matching would become too CPU-intensive. The same key can be stored with different values, as the example shows. If that is not desirable, the key (and its value) should be removed first.

```bracmat
  new$hash:?myhash
& (myhash..insert)$(title."Some title")
& (myhash..insert)$(formula.a+b+x^7)
& (myhash..insert)$(fruit.apples oranges kiwis)
& (myhash..insert)$(meat.)
& (myhash..insert)$(fruit.melons bananas)
& out$(myhash..find)$fruit
& (myhash..remove)$formula
& (myhash..insert)$(formula.x^2+y^2)
& out$(myhash..find)$formula;
```

{{out}}

```txt
(fruit.melons bananas) (fruit.apples oranges kiwis)
formula.x^2+y^2
```



## Brat


```brat
h = [:]  #Empty hash

h[:a] = 1  #Assign value
h[:b] = [1 2 3] #Assign another value

h2 = [a: 1, b: [1 2 3], 10 : "ten"]  #Initialized hash

h2[:b][2]  #Returns 3
```



## C

''Solution is at [[Associative arrays/Creation/C]]''.


## C++

The C++ standard defines std::map as a means of creating an association between a key of one arbitrary type and a value of another arbitrary type.  This requires the inclusion of the standard header '''map'''.


```cpp>#include <map></lang



### Creation

To create a simple map whose key is of type A and whose value is of type B, one would define the variable like so:

```cpp
std::map<A, B> exampleMap
```


If one wanted to us a key type of '''int''' and a value of '''double''', you would define it like so:


```cpp
std::map<int, double> exampleMap
```



### Insertion

Once we've created our map, we've got a couple different ways to insert the value.  Let's use an example key of 7, and an exable value of 3.14.
====Operator[]====
The first method is using the [] operator.

```cpp
exampleMap[7] = 3.14
```


Of course, you can use a variable (or any [[rvalue]] of the correct type) for the key or value parameters:

```cpp
int myKey = 7;
double myValue = 3.14;
exampleMap[myKey] = myValue;
```

====insert()====
The second approach is a little more complicated.  We have to use the '''pair<>''' template:

```cpp
exampleMap.insert(std::pair<int, double>(7,3.14));
```

or by using '''make_pair''' to avoid repeating key/value types:

```cpp
exampleMap.insert(std::make_pair(7,3.14));
```



### Lookup

As with insertion, there are a couple ways we can retrieve the value.
====operator[]====
We use it as an rvalue, supplying the correct key:

```cpp
myValue = exampleMap[myKey]
```

If the value doesn't already exist, a default-constructed object of the value's type will be inserted using the key you specified, and that default value will be returned.

====find()====
Alternatively, you can look up a value by using find(), storing its return value in an [[iterator]], and comparing the iterator against the map's end() [[sentinal value]]:

```cpp
double myValue = 0.0;
std::map<int, double>::iterator myIterator = exampleMap.find(myKey);
if(exampleMap.end() != myIterator)
{
  // Return the value for that key.
  myValue = myIterator->second;
}
```


The need for the '''->second''' code is because our iterator points to a '''pair<>()''', and our value is the second member of that pair.

This code assigns a 0 to myValue if the map contained a value.


### Example

This simple program creates a map, assigns a value to that map, retrieves a value from that map, and prints the value to STDOUT.

```cpp>#include <map

#include <iostreams>

int main()
{
  // Create the map.
  std::map<int, double> exampleMap;

  // Choose our key
  int myKey = 7;

  // Choose our value
  double myValue = 3.14;

  // Assign a value to the map with the specified key.
  exampleMap[myKey] = myValue;

  // Retrieve the value
  double myRetrievedValue = exampleMap[myKey];

  // Display our retrieved value.
  std::cout << myRetrievedValue << std::endl;

  // main() must return 0 on success.
  return 0;
}
```



## C sharp

'''Platform:''' [[.NET]] 1.x

```csharp
System.Collections.HashTable map = new System.Collections.HashTable();
map["key1"] = "foo";
```


'''Platform:''' [[.NET]] 2.0

```csharp
Dictionary<string, string> map = new Dictionary<string,string>();
map[ "key1" ] = "foo";
```


{{works with|C sharp|C#|3.0+}}

```csharp
var map = new Dictionary<string, string> {{"key1", "foo"}};
```



## Ceylon


```ceylon
import ceylon.collection {

	ArrayList,
	HashMap,
	naturalOrderTreeMap
}

shared void run() {

	// the easiest way is to use the map function to create
	// an immutable map
	value myMap = map {
		"foo" -> 5,
		"bar" -> 10,
		"baz" -> 15,
		"foo" -> 6 // by default the first "foo" will remain
	};

	// or you can use the HashMap constructor to create
	// a mutable one
	value myOtherMap = HashMap {
		"foo"->"bar"
	};
	myOtherMap.put("baz", "baxx");

	// there's also a sorted red/black tree map
	value myTreeMap = naturalOrderTreeMap {
		1 -> "won",
		2 -> "too",
		4 -> "fore"
	};
	for(num->homophone in myTreeMap) {
		print("``num`` is ``homophone``");
	}

}
```


## Chapel


In Chapel, associative arrays are regular arrays with a non-integer domain - values used as keys into the array.
The creation of the domain is independent from the creation of the array, and in fact the same domain can be used for multiple arrays, creating associative arrays with identical sets of keys. When the domain is changed, all arrays that use it will be reallocated.

<lang>// arr is an array of string to int. any type can be used in both places.
var keys: domain(string);
var arr: [keys] int;

// keys can be added to a domain using +, new values will be initialized to the default value (0 for int)
keys += "foo";
keys += "bar";
keys += "baz";

// array access via [] or ()
arr["foo"] = 1;
arr["bar"] = 4;
arr("baz") = 6;

// write auto-formats domains and arrays
writeln("Keys: ", keys);
writeln("Values: ", arr);

// keys can be deleted using -
keys -= "bar";

writeln("Keys: ", keys);
writeln("Values: ", arr);

// chapel also supports array literals
var arr2 = [ "John" => 3, "Pete" => 14 ];

writeln("arr2 keys: ", arr2.domain);
writeln("arr2 values: ", arr2);
```


{{out}}
 Keys: {foo, bar, baz}
 Values: 1 4 6
 Keys: {foo, baz}
 Values: 1 6
 arr2 keys: {John, Pete}
 arr2 values: 3 14


## Clojure


```lisp
{:key "value"
 :key2 "value2"
 :key3 "value3"}
```



## ColdFusion


```cfm
<cfset myHash = structNew()>
<cfset myHash.key1 = "foo">
<cfset myHash["key2"] = "bar">
<cfset myHash.put("key3","java-style")>
```


In ColdFusion, a map is literally a java.util.HashMap, thus the above 3rd method is possible.


## Common Lisp


```lisp
;; default :test is #'eql, which is suitable for numbers only,
;; or for implementation identity for other types!
;; Use #'equalp if you want case-insensitive keying on strings.

(setf my-hash (make-hash-table :test #'equal))
(setf (gethash "H2O" my-hash) "Water")
(setf (gethash "HCl" my-hash) "Hydrochloric Acid")
(setf (gethash "CO" my-hash) "Carbon Monoxide")

;; That was actually a hash table, an associative array or
;; alist is written like this:
(defparameter *legs* '((cow . 4) (flamingo . 2) (centipede . 100)))
;; you can use assoc to do lookups and cons new elements onto it to make it longer.
```



## Component Pascal

BlackBox Componente Builder<br/>
Using a handmade collections module with the following interface<br/>

```oberon2

DEFINITION Collections;

	IMPORT Boxes;

	CONST
		notFound = -1;

	TYPE
		Hash = POINTER TO RECORD
			cap-, size-: INTEGER;
			(h: Hash) ContainsKey (k: Boxes.Object): BOOLEAN, NEW;
			(h: Hash) Get (k: Boxes.Object): Boxes.Object, NEW;
			(h: Hash) IsEmpty (): BOOLEAN, NEW;
			(h: Hash) Put (k, v: Boxes.Object): Boxes.Object, NEW;
			(h: Hash) Remove (k: Boxes.Object): Boxes.Object, NEW;
			(h: Hash) Reset, NEW
		END;

		HashMap = POINTER TO RECORD
			cap-, size-: INTEGER;
			(hm: HashMap) ContainsKey (k: Boxes.Object): BOOLEAN, NEW;
			(hm: HashMap) ContainsValue (v: Boxes.Object): BOOLEAN, NEW;
			(hm: HashMap) Get (k: Boxes.Object): Boxes.Object, NEW;
			(hm: HashMap) IsEmpty (): BOOLEAN, NEW;
			(hm: HashMap) Keys (): POINTER TO ARRAY OF Boxes.Object, NEW;
			(hm: HashMap) Put (k, v: Boxes.Object): Boxes.Object, NEW;
			(hm: HashMap) Remove (k: Boxes.Object): Boxes.Object, NEW;
			(hm: HashMap) Reset, NEW;
			(hm: HashMap) Values (): POINTER TO ARRAY OF Boxes.Object, NEW
		END;

		LinkedList = POINTER TO RECORD
			first-, last-: Node;
			size-: INTEGER;
			(ll: LinkedList) Add (item: Boxes.Object), NEW;
			(ll: LinkedList) Append (item: Boxes.Object), NEW;
			(ll: LinkedList) AsString (): POINTER TO ARRAY OF CHAR, NEW;
			(ll: LinkedList) Contains (item: Boxes.Object): BOOLEAN, NEW;
			(ll: LinkedList) Get (at: INTEGER): Boxes.Object, NEW;
			(ll: LinkedList) IndexOf (item: Boxes.Object): INTEGER, NEW;
			(ll: LinkedList) Insert (at: INTEGER; item: Boxes.Object), NEW;
			(ll: LinkedList) IsEmpty (): BOOLEAN, NEW;
			(ll: LinkedList) Remove (item: Boxes.Object), NEW;
			(ll: LinkedList) RemoveAt (at: INTEGER), NEW;
			(ll: LinkedList) Reset, NEW;
			(ll: LinkedList) Set (at: INTEGER; item: Boxes.Object), NEW
		END;

		Vector = POINTER TO RECORD
			size-, cap-: LONGINT;
			(v: Vector) Add (item: Boxes.Object), NEW;
			(v: Vector) AddAt (item: Boxes.Object; i: INTEGER), NEW;
			(v: Vector) Contains (o: Boxes.Object): BOOLEAN, NEW;
			(v: Vector) Get (i: LONGINT): Boxes.Object, NEW;
			(v: Vector) IndexOf (o: Boxes.Object): LONGINT, NEW;
			(v: Vector) Remove (o: Boxes.Object), NEW;
			(v: Vector) RemoveIndex (i: LONGINT): Boxes.Object, NEW;
			(v: Vector) Set (i: LONGINT; o: Boxes.Object): Boxes.Object, NEW;
			(v: Vector) Trim, NEW
		END;

	PROCEDURE NewHash (cap: INTEGER): Hash;
	PROCEDURE NewHashMap (cap: INTEGER): HashMap;
	PROCEDURE NewLinkedList (): LinkedList;
	PROCEDURE NewVector (cap: INTEGER): Vector;

END Collections.

```

The program:

```oberon2

MODULE BbtAssociativeArrays;
IMPORT StdLog, Collections, Boxes;

PROCEDURE Do*;
VAR
	hm : Collections.HashMap;
	o : Boxes.Object;
	keys, values: POINTER TO ARRAY OF Boxes.Object;
	i: INTEGER;

BEGIN
	hm := Collections.NewHashMap(1009);
	o := hm.Put(Boxes.NewString("first"),Boxes.NewInteger(1));
	o := hm.Put(Boxes.NewString("second"),Boxes.NewInteger(2));
	o := hm.Put(Boxes.NewString("third"),Boxes.NewInteger(3));
	o := hm.Put(Boxes.NewString("one"),Boxes.NewInteger(1));

	StdLog.String("size: ");StdLog.Int(hm.size);StdLog.Ln;

END Do;

END BbtAssociativeArrays.

```

Execute:^Q BbtAssociativeArrays.Do<br/>
{{out}}

```txt

size:  4

```



## D


```d
void main() {
    auto hash = ["foo":42, "bar":100];
    assert("foo" in hash);
}
```



## Dao


```dao
m = { => } # empty ordered map, future inserted keys will be ordered
h = { -> } # empty hash map, future inserted keys will not be ordered

m = { 'foo' => 42, 'bar' => 100 } # with ordered keys
h = { 'foo' -> 42, 'bar' -> 100 } # with unordered keys
```



## Delphi


```Delphi
program AssociativeArrayCreation;

{$APPTYPE CONSOLE}

uses Generics.Collections;

var
  lDictionary: TDictionary<string, Integer>;
begin
  lDictionary := TDictionary<string, Integer>.Create;
  try
    lDictionary.Add('foo', 5);
    lDictionary.Add('bar', 10);
    lDictionary.Add('baz', 15);
    lDictionary.AddOrSetValue('foo', 6); // replaces value if it exists
  finally
    lDictionary.Free;
  end;
end.
```



## Dyalect


Dyalect has a <code>Tuple</code> data type which allows to add labels to values:


```dyalect
var t = (x: 1, y: 2, z: 3)
print(t.keys())
```


{{out}}


```txt
{ "x", "y", "z"}
```



## E


```e
[].asMap()                             # immutable, empty
["one" => 1, "two" => 2]               # immutable, 2 mappings
[].asMap().diverge()                   # mutable, empty
["one" => 2].diverge(String, float64)  # mutable, initial contents,
                                        #   typed (coerces to float)
```



## EchoLisp


```scheme

(lib 'hash) ;; needs hash.lib
(define H (make-hash)) ;; new hash table
;; keys may be symbols, numbers, strings ..
;; values may be any lisp object
(hash-set H 'simon 'antoniette)
   → antoniette
(hash-set H 'antoinette 'albert)
   → albert
(hash-set H "Elvis" 42)
    → 42
(hash-ref H 'Elvis)
    → #f ;; not found. Elvis is not "Elvis"
(hash-ref H "Elvis")
    → 42
(hash-ref H 'simon)
    → antoniette
(hash-count H)
    → 3

```



## Elena

ELENA 4.0:

```elena
import system'collections;

public program()
{
    // 1. Create
    var map := new Dictionary();
    map["key"] := "foox";
    map["key"] := "foo";
    map["key2"]:= "foo2";
    map["key3"]:= "foo3";
    map["key4"]:= "foo4";
}
```



###  Strong typed dictionary


```elena
import system'collections;

public program()
{
    // 1. Create
    auto map := new Map<string,string>();
    map["key"] := "foox";
    map["key"] := "foo";
    map["key2"]:= "foo2";
    map["key3"]:= "foo3";
    map["key4"]:= "foo4";
}
```



## Elixir

{{trans|Erlang}}

```elixir
defmodule RC do
  def test_create do
    IO.puts "< create Map.new >"
    m = Map.new                   #=> creates an empty Map
    m1 = Map.put(m,:foo,1)
    m2 = Map.put(m1,:bar,2)
    print_vals(m2)
    print_vals(%{m2 | foo: 3})
  end

  defp print_vals(m) do
    IO.inspect m
    Enum.each(m, fn {k,v} -> IO.puts "#{inspect k} => #{v}" end)
  end
end

RC.test_create
```


{{out}}

```txt

< create Map.new >
%{bar: 2, foo: 1}
:bar => 2
:foo => 1
%{bar: 2, foo: 3}
:bar => 2
:foo => 3

```



## Emacs Lisp


```Lisp
(setq my-table (make-hash-table))
(puthash 'key 'value my-table)
```


<code>make-hash-table</code> compares keys with <code>eql</code> by default.  This suits symbols and numbers (including floating point).  For string keys an <code>equal</code> test can be used,


```Lisp
(setq my-table (make-hash-table :test 'equal))
(puthash "key" 123 my-table)
```


<code>define-hash-table-test</code> can create other key comparison types.


## Erlang

Erlang offers several associative array type data structures, this example uses the dictionary data structure.

```erlang

-module(assoc).
-compile([export_all]).

test_create() ->
    D = dict:new(),
    D1 = dict:store(foo,1,D),
    D2 = dict:store(bar,2,D1),
    print_vals(D2),
    print_vals(dict:store(foo,3,D2)).

print_vals(D) ->
    lists:foreach(fun (K) ->
                          io:format("~p: ~b~n",[K,dict:fetch(K,D)])
                  end, dict:fetch_keys(D)).

```


{{out}}
 32> assoc:test_create().
 bar: 2
 foo: 1
 bar: 2
 foo: 3
 ok

=={{header|F Sharp|F#}}==
.NET 3.5 Generic Dictionary (mutable)

```fsharp

let dic = System.Collections.Generic.Dictionary<string,string>() ;;
dic.Add("key","val") ;
dic.["key"] <- "new val" ;

```

Functional dictionary (immutable)

```fsharp

let d = [("key","val");("other key","other val")] |> Map.ofList
let newd = d.Add("new key","new val")

let takeVal (d:Map<string,string>) =
    match d.TryFind("key") with
        | Some(v) -> printfn "%s" v
        | None -> printfn "not found"

```



## Factor

Associative mappings follow the associative protocol. See [http://docs.factorcode.org/content/article-assocs-protocol.html the docs].
Here's an example using a hashtable that can be run in the listener :

```factor
H{ { "one" 1 } { "two" 2 } }
{ [ "one" swap at . ]
  [ 2 swap value-at . ]
  [ "three" swap at . ]
  [ [ 3 "three" ] dip set-at ]
  [ "three" swap at . ] } cleave
```



## Fantom


Associative arrays are called 'maps' in Fantom:


```fantom

class Main
{
  public static Void main ()
  {
    // create a map which maps Ints to Strs, with given key-value pairs
    Int:Str map := [1:"alpha", 2:"beta", 3:"gamma"]

    // create an empty map
    Map map2 := [:]
    // now add some numbers mapped to their doubles
    10.times |Int i|
    {
      map2[i] = 2*i
    }

  }
}

```



## Forth

{{works with|GNU Forth|0.6.2}}

The Forth dictionary is normally only used for function and symbol definitions, but you can also define separate ''wordlists'' for holding functions or data. There is no special syntax in the language for this, but you can define your own. All of Forth's defining words are available for adding things to the wordlist, but CREATE is most generic.


```forth
: get ( key len table -- data )     \ 0 if not present
  search-wordlist if
    >body @
  else 0 then ;

: put ( data key len table -- )
  >r 2dup r@ search-wordlist if
    r> drop nip nip
    >body !
  else
    r> get-current >r set-current      \ switch definition word lists
    nextname create ,
    r> set-current
  then ;
 wordlist constant bar
5 s" alpha" bar put
9 s" beta"  bar put
2 s" gamma" bar put
s" alpha" bar get .    \ 5
8 s" Alpha" bar put    \ Forth dictionaries are normally case-insensitive
s" alpha" bar get .   \ 8
```

This is not necessarily a good option in all Forths, as the dictionary may be implemented as a simple linked list (normally not a problem because the dictionary is only used for compiling and interactive interpretation). [[GNU Forth]] and many other hosted Forths use hash tables for the dictionary, so this is a fine choice. If you need case-sensitive keys, GNU Forth has <tt>table</tt> and <tt>table-find</tt>, replacing <tt>wordlist</tt> and <tt>search-wordlist</tt>, respectively.

(The use of <tt>nextname ( str len -- )</tt> is a GNU Forth extension to <tt>create</tt>; there is no means in the ANS standard to use a string on the stack to create a dictionary entry.)

{{libheader|Forth Foundation Library}}

Hashtable for mapping strings to integer

```forth
include ffl/hct.fs

\ Create a hash table 'table' in the dictionary with a starting size of 10

10 hct-create htable

\ Insert entries

 5 s" foo" htable hct-insert
10 s" bar" htable hct-insert
15 s" baz" htable hct-insert

\ Get entry from the table

s" bar" htable hct-get [IF]
  .( Value:) . cr
[ELSE]
  .( Entry not present.) cr
[THEN]
```



## Free Pascal

FPC 3.2.0.+. Similar to Delphi.

```pascal
program AssociativeArrayCreation;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}
{$IFDEF WINDOWS}{$APPTYPE CONSOLE}{$ENDIF}
uses Generics.Collections;
var
  lDictionary: TDictionary<string, Integer>;
begin
  lDictionary := TDictionary<string, Integer>.Create;
  try
    lDictionary.Add('foo', 5);
    lDictionary.Add('bar', 10);
    lDictionary.Add('baz', 15);
    lDictionary.AddOrSetValue('foo', 6); // replaces value if it exists
  finally
    lDictionary.Free;
  end;
end.
```

FPC 2.4+. Using FGL instead of rtl-generics:

```pascal
program AssociativeArrayCreation;
{$IFDEF WINDOWS}{$APPTYPE CONSOLE}{$ENDIF}
{$MODE DELPHI}
uses fgl;

var
  lDictionary: TFPGMap<string, Integer>;
begin
  lDictionary := TFPGMap<string, Integer>.Create;
  try
    lDictionary.Add('foo', 5);
    lDictionary.Add('bar', 10);
    lDictionary.Add('baz', 15);
  finally
    lDictionary.Free;
  end;
end.
```


## Futhark


```futhark
let associative_array = {key1=1,key2=2}
```



## Gambas

See [[http://rosettacode.org/wiki/Associative_array/Iteration#Gambas Associative array/Iteration]]


## Go

Allowable key types are those with == and != operators.  This includes is boolean, numeric, string, pointer, channel, and interface types.  It also includes structs and arrays containing only these types.  Disallowed as map keys are all slice, function, and map types.

```go
// declare a nil map variable, for maps from string to int
var x map[string]int

// make an empty map
x = make(map[string]int)

// make an empty map with an initial capacity
x = make(map[string]int, 42)

// set a value
x["foo"] = 3

// getting values
y1 := x["bar"]     // zero value returned if no map entry exists for the key
y2, ok := x["bar"] // ok is a boolean, true if key exists in the map

// removing keys
delete(x, "foo")

// make a map with a literal
x = map[string]int{
	"foo": 2, "bar": 42, "baz": -1,
}
```



## Gosu

As an OOP language with generics Gosu can use any variety of Map classes.  In addition Gosu provides associative array syntax for all objects.

```javascript
// empty map
var emptyMap = new HashMap<String, Integer>()

// map initialization
var map = {"Scott"->50, "Carson"->40, "Luca"->30, "Kyle"->38}

// map key/value assignment
map["Scott"] = 51

// get a value
var x = map["Scott"]

// remove an entry
map.remove("Scott")

// loop and maps
for(entry in map.entrySet()) {
  print("Key: ${entry.Key}, Value: ${entry.Value}")
}

// functional iteration
map.eachKey(\ k ->print(map[k]))
map.eachValue(\ v ->print(v))
map.eachKeyAndValue(\ k, v -> print("Key: ${v}, Value: ${v}"))
var filtered = map.filterByValues(\ v ->v < 50)

// any object can be treated as an associative array
class Person {
  var name: String
  var age: int
}
// access properties on Person dynamically via associative array syntax
var scott = new Person()
scott["name"] = "Scott"
scott["age"] = 29
```



## Groovy

Create an empty map and add values

```groovy
map = [:]
map[7] = 7
map['foo'] = 'foovalue'
map.put('bar', 'barvalue')
map.moo = 'moovalue'

assert 7 == map[7]
assert 'foovalue' == map.foo
assert 'barvalue' == map['bar']
assert 'moovalue' == map.get('moo')
```


Create a pre-populated map and verify values

```groovy
map = [7:7, foo:'foovalue', bar:'barvalue', moo:'moovalue']

assert 7 == map[7]
assert 'foovalue' == map.foo
assert 'barvalue' == map['bar']
assert 'moovalue' == map.get('moo')
```



## Harbour

Create an empty array and add values:

```visualfoxpro
arr := { => }
arr[ 10 ] := "Val_10"
arr[ "foo" ] := "foovalue"
```

Create and initialize array:

```visualfoxpro
arr := hb_Hash( 10, "Val_10", "foo", "foovalue" )
// or
arr := { 10 => "Val_10", "foo" => "foovalue" }
```



## Haskell

Binary trees:
{{works with|GHC}}

```haskell
import Data.Map

dict = fromList [("key1","val1"), ("key2","val2")]

ans = Data.Map.lookup "key2" dict  -- evaluates to Just "val2"

```


It is also possible to use association lists (lists of pairs). It is inefficient (O(n) lookup), but simple.

```haskell
dict = [("key1","val1"), ("key2","val2")]

ans = lookup "key2" dict  -- evaluates to Just "val2"

```


GHC also had an imperative hash table implementation in the <code>Data.HashTable</code> module, but was removed in <code>GHC 7.8</code>.

Other standard associatives arrays libraries are : <code>Data.IntMap</code> and <code>Data.HasMap</code>


## hexiscript


```hexiscript
let d         dict 2  # Initial estimated size
let d["test"] "test"  # Strings can be used as index
let d[123]    123     # Integers can also be used as index

println d["test"]
println d[123]
```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon associative arrays are called tables.  Any value may be used as a key including complex structures. Tables can have default values and they have no inherent size limitation growing from empty to whatever size is needed.


```icon
procedure main()
   local t
   t := table()
   t["foo"] := "bar"
   write(t["foo"])
end
```



## Inform 7

The Inform 7 equivalent of an associative array is a relation between values.


### Static relation


```inform7
Hash Bar is a room.

Connection relates various texts to one number. The verb to be connected to implies the connection relation.

"foo" is connected to 12.
"bar" is connected to 34.
"baz" is connected to 56.

When play begins:
	[change values]
	now "bleck" is connected to 78;
	[check values]
	if "foo" is connected to 12, say "good.";
	if "bar" is not connected to 56, say "good.";
	[retrieve values]
	let V be the number that "baz" relates to by the connection relation;
	say "'baz' => [V].";
	end the story.
```



### Dynamic relation


```inform7
Hash Bar is a room.

When play begins:
	let R be a various-to-one relation of texts to numbers;
	[initialize the relation]
	now R relates "foo" to 12;
	now R relates "bar" to 34;
	now R relates "baz" to 56;
	[check values]
	if R relates "foo" to 12, say "good.";
	if R does not relate "bar" to 56, say "good.";
	[retrieve values]
	let V be the number that "baz" relates to by R;
	say "'baz' => [V].";
	end the story.
```



## Ioke


```ioke
{a: "a", b: "b"}
```



## J


Usually, in J, you would use a named pair of (same length) lists for this purpose - one of keys, one of values. There are a number of details here that vary with the the intended use patterns. (First you get it working and then if you run into bottlenecks you rebuild things to relieve the problems).

However, it's also possible to use the symbol table itself to hold the names. The symbol table has limitations (can only accept syntactically valid names), but we can turn arbitrary strings into valid symbols using base 62 encode and prefixing with a letter (hypothetically speaking, base 64 encode would let us build longer names than base 62, because of computational complexity issues - but the J symbol table also comes with a name length limit - 255 characters - and does not support 64 different characters in names):


```J
coclass 'assocArray'
    encode=: 'z', (a.{~;48 65 97(+ i.)&.>10 26 26) {~ 62x #.inv 256x #. a.&i.
    get=: ".@encode
    has=: 0 <: nc@<@encode
    set=:4 :'(encode x)=:y'
```


Example use:


```j
   example=: conew 'assocArray'
   'foo' set__example 1 2 3
1 2 3
   'bar' set__example 4 5 6
4 5 6
   get__example 'foo'
1 2 3
   has__example 'foo'
1
   bletch__example=: 7 8 9
   get__example 'bletch'
7 8 9
   codestroy__example''
```


Note that J's symbols (http://www.jsoftware.com/help/dictionary/dsco.htm) might also be used for this purpose. However, symbols are not garbage collected within a J session (and, instead, a mechanism is provided to optionally preserve them across sessions).


## Java

{{works with|Java|1.5+}}

Defining the Map:

```java5
Map<String, Integer> map = new HashMap<String, Integer>();
map.put("foo", 5);
map.put("bar", 10);
map.put("baz", 15);
map.put("foo", 6);
```

"Putting" a value for a key that already exists ("map.put("foo", 6)" in this example) will replace and return the old value for the key.

Initializing a Map as a class member:

```java5
public static Map<String, Integer> map = new HashMap<String, Integer>(){{
   put("foo", 5);
   put("bar", 10);
   put("baz", 15);
   put("foo", 6);
}};
```

Retrieving a value:

```java5
map.get("foo"); // => 6
map.get("invalid"); // => null
```

Note that it is possible to put <code>null</code> as a value, so <code>null</code> being returned by <code>get</code> is not sufficient for determining that the key is not in the <code>Map</code>. There is a <code>containsKey</code> method for that.

Iterate over keys:

```java5
for (String key: map.keySet())
   System.out.println(key);
```

Iterate over values:

```java5
for (int value: map.values())
   System.out.println(value);
```

Iterate over key, value pairs:

```java5
for (Map.Entry<String, Integer> entry: map.entrySet())
   System.out.println(entry.getKey() + " => " + entry.getValue());
```



## JavaScript

ECMAScript5.1 does not have associative arrays, however Objects (which are just an unordered bundle of name/value pairs) can be used like associative arrays. JavaScript Arrays may also be used, but Objects are the convention.

Javascript object property names (keys) are strings. Other types and expressions can be used with square bracket notation, they are evaluated and converted to strings and the result used as the property name. Using quotes on property names avoids potential collisions with reserved JavaScript key words.

```javascript
var assoc = {};

assoc['foo'] = 'bar';
assoc['another-key'] = 3;

// dot notation can be used if the property name is a valid identifier
assoc.thirdKey = 'we can also do this!';
assoc[2] = "the index here is the string '2'";

//using JavaScript's object literal notation
var assoc = {
  foo: 'bar',
  'another-key': 3 //the key can either be enclosed by quotes or not
};

//iterating keys
for (var key in assoc) {
  // hasOwnProperty() method ensures the property isn't inherited
  if (assoc.hasOwnProperty(key)) {
    alert('key:"' + key + '", value:"' + assoc[key] + '"');
  }
}
```


ECMAScript 6 (ES6) offers both a map and a weak map implementation. While Objects must use strings, Maps may use objects, functions, and numbers as keys in addition to strings.

```javascript
var map = new Map(),
    fn = function () {},
    obj = {};

map.set(fn, 123);
map.set(obj, 'abc');
map.set('key', 'val');
map.set(3, x => x + x);

map.get(fn); //=> 123
map.get(function () {}); //=> undefined because not the same function
map.get(obj); //=> 'abc'
map.get({}); //=> undefined because not the same object
map.get('key'); //=> 'val'
map.get(3); //=> (x => x + x)

map.size; //=> 4

//iterating using ES6 for..of syntax
for (var key of map.keys()) {
  console.log(key + ' => ' + map.get(key));
}
```



## jq

===Associative Arrays with String-Valued Keys===
In jq, JSON objects can be used as associative arrays, it being understood that only strings can be used as keys.  To avoid confusion, for the remainder of this section, we refer to JSON objects as such.  Their type in jq is "object".

```jq
# An empty object:
{}

# Its type:
{} | type
# "object"

# An object literal:
{"a": 97, "b" : 98}

# Programmatic object construction:
reduce ("a", "b", "c", "d") as $c ({}; . +  { ($c) : ($c|explode[.0])} )
# {"a":97,"c":99,"b":98,"d":100}

# Same as above:
reduce range (97;101) as $i ({}; . + { ([$i]|implode) : $i })

# Addition of a key/value pair by assignment:
{}["A"] = 65  # in this case, the object being added to is {}

# Alteration of the value of an existing key:
{"A": 65}["A"] = "AA"
```


===Associative Arrays with JSON-Valued Keys===
In this subsection, we define addKey(key;value), getKey(key), and removeKey(key)
to operate on a hash table for which the keys may be any JSON entities.  This is done by defining a collisionless hash function.

```jq
def collisionless:
   if type == "object" then with_entries(.value = (.value|collisionless))|tostring
   elif type == "array" then map(collisionless)|tostring
   else (type[0:1] + tostring)
   end;

# WARNING: addKey(key;value) will erase any previous value associated with key
def addKey(key;value):
  if type == "object" then  . + { (key|collisionless): value }
  else {} | addKey(key;value)
  end;

def getKey(key): .[key|collisionless];

def removeKey(key): delpaths( [ [key|collisionless] ] );
```

'''Example''':

```jq
{} | addKey(1;"one") | addKey(2; "two") | removeKey(1) | getKey(2)
```

produces:

```sh
"two"
```



## Jsish

From Javascript.  jsish warns of duplicate ''var'', in this case the ''assoc'' variable is reused.

```javascript
var assoc = {};

assoc['foo'] = 'bar';
assoc['another-key'] = 3;

// dot notation can be used if the property name is a valid identifier
assoc.thirdKey = 'we can also do this!';
assoc[2] = "the index here is the string '2'";
;assoc;

//using JavaScript's object literal notation
var assoc = {
  foo: 'bar',
  'another-key': 3 //the key can either be enclosed by quotes or not
};

//iterating keys
for (var key in assoc) {
  // hasOwnProperty() method ensures the property isn't inherited
  if (assoc.hasOwnProperty(key)) {
    puts('key:"' + key + '", value:"' + assoc[key] + '"');
  }
}
;assoc;

/*
=!EXPECTSTART!=
associativeArray.jsi:12: warn: duplicate var: assoc
assoc ==> { 2:"the index here is the string \'2\'", "another-key":3, foo:"bar", thirdKey:"we can also do this!" }
key:"another-key", value:"3"
key:"foo", value:"bar"
assoc ==> { "another-key":3, foo:"bar" }
=!EXPECTEND!=
*/
```

{{out}}

```txt
prompt$ jsish -u associativeArray.jsi
[PASS] associativeArray.jsi
```



## Julia

{{works with|Julia|0.6}}
We build dictionaries associating to some characters their code points, by listing the key/value pairs, through a dictionary comprehension, by creating an empty dictionary and filling it, by using the specific syntax associated to typed dictionaries.

```julia
dict = Dict('a' => 97, 'b' => 98) # list keys/values
# Dict{Char,Int64} with 2 entries:
#   'b' => 98
#   'a' => 97

dict = Dict(c => Int(c) for c = 'a':'d') # dict comprehension
# Dict{Char,Int64} with 4 entries:
#   'b' => 98
#   'a' => 97
#   'd' => 100
#   'c' => 99

dict['é'] = 233; dict # add an element
# Dict{Char,Int64} with 3 entries:
#   'b' => 98
#   'a' => 97
#   'é' => 233

emptydict = Dict() # create an empty dict
# Dict{Any,Any} with 0 entries

dict["a"] = 1 # type mismatch
# ERROR: MethodError: Cannot `convert` an object of type String to an object of type Char

typeof(dict) # type is infered correctly
# Dict{Char,Int64}

```



## K

Keys in a dictionary must be symbols (`symbol).

```K
   / creating an dictionary
   d1:.((`foo;1); (`bar;2); (`baz;3))

   / extracting a value
   d1[`bar]
2
```


Another approach.

```K
   d2: .()          / create empty dictionary
   d2[`"zero"]:0
   d2[`"one"]:1
   d2[`"two"]:2

   d2
.((`zero;0;)
  (`one;1;)
  (`two;2;))
```


Extracting the keys and values.

```K
   !d2              / the keys
`zero `one `two

   d2[]             /  the values
0 1 2
```



## Kotlin

{{trans|Java}}

```scala
fun main(args: Array<String>) {
    // map definition:
    val map = mapOf("foo" to 5,
                    "bar" to 10,
                    "baz" to 15,
                    "foo" to 6)

    // retrieval:
    println(map["foo"]) // => 6
    println(map["invalid"]) // => null

    // check keys:
    println("foo" in map) // => true
    println("invalid" in map) // => false

    // iterate over keys:
    for (k in map.keys) print("$k ")
    println()

    // iterate over values:
    for (v in map.values) print("$v ")
    println()

    // iterate over key, value pairs:
    for ((k, v) in map) println("$k => $v")
}
```



## Lang5


```lang5
: dip  swap '_ set execute _ ; : nip  swap drop ;
: first  0 extract nip ; : second  1 extract nip ;

: assoc-in  swap keys eq ;
: assoc-index'  over keys swap eq [1] index collapse ;
: at  swap assoc-index' subscript collapse second ;
: delete-at  swap assoc-index' first remove ;
: keys  1 transpose first ;
: set-at
    over 'dup dip assoc-in '+ reduce if 'dup dip delete-at then
    "swap 2 compress 1 compress" dip swap append ;

[['foo 5]]
10 'bar rot set-at
'bar over at .
'hello 'bar rot set-at
20 'baz rot set-at .
```



## Lasso


```Lasso
// In Lasso associative arrays are called maps

// Define an empty map
local(mymap = map)

// Define a map with content
local(mymap = map(
	'one'	= 'Monday',
	'2'	= 'Tuesday',
	3	= 'Wednesday'
))

// add elements to an existing map
#mymap -> insert('fourth' = 'Thursday')

// retrieve a value from a map
#mymap -> find('2') // Tuesday
'<br />'
#mymap -> find(3) // Wednesday, found by the key not the position
'<br />'

// Get all keys from a map
#mymap -> keys // staticarray(2, fourth, one, 3)
'<br />'

// Iterate thru a map and get values
with v in #mymap do {^
	#v
	'<br />'
^}
// Tuesday<br />Thursday<br />Monday<br />Wednesday<br />

// Perform actions on each value of a map
#mymap -> foreach => {
	#1 -> uppercase
	#1 -> reverse
}
#mymap // map(2 = YADSEUT, fourth = YADSRUHT, one = YADNOM, 3 = YADSENDEW)
```



## LFE


```lisp

(let* ((my-dict (: dict new))
       (my-dict (: dict store 'key-1 '"value 1" my-dict))
       (my-dict (: dict store 'key-2 '"value 2" my-dict)))
  (: io format '"size: ~p~n" (list (: dict size my-dict)))
  (: io format '"some data: ~p~n" (list (: dict fetch 'key-1 my-dict))))


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

print " Key 'green' is associated with data item "; sl.Get$( myAssocList$, "green")

```

 Key 'green' is associated with data item 50 255 50


## Lingo


```lingo
props = [#key1: "value1", #key2: "value2"]

put props[#key2]
-- "value2"
put props["key2"]
-- "value2"
put props.key2
-- "value2"
put props.getProp(#key2)
-- "value2"
```



## LiveCode

Livecode arrays are only associative, but can be accessed by ordinal if they are used as the key.

```LiveCode
command assocArray
    local tArray
    put "value 1" into tArray["key 1"]
    put 123 into tArray["key numbers"]
    put "a,b,c" into tArray["abc"]

    put "number of elements:" && the number of elements of tArray & return & \
          "length of item 3:" && the length of tArray["abc"] & return & \
          "keys:" && the keys of tArray
end assocArray
```

Output

```LiveCode
number of elements: 3
length of item 3: 5
keys: key numbers
abc
key 1
```



## Logo

[[UCB Logo]] has "property lists" which associate names with values. They have their own namespace.

```logo
pprop "animals "cat 5
pprop "animals "dog 4
pprop "animals "mouse 11
print gprop "animals "cat    ; 5
remprop "animals "dog
show plist "animals    ;  [mouse 11 cat 5]
```



## Lua

Lua tables are Hashes

```lua
hash = {}
hash[ "key-1" ] = "val1"
hash[ "key-2" ] = 1
hash[ "key-3" ] = {}
```

Returns nil on unknown key.


## M2000 Interpreter

Μ2000 has Inventory object to use it as a Map. All keys converted to strings. If a key has no value then key is the value until we place one. A special type of Inventory is the Inventory Queue, where we can use same keys, and we can't delete except from the last append.

```M2000 Interpreter

Inventory A="100":=1, "200":=5, 10:=500,  20:="Hello There"
Print len(A)
Print A(100)=1, A(200)=5, A$(20)="Hello There"
Return A, 100:=3, 200:=7
\\ print all elements
Print A
For i=0 to Len(A)-1 {
      \\ Key, Value by current order (using !)
      Print Eval$(A, i), A$(i!)
}
\\ Iterator
Append A, "End":=5000
N=Each(A)
While N {
      Print Eval$(A, N^), A$(N^!)
}
Print Len(A)=5
Delete A, "100", 10, 20
Print Len(A)=2
If Exist(A, "End") Then Print Eval(A)=5000


```



## Maple

Maple tables are hashed arrays.  A table can be constructed by using the table constructor.

```Maple>
 T := table( [ (2,3) = 4, "foo" = 1, sin(x) = cos(x) ] );
          T := table(["foo" = 1, sin(x) = cos(x), (2, 3) = 4])

> T[2,3];
                                   4

> T[sin(x)];
                                 cos(x)

> T["foo"];
                                   1
```

New entries are added by assignment.

```Maple>
 T[ "bar" ] := 2;
                             T["bar"] := 2

> T[ "bar" ];
                                   2
```

Entries can be removed as follows.

```Maple>
 T[ "foo" ] := evaln( T[ "foo" ] );
                          T["foo"] := T["foo"]

> T[ "foo" ];
                                T["foo"]
```

(The latter output indicates that T["foo"] is an unassigned name.)

=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
a[2] = "string"; a["sometext"] = 23;
```


=={{header|MATLAB}} / {{header|Octave}}==


### MATLAB/Octave: structs

Associative arrays are called structs. The following methods of creating hash are equivalent.


```MATLAB
   hash.a = 1;
   hash.b = 2;
   hash.C = [3,4,5];
```

alternatively

```MATLAB
   hash = [];
   hash = setfield(hash,'a',1);
   hash = setfield(hash,'b',2);
   hash = setfield(hash,'C',[3,4,5]);
```

or

```MATLAB
   hash.('a') = 1;
   hash.('b') = 2;
   hash.('C') = [3,4,5];
```



```txt
>>    disp(hash)
  scalar structure containing the fields:
    a =  1
    b =  2
    C =

       3   4   5
```


Limitation: key must be a string containing only characters, digits and underscores, and the key string must start with a character.


### MATLAB only: containers.Map

Use of containers.Map removes some restrictions on key types that structs have. Keys can all be numeric or all be strings. Values can be of any type. Key and value types cannot be changed after creation of the containers.Map object.

```MATLAB
m = containers.Map({'a' 'b' 'C'}, [1 2 3]);
```

is equivalent to

```MATLAB
m = containers.Map;
m('a') = 1;
m('b') = 2;
m('C') = 3;
```

since the KeyType defaults to 'char'. For numeric keys, the key and value types must be specified at creation.

```MATLAB
m = containers.Map([51 72 37], {'fiftyone' 'seventytwo' 'thirtyseven'});
```

is equivalent to

```MATLAB
m = containers.Map('KeyType', 'double', 'ValueType', 'any');
m(51) = 'fiftyone';
m(72) = 'seventytwo';
m(37) = 'thirtyseven';
```

Usage:

```txt
>> m = containers.Map([51 72 37], {'fiftyone' 'seventytwo' 'thirtyseven'});
>> keys(m)

ans =

    [37]    [51]    [72]

>> values(m)

ans =

    'thirtyseven'    'fiftyone'    'seventytwo'
```



## Maxima


```maxima
/* No need to declare anything, undeclared arrays are hashed */

h[1]: 6;
h[9]: 2;

arrayinfo(h);
[hashed, 1, [1], [9]]
```



## MiniScript

A map literal in MiniScript is enclosed in curly braces, with key:value pairs separated by semicolons.  Keys and values may be any type.  Retrieval or assignment is by putting the key in square brackets.  As syntactic sugar, when a string key follows the rules of a MiniScript identifier (starts with a letter and contains only letters, numbers, and underscores), you may also access it with dot syntax.

```MiniScript
map = { 3: "test", "foo": 42 }

print map[3]
map[3] = "more tests"
print map[3]
print map["foo"]
print map.foo  // same as map["foo"] (only for string keys that are valid identifiers)

```



## Nemerle

This demonstrates two of several constructors, initializing the hashtable with a list of tuples or just specifying an initial capacity.

```Nemerle
using System;
using System.Console;
using Nemerle.Collections;

module AssocArray
{
    Main() : void
    {
        def hash1 = Hashtable([(1, "one"), (2, "two"), (3, "three")]);
        def hash2 = Hashtable(3);
        foreach (e in hash1)
            hash2[e.Value] = e.Key;
        WriteLine("Enter 1, 2, or 3:");
        def entry = int.Parse(ReadLine());
        WriteLine(hash1[entry]);
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */

options replace format comments java crossref symbols

key0 = '0'
key1 = 'key0'

hash = '.'            -- Initialize the associative array 'hash' to '.'
hash[key1] = 'value0' -- Set a specific key/value pair

say '<hash key="'key0'" value="'hash[key0]'" />' -- Display a value for a key that wasn't set
say '<hash key="'key1'" value="'hash[key1]'" />' -- Display a value for a key that was set
```


{{out}}

```txt

&lt;hash key="0" value="." /&gt;
&lt;hash key="key0" value="value0" /&gt;

```



## Nim


```nim
import tables

var
  hash = initTable[string, int]() # empty hash table
  hash2 = {"key1": 1, "key2": 2}.toTable # hash table with two keys
  hash3 = [("key1", 1), ("key2", 2)].toTable # hash table from tuple array
  hash4 = @[("key1", 1), ("key2", 2)].toTable # hash table from tuple seq
  value = hash2["key1"]

hash["spam"] = 1
hash["eggs"] = 2
hash.add("foo", 3)

echo "hash has ", hash.len, " elements"
echo "hash has key foo? ", hash.hasKey("foo")
echo "hash has key bar? ", hash.hasKey("bar")

echo "iterate pairs:" # iterating over (key, value) pairs
for key, value in hash:
  echo key, ": ", value

echo "iterate keys:" # iterating over keys
for key in hash.keys:
  echo key

echo "iterate values:" # iterating over values
for key in hash.values:
  echo key
```

{{out}}

```txt
hash has 3 elements
hash has key foo? true
hash has key bar? false
iterate pairs:
eggs: 2
foo: 3
spam: 1
iterate keys:
eggs
foo
spam
iterate values:
2
3
1
```


=={{header|Oberon-2}}==
{{works with| oo2c Version 2}}

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

Object parameters must be implicitly casted to the types expected by the method that's called.


###  Associative map


```objeck

# create map
map := StringMap->New();
# insert
map->Insert("two", IntHolder->New(2)->As(Base));
map->Insert("thirteen", IntHolder->New(13)->As(Base));
map->Insert("five", IntHolder->New(5)->As(Base));
map->Insert("seven", IntHolder->New(7)->As(Base));
# find
map->Find("thirteen")->As(IntHolder)->GetValue()->PrintLine();
map->Find("seven")->As(IntHolder)->GetValue()->PrintLine();

```



### Hash table


```objeck

# create map
map := StringHash->New();
# insert
map->Insert("two", IntHolder->New(2)->As(Base));
map->Insert("thirteen", IntHolder->New(13)->As(Base));
map->Insert("five", IntHolder->New(5)->As(Base));
map->Insert("seven", IntHolder->New(7)->As(Base));
# find
map->Find("thirteen")->As(IntHolder)->GetValue()->PrintLine();
map->Find("seven")->As(IntHolder)->GetValue()->PrintLine();

```


=={{header|Objective-C}}==
{{works with|Cocoa}} and {{works with|GNUstep}}

You can use a NSDictionary to create an immutable hash. A dictionary can contain only objects; if you want store non objects like integer, you have to box it in NSNumber.

```objc
NSDictionary *dict = [NSDictionary dictionaryWithObjectsAndKeys:
    @"Joe Doe", @"name",
    [NSNumber numberWithUnsignedInt:42], @"age",
    [NSNull null], @"extra",
    nil];
```


The same as the above with the new literal syntax in clang 3.1+ / Apple LLVM Compiler 4.0+ (XCode 4.4+) :

```objc
NSDictionary *dict = @{
    @"name": @"Joe Doe",
    @"age": @42,
    @"extra": [NSNull null],
    };
```


To create a mutable dictionary, use NSMutableDictionary:

```objc
NSMutableDictionary *dict = [NSMutableDictionary dictionary];
[dict setObject:@"Joe Doe" forKey:@"name"];
[dict setObject:[NSNumber numberWithInt:42] forKey:@"age"];
```


You can access value with objectForKey:. If a key does not exists, nil is returned.

```objc
NSString *name = [dict objectForKey:@"name"];
unsigned age = [dict objectForKey:@"age"] unsignedIntValue];
id missing = [dict objectForKey:@"missing"];
```



## OCaml


### Hash table

A simple idiom to create a hash table mapping strings to integers:

```ocaml
let hash = Hashtbl.create 0;;
List.iter (fun (key, value) -> Hashtbl.add hash key value)
  ["foo", 5; "bar", 10; "baz", 15];;
```

To retrieve a value:

```ocaml
let bar = Hashtbl.find hash "bar";; (* bar = 10 *)
```

To retrieve a value, returning a default if the key is not found:

```ocaml
let quux = try Hashtbl.find hash "quux" with Not_found -> some_value;;
```



### Binary tree

A simple idiom to create a persistent binary tree mapping strings to integers:

```ocaml
module String = struct
   type t = string
   let compare = Pervasives.compare
end
module StringMap = Map.Make(String);;

let map =
  List.fold_left
    (fun map (key, value) -> StringMap.add key value map)
    StringMap.empty
    ["foo", 5; "bar", 10; "baz", 15]
;;
```

To retrieve a value:

```ocaml
let bar = StringMap.find "bar" map;; (* bar = 10 *)
```

To retrieve a value, returning a default if the key is not found:

```ocaml
let quux = try StringMap.find "quux" map with Not_found -> some_value;;
```


### Association list

Some list functions allow you to use a list as an associative map, although the access time is O(N) so a Hashtbl or binary tree should be used for larger data-sets.

```Ocaml
let dict = ["foo", 5; "bar", 10; "baz", 15]

(* retrieve value *)
let bar_num = try List.assoc "bar" dict with Not_found -> 0;;

(* see if key exists *)
print_endline (if List.mem_assoc "foo" dict then "key found" else "key missing")
```



## Ol

Associative arrays in Otus Lisp has name "fixed function" (aka "ff") and fully conforms the functional paradigm. It means that there are no ability to change the existing associative array, only get new changed one.

You can use only values as keys (atomic numbers, constants) and, as exception, symbols (symbols are references, but unique). No strings, lists, vectors and other objects can be used directly. In such cases use hashes or similar mechanisms.


```scheme

;;; empty associative array
#empty
; or short form
#e

;;; creating the new empty associative array
(define empty-map #empty)

;;; creating associative array with values
(define my-map (list->ff '(
   (1 . 100)
   (2 . 200)
   (7 . 777))))
;;; or in short form (available from Ol version 2.1)
(define my-map '{
   (1 . 100)
   (2 . 200)
   (7 . 777)})

;;; add new key-value pair to the existing associative array
(define my-new-map (put my-map 'the-key 'the-value))

;;; print our arrays
(print empty-map)
; ==> #()

(print my-map)
; ==> #((1 . 100) (2 . 200) (7 . 777))

(print my-new-map)
; ==> #((1 . 100) (2 . 200) (7 . 777) (the-key . the-value))

```



## ooRexx

ooRexx has multiple classes that create index-to-item associative relationships.
* Directory -- a mapping for a string index to an object instance
* Table -- a mapping for an object index (of any class) to an object instance.  Index equality is determined by the "==" method.
* Relation -- a one-to-many mapping for an object index (of any class) to object instances.  Index equality is determined by the "==" method.
* IdentityTable -- a mapping for an object index (of any class) to an object instance.  Index equality is determined by unique object identity rather than equality.
* Stem -- The class backing ooRexx stem variables, which is also a first-class collection class.

All of the MapCollections are very similar in usage.  We'll use Directory for the examples here.

Defining the map:

```ooRexx
map = .directory~new
map["foo"] = 5
map["bar"] = 10
map["baz"] = 15
map["foo"] = 6

```

"Putting" a value for a key that already exists ("map["foo"] = 6" in this example) will replace and return the old value for the key.

Retrieving a value:

```ooRexx
item = map["foo"] -- => 6
item = map["invalid"] -- => .nil
```

Note that it is possible to put <code>.nil</code> as a value, so <code>.nil</code> being returned as a value is not sufficient for determining that the key is not in the collection. There is a <code>hasIndex</code> method for that.

Iterate over keys:

```ooRexx
loop key over map
   say key
end

```

Iterate over values:

```ooRexx
loop value over map~allItems
    say value
end

```

Iterate over key, value pairs:

```ooRexx

s = map~supplier
loop while s~available
    say s~index "=>" s~item
    s~next
end

```



## OxygenBasic

Not very efficient but the 'find' method could be optimised very easily.

```oxygenbasic

def n 200

Class AssociativeArray
'
### ===============


  indexbase 1
  string s[n]
  sys    max

  method find(string k) as sys
  sys i,e
  e=max*2
  for i=1 to e step 2
    if k=s[i] then return i
  next
  end method

  method dat(string k) as string
  sys i=find(k)
  if i then return s[i+1]
  end method

  method dat(string k, d) as sys
  sys i=find(k)
  if i=0 then
    if max>=n
      print "Array overflow" : return 0
    end if
    max+=1
    i=max*2-1
    s[i]=k
  end if
  s[i+1]=d
  return i
  end method

end class


'====
'TEST
'====

AssociativeArray A

'fill
A.s<={"shoes","LC1",  "ships","LC2",  "sealingwax","LC3",  "cabbages","LC4",  "kings","LC5"}
A.max=5
'access
print A.dat("ships")       'result LC2
A.dat("computers")="LC99"  '
print A.dat("computers")   'result LC99

```



## Oz

A mutable map is called a 'dictionary' in Oz:

```oz
declare
  Dict = {Dictionary.new}
in
  Dict.foo := 5
  Dict.bar := 10
  Dict.baz := 15
  Dict.foo := 20

  {Inspect Dict}
```


'Records' can be consideres immutable maps:

```oz
declare
  Rec = name(foo:5 bar:10 baz:20)
in
  {Inspect Rec}
```



## PARI/GP

{{works with|PARI/GP|2.8.1+}}

GP's associative arrays are called maps, and can be created like so:

```parigp
M = Map();
```

They can be used as follows:

```parigp
mapput(M, "key", "value");
mapput(M, 17, "different value");
mapput(M, "key2", Pi);
mapget(M, "key2") \\ returns Pi
mapisdefined(M, "key3") \\ returns 0
mapdelete(M, "key2");
```


In PARI the commands are <code>gtomap</code>, <code>mapput</code>, <code>mapget</code>, <code>mapisdefined</code>, and <code>mapdelete</code>. You can also use the solutions in [[Associative arrays/Creation/C]].


## Perl


### Hash

Definition:

```perl># using =
 key does not need to be quoted unless it contains special chars
my %hash = (
  key1 => 'val1',
  'key-2' => 2,
  three => -238.83,
  4 => 'val3',
);

# using , both key and value need to be quoted if containing something non-numeric in nature
my %hash = (
  'key1', 'val1',
  'key-2', 2,
  'three', -238.83,
  4, 'val3',
);
```


Use:

```perl
print $hash{key1};

$hash{key1} = 'val1';

@hash{'key1', 'three'} = ('val1', -238.83);
```


### HashRef

Definition:

```perl
my $hashref = {
 key1 => 'val1',
  'key-2' => 2,
  three => -238.83,
  4 => 'val3',
}
```


Use:

```perl
print $hashref->{key1};

$hashref->{key1} = 'val1';

@{$hashref}{('key1', 'three')} = ('val1', -238.83);
```



### Key Types

Keys are strings.  Anything else is stringized in Perl's usual ways, which generally means integers work too, but for floating point care might be needed against round-off.

Various <code>tie</code> modules implement keys of other types, usually by constructing underlying string keys of suitable nature.  For example <code>Tie::RefHash</code> allows objects (blessed or unblessed) as keys.


## Perl 6

{{works with|Rakudo|2018.03}}

The fatarrow, <code>=></code>, is no longer just a quoting comma; it now constructs a <code>Pair</code> object. But you can still define a hash with an ordinary list of even length.


```perl6
my %h1 = key1 => 'val1', 'key-2' => 2, three => -238.83, 4 => 'val3';
my %h2 = 'key1', 'val1', 'key-2', 2, 'three', -238.83, 4, 'val3';

# Creating a hash from two lists using a metaoperator.

my @a = 1..5;
my @b = 'a'..'e';
my %h = @a Z=> @b;

# Hash elements and hash slices now use the same sigil as the whole hash. This is construed as a feature.
# Curly braces no longer auto-quote, but Perl 6's qw (shortcut < ... >) now auto-subscripts.

say %h1{'key1'};
say %h1<key1>;
%h1<key1> = 'val1';
%h1<key1 three> = 'val1', -238.83;

# Special syntax is no longer necessary to access a hash stored in a scalar.

my $h = {key1 => 'val1', 'key-2' => 2, three => -238.83, 4 => 'val3'};
say $h<key1>;

# Keys are of type Str or Int by default. The type of the key can be provided.

my %hash{Any}; # same as %hash{*}
class C {};
my %cash{C};
%cash{C.new} = 1;
```



## Phix

Associative arrays are supported via just eight simple routines, with no specialised syntax.

Any key can be mapped to any value, and both can be anything (integer|float|string|[nested]sequence, including 0|NULL).

The setd(key,val) procedure is self-explanatory, except for an optional third parameter which is explained below.

The getd(key) function returns the associated data or 0 for non-existent keys: if that might be a valid value see getd_index().

By default, all keys and values are entered into one central dictionary. You can create multiple dictionaries by calling
integer tid=new_dict(), and pass that as an additional (final) parameter to the other routines (taking care not to miss
any). When you have no further use for it, an entire dictionary can be removed by invoking destroy_dict(tid).

```Phix
setd("one",1)
setd(2,"duo")
setd({3,4},{5,"six"})
?getd("one")                                    -- shows 1
?getd({3,4})                                    -- shows {5,"six"}
?getd(2)                                        -- shows "duo"
deld(2)
?getd(2)                                        -- shows 0
```



## PHP


```php
$array = array();
$array = []; // Simpler form of array initialization
$array['foo'] = 'bar';
$array['bar'] = 'foo';

echo($array['foo']); // bar
echo($array['moo']); // Undefined index

// Alternative (inline) way
$array2 = array('fruit' => 'apple',
                'price' => 12.96,
                'colour' => 'green');

// Another alternative (simpler) way
$array2 = ['fruit' => 'apple',
                'price' => 12.96,
                'colour' => 'green'];

// Check if key exists in the associative array
echo(isset($array['foo'])); // Faster, but returns false if the value of the element is set to null
echo(array_key_exists('foo', $array)); // Slower, but returns true if the value of the element is null
```



### Iterate over key/value


```php
foreach($array as $key => $value)
{
   echo "Key: $key Value: $value";
}
```



## PicoLisp

Here we use symbol properties. Other possiblities could be
[http://software-lab.de/doc/refI.html#idx index trees] or
[http://software-lab.de/doc/refA.html#assoc association lists].


```PicoLisp
(put 'A 'foo 5)
(put 'A 'bar 10)
(put 'A 'baz 15)
(put 'A 'foo 20)

: (get 'A 'bar)
-> 10

: (get 'A 'foo)
-> 20

: (show 'A)
A NIL
   foo 20
   bar 10
   baz 15
```



## PL/I


```pli
*process source xref attributes or(!);
 assocarr: Proc Options(main);
 Dcl 1 aa,
      2 an Bin Fixed(31) Init(0),
      2 pairs(100),
       3 key Char(10) Var,
       3 val Char(10) Var;
 Dcl hi Char(10) Value((high(10)));
 Dcl i  Bin Fixed(31);
 Dcl k Char(10) Var;

 Call aadd('1','spam');
 Call aadd('2','eggs');
 Call aadd('3','foo');
 Call aadd('2','spam');
 Call aadd('4','spam');

 Put Skip(' ');
 Put Edit('Iterate over keys')(Skip,a);
 Do i=1 To an;
   k=key(i);
   Put Edit('>'!!k!!'< => >'!!aacc(k)!!'<')(Skip,a);
   End;

 aadd: Proc(k,v);
 Dcl (k,v) Char(*) Var;
 If aacc(k)^=hi Then
   Put Edit('Key >',k,'< would be a duplicate, not added.')
           (Skip,a,a,a);
 Else Do;
   an+=1;
   key(an)=k;
   val(an)=v;
   Put Edit('added >'!!k!!'< -> '!!v!!'<')(Skip,a);
   End;
 End;

 aacc: Proc(k) Returns(Char(10) Var);
 Dcl k Char(*) Var;
 Dcl v Char(10) Var;
 Dcl i Bin Fixed(31);
 Do i=1 To an;
   If key(i)=k Then
     Return(val(i));
   End;
 Return(hi);
 End;

 End;
```

{{out}}

```txt
added >1< -> spam<
added >2< -> eggs<
added >3< -> foo<
Key >2< would be a duplicate, not added.
added >4< -> spam<

Iterate over keys
>1< => >spam<
>2< => >eggs<
>3< => >foo<
>4< => >spam<
```



## PL/SQL

PL/SQL allows associative arrays defined on two different keys types: Varchar2 or PLS/Integer

Associative Arrays are a PL/SQL only construct. Unlike Oracle Nested Tables or Varrays (the other two types of Oracle collections), associative arrays do not have a corresponding type which can be stored natively in the database. The following code will also show a workaround for this feature.

The following example code is a "record definition", which has nothing to do with associative arrays:-

```PL/SQL
DECLARE
    type ThisIsNotAnAssocArrayType is record (
        myShape VARCHAR2(20),
        mySize number,
        isActive BOOLEAN
    );
    assocArray ThisIsNotAnAssocArrayType ;
BEGIN
    assocArray.myShape := 'circle';

    dbms_output.put_line ('assocArray.myShape: ' || assocArray.myShape);
    dbms_output.put_line ('assocArray.mySize: ' || assocArray.mySize);
END;
/
```



## Pop11


```pop11
;;; Create expandable hash table of initial size 50 and with default
;;; value 0 (default value is returned when the item is absent).
vars ht = newmapping([], 50, 0, true);
;;; Set value corresponding to string 'foo'
12 -> ht('foo');
;;; print it
ht('foo') =>
;;; Set value corresponding to vector {1 2 3}
17 -> ht({1 2 3});
;;; print it
ht({1 2 3}) =>
;;; Set value corresponding to number 42 to vector {0 1}
{0 1} -> ht(42);
;;; print it
ht(42) =>

;;; Iterate over keys printing keys and values.
 appproperty(ht,
    procedure (key, value);
      printf(value, '%p\t');
      printf(key, '%p\n');
     endprocedure);
```



## PostScript


```postscript

 <</a 100 /b 200 /c 300>>
 dup /a get =

```



## Potion


```potion
mydictionary = (red=0xff0000, green=0x00ff00, blue=0x0000ff)

redblue = "purple"
mydictionary put(redblue, 0xff00ff)

255 == mydictionary("blue")
65280 == mydictionary("green")
16711935 == mydictionary("purple")
```



## PowerShell

An empty hash table can be created with:

```powershell
$hashtable = @{}
```

A hash table can be initialized with key/value pairs:

```powershell
$hashtable = @{
    "key1" = "value 1"
    key2 = 5            # if the key name has no spaces, no quotes are needed.
}
```

Individual values can be assigned or replaced by either using a property-style access method or indexing into the table with the given key:

```powershell
$hashtable.foo    = "bar"
$hashtable['bar'] = 42
$hashtable."a b"  = 3.14  # keys can contain spaces, property-style access needs quotation marks, then
$hashtable[5]     = 8     # keys don't need to be strings
```

NB. PowerShell compares strings as case-insensitive, that means the hashtable keys 'a' and 'A' are considered the same key. This happens when @{} is turned into a hashtable, but can be overridden by an explicit long-form:

```powershell
# Case insensitive keys, both end up as the same key:
$h=@{}
$h['a'] = 1
$h['A'] = 2
$h

Name                           Value
----                           -----
a                              2

# Case sensitive keys:
$h = New-Object -TypeName System.Collections.Hashtable
$h['a'] = 1
$h['A'] = 2
$h

Name                           Value
----                           -----
A                              2
a                              1
```

Similarly, values can be retrieved using either syntax:

```powershell
$hashtable.key1     # value 1
$hashtable['key2']  # 5
```

It is common to see a hashtable literal used to create an object, by casting it to a new type:

```powershell
$obj = [PSCustomObject]@{
    "key1" = "value 1"
    key2 = 5
}
```

This is a convenience syntax, has less code and runs faster than other ways to create objects.


## Prolog

We use the facts table for this purpose.

```prolog

mymap(key1,value1).
mymap(key2,value2).

?- mymap(key1,V).
   V = value1

```



## PureBasic

Hashes are a built-in type called Map in Purebasic.


```purebasic
NewMap dict.s()
dict("country") = "Germany"
Debug dict("country")
```



## Python

Hashes are a built-in type called dictionaries (or mappings) in Python.


```python
hash = dict()  # 'dict' is the dictionary type.
hash = dict(red="FF0000", green="00FF00", blue="0000FF")
hash = { 'key1':1, 'key2':2, }
value = hash[key]
```


Numerous methods exist for the mapping type https://docs.python.org/3/library/stdtypes.html#mapping-types-dict


```python
# empty dictionary
d = {}
d['spam'] = 1
d['eggs'] = 2

# dictionaries with two keys
d1 = {'spam': 1, 'eggs': 2}
d2 = dict(spam=1, eggs=2)

# dictionaries from tuple list
d1 = dict([('spam', 1), ('eggs', 2)])
d2 = dict(zip(['spam', 'eggs'], [1, 2]))

# iterating over keys
for key in d:
  print key, d[key]

# iterating over (key, value) pairs
for key, value in d.iteritems():
  print key, value
```


Note: Python dictionary keys can be of any arbitrary "hashable" type.  The following contains several distinct key value pairs:


```python
myDict = { '1': 'a string', 1: 'an integer', 1.0: 'a floating point number', (1,): 'a tuple' }
```


(Some other languages such as ''awk'' and ''Perl'' evaluate all keys such that numerically or lexically equivalent expressions become identical entries in the hash or associative array).

User defined classes which implement the ''__hash__()'' special method can also be used as dictionary keys.  It's the responsibility of the programmer to ensure the properties of the resultant hash value.  The instance object's unique ID
(accessible via the ''id()'' built-in function) is commonly used for this purpose.


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
> env[[index]]
```


```txt
[1] "rainfed hay"
```


```r>
 env[["1"]]
```


```txt
[1] "rainfed hay"
```


```r>> env</lang


```txt
<environment: 0xb7cd560>
```


```r>
 print(env)
```


```txt
<environment: 0xb7cd560>
```



###  vector example



```r>
 x <- c(hello=1, world=2, "!"=3)
> print(x)
```


```txt
hello world     !
    1     2     3
```


```r>
 print(names(x))
```


```txt
[1] "hello" "world" "!"
```


```r
print(unname(x))
```


```txt
[1] 1 2 3
```



###  list example



```R>
 a <- list(a=1, b=2, c=3.14, d="xyz")
> print(a)
```


```txt
$a
[1] 1

$b
[1] 2

$c
[1] 3.14

$d
[1] "xyz"
```


```r>
 print(names(a))
```


```txt
[1] "a" "b" "c" "d"
```


```r>
 print(unname(a))
```


```txt
[[1]]
[1] 1

[[2]]
[1] 2

[[3]]
[1] 3.14

[[4]]
[1] "xyz"
```



## Racket

In Racket, hash tables are natively supported and encouraged over association lists in many cases. Data structures that behave like dictionaries support a unified interface.


```racket

#lang racket

;; a-lists
(define a-list '((a . 5) (b . 10)))
(assoc a-list 'a) ; => '(a . 5)

;; hash tables
(define table #hash((a . 5) (b . 10)))
(hash-ref table 'a) ; => 5

;; dictionary interface
(dict-ref a-list 'a) ; => 5
(dict-ref table 'a)  ; => 5

```



## Raven


```raven
{ 'a' 1 'b' 2 'c' 3.14 'd' 'xyz' } as a_hash
a_hash print

hash (4 items)
 a => 1
 b => 2
 c => 3.14
 d => "xyz"

a_hash 'c' get         # get key 'c'
6.28 a_hash 'c' set    # set key 'c'
a_hash.'c'             # get key 'c' shorthand
6.28 a_hash:'c'        # set key 'c' shorthand
```


Null is returned for unknown keys.


## Retro


```Retro
with hashTable'
hashTable constant table

table %{ first = 100 }%
table %{ second = "hello, world!" keepString %}

table @" first" putn
table @" second" puts
```



## REXX


### version 1

Associative arrays are called ''stem variables'' in Rexx.

```Rexx
/* Rexx */

key0 = '0'
key1 = 'key0'

stem. = '.'                    /* Initialize the associative array 'stem' to '.' */
stem.key1 = 'value0'           /* Set a specific key/value pair                  */

Say 'stem.key0= 'stem.key  /* Display a value for a key that wasn't set */
Say 'stem.key1= 'stem.key1 /* Display a value for a key that was set    */
```

{{out}}

```txt
stem.key0= .
stem.key1= value0
```



### version 2


```rexx
/*REXX program shows how to set/display values for an associative array.*/
/*┌────────────────────────────────────────────────────────────────────┐
  │ The (below) two REXX statements aren't really necessary, but it    │
  │ shows how to define any and all entries in a associative array so  │
  │ that if a "key" is used that isn't defined, it can be displayed to │
  │ indicate such, or its value can be checked to determine if a       │
  │ particular associative array element has been set (defined).       │
  └────────────────────────────────────────────────────────────────────┘*/
stateC.=' [not defined yet] '          /*sets any/all state capitols.   */
stateN.=' [not defined yet] '          /*sets any/all state names.      */
/*┌────────────────────────────────────────────────────────────────────┐
  │ In REXX, when a "key" is used, it's normally stored (internally)   │
  │ as uppercase characters (as in the examples below).  Actually, any │
  │ characters can be used,  including blank(s) and non-displayable    │
  │ characters  (including '00'x, 'ff'x, commas, periods, quotes, ...).│
  └────────────────────────────────────────────────────────────────────┘*/
stateC.ca='Sacramento'; stateN.ca='California'
stateC.nd='Bismarck'  ; stateN.nd='North Dakota'
stateC.mn='St. Paul'  ; stateN.mn='Minnesota'
stateC.dc='Washington'; stateN.dc='District of Columbia'
stateC.ri='Providence'; stateN.ri='Rhode Island and Providence Plantations'

say 'capital of California is' stateC.ca
say 'capital of Oklahoma is' stateC.ok
yyy='RI'
say 'capital of' stateN.yyy "is" stateC.yyy
                                       /*stick a fork in it, we're done.*/
```

{{out}}

```txt

capital of California is Sacramento
capital of Oklahoma is  [not defined yet]
capital of Rhode Island and Providence Plantations is Providence

```



## Ring


```ring

# Project  Associative array/Creation

myarray = [["one",1],
                 ["two",2],
                 ["three",3]]
see find(myarray,"two",1) + nl
see find(myarray,2,2) + nl

```

Output:

```txt

2
2

```



## RLaB

Associative arrays are called ''lists'' in RLaB.

```RLaB

x = <<>>;  // create an empty list using strings as identifiers.
x.red   = strtod("0xff0000");  // RLaB doesn't deal with hexadecimal numbers directly. Thus we
x.green = strtod("0x00ff00");  // convert it to real numbers using ''strtod'' function.
x.blue  = strtod("0x0000ff");

// print content of a list
for (i in members(x))
{ printf("%8s %06x\n", i, int(x.[i])); }  // we have to use ''int'' function to convert reals to integers so "%x" format works

// deleting a key/value
clear (x.red);

// we can also use numeric identifiers in the above example
xid = members(x);  // this is a string array

for (i in 1:length(xid))
{ printf("%8s %06x\n", xid[i], int(x.[ xid[i] ])); }

// Finally, we can use numerical identifiers
// Note: ''members'' function orders the list identifiers lexicographically, in other words
// instead of, say, 1,2,3,4,5,6,7,8,9,10,11 ''members'' returns 1,10,11,2,3,4,5,6,7,8,9
x = <<>>;  // create an empty list
for (i in 1:5)
{ x.[i] = i; }  // assign to the element of list ''i'' the real value equal to i.


```



## Ruby

A hash object that returns [[nil]] for unknown keys

```ruby
hash={}
hash[666]='devil'
hash[777]  # => nil
hash[666]  # => 'devil'
```


A hash object that returns 'unknown key' for unknown keys

```ruby
hash=Hash.new('unknown key')
hash[666]='devil'
hash[777]  # => 'unknown key'
hash[666]  # => 'devil'
```


A hash object that returns "unknown key #{key}" for unknown keys

```ruby
hash=Hash.new{|h,k| "unknown key #{k}"}
hash[666]='devil'
hash[777]  # => 'unknown key 777'
hash[666]  # => 'devil'
```


A hash object that adds "key #{key} was added at #{Time.now}" to the hash the first time an unknown key is seen

```ruby
hash=Hash.new{|h,k|h[k]="key #{k} was added at #{Time.now}"}
hash[777]  # => 'key 777 was added at Sun Apr 03 13:49:57 -0700 2011'
hash[555]  # => 'key 555 was added at Sun Apr 03 13:50:01 -0700 2011'
hash[777]  # => 'key 777 was added at Sun Apr 03 13:49:57 -0700 2011'
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
    println!("{:?}", olympic_medals);
}
```



## Sather


```sather
class MAIN is
  main is
    -- creation of a map between strings and integers
    map ::= #MAP{STR, INT};

    -- add some values
    map := map.insert("red", 0xff0000);
    map := map.insert("green", 0xff00);
    map := map.insert("blue", 0xff);

    #OUT + map + "\n"; -- show the map...

    -- test if "indexes" exist
    #OUT +  map.has_ind("red") + "\n";
    #OUT +  map.has_ind("carpet") + "\n";

    -- retrieve a value by index
    #OUT + map["green"] + "\n";
  end;
end;

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



## Scheme


Scheme has association lists (alists), which are inefficient, ordered maps with arbitrary keys and values.

```scheme
(define my-dict '((a b) (1 hello) ("c" (a b c)))
(assoc 'a my-dict)                               ; evaluates to '(a b)
```



Hash tables are provided by SRFI-69 [http://srfi.schemers.org/srfi-69/srfi-69.html]. Many Scheme implementation also provide native hash tables.


```scheme
(define my-alist '((a b) (1 hello) ("c" (a b c)))
(define my-hash (alist->hash-table my-alist))
```


The R6RS standard specifies support for hashtables in the [http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-14.html#node_chap_13 standard libraries] document.


```scheme
#!r6rs

(import (rnrs base)
        (rnrs hashtables (6)))

(define my-hash (make-hashtable equal-hash equal?))
(hashtable-set! my-hash 'a 'b)
(hashtable-set! my-hash 1 'hello)
(hashtable-set! my-hash "c" '(a b c))
```



## Seed7

Seed7 uses the type [http://seed7.sourceforge.net/manual/types.htm#hash hash] to support associative arrays.


```seed7
$ include "seed7_05.s7i";

# Define hash type
const type: myHashType is hash [string] integer;

# Define hash table
var myHashType: aHash is myHashType.value;

const proc: main is func
  local
    var string: stri is "";
    var integer: number is 0;
  begin
    # Add elements
    aHash @:= ["foo"] 42;
    aHash @:= ["bar"] 100;

    # Check presence of an element
    if "foo" in aHash then

      # Access an element
      writeln(aHash["foo"]);
    end if;

    # Change an element
    aHash @:= ["foo"] 7;

    # Remove an element
    excl(aHash, "foo");

    # Loop over the hash values
    for number range aHash do
      writeln(number);
    end for;

    # Loop over the hash keys
    for key stri range aHash do
      writeln(stri);
    end for;

    # Loop over hash keys and values
    for number key stri range aHash do
      writeln("key: " <& stri <& ", value: " <& number);
    end for;
  end func;
```



## SETL

Associative arrays (referred to in SETL terminology as <i>maps</i>) are implemented as sets whose only members are tuples of length 2. Create such a set:

```setl
m := {['foo', 'a'], ['bar', 'b'], ['baz', 'c']};
```

We can then index the set, or map, with the first element of a constituent tuple to return that tuple's second element:

```setl
print( m('bar') );
```

{{out}}

```txt
b
```

If the map might contain more than one value associated with the same key, we can return the set of them (in this instance a unit set because the keys are in fact unique):

```setl
print( m{'bar'} );
```

{{out}}

```txt
{b}
```




## SETL4


```Setl4

* Iterate over key-value pairs of a map

    map = new('map 1:one 2:two 3:three')

visit(domain(map),'expr to evaluate for each member')

visit(range(map),'expr to evaluate for each member')

    next
        this = next(map)             :f(done)
        out(show(this) ':' show(get(map,this)) :next)
    done

    loop(d = domain(map)

    next
        out('next domain entry',next(d))    :s(next)
    done

    loop(d = range(map)

    next
        out('next domain entry',next(d))    :s(next)
    done

```



## Sidef


```ruby
var hash = Hash.new(
    key1 => 'value1',
    key2 => 'value2',
);

# Add a new key-value pair
hash{:key3} = 'value3';
```



## Slate



```slate
Dictionary new*, 'MI' -> 'Michigan', 'MN' -> 'Minnesota'
```



## Smalltalk


```smalltalk
states := Dictionary new.
states at: 'MI' put: 'Michigan'.
states at: 'MN' put: 'Minnesota'.
```



## SNOBOL4


```snobol4
	t = table()
	t<"red"> = "#ff0000"
	t<"green"> = "#00ff00"
	t<"blue"> = "#0000ff"

	output = t<"red">
	output = t<"blue">
	output = t<"green">
end
```




## SQL


```SQL

REM Create a table to associate keys with values
CREATE TABLE  associative_array ( KEY_COLUMN VARCHAR2(10), VALUE_COLUMN VARCHAR2(100)); .
REM Insert a Key Value Pair
INSERT (KEY_COLUMN, VALUE_COLUMN) VALUES ( 'VALUE','KEY');.
REM Retrieve a key value pair
SELECT aa.value_column FROM associative_array aa where aa.key_column = 'KEY';

```



## SQL PL

{{works with|Db2 LUW}} version 9.7 or higher.
With SQL PL:

```sql pl

--#SET TERMINATOR @

SET SERVEROUTPUT ON @

BEGIN
 DECLARE TYPE ASSOC_ARRAY AS VARCHAR(20) ARRAY [VARCHAR(20)];
 DECLARE HASH ASSOC_ARRAY;
 SET HASH['key1'] = 'val1';
 SET HASH['key-2'] = 2;
 SET HASH['three'] = -238.83;
 SET HASH[4] = 'val3';

 CALL DBMS_OUTPUT.PUT_LINE(HASH['key1']);
 CALL DBMS_OUTPUT.PUT_LINE(HASH['key-2']);
 CALL DBMS_OUTPUT.PUT_LINE(HASH['three']);
 CALL DBMS_OUTPUT.PUT_LINE(HASH[4]);
 CALL DBMS_OUTPUT.PUT_LINE(HASH['5']);
END@

```

Output:

```txt

db2 -td@
db2 => BEGIN
...
db2 (cont.) => END @
END
DB21034E  The command was processed as an SQL statement because it was not a
valid Command Line Processor command.  During SQL processing it returned:
SQL20439N  Array index with value "5" is out of range or does not exist.
SQLSTATE=2202E

val1
2
-238.83
val3

```



## Stata


```stata
mata
a=asarray_create()

// Add entries
asarray(a,"one",1)
asarray(a,"two",2)

// Check existence of key
asarray_contains(a,"two")

// Get a vector of all keys
asarray_keys(a)

// Number of entries
asarray_elements(a)

// End Mata session
end
```



## Swift


```swift
// make an empty map
var a = [String: Int]()
// or
var b: [String: Int] = [:]

// make an empty map with an initial capacity
var c = [String: Int](minimumCapacity: 42)

// set a value
c["foo"] = 3

// make a map with a literal
var d = ["foo": 2, "bar": 42, "baz": -1]
```



## Tcl

All arrays in Tcl are associative.


```tcl
# Create one element at a time:
set hash(foo) 5

# Create in bulk:
array set hash {
    foo 5
    bar 10
    baz 15
}

# Access one element:
set value $hash(foo)

# Output all values:
foreach key [array names hash] {
    puts $hash($key)
}
```


Tcl also provides associative map values (called “dictionaries”) from 8.5 onwards.


{{works with|Tcl|8.5}}

```tcl
# Create in bulk
set d [dict create  foo 5  bar 10  baz 15]

# Create/update one element
dict set d foo 5

# Access one value
set value [dict get $d foo]

# Output all values
dict for {key value} $d {
    puts $value
}
# Alternatively...
foreach value [dict values $d] {
    puts $value
}

# Output the whole dictionary (since it is a Tcl value itself)
puts $d
```



## Toka

Toka provides associative arrays via a library.


```toka
needs asarray

( create an associative array )
1024 cells is-asarray foo

( store 100 as the "first" element in the array )
100 " first" foo asarray.put

( store 200 as the "second" element in the array )
200 " second" foo asarray.put

( obtain and print the values )
" first" foo asarray.get .
" second" foo asarray.get .
```



## UNIX Shell

{{works with|ksh}}

```bash
typeset -A hash
hash=( [key1]=val1 [key2]=val2 )
hash[key3]=val3
echo "${hash[key3]}"
```


{{works with|bash}}
assigning values is the same as ksh, but to declare the variable as an associative array:

```bash
declare -A hash
```



## UnixPipes

A key value file can be considered as an associative array

```bash
map='p.map'

function init() {
cat <<EOF > $map
apple a
boy b
cat c
dog d
elephant e
EOF
}

function put() {
    k=$1; v=$2;
    del $k
    echo $v $k >> $map
 }

function get() {
    k=$1
    for v in $(cat $map | grep "$k$"); do
        echo $v
        break
    done
 }

function del() {
    k=$1
    temp=$(mktemp)
    mv $map $temp
    cat $temp | grep -v "$k$" > $map
}

function dump() {
    echo "-- Dump begin --"
    cat $map
    echo "-- Dump complete --"
}

init
get c
put c cow
get c
dump
```



## Vala

{{libheader|Gee}}

```vala

using Gee;

void main(){
    var	map = new HashMap<string, int>(); // creates a HashMap with keys of type string, and values of type int

    // two methods to set key,value pair
    map["one"] = 1;
    map["two"] = 2;

    map.set("four", 4);
    map.set("five", 5);

    // two methods of getting key,value pair
    stdout.printf("%d\n", map["one"]);

    stdout.printf("%d\n", map.get("two"));
}

```


Compile with flag:
```txt
 --pkg gee-1.0
```



## VBA

See '''[https://msdn.microsoft.com/en-us/library/x4k5wbx4.aspx here]''' in the MSDN the reference for the Dictionary object that can be used in VBA. The following example shows how to create a dictionary, add/remove keys, change a key or a value, and check the existence of a key.


```vb
Option Explicit
Sub Test()
    Dim h As Object
    Set h = CreateObject("Scripting.Dictionary")
    h.Add "A", 1
    h.Add "B", 2
    h.Add "C", 3
    Debug.Print h.Item("A")
    h.Item("C") = 4
    h.Key("C") = "D"
    Debug.Print h.exists("C")
    h.Remove "B"
    Debug.Print h.Count
    h.RemoveAll
    Debug.Print h.Count
End Sub
```



## Vim Script

Dictionary keys are always strings.

```vim
" Creating a dictionary with some initial values
let dict = {"one": 1, "two": 2}

" Retrieving a value
let two_a = dict["two"]
let two_b = dict.two
let two_c = get(dict, "two", "default value for missing key")

" Modifying a value
let dict["one"] = 1.0
let dict.two = 2.0

" Adding a new value
let dict["three"] = 3
let dict.four = 4

" Removing a value
let one = remove(dict, "one")
unlet dict["two"]
unlet dict.three
```



## Visual FoxPro

Visual FoxPro has a collection class which can be used for this.

```vfp

LOCAL loCol As Collection, k, n, o
CLEAR
*!* Example using strings
loCol = NEWOBJECT("Collection")
loCol.Add("Apples", "A")
loCol.Add("Oranges", "O")
loCol.Add("Pears", "P")
n = loCol.Count
? "Items:", n
*!* Loop through the collection
k = 1
FOR EACH o IN loCol FOXOBJECT
    ? o, loCol.GetKey(k)
    k = k + 1
ENDFOR
*!* Get an item by its key
? loCol("O")
?
*!* Example using objects
LOCAL loFruits As Collection
loFruits = NEWOBJECT("Collection")
loFruits.Add(CREATEOBJECT("fruit", "Apples"), "A")
loFruits.Add(CREATEOBJECT("fruit", "Oranges"), "O")
loFruits.Add(CREATEOBJECT("fruit", "Pears"), "P")
*!* Loop through the collection
k = 1
FOR EACH o IN loFruits FOXOBJECT
    ? o.Name, loFruits.GetKey(k)
    k = k + 1
ENDFOR
*!* Get an item name by its key
? loFruits("P").Name


DEFINE CLASS fruit As Custom
PROCEDURE Init(tcName As String)
THIS.Name = tcName
ENDPROC
ENDDEFINE

```



## Wart


```wart
h <- (table 'a 1 'b 2)
h 'a
=> 1
```



## XLISP

XLISP refers to associative arrays as tables. The <tt>MAKE-TABLE</tt> function returns a new empty table, for instance:

```lisp
(define starlings (make-table))
```

Values can then be inserted using <tt>TABLE-SET!</tt>:

```lisp
(table-set! starlings "Common starling" "Sturnus vulgaris")
(table-set! starlings "Abbot's starling" "Poeoptera femoralis")
(table-set! starlings "Cape starling" "Lamprotornis nitens")
```

and retrieved using <tt>TABLE-REF</tt> with their keys:

```lisp
(table-ref starlings "Cape starling")
```

Output in a REPL:

```txt
"Lamprotornis nitens"
```

Other functions provided for tables include <tt>MAP-OVER-TABLE-ENTRIES</tt>, which takes a table and a function of two arguments and applies the function to each entry (using the key and value as the two arguments), for instance:

```lisp
(map-over-table-entries starlings (lambda (x y) (print (string-append x " (Linnaean name " y ")"))))
```

Output in a REPL:

```txt
"Abbott's starling (Linnaean name Poeoptera femoralis)"
"Common starling (Linnaean name Sturnus vulgaris)"
"Cape starling (Linnaean name Lamprotornis nitens)"
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

func Lookup(Greek);             \Given Greek name return English letter
char Greek;
int  I;
[for I:= 0, Entries-1 do
    if StrCmp(Greek, @Dict(I,1)) = 0 then return Dict(I,0);
return ^?;
];

[Entries:= 0;
AddEntry(^A, "alpha");
AddEntry(^D, "delta");
AddEntry(^B, "beta");
AddEntry(^C, "gamma");
ChOut(0, Lookup("beta"));  CrLf(0);
ChOut(0, Lookup("omega")); CrLf(0);
]
```


For greater speed a hashing algorithm should be used to look up items in
a large dictionary, however hashing routines are not provided in the
standard library.

{{out}}

```txt

B
?

```



## zkl


```zkl
zkl: Dictionary("one",1, "two",2, "three",3)
D(two:2,three:3,one:1)

zkl: T("one",1, "two",2, "three",3).toDictionary()
D(two:2,three:3,one:1)
```


{{omit from|Applesoft BASIC}}
{{omit from|bc|No associative arrays. No string operations.}}
{{omit from|Brainfuck}}
{{omit from|dc|No associative arrays. No string operations.}}
{{omit from|GUISS}}
{{omit from|Integer BASIC}}
{{omit from|TI-83 BASIC|Same reason as TI-89.}}
{{omit from|TI-89 BASIC|Does not have associative arrays (unless you write a whole library yourself).}}
