+++
title = "Loops/Foreach"
description = ""
date = 2019-10-18T11:09:28Z
aliases = []
[extra]
id = 2822
[taxonomies]
categories = []
tags = []
+++

{{task|Iteration}}
Loop through and print each element in a collection in order.

Use your language's "for each" loop if it has one, otherwise iterate through the collection in order with some other loop.


;Related tasks:
*   [[Loop over multiple arrays simultaneously]]
*   [[Loops/Break]]
*   [[Loops/Continue]]
*   [[Loops/Do-while]]
*   [[Loops/Downward for]]
*   [[Loops/For]]
*   [[Loops/For with a specified step]]
*   [[Loops/Foreach]]
*   [[Loops/Increment loop index within loop body]]
*   [[Loops/Infinite]]
*   [[Loops/N plus one half]]
*   [[Loops/Nested]]
*   [[Loops/While]]
*   [[Loops/with multiple ranges]]
*   [[Loops/Wrong ranges]]





## ACL2



```Lisp
(defun print-list (xs)
   (if (endp xs)
       nil
       (prog2$ (cw "~x0~%" (first xs))
               (print-list (rest xs)))))
```



```txt

&gt; (print-list (list 1 "a" 1/2 (list 1 2) 'sym))
1
"a"
1/2
(1 2)
SYM
NIL

```



## Ada


### arrays


```Ada
with Ada.Integer_Text_IO;
use  Ada.Integer_Text_IO;

procedure For_Each is

   A : array (1..5) of Integer := (-1, 0, 1, 2, 3);

begin

   for Num in A'Range loop
      Put( A (Num) );
   end loop;

end For_Each;
```


Alternative solution (Ada 2012):


```Ada
   for Item of A loop
      Put( Item );
   end loop;
```



### doubly linked lists

{{works with|Ada 2005}}

```Ada
with Ada.Integer_Text_IO, Ada.Containers.Doubly_Linked_Lists;
use  Ada.Integer_Text_IO, Ada.Containers;

procedure Doubly_Linked_List is

   package DL_List_Pkg is new Doubly_Linked_Lists (Integer);
   use     DL_List_Pkg;

   procedure Print_Node (Position : Cursor) is
   begin
      Put (Element (Position));
   end Print_Node;

   DL_List : List;

begin

   DL_List.Append (1);
   DL_List.Append (2);
   DL_List.Append (3);

   -- Iterates through every node of the list.
   DL_List.Iterate (Print_Node'Access);

end Doubly_Linked_List;
```


### vectors

{{works with|Ada 2005}}

```Ada
with Ada.Integer_Text_IO, Ada.Containers.Vectors;
use  Ada.Integer_Text_IO, Ada.Containers;

procedure Vector_Example is

   package Vector_Pkg is new Vectors (Natural, Integer);
   use     Vector_Pkg;

   procedure Print_Element (Position : Cursor) is
   begin
      Put (Element (Position));
   end Print_Element;

   V : Vector;

begin

   V.Append (1);
   V.Append (2);
   V.Append (3);

   -- Iterates through every element of the vector.
   V.Iterate (Print_Element'Access);

end Vector_Example;
```



## Aikido

Aikido's <code>foreach</code> loop allows iteration through multiple value types.

### strings


```aikido
var str = "hello world"
foreach ch str {    // you can also use an optional 'in'
    println (ch)   // one character at a time
}
```


### vectors


```aikido
var vec = [1,2,3,4]
foreach v vec {    // you can also use an optional 'in'
    println (v)
}
```


### maps


```aikido
var cities = {"San Ramon": 50000, "Walnut Creek": 70000, "San Francisco": 700000}   // map literal
foreach city cities {
    println (city.first + " has population " + city.second)
}
```


### integers


```aikido
foreach i 100 {
    println (i)    // prints values 0..99
}

foreach i 10..20 {
    println (i)     // prints values 10..20
}

var a = 20
var b = 10
foreach i a..b {
    println (i)   // prints values from a to b (20..10)
}
```


### Objects

Aikido allows definition of a <code>foreach</code> operator for an object.  In this example we define a single linked list and a foreach operator to iterate through it

```aikido
class List {
    class Element (public data) {
        public var next = null
    }
    var start = null

    public function insert (data) {
        var element = new Element (data)
        element.next = start
        start = element
    }

    public operator foreach (var iter) {
        if (typeof(iter) == "none") {   // first iteration
            iter = start
            return iter.data
        } elif (iter.next == null) {    // check for last iteration
            iter = none
        } else {
            iter = iter.next      // somewhere in the middle
            return iter.data
        }
    }

}

var list = new List()
list.insert (1)
list.insert (2)
list.insert (4)

foreach n list {
    println (n)
}
```


### Coroutines

Aikido supports coroutines.  The foreach operator may be used to iterate thorough the generated values.

```aikido
// coroutine to generate the squares of a sequence of numbers
function squares (start, end) {
    for (var i = start ; i < end ; i++) {
        yield i*i
    }
}

var start = 10
var end = 20

foreach s squares (start, end) {
    println (s)
}
```


### Files

If you open a file you can iterate through all the lines

```aikido
var s = openin ("input.txt")
foreach line s {
    print (line)
}
```


### Enumerations


```aikido
enum Color {
   RED, GREEN, BLUE
}

foreach color Color {
    println (color)
}
```



## Aime


```aime
# iterate over a list of integers
integer i, v;

for (i, v in list(2, 3, 5, 7, 11, 13, 17, 18)) {
}
```




## ALGOL 68

{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386}}

```algol68
[]UNION(STRING, INT, PROC(REF FILE)VOID) collection = ("Mary","Had",1,"little","lamb.",new line);

FOR index FROM LWB collection TO UPB collection DO
  print((collection[index]," "))
OD
```

Output:

```txt

Mary Had          +1 little lamb.

```

Note: [[ALGOL 68S]] actually has a reserved word FOREACH that is used to break arrays in to portions, and process in parallel.


## AmigaE


```amigae
PROC main()
  DEF a_list : PTR TO LONG, a
  a_list := [10, 12, 14]
  FOR a := 0 TO ListLen(a_list)-1
    WriteF('\d\n', a_list[a])
  ENDFOR
  -> if the "action" fits a single statement, we can do instead
  ForAll({a}, a_list, `WriteF('\d\n', a))
ENDPROC
```



## Apex


```Apex

Integer[] myInts = new Integer[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

for (Integer i : myInts) {
    System.debug(i);
}

```



## AppleScript


```AppleScript
repeat with fruit in {"Apple", "Orange", "Banana"}
  log contents of fruit
end repeat
```



## Arturo


```arturo
array #("one" "two" "three")

dict #{
	name	"john"
	surname	"doe"
	age	33
}

loop array {
	print &
}

loop dict [key,val]{
	print key + " => " + val
}
```

{{out}}

```txt
one
two
three
surname => doe
age => 33
name => john

```



## AutoHotkey


```AutoHotkey
string = mary,had,a,little,lamb
Loop, Parse, string, `,
  MsgBox %A_LoopField%
```



## AWK

The <code>for (element_index in array)</code> can be used, but it does not give elements' indexes in the order inside the array (AWK indexes in array are indeed more like hashes).

```awk

BEGIN {
  split("Mary had a little lamb", strs, " ")
  for(el in strs) {
    print strs[el]
  }
}
```

If elements must be returned in some order, keys must be generated in that order.
In the example above the array is filled through the split function,
which uses indexes from 1.
So to iterate over the array's elements in the ''right'' order,
a normal loop can be used:

```awk
BEGIN {
  n = split("Mary had a little lamb", strs, " ")
  for(i=1; i <= n; i++) {
    print strs[i]
  }
}
```


Note that in awk, foreach loops can only be performed against an associative container.
It is not possible to loop against an explicit list, so the following will not work:


```awk
# This will not work
BEGIN {
  for (l in "apples","bananas","cherries") {
  print "I like " l
}
```



## BASIC


=
## BASIC256
=
BASIC-256 does not have a FOR EACH type statement.  Use a FOR loop to iterate through an array by index.

```BASIC256
DIM collection$(1)
collection$ = { "The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog." }

FOR i = 0 TO collection$[?]-1
   PRINT collection$[i]+ " ";
NEXT i
PRINT
```

Output:

```txt
The quick brown fox jumps over the lazy dog.
```


=
## BBC BASIC
=
{{works with|BBC BASIC for Windows}}

```bbcbasic
      DIM collection$(8)
      collection$() = "The", "quick", "brown", "fox", "jumps", \
      \               "over", "the", "lazy", "dog."

      FOR index% = 0 TO DIM(collection$(), 1)
        PRINT collection$(index%) " ";
      NEXT
      PRINT
```


=
## Commodore BASIC
=
Commodore BASIC too does not have a FOR-EACH construct. FOR loop is used to iterate through a string array by index. READ-DATA is used to fill up the string array

```qbasic
10 DIM A$(9) :REM DECLARE STRING ARRAY
20 REM *** FILL ARRAY WITH WORDS ***
30 FOR I = 0 TO 8
40 READ A$(I)
50 NEXT
60 REM *** PRINT ARRAY CONTENTS ***
70 FOR I = 0 TO 8
80 PRINT A$(I)" ";
90 NEXT
100 END
1000 DATA THE, QUICK, BROWN, FOX, JUMPS, OVER, THE, LAZY, DOG.
```


=
## Creative Basic
=

```Creative Basic
DEF AnArray[11]:INT

AnArray=0,1,2,3,4,5,6,7,8,9,10

'A console only program will work without OPENCONSOLE and
'CLOSECONSOLE; however, it does not hurt to use them.
OPENCONSOLE

FOR X=0 TO 10
    PRINT AnArray[X]
NEXT X

'keep the console from closing right away.
DO:UNTIL INKEY$<>""

CLOSECONSOLE

'because this is a console only program.
END

```


=
## FreeBASIC
=

```freebasic
' FB 1.05.0

' FreeBASIC doesn't have a foreach loop but it's easy to manufacture one using macros

#Macro ForEach(I, A)
For _i as integer = LBound(A) To UBound(A)
#Define I (A(_i))
#EndMacro

#Define In ,

Dim a(-5 To 5) As Integer = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}
ForEach(i in a)
  Print i; " ";
Next

Print
Sleep
```


{{out}}

```txt

 1  2  3  4  5  6  7  8  9  10  11

```


=
## Gambas
=
'''[https://gambas-playground.proko.eu/?gist=cb94500c68749f6f93915f3f10de5a03 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siInput As Short[] = [1, 8, 0, 6, 4, 7, 3, 2, 5, 9]
Dim siTemp As Short

For Each siTemp In siInput.Sort()
  Print siTemp;;
Next

End
```

{{out}}

```txt

0 1 2 3 4 5 6 7 8 9

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 STRING COLLECTION$(1 TO 9)*8
110 LET I=1
120 DO
130   READ IF MISSING EXIT DO:COLLECTION$(I)
140   LET I=I+1
150 LOOP
160 FOR J=1 TO I-1
170   PRINT COLLECTION$(J);" ";
180 NEXT
190 DATA The,quick,brown,fox,jumps,over,the,lazy,dog.
```


=
## IWBASIC
=
'''Linked List'''

```IWBASIC
DEF AList:POINTER

AList=ListCreate()

'Add items to the list.
DEF X:INT

FOR X=0 TO 10
    POINTER Temp=ListAdd(AList,NEW(INT,1))
    #<INT>temp=X
'The hash ("#") dereferencing operator is unique to IWBASIC and Creative Basic, and
'it is suitable for most basic pointer needs. IWBASIC also supports a "C style"
'dereferencing operator: "*". And that will work here too.
NEXT X

'A program compiled as console only does not need the commands to open and
'close the console. However, it does not hurt to use them.
OPENCONSOLE

'***Iterate the list with the "for each" loop***
FOR Temp=EACH AList AS INT
     PRINT #Temp
NEXT

PRINT

'A press any key to continue message is automatic in a program compiled as a console only
program. I presume the compiler inserts the code.
CLOSECONSOLE

'Because this is a console only program.
END
```


'''An Array'''

```IWBASIC
DEF AnArray[11]:INT

AnArray=0,1,2,3,4,5,6,7,8,9,10

OPENCONSOLE

FOR X=0 TO 10
	PRINT AnArray[X]
NEXT X

PRINT

'a press any key message is automatic when compiled as console only.
CLOSECONSOLE

'Because this is a console only program.
END

```


=
## Liberty BASIC
=
The most natural way is to use a csv list with a sentinel value.

```lb
in$      ="Not,Hardly,Just,Adequately,Quite,Really,Very,Fantastically,xyzzy"
element$ =""
i =1    '   used to point to successive elements

do
    element$ =word$( in$, i, ",")
    if element$ ="xyzzy" then exit do
    print element$; " good!"
    i =i +1
loop until 1 =2

end
```

 Not good!
 Hardly good!
 Just good!
 Adequately good!
 Quite good!
 Really good!
 Very good!
 Fantastically good!

==={{header|NS-HUBASIC}}===
<lang NS-HUBASIC>10 DIM A$(9) : REM DECLARE STRING ARRAY
20 REM ADD DATA TO ARRAY AND PRINT ARRAY CONTENTS
30 FOR I=0 TO 8
40 READ A$(I)
50 PRINT A$(I)" ";
60 NEXT
70 DATA THE, QUICK, BROWN, FOX, JUMPS, OVER, THE, LAZY, DOG.
```


=
## PureBasic
=
Works for LinkedLists and Maps

```PureBasic
ForEach element()
  PrintN(element())
Next
```


=
## Run BASIC
=

```runbasic
t$={Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday"

while word$(t$,i+1,",") <> ""
  i = i + 1
  print word$(t$,i,",")
wend
```


==={{header|TI-89 BASIC}}===

```ti89b
Local i,strs
Define strs = {"Lorem","ipsum","dolor"}
For i, 1, dim(strs)
  Disp strs[i]
EndFor
```


=
## VBA
=

```VB
Public Sub LoopsForeach()
    Dim FruitArray() As Variant
    Dim Fruit As Variant
    FruitArray = [{"Apple","Banana","Strawberry"}]
    Dim FruitBasket As Collection
    Set FruitBasket = New Collection
    For Each Fruit In FruitArray
        FruitBasket.Add Fruit
    Next Fruit
    For Each Fruit In FruitBasket
        Debug.Print Fruit
    Next Fruit
End Sub
```


=
## VBScript
=

```vbscript
items = Array("Apple", "Orange", "Banana")

For Each x In items
    WScript.Echo x
Next
```


=
## Visual Basic .NET
=

```vbnet
Dim list As New List(Of String)
list.Add("Car")
list.Add("Boat")
list.Add("Train")

For Each item In list
    Console.WriteLine(item)
Next
```



## Batch File

The FOR command can imitate the "Foreach Loop". The whitespace and the comma (,) are the default delimiters.


'''Direct usage:'''

```dos
@echo off
for %%A in (This is a sample collection) do (
     echo %%A
)
```

'''Using a Collection Variable:'''

```dos
@echo off
set "collection=This is a sample collection"
for %%A in (%collection%) do (
     echo %%A
)
```

{{Out|They have the Same Output}}

```txt
This
is
a
sample
collection
```



## bc

There is no "for each"-loop in bc. For accessing each element of an array (the only collection-like type) one uses a straightforward for-loop.

```bc
a[0] = .123
a[1] = 234
a[3] = 95.6
for (i = 0; i < 4; i++) {
    a[i]
}
```



## Bracmat

Bracmat has a more or less traditional 'while' loop (<code>whl'<i>expression</i></code>) which was introduced rather late in the history of Bracmat. Before that, tail recursion was a way to repeat something.
But let us make a list first:

```bracmat
  ( list
  =   Afrikaans
      Ελληνικά
      עברית
      മലയാളം
      ئۇيغۇرچە
  )
```

The 'while' solution. Use an auxiliary variable <code>L</code> that gets its head chopped off until nothing is left:

```bracmat
  !list:?L
& whl'(!L:%?language ?L&out$!language)
```

The tail-recursive solution. When the auxiliary variable is reduced to nothing, the loop fails. By adding the <code>~</code> flag to the initial invocation, failure is turned into success. This solution benefits from tail recursion optimization.

```bracmat
  !list:?L
& ( loop
  =   !L:%?language ?L
    & out$!language
    & !loop
  )
& ~!loop
```

A completely different way of iteration is by using a pattern that matches an element in the list, does something useful as a side effect and then fails, forcing bracmat to backtrack and try the next element in the list. The <code>@</code> flag matches at most one element. The <code>%</code> flag matches at least one element. Together they ensure that exactly one language name is assigned to the variable <code>language</code>. After all elements have been done, control is passed to the rhs of the <code>|</code> operator.

```bracmat
  (   !list
    : ? (%@?language&out$!language&~) ?
  |
  )
```



## C

C does not really have a native 'container' type, nor does it have a 'for each' type statement.  The following shows how to loop through an array and print each element.

```c
#include <stdio.h>
...

const char *list[] = {"Red","Green","Blue","Black","White"};
#define LIST_SIZE (sizeof(list)/sizeof(list[0]))

int ix;
for(ix=0; ix<LIST_SIZE; ix++) {
   printf("%s\n", list[ix]);
}
```


The C language does, however, have a number of standard data structures that can be thought of as collections, and foreach can easily be made with a macro.<BR><BR>

C string as a collection of char

```c

#include <stdio.h>
#include <stdlib.h>
/* foreach macro for using a string as a collection of char */
#define foreach( ptrvar , strvar ) char* ptrvar; for( ptrvar=strvar ; (*ptrvar) != '\0' ; *ptrvar++)

int main(int argc,char* argv[]){
char* s1="abcdefg";
char* s2="123456789";
foreach( p1 , s1 ) {
 printf("loop 1 %c\n",*p1);
}
foreach( p2 , s2 ){
 printf("loop 2 %c\n",*p2);
}
exit(0);
return(0);
}

```


C int array as a collection of int (array size known at compile-time)

```c

#include <stdio.h>
#include <stdlib.h>
int main(int argc,char* argv[]){
/* foreach macro viewing an array of int values as a collection of int values */
#define foreach( intpvar , intary ) int* intpvar; for( intpvar=intary; intpvar < (intary+(sizeof(intary)/sizeof(intary[0]))) ; intpvar++)
int a1[]={ 1 , 1 , 2 , 3 , 5 , 8 };
int a2[]={ 3 , 1 , 4 , 1, 5, 9 };
foreach( p1 , a1 ) {
 printf("loop 1 %d\n",*p1);
}
foreach( p2 , a2 ){
 printf("loop 2 %d\n",*p2);
}
exit(0);
return(0);
}

```


Most general: string or array as collection (collection size known at run-time)
: ''Note: idxtype can be removed and [http://gcc.gnu.org/onlinedocs/gcc/Typeof.html typeof(col&#91;0&#93;)] can be used in it's place with [[GCC]]''

```c

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int main(int argc,char* argv[]){
#define foreach( idxtype , idxpvar , col , colsiz ) idxtype* idxpvar; for( idxpvar=col ; idxpvar < (col+(colsiz)) ; idxpvar++)
#define arraylen( ary ) ( sizeof(ary)/sizeof(ary[0]) )
char* c1="collection";
int c2[]={ 3 , 1 , 4 , 1, 5, 9 };
double* c3;
int c3len=4;
c3=(double*)calloc(c3len,sizeof(double));
c3[0]=1.2;c3[1]=3.4;c3[2]=5.6;c3[3]=7.8;
foreach( char,p1   , c1, strlen(c1) ) {
 printf("loop 1 : %c\n",*p1);
}
foreach( int,p2    , c2, arraylen(c2) ){
 printf("loop 2 : %d\n",*p2);
}
foreach( double,p3 , c3, c3len ){
 printf("loop 3 : %3.1lf\n",*p3);
}
exit(0);
return(0);
}

```



## C++

C++03 did not have a "for each" loop. The following is a generic loop which works with any standard container except for built-in arrays. The code snippet below assumes that the container type in question is typedef'd to <tt>container_type</tt> and the actual container object is named container.

```cpp
for (container_type::iterator i = container.begin(); i != container.end(); ++i)
{
  std::cout << *i << "\n";
}
```

However the idiomatic way to output a container would be

```cpp
std::copy(container.begin(), container.end(),
          std::ostream_iterator<container_type::value_type>(std::cout, "\n"));
```

There's also an algorithm named <tt>for_each</tt>. However, you need a function or function object to use it, e.g.

```cpp
void print_element(container_type::value_type const& v)
{
  std::cout << v << "\n";
}

...
  std::for_each(container.begin(), container.end(), print_element);
```

{{works with|C++11}}

```cpp
for (auto element: container)
{
  std::cout << element << "\n";
}
```

Here <tt>container</tt> is the container variable, <tt>element</tt> is the loop variable (initialized with each container element in turn), and auto means that the compiler should determine the correct type of that variable automatically. If the type is expensive to copy, a const reference can be used instead:

```cpp
for (auto const& element: container)
{
  std::cout << element << "\n";
}
```

Of course the container elements can also be changed by using a non-const reference (provided the container isn't itself constant):

```cpp
for (auto&& element: container) //use a 'universal reference'
{
  element += 42;
}
```


=={{header|C sharp|C#}}==

```csharp
string[] things = {"Apple", "Banana", "Coconut"};

foreach (string thing in things)
{
    Console.WriteLine(thing);
}
```



## Chapel


```chapel
var food = ["Milk", "Bread", "Butter"];
for f in food do writeln(f);
```



## Clojure


```lisp
(doseq [item collection] (println item))
```



## CMake


```cmake
set(list one.c two.c three.c)

foreach(file ${list})
  message(${file})
endforeach(file)
```



## COBOL

The following is in the Managed COBOL dialect:
{{trans|C#}}
{{works with|Visual COBOL}}

```cobol
01  things occurs 3.
...
set content of things to ("Apple", "Banana", "Coconut")
perform varying thing as string through things
    display thing
end-perform
```



## ColdFusion


```cfm

<Cfloop list="Fee, Fi, Foe, Fum" index="i">
  <Cfoutput>#i#!</Cfoutput>
</Cfloop>

```



## Common Lisp


```lisp
(loop for i in list do (print i))
```

or

```lisp
(map nil #'print list)
```




## D

This works if ''collection'' is a string/array/associative array, or if implements an appropriate ''opApply'' function, or if it has the basic Range methods.

```d
import std.stdio: writeln;

void main() {
    auto collection1 = "ABC";
    foreach (element; collection1)
        writeln(element);

    auto collection2 = [1, 2, 3];
    foreach (element; collection1)
        writeln(element);

    auto collection3 = [1:10, 2:20, 3:30];
    foreach (element; collection3)
        writeln(element);

    foreach (key, value; collection3)
        writeln(key, " ", value);
}
```

{{out}}

```txt
A
B
C
A
B
C
10
20
30
1 10
2 20
3 30
```



## Dao


```dao
items = { 1, 2, 3 }
for( item in items ) io.writeln( item )
```



## Delphi

for..in loops were added in Delphi 2005.

Supports arrays (single, multidimensional, and dynamic), sets, strings, collections and any class or interface that implements GetEnumerator().

```Delphi
program LoopForEach;

{$APPTYPE CONSOLE}

var
  s: string;
begin
  for s in 'Hello' do
    Writeln(s);
end.
```

Output:

```txt
H
e
l
l
o
```



## Dragon


```dragon

for (value : array) {
   showln array
}

```



## Dyalect


```Dyalect
for i in [1,2,3] {
   print(i)
}
```


This code would work for any type that has a method "iter" (returning an iterator). In fact a runtime environment silently calls this method for you here and creates an iterator out of an array. The code above is absolutely identical to:


```Dyalect
for i in [1,2,3].iter() {
   print(i)
}
```


This would perfectly work with any custom iterator as well:


```Dyalect
func myCollection() {
    yield 1
    yield 2
    yield 3
}

for i in myCollection() {
   print(i)
}
```


All three code samples would output:


```txt
1
2
3
```



## E


```e
for e in theCollection {
    println(e)
}
```

In E, the for ... in ... loop is also used for iterating over numeric ranges; see [[Loop/For#E]].


## EchoLisp


```scheme

(define my-list '( albert simon antoinette))
(for ((h my-list)) (write h))
    albert simon antoinette

(define my-vector #(55 66 soixante-dix-sept))
(for (( u my-vector)) (write u))
    55 66 soixante-dix-sept

(define my-string "Longtemps")
(for ((une-lettre my-string)) (write une-lettre))
    "L" "o" "n" "g" "t" "e" "m" "p" "s"

;; etc ... for other collections like Streams, Hashes, Graphs, ...

```



## Efene

Any data structure can be printed as a whole, preformated:

```efene
io.format("~p~n", [Collection])
```

However, to iterate over each element of a list, Efene uses <tt>lists.map/2</tt>, except in the case of IO where <tt>lists.foreach/2</tt> has to be used as the evaluation order is defined to be the same as the order of the elements in the list.

```efene
lists.foreach(fn (X) { io.format("~p~n", [X]) }, Collection)
```



## Eiffel

{{works with|EiffelStudio|6.6 beta (with provisional loop syntax)}}
The iteration (foreach) form of the Eiffel loop construct is introduced by the keyword <code lang="eiffel">across</code>.

```eiffel
            across my_list as ic loop print (ic.item) end
```

The local entity <code lang="eiffel">ic</code> is an instance of the library class <code lang="eiffel">ITERATION_CURSOR</code>. The cursor's feature <code lang="eiffel">item</code> provides access to each structure element. Descendants of class <code lang="eiffel">ITERATION_CURSOR</code> can be created to handle specialized iteration algorithms. The types of objects that can be iterated across (<code lang="eiffel">my_list</code> in the example) are based on classes that inherit from the library class <code lang="eiffel">ITERABLE</code>

### Boolean expression variant

The iteration form of the Eiffel loop can also be used as a boolean expression when the keyword <code lang="eiffel">loop</code> is replaced by either <code lang="eiffel">all</code> (effecting [[wp:Universal quantification|universal quantification]]) or <code lang="eiffel">some</code> (effecting [[wp:Existential quantification|existential quantification]]).

This iteration is a boolean expression which is true if all items in <code>my_list</code> have counts greater than three:

```eiffel
            across my_list as ic all ic.item.count > 3 end
```

Whereas, the following is true if at least one item has a count greater than three:

```eiffel
            across my_list as ic some ic.item.count > 3 end
```



## Ela


```ela
open monad io

each [] = do return ()
each (x::xs) = do
  putStrLn $ show x
  each xs
```



## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;

public program()
{
    var things := new string[]::("Apple", "Banana", "Coconut");

    things.forEach:(thing)
    {
        console.printLine:thing
    }
}
```



## Elixir


```elixir
iex(1)> list = [1,3.14,"abc",[3],{0,5}]
[1, 3.14, "abc", [3], {0, 5}]
iex(2)> Enum.each(list, fn x -> IO.inspect x end)
1
3.14
"abc"
[3]
{0, 5}
:ok
```



## Emacs Lisp

For a list either <code>dolist</code> macro


```Lisp
(dolist (x '(1 2 3 4))
  (message "x=%d" x))
```


or <code>mapc</code> function


```Lisp
(mapc (lambda (x)
        (message "x=%d" x))
      '(1 2 3 4))
```


<code>dolist</code> and <code>mapc</code> are both builtin in current Emacs.  For past Emacs both can be had from <code>cl.el</code> with for instance <code>(eval-when-compile (require 'cl))</code>.

<code>cl.el</code> also offers a <code>loop</code> macro similar in style to [[#Common Lisp|Common Lisp]].



## Erlang

Any data structure can be printed as a whole, preformated:

```erlang
io:format("~p~n",[Collection]).
```

However, to iterate over each element of a list, Erlang uses <tt>lists:map/2</tt>, except in the case of IO where <tt>lists:foreach/2</tt> has to be used as the evaluation order is defined to be the same as the order of the elements in the list.

```erlang
lists:foreach(fun(X) -> io:format("~p~n",[X]) end, Collection).
```




## ERRE

It's an extension of 'standard' FOR loop: constant list must be explicit.

```ERRE

      FOR INDEX$=("The","quick","brown","fox","jumps","over","the","lazy","dog.") DO
        PRINT(INDEX$;" ";)
      END FOR
      PRINT

```



## Euphoria

{{works with|OpenEuphoria}}
<lang>
include std/console.e

sequence s = {-2,-1,0,1,2}  --print elements of a numerical list
for i = 1 to length(s) do
	? s[i]
end for

puts(1,'\n')

s = {"Name","Date","Field1","Field2"} -- print elements of a list of 'strings'
for i = 1 to length(s) do
	printf(1,"%s\n",{s[i]})
end for

puts(1,'\n')

for i = 1 to length(s) do  -- print subelements of elements of a list of 'strings'
	for j = 1 to length(s[i]) do
		printf(1,"%s\n",s[i][j])
	end for
	puts(1,'\n')
end for

if getc(0) then end if

```

{{out}}

```txt

-2
-1
0
1
2

Name
Date
Field1
Field2

N
a
m
e

D
a
t
e

F
i
e
l
d
1

F
i
e
l
d
2

```



## Factor


```factor
{ 1 2 4 } [ . ] each
```



## Fantom

Use <code>each</code> method to iterate over a collection of items in a <code>List</code>.

```fantom
class Main
{
  public static Void main ()
  {
    Int[] collection := [1, 2, 3, 4, 5]
    collection.each |Int item|
    {
      echo (item)
    }
  }
}
```



## friendly interactive shell

Unlike, bash or csh, the PATH variable is automatically converted to real array.

```fishshell
for path in $PATH
    echo You have $path in PATH.
end
```


Sample output:

```txt

You have /bin in PATH.
You have /usr/bin in PATH.

```



## Fortran


```fortran
program main

 implicit none

 integer :: i
 character(len=5),dimension(5),parameter :: colors = ['Red  ','Green','Blue ','Black','White']

 !using a do loop:
 do i=1,size(colors)
   write(*,'(A)') colors(i)
 end do

 !this will also print each element:
 write(*,'(A)') colors

end program main
```



## Forth


```forth
create a 3 , 2 , 1 ,
: .array ( a len -- )
  cells bounds do  i @ .  cell +loop ;     \ 3 2 1
```




## Frink

Frink's <CODE>for</CODE> loop is actually a "for each" loop which can iterate over built-in collection types including arrays, sets, dictionaries, enumerating expressions, and Java types such as Map, Iterator, Enumeration, etc.

```frink

array = [1, 2, 3, 5, 7]
for n = array
   println[n]

```


=={{header|F_Sharp|F#}}==
We can use ''for'' directly or list iteration.

```fsharp
for i in [1 .. 10] do printfn "%d" i

List.iter (fun i -> printfn "%d" i) [1 .. 10]
```



## GAP



```gap
for p in AlternatingGroup(4) do
    Print(p, "\n");
od;

()
(1,3,2)
(1,2,3)
(1,4,3)
(2,4,3)
(1,3)(2,4)
(1,2,4)
(1,4)(2,3)
(2,3,4)
(1,3,4)
(1,2)(3,4)
(1,4,2)
```



## Go

<code>range</code> works with all of the built-in container-types. With one variable (''i''), it gives you the key/index of every item. With two variables (''i'', ''x''), it gives you both the key/index and value/item. For channels, only the single-variable variant is allowed.

```go
func printAll(values []int) {
   for i, x := range values {
      fmt.Printf("Item %d = %d\n", i, x)
   }
}
```



## Groovy

"for" loop:

```groovy
def beatles = ["John", "Paul", "George", "Ringo"]

for(name in beatles) {
    println name
}
```

"each()" method:

Though technically not a loop, most Groovy programmers would use the somewhat more terse "each()" method on the list itself in preference to the "for" loop construct.

```groovy
beatles.each {
    println it
}
```

Output (same for either):

```txt
John
Paul
George
Ringo
```



## Halon


```halon
$things = ["Apple", "Banana", "Coconut"];

foreach ($things as $thing) {
    echo $thing;
}
```



## Haskell


```haskell
import Control.Monad (forM_)
forM_ collect print
```

which is the same as

```haskell
mapM_ print collect
```



## Haxe


```haxe
var a = [1, 2, 3, 4];

for(i in a)
    Sys.println(i);
```



## HicEst


```hicest
CHARACTER days="Monday Tuesday Wednesday Thursday Friday Saturday Sunday "

items = INDEX(days, ' ', 256)          ! 256 = count option
DO j = 1, items
  EDIT(Text=days, ITeM=j, Parse=today)
  WRITE() today
ENDDO
```



## Hy


```clojure
(for [x collection] (print x))
```



## Io


```io
collection foreach(println)
```


=={{header|Icon}} and {{header|Unicon}}==
The example below X can be a list, string, table or other data type.

```Icon
procedure main()
X := [1,2,3,-5,6,9]
every x := !L do
   write(x)
end
```

This loop can be written somewhat more concisely as:

```Icon
every write(!L)
```




## J


```J>smoutput each i.10</lang



## Java

{{works with|Java|1.5+}}

```java>Iterable<Type
 collect;
...
for(Type i:collect){
   System.out.println(i);
}
```

This works for any array type as well as any type that implements the Iterable interface (including all Collections).

{{works with|Java|1.8+}}

```java

Iterable collect;
...
collect.forEach(o -> System.out.println(o));

```

This works with any Iterable, but not with arrays.


## JavaScript


For arrays in ES5, we can use '''Array.forEach()''':


```JavaScript
"alpha beta gamma delta".split(' ').forEach(
  function (x) {
    console.log(x);
  }
);
```


Output:

```txt
alpha
beta
gamma
delta
```


though it will probably be more natural – dispensing with side-effects, and allowing for easier composition of nested functions – to simply use '''Array.map()''',


```JavaScript
console.log("alpha beta gamma delta".split(' ').map(
  function (x) {
    return x.toUpperCase(x);
  }
).join('\n'));
```


Output:


```txt
ALPHA
BETA
GAMMA
DELTA
```


or, more flexibly, and with greater generality, obtain an accumulating fold from '''Array.reduce()'''


```JavaScript
console.log(
  "alpha beta gamma delta".split(' ').reduce(
    function (a, x, i, lst) {
      return lst.length - i + '. ' + x + '\n' + a;
    }, ''
  )
)
```


Output:


```txt
1. delta
2. gamma
3. beta
4. alpha
```


More generally, the following works for any object, including an array.  It iterates over the keys of an object.

```JavaScript
for (var a in o) {
    print(o[a]);
}
```

However, it has the often unwanted feature that it lists inherited properties and methods of objects as well as the ones directly set on the object -- consider whether to filter out such properties inside the loop, for example:

```JavaScript
for (var a in o) {
    if (o.hasOwnProperty(a)) {
        print(o[a]);
    }
}
```


{{works with|JavaScript|1.6}}
;Deprecated
There is also a <tt>[https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Statements/for_each...in for each in]</tt> construct that iterates over the values of an object:

```JavaScript
h = {"one":1, "two":2, "three":3}
for (x in h) print(x);
/*
two
one
three
*/

for each (y in h) print(y);
/*
2
1
3
*/
```


{{works with|ECMAScript|6th edition}}
There is also a <tt>[https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Statements/for...of for of]</tt> construct that iterates over the values of an object:

```JavaScript
h = {"one":1, "two":2, "three":3}
for (x in h) print(x);
/*
two
one
three
*/

for (y of h) print(y);
/*
2
1
3
*/
```



## Jsish

Jsi supports ''for of'' for looping over element of an array.

```javascript
for (str of "alpha beta gamma delta".split(' ')) { puts(str); }
```


{{out}}

```txt
alpha
beta
gamma
delta
```



## Julia

{{trans|Python}}

```julia
for i in collection
   println(i)
end
```

The Julia <code>for</code> statement is always a "foreach", and the built-in <code>start:end</code> or <code>start:step:end</code> "range" syntax can be used for iteration over arithmetic sequences. Many Julia objects support iteration: arrays and tuples iterate over each item, strings iterate over each character, dictionaries iterate over (key,value) pairs, numeric scalars provide a length-1 iteration over their value, and so on.

## jq

'''Iterables''':

In this section, the array defined by "example" is used as an example:

```jq
def example: [1,2];
```

jq has two types of iterables -- JSON arrays and JSON objects. In both cases, the ".[]" filter may be used to iterate through the values, it being understand that for objects, the "values" are the values associated with the keys:

```jq
example | .[]
# or equivalently: example[]
```


```jq
{"a":1, "b":2} | .[]
# or equivalently: {"a":1, "b":2}[]
```


In both cases, the output is the stream consisting of the values 1 followed by 2.

Sometimes it is necessary to use an alternative to ".[]". For example, one might want to generate an index along with the array elements. In such cases, the "range(m;n)" generator, which performs a similar role to C's "for(i=m; i<n; i++)", can be used for array.  Here is how range/2 would be used to perform the task for an array:

```jq
example | . as $a | range(0; length) | $a[.]
```

For JSON objects, the corresponding technique involves using <tt>keys</tt>, e.g.

```jq

 {"a":1, "b":2} | . as $o | keys | map( [., $o[.]] )

```

produces:
 [["a",1],["b",2]]


'''Strings''':

To convert the constituent characters (or technically, codepoints) of a string into a stream of values, there are two techniques illustrated by these examples:

```jq
"abc" | . as $s | range(0;length) | $s[.:.+1]

"abc" | explode | map( [.]|implode) | .[]
```


In both cases, the result is the stream of values: "a", "b", "c".


## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    val greek = arrayOf("alpha", "beta", "gamma", "delta")
    for (letter in greek) print("$letter ")
    println()
    // or alternatively
    greek.forEach { print("$it ") }
    println()
}
```


{{out}}

```txt

alpha beta gamma delta
alpha beta gamma delta

```



## Lasso


```Lasso
array(1,2,3) => foreach { stdoutnl(#1) }
```


```Lasso
with i in array(1,2,3) do { stdoutnl(#i) }
```



## K


```K
   {`0:$x} ' !10
```


```K
   _sin ' (1; 2; 3;)
```



## LFE



```lisp
(lists:foreach
  (lambda (x)
    (io:format "item: ~p~n" (list x)))
  (lists:seq 1 10))

```



## LabVIEW

[[LabVIEW]] has a feature known as an Auto-Indexed Tunnel. It is the very small orange box on the lower left of the for loop.<br/>{{VI solution|LabVIEW_Loops_Foreach.png}}


## Lang5


```lang5>: >
say.(*) . ;
5 iota >>say.
```



## LIL


```tcl
# Loops/Foreach, in LIL
set collection [list 1 2 "three"]
append collection [list 4 5 six]  # appended as a single item in collection
print "Collection is: $collection"

# using default "i" variable name set for each item
foreach $collection {print $i}

# user named variable in the steps, retrieving accumulated result of loop
# each loop step quotes two copies of the item
set newlist [foreach j $collection {quote $j $j}]
print "Result of second foreach: $newlist"
```


{{out}}

```txt
prompt$ lil loopsForeach.lil
Collection is: 1 2 three {4 5 six}
1
2
three
4 5 six
Result of second foreach: {1 1} {2 2} {three three} {4 5 six 4 5 six}
```



## Lingo



```lingo
days = ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"]
repeat with day in days
  put day
end repeat
```

A callback-based forEach() can be implemented like this:

```lingo
----------------------------------------
-- One of the five native iterative methods defined in ECMAScript 5
-- @param {list} tList
-- @param {symbol} cbFunc
-- @param {object} [cbObj=_movie]
----------------------------------------
on forEach (tList, cbFunc, cbObj)
  if voidP(cbObj) then cbObj = _movie
  cnt = tList.count
  repeat with i = 1 to cnt
    call(cbFunc, cbObj, tList[i], i, tList)
  end repeat
end
```


```lingo
days = ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"]
forEach(days, #alert, _player)
```



## Lisaac


```Lisaac
"Lisaac loop foreach".split.foreach { word : STRING;
  word.print;
  '\n'.print;
};
```



## LiveCode

Livecode's ''for each'' operates on chunks which may be words, items, lines, tokens. Example is for items.
```LiveCode
repeat for each item x in "red, green, blue"
    put x & cr
    --wait 100 millisec  -- req'd if you want to see in the LC Message Box (akin to repl)
end repeat
```



## Logo


```logo
foreach [red green blue] [print ?]
```



## Lua

Lua has 2 built-in iterators over tables.

<code>pairs()</code> iterates over all entries in a table, but in no particular order:

```lua
t={monday=1, tuesday=2, wednesday=3, thursday=4, friday=5, saturday=6, sunday=0, [7]="fooday"}
for key, value in pairs(t) do
  print(value, key)
end
```

Output:

```txt

0	sunday
fooday	7
2	tuesday
3	wednesday
5	friday
4	thursday
6	saturday
1	monday

```

<code>ipairs()</code> iterates over table entries with positive integer keys,
and is used to iterate over lists in order.

```lua
l={'monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday', not_a_number='fooday', [0]='today', [-1]='yesterday' }
for key, value in ipairs(l) do
  print(key, value)
end
```

Output:

```txt

1	monday
2	tuesday
3	wednesday
4	thursday
5	friday
6	saturday
7	sunday

```

Note that <code>ipairs()</code> ignores non-numeric and non-positive integer keys.


## M2000 Interpreter

Iterators we have for Arrays, Inventories and Stacks (containers). Each(object Start to End), or Each(object 1 to -1) or Each(Object, 1,-1). We can use -2 for second from end item. We can use step inside while iterator {} using Iterator=Each(object, new_start, end_item). We can read cursor using ^. So Print k^, k2^, k1^ return positions (from 0 for inventories). We can use more than one iterators for an object.


```M2000 Interpreter

Module Checkit {
      \\ Inventories may have keys or keys/values
      \\ here keys are values too
      Inventory Alfa="Apple", "Banana", "Coconut"
      \\ key 30 has value 25, other keys have value same as key.
      Inventory Beta=100, 30:=25, 20, 5
      Print "Parallel"
      k=Each(Alfa)
      k1=Each(Alfa End to Start)
      k2=Each(Beta)
      \\ Parallel iterators
      \\ when one of them end then while end too.
      \\ so 5 not printed. Print 100, 25, 20
      While k,k2, k1 {
            Print Eval$(k), Eval$(k1), Eval(k2)
      }
      Print "Nested"
      \\ Nested iterators
      k=Each(Alfa)
      While k {
      k1=Each(Alfa End to Start)
            While k1 {
                  Print Eval$(k), Eval$(k1)
            }
      }
}
Checkit

```



## Maple


```Maple

for p in [2, 3, 5, 7] do
    print(p);
end do;

```



## Mathematica

Foreach over list of strings

```mathematica
s = (StringSplit@Import["ExampleData/USConstitution.txt"])[[1;;7]];
Do[
 Print@i,
 {i, s}
]
```

Output:

```txt
We
the
People
of
the
United
States,
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
    list1 = [1,5,6,7,-7,-9];
    for k = list1,    % list1 must be a row vector (i.e. array of size 1xn)
        printf('%i\n',k)
    end;
```


```Matlab
    list2 = {'AA','BB','CC'};
    for k = list2,    % list2 must be a row vector (i.e. array of size 1xn)
        printf('%s\n',k{1})
    end;
```

A vectorized version of the code is

```Matlab
  printf('%d\n',list1);
  printf('%s\n',list2{:});
```



## Maxima


```maxima
for n in [2, 3, 5, 7] do print(n);
```



## MAXScript


```maxscript

arr = for i in 1 to 50 collect ("Number: " + (random 10 99) as string)
makeuniquearray arr
sort arr

for i in arr do print i as string

```



## Metafont

If we have a list of arbitrary items, we can simply use for:

```metafont
for x = "mary", "had", "a", "little", "lamb": message x; endfor
end
```

The list can be generated in place by any suitable macro or another loop... e.g. let us suppose we have things like <tt>a[n]</tt> defined (with maximum n being 10). Then

```metafont
for x = for i = 1 upto 9: a[i], endfor, a[10]: show x; endfor
end
```

works more like a <tt>foreach</tt>; we could make a macro to hide the strangeness of such a code.


## min

{{works with|min|0.19.3}}

```min
(1 2 3) 'puts foreach
```



## MiniScript



```MiniScript
for i in collection
   print i
end
```

The MiniScript <code>for</code> statement is always a "foreach", and the standard <code>range</code> intrinsic can be used for iteration over arithmetic sequences. All MiniScript collection types support iteration: lists iterate over each item, strings iterate over each character, and maps/objects iterate over (key,value) pairs.


## MOO


```moo
things = {"Apple", "Banana", "Coconut"};

for thing in (things)
    player:tell(thing);
endfor
```



## Nemerle

This works on anything which implements the IEnumerable interface.

```Nemerle
def things = ["Apple", "Banana", "Coconut"];

foreach (thing in things) WriteLine(thing.ToLower());
foreach (i in [5, 10 .. 100]) Write($"$i\t");
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

  say
  say 'Loops/Foreach'

  days = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday']
  daysl = Arrays.asList(days)
  daysi = daysl.iterator

  loop while daysi.hasNext
    say daysi.next
    end
```



## NewLISP


```NewLISP
(map println '(Apple Banana Coconut))
```



## Nim


```nim
var list: seq[string] = @[]
list.add("lorem")
list.add("ipsum")
list.add("dolor")
for i in items(list):
  echo(i)
```

Output:

```txt

lorem
ipsum
dolor

```


=={{header|Objective-C}}==
{{works with|Objective-C|2.0+}}
{{works with|GNUstep}}
{{works with|Cocoa}}

```objc
NSArray *collect;
//...
for(Type i in collect){
   NSLog(@"%@", i);
}
```

''collect'' can be any object that adopts the NSFastEnumeration protocol.

Or (always using OpenStep compatible frameworks):
{{works with|Objective-C|<2.0}}

```objc
NSArray *collect;
//...
NSEnumerator *enm = [collect objectEnumerator];
id i;
while( (i = [enm nextObject]) ) {
  // do something with object i
}
```



## Objeck


```objeck
fruits := ["Apple", "Banana", "Coconut"];
each(i : fruits) {
  fruits[i]->PrintLine();
};
```



## OCaml

List of integers:

```ocaml
List.iter
  (fun i -> Printf.printf "%d\n" i)
  collect_list
```

Array of integers:

```ocaml
Array.iter
  (fun i -> Printf.printf "%d\n" i)
  collect_array
```



## Oforth



```Oforth
: printMonths | m | Date.Months forEach: m [ m . ] ;
```


But, apply can be used instead of a loop :

```Oforth>#. Date.Months apply</lang



## Octave


```octave
a = [ 1,4,3,2 ];
b = [ 1,2,3,4; 5,6,7,8 ];
for v = a
  disp(v); % output single values: 1,4,3,2
endfor
for v = b
  disp(v); % v is the column vector [1;5], then [2;6] ...
endfor
```

We can also iterate over structures:

```octave
x.a = [ 10, 11, 12 ];
x.b = { "Cell", "ul", "ar" };
for [ val, key ] = x
  disp(key);
  disp(val);
endfor
```



## ooRexx

The <tt>OVER</tt> loop control keyword is used to select each item in a collection in turn.
Open Object Rexx allows the <tt>DO</tt> block structure keyword to be used to start a loop for backward compatibility with classic Rexx; the <tt>LOOP</tt> keyword is preferred here as it is self-documenting.

```ooRexx
/* Rexx */
say
say 'Loops/Foreach'
out = ''

days = .array~of('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')

loop daysi over days
  out ||= daysi' '
  end daysi
say out~strip()

exit
```

'''Output:'''

```txt

Loops/Foreach
Sunday Monday Tuesday Wednesday Thursday Friday Saturday

```



## Oz


```oz
declare
  MyList = [1 2 3 4]
in
  {ForAll MyList Show}

  %% or:
  for E in MyList do {Show E} end
```



## PARI/GP


```parigp
for(i=1,#v,print(v[i]))
```


or (PARI/GP >= 2.4)


```parigp
apply(x->print(x),v)
```



## Pascal

See [[Loops/Foreach#Delphi | Delphi]]


## Perl


```perl
foreach my $i (@collection) {
   print "$i\n";
}
```

The keyword <code>for</code> can be used instead of <code>foreach</code>. If a loop variable (here <code>$i</code>) is not given, then <code>$_</code> is used.

A more compact notation using perl statement modifier:

```perl
print "$_\n"  foreach @collection
```


In perl, it is possible to loop against an explicit list, so there is no need to define a container:


```perl
foreach $l ( "apples", "bananas", "cherries" ) {
  print "I like $l\n";
}
```



## Perl 6

{{works with|Rakudo|2015.10-40}}

```perl6
say $_ for @collection;
```

Perl 6 leaves off the <tt>each</tt> from <tt>foreach</tt>, leaving us with <tt>for</tt> instead. The variable <tt>$_</tt> refers to the current element, unless you assign a name to it using <tt>-></tt>.

```perl6
for @collection -> $currentElement { say $currentElement; }
```

Perl 6 will do it's best to put the topic at the right spot.

```perl6
.say for @collection;
for @collection { .say };
```

Iteration can also be done with hyperoperators. In this case it's a candidate for autothreading and as such, execution order may vary. The resulting list will be in order.

```per6>@collection>
.say;
@collection>>.=&infix:<+>(2); # increment each element by 2
```



## Phix


```Phix
sequence s = {-2,"field",3.14159268979,{"this","that"}}
for i=1 to length(s) do
    ?s[i]
end for
```



## PHL


```phl
var numbers = 1..10;

numbers each # (number) [
    printf("%i\n", number);
];
```



## PHP


```php
foreach ($collect as $i) {
   echo "$i\n";
}

foreach ($collect as $key => $i) {
   echo "\$collect[$key] = $i\n";
}
```

<code>foreach</code> can also iterate over objects. By default it iterates over all visible fields of an object.


## PicoLisp


```PicoLisp
(mapc println '(Apple Banana Coconut))
```



## Pike


```pike
int main(){
   array(int|string) collect = ({109, "Hi", "asdf", "qwerty"});
   foreach(collect, int|string elem){
      write(elem + "\n");
   }
}
```

Iterating over the keys and values of a mapping (dictionary):

```pike
int main(){
    mapping(string:string) coll = (["foo":"asdf", "bar":"qwer", "quux":"zxcv"]);
    foreach (coll;string key;string val)
        write(key+" --> "+val+"\n");
    }
}
```



## PL/I


```pli
declare A(10) fixed binary;
do i = lbound(A,1) to hbound(A,1);
   put skip list (A(i));
end;
```



## Pop11

Iteration over list:

```pop11
lvars el, lst = [1 2 3 4 foo bar];
for el in lst do
   printf(el,'%p\n');
endfor;
```



## PostScript

The <code>forall</code> operator performs a loop over a collection (array, string or dictionary). Strings and arrays can be treated very much the same:

```postscript
[1 5 3 2] { = } forall
(abc) { = } forall
```

but dictionaries take a little more work since a key/value pair is pushed on the stack in each iteration:

```postscript><</a 25 /b 42>
 {
  exch (Key: ) print
  =
  (Value: ) print
  =
} forall
```



## PowerShell


```PowerShell

$colors = "Black","Blue","Cyan","Gray","Green","Magenta","Red","White","Yellow",
          "DarkBlue","DarkCyan","DarkGray","DarkGreen","DarkMagenta","DarkRed","DarkYellow"

foreach ($color in $colors)
{
    Write-Host "$color" -ForegroundColor $color
}

```

{{Out}}

```txt

Black
Blue
Cyan
Gray
Green
Magenta
Red
White
Yellow
DarkBlue
DarkCyan
DarkGray
DarkGreen
DarkMagenta
DarkRed
DarkYellow

```



## Prolog

For example :

```Prolog
?- foreach(member(X, [red,green,blue,black,white]), writeln(X)).
red
green
blue
black
white
true.

```




## Python


```python
for i in collection:
   print i
```


Note: The Python <code>for</code> statement is always a ''"foreach" ...'' and the <code>range()</code> and <code>xrange()</code> built-in functions are used to generate lists of indexes over which it will iterate as necessary.  The majority of Python objects support iteration.  Lists and tuples iterate over each item, strings iterate over each character, dictionaries iterate over keys, files iterate over lines, and so on.

For example:

```python
lines = words = characters = 0
f = open('somefile','r')
for eachline in f:
    lines += 1
    for eachword in eachline.split():
        words += 1
        for eachchar in eachword:
            characters += 1

print lines, words, characters
```


Whether <code>for</code> loops over the elements of the collection in order depends on the collection having an inherent order or not. Elements of strings (i.e. characters), tuples and lists, for example, are ordered but the order of elements in dictionaries and sets is not defined.

One can loop over the key/value pairs of a dictionary in ''alphabetic'' or ''numeric'' key order by sorting the sequence of keys, provided that the keys are all of ''comparable'' types. In Python 3.x a sequence of mixed numeric and string elements is not sortable (at least not with the default invocation of <code>sorted()</code>), whereas in Python 2.x numeric types are sorted according to their string representation by default:


```python
d = {3: "Earth", 1: "Mercury", 4: "Mars", 2: "Venus"}
for k in sorted(d):
    print("%i: %s" % (k, d[k]))

d = {"London": "United Kingdom", "Berlin": "Germany", "Rome": "Italy", "Paris": "France"}
for k in sorted(d):
    print("%s: %s" % (k, d[k]))
```


{{works with|Python|2.x}}


```python
d = {"fortytwo": 42, 3.14159: "pi", 23: "twentythree", "zero": 0, 13: "thirteen"}
for k in sorted(d):
    print("%s: %s" % (k, d[k]))
```



## R


```R
a <- list("First", "Second", "Third", 5, 6)
for(i in a) print(i)
```



## Racket



```racket

#lang racket

;; an example sequence
(define sequence '("something" 1 2 "foo"))

;; works for any sequence
(for ([i sequence])
  (displayln i))

```



## REBOL


```REBOL
REBOL [
	Title: "Loop/Foreach"
	URL: http://rosettacode.org/wiki/Loop/Foreach
]

x: [Sork Gun Blues Neds Thirst Fright Catur]

foreach i x [prin rejoin [i "day "]]  print ""

; REBOL also has the 'forall' construct, which provides the rest of
; the list from the current position.

forall x [prin rejoin [x/1 "day "]]  print ""
```

Output:

```txt
Sorkday Gunday Bluesday Nedsday Thirstday Frightday Caturday
Sorkday Gunday Bluesday Nedsday Thirstday Frightday Caturday
```



## Red


```Red>>
 blk: ["John" 23 "dave" 30 "bob" 20 "Jeff" 40]
>> foreach item blk [print item]
John
23
dave
30
bob
20
Jeff
40
>>  foreach [name age] blk [print [name "is" age "years old"]]
John is 23 years old
dave is 30 years old
bob is 20 years old
Jeff is 40 years old

>> forall blk [print blk]
John 23 dave 30 bob 20 Jeff 40
23 dave 30 bob 20 Jeff 40
dave 30 bob 20 Jeff 40
30 bob 20 Jeff 40
bob 20 Jeff 40
20 Jeff 40
Jeff 40
40
```



## Retro

Retro has '''for-each''' combinators for operating on elements of various data structures.


```Retro
# Strings

This will display the ASCII code for each character in a string

~~~
'This_is_a_message [ n:put sp ] s:for-each
~~~

# Array

Display each element

~~~
{ #1 #2 #3 }  [ n:put sp ] a:for-each
~~~

# Linked List

Using the dictionary as an example, display each name

~~~
&Dictionary [ d:name s:put sp ] d:for-each
~~~

```



## REXX


```rexx
days = 'zuntik montik dinstik mitvokh donershtik fraytik shabes'

  do j=1  for words(days)              /*loop through days of the week. */
  say word(days,j)                     /*display the weekday to screen. */
  end   /*j*/
                                       /*stick a fork in it, we're done.*/
```



## Ring


```ring

aList = "Welcome to the Ring Programming Language"
for n in aList
    see n + nl
next

```



## Ruby


```ruby
for i in collection do
  puts i
end
```

This is syntactic sugar for:

```ruby
collection.each do |i|
  puts i
end
```

There are various flavours of <code>each</code> that may be class-dependent: [http://www.ruby-doc.org/core/classes/String.html#M000862 String#each_char], [http://www.ruby-doc.org/core/classes/Array.html#M002174 Array#each_index], [http://www.ruby-doc.org/core/classes/Hash.html#M002863 Hash#each_key], etc




## Rust

Rust's for-loop already is a foreach-loop.

```rust
let collection = vec![1,2,3,4,5];
for elem in collection {
    println!("{}", elem);
}
```


Do note that Rust moves values by default and doesn't copy them. A vector would be unusable after looping over it like above. To preserve it, borrow it or use an Iter, to mutate values do a mutable borrow or create an IterMut. To get an immutable reference omit the mut-part.

```rust
let mut collection = vec![1,2,3,4,5];
for mut_ref in &mut collection {
// alternatively:
// for mut_ref in collection.iter_mut() {
    *mut_ref *= 2;
    println!("{}", *mut_ref);
}

// immutable borrow
for immut_ref in &collection {
// alternatively:
// for immut_ref in collection.iter() {
    println!("{}", *immut_ref);
}
```


Since Rust 1.21 foreach can be used explicitly executing a closure on each element.

```rust
let collection = vec![1, 2, 3, 4, 5];
collection.iter().for_each(|elem| println!("{}", elem));
```



## Salmon


```Salmon
iterate (x; ["Red", "Green", "Blue"])
    x!;
```

output:

```txt

Red
Green
Blue

```



## SAS


```sas
/* Initialize an array with integers 1 to 10, and print their sum */
data _null_;
array a a1-a10;
n=1;
do over a;
  a=n;
  n=n+1;
end;
s=sum(of a{*});
put s;
run;
```



## Sather


```sather
class MAIN is
  main is
     num:ARRAY{INT} := |1, 5, 4, 3, 10|;
     loop
       -- the iterator elt! behaves like a "foreach",
       -- yielding the next element of the array at each iteration
       #OUT + num.elt! + "\n";
     end;
  end;
end;
```



## Scala


```scala
val collection = Array(1, 2, 3, 4)
collection.foreach(println)
```

Alternatively:

```scala
(element <- 1 to 4).foreach(println)
```



## Scheme

List:

```scheme
(for-each
  (lambda (i) (display i) (newline))
  the_list)
```



## Scilab

{{works with|Scilab|5.5.1}}
<lang>for e=["a","b","c"]
    printf("%s\n",e)
end
```

{{out}}

```txt
a
b
c
```



## Seed7

The for loop of Seed7 can be used to loop over the elements of a container.

```seed7
$ include "seed7_05.s7i";

var array string: things is [] ("Apple", "Banana", "Coconut");

const proc: main is func
  local
    var string: thing is "";
  begin
    for thing range things do
      writeln(thing);
    end for;
  end func;
```



## Self


```self
aCollection do: [| :element | element printLine ].
```

(Provided that the objects in the collection understand the <code>printLine</code> method).


## SETL


```setl
S := {1,2,3,5,8,13,21,34,55,89};
for e in S loop
    print(e);
end loop;
```



## Sidef

'''foreach''' loop:

```ruby
foreach [1,2,3] { |i|
    say i
}
```


'''for-in''' loop:

```ruby
for i in [1,2,3] {
    say i
}
```


'''.each''' method:

```ruby
[1,2,3].each { |i|
    say i
}
```



## Slate


```slate
c do: [| :obj | print: obj].
```



## Smalltalk


```smalltalk
aCollection do: [ :element | element displayNl ].
```

(Provided that the objects in the collection understand the <code>displayNl</code> method).


## Snabel

Prints foo, bar & baz followed by newlines.

```snabel
['foo' 'bar' 'baz'] &say for
```



## Sparkling


Sparkling currently has no "foreach" construct, but there's a "foreach" function in the standard library:


```sparkling
let hash = { "foo": 42, "bar": 1337, "baz": "qux" };
foreach(hash, function(key, val) {
    print(key, " -> ", val);
});
```



## Standard ML

List of integers:

```sml
app
  (fn i => print (Int.toString i ^ "\n"))
  collect_list
```

Array of integers:

```sml
Array.app
  (fn i => print (Int.toString i ^ "\n"))
  collect_array
```



## Stata


```stata
local a 2 9 4 7 5 3 6 1 8
foreach i in `a' {
	display "`i'"
}
```


## Suneido


```Suneido
for i in #(1, 2, 3)
    Print(i)
```



## Swift


```swift
for i in [1,2,3] {
   print(i)
}
```

This works for any type that conforms to the <code>SequenceType</code> protocol (including arrays, collections, generators, ranges).

Alternately:
{{works with|Swift|2.x+}}

```swift
[1,2,3].forEach {
   print($0)
}
```



## SystemVerilog


```SystemVerilog
program main;
  int values[$];

  initial begin
    values = '{ 1, 3, 7, 11 };
    foreach (values[i]) begin
       $display( "%0d --> %0d", i, values[i] );
    end
  end
endprogram
```



## Tcl


```tcl
foreach i {foo bar baz} {
    puts "$i"
}
```

Note that <tt>foreach</tt> also accepts multiple variables:

```tcl
foreach {x y} {1 2 3 4} {
    puts "$x,$y"
}
```

And also multiple lists:

```tcl
foreach i {1 2 3} j {a b c} {
    puts "$i,$j"
}
```

Or any combination of variables/list:

```tcl
foreach i {1 2 3} {x y} {a b c d e f} {
    puts "$i,$x,$y"
}
```




## Trith


```trith
[1 2 3 4 5] [print] each
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
week="Monday'Tuesday'Wednesday'Thursday'Friday'Saterday'Sunday"
LOOP day=week
PRINT day
ENDLOOP
```

Output:

```txt

Monday
Tuesday
Wednesday
Thursday
Friday
Saterday
Sunday

```



## UNIX Shell

To iterate any single list, you use a <code>for</code> loop.
{{works with|Bourne Shell}}

```bash
for file in *.sh; do
  echo "filename is $file"
done
```

If the list is in a shell parameter (like <code>PATH</code>), you adjust <code>IFS</code>.
{{works with|Bourne Shell}}

```bash
PATH=/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11R6/bin:/usr/local/bin

oldifs=$IFS
IFS=:
for dir in $PATH; do
  echo search $dir
done
IFS=$oldifs
```

Some shells have real arrays. The <code>for</code> loop can also iterate these.
{{works with|Bash}}

```bash
collection=("first" "second" "third" "fourth" "something else")
for x in "${collection[@]}"; do
  echo "$x"
done
```

{{works with|pdksh|5.2.14}}

```bash
set -A collection "first" "second" "third" "fourth" "something else"
for x in "${collection[@]}"; do
  echo "$x"
done
```


=
## C Shell
=

```csh
set collection=(first second third fourth "something else")
foreach x ($collection:q)
	echo $x:q
end
```



## V


```v
[1 2 3] [puts] step
```



## Vim Script

Vim Script's for-loop is actually a foreach-loop and iterates through a list.

```vim
for i in ["alpha", "beta", 42, 5.54]
    echo i
endfor
```




## Wart


```wart
each x '(1 2 3)
  prn x
```



## WDTE


```WDTE>let a =
 import 'arrays';
let s => import 'stream';

a.stream [5; 7; 3]
-> s.map (io.writeln io.stdout)
-> s.drain
;
```



## XLISP

XLISP's <tt>FOR-EACH</tt> applies a procedure to each member of a list in turn.

```lisp
(FOR-EACH PRINT '(CYRUS CAMBYSES DARIUS XERXES ARTAXERXES))
```

{{out}}

```txt
CYRUS
CAMBYSES
DARIUS
XERXES
ARTAXERXES
```



## XPL0

Translation of C example:

```XPL0
include c:\cxpl\codes;
int List, I;
[List:= ["Red", "Green", "Blue", "Black", "White"];
for I:= 0, 5-1 do
        [Text(0, List(I));  CrLf(0)];
]
```



## XSLT

For-each is the only iteration construct that is built into XSLT. All other iteration is either implied by applying a template to all members matching an XPath expression, or built from recursive application of a template. You have access to something like a loop counter with the one-based "position()" function.

```xml
<fo:block font-weight="bold">Adults:</fo:block>
<xsl:for-each select="person[@age &gt;= 21]">
  <fo:block><xsl:value-of select="position()"/>. <xsl:value-of select="@name"/></fo:block>
</xsl:for-each>
```



## zig


```zig
const warn = @import("std").debug.warn;

pub fn main() void {
    const items = []i16{ 0, 1, 1, 2, 3, 5, 8 };

    for (items) |i| {
        warn("{}\n", i);
    }
}
```



## zkl

{{trans|XPL0}}

```zkl
foreach c in (T("Red","Green","Blue","Black","White"))
  { print(c," "); }
```

{{out}}

```txt
Red Green Blue Black White
```

Other forms of foreach, one that breaks apart a container into elements and one line nested loops.

```zkl
foreach x,y,z in (T(T(1,2,3), T(4,5,6))) { println(x,y,z) }
foreach x,y,z in (T(1,2,3), T(4,5), T(6)){ println(x,y,z) }
```

{{out}}

```txt

123
456

```


```txt

146
156
246
256
346
356

```


{{omit from|GUISS}}
