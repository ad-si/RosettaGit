+++
title = "Apply a callback to an array"
description = ""
date = 2019-10-19T01:40:13Z
aliases = []
[extra]
id = 1639
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}
[[Category:Iteration]]

;Task:
Take a combined set of elements and apply a function to each element.





## 11l

{{trans|Kotlin}}

```11l
V array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
V arrsq = array.map(i -> i * i)
print(arrsq)
```

{{out}}

```txt
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```



## 8th

The builtin word "a:map" does this:

```forth

[ 1 , 2, 3 ]
' n:sqr
a:map

```

That results in the array [1,4,9]


## ACL2


ACL2 does not have first-class functions; this is close, however:


```lisp
(defun apply-to-each (xs)
   (if (endp xs)
       nil
       (cons (fn-to-apply (first xs))
             (sq-each (rest xs)))))

(defun fn-to-apply (x)
   (* x x))

```



## ActionScript


```actionscript
package
{
    public class ArrayCallback
    {
        public function main():void
        {
            var nums:Array = new Array(1, 2, 3);
            nums.map(function(n:Number, index:int, arr:Array):void { trace(n * n * n); });
            
            // You can also pass a function reference
            nums.map(cube);
        }
        
        private function cube(n:Number, index:int, arr:Array):void
        {
            trace(n * n * n);
        }
    }
}
```



## Ada

{{works with|GNAT|GPL 2005}}

```ada
with Ada.Text_Io;
 with Ada.Integer_text_IO;
 
 procedure Call_Back_Example is
    -- Purpose: Apply a callback to an array
    -- Output: Prints the squares of an integer array to the console
   
    -- Define the callback procedure
    procedure Display(Location : Positive; Value : Integer) is
    begin
       Ada.Text_Io.Put("array(");
       Ada.Integer_Text_Io.Put(Item => Location, Width => 1);
       Ada.Text_Io.Put(") = ");
       Ada.Integer_Text_Io.Put(Item => Value * Value, Width => 1);
       Ada.Text_Io.New_Line;
    end Display;
   
    -- Define an access type matching the signature of the callback procedure
    type Call_Back_Access is access procedure(L : Positive; V : Integer);
   
    -- Define an unconstrained array type
    type Value_Array is array(Positive range <>) of Integer;
   
    -- Define the procedure performing the callback
    procedure Map(Values : Value_Array; Worker : Call_Back_Access) is
    begin
       for I in Values'range loop
          Worker(I, Values(I));
       end loop;
    end Map;
   
    -- Define and initialize the actual array
    Sample : Value_Array := (5,4,3,2,1);
   
 begin
    Map(Sample, Display'access);   
 end Call_Back_Example;
```



## Aime


```aime
void
map(list l, void (*fp)(object))
{
    l.ucall(fp, 0);
}

void
out(object o)
{
    o_(o, "\n");
}

integer
main(void)
{
    list(0, 1, 2, 3).map(out);

    return 0;
}
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of FORMATted transput}}

```algol68
 PROC call back proc = (INT location, INT value)VOID:
 (
   printf(($"array["g"] = "gl$, location, value))
 );

 PROC map = (REF[]INT array, PROC (INT,INT)VOID call back)VOID:
 (
   FOR i FROM LWB array TO UPB array DO
      call back(i, array[i])
   OD
 );
 
 main:
 (
   [4]INT array := ( 1, 4, 9, 16 );
   map(array, call back proc)
 )
```


{{Out}}

```txt

array[         +1] =          +1
array[         +2] =          +4
array[         +3] =          +9
array[         +4] =         +16

```



## APL

By default functions in APL work on arrays as it is an array oriented language. Some examples:


```APL
    - 1 2 3
¯1 ¯2 ¯3
    2 * 1 2 3 4
2 4 8 16
    2 × ⍳4
2 4 6 8
    3 * 3 3 ⍴ ⍳9
   3    9    27
  81  243   729
2187 6561 19683

```



## AppleScript


```applescript
on callback for arg
    -- Returns a string like "arc has 3 letters"
    arg & " has " & (count arg) & " letters"
end callback

set alist to {"arc", "be", "circle"}
repeat with aref in alist
    -- Passes a reference to some item in alist
    -- to callback, then speaks the return value.
    say (callback for aref)
end repeat
```


If the callback would <code>set arg's contents to "something"</code>, then <code>alist</code> would be mutated.


For a more general implementation of '''map(function, list)''', '''foldl(function, startValue, list)''', and '''filter(predicate, list)''', we could write:


```applescript
on run
    
    set xs to {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
    
    {map(square, xs), ¬
        filter(even, xs), ¬
        foldl(add, 0, xs)}
    
    --> {{1, 4, 9, 16, 25, 36, 49, 64, 81, 100}, {2, 4, 6, 8, 10}, 55}  
    
end run

-- square :: Num -> Num -> Num
on square(x)
    x * x
end square

-- add :: Num -> Num -> Num
on add(a, b)
    a + b
end add

-- even :: Int -> Bool
on even(x)
    0 = x mod 2
end even


-- GENERIC HIGHER ORDER FUNCTIONS

-- filter :: (a -> Bool) -> [a] -> [a]
on filter(f, xs)
    tell mReturn(f)
        set lst to {}
        set lng to length of xs
        repeat with i from 1 to lng
            set v to item i of xs
            if |λ|(v, i, xs) then set end of lst to v
        end repeat
        return lst
    end tell
end filter

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

-- Lift 2nd class handler function into 1st class script wrapper 
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

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
```

{{Out}}

```txt
{{1, 4, 9, 16, 25, 36, 49, 64, 81, 100}, {2, 4, 6, 8, 10}, 55}
```



## Arturo



```arturo
arr #(1 2 3 4 5)

print $(map arr { & * 2 })
```


{{out}}


```txt
#(2 4 6 8 10)
```



## AutoHotkey


```AutoHotkey
map("callback", "3,4,5")

callback(array){
  Loop, Parse, array, `,
    MsgBox % (2 * A_LoopField)
}
 
map(callback, array){
  %callback%(array)
}
```



## AWK


```awk
$ awk 'func psqr(x){print x,x*x}BEGIN{split("1 2 3 4 5",a);for(i in a)psqr(a[i])}'
4 16
5 25
1 1
2 4
3 9
```



## Babel


Let us define a squaring operator:


```babel
sq { dup * } <
```


Now, we apply the sq operator over a list and display the result using the lsnum utility:


```babel
( 0 1 1 2 3 5 8 13 21 34 ) { sq ! } over ! lsnum !
```


{{Out}}

```txt
( 0 1 1 4 9 25 64 169 441 1156 )
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      DIM a(4)
      a() = 1, 2, 3, 4, 5
      PROCmap(a(), FNsqrt())
      FOR i = 0 TO 4
        PRINT a(i)
      NEXT
      END
      
      DEF FNsqrt(n) = SQR(n)
      
      DEF PROCmap(array(), RETURN func%)
      LOCAL I%
      FOR I% = 0 TO DIM(array(),1)
        array(I%) = FN(^func%)(array(I%))
      NEXT
      ENDPROC

```

{{Out}}

```txt

         1
1.41421356
1.73205081
         2
2.23606798

```



## Bracmat


```bracmat
( ( callbackFunction1
  =   location value
    .   !arg:(?location,?value)
      & out$(str$(array[ !location "] = " !!value))
  )
& ( callbackFunction2
  =   location value
    .   !arg:(?location,?value)
      & !!value^2:?!value
  )
& ( mapar
  =   arr len callback i
    .   !arg:(?arr,?len,?callback)
      & 0:?i
      &   whl
        ' ( !i:<!len
          & !callback$(!i,!i$!arr)
          & 1+!i:?i
          )
  )
& tbl$(array,4)
& 1:?(0$array)
& 2:?(1$array)
& 3:?(2$array)
& 4:?(3$array)
& mapar$(array,4,callbackFunction1)
& mapar$(array,4,callbackFunction2)
& mapar$(array,4,callbackFunction1)
);
```

{{Out}}

```txt
array[0] = 1
array[1] = 2
array[2] = 3
array[3] = 4
array[0] = 1
array[1] = 4
array[2] = 9
array[3] = 16
```



## Brat



```brat
#Print out each element in array
[:a :b :c :d :e].each { element |
	p element
}
```


Alternatively:


```brat
[:a :b :c :d :e].each ->p
```



## C


'''callback.h'''

```c
#ifndef CALLBACK_H
#define CALLBACK_H

/*
 * By declaring the function in a separate file, we allow
 * it to be used by other source files.
 *
 * It also stops ICC from complaining.
 *
 * If you don't want to use it outside of callback.c, this
 * file can be removed, provided the static keyword is prepended
 * to the definition.
 */
void map(int* array, int len, void(*callback)(int,int));

#endif
```


'''callback.c'''

```c>#include <stdio.h

#include "callback.h"

/*
 * We don't need this function outside of this file, so
 * we declare it static.
 */
static void callbackFunction(int location, int value)
{
  printf("array[%d] = %d\n", location, value);
} 

void map(int* array, int len, void(*callback)(int,int))
{
  int i;
  for(i = 0; i < len; i++)
  {
     callback(i, array[i]);
  }
} 

int main()
{
  int array[] = { 1, 2, 3, 4 };
  map(array, 4, callbackFunction);
  return 0;
}
```


{{Out}}

```txt

  array[0] = 1
  array[1] = 2
  array[2] = 3
  array[3] = 4

```



## C sharp

{{works with|C sharp|C#|3.0+}}
This version uses the C# 3 lambda notation. 


```csharp
int[] intArray = { 1, 2, 3, 4, 5 };
// Simplest method:  LINQ, functional
int[] squares1 = intArray.Select(x => x * x).ToArray();

// Slightly fancier: LINQ, query expression
int[] squares2 = (from x in intArray
                  select x * x).ToArray();

// Or, if you only want to call a function on each element, just use foreach
foreach (var i in intArray)
    Console.WriteLine(i * i);
```


{{works with|C sharp|C#|2.0+}}

{{works with|Visual C sharp|Visual C#|2005}}

```csharp
using System; 

static class Program
{
  // Purpose: Apply a callback (or anonymous method) to an Array
  // Output: Prints the squares of an int array to the console.
  // Compiler: Visual Studio 2005
  // Framework: .net 2
   
  [STAThread]
  public static void Main() 
  {
    int[] intArray = { 1, 2, 3, 4, 5 };

    // Using a callback,
    Console.WriteLine("Printing squares using a callback:");
    Array.ForEach<int>(intArray, PrintSquare);

    // or using an anonymous method:
    Console.WriteLine("Printing squares using an anonymous method:");
    Array.ForEach<int>
    (
      intArray,
      delegate(int value) 
      {
        Console.WriteLine(value * value);    
      });
  }

  public static void PrintSquare(int value) 
  { 
    Console.WriteLine(value * value);
  }
}
```



## C++


{{works with|g++|4.1.1}}    
===C-Style Array===

```cpp>#include <iostream
 //cout for printing
#include <algorithm> //for_each defined here

//create the function (print the square)
void print_square(int i) {
  std::cout << i*i << " ";
}

int main() {
  //create the array
  int ary[]={1,2,3,4,5};
  //stl for_each
  std::for_each(ary,ary+5,print_square);
  return 0;
}
//prints 1 4 9 16 25
```



### std::vector

{{libheader|STL}}

```cpp>#include <iostream
  // cout for printing
#include <algorithm> // for_each defined here
#include <vector>    // stl vector class

// create the function (print the square)
void print_square(int i) {
  std::cout << i*i << " ";
}

int main() {
  // create the array
  std::vector<int> ary;
  ary.push_back(1);
  ary.push_back(2);
  ary.push_back(3);
  ary.push_back(4);
  ary.push_back(5);
  // stl for_each
  std::for_each(ary.begin(),ary.end(),print_square);
  return 0;
}
//prints 1 4 9 16 25
```


More tricky with binary function

```cpp>#include <iostream
   // cout for printing
#include <algorithm>  // for_each defined here
#include <vector>     // stl vector class
#include <functional> // bind and ptr_fun

// create a binary function (print any two arguments together)
template<class type1,class type2>
void print_juxtaposed(type1 x, type2 y) {
  std::cout << x << y;
}

int main() {
  // create the array
  std::vector<int> ary;
  ary.push_back(1);
  ary.push_back(2);
  ary.push_back(3);
  ary.push_back(4);
  ary.push_back(5);
  // stl for_each, using binder and adaptable unary function
  std::for_each(ary.begin(),ary.end(),std::bind2nd(std::ptr_fun(print_juxtaposed<int,std::string>),"x "));
  return 0;
}
//prints 1x 2x 3x 4x 5x
```



### Boost.Lambda

{{libheader|Boost}}

```cpp
using namespace std;
using namespace boost::lambda;
vector<int> ary(10);
int i = 0;
for_each(ary.begin(), ary.end(), _1 = ++var(i)); // init array
transform(ary.begin(), ary.end(), ostream_iterator<int>(cout, " "), _1 * _1); // square and output
```



### C++11


```cpp>#include <vector

#include <iostream>
#include <algorithm>
#include <iterator>

int main() {
   std::vector<int> intVec(10);
   std::iota(std::begin(intVec), std::end(intVec), 1 ); // Fill the vector
   std::transform(std::begin(intVec) , std::end(intVec), std::begin(intVec),
	 [](int i) { return i * i ; } ); // Transform it with closures
   std::copy(std::begin(intVec), end(intVec) ,
	 std::ostream_iterator<int>(std::cout, " "));
   std::cout << std::endl;
   return 0;
}
```



## Clean


Define a function and an initial (unboxed) array.


```clean
square x = x * x

values :: {#Int}
values = {x \\ x <- [1 .. 10]}
```


One can easily define a map for arrays, which is overloaded and works for all kinds of arrays (lazy, strict, unboxed).


```clean
mapArray f array = {f x \\ x <-: array}
```


Apply the function to the initial array (using a comprehension) and print result.


```clean
Start :: {#Int}
Start = mapArray square values
```



## Clio

'''Math operations'''

```clio
[1 2 3 4] * 2 + 1 -> print
```

'''Quick functions'''
<lang>[1 2 3 4] -> * n: n * 2 + 1 -> print
```

'''Anonymous function'''

```clio
[1 2 3 4]
  -> * fn n:
     n * 2 + 1
  -> print
```

'''Named function'''

```clio
fn double-plus-one n:
  n * 2 + 1

[1 2 3 4] -> * double-plus-one -> print
```



## Clojure



```lisp
;; apply a named function, inc
(map inc [1 2 3 4])
```



```lisp
;; apply a function
(map (fn [x] (* x x)) [1 2 3 4])
```



```lisp
;; shortcut syntax for a function
(map #(* % %) [1 2 3 4])
```



## COBOL


Basic implementation of a map function:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Map.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Table-Size CONSTANT 30.

       LOCAL-STORAGE SECTION.
       01  I USAGE UNSIGNED-INT.

       LINKAGE SECTION.
       01  Table-Param.
           03  Table-Values USAGE COMP-2 OCCURS Table-Size TIMES.

       01  Func-Id PIC X(30).

       PROCEDURE DIVISION USING Table-Param Func-Id.
           PERFORM VARYING I FROM 1 BY 1 UNTIL Table-Size < I
               CALL Func-Id USING BY REFERENCE Table-Values (I)
           END-PERFORM

           GOBACK
           .
```



## CoffeeScript


```coffeescript

map = (arr, f) -> (f(e) for e in arr)
arr = [1, 2, 3, 4, 5]
f = (x) -> x * x
console.log map arr, f # prints [1, 4, 9, 16, 25]

```




## Common Lisp


Imperative: print 1, 2, 3, 4 and 5:


```lisp
(map nil #'print #(1 2 3 4 5))
```


Functional: collect squares into new vector that is returned:


```lisp
(defun square (x) (* x x))
(map 'vector #'square #(1 2 3 4 5))
```


Destructive, like the Javascript example; add 1 to every slot of vector *a*:


```lisp
(defvar *a* (vector 1 2 3))
(map-into *a* #'1+ *a*)
```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE Callback;
IMPORT StdLog;

TYPE
	Callback = PROCEDURE (x: INTEGER;OUT doubled: INTEGER);
	Callback2 = PROCEDURE (x: INTEGER): INTEGER;
	
	PROCEDURE Apply(proc: Callback; VAR x: ARRAY OF INTEGER);
	VAR
		i: INTEGER;
	BEGIN
		FOR i := 0 TO LEN(x) - 1 DO;
			proc(x[i],x[i]);
		END
	END Apply;
	
	PROCEDURE Apply2(func: Callback2; VAR x: ARRAY OF INTEGER);
	VAR
		i: INTEGER;
	BEGIN
		FOR i := 0 TO LEN(x) - 1 DO;
			x[i] := func(x[i]);
		END
	END Apply2;
	
	PROCEDURE Double(x: INTEGER; OUT y: INTEGER);
	BEGIN	
		y := x * x;
	END Double;
	
	PROCEDURE Double2(x: INTEGER): INTEGER;
	BEGIN
		RETURN x * x
	END Double2;
	
	PROCEDURE Do*;
	VAR
		i: INTEGER;
		ary: ARRAY 10 OF INTEGER;
		
		
	BEGIN
		FOR i := 0 TO LEN(ary) - 1 DO ary[i] := i END;
		Apply(Double,ary);
		FOR i := 0 TO LEN(ary) - 1 DO
			StdLog.Int(ary[i]);StdLog.Ln
		END;
		StdLog.Ln;
		Apply2(Double2,ary);
		FOR  i := 0 TO LEN(ary) - 1 DO
		        StdLog.Int(ary[i]);StdLog.Ln
		END
	END Do;
END Callback.

```

Execute: ^Q Callback.Do<br/>
{{Out}}

```txt

 0
 1
 4
 9
 16
 25
 36
 49
 64
 81

 0
 1
 16
 81
 256
 625
 1296
 2401
 4096
 6561

```



## D


```d
import std.stdio, std.algorithm;

void main() {
    auto items = [1, 2, 3, 4, 5];
    auto m = items.map!(x => x + 5)();
    writeln(m);
}
```

{{out}}

```txt
[6, 7, 8, 9, 10]
```



## Delphi


```Delphi

// Declare the callback function
procedure callback(const AInt:Integer);
begin
  WriteLn(AInt);
end;

const
  // Declare a static array
  myArray:Array[0..4] of Integer=(1,4,6,8,7);
var
  // Declare interator variable
  i:Integer;
begin
  // Iterate the array and apply callback
  for i:=0 to length(myArray)-1 do
    callback(myArray[i]);
end.

```

=={{header|Déjà Vu}}==
There is a <code>map</code> builtin that does just this.

```dejavu
!. map @++ [ 1 4 8 ]

#implemented roughly like this:
#map f lst:
#    ]
#    for i in lst:
#         f i
#    [
```

{{out}}

```txt
[ 2 5 9 ]
```



## Dyalect



```Dyalect
func Array.select(pred) {
    for x in this when pred(x) {
        yield x
    }
}

var arr = [1, 2, 3, 4, 5]
var squares = arr.select(x => x * x)
 
print(squares)
```



## E



```e
def array := [1,2,3,4,5]
def square(value) { 
    return value * value
}
```


Example of builtin iteration:


```e
def callback(index, value) { 
    println(`Item $index is $value.`)
}
array.iterate(callback)
```


There is no built-in map function '''yet'''. 
The following is one of the ways one could be implemented, 
returning a plain list (which is usually an array in implementation).


```e
def map(func, collection) {
    def output := [].diverge()
    for item in collection {
        output.push(func(item))
    }
    return output.snapshot()
}
println(map(square, array))
```



## EchoLisp


```scheme

(vector-map sqrt #(0 4 16 49))
    → #( 0 2 4 7)
;; or
(map exp #(0 1 2))
    → #( 1 2.718281828459045 7.38905609893065)
;; or
(for/vector ([elem #(2 3 4)] [i (in-naturals)]) (printf "v[%d] = %a" i elem) (* elem elem))
v[0] = 2
v[1] = 3
v[2] = 4
    → #( 4 9 16)

```



## Efene



```efene
square = fn (N) {
    N * N
}

# list comprehension
squares1 = fn (Numbers) {
    [square(N) for N in Numbers]
}

# functional form
squares2a = fn (Numbers) {
    lists.map(fn square:1, Numbers)
}

# functional form with lambda
squares2b = fn (Numbers) {
    lists.map(fn (N) { N * N }, Numbers)
}

# no need for a function
squares3 = fn (Numbers) {
    [N * N for N in Numbers]
}

@public
run = fn () {
    Numbers = [1, 3, 5, 7]
    io.format("squares1 : ~p~n", [squares1(Numbers)])
    io.format("squares2a: ~p~n", [squares2a(Numbers)])
    io.format("squares2b: ~p~n", [squares2b(Numbers)])
    io.format("squares3 : ~p~n", [squares3(Numbers)])
}

```



## EGL


```EGL
delegate callback( i int ) returns( int ) end

program ApplyCallbackToArray
	function main()
		values int[] = [ 1, 2, 3, 4, 5 ];

		func callback = square;
		for ( i int to values.getSize() )
			values[ i ] = func( values[ i ] );
		end
		
		for ( i int to values.getSize() )
			SysLib.writeStdout( values[ i ] );
		end
	end
	
	function square( i int ) returns( int )
		return( i * i );
	end
end
```



## Elena

ELENA 4.0 :

```elena
import system'routines;

PrintSecondPower(n){ console.writeLine(n * n) }

public program()
{
    new int[]::(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).forEach:PrintSecondPower
}
```



## Elixir


```Elixir

Enum.map([1, 2, 3], fn(n) -> n * 2 end)
Enum.map [1, 2, 3], &(&1 * 2)

```


{{Out}}

```txt

[2, 4, 6]

```



## Erlang

A list would be more commonly used in Erlang rather than an array.


```Erlang

1> L = [1,2,3].
[1,2,3]

```


You can use lists:foreach/2 if you just want to apply the callback to each element of the list.

<lang>
2> lists:foreach(fun(X) -> io:format("~w ",[X]) end, L).
1 2 3 ok

```


Or you can use lists:map/2 if you want to create a new list with the result of the callback on each element.


```Erlang

3> lists:map(fun(X) -> X + 1 end, L).
[2,3,4]

```


Or you can use lists:foldl/3 if you want to accumulate the result of the callback on each element into one value.


```Erlang

4> lists:foldl(fun(X, Sum) -> X + Sum end, 0, L).
6

```




## ERRE

<lang>
PROGRAM CALLBACK

!
! for rosettacode.org
!

DIM A[5]

FUNCTION CBACK(X)
   CBACK=2*X-1
END FUNCTION

PROCEDURE PROCMAP(ZETA,DUMMY(X)->OUTP)
   OUTP=DUMMY(ZETA)
END PROCEDURE

BEGIN
   A[1]=1  A[2]=2   A[3]=3  A[4]=4  A[5]=5
   FOR I%=1 TO 5 DO
      PROCMAP(A[I%],CBACK(X)->OUTP)
      PRINT(OUTP;)
   END FOR
   PRINT
END PROGRAM

```

This example shows how to pass a function to a procedure.
{{Out}}

```txt

  1  3  5  7  9

```



## Euphoria


```euphoria
function apply_to_all(sequence s, integer f)
    -- apply a function to all elements of a sequence
    sequence result
    result = {}
    for i = 1 to length(s) do
	-- we can call add1() here although it comes later in the program
	result = append(result, call_func(f, {s[i]}))
    end for
    return result
end function

function add1(atom x)
    return x + 1
end function

-- add1() is visible here, so we can ask for its routine id
? apply_to_all({1, 2, 3}, routine_id("add1"))
-- displays {2,3,4}
```

This is also "Example 2" in the Euphoria documentation for <code>routine_id()</code>.
Note that this example will not work for multi-dimensional sequences.


## Factor

Print each element squared:

```factor
{ 1 2 3 4 } [ sq . ] each
```


Collect return values:

```factor
{ 1 2 3 4 } [ sq ] map
```



## Fantom


In Fantom, functions can be passed to a collection iterator, such as 'each'.  'map' is used similarly, and the results are collected into a list.


```fantom

class Main
{
  public static Void main ()
  {
    [1,2,3,4,5].each |Int i| { echo (i) }
    Int[] result := [1,2,3,4,5].map |Int i->Int| { return i * i }
    echo (result) 
  }
}

```


{{Out}}

```txt

1
2
3
4
5
[1, 4, 9, 16, 25]

```



## FBSL

'''User-defined mapping function:'''

```qbasic
#APPTYPE CONSOLE

FOREACH DIM e IN MyMap(Add42, {1, 2, 3})
	PRINT e, " ";
NEXT

PAUSE

FUNCTION MyMap(f, a)
	DIM ret[]
	FOREACH DIM e IN a
		ret[] = f(e)
	NEXT
	RETURN ret
END FUNCTION

FUNCTION Add42(n): RETURN n + 42: END FUNCTION
```

{{Out}}

```txt
43 44 45
Press any key to continue...
```


'''Standard MAP() function:'''

```qbasic
#APPTYPE CONSOLE

DIM languages[] = {{"English", {"one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"}}, _
		  {"French", {"un", "deux", "trois", "quatre", "cinq", "six", "sept", "huit", "neuf", "dix"}}}

MAP(SpeakALanguage, languages)

PAUSE

SUB NameANumber(lang, nb, number)
	PRINT "The number ", nb, " is called ", STRENC(number), " in ", lang
END SUB

SUB SpeakALanguage(lang)
	MAP(NameANumber, lang[0], 1 TO 10, lang[1])
	PRINT LPAD("", 40, "-")
END SUB
```

{{Out}}

```txt
The number 1 is called "one" in English
The number 2 is called "two" in English
The number 3 is called "three" in English
The number 4 is called "four" in English
The number 5 is called "five" in English
The number 6 is called "six" in English
The number 7 is called "seven" in English
The number 8 is called "eight" in English
The number 9 is called "nine" in English
The number 10 is called "ten" in English
----------------------------------------
The number 1 is called "un" in French
The number 2 is called "deux" in French
The number 3 is called "trois" in French
The number 4 is called "quatre" in French
The number 5 is called "cinq" in French
The number 6 is called "six" in French
The number 7 is called "sept" in French
The number 8 is called "huit" in French
The number 9 is called "neuf" in French
The number 10 is called "dix" in French
----------------------------------------
Press any key to continue...
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Apply_a_callback_to_an_array this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


This is a word that will call a given function on each cell in an array.


```forth
: map ( addr n fn -- )
   -rot cells bounds do  i @ over execute i !  cell +loop ;
```


{{Out|Example usage}}

```forth
create data 1 , 2 , 3 , 4 , 5 ,
data 5 ' 1+ map  \ adds one to each element of data
```



## Fortran

Elemental functions.

{{Works with |Fortran|ISO 95 and later}}

```fortran
module arrCallback
contains
    elemental function cube( x )
        implicit none
        real :: cube
        real, intent(in) :: x
        cube = x * x * x
    end function cube
end module arrCallback
```



```fortran
program testAC
    use arrCallback
    implicit none
    integer :: i, j
    real, dimension(3,4) :: b, &
        a = reshape( (/ ((10 * i + j, i = 1, 3), j = 1, 4) /), (/ 3,4 /) )
     
    do i = 1, 3
        write(*,*) a(i,:)
    end do
     
    b = cube( a )  ! Applies CUBE to every member of a,
                   ! and stores each result in the equivalent element of b
    do i = 1, 3
        write(*,*) b(i,:)
    end do
end program testAC
```


{{Works with|ANSI FORTRAN| 77 (with MIL-STD-1753 structured DO) and later}}

```fortran
      program test
C
C--   Declare array:
      integer a(5)
C
C--   Fill it with Data
      data a /45,22,67,87,98/
C
C--   Do something with all elements (in this case: print their squares)
      do i=1,5
        print *,a(i)*a(i)
      end do
C
      end
```



## FP


```fp
{square * . [id, id]}
& square: <1,2,3,4,5>
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub PrintEx(n As Integer)
  Print n, n * n, n * n * n
End Sub

Sub Proc(a() As Integer, callback As Sub(n As Integer))
  For i As Integer = LBound(a) To UBound(a)
    callback(i)
  Next
End Sub

Dim a(1 To 10) As Integer = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
Print " n", "n^2", "n^3"
Print " -", "---", "---"
Proc(a(), @PrintEx)
Print
Print "Press any key to quit the program"
Sleep
```


{{out}}

```txt

 n            n^2           n^3
 -            ---           ---
 1             1             1
 2             4             8
 3             9             27
 4             16            64
 5             25            125
 6             36            216
 7             49            343
 8             64            512
 9             81            729
 10            100           1000

```



## Frink


```frink

f = {|x| x^2}   // Anonymous function to square input
a = [1,2,3,5,7]
println[map[f, a]]

```


=={{header|F_Sharp|F#}}==
Apply a named function to each member of the array. The result is a new array of the same size as the input.

```fsharp
let evenp x = x % 2 = 0
let result = Array.map evenp [| 1; 2; 3; 4; 5; 6 |]
```

The same can be done using anonymous functions, this time squaring the members of the input array.

```fsharp
let result = Array.map (fun x -> x * x) [|1; 2; 3; 4; 5|]
```

Use ''iter'' if the applied function does not return a value.

```fsharp
Array.iter (fun x -> printfn "%d" x) [|1; 2; 3; 4; 5|]
```



## FunL


```funl
[1, 2, 3].foreach( println )

[1, 2, 3].foreach( a -> println(2a) )
```


{{out}}


```txt

1
2
3
2
4
6

```



## Futhark


```Futhark

map f l

```

e.g.

```Futhark

map (\x->x+1) [1,2,3] -- [2,3,4]

```

or equivalently

```Futhark

map (+1) [1,2,3] -- [2,3,4]

```



## GAP


```gap
a := [1 .. 4];
b := ShallowCopy(a);

# Apply and replace values
Apply(a, n -> n*n);
a;
# [ 1, 4, 9, 16 ]

# Apply and don't change values
List(b, n -> n*n);
# [ 1, 4, 9, 16 ]

# Apply and don't return anything (only side effects)
Perform(b, Display);
1
2
3
4

b;
# [ 1 .. 4 ]
```



## Go

{{trans|Ruby}}
The task was originally written with a Ruby example, so here are Go versions of the current Ruby examples.

Perhaps in contrast to Ruby, it is idiomatic in Go to use the for statement:

```go
package main

import "fmt"

func main() {
    for _, i := range []int{1, 2, 3, 4, 5} {
        fmt.Println(i * i)
    }
}
```


Alternatively though, an array-like type can be defined and callback-style methods can be defined on it to apply a function to the elements.

```go
package main

import "fmt"

type intSlice []int

func (s intSlice) each(f func(int)) {
    for _, i := range s {
        f(i)
    }
}

func (s intSlice) Map(f func(int) int) intSlice {
    r := make(intSlice, len(s))
    for j, i := range s {
        r[j] = f(i)
    }
    return r
}

func main() {
    s := intSlice{1, 2, 3, 4, 5}

    s.each(func(i int) {
        fmt.Println(i * i)
    })

    fmt.Println(s.Map(func(i int) int {
        return i * i
    }))
}
```

{{out}}

```txt

1
4
9
16
25
[1 4 9 16 25]

```



## Groovy


Print each value in a list

```groovy
[1,2,3,4].each { println it }
```


Create a new list containing the squares of another list

```groovy
[1,2,3,4].collect { it * it }
```



## Haskell



### List

{{works with|GHC}}

```haskell
let square x = x*x
let values = [1..10]
map square values
```


Using list comprehension to generate a list of the squared values

```haskell
[square x | x <- values]
```


More directly

```haskell
[1 .. 10] >>= pure . (^ 2)
```


Or with one less layer of monadic wrapping

```haskell
(^ 2) <$> [1..10]
```


Using function composition to create a function that will print the squares of a list

```haskell
let printSquares = mapM_ (print.square)
printSquares values
```



### Array

{{works with|GHC|7.10.3}}

```haskell
import Data.Array (Array, listArray)

square :: Int -> Int
square x = x * x

values :: Array Int Int
values = listArray (1, 10) [1 .. 10]

main :: IO ()
main = print $ fmap square values
```

{{Out}}

```txt
array (1,10) [(1,1),(2,4),(3,9),(4,16),(5,25),(6,36),(7,49),(8,64),(9,81),(10,100)]
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure main()
   local lst
   lst := [10, 20, 30, 40]
   every callback(write,!lst)
end

procedure callback(p,arg)
   return p(" -> ", arg)
end
```



## IDL


Hard to come up with an example that isn't completely contrived. IDL doesn't really distinguish between a scalar and an array; thus


```idl
b = a^3
```


will yield a scalar if <tt>a</tt> is scalar or a vector if <tt>a</tt> is a vector or an n-dimensional array if <tt>a</tt> is an n-dimensional array


## Io


```io
list(1,2,3,4,5) map(squared)
```



## J


'''Solution''':

```j
   "_1
```


'''Example''':

```j
   callback =:  *:
   array    =:  1 2 3 4 5
   
   callback"_1 array
1 4 9 16 25
```


But note that this is a trivial example since <code>*: 1 2 3 4 5</code> would get the same result.  Then again, this is something of a trivial exercise in J since all of J is designed around the idea of applying functions usefully to arrays.


## Java


Up to Java 7, you have to define an interface for each type of function you want to use.
The <code>IntConsumer</code> performs an action (which doesn't return anything) on an array of ints,
while the <code>IntToInt</code> is used to replace the array values.


```java
public class ArrayCallback7 {

    interface IntConsumer {
        void run(int x);
    }

    interface IntToInt {
        int run(int x);
    }

    static void forEach(int[] arr, IntConsumer consumer) {
        for (int i : arr) {
            consumer.run(i);
        }
    }

    static void update(int[] arr, IntToInt mapper) {
        for (int i = 0; i < arr.length; i++) {
            arr[i] = mapper.run(arr[i]);
        }
    }

    public static void main(String[] args) {
        int[] numbers = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

        forEach(numbers, new IntConsumer() {
            public void run(int x) {
                System.out.println(x);
            }
        });

        update(numbers, new IntToInt() {
            @Override
            public int run(int x) {
                return x * x;
            }
        });

        forEach(numbers, new IntConsumer() {
            public void run(int x) {
                System.out.println(x);
            }
        });
    }
}
```


Using Java 8 streams:
{{works with|Java|8}}


```java
import java.util.Arrays;

public class ArrayCallback {

    public static void main(String[] args) {
        int[] myIntArray = {1, 2, 3, 4, 5};

        int sum = Arrays.stream(myIntArray)
                .map(x -> {
                    int cube = x * x * x;
                    System.out.println(cube);
                    return cube;
                })
                .reduce(0, (left, right) -> left + right); // <-- could substitute .sum() for .reduce(...) here.
        System.out.println("sum: " + sum);
    }
}
```



## JavaScript



### ES3


```javascript
function map(a, func) {
  var ret = [];
  for (var i = 0; i < a.length; i++) {
    ret[i] = func(a[i]);
  }
  return ret;
}

map([1, 2, 3, 4, 5], function(v) { return v * v; });
```



### ES5


```javascript
[1, 2, 3, 4, 5].map(function(v) { return v * v; });
```



### ES6


```javascript
[1, 2, 3, 4, 5].map(v => v * v);
```


The result is always: 


```txt
[1, 4, 9, 16, 25]
```



## Joy


```joy
[1 2 3 4 5] [dup *] map.
```



## jq


```jq
# Illustration of map/1 using the builtin filter: exp
map(exp)  # exponentiate each item in the input list

# A compound expression can be specified as the argument to map, e.g.
map( (. * .) + sqrt ) # x*x + sqrt(x)

# The compound expression can also be a composition of filters, e.g.
map( sqrt|floor )     # the floor of the sqrt

# Array comprehension
reduce .[] as $n ([]; . + [ exp ])

# Elementwise operation 
 [.[] + 1 ]   # add 1 to each element of the input array

```
Here is a transcript illustrating how the last of these jq expressions can be evaluated:

```jq
$ jq -c ' [.[] + 1 ]'
[0, 1 , 10]
[1,2,11]
```



## Jsish


```javascript
/* Apply callback, in Jsish using array.map() */
;[1, 2, 3, 4, 5].map(function(v,i,a) { return v * v; });

/*
=!EXPECTSTART!=
[1, 2, 3, 4, 5].map(function(v,i,a) { return v * v; }) ==> [ 1, 4, 9, 16, 25 ]
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u applyCallback.jsi
[PASS] applyCallback.jsi
```



## Julia

{{works with|Julia|0.6}}

```julia
numbers = [1, 3, 5, 7]

@show [n ^ 2 for n in numbers]                  # list comprehension
square(x) = x ^ 2; @show map(square, numbers)   # functional form
@show map(x -> x ^ 2, numbers)                  # functional form with anonymous function
@show [n * n for n in numbers]    				# no need for a function,
@show numbers .* numbers                        # element-wise operation
@show numbers .^ 2                              # includes .+, .-, ./, comparison, and bitwise operations as well
```



## Kotlin


```scala
fun main(args: Array<String>) {
    val array = arrayOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)  // build
    val function = { i: Int -> i * i } // function to apply
    val list = array.map { function(it) } // process each item
    println(list) // print results
}
```

{{out}}

```txt
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```



## Lang5


```lang5
: square(*)  dup * ;
[1 2 3 4 5] square        . "\n" .
[1 2 3 4 5] 'square apply . "\n" .
```



## Lasso


```Lasso
define cube(n::integer) => #n*#n*#n

local(
	mynumbers = array(1, 2, 3, 4, 5),
	mycube = array
)

#mynumbers -> foreach => {
	#mycube -> insert(cube(#1))
}

#mycube
```

-> array(1, 8, 27, 64, 125)


## Lisaac


```Lisaac
+ a : ARRAY(INTEGER);
+ b : {INTEGER;};

a := ARRAY(INTEGER).create 1 to 3;
1.to 3 do { i : INTEGER;
  a.put i to i;
};

b := { arg : INTEGER;
  (arg * arg).print;
  '\n'.print;
};

a.foreach b;
```



## Logo


```logo
to square :x
  output :x * :x
end
show map "square [1 2 3 4 5]  ; [1 4 9 16 25]
show map [? * ?] [1 2 3 4 5]  ; [1 4 9 16 25]
foreach [1 2 3 4 5] [print square ?]  ; 1 4 9 16 25, one per line
```



## Lua


Say we have an array:

```lua
myArray = {1, 2, 3, 4, 5}
```

A map function for this would be

```lua
map = function(f, data)
   local result = {}
   for k,v in ipairs(data) do
      result[k] = f(v)
   end
   return result
end
```

Together with our array and a square function this yields:

```lua
myFunc = function(x) return x*x end

print(unpack( map(myFunc, myArray) ))
--> 1   4   9   16  25
```

If you used pairs() instead of ipairs(), this would even work on a hash table in general.
However, remember that hash table do not have an implicit ordering on their elements, like arrays do,
so pairs() is not guaranteed to return the elements in the same order as ipairs()


## M2000 Interpreter


```M2000 Interpreter

a=(1,2,3,4,5)
b=lambda->{
	push number**2	
}
Print a#map(b) ' 1 4 9 16 25
Print a#map(b, b)  ' 1 16 81 256 625
b=lambda (z) ->{
	=lambda z ->{
		push number**z	
	}	
}
Print a#map(b(2)) ' 1 4 9 16 25
Print a#map(b(3)) ' 1 8 27 64 125

\\ second example
a=(1,2,3,4,5)
class s {sum=0}
\\ s is a pointer to an instance of s()
s->s()
c=lambda s -> {
	push number+number
	s=>sum=stackitem()  ' peek the value from stack
}
\\ c passed by value to fold(), but has a pointer to s
Print a#fold(c, 100)=115
Print s=>sum=115


```



## M4


```M4
define(`foreach', `pushdef(`$1')_foreach($@)popdef(`$1')')dnl
define(`_arg1', `$1')dnl
define(`_foreach', `ifelse(`$2', `()', `',
   `define(`$1', _arg1$2)$3`'$0(`$1', (shift$2), `$3')')')dnl
dnl
define(`apply',`foreach(`x',$1,`$2(x)')')dnl
dnl
define(`z',`eval(`$1*2') ')dnl
apply(`(1,2,3)',`z')
```


{{Out}}

```txt

2 4 6

```



## Maple

For lists and sets, which in Maple are immutable, a new object is returned.  
Either the built-in procedure map, or the short syntax of a trailing tilde (~) on the applied operator may be used.

```Maple

> map( sqrt, [ 1.1, 3.2, 5.7 ] );
                [1.048808848, 1.788854382, 2.387467277]

> map( x -> x + 1, { 1, 3, 5 } );
                               {2, 4, 6}

> sqrt~( [ 1.1, 3.2, 5.7 ] );
                [1.048808848, 1.788854382, 2.387467277]

> (x -> x + 1)~( { 1, 3, 5 } );
                               {2, 4, 6}

```

For Arrays (Vectors, Matrices, etc.) both map and trailing tilde also work, and by default create a new object, leaving the input Array unchanged.

```Maple

> a := Array( [ 1.1, 3.2, 5.7 ] );
                          a := [1.1, 3.2, 5.7]

> sqrt~( a );
                [1.048808848, 1.788854382, 2.387467277]

> a;
                            [1.1, 3.2, 5.7]

> map( sqrt, a );
                [1.048808848, 1.788854382, 2.387467277]

> a;
                            [1.1, 3.2, 5.7]

```

However, since these are mutable data structures in Maple, it is possible to use map to modify its input according to the applied procedure.

```Maple

> map[inplace]( sqrt, a );
                [1.048808848, 1.788854382, 2.387467277]

> a;
                [1.048808848, 1.788854382, 2.387467277]

```

The Array a has been modified.

It is also possible to pass additional arguments to the mapped procedure.

```Maple

> map( `+`, [ 1, 2, 3 ], 3 );
                               [4, 5, 6]

```

Passing additional arguments *before* the arguments from the mapped data structure is achieved using map2, or the more general map[n] procedure.

```Maple

> map2( `-`, 5, [ 1, 2, 3 ] );
                               [4, 3, 2]

> map[2]( `/`, 5, [ 1, 2, 3 ] );
                             [5, 5/2, 5/3]

```



## Mathematica


```Mathematica
(#*#)& /@ {1, 2, 3, 4}

Map[Function[#*#], {1, 2, 3, 4}]

Map[((#*#)&,{1,2,3,4}]

Map[Function[w,w*w],{1,2,3,4}]
```



## MATLAB

There are two types of arrays in MATLAB: arrays and cell arrays. MATLAB includes two functions, one for each of these data types, that accomplish the specification for this task. For arrays, we use "arrayfun()"; for cell arrays we use "cellfun()".<br />
Example:
For both of these function the first argument is a function handle for the function we would like to apply to each element. The second argument is the array whose elements are modified by the function. The function can be any function, including user defined functions.

```MATLAB>>
 array = [1 2 3 4 5]

array =

     1     2     3     4     5

>> arrayfun(@sin,array)

ans =

  Columns 1 through 4

   0.841470984807897   0.909297426825682   0.141120008059867  -0.756802495307928

  Column 5

  -0.958924274663138

>> cellarray = {1,2,3,4,5}

cellarray = 

    [1]    [2]    [3]    [4]    [5]

>> cellfun(@tan,cellarray)

ans =

  Columns 1 through 4

   1.557407724654902  -2.185039863261519  -0.142546543074278   1.157821282349578

  Column 5

  -3.380515006246586
```



## Maxima


```maxima
/* for lists or sets */

map(sin, [1, 2, 3, 4]);
map(sin, {1, 2, 3, 4});

/* for matrices */

matrixmap(sin, matrix([1, 2], [2, 4]));
```



## min

{{works with|min|0.19.3}}

```min
(1 2 3 4 5) (sqrt puts) foreach   ; print each square root
(1 2 3 4 5) 'sqrt map             ; collect return values
```


=={{header|Modula-3}}==

```modula3
MODULE Callback EXPORTS Main;

IMPORT IO, Fmt;

TYPE CallBack = PROCEDURE (a: CARDINAL; b: INTEGER);
     Values = REF ARRAY OF INTEGER;

VAR sample := ARRAY [1..5] OF INTEGER {5, 4, 3, 2, 1};
    callback := Display;

PROCEDURE Display(loc: CARDINAL; val: INTEGER) =
  BEGIN
    IO.Put("array[" & Fmt.Int(loc) & "] = " & Fmt.Int(val * val) & "\n");
  END Display;

PROCEDURE Map(VAR values: ARRAY OF INTEGER; size: CARDINAL; worker: CallBack) =
  VAR lvalues := NEW(Values, size);
  BEGIN
    FOR i := FIRST(lvalues^) TO LAST(lvalues^) DO
      worker(i, values[i]);
    END;
  END Map;

BEGIN
  Map(sample, NUMBER(sample), callback);
END Callback.
```



## Nemerle

The <tt>Nemerle.Collections</tt> namespace defines the methods <tt>Iter()</tt> (if the function applied is <tt>void</tt>) and <tt>Map()</tt> (if the function applied returns a value).

```Nemerle
def seg = array[1, 2, 3, 5, 8, 13];
def squares = seq.Map(x => x*x);
```



## NewLISP



```NewLISP>
 (map (fn (x) (* x x)) '(1 2 3 4))
(1 4 9 16)

```



## NGS


```NGS
{
	[1, 2, 3, 4, 5].map(F(x) x*x)
}
```



## Nial



```nial
each (* [first, first] ) 1 2 3 4
=1 4 9 16
```



## Nim



```nim
var arr = @[1,2,3,4]
arr.apply proc(some: var int) = echo(some, " squared = ", some*some)
```


{{Out}}
  1 squared = 1
  2 squared = 4
  3 squared = 9
  4 squared = 16

=={{header|Oberon-2}}==
{{Works with|oo2x}}

```oberon2

MODULE ApplyCallBack;
IMPORT
  Out := NPCT:Console;

TYPE
  Fun = PROCEDURE (x: LONGINT): LONGINT;
  Ptr2Ary = POINTER TO ARRAY OF LONGINT;

VAR
  a: ARRAY 5 OF LONGINT;
  x: ARRAY 3 OF LONGINT;
  r: Ptr2Ary;

  PROCEDURE Min(x,y: LONGINT): LONGINT;
  BEGIN
    IF x <= y THEN RETURN x ELSE RETURN y END;
  END Min;

  PROCEDURE Init(VAR a: ARRAY OF LONGINT);
  BEGIN
    a[0] := 0;
    a[1] := 1;
    a[2] := 2;
    a[3] := 3;
    a[4] := 4;
  END Init;

  PROCEDURE Fun1(x: LONGINT): LONGINT;
  BEGIN
    RETURN x * 2
  END Fun1;

  PROCEDURE Fun2(x: LONGINT): LONGINT;
  BEGIN
    RETURN x DIV 2;
  END Fun2;

  PROCEDURE Fun3(x: LONGINT): LONGINT;
  BEGIN
    RETURN x + 3;
  END Fun3;
  
  PROCEDURE Map(F: Fun; VAR x: ARRAY OF LONGINT);
  VAR
    i: LONGINT;
  BEGIN 
    FOR i := 0 TO LEN(x) - 1 DO
      x[i] := F(x[i])
    END
  END Map;

  PROCEDURE Map2(F: Fun; a: ARRAY OF LONGINT; VAR r: ARRAY OF LONGINT);
  VAR
    i,l: LONGINT;
  BEGIN
    l := Min(LEN(a),LEN(x));
    FOR i := 0 TO l - 1 DO 
      r[i] := F(a[i])
    END
  END Map2;

  PROCEDURE Map3(F: Fun; a: ARRAY OF LONGINT): Ptr2Ary;
  VAR
    r: Ptr2Ary;
    i: LONGINT;
  BEGIN
    NEW(r,LEN(a));
    FOR i := 0 TO LEN(a) - 1 DO
      r[i] := F(a[i]);
    END;
    RETURN r
  END Map3;

  PROCEDURE Show(a: ARRAY OF LONGINT);
  VAR
    i: LONGINT;
  BEGIN
    FOR i := 0 TO LEN(a) - 1 DO
      Out.Int(a[i],4)
    END;
    Out.Ln
  END Show;
  
BEGIN
  Init(a);Map(Fun1,a);Show(a);
  Init(a);Map2(Fun2,a,x);Show(x);
  Init(a);r := Map3(Fun3,a);Show(r^);
END ApplyCallBack.

```

{{Out}}

```txt

   0   2   4   6   8
   0   0   1
   3   4   5   6   7

```



## Objeck


```objeck

use Structure;

bundle Default {
  class Test {
    function : Main(args : String[]) ~ Nil {
      Run();
    }

    function : native : Run() ~ Nil {
      values := IntVector->New([1, 2, 3, 4, 5]);
      squares := values->Apply(Square(Int) ~ Int);
      each(i : squares) {
        squares->Get(i)->PrintLine();
      };
    }
    
    function : Square(value : Int) ~ Int {
      return value * value;
    }
  }
}

```



## OCaml

This function is part of the standard library:


```ocaml>Array.map</lang


Usage example:

```ocaml
let square x = x * x;;
let values = Array.init 10 ((+) 1);;
Array.map square values;;
```


Or with lists (which are more typical in OCaml):

```ocaml
let values = [1;2;3;4;5;6;7;8;9;10];;
List.map square values;;
```


Use <tt>iter</tt> if the applied function does not return a value.


```ocaml
Array.iter (fun x -> Printf.printf "%d" x) [|1; 2; 3; 4; 5|];;
```


```ocaml
List.iter (fun x -> Printf.printf "%d" x) [1; 2; 3; 4; 5];;
```


with partial application we can also write:


```ocaml
Array.iter (Printf.printf "%d") [|1; 2; 3; 4; 5|];;
```


```ocaml
List.iter (Printf.printf "%d") [1; 2; 3; 4; 5];;
```



## Octave


Almost all the built-in can operate on each element of a vector or matrix; e.g. sin([pi/2, pi, 2*pi]) computes the function sin on pi/2, pi and 2*pi (returning a vector). If a function does not accept vectors/matrices as arguments, the <tt>arrayfun</tt> can be used.


```octave
function e = f(x, y)
  e = x^2 + exp(-1/(y+1));
endfunction

% f([2,3], [1,4]) gives and error, but
arrayfun(@f, [2, 3], [1,4])
% works
```


(The function <tt>f</tt> can be rewritten so that it can accept vectors as argument simply changing operators to their dot ''relatives'': <code>e = x.^2 + exp(-1 ./ (y.+1))</code>)


## Oforth

apply allows to perform a function on all elements of a list : 

```Oforth
0 #+ [ 1, 2, 3, 4, 5 ] apply
```


map regroups all results into a new list : 

```Oforth
#sq [ 1, 2, 3, 4, 5 ] map
```



## Ol

Apply custom callback (lambda) to every element of list.

```scheme

(for-each
   (lambda (element)
      (display element))
   '(1 2 3 4 5))
; ==> 12345

```



## ooRexx

ooRexx doesn't directly support callbacks on array items, but this is pretty easy to implement using Routine objects. 

```ooRexx
start = .array~of("Rick", "Mike", "David", "Mark")
new = map(start, .routines~reversit)
call map new, .routines~sayit


-- a function to perform an iterated callback over an array
-- using the provided function.  Returns an array containing
-- each function result
::routine map
  use strict arg array, function
  resultArray = .array~new(array~items)
  do item over array
     resultArray~append(function~call(item))
  end
  return resultArray

::routine reversit
  use arg string
  return string~reverse

::routine sayit
  use arg string
  say string
  return .true   -- called as a function, so a result is required
```

{{out}}

```txt
kciR
ekiM
divaD
kraM
```



## Order

Both sequences and tuples support the usual map operation seen in many functional languages. Sequences also support <code>8seq_for_each</code>, and a few variations, which returns <code>8nil</code>.

```c>#include <order/interpreter.h


ORDER_PP( 8tuple_map(8fn(8X, 8times(8X, 8X)), 8tuple(1, 2, 3, 4, 5)) )
// -> (1,4,9,16,25)

ORDER_PP( 8seq_map(8fn(8X, 8times(8X, 8X)), 8seq(1, 2, 3, 4, 5)) )
// -> (1)(4)(9)(16)(25)

ORDER_PP( 8seq_for_each(8fn(8X, 8print(8X 8comma)), 8seq(1, 2, 3, 4, 5)) )
// prints 1,2,3,4,5, and returns 8nil
```



## Oz


```oz
declare
  fun{Square A}
    A*A
  end

  Lst = [1 2 3 4 5]
  
  %% apply a PROCEDURE to every element
  {ForAll Lst Show}

  %% apply a FUNCTION to every element
  Result = {Map Lst Square}
  {Show Result}
```



## PARI/GP

{{works with|PARI/GP|2.4.2 and above}}

```parigp
callback(n)=n+n;
apply(callback, [1,2,3,4,5])
```


This should be contrasted with <code>call</code>:

```parigp
call(callback, [1,2,3,4,5])
```

which is equivalent to <code>callback(1, 2, 3, 4, 5)</code> rather than <code>[callback(1), callback(2), callback(3), callback(4), callback(5)]</code>.


## Pascal

See [[Apply_a_callback_to_an_array#Delphi | Delphi]]


## Perl


```perl
# create array
my @a = (1, 2, 3, 4, 5);

# create callback function
sub mycallback {
  return 2 * shift;
}

# use array indexing
for (my $i = 0; $i < scalar @a; $i++) {
  print "mycallback($a[$i]) = ", mycallback($a[$i]), "\n";
}

# using foreach
foreach my $x (@a) {
  print "mycallback($x) = ", mycallback($x), "\n";
}

# using map (useful for transforming an array)
my @b = map mycallback($_), @a;                # @b is now (2, 4, 6, 8, 10)

# and the same using an anonymous function
my @c = map { $_ * 2 } @a;                     # @c is now (2, 4, 6, 8, 10)

# use a callback stored in a variable
my $func = \&mycallback;
my @d = map $func->($_), @a;                  # @d is now (2, 4, 6, 8, 10)

# filter an array 
my @e = grep { $_ % 2 == 0 } @a;               # @e is now (2, 4)
```



## Perl 6

{{works with|Rakudo|2015.10-11}}


```perl6
sub function { 2 * $^x + 3 };
my @array = 1 .. 5;

# via map function
.say for map &function, @array;

# via map method
.say for @array.map(&function);

# via for loop
for @array {
    say function($_);
}

# via the "hyper" metaoperator and method indirection
say @array».&function;

# we neither need a variable for the array nor for the function
say [1,2,3]>>.&({ $^x + 1});

```



## Phix


```Phix
function apply(integer f, sequence s)
-- apply function f to all elements of sequence s
    for i = 1 to length(s) do
        s[i] = call_func(f, {s[i]})
    end for
    return s
end function

function add1(integer x)
    return x + 1
end function

? apply(routine_id("add1"),{1,2,3})
```

{{out}}

```txt

{2,3,4}

```



## PHP


```php
function cube($n)
{
   return($n * $n * $n);
}

$a = array(1, 2, 3, 4, 5);
$b = array_map("cube", $a);
print_r($b);
```



## PicoLisp


```PicoLisp
: (mapc println (1 2 3 4 5))  # Print numbers
1
2
3
4
5
-> 5

: (mapcar '((N) (* N N)) (1 2 3 4 5))  # Calculate squares
-> (1 4 9 16 25)

: (mapcar ** (1 2 3 4 5) (2 .))  # Same, using a circular list
-> (1 4 9 16 25)

: (mapcar if '(T NIL T NIL) '(1 2 3 4) '(5 6 7 8))  # Conditional function
-> (1 6 3 8)
```



## Pike


```pike
int cube(int n)
{
    return n*n*n;
}

array(int) a = ({ 1,2,3,4,5 });
array(int) b = cube(a[*]);      // automap operator
array(int) c = map(a, cube);    // conventional map function
```



## PL/I


```PL/I
   declare x(5) initial (1,3,5,7,8);
   x = sqrt(x);
   x = sin(x);
```



## PL/SQL

PL/SQL doesn't have callbacks, though we can pass around an object and use its method to simulate one. Further, this callback method can be defined in an abstract class that the mapping function will expect.

```plsql
-- Let's create a generic class with one method to be used as an interface:
create or replace
TYPE callback AS OBJECT (
    -- A class needs at least one member even though we don't use it
    -- There's no generic OBJECT type, so let's call it NUMBER
    dummy NUMBER,
    -- Here's our function, and since PL/SQL doesn't have generics,
    -- let's use type NUMBER for our params
    MEMBER FUNCTION exec(n number) RETURN number
) NOT FINAL not instantiable;
/

-- Now let's inherit from that, defining a class with one method. We'll have ours square a number.
-- We can pass this class into any function that takes type callback:
CREATE OR REPLACE TYPE CB_SQUARE under callback (
    OVERRIDING MEMBER FUNCTION exec(n NUMBER) RETURN NUMBER
)
/
CREATE OR REPLACE
TYPE BODY CB_SQUARE AS
    OVERRIDING MEMBER FUNCTION exec(n NUMBER) RETURN NUMBER IS
    BEGIN
        RETURN n * n;
    END exec;
END;
/

-- And a package to hold our test
CREATE OR REPLACE 
PACKAGE PKG_CALLBACK AS 
    myCallback cb_square;
    TYPE intTable IS TABLE OF NUMBER INDEX BY BINARY_INTEGER;
    ints intTable;
    i PLS_INTEGER;
    
    procedure test_callback;
END PKG_CALLBACK;
/

CREATE OR REPLACE PACKAGE BODY PKG_CALLBACK AS
    -- Our generic mapping function that takes a "method" and a collection
    -- Note that it takes the generic callback type 
    -- that doesn't know anything about squaring
    procedure do_callback(myCallback IN callback, ints IN OUT intTable) IS
        i PLS_INTEGER;
        myInt NUMBER;
    begin
        for i in 1 .. ints.count loop
            myInt := ints(i);
            -- PL/SQL call's the child's method
            ints(i) := myCallback.exec(myInt);
        END LOOP;
    end do_callback;

    procedure test_callback IS
    BEGIN
        myCallback := cb_square(null);
        FOR i IN 1..5 LOOP
            ints(i) := i;
        END LOOP;
    
        do_callback(myCallback, ints);
    
        i := ints.FIRST;
        WHILE i IS NOT NULL LOOP
            DBMS_OUTPUT.put_line(ints(i));
            i := ints.next(i);
        END LOOP;
    END test_callback;
END PKG_CALLBACK;
/

BEGIN
  PKG_CALLBACK.TEST_CALLBACK();
END;
/
```



## Pop11



```pop11
;;; Define a procedure
define proc(x);
    printf(x*x, '%p,');
enddefine;

;;; Create array
lvars ar = { 1 2 3 4 5};

;;; Apply procedure to array
appdata(ar, proc);
```


If one wants to create a new array consisting of transformed values then procedure mapdata may be more convenient.


## PostScript

The <code>forall</code> operator applies a procedure to each element of an array, a packed array or a string.

```postscript
[1 2 3 4 5] { dup mul = } forall
```

In this case the respective square numbers for the elements are printed.

To create a new array from the results above code can simply be wrapped in <code>[]</code>:

```postscript
[ [1 2 3 4 5] { dup mul } forall ]
```


{{libheader|initlib}}

```postscript

[1 2 3 4 5] {dup *} map

```



## PowerShell

This can be done in PowerShell with the <code>ForEach-Object</code> cmdlet which applies a scriptblock to each element of an array:

```powershell
1..5 | ForEach-Object { $_ * $_ }
```

To recreate a ''map'' function, found in other languages the same method applies:

```powershell
function map ([array] $a, [scriptblock] $s) {
    $a | ForEach-Object $s
}
map (1..5) { $_ * $_ }
```



## Prolog

Prolog doesn't have arrays, but we can do it with lists. This can be done in the console mode. 

```Prolog
 ?- assert((fun(X, Y) :- Y is 2 * X)).
true.

?- maplist(fun, [1,2,3,4,5], L).
L = [2,4,6,8,10].

```



## PureBasic


```PureBasic
Procedure Cube(Array param.i(1))
    Protected n.i
    For n = 0 To ArraySize(param())
        Debug Str(param(n)) + "^3 = " + Str(param(n) * param(n) * param(n))
    Next 
EndProcedure 

Dim AnArray.i(4)

For n = 0 To ArraySize(AnArray()) 
    AnArray(n) = Random(99)
Next 

Cube(AnArray()) 
```



## Python


```python
def square(n):
    return n * n
  
numbers = [1, 3, 5, 7]

squares1 = [square(n) for n in numbers]     # list comprehension

squares2a = map(square, numbers)            # functional form

squares2b = map(lambda x: x*x, numbers)     # functional form with `lambda`

squares3 = [n * n for n in numbers]         # no need for a function,
                                            # anonymous or otherwise

isquares1 = (n * n for n in numbers)        # iterator, lazy

import itertools
isquares2 = itertools.imap(square, numbers) # iterator, lazy
```

To print squares of integers in the range from 0 to 9, type:

```python
print " ".join(str(n * n) for n in range(10))
```

Or:

```python
print " ".join(map(str, map(square, range(10))))
```

Result:

```python>0 1 4 9 16 25 36 49 64 81</lang



## R

Many functions can take advantage of implicit vectorisation, e.g.

```R
cube <- function(x) x*x*x
elements <- 1:5
cubes <- cube(elements)
```

Explicit looping over array elements is also possible.

```R
cubes <- numeric(5)
for(i in seq_along(cubes))
{
   cubes[i] <- cube(elements[i])
}
```

Loop syntax can often simplified using the [http://stat.ethz.ch/R-manual/R-patched/library/base/html/apply.html *apply] family of functions.

```R
elements2 <- list(1,2,3,4,5)
cubes <- sapply(elements2, cube)
```

In each case above, the value of 'cubes' is 
 1   8  27  64 125


## Racket



```racket

#lang racket

;; using the `for/vector' comprehension form
(for/vector ([i #(1 2 3 4 5)]) (sqr i))

;; the usual functional `map'
(vector-map sqr #(1 2 3 4 5))

```



## Raven


```raven
# To print the squared elements
[1 2 3 4 5] each dup * print
```



```raven
# To obtain a new array
group [1 2 3 4 5] each
  dup *
list
```



## REBOL


```REBOL
REBOL [
    Title: "Array Callback"
    URL: http://rosettacode.org/wiki/Apply_a_callback_to_an_Array
]

map: func [
	"Apply a function across an array."
	f [native! function!] "Function to apply to each element of array."
	a [block!] "Array to process."
	/local x
][x: copy []  forall a [append x do [f a/1]]  x]

square: func [x][x * x]

; Tests:

assert: func [code][print [either do code ["  ok"]["FAIL"]  mold code]]

print "Simple loop, modify in place:"
assert [[1 100 81] = (a: [1 10 9]  forall a [a/1: square a/1]  a)]

print [crlf "Functional style with 'map':"]
assert [[4 16 36] = map :square [2 4 6]]

print [crlf "Applying native function with 'map':"]
assert [[2 4 6] = map :square-root [4 16 36]]
```


{{Out}}

```txt
Simple loop, modify in place:
  ok [[1 100 81] = (a: [1 100 81] forall a [a/1: square a/1] a)]

Functional style with 'map':
  ok [[4 16 36] = map :square [2 4 6]]

Applying native function with 'map':
  ok [[2 4 6] = map :square-root [4 16 36]]
```



## Retro


Retro provides a variety of array words. Using these to multiply each value in an array by 10 and display the results:


```Retro
{ #1 #2 #3 #4 #5 } [ #10 * ] a:map [ n:put sp ] a:for-each
```



## REXX


```rexx
/*REXX pgm applies a callback to an array (using factorials for demonstration)*/
a.=;     b.=;         a.0  =  0
                      a.1  =  1
                      a.2  =  2
                      a.3  =  3
                      a.4  =  4
                      a.5  =  5
                      a.6  =  6
                      a.7  =  7
                      a.8  =  8
                      a.9  =  9
                      a.10 = 10
call listAB  'before'
call bangit  'a','b'           /*factorialize the A array, store results───►B.*/
call listAB  ' after'
exit                                   /*stick a fork in it,  we're all done. */
/*────────────────────────────────────────────────────────────────────────────*/
bangit:   do i=0;    _=value(arg(1)'.'i);         if _=='' then return
          call value arg(2)'.'i, fact(_)
          end    /*i*/
/*────────────────────────────────────────────────────────────────────────────*/
fact: procedure; !=1;        do j=2  to arg(1);   !=!*j;   end;         return !
/*────────────────────────────────────────────────────────────────────────────*/
listAB:   do j=0  while a.j\=='';    say arg(1) 'a.'j"="a.j;    end  /*j*/;  say
          do k=0  while b.k\=='';    say arg(1) 'b.'k"="b.k;    end  /*k*/
return
```

{{Out}}

```txt

before a.0=0
before a.1=1
before a.2=2
before a.3=3
before a.4=4
before a.5=5
before a.6=6
before a.7=7
before a.8=8
before a.9=9
before a.10=10

 after a.0=0
 after a.1=1
 after a.2=2
 after a.3=3
 after a.4=4
 after a.5=5
 after a.6=6
 after a.7=7
 after a.8=8
 after a.9=9
 after a.10=10

 after b.0=1
 after b.1=1
 after b.2=2
 after b.3=6
 after b.4=24
 after b.5=120
 after b.6=720
 after b.7=5040
 after b.8=40320
 after b.9=362880
 after b.10=3628800

```



## RLaB

RLaB has two type of arrays: 'standard' or 1-dimensional, that can be a row-
or a column-vectory; and, 'associative' which are called lists.
For standard array its entry identifier (index) is an integer in
range 1:N where N is the size of the array.
For associative array its entry identifier is a string consisting of printable
ASCII characters.

All scalar mathematical functions are 'matrix-optimized' meaning that if the argument
to a function is a matrix, then the return value of the function is a matrix of the
same size as the input argument, where the function is applied to the individual entries
of the matrix.
Consider an example:


```RLaB

>> x = rand(2,4)
 0.707213207   0.275298961   0.396757763   0.232312312
 0.215619868   0.207078017   0.565700032   0.666090571
>> sin(x)
 0.649717845   0.271834652   0.386430003   0.230228332
 0.213952984   0.205601224   0.536006923   0.617916954

```


This can be done on entry-by-entry basis, but one has to keep in mind that the
'for' or 'while' loops are slow in interpreted languages, and RLaB is no exception.


```RLaB

x = rand(2,4);
y = zeros(2,4);
for (i in 1:2)
{
  for (j in 1:4)
  {
    y[i;j] = sin( x[i;j] );
  }
}

```



The functions can take lists as arguments, but then it has to be specified within the body
of the function what to do with the list elements. Given a list call it 'x' there is a RLaB 
function 'members' which returns a string vector with the names of the elements of the list.


```RLaB

x = <<>>;
for (i in 1:9)
{
  x.[i] = rand();
}

y = <<>>;
for (i in members(x))
{
  y.[i] = sin( x.[i] );
}

```



## Ring


```ring

for x in [1,2,3,4,5]
    x = x*x
next

```



## Ruby

You could use a traditional "for i in arr" approach like below:

```ruby
for i in [1,2,3,4,5] do
   puts i**2
end
```


Or you could  the more preferred ruby way of an iterator (which is borrowed from SmallTalk)

```ruby
[1,2,3,4,5].each{ |i| puts i**2 }
```


To create a new array of each value squared

```ruby
[1,2,3,4,5].map{ |i| i**2 }
```



## Rust



```rust
fn echo(n: &i32) {
    println!("{}", n);
}

fn main() {
    let a: [i32; 5];
    a = [1, 2, 3, 4, 5];
    let _: Vec<_> = a.into_iter().map(echo).collect();
}
```



## Salmon


These examples apply the square function to a list of the numbers from 0 through 9 to produce a new list of their squares, then iterate over the resulting list and print the squares.


```Salmon
function apply(list, ageless to_apply)
  (comprehend(x; list) (to_apply(x)));

function square(x) (x*x);

iterate(x; apply([0...9], square))
    x!;
```


With short identifiers:


```Salmon
include "short.salm";

fun apply(list, ageless to_apply)
  (comp(x; list) (to_apply(x)));

fun square(x) (x*x);

iter(x; apply([0...9], square))
    x!;
```


With the numbers given as a list of individual elements:


```Salmon
function apply(list, to_apply)
  (comprehend(x; list) (to_apply(x)));

function square(x) (x*x);

iterate(x; apply([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], square))
    x!;
```



## Sather


```sather
class MAIN is
  do_something(i:INT):INT is
    return i * i;
  end;

  main is
    a:ARRAY{INT} := |1, 2, 3, 4, 5|;
    -- we use an anonymous closure to apply our do_something "callback"
    a.map(bind(do_something(_)));
    loop #OUT + a.elt! + "\n"; end;
  end;
end;
```



## Scala


```scala
val l = List(1,2,3,4)
l.foreach {i => println(i)}
```


When the argument appears only once -as here, i appears only one in println(i) - it may be shortened to

```scala
l.foreach(println(_))
```

Same for an array

```scala
val a = Array(1,2,3,4)
a.foreach {i => println(i)}
a.foreach(println(_))  '' // same as previous line''
```


Or for an externally defined function:

```scala
def doSomething(in: int) = {println("Doing something with "+in)}
l.foreach(doSomething)
```


There is also a ''for'' syntax, which is internally rewritten to call foreach. A foreach method must be defined on ''a''

```scala
for(val i <- a) println(i)
```


It is also possible to apply a function on each item of an list to get a new list (same on array and most collections)

```scala
val squares = l.map{i => i * i} ''//squares is''  List(1,4,9,16)
```


Or the equivalent ''for'' syntax, with the additional keyword ''yield'', map is called instead of foreach

```scala
val squares = for (val i <- l) yield i * i
```



## Scheme


```scheme
(define (square n) (* n n))
(define x #(1 2 3 4 5))
(map square (vector->list x))
```


A single-line variation

```scheme
(map (lambda (n) (* n n)) '(1 2 3 4 5))
```


For completeness, the <tt>map</tt> function (which is R5RS standard) can be coded as follows:

```scheme
(define (map f L)
  (if (null? L)
      L
      (cons (f (car L)) (map f (cdr L)))))
```



## Sidef

Defining a callback function:

```ruby
func callback(i) { say i**2 }
```


The function will get called for each element:

```ruby
[1,2,3,4].each(callback)
```


Same as above, but with the function inlined:

```ruby
[1,2,3,4].each{|i| say i**2 }
```


For creating a new array, we can use the Array.map method:

```ruby
[1,2,3,4,5].map{|i| i**2 }
```


## Simula


```simula
BEGIN

    ! APPLIES A CALLBACK FUNCTION TO AN ARRAY ;
    PROCEDURE APPLY(ARR, FUN);
        REAL ARRAY ARR;
        PROCEDURE FUN IS REAL PROCEDURE FUN(X); REAL X;;
    BEGIN
        INTEGER I;
        FOR I := LOWERBOUND(ARR, 1) STEP 1 UNTIL UPPERBOUND(ARR, 1) DO
            ARR(I) := FUN(ARR(I));
    END APPLY;

    ! CALLBACK ;
    REAL PROCEDURE SQUARE(X); REAL X; SQUARE := X * X;

    REAL ARRAY A(1:5);
    INTEGER I;
    FOR I := 1 STEP 1 UNTIL 5 DO A(I) := I;
    APPLY(A, SQUARE);
    FOR I := 1 STEP 1 UNTIL 5 DO OUTFIX(A(I), 2, 8); OUTIMAGE;

END.
```

{{out}}

```txt

    1.00    4.00    9.00   16.00   25.00

```



## Slate


```slate
#( 1 2 3 4 5 ) collect: [| :n | n * n].
```



## Smalltalk


```smalltalk
#( 1 2 3 4 5 ) collect: [:n | n * n].
```



## Sparkling

The <tt>foreach</tt> function calls the supplied callback on each element of the (possibly associative) array, passing it each key and the corresponding value:

```sparkling
let numbers = { 1, 2, 3, 4 };
foreach(numbers, function(idx, num) {
    print(num);
});
```


The <tt>map</tt> function applies the transform to each key-value pair and constructs a new array, of which the keys are the keys of the original array, and the corresponding values are the return values of each call to the transform function:

```sparkling
let dict = { "foo": 42, "bar": 13, "baz": 37 };
let doubled = map(dict, function(key, val) {
    return val * 2;
});
```



## SQL PL

{{works with|Db2 LUW}} version 9.7 or higher.
With SQL PL:

```sql pl

--#SET TERMINATOR @

SET SERVEROUTPUT ON @

BEGIN
 DECLARE TYPE NUMBERS AS SMALLINT ARRAY[5];
 DECLARE NUMBERS NUMBERS;
 DECLARE I SMALLINT;

 SET I = 1;
 WHILE (I <= 5) DO
  SET NUMBERS[I] = I;
  SET I = I + 1;
 END WHILE;

 BEGIN
  DECLARE PROCEDURE PRINT_SQUARE (
    IN VALUE SMALLINT
   )
  BEGIN
   CALL DBMS_OUTPUT.PUT(VALUE * VALUE || ' ');
  END;

  SET I = 1;
  WHILE (I <= 5) DO
   CALL PRINT_SQUARE(NUMBERS[I]);
   SET I = I + 1;
  END WHILE;
  CALL DBMS_OUTPUT.PUT_LINE('');
 END;
END @

```

Output:

```txt

db2 -td@
db2 => BEGIN
...
db2 (cont.) => END @
DB20000I  The SQL command completed successfully.

1 4 9 16 25 

```



## Standard ML


```Standard ML

map f l

```

i.e.

```Standard ML

map (fn x=>x+1) [1,2,3];; (* [2,3,4] *)

```



## Stata

There is no 'map' function in Mata, but it's easy to implement. Notice that you can only pass functions that are written in Mata, no builtin ones. For instance, the trigonometric functions (cos, sin) or the exponential are builtin. To pass a builtin function to another function, one needs to write a wrapper in Mata. See also Stata help about '''[https://www.stata.com/help.cgi?m2_pointers pointers]''' and '''[https://www.stata.com/help.cgi?m2_ftof passing functions to functions]'''. There are two versions of the function: one to return a numeric array, another to return a string array.


```stata
function map(f,a) {
	nr = rows(a)
	nc = cols(a)
	b = J(nr,nc,.)
	for (i=1;i<=nr;i++) {
		for (j=1;j<=nc;j++) b[i,j] = (*f)(a[i,j])
	}
	return(b)
}

function maps(f,a) {
	nr = rows(a)
	nc = cols(a)
	b = J(nr,nc,"")
	for (i=1;i<=nr;i++) {
		for (j=1;j<=nc;j++) b[i,j] = (*f)(a[i,j])
	}
	return(b)
}

function square(x) {
	return(x*x)
}
```


'''Output'''


```txt
: map(&square(),(1,2,3\4,5,6))
        1    2    3
    +----------------+
  1 |   1    4    9  |
  2 |  16   25   36  |
    +----------------+
```



## SuperCollider

Actually, there is a builtin <tt>squared</tt> operator:

```SuperCollider
[1, 2, 3].squared  // returns [1, 4, 9]
```

Anything that is a <tt>Collection</tt> can be used with <tt>collect</tt>:

```SuperCollider
[1, 2, 3].collect { |x| x * x }
```

[[List Comprehension#SuperCollider|List comprehension]] combined with a higher-order function can also be used:

```SuperCollider
var square = { |x| x * x };
var map = { |fn, xs|
  all {: fn.value(x), x <- xs };
};
map.value(square, [1, 2, 3]);
```



## Swift


```swift
func square(n: Int) -> Int {
    return n * n
}

let numbers = [1, 3, 5, 7]

let squares1a = numbers.map(square)         // map method on array

let squares1b = numbers.map {x in x*x}      // map method on array with anonymous function

let squares1b = numbers.map { $0 * $0 }      // map method on array with anonymous function and unnamed parameters

let isquares1 = numbers.lazy.map(square)   // lazy sequence
```



## Tailspin


```tailspin

def numbers: [1,3,7,10];

templates cube
  $ * $ * $ !
end cube

// Using inline array templates (which also allows access to index by $i)
$numbers -> [i]($ * $i !) -> !OUT::write
$numbers -> [i]($ * $ !) -> !OUT::write
$numbers -> [i]($ -> cube !) -> !OUT::write

// Using array literal and deconstructor
[ $numbers... -> $ * $ ] -> !OUT::write
[ $numbers... -> cube ] -> !OUT::write

```



## Tcl


If I wanted to call "<tt>myfunc</tt>" on each element of <tt>dat</tt> and <tt>dat</tt> were a list:

```tcl
foreach var $dat {
    myfunc $var
}
```

This does not retain any of the values returned by <tt>myfunc</tt>.

if <tt>dat</tt> were an (associative) array, however:

```tcl
foreach name [array names dat] {
    myfunc $dat($name)
}
```


More functional, with a simple <code>map</code> function:

```Tcl
proc map {f list} {
   set res {}
   foreach e $list {lappend res [$f $e]}
   return $res
}
proc square x {expr {$x*$x}}

% map square {1 2 3 4 5}
1 4 9 16 25
```


=={{header|TI-89 BASIC}}==


```ti89b
© For no return value
Define foreach(fe_cname,fe_list) = Prgm
  Local fe_i
  For fe_i,1,dim(fe_list)
    #fe_cname(fe_list[fe_i])
  EndFor
EndPrgm

© For a list of results
Define map(map_cnam,map_list) = seq(#map_cnam(map_list[map_i]),map_i,1,dim(map_list))

Define callback(elem) = Prgm
  Disp elem
EndPrgm

foreach("callback", {1,2,3,4,5})
Disp map("√", {1,2,3,4,5})
```


{{Out}}
<math>1</math>

<math>2</math>

<math>3</math>

<math>4</math>

<math>5</math>

<math>\begin{Bmatrix}1 & \sqrt{2} & \sqrt{3} & 2 & \sqrt{5}\end{Bmatrix}</math>


## TIScript


JavaScript alike:


```javascript
var a = [1, 2, 3, 4, 5];
a.map(function(v) { return v * v; })

```


Using short form of lambda notation:

```javascript
var a = [1, 2, 3, 4, 5];
a.map( :v: v*v );

```



## Toka



```toka
( array count function -- )
{
  value| array fn |
  [ i array ] is I
  [ to fn swap to array 0 swap [ I array.get :stack fn invoke I array.put ] countedLoop ]
} is map-array

( Build an array )
5 cells is-array a
10 0 a array.put
11 1 a array.put
12 2 a array.put
13 3 a array.put
14 4 a array.put

( Add 1 to each item in the array )
a 5  [ 1 + ] map-array
```



## TorqueScript


--[[User:Elm|Elm]] 03:41, 18 June 2012 (UTC)

Callbacks:


```TorqueScript

function map(%array,%arrayCount,%function)
{
	for(%i=0;%i<%arrayCount;%i++)
	{
		eval("%a = "@%array@"["@%i@"];");
		eval(""@%function@"("@%a@");");
	}
}

```


Now to set up an array:


```TorqueScript

$array[0] = "Hello.";
$array[1] = "Hi.";
$array[2] = "How are you?";

```


Now to call the function correctly:


```TorqueScript

map("$array",3,"echo");

```


Which should result in:


```TorqueScript

=> Hello.

=> Hi.

=> How are you?

```



## TXR


Print 1 through 10 out of a vector, using <code>prinl</code> the callback, right from the system shell command prompt:


```bash
$ txr -e '[mapdo prinl #(1 2 3 4 5 6 7 8 9 10)]'
1
2
3
4
5
6
7
8
9
10
```


<code>mapdo</code> is like <code>mapcar</code> but doesn't accumulate a list, suitable for imperative programming situations when the function is invoked to perform a side effect.

TXR extends Lisp list processing primitives to work with vectors and strings also, which is why <code>mapdo</code> cheerfully traverses a vector.


## uBasic/4tH

We cannot transfer the array address, since uBasic/4tH has only got one, but we can transfer the function pointer and size.
<lang>S = 5                                  ' Size of the array

For x = 0 To S - 1                     ' Initialize array
  @(x) = x + 1
Next

Proc _MapArray (_SquareRoot, S)        ' Call mapping procedure

For x = 0 To S - 1                     ' Print results
  Print "SQRT(";x+1;") = ";Using "#.####";@(x)
Next

For x = 0 To S - 1                     ' Reinitialize array
  @(x) = x + 1
Next

Proc _MapArray (_Cosine, S)            ' Call mapping procedure

Print : For x = 0 To S - 1             ' Print results
  Print "COS(";x+1;") = ";Using "#.####";@(x)
Next

End


_MapArray Param(2)                     ' Param(1) = function
  Local (1)                            ' Param(2) = array size

  For c@ = 0 To b@ - 1
    @(c@) = FUNC(a@(@(c@)))
  Next
Return


_SquareRoot Param (1)                  ' This is an integer SQR subroutine
  Local (2)

  b@ = (10^(4*2)) * a@                 ' Output is scaled by 10^4
  a@ = b@

  Do
    c@ = (a@ + (b@ / a@))/2
  Until (Abs(a@ - c@) < 2)
    a@ = c@
  Loop

Return (c@)


_Cosine Param(1)                       ' This is an integer COS subroutine
  Push Abs((a@*10000)%62832)           ' Output is scaled by 10^4
  If Tos()>31416 Then Push 62832-Pop()
  Let a@=Tos()>15708
  If a@ Then Push 31416-Pop()
  Push Tos()
  Push (Pop()*Pop())/10000
  Push 10000+((10000*-(Tos()/56))/10000)
  Push 10000+((Pop()*-(Tos()/30))/10000)
  Push 10000+((Pop()*-(Tos()/12))/10000)
  Push 10000+((Pop()*-(Pop()/2))/10000)
  If a@ Then Push -Pop()               ' Result is directly transferred
Return                                 ' through the stack
```

{{out}}

```txt
SQRT(1) = 1.0000
SQRT(2) = 1.4142
SQRT(3) = 1.7320
SQRT(4) = 2.0000
SQRT(5) = 2.2360

COS(1) = 0.5403
COS(2) = -0.4162
COS(3) = -0.9901
COS(4) = -0.6537
COS(5) = 0.2837

0 OK, 0:514
```


## UNIX Shell

{{works with|Bourne Shell}}

```bash
map() {
	map_command=$1
	shift
	for i do "$map_command" "$i"; done
}
list=1:2:3
(IFS=:; map echo $list)
```


{{works with|ksh93}}
{{works with|pdksh}}
{{works with|zsh}}

```bash
map() {
	typeset command=$1
	shift
	for i do "$command" "$i"; done
}
set -A ary 1 2 3
map print "${ary[@]}"
```


{{works with|zsh}}

```bash
map(){for i ($*[2,-1]) $1 $i}
a=(1 2 3)
map print $a
```



## Ursala

The * is a built-in map operator.
This example shows a map of the successor function over a list of natural numbers.

```Ursala
#import nat

#cast %nL

demo = successor* <325,32,67,1,3,7,315>
```

{{Out}}

```txt

<326,33,68,2,4,8,316>

```



## V

apply squaring (dup *) to each member of collection

```v
[1 2 3 4] [dup *] map
```


## VBA


```vb

Option Explicit

Sub Main()
Dim arr, i
    'init
    arr = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    'Loop and apply a function (Fibonacci) to each element
    For i = LBound(arr) To UBound(arr): arr(i) = Fibonacci(arr(i)): Next
    
    'return
    Debug.Print Join(arr, ", ")
End Sub

Private Function Fibonacci(N) As Variant
    If N <= 1 Then
        Fibonacci = N
    Else
        Fibonacci = Fibonacci(N - 1) + Fibonacci(N - 2)
    End If
End Function
```

{{out}}

```txt
0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55
```



## VBScript

I really have my doubts as to whether this really counts as a callback. 
I used the same thing in the solution to Amb.


### ==Implementation==


```vb

class callback
	dim sRule

	public property let rule( x )
		sRule = x
	end property
	
	public default function applyTo(a)
		dim p1
		for i = lbound( a ) to ubound( a )
			p1 = a( i )
			a( i ) = eval( sRule )
		next
		applyTo = a
	end function
end class

```



### ==Invocation==


```vb

dim a1
dim cb
set cb = new callback

cb.rule = "ucase(p1)"
a1 = split("my dog has fleas", " " )
cb.applyTo a1
wscript.echo join( a1, " " )

cb.rule = "p1 ^ p1"
a1 = array(1,2,3,4,5,6,7,8,9,10)
cb.applyto a1
wscript.echo join( a1, ", " )

```


{{Out}}

```txt

MY DOG HAS FLEAS
1, 4, 27, 256, 3125, 46656, 823543, 16777216, 387420489, 10000000000

```



## Vim Script

<code>map()</code> works with lists and dictionaries. 
The second argument is an expression string where <code>v:val</code> is replaced by the current value and <code>v:key</code> by the current key (for lists the key is the index). 
The result of evaluating the string will be the new value. 
The list/dictionary is modified in place.

```vim
echo map([10, 20, 30], 'v:val * v:val')
echo map([10, 20, 30], '"Element " . v:key . " = " . v:val')
echo map({"a": "foo", "b": "Bar", "c": "BaZ"}, 'toupper(v:val)')
echo map({"a": "foo", "b": "Bar", "c": "BaZ"}, 'toupper(v:key)')
```


{{Out}}

```txt
[100, 400, 900]                                                                 
['Element 0 = 10', 'Element 1 = 20', 'Element 2 = 30']                          
{'a': 'FOO', 'b': 'BAR', 'c': 'BAZ'}                                            
{'a': 'A', 'b': 'B', 'c': 'C'}
```



## Visual Basic .NET

'''Compiler:''' >= Visual Studio 2008

The .NET framework has got us covered.
 
System.Array.ForEach(T(), Action(Of T)) maps a non-value-returning callback,

System.Linq.Enumerable.Select(Of TSource,TResult)(IEnumerable(Of TSource), Func(Of TSource, TResult)) provides a way to lazily map a function, resulting in an IEnumerable(Of T),

and System.Linq.Enumerable.ToArray(Of TSource)(IEnumerable(Of TSource)) eagerly converts the enumerable to an array.


```vbnet
Module Program
    Function OneMoreThan(i As Integer) As Integer
        Return i + 1
    End Function

    Sub Main()
        Dim source As Integer() = {1, 2, 3}

        ' Create a delegate from an existing method.
        Dim resultEnumerable1 = source.Select(AddressOf OneMoreThan)

        ' The above is just syntax sugar for this; extension methods can be called as if they were instance methods of the first parameter.
        resultEnumerable1 = Enumerable.Select(source, AddressOf OneMoreThan)

        ' Or use an anonymous delegate.
        Dim resultEnumerable2 = source.Select(Function(i) i + 1)

        ' The sequences are the same. 
        Console.WriteLine(Enumerable.SequenceEqual(resultEnumerable1, resultEnumerable2))

        Dim resultArr As Integer() = resultEnumerable1.ToArray()

        Array.ForEach(resultArr, AddressOf Console.WriteLine)
    End Sub
End Module
```


{{out}}

```txt
True
2
3
4
```



## Vorpal

Given and array, A, and a function, F, mapping F over the elements of A is simple:

```vorpal
A.map(F)
```

If F takes 2 arguments, x and , then simply pass them to map.  
They will be passed to F when as it is applied to each element of A.

```vorpal
A.map(F, x, y)
```



## Wart


```wart
map prn '(1 2 3 4 5)
```


{{Out}}

```txt
1
2
3
4
5
```



## WDTE


```WDTE>let a =
 import 'arrays';
let s => import 'stream';

let example => [3; 5; 2];

let double => a.stream example
-> s.map (* 2)
-> s.collect
;
```


In WDTE, mapping can be accomplished using the <code>stream</code> module. Streams are essentially lazy iterators. The <code>arrays</code> module provides a function for creating a stream from an array, and then the <code>stream</code> module's functions can be used to perform a map operation. <code>collect</code> runs the iteration, collecting the elements yielded in a new array.


## Wren


```wren
var arr = [1, 2, 3, 4, 5]
arr = arr.map { |x| x * 2 }.toList
arr = arr.map(Fn.new {|x| x / 2}).toList
arr.each {|x| System.print(x) }

```



## Yabasic


```Yabasic
sub map(f$, t())
    local i

    for i = 1 to arraysize(t(), 1)
        t(i) = execute(f$, t(i))
    next i
end sub

sub add1(x)
    return x + 1
end sub

sub square(x)
    return x * x
end sub

dim t(10)

for i = 1 to 10
    t(i) = i
    print t(i), "\t";
next i
print

//map("add1", t())
map("square", t())

for i = 1 to 10
    print t(i), "\t";
next i
print
```



## Yacas


```Yacas
Sin /@ {1, 2, 3, 4}

MapSingle(Sin, {1,2,3,4})

MapSingle({{x}, x^2}, {1,2,3,4})

```



## zkl


```zkl
L(1,2,3,4,5).apply('+(5))
```

{{Out}}

```txt

L(6,7,8,9,10)

```



## zonnon


```zonnon

module Main;
type
	Callback = procedure (integer): integer;
	Vector = array {math} * of integer;

procedure Power(i:integer):integer;
begin
	return i*i;
end Power;

procedure Map(x: Vector;p: Callback): Vector;
var
	i: integer;
	r: Vector;
begin
	r := new Vector(len(x));
	for i := 0 to len(x) - 1 do	
		r[i] := p(i);
	end;
	return r
end Map;

procedure Write(x: Vector);
var
	i: integer;
begin
	for i := 0 to len(x) - 1 do
		write(x[i]:4)
	end;
	writeln
end Write;

var
	x,y: Vector;

begin
	x := [1,2,3,4,5];
	Write(Map(x,Power))
end Main.

```

{{Out}}

```txt

   0   1   4   9  16

```



## ZX Spectrum Basic


```zxbasic
10 LET a$="x+x"
20 LET b$="x*x"
30 LET c$="x+x^2"
40 LET f$=c$: REM Assign a$, b$ or c$
150 FOR i=1 TO 5
160 READ x
170 PRINT x;" = ";VAL f$
180 NEXT i
190 STOP 
200 DATA 2,5,6,10,100

```


{{omit from|gnuplot}}
{{omit from|LaTeX}}
{{omit from|Make}}
{{omit from|NSIS}}
{{omit from|PlainTeX}}
