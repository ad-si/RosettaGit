+++
title = "Assigning Values to an Array"
description = ""
date = 2011-08-31T00:41:31Z
aliases = []
[extra]
id = 1598
[taxonomies]
categories = []
tags = []
+++

{{DeprecatedTask}}

'''Please do not add new code, and move existing code to the [[Arrays]] task.'''

In this task, the goal is to assign a value to an element of an [[array]]. The value should only replace an existing value, and not insert a new key should the key not exist. If the key does not exist, an error should be returned.

==[[ActionScript]]==
 arr[0] = 1;
 arr[1] = 'a';
 arr[2] = 5.678;

==[[Ada]]==
{{works with|GCC| 4.1.2}}

```ada
package Pkg is
      type Arr is array (Positive range <>) of Integer;
      procedure Assign (Value : Integer; To : in out Arr; Index : Positive);
      --  Raise exception Constraint_Error if Index > To'Last
   end Pkg;
   package body Pkg is
      procedure Assign (Value : Integer; To : in out Arr; Index : Positive) is
      begin
         To (Index) := Value;
      end Assign;
   end Pkg;
```


==[[ALGOL 68]]==

```algol68
# Declarations: #
   FLEX [1:8] INT array:=(1,2,3,4,5,6,7,8);
   INT index=1, from=3, to=5, value=333;
# Simple bound checking:#
   IF LWB array <= index AND UPB array >= index THEN
#For a single index/value assignment:#
     array[index] := value;
#To assign multiple values to multiple indices/slice:#
     array[from:to] := (33,44,55);
#Replaces the 4th, 5th and 6th elements with the 33, 44 and 55.

To append/grow the end of the array:#

     PROC append int = (REF FLEX [] INT a, INT v)VOID: (
       HEAP FLEX [LWB a:UPB a + 1] INT out;
       out[:UPB a]:= a;
       out[UPB out] := v;
       a := out
     );
     append int(array,value);
     print((array,new line))
   FI
```

Result:

```txt

       +333         +2        +33        +44        +55         +6         +7         +8       +333

```


==[[APL]]==

       array[index]←value

APL is array-oriented, and so there are many different and powerful ways to index and assign arrays.  Asking an APLer how one assigns a value to an array index is like asking an Eskimo how one says "snow".  The above is just tip of the iceberg (i.e. a direct translation of the non-array-oriented code examples on this page).

==[[AppleScript]]==
 set item location of array to value

To put an item on the end of a list:
 set end of array to item

==[[AWK]]==
In the first example, a["hello"] is overwritten; in the second, a["hellx"] is not added.

```awk

$ awk 'BEGIN{a["hello"]="salut";if("hello" in a)a["hello"]="bonjour";for(i in a)print i,a[i]}'
hello bonjour

$ awk 'BEGIN{a["hello"]="salut";if("hellx" in a)a["hellx"]="bonjour";for(i in a)print i,a[i]}'
hello salut

```

The above code does not raise an error if the array element does not exist.
I am not sure how to raise a deliberate error in awk, but 1/0 comes in handy, if really required:

```AWK
$ awk 'BEGIN{a["hello"]="salut";if("hellx" in a)a["hellx"]="bonjour";else 1/0;for(i in a)print i,a[i]}'
awk: cmd. line:1: fatal: division by zero attempted
```


==[[Brainfuck]]==

To assign values 5, 6, and 7 to array elements 1,2,3:

 [-]>[-]>[-] zero array elements
 << go back to index 1
 +++++    move value 5 to element 1
 >++++++  move value 6 to element 2
 >+++++++ move value 7 to element 3

==[[C]]==

```c
#include <sys/types.h>

int writeToIntArray(int *array, size_t array_sz, int loc, int val)
{
  /* Check bounds on array */
  if (loc > array_sz || loc < 0)
    return -1;
  array[loc] = val;
  return 0;
}
```


==[[C sharp|C #]]==
'''Language Version:''' 1.0+

```csharp

        public void WriteToIntArray(int[] array, int loc, int val)
        {
            //if the index (zero based) we want to use is greater/equal to our length (one based) throw an exception
            if (loc >= array.Length) throw new Exception("Location is out of range.");
            //write value to array
            array[loc] = val;
        }

```


==[[C++]]==
{{works with|g++| 4.1.2}}
   template<class T, std::size:t size>
   inline int writeToArray(T (&array)[size], size_t loc, const T& val)
   {
     if (loc >= size)
       return -1; // Could throw an exception if so desired
     array[loc] = val;
     return 0;
   }

{{works with|Visual C++| 2005}}
   template<class C>
   inline void writeToArray(C& container, typename C::size_type loc, const typename C::value_type& val)
   {
     std::fill_n(container.begin() + loc, 1, val);
   }

==[[ColdFusion]]==
  <cffunction name="writeToArray">
    <cfargument name="array">
    <cfargument name="location">
    <cfargument name="value">
    <cfif arrayLen(arguments.array) GTE arguments.location>
      <cfset arguments.array[arguments.location] = arguments.value>
    <cfelse>
      <cfthrow message="Location does not exist">
    </cfif>
    <cfreturn arguments.array>
  </cffunction>

  <cfset myArray = arrayNew(1)>
  <cfset myArray[1] = 1>
  <cfset myArray = writeToArray(myArray, 1, 123456)>

Note that throwing errors in ColdFusion doesn't give that "friendly" appearance. The standard way to change/add a value in an array would be simply:

  <cfset myArray[3] = 987654>

==[[Common Lisp]]==

 (setf (aref array index) value)

==[[D]]==

 void setValue(T) (T[] array, size_t index, T newValue) { array[index] = newValue; }

==[[E]]==

In E, you may expect that the FlexList data type is an array or something similarly efficient. However, a FlexList will implicitly extend itself if you give an index equal to the current size. Therefore, to implement the specified task:


```e

if (index < array.size()) {
    array[index] := newValue
} else {
    throw(`$index is out of bounds 0..!${array.size()}`)
}

```


The basic assignment operation, <code>a[b] := c</code>, is syntactic sugar; it is equivalent to <code>a.put(b, c)</code>, except that it returns <var>c</var> rather than the result of the call.

==[[Delphi]]==
   procedure WriteToIntArray(var Arr: array of Integer; Loc: Integer; Val: Integer);
   begin
     Arr[Loc] := Val;
   end;

==[[Fortran]]==
In ISO Fortran 90 or later, use an array initializer (with RESHAPE intrinsic for multidimensional arrays):
  real, dimension(50)  :: a = (/ (1.0/(i*i),i=1,50) /)
  real, dimension(6)   :: b = (/ 0, 60, 120, 180, 240, 300 /)
  real, dimension(4,4) :: i4 = reshape( (/ 1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1 /), (/ 4, 4 /) )

In ISO Fortran 90 or later, use array section syntax to assign sections of an array:
  b(1:4) = i4(1,:)          ! gets row 1 of i4 into first 4 elements of b
  a(1:7:2) = i4(:,3)        ! gets column 3 of i4 into first 4 odd-numbered elements of a (stride of 2)
  i4(2:4,2:4) = i4(1:3,1:3) ! gets 3x3 subarray of i4 starting at row 1, column 1
                            ! into 3x3 subarray of i4 starting at row 2, column 2

'''Compiler:''' Any ANSI FORTRAN 77 or later (e.g. [[g77]])

  a(55) = 12

==[[F_Sharp|F#]]==
 let arr = [| 0; 1; 2; 3 |]
 arr.[0] <- 100
 Array.set arr 1 101 // alternative method

==[[Groovy]]==
Groovy, like every other C-derived language in the known universe, uses ZERO-based array/list indexing.


```groovy
def array = [0]*3 // three elements all initialized to zero

println array

array[1] = 1

println array

def strings = ['Mary', 'had', 'a', 'little', 'lamb', ". It's", 'fleece', 'was', 'white', 'as', 'snow']

println strings

strings[0] = 'Arthur'
strings[4] = 'towel'
strings[6] = 'stain'
strings[8] = 'ripe'
strings[10] = 'strawberries'

println strings
```


Output:

```txt
[0, 0, 0]
[0, 1, 0]
["Mary", "had", "a", "little", "lamb", ". It's", "fleece", "was", "white", "as", "snow"]
["Arthur", "had", "a", "little", "towel", ". It's", "stain", "was", "ripe", "as", "strawberries"]

```


==[[Haskell]]==
{{works with|GHC| 6.6}}

### List

Most Haskell programs use lists instead of arrays. This is suitable for the general case.


### =Simple Version=


```txt

setIndex
    :: [a] -- Original list
    -> Int -- Index to insert at
    -> a -- Value to insert
    -> [a] -- Resulting list
setIndex xs ii v =
    let
        (h, (_ : ts)) = splitAt ii xs
    in
        h ++ (v : ts)

```



### =Efficient Version=


```txt

setIndex xs ii v
    | ii < 0 = error "Bad index"
    | otherwise = _setIndex xs ii v
    where
        _setIndex [] _ _ = error "Bad index"
        _setIndex (_ : xs) 0 v = v : xs
        _setIndex (x : xs) ii v = x : (setIndex xs (ii - 1) v)

```



### Array

Technically, this creates clones the original array, then updates the new array; the original array still exists. This applies a list of changes to the array.

```txt

import Data.Array

-- Create the array of data
a1 = array (0, 4) [(ii, ii * 2) | ii <- [0 .. 4]]

-- Update 1 entry
a2 = a1 // [(2, 12)]
-- Update several entries
a3 = a1 // [(ii, ii + 10) | ii <- [1 .. 3]]

```



### Mutable Array



```txt

import Data.Array.MArray
import Data.Array.IO

main = do -- Create the array of data with all elements intialized to 3
          a <- newArray (0, 4) 3 :: IO (IOArray Int Int)

          -- Print entry
          readArray a 2 >>= print  -- prints "3"

          -- Update one entry
          writeArray a 2 5

          -- Print entry again
          readArray a 2 >>= print  -- prints "5"

```


==[[J]]==

    array =: 5 5 5 5 5 5 5

    99 (3}) array             NB.  Simple update
 5 5 5 99 5 5 5

    array =:  99 (3}) array   NB.  In place

    88 99 88 (2 3 4}) array   NB.  Multiple update
 5 5 88 99 88 5 5

==[[mIRC Scripting Language]]==
{{works with|mIRC|}}
{{works with|mArray Snippet|}}
[[Category:mArray Snippet]]
  alias write2array { echo -a $array_write(MyArray, 2, 3, Rosetta) }

==[[Mathematica]]==
Just like Part can extract parts of expressions, it can also be used to assign an element to a value:

```Mathematica

 a={x,y,z};
 a[[2]]=7;
 a

```

gives back:

```Mathematica

 {x, 7, z}

```

Mathematica will give an error if the index is not correct. It doesn't crash the software, however it does print a warning indicating that the element does not exist and thus can't be changed. Note that index 0 is the Head of the expression and 1 is the first element. Negative indices indicate elements counted from the back, -1 being the last element. A safe alternative could be (it will print a message and return the original array if index out of range):

```Mathematica

SaveAssign[list_, index_Integer, value_] := Module[{x = list},
  If[index > Length[list] || index < 0,
   Print["index out of range"];
   ,
   x[[index]] = value;
   ];
  x
  ]

```

Example:

```Mathematica

 a={x,y,z};
 SaveAssign[a,2,7]
 SaveAssign[a,4,7]

```

gives back:

```Mathematica

{x, 7, z}
index out of range
{x,y,z}

```


==[[MAXScript]]==

```txt
arr[index] = value
```

MAXScript arrays dynamically resize when assigning to an index that previously didn't exist.

==[[Nial]]==
 a := 1 2 3 4
 a@1 := 100
 a
 =1 100 3 4

==[[Objective-C]]==
{{works with|GCC| 3.3+}}

  - (void)addValue:(id)value toArray:(NSMutableArray *)array position:(unsigned)pos
  {
    [array insertObject:value atIndex:pos];
  }

==[[OCaml]]==
   arr.(loc) <- val;
   (* equivalent to: *)
   Array.set arr loc val;

==[[Perl]]==
{{works with|Perl|5.8.8}}

For a single index/value assignment:

 $array[$index] = $value;

To assign multiple values to multiple indices:

 @array[@indexes] = @values;

To assign an array slice:

 @array[3..5] = @values;
 # will replace the 4th, 5th and 6th elements with the first 3 values in @values

==[[PHP]]==

 <?php
 function writeToArray(&$array, $index, $value)
 {
     $array[$index] = $value;
 }
 // Usage example
 writeToArray($array, 1, 6 );
 ?>

Note that this is a "function" based example, and the relevant acting code is the following

 $array[$index] = $value;

This does not conform to the (somewhat arbitrary) specific requirements of the task, in that it does not return an error if the key index) does not exist. To satisfy the task requirements:

 <?php
 function writeToArray(&$array, $index, $value)
 {
     if(array_key_exists($index, $array)) $array[$index] = $value;
     else return false;
 }
 // Usage example
 writeToArray($array, 1, 6 );
 ?>

==[[PL/SQL]]==
'''Interpreter:''' Oracle compiler
  set serveroutput on
  declare
        type myarray is table of number index by binary_integer;
        x myarray;
        i pls_integer;
  begin
        -- populate array
        for i in 1..5 loop
                x(i) := i;
        end loop;
        i :=0;

        -- print array
        loop
                i := i + 1;
                begin
                        dbms_output.put_line(x(i));
                exception
                        when no_data_found then exit;
                end;
        end loop;

  end;
  /

==[[Pop11]]==

  value -> array(index);

Note that normal Pop11 array signal error when accessing non existing values (all values within index bounds exist).


==[[Python]]==

To change existing item, (raise IndexError if the index does not exists):

```python

 array[index] = value

```


To append to the end of the array:

```python

 array.append(value)

```


It's also possible modify Python lists using "slices" which can replace, remove or insert elements into the array.  For example:


```python

 mylist = [0,1,2,3]
 mylist[1:3] = [1, 1.2, 1.3]
 print mylist
 ## >>> [0, 1, 1.2, 1.3, 3]
 ## We've replaced 1 and 2 with 1, 1.2 and 1.3, effectively inserting 1.2 and 1.3 while removing the original third element (2)

```


Hint: slice notation should be read as: "from starting index '''up to''' (but '''not including''') ending index" -- a slice of [1:2] only references the single element sub-list containing the second item.  To remember that they are zero based one might even read the slice more verbosely as: "from the n items past the beginning of the list, up to (but not including) ..."

It's even possible (though obscure) to use extended slices with a "stride" to replace every ''n''th element of a list using something like:


```python

mylist = [0,1,2,3]
mylist[0:4:2] = ['x', 'y']   # can also be written as mylist[::2] in this case, to cover the whole list
print mylist
## >>> ['x', 1, 'y', 3]

```


Python lists also support ''.insert(),'' and ''.remove()'' methods, for cases where the slice syntax might be awkward, and a Python list can be treated like a stack by using the ''.pop()'' and ''.append()'' methods.  Finally a whole list or other sequence can be appended to a list using the ''.extend()'' method.


```python

mylist = [0,1]
mylist.extend([2,3])
print mylist
## >>> [0, 1, 2, 3]
## mylist.append([2,3]) would have appended one item to the list
## ... and that item would have been list containing two nested items

```



==[[R]]==

```R

 #Create array
 x <- runif(10)
 #Replace the value at the 7th position
 x[7] <- 99

```


==[[Scala]]==
  val array = new Array[Int](10) // create a 10 element array of integers
  array(3) = 44
  array(22) = 11 // java.lang.ArrayIndexOutOfBoundsException: 22

  import scala.collection.mutable.ArrayBuffer
  val array2 = new ArrayBuffer[Int]
  array2 += 1
  array2 += 2
  array2 += 3
  array2(1) // 2 (zero based indexing)
  array2(1) = 33
  array2.toString // ArrayBuffer(1,33,3)

  var l = List(1,2,3)
  l = 44 :: l //  List(44,1,2,3)
  l(2) // 2

==[[Slate]]==


```slate

{1. 2. 3} `>> [at: 1 put: 500.]. "==> {1. 500. 3}"
[{1. 2. 3} at: 100] on: Error do: [|:err| err return: Nil].

```


==[[Smalltalk]]==


```smalltalk
"suppose array is bound to an array of 20 values"
array at: 5 put: 'substitute fifth element'.

[ array at: 21 put: 'error' ]
   on: SystemExceptions.IndexOutOfRange
   do: [ :sig | 'Out of range!' displayNl ].
```


==[[Standard ML]]==
   Array.update (arr, loc, val);

==[[Tcl]]==

```tcl
proc setIfExist {theVariable value} {
    upvar 1 $theVariable variable
    if {[info exists theVariable]} {
        set theVariable $value
    } else {
        error "$theVariable doesn't exist"
    }
}
```

Note that <tt>setIfExist</tt> is general purpose and works on regular variables as well as arrays:

```tcl
setIfExist foo(bar) 10 ;# error if foo(bar) doesn't exist
setIfExist x 10        ;# error if x doesn't exist
```


==[[Toka]]==
  100 cells is-array foo
  1000 10 foo array.put   ( Put value '1000' into array 'foo' at slot '10' )
  10 chars is-array bar
  char: A 1 foo array.putChar  ( Put value 'A' (ASCII code) into character array 'bar' at slot '1' )

==[[Visual Basic]]==
'''Language Version:''' 5.0+
 Private Function writeToArray(intArray() As Integer, arraySize As Integer, loc As Integer, value As Integer) As Integer
    If loc > arraySize Then
      writeToArray = -1
    Else
      intArray(loc) = value
      writeToArray = 0
    End If
 End Function

==[[VBScript]]==


### Simple Example


Define our Array
  Dim myArray (5)

Use a For Next loop to set the array data.
  For i = 0 To 4
    myArray(i) = i
  Next

Use a For Next loop and MsgBox to display the array data.
  MsgBox("Print array values")
  For i = 0 To 4
    msgbox("myArray element " & i & " = " & myArray(i))
  Next


### Variable Array Length

'''Example where we don't know the required array size at the start and where we need to increase the array size as we go'''

Define an array - but we don't know how big yet.
  Dim myArray2 ()

OK, now we know how big an array we need.
  ReDim myArray2(3)

Load the array
  For i = 0 To 2
    myArray2(i) = i
  Next

Print the array
  MsgBox("We've set the new array size and set the data")
  For i = 0 To 2
    MsgBox "myArray2 element " & i & " = " & myArray2(i)
  Next

Now we need to make the array bigger. Note the Preserve keyword so that the existing data in the array is not lost when we resize it.
  ReDim Preserve myArray2(5)

Load the array
  For i = 3 To 4
    myArray2(i) = i
  Next

Print the array
  MsgBox ("We've increased the array size and loaded more data")
  For i = 0 To 4
    MsgBox "myArray2 element " & i & " = " & myArray2(i)
  Next
