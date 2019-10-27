+++
title = "Sort using a custom comparator"
description = ""
date = 2019-10-18T19:46:41Z
aliases = []
[extra]
id = 1870
[taxonomies]
categories = []
tags = []
+++

{{task|Sorting}}
{{omit from|BBC BASIC}}

;Task:
Sort an array (or list) of strings in order of descending length, and in ascending lexicographic order for strings of equal length. 

Use a sorting facility provided by the language/library, combined with your own callback comparison function.


'''Note:'''   Lexicographic order is case-insensitive.





## Ada

 {{incorrect}} 
{{works with|GNAT|}}

```ada

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gnat.Heap_Sort_G;

procedure Custom_Compare is
   
   type StringArrayType is array (Natural range <>) of Unbounded_String;
   
   Strings : StringArrayType := (Null_Unbounded_String,
                                 To_Unbounded_String("this"),
                                 To_Unbounded_String("is"),
                                 To_Unbounded_String("a"),
                                 To_Unbounded_String("set"),
                                 To_Unbounded_String("of"),
                                 To_Unbounded_String("strings"),
                                 To_Unbounded_String("to"),
                                 To_Unbounded_String("sort"),
                                 To_Unbounded_String("This"),
                                 To_Unbounded_String("Is"),
                                 To_Unbounded_String("A"),
                                 To_Unbounded_String("Set"),
                                 To_Unbounded_String("Of"),
                                 To_Unbounded_String("Strings"),
                                 To_Unbounded_String("To"),
                                 To_Unbounded_String("Sort"));
   
   procedure Move (From, To : in Natural) is
      
   begin
      Strings(To) := Strings(From);
   end Move;
   
   function UpCase (Char : in Character) return Character is
      Temp : Character;
   begin
      if Char >= 'a' and Char <= 'z' then
         Temp := Character'Val(Character'Pos(Char)
                                 - Character'Pos('a') 
                                 + Character'Pos('A'));
      else
         Temp := Char;
      end if;
      return Temp;
   end UpCase;
   
   function Lt (Op1, Op2 : Natural) 
               return Boolean is
      Temp, Len : Natural;
   begin
      Len := Length(Strings(Op1));
      Temp := Length(Strings(Op2));
      if Len < Temp then
         return False;
      elsif Len > Temp then
         return True;
      end if;

      declare
         S1, S2 : String(1..Len);
      begin
         S1 := To_String(Strings(Op1));
         S2 := To_String(Strings(Op2));
      Put("Same size:  ");
      Put(S1);
      Put(" ");
      Put(S2);
      Put(" ");
      for I in S1'Range loop
         Put(UpCase(S1(I)));
         Put(UpCase(S2(I)));
         if UpCase(S1(I)) = UpCase(S2(I)) then
           null;
         elsif UpCase(S1(I)) < UpCase(S2(I)) then
            Put(" LT");
            New_Line;
            return True;
         else
            return False;
         end if;
      end loop;
      Put(" GTE");
      New_Line;
      return False;
      end;
   end Lt;
   
   procedure Put (Arr : in StringArrayType) is
   begin
      for I in 1..Arr'Length-1 loop
         Put(To_String(Arr(I)));
         New_Line;
      end loop;
   end Put;
   
   package Heap is new Gnat.Heap_Sort_G(Move,
                                        Lt);
   use Heap;
   
   
begin
   Put_Line("Unsorted list:");
   Put(Strings);
   New_Line;
   Sort(16);
   New_Line;
   Put_Line("Sorted list:");
   Put(Strings);
end Custom_Compare;
```

{{out}}

```txt
Unsorted list:
this
is
a
set
of
strings
to
sort
This
Is
A
Set
Of
Strings
To
Sort

Sorted list:
strings
Strings
sort
Sort
this
This
Set
set
is
Is
Of
of
to
To
a
A
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}
The Algol 68 version of the Quicksort algorithm, modified to use a custom sort routine, as per this task.

```algol68
# define the MODE that will be sorted #
MODE SITEM = STRING;
#--- Swap function ---#
PROC swap = (REF[]SITEM array, INT first, INT second) VOID:
(
    SITEM temp := array[first];
    array[first] := array[second];
    array[second]:= temp
);
#--- Quick sort partition arg function with custom comparision function ---#
PROC quick = (REF[]SITEM array, INT first, INT last, PROC(SITEM,SITEM)INT compare) VOID:
(
    INT   smaller := first + 1,  
          larger  := last;
    SITEM pivot   := array[first];
    WHILE smaller <= larger DO
        WHILE compare(array[smaller], pivot) < 0 AND smaller < last DO   
            smaller +:= 1        
        OD;
        WHILE compare( array[larger], pivot) > 0 AND larger > first DO   
            larger  -:= 1       
        OD; 
        IF smaller < larger THEN 
            swap(array, smaller, larger); 
            smaller +:= 1;
            larger  -:= 1
        ELSE
            smaller +:= 1
        FI
    OD;
    swap(array, first, larger);    
    IF first < larger-1 THEN
        quick(array, first, larger-1, compare)
    FI;
    IF last > larger +1 THEN
        quick(array, larger+1, last, compare)
    FI
);
#--- Quick sort array function with custom comparison function ---#
PROC quicksort = (REF[]SITEM array, PROC(SITEM,SITEM)INT compare) VOID:
(
  IF UPB array > LWB array THEN
    quick(array, LWB array, UPB array, compare)
  FI
);
#***************************************************************#
main:
(
    OP LENGTH  = (STRING a)INT: ( UPB a + 1 ) - LWB a;
    OP TOLOWER = (STRING a)STRING:
       BEGIN
           STRING result := a;
           FOR i FROM LWB result TO UPB result DO
               CHAR c = a[i];
               IF c >= "A" AND c <= "Z" THEN result[i] := REPR ( ( ABS c - ABS "A" ) + ABS "a" ) FI
           OD;
           result
       END # TOLOWER # ;
    # custom comparison, descending sort on length #
    # if lengths are equal, sort lexicographically #
    PROC compare = (SITEM a, b)INT:
         IF   INT a length = LENGTH a;
              INT b length = LENGTH b;
              a length < b length 
         THEN
             # a is shorter than b #  1
         ELIF a length > b length
         THEN
             # a is longer than b  # -1
         ELIF STRING lower a = TOLOWER a;
              STRING lower b = TOLOWER b;
              lower a < lower b
         THEN
              # lowercase a is before lowercase b # -1
         ELIF lower a > lower b
         THEN
              # lowercase a is after lowercase b  #  1
         ELIF a > b
         THEN
             # a and b are equal ignoring case,        #
             # but a is after b considering case       #  1
         ELIF a < b
         THEN
             # a and b are equal ignoring case,        #
             # but a is before b considering case      # -1
         ELSE
             # the strings are equal                   #  0
         FI # compare # ;
    []SITEM orig = ("Here", "are", "some", "sample", "strings", "to", "be", "sorted"); 
    [LWB orig : UPB orig]SITEM a := orig;
    print(("Before:"));FOR i FROM LWB a TO UPB a DO print((" ",a[i])) OD; print((newline));
    quicksort(a, compare);
    print(("After :"));FOR i FROM LWB a TO UPB a DO print((" ",a[i])) OD; print((newline))
)
```

{{out}}

```txt

Before: Here are some sample strings to be sorted
After : strings sample sorted Here some are be to

```



## AppleScript


AppleScript is not itself well equipped with sorting functions, but from Yosemite onwards we can make some use of ObjC classes. While a classic comparator function can not readily be passed from AppleScript to ObjC, we can at least write a custom function which lifts atomic values into records (with keys to base and derivative values), and also passes a sequence of (key, bool) pairs, where the bool expresses the choice between ascending and descending order for the paired key:


```AppleScript
use framework "Foundation"

-- SORTING LISTS OF ATOMIC (NON-RECORD) DATA WITH A CUSTOM SORT FUNCTION

-- In sortBy, f is a function from () to a tuple of two parts:
-- 1. a function from any value to a record derived from (and containing) that value
--  The base value should be present in the record under the key 'value'
--  additional derivative keys (and optionally the 'value' key) should be included in 2:
-- 2. a list of (record key, boolean) tuples, in the order of sort comparison,
--    where the value *true* selects ascending order for the paired key
--    and the value *false* selects descending order for that key

-- sortBy :: (() -> ((a -> Record), [(String, Bool)])) -> [a] -> [a]
on sortBy(f, xs)
    set {fn, keyBools} to mReturn(f)'s |λ|()
    script unWrap
        on |λ|(x)
            value of x
        end |λ|
    end script
    map(unWrap, sortByComparing(keyBools, map(fn, xs)))
end sortBy

-- SORTING APPLESCRIPT RECORDS BY PRIMARY AND N-ARY SORT KEYS

-- sortByComparing :: [(String, Bool)] -> [Records] -> [Records]
on sortByComparing(keyDirections, xs)
    set ca to current application
    
    script recDict
        on |λ|(x)
            ca's NSDictionary's dictionaryWithDictionary:x
        end |λ|
    end script
    set dcts to map(recDict, xs)
    
    script asDescriptor
        on |λ|(kd)
            set {k, d} to kd
            ca's NSSortDescriptor's sortDescriptorWithKey:k ascending:d selector:dcts
        end |λ|
    end script
    
    ((ca's NSArray's arrayWithArray:dcts)'s ¬
        sortedArrayUsingDescriptors:map(asDescriptor, keyDirections)) as list
end sortByComparing


-- GENERIC FUNCTIONS ---------------------------------------------------------
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


-- TEST ----------------------------------------------------------------------
on run
    set xs to ["Shanghai", "Karachi", "Beijing", "Sao Paulo", "Dhaka", "Delhi", "Lagos"]
    
    -- Custom comparator:
    
    -- Returns a lifting function and a sequence of {key, bool} pairs
    
    -- Strings in order of descending length, 
    -- and ascending lexicographic order
    script lengthDownAZup
        on |λ|()
            script
                on |λ|(x)
                    {value:x, n:length of x}
                end |λ|
            end script
            {result, {{"n", false}, {"value", true}}}
        end |λ|
    end script
    
    sortBy(lengthDownAZup, xs)
end run
```

{{Out}}

```txt
{"Sao Paulo", "Shanghai", "Beijing", "Karachi", "Delhi", "Dhaka", "Lagos"}
```



## AutoHotkey


```AutoHotkey
numbers = 5,3,7,9,1,13,999,-4 
strings = Here,are,some,sample,strings,to,be,sorted
Sort, numbers, F IntegerSort D,
Sort, strings, F StringLengthSort D,
msgbox % numbers 
msgbox % strings

IntegerSort(a1, a2) { 
return a2 - a1
}

StringLengthSort(a1, a2){
return strlen(a1) - strlen(a2)
}
```



## AWK

For GAWK, this uses the inbuilt descending numeric ordering and a custom comparison routine for caseless string comparison.
May need modification for TAWK.

```AWK
# syntax: GAWK -f SORT_USING_A_CUSTOM_COMPARATOR.AWK
#
# sorting:
#   PROCINFO["sorted_in"] is used by GAWK
#   SORTTYPE is used by Thompson Automation's TAWK
#
BEGIN {
    words = "This Is A Set Of Strings To Sort duplicated"
    n = split(words " " tolower(words),tmp_arr," ")
    print("unsorted:")
    for (i=1; i<=n; i++) {
      word = tmp_arr[i]
      arr[length(word)][word]++
      print(word)
    }
    print("\nsorted:")
    PROCINFO["sorted_in"] = "@ind_num_desc" ; SORTTYPE = 9
    for (i in arr) {
      PROCINFO["sorted_in"] = "caselessCompare" ; SORTTYPE = 2 # possibly 14?
      for (j in arr[i]) {
        for (k=1; k<=arr[i][j]; k++) {
          print(j)
        }
      }
    }
    exit(0)
}
function caselessCompare( i1, v1, i2, v2,        l1, l2, result )
{
    l1 = tolower( i1 );
    l2 = tolower( i2 );
return ( ( l1 < l2 ) ? -1 : ( ( l1 == l2 ) ? 0 : 1 ) );
} # caselessCompare
```

{{out}}

```txt

unsorted:
This
Is
A
Set
Of
Strings
To
Sort
duplicated
this
is
a
set
of
strings
to
sort
duplicated

sorted:
duplicated
duplicated
Strings
strings
sort
Sort
This
this
set
Set
is
Is
of
Of
to
To
a
A

```



## Babel


To sort ASCII strings, use the strsort or lexsort utilities to sort alphabetically and lexicographically, respectively.


```babel>babel
 ("Here" "are" "some" "sample" "strings" "to" "be" "sorted") strsort ! lsstr !
( "Here" "are" "be" "sample" "some" "sorted" "strings" "to" )
babel> ("Here" "are" "some" "sample" "strings" "to" "be" "sorted") lexsort ! lsstr !
( "be" "to" "are" "Here" "some" "sample" "sorted" "strings" )
```


If you want to sort UTF-8 encoded Unicode strings, first convert to array-string form using the str2ar operator, then sort using the strcmp operator. To sort lexicographically, use the arcmp operator. The following examples illustrate each case:


```babel>babel
 ("Here" "are" "some" "sample" "strings" "to" "be" "sorted") {str2ar} over ! {strcmp 0 lt?} lssort ! {ar2str} over ! lsstr !
( "Here" "are" "be" "some" "sample" "sorted" "strings" "to" )
babel>  ("Here" "are" "some" "sample" "strings" "to" "be" "sorted") {str2ar} over ! {arcmp 0 lt?} lssort ! {ar2str} over ! lsstr !
( "be" "to" "are" "Here" "some" "sample" "sorted" "strings" )
```


You can sort a list of any kind of structure you like using the lssort utility. Use the lt? numerical comparison operator for sorting numerical lists:


```babel>babel
 ( 5 6 8 4 5 3 9 9 4 9 ) {lt?} lssort ! lsnum !
( 3 4 4 5 5 6 8 9 9 9 )
```


You can even shuffle a list with lssort using the randlf operator (your results will probably differ):


```babel>babel
 (1 2 3 4 5 6 7 8 9) {1 randlf 2 rem} lssort ! lsnum !
( 7 5 9 6 2 4 3 1 8 )
```


To sort complex objects, you need to access the relevant field in each object, and then provide the result of comparing them. For example, to sort a list of pairs by first number:


```babel

babel> 20 lsrange ! {1 randlf 2 rem} lssort ! 2 group ! --> this creates a shuffled list of pairs
babel> dup {lsnum !} ... --> display the shuffled list, pair-by-pair
( 11 10 )
( 15 13 )
( 12 16 )
( 17 3 )
( 14 5 )
( 4 19 )
( 18 9 )
( 1 7 )
( 8 6 )
( 0 2 )
babel> {<- car -> car lt? } lssort ! --> sort the list by first element of each pair
babel> dup {lsnum !} ... --> display the sorted list, pair-by-pair
( 0 2 )
( 1 7 )
( 4 19 )
( 8 6 )
( 11 10 )
( 12 16 )
( 14 5 )
( 15 13 )
( 17 3 )
( 18 9 )
```



## Burlesque



```burlesque

blsq ) {"acb"  "Abc" "Acb" "acc" "ADD"}><
{"ADD" "Abc" "Acb" "acb" "acc"}
blsq ) {"acb"  "Abc" "Acb" "acc" "ADD"}(zz)CMsb
{"Abc" "acb" "Acb" "acc" "ADD"}

```



## C

{{works with|POSIX|.1-2001}}


```c>#include <stdlib.h
  /* for qsort */
#include <string.h>  /* for strlen */
#include <strings.h> /* for strcasecmp */

int mycmp(const void *s1, const void *s2)
{
    const char *l = *(const char **)s1, *r = *(const char **)s2;
    size_t ll = strlen(l), lr = strlen(r);

    if (ll > lr) return -1;
    if (ll < lr) return 1;
    return strcasecmp(l, r);
}

int main()
{
    const char *strings[] = {
      "Here", "are", "some", "sample", "strings", "to", "be", "sorted" };

    qsort(strings, sizeof(strings)/sizeof(*strings), sizeof(*strings), mycmp);
    return 0;
}
```


=={{header|C sharp|C#}}==

{{incorrect}}
Wrong compare. Because can't find "a" < "A" 


C# allows you to specify a custom compare to the built in sort method on a list


```csharp
using System;
using System.Collections.Generic;

namespace RosettaCode {
    class SortCustomComparator {
        // Driver program
        public void CustomSort() {
            String[] items = { "Here", "are", "some", "sample", "strings", "to", "be", "sorted" };
            List<String> list = new List<string>(items);

            DisplayList("Unsorted", list);
            
            list.Sort(CustomCompare);
            DisplayList("Descending Length", list);

            list.Sort();
            DisplayList("Ascending order", list);
        }

        // Custom compare
        public int CustomCompare(String x, String y) {
            int result = -x.Length.CompareTo(y.Length);
            if (result == 0) {
                result = x.ToLower().CompareTo(y.ToLower());
            }

            return result;
        }

        // Output routine
        public void DisplayList(String header, List<String> theList) {
            Console.WriteLine(header);
            Console.WriteLine("".PadLeft(header.Length, '*'));
            foreach (String str in theList) {
                Console.WriteLine(str);
            }
            Console.WriteLine();
        }
    }
}
```


{{out}}

```txt
Unsorted
********
Here
are
some
sample
strings
to
be
sorted

Descending Length
*****************
strings
sample
sorted
Here
some
are
be
to

Ascending order
***************
are
be
Here
sample
some
sorted
strings
to

```


=== Alternative using Linq (.NET 3.5) ===
{{incorrect}}
Has not the case of equal in lower case and then make them in order using the exact character case, so "a" comes before "A"



```csharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace RosettaCode
{
	class SortCustomComparator
	{
		// Driver program
		public void CustomSort()
		{
			List<string> list = new List<string> { "Here", "are", "some", "sample", "strings", "to", "be", "sorted" };

			DisplayList("Unsorted", list);

			var descOrdered = from l in list
					  orderby l.Length descending 
					  select l;
			DisplayList("Descending Length", descOrdered);

			var ascOrdered = from l in list
					 orderby l
					 select l;
			DisplayList("Ascending order", ascOrdered);
		}

		// Output routine
		public void DisplayList(String header, IEnumerable<string> theList)
		{
			Console.WriteLine(header);
			Console.WriteLine("".PadLeft(header.Length, '*'));
			foreach (String str in theList)
			{
				Console.WriteLine(str);
			}
			Console.WriteLine();
		}
	}
}

```



## C++

{{works with|g++|4.1.2}}

```cpp>#include <algorithm

#include <string>
#include <cctype>

// compare character case-insensitive
struct icompare_char {
  bool operator()(char c1, char c2) {
    return std::toupper(c1) < std::toupper(c2);
  }
};

// return true if s1 comes before s2
struct compare {
  bool operator()(std::string const& s1, std::string const& s2) {
    if (s1.length() > s2.length())
      return true;
    if (s1.length() < s2.length())
      return false;
    return std::lexicographical_compare(s1.begin(), s1.end(),
                                        s2.begin(), s2.end(),
                                        icompare_char());
  }
};

int main() {
  std::string strings[8] = {"Here", "are", "some", "sample", "strings", "to", "be", "sorted"};
  std::sort(strings, strings+8, compare());
  return 0;
}
```



## Ceylon


```ceylon
shared void run() {

	value strings = [
		"Cat", "apple", "Adam", "zero", "Xmas", "quit", 
		"Level", "add", "Actor", "base", "butter"
	];
    
	value sorted = strings.sort((String x, String y) =>
			if(x.size == y.size)
			then increasing(x.lowercased, y.lowercased)
			else decreasing(x.size, y.size));
	
	sorted.each(print);
}
```



## Clean


```clean
import StdEnv

less s1 s2
    | size s1 > size s2 = True
    | size s1 < size s2 = False
    | otherwise = lower s1 < lower s2
where
    lower :: String -> String
    lower s = {toLower c \\ c <-: s}

Start = sortBy less ["This", "is", "a", "set", "of", "strings", "to", "sort"]
```



## Clojure

Clojure's ''sort'' function has a 2-argument version where the first argument is a ''java.util.Comparator'', and the second is the collection to be sorted. Thus the heart of this version is a comparator function that satisfies the problem spec. What makes this work is that all Clojure functions (thus ''rosetta-code'' defined here) implement the ''java.util.Comparator'' interface.

```clojure
(defn rosetta-compare [s1 s2]
  (let [len1 (count s1), len2 (count s2)]
    (if (= len1 len2)
      (compare (.toLowerCase s1) (.toLowerCase s2))
      (- len2 len1))))

(println
 (sort rosetta-compare
       ["Here" "are" "some" "sample" "strings" "to" "be" "sorted"]))
```


{{out}}

```txt

(strings sample sorted Here some are be to)

```


An alternative, using <tt>sort-by</tt>:

```clojure
(sort-by (juxt (comp - count) #(.toLowerCase %))
         ["Here" "are" "some" "sample" "strings" "to" "be" "sorted"])
```



## Common Lisp

In Common Lisp, the sort function takes a "less than" predicate that is used as the comparator.  This parameter can be any two-argument function. Note: Common Lisp's <tt>sort</tt> function is destructive; for lists you should not use the original list afterwards, you should only use the return value. This also means you don't call it directly on constant data.

For example, to sort strings case-insensitively in ascending order:


```lisp
CL-USER> (defvar *strings*
                 (list "Cat" "apple" "Adam" "zero" "Xmas" "quit" "Level" "add" "Actor" "base" "butter"))
*STRINGS*
CL-USER> (sort *strings* #'string-lessp)
("Actor" "Adam" "add" "apple" "base" "butter" "Cat" "Level" "quit" "Xmas"
"zero")
```


You can also provide an optional key function which maps each element to a key. The keys are then compared using the comparator. For example, to sort strings by length in descending order:


```lisp
CL-USER> (defvar *strings*
                 (list "Cat" "apple" "Adam" "zero" "Xmas" "quit" "Level" "add" "Actor" "base" "butter"))
*STRINGS*
CL-USER> (sort *strings* #'> :key #'length)
("butter" "apple" "Level" "Actor" "Adam" "zero" "Xmas" "quit" "base"
 "Cat" "add")
```



## D


```d
import std.stdio, std.string, std.algorithm, std.typecons;

void main() {
    "here are Some sample strings to be sorted"
    .split
    .schwartzSort!q{ tuple(-a.length, a.toUpper) }
    .writeln;
}
```

{{out}}

```txt
["strings", "sample", "sorted", "here", "Some", "are", "be", "to"]
```



### Alternative Version

The more natural and efficient way to solve this problem is to use <code>std.algorith.multiSort</code>. 
But currently it's less convenient because it can't be used with the UFCSyntax (same output):

```d
void main() {
    import std.stdio, std.string, std.algorithm;

    auto parts = "here are Some sample strings to be sorted".split;
    parts.multiSort!(q{a.length > b.length}, q{a.toUpper < b.toUpper});
    parts.writeln;
}
```



## Delphi


```Delphi
program SortWithCustomComparator;

{$APPTYPE CONSOLE}

uses SysUtils, Types, Generics.Collections, Generics.Defaults;

var
  lArray: TStringDynArray;
begin
  lArray := TStringDynArray.Create('Here', 'are', 'some', 'sample', 'strings', 'to', 'be', 'sorted');

  TArray.Sort<string>(lArray , TDelegatedComparer<string>.Construct(
  function(const Left, Right: string): Integer
  begin
    //Returns ('Here', 'are', 'be', 'sample', 'some', 'sorted', 'strings', 'to')
    //Result := CompareStr(Left, Right);

    //Returns ('are', 'be', 'Here', 'sample', 'some', 'sorted', 'strings', 'to')
    Result := CompareText(Left, Right);
  end));
end.
```



## E


```e
/** returns a if it is nonzero, otherwise b() */
def nonzeroOr(a, b) { return if (a.isZero()) { b() } else { a } }

["Here", "are", "some", "sample", "strings", "to", "be", "sorted"] \
    .sort(fn a, b { 
              nonzeroOr(b.size().op__cmp(a.size()),
                        fn { a.compareToIgnoreCase(b) }) 
          })
```



## EGL


{{works with|EDT|}}

```EGL
program SortExample
	
    function main()
       	test1 string[] = ["Here", "are", "some", "sample", "strings", "to", "be", "sorted"];
	test1.sort(sortFunction);

	SysLib.writeStdout("Test 1:");
	for(i int from 1 to test1.getSize())
      	    SysLib.writeStdout(test1[i]);
	end

	test2 string[] = ["Cat", "apple", "Adam", "zero", "Xmas", "quit", "Level", "add", "Actor", "base", "butter"];
	test2.sort(sortFunction);
		
	SysLib.writeStdout("Test 2:");
	for(i int from 1 to test2.getSize())
	    SysLib.writeStdout(test2[i]);
	end
    end
    
    function sortFunction(a any in, b any in) returns (int)
	result int = (b as string).length() - (a as string).length();
	if (result == 0)
  	    case
		when ((a as string).toLowerCase() > (b as string).toLowerCase())
	  	    result = 1;
		when ((a as string).toLowerCase() < (b as string).toLowerCase())
		    result = -1;
		otherwise
		    result = 0;
	    end
	end
    	
    	return result;
    end
	
end
```


{{out}}

```txt
Test 1:
strings
sample
sorted
Here
some
are
be
to

Test 2:
butter
Actor
apple
Level
Adam
base
quit
Xmas
zero
add
Cat
```


## Elena

ELENA 4.1 :

```elena
import extensions;
import system'routines;
import system'culture;
 
public program()
{
    var items := new string[]::( "Here", "are", "some", "sample", "strings", "to", "be", "sorted" );
 
    console.printLine("Unsorted:          ", items.asEnumerable());
 
    console.printLine("Descending length: ", items.clone() 
        .sort:(p,n => p.Length > n.Length).asEnumerable());
 
    console.printLine("Ascending order:   ", items.clone() 
        .sort:(p,n => p.toUpper(invariantLocale) < n.toUpper(invariantLocale)).asEnumerable())    
}
```

{{out}}

```txt

Unsorted:          Here,are,some,sample,strings,to,be,sorted
Descending length: strings,sorted,sample,some,Here,are,be,to
Ascending order:   are,be,Here,sample,some,sorted,strings,to

```



## Elixir


```elixir
strs = ~w[this is a set of strings to sort This Is A Set Of Strings To Sort]

comparator = fn s1,s2 -> if String.length(s1)==String.length(s2),
                           do:   String.downcase(s1) <= String.downcase(s2),
                           else: String.length(s1) >= String.length(s2) end
IO.inspect Enum.sort(strs, comparator)

# or
IO.inspect Enum.sort_by(strs, fn str -> {-String.length(str), String.downcase(str)} end)
```


{{out}}

```txt

["strings", "Strings", "sort", "Sort", "this", "This", "set", "Set", "is", "Is",
 "of", "Of", "to", "To", "a", "A"]

```



## Erlang


```Erlang

-module( sort_using_custom_comparator ).

-export( [task/0] ).

task() ->
	lists:sort( fun longest_first_case_insensitive/2, ["this", "is", "a", "set", "of", "strings", "to", "sort", "This", "Is", "A", "Set", "Of", "Strings", "To", "Sort"] ).



longest_first_case_insensitive( String1, String2 ) when erlang:length(String1) =:= erlang:length(String2) -> string:to_lower(String1) < string:to_lower(String2);
longest_first_case_insensitive( String1, String2 ) when erlang:length(String1) =< erlang:length(String2) -> false;
longest_first_case_insensitive( _String1, _String2 ) -> true.

```

{{out}}

```txt

9> sort_using_custom_comparator:task().
["Strings","strings","Sort","sort","This","this","Set",
 "set","Is","is","Of","of","To","to","A","a"]

```



## Euphoria


```euphoria
include sort.e
include wildcard.e
include misc.e

function my_compare(sequence a, sequence b)
    if length(a)!=length(b) then
        return -compare(length(a),length(b))
    else
        return compare(lower(a),lower(b))
    end if
end function

sequence strings
strings = reverse({ "Here", "are", "some", "sample", "strings", "to", "be", "sorted" })

puts(1,"Unsorted:\n")
pretty_print(1,strings,{2})

puts(1,"\n\nSorted:\n")
pretty_print(1,custom_sort(routine_id("my_compare"),strings),{2})
```


{{out}}

```txt
Unsorted:
{
  "sorted",
  "be",
  "to",
  "strings",
  "sample",
  "some",
  "are",
  "Here"
}

Sorted:
{
  "strings",
  "sample",
  "sorted",
  "Here",
  "some",
  "are",
  "be",
  "to"
}
```


=={{header|F_Sharp|F#}}==

```fsharp
let myCompare (s1:string) (s2:string) =
  match compare s2.Length s1.Length with
    | 0 -> compare (s1.ToLower()) (s2.ToLower())
    | X -> X

let strings = ["Here"; "are"; "some"; "sample"; "strings"; "to"; "be"; "sorted"]

let sortedStrings = List.sortWith myCompare strings

printfn "%A" sortedStrings
```


{{out}}

```txt
["strings"; "sample"; "sorted"; "Here"; "some"; "are"; "be"; "to"]
```



## Factor


```factor
: my-compare ( s1 s2 -- <=> )
    2dup [ length ] compare invert-comparison
    dup +eq+ = [ drop [ >lower ] compare ] [ 2nip ] if ;

{ "this" "is" "a" "set" "of" "strings" "to" "sort" } [ my-compare ] sort
```



## Fantom


The List's sort method can be customised using a custom comparator.  This is a method which returns an Int: -1 for less than, 0 for equal, +1 for greater than.


```fantom

class Main
{
  public static Void main ()
  {
    // sample strings from Lisp example
    strs := ["Cat", "apple", "Adam", "zero", "Xmas", "quit", 
             "Level", "add", "Actor", "base", "butter"]

    sorted := strs.dup // make a copy of original list 
    sorted.sort |Str a, Str b -> Int|  // sort using custom comparator
    {
      if (b.size == a.size)           // if size is same
        return a.compareIgnoreCase(b) // then sort in ascending lexicographic order, ignoring case
      else
        return b.size <=> a.size      // else sort in descending size order
    }
    echo ("Started with : " + strs.join(" "))
    echo ("Finished with: " + sorted.join(" "))
  }
}

```


{{out}}

```txt

$ fan comparator-sort.fan 
Started with : Cat apple Adam zero Xmas quit Level add Actor base butter
Finished with: butter Actor apple Level Adam base quit Xmas zero add Cat

```



## Fortran


Fortran does not have builtin to sort arrays (of numbers or strings), with or without custom comparator; so we need modifying e.g. [[Shell sort#Fortran|this code]] in order to handle strings and to accept a custom comparator.


```fortran
module sorts_with_custom_comparator
  implicit none
contains
  subroutine a_sort(a, cc)
    character(len=*), dimension(:), intent(inout) :: a
    interface
       integer function cc(a, b)
         character(len=*), intent(in) :: a, b
       end function cc
    end interface
    
    integer :: i, j, increment
    character(len=max(len(a), 10)) :: temp
    
    increment = size(a) / 2
    do while ( increment > 0 )
       do i = increment+1, size(a)
          j = i
          temp = a(i)
          do while ( j >= increment+1 .and. cc(a(j-increment), temp) > 0)
             a(j) = a(j-increment)
             j = j - increment
          end do
          a(j) = temp
       end do
       if ( increment == 2 ) then
          increment = 1
       else
          increment = increment * 5 / 11
       end if
    end do
  end subroutine a_sort
end module sorts_with_custom_comparator
```


Then we have to put our custom comparator in a module (<tt>to_lower</tt> is defined [[Change string case|here]]):


```fortran
module comparators
  implicit none
contains
  integer function my_compare(a, b)
    character(len=*), intent(in) :: a, b

    character(len=max(len(a),len(b))) :: a1, b1

    a1 = a
    b1 = b
    call to_lower(b1)
    call to_lower(a1)
    
    if ( len(trim(a)) > len(trim(b)) ) then
       my_compare = -1
    elseif ( len(trim(a)) == len(trim(b)) ) then
       if ( a1 > b1 ) then
          my_compare = 1
       else
          my_compare = -1
       end if
    else
       my_compare = 1
    end if
  end function my_compare
end module comparators
```


At the end, we can test these:


```fortran
program CustomComparator
  use comparators
  use sorts_with_custom_comparator
  implicit none

  character(len=100), dimension(8) :: str
  integer :: i

  str = (/ "this", "is", "an", "array", "of", "strings", "to", "sort" /)
  call a_sort(str, my_compare)

  do i = 1, size(str)
     print *, trim(str(i))
  end do
end program CustomComparator
```


## FreeBASIC


```freebasic
' version 23-10-2016
' compile with: fbc -s console

#Include Once "crt/stdlib.bi" ' for qsort

Function mycmp Cdecl (s1 As Any Pointer, s2 As Any Pointer) As Long

    ' -1 no swap first element before second element
    '  0 no swap needed, don't care
    '  1 swap first element after second element

    Dim As String str1 = *Cast(String Ptr, s1)
    Dim As String str2 = *Cast(String Ptr, s2)

    Dim As Long l1 = Len(str1), l2 = Len(str2)
    If (l1 > l2) Then Return -1 ' descending
    If (l1 < l2) Then Return  1 '

    ' there equal length, sort ascending
    If UCase(str1) = UCase(str2) Then
        If str1 > str2 Then Return 1
    Else
        If UCase(str1) > UCase(str2) Then Return 1
    End If
    
    Return 0

End Function

' ------=< MAIN >=------

Dim As String words(0 To ...) = {"Here", "are", "some", "sample", _
                                 "strings", "to", "be", "sorted" }

Dim As ULong array_size = UBound(words) - LBound(words) + 1

qsort(@words(0), array_size, SizeOf(String), @mycmp)

For i As Integer = 0 To UBound(words)
    Print words(i)
Next
Print

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
strings
sample
sorted
Here
some
are
be
to
```



## FunL


```funl
def preceeds( a, b ) = b.length() < a.length() or b.length() == a.length() and a.compareToIgnoreCase( b ) < 0

println( ["here", "are", "Some", "sample", "strings", "to", "be", "sorted"].sortWith(preceeds) )
```


{{out}}


```txt

["strings", "sample", "sorted", "here", "Some", "are", "be", "to"]

```



## Go


```go
package main

import (
    "fmt"
    "sort"
    "strings"
)

type sortable []string

func (s sortable) Len() int      { return len(s) }
func (s sortable) Swap(i, j int) { s[i], s[j] = s[j], s[i] }
func (s sortable) Less(i, j int) bool {
    a, b := s[i], s[j]
    if len(a) != len(b) {
        return len(a) > len(b)
    }
    return strings.ToLower(a) < strings.ToLower(b)
}

func main() {
    var s sortable = strings.Fields("To tell your name the livelong day To an admiring bog")
    fmt.Println(s, "(original)")

    sort.Sort(s)
    fmt.Println(s, "(sorted)")
}
```

{{out}}

```txt
[To tell your name the livelong day To an admiring bog] (original)
[admiring livelong name tell your bog day the an To To] (sorted)
```



## Groovy

The "custom comparator" is just a closure attached to the sort method invocation.

```groovy
def strings = "Here are some sample strings to be sorted".split()
strings.sort { x, y ->
    y.length() <=> x.length() ?: x.compareToIgnoreCase(y)
}
println strings
```


{{out}}

```txt
[strings, sample, sorted, Here, some, are, be, to]
```



## Haskell

{{works with|GHC}}

```haskell
import Data.Ord (comparing)
import Data.Char (toLower)
import Data.List (sortBy)

lengthThenAZ :: String -> String -> Ordering
lengthThenAZ = comparing length `mappend` comparing (fmap toLower)

descLengthThenAZ :: String -> String -> Ordering
descLengthThenAZ = flip (comparing length) `mappend` comparing (fmap toLower)

main :: IO ()
main =
  mapM_
    putStrLn
    (fmap
       unlines
       ([sortBy] <*> [lengthThenAZ, descLengthThenAZ] <*>
        [["Here", "are", "some", "sample", "strings", "to", "be", "sorted"]]))

```


{{Out}}

```txt
be
to
are
Here
some
sample
sorted
strings

strings
sample
sorted
Here
some
are
be
to
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()                     #: demonstrate various ways to sort a list and string
   write("Sorting Demo for custom comparator")
   L := ["Here", "are", "some", "sample", "strings", "to", "be", "sorted"]
   write("  Unsorted Input : ")
   every write("    ",image(!L)) 
   shellsort(L,cmptask)                # most of the RC sorts will work here
   write("  Sorted Output : ")
   every write("    ",image(!L)) 
end

procedure cmptask(a,b)   # sort by descending length and ascending lexicographic order for strings of equal length
if (*a > *b) | ((*a = *b) & (map(a) << map(b))) then return b
end
```


Note(1): This example relies on [[Sorting_algorithms/Bubble_sort#Icon| the supporting procedures 'sortop', and 'demosort' in Bubble Sort]]. 

Note(2): This example can utilize any of the sorting algorithms that share the same base code including: [[Sorting_algorithms/Bubble_sort#Icon_and_Unicon|Bubble]], [[Sorting_algorithms/Cocktail_sort#CocktailIcon_and_Unicon|Cocktail]], [[Sorting_algorithms/Comb_sort#Icon_and_Unicon|Comb]], [[Sorting_algorithms/Gnome_sort#Icon_and_Unicon|Gnome]], and [[Sorting_algorithms/Shell_sort#Icon_and_Unicon|Shell]].

Note(3): Using 'map' in the 'cmptask' procedure would not be efficient on large lists.

{{out}}

```txt
Sorting Demo for custom comparator
  Unsorted Input : 
    "Here"
    "are"
    "some"
    "sample"
    "strings"
    "to"
    "be"
    "sorted"
  Sorted Output : 
    "strings"
    "sample"
    "sorted"
    "Here"
    "some"
    "are"
    "be"
    "to"
```



## J

Case-insensitivity is obtained using <tt>lower</tt>, a verb taken from [[Change string case]]. 
Standard utilities <tt>tolower</tt> or <tt>toupper</tt> may be substituted.


```j
   mycmp=: 1 :'/:u'
   length_and_lex =: (-@:# ; lower)&>
   strings=: 'Here';'are';'some';'sample';'strings';'to';'be';'sorted'
   length_and_lex mycmp strings
+-------+------+------+----+----+---+--+--+
|strings|sample|sorted|Here|some|are|be|to|
+-------+------+------+----+----+---+--+--+
```



## Java

{{works with|Java|1.5+}}

```java5
import java.util.Comparator;
import java.util.Arrays;

public class Test {
  public static void main(String[] args) {
    String[] strings = {"Here", "are", "some", "sample", "strings", "to", "be", "sorted"};

    Arrays.sort(strings, new Comparator<String>() {
      public int compare(String s1, String s2) {
        int c = s2.length() - s1.length();
        if (c == 0)
          c = s1.compareToIgnoreCase(s2);
        return c;
      }
    });

    for (String s: strings)
      System.out.print(s + " ");
  }
}
```


Same thing as above
{{works with|Java|8+}}

```java5
import java.util.Comparator;
import java.util.Arrays;

public class ComparatorTest {
  public static void main(String[] args) {
    String[] strings = {"Here", "are", "some", "sample", "strings", "to", "be", "sorted"};

    Arrays.sort(strings, (s1, s2) -> {
      int c = s2.length() - s1.length();
      if (c == 0)
        c = s1.compareToIgnoreCase(s2);
      return c;
    });

    for (String s: strings)
      System.out.print(s + " ");
  }
}
```



## JavaScript


### ES5


```javascript
function lengthSorter(a, b) {
  var result = b.length - a.length;
  if (result == 0)
    result = a.localeCompare(b);
  return result;
}

var test = ["Here", "are", "some", "sample", "strings", "to", "be", "sorted"];
test.sort(lengthSorter);
alert( test.join(' ') );                      // strings sample sorted Here some are be to
```


Or, abstracting a little for simpler composition of compound and derived searches (ASC and DESC, secondary sorts):


```javascript
(function () {
    'use strict';

    // GENERIC FUNCTIONS FOR COMPARISONS

    // Ordering :: ( LT | EQ | GT ) | ( -1 | 0 | 1 )

    // compare :: a -> a -> Ordering
    var compare = function (a, b) {
        return a < b ? -1 : a > b ? 1 : 0;
    };

    // mappendOrdering :: Ordering -> Ordering -> Ordering
    var mappendOrdering = function (a, b) {
        return a !== 0 ? a : b;
    };

    // on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
    var on = function (f, g) {
        return function (a, b) {
            return f(g(a), g(b));
        };
    };

    // flip :: (a -> b -> c) -> b -> a -> c
    var flip = function (f) {
        return function (a, b) {
            return f.apply(null, [b, a]);
        };
    };

    // arrayCopy :: [a] -> [a]
    var arrayCopy = function (xs) {
        return xs.slice(0);
    };

    // show :: a -> String
    var show = function (x) {
        return JSON.stringify(x, null, 2);
    };

    // TEST
    var xs = ['Shanghai', 'Karachi', 'Beijing', 'Sao Paulo', 'Dhaka', 'Delhi', 'Lagos'];

    var rs = [{
        name: 'Shanghai',
        pop: 24.2
    }, {
        name: 'Karachi',
        pop: 23.5
    }, {
        name: 'Beijing',
        pop: 21.5
    }, {
        name: 'Sao Paulo',
        pop: 24.2
    }, {
        name: 'Dhaka',
        pop: 17.0
    }, {
        name: 'Delhi',
        pop: 16.8
    }, {
        name: 'Lagos',
        pop: 16.1
    }];

    // population :: Dictionary -> Num
    var population = function (x) {
        return x.pop;
    };

    // length :: [a] -> Int
    var length = function (xs) {
        return xs.length;
    };

    // toLower :: String -> String
    var toLower = function (s) {
        return s.toLowerCase();
    };

    // lengthThenAZ :: String -> String -> ( -1 | 0 | 1)
    var lengthThenAZ = function (a, b) {
        return mappendOrdering(
            on(compare, length)(a, b),
            on(compare, toLower)(a, b)
        );
    };

    // descLengthThenAZ :: String -> String -> ( -1 | 0 | 1)
    var descLengthThenAZ = function (a, b) {
        return mappendOrdering(
            on(flip(compare), length)(a, b),
            on(compare, toLower)(a, b)
        );
    };

    return show({
        default: arrayCopy(xs)
            .sort(compare),

        descendingDefault: arrayCopy(xs)
            .sort(flip(compare)),

        byLengthThenAZ: arrayCopy(xs)
            .sort(lengthThenAZ),

        byDescendingLengthThenZA: arrayCopy(xs)
            .sort(flip(lengthThenAZ)),

        byDescendingLengthThenAZ: arrayCopy(xs)
            .sort(descLengthThenAZ),

        byPopulation: arrayCopy(rs)
            .sort(on(compare, population)),

        byDescendingPopulation: arrayCopy(rs)
            .sort(on(flip(compare), population))
    });
})();
```



### ES6


```JavaScript
(() => {
    'use strict';

    // main :: IO ()
    const main = () => {
        const
            lengthThenAZ = mappendOrd(
                comparing(length),
                comparing(toLower)
            ),
            descLengthThenAZ = mappendOrd(
                flip(comparing(length)),
                comparing(toLower)
            );

        console.log(
            apList(apList([sortBy])([
                lengthThenAZ,
                descLengthThenAZ
            ]))([
                [
                    "Here", "are", "some", "sample",
                    "strings", "to", "be", "sorted"
                ]
            ]).map(unlines).join('\n\n')
        );
    };

    // GENERIC FUNCTIONS ----------------------------------

    // apList (<*>) :: [a -> b] -> [a] -> [b]
    const apList = fs => xs =>
        // The application of each of a list of functions,
        // to each of a list of values.
        fs.flatMap(
            f => xs.flatMap(x => [f(x)])
        );

    // comparing :: (a -> b) -> (a -> a -> Ordering)
    const comparing = f =>
        (x, y) => {
            const
                a = f(x),
                b = f(y);
            return a < b ? -1 : (a > b ? 1 : 0);
        };

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f =>
        1 < f.length ? (
            (a, b) => f(b, a)
        ) : (x => y => f(y)(x));

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // mappendOrd (<>) :: Ordering -> Ordering -> Ordering
    const mappendOrd = (a, b) => a !== 0 ? a : b;

    // sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    const sortBy = f => xs =>
        xs.slice()
        .sort(f);

    // toLower :: String -> String
    const toLower = s => s.toLocaleLowerCase();

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
be
to
are
Here
some
sample
sorted
strings

strings
sample
sorted
Here
some
are
be
to
```



## jq

The comparator, cmp, must have 0 arity, and may either be boolean or follow the negative/zero/positive convention.

If "o" is an ordering, and if x and y are two entities for which "x o y" is defined, then "[x,y] | cmp" should return a number, or a boolean value.

As illustrated in the example, the comparator may be any jq filter, whether or not it is defined as a function.

```jq
def quicksort(cmp):
  if length < 2 then .                                     # it is already sorted
  else .[0] as $pivot
    | reduce .[] as $x
      # state: [less, equal, greater]
      ( [ [], [], [] ];                                    # three empty arrays:
        if   $x == $pivot then    .[1] += [$x]             # add x to equal
        else ([$x,$pivot]|cmp) as $order
          | if   $order == 0 then .[1] += [$x]             # ditto
            elif ($order|type) == "number" then
              if $order < 0 then  .[0] += [$x]             # add x to less
              else .[2] += [$x]                            # add x to greater
              end
            else ([$pivot,$x]|cmp) as $order2
              | if $order and $order2 then   .[1] += [$x]  # add x to equal
                elif $order then   .[0] += [$x]            # add x to less
                else .[2] += [$x]                          # add x to greater
                end
            end
        end )
    | (.[0] | quicksort(cmp) ) + .[1] + (.[2] | quicksort(cmp) )
  end ;
```

Example:

```jq
# Sort by string length, breaking ties using ordinary string comparison.
["z", "yz", "ab", "c"]
  | quicksort( (.[0]|length) > (.[1]|length) or ( (.[0]|length) == (.[1]|length) and .[0] < .[1] ) )

```

{{Out}}

```jq
[
  "ab",
  "yz",
  "c",
  "z"
]
```



## Julia

My word list source is the opening sentence of Shelly's [http://www.gutenberg.org/cache/epub/84/pg84.txt Frankenstein].

```julia
wl = filter(!isempty, split("""You will rejoice to hear that no disaster has accompanied the
    commencement of an enterprise which you have regarded with such evil
    forebodings.""", r"\W+"))

println("Original list:\n - ", join(wl, "\n - "))
sort!(wl; by=x -> (-length(x), lowercase(x)))
println("\nSorted list:\n - ", join(wl, "\n - "))

```


{{out}}

```txt
Original List:
   You
   will
   rejoice
   to
   hear
   that
   no
   disaster
   has
   accompanied
   the
   commencement
   of
   an
   enterprise
   which
   you
   have
   regarded
   with
   such
   evil
   forebodings

Sorted List:
   commencement
   accompanied
   forebodings
   enterprise
   disaster
   regarded
   rejoice
   which
   evil
   have
   hear
   such
   that
   will
   with
   has
   the
   You
   you
   an
   no
   of
   to

```



## Kotlin

A translation from Java, also showing the seamless interop between Java and Kotlin code.


```kotlin
import java.util.Arrays

fun main(args: Array<String>) {
    val strings = arrayOf("Here", "are", "some", "sample", "strings", "to", "be", "sorted")

    fun printArray(message: String, array: Array<String>) = with(array) {
        print("$message [")
        forEachIndexed { index, string ->
            print(if (index == lastIndex) string else "$string, ")
        }
        println("]")
    }

    printArray("Unsorted:", strings)

    Arrays.sort(strings) { first, second ->
        val lengthDifference = second.length - first.length
        if (lengthDifference == 0) first.compareTo(second, true) else lengthDifference
    }

    printArray("Sorted:", strings)
}
```


{{out}}

```txt
Unsorted: [Here, are, some, sample, strings, to, be, sorted]
Sorted:   [strings, sample, sorted, Here, some, are, be, to]
```



## Lua


```lua
test = { "Here", "we", "have", "some", "sample", "strings", "to", "be", "sorted" }

function stringSorter(a, b)
	if string.len(a) == string.len(b) then
		return string.lower(a) < string.lower(b)
	end
	return string.len(a) > string.len(b)
end
table.sort(test, stringSorter)

-- print sorted table
for k,v in pairs(test) do print(v) end
```


{{out}}

```txt
strings sample sorted have Here some be to we
```



## M2000 Interpreter

Report statement print document but stop at 3/4 of console lines waiting keypress or space to show more lines. So when run this example press space to continue. Clipboard has the output too.



```M2000 Interpreter

Module Checkit {
      Class Quick {
            Private:
         partition=lambda-> {
               Read &A(), p, r : i = p-1 : x=A(r)
               For j=p to r-1 {If .LE(A(j), x) Then i++:Swap A(i),A(j)
               } : Swap A(i+1), A(r) :  Push  i+2, i 
         }
            Public:
         LE=Lambda->Number<=Number
         Module ForStrings {
               .partition<=lambda-> {
                     Read &a$(), p, r : i = p-1 : x$=a$(r)
                     For j=p to r-1 {If a$(j)<= x$ Then i++:Swap a$(i),a$(j)
                     } : Swap a$(i+1), a$(r) : Push i+2, i
               }
         }
         Function quicksort {
              Read ref$
              {
                      loop : If Stackitem() >= Stackitem(2) Then Drop 2 : if  empty then {Break} else continue
                      over 2,2 : call .partition(ref$) :shift 3 
              }
         }
      }
      Quick=Quick()
      
      ToSort$="this is a set of strings to sort This Is A Set Of Strings To Sort"
      Dim a$()
      a$()=Piece$(ToSort$, " ")
      \\ we can redim to any range
      Dim a$(100 to len(a$())+99)  ' from 100 to 115 (16 items)
      Group Quick {
            Module ForStringsSpecial {
               .partition<=lambda-> {
                        Read &a$(), p, r : i = p-1 : x$=a$(r) :lx$=lcase$(x$) : k=len(x$)
                        For j=p to r-1 {
                              m=len(a$(j))
                              select case compare(m, k)
                              case 0
                              {
                                   aj$=lcase$(a$(j))                              
                                   if aj$>lx$ then exit
                                   if aj$=lx$ then if a$(j)<=x$ then exit
                                   i++ 
                                   Swap a$(i),a$(j) 
                              }
                              case 1
                              {
                                    i++:Swap a$(i),a$(j)
                              }
                              End Select
                     } : Swap a$(i+1), a$(r) : Push i+2, i
               }
         }
      }
      Document doc$={Unsorted List:
      } 
      k=each(a$())
      While k {
            doc$="   "+array$(k)+{
            }
      }
      Quick.ForStringsSpecial
      \\ Dimension(a$(), 0, 1) is Lbound a$() first dimension
      \\ Dimension(a$(), 0, 1) is Ubound a$() first dimension
      Call Quick.quicksort(&a$(), Dimension(a$(), 0, 1), Dimension(a$(), 1,1))
      k=each(a$())
      Doc$={
            Sorted List:
            }
      While k {
            doc$="   "+array$(k)+{
            }
      }
      Report doc$
      Clipboard doc$
}
Checkit

```


ForStringsSpecial can be coded using a Compare(aj$, lx$). See the use of break to break cases in select cases.
Any case in  Select case may have one statement (if then is one statement), or a block of code. We can leave a case with a blank line after, a one statement line, or a block of code, or a case statement. A break statement break cases, so all code executed, until a continue found, to exit from Select (next statement after End Select). We use a sub to make two statements as one.


```M2000 Interpreter

Group Quick {
      Module ForStringsSpecial {
         .partition<=lambda-> {
                  Read &a$(), p, r : i = p-1 : x$=a$(r) :lx$=lcase$(x$) : k=len(x$)
                  For j=p to r-1 {
                        m=len(a$(j))
                        select case compare(m, k)
                        case 0
                        {
                              aj$=lcase$(a$(j))
                              \\ in Case the Break statement execute all cases until a case has a Continue
                              select case compare(aj$, lx$)
                              case 0
                                   if a$(j)>x$ then break
                              Case 1
                                   swapit()
                             End Select 
                        }
                        case 1
                             swapit()
                        End Select
               } : Swap a$(i+1), a$(r) : Push i+2, i
               Sub swapit()
                     i++:Swap a$(i),a$(j)
               End Sub
         }
   }
}

```


{{out}}

```txt

Unsorted List:
   this
   is
   a
   set
   of
   strings
   to
   sort
   This
   Is
   A
   Set
   Of
   Strings
   To
   Sort

Sorted List:
   strings
   Strings
   sort
   Sort
   this
   This
   set
   Set
   is
   Is
   of
   Of
   to
   To
   a
   A

```



## Mathematica

We define a new function to give true or false if two elements are in order. 
After that we can simply use the built-in Sort with an ordering function:

```Mathematica
StringOrderQ[x_String, y_String] := 
 If[StringLength[x] == StringLength[y],
       OrderedQ[{x, y}],
       StringLength[x] >StringLength[y]
   ]
words={"on","sunday","sander","sifted","and","sorted","sambaa","for","a","second"};
Sort[words,StringOrderQ]
```

gives back:

```txt
{sambaa,sander,second,sifted,sorted,sunday,and,for,on,a}
```



## Maxima


```maxima
strangeorderp(a, b) := slength(a) > slength(b) or (slength(a) = slength(b) and orderlessp(a, b))$
s: tokens("Lorem ipsum dolor sit amet consectetur adipiscing elit Sed non risus Suspendisse\
 lectus tortor dignissim sit amet adipiscing nec ultricies sed dolor")$

sort(s, strangeorderp);
["Suspendisse", "consectetur", "adipiscing", "adipiscing", "dignissim", "ultricies",
"lectus", "tortor", "Lorem", "dolor", "dolor", "ipsum", "risus", "amet", "amet",
"elit", "Sed", "nec", "non", "sed", "sit", "sit"]
```



## MAXScript


```maxscript
fn myCmp str1 str2 =
(
    case of
    (
        (str1.count < str2.count):  1
        (str1.count > str2.count): -1
        default:(
                -- String compare is case sensitive, name compare isn't. Hence...
                str1 = str1 as name
                str2 = str2 as name
                case of
                (
                    (str1 > str2):  1
                    (str1 < str2): -1
                    default:        0
                )
                )
    )
)	

strList = #("Here", "are", "some", "sample", "strings", "to", "be", "sorted")
qSort strList myCmp
print strList
```



## min

{{works with|min|0.19.3}}

```min
("Here" "are" "some" "sample" "strings" "to" "be" "sorted")
(((length) (length)) spread <) sort print
```

{{out}}

```txt

("strings" "sample" "sorted" "Here" "some" "are" "to" "be")

```



## Nemerle


```Nemerle
using System.Console;

module CustomSort
{
    Main() : void
    {
        def strings1 = ["these", "are", "strings", "of", "different", "length"];
        def strings2 = ["apple", "House", "chewy", "Salty", "rises", "Later"];
        
        WriteLine(strings1.Sort((x, y) => y.Length.CompareTo(x.Length)));
        WriteLine(strings2.Sort((x, y) => x.CompareTo(y)))
    }
}
```

{{out}}

```txt
[different, strings, length, these, are, of]
[apple, chewy, House, Later, rises, Salty]
```



## NetRexx

{{trans|Java}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

-- 
### =======================================================================

class RSortCustomComparator public
  
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method main(args = String[]) public static
  sample = [String 'Here', 'are', 'some', 'sample', 'strings', 'to', 'be', 'sorted']
  say displayArray(sample)
  Arrays.sort(sample, LengthComparator())
  say displayArray(sample)
  return

method displayArray(harry = String[]) constant
  disp = ''
  loop elmt over harry
    disp = disp','elmt
    end elmt
  return '['disp.substr(2)']' -- trim leading comma

-- 
### =======================================================================

class RSortCustomComparator.LengthComparator implements Comparator

method compare(lft = Object, rgt = Object) public binary returns int
  cRes = int
  if lft <= String, rgt <= String then do
    cRes = (String rgt).length - (String lft).length
    if cRes == 0 then cRes = (String lft).compareToIgnoreCase(String rgt)
    end
  else signal IllegalArgumentException('Arguments must be Strings')
  return cRes

```

{{out}}

```txt

[Here,are,some,sample,strings,to,be,sorted]
[strings,sample,sorted,Here,some,are,be,to]

```



## Nial


```nial
sort fork [=[tally first,tally last],up, >= [tally first,tally last]] ['Here', 'are', 'some', 'sample', 'strings', 'to', 'be', 'sorted']
=+-------+------+------+----+----+---+--+--+
=|strings|sample|sorted|Here|some|are|be|to|
=+-------+------+------+----+----+---+--+--+
```



## Nim


```nim
import strutils, algorithm

var strings = "here are Some sample strings to be sorted".split(' ')

strings.sort(proc (x, y: string): int =
  result = cmp(y.len, x.len)
  if result == 0:
    result = cmpIgnoreCase(x, y)
)

echo strings
```



## Objeck


```objeck
use Collection;

class Test {
  function : Main(args : String[]) ~ Nil {
    v := CreateHolders(["Here", "are", "some", "sample", "strings", "to", "be", "sorted"]);
    "unsorted: "->Print(); Show(v);
    v->Sort();
    "sorted: "->Print(); Show(v);
  }
  
  function : CreateHolders(strings : String[]) ~ CompareVector {
    vector := CompareVector->New();
    each(i : strings) {
      vector->AddBack(StringHolder->New(strings[i]));
    };
    
    return vector;
  }
  
  function : Show(v : CompareVector) ~ Nil {
    each(i : v) {
      s := v->Get(i)->As(StringHolder);
      s->ToString()->Print();
      if(i + 1 < v->Size()) {
        ','->Print();
      };
    };
    '\n'->Print();
  }
}

class StringHolder implements Compare {
  @s : String;
  
  New(s : String) {
    @s := s;
  }
  
  method : public : Compare(c : Compare) ~ Int {
    h := c->As(StringHolder);
    r := h->ToString();
    size := r->Size() - @s->Size();
    if(size = 0) {
      size := @s->ToUpper()->Compare(r->ToUpper());
    };
    
    return size;
  }
  
  method : public : HashID() ~ Int {
    return @s->HashID();
  }
  
  method : public : ToString() ~ String {
    return @s;
  }
}
```



```txt

unsorted: Here,are,some,sample,strings,to,be,sorted
sorted: strings,sample,sorted,Here,some,are,be,to

```


=={{header|Objective-C}}==
{{works with|Cocoa|Mac OS X 10.6+}}
Using blocks:

```objc>#import <Foundation/Foundation.h


#define esign(X) (((X)>0)?1:(((X)<0)?-1:0))

int main()
{
  @autoreleasepool {

    NSMutableArray *arr =
    [NSMutableArray
      arrayWithArray: [@"this is a set of strings to sort"
                       componentsSeparatedByString: @" "]
     ];

    [arr sortUsingComparator: ^NSComparisonResult(id obj1, id obj2){
      NSComparisonResult l = esign((int)([obj1 length] - [obj2 length]));
      return l ? -l // reverse the ordering
               : [obj1 caseInsensitiveCompare: obj2];
     }];

    for( NSString *str in arr )
    {
      NSLog(@"%@", str);
    }

  }
  return EXIT_SUCCESS;
}
```



{{works with|GNUstep}}

{{works with|Cocoa}}

```objc>#import <Foundation/Foundation.h


@interface NSString (CustomComp)
- (NSComparisonResult)my_compare: (id)obj;
@end

#define esign(X) (((X)>0)?1:(((X)<0)?-1:0))
@implementation NSString (CustomComp)
- (NSComparisonResult)my_compare: (id)obj
{
  NSComparisonResult l = esign((int)([self length] - [obj length]));
  return l ? -l // reverse the ordering
           : [self caseInsensitiveCompare: obj];
}
@end

int main()
{
  @autoreleasepool {

    NSMutableArray *arr =
       [NSMutableArray
         arrayWithArray: [@"this is a set of strings to sort"
                           componentsSeparatedByString: @" "]
       ];

    [arr sortUsingSelector: @selector(my_compare:)];

    for ( NSString *str in arr )
    {
      NSLog(@"%@", str);
    }

  }
  return EXIT_SUCCESS;
}
```


This example can also be written using sort descriptors:
{{works with|GNUstep}}
{{works with|Cocoa}}

```objc>#import <Foundation/Foundation.h


int main()
{
  @autoreleasepool {

    NSArray *strings = [@"Here are some sample strings to be sorted" componentsSeparatedByString:@" "];

    NSSortDescriptor *sd1 = [[NSSortDescriptor alloc] initWithKey:@"length" ascending:NO];
    NSSortDescriptor *sd2 = [[NSSortDescriptor alloc] initWithKey:@"lowercaseString" ascending:YES];

    NSArray *sorted = [strings sortedArrayUsingDescriptors:@[sd1, sd2]];
    NSLog(@"%@", sorted);

  }

  return 0;
}
```



## OCaml


```ocaml
let mycmp s1 s2 =
  if String.length s1 <> String.length s2 then
    compare (String.length s2) (String.length s1)
  else
    String.compare (String.lowercase s1) (String.lowercase s2)
```


List:

```ocaml
# let strings = ["Here"; "are"; "some"; "sample"; "strings"; "to"; "be"; "sorted"];;
val strings : string list =
  ["Here"; "are"; "some"; "sample"; "strings"; "to"; "be"; "sorted"]
# List.sort mycmp strings;;
- : string list =
["strings"; "sample"; "sorted"; "Here"; "some"; "are"; "be"; "to"]
```


Array:

```ocaml
# let strings = [|"Here"; "are"; "some"; "sample"; "strings"; "to"; "be"; "sorted"|];;
val strings : string array =
  [|"Here"; "are"; "some"; "sample"; "strings"; "to"; "be"; "sorted"|]
# Array.sort mycmp strings;;
- : unit = ()
# strings;;
- : string array =
[|"strings"; "sample"; "sorted"; "Here"; "some"; "are"; "be"; "to"|]
```



## Oforth



```Oforth
String method: customCmp(s)
   s size self size > ifTrue: [ true return ]
   s size self size < ifTrue: [ false return ]
   s toUpper self toUpper <= ;

["this", "is", "a", "set", "of", "strings", "to", "sort", "This", "Is", "A", "Set", "Of", "Strings", "To", "Sort"] 
sortWith(#customCmp) println
```


{{out}}

```txt

[Strings, strings, Sort, sort, this, This, Set, set, is, Is, of, Of, To, to, A, a]

```



## ooRexx


```ooRexx
A=.array~of('The seven deadly sins','Pride','avarice','Wrath','envy','gluttony','sloth','Lust')
say 'Sorted in order of descending length, and in ascending lexicographic order'
say A~sortWith(.DescLengthAscLexical~new)~makeString
 
::class DescLengthAscLexical mixinclass Comparator
::method compare
use strict arg left, right
if left~length==right~length
  then return left~caselessCompareTo(right)
  else return right~length-left~length
```

{{out}}

```txt

Sorted in order of descending length, and in ascending lexicographic order
The seven deadly sins
gluttony
avarice
Pride
sloth
Wrath
envy
Lust

```



## Oz


```oz
declare
  fun {LexicographicLessThan Xs Ys}
     for
        X in {Map Xs Char.toLower}
        Y in {Map Ys Char.toLower}
        return:Return
        default:{Length Xs}<{Length Ys}
     do
        if X < Y then {Return true} end
     end
  end
 
  fun {LessThan Xs Ys}
     {Length Xs} > {Length Ys}
     orelse
     {Length Xs} == {Length Ys} andthen {LexicographicLessThan Xs Ys}
  end
 
  Strings = ["Here" "are" "some" "sample" "strings" "to" "be" "sorted"]
in
  {ForAll {Sort Strings LessThan} System.showInfo}
```



## PARI/GP


```parigp
cmp(a,b)=if(#a<#b,1,if(#a>#b,-1,lex(a,b)));
vecsort(v,cmp)
```



## Pascal

{{works with|Free Pascal}}
See at http://rosettacode.org/wiki/Sorting_algorithms/Merge_sort#improvement struct/record with myText implementing this task too


## Perl

{{works with|Perl|5.8.6}}

```perl
sub mycmp { length $b <=> length $a || lc $a cmp lc $b }

my @strings = ("Here", "are", "some", "sample", "strings", "to", "be", "sorted");
my @sorted = sort mycmp @strings;
```


Or inline:

```perl
my @strings = qw/here are some sample strings to be sorted/;
my @sorted = sort {length $b <=> length $a || lc $a cmp lc $b} @strings
```


Faster with a [[wp:Schwartzian transform|Schwartzian transform]]:

```perl
my @strings = qw/here are some sample strings to be sorted/;
my @sorted = map  { $_->[0] }
             sort { $a->[1] <=> $b->[1] || $a->[2] cmp $b->[2] }
             map  { [ $_, length, lc ] }
             @strings;
```



## Perl 6


```perl6>my @strings = <Here are some sample strings to be sorted
;
my @sorted_strings = sort { $^a.chars <=> $^b.chars or $^a.lc cmp $^b.lc }, @strings;
.say for @sorted_strings;

# If instead the function you feed to <code>sort</code> is of arity 1, it will do the Schwartzian transform for you, automatically sorting numeric fields numerically, and strings fields stringily:

say @sorted_strings = sort -> $x { [ $x.chars, $x.lc ] }, @strings;
```



## Phix


```Phix
function my_compare(sequence a, b)
    integer c = -compare(length(a),length(b))  -- descending length
    if c=0 then
        c = compare(lower(a),lower(b))         -- ascending lexical within == length
    end if
    return c
end function
?custom_sort(routine_id("my_compare"),{"Here", "are", "some", "sample", "strings", "to", "be", "sorted"})
```

{{out}}

```txt

{"strings","sample","sorted","Here","some","are","be","to"}

```



## PHP

{{works with|PHP|4.4.4 CLI}}

```php
<?php
function mycmp($s1, $s2)
{
    if ($d = strlen($s2) - strlen($s1))
        return $d;
    return strcasecmp($s1, $s2);
}

$strings = array("Here", "are", "some", "sample", "strings", "to", "be", "sorted");
usort($strings, "mycmp");
?>
```



## PicoLisp

By default, the [http://software-lab.de/doc/refS.html#sort sort] function in
PicoLisp returns an ascending list (of any type). To get a result in descending
order, the "greater than" function can be supplied

```PicoLisp
: (sort '("def" "abc" "ghi") >)    
-> ("ghi" "def" "abc")
```

or simply the result reversed (which is, btw, the most efficient way)

```PicoLisp
: (flip (sort '("def" "abc" "ghi")))
-> ("ghi" "def" "abc")
```



## PL/I

{{works with|IBM PL/I|7.5}}

'''Platform:''' [[WIN]]

```pli
MRGEPKG: package exports(MERGESORT,MERGE,RMERGE);

DCL (T(4)) CHAR(20) VAR; /* scratch space of length N/2 */

MERGE: PROCEDURE (A,LA,B,LB,C,CMPFN);
   DECLARE (A(*),B(*),C(*)) CHAR(*) VAR;
   DECLARE (LA,LB) FIXED BIN(31) NONASGN;
   DECLARE (I,J,K) FIXED BIN(31);
   DECLARE CMPFN ENTRY(
          NONASGN CHAR(*) VAR,
          NONASGN CHAR(*) VAR)
          RETURNS (FIXED bin(31));
   
   I=1; J=1; K=1;
   DO WHILE ((I <= LA) & (J <= LB));
      IF CMPFN(A(I),B(J)) <= 0 THEN
         DO; C(K)=A(I); K=K+1; I=I+1; END;
      ELSE
         DO; C(K)=B(J); K=K+1; J=J+1; END;
   END;
   DO WHILE (I <= LA);
      C(K)=A(I); I=I+1; K=K+1;
   END;
   return;
END MERGE;

MERGESORT: PROCEDURE (A,N,CMPFN) RECURSIVE ;
     DECLARE (A(*))               CHAR(*) VAR;
     DECLARE N                    FIXED BINARY(31) NONASGN;
     DECLARE CMPFN                ENTRY(
          NONASGN CHAR(*) VAR,
          NONASGN CHAR(*) VAR)
                                  RETURNS (FIXED bin(31));
     DECLARE (M,I)                FIXED BINARY;
     DECLARE AMP1(N)              CHAR(20) VAR BASED(P);
     DECLARE P POINTER;

   IF (N=1) THEN RETURN;
   M = trunc((N+1)/2);
   IF M > 1 THEN CALL MERGESORT(A,M,CMPFN);
   P=ADDR(A(M+1)); 
   IF (N-M > 1) THEN CALL MERGESORT(AMP1,N-M,CMPFN);
   IF CMPFN(A(M),AMP1(1)) <= 0 THEN RETURN;
   DO I=1 to M; T(I)=A(I); END;
   CALL MERGE(T,M,AMP1,N-M,A,CMPFN);
END MERGESORT;

RMERGE: PROC OPTIONS(MAIN);
DCL I FIXED BIN(31);
DCL A(8) CHAR(20) VAR INIT("this","is","a","set","of","strings","to","sort");

MyCMP: PROCEDURE(A,B) RETURNS (FIXED BIN(31));
   DCL (A,B) CHAR(*) VAR NONASGN;
   DCL (I,J) FIXED BIN(31);

   I = length(trim(A)); J = length(trim(B));
   IF I < J THEN RETURN(+1);
   IF I > J THEN RETURN(-1);
   IF lowercase(A) < lowercase(B) THEN RETURN(-1);
   IF lowercase(A) > lowercase(B) THEN RETURN(+1);
   RETURN (0);
END MyCMP;

CALL MERGESORT(A,8,MyCMP);
DO I=1 TO 8;
   put edit (I,A(I)) (F(5),X(2),A(10)) skip;
END;

put skip;
END RMERGE;
```



## Pop11


```pop11
lvars ls = ['Here' 'are' 'some' 'sample' 'strings' 'to' 'be' 'sorted'];
define compare(s1, s2);
lvars k = length(s2) - length(s1);
if k < 0 then
    return(true);
elseif k > 0 then
    return(false);
else
    return (alphabefore(uppertolower(s1), uppertolower(s2)));
endif;
enddefine;

syssort(ls, compare) -> ls;

NOTE: The definition of compare can also be written thus:
define compare(s1, s2);
 lvars
     l1 = length(s1),
     l2 = length(s2);
 l1 > l2 or (l1 == l2 and alphabefore(uppertolower(s1), uppertolower(s2)))
enddefine;
```



## PowerBASIC

{{works with|PB/Win|9}}
{{works with|PB/CC|4}}


```powerbasic
FUNCTION Sorter(p1 AS STRING, p2 AS STRING) AS LONG
    'if p1 should be first, returns -1
    'if p2 should be first, returns 1
    '     if they're equal, returns 0
    IF LEN(p1) > LEN(p2) THEN
        FUNCTION = -1
    ELSEIF LEN(p2) > LEN(p1) THEN
        FUNCTION = 1
    ELSEIF UCASE$(p1) > UCASE$(p2) THEN
        'if we get here, they're of equal length,
        'so now we're doing a "normal" string comparison
        FUNCTION = -1
    ELSEIF UCASE$(p2) > UCASE$(p1) THEN
        FUNCTION = 1
    ELSE
        FUNCTION = 0
    END IF
END FUNCTION

FUNCTION PBMAIN()
    DIM x(7) AS STRING
    ARRAY ASSIGN x() = "Here", "are", "some", "sample", "strings", "to", "be", "sorted"

    'pb's built-in sorting; "USING" tells it to use our custom comparator
    ARRAY SORT x(), USING Sorter()
END FUNCTION
```



## PowerShell

The <code>Sort-Object</code> cmdlet accepts script blocks as arguments as well as multiple criteria after which to sort.

```powershell
$list = "Here", "are", "some", "sample", "strings", "to", "be", "sorted"
$list | Sort-Object {-$_.Length},{$_}
```

The negated string length is the first sort criterion, the second is the string itself, resulting in descending length and ascending lexicographic order.


## Prolog

Works with SWI-Prolog.

```Prolog
rosetta_sort :-
	L = ["Here", "are", "some", "sample", "strings", "to", "be", "sorted" ],
	predsort(my_comp, L, L1),
	writeln('Input list :'),
	maplist(my_write, L), nl,nl,
	writeln('Sorted list :'),
	maplist(my_write, L1).


my_comp(Comp, W1, W2) :-
	length(W1,L1),
	length(W2, L2),
	(   L1 < L2 -> Comp = '>'
	;   L1 > L2 -> Comp = '<'
	;   compare(Comp, W1, W2)).

my_write(W) :-
	format('~s ', [W]).

```


{{out}}

```txt
 ?- rosetta_sort.
Input list :
Here are some sample strings to be sorted 

Sorted list :
strings sample sorted Here some are be to 
true.

```



## Python

Using a key function is usually more efficient than a comparator. We can take advantage of the fact that tuples are ordered first by the first element, then by the second, etc., to perform a sort on multiple criteria.

```python
strings = "here are Some sample strings to be sorted".split()

def mykey(x):
    return -len(x), x.upper()

print sorted(strings, key=mykey)
```


{{out}}

```python
['strings', 'sample', 'sorted', 'here', 'Some', 'are', 'be', 'to']
```



### Alternative method using cmp

To technically comply with this task, we can also use an actual comparator (''cmp'') function which will be called every time members of the original list are to be compared. Note that this feature is worse than using the key argument and has been removed from Python 3, so should no longer be used in new code.

```python
def mycmp(s1, s2):
    return cmp(len(s2), len(s1)) or cmp(s1.upper(), s2.upper())

print sorted(strings, cmp=mycmp)
```



## R



```R
v = c("Here", "are", "some", "sample", "strings", "to", "be", "sorted")
print(v[order(-nchar(v), tolower(v))])
```



## Racket



```Racket

#lang racket

;; Using a combination of the two comparisons
(define (sort1 words)
  (sort words (λ(x y)
                (define xl (string-length x)) (define yl (string-length y))
                (or (> xl yl) (and (= xl yl) (string-ci<? x y))))))
(sort1 '("Some" "pile" "of" "words"))
;; -> '("words" "pile" "Some" "of")

;; Doing two sorts, relying on `sort's stability
(define (sort2 words)
  (sort (sort words string-ci<?) > #:key string-length))
(sort2 '("Some" "pile" "of" "words"))
;; -> '("words" "pile" "Some" "of")

```



## REXX


```rexx
/*REXX program sorts a (stemmed) array using the  merge-sort method. */
/*   using mycmp function for the sort order                         */
/**********************************************************************
* mergesort taken from REXX (adapted for ooRexx (and all other REXXes))
* 28.07.2013 Walter Pachl
**********************************************************************/
  Call gena                        /* generate the array elements.   */
  Call showa 'before sort'         /* show the before array elements.*/
  Call mergeSort highitem          /* invoke the merge sort for array*/
  Call showa ' after sort'         /* show the  after array elements.*/
  Exit                             /* stick a fork in it, we're done.*/
/*---------------------------------GENa subroutine-------------------*/
gena:
  a.=''                            /* assign default value for a stem*/
  a.1='---The seven deadly sins---'/* everybody:  pick your favorite.*/
  a.2='
### =====================
'
  a.3='pride'
  a.4='avarice'
  a.5='wrath'
  a.6='envy'
  a.7='gluttony'
  a.8='sloth'
  a.9='lust'
  Do highitem=1 While a.highitem\=='' /*find number of entries       */
    End
  highitem=highitem-1              /* adjust highitem by -1.         */
  Return
/*---------------------------------MERGETOa subroutine---------------*/
mergetoa: Procedure Expose a. !.
  Parse Arg l,n
  Select
    When n==1 Then
      Nop
    When n==2 Then Do
      h=l+1
      If mycmp(a.l,a.h)=1 Then Do
        _=a.h
        a.h=a.l
        a.l=_
        End
      End
    Otherwise Do
      m=n%2
      Call mergeToa l+m,n-m
      Call mergeTo! l,m,1
      i=1
      j=l+m
      Do k=l While k<j
        If j==l+n|mycmp(!.i,a.j)<>1 Then Do
          a.k=!.i
          i=i+1
          End
        Else Do
          a.k=a.j
          j=j+1
          End
        End
      End
    End
  Return
/*---------------------------------MERGESORT subroutine--------------*/
mergesort: Procedure Expose a.
  Call mergeToa 1,arg(1)
  Return
/*---------------------------------MERGETO! subroutine---------------*/
mergeto!: Procedure Expose a. !.
  Parse Arg l,n,_
  Select
    When n==1 Then
      !._=a.l
    When n==2 Then Do
      h=l+1
      q=1+_
      If mycmp(a.l,a.h)=1 Then Do
        q=_
        _=q+1
        End
      !._=a.l
      !.q=a.h
      Return
      End
    Otherwise Do
      m=n%2
      Call mergeToa l,m
      Call mergeTo! l+m,n-m,m+_
      i=l
      j=m+_
      Do k=_ While k<j
        If j==n+_|mycmp(a.i,!.j)<>1 Then Do
          !.k=a.i
          i=i+1
          End
        Else Do
          !.k=!.j
          j=j+1
          End
        End
      End
    End
  Return
/*---------------------------------SHOWa subroutine------------------*/
showa:
  widthh=length(highitem)           /* maximum the width of any line.*/
  Do j=1 For highitem
    Say 'element' right(j,widthh) arg(1)':' a.j
    End
  Say copies('-',60)                /* show a separator line (fence).*/
  Return

mycmp: Procedure
/**********************************************************************
* shorter string considered higher
* when lengths are equal: caseless 'Z' considered higher than 'X' etc.
* Result:  1  B consider higher than A
*         -1  A consider higher than B
*          0  A==B (caseless)
**********************************************************************/
  Parse Upper Arg A,B
    A=strip(A)
    B=strip(B)
    I = length(A)
    J = length(B)
    Select
      When I << J THEN res=1
      When I >> J THEN res=-1
      When A >> B THEN res=1
      When A << B THEN res=-1
      Otherwise        res=0
      End
    RETURN res
```

{{out}}

```txt

element 1 before sort: ---The seven deadly sins---
element 2 before sort: 
### =====================

element 3 before sort: pride
element 4 before sort: avarice
element 5 before sort: wrath
element 6 before sort: envy
element 7 before sort: gluttony
element 8 before sort: sloth
element 9 before sort: lust
------------------------------------------------------------
element 1  after sort: ---The seven deadly sins---
element 2  after sort: 
### =====================

element 3  after sort: gluttony
element 4  after sort: avarice
element 5  after sort: pride
element 6  after sort: sloth
element 7  after sort: wrath
element 8  after sort: envy
element 9  after sort: lust
------------------------------------------------------------

```



## Ring


```ring

load "stdlib.ring"

sList = newlist(8, 2)
aList = ["Here", "are", "some", "sample", "strings", "to", "be", "sorted"]
ind = len(aList)

for n = 1 to ind
    sList[n] [1] = aList[n]
    sList[n] [2] = len(aList[n])
next

nList = sortFirstSecond(sList, 2)
oList = newlist(8, 2)
count = 0

for n = len(nList) to 1 step -1
    count = count + 1
    oList[count] [1] = nList[n] [1]
    oList[count] [2] = nList[n] [2]
next

for n = 1 to len(oList) - 1
    temp1 = oList[n] [1]
    temp2 = oList[n+1] [1]
    if (oList[n] [2] = oList[n+1] [2]) and (strcmp(temp1, temp2) > 0)
       temp = oList[n] [1]
       oList[n] [1] = oList[n+1] [1]
       oList[n+1] [1] = temp
    ok
next

for n = 1 to len(oList)
    see oList[n] [1] + nl
next

```

Output:

```txt

strings
sample
sorted
Here
some
are
be
to

```



## Ruby

Since Ruby 1.8.6 Enumerables have a "sort_by" method, taking a key block, which is more efficient than a comparator. We can take advantage of the fact that Arrays are ordered first by the first element, then by the second, etc., to perform a sort on multiple criteria.


```ruby
words = %w(Here are some sample strings to be sorted)
p words.sort_by {|word| [-word.size, word.downcase]}
```


To technically comply with this task, we can also use an actual comparator block which will be called every time members of the original list are to be compared.

```ruby
p words.sort {|a, b| d = b.size <=> a.size
                     d != 0 ? d : a.upcase <=> b.upcase}
```



## Rust


```rust

fn main() {
    let mut words = ["Here", "are", "some", "sample", "strings", "to", "be", "sorted"];
    words.sort_by(|l, r| Ord::cmp(&r.len(), &l.len()).then(Ord::cmp(l, r)));
    println!("{:?}", words);
}

```



## Sather


```sather
class MAIN is

  custom_comp(a, b:STR):BOOL is
    l ::= a.length - b.length;
    if l = 0 then return a.lower < b.lower; end;
    return l > 0;
  end;

  main is
    s:ARRAY{STR} := |"this", "is", "an", "array", "of", "strings", "to", "sort"|;
    
    s.insertion_sort_by(bind(custom_comp(_,_)));
    loop #OUT + s.elt! + "\n"; end;
  end;
end;
```



## Scala


```scala
List("Here", "are", "some", "sample", "strings", "to", "be", "sorted").sortWith{(a,b) => 
  val cmp=a.size-b.size
  (if (cmp==0) -a.compareTo(b) else cmp) > 0
}
```

{{out}}

```txt
List(strings, sample, sorted, Here, some, are, be, to)
```



## Scheme


```scheme
(use srfi-13);;Syntax for module inclusion depends on implementation, 
;;a sort function may be predefined, or available through srfi 95
(define (mypred? a b)
  (let ((len-a (string-length a))
	(len-b (string-length b)))
    (if (= len-a len-b)
	(string>? (string-downcase b) (string-downcase a))
        (> len-a len-b))))

(sort '("sorted" "here" "strings" "sample" "Some" "are" "be" "to") mypred?) 
```

{{out}}

```scheme
("strings" "sample" "sorted" "here" "Some" "are" "be" "to")
```




###  An alternative solution: 

{{works with|Gauche Scheme}}


```Scheme
(define strings '(
  "This" "Is" "A" "Set" "Of" "Strings" "To" "Sort" "duplicated"
  "this" "is" "a" "set" "of" "strings" "to" "sort" "duplicated"))

(print 
  (sort strings
    (lambda two
      (define sizes (map string-length two))
      (if (apply = sizes)
        (apply string-ci<? two)
        (apply > sizes)))))

```

{{out}}

```txt

(duplicated duplicated Strings strings Sort sort This this Set set Is is
 Of of To to A a)

```



## Sidef


```ruby
func mycmp(a, b) { (b.len <=> a.len) || (a.lc <=> b.lc) };
var strings = %w(Here are some sample strings to be sorted);
var sorted = strings.sort(mycmp);
```



## Slate


```slate
define: #words -> #('here' 'are' 'some' 'sample' 'strings' 'to' 'sort' 'since' 'this' 'exercise' 'is' 'not' 'really' 'all' 'that' 'dumb' '(sorry)').
words sortBy: [| :first :second | (first lexicographicallyCompare: second) isNegative]
```



## Smalltalk


```smalltalk
#('here' 'are' 'some' 'sample' 'strings' 'to' 'sort' 'since' 'this' 'exercise' 'is' 'not' 'really' 'all' 'that' 'dumb' '(sorry)' ) asSortedCollection
          sortBlock:
                     [:first :second | (second size = first size)
                                            ifFalse: [second size < first size]
                                            ifTrue: [first < second]]
```



## Standard ML

List:
{{works with|SML/NJ}}

```sml
fun mygt (s1, s2) =
  if size s1 <> size s2 then
    size s2 > size s1
  else
    String.map Char.toLower s1 > String.map Char.toLower s2
```



```sml
- val strings = ["Here", "are", "some", "sample", "strings", "to", "be", "sorted"];
val strings = ["Here","are","some","sample","strings","to","be","sorted"]
  : string list
- ListMergeSort.sort mygt strings;
val it = ["strings","sample","sorted","Here","some","are","be","to"]
  : string list
```


Array:
{{works with|SML/NJ}}

```sml
fun mycmp (s1, s2) =
  if size s1 <> size s2 then
    Int.compare (size s2, size s1)
  else
    String.compare (String.map Char.toLower s1, String.map Char.toLower s2)
```



```sml
- val strings = Array.fromList ["Here", "are", "some", "sample", "strings", "to", "be", "sorted"];
val strings = [|"Here","are","some","sample","strings","to","be","sorted"|]
  : string array
- ArrayQSort.sort mycmp strings;
val it = () : unit
- strings;
val it = [|"strings","sample","sorted","Here","some","are","be","to"|]
  : string array
```



## Swift

{{works with|Swift|2.x+}}

```swift
import Foundation

var list = ["this",
  "is",
  "a",
  "set",
  "of",
  "strings",
  "to",
  "sort",
  "This",
  "Is",
  "A",
  "Set",
  "Of",
  "Strings",
  "To",
  "Sort"]

list.sortInPlace {lhs, rhs in
  let lhsCount = lhs.characters.count
  let rhsCount = rhs.characters.count
  let result = rhsCount - lhsCount
  
  if result == 0 {
    return lhs.lowercaseString > rhs.lowercaseString
  }
  
  return lhsCount > rhsCount
}
```

{{works with|Swift|1.2}}

```swift
import Foundation

var list = ["this",
    "is",
    "a",
    "set",
    "of",
    "strings",
    "to",
    "sort",
    "This",
    "Is",
    "A",
    "Set",
    "Of",
    "Strings",
    "To",
    "Sort"]

sort(&list) {lhs, rhs in
    let lhsCount = count(lhs)
    let rhsCount = count(rhs)
    let result = rhsCount - lhsCount
    
    if result == 0 {
        return lhs.lowercaseString > rhs.lowercaseString
    }
    
    return lhsCount > rhsCount
}
```



## Tcl


```tcl
proc sorter {a b} {
    set la [string length $a]
    set lb [string length $b]
    if {$la < $lb} {
        return 1
    } elseif {$la > $lb} {
        return -1
    }
    return [string compare [string tolower $a] [string tolower $b]]
}

set strings {here are Some sample strings to be sorted}
lsort -command sorter $strings ;# ==> strings sample sorted here Some are be to
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
setofstrings="this is a set of strings to sort This Is A Set Of Strings To Sort"
unsorted=SPLIT (setofstrings,": :")
PRINT "1. setofstrings unsorted"
index=""
LOOP l=unsorted
PRINT l
length=LENGTH (l),index=APPEND(index,length)
ENDLOOP
index =DIGIT_INDEX (index)
sorted=INDEX_SORT (unsorted,index)
PRINT "2. setofstrings sorted"
*{sorted}

```

{{out}}
<pre style='height:30ex;overflow:scroll'>
1. setofstrings unsorted
this
is
a
set
of
strings
to
sort
This
Is
A
Set
Of
Strings
To
Sort
2. setofstrings sorted
a
A
is
of
to
Is
Of
To
set
Set
this
sort
This
Sort
strings
Strings 

```



## Ursala

A standard library function, psort, takes a list of binary relational predicates and returns a function that uses them in order of decreasing priority to perform a sort. 
The less or equal length predicate (leql) and lexically less or equal predicate (lleq) are also standard library functions. This task is therefore easily dispatched as shown.


```Ursala
#import std
#show+

data = <'this','is','a','list','of','strings','to','be','sorted'>

example = psort<not leql,lleq+ ~* ~&K31K30piK26 letters> data
```

The lleq library function is case sensitive, so it is composed with a function to convert the words to lower case on the fly (without destructively modifying them) in order to meet the task requirement of case insensitivity.

{{out}}
 strings
 sorted
 list
 this
 be
 is
 of
 to
 a


## Visual Basic .NET



```vbnet
Imports System

Module Sorting_Using_a_Custom_Comparator
    Function CustomComparator(ByVal x As String, ByVal y As String) As Integer
        Dim result As Integer
        result = y.Length - x.Length
        If result = 0 Then
            result = String.Compare(x, y, True)
        End If
        Return result
    End Function

    Sub Main()
        Dim strings As String() = {"test", "Zoom", "strings", "a"}

        Array.Sort(strings, New Comparison(Of String)(AddressOf CustomComparator))
    End Sub
End Module
```



## zkl


```zkl
s:=T("Cat","apple","Adam","zero","Xmas","quit","Level","add","Actor","base","butter");
r:=s.sort(fcn(a,b){ 
	    an,bn := a.len(),b.len(); 
            if(an==bn)(a.toLower() < b.toLower()) else (an > bn)
	  });
r.pump(Console.println);
```

{{out}}

```txt

butter
Actor
apple
Level
Adam
base
quit
Xmas
zero
add
Cat

```



{{omit from|GUISS|No sort facilities.To sort, we paste into a spreadsheet}}
