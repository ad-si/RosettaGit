+++
title = "Sort an array of composite structures"
description = ""
date = 2019-10-18T19:36:46Z
aliases = []
[extra]
id = 2099
[taxonomies]
categories = ["task", "Sorting"]
tags = []
languages = [
  "acl2",
  "ada",
  "algol_68",
  "applescript",
  "autohotkey",
  "awk",
  "babel",
  "bbc_basic",
  "bracmat",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "e",
  "echolisp",
  "elena",
  "elixir",
  "erlang",
  "euphoria",
  "factor",
  "fantom",
  "fortran",
  "freebasic",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "liberty_basic",
  "lua",
  "m2000_interpreter",
  "mathematica",
  "maxscript",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "oorexx",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "seed7",
  "sidef",
  "simula",
  "sql",
  "tcl",
  "unix_shell",
  "ursala",
  "xpl0",
  "zkl",
]
+++

## Task

Sort an array of composite structures by a key.


For example, if you define a composite structure that presents a name-value pair (in pseudo-code):

 Define structure pair such that:
    name as a string
    value as a string

and an array of such pairs:

 x: array of pairs

then define a sort routine that sorts the array ''x'' by the key ''name''.

This task can always be accomplished with [[Sorting Using a Custom Comparator]]. If your language is not listed here, please see the other article.





## ACL2


```Lisp
(defun insert-by-key (o os key)
   (cond ((endp os) (list o))
         ((< (cdr (assoc key o))
             (cdr (assoc key (first os))))
          (cons o os))
         (t (cons (first os)
                  (insert-by-key o (rest os) key)))))

(defun isort-by-key (os key)
   (if (endp os)
       nil
       (insert-by-key (first os)
                      (isort-by-key (rest os) key)
                      key)))

(isort-by-key
 '(((name  . "map")
   (weight . 9)
   (value  . 150))
  ((name   . "compass")
   (weight . 13)
   (value  . 35))
  ((name   . "water")
   (weight . 153)
   (value  . 200))
  ((name   . "sandwich")
   (weight . 50)
   (value  . 60))
  ((name   . "glucose")
   (weight . 15)
   (value  . 60)))
 'value)
```


Output:

```txt
(((NAME . "compass")
  (WEIGHT . 13)
  (VALUE . 35))
 ((NAME . "glucose")
  (WEIGHT . 15)
  (VALUE . 60))
 ((NAME . "sandwich")
  (WEIGHT . 50)
  (VALUE . 60))
 ((NAME . "map")
  (WEIGHT . 9)
  (VALUE . 150))
 ((NAME . "water")
  (WEIGHT . 153)
  (VALUE . 200)))
```



## Ada

[[Ada 2005]] defines 2 standard subprograms for sorting arrays - 1 for constrained arrays and 1 for unconstrained arrays. Below is a example of using the unconstrained version.

```ada
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Ada.Containers.Generic_Array_Sort;

procedure Demo_Array_Sort is

   function "+" (S : String) return Unbounded_String renames To_Unbounded_String;

   type A_Composite is
      record
         Name  : Unbounded_String;
         Value : Unbounded_String;
      end record;

   function "<" (L, R : A_Composite) return Boolean is
   begin
      return L.Name < R.Name;
   end "<";

   procedure Put_Line (C : A_Composite) is
   begin
      Put_Line (To_String (C.Name) & " " & To_String (C.Value));
   end Put_Line;

   type An_Array is array (Natural range <>) of A_Composite;

   procedure Sort is new Ada.Containers.Generic_Array_Sort (Natural, A_Composite, An_Array);

   Data : An_Array := (1 => (Name => +"Joe",    Value => +"5531"),
                       2 => (Name => +"Adam",   Value => +"2341"),
                       3 => (Name => +"Bernie", Value => +"122"),
                       4 => (Name => +"Walter", Value => +"1234"),
                       5 => (Name => +"David",  Value => +"19"));

begin
   Sort (Data);
   for I in Data'Range loop
      Put_Line (Data (I));
   end loop;
end Demo_Array_Sort;
```

Result:

```txt

  C:\Ada\sort_composites\lib\demo_array_sort
  Adam 2341
  Bernie 122
  David 19
  Joe 5531
  Walter 1234

```

[[Ada 2005]] also provides ordered containers, so no explicit call is required. Here is an example of an ordered set:

```ada
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Ada.Containers.Ordered_Sets;

procedure Sort_Composites is

   function "+" (S : String) return Unbounded_String renames To_Unbounded_String;

   type A_Composite is
      record
         Name  : Unbounded_String;
         Value : Unbounded_String;
      end record;

   function "<" (L, R : A_Composite) return Boolean is
   begin
      return L.Name < R.Name;
   end "<";

   procedure Put_Line (C : A_Composite) is
   begin
      Put_Line (To_String (C.Name) & " " & To_String (C.Value));
   end Put_Line;

   package Composite_Sets is new Ada.Containers.Ordered_Sets (A_Composite);

   procedure Put_Line (C : Composite_Sets.Cursor) is
   begin
      Put_Line (Composite_Sets.Element (C));
   end Put_Line;

   Data : Composite_Sets.Set;

begin
   Data.Insert (New_Item => (Name => +"Joe",    Value => +"5531"));
   Data.Insert (New_Item => (Name => +"Adam",   Value => +"2341"));
   Data.Insert (New_Item => (Name => +"Bernie", Value => +"122"));
   Data.Insert (New_Item => (Name => +"Walter", Value => +"1234"));
   Data.Insert (New_Item => (Name => +"David",  Value => +"19"));
   Data.Iterate (Put_Line'Access);
end Sort_Composites;
```

Result:

```txt

  C:\Ada\sort_composites\lib\sort_composites
  Adam 2341
  Bernie 122
  David 19
  Joe 5531
  Walter 1234

```

There is no standard sort function for [[Ada 95]]. The example below implements a simple bubble sort.

```ada
with Ada.Text_Io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Sort_Composite is
   type Composite_Record is record
      Name : Unbounded_String;
      Value : Unbounded_String;
   end record;

   type Pairs_Array is array(Positive range <>) of Composite_Record;

   procedure Swap(Left, Right : in out Composite_Record) is
      Temp : Composite_Record := Left;
   begin
      Left := Right;
      Right := Temp;
   end Swap;

   -- Sort_Names uses a bubble sort

   procedure Sort_Name(Pairs : in out Pairs_Array) is
      Swap_Performed : Boolean := True;
   begin
      while Swap_Performed loop
         Swap_Performed := False;
         for I in Pairs'First..(Pairs'Last - 1) loop
            if Pairs(I).Name > Pairs(I + 1).Name then
               Swap (Pairs(I), Pairs(I + 1));
               Swap_Performed := True;
            end if;
         end loop;
      end loop;
   end Sort_Name;

   procedure Print(Item : Pairs_Array) is
   begin
      for I in Item'range loop
         Ada.Text_Io.Put_Line(To_String(Item(I).Name) & ", " &
            to_String(Item(I).Value));
      end loop;
   end Print;
   type Names is (Fred, Barney, Wilma, Betty, Pebbles);
   type Values is (Home, Work, Cook, Eat, Bowl);
   My_Pairs : Pairs_Array(1..5);
begin
   for I in My_Pairs'range loop
      My_Pairs(I).Name := To_Unbounded_String(Names'Image(Names'Val(Integer(I - 1))));
      My_Pairs(I).Value := To_Unbounded_String(Values'Image(Values'Val(Integer(I - 1))));
   end loop;
   Print(My_Pairs);
   Ada.Text_Io.Put_Line("
### ===================
");
   Sort_Name(My_Pairs);
   Print(My_Pairs);
end Sort_Composite;
```


## ALGOL 68

<!-- {{does not work with|ELLA ALGOL 68|Any (with appropriate job cards AND formatted transput statements removed) - tested with release 1.8.8d.fc9.i386 - ELLA has no FORMATted transput}} -->

```algol68
MODE SORTSTRUCT = PERSON;
OP < = (PERSON a,b)BOOL: age OF a < age OF b;
PR READ "prelude/sort.a68" PR;

MODE PERSON = STRUCT (STRING name, INT age);
FORMAT person repr = $"Name: "g", Age: "g(0)l$;

[]SORTSTRUCT person = (("joe", 120), ("foo", 31), ("bar", 51));
printf((person repr, shell sort(person), $l$))
```

Output:

```txt

Name: foo, Age: 31
Name: bar, Age: 51
Name: joe, Age: 120

```



## AppleScript

macOS Yosemite onwards, for import of Foundation framework


```AppleScript
use framework "Foundation"

-- SORTING COMPOSITE STRUCTURES (BY PRIMARY AND N-ARY KEYS)

-- List of {strKey, blnAscending} pairs -> list of records -> sorted list of records
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
set xs to [¬
    {city:"Shanghai ", pop:24.2}, ¬
    {city:"Karachi ", pop:23.5}, ¬
    {city:"Beijing ", pop:21.5}, ¬
    {city:"Sao Paulo ", pop:24.2}, ¬
    {city:"Dhaka ", pop:17.0}, ¬
    {city:"Delhi ", pop:16.8}, ¬
    {city:"Lagos ", pop:16.1}]

-- Boolean true for ascending order, false for descending:

sortByComparing([{"pop", false}, {"city", true}], xs)
```

```txt
```



## AutoHotkey

built ListView Gui, contains a table sorting function which can be used for this.

```AutoHotkey
start:
Gui, Add, ListView, r20 w200, 1|2
data =
(
foo,53
joe,34
bar,23
)

Loop, parse, data, `n
{
  stringsplit, row, A_LoopField, `,
  LV_Add(row, row1, row2)
}
LV_ModifyCol()  ; Auto-size columns
Gui, Show
msgbox, sorting by column1
LV_ModifyCol(1, "sort") ; sort by first column
msgbox, sorting by column2
LV_ModifyCol(2, "sort Integer") ; sort by second column numerically
return

GuiClose:
ExitApp
```


## AWK


```AWK

# syntax: GAWK -f SORT_AN_ARRAY_OF_COMPOSITE_STRUCTURES.AWK
BEGIN {
# AWK lacks structures but one can be simulated using an associative array.
    arr["eight  8 "]
    arr["two    2 "]
    arr["five   5 "]
    arr["nine   9 "]
    arr["one    1 "]
    arr["three  3 "]
    arr["six    6 "]
    arr["seven  7 "]
    arr["four   4 "]
    arr["ten    10"]
    arr["zero   0 "]
    arr["twelve 12"]
    arr["minus2 -2"]
    show(1,7,"@val_str_asc","name") # use name part of name-value pair
    show(8,9,"@val_num_asc","value") # use value part of name-value pair
    exit(0)
}
function show(a,b,sequence,description,  i,x) {
    PROCINFO["sorted_in"] = "@unsorted"
    for (i in arr) {
      x = substr(i,a,b)
      sub(/ +/,"",x)
      arr[i] = x
    }
    PROCINFO["sorted_in"] = sequence
    printf("sorted by %s:",description)
    for (i in arr) {
      printf(" %s",arr[i])
    }
    printf("\n")
}

```

```txt

sorted by name: eight five four minus2 nine one seven six ten three twelve two zero
sorted by value: -2 0 1 2 3 4 5 6 7 8 9 10 12

```



## Babel


First, we construct a list-of-maps and assign it to variable baz. Next, we sort baz by key "foo" and assign it to variable bop. Finally, we lookup "foo" in each map in list bop and display the resulting list of numbers - they are in sorted order.


```babel
babel>
 baz ([map "foo" 3 "bar" 17] [map "foo" 4 "bar" 18] [map "foo" 5 "bar" 19] [map "foo" 0 "bar" 20]) <
babel> bop baz { <- "foo" lumap ! -> "foo" lumap ! lt? } lssort ! <
babel> bop {"foo" lumap !} over ! lsnum !
( 0 3 4 5 )
```


The same technique works for any list of data-objects you may have. User-code can expect to have the top two elements of the stack set to be the two objects to be compared. Simply access the relevant field in each object, and then perform a comparison. For example, here is a list of pairs sorted by first element:


```babel
babel>
 20 lsrange ! {1 randlf 2 rem} lssort ! 2 group ! --> this creates a shuffled list of pairs
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


The gpsort utility performs this kind of comparison "automagically" by leveraging the ordering of Babel's underlying data-structure. Using the shuffled list from the example above:


```babel
babel>
 gpsort !
babel> dup {lsnum !} ...
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


Note that gpsort will not work for the case where you want to sort on the second element of a list of pairs. But it will work for performing a canonical sort on numbers, arrays of numbers, lists of numbers, lists of lists, lists of arrays, arrays of lists, and so on. You should not use gpsort with strings; use lexsort or strsort instead. Here's an example of sorting a mixture of pairs and triples using gpsort:


```babel
babel>
 dup {lsnum !} ... --> display the shuffled list of pairs and triples
( 7 2 )
( 6 4 )
( 8 9 )
( 0 5 )
( 5 14 0 )
( 3 1 )
( 9 6 10 )
( 1 12 4 )
( 11 13 7 )
( 8 2 3 )
babel> gpsort ! --> sort the list
babel> dup {lsnum !} ... --> display the result
( 0 5 )
( 3 1 )
( 6 4 )
( 7 2 )
( 8 9 )
( 1 12 4 )
( 5 14 0 )
( 8 2 3 )
( 9 6 10 )
( 11 13 7 )
```



## BBC BASIC

Uses the supplied SORTSALIB library.

```bbcbasic
      INSTALL @lib$+"SORTSALIB"
      sort% = FN_sortSAinit(0,0)

      DIM pair{name$, number%}
      DIM array{(10)} = pair{}
      FOR i% = 1 TO DIM(array{()}, 1)
        READ array{(i%)}.name$, array{(i%)}.number%
      NEXT

      DATA "Eight", 8, "Two", 2, "Five", 5, "Nine", 9, "One", 1
      DATA "Three", 3, "Six", 6, "Seven", 7, "Four", 4, "Ten", 10

      C% = DIM(array{()}, 1)
      D% = 1
      CALL sort%, array{()}, array{(0)}.number%, array{(0)}.name$

      FOR i% = 1 TO DIM(array{()}, 1)
        PRINT array{(i%)}.name$, array{(i%)}.number%
      NEXT
```

Output:

```txt

One                1
Two                2
Three              3
Four               4
Five               5
Six                6
Seven              7
Eight              8
Nine               9
Ten               10

```



## Bracmat

The easiest way to sort an array of elements in Bracmat is to handle it as a sum of terms. A sum, when evaluated, is automatically sorted.

```bracmat
( (tab=("cpp",1979)+(Ada,1983)+(Ruby,1995)+(Eiffel,1985))
& out$"unsorted array:"
& lst$tab
& out$("sorted array:" !tab \n)
& out$"But tab is still unsorted:"
& lst$tab
);
```


Output:

```txt
unsorted array:
(tab=
("cpp",1979)+(Ada,1983)+(Ruby,1995)+(Eiffel,1985)
);
  sorted array:
  (Ada,1983)+(C++,1979)+(Eiffel,1985)+(Ruby,1995)


But tab is still unsorted:
(tab=
("cpp",1979)+(Ada,1983)+(Ruby,1995)+(Eiffel,1985)
);
```


When evaluating <code>!tab</code>, the expression bound to the variable name <code>tab</code> is sorted, but the unevaluated expression is still bound to <code>tab</code>. An assignment binds the sorted expression to <code>tab</code>:

```bracmat
( !tab:?tab
& out$"Now tab is sorted:"
& lst$tab
);
```


Output:

```txt
Now tab is sorted:
(tab=
(Ada,1983)+("cpp",1979)+(Eiffel,1985)+(Ruby,1995)
);
```


To sort an array that is not a sum expression, we can convert it to a sum:

```bracmat
(     ((name.map),(weight.9),(value.150))
      ((name.compass),(weight.13),(value.35))
      ((name.water),(weight.153),(value.200))
      ((name.sandwich),(weight.50),(value.60))
      ((name.glucose),(weight.15),(value.60))
  : ?array
& ( reverse
  =   e A
    .   :?A
      & whl'(!arg:%?e ?arg&!e !A:?A)
      & !A
  )
& out$("Array before sorting:" !array \n)
& 0:?sum
&   whl
  ' (!array:%?element ?array&!element+!sum:?sum)
&   whl
  ' (!sum:%?element+?sum&!element !array:?array)
& out$("Array after sorting (descending order):" !array \n)
& out$("Array after sorting (ascending order):" reverse$!array \n)
);
```


Output:

```txt
  Array before sorting:
  ((name.map),(weight.9),(value.150))
  ((name.compass),(weight.13),(value.35))
  ((name.water),(weight.153),(value.200))
  ((name.sandwich),(weight.50),(value.60))
  ((name.glucose),(weight.15),(value.60))


  Array after sorting (descending order):
  ((name.water),(weight.153),(value.200))
  ((name.sandwich),(weight.50),(value.60))
  ((name.map),(weight.9),(value.150))
  ((name.glucose),(weight.15),(value.60))
  ((name.compass),(weight.13),(value.35))


  Array after sorting (ascending order):
  ((name.compass),(weight.13),(value.35))
  ((name.glucose),(weight.15),(value.60))
  ((name.map),(weight.9),(value.150))
  ((name.sandwich),(weight.50),(value.60))
  ((name.water),(weight.153),(value.200))
```


Bracmat has a left to right sorting order. If an array must be sorted on another field than the first field, that other field has to be made the first field. After sorting, the fields can take their original positions.

```bracmat
(     (Joe,5531)
      (Adam,2341)
      (Bernie,122)
      (Walter,1234)
      (David,19)
  : ?array
& 0:?sum
&   whl
  ' ( !array:(?car,?cdr) ?array
    & (!cdr.!car)+!sum:?sum
    )
&   whl
  ' ( !sum:(?car.?cdr)+?sum
    & (!cdr,!car) !array:?array
    )
& out$("Array after sorting on second field (descending order):" !array \n)
&   out
  $ ( "Array after sorting on second field (ascending order):"
      reverse$!array
      \n
    )
);
```


Output:

```txt
  Array after sorting on second field (descending order):
  (Joe,5531)
  (Adam,2341)
  (Walter,1234)
  (Bernie,122)
  (David,19)


  Array after sorting on second field (ascending order):
  (David,19)
  (Bernie,122)
  (Walter,1234)
  (Adam,2341)
  (Joe,5531)
```



## C

Using qsort, from the standard library.

```c

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

typedef struct twoStringsStruct {
    char * key, *value;
} sTwoStrings;

int ord( char v )
{
    static char *dgts = "012345679";
    char *cp;
    for (cp=dgts; v != *cp; cp++);
    return (cp-dgts);
}

int cmprStrgs(const sTwoStrings *s1,const sTwoStrings *s2)
{
    char *p1 = s1->key;
    char *p2 = s2->key;
    char *mrk1, *mrk2;
    while ((tolower(*p1) == tolower(*p2)) && *p1) { p1++; p2++;}
    if (isdigit(*p1) && isdigit(*p2)) {
        long v1, v2;
        if ((*p1 == '0') ||(*p2 == '0')) {
            while (p1 > s1->key) {
                p1--; p2--;
                if (*p1 != '0') break;
            }
            if (!isdigit(*p1)) {
                p1++; p2++;
            }
        }
        mrk1 = p1; mrk2 = p2;
        v1 = 0;
        while(isdigit(*p1)) {
            v1 = 10*v1+ord(*p1);
            p1++;
        }
        v2 = 0;
        while(isdigit(*p2)) {
            v2 = 10*v2+ord(*p2);
            p2++;
        }
        if (v1 == v2)
           return(p2-mrk2)-(p1-mrk1);
        return v1 - v2;
    }
    if (tolower(*p1) != tolower(*p2))
       return (tolower(*p1) - tolower(*p2));
    for(p1=s1->key, p2=s2->key; (*p1 == *p2) && *p1; p1++, p2++);
    return (*p1 -*p2);
}

int maxstrlen( char *a, char *b)
{
	int la = strlen(a);
	int lb = strlen(b);
	return (la>lb)? la : lb;
}

int main()
{
    sTwoStrings toBsorted[] = {
        { "Beta11a", "many" },
        { "alpha1", "This" },
        { "Betamax", "sorted." },
        { "beta3", "order" },
        { "beta11a", "strings" },
        { "beta001", "is" },
        { "beta11", "which" },
        { "beta041", "be" },
        { "beta05", "in" },
        { "beta1", "the" },
        { "beta40", "should" },
    };
#define ASIZE (sizeof(toBsorted)/sizeof(sTwoStrings))
    int k, maxlens[ASIZE];
    char format[12];
    sTwoStrings *cp;

    qsort( (void*)toBsorted, ASIZE, sizeof(sTwoStrings),cmprStrgs);

    for (k=0,cp=toBsorted; k < ASIZE; k++,cp++) {
        maxlens[k] = maxstrlen(cp->key, cp->value);
        sprintf(format," %%-%ds", maxlens[k]);
        printf(format, toBsorted[k].value);
	}
    printf("\n");
    for (k=0; k < ASIZE; k++) {
        sprintf(format," %%-%ds", maxlens[k]);
        printf(format, toBsorted[k].key);
	}
    printf("\n");

  return 0;
}
```

Output:

```txt
   This      is   the order     in  which    many strings should      be sorted.
 alpha1 beta001 beta1 beta3 beta05 beta11 Beta11a beta11a beta40 beta041 Betamax
```



## C++

Uses C++11. Compile with
 g++ -std=c++11 sort.cpp

```cpp
#include <algorithm>
#include <iostream>
#include <string>

struct entry {
  std::string name;
  std::string value;
};

int main() {
  entry array[] = { { "grass", "green" }, { "snow", "white" },
                    { "sky", "blue" }, { "cherry", "red" } };

  std::cout << "Before sorting:\n";
  for (const auto &e : array) {
    std::cout << "{" << e.name << ", " << e.value << "}\n";
  }

  std::sort(std::begin(array), std::end(array),
            [](const entry & a, const entry & b) {
    return a.name < b.name;
  });

  std::cout << "After sorting:\n";
  for (const auto &e : array) {
    std::cout << "{" << e.name << ", " << e.value << "}\n";
  }
}
```

Output:

```txt

Before sorting:
{grass, green}
{snow, white}
{sky, blue}
{cherry, red}
After sorting:
{cherry, red}
{grass, green}
{sky, blue}
{snow, white}

```


## C#
```c#
using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    struct Entry
    {
        public Entry(string name, double value) { Name = name; Value = value; }
        public string Name;
        public double Value;
    }

    static void Main(string[] args)
    {
        var Elements = new List<Entry>
        {
            new Entry("Krypton", 83.798), new Entry("Beryllium", 9.012182), new Entry("Silicon", 28.0855),
            new Entry("Cobalt", 58.933195), new Entry("Selenium", 78.96), new Entry("Germanium", 72.64)
        };

        var sortedElements = Elements.OrderBy(e => e.Name);

        foreach (Entry e in sortedElements)
            Console.WriteLine("{0,-11}{1}", e.Name, e.Value);
    }
}
```


Output:


```txt
Beryllium  9.012182
Cobalt     58.933195
Germanium  72.64
Krypton    83.798
Selenium   78.96
Silicon    28.0855
```



## Clojure

Clojure has a ''sort-by'' function which takes a ''keyfn'' and a ''coll''. It returns a sorted sequence of the items in ''coll'', where the sort order is determined by comparing ''(keyfn item)''.


```clojure

;; Gathered with Google Squared
(def *langs* [["Clojure" 2007] ["Common Lisp" 1984] ["Java" 1995] ["Haskell" 1990]
              ["Lisp" 1958] ["Scheme" 1975]])

user> (sort-by second *langs*) ; using a keyfn

(["Lisp" 1958] ["Scheme" 1975] ["Common Lisp" 1984] ["Haskell" 1990] ["Java" 1995] ["Clojure" 2007])

```


You can also supply a comparator (using ''compare'' or a sibling of ''<''). A comparator can be used with the regular ''sort'' function or the ''sort-by'' function. In the latter case, the comparator will be used on ''(keyfn item)'' instead of ''item''.


```clojure

user> (sort #(compare (second %1) (second %2)) *langs*) ; using a comparator

(["Lisp" 1958] ["Scheme" 1975] ["Common Lisp" 1984] ["Haskell" 1990] ["Java" 1995] ["Clojure" 2007])

user> (sort-by second > *langs*) ; using a keyfn and a comparator

(["Clojure" 2007] ["Java" 1995] ["Haskell" 1990] ["Common Lisp" 1984] ["Scheme" 1975] ["Lisp" 1958])

```


Read the docstring of ''sort'' and ''sort-by'' for more info.


## Common Lisp

In Common Lisp, the ''sort'' function takes a predicate that is used as the comparator.  This parameter can be any two-argument function.  Additionally, the ''sort'' function can take a keyword argument '':key'' whose result is passed to the predicate.

Let's define a composite structure of U.S. states and average test scores.


```lisp
CL-USER> (defparameter *test-scores* '(("texas" 68.9) ("ohio" 87.8) ("california" 76.2) ("new york" 88.2)) )
*TEST-SCORES*
```


We can sort by the state name by supplying a one-argument key function that is called by the ''sort'' function to determine the value to compare. In this case, the function is ''first'' will retrieve the state name:


```lisp
CL-USER> (sort (copy-list *test-scores*) #'string-lessp :key #'first)
(("california" 76.2) ("new york" 88.2) ("ohio" 87.8) ("texas" 68.9))
```


we can also sort by the test scores by supplying a different key function that return the test score instead:


```lisp
CL-USER> (sort (copy-list *test-scores*) #'< :key #'second)
(("texas" 68.9) ("california" 76.2) ("ohio" 87.8) ("new york" 88.2))
```



## D


```d
import std.stdio, std.algorithm;

struct Pair { string name, value; }

void main() {
    Pair[] pairs = [{"Joe",    "5531"},
                    {"Adam",   "2341"},
                    {"Bernie",  "122"},
                    {"Walter", "1234"},
                    {"David",    "19"}];

    pairs.schwartzSort!q{ a.name }.writeln;
}
```

```txt
[Pair("Adam", "2341"), Pair("Bernie", "122"), Pair("David", "19"), Pair("Joe", "5531"), Pair("Walter", "1234")]
```



## Delphi


```Delphi
program SortCompositeStructures;

{$APPTYPE CONSOLE}

uses SysUtils, Generics.Collections, Generics.Defaults;

type
  TStructurePair = record
    name: string;
    value: string;
    constructor Create(const aName, aValue: string);
  end;

constructor TStructurePair.Create(const aName, aValue: string);
begin
  name := aName;
  value := aValue;
end;

var
  lArray: array of TStructurePair;
begin
  SetLength(lArray, 3);
  lArray[0] := TStructurePair.Create('dog', 'rex');
  lArray[1] := TStructurePair.Create('cat', 'simba');
  lArray[2] := TStructurePair.Create('horse', 'trigger');

  TArray.Sort<TStructurePair>(lArray , TDelegatedComparer<TStructurePair>.Construct(
  function(const Left, Right: TStructurePair): Integer
  begin
    Result := CompareText(Left.Name, Right.Name);
  end));
end.
```



## E



```e
def compareBy(keyfn) { # This ought to be in the standard library
  return def comparer(a, b) {
    return keyfn(a).op__cmp(keyfn(b))
  }
}

def x := [
  ["Joe",3],
  ["Bill",4],
  ["Alice",20],
  ["Harry",3],
]

println(x.sort(compareBy(fn [name,_] { name })))
```



## EchoLisp


```scheme

;; sorting (name value) by name - Ignoring case
(define (name a) (first a))
(define( sort-proc a b)
    (string-ci<? (name a) (name b)))

(define people
   '(("😎" -42) ("albert" 33) ("Simone" 44) ("Antoinette" 42) ("elvis" 666) ("😃" 1000)))

(list-sort sort-proc people)
   → (("albert" 33) ("Antoinette" 42) ("elvis" 666) ("Simone" 44) ("😃" 1000) ("😎" -42))


```


## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;

public program()
{
    var elements := new::(
            KeyValue.new("Krypton", 83.798r),
            KeyValue.new("Beryllium", 9.012182r),
            KeyValue.new("Silicon", 28.0855r),
            KeyValue.new("Cobalt", 58.933195r),
            KeyValue.new("Selenium", 78.96r),
            KeyValue.new("Germanium", 72.64r));

    var sorted := elements.sort:(former,later => former.Key < later.Key );

    sorted.forEach:(element)
    {
         console.printLine(element.Key," - ",element)
    }
}
```



## Elixir


```elixir
defmodule Person do
  defstruct name: "", value: 0
end

list = [struct(Person, [name: "Joe", value: 3]),
        struct(Person, [name: "Bill", value: 4]),
        struct(Person, [name: "Alice", value: 20]),
        struct(Person, [name: "Harry", value: 3])]

Enum.sort(list) |> Enum.each(fn x -> IO.inspect x end)
IO.puts ""
Enum.sort_by(list, &(&1.value)) |> Enum.each(&IO.inspect &1)
```


```txt

%Person{name: "Alice", value: 20}
%Person{name: "Bill", value: 4}
%Person{name: "Harry", value: 3}
%Person{name: "Joe", value: 3}

%Person{name: "Joe", value: 3}
%Person{name: "Harry", value: 3}
%Person{name: "Bill", value: 4}
%Person{name: "Alice", value: 20}

```



## Erlang

Any Erlang type can be compared to any Erlang type. As such, nothing special needs to be done:

```Erlang
1>
 lists:sort([{{2006,2007},"Ducks"},
               {{2000,2001},"Avalanche"},
               {{2002,2003},"Devils"},
               {{2001,2002},"Red Wings"},
               {{2003,2004},"Lightning"},
               {{2004,2005},"N/A: lockout"},
               {{2005,2006},"Hurricanes"},
               {{1999,2000},"Devils"},
               {{2007,2008},"Red Wings"},
               {{2008,2009},"Penguins"}]).
[{{1999,2000},"Devils"},
 {{2000,2001},"Avalanche"},
 {{2001,2002},"Red Wings"},
 {{2002,2003},"Devils"},
 {{2003,2004},"Lightning"},
 {{2004,2005},"N/A: lockout"},
 {{2005,2006},"Hurricanes"},
 {{2006,2007},"Ducks"},
 {{2007,2008},"Red Wings"},
 {{2008,2009},"Penguins"}]
```


It is also possible to sort with custom functions, in this case by the team's name:

```Erlang
2>
 F = fun({_,X},{_,Y}) -> X < Y end.
#Fun<erl_eval.12.113037538>
3> lists:usort(F, [{{2006,2007},"Ducks"},
                   {{2000,2001},"Avalanche"},
                   {{2002,2003},"Devils"},
                   {{2001,2002},"Red Wings"},
                   {{2003,2004},"Lightning"},
                   {{2004,2005},"N/A: lockout"},
                   {{2005,2006},"Hurricanes"},
                   {{1999,2000},"Devils"},
                   {{2007,2008},"Red Wings"},
                   {{2008,2009},"Penguins"}]).
[{{2000,2001},"Avalanche"},
 {{1999,2000},"Devils"},
 {{2002,2003},"Devils"},
 {{2006,2007},"Ducks"},
 {{2005,2006},"Hurricanes"},
 {{2003,2004},"Lightning"},
 {{2004,2005},"N/A: lockout"},
 {{2008,2009},"Penguins"},
 {{2007,2008},"Red Wings"},
 {{2001,2002},"Red Wings"}]
```



## Euphoria


```euphoria
include sort.e
include misc.e

constant NAME = 1
function compare_names(sequence a, sequence b)
    return compare(a[NAME],b[NAME])
end function

sequence s
s = { { "grass",  "green" },
      { "snow",   "white" },
      { "sky",    "blue"  },
      { "cherry", "red"   } }

pretty_print(1,custom_sort(routine_id("compare_names"),s),{2})
```


Output:

```txt
{
  {
    "cherry",
    "red"
  },
  {
    "grass",
    "green"
  },
  {
    "sky",
    "blue"
  },
  {
    "snow",
    "white"
  }
}
```



## Factor

This is essentially the same as [[Sorting Using a Custom Comparator]].


```factor
TUPLE: example-pair name value ;

: sort-by-name ( seq -- seq' ) [ [ name>> ] compare ] sort ;
```


 ( scratchpad ) { T{ example-pair f "omega" "a" } T{ example-pair f "gamma" "q" } T{ example-pair f "alpha" "z" } } sort-by-name .
 {
     T{ example-pair { name "alpha" } { value "z" } }
     T{ example-pair { name "gamma" } { value "q" } }
     T{ example-pair { name "omega" } { value "a" } }
 }


## Fantom


Any object can be sorted as needed by passing an appropriate block to the 'sort' method.


```fantom

class Pair // create a composite structure
{
  Str name
  Str value
  new make (Str name, Str value)
  {
    this.name = name
    this.value = value
  }

  override Str toStr ()
  {
    "(Pair: $name, $value)"
  }
}

class Main
{
  public static Void main ()
  {
    // samples
    pairs := [Pair("Fantom", "OO"), Pair("Clojure", "Functional"), Pair("Java", "OO") ]

    sorted := pairs.dup // make a copy of original list
    sorted.sort |Pair a, Pair b -> Int|  // sort using custom comparator
    {
      a.name <=> b.name
    }
    echo ("Started with : " + pairs.join(" "))
    echo ("Finished with: " + sorted.join(" "))
  }
}

```



## Fortran

Standard Fortran has no built-in sort function although some compilers add them. The following example uses an insertion sort.

```fortran
PROGRAM EXAMPLE
  IMPLICIT NONE

  TYPE Pair
    CHARACTER(6) :: name
    CHARACTER(1) :: value
  END TYPE Pair

  TYPE(Pair) :: rcc(10), temp
  INTEGER :: i, j

  rcc(1) = Pair("Black", "0")
  rcc(2) = Pair("Brown", "1")
  rcc(3) = Pair("Red", "2")
  rcc(4) = Pair("Orange", "3")
  rcc(5) = Pair("Yellow", "4")
  rcc(6) = Pair("Green", "5")
  rcc(7) = Pair("Blue", "6")
  rcc(8) = Pair("Violet", "7")
  rcc(9) = Pair("Grey", "8")
  rcc(10) = Pair("White", "9")

  DO i = 2, SIZE(rcc)
     j = i - 1
     temp = rcc(i)
        DO WHILE (j>=1 .AND. LGT(rcc(j)%name, temp%name))
           rcc(j+1) = rcc(j)
           j = j - 1
        END DO
     rcc(j+1) = temp
  END DO

  WRITE (*,"(2A6)") rcc

END PROGRAM EXAMPLE
```

Output
 Black      0
 Blue       6
 Brown      1
 Green      5
 Grey       8
 Orange     3
 Red        2
 Violet     7
 White      9
 Yellow     4


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Type Pair
  As String name, value
  Declare Constructor(name_ As String, value_ As String)
  Declare Operator Cast() As String
End Type

Constructor Pair(name_ As String, value_ As String)
  name  = name_
  value = value_
End Constructor

Operator Pair.Cast() As String
  Return "[" + name + ", " + value + "]"
End Operator

' selection sort, quick enough for sorting small number of pairs
Sub sortPairsByName(p() As Pair)
  Dim As Integer i, j, m
  For i = LBound(p) To UBound(p) - 1
    m = i
    For j = i + 1 To UBound(p)
      If p(j).name < p(m).name Then m = j
    Next j
    If m <> i Then Swap p(i), p(m)
  Next i
End Sub

Dim As Pair pairs(1 To 4) = _
{ _
  Pair("grass", "green"), _
  Pair("snow", "white" ), _
  Pair("sky", "blue"),    _
  Pair("cherry", "red")   _
}

Print "Before sorting :"
For i As Integer = 1 To 4
  Print Tab(3); pairs(i)
Next

sortPairsByName pairs()

Print
Print "After sorting by name :"
For i As Integer = 1 To 4
  Print Tab(3); pairs(i)
Next

Print
Print "Press any key to quit"
Sleep
```


```txt

Before sorting :
  [grass, green]
  [snow, white]
  [sky, blue]
  [cherry, red]

After sorting by name :
  [cherry, red]
  [grass, green]
  [sky, blue]
  [snow, white]

```


=={{header|F_Sharp|F#}}==
F# has <code>sortBy</code> functions that work on collection types for this purpose. An example using an array of pairs:

```fsharp
let persons = [| ("Joe", 120); ("foo", 31); ("bar", 51) |]
Array.sortInPlaceBy fst persons
printfn "%A" persons
```


Output:

```txt
[|("Joe", 120); ("bar", 51); ("foo", 31)|]
```


An example using a list of records:

```fsharp
type Person = { name:string; id:int }
let persons2 = [{name="Joe"; id=120}; {name="foo"; id=31}; {name="bar"; id=51}]
let sorted = List.sortBy (fun p -> p.id) persons2
for p in sorted do printfn "%A" p
```


Output:

```txt
{name = "foo";
 id = 31;}
{name = "bar";
 id = 51;}
{name = "Joe";
 id = 120;}
```



## Go


```go
package main

import (
    "fmt"
    "sort"
)

type pair struct {
    name, value string
}
type csArray []pair

// three methods satisfy sort.Interface
func (a csArray) Less(i, j int) bool { return a[i].name < a[j].name }
func (a csArray) Len() int           { return len(a) }
func (a csArray) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }

var x = csArray{
    pair{"joe", "120"},
    pair{"foo", "31"},
    pair{"bar", "251"},
}

func main() {
    sort.Sort(x)
    for _, p := range x {
        fmt.Printf("%5s: %s\n", p.name, p.value)
    }
}
```



## Groovy


```groovy
class Holiday {
    def date
    def name
    Holiday(dateStr, name) { this.name = name; this.date = format.parse(dateStr) }
    String toString() { "${format.format date}: ${name}" }
    static format = new java.text.SimpleDateFormat("yyyy-MM-dd")
}

def holidays = [ new Holiday("2009-12-25", "Christmas Day"),
                 new Holiday("2009-04-22", "Earth Day"),
                 new Holiday("2009-09-07", "Labor Day"),
                 new Holiday("2009-07-04", "Independence Day"),
                 new Holiday("2009-10-31", "Halloween"),
                 new Holiday("2009-05-25", "Memorial Day"),
                 new Holiday("2009-03-14", "PI Day"),
                 new Holiday("2009-01-01", "New Year's Day"),
                 new Holiday("2009-12-31", "New Year's Eve"),
                 new Holiday("2009-11-26", "Thanksgiving"),
                 new Holiday("2009-02-14", "St. Valentine's Day"),
                 new Holiday("2009-03-17", "St. Patrick's Day"),
                 new Holiday("2009-01-19", "Martin Luther King Day"),
                 new Holiday("2009-02-16", "President's Day") ]

holidays.sort { x, y -> x.date <=> y.date }
holidays.each { println it }
```


Output:

```txt
2009-01-01: New Year's Day
2009-01-19: Martin Luther King Day
2009-02-14: St. Valentine's Day
2009-02-16: President's Day
2009-03-14: PI Day
2009-03-17: St. Patrick's Day
2009-04-22: Earth Day
2009-05-25: Memorial Day
2009-07-04: Independence Day
2009-09-07: Labor Day
2009-10-31: Halloween
2009-11-26: Thanksgiving
2009-12-25: Christmas Day
2009-12-31: New Year's Eve
```



## Haskell


```haskell
import Data.List
import Data.Function (on)

data Person =
  P String
    Int
  deriving (Eq)

instance Show Person where
  show (P name val) = "Person " ++ name ++ " with value " ++ show val

instance Ord Person where
  compare (P a _) (P b _) = compare a b

pVal :: Person -> Int
pVal (P _ x) = x

people :: [Person]
people = [P "Joe" 12, P "Bob" 8, P "Alice" 9, P "Harry" 2]


main :: IO ()
main = do
  mapM_ print $ sort people
  putStrLn []
  mapM_ print $ sortBy (on compare pVal) people
```


```txt
Person Alice with value 9
Person Bob with value 8
Person Harry with value 2
Person Joe with value 12

Person Harry with value 2
Person Bob with value 8
Person Alice with value 9
Person Joe with value 12
```


More generally, '''sortBy''' takes any (a -> a -> Ordering) function as its first argument. A function of this kind can be derived from a simpler (b -> a) function using the higher order '''comparing''' function.

To sort a list of triples by the third element, for example:


```haskell
import Data.Ord (comparing)
import Data.List (sortBy)

xs :: [(String, String, Int)]
xs =
  zip3
    (words "Richard John Marvin Alan Maurice James")
    (words "Hamming McCarthy Minskey Perlis Wilkes Wilkinson")
    [1915, 1927, 1926, 1922, 1913, 1919]

main :: IO ()
main = mapM_ print $ sortBy (comparing (\(_, _, y) -> y)) xs
```

```txt
("Maurice","Wilkes",1913)
("Richard","Hamming",1915)
("James","Wilkinson",1919)
("Alan","Perlis",1922)
("Marvin","Minskey",1926)
("John","McCarthy",1927)
```


=={{header|Icon}} and {{header|Unicon}}==
The built-in procedure sortf will sort a list by the field in a records.

```Icon
record star(name,HIP)

procedure main()

Ori := [ star("Betelgeuse",27989),
         star("Rigel",24436),
         star("Belatrix", 25336),
         star("Alnilam",26311) ]

write("Some Orion stars by HIP#")
every write( (x := !sortf(Ori,2)).name, " HIP ",x.HIP)
end
```


Sample output:
```txt
Some Orion stars by HIP#
Rigel HIP 24436
Belatrix HIP 25336
Alnilam HIP 26311
Betelgeuse HIP 27989
```



## J

The function<tt> /: </tt>sorts anything (its left argument) based on the keys supplied in its right argument.  For example:


```j
   names =: ;: 'Perlis Wilkes Hamming Minsky Wilkinson McCarthy'
   values=: ;: 'Alan Maurice Richard Marvin James John'
   pairs =: values ,. names
   pairs /: names
+-------+---------+
|Richard|Hamming  |
+-------+---------+
|John   |McCarthy |
+-------+---------+
|Marvin |Minsky   |
+-------+---------+
|Alan   |Perlis   |
+-------+---------+
|Maurice|Wilkes   |
+-------+---------+
|James  |Wilkinson|
+-------+---------+
```


Alternatively, J's cross operator will use the same values for both the left and right arguments for /: but, in this case, /:~ is not desirable because that would have us sorting on the values (the first column) and only using the second column for equal names (none of which appear, here).


## Java


```java
import java.util.Arrays;
import java.util.Comparator;

public class SortComp {
    public static class Pair {
        public String name;
        public String value;
        public Pair(String n, String v) {
            name = n;
            value = v;
        }
    }

    public static void main(String[] args) {
        Pair[] pairs = {new Pair("06-07", "Ducks"), new Pair("00-01", "Avalanche"),
            new Pair("02-03", "Devils"), new Pair("01-02", "Red Wings"),
            new Pair("03-04", "Lightning"), new Pair("04-05", "lockout"),
            new Pair("05-06", "Hurricanes"), new Pair("99-00", "Devils"),
            new Pair("07-08", "Red Wings"), new Pair("08-09", "Penguins")};

        sortByName(pairs);
        for (Pair p : pairs) {
            System.out.println(p.name + " " + p.value);
        }
    }

    public static void sortByName(Pair[] pairs) {
        Arrays.sort(pairs, new Comparator<Pair>() {
            public int compare(Pair p1, Pair p2) {
                return p1.name.compareTo(p2.name);
            }
        });
    }
}
```

Output:

```txt
00-01 Avalanche
01-02 Red Wings
02-03 Devils
03-04 Lightning
04-05 lockout
05-06 Hurricanes
06-07 Ducks
07-08 Red Wings
08-09 Penguins
99-00 Devils
```


In Java 8, we can write the above using a lambda:
```java
    public static void sortByName(Pair[] pairs) {
        Arrays.sort(pairs, (p1, p2) -> p1.name.compareTo(p2.name));
    }
```


We can further use <code>Comparator.comparing()</code> to construct the comparator from a "key" function:
```java
    public static void sortByName(Pair[] pairs) {
        Arrays.sort(pairs, Comparator.comparing(p -> p.name));
    }
```



## JavaScript


### ES5


```javascript
var arr = [
  {id: 3, value: "foo"},
  {id: 2, value: "bar"},
  {id: 4, value: "baz"},
  {id: 1, value: 42},
  {id: 5, something: "another string"} // Works with any object declaring 'id' as a number.
];
arr = arr.sort(function(a, b) {return a.id - b.id}); // Sort with comparator checking the id.

```



### ES6



```JavaScript
(() => {
    'use strict';

    // GENERIC FUNCTIONS FOR COMPARISONS

    // compare :: a -> a -> Ordering
    const compare = (a, b) => a < b ? -1 : (a > b ? 1 : 0);

    // on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
    const on = (f, g) => (a, b) => f(g(a), g(b));

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f => (a, b) => f.apply(null, [b, a]);

    // arrayCopy :: [a] -> [a]
    const arrayCopy = (xs) => xs.slice(0);

    // show :: a -> String
    const show = x => JSON.stringify(x, null, 2);


    // TEST
    const xs = [{
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
    }]

    // population :: Dictionary -> Num
    const population = x => x.pop;

    // name :: Dictionary -> String
    const name = x => x.name;

    return show({
        byPopulation: arrayCopy(xs)
            .sort(on(compare, population)),
        byDescendingPopulation: arrayCopy(xs)
            .sort(on(flip(compare), population)),
        byName: arrayCopy(xs)
            .sort(on(compare, name)),
        byDescendingName: arrayCopy(xs)
            .sort(on(flip(compare), name))
    });
})();
```


```txt
{
  "byPopulation": [
    {
      "name": "Lagos",
      "pop": 16.1
    },
    {
      "name": "Delhi",
      "pop": 16.8
    },
    {
      "name": "Dhaka",
      "pop": 17
    },
    {
      "name": "Beijing",
      "pop": 21.5
    },
    {
      "name": "Karachi",
      "pop": 23.5
    },
    {
      "name": "Shanghai",
      "pop": 24.2
    },
    {
      "name": "Sao Paulo",
      "pop": 24.2
    }
  ],
  "byDescendingPopulation": [
    {
      "name": "Shanghai",
      "pop": 24.2
    },
    {
      "name": "Sao Paulo",
      "pop": 24.2
    },
    {
      "name": "Karachi",
      "pop": 23.5
    },
    {
      "name": "Beijing",
      "pop": 21.5
    },
    {
      "name": "Dhaka",
      "pop": 17
    },
    {
      "name": "Delhi",
      "pop": 16.8
    },
    {
      "name": "Lagos",
      "pop": 16.1
    }
  ],
  "byName": [
    {
      "name": "Beijing",
      "pop": 21.5
    },
    {
      "name": "Delhi",
      "pop": 16.8
    },
    {
      "name": "Dhaka",
      "pop": 17
    },
    {
      "name": "Karachi",
      "pop": 23.5
    },
    {
      "name": "Lagos",
      "pop": 16.1
    },
    {
      "name": "Sao Paulo",
      "pop": 24.2
    },
    {
      "name": "Shanghai",
      "pop": 24.2
    }
  ],
  "byDescendingName": [
    {
      "name": "Shanghai",
      "pop": 24.2
    },
    {
      "name": "Sao Paulo",
      "pop": 24.2
    },
    {
      "name": "Lagos",
      "pop": 16.1
    },
    {
      "name": "Karachi",
      "pop": 23.5
    },
    {
      "name": "Dhaka",
      "pop": 17
    },
    {
      "name": "Delhi",
      "pop": 16.8
    },
    {
      "name": "Beijing",
      "pop": 21.5
    }
  ]
}
```



## jq

In this section we will focus on JSON objects since the task description mentions keys.  As an example, we will use this array:

```jq
def example:
 [
  {"name": "Joe", "value": 3},
  {"name": "Bill", "value": 4},
  {"name": "Alice", "value": 20},
  {"name": "Harry", "value": 3}
 ];
```

====Using sort_by builtin ====
jq's sort_by builtin can be used to sort by the value of a given key (whether or not it is a string), so we will first use that.

```jq
# To sort the array:
# example | sort_by(.name)

# To abbreviate the results, we will just show the names after sorting:

example | sort_by(.name) | map( .name )
```

 $ jq -n -c -f Sort_an_array_of_composite_structures.jq
 ["Alice","Bill","Harry","Joe"]

====Using quicksort(cmp)====
sort_by(f) can easily be implemented using quicksort(cmp) as defined at [[Sorting_Using_a_Custom_Comparator#jq]] as follows:

```jq
def quicksort_by(f): quicksort( (.[0]|f) <= (.[1]|f) );
```

'''Example''':

```jq
example | quicksort_by(.name) | map( .name )
```

As above.


## Julia

```julia
lst = Pair[Pair("gold", "shiny"),
           Pair("neon", "inert"),
           Pair("sulphur", "yellow"),
           Pair("iron", "magnetic"),
           Pair("zebra", "striped"),
           Pair("star", "brilliant"),
           Pair("apple", "tasty"),
           Pair("ruby", "red"),
           Pair("dice", "random"),
           Pair("coffee", "stimulating"),
           Pair("book", "interesting")]

println("The original list: \n - ", join(lst, "\n - "))
sort!(lst; by=first)
println("\nThe list, sorted by name: \n - ", join(lst, "\n - "))
sort!(lst; by=last)
println("\nThe list, sorted by value: \n - ", join(lst, "\n - "))
```


```txt
The original list:
 - "gold"=>"shiny"
 - "neon"=>"inert"
 - "sulphur"=>"yellow"
 - "iron"=>"magnetic"
 - "zebra"=>"striped"
 - "star"=>"brilliant"
 - "apple"=>"tasty"
 - "ruby"=>"red"
 - "dice"=>"random"
 - "coffee"=>"stimulating"
 - "book"=>"interesting"

The list, sorted by name:
 - "apple"=>"tasty"
 - "book"=>"interesting"
 - "coffee"=>"stimulating"
 - "dice"=>"random"
 - "gold"=>"shiny"
 - "iron"=>"magnetic"
 - "neon"=>"inert"
 - "ruby"=>"red"
 - "star"=>"brilliant"
 - "sulphur"=>"yellow"
 - "zebra"=>"striped"

The list, sorted by value:
 - "star"=>"brilliant"
 - "neon"=>"inert"
 - "book"=>"interesting"
 - "iron"=>"magnetic"
 - "dice"=>"random"
 - "ruby"=>"red"
 - "gold"=>"shiny"
 - "coffee"=>"stimulating"
 - "zebra"=>"striped"
 - "apple"=>"tasty"
 - "sulphur"=>"yellow"
```



## Kotlin


```scala
// version 1.1

data class Employee(val name: String, var category: String) : Comparable<Employee> {
    override fun compareTo(other: Employee) = this.name.compareTo(other.name)
}

fun main(args: Array<String>) {
    val employees = arrayOf(
        Employee("David", "Manager"),
        Employee("Alice", "Sales"),
        Employee("Joanna", "Director"),
        Employee("Henry", "Admin"),
        Employee("Tim", "Sales"),
        Employee("Juan", "Admin")
    )
    employees.sort()
    for ((name, category) in employees) println("${name.padEnd(6)} : $category")
}
```


```txt

Alice  : Sales
David  : Manager
Henry  : Admin
Joanna : Director
Juan   : Admin
Tim    : Sales

```



## Liberty BASIC

NB LB sorts in a non standard order. See http://libertybasicbugs.wikispaces.com/Comparison+of+characters+and+strings+is+not+ASCII+based


The method used here to simulate a compound structure can only hold pairs of terms, since LB arrays ar 1D or 2D. More complicated associated arrays could be stored in delimiter-separated string arrays.

```lb

N =20
dim IntArray$( N, 2)

print "Original order"
for i =1 to N
    name$ =mid$( "SortArrayOfCompositeStructures", int( 25 *rnd( 1)), 1 +int( 4 *rnd( 1)))
    IntArray$( i, 1) =name$
    print name$,
    t$ =str$( int( 1000 *rnd( 1)))
    IntArray$( i, 2) =t$
    print t$
next i

sort IntArray$(), 1, N, 1
print "Sorted by name"  ' (  we specified column 1)
for i =1 to N
    print IntArray$( i, 1), IntArray$( i, 2)
next i

```



## Lua


```lua
function sorting( a, b )
    return a[1] < b[1]
end

tab = { {"cpp", 1979}, {"Ada", 1983}, {"Ruby", 1995}, {"Eiffel", 1985} }

table.sort( tab, sorting )
for _, v in ipairs( tab ) do
    print( unpack(v) )
end
```



## M2000 Interpreter

Checkit do exactly task need: make pairs as groups of name and value_ (note _ used because value used for other purpose in a group definition), make a Quick group from class Quick, pass a new lambda for comparing items, and pass array with objects to sort in quicksort function of Quick group. Keys no need to be unique.

Checkit2 make an inventory with keys and values, then sort them (internal use Quicksort, and keys must be unique)

Checkit3 same as CheckIt3 except values are groups, which now have only a x as value, but we can add more.


```M2000 Interpreter

Module CheckIt {
      Flush ' empty stack of values
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
      Quick.LE=lambda (a, b)->{
            =a.name$<=b.name$
      }
      Data "Joe",   5531
      Data "Adam", 2341
      Data "Bernie", 122
      Data "Walter", 1234
      Data "David",  19
      Class pair {
            name$
            value_
      }
      Document Doc$={Unsorted Pairs:
      }
      Dim A(1 to 5)=pair()
      For i=1 to 5 {
            For A(i) {
                  Read .name$, .value_
                  Doc$=Format$("{0}, {1}", .name$, .value_)+{
                  }
            }
      }

      Call Quick.quicksort(&A(),1, 5)
      Doc$={
            Sorted Pairs
            }
      k=Each(A())
      While k {
            getone=array(k)
            For getone {
                              Doc$=Format$("{0}, {1}", .name$, .value_)+{
                              }
            }
      }
      Report Doc$
      Clipboard Doc$
}
Checkit
module Checkit2 {
      Inventory Alfa="Joe":=5531, "Adam":=2341, "Bernie":=122
      Append Alfa, "Walter":=1234, "David":=19
      Sort Alfa
      k=Each(Alfa)
      While k {
            Print eval$(Alfa, k^), Eval(k)
      }
}
Checkit2
module Checkit3 {
      class any {
             x
            class:
            Module any (.x) {}
      }
      Inventory Alfa="Joe":=any(5531), "Adam":=any(2341), "Bernie":=any(122)
      Append Alfa, "Walter":=any(1234), "David":=any(19)
      Sort Alfa
      k=Each(Alfa)
      While k {
            \\ k^ is the index number by k cursor
            \\ Alfa("joe") return object
            \\ Alfa(0!) return first element object
            \\ Alfa(k^!) return (k^) objext
            Print eval$(Alfa, k^),  Alfa(k^!).x
      }
}
Checkit3

```


From Checkit (checkit2 and checkit3 show exact sorted inventories)

```txt

Unsorted Pairs:
Joe, 5531
Adam, 2341
Bernie, 122
Walter, 1234
David, 19

Sorted Pairs
Adam, 2341
Bernie, 122
David, 19
Joe, 5531
Walter, 1234

```



## Mathematica


```Mathematica
events = {{"2009-12-25", "Christmas Day"}, {"2009-04-22",
    "Earth Day"}, {"2009-09-07", "Labor Day"}, {"2009-07-04",
    "Independence Day"}, {"2009-10-31", "Halloween"}, {"2009-05-25",
    "Memorial Day"}, {"2009-03-14", "PI Day"}, {"2009-01-01",
    "New Year's Day"}, {"2009-12-31",
    "New Year's Eve"}, {"2009-11-26", "Thanksgiving"}, {"2009-02-14",
    "St. Valentine's Day"}, {"2009-03-17",
    "St. Patrick's Day"}, {"2009-01-19",
    "Martin Luther King Day"}, {"2009-02-16", "President's Day"}};
date = 1;
name = 2;
SortBy[events, #[[name]] &] // Grid
SortBy[events, #[[date]] &] // Grid
```

gives back:

```Mathematica
2009-12-25 Christmas Day
2009-04-22 Earth Day
2009-10-31 Halloween
2009-07-04 Independence Day
2009-09-07 Labor Day
2009-01-19 Martin Luther King Day
2009-05-25 Memorial Day
2009-01-01 New Year's Day
2009-12-31 New Year's Eve
2009-03-14 PI Day
2009-02-16 President's Day
2009-03-17 St. Patrick's Day
2009-02-14 St. Valentine's Day
2009-11-26 Thanksgiving


2009-01-01 New Year's Day
2009-01-19 Martin Luther King Day
2009-02-14 St. Valentine's Day
2009-02-16 President's Day
2009-03-14 PI Day
2009-03-17 St. Patrick's Day
2009-04-22 Earth Day
2009-05-25 Memorial Day
2009-07-04 Independence Day
2009-09-07 Labor Day
2009-10-31 Halloween
2009-11-26 Thanksgiving
2009-12-25 Christmas Day
2009-12-31 New Year's Eve
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

--
### =======================================================================

class RSortCompsiteStructure public

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method main(args = String[]) public static
    places = [ -
      PairBean('London',     'UK'), PairBean('New York',   'US') -
    , PairBean('Boston',     'US'), PairBean('Washington', 'US') -
    , PairBean('Washington', 'UK'), PairBean("Birmingham", 'US') -
    , PairBean("Birmingham", 'UK'), PairBean("Boston",     'UK') -
    ]
    say displayArray(places)
    Arrays.sort(places, PairComparator())
    say displayArray(places)
    return

  method displayArray(harry = PairBean[]) constant
    disp = ''
    loop elmt over harry
      disp = disp','elmt
      end elmt
    return '['disp.substr(2)']' -- trim leading comma

--
### =======================================================================

class RSortCompsiteStructure.PairBean
  properties indirect
    name
    value
  method PairBean(name_, value_) public
    setName(name_)
    setValue(value_)
    return
  method toString() public returns String
    return '('getName()','getValue()')'

--
### =======================================================================

class RSortCompsiteStructure.PairComparator implements Comparator
  method compare(lft = Object, rgt = Object) public binary returns int
    cRes = int
    if lft <= RSortCompsiteStructure.PairBean, rgt <= RSortCompsiteStructure.PairBean then do
      lName = String (RSortCompsiteStructure.PairBean lft).getName()
      rName = String (RSortCompsiteStructure.PairBean rgt).getName()
      cRes = lName.compareTo(rName)
      if cRes == 0 then do
        lVal = String (RSortCompsiteStructure.PairBean lft).getValue()
        rVal = String (RSortCompsiteStructure.PairBean rgt).getValue()
        cRes = lVal.compareTo(rVal)
        end
      end
    else signal IllegalArgumentException('Arguments must be of type PairBean')
    return cRes

```

'''Output:'''

```txt

[(London,UK),(New York,US),(Boston,US),(Washington,US),(Washington,UK),(Birmingham,US),(Birmingham,UK),(Boston,UK)]
[(Birmingham,UK),(Birmingham,US),(Boston,UK),(Boston,US),(London,UK),(New York,US),(Washington,UK),(Washington,US)]

```



## MAXScript


```maxscript
fn keyCmp comp1 comp2 =
(
    case of
    (
        (comp1[1] > comp2[1]):	1
        (comp1[1] < comp2[1]):	-1
        default:		0
    )
)

people = #(#("joe", 39), #("dave", 37), #("bob", 42))
qsort people keyCmp
print people
```



## Nim


```nim
import algorithm, future

var people = @{"joe": 120, "foo": 31, "bar": 51}
sort(people, (x,y) => cmp(x[0], y[0]))
echo people
```

Output:

```txt
@[(Field0: bar, Field1: 51), (Field0: foo, Field1: 31), (Field0: joe, Field1: 120)]
```



## Objeck


```objeck

use Collection;

class Entry implements Compare {
  @name : String;
  @value : Float;

  New(name : String, value : Float) {
    @name := name;
    @value := value;
  }

  method : public : Compare(rhs : Compare) ~ Int  {
    return @name->Compare(rhs->As(Entry)->GetName());
  }

  method : public : GetName() ~ String {
    return @name;
  }

  method : public : HashID() ~ Int {
    return @name->HashID();
  }

  method : public : ToString() ~ String {
    return "name={$@name}, value={$@value}";
  }
}

class Sorter {
  function : Main(args : String[]) ~ Nil {
    entries := CompareVector->New();
    entries->AddBack(Entry->New("Krypton", 83.798));
    entries->AddBack(Entry->New("Beryllium", 9.012182));
    entries->AddBack(Entry->New("Silicon", 28.0855));
    entries->AddBack(Entry->New("Cobalt", 58.933195));
    entries->AddBack(Entry->New("Selenium", 78.96));
    entries->AddBack(Entry->New("Germanium", 72.64));

    entries->Sort();
    each(i : entries) {
      entries->Get(i)->As(Entry)->ToString()->PrintLine();
    };
  }
}

```



```txt

name=Beryllium, value=9.12
name=Cobalt, value=58.934
name=Germanium, value=72.640
name=Krypton, value=83.798
name=Selenium, value=78.960
name=Silicon, value=28.85

```


=={{header|Objective-C}}==

```objc
@interface Pair : NSObject {
    NSString *name;
    NSString *value;
}
+(instancetype)pairWithName:(NSString *)n value:(NSString *)v;
-(instancetype)initWithName:(NSString *)n value:(NSString *)v;
-(NSString *)name;
-(NSString *)value;
@end

@implementation Pair
+(instancetype)pairWithName:(NSString *)n value:(NSString *)v {
    return [[self alloc] initWithName:n value:v];
}
-(instancetype)initWithName:(NSString *)n value:(NSString *)v {
    if ((self = [super init])) {
        name = n;
        value = v;
    }
    return self;
}
-(NSString *)name { return name; }
-(NSString *)value { return value; }
-(NSString *)description {
    return [NSString stringWithFormat:@"< %@ -> %@ >", name, value];
}
@end

int main() {
    @autoreleasepool {

        NSArray *pairs = @[
                       [Pair pairWithName:@"06-07" value:@"Ducks"],
                       [Pair pairWithName:@"00-01" value:@"Avalanche"],
                       [Pair pairWithName:@"02-03" value:@"Devils"],
                       [Pair pairWithName:@"01-02" value:@"Red Wings"],
                       [Pair pairWithName:@"03-04" value:@"Lightning"],
                       [Pair pairWithName:@"04-05" value:@"lockout"],
                       [Pair pairWithName:@"05-06" value:@"Hurricanes"],
                       [Pair pairWithName:@"99-00" value:@"Devils"],
                       [Pair pairWithName:@"07-08" value:@"Red Wings"],
                       [Pair pairWithName:@"08-09" value:@"Penguins"]];

        // optional 3rd arg: you can also specify a selector to compare the keys
        NSSortDescriptor *sd = [[NSSortDescriptor alloc] initWithKey:@"name" ascending:YES];

        // it takes an array of sort descriptors, and it will be ordered by the
        // first one, then if it's a tie by the second one, etc.
        NSArray *sorted = [pairs sortedArrayUsingDescriptors:@[sd]];
        NSLog(@"%@", sorted);

    }

    return 0;
}
```



## OCaml


```ocaml
# let people = [("Joe", 12); ("Bob", 8); ("Alice", 9); ("Harry", 2)];;
val people : (string * int) list =
  [("Joe", 12); ("Bob", 8); ("Alice", 9); ("Harry", 2)]
# let sortedPeopleByVal = List.sort (fun (_, v1) (_, v2) -> compare v1 v2) people;;
val sortedPeopleByVal : (string * int) list =
  [("Harry", 2); ("Bob", 8); ("Alice", 9); ("Joe", 12)]
```



## Oforth



```Oforth
[["Joe",5531], ["Adam",2341], ["Bernie",122], ["David",19]] sortBy(#first) println
```

```txt

[[Adam, 2341], [Bernie, 122], [David, 19], [Joe, 5531]]

```



## ooRexx


```ooRexx

a = .array~new

a~append(.pair~new("06-07", "Ducks"))
a~append(.pair~new("00-01", "Avalanche"))
a~append(.pair~new("02-03", "Devils"))
a~append(.pair~new("01-02", "Red Wings"))
a~append(.pair~new("03-04", "Lightning"))
a~append(.pair~new("04-05", "lockout"))
a~append(.pair~new("05-06", "Hurricanes"))
a~append(.pair~new("99-00", "Devils"))
a~append(.pair~new("07-08", "Red Wings"))
a~append(.pair~new("08-09", "Penguins"))

b = a~copy   -- make a copy before sorting
b~sort
say "Sorted using direct comparison"
do pair over b
   say pair
end

c = a~copy
-- this uses a custom comparator instead
c~sortWith(.paircomparator~new)
say
say "Sorted using a comparator (inverted)"
do pair over c
   say pair
end

-- a name/value mapping class that directly support the sort comparisons
::class pair inherit comparable
::method init
  expose name value
  use strict arg name, value

::attribute name
::attribute value

::method string
  expose name value
  return name "=" value

-- the compareto method is a requirement brought in
-- by the
::method compareto
  expose name
  use strict arg other
  return name~compareto(other~name)

-- a comparator that shows an alternate way of sorting
::class pairComparator subclass comparator
::method compare
  use strict arg left, right
  -- perform the comparison on the names
  return -left~name~compareTo(right~name)

```

Output:

```txt

Sorted using direct comparison
00-01 = Avalanche
01-02 = Red Wings
02-03 = Devils
03-04 = Lightning
04-05 = lockout
05-06 = Hurricanes
06-07 = Ducks
07-08 = Red Wings
08-09 = Penguins
99-00 = Devils

Sorted using a comparator (inverted)
99-00 = Devils
08-09 = Penguins
07-08 = Red Wings
06-07 = Ducks
05-06 = Hurricanes
04-05 = lockout
03-04 = Lightning
02-03 = Devils
01-02 = Red Wings
00-01 = Avalanche

```




## Oz


```oz
declare
  People = [person(name:joe value:3)
            person(name:bill value:4)
            person(name:alice value:20)
            person(name:harry value:3)]

  SortedPeople = {Sort People
                  fun {$ P1 P2}
                     P1.name < P2.name
                  end
                 }
in
  {ForAll SortedPeople Show}
```



## PARI/GP

The flag "2" means that lexicographic sorting is to be used; the "1" means that the array is to be sorted using the first element of each constituent vector.

```parigp
vecsort([["name", "value"],["name2", "value2"]], 1, 2)
```



## Pascal

mergesort example sorts an array of record http://rosettacode.org/wiki/Sorting_algorithms/Merge_sort#improvement

## Perl

Sort by name using cmp to compare strings:

```perl
@people = (['joe', 120], ['foo', 31], ['bar', 51]);
@people = sort { $a->[0] cmp $b->[0] } @people;
```


Sort by number using <=> to compare numbers:

```perl
@people = (['joe', 120], ['foo', 31], ['bar', 51]);
@people = sort { $a->[1] <=> $b->[1] } @people;
```



## Perl 6

```perl6
my class Employee {
   has Str $.name;
   has Rat $.wage;
}

my $boss     = Employee.new( name => "Frank Myers"     , wage => 6755.85 );
my $driver   = Employee.new( name => "Aaron Fast"      , wage => 2530.40 );
my $worker   = Employee.new( name => "John Dude"       , wage => 2200.00 );
my $salesman = Employee.new( name => "Frank Mileeater" , wage => 4590.12 );

my @team = $boss, $driver, $worker, $salesman;

my @orderedByName = @team.sort( *.name )».name;
my @orderedByWage = @team.sort( *.wage )».name;

say "Team ordered by name (ascending order):";
say @orderedByName.join('  ');
say "Team ordered by wage (ascending order):";
say @orderedByWage.join('  ');
```

this produces the following output:

```txt
Team ordered by name (ascending order):
Aaron Fast   Frank Mileeater   Frank Myers   John Dude
Team ordered by wage (ascending order):
John Dude   Aaron Fast   Frank Mileeater   Frank Myers

```

Note that when the sort receives a unary function, it automatically generates an appropriate comparison function based on the type of the data.


## Phix

The standard sort compares the first element, then 2nd, 3rd, etc. A custom_sort or sort_columns can be used to sort by other elements.

Elements can be any mix of types, with atoms (ie ints/floats) deemed less than sequences/strings.

```Phix
sequence s = {{"grass","green"},{"snow","white"},{"sky","blue"},{"cherry","red"},{0,1.2},{3.4,-1}}

?sort(s)
function compare_col2(sequence a, b) return compare(a[2],b[2]) end function
?custom_sort(routine_id("compare_col2"),s)
?sort_columns(s,{2})    -- 0.8.0+, same result as above w/o needing an explicit comparison routine
```

```txt

```



## PicoLisp

By default, the [http://software-lab.de/doc/refS.html#sort sort] function in
PicoLisp returns an ascending list (of any type)

```PicoLisp
: (sort '(("def" 456) ("abc" 789) ("ghi" 123)))
-> (("abc" 789) ("def" 456) ("ghi" 123))
```

To sort by a certain sub-element, the function
[http://software-lab.de/doc/refB.html#by by] can be used. For example, to
sort by the first element

```PicoLisp
: (by car sort '(("def" 456) ("abc" 789) ("ghi" 123)))
-> (("abc" 789) ("def" 456) ("ghi" 123))
```

or by the second element

```PicoLisp
: (by cadr sort '(("def" 456) ("abc" 789) ("ghi" 123)))
-> (("ghi" 123) ("def" 456) ("abc" 789))
```



## PowerShell


```PowerShell

$list = @{
"def" = "one"
"abc" = "two"
"jkl" = "three"
"abcdef" = "four"
"ghi" = "five"
"ghijkl" = "six"
 }
 $list.GetEnumerator() | sort {-($PSItem.Name).length}, Name

```

<b>Output:</b>

```txt

Name                           Value
----                           -----
abcdef                         four
ghijkl                         six
abc                            two
def                            one
ghi                            five
jkl                            three

```



## PureBasic

PureBasic natively supports sorting of structured data with;
*SortStructuredArray()
*SortStructuredList()
The [http://www.purebasic.com/documentation/sort/index.html on-line documentations] gives a more complete picture.


'''Example'''

```PureBasic
Structure MyPair ; Define a structured data type
  Name$
  Value.i
EndStructure

Dim People.MyPair(2)             ; Allocate some elements

People(0)\Name$ = "John"         ; Start filling them in
People(0)\Value = 100

People(1)\Name$ = "Emma"
People(1)\Value = 200

People(2)\Name$ = "Johnny"
People(2)\Value = 175

If OpenConsole()
  Define i
  ; Sort ascending by name
  SortStructuredArray(People(), #PB_Sort_Ascending, OffsetOf(MyPair\Name$), #PB_Sort_String)
  PrintN(#CRLF$+"Sorted ascending by name.")
  For i=0 To 2
    PrintN(People(i)\Name$+" - Value: "+Str(People(i)\Value))
  Next
  ; Sort descending by value
  SortStructuredArray(People(), #PB_Sort_Descending, OffsetOf(MyPair\Value), #PB_Sort_Integer)
  PrintN(#CRLF$+"Sorted descending by value.")
  For i=0 To 2
    PrintN(People(i)\Name$+" - Value: "+Str(People(i)\Value))
  Next
  ; Wait for user...
  PrintN(#CRLF$+"Press ENTER to exit"):Input()
EndIf
```


'''Outputs
 Sorted ascending by name.
 Emma - Value: 200
 John - Value: 100
 Johnny - Value: 175

 Sorted descending by value.
 Emma - Value: 200
 Johnny - Value: 175
 John - Value: 100


## Python

Recent versions of Python provide the ''sorted()'' built-in that works on any iterable.


```python
people = [('joe', 120), ('foo', 31), ('bar', 51)]
sorted(people)
```

Which leaves <code>people</code> with the value:

```python
[('bar', 51), ('foo', 31), ('joe', 120)]
```



The most Pythonic (and fastest) version is to use itemgetter together with the key parameter to <code>sort</code> resp. <code>sorted</code> to perform the [[wp:Decorate-sort-undecorate|Decorate-sort-undecorate]] pattern:


```python
from operator import itemgetter
people = [(120, 'joe'), (31, 'foo'), (51, 'bar')]
people.sort(key=itemgetter(1))
```

Which leaves <code>people</code> with the value:

```python
[(51, 'bar'), (31, 'foo'), (120, 'joe')]
```



## R

In R, vectors can have names associated with any of its elements.  The data is taken from the Common Lisp example.

```R
sortbyname <- function(x, ...) x[order(names(x), ...)]
x <- c(texas=68.9, ohio=87.8, california=76.2, "new york"=88.2)
sortbyname(x)
```

 california   new york       ohio      texas
       76.2       88.2       87.8       68.9


```R
sortbyname(x, decreasing=TRUE)
```

      texas       ohio   new york california
       68.9       87.8       88.2       76.2


## Racket


```Racket

#lang racket

(define data '([Joe 5531] [Adam 2341] [Bernie 122] [Walter 1234] [David 19]))

(sort data < #:key cadr)
;; --> '((David 19) (Bernie 122) (Walter 1234) (Adam 2341) (Joe 5531))

;; Demonstrating a "key" that is not just a direct element
(sort data string<? #:key (compose1 symbol->string car))
;; --> '((Adam 2341) (Bernie 122) (David 19) (Joe 5531) (Walter 1234))

```



## REXX

This version sorts the structure by color;   as entered (built),   the structure is ordered by value (percent).

```rexx
/*REXX program  sorts an array of composite structures  (which has two classes of data).*/
#=0                                              /*number elements in structure (so far)*/
name='tan'   ;  value= 0;  call add name,value   /*tan    peanut M&M's are  0%  of total*/
name='orange';  value=10;  call add name,value   /*orange    "    "     "  10%   "   "  */
name='yellow';  value=20;  call add name,value   /*yellow    "    "     "  20%   "   "  */
name='green' ;  value=20;  call add name,value   /*green     "    "     "  20%   "   "  */
name='red'   ;  value=20;  call add name,value   /*red       "    "     "  20%   "   "  */
name='brown' ;  value=30;  call add name,value   /*brown     "    "     "  30%   "   "  */
call show  'before sort',  #
say  copies('▒', 70)
call xSort                 #
call show  ' after sort',  #
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
add:   procedure expose # @.;   #=#+1            /*bump the number of structure entries.*/
       @.#.color=arg(1);      @.#.pc=arg(2)      /*construct a entry of the structure.  */
       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:  procedure expose @.;   do j=1  for arg(2) /*2nd arg≡number of structure elements.*/
                              say right(arg(1),30)  right(@.j.color,9)  right(@.j.pc,4)'%'
                              end   /*j*/        /* [↑]  display  what,  name,  value.  */
       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
xSort: procedure expose @.; parse arg N;    h=N
                              do while h>1;                       h=h%2
                                do i=1  for N-h;        j=i;      k=h+i
                                  do  while @.k.color<@.j.color         /*swap elements.*/
                                  _=@.j.color;        @.j.color=@.k.color;     @.k.color=_
                                  _=@.j.pc;           @.j.pc   =@.k.pc;        @.k.pc   =_
                                  if h>=j  then leave;    j=j-h;    k=k-h
                                  end   /*while @.k.color ···*/
                                end     /*i*/
                              end       /*while h>1*/
       return
```

'''output'''   when using the (internal) default inputs:

```txt

                   before sort       tan    0%
                   before sort    orange   10%
                   before sort    yellow   20%
                   before sort     green   20%
                   before sort       red   20%
                   before sort     brown   30%
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
                    after sort     brown   30%
                    after sort     green   20%
                    after sort    orange   10%
                    after sort       red   20%
                    after sort       tan    0%
                    after sort    yellow   20%

```



## Ruby



```ruby
Person = Struct.new(:name,:value) do
  def to_s; "name:#{name}, value:#{value}" end
end

list = [Person.new("Joe",3),
        Person.new("Bill",4),
        Person.new("Alice",20),
        Person.new("Harry",3)]
puts list.sort_by{|x|x.name}
puts
puts list.sort_by(&:value)
```

```txt

name:Alice, value:20
name:Bill, value:4
name:Harry, value:3
name:Joe, value:3

name:Joe, value:3
name:Harry, value:3
name:Bill, value:4
name:Alice, value:20

```



## Run BASIC


```runbasic
sqliteconnect #mem, ":memory:"                          ' create in memory db
mem$	= "CREATE TABLE people(num integer, name text,city text)"
#mem execute(mem$)
data "1","George","Redding","2","Fred","Oregon","3","Ben","Seneca","4","Steve","Fargo","5","Frank","Houston"

for i = 1 to 5                                          ' read data and place in memory DB
 read num$ :read name$: read city$
 #mem execute("INSERT INTO people VALUES(";num$;",'";name$;"','";city$;"')")
next i
#mem execute("SELECT * FROM people ORDER BY name")      'sort by name order
WHILE  #mem hasanswer()
  #row  = #mem #nextrow()
  num   = #row num()
  name$	= #row name$()
  city$	= #row city$()
  print num;" ";name$;" ";city$
WEND
```

```txt
3 Ben Seneca
5 Frank Houston
2 Fred Oregon
1 George Redding
4 Steve Fargo
```



## Rust

```Rust
use std::cmp::Ordering;

#[derive(Debug)]
struct Employee {
    name: String,
    category: String,
}

impl Employee {
    fn new(name: &str, category: &str) -> Self {
        Employee {
            name: name.into(),
            category: category.into(),
        }
    }
}

impl PartialEq for Employee {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for Employee {}

impl PartialOrd for Employee {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Employee {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

fn main() {
    let mut employees = vec![
        Employee::new("David", "Manager"),
        Employee::new("Alice", "Sales"),
        Employee::new("Joanna", "Director"),
        Employee::new("Henry", "Admin"),
        Employee::new("Tim", "Sales"),
        Employee::new("Juan", "Admin"),
    ];
    employees.sort();
    for e in employees {
        println!("{:<6} : {}", e.name, e.category);
    }
}
```

```txt
Alice  : Sales
David  : Manager
Henry  : Admin
Joanna : Director
Juan   : Admin
Tim    : Sales
```



## Scala


```scala
case class Pair(name:String, value:Double)
val input = Array(Pair("Krypton", 83.798), Pair("Beryllium", 9.012182), Pair("Silicon", 28.0855))
input.sortBy(_.name) // Array(Pair(Beryllium,9.012182), Pair(Krypton,83.798), Pair(Silicon,28.0855))

// alternative versions:
input.sortBy(struct => (struct.name, struct.value)) // additional sort field (name first, then value)
input.sortWith((a,b) => a.name.compareTo(b.name) < 0) // arbitrary comparison function
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const type: pair is new struct
    var string: name is "";
    var string: value is "";
  end struct;

const func integer: compare (in pair: pair1, in pair: pair2) is
  return compare(pair1.name, pair2.name);

const func string: str (in pair: aPair) is
  return "(" <& aPair.name <& ", " <& aPair.value <& ")";

enable_output(pair);

const func pair: pair (in string: name, in string: value) is func
  result
    var pair: newPair is pair.value;
  begin
    newPair.name := name;
    newPair.value := value;
  end func;

var array pair: data is [] (
    pair("Joe",    "5531"),
    pair("Adam",   "2341"),
    pair("Bernie", "122"),
    pair("Walter", "1234"),
    pair("David",  "19"));

const proc: main is func
  local
    var pair: aPair is pair.value;
  begin
    data := sort(data);
    for aPair range data do
      writeln(aPair);
    end for;
  end func;
```


Output:

```txt

(Adam, 2341)
(Bernie, 122)
(David, 19)
(Joe, 5531)
(Walter, 1234)

```



## Sidef


```ruby
# Declare an array of pairs
var people = [['joe', 120], ['foo', 31], ['bar', 51]];

# Sort the array in-place by name
people.sort! {|a,b| a[0] <=> b[0] };

# Alternatively, we can use the `.sort_by{}` method
var sorted = people.sort_by { |item| item[0] };

# Display the sorted array
say people;
```


```txt
[["bar", 51], ["foo", 31], ["joe", 120]]
```


## Simula


```simula
BEGIN

    CLASS COMPARABLE;;

    COMPARABLE CLASS PAIR(N,V); TEXT N,V;;

    CLASS COMPARATOR;
    VIRTUAL:
        PROCEDURE COMPARE IS
            INTEGER PROCEDURE COMPARE(A, B); REF(COMPARABLE) A, B;;
    BEGIN
    END**OF**COMPARATOR;

    COMPARATOR CLASS PAIRBYNAME;
    BEGIN
        INTEGER PROCEDURE COMPARE(A, B); REF(COMPARABLE) A, B;
        BEGIN
            COMPARE := IF A QUA PAIR.N < B QUA PAIR.N THEN -1 ELSE
                       IF A QUA PAIR.N > B QUA PAIR.N THEN +1 ELSE 0;
        END;
    END**OF**PAIRBYNAME;

    PROCEDURE BUBBLESORT(A, C); NAME A; REF(COMPARABLE) ARRAY A; REF(COMPARATOR) C;
    BEGIN
       INTEGER LOW, HIGH, I;
       BOOLEAN SWAPPED;

       PROCEDURE SWAP(I, J); INTEGER I, J;
       BEGIN
           REF(COMPARABLE) TEMP;
           TEMP :- A(I); A(I) :- A(J); A(J) :- TEMP;
       END**OF**SWAP;

       LOW := LOWERBOUND(A, 1);
       HIGH := UPPERBOUND(A, 1);
       SWAPPED := TRUE;
       WHILE SWAPPED DO
       BEGIN
           SWAPPED := FALSE;
           FOR I := LOW + 1 STEP 1 UNTIL HIGH DO
           BEGIN
               COMMENT IF THIS PAIR IS OUT OF ORDER ;
               IF C.COMPARE(A(I - 1), A(I)) > 0 THEN
               BEGIN
                   COMMENT SWAP THEM AND REMEMBER SOMETHING CHANGED ;
                   SWAP(I - 1, I);
                   SWAPPED := TRUE;
               END;
           END;
       END;
    END**OF**BUBBLESORT;

    COMMENT ** MAIN PROGRAM **;
    REF(PAIR) ARRAY A(1:5);
    INTEGER I;

    A(1) :- NEW PAIR( "JOE", "5531" );
    A(2) :- NEW PAIR( "ADAM", "2341" );
    A(3) :- NEW PAIR( "BERNIE", "122" );
    A(4) :- NEW PAIR( "WALTER", "1234" );
    A(5) :- NEW PAIR( "DAVID", "19" );

    BUBBLESORT(A, NEW PAIRBYNAME);

    FOR I:= 1 STEP 1 UNTIL 5 DO
    BEGIN OUTTEXT(A(I).N); OUTCHAR(' '); OUTTEXT(A(I).V); OUTIMAGE; END;
    OUTIMAGE;

END.
```

```txt
ADAM 2341
BERNIE 122
DAVID 19
JOE 5531
WALTER 1234
```



## SQL

We can treat the array of data structures as a table. An <code>order by</code> clause in a query will sort the data.
```sql
-- setup
create table pairs (name varchar(16), value varchar(16));
insert into pairs values ('Fluffy', 'cat');
insert into pairs values ('Fido', 'dog');
insert into pairs values ('Francis', 'fish');
-- order them by name
select * from pairs order by name;
```

```txt
NAME             VALUE
---------------- ----------------
Fido             dog
Fluffy           cat
Francis          fish
```


## Tcl

Modeling the data structure being sorted as a list (a common Tcl practice):

```tcl
set people {{joe 120} {foo 31} {bar 51}}
# sort by the first element of each pair
lsort -index 0 $people
```



## UNIX Shell

With this language, everything is a string. My list of pairs is a string where a colon ":" separates "name:value", and a newline separates different pairs. Then I can use <tt>sort -t: -k1,1</tt> to sort the pairs by name.


```bash
list="namez:order!
name space:in
name1:sort
name:Please"

newline="
"

dumplist() {
	(
		IFS=$newline
		for pair in $list; do
			(
				IFS=:
				set -- $pair
				echo "  $1 => $2"
			)
		done
	)
}

echo "Original list:"
dumplist

list=`IFS=$newline; printf %s "$list" | sort -t: -k1,1`

echo "Sorted list:"
dumplist
```


Output:
```txt
Original list:
  namez => order!
  name space => in
  name1 => sort
  name => Please
Sorted list:
  name => Please
  name space => in
  name1 => sort
  namez => order!
```



## Ursala


The built in sort operator, -<, can be parameterized by an
anonymous field specification and/or a relational predicate.

```Ursala
#import std

#cast %sWLW

examples =

(
   -<&l <('z','a'),('x','c'),('y','b')>,  # sorted by the left
   -<&r <('z','a'),('x','c'),('y','b')>)  # sorted by the right
```

output:

```txt
(
   <('x','c'),('y','b'),('z','a')>,
   <('z','a'),('y','b'),('x','c')>)
```


a more verbose example, showing a list of records of a user defined type sorted by a named field:


```Ursala
#import std

person :: name %s value %s

people =

<
   person[name: 'Marilyn Monroe',value: 'priceless'],
   person[name: 'Victor Hugo',value: 'millions'],
   person[name: 'Johnny Carson',value: 'up there']>

#cast _person%L

example = (lleq+ ~name~~)-< people
```

output:

```txt

<
   person[name: 'Johnny Carson',value: 'up there'],
   person[name: 'Marilyn Monroe',value: 'priceless'],
   person[name: 'Victor Hugo',value: 'millions']>
```



## XPL0


```XPL0
include c:\cxpl\stdlib;
char Dict(10,10);
int  Entries;

proc BSort(A, N);       \Bubble sort array A's key string into ascending order
char A;                 \address of array
int  N;                 \number of items in array (size)
int  B, I, J, T;
[B:= A;                 \B(I) accesses 32-bit pointers, not A(I) 8-bit bytes
for J:= N-1 downto 0 do
    for I:= 0 to J-1 do
        if StrCmp(A(I,1), A(I+1,1)) > 0 then
            [T:= B(I);  B(I):= B(I+1);  B(I+1):= T];    \swap pointers
];

proc AddEntry(Letter, Greek);   \Insert entry into associative array
char Letter, Greek;
[Dict(Entries,0):= Letter;
StrCopy(Greek, @Dict(Entries,1));
Entries:= Entries+1;            \(limit checks ignored for simplicity)
];

int I;
[Entries:= 0;
AddEntry(^A, "alpha");          \add items in arbitrary order
AddEntry(^D, "delta");
AddEntry(^B, "beta");
AddEntry(^C, "gamma");
BSort(Dict, Entries);           \sort entries by Greek name
for I:= 0 to Entries-1 do       \show sorted entries
    [ChOut(0, Dict(I,0));  ChOut(0, ^ );  Text(0, @Dict(I,1));  CrLf(0)];
]
```


```txt

A alpha
B beta
D delta
C gamma

```



## zkl

The list of lists way is available or:

```zkl
class P{var name,value;
   fcn init(nm,val){name,value=vm.arglist}
   fcn __opLT(p){name<p.name}  // implementation of P1 < P2
}
// create list of pairs:
p:=List(P("sam","a"),P("fred","b"),P("chris","c"));
p.sort();
p.apply("name"); //-->L("chris","fred","sam")
```

