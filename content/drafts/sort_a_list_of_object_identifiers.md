+++
title = "Sort a list of object identifiers"
description = ""
date = 2019-05-19T01:37:23Z
aliases = []
[extra]
id = 20993
[taxonomies]
categories = []
tags = []
+++

{{task|Sorting Algorithms}}
{{Sorting Algorithm}}

[[wp:Object identifier|Object identifiers (OID)]] are strings used to identify objects in network data.


;Task:
Show how to sort a list of OIDs, in their natural sort order.

{{task heading|Details}}

* An OID consists of one or more non-negative integers in base 10, separated by dots. It starts and ends with a number.
* Their natural sort order is [[wp:Lexicographical_order|lexicographical]] with regard to the dot-separated fields, using numeric comparison between fields.

{{task heading|Test case}}

{| class="wikitable"
|-
! Input ''(list of strings)''
! Output ''(list of strings)''
|-
|
<code>1.3.6.1.4.1.11.2.17.19.3.4.0.10</code>

<code>1.3.6.1.4.1.11.2.17.5.2.0.79</code>

<code>1.3.6.1.4.1.11.2.17.19.3.4.0.4</code>

<code>1.3.6.1.4.1.11150.3.4.0.1</code>

<code>1.3.6.1.4.1.11.2.17.19.3.4.0.1</code>

<code>1.3.6.1.4.1.11150.3.4.0</code>
|
<code>1.3.6.1.4.1.11.2.17.5.2.0.79</code>

<code>1.3.6.1.4.1.11.2.17.19.3.4.0.1</code>

<code>1.3.6.1.4.1.11.2.17.19.3.4.0.4</code>

<code>1.3.6.1.4.1.11.2.17.19.3.4.0.10</code>

<code>1.3.6.1.4.1.11150.3.4.0</code>

<code>1.3.6.1.4.1.11150.3.4.0.1</code>
|}

{{task heading|Related tasks}}

* [[Natural sorting]]
* [[Sort using a custom comparator]]

<hr>


## Ada


{{works with|Ada|Ada|2012}}


```Ada
with Ada.Containers.Generic_Array_Sort;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

procedure Sort_List_Identifiers is
   type Natural_Array is array (Positive range <>) of Natural;
   type Unbounded_String_Array is array(Positive range <>) of Unbounded_String;

   function To_Natural_Array(input : in String) return Natural_Array
   is
      target : Natural_Array(1 .. Ada.Strings.Fixed.Count(input, ".") + 1);
      from : Natural := input'First;
      to : Natural := Ada.Strings.Fixed.Index(input, ".");
      index : Positive := target'First;
   begin
      while to /= 0 loop
         target(index) := Natural'Value(input(from .. to - 1));
         from := to + 1;
         index := index + 1;
         to := Ada.Strings.Fixed.Index(input, ".", from);
      end loop;
      target(index) := Natural'Value(input(from .. input'Last));
      return target;
   end To_Natural_Array;

   function Lesser(Left, Right : in Unbounded_String) return Boolean is
   begin
      return To_Natural_Array(To_String(Left)) < To_Natural_Array(To_String(Right));
   end Lesser;

   procedure Sort is new Ada.Containers.Generic_Array_Sort
     (Index_Type   => Positive,
      Element_Type => Unbounded_String,
      Array_Type   => Unbounded_String_Array,
      "<"          => Lesser);

   table : Unbounded_String_Array :=
     (To_Unbounded_String("1.3.6.1.4.1.11.2.17.19.3.4.0.10"),
      To_Unbounded_String("1.3.6.1.4.1.11.2.17.5.2.0.79"),
      To_Unbounded_String("1.3.6.1.4.1.11.2.17.19.3.4.0.4"),
      To_Unbounded_String("1.3.6.1.4.1.11150.3.4.0.1"),
      To_Unbounded_String("1.3.6.1.4.1.11.2.17.19.3.4.0.1"),
      To_Unbounded_String("1.3.6.1.4.1.11150.3.4.0"));
begin
   Sort(table);
   for element of table loop
      Ada.Text_IO.Put_Line(To_String(element));
   end loop;
end Sort_List_Identifiers;
```

{{out}}

```txt
1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1

```



## C#


```c#
using System;
using System.Linq;
using System.Collections.Generic;

public class Program
{
    public static void Main() {
        var oids = new [] {
            "1.3.6.1.4.1.11.2.17.19.3.4.0.10",
            "1.3.6.1.4.1.11.2.17.5.2.0.79",
            "1.3.6.1.4.1.11.2.17.19.3.4.0.4",
            "1.3.6.1.4.1.11150.3.4.0.1",
            "1.3.6.1.4.1.11.2.17.19.3.4.0.1",
            "1.3.6.1.4.1.11150.3.4.0"
        };

        var comparer = Comparer<string>.Create((a, b) => {
            int c = a.Split('.').Select(int.Parse)
	        .Zip(b.Split('.').Select(int.Parse),
                    (i, j) => i.CompareTo(j)).FirstOrDefault(x => x != 0);
            return c != 0 ? c : a.Length.CompareTo(b.Length);
        });

        Array.Sort(oids, comparer);

        Console.WriteLine(string.Join(Environment.NewLine, oids));
    }
}
```

{{out}}

```txt

1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1

```



## C++


```cpp
#include <string>
#include <vector>
#include <algorithm>
#include <boost/tokenizer.hpp>
#include <iostream>

std::vector<std::string> splitOnChar ( std::string & s , const char c ) {
   typedef boost::tokenizer<boost::char_separator<char>> tokenizer ;
   std::vector<std::string> parts ;
   boost::char_separator<char> sep( &c ) ;
   tokenizer tokens( s , sep ) ;
   for ( auto it = tokens.begin( ) ; it != tokens.end( ) ; it++ )
      parts.push_back( *it ) ;
   return parts ;
}

bool myCompare ( const std::string & s1 , const std::string & s2 ) {
   std::string firstcopy( s1 ) ;
   std::string secondcopy ( s2 ) ;
   std::vector<std::string> firstparts( splitOnChar ( firstcopy, '.' ) ) ;
   std::vector<std::string> secondparts( splitOnChar ( secondcopy, '.' ) ) ;
   std::vector<int> numbers1( firstparts.size( ) ) ;
   std::vector<int> numbers2( secondparts.size( ) ) ;
   std::transform( firstparts.begin( ) , firstparts.end( ) , numbers1.begin( ) ,
	 []( std::string st ) { return std::stoi( st , nullptr ) ; } ) ;
   std::transform( secondparts.begin( ) , secondparts.end( ) , numbers2.begin( ) ,
	 []( std::string st ) { return std::stoi( st , nullptr ) ; } ) ;
   auto it1 = numbers1.begin( ) ;
   auto it2 = numbers2.begin( ) ;
   while ( *it1 == *it2 ) {
      it1++ ;
      it2++ ;
   }
   if ( it1 == numbers1.end( )  || it2 == numbers2.end( )  )
      return std::lexicographical_compare( s1.begin( ) , s1.end( ) , s2.begin( ) , s2.end( ) ) ;
   return *it1 < *it2 ;
}

int main( ) {
   std::vector<std::string> arrayOID { "1.3.6.1.4.1.11.2.17.19.3.4.0.10" ,
      "1.3.6.1.4.1.11.2.17.5.2.0.79" ,
      "1.3.6.1.4.1.11.2.17.19.3.4.0.4" ,
      "1.3.6.1.4.1.11150.3.4.0.1" ,
      "1.3.6.1.4.1.11.2.17.19.3.4.0.1" ,
      "1.3.6.1.4.1.11150.3.4.0" } ;
   std::sort( arrayOID.begin( ) , arrayOID.end( ) , myCompare ) ;
   for ( std::string s : arrayOID )
      std::cout << s << '\n' ;
   return 0 ;
}
```

{{out}}

```txt
1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1

```



## Common Lisp


```lisp
(defun oid->list (oid)
  (loop for start = 0 then (1+ pos)
        for pos = (position #\. oid :start start)
        collect (parse-integer oid :start start :end pos)
        while pos))

(defun list< (list1 list2)
  (loop for e1 in list1
        for e2 in list2
        do (cond ((< e1 e2)
                  (return t))
                 ((> e1 e2)
                  (return nil)))
        finally (return (< (length list1) (length list2)))))

(defun sort-oids (oids)
  (sort oids #'list< :key #'oid->list))

(defun main ()
  (let ((oids (list "1.3.6.1.4.1.11.2.17.19.3.4.0.10"
                    "1.3.6.1.4.1.11.2.17.5.2.0.79"
                    "1.3.6.1.4.1.11.2.17.19.3.4.0.4"
                    "1.3.6.1.4.1.11150.3.4.0.1"
                    "1.3.6.1.4.1.11.2.17.19.3.4.0.1"
                    "1.3.6.1.4.1.11150.3.4.0")))
    (dolist (oid (sort-oids oids))
      (write-line oid))))
```

{{out}}

```txt
1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1
```



## Elixir


```elixir
defmodule Sort_by_OID do
  def numbers(list) do
    Enum.sort_by(list, fn oid ->
      String.split(oid, ".") |> Enum.map(&String.to_integer/1)
    end)
  end
end

~w[
  1.3.6.1.4.1.11.2.17.19.3.4.0.10
  1.3.6.1.4.1.11.2.17.5.2.0.79
  1.3.6.1.4.1.11.2.17.19.3.4.0.4
  1.3.6.1.4.1.11150.3.4.0.1
  1.3.6.1.4.1.11.2.17.19.3.4.0.1
  1.3.6.1.4.1.11150.3.4.0
]
|> Sort_by_OID.numbers
|> Enum.each(fn oid -> IO.puts oid end)
```


{{out}}

```txt

1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1

```



## Factor

Factor provides the <code>human<=></code> word which converts numbers in a string to integers before comparing them.

```factor
USING: io qw sequences sorting sorting.human ;

qw{
    1.3.6.1.4.1.11.2.17.19.3.4.0.10
    1.3.6.1.4.1.11.2.17.5.2.0.79
    1.3.6.1.4.1.11.2.17.19.3.4.0.4
    1.3.6.1.4.1.11150.3.4.0.1
    1.3.6.1.4.1.11.2.17.19.3.4.0.1
    1.3.6.1.4.1.11150.3.4.0
} [ human<=> ] sort [ print ] each
```

{{out}}

```txt

1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1

```



## Go


```go
package main

import (
    "fmt"
    "log"
    "math/big"
    "sort"
    "strings"
)

var testCases = []string{
    "1.3.6.1.4.1.11.2.17.19.3.4.0.10",
    "1.3.6.1.4.1.11.2.17.5.2.0.79",
    "1.3.6.1.4.1.11.2.17.19.3.4.0.4",
    "1.3.6.1.4.1.11150.3.4.0.1",
    "1.3.6.1.4.1.11.2.17.19.3.4.0.1",
    "1.3.6.1.4.1.11150.3.4.0",
}

// a parsed representation
type oid []big.Int

// "constructor" parses string representation
func newOid(s string) oid {
    ns := strings.Split(s, ".")
    os := make(oid, len(ns))
    for i, n := range ns {
        if _, ok := os[i].SetString(n, 10); !ok || os[i].Sign() < 0 {
            return nil
        }
    }
    return os
}

// "stringer" formats into string representation
func (o oid) String() string {
    s := make([]string, len(o))
    for i, n := range o {
        s[i] = n.String()
    }
    return strings.Join(s, ".")
}

func main() {
    // parse test cases
    os := make([]oid, len(testCases))
    for i, s := range testCases {
        os[i] = newOid(s)
        if os[i] == nil {
            log.Fatal("invalid OID")
        }
    }
    // sort
    sort.Slice(os, func(i, j int) bool {
        // "less" function must return true if os[i] < os[j]
        oi := os[i]
        for x, v := range os[j] {
            // lexicographic defintion: less if prefix or if element is <
            if x == len(oi) || oi[x].Cmp(&v) < 0 {
                return true
            }
            if oi[x].Cmp(&v) > 0 {
                break
            }
        }
        return false
    })
    // output sorted list
    for _, o := range os {
        fmt.Println(o)
    }
}
```

{{out}}

```txt

1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1

```



## Haskell


### =Data.List=


```Haskell
import Data.List ( sort , intercalate )

splitString :: Eq a => (a) -> [a] -> [[a]]
splitString c [] = []
splitString c s = let ( item , rest ) = break ( == c ) s
                      ( _ , next ) = break ( /= c ) rest
		  in item : splitString c next

convertIntListToString :: [Int] -> String
convertIntListToString = intercalate "." . map show

orderOID :: [String] -> [String]
orderOID = map convertIntListToString . sort . map ( map read . splitString '.' )

oid :: [String]
oid = ["1.3.6.1.4.1.11.2.17.19.3.4.0.10" ,
    "1.3.6.1.4.1.11.2.17.5.2.0.79" ,
    "1.3.6.1.4.1.11.2.17.19.3.4.0.4" ,
    "1.3.6.1.4.1.11150.3.4.0.1" ,
    "1.3.6.1.4.1.11.2.17.19.3.4.0.1" ,
    "1.3.6.1.4.1.11150.3.4.0"]

main :: IO ( )
main = do
   mapM_ putStrLn $ orderOID oid
```


{{out}}

```txt
3.6.1.4.1.11.2.17.5.2.0.79
3.6.1.4.1.11.2.17.19.3.4.0.1
3.6.1.4.1.11.2.17.19.3.4.0.4
3.6.1.4.1.11.2.17.19.3.4.0.10
3.6.1.4.1.11150.3.4.0
3.6.1.4.1.11150.3.4.0.1

```



### =Data.Text=

(To use '''split :: (Char -> Bool) -> Text -> [Text]''' in the standard libraries, we would have to temporarily convert the strings from [Char] to Text with pack and unpack)


```haskell
import Data.Text (pack, split, unpack)
import Data.List (sort, intercalate)

-- SORTING OBJECT IDENTIFIERS ------------------------------------------------
oidSort :: [String] -> [String]
oidSort =
  fmap (intercalate "." . fmap show) .
  sort . fmap (fmap readInt . splitString '.')

-- GENERIC FUNCTIONS ---------------------------------------------------------
splitString :: Char -> String -> [String]
splitString c s = unpack <$> split (c ==) (pack s)

readInt :: String -> Int
readInt xs = read xs :: Int

-- TEST ----------------------------------------------------------------------
main :: IO ()
main =
  mapM_ putStrLn $
  oidSort
    [ "1.3.6.1.4.1.11.2.17.19.3.4.0.10"
    , "1.3.6.1.4.1.11.2.17.5.2.0.79"
    , "1.3.6.1.4.1.11.2.17.19.3.4.0.4"
    , "1.3.6.1.4.1.11150.3.4.0.1"
    , "1.3.6.1.4.1.11.2.17.19.3.4.0.1"
    , "1.3.6.1.4.1.11150.3.4.0"
    ]
```

{{Out}}

```txt
1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1
```


Where Data.List.Split is available (https://hackage.haskell.org/package/split-0.2.3.1/docs/Data-List-Split.html)
we can alternatively write:


```haskell
import Data.List.Split (splitOn)
import Data.List (sort, intercalate)

-- SORTING OBJECT IDENTIFIERS ------------------------------------------------
oidSort :: [String] -> [String]
oidSort =
  fmap (intercalate "." . fmap show) . sort . fmap (fmap readInt . splitOn ".")

readInt :: String -> Int
readInt x = read x :: Int
```



## jq


```jq
def data: [
 "1.3.6.1.4.1.11.2.17.19.3.4.0.10",
 "1.3.6.1.4.1.11.2.17.5.2.0.79",
 "1.3.6.1.4.1.11.2.17.19.3.4.0.4",
 "1.3.6.1.4.1.11150.3.4.0.1",
 "1.3.6.1.4.1.11.2.17.19.3.4.0.1",
 "1.3.6.1.4.1.11150.3.4.0"
 ];

data | map( split(".") | map(tonumber) ) | sort | map(join("."))
```


{{out}}

```txt
[
  "1.3.6.1.4.1.11.2.17.5.2.0.79",
  "1.3.6.1.4.1.11.2.17.19.3.4.0.1",
  "1.3.6.1.4.1.11.2.17.19.3.4.0.4",
  "1.3.6.1.4.1.11.2.17.19.3.4.0.10",
  "1.3.6.1.4.1.11150.3.4.0",
  "1.3.6.1.4.1.11150.3.4.0.1"
]
```



## J


Data:


```J
oids=:<@-.&' ';._2]0 :0
  1.3.6.1.4.1.11.2.17.19.3.4.0.10
  1.3.6.1.4.1.11.2.17.5.2.0.79
  1.3.6.1.4.1.11.2.17.19.3.4.0.4
  1.3.6.1.4.1.11150.3.4.0.1
  1.3.6.1.4.1.11.2.17.19.3.4.0.1
  1.3.6.1.4.1.11150.3.4.0
)
```


In other words, for each line in that script, remove the spaces and put the rest in a box.

Sorting:


```J>
(/: __&".;._1&.('.'&,)&>) oids
1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1
```


In other words, for our sort key, we break the contents of each box by an initial '.' and treat the remainder as numbers.

We also pull the result out of its boxes for display purposes.


## Java

{{works with|Java|8 or higher}}


```java

package com.rosettacode;

import java.util.Comparator;
import java.util.stream.Stream;

public class OIDListSorting {

    public static void main(String[] args) {

        final String dot = "\\.";

        final Comparator<String> oids_comparator = (o1, o2) -> {
            final String[] o1Numbers = o1.split(dot), o2Numbers = o2.split(dot);
            for (int i = 0; ; i++) {
                if (i == o1Numbers.length && i == o2Numbers.length)
                    return 0;
                if (i == o1Numbers.length)
                    return -1;
                if (i == o2Numbers.length)
                    return 1;
                final int nextO1Number = Integer.valueOf(o1Numbers[i]), nextO2Number = Integer.valueOf(o2Numbers[i]);
                final int result = Integer.compare(nextO1Number, nextO2Number);
                if (result != 0)
                    return result;
            }
        };

        Stream.of("1.3.6.1.4.1.11.2.17.19.3.4.0.10", "1.3.6.1.4.1.11.2.17.5.2.0.79", "1.3.6.1.4.1.11.2.17.19.3.4.0.4",
                  "1.3.6.1.4.1.11150.3.4.0.1", "1.3.6.1.4.1.11.2.17.19.3.4.0.1", "1.3.6.1.4.1.11150.3.4.0")
                .sorted(oids_comparator)
                .forEach(System.out::println);
    }
}
```


{{out}}

```txt
1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1
```



## Julia

{{works with|Julia|0.6}}


```julia
oidlist = ["1.3.6.1.4.1.11.2.17.19.3.4.0.10",
           "1.3.6.1.4.1.11.2.17.5.2.0.79",
           "1.3.6.1.4.1.11.2.17.19.3.4.0.4",
           "1.3.6.1.4.1.11150.3.4.0.1",
           "1.3.6.1.4.1.11.2.17.19.3.4.0.1",
           "1.3.6.1.4.1.11150.3.4.0"]

sort!(oidlist; lt=lexless,
    by=x -> parse.(Int, String.(split(x, "."))))
println.(oidlist)
```


{{out}}

```txt
1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1
```



## Kotlin


```scala
// version 1.0.6

class Oid(val id: String): Comparable<Oid> {
    override fun compareTo(other: Oid): Int {
        val splits1 = this.id.split('.')
        val splits2 = other.id.split('.')
        val minSize = if (splits1.size < splits2.size) splits1.size else splits2.size
        for (i in 0 until minSize) {
            if (splits1[i].toInt() < splits2[i].toInt()) return -1
            else if (splits1[i].toInt() > splits2[i].toInt()) return 1
        }
        return splits1.size.compareTo(splits2.size)
    }

    override fun toString() = id
}

fun main(args: Array<String>) {
    val oids = arrayOf(
        Oid("1.3.6.1.4.1.11.2.17.19.3.4.0.10"),
        Oid("1.3.6.1.4.1.11.2.17.5.2.0.79"),
        Oid("1.3.6.1.4.1.11.2.17.19.3.4.0.4"),
        Oid("1.3.6.1.4.1.11150.3.4.0.1"),
        Oid("1.3.6.1.4.1.11.2.17.19.3.4.0.1"),
        Oid("1.3.6.1.4.1.11150.3.4.0")
    )
    println(oids.sorted().joinToString("\n"))
}
```


{{out}}

```txt

1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1

```



## Lua

Using the in-built table.sort with a custom compare function.

```Lua
local OIDs = {
    "1.3.6.1.4.1.11.2.17.19.3.4.0.10",
    "1.3.6.1.4.1.11.2.17.5.2.0.79",
    "1.3.6.1.4.1.11.2.17.19.3.4.0.4",
    "1.3.6.1.4.1.11150.3.4.0.1",
    "1.3.6.1.4.1.11.2.17.19.3.4.0.1",
    "1.3.6.1.4.1.11150.3.4.0"
}

function compare (a, b)
    local aList, bList, Na, Nb = {}, {}
    for num in a:gmatch("%d+") do table.insert(aList, num) end
    for num in b:gmatch("%d+") do table.insert(bList, num) end
    for i = 1, math.max(#aList, #bList) do
        Na, Nb = tonumber(aList[i]) or 0, tonumber(bList[i]) or 0
        if Na ~= Nb then return Na < Nb end
    end
end

table.sort(OIDs, compare)
for _, oid in pairs(OIDs) do print(oid) end
```

{{out}}

```txt
1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1
```


### Using Coroutine


```lua

local function oidGen(s)
  local wrap, yield = coroutine.wrap, coroutine.yield
  return wrap(function()
    for n in s:gmatch"%d+"do yield(tonumber(n))end
  end)
end

local function oidCmp(a,b)
  local agen,bgen = oidGen(a),oidGen(b)
  local n,m = agen(),bgen()
  while n and m do
    if n~=m then return n<m end
    n,m = agen(),bgen()
  end
  return m and true or false -- bgen longer with previous equal
end

local OIDs = {
    "1.3.6.1.4.1.11.2.17.19.3.4.0.10",
    "1.3.6.1.4.1.11.2.17.5.2.0.79",
    "1.3.6.1.4.1.11.2.17.19.3.4.0.4",
    "1.3.6.1.4.1.11150.3.4.0.1",
    "1.3.6.1.4.1.11.2.17.19.3.4.0.1",
    "1.3.6.1.4.1.11150.3.4.0"
}

table.sort(OIDs, oidCmp)
for _, oid in pairs(OIDs) do print(oid) end

```



## M2000 Interpreter

In this example we have to change dot to #, to make each number as an integer one.

```M2000 Interpreter

Module CheckIt {
      Flush ' empty stack of values
      Data "1.3.6.1.4.1.11.2.17.19.3.4.0.4" ,  "1.3.6.1.4.1.11.2.17.19.3.4.0.1",  "1.3.6.1.4.1.11150.3.4.0.1"
      Data "1.3.6.1.4.1.11.2.17.19.3.4.0.10", "1.3.6.1.4.1.11.2.17.5.2.0.79",  "1.3.6.1.4.1.11150.3.4.0"
      \\ Inventories of type queue can get same keys, and have sort where numbers (float type) as part of key count as numbers
      Inventory queue OID
      \\ prepare keys (replace dot to #)
      While not empty {
            Append OID, Replace$(".","#", letter$)
      }
      Sort Ascending OID
      n=Each(OID)
      a$=""
      While n {
            \\ replace # to dot
            a$+=Replace$("#",".", Eval$(n))+{
            }
      }
      Clipboard a$
      Report a$
}
Checkit

```

{{out}}

```txt

1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1

```

===Using Piece$() and Stack Sort===
We use a stack (a linked list) to save numbers, and a function to check piece by piece for "Grater than" only

piece$(a$,".")(i) works from 0

piece$(a$,".", i) works from 1

piece$(a$,".") export a pointer to an array with each piece on it



```M2000 Interpreter

GT=lambda (a$, b$)->{
      def i
      do {
            m$=piece$(a$,".")(i)
            n$=piece$(b$,".")(i)
            i++
      } until n$="" or m$="" or m$<>n$
      if n$="" then =m$<>"":exit
      if m$="" then =False:exit
      =val(m$)>val(n$)
}
Stack new {
      \\ data push to end of stack (we use it as FIFO)
      data "1.3.6.1.4.1.11.2.17.19.3.4.0.10"
      data "1.3.6.1.4.1.11.2.17.5.2.0.79"
      data "1.3.6.1.4.1.11.2.17.19.3.4.0.4"
      data "1.3.6.1.4.1.11150.3.4.0.1"
      data "1.3.6.1.4.1.11.2.17.19.3.4.0.1"
      data "1.3.6.1.4.1.11150.3.4.0"
      M=Stack.Size-1
      While M>0 {
                  N=1
                  For i=1 to M {
                        \\ if peek item i > peek item i+1 then get i+1 to top, and send to i
                        \\ stack is a linked list, so moving items done with pointers only
                        if Gt(stackitem$(i), stackitem$(i+1)) then Shift i+1 : ShiftBack i : N=i
                  }
                  M=N-1
      }
      While not empty {
            Print Letter$
      }
}

```


Using a function which split pieces one time. We have to insert one more item, by append a "." to a$ and b$

```M2000 Interpreter

      GT=lambda (a$, b$)->{
            def i=-1
            dim Base 0,  a$(), b$()
            a$()=piece$(a$+".", ".")
            b$()=piece$(b$+".", ".")
            do {
                  i++
            } until a$(i)="" or b$(i)="" or a$(i)<>b$(i)
            if  b$(i)="" then =a$(i)<>"":exit
            if a$(i)="" then =False:exit
            =val(a$(i))>val(b$(i))
      }

```



### Using QuickSort

We can make any OID as array of numbers and we can put all OIDs in an array to sort by a custom Quick Sort, where we place the compare function as a lambda function. Swaps made to pointers of arrays of OIDs. There is no need to use PIECE$() in compare function.

Note that QuickSort need Lower or Equal

There is a Let numeric_expression = string_expression
Normally this numeric_expression = string_expression  is a syntax error,
but Let is a two part statement, a Push to stack and a Read from stack: Push string_expression : Read numeric_expression
So first executed the string expression which return a pointer to array and then the read statement get this pointer;



```M2000 Interpreter

Group Quick {
Private:
      Function partition {
               Read &A(), p, r
               x = A(r)
               i = p-1
               For j=p to r-1 {
                   If .LE(A(j), x) Then {
                          i++
                          Swap A(i),A(j)
                       }
                }
                Swap A(i+1),A(r)
               = i+1
            }
Public:
      LE=Lambda->False
      Function quicksort {
           Read &A(), p, r
           If p < r Then {
             q = .partition(&A(), p, r)
             Call .quicksort(&A(), p, q - 1)
             Call .quicksort(&A(), q + 1, r)
          }
      }
}
\\ no easy way to join ;
\\ n^ is the cursor from iterator n
Function join$(a$()) {
      n=each(a$(), 1, -2)
      k$=""
      while n {
            overwrite k$, ".", n^:=array$(n)
      }
      =k$
}
Stack New {
            Data "1.3.6.1.4.1.11.2.17.19.3.4.0.4" ,  "1.3.6.1.4.1.11.2.17.19.3.4.0.1",  "1.3.6.1.4.1.11150.3.4.0.1"
            Data "1.3.6.1.4.1.11.2.17.19.3.4.0.10", "1.3.6.1.4.1.11.2.17.5.2.0.79",  "1.3.6.1.4.1.11150.3.4.0"
            Dim  Base 0, arr(Stack.Size)
            link arr() to arr$()
            i=0 :  While not Empty {arr$(i)=piece$(letter$+".", ".") : i++ }
}

For i=0 to len(arr())-1 {
      Print join$(arr(i))
}
Quick.LE=lambda (a, b)->{
      Link a, b to a$(), b$()
       def i=-1
       do {
             i++
       } until a$(i)="" or b$(i)="" or a$(i)<>b$(i)
       if  b$(i)="" then =a$(i)="":exit
       if a$(i)="" then =true:exit
       =val(a$(i))<=val(b$(i))
}
Call Quick.quicksort(&arr(), 0, Len(arr())-1)
For i=0 to len(arr())-1 {
      Print join$(arr(i))
}


```



## Perl



```perl
my @OIDs = qw(
    1.3.6.1.4.1.11.2.17.19.3.4.0.10
    1.3.6.1.4.1.11.2.17.5.2.0.79
    1.3.6.1.4.1.11.2.17.19.3.4.0.4
    1.3.6.1.4.1.11150.3.4.0.1
    1.3.6.1.4.1.11.2.17.19.3.4.0.1
    1.3.6.1.4.1.11150.3.4.0
);

my @sorted =
    map { $_->[0] }
    sort { $a->[1] cmp $b->[1] }
    map { [$_, join '', map { sprintf "%8d", $_ } split /\./, $_] }
    @OIDs;

print "$_\n" for @sorted;
```


{{out}}

```txt

1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1

```


Alternately, you can sort them as "version strings", which is a Perl syntax allowing you to specify a character string in the source code with the characters' codes specified as a dot-delimited sequence of integers.


```perl
my @sorted =
    map { $_->[0] }
    sort { $a->[1] cmp $b->[1] }
    map { [$_, eval "v$_"] }
    @OIDs;
```



## Perl 6


The <tt>sort</tt> routine accepts a sort key callback as the first argument. Here we generate a list of integers as the sort key for each OID, which gets sorted lexicographically with numeric comparison by default.


```perl6
.say for sort *.comb(/\d+/)».Int, <
    1.3.6.1.4.1.11.2.17.19.3.4.0.10
    1.3.6.1.4.1.11.2.17.5.2.0.79
    1.3.6.1.4.1.11.2.17.19.3.4.0.4
    1.3.6.1.4.1.11150.3.4.0.1
    1.3.6.1.4.1.11.2.17.19.3.4.0.1
    1.3.6.1.4.1.11150.3.4.0
>;
```


{{out}}

```txt

1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1

```


Alternatively, using the <tt>sprintf</tt>-based approach used by the Perl solution, for comparison ''(input elided)'':


```perl6
.say for sort *.split('.').fmt('%08d'), <...>;
```


Or if using a third-party module is acceptable:


```Perl6
use Sort::Naturally;

.say for sort &naturally, <...>;
```



## Phix

I would normally recommend a tagsort, but we can avoid the extra routine and tagset here.

```Phix
sequence strings = {"1.3.6.1.4.1.11.2.17.19.3.4.0.10",
                    "1.3.6.1.4.1.11.2.17.5.2.0.79",
                    "1.3.6.1.4.1.11.2.17.19.3.4.0.4",
                    "1.3.6.1.4.1.11150.3.4.0.1",
                    "1.3.6.1.4.1.11.2.17.19.3.4.0.1",
                    "1.3.6.1.4.1.11150.3.4.0"}

constant len = length(strings)
sequence sortable = repeat(0,len)

for i=1 to len do
    sequence si = split(strings[i],'.')
    for j=1 to length(si) do
        si[j] = to_number(si[j])
    end for
    sortable[i] = {si,i}
end for
sortable = sort(sortable)
for i=1 to len do
    ?strings[sortable[i][2]]
end for
```


```txt

"1.3.6.1.4.1.11.2.17.5.2.0.79"
"1.3.6.1.4.1.11.2.17.19.3.4.0.1"
"1.3.6.1.4.1.11.2.17.19.3.4.0.4"
"1.3.6.1.4.1.11.2.17.19.3.4.0.10"
"1.3.6.1.4.1.11150.3.4.0"
"1.3.6.1.4.1.11150.3.4.0.1"

```



## PicoLisp


```PicoLisp
(for I
   (by
      '((L) (mapcar format (split (chop L) ".")))
      sort
      (quote
         "1.3.6.1.4.1.11.2.17.19.3.4.0.10"
         "1.3.6.1.4.1.11.2.17.5.2.0.79"
         "1.3.6.1.4.1.11.2.17.19.3.4.0.4"
         "1.3.6.1.4.1.11150.3.4.0.1"
         "1.3.6.1.4.1.11.2.17.19.3.4.0.1"
         "1.3.6.1.4.1.11150.3.4.0" ) )
   (prinl I) )
```

{{out}}

```txt
1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1
```



## Python


We need to split the input and map each part to int otherwise elements gets compared as a string

```Python

data = [
    '1.3.6.1.4.1.11.2.17.19.3.4.0.10',
    '1.3.6.1.4.1.11.2.17.5.2.0.79',
    '1.3.6.1.4.1.11.2.17.19.3.4.0.4',
    '1.3.6.1.4.1.11150.3.4.0.1',
    '1.3.6.1.4.1.11.2.17.19.3.4.0.1',
    '1.3.6.1.4.1.11150.3.4.0'
]

for s in sorted(data, key=lambda x: list(map(int, x.split('.')))):
    print(s)

```



## Racket


```racket
#lang racket
(require data/order)

;; allows for key caching
(define (oid->oid-key o)
  (map string->number (string-split o ".")))

(define oid-key< (order-<? datum-order))

(module+ test
  (require rackunit)
  (check-equal?
   (sort
    '("1.3.6.1.4.1.11.2.17.19.3.4.0.10"
      "1.3.6.1.4.1.11.2.17.5.2.0.79"
      "1.3.6.1.4.1.11.2.17.19.3.4.0.4"
      "1.3.6.1.4.1.11150.3.4.0.1"
      "1.3.6.1.4.1.11.2.17.19.3.4.0.1"
      "1.3.6.1.4.1.11150.3.4.0")
    oid-key<
    #:key oid->oid-key
    #:cache-keys? #t)
   '("1.3.6.1.4.1.11.2.17.5.2.0.79"
     "1.3.6.1.4.1.11.2.17.19.3.4.0.1"
     "1.3.6.1.4.1.11.2.17.19.3.4.0.4"
     "1.3.6.1.4.1.11.2.17.19.3.4.0.10"
     "1.3.6.1.4.1.11150.3.4.0"
     "1.3.6.1.4.1.11150.3.4.0.1")))
```

Tests run with no output, indicating success.


## REXX

This REXX version supports negative integers in the OID.

```rexx
/*REXX program performs a  sort  of  OID  (Object IDentifiers ◄── used in Network data).*/
          $= 1.3.6.1.4.1.11.2.17.19.3.4.0.10 ,   /* ◄──┐                                */
             1.3.6.1.4.1.11.2.17.5.2.0.79    ,   /* ◄──┤                                */
             1.3.6.1.4.1.11.2.17.19.3.4.0.4  ,   /* ◄──┼─◄─ six OID numbers (as a list).*/
             1.3.6.1.4.1.11150.3.4.0.1       ,   /* ◄──┤                                */
             1.3.6.1.4.1.11.2.17.19.3.4.0.1  ,   /* ◄──┤                                */
             1.3.6.1.4.1.11150.3.4.0             /* ◄──┘                                */
call gen                                         /*generate an array (@.) from the OIDs.*/
call show   'before sort ───► '                  /*display the  @  array before sorting.*/
     say copies('░', 79)                         /*display fence, separate before &after*/
call adj 1;   call bSort #;       call adj 0     /*expand/sort/shrink the internal OID's*/
call show   ' after sort ───► '                  /*display the  @  array after sorting. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
bSort: procedure expose @.; parse arg n;  m=n-1  /*N: is the number of @ array elements.*/
         do m=m  for m  by -1  until ok;   ok=1  /*keep sorting the @ array until done. */
            do j=1  for m;  _=j+1;  if @.j>@._  then parse value @.j @._ 0 with @._ @.j ok
            end   /*j*/                          /* [↑]  swap two out─of─order elements.*/
         end      /*m*/;          return         /* [↑]  use a simple  bubble  sort.    */
/*──────────────────────────────────────────────────────────────────────────────────────*/
gen:   #=words($); L=length(#);      do i=1  for #;     @.i=word($,i);    end;      return
                       /*length of the  number  of words in $.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
adj:   arg LZ;  do j=1  for #;       x=translate(@.j, , .);  y=  /*construct X version. */
                   do k=1  for words(x);           _=word(x, k)  /*get a number in  X.  */
                   if LZ  then y=y right(_,90,0);  else y=y  _+0 /*add│elide leading 0's*/
                   end   /*k*/                                   /*adjust number, append*/
                @.j = translate( space(y), ., ' ')               /*reconstitute number. */
                end      /*j*/                                   /*LZ: Leading Zero(s). */
       return                                                    /*──  ─       ─        */
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:  do a=1  for #;  say right("OID number",20)  right(a,L)  arg(1)  @.a;   end;  return
```

{{out|output|text=  when using the (internal) default input:}}

```txt

          OID number 1 before sort ───►  1.3.6.1.4.1.11.2.17.19.3.4.0.10
          OID number 2 before sort ───►  1.3.6.1.4.1.11.2.17.5.2.0.79
          OID number 3 before sort ───►  1.3.6.1.4.1.11.2.17.19.3.4.0.4
          OID number 4 before sort ───►  1.3.6.1.4.1.11150.3.4.0.1
          OID number 5 before sort ───►  1.3.6.1.4.1.11.2.17.19.3.4.0.1
          OID number 6 before sort ───►  1.3.6.1.4.1.11150.3.4.0
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
          OID number 1  after sort ───►  1.3.6.1.4.1.11.2.17.5.2.0.79
          OID number 2  after sort ───►  1.3.6.1.4.1.11.2.17.19.3.4.0.1
          OID number 3  after sort ───►  1.3.6.1.4.1.11.2.17.19.3.4.0.4
          OID number 4  after sort ───►  1.3.6.1.4.1.11.2.17.19.3.4.0.10
          OID number 5  after sort ───►  1.3.6.1.4.1.11150.3.4.0
          OID number 6  after sort ───►  1.3.6.1.4.1.11150.3.4.0.1

```



## Ring



```Ring


/*
 +--------------------------------------------------------------
 +        Program Name : SortOIDNumeric.ring
 +        Date         : 2016-07-14
 +        Author       : Bert Mariani
 +        Purpose      : Sort OID List in Numeric Order
 +--------------------------------------------------------------
*/

oldOidList =
[
	".1.3.6.1.4.1.11.2.17.19.3.4.0.10",
	".1.3.6.1.4.1.11.2.17.5.2.0.79",
	".1.3.6.1.4.1.11.2.17.19.3.4.0.4",
	".1.3.6.1.4.1.11150.3.4.0.1",
	".1.3.6.1.4.1.11.2.17.19.3.4.0.1",
	".1.3.6.1.4.1.11150.3.4.0"
]

        ### SHOW BEFORE SORT
        See nl + "oldOIDList Before Sort" +nl
        See  oldOidList

    #---------------------

     delChar = "."
     nulChar = ""
     padChar = " "
     padSize = 6
     newDotPadList = []

    ### Split list into lines
    for line in oldOidList

        ### Split line by . into components
        noDotList  = str2list( substr(line, delChar, nl) )

        ### Pad components with left blanks to make equal size
        newPadList  = PadStringList(noDotList, padChar, padSize)

        ### Join the components back to a line
        newDotPadString  = JoinStringList(delChar, newPadList)

        ### Create new list - Alpha
        Add(newDotPadList, newDotPadString)
    next

    ### Sorts Alpha list
    newDotPadListSorted = sort(newDotPadList)

         ### SHOW ALPHA INTERMEDIATE OUTPUT
         See nl + "newDotPadListSorted Intermediate Sort" +nl
         See  newDotPadListSorted

    ### Remove blanks for original look
    newOidList = RemovePadCharList( newDotPadListSorted, padChar, nulChar)

    ###--------------------

        ### SHOW AFTER SORT - NUMERIC
        See nl + "newOIDList Final Sort" +nl
        See  newOidList


###--------------------------------------------------------------------
### Function: PadStringList
###         newList = PadStringList(oldList, padChar, padSize )
###--------------------------------------------------------------------

Func PadStringList oldList, padChar, padSize
    newList = []
    for line in oldList
        newPadSize = padSize - len(line)
        newLine = Copy( padChar, newPadSize) + line
        Add(newList, newLine)
    next

    ### First line in all blank because of leading dot - remove
    Del(newList,1)
return newList

###------------------------------------------------------------
### FUNC JoinStringList
###         newString = JoinStringList( joinChar, oldList)
###------------------------------------------------------------

Func JoinStringList joinChar, OldList
    newString = ""
    for line in OldList
        newString = newString + joinChar + line
    next
return newString

###---------------------------------------------------------------------
### FUNC RemovePadCharList
###         newOidList = RemovePadCharList( oldList, padChar, nulChar)
###---------------------------------------------------------------------

Func RemovePadCharList oldList, padChar, nulChar
    newList = []
    for line in oldList
          noPadString = substr(line, padChar, nulChar)
        Add(newList, noPadString)
    next
return newList
###-----------------------------------------------------------

>;
```

{{out}}

```txt


newOIDList Final Sort
.1.3.6.1.4.1.11.2.17.5.2.0.79
.1.3.6.1.4.1.11.2.17.19.3.4.0.1
.1.3.6.1.4.1.11.2.17.19.3.4.0.4
.1.3.6.1.4.1.11.2.17.19.3.4.0.10
.1.3.6.1.4.1.11150.3.4.0
.1.3.6.1.4.1.11150.3.4.0.1


```



## Ruby


```ruby
%w[
  1.3.6.1.4.1.11.2.17.19.3.4.0.10
  1.3.6.1.4.1.11.2.17.5.2.0.79
  1.3.6.1.4.1.11.2.17.19.3.4.0.4
  1.3.6.1.4.1.11150.3.4.0.1
  1.3.6.1.4.1.11.2.17.19.3.4.0.1
  1.3.6.1.4.1.11150.3.4.0
]
.sort_by{|oid| oid.split(".").map(&:to_i)}
.each{|oid| puts oid}
```


{{out}}

```txt

1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1

```

Or, using the Gem module (which knows about versions):

```ruby
puts %w[
  1.3.6.1.4.1.11.2.17.19.3.4.0.10
  1.3.6.1.4.1.11.2.17.5.2.0.79
  1.3.6.1.4.1.11.2.17.19.3.4.0.4
  1.3.6.1.4.1.11150.3.4.0.1
  1.3.6.1.4.1.11.2.17.19.3.4.0.1
  1.3.6.1.4.1.11150.3.4.0
].sort_by{|oid| Gem::Version.new(oid) }
```

with identical output.


## Rust


```Rust
fn split(s: &str) -> impl Iterator<Item = u64> + '_ {
    s.split('.').map(|x| x.parse().unwrap())
}

fn main() {
    let mut oids = vec![
        "1.3.6.1.4.1.11.2.17.19.3.4.0.10",
        "1.3.6.1.4.1.11.2.17.5.2.0.79",
        "1.3.6.1.4.1.11.2.17.19.3.4.0.4",
        "1.3.6.1.4.1.11150.3.4.0.1",
        "1.3.6.1.4.1.11.2.17.19.3.4.0.1",
        "1.3.6.1.4.1.11150.3.4.0",
    ];

    oids.sort_by(|a, b| Iterator::cmp(split(a), split(b)));

    println!("{:#?}", oids);
}
```

{{out}}

```txt
[
    "1.3.6.1.4.1.11.2.17.5.2.0.79",
    "1.3.6.1.4.1.11.2.17.19.3.4.0.1",
    "1.3.6.1.4.1.11.2.17.19.3.4.0.4",
    "1.3.6.1.4.1.11.2.17.19.3.4.0.10",
    "1.3.6.1.4.1.11150.3.4.0",
    "1.3.6.1.4.1.11150.3.4.0.1"
]
```



## Sidef


```ruby
func sort_OIDs(ids) {
    ids.sort_by { |id|
        id.split('.').map { Num(_) }
    }
}

var OIDs = %w(
    1.3.6.1.4.1.11.2.17.19.3.4.0.10
    1.3.6.1.4.1.11.2.17.5.2.0.79
    1.3.6.1.4.1.11.2.17.19.3.4.0.4
    1.3.6.1.4.1.11150.3.4.0.1
    1.3.6.1.4.1.11.2.17.19.3.4.0.1
    1.3.6.1.4.1.11150.3.4.0
)

sort_OIDs(OIDs).each { .say }
```

{{out}}

```txt

1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1

```



## Swift



```swift
import Foundation

public struct OID {
  public var val: String

  public init(_ val: String) {
    self.val = val
  }
}

extension OID: CustomStringConvertible {
  public var description: String {
    return val
  }
}

extension OID: Comparable {
  public static func < (lhs: OID, rhs: OID) -> Bool {
    let split1 = lhs.val.components(separatedBy: ".").compactMap(Int.init)
    let split2 = rhs.val.components(separatedBy: ".").compactMap(Int.init)
    let minSize = min(split1.count, split2.count)

    for i in 0..<minSize {
      if split1[i] < split2[i] {
        return true
      } else if split1[i] > split2[i] {
        return false
      }
    }

    return split1.count < split2.count
  }

  public static func == (lhs: OID, rhs: OID) -> Bool {
    return lhs.val == rhs.val
  }
}

let ids = [
  "1.3.6.1.4.1.11.2.17.19.3.4.0.10",
  "1.3.6.1.4.1.11.2.17.5.2.0.79",
  "1.3.6.1.4.1.11.2.17.19.3.4.0.4",
  "1.3.6.1.4.1.11150.3.4.0.1",
  "1.3.6.1.4.1.11.2.17.19.3.4.0.1",
  "1.3.6.1.4.1.11150.3.4.0"
].map(OID.init)

for id in ids.sorted() {
  print(id)
}
```


{{out}}


```txt
1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1
```



## Tcl


```Tcl

# Example input data:
set oid_list [list \
                  1.3.6.1.4.1.11.2.17.19.3.4.0.10 \
                  1.3.6.1.4.1.11.2.17.5.2.0.79 \
                  1.3.6.1.4.1.11.2.17.19.3.4.0.4 \
                  1.3.6.1.4.1.11150.3.4.0.1 \
                  1.3.6.1.4.1.11.2.17.19.3.4.0.1 \
                  1.3.6.1.4.1.11150.3.4.0 ]
set oid2_lists [list ]
set dots_max 0
set i 0
foreach oid $oid_list {
    set oid_list [split $oid "."]
    set dot_count [llength $oid_list]
    incr dot_count -1
    if { $dot_count > $dots_max } {
        set dots_max $dot_count
    }
    set dots_arr(${i}) $dot_count
    lappend oid2_lists $oid_list
    incr i
}
# pad for strings of different dot counts
set oid3_lists [list]
for {set ii 0} {$ii < $i} {incr ii} {
    set oid_list [lindex $oid2_lists $ii]
    set add_fields [expr { $dots_max - $dots_arr(${ii}) } ]
    if { $add_fields > 0 } {
        for {set j 0} {$j < $add_fields} {incr j} {
            lappend oid_list -1
        }
    }
    lappend oid3_lists $oid_list
}
for {set n $dots_max} {$n >= 0 } {incr n -1} {
    set oid3_lists [lsort -integer -index $n -increasing $oid3_lists]
}
# unpad strings of different dot counts
set oid4_lists [list]
for {set ii 0} {$ii < $i} {incr ii} {
    set oid_list [lindex $oid3_lists $ii]
    set j [lsearch -exact -integer $oid_list -1]
    if { $j > -1 } {
        set oid2_list [lrange $oid_list 0 ${j}-1]
        lappend oid4_lists $oid2_list
    } else {
        lappend oid4_lists $oid_list
    }
}
foreach oid_list $oid4_lists {
    puts [join $oid_list "."]
}

```


{{out}}

```txt

% source sort-a-list-of-oids.tcl
1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1

```




## zkl

Translation of http://rosettacode.org/wiki/Natural_sorting#zkl

Basically, blow apart each line into a list of numbers and sort that.

```zkl
fcn sortOIDS(oids){  // oids is not modified, a new list is created
   // pad each oid with a terminal (-1) so zip won't short cut
   oids=oids.pump(List(),fcn(oid){ (oid + ".-1").split(".").apply("toInt") });
   oids.sort(  // in place sort
      fcn(a,b){ // a & b are (x,y,z,...-1), eg (0,4,2,54,-1), (4,6,-1)
	 a.zip(b).reduce(fcn(_,[(a,b)]){  // if one list longer, zip truncates
	    if(a==b) return(True);	 // continue to next field
	    return(Void.Stop,a<b);	// OIDa<OIDb == cmp this field
	 },True);
      });
   oids.pump(List,fcn(list){ list[0,-1].concat(".") }) // back to strings
}
```


```zkl
oids:=List(
   "1.3.6.1.4.1.11.2.17.19.3.4.0.10",
   "1.3.6.1.4.1.11.2.17.5.2.0.79",
   "1.3.6.1.4.1.11.2.17.19.3.4.0.4",
   "1.3.6.1.4.1.11150.3.4.0.1",
   "1.3.6.1.4.1.11.2.17.19.3.4.0.1",
   "1.3.6.1.4.1.11150.3.4.0");
oids=sortOIDS(oids);
oids.pump(Console.println);  // print one OID per line
```

{{out}}

```txt

1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1

```

