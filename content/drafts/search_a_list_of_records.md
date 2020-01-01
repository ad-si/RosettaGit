+++
title = "Search a list of records"
description = ""
date = 2019-10-18T19:18:34Z
aliases = []
[extra]
id = 19635
[taxonomies]
categories = []
tags = []
+++

{{Task|List processing}}

Many programming languages provide convenient ways to look for a known value in a simple list of strings or numbers.

But what if the elements of the list are themselves compound records/objects/data-structures, and the search condition is more complex than a simple equality test?

{{task heading}}

Write a function/method/etc. that can find the first element in a given list matching a given condition.

It should be as generic and reusable as possible.

(Of course if your programming language already provides such a feature, you can use that instead of recreating it.)

Then to demonstrate its functionality, create the data structure specified under [[#Data set]], and perform on it the searches specified under [[#Test cases]].

{{task heading|Data set}}

The data structure to be used contains the names and populations (in millions) of the 10 largest metropolitan areas in Africa, and looks as follows when represented in JSON:


```JavaScript
[
  { "name": "Lagos",                "population": 21.0  },
  { "name": "Cairo",                "population": 15.2  },
  { "name": "Kinshasa-Brazzaville", "population": 11.3  },
  { "name": "Greater Johannesburg", "population":  7.55 },
  { "name": "Mogadishu",            "population":  5.85 },
  { "name": "Khartoum-Omdurman",    "population":  4.98 },
  { "name": "Dar Es Salaam",        "population":  4.7  },
  { "name": "Alexandria",           "population":  4.58 },
  { "name": "Abidjan",              "population":  4.4  },
  { "name": "Casablanca",           "population":  3.98 }
]
```


However, you shouldn't parse it from JSON, but rather represent it natively in your programming language.

* The top-level data structure should be an '''ordered''' collection ''(i.e. a list, array, vector, or similar)''.
* Each element in this list should be an '''associative''' collection that maps from keys to values ''(i.e. a struct, object, hash map, dictionary, or similar)''.
* Each of them has two entries: One string value with key "<tt>name</tt>", and one numeric value with key "<tt>population</tt>".
* You may rely on the list being sorted by population count, as long as you explain this to readers.



If any of that is impossible or unreasonable in your programming language, then feel free to deviate, as long as you explain your reasons in a comment above your solution.

{{task heading|Test cases}}

{| class="wikitable"
|-
! Search
! Expected result
|-
| Find the (zero-based) '''index''' of the first city in the list whose '''name is "<tt>Dar Es Salaam</tt>"'''
| <tt>6</tt>
|-
| Find the '''name''' of the first city in this list whose '''population is less than 5 million'''
| <tt>Khartoum-Omdurman</tt>
|-
| Find the '''population''' of the first city in this list whose '''name starts with the letter "<tt>A</tt>"'''
| <tt>4.58</tt>
|}

{{task heading|Guidance}}

If your programming language supports [[wp:Higher-order programming|higher-order programming]], then the most elegant way to implement the requested functionality in a generic and reusable way, might be to write a function (maybe called "<tt>find_index</tt>" or similar), that takes two arguments:
# The list to search through.
# A function/lambda/closure (the so-called "predicate"), which will be applied in turn to each element in the list, and whose boolean return value defines whether that element matches the search requirement.

If this is not the approach which would be most natural or idiomatic in your language, explain why, and show what is.

{{task heading|Related tasks}}

* [[Search a list]]

<hr>


## 8th

8th uses JSON as its native data representation, so using it is quite natural:

```forth
[
  { "name": "Lagos",                "population": 21.0  },
  { "name": "Cairo",                "population": 15.2  },
  { "name": "Kinshasa-Brazzaville", "population": 11.3  },
  { "name": "Greater Johannesburg", "population":  7.55 },
  { "name": "Mogadishu",            "population":  5.85 },
  { "name": "Khartoum-Omdurman",    "population":  4.98 },
  { "name": "Dar Es Salaam",        "population":  4.7  },
  { "name": "Alexandria",           "population":  4.58 },
  { "name": "Abidjan",              "population":  4.4  },
  { "name": "Casablanca",           "population":  3.98 }
] var, cities-raw

"Index of first occurrence of 'Dar Es Salaam': " .
"Dar Es Salaam" >r cities-raw @
(
  "name" m:@ r@ s:= if
    drop . cr ;;
  then
  2drop
) a:each drop rdrop

"The name of the first city in this list whose population is less than 5 million: " .
5 >r cities-raw @
(
  nip
  "population" m:@ r@ n:< if
    "name" m:@ . cr break
  then
  drop
) a:each drop rdrop

"The population of the first city in this list whose name starts with the letter \"A\": " .
'A >r cities-raw @
(
  nip
  "name" m:@ 0 s:@ r@ n:= if
    drop "population" m:@ . cr break
  then
  2drop
) a:each drop rdrop

bye
```

{{out}}

```txt

Index of first occurrence of 'Dar Es Salaam': 6
The name of the first city in this list whose population is less than 5 million: Khartoum-Omdurman
The population of the first city in this list whose name starts with the letter "A": 4.58000

```



## Ada


This solution in inspired by how the <code>Ada.Containers</code> child packages work. Usually a <code>Cursor</code> contains two accesses: one to the container and one to the element. Since we want to get the index of the element as well, the index was stored instead.

{{works with|Ada|Ada|2005}}


```Ada
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

procedure Search_A_List_Of_Records
is
   function "+"(input : in String) return Unbounded_String renames To_Unbounded_String;
   function "+"(input : in Unbounded_String) return String renames To_String;

   type City is record
      name : Unbounded_String;
      population : Float;
   end record;

   type City_Array is array(Positive range <>) of City;
   type City_Array_Access is access City_Array;

   type Cursor is record
      container : City_Array_Access;
      index : Natural;
   end record;

   function Element(C : in Cursor) return City is
   begin
      if C.container = null or C.index = 0 then
         raise Constraint_Error with "No element.";
      end if;

      return C.container.all(C.index);
   end Element;

   function Index_0(C : in Cursor) return Natural is
   begin
      if C.container = null or C.index = 0 then
         raise Constraint_Error with "No element.";
      end if;

      return C.index - C.container.all'First;
   end Index_0;

   function Find
     (container : in City_Array;
      check : not null access function(Element : in City) return Boolean)
      return Cursor
   is
   begin
      for I in container'Range loop
         if check.all(container(I)) then
            return (new City_Array'(container), I);
         end if;
      end loop;
      return (null, 0);
   end;

   function Dar_Es_Salaam(Element : in City) return Boolean is
   begin
      return Element.name = "Dar Es Salaam";
   end Dar_Es_Salaam;

   function Less_Than_Five_Million(Element : in City) return Boolean is
   begin
      return Element.population < 5.0;
   end Less_Than_Five_Million;

   function Starts_With_A(Item : in City) return Boolean is
   begin
      return Element(Item.name, 1) = 'A';
   end Starts_With_A;

   cities : constant City_Array :=
     ((+"Lagos",                21.0),
      (+"Cairo",                15.2),
      (+"Kinshasa-Brazzaville", 11.3),
      (+"Greater Johannesburg", 7.55),
      (+"Mogadishu",            5.85),
      (+"Khartoum-Omdurman",    4.98),
      (+"Dar Es Salaam",        4.7 ),
      (+"Alexandria",           4.58),
      (+"Abidjan",              4.4 ),
      (+"Casablanca",           3.98));
begin
   Ada.Text_IO.Put_Line(Index_0(Find(cities, Dar_Es_Salaam'Access))'Img);
   Ada.Text_IO.Put_Line(+Element(Find(cities, Less_Than_Five_Million'Access)).name);
   Ada.Text_IO.Put_Line(Element(Find(cities, Starts_With_A'Access)).population'Img);
end Search_A_List_Of_Records;
```

{{out}}

```txt
 6
Khartoum-Omdurman
 4.58000E+00

```



## ALGOL 68


```algol68
# Algol 68 doesn't have generic array searches but we can easily provide #
# type specific ones                                                     #

# mode to hold the city/population info #
MODE CITYINFO = STRUCT( STRING name, REAL population in millions );

# array of cities and populations #
[ 1 : 10 ]CITYINFO cities := ( ( "Lagos",                21.0 )
                             , ( "Cairo",                15.2 )
                             , ( "Kinshasa-Brazzaville", 11.3 )
                             , ( "Greater Johannesburg", 7.55 )
                             , ( "Mogadishu",            5.85 )
                             , ( "Khartoum-Omdurman",    4.98 )
                             , ( "Dar Es Salaam",        4.7  )
                             , ( "Alexandria",           4.58 )
                             , ( "Abidjan",              4.4  )
                             , ( "Casablanca",           3.98 )
                             );

# operator to find the first city with the specified criteria, expressed as a procedure #
# returns the index of the CITYINFO. We can also overload FIND so it can be applied to  #
# arrays of other types                                                                 #
# If there is no city matching the criteria, a value greater than the upper bound of    #
# the cities array is returned                                                          #
PRIO FIND = 1;
OP   FIND = ( REF[]CITYINFO cities, PROC( REF CITYINFO )BOOL criteria )INT:
     BEGIN
         INT  result := UPB cities + 1;
         BOOL found  := FALSE;
         FOR pos FROM LWB cities TO UPB cities WHILE NOT found DO
             IF criteria( cities[ pos ] )
             THEN
                 found  := TRUE;
                 result := pos
             FI
         OD;
         result
     END # FIND # ;

# convenience operator to determine whether a STRING starts with a particular character #
# returns TRUE if s starts with c, FALSE otherwise                                      #
PRIO STARTSWITH = 9;
OP   STARTSWITH = ( STRING s, CHAR c )BOOL:
     IF LWB s > UPB s THEN FALSE # empty string                                         #
     ELSE s[ LWB s ] = c
     FI # STARTSWITH # ;

# find the 0-based index of Dar Es Salaam #
# ( if we remove the "[ @ 0 ]", it would find the 1-based index ) #
# NB - this assumes there is one - would get a subscript bound error if there isn't #
print( ( "index of Dar Es Salaam (from 0): "
       , whole( cities[ @ 0 ] FIND ( ( REF CITYINFO city )BOOL: name OF city = "Dar Es Salaam" ), 0 )
       , newline
       )
     );

# find the first city with population under 5M #
# NB - this assumes there is one - would get a subscript bound error if there isn't #
print( ( name OF cities[ cities FIND ( ( REF CITYINFO city )BOOL: population in millions OF city < 5.0 ) ]
       , " has a population under 5M"
       , newline
       )
     );

# find the population of the first city whose name starts with "A" #
# NB - this assumes there is one - would get a subscript bound error if there isn't #
print( ( "The population of a city named ""A..."" is: "
       , fixed( population in millions OF cities[ cities FIND ( ( REF CITYINFO city )BOOL: name OF city STARTSWITH "A" ) ], 0, 2 )
       , newline
       )
     )

```

{{out}}

```txt

index of Dar Es Salaam (from 0): 6
Khartoum-Omdurman has a population under 5M
The population of a city named "A..." is: 4.58

```



## AppleScript


{{Trans|JavaScript}}


```AppleScript
-- RECORDS

property lstCities : [¬
    {|name|:"Lagos", population:21.0}, ¬
    {|name|:"Cairo", population:15.2}, ¬
    {|name|:"Kinshasa-Brazzaville", population:11.3}, ¬
    {|name|:"Greater Johannesburg", population:7.55}, ¬
    {|name|:"Mogadishu", population:5.85}, ¬
    {|name|:"Khartoum-Omdurman", population:4.98}, ¬
    {|name|:"Dar Es Salaam", population:4.7}, ¬
    {|name|:"Alexandria", population:4.58}, ¬
    {|name|:"Abidjan", population:4.4}, ¬
    {|name|:"Casablanca", population:3.98}]


-- SEARCHES

-- nameIsDar :: Record -> Bool
on nameIsDar(rec)
    |name| of rec = "Dar Es Salaam"
end nameIsDar

-- popBelow :: Record -> Bool
on popBelow5M(rec)
    population of rec < 5
end popBelow5M

-- nameBeginsWith :: Record -> Bool
on nameBeginsWithA(rec)
    text 1 of |name| of rec = "A"
end nameBeginsWithA


-- TEST
on run

    return {¬
        findIndex(nameIsDar, lstCities), ¬
        ¬
            |name| of find(popBelow5M, lstCities), ¬
        ¬
            population of find(nameBeginsWithA, lstCities)}

end run




-- GENERIC FUNCTIONS

-- find :: (a -> Bool) -> [a] -> Maybe a
on find(f, xs)
    tell mReturn(f)
        set lng to length of xs
        repeat with i from 1 to lng
            if lambda(item i of xs) then return item i of xs
        end repeat
        return missing value
    end tell
end find

-- findIndex :: (a -> Bool) -> [a] -> Maybe Int
on findIndex(f, xs)
    tell mReturn(f)
        set lng to length of xs
        repeat with i from 1 to lng
            if lambda(item i of xs) then return i
        end repeat
        return missing value
    end tell
end findIndex

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property lambda : f
        end script
    end if
end mReturn
```


{{Out}}

```AppleScript
{6, "Khartoum-Omdurman", 4.58}
```



## C


This solution makes use of the 'bsearch' and 'lfind' library functions. Note: 'lfind' is available only on Posix systems, and is found in the 'search.h' header. However it is also part of the Ming C Compiler for Windows.

```c

#include <stdint.h> /* intptr_t */
#include <stdio.h>
#include <stdlib.h> /* bsearch */
#include <string.h>
#include <search.h> /* lfind */

#define LEN(x) (sizeof(x) / sizeof(x[0]))

struct cd {
    char *name;
    double population;
};

/* Return -1 if name could not be found */
int search_get_index_by_name(const char *name, const struct cd *data, const size_t data_length,
        int (*cmp_func)(const void *, const void *))
{
    struct cd key = { (char *) name, 0 };
    struct cd *match = bsearch(&key, data, data_length,
            sizeof(struct cd), cmp_func);

    if (match == NULL)
        return -1;
    else
        return ((intptr_t) match - (intptr_t) data) / sizeof(struct cd);
}

/* Return -1 if no matching record can be found */
double search_get_pop_by_name(const char *name, const struct cd *data, size_t data_length,
        int (*cmp_func)(const void *, const void *))
{
    struct cd key = { (char *) name, 0 };
    struct cd *match = lfind(&key, data, &data_length,
            sizeof(struct cd), cmp_func);

    if (match == NULL)
        return -1;
    else
        return match->population;
}

/* Return NULL if no value satisfies threshold */
char* search_get_pop_threshold(double pop_threshold, const struct cd *data, size_t data_length,
        int (*cmp_func)(const void *, const void *))
{
    struct cd key = { NULL, pop_threshold };
    struct cd *match = lfind(&key, data, &data_length,
            sizeof(struct cd), cmp_func);

    if (match == NULL)
        return NULL;
    else
        return match->name;
}

int cd_nameChar_cmp(const void *a, const void *b)
{
    struct cd *aa = (struct cd *) a;
    struct cd *bb = (struct cd *) b;

	int i,len = strlen(aa->name);

	for(i=0;i<len;i++)
		if(bb->name[i]!=aa->name[i])
			return -1;
	return 0;
}

int cd_name_cmp(const void *a, const void *b)
{
    struct cd *aa = (struct cd *) a;
    struct cd *bb = (struct cd *) b;
    return strcmp(bb->name, aa->name);
}

int cd_pop_cmp(const void *a, const void *b)
{
    struct cd *aa = (struct cd *) a;
    struct cd *bb = (struct cd *) b;
    return bb->population >= aa->population;
}

int main(void)
{
    const struct cd citydata[] = {
        { "Lagos", 21 },
        { "Cairo", 15.2 },
        { "Kinshasa-Brazzaville", 11.3 },
        { "Greater Johannesburg", 7.55 },
        { "Mogadishu", 5.85 },
        { "Khartoum-Omdurman", 4.98 },
        { "Dar Es Salaam", 4.7 },
        { "Alexandria", 4.58 },
        { "Abidjan", 4.4 },
        { "Casablanca", 3.98 }
    };

    const size_t citydata_length = LEN(citydata);

    printf("%d\n", search_get_index_by_name("Dar Es Salaam", citydata, citydata_length, cd_name_cmp));
    printf("%s\n", search_get_pop_threshold(5, citydata, citydata_length, cd_pop_cmp));
    printf("%lf\n", search_get_pop_by_name("A", citydata, citydata_length, cd_nameChar_cmp));

    return 0;
}

```


Output

```txt

6
Khartoum-Omdurman
4.580000

```



## C++


<tt>std::find_if</tt> accepts a lambda as predicate.


```cpp
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

struct city {
    std::string name;
    float population;
};

int main()
{
    std::vector<city> cities = {
        { "Lagos", 21 },
        { "Cairo", 15.2 },
        { "Kinshasa-Brazzaville", 11.3 },
        { "Greater Johannesburg", 7.55 },
        { "Mogadishu", 5.85 },
        { "Khartoum-Omdurman", 4.98 },
        { "Dar Es Salaam", 4.7 },
        { "Alexandria", 4.58 },
        { "Abidjan", 4.4 },
        { "Casablanca", 3.98 },
    };

    auto i1 = std::find_if( cities.begin(), cities.end(),
        [](city c){ return c.name == "Dar Es Salaam"; } );
    if (i1 != cities.end()) {
        std::cout << i1 - cities.begin() << "\n";
    }

    auto i2 = std::find_if( cities.begin(), cities.end(),
        [](city c){ return c.population < 5.0; } );
    if (i2 != cities.end()) {
        std::cout << i2->name << "\n";
    }

    auto i3 = std::find_if( cities.begin(), cities.end(),
        [](city c){ return c.name.length() > 0 && c.name[0] == 'A'; } );
    if (i3 != cities.end()) {
        std::cout << i3->population << "\n";
    }
}
```


{{out}}

```txt

6
Khartoum-Omdurman
4.58

```


=={{header|C sharp|C#}}==

```csharp
using System;
using System.Collections.Generic;

namespace RosettaSearchListofRecords
{
    class Program
    {
        static void Main(string[] args)
        {
            var dataset = new List<Dictionary<string, object>>() {
                new Dictionary<string, object>   {{ "name" , "Lagos"},                {"population", 21.0  }},
                new Dictionary<string, object>   {{ "name" , "Cairo"},                {"population", 15.2  }},
                new Dictionary<string, object>   {{ "name" , "Kinshasa-Brazzaville"}, {"population", 11.3  }},
                new Dictionary<string, object>   {{ "name" , "Greater Johannesburg"}, {"population",  7.55 }},
                new Dictionary<string, object>   {{ "name" , "Mogadishu"},            {"population",  5.85 }},
                new Dictionary<string, object>   {{ "name" , "Khartoum-Omdurman"},    {"population",  4.98 }},
                new Dictionary<string, object>   {{ "name" , "Dar Es Salaam"},        {"population",  4.7  }},
                new Dictionary<string, object>   {{ "name" , "Alexandria"},           {"population",  4.58 }},
                new Dictionary<string, object>   {{ "name" , "Abidjan"},              {"population",  4.4  }},
                new Dictionary<string, object>   {{ "name" , "Casablanca"},           {"population",  3.98 }}
            };

            // Find the (zero-based) index of the first city in the list whose name is "Dar Es Salaam"
            var index = dataset.FindIndex(x => ((string)x["name"]) == "Dar Es Salaam");
            Console.WriteLine(index);

            // Find the name of the first city in this list whose population is less than 5 million
            var name = (string)dataset.Find(x => (double)x["population"] < 5.0)["name"];
            Console.WriteLine(name);

            // Find the population of the first city in this list whose name starts with the letter "A"
            var aNamePopulation = (double)dataset.Find(x => ((string)x["name"]).StartsWith("A"))["population"];
            Console.WriteLine(aNamePopulation);
        }
    }
}
```

{{out}}

```txt

6
Khartoum-Omdurman
4.58

```



## Common Lisp


```lisp
(defstruct city
  (name nil :type string)
  (population nil :type number))

(defparameter *cities*
  (list (make-city :name "Lagos"                :population 21.0)
        (make-city :name "Cairo"                :population 15.2)
        (make-city :name "Kinshasa-Brazzaville" :population 11.3)
        (make-city :name "Greater Johannesburg" :population  7.55)
        (make-city :name "Mogadishu"            :population  5.85)
        (make-city :name "Khartoum-Omdurman"    :population  4.98)
        (make-city :name "Dar Es Salaam"        :population  4.7)
        (make-city :name "Alexandria"           :population  4.58)
        (make-city :name "Abidjan"              :population  4.4)
        (make-city :name "Casablanca"           :population  3.98)))

(defun main ()
  (let ((answer1 (position "Dar Es Salaam" *cities* :key #'city-name :test #'string=))
        (answer2 (city-name (find-if (lambda (population) (< population 5))
                                     *cities* :key #'city-population)))
        (answer3 (city-population (find-if (lambda (name) (char= (char name 0) #\A))
                                           *cities* :key #'city-name))))
    (format t "Answer 1: ~A~%" answer1)
    (format t "Answer 2: ~A~%" answer2)
    (format t "Answer 3: ~A~%" answer3)))
```

{{out}}

```txt
Answer 1: 6
Answer 2: Khartoum-Omdurman
Answer 3: 4.58
```



## EchoLisp


{{update|EchoLisp|
* You shouldn't parse the input from JSON - instead, show to readers what the data structure looks like natively.
* A third test-case has been added.
}}

We demonstrate the '''vector-search''' primitive, which takes as input a vector, and a predicate.

```scheme

(require 'struct)
(require 'json)

;; importing data
(define cities
#<<
[{"name":"Lagos", "population":21}, {"name":"Cairo", "population":15.2}, {"name":"Kinshasa-Brazzaville", "population":11.3}, {"name":"Greater Johannesburg", "population":7.55}, {"name":"Mogadishu", "population":5.85}, {"name":"Khartoum-Omdurman", "population":4.98}, {"name":"Dar Es Salaam", "population":4.7}, {"name":"Alexandria", "population":4.58}, {"name":"Abidjan", "population":4.4}, {"name":"Casablanca", "population":3.98}]
>>#)

;; define a structure matching data
;; heterogenous  slots values
(struct city (name population))

;; convert JSON to EchoLisp instances of structures
(set! cities
	(vector-map (lambda(x) (json->struct x struct:city)) (json-import cities)))

;; search by name, case indifferent
(define (city-index name)
	(vector-search (lambda(x) (string-ci=? (city-name x) name)) cities))

;; returns first city name such as population < seuil
(define (city-pop seuil)
	(define idx (vector-search (lambda(x) (< (city-population x) seuil)) cities))
	(if idx
		(city-name (vector-ref cities idx))
		(cons seuil 'not-found)))


(city-index "Dar Es Salaam") → 6
(city-pop 5) → "Khartoum-Omdurman"
(city-pop -666) → (-666 . not-found)
(city-index "alexandra") → #f

```


## Elena

ELENA 4.1 :

```elena
import extensions;
import system'routines;

public program()
{
    var dataset := new::
    (
        new :: { Name = "Lagos";  Population = 21.0r; },
        new :: { Name = "Cairo";  Population = 15.2r; },
        new :: { Name = "Kinshasa-Brazzaville";  Population = 11.3r; },
        new :: { Name = "Greater Johannesburg";  Population = 7.55r; },
        new :: { Name = "Mogadishu";  Population = 5.85r; },
        new :: { Name = "Khartoum-Omdurman";  Population = 4.98r; },
        new :: { Name = "Dar Es Salaam";  Population = 4.7r; },
        new :: { Name = "Alexandria";  Population = 4.58r; },
        new :: { Name = "Abidjan";  Population = 4.4r; },
        new :: { Name = "Casablanca";  Population = 3.98r; }
    );

    var index := dataset.selectBy:(r => r.Name).toArray().indexOfElement("Dar Es Salaam");
    console.printLine(index);

    var name := dataset.filterBy:(c => c.Population < 5.0r).toArray().FirstMember.Name;
    console.printLine(name);

    var namePopulation := dataset.filterBy:(c => c.Name.startingWith("A")).toArray().FirstMember.Population;
    console.printLine(namePopulation)
}
```

{{out}}

```txt

6
Khartoum-Omdurman
4.58

```



## Elixir


```elixir
cities = [
  [name: "Lagos",                 population: 21.0 ],
  [name: "Cairo",                 population: 15.2 ],
  [name: "Kinshasa-Brazzaville",  population: 11.3 ],
  [name: "Greater Johannesburg",  population:  7.55],
  [name: "Mogadishu",             population:  5.85],
  [name: "Khartoum-Omdurman",     population:  4.98],
  [name: "Dar Es Salaam",         population:  4.7 ],
  [name: "Alexandria",            population:  4.58],
  [name: "Abidjan",               population:  4.4 ],
  [name: "Casablanca",            population:  3.98]
]

IO.puts Enum.find_index(cities, fn city -> city[:name] == "Dar Es Salaam" end)
IO.puts Enum.find(cities, fn city -> city[:population] < 5.0 end)[:name]
IO.puts Enum.find(cities, fn city -> String.first(city[:name])=="A" end)[:population]
```


{{out}}

```txt

6
Khartoum-Omdurman
4.58

```



## Factor

For our associative structure, we use the tuple: an object composed of named slots, each holding a value which can be accessed using automatically-generated accessors. Factor is built around combinators (Factor's word for higher-order functions), so completing the task as requested is idiomatic. <code>find</code> is an existing combinator that takes a sequence and a predicate quotation (quotation being Factor's word for anonymous function) and returns the first element of the sequence for which the predicate quotation yields <code>t</code>.

Not only does <code>find</code> return the element, but also the index, which allows us to use <code>find</code> for all of the required tasks. Since Factor is a stack-based concatenative language, multiple return values are elegant to use. We can simply <code>drop</code> the sequence element on the top of the data stack if we are only interested in the index, or we can <code>nip</code> the index if we are only interested in the sequence element on the top of the stack.

```factor
USING: accessors io kernel math prettyprint sequences ;
IN: rosetta-code.search-list

TUPLE: city name pop ;

CONSTANT: data {
    T{ city f "Lagos" 21.0 }
    T{ city f "Cairo" 15.2 }
    T{ city f "Kinshasa-Brazzaville" 11.3 }
    T{ city f "Greater Johannesburg" 7.55 }
    T{ city f "Mogadishu" 5.85 }
    T{ city f "Khartoum-Omdurman" 4.98 }
    T{ city f "Dar Es Salaam" 4.7 }
    T{ city f "Alexandria" 4.58 }
    T{ city f "Abidjan" 4.4 }
    T{ city f "Casablanca" 3.98 }
}

! Print the index of the first city named Dar Es Salaam.
data [ name>> "Dar Es Salaam" = ] find drop .

! Print the name of the first city with under 5 million people.
data [ pop>> 5 < ] find nip name>> print

! Print the population of the first city starting with 'A'.
data [ name>> first CHAR: A = ] find nip pop>> .
```

{{out}}

```txt

6
Khartoum-Omdurman
4.58

```



## Fortran

In order to employ a compound data aggregate such as CITY with components CITY.NAME and CITY.POPULATION, F90 is required. Earlier, one would have employed a collection of separate arrays with similar names, such as CNAME and CPOP. This may still be desirable when array parameters from a data aggregate will be passed via copy-in and copy-out instead of by reference.  An alternative declaration would be to have the components of the aggregate use the array aspect, as in <code>REAL POPULATION(10)</code>  This would also avoid wasting storage due to multiple padding forced by any alignment requirements, as when a floating-point variable has to be aligned to a storage boundary. Otherwise, with "packed" storage, extra code required to access unaligned items consumes extra storage and of course, extra time. But the specification calls for multiplicity of the pairs. By contrast, pl/i offers "floating indexing" whereby <code>CITY(i).NAME</code> ''and'' <code>CITY.NAME(i)</code> would be acceptable usages for ''either'' form of declaration, thus concealing the issue.

As for the names, one must choose a maximum size for the city name, and 28 seems a perfect choice, even for names in Madagascar. Later fortran offers facilities for character variables "fitted to size", at the cost of extra indirection.

Fortran does not offer a "pass-by-name" facility, whereby the test function for the searches could be specified in the manner of [[Jensen's_Device]], something like <code>FINDFIRST(I,CITY(I).NAME .EQ. "Dar Es Salaam")</code> and <code>FINDFIRST(I,CITY(I).POPULATION < 5)</code> in the form of boolean expressions evaluated in the calling environment, so one must devise a suitable procedure, and because the two components NAME and POPULATION have different types, the parameter matching protocol requires two different procedures. Via additional F90 syntax it is possible to define a "generic" routine with a single name that, when invoked, will in fact invoke the appropriate specific routine, however in this example one test is for equality, and the other is for "less than" so the commonality is weak. One could instead define two boolean functions (one each for the two types of test, say NAMEISEQUAL and POPULATIONISLESS) and pass the appropriate function as a parameter to a more general FINDFIRST routine, but this is both messy and restrictive: two functions to be defined separately and with FINDFIRST, all will only accept a type CITYSTAT as parameters. And as the TARGET parameter is of two different types, there can't be one FINDFIRST routine anyway.

Instead, two search functions. Using the function's name as a variable within the function could not be relied on prior to F90: some compilers would disallow it and in other cases odd results might be produced, so a local variable would have to be used. These functions turn out to be general in the sense that they're not limited to a type of CITYSTAT. Instead, FIRSTMATCH searches an array of texts, and FIRSTLESS an array of floating-point numbers - they could be applied to any such arrays. But sometimes at a cost. They are not prepared to deal with arrays having a "stride" other than one, such as the CITY.NAME entries, whose successive elements are not in successive storage locations. Instead, the compiler generates code to copy the desired elements from the CITY aggregate into a temporary variable having that arrangement, and passes that to the routine by reference. One could use the special declaration <code>INTENT(IN)</code> for read-only activities, and that will at least abate the copy-back, but for small items and only ten at that, this overhead can be ignored; otherwise in-line code would be required for each search. On the other hand, it does allow the special usage <code>CITY.NAME(1:1)</code> to search only the first character of each name.

Although F90 allows an array to be defined with a lower bound other than the default of one, this requires such a non-default lower bound to be specified in every declaration, which is tiresome, and leads to confusion over counting. Still, to suit the requirement, the index found for Dar Es Salaam is reduced by one. Returning a value of zero for "not found" is helpful (-1 would be good too, but this can't be an unsigned integer) and it is often useful to have an actual element zero with a dummy entry, such as <code>CITYSTAT("--No such city!",3E33)</code> so that a WRITE statement need not be prefaced by a test so as to avoid an index-out-of-bounds error. The dummy population value, if printed, will likely overflow its format code and fill its space with asterisks in the standard behaviour. More modern systems include the reading or writing of the special mnemonic "NaN" for "Not a Number" but this mnemonic is not recognised in fortran source itself so something like <code>PARAMETER (NaN = Z'FFFFFFFFFFFFFFFF')</code> would be needed if preferred - though readers may not recgnise that mnemonic either. With such a dummy-element scheme, the searches would be specified by <code>FIRSTMATCH(CITY(1:).NAME,"Dar Es Salaam")</code> to signify that the zero'th element was not involved. Alternatively, the special entry could follow the "live" entries and "not found" would be an index value of eleven, there being ten entries in this example. If entries are to be added or removed, this becomes tiresome, but it does mean that there need no longer be an assignment of the "not found" value after the DO-loop's search, because that will be the value of the index variable on exit - though some argue that no such exit value should be relied upon. Coherent organisation is required for this! Incidentally, 0 (zero) in the source continuation field is treated as a space (meaning no continuation), thus the letter o instead: O would be asking for misreading.

The specification mentions an ordered list: here, the items are indeed ordered in storage (as first, second, etc) but ordering more usefully refers to the values of the components. The example is presented in reducing order of population. More generally, the storage order might be arbitrary, and one would use indices, arrays such as XCNAME which would list the entries in the order of their names, and XCPOP their populations. These arrays would each be produced by an indexed sort process that would not move the data about, and so long as entries were not frequently altered (requiring re-sorting) these indices could be used for searching. <code>IT = FIRSTMATCH(CITY(XCNAME).NAME,"Dar Es Salaam")</code> would search the city names in alphabetical order rather than storage order and <code>IT</code> would be its position in XCNAME so that <code>CITY(XCNAME(IT)).POPULATION</code> would give the city's population. However, the "not found" result would be troublesome if not tested for. If zero was the value for that, arranging that XCNAME(0) was zero would help, however an often useful usage for XCNAME(0) is for it to contain the number of elements in its list.

Such arrays involve the ability to access an individual city's information at random, simply by specifying the index into the CITY array, however the given problem requires only sequential access. In such a case, the storage for the city elements could be formed as a linked-list which would be followed sequentially. Even so, random access can be regained via an array such as XCNAME, which now would hold the storage address of the corresponding CITY element. And as ever, how long is a piece of string? Here, ten.

If the data were stored as records in a disc file, a record zero won't exist and so appropriate testing for "not found" will be required. For this example however there is no attempt either to prepare a suitable "not found" entry nor to test and evade such a misfortune. The test data employed do not provoke such errors...
```Fortran
      MODULE SEMPERNOVIS	!Keep it together.
       TYPE CITYSTAT		!Define a compound data type.
        CHARACTER*28	NAME		!Long enough?
        REAL		POPULATION	!Accurate enough.
       END TYPE CITYSTAT	!Just two parts, but different types.
       TYPE(CITYSTAT) CITY(10)	!Righto, I'll have some.
       DATA CITY/		!Supply the example's data.
     1   CITYSTAT("Lagos",               21.0 ),
     2   CITYSTAT("Cairo",               15.2 ),
     3   CITYSTAT("Kinshasa-Brazzaville",11.3 ),
     4   CITYSTAT("Greater Johannesburg", 7.55),
     5   CITYSTAT("Mogadishu",            5.85),
     6   CITYSTAT("Khartoum-Omdurman",    4.98),
     7   CITYSTAT("Dar Es Salaam",        4.7 ),
     8   CITYSTAT("Alexandria",           4.58),
     9   CITYSTAT("Abidjan",              4.4 ),
     o   CITYSTAT("Casablanca",           3.98)/
       CONTAINS
        INTEGER FUNCTION FIRSTMATCH(TEXT,TARGET)	!First matching.
         CHARACTER*(*) TEXT(:)	!An array of texts.
         CHARACTER*(*) TARGET	!The text to look for.
          DO FIRSTMATCH = 1,UBOUND(TEXT,DIM = 1)	!Scan the array from the start.
            IF (TEXT(FIRSTMATCH) .EQ. TARGET) RETURN	!An exact match? Ignoring trailing spaces.
          END DO				!Try the next.
          FIRSTMATCH = 0		!No match. Oh dear.
        END FUNCTION FIRSTMATCH

        INTEGER FUNCTION FIRSTLESS(VAL,TARGET)	!First matching.
         REAL VAL(:)	!An array of values.
         REAL TARGET	!The value to look for.
          DO FIRSTLESS = 1,UBOUND(VAL,DIM = 1)	!Step through the array from the start.
            IF (VAL(FIRSTLESS) .LT. TARGET) RETURN	!Suitable?
          END DO				!Try the next.
          FIRSTLESS = 0		!No match. Oh dear.
        END FUNCTION FIRSTLESS
      END MODULE SEMPERNOVIS

      PROGRAM POKE
      USE SEMPERNOVIS	!Ex Africa, ...
      CHARACTER*(*) BLAH	!Save on some typing.
      PARAMETER (BLAH = "The first city in the list whose ")	!But also, for layout.

      WRITE (6,1) BLAH,FIRSTMATCH(CITY.NAME,"Dar Es Salaam") - 1	!My array starts with one.
    1 FORMAT (A,"name is Dar Es Salaam, counting with zero, is #",I0)

      WRITE (6,2) BLAH,CITY(FIRSTLESS(CITY.POPULATION,5.0)).NAME
    2 FORMAT (A,"population is less than 5 is ",A)

      WRITE (6,3) BLAH,CITY(FIRSTMATCH(CITY.NAME(1:1),"A")).POPULATION
    3 FORMAT (A,"whose name starts with A has population",F5.2)
      END
```


The words NAME and TARGET can have special usages in F90, but fortran has ''no'' reserved words so these names can be put to ordinary use. Alas, the syntax highlighter does not recognise their non-special use and gives them colour. One could fuss further over the layout of the output, but here it is:

```txt

The first city in the list whose name is Dar Es Salaam, counting with zero, is #6
The first city in the list whose population is less than 5 is Khartoum-Omdurman
The first city in the list whose whose name starts with 'A' has population 4.58

```



## Go

'''Basic solution:'''

```go
package main

import (
    "fmt"
    "strings"
)

type element struct {
    name       string
    population float64
}

var list = []element{
    {"Lagos", 21},
    {"Cairo", 15.2},
    {"Kinshasa-Brazzaville", 11.3},
    {"Greater Johannesburg", 7.55},
    {"Mogadishu", 5.85},
    {"Khartoum-Omdurman", 4.98},
    {"Dar Es Salaam", 4.7},
    {"Alexandria", 4.58},
    {"Abidjan", 4.4},
    {"Casablanca", 3.98},
}

func find(cond func(*element) bool) int {
    for i := range list {
        if cond(&list[i]) {
            return i
        }
    }
    return -1
}

func main() {
    fmt.Println(find(func(e *element) bool {
        return e.name == "Dar Es Salaam"
    }))

    i := find(func(e *element) bool {
        return e.population < 5
    })
    if i < 0 {
        fmt.Println("*** not found ***")
    } else {
        fmt.Println(list[i].name)
    }

    i = find(func(e *element) bool {
        return strings.HasPrefix(e.name, "A")
    })
    if i < 0 {
        fmt.Println("*** not found ***")
    } else {
        fmt.Println(list[i].population)
    }
}
```

{{out}}

```txt

6
Khartoum-Omdurman
4.58

```


'''Data conversion solution:'''

The prohibition on parsing JSON is unreasonable in any language.  The alternative to parsing by computer is parsing manually, reformatting data with human fingers on a keyboard.  It's okay for ten lines of test data but would be unreasonable in any real application.  But let's say than in real life you have a pointy-haired boss that decrees that no JSON parsing will be done &mdash; even though he has contracted a vendor that supplies JSON only.  You would write a data converter that quietly converts data outside the application.

First, a package to define the data structure needed.  This package will be imported by both the data conversion tool and the application.

```go
package datadef

type Element struct {
    Name       string
    Population float64
}

type List []Element
```


Then the data conversion tool.  This program reads JSON from stdin and generates the Go code of a package "data" containing an equivalent Go literal.

```go
package main

import (
    "encoding/json"
    "fmt"
    "go/build"
    "log"
    "os"
    "path/filepath"

    "datadef"
)

func main() {
    var l datadef.List
    if err := json.NewDecoder(os.Stdin).Decode(&l); err != nil {
        log.Fatal(err)
    }
    pp := filepath.Join(filepath.SplitList(build.Default.GOPATH)[0], "src/data")
    f, err := os.Create(filepath.Join(pp, "data.go"))
    if err != nil {
        log.Fatal(err)
    }
    fmt.Fprintln(f, `package data

import "datadef"

var List = datadef.List {`)
    for i, e := range l {
        fmt.Fprintf(f, "   %d: {%q, %g},\n", i, e.Name, e.Population)
    }
    fmt.Fprintln(f, "}")
}
```

{{out}}

```go
package data

import "datadef"

var List = datadef.List {
   0: {"Lagos", 21},
   1: {"Cairo", 15.2},
   2: {"Kinshasa-Brazzaville", 11.3},
   3: {"Greater Johannesburg", 7.55},
   4: {"Mogadishu", 5.85},
   5: {"Khartoum-Omdurman", 4.98},
   6: {"Dar Es Salaam", 4.7},
   7: {"Alexandria", 4.58},
   8: {"Abidjan", 4.4},
   9: {"Casablanca", 3.98},
}
```


The desired program imports the generated package containing the converted data.  Program and imported data are JSON-free.

```go
package main

import (
    "fmt"
    "strings"

    "data"
    "datadef"
)

func find(cond func(*datadef.Element) bool) int {
    for i := range data.List {
        if cond(&data.List[i]) {
            return i
        }
    }
    return -1
}

func main() {
    i := find(func(e *datadef.Element) bool {
        return e.Name == "Dar Es Salaam"
    })
    if i < 0 {
        fmt.Println("*** not found ***")
    } else {
        fmt.Println(i)
    }

    i = find(func(e *datadef.Element) bool {
        return e.Population < 5
    })
    if i < 0 {
        fmt.Println("*** not found ***")
    } else {
        fmt.Println(data.List[i].Name)
    }

    i = find(func(e *datadef.Element) bool {
        return strings.HasPrefix(e.Name, "A")
    })
    if i < 0 {
        fmt.Println("*** not found ***")
    } else {
        fmt.Println(data.List[i].Population)
    }
}
```

Output same as basic solution.

'''Solution using sorted population count:'''

The population ordering is useful only for queries against population and so cannot be used, at least not in any simple way, by the general find function shown above.  The sort package of the Go standard library however contains a function for making general queries against an ordered list.  This solution shows how the population query can be done with this function.


```go
package main

import (
    "fmt"
    "sort"

    "data"
)

func main() {
    if !sort.SliceIsSorted(data.List, func(i, j int) bool {
        return data.List[i].Population > data.List[j].Population
    }) {
        panic("data not sorted by decreasing population")
    }

    i := sort.Search(len(data.List), func(i int) bool {
        return data.List[i].Population < 5
    })
    if i == len(data.List) {
        fmt.Println("*** not found ***")
    } else {
        fmt.Println(data.List[i].Name)
    }
}
```

{{out}}

```txt

Khartoum-Omdurman

```


'''sort.Search for the other queries:'''

The same sort.Search function is general enough to be used for the other two queries of the task as long as appropriate indexes are constructed.  sort.Search is interesting because it is in the standard library and is much like the function required by the task, being generalized to take a function as an argument.  It does not completely meet task requirements though because it works on a single ordering that must already exist or already be computed.  A function that would analyze a general query, use available orderings when possible, and fall back on linear search otherwise is surely beyond the task scope.


```go
package main

import (
    "fmt"
    "sort"
    "strings"

    "data"
)

func main() {
    nx := make([]int, len(data.List))
    for i := range nx {
        nx[i] = i
    }
    sort.Slice(nx, func(i, j int) bool {
        return data.List[nx[i]].Name < data.List[nx[j]].Name
    })

    i := sort.Search(len(nx), func(i int) bool {
        return data.List[nx[i]].Name >= "Dar Es Salaam"
    })
    if i == len(nx) || data.List[nx[i]].Name != "Dar Es Salaam" {
        fmt.Println("*** not found ***")
    } else {
        fmt.Println(nx[i])
    }

    for i := range nx {
        nx[i] = i
    }
    sort.SliceStable(nx, func(i, j int) bool {
        return data.List[nx[i]].Name[0] < data.List[nx[j]].Name[0]
    })

    i = sort.Search(len(nx), func(i int) bool {
        return data.List[nx[i]].Name >= "A"
    })
    if i == len(nx) || !strings.HasPrefix(data.List[nx[i]].Name, "A") {
        fmt.Println("*** not found ***")
    } else {
        fmt.Println(data.List[nx[i]].Population)
    }
}
```

{{out}}

```txt

6
4.58

```



## Haskell



```Haskell
import Data.List (findIndex, find)

data City = City
  { name :: String
  , population :: Float
  } deriving (Read, Show)

-- CITY PROPERTIES ------------------------------------------------------------
cityName :: City -> String
cityName (City x _) = x

cityPop :: City -> Float
cityPop (City _ x) = x

mbCityName :: Maybe City -> Maybe String
mbCityName (Just x) = Just (cityName x)
mbCityName _ = Nothing

mbCityPop :: Maybe City -> Maybe Float
mbCityPop (Just x) = Just (cityPop x)
mbCityPop _ = Nothing

-- EXAMPLES -------------------------------------------------------------------
mets :: [City]
mets =
  [ City
    { name = "Lagos"
    , population = 21.0
    }
  , City
    { name = "Cairo"
    , population = 15.2
    }
  , City
    { name = "Kinshasa-Brazzaville"
    , population = 11.3
    }
  , City
    { name = "Greater Johannesburg"
    , population = 7.55
    }
  , City
    { name = "Mogadishu"
    , population = 5.85
    }
  , City
    { name = "Khartoum-Omdurman"
    , population = 4.98
    }
  , City
    { name = "Dar Es Salaam"
    , population = 4.7
    }
  , City
    { name = "Alexandria"
    , population = 4.58
    }
  , City
    { name = "Abidjan"
    , population = 4.4
    }
  , City
    { name = "Casablanca"
    , population = 3.98
    }
  ]

-- TEST -----------------------------------------------------------------------
main :: IO ()
main = do
  mbPrint $ findIndex (("Dar Es Salaam" ==) . cityName) mets
  mbPrint $ mbCityName $ find ((< 5.0) . cityPop) mets
  mbPrint $ mbCityPop $ find (("A" ==) . take 1 . cityName) mets

mbPrint
  :: Show a
  => Maybe a -> IO ()
mbPrint (Just x) = print x
mbPrint x = print x
```


{{Out}}

```txt
6
"Khartoum-Omdurman"
4.58
```





## J


To represent the data in the task description, we will be using a tabular format as follows:


```J
colnumeric=: 0&".&.>@{`[`]}

data=: 1 colnumeric |: fixcsv 0 :0
Lagos, 21
Cairo, 15.2
Kinshasa-Brazzaville, 11.3
Greater Johannesburg, 7.55
Mogadishu, 5.85
Khartoum-Omdurman, 4.98
Dar Es Salaam, 4.7
Alexandria, 4.58
Abidjan, 4.4
Casablanca, 3.98
)
```


And here are the required computations:


```J
   (0 { data) i. <'Dar Es Salaam'
6
   (i. >./)@(* 5&>)@:>@{: data
5
   5 {:: 0 {data
Khartoum-Omdurman
   (1 { data) {::~ 'A' i.~ {.&> 0 { data
4.58
```


The "general search function" mentioned in the task does not seem a natural fit for this set of data, because of the multi-column nature of this data. Nevertheless, we could for example define:


```j
gsf=: 1 :0
:
   I. u x { y
)
```


This uses the single argument aspect of the definition of <code>I.</code> to convert a bit mask to the corresponding sequence of indices. And the column(s) we are searching on are exposed as a parameter for the interface, which allows us to ignore (for this problem) the irrelevant columns...

Thus, we could say:


```J
   1 (= >./)@(* 5&>)@:> gsf data
5
```


But this doesn't seem any clearer or more concise than our previous expression which finds the index of the first example of the most populous city with a population less than five million. Not only that, but if there were multiple cities which had the same population number which satisfied this constraint, this version would return all of those indices where the task explicitly required we return the first example.


###  J: Another approach

The following approach is arguably more natural in J than requiring a dictionary-type structure.

```j
   city=: <;._1 ';Lagos;Cairo;Kinshasa-Brazzaville;Greater Johannesburg;Mogadishu;Khartoum-Omdurman;Dar Es Salaam;Alexandria;Abidjan;Casablanca'
   popln=: 21 15.2 11.3 7.55 5.85 4.98 4.7 4.58 4.4 3.98
   city i. <'Dar Es Salaam'              NB. index of Dar Es Salaam
6
   (city i. boxopen) 'Dar Es Salaam'     NB. anonymous search function with city name as argument
6
   city {::~ (popln < 5) {.@# \: popln   NB. name of first city with population less than 5 million
Khartoum-Omdurman
   popln&(city {::~ \:@[ {.@#~ <) 5      NB. anonymous search function with popln limit as argument
Khartoum-Omdurman
   popln {~ 'A' i.~ {.&> city            NB. population of first city whose name starts with "A"
4.58
   (popln {~ ({.&> city)&i.) 'A'         NB. anonymous search function with first letter as argument
4.58
```



## Java


Java (up to and including) version 7 was not capable of using Predicates or the mentioned higher-order-Programming without external libraries. Java 8 introduced Predicated and Streams (not to be confused with the Java-IO-Streams!) that changed the way how collections of objects can be processed. This example illustrates the changes by solving the three subtasks in different ways:
<ol>
<li> "find the index of the fist city [...]" uses a predicate, but the actual search is done using classic iteration (this is also because the streams don't maintain anything "index-like" and so this would not be possible using stream-processing).
<li> "find city name by population [...]" uses a predicate to descibe the "search term" and returns the name of the city
<li> "find the population by letter [...]" uses a predicate as search term and accepts a "consumer" so the caller of the method specifies what to do with the found result.
</ol>
{{works with|Java|8}}


```Java
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Predicate;

/**
 * Represent a City and it's population.
 * City-Objects do have a natural ordering, they are ordered by their poulation (descending)
 */
class City implements Comparable<City> {
    private final String name;
    private final double population;

    City(String name, double population) {
        this.name = name;
        this.population = population;
    }

    public String getName() {
        return this.name;
    }

    public double getPopulation() {
        return this.population;
    }

    @Override
    public int compareTo(City o) {
        //compare for descending order. for ascending order, swap o and this
        return Double.compare(o.population, this.population);
    }
}

public class SearchListOfRecords {

    public static void main(String[] args) {

        //Array-of-City-Objects-Literal
        City[] datasetArray = {new City("Lagos", 21.),
                new City("Cairo", 15.2),
                new City("Kinshasa-Brazzaville", 11.3),
                new City("Greater Johannesburg", 7.55),
                new City("Mogadishu", 5.85),
                new City("Khartoum-Omdurman", 4.98),
                new City("Dar Es Salaam", 4.7),
                new City("Alexandria", 4.58),
                new City("Abidjan", 4.4),
                new City("Casablanca", 3.98)};

        //Since this is about "collections smarter that arrays", the Array is converted to a List
        List<City> dataset = Arrays.asList(datasetArray);

        //the City-Objects know that they are supposed to be compared by population
        Collections.sort(dataset);


        //Find the first City that matches the given predicate and print it's index in the dataset
        //the Predicate here is given in the form a Java 8 Lambda that returns true if the given name
        //Note that the Predicate is not limited to searching for names. It can operate on anything one can done with
        // and compared about City-Objects
        System.out.println(findIndexByPredicate(dataset, city -> city.getName().equals("Dar Es Salaam")));

        //Find the first City whose population matches the given Predicate (here: population <= 5.) and print it's name
        //here the value is returned an printed by the caller
        System.out.println(findFirstCityByPredicate(dataset, city -> city.getPopulation() <= 5.));

        //Find the first City that matches the given predicate (here: name starts with "A") and
        //apply the given consumer (here: print the city's population)
        //here the caller specifies what to do with the object. This is the most generic solution and could also be used to solve Task 2
        applyConsumerByPredicate(dataset, city -> city.getName().startsWith("A"), city -> System.out.println(city.getPopulation()));

    }

    /**
     * Finds a City by Predicate.
     * The predicate can be anything that can be done or compared about a City-Object.
     * <p>
     * Since the task was to "find the index" it is not possible to use Java 8's stream facilities to solve this.
     * The Predicate is used very explicitly here - this is unusual.
     *
     * @param dataset the data to operate on, assumed to be sorted
     * @param p       the Predicate that wraps the search term.
     * @return the index of the City in the dataset
     */
    public static int findIndexByPredicate(List<City> dataset, Predicate<City> p) {
        for (int i = 0; i < dataset.size(); i++) {
            if (p.test(dataset.get(i)))
                return i;
        }
        return -1;
    }

    /**
     * Finds and returns the name of the first City where the population matches the Population-Predicate.
     * This solutions makes use of Java 8's stream facilities.
     *
     * @param dataset   the data to operate on, assumed to be sorted
     * @param predicate a predicate that specifies the city searched. Can be "any predicate that can be applied to a City"
     * @return the name of the first City in the dataset whose population matches the predicate
     */
    private static String findFirstCityByPredicate(List<City> dataset, Predicate<City> predicate) {
        //turn the List into a Java 8 stream, so it can used in stream-operations
        //filter() by the specified predicate (to the right of this operation, only elements matching the predicate are left in the stream)
        //find the first element (which is "the first city..." from the task)
        //get() the actualy object (this is necessary because it is wrapped in a Java 8 Optional<T>
        //getName() the name and return it.
        return dataset.stream().filter(predicate).findFirst().get().getName();
    }

    /**
     * In specified dataset, find the first City whose name matches the specified predicate, and apply the specified consumer
     * <p>
     * Since this can be solved pretty much like the "find a city by population", this has been varied. The caller specifies what to do with the result.
     * So this method does not return anything, but requiers a "consumer" that processes the result.
     *
     * @param dataset      the data to operate on, assumed to be sorted
     * @param predicate    a predicate that specifies the city searched. Can be "any predicate that can be applied to a City"
     * @param doWithResult a Consumer that specified what to do with the results
     */
    private static void applyConsumerByPredicate(List<City> dataset, Predicate<City> predicate, Consumer<City> doWithResult) {
        //turn the List in to a Java 8 stream in stream-operations
        //filter() by the specified predicate (to the right of this operation, only elements matching the predicate are left in the stream)
        //find the first element (which is "the first city..." from the task)
        // if there is an element found, feed it to the Consumer
        dataset.stream().filter(predicate).findFirst().ifPresent(doWithResult);
    }
}

```


{{Out}}

```txt
6
Khartoum-Omdurman
4.58
```



## JavaScript



### ES5



```JavaScript
(function () {
    'use strict';

    // find :: (a -> Bool) -> [a] -> Maybe a
    function find(f, xs) {
        for (var i = 0, lng = xs.length; i < lng; i++) {
            if (f(xs[i])) return xs[i];
        }
        return undefined;
    }

    // findIndex :: (a -> Bool) -> [a] -> Maybe Int
    function findIndex(f, xs) {
        for (var i = 0, lng = xs.length; i < lng; i++) {
            if (f(xs[i])) return i;
        }
        return undefined;
    }


    var lst = [
      { "name": "Lagos",                "population": 21.0  },
      { "name": "Cairo",                "population": 15.2  },
      { "name": "Kinshasa-Brazzaville", "population": 11.3  },
      { "name": "Greater Johannesburg", "population":  7.55 },
      { "name": "Mogadishu",            "population":  5.85 },
      { "name": "Khartoum-Omdurman",    "population":  4.98 },
      { "name": "Dar Es Salaam",        "population":  4.7  },
      { "name": "Alexandria",           "population":  4.58 },
      { "name": "Abidjan",              "population":  4.4  },
      { "name": "Casablanca",           "population":  3.98 }
    ];

    return {
        darEsSalaamIndex: findIndex(function (x) {
            return x.name === 'Dar Es Salaam';
        }, lst),

        firstBelow5M: find(function (x) {
                return x.population < 5;
            }, lst)
            .name,

        firstApop: find(function (x) {
                return x.name.charAt(0) === 'A';
            }, lst)
            .population
    };

})();
```



{{Out}}

```txt
{"darEsSalaamIndex":6, "firstBelow5M":"Khartoum-Omdurman", "firstApop":4.58}
```



### ES6



```JavaScript
(() => {
    'use strict';

    let lst = [
          { "name": "Lagos",                "population": 21.0  },
          { "name": "Cairo",                "population": 15.2  },
          { "name": "Kinshasa-Brazzaville", "population": 11.3  },
          { "name": "Greater Johannesburg", "population":  7.55 },
          { "name": "Mogadishu",            "population":  5.85 },
          { "name": "Khartoum-Omdurman",    "population":  4.98 },
          { "name": "Dar Es Salaam",        "population":  4.7  },
          { "name": "Alexandria",           "population":  4.58 },
          { "name": "Abidjan",              "population":  4.4  },
          { "name": "Casablanca",           "population":  3.98 }
        ];

    return {
        darEsSalaamIndex: lst.findIndex(x => x.name === 'Dar Es Salaam'),
        firstBelow5M: lst.find(x => x.population < 5)
            .name,
        firstApop: lst.find(x => x.name[0] === 'A')
            .population
    };

})();
```



{{Out}}

```txt
{"darEsSalaamIndex":6, "firstBelow5M":"Khartoum-Omdurman", "firstApop":4.58}
```



## jq


"jq" is so-name because it is a JSON Query Language, and thus all the abstractions are already available for the given searches, except that early versions of jq do not have `first/1` as a builtin.  If your jq does not define `first/1`, then for present purposes, an appropriate definition would be:


```jq
def first(s): [s][0];
```


This will emit `null` if the stream, s, is empty.

In each case where `first/1` is used below, a less efficient alternative is also shown.

====Find the (zero-based) index of the first city in the list whose name is "Dar Es Salaam"====

    map(.name) | index("Dar Es Salaam")


### =Find the name of the first city in this list whose population is less than 5 million=


    first(.[] | select(.population < 5)) | .name

    # Alternatively:

    map(select(.population < 5)) | .[0] | .name

====Find the population of the first city in this list whose name starts with the letter "A"====

    first(.[] | select(.name[0:1] == "A")) | .population

    # Alternatively:

    map(select(.name[0:1] == "A")) | .[0] | .population


## Julia

{{works with|Julia|0.6}}


```julia
using DataFrames

dataset = DataFrame(name=["Lagos", "Cairo", "Kinshasa-Brazzaville", "Greater Johannesburg", "Mogadishu",
                          "Khartoum-Omdurman", "Dar Es Salaam", "Alexandria", "Abidjan", "Casablanca"],
                    population=[21.0, 15.2, 11.3, 7.55, 5.85, 4.98, 4.7, 4.58, 4.4, 3.98])

print("Find the (one-based) index of the first city in the list whose name is \"Dar Es Salaam\": ")
println(findfirst(dataset[:name], "Dar Es Salaam"))
print("Find the name of the first city in this list whose population is less than 5 million: ")
println(dataset[first(find(dataset[:population] .< 5)), :name])
print("Find the population of the first city in this list whose name starts with the letter \"A\": ")
println(dataset[first(find(startswith.(dataset[:name], 'A'))), :population])
```


{{out}}

```txt
Find the (one-based) index of the first city in the list whose name is "Dar Es Salaam": 7
Find the name of the first city in this list whose population is less than 5 million: Khartoum-Omdurman
Find the population of the first city in this list whose name starts with the letter "A": 4.58
```



## Kotlin


```scala
// version 1.1.2

class City(val name: String, val pop: Double)

val cities = listOf(
    City("Lagos", 21.0),
    City("Cairo", 15.2),
    City("Kinshasa-Brazzaville", 11.3),
    City("Greater Johannesburg", 7.55),
    City("Mogadishu", 5.85),
    City("Khartoum-Omdurman", 4.98),
    City("Dar Es Salaam", 4.7),
    City("Alexandria", 4.58),
    City("Abidjan", 4.4),
    City("Casablanca", 3.98)
)

fun main(args: Array<String>) {
    val index = cities.indexOfFirst { it.name == "Dar Es Salaam" }
    println("Index of first city whose name is 'Dar Es Salaam'          = $index")
    val name = cities.first { it.pop < 5.0 }.name
    println("Name of first city whose population is less than 5 million = $name")
    val pop = cities.first { it.name[0] == 'A' }.pop
    println("Population of first city whose name starts with 'A'        = $pop")
}
```


{{out}}

```txt

Index of first city whose name is 'Dar Es Salaam'          = 6
Name of first city whose population is less than 5 million = Khartoum-Omdurman
Population of first city whose name starts with 'A'        = 4.58

```



## Lingo


```lingo
on findFirstRecord (data, condition)
  cnt = data.count
  repeat with i = 1 to cnt
    record = data[i]
    if value(condition) then return [#index:i-1, #record:record]
  end repeat
end
```



```lingo
data = [\
  [ "name": "Lagos",                "population": 21.0  ],\
  [ "name": "Cairo",                "population": 15.2  ],\
  [ "name": "Kinshasa-Brazzaville", "population": 11.3  ],\
  [ "name": "Greater Johannesburg", "population":  7.55 ],\
  [ "name": "Mogadishu",            "population":  5.85 ],\
  [ "name": "Khartoum-Omdurman",    "population":  4.98 ],\
  [ "name": "Dar Es Salaam",        "population":  4.7  ],\
  [ "name": "Alexandria",           "population":  4.58 ],\
  [ "name": "Abidjan",              "population":  4.4  ],\
  [ "name": "Casablanca",           "population":  3.98 ]\
]

q = QUOTE

-- Find the (zero-based) index of the first city in the list whose name is "Dar Es Salaam"
res = findFirstRecord(data, "record.name="&q&"Dar Es Salaam"&q)
if listP(res) then put res.index
-- 6

-- Find the name of the first city in this list whose population is less than 5 million
res = findFirstRecord(data, "record.population<5")
if listP(res) then put res.record.name
-- "Khartoum-Omdurman"

-- Find the population of the first city in this list whose name starts with the letter "A"
res = findFirstRecord(data, "record.name.char[1]="&q&"A"&q)
if listP(res) then put res.record.population
-- 4.5800
```



## Lua

Lua tables are well suited as the element type for this task.  The master data structure is a table of tables.

```Lua
-- Dataset declaration
local cityPops = {
    {name = "Lagos", population = 21.0},
    {name = "Cairo", population = 15.2},
    {name = "Kinshasa-Brazzaville", population = 11.3},
    {name = "Greater Johannesburg", population = 7.55},
    {name = "Mogadishu", population = 5.85},
    {name = "Khartoum-Omdurman", population = 4.98},
    {name = "Dar Es Salaam", population = 4.7},
    {name = "Alexandria", population = 4.58},
    {name = "Abidjan", population = 4.4},
    {name = "Casablanca", population = 3.98}
}

-- Function to search a dataset using a custom match function
function recordSearch (dataset, matchFunction)
    local returnValue
    for index, element in pairs(dataset) do
        returnValue = matchFunction(index, element)
        if returnValue then return returnValue end
    end
    return nil
end

-- Main procedure
local testCases = {
    function (i, e) if e.name == "Dar Es Salaam" then return i - 1 end end,
    function (i, e) if e.population < 5 then return e.name end end,
    function (i, e) if e.name:sub(1, 1) == "A" then return e.population end end
}
for _, func in pairs(testCases) do print(recordSearch(cityPops, func)) end
```

{{out}}

```txt
6
Khartoum-Omdurman
4.58
```



## Maple


```Maple
rec := [table(["name"="Lagos","population"=21.0]),
      table(["name"="Cairo","population"=15.2]),
      table(["name"="Kinshasa-Brazzaville","population"=11.3]),
      table(["name"="Greater Johannesburg","population"=7.55]),
      table(["name"="Mogadishu","population"=5.85]),
      table(["name"="Khartoum-Omdurman","population"=4.98]),
      table(["name"="Dar Es Salaam","population"=4.7  ]),
      table(["name"="Alexandria","population"=4.58]),
      table(["name"="Abidjan","population"=4.4]),
      table(["name"="Casablanca","population"=3.98])]:

searchRec := proc(rec, pred, operation)
	local i:
	for i to numelems(rec) do
		if pred(rec[i]) then
			return operation(rec[i],i):
		fi:
	od:
end proc:
searchRec(rec, x->x["name"] = "Dar Es Salaam", (x,i)->print(i-1)): # minus 1 since Maple is 1-indexed
searchRec(rec, x->x["population"]<5, (x,i)->print(x["name"])):
searchRec(rec, x->x["name"][1] = "A", (x,i)->print(x["population"])):
```

{{out}}

```txt
                               6
                      "Khartoum-Omdurman"
                              4.58

```



## Mathematica


```Mathematica

data = Dataset[{
  <|"name" -> "Lagos", "population" -> 21.|>,
  <|"name" -> "Cairo", "population" -> 15.2|>,
  <|"name" -> "Kinshasa-Brazzaville", "population" -> 11.3|>,
  <|"name" -> "Greater Johannesburg", "population" -> 7.55|>,
  <|"name" -> "Mogadishu", "population" -> 5.85|>,
  <|"name" -> "Khartoum-Omdurman", "population" -> 4.98|>,
  <|"name" -> "Dar Es Salaam", "population" -> 4.7|>,
  <|"name" -> "Alexandria", "population" -> 4.58|>,
  <|"name" -> "Abidjan", "population" -> 4.4|>,
  <|"name" -> "Casablanca", "population" -> 3.98|>
}]

data[Position["Dar Es Salaam"], "name"][1, 1] - 1

data[Select[#population < 5 &]][1, "name"]

data[Select[StringMatchQ[#name, "A*"] &]][1, "population"]
```



## OCaml

{{works with|OCaml|4.03+}}

```ocaml

#load "str.cma"


(* We are going to use literally a list of records as said in the title of the
 * task. *)
(* First: Definition of the record type. *)
type city = {
  name : string;
  population : float
}

(* Second: The actual list of records containing the data. *)
let cities = [
  { name = "Lagos";                population = 21.0  };
  { name = "Cairo";                population = 15.2  };
  { name = "Kinshasa-Brazzaville"; population = 11.3  };
  { name = "Greater Johannesburg"; population =  7.55 };
  { name = "Mogadishu";            population =  5.85 };
  { name = "Khartoum-Omdurman";    population =  4.98 };
  { name = "Dar Es Salaam";        population =  4.7  };
  { name = "Alexandria";           population =  4.58 };
  { name = "Abidjan";              population =  4.4  };
  { name = "Casablanca";           population =  3.98 }
]


(* I can't find in the standard library any function in module List that returns
 * an index. Well, never mind, I make my own... *)
let find_index pred =
  let rec doloop i = function
    | [] -> raise Not_found
    | x :: xs -> if pred x then i else doloop (i + 1) xs
  in
  doloop 0


(* List.find returns the first element that satisfies the predicate.
 * List.filter or List.find_all would return *all* the elements that satisfy the
 * predicate. *)
let get_first pred = List.find pred


(* Simulate the 'startswith' function found in other languages. *)
let startswith sub s =
  Str.string_match (Str.regexp sub) s 0


let () =
  (* We use a typical dot notation to access the record fields. *)
  find_index (fun c -> c.name = "Dar Es Salaam") cities
  |> print_int
  |> print_newline;

  (get_first (fun c -> c.population < 5.0) cities).name
  |> print_endline;

  (get_first (fun c -> startswith "A" c.name) cities).population
  |> print_float
  |> print_newline;

```

{{out}}
 6
 Khartoum-Omdurman
 4.58



## Perl


The <tt>first</tt> function from the core module <tt>List::Util</tt> provides short-circuiting search using a block as predicate. However, it can only return the value of the found element, not its index &ndash; so for the first test-case we need to operate on the list of indices.


```perl
use feature 'say';
use List::Util qw(first);

my @cities = (
  { name => 'Lagos',                population => 21.0  },
  { name => 'Cairo',                population => 15.2  },
  { name => 'Kinshasa-Brazzaville', population => 11.3  },
  { name => 'Greater Johannesburg', population =>  7.55 },
  { name => 'Mogadishu',            population =>  5.85 },
  { name => 'Khartoum-Omdurman',    population =>  4.98 },
  { name => 'Dar Es Salaam',        population =>  4.7  },
  { name => 'Alexandria',           population =>  4.58 },
  { name => 'Abidjan',              population =>  4.4  },
  { name => 'Casablanca',           population =>  3.98 },
);

my $index1 = first { $cities[$_]{name} eq 'Dar Es Salaam' } 0..$#cities;
say $index1;

my $record2 = first { $_->{population} < 5 } @cities;
say $record2->{name};

my $record3 = first { $_->{name} =~ /^A/ } @cities;
say $record3->{population};
```


{{out}}

```txt

6
Khartoum-Omdurman
4.58

```


The CPAN module <tt>List::MoreUtils</tt> provides the <tt>first_index</tt> function which could be used to write that first case more elegantly:


```perl6
use List::MoreUtils qw(first_index);

$index1 = first_index { $_->{name} eq 'Dar Es Salaam' } @cities;
```



## Perl 6


The built-in method <tt>.first</tt> fulfills the requirements of this task.

It takes any [https://docs.perl6.org/language/operators#infix_~~ smart-matcher] as a predicate. The <tt>:k</tt> adverb makes it return the key (i.e. numerical index) instead of the value of the element.

{{Works with|Rakudo|2016.08}}

```perl6
my @cities =
  { name => 'Lagos',                population => 21.0  },
  { name => 'Cairo',                population => 15.2  },
  { name => 'Kinshasa-Brazzaville', population => 11.3  },
  { name => 'Greater Johannesburg', population =>  7.55 },
  { name => 'Mogadishu',            population =>  5.85 },
  { name => 'Khartoum-Omdurman',    population =>  4.98 },
  { name => 'Dar Es Salaam',        population =>  4.7  },
  { name => 'Alexandria',           population =>  4.58 },
  { name => 'Abidjan',              population =>  4.4  },
  { name => 'Casablanca',           population =>  3.98 },
;

say @cities.first(*<name> eq 'Dar Es Salaam', :k);
say @cities.first(*<population> < 5).<name>;
say @cities.first(*<name>.match: /^A/).<population>;
```


{{out}}

```txt

6
Khartoum-Omdurman
4.58

```



## Phix


```Phix
constant CITY_NAME = 1, POPULATION = 2
constant municipalities = {{"Lagos",21},
                           {"Cairo",15.2},
                           {"Kinshasa-Brazzaville",11.3},
                           {"Greater Johannesburg",7.55},
                           {"Mogadishu",5.85},
                           {"Khartoum-Omdurman",4.98},
                           {"Dar Es Salaam",4.7},
                           {"Alexandria",4.58},
                           {"Abidjan",4.4},
                           {"Casablanca",3.98}}

function searchfor(sequence s, integer rid, object user_data, integer return_index=0)
    for i=1 to length(s) do
        if call_func(rid,{s[i],user_data}) then
            return iff(return_index?i:s[i])
        end if
    end for
    return 0 -- not found
end function

function city_named(sequence si, string city_name)
    return si[CITY_NAME]=city_name
end function

?searchfor(municipalities,routine_id("city_named"),"Dar Es Salaam",1)

function smaller_than(sequence si, atom population)
    return si[POPULATION]<population
end function

?searchfor(municipalities,routine_id("smaller_than"),5)[CITY_NAME]

function starts_with(sequence si, integer ch)
    return si[CITY_NAME][1]=ch
end function

?searchfor(municipalities,routine_id("starts_with"),'A')[POPULATION]
```

The columnize function reorganises hetrogenous data into corresponding homogenous arrays, which can make this sort of thing much simpler,
at least for exact matches.

```Phix
constant {cities,populations} = columnize(municipalities)

?populations[find("Dar Es Salaam",cities)]
```

{{out}}

```txt

7
"Khartoum-Omdurman"
4.58
4.7

```

Note that Phix subscripts are 1-based, hence the output of 7 not 6.


## PHP


```PHP

<?php

$data_array = [
  ['name' => 'Lagos', 'population' => 21.0],
  ['name' => 'Cairo', 'population' => 15.2],
  ['name' => 'Kinshasa-Brazzaville', 'population' => 11.3],
  ['name' => 'Greater Johannesburg', 'population' => 7.55],
  ['name' => 'Mogadishu', 'population' => 5.85],
  ['name' => 'Khartoum-Omdurman', 'population' => 4.98],
  ['name' => 'Dar Es Salaam', 'population' => 4.7],
  ['name' => 'Alexandria', 'population' => 4.58],
  ['name' => 'Abidjan', 'population' => 4.4],
  ['name' => 'Casablanca', 'population' => 3.98],
];
$found=0;
$search_name = 'Dar Es Salaam';
echo "Find the (zero-based) index of the first city in the list whose name is \"$search_name\" - 6";

$index = array_search($search_name, array_column($data_array, 'name'));
$population = $data_array[$index]['population'];
echo "\nAnswer 1: Index: [$index] Population for $search_name is $population Million\n";

$search_val = 5;
echo "\nFind the name of the first city in this list whose population is less than $search_val million - Khartoum-Omdurman";
foreach ($data_array as $index => $row) {
  if ($row['population'] < $search_val) {
    $name = $row['name'];
    echo "\nAnswer 2: Index [$index] Population for $row[name] is $row[population] Million\n";
    break;
  }
}

$search_term = 'A';
echo "\n\nFind the population of the first city in this list whose name starts with the letter \"$search_term\" - 4.58";
foreach ($data_array as $index => $row) {
  if (strpos($row['name'], $search_term) === 0) {
    echo "\nAnswer 3: Index: [$index] Population for $row[name] is $row[population] Million\n";
    break;
  }
}

echo "\nDone...";

Output:
Find the (zero-based) index of the first city in the list whose name is "Dar Es Salaam" - 6
Answer 1: Index: [6] Population for Dar Es Salaam is 4.7 Million

Find the name of the first city in this list whose population is less than 5 million - Khartoum-Omdurman
Answer 2: Index [5] Population for Khartoum-Omdurman is 4.98 Million

Find the population of the first city in this list whose name starts with the letter "A" - 4.58
Answer 3: Index: [7] Population for Alexandria is 4.58 Million

Done...

```



## PicoLisp


```PicoLisp
(scl 2)

(de *Data
   ("Lagos"                21.0)
   ("Cairo"                15.2)
   ("Kinshasa-Brazzaville" 11.3)
   ("Greater Johannesburg" 7.55)
   ("Mogadishu"            5.85)
   ("Khartoum-Omdurman"    4.98)
   ("Dar Es Salaam"        4.7)
   ("Alexandria"           4.58)
   ("Abidjan"              4.4)
   ("Casablanca"           3.98) )

(test 6
   (dec (index (assoc "Dar Es Salaam" *Data) *Data)) )

(test "Khartoum-Omdurman"
   (car (find '((L) (> 5.0 (cadr L))) *Data)) )

(test 4.58
   (cadr (find '((L) (pre? "A" (car L))) *Data)) )
```



## PowerShell

The <code>ConvertFrom-Json</code> cmdlet converts a JSON formatted string to a custom PSCustomObject object that has a property for each field in the JSON string:

```PowerShell

$jsonData = @'
[
    { "Name": "Lagos",                "Population": 21.0  },
    { "Name": "Cairo",                "Population": 15.2  },
    { "Name": "Kinshasa-Brazzaville", "Population": 11.3  },
    { "Name": "Greater Johannesburg", "Population":  7.55 },
    { "Name": "Mogadishu",            "Population":  5.85 },
    { "Name": "Khartoum-Omdurman",    "Population":  4.98 },
    { "Name": "Dar Es Salaam",        "Population":  4.7  },
    { "Name": "Alexandria",           "Population":  4.58 },
    { "Name": "Abidjan",              "Population":  4.4  },
    { "Name": "Casablanca",           "Population":  3.98 }
]
'@

$cities = $jsonData | ConvertFrom-JSON

```

The <code>$cities</code> variable contains an array of objects with '''Name''' and '''Population''' properties:

```PowerShell

$cities

```

{{Out}}

```txt

Name                 Population
----                 ----------
Lagos                      21.0
Cairo                      15.2
Kinshasa-Brazzaville       11.3
Greater Johannesburg       7.55
Mogadishu                  5.85
Khartoum-Omdurman          4.98
Dar Es Salaam               4.7
Alexandria                 4.58
Abidjan                     4.4
Casablanca                 3.98

```

Find the index of the first city in the list whose name is "Dar Es Salaam":

```PowerShell

$cities.Name.IndexOf("Dar Es Salaam")

```

{{Out}}

```txt

6

```

Find the name of the first city in this list whose population is less than 5 million:

```PowerShell

($cities | Where-Object -Property Population -LT 5)[0].Name

```

{{Out}}

```txt

Khartoum-Omdurman

```

Find the population of the first city in this list whose name starts with the letter "A":

```PowerShell

($cities | Where-Object -Property Name -Match "^A")[0].Population

```

{{Out}}

```txt

4.58

```



## Python



```Python
cities = [
    { "name": "Lagos",                "population": 21.0  },
    { "name": "Cairo",                "population": 15.2  },
    { "name": "Kinshasa-Brazzaville", "population": 11.3  },
    { "name": "Greater Johannesburg", "population":  7.55 },
    { "name": "Mogadishu",            "population":  5.85 },
    { "name": "Khartoum-Omdurman",    "population":  4.98 },
    { "name": "Dar Es Salaam",        "population":  4.7  },
    { "name": "Alexandria",           "population":  4.58 },
    { "name": "Abidjan",              "population":  4.4  },
    { "name": "Casablanca",           "population":  3.98 }
]

def first(query):
    return next(query, None)

print(
    first(index for index, city in enumerate(cities)
        if city['name'] == "Dar Es Salaam"),
    first(city['name'] for city in cities if city['population'] < 5),
    first(city['population'] for city in cities if city['name'][0] == 'A'),
    sep='\n')
```


{{out}}

```txt

6
Khartoum-Omdurman
4.58

```



## Racket


The more idiomatic functions for the task is <code>findf</code> but it doesn't provide the position of the element in the list, so we write a variant. If the item is not found we return <code>#f</code> as most of the Racket primitives do in these cases.

```Racket

#lang racket

(define (findf/pos proc lst)
  (let loop ([lst lst] [pos 0])
    (cond
      [(null? lst) #f]
      [(proc (car lst)) pos]
      [else (loop (cdr lst) (add1 pos))])))

```

Now we define the list that has the data for the task.

```Racket

(define data '(#hash((name . "Lagos") (population . 21))
               #hash((name . "Cairo") (population .  15.2))
               #hash((name . "Kinshasa-Brazzaville") (population .  11.3))
               #hash((name . "Greater Johannesburg") (population .  7.55))
               #hash((name . "Mogadishu") (population .  5.85))
               #hash((name . "Khartoum-Omdurman") (population .  4.98))
               #hash((name . "Dar Es Salaam") (population .  4.7))
               #hash((name . "Alexandria") (population .  4.58))
               #hash((name . "Abidjan") (population .  4.4))
               #hash((name . "Casablanca") (population .  3.98))))

```

We write tiny wrappers to retrieve values from the hash.

```Racket

(define get-name
  (lambda (x) (hash-ref x 'name)))

(define get-population
  (lambda (x) (hash-ref x 'population)))

```

For completeness, ensure the data is sorted by population largest to smallest.

```Racket

(define sorted-data (sort data > #:key get-population))

```

Use an unnamed function with our findf/pos function to get the position of "Dar Es Salaam".

```Racket

(findf/pos (lambda (x) (equal? "Dar Es Salaam" (get-name x))) sorted-data)
;; -> 6

```

Use unnamed functions with findf for the other two test cases.

```Racket

(get-name (findf (lambda (x) (< (get-population x) 5)) sorted-data))
;; -> "Khartoum-Omdurman"
(get-population (findf (lambda (x) (regexp-match? #rx"^A" (get-name x))) sorted-data))
;; -> 4.58

```

{{out}}

```txt

6
"Khartoum-Omdurman"
4.58

```



## REXX

It is more idiomatic in REXX to use sparse arrays to express a list of CSV values, especially those which have

embedded blanks in them   (or other special characters).

Most REXX interpreters use (very efficient) hashing to index sparse arrays, which is much faster than performing an

incremental (sequential) search through an indexed array.

Only one loop is needed to find the result for the 2nd task requirement   (although the loop could be eliminated).

The other two task requirements are found without using traditional   '''IF'''   statements.

The approach taken in this REXX program makes use of a   '''DO WHILE'''   and   '''DO UNTIL'''   structure which

makes it much simpler (and idiomatic) and easier to code   (instead of adding multiple   '''IF'''   statements to a

generic search routine/function).

This REXX version does   ''not''   rely on the list being sorted by population count.

```rexx
/*REXX program (when using criteria) locates values (indices)  from an associate array. */
$="Lagos=21,  Cairo=15.2,  Kinshasa-Brazzaville=11.3, Greater Johannesburg=7.55, Mogadishu=5.85,",
  "Khartoum-Omdurman=4.98, Dar Es Salaam=4.7,  Alexandria=4.58,   Abidjan=4.4,  Casablanca=3.98"
@.= '(city not found)';    city.= "(no city)"       /*city search results for not found.*/
                                                    /* [↓]  construct associate arrays. */
    do #=0  while $\='';  parse var $ c '=' p "," $;  c=space(c);  parse var c a 2;  @.c=#
    city.#=c;  pop.#=p;  pop.c=#;  if @.a==@.  then @.a=c;  /*assign city, pop, indices.*/
    end   /*#*/                                     /* [↑]  city array starts at 0 index*/
                        /*▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ task 1:  show the  INDEX  of a city.*/
town= 'Dar Es Salaam'                               /*the name of a city for the search.*/
say 'The city of ' town " has an index of: " @.town /*show (zero─based) index of a city.*/
say                     /*▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ task 2:  show 1st city whose pop<5 M*/
many=5                                              /*size of a city's pop in millions. */
      do k=0  for #  until pop.k<many; end          /*find a city's pop from an index.  */
say '1st city that has a population less than '     many     " million is: "    city.k
say                     /*▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ task 3:  show 1st city with A* name.*/
c1= 'A'                                             /*1st character of a city for search*/
say '1st city that starts with the letter' c1 "is: " @.c1 /*stick a fork in it, all done*/
```

'''output'''   when using the default inputs:

```txt

The city of  Dar Es Salaam  has an index of:  6

1st city that has a population less than  5  million is:  Khartoum-Omdurman

1st city that starts with the letter A is:  Alexandria

```



## Ring


```ring

# Project : Search a list of records

cities = [[:name = "Lagos",:population = 21.0 ],
             [:name = "Cairo",:population =  15.2 ],
             [:name = "Kinshasa-Brazzaville",:population =  11.3 ],
             [:name = "Greater Johannesburg",:population =  7.55],
             [:name = "Mogadishu",:population =  5.85],
             [:name = "Khartoum-Omdurman",:population =  4.98],
             [:name = "Dar Es Salaam",:population =  4.7 ],
             [:name = "Alexandria",:population =  4.58],
             [:name = "Abidjan",:population =  4.4 ],
             [:name = "Casablanca",:population =  3.98]]

for n = 1 to len(cities)
     if cities[n][:name] = "Dar Es Salaam"
        see n-1 + nl
     ok
next

for n = 1 to len(cities)
     if cities[n][:population] < 5.00
        see cities[n][:name] + nl
        exit
     ok
next

for n = 1 to len(cities)
     if left(cities[n][:name],1) = "A"
        see cities[n][:population] + nl
        exit
     ok
next

```

Output:

```txt

6
Khartoum-Omdurman
4.58

```



## Ruby



```Ruby
cities = [
    {name: "Lagos", population: 21},
    {name: "Cairo", population: 15.2},
    {name: "Kinshasa-Brazzaville", population: 11.3},
    {name: "Greater Johannesburg", population: 7.55},
    {name: "Mogadishu", population: 5.85},
    {name: "Khartoum-Omdurman", population: 4.98},
    {name: "Dar Es Salaam", population: 4.7},
    {name: "Alexandria", population: 4.58},
    {name: "Abidjan", population: 4.4},
    {name: "Casablanca", population: 3.98},
]

puts cities.index{|city| city[:name] == "Dar Es Salaam"}      # => 6
puts cities.find {|city| city[:population] < 5.0}[:name]      # => Khartoum-Omdurman
puts cities.find {|city| city[:name][0] == "A"}[:population]  # => 4.58

```



## Rust


```Rust
struct City {
    name: &'static str,
    population: f64,
}

fn main() {
    let cities = [
        City {
            name: "Lagos",
            population: 21.0,
        },
        City {
            name: "Cairo",
            population: 15.2,
        },
        City {
            name: "Kinshasa-Brazzaville",
            population: 11.3,
        },
        City {
            name: "Greater Johannesburg",
            population: 7.55,
        },
        City {
            name: "Mogadishu",
            population: 5.85,
        },
        City {
            name: "Khartoum-Omdurman",
            population: 4.98,
        },
        City {
            name: "Dar Es Salaam",
            population: 4.7,
        },
        City {
            name: "Alexandria",
            population: 4.58,
        },
        City {
            name: "Abidjan",
            population: 4.4,
        },
        City {
            name: "Casablanca",
            population: 3.98,
        },
    ];

    println!(
        "{:?}",
        cities.iter().position(|city| city.name == "Dar Es Salaam")
    );
    println!(
        "{:?}",
        cities
            .iter()
            .find(|city| city.population < 5.0)
            .map(|city| city.name)
    );
    println!(
        "{:?}",
        cities
            .iter()
            .find(|city| city.name.starts_with('A'))
            .map(|city| city.population)
    );
}

```

{{Out}}

```txt
Some(6)
Some("Khartoum-Omdurman")
Some(4.58)
```



## Scala

{{Out}}See it in running in your browser by [https://scalafiddle.io/sf/8OlRzf8/0 ScalaFiddle (JavaScript executed in browser)] or by [https://scastie.scala-lang.org/fGhNDUNHRqq8h04x3Mm2Kw Scastie (remote JVM)].

```Scala
object SearchListOfRecords extends App {
  val cities = Vector(
    City("Lagos", 21.0e6),
    City("Cairo", 15.2e6),
    City("Kinshasa-Brazzaville", 11.3e6),
    City("Greater Johannesburg", 7.55e6),
    City("Mogadishu", 5.85e6),
    City("Khartoum-Omdurman", 4.98e6),
    City("Dar Es Salaam", 4.7e6),
    City("Alexandria", 4.58e6),
    City("Abidjan", 4.4e6),
    City("Casablanca", 3.98e6)
  )

  def index = cities.indexWhere((_: City).name == "Dar Es Salaam")

  def name = cities.find(_.pop < 5.0e6).map(_.name)

  def pop = cities.find(_.name(0) == 'A').map(_.pop)

  case class City(name: String, pop: Double)

  println(
    s"Index of first city whose name is 'Dar Es Salaam'          = $index\n" +
      s"Name of first city whose population is less than 5 million = ${name.get}\n" +
      f"Population of first city whose name starts with 'A'        = ${pop.get}%,.0f")

}
```

{{Out}}

```txt
Index of first city whose name is 'Dar Es Salaam'          = 6
Name of first city whose population is less than 5 million = Khartoum-Omdurman
Population of first city whose name starts with 'A'        = 4,580,000
```



## Scheme

{{libheader|Scheme/SRFIs}}

High-level functions for finding items in lists are provided by the (srfi 1) library:

* (find pred? list) returns the first item matching the given pred? function, or #f if none match
* (list-index pred? list) is similar but returns the index of the match (or #f)

The predicate can be used to pick out the part of a compound structure we want to find.

In a similar way (srfi 132) provides sorting routines with user-defined predicates: one is illustrated to ensure the list of items is sorted in decreasing population order.


```scheme

(import (scheme base)
        (scheme char)
        (scheme write)
        (srfi 1)    ; lists
        (srfi 132)) ; sorting

(define-record-type <places> ; compound data type is a record with two fields
                    (make-place name population)
                    place?
                    (name place-name)
                    (population place-population))

(define *items*
  (list-sort ; sort by decreasing population
    (lambda (r1 r2) (> (place-population r1)
                       (place-population r2)))
    (list (make-place "Lagos" 21.0)
          (make-place "Cairo" 15.2)
          (make-place "Kinshasa-Brazzaville" 11.3)
          (make-place "Greater Johannesburg" 7.55)
          (make-place "Mogadishu" 5.85)
          (make-place "Khartoum-Omdurman" 4.98)
          (make-place "Dar Es Salaam" 4.7)
          (make-place "Alexandria" 4.58)
          (make-place "Abidjan" 4.4)
          (make-place "Casablanca" 3.98))))

;; Find the (zero-based) index of the first city in the list
;; whose name is "Dar Es Salaam"
(display "Test 1: ")
(display (list-index (lambda (item)
                       (string=? "Dar Es Salaam" (place-name item)))
                     *items*))
(newline)

;; Find the name of the first city in this list
;; whose population is less than 5 million
(display "Test 2: ")
(display (place-name
           (find (lambda (item)
                   (< (place-population item) 5.0))
                 *items*)))
(newline)

;; Find the population of the first city in this list
;; whose name starts with the letter "A"
(display "Test 3: ")
(display (place-population
           (find (lambda (item)
                   (char=? (string-ref (place-name item) 0)
                           #\A))
                 *items*)))
(newline)

```


{{out}}

```txt

Test 1: 6
Test 2: Khartoum-Omdurman
Test 3: 4.58

```



## Sidef



```ruby
struct City {
    String name,
    Number population,
}

var cities = [
    City("Lagos", 21),
    City("Cairo", 15.2),
    City("Kinshasa-Brazzaville", 11.3),
    City("Greater Johannesburg", 7.55),
    City("Mogadishu", 5.85),
    City("Khartoum-Omdurman", 4.98),
    City("Dar Es Salaam", 4.7),
    City("Alexandria", 4.58),
    City("Abidjan", 4.4),
    City("Casablanca", 3.98),
]

say cities.index{|city| city.name == "Dar Es Salaam"}
say cities.first{|city| city.population < 5.0}.name
say cities.first{|city| city.name.begins_with("A")}.population
```


{{out}}

```txt

6
Khartoum-Omdurman
4.58

```



## Standard ML



```sml
type city = { name : string, population : real }

val citys : city list = [
        { name = "Lagos",                population = 21.0  },
        { name = "Cairo",                population = 15.2  },
        { name = "Kinshasa-Brazzaville", population = 11.3  },
        { name = "Greater Johannesburg", population =  7.55 },
        { name = "Mogadishu",            population =  5.85 },
        { name = "Khartoum-Omdurman",    population =  4.98 },
        { name = "Dar Es Salaam",        population =  4.7  },
        { name = "Alexandria",           population =  4.58 },
        { name = "Abidjan",              population =  4.4  },
        { name = "Casablanca",           population =  3.98 } ]

val firstCityi   = #1 (valOf (List.findi (fn (_, city) => #name(city) = "Dar Es Salaam") citys))
val firstBelow5M = #name (valOf (List.find (fn city => #population(city) < 5.0) citys))
val firstPopA    = #population (valOf (List.find (fn city => String.substring (#name(city), 0, 1) = "A") citys))
```


{{Output}}

```txt
val firstCityi = 6 : int
val firstBelow5M = "Khartoum-Omdurman" : string
val firstPopA = 4.58 : real
```



## Tcl


```Tcl
# records is a list of dicts.
set records {
  { name "Lagos"                population 21.0  }
  { name "Cairo"                population 15.2  }
  { name "Kinshasa-Brazzaville" population 11.3  }
  { name "Greater Johannesburg" population  7.55 }
  { name "Mogadishu"            population  5.85 }
  { name "Khartoum-Omdurman"    population  4.98 }
  { name "Dar Es Salaam"        population  4.7  }
  { name "Alexandria"           population  4.58 }
  { name "Abidjan"              population  4.4  }
  { name "Casablanca"           population  3.98 }
}

# Tcl's version of "higher order programming" is a bit unusual.  Instead of passing lambda
# functions, it is often easier to pass script fragments.  This command takes two such
# arguments: $test is an expression (as understood by [expr]), and $action is a script.
# thanks to [dict with], both $test and $action can refer to the fields of the current
# record by name - or to other variables used in the proc, like $index or $record.
proc search {records test action} {
    set index 0
    foreach record $records {
        dict with record {}
        if $test $action
        incr index
    }
    error "No match found!"
}

#  Find the (zero-based) index of the first city in the list whose name is "Dar Es Salaam"
puts [search $records {$name eq "Dar Es Salaam"} {return $index}]
#  Find the name of the first city in this list whose population is less than 5 million
puts [search $records {$population < 5.0} {return $name}]
#  Find the population of the first city in this list whose name starts with the letter "A"
puts [search $records {[string match A* $name]} {return $population}]

```


{{out}}

```txt
6
Khartoum-Omdurman
4.58
```



## zkl


```zkl
list:=T(SD("name","Lagos", 		  "population",21.0), // SD is a fixed dictionary
        SD("name","Cairo", 		  "population",15.2),
	SD("name","Kinshasa-Brazzaville", "population",11.3),
	SD("name","Greater Johannesburg", "population", 7.55),
	SD("name","Mogadishu", 		  "population", 5.85),
	SD("name","Khartoum-Omdurman", 	  "population", 4.98),
	SD("name","Dar Es Salaam", 	  "population", 4.7),
	SD("name","Alexandria", 	  "population", 4.58),
	SD("name","Abidjan", 		  "population", 4.4),
	SD("name","Casablanca", 	  "population", 3.98));

// Test case 1:
n:=list.filter1n(fcn(city){ city["name"]=="Dar Es Salaam" });  // one way
n:=list.filter1n(fcn(city){ city["name"].matches("dar es salaam") }); // or this way
n.println("==index of ",list[n].values);

// Test case 2:
city:=list.filter1(fcn(city){ city["population"]<5.0 });  // stop after first match
city["name"].println(" is the first city with population under 5 million.");

// Test case 3:
city:=list.filter1(fcn(city){ city["name"][0]=="A" });
println("The first \"A*\" city (%s) with population under 5 million: %f".fmt(city.values.xplode()));
```

where a SD is a small read only dictionary and filter1 is a filter that stops at the first match (returning the matched item). The filter method returns False on failure.
The YAJL library could be used to parse the JSON data directly (eg if the data is from the web).
{{out}}

```txt

6==index of L("Dar Es Salaam",4.7)
Khartoum-Omdurman is the first city with population under 5 million.
The first "A*" city (Alexandria) with population under 5 million: 4.580000

```

