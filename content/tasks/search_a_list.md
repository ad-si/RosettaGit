+++
title = "Search a list"
description = ""
date = 2019-10-18T19:15:49Z
aliases = []
[extra]
id = 3057
[taxonomies]
categories = ["task", "Text processing"]
tags = []
languages = [
  "acl2",
  "actionscript",
  "ada",
  "aime",
  "algol_68",
  "autohotkey",
  "awk",
  "basic",
  "batch_file",
  "bbc_basic",
  "bracmat",
  "burlesque",
  "c",
  "ceylon",
  "clojure",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "dwscript",
  "e",
  "elena",
  "elixir",
  "erlang",
  "euphoria",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "gambas",
  "gap",
  "go",
  "groovy",
  "haskell",
  "hicest",
  "io",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "kotlin",
  "lang5",
  "lasso",
  "liberty_basic",
  "lingo",
  "lisaac",
  "logo",
  "lua",
  "m2000_interpreter",
  "maple",
  "mathematica",
  "matlab",
  "maxima",
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
  "php",
  "picolisp",
  "pl_i",
  "powerbasic",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rebol",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "sather",
  "scala",
  "scheme",
  "sidef",
  "slate",
  "smalltalk",
  "standard_ml",
  "swift",
  "tcl",
  "torquescript",
  "tuscript",
  "unix_shell",
  "ursala",
  "vba",
  "vbscript",
  "wart",
  "xpl0",
  "yabasic",
  "yorick",
  "zkl",
]
+++

## Task

Find the index of a string (needle) in an indexable, ordered collection of strings (haystack).

Raise an exception if the needle is missing.

If there is more than one occurrence then return the smallest index to the needle.

Return the largest index to a needle that has multiple occurrences in the haystack.

* [[Search a list of records]]

<hr>


## ACL2


```lisp
(defun index-of-r (e xs i)
   (cond ((endp xs) nil)
         ((equal e (first xs)) i)
         (t (index-of-r e (rest xs) (1+ i)))))

(defun index-of (e xs)
   (index-of-r e xs 0))
```



## ActionScript

===Using the built-in Error class===

```ActionScript>var list:Vector.<String> = Vector.<String
(["Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty", "Charlie", "Bush", "Boz", "Zag"]);
function lowIndex(listToSearch:Vector.<String>, searchString:String):int
{
	var index:int = listToSearch.indexOf(searchString);
	if(index == -1)
		throw new Error("String not found: " + searchString);
	return index;
}

function highIndex(listToSearch:Vector.<String>, searchString:String):int
{
	var index:int = listToSearch.lastIndexOf(searchString);
	if(index == -1)
		throw new Error("String not found: " + searchString);
	return index;
}
```



### Using a custom error

In StringNotFoundError.as:

```ActionScript
package {
	public class StringNotFoundError extends Error {
		public function StringNotFoundError(message:String) {
			super(message);
		}
	}
}
```

In a separate file:

```ActionScript
import StringNotFoundError;
var list:Vector.<String> = Vector.<String>(["Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty", "Charlie", "Bush", "Boz", "Zag"]);
function lowIndex(listToSearch:Vector.<String>, searchString:String):int
{
	var index:int = listToSearch.indexOf(searchString);
	if(index == -1)
		throw new StringNotFoundError("String not found: " + searchString);
	return index;
}

function highIndex(listToSearch:Vector.<String>, searchString:String):int
{
	var index:int = listToSearch.lastIndexOf(searchString);
	if(index == -1)
		throw new StringNotFoundError("String not found: " + searchString);
	return index;
}

```


## Ada


```ada
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Text_IO;            use Ada.Text_IO;

procedure Test_List_Index is
   Not_In : exception;

   type List is array (Positive range <>) of Unbounded_String;

   function Index (Haystack : List; Needle : String) return Positive is
   begin
      for Index in Haystack'Range loop
         if Haystack (Index) = Needle then
            return Index;
         end if;
      end loop;
      raise Not_In;
   end Index;

      -- Functions to create lists
   function "+" (X, Y : String) return List is
   begin
      return (1 => To_Unbounded_String (X), 2 => To_Unbounded_String (Y));
   end "+";

   function "+" (X : List; Y : String) return List is
   begin
      return X & (1 => To_Unbounded_String (Y));
   end "+";

   Haystack : List := "Zig"+"Zag"+"Wally"+"Ronald"+"Bush"+"Krusty"+"Charlie"+"Bush"+"Bozo";

   procedure Check (Needle : String) is
   begin
      Put (Needle);
      Put_Line ("at" & Positive'Image (Index (Haystack, Needle)));
   exception
      when Not_In => Put_Line (" is not in");
   end Check;
begin
   Check ("Washington");
   Check ("Bush");
end Test_List_Index;
```

```txt

Washington is not in
Bushat 5

```



## Aime


```aime
void
search(list l, text s)
{
    integer i;

    i = 0;
    while (i < ~l) {
        if (l[i] == s) {
            break;
        }
        i += 1;
    }

    o_(s, " is ", i == ~l ? "not in the haystack" : "at " + itoa(i), "\n");
}

integer
main(void)
{
    list l;

    l = l_effect("Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty",
                 "Charlie", "Bush", "Boz", "Zag");
    __ucall(search, 1, 1, l, "Bush", "Washington", "Zag");

    return 0;
}
```

```txt
Bush is at 4
Washington is not in the haystack
Zag is at 1
```



## ALGOL 68

===Using a FORMAT "value error" exception===
```algol68
 FORMAT hay stack := $c("Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo")$;

 FILE needle exception; STRING ref needle;
 associate(needle exception, ref needle);

 PROC index = (FORMAT haystack, REF STRING needle)INT:(
   INT out;
   ref needle := needle;
   getf(needle exception,(haystack, out));
   out
 );

 test:(
   []STRING needles = ("Washington","Bush");
   FOR i TO UPB needles DO
     STRING needle := needles[i];
     on value error(needle exception, (REF FILE f)BOOL: value error);
       printf(($d" "gl$,index(hay stack, needle), needle));
       end on value error;
     value error:
       printf(($g" "gl$,needle, "is not in haystack"));
     end on value error: reset(needle exception)
   OD
 )
```

```txt

Washington is not in haystack
5 Bush

```



### Using a manual FOR loop with no exception

```algol68
 []STRING hay stack = ("Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo");

 PROC index = ([]STRING hay stack, STRING needle)INT:(
   INT index;
   FOR i FROM LWB hay stack TO UPB hay stack DO
     index := i;
     IF hay stack[index] = needle THEN
       found
     FI
   OD;
   else:
     LWB hay stack - 1
   EXIT
   found:
     index
 );
 test:(
   []STRING needles = ("Washington","Bush");
   FOR i TO UPB needles DO
     STRING needle := needles[i];
     INT result = index(hay stack, needle);
     IF result >= LWB hay stack THEN
       printf(($d" "gl$, result, needle))
     ELSE
       printf(($g" "gl$,needle, "is not in haystack"))
     FI
   OD
 )
```

```txt

Washington is not in haystack
5 Bush

```



## AutoHotkey


```AutoHotkey
haystack = Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo
needle = bush, washington
Loop, Parse, needle, `,
{
  If InStr(haystack, A_LoopField)
    MsgBox, % A_LoopField
  Else
    MsgBox % A_LoopField . " not in haystack"
}
```



## AWK


If we use an awk array indexed with "the order" of the string, to check if the needle is in the haystack we must walk the whole array; if we use the string itself as ''index'' (in awk index for an array is indeed an hash), and put its "index" (order number in the list) as associated value, we can fastly check if the needle is in the haystack. But we can't fastly use its order number to get the string value at that position.

In the following implementation we can reach the strings by numeric index with the array <code>haystack_byorder</code> (so, e.g. <code>haystack_byorder[4]</code> gives Bush), and know the "position" of the needle (if it exists) using it as string index for the array <code>haystack</code>, as example does. ('''Beware''': this method does not work when there are duplicates!)


```awk
#! /usr/bin/awk -f
BEGIN {
    # create the array, using the word as index...
    words="Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo";
    split(words, haystack_byorder, " ");
    j=0;
    for(idx in haystack_byorder) {
	haystack[haystack_byorder[idx]] = j;
	j++;
    }
    # now check for needle (we know it is there, so no "else")...
    if ( "Bush" in haystack ) {
	print "Bush is at " haystack["Bush"];
    }
    # check for unexisting needle
    if ( "Washington" in haystack ) {
	print "impossible";
    } else {
	print "Washington is not here";
    }
}
```



## BASIC

```qbasic
DATA foo, bar, baz, quux, quuux, quuuux, bazola, ztesch, foo, bar, thud, grunt
DATA foo, bar, bletch, foo, bar, fum, fred, jim, sheila, barney, flarp, zxc
DATA spqr, wombat, shme, foo, bar, baz, bongo, spam, eggs, snork, foo, bar
DATA zot, blarg, wibble, toto, titi, tata, tutu, pippo, pluto, paperino, aap
DATA noot, mies, oogle, foogle, boogle, zork, gork, bork

DIM haystack(54) AS STRING
DIM needle AS STRING, found AS INTEGER, L0 AS INTEGER

FOR L0 = 0 TO 54
    READ haystack(L0)
NEXT

DO
    INPUT "Word to search for? (Leave blank to exit) ", needle
    IF needle <> "" THEN
        FOR L0 = 0 TO UBOUND(haystack)
            IF UCASE$(haystack(L0)) = UCASE$(needle) THEN
                found = 1
                PRINT "Found "; CHR$(34); needle; CHR$(34); " at index "; LTRIM$(STR$(L0))
            END IF
        NEXT
        IF found < 1 THEN
            PRINT CHR$(34); needle; CHR$(34); " not found"
        END IF
    ELSE
        EXIT DO
    END IF
LOOP
```


```txt

 Word to search for? (Leave blank to exit) foo
 Found "foo" at index 0
 Found "foo" at index 8
 Found "foo" at index 12
 Found "foo" at index 15
 Found "foo" at index 27
 Found "foo" at index 34
 Word to search for? (Leave blank to exit) bar
 Found "bar" at index 1
 Found "bar" at index 9
 Found "bar" at index 13
 Found "bar" at index 16
 Found "bar" at index 28
 Found "bar" at index 35
 Word to search for? (Leave blank to exit) baz
 Found "baz" at index 2
 Found "baz" at index 29
 Word to search for? (Leave blank to exit)

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Search.bas"
110 STRING A$(1 TO 55)*8
120 FOR I=1 TO 55
130   READ A$(I)
140   PRINT A$(I);" ";
150 NEXT
160 DO
170   PRINT :INPUT PROMPT "Word to seatch for? (Leave blank to exit) ":S$
180   LET S$=LCASE$(LTRIM$(RTRIM$(S$))):LET FOUND=0
190   IF S$="" THEN EXIT DO
200   FOR I=LBOUND(A$) TO UBOUND(A$)
210     IF A$(I)=S$ THEN LET FOUND=-1:PRINT "Found """;S$;""" at index";I
220   NEXT
230   IF NOT FOUND THEN PRINT """";S$;""" not found."
240 LOOP
250 DATA foo,bar,baz,quux,quuux,quuuux,bazola,ztesch,foo,bar,thud,grunt,foo,bar,bletch,foo,bar,fum,fred,jim,sheila,barney,flarp,zxc
260 DATA spqr,wombat,shme,foo,bar,baz,bongo,spam,eggs,snork,foo,bar,zot,blarg,wibble,toto,titi,tata,tutu,pippo,pluto,paperino,aap,noot,mies,oogle,foogle,boogle,zork,gork,bork
```



## Batch File

The index of this simple implementation is 1-based. The "haystack" data are borrowed from the [[Search_a_list#BASIC|BASIC]] implementation.

```dos
@echo off
setlocal enabledelayedexpansion

	%==Sample list==%
set "data=foo, bar, baz, quux, quuux, quuuux, bazola, ztesch, foo, bar, thud, grunt"
set "data=%data% foo, bar, bletch, foo, bar, fum, fred, jim, sheila, barney, flarp, zxc"
set "data=%data% spqr, wombat, shme, foo, bar, baz, bongo, spam, eggs, snork, foo, bar"
set "data=%data% zot, blarg, wibble, toto, titi, tata, tutu, pippo, pluto, paperino, aap"
set "data=%data% noot, mies, oogle, foogle, boogle, zork, gork, bork"

	%==Sample "needles" [whitespace is the delimiter]==%
set "needles=foo bar baz jim bong"

	%==Counting and Seperating each Data==%
set datalen=0
for %%. in (!data!) do (
	set /a datalen+=1
	set data!datalen!=%%.
)
	%==Do the search==%
for %%A in (!needles!) do (
	set "first="
	set "last="
	set "found=0"
	for /l %%B in (1,1,%datalen%) do (
		if "!data%%B!" == "%%A" (
			set /a found+=1
			if !found! equ 1 set first=%%B
			set last=%%B
		)
	)

	if !found! equ 0 echo."%%A": Not found.
	if !found! equ 1 echo."%%A": Found once in index [!first!].
	if !found! gtr 1 echo."%%A": Found !found! times. First instance:[!first!] Last instance:[!last!].

)
	%==We are done==%
echo.
pause
```

```txt
"foo": Found 6 times. First instance:[1] Last instance:[35].
"bar": Found 6 times. First instance:[2] Last instance:[36].
"baz": Found 2 times. First instance:[3] Last instance:[30].
"jim": Found once in index [20].
"bong": Not found.

Press any key to continue . . .
```



## BBC BASIC

```bbcbasic
      DIM haystack$(27)
      haystack$() = "alpha","bravo","charlie","delta","echo","foxtrot","golf",   \
      \             "hotel","india","juliet","kilo","lima","mike","needle",      \
      \             "november","oscar","papa","quebec","romeo","sierra","tango", \
      \             "needle","uniform","victor","whisky","x-ray","yankee","zulu"

      needle$ = "needle"
      maxindex% = DIM(haystack$(), 1)

      FOR index% = 0 TO maxindex%
        IF needle$ = haystack$(index%) EXIT FOR
      NEXT
      IF index% <= maxindex% THEN
        PRINT "First found at index "; index%
        FOR last% = maxindex% TO 0 STEP -1
          IF needle$ = haystack$(last%) EXIT FOR
        NEXT
        IF last%<>index% PRINT "Last found at index "; last%
      ELSE
        ERROR 100, "Not found"
      ENDIF
```



## Bracmat

For both subtasks, pattern matching is used. The second subtasks proceeds in two steps. First, the first word that occurs twice is found (if it exists). Then, the last occurrence of this word is found using forced backtracking (see the <code>~</code> node) until failure.

```Bracmat
(     return the largest index to a needle that has multiple
      occurrences in the haystack and print the needle
  : ?list
& (   !list:? haystack [?index ?
    & out$("The word 'haystack' occurs at 1-based index" !index)
  | out$"The word 'haystack' does not occur"
  )
& (   !list
    : ? %@?needle ? !needle ?
    : ( ? !needle [?index (?&~)
      |   ?
        &   out
          $ ( str
            $ ( "The word '"
                !needle
                "' occurs more than once. The last 1-based index is "
                !index
              )
            )
      )
  | out$"No word occurs more than once."
  )
);
```

```txt
The word 'haystack' occurs at 1-based index 14
The word 'the' occurs more than once. The last 1-based index is 17
```



## Burlesque



```burlesque
blsq ) {"Zig" "Zag" "Wally" "Bush" "Ronald" "Bush"}"Bush"Fi
3
```


If you want all indices:


```burlesque
blsq ) {"Zig" "Zag" "Wally" "Bush" "Ronald" "Bush"}{"Bush"==}fI
{3 5}
```



## C



```c
#include <stdio.h>
#include <string.h>

const char *haystack[] = {
  "Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty", "Charlie",
  "Bush", "Boz", "Zag", NULL
};

int search_needle(const char *needle, const char **hs)
{
  int i = 0;
  while( hs[i] != NULL ) {
    if ( strcmp(hs[i], needle) == 0 ) return i;
    i++;
  }
  return -1;
}

int search_last_needle(const char *needle, const char **hs)
{
  int i, last=0;
  i = last = search_needle(needle, hs);
  if ( last < 0 ) return -1;
  while( hs[++i] != NULL ) {
    if ( strcmp(needle, hs[i]) == 0 ) {
      last = i;
    }
  }
  return last;
}

int main()
{
  printf("Bush is at %d\n", search_needle("Bush", haystack));
  if ( search_needle("Washington", haystack) == -1 )
    printf("Washington is not in the haystack\n");
  printf("First index for Zag: %d\n", search_needle("Zag", haystack));
  printf("Last index for Zag: %d\n", search_last_needle("Zag", haystack));
  return 0;
}
```


```txt
Bush is at 4
Washington is not in the haystack
First index for Zag: 1
Last index for Zag: 9
```



## C++

The following code shows three different ways to solve the task.


```cpp
#include <string>
#include <algorithm>
#include <iterator>
#include <cstddef>
#include <exception>
#include <iostream>

// an exception to throw (actually, throwing an exception in this case is generally considered bad style, but it's part of the task)
class not_found: public std::exception
{
public:
  not_found(std::string const& s): text(s + " not found") {}
  char const* what() const throw() { return text.c_str(); }
  ~not_found() throw() {}
private:
  std::string text;
};

// needle search function, C-style interface version using standard library
std::size_t get_index(std::string* haystack, int haystack_size, std::string needle)
{
  std::size_t index = std::find(haystack, haystack+haystack_size, needle) - haystack;
  if (index == haystack_size)
    throw not_found(needle);
  else
    return index;
}

// needle search function, completely generic style, needs forward iterators
// (works with any container, but inefficient if not random-access-iterator)
template<typename FwdIter>
 typename std::iterator_traits<FwdIter>::difference_type fwd_get_index(FwdIter first, FwdIter last, std::string needle)
{
  FwdIter elem = std::find(first, last, needle);
  if (elem == last)
    throw not_found(needle);
  else
    return std::distance(first, elem);
}

// needle search function, implemented directly, needs only input iterator, works efficiently with all sequences
template<typename InIter>
 typename std::iterator_traits<InIter>::difference_type generic_get_index(InIter first, InIter last, std::string needle)
{
  typename std::iterator_traits<InIter>::difference_type index = 0;
  while (first != last && *first != needle)
  {
    ++index;
    ++first;
  }
  if (first == last)
    throw not_found(needle);
  else
    return index;
}

// ----------------------------------------------------------------------------------------------------------------------------------

// a sample haystack (content copied from Haskell example)
std::string haystack[] = { "Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty", "Charlie", "Bush", "Bozo" };

// some useful helper functions
template<typename T, std::size_t sz> T* begin(T (&array)[sz]) { return array; }
template<typename T, std::size_t sz> T* end(T (&array)[sz]) { return array + sz; }
template<typename T, std::size_t sz> std::size_t size(T (&array)[sz]) { return sz; }

// test function searching a given needle with each of the methods
void test(std::string const& needle)
{
  std::cout << "-- C style interface --\n";
  try
  {
    std::size_t index = get_index(haystack, size(haystack), needle);
    std::cout << needle << " found at index " << index << "\n";
  }
  catch(std::exception& exc) // better catch standard exceptions as well; me might e.g. run out of memory
  {
    std::cout << exc.what() << "\n";
  }

  std::cout << "-- generic interface, first version --\n";
  try
  {
    std::size_t index = fwd_get_index(begin(haystack), end(haystack), needle);
    std::cout << needle << " found at index " << index << "\n";
  }
  catch(std::exception& exc) // better catch standard exceptions as well; me might e.g. run out of memory
  {
    std::cout << exc.what() << "\n";
  }

  std::cout << "-- generic interface, second version --\n";
  try
  {
    std::size_t index = generic_get_index(begin(haystack), end(haystack), needle);
    std::cout << needle << " found at index " << index << "\n";
  }
  catch(std::exception& exc) // better catch standard exceptions as well; me might e.g. run out of memory
  {
    std::cout << exc.what() << "\n";
  }
}

int main()
{
  std::cout << "\n
###  Word which only occurs once
\n";
  test("Wally");
  std::cout << "\n
###  Word occuring multiple times
\n";
  test("Bush");
  std::cout << "\n
###  Word not occuring at all
\n";
  test("Goofy");
}
```


{{out}} (note that in C++, indices start at 0):

```txt



###  Word which only occurs once

-- C style interface --
Wally found at index 2
-- generic interface, first version --
Wally found at index 2
-- generic interface, second version --
Wally found at index 2


###  Word occuring multiple times

-- C style interface --
Bush found at index 4
-- generic interface, first version --
Bush found at index 4
-- generic interface, second version --
Bush found at index 4


###  Word not occuring at all

-- C style interface --
Goofy not found
-- generic interface, first version --
Goofy not found
-- generic interface, second version --
Goofy not found

```



###  C++11

```cpp
/* new c++-11 features
 * list class
 * initialization strings
 * auto typing
 * lambda functions
 * noexcept
 * find
 * for/in loop
 */

#include <iostream>   // std::cout, std::endl
#include <algorithm>  // std::find
#include <list>       // std::list
#include <vector>     // std::vector
#include <string>     // string::basic_string


using namespace std;     // saves typing of "std::" before everything

int main()
{

  // initialization lists
  // create objects and fully initialize them with given values

  list<string> l { "Zig", "Zag", "Wally", "Homer", "Madge",
                   "Watson", "Ronald", "Bush", "Krusty", "Charlie",
                   "Bush", "Bush", "Boz", "Zag" };

  list<string> n { "Bush" , "Obama", "Homer",  "Sherlock" };


  //  lambda function with auto typing
  //  auto is easier to write than looking up the compicated
  //  specialized iterator type that is actually returned.
  //  Just know that it returns an iterator for the list at the position found,
  //  or throws an exception if s in not in the list.
  //  runtime_error is used because it can be initialized with a message string.

  auto contains = [](list<string> l, string s) throw(runtime_error)
    {
      auto r = find(begin(l), end(l), s );

      if ( r == end(l) ) throw runtime_error( s + " not found" );

      return r;
    };


  // returns an int vector with the indexes of the search string
  // The & is a "default capture" meaning that it "allows in"
  // the variables that are in scope where it is called by their
  // name to simplify things.

  auto index = [&](list<string> l, string s) noexcept
    {
      vector<int> index_v;

      int idx = 0;

      for(auto& r : l)
	{
	  if ( s.compare(r) == 0 ) index_v.push_back(idx); // match -- add to vector
	  idx++;
	}

      // even though index_v is local to the lambda function,
      // c++11 move semantics does what you want and returns it
      // live and intact instead of destroying it or returning a copy.
      // (very efficient for large objects!)
      return index_v;
    };



  // for/in loop
  for (const string& s : n) // new iteration syntax is simple and intuitive
    {
      try
	{

	  auto cont = contains( l , s); // checks if there is any match

	  vector<int> vf = index( l, s );

	  cout << "l contains: " << s <<  " at " ;

	  for (auto x : vf)
	    { cout << x << " "; }   // if vector is empty this doesn't run

	  cout << endl ;

      }
      catch (const runtime_error& r)  // string not found
	{
	  cout << r.what() << endl;
	  continue;                   // try next string
	}
    } //for


  return 0;

} // main

/* end */
```

```txt

l contains: Bush at 7 10 11
Obama not found
l contains: Homer at 3
Sherlock not found

```


## C#


```c#
using System;
using System.Collections.Generic;

class Program {
    static void Main(string[] args) {
        List<string> haystack = new List<string>() { "Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty", "Charlie", "Bush", "Bozo" };

        foreach (string needle in new string[] { "Washington", "Bush" }) {
            int index = haystack.IndexOf(needle);

            if (index < 0) Console.WriteLine("{0} is not in haystack",needle);
            else Console.WriteLine("{0} {1}",index,needle);
        }
    }
}
```



## Ceylon


```ceylon
shared test void searchAListTask() {
    value haystack = [
            "Zig", "Zag", "Wally", "Ronald", "Bush",
            "Krusty", "Charlie", "Bush", "Bozo"];

    assert(exists firstIdx = haystack.firstOccurrence("Bush"));
    assert(exists lastIdx = haystack.lastOccurrence("Bush"));

    assertEquals(firstIdx, 4);
    assertEquals(lastIdx, 7);
}
```



## Clojure


```clojure
(let [haystack ["Zig" "Zag" "Wally" "Ronald" "Bush" "Krusty" "Charlie" "Bush" "Bozo"]]
  (let [idx (.indexOf haystack "Zig")]
    (if (neg? idx)
      (throw (Error. "item not found."))
      idx)))
```


Extra credit: Since Clojure vectors implement java.util.List, you can switch .indexOf for .lastIndexOf to find the highest index of your value.


## COBOL


```cobol
*> This is written to COBOL85, which does not include exceptions.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Search-List.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  haystack-area.
           78  Haystack-Size VALUE 10.
           03  haystack-data.
               05  FILLER     PIC X(7) VALUE "Zig".
               05  FILLER     PIC X(7) VALUE "Zag".
               05  FILLER     PIC X(7) VALUE "Wally".
               05  FILLER     PIC X(7) VALUE "Ronald".
               05  FILLER     PIC X(7) VALUE "Bush".
               05  FILLER     PIC X(7) VALUE "Krusty".
               05  FILLER     PIC X(7) VALUE "Charlie".
               05  FILLER     PIC X(7) VALUE "Bush".
               05  FILLER     PIC X(7) VALUE "Boz".
               05  FILLER     PIC X(7) VALUE "Zag".

           03  haystack-table REDEFINES haystack-data.
               05  haystack   PIC X(7) OCCURS Haystack-Size TIMES
                   INDEXED BY haystack-index.

       01  needle             PIC X(7).

       PROCEDURE DIVISION.
       main.
           MOVE "Bush" TO needle
           PERFORM find-needle

           MOVE "Goofy" TO needle
           PERFORM find-needle

*          *> Extra task
           MOVE "Bush" TO needle
           PERFORM find-last-of-needle

           GOBACK
           .

       find-needle.
           SEARCH haystack
               AT END
                   DISPLAY needle " not found."

               WHEN haystack (haystack-index) = needle
                   DISPLAY "Found " needle " at " haystack-index "."
           END-SEARCH
           .

       find-last-of-needle.
           PERFORM VARYING haystack-index FROM Haystack-Size BY -1
               UNTIL haystack-index = 0
               OR haystack (haystack-index) = needle
           END-PERFORM

           IF haystack-index = 0
               DISPLAY needle " not found."
           ELSE
               DISPLAY "Found last of " needle " at " haystack-index "."
           END-IF
           .
```


```txt

Found Bush    at +000000005.
Goofy   not found.
Found last of Bush    at +000000008.

```



## Common Lisp


```lisp
(let ((haystack '(Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo)))
  (dolist (needle '(Washington Bush))
    (let ((index (position needle haystack)))
      (if index
          (progn (print index) (princ needle))
          (progn (print needle) (princ "is not in haystack"))))))
```

```txt

WASHINGTON is not in haystack
4 BUSH

```

The position function solves this task elegantly.

```lisp

CL-USER> (defparameter *list* '(Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo))
*LIST*
CL-USER> (position 'Bush *list*)
4
CL-USER> (position 'Bush *list* :from-end t)
7
CL-USER> (position 'Washington *list*)
NIL
```



## D


```d
import std.algorithm, std.range, std.string;

auto firstIndex(R, T)(R hay, T needle) {
  auto i = countUntil(hay, needle);
  if (i == -1)
    throw new Exception("No needle found in haystack");
  return i;
}

auto lastIndex(R, T)(R hay, T needle) {
  return walkLength(hay) - firstIndex(retro(hay), needle) - 1;
}

void main() {
  auto h = split("Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo");
  assert(firstIndex(h, "Bush") == 4);
  assert(lastIndex(h, "Bush") == 7);
}
```



## Delphi



```Delphi
program Needle;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes;

var
  list: TStringList;
  needle: string;
  ind: Integer;
begin
  list := TStringList.Create;
  try
    list.Append('triangle');
    list.Append('fork');
    list.Append('limit');
    list.Append('baby');
    list.Append('needle');

    list.Sort;

    needle := 'needle';
    ind := list.IndexOf(needle);
    if ind < 0 then
      raise Exception.Create('Needle not found')
    else begin
      Writeln(ind);
      Writeln(list[ind]);
    end;

    Readln;
  finally
    list.Free;
  end;
end.
```

```txt

3
needle

```



## DWScript



```Delphi
var haystack : array of String = ["Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo"];

function Find(what : String) : Integer;
begin
   Result := haystack.IndexOf(what);
   if Result < 0 then
      raise Exception.Create('not found');
end;

PrintLn(Find("Ronald")); // 3
PrintLn(Find('McDonald')); // exception
```



## E



```e
def haystack := ["Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo"]

/** meet the 'raise an exception' requirement */
def find(needle) {
    switch (haystack.indexOf1(needle)) {
        match ==(-1) { throw("an exception") }
        match index { return index }
    }
}

println(find("Ronald")) # prints 3
println(find("McDonald")) # will throw
```


## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;

public program()
{
    var haystack := new string[]::("Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty", "Charlie", "Bush", "Bozo");

    new string[]::("Washington", "Bush").forEach:(needle)
    {
        var index := haystack.indexOfElement:needle;

        if (index == -1)
        {
            console.printLine(needle," is not in haystack")
        }
        else
        {
            console.printLine(needle, " - ", index)
        }
    }
}
```



## Elixir


```elixir
haystack = ~w(Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo)

Enum.each(~w(Bush Washington), fn needle ->
  index = Enum.find_index(haystack, fn x -> x==needle end)
  if index, do: (IO.puts "#{index} #{needle}"),
            else: raise "#{needle} is not in haystack\n"
end)
```


```txt

4 Bush
** (RuntimeError) Washington is not in haystack

    search.exs:5: anonymous fn/1 in :elixir_compiler_0.__FILE__/1
    (elixir) lib/enum.ex:537: Enum."-each/2-lists^foreach/1-0-"/2
    (elixir) lib/enum.ex:537: Enum.each/2
    (elixir) lib/code.ex:316: Code.require_file/2

```



## Erlang


Erlang lists can be accessed with the function lists:nth/2, which starts at 1 (first element). As such Erlang can be considered 1-indexed for this problem.
Note that you could set the indexing to 0 by modifying the function call in pos/2.

```erlang
-module(index).
-export([main/0]).

main() ->
    Haystack = ["Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo"],
    Needles = ["Washington","Bush"],
    lists:foreach(fun ?MODULE:print/1, [{N,pos(N, Haystack)} || N <- Needles]).

pos(Needle, Haystack) -> pos(Needle, Haystack, 1).
pos(_, [], _) -> undefined;
pos(Needle, [Needle|_], Pos) -> Pos;
pos(Needle, [_|Haystack], Pos) -> pos(Needle, Haystack, Pos+1).

print({Needle, undefined}) -> io:format("~s is not in haystack.~n",[Needle]);
print({Needle, Pos}) -> io:format("~s at position ~p.~n",[Needle,Pos]).
```


```txt

Washington is not in haystack.
Bush at position 5.

```



## Euphoria

The find_all function from the standard library's search.e does nearly all the needed work here.There may be other ways to do this using Euphoria's various sequence searching functions as part of the standard library (std/search.e) and/or built into the language.
The procedure can be made into a function to search with other strings, take user input and give output of the searched haystack.


```euphoria

include std/search.e
include std/console.e

--the string "needle" and example haystacks to test the procedure
sequence searchStr1 = "needle"
sequence haystack1 = { "needle", "needle", "noodle", "node", "need", "needle  ", "needle" }
sequence haystack2 = {"spoon", "fork", "hay", "knife", "needle", "barn", "etcetera", "more hay", "needle", "a cow", "farmer", "needle", "dirt"}
sequence haystack3 = {"needle"}
sequence haystack4 = {"no", "need le s", "in", "this", "haystack"}
sequence haystack5 = {"knee", "needle", "dull", "needle"}
sequence haystack6 = {}

--search procedure with console output
procedure haystackSearch(sequence hStack)
    sequence foundNeedles = find_all(searchStr1, hStack)
    puts(1,"---------------------------------\r\n")
    if object(foundNeedles) and length(foundNeedles) > 0 then
        printf(1, "First needle found at index %d \r\n", foundNeedles[1])

        if length(foundNeedles) > 1 then
            printf(1, "Last needle found at index %d \r\n", foundNeedles[length(foundNeedles)] )

            for i = 1 to length(foundNeedles) do
                printf(1, "Needle #%d ", i)
                printf(1, "was at index %d .\r\n", foundNeedles[i])
            end for

            else
                puts(1, "There was only one needle found in this haystack. \r\n")
        end if

        else
            puts(1, "Simulated exception - No needles found in this haystack.\r\n")
    end if

end procedure

--runs the procedure on all haystacks
haystackSearch(haystack1)
haystackSearch(haystack2)
haystackSearch(haystack3)
haystackSearch(haystack4)
haystackSearch(haystack5)
haystackSearch(haystack6)
--wait for user to press a key to exit
any_key()

```


```txt

---------------------------------
First needle found at index 1
Last needle found at index 7
Needle #1 was at index 1 .
Needle #2 was at index 2 .
Needle #3 was at index 7 .
---------------------------------
First needle found at index 5
Last needle found at index 12
Needle #1 was at index 5 .
Needle #2 was at index 9 .
Needle #3 was at index 12 .
---------------------------------
First needle found at index 1
There was only one needle found in this haystack.
---------------------------------
Simulated exception - No needles found in this haystack.
---------------------------------
First needle found at index 2
Last needle found at index 4
Needle #1 was at index 2 .
Needle #2 was at index 4 .
---------------------------------
Simulated exception - No needles found in this haystack.
Press Any Key to continue...

```


=={{header|F_Sharp|F#}}==

```fsharp
List.findIndex (fun x -> x = "bar") ["foo"; "bar"; "baz"; "bar"]  // -> 1
                                      // A System.Collections.Generic.KeyNotFoundException
                                      // is raised, if the predicate does not evaluate to
                                      // true for any list element.
```



## Factor


```factor
: find-index ( seq elt -- i )
    '[ _ = ] find drop [ "Not found" throw ] unless* ; inline

: find-last-index ( seq elt -- i )
    '[ _ = ] find-last drop [ "Not found" throw ] unless* ; inline
```


 ( scratchpad ) { "a" "b" "c" "d" "c" } "c" find-index .
 2
 ( scratchpad ) { "a" "b" "c" "d" "c" } "c" find-last-index .
 4


## Forth

```forth
include lib/row.4th

create haystack
  ," Zig"  ," Zag" ," Wally" ," Ronald" ," Bush" ," Krusty" ," Charlie"
  ," Bush" ," Boz" ," Zag" NULL ,
does>
  dup >r 1 string-key row 2>r type 2r> ."  is "
  if r> - ." at " . else r> drop drop ." not found" then cr
;

s" Washington" haystack s" Bush" haystack
```



Works with any ANS Forth

Needs the FMS-SI (single inheritance) library code located here:
http://soton.mpeforth.com/flag/fms/index.html

```forth
include FMS-SI.f
include FMS-SILib.f

${ Dishonest Fake Left Karl Hillary Monica Bubba Hillary Multi-Millionaire } constant haystack

: needleIndex { addr len $list | cnt -- idx }
  0 to cnt  $list uneach:
  begin
    $list each:
  while
    @: addr len compare 0= if cnt exit then
    cnt 1+ to cnt
  repeat true abort" Not found" ;

: LastIndexOf { addr len $list | cnt last-found -- idx }
  0 to cnt 0 to last-found  $list uneach:
  begin
    $list each:
  while
    @: addr len compare 0= if cnt to last-found  then
    cnt 1+ to cnt
  repeat
  last-found if last-found
  else true abort" Not found"
  then ;

s" Hillary" haystack needleIndex . \ => 4
s" Hillary" haystack LastIndexOf . \ => 7
s" Washington" haystack needleIndex . \ => aborted: Not found

```



## Fortran



```fortran
program main

 implicit none

 character(len=7),dimension(10) :: haystack = [  &
  'Zig    ',&
  'Zag    ',&
  'Wally  ',&
  'Ronald ',&
  'Bush   ',&
  'Krusty ',&
  'Charlie',&
  'Bush   ',&
  'Boz    ',&
  'Zag    ']

 call find_needle('Charlie')
 call find_needle('Bush')

 contains

	subroutine find_needle(needle)
	implicit none
	character(len=*),intent(in) :: needle
	integer :: i
	do i=1,size(haystack)
		if (needle==haystack(i)) then
			write(*,'(A,I4)') trim(needle)//' found at index:',i
			return
		end if
	end do
	write(*,'(A)') 'Error: '//trim(needle)//' not found.'
	end subroutine find_needle

 end program main
```



## FreeBASIC

FreeBASIC doesn't have exceptions so we use a different approach to check if the needle is present or not in the haystack:

```freebasic
' FB 1.05.0 Win64
' Works FB 1.05.0 Linux Mint 64

Function tryFindString(s() As String, search As String, ByRef index As Integer) As Boolean
  Dim length As Integer = UBound(s) - LBound(s) + 1
  If length = 0 Then
    index = LBound(s) - 1  '' outside array
    Return False
  End If
  For i As Integer = LBound(s) To UBound(s)
    If s(i) = search Then
      index = i  '' first occurrence
      Return True
    End If
  Next
  index = LBound(s) - 1  '' outside array
  Return False
End Function

Function tryFindLastString(s() As String, search As String, ByRef index As Integer) As Boolean
  Dim length As Integer = UBound(s) - LBound(s) + 1
  If length = 0 Then
    index = LBound(s) - 1  '' outside array
    Return False
  End If
  Dim maxIndex As Integer = LBound(s) - 1  '' outside array
  For i As Integer = LBound(s) To UBound(s)
    If s(i) = search Then
      maxIndex = i
    End If
  Next
  If maxIndex > LBound(s) - 1 Then
    index = maxIndex  '' last occurrence
    Return True
  Else
    Return False
  End If
End Function

Dim haystack(1 To 9) As String = {"Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty", "Charlie", "Bush", "Bozo"}
Dim needle(1 To 4)   As String = {"Zag", "Krusty", "Washington", "Bush"}

Dim As Integer index
Dim As Boolean found
For i As Integer  = 1 To 4
  found = tryFindString(haystack(), needle(i), index)
  If found Then
    Print needle(i); " found first at index"; index
  Else
    Print needle(i); " is not present"
  End If
Next
found = tryFindLastString(haystack(), needle(4), index)
If found Then
  Print needle(4); " found last at index"; index
Else
  Print needle(4); " is not present"
End If
Print
Print "Press any key to quit"
Sleep
```


```txt

Zag found first at index 2
Krusty found first at index 6
Washington is not present
Bush found first at index 5
Bush found last at index 8

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=7729d65f8af3f128db6c6992c5f74e98 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sHaystack As String[] = ["Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty", "Charlie", "Bush", "Boz", "Zag"]
Dim sNeedle As String = "Charlie"
Dim sOutput As String = "No needle found!"
Dim siCount As Short

For siCount = 0 To sHaystack.Max
  If sNeedle = sHaystack[siCount] Then
    sOutPut = sNeedle & " found at index " & Str(siCount)
    Break
  End If
Next

Print sOutput

End
```

Output:

```txt

Charlie found at index 6

```



## GAP


```gap
# First position is built-in
haystack := Eratosthenes(10000);;
needle := 8999;;
Position(haystack, needle);
# 1117

LastPosition := function(L, x)
  local old, new;
  old := 0;
  new := 0;
  while new <> fail do
    new := Position(L, x, old);
    if new <> fail then
      old := new;
    fi;
  od;
  return old;
end;

a := Shuffle(List([1 .. 100], x -> x mod 10));
# [ 0, 2, 4, 5, 3, 1, 0, 4, 8, 8, 2, 7, 6, 3, 3, 6, 4, 4, 3, 0, 7, 1, 8, 7, 2, 4, 7, 9, 4, 9, 4, 5, 9, 9, 6, 7, 8, 2, 3,
#   5, 1, 5, 4, 2, 0, 9, 6, 1, 1, 2, 2, 0, 5, 7, 6, 8, 8, 3, 1, 9, 5, 1, 9, 6, 8, 9, 2, 0, 6, 2, 1, 6, 1, 1, 2, 5, 3, 3,
#   0, 3, 5, 7, 5, 4, 6, 8, 0, 9, 8, 3, 7, 8, 0, 4, 9, 7, 0, 6, 5, 7 ]
Position(a, 0);
# 1
LastPosition(a, 0);
# 97
```

See also [[Sieve of Eratosthenes#GAP|Eratosthenes]] and [[Knuth shuffle#GAP|Shuffle]] functions in RosettaCode.


## Go

Data used by both examples below.  (You can give multiple files to go run, like <tt>$ go run data.go example.go</tt>)

```go
package main

var haystack = []string{"Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty",
    "Charlie", "Bush", "Bozo", "Zag", "mouse", "hat", "cup", "deodorant",
    "television", "soap", "methamphetamine", "severed cat heads", "foo",
    "bar", "baz", "quux", "quuux", "quuuux", "bazola", "ztesch", "foo",
    "bar", "thud", "grunt", "foo", "bar", "bletch", "foo", "bar", "fum",
    "fred", "jim", "sheila", "barney", "flarp", "zxc", "spqr", ";wombat",
    "shme", "foo", "bar", "baz", "bongo", "spam", "eggs", "snork", "foo",
    "bar", "zot", "blarg", "wibble", "toto", "titi", "tata", "tutu", "pippo",
    "pluto", "paperino", "aap", "noot", "mies", "oogle", "foogle", "boogle",
    "zork", "gork", "bork", "sodium", "phosphorous", "californium",
    "copernicium", "gold", "thallium", "carbon", "silver", "gold", "copper",
    "helium", "sulfur"}
```


### Linear search


```go
package main

import "fmt"

func main() {
    // first task
    printSearchForward("soap")
    printSearchForward("gold")
    printSearchForward("fire")
    // extra task
    printSearchReverseMult("soap")
    printSearchReverseMult("gold")
    printSearchReverseMult("fire")
}

// First task solution uses panic as an exception-like mechanism, as requested
// by the task.  Note however, this is not idiomatic in Go and in fact
// is considered bad practice.
func printSearchForward(s string) {
    fmt.Printf("Forward search: %s: ", s)
    defer func() {
        if x := recover(); x != nil {
            if err, ok := x.(string); ok && err == "no match" {
                fmt.Println(err)
                return
            }
            panic(x)
        }
    }()
    fmt.Println("smallest index =", searchForwardPanic(s))
}

func searchForwardPanic(s string) int {
    for i, h := range haystack {
        if h == s {
            return i
        }
    }
    panic("no match")
    return -1
}

// Extra task, a quirky search for multiple occurrences.  This is written
// without panic, and shows more acceptable Go programming practice.
func printSearchReverseMult(s string) {
    fmt.Printf("Reverse search for multiples: %s: ", s)
    if i := searchReverseMult(s); i > -1 {
        fmt.Println("largest index =", i)
    } else {
        fmt.Println("no multiple occurrence")
    }
}

func searchReverseMult(s string) int {
    largest := -1
    for i := len(haystack) - 1; i >= 0; i-- {
        switch {
        case haystack[i] != s:
        case largest == -1:
            largest = i
        default:
            return largest
        }
    }
    return -1
}
```

```txt

Forward search: soap: smallest index = 15
Forward search: gold: smallest index = 77
Forward search: fire: no match
Reverse search for multiples: soap: no multiple occurrence
Reverse search for multiples: gold: largest index = 81
Reverse search for multiples: fire: no multiple occurrence

```


### Map lookup

More efficient, if you're doing lots of lookups, is to build a map.  This example doesn't completely conform to the task but gives the idea that you could store indexes as map values.

```go
package main

import "fmt"

func main() {
    m := map[string][]int{}
    for i, needle := range haystack {
        m[needle] = append(m[needle], i)
    }
    for _, n := range []string{"soap", "gold", "fire"} {
        fmt.Println(n, m[n])
    }
}
```

```txt

soap [15]
gold [77 81]
fire []

```



## Groovy


```groovy
def haystack = ["Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo"]
def needles = ["Washington","Bush","Wally"]
needles.each { needle ->
    def index = haystack.indexOf(needle)
    def lastindex = haystack.lastIndexOf(needle)
    if (index < 0) {
        assert lastindex < 0
        println needle + " is not in haystack"
    } else {
        println "First index: " + index + " " + needle
        println "Last index:  " + lastindex + " " + needle
    }
}
```


```txt

Washington is not in haystack
First index: 4 Bush
Last index:  7 Bush
First index: 2 Wally
Last index:  2 Wally

```



## Haskell

Libraries and data:

```haskell
import Data.List

haystack=["Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo"]
needles = ["Washington","Bush"]
```

I use 'lambda' notation for readability.
:Find 'just' an index:

```haskell
*Main> map (\x -> (x,elemIndex x haystack)) needles
[("Washington",Nothing),("Bush",Just 4)]
```

Want to know if there are there more Bushes hiding in the haystack?

```haskell
*Main> map (\x -> (x,elemIndices x haystack)) needles
[("Washington",[]),("Bush",[4,7])]
```

To be complete. Here is the 'point free' version of the task:

```haskell
*Main> ((,) <*> flip elemIndex haystack) <$> needles
[("Washington",Nothing),("Bush",Just 4)]
```



## HicEst


```HicEst
CHARACTER haystack='Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo.'
CHARACTER needle*10

DLG(TItle="Enter search string", Edit=needle)

n = EDIT(Text=haystack, Option=2, End, Count=needle) ! Option = word

IF( n == 0 ) THEN
  WRITE(Messagebox="!") needle, "not found"    ! bus not found
ELSE
  first = EDIT(Text=needle, LeXicon=haystack)
  WRITE(ClipBoard) "First ", needle, "found in position ", first
  ! First bush      found in position 5

  last = EDIT(Text=haystack, End, Left=needle, Count=" ") + 1
  WRITE(ClipBoard) "Last ", needle, "found in position ", last
  ! Last bush      found in position 8
ENDIF
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon

link lists

procedure main()
haystack := ["Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo"]  # the haystack
every needle := !["Bush","Washington"] do {                                         # the needles

   if i := lindex(haystack,needle) then {                                           # first occurrence
      write("needle=",needle, " is at position ",i," in haystack.")

      if i <:= last(lindex,[haystack,needle]) then                                  # last occurrence
         write("needle=",needle, " is at last position ",i," in haystack.")
      }
   else {
      write("needle=",needle, " is not in haystack.")
      runerr(500,needle)        # throw an error
      }
   }

end

procedure last(p,arglist)               #: return the last generation of p(arglist) or fail
local i
every i := p!arglist
return \i
end
```


Taken from the public domain Icon Programming Library's [http://www.cs.arizona.edu/icon/library/src/procs/lists.icn lindex in lists] which generates list indices for x of any type

```Icon
procedure lindex(lst, x)		#: generate indices for items matching x
   local i

   every i := 1 to *lst do
      if lst[i] === x then suspend i

end
```


```txt
needle=Bush is at position 5 in haystack.
needle=Bush is at last position 8 in haystack.
needle=Washington is not in haystack.

Run-time error 500
File haystack.icn; Line 7
program malfunction
offending value: "Washington"
Traceback:
   main(list_1 = [])
   runerr(500,"Washington") from line 7 in haystack.icn
```



## Io


List has a <code>indexOf</code> method which does not raise an exception on lookup failure but returns <code>nil</code> therefore I extend List with a <code>firstIndex</code> method that does raise an exception.  I also create a <code>lastIndex</code> extension that finds the last index of a matching object by iterating in reverse over the list.  Note that all these methods find any object not just strings.


```Io
NotFound := Exception clone
List firstIndex := method(obj,
    indexOf(obj) ifNil(NotFound raise)
)
List lastIndex := method(obj,
    reverseForeach(i,v,
        if(v == obj, return i)
    )
    NotFound raise
)

haystack := list("Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo")
list("Washington","Bush") foreach(needle,
    try(
        write("firstIndex(\"",needle,"\"): ")
        writeln(haystack firstIndex(needle))
    )catch(NotFound,
        writeln(needle," is not in haystack")
    )pass
    try(
        write("lastIndex(\"",needle,"\"): ")
        writeln(haystack lastIndex(needle))
    )catch(NotFound,
        writeln(needle," is not in haystack")
    )pass
)
```

```txt
firstIndex("Washington"): Washington is not in haystack
lastIndex("Washington"): Washington is not in haystack
firstIndex("Bush"): 4
lastIndex("Bush"): 7
```



## J


J has a general and optimized lookup function, <code>i.</code>

For example:


```j
   Haystack =: ;:'Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo'
   Needles  =: ;:'Washington Bush'

   Haystack i. Needles     NB. first positions
9 4
   Haystack i: Needles     NB. last positions
9 7
```


Note that the arguments to <code>i.</code> can be anything (ie either or both may be scalars, lists, multidimensional arrays, etc). Nonmatches get a result of 1+largest valid index.

Other search primitives include:

<code>e.</code> finds whether items are members of a set, returning a bitmask to select the members:


```j
   Needles e. Haystack
0 1
   1 2 3 4 5 6 7 8 9 e. 2 3 5 60
0 1 1 0 1 0 0 0 0
```


<code>I.</code> finds indices, but performs a binary search (which requires that the list being searched is sorted). This can be useful for finding non-exact matches (the index of the next value is returned for non-exact matches).


```j
   1 2 3 4 5 6 7 8 9 I. 2 3 5 60 6.66
1 2 4 9 6
   (;:'eight five four nine one seven six three two') I. ;:'two three five sixty'
8 7 1 7
```


To format output similar to the other examples, one might write:


```j
    Haystack ;:^:_1@(] ,. [ ((<'is not in haystack')"_)`(#@[ I.@:= ])`(8!:0@])} i.) Needles
Washington is not in haystack
Bush 4
```


Or broken up into components and defined as a verb/function for finding the last positions:

```j
   msg=: (<'is not in haystack')"_                  NB. not found message
   idxmissing=: #@[ I.@:= ]                         NB. indices of items not found
   fmtdata=: 8!:0@]                                 NB. format atoms as boxed strings
   findLastIndex=: ;:inv@(] ,. [ msg`idxmissing`fmtdata} i:)

   Haystack findLastIndex Needles                   NB. usage
Washington is not in haystack
Bush 7
```


To elaborate a bit:  Array-oriented languages (like J) consume the input and produce the output ''in toto''.

That is, all the results are produced simultaneously; consequently, throwing an exception for any part of the input would prohibit producing any output at all.

And while it is both possible and simple to treat the input item by item, this is significantly slower and loses the great advantage of array processing.

Therefore these languages generally produce a special, but conforming,
output for "bad" inputs (in this case, an index past the end of the list).
Then the functions which consume these outputs may be left untouched
(as the special outputs are already in their domain) or may be extended simply.

In this case, there is only one function which formats and prints the results, and its treatment of "good" and "bad" outputs is identical (it cannot distinguish the two).
It is simply that the outputs of previous functions have been arranged such that the results are conformable.


## Java

for Lists, they have an indexOf() method:

```java
import java.util.List;
import java.util.Arrays;

List<String> haystack = Arrays.asList("Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo");

for (String needle : new String[]{"Washington","Bush"}) {
    int index = haystack.indexOf(needle);
    if (index < 0)
        System.out.println(needle + " is not in haystack");
    else
        System.out.println(index + " " + needle);
}
```


for arrays, you have to do it manually:

```java
import java.util.Arrays;

String[] haystack = { "Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo"};

for (String needle : new String[]{"Washington","Bush"}) {
    int index = Arrays.binarySearch(haystack, needle);
    if (index < 0)
        System.out.println(needle + " is not in haystack");
    else
        System.out.println(index + " " + needle);
}
```


```txt

Washington is not in haystack
4 Bush

```



## JavaScript


```javascript
var haystack = ['Zig', 'Zag', 'Wally', 'Ronald', 'Bush', 'Krusty', 'Charlie', 'Bush', 'Bozo']
var needles = ['Bush', 'Washington']

for (var i in needles) {
    var found = false;
    for (var j in haystack) {
        if (haystack[j] == needles[i]) {
            found = true;
            break;
        }
    }
    if (found)
        print(needles[i] + " appears at index " + j + " in the haystack");
    else
        throw needles[i] + " does not appear in the haystack"
}
```


The following {{works with|JavaScript|1.6}}:

```javascript
for each (var needle in needles) {
    var idx = haystack.indexOf(needle);
    if (idx == -1)
        throw needle + " does not appear in the haystack"
    else
        print(needle + " appears at index " + idx + " in the haystack");
}

// extra credit

for each (var elem in haystack) {
    var first_idx = haystack.indexOf(elem);
    var last_idx  = haystack.lastIndexOf(elem);
    if (last_idx > first_idx) {
        print(elem + " last appears at index " + last_idx + " in the haystack");
        break
    }
}
```



Or, generalising enough (in ES5) to allow for varying definitions of the type of match we are looking for:


```JavaScript
(function () {

  function findIndex(fnPredicate, list) {
    for (var i = 0, lng = list.length; i < lng; i++) {
      if (fnPredicate(list[i])) {
        return i;
      }
    }
    return Error("not found");
  };

  // DEFINING A PARTICULAR TYPE OF SEARCH MATCH

  function matchCaseInsensitive(s, t) {
    return s.toLowerCase() === t.toLowerCase();
  }

  var lstHaystack = [
    'Zig', 'Zag', 'Wally', 'Ronald', 'Bush',
    'Krusty', 'Charlie', 'Bush', 'Bozo'
  ],
    lstReversed = lstHaystack.slice(0).reverse(),
    iLast = lstHaystack.length - 1,
    lstNeedles = ['bush', 'washington'];

  return {
    'first': lstNeedles.map(function (s) {
      return [s, findIndex(function (t) {
          return matchCaseInsensitive(s, t);
        },
        lstHaystack)];
    }),

    'last': lstNeedles.map(function (s) {
      var varIndex = findIndex(function (t) {
          return matchCaseInsensitive(s, t);
        },
        lstReversed);

      return [
        s,
        typeof varIndex === 'number' ?
          iLast - varIndex : varIndex
      ];
    })
  }
})();
```


Output:


```JavaScript
{
  "first": [
    [
      "bush",
      4
    ],
    [
      "washington",
      "Error: not found"
    ]
  ],
  "last": [
    [
      "bush",
      7
    ],
    [
      "washington",
      "Error: not found"
    ]
  ]
}
```



## jq

The jq index origin is 0.
The relevant methods for the tasks at hand are index/1 and rindex/1; indices/1 can also be used.

In the following, the output is shown after the "# =>":

```jq

["a","b","c"] | index("b")
# => 1

["a","b","c","b"] | index("b")
# => 1

["a","b","c","b"]
  | index("x") // error("element not found")
# => jq: error: element not found

# Extra task - the last element of an array can be retrieved
# using `rindex/` or by using -1 as an index into the array produced by `indices/1`:
["a","b","c","b","d"] | rindex("b")
# => 3

["a","b","c","b","d"] | indices("b")[-1]
# => 3
```



## Julia

```julia
@show findfirst(["no", "?", "yes", "maybe", "yes"], "yes")
@show indexin(["yes"], ["no", "?", "yes", "maybe", "yes"])
@show findin(["no", "?", "yes", "maybe", "yes"], ["yes"])
@show find(["no", "?", "yes", "maybe", "yes"] .== "yes")
```


```txt
findfirst(["no", "?", "yes", "maybe", "yes"], "yes") = 3
indexin(["yes"], ["no", "?", "yes", "maybe", "yes"]) = [5]
findin(["no", "?", "yes", "maybe", "yes"], ["yes"]) = [3, 5]
find(["no", "?", "yes", "maybe", "yes"] .== "yes") = [3, 5]
```



## K


```K
  Haystack:("Zig";"Zag";"Wally";"Ronald";"Bush";"Krusty";"Charlie";"Bush";"Bozo")
  Needles:("Washington";"Bush")
  {:[y _in x;(y;x _bin y);(y;"Not Found")]}[Haystack]'Needles
```


```K
(("Washington"
  "Not Found")
 ("Bush"
  4))
```


Additional:
If more than one occurrence ("Bush"), also show position of the last occurrence.
Here we use the dyadic verb _sm (string match) instead of _bin (binary search).


```K
  Haystack2: Haystack,,"Bush"
  Needles2:Needles,,"Zag"
  {+(x;{:[#&x;,/?(*&x;*|&x);"Not found"]}'+x _sm/:y)}[Needles2;Haystack2]
```


```K
(("Washington"
  "Not found")
 ("Bush"
  4 9)
 ("Zag"
  1))
```



## Kotlin


```scala
// version 1.0.6 (search_list.kt)

fun main(args: Array<String>) {
    val haystack = listOf("Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty", "Charlie", "Bush", "Boz", "Zag")
    println(haystack)
    var needle = "Zag"
    var index  = haystack.indexOf(needle)
    val index2 = haystack.lastIndexOf(needle)
    println("\n'$needle' first occurs at index $index of the list")
    println("'$needle' last  occurs at index $index2 of the list\n")
    needle = "Donald"
    index  = haystack.indexOf(needle)
    if (index == -1) throw Exception("$needle does not occur in the list")
}
```


```txt

[Zig, Zag, Wally, Ronald, Bush, Krusty, Charlie, Bush, Boz, Zag]

'Zag' first occurs at index 1 of the list
'Zag' last  occurs at index 9 of the list

Exception in thread "main" java.lang.Exception: Donald does not occur in the list
        at Search_listKt.main(search_list.kt:13)

```



## Lang5


```lang5
: haystack(*)  ['rosetta 'code 'search 'a 'list 'lang5 'code] find-index ;
: find-index
    2dup eq length iota swap select swap drop
    length if swap drop
    else drop " is not in haystack" 2 compress "" join
    then ;
: ==>search  apply ;

['hello 'code] 'haystack ==>search .
```

```txt
[ hello is not in haystack
  [    1     6  ]
]
```


## Lasso

Lasso arrays have a findindex method which returns all matching indexes. [http://lassoguide.com/operations/containers.html?#array]


```Lasso
local(haystack) = array('Zig', 'Zag', 'Wally', 'Ronald', 'Bush', 'Krusty', 'Charlie', 'Bush', 'Bozo')

#haystack->findindex('Bush')->first // 5
#haystack->findindex('Bush')->last // 8

protect => {^
    handle_error => {^ error_msg ^}
        fail_if(not #haystack->findindex('Washington')->first,'Washington is not in haystack.')
^}
```


```txt

5
8
Washington is not in haystack.
```



## Liberty BASIC


```lb
haystack$="apple orange pear cherry melon peach banana needle blueberry mango strawberry needle "
haystack$=haystack$+"pineapple grape kiwi blackberry plum raspberry needle cranberry apricot"

idx=1
do until word$(haystack$,idx)=""
idx=idx+1
loop
total=idx-1

needle$="needle"
'index of first occurrence
for i = 1 to total
    if word$(haystack$,i)=needle$ then exit for
next
print needle$;" first found at index ";i

'index of last occurrence
for j = total to 1
    if word$(haystack$,j)=needle$ then exit for
next
print needle$;" last found at index ";j
if i<>j then
    print "Multiple instances of ";needle$
    else
    print "Only one instance of ";needle$;" in list."
end if

'raise exception
needle$="cauliflower"
for k=1 to total
    if word$(haystack$,k)=needle$ then exit for
next
if k>total then
    print needle$;" not found in list."
else
    print needle$;" found at index ";k
end if
```



## Lingo


```lingo
haystack = ["apples", "oranges", "bananas", "oranges"]
needle = "oranges"

pos = haystack.getPos(needle)
if pos then
  put "needle found at index "&pos
else
  put "needle not found in haystack"
end if

-- "needle found at index 2"
```



## Lisaac


```Lisaac
+ haystack : ARRAY[STRING];
haystack := "Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo".split;
"Washington Bush".split.foreach { needle : STRING;
  haystack.has(needle).if {
    haystack.first_index_of(needle).print;
    ' '.print;
    needle.print;
    '\n'.print;
  } else {
    needle.print;
    " is not in haystack\n".print;
  };
};
```



## Logo


```logo
to indexof :item :list
  if empty? :list [(throw "NOTFOUND 0)]
  if equal? :item first :list [output 1]
  output 1 + indexof :item butfirst :list
end

to showindex :item :list
  make "i catch "NOTFOUND [indexof :item :list]
  ifelse :i = 0 [(print :item [ not found in ] :list)] [(print :item [ found at position ] :i [ in ] :list)]
end

showindex "dog [My dog has fleas]   ; dog found at position 2 in My dog has fleas
showindex "cat [My dog has fleas]   ; cat not found in My dog has fleas
```




## Lua


```lua
list = {"mouse", "hat", "cup", "deodorant", "television", "soap", "methamphetamine", "severed cat heads"} --contents of my desk

item = io.read()

for i,v in ipairs(list)
  if v == item then print(i) end
end
```



## M2000 Interpreter

Example based on BASIC's example, changed to find only first occurrence, and last if a second exist. We make one inventory queue which can take sane keys, and a second one with all keys in reverse order. Search works with hash table inside inventory.
Normally we use Exist(inventoryA, "key") and if it is true then we get the value as Eval(inventoryA) without using second search, by temporary use of an index. We can read that index by making a variable to bind a property of COM object (the object under the inventory).


```M2000 Interpreter

Module Checkit {
      Flush ' empty stack
      Inventory Queue Haystack=  "foo", "bar", "baz", "quux", "quuux", "quuuux", "bazola", "ztesch", "foo", "bar", "thud", "grunt"
      Append  Haystack, "foo", "bar", "bletch", "foo", "bar", "fum", "fred", "jim", "sheila", "barney", "flarp", "zxc"
      Append  Haystack,  "spqr", "wombat", "shme", "foo", "bar", "baz", "bongo", "spam", "eggs", "snork", "foo", "bar"
      Append  Haystack,  "zot", "blarg", "wibble", "toto", "titi", "tata", "tutu", "pippo", "pluto", "paperino", "aap"
      Append  Haystack,  "noot", "mies", "oogle", "foogle", "boogle", "zork", "gork", "bork"
      \\ Inventories are objects and we have access to properties using COM model
      With HayStack, "index" as index
      Inventory Queue HayStackRev
      N=Each(HayStack, -1, 1)
      While N {
            Append HayStackRev, Eval$(N, N^)
      }
      With HayStackRev, "index" as indexRev
      Print Len(HayStack)
      Print Len(HayStackRev)
      local needle$
      \\ Print all elements using columns
      Print haystack
      Repeat {
                Input "Word to search for? (Leave blank to exit) ", needle$
                If needle$ <> "" Then {
                          If Exist(haystackrev,lcase$(needle$) ) Then {
                              Print "Found "; CHR$(34); needle$; CHR$(34); " at index "; STR$(len(haystackrev)-indexrev,"")

                              If Exist(haystack,lcase$(needle$) ) Then  {
                                    if len(haystackrev)-1<>indexrev+index then {
                                                Print "Found "; CHR$(34); needle$; CHR$(34); " at index "; STR$(Len(haystack)-index,"")
                                    }
                              }
                        } Else  Print CHR$(34); needle$; CHR$(34); " not found"
            } Else Exit
      } Always
}
CheckIt

```




## Maple


```Maple
haystack := ["Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo"]:
occurences := ListTools:-SearchAll(needle,haystack):
try
	#first occurence
	printf("The first occurence is at index %d\n", occurences[1]);
	#last occurence, note that StringTools:-SearchAll()retuns a list of all occurences positions
	printf("The last occurence is at index %d\n", occurences[-1]);
catch :
	print("Erros: Needle not found in the haystack"):
end try:
```

needle := "Washington":

```txt
"Needle not found in the haystack"
```

needle := "Bush":

```txt
The first occurence is at index 5
The last occurence is at index 8
```



## Mathematica

This examples shows you the first appearance, the last appearance, and all appearances (as a list):

```Mathematica
haystack = {"Zig","Zag","Wally","Ronald","Bush","Zig","Zag","Krusty","Charlie","Bush","Bozo"};
needle = "Zag";
first = Position[haystack,needle,1][[1,1]]
last = Position[haystack,needle,1][[-1,1]]
all = Position[haystack,needle,1][[All,1]]
```

gives back:

```Mathematica
2
7
{2,7}
```



## MATLAB

Collections of strings are stored in cell arrays in MATLAB. The solution bellow will only work for a cell array of this construction:
```MATLAB
stringCollection = {'string1','string2',...,'stringN'}
```
 It will not work for any other construction, for example:
```MATLAB
stringCollection = {{'string1'},{'string2'},{...},{'stringN'}}
```


searchCollection.m:

```MATLAB
function index = searchCollection(list,searchItem,firstLast)

    %firstLast is a string containing either 'first' or 'last'. The 'first'
    %flag will cause searchCollection to return the index of the first
    %instance of the item being searched. 'last' will cause
    %searchCollection to return the index of the last instance of the item
    %being searched.

    indicies = cellfun(@(x)x==searchItem,list);
    index = find(indicies,1,firstLast);
    assert(~isempty(index),['The string ''' searchItem ''' does not exist in this collection of strings.']);

end
```


```MATLAB>>
 list = {'a','b','c','d','e','c','f','c'};
>> searchCollection(list,'c','first')

ans =

     3

>> searchCollection(list,'c','last')

ans =

     8

>> searchCollection(list,'g','last')
??? Error using ==> searchCollection at 11
The string 'g' does not exist in this collection of strings.
```



## MAXScript


```maxscript
haystack=#("Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo")

for needle in #("Washington","Bush") do
(
    index = findItem haystack needle

    if index == 0 then
    (
        format "% is not in haystack\n" needle
    )
    else
    (
        format "% %\n" index needle
    )
)
```


```maxscript
Washington is not in haystack
5 Bush
```



## Maxima


```maxima
haystack: ["Zig","Zag","Wally","Ronald","Bush","Zig","Zag","Krusty","Charlie","Bush","Bozo"];
needle:  "Zag";

findneedle(needle, haystack, [opt]):=block([idx],
  idx: sublist_indices(haystack, lambda([w], w=needle)),
  if emptyp(idx) then throw('notfound),
  if emptyp(opt) then return(idx),
  opt: first(opt),
  if opt='f then first(idx) else if opt='l then last(idx) else throw('unknownmode));
```


```txt
(%i32) catch(findneedle("Zag", haystack, 'f));
(%o32)                                 2
(%i33) catch(findneedle("Zag", haystack, 'l));
(%o33)                                 7
(%i34) catch(findneedle("Washington", haystack));
(%o34)                             notfound
(%i35) catch(findneedle("Bush", haystack, 'f));
(%o35)                                 5
(%i36) catch(findneedle("Zag", haystack));
(%o36)                              [2, 7]
(%i37) catch(findneedle("Zag", haystack, 'l));
(%o37)                                 7
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

driver(arg) -- call the test wrapper
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method searchListOfWords(haystack, needle, forwards = (1 == 1), respectCase = (1 == 1)) public static signals Exception

  if \respectCase then do
    needle   = needle.upper()
    haystack = haystack.upper()
    end
  if forwards then wp = haystack.wordpos(needle)
  else             wp = haystack.words() - haystack.reverse().wordpos(needle.reverse()) + 1
  if wp = 0 then signal Exception('*** Error! "'needle'" not found in list ***')

  return wp

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method searchIndexedList(haystack, needle, forwards = (1 == 1), respectCase = (1 == 1)) public static signals Exception
  if forwards then do
    strtIx = 1
    endIx  = haystack[0]
    incrIx = 1
    end
  else do
    strtIx = haystack[0]
    endIx  = 1
    incrIx = -1
    end

    wp = 0
    loop ix = strtIx to endIx by incrIx
      if respectCase then
        if needle == haystack[ix] then wp = ix
        else nop
      else
        if needle.upper() == haystack[ix].upper() then wp = ix
        else nop
      if wp > 0 then leave ix
      end ix
    if wp = 0 then signal Exception('*** Error! "'needle'" not found in indexed list ***')

  return wp

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Test wrapper
method driver(arg) public static
  -- some manifests
  TRUE_        = (1 == 1); FALSE_      = \TRUE_
  FORWARDS_    = TRUE_;    BACKWARDS_  = FALSE_
  CASERESPECT_ = TRUE_;    CASEIGNORE_ = \CASERESPECT_

  -- test data
  needles = ['barley', 'quinoa']

  -- a simple list of words.  Lists of words are indexable in NetRexx via the word(N) function
  hayrick = 'Barley maize barley sorghum millet wheat rice rye barley Barley oats flax'

  -- a Rexx indexed string made up from the words in hayrick
  cornstook = ''
  loop w_ = 1 to hayrick.words() -- populate the indexed string
    cornstook[0]  = w_
    cornstook[w_] = hayrick.word(w_)
    end w_

  loop needle over needles
    do -- process the list of words
      say 'Searching for "'needle'" in the list "'hayrick'"'
      idxF = searchListOfWords(hayrick, needle)
      idxL = searchListOfWords(hayrick, needle, BACKWARDS_)
      say '  The first occurence of "'needle'" is at index' idxF 'in the list'
      say '  The last occurence of "'needle'" is at index' idxL 'in the list'
      idxF = searchListOfWords(hayrick, needle, FORWARDS_, CASEIGNORE_)
      idxL = searchListOfWords(hayrick, needle, BACKWARDS_, CASEIGNORE_)
      say '  The first caseless occurence of "'needle'" is at index' idxF 'in the list'
      say '  The last caseless occurence of "'needle'" is at index' idxL 'in the list'
      say
    catch ex = Exception
      say '  'ex.getMessage()
      say
    end

    do -- process the indexed list
      corn = ''
      loop ci = 1 to cornstook[0]
        corn = corn cornstook[ci]
        end ci
      say 'Searching for "'needle'" in the indexed list "'corn.space()'"'
      idxF = searchIndexedList(cornstook, needle)
      idxL = searchIndexedList(cornstook, needle, BACKWARDS_)
      say '  The first occurence of "'needle'" is at index' idxF 'in the indexed list'
      say '  The last occurence of "'needle'" is at index' idxL 'in the indexed list'
      idxF = searchIndexedList(cornstook, needle, FORWARDS_, CASEIGNORE_)
      idxL = searchIndexedList(cornstook, needle, BACKWARDS_, CASEIGNORE_)
      say '  The first caseless occurence of "'needle'" is at index' idxF 'in the indexed list'
      say '  The last caseless occurence of "'needle'" is at index' idxL 'in the indexed list'
      say
    catch ex = Exception
      say '  'ex.getMessage()
      say
    end
    end needle

  return
```

```txt

Searching for "barley" in the list "Barley maize barley sorghum millet wheat rice rye barley Barley oats flax"
  The first occurence of "barley" is at index 3 in the list
  The last occurence of "barley" is at index 9 in the list
  The first caseless occurence of "barley" is at index 1 in the list
  The last caseless occurence of "barley" is at index 10 in the list

Searching for "barley" in the indexed list "Barley maize barley sorghum millet wheat rice rye barley Barley oats flax"
  The first occurence of "barley" is at index 3 in the indexed list
  The last occurence of "barley" is at index 9 in the indexed list
  The first caseless occurence of "barley" is at index 1 in the indexed list
  The last caseless occurence of "barley" is at index 10 in the indexed list

Searching for "quinoa" in the list "Barley maize barley sorghum millet wheat rice rye barley Barley oats flax"
  *** Error! "quinoa" not found in list ***

Searching for "quinoa" in the indexed list "Barley maize barley sorghum millet wheat rice rye barley Barley oats flax"
  *** Error! "quinoa" not found in indexed list ***

```



## Nim


```nim
let haystack = ["Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo"]

for needle in ["Bush", "Washington"]:
  let f = haystack.find(needle)
  if f >= 0:
    echo f, " ", needle
  else:
    raise newException(ValueError, needle & " not in haystack")
```


=={{header|Objective-C}}==
```objc
NSArray *haystack = @[@"Zig",@"Zag",@"Wally",@"Ronald",@"Bush",@"Krusty",@"Charlie",@"Bush",@"Bozo"];
for (id needle in @[@"Washington",@"Bush"]) {
    int index = [haystack indexOfObject:needle];
    if (index == NSNotFound)
        NSLog(@"%@ is not in haystack", needle);
    else
        NSLog(@"%i %@", index, needle);
}
```



## Objeck


```objeck
use Collection;

class Test {
  function : Main(args : String[]) ~ Nil {
    haystack := ["Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo"];
    values := CompareVector->New();
    each(i : haystack) {
      values->AddBack(haystack[i]->As(Compare));
    };

    needles := ["Washington", "Bush"];
    each(i : needles) {
      values->Has(needles[i]->As(Compare))->PrintLine();
    };
  }
}
```



## OCaml


```ocaml
# let find_index pred lst =
    let rec loop n = function
       []    -> raise Not_found
     | x::xs -> if pred x then n
                          else loop (n+1) xs
    in
    loop 0 lst;;
val find_index : ('a -> bool) -> 'a list -> int = <fun>

# let haystack =
    ["Zig";"Zag";"Wally";"Ronald";"Bush";"Krusty";"Charlie";"Bush";"Bozo"];;
val haystack : string list =
  ["Zig"; "Zag"; "Wally"; "Ronald"; "Bush"; "Krusty"; "Charlie"; "Bush";
   "Bozo"]
# List.iter (fun needle ->
               try
                 Printf.printf "%i %s\n" (find_index ((=) needle) haystack) needle
               with Not_found ->
                 Printf.printf "%s is not in haystack\n" needle)
            ["Washington"; "Bush"];;
Washington is not in haystack
4 Bush
- : unit = ()
```



## Oforth


indexOf returns null if an object is not into a collection, not an exception.


```Oforth
: needleIndex(needle, haystack)
   haystack indexOf(needle) dup ifNull: [ drop ExRuntime throw("Not found", needle) ] ;

[ "Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty", "Charlie", "Bush", "Boz" ] const: Haystack

needleIndex("Bush", Haystack) println
Haystack lastIndexOf("Bush") println
needleIndex("Washington", Haystack) println
```


```txt

5
8
[1:interpreter] ExRuntime : Not found <Washington>

```



## ooRexx

All ooRexx collections support an index method that will search for an item.
For ordered collections, this will always be the first item.
For unordered collections, the index returned is undetermined.

```ooRexx
-- ordered collections always return the first hit
a = .array~of(1,2,3,4,4,5)
say a~index(4)
a2 = .array~new(5,5)  -- multidimensional
a2[3,3] = 4
-- the returned index is an array of values
say a2~index(4)~makestring('line', ',')
-- Note, list indexes are assigned when an item is added and
-- are not tied to relative position
l = .list~of(1,2,3,4,4,5)
say l~index(4)
q = .queue~of(1,2,3,4,4,5)
say q~index(4)
-- directories are unordered, so it is
-- undertermined which one is returned
d = .directory~new
d["foo"] = 4
d["bar"] = 4
say d~index(4)
```



## Oz

No such function exists for the built-in list type
(the operation is quite inefficient, after all).
A possible implementation:

```oz
declare
  %% Lazy list of indices of Y in Xs.
  fun {Indices Y Xs}
     for
        X in Xs
        I in 1;I+1
        yield:Yield
     do
        if Y == X then {Yield I} end
     end
  end

  fun {Index Y Xs}
     case {Indices Y Xs} of X|_ then X
     else raise index(elementNotFound Y) end
     end
  end

  Haystack = ["Zig" "Zag" "Wally" "Ronald" "Bush" "Krusty" "Charlie" "Bush" "Bozo"]
in
  {Show {Index "Bush" Haystack}}
  {Show {List.last {Indices "Bush" Haystack}}}

  {Show {Index "Washington" Haystack}} %% throws
```



## PARI/GP

```parigp
find(v,n)={
  my(i=setsearch(v,n));
  if(i,
    while(i>1, if(v[i-1]==n,i--))
  ,
    error("Could not find")
  );
  i
};
```



## Pascal

See [[Search_a_list#Delphi | Delphi]]


## Perl


```perl
use List::Util qw(first);

my @haystack = qw(Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo);

foreach my $needle (qw(Washington Bush)) {
  my $index = first { $haystack[$_] eq $needle } (0 .. $#haystack); # note that "eq" was used because we are comparing strings
                                                                    # you would use "==" for numbers
  if (defined $index) {
    print "$index $needle\n";
  } else {
    print "$needle is not in haystack\n";
  }
}
```

```txt

Washington is not in haystack
4 Bush

```


You could install a non-standard module List::MoreUtils:

```perl
use List::MoreUtils qw(first_index);

my @haystack = qw(Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo);

foreach my $needle (qw(Washington Bush)) {
  my $index = first_index { $_ eq $needle } @haystack; # note that "eq" was used because we are comparing strings
                                                       # you would use "==" for numbers
  if (defined $index) {
    print "$index $needle\n";
  } else {
    print "$needle is not in haystack\n";
  }
}
```


Alternatively, if you need to do this a lot, you could create a hash table mapping values to indices in the haystack:

```perl
my @haystack = qw(Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo);

my %haystack_indices;
@haystack_indices{ @haystack } = (0 .. $#haystack); # Caution: this finds the largest index, not the smallest

foreach my $needle (qw(Washington Bush)) {
  my $index = $haystack_indices{$needle};
  if (defined $index) {
    print "$index $needle\n";
  } else {
    print "$needle is not in haystack\n";
  }
}
```

```txt

Washington is not in haystack
7 Bush

```



## Perl 6


```perl6>my @haystack = <Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo
;

for <Washington Bush> -> $needle {
    say "$needle -- { @haystack.first($needle, :k) // 'not in haystack' }";
}
```


```txt

Washington -- not in haystack
Bush -- 4

```




Or, including the "extra credit" task:
```perl6>my Str @haystack = <Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo
;

for <Washingston Bush> -> $needle {
    my $first = @haystack.first($needle, :k);

    if defined $first {
        my $last = @haystack.first($needle, :k, :end);
        say "$needle -- first at $first, last at $last";
    }
    else {
        say "$needle -- not in haystack";
    }
}
```


```txt

Washingston -- not in haystack
Bush -- first at 4, last at 7

```


The built-in method <code>.first</code> takes a [https://docs.perl6.org/language/operators#infix_~~ smart-matcher], and returns the first matching list element.

The <code>:k</code> adverb tells it to return the key (a.k.a. list index) instead of the value of the matching element.

The <code>:end</code> adverb tells it to start searching from the end of the list.




If you plan to do many searches on the same large list, you might want to build a search hash first for efficient look-up:

```perl6>my @haystack = <Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo
;

my %index;
%index{.value} //= .key for @haystack.pairs;

for <Washington Bush> -> $needle {
    say "$needle -- { %index{$needle} // 'not in haystack' }";
}
```



## Phix


```Phix
constant s = {"Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty", "Charlie", "Bush", "Boz", "Zag"}

integer r = find("Zag",s)       ?r  -- 2    (first)
r = find("Zag",s,r+1)           ?r  -- 10   (next)
r = find("Zag",s,r+1)           ?r  -- 0    (no more)
r = rfind("Zag",s)              ?r  -- 10   (last)
r = find("Zog",s)               ?r  -- 0    (none)
```



## PHP


```php
$haystack = array("Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo");

foreach (array("Washington","Bush") as $needle) {
  $i = array_search($needle, $haystack);
  if ($i === FALSE) // note: 0 is also considered false in PHP, so you need to specifically check for FALSE
    echo "$needle is not in haystack\n";
  else
    echo "$i $needle\n";
}
```

```txt

Washington is not in haystack
4 Bush

```



## PicoLisp

Note that in PicoLisp all indexes are one-based
(the first element has the position '1')

```PicoLisp
(de lastIndex (Item Lst)
   (- (length Lst) (index Item (reverse Lst)) -1) )

(de findNeedle (Fun Sym Lst)
   (prinl Sym " " (or (Fun Sym Lst) "not found")) )

(let Lst '(Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo)
   (findNeedle index 'Washington Lst)
   (findNeedle index 'Bush Lst)
   (findNeedle lastIndex 'Bush Lst) )
```

```txt
Washington not found
Bush 5
Bush 8
```



## PL/I


```pli
search: procedure () returns (fixed binary);
   declare haystack (0:9) character (200) varying static initial
      ('apple', 'banana', 'celery', 'dumpling', 'egg', 'flour',
       'grape', 'pomegranate', 'raisin', 'sugar' );
   declare needle character (200) varying;
   declare i fixed binary;
   declare missing_needle condition;

   on condition(missing_needle) begin;
      put skip list ('your string ''' || needle ||
         ''' does not exist in the haystack.');
   end;

   put ('Please type a string');
   get edit (needle) (L);
   do i = lbound(haystack,1) to hbound(haystack,1);
      if needle = haystack(i) then return (i);
   end;
   signal condition(missing_needle);
   return (lbound(haystack,1)-1);
end search;
```



## PowerBASIC

```powerbasic
FUNCTION PBMAIN () AS LONG
    DIM haystack(54) AS STRING
    ARRAY ASSIGN haystack() = "foo", "bar", "baz", "quux", "quuux", "quuuux", _
                 "bazola", "ztesch", "foo", "bar", "thud", "grunt", "foo", _
                 "bar", "bletch", "foo", "bar", "fum", "fred", "jim", _
                 "sheila", "barney", "flarp", "zxc", "spqr", ";wombat", "shme", _
                 "foo", "bar", "baz", "bongo", "spam", "eggs", "snork", "foo", _
                 "bar", "zot", "blarg", "wibble", "toto", "titi", "tata", _
                 "tutu", "pippo", "pluto", "paperino", "aap", "noot", "mies", _
                 "oogle", "foogle", "boogle", "zork", "gork", "bork"
    DIM needle AS STRING, found AS LONG, lastFound AS LONG
    DO
        needle = INPUTBOX$("Word to search for? (Leave blank to exit)")
        IF needle <> "" THEN
            ' collate ucase -> case insensitive
            ARRAY SCAN haystack(), COLLATE UCASE, = needle, TO found
            IF found > 0 THEN
                lastFound = found
                MSGBOX "Found """ & needle & """ at index " & TRIM$(STR$(found - 1))
                IF found < UBOUND(haystack) THEN
                    DO
                        ARRAY SCAN haystack(lastFound), COLLATE UCASE, = needle, TO found
                        IF found > 0 THEN
                            MSGBOX "Another occurence of """ & needle & """ at index " & _
                                   TRIM$(STR$(found + lastFound - 1))
                            lastFound = found + lastFound
                        ELSE
                            MSGBOX "No more occurences of """ & needle & """ found"
                            EXIT DO 'will exit inner DO, not outer
                        END IF
                    LOOP
                END IF
            ELSE
                MSGBOX "No occurences of """ & needle & """ found"
            END IF
        ELSE
            EXIT DO
        END IF
    LOOP
END FUNCTION
```



## PowerShell

```PowerShell

function index($haystack,$needle) {
    $index = $haystack.IndexOf($needle)
    if($index -eq -1) {
        Write-Warning "$needle is absent"
    } else {
        $index
    }

}
$haystack = @("word", "phrase", "preface", "title", "house", "line", "chapter", "page", "book", "house")
index $haystack "house"
index $haystack "paragraph"

```

<b>Output:</b>

```txt

4
WARNING: paragraph is absent

```



### PowerShell Extra credit

The -Verbose switch is available to any advanced function.

```PowerShell

function Find-Needle
{
    [CmdletBinding()]
    [OutputType([int])]
    Param
    (
        [Parameter(Mandatory=$true, Position=0)]
        [string]
        $Needle,

        [Parameter(Mandatory=$true, Position=1)]
        [string[]]
        $Haystack,

        [switch]
        $LastIndex
    )

    if ($LastIndex)
    {
        $index = [Array]::LastIndexOf($Haystack,$Needle)

        if ($index -eq -1)
        {
            Write-Verbose "Needle not found in Haystack"
            return $index
        }

        if ((($Haystack | Group-Object | Where-Object Count -GT 1).Group).IndexOf($Needle) -ne -1)
        {
            Write-Verbose "Last needle found in Haystack at index $index"
        }
        else
        {
            Write-Verbose "Needle found in Haystack at index $index  (No duplicates were found)"
        }

        return $index
    }
    else
    {
        $index = [Array]::IndexOf($Haystack,$Needle)

        if ($index -eq -1)
        {
            Write-Verbose "Needle not found in Haystack"
        }
        else
        {
            Write-Verbose "Needle found in Haystack at index $index"
        }

        return $index
    }
}

$haystack = @("word", "phrase", "preface", "title", "house", "line", "chapter", "page", "book", "house")

```


```PowerShell

Find-Needle "house" $haystack

```

```txt

4

```


```PowerShell

Find-Needle "house" $haystack -Verbose

```

```txt

VERBOSE: Needle found in Haystack at index 4
4

```


```PowerShell

Find-Needle "house" $haystack -LastIndex -Verbose

```

```txt

VERBOSE: Last needle found in Haystack at index 9
9

```


```PowerShell

Find-Needle "title" $haystack -LastIndex -Verbose

```

```txt

VERBOSE: Needle found in Haystack at index 3  (No duplicates were found)
3

```


```PowerShell

Find-Needle "something" $haystack -Verbose

```

```txt

VERBOSE: Needle not found in Haystack
-1

```



## Prolog

Works with SWI-Prolog

```Prolog
search_a_list(N1, N2) :-
	L = ["Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty", "Charlie", "Bush", "Boz", "Zag"],

	write('List is :'), maplist(my_write, L), nl, nl,

	(   nth1(Ind1, L, N1) ->
	    format('~s is in position ~w~n', [N1, Ind1])
	;   format('~s is not present~n', [N1])),
	(   nth1(Ind2, L, N2) ->
	    format('~s is in position ~w~n', [N2, Ind2])
	;   format('~s is not present~n', [N2])),
	(   reverse_nth1(Ind3, L, N1) ->
	    format('~s last position is ~w~n', [N1, Ind3])
	;   format('~s is not present~n', [N1])).

reverse_nth1(Ind, L, N) :-
	reverse(L, RL),
	length(L, Len),
	nth1(Ind1, RL, N),
	Ind is Len - Ind1 + 1.

my_write(Name) :-
	writef(' %s', [Name]).

```


```txt
 ?- search_a_list("Zag", "Simpson").
List is : Zig Zag Wally Ronald Bush Krusty Charlie Bush Boz Zag

Zag is in position 2
Simpson is not present
Zag last position is 10
true.

```



## PureBasic


```PureBasic
If OpenConsole()  ; Open a simple console to interact with user
  NewList Straws.s()
  Define Straw$, target$="TBA"
  Define found

  Restore haystack ; Read in all the straws of the haystack.
  Repeat
    Read.s Straw$
    If Straw$<>""
      AddElement(Straws())
      Straws()=UCase(Straw$)
      Continue
    Else
      Break
    EndIf
  ForEver

  While target$<>""
    Print(#CRLF$+"Enter word to search for (leave blank to quit) :"): target$=Input()
    ResetList(Straws()): found=#False
    While NextElement(Straws())
      If UCase(target$)=Straws()
        found=#True
        PrintN(target$+" found as index #"+Str(ListIndex(Straws())))
      EndIf
    Wend
    If Not found
      PrintN("Not found.")
    EndIf
  Wend
EndIf

DataSection
  haystack:
  Data.s "Zig","Zag","Zig","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo",""
EndDataSection
```



## Python


```python
haystack=["Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo"]

for needle in ("Washington","Bush"):
  try:
    print haystack.index(needle), needle
  except ValueError, value_error:
    print needle,"is not in haystack"
```

```txt

Washington is not in haystack
4 Bush

```


Note that in Python, the index method of a list already raises an exception.
The following shows the default information given
when the exception is not captured in the program:

```python>>>
 haystack=["Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo"]
>>> haystack.index('Bush')
4
>>> haystack.index('Washington')
Traceback (most recent call last):
  File "<pyshell#95>", line 1, in <module>
    haystack.index('Washington')
ValueError: list.index(x): x not in list
>>>
```


There is no built-in method for returning the highest index of a repeated string in a Python list, tuple or array, (although strings have [http://docs.python.org/library/stdtypes.html?highlight=rindex#str.rindex rindex]).
Instead we need to look for the index in the reversed list and adjust the result.

```python>>>
 def hi_index(needle, haystack):
	return len(haystack)-1 - haystack[::-1].index(needle)

>>> # Lets do some checks
>>> for n in haystack:
	hi = hi_index(n, haystack)
	assert haystack[hi] == n, "Hi index is of needle"
	assert n not in haystack[hi+1:], "No higher index exists"
	if haystack.count(n) == 1:
		assert hi == haystack.index(n), "index == hi_index if needle occurs only once"

>>>
```



## R


```R
find.needle <- function(haystack, needle="needle", return.last.index.too=FALSE)
{
   indices <- which(haystack %in% needle)
   if(length(indices)==0) stop("no needles in the haystack")
   if(return.last.index.too) range(indices) else min(indices)
}
```

Example usage:

```R
haystack1 <- c("where", "is", "the", "needle", "I", "wonder")
haystack2 <- c("no", "sewing", "equipment", "in", "here")
haystack3 <- c("oodles", "of", "needles", "needles", "needles", "in", "here")

find.needle(haystack1)                              # 4
find.needle(haystack2)                              # error
find.needle(haystack3)                              # 3
find.needle(haystack3, needle="needles", ret=TRUE)  # 3 5
```




## Racket

The function index returns the index of the the element x in the sequence xs.
If the element is not found, then #f is returned.

```racket
(define (index xs y)
  (for/first ([(x i) (in-indexed xs)]
              #:when (equal? x y))
    i))
```


If the last index of an element is needed, for/last is used:

```racket
(define (index-last xs y)
  (for/last ([(x i) (in-indexed xs)]
             #:when (equal? x y))
    i))
```


Both index and index-last can handle any sequence such as lists, vectors, sets etc.
Let us test with a linked list:

```racket
(define haystack '("Zig" "Zag" "Wally" "Ronald" "Bush" "Krusty" "Charlie" "Bush" "Bozo"))

(for/list ([needle '("Bender" "Bush")])
    (index haystack needle))

(for/list ([needle '("Bender" "Bush")])
    (index-last haystack needle))
```

```txt

'(#f 4)
'(#f 7)

```



## REBOL


```REBOL
REBOL [
	Title: "List Indexing"
	URL: http://rosettacode.org/wiki/Index_in_a_list
]

locate: func [
	"Find the index of a string (needle) in string collection (haystack)."
	haystack [series!] "List of values to search."
	needle [string!] "String to find in value list."
	/largest "Return the largest index if more than one needle."
	/local i
][
	i: either largest [
		find/reverse tail haystack needle][find haystack needle]
	either i [return index? i][
		throw reform [needle "is not in haystack."]
	]
]

; Note that REBOL uses 1-base lists instead of 0-based like most
; computer languages. Therefore, the index provided will be one
; higher than other results on this page.

haystack: parse "Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo" none

print "Search for first occurance:"
foreach needle ["Washington" "Bush"] [
	print catch [
		reform [needle "=>" locate haystack needle]
	]
]

print [crlf "Search for last occurance:"]
foreach needle ["Washington" "Bush"] [
	print catch [
		reform [needle "=>" locate/largest haystack needle]
	]
]
```


```txt
Search for first occurance:
Washington is not in haystack.
Bush => 5

Search for last occurance:
Washington is not in haystack.
Bush => 8
```



## REXX


### version 1

This REXX program searches a collection of string (haystack)
that are stored in a sequential REXX array.


No counter is kept of the number of items,
but they should be numbered consecutively and can't have any gaps.


The haystack items may have any character, including blanks.

A ''null'' value isn't allowed in this method of representing values.

```rexx
/*REXX program searches a collection of strings   (an array of periodic table elements).*/
hay.=                                            /*initialize the haystack collection.  */
hay.1  = 'sodium'
hay.2  = 'phosphorous'
hay.3  = 'californium'
hay.4  = 'copernicium'
hay.5  = 'gold'
hay.6  = 'thallium'
hay.7  = 'carbon'
hay.8  = 'silver'
hay.9  = 'curium'
hay.10 = 'copper'
hay.11 = 'helium'
hay.12 = 'sulfur'

needle = 'gold'                                  /*we'll be looking for the gold.       */
upper needle                                     /*in case some people capitalize stuff.*/
found=0                                          /*assume the needle isn't found yet.   */

          do j=1  while hay.j\==''               /*keep looking in the haystack.        */
          _=hay.j;     upper _                   /*make it uppercase to be safe.        */
          if _=needle  then do;  found=1         /*we've found the needle in haystack.  */
                                 leave           /*   ··· and stop looking, of course.  */
                            end
          end   /*j*/

if found  then return j                          /*return the haystack  index  number.  */
          else say  needle  "wasn't found in the haystack!"
return 0                                         /*indicates the needle  wasn't  found. */
```



### version 2

This REXX program searches a collection of string (haystack)
that are stored in a REXX array (which may have gaps).


A safe counter is kept of the maximum (highest) index in the array,
this counter may be any sufficiently high number.


The array may be out of order (but not recommended!).

```rexx
/*REXX program searches a collection of strings   (an array of periodic table elements).*/
hay.0   =      1000                              /*safely indicate highest item number. */
hay.200 = 'Binilnilium'
hay.98  = 'californium'
hay.6   = 'carbon'
hay.112 = 'copernicium'
hay.29  = 'copper'
hay.114 = 'flerovium'
hay.79  = 'gold'
hay.2   = 'helium'
hay.1   = 'hydrogen'
hay.82  = 'lead'
hay.116 = 'livermorium'
hay.15  = 'phosphorous'
hay.47  = 'silver'
hay.11  = 'sodium'
hay.16  = 'sulfur'
hay.81  = 'thallium'
hay.92  = 'uranium'
                                                 /* [↑]  sorted by the element name.    */
needle  = 'gold'                                 /*we'll be looking for the gold.       */
upper needle                                     /*in case some people capitalize.      */
found=0                                          /*assume the needle isn't found  (yet).*/

          do j=1  for hay.0                      /*start looking in haystack,  item 1.  */
          _=hay.j;     upper _                   /*make it uppercase just to be safe.   */
          if _=needle  then do;  found=1         /*we've found the needle in haystack.  */
                                 leave           /*  ··· and stop looking, of course.   */
                            end
          end   /*j*/

if found  then return j                          /*return the haystack  index  number.  */
          else say  needle  "wasn't found in the haystack!"
return 0                                         /*indicates the needle  wasn't  found. */
```



### version 3

This REXX program searches a collection of string (haystack)
that are stored in a REXX array.


This form uses a type of array called a '''sparse array'''   (with non-numeric indexes).


One drawback of this approach is that the items can't have leading/trailing/imbedded blanks,

nor can they have special characters.


Only letters, numerals, and a few special characters are allowed:   '''!''',   '''@''',   '''#''',   '''$''',   '''?''',   and   '''_'''.


This method (finding a needle in a haystack) is extremely fast as there isn't any

table look-up, the "finding" is done by REXX's own internal method of variable lookup,

and, for the most part, it based on a table hashing algorithm.


This method pre-prends an underscore (underbar) to avoid collision with any REXX

variable names.  Therefore, there shouldn't be any REXX variable names (in this

program) that have a leading underscore   ('''_''').

```rexx
/*REXX program searches a collection of strings   (an array of periodic table elements).*/
hay.=0                                           /*initialize the haystack collection.  */
hay._sodium       = 1
hay._phosphorous  = 1
hay._californium  = 1
hay._copernicium  = 1
hay._gold         = 1
hay._thallium     = 1
hay._carbon       = 1
hay._silver       = 1
hay._copper       = 1
hay._helium       = 1
hay._sulfur       = 1
                                                 /*underscores (_) are used to NOT ...  */
                                                 /*   ... conflict with variable names. */

needle  = 'gold'                                 /*we'll be looking for the gold.       */

Xneedle = '_'needle                              /*prefix an underscore (_)  character. */
upper Xneedle                                    /*uppercase:  how REXX stores them.    */

                                                 /*alternative version of above:        */
                                                 /*       Xneedle=translate('_'needle)  */

found=hay.Xneedle                                /*this is it, it's found (or maybe not)*/

if found  then return j                          /*return the haystack  index  number.  */
          else say  needle  "wasn't found in the haystack!"
return 0                                         /*indicates the needle  wasn't  found. */
```



### version 4

This method uses a simple string (so haystack items can't have embedded blanks or tabs in them).

Code was added to uppercase both the   '''haystack'''   and the   '''needle'''   to make the search   ''case insensitive''.

```rexx
/*REXX program searches a collection of strings   (an array of periodic table elements).*/
    /*───────────────names of the first 200 elements of the periodic table.─────────────*/
_=  'hydrogen helium lithium beryllium boron carbon nitrogen oxygen fluorine neon sodium'
_=_ 'magnesium aluminum silicon phosphorous sulfur chlorine argon potassium calcium'
_=_ 'scandium titanium vanadium chromium manganese iron cobalt nickel copper zinc'
_=_ 'gallium germanium arsenic selenium bromine krypton rubidium strontium yttrium'
_=_ 'zirconium niobium molybdenum technetium ruthenium rhodium palladium silver cadmium'
_=_ 'indium tin antimony tellurium iodine xenon cesium barium lanthanum cerium'
_=_ 'praseodymium neodymium promethium samarium europium gadolinium terbium dysprosium'
_=_ 'holmium erbium thulium ytterbium lutetium hafnium tantalum tungsten rhenium osmium'
_=_ 'iridium platinum gold mercury thallium lead bismuth polonium astatine radon'
_=_ 'francium radium actinium thorium protactinium uranium neptunium plutonium americium'
_=_ 'curium berkelium californium einsteinium fermium mendelevium nobelium lawrencium'
_=_ 'rutherfordium dubnium seaborgium bohrium hassium meitnerium darmstadtium'
_=_ 'roentgenium copernicium nihonium flerovium moscovium livermorium tennessine'
_=_ 'oganesson ununennium unbinilium unbiunium unbibium unbitrium unbiquadium'
_=_ 'unbipentium unbihexium unbiseptium unbioctium unbiennium untrinilium untriunium'
_=_ 'untribium untritrium untriquadium untripentium untrihexium untriseptium untrioctium'
_=_ 'untriennium unquadnilium unquadunium unquadbium unquadtrium unquadquadium'
_=_ 'unquadpentium unquadhexium unquadseptium unquadoctium unquadennium unpentnilium'
_=_ 'unpentunium unpentbium unpenttrium unpentquadium unpentpentium unpenthexium'
_=_ 'unpentseptium unpentoctium unpentennium unhexnilium unhexunium unhexbium unhextrium'
_=_ 'unhexquadium unhexpentium unhexhexium unhexseptium unhexoctium unhexennium'
_=_ 'unseptnilium unseptunium unseptbium unsepttrium unseptquadium unseptpentium'
_=_ 'unsepthexium unseptseptium unseptoctium unseptennium unoctnilium unoctunium'
_=_ 'unoctbium unocttrium unoctquadium unoctpentium unocthexium unoctseptium unoctoctium'
_=_ 'unoctennium unennilium unennunium unennbium unenntrium unennquadium unennpentium'
_=_ 'unennhexium unennseptium unennoctium unennennium binilnilium'

haystack= _                                      /*assign the elements ───►  haystack.  */
needle  = 'gold'                                 /*we'll be looking for the gold.       */
upper needle haystack                            /*in case some people capitalize stuff.*/
idx= wordpos(needle, haystack)                   /*use REXX's BIF:  WORDPOS             */
if idx\==0  then return idx                      /*return the haystack  index  number.  */
            else say  needle  "wasn't found in the haystack!"
return 0                                         /*indicates the needle  wasn't  found. */
                                                 /*stick a fork in it,  we're all done. */
```



## Ring


```ring

haystack = ["alpha","bravo","charlie","delta","echo","foxtrot","golf",
"hotel","india","juliet","kilo","lima","mike","needle",
"november","oscar","papa","quebec","romeo","sierra","tango",
"needle","uniform","victor","whisky","x-ray","yankee","zulu"]

needle = "needle"
maxindex = len(haystack)

for index = 1 to maxindex
    if needle = haystack[index] exit ok
next
if index <= maxindex
   see "first found at index " + index + nl ok
for last = maxindex to 0 step -1
    if needle = haystack[last] exit ok
next
if !=index see " last found at index " + last + nl
else see "not found" + nl ok

```

Output:

```txt

first found at index : 14
 last found at index : 22

```



## Ruby


```ruby
haystack = %w(Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo)

%w(Bush Washington).each do |needle|
  if (i = haystack.index(needle))
    puts "#{i} #{needle}"
  else
    raise "#{needle} is not in haystack\n"
  end
end
```

```txt

4 Bush
search_a_list.rb:8:in `block in <main>': Washington is not in haystack (RuntimeError)
	from search_a_list.rb:4:in `each'
	from search_a_list.rb:4:in `<main>'

```


'''Extra credit'''

```ruby
haystack.each do |item|
  last = haystack.rindex(item)
  if last > haystack.index(item)
    puts "#{item} last appears at index #{last}"
    break
  end
end
#=> Bush last appears at index 7
```

or

```ruby
multi_item = haystack.each_index.group_by{|idx| haystack[idx]}.select{|key, val| val.length > 1}
# multi_item is => {"Bush"=>[4, 7]}
multi_item.each do |key, val|
  puts "#{key} appears at index #{val}"
end
#=> Bush appears at index [4, 7]
```



## Run BASIC


```runbasic
haystack$ = ("Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo Bush ")
needle$   = "Zag Wally Bush Chicken"

while word$(needle$,i+1," ") <> ""
  i  = i + 1
  thisNeedle$ = word$(needle$,i," ") + " "
  j  = instr(haystack$,thisNeedle$)
  k1 = 0
  k  = instr(haystack$,thisNeedle$,j+1)
  while k <> 0
    k1 = k
    k  = instr(haystack$,thisNeedle$,k+1)
  wend
  if j <> 0 then
    print thisNeedle$;" located at:";j;
    if k1 <> 0 then print " Last position located at:";k1;
    print
   else
    print thisNeedle$;" is not in the list"
  end if
wend
```

```txt
Zag  located at:5
Wally  located at:9
Bush  located at:22 Last position located at:52
Chicken  is not in the list
```



## Rust


Rust encourages to encode possible errors in function's return type. For example, <code>position</code> returns <code>Option<usize></code>, which can be <code>None</code> or <code>Some(x)</code>.


```rust
fn main() {
    let haystack=vec!["Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty", "Charlie",
                        "Bush", "Boz", "Zag"];

    println!("First occurence of 'Bush' at {:?}",haystack.iter().position(|s| *s=="Bush"));
    println!("Last occurence of 'Bush' at {:?}",haystack.iter().rposition(|s| *s=="Bush"));
    println!("First occurence of 'Rob' at {:?}",haystack.iter().position(|s| *s=="Rob"));
}

```


```txt
First occurence of 'Bush' at Some(4)
Last occurence of 'Bush' at Some(7)
First occurence of 'Rob' at None

```



### Version that panics



```rust
fn main() {
    let haystack=vec!["Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty", "Charlie",
                        "Bush", "Boz", "Zag"];

    println!("First occurence of 'Bush' at {:?}",haystack.iter().position(|s| *s=="Bush").unwrap());
    println!("Last occurence of 'Bush' at {:?}",haystack.iter().rposition(|s| *s=="Bush").unwrap());
    println!("First occurence of 'Rob' at {:?}",haystack.iter().position(|s| *s=="Rob").unwrap());
}

```


```txt
First occurence of 'Bush' at 4
Last occurence of 'Bush' at 7
thread '<main>' panicked at 'called `Option::unwrap()` on a `None` value', /home/rustbuild/src/rust-buildbot/slave/stable-dist-rustc-linux/build/src/libcore/option.rs:362
playpen: application terminated with error code 101

```


=={{header|S-lang}}==
<lang S-lang>variable haystack = ["Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo","Ronald"];

define find(needle)
{
    variable i = where(haystack == needle);
    if (length(i)) {
       % print(sprintf("%s: first=%d, last=%d", needle, i[0], i[-1]));
       return(i[0], i[-1]);
    }
    else
       throw ApplicationError, "an exception";
}

($1, $2) = find("Ronald");     % returns 3, 9
($1, $2) = find("McDonald");   % throws ApplicationError, labelled "an exception"

```



## Sather

```sather
class MAIN is
   main is
      haystack :ARRAY{STR} := |"Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty", "Charlie", "Bush", "Bozo"|;
      needles :ARRAY{STR} := | "Washington", "Bush" |;
      loop needle ::= needles.elt!;
	 index ::= haystack.index_of(needle);
	 if index < 0 then
	    #OUT + needle + " is not in the haystack\n";
	 else
	    #OUT + index + " " + needle + "\n";
	 end;
      end;
   end;
end;
```



## Scala

The method indexOf, defined for all classes inheriting from,
or having an implicit conversion to,
Seq returns the index of the first element, or -1 if none exists.
The method lastIndexOf does the same for the last element.
Neither throws an exception, but that's easily done afterwards.

However, a simple implementation,
not using those or similar methods might be written like this:


```scala
def findNeedles(needle: String, haystack: Seq[String]) = haystack.zipWithIndex.filter(_._1 == needle).map(_._2)
def firstNeedle(needle: String, haystack: Seq[String]) = findNeedles(needle, haystack).head
def lastNeedle(needle: String, haystack: Seq[String]) = findNeedles(needle, haystack).last
```


It does raise an exception if there's no needle.


## Scheme


```scheme
(define haystack
  '("Zig" "Zag" "Wally" "Ronald" "Bush" "Krusty" "Charlie" "Bush" "Bozo"))

(define index-of
  (lambda (needle hackstack)
    (let ((tail (member needle haystack)))
      (if tail
          (- (length haystack) (length tail))
          (throw 'needle-missing)))))

(define last-index-of
  (lambda (needle hackstack)
    (let ((tail (member needle (reverse haystack))))
      (if tail
          (- (length tail) 1)
          (throw 'needle-missing)))))
```

```txt

(index-of "Bush" haystack)
4
(last-index-of "Bush" haystack)
7

```



## Sidef


```ruby
var haystack = %w(Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo);

%w(Bush Washington).each { |needle|
    var i = haystack.first_index{|item| item == needle};
    if (i >= 0) {
        say "#{i} #{needle}";
    } else {
        die "#{needle} is not in haystack";
    }
}
```

```txt

4 Bush
Washington is not in haystack at find.sf line 9.

```

Extra credit:

```ruby
var haystack = %w(Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo);
say haystack.last_index{|item| item == "Bush"};
```

```txt

7

```



## Slate


```slate
define: #haystack -> ('Zig,Zag,Wally,Ronald,Bush,Krusty,Charlie,Bush,Bozo' splitWith: $,).
{'Washington'. 'Bush'} do: [| :needle |
  (haystack indexOf: needle)
    ifNil: [inform: word ; ' is not in the haystack']
    ifNotNilDo: [| :firstIndex lastIndex |
      inform: word ; ' is in the haystack at index ' ; firstIndex printString.
      lastIndex: (haystack lastIndexOf: word).
      lastIndex isNotNil /\ (lastIndex > firstIndex) ifTrue:
        [inform: 'last occurrence of ' ; word ; ' is at index ' ; lastIndex]]].
```



## Smalltalk

Smalltalk indexes start at 1.

```smalltalk
| haystack |
haystack :=
   'Zig,Zag,Wally,Ronald,Bush,Krusty,Charlie,Bush,Bozo' subStrings: $,.
{ 'Washington' . 'Bush'  } do: [:i|
  |t l|
  t := (haystack indexOf: i).
  (t = 0) ifTrue: [ ('%1 is not in the haystack' % { i }) displayNl ]
          ifFalse: [ ('%1 is at index %2' % { i . t }) displayNl.
                     l := ( (haystack size) - (haystack reverse indexOf: i) + 1 ).
                     ( t = l ) ifFalse: [
                       ('last occurence of %1 is at index %2' %
                             { i . l }) displayNl ]
                   ]
].
```



## Standard ML



```sml
fun find_index (pred, lst) = let
  fun loop (n, [])    = NONE
    | loop (n, x::xs) = if pred x then SOME n
                                  else loop (n+1, xs)
in
  loop (0, lst)
end;

val haystack = ["Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo"];

app (fn needle =>
       case find_index (fn x => x = needle, haystack) of
            SOME i => print (Int.toString i ^ " " ^ needle ^ "\n")
          | NONE   => print (needle ^ " is not in haystack\n"))
    ["Washington", "Bush"];
```



## Swift

```swift
let haystack = ["Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo"]
for needle in ["Washington","Bush"] {
  if let index = haystack.indexOf(needle) {
    print("\(index) \(needle)")
  } else {
    print("\(needle) is not in haystack")
  }
}
```

```swift
let haystack = ["Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo"]
for needle in ["Washington","Bush"] {
    if let index = find(haystack, needle) {
        println("\(index) \(needle)")
    } else {
        println("\(needle) is not in haystack")
    }
}
```


The second task:
```swift

// the second part can be done several ways, but extending any Array of Comparable objects is the most generic approach
extension Array where Element : Comparable {
    func lastIndexMatching(needle:Element) -> Int? {

        for i in stride(from: count-1, through: 0, by: -1) {
            if self[i] == needle {
                return i
            }
        }
        return nil
    }
}

for needle in ["Washington","Bush"] {
    if let index = haystack.lastIndexMatching(needle) {
        print("\(index) \(needle)")
    } else {
        print("\(needle) is not in haystack")
    }
}



```



## Tcl


```tcl
set haystack {Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo}
foreach needle {Bush Washington} {
    if {[set idx [lsearch -exact $haystack $needle]] == -1} {
        error "$needle does not appear in the haystack"
    } else {
        puts "$needle appears at index $idx in the haystack"
    }
}
```

'''Extra credit:'''

```tcl
set haystack {Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo}
foreach needle {Bush Washington} {
    set indices [lsearch -all -exact $haystack $needle]
    if {[llength $indices] == 0} {
        error "$needle does not appear in the haystack"
    } else {
        puts "$needle appears first at index [lindex $indices 0] and last at [lindex $indices end]"
    }
}
```



## TorqueScript


--[[User:Elm|Elm]] 03:38, 18 June 2012 (UTC)

Find multiple needles in a haystack:


```TorqueScript
function findIn(%haystack,%needles)
{
	%hc = getWordCount(%haystack);
	%nc = getWordCount(%needles);

	for(%i=0;%i<%nc;%i++)
	{
		%nword = getWord(%needles,%i);
		%index[%nword] = -1;
	}

	for(%i=0;%i<%hc;%i++)
	{
		%hword = getWord(%haystack,%i);

		for(%j=0;%j<%nc;%j++)
		{
			%nword = getWord(%needles,%j);

			if(%hword $= %nword)
			{
				%index[%nword] = %i;
			}
		}
	}

	for(%i=0;%i<%nc;%i++)
	{
		%nword = getWord(%needles,%i);
		%string = %string SPC %nword@"_"@%index[%nword];
		%string = trim(%string);
	}

	return %string;
}
```


How to use it:


```TorqueScript
echo(findIn("Hello world, you are quite sunny today.","quite hello somethingelse"));
```


returns:


```TorqueScript>=
 "quite_4 hello_0 somethingelse_-1"
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
SET haystack="Zig'Zag'Wally'Ronald'Bush'Krusty'Charlie'Bush'Bozo"
PRINT "haystack=",haystack
LOOP needle="Washington'Bush'Wally"
SET table  =QUOTES (needle)
BUILD S_TABLE needle = table
 IF (haystack.ct.needle) THEN
  BUILD R_TABLE needle = table
  SET position=FILTER_INDEX(haystack,needle,-)
  RELEASE R_TABLE needle
  PRINT "haystack contains ", needle, " on position(s): ",position
 ELSE
  PRINT "haystack not contains ",needle
 ENDIF
RELEASE S_TABLE needle
ENDLOOP
```


```txt

haystack=Zig'Zag'Wally'Ronald'Bush'Krusty'Charlie'Bush'Bozo
haystack not contains Washington
haystack contains Bush on position(s): 5'8
haystack contains Wally on position(s): 3

```



## UNIX Shell

```sh
if [ $1 ];then
haystack="Zip Zag Wally Ronald Bush Krusty Charlie Bush Bozo"

index=$(echo $haystack|tr " " "\n"|grep -in "^$1$")
if [ $? = 0 ];then
quantity_of_hits=$(echo $index|tr " " "\n"|wc -l|tr -d " ")
first_index=$(echo $index|cut -f 1 -d ":")
if [ $quantity_of_hits = 1 ];then
echo The sole index for $1 is: $first_index
else
echo The smallest index for $1 is: $first_index
greatest_index=$(echo $index|tr " " "\n"|tail -1|cut -f 1 -d ":")
echo "The greatest index for $1 is: $greatest_index";fi
else echo $1 is absent from haystatck.;fi
else echo Must provide string to find in haystack.;fi
```


```txt

./needle_haystack clay     ---> clay is absent from haystatck.
./needle_haystack charlie  ---> The sole index for charlie is: 7
./needle_haystack bush     ---> The smallest index for bush is: 5
                                The greatest index for bush is: 8
```



## Ursala

The <code>indices</code> function takes a pair <math>(needle,haystack)</math> of any type, treats haystack as a list, and returns the pair of indices giving the first and last positions of needle in it, which are numbered from zero and may be equal.
If it's not present, an exception is thrown with a diagnostic message of 'missing'.
The search is expressed by <code>~|</code>, the built-in distributing filter operator.

```Ursala
#import std

indices = ||<'missing'>!% ~&nSihzXB+ ~&lrmPE~|^|/~& num
```

The explanation is somewhat longer than the program.
* The <code>^|</code> operator takes a right operand consisting of a pair of functions <math>(f,g)</math>, and returns a function that takes a pair <math>(x,y)</math> to the result <math>(f(x),g(y))</math>.
* An expression of the form <code>h/f g</code> where <code>h</code> is a function taking a pair, is equivalent to <code>h(f,g)</code>.
* The <code>~&</code> operator represents the identity function.
* The expression <code>^|/~& num</code> applied to an argument <math>(needle,haystack)</math> therefore evaluates to <math>(needle,</math><code>num </code><math>haystack)</math>
* The <code>num</code> function takes any list <math>\langle x_0,x_1\dots x_n\rangle</math> and transforms it to a list of pairs <math>\langle (0,x_0),(1,x_1)\dots(n,x_n)\rangle</math>.
* The left operand to the <code>^|</code> operator, if any, is composed with the function constructed from the right. In this case, the left operand is <code>~&lrmPE~|</code>
* The <code>~|</code> operator takes a predicate as its left operand and returns a function that operates on a pair <math>(a,b)</math>, where <math>b</math> is expected to be a list. The resulting function is evaluated by pairing <math>a</math> with each item of <math>b</math>, applying the predicate to each pair, and making a list of the items of <math>b</math> for which the predicate holds on the pair.
* The predicate in this case is <code>~&lrmPE</code>, which will be passed an input of the form <math>(needle,(i,x_i))</math> for the <math>i</math>-th item in terms of the notation above.
* The expression <code>~&lrmPE</code> has a root operator <code>E</code>, which tests for equality, a left operand <code>l</code>, which extracts the left side of its argument, and a right operand of <code>rmP</code>, which is the reverse composition (<code>P</code>) of the right side extraction (<code>r</code>) operator, followed by a further right side extraction expressed more idiomatically as <code>m</code> when the argument in question represents some type of key-value pair.
* The predicate therefore compares the left side of <math>(needle,(i,x_i))</math>, which is <math>needle</math>, to the right of the right, which is <math>x_i</math>
* The result from <code>~&lrmPE~|</code> will be a list of pairs of the form <math>(i,needle)</math>, for indices <math>i</math> at which <math>needle</math> appears in the list.
* This result is passed to the function <code>~&nSihzXB</code>, which consists of subexpressions <code>nS</code> and <code>ihzXB</code> that operate sequentially.
* The <code>nS</code> subexpression makes a list of the left sides of all items of a list of key-value pairs, in this case constructing a list of indices <math>i</math> from the input, and passing it to the subexpression <code>ihzXB</code>.
* The subexpression <code>ihzXB</code> has a left subexpression <code>i</code>, a right subexpression <code>hzX</code> and a root <code>B</code>.
* The <code>B</code> (mnemonic for "both") operator causes the left subexpression to be applied to the argument as a test, and if the result is non-empty, returns the result of applying the right.
* The left subexpression <code>i</code> represents the identity function, and tests whether the argument list is non-empty.
* If the list is non-empty, the expression <code>hzX</code> constructs the pair (<code>X</code>) of the head (<code>h</code>) and the last item (<code>z</code>) of the list given in the argument.
* The disjunction operator <code>||</code> used in an expression of the form <code>||u v</code> with functions <code>u</code> and <code>v</code> constructs a function that applies <code>v</code> to the argument, returns that result if non-empty, but otherwise returns the the result of applying <code>v</code> to the argument.
* The expression <code><'missing'></code> is a list of strings representing the diagnostic message to be returned in the event of an empty list (corresponding to the <math>needle</math> not being present).
* The constant operator (<code>!</code>) is used because the message is not data-dependent.
* The exception throwing operator (<code>%</code>) compels the result of its operand to be returned in a way that bypasses the usual flow of control.

Test program:

```Ursala
#cast %nW

test = indices/'bar' <'foo','bar','baz','bar'>
```

```txt
(1,3)
```



## VBA


```vb
Function IsInArray(stringToBeFound As Variant, arr As Variant, _
    Optional start As Integer = 1, Optional reverse As Boolean = False) As Long
'Adapted from https://stackoverflow.com/questions/12414168/use-of-custom-data-types-in-vba
    Dim i As Long, lo As Long, hi As Long, stp As Long
    ' default return value if value not found in array
    IsInArray = -1
    If reverse Then
        lo = UBound(arr): hi = start: stp = -1
    Else
        lo = start: hi = UBound(arr): stp = 1
    End If
    For i = lo To hi Step stp 'start in stead of LBound(arr)
        If StrComp(stringToBeFound, arr(i), vbTextCompare) = 0 Then
            IsInArray = i
            Exit For
        End If
    Next i
End Function
Public Sub search_a_list()
    Dim haystack() As Variant, needles() As Variant
    haystack = [{"Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo"}]
    needles = [{"Washington","Bush"}]
    For i = 1 To 2
        If IsInArray(needles(i), haystack) = -1 Then
            Debug.Print needles(i); " not found in haystack."
        Else
            Debug.Print needles(i); " is at position "; CStr(IsInArray(needles(i), haystack)); ".";
            Debug.Print " And last position is ";
            Debug.Print CStr(IsInArray(needles(i), haystack, 1, True)); "."
        End If
    Next i
End Sub
```
```txt
Washington not found in haystack.
Bush is at position 5. And last position is 8.
```


## VBScript

Shamelessly derived from the BASIC version.

```vb

data = "foo,bar,baz,quux,quuux,quuuux,bazola,ztesch,foo,bar,thud,grunt," &_
		"foo,bar,bletch,foo,bar,fum,fred,jim,sheila,barney,flarp,zxc," &_
		"spqr,wombat,shme,foo,bar,baz,bongo,spam,eggs,snork,foo,bar," &_
		"zot,blarg,wibble,toto,titi,tata,tutu,pippo,pluto,paperino,aap," &_
		"noot,mies,oogle,foogle,boogle,zork,gork,bork"

haystack = Split(data,",")

Do
	WScript.StdOut.Write "Word to search for? (Leave blank to exit) "
	needle = WScript.StdIn.ReadLine
	If needle <> "" Then
		found = 0
		For i = 0 To UBound(haystack)
			If UCase(haystack(i)) = UCase(needle) Then
				found = 1
				WScript.StdOut.Write "Found " & Chr(34) & needle & Chr(34) & " at index " & i
				WScript.StdOut.WriteLine
			End If
		Next
		If found < 1 Then
			WScript.StdOut.Write Chr(34) & needle & Chr(34) & " not found."
			WScript.StdOut.WriteLine
		End If
	Else
		Exit do
	End If
Loop

```

```txt
F:\VBScript>cscript /nologo search_a_list.vbs
Word to search for? (Leave blank to exit) foo
Found "foo" at index 0
Found "foo" at index 8
Found "foo" at index 12
Found "foo" at index 15
Found "foo" at index 27
Found "foo" at index 34
Word to search for? (Leave blank to exit) bar
Found "bar" at index 1
Found "bar" at index 9
Found "bar" at index 13
Found "bar" at index 16
Found "bar" at index 28
Found "bar" at index 35
Word to search for? (Leave blank to exit) fff
"fff" not found.
Word to search for? (Leave blank to exit)

F:\VBScript>
```



## Wart

Wart uses the function <code>pos</code> to search a list for an element.
Here's how it's implemented:

```python
def (pos x (seq | (head ... tail)) n)
  default n :to 0
  if seq
    if (head = x)
      n
      (pos x tail n+1)
```


```txt
pos 3 '(1 2 3 4 5)
=> 2
pos 24 '(1 2 3 4 5)
=> nil
```



## XPL0


```XPL0
\Based on C example:
include c:\cxpl\stdlib;     \provides StrCmp routine, etc.
int Haystack;               \('int' is used instead of 'char' for 2D array)

func Search(Str, First);    \Return first (or last) index for string in haystack
char Str; int First;
int I, SI;
[I:= 0;  SI:= 0;
repeat  if StrCmp(Str, Haystack(I)) = 0 then
                [if First then return I;
                SI:= I;     \save index
                ];
        I:= I+1;
until   Haystack(I) = 0;
return SI;
];

[Haystack:= ["Zig", "Zag", "Wally", "Ronald", "Bush",
             "Krusty", "Charlie", "Bush", "Boz", "Zag", 0];
Text(0, "Bush is at ");  IntOut(0, Search("Bush", true));  CrLf(0);
if Search("Washington", true) = 0 then
        Text(0, "Washington is not in the haystack^M^J");
Text(0, "First index for Zag: ");  IntOut(0, Search("Zag", true));  CrLf(0);
Text(0,  "Last index for Zag: ");  IntOut(0, Search("Zag", false)); CrLf(0);
]
```


```txt

Bush is at 4
Washington is not in the haystack
First index for Zag: 1
Last index for Zag: 9

```



## Yabasic


```Yabasic
list$ = "mouse,hat,cup,deodorant,television,soap,methamphetamine,severed cat heads,cup"

dim item$(1)

n = token(list$, item$(), ",")

line input "Enter string to search: " line$
for i = 1 to n
    if line$ = item$(i) then
        if not t print "First index for ", line$, ": ", i
        t = i
        j = j + 1
    end if
next

if t = 0 then
    print "String not found in list"
else
    if j > 1 print "Last index for ", line$, ": ", t
end if
```



## Yorick


```yorick
haystack = ["Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty", "Charlie", "Bush", "Bozo"];
needles = ["Bush", "Washington"];
for(i = 1; i <= numberof(needles); i++) {
    w = where(haystack == needles(i));
    if(!numberof(w))
        error, "Needle "+needles(i)+" not found";
    write, format="Needle %s appears first at index %d\n", needles(i), w(1);
    if(numberof(w) > 1)
        write, format="Needle %s appears last at index %d\n", needles(i), w(0);
}
```



## zkl


```zkl
L("Krusty","Charlie","Bozo","Bozo").index("Charlie") //--> 1
L("Krusty","Charlie","Bozo","Bozo").index("fred") //--> throws index error
```

Find last needle:

```zkl
haystack:=L("Krusty","Charlie","Bozo","Bozo");
haystack.filterNs('==("Bozo"))[-1];  // -->3, indexError if not found
haystack.len() - 1 - haystack.reverse().index("Bozo");  // or this
```

Use a bit bucket of ASCIIZ strings

```zkl
haystack:=Data(0,String,"Krusty","Charlie","Bozo","Bozo");
if((n:=haystack.findString("Charlie")) != Void) n else throw(Exception.IndexError);
//-->7
```

