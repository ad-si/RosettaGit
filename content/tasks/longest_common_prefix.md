+++
title = "Longest common prefix"
description = ""
date = 2019-10-12T09:45:59Z
aliases = []
[extra]
id = 18892
[taxonomies]
categories = ["task"]
tags = []
+++

It is often useful to find the common prefix of a set of strings, that is, the longest initial portion of all strings that are identical.

Given a set of strings, ''R'', for a prefix ''S'', it should hold that:
:<math>\forall x\ \in\ R:\ S \le</math><sub>pref</sub> <math>\ x</math> ~ "for all members ''x'' of set ''R'', it holds true that string ''S'' is a prefix of ''x''"
:(help here: ''does not express that ''S'' is the <u>longest</u> common prefix of ''x)
An example use case for this: given a set of phone numbers, identify a common dialing code.  This can be accomplished by first determining the common prefix (if any), and then matching it against know dialing codes (iteratively dropping characters from rhs until a match is found, as the ''lcp'' function may match more than the dialing code).


;Test cases
For a function, ''lcp'', accepting a list of strings, the following should hold true
(the empty string, <math>\varepsilon</math>, is considered a prefix of all strings):
 lcp("interspecies","interstellar","interstate") = "inters"
 lcp("throne","throne") = "throne"
 lcp("throne","dungeon") = <math>\varepsilon</math>
 lcp("throne",<math>\varepsilon</math>,"throne") = <math>\varepsilon</math>
 lcp("cheese") = "cheese"
 lcp(<math>\varepsilon</math>) = <math>\varepsilon</math>
 lcp(<math>\emptyset</math>) = <math>\varepsilon</math>
 lcp("prefix","suffix") = <math>\varepsilon</math>
 lcp("foo","foobar") = "foo"

''Task inspired by this stackoverflow question'': [http://stackoverflow.com/questions/1916218/find-the-longest-common-starting-substring-in-a-set-of-strings Find the longest common starting substring in a set of strings]


## Related tasks

:*   [[Abbreviations, simple]]
:*   [[Abbreviations, easy]]
:*   [[Longest common prefix]]
:*   [[Longest increasing subsequence]]
:*   [[Suffixation of decimal numbers]]
:*   [[Find common directory path]]

## Aime


```aime
lcp(...)
{
    integer n;
    record r;
    text l;

    ucall(r_fix, 1, r, 0);
    n = 0;
    if (~r) {
        l = r.low;
        n = prefix(r.high, l);
    }

    l.cut(0, n);
}

main(void)
{
    o_("\"", lcp("interspecies", "interstellar", "interstate"), "\"\n");
    o_("\"", lcp("throne", "throne"), "\"\n");
    o_("\"", lcp("throne", "dungeon"), "\"\n");
    o_("\"", lcp("throne", "", "throne"), "\"\n");
    o_("\"", lcp("cheese"), "\"\n");
    o_("\"", lcp(""), "\"\n");
    o_("\"", lcp(), "\"\n");
    o_("\"", lcp("prefix", "suffix"), "\"\n");
    o_("\"", lcp("foo", "foobar"), "\"\n");

    0;
}
```

```txt
"inters"
"throne"
""
""
"cheese"
""
""
""
"foo"
```



## ALGOL 68

```algol68
# find the longest common prefix of two strings #
PRIO COMMONPREFIX = 1;
OP   COMMONPREFIX = ( STRING a, b )STRING:
    BEGIN
        INT a pos := LWB a; INT a max = UPB a;
        INT b pos := LWB b; INT b max = UPB b;
        WHILE
            IF a pos > a max OR b pos > b max THEN FALSE
            ELSE a[ a pos ] = b[ b pos ]
            FI
        DO
            a pos +:= 1; b pos +:= 1
        OD;
        a[ LWB a : a pos - 1 ]
    END # COMMONPREFIX # ;

# get the length of a string #
OP  LEN = ( STRING a )INT: ( UPB a + 1 ) - LWB a;

# find the longest common prefix of an array of STRINGs #
OP  LONGESTPREFIX = ( []STRING list )STRING:
    IF  UPB list < LWB list
    THEN
        # no elements #
        ""
    ELIF UPB list = LWB list
    THEN
        # only one element #
        list[ LWB list ]
    ELSE
        # more than one element #
        STRING prefix := list[ LWB list ] COMMONPREFIX list[ 1 + LWB list ];
        FOR pos FROM 2 + LWB list TO UPB list DO
            STRING next prefix := list[ pos ] COMMONPREFIX prefix;
            IF LEN next prefix < LEN prefix
            THEN
                # this element has a smaller common prefix #
                prefix := next prefix
            FI
        OD;
        prefix
    FI ;


# test the LONGESTPREFIX operator #

PROC test prefix = ( []STRING list, STRING expected result )VOID:
    BEGIN
        STRING prefix = LONGESTPREFIX list;
        print( ( "longest common prefix of (" ) );
        FOR pos FROM LWB list TO UPB list DO print( ( " """, list[ pos ], """" ) ) OD;
        print( ( " ) is: """, prefix, """ "
               , IF prefix = expected result THEN "as expected" ELSE "NOT AS EXPECTED" FI
               , newline
               )
             )
    END # test prefix # ;

[ 1 : 0 ]STRING empty list; # for recent versions of Algol 68G, can't just put "()" for an empty list #

BEGIN
    test prefix( ( "interspecies", "interstellar", "interstate" ), "inters" );
    test prefix( ( "throne", "throne" ), "throne" );
    test prefix( ( "throne", "dungeon" ), "" );
    test prefix( ( "throne", "", "throne" ), "" );
    test prefix( ( "cheese" ), "cheese" );
    test prefix( ( "" ), "" );
    test prefix( empty list, "" );
    test prefix( ( "prefix", "suffix" ), "" );
    test prefix( ( "foo", "foobar" ), "foo" )
END
```

```txt

longest common prefix of ( "interspecies" "interstellar" "interstate" ) is: "inters" as expected
longest common prefix of ( "throne" "throne" ) is: "throne" as expected
longest common prefix of ( "throne" "dungeon" ) is: "" as expected
longest common prefix of ( "throne" "" "throne" ) is: "" as expected
longest common prefix of ( "cheese" ) is: "cheese" as expected
longest common prefix of ( "" ) is: "" as expected
longest common prefix of ( ) is: "" as expected
longest common prefix of ( "prefix" "suffix" ) is: "" as expected
longest common prefix of ( "foo" "foobar" ) is: "foo" as expected

```



## Arturo


```arturo
lcp [list]{
    ret ""
    idx 0

    loop true {
        thisLetter ""
        loop list [word]{
        	if idx=$(size word) { return ret }
        	if thisLetter="" { thisLetter $(get $(characters word) idx) }
        	if thisLetter!=$(get $(characters word) idx) { return ret }

        }
        ret ret+thisLetter
        idx idx+1
    }
}

print $(lcp #("interspecies" "interstellar" "interstate"))
print $(lcp #("throne" "throne"))
print $(lcp #("throne" "dungeon"))
print $(lcp #("throne" "" "throne"))
print $(lcp #("cheese"))
print $(lcp #(""))
print $(lcp #("prefix" "suffix"))
print $(lcp #("foo" "foobar"))
```


```txt
inters
throne


cheese


foo
```



## AutoHotkey


```AutoHotkey
lcp(str*){
	for k, v in str
		w := v, list .= (list ? "`n" : "") v
	return RegExReplace(list, "^(.*)\K(\V*\R\1\V*)+$")
}
```

Examples:
```AutoHotkey
MsgBox % lcp("interspecies","interstellar","interstate")
```

Outputs:
```txt
inters
```


## AWK


```AWK

# syntax: GAWK -f LONGEST_COMMON_PREFIX.AWK
BEGIN {
    words_arr[++n] = "interspecies,interstellar,interstate"
    words_arr[++n] = "throne,throne"
    words_arr[++n] = "throne,dungeon"
    words_arr[++n] = "throne,,throne"
    words_arr[++n] = "cheese"
    words_arr[++n] = ""
    words_arr[++n] = "prefix,suffix"
    words_arr[++n] = "foo,foobar"
    for (i=1; i<=n; i++) {
      str = words_arr[i]
      printf("'%s' = '%s'\n",str,lcp(str))
    }
    exit(0)
}
function lcp(str,  arr,hits,i,j,lcp_leng,n,sw_leng) {
    n = split(str,arr,",")
    if (n == 0) { # null string
      return("")
    }
    if (n == 1) { # only 1 word, then it's the longest
      return(str)
    }
    sw_leng = length(arr[1])
    for (i=2; i<=n; i++) { # find shortest word length
      if (length(arr[i]) < sw_leng) {
        sw_leng = length(arr[i])
      }
    }
    for (i=1; i<=sw_leng; i++) { # find longest common prefix
      hits = 0
      for (j=1; j<n; j++) {
        if (substr(arr[j],i,1) == substr(arr[j+1],i,1)) {
          hits++
        }
      }
      if (hits == 0) {
        break
      }
      if (hits + 1 == n) {
        lcp_leng++
      }
    }
    return(substr(str,1,lcp_leng))
}

```

<p>Output:</p>

```txt

'interspecies,interstellar,interstate' = 'inters'
'throne,throne' = 'throne'
'throne,dungeon' = ''
'throne,,throne' = ''
'cheese' = 'cheese'
'' = ''
'prefix,suffix' = ''
'foo,foobar' = 'foo'

```



## C


```C

#include<stdarg.h>
#include<string.h>
#include<stdlib.h>
#include<stdio.h>

char* lcp(int num,...){
	va_list vaList,vaList2;
	int i,j,len,min;
	char* dest;
	char** strings = (char**)malloc(num*sizeof(char*));

	va_start(vaList,num);
	va_start(vaList2,num);

	for(i=0;i<num;i++){
		len = strlen(va_arg(vaList,char*));
		strings[i] = (char*)malloc((len + 1)*sizeof(char));

		strcpy(strings[i],va_arg(vaList2,char*));

		if(i==0)
			min = len;
		else if(len<min)
			min = len;
	}

	if(min==0)
		return "";

	for(i=0;i<min;i++){
		for(j=1;j<num;j++){
			if(strings[j][i]!=strings[0][i]){
				if(i==0)
					return "";
				else{
					dest = (char*)malloc(i*sizeof(char));
					strncpy(dest,strings[0],i-1);
					return dest;
				}
			}
		}
	}

	dest = (char*)malloc((min+1)*sizeof(char));
	strncpy(dest,strings[0],min);
	return dest;
}

int main(){

	printf("\nLongest common prefix : %s",lcp(3,"interspecies","interstellar","interstate"));
        printf("\nLongest common prefix : %s",lcp(2,"throne","throne"));
        printf("\nLongest common prefix : %s",lcp(2,"throne","dungeon"));
        printf("\nLongest common prefix : %s",lcp(3,"throne","","throne"));
        printf("\nLongest common prefix : %s",lcp(1,"cheese"));
        printf("\nLongest common prefix : %s",lcp(1,""));
        printf("\nLongest common prefix : %s",lcp(0,NULL));
        printf("\nLongest common prefix : %s",lcp(2,"prefix","suffix"));
        printf("\nLongest common prefix : %s",lcp(2,"foo","foobar"));
	return 0;
}

```

Output:

```txt

Longest common prefix : inter
Longest common prefix : throne
Longest common prefix :
Longest common prefix :
Longest common prefix : cheese
Longest common prefix :
Longest common prefix :
Longest common prefix :
Longest common prefix : foo

```



## C++


```cpp
#include <set>
#include <algorithm>
#include <string>
#include <iostream>
#include <vector>
#include <numeric>

std::set<std::string> createPrefixes ( const std::string & s ) {
   std::set<std::string> result ;
   for ( int i = 1 ; i < s.size( ) + 1 ; i++ )
      result.insert( s.substr( 0 , i )) ;
   return result ;
}

std::set<std::string> findIntersection ( const std::set<std::string> & a ,
      const std::set<std::string> & b ) {
   std::set<std::string> intersection ;
   std::set_intersection( a.begin( ) , a.end( ) , b.begin( ) , b.end( ) ,
	 std::inserter ( intersection , intersection.begin( ) ) ) ;
   return intersection  ;
}

std::set<std::string> findCommonPrefixes( const std::vector<std::string> & theStrings ) {
   std::set<std::string> result ;
   if ( theStrings.size( ) == 1 ) {
      result.insert( *(theStrings.begin( ) ) ) ;
   }
   if ( theStrings.size( ) > 1 ) {
      std::vector<std::set<std::string>> prefixCollector ;
      for ( std::string s : theStrings )
	 prefixCollector.push_back( createPrefixes ( s ) ) ;
      std::set<std::string> neutralElement (createPrefixes( *(theStrings.begin( ) ) )) ;
      result = std::accumulate( prefixCollector.begin( ) , prefixCollector.end( ) ,
	    neutralElement , findIntersection ) ;
   }
   return result ;
}

std::string lcp( const std::vector<std::string> & allStrings ) {
   if ( allStrings.size( ) == 0 )
      return "" ;
   if ( allStrings.size( ) == 1 ) {
      return allStrings[ 0 ] ;
   }
   if ( allStrings.size( ) > 1 ) {
      std::set<std::string> prefixes( findCommonPrefixes ( allStrings ) ) ;
      if ( prefixes.empty( ) )
	 return "" ;
      else {
	 std::vector<std::string> common ( prefixes.begin( ) , prefixes.end( ) ) ;
	 std::sort( common.begin( ) , common.end( ) , [] ( const std::string & a,
		  const std::string & b ) { return a.length( ) > b.length( ) ; } ) ;
	 return *(common.begin( ) ) ;
      }
   }
}

int main( ) {
   std::vector<std::string> input { "interspecies" , "interstellar" , "interstate" } ;
   std::cout << "lcp(\"interspecies\",\"interstellar\",\"interstate\") = " << lcp ( input ) << std::endl ;
   input.clear( ) ;
   input.push_back( "throne" ) ;
   input.push_back ( "throne" ) ;
   std::cout << "lcp( \"throne\" , \"throne\"" << ") = " << lcp ( input ) << std::endl ;
   input.clear( ) ;
   input.push_back( "cheese" ) ;
   std::cout << "lcp( \"cheese\" ) = " << lcp ( input ) << std::endl ;
   input.clear( ) ;
   std::cout << "lcp(\"\") = " << lcp ( input ) << std::endl ;
   input.push_back( "prefix" ) ;
   input.push_back( "suffix" ) ;
   std::cout << "lcp( \"prefix\" , \"suffix\" ) = " << lcp ( input ) << std::endl ;
   input.clear( ) ;
   input.push_back( "foo" ) ;
   input.push_back( "foobar" ) ;
   std::cout << "lcp( \"foo\" , \"foobar\" ) = " << lcp ( input ) << std::endl ;
   return 0 ;
}
```


Another more concise version (C++14 for comparing dissimilar containers):


```cpp

#include <algorithm>
#include <string>
#include <iostream>
#include <vector>

std::string lcp( const std::vector<std::string> & allStrings ) {
	if (allStrings.empty()) return std::string();
	const std::string &s0 = allStrings.front();
	auto end = s0.cend();
	for(auto it=std::next(allStrings.cbegin()); it != allStrings.cend(); it++){
		auto loc = std::mismatch(s0.cbegin(), s0.cend(), it->cbegin(), it->cend());
		if (std::distance(loc.first, end)>0) end = loc.first;
	}
	return std::string(s0.cbegin(), end);
}

int main( ) {
   std::vector<std::string> input { "interspecies" , "interstellar" , "interstate" } ;
   std::cout << "lcp(\"interspecies\",\"interstellar\",\"interstate\") = " << lcp ( input ) << std::endl ;
   input.clear( ) ;
   input.push_back( "throne" ) ;
   input.push_back ( "throne" ) ;
   std::cout << "lcp( \"throne\" , \"throne\"" << ") = " << lcp ( input ) << std::endl ;
   input.clear( ) ;
   input.push_back( "cheese" ) ;
   std::cout << "lcp( \"cheese\" ) = " << lcp ( input ) << std::endl ;
   input.clear( ) ;
   std::cout << "lcp(\"\") = " << lcp ( input ) << std::endl ;
   input.push_back( "prefix" ) ;
   input.push_back( "suffix" ) ;
   std::cout << "lcp( \"prefix\" , \"suffix\" ) = " << lcp ( input ) << std::endl ;
   input.clear( ) ;
   input.push_back( "foo" ) ;
   input.push_back( "foobar" ) ;
   std::cout << "lcp( \"foo\" , \"foobar\" ) = " << lcp ( input ) << std::endl ;
   return 0 ;
}

```

```txt

lcp("interspecies","interstellar","interstate") = inters
lcp( "throne" , "throne") = throne
lcp( "cheese" ) = cheese
lcp("") =
lcp( "prefix" , "suffix" ) =
lcp( "foo" , "foobar" ) = foo

```


## C#
```c#
using System;

namespace LCP {
    class Program {
        public static string LongestCommonPrefix(params string[] sa) {
            if (null == sa) return ""; //special case
            string ret = "";
            int idx = 0;

            while (true) {
                char thisLetter = '\0';
                foreach (var word in sa) {
                    if (idx == word.Length) {
                        // if we reached the end of a word then we are done
                        return ret;
                    }
                    if (thisLetter == '\0') {
                        // if this is the first word then note the letter we are looking for
                        thisLetter = word[idx];
                    }
                    if (thisLetter != word[idx]) {
                        return ret;
                    }
                }

                // if we haven't said we are done then this position passed
                ret += thisLetter;
                idx++;
            }
        }

        static void Main(string[] args) {
            Console.WriteLine(LongestCommonPrefix("interspecies", "interstellar", "interstate"));
            Console.WriteLine(LongestCommonPrefix("throne", "throne"));
            Console.WriteLine(LongestCommonPrefix("throne", "dungeon"));
            Console.WriteLine(LongestCommonPrefix("throne", "", "throne"));
            Console.WriteLine(LongestCommonPrefix("cheese"));
            Console.WriteLine(LongestCommonPrefix(""));
            Console.WriteLine(LongestCommonPrefix(null));
            Console.WriteLine(LongestCommonPrefix("prefix", "suffix"));
            Console.WriteLine(LongestCommonPrefix("foo", "foobar"));
        }
    }
}
```

```txt
inters
throne


cheese



foo
```



## D

```D
import std.stdio;

string lcp(string[] list ...) {
    string ret = "";
    int idx;

    while(true) {
        char thisLetter = 0;
        foreach (word; list) {
            if (idx == word.length) {
                return ret;
            }
            if(thisLetter == 0) { //if this is the first word then note the letter we are looking for
                thisLetter = word[idx];
            }
            if (thisLetter != word[idx]) { //if this word doesn't match the letter at this position we are done
                return ret;
            }
        }
        ret ~= thisLetter; //if we haven't said we are done then this position passed
        idx++;
    }
}

void main() {
    writeln(lcp("interspecies","interstellar","interstate"));
    writeln(lcp("throne","throne"));
    writeln(lcp("throne","dungeon"));
    writeln(lcp("throne","","throne"));
    writeln(lcp("cheese"));
    writeln(lcp(""));
    writeln(lcp("prefix","suffix"));
    writeln(lcp("foo","foobar"));
}
```

```txt
inters
throne


cheese


foo
```



## Dyalect


```dyalect
func lcp(sa...) {
    if sa.len() == 0 || !sa[0] {
        return ""
    }

    var ret = ""
    var idx = 0

    while true {
        var thisLetter = '\0'
        for word in sa {
            if idx == word.len() {
                return ret
            }
            if thisLetter == '\0' {
                thisLetter = word[idx]
            }
            if thisLetter != word[idx] {
                return ret
            }
        }

        ret += thisLetter
        idx += 1
    }
}

print(lcp("interspecies", "interstellar", "interstate"))
print(lcp("throne", "throne"))
print(lcp("throne", "dungeon"))
print(lcp("throne", "", "throne"))
print(lcp("cheese"))
print(lcp(""))
print(lcp(nil))
print(lcp("prefix", "suffix"))
print(lcp("foo", "foobar"))
```


```txt
inters
throne


cheese



foo
```



## EchoLisp


```lisp

;; find common prefix of two strings
(define (prefix s t ) (for/string ((u s) (v t)) #:break (not (= u v)) u))

(prefix "foo" "foobar") → "foo"

;; fold over a list of strings
(define (lcp strings)
	(if
	(null? strings) ""
	(foldl prefix (first strings) (rest strings))))

 define lcp-test '(
 ("interspecies" "interstellar" "interstate")
 ("throne" "throne")
 ("throne" "dungeon")
 ("cheese")
 ("")
 ()
 ("prefix" "suffix")))

;;
(for ((t lcp-test)) (writeln t '→ (lcp t)))
    ("interspecies" "interstellar" "interstate")    →     "inters"
    ("throne" "throne")     →    "throne"
    ("throne" "dungeon")     →     ""
    ("cheese")     →     "cheese"
    ("")     →     ""
    null     →     ""
    ("prefix" "suffix")     →     ""


```



## Elixir

```elixir
defmodule RC do
  def lcp([]), do: ""
  def lcp(strs) do
    min = Enum.min(strs)
    max = Enum.max(strs)
    index = Enum.find_index(0..String.length(min), fn i -> String.at(min,i) != String.at(max,i) end)
    if index, do: String.slice(min, 0, index), else: min
  end
end

data = [
  ["interspecies","interstellar","interstate"],
  ["throne","throne"],
  ["throne","dungeon"],
  ["throne","","throne"],
  ["cheese"],
  [""],
  [],
  ["prefix","suffix"],
  ["foo","foobar"]
]

Enum.each(data, fn strs ->
  IO.puts "lcp(#{inspect strs}) = #{inspect RC.lcp(strs)}"
end)
```


```txt

lcp(["interspecies", "interstellar", "interstate"]) = "inters"
lcp(["throne", "throne"]) = "throne"
lcp(["throne", "dungeon"]) = ""
lcp(["throne", "", "throne"]) = ""
lcp(["cheese"]) = "cheese"
lcp([""]) = ""
lcp([]) = ""
lcp(["prefix", "suffix"]) = ""
lcp(["foo", "foobar"]) = "foo"

```




## Erlang


A bow to the perversion of the Scala implementation. Not canonical erlang, this.

```erlang


-module(lcp).
-export([ main/1 ]).

shortest(List,Size) when length(List) =:= 0 ->
    Size;

shortest(List,Size) ->
    [H|T] = List,
    if
      length(H) < Size ->
         shortest(T, length(H) );
       true ->
         shortest(T, Size )
end.

uniq(List, Size ) ->
    First = string:substr(hd(List),1,Size),
    Last = string:substr(lists:last(List),1,Size),
    Ttuples = lists:zip(First, Last),
    % this is the bit that is like the scala version
    TheList = lists:takewhile(
        fun(E) ->
          case element(1,E) =:= element(2,E) of true -> true;
          _ -> false
          end
        end, Ttuples),
    Prefix = length(TheList),
    io:format("Prefix: ~p~n", [string:substr(First,1,Prefix)]).

main(List) ->
      Sorted = lists:sort(List),
      if
        length(List) < 2 ->
          io:format("Prefix empty:$~p~n",[List]);
        true ->
          Size = length(hd(List)),
          uniq(Sorted, shortest(Sorted,Size))
      end.



```

```txt

6> Data =
[["interspecies","interstellar","interstate"],
 ["throne","throne"],
 ["throne","dungeon"],
 ["throne",[],"throne"],
 ["cheese"],
 [[]],
 [],
 ["prefix","suffix"],
 ["foo","foobar"],
 ["foreign","forsake","forget","forlorn","forgiven"]].
7> [lcp:main(X) || X <- Data].
Prefix: "inters"
Prefix: "throne"
Prefix: []
Prefix: []
Prefix empty:$["cheese"]
Prefix empty:$[[]]
Prefix empty:$[]
Prefix: []
Prefix: "foo"
Prefix: "for"
[ok,ok,ok,ok,ok,ok,ok,ok,ok,ok]

```



## Factor


```factor
USING: continuations formatting fry kernel sequences strings ;
IN: rosetta-code.lcp

! Find the longest common prefix of two strings.
: binary-lcp ( str1 str2 -- str3 )
    [ SBUF" " clone ] 2dip
    '[ _ _ [ over = [ suffix! ] [ drop return ] if ] 2each ]
    with-return >string ;

! Reduce a sequence of strings using binary-lcp.
: lcp ( seq -- str )
    [ "" ] [ dup first [ binary-lcp ] reduce ] if-empty ;

: lcp-demo ( -- )
    {
        { "interspecies" "interstellar" "interstate" }
        { "throne" "throne" }
        { "throne" "dungeon" }
        { "throne" "" "throne" }
        { "cheese" }
        { "" }
        { }
        { "prefix" "suffix" }
        { "foo" "foobar" }
    } [ dup lcp "%u lcp = %u\n" printf ] each ;

MAIN: lcp-demo
```

```txt

{ "interspecies" "interstellar" "interstate" } lcp = "inters"
{ "throne" "throne" } lcp = "throne"
{ "throne" "dungeon" } lcp = ""
{ "throne" "" "throne" } lcp = ""
{ "cheese" } lcp = "cheese"
{ "" } lcp = ""
{ } lcp = ""
{ "prefix" "suffix" } lcp = ""
{ "foo" "foobar" } lcp = "foo"

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function lcp(s() As String) As String
  Dim As Integer lb = LBound(s)
  Dim As Integer ub = UBound(s)
  Dim length As Integer = ub - lb + 1
  If length = 0 Then Return ""    '' empty array
  If length = 1 Then Return s(lb) '' only one element
  ' find length of smallest string
  Dim minLength As Integer = Len(s(lb))
  For i As Integer = lb + 1 To ub
    If Len(s(i)) < minLength Then minLength = Len(s(i))
    If minLength = 0 Then Return ""  '' at least one string is empty
  Next
  Dim prefix As String
  Dim isCommon As Boolean
  Do
     prefix = Left(s(lb), minLength)
     isCommon = True
     For i As Integer = lb + 1 To ub
       If Left(s(i), minLength) <> prefix Then
         isCommon = False
         Exit For
       End If
     Next
     If isCommon Then Return prefix
     minLength -= 1
     If minLength = 0 Then Return ""
  Loop
End Function


Dim s1(1 To 3) As String = {"interspecies","interstellar","interstate"}
Print "lcp(""interspecies"",""interstellar"",""interstate"") = """; lcp(s1()); """"

Dim s2(1 To 2) As String = {"throne", "throne"}
Print "lcp(""throne"", ""throne"") = """; lcp(s2()); """"

Dim s3(1 To 2) As String = {"throne", "dungeon"}
Print "lcp(""throne"", ""dungeon"") = """; lcp(s3()); """"

Dim s4(1 To 3) As String = {"throne", "", "dungeon"}
Print "lcp(""throne"", """", ""dungeon"") = """; lcp(s4()); """"

Dim s5(1 To 1) As String = {"cheese"}
Print "lcp(""cheese"") = """; lcp(s5()); """"

Dim s6(1 To 1) As String
Print "lcp("""") = """; lcp(s6()); """"

Dim s7() As String
Print "lcp() = """; lcp(s7()); """"

Dim s8(1 To 2) As String = {"prefix", "suffix"}
Print "lcp(""prefix"", ""suffix"") = """; lcp(s8()); """"

Dim s9(1 To 2) As String = {"foo", "foobar"}
Print "lcp(""foo"", ""foobar"") = """; lcp(s9()); """"

Print
Print "Press any key to quit"
Sleep
```


```txt

lcp("interspecies","interstellar","interstate") = "inters"
lcp("throne", "throne") = "throne"
lcp("throne", "dungeon") = ""
lcp("throne", "", "dungeon") = ""
lcp("cheese") = "cheese"
lcp("") = ""
lcp() = ""
lcp("prefix", "suffix") = ""
lcp("foo", "foobar") = "foo"

```



## Go


```go
package main

import "fmt"

// lcp finds the longest common prefix of the input strings.
// It compares by bytes instead of runes (Unicode code points).
// It's up to the caller to do Unicode normalization if desired
// (e.g. see golang.org/x/text/unicode/norm).
func lcp(l []string) string {
	// Special cases first
	switch len(l) {
	case 0:
		return ""
	case 1:
		return l[0]
	}
	// LCP of min and max (lexigraphically)
	// is the LCP of the whole set.
	min, max := l[0], l[0]
	for _, s := range l[1:] {
		switch {
		case s < min:
			min = s
		case s > max:
			max = s
		}
	}
	for i := 0; i < len(min) && i < len(max); i++ {
		if min[i] != max[i] {
			return min[:i]
		}
	}
	// In the case where lengths are not equal but all bytes
	// are equal, min is the answer ("foo" < "foobar").
	return min
}

// Normally something like this would be a TestLCP function in *_test.go
// and use the testing package to report failures.
func main() {
	for _, l := range [][]string{
		{"interspecies", "interstellar", "interstate"},
		{"throne", "throne"},
		{"throne", "dungeon"},
		{"throne", "", "throne"},
		{"cheese"},
		{""},
		nil,
		{"prefix", "suffix"},
		{"foo", "foobar"},
	} {
		fmt.Printf("lcp(%q) = %q\n", l, lcp(l))
	}
}
```

```txt

lcp(["interspecies" "interstellar" "interstate"]) = "inters"
lcp(["throne" "throne"]) = "throne"
lcp(["throne" "dungeon"]) = ""
lcp(["throne" "" "throne"]) = ""
lcp(["cheese"]) = "cheese"
lcp([""]) = ""
lcp([]) = ""
lcp(["prefix" "suffix"]) = ""
lcp(["foo" "foobar"]) = "foo"

```



## Haskell

This even works on infinite strings (that have a finite longest common prefix), due to Haskell's laziness.

```haskell
import Data.List (intercalate)

lcp
  :: (Eq a)
  => [[a]] -> [a]
lcp = fmap head . takeWhile allEqual . truncTranspose
  where
    truncTranspose :: [[a]] -> [[a]]
    truncTranspose xs
      | any null xs = []
      | otherwise = (head <$> xs) : truncTranspose (tail <$> xs)
    allEqual
      :: (Eq a)
      => [a] -> Bool
    allEqual (x:xs) = all (== x) xs

-- Similar to transpose, but stops on end of shortest list.
showPrefix :: [String] -> String
showPrefix xs = show xs ++ " -> " ++ show (lcp xs)

main :: IO ()
main = do
  putStrLn $
    intercalate
      "\n"
      (showPrefix <$>
       [ ["interspecies", "interstellar", "interstate"]
       , ["throne", "throne"]
       , ["throne", "dungeon"]
       , ["cheese"]
       , [""]
       , ["prefix", "suffix"]
       , ["foo", "foobar"]
       ])
  putStrLn []
  print $ lcp ["abc" ++ repeat 'd', "abcde" ++ repeat 'f'] -- prints
```

```txt
["interspecies","interstellar","interstate"] -> "inters"
["throne","throne"] -> "throne"
["throne","dungeon"] -> ""
["cheese"] -> "cheese"
[""] -> ""
["prefix","suffix"] -> ""
["foo","foobar"] -> "foo"

"abcd"
```



## J



```J
lcp=: {. {.~ 0 i.~ [: */2 =/\ ]
```


In other words: compare adjacent strings pair-wise, combine results logically, find first mismatch in any of them, take that many characters from the first of the strings.

Note that we rely on J's handling of edge cases here. In other words: if we have only one string that falls out as the longest prefix, and if we have no strings the result is the empty string.

As the number of adjacent pairs is O(n) where n is the number of strings, this approach could be faster in the limit cases than sorting.

Examples:


```J
   lcp 'interspecies','interstellar',:'interstate'
inters
   lcp 'throne',:'throne'
throne
   lcp 'throne',:'dungeon'

   lcp ,:'cheese'
cheese
   lcp ,:''

   lcp 0 0$''

   lcp 'prefix',:'suffix'

```



## Java

```java5
public class LCP {
    public static String lcp(String... list){
        if(list == null) return "";//special case
        String ret = "";
        int idx = 0;

        while(true){
            char thisLetter = 0;
            for(String word : list){
                if(idx == word.length()){ //if we reached the end of a word then we are done
                    return ret;
                }
                if(thisLetter == 0){ //if this is the first word then note the letter we are looking for
                    thisLetter = word.charAt(idx);
                }
                if(thisLetter != word.charAt(idx)){ //if this word doesn't match the letter at this position we are done
                    return ret;
                }
            }
            ret += thisLetter;//if we haven't said we are done then this position passed
            idx++;
        }
    }

    public static void main(String[] args){
        System.out.println(lcp("interspecies","interstellar","interstate"));
        System.out.println(lcp("throne","throne"));
        System.out.println(lcp("throne","dungeon"));
        System.out.println(lcp("throne","","throne"));
        System.out.println(lcp("cheese"));
        System.out.println(lcp(""));
        System.out.println(lcp(null));
        System.out.println(lcp("prefix","suffix"));
        System.out.println(lcp("foo","foobar"));
    }
}
```

```txt
inters
throne


cheese



foo
```




## JavaScript



### ES5



```JavaScript
(function () {
    'use strict';

    function lcp() {
        var lst = [].slice.call(arguments),
            n = lst.length ? takewhile(same, zip.apply(null, lst)).length : 0;

        return n ? lst[0].substr(0, n) : '';
    }


    // (a -> Bool) -> [a] -> [a]
    function takewhile(p, lst) {
        var x = lst.length ? lst[0] : null;
        return x !== null && p(x) ? [x].concat(takewhile(p, lst.slice(1))) : [];
    }

    // Zip arbitrary number of lists (an imperative implementation)
    // [[a]] -> [[a]]
    function zip() {
        var lngLists = arguments.length,
            lngMin = Infinity,
            lstZip = [],
            arrTuple = [],
            lngLen, i, j;

        for (i = lngLists; i--;) {
            lngLen = arguments[i].length;
            if (lngLen < lngMin) lngMin = lngLen;
        }

        for (i = 0; i < lngMin; i++) {
            arrTuple = [];
            for (j = 0; j < lngLists; j++) {
                arrTuple.push(arguments[j][i]);
            }
            lstZip.push(arrTuple);
        }
        return lstZip;
    }

    // [a] -> Bool
    function same(lst) {
        return (lst.reduce(function (a, x) {
            return a === x ? a : null;
        }, lst[0])) !== null;
    }


    // TESTS

    return [
        lcp("interspecies", "interstellar", "interstate") === "inters",
        lcp("throne", "throne") === "throne",
        lcp("throne", "dungeon") === "",
        lcp("cheese") === "cheese",
        lcp("") === "",
        lcp("prefix", "suffix") === "",
        lcp("foo", "foobar") == "foo"
    ];

})();
```


```JavaScript
[true, true, true, true, true, true, true]
```



We could also, of course, use a functional implementation of a zip for an arbitrary number of arguments (e.g. as below).
A good balance is often, however, to functionally compose primitive elements which are themselves iteratively implemented.

The functional composition facilitates refactoring, code reuse, and brisk development, while the imperative implementations can sometimes give significantly better performance in ES5, which does not optimise tail recursion.
( Tail call optimisation is, however, envisaged for ES6 - see https://kangax.github.io/compat-table/es6/ for progress towards its implementation ).

This functionally implemented zip is significantly slower than the iterative version used above:


```JavaScript
// Zip arbitrary number of lists (a functional implementation, this time)
// Accepts arrays or strings, and returns [[a]]
function zip() {
    var args = [].slice.call(arguments),
        lngMin = args.reduce(function (a, x) {
            var n = x.length;
            return n < a ? n : a;
        }, Infinity);

    if (lngMin) {
        return args.reduce(function (a, v) {
            return (
                typeof v === 'string' ? v.split('') : v
            ).slice(0, lngMin).map(a ? function (x, i) {
                return a[i].concat(x);
            } : function (x) {
                return [x];
            });
        }, null)
    } else return [];
}
```



### ES6


```javascript
(() => {
    'use strict';

    // lcp :: (Eq a) => [[a]] -> [a]
    const lcp = xs => {
        const go = xs =>
            xs.some(isNull) ? (
                []
            ) : cons(
                map(head, xs),
                go(map(tail, xs))
            );
        return concat(map(
            head,
            takeWhile(
                allSame,
                go(map(chars, xs))
            )
        ));
    };


    // TEST ---------------------------------------------

    // showPrefix :: [String] -> String
    const showPrefix = xs =>
        concat([show(xs), '  --> ', show(lcp(xs))]);

    // main :: IO ()
    const main = () => {
        const strResults = unlines(map(
            showPrefix, [
                ["interspecies", "interstellar", "interstate"],
                ["throne", "throne"],
                ["throne", "dungeon"],
                ["cheese"],
                [""],
                ["prefix", "suffix"],
                ["foo", "foobar"]
            ]
        ));
        return (
            // console.log(strResults),
            strResults
        );
    };

    // GENERIC FUNCTIONS --------------------------------

    // allSame :: [a] -> Bool
    const allSame = xs =>
        0 === xs.length || (() => {
            const x = xs[0];
            return xs.every(y => x === y)
        })();

    // chars :: String -> [Char]
    const chars = s => s.split('');

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];

    // cons :: a -> [a] -> [a]
    const cons = (x, xs) => [x].concat(xs);

    // head :: [a] -> a
    const head = xs => xs.length ? xs[0] : undefined;

    // isNull :: [a] -> Bool
    // isNull :: String -> Bool
    const isNull = xs =>
        Array.isArray(xs) || ('string' === typeof xs) ? (
            1 > xs.length
        ) : undefined;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);

    // show :: a -> String
    const show = JSON.stringify;

    // tail :: [a] -> [a]
    const tail = xs => 0 < xs.length ? xs.slice(1) : [];

    // takeWhile :: (a -> Bool) -> [a] -> [a]
    // takeWhile :: (Char -> Bool) -> String -> String
    const takeWhile = (p, xs) => {
        const lng = xs.length;
        return 0 < lng ? xs.slice(
            0,
            until(
                i => lng === i || !p(xs[i]),
                i => 1 + i,
                0
            )
        ) : [];
    };

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = (p, f, x) => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    };

    // MAIN ---
    return main();
})();
```

```txt
["interspecies","interstellar","interstate"]  --> "inters"
["throne","throne"]  --> "throne"
["throne","dungeon"]  --> []
["cheese"]  --> "cheese"
[""]  --> []
["prefix","suffix"]  --> []
["foo","foobar"]  --> "foo"
```



## jq


See [[#Scala]] for a description of the approach used in this section.

```jq
# If your jq includes until/2
# then feel free to omit the following definition:
def until(cond; next):
  def _until: if cond then . else (next|_until) end;  _until;
```



```jq
def longest_common_prefix:
 if length == 0 then ""        # by convention
 elif length == 1 then .[0]    # for speed
 else sort
 | if .[0] == "" then ""       # for speed
   else .[0] as $first
   |    .[length-1] as $last
   | ([$first, $last] | map(length) | min) as $n
   | 0 | until( . == $n or $first[.:.+1] != $last[.:.+1]; .+1)
   | $first[0:.]
   end
 end;
```


'''Test Cases'''

```jq
def check(ans): longest_common_prefix == ans;

(["interspecies","interstellar","interstate"] | check("inters")) and
(["throne","throne"]                          | check("throne")) and
(["throne","dungeon"]                         | check("")) and
(["throne", "", "throne"]                     | check("")) and
(["cheese"]                                   | check("cheese")) and
([""]                                         | check("")) and
([]                                           | check("")) and
(["prefix","suffix"]                          | check("")) and
(["foo","foobar"]                             | check("foo"))

```

```sh
$ jq -n -f longest_common_prefix.jq
true
```



## Julia

```julia
function lcp(str::AbstractString...)
    r = IOBuffer()
    str = [str...]
    if !isempty(str)
        i = 1
        while all(i ≤ length(s) for s in str) && all(s == str[1][i] for s in getindex.(str, i))
            print(r, str[1][i])
            i += 1
        end
    end
    return String(r)
end

@show lcp("interspecies", "interstellar", "interstate")
@show lcp("throne","throne")
@show lcp("throne","dungeon")
@show lcp("throne", "", "throne")
@show lcp("cheese")
@show lcp("")
@show lcp()
@show lcp("prefix","suffix")
@show lcp("foo","foobar")
```


```txt
lcp("interspecies", "interstellar", "interstate") = "inters"
lcp("throne", "throne") = "throne"
lcp("throne", "dungeon") = ""
lcp("throne", "", "throne") = ""
lcp("cheese") = "cheese"
lcp("") = ""
lcp() = ""
lcp("prefix", "suffix") = ""
lcp("foo", "foobar") = "foo"
```



## Kotlin


```scala
// version 1.0.6

fun lcp(vararg sa: String): String {
    if (sa.isEmpty()) return ""
    if (sa.size == 1) return sa[0]
    val minLength = sa.map { it.length }.min()!!
    var oldPrefix = ""
    var newPrefix: String
    for (i in 1 .. minLength) {
        newPrefix = sa[0].substring(0, i)
        for (j in 1 until sa.size)
            if (!sa[j].startsWith(newPrefix)) return oldPrefix
        oldPrefix = newPrefix
    }
    return oldPrefix
}

fun main(args: Array<String>) {
    println("The longest common prefixes of the following collections of strings are:\n")
    println("""["interspecies","interstellar","interstate"] = "${lcp("interspecies", "interstellar", "interstate")}"""")
    println("""["throne","throne"]                          = "${lcp("throne", "throne")}"""")
    println("""["throne","dungeon"]                         = "${lcp("throne", "dungeon")}"""")
    println("""["throne","","throne"]                       = "${lcp("throne", "", "throne")}"""")
    println("""["cheese"]                                   = "${lcp("cheese")}"""")
    println("""[""]                                         = "${lcp("")}"""")
    println("""[]                                           = "${lcp()}"""")
    println("""["prefix","suffix"]                          = "${lcp("prefix", "suffix")}"""")
    println("""["foo","foobar"]                             = "${lcp("foo", "foobar")}"""")
}
```


```txt

The longest common prefixes of the following collections of strings are:

["interspecies","interstellar","interstate"] = "inters"
["throne","throne"]                          = "throne"
["throne","dungeon"]                         = ""
["throne","","throne"]                       = ""
["cheese"]                                   = "cheese"
[""]                                         = ""
[]                                           = ""
["prefix","suffix"]                          = ""
["foo","foobar"]                             = "foo"

```



## Lua


```Lua
function lcp (strList)
    local shortest, prefix, first = math.huge, ""
    for _, str in pairs(strList) do
        if str:len() < shortest then shortest = str:len() end
    end
    for strPos = 1, shortest do
        if strList[1] then
            first = strList[1]:sub(strPos, strPos)
        else
            return prefix
        end
        for listPos = 2, #strList do
            if strList[listPos]:sub(strPos, strPos) ~= first then
                return prefix
            end
        end
        prefix = prefix .. first
    end
    return prefix
end

local testCases, pre = {
    {"interspecies", "interstellar", "interstate"},
    {"throne", "throne"},
    {"throne", "dungeon"},
    {"throne", "", "throne"},
    {"cheese"},
    {""},
    {nil},
    {"prefix", "suffix"},
    {"foo", "foobar"}
}
for _, stringList in pairs(testCases) do
    pre = lcp(stringList)
    if pre == "" then print(string.char(238)) else print(pre) end
end
```

```txt
inters
throne
ε
ε
cheese
ε
ε
ε
foo
```




## Maple


```Maple
lcp := proc(arr)
	local A:
	if (arr = []) then return "": end if:
	A := sort(arr):
	return (A[1][1..(StringTools:-CommonPrefix(A[1],A[-1]))]):
end proc:
```

'''Test Cases'''

```Maple
lcp(["interspecies","interstellar","interstate"]);
lcp(["throne","throne"]);
lcp(["throne","dungeon"]);
lcp(["throne","","dungeon"]);
lcp(["cheese"]);
lcp([""]);
lcp([]);
lcp(["prefix","suffix"]);
lcp(["foo","foobar"]);
```

```txt
inters
throne
""
""
cheese
""
""
""
foo
```



## MiniScript

We find the shortest and longest strings (without sorting, which makes the code slightly longer but much more efficient), and then just compare those.

```MiniScript
lcp = function(strList)
    if not strList then return null
    // find the shortest and longest strings (without sorting!)
    shortest = strList[0]
    longest = strList[0]
    for s in strList
        if s.len < shortest.len then shortest = s
        if s.len > longest.len then longest = s
    end for
    if shortest.len < 1 then return ""
    // now find how much of the shortest matches the longest
    for i in range(0, shortest.len-1)
        if shortest[i] != longest[i] then return shortest[:i]
    end for
    return shortest
end function

print lcp(["interspecies","interstellar","interstate"])
print lcp(["throne","throne"])
print lcp(["throne","dungeon"])
print lcp(["throne", "", "throne"])
print lcp(["cheese"])
print lcp([])
print lcp(["foo","foobar"])
```


```txt
inters
throne


cheese
null
foo
```


=={{header|Modula-2}}==
```modula2
MODULE LCP;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

TYPE String = ARRAY[0..15] OF CHAR;

PROCEDURE Length(str : String) : CARDINAL;
VAR len : CARDINAL;
BEGIN
    len := 0;
    WHILE str[len] # 0C DO
        INC(len)
    END;
    RETURN len
END Length;

PROCEDURE LongestCommonPrefix(params : ARRAY OF String) : String;
VAR
    ret : String;
    idx,widx : CARDINAL;
    thisLetter : CHAR;
BEGIN
    ret := "";
    idx := 0;
    LOOP
        thisLetter := 0C;
        FOR widx:=0 TO HIGH(params) DO
            IF idx = Length(params[widx]) THEN
                (* if we reached the end of a word then we are done *)
                RETURN ret
            END;
            IF thisLetter = 0C THEN
                (* if this is the first word then note the letter we are looking for *)
                thisLetter := params[widx][idx]
            END;
            IF thisLetter # params[widx][idx] THEN
                RETURN ret
            END
        END;

        (* if we haven't said we are done then this position passed *)
        ret[idx] := thisLetter;
        INC(idx);
        ret[idx] := 0C
    END;
    RETURN ret
END LongestCommonPrefix;

(* Main *)
TYPE
    AS3 = ARRAY[0..2] OF String;
    AS2 = ARRAY[0..1] OF String;
    AS1 = ARRAY[0..0] OF String;
BEGIN
    WriteString(LongestCommonPrefix(AS3{"interspecies", "interstellar", "interstate"}));
    WriteLn;
    WriteString(LongestCommonPrefix(AS2{"throne", "throne"}));
    WriteLn;
    WriteString(LongestCommonPrefix(AS2{"throne", "dungeon"}));
    WriteLn;
    WriteString(LongestCommonPrefix(AS3{"throne", "", "throne"}));
    WriteLn;
    WriteString(LongestCommonPrefix(AS1{"cheese"}));
    WriteLn;
    WriteString(LongestCommonPrefix(AS1{""}));
    WriteLn;
    WriteString(LongestCommonPrefix(AS2{"prefix", "suffix"}));
    WriteLn;
    WriteString(LongestCommonPrefix(AS2{"foo", "foobar"}));
    WriteLn;

    ReadChar
END LCP.
```

```txt
inters
throne


cheese

foo
```


==ooRexx==
```oorexx
Call assert lcp(.list~of("interspecies","interstellar","interstate")),"inters"
Call assert lcp(.list~of("throne","throne")),"throne"
Call assert lcp(.list~of("throne","dungeon")),""
Call assert lcp(.list~of("cheese")),"cheese"
Call assert lcp(.list~of("",""))
Call assert lcp(.list~of("prefix","suffix")),""
Call assert lcp(.list~of("a","b","c",'aaa')),""
Exit

assert:
  If arg(1)==arg(2) Then tag='ok'
                    Else tag='??'
  Say tag 'lcp="'arg(1)'"'
  Say ''
  Return

lcp:
Use Arg l
a=l~makearray()
s=l~makearray()~makestring((LINE),',')
say 'lcp('s')'
an=a~dimension(1)
If an=1 Then
  Return a[1]
s=lcp2(a[1],a[2])
Do i=3 To an While s<>''
  s=lcp2(s,a[i])
  End
Return s

lcp2:
Do i=1 To min(length(arg(1)),length(arg(2)))
  If substr(arg(1),i,1)<>substr(arg(2),i,1) Then
    Leave
  End
Return left(arg(1),i-1)
```

```txt
lcp(interspecies,interstellar,interstate)
ok lcp="inters"

lcp(throne,throne)
ok lcp="throne"

lcp(throne,dungeon)
ok lcp=""

lcp(cheese)
ok lcp="cheese"

lcp(,)
ok lcp=""

lcp(prefix,suffix)
ok lcp=""

lcp(a,b,c,aaa)
ok lcp=""
```



## Perl


If the strings are known not to contain null-bytes, we can let the regex backtracking engine find the longest common prefix like this:


```perl
sub lcp {
    (join("\0", @_) =~ /^ ([^\0]*) [^\0]* (?:\0 \1 [^\0]*)* $/sx)[0];
}
```


Testing:

```perl
use Test::More;
plan tests => 8;

is lcp("interspecies","interstellar","interstate"), "inters";
is lcp("throne","throne"),                          "throne";
is lcp("throne","dungeon"),                         "";
is lcp("cheese"),                                   "cheese";
is lcp(""),                                         "";
is lcp(),                                           "";
is lcp("prefix","suffix"),                          "";
is lcp("foo","foobar"),                             "foo";
```


As in the Perl 6 example.


## Perl 6

This should work on infinite strings (if and when we get them), since <tt>.ords</tt> is lazy.  In any case, it does just about the minimal work by evaluating all strings lazily in parallel.  A few explanations of the juicy bits: <tt>@s</tt> is the list of strings, and the hyper operator <tt>»</tt> applies the <tt>.ords</tt> to each of those strings, producing a list of lists.  The <tt>|</tt> operator inserts each of those sublists as an argument into an argument list so that we can use a reduction operator across the list of lists, which makes sense if the operator in question knows how to deal with list arguments.  In this case we use the <tt>Z</tt> ('zip') metaoperator with <tt>eqv</tt> as a base operator, which runs <tt>eqv</tt> across all the lists in parallel for each position, and will fail if not all the lists have the same ordinal value at that position, or if any of the strings run out of characters.  Then we count up the leading matching positions and carve up one of the strings to that length.

```perl6
multi lcp()    { '' }
multi lcp($s)  { ~$s }
multi lcp(*@s) { substr @s[0], 0, [+] [\and] [Zeqv] |@s».ords }

use Test;
plan 8;

is lcp("interspecies","interstellar","interstate"), "inters";
is lcp("throne","throne"), "throne";
is lcp("throne","dungeon"), '';
is lcp("cheese"), "cheese";
is lcp(''), '';
is lcp(), '';
is lcp("prefix","suffix"), '';
is lcp("foo","foobar"), 'foo';
```

```txt
1..8
ok 1 -
ok 2 -
ok 3 -
ok 4 -
ok 5 -
ok 6 -
ok 7 -
ok 8 -
```



## Phix


```Phix
function lcp(sequence strings)
string res = ""
    if length(strings) then
        res = strings[1]
        for i=2 to length(strings) do
            string si = strings[i]
            for j=1 to length(res) do
                if j>length(si) or res[j]!=si[j] then
                    res = res[1..j-1]
                    exit
                end if
            end for
            if length(res)=0 then exit end if
        end for
    end if
    return res
end function

constant tests = {{"interspecies", "interstellar", "interstate"},
                  {"throne", "throne"},
                  {"throne", "dungeon"},
                  {"throne", "", "throne"},
                  {"cheese"},
                  {""},
                  {},
                  {"prefix", "suffix"},
                  {"foo", "foobar"}
                 }
for i=1 to length(tests) do
    ?lcp(tests[i])
end for
```

```txt

"inters"
"throne"
""
""
"cheese"
""
""
""
"foo"

```



## PL/I

```pli
*process source xref attributes or(!);
 (subrg):
 lcpt: Proc Options(main);
 Call assert(lcp('interspecies interstellar interstate'),'inters');
 Call assert(lcp('throne throne'),'throne');
 Call assert(lcp('throne dungeon'),'');
 Call assert(lcp('cheese'),'cheese');
 Call assert(lcp(' '),' ');
 Call assert(lcp('prefix suffix'),'');
 Call assert(lcp('a b c aaa'),'');

 assert: Proc(result,expected);
   Dcl (result,expected) Char(*) Var;
   Dcl tag Char(2) Init('ok');
   If result^=expected Then tag='??';
   Put Edit(tag,' lcp="',result,'"','')(Skip,4(a));
   End;

 lcp: Proc(string) Returns(Char(50) Var);
   Dcl string Char(*);
   Dcl xstring Char(50) Var;
   Dcl bn Bin Fixed(31) Init(0);
   Dcl bp(20) Bin Fixed(31);
   Dcl s Char(50) Var;
   Dcl i Bin Fixed(31);
   xstring=string!!' ';
   Put Edit('"'!!string!!'"')(Skip,a);
   Do i=2 To length(xstring);
     If substr(xstring,i,1)=' ' Then Do;
       bn+=1;
       bp(bn)=i;
       End;
     End;
   If bn=1 Then Return(substr(string,1,bp(1)-1));
   s=lcp2(substr(string,1,bp(1)-1),substr(string,bp(1)+1,bp(2)-bp(1)));
   Do i=3 To bn While(s^='');
     s=lcp2(s,substr(string,bp(i-1)+1,bp(i)-bp(i-1)));
     End;
   Return(s);
   End;

 lcp2: Proc(u,v) Returns(Char(50) Var);
   Dcl (u,v) Char(*);
   Dcl s Char(50) Var;
   Dcl i Bin Fixed(31);
   Do i=1 To min(length(u),length(v));
     If substr(u,i,1)^=substr(v,i,1) Then
       Leave;
     End;
   Return(left(u,i-1));
   End;

 End;
```

```txt
"interspecies interstellar interstate"
ok lcp="inters"

"throne throne"
ok lcp="throne"

"throne dungeon"
ok lcp=""

"cheese"
ok lcp="cheese"

" "
ok lcp=" "

"prefix suffix"
ok lcp=""

"a b c aaa"
ok lcp=""
```



## PowerShell


```PowerShell

function lcp ($arr) {
    if($arr){
        $arr = $arr | sort {$_.length} | select -unique
        if(1 -lt $arr.count) {
            $lim, $i, $test = $arr[0].length, 0, $true
            while (($i -lt $lim) -and $test) {
                $test = ($arr | group {$_[$i]}).Name.Count -eq 1
                if ($test) {$i += 1}
            }
            $arr[0].substring(0,$i)
        } else {$arr}
    } else{''}

}
function show($arr) {
    function quote($str) {"`"$str`""}
    "lcp @($(($arr | foreach{quote $_}) -join ', ')) = $(lcp $arr)"
}
show @("interspecies","interstellar","interstate")
show @("throne","throne")
show @("throne","dungeon")
show @("throne", "","throne")
show @("cheese")
show @("")
show @()
show @("prefix","suffix")
show @("foo","foobar")

```

<b>Output:</b>

```txt

lcp @("interspecies", "interstellar", "interstate") = inters
lcp @("throne", "throne") = throne
lcp @("throne", "dungeon") =
lcp @("throne", "", "throne") =
lcp @("cheese") = cheese
lcp @("") =
lcp @() =
lcp @("prefix", "suffix") =
lcp @("foo", "foobar") = foo

```



## Python

Note: this makes use of the error in <code>os.path.commonprefix</code> where it computes the longest common prefix regardless of directory separators rather than [[Find common directory path#Python|finding the common directory path]].


```python
import os.path

def lcp(*s):
    return os.path.commonprefix(s)

assert lcp("interspecies","interstellar","interstate") == "inters"
assert lcp("throne","throne") == "throne"
assert lcp("throne","dungeon") == ""
assert lcp("cheese") == "cheese"
assert lcp("") == ""
assert lcp("prefix","suffix") == ""
assert lcp("foo","foobar") == "foo"
```



### Python: Functional

To see if all the n'th characters are the same I compare the min and max characters in the lambda function.


```python
from itertools import takewhile

def lcp(*s):
    return ''.join(ch[0] for ch in takewhile(lambda x: min(x) == max(x),
					     zip(*s)))

assert lcp("interspecies","interstellar","interstate") == "inters"
assert lcp("throne","throne") == "throne"
assert lcp("throne","dungeon") == ""
assert lcp("cheese") == "cheese"
assert lcp("") == ""
assert lcp("prefix","suffix") == ""
assert lcp("foo","foobar") == "foo"
```


The above runs without output.

;Alternative Functional:
An alternative solution that takes advantage of the observation that the longest common prefix of a set of strings must be the same as the longest common prefix of the lexicographically minimal string and the lexicographically maximal string, since moving away lexicographically can only shorten the common prefix, never lengthening it. Finding the min and max could do a lot of unnecessary work though, if the strings are long and the common prefix is short.

```python
from itertools import takewhile

def lcp(*s):
    return ''.join(a for a,b in takewhile(lambda x: x[0] == x[1],
					  zip(min(s), max(s))))
```



Or, defined in terms of a generic '''transpose''' function:

```Python
from itertools import (takewhile)


# lcp :: [String] -> String
def lcp(xs):
    return ''.join(
        x[0] for x in takewhile(allSame, transpose(xs))
    )


# TEST --------------------------------------------------

# main :: IO ()
def main():
    def showPrefix(xs):
        return ''.join(
            ['[' + ', '.join(xs), '] -> ', lcp(xs)]
        )

    print (*list(map(showPrefix, [
        ["interspecies", "interstellar", "interstate"],
        ["throne", "throne"],
        ["throne", "dungeon"],
        ["cheese"],
        [""],
        ["prefix", "suffix"],
        ["foo", "foobar"]])), sep='\n'
    )


# GENERIC FUNCTIONS -------------------------------------


# allSame :: [a] -> Bool
def allSame(xs):
    if 0 < len(xs):
        x = xs[0]
        return all(map(lambda y: x == y, xs))
    else:
        return True


# transpose :: [[a]] -> [[a]]
def transpose(xs):
    return map(list, zip(*xs))


# TEST ---
if __name__ == '__main__':
    main()
```

```txt
[interspecies, interstellar, interstate] -> inters
[throne, throne] -> throne
[throne, dungeon] ->
[cheese] -> cheese
[] ->
[prefix, suffix] ->
[foo, foobar] -> foo
```



## Racket


Note that there are three cases to the match, because <code>zip</code> needs at least one list, and <code>char=?</code> needs at least 2 characters to compare.

<lang>#lang racket
(require srfi/1)

(define ε "")
(define lcp
  (match-lambda*
    [(list) ε]
    [(list a) a]
    [ss (list->string
         (reverse
          (let/ec k
            (fold (lambda (a d) (if (apply char=? a) (cons (car a) d) (k d))) null
                  (apply zip (map string->list ss))))))]))

(module+ test
  (require tests/eli-tester)
  (test
   (lcp "interspecies" "interstellar" "interstate") => "inters"
   (lcp "throne" "throne") => "throne"
   (lcp "throne" "dungeon") => ""
   (lcp "cheese") => "cheese"
   (lcp ε) => ε
   (lcp) => ε
   (lcp "prefix" "suffix") => ε))
```


All tests pass.


## REXX


### version 1


```rexx
/* REXX */
Call assert lcp("interspecies","interstellar","interstate"),"inters"
Call assert lcp("throne","throne"),"throne"
Call assert lcp("throne","dungeon"),""
Call assert lcp("cheese"),"cheese"
Call assert lcp("","")
Call assert lcp("prefix","suffix"),""
Call assert lcp("a","b","c",'aaa'),""
Call assert lcp("foo",'foobar'),"foo"
Call assert lcp("ab","","abc"),""
Exit

assert:
  If arg(1)==arg(2) Then tag='ok'
                    Else tag='??'
  Say tag 'lcp="'arg(1)'"'
  Say ''
  Return

lcp: Procedure
ol='test lcp('
Do i=1 To arg()
  ol=ol||""""arg(i)""""
  If i<arg() Then ol=ol','
             Else ol=ol')'
  End
Say ol
If arg()=1 Then
  Return arg(1)
s=lcp2(arg(1),arg(2))
Do i=3 To arg() While s<>''
  s=lcp2(s,arg(i))
  End
Return s

lcp2: Procedure
Do i=1 To min(length(arg(1)),length(arg(2)))
  If substr(arg(1),i,1)<>substr(arg(2),i,1) Then
    Leave
  End
Return left(arg(1),i-1)
```

```txt
test lcp("interspecies","interstellar","interstate")
ok lcp="inters"

test lcp("throne","throne")
ok lcp="throne"

test lcp("throne","dungeon")
ok lcp=""

test lcp("cheese")
ok lcp="cheese"

test lcp("","")
ok lcp=""

test lcp("prefix","suffix")
ok lcp=""

test lcp("a","b","c","aaa")
ok lcp=""
```


test lcp("foo","foobar")
ok lcp="foo"

test lcp("ab","","abc")
ok lcp=""


### version 2

This REXX version makes use of the   '''compare'''   BIF.

```rexx
/*REXX program  computes the   longest common prefix  (LCP)   of any number of  strings.*/
say LCP('interspecies',  "interstellar",  'interstate')
say LCP('throne',  "throne")                     /*2 strings, they are exactly the same.*/
say LCP('throne',  "dungeon")                    /*2 completely different strings.      */
say LCP('throne',  '',   "throne")               /*3 strings, the middle string is null.*/
say LCP('cheese')                                /*just a single cheesy argument.       */
say LCP('')                                      /*just a single  null  argument.       */
say LCP()                                        /*no arguments are specified at all.   */
say LCP('prefix',  "suffix")                     /*two mostly different strings.        */
say LCP('foo',     "foobar")                     /*two mostly similar strings.          */
say LCP('a',  "b",  'c',  "aaa")                 /*four strings, mostly different.      */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
LCP: @=arg(1);  m=length(@);  #=arg();  say copies('▒', 50)
                                 do i=1  for #;  say '────────────── string'  i":"  arg(i)
                                 end   /*i*/
                   do j=2  to #;    x=arg(j);     t=compare(@, x)     /*compare to next.*/
                   if t==1 | x==''  then do;  @=;  leave;   end       /*mismatch of strs*/
                   if t==0 & @==x   then t=length(@) + 1              /*both are equal. */
                   if t>=m  then iterate                              /*not longest str.*/
                   m=t-1;   @=left(@,  max(0, m))                     /*define maximum. */
                   end   /*j*/
     return  '  longest common prefix='    @                          /*return answer.  */
```

'''output'''   when using the default inputs:

```txt

▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
────────────── string 1: interspecies
────────────── string 2: interstellar
────────────── string 3: interstate
  longest common prefix= inters
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
────────────── string 1: throne
────────────── string 2: throne
  longest common prefix= throne
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
────────────── string 1: throne
────────────── string 2: dungeon
  longest common prefix=
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
────────────── string 1: throne
────────────── string 2:
────────────── string 3: throne
  longest common prefix=
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
────────────── string 1: cheese
  longest common prefix= cheese
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
────────────── string 1:
  longest common prefix=
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
  longest common prefix=
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
────────────── string 1: prefix
────────────── string 2: suffix
  longest common prefix=
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
────────────── string 1: foo
────────────── string 2: foobar
  longest common prefix= foo
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
────────────── string 1: a
────────────── string 2: b
────────────── string 3: c
────────────── string 4: aaa
  longest common prefix=

```



### version 3

This REXX version explicitly shows   ''null''   values and the number of strings specified.

```rexx
/*REXX program  computes the   longest common prefix  (LCP)   of any number of  strings.*/
say LCP('interspecies',  "interstellar",  'interstate')
say LCP('throne',  "throne")                     /*2 strings, they are exactly the same.*/
say LCP('throne',  "dungeon")                    /*2 completely different strings.      */
say LCP('throne',  '',   "throne")               /*3 strings, the middle string is null.*/
say LCP('cheese')                                /*just a single cheesy argument.       */
say LCP('')                                      /*just a single  null  argument.       */
say LCP()                                        /*no arguments are specified at all.   */
say LCP('prefix',  "suffix")                     /*two mostly different strings.        */
say LCP('foo',     "foobar")                     /*two mostly similar strings.          */
say LCP('a',  "b",  'c',  "aaa")                 /*four strings, mostly different.      */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
LCP: @=arg(1);  m=length(@);  #=arg();  say copies('▒', 60)
                say '──────────────     number of strings specified:'  #
                        do i=1  for #;  say '────────────── string' i":"  showNull(arg(i))
                        end   /*i*/

                    do j=2  to #;    x=arg(j);     t=compare(@,x)     /*compare to next.*/
                    if t==1 | x==''  then do;  @=;  leave;   end      /*mismatch of strs*/
                    if t==0 & @==x   then t=length(@) + 1             /*both are equal. */
                    if t>=m          then iterate                     /*not longest str.*/
                    m=t-1;   @=left(@, max(0, m))                     /*define maximum. */
                    end   /*j*/
     return  '  longest common prefix='    @                          /*return answer.  */
/*──────────────────────────────────────────────────────────────────────────────────────*/
showNull: procedure;   parse arg z;        if z==''  then z="«null»";   return z
```

'''output'''   when using the default inputs:

```txt

▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
──────────────     number of strings specified: 3
────────────── string 1: interspecies
────────────── string 2: interstellar
────────────── string 3: interstate
  longest common prefix= inters
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
──────────────     number of strings specified: 2
────────────── string 1: throne
────────────── string 2: throne
  longest common prefix= throne
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
──────────────     number of strings specified: 2
────────────── string 1: throne
────────────── string 2: dungeon
  longest common prefix= «null»
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
──────────────     number of strings specified: 3
────────────── string 1: throne
────────────── string 2: «null»
────────────── string 3: throne
  longest common prefix= «null»
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
──────────────     number of strings specified: 1
────────────── string 1: cheese
  longest common prefix= cheese
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
──────────────     number of strings specified: 1
────────────── string 1: «null»
  longest common prefix= «null»
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
──────────────     number of strings specified: 0
  longest common prefix= «null»
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
──────────────     number of strings specified: 2
────────────── string 1: prefix
────────────── string 2: suffix
  longest common prefix= «null»
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
──────────────     number of strings specified: 2
────────────── string 1: foo
────────────── string 2: foobar
  longest common prefix= foo
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
──────────────     number of strings specified: 4
────────────── string 1: a
────────────── string 2: b
────────────── string 3: c
────────────── string 4: aaa
  longest common prefix= «null»

```



## Ring


```ring

# Project : Longest common prefix

aList1 = ["interspecies","interstellar","interstate"]
aList2 = list(len(aList1))
flag = 1
comp=""
for n=1 to len(aList1[1])
    aList2 = list(len(aList1))
    flag=1
    for m=1 to len(aList1)
        aList2[m] = left(aList1[m], n )
        compare =  left(aList1[1], n )
    next
    for p=1 to len(aList1)
        if aList2[p] != compare
           flag = 0
           exit
        ok
    next
    if flag=1
       if len(compare) > comp
          comp=compare
        ok
     ok
next
if comp=""
   see "none"
else
   see comp + nl
ok

```

Output:

```txt

inters

```



## Ruby


```ruby
def lcp(*strs)
  return "" if strs.empty?
  min, max = strs.minmax
  idx = min.size.times{|i| break i if min[i] != max[i]}
  min[0...idx]
end

data = [
  ["interspecies","interstellar","interstate"],
  ["throne","throne"],
  ["throne","dungeon"],
  ["throne","","throne"],
  ["cheese"],
  [""],
  [],
  ["prefix","suffix"],
  ["foo","foobar"]
]

data.each do |set|
  puts "lcp(#{set.inspect[1..-2]}) = #{lcp(*set).inspect}"
end
```


```txt

lcp("interspecies", "interstellar", "interstate") = "inters"
lcp("throne", "throne") = "throne"
lcp("throne", "dungeon") = ""
lcp("throne", "", "throne") = ""
lcp("cheese") = "cheese"
lcp("") = ""
lcp() = ""
lcp("prefix", "suffix") = ""
lcp("foo", "foobar") = "foo"

```




## Rust


Rust String by default is utf-8 encoded. Since utf-8 is variable width, indexing in constant time is not possible. This example therefore uses byte strings (slices of u8) for the strings. The implementation shown here is similar to the Java implementation.


```Rust

fn main() {
    let strs: [&[&[u8]]; 7] = [
        &[b"interspecies", b"interstellar", b"interstate"],
        &[b"throne", b"throne"],
        &[b"throne", b"dungeon"],
        &[b"cheese"],
        &[b""],
        &[b"prefix", b"suffix"],
        &[b"foo", b"foobar"],
    ];
    strs.iter().for_each(|list| match lcp(list) {
        Some(prefix) => println!("{}", String::from_utf8_lossy(&prefix)),
        None => println!(),
    });
}

fn lcp(list: &[&[u8]]) -> Option<Vec<u8>> {
    if list.is_empty() {
        return None;
    }
    let mut ret = Vec::new();
    let mut i = 0;
    loop {
        let mut c = None;
        for word in list {
            if i == word.len() {
                return Some(ret);
            }
            match c {
                None => {
                    c = Some(word[i]);
                }
                Some(letter) if letter != word[i] => return Some(ret),
                _ => continue,
            }
        }
        if let Some(letter) = c {
            ret.push(letter);
        }
        i += 1;
    }
}

```


'''Output:'''

```txt

inters
throne

cheese


foo

```



## Scala

Take the first and last of the set of sorted strings; zip the two strings into a sequence of tuples ('view' makes this happen laziliy, on demand), until the two characters in the tuple differ, at which point, unzip the sequence into two character sequences; finally, arbitarily take one of these sequences (they are identical) and convert back to a string

```txt

"interspecies" \                                                                 / i, n, t, e, r, s \
                > zip takeWhile: (i,i), (n,n), (t,t), (e,e), (r,r), (s,s) unzip <                     > "inters"
"intesteller"  /                                                                 \ i, n, t, e, r, s

```

```scala
class TestLCP extends FunSuite {
  test("shared start") {
    assert(lcp("interspecies","interstellar","interstate") === "inters")
    assert(lcp("throne","throne") === "throne")
    assert(lcp("throne","dungeon").isEmpty)
    assert(lcp("cheese") === "cheese")
    assert(lcp("").isEmpty)
    assert(lcp(Nil :_*).isEmpty)
    assert(lcp("prefix","suffix").isEmpty)
  }

  def lcp(list: String*) = list.foldLeft("")((_,_) =>
    (list.min.view,list.max.view).zipped.takeWhile(v => v._1 == v._2).unzip._1.mkString)
}
```



## Sidef


```ruby
# Finds the first point where the tree bifurcates
func find_common_prefix(hash, acc) {
    if (hash.len == 1) {
        var pair = hash.to_a[0]
        return __FUNC__(pair.value, acc+pair.key)
    }
    return acc
}

# Creates a tree like: {a => {b => {c => {}}}}
func lcp(*strings) {
    var hash = Hash()

    for str in (strings.sort_by{.len}) {
        var ref = hash
        str.is_empty && return ''
        for char in str {
            if (ref.contains(char)) {
                ref = ref{char}
                ref.len == 0 && break
            }
            else {
                ref = (ref{char} = Hash())
            }
        }
    }

    return find_common_prefix(hash, '')
}
```


Demonstration:

```ruby
var data = [
  ["interspecies","interstellar","interstate"],
  ["throne","throne"],
  ["throne","dungeon"],
  ["throne","","throne"],
  ["cheese"],
  [""],
  [],
  ["prefix","suffix"],
  ["foo","foobar"]
];

data.each { |set|
    say "lcp(#{set.dump.substr(1,-1)}) = #{lcp(set...).dump}";
};
```

```txt

lcp("interspecies", "interstellar", "interstate") = "inters"
lcp("throne", "throne") = "throne"
lcp("throne", "dungeon") = ""
lcp("throne", "", "throne") = ""
lcp("cheese") = "cheese"
lcp("") = ""
lcp() = ""
lcp("prefix", "suffix") = ""
lcp("foo", "foobar") = "foo"

```



## VBScript


```vb
Function lcp(s)
	'declare an array
	str = Split(s,",")
	'indentify the length of the shortest word in the array
	For i = 0 To UBound(str)
		If i = 0 Then
			l = Len(str(i))
		ElseIf Len(str(i)) < l Then
			l = Len(str(i))
		End If
	Next
	'check prefixes and increment index
	idx = 0
	For j = 1 To l
		For k = 0 To UBound(str)
			If UBound(str) = 0 Then
				idx = Len(str(0))
			Else
				If k = 0 Then
					tstr = Mid(str(k),j,1)
				ElseIf k <> UBound(str) Then
					If Mid(str(k),j,1) <> tstr Then
						Exit For
					End If
				Else
					If Mid(str(k),j,1) <> tstr Then
						Exit For
					Else
						idx = idx + 1
					End If
				End If
			End If
		Next
		If idx = 0 Then
			Exit For
		End If
	Next
	'return lcp
	If idx = 0 Then
		lcp = "No Matching Prefix"
	Else
		lcp = Mid(str(0),1,idx)
	End If
End Function

'Calling the function for test cases.
test = Array("interspecies,interstellar,interstate","throne,throne","throne,dungeon","cheese",_
		"","prefix,suffix")

For n = 0 To UBound(test)
	WScript.StdOut.Write "Test case " & n & " " & test(n) & " = " & lcp(test(n))
	WScript.StdOut.WriteLine
Next
```


```txt
Test case 0 interspecies,interstellar,interstate = inters
Test case 1 throne,throne = throne
Test case 2 throne,dungeon = No Matching Prefix
Test case 3 cheese = cheese
Test case 4  = No Matching Prefix
Test case 5 prefix,suffix = No Matching Prefix
```



## Tcl


Since [http://www.tcl.tk/cgi-bin/tct/tip/195.html TIP#195] this has been present as a core command:


```Tcl
% namespace import ::tcl::prefix
% prefix longest {interstellar interspecies interstate integer} ""
inte

```



## zkl

The string method prefix returns the number of common prefix characters.

```zkl
fcn lcp(s,strings){ s[0,s.prefix(vm.pasteArgs(1))] }
```

Or, without using prefix:
```zkl
fcn lcp(strings){
   vm.arglist.reduce(fcn(prefix,s){ Utils.Helpers.zipW(prefix,s) // lazy zip
      .pump(String,fcn([(a,b)]){ a==b and a or Void.Stop })
   })
}
```


```zkl
tester:=TheVault.Test.UnitTester.UnitTester();
tester.testRun(lcp.fp("interspecies","interstellar","interstate"),Void,"inters",__LINE__);
tester.testRun(lcp.fp("throne","throne"),Void,"throne",__LINE__);
tester.testRun(lcp.fp("throne","dungeon"),Void,"",__LINE__);
tester.testRun(lcp.fp("cheese"),Void,"cheese",__LINE__);
tester.testRun(lcp.fp(""),Void,"",__LINE__);
tester.testRun(lcp.fp("prefix","suffix"),Void,"",__LINE__);
tester.stats();
```

The fp (partial application) method is used to delay running lcp until the tester actually tests.
```txt


### ================== Unit Test 1 ==================

Test 1 passed!

### ================== Unit Test 2 ==================

Test 2 passed!

### ================== Unit Test 3 ==================

Test 3 passed!

### ================== Unit Test 4 ==================

Test 4 passed!

### ================== Unit Test 5 ==================

Test 5 passed!

### ================== Unit Test 6 ==================

Test 6 passed!
6 tests completed.
Passed test(s): 6 (of 6)

```

