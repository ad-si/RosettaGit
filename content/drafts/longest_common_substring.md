+++
title = "Longest Common Substring"
description = ""
date = 2019-10-18T02:20:13Z
aliases = []
[extra]
id = 18731
[taxonomies]
categories = []
tags = []
+++

{{task}}
Write a function that returns the longest common substring of two strings. Use it within a program that demonstrates sample output from the function, which will consist of the longest common substring between "thisisatest" and "testing123testing". Note that substrings are consecutive characters within a string. This distinguishes them from subsequences, which is any sequence of characters within a string, even if there are extraneous characters in between them. Hence, the [[longest common subsequence]] between "thisisatest" and "testing123testing" is "tsitest", whereas the longest common sub''string'' is just "test".


{{Template:Strings}}


;References:
*[http://en.wikipedia.org/wiki/Generalized_suffix_tree Generalize Suffix Tree]
*[[Ukkonen’s Suffix Tree Construction]]





## Aime


```aime
void
test_string(text &g, text v, text l)
{
    integer n;

    n = 0;
    while (l[n] && v[n] == l[n]) {
        n += 1;
    }
    if (length(g) < n) {
        g = cut(l, 0, n);
    }
}

text
longest(text u, text v)
{
    record r;
    text g, l, s;

    while (length(u)) {
        r[u] = 0;
        u = delete(u, 0);
    }
    while (length(v)) {
        if (rsk_lower(r, v, l)) {
            test_string(g, v, l);
        }
        if (rsk_upper(r, v, l)) {
            test_string(g, v, l);
        }
        v = delete(v, 0);
    }

    return g;
}
```


```aime
o_(longest("thisisatest", "testing123testing"), "\n");
```



## AppleScript


```AppleScript
on LCS(a as text, b as text)
    local t1, t2
 
    script
        property s : substrings(a)
        property t : substrings(b)
        property list : missing value
 
        on longest from L at w : ""
            if length of L = 1 then return w
 
            tell item 1 of L to if ¬
                w's length < its length ¬
                then set w to it
 
            longest from the rest of L at w
        end longest
    end script
 
    tell the result
        repeat with x in (a reference to its s)
            if x is not in its t then set the contents of x to null
        end repeat
 
        set its list to every string in its s
 
        longest from its list
    end tell
end LCS
 
 
on substrings(t)
    local t
 
    script
        property result : {}
 
        to iterate thru s at n : 1
            local s, n
 
            if the length of s < n then return {}
            set my result to my result & (text 1 thru n of s)
            iterate thru s at n + 1
        end iterate
 
        to recurse thru s
            local s
 
            if length of s = 1 then return s
            iterate thru s
            my result & substrings(text 2 thru -1 of s)
        end recurse
    end script
 
    tell the result to recurse thru t
end substrings
```



```AppleScript
LCS("thisisatest", "testing123testing")
```


{{out}}

```txt
"test"
```



## AutoHotkey


### Using Text Comparison


```AutoHotkey
LCS(a, b){
	x := i := 1
	while StrLen(x)
		Loop % StrLen(a)
			IfInString, b, % x := SubStr(a, i:=StrLen(x)=1 ? i+1 : i, n:=StrLen(a)+1-A_Index)
				res := StrLen(res) > StrLen(x) ? res : x
	return res
}
```

Examples:
```AutoHotkey
MsgBox % LCS("thisisatest", "testing123testing")
```

Outputs:
```txt
test
```


### Using RegEx


```AutoHotkey
LCS(a, b){
	while pos := RegExMatch(a "`n" b, "(.+)(?=.*\R.*\1)", m, pos?pos+StrLen(m):1)
		res := StrLen(res) > StrLen(m1) ? res : m1
	return res
}
```

Examples:
```AutoHotkey
MsgBox % LCS("thisisatest", "testing123testing")
```

Outputs:
```txt
test
```





## C

{{trans|Modula-2}}

```c
#include <stdio.h>

void lcs(const char * const sa, const char * const sb, char ** const beg, char ** const end) {
    size_t apos, bpos;
    ptrdiff_t len;

    *beg = 0;
    *end = 0;
    len = 0;

    for (apos = 0; sa[apos] != 0; ++apos) {
        for (bpos = 0; sb[bpos] != 0; ++bpos) {
            if (sa[apos] == sb[bpos]) {
                len = 1;
                while (sa[apos + len] != 0 && sb[bpos + len] != 0 && sa[apos + len] == sb[bpos + len]) {
                    len++;
                }
            }

            if (len > *end - *beg) {
                *beg = sa + apos;
                *end = *beg + len;
                len = 0;
            }
        }
    }
}

int main() {
    char *s1 = "thisisatest";
    char *s2 = "testing123testing";
    char *beg, *end, *it;

    lcs(s1, s2, &beg, &end);

    for (it = beg; it != end; it++) {
        putchar(*it);
    }
    printf("\n");

    return 0;
}
```

{{out}}

```txt
test
```



## C++


```cpp
#include <string>
#include <algorithm>
#include <iostream>
#include <set>
#include <vector>

void findSubstrings ( const std::string & word , std::set<std::string> & substrings ) {
   int l = word.length( ) ;
   for ( int start = 0 ; start < l ; start++ ) {
      for ( int length = 1 ; length < l - start + 1 ; length++ ) {
	 substrings.insert ( word.substr( start , length ) ) ;
      }
   }
}

std::string lcs ( const std::string & first , const std::string & second ) {
   std::set<std::string> firstSubstrings , secondSubstrings ;
   findSubstrings ( first , firstSubstrings ) ;
   findSubstrings ( second , secondSubstrings ) ;
   std::set<std::string> common ;
   std::set_intersection ( firstSubstrings.begin( ) , firstSubstrings.end( ) ,
	 secondSubstrings.begin( ) , secondSubstrings.end( ) ,
	 std::inserter ( common , common.begin( ) ) ) ;
   std::vector<std::string> commonSubs ( common.begin( ) , common.end( ) ) ;
   std::sort ( commonSubs.begin( ) , commonSubs.end( ) , []( const std::string &s1 ,
	    const std::string &s2 ) { return s1.length( ) > s2.length( ) ; } ) ;
   return *(commonSubs.begin( ) ) ;
}

int main( ) {
   std::string s1 ("thisisatest" ) ;
   std::string s2 ( "testing123testing" ) ;
   std::cout << "The longest common substring of " << s1 << " and " << s2 << " is:\n" ;
   std::cout << lcs ( s1 , s2 ) << " !\n" ;
   return 0 ;
}
```

{{out}}

```txt
The longest common substring of thisisatest and testing123testing is:
test !
```


=={{header|C sharp|C#}}==

### Using dynamic programming


```csharp
using System;

namespace LongestCommonSubstring
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine(lcs("thisisatest", "testing123testing"));
            Console.ReadKey(true);
        }

        public static string lcs(string a, string b)
        {
            var lengths = new int[a.Length, b.Length];
            int greatestLength = 0;
            string output = "";
            for (int i = 0; i < a.Length; i++)
            {
                for (int j = 0; j < b.Length; j++)
                {
                    if (a[i] == b[j])
                    {
                        lengths[i, j] = i == 0 || j == 0 ? 1 : lengths[i - 1, j - 1] + 1;
                        if (lengths[i, j] > greatestLength)
                        {
                            greatestLength = lengths[i, j];
                            output = a.Substring(i - greatestLength + 1, greatestLength);
                        }
                    }
                    else
                    {
                        lengths[i, j] = 0;
                    }
                }
            }
            return output;
        }
    }
}
```

{{out}}

```txt

test

```



### Searching for smaller substrings of a in b

{{trans|REXX}}

```csharp
//C# program tests the LCSUBSTR (Longest Common Substring) subroutine.
using System;
namespace LongestCommonSubstring
{
    class Program
    {
        static void Main(string[] args)
        {
            string a = args.Length >= 1 ? args[0] : "";                                             /*get two arguments (strings).   */
            string b = args.Length == 2 ? args[1] : "";
            if (a == "") a = "thisisatest";                                                         /*use this string for a default. */
            if (b == "") b = "testing123testing";                                                   /* "    "     "    "  "    "     */
            Console.WriteLine("string A = {0}", a);                                                 /*echo string  A  to screen.     */
            Console.WriteLine("string B = {0}", b);                                                 /*echo string  B  to screen.     */
            Console.WriteLine("LCsubstr = {0}", LCsubstr(a, b));                                    /*tell Longest Common Substring. */
            Console.ReadKey(true);
        }                                                                                           /*stick a fork in it, we're done.*/

        /*─────────────────────────────────LCSUBSTR subroutine─────────────────────────────────*/
        public static string LCsubstr(string x, string y)                                           /*Longest Common Substring.      */
        {
            string output = "";
            int lenx = x.Length;                                                                    /*shortcut for using the X length*/
            for (int j = 0; j < lenx; j++)                                                          /*step through start points in X.*/
            {
                for (int k = lenx - j; k > -1; k--)                                                 /*step through string lengths.   */
                {
                    string common = x.Substring(j, k);                                              /*extract a common substring.    */
                    if (y.IndexOf(common) > -1 && common.Length > output.Length) output = common;   /*longest?*/
                }                                                                                   /*k*/
            }                                                                                       /*j*/
            return output;                                                                          /*$  is "" if no common string.  */
        }
    }
}
```

'''output''' when using the default inputs:

```txt

   string A = thisisatest
   string B = testing123testing
   LCsubstr = test

```


===Searching for smaller substrings of a in b (simplified)===
{{trans|zkl}}

```csharp
//C# program tests the LCS (Longest Common Substring) subroutine.
using System;
namespace LongestCommonSubstring
{
    class Program
    {
        static void Main(string[] args)
        {
            string a = args.Length >= 1 ? args[0] : "";                                             /*get two arguments (strings).   */
            string b = args.Length == 2 ? args[1] : "";
            if (a == "") a = "thisisatest";                                                         /*use this string for a default. */
            if (b == "") b = "testing123testing";                                                   /* "    "     "    "  "    "     */
            Console.WriteLine("string A = {0}", a);                                                 /*echo string  A  to screen.     */
            Console.WriteLine("string B = {0}", b);                                                 /*echo string  B  to screen.     */
            Console.WriteLine("LCS = {0}", lcs(a, b));                                              /*tell Longest Common Substring. */
            Console.ReadKey(true);
        }                                                                                           /*stick a fork in it, we're done.*/

        /*─────────────────────────────────LCS subroutine─────────────────────────────────*/
        private static string lcs(string a, string b)
        {
           if(b.Length<a.Length){ string t=a; a=b; b=t; }
           for (int n = a.Length; n > 0; n--)
           {
              for (int m = a.Length-n; m <= a.Length-n; m++)
              {
                  string s=a.Substring(m,n);
                  if(b.Contains(s)) return(s);
              }
           }
           return "";
        }
    }

```

'''output''' when using the default inputs:

```txt

   string A = thisisatest
   string B = testing123testing
   LCS = test

```



## D

{{trans|C#}}

```d
import std.stdio;

string lcs(string a, string b) {
    int[][] lengths;
    lengths.length = a.length;
    for (int i=0; i<a.length; i++) {
        lengths[i].length = b.length;
    }

    int greatestLength;
    string output;
    for (int i=0; i<a.length; i++) {
        for (int j=0; j<b.length; j++) {
            if (a[i]==b[j]) {
                lengths[i][j] = i==0 || j==0 ? 1 : lengths[i-1][j-1] + 1;
                if (lengths[i][j] > greatestLength) {
                    greatestLength = lengths[i][j];
                    int start = i-greatestLength+1;
                    output = a[start..start+greatestLength];
                }
            } else {
                lengths[i][j] = 0;
            }
        }
    }
    return output;
}

void main() {
    writeln(lcs("testing123testing", "thisisatest"));
}
```

{{out}}

```txt
test
```



## Dyalect


{{trans|Swift}}


```dyalect
func lComSubStr(w1, w2) {
    var (len, end) = (0, 0)
    var mat  = Array.empty(w1.len() + 1, () => Array.empty(w2.len() + 1, 0))
    var (i, j) = (0, 0)

    for sLett in w1 {
      for tLett in w2 {
        if tLett == sLett {
            const curLen = mat[i][j] + 1
            mat[i + 1][j + 1] = curLen
            if curLen > len {
            len = curLen
            end = i
            }
        }
        j += 1
      }
      j = 0
      i += 1
    }
    String(values: w1).sub((end + 1) - len, len)
}

func comSubStr(w1, w2) {
  return String(lComSubStr(w1.iter().toArray(), w2.iter().toArray()))
}

comSubStr("thisisatest", "testing123testing") // "test"
```



## Elixir

{{works with|Elixir|1.3}}

```elixir
defmodule LCS do
  def longest_common_substring(a,b) do
    alist = to_charlist(a) |> Enum.with_index
    blist = to_charlist(b) |> Enum.with_index
    lengths = for i <- 0..length(alist)-1, j <- 0..length(blist), into: %{}, do: {{i,j},0}
    Enum.reduce(alist, {lengths,0,""}, fn {x,i},acc ->
      Enum.reduce(blist, acc, fn {y,j},{map,gleng,lcs} ->
        if x==y do
          len = if i==0 or j==0, do: 1, else: map[{i-1,j-1}]+1
          map = %{map | {i,j} => len}
          if len > gleng, do: {map, len, String.slice(a, i - len + 1, len)},
                        else: {map, gleng, lcs}
        else
          {map, gleng, lcs}
        end
      end)
    end)
    |> elem(2)
  end
end

IO.puts LCS.longest_common_substring("thisisatest", "testing123testing")
```


{{out}}

```txt

test

```



## Go

{{trans|C#}}

```go
package main

import "fmt"

func lcs(a, b string) (output string) {
    lengths := make([]int, len(a)*len(b))
    greatestLength := 0
    for i, x := range a {
        for j, y := range b {
            if x == y {
                if i == 0 || j == 0 {
                    lengths[i*len(b)+j] = 1
                } else {
                    lengths[i*len(b)+j] = lengths[(i-1)*len(b)+j-1] + 1
                }
                if lengths[i*len(b)+j] > greatestLength {
                    greatestLength = lengths[i*len(b)+j]
                    output = a[i-greatestLength+1 : i+1]
                }
            }
        }
    }
    return
}

func main() {
    fmt.Println(lcs("thisisatest", "testing123testing"))
}
```

{{out}}

```txt

test

```



## Haskell


```Haskell
import Data.Ord (comparing)
import Data.List (maximumBy, intersect)

subStrings :: [a] -> [[a]]
subStrings s =
  let intChars = length s
  in [ take n $ drop i s
     | i <- [0 .. intChars - 1]
     , n <- [1 .. intChars - i] ]

longestCommon :: Eq a => [a] -> [a] -> [a]
longestCommon a b =
  maximumBy (comparing length) (subStrings a `intersect` subStrings b)

main :: IO ()
main = putStrLn $ longestCommon "testing123testing" "thisisatest"
```

{{out}}

```txt
test
```


Or, fusing subStrings as ''tail . inits <=< tails''


```haskell
import Data.Ord (comparing)
import Control.Monad ((<=<))
import Data.List (inits, intersect, maximumBy, tails)

longestCommon :: Eq a => [a] -> [a] -> [a]
longestCommon a b =
  maximumBy (comparing length) $
  (uncurry intersect . pair) $ [tail . inits <=< tails] <*> [a, b]

pair :: [a] -> (a, a)
pair [x, y] = (x, y)

main :: IO ()
main = putStrLn $ longestCommon "testing123testing" "thisisatest"
```

{{Out}}

```txt
test
```



## J


This algorithm starts by comparing each character in the one string to each character in the other, and then iterates on this result until it finds the length of the longest common substring. So if Lx is the length of one argument string, Ly is the length of the other argument string, and Lr is the  length of the result string, this algorithm uses space on the order of Lx*Ly and time on the order of Lx*Ly*Lr.

In other words: this can be suitable for small problems, but you might want something better if you're comparing gigabyte length strings with high commonality.


```J
lcstr=:4 :0
  C=. ({.~ 1+$) x=/y
  M=. >./ (* * * >. * + (_1&|.)@:|:^:2)^:_  C
  N=. >./ M
  y {~ (M i. N)-i.-N
)
```


Intermedate results:

    C shows which characters are in common between the two strings.
    M marks the length of the longest common substring ending at each position in the right argument
    N is the length of the longest common substring

Example use:


```J
   'thisisatest' lcs 'testing123testing'
test
```



## JavaScript

{{Trans|Haskell}}

```javascript
(() => {
    'use strict';

    // longestCommon :: String -> String -> String
    const longestCommon = (s1, s2) => maximumBy(
        comparing(length),
        intersect(...apList(
            [s => map(
                concat,
                concatMap(tails, compose(tail, inits)(s))
            )],
            [s1, s2]
        ))
    );

    // main :: IO ()
    const main = () =>
        console.log(
            longestCommon(
                "testing123testing",
                "thisisatest"
            )
        );

    // GENERIC FUNCTIONS ----------------------------

    // Each member of a list of functions applied to each
    // of a list of arguments, deriving a list of new values.

    // apList (<*>) :: [(a -> b)] -> [a] -> [b]
    const apList = (fs, xs) => //
        fs.reduce((a, f) => a.concat(
            xs.reduce((a, x) => a.concat([f(x)]), [])
        ), []);

    // comparing :: (a -> b) -> (a -> a -> Ordering)
    const comparing = f =>
        (x, y) => {
            const
                a = f(x),
                b = f(y);
            return a < b ? -1 : (a > b ? 1 : 0);
        };

    // compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
    const compose = (f, g) => x => f(g(x));

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) =>
        xs.reduce((a, x) => a.concat(f(x)), []);

    // inits([1, 2, 3]) -> [[], [1], [1, 2], [1, 2, 3]
    // inits('abc') -> ["", "a", "ab", "abc"]

    // inits :: [a] -> [[a]]
    // inits :: String -> [String]
    const inits = xs => [
            []
        ]
        .concat(('string' === typeof xs ? xs.split('') : xs)
            .map((_, i, lst) => lst.slice(0, i + 1)));

    // intersect :: (Eq a) => [a] -> [a] -> [a]
    const intersect = (xs, ys) =>
        xs.filter(x => -1 !== ys.indexOf(x));

    // Returns Infinity over objects without finite length.
    // This enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // maximumBy :: (a -> a -> Ordering) -> [a] -> a
    const maximumBy = (f, xs) =>
        0 < xs.length ? (
            xs.slice(1)
            .reduce((a, x) => 0 < f(x, a) ? x : a, xs[0])
        ) : undefined;

    // tail :: [a] -> [a]
    const tail = xs => 0 < xs.length ? xs.slice(1) : [];

    // tails :: [a] -> [[a]]
    const tails = xs => {
        const
            es = ('string' === typeof xs) ? (
                xs.split('')
            ) : xs;
        return es.map((_, i) => es.slice(i))
            .concat([
                []
            ]);
    };

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
test
```



## Java


```java
public class LongestCommonSubstring {

    public static void main(String[] args) {
        System.out.println(lcs("testing123testing", "thisisatest"));
    }

    static String lcs(String a, String b) {
        if (a.length() > b.length())
            return lcs(b, a);

        String res = "";
        for (int ai = 0; ai < a.length(); ai++) {
            for (int len = a.length() - ai; len > 0; len--) {

                for (int bi = 0; bi < b.length() - len; bi++) {

                    if (a.regionMatches(ai, b, bi, len) && len > res.length()) {
                        res = a.substring(ai, ai + len);
                    }
                }
            }
        }
        return res;
    }
}
```



```txt
test
```



## jq

{{trans|C#, Go, Ruby}}
{{works with|jq|1.4}}

'''Utility functions''':

```jq
# Create an m x n matrix
def matrix(m; n; init):
  if m == 0 then []
  elif m == 1 then [range(0;n) | init]
  elif m > 0 then
    matrix(1;n;init) as $row
    | [range(0;m) | $row ]
  else error("matrix\(m);_;_) invalid")
  end;

def set(i;j; value):
  setpath([i,j]; value);
```


'''Longest Common Substring''':

```jq
def lcs(a; b):
  matrix(a|length; b|length; 0) as $lengths
  # state: [ $lengths, greatestLength, answer ]
  | [$lengths, 0]
  | reduce range(0; a|length) as $i
      (.;
       reduce range(0; b|length) as $j
         (.;
           if a[$i:$i+1] == b[$j:$j+1] then
            (if $i == 0 or $j == 0 then 1
             else .[0][$i-1][$j-1] + 1
 	     end) as $x
            | .[0] |= set($i; $j; $x)
            | if $x > .[1] then
                .[1] = $x
                | .[2] = a[1+$i - $x : 1+$i] # output
              else .
              end
          else .
          end )) | .[2];
```

'''Example''':

```jq
lcs("thisisatest"; "testing123testing")
```

{{out}}

```sh
$ jq -n -f Longest_common_substring.jq
"test"
```



## Julia

{{works with|Julia|0.6}}


```julia
function lcs(s1::AbstractString, s2::AbstractString)
    r = ""
    i = 1
    for i in 1:length(s1)
        j = i
        while j ≤ length(s1) && contains(s2, s1[i:j])
            if length(r) < j - i + 1 r = s1[i:j] end
            j += 1
        end
    end
    return r
end

@show lcs("thisisatest", "testing123testing")
```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.2

fun lcs(a: String, b: String): String {
    if (a.length > b.length) return lcs(b, a)
    var res = ""
    for (ai in 0 until a.length) {
        for (len in a.length - ai downTo 1) {
            for (bi in 0 until b.length - len) {
                if (a.regionMatches(ai, b, bi,len) && len > res.length) {
                    res = a.substring(ai, ai + len)
                }
            }
        }
    }
    return res
}

fun main(args: Array<String>) = println(lcs("testing123testing", "thisisatest"))
```


{{out}}

```txt

test

```



## Maple

<code>StringTools:-LongestCommonSubString()</code> returns the longest common substring of two strings.
<code>StringTools:-CommonSubSequence()</code> returns the longest common subsequence() of two strings.

```Maple
StringTools:-LongestCommonSubString("thisisatest","testing123testing");
```




## Mathematica

The function <code>LongestCommonSubsequence</code> returns the longest common substring, and <code>LongestCommonSequence</code> returns the longest common subsequence.

```Mathematica
Print[LongestCommonSubsequence["thisisatest", "testing123testing"]];
```

{{out}}

```txt
test
```


=={{header|Modula-2}}==

```Modula2
MODULE LCS;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,Write,ReadChar;

PROCEDURE WriteSubstring(s : ARRAY OF CHAR; b,e : CARDINAL);
VAR i : CARDINAL;
BEGIN
    IF b=e THEN RETURN END;
    IF e>HIGH(s) THEN e := HIGH(s) END;

    FOR i:=b TO e DO
        Write(s[i])
    END
END WriteSubstring;

TYPE
    Pair = RECORD
        a,b : CARDINAL;
    END;

PROCEDURE lcs(sa,sb : ARRAY OF CHAR) : Pair;
VAR
    output : Pair;
    a,b,len : CARDINAL;
BEGIN
    output := Pair{0,0};

    FOR a:=0 TO HIGH(sa) DO
        FOR b:=0 TO HIGH(sb) DO
            IF (sa[a]#0C) AND (sb[b]#0C) AND (sa[a]=sb[b]) THEN
                len := 1;
                WHILE (a+len<HIGH(sa)) AND (b+len<HIGH(sb)) DO
                    IF sa[a+len] = sb[b+len] THEN
                        INC(len)
                    ELSE
                        BREAK
                    END
                END;
                DEC(len);

                IF len>output.b-output.a THEN
                    output := Pair{a,a+len}
                END
            END
        END
    END;

    RETURN output
END lcs;

VAR res : Pair;
BEGIN
    res := lcs("testing123testing", "thisisatest");
    WriteSubstring("testing123testing", res.a, res.b);
    WriteLn;

    ReadChar
END LCS.
```



## Perl


```Perl
#!/usr/bin/perl
use strict ;
use warnings ;

sub longestCommonSubstr {
   my $first = shift ;
   my $second = shift ;
   my %firstsubs = findSubstrings ( $first );
   my %secondsubs = findSubstrings ( $second ) ;
   my @commonsubs ;
   foreach my $subst ( keys %firstsubs ) {
      if ( exists $secondsubs{ $subst } ) {
	 push ( @commonsubs , $subst ) ;
      }
   }
   my @sorted = sort { length $b <=> length $a } @commonsubs ;
   return $sorted[0] ;
}

sub findSubstrings {
   my $string = shift ;
   my %substrings ;
   my $l = length $string ;
   for ( my $start = 0 ; $start < $l ; $start++ ) {
      for ( my $howmany = 1 ; $howmany < $l - $start + 1 ; $howmany++) {
	 $substrings{substr( $string , $start , $howmany) } = 1 ;
      }
   }
   return %substrings ;
}

my $longest = longestCommonSubstr( "thisisatest" ,"testing123testing" ) ;
print "The longest common substring of <thisisatest> and <testing123testing> is $longest !\n" ;

```

{{out}}

```txt
The longest common substring of <thisisatest> and <testing123testing> is test !
```



## Perl 6


```perl6

sub createSubstrings( Str $word --> Array ) {
   my $length = $word.chars ;
   my @substrings ;
   for (0..$length - 1) -> $start {
      for (1..$length - $start) -> $howmany {
	 @substrings.push( $word.substr( $start , $howmany ) ) ;
      }
   }
   return @substrings ;
}

sub findLongestCommon( Str $first , Str $second --> Str ) {
   my @substringsFirst = createSubstrings( $first ) ;
   my @substringsSecond = createSubstrings( $second ) ;
   my $firstset = set( @substringsFirst ) ;
   my $secondset = set( @substringsSecond ) ;
   my $common = $firstset (&) $secondset ;
   return $common.keys.sort({$^b.chars <=> $^a.chars})[0] ;
}

sub MAIN( Str $first , Str $second ) {
   my $phrase = "The longest common substring of $first and $second is " ~
   "{findLongestCommon( $first , $second ) } !" ;
   $phrase.say ;
}
```

{{Out}}

```txt
The longest common substring of thisisatest and testing123testing is test !
```



## Phix


```Phix
function lcs(string a, b)
integer longest = 0
string best = ""
    for i=1 to length(a) do
        integer ch = a[i]
        for j=1 to length(b) do
            if ch=b[j] then
                integer n=1
                while i+n<=length(a)
                  and j+n<=length(b)
                  and a[i+n]=b[j+n] do
                    n += 1
                end while
                if n>longest then
                    longest = n
                    best = a[i..i+n-1]
                end if
            end if
        end for
    end for
    return best
end function
?lcs("thisisatest", "testing123testing")
?lcs("testing123testing","thisisatest")
```

{{out}}

```txt

"test"
"test"

```



## PicoLisp


```PicoLisp
(de longestCommonSubstring (Str1 Str2)
   (setq Str1 (chop Str1)  Str2 (chop Str2))
   (let Res NIL
      (map
         '((Lst1)
            (map
               '((Lst2)
                  (let Len 0
                     (find
                        '((A B) (nand (= A B) (inc 'Len)))
                        Lst1
                        Lst2 )
                     (when (> Len (length Res))
                        (setq Res (head Len Lst1)) ) ) )
               Str2 ) )
         Str1 )
      (pack Res) ) )
```

Test:

```PicoLisp
: (longestCommonSubstring "thisisatest" "testing123testing")
-> "test"
```



## Prolog



```Prolog
common_sublist(A, B, M) :-
	append(_, Ma, A),
	append(M, _, Ma),
	append(_, Mb, B),
	append(M, _, Mb).

longest_list([], L, _, L).
longest_list([L|Ls], LongestList, LongestLength, Result) :-
	length(L, Len),
	Len >= LongestLength -> longest_list(Ls, L, Len, Result)
	; longest_list(Ls, LongestList, LongestLength, Result).

longest_substring(A, B, Result) :-
	string_chars(A, AChars),
	string_chars(B, BChars),
	findall(SubString, (
		dif(SubString, []), common_sublist(AChars, BChars, SubString)
	), AllSubstrings),
	longest_list(AllSubstrings, [], 0, LongestSubString),
	string_chars(Result, LongestSubString).
```

{{out}}

```txt

?- longest_substring("thisisatest", "testing123testing", Longest).
Longest = "test".

```



## Python



### Using Indexes


```python
s1 = "thisisatest"
s2 = "testing123testing"
len1, len2 = len(s1), len(s2)
ir, jr = 0, 0
for i1 in range(len1):
    i2 = s2.find(s1[i1])
    while i2 >= 0:
        j1, j2 = i1+1, i2+1
        while j1 < len1 and j2 < len2 and s2[j2] == s1[j1]:
            if j1-i1 > jr-ir:
                ir, jr = i1, j1
            j1 += 1; j2 += 1
        i2 = s2.find(s1[i1], i2+1)
print (s1[ir:jr+1])
```

{{out}}

```txt
"test"
```



### Functional

{{Trans|Haskell}}
{{Trans|JavaScript}}


Expressed as a composition of generic functions:

```python
from itertools import (accumulate, chain)
from functools import (reduce)


# longestCommon :: String -> String -> String
def longestCommon(s1):
    return lambda s2: max(intersect(
        *map(lambda s: map(
            concat,
            concatMap(tails)(
                compose(tail)(inits)(s)
            )
        ), [s1, s2])
    ), key=len)


# TEST ----------------------------------------------------
def main():
    print(
        longestCommon("testing123testing")(
            "thisisatest"
        )
    )


# GENERIC -------------------------------------------------


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    return lambda f: lambda x: g(f(x))


# concat :: [String] -> String
def concat(xs):
    return ''.join(chain.from_iterable(xs))


# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    return lambda xs: list(
        chain.from_iterable(
            map(f, xs)
        )
    )


# inits :: [a] -> [[a]]
def inits(xs):
    return scanl(lambda a, x: a + [x])(
        []
    )(list(xs))


# intersect :: [a] -> [a] -> [a]
def intersect(xs, ys):
    s = set(ys)
    return [x for x in xs if x in s]


# map :: (a -> b) -> [a] -> [b]
def map_(f):
    return lambda xs: list(map(f, xs))


# scanl is like reduce, but returns a succession of
# intermediate values, building from the left.
# scanl :: (b -> a -> b) -> b -> [a] -> [b]


def scanl(f):
    return lambda a: lambda xs: (
        list(accumulate([a] + list(xs), f))
    )


# tail :: [a] -> [a]
def tail(xs):
    return xs[1:]


# tails :: [a] -> [[a]]
def tails(xs):
    return list(map(
        lambda i: xs[i:],
        range(0, 1 + len(xs))
    ))


# MAIN ---
main()
```


```txt
test
```



## Racket


A chance to show off how to use <code>HashTable</code> types in <i>typed/racket</i>


```racket
#lang typed/racket
(: lcs (String String -> String))
(define (lcs a b)
  (: all-substrings# (String -> (HashTable String Boolean)))
  (define (all-substrings# str)
    (define l (string-length str))
    (for*/hash : (HashTable String Boolean)
      ((s (in-range 0 l)) (e (in-range (add1 s) (add1 l))))
      (values (substring str s e) #t)))

  (define a# (all-substrings# a))

  (define b# (all-substrings# b))

  (define-values (s l)
    (for/fold : (Values String Nonnegative-Integer)
    ((s "") (l : Nonnegative-Integer 0))
    ((a_ (in-hash-keys a#))
     #:when (and (> (string-length a_) l) (hash-ref b# a_ #f)))
    (values a_ (string-length a_))))

  s)

(module+ test
  ("thisisatest" . lcs . "testing123testing"))
```


{{out}}


```txt
"test"
```



## REXX


```rexx
/*REXX program determines the   LCSUBSTR   (Longest Common Substring)  via a function.  */
parse arg a b .                                  /*obtain optional arguments from the CL*/
if a==''  then a= "thisisatest"                  /*Not specified?  Then use the default.*/
if b==''  then b= "testing123testing"            /* "      "         "   "     "    "   */
say '   string A ='    a                         /*echo string A to the terminal screen.*/
say '   string B ='    b                         /*  "     "   B  "  "      "       "   */
say '   LCsubstr ='   LCsubstr(a, b)             /*display the Longest Common Substring.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
LCsubstr: procedure;  parse arg x,y,,$;     #=0  /*LCsubstr:  Longest Common Substring. */
          L= length(x);     w= length(y)         /*placeholders for string length of X,Y*/
          if w<L  then do;  parse arg y,x;  L=w  /*switch X & Y   if Y is shorter than X*/
                       end
             do j=1  for L  while j<=L-#         /*step through start points in string X*/
                do k=L-j+1   to #   by -1        /*step through string lengths.         */
                _= substr(x, j, k)               /*extract a possible common substring. */
                if pos(_, y)\==0  then  if k>#  then do;    $=_;    #=k;     end
                end   /*k*/                      /* [↑]  determine if string _ is longer*/
             end      /*j*/                      /*#:  the current length of  $  string.*/
          return $                               /*$:  (null if there isn't common str.)*/
```

{{out|output|text=  when using the default inputs:}}

```txt

   string A = thisisatest
   string B = testing123testing
   LCsubstr = test

```



## Ring


```ring

# Project : Longest Common Substring

str1 = "testing123testing"
str2 = "tsitest"
see longest(str1, str2)

func  longest(str1, str2)
subarr = []
for n=1 to len(str1)
    for m=1 to len(str1)
        sub = substr(str1, n, m)
        if substr(str2, sub) > 0
           add(subarr, sub)
        ok
    next
next

temp = 0
for n=1 to len(subarr)
    if len(subarr[n]) > temp
       temp = len(subarr[n])
       subend = subarr[n]
    ok
next
see subend + nl

```

Output:

```txt

test

```



## Ruby

{{trans|C#}}

```ruby
def longest_common_substring(a,b)
  lengths = Array.new(a.length){Array.new(b.length, 0)}
  greatestLength = 0
  output = ""
  a.each_char.with_index do |x,i|
    b.each_char.with_index do |y,j|
      next if x != y
      lengths[i][j] = (i.zero? || j.zero?) ? 1 : lengths[i-1][j-1] + 1
      if lengths[i][j] > greatestLength
        greatestLength = lengths[i][j]
        output = a[i - greatestLength + 1, greatestLength]
      end
    end
  end
  output
end

p longest_common_substring("thisisatest", "testing123testing")
```


{{out}}

```txt

"test"

```



## Scala


### Dynamic Programming

====Functional Prog, (tail) recursive====
{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/pJTYkcr/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/bRp8rmyjQvyoB99mhrXOcw Scastie (remote JVM)].

```Scala
import scala.annotation.tailrec

object LongestCommonSubstring extends App {

  def longestCommonSubstring(s: String, t: String): Seq[String] = {
    val p = (s.length, t.length)
    val nonEmpty = s.nonEmpty && t.nonEmpty

    @tailrec
    def iter(lcSufx: Map[(Int, Int), Int], indexes: (Int, Int), z: Int): Map[(Int, Int), Int] = {
      val (i, j) = indexes

      def newIndexes: (Int, Int) = if (j == p._2) (i + 1, 1) else (i, j + 1)

      if (indexes != p && nonEmpty)
        if (s(i - 1) == t(j - 1)) {
          val count = lcSufx.withDefaultValue(0)((i - 1, j - 1)) + 1

          @inline
          def newLcSufx = lcSufx.filter(_._2 >= z).updated(indexes, count)

          iter(newLcSufx, newIndexes, if (count >= z) count else z)
        } else iter(lcSufx, newIndexes, z)
      else lcSufx.filter(_._2 > 1)
    }

    iter(Map.empty[(Int, Int), Int], (1, 1), 0).map {
      case ((i, _), z) => s.substring(i - z, i)
    }.toSeq
  }

  println(longestCommonSubstring("testing123testing", "123thisisatest"))

}
```


## Sidef

{{trans|Perl 6}}

```ruby
func createSubstrings(String word) -> Array {
  gather {
    combinations(word.len+1, 2, {|i,j|
        take(word.substr(i, j-i))
    })
  }
}

func findLongestCommon(String first, String second) -> String {
    createSubstrings(first) & createSubstrings(second) -> max_by { .len }
}

say findLongestCommon("thisisatest", "testing123testing")
```

{{out}}

```txt
test
```



## Swift



```swift
func lComSubStr<
  S0: Sliceable, S1: Sliceable, T: Equatable where
  S0.Generator.Element == T, S1.Generator.Element == T,
  S0.Index.Distance == Int, S1.Index.Distance == Int
  >(w1: S0, _ w2: S1) -> S0.SubSlice {

    var (len, end) = (0, 0)

    let empty = Array(Repeat(count: w2.count + 1, repeatedValue: 0))
    var mat: [[Int]] = Array(Repeat(count: w1.count + 1, repeatedValue: empty))

    for (i, sLett) in w1.enumerate() {
      for (j, tLett) in w2.enumerate() where tLett == sLett {
        let curLen = mat[i][j] + 1
        mat[i + 1][j + 1] = curLen
        if curLen > len {
          len = curLen
          end = i
        }
      }
    }
    return w1[advance(w1.startIndex, (end + 1) - len)...advance(w1.startIndex, end)]
}

func lComSubStr(w1: String, _ w2: String) -> String {
  return String(lComSubStr(w1.characters, w2.characters))
}
```


Output:


```swift
lComSubStr("thisisatest", "testing123testing") // "test"
```



## VBScript


```vb

Function lcs(string1,string2)
	For i = 1 To Len(string1)
		tlcs = tlcs & Mid(string1,i,1)
		If InStr(string2,tlcs) Then
			If Len(tlcs) > Len(lcs) Then
				lcs = tlcs
			End If
		Else
			tlcs = ""
		End If
	Next
End Function

WScript.Echo lcs(WScript.Arguments(0),WScript.Arguments(1))

```


{{Out}}
Invoke the script from a command prompt.

```txt

C:\>cscript.exe /nologo lcs.vbs "thisisatest" "testing123testing"
test

```



## zkl


```zkl
fcn lcd(a,b){
   if(b.len()<a.len()){ t:=a; a=b; b=t; }
   foreach n,m in ([a.len()..1,-1],a.len()-n+1){
      s:=a[m,n];
      if(b.holds(s)) return(s);
   }
   ""
}
```


```zkl
lcd("testing123testing","thisisatest").println();
```

{{out}}
```txt
test
```

