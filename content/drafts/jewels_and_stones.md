+++
title = "Jewels and Stones"
description = ""
date = 2019-08-22T23:07:57Z
aliases = []
[extra]
id = 21809
[taxonomies]
categories = []
tags = []
+++

[[Category:Puzzles]]

{{task}}

;Task:

Create a function which takes two string parameters: 'stones' and 'jewels' and returns an integer.

Both strings can contain any number of upper or lower case letters. However, in the case of 'jewels', all letters must be distinct.

The function should count (and return) how many 'stones' are 'jewels' or, in other words, how many letters in 'stones' are also letters in 'jewels'.


Note that:
:# Only letters in the [https://en.wikipedia.org/wiki/ISO_basic_Latin_alphabet ISO basic Latin alphabet] i.e. 'A to Z' or 'a to z' need be considered. 
:# A lower case letter is considered to be different to its upper case equivalent for this purpose i.e. 'a' != 'A'.
:# The parameters do not need to have exactly the same names.
:# Validating the arguments is unnecessary.  

So, for example, if passed "aAAbbbb" for 'stones' and "aA" for 'jewels', the function should return 3.

This task was inspired by [https://leetcode.com/problems/jewels-and-stones/description/ this problem].





## ALGOL 68


```algol68
BEGIN
    # procedure that counts the number of times the letters in jewels occur in stones #
    PROC count jewels = ( STRING stones, jewels )INT:
         BEGIN
             # count the occurences of each letter in stones #
             INT upper a pos = 0;
             INT lower a pos = 1 + ( ABS "Z" - ABS "A" );
             [ upper a pos : lower a pos + 26 ]INT letter counts;
             FOR c FROM LWB letter counts TO UPB letter counts DO letter counts[ c ] := 0 OD;
             FOR s pos FROM LWB stones TO UPB stones DO
                 CHAR s = stones[ s pos ];
                 IF   s >= "A" AND s <= "Z" THEN letter counts[ upper a pos + ( ABS s - ABS "A" ) ] +:= 1
                 ELIF s >= "a" AND s <= "z" THEN letter counts[ lower a pos + ( ABS s - ABS "a" ) ] +:= 1
                 FI
             OD;
             # sum the counts of the letters that appear in jewels #
             INT count := 0;
             FOR j pos FROM LWB jewels TO UPB jewels DO
                 CHAR j = jewels[ j pos ];
                 IF   j >= "A" AND j <= "Z" THEN count +:= letter counts[ upper a pos + ( ABS j - ABS "A" ) ]
                 ELIF j >= "a" AND j <= "z" THEN count +:= letter counts[ lower a pos + ( ABS j - ABS "a" ) ]
                 FI 
             OD;
             count
         END # count jewels # ;

    print( ( count jewels( "aAAbbbb", "aA" ), newline ) );
    print( ( count jewels( "ABCDEFGHIJKLMNOPQRSTUVWXYZ@abcdefghijklmnopqrstuvwxyz"
                         , "ABCDEFGHIJKLMNOPQRSTUVWXYZ@abcdefghijklmnopqrstuvwxyz"
                         )
           , newline
           )
         );
    print( ( count jewels( "AB", "" ), newline ) );
    print( ( count jewels( "ZZ", "z" ), newline ) )

END
```

{{out}}

```txt
         +3
        +52
         +0
         +0
```



## AppleScript


```applescript
-- jewelCount :: String -> String -> Int
on jewelCount(jewels, stones)
    set js to chars(jewels)
    script
        on |λ|(a, c)
            if elem(c, jewels) then
                a + 1
            else
                a
            end if
        end |λ|
    end script
    foldl(result, 0, chars(stones))
end jewelCount

-- OR in terms of filter
-- jewelCount :: String -> String -> Int
on jewelCount2(jewels, stones)
    script
        on |λ|(c)
            elem(c, jewels)
        end |λ|
    end script
    length of filter(result, stones)
end jewelCount2

-- TEST --------------------------------------------------
on run
    
    unlines(map(uncurry(jewelCount), ¬
        {Tuple("aA", "aAAbbbb"), Tuple("z", "ZZ")}))
    
end run


-- GENERIC FUNCTIONS -------------------------------------

-- Tuple (,) :: a -> b -> (a, b)
on Tuple(a, b)
    {type:"Tuple", |1|:a, |2|:b}
end Tuple

-- chars :: String -> [Char]
on chars(s)
    characters of s
end chars

-- elem :: Eq a => a -> [a] -> Bool
on elem(x, xs)
    considering case
        xs contains x
    end considering
end elem

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

-- Returns a function on a single tuple (containing 2 arguments)
-- derived from an equivalent function with 2 distinct arguments
-- uncurry :: (a -> b -> c) -> ((a, b) -> c)
on uncurry(f)
    script
        property mf : mReturn(f)'s |λ|
        on |λ|(pair)
            mf(|1| of pair, |2| of pair)
        end |λ|
    end script
end uncurry

-- unlines :: [String] -> String
on unlines(xs)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, linefeed}
    set str to xs as text
    set my text item delimiters to dlm
    str
end unlines
```

{{Out}}

```txt
3
0
```



## AutoHotkey


```AutoHotkey
JewelsandStones(ss, jj){
	for each, jewel in StrSplit(jj)
		for each, stone in StrSplit(ss)
			if (stone == jewel)
				num++
	return num
}
```

Example:
```AutoHotkey
MsgBox % JewelsandStones("aAAbbbbz", "aAZ")
return
```

Outputs:
```txt
3
```



## AWK


```AWK
# syntax: GAWK -f JEWELS_AND_STONES.AWK
BEGIN {
    printf("%d\n",count("aAAbbbb","aA"))
    printf("%d\n",count("ZZ","z"))
    exit(0)
}
function count(stone,jewel,  i,total) {
    for (i=1; i<length(stone); i++) {
      if (jewel ~ substr(stone,i,1)) {
        total++
      }
    }
    return(total)
}

```

{{out}}

```txt
3
0
```




## BASIC256


```BASIC256

function contar_joyas(piedras, joyas)
	cont = 0
	for i = 1 to length(piedras)
		bc = instr(joyas, mid(piedras, i, 1), 1)
		if bc <> 0 then cont += 1
	next i
	return cont
end function

print contar_joyas("aAAbbbb", "aA")
print contar_joyas("ZZ", "z")
print contar_joyas("ABCDEFGHIJKLMNOPQRSTUVWXYZ@abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ@abcdefghijklmnopqrstuvwxyz")
print contar_joyas("AB", "")

```

{{out}}

```txt

Igual que la entrada de FreeBASIC.

```




## C

{{trans|Kotlin}}

```c>#include <stdio.h

#include <string.h>

int count_jewels(const char *s, const char *j) {
    int count = 0;
    for ( ; *s; ++s) if (strchr(j, *s)) ++count;
    return count;
}

int main() {
    printf("%d\n", count_jewels("aAAbbbb", "aA"));
    printf("%d\n", count_jewels("ZZ", "z"));
    return 0;
}
```


{{output}}

```txt

3
0

```



## C++

{{trans|D}}

```cpp>#include <algorithm

#include <iostream>

int countJewels(const std::string& s, const std::string& j) {
    int count = 0;
    for (char c : s) {
        if (j.find(c) != std::string::npos) {
            count++;
        }
    }
    return count;
}

int main() {
    using namespace std;

    cout << countJewels("aAAbbbb", "aA") << endl;
    cout << countJewels("ZZ", "z") << endl;

    return 0;
}
```

{{out}}

```txt
3
0
```



## C sharp


```csharp
using System;
using System.Linq;

public class Program
{
    public static void Main() {
        Console.WriteLine(Count("aAAbbbb", "Aa"));
        Console.WriteLine(Count("ZZ", "z"));
    }

    private static int Count(string stones, string jewels) {
        var bag = jewels.ToHashSet();
        return stones.Count(bag.Contains);
    }
}
```

{{out}}

```txt

3
0

```



## D

{{trans|Kotlin}}

```d
import std.algorithm;
import std.stdio;

int countJewels(string s, string j) {
    int count;
    foreach (c; s) {
        if (j.canFind(c)) {
            count++;
        }
    }
    return count;
}

void main() {
    countJewels("aAAbbbb", "aA").writeln;
    countJewels("ZZ", "z").writeln;
}
```

{{out}}

```txt
3
0
```



## Factor


```factor
USING: kernel prettyprint sequences ;
: count-jewels ( stones jewels -- n ) [ member? ] curry count ;

"aAAbbbb" "aA"
"ZZ" "z" [ count-jewels . ] 2bi@
```

{{out}}

```txt

3
0

```




## FreeBASIC


```freebasic

function contar_joyas(piedras as string, joyas as string) as integer
    dim as integer bc, cont: cont = 0
    for i as integer = 1 to len(piedras)
        bc = instr(1, joyas, mid(piedras, i, 1))
        if bc <> 0 then cont += 1
    next i
    contar_joyas = cont
end function

print contar_joyas("aAAbbbb", "aA")
print contar_joyas("ZZ", "z")
print contar_joyas("ABCDEFGHIJKLMNOPQRSTUVWXYZ@abcdefghijklmnopqrstuvwxyz", _
                   "ABCDEFGHIJKLMNOPQRSTUVWXYZ@abcdefghijklmnopqrstuvwxyz")
print contar_joyas("AB", "")

```

{{out}}

```txt

3
0
53
0

```




## Go

Four solutions are shown here.  The first of two simpler solutions iterates over the stone string in an outer loop and makes repeated searches into the jewel string, incrementing a count each time it finds a stone in the jewels.  The second of the simpler solutions reverses that, iterating over the jewel string in the outer loop and accumulating counts of matching stones.  This solution works because we are told that all letters of the jewel string must be unique.  These two solutions are simple but are both O(|j|*|s|).

The two more complex solutions are analogous to the two simpler ones but build a set or multiset as preprocessing step, replacing the inner O(n) operation with an O(1) operation.  The resulting complexity in each case is O(|j|+|s|).

'''Outer loop stones, index into jewels:'''

```go
package main
  
import (
    "fmt"
    "strings"
)

func js(stones, jewels string) (n int) {
    for _, b := range []byte(stones) {
        if strings.IndexByte(jewels, b) >= 0 {
            n++
        }
    }
    return
}

func main() {
    fmt.Println(js("aAAbbbb", "aA"))
}
```

{{out}}

```txt
3
```


'''Outer loop jewels, count stones:'''

```go
func js(stones, jewels string) (n int) {
    for _, b := range []byte(jewels) {
        n += strings.Count(stones, string(b))
    }
    return
}
```


'''Construct jewel set, then loop over stones:'''

```go
func js(stones, jewels string) (n int) {
    var jSet ['z' + 1]int
    for _, b := range []byte(jewels) {
        jSet[b] = 1
    }
    for _, b := range []byte(stones) {
        n += jSet[b]
    }
    return
}
```


'''Construct stone multiset, then loop over jewels:'''

```go
func js(stones, jewels string) (n int) {
    var sset ['z' + 1]int
    for _, b := range []byte(stones) {
        sset[b]++
    }
    for _, b := range []byte(jewels) {
        n += sset[b]
    }
    return
}
```



## Haskell


```haskell>jewelCount :: Eq a =
 [a] -> [a] -> Int 
jewelCount jewels = 
  foldr (\c -> if elem c jewels then succ else id) 0
  

-- TEST ----------------------------------------------

main :: IO ()
main = mapM_ print $ 
  (uncurry jewelCount) <$> [
       ("aA", "aAAbbbb")
      ,("z", "ZZ")
    ]
```

{{Out}}

```txt
3
0
```


Or in terms of filter rather than foldr


```haskell>jewelCount :: Eq a =
 [a] -> [a] -> Int 
jewelCount jewels = 
  length . filter (flip elem jewels)
  
-- Which could be further reduced to
-- jewelCount = (length .) . filter . flip elem

-- TEST ----------------------------------------------

main :: IO ()
main = do
  print $ jewelCount "aA" "aAAbbbb"
  print $ jewelCount "z" "ZZ"
```

{{Out}}

```txt
3
0
```



## J


```J

   NB. jewels sums a raveled equality table
   NB. use: x jewels y  x are the stones, y are the jewels.
   intersect =: -.^:2
   jewels =: ([: +/ [: , =/~) ~.@:intersect&Alpha_j_

   'aAAbbbb ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz' jewels&>&;: 'aA ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz'
3 52

   'none' jewels ''
0
   'ZZ' jewels 'z'
0
   

```



## Java


```java
import java.util.HashSet;
import java.util.Set;

public class App {
    private static int countJewels(String stones, String jewels) {
        Set<Character> bag = new HashSet<>();
        for (char c : jewels.toCharArray()) {
            bag.add(c);
        }

        int count = 0;
        for (char c : stones.toCharArray()) {
            if (bag.contains(c)) {
                count++;
            }
        }

        return count;
    }

    public static void main(String[] args) {
        System.out.println(countJewels("aAAbbbb", "aA"));
        System.out.println(countJewels("ZZ", "z"));
    }
}
```

{{out}}

```txt
3
0
```



## Javascript


```javascript
(() => {

    // jewelCount :: String -> String -> Int
    const jewelCount = (j, s) => {
        const js = j.split('');
        return s.split('')
            .reduce((a, c) => js.includes(c) ? a + 1 : a, 0)
    };

    // TEST -----------------------------------------------
    return [
            ['aA', 'aAAbbbb'],
            ['z', 'ZZ']
        ]
        .map(x => jewelCount(...x))
})();
```

{{Out}}

```txt
[3, 0]
```



## Julia


'''Module''':

```julia
module Jewels

count(s, j) = Base.count(x ∈ j for x in s)

end  # module Jewels
```


'''Main''':

```julia
@show Jewels.count("aAAbbbb", "aA")
@show Jewels.count("ZZ", "z")
```


{{out}}

```txt
Jewels.count("aAAbbbb", "aA") = 3
Jewels.count("ZZ", "z") = 0
```



## Kotlin


```scala
// Version 1.2.40

fun countJewels(s: String, j: String) = s.count { it in j }

fun main(args: Array<String>) {
    println(countJewels("aAAbbbb", "aA"))
    println(countJewels("ZZ", "z"))
}
```


{{output}}

```txt

3
0

```



## Lua

{{trans|C}}

```lua
function count_jewels(s, j)
    local count = 0
    for i=1,#s do
        local c = s:sub(i,i)
        if string.match(j, c) then
            count = count + 1
        end
    end
    return count
end

print(count_jewels("aAAbbbb", "aA"))
print(count_jewels("ZZ", "z"))
```

{{out}}

```txt
3
0
```



## Maple


```Maple
count_jewel := proc(stones, jewels)
	local count, j, letter:
	j := convert(jewels,set):
	count := 0:
	for letter in stones do
		if (member(letter, j)) then
			count++:
		end if:
	end do:
	return count:
end proc:
count_jewel("aAAbbbb", "aA")
```

{{Out|Output}}

```txt
3
```


=={{header|Modula-2}}==

```modula2
MODULE Jewels;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE WriteInt(n : INTEGER);
VAR buf : ARRAY[0..15] OF CHAR;
BEGIN
    FormatString("%i", buf, n);
    WriteString(buf)
END WriteInt;

PROCEDURE CountJewels(s,j : ARRAY OF CHAR) : INTEGER;
VAR c,i,k : CARDINAL;
BEGIN
    c :=0;

    FOR i:=0 TO HIGH(s) DO
        FOR k:=0 TO HIGH(j) DO
            IF (j[k]#0C) AND (s[i]#0C) AND (j[k]=s[i]) THEN
                INC(c);
                BREAK
            END
        END
    END;

    RETURN c
END CountJewels;

BEGIN
    WriteInt(CountJewels("aAAbbbb", "aA"));
    WriteLn;
    WriteInt(CountJewels("ZZ", "z"));
    WriteLn;

    ReadChar
END Jewels.
```

{{out}}

```txt
3
0
```



## Objeck

{{trans|Java}}

```Objeck
use Collection.Generic;

class JewelsStones {
  function : Main(args : String[]) ~ Nil {
    Count("aAAbbbb", "aA")->PrintLine();
    Count("ZZ", "z")->PrintLine();
  }

  function : Count(stones : String, jewels : String) ~ Int {
    bag := Set->New()<CharHolder>;

    each(i : jewels) {
      bag->Insert(jewels->Get(i));
    };

    count := 0;
    each(i : stones) {
      if(bag->Has(stones->Get(i))) {
        count++;
      };
    };
 
    return count;
  }
}
```


{{out}}

```txt

3
0

```



## Perl


```perl
sub count_jewels {
    my( $j, $s ) = @_;
    my($c,%S);

    $S{$_}++     for split //, $s;
    $c += $S{$_} for split //, $j;
    return "$c\n";
}

print count_jewels 'aA' , 'aAAbbbb';
print count_jewels 'z'  , 'ZZ';
```

{{out}}

```txt
3
0
```



## Perl 6


```perl6
sub count-jewels ( Str $j, Str $s --> Int ) {
    my %counts_of_all = $s.comb.Bag;
    my @jewel_list    = $j.comb.unique;

    return %counts_of_all ∩ @jewel_list.Bag ?? %counts_of_all{ @jewel_list }.sum !! 0;
}

say count-jewels 'aA' , 'aAAbbbb';
say count-jewels 'z'  , 'ZZ';
```

{{Out}}

```txt
3
0
```



## Phix


```Phix
function count_jewels(string stones, jewels)
    integer res = 0
    for i=1 to length(stones) do
        res += find(stones[i],jewels)!=0
    end for
    return res
end function
?count_jewels("aAAbbbb","aA")
?count_jewels("ZZ","z")
```

{{out}}

```txt

3
0

```



## Python


```python
def countJewels(s, j):
    return sum(x in j for x in s)

print countJewels("aAAbbbb", "aA")
print countJewels("ZZ", "z")
```

{{Out}}

```txt
3
0
```



### Python 3 Alternative


```python
def countJewels(stones, jewels):
    jewelset = set(jewels)
    return sum(1 for stone in stones if stone in jewelset)

print(countJewels("aAAbbbb", "aA"))
print(countJewels("ZZ", "z"))
```

{{Out}}

```txt
3
0
```



## Racket


```racket
#lang racket

(define (jewels-and-stones stones jewels)
  (length (filter (curryr member (string->list jewels)) (string->list stones))))

(module+ main
  (jewels-and-stones "aAAbbbb" "aA")
  (jewels-and-stones "ZZ" "z"))

```

{{out}}

```txt
3
0
```



## REXX
 
Programming note:   a check is made so that only (Latin) letters are counted as a match.

```rexx
/*REXX pgm counts how many letters (in the 1st string) are in common with the 2nd string*/
say  count('aAAbbbb', "aA")
say  count('ZZ'     , "z" )
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
count: procedure;  parse arg stones,jewels       /*obtain the two strings specified.    */
       #= 0                                      /*initialize the variable  #  to  zero.*/
                   do j=1  for length(stones)    /*scan STONES for matching JEWELS chars*/
                   x= substr(stones, j, 1)       /*obtain a character of the STONES var.*/
                   if datatype(x, 'M')  then if pos(x, jewels)\==0  then #= # + 1
                   end   /*j*/                   /* [↑]  if a letter and a match, bump #*/
       return #                                  /*return the number of common letters. */
```

{{out|output|text=  when using the default inputs:}}

```txt
3
0
```



## Ring


```ring
# Project  Jewels and Stones
 
jewels = "aA"
stones = "aAAbbbb"
see jewelsandstones(jewels,stones) + nl
jewels = "z"
stones = "ZZ"
see jewelsandstones(jewels,stones) + nl

func jewelsandstones(jewels,stones)
        num = 0
        for n = 1 to len(stones)
             pos = substr(jewels,stones[n])
             if pos > 0
                num = num + 1
             ok
        next
        return num

```

Output:

```txt
3
0
```


## Ruby


```ruby
stones, jewels = "aAAbbbb", "aA"

stones.count(jewels)  # => 3

```



## Rust


```rust
fn count_jewels(stones: &str, jewels: &str) -> u8 {
    let mut count: u8 = 0;
    for cur_char in stones.chars() {
        if jewels.contains(cur_char) {
            count += 1;
        }
    }
    count
}
fn main() {
    println!("{}", count_jewels("aAAbbbb", "aA"));
    println!("{}", count_jewels("ZZ", "z"));
}

```

Output:
```txt
3
0
```


## Scala


```Scala
object JewelsStones extends App {
  def countJewels(s: String, j: String): Int = s.count(i => j.contains(i))

  println(countJewels("aAAbbbb", "aA"))
  println(countJewels("ZZ", "z"))
}
```

{{Out}}See it in running in your browser by [https://scalafiddle.io/sf/Cz1HXAT/0 ScalaFiddle (JavaScript)] or by [https://scastie.scala-lang.org/7ZCCN5hISRuDqLWTKVBHow Scastie (JVM)].

## Sidef


```ruby
func countJewels(s, j) {
    s.chars.count { |c|
        j.contains(c)
    }
}

say countJewels("aAAbbbb", "aA")    #=> 3
say countJewels("ZZ", "z")          #=> 0
```



## Swift


```swift
func countJewels(_ stones: String, _ jewels: String) -> Int {
  return stones.map({ jewels.contains($0) ? 1 : 0 }).reduce(0, +)
}

print(countJewels("aAAbbbb", "aA"))
print(countJewels("ZZ", "z"))
```


{{out}}


```txt
3
0
```



## VBA

{{trans|Phix}}
```vb
Function count_jewels(stones As String, jewels As String) As Integer
    Dim res As Integer: res = 0
    For i = 1 To Len(stones)
        res = res - (InStr(1, jewels, Mid(stones, i, 1), vbBinaryCompare) <> 0)
    Next i
    count_jewels = res
End Function
Public Sub main()
    Debug.Print count_jewels("aAAbbbb", "aA")
    Debug.Print count_jewels("ZZ", "z")
End Sub
```
{{out}}

```txt
 3 
 0 
```


## zkl


```zkl
fcn countJewels(a,b){ a.inCommon(b).len() }
```


```zkl
println(countJewels("aAAbbbb", "aA"));
println(countJewels("ZZ", "z"));
```

{{out}}

```txt
3
0
```

