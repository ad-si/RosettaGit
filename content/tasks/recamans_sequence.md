+++
title = "Recaman's sequence"
description = ""
date = 2019-10-21T11:02:23Z
aliases = []
[extra]
id = 21939
[taxonomies]
categories = ["task"]
tags = []
+++

The Recamán's sequence generates Natural numbers.

Starting from zero, the n'th term <code>a(n)</code> is the previous term minus <code>n</code> i.e <code>a(n) = a(n-1) - n</code> but only if this is '''both''' positive ''and'' has not been previousely generated.


If the conditions ''don't'' hold then <code>a(n) = a(n-1) + n</code>.

## Task

# Generate and show here the first 15 members of the sequence.
# Find and show here, the first duplicated number in the sequence.
# '''Optionally''': Find and show here, How many terms of the sequence are needed until all the integers 0..1000, inclusive, are generated.


## References

* [https://oeis.org/A005132 A005132],  The On-Line Encyclopedia of Integer Sequences.
* [https://www.youtube.com/watch?v=FGC5TdIiT9U The Slightly Spooky Recamán Sequence], Numberphile video.





## ALGOL W


```algolw
begin
    % calculate Recaman's sequence values                                              %

    % a hash table element - holds n, A(n) and a link to the next element with the     %
    %                        same hash value                                           %
    record AValue ( integer eN, eAn ; reference(AValue) eNext );

    % hash modulus                                                                     %
    integer HMOD;
    HMOD := 100000;

    begin
        reference(AValue) array hashTable ( 0 :: HMOD - 1 );
        integer array A ( 0 :: 14 );
        integer le1000Count, firstN, duplicateN, duplicateValue, n, An, An1, prevN;

        % adds an element to the hash table, returns true if an element with value An  %
        % was already present, false otherwise                                         %
        % if the value was already present, its eN value is returned in prevN          %
        logical procedure addAValue( integer value n, An ; integer result prevN ) ;
            begin
                integer hash;
                logical duplicate;
                reference(AValue) element;
                hash      := An rem HMOD;
                element   := hashTable( hash );
                duplicate := false;
                while element not = null and eAn(element) not = An do element := eNext(element);
                duplicate := element not = null;
                if not duplicate then hashTable( hash ) := AValue( n, An, hashTable( hash ) )
                                 else prevN := eN(element);
                duplicate
            end addAValue ;

        % initialise the hash table                                                    %
        for h := 0 until HMOD - 1 do hashTable( h ) := null;

        % calculate the values of the sequence until we have found values that         %
        % include all numbers in 1..1000                                               %
        % also store the first 15 values                                               %

        A( 0 ) := An1 := n := 0;
        le1000Count := 0;
        firstN := duplicateN := duplicateValue := -1;
        while le1000Count < 1000 do begin
            logical le0, duplicate;
            n  := n + 1;
            An := An1 - n;
            le0 := ( An <= 0 );
            if le0 then An := An1 + n;
            prevN := -1;
            duplicate := addAValue( n, An, prevN );
            if duplicate and not le0 then begin
                An := An1 + n;
                duplicate := addAValue( n, An, prevN )
            end if_duplicate_and_not_le0 ;
            if duplicate then begin
                % the value was already present %
                if firstN < 0 then begin   % have the first duplicate                  %
                    firstN         := n;
                    duplicateN     := prevN;
                    duplicateValue := An;
                end if_firstN_lt_0
                end
            else if An <= 1000 then le1000Count := le1000Count + 1;;
            if n < 15 then A( n ) := An;
            An1 := An
        end while_le1000Count_lt_1000 ;

        % show the first 15 values of the sequence                                     %
        write( "A( 0 .. 14 ): " );
        for n := 0 until 14 do writeon( i_w := 1, A( n ) );
        % positions of the first duplicate                                             %
        write( i_w := 1
               , s_w := 0
               , "First duplicates: "
               , duplicateN
               , " "
               , firstN
               , " ("
               , duplicateValue
               , ")"
               );
        % number of elements required to include the first 1000 integers               %
        write( i_w := 1, "first element to include all 1..1000: ", n )
    end

end.
```

```txt

A( 0 .. 14 ): 0  1  3  6  2  7  13  20  12  21  11  22  10  23  9
First duplicates: 20 24 (42)
first element to include all 1..1000: 328002

```



## AppleScript


The third of these tasks probably stretches Applescript a bit beyond the point of its usefulness – it takes about 1 minute to find the result, and even that requires the use of NSMutableSet, from the Apple Foundation classes.


```applescript
use AppleScript version "2.4"
use framework "Foundation"
use scripting additions

on run

  -- FIRST FIFTEEN RECAMANs ------------------------------------------------------

  script term15
    on |λ|(i)
      15 = (i as integer)
    end |λ|
  end script
  set strFirst15 to unwords(snd(recamanUpto(true, term15)))

  set strFirstMsg to "First 15 Recamans:" & linefeed
  display notification strFirstMsg & strFirst15
  delay 2

  -- FIRST DUPLICATE RECAMAN ----------------------------------------------------

  script firstDuplicate
    on |λ|(_, seen, rs)
      setSize(seen) as integer is not (length of (rs as list))
    end |λ|
  end script
  set strDuplicate to (item -1 of snd(recamanUpto(true, firstDuplicate))) as integer as string

  set strDupMsg to "First duplicated Recaman:" & linefeed
  display notification strDupMsg & strDuplicate
  delay 2

  -- NUMBER OF RECAMAN TERMS NEEDED TO GET ALL OF [0..1000]
  -- (takes about a minute, depending on system)

  set setK to setFromList(enumFromTo(0, 1000))
  script supersetK
    on |λ|(i, setR)
      setK's isSubsetOfSet:(setR)
    end |λ|
  end script

  display notification "Superset size result will take c. 1 min to find ..."
  set dteStart to current date

  set strSetSize to (fst(recamanUpto(false, supersetK)) - 1) as string

  set dteEnd to current date

  set strSetSizeMsg to "Number of Recaman terms needed to generate" & ¬
    linefeed & "all integers from [0..1000]:" & linefeed
  set strElapsed to "(Last result took c. " & (dteEnd - dteStart) & " seconds to find)"
  display notification strSetSizeMsg & linefeed & strSetSize

  -- CLEARED REFERENCE TO NSMUTABLESET -------------------------------------
  set setK to missing value

  -- REPORT ----------------------------------------------------------------
  unlines({strFirstMsg & strFirst15, "", ¬
    strDupMsg & strDuplicate, "", ¬
    strSetSizeMsg & strSetSize, "", ¬
    strElapsed})
end run

-- nextR :: Set Int -> Int -> Int
on nextR(seen, i, n)
  set bk to n - i
  if 0 > bk or setMember(bk, seen) then
    n + i
  else
    bk
  end if
end nextR

-- recamanUpto :: Bool -> (Int -> Set Int > [Int] -> Bool) -> (Int, [Int])
on recamanUpto(bln, p)
  script recaman
    property mp : mReturn(p)'s |λ|
    on |λ|()
      set i to 1
      set r to 0
      set rs to {r}
      set seen to setFromList(rs)
      repeat while not mp(i, seen, rs)
        set r to nextR(seen, i, r)
        setInsert(r, seen)
        if bln then set end of rs to r
        set i to i + 1
      end repeat
      set seen to missing value -- clear pointer to NSMutableSet
      {i, rs}
    end |λ|
  end script
  recaman's |λ|()
end recamanUpto

-- GENERIC FUNCTIONS -------------------------------------------------------

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
  if m ≤ n then
    set lst to {}
    repeat with i from m to n
      set end of lst to i
    end repeat
    return lst
  else
    return {}
  end if
end enumFromTo

-- fst :: (a, b) -> a
on fst(tpl)
  if class of tpl is record then
    |1| of tpl
  else
    item 1 of tpl
  end if
end fst

-- intercalateS :: String -> [String] -> String
on intercalateS(sep, xs)
  set {dlm, my text item delimiters} to {my text item delimiters, sep}
  set s to xs as text
  set my text item delimiters to dlm
  return s
end intercalateS

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

-- NB All names of NSMutableSets should be set to *missing value*
-- before the script exits.
-- ( scpt files containing residual ObjC pointer values can not be saved)
-- setFromList :: Ord a => [a] -> Set a
on setFromList(xs)
  set ca to current application
  ca's NSMutableSet's ¬
    setWithArray:(ca's NSArray's arrayWithArray:(xs))
end setFromList

-- setMember :: Ord a => a -> Set a -> Bool
on setMember(x, objcSet)
  missing value is not (objcSet's member:(x))
end setMember

-- setInsert :: Ord a => a -> Set a -> Set a
on setInsert(x, objcSet)
  objcSet's addObject:(x)
  objcSet
end setInsert

-- setSize :: Set a -> Int
on setSize(objcSet)
  objcSet's |count|() as integer
end setSize

-- snd :: (a, b) -> b
on snd(tpl)
  if class of tpl is record then
    |2| of tpl
  else
    item 2 of tpl
  end if
end snd

-- unlines :: [String] -> String
on unlines(xs)
  set {dlm, my text item delimiters} to ¬
    {my text item delimiters, linefeed}
  set str to xs as text
  set my text item delimiters to dlm
  str
end unlines

-- unwords :: [String] -> String
on unwords(xs)
  intercalateS(space, xs)
end unwords
```

```txt
First 15 Recamans:
0 1 3 6 2 7 13 20 12 21 11 22 10 23 9

First duplicated Recaman:
42

Number of Recaman terms needed to generate
all integers from [0..1000]:
328002

(Last result took c. 40 seconds to find)
```


## AWK


```AWK

# syntax: GAWK -f RECAMANS_SEQUENCE.AWK
# converted from Microsoft Small Basic
BEGIN {
    found_dup = 0
    n = -1
    do {
      n++
      ap = a[n-1] + n
      if (a[n-1] <= n) {
        a[n] = ap
        b[ap] = 1
      }
      else {
        am = a[n-1] - n
        if (b[am] == 1) {
          a[n] = ap
          b[ap] = 1
        }
        else {
          a[n] = am
          b[am] = 1
        }
      }
      if (n <= 14) {
        terms = sprintf("%s%s ",terms,a[n])
        if (n == 14) {
          printf("first %d terms: %s\n",n+1,terms)
        }
      }
      if (!found_dup) {
        if (dup[a[n]] == 1) {
          printf("first duplicated term: a[%d]=%d\n",n,a[n])
          found_dup = 1
        }
        dup[a[n]] = 1
      }
      if (a[n] <= 1000) {
        arr[a[n]] = ""
      }
    } while (n <= 15 || !found_dup || length(arr) < 1001)
    printf("terms needed to generate integers 0 - 1000: %d\n",n)
    exit(0)
}

```

```txt

first 15 terms: 0 1 3 6 2 7 13 20 12 21 11 22 10 23 9
first duplicated term: a[24]=42
terms needed to generate integers 0 - 1000: 328002

```



## C

```c
#include <stdio.h>
#include <stdlib.h>
#include <gmodule.h>

typedef int bool;

int main() {
    int i, n, k = 0, next, *a;
    bool foundDup = FALSE;
    gboolean alreadyUsed;
    GHashTable* used = g_hash_table_new(g_direct_hash, g_direct_equal);
    GHashTable* used1000 = g_hash_table_new(g_direct_hash, g_direct_equal);
    a = malloc(400000 * sizeof(int));
    a[0] = 0;
    g_hash_table_add(used, GINT_TO_POINTER(0));
    g_hash_table_add(used1000, GINT_TO_POINTER(0));

    for (n = 1; n <= 15 || !foundDup || k < 1001; ++n) {
        next = a[n - 1] - n;
        if (next < 1 || g_hash_table_contains(used, GINT_TO_POINTER(next))) {
            next += 2 * n;
        }
        alreadyUsed = g_hash_table_contains(used, GINT_TO_POINTER(next));
        a[n] = next;

        if (!alreadyUsed) {
            g_hash_table_add(used, GINT_TO_POINTER(next));
            if (next >= 0 && next <= 1000) {
                g_hash_table_add(used1000, GINT_TO_POINTER(next));
            }
        }

        if (n == 14) {
            printf("The first 15 terms of the Recaman's sequence are: ");
            printf("[");
            for (i = 0; i < 15; ++i) printf("%d ", a[i]);
            printf("\b]\n");
        }

        if (!foundDup && alreadyUsed) {
            printf("The first duplicated term is a[%d] = %d\n", n, next);
            foundDup = TRUE;
        }
        k = g_hash_table_size(used1000);

        if (k == 1001) {
            printf("Terms up to a[%d] are needed to generate 0 to 1000\n", n);
        }
    }
    g_hash_table_destroy(used);
    g_hash_table_destroy(used1000);
    free(a);
    return 0;
}
```


```txt

The first 15 terms of the Recaman's sequence are: [0 1 3 6 2 7 13 20 12 21 11 22 10 23 9]
The first duplicated term is a[24] = 42
Terms up to a[328002] are needed to generate 0 to 1000

```



## C++

```cpp
#include <iostream>
#include <ostream>
#include <set>
#include <vector>

template<typename T>
std::ostream& operator<<(std::ostream& os, const std::vector<T>& v) {
    auto i = v.cbegin();
    auto e = v.cend();
    os << '[';
    if (i != e) {
        os << *i;
        i = std::next(i);
    }
    while (i != e) {
        os << ", " << *i;
        i = std::next(i);
    }
    return os << ']';
}

int main() {
    using namespace std;

    vector<int> a{ 0 };
    set<int> used{ 0 };
    set<int> used1000{ 0 };
    bool foundDup = false;
    int n = 1;
    while (n <= 15 || !foundDup || used1000.size() < 1001) {
        int next = a[n - 1] - n;
        if (next < 1 || used.find(next) != used.end()) {
            next += 2 * n;
        }
        bool alreadyUsed = used.find(next) != used.end();
        a.push_back(next);
        if (!alreadyUsed) {
            used.insert(next);
            if (0 <= next && next <= 1000) {
                used1000.insert(next);
            }
        }
        if (n == 14) {
            cout << "The first 15 terms of the Recaman sequence are: " << a << '\n';
        }
        if (!foundDup && alreadyUsed) {
            cout << "The first duplicated term is a[" << n << "] = " << next << '\n';
            foundDup = true;
        }
        if (used1000.size() == 1001) {
            cout << "Terms up to a[" << n << "] are needed to generate 0 to 1000\n";
        }
        n++;
    }

    return 0;
}
```

```txt
The first 15 terms of the Recaman sequence are: [0, 1, 3, 6, 2, 7, 13, 20, 12, 21, 11, 22, 10, 23, 9]
The first duplicated term is a[24] = 42
Terms up to a[328002] are needed to generate 0 to 1000
```


## C#
```c#
using System;
using System.Collections.Generic;

namespace RecamanSequence {
    class Program {
        static void Main(string[] args) {
            List<int> a = new List<int>() { 0 };
            HashSet<int> used = new HashSet<int>() { 0 };
            HashSet<int> used1000 = new HashSet<int>() { 0 };
            bool foundDup = false;
            int n = 1;
            while (n <= 15 || !foundDup || used1000.Count < 1001) {
                int next = a[n - 1] - n;
                if (next < 1 || used.Contains(next)) {
                    next += 2 * n;
                }
                bool alreadyUsed = used.Contains(next);
                a.Add(next);
                if (!alreadyUsed) {
                    used.Add(next);
                    if (0 <= next && next <= 1000) {
                        used1000.Add(next);
                    }
                }
                if (n == 14) {
                    Console.WriteLine("The first 15 terms of the Recaman sequence are: [{0}]", string.Join(", ", a));
                }
                if (!foundDup && alreadyUsed) {
                    Console.WriteLine("The first duplicated term is a[{0}] = {1}", n, next);
                    foundDup = true;
                }
                if (used1000.Count == 1001) {
                    Console.WriteLine("Terms up to a[{0}] are needed to generate 0 to 1000", n);
                }
                n++;
            }
        }
    }
}
```

```txt
The first 15 terms of the Recaman sequence are: [0, 1, 3, 6, 2, 7, 13, 20, 12, 21, 11, 22, 10, 23, 9]
The first duplicated term is a[24] = 42
Terms up to a[328002] are needed to generate 0 to 1000
```



## D

```d
import std.stdio;

void main() {
    int[] a;
    bool[int] used;
    bool[int] used1000;
    bool foundDup;

    a ~= 0;
    used[0] = true;
    used1000[0] = true;

    int n = 1;
    while (n <= 15 || !foundDup || used1000.length < 1001) {
        int next = a[n - 1] - n;
        if (next < 1 || (next in used) !is null) {
            next += 2 * n;
        }
        bool alreadyUsed = (next in used) !is null;
        a ~= next;
        if (!alreadyUsed) {
            used[next] = true;
            if (0 <= next && next <= 1000) {
                used1000[next] = true;
            }
        }
        if (n == 14) {
            writeln("The first 15 terms of the Recaman sequence are: ", a);
        }
        if (!foundDup && alreadyUsed) {
            writefln("The first duplicated term is a[%d] = %d", n, next);
            foundDup = true;
        }
        if (used1000.length == 1001) {
            writefln("Terms up to a[%d] are needed to generate 0 to 1000", n);
        }
        n++;
    }
}
```

```txt
The first 15 terms of the Recaman sequence are: [0, 1, 3, 6, 2, 7, 13, 20, 12, 21, 11, 22, 10, 23, 9]
The first duplicated term is a[24] = 42
Terms up to a[328002] are needed to generate 0 to 1000
```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Recaman%27s_sequence this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## FreeBASIC


```freebasic
' version 26-01-2019
' compile with: fbc -s console

Dim As UByte used()
Dim As Integer sum, temp
Dim As UInteger n, max, count, i

max = 1000 : ReDim used(max)

Print "The first 15 terms are 0";

For n = 0 To 14
    temp = sum - n
    If temp < 1 OrElse used(temp) = 1 Then
        temp = sum + n
    End If
    If temp <= max Then used(temp) = 1
    sum = temp
    Print sum;
Next


sum = 0 : max = 1000 : ReDim used(max)
Print : Print

For n = 0 To 50
    temp = sum - n
    If temp < 1 OrElse used(temp) = 1 Then
        temp = sum + n
    End If
    If used(temp) = 1 Then
        Print "First duplicated number is a(" + Str(n) + ")"
        Exit For
    End If
    If temp <= max Then used(temp) = 1
    sum = temp
Next


sum = 0 : max = 2000000 : ReDim used(max)
Print : Print

For n = 0 To max
    temp = sum - n
    If temp < 1 OrElse used(temp) = 1 Then
        temp = sum + n
    End If
    If temp <= max Then used(temp) = 1
    If i = temp Then
        While used(i) = 1
            i += 1
            If i > 1000 Then
                Exit For
            End If
        Wend
    End If
    sum = temp
    count += 1
Next

Print "All integers from 0 to 1000 are generated in " & count & " terms"
Print

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
The first 15 terms are 0 0 1 3 6 2 7 13 20 12 21 11 22 10 23 9

First duplicated number is a(24)


All integers from 0 to 1000 are generated in 328002 terms
```



## Go



```go
package main

import "fmt"

func main() {
    a := []int{0}
    used := make(map[int]bool, 1001)
    used[0] = true
    used1000 := make(map[int]bool, 1001)
    used1000[0] = true
    for n, foundDup := 1, false; n <= 15 || !foundDup || len(used1000) < 1001; n++ {
        next := a[n-1] - n
        if next < 1 || used[next] {
            next += 2 * n
        }
        alreadyUsed := used[next]
        a = append(a, next)

        if !alreadyUsed {
            used[next] = true
            if next >= 0 && next <= 1000 {
                used1000[next] = true
            }
        }

        if n == 14 {
            fmt.Println("The first 15 terms of the Recaman's sequence are:", a)
        }

        if !foundDup && alreadyUsed {
            fmt.Printf("The first duplicated term is a[%d] = %d\n", n, next)
            foundDup = true
        }

        if len(used1000) == 1001 {
            fmt.Printf("Terms up to a[%d] are needed to generate 0 to 1000\n", n)
        }
    }
}
```


```txt

The first 15 terms of the Recaman's sequence are: [0 1 3 6 2 7 13 20 12 21 11 22 10 23 9]
The first duplicated term is a[24] = 42
Terms up to a[328002] are needed to generate 0 to 1000

```



## Haskell


### Recursion

A basic recursive function for the first N terms,

```haskell
recaman :: Int -> [Int]
recaman n = fst <$> reverse (go n)
  where
    go 0 = []
    go 1 = [(0, 1)]
    go x =
        let xs@((r, i):_) = go (pred x)
            back = r - i
        in ( if 0 < back && not (any ((back ==) . fst) xs)
               then back
               else r + i
           , succ i) :
           xs

main :: IO ()
main = print $ recaman 15
```

```txt
[0,1,3,6,2,7,13,20,12,21,11,22,10,23,9]
```



### Conditional iteration

Or, a little more flexibly, a '''recamanUpto''' (predicate) function.
```haskell
import Data.Set (Set, fromList, insert, isSubsetOf, member, size)
import Data.Bool (bool)

firstNRecamans :: Int -> [Int]
firstNRecamans n = reverse $ recamanUpto (\(_, i, _) -> n == i)

firstDuplicateR :: Int
firstDuplicateR = head $ recamanUpto (\(rs, _, set) -> size set /= length rs)

recamanSuperset :: Set Int -> [Int]
recamanSuperset setInts =
  tail $ recamanUpto (\(_, _, setR) -> isSubsetOf setInts setR)

recamanUpto :: (([Int], Int, Set Int) -> Bool) -> [Int]
recamanUpto p = rs
  where
    (rs, _, _) =
      until
        p
        (\(rs@(r:_), i, seen) ->
            let n = nextR seen i r
            in (n : rs, succ i, insert n seen))
        ([0], 1, fromList [0])

nextR :: Set Int -> Int -> Int -> Int
nextR seen i r =
  let back = r - i
  in bool back (r + i) (0 > back || member back seen)

-- TEST ---------------------------------------------------------------
main :: IO ()
main =
  (putStrLn . unlines)
    [ "First 15 Recamans:"
    , show $ firstNRecamans 15
    , []
    , "First duplicated Recaman:"
    , show firstDuplicateR
    , []
    , "Length of Recaman series required to include [0..1000]:"
    , (show . length . recamanSuperset) $ fromList [0 .. 1000]
    ]
```

```txt
First 15 Recamans:
[0,1,3,6,2,7,13,20,12,21,11,22,10,23,9]

First duplicated Recaman:
42

Length of Recaman series required to include [0..1000]:
328002
```



### Lazy search over infinite lists

For a lazier solution, we could define an infinite series of Recaman sequences of growing length, starting with '''[0]''', and simply search through them for the first series of length 15,
or the first to include a duplicated integer.
For the third task, it would be enough to search through an infinite stream of Recaman-generated integer '''sets''' of increasing size, until we find the first that contains [0..1000] as a subset.


```haskell
import Data.Set (Set, fromList, insert, isSubsetOf, member)
import Data.List (find, findIndex, nub)
import Data.Maybe (fromJust)
import Data.Bool (bool)

-- Infinite stream of Recaman series of growing length
rSeries :: [[Int]]
rSeries =
  scanl
    (\rs@(r:_) i ->
        (let back = r - i
         in bool back (r + i) ((0 > back) || elem back rs) : rs))
    [0]
    [1 ..]

-- Infinite stream of Recaman-generated integer sets, of growing size
rSets :: [(Set Int, Int)]
rSets =
  scanl
    (\(seen, r) i ->
        (let back = r - i
             nxt = bool back (r + i) ((0 > back) || member back seen)
         in (insert nxt seen, nxt)))
    (fromList [0], 0)
    [1 ..]

-- TEST ---------------------------------------------------------------
main :: IO ()
main = do
  let setK = fromList [0 .. 1000]
  (putStrLn . unlines)
    [ "First 15 Recamans:"
    , show . reverse . fromJust $ find ((15 ==) . length) rSeries
    , []
    , "First duplicated Recaman:"
    , show . head . fromJust $ find ((/=) <$> length <*> (length . nub)) rSeries
    , []
    , "Length of Recaman series required to include [0..1000]:"
    , show . fromJust $ findIndex (\(setR, _) -> isSubsetOf setK setR) rSets
    ]
```

```txt
First 15 Recamans:
[0,1,3,6,2,7,13,20,12,21,11,22,10,23,9]

First duplicated Recaman:
42

Length of Recaman series required to include [0..1000]:
328002
```



## J



```txt

   positive =: >&0
   unique =: -.@:e.
   condition =: (positive@:] *. unique~) ({: - #)

   NB. with the agenda set by the condition, add or subtract tail with tally
   recaman_term =: ({: + #)`({: - #)@.condition


   NB. generate four hundred thousand terms and display the first 15
   15 {. R=:(, recaman_term)^:400000]0
0 1 3 6 2 7 13 20 12 21 11 22 10 23 9


   NB. plot the sequence to see why numberphile might be interested.
   load'plot'
   plot 470{.R

   NB. binaryish search for first duplicate.
   (-:&# ~.) 100 {. R
0
   (-:&# ~.) 50 {. R
0
   (-:&# ~.) 25 {. R
0
   (-:&# ~.) 12 {. R
1
   (-:&# ~.) 18 {. R
1
   (-:&# ~.) 21 {. R
1
   (-:&# ~.) 23 {. R
1
   (-:&# ~.) 24 {. R
1
   (-:&# ~.) 25 {. R
0

```

Let's write a binary search adverb.

```J

average =: +/ % #
NB. extra_data u Bsearch bounds
NB. Bsearch returns narrowed bounds depending if u return 0 (left) or 1 (right)
NB. u is called as extra_data u index
NB.   or as        index u index
NB. u is invoked as a dyad
Bsearch =: 1 :'((0 1 + (u <.@:average)) { ({. , <.@:average, {:)@:])^:_'

```


```txt

   NB. f expresses "not all [0, 1000] are in the first y members of list x"
   f =: ([: -. [: *./ (i.1001) e. ~.)@:{.~

   R f Bsearch 0 , #R
328002 328003

   (<: 328002 328003) { R
328881 879

```


The sequence begins 0 1 3 6 2 7 13 20 12 21 11 22 10 23 9 .
With 0 as the first term,
We've learned that 25 terms are required to generate a duplicate,
and that 328003 terms are needed to generate 0 through 1000 inclusively.




## Java

```java
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class RecamanSequence {
    public static void main(String[] args) {
        List<Integer> a = new ArrayList<>();
        a.add(0);

        Set<Integer> used = new HashSet<>();
        used.add(0);

        Set<Integer> used1000 = new HashSet<>();
        used1000.add(0);

        boolean foundDup = false;
        int n = 1;
        while (n <= 15 || !foundDup || used1000.size() < 1001) {
            int next = a.get(n - 1) - n;
            if (next < 1 || used.contains(next)) {
                next += 2 * n;
            }
            boolean alreadyUsed = used.contains(next);
            a.add(next);
            if (!alreadyUsed) {
                used.add(next);
                if (0 <= next && next <= 1000) {
                    used1000.add(next);
                }
            }
            if (n == 14) {
                System.out.printf("The first 15 terms of the Recaman sequence are : %s\n", a);
            }
            if (!foundDup && alreadyUsed) {
                System.out.printf("The first duplicate term is a[%d] = %d\n", n, next);
                foundDup = true;
            }
            if (used1000.size() == 1001) {
                System.out.printf("Terms up to a[%d] are needed to generate 0 to 1000\n", n);
            }
            n++;
        }
    }
}
```

```txt
The first 15 terms of the Recaman sequence are : [0, 1, 3, 6, 2, 7, 13, 20, 12, 21, 11, 22, 10, 23, 9]
The first duplicate term is a[24] = 42
Terms up to a[328002] are needed to generate 0 to 1000
```



## JavaScript

```javascript
(() => {
    const main = () => {

        console.log(
            'First 15 Recaman:\n' +
            recamanUpto(i => 15 === i)
        );

        console.log(
            '\n\nFirst duplicated Recaman:\n' +
            last(recamanUpto(
                (_, set, rs) => set.size !== rs.length
            ))
        );

        const setK = new Set(enumFromTo(0, 1000));
        console.log(
            '\n\nNumber of Recaman terms needed to generate' +
            '\nall integers from [0..1000]:\n' +
            (recamanUpto(
                (_, setR) => isSubSetOf(setK, setR)
            ).length - 1)
        );
    };

    // RECAMAN --------------------------------------------

    // recamanUpto :: (Int -> Set Int > [Int] -> Bool) -> [Int]
    const recamanUpto = p => {
        let
            i = 1,
            r = 0, // First term of series
            rs = [r];
        const seen = new Set(rs);
        while (!p(i, seen, rs)) {
            r = nextR(seen, i, r);
            seen.add(r);
            rs.push(r);
            i++;
        }
        return rs;
    }

    // Next Recaman number.

    // nextR :: Set Int -> Int -> Int
    const nextR = (seen, i, n) => {
        const back = n - i;
        return (0 > back || seen.has(back)) ? (
            n + i
        ) : back;
    };

    // GENERIC --------------------------------------------

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        m <= n ? iterateUntil(
            x => n <= x,
            x => 1 + x,
            m
        ) : [];

    // isSubsetOf :: Ord a => Set a -> Set a -> Bool
    const isSubSetOf = (a, b) => {
        for (let x of a) {
            if (!b.has(x)) return false;
        }
        return true;
    };

    // iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
    const iterateUntil = (p, f, x) => {
        const vs = [x];
        let h = x;
        while (!p(h))(h = f(h), vs.push(h));
        return vs;
    };

    // last :: [a] -> a
    const last = xs =>
        0 < xs.length ? xs.slice(-1)[0] : undefined;

    // MAIN ------------------------------------------------
    return main();
})();
```

```txt
First 15 Recaman:
0,1,3,6,2,7,13,20,12,21,11,22,10,23,9

First duplicated Recaman:
42

Number of Recaman terms needed to generate
all integers from [0..1000]:
328002
```



## Julia

```julia
function recaman()
    a = Vector{Int}([0])
    used = Dict{Int, Bool}(0 => true)
    used1000 = Set(0)
    founddup = false
    termcount = 1
    while length(used1000) <= 1000
        nextterm = a[termcount] - termcount
        if nextterm < 1 || haskey(used, nextterm)
            nextterm += termcount + termcount
        end
        push!(a, nextterm)
        if !haskey(used, nextterm)
            used[nextterm] = true
            if 1 <= nextterm <= 1000
                push!(used1000, nextterm)
            end
        elseif !founddup
            println("The first duplicated term is a[$(termcount + 1)] = $nextterm.")
            founddup = true
        end
        if termcount == 14
            println("The first 15 terms of the Recaman sequence are $a")
        end
        termcount += 1
    end
    println("Terms up to $(termcount - 1) are needed to generate 0 to 1000.")
end

recaman()

```
```txt

 The first 15 terms of the Recaman sequence are [0, 1, 3, 6, 2, 7, 13, 20, 12, 21, 11, 22, 10, 23, 9]
 The first duplicated term is a[25] = 42.
 Terms up to 328002 are needed to generate 0 to 1000.

```




## Kotlin

```scala
// Version 1.2.60

fun main(args: Array<String>) {
    val a = mutableListOf(0)
    val used = mutableSetOf(0)
    val used1000 = mutableSetOf(0)
    var foundDup = false
    var n = 1
    while (n <= 15 || !foundDup || used1000.size < 1001) {
        var next = a[n - 1] - n
        if (next < 1 || used.contains(next)) next += 2 * n
        val alreadyUsed = used.contains(next)
        a.add(next)
        if (!alreadyUsed) {
            used.add(next)
            if (next in 0..1000) used1000.add(next)
        }
        if (n == 14) {
            println("The first 15 terms of the Recaman's sequence are: $a")
        }
        if (!foundDup && alreadyUsed) {
            println("The first duplicated term is a[$n] = $next")
            foundDup = true
        }
        if (used1000.size == 1001) {
            println("Terms up to a[$n] are needed to generate 0 to 1000")
        }
        n++
    }
}
```


```txt

The first 15 terms of the Recaman's sequence are: [0, 1, 3, 6, 2, 7, 13, 20, 12, 21, 11, 22, 10, 23, 9]
The first duplicated term is a[24] = 42
Terms up to a[328002] are needed to generate 0 to 1000

```



## Lua

This runs out of memory determining the final part :(
```lua
local a = {[0]=0}
local used = {[0]=true}
local used1000 = {[0]=true}
local foundDup = false
local n = 1

while n<=15 or not foundDup or #used1000<1001 do
    local nxt = a[n - 1] - n
    if nxt<1 or used[nxt] ~= nil then
        nxt = nxt + 2 * n
    end
    local alreadyUsed = used[nxt] ~= nil
    table.insert(a, nxt)
    if not alreadyUsed then
        used[nxt] = true
        if 0<=nxt and nxt<=1000 then
            used1000[nxt] = true
        end
    end
    if n==14 then
        io.write("The first 15 terms of the Recaman sequence are:")
        for k=0,#a do
            io.write(" "..a[k])
        end
        print()
    end
    if not foundDup and alreadyUsed then
        print("The first duplicated term is a["..n.."] = "..nxt)
        foundDup = true
    end
    if #used1000 == 1001 then
        print("Terms up to a["..n.."] are needed to generate 0 to 1000")
    end
    n = n + 1
end
```

```txt
The first 15 terms of the Recaman sequence are: 0 1 3 6 2 7 13 20 12 21 11 22 10 23 9
The first duplicated term is a[24] = 42
lua: not enough memory
```



## Microsoft Small Basic

Inefficency of associative array allocation in Small Basic ban to provide the optional task.

```smallbasic
' Recaman's sequence - smallbasic - 05/08/2015
    nn=15
    TextWindow.WriteLine("Recaman's sequence for the first " + nn + " numbers:")
    recaman()
    TextWindow.WriteLine(Text.GetSubTextToEnd(recaman,2))
    nn="firstdup"
    recaman()
    TextWindow.WriteLine("The first duplicated term is a["+n+"]="+a[n])

Sub recaman
    a=""
    b=""
    dup=""
    recaman=""
    firstdup=""
    If nn="firstdup" Then
        nn=1000
        firstdup="True"
    EndIf
    For n=0 To nn-1
        ap=a[n-1]+n
        If a[n-1]<=n Then
            a[n]=ap  'a[n]=a[n-1]+n
            b[ap]=1
        Else
            am=a[n-1]-n
            If b[am]=1 Then
                a[n]=ap  'a[n]=a[n-1]+n
                b[ap]=1
            Else
                a[n]=am  'a[n]=a[n-1]-n
                b[am]=1
            EndIf
        EndIf
        If firstdup Then
            If dup[a[n]]=1 Then
                Goto exitsub
            EndIf
            dup[a[n]]=1
        EndIf
        recaman=recaman+","+a[n]
    EndFor
    exitsub:
EndSub
```

```txt

Recaman's sequence for the first 15 numbers:
0,1,3,6,2,7,13,20,12,21,11,22,10,23,9
The first duplicated term is a[24]=42

```



## Objeck

```objeck
use Collection.Generic;

class RecamanSequence {
  function : Main(args : String[]) ~ Nil {
    GenerateSequence();
  }

  function : native : GenerateSequence() ~ Nil {
    a := Vector->New()<IntHolder>;
    a->AddBack(0);

    used := Set->New()<IntHolder>;
    used->Insert(0);

    used1000 := Set->New()<IntHolder>;
    used1000->Insert(0);

    foundDup := false;
    n := 1;
    while (n <= 15 | <>foundDup | used1000->Size() < 1001) {
      next := a->Get(n - 1) - n;
      if (next < 1 | used->Has(next)) {
        next += 2 * n;
      };
      alreadyUsed := used->Has(next);
      a->AddBack(next);
      if (<>alreadyUsed) {
        used->Insert(next);
        if (0 <= next & next <= 1000) {
          used1000->Insert(next);
        };
      };
      if (n = 14) {
        str := ToString(a);
        "The first 15 terms of the Recaman sequence are : {$str}"->PrintLine();
      };
      if (<>foundDup & alreadyUsed) {
        "The first duplicate term is a[{$n}] := {$next}"->PrintLine();
        foundDup := true;
      };
      if (used1000->Size() = 1001) {
        "Terms up to a[{$n}] are needed to generate 0 to 1000"->PrintLine();
      };
      n++;
    };
  }

  function : ToString(a : Vector<IntHolder>) ~ String {
    out := "[";
    each(i : a) {
      out += a->Get(i)->Get();
      if(i + 1 < a->Size())  {
        out += ',';
      };
    };
    out += ']';

    return out;
  }
}
```


```txt

The first 15 terms of the Recaman sequence are : [0,1,3,6,2,7,13,20,12,21,11,22,10,23,9]
The first duplicate term is a[24] := 42
Terms up to a[328002] are needed to generate 0 to 1000

```



## Perl


```perl
use bignum;

$max = 1000;
$remaining += $_ for 1..$max;

my @recamans = 0;
my $previous = 0;

while ($remaining > 0) {
   $term++;
   my $this = $previous - $term;
   $this = $previous + $term unless $this > 0 and !$seen{$this};
   push @recamans, $this;
   $dup = $term if !$dup and defined $seen{$this};
   $remaining -= $this if $this <= $max and ! defined $seen{$this};
   $seen{$this}++;
   $previous = $this;
}

print "First fifteen terms of Recaman's sequence: " . join(' ', @recamans[0..14]) . "\n";
print "First duplicate at term: a[$dup]\n";
print "Range 0..1000 covered by terms up to a[$term]\n";
```

```txt
First fifteen terms of Recaman's sequence: 0 1 3 6 2 7 13 20 12 21 11 22 10 23 9
First duplicate at term: a[24]
Range 0..1000 covered by terms up to a[328002]
```



## Perl 6

```perl6
my @recamans = 0, {
   state %seen;
   state $term;
   $term++;
   my $this = $^previous - $term;
   $this = $previous + $term unless ($this > 0) && !%seen{$this};
   %seen{$this} = True;
   $this
} … *;

put "First fifteen terms of Recaman's sequence: ", @recamans[^15];

say "First duplicate at term: a[{ @recamans.first({@recamans[^$_].Bag.values.max == 2})-1 }]";

my @seen;
my int $i = 0;
loop {
    next if (my int $this = @recamans[$i++]) > 1000 or @seen[$this];
    @seen[$this] = 1;
    say "Range 0..1000 covered by terms up to a[{$i - 1}]" and last if ++$ == 1001;
}
```

```txt
First fifteen terms of Recaman's sequence: 0 1 3 6 2 7 13 20 12 21 11 22 10 23 9
First duplicate at term: a[24]
Range 0..1000 covered by terms up to a[328002]
```



## Phix

```Phix
bool found_duplicate = false
sequence used = {} -- (grows to 1,942,300 entries)
integer all_used = 0
sequence a = {0}
integer n = 1, next, prev = 0
while n<=15 or not found_duplicate or all_used<1000 do
    next = prev - n
    if next<1 or (next<=length(used) and used[next]) then
        next = prev + n
    end if
    a &= next
    integer pad = next-length(used)
    bool already_used = pad<=0 and used[next]
    if not already_used then
        if pad>0 then used &= repeat(false,pad) end if
        used[next] = true
        while all_used<length(used) and used[all_used+1] do
            all_used += 1
        end while
    end if
    if length(a)=15 then
        puts(1,"The first 15 terms of the Recaman sequence are: ") ?a
    end if
    if already_used and not found_duplicate then
        printf(1,"The first duplicated term is a[%d] = %d\n", {n, next})
        found_duplicate = true;
    end if
    if all_used>=1000 then
        printf(1,"Terms up to a[%d] are needed to generate 0 to 1000\n", {n});
    end if
    prev = next
    n += 1
end while
```

```txt

The first 15 terms of the Recaman sequence are: {0,1,3,6,2,7,13,20,12,21,11,22,10,23,9}
The first duplicated term is a[24] = 42
Terms up to a[328002] are needed to generate 0 to 1000

```



## PHP

```php
<?php
$a = array();
array_push($a, 0);

$used = array();
array_push($used, 0);

$used1000 = array();
array_push($used1000, 0);

$foundDup = false;
$n = 1;

while($n <= 15 || !$foundDup || count($used1000) < 1001) {
	$next = $a[$n - 1] - $n;
	if ($next < 1 || in_array($next, $used)) {
		$next += 2 * $n;
	}
	$alreadyUsed = in_array($next, $used);
	array_push($a, $next);
	if (!$alreadyUsed) {
		array_push($used, $next);
		if (0 <= $next && $next <= 1000) {
			array_push($used1000, $next);
		}
	}
	if ($n == 14) {
		echo "The first 15 terms of the Recaman sequence are : [";
		foreach($a as $i => $v) {
			if ( $i == count($a) - 1)
				echo "$v";
			else
				echo "$v, ";
		}
		echo "]\n";
	}
	if (!$foundDup && $alreadyUsed) {
		printf("The first duplicate term is a[%d] = %d\n", $n, $next);
		$foundDup = true;
	}
	if (count($used1000) == 1001) {
		printf("Terms up to a[%d] are needed to generate 0 to 1000\n", $n);
	}
	$n++;
}

```


```txt

The first 15 terms of the Recaman sequence are : [0, 1, 3, 6, 2, 7, 13, 20, 12, 21, 11, 22, 10, 23, 9]
The first duplicate term is a[24] = 42
Terms up to a[328002] are needed to generate 0 to 1000

```



## Python


### Conditional iteration over a generator


```python
from itertools import islice

class Recamans():
    "Recamán's sequence generator callable class"
    def __init__(self):
        self.a = None   # Set of results so far
        self.n = None   # n'th term (counting from zero)

    def __call__(self):
        "Recamán's sequence  generator"
        nxt = 0
        a, n = {nxt}, 0
        self.a = a
        self.n = n
        yield nxt
        while True:
            an1, n = nxt, n + 1
            nxt = an1 - n
            if nxt < 0 or nxt in a:
                nxt = an1 + n
            a.add(nxt)
            self.n = n
            yield nxt

if __name__ == '__main__':
    recamans = Recamans()
    print("First fifteen members of Recamans sequence:",
          list(islice(recamans(), 15)))

    so_far = set()
    for term in recamans():
        if term in so_far:
            print(f"First duplicate number in series is: a({recamans.n}) = {term}")
            break
        so_far.add(term)

    n = 1_000
    setn = set(range(n + 1))    # The target set of numbers to be covered
    for _ in recamans():
        if setn.issubset(recamans.a):
            print(f"Range 0 ..{n} is covered by terms up to a({recamans.n})")
            break
```


```txt
First fifteen members of Recamans sequence: [0, 1, 3, 6, 2, 7, 13, 20, 12, 21, 11, 22, 10, 23, 9]
First duplicate number in series is: a(24) = 42
Range 0 ..1000 is covered by terms up to a(328002)
```



### Parameterised query predicates

Passing different query predicates to a single more general function:

( This turns out to be c. 8X faster than than the ''iteration over generator'' approach above, on a simple start to end measure using ''time.time()'')

```python
'''Recaman sequence'''


# recamanUntil :: (Int -> Set Int > [Int] -> Bool) -> [Int]
def recamanUntil(p):
    '''All terms of the Recaman series before the
       first term for which the predicate p holds.'''
    n = 1
    r = 0  # First term of series
    rs = [r]
    seen = set(rs)
    blnNew = True
    while not p(seen, n, r, blnNew):
        r = recamanSucc(seen, n, r)
        blnNew = r not in seen
        seen.add(r)
        rs.append(r)
        n = 1 + n
    return rs


# recamanSucc :: Set Int -> Int -> Int
def recamanSucc(seen, n, r):
    '''The successor for a given Recaman term,
       given the set of Recaman terms seen so far.'''
    back = r - n
    return n + r if 0 > back or (back in seen) else back


# TEST -------------------------------------------------
# main :: IO ()
def main():
    '''Test'''
    print(
        'First 15 Recaman:\r',
        recamanUntil(
            lambda seen, n, r, _: 15 == n
        )
    )
    print(
        'First duplicated Recaman:\r',
        recamanUntil(
            lambda seen, n, r, blnNew: not blnNew
        )[-1]
    )
    setK = set(enumFromTo(0)(1000))
    print(
        'Number of Recaman terms needed to generate',
        'all integers from [0..1000]:\r',
        len(recamanUntil(
            lambda seen, n, r, blnNew: (
                blnNew and 1001 > r and setK.issubset(seen)
            )
        )) - 1
    )


# GENERIC -------------------------------------------------

# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


if __name__ == '__main__':
    main()
```

```txt
First 15 Recaman:
 [0, 1, 3, 6, 2, 7, 13, 20, 12, 21, 11, 22, 10, 23, 9]
First duplicated Recaman:
 42
Number of Recaman terms needed to generate all integers from [0..1000]:
 328002
```


Alternatively, we can use query predicates in combination with the iteration of a function over a tuple,

and encapsulate the querying in the generic abstractions '''iterate''' and '''until'''.

This additional level of abstraction reduces the amount of new code that we have to write, facilitates refactoring, and turns out to have insignificant cost.

( This version is still c. 8X faster than the ''conditional iteration over generator'' version, as measured by a simple start and end test using ''time.time()'' ).


```python
'''Recaman by iteration of a function over a tuple.'''

from itertools import (islice)


# recamanTupleSucc :: Set Int -> (Int, Int, Bool) -> (Int, Int, Bool)
def recamanTupleSucc(seen):
    '''The Nth in a series of Recaman tuples,
       (N, previous term, boolPreviouslySeen?)
       given the set of all terms seen so far.'''
    def go(n, r, _):
        back = r - n
        nxt = n + r if 0 > back or (back in seen) else back
        bln = nxt in seen
        seen.add(nxt)
        return (1 + n, nxt, bln)
    return lambda tpl: go(*tpl)


# TEST -------------------------------------------------
# main :: IO()
def main():
    '''Test'''

    f = recamanTupleSucc(set([0]))
    print(
        'First 15 Recaman:\n',
        list(map(
            snd,
            take(15)(iterate(f)((1, 0, False)))
        ))
    )
    f = recamanTupleSucc(set([0]))
    print(
        'First duplicated Recaman:\n',
        until(lambda x: x[2])(f)(
            (1, 0, False)
        )[1]
    )

    sk = set(enumFromTo(0)(1000))
    sr = set([0])
    f = recamanTupleSucc(sr)
    print(
        'Number of Recaman terms needed to generate',
        'all integers from [0..1000]:\n',
        until(lambda x: not x[2] and 1001 > x[1] and sk.issubset(sr))(f)(
            (1, 0, False)
        )[0] - 1
    )

# GENERIC ABSTRACTIONS ------------------------------------


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# iterate :: (a -> a) -> a -> Gen [a]
def iterate(f):
    '''An infinite list of repeated applications of f to x.'''
    def go(x):
        v = x
        while True:
            yield v
            v = f(v)
    return lambda x: go(x)


# snd :: (a, b) -> b
def snd(tpl):
    '''Second component of a tuple.'''
    return tpl[1]


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.'''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, list)
        else list(islice(xs, n))
    )


# until :: (a -> Bool) -> (a -> a) -> a -> a
def until(p):
    '''The result of repeatedly applying f until p holds.
       The initial seed value is x.'''
    def go(f, x):
        v = x
        while not p(v):
            v = f(v)
        return v
    return lambda f: lambda x: go(f, x)


# MAIN ---
if __name__ == '__main__':
    main()
```


```txt
First 15 Recaman:
 [0, 1, 3, 6, 2, 7, 13, 20, 12, 21, 11, 22, 10, 23, 9]
First duplicated Recaman:
 42
Number of Recaman terms needed to generate all integers from [0..1000]:
 328002
```



## R

A bit slow because the append() function is expensive.

```rsplus

visited <- vector('logical', 1e8)

terms <- vector('numeric')

in_a_interval <- function(v) {
	visited[[v+1]]
}

add_value <- function(v) {
	visited[[v+1]] <<- TRUE
	terms <<- append(terms, v)
}

add_value(0)
step <- 1
value <- 0

founddup <- FALSE

repeat {
	if ((value-step>0) && (!in_a_interval(value-step))) {
		value <- value - step
	} else {
		value <- value + step
	}
	if (in_a_interval(value) && !founddup) {
		cat("The first duplicated term is a[",step,"] = ",value,"\n", sep = "")
		founddup <- TRUE
	}
	add_value(value)
	if (all(visited[1:1000])) {
		cat("Terms up to a[",step,"] are needed to generate 0 to 1000\n",sep = "")
		break
	}
	step <- step + 1
	if (step == 15) {
		cat("The first 15 terms are :")
		for (aterm in terms) { cat(aterm," ", sep = "") }
		cat("\n")
	}
}

```

```txt

The first 15 terms are :0 1 3 6 2 7 13 20 12 21 11 22 10 23 9
The first duplicated term is a[24] = 42
Terms up to a[328002] are needed to generate 0 to 1000

```



## REXX


### version 1

Instead of using a subroutine to perform the three tasks with one invocation,   the subroutine was used three times for

each of the task's three requirements.

Programmer's note:   the short-circuited   '''if'''   REXX statement   (lines '''14''' <small>&</small> '''15'''):
   if z<0  then              z= _ + #
           else if !.z  then z= _ + #
could've been replaced with:
   if !.z | z<0         then z= _ + #

```rexx
/*REXX pgm computes a Recamán sequence up to N; the 1st dup; # terms for a range of #'s.*/
parse arg N h .                                  /*obtain optional arguments from the CL*/
if N=='' | N==","  then N=   15                  /*Not specified?  Then use the default.*/
if h=='' | h==","  then h= 1000                  /* "      "         "   "   "     "    */
      say "Recamán's sequence for the first "        N         " numbers: "    recaman(N)
say;  say "The first duplicate number in the Recamán's sequence is: "          recaman(0)
say;  say "The number of terms to complete the range  0───►"h    ' is: '       recaman(-h)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
recaman: procedure; parse arg y,,d.; $=0;  !.=0;   _=!.;   !.0=1 /*init. array and vars.*/
                    r= y<0;          Reca= 0;    hi= abs(y)      /*for the 2nd invoke.  */
                    p= y>0;          if y<1  then y= 1e8         /* "   "  3rd    "     */
           do #=1  for y-1;          z= _ - #                    /*next # might be < 0. */
           if z<0  then              z= _ + #                    /*this is faster than: */
                   else if !.z  then z= _ + #                    /*if !.z | z<0 then ···*/
                           !.z= 1;      _= z                     /*mark it;  add to seq.*/
              select                                             /*ordered WHENs by vote*/
              when r  then do;  if z>hi      then iterate        /*ignore #'s too large.*/
                                if d.z==''   then Reca= Reca + 1 /*Unique? Bump counter.*/
                                d.z= .                           /*mark # as a new low. */
                                if Reca>=hi  then return #       /*list is complete ≥ HI*/
                           end                                   /* [↑]  a range of #s. */
              when p  then      $= $ z                           /*add number to $ list?*/
              otherwise         if d.z==.  then return z;  d.z=. /*check if duplicate #.*/
              end   /*select*/
           end      /*#*/;                      return $         /*return the  $  list. */
```

Run time was under   '''<sup>1</sup>/<sub>3</sub>'''   of a second.

```txt

Recamán's sequence for the first  15  numbers:  0 1 3 6 2 7 13 20 12 21 11 22 10 23 9

The first duplicate number in the Recamán's sequence is:  42

The number of terms to complete the range  0───►1000  is:  328002

```



### version 2


```rexx
/*REXX program computes & displays the Recaman sequence           */
/*improved using version 1's method for task 3                    */
Call time 'R'                  /* Start timer                     */
Parse Arg n
If n='' Then n=15
Say 'the first' n 'elements:' recaman(n)
Say ans.2
Say ans.3
Say time('E') 'seconds elapsed'
Exit

recaman:
Parse Arg n                    /* Wanted number of elements       */
have.=0                        /* Number not yet in sequence      */
e.0=0                          /* First element                   */
have.0=1                       /*   is in the sequence            */
s=0                            /* Sequence to be shodn            */
done=0                         /* turn on first duplicate switch  */
d.=0
d.0=1
dn=1                           /* number of elements <=1000       */
 Do i=1 until dn==1001         /* Loop until all found            */
  ip=i-1                       /* previous index                  */
  temp=e.ip-i                  /* potential next element          */
  If temp>0 & have.temp=0 Then /*   to be used                    */
    Nop
  Else                         /* compute the alternative         */
    temp=e.ip+i
  e.i=temp                     /* Set next element                */
  If words(s)<n Then           /* not enough in output            */
    s=s temp                   /* add the element to the output   */
  If temp<=1000 Then Do        /* eligible for task 3             */
    If d.temp=0 Then Do        /* not yet encountered             */
      d.temp=1                 /* Remember it's there             */
      dn=dn+1                  /* count of integers<=1000 found   */
      End
    End
  If done=0 & have.temp=1 Then Do
    ans.2='First duplicate ('temp') added in iteration' i,
          'elapsed:' time('E') 'seconds'
    done=1
    End
  ans.3='Element number' i 'is the last to satisfy task 3. It is' temp
  Have.temp=1
  End
Return s
```

```txt
the first 15 elements: 0 1 3 6 2 7 13 20 12 21 11 22 10 23 9
First duplicate (42) added in iteration 24 elapsed: 0 seconds
Element number 328002 is the last to satisfy task 3. It is 879
7.126000 seconds elapsed
```



## Scala

{{Out}}Best seen in running your browser either by [https://scalafiddle.io/sf/xjLHy7m/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/FdJaIB68S8i5OSndpigBtA Scastie (remote JVM)].

```Scala
import scala.collection.mutable

object RecamansSequence extends App {
  val (a, used) = (mutable.ArrayBuffer[Int](0), mutable.BitSet())
  var (foundDup, hop, nUsed1000) = (false, 1, 0)

  while (nUsed1000 < 1000) {
    val _next = a(hop - 1) - hop
    val next = if (_next < 1 || used.contains(_next)) _next + 2 * hop else _next
    val alreadyUsed = used.contains(next)

    a += next
    if (!alreadyUsed) {
      used.add(next)
      if (next <= 1000) nUsed1000 += 1
    }
    if (!foundDup && alreadyUsed) {
      println(s"The first duplicate term is a($hop) = $next")
      foundDup = true
    }
    if (nUsed1000 == 1000)
      println(s"Terms up to $hop are needed to generate 0 to 1000")

    hop += 1
  }

  println(s"The first 15 terms of the Recaman sequence are : ${a.take(15)}")

}
```



## Sidef


```ruby
func recamans_generator() {

    var term = 0
    var prev = 0
    var seen = Hash()

    {
        var this = (prev - term)

        if ((this <= 0) || seen{this}) {
            this = (prev + term)
        }

        prev = this
        seen{this} = true
        term++
        this
    }
}

with (recamans_generator()) { |r|
    say ("First 15 terms of the Recaman's sequence: ", 15.of { r.run }.join(', '))
}

with (recamans_generator()) {|r|
    var seen = Hash()
    Inf.times {|i|
        var n = r.run
        if (seen{n}) {
            say "First duplicate term in the series is a(#{i}) = #{n}"
            break
        }
        seen{n} = true
    }
}

with (recamans_generator()) {|r|
    var seen = Hash()
    Inf.times {|i|
        var n = r.run
        if ((n <= 1000) && (seen{n} := true) && (seen.len == 1001)) {
            say "Terms up to a(#{i}) are needed to generate 0 to 1000"
            break
        }
    }
}
```

```txt
First 15 terms of the Recaman's sequence: 0, 1, 3, 6, 2, 7, 13, 20, 12, 21, 11, 22, 10, 23, 9
First duplicate term in the series is a(24) = 42
Terms up to a(328002) are needed to generate 0 to 1000
```



## VBScript

To run in console mode with cscript.

```vb
' Recaman's sequence - vbscript - 04/08/2015
	nx=15
	h=1000
	Wscript.StdOut.WriteLine "Recaman's sequence for the first " & nx & " numbers:"
	Wscript.StdOut.WriteLine recaman("seq",nx)
	Wscript.StdOut.WriteLine "The first duplicate number is: " & recaman("firstdup",0)
	Wscript.StdOut.WriteLine "The number of terms to complete the range 0--->"& h &" is: "& recaman("numterm",h)
	Wscript.StdOut.Write vbCrlf&".../...": zz=Wscript.StdIn.ReadLine()

function recaman(op,nn)
	Dim b,d,h
	Set b = CreateObject("Scripting.Dictionary")
	Set d = CreateObject("Scripting.Dictionary")
    list="0" : firstdup=0
	if op="firstdup" then
		nn=1000 : firstdup=1
	end if
	if op="numterm" then
		h=nn : nn=10000000 : numterm=1
	end if
	ax=0  'a(0)=0
	b.Add 0,1  'b(0)=1
	s=0
	for n=1 to nn-1
        an=ax-n
		if an<=0 then
			an=ax+n
		elseif b.Exists(an) then
			an=ax+n
		end if
		ax=an  'a(n)=an
		if not b.Exists(an) then b.Add an,1  'b(an)=1
		if op="seq" then
			list=list&" "&an
		end if
		if firstdup then
			if d.Exists(an) then
				recaman="a("&n&")="&an
				exit function
			else
				d.Add an,1  'd(an)=1
			end if
		end if
		if numterm then
			if an<=h then
				if not d.Exists(an) then
					s=s+1
					d.Add an,1  'd(an)=1
				end if
				if s>=h then
					recaman=n
					exit function
				end if
			end if
		end if
	next 'n
	recaman=list
end function 'recaman
```

```txt

Recaman's sequence for the first 15 numbers:
0 1 3 6 2 7 13 20 12 21 11 22 10 23 9
The first duplicate number is: a(24)=42
The number of terms to complete the range 0--->1000 is: 328002

```


## Visual Basic .NET

```vbnet
Imports System
Imports System.Collections.Generic

Module Module1
    Sub Main(ByVal args As String())
        Dim a As List(Of Integer) = New List(Of Integer)() From { 0 },
            used As HashSet(Of Integer) = New HashSet(Of Integer)() From { 0 },
            used1000 As HashSet(Of Integer) = used.ToHashSet(),
            foundDup As Boolean = False
        For n As Integer = 1 to Integer.MaxValue
            Dim nv As Integer = a(n - 1) - n
            If nv < 1 OrElse used.Contains(nv) Then nv += 2 * n
            Dim alreadyUsed As Boolean = used.Contains(nv) : a.Add(nv)
            If Not alreadyUsed Then used.Add(nv) : If nv > 0 AndAlso nv <= 1000 Then used1000.Add(nv)
            If Not foundDup Then
                If a.Count = 15 Then _
                    Console.WriteLine("The first 15 terms of the Recamán sequence are: ({0})", String.Join(", ", a))
                If alreadyUsed Then _
                    Console.WriteLine("The first duplicated term is a({0}) = {1}", n, nv) : foundDup = True
            End If
            If used1000.Count = 1001 Then _
                Console.WriteLine("Terms up to a({0}) are needed to generate 0 to 1000", n) : Exit For
        Next
    End Sub
End Module
```
```txt
The first 15 terms of the Recamán sequence are: (0, 1, 3, 6, 2, 7, 13, 20, 12, 21, 11, 22, 10, 23, 9)
The first duplicated term is a(24) = 42
Terms up to a(328002) are needed to generate 0 to 1000
```


## zkl


```zkl
fcn recamanW{  // -->iterator -->(n,a,True if a is a dup)
   Walker.tweak(fcn(rn,rp,d){
      n,p,a := rn.value, rp.value, p - n;
      if(a<=0 or d.find(a)) a+=2*n;
      d.incV(a); rp.set(a);
      return(rn.inc(),a,d[a]>1);
   }.fp(Ref(0),Ref(0),Dictionary()) )
}
```


```zkl
print("First 15 members of Recaman's sequence: ");
recamanW().walk(15).apply("get",1).println();

n,a := recamanW().filter1("get",2);  // ie filter(a[n].dup)
println("First duplicate number in series is: a(%d) = %d".fmt(n,a));

rw,ns,n,a,dup := recamanW(),1000,0,0,0;
do{ n,a,dup=rw.next(); if(not dup and a<1000) ns-=1; }while(ns);
println("Range 0..1000 is covered by terms up to a(%,d)".fmt(n));
```

```txt

First 15 members of Recamans sequence: L(0,1,3,6,2,7,13,20,12,21,11,22,10,23,9)
First duplicate number in series is: a(24) = 42
Range 0..1000 is covered by terms up to a(328,002)

```

