+++
title = "Find the missing permutation"
description = ""
date = 2019-10-10T05:33:29Z
aliases = []
[extra]
id = 5247
[taxonomies]
categories = []
tags = []
+++

{{task}}


```txt

ABCD
CABD
ACDB
DACB
BCDA
ACBD
ADCB
CDAB
DABC
BCAD
CADB
CDBA
CBAD
ABDC
ADBC
BDCA
DCBA
BACD
BADC
BDAC
CBDA
DBCA
DCAB

```

Listed above are all of the permutations of the symbols   '''A''',   '''B''',   '''C''',   and   '''D''',   ''except''   for one permutation that's   ''not''   listed.


;Task:
Find that missing permutation.


;Methods:
* Obvious method:
         enumerate all permutations of   '''A''',  '''B''',  '''C''',  and  '''D''',
         and then look for the missing permutation.

* alternate method:
         Hint:  if all permutations were shown above,  how many
         times would  '''A'''  appear in each position?
         What is the  ''parity''  of this number?

* another alternate method:
         Hint:  if you add up the letter values of each column,
         does a missing letter   '''A''',  '''B''',  '''C''',  and  '''D'''   from each
         column cause the total value for each column to be unique?


;Related task:
*   [[Permutations]])





## 360 Assembly

{{trans|BBC BASIC}}
Very compact version, thanks to the clever [[#Perl 6|Perl 6]] "xor" algorithm.

```360asm
*        Find the missing permutation - 19/10/2015
PERMMISX CSECT
         USING  PERMMISX,R15       set base register
         LA     R4,0               i=0
         LA     R6,1               step
         LA     R7,23              to
LOOPI    BXH    R4,R6,ELOOPI       do i=1 to hbound(perms)
         LA     R5,0               j=0
         LA     R8,1               step
         LA     R9,4               to
LOOPJ    BXH    R5,R8,ELOOPJ       do j=1 to hbound(miss)
         LR     R1,R4              i
         SLA    R1,2               *4
         LA     R3,PERMS-5(R1)     @perms(i)
         AR     R3,R5              j
         LA     R2,MISS-1(R5)      @miss(j)
         XC     0(1,R2),0(R3)      miss(j)=miss(j) xor substr(perms(i),j,1)
         B      LOOPJ
ELOOPJ   B      LOOPI
ELOOPI   XPRNT  MISS,15            print buffer
         XR     R15,R15            set return code
         BR     R14                return to caller
PERMS    DC     C'ABCD',C'CABD',C'ACDB',C'DACB',C'BCDA',C'ACBD'
         DC     C'ADCB',C'CDAB',C'DABC',C'BCAD',C'CADB',C'CDBA'
         DC     C'CBAD',C'ABDC',C'ADBC',C'BDCA',C'DCBA',C'BACD'
         DC     C'BADC',C'BDAC',C'CBDA',C'DBCA',C'DCAB'
MISS     DC     4XL1'00',C' is missing'  buffer
         YREGS
         END    PERMMISX
```

{{out}}

```txt
DBAC is missing
```



## Ada



```Ada
with Ada.Text_IO;
procedure Missing_Permutations is
   subtype Permutation_Character is Character range 'A' .. 'D';

   Character_Count : constant :=
      1 + Permutation_Character'Pos (Permutation_Character'Last)
        - Permutation_Character'Pos (Permutation_Character'First);

   type Permutation_String is
     array (1 .. Character_Count) of Permutation_Character;

   procedure Put (Item : Permutation_String) is
   begin
      for I in Item'Range loop
         Ada.Text_IO.Put (Item (I));
      end loop;
   end Put;

   Given_Permutations : array (Positive range <>) of Permutation_String :=
     ("ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD",
      "ADCB", "CDAB", "DABC", "BCAD", "CADB", "CDBA",
      "CBAD", "ABDC", "ADBC", "BDCA", "DCBA", "BACD",
      "BADC", "BDAC", "CBDA", "DBCA", "DCAB");

   Count     : array (Permutation_Character, 1 .. Character_Count) of Natural
      := (others => (others => 0));
   Max_Count : Positive := 1;

   Missing_Permutation : Permutation_String;
begin
   for I in Given_Permutations'Range loop
      for Pos in 1 .. Character_Count loop
         Count (Given_Permutations (I) (Pos), Pos)   :=
           Count (Given_Permutations (I) (Pos), Pos) + 1;
         if Count (Given_Permutations (I) (Pos), Pos) > Max_Count then
            Max_Count := Count (Given_Permutations (I) (Pos), Pos);
         end if;
      end loop;
   end loop;

   for Char in Permutation_Character loop
      for Pos in 1 .. Character_Count loop
         if Count (Char, Pos) < Max_Count then
            Missing_Permutation (Pos) := Char;
         end if;
      end loop;
   end loop;

   Ada.Text_IO.Put_Line ("Missing Permutation:");
   Put (Missing_Permutation);
end Missing_Permutations;
```




## Aime


```aime
void
paste(record r, index x, text p, integer a)
{
    p = insert(p, -1, a);
    x.delete(a);
    if (~x) {
        x.vcall(paste, -1, r, x, p);
    } else {
        r[p] = 0;
    }
    x[a] = 0;
}

integer
main(void)
{
    record r;
    list l;
    index x;

    l.bill(0, "ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD", "ADCB",
           "CDAB", "DABC", "BCAD", "CADB", "CDBA", "CBAD", "ABDC", "ADBC",
           "BDCA", "DCBA", "BACD", "BADC", "BDAC", "CBDA", "DBCA", "DCAB");

    x['A'] = x['B'] = x['C'] = x['D'] = 0;

    x.vcall(paste, -1, r, x, "");

    l.ucall(r_delete, 1, r);

    o_(r.low, "\n");

    return 0;
}
```

{{Out}}

```txt
DBAC
```



## AppleScript

{{Trans|JavaScript}}
{{Trans|Haskell}} (Statistical versions)
Taking the third approach from the task description, and composing with functional primitives:

Yosemite OS X onwards (uses NSString for sorting):

```AppleScript
use framework "Foundation" -- ( sort )

-- RAREST LETTER IN EACH COLUMN ----------------------------------------------
on run
    intercalate("", ¬
        map(composeAll({¬
            head, ¬
            curry(minimumBy)'s |λ|(comparing(|length|)), ¬
            group, ¬
            sort}), ¬
            transpose(map(chars, ¬
                |words|("ABCD CABD ACDB DACB BCDA ACBD " & ¬
                    "ADCB CDAB DABC BCAD CADB CDBA " & ¬
                    "CBAD ABDC ADBC BDCA DCBA BACD " & ¬
                    "BADC BDAC CBDA DBCA DCAB")))))

    --> "DBAC"
end run

-- GENERIC FUNCTIONS ----------------------------------------------------------

-- chars :: String -> [String]
on chars(s)
    characters of s
end chars

-- Ordering  :: (-1 | 0 | 1)
-- compare :: a -> a -> Ordering
on compare(a, b)
    if a < b then
        -1
    else if a > b then
        1
    else
        0
    end if
end compare

-- comparing :: (a -> b) -> (a -> a -> Ordering)
on comparing(f)
    script
        on |λ|(a, b)
            tell mReturn(f) to compare(|λ|(a), |λ|(b))
        end |λ|
    end script
end comparing

-- composeAll :: [(a -> a)] -> (a -> a)
on composeAll(fs)
    script
        on |λ|(x)
            script
                on |λ|(f, a)
                    mReturn(f)'s |λ|(a)
                end |λ|
            end script

            foldr(result, x, fs)
        end |λ|
    end script
end composeAll

-- curry :: (Script|Handler) -> Script
on curry(f)
    script
        on |λ|(a)
            script
                on |λ|(b)
                    |λ|(a, b) of mReturn(f)
                end |λ|
            end script
        end |λ|
    end script
end curry

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

-- foldr :: (b -> a -> a) -> a -> [b] -> a
on foldr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from lng to 1 by -1
            set v to |λ|(item i of xs, v, i, xs)
        end repeat
        return v
    end tell
end foldr

-- group :: Eq a => [a] -> [[a]]
on group(xs)
    script eq
        on |λ|(a, b)
            a = b
        end |λ|
    end script

    groupBy(eq, xs)
end group

-- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
on groupBy(f, xs)
    set mf to mReturn(f)

    script enGroup
        on |λ|(a, x)
            if length of (active of a) > 0 then
                set h to item 1 of active of a
            else
                set h to missing value
            end if

            if h is not missing value and mf's |λ|(h, x) then
                {active:(active of a) & x, sofar:sofar of a}
            else
                {active:{x}, sofar:(sofar of a) & {active of a}}
            end if
        end |λ|
    end script

    if length of xs > 0 then
        set dct to foldl(enGroup, {active:{item 1 of xs}, sofar:{}}, tail(xs))
        if length of (active of dct) > 0 then
            sofar of dct & {active of dct}
        else
            sofar of dct
        end if
    else
        {}
    end if
end groupBy

-- head :: [a] -> a
on head(xs)
    if length of xs > 0 then
        item 1 of xs
    else
        missing value
    end if
end head

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- length :: [a] -> Int
on |length|(xs)
    length of xs
end |length|

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

-- minimumBy :: (a -> a -> Ordering) -> [a] -> a
on minimumBy(f, xs)
    if length of xs < 1 then return missing value
    tell mReturn(f)
        set v to item 1 of xs
        repeat with x in xs
            if |λ|(x, v) < 0 then set v to x
        end repeat
        return v
    end tell
end minimumBy

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

-- sort :: [a] -> [a]
on sort(xs)
    ((current application's NSArray's arrayWithArray:xs)'s ¬
        sortedArrayUsingSelector:"compare:") as list
end sort

-- tail :: [a] -> [a]
on tail(xs)
    if length of xs > 1 then
        items 2 thru -1 of xs
    else
        {}
    end if
end tail

-- transpose :: [[a]] -> [[a]]
on transpose(xss)
    script column
        on |λ|(_, iCol)
            script row
                on |λ|(xs)
                    item iCol of xs
                end |λ|
            end script

            map(row, xss)
        end |λ|
    end script

    map(column, item 1 of xss)
end transpose

-- words :: String -> [String]
on |words|(s)
    words of s
end |words|
```

{{Out}}

```txt
"DBAC"
```



## AutoHotkey


```AutoHotkey
IncompleteList := "ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB"

CompleteList := Perm( "ABCD" )
Missing := ""

Loop, Parse, CompleteList, `n, `r
  If !InStr( IncompleteList , A_LoopField )
    Missing .= "`n" A_LoopField

MsgBox Missing Permutation(s):%Missing%

;-------------------------------------------------

; Shortened version of [VxE]'s permutation function
; http://www.autohotkey.com/forum/post-322251.html#322251
Perm( s , dL="" , t="" , p="") {
   StringSplit, m, s, % d := SubStr(dL,1,1) , %t%
   IfEqual, m0, 1, return m1 d p
   Loop %m0%
   {
      r := m1
      Loop % m0-2
         x := A_Index + 1, r .= d m%x%
      L .= Perm(r, d, t, m%m0% d p)"`n" , mx := m1
      Loop % m0-1
         x := A_Index + 1, m%A_Index% := m%x%
      m%m0% := mx
   }
   return substr(L, 1, -1)
}
```



## AWK


This reads the list of permutations as standard input and outputs the missing one.


```awk
{
  split($1,a,"");
  for (i=1;i<=4;++i) {
    t[i,a[i]]++;
  }
}
END {
  for (k in t) {
    split(k,a,SUBSEP)
    for (l in t) {
      split(l, b, SUBSEP)
      if (a[1] == b[1] && t[k] < t[l]) {
        s[a[1]] = a[2]
        break
      }
    }
  }
  print s[1]s[2]s[3]s[4]
}
```


{{Out}}
DBAC


## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      DIM perms$(22), miss&(4)
      perms$() = "ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD", "ADCB", \
      \  "CDAB", "DABC", "BCAD", "CADB", "CDBA", "CBAD", "ABDC", "ADBC", \
      \  "BDCA", "DCBA", "BACD", "BADC", "BDAC", "CBDA", "DBCA", "DCAB"

      FOR i% = 0 TO DIM(perms$(),1)
        FOR j% = 1 TO DIM(miss&(),1)
          miss&(j%-1) EOR= ASCMID$(perms$(i%),j%)
        NEXT
      NEXT
      PRINT $$^miss&(0) " is missing"
      END
```

{{out}}

```txt

DBAC is missing

```



## Burlesque



```burlesque

ln"ABCD"r@\/\\

```


(Feed permutations via STDIN. Uses the naive method).

Version calculating frequency of occurences of each letter in each row and thus finding the missing permutation by choosing
the letters with the lowest frequency:


```burlesque

ln)XXtp)><)F:)<]u[/v\[

```



## C


```c
#include <stdio.h>

#define N 4
const char *perms[] = {
	"ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD", "ADCB", "CDAB",
	"DABC", "BCAD", "CADB", "CDBA", "CBAD", "ABDC", "ADBC", "BDCA",
	"DCBA", "BACD", "BADC", "BDAC", "CBDA", "DBCA", "DCAB",
};

int main()
{
	int i, j, n, cnt[N];
	char miss[N];

	for (n = i = 1; i < N; i++) n *= i; /* n = (N-1)!, # of occurrence */

	for (i = 0; i < N; i++) {
		for (j = 0; j < N; j++) cnt[j] = 0;

		/* count how many times each letter occur at position i */
		for (j = 0; j < sizeof(perms)/sizeof(const char*); j++)
			cnt[perms[j][i] - 'A']++;

		/* letter not occurring (N-1)! times is the missing one */
		for (j = 0; j < N && cnt[j] == n; j++);

		miss[i] = j + 'A';
	}
	printf("Missing: %.*s\n", N, miss);

	return 0;

}
```

{{out}}

```txt
Missing: DBAC
```



## C++


```cpp
#include <algorithm>
#include <vector>
#include <set>
#include <iterator>
#include <iostream>
#include <string>

static const std::string GivenPermutations[] = {
  "ABCD","CABD","ACDB","DACB",
  "BCDA","ACBD","ADCB","CDAB",
  "DABC","BCAD","CADB","CDBA",
  "CBAD","ABDC","ADBC","BDCA",
  "DCBA","BACD","BADC","BDAC",
  "CBDA","DBCA","DCAB"
};
static const size_t NumGivenPermutations = sizeof(GivenPermutations) / sizeof(*GivenPermutations);

int main()
{
    std::vector<std::string> permutations;
    std::string initial = "ABCD";
    permutations.push_back(initial);

    while(true)
    {
        std::string p = permutations.back();
        std::next_permutation(p.begin(), p.end());
        if(p == permutations.front())
            break;
        permutations.push_back(p);
    }

    std::vector<std::string> missing;
    std::set<std::string> given_permutations(GivenPermutations, GivenPermutations + NumGivenPermutations);
    std::set_difference(permutations.begin(), permutations.end(), given_permutations.begin(),
        given_permutations.end(), std::back_inserter(missing));
    std::copy(missing.begin(), missing.end(), std::ostream_iterator<std::string>(std::cout, "\n"));
    return 0;
}
```


=={{header|C sharp|C#}}==

### By permutating

{{works with|C sharp|C#|2+}}

```csharp
using System;
using System.Collections.Generic;

namespace MissingPermutation
{
    class Program
    {
        static void Main()
        {
            string[] given = new string[] { "ABCD", "CABD", "ACDB", "DACB",
                                            "BCDA", "ACBD", "ADCB", "CDAB",
                                            "DABC", "BCAD", "CADB", "CDBA",
                                            "CBAD", "ABDC", "ADBC", "BDCA",
                                            "DCBA", "BACD", "BADC", "BDAC",
                                            "CBDA", "DBCA", "DCAB" };

            List<string> result = new List<string>();
            permuteString(ref result, "", "ABCD");

            foreach (string a in result)
                if (Array.IndexOf(given, a) == -1)
                    Console.WriteLine(a + " is a missing Permutation");
        }

        public static void permuteString(ref List<string> result, string beginningString, string endingString)
        {
            if (endingString.Length <= 1)
            {
                result.Add(beginningString + endingString);
            }
            else
            {
                for (int i = 0; i < endingString.Length; i++)
                {
                    string newString = endingString.Substring(0, i) + endingString.Substring(i + 1);
                    permuteString(ref result, beginningString + (endingString.ToCharArray())[i], newString);
                }
            }
        }
    }
}
```

===By xor-ing the values===
{{works with|C sharp|C#|3+}}

```csharp
using System;
using System.Linq;

public class Test
{
    public static void Main()
    {
        var input = new [] {"ABCD","CABD","ACDB","DACB","BCDA",
            "ACBD","ADCB","CDAB","DABC","BCAD","CADB",
            "CDBA","CBAD","ABDC","ADBC","BDCA","DCBA",
            "BACD","BADC","BDAC","CBDA","DBCA","DCAB"};

        int[] values = {0,0,0,0};
        foreach (string s in input)
            for (int i = 0; i < 4; i++)
                values[i] ^= s[i];
        Console.WriteLine(string.Join("", values.Select(i => (char)i)));
    }
}
```



## Clojure


```clojure

(use 'clojure.math.combinatorics)
(use 'clojure.set)

(def given (apply hash-set (partition 4 5 "ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB" )))
(def s1 (apply hash-set (permutations "ABCD")))
(def missing (difference s1 given))

```

Here's a version based on the hint in the description. ''freqs'' is a sequence of letter frequency maps, one for each column. There should be 6 of each letter in each column, so we look for the one with 5.

```clojure
(def abcds ["ABCD" "CABD" "ACDB" "DACB" "BCDA" "ACBD" "ADCB" "CDAB"
            "DABC" "BCAD" "CADB" "CDBA" "CBAD" "ABDC" "ADBC" "BDCA"
            "DCBA" "BACD" "BADC" "BDAC" "CBDA" "DBCA" "DCAB"])

(def freqs (->> abcds (apply map vector) (map frequencies)))

(defn v->k [fqmap v] (->> fqmap (filter #(-> % second (= v))) ffirst))

(->> freqs (map #(v->k % 5)) (apply str) println)
```



## CoffeeScript



```coffeescript

missing_permutation = (arr) ->
  # Find the missing permutation in an array of N! - 1 permutations.

  # We won't validate every precondition, but we do have some basic
  # guards.
  if arr.length == 0
    throw Error "Need more data"
  if arr.length == 1
      return [arr[0][1] + arr[0][0]]

  # Now we know that for each position in the string, elements should appear
  # an even number of times (N-1 >= 2).  We can use a set to detect the element appearing
  # an odd number of times.  Detect odd occurrences by toggling admission/expulsion
  # to and from the set for each value encountered.  At the end of each pass one element
  # will remain in the set.
  result = ''
  for pos in [0...arr[0].length]
      set = {}
      for permutation in arr
          c = permutation[pos]
          if set[c]
            delete set[c]
          else
            set[c] = true
      for c of set
        result += c
        break
  result

given = '''ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA
  CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB'''

arr = (s for s in given.replace('\n', ' ').split ' ' when s != '')

console.log missing_permutation(arr)

```


{{out}}

```txt

 > coffee missing_permute.coffee
DBAC

```



## Common Lisp


```lisp
(defparameter *permutations*
  '("ABCD" "CABD" "ACDB" "DACB" "BCDA" "ACBD" "ADCB" "CDAB" "DABC" "BCAD" "CADB" "CDBA"
    "CBAD" "ABDC" "ADBC" "BDCA" "DCBA" "BACD" "BADC" "BDAC" "CBDA" "DBCA" "DCAB"))

(defun missing-perm (perms)
  (let* ((letters (loop for i across (car perms) collecting i))
	 (l (/ (1+ (length perms)) (length letters))))
    (labels ((enum (n) (loop for i below n collecting i))
	     (least-occurs (pos)
	       (let ((occurs (loop for i in perms collecting (aref i pos))))
		 (cdr (assoc (1- l) (mapcar #'(lambda (letter)
						(cons (count letter occurs) letter))
					    letters))))))
      (concatenate 'string (mapcar #'least-occurs (enum (length letters)))))))
```

{{out}}

```txt
ROSETTA> (missing-perm *permutations*)
"DBAC"
```



## D


```d
void main() {
    import std.stdio, std.string, std.algorithm, std.range, std.conv;

    immutable perms = "ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC
                       BCAD CADB CDBA CBAD ABDC ADBC BDCA DCBA BACD
                       BADC BDAC CBDA DBCA DCAB".split;

    // Version 1: test all permutations.
    immutable permsSet = perms
                         .map!representation
                         .zip(true.repeat)
                         .assocArray;
    auto perm = perms[0].dup.representation;
    do {
        if (perm !in permsSet)
            writeln(perm.map!(c => char(c)));
    } while (perm.nextPermutation);

    // Version 2: xor all the ASCII values, the uneven one
    // gets flushed out. Based on Perl 6 (via Go).
    enum len = 4;
    char[len] b = 0;
    foreach (immutable p; perms)
        b[] ^= p[];
    b.writeln;

    // Version 3: sum ASCII values.
    immutable rowSum = perms[0].sum;
    len
    .iota
    .map!(i => to!char(rowSum - perms.transversal(i).sum % rowSum))
    .writeln;

    // Version 4: a checksum, Java translation. maxCode will be 36.
    immutable maxCode = reduce!q{a * b}(len - 1, iota(3, len + 1));

    foreach (immutable i; 0 .. len) {
        immutable code = perms.map!(p => perms[0].countUntil(p[i])).sum;

        // Code will come up 3, 1, 0, 2 short of 36.
        perms[0][maxCode - code].write;
    }
}
```

{{out}}

```txt
DBAC
DBAC
DBAC
DBAC
```



## EchoLisp


```lisp

;; use the obvious methos
(lib 'list) ; for (permutations) function

;; input
(define perms '
(ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB))

;; generate all permutations
(define all-perms (map list->string (permutations '(A B C D))))
   → all-perms

;; {set} substraction
(set-substract (make-set all-perms) (make-set perms))
  → { DBAC }

```





## Elixir


```elixir
defmodule RC do
  def find_miss_perm(head, perms) do
    all_permutations(head) -- perms
  end

  defp all_permutations(string) do
    list = String.split(string, "", trim: true)
    Enum.map(permutations(list), fn x -> Enum.join(x) end)
  end

  defp permutations([]), do: [[]]
  defp permutations(list), do: (for x <- list, y <- permutations(list -- [x]), do: [x|y])
end

perms = ["ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD", "ADCB", "CDAB", "DABC", "BCAD", "CADB", "CDBA",
         "CBAD", "ABDC", "ADBC", "BDCA", "DCBA", "BACD", "BADC", "BDAC", "CBDA", "DBCA", "DCAB"]

IO.inspect RC.find_miss_perm( hd(perms), perms )
```


{{out}}

```txt

["DBAC"]

```



## Erlang

The obvious method. It seems fast enough (no waiting time).

```Erlang

-module( find_missing_permutation ).

-export( [difference/2, task/0] ).

difference( Permutate_this, Existing_permutations ) -> all_permutations( Permutate_this ) -- Existing_permutations.

task() -> difference( "ABCD", existing_permutations() ).



all_permutations( String ) -> [[A, B, C, D] || A <- String, B <- String, C <- String, D <- String, is_different([A, B, C, D])].

existing_permutations() -> ["ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD", "ADCB", "CDAB", "DABC", "BCAD", "CADB", "CDBA", "CBAD", "ABDC", "ADBC", "BDCA", "DCBA", "BACD", "BADC", "BDAC", "CBDA", "DBCA", "DCAB"].

is_different( [_H] ) -> true;
is_different( [H | T] ) -> not lists:member(H, T) andalso is_different( T ).

```

{{out}}

```txt

6> find_the_missing_permutation:task().
["DBAC"]

```



## ERRE


```ERRE

PROGRAM MISSING

CONST N=4

DIM PERMS$[23]

BEGIN
  PRINT(CHR$(12);) ! CLS
  DATA("ABCD","CABD","ACDB","DACB","BCDA","ACBD","ADCB")
  DATA("CDAB","DABC","BCAD","CADB","CDBA","CBAD","ABDC","ADBC")
  DATA("BDCA","DCBA","BACD","BADC","BDAC","CBDA","DBCA","DCAB")

  FOR I%=1 TO UBOUND(PERMS$,1) DO
    READ(PERMS$[I%])
  END FOR

  SOL$="...."

  FOR I%=1 TO N DO
    CH$=CHR$(I%+64)
    COUNT%=0
    FOR Z%=1 TO N DO
       COUNT%=0
       FOR J%=1 TO UBOUND(PERMS$,1) DO
          IF CH$=MID$(PERMS$[J%],Z%,1) THEN COUNT%=COUNT%+1 END IF
       END FOR
       IF COUNT%<>6 THEN
           !$RCODE="MID$(SOL$,Z%,1)=CH$"
       END IF
    END FOR
  END FOR
  PRINT("Solution is: ";SOL$)
END PROGRAM

```

{{out}}

```txt

Solution is: DBAC

```



## Factor

Permutations are read in via STDIN.

```factor
USING: io math.combinatorics sequences sets ;

"ABCD" all-permutations lines diff first print
```

{{out}}

```txt

DBAC

```



## Forth

'''Tested with:''' GForth, VFX Forth, SwiftForth, Win32 Forth. Should work with any ANS Forth system.

'''Method:''' Read the permutations in as hexadecimal numbers, exclusive ORing them together gives the answer.
(This solution assumes that none of the permutations is defined as a Forth word.)

```forth
 hex
 ABCD     CABD xor ACDB xor DACB xor BCDA xor ACBD xor
 ADCB xor CDAB xor DABC xor BCAD xor CADB xor CDBA xor
 CBAD xor ABDC xor ADBC xor BDCA xor DCBA xor BACD xor
 BADC xor BDAC xor CBDA xor DBCA xor DCAB xor
 cr .( Missing permutation: ) u.
 decimal
```

{{out}}

```txt
Missing permutation: DBAC  ok
```



## Fortran

'''Work-around''' to let it run properly with some bugged versions (e.g. 4.3.2) of gfortran: remove the ''parameter'' attribute to the array list.

```fortran
program missing_permutation

  implicit none
  character (4), dimension (23), parameter :: list =                    &
    & (/'ABCD', 'CABD', 'ACDB', 'DACB', 'BCDA', 'ACBD', 'ADCB', 'CDAB', &
    &   'DABC', 'BCAD', 'CADB', 'CDBA', 'CBAD', 'ABDC', 'ADBC', 'BDCA', &
    &   'DCBA', 'BACD', 'BADC', 'BDAC', 'CBDA', 'DBCA', 'DCAB'/)
  integer :: i, j, k

  do i = 1, 4
    j = minloc ((/(count (list (:) (i : i) == list (1) (k : k)), k = 1, 4)/), 1)
    write (*, '(a)', advance = 'no') list (1) (j : j)
  end do
  write (*, *)

end program missing_permutation
```

{{out}}

```txt
DBAC
```


## FreeBASIC


### Simple count


```freebasic
' version 30-03-2017
' compile with: fbc -s console

Data "ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD"
Data "ADCB", "CDAB", "DABC", "BCAD", "CADB", "CDBA"
Data "CBAD", "ABDC", "ADBC", "BDCA", "DCBA", "BACD"
Data "BADC", "BDAC", "CBDA", "DBCA", "DCAB"

' ------=< MAIN >=------

Dim As ulong total(3, Asc("A") To Asc("D"))  ' total(0 to 3, 65 to 68)
Dim As ULong i, j, n = 24 \ 4   ' n! \ n
Dim As String tmp

For i = 1 To 23
    Read tmp
    For j = 0 To 3
        total(j, tmp[j]) += 1
    Next
Next

tmp = Space(4)
For i = 0 To 3
    For j = Asc("A") To Asc("D")
        If total(i, j) <> n Then
         tmp[i] = j
        End If
    Next
Next

Print "The missing permutation is : "; tmp

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
The missing permutation is : DBAC
```

===Add the value's===

```freebasic
' version 30-03-2017
' compile with: fbc -s console

Data "ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD"
Data "ADCB", "CDAB", "DABC", "BCAD", "CADB", "CDBA"
Data "CBAD", "ABDC", "ADBC", "BDCA", "DCBA", "BACD"
Data "BADC", "BDAC", "CBDA", "DBCA", "DCAB"

' ------=< MAIN >=------

Dim As ULong total(3)  ' total(0 to 3)
Dim As ULong i, j, n = 24 \ 4   ' n! \ n
Dim As ULong total_val = (Asc("A") + Asc("B") + Asc("C") + Asc("D")) * n
Dim As String tmp

For i = 1 To 23
    Read tmp
    For j = 0 To 3
        total(j) += tmp[j]
    Next
Next

tmp = Space(4)
For i = 0 To 3
    tmp[i] = total_val - total(i)
Next

Print "The missing permutation is : "; tmp

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```


```txt
output is same as the first version
```


### Using Xor


```freebasic
' version 30-03-2017
' compile with: fbc -s console

Data "ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD"
Data "ADCB", "CDAB", "DABC", "BCAD", "CADB", "CDBA"
Data "CBAD", "ABDC", "ADBC", "BDCA", "DCBA", "BACD"
Data "BADC", "BDAC", "CBDA", "DBCA", "DCAB"

' ------=< MAIN >=------

Dim As ULong i,j
Dim As String tmp, missing = chr(0, 0, 0, 0) ' or string(4, 0)

For i = 1 To 23
    Read tmp
    For j = 0 To 3
        missing[j] Xor= tmp[j]
    Next
Next

Print "The missing permutation is : "; missing

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```


```txt
Output is the same as the first version
```



## GAP


```gap
# our deficient list
L :=
[ "ABCD", "CABD", "ACDB", "DACB", "BCDA",
  "ACBD", "ADCB", "CDAB", "DABC", "BCAD",
  "CADB", "CDBA", "CBAD", "ABDC", "ADBC",
  "BDCA", "DCBA", "BACD", "BADC", "BDAC",
  "CBDA", "DBCA", "DCAB" ];

# convert L to permutations on 1..4
u := List(L, s -> List([1..4], i -> Position("ABCD", s[i])));

# set difference (with all permutations)
v := Difference(PermutationsList([1..4]), u);

# convert back to letters
s := "ABCD";
List(v, p -> List(p, i -> s[i]));
```


## Go

Alternate method suggested by task description:

```go
package main

import (
    "fmt"
    "strings"
)

var given = strings.Split(`ABCD
CABD
ACDB
DACB
BCDA
ACBD
ADCB
CDAB
DABC
BCAD
CADB
CDBA
CBAD
ABDC
ADBC
BDCA
DCBA
BACD
BADC
BDAC
CBDA
DBCA
DCAB`, "\n")

func main() {
    b := make([]byte, len(given[0]))
    for i := range b {
        m := make(map[byte]int)
        for _, p := range given {
            m[p[i]]++
        }
        for char, count := range m {
            if count&1 == 1 {
                b[i] = char
                break
            }
        }
    }
    fmt.Println(string(b))
}
```

Xor method suggested by Perl 6 contributor:

```go
func main() {
    b := make([]byte, len(given[0]))
    for _, p := range given {
        for i, c := range []byte(p) {
            b[i] ^= c
        }
    }
    fmt.Println(string(b))
}
```

{{out}} in either case:

```txt

DBAC

```



## Groovy

Solution:

```groovy
def fact = { n -> [1,(1..<(n+1)).inject(1) { prod, i -> prod * i }].max() }
def missingPerms
missingPerms = {List elts, List perms ->
    perms.empty ? elts.permutations() : elts.collect { e ->
        def ePerms = perms.findAll { e == it[0] }.collect { it[1..-1] }
        ePerms.size() == fact(elts.size() - 1) ? [] \
            : missingPerms(elts - e, ePerms).collect { [e] + it }
    }.sum()
}
```


Test:

```groovy
def e = 'ABCD' as List
def p = ['ABCD', 'CABD', 'ACDB', 'DACB', 'BCDA', 'ACBD', 'ADCB', 'CDAB', 'DABC', 'BCAD', 'CADB', 'CDBA',
        'CBAD', 'ABDC', 'ADBC', 'BDCA', 'DCBA', 'BACD', 'BADC', 'BDAC', 'CBDA', 'DBCA', 'DCAB'].collect { it as List }

def mp = missingPerms(e, p)
mp.each { println it }
```


{{out}}

```txt
[D, B, A, C]
```



## Haskell


### =Difference between two lists=

{{works with|GHC|7.10.3}}

```haskell
import Data.List ((\\), permutations, nub)
import Control.Monad (join)

missingPerm
  :: Eq a
  => [[a]] -> [[a]]
missingPerm = (\\) =<< permutations . nub . join

deficientPermsList :: [String]
deficientPermsList =
  [ "ABCD"
  , "CABD"
  , "ACDB"
  , "DACB"
  , "BCDA"
  , "ACBD"
  , "ADCB"
  , "CDAB"
  , "DABC"
  , "BCAD"
  , "CADB"
  , "CDBA"
  , "CBAD"
  , "ABDC"
  , "ADBC"
  , "BDCA"
  , "DCBA"
  , "BACD"
  , "BADC"
  , "BDAC"
  , "CBDA"
  , "DBCA"
  , "DCAB"
  ]

main :: IO ()
main = print $ missingPerm deficientPermsList
```

{{Out}}

```txt
["DBAC"]
```



### =Character frequency in each column=

Another, more statistical, approach is to return the least common letter in each of the four columns. (If all permutations were present, letter frequencies would not vary).


```haskell
import Data.List (minimumBy, group, sort, transpose)
import Data.Ord (comparing)

missingPerm
  :: Ord a
  => [[a]] -> [a]
missingPerm = ((head . minimumBy (comparing length) . group . sort) <$>) . transpose

deficientPermsList :: [String]
deficientPermsList =
  [ "ABCD"
  , "CABD"
  , "ACDB"
  , "DACB"
  , "BCDA"
  , "ACBD"
  , "ADCB"
  , "CDAB"
  , "DABC"
  , "BCAD"
  , "CADB"
  , "CDBA"
  , "CBAD"
  , "ABDC"
  , "ADBC"
  , "BDCA"
  , "DCBA"
  , "BACD"
  , "BADC"
  , "BDAC"
  , "CBDA"
  , "DBCA"
  , "DCAB"
  ]

main :: IO ()
main = print $ missingPerm deficientPermsList
```

{{Out}}

```txt
"DBAC"
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
link strings    # for permutes

procedure main()
givens := set![ "ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD", "ADCB", "CDAB", "DABC", "BCAD", "CADB",
                "CDBA", "CBAD", "ABDC", "ADBC", "BDCA", "DCBA", "BACD", "BADC", "BDAC", "CBDA", "DBCA", "DCAB"]

every insert(full := set(), permutes("ABCD"))  # generate all permutations
givens := full--givens                         # and difference

write("The difference is : ")
every write(!givens, " ")
end
```


The approach above generates a full set of permutations and calculates the difference.  Changing the two commented lines to the three below will calculate on the fly and would be more efficient for larger data sets.


```Icon
every x := permutes("ABCD") do                    # generate all permutations
   if member(givens,x) then delete(givens,x)      # remove givens as they are generated
   else insert(givens,x)                          # add back any not given
```


A still more efficient version is:

```Icon
link strings

procedure main()
    givens := set("ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD",
                  "ADCB", "CDAB", "DABC", "BCAD", "CADB", "CDBA",
                  "CBAD", "ABDC", "ADBC", "BDCA", "DCBA", "BACD",
                  "BADC", "BDAC", "CBDA", "DBCA", "DCAB")

    every p := permutes("ABCD") do
        if not member(givens, p) then write(p)

end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/strings.icn member 'strings' provides permutes(s) which generates all permutations of a string]


## J

'''Solution:'''

```J
permutations=: A.~ i.@!@#
missingPerms=: -.~ permutations @ {.
```

'''Use:'''

```txt
data=: >;: 'ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA'
data=: data,>;: 'CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB'

   missingPerms data
DBAC
```



### Alternatives


Or the above could be a single definition that works the same way:


```J
missingPerms=: -.~ (A.~ i.@!@#) @ {.
```


Or the equivalent explicit (cf. tacit above) definition:

```J
missingPerms=: monad define
  item=. {. y
  y -.~ item A.~ i.! #item
)
```


Or, the solution could be obtained without defining an independent program:


```J
   data -.~ 'ABCD' A.~ i.!4
DBAC
```


Here, <code>'ABCD'</code> represents the values being permuted (their order does not matter), and <code>4</code> is how many of them we have.

Yet another alternative expression, which uses parentheses instead of the [http://www.jsoftware.com/help/dictionary/d220v.htm passive operator] (<code>~</code>), would be:


```J
   ((i.!4) A. 'ABCD') -. data
DBAC
```


Of course the task suggests that the missing permutation can be found without generating all permutations. And of course that is doable:


```J
   'ABCD'{~,I.@(= <./)@(#/.~)@('ABCD' , ])"1 |:perms
DBAC
```


However, that's actually a false economy - not only does this approach take more code to implement (at least, in J) but we are already dealing with a data structure of approximately the size of all permutations. So what is being saved by this supposedly "more efficient" approach? Not much... (Still, perhaps this exercise is useful as an illustration of some kind of advertising concept?)

We could use parity, as suggested in the task hints:

```J
   ,(~.#~2|(#/.~))"1|:data
DBAC
```


We could use arithmetic, as suggested in the task hints:

```J
   ({.data){~|(->./)+/({.i.])data
DBAC
```



## Java

'''optimized'''
Following needs: [[User:Margusmartsepp/Contributions/Java/Utils.java|Utils.java]]


```java
import java.util.ArrayList;

import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;

public class FindMissingPermutation {
	public static void main(String[] args) {
		Joiner joiner = Joiner.on("").skipNulls();
		ImmutableSet<String> s = ImmutableSet.of("ABCD", "CABD", "ACDB",
				"DACB", "BCDA", "ACBD", "ADCB", "CDAB", "DABC", "BCAD", "CADB",
				"CDBA", "CBAD", "ABDC", "ADBC", "BDCA", "DCBA", "BACD", "BADC",
				"BDAC", "CBDA", "DBCA", "DCAB");

		for (ArrayList<Character> cs : Utils.Permutations(Lists.newArrayList(
				'A', 'B', 'C', 'D')))
			if (!s.contains(joiner.join(cs)))
				System.out.println(joiner.join(cs));
	}
}
```


{{out}}

```txt
DBAC
```


Alternate version, based on checksumming each position:


```java
public class FindMissingPermutation
{
  public static void main(String[] args)
  {
    String[] givenPermutations = { "ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD",
                                   "ADCB", "CDAB", "DABC", "BCAD", "CADB", "CDBA",
                                   "CBAD", "ABDC", "ADBC", "BDCA", "DCBA", "BACD",
                                   "BADC", "BDAC", "CBDA", "DBCA", "DCAB" };
    String characterSet = givenPermutations[0];
    // Compute n! * (n - 1) / 2
    int maxCode = characterSet.length() - 1;
    for (int i = characterSet.length(); i >= 3; i--)
      maxCode *= i;
    StringBuilder missingPermutation = new StringBuilder();
    for (int i = 0; i < characterSet.length(); i++)
    {
      int code = 0;
      for (String permutation : givenPermutations)
        code += characterSet.indexOf(permutation.charAt(i));
      missingPermutation.append(characterSet.charAt(maxCode - code));
    }
    System.out.println("Missing permutation: " + missingPermutation.toString());
  }
}
```



## JavaScript



### ES5


### =Imperative=


The permute() function taken from http://snippets.dzone.com/posts/show/1032

```javascript
permute = function(v, m){ //v1.0
    for(var p = -1, j, k, f, r, l = v.length, q = 1, i = l + 1; --i; q *= i);
    for(x = [new Array(l), new Array(l), new Array(l), new Array(l)], j = q, k = l + 1, i = -1;
        ++i < l; x[2][i] = i, x[1][i] = x[0][i] = j /= --k);
    for(r = new Array(q); ++p < q;)
        for(r[p] = new Array(l), i = -1; ++i < l; !--x[1][i] && (x[1][i] = x[0][i],
            x[2][i] = (x[2][i] + 1) % l), r[p][i] = m ? x[3][i] : v[x[3][i]])
            for(x[3][i] = x[2][i], f = 0; !f; f = !f)
                for(j = i; j; x[3][--j] == x[2][i] && (x[3][i] = x[2][i] = (x[2][i] + 1) % l, f = 1));
    return r;
};

list = [ 'ABCD', 'CABD', 'ACDB', 'DACB', 'BCDA', 'ACBD', 'ADCB', 'CDAB',
        'DABC', 'BCAD', 'CADB', 'CDBA', 'CBAD', 'ABDC', 'ADBC', 'BDCA',
        'DCBA', 'BACD', 'BADC', 'BDAC', 'CBDA', 'DBCA', 'DCAB'];

all = permute(list[0].split('')).map(function(elem) {return elem.join('')});

missing = all.filter(function(elem) {return list.indexOf(elem) == -1});
print(missing);  // ==> DBAC
```



### =Functional=



```JavaScript
(function (strList) {

    // [a] -> [[a]]
    function permutations(xs) {
        return xs.length ? (
            chain(xs, function (x) {
                return chain(permutations(deleted(x, xs)), function (ys) {
                    return [[x].concat(ys).join('')];
                })
            })) : [[]];
    }

    // Monadic bind/chain for lists
    // [a] -> (a -> b) -> [b]
    function chain(xs, f) {
        return [].concat.apply([], xs.map(f));
    }

    // a -> [a] -> [a]
    function deleted(x, xs) {
        return xs.length ? (
            x === xs[0] ? xs.slice(1) : [xs[0]].concat(
                deleted(x, xs.slice(1))
            )
        ) : [];
    }

    // Provided subset
    var lstSubSet = strList.split('\n');

    // Any missing permutations
    // (we can use fold/reduce, filter, or chain (concat map) here)
    return chain(permutations('ABCD'.split('')), function (x) {
        return lstSubSet.indexOf(x) === -1 ? [x] : [];
    });

})(
    'ABCD\nCABD\nACDB\nDACB\nBCDA\nACBD\nADCB\nCDAB\nDABC\nBCAD\nCADB\n\
CDBA\nCBAD\nABDC\nADBC\nBDCA\nDCBA\nBACD\nBADC\nBDAC\nCBDA\nDBCA\nDCAB'
);
```


{{Out}}


```JavaScript
["DBAC"]
```



### ES6


### =Statistical=


### ==Using a dictionary==


```JavaScript
(() => {
    'use strict';

    // transpose :: [[a]] -> [[a]]
    let transpose = xs =>
        xs[0].map((_, iCol) => xs
            .map((row) => row[iCol]));


    let xs = 'ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB' +
        ' DABC BCAD CADB CDBA CBAD ABDC ADBC BDCA DCBA' +
        ' BACD BADC BDAC CBDA DBCA DCAB'

    return transpose(xs.split(' ')
            .map(x => x.split('')))
        .map(col => col.reduce((a, x) => ( // count of each character in each column
            a[x] = (a[x] || 0) + 1,
            a
        ), {}))
        .map(dct => { // character with frequency below mean of distribution ?
            let ks = Object.keys(dct),
                xs = ks.map(k => dct[k]),
                mean = xs.reduce((a, b) => a + b, 0) / xs.length;

            return ks.reduce(
                (a, k) => a ? a : (dct[k] < mean ? k : undefined),
                undefined
            );
        })
        .join(''); // 4 chars as single string

    // --> 'DBAC'
})();
```


{{Out}}

```txt
DBAC
```




### ==Composing functional primitives==

{{Trans|Haskell}}

```JavaScript
(() => {
    'use strict';

    // MISSING PERMUTATION ---------------------------------------------------

    // missingPermutation :: [String] -> String
    const missingPermutation = xs =>
        map(
            // Rarest letter,
            compose([
                sort,
                group,
                curry(minimumBy)(comparing(length)),
                head
            ]),

            // in each column.
            transpose(map(stringChars, xs))
        )
        .join('');


    // GENERIC FUNCTIONAL PRIMITIVES -----------------------------------------

    // transpose :: [[a]] -> [[a]]
    const transpose = xs =>
        xs[0].map((_, iCol) => xs.map(row => row[iCol]));

    // sort :: Ord a => [a] -> [a]
    const sort = xs => xs.sort();

    // group :: Eq a => [a] -> [[a]]
    const group = xs => groupBy((a, b) => a === b, xs);

    // groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
    const groupBy = (f, xs) => {
        const dct = xs.slice(1)
            .reduce((a, x) => {
                const
                    h = a.active.length > 0 ? a.active[0] : undefined,
                    blnGroup = h !== undefined && f(h, x);

                return {
                    active: blnGroup ? a.active.concat(x) : [x],
                    sofar: blnGroup ? a.sofar : a.sofar.concat([a.active])
                };
            }, {
                active: xs.length > 0 ? [xs[0]] : [],
                sofar: []
            });
        return dct.sofar.concat(dct.active.length > 0 ? [dct.active] : []);
    };

    // length :: [a] -> Int
    const length = xs => xs.length;

    // comparing :: (a -> b) -> (a -> a -> Ordering)
    const comparing = f =>
        (x, y) => {
            const
                a = f(x),
                b = f(y);
            return a < b ? -1 : a > b ? 1 : 0
        };

    // minimumBy :: (a -> a -> Ordering) -> [a] -> a
    const minimumBy = (f, xs) =>
        xs.reduce((a, x) => a === undefined ? x : (
            f(x, a) < 0 ? x : a
        ), undefined);

    // head :: [a] -> a
    const head = xs => xs.length ? xs[0] : undefined;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f)

    // compose :: [(a -> a)] -> (a -> a)
    const compose = fs => x => fs.reduce((a, f) => f(a), x);

    // curry :: ((a, b) -> c) -> a -> b -> c
    const curry = f => a => b => f(a, b);

    // stringChars :: String -> [Char]
    const stringChars = s => s.split('');


    // TEST ------------------------------------------------------------------

    return missingPermutation(["ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD",
        "ADCB", "CDAB", "DABC", "BCAD", "CADB", "CDBA", "CBAD", "ABDC", "ADBC",
        "BDCA", "DCBA", "BACD", "BADC", "BDAC", "CBDA", "DBCA", "DCAB"
    ]);

    // -> "DBAC"
})();
```

{{Out}}

```txt
DBAC
```



## jq


{{works with|jq|1.4}}

The following assumes that a file, Find_the_missing_permutation.txt,
has the text exactly as presented in the task description.

To find the missing permutation, we can for simplicity invoke jq twice:
  jq -R . Find_the_missing_permutation.txt | jq -s -f Find_the_missing_permutation.jq

The first invocation simply converts the raw text into a stream of JSON strings; these
are then processed by the following program, which implements the parity-based approach.

The program will handle permutations of any set of uppercase letters.  The letters need not be consecutive.
Note that the following encoding of letters is used: A => 0, B => 1, ....

'''Infrastructure''':

If your version of jq has transpose/0, the definition given here
(which is the same as in [[Matrix_Transpose#jq]]) may be omitted.

```jq
def transpose:
  if (.[0] | length) == 0 then []
  else [map(.[0])] + (map(.[1:]) | transpose)
  end ;

# Input:  an array of integers (based on the encoding of A=0, B=1, etc)
#         corresponding to the occurrences in any one position of the
#         letters in the list of permutations.
# Output: a tally in the form of an array recording in position i the
#         parity of the number of occurrences of the letter corresponding to i.
# Example: given [0,1,0,1,2], the array of counts of 0, 1, and 2 is [2, 2, 1],
#          and thus the final result is [0, 0, 1].
def parities:
  reduce .[] as $x ( []; .[$x] = (1 + .[$x]) % 2);

# Input: an array of parity-counts, e.g. [0, 1, 0, 0]
# Output: the corresponding letter, e.g. "B".
def decode:
  [index(1) + 65] | implode;

# encode a string (e.g. "ABCD") as an array (e.g. [0,1,2,3]):
def encode_string: [explode[] - 65];
```


'''The task''':

```jq
map(encode_string) | transpose | map(parities | decode) | join("")
```


{{Out}}

```sh
$ jq -R . Find_the_missing_permutation.txt | jq -s -f Find_the_missing_permutation.jq
"DBAC"
```



## Julia

{{works with|Julia|0.6}}

== Obvious method ==
Calculate all possible permutations and return the first not included in the array.

```julia
using BenchmarkTools, Combinatorics

function missingperm(arr::Vector)
    allperms = String.(permutations(arr[1]))  # revised for type safety
    for perm in allperms
        if perm ∉ arr return perm end
    end
end

arr = ["ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD", "ADCB", "CDAB", "DABC", "BCAD",
       "CADB", "CDBA", "CBAD", "ABDC", "ADBC", "BDCA", "DCBA", "BACD", "BADC", "BDAC",
       "CBDA", "DBCA", "DCAB"]
@show missingperm(arr)
```


{{out}}

```txt
missingperm(arr) = "DBAC"
```


== Alternative method 1 ==
{{trans|Python}}

```julia
function missingperm1(arr::Vector{<:AbstractString})
    missperm = string()
    for pos in 1:length(arr[1])
        s = Set()
        for perm in arr
            c = perm[pos]
            if c ∈ s pop!(s, c) else push!(s, c) end
        end
        missperm *= first(s)
    end
    return missperm
end

```


== Alternative method 2 ==
{{trans|Perl 6}}

```julia
function missingperm2(arr::Vector)
    len = length(arr[1])
    xorval = zeros(UInt8, len)
    for perm in [Vector{UInt8}(s) for s in arr], i in 1:len
        xorval[i] ⊻= perm[i]
    end
    return String(xorval)
end

@show missingperm(arr)
@show missingperm1(arr)
@show missingperm2(arr)

@btime missingperm(arr)
@btime missingperm1(arr)
@btime missingperm2(arr)

```
{{out}}

```txt

missingperm(arr) = "DBAC"
missingperm1(arr) = "DBAC"
missingperm2(arr) = "DBAC"
  6.460 μs (148 allocations: 8.55 KiB)
  6.780 μs (24 allocations: 2.13 KiB)
  3.100 μs (50 allocations: 2.94 KiB)

```



## K


```K
   split:{1_'(&x=y)_ x:y,x}

   g: ("ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB")
   g,:(" CDBA CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB")
   p: split[g;" "];

   / All permutations of "ABCD"
   perm:{:[1<x;,/(>:'(x,x)#1,x#0)[;0,'1+_f x-1];,!x]}
   p2:a@(perm(#a:"ABCD"));

   / Which permutations in p are there in p2?
   p2 _lin p
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1

   / Invert the result
   ~p2 _lin p
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0

   / It's the 20th permutation that is missing
   &~p2 _lin p
,20

   p2@&~p2 _lin p
"DBAC"
```


Alternative approach:

```K

table:{b@<b:(x@*:'a),'#:'a:=x}
,/"ABCD"@&:'{5=(table p[;x])[;1]}'!4
"DBAC"
```


Third approach (where p is the given set of permutations):

```K

,/p2@&~(p2:{x@m@&n=(#?:)'m:!n#n:#x}[*p]) _lin p

```



## Kotlin


```scala
// version 1.1.2

fun <T> permute(input: List<T>): List<List<T>> {
    if (input.size == 1) return listOf(input)
    val perms = mutableListOf<List<T>>()
    val toInsert = input[0]
    for (perm in permute(input.drop(1))) {
        for (i in 0..perm.size) {
            val newPerm = perm.toMutableList()
            newPerm.add(i, toInsert)
            perms.add(newPerm)
        }
    }
    return perms
}

fun <T> missingPerms(input: List<T>, perms: List<List<T>>) = permute(input) - perms

fun main(args: Array<String>) {
    val input = listOf('A', 'B', 'C', 'D')
    val strings = listOf(
        "ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD", "ADCB", "CDAB",
        "DABC", "BCAD", "CADB", "CDBA", "CBAD", "ABDC", "ADBC", "BDCA",
        "DCBA", "BACD", "BADC", "BDAC", "CBDA", "DBCA", "DCAB"
    )
    val perms = strings.map { it.toList() }
    val missing = missingPerms(input, perms)
    if (missing.size == 1)
        print("The missing permutation is ${missing[0].joinToString("")}")
    else {
        println("There are ${missing.size} missing permutations, namely:\n")
        for (perm in missing) println(perm.joinToString(""))
    }
}
```


{{out}}

```txt

The missing permutation is DBAC

```



## Lua

Using the popular Penlight extension module - https://luarocks.org/modules/steved/penlight

```Lua
local permute, tablex = require("pl.permute"), require("pl.tablex")
local permList, pStr = {
    "ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD", "ADCB", "CDAB",
    "DABC", "BCAD", "CADB", "CDBA", "CBAD", "ABDC", "ADBC", "BDCA",
    "DCBA", "BACD", "BADC", "BDAC", "CBDA", "DBCA", "DCAB"
}
for perm in permute.iter({"A","B","C","D"}) do
    pStr = table.concat(perm)
    if not tablex.find(permList, pStr) then print(pStr) end
end
```

{{out}}

```txt
DBAC
```



## Maple


```Maple
lst := ["ABCD","CABD","ACDB","DACB","BCDA","ACBD","ADCB","CDAB","DABC","BCAD","CADB","CDBA","CBAD","ABDC","ADBC","BDCA","DCBA","BACD","BADC","BDAC","CBDA","DBCA","DCAB"]:
perm := table():
for letter in "ABCD" do
	perm[letter] := 0:
end do:
for item in lst do
	for letter in "ABCD" do
		perm[letter] += StringTools:-FirstFromLeft(letter, item):
	end do:
end do:
print(StringTools:-Join(ListTools:-Flatten([indices(perm)], 4)[sort(map(x->60-x, ListTools:-Flatten([entries(perm)],4)),'output=permutation')], "")):
```

{{Out|Output}}

```txt
"DBAC"
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
ProvidedSet = {"ABCD" , "CABD" , "ACDB" , "DACB" , "BCDA" , "ACBD",
"ADCB" , "CDAB", "DABC", "BCAD" , "CADB", "CDBA" , "CBAD" , "ABDC",
"ADBC" , "BDCA",  "DCBA" , "BACD", "BADC", "BDAC" , "CBDA", "DBCA", "DCAB"};

Complement[StringJoin /@ Permutations@Characters@First@#, #] &@ProvidedSet


->{"DBAC"}
```



## MATLAB

This solution is designed to work on a column vector of strings. This will not work with a cell array or row vector of strings.


```MATLAB
function perm = findMissingPerms(list)

    permsList = perms(list(1,:)); %Generate all permutations of the 4 letters
    perm = []; %This is the functions return value if the list is not missing a permutation

    %Normally the rest of this would be vectorized, but because this is
    %done on a vector of strings, the vectorized functions will only access
    %one character at a time. So, in order for this to work we have to use
    %loops.
    for i = (1:size(permsList,1))

        found = false;

        for j = (1:size(list,1))
            if (permsList(i,:) == list(j,:))
                found = true;
                break
            end
        end

        if not(found)
            perm = permsList(i,:);
            return
        end

    end %for
end %fingMissingPerms
```


{{out}}

```MATLAB>>
 list = ['ABCD';
'CABD';
'ACDB';
'DACB';
'BCDA';
'ACBD';
'ADCB';
'CDAB';
'DABC';
'BCAD';
'CADB';
'CDBA';
'CBAD';
'ABDC';
'ADBC';
'BDCA';
'DCBA';
'BACD';
'BADC';
'BDAC';
'CBDA';
'DBCA';
'DCAB']

list =

ABCD
CABD
ACDB
DACB
BCDA
ACBD
ADCB
CDAB
DABC
BCAD
CADB
CDBA
CBAD
ABDC
ADBC
BDCA
DCBA
BACD
BADC
BDAC
CBDA
DBCA
DCAB

>> findMissingPerms(list)

ans =

DBAC
```



## Nim

{{trans|Python}}

```nim
import strutils

proc missingPermutation(arr): string =
  result = ""
  if arr.len == 0: return
  if arr.len == 1: return arr[0][1] & arr[0][0]

  for pos in 0 .. <arr[0].len:
    var s: set[char] = {}
    for permutation in arr:
      let c = permutation[pos]
      if c in s: s.excl c
      else:      s.incl c
    for c in s: result.add c

const given = """ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA
  CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB""".split()

echo missingPermutation(given)
```

{{out}}

```txt
DBAC
```



## OCaml


some utility functions:

```ocaml
(* insert x at all positions into li and return the list of results *)
let rec insert x = function
  | [] -> [[x]]
  | a::m as li -> (x::li) :: (List.map (fun y -> a::y) (insert x m))

(* list of all permutations of li *)
let permutations li =
  List.fold_right (fun a z -> List.concat (List.map (insert a) z)) li [[]]

(* convert a string to a char list *)
let chars_of_string s =
  let cl = ref [] in
  String.iter (fun c -> cl := c :: !cl) s;
  (List.rev !cl)

(* convert a char list to a string *)
let string_of_chars cl =
  String.concat "" (List.map (String.make 1) cl)
```


resolve the task:


```ocaml
let deficient_perms = [
  "ABCD";"CABD";"ACDB";"DACB";
  "BCDA";"ACBD";"ADCB";"CDAB";
  "DABC";"BCAD";"CADB";"CDBA";
  "CBAD";"ABDC";"ADBC";"BDCA";
  "DCBA";"BACD";"BADC";"BDAC";
  "CBDA";"DBCA";"DCAB";
  ]

let it = chars_of_string (List.hd deficient_perms)

let perms = List.map string_of_chars (permutations it)

let results = List.filter (fun v -> not(List.mem v deficient_perms)) perms

let () = List.iter print_endline results
```


Alternate method : if we had all permutations,
each letter would appear an even number of times at each position.
Since there is only one permutation missing,
we can find where each letter goes by looking at the parity
of the number of occurences of each letter.
The following program works with permutations of at least 3 letters:

```ocaml
let array_of_perm s =
	let n = String.length s in
	Array.init n (fun i -> int_of_char s.[i] - 65);;

let perm_of_array a =
	let n = Array.length a in
	let s = String.create n in
	Array.iteri (fun i x ->
		s.[i] <- char_of_int (x + 65)
	) a;
	s;;

let find_missing v =
	let n = String.length (List.hd v) in
	let a = Array.make_matrix n n 0
	and r = ref v in
	List.iter (fun s ->
		let u = array_of_perm s in
		Array.iteri (fun i x -> x.(u.(i)) <- x.(u.(i)) + 1) a
	) v;
	let q = Array.make n 0 in
	Array.iteri (fun i x ->
		Array.iteri (fun j y ->
			if y mod 2 != 0 then q.(i) <- j
		) x
	) a;
	perm_of_array q;;

find_missing deficient_perms;;
(* - : string = "DBAC" *)
```



## Octave


```octave
given = [ 'ABCD';'CABD';'ACDB';'DACB'; ...
          'BCDA';'ACBD';'ADCB';'CDAB'; ...
          'DABC';'BCAD';'CADB';'CDBA'; ...
          'CBAD';'ABDC';'ADBC';'BDCA'; ...
          'DCBA';'BACD';'BADC';'BDAC'; ...
          'CBDA';'DBCA';'DCAB' ];
val = 4.^(3:-1:0)';
there = 1+(toascii(given)-toascii('A'))*val;
every = 1+perms(0:3)*val;

bits = zeros(max(every),1);
bits(every) = 1;
bits(there) = 0;
missing = dec2base(find(bits)-1,'ABCD')

```



## Oz

Using constraint programming for this problem may be a bit overkill...


```oz
declare
  GivenPermutations =
  ["ABCD" "CABD" "ACDB" "DACB" "BCDA" "ACBD" "ADCB" "CDAB" "DABC" "BCAD" "CADB" "CDBA"
   "CBAD" "ABDC" "ADBC" "BDCA" "DCBA" "BACD" "BADC" "BDAC" "CBDA" "DBCA" "DCAB"]

  %% four distinct variables between "A" and "D":
  proc {Description Root}
     Root = {FD.list 4 &A#&D}
     {FD.distinct Root}
     {FD.distribute naiv Root}
  end

  AllPermutations = {SearchAll Description}
in
  for P in AllPermutations do
     if {Not {Member P GivenPermutations}} then
        {System.showInfo "Missing: "#P}
     end
  end
```


## PARI/GP


```parigp
v=["ABCD","CABD","ACDB","DACB","BCDA","ACBD","ADCB","CDAB","DABC","BCAD","CADB","CDBA","CBAD","ABDC","ADBC","BDCA","DCBA","BACD","BADC","BDAC","CBDA","DBCA","DCAB"];
v=apply(u->permtonum(apply(n->n-64,Vec(Vecsmall(u)))),v);
t=numtoperm(4, binomial(4!,2)-sum(i=1,#v,v[i]));
Strchr(apply(n->n+64,t))
```

{{out}}

```txt
%1 = "DBAC"
```


## Pascal

like [[c]], summation, and [[Perl 6]] XORing

```pascal
program MissPerm;
{$MODE DELPHI} //for result

const
  maxcol = 4;
type
  tmissPerm = 1..23;
  tcol = 1..maxcol;
  tResString = String[maxcol];
const
  Given_Permutations : array [tmissPerm] of tResString =
     ('ABCD', 'CABD', 'ACDB', 'DACB', 'BCDA', 'ACBD',
      'ADCB', 'CDAB', 'DABC', 'BCAD', 'CADB', 'CDBA',
      'CBAD', 'ABDC', 'ADBC', 'BDCA', 'DCBA', 'BACD',
      'BADC', 'BDAC', 'CBDA', 'DBCA', 'DCAB');
  chOfs =  Ord('A')-1;
var
  SumElemCol: array[tcol,tcol] of NativeInt;
function fib(n: NativeUint): NativeUint;
var
  i : NativeUint;
Begin
  result := 1;
  For i := 2 to n do
    result:= result*i;
end;

function CountOccurences: tresString;
//count the number of every letter in every column
//should be (colmax-1)! => 6
//the missing should count (colmax-1)! -1 => 5
var
  fibN_1 : NativeUint;
  row, col: NativeInt;
Begin
  For row := low(tmissPerm) to High(tmissPerm) do
    For col := low(tcol) to High(tcol) do
      inc(SumElemCol[col,ORD(Given_Permutations[row,col])-chOfs]);

  //search the missing
  fibN_1 := fib(maxcol-1)-1;
  setlength(result,maxcol);
  For col := low(tcol) to High(tcol) do
    For row := low(tcol) to High(tcol) do
      IF SumElemCol[col,row]=fibN_1 then
        result[col]:= chr(row+chOfs);
end;

function CheckXOR: tresString;
var
  row,col: NativeUint;
Begin
  setlength(result,maxcol);
  fillchar(result[1],maxcol,#0);
  For row := low(tmissPerm) to High(tmissPerm) do
    For col := low(tcol) to High(tcol) do
      result[col] := chr(ord(result[col]) XOR ord(Given_Permutations[row,col]));
end;

Begin
  writeln(CountOccurences,' is missing');
  writeln(CheckXOR,' is missing');
end.
```
{{out}}
```txt
DBAC is missing
DBAC is missing
```



## Perl


Because the set of all permutations contains all its own rotations,
the first missing rotation is the target.


```Perl
sub check_perm {
    my %hash; @hash{@_} = ();
    for my $s (@_) { exists $hash{$_} or return $_
        for map substr($s,1) . substr($s,0,1), (1..length $s); }
}

# Check and display
@perms = qw(ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA
            CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB);
print check_perm(@perms), "\n";
```


{{out}}

```txt
DBAC
```



## Perl 6


```perl6
my @givens = <ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA
                CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB>;

my @perms = <A B C D>.permutations.map: *.join;

.say when none(@givens) for @perms;
```

{{out}}
```txt
DBAC
```

Of course, all of these solutions are working way too hard,
when you can just xor all the bits,
and the missing one will just pop right out:

```perl6
say [~^] <ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA
          CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB>;
```

{{out}}
```txt
DBAC
```



## Phix


```Phix
constant perms = {"ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD", "ADCB", "CDAB",
                  "DABC", "BCAD", "CADB", "CDBA", "CBAD", "ABDC", "ADBC", "BDCA",
                  "DCBA", "BACD", "BADC", "BDAC", "CBDA", "DBCA", "DCAB"}

-- 1: sum of letters
sequence r = repeat(0,4)
for i=1 to length(perms) do
    r = sq_add(r,perms[i])
end for
r = sq_sub(max(r)+'A',r)
puts(1,r&'\n')
-- based on the notion that missing = sum(full)-sum(partial) would be true,
--  and that sum(full) would be like {M,M,M,M} rather than a mix of numbers.
-- the final step is equivalent to eg {1528,1530,1531,1529}
--                        max-r[i] -> {   3,   1,   0,   2}
--                        to chars -> {   D,   B,   A,   C}
-- (but obviously both done in one line)

-- 2: the xor trick
r = repeat(0,4)
for i=1 to length(perms) do
    r = sq_xor_bits(r,perms[i])
end for
puts(1,r&'\n')
-- (relies on the missing chars being present an odd number of times, non-missing chars an even number of times)

-- 3: find least frequent letters
r = "    "
for i=1 to length(r) do
    sequence count = repeat(0,4)
    for j=1 to length(perms) do
        count[perms[j][i]-'A'+1] += 1
    end for
    r[i] = smallest(count,1)+'A'-1
end for
puts(1,r&'\n')
-- (relies on the assumption that a full set would have each letter occurring the same number of times in each position)
-- (smallest(count,1) returns the index position of the smallest, rather than it's value)

-- 4: test all permutations
for i=1 to factorial(4) do
    r = permute(i,"ABCD")
    if not find(r,perms) then exit end if
end for
puts(1,r&'\n')
-- (relies on brute force(!) - but this is the only method that could be made to cope with >1 omission)
```

{{out}}

```txt

DBAC
DBAC
DBAC
DBAC

```



## PHP


```php
<?php
$finalres = Array();
function permut($arr,$result=array()){
	global  $finalres;
	if(empty($arr)){
		$finalres[] = implode("",$result);
	}else{
		foreach($arr as $key => $val){
			$newArr = $arr;
			$newres = $result;
			$newres[] = $val;
			unset($newArr[$key]);
			permut($newArr,$newres);
		}
	}
}
$givenPerms = Array("ABCD","CABD","ACDB","DACB","BCDA","ACBD","ADCB","CDAB","DABC","BCAD","CADB","CDBA","CBAD","ABDC","ADBC","BDCA","DCBA","BACD","BADC","BDAC","CBDA","DBCA","DCAB");
$given = Array("A","B","C","D");
permut($given);
print_r(array_diff($finalres,$givenPerms)); // Array ( [20] => DBAC )

```



## PicoLisp


```PicoLisp
(setq *PermList
   (mapcar chop
      (quote
         "ABCD" "CABD" "ACDB" "DACB" "BCDA" "ACBD" "ADCB" "CDAB"
         "DABC" "BCAD" "CADB" "CDBA" "CBAD" "ABDC" "ADBC" "BDCA"
         "DCBA" "BACD" "BADC" "BDAC" "CBDA" "DBCA" "DCAB" ) ) )

(let (Lst (chop "ABCD")  L Lst)
   (recur (L)  # Permute
      (if (cdr L)
         (do (length L)
            (recurse (cdr L))
            (rot L) )
         (unless (member Lst *PermList)  # Check
            (prinl Lst) ) ) ) )
```

{{out}}

```txt
DBAC
```



## PowerShell


{{works with|PowerShell|4.0}}

```PowerShell

function permutation ($array) {
    function generate($n, $array, $A) {
        if($n -eq 1) {
            $array[$A] -join ''
        }
        else{
            for( $i = 0; $i -lt ($n - 1); $i += 1) {
                generate ($n - 1) $array $A
                if($n % 2 -eq 0){
                    $i1, $i2 = $i, ($n-1)
                    $temp = $A[$i1]
                    $A[$i1] = $A[$i2]
                    $A[$i2] = $temp
                }
                else{
                    $i1, $i2 = 0, ($n-1)
                    $temp = $A[$i1]
                    $A[$i1] = $A[$i2]
                    $A[$i2] = $temp
                }
            }
            generate ($n - 1) $array $A
        }
    }
    $n = $array.Count
    if($n -gt 0) {
        (generate $n $array (0..($n-1)))
    } else {$array}
}
$perm = permutation @('A','B','C', 'D')
$find = @(
"ABCD"
"CABD"
"ACDB"
"DACB"
"BCDA"
"ACBD"
"ADCB"
"CDAB"
"DABC"
"BCAD"
"CADB"
"CDBA"
"CBAD"
"ABDC"
"ADBC"
"BDCA"
"DCBA"
"BACD"
"BADC"
"BDAC"
"CBDA"
"DBCA"
"DCAB"
)
$perm | where{-not $find.Contains($_)}

```

<b>Output:</b>

```txt

DBAC

```



## PureBasic


```PureBasic
Procedure in_List(in.s)
  Define.i i, j
  Define.s a
  Restore data_to_test
  For i=1 To 3*8-1
    Read.s a
    If in=a
      ProcedureReturn #True
    EndIf
  Next i
  ProcedureReturn #False
EndProcedure

Define.c z, x, c, v
If OpenConsole()
  For z='A' To 'D'
    For x='A' To 'D'
      If z=x:Continue:EndIf
      For c='A' To 'D'
        If c=x Or c=z:Continue:EndIf
        For v='A' To 'D'
          If v=c Or v=x Or v=z:Continue:EndIf
          Define.s test=Chr(z)+Chr(x)+Chr(c)+Chr(v)
          If Not in_List(test)
            PrintN(test+" is missing.")
          EndIf
        Next
      Next
    Next
  Next
  PrintN("Press Enter to exit"):Input()
EndIf

DataSection
data_to_test:
  Data.s "ABCD","CABD","ACDB","DACB","BCDA","ACBD","ADCB","CDAB"
  Data.s "DABC","BCAD","CADB","CDBA","CBAD","ABDC","ADBC","BDCA"
  Data.s "DCBA","BACD","BADC","BDAC","CBDA","DBCA","DCAB"
EndDataSection
```


Based on the [[Permutations#PureBasic|Permutations]] task,
the solution could be:

```PureBasic
If OpenConsole()
  NewList a.s()
  findPermutations(a(), "ABCD", 4)
  ForEach a()
    Select a()
      Case "ABCD","CABD","ACDB","DACB","BCDA","ACBD","ADCB","CDAB","DABC"
      Case "BCAD","CADB","CDBA","CBAD","ABDC","ADBC","BDCA","DCBA","BACD"
      Case "BADC","BDAC","CBDA","DBCA","DCAB"
      Default
        PrintN(A()+" is missing.")
    EndSelect
  Next

  Print(#CRLF$ + "Press ENTER to exit"): Input()
EndIf
```



## Python


### Python: Calculate difference when compared to all permutations

{{works with|Python|2.6+}}

```python
from itertools import permutations

given = '''ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA
           CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB'''.split()

allPerms = [''.join(x) for x in permutations(given[0])]

missing = list(set(allPerms) - set(given)) # ['DBAC']
```



### Python:Counting lowest frequency character at each position

Here is a solution that is more in the spirit of the challenge,
i.e. it never needs to generate the full set of expected permutations.


```python

def missing_permutation(arr):
  "Find the missing permutation in an array of N! - 1 permutations."

  # We won't validate every precondition, but we do have some basic
  # guards.
  if len(arr) == 0: raise Exception("Need more data")
  if len(arr) == 1:
      return [arr[0][1] + arr[0][0]]

  # Now we know that for each position in the string, elements should appear
  # an even number of times (N-1 >= 2).  We can use a set to detect the element appearing
  # an odd number of times.  Detect odd occurrences by toggling admission/expulsion
  # to and from the set for each value encountered.  At the end of each pass one element
  # will remain in the set.
  missing_permutation = ''
  for pos in range(len(arr[0])):
      s = set()
      for permutation in arr:
          c = permutation[pos]
          if c in s:
            s.remove(c)
          else:
            s.add(c)
      missing_permutation += list(s)[0]
  return missing_permutation

given = '''ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA
           CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB'''.split()

print missing_permutation(given)

```



### Python:Counting lowest frequency character at each position: functional

Uses the same method as explained directly above,
but calculated in a more functional manner:

```python>>>
 from collections import Counter
>>> given = '''ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA
           CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB'''.split()
>>> ''.join(Counter(x).most_common()[-1][0] for x in zip(*given))
'DBAC'
>>>
```


;Explanation
It is rather obfuscated, but can be explained
by showing these intermediate results and noting
that <code>zip(*x)</code> transposes x;
and that at the end of the list
created by the call to <code>most_common()</code>
is the least common character.

```python>>>
 from pprint import pprint as pp
>>> pp(list(zip(*given)), width=120)
[('A', 'C', 'A', 'D', 'B', 'A', 'A', 'C', 'D', 'B', 'C', 'C', 'C', 'A', 'A', 'B', 'D', 'B', 'B', 'B', 'C', 'D', 'D'),
 ('B', 'A', 'C', 'A', 'C', 'C', 'D', 'D', 'A', 'C', 'A', 'D', 'B', 'B', 'D', 'D', 'C', 'A', 'A', 'D', 'B', 'B', 'C'),
 ('C', 'B', 'D', 'C', 'D', 'B', 'C', 'A', 'B', 'A', 'D', 'B', 'A', 'D', 'B', 'C', 'B', 'C', 'D', 'A', 'D', 'C', 'A'),
 ('D', 'D', 'B', 'B', 'A', 'D', 'B', 'B', 'C', 'D', 'B', 'A', 'D', 'C', 'C', 'A', 'A', 'D', 'C', 'C', 'A', 'A', 'B')]
>>> pp([Counter(x).most_common() for x in zip(*given)])
[[('C', 6), ('B', 6), ('A', 6), ('D', 5)],
 [('D', 6), ('C', 6), ('A', 6), ('B', 5)],
 [('D', 6), ('C', 6), ('B', 6), ('A', 5)],
 [('D', 6), ('B', 6), ('A', 6), ('C', 5)]]
>>> pp([Counter(x).most_common()[-1] for x in zip(*given)])
[('D', 5), ('B', 5), ('A', 5), ('C', 5)]
>>> pp([Counter(x).most_common()[-1][0] for x in zip(*given)])
['D', 'B', 'A', 'C']
>>> ''.join([Counter(x).most_common()[-1][0] for x in zip(*given)])
'DBAC'
>>>
```



## R

This uses the "combinat" package, which is a standard R package:
<lang>
library(combinat)

permute.me <- c("A", "B", "C", "D")
perms  <- permn(permute.me)  # list of all permutations
perms2 <- matrix(unlist(perms), ncol=length(permute.me), byrow=T)  # matrix of all permutations
perms3 <- apply(perms2, 1, paste, collapse="")  # vector of all permutations

incomplete <- c("ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD", "ADCB", "CDAB",
                "DABC", "BCAD", "CADB", "CDBA", "CBAD", "ABDC", "ADBC", "BDCA",
                "DCBA", "BACD", "BADC", "BDAC", "CBDA", "DBCA", "DCAB")

setdiff(perms3, incomplete)

```


{{out}}

```txt

[1] "DBAC"

```



## Racket


```racket

#lang racket

(define almost-all
  '([A B C D] [C A B D] [A C D B] [D A C B] [B C D A] [A C B D] [A D C B]
    [C D A B] [D A B C] [B C A D] [C A D B] [C D B A] [C B A D] [A B D C]
    [A D B C] [B D C A] [D C B A] [B A C D] [B A D C] [B D A C] [C B D A]
    [D B C A] [D C A B]))


;; Obvious method:
(for/first ([p (in-permutations (car almost-all))]
            #:unless (member p almost-all))
  p)
;; -> '(D B A C)


;; For permutations of any set
(define charmap
  (for/hash ([x (in-list (car almost-all))] [i (in-naturals)])
    (values x i)))
(define size (hash-count charmap))

;; Illustrating approach mentioned in the task description.
;; For each position, character with odd parity at that position.

(require data/bit-vector)

(for/list ([i (in-range size)])
  (define parities (make-bit-vector size #f))
  (for ([permutation (in-list almost-all)])
    (define n (hash-ref charmap (list-ref permutation i)))
    (bit-vector-set! parities n (not (bit-vector-ref parities n))))
  (for/first ([(c i) charmap] #:when (bit-vector-ref parities i))
    c))
;; -> '(D B A C)

```



## RapidQ


```vb

Dim PList as QStringList
PList.addItems "ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD", "ADCB", "CDAB"
PList.additems "DABC", "BCAD", "CADB", "CDBA", "CBAD", "ABDC", "ADBC", "BDCA"
PList.additems "DCBA", "BACD", "BADC", "BDAC", "CBDA", "DBCA", "DCAB"

Dim NumChar(4, 65 to 68) as integer
Dim MPerm as string

'Create table with occurences
For x = 0 to PList.Itemcount -1
    for y = 1 to 4
        Inc(NumChar(y, asc(PList.Item(x)[y])))
    next
next

'When a char only occurs 5 times it's the missing one
for x = 1 to 4
    for y = 65 to 68
        MPerm = MPerm + iif(NumChar(x, y)=5, chr$(y), "")
    next
next

showmessage MPerm
'= DBAC

```



## REXX


```rexx
/*REXX pgm finds one or more missing permutations from an internal list & displays them.*/
          list = 'ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA',
                 'CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB'
@.=                                              /* [↓]  needs to be as long as  THINGS.*/
@abcU  = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'            /*an uppercase (Latin/Roman) alphabet. */
things = 4                                       /*number of unique letters to be used. */
bunch  = 4                                       /*number letters to be used at a time. */
                 do j=1  for things              /* [↓]  only get a portion of alphabet.*/
                 $.j=substr(@abcU,j,1)           /*extract just one letter from alphabet*/
                 end   /*j*/                     /* [↑]  build a letter array for speed.*/
call permSet 1                                   /*invoke PERMSET subroutine recursively*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
permSet: procedure expose $. @. bunch list things;    parse arg ?
         if ?>bunch  then do;  _=
                                   do m=1  for bunch           /*build a permutation.   */
                                   _=_ || @.m                  /*add permutation──►list.*/
                                   end   /*m*/
                                                               /* [↓]  is in the list?  */
                          if wordpos(_,list)==0  then say _  ' is missing from the list.'
                          end
                     else do x=1  for things                   /*build a permutation.   */
                                   do k=1  for ?-1
                                   if @.k==$.x then iterate x  /*was permutation built? */
                                   end  /*k*/
                          @.?=$.x                              /*define as being built. */
                          call permSet  ?+1                    /*call subr. recursively.*/
                          end   /*x*/
         return
```

'''output'''

```txt

DBAC  is missing from the list.

```



## Ring


```ring

list = "ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB"

for a = ascii("A") to ascii("D")
    for b = ascii("A") to ascii("D")
        for c = ascii("A") to ascii("D")
            for d = ascii("A") to ascii("D")
                x = char(a) + char(b) + char(c)+ char(d)
                if a!=b and a!=c and a!=d and b!=c and b!=d and c!=d
                   if substr(list,x) = 0 see x + " missing" + nl ok ok
            next
        next
    next
next

```

Output:

```txt

DBAC missing

```



## Ruby

{{works with|Ruby|2.0+}}

```ruby
given = %w{
  ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA
  CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB
}

all = given[0].chars.permutation.collect(&:join)

puts "missing: #{all - given}"
```

{{out}}

```txt

missing: ["DBAC"]

```



## Run BASIC


```runbasic
list$ = "ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB"

for a = asc("A") to asc("D")
  for b = asc("A") to asc("D")
    for c = asc("A") to asc("D")
      for d = asc("A") to asc("D")
        x$ = chr$(a) + chr$(b) + chr$(c)+ chr$(d)
        for i = 1 to 4                                            ' make sure each letter is unique
          j = instr(x$,mid$(x$,i,1))
          if instr(x$,mid$(x$,i,1),j + 1) <> 0 then goto [nxt]
        next i
       if instr(list$,x$) = 0 then print x$;" missing"            ' found missing permutation
[nxt] next d
    next c
  next b
next a
```

{{out}}

```txt
DBAC missing
```



## Scala

{{libheader|Scala}}
{{works with|Scala|2.8}}

```scala
def fat(n: Int) = (2 to n).foldLeft(1)(_*_)
def perm[A](x: Int, a: Seq[A]): Seq[A] = if (x == 0) a else {
  val n = a.size
  val fatN1 = fat(n - 1)
  val fatN = fatN1 * n
  val p = x / fatN1 % fatN
  val (before, Seq(el, after @ _*)) = a splitAt p
  el +: perm(x % fatN1, before ++ after)
}
def findMissingPerm(start: String, perms: Array[String]): String = {
  for {
    i <- 0 until fat(start.size)
    p = perm(i, start).mkString
  } if (!perms.contains(p)) return p
  ""
}
val perms = """ABCD
CABD
ACDB
DACB
BCDA
ACBD
ADCB
CDAB
DABC
BCAD
CADB
CDBA
CBAD
ABDC
ADBC
BDCA
DCBA
BACD
BADC
BDAC
CBDA
DBCA
DCAB""".stripMargin.split("\n")
println(findMissingPerm(perms(0), perms))
```



### Scala 2.9.x

{{works with|Scala|2.9.1}}

```Scala
println("missing perms: "+("ABCD".permutations.toSet
  --"ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB".stripMargin.split(" ").toSet))
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func string: missingPermutation (in array string: perms) is func
  result
    var string: missing is "";
  local
    var integer: pos is 0;
    var set of char: chSet is (set of char).EMPTY_SET;
    var string: permutation is "";
    var char: ch is ' ';
  begin
    if length(perms) <> 0 then
      for key pos range perms[1] do
        chSet := (set of char).EMPTY_SET;
        for permutation range perms do
          ch := permutation[pos];
          if ch in chSet then
            excl(chSet, ch);
          else
            incl(chSet, ch);
          end if;
        end for;
        missing &:= min(chSet);
      end for;
    end if;
  end func;

const proc: main is func
  begin
    writeln(missingPermutation([] ("ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD",
           "ADCB", "CDAB", "DABC", "BCAD", "CADB", "CDBA", "CBAD", "ABDC", "ADBC",
           "BDCA", "DCBA", "BACD", "BADC", "BDAC", "CBDA", "DBCA", "DCAB")));
  end func;
```


{{out}}

```txt

DBAC

```



## Sidef

{{trans|Perl}}

```ruby
func check_perm(arr) {
    var hash = Hash()
    hash.set_keys(arr...)
    arr.each { |s|
        {
            var t = (s.substr(1) + s.substr(0, 1))
            hash.has_key(t) || return t
        } * s.len
    }
}

var perms = %w(ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA
               CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB)

say check_perm(perms)
```

{{out}}

```txt

DBAC

```



## Tcl

{{tcllib|struct::list}}

```tcl

package require struct::list

set have { \
    ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA CBAD ABDC \
    ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB \
}

struct::list foreachperm element {A B C D} {
	set text [join $element ""]
	if {$text ni $have} {
		puts "Missing permutation(s): $text"
	}
}

```



## Ursala

The permutation generating function is imported from the standard library below
and needn't be reinvented, but its definition is shown here in the interest of
comparison with other solutions.

```Ursala
permutations = ~&itB^?a\~&aNC *=ahPfatPRD refer ^C/~&a ~&ar&& ~&arh2falrtPXPRD
```

The <code>~&j</code> operator computes set differences.

```Ursala
#import std
#show+

main =

~&j/permutations'ABCD' -[
ABCD
CABD
ACDB
DACB
BCDA
ACBD
ADCB
CDAB
DABC
BCAD
CADB
CDBA
CBAD
ABDC
ADBC
BDCA
DCBA
BACD
BADC
BDAC
CBDA
DBCA
DCAB]-
```

{{out}}

```txt

DBAC

```



## VBScript

Uses the 3rd method approach by adding the columns.

```vb

arrp = Array("ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD",_
      "ADCB", "CDAB", "DABC", "BCAD", "CADB", "CDBA",_
      "CBAD", "ABDC", "ADBC", "BDCA", "DCBA", "BACD",_
      "BADC", "BDAC", "CBDA", "DBCA", "DCAB")

Dim col(4)

'supposes that a complete column have 6 of each letter.
target = (6*Asc("A")) + (6*Asc("B")) + (6*Asc("C")) + (6*Asc("D"))

missing = ""

For i = 0 To UBound(arrp)
	For j = 1 To 4
            col(j) = col(j) + Asc(Mid(arrp(i),j,1))
	Next
Next

For k = 1 To 4
	n = target - col(k)
	missing = missing & Chr(n)
Next

WScript.StdOut.WriteLine missing

```


{{Out}}

```txt
DBAC
```



## XPL0

The list of permutations is input by using a command line like this:
missperm <missperm.txt


```XPL0
code HexIn=26, HexOut=27;
int  P, I;
[P:= 0;
for I:= 1 to 24-1 do P:= P xor HexIn(1);
HexOut(0, P);
]
```


{{out}}

```txt

0000DBAC

```



## zkl

Since I just did the "generate the permutations" task, I'm going to use it to do the brute force solution.

```zkl
var data=L("ABCD","CABD","ACDB","DACB","BCDA","ACBD","ADCB","CDAB",
           "DABC","BCAD","CADB","CDBA","CBAD","ABDC","ADBC","BDCA",
           "DCBA","BACD","BADC","BDAC","CBDA","DBCA","DCAB");
Utils.Helpers.permute(["A".."D"]).apply("concat").copy().remove(data.xplode());
```

Copy creates a read/write list from a read only list.
xplode() pushes all elements of data as parameters to remove.
{{out}}

```txt

L("DBAC")

```



## ZX Spectrum Basic


```zxbasic
10 LET l$="ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB"
20 LET length=LEN l$
30 FOR a= CODE "A" TO  CODE "D"
40 FOR b= CODE "A" TO  CODE "D"
50 FOR c= CODE "A" TO  CODE "D"
60 FOR d= CODE "A" TO  CODE "D"
70 LET x$=""
80 IF a=b OR a=c OR a=d OR b=c OR b=d OR c=d THEN GO TO 140
90 LET x$=CHR$ a+CHR$ b+CHR$ c+CHR$ d
100 FOR i=1 TO length STEP 5
110 IF x$=l$(i TO i+3) THEN GO TO 140
120 NEXT i
130 PRINT x$;" is missing"
140 NEXT d: NEXT c: NEXT b: NEXT a
```

